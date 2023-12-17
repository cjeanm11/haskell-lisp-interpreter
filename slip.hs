{-# OPTIONS_GHC -Wall #-}
import Text.ParserCombinators.Parsec 
import Data.Char        -- Conversion de Chars de/vers Int et autres
--import Numeric               -- Pour la fonction showInt
import System.IO                -- Pour stdout, hPutStr
--import Data.Maybe            -- Pour isJust and fromJust
import Data.List

data SpecialForm = SFdefs | SFdef | SFlet | SFif | SFlambda | SFquote
                 deriving (Show, Eq)

data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          | Sfun String (Sexp -> Sexp)   -- Une fonction
          | Sform SpecialForm           -- Une forme spéciale

-- Exemples:
-- (+ 2 3) == (+ . (2 . (3 . ())))
--         ==> Scons (Ssym "+")
--                   (Scons (Snum 2)
--                          (Scons (Snum 3) Snil))
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Scons (Ssym "/")
--       (Scons (Scons (Ssym "*")
--                     (Scons (Scons (Ssym "-")
--                                   (Scons (Snum 68)
--                                          (Scons (Snum 32) Snil)))
--                            (Scons (Snum 5) Snil)))
--              (Scons (Snum 9) Snil))

---------------------------------------------------------------------------
-- Pretty Printer                                                        --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Sfun name _) = showString ("#<function:" ++ name ++ ">")
showSexp' (Sform sf)
  = showString ("#<special-form:" ++ (tail (tail (show sf))) ++ ">")
showSexp' (Scons e1 e2) =
    let showTail Snil = showChar ')'
        showTail (Scons e1' e2') =
            showChar ' ' . showSexp' e1' . showTail e2'
        showTail e = showString " . " . showSexp' e . showChar ')'
    in showChar '(' . showSexp' e1 . showTail e2

instance Show Sexp where
    showsPrec _ = showSexp'

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                pChar '\n'; return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment); return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol = do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }
  
---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(shorthand-quote E)"
-- La notation "`E" est équivalente à "(shorthand-backquote E)"
-- La notation ",E" est équivalente à "(shorthand-comma E)"
pQuote :: Parser Sexp
pQuote = do { c <- satisfy (\c -> c `elem` "'`,"); pSpaces; e <- pSexp;
              return (Scons (Ssym (case c of
                                     ',' -> "shorthand-comma"
                                     '`' -> "shorthand-backquote"
                                     _ -> "shorthand-quote"))
                            (Scons e Snil)) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces; pTail }
pTail :: Parser Sexp
pTail  = do { pChar ')'; return Snil }
     <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
              pChar ')' <|> error ("Missing ')' after: " ++ show e);
              return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (Scons e es) }

pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                       Left _ -> []
                       Right e -> [(e,"")]

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

instance Eq Sexp where
  -- La fonction d'égalité entre Sexps.  On ne peut pas utiliser "deriving Eq"
  -- pour la définir parce que Sexp inclu des fonctions et qu'il n'y a pas
  -- d'égalité prédéfinie sur les fonctions.
  (==) Snil Snil = True
  (==) (Snum a) (Snum b) = a == b
  (==) (Ssym a) (Ssym b) = a == b
  (==) (Sform a) (Sform b) = a == b
  (==) (Scons a b) (Scons c d) = a == c && b == d 
  -- Définition naïve de Sfun et probablement inexacte, mais rien trouvé de mieux
  (==) (Sfun a _) (Sfun b _) = a == b
  (==) _ _ = False


  ---------------------------------------------------------------------------
-- Initial environment                                                   --
---------------------------------------------------------------------------

type Value = Sexp
type Env = [(Var, Value)]

sfun :: (Sexp -> Sexp) -> Sexp
sfun = Sfun "closure"

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = [("defs",   Sform SFdefs),
        ("def",    Sform SFdef),
        ("let",    Sform SFlet),
        ("if",     Sform SFif),
        ("lambda", Sform SFlambda),
        ("quote",  Sform SFquote)]
       ++ map (\(n,f) -> (n, Sfun n f))
              [("cons",  \x -> sfun (\xs -> Scons x xs)),
               ("cons?", \x -> case x of {Scons _ _ -> Ssym "t"; _ -> Snil}),
               ("car",   \v -> case v of {Scons x _ -> x; _ -> error ("Not a cons: " ++ show v)}),
               ("cdr",   \v -> case v of {Scons _ y -> y; _ -> error ("Not a cons: " ++ show v)}),
               ("=",     \ x -> sfun (\ y -> if x == y then Ssym "t" else Snil)),
               ("+",    \(Snum x) -> sfun (\(Snum y) -> Snum (x + y))),
               ("-",    \(Snum x) -> sfun (\(Snum y) -> Snum (x - y))),
               ("/",    \(Snum x) -> sfun (\(Snum y) -> Snum (x `div` y))),
               ("*",    \(Snum x) -> sfun (\(Snum y) -> Snum (x * y))),
               ("make-symbol", \x -> sfun (\y -> Ssym (show x ++ show y)))
              ]

---------------------------------------------------------------------------
-- Représentation intermédiaire                                          --
---------------------------------------------------------------------------

type Var = String

data Lexp = Lnum Int            -- Constante entière.
          | Lquote Value        -- Une valeur immédiate.
          | Lvar Var            -- Référence à une variable.
          | Lfun Var Lexp       -- Fonction anonyme prenant un argument.
          | Lcall Lexp Lexp     -- Appel de fonction, avec un argument.
          | Lif Lexp Lexp Lexp  -- Test conditionnel.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lletrec [(Var, Lexp)] Lexp
          deriving (Show)

type Defs = [(Var, Lexp)]
---------------------------------------------------------------------------
-- Élaboration                                                           --
---------------------------------------------------------------------------

-- Élaboration d'une définition.
elabdef :: Env -> Sexp -> Defs
elabdef _ Snil = []
elabdef env (Scons a b) = case a of
        (Scons _ _) -> (elabdef env a) ++ (elabdef env b)
        (Ssym "def") -> case b of
               (Scons (Ssym value) b) -> [(value, elab env b)]
               _ -> error ("Invalid declaration syntax: " ++ show b)
        (Ssym "defs") -> elabdef env b -- d ::= (defs d1 ... dn) | (def x e)
        _ -> elabdef env b
elabdef _ d = error ("Invalid declaration syntax: " ++ show d)

-- Fonction qui retourne les variables locales du 'let' sous forme de tableau.
getLocal :: Env -> Sexp -> [(String,Lexp)]
getLocal env (Scons (Ssym v) e2) = [(v, elabSF env e2)]
getLocal env (Scons (Scons (Ssym v) e2) Snil) = [(v, elabSF env e2)]
getLocal env (Scons (Scons (Ssym v) e2) e3) =
                             [(v, elabSF env e2)] ++ getLocal env e3
getLocal _ d = error ("Invalid declaration syntax: " ++ show d)


-- Fonction qui verifie si une constante "quote" est de la bonne forme.
-- Ex: (val) v ::= n | s | f | (v1 . v2) |  ()
checkVal :: Env -> Sexp -> Bool -- Var = String
checkVal _ (Scons Snil Snil) = True
checkVal _ (Scons (Ssym a) Snil ) = True
checkVal _ (Scons (Snum a) Snil) = True
checkVal env (Scons e@(Scons a b) Snil) = (checkVal env e)
checkVal _ (Snum a) = True
checkVal _ (Ssym a) = True
checkVal env (Scons a b) = (checkVal env a) && (checkVal env b)
checkVal _ _ =  False

-- Fonction qui retir les parenteses d'un expression Sexp.
elabVal :: Sexp -> Sexp
elabVal  (Scons Snil Snil) = Ssym "()"
elabVal  (Scons (Ssym a) Snil) = Ssym a
elabVal  (Scons (Snum a) Snil) = Snum a
elabVal (Scons e@(Scons a b) Snil) = (elabVal e)
elabVal (Snum a) = Snum a
elabVal (Ssym a) = Ssym a
elabVal e@(Scons a b) =  Scons e Snil


-- Élaboration d'une expression.  Fonction utilisée par `elabdef`.
elab :: Env -> Sexp -> Lexp
elab _ (Snum n) = Lnum n
elab _ (Ssym sym) = Lvar sym
elab _ (Scons Snil Snil) = Lvar "()"
elab env (Scons (Ssym "quote") a) =
                if (checkVal env a) -- Verifie si c'est un '(val)'
                then  Lquote (elabVal a) -- Retire les parenteses
                else Lvar "quote" -- Cas avec association à une SForm
elab env (Scons (Ssym "lambda") a) =
                let (Scons (Ssym v) e2) = a
                in Lfun v (elab env e2)
elab env (Scons (Ssym "let") a) =
                let (Scons e1 e2) = a
                in Lletrec (getLocal env e1) (elab env e2)
elab env (Scons (Ssym "if") e) =
                let (Scons cond (Scons block1 block2)) = e
                in Lif (elab env cond) (elab env block1) (elab env block2)
elab env (Scons a Snil) = elab env a -- Cas: ((+) (1) (1))
elab env (Scons (Ssym a) b) = let e = find (\(n, _) -> n == a) env
             in case b of
                (Scons b (Scons c _)) ->
                          case e of
                          (Just (_,Sform SFquote)) -> Lquote b
                          {-- -- Code associé aux macros (imcomplet)
                           (Just (_,Scons (macro) e1@(Sfun _ func))) ->
                                    let  e = find (\(n, _) -> n == "macro") env
                                    in case e of
                                        (Just ("macro",e2@(Sfun _ func))) ->
                                                         elab env (Scons e1 e2)
                                        _ -> error "macro not found"
                                        -}
                          _ -> Lcall (Lcall (Lvar a) (elab env b)) (elab env c)
                _ -> case e of
                    (Just (_,Sform SFquote)) -> Lquote b
                    {- -- Code associé aux macros (imcomplet)
                    (Just (_,Scons (macro) e1@(Sfun _ func))) ->
                               let
                               e = find (\(n, _) -> n == "macro") env
                               in case e of
                               (Just ("macro",e2@(Sfun _ func))) ->
                                               elab env (Scons e1 e2)
                               _ -> error "macro not found"
                               -}
                    _ -> Lcall (Lvar a) (elab env b)
-- divers cas: ((+ 1) 1), etc
elab env (Scons (Scons e1@(Ssym a) e2@(Snum b)) c) =
                                               elab env (Scons e1 (Scons e2 c))
elab env (Scons a b) = Lcall (elab env a) (elab env b)
elab env (Scons(Scons e1@(Ssym s) Snil ) (Scons e2@(Snum n) Snil )) =
                                              Lcall (elab env e1) (elab env e2)
elab _ se = error ("Invalid expression: " ++ show se)


elabSF :: Env -> Sexp -> Lexp
elabSF env e@(Scons (Ssym "lambda") a) = Lquote (Scons (Sform SFlambda) e)
elabSF env (Scons a Snil) = elabSF env a
elabSF env (Scons (Scons e1@(Ssym a) e2@(Snum b)) c) =
                                     elabSF env (Scons e1 (Scons e2 c))
elabSF env e = elab env e

-- Évaluateur                                                            --
---------------------------------------------------------------------------

eval_defs :: Env -> Defs -> Env
eval_defs env = foldl (\ env' (x,e) -> ((x, eval env e):env')) []


-- La fonction d'évaluation principale.
eval :: Env -> Lexp -> Value
eval _ (Lnum n) = Snum n
eval _ (Lvar "()") = Snil
eval env (Lquote v) = v
     {- Code associé aux macros (incomplet)
         let value = find (\(n, _) -> n == "macro") env
         in case value of
             (Just a) -> eval env (elab env (readSexp (show v)))
             Nothing -> v -}
eval env (Lvar s) =
         let value = find (\(n, _) -> n == s) env -- find reference of val
         in case value of
          -- (Just (_,(Scons macro e))) -> e -- code incomplet (macro)
             (Just (_,val)) -> val
             Nothing -> (Ssym s)
eval env (Lcall a b) =
          let argument = eval env b
          in case a of
              (Lvar v) ->
                    let value = find (\(n, _) -> n == v) env
                    in case value of
                        (Just (_,(Sfun _ f))) -> f argument
                        (Just (_,(Scons (Sform SFlambda) e))) ->
                          let Sfun _ f = (eval env (elab env e))
                          in f argument
                       -- (Just (_,Scons macro e)) -> let Sfun _ f = e
                       --   in f argument
                        (Just (_,s)) -> error (show s)
                        Nothing -> error v
              (Lcall _ _) -> let (Sfun _ f) = eval env a
                    in f argument
              -- pour faire des appels de fonction de forme: ((lambda x e1) e2)
              (Lfun var f) -> eval ((\x -> [(var,x)] ++ env) (eval env b)) f
eval env (Lfun a b) =  sfun (\x -> eval ([(a,x)] ++ env) b)
eval env (Lletrec a b) =
         let a' = (map (\(x,y) -> (x,eval env y)) a) --eval definition locales
         in eval (a' ++ env) b
eval env (Lif a b c) =
         let a' = eval env a
         in case a' of
             Snil -> eval env c
             _ -> eval env b

-- Toplevel                                                              --
---------------------------------------------------------------------------

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    let sexps s' = case parse pSexps filename s' of
                            Left err -> error (show err)
                            Right es -> es
        run1 _ [] = hPutStr stdout "\n"
        run1 env (s:ss) =
          --hPutStr stdout (show (elabdef env s))
          let env' = eval_defs env (elabdef env s)
          in do hPutStr stdout (show env')
                hPutStr stdout "\n"
                run1 (env' ++ env) ss
    in do s <- readFile filename
          run1 env0 (sexps s)


-- Don't use type classes for `read'.
-- test :: String -> Ltype
-- test = infer tenv0 . elab . read
main :: IO ()
main = run "tests.hLisp"

