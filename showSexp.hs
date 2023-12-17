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
showSexp' a = showString (sexpToString a)

sexpToString :: Sexp ->  String
sexpToString Snil = "Snil "
sexpToString (Snum n) = "(Snum " ++ (show n) ++ ") "
sexpToString (Ssym s) = "(Ssym " ++ s ++ ") "
sexpToString (Sfun name func) = "(Sfun" ++ name ++ ") "
sexpToString (Sform sf) = "(Sform" ++ (show sf) ++ ") "
sexpToString (Scons e1 e2) = "(Scons " ++ (sexpToString e1) ++ (sexpToString e2) ++ ") "

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
  -- ¡¡¡ COMPLETER ICI !!! --
  (==) (Snum a) (Snum b) = a == b
  (==) (Ssym a) (Ssym b) = a == b
  (==) (Sform a) (Sform b) = a == b
  (==) (Scons a b) (Scons c d) = a == c && b == d 
  -- Définition naïve de Sfun, à modifier
  (==) (Sfun a b) (Sfun c d) = a == c 
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
               -- ¡¡¡ COMPLETER ICI !!! --
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
-----------------------------------------------------------------
elabdef _ Snil = []
elabdef env (Scons a b) = case a of
                          (Ssym "def") -> case b of
                                     (Scons (Ssym a) b@(Scons c d)) -> [(a, elab env b)]
                                                                       ++ (elabdef env b)
                          (Ssym "defs") -> elabdef env b -- d ::= (defs d1 ... dn) | (def x e)
                          _  -> elabdef env b
elabdef _ d = error ("Invalid declaration syntax: " ++ show d)

-- Élaboration d'une expression.  Fonction utilisée par `elabdef`.
elab :: Env -> Sexp -> Lexp
elab _ (Snum n) = Lnum n
elab _ (Ssym sym) = Lvar sym
elab env (Scons a Snil) = elab env a
elab env (Scons a (Scons (Ssym "def") _)) = elab env a
elab env (Scons (Ssym a) (Scons b (Scons c _))) = Lcall (Lcall (Lvar a) (elab env b)) (elab env c)
elab env (Scons (Ssym a) b) = Lcall (Lvar a) (elab env b)

elab _ _ = Lnum (-1) --  valeur tempo pour test
-----------------------------------------------------------------  



-----------------------------------------------------------------
elab _ se = error ("Invalid expression: " ++ show se)
  
-- Évaluateur                                                            --
---------------------------------------------------------------------------

eval_defs :: Env -> Defs -> Env
eval_defs env = foldl (\ env' (x,e) -> ((x, eval env e):env')) []

--func =   sfun (\(Snum x) -> (\(Snum y) -> Snum (x + y)))




-- La fonction d'évaluation principale.
eval :: Env -> Lexp -> Value
eval _ (Lnum n) = Snum n
eval _ (Lquote v) = v
eval env (Lcall a b) = case a of
                        (Lvar v) -> let value = find (\(n, _) -> n == v) env
                              in case value of
                              (Just (_,(Sfun _ f))) -> (f (eval env b))
                              Nothing -> error "sym not found"
                        (Lcall _ _) -> let (Sfun _ f) = eval env a
                              in f (eval env b)


--eval env (Lif a b c) = ...
--eval env (Lletrec a b) = ...
--eval env (Lfun a b) = ...
--eval env (Lvar a) = ...
--eval env (Lcall a b) = ...
---------------------------------------------------------------------------


                    
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
          hPutStr stdout (show s)
          --let env' = eval_defs env (elabdef env s)
          --in do hPutStr stdout (show env')
          --      hPutStr stdout "\n"
          --      run1 (env' ++ env) ss
    in do s <- readFile filename
          run1 env0 (sexps s)

-- Don't use type classes for `read'.
-- test :: String -> Ltype
-- test = infer tenv0 . elab . read
main :: IO ()
main = run "sample.hLisp"
