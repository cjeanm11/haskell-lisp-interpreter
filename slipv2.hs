--- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur (la partie que vous devez compléter)
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Libraire d'analyse syntaxique (et lexicale).
import Data.Char        -- Conversion de Chars de/vers Int et autres
-- import Numeric               -- Pour la fonction showInt
import System.IO                -- Pour stdout, hPutStr
-- import Data.Maybe            -- Pour isJust and fromJust
import Data.List

---------------------------------------------------------------------------
-- Représentation interne des valeurs de notre language                  --
---------------------------------------------------------------------------
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
pSymbol= do { s <- many1 (pSymchar);
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
  -- closure est utilisé pour les fonctions anonymes, donc on les considère
  -- toujours différentes
  (==) (Sfun "closure" _) (Sfun "closure" _) = False
  -- On se fit au nom de la fonction pour l'égalité
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

---------------------------------
-- Élaboration d'une définition
---------------------------------
elabdef :: Env -> Sexp -> Defs
elabdef env (Scons (Ssym sym) b) = let x = (find (\(n,_) -> n == sym) env) 
                                    in case x of
                                        (Just (_,Sform SFdef)) -> getDef env b
                                        (Just (_,Sform SFdefs)) -> elabdefs env b
                                        _ -> error "Not a def or defs"
elabdef _ d = error ("Invalid declaration syntax: " ++ show d)

------------------------------------
-- Élaboration des def dans un defs
------------------------------------
elabdefs :: Env -> Sexp -> Defs
-- Réduction des listes
elabdefs env (Scons a Snil) = elabdef env a;
-- Élaboration d'un def
elabdefs env (Scons (Ssym sym) b) = let x = (find (\(n,_) -> n == sym) env) 
                                    in case x of
                                        (Just (_,Sform SFdef)) -> getDef env b
                                        _ -> error "Not a def or defs"
-- Élaboration d'une liste de def
elabdefs env (Scons a b) = (elabdefs env a) ++ (elabdefs env b)
elabdefs _ d = error ("Invalid declaration syntax: " ++ show d)

----------------------------
-- Association sym et Lexp
----------------------------
getDef :: Env -> Sexp -> Defs
getDef env (Scons (Ssym sym) b) = [(sym, elab env b)]
getDef _ d = error ("not a def" ++ show d) 

-------------------------------------------------------------------
-- Élaboration d'une expression.  Fonction utilisée par `elabdef`.
-------------------------------------------------------------------
elab :: Env -> Sexp -> Lexp
-- On permet aux fonctions et aux macros de prendre en argument la liste vide
elab env (Scons (Ssym sym) Snil) = let e = find (\(n,_) -> n == sym) env
                                    in case e of 
                                        (Just (_, Scons (Ssym "macro") _)) -> 
                                            Lcall (Lvar sym) (Lquote Snil)
                                        (Just (_, Sfun _ _)) ->
                                            Lcall (Lvar sym) (Lquote Snil)
                                        _ -> elab env (Ssym sym)
--Réduction des listes
elab env (Scons a Snil) = elab env a
elab _ (Snum n) = Lnum n
elab _ (Ssym sym) = Lvar sym
elab env (Scons (Ssym "macro") a) = Lcall (Lvar "macro") (elab env a)
elab env (Scons (Ssym sym) b) = let x = (find (\(n,_) -> n == sym) env)
                                in case x of 
                                    (Just (_, Scons (Ssym "macro") f)) -> 
                                        elabMacro env f b
                                    (Just (_, Sfun _ _)) -> elabCall env sym b
                                    (Just (_, Sform SFquote)) -> elabQuote env b
                                    (Just (_, Sform SFlambda)) -> elabFunc env b
                                    (Just (_, Sform SFif)) -> elabIf env b
                                    (Just (_, Sform SFlet)) -> elabLet env b
                                    _ -> Lcall (Lvar sym) (elab env b)
-- Cas de la forme ((e0 e1) e2)
elab env (Scons a b) = Lcall (elab env a) (elab env b)
elab _ se = error ("Invalid expression: " ++ show se)

---------------------------
-- Élaboration des macros
---------------------------
elabMacro :: Env -> Sexp -> Sexp -> Lexp
-- Réduction des listes
elabMacro env (Sfun _ f) arg = elab env (f arg) 
elabMacro _ f a = error ((show f) ++ " " ++ (show a))

-------------------------
-- Élaboration des Lcall
-------------------------
elabCall :: Env -> Var -> Sexp -> Lexp
-- Réduction des listes
elabCall env sym (Scons a Snil) = elabCall env sym a
-- Opérateur à 2 arguments
elabCall env sym (Scons a b) = Lcall (Lcall (Lvar sym) (elab env a)) (elab env b)   
-- Fonction à un argument
elabCall env sym a = Lcall (Lvar sym) (elab env a)

--------------------------
-- Élaboration des Lquote
--------------------------
elabQuote :: Env -> Sexp -> Lexp
-- Réduction des listes
elabQuote env (Scons a Snil) = elabQuote env a
elabQuote env a | (checkVal env a) = Lquote a
elabQuote _ se = error ("Invalid Lquote: " ++ show se)

------------------------------------------------------------
-- Vérifie si la constante de "quote" est de la bonne forme
------------------------------------------------------------
checkVal :: Env -> Sexp -> Bool -- Var = String
checkVal _ Snil = True
checkVal _ (Scons Snil Snil) = True
checkVal _ (Snum _) = True
checkVal _ (Ssym _) = True
checkVal env (Scons a b) = (checkVal env a) && (checkVal env b)
checkVal _ _ =  False

------------------------
-- Élaboration des Lfun
------------------------
elabFunc :: Env -> Sexp -> Lexp
-- Réduction des listes
elabFunc env (Scons a Snil) = elabFunc env a
elabFunc env (Scons (Ssym sym) a) = Lfun sym (elab env a) 
elabFunc env (Scons a b) = elabFunc env (Scons (elabReduc a) (elabReduc b))
elabFunc _ se = error ("Invalid Lfun: " ++ show se)

-----------------------------
-- Réductions de sous-listes
-----------------------------
elabReduc :: Sexp -> Sexp
elabReduc (Scons a Snil) = a
elabReduc a = a

-----------------------
-- Élaboration de Lif
-----------------------
elabIf :: Env -> Sexp -> Lexp
-- Réduction des listes 
elabIf env (Scons a Snil) = elabFunc env a
elabIf env (Scons cond (Scons e1 e2)) = Lif (elab env cond) (elab env e1) (elab env e2)
elabIf _ se = error ("Invalid Lif: " ++ show se)

--------------------------
-- Élaboration de Lletrec
--------------------------
elabLet :: Env -> Sexp -> Lexp
-- Réduction des listes 
elabLet env (Scons a Snil) = elabLet env a
elabLet env (Scons a b) = Lletrec (getLocal env a) (elab env b)
elabLet _ se = error ("Invalit Lletrec: " ++ show se)

----------------------------------------------
-- Retourne les variables du let en tableau
----------------------------------------------
getLocal :: Env -> Sexp -> [(Var,Lexp)]
-- Réduction des listes
getLocal env (Scons a Snil) = getLocal env a
getLocal env (Scons (Ssym sym) a) = [(sym, elabRec env a)]
getLocal env (Scons a b) = getLocal env a ++ getLocal env b
getLocal _ d = error ("Invalid getLocal: " ++ show d)

-----------------------------------------------------------------------
-- Élabore les fonctions d'un let de façon à ce qu'elle soit récursive
-- (utilisation de la forme spéciale lambda)
-----------------------------------------------------------------------
elabRec :: Env -> Sexp -> Lexp
-- Réductions des listes
elabRec env (Scons a Snil) = elabRec env a
elabRec _ e@(Scons (Ssym "lambda") _) = Lquote (Scons (Sform SFlambda) e)
-- Réorganisation des listes
elabRec env (Scons (Scons e1@(Ssym _) e2@(Snum _)) c) =
                                     elabRec env (Scons e1 (Scons e2 c))
elabRec env e = elab env e

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

eval_defs :: Env -> Defs -> Env
eval_defs env = foldl (\ env' (x,e) -> ((x, eval env e):env')) []

-- La fonction d'évaluation principale.
eval :: Env -> Lexp -> Value
eval _ (Lnum n) = Snum n
eval _ (Lquote v) = v
eval env (Lvar sym) = evalVar env sym
eval env (Lcall a b) = evalCall env a b
eval env (Lfun sym a) = sfun (\x -> eval ([(sym,x)] ++ env) a)
eval env (Lif cond a b) = let cond' = eval env cond
                            in case cond' of
                                Snil -> eval env b
                                _ -> eval env a
-- Pour Lletrec, on évalue d'abord les définitions locales puis on les 
-- ajoute à l'environnemnt
eval env (Lletrec a b) = let a' = (map (\(x,y) -> (x,eval env y)) a)
                            in eval (a' ++ env) b


---------------------------------------------------------------------------
-- Évaluation de Var, retourne sa valeur associée dans env si elle existe
-- sinon retourne un Ssym
---------------------------------------------------------------------------
evalVar :: Env -> Var -> Value
evalVar env sym = let value = find (\(n, _) -> n == sym) env
                    in case value of
                        (Just (_,val)) -> val
                        _ -> Ssym sym

-------------------------------------------------------------------------
-- Évaluation de Lcall pattern non exaustif
-------------------------------------------------------------------------
evalCall :: Env -> Lexp -> Lexp -> Value
evalCall env a b = let arg = eval env b
                in case a of
                    (Lvar sym) -> let value = find (\(n, _) -> n == sym) env
                       in case value of
                        (Just (_,Sfun _ f)) -> f arg
                        -- Fonctions récursives
                        (Just (_,(Scons (Sform SFlambda) e))) ->
                            let Sfun _ f = eval env (elab env e)
                            in f arg
                        -- Macros
                        (Just (_,(Scons (Ssym "macro") f))) ->
                            evalMacro env f arg
                        _ -> Ssym sym
                    (Lcall _ _) -> let e = eval env a
                                    in case e of 
                                        (Sfun _ f) -> f arg
                                        _ -> error (show e)
                    -- Imbrication de lambda
                    (Lfun sym f) -> eval ((\x -> [(sym,x)] ++ env) arg) f
                    _ -> error ("Not a function " ++ (show a))

--------------------------
-- Évaluation des macros
--------------------------
evalMacro :: Env -> Sexp -> Sexp -> Value
evalMacro _ (Sfun _ f) arg = f arg
evalMacro _ _ _ = error "Not a macro"

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
          let env' = eval_defs env (elabdef env s)
          in do hPutStr stdout (show env')
                hPutStr stdout "\n"
                run1 (env' ++ env) ss
    in do s <- readFile filename
          run1 env0 (sexps s)


-- Don't use type classes for `read'.
-- test :: String -> Ltype
-- test = infer tenv0 . elab . read