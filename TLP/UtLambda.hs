module UtLambda where

newtype Var = Var String
  deriving (Eq, Ord)

instance Show Var where
  show (Var a) = a

data Term
  = VarTerm Var
  | Lambda Var Term
  | App Term Term
  deriving (Eq, Ord, Show)

data Lit
  = IntLit Int
  | BoolLit Bool

fv :: Term -> [Var]
fv (VarTerm v) = [v]
fv (Lambda v t) = filter (/= v) (fv t)
fv (App t1 t2) = fv t1 ++ fv t2

beta :: Term -> Term
beta (VarTerm v) = VarTerm v
beta (Lambda v t) = Lambda v (beta t)
beta (App t s) = App (beta t) (beta s)

-- From Haskell to Terms
boolChurch :: Bool -> Term
boolChurch True = Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "t")))
boolChurch False = Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "f")))

-- From Terms to Haskel
boolUnChurch :: Term -> Bool
-- boolUnChurch (VarTerm (Var "t")) = True
-- boolUnChurch (VarTerm (Var "f")) = False
boolUnChurch (Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "t")))) = True
boolUnChurch (Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "f")))) = False
boolUnChurch _ = error "Not a boolean"

cond :: Term -> Term -> Term -> Term
cond (Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "t")))) t _ = t
cond (Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "f")))) _ f = f
cond _ _ _ = error "Not a boolean"

-- Church numbers: Exercise 12

-- From Haskell nat to Church nat
natChurch :: Integer -> Term
natChurch 0 = Lambda (Var "s") (Lambda (Var "z") (VarTerm (Var "z")))
natChurch n = Lambda (Var "s") (Lambda (Var "z") (App (VarTerm (Var "s")) (App (VarTerm (Var "z")) (natChurch (n - 1)))))

-- From Church nat to Haskell nat
--natUnChurch :: Term -> Integer
--natUnChurch (Lambda (Var "s") (Lambda (Var "z") (VarTerm (Var "z")))) = 0
--natUn