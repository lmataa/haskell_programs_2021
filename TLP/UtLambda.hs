module UtLambda where

newtype Var = Var String
  deriving (Eq, Ord)

instance Show Var where
  show (Var a) = a

data Term
  = VarTerm Var
  | Lambda Var Term
  | App Term Term

-- Church booleans

-- From Haskell to Terms
boolChurch :: Bool -> Term
boolChurch True = Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "t")))
boolChurch False = Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "f")))

-- From Terms to Haskel
boolUnChurch :: Term -> Bool
boolUnChurch (VarTerm (Var "t")) = True
boolUnChurch (VarTerm (Var "f")) = False
boolUnChurch (Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "t")))) = True
boolUnChurch (Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "f")))) = False
boolUnChurch _ = error "Not a boolean"
