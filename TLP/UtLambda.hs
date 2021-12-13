module UtLambda where

import Data.List

-- | Haskell datatype to represent the abstract syntax of the lambda calculus
newtype Var = Var String
  deriving (Eq, Ord)

instance Show Var where
  show (Var a) = a

data Term
  = VarTerm Var
  | Lambda Var Term
  | App Term Term
  deriving (Eq, Ord, Show)

-- | Free variables of a term (i.e. variables that are not bound by a lambda)
fv :: Term -> [Var]
fv (VarTerm v) = [v]
fv (Lambda v t) = delete v (fv t)
fv (App t1 t2) = fv t1 `union` fv t2

-- | Infinite list generation with variable names
-- filter list by removing the variable names that are already in the list
-- and then generate a new variable name by appending a number to the variable name
genVar :: [Var] -> Term

allVars = map (Var . (: [])) ['a' .. 'z']

genVar vars = VarTerm (head $ filter (`notElem` vars) allVars)

-- | Capture avoiding substitution
subst :: Term -> Var -> Term -> Term
subst e x t = case e of
  VarTerm v -> if v == x then t else e
  Lambda v p
    | v == x -> Lambda x (subst p x t)
    | x `notElem` fv p -> Lambda v p
    | v `notElem` fv t -> Lambda v (subst p x t)
    | otherwise ->
      ( let z = genVar (fv p `union` fv t)
         in Lambda (head $ fv z) (subst (subst p v z) x t)
      )
  App e1 e2 -> App (subst e1 x t) (subst e2 x t)

-- | Beta reduction
beta :: Term -> Term
beta (App (Lambda x e) e') = subst e x e'
beta (Lambda x p) = Lambda x (beta p)
beta (App e1 e2) = App (beta e1) (beta e2)
beta e = e

-- | Alpha convesrion
alpha :: Term -> Var -> Var -> Term
alpha (App e1 e2) x y = App (alpha e1 x y) (alpha e2 x y)
alpha (Lambda v e) x y =
  if y `elem` fv e
    then error "occurs check"
    else
      let e' = subst e x (VarTerm y)
       in Lambda y e'
alpha e x y = e

-- | Church booleans
-- From Haskell to Terms
boolChurch :: Bool -> Term
boolChurch True = Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "t")))
boolChurch False = Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "f")))

-- From Terms to Haskel
boolUnChurch :: Term -> Bool
boolUnChurch (Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "t")))) = True
boolUnChurch (Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "f")))) = False
boolUnChurch _ = error "Not a boolean"

-- Condition
cond :: Term -> Term -> Term -> Term
cond (Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "t")))) t _ = t
cond (Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "f")))) _ f = f
cond _ _ _ = error "Not a boolean"

-- disjunction
disj :: Term -> Term -> Term
disj (Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "t")))) _ = boolChurch True
disj (Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "f")))) t = t
disj _ _ = error "Not a boolean"

-- conjunction
conj :: Term -> Term -> Term
conj (Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "t")))) t = t
conj (Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "f")))) _ = boolChurch False
conj _ _ = error "Not a boolean"

-- negation
neg :: Term -> Term
neg (Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "t")))) = boolChurch False
neg (Lambda (Var "t") (Lambda (Var "f") (VarTerm (Var "f")))) = boolChurch True
neg _ = error "Not a boolean"

-- Church numbers: Exercise 12
-- | Successor function
succ_ :: Term -> Term
succ_ (Lambda v (Lambda v' t)) = Lambda v (Lambda v' (App (VarTerm(Var "s")) t))
succ_ _ = error "Not a number"

-- From Haskell nat to Church nat
natChurch :: Integer -> Term
natChurch 0 = Lambda (Var "s") (Lambda (Var "z") (VarTerm (Var "z")))
natChurch n = succ_ (natChurch (n - 1))

-- From Church nat to Haskell nat
natUnChurch :: Term -> Integer
natUnChurch (Lambda (Var "s") (Lambda (Var "z") (VarTerm (Var "z")))) = 0
natUnChurch (Lambda (Var "s") (Lambda (Var "z") (App f t))) = 1 + natUnChurch (Lambda (Var "s") (Lambda (Var "z") t))
natUnChurch _ = error "Not a number"

-- | Some tests
-- | Note : a $ b c = a (b c)
-- boolean true and false
t = boolChurch True

f = boolChurch False

-- lambda xy . xy
t1 = Lambda (Var "x") (Lambda (Var "y") (App (VarTerm (Var "x")) (VarTerm (Var "y"))))

-- lambda xy . yb
t2 = Lambda (Var "x") (Lambda (Var "y") (App (VarTerm (Var "y")) (VarTerm (Var "b"))))

-- aa
t3 = App (VarTerm (Var "a")) (VarTerm (Var "a"))

-- ab
t4 = App (VarTerm (Var "a")) (VarTerm (Var "b"))

-- lambda x . xy
t5 = Lambda (Var "x") (App (VarTerm (Var "x")) (VarTerm (Var "y")))

-- a
t6 = VarTerm (Var "a")

-- lambda x . xy (ab)
t7 = App t5 t4

-- lambda xy . xy
t8 = Lambda (Var "x") (Lambda (Var "y") (App (VarTerm (Var "x")) (VarTerm (Var "y"))))

-- lambda x . xx
t9 = Lambda (Var "x") (App (VarTerm (Var "x")) (VarTerm (Var "x")))

-- (lambda x . xx) ((lambda xy . xy) (a))
t10 = App t9 (App t8 t6)

-- | Substitutions
t1' = subst t1 (Var "x") (VarTerm (Var "k"))

t1'' = subst t1 (Var "z") (VarTerm (Var "k"))

t2' = subst t2 (Var "x") (VarTerm (Var "k"))

t3' = subst t3 (Var "a") (VarTerm (Var "k"))

t4' = subst t4 (Var "a") (VarTerm (Var "k"))

t5' = subst t5 (Var "x") (VarTerm (Var "k"))

t5'' = subst t5 (Var "y") (VarTerm (Var "k"))

t6' = subst t6 (Var "a") (VarTerm (Var "k"))

-- | Alpha equivalence
t1Alpha = alpha t1 (Var "x") (Var "w")

-- | Numerals

one = natChurch 1
two = natChurch 2
three = natChurch 3
ten = natChurch 10
zero = natChurch 0

six = succ_ (natChurch 5)