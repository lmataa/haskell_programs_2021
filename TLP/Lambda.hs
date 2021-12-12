module Lambda where

type Boolean a = a -> a -> a

true :: Boolean a
true = \x y -> x

false :: Boolean a
false = \x y -> y

-- if I woudn't have put types to true and false this inference on not,
-- could not have been made
not :: Boolean a -> Boolean a
not = \f -> \x y -> f y x
