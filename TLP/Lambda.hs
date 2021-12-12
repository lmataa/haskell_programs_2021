{-# LANGUAGE RankNTypes #-}

module Lambda where

import Unsafe.Coerce

y :: (a -> a) -> a
y = \f -> (\x -> f (unsafeCoerce x x)) (\x -> f (unsafeCoerce x x))

true = (\x y -> x)

false = (\x y -> y)

cond = \b t f -> b t f

newtype Chur = Chr (forall a. (a -> a) -> (a -> a))

zer :: Chur
zer = Chr (\x y -> y)

suc :: Chur -> Chur
suc (Chr cn) = Chr (\h -> cn h . h)

ci :: Chur -> Integer
ci (Chr cn) = cn (+ 1) 0

ic :: Integer -> Chur
ic 0 = zer
ic n = suc $ ic (n - 1)