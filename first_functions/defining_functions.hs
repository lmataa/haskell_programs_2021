-- set of functions for haskell from first; an introduction to haskell programming
-- let define the triple function, i.e. multiply by 3
triple x = x * 3

-- let define the square function, i.e. multiply by itself
square x = x * x

-- let define the sum of squares function, i.e. sum of squares of two numbers
sumOfSquares x y = (square x) + (square y)

-- let define the factorial function, i.e. product of all numbers from 1 to n
factorial n = product [1 .. n]

-- let define the fibonacci function, i.e. sum of two previous numbers
fibonacci n = sum [1 .. n]

-- let define the isEven function, i.e. true if even, false if odd
isEven n = n `mod` 2 == 0

-- let define the isOdd function, i.e. true if odd, false if even
isOdd n = n `mod` 2 /= 0

-- let define the isPrime function, i.e. true if prime, false if not
isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [2 .. n -1]

-- let define the isPerfect function, i.e. true if perfect, false if not
isPerfect n = sum [1 .. n -1] == n

-- let define the isPerfectSquare function, i.e. true if perfect square, false if not
isPerfectSquare n = isPerfect (square n)

-- let define the isPerfectCube function, i.e. true if perfect cube, false if not
isPerfectCube n = isPerfect (cube n)

-- let define the isPerfectPower function, i.e. true if perfect power, false if not
isPerfectPower n = isPerfect (power n)

-- let define the Taylor series of pi
taylorPi n = 4 * (sumOfSquares (1 / (fromIntegral n)) (1 / (fromIntegral (n * 2))))

-- here we have to take into account what the fromIntegral function does, this is
-- a function that takes a number and converts it to a floating point number
-- let define the power function, i.e. n^m
power n m = product [1 .. m]

-- let define the cube function, i.e. n^3
cube n = n * n * n

-- let define the square root function, i.e. square root of n
squareRoot n = sqrt n

-- let define the cube root function, i.e. cube root of n
cubeRoot n = cbrt n

-- okay, I think we get the idea, but we have to work with lambda calculus
-- let define the sum of squares function, i.e. sum of squares of two numbers
sumOfSquares' x y = (\x -> (\y -> x * x + y * y)) x y

-- let define the factorial function, i.e. product of all numbers from 1 to n
factorial' n = (\n -> product [1 .. n]) n

-- let define the fibonacci function, i.e. sum of two previous numbers
fibonacci' n = (\n -> sum [1 .. n]) n

-- now feel free to play with these functions, i.e. try to find out what they do
-- to compile this file you need to use ghc defining_functions.hs