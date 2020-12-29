concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]


factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]


prime :: Int -> Bool
prime n = factors n == [1, n]


primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]


-- DIFFERENT IMPLEMENTATION OF ZIP FUNCTION! 

ownZip_1 xs ys = foldr step done xs ys
  where done ys = []
        step x zipsfn []     = []
        step x zipsfn (y:ys) = (x, y) : (zipsfn ys)


ownZip_2 :: [a] -> [b] -> [(a, b)]
ownZip_2 xs     []     = []
ownZip_2 []     ys     = []
ownZip_2 (x:xs) (y:ys) = (x, y) : ownZip_2 xs ys



-- in a similar way
ownZip_3 :: [a] -> [b] -> [(a, b)]
ownZip_3 [] _ = []
ownZip_3 _ [] = []
ownZip_3 (x:xs) (y:ys) = (x, y) : ownZip_3 xs ys


pairs :: [a] -> [(a, a)]
pairs xs = Main.ownZip_1 xs (tail xs)


sorted :: Ord a => [a] -> Bool
sorted xs =
    and [x <= y | (x, y) <- pairs xs]


positions :: Eq a => a -> [a] -> [Int]
positions x xs =
    [i | (x', i) <- zip xs [0..n], x == x']
    where n = length xs - 1


-- isLower not found in Prelude
-- lowers :: String -> Int
-- lowers xs = 
--    length [x | x <- xs, isLower x]


-- exercise 1 -> pyths 
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y == z*z]


-- exercise 2 -> check perfects from integer
check n = [x | x <- [1..n], n `mod` x == 0 && x /= n]
perfects n = [x | x <- [1..n], sum (check x) == x]


-- exercise 3 -> scalar product
scalar xs ys = sum [x*y | (x,y) <- zip xs ys]

main = do

    -- comprehension notation used to construct new lists from old lists
    print([x^2 | x <- [1..5]])

    -- comprehesion using multiple generators
    print([(x, y) | x <- [1, 2, 3], y <- [4, 5]])
    print([(x, y) | y <- [4, 5], x <- [1, 2, 3]]) -- order changed

    -- depend generators
    print([(x, y) | x <- [1, 2, 3], y <- [x..3]]) 

    -- concat using function defined with generators
    print(Main.concat[[1, 2, 3], [4, 5], [6]])

    -- comprehensions using guards to restrict the values produced by earlier generators
    print([x | x <- [1..10], even x])

    print(factors 15)

    print(prime 15) -- False
    print(prime 7) -- True

    print(primes 40) 

    --print(Prelude.zip(['a', 'b', 'c'] [1, 2, 3, 4]))

    
    print(Prelude.zip ['a', 'b', 'c'] [1, 2, 3, 4] )
    print(Main.ownZip_1 ['a', 'b', 'c'] [1, 2, 3, 4] )
    print(Main.ownZip_2 ['a', 'b', 'c'] [1, 2, 3, 4] )
    print(Main.ownZip_3 ['a', 'b', 'c'] [1, 2, 3, 4] )


    print(Main.pairs [1, 2, 3, 4] )
    print(Main.sorted [1, 2, 3, 4] )
    print(Main.sorted [1, 2, 4, 3] )


    print(positions 0 [1, 0, 0, 1, 0, 1, 1, 0])


    print("Strings:\n")
    print(length "abcde")
    print(take 3 "abcde")
    print(zip "abc" [1, 2, 3, 4])


    -- Exercises:
    print(Main.pyths 5)
    print(Main.perfects 500)
    print(Main.scalar [1..3][5..8])