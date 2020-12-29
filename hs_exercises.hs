-- EXERCISE 1 => DEFINE ZIP FUNCTION YOURSELF
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



-- EXERCISE 2 QUICK SORT
(++++) :: [a] -> [a] -> [a]
[] ++++ ys = ys
(x:xs) ++++ ys = x : (xs ++++ ys)

ownQsort :: [Int] -> [Int]
ownQsort[] = []
ownQsort(x:xs) =
    ownQsort smaller ++++ [x] ++++ ownQsort larger
    where 
        smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x] 

-- EXERCISE 3 => CHAPTER 7 EXERCISES

-- exercise 7.1
{- 
In Haksell Higher order functions are functions that take other functions as parameters and return functions as result.
-}

-- exercise 7.2

own_comprehension xs f p = map f $ filter p xs


-- exercise 7.3

own_foldr :: (a -> b -> b) -> b -> [a] -> b
own_foldr f v [] = v
own_foldr f v (x:xs) = f x (own_foldr f v xs)

own_map f = own_foldr (\x acc -> f x : acc) []
own_filter p = own_foldr (\x acc -> if p x then x : acc else acc) []


main = do
    
    print(Prelude.zip ['a', 'b', 'c'] [1, 2, 3, 4] )
    print(Main.ownZip_1 ['a', 'b', 'c'] [1, 2, 3, 4] )
    print(Main.ownZip_2 ['a', 'b', 'c'] [1, 2, 3, 4] )
    print(Main.ownZip_3 ['a', 'b', 'c'] [1, 2, 3, 4] )


    print(ownQsort[3, -2, 30, 5])


    print(own_map (+1) [1, 3, 5, 7])
    print(own_filter even [1..10])