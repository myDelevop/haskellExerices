import Data.Char

twice :: (a -> a) -> a -> a
twice f x = f (f x)


own_map_1 :: (a -> b) -> [a] -> [b]
own_map_1 f xs = [f x | x <- xs]

own_map_2 :: (a -> b) -> [a] -> [b]
own_map_2 f [] = []
own_map_2 f (x:xs) = f x : own_map_2 f xs


own_filter_1 :: (a->Bool) -> [a] -> [a]
own_filter_1 p xs = [x | x <- xs, p x]

own_filter_2 :: (a->Bool) -> [a] -> [a]
own_filter_2 p [] = []
own_filter_2 p (x:xs)
    | p x = x : own_filter_2 p xs
    | otherwise = own_filter_2 p xs 


own_foldr :: (a -> b -> b) -> b -> [a] -> b
own_foldr f v [] = v
own_foldr f v (x:xs) = f x (own_foldr f v xs)


own_sum_1 :: [Int] -> Int
own_sum_1 [] = 0
own_sum_1 (x:xs) = x + own_sum_1 xs


own_product_1 :: [Int] -> Int
own_product_1 [] = 1
own_product_1 (x:xs) = x * own_product_1 xs


own_and_1 :: [Bool] -> Bool
own_and_1 [] = True
own_and_1 (x:xs) = x && own_and_1 xs

own_and_2 = own_foldr (&&) True


own_or_1 :: [Bool] -> Bool
own_or_1 [] = True
own_or_1 (x:xs) = x || own_or_1 xs

own_or_2 = own_foldr (||) False

own_sum_2 = own_foldr (+) 0

own_product_2 = own_foldr (*) 1

own_length_1 :: [a] -> Int
own_length_1 [] = 0
own_length_1 (_:xs) = 1 + own_length_1 xs

own_length_2 :: [a] -> Int
own_length_2 = own_foldr(\_ n -> 1+n) 0


own_reverse_1 :: [a] -> [a]
own_reverse_1 [] = []
own_reverse_1(x:xs) = own_reverse_1 xs ++ [x]


own_reverse_2 :: [a] -> [a]
own_reverse_2 = own_foldr (\x xs -> xs ++ [x]) []


-- (++ ys) = own_foldr (:) ys

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)


own_odd :: Int -> Bool
own_odd = not Main.. even

own_all :: (a -> Bool) -> [a] -> Bool
own_all p xs = own_and_1 [p x | x <- xs]

own_any :: (a -> Bool) -> [a] -> Bool
own_any p xs = own_or_2 [p x | x <- xs]


own_takeWhile :: (a -> Bool) -> [a] -> [a]
own_takeWhile p [] = []
own_takeWhile p (x:xs)
    | p x = x : own_takeWhile p xs
    | otherwise = []



own_dropWhile :: (a -> Bool) -> [a] -> [a]
own_dropWhile p [] = []
own_dropWhile p (x:xs)
    | p x = own_dropWhile p xs
    | otherwise = x:xs




-- exercise 1
{- 
In Haksell Higher order functions are functions that take other functions as parameters and return functions as result.
-}

-- exercise 2

own_comprehension xs f p = map f $ filter p xs


-- exercise 3

ex_map f = own_foldr (\x acc -> f x : acc) []
ex_filter p = own_foldr (\x acc -> if p x then x : acc else acc) []


main = do
      
    -- print(twice 1 3)
    print(own_map_1 (+1) [1, 3, 5, 7])
    print(own_map_2 (+1) [1, 3, 5, 7])

    print(own_filter_1 even [1..10])
    print(own_filter_2 even [1..10])

    print(own_sum_1[1..5])
    print(own_sum_2[1..5])
    print(own_product_1[1..5])
    print(own_product_2[1..5])
    print(own_and_1[True, True, True, False])
    print(own_and_2[True, True, True, False])
    print(own_and_1[True, True, True, True])
    print(own_and_2[True, True, True, True])
    print(own_or_1[True, True, True, True])
    print(own_or_2[True, True, True, True])
    print(own_or_1[True, True, True, False])
    print(own_or_2[True, True, True, False])


    print(sum[1, 2, 3])
    print(own_foldr (+) 0 [1, 2, 3])
    print(own_foldr (+) 0 (1:(2:(3:[]))))
    print(1+(2+(3+0)))


    print(own_product_1[1, 2, 3])
    print(own_foldr (*) 1 [1, 2, 3])
    print(own_foldr (*) 1 (1:(2:(3:[]))))
    print(1*(2*(3*1)))

    print(own_length_1[1, 2, 3])
    print(own_length_1(1:(2:(3:[]))))

    print(own_length_2[1, 2, 3])

    print(own_reverse_1 [1..3])
    print(own_reverse_1 (1:(2:(3:[]))))

    print(own_reverse_2 [1..3])
    print(own_reverse_2 (1:(2:(3:[]))))

    print((([] ++ [3]) ++ [2]) ++ [1])

    print(own_odd 4)
    print(own_odd 3)

    print(own_all even [2, 4, 6, 8, 10])
    print(own_all even [2, 4, 5, 8, 10])

    print(own_any even [1, 3, 5, 8, 9])
    print(own_any even [1, 3, 5, 7, 9])

    print(own_any isSpace "abc def")
    print(own_takeWhile isAlpha "abc def")    
    print(own_dropWhile isSpace "   abc")




    -- exercises:
    print(ex_map (+1) [1, 3, 5, 7])
    print(ex_filter even [1..10])
