{-# LANGUAGE UnicodeSyntax #-}

factorial :: Int -> Int
factorial n = Prelude.product [1..n]


facorial_rec :: Int -> Int
facorial_rec 0 = 1
facorial_rec (n) = n * factorial (n-1)


own_product :: [Int] -> Int
own_product [] = 1
own_product (n:ns) = n * own_product ns


own_length :: [a] -> Int
own_length[] = 0
own_length(n:ns) = 1 + own_length ns


own_reverse :: [a] -> [a]
own_reverse [] = []
own_reverse(x:xs) = own_reverse xs ++ [x]


own_zip :: [a] -> [b] -> [(a,b)]
own_zip [] _ = []
own_zip _ [] = []
own_zip (x:xs) (y:ys) = (x,y) : own_zip xs ys


--own_drop :: Int -> [a] -> [a]
--own_drop 0 xs = xs
--own_drop (n) [] = []
--own_drop (n) (_:xs) = own_drop n-1 xs

own_drop :: Int -> [a] -> [a]
own_drop _ [] = []
own_drop n xs@(_:xs')
   | n > 0     = own_drop (n-1) xs'
   | otherwise = xs


(++++) :: [a] -> [a] -> [a]
[] ++++ ys = ys
(x:xs) ++++ ys = x : (xs ++++ ys)



qsort :: [Int] -> [Int]
qsort[] = []
qsort(x:xs) =
    qsort smaller ++++ [x] ++++ qsort larger
    where 
        smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x] 



-- Exercise 1



-- (a) Decide if all logical values in a list are true
own_and :: [Bool] -> Bool
own_and [] = True
own_and (b:bs) = b && own_and bs


-- (b) Concatenate a list of lists:
own_concat :: [[a]] -> [a]
own_concat [] = []
own_concat (x:xs) = x ++ own_concat xs

-- (c) Produce a list with n identical elements:
own_replicate :: Int -> a -> [a]
own_replicate 0 _ = []
own_replicate n x = [x] ++ own_replicate (n-1) x

-- (d) Select the nth element of a list:
(!!!!) :: [a] -> Int -> a
(!!!!) (x:xs) 0 = x
(!!!!) (x:xs) i = (!!!!) xs (i-1)

-- (e) Decide if a value is an element of a list:
own_elem :: Eq a => a -> [a] -> Bool 
own_elem _ [] = False
own_elem v (x:xs) = v == x || own_elem v xs



-- Exercise 2
own_merge ∷ [Int] -> [Int] -> [Int]
own_merge [] [] = []
own_merge xs [] = xs
own_merge [] ys = ys
own_merge (x : xs) (y : ys) =
	if x < y
	then x : own_merge xs (y : ys)
	else y : own_merge (x : xs) ys


-- Exercise 3
own_msort ∷ [Int] -> [Int] 
own_msort [] = []
own_msort (x : []) = [x]
own_msort xs = own_merge (own_msort (take divideAt xs)) (own_msort (drop divideAt xs)) where
	divideAt = div (length xs) 2



main = do
    print(factorial 5)
    print(facorial_rec 5)
    print(own_product [1..5])
    print(own_length [1..5])
    print(own_reverse [1..5])
    print(own_zip ['a', 'b', 'c'] [1, 2, 3, 4] )

    print(own_drop 4 [1..6])

    print([1, 8] ++++ [2, 7])
    print(qsort[3, -2, 30, 5])


    -- Exercises:
    print(own_and[True, False, True])
    print(own_and[True, True, True])

    print(own_concat[[1, 2, 5], [7, 9]])

    print(own_replicate 7 'a')
    print((!!!!)[1,2,3] 2)

    print(own_elem 4 [1..5])
    print(own_elem 4 [1..3])


    print(Main.own_merge[2,5,6][1,3,4])
    print(Main.own_msort[2,5,6, 1,3,4])