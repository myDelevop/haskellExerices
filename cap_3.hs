add ::(Int, Int) -> Int
add(x,y) = x + y

add_1 :: Int -> (Int -> Int)
add_1 x y = x + y


mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

zeroto :: Int -> [Int]
zeroto n = [0..n]



main = do

    -- ERROR:
    --print(1+false);

    print(not False);

    -- print(:type 'a'); ?? not works

    -- own function to add two numbers
    print(add(3,4));
    -- same thing with carried function
    print(add_1 4 3)
    
    -- own function to multiply three numbers
    print(mult 4 3 4);

    -- print array from 0 to 17
    print(zeroto(17));


    print(mult 2 3 4); -- is the same of:
    print(((mult 2) 3) 4); -- this

    -- The length is a polymorphic function like fst, head, take, zip, id
    print(length[False, True]);
    print(length[1,2,3,4]);


    print(fst(13, 42));
    print(head[1,2,3,4]);
    print(take 2 [1,2,3,4]);
    print(zip[1,2,3,4] [3,4,55]);
    print(id[1,2,3,4]);


    print(sum[1,2,3]);
    print(sum[1.1, 2.2, 3.3]);
    -- GIVES ERROR:
    -- print(sum['a', 'b', 'c']);


    {-|  
        Exercise 1:

        - the type of ['a', 'b', 'c'] is [Char]
        - the type of ('a', 'b', 'b') is (Char, Char, Char)
        - the type of [(False, '0'), (True, '1')] is [(Bool, Char)]
        - the type of ([False, True], ['0', '1']) is ([Bool], [Char])
        - the type of [tail, init, reverse] is [[a] -> [a]] 

        Excercise 2:
        - the type of second xs = head(tail xs) is second :: [a] -> a
        - the type of swap is swap :: (a, b) -> (b, a)
        - the type of pair is pair :: a -> b -> (a, b)
        - the type of double is double :: Num a => a -> a
        - the type of palindrome is palindrome :: Eq a => [a] -> Bool
        - the type of twice f x is twice :: (a -> a) -> a -> a
    -}