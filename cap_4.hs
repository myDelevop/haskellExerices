absolute :: Int -> Int
absolute n = if n >= 0 then n else -n

absolute_1 n 
    | n >= 0 = n
    | otherwise = -n


sign :: Int -> Int
sign n = if n < 0 then -1 else
            if n == 0 then 0 else
                1

sign_1 n 
    | n < 0 = -1
    | n == 0 = 0
    | otherwise = 1

 
head :: [a] -> a
head(x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs


-- safetail using a conditional expression
safetail_3 :: Eq a => [a] -> [a]
safetail_3 xs = if xs == [] then [] else Prelude.tail xs

-- safetail using guarded equations
safetail_1 :: Eq a => [a] -> [a]
safetail_1 xs
  | xs == []  = []
  | otherwise = Prelude.tail xs

-- safetail using pattern matching
safetail_2 :: Eq a => [a] -> [a]
safetail_2 [] = []
safetail_2 xs = Prelude.tail xs


pred :: Int -> Int
pred n = n-1 

(&&&) :: Bool -> Bool -> Bool
True &&& True = True
-- \!/ SBAGLIATO LOGICAMENTE
True &&& False = True -- it should be False, but see in the main that works
-- \!/ SBAGLIATO LOGICAMENTE
--True &&& False = False
False &&& True = False
False &&& False = False


-- Corrected and more compact version of the logical AND 
(&&&&) :: Bool -> Bool -> Bool
True &&&& True = True
_ &&&& _ = False 

-- another two definitions of and
a &&&&& b = if a && b then True else False

a &&&&&& b = if a then b else False

-- Different definitions of logical OR

(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True

(||||) :: Bool -> Bool -> Bool
False |||| b = b
True |||| _ = True

(|||||) :: Bool -> Bool -> Bool
a ||||| b
    | a == True = True
    | b == True = True
    | otherwise = False

(||||||) :: Bool -> Bool -> Bool
False |||||| False = False
True |||||| True = True
True |||||| False = True
False |||||| True = True


main = do
    -- abs of an Int
    print(absolute(3));
    print(absolute(-3));

    print(absolute_1(-7));
    print(absolute_1(7));

    -- signum of an Int
    print(sign(4))
    print(sign(0))
    print(sign(-4))

    print(sign_1(-5))
    print(sign_1(0))
    print(sign_1(5))

    -- True && False return True (mistake in the definition on &&)
    print(True &&& False);

    print(True &&&& False);

    print(1:(2:(3:(4:[]))))

    print(Main.head[1..5])
    print(Main.tail[1..5])

    -- ERROR: print(Main.head[])

    print(Main.pred(34));

    print((+) 1 3) -- 4
    print((1+) 2) -- 3
    print((+2) 1) -- 3

    print((1+) 3) -- 4 successor function
    print((3/) 3) -- 1.0 reciprocation function
    print((*2) 3) -- 6 doubling function
    print((/2) 6) -- 3.0 halving function
    
    print(Main.safetail_1([1..5]));
    print(Main.safetail_2([1..5]));
    print(Main.safetail_3([1..5]));


    print(True ||| False);
    print(True |||| False);
    print(True ||||| False);
    print(True |||||| False);

    print(True &&&&& True);
    print(True &&&&& False);

    print(True &&&&&& True);
    print(True &&&&&& False);
