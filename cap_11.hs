{-# LANGUAGE NPlusKPatterns #-}

-- Type declarations:

type OwnString = [Char]

type Pos = (Int, Int)

type Pair a = (a, a)

type Trans = Pos -> Pos


-- Type declaration cannot be recursive
-- type Tree = (Int, [Tree])


-- Data declarations:
data Bool = False | True

data Answer = Yes | No | Unknown

data Shape = Circle Float | Rect Float Float

data Maybe a = Nothing | Just a

data Nat = Zero | Succ Nat

data Expr = Val Int
    | Add Expr Expr
    | Mul Expr Expr


data Tree = Leaf Int
    | Node Tree Int Tree

-- Functions:

origin :: Pos
origin = (0,0)

left :: Pos -> Pos
left (x,y) = (x-1, y)


mult :: Pair Int -> Int
mult(m, n) = m * n

copy :: a -> Pair a
copy x = (x,x)

answers :: [Answer]
answers = [Yes, No, Unknown]

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x*y


safediv :: Int -> Int -> Main.Maybe Int
safediv _ 0 = Main.Nothing
safediv m n = Main.Just (div m n)


safehead :: [a] -> Main.Maybe a
safehead [] = Main.Nothing
safehead xs = Main.Just (head xs)


nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n


int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat (n+1) = Succ (int2nat n)


add_1 :: Nat -> Nat -> Nat
add_1 m n = int2nat(nat2int m + nat2int n)


add_2 :: Nat -> Nat -> Nat
add_2 Zero n = n
add_2 (Succ m) n = Succ(add_2 m n) 

--Circle :: Float -> Shape
--Rect :: Float -> Float -> Shape

size :: Expr -> Int
size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

--Val :: Int -> Expr
--Add :: Expr -> Expr -> Expr
--Mul :: Expr -> Expr -> Expr

occurs_1 :: Int -> Tree -> Prelude.Bool
occurs_1 m (Leaf n) = m == n
occurs_1 m (Node l n r) = m == n
                        || occurs_1 m l
                        || occurs_1 m r

occurs_2 :: Int -> Tree -> Prelude.Bool
occurs_2 m (Leaf n) = m == n
occurs_2 m (Node l n r) | m == n = Prelude.True
                        | m < n = occurs_2 m l
                        | m > n = occurs_2 m r

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r


main = do
    print('s')

    -- todo exercises