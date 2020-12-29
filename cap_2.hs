double x = x + x
quadruple x = double(double x)
factorial n = product[1..n]
average ns = sum ns `div` length ns


solution ns = a `div` length xs
    where 
        a = 10
        xs = [1..10]

{-| various definitions of last -}
last_1 ns = last ns
last_2 ns = head(reverse ns)
last_3 ns = ns !! (length ns - 1)
last_4 ns = reverse ns !! 0


{-| define init that removes the last element in two ways -}
inti_1 ns = init ns
inti_2 ns = reverse(tail(reverse ns))
inti_3 ns = take(length ns - 1) ns


main = do
    -- rapid calculator 
    print(2+3*4);
    print((2+3)*4);
    -- call of math functions
    print(sqrt(3^2 + 4^2));

    -- print the first element of a list
    print(head[1,2,3,4,5]);
    -- remove the first element from a list
    print(tail[1..5]);
    -- selection of the nth element of list
    print([1..5] !! 2);
    -- take the first nth elements of list
    print(take 3 [1..5]);

    -- drop the first n elements from a list
    print(drop 3 [1..5]);
    -- calculate the length of the list
    print(length [1..5]);
    -- calculate the sum of a list of numbers
    print(sum[1..5]);

    -- calculate the product of a list of numbers
    print(product[1..5]);
    -- append two lists
    print([1..3] ++ [4,5]);
    -- reverse a list
    print(reverse[1,2,3,4,5]);

    -- call of own function defined at the begin of the script
    print(quadruple 10);

    -- call of Prelude and own function
    print(take(double 2) [1..6]);

    -- call of factorial defined at the begin of the script
    print(factorial 10);
    -- call of average defined at the begin of the script
    print(average[1..5]);

    -- solution of the last slide with corrected errors 
    print(solution[1..5]);

    {-| Get the last element of a list -}
    print(last_3[1..10])

    {-| Remove the last element from a list -}
    print(inti_3[1..10])
