qsort [] = []
qsort (x:xs) = qsort ys ++ [x] ++ qsort zs
    where ys = [a | a <- xs, a <= x]
          zs = [b | b <- xs, b > x]
            

main = do
    -- very simple way to sum number from 1 to 10
    let res = sum([1..10]);
    print(res);

    -- call the quick sort algorithm defined in three lines
    let x = qsort([1, -2 ,2,3]);
    print(x);
