import Data.Time
import Control.Concurrent

listToSort :: [Int]
listToSort = [13, 2, 3, 14, 17, 4, 1, 5, 16, 12, 9, 10, 15, 8, 7, 11, 18, 19, 6, 20]

--bubble sort
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort lst = if bpassed == lst then lst
                 else bubbleSort bpassed
                 where bpassed = bubblePass lst


bubblePass :: (Ord a) => [a] -> [a]
bubblePass [] = []
bubblePass [x] = [x]
bubblePass (x1:x2:xs) = if x1 > x2
                        then [x2] ++ (bubblePass ([x1] ++ xs))
                        else [x1] ++ (bubblePass ([x2] ++ xs))

-- insertion sort
insertionSort:: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insert x (insertionSort xs)


insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x lst@(y:ys) = if x <= y then x:lst else y:(insert x ys)

-- selection sort
selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort (x:xs) =
    let (y, ys) = leastUnsorted (x:xs)
    in y : selectionSort ys

leastUnsorted :: (Ord a) => [a] -> (a, [a])
leastUnsorted [x] = (x, [])
leastUnsorted (x:xs) =
    let (y, ys) = leastUnsorted xs
    in if x <= y then (x, xs) else (y, x:ys)


-- merge sort
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort [x, y] = [(min x y), (max x y)]
mergeSort lst = merge (mergeSort leftL) (mergeSort rightL)
                where leftL = take splitPoint lst
                      rightL = drop splitPoint lst
                      splitPoint = (length lst) `div` 2

merge :: (Ord a) => [a] -> [a] -> [a]
merge l1 [] = l1
merge [] l2 = l2
merge lst1@(x:xs) lst2@(y:ys) = if x < y 
                                then x:(merge xs lst2)
                                else y:(merge lst1 ys)


main = do
    putStrLn $ "Unsorted: " ++ show listToSort
    
    a <- getCurrentTime
    putStrLn $ "Sorted: " ++ show (bubbleSort listToSort)
    b <- getCurrentTime
    putStrLn "This is time difference: "
    print(diffUTCTime b a)
    
    a <- getCurrentTime
    putStrLn $ "Sorted: " ++ show (insertionSort listToSort)
    b <- getCurrentTime
    putStrLn "This is time difference: "
    print(diffUTCTime b a)
    
    a <- getCurrentTime
    putStrLn $ "Sorted: " ++ show (selectionSort listToSort)
    b <- getCurrentTime
    putStrLn "This is time difference: "
    print(diffUTCTime b a)
    
    a <- getCurrentTime
    putStrLn $ "Sorted: " ++ show (mergeSort listToSort)
    b <- getCurrentTime
    putStrLn "This is time difference: "
    print(diffUTCTime b a)
    
