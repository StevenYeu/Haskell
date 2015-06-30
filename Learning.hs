import Data.List

count l x = foldl (\acc elmet -> if elmet == x then acc+1 else acc) 0 l

sum1 :: Num a => [a] -> a
sum1 [] = 0
sum1 (x:xs) = x + sum1(xs)

-- Problem # 1
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- Problem # 2
myButLast :: [t] -> t
myButLast l = last (init l)

-- Problem # 3
elementAt ::  [t] -> Int -> t
elementAt (x:xs) i = if i == 1 then x else elementAt xs (i-1)

-- Problem # 4
myLength :: [t] -> Int
myLength l = foldl (\acc elmet -> acc +1) 0 l

-- Problem # 5
myReverse :: [t] -> [t]
myReverse l = foldl (\acc elmet -> elmet : acc) [] l

-- Problem # 6
isPalindrome :: (Eq t) => [t] -> Bool
isPalindrome l = if myReverse l == l then True else False

-- Problem # 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

--flatten (Elem x) = [x]
--flatten (List (x:xs)) = flatten x ++ flatten (List xs)
--flatten (List []) = []

-- Problem # 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = if x == head (compress xs) then compress xs else x:compress xs

-- Problem # 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` (head (pack xs))
              then (x:(head (pack xs))):(tail (pack xs))
              else [x]:(pack xs)

-- Problem #10
encode :: (Eq a) => [a] -> [(Int,a)]
encode x = map (\l -> (length l, head l)) (group x)

-- Problem # 14
dupli :: [a] -> [a]
dupli l = foldl (\acc elmet -> acc ++ [elmet,elmet]) [] l
