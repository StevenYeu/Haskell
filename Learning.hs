import Data.List

-- Sieve of Eratosthenes
sieve :: Integral a => a -> [a]
sieve n = [ x| x <-[2..n], all (\f-> x `mod` f /= 0) [2..floor(sqrt(fromIntegral x))] ]

-- A filter funciton
filter' :: (a->Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
   | p x = x : filter' p xs
   | otherwise = filter' p xs

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

-- Problem # 10
encode :: (Eq a) => [a] -> [(Int,a)]
encode x = map (\l -> (length l, head l)) (group x)

-- Problem # 11
data ListElemt a = Single a | Multiple Int a
  deriving (Show)
encodeModified :: (Eq a) => [a] -> [ListElemt a]
encodeModified xs  = map helper (encode xs)
  where helper (1,a) = Single a
        helper (n,a) = Multiple n a

-- Problem # 12


-- Problem # 14
dupli :: [a] -> [a]
dupli l = foldl (\acc elm -> acc ++ [elm,elm]) [] l

-- Problem # 15
relpi :: [a] -> Int -> [a]
relpi l n = concatMap (replicate n) l

-- Problem # 16
dropEvery :: [a] -> Int -> [a]
-- Assuming 0 is first index
dropEvery x n = fst $ foldl helper ([],0) x
   where helper (acc,index) elemt = if index == n then (acc,0) else (acc++[elemt],index+1)

-- Problem # 17
split :: [a] -> Int -> ([a],[a])
split l x = fst $ foldl helper (([],[]),0) l
   where helper ((left,right),i) elm = if i < x then ((left++[elm],right),i+1) else ((left,right++[elm]),i+1)

-- Problem #18
slice :: [a] -> Int -> Int -> [a]
slice [] x y = []

-- Problem #19
rotate :: [a] -> Int -> [a]
rotate [a] _ = [a]
