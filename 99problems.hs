--99problems.hs
import Data.List
import System.Random
import Data.Char



a = [1..20]
b = ['a'..'z']
c = "abHba"
d = "aaabba"
e = list d
f = List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]
g = List [List [ List [Elem 5, Elem 4], Elem 3], Elem 2, Elem 1]
h = List [Elem 1, Elem 2, Elem 3]
i = [Multiple 5 'a',Multiple 2 'b',Single 'c']
j = "aaaaabbaccac"
k = "aaaaa"
l = [1..10]




--problem1
myLast :: [a] -> a
myLast list 
        | length(list) < 2 = head(list)
        | otherwise = myLast(tail(list))
        
        
--problem2
myButLast :: [a] -> a
myButLast list
        | length(list) < 3 = head(list)
        | otherwise = myButLast(tail(list))


--problem3
elementAt :: [a] -> Int -> a
elementAt list k
  | length(list) <= k = myLast(list)
  | otherwise = elementAt (init(list)) k


--problem4
myLength ::[a] -> Int
myLength []=0
myLength list = 1+myLength(init(list))


--problem5
myReverse ::[a] -> [a]
myReverse [] = []
myReverse list = last(list):myReverse(init(list))


--problem6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list
  | (myReverse(list) == list) = True
  | otherwise = False
  
  
--problem7
data NestedList a = Elem a | List [NestedList a]
                deriving (Show)

flatten :: NestedList t -> [NestedList t]
flatten (List []) = []
flatten (List a)
  | typeCheck (head a) = (head a):flatten(List (tail a))
  | otherwise = flatten((head a))++flatten(List (tail a))
  
typeCheck (Elem a) = True
typeCheck (List a) = False

nestHead (List x) = head x

nestTail (List x) = tail x




--problem8
compress :: Eq a => [a] -> [a]
compress list
 | myLength(list) <= 1 = list
 | head(list) == head(tail(list)) = compress(tail(list))
 | otherwise = head(list):compress(tail(list))
 
--problem9
pack :: Eq a => [a] -> [[a]]
pack array = yams(list(array))

list :: [a] -> [[a]]
list array
  | myLength(array) <= 1 = [array]
  | otherwise = [head(array)]:list(tail(array))

second :: [a] -> a
second [] = error "too $hort!"
second [a] = error "too $hort!"
second (x:y:_) = y

yams :: Eq a => [[a]] -> [[a]]
yams array
  | myLength(array) <= 1 = array
  | head(head(array)) == head(second(array)) = yams([head(array)++second(array)]++tail(tail(array)))
  | otherwise = head(array):yams(tail(array))



--problem10
encode :: Eq a => [a] -> [(Int, a)]
encode list = [(myLength(x), head(x)) | x <- pack(list)]


--problem11
data ListElement = Single Char | Multiple Int Char
    deriving (Show)

encodeModified :: [Char] -> [ListElement]
encodeModified list = [if (fst x)>1 then (Multiple (fst x) (snd x)) else Single (snd x) | x <- encode(list)]


--problem12
decodeModified :: [ListElement] -> [Char]
decodeModified array = merge [if lE_typeCheck x then lE_multichar x else [lE_single x] | x <- array] 

lE_typeCheck (Multiple a b) = True
lE_typeCheck (Single a) = False
lE_multichar (Multiple a b) = replicate a b
lE_multiple (Multiple a b) = (a,b)
lE_single (Single a) = a 

merge [] = ""
merge array = (head array)++merge (tail array)


--problem13
encodeDirect :: [Char] -> [ListElement]
encodeDirect array = convert (encodeDirect_pt2 (initi array))

convert tuple_array = [ if fst x > 1 then Multiple (fst x) (snd x) else Single (snd x) | x <- tuple_array ]

encodeDirect_pt2 i_array
  | tail(i_array) == [] = i_array
  | snd (head i_array) == snd (head (tail i_array)) = encodeDirect_pt2 (encAdd i_array)
  | otherwise = (head i_array):encodeDirect_pt2 (tail i_array)

encAdd array
  | myLength array > 1 = ((fst (head array)) + (fst (head (tail array))), snd (head array)):(tail (tail array))
  | otherwise = array
  
initi array = [ (1, x) | x <- array]


--problem14
dupli:: [a] -> [a]
dupli [] = []
dupli array = [head array, head array]++dupli (tail array)


--problem15
repli :: [a] -> Int -> [a]
repli [] n = []
repli array n = (replicate n (head array)) ++ (repli (tail array) n)


--problem16
dropEvery :: [a] -> Int -> [a]
dropEvery [] n = []
dropEvery array n = (take (n-1) array)++(dropEvery (drop n array) n)



--problem17
mySplit :: [a] -> Int -> ([a], [a])
mySplit array n = (take n array, drop n array)


--problem18
slice :: [a] -> Int -> Int -> [a]
slice array n1 n2 = fst (mySplit (drop (n1-1) array) (n2-1))

--problem19
rotate :: [a] -> Int -> [a]
rotate array n = (slice array (rot_help array (n+1)) (myLength array))++(slice array 0 (rot_help array (n+1)))

rot_help array n
  | n >= 0 = n
  | otherwise = (myLength array)+n
  


--problem20
removeAt :: Int -> [a] -> (a, [a])
removeAt i array = (at_index i array, (slice array 0 i)++(slice array (i+1) (myLength array)))

at_index i array = head (slice array i i)




--problem21
insertAt :: a -> [a] -> Int -> [a]
insertAt el array i = (slice array 0 i)++(el:(slice array i (myLength array)))



--problem22
range:: Int -> Int -> [Int]
range i j = [i..j]


--problem23


--problem24


--problem25


--problem26
--combinations k array

--lexicographic permutations of len(k) of n numbers
-- in_lex [] = True
-- in_lex array
--  | head array <= head tail array = in_lex (tail array)
--  | 

nChooseK n k = [1..nchoosek]
  where nchoosek = fact n / ((fact k) * fact (n-k))

kLists_lenN n 0 = []
kLists_lenN n k = [0..n-1]:kLists_lenN n (k-1)

lexx n = [ [a,b,c] | a <- [0..n-1], b <- [0..n-1], c <- [0..n-1], (a<b), (b<c)]

array ## ele = head [ x | x <- [0..(length array)-1], (array !! x) == ele]




  

fact n = foldl (*) 1 [1..n]
















  