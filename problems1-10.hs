--problems1-10.hs
import Data.List


a = [1..20]
b = ['a'..'z']
c = "abHba"
d = "aaabba"


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

f = list d


--problem10
encode list = [(myLength(x), head(x)) | x <- pack(list)]













  
  