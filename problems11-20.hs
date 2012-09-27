--problems11-20.hs
import Data.List

a = [1..20]
b = ['a'..'z']
c = "abHba"
d = "aaabba"


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

--problem11
-- encodeModified list
--   | fst(head(encode(list))) > 1 = (Multiple )
  
  
  
  
  
  
  
  
  
  