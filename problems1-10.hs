--problems1-10.hs


a = [1..20]
b = ['a'..'z']


--problem1
myLast:: [a] -> a
myLast list 
        | length(list) < 2 = head(list)
        | otherwise = myLast(tail(list))
        
        
--problem2
myButLast:: [a] -> a
myButLast list
        | length(list) < 3 = head(list)
        | otherwise = myButLast(tail(list))

--problem3
elementAt:: [a] -> Int -> a
elementAt list k
  | length(list) <= k = myLast(list)
  | otherwise = elementAt (init(list)) k

--problem4
