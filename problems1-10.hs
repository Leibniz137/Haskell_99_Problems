--problems1-10.hs


a = [1..20]


--problem1
myLast list 
        | length(list)< 2 = return list
        | otherwise = myLast(tail(list))
        
        
--problem2
myButLast list
        | length(list) < 3 = return [head(list)]
        | otherwise = myButLast(tail(list))

--problem3 in progress
elementAt:: ([Int],Int) -> 
elementAt(list, k)
  | length(list) <= k = return(last(list))
  | otherwise = elementAt(init(list), k)

