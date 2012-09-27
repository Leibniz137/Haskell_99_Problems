--help.hs

module Help (NestedList a)
where 
  
data Elem a = a
data NestedList a = Elem a | List [NestedList a]
