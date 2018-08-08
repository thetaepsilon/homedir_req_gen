module User.Ds2.SetMisc where
-- misc helpers for Data.Set

import Data.Set (Set, member)

-- reverse the arguments of the member function,
-- to bake in a set with partial application first.
-- this lets us get a predicate function,
-- which answers yes if the element is in the set.
isInSet :: Ord a => Set a -> a -> Bool
isInSet set q = member q set

