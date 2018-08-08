module User.Ds2.Split where

split_i :: Eq a => a -> [a] -> [a] -> [[a]] -> [[a]]
-- stores prefix in reverse as we don't have a "right-hand" cons,
-- as that would break immutability of the list cell.

split_i sep (ac:as) prefix resultmem
  | ac == sep = (reverse prefix):(split_i sep as [] [])
  | otherwise = split_i sep as (ac:prefix) resultmem

-- ran out of string to process...
split_i _ [] prefix resultmem = (reverse prefix):resultmem

split :: Eq a => a -> [a] -> [[a]]
split sep input = (split_i sep input [] [])

