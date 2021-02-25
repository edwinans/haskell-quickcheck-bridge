module Split where

split :: Char -> String -> [String]
split _ _ = undefined

unsplit :: Char -> [String] -> String
unsplit _ _ = undefined

prop_split_unsplit :: Char -> String -> Bool
prop_split_unsplit c str = unsplit c (split c str) == str
