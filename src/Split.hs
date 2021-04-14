module Split where

split :: Char -> String -> [String]
split del s = splitAux del s [] []
  where
    splitAux :: Char -> String -> String -> [String] -> [String]
    splitAux _ [] acc res = reverse (reverse acc : res)
    splitAux del (c : cs) acc res
      | c == del = splitAux del cs [] (reverse acc : res)
      | otherwise = splitAux del cs (c : acc) res

unsplit :: Char -> [String] -> String
unsplit del s_list = drop 1 $ foldl (\x y -> x <> [del] <> y) [] s_list

prop_split_unsplit :: Char -> String -> Bool
prop_split_unsplit c str = unsplit c (split c str) == str

--- >>> split '/' "usr/home/edwin"
-- ["usr","home","edwin"]

--- >>> unsplit '/' ["usr","home","edwin"]
-- "usr/home/edwin"
