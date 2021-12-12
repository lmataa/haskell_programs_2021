-- head maybe implementation for lists
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

-- head maybe implementation in lambda calculus
headMaybe' :: [a] -> Maybe a
headMaybe' = \xs -> case xs of
  [] -> Nothing
  (x : _) -> Just x
