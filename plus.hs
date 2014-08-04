{-
    To import your parsing library, put this line at the top of it:

    module Parser ( Parser( P ), parse, (+++) ) where
-}

import Parser

getA :: Parser Char
getA = P (\inp -> case inp of
                    ('a':xs) -> Just ('a',xs)
                    _ -> Nothing
        )

getB :: Parser Char
getB = P (\inp -> case inp of
                    ('b':xs) -> Just ('b',xs)
                    _ -> Nothing
        )
