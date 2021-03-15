type Parser a = String -> [(a, String)]

result :: a -> Parser a
result v = \inp -> [(v, inp)]

zero :: Parser a
zero = \inp -> []

item :: Parser Char
item :: \inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x,xs)]

seq :: Parser a -> Parser b -> Parser (a,b)
p `seq` q = \inp -> [((v,w), inp'') | (v, inp') <- p inp
                                    , (w, inp'') <- q inp]

bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = \inp -> concat [f v inp' | (v, inp') <- p inp]

seqM :: Parser a -> Parser b -> Parser (a,b)
p `seqM` q = p `bind` \x ->
             q `bind` \y ->
             return (x, y)

sat :: (Char -> Bool) -> Parser Char
sat p = item `bind` \x ->
        if p x then return x else zero

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

plus :: Parser a -> Parser a -> Parser a
p `plus` q = \inp -> (p inp ++ q inp)

letter :: Parser Char
letter = lower `plus` upper

alphanum :: Parser Char
alphanum = letter `plus` digit

word :: Parser String
word = newWord `plus` return ""
        where 
                newWord = letter `bind` \x ->
                          word  `bind` \xs ->
                          return (x:xs)

