{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
import Distribution.Simple.Utils (xargs)

-- (a)

firstLast xs = (head xs, last xs)

-- (b)

name xs = (head xs, last xs)

-- (c)

oneName list = if null list then "The list cannot be empty" else  "So your name is: " ++ show (name list)

-- (d)

lastName ys = if null ys then 0 else length(last ys)

-- (e)

inString :: Char -> String -> Bool
inString w str = w `elem` str

-- (f) Pensei

-- (g) Pensei

-- (h) Pensei

-- (i) Pensei

-- (j) Pensei

-- (k) 

inicial ws = head(head ws) : ". " ++ last ws

-- (l)

cabeca :: [(Int,Int)] -> Int
cabeca xs
    | null xs = 0
    | otherwise = snd (head xs)

-- (m)

ultimo :: [(Int, Int)] -> Int
ultimo la
    | null la   = 0
    | otherwise = fst (last la) + snd (last la)
    

-- (n)

pessoas :: [(String,Int)] -> String
pessoas pa 
    | snd (head pa) > snd (last pa) = fst (last pa)
    | snd (last pa) > snd (head pa) = fst (head pa)
    | snd (head pa) == snd (last pa) = "They have the same age"
    | otherwise = ""
    
-- (o)