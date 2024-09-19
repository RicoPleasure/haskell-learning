{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
import Distribution.Simple.Utils (xargs)
import Language.Haskell.TH (prim)
import Data.ByteString.Builder.Prim (primFixed)

-- (a)

firstLast xs = (head xs, last xs)

-- (b)

name xs = (head xs, last xs)

-- (c)

oneName list = if null list then "The list cannot be empty" else  "So your name is: " ++ show (name list)

-- (d)

lastName ys = if null ys then 0 else length (last ys)

-- (e)

inString :: Char -> String -> Bool
inString w str = w `elem` str

-- (f) 

listaParImpar :: [(Int, Int)] -> [(Int, Int)] 
listaParImpar xm 
    | even (length xm) = tail xm
    | odd (length xm) = init xm

-- (g) 
-- i
juncaoListas :: [Int] -> [Int]-> [Int]  
juncaoListas primeiraLista segundaLista = primeiraLista ++ segundaLista

--ii
numeroAlunos :: [Int] -> [Int] -> Int
numeroAlunos primeiraLista segundaLista = length (juncaoListas primeiraLista segundaLista)

--iii
diferencaAlunos :: [Int] -> [Int] -> Int
diferencaAlunos primeiraLista segundaLista 
    | primeiraLista > segundaLista = length primeiraLista - length segundaLista
    | segundaLista > primeiraLista = length segundaLista - length primeiraLista
    | otherwise = 0
    
--iv
pertenceAoTurno :: [Int] -> Int -> String
pertenceAoTurno listaAlunos numeroAluno 
    | numeroAluno `elem` listaAlunos = "Yes"
    | otherwise = "No"

-- (h) 

juntaListas :: [Int] -> [Int] -> [Int]
juntaListas primeiraLista segundaLista 
    | length primeiraLista > length segundaLista = segundaLista ++ primeiraLista
    | length segundaLista > length primeiraLista = primeiraLista ++ segundaLista
    | otherwise = primeiraLista ++ segundaLista

-- (i) 

juntaListasHead :: [Int] -> [Int] -> (Int, [Int])
juntaListasHead xy xs = (head xy, xs)

-- (j) 

parListas :: [Int] -> [Int] -> [Int]
parListas primeiraLista segundaLista 
    | head primeiraLista > head segundaLista = segundaLista ++ primeiraLista
    | head segundaLista > head primeiraLista = primeiraLista ++ segundaLista
    | otherwise = primeiraLista ++ segundaLista


-- (k) 

inicial ws = head (head ws) : ". " ++ last ws

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

lados :: (Int,Int) -> Int -> (Int,Int)
lados (x,y) lado = (x + lado, y)

-- (p)

quadrados :: ((Int,Int), Int) -> ((Int,Int), Int) -> Int 
quadrados ((x,y),z) ((a,b),c)
    | y >= b = z*z
    | b > y = c*c