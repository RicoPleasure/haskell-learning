module Ficha2 where

-- Ficha 2 LI - Haskell 

-- Exercício 1

data Movimento = Norte | Sul | Este | Oeste deriving Show

type Ponto = (Double, Double)

-- (a)

-- move :: Ponto -> Movimento -> Ponto
-- move (positionX,positionY) Norte = (positionX,positionY+1)
-- move (positionX,positionY) Sul = (positionX,positionY-1)
-- move (positionX,positionY) Este = (positionX+1,positionY)
-- move (positionX,positionY) Oeste = (positionX-1,positionY)

-- (b)

dist :: Ponto -> Ponto -> Double
dist (positionX1,positionY1) (positionX2,positionY2) = sqrt ((positionX2-positionX1)^2+(positionY2-positionY1)^2)

-- Distância de Manhattan

dist' :: Ponto -> Ponto -> Double
dist' (positionX1,positionY1) (positionX2,positionY2) = abs (positionX1 - positionX2) + abs (positionY1 - positionY2)

-- (c)

relativeSul :: Ponto -> Ponto -> Ponto
relativeSul (posX1,posY1) (posX2,posY2)
  | posY1 >= posY2 = (posX1,posY1)
  | posY2 > posY1 = (posX2,posY2)

type LadoJanela = Double

-- Exercício 2

move :: Ponto -> Movimento -> LadoJanela -> Ponto
move (positionX, positionY) direction ladoJanela = case direction of
   Norte -> (positionX, min (positionY + 1) ladoJanela)
   Sul   -> (positionX, max (positionY - 1) 0)
   Este  -> (min (positionX + 1) ladoJanela, positionY)
   Oeste -> (max (positionX - 1) 0, positionY)

-- Exercício 3 

originTopRight :: Ponto -> LadoJanela -> Ponto
originTopRight (posX, posY) ladoJanela = (ladoJanela - posX, ladoJanela - posY)

-- Exercício 4

-- originCenter :: Ponto -> LadoJanela -> Ponto
-- originCenter (x, y) ladoJanela = (1,2)

-- Exercício 5

type Velocidade = Double
type Tempo = Double

-- move'' :: Ponto -> Velocidade -> Tempo -> Ponto 
-- move'' (x,y) v dt = (x + (v*dt),y)

-- Exercício 6

-- move''' :: Ponto -> Velocidade -> Tempo -> Ponto 
-- move''' (x,y) v dt = (x,y + (v*dt))

-- Exercício 7

type Velocidade' = (Double, Double)

-- move'''' :: Ponto -> Velocidade' -> Tempo -> Ponto 
-- move'''' (x,y) (xv,yv) dt = (x + (xv * dt), y+(yv * dt))

-- Exercício 8

data Figura =
  Circulo Ponto Double |
  Rectangulo Ponto Ponto |
  Quadrado Ponto Double deriving (Show, Eq)

-- (a) 

{- isInside :: Ponto -> Figura -> Bool
isInside (x,y) (Circulo (xc,yc) r) = (xc-x)^2 + (yc-y)^2 <= r^2 
isInside (x,y) (Quadrado (x1,y1) lado) = x <= x1 + lado && x >= x1 && y <= y1 + lado && y >= y1
isInside (x,y) (Rectangulo (x1,y1) (x2,y2)) = 
  x >= min x1 x2 && x <= max x1 x2 &&
  y >= min y1 y2 && y <= max y1 y2 -}


-- (b)

{- menorQuadrado :: Figura -> Figura
menorQuadrado (Circulo (xc, yc) r) = Quadrado (xc-r,yc-r) (2*r)
menorQuadrado (Quadrado (x1,y1) lado) = Quadrado (x1,y1) lado
menorQuadrado (Rectangulo (x1,y1) (x2,y2)) = Quadrado (min x1 x2, min x1 y2) (max (abs (x2 - x1)) (abs (y2 - y1)))

-- (c)

maiorCirculo :: Figura -> Figura
maiorCirculo (Circulo (xc, yc) r) = Circulo (xc, yc) r
maiorCirculo (Quadrado (x1,y1) lado) = Circulo (x1 + lado / 2,y1 + lado/2) (dist (x1 + (lado/2),y1 + (lado/2)) (x1,y1))
-- Não vou fazer o rectangulo pois exige pensar demais do mesmo
 -}
-- (d)

{- contida :: Figura -> Figura -> Bool
contida (Circulo (xc1, yc1) r1) (Circulo (xc, yc) r) =
contida (Quadrado (x1,y1) lado) (Quadrado (x2,y2) lado) =  -}

-- Exercício 9

-- OK, LOL