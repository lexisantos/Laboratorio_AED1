-- Ejercicio 1.
{--
f :: Int -> Int
g :: Int -> Int

f x | x == 1 = 8
    | x == 4 = 131
    | x == 16 = 16

g x | x == 8 = 16
    | x == 16 = 4
    | x == 131 = 1

h :: Int -> Int
k :: Int -> Int

h y = f(g y)

k y = g(f y)

--}
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 2.
-- a)
absoluto :: Integer -> Integer

absoluto x | x < 0 = -x
           | otherwise = x

-- b)
maximoAbsoluto :: Integer -> Integer -> Integer

maximoAbsoluto x y 
    | (absoluto x < absoluto y) = absoluto y
    | (absoluto y < absoluto x) = absoluto x
    | otherwise = undefined 

-- c)
maximo3 :: Integer -> Integer -> Integer -> Integer

maximo3 x y z
    | x <= z && y <= z =z
    | x <= y && z <= y =y
    | y <= x && z <= x =x
    | otherwise =undefined

-- d)
algunoEsCero :: Float -> Float -> Bool

{--
algunoEsCero x y
  | x == 0 || y == 0 =True
  | otherwise =False
--}

algunoEsCero x y = (x == 0 || y == 0) 

-- e)
ambosSonCero :: Float -> Float -> Bool

ambosSonCero x y = (x == 0 && y == 0) 

-- f)
enMismoIntervalo :: Float -> Float -> Bool
 
enMismoIntervalo x y
   | (3 >= x && 3 >= y) =True
   | (7 >= x && 7 >= y) =True
   | (7 < x && 7 < y) =True
   | otherwise =False

-- g) 
sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos2 :: Integer -> Integer -> Integer

sumaDistintos2 x y 
   | (x /= y)  = x+y
   | otherwise = x

sumaDistintos x y z = sumaDistintos2 x (sumaDistintos2 y z)

-- h)
esMultiploDe :: Integer -> Integer -> Bool

esMultiploDe n1 n2 = (mod n2 n1 == 0)

-- i)
digitoUnidades :: Integer -> Integer

{--
digitoUnidades n 
   | n < 10 =n
   | otherwise =digitoUnidades(mod n 10) 
--}

digitoUnidades n = mod n 10

-- j)
digitoDecenas :: Integer -> Integer
digitoDecenas n = mod (div n 10) 10

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 3.
estanRelacionados :: Integer -> Integer -> Bool

--estanRelacionados a b = (mod (a^2) ((-b)*a) == 0)

estanRelacionados a b 
   | a == 0 || b == 0 =undefined
   | esMultiploDe a b  =True -- como a /= 0, basta pedir a = -k*b  
   | otherwise =False
  
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 4.
-- a)

productoInterno :: (Float, Float) -> (Float, Float) -> Float

productoInterno (a1, a2) (b1, b2) = a1*b1 + a2*b2

-- c)

distancia :: (Float, Float) -> (Float, Float) -> Float

distancia (a1, a2) (b1, b2) = sqrt ((a1 - b1)^2 + (a2 - b2)^2)

-- d)

sumaTerna :: (Float, Float, Float) -> Float

sumaTerna (x, y, z) = x + y + z

-- e)

sumarSoloMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer 
esMultiploDe_int :: Integer -> Integer -> Integer

esMultiploDe_int n1 n2
   | mod n1 n2 == 0 =1
   | otherwise =0

sumarSoloMultiplos (x, y, z) n = (esMultiploDe_int x n)*x + (esMultiploDe_int y n)*y + (esMultiploDe_int z n)*z

-- f)

posPrimerPar :: (Integer, Integer, Integer) -> Integer

posPrimerPar (x, y, z)
   | esMultiploDe 2 x =1
   | esMultiploDe 2 y =2
   | esMultiploDe 2 z =3
   | otherwise =4

-- g)

crearPar :: a -> b -> (a, b)

crearPar x y = (x, y)

-- h)

invertir :: (a, b) -> (b, a)

invertir (x, y) = (y, x)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 5.

f :: Integer -> Integer

f n
   | n <= 7 =n^2
   | n > 7 = 2*n - 1
   
g :: Integer -> Integer

g n
   | mod n 2 == 0 = div n 2
   | otherwise = 3*n + 1

todosMenores :: (Integer, Integer, Integer) -> Bool

todosMenores (t0, t1, t2)
   | (f t0 > g t0) && (f t1 > g t1) && (f t2 > g t2) =True
   | otherwise =False

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 6.

type Anio = Integer
type EsBisiesto = Bool

bisiesto :: Anio -> EsBisiesto

bisiesto año
   | ((mod año 100 == 0 && mod año 400 /= 0) || mod año 4 /= 0) =False
   | otherwise =True

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 7.
type Punto3D = (Float, Float, Float)

absoluto_fl :: Float -> Float

absoluto_fl x | x < 0 = -x
              | otherwise = x

distanciaManhattan :: Punto3D -> Punto3D -> Float

distanciaManhattan (p0, p1, p2) (q0, q1, q2) = absoluto_fl (p1-q1) + absoluto_fl (p0-q0) + absoluto_fl (p2 - q2)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 8.

sumaUltimosDosDigitos :: Integer -> Integer

sumaUltimosDosDigitos n = digitoDecenas (absoluto n) + digitoUnidades (absoluto n)

comparar :: Integer -> Integer -> Integer

comparar a b | (sumaUltimosDosDigitos (a) < sumaUltimosDosDigitos (b)) = 1
             | (sumaUltimosDosDigitos (a) > sumaUltimosDosDigitos (b)) = -1
             | otherwise = 0

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
