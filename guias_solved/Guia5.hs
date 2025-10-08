------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 1.1

{--
longitud :: [t] -> Integer

longitud s = length (s)
--}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 1.2

ultimo :: [t] -> t

ultimo [s] = s
ultimo (x:xs) = ultimo xs

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 1.3

principio :: [t] -> [t]

principio [x] = []
principio (x:xs) = [x] ++ principio(xs) 

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 1.4

reverso :: [t] -> [t]


reverso (x:xs) = reverso (xs):[x]
reverso [x] = 

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 8.2

cantidadDeAparicionesFila :: Integer -> [Integer] -> Integer
cantidadDeApariciones :: Integer -> [[Integer]] -> Integer

cantidadDeAparicionesFila e [] = 0
cantidadDeAparicionesFila e (x:xs)
   | e == x = 1 + cantidadDeAparicionesFila e xs
   | e /= x = 0 + cantidadDeAparicionesFila e xs

cantidadDeApariciones e (fila:[]) = cantidadDeAparicionesFila e fila
cantidadDeApariciones e (fila:filas) = (cantidadDeAparicionesFila e fila) + (cantidadDeApariciones e filas)

