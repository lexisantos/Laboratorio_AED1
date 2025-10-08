-- Ejercicio 1. Fibonacci
fib :: Integer -> Integer

{-- fib n 
    | n == 0 =0
    | n == 1 =1
    | otherwise = fib(n-1) + fib(n-2)
--}

fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 2. parteEntera de un flotante
parteEntera :: Float -> Integer

parteEntera x
        | x < 1 =0
        | otherwise =1+parteEntera(x-1)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 3. esDivisible

{--
esDivisible :: Integer -> Integer -> Bool

esDivisible n m
   | m == 1 = True
   | n == m = True
   | m > n  = False
   | otherwise = esDivisible ((n - m) m)
--} 

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 4. Suma Impares
sumaImpares :: Integer -> Integer

sumaImpares 1 = 1
sumaImpares n = 2*n-1 + sumaImpares(n-1)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 5. medioFact

medioFact :: Integer -> Integer

medioFact 0 = 1
medioFact 1 = 1
medioFact n = n*medioFact(n - 2)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 6. todosDigitosIguales
todosDigitosIguales :: Integer -> Bool

todosDigitosIguales n
   | div n 10 == 0 =True
   -- | div n 100 == 0
   | otherwise = (mod n 10 == mod (div n 10) 10) && todosDigitosIguales(div n 100)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 7. iesimoDigito

cantDigitos :: Integer -> Integer
-- sacarUnidades :: Integer -> Integer
iesimoDigito :: Integer -> Integer  -> Integer

{-- 
sacarUnidades x = x div 10 -- y usarlo adentro de cantDigitos   
--}

cantDigitos n
    | n < 10 =1
    | otherwise = 1 + cantDigitos(div n 10)

iesimoDigito n i = mod (div n (10^(cantDigitos (n) - i))) 10 

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 8. sumaDigitos 

sumaDigitos :: Integer -> Integer

sumaDigitos n
   | cantDigitos n == 1 =n
   | otherwise = mod n 10 + sumaDigitos (div n 10)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 9. esCapicua

esCapicua :: Integer -> Bool
-- probar de agregar una función auxiliar
esCapicua n | cantDigitos n == 1 =True
  | otherwise = (div n (10^((cantDigitos n) - 1)) == mod n 10) && esCapicua(div (mod n (10^((cantDigitos n) - 1))) 10)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 10
-- a)

f1 :: Integer -> Integer

f1 0 = 1
f1 n = 2^n + f1 (n-1)

-- b)

f2 :: Integer -> Integer -> Integer

f2 1 q = q
f2 n q = q^n + f2 (n-1) q

-- c)

f3 :: Integer -> Integer -> Integer

f3 0 q = 0
f3 n q = f2 (2*n) q

-- d)

f4 :: Integer -> Integer -> Integer

f4 0 q = 0
f4 n q = (q^n)*(f2 n q + 1)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 11
-- Usar fromInteger  :: Integer -> Float

factorial :: Integer -> Integer

factorial n
        | n==0 =1
        | n>0 =n*factorial(n-1)


eAprox :: Integer -> Float

eAprox 0 = 1
eAprox n = 1/(fromInteger(factorial n)) + eAprox (n-1)

e :: Float

e = eAprox 10
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 12

anSucesion :: Integer -> Float

anSucesion 1 = 2.0 --puedo usar Floats o fromInteger sí o sí
anSucesion n = 2.0 + 1/(anSucesion (n-1))

raizDe2Aprox :: Integer -> Float

raizDe2Aprox n = (anSucesion n) - 1

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 13

sumaInt :: Integer -> Integer -> Integer

sumaInt n 1 = n 
sumaInt n m = n^m + sumaInt n (m-1)

sumaExt :: Integer -> Integer -> Integer

sumaExt 1 m = sumaInt 1 m
sumaExt n m = sumaInt n m + sumaExt (n-1) m

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 14

sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaInterna :: Integer -> Integer -> Integer -> Integer


sumaInterna q 1 n = q^(1 + n)
sumaInterna q m n = q^(n + m) + sumaInterna q (m - 1) n 

sumaPotencias q m 1 = sumaInterna q m 1
sumaPotencias q m n = sumaInterna q m n + sumaPotencias q m (n - 1)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 15
sumaDenominador :: Integer -> Integer -> Float
sumaRacionales :: Integer -> Integer -> Float

sumaDenominador n 1 = fromInteger(n)   
sumaDenominador n m = fromInteger(n)/fromInteger(m) + sumaDenominador n (m-1)

sumaRacionales 1 m = sumaDenominador 1 m
sumaRacionales n m = sumaDenominador n m + sumaRacionales (n-1) m

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 16

-- a)
menorDivisor :: Integer -> Integer
menorDivisorDesde :: Integer -> Integer -> Integer

menorDivisorDesde n m | mod n m == 0 = m
   | otherwise = menorDivisorDesde n (m + 1)

menorDivisor n = menorDivisorDesde n 2

-- b)
esPrimo :: Integer -> Bool

esPrimo n | menorDivisor n == n = True
   | otherwise = False

-- c)

iesimoDivisor :: Integer -> Integer -> Integer

iesimoDivisor n i
   | i == 1 =menorDivisor n
   | (iesimoDivisor n (i-1)) + 1 > n =undefined
   | otherwise = menorDivisorDesde n ((iesimoDivisor n (i-1)) + 1)

cantDivisoresDesde :: Integer -> Integer -> Integer

cantDivisoresDesde n m | n==m = 1
   | n>m && (mod n m ==0) = 1 + cantDivisoresDesde n (m + 1)
   | otherwise =0 + cantDivisoresDesde n (m + 1)

cantDivisores :: Integer -> Integer 

cantDivisores n = cantDivisoresDesde n 2


sonCoprimos :: Integer -> Integer -> Bool

sonCoprimosAux :: Integer -> Integer -> Integer -> Bool

sonCoprimosAux n m k
   | (n == k) || (m == k) = not ((mod n m == 0) || (mod m n == 0))
   | (mod n k == 0) && (mod m k == 0) = False && sonCoprimosAux n m (k+1)
   | otherwise = True && sonCoprimosAux n m (k+1)

sonCoprimos n m = sonCoprimosAux n m 2

-- d)
nEsimoPrimoAux :: Integer -> Integer -> Integer

nEsimoPrimoAux 1 p | esPrimo p = p
nEsimoPrimoAux n p | esPrimo p && n>1 = nEsimoPrimoAux (n-1) (p+1)
   | otherwise =nEsimoPrimoAux n (p+1)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 17.
esFiboAux :: Integer -> Integer -> Bool
esFibo :: Integer -> Bool

esFiboAux n k | fib n == k =True
   | fib n < k =esFiboAux (n+1) k
   | otherwise = False

esFibo k = esFiboAux 0 k

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 18.
--mayorDigitoPar :: Integer -> Integer
{--
iEsimoDigitoPar :: Integer -> Integer
esPar :: Integer -> Bool

esPar n = (mod n 2 == 0)

iEsimoDigitoPar n i
   | i == 1 && esPar n && (div n 10 == 0) = n
   | i > 1 && esPar (mod n 10) = iEsimoDigitoPar (div n 10) (i-1)
   | otherwise = iEsimoDigitoPar (div 10) i
   where ipar = iesimoDigito n 1
--}
{--
mayorDigitoPar n
   | div n 10 == 0 && esPar n = 
   | div n 10 = -1
   |  mayorDigitoPar div n 10
--}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 19. 

--esSumaInicialdePrimos :: Integer -> Bool
{--
sumakPrimos :: Integer -> Integer
sumakPrimosDesde :: Integer -> Integer -> Integer

sumakPrimos n | n == 2 =2
   | esPrimo n == True = n + sumakPrimos (n-1) 
   | otherwise = 0 + sumakPrimos (n-1) 

sumakPrimosDesde n m
   | sumakPrimos m == n =sumakPrimos m 
   | sumakPrimos m > n = sumakPrimos (m-1) 
   | sumakPrimos m < n = sumakPrimosDesde n (m+1)

esSumaInicialdePrimos n
   | sumakPrimosDesde n 2 < n 

--}

kesimoPrimoAux :: Integer -> Integer -> Integer
--Veo desde n el k-esimo nro primo
kesimoPrimoAux n k | esPrimo n && k == 1 =n
   | esPrimo n && k>1 = kesimoPrimoAux (n+1) (k-1) 
   | otherwise = kesimoPrimoAux (n+1) k

kesimoPrimo :: Integer -> Integer

kesimoPrimo k = kesimoPrimoAux 2 k

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 20.

sumaDivisores :: Integer -> Integer
tomaValorMax :: Integer -> Integer -> Integer

sumaDivisoresDesd :: Integer -> Integer

sumaDivisoresDesde n i
   | i == 1 = 1
   | divi sumaDivisoresDesde n (i+1)
   | otherwise = iesimoDivisor n i + sumaDivisoresAux n (i+1)

sumaDivisores m 
tomaValor n1 n2
