-- Funciones de uso general
duplicar :: Integer -> Integer
duplicar x = 2 * x

triplicar :: Integer -> Integer
triplicar x = 3 * x

-- Consigna 1
funcion1 :: Char -> Bool
funcion1 car = elem car vocales
            where vocales = ['a','e','i','o','u','A','E','I','O','U']

funcion2 :: Char -> Bool
funcion2 car = elem car digitos
            where digitos = ['0'..'9']

-- Consigna 2
funcion3 :: Integer -> Char -> Integer
funcion3 x car | funcion1 car = duplicar x
               | funcion2 car = triplicar x
               | otherwise = x

-- Consigna 3
funcion4 :: [Integer] -> Char -> [Integer]
funcion4 [] _ = []
funcion4 (x:xs) car | funcion1 car = duplicar x : funcion4 xs car
                    | funcion2 car = triplicar x : funcion4 xs car
                    | otherwise = x : funcion4 xs car

-- Consigna 4
funcion5 :: [Integer] -> Char -> Int -> [Integer]
funcion5 [] _ _ = []
funcion5 _ _ 0 = []
funcion5 (x:xs) car cant | funcion1 car = duplicar x : funcion5 xs car (cant - 1)
                         | funcion2 car = triplicar x : funcion5 xs car (cant - 1)
                         | otherwise = x : funcion5 xs car (cant - 1)

-- Consigna 5
funcion6 :: Integer -> (Integer, Integer)
funcion6 n = (n, triplicar n)

-- Consigna 6
funcion7 :: Integer -> Integer -> [(Integer, Integer)]
funcion7 _ cant | cant <= 0 = []
funcion7 n cant = (n, triplicar n) : funcion7 (triplicar n) (cant - 1)
