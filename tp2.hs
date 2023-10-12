-- Funciones de uso general
duplicar :: Integer -> Integer
duplicar x = 2 * x

triplicar :: Integer -> Integer
triplicar x = 3 * x

-- Consigna 1
es_vocal :: Char -> Bool
es_vocal car = elem car vocales
            where vocales = ['a','e','i','o','u','A','E','I','O','U']

es_digito :: Char -> Bool
es_digito car = elem car digitos
            where digitos = ['0'..'9']

-- Consigna 2
funcion2 :: Integer -> Char -> Integer
funcion2 x car | es_vocal car = duplicar x
               | es_digito car = triplicar x
               | otherwise = x

-- Consigna 3
funcion3 :: [Integer] -> Char -> [Integer]
funcion3 [] _ = []
funcion3 (x:xs) car | es_vocal car = duplicar x : funcion3 xs car
                    | es_digito car = triplicar x : funcion3 xs car
                    | otherwise = x : funcion3 xs car

-- Consigna 4
funcion4 :: [Integer] -> Char -> Int -> [Integer]
funcion4 [] _ _ = []
funcion4 _ _ 0 = []
funcion4 (x:xs) car cant | es_vocal car = duplicar x : funcion4 xs car (cant - 1)
                         | es_digito car = triplicar x : funcion4 xs car (cant - 1)
                         | otherwise = x : funcion4 xs car (cant - 1)

-- Consigna 5
funcion5 :: Integer -> (Integer, Integer)
funcion5 n = (n, triplicar n)

-- Consigna 6
funcion6 :: Integer -> Integer -> [(Integer, Integer)]
funcion6 _ cant | cant <= 0 = []
funcion6 n cant = (n, triplicar n) : funcion6 (triplicar n) (cant - 1)
