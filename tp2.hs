duplicar :: Integer -> Integer
duplicar x = 2 * x

triplicar :: Integer -> Integer
triplicar x = 3 * x



es_vocal :: Char -> Bool
es_vocal car = elem car vocales
            where vocales = ['a','e','i','o','u','A','E','I','O','U']

es_digito :: Char -> Bool
es_digito car = elem car digitos
            where digitos = ['0'..'9']

funcion2 :: Integer -> Char -> Integer
funcion2 x car | es_vocal car = duplicar x
               | es_digito car = triplicar x
               | otherwise = x


funcion3 :: [Integer] -> Char -> [Integer]
funcion3 [] _ = []
funcion3 (x:xs) car | es_vocal car = duplicar x : funcion3 xs car
                    | es_digito car = triplicar x : funcion3 xs car
                    | otherwise = x : funcion3 xs car
                    