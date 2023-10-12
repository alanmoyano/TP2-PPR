es_vocal car = elem car vocales
            where vocales = ['a','e','i','o','u','A','E','I','O','U']

es_digito car = elem car digitos
            where digitos = ['0'..'9']

funcion2 :: Integer -> Char -> Integer
funcion2 x car | es_vocal car = 2 * x
               | es_digito car = 3 * x
               | otherwise = x
