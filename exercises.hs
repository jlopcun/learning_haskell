  
-- FUNCIÓN QUE SIRVE PARA CALCULAR LAS SOLUCIONES DE UNA ECUACIÓN DE SEGUNDO GRADO
scnGradeEq :: Float -> Float -> Float -> [Float]
scnGradeEq a b c = [x1,x2] 
        where
            x1 = (-b + sqrtOp) / ( 2 * a)
            x2 = (-b - sqrtOp) / ( 2 * a)
            sqrtOp = sqrt( b ^ 2 - 4 * a * c)

--FUNCIÓN QUE RECIBE UN NÚMERO COMO PARÁMETRO Y DEVUELVE EL ELEMENTO EN DICHA POSICIÓN DENTRO DE LA SUCESIÓN DE FIBONACCI
fib :: (Eq a, Num a, Num p) => a -> p
fib 0 = 0
fib 1 = 1

fib n = fib(n-1) + fib (n-2)

--FUNCIÓN QUE CALCULA EL FACTORIAL DE UN NÚMERO
fac :: (Eq a, Num a, Ord a) => a -> a
fac num 
    |num==0 =1
    |otherwise = fac (num - 1) * num

--Escribe una función en la que , recibida una lista de números , devuelva el promedio de sus valores



getPrometer :: (Fractional a, Foldable t) => t a -> a
getPrometer ls = sum ls / fromIntegral (length ls)
