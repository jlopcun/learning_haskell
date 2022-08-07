import Data.Char (ord, chr,isSpace)
import Data.Time.Calendar.MonthDay (monthLength)
import Data.List


{-Este es mi primer archivo en haskell :)-}
-- este es mi primer comentario de una sola línea en haskell

y :: Integer
y =  x*2
x :: Integer
x = 5

-- Creamos una función que recibe un parámetro (r) y devuelve el área de un círculo con esa base
areaOfACircle :: Floating a => a -> a
areaOfACircle r = pi * r ^ 2

areaOf2BaseCircle :: Double
areaOf2BaseCircle = areaOfACircle 2

areaOfFiveTimesThree :: Double
areaOfFiveTimesThree = areaOfACircle (5*3)

areaOfFiveMultThree :: Double
areaOfFiveMultThree = areaOfACircle 5 * 3


double :: Num a => a -> a
double x = x * 2
-- ejemplo de reutilización de una función dentro de otra función
quadruple :: Num a => a -> a
quadruple x = double (double x)
{- quadruple x = double double x daría error, porque lo que haskell interpreta es que estas llamando a la función dobule con dos parámetros, double y x , por eso es necesario agruparlos para que recoja el valor de dobule x primero y despues ese valor sea usado como parámetro para double-}
--ejemplo de reutilización de funciones
areaSquare :: Num a => a -> a
areaSquare s = rectArea s s

-- ejemplo de función que recibe más de un parámetro
rectArea :: Num a => a -> a -> a
rectArea l w = l * w

--EJERCICIO haz una función que calcule el volumen de una caja


boxVolumn :: Num a => a -> a
boxVolumn l = areaSquare l * l


--EJERCICIO :escribir una función que retorne el volumen de un cilíndro sabiendo el radio y la altura

areaCilinder :: Floating a => a -> a -> a
areaCilinder radix height = areaOfACircle radix * height


-- Uso de variables locales dentro de una función
heron :: Floating a => a -> a -> a -> a
heron a b c = sqrt(s*(s - a)*(s - b)*(s - c))
                where
                    s= a+b+c



-- IGUALDAD Y OTRAS COMPARACIONES
multBy :: Num a => a -> a -> a
multBy mult num = num * mult

db :: Integer -> Integer
db  = multBy 2


--db 5 > 5 -->True
--db 5 <= 1 -->False


-- INFIX OPERATORS
sumar :: Num a => a -> a -> a
sumar a b = (+) a b

igual :: Eq a => a -> a -> Bool
igual a b = (==) a b

-- BOOLEAN OPERATIONS
andOperator :: Bool
andOperator = (5+5)>=10 && 2>1 --los dos valores son verdaderos, por lo tanto, devuelve true


orOperator :: Bool
orOperator = (5+5)==10 || 2<1 -- al menos uno de los dos valores es verdadero, por lo tanto, devuelve true

notOperator :: Bool
notOperator = not ((==) 5 5) -- el valor es True, por lo tanto, devuelve False


-- GUARDS
myFirstGuard :: (Num p, Ord p) => p -> p
myFirstGuard x
    |x <= 0       = 1
    |otherwise    = x + 1



-- GUARDS CON WHERE
numOfRealSolutions :: (Ord a, Num p, Num a) => a -> a -> a -> p
numOfRealSolutions a b c
    |disc > 0   = 2
    |disc == 0  =1
    |otherwise  =0
    where
        disc = b ^ 2 -4*a*c


--INLINE GUARD
someGuard :: (Ord a, Num a) => a -> [Char]
someGuard a  |a>1 = "greater than one" |a<1 = "less than one" |otherwise = "equal"



--HASKELL/TYPES BASICS

cadenaString :: String
cadenaString = "hola que tal"

cadenaChar :: [Char]
cadenaChar = "hola que tal"

character :: Char
character = 'C'

--Functional types

getReverse :: Num a => a -> a
getReverse x = -x
--getReverse es una función que recibe parámetros tipo Num y devuelve valores tipo Bool


--chr and ord

encodeStr :: Char -> Int
encodeStr str = ord str

decodeStr :: Int -> Char
decodeStr num = chr num

--Types in functions with more than one argument && Exercises
isOnlyOneTrue :: Bool -> Bool -> Bool
isOnlyOneTrue boo1 boo2 = (boo1 || boo2) && (boo1 && boo2)

negateJlop :: Int -> Int
negateJlop num = -num

orJlop :: Bool -> Bool -> Bool
orJlop boo1 boo2 = boo1 || boo2

monthLengthJlop :: Bool -> Int -> Int
monthLengthJlop isLeap month     
        |month == 2 && not isLeap =28
        |even month && month < 6 || odd month && month > 6 || month == 2 && isLeap = 30
        |otherwise = 31
        
funcExpBool :: Bool -> Bool ->Bool
funcExpBool x y = not x && y



funcExpInt :: Num a => a -> a
funcExpInt x = (2 * x - 1) ^ 2

--LISTAS Y TUPLAS
numList :: [Int]
numList = [1,2,3,4,5]

strList :: [String]
strList = ["esto","es","una","lista","de","strings"]

--Consing a list

numZTF , atLeft , atRight :: [Int]

numZTF = [0..5]

--adding elements to the left(start) of the list using consing

atLeft = 10:9:8:7:6:numZTF

--adding elements to the right(end) of the list using consing

atRight = reverse $ 10:9:8:7:6:reverse numZTF 

--EXERCISES

--crea una función que reciba una lista y le añada un 8 al inicio
cons8 :: [Int] -> [Int]
cons8 ls = 8:ls

--adapta la función anterior para que añada el 8 al final de la lista en vez de al inicio
cons8End :: [Int] -> [Int]
cons8End ls = ls ++ [8]

--crea una función que reciba una lista y un elemento, y le añada ese elemento a la lista
doCons :: [a] -> a ->[a]
doCons ls el = el:ls

--TUPLAS
myTuple :: (Bool,Int,Char,String,[a],[Int])
myTuple = (True,1,'C',"String",[],[1,2,3,4])

--Escribe una 3-tupla , es decir, un triple cuyo primer elemento sea 4, el segundo sea "hello" y el tercero True
triple :: (Int,String,Bool)
triple = (4,"hello",True)

--EXTRAER VALORES DE LISTAS Y TUPLAS (ejercicios)

--Extrae de la siguiente lista el número 4
exTup :: ((String, Integer), Bool)
exTup = (("Hello", 4), True)

get4FromExTup :: Integer
get4FromExTup = snd $ fst exTup

--Elabora una función que dada una lista como parámetro, devuelva una tupla con el head y el tail como primer y segundo elemento respectivamente
getHeadTail :: [a] -> (a,[a])
getHeadTail ls = (head ls,tail ls)
--Elabora una función que recibiendo una lista como parámetro , devuelva el quinto elemento de la misma

getFifth :: [a] -> a
getFifth ls = head $ tail $ tail $ tail $ tail ls


--TYPE BASICS II

myInt :: Int
myInt = 4

myFractional :: Fractional a => a
myFractional = 3.12


--If then else clauses


getSign :: (Ord a, Num a, Num p) => a -> p
getSign x = 
    if x <0
        then -1
    else if x==0
        then 0
    else 1

getSignGuard :: (Ord a, Num a, Num p) => a -> p
getSignGuard x
    |x<0       = -1
    |x==0      = 0
    |otherwise = 1   
    
getAbs :: (Ord p, Num p) => p -> p
getAbs x=
    if x<0
        then -x
    else x

getAbsGuard :: (Ord p, Num p) => p -> p
getAbsGuard x
    |x<0    = -x
    |otherwise =x


-- PATTERN MATCHING
{-    10 points for the winner;
    6 for second-placed;
    4 for third-placed;
    3 for fourth-placed;
    2 for fifth-placed;
    1 for sixth-placed;
    no points for other racers.
-}
-- USO DE ESTRUCTURA IF THEN ELSE PARA RESOLVER EL PROBLEMA
getPoints :: Int -> Int
getPoints x = 
    if x==1
        then 10
    else if x==2
        then 6
    else if x==3
        then 4
    else if x==4
        then 3
    else if x==5
        then 2
    else if x==6
        then 1
    else 0

-- USO DEL WISE-DEFINITION PARA RESOLVER EL PROBLEMA
getWisePoints :: Int -> Int
getWisePoints 1 = 10
getWisePoints 2 = 6
getWisePoints 3 = 4
getWisePoints 4 = 3
getWisePoints 5 = 2
getWisePoints 6 = 1
getWisePoints _ = 0

--USO DE UN GUARD PARA RESOLVER EL PROBLEMA
getGuardPoints :: Int -> Int
getGuardPoints x
    |x==1   =10
    |x<3   = 10 - x * 2
    |x<=6   = 7 - x
    |otherwise = 0

-- USO DEL WISE-DEFINITION Y DE UN GUARD PARA RESOLVER EL PROBLEMA
getPointsMixed :: Int -> Int
getPointsMixed 1 = 10
getPointsMixed 2 = 6

getPointsMixed x
    |x <= 6   = 7-x
    |otherwise =0

--ejemplo de como se puede definir una función que haga algo para cualquier valor que reciba como parámetro , pero que pueda tener alguna excepción
getANum :: Int -> Int
getANum 15 = 368
--en este caso la excepción es 15 , que en lugar de devolver su doble como con cualquier otro número, devuelve 368
getANum x = x * 2

--TUPPLE AND LIST PATTERNS

fst' :: (a, b) -> a
fst' (x,d) = x

snd' :: (a, b) -> b
snd' (x,d) = d

head' :: [a] -> a
head' (x:xs) = x

tail' :: [a] -> [a]
tail' (x:xs) = xs

getFifthOfList :: [a] -> a
getFifthOfList (a:b:c:d:e:xs) = e

fd :: [a] -> a
fd (a:b:c:d:e:xs) = b


-- LET BIDINGS
getSquareRoot :: Float -> Float -> Float -> (Float,Float)
getSquareRoot a b c = 
    let sdisc = sqrt(b ^ 2 - 4 * a * b)
        twice_a = 2 * a
    in (
        (-b + sdisc) / twice_a,
        (-b - sdisc) / twice_a
    )

--BUILDING VOCABULARY
-- FUNCTION COMPOSITION

square :: Num a => a -> a
square x = x ^ 2

plusTwo :: Num a => a -> a
plusTwo x = x + 2

squarePlusTwo :: Int -> Int
squarePlusTwo = square . plusTwo --esto es igual que (square . plusTwo) y que square (plusTwo x)
-- en este caso no hace falta usar paréntesis , ya que no estamos incluyendo ningún argumento, solo creando una nueva función

--One exhibit
reverseString :: String -> [Char]
reverseString = unwords . reverse . words 

-- SIMPLE INPUT AND OUTPUT
-- main :: IO ()
-- main = do
--     putStrLn "what is your name??"
--     name <- getLine --asigna a una variable name el valor recibido de getLine
--     putStrLn ("hola " ++ name ++ " como estás")

main :: IO ()
main = do
    putStrLn "what is the base??"
    base <- getLine
    putStrLn "what is the height??"
    height <- getLine
    putStrLn ("the area of that triangle is: " ++ show ((read base * read height)/2))
