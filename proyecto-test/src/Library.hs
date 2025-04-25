module Library where
import PdePreludat

signo :: Number -> Char
signo numero  | numero > 0 = '+'
              | numero < 0 = '-' --ingresar numero negativo entre () 
              | numero == 0 = '0'



probLluvias :: String -> Number --ingresar string entre "" 
probLluvias clima  | clima == "despejado" = 0 
                   | clima == "nublado" = 25 
                   | clima == "lluvioso" = 100 
                   | otherwise = 50 

maximo :: Number -> Number -> Number -> Number
maximo num1 num2 num3 = max (max num1 num2) num3

bisiesto :: Number -> Bool
bisiesto anio  | mod anio 4 == 0 && mod anio 100 /= 0 = True
               | mod anio 400 == 0 = True
               | otherwise = False

-- OPERACIONES

esMultiploDeTres :: Number -> Bool
esMultiploDeTres numero = mod numero 3 == 0

esMultiploDe :: Number -> Number -> Bool
esMultiploDe numero multiplo = mod multiplo numero == 0

esPar :: Number -> Bool
esPar numero = even numero

--GEOMETRIA

areaRectangulo :: Number -> Number -> Number
areaRectangulo base altura = base * altura

volumenCubo :: Number -> Number
volumenCubo lado = lado ^ 3

superficieCeleste :: Number -> Number
superficieCeleste lado = lado * lado

superficieAzulClaro :: Number -> Number
superficieAzulClaro lado = pi * (lado / 2) ^ 2

superficieAzulOscuro:: Number -> Number
superficieAzulOscuro lado = lado ^ 2 / 2

-- TEMPERATURAS

-- pasa una temperatura en grados Celsius a grados Fahrenheit.
celsiusToFahr :: Number -> Number
celsiusToFahr celsius = celsius * 9 / 5 + 32

fahrToCelsius :: Number -> Number
fahrToCelsius fahrenheit = (fahrenheit - 32) * 5 / 9

-- indica si una temperatura expresada en grados Fahrenheit es fría. Decimos que hace frío si la temperatura es menor a 8 grados Celsius.

haceFrioF :: Number -> Bool
haceFrioF fahrenheit = (fahrToCelsius fahrenheit) < 8

-- MULTIPLICACIONES SUCESIVAS

--Obtener el factorial de un número
factorial :: Number -> Number
factorial num = foldl (*) 1 [1..num]-- recorre de 1 en 1 hasta el num multiplicando

-- Elevar un número a una potencia (solo potencias enteras no negativas)
potencia :: Number -> Number -> Number
potencia base exponente 
                    | exponente == 0 = 1
                    | exponente >0 = base * potencia base (exponente - 1)

--Averiguar si un número es cuadrado perfecto
cuadradoPerfecto :: Number -> Bool 
cuadradoPerfecto num = (sqrt num) ^ 2 == num -- no se porque no me da con 9 

--CLASE 25/4

data Figura = Rectangulo {base :: Number , altura :: Number}
            | Triangulo {lado :: Number}
            | Circulo {radio :: Number}
            deriving Show

-- funcion definida por partes (una parte para el rectangulo y otra para el perimetro)
perimetro :: Figura -> Number
--con patern matching estoy desarmando el constructor "Rectangulo"
--Para ingresar valores definir r1 = Rectangulo num num
perimetro (Rectangulo base altura) = (base + altura) * 2
perimetro (Triangulo lado) = 3 * lado
perimetro (Circulo radio) = 2 * pi * radio

--hacer funcion superficie y "esRegular"
--superficie :: Figura -> Number
--superficie (rectangulo)

--esRegular es una funcion que confice el filter
esRegular (Rectangulo base altura) = base == altura
esRegular (Triangulo lado) = True
esRegular (Circulo radio) = True

l1 = [Rectangulo 2 2, Circulo 4, Triangulo 5, Rectangulo 3 6]

--data Palabra = Letras {tamanio :: Number}
            --deriving Show

esCorta :: String -> Bool
esCorta palabra = length palabra < 5

s1 = ["hola", "shirly", "juju", "jujuuuuuuuu"] --para ejecutar: filter esCorta s1

--PARTE 1 Los Nomus son humanos mutados que poseen distintas capacidades físicas, como tener alas, múltiples brazos, cantidad de ojos y el color de piel, además de tener una cantidad de vida y fuerza.

--Luego se nos pide averiguar si puede ver, es decir, si tiene ojos y su categoría.

--Categorías de un Nomu:

-- Los nomus pichis son aquellos que su fuerza es inferior a 100

-- Los nomus comunes son aquellos que su fuerza es inferior a 3000 y mayor a 1000

-- Los nomus fuertes son aquellos que su fuerza es inferior a 10000 y mayor que 3000

-- Por último, los nomus high-end poseen una fuerza mayor a 10000


--PARTE 2 Dada una lista de nomus se quiere entrenarlos y saber si el ejercito puede ir a la guerra despues del entrenamiento. 
--Entrenar un nomu aumenta su fuerza dos mil veces y para que un ejercito pueda ir a la guerra todos los nomus deben tener una fuerza mayor a 2500. Ademas queremos saber cuales nomus de nuestro ejercito, para eso deben cumplir la condicion de la parte 1.

data Nomus = Nomu {ojos:: Number, fuerza :: Number}
            deriving Show

categoria :: Nomus -> String
categoria (Nomu _ fuerza) 
            | fuerza < 1000 = "Nomus Pichis"
            | fuerza < 3000 && fuerza > 1000 = "Nomus Comunes"
            | fuerza < 10000 && fuerza > 3000 = "Nomus Fuertes"
            | otherwise = "Nomus High-end"

vision :: Nomus -> Bool
vision nomu = ojos nomu > 0 

nomu1 :: Nomus 
nomu1 = Nomu {ojos = 0, fuerza = 123} 
nomu2 :: Nomus 
nomu2 = Nomu {ojos = 5, fuerza = 9748} 

--parte 2
nomus = [nomu1, nomu2]
ejercito :: [Nomu]
ejercito nomus = 
