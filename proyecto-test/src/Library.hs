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