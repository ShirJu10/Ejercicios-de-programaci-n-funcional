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
