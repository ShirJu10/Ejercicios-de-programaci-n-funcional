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

--PARTE 1 NOMUS
data Nomus = Nomu {
    ojos:: Number, 
    fuerza :: Number,
    alas :: Number,
    brazos :: Number
} deriving Show

categoria :: Nomus -> String
categoria (Nomu _ fuerza _ _) 
            | fuerza < 1000 = "Nomus Pichis"
            | fuerza < 3000 && fuerza > 1000 = "Nomus Comunes"
            | fuerza < 10000 && fuerza > 3000 = "Nomus Fuertes"
            | otherwise = "Nomus High-end"

vision :: Nomus -> Bool
vision nomu = ojos nomu > 0 

nomu1 :: Nomus 
nomu1 = Nomu {ojos = 0, fuerza = 123, alas = 5, brazos = 8} 
nomu2 :: Nomus 
nomu2 = Nomu {ojos = 5, fuerza = 9748,  alas = 0, brazos = 8} 
nomu3 :: Nomus 
nomu3 = Nomu {ojos = 8, fuerza = 500,  alas = 5, brazos = 0}

--PARTE 2 

listaNomus :: [Nomus]
listaNomus = [nomu1, nomu2, nomu3]

--Entrenar un nomu aumenta su fuerza dos mil veces 
entrenamiento :: [Nomus] -> [Nomus]
entrenamiento lista = map aumentoFuerza lista

aumentoFuerza :: Nomus -> Nomus
aumentoFuerza nomu = nomu {fuerza = fuerza nomu * 2000}

fuerzaMayor :: Nomus -> Bool
fuerzaMayor nomu = fuerza nomu > 2500 

--para que un ejercito pueda ir a la guerra todos los nomus deben tener una fuerza mayor a 2500.
puedeIrAGuerra :: [Nomus] -> Bool
puedeIrAGuerra = all fuerzaMayor . entrenamiento

nomuFuerte :: [Nomus] -> [Nomus]
nomuFuerte = filter esFuerte

esFuerte :: Nomus -> Bool
esFuerte nomu = categoria nomu == "Nomus Fuertes"

--PARTE 3
esAereo :: Nomus -> Bool
esAereo nomu = alas nomu > 0 && brazos nomu <= 0

esTerrestre :: Nomus -> Bool
esTerrestre nomu = alas nomu <= 0 && brazos nomu > 0

esElegido :: Nomus -> Bool
esElegido nomu = alas nomu > 0 && brazos nomu > 0

cantidadAereos :: [Nomus] -> Number
cantidadAereos = length.filter esAereo

poderTotal :: [Nomus] -> Number
poderTotal = sum . map fuerza . filter esTerrestre

hayElegido :: [Nomus] -> Bool
hayElegido = any esElegido

--treino :: Nomus -> Nomus
--treino nomu =  aumentoFuerza . aumentoFuerza

--Torneito de penales ejercico clase 16/05
data Jugador = UnJugador {
    edadJ :: Number,
    nombreJ :: String, 
    piernaHabil :: String,
    potenciaTiro :: Number,
    definicion :: Number
} deriving (Show, Eq)

gonzalo = UnJugador 22 "Gonzalo" "ambidiestro" 73 92

juan = UnJugador  23 "Juan" "ninguna" 68 55

data Arquero = UnArquero {
    reflejos :: Number, 
    altura :: Number, 
    nacionalidad :: String,
    nombre :: String,
    edad :: Number
} deriving (Show, Eq)

agus = UnArquero 70 180 "Argentina" "Agus" 23

patiarPenal :: Jugador -> Arquero -> Bool
patiarPenal jugador arquero = potenciaTiro jugador > reflejos arquero

panenka :: Jugador -> Bool -> Arquero -> Bool
panenka jug miedo arq = miedo && definicion jug > (altura arq) /2 

contarChiste :: String -> Jugador -> Arquero -> Bool
contarChiste categoria jug arq = length categoria * edadJ jug > length (nacionalidad arq) * 15

--Tiro con efecto psicológico: el jugador intenta desconcentrar al arquero mirándolo fijamente antes de patear. Se sabe cuánto tiempo lo miró (en segundos), y también si el arquero es muy susceptible a estas miradas. 
--Si el tiempo de mirada es exactamente 3 segundos y el arquero es susceptible, el penal se mete. En cualquier otro caso, no.: 
--El arquero es susceptible si su nacionalidad es “francés” y el jugador es “ambidiestro” o si la nacionalidad del arquero es “brasilero” y el jugador es “zurdo”. 
efectoPsicologico :: Arquero -> Jugador -> Number -> Bool
efectoPsicologico arq jug tiempoMirar = tiempoMirar == 3 && esSusceptible arq jug

esSusceptible :: Arquero -> Jugador -> Bool
esSusceptible arq jug = (nacionalidad arq == "francés" && piernaHabil jug == "ambidiestro") || (nacionalidad arq == "brasilero" && piernaHabil jug == "zurdo")
