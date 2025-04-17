module Library where
import PdePreludat

signo :: Number -> Char
signo numero  | numero > 0 = '+'
              | numero < 0 = '-'--ingresar numero negativo entre () 
              | numero == 0 = '0'



probLluvias :: String -> Number --ingresar string entre "" 
probLluvias clima  | clima == "despejado" = 0 
                   | clima == "nublado" = 25 
                   | clima == "lluvioso" = 100 
                   | otherwise = 50 