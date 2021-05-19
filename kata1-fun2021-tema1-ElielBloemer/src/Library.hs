module Library where
import PdePreludat


calculin :: Number -> Number -> Number
calculin  primerNum  segNum
   | primerNum > segNum                = primerNum
   | ((rem primerNum 3) == 0)          = segNum / primerNum
   | otherwise                         = rem segNum 4

type Nombre = String
type Edad = Number
type Persona = (Nombre,Edad)

mayorEdad :: Edad -> Bool
mayorEdad edad = edad >= 18