module Library where
import PdePreludat

type Modelo = String
type Kilometros = Number
 
type Auto = (Modelo, Kilometros)
 
modelo :: Auto -> Modelo
modelo = fst 
 
kilometraje :: Auto -> Kilometros
kilometraje = snd 

-- Punto 1
autoVeloz :: Auto -> Bool
autoVeloz auto = (odd . length . modelo) auto      

-- Punto 2
cambioDeAceite :: Auto -> Bool
cambioDeAceite auto = ((>10000) . kilometraje) auto

