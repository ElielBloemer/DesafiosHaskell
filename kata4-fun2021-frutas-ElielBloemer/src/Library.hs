module Library where
import PdePreludat
----------------------------------------- KATA 4 ----------------------------------------------
data Fruta = Fruta {
 calorias :: Number,
 nombre :: String,
 propiedades :: [String]
} deriving (Eq, Show)
 
data Dieta = Dieta {
 frutas :: [Fruta],
 maximoCalorias :: Number
} deriving (Eq, Show)


misFrutas = [
    Fruta 12 "pelón" ["antioxidante","regula Rinhones"],
    Fruta 10 "cereza" ["acidez","vitamina D"],
    Fruta 22 "naranja" ["vitamina C","vitamina A"]
 ] 

miDieta =  Dieta misFrutas 50
 
------------------------------------- PUNTO 1 ------------------------------------
--Saber si una dieta es imposible de cumplir, esto es si la sumatoria
-- de calorías de las frutas supera el máximo de calorías de la dieta.
 --Por ejemplo, si una dieta contiene, manzana (10 calorías) y pera (12 calorías),
 --una dieta de 22 calorías o más calorias es posible de cumplir, una dieta de menos de 22 es imposible de cumplir.

-- punto 1
dietaImposible :: Dieta -> Bool
dietaImposible dieta = sumatoriaCalorias dieta > maximoCalorias dieta
 
sumatoriaCalorias :: Dieta -> Number
sumatoriaCalorias dieta =  foldr ((+) . calorias) 0 (frutas dieta)
 
--------------- PUNTO 2 ------------------------------------
--Dada una dieta, queremos saber si alguna tiene una propiedad,
--por ejemplo, "antioxidante". Si una dieta contiene pelón ("vitamina C")
--y cereza ("acidez"), la función debería satisfacerse si buscamos "vitamina C" o "acidez",
--respetando las mayúsculas y minúsculas.

             -- punto 2
algunaTienePropiedad :: String -> Dieta -> Bool
algunaTienePropiedad propiedad dieta =  any (elem propiedad . propiedades) (frutas dieta)

{-Solo puede resolverse con funciones de orden superior, no puede utilizar recursividad en ningún punto.
Debe utilizar composición únicamente mediante la función (.).
Recomendamos revisar los nombres de variables (ninguna de una sola letra, deben expresar claramente el dominio que están resoviendo)-}