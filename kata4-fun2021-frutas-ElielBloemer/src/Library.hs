module Library where
import PdePreludat

data Fruta = Fruta {
 calorias :: Number,
 nombre :: String,
 propiedades :: [String]
} deriving (Eq, Show)
 
data Dieta = Dieta {
 frutas :: [Fruta],
 maximoCalorias :: Number
} deriving (Eq, Show)
 
------------------------------------- PUNTO 1 ------------------------------------

dietaImposible :: Dieta -> Bool
dietaImposible dieta = sumatoriaCalorias dieta > maximoCalorias dieta
 
sumatoriaCalorias :: Dieta -> Number
sumatoriaCalorias dieta =  foldr ((+) . calorias) 0 (frutas dieta)
 
--------------- PUNTO 2 ------------------------------------

algunaTienePropiedad :: String -> Dieta -> Bool
algunaTienePropiedad propiedad dieta =  any (elem propiedad . propiedades) (frutas dieta)

