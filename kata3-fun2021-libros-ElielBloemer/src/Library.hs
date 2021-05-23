module Library where
import PdePreludat

data Libro = Libro {
 titulo :: String,
 paginas :: Number,
 temas :: [String]
} deriving (Show)

misLibros = [
    Libro "libro de PdeP" 100 ["fold","funcion parcial"],
    Libro "Sintaxis" 200 ["BNF","ejrjek"],
    Libro "Mat. Superior" 325 ["Complejos","Serie de Fourier","sistemas estables"],
    Libro "Mat. Discreta" 500 ["conjuntos"]
 ]

masPaginasQue :: Number -> Libro -> Bool
masPaginasQue minimo libro  =  (>minimo).paginas $ libro

librosOrdenados :: [Libro] -> Bool
librosOrdenados [] = error "la lista de libros, no contiene ningun libro"
librosOrdenados [libro] = True
librosOrdenados (primerLibro:segundoLibro:libros) = ((paginas primerLibro) <= (paginas segundoLibro)) && librosOrdenados(segundoLibro:libros)
   
librosConMasPaginasQue:: Number -> [Libro] -> [String]
librosConMasPaginasQue minimo libros = (map(titulo).(filter (masPaginasQue minimo)))libros


