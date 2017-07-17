import Text.Show.Functions -- importando esto puedo darle una representacion por consola a las funciones, mostrando "<function>".

-- Por como vamos a definir Alumno, de un parcial necesitamos conocer la materia (para cabulero) y cantidad de preguntas (para hijos del rigor).
-- Modelamos un parcial, usando la sintaxis normal de data.
data Parcial = Parcial String Int deriving (Show) -- Heredo de la clase "Show" para poder imprimir el "data" por consola

-- Al elegir esta sintaxis debemos implementar las funciones de acceso a datos.
materia :: Parcial -> String
materia (Parcial materia _) = materia 

cantidadPreguntas :: Parcial -> Int
cantidadPreguntas (Parcial _ cantidadPreguntas) = cantidadPreguntas

-- Importante: Tener en cuenta la línea de la definición “data”, más arriba.
-- La sintaxis de un Parcial no requiere paréntesis pero, para decir “todo esto es un único parámetro” vamos a necesitar usarlos.

{- 
   El criterio (el cual modelamos como una funcion "Parcial -> Bool") para estudiar ante un parcial, es decir saber si un alumno estudia o no:
     *estudioso: estudia siempre. 
     *hijo del rigor: estudia si el parcial tiene más de n preguntas.
     *cabulero: que estudia si la materia tiene una cantidad impar de letras. 
-}
type CriterioEstudio = Parcial -> Bool

estudioso :: CriterioEstudio
estudioso _ = True

hijoDelRigor :: Int -> CriterioEstudio
hijoDelRigor n (Parcial _ cantidadPreguntas) = cantidadPreguntas > n

cabulero :: CriterioEstudio
cabulero parcial = odd.length.materia $ parcial


type Fecha = (Int,Int,Int)
-- Modelando un alumno, usando "sintaxis de registro" (proporciona los metodos de acceso desde la definicion)
data Alumno = Alumno { 
                       nombre :: String,
                       fechaNacimiento :: Fecha,
                       legajo :: Int, 
                       materias :: [String],
                       criterio ::CriterioEstudio
                     } deriving (Show)

-- modelamos un alumno 
nico = Alumno {
    fechaNacimiento = (10, 3, 1993),
    nombre = "Nico",
    materias = ["sysop", "proyecto"],
    criterio = estudioso,
    legajo = 124124
}