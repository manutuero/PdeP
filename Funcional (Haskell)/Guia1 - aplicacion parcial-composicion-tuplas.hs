import Text.Show.Functions -- con este import permitimos "imprimir" una funcion

-- Vamos a ver que onda con Haskell
aproboAlumno :: Int -> Bool
aproboAlumno nota = nota >= 6

-- Funcional guia 1 --
-- Primeros ejercicios de funcional --

-- 1.1
esMultiploDeTres :: Int -> Bool
esMultiploDeTres x = (mod x 3) == 0

--1.2
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = (mod x y) == 0

--1.3
-- Si defino el dominio de entrada como Int estoy restingiendo los valores en punto flotante.
cubo x = x^3

--1.4
area base altura = base*altura

--1.5
esBisiesto anio = (esMultiploDe anio 400) || ((esMultiploDe anio 4) && not (esMultiploDe anio 100))

--1.6
celsiusToFahr c = c*1.8000+32.00 -- googlear 

--1.7
fahrToCelsius f = (f-32)/1.8000 -- googlear 

--1.8
haceFrioF f = fahrToCelsius f < 8

--1.9
-- gcd (greatest common divisor = maximo comun divisor)
mcm a b = (a*b) `div` (gcd a b)

--1.10.a
dispersion m1 m2 m3 = max (max m1 m2) m3 - min (min m1 m2) m3

--1.10.b
-- medidas "mx" expresadas en cm
diasParejos m1 m2 m3 = dispersion m1 m2 m3 < 30

diasLocos m1 m2 m3 = dispersion m1 m2 m3 > 100

diasNormales m1 m2 m3 = not (diasParejos m1 m2 m3) && not (diasLocos m1 m2 m3)

--1.11
-- Parece que al interprete de Haskell no le gustan los caracteres tabs (\t), para las guardas usar espacios en blanco
-- altura expresada en cm, devuelve peso en Kg
pesoPino altura | altura <= 300 = 3*altura
                | otherwise = 300 + 2*altura

esPesoUtil peso = peso >= 400 && peso <= 1000

-- me pide que lo haga por composicion, entonces las 3 maneras posibles son:
sirvePino altura = esPesoUtil.pesoPino $ altura -- el operador aplicacion ($) aplica una funcion con "precedencia a derecha", es decir, la expresion a derecha se toma como parametro para la funcion de la izquierda.
sirvePino' altura = esPesoUtil (pesoPino altura)
sirvePino'' = esPesoUtil . pesoPino -- notacion "point-free" (simplificando terminos)


--1.12
sumarImpares n i j | (n == 0) || (n < i) = 0
                   | (n == i) = n
                   | n > i =  sumarImpares n (i+j) (j+2)
                   
esCuadradoPerfecto n = (sumarImpares n 0 1) == n

-- Aplicacion parcial --
--2.1
siguiente = (+1)

--2.2
mitad = (/2)

--2.3
inversa = (1/)

--2.4
triple = (3*)

--2.5
esNumeroPositivo = (>0)

-- Composicion --
--3.1
esMultiploDe' y = (== 0).(mod y) -- point-free para el parametro "x", que viene a ser el año

--3.2
esBisiesto' anio = (((esMultiploDe anio 400)||).(esMultiploDe anio 4 &&).not.(esMultiploDe anio)) 100

--3.3
inversaRaizCuadrada = inversa.sqrt

--3.4
incrementMCuadradoN n m = (+m).(^2) $ n -- con el operador $ me ahorro de poner los parentesis en la nueva funcion compuesta

--3.5
esResultadoPar n m = (even.(^m)) n

-- Tuplas --
--4.1
fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c

--4.2
aplicar (f1,f2) x = (f1 x,f2 x)

--4.3
cuentaBizarra (x,y) | x > y = x + y
                    | abs (x - y) > 10 = y - x
                    | y > x && abs (x - y) < 10 = x * y

--4.4
-- habria que actualizar al guia, ahora se aprueba con 6 :p
esNotaBochazo = (<4)

aprobo (nota1,nota2) = (not(esNotaBochazo nota2) &&).not.(esNotaBochazo) $ nota1 -- notar que esNotaBochazo nota2 no esta siendo aplicada parcialmente, pero la funcion && si. Donde "not(esNotaBochazo nota2)" es el miembro izquierdo del operador logico &&.

promociono (nota1,nota2) = nota1+nota2 >= 14 && (nota1 >= 6) && (nota2 >= 6)

aproboPrimerParcial (nota1,_) = (not.esNotaBochazo) nota1

--4.5 n: nota, r: recuperatorio
notasFinales ((n1,n2),(r1,r2)) = (max n1 r1, max n2 r2)

recursa = not.aprobo.notasFinales

recuperoPrimerParcial ((n1,_),(r1,_)) = (not(esNotaBochazo r1) &&).(esNotaBochazo) $ n1

recuperoDeGusto ((n1,n2),(r1,r2)) =  promociono (n1,n2) && (r1 > -1) || (r2 > -1) -- el recupero de gusto no contempla que haya aprobado ese recu (igual que en nuestro nuevo reglamento jeje :p)

--4.6
esMayorDeEdad = (>=21).snd

--4.7
calcular (x,y) | ((odd y &&).even) x = (2*x,y+1) -- el orden es importante, es secuencial
               | even x = (2*x,y)
               | odd y = (x,y+1)
               | otherwise = (x,y)