-- Funcional guia 2 --
-- Manejo de Funciones de Orden Superior y Listas --

-- Funciones de Orden Superior --
--1.1 
existsAny f (a,b,c) = f a || f b || f c

--1.2
mejor f1 f2 x = max (f1 x) (f2 x)

--1.3
aplicarPar f (a,b) = (f a, f b)

--1.4
parDeFns f1 f2 x = (f1 x, f2 x)


-- Listas --
--2.1
sumarElementos lista = sum lista

--2.2.1
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]
promedioFrecuenciaCardiaca lista = fromInteger(sum lista) / fromInteger(toInteger(length lista)) -- es necesario castear a Integer para hacer la division

--2.2.2
-- mi funcion auxiliar "getIndexFrecuencia", si le pasas algo que no esta en el dominio lanza una excepcion
getIndexFrecuencia minuto | minuto == 0 = 0
                          | minuto == 10 = 1
                          | minuto == 20 = 2
                          | minuto == 30 = 3
                          | minuto == 40 = 4
                          | minuto == 50 = 5
                          | minuto == 60 = 6

frecuenciaCardiacaMinuto = (frecuenciaCardiaca !!).getIndexFrecuencia

--2.2.3
frecuenciasHastaMomento minuto =  take ((+1).getIndexFrecuencia $ minuto) frecuenciaCardiaca  -- hay que tener cuidado, las listas empiezan con indice base 0, entonces incremento en 1 al indice obtenido por la funcion auxiliar

--2.3
esCapicua lista =  (concat lista ==).reverse.concat $ lista

--2.4
duracionLlamadas = ( ("horarioReducido",[20,10,25,15]), ("horarioNormal",[10,5,8,2,9,10]) )

--2.4.1
cuandoHabloMasMinutos | ((sum.snd.fst) duracionLlamadas >).(sum.snd.snd) $ duracionLlamadas = (fst.fst) duracionLlamadas
                      | otherwise = (fst.snd) duracionLlamadas

--2.4.2
cuandoHizoMasLlamadas | ((length.snd.fst) duracionLlamadas >).(length.snd.snd) $ duracionLlamadas = (fst.fst) duracionLlamadas
                      | otherwise = (fst.snd) duracionLlamadas

-- Funciones de Orden Superior con manejo de Listas --

--3.1
esMultiploDeAlguno numero = any (esMultiploDe numero)

--3.2
calcularPromedio lista = fromInteger(sum lista)/(fromInteger.toInteger.length) lista
promedios lista = (map calcularPromedio) lista

--3.3
promediosSinAplazos lista = map(\x -> calcularPromedio.filter (>=4) $ x) lista

--3.4
mejoresNotas lista = map(\x -> maximum x) lista

--3.5
aprobo' lista = (>=4).minimum $ lista

--3.6
aprobaron lista = filter(aprobo') lista

--3.7
divisores n = filter(esMultiploDe n) [1..n]

--3.8
exists funcionBooleana lista = any (funcionBooleana) lista

--3.9 
hayAlgunNegativo lista algo = algo (<0) lista -- ese algo es la funcion "exists" que hay que pasar como parametro a la funcion de orden superior "hayAlgunNegativo" para que se aplique sobre la lista

--3.10
aplicarFunciones funciones valor = map (\f -> f valor) funciones 

-- aplicarFunciones[(*4),even,abs] 8  : da error porque no se respeta la homogeneidad de listas, es decir el tipo de datos Bool que devuelve al aplicarse "even" rompe con los Integer, en este caso.

--3.11
sumaF funciones valor = (sum.aplicarFunciones funciones) valor

--3.12
subirHabilidad n lista = map (\x -> min (x+n) 12) lista

--3.13
flimitada f n =  (max 0).(min 12).f $ n

--3.13.1
cambiarHabilidad f lista = map(flimitada f) lista

--3.13.2
-- *Main> cambiarHabilidad (max 4)  [2,4,5,3,8]
-- [4,4,5,4,8]

--3.14
{-
   *Main> :t takeWhile
   takeWhile :: (a -> Bool) -> [a] -> [a]
   
   Mirando la definicion de la funcion se puede inferir que recibe como parametros una "funcion booleana" y una lista,
   y devuelve una lista de ese mismo tipo, formada por elementos de la primera lista, tomados a medida que se siga cumpliendo 
   dicha funcion booleana. Ej.: 
   
   *Main> takeWhile even [1,3,2]
   []
   
   *Main> takeWhile odd [1,3,2]
   [1,3]
-}

--3.15
primerosPares lista = takeWhile even lista

primerosDivisores n lista = takeWhile (esMultiploDe n) lista

primerosNoDivisores n lista = takeWhile (not.esMultiploDe n) lista

--3.16
huboMesMejorDe ingresos egresos num = any (>num).zipWith (-) ingresos $ egresos --importante: zipWith f lista1 lista2 -> Hace una lista, sus elementos se calculan a partir de la función y los elementos de las listas de entrada que ocurren en la misma posición en ambas listas

{-
	Ej. de uso zipWith:
	
	Input: zipWith (+) [1,2,3] [3,2,1]
	Output: [4,4,4]
-}

--3.17

--3.17.1
crecimientoAnual edad | (edad >=1) && (edad < 10) = 24 - (edad * 2)
                      | (edad >= 10) && (edad <= 15) = 4
                      | (edad == 16) || (edad == 17) = 2
                      | (edad == 18) || (edad == 19) = 1
                      | otherwise = 0

--3.17.2
crecimientoEntreEdades edad1 edad2 = sum.map (crecimientoAnual) $ [edad1..edad2-1] -- edad2-1 es para no contar al ultimo elemento

--3.17.3
alturasEnUnAnio edad lista = map (+ crecimientoAnual edad) lista

--3.17.4
alturaEnEdades altura edad listaEdades = map ((altura +).crecimientoEntreEdades edad) $ listaEdades

--3.18
lluviasEnero = [0,2,5,1,34,2,0,21,0,0,0,5,9,18,4,0]

--3.18.1.
{-
    funcion "dropWhile"
   
    dropWhile :: (a -> Bool) -> [a] -> [a]
	
	Crea una lista a partir de otra, inspecciona la lista original y toma de ella sus elementos desde el momento en que la 
	condición falla por primera vez hasta el final de la lista.
	
	Input: dropWhile (<3) [1,2,3,4,5]
	Output: [3,4,5]
-}

rachasLluvia [] = []
rachasLluvia lista = (takeWhile(0/=).dropWhile(0==)) lista : (filter(/=[]).rachasLluvia.dropWhile(0/=).dropWhile(0==)) lista -- Bastante Jodido! se usa composicion, pattern matching, las funciones del prelude takeWhile y dropWhile y recursividad

mayorRachaDeLluvias = maximum.map(length).rachasLluvia

--3.19

{-
   foldl funcionBinaria acumulador lista
   
   foldr funcionBinaria acumulador lista
   
   IMPORTANTE: si la funcionBinaria que se usa es conmutativa, el resultado es indiferente si se usa foldl o foldr
   pero en caso contrario dara un resultado distinto. foldl es asociativa a izquierda mientras que foldr lo es a derecha.
-}

sumarElementos'  lista = foldl (+) 0 lista

sumarElementos'' lista = foldr (+) 0 lista

--3.20
multiplicarElementos lista = foldl (*) 1 lista

multiplicarElementos' lista = foldr (*) 1 lista

--3.21
dispersion' lista = maximum lista - minimum lista

-- Listas por comprension o listas intensionales --
{-
    Las listas por comprension son como los conjuntos matematicos definidos de la misma manera.
	La sintaxis es:
	[funcion de salida | variable <- conjunto de entrada , predicado (opcional)]
	
	Tambien se puede trabajar con varias listas, es decir con varios conjuntos de entrada, ej.:
    [ x*y | x <- [2,5,10], y <- [8,10,11]]
    [16,20,22,40,50,55,80,100,110] 
	
	Como se puede ver, Haskell hace un producto cartesiano entre ambos dominios pasando luego por la funcion de salida.
	Si no filtramos usando el predicado obtenemos el producto cartesiano, en caso de usarlo:
	
    [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
    [55,80,100,110]
-}

--4.1
multiplos lista n = [x | x <- lista, mod x n == 0]

--4.2
doblesDeLosPares lista = [2*x | x <- lista,even x]

--4.3
menoresA n lista = [x | x <- lista, x < n]

--4.4
diferencia lista1 lista2 = [x | x <- lista1, (not.elem x) lista2]

--4.5
interseccion lista1 lista2 = [x | x <- lista1, y <- lista2, x == y]

--4.6
-- continuara..