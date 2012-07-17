-- PEDRO ALVAREZ PIEDEHIERRO

--lights n encendidas -> lmarcadas
--Esta función devuelve una lista con todas las posibles combinaciones de casillas que hay que pulsar para apagar la combinación de luces encendidas pasadas por el parámetro “encendidas”. Devuelve dentro de esta lista todas las posibilidades, sin permutaciones de éstas.
--
--Argumentos:
	--N: Entero que indica el lado de el tablero
	--Encendidas: Lista de tuplas (F, C) que indica las casillas que están encendidas.
--Devuelve:
	--Lista de tuplas (F, C) que indica que casillas hay que pulsar para apagar la combinación LEncendidas.

lights::Int-> [(Int,Int)] -> [[(Int,Int)]]
lights n lencendidas = [i | i <- generarPosibles n , probarCombinacion n lencendidas i]




--insertar lista dato -> lista
--Esta función añade un elemento a una lista y lo devuelve.

--Argumentos:
	--lista: Lista en la que insertar el elemento.
	--dato: Elemento a insertar en la lista.
--Devuelve:
	--Lista en la que se ha insertado el dato.

insertar::[a] -> a -> [a]
insertar xs x =(x:xs)




--borrar lista dato -> lista
--Esta función borra un elemento de una lista y devuelve la nueva lista.

--Argumentos:
	--lista: Lista en la que borrar el elemento.
	--dato: Elemento a borrar de la lista.
--Devuelve:
	--Lista en la que se ha borrado el dato.

borrar::[(Int,Int)] -> (Int,Int) -> [(Int,Int)]
borrar [] _	= []
borrar (x:xs) y | (iguales x y) = xs
				| otherwise = (x:(borrar xs y))



				
--buscar lista dato -> resultado
--Busca el dato “dato” en la lista “lista”. Devuelve Verdadero si está en la lista, y Falso si no lo está.

--Argumentos:
	--dato: Elemento a buscar en la lista.
	--lista: Lista en la que buscar el dato.
--Devuelve:
	--Valor Booleano de resultado.

buscar::[(Int,Int)] -> (Int,Int) -> Bool
buscar [] _	= False
buscar (x:xs) y | (iguales x y) = True
				| otherwise = (buscar xs y)


				
				
--iguales casillaA casillaB -> resultado
--Esta función nos indica si la tupla (F,C) pasada por el parámetro “casillaA” es igual a la tupla (F,C) pasada por el parametro “casillaB”. Devolverá Verdadero si son iguales, y Falso en caso contrario.

--Argumentos:
	--casillaA: Tupla (F,C) que se quiere comparar.
	--casillaB: Tupla (F,C) que se quiere comparar.
--Devuelve:
	--Valor Booleano de resultado.

iguales::(Int,Int) -> (Int,Int) -> Bool
iguales (a,b) (c,d) | ((a == c) && (b == d)) = True
					|otherwise =False


					
					
--casillaValida n casilla -> resultado
--Esta función nos indica si la tupla (F,C) pasada por el parámetro “casilla” esta dentro de un tablero de juego de lado “n”. Devolverá Verdadero si pertenece al tablero, y Falso en caso contrario.

--Argumentos:
	--N: Entero que indica el lado del tablero.
	--casilla: Tupla (F,C) que se quiere comprobar si es válida.
--Devuelve:
	--Valor Booleano de resultado.

casillaValida::Int-> (Int,Int) -> Bool
casillaValida n (a,b)	|((a>0)&&(a<=n)&&(b>0)&&(b<=n)) =True
						| otherwise =False




--modificarCasilla n lencendidas casilla -> lencendidas
--Esta función nos modifica el estado de la casilla (F,C) pasada por el parámetro “casilla”, es decir, si estaba la casilla encendida en el tablero la apaga, y si estaba apagada la enciende. Para representar esto utilizamos una lista en la cual guardamos las casillas encendidas “lencendidas”. Por tanto si esta en la lista la borramos, y si no esta la insertamos, devolviendo como resultado la lista “lencendidas” tras la modificación. 

--Argumentos:
	--N: Entero que indica el lado del tablero.
	--lencendidas: Lista de tuplas (F,C) que indica las casillas encendidas.
	--casilla: Tupla (F,C) que se quiere modificar el estado.
--Devuelve:
	--Lista de tuplas (F,C) que representa las casillas encendidas tras la modificación.

modificarCasilla:: Int -> [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
modificarCasilla n lencendidas x	| ((casillaValida n x) && ( buscar lencendidas x)) = borrar lencendidas x
									| (casillaValida n x)  = insertar lencendidas x
									| otherwise	= lencendidas




--pulsarCasilla n lencendidas casilla -> lencendidas
--Esta función nos modifica el estado de la casilla (F,C) pasada por el parámetro “casilla” y también el estado de las casillas adyacentes según el patrón lights out. Devuelve  como resultado la lista “lencendidas” tras la modificación. 

--Argumentos:
	--N: Entero que indica el lado del tablero.
	--lencendidas: Lista de tuplas (F,C) que indica las casillas encendidas.
	--casilla: Tupla (F,C) que se quiere pulsar.
--Devuelve:
	--Lista de tuplas (F,C) que representa las casillas encendidas tras la modificación.

pulsarCasilla:: Int -> [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
pulsarCasilla n lencendidas (a,b) = modificarCasilla n (modificarCasilla n (modificarCasilla n (modificarCasilla n (modificarCasilla n lencendidas (a,b)) (a-1,b)) (a+1,b)) (a,b-1)) (a,b+1)




--probarCombinacion n lencendidas lsolucion -> resultado
--Esta función comprueba si al pulsar la combinación de casillas “lsolucion” se apagan todas las luces que se encuentran en “lencendidas”. Devuelve resultado Verdadero si ha apagado todas las casillas, y Falso si la solucion no es válida.

--Argumentos:
	--N: Entero que indica el lado del tablero.
	--lencendidas: Lista de tuplas (F,C) que indica las casillas encendidas.
	--lsolucion: Lista de tuplas (F,C) que se quiere comprobar si es solución.
--Devuelve:
	--Valor Booleano de resultado.

probarCombinacion::Int -> [(Int,Int)] -> [(Int,Int)] -> Bool
probarCombinacion _ [] [] = True
probarCombinacion _ (x:xs) [] = False
probarCombinacion n lencendidas (x:lmarcadas) = probarCombinacion n (pulsarCasilla n lencendidas x) lmarcadas




--generarPosibles n -> lposibles
--Esta función genera todas las posibles combinaciones de tuplas (F,C) (casillas) que se pueden pulsar en un tablero de lado “n” para solucionarlo. Devuelve una lista la cual contiene listas de tuplas (F,C). 
--Argumentos:
	--N: Entero que indica el lado del tablero.
--Devuelve:
	--Lista de listas de tuplas (F,C) con todas las combinaciones posibles.

generarPosibles:: Int -> [[(Int,Int)]]
generarPosibles n = subs (crearCasillas n)

subs:: [a]->[[a]]
subs [] = [ [] ]
subs (x:xs) = map (x:) subsxs ++ subsxs
				where subsxs = subs xs



				
--crearCasillas n -> lcasillas
--Esta función genera todas las casillas de un tablero representándolas como tuplas (F,C). Devuelve una lista con todas estas tuplas (F,C).
--Argumentos:
	--N: Entero que indica el lado del tablero.
--Devuelve:
	--Lista de tuplas (F,C) con todas las casillas del tablero.

crearCasillas:: Int-> [(Int,Int)]
crearCasillas n = [i | i <- zip (casillaF n) (casillaC n n)]

casillaF :: Int -> [Int]
casillaF n 	| n>0 =[(i `mod` n)+1 | i<-[1..n*n]]
			|otherwise =[]
casillaC :: Int -> Int -> [Int]
casillaC n m	|n>0 =[n | i <- [1..m]] ++ casillaC (n-1) m
				|otherwise =[]



				
-- pruebas, ejemplos sobre algunos tableros.
prueba:: Int -> [[(Int,Int)]]
prueba n	|n==2 =(lights 2 [(1,2),(2,1),(2,2)])
			|n==3 =(lights 3 [(1,2),(2,1),(2,2)])
			|n==4 =(lights 4 [(2,1),(2,2),(3,1),(3,3),(3,4),(4,3),(4,4)])
			|n==5 =(lights 5 [(2,2), (2,4), (4,1), (4,2), (4,4), (4,5)])
			|otherwise =[[]]

