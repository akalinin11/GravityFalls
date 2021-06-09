module Library where
import PdePreludat

--Kalinin Alexander 


--Parte A

--Modelado de Personas

data Persona = UnaPersona{
  nombre :: String
, direccion :: String
, dinero :: Number
, comidaFavorita :: Comida
, cupones :: [String]
} deriving (Show)

--Modelado de Comidas
data Comida = UnaComida{
  nombree :: String
, costo :: Number
, ingredientes :: [String]    
} deriving (Show)



--Paula

paula = UnaPersona{
  nombre = "Paula"
, direccion = "Thames 1585"
, dinero = 3600
, comidaFavorita = hamburguesaDeluxe
, cupones  = []
} 

-- Hamburguesa Deluxe

hamburguesaDeluxe = UnaComida{
  nombree = "Hamburguesa Deluxe"
, costo = 350   
, ingredientes = ["pan", "carne", "lechuga","tomate", "panceta", "queso", "huevo frito"]
} 

-- Rusito

alex= UnaPersona{
  nombre = "Alexander"
, direccion = "Queti 1600" 
, dinero = 6000
, comidaFavorita = hamburguesaDeluxe
, cupones  = []
} 


papas = UnaComida{
  nombree = "papas"
, costo = 10 
, ingredientes = ["papas" ]
} 


--Parte B

--1)

comprar :: Persona->Comida->Persona
comprar aPersona  aComida 
 | alcanzaPlata (dinero aPersona) (costo aComida) && seraNuevaComidaFavorita (costo aComida)  =  cambiarComidaYDescontar aComida aPersona (costo aComida)
 | alcanzaPlata (dinero aPersona) (costo aComida) =  descontarCosto aPersona (costo aComida)
 | otherwise = aPersona

alcanzaPlata :: Number->Number->Bool   --Condicion 1
alcanzaPlata dinero  = (dinero >) 

seraNuevaComidaFavorita :: Number ->Bool --Condicion 2
seraNuevaComidaFavorita = (<200)

cambiarComidaYDescontar :: Comida->Persona->Number->Persona  --Dos acciones q se toman
cambiarComidaYDescontar aComida aPersona = nuevaComidaFav aComida . descontarCosto aPersona 
--Se delega las tareas
descontarCosto :: Persona->Number->Persona
descontarCosto aPersona costo = aPersona{dinero= dinero aPersona - costo} 

nuevaComidaFav :: Comida->Persona->Persona
nuevaComidaFav nuevaComida aPersona = aPersona{comidaFavorita= nuevaComida}

--2)

carritoDeCompras :: Persona->[Comida]->Persona
carritoDeCompras aPersona  =  agregadoXEmpague 100 . realizarCompras aPersona

realizarCompras :: Persona->[Comida]->Persona
realizarCompras aPersona = foldl (comprar) aPersona 

agregadoXEmpague :: Number->Persona->Persona
agregadoXEmpague  dineroExtra aPersona = aPersona{dinero= dinero aPersona - dineroExtra}


--Cupones--
type Cupon = Comida->Comida

semanaVegana :: Cupon
semanaVegana aComida | esVegana aComida = reducirCosto 50 aComida
                     | otherwise = aComida


esVegana :: Comida->Bool
esVegana  = (==0). length . ingredientesCarne


ingredientesCarne :: Comida->[String]
ingredientesCarne aComida = filter (hayIngredienteConCarne) (ingredientes aComida)

hayIngredienteConCarne :: String->Bool
hayIngredienteConCarne bIngrediente = elem bIngrediente ingredientesNoVeganos

ingredientesNoVeganos = ["carne","huevo", "queso"]

reducirCosto :: Number->Cupon
reducirCosto porcentaje aComida = aComida{costo= restarCosto (costo aComida) porcentaje}

restarCosto :: Number->Number->Number
restarCosto costoComida  =  (costoComida - ) . (costoComida *) . (/100)
-------------

esoNoEsCocaPapi ::  String->Cupon
esoNoEsCocaPapi bebida aComida = aComida {nombree= nombree aComida ++ " party" , ingredientes = ingredientes aComida++[bebida] }
-------

sinTACCis :: Cupon
sinTACCis aComida = aComida{ingredientes= ingredientes aComida ++ [" libre de gluten"]}

-------

findeVegetariano :: Cupon
findeVegetariano aComida | esVegetariana aComida = reducirCosto 30 aComida
                         | otherwise = aComida


esVegetariana :: Comida->Bool
esVegetariana  = (==0). length . ingredientesCarne'

ingredientesCarne' :: Comida->[String]
ingredientesCarne' aComida = filter (hayIngredienteConCarne') (ingredientes aComida)

hayIngredienteConCarne' :: String->Bool
hayIngredienteConCarne' bIngrediente = elem bIngrediente ingredientesNoVegetarianos

ingredientesNoVegetarianos = ["carne"]
-----------


largaDistancia :: Cupon
largaDistancia  = perderIngredientes . aumentarCostoPorLejania 

aumentarCostoPorLejania :: Comida->Comida
aumentarCostoPorLejania aComida = aComida{costo= costo aComida +50}

perderIngredientes :: Comida->Comida
perderIngredientes aComida = aComida{ingredientes = ingredientesNoPerdidos (ingredientes aComida)}


ingredientesNoPerdidos :: [String]->[String]
ingredientesNoPerdidos = filter (cantidadDeLetrasMenor 10) 

cantidadDeLetrasMenor:: Number->String->Bool
cantidadDeLetrasMenor n  = (<n) . length 

------------PARTE C---------------

