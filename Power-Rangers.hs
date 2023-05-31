module Library where
import PdePreludat

data Persona = UnaPersona{
    nombre :: String,
    habilidades :: [String],
    buena :: Bool
} deriving (Show,Eq)

data PowerRanger = UnPowerRanger{
    color :: String,
    habilidadesPower :: [String],
    nivelPelea :: Number
} deriving (Show,Eq)

juanpa = UnaPersona "Juan Pablo" ["Valiente","Veloz"] True

powerRojo = UnPowerRanger "rojo" ["Crecer","Invisibilidad"] 20

---- Punto 2 -----

habilidadesSuper :: Persona -> [String]
habilidadesSuper persona = map (++"Super") (habilidades persona)

sumarTamañoPalabra :: Persona -> Number
sumarTamañoPalabra persona = sum . map length . habilidades $ persona

convertirEnPowerRanger :: String -> Persona -> PowerRanger
convertirEnPowerRanger colorDado persona = UnPowerRanger {color=colorDado, habilidadesPower= habilidadesSuper persona, nivelPelea= sumarTamañoPalabra persona}

------ Punto 3 ------

personaBuena :: Persona -> Bool
personaBuena persona = buena persona   -- suponemos que esta bien

formarEquipoRanger :: [String] -> [Persona] -> [PowerRanger]
formarEquipoRanger colores personas = map convertirEnPowerRanger (filter personaBuena personas)

---- Punto 4 a) ------

findOrElse :: (a -> Bool) -> a -> [a] -> a
findOrElse funcion valor lista
    | find funcion lista \= null = find funcion lista     -- suponiendo que exista null
    | otherwise = valor

---- Punto 4 b) ----

powerRojo :: PowerRanger -> Bool
powerRojo power = color power == "rojo"

buscarPowerRojo :: [PowerRanger] -> PowerRanger
buscarPowerRojo equipo = filter powerRojo equipo

rangerLider :: [PowerRanger] -> PowerRanger
rangerLider equipo
    | length (buscarPowerRojo equipo) > 0 = head(buscarPowerRojo equipo)
    | otherwise = head equipo

---- Punto 5 a) ----

maximumBy :: Ord a => [a] -> a
maximumBy lista = maximum lista

---- Punto 5 b) ----

rangerMásPoderoso :: 
rangerMásPoderoso


----- Punto 6 ------

rangerHabilidoso ::
rangerHabilidoso