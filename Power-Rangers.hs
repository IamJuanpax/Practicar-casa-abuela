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

habilidadesSuper :: Persona -> [String]
habilidadesSuper persona = map (++"Super") (habilidades persona)

sumarTamañoPalabra :: Persona -> Number
sumarTamañoPalabra persona = sum . map length . habilidades $ persona

convertirEnPowerRanger :: String -> Persona -> PowerRanger
convertirEnPowerRanger colorDado persona = UnPowerRanger {color=colorDado, habilidadesPower= habilidadesSuper persona, nivelPelea= sumarTamañoPalabra persona}

personaBuena :: Persona -> Bool
personaBuena persona = buena persona   -- suponemos que esta bien

formarEquipoRanger :: [String] -> [Persona] -> [PowerRanger]
formarEquipoRanger colores personas = map convertirEnPowerRanger (filter personaBuena personas)

