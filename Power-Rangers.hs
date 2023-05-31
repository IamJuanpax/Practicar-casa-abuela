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

convertirEnPowerRanger :: String -> Persona -> PowerRanger
convertirEnPowerRanger colorDado persona = UnPowerRanger {color=colorDado, habilidadesPower= habilidadesSuper persona, nivelPelea= foldl length} -- con el fold hacer que se sumen el tamaño de las palabras de toda la lista

formarEquipoRanger :: [String] -> [Persona] -> [PowerRanger]
formarEquipoRanger colores personas = map convertirEnPowerRanger personas    --- No funciona ----