module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

data Personaje = Personaje{
    nombre :: String,
    poderBasico :: String,
    superPoder :: String,
    superPoderActivo :: Bool,
    cantidadDeVida :: Int
} deriving (Show, Eq)

-- Funciones para usar

sacarVida :: Personaje -> Int -> Personaje
sacarVida unPersonaje vida
    | vida >= cantidadDeVida unPersonaje = unPersonaje { cantidadDeVida = 0 }
    | otherwise = unPersonaje { cantidadDeVida = cantidadDeVida unPersonaje - vida }

sumarVida :: Personaje -> Int -> Personaje
sumarVida unPersonaje vida = unPersonaje {cantidadDeVida = cantidadDeVida unPersonaje + vida}

mitadVida :: Personaje -> Personaje
mitadVida unPersonaje = unPersonaje{cantidadDeVida = div (cantidadDeVida unPersonaje) 2}

-- Poderes

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa unPersonaje = sacarVida unPersonaje 1000
  
lluviaDeTuercas :: Personaje -> String -> Personaje -> Personaje
lluviaDeTuercas unPersonaje tipo otroPersonaje | tipo == "sanadoras" = sumarVida otroPersonaje 800
                                                 | tipo  == "dañinas" = mitadVida otroPersonaje
                                                 | otherwise = otroPersonaje                                                 
                                            
granadaDeEspinas :: Int -> Personaje -> Personaje ->Personaje
granadaDeEspinas radio unPersonaje otroPersonaje | radio > 3 && cantidadDeVida otroPersonaje < 800 = (unPersonaje {nombre = nombre unPersonaje ++ "Espinas estuvo aqui"  ,superPoderActivo = False, cantidadDeVida = 0})
                                                   | radio  > 3 = (unPersonaje {nombre = nombre unPersonaje ++ "Espinas estuvo aqui" })
                                                   | otherwise = bolaEspinosa unPersonaje

torretaCurativa :: Personaje -> Personaje
torretaCurativa unPersonaje = unPersonaje {superPoderActivo = True, cantidadDeVida = cantidadDeVida $ sumarVida unPersonaje 100}


-- Reportes

-- Modelos de personajes
espina :: Personaje
espina = Personaje { -- falta radio
    nombre = "Espina",
    poderBasico = "Bola Espinosa",
    superPoder = "Granada de Espinas",
    superPoderActivo = True,
    cantidadDeVida = 4800
}



