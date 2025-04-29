{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

data Personaje = Personaje{
    nombre :: String,
    poderBasico :: Personaje -> Personaje,
    superPoder :: Personaje -> Personaje,
    superPoderActivo :: Bool,
    cantidadDeVida :: Int
} deriving (Show)

-- Funciones para usar
sacarVida :: Personaje -> Int -> Personaje
sacarVida unPersonaje vida
    | vida >= cantidadDeVida unPersonaje = unPersonaje { cantidadDeVida = 0 }
    | otherwise = unPersonaje { cantidadDeVida = cantidadDeVida unPersonaje - vida }

sumarVida :: Personaje -> Int -> Personaje
sumarVida unPersonaje vida = unPersonaje {cantidadDeVida = cantidadDeVida unPersonaje + vida}

mitadVida :: Personaje -> Personaje
mitadVida unPersonaje = unPersonaje{cantidadDeVida = div (cantidadDeVida unPersonaje) 2}

type Personajes = [Personaje]

estaEnLasUltimas :: Personaje -> Bool
estaEnLasUltimas unPersonaje = cantidadDeVida unPersonaje < 800


-- Poderes
bolaEspinosa :: Personaje -> Personaje
bolaEspinosa unPersonaje = sacarVida unPersonaje 1000
  
lluviaDeTuercas :: String -> Personaje -> Personaje
lluviaDeTuercas tipo unPersonaje | tipo == "sanadoras" = sumarVida unPersonaje 800
                                 | tipo  == "dañinas" = mitadVida unPersonaje
                                 | otherwise = unPersonaje                                                 
                                            
granadaDeEspinas :: Int -> Personaje ->Personaje
granadaDeEspinas radio unPersonaje | radio > 3 && cantidadDeVida unPersonaje < 800 = unPersonaje {nombre = nombre unPersonaje ++ "Espinas estuvo aqui", superPoderActivo = False, cantidadDeVida = 0}
                                   | radio  > 3 = unPersonaje {nombre = nombre unPersonaje ++ "Espinas estuvo aqui" }
                                   | otherwise = bolaEspinosa unPersonaje

torretaCurativa :: Personaje -> Personaje
torretaCurativa unPersonaje = unPersonaje {superPoderActivo = True, cantidadDeVida = cantidadDeVida $ sumarVida unPersonaje 100}




-- Reportes
atacarConElPoderEspecial :: Personaje -> Personaje -> Personaje
atacarConElPoderEspecial unPersonaje elContrincante
    | superPoderActivo unPersonaje = (poderBasico unPersonaje).(superPoder unPersonaje) $ elContrincante
    | otherwise = elContrincante

quienesEstanEnLasUltimas :: Personajes -> [String]
quienesEstanEnLasUltimas listaPersonajes =  map (nombre) $ filter (estaEnLasUltimas) listaPersonajes



-- Modelos de personajes
espina :: Personaje
espina = Personaje {
    nombre = "Espina",
    poderBasico = (bolaEspinosa),
    superPoder = (granadaDeEspinas 5),
    superPoderActivo = True,
    cantidadDeVida = 4800
}

pamela :: Personaje
pamela = Personaje {
    nombre = "Pamela",
    poderBasico = (lluviaDeTuercas "sanadoras"),
    superPoder = (torretaCurativa),
    superPoderActivo = False,
    cantidadDeVida = 9600
}

todosLosPersonajes :: Personajes
todosLosPersonajes = [pamela, espina]
