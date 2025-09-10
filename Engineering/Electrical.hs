module Engineering.Electrical (
    tensaoOhm,

    potenciaEletricaVI,
    potenciaEletricaRI,
    potenciaEletricaVR,

    resistenciaSerie,
    resistenciaParalelo,
    impedanciaAC,

    polarParaRetangular,
    retangularParaPolar
) where

import Types

-- V = I · R
tensaoOhm::Corrente -> Resistencia -> Tensao
tensaoOhm i r = i * r


-- P = V · I
-- P = R · I²
-- P = V² / R
potenciaEletricaVI::Tensao -> Corrente -> PotenciaEletrica
potenciaEletricaVI v i = v * i
potenciaEletricaRI::Resistencia -> Corrente -> PotenciaEletrica
potenciaEletricaRI r i = r * i**2
potenciaEletricaVR::Tensao -> Resistencia -> PotenciaEletrica
potenciaEletricaVR v r = v**2 / r


resistenciaSerie::[Resistencia] -> Resistencia
resistenciaSerie = sum
resistenciaParalelo::[Resistencia] -> Resistencia
resistenciaParalelo xs = recip $ sum $ map recip xs

-- Z = sqrt(R² + X²)
impedanciaAC::Resistencia -> Reatancia -> Impedancia
impedanciaAC r z = sqrt(r ** 2 + z ** 2)


polarParaRetangular::Double -> Angulo -> (Double, Double)
polarParaRetangular r a = (r * cos a, r * sin a)
retangularParaPolar::Double -> Double -> (Double, Angulo)
retangularParaPolar x y = (sqrt(x ** 2 + y ** 2), atan2 y x)