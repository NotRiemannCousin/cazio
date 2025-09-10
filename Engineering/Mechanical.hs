module Engineering.Mechanical (
    calcularTorque,
    velocidadeAngular,
    aceleracaocentripeta,

    energiaCinetica,
    energiaPotencial,

    centroMassaX
) where

import Types

--  τ = F · r · sin(θ)
calcularTorque::Forca -> Distancia -> Angulo -> Torque
calcularTorque f r a = f * r * sin a
--  ω = v / r
velocidadeAngular::Velocidade -> Raio -> VelocidadeAngular
velocidadeAngular = (/)
-- ac = v² / r
aceleracaocentripeta::Velocidade -> Raio -> Aceleracao
aceleracaocentripeta v r = v ^ 2 / r

-- Ec = 0.5 * m * v²
energiaCinetica::Massa -> Velocidade -> Energia
energiaCinetica m v = 0.5 * m * v ^ 2
-- Ep = m · g · h
energiaPotencial::Massa -> Altura -> Energia
energiaPotencial m h = m * g * h
    where
        g = 9.81

-- xcm = Σmi · Σxi / Σmi
centroMassaX::[(Massa, Distancia)] -> Distancia
centroMassaX objetos = sum [a * b | (a,b) <- objetos] / somaMassas
    where
        somaMassas = sum $ map fst objetos