module Geometry (
    distanciaEntrePontos,
    distancia3D,
    pontoMedio,
    calcularArea,
    calcularPerimetro,
    calcularVolume,
    dentroDoPoligono,
    intersecaoRetas
    ) where

import Types

-- C² = A² + B²
distanciaEntrePontos::Ponto2D -> Ponto2D -> Distancia
distanciaEntrePontos (Ponto2D x1 y1) (Ponto2D x2 y2) =
    sqrt $ (x1 - x2)^2 + (y1 - y2)^2


-- D² = A² + B² + C²
distancia3D::Ponto3D -> Ponto3D -> Distancia
distancia3D (Ponto3D x1 y1 z1) (Ponto3D x2 y2 z2) =
    sqrt $ (x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2


pontoMedio::Ponto2D -> Ponto2D -> Ponto2D
pontoMedio (Ponto2D x1 y1) (Ponto2D x2 y2) =
    Ponto2D ((x1 + x2) / 2) ((y1 + y2) / 2)

edges::[Ponto2D] -> [(Ponto2D, Ponto2D)]
edges (x:xs) = zip (x:xs) (xs ++ [x])

calcularArea::Figura -> Area
calcularArea (Circulo r)            = pi * r ^ 2                                -- πr²
calcularArea (Esfera r)             = 4 * pi * r ^ 2                            -- 4πr² (muito engraçado isso na vdd kk)
calcularArea (Cilindro r h)         = 2 * pi * r * (r + h)                      -- 2tampa + lado = 2 * πr² + 2πrh = 2πr(r + altura)
calcularArea (Retangulo b h)        = b * h                                     -- bL
calcularArea (Paralelepipedo b l h) = 2 * (b * l + b * h + h * l)               -- bl + bl + bh + bh + hl + hl = 2(bl + bh + hl)
calcularArea (Triangulo a b c)      = sqrt $ p * (p - a') * (p - b') * (p - c') -- Formula de heron
    where
            a' = distanciaEntrePontos a b
        b' = distanciaEntrePontos a c
        c' = distanciaEntrePontos b c
        p  = (a' + b' + c') / 2
calcularArea (Poligono [])          = 0                                         -- fórmula do polígono (shoelace / Gauss)
calcularArea (Poligono [_])         = 0
calcularArea (Poligono [_, _])      = 0
calcularArea (Poligono ps)          = (sum $ map det2D $ edges ps) / 2
    where
            det2D ((Ponto2D x1 y1), (Ponto2D x2 y2)) =
            x1 * y2 - x2 * y1


calcularPerimetro::Figura -> Perimetro
calcularPerimetro (Circulo r)            = 2 * pi * r
calcularPerimetro (Retangulo b h)        = 2 * (b + h)
calcularPerimetro (Triangulo a b c)      = a' +  b' + c'
    where
            a' = distanciaEntrePontos a b
        b' = distanciaEntrePontos a c
        c' = distanciaEntrePontos b c
calcularPerimetro (Poligono ps)          = sum $ map distTupla $ edges ps
    where
            distTupla (a, b) = distanciaEntrePontos a b

calcularVolume::Figura -> Volume
calcularVolume (Esfera r)             = 4 / 3 * pi * r ^ 3                        -- (4/3)πr³
calcularVolume (Cilindro r h)         = h * pi * r ^ 2                            -- hπr²
calcularVolume (Paralelepipedo b l h) = b * l * h                                 -- blh


    
-- TODO trocar por determinante
intersecaoRetas::(Ponto2D, Ponto2D) -> (Ponto2D, Ponto2D) -> Maybe Ponto2D
intersecaoRetas (Ponto2D x1 y1, Ponto2D x2 y2) (Ponto2D x3 y3, Ponto2D x4 y4)
        | det == 0  = Nothing
        | otherwise = Just $ Ponto2D x y
    where
            det = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
        x = ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) / det
        y = ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) / det


-- Vê quantas vezes ele entra e sai do poligono pela esquerda até chegar ao ponto.
-- Entra -> Sai -> Entra -> Sai ...
-- numero impar => dentro
-- numero par   => fora
dentroDoPoligono::Ponto2D -> [Ponto2D] -> Bool
dentroDoPoligono _ [] = False
dentroDoPoligono p@(Ponto2D x y) ps = p `elem` ps || count `mod` 2 == 1
    where
            count = length $ filter (valido.intersectaX) $ edges ps
        entre a b = a > x
        valido Nothing = False
        valido (Just (Ponto2D a b)) = entre a b
        intersectaX = intersecaoRetas ((Ponto2D (- 2 ^ 14) y), p)