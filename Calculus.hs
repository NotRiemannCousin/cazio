module Calculus (
    avaliarFuncao,
    derivadaNumerica,
    integralNumerica,
    encontrarRaizes,
    encontrarMaximo,
    encontrarMinimo,
    calcularComprimentoCurva
) where

import Types

ln = logBase (exp 1)

avaliarFuncao::Funcao -> Double -> Double
avaliarFuncao (Funcao (Linear a b) _)              x = a*x + b           -- ax + b
avaliarFuncao (Funcao (Quadratica a b c) _)        x = a*x^2 + b*x + c   -- ax² + bx + c
avaliarFuncao (Funcao (Exponencial a b) _)         x = a*exp(b*x)        -- a * e^(bx)
avaliarFuncao (Funcao (Logaritmica a b) _)         x = a*ln(b*x)         -- a * ln(bx)
avaliarFuncao (Funcao (Trigonometrica tipo a b) _) x = a*func(x) + b -- a * func(bx)
    where
        func = case tipo of
            Seno      -> sin
            Cosseno   -> cos
            Tangente  -> tan




epsilon = 2 ** (-6)
meioEpsilon = epsilon/2

-- derivada central, f'(x) = lim h->0(f(x+h) - f(x-h))/(2h)
derivadaNumerica::Funcao -> Double -> Double
derivadaNumerica func x = (avaliarFuncao func (x + meioEpsilon) - avaliarFuncao func (x - meioEpsilon)) / epsilon



-- Soma de Riemmann normal
integralNumerica::Funcao -> Double -> Double -> Int -> Double
integralNumerica func a b passos = integralNumerica' func a b 0
        where
            h = (b - a) / fromIntegral passos
            integralNumerica'::Funcao -> Double -> Double -> Double -> Double
            integralNumerica' func a b acc
                    | a >= b    = acc
                    | otherwise = integralNumerica' func (a+h) b (acc + h * avaliarFuncao func a)




entre a b x = a <= x && x <= b

encontrarRaizes :: Funcao -> Double -> Double -> [Double]
encontrarRaizes (Funcao (Linear a b) _)              l r
        | a == 0    = []
        | otherwise = filter (entre l r) [p]
    where
        p = (-b) / a

encontrarRaizes (Funcao (Quadratica a b c) _)        l r
        | a == 0  || delta < 0 = []
        | delta == 0           = filter (entre l r) [r1]
        | otherwise            = filter (entre l r) [r1, r2]
    where
        delta = b ** 2 - 4 * a * c
        r1    = ((-b) - sqrt delta)/(2 * a)
        r2    = ((-b) + sqrt delta)/(2 * a)

-- encontrarRaizes (Funcao (Exponencial a b) _)      Não tem!!

encontrarRaizes (Funcao (Logaritmica a b) _)         l r
        | a == 0    = []
        | otherwise = filter (entre l r) [1/b]
-- encontrarRaizes (Funcao (Trigonometrica tipo a b) _) a b = 

encontrarMaximo :: Funcao -> Double -> Double -> Maybe Double
encontrarMaximo func a b = encontrarExtremo func a b True

encontrarMinimo :: Funcao -> Double -> Double -> Maybe Double
encontrarMinimo func a b = encontrarExtremo func a b False

encontrarExtremo :: Funcao -> Double -> Double -> Bool -> Maybe Double
encontrarExtremo func a b buscaMaximo =
    let passos = 1000
        h = (b - a) / fromIntegral passos
        pontos = [a + fromIntegral i * h | i <- [0..passos]]
        valores = map (avaliarFuncao func) pontos
        pares = zip pontos valores
        comparador = if buscaMaximo then (\(_,y1) (_,y2) -> compare y1 y2) else (\(_,y1) (_,y2) -> compare y2 y1)
    in case pares of
        [] -> Nothing
        (x:xs) -> Just $ fst $ foldl (\acc p -> if comparador acc p == GT then acc else p) x xs

calcularComprimentoCurva :: Funcao -> Double -> Double -> Double
calcularComprimentoCurva func a b =
    let passos = 1000
        h = (b - a) / fromIntegral passos
        comprimento = sum [sqrt (h^2 + (avaliarFuncao func (x + h) - avaliarFuncao func x)^2) | x <- [a, a+h .. b-h]]
    in comprimento