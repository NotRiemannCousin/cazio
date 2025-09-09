module LinearAlgebra (
    somarMatrizes,
    transpostaMatriz,
    multiplicarMatrizes,
    determinante,
    resolverSistemaLinear,
    
    produtoEscalar,
    normaVetor,
    anguloEntreVetores,
) where

import Types


-- Vetores

unsafeProdutoEscalar::[Double] -> [Double] -> Double
unsafeProdutoEscalar xs ys = sum $ zipWith (*) xs ys

produtoEscalar::Vetor -> Vetor -> Maybe Double
produtoEscalar (Vetor xs) (Vetor ys)
        | length xs /= length ys = Nothing
        | otherwise              = Just $ unsafeProdutoEscalar xs ys


normaVetor::Vetor -> Double
normaVetor (Vetor xs) = sqrt $ sum $ map (^2) xs


anguloEntreVetores :: Vetor -> Vetor -> Maybe Angulo
anguloEntreVetores u@(Vetor xs) v@(Vetor ys)
    | length xs /= length ys     = Nothing
    | normaU == 0 || normaV == 0 = Nothing
    | otherwise                  = Just $ acos $ (unsafeProdutoEscalar xs ys) / (normaU * normaV)
    where
            normaU = normaVetor u
        normaV = normaVetor v



-- Matrizes

tudoIgual::Eq a => [a] -> Bool
tudoIgual []     = True
tudoIgual (x:xs) = all (==x) xs


checarTamanhoSum::Matriz -> Matriz -> Bool
checarTamanhoSum (Matriz m1) (Matriz m2) =
        length m1 == length m2 && tudoIgual (colTam m1 ++ colTam m2)
    where
            colTam = map length


somarMatrizes::Matriz -> Matriz -> Maybe Matriz
somarMatrizes m1@(Matriz xss) m2@(Matriz yss)
        | checarTamanhoSum m1 m2 == False = Nothing
        | otherwise                    = Just $ Matriz $ map somarCols $ zip xss yss
    where
            somarCols::([Double], [Double]) -> [Double]
        somarCols (a, b) = zipWith (+) a b


transpostaMatriz :: Matriz -> Matriz
transpostaMatriz (Matriz xss) = Matriz (transposta' xss)
    where
            transposta' :: [[Double]] -> [[Double]]
        transposta' ([] : _) = []
        transposta' xs = (map head xs) : transposta' (map tail xs)



checarTamanhoMul :: Matriz -> Matriz -> Bool
checarTamanhoMul (Matriz m1) (Matriz m2) =
        tudoIgual (colTam m1) && tudoIgual (colTam m2) &&
        case m1 of
            []    -> True
            (r:_) -> length r == length m2
    where
            colTam = map length


multiplicarMatrizes :: Matriz -> Matriz -> Maybe Matriz
multiplicarMatrizes m1@(Matriz xss) m2
    | checarTamanhoMul m1 m2 == False = Nothing
    | otherwise                       = Just $ Matriz resultado
    where
            yssT = getMatriz $ transpostaMatriz m2
        getMatriz (Matriz xs) = xs
        resultado = [ [ unsafeProdutoEscalar lin col | col <- yssT ] | lin <- xss ]
    


quadrada xs = tudoIgual (length xs : map length xs)

determinante :: Matriz -> Maybe Double
determinante (Matriz m)
        | not (quadrada m) = Nothing
        | otherwise = Just (det m)
    where
            det [] = 1
        det [[x]] = x
        det (linha:resto) = sum $ zipWith (\j x -> (-1)^j * x * det (menor j resto)) [0..] linha
        
        menor j resto = map (remover j) resto
        remover j linha = take j linha ++ drop (j+1) linha


resolverSistemaLinear :: Matriz -> Vetor -> Maybe Vetor
resolverSistemaLinear mat@(Matriz m) (Vetor v)
        | not (quadrada m) || length m /= length v = Nothing
        | otherwise = case determinante mat of
            Nothing -> Nothing
            Just detA -> if detA == 0
                then Nothing
                else Just $ Vetor [det (substitui i) / detA | i <- [0..n-1]]
    where
            n = length m
        
        substitui col = [take col linha ++ [v !! i] ++ drop (col+1) linha | (i, linha) <- zip [0..] m]
        
        det [] = 1
        det [[x]] = x
        det m = sum [(-1)^j * (head m !! j) * det (menor m 0 j) | j <- [0..length m - 1]]
        
        menor m i j = [remove j linha | (k, linha) <- zip [0..] m, k /= i]
            where remove j linha = take j linha ++ drop (j+1) linha