module Algorithms (
    quickSort,
    mergeSort,
    insertionSort,
    buscarProjeto,
    filtrarProjetos
) where
    
import Types

quickSort :: (Ord a) => [a]  -> [a]
quickSort [] = []
quickSort (x:xs) =
    let menores = quickSort [a | a <- xs, a <= x]
        maiores = quickSort [a | a <- xs, a > x]
    in menores ++ [x] ++ maiores

mergeSort :: (Ord a) => [a]  -> [a]
mergeSort xs = case xs of
    []        -> []
    [x]       -> [x]
    [x, y]    -> sort2 x y
    otherwise -> merge (mergeSort esquerda) (mergeSort direita)
  where
    (esquerda, direita) = splitAt (length xs `div` 2) xs
    sort2 a b = if a <= b
        then [a, b]
        else [b, a]
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
        | x <= y    = x : merge xs (y:ys)
        | otherwise = y : merge (x:xs) ys

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert x [] = [x]
    insert x (y:ys)
        | x <= y    = x : y : ys
        | otherwise = y : insert x ys


buscarProjeto::Int -> [Projeto] -> Maybe Projeto
buscarProjeto _ [] = Nothing
buscarProjeto id (x:xs)
        | id == idProjeto x = Just x
        | otherwise         = buscarProjeto id xs


filtrarProjetos::(Projeto -> Bool) -> [Projeto] -> [Projeto]
filtrarProjetos pred xs = filter pred xs
 