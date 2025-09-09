module DataStructures (
    inserirOrdenado,
    construirArvore,
    buscarArvore
) where
    
import Types


inserirOrdenado::(Ord a) => a -> [a] -> [a]
inserirOrdenado x xs = case xs of
        []     -> [x]
        (y:ys) -> if x > y
            then y:inserirOrdenado x ys
            else x:y:ys

construirArvore ::(Ord a) => [a] -> ArvoreBinaria a
construirArvore []  = Vazia
construirArvore xs  = inserirLista Vazia xs
    where
        inserirLista raiz []     = raiz
        inserirLista raiz (y:ys) = inserirLista (inserir raiz y) ys

        inserir Vazia x = No x Vazia Vazia
        inserir (No atual l r) x
            | x <= atual = No atual (inserir l x) r
            | otherwise  = No atual l (inserir r x) -- trocar nome. linhagem? caminho? filho?


buscarArvore::(Ord a) => a -> ArvoreBinaria a -> Bool
buscarArvore _ Vazia = False
buscarArvore alvo (No valor l r)
        | alvo == valor = True
        | alvo <  valor = buscarArvore alvo l
        | alvo >  valor = buscarArvore alvo r
