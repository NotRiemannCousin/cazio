module Algorithms where
    
import Types

quickSort::(Ord a) => [a]-> [a]


mergeSort::(Ord a) => [a]-> [a]


insertionSort::(Ord a) => [a]-> [a]


buscarProjeto::Int-> [Projeto]-> Maybe Projeto


inserirOrdenado::(Ord a) => a-> [a]-> [a]


construirArvore::[a]-> ArvoreBinaria a


buscarArvore::(Ord a) => a-> ArvoreBinaria a-> Bool


filtrarProjetos::(Projeto-> Bool)-> [Projeto]-> [Projeto]
 