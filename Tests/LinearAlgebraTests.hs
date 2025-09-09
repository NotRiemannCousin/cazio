module Tests.LinearAlgebraTests where
import Types
import LinearAlgebra

-- Testes para somarMatrizes



testeSomaMatrizes::Bool
testeSomaMatrizes =
    let m1 = Matriz [[1, 2], [3, 4]]
            m2 = Matriz [[5, 6], [7, 8]]
        resultado = somarMatrizes m1 m2
        esperado = Just (Matriz [[6, 8], [10, 12]])
    in resultado == esperado



testeSomaMatrizesIdentidade::Bool
testeSomaMatrizesIdentidade =
    let m1 = Matriz [[1, 0], [0, 1]]
            m2 = Matriz [[0, 0], [0, 0]]
        resultado = somarMatrizes m1 m2
        esperado = Just (Matriz [[1, 0], [0, 1]])
    in resultado == esperado

testeSomaMatrizesNegativas::Bool    
testeSomaMatrizesNegativas =
    let m1 = Matriz [[1, 2], [3, 4]]
            m2 = Matriz [[-1,-2], [-3,-4]]
        resultado = somarMatrizes m1 m2
        esperado = Just (Matriz [[0, 0], [0, 0]])
    in resultado == esperado



testeSomaMatrizesIncompativeis::Bool
testeSomaMatrizesIncompativeis =
    let m1 = Matriz [[1, 2], [3, 4]]
            m2 = Matriz [[1, 2, 3]]
        resultado = somarMatrizes m1 m2
    in resultado == Nothing



testeSomaMatrizes3x3::Bool
testeSomaMatrizes3x3 =
    let m1 = Matriz [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
            m2 = Matriz [[9, 8, 7], [6, 5, 4], [3, 2, 1]]
        resultado = somarMatrizes m1 m2
        esperado = Just (Matriz [[10, 10, 10], [10, 10, 10], [10, 10, 10]])
    in resultado == esperado
        

-- Testes para multiplicarMatrizes

testeMultiplicacaoMatrizes::Bool
testeMultiplicacaoMatrizes =
    let m1 = Matriz [[1, 2], [3, 4]]
            m2 = Matriz [[5, 6], [7, 8]]
        resultado = multiplicarMatrizes m1 m2
        esperado = Just (Matriz [[19, 22], [43, 50]])
    in resultado == esperado



testeMultiplicacaoIdentidade::Bool
testeMultiplicacaoIdentidade =
    let m1 = Matriz [[1, 2], [3, 4]]
            identidade = Matriz [[1, 0], [0, 1]]
        resultado = multiplicarMatrizes m1 identidade
        esperado = Just m1
    in resultado == esperado



testeMultiplicacaoZero::Bool
testeMultiplicacaoZero =
    let m1 = Matriz [[1, 2], [3, 4]]
            zero = Matriz [[0, 0], [0, 0]]
        resultado = multiplicarMatrizes m1 zero
        esperado = Just zero
    in resultado == esperado



testeMultiplicacaoMatrizesIncompativeis::Bool
testeMultiplicacaoMatrizesIncompativeis =
    let m1 = Matriz [[1, 2, 3], [4, 5, 6]]
            m2 = Matriz [[1, 2], [3, 4]]
        resultado = multiplicarMatrizes m1 m2
    in resultado == Nothing



testeMultiplicacaoEscalar::Bool
testeMultiplicacaoEscalar =
    let m1 = Matriz [[2]]
            m2 = Matriz [[3]]
        resultado = multiplicarMatrizes m1 m2
        esperado = Just (Matriz [[6]])
    in resultado == esperado


-- Testes para transpostaMatriz

testeTransposta2x2::Bool
testeTransposta2x2 =
    let matriz = Matriz [[1, 2], [3, 4]]
            resultado = transpostaMatriz matriz
        esperado = Matriz [[1, 3], [2, 4]]
    in resultado == esperado



testeTransposta3x2::Bool
testeTransposta3x2 =
    let matriz = Matriz [[1, 2, 3], [4, 5, 6]]
            resultado = transpostaMatriz matriz
        esperado = Matriz [[1, 4], [2, 5], [3, 6]]
    in resultado == esperado



testeTranspostaIdentidade::Bool
testeTranspostaIdentidade =
    let identidade = Matriz [[1, 0], [0, 1]]
            resultado = transpostaMatriz identidade
    in resultado == identidade



testeTranspostaVetor::Bool
testeTranspostaVetor =
    let vetor = Matriz [[1, 2, 3]] -- vetor linha
            resultado = transpostaMatriz vetor
        esperado = Matriz [[1], [2], [3]] -- vetor coluna
    in resultado == esperado



testeTranspostaSimetrica::Bool
testeTranspostaSimetrica =
    let matriz = Matriz [[1, 2, 3], [2, 4, 5], [3, 5, 6]]
            resultado = transpostaMatriz matriz
    in resultado == matriz-- matriz simétrica = sua transposta


-- Testes para determinante

testeDeterminante2x2::Bool
testeDeterminante2x2 =
    let matriz = Matriz [[3, 2], [1, 4]]
            det = determinante matriz
        esperado = Just 10.0-- 3*4- 2*1
    in det == esperado



testeDeterminanteIdentidade::Bool
testeDeterminanteIdentidade =
    let identidade = Matriz [[1, 0], [0, 1]]
            det = determinante identidade
        esperado = Just 1.0
    in det == esperado



testeDeterminanteZero::Bool
testeDeterminanteZero =
    let zero = Matriz [[0, 0], [0, 0]]
            det = determinante zero
        esperado = Just 0.0
    in det == esperado



testeDeterminanteMatrizSingular::Bool
testeDeterminanteMatrizSingular =
    let matriz = Matriz [[1, 2], [2, 4]] -- segunda linha é múltiplo da primeira
            det = determinante matriz
    in det == Just 0.0



testeDeterminante1x1::Bool
testeDeterminante1x1 =
    let matriz = Matriz [[5]]
            det = determinante matriz
        esperado = Just 5.0
    in det == esperado


-- Testes para resolverSistemaLinear

testeSistemaLinear2x2::Bool
testeSistemaLinear2x2 =
    let matrizA = Matriz [[2, 1], [1, 1]]
            vetorB = Vetor [3, 2]
        resultado = resolverSistemaLinear matrizA vetorB-- Sistema: 2x + y = 3, x + y = 2 => x = 1, y = 1
        esperado = Just (Vetor [1.0, 1.0])
    in resultado == esperado



testeSistemaLinearIdentidade::Bool
testeSistemaLinearIdentidade =
    let identidade = Matriz [[1, 0], [0, 1]]
            vetor = Vetor [3, 4]
        resultado = resolverSistemaLinear identidade vetor
        esperado = Just vetor
    in resultado == esperado



testeSistemaLinearSingular::Bool
testeSistemaLinearSingular =
    let matrizA = Matriz [[1, 2], [2, 4]] -- matriz singular
            vetorB = Vetor [3, 6]
        resultado = resolverSistemaLinear matrizA vetorB
    in resultado == Nothing



testeSistemaLinear1x1::Bool
testeSistemaLinear1x1 =
    let matrizA = Matriz [[2]]
            vetorB = Vetor [6]
        resultado = resolverSistemaLinear matrizA vetorB
        esperado = Just (Vetor [3.0]) -- 2x = 6 => x = 3
    in resultado == esperado



testeSistemaLinearZero::Bool
testeSistemaLinearZero =
    let matrizA = Matriz [[1, 0], [0, 1]]
            vetorB = Vetor [0, 0]
        resultado = resolverSistemaLinear matrizA vetorB
        esperado = Just (Vetor [0, 0])
    in resultado == esperado


-- Testes para produtoEscalar

testeProdutoEscalar::Bool
testeProdutoEscalar =
    let v1 = Vetor [1, 2, 3]
            v2 = Vetor [4, 5, 6]
        resultado = produtoEscalar v1 v2
        esperado = Just 32.0-- 1*4 + 2*5 + 3*6
    in resultado == esperado



testeProdutoEscalarOrtogonais::Bool
testeProdutoEscalarOrtogonais =
    let v1 = Vetor [1, 0]
            v2 = Vetor [0, 1]
        resultado = produtoEscalar v1 v2
        esperado = Just 0.0
    in resultado == esperado



testeProdutoEscalarMesmoVetor::Bool
testeProdutoEscalarMesmoVetor =
    let v1 = Vetor [3, 4]
            resultado = produtoEscalar v1 v1
        esperado = Just 25.0-- 3*3 + 4*4 = 25
    in resultado == esperado



testeProdutoEscalarIncompativel::Bool
testeProdutoEscalarIncompativel =
    let v1 = Vetor [1, 2]
            v2 = Vetor [1, 2, 3]
        resultado = produtoEscalar v1 v2
    in resultado == Nothing



testeProdutoEscalarNegativo::Bool
testeProdutoEscalarNegativo =
    let v1 = Vetor [1, 2]
            v2 = Vetor [-1,-2]
        resultado = produtoEscalar v1 v2
        esperado = Just (-5.0) -- 1*(-1) + 2*(-2) =-5
    in resultado == esperado


-- Testes para normaVetor

testeNormaVetor::Bool
testeNormaVetor =
    let vetor = Vetor [3, 4]
            norma = normaVetor vetor
    in abs (norma- 5.0) < 0.001-- sqrt(3² + 4²)



testeNormaVetorUnitario::Bool
testeNormaVetorUnitario =
    let vetor = Vetor [1, 0, 0]
            norma = normaVetor vetor
    in abs (norma- 1.0) < 0.001



testeNormaVetorZero::Bool
testeNormaVetorZero =
    let vetor = Vetor [0, 0, 0]
            norma = normaVetor vetor
    in abs norma < 0.001



testeNormaVetor3D::Bool
testeNormaVetor3D =
    let vetor = Vetor [1, 2, 2]
            norma = normaVetor vetor
    in abs (norma- 3.0) < 0.001-- sqrt(1² + 2² + 2²)



testeNormaVetorNegativo::Bool
testeNormaVetorNegativo =
    let vetor = Vetor [-3,-4]
            norma = normaVetor vetor
    in abs (norma- 5.0) < 0.001 -- norma sempre positiva


-- Testes para anguloEntreVetores

testeAnguloVetoresOrtogonais::Bool
testeAnguloVetoresOrtogonais =
    let v1 = Vetor [1, 0]
            v2 = Vetor [0, 1]
        angulo = anguloEntreVetores v1 v2
        esperado = Just (pi/2) -- 90 graus
    in fmap (abs . subtract (pi/2)) angulo < Just 0.001



testeAnguloVetoresParalelos::Bool
testeAnguloVetoresParalelos =
    let v1 = Vetor [1, 2]
            v2 = Vetor [2, 4] -- v2 = 2*v1
        angulo = anguloEntreVetores v1 v2
    in fmap (abs) angulo < Just 0.001 -- 0 radianos



testeAnguloVetoresOpostos::Bool
testeAnguloVetoresOpostos =
    let v1 = Vetor [1, 0]
            v2 = Vetor [-1, 0]
        angulo = anguloEntreVetores v1 v2
        esperado = Just pi -- 180 graus
    in fmap (abs . subtract pi) angulo < Just 0.001



testeAnguloVetoresMesmos::Bool
testeAnguloVetoresMesmos =
    let v1 = Vetor [3, 4]
            angulo = anguloEntreVetores v1 v1
    in fmap (abs) angulo < Just 0.001 -- 0 radianos



testeAnguloVetoresZero::Bool
testeAnguloVetoresZero =
    let v1 = Vetor [0, 0]
            v2 = Vetor [1, 1]
        angulo = anguloEntreVetores v1 v2
    in angulo == Nothing -- vetor zero não tem direção definida

executarTestesAlgebraLinear :: IO ()
executarTestesAlgebraLinear = do
    putStrLn "======================================"
    putStrLn "    TESTES ÁLGEBRA LINEAR"
    putStrLn "======================================"
    
    putStrLn "\n -- TESTES SOMA MATRIZES --"
    putStrLn $ "Básico: " ++ show testeSomaMatrizes
    putStrLn $ "Identidade: " ++ show testeSomaMatrizesIdentidade
    putStrLn $ "Negativas: " ++ show testeSomaMatrizesNegativas
    putStrLn $ "Incompatíveis: " ++ show testeSomaMatrizesIncompativeis
    putStrLn $ "3x3: " ++ show testeSomaMatrizes3x3
    
    putStrLn "\n -- TESTES MULTIPLICAÇÃO MATRIZES --"
    putStrLn $ "Básico: " ++ show testeMultiplicacaoMatrizes
    putStrLn $ "Identidade: " ++ show testeMultiplicacaoIdentidade
    putStrLn $ "Zero: " ++ show testeMultiplicacaoZero
    putStrLn $ "Incompatíveis: " ++ show testeMultiplicacaoMatrizesIncompativeis
    putStrLn $ "Escalar: " ++ show testeMultiplicacaoEscalar
    
    putStrLn "\n -- TESTES TRANSPOSTA --"
    putStrLn $ "2x2: " ++ show testeTransposta2x2
    putStrLn $ "3x2: " ++ show testeTransposta3x2
    putStrLn $ "Identidade: " ++ show testeTranspostaIdentidade
    putStrLn $ "Vetor: " ++ show testeTranspostaVetor
    putStrLn $ "Simétrica: " ++ show testeTranspostaSimetrica
    
    putStrLn "\n -- TESTES DETERMINANTE --"
    putStrLn $ "2x2: " ++ show testeDeterminante2x2
    putStrLn $ "Identidade: " ++ show testeDeterminanteIdentidade
    putStrLn $ "Zero: " ++ show testeDeterminanteZero
    putStrLn $ "Matriz Singular: " ++ show testeDeterminanteMatrizSingular
    putStrLn $ "1x1: " ++ show testeDeterminante1x1
    
    putStrLn "\n -- TESTES SISTEMA LINEAR --"
    putStrLn $ "2x2: " ++ show testeSistemaLinear2x2
    putStrLn $ "Identidade: " ++ show testeSistemaLinearIdentidade
    putStrLn $ "Singular: " ++ show testeSistemaLinearSingular
    putStrLn $ "1x1: " ++ show testeSistemaLinear1x1
    putStrLn $ "Zero: " ++ show testeSistemaLinearZero
    
    putStrLn "\n -- TESTES PRODUTO ESCALAR --"
    putStrLn $ "Básico: " ++ show testeProdutoEscalar
    putStrLn $ "Ortogonais: " ++ show testeProdutoEscalarOrtogonais
    putStrLn $ "Mesmo Vetor: " ++ show testeProdutoEscalarMesmoVetor
    putStrLn $ "Incompatível: " ++ show testeProdutoEscalarIncompativel
    putStrLn $ "Negativo: " ++ show testeProdutoEscalarNegativo
    
    putStrLn "\n -- TESTES NORMA VETOR --"
    putStrLn $ "Básico: " ++ show testeNormaVetor
    putStrLn $ "Unitário: " ++ show testeNormaVetorUnitario
    putStrLn $ "Zero: " ++ show testeNormaVetorZero
    putStrLn $ "3D: " ++ show testeNormaVetor3D
    putStrLn $ "Negativo: " ++ show testeNormaVetorNegativo
    
    putStrLn "\n -- TESTES ÂNGULO VETORES --"
    putStrLn $ "Ortogonais: " ++ show testeAnguloVetoresOrtogonais
    putStrLn $ "Paralelos: " ++ show testeAnguloVetoresParalelos
    putStrLn $ "Opostos: " ++ show testeAnguloVetoresOpostos
    putStrLn $ "Mesmos: " ++ show testeAnguloVetoresMesmos
    putStrLn $ "Zero: " ++ show testeAnguloVetoresZero

testesAlgebraLinear :: [(String, Bool)]
testesAlgebraLinear = [
        -- Soma de Matrizes (5 testes)
        ("Soma Matrizes Básico", testeSomaMatrizes),
        ("Soma Matrizes Identidade", testeSomaMatrizesIdentidade),
        ("Soma Matrizes Negativas", testeSomaMatrizesNegativas),
        ("Soma Matrizes Incompatíveis", testeSomaMatrizesIncompativeis),
        ("Soma Matrizes 3x3", testeSomaMatrizes3x3),
        
        -- Multiplicação de Matrizes (5 testes)
        ("Multiplicação Matrizes Básico", testeMultiplicacaoMatrizes),
        ("Multiplicação Identidade", testeMultiplicacaoIdentidade),
        ("Multiplicação Zero", testeMultiplicacaoZero),
        ("Multiplicação Matrizes Incompatíveis", testeMultiplicacaoMatrizesIncompativeis),
        ("Multiplicação Escalar", testeMultiplicacaoEscalar),
        
        -- Transposta (5 testes)
        ("Transposta 2x2", testeTransposta2x2),
        ("Transposta 3x2", testeTransposta3x2),
        ("Transposta Identidade", testeTranspostaIdentidade),
        ("Transposta Vetor", testeTranspostaVetor),
        ("Transposta Simétrica", testeTranspostaSimetrica),
        
        -- Determinante (5 testes)
        ("Determinante 2x2", testeDeterminante2x2),
        ("Determinante Identidade", testeDeterminanteIdentidade),
        ("Determinante Zero", testeDeterminanteZero),
        ("Determinante Matriz Singular", testeDeterminanteMatrizSingular),
        ("Determinante 1x1", testeDeterminante1x1),
        
        -- Sistema Linear (5 testes)
        ("Sistema Linear 2x2", testeSistemaLinear2x2),
        ("Sistema Linear Identidade", testeSistemaLinearIdentidade),
        ("Sistema Linear Singular", testeSistemaLinearSingular),
        ("Sistema Linear 1x1", testeSistemaLinear1x1),
        ("Sistema Linear Zero", testeSistemaLinearZero),
        
        -- Produto Escalar (5 testes)
        ("Produto Escalar Básico", testeProdutoEscalar),
        ("Produto Escalar Ortogonais", testeProdutoEscalarOrtogonais),
        ("Produto Escalar Mesmo Vetor", testeProdutoEscalarMesmoVetor),
        ("Produto Escalar Incompatível", testeProdutoEscalarIncompativel),
        ("Produto Escalar Negativo", testeProdutoEscalarNegativo),
        
        -- Norma Vetor (5 testes)
        ("Norma Vetor Básico", testeNormaVetor),
        ("Norma Vetor Unitário", testeNormaVetorUnitario),
        ("Norma Vetor Zero", testeNormaVetorZero),
        ("Norma Vetor 3D", testeNormaVetor3D),
        ("Norma Vetor Negativo", testeNormaVetorNegativo),
        
        -- Ângulo Vetores (5 testes)
        ("Ângulo Vetores Ortogonais", testeAnguloVetoresOrtogonais),
        ("Ângulo Vetores Paralelos", testeAnguloVetoresParalelos),
        ("Ângulo Vetores Opostos", testeAnguloVetoresOpostos),
        ("Ângulo Vetores Mesmos", testeAnguloVetoresMesmos),
        ("Ângulo Vetores Zero", testeAnguloVetoresZero)
    ]