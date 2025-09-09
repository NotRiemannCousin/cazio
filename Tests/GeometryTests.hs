module Tests.GeometryTests where
import Types
import Geometry

-- =================================
-- Testes para distanciaEntrePontos
-- =================================

testeDistancia2DBasico::Bool
testeDistancia2DBasico =
    let p1 = Ponto2D 0 0
            p2 = Ponto2D 3 4
        resultado = distanciaEntrePontos p1 p2
    in abs (resultado- 5.0) < 0.001



testeDistancia2DMesmosPontos::Bool
testeDistancia2DMesmosPontos =
    let p1 = Ponto2D 5 7
            resultado = distanciaEntrePontos p1 p1
    in abs resultado < 0.001



testeDistancia2DNegativa::Bool
testeDistancia2DNegativa =
    let p1 = Ponto2D (-2) (-3)
            p2 = Ponto2D 1 1
        resultado = distanciaEntrePontos p1 p2
        esperado = sqrt ((1-(-2))^2 + (1-(-3))^2) -- sqrt(9+16) = 5
    in abs (resultado- esperado) < 0.001



testeDistancia2DDecimal::Bool
testeDistancia2DDecimal =
    let p1 = Ponto2D 1.5 2.5
            p2 = Ponto2D 4.5 6.5
        resultado = distanciaEntrePontos p1 p2
        esperado = sqrt ((4.5-1.5)^2 + (6.5-2.5)^2) -- sqrt(9+16) = 5
    in abs (resultado- esperado) < 0.001



testeDistancia2DGrande::Bool
testeDistancia2DGrande =
    let p1 = Ponto2D 0 0
            p2 = Ponto2D 1000000 1000000
        resultado = distanciaEntrePontos p1 p2
        esperado = sqrt (2 * 1000000^2)
    in abs (resultado- esperado) < 1000 -- tolerância maior para números grandes 


-- Testes para distancia3D

testeDistancia3DBasico::Bool
testeDistancia3DBasico =
    let p1 = Ponto3D 1 2 2
            p2 = Ponto3D 4 6 2
        resultado = distancia3D p1 p2
        esperado = 5.0 -- sqrt((4-1)² + (6-2)² + (2-2)²) = sqrt(9+16+0)
    in abs (resultado- esperado) < 0.001



testeDistancia3DMesmosPontos::Bool
testeDistancia3DMesmosPontos =
    let p1 = Ponto3D 3 4 5
            resultado = distancia3D p1 p1
    in abs resultado < 0.001



testeDistancia3DCompleto::Bool
testeDistancia3DCompleto =
    let p1 = Ponto3D 0 0 0
            p2 = Ponto3D 2 3 6
        resultado = distancia3D p1 p2
        esperado = sqrt (2^2 + 3^2 + 6^2) -- sqrt(4+9+36) = 7
    in abs (resultado- esperado) < 0.001



testeDistancia3DNegativa::Bool
testeDistancia3DNegativa =
    let p1 = Ponto3D (-1) (-2) (-3)
            p2 = Ponto3D 1 2 3
        resultado = distancia3D p1 p2
        esperado = sqrt (2^2 + 4^2 + 6^2) -- sqrt(4+16+36) = sqrt(56)
    in abs (resultado- esperado) < 0.001



testeDistancia3DPequena::Bool
testeDistancia3DPequena =
    let p1 = Ponto3D 0.001 0.001 0.001
            p2 = Ponto3D 0.002 0.002 0.002
        resultado = distancia3D p1 p2
        esperado = sqrt (3 * 0.001^2)
    in abs (resultado- esperado) < 0.0001 


-- Testes para pontoMedio

testePontoMedioBasico::Bool
testePontoMedioBasico =
    let p1 = Ponto2D 0 0
            p2 = Ponto2D 4 6
        resultado = pontoMedio p1 p2
        esperado = Ponto2D 2 3
    in resultado == esperado



testePontoMedioNegativo::Bool
testePontoMedioNegativo =
    let p1 = Ponto2D (-2) (-4)
            p2 = Ponto2D 2 4
        resultado = pontoMedio p1 p2
        esperado = Ponto2D 0 0
    in resultado == esperado



testePontoMedioMesmoPonto::Bool
testePontoMedioMesmoPonto =
    let p1 = Ponto2D 5 7
            resultado = pontoMedio p1 p1
    in resultado == p1



testePontoMedioDecimal::Bool
testePontoMedioDecimal =
    let p1 = Ponto2D 1.5 2.5
            p2 = Ponto2D 3.5 4.5
        resultado = pontoMedio p1 p2
        esperado = Ponto2D 2.5 3.5
    in resultado == esperado



testePontoMedioDistante::Bool
testePontoMedioDistante =
    let p1 = Ponto2D (-1000) (-2000)
            p2 = Ponto2D 1000 2000
        resultado = pontoMedio p1 p2
        esperado = Ponto2D 0 0
    in resultado == esperado 


-- Testes para calcularArea

testeAreaRetangulo::Bool
testeAreaRetangulo =
    let retangulo = Retangulo 5 3
            area = calcularArea retangulo
    in abs (area - 15.0) < 0.001



testeAreaCirculo::Bool
testeAreaCirculo =
    let circulo = Circulo 2
            area = calcularArea circulo
        esperado = pi * 4 -- π * r²
    in abs (area - esperado) < 0.001



testeAreaTriangulo::Bool
testeAreaTriangulo =
    let p1 = Ponto2D 0 0
            p2 = Ponto2D 4 0
        p3 = Ponto2D 0 3
        triangulo = Triangulo p1 p2 p3
        area = calcularArea triangulo
    in abs (area - 6.0) < 0.001 -- (base * altura) / 2



testeAreaCirculoUnitario::Bool
testeAreaCirculoUnitario =
    let circulo = Circulo 1
            area = calcularArea circulo
        esperado = pi
    in abs (area - esperado) < 0.001



testeAreaRetanguloQuadrado::Bool
testeAreaRetanguloQuadrado =
    let quadrado = Retangulo 4 4
            area = calcularArea quadrado
    in abs (area - 16.0) < 0.001 


-- Testes para calcularVolume

testeVolumeEsfera::Bool
testeVolumeEsfera =
    let esfera = Esfera 3
            volume = calcularVolume esfera
        esperado = (4/3) * pi * 27 -- (4/3) * π * r³
    in abs (volume- esperado) < 0.001



testeVolumeCilindro::Bool
testeVolumeCilindro =
    let cilindro = Cilindro 2 5
            volume = calcularVolume cilindro
        esperado = pi * 4 * 5 -- π * r² * h
    in abs (volume- esperado) < 0.001



testeVolumeParalelepipedo::Bool
testeVolumeParalelepipedo =
    let paralele = Paralelepipedo 2 3 4
            volume = calcularVolume paralele
    in abs (volume- 24.0) < 0.001 -- comprimento * largura * altura



testeVolumeEsferaUnitaria::Bool
testeVolumeEsferaUnitaria =
    let esfera = Esfera 1
            volume = calcularVolume esfera
        esperado = (4/3) * pi
    in abs (volume- esperado) < 0.001



testeVolumeCilindroUnitario::Bool
testeVolumeCilindroUnitario =
    let cilindro = Cilindro 1 1
            volume = calcularVolume cilindro
        esperado = pi
    in abs (volume- esperado) < 0.001 


-- Testes para calcularPerimetro

testePerimetroRetangulo::Bool
testePerimetroRetangulo =
    let retangulo = Retangulo 3 4
            perimetro = calcularPerimetro retangulo
    in abs (perimetro- 14.0) < 0.001 -- 2*(3+4)



testePerimetroCirculo::Bool
testePerimetroCirculo =
    let circulo = Circulo 3
            perimetro = calcularPerimetro circulo
        esperado = 2 * pi * 3in abs (perimetro- esperado) < 0.001



testePerimetroCirculoUnitario::Bool
testePerimetroCirculoUnitario =
    let circulo = Circulo 1
            perimetro = calcularPerimetro circulo
        esperado = 2 * pi
    in abs (perimetro- esperado) < 0.001



testePerimetroQuadrado::Bool
testePerimetroQuadrado =
    let quadrado = Retangulo 5 5
            perimetro = calcularPerimetro quadrado
    in abs (perimetro- 20.0) < 0.001 -- 4*5



testePerimetroTriangulo::Bool
testePerimetroTriangulo =
    let p1 = Ponto2D 0 0
            p2 = Ponto2D 3 0
        p3 = Ponto2D 0 4
        triangulo = Triangulo p1 p2 p3
        perimetro = calcularPerimetro triangulo
        esperado = 3 + 4 + 5 -- triângulo 3-4-5
    in abs (perimetro- esperado) < 0.001


    


-- Testes para dentroDoPoligono

testeDentroPoligonoQuadrado::Bool
testeDentroPoligonoQuadrado =
    let quadrado = [Ponto2D 0 0, Ponto2D 2 0, Ponto2D 2 2, Ponto2D 0 2]
            pontoDentro = Ponto2D 1 1
        pontoFora = Ponto2D 3 3
    in dentroDoPoligono pontoDentro quadrado && not (dentroDoPoligono pontoFora quadrado)



testeDentroPoligonoTriangulo::Bool
testeDentroPoligonoTriangulo =
    let triangulo = [Ponto2D 0 0, Ponto2D 4 0, Ponto2D 2 3]
            pontoDentro = Ponto2D 2 1
        pontoFora = Ponto2D 0 4
    in dentroDoPoligono pontoDentro triangulo && not (dentroDoPoligono pontoFora triangulo)



testeDentroPoligonoBorda::Bool
testeDentroPoligonoBorda =
    let quadrado = [Ponto2D 0 0, Ponto2D 2 0, Ponto2D 2 2, Ponto2D 0 2]
            pontoBorda = Ponto2D 1 0 -- na borda inferior
    in dentroDoPoligono pontoBorda quadrado -- assumindo que borda conta como dentro



testeDentroPoligonoVertice::Bool
testeDentroPoligonoVertice =
    let triangulo = [Ponto2D 0 0, Ponto2D 3 0, Ponto2D 1.5 2]
            pontoVertice = Ponto2D 0 0 -- no vértice
    in dentroDoPoligono pontoVertice triangulo


testeDentroPoligonoVazio::Bool
testeDentroPoligonoVazio =
    let poligonoVazio = []
            ponto = Ponto2D 1 1
    in not (dentroDoPoligono ponto poligonoVazio) 


-- Testes para intersecaoRetas

testeIntersecaoRetasBasico::Bool
testeIntersecaoRetasBasico =
    let reta1 = (Ponto2D 0 0, Ponto2D 2 2) -- y = x
            reta2 = (Ponto2D 0 2, Ponto2D 2 0) -- y =-x + 2
        resultado = intersecaoRetas reta1 reta2
        esperado = Just (Ponto2D 1 1)
    in resultado == esperado



testeIntersecaoRetasParalelas::Bool
testeIntersecaoRetasParalelas =
    let reta1 = (Ponto2D 0 0, Ponto2D 2 2) -- y = x
            reta2 = (Ponto2D 0 1, Ponto2D 2 3) -- y = x + 1
        resultado = intersecaoRetas reta1 reta2
    in resultado == Nothing -- retas paralelas não se intersectam



testeIntersecaoRetasCoincidentes::Bool
testeIntersecaoRetasCoincidentes =
    let reta1 = (Ponto2D 0 0, Ponto2D 2 2)
            reta2 = (Ponto2D 1 1, Ponto2D 3 3) -- mesma reta
        resultado = intersecaoRetas reta1 reta2
    in resultado == Nothing -- retas coincidentes têm infinitos pontos



testeIntersecaoRetasPerpendicualres::Bool
testeIntersecaoRetasPerpendicualres =
    let reta1 = (Ponto2D 0 1, Ponto2D 2 1) -- reta horizontal
            reta2 = (Ponto2D 1 0, Ponto2D 1 2) -- reta vertical
        resultado = intersecaoRetas reta1 reta2
        esperado = Just (Ponto2D 1 1)
    in resultado == esperado



testeIntersecaoRetasNegativas::Bool
testeIntersecaoRetasNegativas =
    let reta1 = (Ponto2D (-2) (-2), Ponto2D 0 0) -- y = x
            reta2 = (Ponto2D (-2) 2, Ponto2D 0 0)
        resultado = intersecaoRetas reta1 reta2
        esperado = Just (Ponto2D 0 0)
    in resultado == esperado

executarTestesGeometria :: IO ()
executarTestesGeometria = do
    putStrLn "======================================"
    putStrLn "TESTES GEOMETRIA"
    putStrLn "======================================"
    putStrLn "\n -- DISTÂNCIA 2D --"
    putStrLn $ "Básico: " ++ show testeDistancia2DBasico
    putStrLn $ "Mesmos pontos: " ++ show testeDistancia2DMesmosPontos
    putStrLn $ "Negativa: " ++ show testeDistancia2DNegativa
    putStrLn $ "Decimal: " ++ show testeDistancia2DDecimal
    putStrLn $ "Grande: " ++ show testeDistancia2DGrande

    putStrLn "\n -- DISTÂNCIA 3D --"
    putStrLn $ "Básico: " ++ show testeDistancia3DBasico
    putStrLn $ "Mesmos pontos: " ++ show testeDistancia3DMesmosPontos
    putStrLn $ "Completo: " ++ show testeDistancia3DCompleto
    putStrLn $ "Negativa: " ++ show testeDistancia3DNegativa
    putStrLn $ "Pequena: " ++ show testeDistancia3DPequena

    putStrLn "\n -- PONTO MÉDIO --"
    putStrLn $ "Básico: " ++ show testePontoMedioBasico
    putStrLn $ "Negativo: " ++ show testePontoMedioNegativo
    putStrLn $ "Mesmo ponto: " ++ show testePontoMedioMesmoPonto
    putStrLn $ "Decimal: " ++ show testePontoMedioDecimal
    putStrLn $ "Distante: " ++ show testePontoMedioDistante

    putStrLn "\n -- ÁREA --"
    putStrLn $ "Retângulo: " ++ show testeAreaRetangulo
    putStrLn $ "Círculo: " ++ show testeAreaCirculo
    putStrLn $ "Triângulo: " ++ show testeAreaTriangulo
    putStrLn $ "Círculo unitário: " ++ show testeAreaCirculoUnitario
    putStrLn $ "Quadrado: " ++ show testeAreaRetanguloQuadrado

    putStrLn "\n -- VOLUME --"
    putStrLn $ "Esfera: " ++ show testeVolumeEsfera
    putStrLn $ "Cilindro: " ++ show testeVolumeCilindro
    putStrLn $ "Paralelepípedo: " ++ show testeVolumeParalelepipedo
    putStrLn $ "Esfera unitária: " ++ show testeVolumeEsferaUnitaria
    putStrLn $ "Cilindro unitário: " ++ show testeVolumeCilindroUnitario

    putStrLn "\n -- PERÍMETRO --"
    putStrLn $ "Retângulo: " ++ show testePerimetroRetangulo
    putStrLn $ "Círculo: " ++ show testePerimetroCirculo
    putStrLn $ "Círculo unitário: " ++ show testePerimetroCirculoUnitario
    putStrLn $ "Quadrado: " ++ show testePerimetroQuadrado
    putStrLn $ "Triângulo: " ++ show testePerimetroTriangulo

    putStrLn "\n -- PONTO DENTRO DE POLÍGONO --"
    putStrLn $ "Quadrado: " ++ show testeDentroPoligonoQuadrado
    putStrLn $ "Triângulo: " ++ show testeDentroPoligonoTriangulo
    putStrLn $ "Na borda: " ++ show testeDentroPoligonoBorda
    putStrLn $ "No vértice: " ++ show testeDentroPoligonoVertice
    putStrLn $ "Polígono vazio: " ++ show testeDentroPoligonoVazio

    putStrLn "\n -- INTERSEÇÃO DE RETAS --"
    putStrLn $ "Básico: " ++ show testeIntersecaoRetasBasico
    putStrLn $ "Paralelas: " ++ show testeIntersecaoRetasParalelas
    putStrLn $ "Coincidentes: " ++ show testeIntersecaoRetasCoincidentes
    putStrLn $ "Perpendiculares: " ++ show testeIntersecaoRetasPerpendicualres
    putStrLn $ "Negativas: " ++ show testeIntersecaoRetasNegativas


testesGeometria :: [(String, Bool)]
testesGeometria = [
        -- Distância 2D (5 testes)
        ("Distância 2D Básico", testeDistancia2DBasico),
        ("Distância 2D Mesmos", testeDistancia2DMesmosPontos),
        ("Distância 2D Negativa", testeDistancia2DNegativa),
        ("Distância 2D Decimal", testeDistancia2DDecimal),
        ("Distância 2D Grande", testeDistancia2DGrande),

        -- Distância 3D (5 testes)
        ("Distância 3D Básico", testeDistancia3DBasico),
        ("Distância 3D Mesmos", testeDistancia3DMesmosPontos),
        ("Distância 3D Completo", testeDistancia3DCompleto),
        ("Distância 3D Negativa", testeDistancia3DNegativa),
        ("Distância 3D Pequena", testeDistancia3DPequena),

        -- Ponto Médio (5 testes)
        ("Ponto Médio Básico", testePontoMedioBasico),
        ("Ponto Médio Negativo", testePontoMedioNegativo),
        ("Ponto Médio Mesmo", testePontoMedioMesmoPonto),
        ("Ponto Médio Decimal", testePontoMedioDecimal),
        ("Ponto Médio Distante", testePontoMedioDistante),

        -- Área (5 testes)
        ("Área Retângulo", testeAreaRetangulo),
        ("Área Círculo", testeAreaCirculo),
        ("Área Triângulo", testeAreaTriangulo),
        ("Área Círculo Unitário", testeAreaCirculoUnitario),
        ("Área Quadrado", testeAreaRetanguloQuadrado),

        -- Volume (5 testes)
        ("Volume Esfera", testeVolumeEsfera),
        ("Volume Cilindro", testeVolumeCilindro),
        ("Volume Paralelepípedo", testeVolumeParalelepipedo),
        ("Volume Esfera Unitária", testeVolumeEsferaUnitaria),
        ("Volume Cilindro Unitário", testeVolumeCilindroUnitario),

        -- Perímetro (6 testes)
        ("Perímetro Retângulo", testePerimetroRetangulo),
        ("Perímetro Círculo", testePerimetroCirculo),
        ("Perímetro Círculo Unitário", testePerimetroCirculoUnitario),
        ("Perímetro Quadrado", testePerimetroQuadrado),
        ("Perímetro Triângulo", testePerimetroTriangulo),

        -- Dentro Polígono (5 testes)
        ("Dentro Polígono Quadrado", testeDentroPoligonoQuadrado),
        ("Dentro Polígono Triângulo", testeDentroPoligonoTriangulo),
        ("Dentro Polígono Borda", testeDentroPoligonoBorda),
        ("Dentro Polígono Vértice", testeDentroPoligonoVertice),
        ("Dentro Polígono Vazio", testeDentroPoligonoVazio),

        -- Interseção Retas (5 testes)
        ("Interseção Retas Básico", testeIntersecaoRetasBasico),
        ("Interseção Retas Paralelas", testeIntersecaoRetasParalelas),
        ("Interseção Retas Coincidentes", testeIntersecaoRetasCoincidentes),
        ("Interseção Retas Perpendiculares", testeIntersecaoRetasPerpendicualres),
        ("Interseção Retas Negativas", testeIntersecaoRetasNegativas)
    ]
