module Tests.CivilEngineeringTests where
import Types
import Engineering.Civil
import Data.List (sort)

-- =================================
-- TESTES PARA MOMENTO DE INÉRCIA RETANGULAR
-- =================================

-- Testes básicos



testeMomentoInerciaBasico::Bool
testeMomentoInerciaBasico =
    let base = 0.3 -- 30 cm
        altura = 0.5 -- 50 cm
        inercia = momentoInerciaRetangular base altura
        esperado = (0.3 * 0.5^3) / 12 -- I = bh³/12 = 0.003125 m⁴
    in abs (inercia- esperado) < 0.000001


testeMomentoInerciaQuadrado::Bool
testeMomentoInerciaQuadrado =
    let lado = 0.2 -- seção quadrada 20x20 cm
        inercia = momentoInerciaRetangular lado lado
        esperado = (0.2 * 0.2^3) / 12 -- I = 0.000013333 m⁴
    in abs (inercia- esperado) < 0.000001


testeMomentoInerciaVigaComum::Bool
testeMomentoInerciaVigaComum =
    let base = 0.20 -- 20 cm
        altura = 0.60 -- 60 cm (viga comum)
        inercia = momentoInerciaRetangular base altura
        esperado = (0.20 * 0.60^3) / 12 -- I = 0.0036 m⁴
    in abs (inercia- esperado) < 0.000001


testeMomentoInerciaPilar::Bool
testeMomentoInerciaPilar =
    let base = 0.40 -- 40 cm
        altura = 0.40 -- 40 cm (pilar quadrado)
        inercia = momentoInerciaRetangular base altura
        esperado = (0.40 * 0.40^3) / 12 -- I = 0.002133 m⁴
    in abs (inercia- esperado) < 0.000001


testeMomentoInerciaLaje::Bool
testeMomentoInerciaLaje =
    let base = 1.0 -- 100 cm (1 metro de largura)
        altura = 0.15 -- 15 cm de espessura
        inercia = momentoInerciaRetangular base altura
        esperado = (1.0 * 0.15^3) / 12 -- I = 0.00028125 m⁴
    in abs (inercia- esperado) < 0.000001 -- Testes com valores extremos


testeMomentoInerciaMinimo::Bool
testeMomentoInerciaMinimo =
    let base = 0.001 -- 1 mm
        altura = 0.001 -- 1 mm
        inercia = momentoInerciaRetangular base altura
        esperado = (0.001 * 0.001^3) / 12
    in abs (inercia- esperado) < 1e-15


testeMomentoInerciaMaximo::Bool
testeMomentoInerciaMaximo =
    let base = 10.0 -- 10 m (viga muito grande)
        altura = 2.0 -- 2 m
        inercia = momentoInerciaRetangular base altura
        esperado = (10.0 * 2.0^3) / 12 -- I = 6.667 m⁴
    in abs (inercia- esperado) < 0.001


testeMomentoInerciaProporção::Bool
testeMomentoInerciaProporção =
    let base1 = 0.2
        altura1 = 0.4
        inercia1 = momentoInerciaRetangular base1 altura1
        base2 = 0.4 -- dobrou a base
        altura2 = 0.8 -- dobrou a altura
        inercia2 = momentoInerciaRetangular base2 altura2 -- Inércia deve ser 16x maior (2 * 2³ = 16)
        razao = inercia2 / inercia1
    in abs (razao- 16.0) < 0.001


testeMomentoInerciaAlturaImportante::Bool
testeMomentoInerciaAlturaImportante =
    let base = 0.2
        altura1 = 0.3
        altura2 = 0.6 -- dobrou a altura
        inercia1 = momentoInerciaRetangular base altura1
        inercia2 = momentoInerciaRetangular base altura2 -- Inércia deve ser 8x maior (2³ = 8)
        razao = inercia2 / inercia1
    in abs (razao- 8.0) < 0.001


testeMomentoInerciaDecimal::Bool
testeMomentoInerciaDecimal =
    let base = 0.123
        altura = 0.456
        inercia = momentoInerciaRetangular base altura
        esperado = (0.123 * 0.456^3) / 12
    in abs (inercia- esperado) < 1e-12




testeTensaoNormalBasica::Bool
testeTensaoNormalBasica =
    let forca = 10000 -- 10 kN
        area = 0.04 -- 0.04 m² (20cm x 20cm)
        tensao = tensaoNormal forca area
        esperada = 250000 -- 250 kPa
    in abs (tensao- esperada) < 1.0


testeTensaoNormalCompressao::Bool
testeTensaoNormalCompressao =
    let forca = 50000 -- 50 kN (compressão em pilar)
        area = 0.16 -- 0.16 m² (40cm x 40cm)
        tensao = tensaoNormal forca area
        esperada = 312500 -- 312.5 kPa
    in abs (tensao- esperada) < 1.0


testeTensaoNormalTracao::Bool
testeTensaoNormalTracao =
    let forca = 15000 -- 15 kN (tração)
        area = 0.01 -- 0.01 m² (10cm x 10cm)
        tensao = tensaoNormal forca area
        esperada = 1500000 -- 1.5 MPa
    in abs (tensao- esperada) < 1.0


testeTensaoNormalAreaGrande::Bool
testeTensaoNormalAreaGrande =
    let forca = 100000 -- 100 kN
        area = 1.0 -- 1 m²
        tensao = tensaoNormal forca area
        esperada = 100000 -- 100 kPa
    in abs (tensao- esperada) < 1.0


testeTensaoNormalAreaPequena::Bool
testeTensaoNormalAreaPequena =
    let forca = 1000 -- 1 kN
        area = 0.0001 -- 1 cm²
        tensao = tensaoNormal forca area
        esperada = 10000000 -- 10 MPa
    in abs (tensao- esperada) < 1.0


testeTensaoNormalZeroForca::Bool
testeTensaoNormalZeroForca =
    let forca = 0 -- sem força
        area = 0.04 -- área qualquer
        tensao = tensaoNormal forca area
    in abs tensao < 0.001
        

testeTensaoNormalConcreto::Bool
testeTensaoNormalConcreto =
    let forca = 30000 -- 30 kN (limite concreto ~25-30 MPa)
        area = 0.001 -- 0.001 m² (provoque alta tensão)
        tensao = tensaoNormal forca area
        esperada = 30000000 -- 30 MPa
    in abs (tensao- esperada) < 1.0


testeTensaoNormalAco::Bool
testeTensaoNormalAco =
    let forca = 250000 -- 250 kN (aço ~250 MPa)
        area = 0.001 -- 0.001 m²
        tensao = tensaoNormal forca area
        esperada = 250000000 -- 250 MPa
    in abs (tensao- esperada) < 1.0


testeTensaoNormalPrecisao::Bool
testeTensaoNormalPrecisao =
    let forca = 12345.67
        area = 0.123456
        tensao = tensaoNormal forca area
        esperada = forca / area
    in abs (tensao- esperada) < 0.01


testeTensaoNormalUnidades::Bool
testeTensaoNormalUnidades =
    let forca = 1000000 -- 1000 kN = 1 MN
        area = 1.0 -- 1 m²
        tensao = tensaoNormal forca area
        esperada = 1000000 -- 1 MPa
    in abs (tensao- esperada) < 1.0




testeDeflexaoVigaBasica::Bool
testeDeflexaoVigaBasica =
    let carga = 5000 -- 5 kN
        comprimento = 4 -- 4 m
        modulo = 30e9 -- 30 GPa (concreto)
        inercia = 0.002 -- 0.002 m⁴
        deflexao = deflexaoViga carga comprimento modulo inercia
        esperada = (5000 * 4^3) / (48 * 30e9 * 0.002)

    -- δ = PL/48EI
    in abs (deflexao- esperada) < 0.0001


testeDeflexaoVigaAco::Bool
testeDeflexaoVigaAco =
    let carga = 10000 -- 10 kN
        comprimento = 6 -- 6 m
        modulo = 200e9 -- 200 GPa (aço)
        inercia = 0.001 -- 0.001 m⁴
        deflexao = deflexaoViga carga comprimento modulo inercia
        esperada = (10000 * 6^3) / (48 * 200e9 * 0.001)
    in abs (deflexao- esperada) < 0.0001


testeDeflexaoVigaMadeira::Bool
testeDeflexaoVigaMadeira =
    let carga = 2000 -- 2 kN
        comprimento = 3 -- 3 m
        modulo = 12e9 -- 12 GPa (madeira)
        inercia = 0.0005 -- 0.0005 m⁴
        deflexao = deflexaoViga carga comprimento modulo inercia
        esperada = (2000 * 3^3) / (48 * 12e9 * 0.0005)
    in abs (deflexao- esperada) < 0.0001


testeDeflexaoInfluenciaComprimento::Bool
testeDeflexaoInfluenciaComprimento =
    let carga = 5000
        modulo = 30e9
        inercia = 0.002
        comp1 = 2.0 -- 2 m
        comp2 = 4.0 -- 4 m (dobrou)
        def1 = deflexaoViga carga comp1 modulo inercia
        def2 = deflexaoViga carga comp2 modulo inercia -- Deflexão deve ser 8x maior (2³ = 8)
        razao = def2 / def1
    in abs (razao- 8.0) < 0.1


testeDeflexaoInfluenciaCarga::Bool
testeDeflexaoInfluenciaCarga =
    let comprimento = 4
        modulo = 30e9
        inercia = 0.002
        carga1 = 5000 -- 5 kN
        carga2 = 10000 -- 10 kN (dobrou)
        def1 = deflexaoViga carga1 comprimento modulo inercia
        def2 = deflexaoViga carga2 comprimento modulo inercia -- Deflexão deve dobrar
        razao = def2 / def1
    in abs (razao- 2.0) < 0.01


testeDeflexaoSemCarga::Bool
testeDeflexaoSemCarga =
    let carga = 0
        comprimento = 4
        modulo = 30e9
        inercia = 0.002
        deflexao = deflexaoViga carga comprimento modulo inercia
    in abs deflexao < 1e-10


testeDeflexaoVigaCurta::Bool
testeDeflexaoVigaCurta =
    let carga = 5000
        comprimento = 1 -- viga muito curta
        modulo = 30e9
        inercia = 0.002
        deflexao = deflexaoViga carga comprimento modulo inercia
        esperada = (5000 * 1^3) / (48 * 30e9 * 0.002)
    in abs (deflexao- esperada) < 1e-8


testeDeflexaoVigaLonga::Bool
testeDeflexaoVigaLonga =
    let carga = 5000
        comprimento = 20 -- viga longa
        modulo = 30e9
        inercia = 0.010 -- inércia maior para suportar
        deflexao = deflexaoViga carga comprimento modulo inercia
        esperada = (5000 * 20^3) / (48 * 30e9 * 0.010)
    in abs (deflexao- esperada) < 0.001


testeDeflexaoAltoModulo::Bool
testeDeflexaoAltoModulo =
    let carga = 5000
        comprimento = 4
        modulo = 500e9 -- material muito rígido
        inercia = 0.002
        deflexao = deflexaoViga carga comprimento modulo inercia
        esperada = (5000 * 4^3) / (48 * 500e9 * 0.002)
    in abs (deflexao- esperada) < 1e-8


testeDeflexaoAltaInercia::Bool
testeDeflexaoAltaInercia =
    let carga = 5000
        comprimento = 4
        modulo = 30e9
        inercia = 0.020 -- seção com alta inércia
        deflexao = deflexaoViga carga comprimento modulo inercia
        esperada = (5000 * 4^3) / (48 * 30e9 * 0.020)
    in abs (deflexao- esperada) < 1e-6




testeCargaCriticaBasica::Bool
testeCargaCriticaBasica =
    let modulo = 200e9 -- 200 GPa (aço)
        inercia = 0.001 -- 0.001 m⁴
        comprimento = 3 -- 3 m
        cargaCritica = cargaCriticaEuler modulo inercia comprimento
        esperada = (pi^2 * 200e9 * 0.001) / 3^2 -- Pcr = π2EI/L²
    in abs (cargaCritica- esperada) < 1000


testeCargaCriticaConcreto::Bool
testeCargaCriticaConcreto =
    let modulo = 30e9 -- 30 GPa (concreto)
        inercia = 0.002 -- 0.002 m⁴
        comprimento = 4 -- 4 m
        cargaCritica = cargaCriticaEuler modulo inercia comprimento
        esperada = (pi^2 * 30e9 * 0.002) / 4^2
    in abs (cargaCritica- esperada) < 1000


testeCargaCriticaMadeira::Bool
testeCargaCriticaMadeira =
    let modulo = 12e9 -- 12 GPa (madeira)
        inercia = 0.0005 -- 0.0005 m⁴
        comprimento = 2.5 -- 2.5 m
        cargaCritica = cargaCriticaEuler modulo inercia comprimento
        esperada = (pi^2 * 12e9 * 0.0005) / 2.5^2
    in abs (cargaCritica- esperada) < 1000


testeCargaCriticaInfluenciaComprimento::Bool
testeCargaCriticaInfluenciaComprimento =
    let modulo = 200e9
        inercia = 0.001
        comp1 = 2.0 -- 2 m
        comp2 = 4.0 -- 4 m (dobrou)
        carga1 = cargaCriticaEuler modulo inercia comp1
        carga2 = cargaCriticaEuler modulo inercia comp2 -- Carga crítica deve ser 1/4 (1/2² = 1/4)
        razao = carga2 / carga1
    in abs (razao- 0.25) < 0.01


testeCargaCriticaInfluenciaInercia::Bool
testeCargaCriticaInfluenciaInercia =
    let modulo = 200e9
        comprimento = 3
        inercia1 = 0.001 -- 0.001 m⁴
        inercia2 = 0.004 -- 0.004 m⁴ (4x maior)
        carga1 = cargaCriticaEuler modulo inercia1 comprimento
        carga2 = cargaCriticaEuler modulo inercia2 comprimento -- Carga crítica deve ser 4x maior
        razao = carga2 / carga1
    in abs (razao- 4.0) < 0.01

testeCargaCriticaPilarCurto::Bool
testeCargaCriticaPilarCurto =
    let modulo = 30e9 -- concreto
        inercia = 0.002 -- pilar robusto
        comprimento = 1 -- muito curto
        cargaCritica = cargaCriticaEuler modulo inercia comprimento
        esperada = (pi^2 * 30e9 * 0.002) / 1^2
    in abs (cargaCritica- esperada) < 10000


testeCargaCriticaPilarEsbelto::Bool
testeCargaCriticaPilarEsbelto =
    let modulo = 200e9 -- aço
        inercia = 0.0001 -- seção pequena
        comprimento = 10 -- muito longo
        cargaCritica = cargaCriticaEuler modulo inercia comprimento
        esperada = (pi^2 * 200e9 * 0.0001) / 10^2
    in abs (cargaCritica- esperada) < 1000


testeCargaCriticaValorGrande::Bool
testeCargaCriticaValorGrande =
    let modulo = 300e9 -- material muito rígido
        inercia = 0.010 -- seção muito robusta
        comprimento = 2 -- relativamente curto
        cargaCritica = cargaCriticaEuler modulo inercia comprimento
    in cargaCritica > 1e6 -- deve ser muito alta


testeCargaCriticaValorPequeno::Bool
testeCargaCriticaValorPequeno =
    let modulo = 1e9 -- material flexível
        inercia = 0.00001 -- seção muito pequena
        comprimento = 15 -- muito longo
        cargaCritica = cargaCriticaEuler modulo inercia comprimento
    in cargaCritica > 0 && cargaCritica < 1000 -- deve ser baixa


testeCargaCriticaPrecisao::Bool
testeCargaCriticaPrecisao =
    let modulo = 205.7e9
        inercia = 0.001234
        comprimento = 3.456
        cargaCritica = cargaCriticaEuler modulo inercia comprimento
        esperada = (pi^2 * 205.7e9 * 0.001234) / 3.456^2
    in abs (cargaCritica- esperada) < 1000




testeVolumeConcretoRetangulo::Bool
testeVolumeConcretoRetangulo =
    let viga = Paralelepipedo 10 0.30 0.50 -- 10m x 30cm x 50cm
        volume = volumeConcreto viga
        esperado = 10 * 0.30 * 0.50 -- 1.5 m³
    in abs (volume- esperado) < 0.001


testeVolumeConcretoLaje::Bool
testeVolumeConcretoLaje =
    let laje = Paralelepipedo 20 10 0.15
        volume = volumeConcreto laje
        esperado = 20 * 10 * 0.15 -- 30 m³
    in abs (volume- esperado) < 0.001


testeVolumeConcretoPilar::Bool
testeVolumeConcretoPilar =
    -- 20m x 10m x 15cm
    let pilar = Paralelepipedo 3.0 0.40 0.40 -- 3m x 40cm x 40cm
        volume = volumeConcreto pilar
        esperado = 3.0 * 0.40 * 0.40 -- 0.48 m³
    in abs (volume- esperado) < 0.001


testeVolumeConcretoFundacao::Bool
testeVolumeConcretoFundacao =
    let fundacao = Paralelepipedo 5 5 1.0
        volume = volumeConcreto fundacao
        esperado = 5 * 5 * 1.0 -- 25 m³
    in abs (volume- esperado) < 0.001


testeVolumeConcretoVigaLonga::Bool
testeVolumeConcretoVigaLonga =
    -- 5m x 5m x 1m
    let viga = Paralelepipedo 50 0.20 0.60 -- viga longa
        volume = volumeConcreto viga
        esperado = 50 * 0.20 * 0.60 -- 6 m³
    in abs (volume- esperado) < 0.001


testeVolumeConcretoPequeno::Bool
testeVolumeConcretoPequeno =
    let elemento = Paralelepipedo 0.1 0.1 0.1 -- 10cm cúbicos
        volume = volumeConcreto elemento
        esperado = 0.1 * 0.1 * 0.1 -- 0.001 m³
    in abs (volume- esperado) < 0.000001


testeVolumeConcreteGrande::Bool
testeVolumeConcreteGrande =
    let estrutura = Paralelepipedo 100 20 5
        volume = volumeConcreto estrutura
        esperado = 100 * 20 * 5 -- 10000 m³
    in abs (volume- esperado) < 1.0


testeVolumeConcretoDecimal::Bool
testeVolumeConcretoDecimal =
    -- grande estrutura
    let viga = Paralelepipedo 12.345 0.678 0.432
        volume = volumeConcreto viga
        esperado = 12.345 * 0.678 * 0.432
    in abs (volume- esperado) < 0.001


testeVolumeConcretoUnidades::Bool
testeVolumeConcretoUnidades =
    let -- Teste com dimensões típicas de construção
        viga = Paralelepipedo 8.0 0.25 0.55 -- 8m x 25cm x 55cm
        volume = volumeConcreto viga
        esperado = 8.0 * 0.25 * 0.55 -- 1.1 m³
    in abs (volume- esperado) < 0.001


testeVolumeConcretoZero::Bool
testeVolumeConcretoZero =
    let elemento = Paralelepipedo 0 1 1 -- uma dimensão zero
        volume = volumeConcreto elemento
    in abs volume < 0.001 -- deve ser zero




testeCasoRealVigaSimples::Bool
testeCasoRealVigaSimples =
    -- Viga de concreto armado típica: 20cm x 60cm, vão 6m, carga 15 kN
    let base = 0.20
        altura = 0.60
        vao = 6.0
        carga = 15000
        moduloConcreto = 25e9 -- 25 GPa -- Cálculos
        inercia = momentoInerciaRetangular base altura
        area = base * altura
        tensao = tensaoNormal carga area
        deflexao = deflexaoViga carga vao moduloConcreto inercia
        volume = volumeConcreto (Paralelepipedo vao base altura)

    -- Verificações
        inerciaOk = inercia > 0.003 && inercia < 0.004 -- ~0.0036 m⁴
        tensaoOk = tensao > 100000 && tensao < 200000 -- ~125 kPa
        deflexaoOk = deflexao > 0 && deflexao < 0.050 -- < 5 cm (L/120)
        volumeOk = abs (volume- 0.72) < 0.01
    in inerciaOk && tensaoOk && deflexaoOk && volumeOk


testeCasoRealPilarConcreto::Bool
testeCasoRealPilarConcreto =
    -- 0.72 m³-- Pilar quadrado 40x40 cm, altura 3m, carga 800 kN
    let lado = 0.40
        altura = 3.0
        carga = 800000
        moduloConcreto = 30e9 -- Cálculos
        inercia = momentoInerciaRetangular lado lado
        area = lado * lado
        tensao = tensaoNormal carga area
        cargaCritica = cargaCriticaEuler moduloConcreto inercia altura
        volume = volumeConcreto (Paralelepipedo altura lado lado)

    -- Verificações
        inerciaOk = inercia > 0.002 && inercia < 0.003
        tensaoOk = tensao > 4000000 && tensao < 6000000
        cargaCriticaOk = cargaCritica > carga * 3
        volumeOk = abs (volume- 0.48) < 0.01
    in inerciaOk && tensaoOk && cargaCriticaOk && volumeOk


testeCasoRealLajeResidencial::Bool
testeCasoRealLajeResidencial =
    -- ~0.00213 m⁴-- ~5 MPa -- fator segurança > 3 -- 0.48 m³-- Laje residencial: 4m x 6m x 12cm, carga distribuída
    let comprimento = 4.0
        largura = 6.0
        espessura = 0.12
        cargaPorM2 = 5000 -- 5 kN/m² (carga típica)
        cargaTotal = cargaPorM2 * comprimento * largura
        moduloConcreto = 25e9 -- Para laje, considera faixa unitária para cálculo
        inerciaFaixa = momentoInerciaRetangular 1.0 espessura
        deflexao = deflexaoViga cargaTotal comprimento moduloConcreto (inerciaFaixa * largura)
        volume = volumeConcreto (Paralelepipedo comprimento largura espessura)

    -- Verificações
        volumeOk = abs (volume- 2.88) < 0.01
        deflexaoOk = deflexao < comprimento / 250
    in volumeOk && deflexaoOk


testeCasoRealVigaAco::Bool
testeCasoRealVigaAco =
    -- 2.88 m³-- deflexão < L/250 -- Viga de aço: perfil equivalente a 30cm x 60cm, vão 12m
    let base = 0.30
        altura = 0.60
        vao = 12.0
        carga = 50000 -- 50 kN
        moduloAco = 200e9 -- Cálculos
        inercia = momentoInerciaRetangular base altura -- aproximação
        area = base * altura
        tensao = tensaoNormal carga area
        deflexao = deflexaoViga carga vao moduloAco inercia -- Verificações para aço
        tensaoOk = tensao < 150e6 -- < 150 MPa (limite conservador)
        deflexaoOk = deflexao < vao/300 -- < L/300 para aço
    in tensaoOk && deflexaoOk


testeCasoRealFundacao::Bool
testeCasoRealFundacao =
    -- Sapata de fundação: 3m x 3m x 0.8m
    let lado = 3.0
        altura = 0.8
        cargaTotal = 2000000 -- 2000 kN (carga do edifício),

    -- Cálculos
        area = lado * lado
        tensao = tensaoNormal cargaTotal area -- tensão no solo
        volume = volumeConcreto (Paralelepipedo lado lado altura)

    -- Verificações
        tensaoSoloOk = tensao < 400000 -- < 400 kPa (solo típico)
        volumeOk = abs (volume- 7.2) < 0.1 -- 7.2 m³
    in tensaoSoloOk && volumeOk


executarTestesCivilEngineering::IO ()
executarTestesCivilEngineering = do
    putStrLn "======================================"
    putStrLn "TESTES ENGENHARIA CIVIL"
    putStrLn "======================================"
    putStrLn "\n -- TESTES MOMENTO DE INÉRCIA --"
    putStrLn $ "Básico: " ++ show testeMomentoInerciaBasico
    putStrLn $ "Quadrado: " ++ show testeMomentoInerciaQuadrado
    putStrLn $ "Viga comum: " ++ show testeMomentoInerciaVigaComum
    putStrLn $ "Pilar: " ++ show testeMomentoInerciaPilar
    putStrLn $ "Laje: " ++ show testeMomentoInerciaLaje
    putStrLn $ "Mínimo: " ++ show testeMomentoInerciaMinimo
    putStrLn $ "Máximo: " ++ show testeMomentoInerciaMaximo
    putStrLn $ "Proporção: " ++ show testeMomentoInerciaProporção
    putStrLn $ "Altura importante: " ++ show testeMomentoInerciaAlturaImportante
    putStrLn $ "Decimal: " ++ show testeMomentoInerciaDecimal
    putStrLn "\n -- TESTES TENSÃO NORMAL --"
    putStrLn $ "Básica: " ++ show testeTensaoNormalBasica
    putStrLn $ "Compressão: " ++ show testeTensaoNormalCompressao
    putStrLn $ "Tração: " ++ show testeTensaoNormalTracao
    putStrLn $ "Área grande: " ++ show testeTensaoNormalAreaGrande
    putStrLn $ "Área pequena: " ++ show testeTensaoNormalAreaPequena
    putStrLn $ "Zero força: " ++ show testeTensaoNormalZeroForca
    putStrLn $ "Concreto: " ++ show testeTensaoNormalConcreto
    putStrLn $ "Aço: " ++ show testeTensaoNormalAco
    putStrLn $ "Precisão: " ++ show testeTensaoNormalPrecisao
    putStrLn $ "Unidades: " ++ show testeTensaoNormalUnidades
    putStrLn "\n -- TESTES DEFLEXÃO VIGA --"
    putStrLn $ "Básica: " ++ show testeDeflexaoVigaBasica
    putStrLn $ "Aço: " ++ show testeDeflexaoVigaAco
    putStrLn $ "Madeira: " ++ show testeDeflexaoVigaMadeira
    putStrLn $ "Influência comprimento: " ++ show testeDeflexaoInfluenciaComprimento
    putStrLn $ "Influência carga: " ++ show testeDeflexaoInfluenciaCarga
    putStrLn $ "Sem carga: " ++ show testeDeflexaoSemCarga
    putStrLn $ "Viga curta: " ++ show testeDeflexaoVigaCurta
    putStrLn $ "Viga longa: " ++ show testeDeflexaoVigaLonga
    putStrLn $ "Alto módulo: " ++ show testeDeflexaoAltoModulo
    putStrLn $ "Alta inércia: " ++ show testeDeflexaoAltaInercia
    putStrLn "\n -- TESTES CARGA CRÍTICA EULER --"
    putStrLn $ "Básica: " ++ show testeCargaCriticaBasica
    putStrLn $ "Concreto: " ++ show testeCargaCriticaConcreto
    putStrLn $ "Madeira: " ++ show testeCargaCriticaMadeira
    putStrLn $ "Influência comprimento: " ++ show testeCargaCriticaInfluenciaComprimento
    putStrLn $ "Influência inércia: " ++ show testeCargaCriticaInfluenciaInercia
    putStrLn $ "Pilar curto: " ++ show testeCargaCriticaPilarCurto
    putStrLn $ "Pilar esbelto: " ++ show testeCargaCriticaPilarEsbelto
    putStrLn $ "Valor grande: " ++ show testeCargaCriticaValorGrande
    putStrLn $ "Valor pequeno: " ++ show testeCargaCriticaValorPequeno
    putStrLn $ "Precisão: " ++ show testeCargaCriticaPrecisao
    putStrLn "\n -- TESTES VOLUME CONCRETO --"
    putStrLn $ "Retângulo: " ++ show testeVolumeConcretoRetangulo
    putStrLn $ "Laje: " ++ show testeVolumeConcretoLaje
    putStrLn $ "Pilar: " ++ show testeVolumeConcretoPilar
    putStrLn $ "Fundação: " ++ show testeVolumeConcretoFundacao
    putStrLn $ "Viga longa: " ++ show testeVolumeConcretoVigaLonga
    putStrLn $ "Pequeno: " ++ show testeVolumeConcretoPequeno
    putStrLn $ "Grande: " ++ show testeVolumeConcreteGrande
    putStrLn $ "Decimal: " ++ show testeVolumeConcretoDecimal
    putStrLn $ "Unidades: " ++ show testeVolumeConcretoUnidades
    putStrLn $ "Zero: " ++ show testeVolumeConcretoZero
    putStrLn "\n -- TESTES CASOS REAIS --"
    putStrLn $ "Viga simples: " ++ show testeCasoRealVigaSimples
    putStrLn $ "Pilar concreto: " ++ show testeCasoRealPilarConcreto
    putStrLn $ "Laje residencial: " ++ show testeCasoRealLajeResidencial
    putStrLn $ "Viga aço: " ++ show testeCasoRealVigaAco
    putStrLn $ "Fundação: " ++ show testeCasoRealFundacao


testesCivilEngineering::[(String, Bool)]
testesCivilEngineering = [
        -- Momento de Inércia (10 testes)
        ("Momento Inércia Básico", testeMomentoInerciaBasico),
        ("Momento Inércia Quadrado", testeMomentoInerciaQuadrado),
        ("Momento Inércia Viga Comum", testeMomentoInerciaVigaComum),
        ("Momento Inércia Pilar", testeMomentoInerciaPilar),
        ("Momento Inércia Laje", testeMomentoInerciaLaje),
        ("Momento Inércia Mínimo", testeMomentoInerciaMinimo),
        ("Momento Inércia Máximo", testeMomentoInerciaMaximo),
        ("Momento Inércia Proporção", testeMomentoInerciaProporção),
        ("Momento Inércia Altura Importante", testeMomentoInerciaAlturaImportante),
        ("Momento Inércia Decimal", testeMomentoInerciaDecimal),

        -- Tensão Normal (10 testes)
        ("Tensão Normal Básica", testeTensaoNormalBasica),
        ("Tensão Normal Compressão", testeTensaoNormalCompressao),
        ("Tensão Normal Tração", testeTensaoNormalTracao),
        ("Tensão Normal Área Grande", testeTensaoNormalAreaGrande),
        ("Tensão Normal Área Pequena", testeTensaoNormalAreaPequena),
        ("Tensão Normal Zero Força", testeTensaoNormalZeroForca),
        ("Tensão Normal Concreto", testeTensaoNormalConcreto),
        ("Tensão Normal Aço", testeTensaoNormalAco),
        ("Tensão Normal Precisão", testeTensaoNormalPrecisao),
        ("Tensão Normal Unidades", testeTensaoNormalUnidades),

        -- Deflexão Viga (10 testes)
        ("Deflexão Viga Básica", testeDeflexaoVigaBasica),
        ("Deflexão Viga Aço", testeDeflexaoVigaAco),
        ("Deflexão Viga Madeira", testeDeflexaoVigaMadeira),
        ("Deflexão Influência Comprimento", testeDeflexaoInfluenciaComprimento),
        ("Deflexão Influência Carga", testeDeflexaoInfluenciaCarga),
        ("Deflexão Sem Carga", testeDeflexaoSemCarga),
        ("Deflexão Viga Curta", testeDeflexaoVigaCurta),
        ("Deflexão Viga Longa", testeDeflexaoVigaLonga),
        ("Deflexão Alto Módulo", testeDeflexaoAltoModulo),
        ("Deflexão Alta Inércia", testeDeflexaoAltaInercia),

        -- Carga Crítica Euler (10 testes)
        ("Carga Crítica Básica", testeCargaCriticaBasica),
        ("Carga Crítica Concreto", testeCargaCriticaConcreto),
        ("Carga Crítica Madeira", testeCargaCriticaMadeira),
        ("Carga Crítica Influência Comprimento", testeCargaCriticaInfluenciaComprimento),
        ("Carga Crítica Influência Inércia", testeCargaCriticaInfluenciaInercia),
        ("Carga Crítica Pilar Curto", testeCargaCriticaPilarCurto),
        ("Carga Crítica Pilar Esbelto", testeCargaCriticaPilarEsbelto),
        ("Carga Crítica Valor Grande", testeCargaCriticaValorGrande),
        ("Carga Crítica Valor Pequeno", testeCargaCriticaValorPequeno),
        ("Carga Crítica Precisão", testeCargaCriticaPrecisao),

        -- Volume Concreto (10 testes)
        ("Volume Concreto Retângulo", testeVolumeConcretoRetangulo),
        ("Volume Concreto Laje", testeVolumeConcretoLaje),
        ("Volume Concreto Pilar", testeVolumeConcretoPilar),
        ("Volume Concreto Fundação", testeVolumeConcretoFundacao),
        ("Volume Concreto Viga Longa", testeVolumeConcretoVigaLonga),
        ("Volume Concreto Pequeno", testeVolumeConcretoPequeno),
        ("Volume Concreto Grande", testeVolumeConcreteGrande),
        ("Volume Concreto Decimal", testeVolumeConcretoDecimal),
        ("Volume Concreto Unidades", testeVolumeConcretoUnidades),
        ("Volume Concreto Zero", testeVolumeConcretoZero),

        -- Casos Reais (5 testes)
        ("Caso Real Viga Simples", testeCasoRealVigaSimples),
        ("Caso Real Pilar Concreto", testeCasoRealPilarConcreto),
        ("Caso Real Laje Residencial", testeCasoRealLajeResidencial),
        ("Caso Real Viga Aço", testeCasoRealVigaAco),
        ("Caso Real Fundação", testeCasoRealFundacao)
    ]

