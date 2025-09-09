module Tests.ElectricalEngineeringTests where
import Types
import Engineering.Electrical

-- =================================
-- TESTES PARA LEI DE OHM (TENSãO)
-- =================================

-- Testes básicos



testeLeiOhmBasica::Bool
testeLeiOhmBasica =
    let corrente = 2 -- 2 A
        resistencia = 10 -- 10 Ω
        tensao = tensaoOhm corrente resistencia
    in abs (tensao- 20) < 0.001 -- V = I × R = 20V


testeLeiOhmLampada::Bool
testeLeiOhmLampada =
    -- Lâmpada incandescente típica: 0.5A em 240Ω
    let corrente = 0.5
        resistencia = 240
        tensao = tensaoOhm corrente resistencia
        esperada = 120 -- 120V (tensão doméstica)
    in abs (tensao- esperada) < 0.001



testeLeiOhmResistorEletronico::Bool
testeLeiOhmResistorEletronico =
    -- Resistor de 1kΩ com 5mA
    let corrente = 0.005 -- 5 mA
        resistencia = 1000 -- 1 kΩ
        tensao = tensaoOhm corrente resistencia
        esperada = 5 -- 5V
    in abs (tensao- esperada) < 0.001



testeLeiOhmMotorEletrico::Bool
testeLeiOhmMotorEletrico =
    -- Motor elétrico: 15A, resistência interna 2Ω
    let corrente = 15
        resistencia = 2
        tensao = tensaoOhm corrente resistencia
        esperada = 30 -- 30V (queda de tensão interna)
    in abs (tensao- esperada) < 0.001



testeLeiOhmCorrenteZero::Bool
testeLeiOhmCorrenteZero =
    let corrente = 0 -- sem corrente
        resistencia = 100
        tensao = tensaoOhm corrente resistencia
    in abs tensao < 0.001



testeLeiOhmCorrenteAlta::Bool
testeLeiOhmCorrenteAlta =
    -- Soldador elétrico: 50A, 0.1Ω
    let corrente = 50
        resistencia = 0.1
        tensao = tensaoOhm corrente resistencia
        esperada = 5 -- 5V
    in abs (tensao- esperada) < 0.001



testeLeiOhmMicroeletronica::Bool
testeLeiOhmMicroeletronica =
    -- Circuito integrado: 1µA, 1MΩ
    let corrente = 1e-6 -- 1 µA
        resistencia = 1e6 -- 1 MΩ
        tensao = tensaoOhm corrente resistencia
        esperada = 1 -- 1V
    in abs (tensao- esperada) < 0.001



testeLeiOhmTransmissao::Bool
testeLeiOhmTransmissao =
    -- Linha de transmissão: 1000A, 0.01Ω
    let corrente = 1000
        resistencia = 0.01
        tensao = tensaoOhm corrente resistencia
        esperada = 10 -- 10V (perda na linha)
    in abs (tensao- esperada) < 0.001



testeLeiOhmDecimal::Bool
testeLeiOhmDecimal =
    let corrente = 2.5
        resistencia = 4.7
        tensao = tensaoOhm corrente resistencia
        esperada = 11.75 -- 2.5 × 4.7
    in abs (tensao- esperada) < 0.001


testeLeiOhmPrecisao::Bool
testeLeiOhmPrecisao =
    let corrente = 1.2345
        resistencia = 6.789
        tensao = tensaoOhm corrente resistencia
        esperada = corrente * resistencia
    in abs (tensao- esperada) < 0.001

-- =================================
-- TESTES PARA POTêNCIA ELéTRICA (V×I)
-- =================================



testePotenciaVIBasica::Bool
testePotenciaVIBasica =
    let tensao = 12 -- 12 V
        corrente = 2 -- 2 A
        potencia = potenciaEletricaVI tensao corrente
    in abs (potencia- 24) < 0.001 -- P = V × I = 24W


testePotenciaVILED::Bool
testePotenciaVILED =
    -- LED de alta potência: 3.3V, 1A
    let tensao = 3.3
        corrente = 1.0
        potencia = potenciaEletricaVI tensao correnteesperada = 3.3 -- 3.3W
    in abs (potencia- esperada) < 0.001


testePotenciaVIChuveiro::Bool
testePotenciaVIChuveiro =
    -- Chuveiro elétrico: 220V, 25A
    let tensao = 220
        corrente = 25
        potencia = potenciaEletricaVI tensao corrente
        esperada = 5500 -- 5.5 kW
    in abs (potencia- esperada) < 1.0


testePotenciaVIMicroprocessador::Bool
testePotenciaVIMicroprocessador =
    -- CPU: 1.2V, 50A (alta corrente, baixa tensão)
    let tensao = 1.2
        corrente = 50
        potencia = potenciaEletricaVI tensao corrente
        esperada = 60 -- 60W
    in abs (potencia- esperada) < 0.1


testePotenciaVICarregadorCelular::Bool
testePotenciaVICarregadorCelular =
    -- Carregador: 5V, 2A
    let tensao = 5
        corrente = 2
        potencia = potenciaEletricaVI tensao corrente
        esperada = 10 -- 10W
    in abs (potencia- esperada) < 0.001


testePotenciaVIZero::Bool
testePotenciaVIZero =
    let tensao = 10
        corrente = 0 -- sem corrente
        potencia = potenciaEletricaVI tensao corrente
    in abs potencia < 0.001


testePotenciaVIAltaTensao::Bool
testePotenciaVIAltaTensao =
    -- Sistema industrial: 13.8kV, 100A
    let tensao = 13800
        corrente = 100
        potencia = potenciaEletricaVI tensao corrente
        esperada = 1380000 -- 1.38 MW
    in abs (potencia- esperada) < 1000


testePotenciaVIBaixaPotencia::Bool
testePotenciaVIBaixaPotencia =
    -- Sensor: 3V, 1mA
    let tensao = 3
        corrente = 0.001 -- 1 mA
        potencia = potenciaEletricaVI tensao correnteesperada = 0.003 -- 3 mW
    in abs (potencia- esperada) < 0.0001


testePotenciaVIDecimal::Bool
testePotenciaVIDecimal =
    let tensao = 12.5
        corrente = 3.2
        potencia = potenciaEletricaVI tensao corrente
        esperada = 40.0 -- 12.5 × 3.2
    in abs (potencia- esperada) < 0.01


testePotenciaVIDispositivo9V::Bool
testePotenciaVIDispositivo9V =
    -- Exemplo do web search: 18W, 9V → 2A
    let tensao = 9
        corrente = 2 -- corrente calculada
        potencia = potenciaEletricaVI tensao corrente
        esperada = 18 -- 18W
    in abs (potencia- esperada) < 0.001




testePotenciaRIBasica::Bool
testePotenciaRIBasica =
    let resistencia = 10 -- 10 Ω
        corrente = 3 -- 3 A
        potencia = potenciaEletricaRI resistencia corrente
        esperada = 90 -- P = R × I² = 90W
    in abs (potencia- esperada) < 0.001


testePotenciaRIResistorAquecimento::Bool
testePotenciaRIResistorAquecimento =
    -- Resistor de aquecimento: 5Ω, 10A
    let resistencia = 5
        corrente = 10
        potencia = potenciaEletricaRI resistencia corrente
        esperada = 500 -- 500W
    in abs (potencia- esperada) < 1.0


testePotenciaRIFilamentoLampada::Bool
testePotenciaRIFilamentoLampada =
    -- Filamento de lâmpada: 240Ω, 0.5A
    let resistencia = 240
        corrente = 0.5
        potencia = potenciaEletricaRI resistencia corrente
        esperada = 60 -- 60W
    in abs (potencia- esperada) < 0.1


testePotenciaRIFioTransmissao::Bool
testePotenciaRIFioTransmissao =
    -- Perda em fio: 0.1Ω, 20A
    let resistencia = 0.1
        corrente = 20
        potencia = potenciaEletricaRI resistencia corrente
        esperada = 40 -- 40W (perda)
    in abs (potencia- esperada) < 0.1


testePotenciaRICorrenteZero::Bool
testePotenciaRICorrenteZero =
    let resistencia = 100
        corrente = 0 -- sem corrente
        potencia = potenciaEletricaRI resistencia corrente
    in abs potencia < 0.001


testePotenciaRIBaixaResistencia::Bool
testePotenciaRIBaixaResistencia =
    -- Fusível: 0.001Ω, 10A
    let resistencia = 0.001
        corrente = 10
        potencia = potenciaEletricaRI resistencia corrente
        esperada = 0.1 -- 0.1W
    in abs (potencia- esperada) < 0.001


testePotenciaRIAltaResistencia::Bool
testePotenciaRIAltaResistencia =
    -- Resistor de alta impedância: 1MΩ, 1mA
    let resistencia = 1e6
        corrente = 1e-3
        potencia = potenciaEletricaRI resistencia corrente
        esperada = 1 -- 1W
    in abs (potencia- esperada) < 0.01


testePotenciaRIDecimal::Bool
testePotenciaRIDecimal =
    let resistencia = 4.7
        corrente = 2.1
        potencia = potenciaEletricaRI resistencia corrente
        esperada = 4.7 * 2.1^2 -- 4.7 × 4.41 = 20.727
    in abs (potencia- esperada) < 0.01


testePotenciaRITorradeira::Bool
testePotenciaRITorradeira =
    -- Torradeira: 10Ω, 5A (exemplo do web search)
    let resistencia = 10
        corrente = 5
        potencia = potenciaEletricaRI resistencia corrente
        esperada = 250 -- 250W
    in abs (potencia- esperada) < 1.0


testePotenciaRIFerroSolda::Bool
testePotenciaRIFerroSolda =
    -- Ferro de solda: 20Ω, 2Alet resistencia = 20
    corrente = 2
    potencia = potenciaEletricaRI resistencia corrente
    esperada = 80 -- 80W
    in abs (potencia- esperada) < 0.1




testePotenciaVRBasica::Bool
testePotenciaVRBasica =
    let tensao = 20 -- 20 V
        resistencia = 4 -- 4 Ω
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 100 -- P = V²/R = 100W
    in abs (potencia- esperada) < 0.001


testePotenciaVRLampadaDomestica::Bool
testePotenciaVRLampadaDomestica =
    -- Lâmpada 60W, 120V
    let tensao = 120 -- R = V²/P → R = 120²/60 = 240Ω
        resistencia = 240
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 60 -- 60W
    in abs (potencia- esperada) < 0.1


testePotenciaVRAquecedor::Bool
testePotenciaVRAquecedor =
    -- Aquecedor elétrico: 220V, 4.84Ω
    let tensao = 220
        resistencia = 4.84
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 220^2 / 4.84 -- ~10kW
    in abs (potencia- esperada) < 100


testePotenciaVRCircuitoEletronico::Bool
testePotenciaVRCircuitoEletronico =
    -- Circuito 5V: resistor de 100Ω
    let tensao = 5
        resistencia = 100
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 0.25 -- 0.25W
    in abs (potencia- esperada) < 0.001


testePotenciaVRTensaoZero::Bool
testePotenciaVRTensaoZero =
    let tensao = 0 -- sem tensão
        resistencia = 50
        potencia = potenciaEletricaVR tensao resistencia
    in abs potencia < 0.

001testePotenciaVRAltaTensao::Bool
    testePotenciaVRAltaTensao =
    -- Sistema de potência: 13.8kV, 1000Ω
    let tensao = 13800
        resistencia = 1000
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 190440 -- ~190kW
    in abs (potencia- esperada) < 1000


testePotenciaVRResistorPequeno::Bool
testePotenciaVRResistorPequeno =
    -- Resistor SMD: 3.3V, 1kΩ
    let tensao = 3.3
        resistencia = 1000
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 0.01089 -- ~10.9mW
    in abs (potencia- esperada) < 0.001


testePotenciaVRDecimal::Bool
testePotenciaVRDecimal =
    let tensao = 15.5
        resistencia = 7.8
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 15.5^2 / 7.8
    in abs (potencia- esperada) < 0.01


testePotenciaVRSecadorCabelo::Bool
testePotenciaVRSecadorCabelo =
    -- Secador: 220V, 20Ω (exemplo baseado no web search)
    let tensao = 220
        resistencia = 20
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 2420 -- 2.42kW
    in abs (potencia- esperada) < 10


testePotenciaVRForno::Bool
testePotenciaVRForno =
    -- Forno elétrico: 240V, 19.2Ω
    let tensao = 240
        resistencia = 19.2
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 3000 -- 3kW
    in abs (potencia- esperada) < 50




testeResistenciaSerieBasica::Bool
testeResistenciaSerieBasica =
    let resistencias = [10, 20, 30] -- 10Ω, 20Ω, 30Ωtotal = resistenciaSerie resistencias
    in abs (total- 60) < 0.001 -- Rtotal = 60Ω


testeResistenciaSerieUmResistor::Bool
testeResistenciaSerieUmResistor =
    let resistencias = [47] -- um resistor de 47Ω
        total = resistenciaSerie resistencias
    in abs (total- 47) < 0.001


testeResistenciaSerieVazia::Bool
testeResistenciaSerieVazia =
    let resistencias = [] -- lista vazia
        total = resistenciaSerie resistencias
    in abs total < 0.001 -- deve ser zero


testeResistenciaSerieCircuitoLED::Bool
testeResistenciaSerieCircuitoLED =
    -- LED + resistor limitador: 220Ω + 100Ω
    let resistencias = [220, 100]
        total = resistenciaSerie resistencias
        esperada = 320 -- 320Ω
    in abs (total- esperada) < 0.001


testeResistenciaSerieDivisorTensao::Bool
testeResistenciaSerieDivisorTensao =
    -- Divisor de tensão: 1kΩ + 2kΩ + 3kΩ
    let resistencias = [1000, 2000, 3000]
        total = resistenciaSerie resistencias
        esperada = 6000 -- 6kΩ
    in abs (total- esperada) < 0.001


testeResistenciaSerieAltosValores::Bool
testeResistenciaSerieAltosValores =
    -- Resistores de alta impedância: 1MΩ + 2MΩ
    let resistencias = [1e6, 2e6]
        total = resistenciaSerie resistencias
        esperada = 3e6 -- 3MΩ
    in abs (total- esperada) < 1000


testeResistenciaSerieBaixosValores::Bool
testeResistenciaSerieBaixosValores =
    -- Resistores de baixa impedância: 0.1Ω + 0.2Ω + 0.3Ω
    let resistencias = [0.1, 0.2, 0.3]
        total = resistenciaSerie resistencias
        esperada = 0.6 -- 0.6Ω
    in abs (total- esperada) < 0.001


testeResistenciaSerieDecimais::Bool
testeResistenciaSerieDecimais =
    let resistencias = [4.7, 2.2, 1.8, 6.8]
        total = resistenciaSerie resistencias
        esperada = 15.5 -- soma diretain abs (total- esperada) < 0.01


testeResistenciaSerieMuitosResistores::Bool
testeResistenciaSerieMuitosResistores =
    -- 10 resistores de 100Ω cada
    let resistencias = replicate 10 100
        total = resistenciaSerie resistencias
        esperada = 1000 -- 1kΩ
    in abs (total- esperada) < 0.1


testeResistenciaSerieValoresVariados::Bool
testeResistenciaSerieValoresVariados =
    let resistencias = [0.47, 1.0, 2.2, 4.7, 10, 22, 47, 100]
        total = resistenciaSerie resistencias
        esperada = sum resistencias
    in abs (total- esperada) < 0.01




testeResistenciaParaleloBasica::Bool
testeResistenciaParaleloBasica =
    let resistencias = [20, 30] -- 20Ω, 30Ω em paralelo
        total = resistenciaParalelo resistencias
        esperada = (20 * 30) / (20 + 30)

    -- 12Ω
    in abs (total- esperada) < 0.001


testeResistenciaParaleloIguais::Bool
testeResistenciaParaleloIguais =
    -- Dois resistores iguais: 100Ω || 100Ω
    let resistencias = [100, 100]
        total = resistenciaParalelo resistencias
        esperada = 50 -- metade do valor
    in abs (total- esperada) < 0.001


testeResistenciaParaleloTresIguais::Bool
testeResistenciaParaleloTresIguais =
    -- Três resistores iguais: 150Ω || 150Ω || 150Ω
    let resistencias = [150, 150, 150]
        total = resistenciaParalelo resistencias
        esperada = 50 -- R/3 = 150/3 = 50Ω
    in abs (total- esperada) < 0.001


testeResistenciaParaleloUmResistor::Bool
testeResistenciaParaleloUmResistor =
    let resistencias = [75] -- um resistor
        total = resistenciaParalelo resistencias
    in abs (total- 75) < 0.001


testeResistenciaParaleloValoresAltos::Bool
testeResistenciaParaleloValoresAltos =
    -- 1MΩ || 2MΩ
    let resistencias = [1e6, 2e6]
        total = resistenciaParalelo resistencias
        esperada = (1e6 * 2e6) / (1e6 + 2e6)

    -- ≈ 666.67kΩ
    in abs (total- esperada) < 1000


testeResistenciaParaleloBaixosValores::Bool
testeResistenciaParaleloBaixosValores =
    -- 0.2Ω || 0.3Ω
    let resistencias = [0.2, 0.3]
        total = resistenciaParalelo resistencias
        esperada = 0.12 -- 0.12Ω
    in abs (total- esperada) < 0.001


testeResistenciaParaleloAssimetrico::Bool
testeResistenciaParaleloAssimetrico =
    -- 1Ω || 1000Ω (um muito menor que outro)
    let resistencias = [1, 1000]
        total = resistenciaParalelo resistencias -- Resultado próximo do menor resistor
        esperada = 1000 / 1001 -- ≈ 0.999Ω
    in abs (total- esperada) < 0.01


testeResistenciaParaleloQuatroResistores::Bool
testeResistenciaParaleloQuatroResistores =
    -- 40Ω || 60Ω || 120Ω || 200Ω
    let resistencias = [40, 60, 120, 200]
        total = resistenciaParalelo resistencias -- 1/Rp = 1/40 + 1/60 + 1/120 + 1/200
        reciprocoSoma = sum (map (1/) resistencias)
        esperada = 1 / reciprocoSoma
    in abs (total- esperada) < 0.1


testeResistenciaParaleloDecimais::Bool
testeResistenciaParaleloDecimais =
    let resistencias = [4.7, 10, 22]
        total = resistenciaParalelo resistencias
        reciprocoSoma = (1/4.7) + (1/10) + (1/22)
        esperada = 1 / reciprocoSoma
    in abs (total- esperada) < 0.01


testeResistenciaParaleloCircuitoPratico::Bool
testeResistenciaParaleloCircuitoPratico =
    -- Banco de resistores paralelos: 100Ω || 220Ω || 470Ω
    let resistencias = [100, 220, 470]
        total = resistenciaParalelo resistencias
        reciprocoSoma = (1/100) + (1/220) + (1/470)
        esperada = 1 / reciprocoSoma -- ~64.4Ω
    in abs (total- esperada) < 1.0

-- =================================


-- TESTES PARA IMPEDâNCIA AC

-- =================================




testeImpedanciaACBasica::Bool
testeImpedanciaACBasica =
    let resistencia = 3 -- 3 Ω
        reatancia = 4 -- 4 Ω
        impedancia = impedanciaAC resistencia reatancia
        esperada = 5 -- Z = √R+X =√9+16=5Ω
    in abs (impedancia- esperada) < 0.001


testeImpedanciaACResistivoPuro::Bool
testeImpedanciaACResistivoPuro =
    -- Circuito puramente resistivo (X = 0)
    let resistencia = 50
        reatancia = 0
        impedancia = impedanciaAC resistencia reatancia
        esperada = 50 -- Z = R quando X = 0
    in abs (impedancia- esperada) < 0.001


testeImpedanciaACReativoPuro::Bool
testeImpedanciaACReativoPuro =
    -- Circuito puramente reativo (R = 0)
    let resistencia = 0
        reatancia = 25
        impedancia = impedanciaAC resistencia reatancia
        esperada = 25 -- Z = |X| quando R = 0
    in abs (impedancia- esperada) < 0.001


testeImpedanciaACMotorCA::Bool
testeImpedanciaACMotorCA =
    -- Motor CA: R = 2Ω, X = 6Ω
    let resistencia = 2
        reatancia = 6
        impedancia = impedanciaAC resistencia reatancia
        esperada = sqrt (4 + 36)

    -- √40 ≈6.32Ω
    in abs (impedancia- esperada) < 0.01


testeImpedanciaACTransformador::Bool
testeImpedanciaACTransformador =
    -- Transformador: R = 0.5Ω, X = 2.5 Ω
    let resistencia = 0.5
        reatancia = 2.5
        impedancia = impedanciaAC resistencia reatancia
        esperada = sqrt (0.25 + 6.25)

    -- √6.5 ≈ 2.55Ω
    in abs (impedancia- esperada) < 0.01


testeImpedanciaACCircuitoRLC::Bool
testeImpedanciaACCircuitoRLC =
    -- Circuito RLC: R = 10Ω, X = 24Ω
    let resistencia = 10
        reatancia = 24
        impedancia = impedanciaAC resistencia reatanciaesperada = 26 -- √100+576 = 26Ω
    in abs (impedancia- esperada) < 0.1


testeImpedanciaACReatanciaNegativa::Bool
testeImpedanciaACReatanciaNegativa =
    -- Reatância capacitiva (negativa): R = 8Ω, X =-6Ω
    let resistencia = 8
        reatancia =-6 -- capacitiva
        impedancia = impedanciaAC resistencia reatancia
        esperada = 10 -- √64+36 =10Ω
    in abs (impedancia- esperada) < 0.001


testeImpedanciaACValoresAltos::Bool
testeImpedanciaACValoresAltos =
    -- Alta impedância: R = 1000Ω, X = 1500Ω
    let resistencia = 1000
        reatancia = 1500
        impedancia = impedanciaAC resistencia reatancia
        esperada = sqrt (1000000 + 2250000)

    -- ≈ 1802.8Ω
    in abs (impedancia- esperada) < 1.0


testeImpedanciaACBaixaImpedancia::Bool
testeImpedanciaACBaixaImpedancia =
    -- Baixa impedância: R = 0.1Ω, X = 0.2Ω
    let resistencia = 0.1
        reatancia = 0.2
        impedancia = impedanciaAC resistencia reatancia
        esperada = sqrt (0.01 + 0.04)

    -- √0.05 ≈ 0.224Ω
    in abs (impedancia- esperada) < 0.001


testeImpedanciaACDecimal::Bool
testeImpedanciaACDecimal =
    let resistencia = 12.5
        reatancia = 9.3
        impedancia = impedanciaAC resistencia reatancia
        esperada = sqrt (12.5^2 + 9.3^2)
    in abs (impedancia- esperada) < 0.01




testePolarParaRetangularBasico::Bool
testePolarParaRetangularBasico =
    let modulo = 5 -- 5
        angulo = pi/3 -- 60°
        (x, y) = polarParaRetangular modulo angulo
        xEsperado = 5 * cos(pi/3)

    -- 2.5
        yEsperado = 5 * sin(pi/3)

    -- 4.33
    in abs (x- xEsperado) < 0.01 && abs (y- yEsperado) < 0.01


testePolarParaRetangularAngulo0::Bool
testePolarParaRetangularAngulo0 =
    -- ângulo 0° (eixo real positivo)
    let modulo = 10
        angulo = 0
        (x, y) = polarParaRetangular modulo angulo
    in abs (x- 10) < 0.001 && abs y < 0.001


testePolarParaRetangularAngulo90::Bool
testePolarParaRetangularAngulo90 =
    -- ângulo 90° (eixo imaginário positivo)
    let modulo = 8
        angulo = pi/2
        (x, y) = polarParaRetangular modulo angulo
    in abs x < 0.001 && abs (y- 8) < 0.001


testePolarParaRetangularAngulo180::Bool
testePolarParaRetangularAngulo180 =
    -- ângulo 180° (eixo real negativo)
    let modulo = 6
        angulo = pi
        (x, y) = polarParaRetangular modulo angulo
    in abs (x- (-6)) < 0.01 && abs y < 0.01


testePolarParaRetangularAngulo270::Bool
testePolarParaRetangularAngulo270 =
    -- ângulo 270° (eixo imaginário negativo)
    let modulo = 4
        angulo = 3*pi/2
        (x, y) = polarParaRetangular modulo angulo
    in abs x < 0.01 && abs (y- (-4)) < 0.01


testeRetangularParaPolarBasico::Bool
testeRetangularParaPolarBasico =
        let x = 3
            y = 4
            (r, theta) = retangularParaPolar x y
            rEsperado = 5
            thetaEsperado = atan2 4 3 -- √3+4=5 -- atan(4/3)
        in abs (r- rEsperado) < 0.001 && abs (theta- thetaEsperado) < 0.001


testeRetangularParaPolarEixoReal::Bool
testeRetangularParaPolarEixoReal =
    -- Ponto no eixo real positivo
    let x = 7
        y = 0
        (r, theta) = retangularParaPolar x y
    in abs (r- 7) < 0.001 && abs theta < 0.001


testeRetangularParaPolarEixoImaginario::Bool
testeRetangularParaPolarEixoImaginario =
    -- Ponto no eixo imaginário positivo
    let x = 0y = 9
        (r, theta) = retangularParaPolar x y
    in abs (r- 9) < 0.001 && abs (theta- pi/2) < 0.01


testeRetangularParaPolarQuadrante2::Bool
testeRetangularParaPolarQuadrante2 =
    -- Segundo quadrante
    let x =-3
        y = 4
        (r, theta) = retangularParaPolar x y
        rEsperado = 5
        thetaEsperado = atan2 4 (-3)

    -- ângulo no 2° quadrante
    in abs (r- rEsperado) < 0.001 && abs (theta- thetaEsperado) < 0.01


testeRetangularParaPolarQuadrante3::Bool
testeRetangularParaPolarQuadrante3 =
    -- Terceiro quadrante
    let x =-5
        y =-12
        (r, theta) = retangularParaPolar x y
        rEsperado = 13 -- √25+144 = 13
        thetaEsperado = atan2 (-12) (-5)
    in abs (r- rEsperado) < 0.001 && abs (theta- thetaEsperado) < 0.01


testeConversaoRoundTrip::Bool
testeConversaoRoundTrip =
    -- Teste ida e volta
    let rOriginal = 10
        thetaOriginal = pi/4
        (x, y) = polarParaRetangular rOriginal thetaOriginal
        (rFinal, thetaFinal) = retangularParaPolar x y
    in abs (rOriginal- rFinal) < 0.001 &&
        abs (thetaOriginal- thetaFinal) < 0.001


testeConversaoRoundTripComplexo::Bool
testeConversaoRoundTripComplexo =
    -- Teste com valores complexos
    let xOriginal =-7.5
        yOriginal = 2.3
        (r, theta) = retangularParaPolar xOriginal yOriginal
        (xFinal, yFinal) = polarParaRetangular r theta
    in abs (xOriginal- xFinal) < 0.01 && abs (yOriginal- yFinal) < 0.01


testePolarParaRetangularFasor::Bool
testePolarParaRetangularFasor =
    -- Fasor elétrico: 100̸ 30°
    let modulo = 100
        angulo = pi/6 -- 30°
        (x, y) = polarParaRetangular modulo angulo
        xEsperado = 100 * cos(pi/6)

    -- ≈ 86.6
        yEsperado = 100 * sin(pi/6)

    -- 50
    in abs (x- xEsperado) < 0.1 && abs (y- yEsperado) < 0.1

testeRetangularParaPolarImpedancia::Bool
testeRetangularParaPolarImpedancia =
    -- Impedância complexa: Z = 6 + j8
    let x = 6 -- resistência
        y = 8 -- reatância
        (r, theta) = retangularParaPolar x y
        rEsperado = 10 -- |Z| = 10Ω
        thetaEsperado = atan2 8 6 -- ângulo de fase
    in abs (r- rEsperado) < 0.1 && abs (theta- thetaEsperado) < 0.01


testeConversaoOrigemZero::Bool
testeConversaoOrigemZero =
    let x = 0
        y = 0
        (r, theta) = retangularParaPolar x y
    in abs r < 0.001 -- módulo deve ser zero (ângulo indefinido)




testeCasoRealCircuitoResidencial::Bool
testeCasoRealCircuitoResidencial =
    -- Circuito residencial: lâmpadas em paralelo
    let lampadaW = 60
        tensao = 120 -- 60W cada -- 120V
        resistenciaUmaLampada = tensao^2 / lampadaW -- R = V²/P = 240Ω
        resistencias = replicate 5 resistenciaUmaLampada -- 5 lâmpadas
        resistenciaTotal = resistenciaParalelo resistencias
        correnteTotal = tensao / resistenciaTotal
        potenciaTotal = potenciaEletricaVI tensao correnteTotal -- Verificações
        resistenciaOk = resistenciaTotal < 50 -- menor que individual
        potenciaOk = abs (potenciaTotal- 300) < 10 -- 5 × 60W = 300W
    in resistenciaOk && potenciaOk


testeCasoRealMotorTrifasico::Bool
testeCasoRealMotorTrifasico =
    -- Motor trifásico industrial
    let tensao = 380 -- 380V
        resistencia = 0.5 -- 0.5Ω por fase
        reatancia = 2.0 -- 2Ω reatância
        impedancia = impedanciaAC resistencia reatancia
        corrente = tensao / impedancia
        potenciaAparente = potenciaEletricaVI tensao corrente -- Conversão para forma retangular/polar
        (x, y) = (resistencia, reatancia)
        (r, theta) = retangularParaPolar x y -- Verificações
        impedanciaOk = impedancia > 2 && impedancia < 3 -- ~2.06Ω
        anguloOk = theta > 1.0 && theta < 1.4 -- ~76° em radianos
    in impedanciaOk && anguloOk


testeCasoRealDivisorTensao::Bool
testeCasoRealDivisorTensao =
    -- Divisor de tensão para sensor
    let tensaoFonte = 12 -- 12V
        r1 = 10000 -- 10kΩ
        r2 = 5000 -- 5kΩ (sensor)
        resistenciaTotal = resistenciaSerie [r1, r2]
        correnteCircuito = tensaoFonte / resistenciaTotal
        tensaoSensor = tensaoOhm correnteCircuito r2
        potenciaSensor = potenciaEletricaRI r2 correnteCircuito -- Verificações
        tensaoOk = abs (tensaoSensor- 4) < 0.1 -- 1/3 de 12V = 4V
        potenciaOk = potenciaSensor < 0.005 -- menos de 5mW
    in tensaoOk && potenciaOk


testeCasoRealCarregadorCelular::Bool
testeCasoRealCarregadorCelular =
    -- Carregador USB-C: 20V, 3A
    let tensao = 20
        corrente = 3
        potenciaSaida = potenciaEletricaVI tensao corrente
        resistenciaInterna = 0.1 -- 0.1Ω resistência interna
        perdaInterna = potenciaEletricaRI resistenciaInterna corrente
        potenciaEntrada = potenciaSaida + perdaInterna
        eficiencia = potenciaSaida / potenciaEntrada -- Verificações
        potenciaOk = abs (potenciaSaida- 60) < 1 -- 60W
        eficienciaOk = eficiencia > 0.98 -- > 98% eficiência
    in potenciaOk && eficienciaOk


testeCasoRealFiltroPassaBaixa::Bool
testeCasoRealFiltroPassaBaixa =
    -- Filtro RC passa-baixa: R=1kΩ, f=1kHz, C=159nF
    let resistencia = 1000
        frequencia = 1000 -- 1kHz
        capacitancia = 159e-9 -- 159nF
        reatanciaCapacitiva = 1 / (2 * pi * frequencia * capacitancia)
        impedancia = impedanciaAC resistencia reatanciaCapacitiva -- Conversão para análise fasorial
        (r, theta) = retangularParaPolar resistencia reatanciaCapacitiva -- Verificações (reatância capacitiva é negativa, mas usamos módulo)
        impedanciaOk = impedancia > 1000 && impedancia < 1500 -- ~1414Ω
        anguloOk = abs theta > 0.7 && abs theta < 0.9 -- ~45° = π/4 rad
    in impedanciaOk && anguloOk


executarTestesEngenhariaEletrica::IO ()
executarTestesEngenhariaEletrica = do
    putStrLn "======================================"
    putStrLn "TESTES ENGENHARIA ELÉTRICA"
    putStrLn "======================================"
    putStrLn "\n -- TESTES LEI DE OHM --"
    putStrLn $ "Básica: " ++ show testeLeiOhmBasica
    putStrLn $ "Lâmpada: " ++ show testeLeiOhmLampada
    putStrLn $ "Resistor eletrônico: " ++ show testeLeiOhmResistorEletronico
    putStrLn $ "Motor elétrico: " ++ show testeLeiOhmMotorEletrico
    putStrLn $ "Corrente zero: " ++ show testeLeiOhmCorrenteZero
    putStrLn $ "Corrente alta: " ++ show testeLeiOhmCorrenteAlta
    putStrLn $ "Microeletrônica: " ++ show testeLeiOhmMicroeletronica
    putStrLn $ "Transmissão: " ++ show testeLeiOhmTransmissao
    putStrLn $ "Decimal: " ++ show testeLeiOhmDecimal
    putStrLn $ "Precisão: " ++ show testeLeiOhmPrecisao
    putStrLn "\n -- TESTES POTÊNCIA V×I --"
    putStrLn $ "Básica: " ++ show testePotenciaVIBasica
    putStrLn $ "LED: " ++ show testePotenciaVILED
    putStrLn $ "Chuveiro: " ++ show testePotenciaVIChuveiro
    putStrLn $ "Microprocessador: " ++ show testePotenciaVIMicroprocessador
    putStrLn $ "Carregador celular: " ++ show testePotenciaVICarregadorCelular
    putStrLn $ "Zero: " ++ show testePotenciaVIZero
    putStrLn $ "Alta tensão: " ++ show testePotenciaVIAltaTensao
    putStrLn $ "Baixa potência: " ++ show testePotenciaVIBaixaPotencia
    putStrLn $ "Decimal: " ++ show testePotenciaVIDecimal
    putStrLn $ "Dispositivo 9V: " ++ show testePotenciaVIDispositivo9V
    putStrLn "\n -- TESTES POTÊNCIA R×I² --"
    putStrLn $ "Básica: " ++ show testePotenciaRIBasica
    putStrLn $ "Resistor aquecimento: " ++ show testePotenciaRIResistorAquecimento
    putStrLn $ "Filamento lâmpada: " ++ show testePotenciaRIFilamentoLampada
    putStrLn $ "Fio transmissão: " ++ show testePotenciaRIFioTransmissao
    putStrLn $ "Corrente zero: " ++ show testePotenciaRICorrenteZero
    putStrLn $ "Baixa resistência: " ++ show testePotenciaRIBaixaResistencia
    putStrLn $ "Alta resistência: " ++ show testePotenciaRIAltaResistencia
    putStrLn $ "Decimal: " ++ show testePotenciaRIDecimal
    putStrLn $ "Torradeira: " ++ show testePotenciaRITorradeira
    putStrLn $ "Ferro solda: " ++ show testePotenciaRIFerroSolda
    putStrLn "\n -- TESTES POTÊNCIA V²/R --"
    putStrLn $ "Básica: " ++ show testePotenciaVRBasica
    putStrLn $ "Lâmpada doméstica: " ++ show testePotenciaVRLampadaDomestica
    putStrLn $ "Aquecedor: " ++ show testePotenciaVRAquecedor
    putStrLn $ "Circuito eletrônico: " ++ show testePotenciaVRCircuitoEletronico
    putStrLn $ "Tensão zero: " ++ show testePotenciaVRTensaoZero
    putStrLn $ "Alta tensão: " ++ show testePotenciaVRAltaTensao
    putStrLn $ "Resistor pequeno: " ++ show testePotenciaVRResistorPequeno
    putStrLn $ "Decimal: " ++ show testePotenciaVRDecimal
    putStrLn $ "Secador cabelo: " ++ show testePotenciaVRSecadorCabelo
    putStrLn $ "Forno: " ++ show testePotenciaVRForno
    putStrLn "\n -- TESTES RESISTÊNCIA SÉRIE --"
    putStrLn $ "Básica: " ++ show testeResistenciaSerieBasica
    putStrLn $ "Um resistor: " ++ show testeResistenciaSerieUmResistor
    putStrLn $ "Vazia: " ++ show testeResistenciaSerieVazia
    putStrLn $ "Circuito LED: " ++ show testeResistenciaSerieCircuitoLED
    putStrLn $ "Divisor tensão: " ++ show testeResistenciaSerieDivisorTensao
    putStrLn $ "Altos valores: " ++ show testeResistenciaSerieAltosValores
    putStrLn $ "Baixos valores: " ++ show testeResistenciaSerieBaixosValores
    putStrLn $ "Decimais: " ++ show testeResistenciaSerieDecimais
    putStrLn $ "Muitos resistores: " ++ show testeResistenciaSerieMuitosResistores
    putStrLn $ "Valores variados: " ++ show testeResistenciaSerieValoresVariados
    putStrLn "\n -- TESTES RESISTÊNCIA PARALELO --"
    putStrLn $ "Básica: " ++ show testeResistenciaParaleloBasica
    putStrLn $ "Iguais: " ++ show testeResistenciaParaleloIguais
    putStrLn $ "Três iguais: " ++ show testeResistenciaParaleloTresIguais
    putStrLn $ "Um resistor: " ++ show testeResistenciaParaleloUmResistor
    putStrLn $ "Valores altos: " ++ show testeResistenciaParaleloValoresAltos
    putStrLn $ "Baixos valores: " ++ show testeResistenciaParaleloBaixosValores
    putStrLn $ "Assimétrico: " ++ show testeResistenciaParaleloAssimetrico
    putStrLn $ "Quatro resistores: " ++ show testeResistenciaParaleloQuatroResistores
    putStrLn $ "Decimais: " ++ show testeResistenciaParaleloDecimais
    putStrLn $ "Circuito prático: " ++ show testeResistenciaParaleloCircuitoPratico
    putStrLn "\n -- TESTES IMPEDÂNCIA AC --"
    putStrLn $ "Básica: " ++ show testeImpedanciaACBasica
    putStrLn $ "Resistivo puro: " ++ show testeImpedanciaACResistivoPuro
    putStrLn $ "Reativo puro: " ++ show testeImpedanciaACReativoPuro
    putStrLn $ "Motor CA: " ++ show testeImpedanciaACMotorCA
    putStrLn $ "Transformador: " ++ show testeImpedanciaACTransformador
    putStrLn $ "Circuito RLC: " ++ show testeImpedanciaACCircuitoRLC
    putStrLn $ "Reatância negativa: " ++ show testeImpedanciaACReatanciaNegativa
    putStrLn $ "Valores altos: " ++ show testeImpedanciaACValoresAltos
    putStrLn $ "Baixa impedância: " ++ show testeImpedanciaACBaixaImpedancia
    putStrLn $ "Decimal: " ++ show testeImpedanciaACDecimal
    putStrLn "\n -- TESTES CONVERSÕES COORDENADAS --"
    putStrLn $ "Polar→Retangular básico: " ++ show testePolarParaRetangularBasico
    putStrLn $ "Ângulo 0°: " ++ show testePolarParaRetangularAngulo0
    putStrLn $ "Ângulo 90°: " ++ show testePolarParaRetangularAngulo90
    putStrLn $ "Ângulo 180°: " ++ show testePolarParaRetangularAngulo180
    putStrLn $ "Ângulo 270°: " ++ show testePolarParaRetangularAngulo270
    putStrLn $ "Retangular→Polar básico: " ++ show testeRetangularParaPolarBasico
    putStrLn $ "Eixo real: " ++ show testeRetangularParaPolarEixoReal
    putStrLn $ "Eixo imaginário: " ++ show testeRetangularParaPolarEixoImaginario
    putStrLn $ "2° quadrante: " ++ show testeRetangularParaPolarQuadrante2
    putStrLn $ "3° quadrante: " ++ show testeRetangularParaPolarQuadrante3
    putStrLn $ "Round trip: " ++ show testeConversaoRoundTrip
    putStrLn $ "Round trip complexo: " ++ show testeConversaoRoundTripComplexo
    putStrLn $ "Fasor elétrico: " ++ show testePolarParaRetangularFasor
    putStrLn $ "Impedância complexa: " ++ show testeRetangularParaPolarImpedancia
    putStrLn $ "Origem zero: " ++ show testeConversaoOrigemZero
    putStrLn "\n -- TESTES CASOS REAIS --"
    putStrLn $ "Circuito residencial: " ++ show testeCasoRealCircuitoResidencial
    putStrLn $ "Motor trifásico: " ++ show testeCasoRealMotorTrifasico
    putStrLn $ "Divisor tensão: " ++ show testeCasoRealDivisorTensao
    putStrLn $ "Carregador celular: " ++ show testeCasoRealCarregadorCelular
    putStrLn $ "Filtro passa-baixa: " ++ show testeCasoRealFiltroPassaBaixa


testesEngenhariaEletrica::[(String, Bool)]
testesEngenhariaEletrica = [
    -- Lei de Ohm (10 testes)
    ("Lei Ohm Básica", testeLeiOhmBasica),
    ("Lei Ohm Lâmpada", testeLeiOhmLampada),
    ("Lei Ohm Resistor Eletrônico", testeLeiOhmResistorEletronico),
    ("Lei Ohm Motor Elétrico", testeLeiOhmMotorEletrico),
    ("Lei Ohm Corrente Zero", testeLeiOhmCorrenteZero),
    ("Lei Ohm Corrente Alta", testeLeiOhmCorrenteAlta),
    ("Lei Ohm Microeletrônica", testeLeiOhmMicroeletronica),
    ("Lei Ohm Transmissão", testeLeiOhmTransmissao),
    ("Lei Ohm Decimal", testeLeiOhmDecimal),
    ("Lei Ohm Precisão", testeLeiOhmPrecisao),

    -- Potência V×I (10 testes)
    ("Potência VI Básica", testePotenciaVIBasica),
    ("Potência VI LED", testePotenciaVILED),
    ("Potência VI Chuveiro", testePotenciaVIChuveiro),
    ("Potência VI Microprocessador", testePotenciaVIMicroprocessador),
    ("Potência VI Carregador", testePotenciaVICarregadorCelular),
    ("Potência VI Zero", testePotenciaVIZero),
    ("Potência VI Alta Tensão", testePotenciaVIAltaTensao),
    ("Potência VI Baixa", testePotenciaVIBaixaPotencia),
    ("Potência VI Decimal", testePotenciaVIDecimal),
    ("Potência VI Dispositivo 9V", testePotenciaVIDispositivo9V),

    -- Potência R×I² (10 testes)
    ("Potência RI Básica", testePotenciaRIBasica),
    ("Potência RI Aquecimento", testePotenciaRIResistorAquecimento),
    ("Potência RI Filamento", testePotenciaRIFilamentoLampada),
    ("Potência RI Fio", testePotenciaRIFioTransmissao),
    ("Potência RI Corrente Zero", testePotenciaRICorrenteZero),
    ("Potência RI Baixa R", testePotenciaRIBaixaResistencia),
    ("Potência RI Alta R", testePotenciaRIAltaResistencia),
    ("Potência RI Decimal", testePotenciaRIDecimal),
    ("Potência RI Torradeira", testePotenciaRITorradeira),
    ("Potência RI Ferro Solda", testePotenciaRIFerroSolda),

    -- Potência V²/R (10 testes)
    ("Potência VR Básica", testePotenciaVRBasica),
    ("Potência VR Lâmpada", testePotenciaVRLampadaDomestica),
    ("Potência VR Aquecedor", testePotenciaVRAquecedor),
    ("Potência VR Eletrônico", testePotenciaVRCircuitoEletronico),
    ("Potência VR Tensão Zero", testePotenciaVRTensaoZero),
    ("Potência VR Alta Tensão", testePotenciaVRAltaTensao),
    ("Potência VR Resistor Pequeno", testePotenciaVRResistorPequeno),
    ("Potência VR Decimal", testePotenciaVRDecimal),
    ("Potência VR Secador", testePotenciaVRSecadorCabelo),
    ("Potência VR Forno", testePotenciaVRForno),

    -- Resistência Série (10 testes)
    ("Resistência Série Básica", testeResistenciaSerieBasica),
    ("Resistência Série Um", testeResistenciaSerieUmResistor),
    ("Resistência Série Vazia", testeResistenciaSerieVazia),
    ("Resistência Série LED", testeResistenciaSerieCircuitoLED),
    ("Resistência Série Divisor", testeResistenciaSerieDivisorTensao),
    ("Resistência Série Alta", testeResistenciaSerieAltosValores),
    ("Resistência Série Baixa", testeResistenciaSerieBaixosValores),
    ("Resistência Série Decimal", testeResistenciaSerieDecimais),
    ("Resistência Série Muitos", testeResistenciaSerieMuitosResistores),
    ("Resistência Série Variados", testeResistenciaSerieValoresVariados),

    -- Resistência Paralelo (10 testes)
    ("Resistência Paralelo Básica", testeResistenciaParaleloBasica),
    ("Resistência Paralelo Iguais", testeResistenciaParaleloIguais),
    ("Resistência Paralelo Três", testeResistenciaParaleloTresIguais),
    ("Resistência Paralelo Um", testeResistenciaParaleloUmResistor),
    ("Resistência Paralelo Alta", testeResistenciaParaleloValoresAltos),
    ("Resistência Paralelo Baixa", testeResistenciaParaleloBaixosValores),
    ("Resistência Paralelo Assimétrico", testeResistenciaParaleloAssimetrico),
    ("Resistência Paralelo Quatro", testeResistenciaParaleloQuatroResistores),
    ("Resistência Paralelo Decimal", testeResistenciaParaleloDecimais),
    ("Resistência Paralelo Prático", testeResistenciaParaleloCircuitoPratico),

    -- Impedância AC (10 testes)
    ("Impedância AC Básica", testeImpedanciaACBasica),
    ("Impedância AC Resistivo", testeImpedanciaACResistivoPuro),
    ("Impedância AC Reativo", testeImpedanciaACReativoPuro),
    ("Impedância AC Motor", testeImpedanciaACMotorCA),
    ("Impedância AC Transformador", testeImpedanciaACTransformador),
    ("Impedância AC RLC", testeImpedanciaACCircuitoRLC),
    ("Impedância AC Negativa", testeImpedanciaACReatanciaNegativa),
    ("Impedância AC Alta", testeImpedanciaACValoresAltos),
    ("Impedância AC Baixa", testeImpedanciaACBaixaImpedancia),
    ("Impedância AC Decimal", testeImpedanciaACDecimal),

    -- Conversões Coordenadas (15 testes)
    ("Polar→Retangular Básico", testePolarParaRetangularBasico),
    ("Polar→Retangular 0°", testePolarParaRetangularAngulo0),
    ("Polar→Retangular 90°", testePolarParaRetangularAngulo90),
    ("Polar→Retangular 180°", testePolarParaRetangularAngulo180),
    ("Polar→Retangular 270°", testePolarParaRetangularAngulo270),
    ("Retangular→Polar Básico", testeRetangularParaPolarBasico),
    ("Retangular→Polar Eixo Real", testeRetangularParaPolarEixoReal),
    ("Retangular→Polar Eixo Imag", testeRetangularParaPolarEixoImaginario),
    ("Retangular→Polar Q2", testeRetangularParaPolarQuadrante2),
    ("Retangular→Polar Q3", testeRetangularParaPolarQuadrante3),
    ("Conversão Round Trip", testeConversaoRoundTrip),
    ("Conversão Round Trip Complexo", testeConversaoRoundTripComplexo),
    ("Polar→Retangular Fasor", testePolarParaRetangularFasor),
    ("Retangular→Polar Impedância", testeRetangularParaPolarImpedancia),
    ("Conversão Origem Zero", testeConversaoOrigemZero),

    -- Casos Reais (5 testes)
    ("Caso Real Residencial", testeCasoRealCircuitoResidencial),
    ("Caso Real Motor Trifásico", testeCasoRealMotorTrifasico),
    ("Caso Real Divisor Tensão", testeCasoRealDivisorTensao),
    ("Caso Real Carregador", testeCasoRealCarregadorCelular),
    ("Caso Real Filtro", testeCasoRealFiltroPassaBaixa)
]