module Engineering.Civil (
    momentoInerciaRetangular,

    tensaoNormal,
    deflexaoViga,

    cargaCriticaEuler,
    
    volumeConcreto
) where

import Types
import Geometry(calcularVolume)

--  I = b · h² / 12
momentoInerciaRetangular::Largura -> Altura -> MomentoInercia
momentoInerciaRetangular b l = (b * l^3) / 12

-- σ = P / A
tensaoNormal::Forca -> Area -> Pressao
tensaoNormal = (/)
-- δ = P · L² / (48 · E ·I)
deflexaoViga::Forca -> Comprimento -> ModuloElasticidade -> MomentoInercia -> Distancia
deflexaoViga p l e i =  p * l^3 / (48 * e * i)

-- Pcr = π² · E · I / L²
cargaCriticaEuler::ModuloElasticidade -> MomentoInercia -> Comprimento -> Forca
cargaCriticaEuler e i l = (pi^2 * e * i) / (l^2)

volumeConcreto::Figura -> Volume
volumeConcreto = calcularVolume