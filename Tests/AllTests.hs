module Tests.AllTests where

import Tests.AlgorithmsTests
import Tests.BinaryTreeTests
import Tests.CalculusTests
import Tests.CivilEngineeringTests
import Tests.ElectricalEngineeringTests
import Tests.GeometryTests
import Tests.LinearAlgebraTests
import Tests.MechanicalEngineeringTests
import Tests.SortingTests
-- import Tests.ValidationTests


executarTestes:: IO ()
executarTestes = do
        executarTestesEspecialista4
        executarTestesCalculo
        executarTestesCivilEngineering
        executarTestesEngenhariaEletrica
        executarTestesAlgebraLinear
        executarTestesEngenhariaMecanica
        -- executarTestesEspecialista5