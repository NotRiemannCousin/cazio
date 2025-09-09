import Types

validarProjeto::Projeto-> [String]


calcularCustoTotal::Projeto-> Custo


gerarRelatorioProjeto::Projeto-> [String]


compararProjetos::Projeto-> Projeto-> String


estatisticasBasicas::[Double]-> (Double, Double, Double)


contarPorTipo::[Projeto]-> [(TipoProjeto, Int)]


projetosEmAtraso::[Projeto]-> Day-> [Projeto]