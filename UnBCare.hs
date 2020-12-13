module UnBCare where

import ModeloDados

import Data.Function (on)
import Data.List

{-

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

 
 
O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}



{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}
comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento m q [] = (m,q):[]
comprarMedicamento m q (x:xs)
   | m == fst x = (m, q + snd x) : xs
   | otherwise = x : comprarMedicamento m q xs
   
{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento m (x:xs)
   | m == fst x = Just $ (m, snd x - 1):xs
   | otherwise = Nothing

{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento _ [] = 0
consultarMedicamento m (x:xs)
   | m == fst x = snd x
   | otherwise = consultarMedicamento m xs

{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos xs = qs(map count xs)
      where count x = (fst x, length(snd x))

qs :: Ord a => [a] -> [a]
qs [] = []
qs (a:as) = qs [e | e <- as, e <= a]  ++ [a] ++ qs [e | e <- as, e > a]

{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}

receituarioValido :: Receituario -> Bool
receituarioValido [_] = True
receituarioValido (x:y:xys)
   | (fst x /= fst y) && (fst x < fst y) && (verificaOrdemDestincao(snd x)) = receituarioValido (y:xys)
   | otherwise = False

planoValido :: PlanoMedicamento -> Bool
planoValido [_] = True
planoValido (x:y:xys)
   | (fst x /= fst y) && (fst x < fst y) && (verificaOrdemDestincao(snd x)) = planoValido(y:xys)
   | otherwise = False

verificaOrdemDestincao::Ord a => [a] -> Bool
verificaOrdemDestincao [_] = True
verificaOrdemDestincao (x:y:xys)
   | (x /= y) && (x < y) = verificaOrdemDestincao(y:xys)
   | otherwise = False


{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

 -}

plantaoValido :: Plantao -> Bool
plantaoValido [_] = True
plantaoValido (x:y:xys)
   | (fst x /= fst y) && (fst x < fst y) && (verificaCuidado(snd x)) = plantaoValido (y:xys)
   | otherwise = False

verificaCuidado:: [Cuidado] -> Bool
verificaCuidado [_] = True
verificaCuidado (x:y:xys)
   | ( (get1of3(tipoCuidado x) /= get1of3(tipoCuidado y)) && (get2of3(tipoCuidado x) == get2of3(tipoCuidado y)) )
      ||
      ( (get1of3(tipoCuidado x) == get1of3(tipoCuidado y)) && (get2of3(tipoCuidado x) > get2of3(tipoCuidado y)) ) = False
   | otherwise = verificaCuidado (y:xys)


-- As seguintes funções foram feitas de forma que o uso seja possível na questão 6 e 9, mas não consegui terminar a 9.
tipoCuidado:: Cuidado -> (Int, String, Int)
tipoCuidado (Comprar m q) = (1,m,q)
tipoCuidado (Medicar m) = (2,m,0)

get1of3:: (a,b,c) -> a
get1of3 (a,_,_) = a

get2of3:: (a,b,c) -> b
get2of3 (_,b,_) = b

get3of3:: (a,b,c) -> c
get3of3 (_,_,c) = c

{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario [] = []
geraPlanoReceituario xs =  agrupaPadraoLista(qs(linearizaLista xs))

linearizaLista:: [(a, [b])] -> [(b, a)]
linearizaLista [] = []
linearizaLista ((a, b):abs)
   | length b >= 1 = map decompoeLista b ++ linearizaLista abs
   where decompoeLista b = (b, a)

agrupaPadraoLista:: Eq a => [(a, b)] -> [(a,[b])]
agrupaPadraoLista =  map (\lista -> (fst.head $ lista, map snd lista)).groupBy( (==) `on` fst )

{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano [] = []
geraReceituarioPlano xs = agrupaPadraoLista(qs(linearizaLista xs))


{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado 
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao [] [] = Nothing
-- executaPlantao ps es = fazOperacao(linearizaLista ps) es

-- fazOperacao:: [(Cuidado,Horario)] -> EstoqueMedicamentos -> EstoqueMedicamentos
-- -- fazOperacao (a:as) es
-- --    | get1of3(tipoCuidado (fst a)) == 1 = comprarMedicamento( get2of3(tipoCuidado (fst a)) get3of3(tipoCuidado (fst a)) es)
-- --    | otherwise = tomarMedicamento(  )

{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano 
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão 
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos  -> Bool
satisfaz = undefined


{-

QUESTÃO 11 (EXTRA) VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}

plantaoCorreto :: PlanoMedicamento ->  EstoqueMedicamentos  -> Plantao
plantaoCorreto = undefined

