
* Neste sintaxe, tratarei a base do CEB 2012, cruzando-a com as bases limpas do CES 2013-2017, visando construir as bases finais para análise
#delimit cr
version 16.1
clear
set more off
set linesize 120
capture log close

*** Definir diretórios
global data "inserir caminho das bases do CEB"
global ces "inserir caminho das bases do CES"
global output "inserir caminho dos produtos"

*** Abrir base de egressos 2012
use "${data}\ceb_egressos_2012", clear
drop if chave_unica==.

*************************************************************
*** Cruzamento e limpando com CES em MENOR nível de agregação

* Cruzar a base de egressos com cada ano do CES, mantendo apenas os casos da base original (master) ou que cruzaram (match)
foreach year in "13" "14" "15" "16" "17" {
sort chave_unica	
merge m:m chave_unica using "${ces}\ces_aluno_matching_`year'", keep(1 3)  nogenerate
rename (co_aluno-carreira) =_`year'
}

* Verificar duplicações da chave única
duplicates tag chave_unica, gen(dupchave)
tab dupchave
******************************

*** Definir qual é a edição de ingresso de cada indivíduo
gen ingresso = 0
gen ano_ingresso = .
replace ano_ingresso = 2017 if co_aluno_17 != .
replace ano_ingresso = 2016 if co_aluno_16 != .
replace ano_ingresso = 2015 if co_aluno_15 != .
replace ano_ingresso = 2014 if co_aluno_14 != .
replace ano_ingresso = 2013 if co_aluno_13 != .
replace ingresso = 1 if ano_ingresso != .
******************************

*** Definir de qual edição do Enem se vai usar
* Ajustando a variável de nota média
su media12-media16 /* Há casos com nota zero */
recode media12-media16 (0=.)
foreach year in "12" "13" "14" "15" "16" {
replace participante`year' = 0 if media`year'==. & inscrito`year'==1
}

* Definir qual é o Enem anterior ao ingresso
generate enemingresso = .
replace enemingresso = 2012 if ano_ingresso == 2013 & participante12 == 1
replace enemingresso = 2013 if ano_ingresso == 2014 & participante13 == 1
replace enemingresso = 2014 if ano_ingresso == 2015 & participante14 == 1
replace enemingresso = 2015 if ano_ingresso == 2016 & participante15 == 1
replace enemingresso = 2016 if ano_ingresso == 2017 & participante16 == 1

* Também definir qual é o último Enem
generate ultimoenem = .
replace ultimoenem = 2016 if participante16 == 1
replace ultimoenem = 2015 if ultimoenem == . & participante15 == 1
replace ultimoenem = 2014 if ultimoenem == . & participante14 == 1
replace ultimoenem = 2013 if ultimoenem == . & participante13 == 1
replace ultimoenem = 2012 if ultimoenem == . & participante12 == 1

* Para quem tem um Enem anterior ao ingresso, será este;
* Para quem não tem, será o último Enem
generate ano_enem = enemingresso
replace ano_enem = 2016 if enemingresso == . & ultimoenem == 2016
replace ano_enem = 2015 if enemingresso == . & ultimoenem == 2015
replace ano_enem = 2014 if enemingresso == . & ultimoenem == 2014
replace ano_enem = 2013 if enemingresso == . & ultimoenem == 2013
replace ano_enem = 2012 if enemingresso == . & ultimoenem == 2012
drop enemingresso ultimoenem
******************************

*** Organizando dados do Enem 2012-2016

* Atribuindo as variáveis do Enem para cada indivíduo
rename (escp12_v1 escp13_v1 escp14_v1 escp15_v1 escp16_v1) ///
(escp_v112 escp_v113 escp_v114 escp_v115 escp_v116)

local varlist "rfpc media fundprivado escp trabalho"
foreach var of local varlist {
	gen `var' = .
    replace `var' = `var'12 if ano_enem == 2012
	replace `var' = `var'13 if ano_enem == 2013
	replace `var' = `var'14 if ano_enem == 2014
	replace `var' = `var'15 if ano_enem == 2015
	replace `var' = `var'16 if ano_enem == 2016
	replace `var' = . if inscrito == . | inscrito == 0
}

******************************

*** Limpando as variáveis do CES
* Uma vez definido o ano de ingresso, trazer as informações do ano selecionado para uma única variável para cada ano

local varlist "co_aluno categ ies grau turno curso sit"
foreach var of local varlist {
	gen `var' = .
    replace `var' = `var'_13 if ano_ingresso == 2013
	replace `var' = `var'_14 if ano_ingresso == 2014
	replace `var' = `var'_15 if ano_ingresso == 2015
	replace `var' = `var'_16 if ano_ingresso == 2016
	replace `var' = `var'_17 if ano_ingresso == 2017
}
format co_aluno %12.0f

* Remover as variáveis de cada ano
drop co_aluno_13-sit_17

* Remover a variável CO_ALUNO por não ser um registro único
drop co_aluno

* Eliminar duplicações de linhas pós-limpeza
drop dupchave
duplicates drop
duplicates tag chave_unica, gen(dupcpf)
tab dupchave
******************************

*** Organizar a base de dados
* Ordenar variáveis
order(ano_enem escp rfpc trabalho fundprivado media), after(participante)

* Renomear e recodificar variáveis
rename (homem ppi) (sexo raca)
recode trabalho (2=1)

* Atribuir rótulos de variáveis
label var uf "UF da Escola"
label var municipio "Município da Escola"
label var inscrito "Inscrito Enem"
label var participante "Participante Enem"
label var ano_enem "Ano participação Enem"
label var media "Média Enem"
label var rfpc "Renda familiar Enem"
label var fundprivado "EF privado Enem"
label var escp "Esc parental Enem"
label var trabalho "Exercício de atividade remunerada"
label var ies "Código da IES"
label var no_ies "Nome da IES"
label var categ "Categoria Administrativa"
label var grau "Grau Acadêmico"
label var sit "Situação de Matrícula"
label var turno "Turno"
label var curso "Curso"
label var ingresso "Ingresso"
label var ano_ingresso "Ano do Ingresso"

* Atribuir rótulos de valores
lab val ingresso binary
lab val categ categ
lab val grau grau
lab val turno turno
lab val sit sit
lab val uf_curso uf
label define escp 1 "Não estudou ou EF1 incompleto" 2 "EF1 completo" 3 "EF2 completo" 4 "Médio completo" 5 "Superior completo" 6 "Pós-Graduação"
label define sexo 0 "Feminino" 1 "Masculino"
label define cor_raca 0 "Branca/Amarela" 1 "Preta/Parda/Indígena"
label values sexo sexo
label values raca cor_raca
label values escp escp
lab val (fundprivado trabalho) binary
******************************

*** Preparar o filtro para individualizar os registros
* Visualizar duplicações
duplicates tag chave_unica, gen(dupchave)
tab dupchave

*** Criar um filtro ALEATÓRIO, selecionado por acaso um vínculo por indivíduo
set seed 290689
gen r = uniform()
gen filtro_aleatorio = 0
bysort id (r): replace filtro_aleatorio = 1 if _n==1
tab filtro_aleatorio

*** Organizar variáveis de filtro
lab var filtro_aleatorio "Filtro de vínculos aleatório"
lab val filtro_aleatorio binary
******************************

*** Com isso, temos uma base pronta para ser analisada. Para replicar os dados
* apresentados neste artigo, no entanto, vamos aplicar novos tratamentos à base 
* de dados, mantendo apenas as variáveis que utilizamos e filtrando somente os
* casos necessários para se alcançar o mesmo quantitativo.

*** Ao final, exportaremos uma base para .csv, de modo que possamos abrir em
* R e seguir com as análises descritivas e a modelagem nos próximos scripts.

*** Filtrar a base
keep if idade >= 16 & idade <= 22 /* Idade entre 16 e 22 anos */
keep if filtro_aleatorio == 1 /* Filtro para individualizar vínculos */

*** Gerar variável dependente
tab participante ingresso, m nol
gen dep_var=.
replace dep_var=0 if participante==1 & ingresso==0
replace dep_var=1 if participante==1 & ingresso==1
tab dep_var

replace dep_var = 1 if categ == 1
replace dep_var = 2 if categ == 2
label var dep_var "Variável dependente"
label def dep_var 0 "Não ingresso" 1 "Pública" 2 "Privada"
label val dep_var dep_var

*** Recodificar variáveis
recode escp (1/2=0) (3=3) (4/5=4), gen(origem_esc)
label var origem_esc "Esc parental Enem"
label def eduparen 0 "Até o Fundamental" 3 "Médio" 4 "Superior ou pós"
label val origem_esc eduparen
rename raca cor
rename media notas
rename (ies turno curso) (co_ies co_turno co_curso)

*** Marcar observações sem missing e reduzir tamanho da base
marktouse use dep_var sexo cor idade rfpc origem_esc fundprivado trabalho notas depadm rural profissional uf
keep if use == 1

*** Excluir variáveis desnecessárias para a análise
drop regiao municipio escola escp fundprivado trabalho profissional inscrito participante ingresso duchave-filtro_aleatorio ln_renda use categ

*** Ajustes finais
order dep_var ano_ingresso ano_enem rfpc notas sexo cor idade origem_esc depadm rural, after(id)
rename notas desempenho
rename origem_esc edupar

*** Exportar
export delimited using "${output}\base_para_abrir_em_r.csv", delimiter(";") replace

* Fim da sintaxe
***