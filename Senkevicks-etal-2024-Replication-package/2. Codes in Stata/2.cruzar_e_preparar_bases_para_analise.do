
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

********* Conversão para o R, por TMancano ************

library(tidyverse)
library(haven) # Para ler arquivos .dta se necessário

# Definir diretórios
data_dir <- "inserir caminho das bases do CEB"
ces_dir <- "inserir caminho das bases do CES"
output_dir <- "inserir caminho dos produtos"

# Abrir base de egressos 2012
ceb_egressos <- read_rds(file.path(data_dir, "ceb_egressos_2012.rds")) %>% 
  filter(!is.na(chave_unica))

###########################################################
# Cruzamento e limpeza com CES em menor nível de agregação

# Lista de anos para processar
anos_ces <- c("13", "14", "15", "16", "17")

# Função para carregar e mesclar dados CES
carregar_e_mesclar <- function(ano) {
  read_rds(file.path(ces_dir, str_glue("ces_aluno_matching_{ano}.rds"))) %>% 
    select(-any_of("dupchave")) %>% # Remover coluna de duplicatas se existir
    rename_with(~ paste0(., "_", ano), everything())
}

# Mesclar todas as bases CES
dados_completos <- anos_ces %>% 
  map(carregar_e_mesclar) %>% 
  reduce(full_join, by = "chave_unica") %>% 
  right_join(ceb_egressos, by = "chave_unica")

# Verificar duplicações
dados_completos <- dados_completos %>% 
  add_count(chave_unica, name = "dupchave") %>% 
  mutate(dupchave = dupchave - 1) # Ajuste para contar duplicatas

###########################################################
# Definir ano de ingresso

dados_completos <- dados_completos %>% 
  mutate(
    ingresso = if_else(
      !is.na(co_aluno_13) | !is.na(co_aluno_14) | !is.na(co_aluno_15) | 
        !is.na(co_aluno_16) | !is.na(co_aluno_17), 
      1, 0
    ),
    ano_ingresso = case_when(
      !is.na(co_aluno_17) ~ 2017,
      !is.na(co_aluno_16) ~ 2016,
      !is.na(co_aluno_15) ~ 2015,
      !is.na(co_aluno_14) ~ 2014,
      !is.na(co_aluno_13) ~ 2013,
      TRUE ~ NA_real_
    )
  )

###########################################################
# Definir qual edição do Enem usar

# Ajustar notas zero para NA
dados_completos <- dados_completos %>% 
  mutate(across(starts_with("media"), ~ na_if(., 0)) %>% 
  mutate(across(starts_with("participante"), 
         ~ if_else(is.na(get(str_replace(cur_column(), "participante", "media"))) & 
           get(str_replace(cur_column(), "participante", "inscrito")) == 1, 
         0, .))

# Definir Enem anterior ao ingresso
dados_completos <- dados_completos %>% 
  mutate(
    enemingresso = case_when(
      ano_ingresso == 2013 & participante12 == 1 ~ 2012,
      ano_ingresso == 2014 & participante13 == 1 ~ 2013,
      ano_ingresso == 2015 & participante14 == 1 ~ 2014,
      ano_ingresso == 2016 & participante15 == 1 ~ 2015,
      ano_ingresso == 2017 & participante16 == 1 ~ 2016,
      TRUE ~ NA_real_
    ),
    ultimoenem = case_when(
      participante16 == 1 ~ 2016,
      participante15 == 1 ~ 2015,
      participante14 == 1 ~ 2014,
      participante13 == 1 ~ 2013,
      participante12 == 1 ~ 2012,
      TRUE ~ NA_real_
    ),
    ano_enem = coalesce(enemingresso, ultimoenem)
  ) %>% 
  select(-enemingresso, -ultimoenem)

###########################################################
# Organizar dados do Enem 2012-2016

# Renomear variáveis de escolaridade parental
dados_completos <- dados_completos %>% 
  rename_with(~ str_replace(., "escp(\\d{2})_v1", "escp_v1\\1"), 
              matches("escp\\d{2}_v1"))

# Selecionar variáveis do Enem baseado no ano_enem
variaveis_enem <- c("rfpc", "media", "fundprivado", "escp", "trabalho")

for (var in variaveis_enem) {
  dados_completos <- dados_completos %>% 
    mutate(
      !!var := case_when(
        ano_enem == 2012 ~ get(paste0(var, "12")),
        ano_enem == 2013 ~ get(paste0(var, "13")),
        ano_enem == 2014 ~ get(paste0(var, "14")),
        ano_enem == 2015 ~ get(paste0(var, "15")),
        ano_enem == 2016 ~ get(paste0(var, "16")),
        TRUE ~ NA_real_
      ),
      !!var := if_else(is.na(inscrito) | inscrito == 0, NA_real_, get(var))
    )
}

###########################################################
# Limpar variáveis do CES

variaveis_ces <- c("co_aluno", "categ", "ies", "grau", "turno", "curso", "sit")

for (var in variaveis_ces) {
  dados_completos <- dados_completos %>% 
    mutate(
      !!var := case_when(
        ano_ingresso == 2013 ~ get(paste0(var, "_13")),
        ano_ingresso == 2014 ~ get(paste0(var, "_14")),
        ano_ingresso == 2015 ~ get(paste0(var, "_15")),
        ano_ingresso == 2016 ~ get(paste0(var, "_16")),
        ano_ingresso == 2017 ~ get(paste0(var, "_17")),
        TRUE ~ NA_real_
      )
    )
}

# Remover variáveis por ano e co_aluno
dados_completos <- dados_completos %>% 
  select(-matches("_(13|14|15|16|17)$"), -co_aluno) %>% 
  distinct() %>% 
  add_count(chave_unica, name = "dupcpf") %>% 
  mutate(dupcpf = dupcpf - 1)

###########################################################
# Organizar a base de dados

# Renomear e recodificar variáveis
dados_completos <- dados_completos %>% 
  rename(
    sexo = homem,
    raca = ppi
  ) %>% 
  mutate(
    trabalho = recode(trabalho, `2` = 1),
    escp = factor(escp, 
                 levels = 1:6,
                 labels = c("Não estudou ou EF1 incompleto", "EF1 completo",
                            "EF2 completo", "Médio completo", 
                            "Superior completo", "Pós-Graduação")),
    sexo = factor(sexo, levels = 0:1, labels = c("Feminino", "Masculino")),
    raca = factor(raca, levels = 0:1, 
                 labels = c("Branca/Amarela", "Preta/Parda/Indígena")),
    across(c(fundprivado, trabalho), ~ factor(., levels = 0:1, labels = c("Não", "Sim")))
  )

# Criar filtro aleatório
set.seed(290689)
dados_completos <- dados_completos %>% 
  group_by(chave_unica) %>% 
  mutate(
    r = runif(n()),
    filtro_aleatorio = if_else(row_number() == 1, 1L, 0L)
  ) %>% 
  ungroup()

###########################################################
# Filtrar e preparar base final

base_final <- dados_completos %>% 
  filter(
    between(idade, 16, 22),
    filtro_aleatorio == 1
  ) %>% 
  mutate(
    dep_var = case_when(
      participante == 1 & ingresso == 0 ~ 0L,
      participante == 1 & ingresso == 1 & categ == "Pública" ~ 1L,
      participante == 1 & ingresso == 1 & categ == "Privada" ~ 2L,
      TRUE ~ NA_integer_
    ),
    dep_var = factor(dep_var, 
                    levels = 0:2, 
                    labels = c("Não ingresso", "Pública", "Privada")),
    origem_esc = recode(escp,
                       "Não estudou ou EF1 incompleto" = 0,
                       "EF1 completo" = 0,
                       "EF2 completo" = 3,
                       "Médio completo" = 4,
                       "Superior completo" = 4,
                       "Pós-Graduação" = 4),
    origem_esc = factor(origem_esc,
                       levels = c(0, 3, 4),
                       labels = c("Até o Fundamental", "Médio", "Superior ou pós"))
  ) %>% 
  rename(
    cor = raca,
    desempenho = media,
    edupar = origem_esc,
    co_ies = ies,
    co_turno = turno,
    co_curso = curso
  ) %>% 
  select(
    chave_unica, dep_var, ano_ingresso, ano_enem, rfpc, desempenho,
    sexo, cor, idade, edupar, depadm, rural, everything(),
    -c(regiao, municipio, escola, fundprivado, trabalho, profissional,
       inscrito, participante, ingresso, dupchave, dupcpf, r, filtro_aleatorio,
       starts_with("ln_"), escp)
  ) %>% 
  filter(!is.na(dep_var)) # Remover casos missing na variável dependente

# Exportar para CSV
write_csv2(base_final, file.path(output_dir, "base_para_abrir_em_r.csv"))

# Salvar também em RDS para uso posterior
write_rds(base_final, file.path(output_dir, "base_final.rds"))
