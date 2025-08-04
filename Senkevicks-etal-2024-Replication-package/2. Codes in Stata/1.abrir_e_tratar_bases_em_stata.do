
/* Neste sintaxe, importarei os microdados identificados do Censo da Educação Básica, do Enem e do Censo da Educação Superior de CSV para Stata, aplicando alguns tratamentos iniciais, e salvando em .dta para posterior tratamento e cruzamento */

#delimit cr
version 16.1
clear
set more off
set linesize 120
capture log close

*** Definir diretório
global data "inserir o caminho das bases"

******************************
******************************
*** CENSO ESCOLAR + ENEM

* Importar base de dados
quietly import delimited "${data}\censo_enem.csv", clear

* Para este artigo, vamos trabalhar exclusivamente com egressos do ensino médio de 2012
keep if nu_ano_censo==2012

* Usar a cor/raça do Enem e complementar, se necessário, com Censo
gen raca = iein_co_cor_raca 
replace raca = tp_cor_raca if iein_co_cor_raca == "." | iein_co_cor_raca == "6"
drop tp_cor_raca iein_co_cor_raca

* Calcular a média nas quatro provas objetivas do Enem
egen media = rowmean(iere_vl_nota_ch iere_vl_nota_cn iere_vl_nota_lc iere_vl_nota_mt)
drop iere_vl_nota_ch iere_vl_nota_cn iere_vl_nota_lc iere_vl_nota_mt

* Calcular a renda domiciliar per capita
recode iequ_questao_06 ("A"=0) ("B"=1) ("C"=1.25) ("D"=1.75) ("E"=2.25) ///
                ("F"=2.75) ("G"=3.5) ("H"=4.5) ("I"=5.5) ("J"=6.5) ("K"=7.5) ///
				("L"=8.5) ("M"=9.5) ("N"=11) ("O"=13.5) ("P"=17.5) ("Q"=20), ///
				gen(pm_sm)
gen rfpc = pm_sm / iequ_questao_05
drop iequ_questao_05 iequ_questao_06

* Computar a maior escolaridade parental, entre mae e pai
rename iequ_questao_01 edupai
rename iequ_questao_02 edumae
gen escp = .
replace escp = edumae if edumae >= edupai
replace escp = edupai if edumae < edupai
drop edupai edumae

rename iequ_questao_26 trabalho
rename iequ_questao_42 fundprivado
rename iein_nu_ano ano_enem
drop iein_co_inscricao iequ_questao_47

* Salvar base de egressos do ensino médio com dados do Enem
save "${data}\ceb_egressos_2012", replace

******************************
******************************
*** CENSO DA EDUCAÇÃO SUPERIOR

****************
*** CES 2013

* Importar base de dados
quietly import delimited "${data}\aluno_13.csv", clear
format co_aluno %12.0f
foreach v of varlist * {
   label var `v' ""
   }
label data "Censo da Educação Superior 2013"

* Exclui os cursos sequenciais de formação específica
drop if tp_nivel_academico == 2

* Eliminar variáveis desnecessárias
drop co_aluno_curso tp_cor_raca tp_organizacao_academica ///
tp_nivel_academico co_area_geral co_area_especifica ///
co_area_detalhada nu_ano_censo

* Recodificar variáveis
rename co_rotulo co_ocde
recode tp_categoria_administrativa (1/3=1) (4/9=2)
gen turno = tp_turno
recode turno (1=1) (2=1) (3=2) (4=1)
replace turno = 3 if turno == . & tp_modalidade_ensino == 2
drop tp_turno tp_modalidade_ensino
lab var co_aluno "Código de Aluno"
lab var chave_unica "Chave Única"
lab var co_ies "Código de IES"
lab var tp_categoria_administrativa "Categoria Administrativa"
lab var tp_grau_academico "Grau Acadêmico"
recode tp_grau_academico (.=0)
lab var co_ocde "Código OCDE"
lab var tp_situacao "Situação do Aluno"
lab var turno "Turno"
lab var co_curso "Curso"
rename (co_ies tp_categoria_administrativa tp_grau_academico tp_situacao co_curso) (ies categ grau sit curso)
order co_aluno chave_unica categ ies grau turno curso co_ocde sit
lab def categ 1 "Pública" 2 "Privada"
lab def grau 0 "NA" 1 "Bacharelado" 2 "Licenciatura" 3 "Tecnológico"
lab def turno 1 "Diurno" 2 "Noturno" 3 "EaD"
lab def sit 2 "Cursando" 3 "Matrícula trancada" 4 "Desvinculado do curso" 5 "Transferido para curso da mesma IES" 6 "Formado" 7 "Falecido"
lab val categ categ
lab val grau grau
lab val turno turno
lab val sit sit

* Salvar base de dados
save "${data}\ces_aluno_13.dta", replace


****************
*** CES 2014

* Importar base de dados
quietly import delimited "${data}\aluno_14.csv", clear
format co_aluno %12.0f
foreach v of varlist * {
   label var `v' ""
   }
label data "Censo da Educação Superior 2014" 

* Exclui os cursos sequenciais de formação específica
drop if tp_nivel_academico == 2

* Eliminar variáveis desnecessárias
drop co_aluno_curso tp_cor_raca tp_organizacao_academica ///
tp_nivel_academico co_area_geral co_area_especifica ///
co_area_detalhada nu_ano_censo

* Recodificar variáveis
rename co_rotulo co_ocde
recode tp_categoria_administrativa (1/3=1) (4/9=2)
gen turno = tp_turno
recode turno (1=1) (2=1) (3=2) (4=1)
replace turno = 3 if turno == . & tp_modalidade_ensino == 2
drop tp_turno tp_modalidade_ensino
lab var co_aluno "Código de Aluno"
lab var chave_unica "Chave Única"
lab var co_ies "Código de IES"
lab var tp_categoria_administrativa "Categoria Administrativa"
lab var tp_grau_academico "Grau Acadêmico"
recode tp_grau_academico (.=0)
lab var co_ocde "Código OCDE"
lab var tp_situacao "Situação do Aluno"
lab var turno "Turno"
lab var co_curso "Curso"
rename (co_ies tp_categoria_administrativa tp_grau_academico tp_situacao co_curso) (ies categ grau sit curso)
order co_aluno chave_unica categ ies grau turno curso co_ocde sit
lab def categ 1 "Pública" 2 "Privada"
lab def grau 0 "NA" 1 "Bacharelado" 2 "Licenciatura" 3 "Tecnológico"
lab def turno 1 "Diurno" 2 "Noturno" 3 "EaD"
lab def sit 2 "Cursando" 3 "Matrícula trancada" 4 "Desvinculado do curso" 5 "Transferido para curso da mesma IES" 6 "Formado" 7 "Falecido"
lab val categ categ
lab val grau grau
lab val turno turno
lab val sit sit

* Salvar base de dados
save "${data}\ces_aluno_14.dta", replace


****************
*** CES 2015

* Importar base de dados
quietly import delimited "${data}\aluno_15.csv", clear
format co_aluno %12.0f
foreach v of varlist * {
   label var `v' ""
   }
label data "Censo da Educação Superior 2015" 

* Exclui os cursos sequenciais de formação específica
drop if tp_nivel_academico == 2

* Eliminar variáveis desnecessárias
drop co_aluno_curso tp_cor_raca tp_organizacao_academica ///
tp_nivel_academico co_area_geral co_area_especifica ///
co_area_detalhada nu_ano_censo

* Recodificar variáveis
rename co_rotulo co_ocde
recode tp_categoria_administrativa (1/3=1) (4/9=2)
gen turno = tp_turno
recode turno (1=1) (2=1) (3=2) (4=1)
replace turno = 3 if turno == . & tp_modalidade_ensino == 2
drop tp_turno tp_modalidade_ensino
lab var co_aluno "Código de Aluno"
lab var chave_unica "Chave Única"
lab var co_ies "Código de IES"
lab var tp_categoria_administrativa "Categoria Administrativa"
lab var tp_grau_academico "Grau Acadêmico"
recode tp_grau_academico (.=0)
lab var co_ocde "Código OCDE"
lab var tp_situacao "Situação do Aluno"
lab var turno "Turno"
lab var co_curso "Curso"
rename (co_ies tp_categoria_administrativa tp_grau_academico tp_situacao co_curso) (ies categ grau sit curso)
order co_aluno chave_unica categ ies grau turno curso co_ocde sit
lab def categ 1 "Pública" 2 "Privada"
lab def grau 0 "NA" 1 "Bacharelado" 2 "Licenciatura" 3 "Tecnológico"
lab def turno 1 "Diurno" 2 "Noturno" 3 "EaD"
lab def sit 2 "Cursando" 3 "Matrícula trancada" 4 "Desvinculado do curso" 5 "Transferido para curso da mesma IES" 6 "Formado" 7 "Falecido"
lab val categ categ
lab val grau grau
lab val turno turno
lab val sit sit

* Salvar base de dados
save "${data}\ces_aluno_15.dta", replace


****************
*** CES 2016

* Importar base de dados
quietly import delimited "${data}\aluno_16.csv", clear
format co_aluno %12.0f
foreach v of varlist * {
   label var `v' ""
   }
label data "Censo da Educação Superior 2016" 

* Exclui os cursos sequenciais de formação específica
drop if tp_nivel_academico == 2

* Eliminar variáveis desnecessárias
drop co_aluno_curso tp_cor_raca tp_organizacao_academica ///
tp_nivel_academico co_area_geral co_area_especifica ///
co_area_detalhada nu_ano_censo

* Recodificar variáveis
rename co_rotulo co_ocde
recode tp_categoria_administrativa (1/3=1) (4/9=2)
gen turno = tp_turno
recode turno (1=1) (2=1) (3=2) (4=1)
replace turno = 3 if turno == . & tp_modalidade_ensino == 2
drop tp_turno tp_modalidade_ensino
lab var co_aluno "Código de Aluno"
lab var chave_unica "Chave Única"
lab var co_ies "Código de IES"
lab var tp_categoria_administrativa "Categoria Administrativa"
lab var tp_grau_academico "Grau Acadêmico"
recode tp_grau_academico (.=0)
lab var co_ocde "Código OCDE"
lab var tp_situacao "Situação do Aluno"
lab var turno "Turno"
lab var co_curso "Curso"
rename (co_ies tp_categoria_administrativa tp_grau_academico tp_situacao co_curso) (ies categ grau sit curso)
order co_aluno chave_unica categ ies grau turno curso co_ocde sit
lab def categ 1 "Pública" 2 "Privada"
lab def grau 0 "NA" 1 "Bacharelado" 2 "Licenciatura" 3 "Tecnológico"
lab def turno 1 "Diurno" 2 "Noturno" 3 "EaD"
lab def sit 2 "Cursando" 3 "Matrícula trancada" 4 "Desvinculado do curso" 5 "Transferido para curso da mesma IES" 6 "Formado" 7 "Falecido"
lab val categ categ
lab val grau grau
lab val turno turno
lab val sit sit

* Salvar base de dados
save "${data}\ces_aluno_16.dta", replace


****************
*** CES 2017

* Importar base de dados
quietly import delimited "${data}\aluno_17.csv", clear
format co_aluno %12.0f
foreach v of varlist * {
   label var `v' ""
   }
label data "Censo da Educação Superior 2017" 

* Exclui os cursos sequenciais de formação específica
drop if tp_nivel_academico == 2

* Eliminar variáveis desnecessárias
drop co_aluno_curso tp_cor_raca tp_organizacao_academica ///
tp_nivel_academico co_area_geral co_area_especifica ///
co_area_detalhada nu_ano_censo

* Recodificar variáveis
rename co_rotulo co_ocde
recode tp_categoria_administrativa (1/3=1) (4/9=2)
gen turno = tp_turno
recode turno (1=1) (2=1) (3=2) (4=1)
replace turno = 3 if turno == . & tp_modalidade_ensino == 2
drop tp_turno tp_modalidade_ensino
lab var co_aluno "Código de Aluno"
lab var chave_unica "Chave Única"
lab var co_ies "Código de IES"
lab var tp_categoria_administrativa "Categoria Administrativa"
lab var tp_grau_academico "Grau Acadêmico"
recode tp_grau_academico (.=0)
lab var co_ocde "Código OCDE"
lab var tp_situacao "Situação do Aluno"
lab var turno "Turno"
lab var co_curso "Curso"
rename (co_ies tp_categoria_administrativa tp_grau_academico tp_situacao co_curso) (ies categ grau sit curso)
order co_aluno chave_unica categ ies grau turno curso co_ocde sit
lab def categ 1 "Pública" 2 "Privada"
lab def grau 0 "NA" 1 "Bacharelado" 2 "Licenciatura" 3 "Tecnológico"
lab def turno 1 "Diurno" 2 "Noturno" 3 "EaD"
lab def sit 2 "Cursando" 3 "Matrícula trancada" 4 "Desvinculado do curso" 5 "Transferido para curso da mesma IES" 6 "Formado" 7 "Falecido"
lab val categ categ
lab val grau grau
lab val turno turno
lab val sit sit

* Salvar base de dados
save "${data}\ces_aluno_17.dta", replace

********************************************
*** LOOP PARA TRATAMENTO DAS BASES 2013-2017

foreach year in "13" "14" "15" "16" "17" {

*** Abrir base de dados
use "${data}\aluno_`year'.dta", clear
drop if chave_unica == ""
sort co_ocde

*** Reconstruir variável de situação de matrícula
label list sit
recode sit (2=1) (6=2) (7=6)
label def sit 1 "Cursando" 2 "Formado" 3 "Matrícula trancada" 4 "Desvinculado do curso" 5 "Transferido para curso da mesma IES" 6 "Falecido", replace
tab sit, m

*** Analisar as duplicações de código de aluno 
sort co_aluno sit
duplicates tag co_aluno, gen(dupid)
tab dupid

*** Eliminar duplicações completas das variáveis importantes:
duplicates drop co_aluno ies grau turno curso sit, force

drop dupid
duplicates tag co_aluno, gen(dupid)
tab dupid

*** Salvar bases de dados
drop dupid
order(carreira), last
save "${output}\2.Bases_finais\ces_aluno_matching_`year'",replace

}

* Fim
***

********* Conversão para o R, por TMancano ************

# Configuração inicial
rm(list = ls()) # Limpar ambiente
options(width = 120) # Definir largura do output
set.seed(123) # Para reprodutibilidade

# Definir diretório (substitua pelo seu caminho)
data_dir <- "inserir_o_caminho_das_bases"

##############################
### CENSO ESCOLAR + ENEM ###

# Importar base de dados
library(data.table)
censo_enem <- fread(file.path(data_dir, "censo_enem.csv"))

# Manter apenas egressos do ensino médio de 2012
censo_enem <- censo_enem[nu_ano_censo == 2012, ]

# Usar a cor/raça do Enem e complementar com Censo se necessário
censo_enem[, raca := iein_co_cor_raca]
censo_enem[is.na(iein_co_cor_raca) | iein_co_cor_raca == 6, raca := tp_cor_raca]
censo_enem[, c("tp_cor_raca", "iein_co_cor_raca") := NULL]

# Calcular média nas quatro provas objetivas do Enem
provas <- c("iere_vl_nota_ch", "iere_vl_nota_cn", "iere_vl_nota_lc", "iere_vl_nota_mt")
censo_enem[, media := rowMeans(.SD, na.rm = TRUE), .SDcols = provas]
censo_enem[, (provas) := NULL]

# Calcular renda domiciliar per capita
renda_map <- c("A"=0, "B"=1, "C"=1.25, "D"=1.75, "E"=2.25, "F"=2.75, 
               "G"=3.5, "H"=4.5, "I"=5.5, "J"=6.5, "K"=7.5, "L"=8.5, 
               "M"=9.5, "N"=11, "O"=13.5, "P"=17.5, "Q"=20)

censo_enem[, pm_sm := renda_map[iequ_questao_06]]
censo_enem[, rfpc := pm_sm / iequ_questao_05]
censo_enem[, c("iequ_questao_05", "iequ_questao_06") := NULL]

# Computar maior escolaridade parental
censo_enem[, escp := pmax(iequ_questao_01, iequ_questao_02, na.rm = TRUE)]
censo_enem[, c("iequ_questao_01", "iequ_questao_02") := NULL]

# Renomear outras variáveis
setnames(censo_enem, 
         c("iequ_questao_26", "iequ_questao_42", "iein_nu_ano"),
         c("trabalho", "fundprivado", "ano_enem"))

# Remover variáveis não utilizadas
censo_enem[, c("iein_co_inscricao", "iequ_questao_47") := NULL]

# Salvar base
saveRDS(censo_enem, file.path(data_dir, "ceb_egressos_2012.rds"))

##############################
### CENSO DA EDUCAÇÃO SUPERIOR ###

# Função para processar cada ano do CES
processar_ces <- function(ano, data_dir) {
  # Importar dados
  arquivo <- sprintf("aluno_%s.csv", ano)
  dados <- fread(file.path(data_dir, arquivo))
  
  # Formatar código do aluno
  dados[, co_aluno := as.character(co_aluno)]
  
  # Excluir cursos sequenciais de formação específica
  dados <- dados[tp_nivel_academico != 2, ]
  
  # Eliminar variáveis desnecessárias
  vars_remover <- c("co_aluno_curso", "tp_cor_raca", "tp_organizacao_academica",
                    "tp_nivel_academico", "co_area_geral", "co_area_especifica",
                    "co_area_detalhada", "nu_ano_censo")
  dados[, (vars_remover) := NULL]
  
  # Recodificar variáveis
  setnames(dados, "co_rotulo", "co_ocde")
  
  # Categoria administrativa
  dados[, categ := ifelse(tp_categoria_administrativa %in% 1:3, 1, 2)]
  
  # Turno
  dados[, turno := tp_turno]
  dados[tp_turno %in% 1:2, turno := 1]
  dados[tp_turno == 3, turno := 2]
  dados[tp_turno == 4, turno := 1]
  dados[is.na(turno) & tp_modalidade_ensino == 2, turno := 3]
  dados[, c("tp_turno", "tp_modalidade_ensino") := NULL]
  
  # Grau acadêmico
  dados[is.na(tp_grau_academico), tp_grau_academico := 0]
  
  # Renomear variáveis
  setnames(dados, 
           c("co_ies", "tp_grau_academico", "tp_situacao", "co_curso"),
           c("ies", "grau", "sit", "curso"))
  
  # Definir labels (fatores)
  dados[, categ := factor(categ, levels = 1:2, labels = c("Pública", "Privada"))]
  dados[, grau := factor(grau, levels = 0:3, 
                         labels = c("NA", "Bacharelado", "Licenciatura", "Tecnológico"))]
  dados[, turno := factor(turno, levels = 1:3, 
                          labels = c("Diurno", "Noturno", "EaD"))]
  dados[, sit := factor(sit, levels = 2:7,
                        labels = c("Cursando", "Matrícula trancada", "Desvinculado do curso",
                                   "Transferido para curso da mesma IES", "Formado", "Falecido"))]
  
  # Reordenar colunas
  setcolorder(dados, c("co_aluno", "chave_unica", "categ", "ies", "grau", 
                       "turno", "curso", "co_ocde", "sit"))
  
  # Salvar base
  output_file <- sprintf("ces_aluno_%s.rds", ano)
  saveRDS(dados, file.path(data_dir, output_file))
  
  return(dados)
}

# Processar cada ano
for (ano in c("13", "14", "15", "16", "17")) {
  assign(paste0("ces_", ano), processar_ces(ano, data_dir))
}

################################
### LOOP PARA TRATAMENTO DAS BASES 2013-2017 ###

for (ano in c("13", "14", "15", "16", "17")) {
  # Carregar dados
  dados <- get(paste0("ces_", ano))
  
  # Reconstruir variável de situação de matrícula
  levels(dados$sit) <- list("Cursando" = "Cursando", 
                            "Formado" = "Formado",
                            "Matrícula trancada" = "Matrícula trancada",
                            "Desvinculado do curso" = "Desvinculado do curso",
                            "Transferido para curso da mesma IES" = "Transferido para curso da mesma IES",
                            "Falecido" = "Falecido")
  
  # Analisar duplicações
  setkey(dados, co_aluno)
  dup_count <- duplicated(dados, by = "co_aluno")
  cat("Duplicações em", ano, ":", sum(dup_count), "\n")
  
  # Eliminar duplicações completas
  unique_cols <- c("co_aluno", "ies", "grau", "turno", "curso", "sit")
  dados <- unique(dados, by = unique_cols)
  
  # Verificar duplicações novamente
  dup_count <- duplicated(dados, by = "co_aluno")
  cat("Duplicações após limpeza em", ano, ":", sum(dup_count), "\n")
  
  # Salvar bases finais
  output_file <- sprintf("ces_aluno_matching_%s.rds", ano)
  saveRDS(dados, file.path(data_dir, "2.Bases_finais", output_file))
}

