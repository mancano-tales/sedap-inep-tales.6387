---
title: "tales.6387_0_Data_Load_&_Prep"
format:
  html:
    code-tools:
      source: true
      toggle: true
      collapse: false
      copy: true
editor: 
  markdown: 
    wrap: 72
---

Este script tem como objetivo acompanhar, testar e documentar, do lado
de fora da sala segura do INEP, o desenvolvimento do código que está
sendo elaborado internamente.

A intenção é que, caso tudo transcorra como previsto, possamos extrair o
script final — atualmente sendo desenvolvido em um documento Quarto
dentro da sala — para uso e replicação externa.

Além disso, este documento funciona como um caderno de campo: serve
tanto para preparar materiais a serem utilizados dentro da sala quanto
para registrar os avanços realizados no ambiente restrito, facilitando o
acompanhamento e a reconstrução do trabalho fora dele.

# Início do Script

O Script se inicia na sala do Sedap com: 1. Definição do Diretório de
Trabalho (especialmente importante pois lá não é possível usar Rproject
e temos que salvar os arquivos em pastas específicas ou ele pode ser
deletado). Os dados ficam disponíveis via acesso na rede em disco de
nome "T:/"

Depis demos que verificar se os pacotes necessário s do tidyverse foram
instalados, se não é imporssível ler os bancos de dados CSV

```{R}
getwd()
setwd("T:/")
installed.packages()
```

Os pacotes na sala do INEP não podem ser baixados via CRAN, ao invés
disso eles são disponibilziados em arquivos diretamente no computador,
que, no entanto, ainda não tive acesso.

```{R}

#Código de instalação dos pacotes no R do Sedap

#Como não é o caso aqui, vou chamar os pacotes que utilizo normalmente
library(tidyverse)

```

Depois de instalar os pacotes podemos finalmente ler as bases de dados
usando a função readr

```{R}
ENEM_2010 <- readr::read_delim("1. Bases Enem\ENEM_2010.csv", delim=";", n_max = 0)
colnames()
#Essa forma de leitura se repete para todas as outras bases
```

## Tratamento e cruzamento das bases de dados

Após realizar a leitura da primeira base de dados, seja do Censo da
Educação Básica (doravante CEB) ou do Censo da Educação Superio (CES) é
necessário removar a maioria das colunas (que não tem uso para a
pesquisa e aumentam muito o peso do dataset) e realizar o merge através
da função *left_join*.

C:\

### Censo Escolar

Vamos iniciar carregando a base do CEB e excluindo as colunas que não
nos interessam:

```{R}
CEB_2009 <- readr::read_delim("C:\CENSO_ESCOLAR\CEB_2009.csv", delim=";", n_max = 0)
print(colnamesCEB_2009)
VAR_CESCOLAR_2009 <-colnamesCEB_2009


#Códifo para diferenciar as colunas

colunas_desejadas <- c("ID_ALUNO", "TP_COR_RACA")

# Lê somente essas colunas
dados <- fread("arquivo.csv", select = colunas_desejadas)


#Essa forma de leitura se repete para todas as outras bases
```

```{r}


# Supõe que os dados já estejam lidos em um data frame chamado bas_situacao
censo_1 <- bas_situacao %>%
  filter(
    NU_ANO_CENSO %in% 2009:2012,
    IN_CONCLUINTE == 1,
    TP_SITUACAO %in% c(5, 9),
    TP_ETAPA_ENSINO %in% c(27, 28, 29, 32, 33, 34, 37, 38),
    between(NU_IDADE, 15, 29)
  ) %>%
  select(
    NU_ANO_CENSO,
    CO_PESSOA_FISICA,
    CHAVE_UNICA,
    NU_IDADE,
    TP_SEXO,
    TP_COR_RACA,
    CO_UF,
    CO_MUNICIPIO,
    CO_ENTIDADE,
    TP_DEPENDENCIA,
    TP_LOCALIZACAO,
    IN_PROFISSIONALIZANTE,
    TP_ETAPA_ENSINO,
    TP_SITUACAO
  )

```

```{R}

merge()
left_join()
join_by()
left_join()

```

--- Código Senkevicks

```{r}
library(tidyverse)

# Configurações iniciais
rm(list = ls())
options(readr.show_col_types = FALSE)

# Definir diretório
data_dir <- "inserir o caminho das bases"

##############################
# CENSO ESCOLAR + ENEM

# Importar e processar dados Censo + ENEM
ceb_egressos_2012 <- read_csv(file.path(data_dir, "censo_enem.csv")) %>% 
  # Filtrar apenas 2012
  filter(nu_ano_censo == 2012) %>% 
  # Tratar raça/cor
  mutate(
    raca = coalesce(iein_co_cor_raca, tp_cor_raca),
    raca = if_else(iein_co_cor_raca == "6", tp_cor_raca, raca)
  ) %>% 
  # Calcular média das notas
  mutate(
    media = rowMeans(across(c(iere_vl_nota_ch, iere_vl_nota_cn, 
                           iere_vl_nota_lc, iere_vl_nota_mt)),
    .keep = "unused"
  ) %>% 
  # Calcular renda per capita
  mutate(
    pm_sm = case_match(
      iequ_questao_06,
      "A" ~ 0, "B" ~ 1, "C" ~ 1.25, "D" ~ 1.75, "E" ~ 2.25,
      "F" ~ 2.75, "G" ~ 3.5, "H" ~ 4.5, "I" ~ 5.5, "J" ~ 6.5,
      "K" ~ 7.5, "L" ~ 8.5, "M" ~ 9.5, "N" ~ 11, "O" ~ 13.5,
      "P" ~ 17.5, "Q" ~ 20,
      .default = NA_real_
    ),
    rfpc = pm_sm / as.numeric(iequ_questao_05)
  ) %>% 
  # Escolaridade parental
  mutate(
    escp = pmax(
      as.numeric(iequ_questao_02), 
      as.numeric(iequ_questao_01), 
      na.rm = TRUE
    )
  ) %>% 
  # Renomear e selecionar variáveis
  rename(
    trabalho = iequ_questao_26,
    fundprivado = iequ_questao_42,
    ano_enem = iein_nu_ano
  ) %>% 
  select(-c(iein_co_inscricao, iequ_questao_47, iequ_questao_01, iequ_questao_02,
            iequ_questao_05, iequ_questao_06, tp_cor_raca, iein_co_cor_raca))

# Salvar dados
write_rds(ceb_egressos_2012, file.path(data_dir, "ceb_egressos_2012.rds"))

##############################
# CENSO DA EDUCAÇÃO SUPERIOR

# Função para processamento dos dados CES
process_ces_data <- function(file_path, year) {
  # Definir labels para fatores
  categ_labels <- c("Pública", "Privada")
  grau_labels <- c("NA", "Bacharelado", "Licenciatura", "Tecnológico")
  turno_labels <- c("Diurno", "Noturno", "EaD")
  situacao_labels <- c("Cursando", "Matrícula trancada", "Desvinculado do curso",
                      "Transferido para curso da mesma IES", "Formado", "Falecido")
  
  read_csv(file_path) %>% 
    # Filtrar e selecionar colunas
    filter(tp_nivel_academico != 2) %>% 
    select(-c(co_aluno_curso, tp_cor_raca, tp_organizacao_academica,
             tp_nivel_academico, co_area_geral, co_area_especifica,
             co_area_detalhada, nu_ano_censo)) %>% 
    # Processar variáveis
    rename(co_ocde = co_rotulo) %>% 
    mutate(
      # Categoria administrativa
      categ = if_else(tp_categoria_administrativa %in% 1:3, 1L, 2L),
      categ = factor(categ, levels = 1:2, labels = categ_labels),
      
      # Turno
      turno = case_when(
        is.na(tp_turno) & tp_modalidade_ensino == 2 ~ 3L,
        tp_turno %in% c(1, 2, 4) ~ 1L,
        tp_turno == 3 ~ 2L,
        TRUE ~ NA_integer_
      ),
      turno = factor(turno, levels = 1:3, labels = turno_labels),
      
      # Grau acadêmico
      grau = replace_na(tp_grau_academico, 0),
      grau = factor(grau, levels = 0:3, labels = grau_labels),
      
      # Situação
      sit = factor(tp_situacao, levels = 2:7, labels = situacao_labels),
      
      # IES e curso
      ies = co_ies,
      curso = co_curso,
      
      # Formatar código do aluno
      co_aluno = as.character(co_aluno)
    ) %>% 
    # Selecionar e ordenar colunas
    select(co_aluno, chave_unica, categ, ies, grau, turno, 
           curso, co_ocde, sit) %>% 
    # Salvar dados
    {write_rds(., file.path(data_dir, str_glue("ces_aluno_{year}.rds"))); .}
}

# Processar cada ano
walk2(
  list.files(data_dir, pattern = "aluno_\\d{2}\\.csv", full.names = TRUE),
  str_extract(list.files(data_dir, pattern = "aluno_\\d{2}\\.csv"), "\\d{2}"),
  process_ces_data
)

############################################
# TRATAMENTO FINAL DAS BASES 2013-2017

# Função para tratamento final
final_processing <- function(year) {
  read_rds(file.path(data_dir, str_glue("ces_aluno_{year}.rds"))) %>% 
    # Filtrar casos com chave única
    filter(chave_unica != "") %>% 
    # Remover duplicatas completas
    distinct(co_aluno, ies, grau, turno, curso, sit, .keep_all = TRUE) %>% 
    # Salvar dados finais
    write_rds(file.path(data_dir, str_glue("ces_aluno_matching_{year}.rds")))
}

# Aplicar a todos os anos
walk(c("13", "14", "15", "16", "17"), final_processing)
```

library(tidyverse)

# Caminho para a pasta onde estão seus arquivos

pasta \<- "caminho/para/as/bases"

# Lista todos os arquivos .rds

arquivos \<- list.files(pasta, pattern = "\\.rds\$", full.names = TRUE)

# Extrai o ano do nome do arquivo (assumindo que está nos últimos 4 dígitos antes do .rds)

extrair_ano \<- function(nome_arquivo) { str_extract(nome_arquivo,
"\\d{4}(?=\\.rds\$)") }

# Organiza arquivos por tipo

arquivos_df \<- tibble( caminho = arquivos, ano =
as.integer(extrair_ano(arquivos)), tipo = case_when(
str_detect(arquivos, "ensino_medio") \~ "em", str_detect(arquivos,
"enem") \~ "enem", str_detect(arquivos, "ensino_superior") \~ "es", TRUE
\~ "outro" ) ) %\>% filter(tipo != "outro")

# Cria função para processar 1 ano base

processar_ano \<- function(ano_base) { \# Carrega base EM em \<-
arquivos_df %\>% filter(tipo == "em", ano == ano_base) %\>%
pull(caminho) %\>% read_rds()

\# Junta com Enem do ano, ano+1, ano+2 enem \<- arquivos_df %\>%
filter(tipo == "enem", ano %in% c(ano_base, ano_base + 1, ano_base + 2))
%\>% arrange(ano) %\>% mutate(dados = map(caminho, read_rds)) %\>%
pull(dados) %\>% bind_rows()

em_enem \<- left_join(em, enem, by = "id_pessoa") \# adapte a chave

\# Junta com ES do ano+1, ano+2, ano+3 es \<- arquivos_df %\>%
filter(tipo == "es", ano %in% c(ano_base + 1, ano_base + 2, ano_base +
3)) %\>% arrange(ano) %\>% mutate(dados = map(caminho, read_rds)) %\>%
pull(dados) %\>% bind_rows()

final \<- left_join(em_enem, es, by = "id_pessoa") \# adapte a chave
final }

# Executa para os anos de 2010 a 2020

anos_base \<- 2010:2020

resultados \<- map(anos_base, processar_ano)

# Opcional: empilhar tudo

resultado_total \<- bind_rows(resultados)

# Você pode salvar o resultado final, por exemplo:

# write_rds(resultado_total, "resultado_completo.rds")
