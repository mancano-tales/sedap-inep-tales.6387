---
title: "tales.6387_0_Data_Load_&_PrepZZZZZZZZZZZZZZZZZZZZZZZ"
---

## Planejamento

O que precisamos fazer? primeiro precisamos gerar os arquivos de simulação com que vamos trabalhar.

## Apresentação

Este é o primeiro arquivo de código do projeto registrado junto ao INEP sob o código #23036.006387/2024-81, usuário: tales.6387. Temos como objetivo estimar e decompor o impacto de políticas como as cotas e o ProUNI na redução das desigualdades de acesso na educação superior no Brasil conforme identificado por Salata et al. (2025) via PNAD. Fazemos isso através de um esforço de replicação e ampliação do desenho de pesquisa apresentado por Senkevicks et al. (2024). A proposta é, tal qual fizeram Senkevicks, Barbosa, Carvalhaes e Ribeiro, realizar um cruzamento das coortes de egressos da educação básica a cada ano de 2008 à 2019. Iniciamos assim, carregando e tratando as bases do Censo Escolar. Através da coluna *CPF_MASC* cruzaremos essa base com a base do ENEM, cujos dados de desempenho acadêmico e dados socioeconômicos, como renda, raça e escolaridade dos pais, são agregados à analise. Por fim, cruzamos com as vases do Es


tales.6387
# 1. Configuração Inicial

```{r}
# Diretório de trabalho
getwd()
setwd("T:/")

# Verificação dos pacotes instalados
installed.packages()

# Carregar pacotes
library(tidyverse)

# 2. Leitura das Bases de Dados

# Leitura das colunas da base ENEM 2010
ENEM_2010 <- readr::read_delim("1. Bases Enem/ENEM_2010.csv", delim = ";", n_max = 0)
colnames(ENEM_2010)
```

# 3. Pré-processamento dos Dados

## 3.1 Censo Escolar

```{r}
library(data.table)

colunas_desejadas <- c("ID_ALUNO", "TP_COR_RACA")
dados <- fread("CENSO_ESCOLAR/CEB_2009.csv", select = colunas_desejadas)
```

## 3.2 Filtro de Concluintes

```{r}
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

# 4. Código de Referência (Senkevicks)

## 4.1 Importação e Tratamento do Enem + Censo

```{r}
data_dir <- "inserir o caminho das bases"

ceb_egressos_2012 <- read_csv(file.path(data_dir, "censo_enem.csv")) %>%
  filter(nu_ano_censo == 2012) %>%
  mutate(
    raca = coalesce(iein_co_cor_raca, tp_cor_raca),
    raca = if_else(iein_co_cor_raca == "6", tp_cor_raca, raca),
    media = rowMeans(across(c(iere_vl_nota_ch, iere_vl_nota_cn, 
                              iere_vl_nota_lc, iere_vl_nota_mt)), na.rm = TRUE),
    pm_sm = case_match(
      iequ_questao_06,
      "A" ~ 0, "B" ~ 1, "C" ~ 1.25, "D" ~ 1.75, "E" ~ 2.25,
      "F" ~ 2.75, "G" ~ 3.5, "H" ~ 4.5, "I" ~ 5.5, "J" ~ 6.5,
      "K" ~ 7.5, "L" ~ 8.5, "M" ~ 9.5, "N" ~ 11, "O" ~ 13.5,
      "P" ~ 17.5, "Q" ~ 20, .default = NA_real_
    ),
    rfpc = pm_sm / as.numeric(iequ_questao_05),
    escp = pmax(as.numeric(iequ_questao_02), as.numeric(iequ_questao_01), na.rm = TRUE)
  ) %>%
  rename(
    trabalho = iequ_questao_26,
    fundprivado = iequ_questao_42,
    ano_enem = iein_nu_ano
  ) %>%
  select(-c(iein_co_inscricao, iequ_questao_47, iequ_questao_01, 
            iequ_questao_02, iequ_questao_05, iequ_questao_06, 
            tp_cor_raca, iein_co_cor_raca))

write_rds(ceb_egressos_2012, file.path(data_dir, "ceb_egressos_2012.rds"))
```

## 4.2 Censo da Educação Superior (CES)

```{r}
process_ces_data <- function(file_path, year) {
  read_csv(file_path) %>%
    filter(tp_nivel_academico != 2) %>%
    select(-c(co_aluno_curso, tp_cor_raca, tp_organizacao_academica,
              tp_nivel_academico, co_area_geral, co_area_especifica,
              co_area_detalhada, nu_ano_censo)) %>%
    rename(co_ocde = co_rotulo) %>%
    mutate(
      categ = if_else(tp_categoria_administrativa %in% 1:3, 1L, 2L),
      categ = factor(categ, levels = 1:2, labels = c("Pública", "Privada")),
      turno = case_when(
        is.na(tp_turno) & tp_modalidade_ensino == 2 ~ 3L,
        tp_turno %in% c(1, 2, 4) ~ 1L,
        tp_turno == 3 ~ 2L,
        TRUE ~ NA_integer_
      ),
      turno = factor(turno, levels = 1:3, labels = c("Diurno", "Noturno", "EaD")),
      grau = replace_na(tp_grau_academico, 0),
      grau = factor(grau, levels = 0:3, labels = c("NA", "Bacharelado", "Licenciatura", "Tecnológico")),
      sit = factor(tp_situacao, levels = 2:7, labels = c("Cursando", "Trancado", "Desvinculado",
                                                         "Transferido", "Formado", "Falecido")),
      co_aluno = as.character(co_aluno),
      ies = co_ies,
      curso = co_curso
    ) %>%
    select(co_aluno, chave_unica, categ, ies, grau, turno, curso, co_ocde, sit) %>%
    {write_rds(., file.path(data_dir, str_glue("ces_aluno_{year}.rds"))); .}
}

walk2(
  list.files(data_dir, pattern = "aluno_\\d{2}\\.csv", full.names = TRUE),
  str_extract(list.files(data_dir, pattern = "aluno_\\d{2}\\.csv"), "\\d{2}"),
  process_ces_data
)
```

## 4.3 Tratamento Final CES

```{r}
final_processing <- function(year) {
  read_rds(file.path(data_dir, str_glue("ces_aluno_{year}.rds"))) %>%
    filter(chave_unica != "") %>%
    distinct(co_aluno, ies, grau, turno, curso, sit, .keep_all = TRUE) %>%
    write_rds(file.path(data_dir, str_glue("ces_aluno_matching_{year}.rds")))
}

walk(c("13", "14", "15", "16", "17"), final_processing)
```

# 5. Cruzamento EM + Enem + ES

```{r}
pasta <- "caminho/para/as/bases"
arquivos <- list.files(pasta, pattern = "\\.rds$", full.names = TRUE)

extrair_ano <- function(nome_arquivo) {
  str_extract(nome_arquivo, "\\d{4}(?=\\.rds$)")
}

arquivos_df <- tibble(
  caminho = arquivos,
  ano = as.integer(extrair_ano(arquivos)),
  tipo = case_when(
    str_detect(arquivos, "ensino_medio") ~ "em",
    str_detect(arquivos, "enem") ~ "enem",
    str_detect(arquivos, "ensino_superior") ~ "es",
    TRUE ~ "outro"
  )
) %>% filter(tipo != "outro")

processar_ano <- function(ano_base) {
  em <- arquivos_df %>%
    filter(tipo == "em", ano == ano_base) %>%
    pull(caminho) %>%
    read_rds()
  
  enem <- arquivos_df %>%
    filter(tipo == "enem", ano %in% c(ano_base, ano_base + 1, ano_base + 2)) %>%
    arrange(ano) %>%
    mutate(dados = map(caminho, read_rds)) %>%
    pull(dados) %>%
    bind_rows()
  
  em_enem <- left_join(em, enem, by = "id_pessoa")
  
  es <- arquivos_df %>%
    filter(tipo == "es", ano %in% c(ano_base + 1, ano_base + 2, ano_base + 3)) %>%
    arrange(ano) %>%
    mutate(dados = map(caminho, read_rds)) %>%
    pull(dados) %>%
    bind_rows()
  
  final <- left_join(em_enem, es, by = "id_pessoa")
  final
}

anos_base <- 2010:2020
resultados <- map(anos_base, processar_ano)
resultado_total <- bind_rows(resultados)

# write_rds(resultado_total, "resultado_completo.rds")
```

Se quiser, posso gerar um sumário em PDF ou sugerir como integrar com GitHub ou CI/CD para versionamento e reprodutibilidade. Deseja isso?
