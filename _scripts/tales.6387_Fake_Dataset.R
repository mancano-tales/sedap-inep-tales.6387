------------------------------------------------------------------------

------------------------------------------------------------------------

Este código é responsável por gerar alguns datasets que vamos usar para ir testando ficticiamente o código principal. A partir dos dicionários de dados disponobilizados pelo INEP, vamos gerar os datasets com poquíssimas observações, apenas para fazer testes de manipulação e testar o nosso código antes de enviar para o SEDAP.

CATALOGO_CESCOLAR_BAS_SITUACAO

Eu quero gerar bancos de dados a partir de um dicionario de dados, quero que o R crie na pasta SIMU_DATA uma pasta que chamará CESCOLAR, como manda a primeira colunas do dicionário chamada BAS_CAT. Depois outra pasta de acordo com a segunda coluna chamada BAS_NAME, Nela eu quero criar diversos arquivos em CSV de uma base de dados que tem uma edição por ano de 2008 a 2020, Cada um desses aquibos tem várias colunas, como representado pela coluna VAR_NAME, cada uma dessas colunas tem informações sobre individuos as regras sobre o que tem nas linhas dos indiciduos estão nas colunas TYPE, que informa sobre o tipo de variavel, TAM que informa sobre o tamaho e RANGE_MIN e RANGE_MAX que limita a variação dos dados que vão ser gerados, pode escrever um código para mim que faça isso?


```{r Load the main packages}
library(tidyverse)
library(charlatan) 
library(stringi) 

# para gerar dados falsos
set.seed(6387)
```

```{r Creates a data catalog based on the one INEP provided}
CATALOGO_CESCOLAR_BAS_SITUACAO <- tibble::tibble(
  BAS_CAT = rep("CESCOLAR", 15), # Categoria da Base, todas do Censo Escolar
  BAS_NAME = rep("CESCOLAR_BAS_SITUACAO", 15), # BAS do CESCOLAR, a BAS_SITUACAO
  FIRST_YEAR = rep(2008L, 15), # First Year we are using it
  LAST_YEAR = rep(2020L, 15), # Last Year we are generating
  VAR_NO = 1:15, # Just to properly sequel de variables
  
  # Now the name of the collums according to the data dictionaries
  VAR_NAME = c(
    "NU_ANO", "ID_MATRICULA", "CPF_MASC", "CO_PESSOA_FISICA", "TP_ETAPA_ENSINO",
    "IN_REGULAR", "ID_TURMA", "CO_ENTIDADE", "CO_ORGAO_REGIONAL", "CO_UF",
    "CO_MUNICIPIO", "TP_DEPENDENCIA", "TP_SITUACAO", "IN_CONCLUINTE", "IN_TRANSFERIDO"
  ), 
  
  # Now the Description of those variables
  DESCRIPTION = c(
    "Ano do Censo",
    "Código único da matrícula",
    "CPF mascarado no formato char(32)",
    "Código do aluno (ID_INEP)",
    "Etapa de ensino da matrícula",
    "Modo, maneira ou metodologia de ensino correspondente às turmas com etapas de escolarização consecutivas, Creche ao Ensino Médio. Etapas consideradas (nas antigas modalidades 1 ou 2): TP_ETAPA_ENSINO igual a 1,2,4,5,6,7,8,9,10,11,14, 15,16,17,18,19,20,21,41,25,26,27,28,29,30,31, 32,33,34,35,36,37 ou 38.",
    "Código único da Turma 3",
    "Código da Escola 3",
    "Código do Órgão Regional de Ensino",
    "Código UF da escola",
    "Código Município da escola",
    "Dependência Administrativa (Escola)",
    "Situação de rendimento ou movimento do aluno ao final do ano letivo",
    "Aluno que tenha sido aprovado e terminado uma etapa final do ensino fundamental, ensino médio ou de educação profissional com emissão de certificado. (etapas: 11,41,27,28,29,32,33,34,37,38,39,40,70,71,73,74,65,67 e 68).",
    "Aluno que foi para outra escola após a data de referência do Censo Escolar."
  ),
  
  # Their data type
  TYPE = c(
    "integer", "integer", "integer", "integer", "integer",
    "binary", "integer", "integer", "integer", "integer",
    "integer", "integer", "integer", "binary", "binary"
  ),
  
  # Tamanhos dos campos
  TAM = c(4, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1),
  
  # Domínios de valores - descrição textual dos valores permitidos
  DOM = c(
    "2008-2020", "0-99", "0-99", "0-99", "1-74",
    "1=Sim;2=Não;3=Ignorado", NA, NA, NA, NA,
    NA, "1-4", "2-9", "1=Sim;2=Não;3=Ignorado", "1=Sim;2=Não;3=Ignorado"
  ),
  
  
  RANGE_MIN = c(2008, 0, 0, 0, 1, 0, NA, NA, NA, NA, NA, 1, 2, 0, 0),
  
  
  RANGE_MAX = c(2020, 99, 99, 99, 74, 1, NA, NA, NA, NA, NA, 4, 9, 1, 1),
  
  # Flags de seleção - indica quais variáveis são importantes para análise. Those marked with one are the ones that I use
  SELECT = c(1, NA, 1, NA, NA, NA, NA, NA, NA, NA, NA, 1, NA, 1, NA),
  
  # Campos para observações e scripts personalizados (não utilizados atualmente)
  OBS = rep(NA, 15),
  SCRIPT = rep(NA, 15)
)
```


```{r We will do only 20 rows this time}
# Número de estudantes simulados por ano
N_ESTUDANTES <- 20
```


```{r Create a unique identifier for people using CPF}
# Criar contador global de CPF
CPF_COUNTER <- 0
```


```{r VAR_GENERATOR Function}
# Função de geração de variáveis
VAR_GENERATOR <- function(var_name, tipo, tam, min, max, n) {
  # Acesso ao contador global
  if (var_name == "CPF_MASC") {
    CPF_VALUE <- seq(from = CPF_COUNTER + 1, length.out = n)
    assign("CPF_COUNTER", CPF_COUNTER + n, envir = .GlobalEnv)
    return(CPF_VALUE)
  }
  
  if (tipo == "integer") {
    return(sample(min:max, n, replace = TRUE))
  } else if (tipo == "binary") {
    return(sample(0:1, n, replace = TRUE))
  } else if (tipo == "string") {
    return(stringi::stri_rand_strings(n, tam))
  } else {
    return(rep(NA, n))
  }
}
```


```{r Main Loop}
# Loop principal
for (bas_cat in unique(variaveis_cescolar$BAS_CAT)) {
  sub_df_cat <- variaveis_cescolar %>% filter(BAS_CAT == bas_cat)
  dir_cat <- file.path("SIMU_DATA", bas_cat)
  dir.create(dir_cat, recursive = TRUE, showWarnings = FALSE)

  for (bas_name in unique(sub_df_cat$BAS_NAME)) {
    sub_df_name <- sub_df_cat %>% filter(BAS_NAME == bas_name)
    dir_name <- file.path(dir_cat, bas_name)
    dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)

    anos <- sub_df_name$FIRST_YEAR[1]:sub_df_name$LAST_YEAR[1]

    for (ano in anos) {
      df <- tibble()

      for (i in 1:nrow(sub_df_name)) {
        row <- sub_df_name[i, ]
        col_name <- row$VAR_NAME
        tipo <- row$TYPE
        tam <- row$TAM
        min <- suppressWarnings(as.integer(row$RANGE_MIN))
        max <- suppressWarnings(as.integer(row$RANGE_MAX))

        df[[col_name]] <- VAR_GENERATOR(col_name, tipo, tam, min, max, N_ESTUDANTES)
      }

      df$ANO_SIMULADO <- ano
      nome_arquivo <- paste0(bas_name, "_", ano, ".csv")
      readr::write_csv(df, file.path(dir_name, nome_arquivo))
    }
  }
}
```






