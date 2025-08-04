------------------------------------------------------------------------

------------------------------------------------------------------------

Este código é responsável por gerar alguns datasets que vamos usar para ir testando ficticiamente o código principal. A partir dos dicionários de dados disponobilizados pelo INEP, vamos gerar os datasets com poquíssimas observações, apenas para fazer testes de manipulação e testar o nosso código antes de enviar para o SEDAP.

CATALOGO_CESCOLAR_BAS_SITUACAO

Eu quero gerar bancos de dados a partir de um dicionario de dados, quero que o R crie na pasta SIMU_DATA uma pasta que chamará CESCOLAR, como manda a primeira colunas do dicionário chamada BAS_CAT. Depois outra pasta de acordo com a segunda coluna chamada BAS_NAME, Nela eu quero criar diversos arquivos em CSV de uma base de dados que tem uma edição por ano de 2018 a 2020, Cada um desses aquibos tem várias colunas, como representado pela coluna VAR_NAME, cada uma dessas colunas tem informações sobre individuos as regras sobre o que tem nas linhas dos indiciduos estão nas colunas TYPE, que informa sobre o tipo de variavel, TAM que informa sobre o tamaho e RANGE_MIN e RANGE_MAX que limita a variação dos dados que vão ser gerados, pode escrever um código para mim que faça isso?


```{r Carrega alguns pacotes fundamentais}
library(tidyverse)
library(charlatan) 
library(stringi) 

# para gerar dados falsos
set.seed(6387)
```

```{r First Option for }
# 1. Lê o dicionário de dados (salvo como CSV com separador TAB ou vírgula, ajuste se necessário)
dict <- readr::read_delim("caminho/para/o/seu/dicionario.csv", delim = "\t", show_col_types = FALSE)

# 2. Transforma em tibble e filtra apenas as variáveis necessárias
dicionario <- dicionario %>%
  filter(!is.na(VAR_NAME)) %>%
  mutate(across(c(RANGE_MIN, RANGE_MAX), as.numeric))

# 3. Parâmetros de simulação
anos <- 2018:2020
n <- 1000  # número de registros por ano

# 4. Loop para criar dados simulados por ano
for (ano in anos) {
  # Subconjunto do dicionário para o ano
  dict_ano <- dict %>%
    filter(FIRST_YEAR <= ano, LAST_YEAR >= ano)

  # Nome das pastas
  pasta_base <- file.path("SIMU_DATA", dict_ano$BAS_CAT[1], dict_ano$BAS_NAME[1])
  dir.create(pasta_base, recursive = TRUE, showWarnings = FALSE)

  # Simular os dados
  dados <- map_dfc(1:nrow(dict_ano), function(i) {
    var <- dict_ano[i, ]
    nome <- var$VAR_NAME
    tipo <- tolower(var$TYPE)
    min <- var$RANGE_MIN
    max <- var$RANGE_MAX
    tam <- var$TAM

    if (tipo == "integer" || tipo == "numeric") {
      if (!is.na(min) && !is.na(max)) {
        col <- sample(min:max, n, replace = TRUE)
      } else {
        col <- sample(1:100, n, replace = TRUE)
      }
    } else if (tipo == "binary") {
      col <- sample(0:1, n, replace = TRUE)
    } else if (tipo == "character") {
      col <- replicate(n, paste0(sample(LETTERS, tam, TRUE), collapse = ""))
    } else {
      col <- rep(NA, n)
    }

    setNames(list(col), nome)
  })

  # Adiciona coluna do ano
  if ("NU_ANO" %in% names(dados)) {
    dados$NU_ANO <- ano
  }

  # Salva o CSV
  file_name <- file.path(pasta_base, paste0("base_simulada_", ano, ".csv"))
  readr::write_csv(dados, file_name)
}

```

```{python}
'c`a~a`a~Aa`A~~~~~~Aa~  'c`c`c'c'c`c'cc  
```

```{python}

```

```{R testando um código tibble usa uma função interessante quero estudar mais esse códigoTó}

#CRIA UMA FUNCAOCH
criar_variaveis_base <- function(base_cat, base_name, first_year, last_year, var_names) {
  tibble::tibble(
    BAS_CAT = base_cat,
    BAS_NAME = base_name,
    FIRST_YEAR = first_year,
    LAST_YEAR = last_year,
    VAR_NO = paste0("#", seq_along(var_names)),
    VAR_NAME = var_names
  )
}

PIUPIU <- criar_variaveis_base(
  base_cat = "CESCOLAR",
  base_name = "CESCOLAR_BAS_SITUACAO",
  first_year = 2008,
  last_year = 2020,
  var_names = c(
    "NU_ANO",
    "ID_MATRICULA",
    "CPF_MASC",
    "CO_PESSOA_FISICA",
    "TP_ETAPA_ENSINO",
    "IN_REGULAR",
    "ID_TURMA",
    "CO_ENTIDADE",
    "CO_ORGAO_REGIONAL",
    "CO_UF",
    "CO_MUNICIPIO",
    "TP_DEPENDENCIA",
    "TP_SITUACAO",
    "IN_CONCLUINTE",
    "IN_TRANSFERIDO"
  )
)

```

# Exemplo simples de dicionário

```{r}
library(tibble)

Gay <- tibble::tibble(
  BAS_CAT = "CESCOLAR",
  BAS_NAME = "CESCOLAR_BAS_SITUACAO",
  FIRST_YEAR = 2008,
  LAST_YEAR = 2020,
  VAR_NO = paste0("#", 1:15),
  VAR_NAME = c(
    "NU_ANO",
    "ID_MATRICULA",
    "CPF_MASC",
    "CO_PESSOA_FISICA",
    "TP_ETAPA_ENSINO",
    "IN_REGULAR",
    "ID_TURMA",
    "CO_ENTIDADE",
    "CO_ORGAO_REGIONAL",
    "CO_UF",
    "CO_MUNICIPIO",
    "TP_DEPENDENCIA",
    "TP_SITUACAO",
    "IN_CONCLUINTE",
    "IN_TRANSFERIDO"
  )
)


library(tibble)
library(readr)

# Copiamos o conteúdo como string
dicionario_str <- "
BAS_CAT\tBAS_NAME\tFIRST_YEAR\tLAST_YEAR\tVAR_NO\tVAR_NAME\tDESCRIPTION\tTYPE\tTAM\tRANGE_MIN\tRANGE_MAX\tCATEGORIES\tSELECT\tOBS\tSCRIPT
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#1\tNU_ANO\tAno do Censo\tinteger\t4\t2008\t2020\tAnos do censo entre 2008 e 2020\t1\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#2\tID_MATRICULA\tCódigo único da matrícula\tinteger\t2\t0\t99\tNA\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#3\tCPF_MASC\tCPF mascarado no formato char(32)\tinteger\t2\t0\t99\tNA\t1\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#4\tCO_PESSOA_FISICA\tCódigo do aluno (ID_INEP)\tinteger\t2\t0\t99\tNA\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#5\tTP_ETAPA_ENSINO\tEtapa de ensino da matrícula\tinteger\t2\t1\t74\t\"1 - Educação Infantil - Creche\n2 - Educação Infantil - Pré-escola\n4 - Ensino Fundamental de 8 anos - 1ª Série\n5 - Ensino Fundamental de 8 anos - 2ª Série\n6 - Ensino Fundamental de 8 anos - 3ª Série\n7 - Ensino Fundamental de 8 anos - 4ª Série\n8 - Ensino Fundamental de 8 anos - 5ª Série\n9 - Ensino Fundamental de 8 anos - 6ª Série\n10 - Ensino Fundamental de 8 anos - 7ª Série\n11 - Ensino Fundamental de 8 anos - 8ª Série\n14 - Ensino Fundamental de 9 anos - 1º Ano\n15 - Ensino Fundamental de 9 anos - 2º Ano\n16 - Ensino Fundamental de 9 anos - 3º Ano\n17 - Ensino Fundamental de 9 anos - 4º Ano\n18 - Ensino Fundamental de 9 anos - 5º Ano\n19 - Ensino Fundamental de 9 anos - 6º Ano\n20 - Ensino Fundamental de 9 anos - 7º Ano\n21 - Ensino Fundamental de 9 anos - 8º Ano\n41 - Ensino Fundamental de 9 anos - 9º Ano\n25 - Ensino Médio - 1º ano/1ª Série\n26 - Ensino Médio - 2º ano/2ª Série\n27 - Ensino Médio - 3ºano/3ª Série\n28 - Ensino Médio - 4º ano/4ª Série\n29 - Ensino Médio - Não Seriada\n30 - Curso Técnico Integrado (Ensino Médio Integrado) 1ª Série\n31 - Curso Técnico Integrado (Ensino Médio Integrado) 2ª Série\n32 - Curso Técnico Integrado (Ensino Médio Integrado) 3ª Série\n33 - Curso Técnico Integrado (Ensino Médio Integrado) 4ª Série\n34 - Curso Técnico Integrado (Ensino Médio Integrado) Não Seriada\n35 - Ensino Médio - Modalidade Normal/Magistério 1ª Série\n36 - Ensino Médio - Modalidade Normal/Magistério 2ª Série\n37 - Ensino Médio - Modalidade Normal/Magistério 3ª Série\n38 - Ensino Médio - Modalidade Normal/Magistério 4ª Série\n39 - Curso Técnico - Concomitante\n40 - Curso Técnico - Subsequente\n65 - EJA - Ensino Fundamental - Projovem Urbano\n67 - Curso FIC integrado na modalidade EJA  - Nível Médio\n68 - Curso FIC Concomitante \n69 - EJA - Ensino Fundamental - Anos Iniciais\n70 - EJA - Ensino Fundamental - Anos Finais\n71 - EJA - Ensino Médio\n72 - EJA - Ensino Fundamental  - Anos iniciais e Anos finais\n73 - Curso FIC integrado na modalidade EJA - Nível Fundamental (EJA integrada à Educação Profissional de Nível Fundamental) \n74 - Curso Técnico Integrado na Modalidade EJA (EJA integrada à Educação Profissional de Nível Médio)\"\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#6\tIN_REGULAR\tModo, maneira ou metodologia de ensino...\tbinary\t1\t0\t1\t\"0 - Não\n1 - Sim\"\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#7\tID_TURMA\tCódigo único da Turma 3\tinteger\t2\t\t\tTurmas do evento 55...\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#8\tCO_ENTIDADE\tCódigo da Escola 3\tinteger\t2\t\t\tEscolas que não...\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#9\tCO_ORGAO_REGIONAL\tCódigo do Órgão Regional de Ensino\tinteger\t2\t\t\tNA\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#10\tCO_UF\tCódigo UF da escola\tinteger\t2\t\t\tNA\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#11\tCO_MUNICIPIO\tCódigo Município da escola\tinteger\t2\t\t\tNA\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#12\tTP_DEPENDENCIA\tDependência Administrativa (Escola)\tinteger\t1\t1\t4\t\"1 - Federal\n2 - Estadual\n3 - Municipal\n4 - Privada\"\t1\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#13\tTP_SITUACAO\tSituação de rendimento ou movimento do aluno...\tinteger\t1\t2\t9\t\"2 - Abandono...\n3 - Falecido...\n4 - Reprovado...\n5 - Aprovado...\n9 - Sir...\"\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#14\tIN_CONCLUINTE\tAluno que tenha sido aprovado...\tbinary\t1\t0\t1\t\"0 - Não\n1 - Sim\"\t1\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#15\tIN_TRANSFERIDO\tAluno que foi para outra escola...\tbinary\t1\t0\t1\t\"0 - Não\n1 - Sim\"\t\t\t
"
```

```{r Suposedly a working code}
CATALOGO_CESCOLAR_BAS_SITUACAO <- tibble::tibble(
  BAS_CAT = rep("CESCOLAR", 15),
  BAS_NAME = rep("CESCOLAR_BAS_SITUACAO", 15),
  FIRST_YEAR = rep(2008L, 15),
  LAST_YEAR = rep(2020L, 15),
  VAR_NO = 1:15,
  VAR_NAME = c(
    "NU_ANO", "ID_MATRICULA", "CPF_MASC", "CO_PESSOA_FISICA", "TP_ETAPA_ENSINO",
    "IN_REGULAR", "ID_TURMA", "CO_ENTIDADE", "CO_ORGAO_REGIONAL", "CO_UF",
    "CO_MUNICIPIO", "TP_DEPENDENCIA", "TP_SITUACAO", "IN_CONCLUINTE", "IN_TRANSFERIDO"
  ),
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
  TYPE = c(
    "integer", "integer", "integer", "integer", "integer",
    "binary", "integer", "integer", "integer", "integer",
    "integer", "integer", "integer", "binary", "binary"
  ),
  TAM = c(4, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1),
  DOM = c(
    "2008-2020", "0-99", "0-99", "0-99", "1-74",
    "1=Sim;2=Não;3=Ignorado", NA, NA, NA, NA,
    NA, "1-4", "2-9", "1=Sim;2=Não;3=Ignorado", "1=Sim;2=Não;3=Ignorado"
  ),
  RANGE_MIN = c(2008, 0, 0, 0, 1, 0, NA, NA, NA, NA, NA, 1, 2, 0, 0),
  RANGE_MAX = c(2020, 99, 99, 99, 74, 1, NA, NA, NA, NA, NA, 4, 9, 1, 1),
  SELECT = c(1, NA, 1, NA, NA, NA, NA, NA, NA, NA, NA, 1, NA, 1, NA),
  OBS = rep(NA, 15),
  SCRIPT = rep(NA, 15)
)
variaveis_cescolar <- CATALOGO_CESCOLAR_BAS_SITUACAO

# Número de estudantes simulados por ano
N_ESTUDANTES <- 20

# Criar contador global de CPF
CPF_COUNTER <- 0

# Função de geração de variáveis
VAR_GENERATOR <- function(var_name, tipo, tam, min, max, n) {
  # Acesso ao contador global
  if (var_name == "CPF_MASC") {
    cpf_vals <- seq(from = CPF_COUNTER + 1, length.out = n)
    assign("CPF_COUNTER", CPF_COUNTER + n, envir = .GlobalEnv)
    return(cpf_vals)
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

```{r Suposedly a working code}
# Lê como tibble
dicionario <- read_tsv(dicionario_str)

# Exibe
print(dicionario)



dicionario_str <- tibble::tibble("
BAS_CAT\tBAS_NAME\tFIRST_YEAR\tLAST_YEAR\tVAR_NO\tVAR_NAME\tDESCRIPTION\tTYPE\tTAM\tRANGE_MIN\tRANGE_MAX\tCATEGORIES\tSELECT\tOBS\tSCRIPT
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#1\tNU_ANO\tAno do Censo\tinteger\t4\t2008\t2020\tAnos do censo entre 2008 e 2020\t1\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#2\tID_MATRICULA\tCódigo único da matrícula\tinteger\t2\t0\t99\tNA\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#3\tCPF_MASC\tCPF mascarado no formato char(32)\tinteger\t2\t0\t99\tNA\t1\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#4\tCO_PESSOA_FISICA\tCódigo do aluno (ID_INEP)\tinteger\t2\t0\t99\tNA\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#5\tTP_ETAPA_ENSINO\tEtapa de ensino da matrícula\tinteger\t2\t1\t74\t\"1 - Educação Infantil - Creche\n2 - Educação Infantil - Pré-escola\n4 - Ensino Fundamental de 8 anos - 1ª Série\n5 - Ensino Fundamental de 8 anos - 2ª Série\n6 - Ensino Fundamental de 8 anos - 3ª Série\n7 - Ensino Fundamental de 8 anos - 4ª Série\n8 - Ensino Fundamental de 8 anos - 5ª Série\n9 - Ensino Fundamental de 8 anos - 6ª Série\n10 - Ensino Fundamental de 8 anos - 7ª Série\n11 - Ensino Fundamental de 8 anos - 8ª Série\n14 - Ensino Fundamental de 9 anos - 1º Ano\n15 - Ensino Fundamental de 9 anos - 2º Ano\n16 - Ensino Fundamental de 9 anos - 3º Ano\n17 - Ensino Fundamental de 9 anos - 4º Ano\n18 - Ensino Fundamental de 9 anos - 5º Ano\n19 - Ensino Fundamental de 9 anos - 6º Ano\n20 - Ensino Fundamental de 9 anos - 7º Ano\n21 - Ensino Fundamental de 9 anos - 8º Ano\n41 - Ensino Fundamental de 9 anos - 9º Ano\n25 - Ensino Médio - 1º ano/1ª Série\n26 - Ensino Médio - 2º ano/2ª Série\n27 - Ensino Médio - 3ºano/3ª Série\n28 - Ensino Médio - 4º ano/4ª Série\n29 - Ensino Médio - Não Seriada\n30 - Curso Técnico Integrado (Ensino Médio Integrado) 1ª Série\n31 - Curso Técnico Integrado (Ensino Médio Integrado) 2ª Série\n32 - Curso Técnico Integrado (Ensino Médio Integrado) 3ª Série\n33 - Curso Técnico Integrado (Ensino Médio Integrado) 4ª Série\n34 - Curso Técnico Integrado (Ensino Médio Integrado) Não Seriada\n35 - Ensino Médio - Modalidade Normal/Magistério 1ª Série\n36 - Ensino Médio - Modalidade Normal/Magistério 2ª Série\n37 - Ensino Médio - Modalidade Normal/Magistério 3ª Série\n38 - Ensino Médio - Modalidade Normal/Magistério 4ª Série\n39 - Curso Técnico - Concomitante\n40 - Curso Técnico - Subsequente\n65 - EJA - Ensino Fundamental - Projovem Urbano\n67 - Curso FIC integrado na modalidade EJA  - Nível Médio\n68 - Curso FIC Concomitante \n69 - EJA - Ensino Fundamental - Anos Iniciais\n70 - EJA - Ensino Fundamental - Anos Finais\n71 - EJA - Ensino Médio\n72 - EJA - Ensino Fundamental  - Anos iniciais e Anos finais\n73 - Curso FIC integrado na modalidade EJA - Nível Fundamental (EJA integrada à Educação Profissional de Nível Fundamental) \n74 - Curso Técnico Integrado na Modalidade EJA (EJA integrada à Educação Profissional de Nível Médio)\"\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#6\tIN_REGULAR\tModo, maneira ou metodologia de ensino...\tbinary\t1\t0\t1\t\"0 - Não\n1 - Sim\"\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#7\tID_TURMA\tCódigo único da Turma 3\tinteger\t2\t\t\tTurmas do evento 55...\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#8\tCO_ENTIDADE\tCódigo da Escola 3\tinteger\t2\t\t\tEscolas que não...\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#9\tCO_ORGAO_REGIONAL\tCódigo do Órgão Regional de Ensino\tinteger\t2\t\t\tNA\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#10\tCO_UF\tCódigo UF da escola\tinteger\t2\t\t\tNA\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#11\tCO_MUNICIPIO\tCódigo Município da escola\tinteger\t2\t\t\tNA\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#12\tTP_DEPENDENCIA\tDependência Administrativa (Escola)\tinteger\t1\t1\t4\t\"1 - Federal\n2 - Estadual\n3 - Municipal\n4 - Privada\"\t1\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#13\tTP_SITUACAO\tSituação de rendimento ou movimento do aluno...\tinteger\t1\t2\t9\t\"2 - Abandono...\n3 - Falecido...\n4 - Reprovado...\n5 - Aprovado...\n9 - Sir...\"\t\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#14\tIN_CONCLUINTE\tAluno que tenha sido aprovado...\tbinary\t1\t0\t1\t\"0 - Não\n1 - Sim\"\t1\t\t
CESCOLAR\tCESCOLAR_BAS_SITUACAO\t2008\t2020\t#15\tIN_TRANSFERIDO\tAluno que foi para outra escola...\tbinary\t1\t0\t1\t\"0 - Não\n1 - Sim\"\t\t\t
")


```

# Função para gerar uma variável com base no dicionário

simula_var \<- function(var, type, domain, n) { if (type == "int") { bounds \<- str_split(domain, "-", simplify = TRUE) %\>% as.integer() return(sample(seq(bounds[1], bounds[2]), n, replace = TRUE)) } else if (type == "factor") { levels \<- str_split(domain, ";\|,", simplify = TRUE) %\>% str_trim() return(sample(levels, n, replace = TRUE)) } else if (type == "string") { return(ch_name(n = n)) \# nomes aleatórios } else { return(rep(NA, n)) } }

# Número de linhas da base simulada

n \<- 1000

# Simular todas as variáveis

dados_simulados \<- map2_dfc(dic$var, seq_along(dic$var), \~{ col \<- simula_var(dic$var[.y], dic$type[.y], dic$domain[.y], n)
  tibble(!!dic$var[.y] := col) })

# Visualizar

glimpse(dados_simulados)

# Salvar

write_csv(dados_simulados, "dados_simulados.csv")
