---

---

Este código é responsável por gerar alguns datasets que vamos usar para ir testando ficticiamente o código principal. A partir dos dicionários de dados disponobilizados pelo INEP, vamos gerar os datasets com poquíssimas observações, apenas para fazer testes de manipulação e testar o nosso código antes de enviar para o SEDAP.




```{r}
library(tidyverse)
library(charlatan) 
```

# para gerar dados falsos
set.seed(6387)

# Exemplo simples de dicionário

```{r}
library(tibble)

variaveis_cescolar <- tibble::tibble(
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
simula_var <- function(var, type, domain, n) {
  if (type == "int") {
    bounds <- str_split(domain, "-", simplify = TRUE) %>% as.integer()
    return(sample(seq(bounds[1], bounds[2]), n, replace = TRUE))
  } else if (type == "factor") {
    levels <- str_split(domain, ";|,", simplify = TRUE) %>% str_trim()
    return(sample(levels, n, replace = TRUE))
  } else if (type == "string") {
    return(ch_name(n = n)) # nomes aleatórios
  } else {
    return(rep(NA, n))
  }
}

# Número de linhas da base simulada
n <- 1000

# Simular todas as variáveis
dados_simulados <- map2_dfc(dic$var, seq_along(dic$var), ~{
  col <- simula_var(dic$var[.y], dic$type[.y], dic$domain[.y], n)
  tibble(!!dic$var[.y] := col)
})

# Visualizar
glimpse(dados_simulados)

# Salvar
write_csv(dados_simulados, "dados_simulados.csv")
