---
title: "Sedap-INEP - Projeto tales.6387"
format: html
---

Este script tem como objetivo acompanhar, testar e documentar, do lado de fora da sala segura do INEP, o desenvolvimento do código que está sendo elaborado internamente.

A intenção é que, caso tudo transcorra como previsto, possamos extrair o script final — atualmente sendo desenvolvido em um documento Quarto dentro da sala — para uso e replicação externa.

Além disso, este documento funciona como um caderno de campo: serve tanto para preparar materiais a serem utilizados dentro da sala quanto para registrar os avanços realizados no ambiente restrito, facilitando o acompanhamento e a reconstrução do trabalho fora dele.

# Início do Script

O Script se inicia na sala do Sedap com: 1. Definição do Diretório de Trabalho (especialmente importante pois lá não é possível usar Rproject e temos que salvar os arquivos em pastas específicas ou ele pode ser deletado). Os dados ficam disponíveis via acesso na rede em disco de nome "T:/"

Depis demos que verificar se os pacotes necessário s do tidyverse foram instalados, se não é imporssível ler os bancos de dados CSV

```{R}
getwd()
setwd("T:/")
installed.packages()
```

Os pacotes na sala do INEP não podem ser baixados via CRAN, ao invés disso eles são disponibilziados em arquivos diretamente no computador, que, no entanto, ainda não tive acesso.

```{R}

#Código de instalação dos pacotes no R do Sedap

#Como não é o caso aqui, vou chamar os pacotes que utilizo normalmente
library(tidyverse)

```

Depois de instalar os pacotes podemos finalmente ler as bases de dados usando a função readr

```{R}
readr::read_delim("1. Bases Enem\ENEM_2010.csv", delim=";")
#Essa forma de leitura se repete para todas as outras bases
```

## Tratamento e cruzamento das bases de dados

Após realizar a leitura da primeira base de dados, seja do Censo da Educação Básica (doravante CEB) ou do Censo da Educação Superio (CES) é necessário removar a maioria das colunas (que não tem uso para a pesquisa e aumentam muito o peso do dataset) e realizar o merge através da função *left_join*.


### Censo Escolar

Vamos iniciar carregando a base do CEB e excluindo as colunas que não nos interessam:
  
```{R}
colnamesCEB_2009 <- readr::read_delim("1. Bases CEB\CEB_2009.csv", delim=";", n_max = 0)
print(colnamesCEB_2009)


colunas_desejadas <- c("ID_ALUNO", "TP_COR_RACA")

# Lê somente essas colunas
dados <- fread("arquivo.csv", select = colunas_desejadas)


#Essa forma de leitura se repete para todas as outras bases
```




library(readr)

nomes <- names(read_csv("arquivo.csv", n_max = 0))
print(nomes)


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

