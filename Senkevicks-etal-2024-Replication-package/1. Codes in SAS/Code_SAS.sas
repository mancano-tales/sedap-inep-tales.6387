
libname DATADEED meta library="DATA_DEED"; RUN;
libname DATAENEM meta library="DATA_ENEM"; RUN;

/*****************************************************************************************************/
/***************************/
/***************************/
/*     CENSO ESCOLAR       */
/***************************/
/***************************/

/* Preparando dados de concluintes do ensino médio no Censo Escolar */
PROC SQL;
   CREATE TABLE WORK.CENSO_1 AS 
   SELECT t1.NU_ANO_CENSO, 
          t1.CO_PESSOA_FISICA, 
		  t1.CHAVE_UNICA /* Esta é variável identificadora, acessível somente via SEDAP/INEP */
          t1.NU_IDADE, 
          t1.TP_SEXO, 
          t1.TP_COR_RACA, 
          t1.CO_UF, 
          t1.CO_MUNICIPIO, 
          t1.CO_ENTIDADE, 
          t1.TP_DEPENDENCIA, 
          t1.TP_LOCALIZACAO, 
          t1.IN_PROFISSIONALIZANTE, 
          t1.TP_ETAPA_ENSINO, 
          t1.TP_SITUACAO
      FROM DATADEED.BAS_SITUACAO t1
      WHERE t1.NU_ANO_CENSO IN 
           (
           2009,
           2010,
           2011,
           2012
           ) AND t1.IN_CONCLUINTE = 1 AND t1.TP_SITUACAO IN 
           (
           5,
           9
           ) AND t1.TP_ETAPA_ENSINO IN 
           (
           27,
           28,
           29,
           32,
           33,
           34,
           37,
           38
           ) AND t1.NU_IDADE BETWEEN 15 AND 29;
QUIT;

/*****************************************************************************************************/
/***************************/
/***************************/
/*          ENEM           */
/***************************/
/***************************/

/* Preparação dos dados de inscritos no Enem, filtrando apenas participantes */
PROC SQL;
   CREATE TABLE WORK.ENEM_1 AS
 
   SELECT t1.IEIN_CO_INSCRICAO, 
          t1.IEIN_CHAVE_UNICA, /* Esta é variável identificadora, acessível somente via SEDAP/INEP */
          t1.IEIN_NU_ANO, 
          t1.IEIN_CO_COR_RACA

      FROM DATAENEM.INT_ENM_INSCRICAO t1
      WHERE t1.IEIN_NU_ANO BETWEEN 2009 AND 2016 
            AND t1.IEIN_IN_PARTICIPANTE = 1;
QUIT;

/* Trazer os dados de desempenho no Enem */
PROC SQL;
   CREATE TABLE WORK.ENEM_2 AS 

   SELECT t1.IEIN_CO_INSCRICAO,
          t1.IEIN_CHAVE_UNICA,
          t1.IEIN_NU_ANO, 
          t1.IEIN_CO_COR_RACA 

          t2.IERE_VL_NOTA_CN, 
          t2.IERE_VL_NOTA_CH, 
          t2.IERE_VL_NOTA_LC, 
          t2.IERE_VL_NOTA_MT

      FROM DATAENEM.ENEM_1 t1
      LEFT JOIN DATAENEM.INT_ENM_RESULTADO t2 ON (t1.IEIN_CO_INSCRICAO = t2.IEIN_CO_INSCRICAO);
QUIT;

/* Trazer os dados do questionário do Enem */
PROC SQL;
   CREATE TABLE WORK.ENEM_3 AS 

   SELECT t1.IEIN_CO_INSCRICAO,
          t1.IEIN_CHAVE_UNICA,
          t1.IEIN_NU_ANO, 
          t1.IEIN_CO_COR_RACA, 
          t1.IERE_VL_NOTA_CN, 
          t1.IERE_VL_NOTA_CH, 
          t1.IERE_VL_NOTA_LC, 
          t1.IERE_VL_NOTA_MT,

          t2.IEQU_QUESTAO_01, /* Escolaridade do pai */
		  t2.IEQU_QUESTAO_02, /* Escolaridade da mãe */
          t2.IEQU_QUESTAO_05, /* Número de habitantes no domicílio */
		  t2.IEQU_QUESTAO_06, /* Faixas de renda domiciliar */
		  t2.IEQU_QUESTAO_26, /* Exercício de atividade remunerada */
		  t2.IEQU_QUESTAO_42, /* Em que tipo de escola frequentou o Ensino Fundamental */
		  t2.IEQU_QUESTAO_47  /* Em que tipo de escola frequentou o Ensino Médio */

      FROM DATAENEM.ENEM_2 t1
      LEFT JOIN DATAENEM.INT_ENM_QUESTIONARIO t2 ON (t1.IEIN_CO_INSCRICAO = t2.IEIN_CO_INSCRICAO);
QUIT;

/*****************************************************************************************************/
/***************************/
/***************************/
/*  CENSO ESCOLAR + ENEM   */
/***************************/
/***************************/

PROC SQL;
   CREATE TABLE WORK.CENSO_ENEM_1 AS 
   SELECT t1.NU_ANO_CENSO, 
          t1.CO_PESSOA_FISICA, 
		  t1.CHAVE_UNICA
          t1.NU_IDADE, 
          t1.TP_SEXO, 
          t1.TP_COR_RACA, 
          t1.CO_UF, 
          t1.CO_MUNICIPIO, 
          t1.CO_ENTIDADE, 
          t1.TP_DEPENDENCIA, 
          t1.TP_LOCALIZACAO, 
          t1.IN_PROFISSIONALIZANTE, 
          t1.TP_ETAPA_ENSINO, 
          t1.TP_SITUACAO,

          t2.IEIN_CO_INSCRICAO,
          t2.IEIN_NU_ANO, 
          t2.IEIN_CO_COR_RACA, 
          t2.IERE_VL_NOTA_CN, 
          t2.IERE_VL_NOTA_CH, 
          t2.IERE_VL_NOTA_LC, 
          t2.IERE_VL_NOTA_MT,
          t2.IEQU_QUESTAO_01, 
          t2.IEQU_QUESTAO_02, 
          t2.IEQU_QUESTAO_05, 
          t2.IEQU_QUESTAO_06, 
          t2.IEQU_QUESTAO_26, 
          t2.IEQU_QUESTAO_42, 
          t2.IEQU_QUESTAO_47

FROM WORK.CENSO_1 t1
LEFT JOIN WORK.ENEM_3 t2 ON (t1.CHAVE_UNICA = t2.CHAVE_UNICA);
QUIT; 

/* A base acima (WORK.CENSO_ENEM_1) estará em formato long com as informações
/* de distintas edições do Enem. O próximo passo é reestruturar para um formato
/* wide e tratar as variáveis. A reestruturação segue o comando abaixo. */

PROC TRANSPOSE 
DATA=WORK.CENSO_ENEM_1 
OUT=WORK.CENSO_ENEM_2
PREFIX=IEIN_;
BY NU_ANO_CENSO CO_PESSOA_FISICA CHAVE_UNICA NU_IDADE TP_SEXO TP_COR_RACA CO_UF 
       CO_MUNICIPIO CO_ENTIDADE TP_DEPENDENCIA TP_LOCALIZACAO IN_PROFISSIONALIZANTE 
       TP_ETAPA_ENSINO TP_SITUACAO;
ID IEIN_NU_ANO;
VAR IEIN_CO_INSCRICAO IEIN_CO_COR_RACA IERE_VL_NOTA_CN IERE_VL_NOTA_CH 
    IERE_VL_NOTA_LC IERE_VL_NOTA_MT IEQU_QUESTAO_01 IEQU_QUESTAO_02 
    IEQU_QUESTAO_05 IEQU_QUESTAO_06 IEQU_QUESTAO_26 IEQU_QUESTAO_42 
    IEQU_QUESTAO_47;
QUIT;

/* Os próximos cruzamentos e tratamentos de variáveis serão realizados em Stata (vide próxima pasta) */
PROC EXPORT DATA=CENSO_ENEM_2
    OUTFILE='/caminho/para/o/arquivo/censo_enem.csv'
    DBMS=CSV REPLACE;
RUN;


/*****************************************************************************************************/
/***************************/
/***************************/
/*  CENSO DA ED. SUPERIOR  */
/***************************/
/***************************/

/* Preparar a base de matrículas de ingressantes no Censo da Educação Superior. */
/********************************************/
/* SELEÇÃO DAS VARIÁVEIS DAS EDIÇÕES DO CES */
/*              CES 2010-2017               */
/********************************************/

/* Primeiramente, vamos selecionar apenas as variáveis que nos interessam na tabela de aluno. */
PROC SQL;
   CREATE TABLE WORK.ALUNO_2010_2017 AS 
   SELECT t1.NU_ANO_CENSO, 
          t1.CO_IES, 
          t1.TP_CATEGORIA_ADMINISTRATIVA, 
          t1.TP_ORGANIZACAO_ACADEMICA, 
          t1.CO_CURSO, 
          t1.TP_TURNO, 
          t1.TP_GRAU_ACADEMICO, 
          t1.TP_MODALIDADE_ENSINO, 
          t1.TP_NIVEL_ACADEMICO, 
          t1.CO_AREA_GERAL, 
          t1.CO_AREA_ESPECIFICA, 
          t1.CO_AREA_DETALHADA, 
          t1.CO_ROTULO, 
          t1.CO_ALUNO, 
          t1.CO_ALUNO_CURSO, 
          t1.TP_COR_RACA, 
          t1.TP_SITUACAO
      FROM DATADEED.SUP_ALUNO AS t1;
QUIT;


/***********************************************/
/*    ADICIONAR A CHAVE UNICA DE CADA ALUNO    */
/*               CES 2010-2017                 */
/***********************************************/

/* Para fazer isso, usarei a lista de chaves únicas que preparei a partir dos dados protegidos. */
PROC SQL;
   CREATE TABLE WORK.ALUNO_2010_2017_CHAVE_UNICA AS 
   SELECT t1.NU_ANO_CENSO,
          t1.CO_ALUNO,
          
          t2.CHAVE_UNICA,
           
          t1.CO_ALUNO_CURSO,
          t1.TP_COR_RACA,  
          t1.CO_IES, 
          t1.TP_CATEGORIA_ADMINISTRATIVA, 
          t1.TP_ORGANIZACAO_ACADEMICA, 
          t1.CO_CURSO, 
          t1.TP_TURNO, 
          t1.TP_GRAU_ACADEMICO, 
          t1.TP_MODALIDADE_ENSINO, 
          t1.TP_NIVEL_ACADEMICO, 
          t1.CO_AREA_GERAL, 
          t1.CO_AREA_ESPECIFICA, 
          t1.CO_AREA_DETALHADA, 
          t1.CO_ROTULO, 
          t1.TP_SITUACAO
      FROM WORK.ALUNO_2010_2017 AS t1 LEFT JOIN
      LIBNAME.LISTA_CES_CHAVE_UNICA_2010_2017 AS t2 ON
      (t1.CO_ALUNO = t2.CO_ALUNO);
QUIT;


/***********************************/
/* DIVIDIR A BASE POR ANO DO CENSO */
/*         CES 2010-2017           */
/***********************************/

/* A partir da base geral montada acima, vamos quebrar por edição do CES 2010-2017. */

/* 2010 */
PROC SQL;
   CREATE TABLE WORK.ALUNO_2010 AS 
   SELECT DISTINCT 
          *,
          /* IN_CES_10 */
          (1) AS IN_CES_10
      FROM WORK.ALUNO_2010_2017_CHAVE_UNICA AS t1
      WHERE t1.NU_ANO_CENSO = 2010
      ORDER BY t1.CO_ALUNO,
               t1.TP_CATEGORIA_ADMINISTRATIVA,
               t1.TP_SITUACAO;
RUN; QUIT;

/* 2011 */
PROC SQL;
   CREATE TABLE WORK.ALUNO_2011 AS 
   SELECT DISTINCT 
          *,
          /* IN_CES_11 */
          (1) AS IN_CES_11
      FROM WORK.ALUNO_2010_2017_CHAVE_UNICA AS t1
      WHERE t1.NU_ANO_CENSO = 2011
      ORDER BY t1.CO_ALUNO,
               t1.TP_CATEGORIA_ADMINISTRATIVA,
               t1.TP_SITUACAO;
RUN; QUIT;

/* 2012 */
PROC SQL;
   CREATE TABLE WORK.ALUNO_2012 AS 
   SELECT DISTINCT 
          *,
          /* IN_CES_12 */
          (1) AS IN_CES_12
      FROM WORK.ALUNO_2010_2017_CHAVE_UNICA AS t1
      WHERE t1.NU_ANO_CENSO = 2012
      ORDER BY t1.CO_ALUNO,
               t1.TP_CATEGORIA_ADMINISTRATIVA,
               t1.TP_SITUACAO;
RUN; QUIT;

/* 2013 */
PROC SQL;
   CREATE TABLE WORK.ALUNO_2013 AS 
   SELECT DISTINCT 
          *,
          /* IN_CES_13 */
          (1) AS IN_CES_13
      FROM WORK.ALUNO_2010_2017_CHAVE_UNICA AS t1
      WHERE t1.NU_ANO_CENSO = 2013
      ORDER BY t1.CO_ALUNO,
               t1.TP_CATEGORIA_ADMINISTRATIVA,
               t1.TP_SITUACAO;
RUN; QUIT;

/* 2014 */
PROC SQL;
   CREATE TABLE WORK.ALUNO_2014 AS 
   SELECT DISTINCT 
          *,
          /* IN_CES_14 */
          (1) AS IN_CES_14
      FROM WORK.ALUNO_2010_2017_CHAVE_UNICA AS t1
      WHERE t1.NU_ANO_CENSO = 2014
      ORDER BY t1.CO_ALUNO,
               t1.TP_CATEGORIA_ADMINISTRATIVA,
               t1.TP_SITUACAO;
RUN; QUIT;

/* 2015 */
PROC SQL;
   CREATE TABLE WORK.ALUNO_2015 AS 
   SELECT DISTINCT 
          *,
          /* IN_CES_15 */
          (1) AS IN_CES_15
      FROM WORK.ALUNO_2010_2017_CHAVE_UNICA AS t1
      WHERE t1.NU_ANO_CENSO = 2015
      ORDER BY t1.CO_ALUNO,
               t1.TP_CATEGORIA_ADMINISTRATIVA,
               t1.TP_SITUACAO;
RUN; QUIT;

/* 2016 */
PROC SQL;
   CREATE TABLE WORK.ALUNO_2016 AS 
   SELECT DISTINCT 
          *,
          /* IN_CES_16 */
          (1) AS IN_CES_16
      FROM WORK.ALUNO_2010_2017_CHAVE_UNICA AS t1
      WHERE t1.NU_ANO_CENSO = 2016
      ORDER BY t1.CO_ALUNO,
               t1.TP_CATEGORIA_ADMINISTRATIVA,
               t1.TP_SITUACAO;
RUN; QUIT;

/* 2017 */
PROC SQL;
   CREATE TABLE WORK.ALUNO_2017 AS 
   SELECT DISTINCT 
          *,
          /* IN_CES_17 */
          (1) AS IN_CES_17
      FROM WORK.ALUNO_2010_2017_CHAVE_UNICA AS t1
      WHERE t1.NU_ANO_CENSO = 2017
      ORDER BY t1.CO_ALUNO,
               t1.TP_CATEGORIA_ADMINISTRATIVA,
               t1.TP_SITUACAO;
RUN; QUIT;

/******************************/
/* EXPORTAR AS BASES PARA CSV */
/*       CES 2010-2017        */
/******************************/

PROC EXPORT DATA=ALUNO_2010
    OUTFILE='/caminho/para/o/arquivo/aluno_2010.csv'
    DBMS=CSV REPLACE;
RUN;

PROC EXPORT DATA=ALUNO_2011
    OUTFILE='/caminho/para/o/arquivo/aluno_2011.csv'
    DBMS=CSV REPLACE;
RUN;

PROC EXPORT DATA=ALUNO_2012
    OUTFILE='/caminho/para/o/arquivo/aluno_2012.csv'
    DBMS=CSV REPLACE;
RUN;

PROC EXPORT DATA=ALUNO_2013
    OUTFILE='/caminho/para/o/arquivo/aluno_2013.csv'
    DBMS=CSV REPLACE;
RUN;

PROC EXPORT DATA=ALUNO_2014
    OUTFILE='/caminho/para/o/arquivo/aluno_2014.csv'
    DBMS=CSV REPLACE;
RUN;

PROC EXPORT DATA=ALUNO_2015
    OUTFILE='/caminho/para/o/arquivo/aluno_2015.csv'
    DBMS=CSV REPLACE;
RUN;

PROC EXPORT DATA=ALUNO_2016
    OUTFILE='/caminho/para/o/arquivo/aluno_2016.csv'
    DBMS=CSV REPLACE;
RUN;

PROC EXPORT DATA=ALUNO_2017
    OUTFILE='/caminho/para/o/arquivo/aluno_2017.csv'
    DBMS=CSV REPLACE;
RUN;


