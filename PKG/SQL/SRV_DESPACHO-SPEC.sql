--------------------------------------------------------
--  File created - Thursday-June-21-2018   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Package SRV_DESPACHO
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE "DADCORPDS"."SRV_DESPACHO" AS

  PROCEDURE DESPACHOS_ACTIVOS_CLIENTE(RUT             IN NUMBER,
                                      IN_PROCESS_LUW  IN CHAR DEFAULT 'T',
                                      RSDESPACHOS     OUT SYS_REFCURSOR,
                                      RSRETIROS       OUT SYS_REFCURSOR,
                                      OUT_STATUS_CODE OUT NUMBER,
                                      OUT_STATUS_MSG  OUT VARCHAR);

  PROCEDURE COBERTURA_CONSULTAR(XML             IN XMLTYPE,
                                IN_PROCESS_LUW  IN CHAR DEFAULT 'T',
                                OUT_STATUS_CODE OUT NUMBER,
                                OUT_STATUS_MSG  OUT VARCHAR,
                                RS              OUT SYS_REFCURSOR);

  PROCEDURE FLETE_CALCULAR("xml"           IN SYS.XMLTYPE,
                           IN_PROCESS_LUW  IN CHAR DEFAULT 'T',
                           "rs"            OUT SYS_REFCURSOR,
                           OUT_STATUS_CODE OUT NUMBER,
                           OUT_STATUS_MSG  OUT VARCHAR);

  PROCEDURE DESPACHOS_RETRASADOS(IN_FECHA        IN DATE,
                                 IN_PROCESS_LUW  IN CHAR DEFAULT 'T',
                                 OUT_STATUS_CODE OUT NUMBER,
                                 OUT_STATUS_MSG  OUT VARCHAR);

  PROCEDURE DESPACHOS_VALIDOS(IN_FECHA        IN DATE,
                              IN_PROCESS_LUW  IN CHAR DEFAULT 'T',
                              OUT_STATUS_CODE OUT NUMBER,
                              OUT_STATUS_MSG  OUT VARCHAR);

END SRV_DESPACHO;


/
