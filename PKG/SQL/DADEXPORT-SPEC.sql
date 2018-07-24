--------------------------------------------------------
--  File created - Thursday-June-21-2018   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Package DADEXPORT
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE "DADCORPDS"."DADEXPORT" is
  PROCEDURE ENVIAR_PRT
  (
  IN_FECHA_PROCESO        IN  DATE,
  IN_AUDIT_NUMBER         IN  NUMBER DEFAULT 0,
  IN_CANT_REG              IN  NUMBER,
  IN_PROCESS_LUW          IN  VARCHAR2 DEFAULT 'T',
    "rs"                    out sys_refcursor,
  OUT_KEYMAES_INICIO      OUT VARCHAR2,
  OUT_KEYMAES_FINAL       OUT VARCHAR2,
  OUT_CUADRAT_NUMLIN_ZON  OUT NUMBER,
  OUT_CUADRAT_CAMPO_ZON   OUT VARCHAR2,
  OUT_CUADRAT_SUMA_ZON    OUT NUMBER,
  OUT_STATUS_CODE         OUT NUMBER,
  OUT_STATUS_MSG          OUT VARCHAR2
  );

  PROCEDURE ENVIAR_UNIGIS(
  IN_FECHA_PROCESO        IN  DATE,
  IN_AUDIT_NUMBER         IN  NUMBER DEFAULT 0,
  IN_CANT_REG             IN  NUMBER,
  IN_PROCESS_LUW          IN  VARCHAR2 DEFAULT 'T',
  rs_hdr                  OUT SYS_REFCURSOR,
  rs_dtl                  OUT SYS_REFCURSOR,
  OUT_KEYMAES_INICIO      OUT VARCHAR2,
  OUT_KEYMAES_FINAL       OUT VARCHAR2,
  OUT_CUADRAT_NUMLIN_ZON  OUT NUMBER,
  OUT_CUADRAT_CAMPO_ZON   OUT VARCHAR2,
  OUT_CUADRAT_SUMA_ZON    OUT NUMBER,
  OUT_STATUS_CODE         OUT NUMBER,
  OUT_STATUS_MSG          OUT VARCHAR2);

    PROCEDURE ENVIAR_RESERVA
  (
  IN_FECHA_PROCESO        IN  DATE,
  IN_AUDIT_NUMBER         IN  NUMBER DEFAULT 0,
  IN_CANT_REG              IN  NUMBER,
  IN_PROCESS_LUW          IN  VARCHAR2 DEFAULT 'T',
  "rs"                    out sys_refcursor,
  OUT_KEYMAES_INICIO      OUT VARCHAR2,
  OUT_KEYMAES_FINAL       OUT VARCHAR2,
  OUT_CUADRAT_NUMLIN_ZON  OUT NUMBER,
  OUT_CUADRAT_CAMPO_ZON   OUT VARCHAR2,
  OUT_CUADRAT_SUMA_ZON    OUT NUMBER,
  OUT_STATUS_CODE         OUT NUMBER,
  OUT_STATUS_MSG          OUT VARCHAR2
  );

   PROCEDURE ENVIAR_RESERVA_NEW
  (
  IN_KEY_ORIGEN           IN varchar2,
  IN_CANTIDAD_REG         IN NUMBER,
  IN_PROCESO_LUW          IN CHAR DEFAULT 'T',
  OUT_CUADRAT_NUMLIN      OUT number,
  OUT_CUADRAT_CAMPO       OUT varchar2,
  OUT_CUADRAT_SUMA_ZON    OUT number,
  OUT_STATUS_CODE         out number,
  OUT_STATUS_MSG          out varchar2,
  "rs"  out sys_refcursor
   );

    PROCEDURE ENVIAR_CAMBIOESTADO_RESERVA
  (
  IN_FECHA_PROCESO        IN  DATE,
  IN_AUDIT_NUMBER         IN  NUMBER DEFAULT 0,
  IN_CANT_REG              IN  NUMBER,
  IN_PROCESS_LUW          IN  VARCHAR2 DEFAULT 'T',
  "rs"                    out sys_refcursor,
  OUT_KEYMAES_INICIO      OUT VARCHAR2,
  OUT_KEYMAES_FINAL       OUT VARCHAR2,
  OUT_CUADRAT_NUMLIN_ZON  OUT NUMBER,
  OUT_CUADRAT_CAMPO_ZON   OUT VARCHAR2,
  OUT_CUADRAT_SUMA_ZON    OUT NUMBER,
  OUT_STATUS_CODE         OUT NUMBER,
  OUT_STATUS_MSG          OUT VARCHAR2
  );

  Procedure ENVIAR_HOJA_RUTA
  (
  in_key_origen          in wli_event_itfkey_dad.key_origen%TYPE,
   in_cantidad_reg        in  number,                 -- cantidad de registros a buscar
   in_proceso_luw         in  char default 'T',       -- T: commit; F: no se ejecuta commit
   out_cuadrat_numlin     out number,                 -- cantidad de lineas
   out_cuadrat_campo      out varchar2,               -- campo a cuadrar
   out_cuadrat_suma_zon   out number,                 -- suma de cantidad en relacion a al campo a cuadrar
   OUT_STATUS_CODE         out number,
   OUT_STATUS_MSG          out varchar2,
   "rs"  out sys_refcursor);

   PROCEDURE GET_RESERVA(
    rutNum     IN NUMBER,
    rutVer     IN VARCHAR2,
    reserva    IN NUMBER,
    rsCab   out sys_refcursor,
    rsDet   out sys_refcursor,
    rsReq   out sys_refcursor,
    OUT_STATUS_CODE OUT NUMBER,
    OUT_STATUS_MSG OUT VARCHAR);

   PROCEDURE GET_RESERVA2(
    rutNum            IN NUMBER,
    rutVer            IN VARCHAR2,
    reserva           IN NUMBER,
    rsCab             OUT SYS_REFCURSOR,
    rsCabHist         OUT SYS_REFCURSOR,
    rsDet             OUT SYS_REFCURSOR,
    rsDetHist         OUT SYS_REFCURSOR,
    rsReq             OUT SYS_REFCURSOR,
    OUT_STATUS_CODE   OUT NUMBER,
    OUT_STATUS_MSG    OUT VARCHAR);

     PROCEDURE ENVIAR_AJUSTE
  (
  in_key_origen          in wli_event_itfkey_dad.key_origen%TYPE,
   in_cantidad_reg        in  number,                 -- cantidad de registros a buscar
   in_proceso_luw         in  char default 'T',       -- T: commit; F: no se ejecuta commit
   out_cuadrat_numlin     out number,                 -- cantidad de lineas
   out_cuadrat_campo      out varchar2,               -- campo a cuadrar
   out_cuadrat_suma_zon   out number,                 -- suma de cantidad en relacion a al campo a cuadrar
   OUT_STATUS_CODE         out number,
   OUT_STATUS_MSG          out varchar2,
   "rs"  out sys_refcursor
  );

   PROCEDURE CONFIRMA_FOLIO
  (
  in_key_origen          in wli_event_itfkey_dad.key_origen%TYPE,
   in_cantidad_reg        in  number,                 -- cantidad de registros a buscar
   in_proceso_luw         in  char default 'T',       -- T: commit; F: no se ejecuta commit
   out_cuadrat_numlin     out number,                 -- cantidad de lineas
   out_cuadrat_campo      out varchar2,               -- campo a cuadrar
   out_cuadrat_suma_zon   out number,                 -- suma de cantidad en relacion a al campo a cuadrar
   OUT_STATUS_CODE         out number,
   OUT_STATUS_MSG          out varchar2,
   "rs"  out sys_refcursor
  );

     PROCEDURE ENVIAR_GDD
  (
  in_key_origen          in wli_event_itfkey_dad.key_origen%TYPE,
   in_proceso_luw         in  char default 'T',       -- T: commit; F: no se ejecuta commit
   OUT_STATUS_CODE         out number,
   OUT_STATUS_MSG          out varchar2,
   "rs_hdr"  out sys_refcursor,
   "rs_dtl"  out sys_refcursor
  );

PROCEDURE INFOPALET(
  PCEMPRESA       IN VARCHAR2,
  PCNUMEXPE       IN NUMBER,
  PCNUPALET       IN VARCHAR2,
  PQTBULTOS       IN NUMBER,
  XPARAMETR       IN VARCHAR2,
  RESULTADO            out CLOB);

PROCEDURE TCKRESERVAHDR(
  vrut         IN  VARCHAR2,
  vnum_reserva IN  NUMBER,
  out_rsv OUT SYS_REFCURSOR);

PROCEDURE TCKRESERVADTL(
  vnum_reserva    IN  eventotck_dtl.num_reserva%TYPE,
  vprd_lvl_number IN  eventotck_dtl.prd_lvl_number%TYPE,
  out_prod        OUT SYS_REFCURSOR,
  out_hist        OUT SYS_REFCURSOR);

PROCEDURE ENVIAR_CAMBIOFECHA
  (
  IN_FECHA_PROCESO        IN  DATE,
  IN_AUDIT_NUMBER         IN  NUMBER DEFAULT 0,
  IN_CANT_REG              IN  NUMBER,
  IN_PROCESS_LUW          IN  VARCHAR2 DEFAULT 'T',
  "rs"                    out sys_refcursor,
  OUT_KEYMAES_INICIO      OUT VARCHAR2,
  OUT_KEYMAES_FINAL       OUT varchar2,
  OUT_CUADRAT_NUMLIN_CFECHA  OUT number,
  OUT_CUADRAT_CAMPO_CFECHA   OUT varchar2,
  OUT_CUADRAT_SUMA_CFECHA    OUT NUMBER,
  OUT_STATUS_CODE         OUT NUMBER,
  OUT_STATUS_MSG          OUT VARCHAR2
  );

PROCEDURE ENVIAR_CAMION
  (
  in_fecha_proceso       IN  DATE,
  in_audit_number        IN  NUMBER DEFAULT 0,
  in_cant_reg              IN  NUMBER,
  in_process_luw         IN  VARCHAR2 DEFAULT 'T',
  rs                  OUT SYS_REFCURSOR,
  out_cuadrat_numlin       OUT NUMBER,
  out_cuadrat_campo        OUT VARCHAR2,
  out_cuadrat_suma         OUT NUMBER,
  out_status_code        OUT NUMBER,
  out_status_msg         OUT VARCHAR2
  );

PROCEDURE ENVIAR_DESP_RETRASADOS(
  IN_FECHA_DESPACHO       IN  DATE,
  IN_FECHA_PROCESO        IN  DATE,
  IN_AUDIT_NUMBER         IN  NUMBER DEFAULT 0,
  IN_CANT_REG             IN  NUMBER,
  IN_PROCESS_LUW          IN  VARCHAR2 DEFAULT 'T',
  "rs"                    OUT SYS_REFCURSOR,
  OUT_KEYMAES_INICIO      OUT VARCHAR2,
  OUT_KEYMAES_FINAL       OUT VARCHAR2,
  OUT_CUADRAT_NUMLIN      OUT NUMBER,
  OUT_CUADRAT_CAMPO       OUT VARCHAR2,
  OUT_CUADRAT_SUMA        OUT NUMBER,
  OUT_STATUS_CODE         OUT NUMBER,
  OUT_STATUS_MSG          OUT VARCHAR2
);

PROCEDURE ENVIAR_DESP_VALIDOS(
  IN_FECHA_DESPACHO       IN  DATE,
  IN_FECHA_PROCESO        IN  DATE,
  IN_AUDIT_NUMBER         IN  NUMBER DEFAULT 0,
  IN_CANT_REG             IN  NUMBER,
  IN_PROCESS_LUW          IN  VARCHAR2 DEFAULT 'T',
  "rs"                    OUT SYS_REFCURSOR,
  OUT_KEYMAES_INICIO      OUT VARCHAR2,
  OUT_KEYMAES_FINAL       OUT VARCHAR2,
  OUT_CUADRAT_NUMLIN      OUT NUMBER,
  OUT_CUADRAT_CAMPO       OUT VARCHAR2,
  OUT_CUADRAT_SUMA        OUT NUMBER,
  OUT_STATUS_CODE         OUT NUMBER,
  OUT_STATUS_MSG          OUT VARCHAR2
);

PROCEDURE PRC_UBICACIONESGEO(
  out_status_code  OUT NUMBER,
  out_status_msg   OUT VARCHAR2
);

PROCEDURE PRC_COBERTURACC(
  out_status_code  OUT NUMBER,
  out_status_msg   OUT VARCHAR2
);

PROCEDURE PRC_CAPACIDADES(
  out_status_code  OUT NUMBER,
  out_status_msg   OUT VARCHAR2
);

END DADEXPORT;


/
