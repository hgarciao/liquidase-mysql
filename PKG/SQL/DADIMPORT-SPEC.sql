--------------------------------------------------------
--  File created - Thursday-June-21-2018   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Package DADIMPORT
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE "DADCORPDS"."DADIMPORT" IS
  -- Consulta Reserva
  PROCEDURE ACTUALIZAR_RESERVA("xml"           IN SYS.XMLTYPE,
                               IN_PROCESS_LUW  IN VARCHAR2,
                               OUT_STATUS_CODE OUT NUMBER,
                               OUT_STATUS_MSG  OUT VARCHAR);

  PROCEDURE ACTUALIZAR_ESTADOS_UNIGIS(
  IN_XML           IN  XMLTYPE,
  IN_PROCESS_LUW   IN  VARCHAR2 DEFAULT 'T',
  OUT_STATUS_CODE  OUT NUMBER,
  OUT_STATUS_MSG   OUT VARCHAR2);

  PROCEDURE PREPARAR_UNIGIS_STAGE(
                                IN_XML           IN  XMLTYPE,
                                IN_PROCESS_LUW   IN  VARCHAR2 DEFAULT 'T',
                                OUT_STATUS_CODE   OUT NUMBER,
                                OUT_STATUS_MSG    OUT VARCHAR2);

  PROCEDURE REGISTRAR_PRERUTEO_UNIGIS(
  IN_XML           IN  XMLTYPE,
  IN_PROCESS_LUW   IN  VARCHAR2 DEFAULT 'T',
  OUT_STATUS_CODE  OUT NUMBER,
  OUT_STATUS_MSG   OUT VARCHAR2);

  PROCEDURE CONFIRMAR_HOJARUTA_UNIGIS(
  IN_XML           IN  XMLTYPE,
  IN_PROCESS_LUW   IN  VARCHAR2 DEFAULT 'T',
  OUT_STATUS_CODE  OUT NUMBER,
  OUT_STATUS_MSG   OUT VARCHAR2);

  PROCEDURE CONFIRMAR_UNIGIS(
  IN_XML           IN XMLTYPE,
  IN_PROCESS_LUW   IN VARCHAR2 DEFAULT 'T',
  OUT_STATUS_CODE  OUT NUMBER,
  OUT_STATUS_MSG   OUT VARCHAR2);

  PROCEDURE INFROMA_CIERRE_CARGA("xml"           IN SYS.XMLTYPE,
                                 IN_PROCESS_LUW  IN VARCHAR2,
                                 OUT_STATUS_CODE OUT NUMBER,
                                 OUT_STATUS_MSG  OUT VARCHAR);

  PROCEDURE INGRESAR_CENTRO_COSTO("xml"           IN SYS.XMLTYPE,
                                  IN_PROCESS_LUW  IN VARCHAR2,
                                  OUT_STATUS_CODE OUT NUMBER,
                                  OUT_STATUS_MSG  OUT VARCHAR);

  PROCEDURE ADD_REQUEST(VARNUMRESERVA   IN NUMBER,
                        VARCODTIPOREQ   IN NUMBER,
                        VARCCREQ        IN NUMBER,
                        VAROBS          IN VARCHAR2,
                        VARLOGINUSR     IN VARCHAR2,
                        OUT_STATUS_CODE OUT NUMBER,
                        OUT_STATUS_MSG  OUT VARCHAR);

PROCEDURE ACTUALIZA_ESTADO_RESERVA(
  "xml"             IN SYS.XMLTYPE,
  "accion"          IN NUMBER,
  IN_PROCESS_LUW    IN VARCHAR2,
  OUT_REPROCESA_NCR OUT NUMBER,
  OUT_STATUS_CODE   OUT NUMBER,
  OUT_STATUS_MSG    OUT VARCHAR);


  PROCEDURE CONSULTA_RESERVA(
 pcanal_venta    IN NUMBER,
 pnum_reserva     IN NUMBER,
 pnum_ordenventa    IN NUMBER,
 in_process_luw  IN VARCHAR2,
 OUT_STATUS_CODE OUT NUMBER,
 OUT_STATUS_MSG  OUT VARCHAR,
 out_cursor_reserva_hdr           OUT sys_refcursor,
 out_cursor_reserva_dtl           OUT sys_refcursor,
 out_cursor_reservaPrima_hdr      OUT sys_refcursor,
 out_cursor_reservaPrima_dtl      OUT sys_refcursor
 );

   PROCEDURE CARGA_FOLIOS(
    PID IN GDD_RANGE.ID%TYPE,
  pFACILITY_ID IN GDD_RANGE.FACILITY_ID%TYPE,
  pCOMPANY_ID  IN GDD_RANGE.COMPANY_ID%TYPE,
  pSTART_DOC_NUMBER  IN GDD_RANGE.START_DOC_NUMBER%TYPE,
  pEND_DOC_NUMBER  IN GDD_RANGE.END_DOC_NUMBER%TYPE,
  pCURRENT_DOC_NUMBER  IN GDD_RANGE.CURRENT_DOC_NUMBER%TYPE,
  pXML  IN GDD_RANGE.XML%TYPE,
  pSTATUS_ID  IN GDD_RANGE.STATUS_ID%TYPE,
  pMOD_USER  IN GDD_RANGE.MOD_USER%TYPE,
 OUT_STATUS_CODE OUT NUMBER,
 OUT_STATUS_MSG  OUT VARCHAR
 );

  PROCEDURE ENTREGA_DOCUMENTO
       (
          ctpdoc                  IN CHAR,
          nfolio_docto            IN VARCHAR2,
          in_cpatente             IN VARCHAR2,
          devento                 IN CHAR,
          nlatitud                IN NUMBER,
          nlongitud               IN NUMBER,
          cnoentrega              IN VARCHAR2,
          nrut_ent                IN VARCHAR2,
          xdvrut_ent              IN VARCHAR2,
          cparentesco             IN VARCHAR2,
          nbultos                 IN VARCHAR2,
          cautoriza               IN CHAR,
          OUT_STATUS_MSG          OUT VARCHAR
       );

END DADIMPORT;


/
