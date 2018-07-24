--------------------------------------------------------
--  File created - Thursday-June-21-2018   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Package Body DADEXPORT
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE BODY "DADCORPDS"."DADEXPORT" IS
PROCEDURE ENVIAR_PRT(
  IN_FECHA_PROCESO        IN  DATE,
  IN_AUDIT_NUMBER         IN  NUMBER DEFAULT 0,
  IN_CANT_REG             IN  NUMBER,
  IN_PROCESS_LUW          IN  VARCHAR2 DEFAULT 'T',
  "rs"                    OUT SYS_REFCURSOR,
  OUT_KEYMAES_INICIO      OUT VARCHAR2,
  OUT_KEYMAES_FINAL       OUT VARCHAR2,
  OUT_CUADRAT_NUMLIN_ZON  OUT NUMBER,
  OUT_CUADRAT_CAMPO_ZON   OUT VARCHAR2,
  OUT_CUADRAT_SUMA_ZON    OUT NUMBER,
  OUT_STATUS_CODE         OUT NUMBER,
  OUT_STATUS_MSG          OUT VARCHAR2) AS
BEGIN
DECLARE
    var_cont_reg   NUMBER;
    --var_estado_zona TMPADMACZON.ESTADO_ZONA%TYPE;
BEGIN
    OUT_STATUS_CODE       := 0;
    OUT_STATUS_MSG        := 'OK';
    var_cont_reg          := 1;
    --var_estado_zona       := '99';
    IF IN_CANT_REG IS NULL THEN
       OUT_STATUS_CODE := 1;
       OUT_STATUS_MSG  := 'DAD: ' ||  'Cantidad de Registros no debe ser nulo';
            RETURN;
    ELSE
        IF IN_CANT_REG < 0 THEN
           OUT_STATUS_CODE := 1;
           OUT_STATUS_MSG  := 'DAD: ' ||  'Cantidad de Registros debe ser mayor a cero';
           RETURN;
        END IF;
    END IF;

    BEGIN
      SELECT MAX(SEQUENCIA_DAD)
      INTO OUT_KEYMAES_FINAL
      FROM wli_stage_preruteo
      WHERE SEQUENCIA_DAD IN (SELECT SEQUENCIA_DAD
                              FROM wli_stage_preruteo
                              WHERE (rownum <= IN_CANT_REG OR IN_CANT_REG=0)
                                AND sequencia_dad > IN_AUDIT_NUMBER)
      ;
    EXCEPTION
       WHEN NO_DATA_FOUND THEN
            OUT_STATUS_CODE := 0;
            OUT_STATUS_MSG  := 'DAD: ' ||  'No Existen Datos Nuevos en la Tabla wli_stage_preruteo para la seq: '||IN_AUDIT_NUMBER;
       WHEN OTHERS THEN
            OUT_STATUS_CODE := 1;
            OUT_STATUS_MSG  := 'DAD: ' ||  SQLERRM;
            RETURN;
    END;

    BEGIN
      SELECT SUM(NUM_JAULA), COUNT(sequencia_dad), 'num_jaula'
      INTO OUT_CUADRAT_SUMA_ZON , OUT_CUADRAT_NUMLIN_ZON,OUT_CUADRAT_CAMPO_ZON
      FROM wli_stage_preruteo
      WHERE (sequencia_dad <= OUT_KEYMAES_FINAL)
        AND sequencia_dad > IN_AUDIT_NUMBER
      ;
    EXCEPTION
       WHEN NO_DATA_FOUND THEN
            OUT_STATUS_CODE := 0;
             OUT_STATUS_MSG  := 'DAD: ' ||  'No Existen Datos Nuevos en la Tabla wli_stage_preruteo para la seq: '||IN_AUDIT_NUMBER;
       WHEN OTHERS THEN
            OUT_STATUS_CODE := 1;
            OUT_STATUS_MSG  := 'DAD: ' ||  SQLERRM;
            RETURN;
    END;

    IF OUT_KEYMAES_FINAL IS NOT NULL THEN
      BEGIN
        INSERT INTO WLI_EVENT_ITFTRAMO_DAD(
                  SEQUENCIA_DAD,
                  TIPO_EVENTO,
                  FECHA_AUD_FINAL,
                  ESTADO_DAD,
                  FECHA_CREACION)
         VALUES (OUT_KEYMAES_FINAL,
                 'PRTENVIAR ',
                 SYSDATE,--var_fecha_audit_final, -- Fecha fin de tramo (en tabla auditoria)
                 'PU',
                 SYSDATE);

      EXCEPTION
         WHEN DUP_VAL_ON_INDEX THEN
              OUT_STATUS_CODE := 1;
              OUT_STATUS_MSG  := 'DAD: ' ||  'El Correlativo ya fue procesado valide la tabla WLI_EVENT_ITFTRAMO_DAD';
              RETURN;
              when COLLECTION_IS_NULL THEN
              OUT_STATUS_CODE := 1;
              OUT_STATUS_MSG  := 'DAD: ' ||  'No hay Datos a Procesar';
              return;
           WHEN OTHERS THEN
              OUT_STATUS_CODE := 1;
              OUT_STATUS_MSG  :='DAD: ' ||   SQLERRM;
              RETURN;

      END;
    END IF;

    OPEN  "rs" FOR
    SELECT num_pkt, num_jaula, wharehouse, campo1, campo2, campo3
    FROM wli_stage_preruteo
    WHERE sequencia_dad <= OUT_KEYMAES_FINAL
      AND sequencia_dad > IN_AUDIT_NUMBER;

    IF in_process_luw = 'T' THEN
       COMMIT ;
    END IF;

  END;
END ENVIAR_PRT;

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
  OUT_STATUS_MSG          OUT VARCHAR2)
IS
  VAR_CONT_REG NUMBER;
BEGIN
  OUT_STATUS_CODE   :=  0;
  OUT_STATUS_MSG    :=  'OK';
  VAR_CONT_REG      :=  1;
  --DBMS_OUTPUT.PUT_LINE('OUT_STATUS_MSG = ' || OUT_STATUS_MSG);
  IF IN_CANT_REG IS NULL THEN
    OUT_STATUS_CODE := 1;
    OUT_STATUS_MSG  := 'DAD: ' ||  'Cantidad de Registros no debe ser nulo';
    RETURN;
  ELSE
    IF IN_CANT_REG < 0 THEN
       OUT_STATUS_CODE := 1;
       OUT_STATUS_MSG  := 'DAD: ' ||  'Cantidad de Registros debe ser mayor a cero';
       RETURN;
    END IF;
  END IF;
  --DBMS_OUTPUT.PUT_LINE('OUT_STATUS_MSG = ' || OUT_STATUS_MSG);
  BEGIN
    SELECT MAX(sequencia_dad)
    INTO OUT_KEYMAES_FINAL
    FROM wli_stage_uni_hdr
    WHERE sequencia_dad IN (
          SELECT sequencia_dad
          FROM wli_stage_uni_hdr
          WHERE (ROWNUM <= IN_CANT_REG OR IN_CANT_REG = 0)
            AND sequencia_dad > IN_AUDIT_NUMBER
            AND cod_estadouni IN (1, 4)
          )
      --AND cod_estadouni IN (1, 4)
    ;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      OUT_STATUS_CODE := 0;
      OUT_STATUS_MSG  := 'DAD: ' || 'No Existen Datos Nuevos en la Tabla wli_stage_uni_dtl para la seq: '||IN_AUDIT_NUMBER;
      --RETURN;
    WHEN OTHERS THEN
      OUT_STATUS_CODE := 1;
      OUT_STATUS_MSG  := 'DAD: ' ||  SQLERRM;
      RETURN;
  END;
  --DBMS_OUTPUT.PUT_LINE('OUT_KEYMAES_FINAL = ' || OUT_KEYMAES_FINAL);
  BEGIN
  -- Checksum, campo a cuadrar
  --SELECT SUM(CCOSTO), COUNT(sequencia_dad), 'CCOSTO'
  SELECT SUM(ccosto_cc1), COUNT(sequencia_dad), 'CCOSTO_CC1'
  INTO OUT_CUADRAT_SUMA_ZON , OUT_CUADRAT_NUMLIN_ZON,OUT_CUADRAT_CAMPO_ZON
  FROM WLI_STAGE_UNI_HDR RH
  WHERE RH.SEQUENCIA_DAD  > IN_AUDIT_NUMBER
    AND RH.SEQUENCIA_DAD <= OUT_KEYMAES_FINAL
    AND RH.COD_ESTADOUNI IN (1, 4) --Por Enviar, Error
  ;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      OUT_STATUS_CODE := 0;
      OUT_STATUS_MSG  := 'DAD: ' ||  'No Existen Datos Nuevos en la Tabla wli_stage_preruteo para la seq: '||IN_AUDIT_NUMBER;
    --  RETURN;
    WHEN OTHERS THEN
      OUT_STATUS_CODE := 1;
      OUT_STATUS_MSG  := 'DAD: ' ||  SQLERRM;
      RETURN;
   END;
  --DBMS_OUTPUT.PUT_LINE('OUT_KEYMAES_FINAL = ' || OUT_KEYMAES_FINAL);
  IF OUT_KEYMAES_FINAL IS NOT NULL THEN
    BEGIN
      INSERT INTO WLI_EVENT_ITFTRAMO_DAD(
                  SEQUENCIA_DAD,
                  TIPO_EVENTO,
                  FECHA_AUD_FINAL,
                  ESTADO_DAD,
                  FECHA_CREACION)
         VALUES ( OUT_KEYMAES_FINAL,
                  'ENVIAORDEN', -- Preguntar a Emerson por nombre del evento
                  SYSDATE,     --var_fecha_audit_final, -- Fecha fin de tramo (en tabla auditoria)
                  'PU',
                  SYSDATE)
      ;
    EXCEPTION
      WHEN DUP_VAL_ON_INDEX THEN
        OUT_STATUS_CODE := 1;
        OUT_STATUS_MSG  := 'DAD: ' ||  'El Correlativo ya fue procesado valide la tabla WLI_EVENT_ITFTRAMO_DAD';
        RETURN;
      WHEN COLLECTION_IS_NULL THEN
        OUT_STATUS_CODE := 1;
        OUT_STATUS_MSG  := 'DAD: ' ||  'No hay Datos a Procesar';
        RETURN;
      WHEN OTHERS THEN
        OUT_STATUS_CODE := 1;
        OUT_STATUS_MSG  :='DAD: ' ||   SQLERRM;
        RETURN;
    END;
  END IF;
  --DBMS_OUTPUT.PUT_LINE('OUT_STATUS_MSG = ' || OUT_STATUS_MSG);

  OPEN  rs_hdr FOR
  SELECT DISTINCT
        rh.fecha,
        rh.sequencia_dad,
        rh.ccosto,
        rh.canal_venta,
        rh.num_reserva,
        rh.orden,
        rh.num_ordendet  ,
        'D' AS tipo_idorden,--rh.tipo_idorden,
        rh.tipo_orden,
        rh.cod_estadocab,
        rh.fechaPactada,
        DECODE(rh.type_ide, 'DNI',          1,
                            'C.CIUDADANIA', 2,
                            'D.EXTRANJERO', 3,
                            'PASAPORTE'   , 4,
                            'RUT'         , 5)
                                          AS type_ide,        --rh.type_ide
        rh.ide                            AS ide,             --rh.ide
        rh.ide_dv                         AS ide_dv,          --rh.ide_dv
        rh.nombre_cli                     AS nombre_cli,      --rh.nombre_cli
        SUBSTR(rh.direccion_cli, 0, 50)   AS direccion_cli,   --SUBSTR(rh.direccion_cli, 0, 50)
        rh.comuna_cli                     AS comuna_cli,      --rh.comuna_cli
        rh.ciudad_cli                     AS ciudad_cli,      --rh.ciudad_cli
        rh.region_cli                     AS region_cli,      --rh.region_cli
        rh.latitud_cli,
        rh.longitud_cli,
        rh.calle_cli,
        rh.numero_cli,
        rh.entre_calle_cli,
        rh.barrio_cli,
        rh.fono_cli,
        rh.fono2_cli,
        rh.fono3_cli,
        rh.email_cli,
        rh.domicilio_ext_cli,
        rh.domicilio_desc_cli,
        rh.tiempo_espera_cli,
        rh.nombre_desp                    AS descripcion_desp,  --rh.descripcion_desp (A pedido de UNIGIS se copia el nombre del despacho)
        rh.nombre_desp,
        SUBSTR(rh.direccion_desp, 0, 50)  AS direccion_desp,
        rh.calle_desp,
        rh.numero_desp,
        rh.ref_calle_desp,
        rh.barrio_desp,
        rh.comuna_desp,
        rh.ciudad_desp,
        rh.region_desp,
        rh.INI_hora_plani,
        rh.fin_hora_plani,
        rh.ini_hora_desp1,
        rh.fin_hora_desp1,
        rh.ini_hora_desp2,
        rh.fin_hora_desp2,
        rh.ini_hora1,
        rh.fin_hora1,
        rh.ini_hora2,
        rh.fin_hora2,
        rh.tiempo_espera_desp,
        rh.prd_kilo,
        rh.prd_m3,
        rh.BULTOS,
        rh.pallets,
        rh.latitud_desp,
        rh.longitud_desp,
        rh.observacion,
        rh.email_desp,
        rh.fono_desp,
        rh.fono2_desp,
        rh.fono3_desp,
        rh.campo1,
        rh.campo2,
        rh.campo3,
        rh.campo4,
        rh.id_rutauni,
        DECODE(rh.org_is_store, 'F', rh.cc_despacha,   rh.ccosto_cc1)      AS ccosto_cc1,
        DECODE(rh.org_is_store, 'F', rh.org_name_full, rh.ccosto_desc_cc1) AS ccosto_desc_cc1,
        SUBSTR(rh.direccion_cc1, 0, 50)   AS direccion_cc1,
        rh.numero_cc1,
        rh.bassAddr1_cc1,
        rh.bassAddr2_cc1,
        rh.bassAddr3_cc1,
        rh.ini_hora_cc1,
        rh.fin_hora_cc1,
        rh.tiempoespera_cc1,
        rh.latitud_cc1,
        rh.longitud_cc1,
        rh.calle_cc1,
        rh.ref_calle_cc1,
        rh.barrio_cc1,
        rh.comuna_cc1,
        rh.ciudad_cc1,
        rh.region_cc1,
        rh.ccosto_cc2,
        rh.ccosto_desc_cc2,
        SUBSTR(rh.direccion_cc2, 0, 50)   AS direccion_cc2,
        rh.numero_cc2,
        rh.bassAddr1_cc2,
        rh.bassAddr2_cc2,
        rh.bassAddr3_cc2,
        rh.ini_hora_cc2,
        rh.fin_hora_cc2,
        rh.tiempoEspera_cc2,
        rh.latitud_cc2,
        rh.longitud_cc2,
        rh.calle_cc2,
        rh.ref_calle_cc2,
        rh.barrio_cc2,
        rh.comuna_cc2,
        rh.ciudad_cc2,
        rh.region_cc2
  FROM wli_stage_uni_hdr rh
  WHERE rh.sequencia_dad  > IN_AUDIT_NUMBER
    AND rh.sequencia_dad <= OUT_KEYMAES_FINAL
    AND rh.cod_estadouni IN (1, 4) --Por Enviar, Error
  ;
  OPEN  rs_dtl FOR
  SELECT DISTINCT
        rh.sequencia_dad,
        rd.num_detalle,
        TO_NUMBER(SUBSTR(TRIM(rd.prd_lvl_number),0,(LENGTH(TRIM(rd.prd_lvl_number))-1))) AS prd_lvl_number,
        rd.prd_name_full,
        rd.cantidad,
        rd.prd_m3_uni,
        rd.prd_m3_tot,
        rd.prd_kilo_uni,
        rd.prd_kilo_tot,
        rd.bultos_uni,
        rd.bultos_tot,
        rd.alto,
        rd.ancho,
        rd.profundidad,
        rd.apilable,
        rd.rotacion,
        rd.pallets,
        rd.num_gdd -- [CDD num gdd uni] Agregado el 03/10/2016
  FROM wli_stage_uni_hdr rh , wli_stage_uni_dtl rd
  WHERE rh.sequencia_dad  = rd.sequencia_dad
    AND rh.canal_venta    = rd.canal_venta
    AND rh.num_reserva    = rd.num_reserva
    AND rh.sequencia_dad  > IN_AUDIT_NUMBER
    AND rh.sequencia_dad <= OUT_KEYMAES_FINAL
    AND rh.cod_estadouni IN (1, 4) --Por Enviar, Error
  ;
  UPDATE preruteo_reservadet pr
  SET pr.cod_estadouni = 2 --Enviado
  WHERE pr.en_jaula      = 1
--    AND pr.cod_estadouni IN (1, 4) --Por Enviar, Error
    AND (pr.sequencia_dad, pr.canal_venta, pr.num_reserva, pr.num_detalle) IN (
        SELECT DISTINCT rh.sequencia_dad, rd.canal_venta, rd.num_reserva, rd.num_detalle
        FROM wli_stage_uni_dtl rd,
             wli_stage_uni_hdr rh
        WHERE rd.canal_venta    = rh.canal_venta
          AND rd.num_reserva    = rh.num_reserva
          AND rd.sequencia_dad  = rh.sequencia_dad
          AND rh.sequencia_dad  > IN_AUDIT_NUMBER
          AND rh.sequencia_dad <= OUT_KEYMAES_FINAL
          AND rh.cod_estadouni IN (1, 4) --Por Enviar, Error
        )
  ;
  UPDATE wli_stage_uni_hdr rh
  SET rh.cod_estadouni = 2 --Enviado
  WHERE rh.sequencia_dad  > IN_AUDIT_NUMBER
    AND rh.sequencia_dad <= OUT_KEYMAES_FINAL
    AND rh.cod_estadouni IN (1, 4) --Por Enviar, Error
  ;

  UPDATE wli_stage_uni_dtl rd
  SET rd.cod_estadouni = 2 --Enviado
  WHERE rd.sequencia_dad  > IN_AUDIT_NUMBER
    AND rd.sequencia_dad <= OUT_KEYMAES_FINAL
    AND rd.cod_estadouni IN (1, 4) --Por Enviar, Error
  ;

  IF in_process_luw = 'T' THEN
     COMMIT ;
  END IF;
END ENVIAR_UNIGIS;

PROCEDURE ENVIAR_RESERVA(
  IN_FECHA_PROCESO        IN  DATE,
  IN_AUDIT_NUMBER         IN  NUMBER DEFAULT 0,
  IN_CANT_REG             IN  NUMBER,
  IN_PROCESS_LUW          IN  VARCHAR2 DEFAULT 'T',
  "rs"                    OUT SYS_REFCURSOR,
  OUT_KEYMAES_INICIO      OUT VARCHAR2,
  OUT_KEYMAES_FINAL       OUT VARCHAR2,
  OUT_CUADRAT_NUMLIN_ZON  OUT NUMBER,
  OUT_CUADRAT_CAMPO_ZON   OUT VARCHAR2,
  OUT_CUADRAT_SUMA_ZON    OUT NUMBER,
  OUT_STATUS_CODE         OUT NUMBER,
  OUT_STATUS_MSG          OUT VARCHAR2) AS
BEGIN
DECLARE
    var_cont_reg              NUMBER;
    --var_estado_zona           TMPADMACZON.ESTADO_ZONA%TYPE;
BEGIN
    OUT_STATUS_CODE       := 0;
    OUT_STATUS_MSG        := 'OK';
    var_cont_reg          := 1;
    --var_estado_zona       := '99';
    IF IN_CANT_REG IS NULL THEN
       OUT_STATUS_CODE := 1;
       OUT_STATUS_MSG  := 'DAD: ' ||  'Cantidad de Registros no debe ser nulo';
       RETURN;
    ELSE
        IF IN_CANT_REG < 0 THEN
           OUT_STATUS_CODE := 1;
           OUT_STATUS_MSG  := 'DAD: ' ||  'Cantidad de Registros debe ser mayor a cero';
           RETURN;
        END IF;
    END IF;

    BEGIN
    select max(SEQUENCIA_DAD) into OUT_KEYMAES_FINAL from WLI_STAGE_RSV
    where SEQUENCIA_DAD in( select SEQUENCIA_DAD  from WLI_STAGE_RSV
    where (rownum <= IN_CANT_REG or IN_CANT_REG=0)
    and sequencia_dad > IN_AUDIT_NUMBER);

         EXCEPTION
           WHEN NO_DATA_FOUND THEN
                OUT_STATUS_CODE := 0;
                OUT_STATUS_MSG  := 'DAD: ' ||  'No Existen Datos Nuevos en la Tabla WLI_STAGE_RSV para la seq: '||IN_AUDIT_NUMBER;
          WHEN OTHERS THEN
                OUT_STATUS_CODE := 1;
                OUT_STATUS_MSG  := 'DAD: ' ||  SQLERRM;
                RETURN;
    END;

    BEGIN
     select sum(NUM_RESERVA) ,count(sequencia_dad),'num_reserva'
     into OUT_CUADRAT_SUMA_ZON , OUT_CUADRAT_NUMLIN_ZON,OUT_CUADRAT_CAMPO_ZON from WLI_STAGE_RSV
    where (sequencia_dad <= OUT_KEYMAES_FINAL)
    and sequencia_dad > IN_AUDIT_NUMBER group by 'num_reserva' ;
             EXCEPTION
           WHEN NO_DATA_FOUND THEN
                OUT_STATUS_CODE := 0;
                 OUT_STATUS_MSG  := 'DAD: ' ||  'No Existen Datos Nuevos en la Tabla WLI_STAGE_RSV para la seq: '||IN_AUDIT_NUMBER;
            WHEN OTHERS THEN
                OUT_STATUS_CODE := 1;
                OUT_STATUS_MSG  :='DAD: ' ||   SQLERRM;
                RETURN;
    END;

     IF OUT_KEYMAES_FINAL IS NOT NULL THEN
    BEGIN

--  INSERT INTO WLI_EVENT_ITFTRAMO_ADMAC
      INSERT INTO WLI_EVENT_ITFTRAMO_DAD
                (
                  SEQUENCIA_DAD,
                  TIPO_EVENTO,
                  FECHA_AUD_FINAL,
                  ESTADO_DAD,
                  FECHA_CREACION
                )
         VALUES (
                  OUT_KEYMAES_FINAL,
                  'RSVMODIFIC',
                  SYSDATE,--var_fecha_audit_final, -- Fecha fin de tramo (en tabla auditoria)
                  'PU',
                  SYSDATE
                );
      EXCEPTION
           WHEN DUP_VAL_ON_INDEX THEN
                OUT_STATUS_CODE := 1;
                OUT_STATUS_MSG  := SQLERRM;
                RETURN;
           WHEN OTHERS THEN
                OUT_STATUS_CODE := 1;
                OUT_STATUS_MSG  := SQLERRM;
                RETURN;
    END;
    END IF;

    OPEN  "rs" for SELECT /*DISTINCT*/
    "CANAL_VENTA" ,
    "NUM_RESERVA" ,
    "NUM_DETALLE"   ,
    "NUM_ORDENVENTA",
    "SUB_ORDEN"     ,
    "PRD_LVL_NUMBER" ,
    "PRD_NAME_FULL",
    "COD_ESTADOCAB"  ,
    "DESC_ESTADOCAB",
    "COD_ESTADODET" ,
    "DESC_ESTADODET" ,
    "FECHA_RESERVA" ,
    "FECHA_PAGO" ,
    "NOMBRE_DESP" ,
    "DIRECCION_DESP" ,
    "FONO_DESP" ,
    "FONO2_DESP"  ,
    "FECHA_DESPACHO" ,
    "FECHA_ENTREGA" ,
    "FECHA_ESTADO",
    "ASL_ESTADODET" AS "ESTADO_ASL"
    FROM WLI_STAGE_RSV
    WHERE (sequencia_dad <= OUT_KEYMAES_FINAL)
    and sequencia_dad > IN_AUDIT_NUMBER;

    IF in_process_luw = 'T' THEN
       COMMIT ;
    END IF;

  END;
END ENVIAR_RESERVA;

PROCEDURE ENVIAR_RESERVA_NEW
  (
  IN_KEY_ORIGEN           IN varchar2,
  IN_CANTIDAD_REG         IN NUMBER,            -- cantidad de registros a buscar
  IN_PROCESO_LUW          IN CHAR DEFAULT 'T',  -- T: commit; F: no se ejecuta commit
  OUT_CUADRAT_NUMLIN      OUT number,           -- cantidad de lineas
  OUT_CUADRAT_CAMPO       OUT varchar2,         -- campo a cuadrar
  OUT_CUADRAT_SUMA_ZON    OUT number,           -- suma de cantidad en relacion a al campo a cuadrar
  OUT_STATUS_CODE         out number,
  OUT_STATUS_MSG          out varchar2,
  "rs"  out sys_refcursor
  )
AS
BEGIN
DECLARE
    var_cont_reg              NUMBER;

BEGIN
   IF IN_CANTIDAD_REG IS NULL or IN_CANTIDAD_REG < 0 THEN
          OUT_STATUS_CODE := 1;
          OUT_STATUS_MSG  := 'DAD: ' ||  'Cantidad de Registros no debe ser nulo o debe ser mayor igual a cero';
           RETURN;
   END IF;

   OUT_STATUS_CODE    := 0;
   OUT_STATUS_MSG     := 'OK';
   OUT_CUADRAT_CAMPO  := 'NUM_DETALLE';
   var_cont_reg          := 1;

   BEGIN
      select sum(NUM_DETALLE), count(SEQUENCIA_DAD), 'NUM_DETALLE'
      into OUT_CUADRAT_SUMA_ZON , OUT_CUADRAT_NUMLIN, OUT_CUADRAT_CAMPO
      from WLI_STAGE_RSV
      where SEQUENCIA_DAD = IN_KEY_ORIGEN  ;
      EXCEPTION
      WHEN NO_DATA_FOUND THEN
           OUT_STATUS_CODE := 0;
           OUT_STATUS_MSG  := 'DAD: ' ||  'No Existen Datos Nuevos en la Tabla WLI_STAGE_RSV para la key: ' || IN_KEY_ORIGEN;
      WHEN OTHERS THEN
           OUT_STATUS_CODE := 1;
           OUT_STATUS_MSG  := 'DAD: ' ||  SQLERRM;
      RETURN;
   END;

   OPEN  "rs" FOR SELECT
    "CANAL_VENTA" ,
    "NUM_RESERVA" ,
    "NUM_DETALLE"   ,
    "NUM_ORDENVENTA",
    "SUB_ORDEN"     ,
    "PRD_LVL_NUMBER" ,
    "PRD_NAME_FULL",
    "COD_ESTADOCAB"  ,
    "DESC_ESTADOCAB",
    "COD_ESTADODET" ,
    "DESC_ESTADODET" ,
    "FECHA_RESERVA" ,
    "FECHA_PAGO" ,
    "NOMBRE_DESP" ,
    "DIRECCION_DESP" ,
    "FONO_DESP" ,
    "FONO2_DESP"  ,
    "FECHA_DESPACHO" ,
    "FECHA_ENTREGA" ,
    "FECHA_ESTADO" ,
    "FECHA_ENTREGA_CLI",
    "NUM_OC"
    FROM WLI_STAGE_RSV
    WHERE SEQUENCIA_DAD=IN_KEY_ORIGEN;

    IF OUT_CUADRAT_NUMLIN = 0 THEN
         OUT_STATUS_CODE := 1;
         OUT_STATUS_MSG  :='DAD: ' ||  'No Existen Datos Para Enviar';
    ELSE
         if IN_PROCESO_LUW = 'T' then
             COMMIT ;
             OUT_STATUS_MSG  := 'OK';
          end if;
    END IF;
END;
END ENVIAR_RESERVA_NEW;

PROCEDURE ENVIAR_CAMBIOESTADO_RESERVA(
  IN_FECHA_PROCESO        IN  DATE,
  IN_AUDIT_NUMBER         IN  NUMBER DEFAULT 0,
  IN_CANT_REG             IN  NUMBER,
  IN_PROCESS_LUW          IN  VARCHAR2 DEFAULT 'T',
  "rs"                    out sys_refcursor,
  OUT_KEYMAES_INICIO      OUT VARCHAR2,
  OUT_KEYMAES_FINAL       OUT VARCHAR2,
  OUT_CUADRAT_NUMLIN_ZON  OUT NUMBER,
  OUT_CUADRAT_CAMPO_ZON   OUT VARCHAR2,
  OUT_CUADRAT_SUMA_ZON    OUT NUMBER,
  OUT_STATUS_CODE         OUT NUMBER,
  OUT_STATUS_MSG          OUT VARCHAR2) AS
BEGIN
  DECLARE
    var_cont_reg              NUMBER;
    --var_estado_zona           TMPADMACZON.ESTADO_ZONA%TYPE;
BEGIN
  OUT_STATUS_CODE       := 0;
  OUT_STATUS_MSG        := 'OK';
  var_cont_reg          := 1;
  --var_estado_zona       := '99';

  IF IN_CANT_REG IS NULL THEN
    OUT_STATUS_CODE := 1;
    OUT_STATUS_MSG  := 'DAD: ' ||  'Cantidad de Registros no debe ser nulo';
    RETURN;
  ELSE
    IF IN_CANT_REG < 0 THEN
      OUT_STATUS_CODE := 1;
      OUT_STATUS_MSG  := 'DAD: ' ||  'Cantidad de Registros debe ser mayor a cero';
      RETURN;
    END IF;
  END IF;

  BEGIN
    select max(SEQUENCIA_DAD) into OUT_KEYMAES_FINAL from WLI_STAGE_RSV_ESTADO
    where SEQUENCIA_DAD in( select SEQUENCIA_DAD  from WLI_STAGE_RSV_ESTADO
    where (rownum <= IN_CANT_REG or IN_CANT_REG=0)
    and sequencia_dad > IN_AUDIT_NUMBER);
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      OUT_STATUS_CODE := 0;
      OUT_STATUS_MSG  := 'DAD: ' ||  'No Existen Datos Nuevos en la Tabla WLI_STAGE_RSV_ESTADO para la seq: '||IN_AUDIT_NUMBER;
    WHEN OTHERS THEN
      OUT_STATUS_CODE := 1;
      OUT_STATUS_MSG  := 'DAD: ' || SQLERRM;
      RETURN;
  END;

  BEGIN
    SELECT sum(NUM_RESERVA), count(1), 'num_reserva'
     INTO OUT_CUADRAT_SUMA_ZON, OUT_CUADRAT_NUMLIN_ZON, OUT_CUADRAT_CAMPO_ZON
     FROM WLI_STAGE_RSV_ESTADO
    WHERE sequencia_dad <= OUT_KEYMAES_FINAL
      and sequencia_dad > IN_AUDIT_NUMBER;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      OUT_STATUS_CODE := 0;
      OUT_STATUS_MSG  := 'DAD: ' ||  'No Existen Datos Nuevos en la Tabla WLI_STAGE_RSV_ESTADO para la seq: '||IN_AUDIT_NUMBER;
    WHEN OTHERS THEN
      OUT_STATUS_CODE := 1;
      OUT_STATUS_MSG  :='DAD: ' || SQLERRM;
      RETURN;
  END;

  IF OUT_KEYMAES_FINAL IS NOT NULL THEN
    BEGIN
      --INSERT INTO WLI_EVENT_ITFTRAMO_ADMAC
      INSERT INTO WLI_EVENT_ITFTRAMO_DAD(SEQUENCIA_DAD,
                                         TIPO_EVENTO,
                                         FECHA_AUD_FINAL,
                                         ESTADO_DAD,
                                         FECHA_CREACION)
      VALUES (OUT_KEYMAES_FINAL,
              'RSVCESTADO',
              SYSDATE,--var_fecha_audit_final, -- Fecha fin de tramo (en tabla auditoria)
              'PU',
              SYSDATE);
    EXCEPTION
      WHEN DUP_VAL_ON_INDEX THEN
        OUT_STATUS_CODE := 1;
        OUT_STATUS_MSG  := SQLERRM;
        RETURN;
      WHEN OTHERS THEN
        OUT_STATUS_CODE := 1;
        OUT_STATUS_MSG  := SQLERRM;
        RETURN;
    END;
  END IF;

  OPEN  "rs" for
  SELECT canal_venta,
         num_reserva,
         num_ordenventa,
         sub_orden,
         cod_estadocab,
         fecha_estado,
         asl_estadocab
    FROM WLI_STAGE_RSV_ESTADO
    WHERE sequencia_dad <= OUT_KEYMAES_FINAL
      AND sequencia_dad > IN_AUDIT_NUMBER
  ORDER BY "CANAL_VENTA", "NUM_RESERVA", "NUM_ORDENVENTA", sequencia_dad, "COD_ESTADOCAB", "SUB_ORDEN" ASC;

  IF in_process_luw = 'T' THEN
    COMMIT ;
  END IF;

END;
END ENVIAR_CAMBIOESTADO_RESERVA;

PROCEDURE ENVIAR_HOJA_RUTA(        -- Cursor de Distros cancelados
  in_key_origen          in wli_event_itfkey_dad.key_origen%TYPE,
   in_cantidad_reg        in  number,                 -- cantidad de registros a buscar
   in_proceso_luw         in  char default 'T',       -- T: commit; F: no se ejecuta commit
   out_cuadrat_numlin     out number,                 -- cantidad de lineas OUT_CUADRAT_NUMLIN_ZON
   out_cuadrat_campo      out varchar2,               -- campo a cuadrar OUT_CUADRAT_CAMPO_ZON
   out_cuadrat_suma_zon       out number,                 -- suma de cantidad en relacion a al campo a cuadrar OUT_CUADRAT_SUMA_ZON
   OUT_STATUS_CODE         out number,              --OUT_STATUS_CODE
   OUT_STATUS_MSG          out varchar2,            --OUT_STATUS_MSG
  "rs"  out sys_refcursor
  )
  AS

BEGIN
DECLARE
    var_cont_reg              NUMBER;
    --var_estado_zona           TMPADMACZON.ESTADO_ZONA%TYPE;

BEGIN


       IF in_cantidad_reg IS NULL or in_cantidad_reg < 0 THEN
          OUT_STATUS_CODE := 1;
          OUT_STATUS_MSG  := 'DAD: ' ||  'Cantidad de Registros no debe ser nulo o debe ser mayor igual a cero';
           RETURN;
       END IF;

     OUT_STATUS_CODE    := 0;
     OUT_STATUS_MSG     := 'OK';
     out_cuadrat_campo := 'cantidad';




    BEGIN
     select sum(cantidad) ,count(sequencia_dad),'cantidad'
     into out_cuadrat_suma_zon , out_cuadrat_numlin, out_cuadrat_campo from WLI_STAGE_HOJARUTA
    where sequencia_dad = in_key_origen  ;
             EXCEPTION
           WHEN NO_DATA_FOUND THEN
                OUT_STATUS_CODE := 0;
                 OUT_STATUS_MSG  := 'DAD: ' ||  'No Existen Datos Nuevos en la Tabla WLI_STAGE_HOJARUTA para la key: ' || in_key_origen;
           WHEN OTHERS THEN
                OUT_STATUS_CODE := 1;
                OUT_STATUS_MSG  := 'DAD: ' ||  SQLERRM;
                RETURN;
    END;


       OPEN  "rs" for SELECT
        COD_HOJARUTA,
        COD_TRANSP,
        RUT_CHOFER,
        PATENTE,
        FECHA,
        CANTIDAD,
        VOLUMEN,
        PESO,
        USUARIO_CREA,
        OBSERVACIONES,
        EMPRESA
      FROM WLI_STAGE_HOJARUTA
      WHERE SEQUENCIA_DAD=in_key_origen;





      IF out_cuadrat_numlin = 0 THEN
         OUT_STATUS_CODE := 1;
         OUT_STATUS_MSG  :='DAD: ' ||  'No Existen Datos Para Enviar';
      ELSE
          if in_proceso_luw = 'T' then
             COMMIT ;
             OUT_STATUS_MSG  := 'OK';
          end if;
      END IF;

END;
END ENVIAR_HOJA_RUTA;

PROCEDURE GET_RESERVA(
    rutNum          IN NUMBER,
    rutVer          IN VARCHAR2,
    reserva         IN NUMBER,
    rsCab           OUT SYS_REFCURSOR,
    rsDet           OUT SYS_REFCURSOR,
    rsReq           OUT SYS_REFCURSOR,
    OUT_STATUS_CODE OUT NUMBER,
    OUT_STATUS_MSG  OUT VARCHAR)
AS
BEGIN
   IF rutVer IS NULL THEN
        --Cabecera
          OPEN rsCab for
          SELECT r.canal_venta,r.num_reserva,r.cod_estadocab,e.desc_estadocab,r.org_lvl_number,r.num_ordenventa,r.fecha_reserva,r.fecha_pago,r.id_vendedor,r.type_doc,r.num_doc,r.monto,r.doc_date,r.num_caja,r.type_ide,r.ide,r.ide_dv,r.region_cli,r.ciudad_cli,r.comuna_cli,r.comuna_clitipo,r.nombre_cli,r.direccion_cli,r.fono_cli,r.region_desp,r.ciudad_desp,r.comuna_desp,r.comuna_desptipo,r.direccion_desp,r.nombre_desp,r.fono_desp,r.fono2_desp,r.observacion,r.marca_guia,r.campo1,r.campo2,r.campo3,r.campo4,r.campo5,r.campo6,r.campo7,r.campo8,r.campo9,r.campo10,r.numrsv_padre
          FROM RESERVA_HDR r, ESTADOSCAB e
          WHERE r.cod_estadocab = e.cod_estadocab and r.NUM_RESERVA = reserva
          ORDER BY r.num_reserva desc;
       --Detalle
          OPEN rsDet for
          SELECT r.canal_venta,r.num_reserva,r.num_detalle,r.num_ordenventa,r.sub_orden,r.prd_lvl_number,r.prd_name_full,r.cod_estadodet,e.desc_estadodet,r.cc_origen,r.cc_despacha,r.cc_via,r.prd_kilo,r.prd_m3,r.tipo_stock,r.prioridad,r.vendor_number,r.fecha_recep_oc,r.fecha_ini_proceso,r.fecha_despacho,r.fecha_entrega,r.fecha_entrega_ori,r.cantidad,r.precio,r.tipo_ped_odbms,r.num_ped_odbms,r.desc_estado_odbms,r.tipo_ped_despacho,r.num_ped_despacho,r.desc_estado_despacho,r.num_pkt,r.cod_estadopkt,r.cc_envia,r.cc_recibe,r.num_gd,r.lpn_case_nbr,r.cod_motivodev,m.desc_motivodev,r.complementos,r.fecha_entrega_cli,r.bloqueado,r.envoltura,r.tarjeta_msj,r.campo1,r.campo2,r.campo3,r.campo4,r.campo5,r.campo6,r.campo7,r.campo8,r.campo9,r.campo10,r.fecha_despacho_ori, r.courier_os, r.courier_estado, r.courier_obs, r.num_reserva_b2b, r.fecha_despacho2, r.fecha_entrega2, r.datos_hua, r.cantidad_entrega_cli, r.num_gd_prov, r.num_seriegd_prov, r.num_factura_prov, r.ide_recive, r.ide_dv_recive, r.nombre_recive, r.ide_chofer, r.ide_dv_chofer, r.nombre_chofer, r.tipo_despacho, r.express, r.prima_pendiente, r.cant_desp, r.cant_ret, r.cant_trf, r.fecha_recep, r.num_ncr, r.cant_ncr
          FROM reserva_dtl r, ESTADOSDET e, MOTIVODEV m
          WHERE r.cod_estadodet = e.cod_estadodet and r.cod_motivodev = m.cod_motivodev (+) and r.NUM_RESERVA = reserva
          ORDER BY r.num_reserva desc;
       --Requerimiento
          OPEN rsReq for
          SELECT r.id_req,r.canal_venta,r.num_reserva,r.cod_tiporeq,t.desc_tiporeq,r.cc_req,r.fecha_req,r.observaciones,r.login_usuario
          FROM REQUERIMIENTO r, TIPO_REQUERIMIENTO t
          WHERE r.NUM_RESERVA = reserva and r.cod_tiporeq = t.cod_tiporeq
          ORDER BY r.num_reserva desc;
   ELSE
       IF (reserva = 0) THEN
       --Cabecera
          OPEN rsCab for
          select * from (SELECT r.canal_venta,r.num_reserva,r.cod_estadocab,e.desc_estadocab,r.org_lvl_number,r.num_ordenventa,r.fecha_reserva,r.fecha_pago,r.id_vendedor,r.type_doc,r.num_doc,r.monto,r.doc_date,r.num_caja,r.type_ide,r.ide,r.ide_dv,r.region_cli,r.ciudad_cli,r.comuna_cli,r.comuna_clitipo,r.nombre_cli,r.direccion_cli,r.fono_cli,r.region_desp,r.ciudad_desp,r.comuna_desp,r.comuna_desptipo,r.direccion_desp,r.nombre_desp,r.fono_desp,r.fono2_desp,r.observacion,r.marca_guia,r.campo1,r.campo2,r.campo3,r.campo4,r.campo5,r.campo6,r.campo7,r.campo8,r.campo9,r.campo10, r.numrsv_padre
          FROM RESERVA_HDR r, ESTADOSCAB e
          WHERE r.ide = rutnum and r.ide_dv = rutver and r.cod_estadocab = e.cod_estadocab
          ORDER BY r.num_reserva desc) where rownum <= (SELECT valor_num FROM parametros WHERE cod_param = 15);
       --Detalle
          OPEN rsDet for
          SELECT r.canal_venta,r.num_reserva,r.num_detalle,r.num_ordenventa,r.sub_orden,r.prd_lvl_number,r.prd_name_full,r.cod_estadodet,e.desc_estadodet,r.cc_origen,r.cc_despacha,r.cc_via,r.prd_kilo,r.prd_m3,r.tipo_stock,r.prioridad,r.vendor_number,r.fecha_recep_oc,r.fecha_ini_proceso,r.fecha_despacho,r.fecha_entrega,r.fecha_entrega_ori,r.cantidad,r.precio,r.tipo_ped_odbms,r.num_ped_odbms,r.desc_estado_odbms,r.tipo_ped_despacho,r.num_ped_despacho,r.desc_estado_despacho,r.num_pkt,r.cod_estadopkt,r.cc_envia,r.cc_recibe,r.num_gd,r.lpn_case_nbr,r.cod_motivodev,m.desc_motivodev,r.complementos,r.fecha_entrega_cli,r.bloqueado,r.envoltura,r.tarjeta_msj,r.campo1,r.campo2,r.campo3,r.campo4,r.campo5,r.campo6,r.campo7,r.campo8,r.campo9,r.campo10,r.fecha_despacho_ori, r.courier_os, r.courier_estado, r.courier_obs, r.num_reserva_b2b, r.fecha_despacho2, r.fecha_entrega2, r.datos_hua, r.cantidad_entrega_cli, r.num_gd_prov, r.num_seriegd_prov, r.num_factura_prov, r.ide_recive, r.ide_dv_recive, r.nombre_recive, r.ide_chofer, r.ide_dv_chofer, r.nombre_chofer, r.tipo_despacho, r.express, r.prima_pendiente, r.cant_desp, r.cant_ret, r.cant_trf, r.fecha_recep, r.num_ncr, r.cant_ncr
          FROM reserva_dtl r, ESTADOSDET e, MOTIVODEV m
          WHERE r.NUM_RESERVA in (select NUM_RESERVA from (select NUM_RESERVA from reserva_hdr where ide = rutnum and ide_dv = rutver order by NUM_RESERVA desc) where rownum <= (SELECT valor_num FROM parametros WHERE cod_param = 15)) and r.cod_estadodet = e.cod_estadodet and r.cod_motivodev = m.cod_motivodev  (+)
          ORDER BY r.num_reserva desc;
       --Requerimiento
          OPEN rsReq for
          SELECT r.id_req,r.canal_venta,r.num_reserva,r.cod_tiporeq,t.desc_tiporeq,r.cc_req,r.fecha_req,r.observaciones,r.login_usuario
          FROM REQUERIMIENTO r, TIPO_REQUERIMIENTO t
          WHERE r.NUM_RESERVA in (select NUM_RESERVA from (select NUM_RESERVA from reserva_hdr where ide = rutnum and ide_dv = rutver order by NUM_RESERVA desc) where rownum <= (SELECT valor_num FROM parametros WHERE cod_param = 15)) and r.cod_tiporeq = t.cod_tiporeq
          ORDER BY r.num_reserva desc;
       ELSE
          --Cabecera
          OPEN rsCab for
          SELECT r.canal_venta,r.num_reserva,r.cod_estadocab,e.desc_estadocab,r.org_lvl_number,r.num_ordenventa,r.fecha_reserva,r.fecha_pago,r.id_vendedor,r.type_doc,r.num_doc,r.monto,r.doc_date,r.num_caja,r.type_ide,r.ide,r.ide_dv,r.region_cli,r.ciudad_cli,r.comuna_cli,r.comuna_clitipo,r.nombre_cli,r.direccion_cli,r.fono_cli,r.region_desp,r.ciudad_desp,r.comuna_desp,r.comuna_desptipo,r.direccion_desp,r.nombre_desp,r.fono_desp,r.fono2_desp,r.observacion,r.marca_guia,r.campo1,r.campo2,r.campo3,r.campo4,r.campo5,r.campo6,r.campo7,r.campo8,r.campo9,r.campo10, r.numrsv_padre
          FROM RESERVA_HDR r, ESTADOSCAB e
          WHERE r.NUM_RESERVA = reserva  and r.ide = rutnum and r.ide_dv = rutver and r.cod_estadocab = e.cod_estadocab ORDER BY r.num_reserva desc;
       --Detalle
          OPEN rsDet for
          SELECT r.canal_venta,r.num_reserva,r.num_detalle,r.num_ordenventa,r.sub_orden,r.prd_lvl_number,r.prd_name_full,r.cod_estadodet,e.desc_estadodet,r.cc_origen,r.cc_despacha,r.cc_via,r.prd_kilo,r.prd_m3,r.tipo_stock,r.prioridad,r.vendor_number,r.fecha_recep_oc,r.fecha_ini_proceso,r.fecha_despacho,r.fecha_entrega,r.fecha_entrega_ori,r.cantidad,r.precio,r.tipo_ped_odbms,r.num_ped_odbms,r.desc_estado_odbms,r.tipo_ped_despacho,r.num_ped_despacho,r.desc_estado_despacho,r.num_pkt,r.cod_estadopkt,r.cc_envia,r.cc_recibe,r.num_gd,r.lpn_case_nbr,r.cod_motivodev,m.desc_motivodev,r.complementos,r.fecha_entrega_cli,r.bloqueado,r.envoltura,r.tarjeta_msj,r.campo1,r.campo2,r.campo3,r.campo4,r.campo5,r.campo6,r.campo7,r.campo8,r.campo9,r.campo10,r.fecha_despacho_ori, r.courier_os, r.courier_estado, r.courier_obs, r.num_reserva_b2b, r.fecha_despacho2, r.fecha_entrega2, r.datos_hua, r.cantidad_entrega_cli, r.num_gd_prov, r.num_seriegd_prov, r.num_factura_prov, r.ide_recive, r.ide_dv_recive, r.nombre_recive, r.ide_chofer, r.ide_dv_chofer, r.nombre_chofer, r.tipo_despacho, r.express, r.prima_pendiente, r.cant_desp, r.cant_ret, r.cant_trf, r.fecha_recep, r.num_ncr, r.cant_ncr
          FROM reserva_dtl r, ESTADOSDET e, MOTIVODEV m
          WHERE r.NUM_RESERVA in (select NUM_RESERVA from reserva_hdr where ide = rutnum and ide_dv = rutver and NUM_RESERVA = reserva) and r.cod_estadodet = e.cod_estadodet and r.cod_motivodev = m.cod_motivodev ORDER BY r.num_reserva desc;
       --Requerimiento
          OPEN rsReq for
          SELECT r.id_req,r.canal_venta,r.num_reserva,r.cod_tiporeq,t.desc_tiporeq,r.cc_req,r.fecha_req,r.observaciones,r.login_usuario
          FROM REQUERIMIENTO r, TIPO_REQUERIMIENTO t
          WHERE r.NUM_RESERVA in (select NUM_RESERVA from reserva_hdr where ide = rutnum and ide_dv = rutver and NUM_RESERVA = reserva) and r.cod_tiporeq = t.cod_tiporeq ORDER BY r.num_reserva desc;
       END IF;
   END IF;
END GET_RESERVA;

PROCEDURE GET_RESERVA2(
  rutNum            IN  NUMBER,
  rutVer            IN  VARCHAR2,
  reserva           IN  NUMBER,
  rsCab             OUT SYS_REFCURSOR,
  rsCabHist         OUT SYS_REFCURSOR,
  rsDet             OUT SYS_REFCURSOR,
  rsDetHist         OUT SYS_REFCURSOR,
  rsReq             OUT SYS_REFCURSOR,
  OUT_STATUS_CODE   OUT NUMBER,
  OUT_STATUS_MSG    OUT VARCHAR) AS
BEGIN
IF rutVer IS NULL THEN --Sin RUT, busca por reserva
 --Cabecera
 OPEN rsCab FOR
      SELECT r.canal_venta,
             r.num_reserva,
             r.cod_estadocab,
             e.desc_estadocab,
             r.org_lvl_number,
             r.num_ordenventa,
             r.fecha_reserva,
             r.fecha_pago,
             r.id_vendedor,
             r.type_doc,
             r.num_doc,
             r.monto,
             r.doc_date,
             r.num_caja,
             r.type_ide,
             r.ide,
             r.ide_dv,
             r.region_cli,
             r.ciudad_cli,
             r.comuna_cli,
             r.comuna_clitipo,
             r.nombre_cli,
             r.direccion_cli,
             r.fono_cli,
             r.region_desp,
             r.ciudad_desp,
             r.comuna_desp,
             r.comuna_desptipo,
             r.direccion_desp,
             r.nombre_desp,
             r.fono_desp,
             r.fono2_desp,
             r.observacion,
             r.marca_guia,
             r.campo1,
             r.campo2,
             r.campo3,
             r.campo4,
             r.campo5,
             r.campo6,
             r.campo7,
             r.campo8,
             r.campo9,
             r.campo10,
             NVL (r.numrsv_padre, 0) AS numrsv_padre,
             r.monto_desp,
             r.tipo_rt
        FROM RESERVA_HDR r, ESTADOSCAB e
        WHERE r.cod_estadocab = e.cod_estadocab
          AND r.NUM_RESERVA   = reserva
    ORDER BY r.fecha_reserva DESC, r.num_reserva DESC;

 --Historial de cambios de la cabecera
 OPEN rsCabHist FOR
          SELECT  "cod_tipo_evento" as cod_tipo_evento,
                  "canal_venta" AS canal_venta,
                  "numReserva" AS num_reserva,
                  "fecha_evento" AS fecha_evento,
                  "cod_estado" AS cod_estado,
                  "desc_estado" AS desc_estado,
                  "nombre_desp" AS  nombre_desp,
                  "direccion_desp"  AS direccion_desp,
                  "comuna_desp" AS comuna_desp,
                  "ciudad_desp" AS ciudad_desp,
                  "region_desp" AS region_desp,
                  "fono_desp" AS fono_desp,
                  "fono2_desp" AS fono2_desp,
                  "observacion" AS observacion,
                  "dato" as dato,
                  "datoFormateado" as dato_formateado,
                  "usuario" AS login_usuario
          FROM EVENTOSVIEW2
          WHERE "numReserva" = reserva
          ORDER BY "numReserva", "fecha_evento" DESC;

 --Detalle
 OPEN rsDet FOR
      SELECT r.canal_venta,
             r.num_reserva,
             r.num_detalle,
             r.num_ordenventa,
             r.sub_orden,
             r.prd_lvl_number,
             r.prd_name_full,
             NVL (r.cod_estadodet, 0) AS cod_estadodet,
             e.desc_estadodet,
             r.cc_origen,
             r.cc_despacha,
             NVL (r.cc_via, 0) AS cc_via,
             r.prd_kilo,
             r.prd_m3,
             r.tipo_stock,
             r.prioridad,
             r.vendor_number,
             r.fecha_recep_oc,
             r.fecha_ini_proceso,
             r.fecha_despacho,
             r.fecha_entrega,
             r.fecha_entrega_ori,
             r.cantidad,
             r.precio,
             r.tipo_ped_odbms,
             NVL (r.num_ped_odbms, 0) AS num_ped_odbms,
             r.desc_estado_odbms,
             r.tipo_ped_despacho,
             NVL (r.num_ped_despacho, 0) AS num_ped_despacho,
             r.desc_estado_despacho,
             r.num_pkt,
             NVL (r.cod_estadopkt, 0) AS cod_estadopkt,
             NVL (r.cc_envia, 0) AS cc_envia,
             NVL (r.cc_recibe, 0) AS cc_recibe,
             NVL (r.num_gd, 0) AS num_gd,
             r.lpn_case_nbr,
             NVL (r.cod_motivodev, 0) AS cod_motivodev,
             m.desc_motivodev,
             NVL (r.complementos, 0) AS complementos,
             r.fecha_entrega_cli,
             r.bloqueado,
             r.envoltura,
             r.tarjeta_msj,
             r.campo1,
             r.campo2,
             r.campo3,
             r.campo4,
             r.campo5,
             r.campo6,
             r.campo7,
             r.campo8,
             r.campo9,
             r.campo10,
             r.fecha_despacho_ori,
             NVL (r.courier_os, 0) AS courier_os,
             r.courier_estado,
             r.courier_obs,
             NVL (r.num_reserva_b2b, 0) AS num_reserva_b2b,
             r.fecha_despacho2,
             r.fecha_entrega2,
             r.datos_hua,
             NVL (r.cantidad_entrega_cli, 0) AS cantidad_entrega_cli,
             NVL (r.num_gd_prov, 0) AS num_gd_prov,
             NVL (r.num_seriegd_prov, 0) AS num_seriegd_prov,
             NVL (r.num_factura_prov, 0) AS num_factura_prov,
             NVL (r.ide_recive, 0) AS ide_recive,
             NVL (r.ide_dv_recive, 0) AS ide_dv_recive -- no es integer
                                                      ,
             r.nombre_recive,
             NVL (r.ide_chofer, 0) AS ide_chofer,
             NVL (r.ide_dv_chofer, 0) AS ide_dv_chofer,
             r.nombre_chofer,
             r.tipo_despacho,
             r.express,
             NVL (r.prima_pendiente, 0) AS prima_pendiente,
             NVL (r.cant_desp, 0) AS cant_desp,
             NVL (r.cant_ret, 0) AS cant_ret,
             NVL (r.cant_trf, 0) AS cant_trf,
             r.fecha_recep,
             NVL (r.num_ncr, 0) AS num_ncr,
             NVL (r.cant_ncr, 0) AS cant_ncr,
             r.GLOSA_RANGO AS glosa_rango
        FROM reserva_dtl r, ESTADOSDET e, MOTIVODEV m
       WHERE     r.cod_estadodet = e.cod_estadodet
             AND r.cod_motivodev = m.cod_motivodev(+)
             AND r.NUM_RESERVA = reserva
    ORDER BY r.num_reserva DESC;

 --Historial del Detalle
 OPEN rsDetHist FOR
      SELECT EVENTO.COD_TIPOEVENTO AS cod_tipo_evento,
             EVENTO.CANAL_VENTA AS canal_venta,
             EVENTO.NUM_RESERVA AS num_reserva,
             EVENTO.NUM_DETALLE AS num_detalle,
             RESERVA_DTL.PRD_LVL_NUMBER AS sku,
             RESERVA_DTL.PRD_NAME_FULL AS sku_desc,
             EVENTO.FECHA_EVENTO AS fecha_evento,
             NVL (EVENTO.COD_ESTADO, 0) AS cod_estado,
             ESTADOSDET.DESC_ESTADODET AS desc_estadodet,
             EVENTO.FECHA_DESP_FIN AS fecha_despacho,
             EVENTO.FECHA_ENTREGA_FIN AS fecha_entrega,
             EVENTO.COURIER_ESTADO AS curier_estado,
             EVENTO.COURIER_OBS AS curier_obs,
             NVL (EVENTO.COD_MOTIVODEV, 0) AS cod_motivo_dev,
             MOTIVODEV.DESC_MOTIVODEV AS desc_motivo_dev,
             TIPO_EVENTO.DESC_EVENTO AS desc_evento,
             HOJARUTA.PATENTE AS patente,
             NVL(HOJARUTA.COD_HOJARUTA,0) AS cod_hojaruta,
             CHOFER.NOM_CHOFER AS nom_chofer,
             TRANSPORTISTA.EMPRESA AS empresa,
             EVENTO.LOGIN_USUARIO AS login_usuario,
             (CASE
                WHEN EVENTO.fecha_entrega_ini != EVENTO.fecha_entrega_fin THEN
                 ' Fecha de Entrega, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.fecha_desp_ini != EVENTO.fecha_desp_fin THEN
                 ' Fecha de Despacho, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.nombre_desp_ini != EVENTO.nombre_desp_fin THEN
                 'Nombre, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.fono_desp_ini != EVENTO.fono_desp_fin THEN
                 'Telefono, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.fono2_desp_ini != EVENTO.fono2_desp_fin THEN
                 'Telefono 2, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.direccion_desp_ini != EVENTO.direccion_desp_fin THEN
                 'Direccion, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.region_desp_ini != EVENTO.region_desp_fin THEN
                 'Region, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.ciudad_desp_ini != EVENTO.ciudad_desp_fin THEN
                 'Ciudad, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.cc_despacha_ini != EVENTO.cc_despacha_fin THEN
                 ' CC Despacha, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.comuna_desp_ini != EVENTO.comuna_desp_fin THEN
                 'Comuna, '
                ELSE
                 ''
             END) AS dato,
             (CASE
                WHEN EVENTO.fecha_desp_ini != EVENTO.fecha_desp_fin THEN
                 ' <P><B>' || ' Fecha: </B>de ' || EVENTO.fecha_desp_ini || ' a ' ||
                 EVENTO.fecha_desp_fin || '</P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.fecha_entrega_ini != EVENTO.fecha_entrega_fin THEN
                 ' <P><B>' || ' Fecha: </B>de ' || EVENTO.fecha_entrega_ini ||
                 ' a ' || EVENTO.fecha_entrega_fin || '</P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.nombre_desp_ini != EVENTO.nombre_desp_fin THEN
                 '<P><B> Nombre:</B> de ' || EVENTO.nombre_desp_ini || ' a ' ||
                 EVENTO.nombre_desp_fin || '</P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.fono_desp_ini != EVENTO.fono_desp_fin THEN
                 '<P><B> Telefono: </B>de ' || EVENTO.fono_desp_ini || ' a ' ||
                 EVENTO.fono_desp_fin || '</P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.fono2_desp_ini != EVENTO.fono2_desp_fin THEN
                 '<P><B> Telefono 2: </B>de ' || EVENTO.fono2_desp_ini || ' a ' ||
                 EVENTO.fono2_desp_fin || '</P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.direccion_desp_ini != EVENTO.direccion_desp_fin THEN
                 '<P><B> Direccion:</B> de ' || EVENTO.direccion_desp_ini ||
                 ' a ' || EVENTO.direccion_desp_fin || '</P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.region_desp_ini != EVENTO.region_desp_fin THEN
                 '<P><B> Ciudad: </B>de ' ||
                 (SELECT "descripcion"
                    FROM "REGIONVIEW"
                   WHERE "codigo" = EVENTO.region_desp_ini) || ' a ' ||
                 (SELECT "descripcion"
                    FROM "REGIONVIEW"
                   WHERE "codigo" = EVENTO.region_desp_fin) || '</P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.ciudad_desp_ini != EVENTO.ciudad_desp_fin THEN
                 '<P><B> Ciudad: </B>de ' ||
                 (SELECT "descripcion"
                    FROM "CIUDADVIEW"
                   WHERE "codigo" = EVENTO.ciudad_desp_ini) || ' a ' ||
                 (SELECT "descripcion"
                    FROM "CIUDADVIEW"
                   WHERE "codigo" = EVENTO.ciudad_desp_fin) || '</P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.comuna_desp_ini != EVENTO.comuna_desp_fin THEN
                 '<P><B> Comuna: </B>de ' ||
                 (SELECT "descripcion"
                    FROM "COMUNAVIEW"
                   WHERE "codigo" = EVENTO.comuna_desp_ini) || ' a ' ||
                 (SELECT "descripcion"
                    FROM "COMUNAVIEW"
                   WHERE "codigo" = EVENTO.comuna_desp_fin) || '<P></P></P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.cc_despacha_ini != EVENTO.cc_despacha_fin THEN
                 '<P><B> CC Despacha: </B>de ' || EVENTO.cc_despacha_ini || ' a ' ||
                 EVENTO.cc_despacha_fin || '</P>'
                ELSE
                 ''
             END) AS dato_formateado

        FROM EVENTO,
             MOTIVODEV,
             RESERVA_DTL,
             ESTADOSDET,
             TIPO_EVENTO,
             HOJARUTA,
             TRANSPORTISTA,
             CHOFER
       WHERE     TIPO_EVENTO.COD_TIPOEVENTO = EVENTO.COD_TIPOEVENTO
             AND EVENTO.COD_MOTIVODEV = MOTIVODEV.COD_MOTIVODEV(+)
             AND EVENTO.CANAL_VENTA = RESERVA_DTL.CANAL_VENTA
             AND EVENTO.NUM_RESERVA = RESERVA_DTL.NUM_RESERVA
             AND EVENTO.NUM_DETALLE = RESERVA_DTL.NUM_DETALLE
             AND EVENTO.COD_HOJARUTA = HOJARUTA.COD_HOJARUTA(+)
             AND HOJARUTA.COD_TRANSP = TRANSPORTISTA.COD_TRANSP(+)
             AND HOJARUTA.COD_TRANSP = CHOFER.COD_TRANSP(+)
             AND EVENTO.COD_ESTADO = ESTADOSDET.COD_ESTADODET(+)
             AND HOJARUTA.RUT_CHOFER     = CHOFER.RUT_CHOFER(+)
             AND EVENTO.NUM_DETALLE IS NOT NULL
             AND EVENTO.COD_ESTADO  NOT IN ( 1,2,3 )
             AND EVENTO.NUM_RESERVA = reserva
    ORDER BY NUM_RESERVA, EVENTO.FECHA_EVENTO DESC;

 --Requerimiento
 OPEN rsReq FOR
      SELECT r.id_req,
             r.canal_venta,
             r.num_reserva,
             r.cod_tiporeq,
             t.desc_tiporeq,
             r.cc_req,
             r.fecha_req,
             r.observaciones,
             r.login_usuario
        FROM REQUERIMIENTO r, TIPO_REQUERIMIENTO t
       WHERE     1 = 1
             AND r.NUM_RESERVA = reserva
         AND r.cod_tiporeq = t.cod_tiporeq
    ORDER BY r.num_reserva DESC, r.fecha_req DESC;

ELSE --Con RUT
 IF (reserva = 0) THEN --Se buscan las N reservas del RUT
    --Cabecera
    OPEN rsCab FOR
       SELECT *
         FROM (  SELECT r.canal_venta,
                        r.num_reserva,
                        r.cod_estadocab,
                        e.desc_estadocab,
                        r.org_lvl_number,
                        r.num_ordenventa,
                        r.fecha_reserva,
                        r.fecha_pago,
                        r.id_vendedor,
                        r.type_doc,
                        r.num_doc,
                        r.monto,
                        r.doc_date,
                        r.num_caja,
                        r.type_ide,
                        r.ide,
                        r.ide_dv,
                        r.region_cli,
                        r.ciudad_cli,
                        r.comuna_cli,
                        r.comuna_clitipo,
                        r.nombre_cli,
                        r.direccion_cli,
                        r.fono_cli,
                        r.region_desp,
                        r.ciudad_desp,
                        r.comuna_desp,
                        r.comuna_desptipo,
                        r.direccion_desp,
                        r.nombre_desp,
                        r.fono_desp,
                        r.fono2_desp,
                        r.observacion,
                        r.marca_guia,
                        r.campo1,
                        r.campo2,
                        r.campo3,
                        r.campo4,
                        r.campo5,
                        r.campo6,
                        r.campo7,
                        r.campo8,
                        r.campo9,
                        r.campo10,
                        NVL (r.numrsv_padre, 0) AS numrsv_padre,
                        r.monto_desp,
                        r.tipo_rt
                   FROM RESERVA_HDR r, ESTADOSCAB e
                   WHERE r.ide            = rutnum
                     AND r.ide_dv         = rutver
                     AND r.cod_estadocab  = e.cod_estadocab
                     AND r.cod_estadocab NOT IN (1, 2, 3)
               ORDER BY r.fecha_reserva DESC, r.num_reserva DESC)
        WHERE ROWNUM <= (SELECT valor_num FROM parametros WHERE cod_param = 15);

    --Historial de cambios de la cabecera
    OPEN rsCabHist FOR
         select "cod_tipo_evento" as cod_tipo_evento,
                "canal_venta" AS canal_venta,
                "numReserva" AS num_reserva,
                "fecha_evento" AS fecha_evento,
                "cod_estado" AS cod_estado,
                "desc_estado" AS desc_estado,
                "nombre_desp" AS  nombre_desp,
                "direccion_desp"  AS direccion_desp,
                "comuna_desp" AS comuna_desp,
                "ciudad_desp" AS ciudad_desp,
                "region_desp" AS region_desp,
                "fono_desp" AS fono_desp,
                "fono2_desp" AS fono2_desp,
                "observacion" AS observacion,
                "dato" as dato,
                "datoFormateado" as dato_formateado,
                "usuario" AS login_usuario
          from   EVENTOSVIEW2
          where "ide" = rutnum
          AND   "ide_dv" = rutver
          AND    ROWNUM <= (  SELECT valor_num
                                FROM parametros
                              WHERE cod_param = 15)
           ORDER BY "numReserva", "fecha_evento" DESC;

    --Detalle
    OPEN rsDet FOR
         SELECT r.canal_venta,
                r.num_reserva,
                r.num_detalle,
                r.num_ordenventa,
                r.sub_orden,
                r.prd_lvl_number,
                r.prd_name_full,
                NVL (r.cod_estadodet, 0) AS cod_estadodet,
                e.desc_estadodet,
                r.cc_origen,
                r.cc_despacha,
                NVL (r.cc_via, 0) AS cc_via,
                r.prd_kilo,
                r.prd_m3,
                r.tipo_stock,
                r.prioridad,
                r.vendor_number,
                r.fecha_recep_oc,
                r.fecha_ini_proceso,
                r.fecha_despacho,
                r.fecha_entrega,
                r.fecha_entrega_ori,
                r.cantidad,
                r.precio,
                r.tipo_ped_odbms,
                NVL (r.num_ped_odbms, 0) AS num_ped_odbms,
                r.desc_estado_odbms,
                r.tipo_ped_despacho,
                NVL (r.num_ped_despacho, 0) AS num_ped_despacho,
                r.desc_estado_despacho,
                r.num_pkt,
                NVL (r.cod_estadopkt, 0) AS cod_estadopkt,
                NVL (r.cc_envia, 0) AS cc_envia,
                NVL (r.cc_recibe, 0) AS cc_recibe,
                NVL (r.num_gd, 0) AS num_gd,
                r.lpn_case_nbr,
                NVL (r.cod_motivodev, 0) AS cod_motivodev,
                m.desc_motivodev,
                NVL (r.complementos, 0) AS complementos,
                r.fecha_entrega_cli,
                r.bloqueado,
                r.envoltura,
                r.tarjeta_msj,
                r.campo1,
                r.campo2,
                r.campo3,
                r.campo4,
                r.campo5,
                r.campo6,
                r.campo7,
                r.campo8,
                r.campo9,
                r.campo10,
                r.fecha_despacho_ori,
                NVL (r.courier_os, 0) AS courier_os,
                r.courier_estado,
                r.courier_obs,
                NVL (r.num_reserva_b2b, 0) AS num_reserva_b2b,
                r.fecha_despacho2,
                r.fecha_entrega2,
                r.datos_hua,
                NVL (r.cantidad_entrega_cli, 0) AS cantidad_entrega_cli,
                NVL (r.num_gd_prov, 0) AS num_gd_prov,
                NVL (r.num_seriegd_prov, 0) AS num_seriegd_prov,
                NVL (r.num_factura_prov, 0) AS num_factura_prov,
                NVL (r.ide_recive, 0) AS ide_recive,
                NVL (r.ide_dv_recive, 0) AS ide_dv_recive, -- no es integer
                r.nombre_recive,
                NVL (r.ide_chofer, 0) AS ide_chofer,
                NVL (r.ide_dv_chofer, 0) AS ide_dv_chofer,
                r.nombre_chofer,
                r.tipo_despacho,
                r.express,
                NVL (r.prima_pendiente, 0) AS prima_pendiente,
                NVL (r.cant_desp, 0) AS cant_desp,
                NVL (r.cant_ret, 0) AS cant_ret,
                NVL (r.cant_trf, 0) AS cant_trf,
                r.fecha_recep,
                NVL (r.num_ncr, 0) AS num_ncr,
                NVL (r.cant_ncr, 0) AS cant_ncr,
                r.GLOSA_RANGO AS glosa_rango
           FROM reserva_dtl r, ESTADOSDET e, MOTIVODEV m
          WHERE r.NUM_RESERVA IN
                       (SELECT num_reserva
                        FROM (SELECT num_reserva
                              FROM reserva_hdr r2
                              WHERE r2.ide = rutnum
                                AND r2.ide_dv = rutver
                                AND r2.cod_estadocab NOT IN (1, 2, 3)
                              ORDER BY fecha_reserva DESC,
                                         NUM_RESERVA DESC)
                         WHERE ROWNUM <= (SELECT valor_num
                                            FROM parametros
                                           WHERE cod_param = 15))
            AND r.cod_estadodet = e.cod_estadodet
            AND r.cod_motivodev = m.cod_motivodev(+)
       ORDER BY r.num_reserva DESC;

    --Hostorial del detalle
    OPEN rsDetHist FOR
         SELECT EVENTO.COD_TIPOEVENTO AS cod_tipo_evento,
                EVENTO.CANAL_VENTA AS canal_venta,
                EVENTO.NUM_RESERVA AS num_reserva,
                EVENTO.NUM_DETALLE AS num_detalle,
                  RESERVA_DTL.PRD_LVL_NUMBER AS sku,
                  RESERVA_DTL.PRD_NAME_FULL AS sku_desc,
                  EVENTO.FECHA_EVENTO AS fecha_evento,
                  NVL (EVENTO.COD_ESTADO, 0) AS cod_estado,
                  ESTADOSDET.DESC_ESTADODET AS desc_estadodet,
                  EVENTO.FECHA_DESP_FIN AS fecha_despacho,
                  EVENTO.FECHA_ENTREGA_FIN AS fecha_entrega,
                  EVENTO.COURIER_ESTADO AS curier_estado,
                  EVENTO.COURIER_OBS AS curier_obs,
                  NVL (EVENTO.COD_MOTIVODEV, 0) AS cod_motivo_dev,
                  MOTIVODEV.DESC_MOTIVODEV AS desc_motivo_dev,
                  TIPO_EVENTO.DESC_EVENTO AS desc_evento,
                  HOJARUTA.PATENTE AS patente,
                  NVL(HOJARUTA.COD_HOJARUTA,0) AS cod_hojaruta,
                  CHOFER.NOM_CHOFER AS nom_chofer,
                  TRANSPORTISTA.EMPRESA AS empresa,
                  EVENTO.LOGIN_USUARIO AS login_usuario,
             (CASE
                WHEN EVENTO.fecha_entrega_ini != EVENTO.fecha_entrega_fin THEN
                 ' Fecha de Entrega, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.fecha_desp_ini != EVENTO.fecha_desp_fin THEN
                 ' Fecha de Despacho, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.nombre_desp_ini != EVENTO.nombre_desp_fin THEN
                 'Nombre, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.fono_desp_ini != EVENTO.fono_desp_fin THEN
                 'Telefono, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.fono2_desp_ini != EVENTO.fono2_desp_fin THEN
                 'Telefono 2, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.direccion_desp_ini != EVENTO.direccion_desp_fin THEN
                 'Direccion, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.region_desp_ini != EVENTO.region_desp_fin THEN
                 'Region, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.ciudad_desp_ini != EVENTO.ciudad_desp_fin THEN
                 'Ciudad, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.cc_despacha_ini != EVENTO.cc_despacha_fin THEN
                 ' CC Despacha, '
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.comuna_desp_ini != EVENTO.comuna_desp_fin THEN
                 'Comuna, '
                ELSE
                 ''
             END) AS dato,
             (CASE
                WHEN EVENTO.fecha_desp_ini != EVENTO.fecha_desp_fin THEN
                 ' <P><B>' || ' Fecha: </B>de ' || EVENTO.fecha_desp_ini || ' a ' ||
                 EVENTO.fecha_desp_fin || '</P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.fecha_entrega_ini != EVENTO.fecha_entrega_fin THEN
                 ' <P><B>' || ' Fecha: </B>de ' || EVENTO.fecha_entrega_ini ||
                 ' a ' || EVENTO.fecha_entrega_fin || '</P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.nombre_desp_ini != EVENTO.nombre_desp_fin THEN
                 '<P><B> Nombre:</B> de ' || EVENTO.nombre_desp_ini || ' a ' ||
                 EVENTO.nombre_desp_fin || '</P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.fono_desp_ini != EVENTO.fono_desp_fin THEN
                 '<P><B> Telefono: </B>de ' || EVENTO.fono_desp_ini || ' a ' ||
                 EVENTO.fono_desp_fin || '</P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.fono2_desp_ini != EVENTO.fono2_desp_fin THEN
                 '<P><B> Telefono 2: </B>de ' || EVENTO.fono2_desp_ini || ' a ' ||
                 EVENTO.fono2_desp_fin || '</P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.direccion_desp_ini != EVENTO.direccion_desp_fin THEN
                 '<P><B> Direccion:</B> de ' || EVENTO.direccion_desp_ini ||
                 ' a ' || EVENTO.direccion_desp_fin || '</P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.region_desp_ini != EVENTO.region_desp_fin THEN
                 '<P><B> Ciudad: </B>de ' ||
                 (SELECT "descripcion"
                    FROM "REGIONVIEW"
                   WHERE "codigo" = EVENTO.region_desp_ini) || ' a ' ||
                 (SELECT "descripcion"
                    FROM "REGIONVIEW"
                   WHERE "codigo" = EVENTO.region_desp_fin) || '</P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.ciudad_desp_ini != EVENTO.ciudad_desp_fin THEN
                 '<P><B> Ciudad: </B>de ' ||
                 (SELECT "descripcion"
                    FROM "CIUDADVIEW"
                   WHERE "codigo" = EVENTO.ciudad_desp_ini) || ' a ' ||
                 (SELECT "descripcion"
                    FROM "CIUDADVIEW"
                   WHERE "codigo" = EVENTO.ciudad_desp_fin) || '</P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.comuna_desp_ini != EVENTO.comuna_desp_fin THEN
                 '<P><B> Comuna: </B>de ' ||
                 (SELECT "descripcion"
                    FROM "COMUNAVIEW"
                   WHERE "codigo" = EVENTO.comuna_desp_ini) || ' a ' ||
                 (SELECT "descripcion"
                    FROM "COMUNAVIEW"
                   WHERE "codigo" = EVENTO.comuna_desp_fin) || '<P></P></P>'
                ELSE
                 ''
             END || CASE
                WHEN EVENTO.cc_despacha_ini != EVENTO.cc_despacha_fin THEN
                 '<P><B> CC Despacha: </B>de ' || EVENTO.cc_despacha_ini || ' a ' ||
                 EVENTO.cc_despacha_fin || '</P>'
                ELSE
                 ''
             END) AS dato_formateado
             FROM EVENTO,
                  MOTIVODEV,
                  RESERVA_DTL,
                  ESTADOSDET,
                  TIPO_EVENTO,
                  HOJARUTA,
                  TRANSPORTISTA,
                  CHOFER
            WHERE TIPO_EVENTO.COD_TIPOEVENTO = EVENTO.COD_TIPOEVENTO
              AND EVENTO.COD_MOTIVODEV = MOTIVODEV.COD_MOTIVODEV(+)
              AND EVENTO.CANAL_VENTA = RESERVA_DTL.CANAL_VENTA
              AND EVENTO.NUM_RESERVA = RESERVA_DTL.NUM_RESERVA
              AND EVENTO.NUM_DETALLE = RESERVA_DTL.NUM_DETALLE
              AND EVENTO.COD_HOJARUTA = HOJARUTA.COD_HOJARUTA(+)
              AND HOJARUTA.COD_TRANSP = TRANSPORTISTA.COD_TRANSP(+)
              AND HOJARUTA.COD_TRANSP = CHOFER.COD_TRANSP(+)
              AND EVENTO.COD_ESTADO = ESTADOSDET.COD_ESTADODET(+)
              AND HOJARUTA.RUT_CHOFER = CHOFER.RUT_CHOFER(+)
              AND EVENTO.NUM_DETALLE IS NOT NULL
              AND EVENTO.COD_ESTADO NOT IN (1, 2, 3)
              AND EVENTO.NUM_RESERVA IN
                   (SELECT num_reserva
                    FROM (SELECT num_reserva
                          FROM reserva_hdr r2
                          WHERE r2.ide = rutnum
                            AND r2.ide_dv = rutver
                            AND r2.cod_estadocab NOT IN (1, 2, 3)
                          ORDER BY fecha_reserva DESC,
                                   num_reserva DESC)
                    WHERE ROWNUM <= (SELECT valor_num
                                     FROM parametros
                                     WHERE cod_param = 15))
         ORDER BY EVENTO.NUM_RESERVA, EVENTO.FECHA_EVENTO DESC;

      --Requerimiento
      OPEN rsReq FOR
           SELECT r.id_req,
                  r.canal_venta,
                  r.num_reserva,
                  r.cod_tiporeq,
                  t.desc_tiporeq,
                  r.cc_req,
                  r.fecha_req,
                  r.observaciones,
                  r.login_usuario
           FROM REQUERIMIENTO r, TIPO_REQUERIMIENTO t
           WHERE r.num_reserva IN
                 (SELECT num_reserva
                  FROM (SELECT num_reserva
                        FROM reserva_hdr r2
                        WHERE r2.ide = rutnum
                          AND r2.ide_dv = rutver
                          AND r2.cod_estadocab NOT IN (1, 2, 3)
                        ORDER BY fecha_reserva DESC,
                                 num_reserva DESC)
                  WHERE ROWNUM <= (SELECT valor_num
                                   FROM parametros
                                   WHERE cod_param = 15))
             AND r.cod_tiporeq = t.cod_tiporeq
         ORDER BY r.num_reserva DESC, r.fecha_req DESC;

   ELSE --Se buscan las N reservas del RUT
      --Cabecera
      OPEN rsCab FOR
           SELECT r.canal_venta,
                  r.num_reserva,
                  r.cod_estadocab,
                  e.desc_estadocab,
                  r.org_lvl_number,
                  r.num_ordenventa,
                  r.fecha_reserva,
                  r.fecha_pago,
                  r.id_vendedor,
                  r.type_doc,
                  r.num_doc,
                  r.monto,
                  r.doc_date,
                  r.num_caja,
                  r.type_ide,
                  r.ide,
                  r.ide_dv,
                  r.region_cli,
                  r.ciudad_cli,
                  r.comuna_cli,
                  r.comuna_clitipo,
                  r.nombre_cli,
                  r.direccion_cli,
                  r.fono_cli,
                  r.region_desp,
                  r.ciudad_desp,
                  r.comuna_desp,
                  r.comuna_desptipo,
                  r.direccion_desp,
                  r.nombre_desp,
                  r.fono_desp,
                  r.fono2_desp,
                  r.observacion,
                  r.marca_guia,
                  r.campo1,
                  r.campo2,
                  r.campo3,
                  r.campo4,
                  r.campo5,
                  r.campo6,
                  r.campo7,
                  r.campo8,
                  r.campo9,
                  r.campo10,
                  NVL (r.numrsv_padre, 0) AS numrsv_padre,
                  r.monto_desp,
                  r.tipo_rt
             FROM RESERVA_HDR r, ESTADOSCAB e
             WHERE r.NUM_RESERVA = reserva
               AND r.ide = rutnum
               AND r.ide_dv = rutver
               AND r.cod_estadocab = e.cod_estadocab
               AND r.cod_estadocab NOT IN (1, 2, 3)
         ORDER BY r.fecha_reserva DESC, r.num_reserva DESC;

      --Historial de cambios de la cabecera
      OPEN rsCabHist FOR
           SELECT EVENTO.COD_TIPOEVENTO AS cod_tipo_evento,
                  CANAL_VENTA AS canal_venta,
                  NUM_RESERVA AS num_reserva,
                  FECHA_EVENTO AS fecha_evento,
                  COD_ESTADO AS cod_estado,
                  desc_estadocab AS desc_estado,
                  NOMBRE_DESP_FIN AS nombre_desp,
                  DIRECCION_DESP_FIN AS direccion_desp,
                  COMUNA_DESP_FIN AS comuna_desp,
                  CIUDAD_DESP_FIN AS ciudad_desp,
                  REGION_DESP_FIN AS region_desp,
                  FONO_DESP_FIN AS fono_desp,
                  FONO2_DESP_FIN AS fono2_desp,
                  OBSERVACION_FIN AS observacion
             FROM EVENTO, ESTADOSCAB, TIPO_EVENTO
            WHERE TIPO_EVENTO.COD_TIPOEVENTO = EVENTO.COD_TIPOEVENTO
              AND COD_ESTADO = COD_ESTADOCAB(+)
              AND NUM_DETALLE IS NULL
              AND EVENTO.COD_TIPOEVENTO IN (1, 2)
              AND NUM_RESERVA IN
                     (SELECT num_reserva
                      FROM reserva_hdr r2
                      WHERE r2.ide = rutnum
                        AND r2.ide_dv = rutver
                        AND r2.num_reserva = reserva
                        AND r2.cod_estadocab NOT IN (1, 2, 3))
         ORDER BY NUM_RESERVA, FECHA_EVENTO DESC;

      --Detalle
      OPEN rsDet FOR
           SELECT r.canal_venta,
                  r.num_reserva,
                  r.num_detalle,
                  r.num_ordenventa,
                  r.sub_orden,
                  r.prd_lvl_number,
                  r.prd_name_full,
                  NVL (r.cod_estadodet, 0) AS cod_estadodet,
                  e.desc_estadodet,
                  r.cc_origen,
                  r.cc_despacha,
                  NVL (r.cc_via, 0) AS cc_via,
                  r.prd_kilo,
                  r.prd_m3,
                  r.tipo_stock,
                  r.prioridad,
                  r.vendor_number,
                  r.fecha_recep_oc,
                  r.fecha_ini_proceso,
                  r.fecha_despacho,
                  r.fecha_entrega,
                  r.fecha_entrega_ori,
                  r.cantidad,
                  r.precio,
                  r.tipo_ped_odbms,
                  NVL (r.num_ped_odbms, 0) AS num_ped_odbms,
                  r.desc_estado_odbms,
                  r.tipo_ped_despacho,
                  NVL (r.num_ped_despacho, 0) AS num_ped_despacho,
                  r.desc_estado_despacho,
                  r.num_pkt,
                  NVL (r.cod_estadopkt, 0) AS cod_estadopkt,
                  NVL (r.cc_envia, 0) AS cc_envia,
                  NVL (r.cc_recibe, 0) AS cc_recibe,
                  NVL (r.num_gd, 0) AS num_gd,
                  r.lpn_case_nbr,
                  NVL (r.cod_motivodev, 0) AS cod_motivodev,
                  m.desc_motivodev,
                  NVL (r.complementos, 0) AS complementos,
                  r.fecha_entrega_cli,
                  r.bloqueado,
                  r.envoltura,
                  r.tarjeta_msj,
                  r.campo1,
                  r.campo2,
                  r.campo3,
                  r.campo4,
                  r.campo5,
                  r.campo6,
                  r.campo7,
                  r.campo8,
                  r.campo9,
                  r.campo10,
                  r.fecha_despacho_ori,
                  NVL (r.courier_os, 0) AS courier_os,
                  r.courier_estado,
                  r.courier_obs,
                  NVL (r.num_reserva_b2b, 0) AS num_reserva_b2b,
                  r.fecha_despacho2,
                  r.fecha_entrega2,
                  r.datos_hua,
                  NVL (r.cantidad_entrega_cli, 0) AS cantidad_entrega_cli,
                  NVL (r.num_gd_prov, 0) AS num_gd_prov,
                  NVL (r.num_seriegd_prov, 0) AS num_seriegd_prov,
                  NVL (r.num_factura_prov, 0) AS num_factura_prov,
                  NVL (r.ide_recive, 0) AS ide_recive,
                  NVL (r.ide_dv_recive, 0) AS ide_dv_recive -- no es integer
                                                           ,
                  r.nombre_recive,
                  NVL (r.ide_chofer, 0) AS ide_chofer,
                  NVL (r.ide_dv_chofer, 0) AS ide_dv_chofer,
                  r.nombre_chofer,
                  r.tipo_despacho,
                  r.express,
                  NVL (r.prima_pendiente, 0) AS prima_pendiente,
                  NVL (r.cant_desp, 0) AS cant_desp,
                  NVL (r.cant_ret, 0) AS cant_ret,
                  NVL (r.cant_trf, 0) AS cant_trf,
                  r.fecha_recep,
                  NVL (r.num_ncr, 0) AS num_ncr,
                  NVL (r.cant_ncr, 0) AS cant_ncr,
                  r.GLOSA_RANGO AS glosa_rango
             FROM reserva_dtl r, ESTADOSDET e, MOTIVODEV m
             WHERE r.num_reserva IN
                     (SELECT num_reserva
                      FROM reserva_hdr r2
                      WHERE r2.ide = rutnum
                        AND r2.ide_dv = rutver
                        AND r2.num_reserva = reserva
                        AND r2.cod_estadocab NOT IN (1, 2, 3))
               AND r.cod_estadodet = e.cod_estadodet
               AND r.cod_motivodev = m.cod_motivodev
         ORDER BY r.num_reserva DESC;

      OPEN rsDetHist FOR
           SELECT EVENTO.COD_TIPOEVENTO AS cod_tipo_evento,
                  EVENTO.CANAL_VENTA AS canal_venta,
                  EVENTO.NUM_RESERVA AS num_reserva,
                  EVENTO.NUM_DETALLE AS num_detalle,
                  RESERVA_DTL.PRD_LVL_NUMBER AS sku,
                  RESERVA_DTL.PRD_NAME_FULL AS sku_desc,
                  EVENTO.FECHA_EVENTO AS fecha_evento,
                  NVL (EVENTO.COD_ESTADO, 0) AS cod_estado,
                  ESTADOSDET.DESC_ESTADODET AS desc_estadodet,
                  EVENTO.FECHA_DESP_FIN AS fecha_despacho,
                  EVENTO.FECHA_ENTREGA_FIN AS fecha_entrega,
                  EVENTO.COURIER_ESTADO AS curier_estado,
                  EVENTO.COURIER_OBS AS curier_obs,
                  NVL (EVENTO.COD_MOTIVODEV, 0) AS cod_motivo_dev,
                  MOTIVODEV.DESC_MOTIVODEV AS desc_motivo_dev,
                  TIPO_EVENTO.DESC_EVENTO AS desc_evento,
                  HOJARUTA.PATENTE AS patente,
                  NVL(HOJARUTA.COD_HOJARUTA,0) AS cod_hojaruta,
                  CHOFER.NOM_CHOFER AS nom_chofer,
                  TRANSPORTISTA.EMPRESA AS empresa,
                  EVENTO.LOGIN_USUARIO AS login_usuario
             FROM EVENTO,
                  MOTIVODEV,
                  RESERVA_DTL,
                  ESTADOSDET,
                  TIPO_EVENTO,
                  HOJARUTA,
                  TRANSPORTISTA,
                  CHOFER
            WHERE TIPO_EVENTO.COD_TIPOEVENTO = EVENTO.COD_TIPOEVENTO
              AND EVENTO.COD_MOTIVODEV = MOTIVODEV.COD_MOTIVODEV(+)
              AND EVENTO.CANAL_VENTA = RESERVA_DTL.CANAL_VENTA
              AND EVENTO.NUM_RESERVA = RESERVA_DTL.NUM_RESERVA
              AND EVENTO.NUM_DETALLE = RESERVA_DTL.NUM_DETALLE
              AND EVENTO.COD_HOJARUTA = HOJARUTA.COD_HOJARUTA(+)
              AND HOJARUTA.COD_TRANSP = TRANSPORTISTA.COD_TRANSP(+)
              AND HOJARUTA.COD_TRANSP = CHOFER.COD_TRANSP(+)
              AND EVENTO.COD_ESTADO = ESTADOSDET.COD_ESTADODET(+)
              AND HOJARUTA.RUT_CHOFER = CHOFER.RUT_CHOFER(+)
              AND EVENTO.NUM_DETALLE IS NOT NULL
              AND EVENTO.COD_ESTADO NOT IN (1, 2, 3)
              AND EVENTO.NUM_RESERVA IN
                   (SELECT num_reserva
                    FROM reserva_hdr r2
                    WHERE r2.ide = rutnum
                      AND r2.ide_dv = rutver
                      AND r2.num_reserva = reserva
                      AND r2.cod_estadocab NOT IN (1, 2, 3))
         ORDER BY EVENTO.FECHA_EVENTO DESC;

      --Requerimiento
      OPEN rsReq FOR
           SELECT r.id_req,
                  r.canal_venta,
                  r.num_reserva,
                  r.cod_tiporeq,
                  t.desc_tiporeq,
                  r.cc_req,
                  r.fecha_req,
                  r.observaciones,
                  r.login_usuario
             FROM REQUERIMIENTO r, TIPO_REQUERIMIENTO t
             WHERE r.num_reserva IN
                     (SELECT num_reserva
                      FROM reserva_hdr r2
                      WHERE r2.ide = rutnum
                        AND r2.ide_dv = rutver
                        AND r2.num_reserva = reserva
                        AND r2.cod_estadocab NOT IN (1, 2, 3))
               AND r.cod_tiporeq = t.cod_tiporeq
         ORDER BY r.num_reserva DESC, r.fecha_req DESC;

   END IF;
END IF;
END GET_RESERVA2;

PROCEDURE ENVIAR_AJUSTE(
  in_key_origen          in wli_event_itfkey_dad.key_origen%TYPE,
  in_cantidad_reg        in  number,                 -- cantidad de registros a buscar
  in_proceso_luw         in  char default 'T',       -- T: commit; F: no se ejecuta commit
  out_cuadrat_numlin     out number,                 -- cantidad de lineas
  out_cuadrat_campo      out varchar2,               -- campo a cuadrar
  out_cuadrat_suma_zon   out number,                 -- suma de cantidad en relacion a al campo a cuadrar
  OUT_STATUS_CODE        out number,
  OUT_STATUS_MSG         out varchar2,
  "rs"  out sys_refcursor) AS

BEGIN
  DECLARE
   var_cont_reg NUMBER;

BEGIN
  IF IN_CANTIDAD_REG IS NULL or IN_CANTIDAD_REG < 0 THEN
    OUT_STATUS_CODE := 1;
    OUT_STATUS_MSG  := 'DAD: ' ||  'Cantidad de Registros no debe ser nulo o debe ser mayor igual a cero';
    RETURN;
  END IF;

  OUT_STATUS_CODE    := 0;
  OUT_STATUS_MSG     := 'OK';
  OUT_CUADRAT_CAMPO := 'cantidad';

  BEGIN
    SELECT SUM(cantidad), COUNT(sequencia_dad),'cantidad'
    INTO OUT_CUADRAT_SUMA_ZON , OUT_CUADRAT_NUMLIN, OUT_CUADRAT_CAMPO FROM WLI_STAGE_AJUSTE
    WHERE sequencia_dad = IN_KEY_ORIGEN  ;

    EXCEPTION
    WHEN NO_DATA_FOUND THEN
      OUT_STATUS_CODE := 0;
      OUT_STATUS_MSG  := 'DAD: ' ||  'No Existen Datos Nuevos en la Tabla WLI_STAGE_AJUSTE para la key: ' || IN_KEY_ORIGEN;
    WHEN OTHERS THEN
      OUT_STATUS_CODE := 1;
      OUT_STATUS_MSG  := 'DAD: ' ||  SQLERRM;
      RETURN;
    END;

    OPEN  "rs" FOR
    SELECT
          SEQUENCIA_DAD,
          CANAL_VENTA,
          NUM_RESERVA,
          NUMERO_DETALLE,
          NUM_ORDENVENTA,
          CC_MOV,
          COD_TIPO_EVENTO,
          COD_MOTIVO,
          DESCRIP_MOTIVO,
          DOC_EVENTO,
          PRODUCTO,
          CANTIDAD,
          USUARIO,
          FECHA,
          CAMPO_1,
          CAMPO_2,
          CAMPO_3
        FROM WLI_STAGE_AJUSTE
        WHERE SEQUENCIA_DAD = IN_KEY_ORIGEN;

    IF OUT_CUADRAT_NUMLIN = 0 THEN
      OUT_STATUS_CODE := 1;
      OUT_STATUS_MSG  :='DAD: ' ||  'No Existen Datos Para Enviar';
    ELSE
      IF IN_PROCESO_LUW = 'T' THEN
        COMMIT ;
        OUT_STATUS_MSG  := 'OK';
      END IF;
    END IF;
  END;
END ENVIAR_AJUSTE;

PROCEDURE CONFIRMA_FOLIO(
  in_key_origen          in wli_event_itfkey_dad.key_origen%TYPE,
  in_cantidad_reg        in  number,                 -- cantidad de registros a buscar
  in_proceso_luw         in  char default 'T',       -- T: commit; F: no se ejecuta commit
  out_cuadrat_numlin     out number,                 -- cantidad de lineas
  out_cuadrat_campo      out varchar2,               -- campo a cuadrar
  out_cuadrat_suma_zon   out number,                 -- suma de cantidad en relacion a al campo a cuadrar
  OUT_STATUS_CODE        out number,
  OUT_STATUS_MSG         out varchar2,
  "rs"  out sys_refcursor)AS

BEGIN
        DECLARE
            var_cont_reg              NUMBER;

        BEGIN

               IF IN_CANTIDAD_REG IS NULL or IN_CANTIDAD_REG < 0 THEN
                  OUT_STATUS_CODE := 1;
                  OUT_STATUS_MSG  := 'DAD: ' ||  'Cantidad de Registros no debe ser nulo o debe ser mayor igual a cero';
                   RETURN;
               END IF;

             OUT_STATUS_CODE    := 0;
             OUT_STATUS_MSG     := 'OK';
             OUT_CUADRAT_CAMPO := 'cantidad';

            BEGIN

             SELECT SUM(ID), COUNT(ID),'id'
             INTO OUT_CUADRAT_SUMA_ZON , OUT_CUADRAT_NUMLIN, OUT_CUADRAT_CAMPO FROM WLI_STAGE_FOLIO
            WHERE ID = IN_KEY_ORIGEN group by 'id'  ;
                     EXCEPTION
                   WHEN NO_DATA_FOUND THEN
                        OUT_STATUS_CODE := 0;
                         OUT_STATUS_MSG  := 'DAD: ' ||  'No Existen Datos Nuevos en la Tabla WLI_STAGE_FOLIO para la key: ' || IN_KEY_ORIGEN;
                   WHEN OTHERS THEN
                        OUT_STATUS_CODE := 1;
                        OUT_STATUS_MSG  := 'DAD: ' ||  SQLERRM;
                        RETURN;
            END;

               OPEN  "rs" for
               SELECT SEQ_FOLIO, ID, STATUS FROM WLI_STAGE_FOLIO
              WHERE ID = IN_KEY_ORIGEN;

              IF OUT_CUADRAT_NUMLIN = 0 THEN
                 OUT_STATUS_CODE := 1;
                 OUT_STATUS_MSG  :='DAD: ' ||  'No Existen Datos Para Enviar';
              ELSE
                  if IN_PROCESO_LUW = 'T' then
                     COMMIT ;
                     OUT_STATUS_MSG  := 'OK';
                  end if;
              END IF;

        END;
    END CONFIRMA_FOLIO;

 PROCEDURE ENVIAR_GDD
  (
  in_key_origen          in wli_event_itfkey_dad.key_origen%TYPE,
   in_proceso_luw         in  char default 'T',       -- T: commit; F: no se ejecuta commit
   OUT_STATUS_CODE         out number,
   OUT_STATUS_MSG          out varchar2,
   "rs_hdr"  out sys_refcursor,
   "rs_dtl"  out sys_refcursor
  )
     AS

        BEGIN
        DECLARE
            var_cont_reg              NUMBER;
        BEGIN

        OUT_STATUS_CODE := 0;
        OUT_STATUS_MSG := 'OK';



    OPEN  "rs_hdr" for
    SELECT CCOSTO_GD,
          NUM_GD,
          FECHA_GD,
          ESTADO_GD,
          FECHA_RESERVA,
          IDE,
          IDE_DV,
          NOMBRE_CLI,
          DIRECCION_CLI,
          COMUNA_CLI,
          CIUDAD_CLI,
          FONO_CLI,
          DIRECCION_DESP,
          COMUNA_DESP,
          CIUDAD_DESP,
          MONTO_TOTAL,
          MONTO_NETO,
          MONTO_IVA,
         (select COD_SII from DOC_SII WHERE DOC_SII.TYPE_DOC= GDD_HDR.TYPE_DOC) AS TYPE_DOC,
          NUM_DOC,
          ID_VENDEDOR,
          ORG_LVL_NUMBER,
          OBSERVACION,
          PATENTE,
          RUT_CHOFER,
          COD_HOJARUTA,
          TS_TIMBRE,
          GIRO
        FROM GDD_HDR
        WHERE NUM_GD=in_key_origen ;


          OPEN  "rs_dtl" for
          SELECT CCOSTO_GD,
              NUM_GD,
              NUM_LINEA,
              CANAL_VENTA,
              NUM_RESERVA,
              NUM_DETALLE,
              PRD_LVL_NUMBER,
              PRD_NAME_FULL,
              CANTIDAD,
              PRECIO,
              MONTO_TOTAL,
              DATOS_HUA,
              CLACOM,
              BULTOS,
              UNID_MEDIDA
          FROM GDD_DTL WHERE NUM_GD=in_key_origen ;


        END ;
END ENVIAR_GDD;

PROCEDURE INFOPALET(
  PCEMPRESA  IN  VARCHAR2,
  PCNUMEXPE  IN  NUMBER,
  PCNUPALET  IN  VARCHAR2,
  PQTBULTOS  IN  NUMBER,
  XPARAMETR  IN  VARCHAR2,
  RESULTADO  OUT  CLOB) AS
BEGIN
  DECLARE
  --PCNUMEXPE Hoja de ruta
  --PCNUPALET numero de reserva
  CONTEXTO   DBMS_XMLQUERY.ctxtype;
  hoja_ruta          NUMBER;
  reserva            NUMBER;
  is_hr              VARCHAR2(3);
  valida_reserva     NUMBER;
  valida_rsv         NUMBER;
  valida_hr          NUMBER;
  var_fecha1         VARCHAR2(60);
  var_fecha2         VARCHAR2(60);
  var_fecha3         VARCHAR2(60);
  validador          NUMBER;
  dias               NUMBER;
  precio             VARCHAR2(60);
  kilo               VARCHAR2(60);
  BEGIN
    is_hr :='';
    validador := 0;
    dias := 0;
    IF PCNUMEXPE IS NOT NULL THEN
          is_hr := 'HR';
          SELECT cod_hojaruta
          INTO   hoja_ruta
          FROM   hojaruta
          WHERE  cod_hojaruta = PCNUMEXPE;
    ELSE
         CONTEXTO := DBMS_XMLQUERY.newContext (' SELECT ' || nvl(PCNUMEXPE,-1) || ' as cnumexpe,' || ''' HOJA_DE_RUTA_NO_INGRESADA ''' || '  as derror from dual ');
         RESULTADO := DBMS_XMLQUERY.getXML(CONTEXTO);
         DBMS_XMLQUERY.closeContext(CONTEXTO);
         RETURN;
         --dbms_output.put_line( RESULTADO);
    END IF ;

    IF PCNUPALET IS NOT NULL THEN
      is_hr := 'HR';
      SELECT num_reserva
      INTO   reserva
      FROM reserva_hdr
      WHERE num_reserva = PCNUPALET;
    ELSE
         CONTEXTO := DBMS_XMLQUERY.newContext(' select ' || nvl(PCNUPALET,-1) || ' as cnumpalet,' || ''' RESERVA_NO_INGRESADA ''' || '  as derror from dual ');
         RESULTADO := DBMS_XMLQUERY.getXML(CONTEXTO);
         DBMS_XMLQUERY.closeContext(CONTEXTO);
         RETURN;
         --dbms_output.put_line( RESULTADO);
    END IF;

    is_hr := '0';
    SELECT DISTINCT num_reserva
    INTO valida_reserva
    FROM preruteo_reservadet
    WHERE cod_hojaruta = PCNUMEXPE
    AND   num_reserva  = PCNUPALET;
    IF  is_hr = '0' THEN
      -- Se calcula si se obtiene una hr mayor en caso de serlo no permitir el envio de carga a courier
      SELECT MAX(hr)
      INTO valida_hr
      FROM (
        SELECT MAX(pr.cod_hojaruta) hr
        FROM preruteo_reservadet pr
        WHERE num_reserva = PCNUPALET
        AND cod_hojaruta  > PCNUMEXPE
        AND num_detalle IN (SELECT p2.num_detalle
                            FROM preruteo_reservadet p2
                            WHERE p2.num_reserva = PCNUPALET
                            AND p2.cod_hojaruta = PCNUMEXPE)
        GROUP BY pr.num_reserva
        UNION SELECT 0 hr FROM dual
        ORDER BY hr DESC);

        IF valida_hr != 0 THEN
         CONTEXTO := DBMS_XMLQUERY.newContext(' select ' || nvl(PCNUMEXPE,-1) || ' as cnumexpe,' || ''' HOJA_RUTA_HA_CAMBIADO_PARA_LA RESERVA = ''' || ' || ' || PCNUPALET || ' as derror from dual ');
         RESULTADO := DBMS_XMLQUERY.getXML(CONTEXTO);
         DBMS_XMLQUERY.closeContext(CONTEXTO);
         RETURN;
        END IF;
    END IF;

    --Se calcuala fecha de entrega
    SELECT to_char(fecha_entrega + (SELECT COUNT(*) FROM dias_feriados WHERE fecha BETWEEN fecha_reparto      AND fecha_entrega),'DD-MM-YYYY') AS fecha
    INTO   var_fecha1
    FROM (SELECT (SELECT trunc(h.fecha) FROM hojaruta h WHERE h.cod_hojaruta = PCNUMEXPE)                  AS fecha_reparto,
                 (SELECT trunc(h.fecha) FROM hojaruta h WHERE h.cod_hojaruta = PCNUMEXPE) + cc.tiempo_desp AS fecha_entrega
          FROM cc_comuna cc, reserva_hdr rh
          WHERE rh.num_reserva = PCNUPALET
            AND rh.canal_venta = cc.canal_venta
            AND rh.comuna_desp = cc.comuna
            AND cc.ccosto IN (
                SELECT DISTINCT p.ccosto
                FROM preruteo_reservadet p
                WHERE p.cod_hojaruta = PCNUMEXPE)
         )fechas;

    var_fecha2 := trim(var_fecha1);

     WHILE validador = 0 LOOP
       SELECT nvl(COUNT(*),0)
       INTO   dias
       FROM   dias_feriados d
       WHERE  d.fecha IN (to_date(TRIM(var_fecha2), 'DD-MM-YYYY'));

       var_fecha2 := to_char(to_date(trim(var_fecha2), 'DD-MM-YYYY') + dias, 'DD-MM-YYYY');

       IF dias = 0 THEN
         validador := 1;
       END IF;
     END LOOP;

    var_fecha3 := '''' || trim(to_char(to_date(var_fecha2,'dd/mm/yyyy'), 'dd/mm/yyyy')) || ' 0:0:0' || '''';

    --guardar fecha_entrega_courier en la tabla reserva_dtl
    UPDATE reserva_dtl r3 SET r3.fecha_entrega_courier = to_date(var_fecha2,'DD-MM-YYYY')
    WHERE  r3.num_reserva = PCNUPALET
    AND    (r3.canal_venta, r3.num_reserva, r3.num_detalle)
    IN     (SELECT pr.canal_venta, pr.num_reserva, pr.num_detalle
            FROM preruteo_reservadet pr
            WHERE pr.canal_venta = r3.canal_venta
            AND   pr.num_reserva = r3.num_reserva
            AND   pr.num_detalle = r3.num_detalle
            AND   pr.cod_hojaruta = PCNUMEXPE
            AND   pr.num_reserva  = PCNUPALET)
    ;
    --calculo de precio o monto
    --SELECT TO_CHAR(1000800000.3829,'999999999999D9999','NLS_NUMERIC_CHARACTERS = '',.''') FROM DUAL;
    --calculo de kilos
     SELECT to_char(SUM(rd.precio * rd.cantidad),'99999999999'), to_char(SUM(rd.prd_kilo * cantidad),'999999999999990D999', 'NLS_NUMERIC_CHARACTERS = ''.,''' )
     INTO   precio, kilo
     FROM   reserva_dtl rd
     WHERE rd.num_reserva =PCNUPALET
     AND   rd.tipo_despacho ='D'
     AND   (rd.canal_venta, rd.num_reserva, rd.num_detalle) IN
           (SELECT pr.canal_venta, pr.num_reserva, abs(pr.num_detalle)
            FROM   preruteo_reservadet pr
            WHERE  pr.canal_venta = rd.canal_venta
            AND    pr.num_reserva = rd.num_reserva
            AND    abs(pr.num_detalle) = abs(rd.num_detalle)
            AND    pr.num_reserva =  PCNUPALET
            AND    pr.cod_hojaruta = PCNUMEXPE
            );


     precio:= '''' || trim(precio) || '''';
     kilo :=  '''' || trim(kilo) || '''';

      CONTEXTO := DBMS_XMLQUERY.newContext ('SELECT DISTINCT pr.cod_hojaruta AS c_soc, '
       || trim(var_fecha3) || '         AS f_rep,
       rh.num_reserva                   AS n_pallet,
       ' || '''DAD''' || '              AS a_tipo,
       rh.region_cli                    AS a_origen,
       rh.ciudad_cli                    AS a_destino, '
       || kilo || '                     AS c_peso, '
       || precio || '                   AS n_monto,
       nvl(rh.bultos,0)                 AS n_bultos,
       ' || 'rh.ide || ' || '
       ' || '''-''' || '|| rh.ide_dv    AS a_rut,
       rh.nombre_desp                   AS a_nombre,
       rh.direccion_desp                AS a_direccion,
       rh.comuna_desp                   AS a_comuna,
       rh.region_desp                   AS a_region,
       nvl(rh.fono_desp, rh.fono2_desp) AS a_fono,
       rh.fono2_desp                    AS a_fono2,
       rh.e_mail                        AS a_mail,
       cursor(select distinct num_reserva as n_f12, cursor(SELECT distinct trim(r1.prd_lvl_number) AS a_sku,
                                                         CAST (r1.prd_m3 * r1.cantidad AS NUMERIC (9, 3)) AS n_alto ,
                                                         CAST (r1.prd_m3 * r1.cantidad AS NUMERIC (9, 3)) AS n_ancho,
                                                         CAST (r1.prd_m3 * r1.cantidad AS NUMERIC (9, 3)) AS n_largo,
                                                         r1.prd_name_full                                 AS a_detalle,
                                                         r1.cantidad                                      AS qcantida
                                                         FROM reserva_dtl r1 WHERE r1.num_reserva = '|| PCNUPALET || ' and r1.cod_hojaruta = '|| PCNUMEXPE ||' ) as productos
       from reserva_hdr where num_reserva = ' || PCNUPALET || ' ) as f12
FROM   reserva_hdr rh,
       preruteo_reservadet pr
WHERE   rh.canal_venta = pr.canal_venta
AND    rh.num_reserva = pr.num_reserva
AND    rh.num_reserva = ' || PCNUPALET || '
AND    pr.cod_hojaruta = ' || PCNUMEXPE || '
GROUP BY pr.cod_hojaruta,
         rh.num_reserva,
         rh.region_cli,
         rh.ciudad_cli,
         rh.ciudad_cli,
         rh.bultos,
         rh.ide||''-''||rh.ide_dv,
         rh.nombre_desp,
         rh.direccion_desp,
         rh.comuna_desp,
         rh.region_desp,
         rh.fono_desp,
         rh.fono2_desp,
         rh.e_mail');

    RESULTADO := DBMS_XMLQUERY.getXML(CONTEXTO);

    DBMS_XMLQUERY.closeContext(CONTEXTO);
     --dbms_output.put_line( RESULTADO);

     EXCEPTION WHEN NO_DATA_FOUND  THEN
         IF  trim(is_hr) = 'RSV'  THEN
         CONTEXTO := DBMS_XMLQUERY.newContext(' select ' || nvl(PCNUPALET,-1) || ' as cnumpalet,' || ''' RESERVA_NO_VALIDA ''' || '  as derror from dual ');
         RESULTADO := DBMS_XMLQUERY.getXML(CONTEXTO);
         DBMS_XMLQUERY.closeContext(CONTEXTO);
         --dbms_output.put_line( RESULTADO);
         ELSIF trim(is_hr) = 'HR' THEN
         CONTEXTO := DBMS_XMLQUERY.newContext(' select ' || nvl(PCNUMEXPE,-1) || ' as cnumexpe,' || ''' HOJA_RUTA_NO_VALIDA ''' || '  as derror from dual ');
         RESULTADO := DBMS_XMLQUERY.getXML(CONTEXTO);
         DBMS_XMLQUERY.closeContext(CONTEXTO);
         --dbms_output.put_line( RESULTADO);
         ELSIF trim(is_hr) = '0' THEN
         CONTEXTO := DBMS_XMLQUERY.newContext(' select ' || nvl(PCNUPALET,-1) || ' as cnumpalet,' || ''' RESRVA_NO_EXISTE_PARA_HOJA_DE_RUTA = ''' || ' || ' || PCNUMEXPE || ' as derror from dual ');
         RESULTADO := DBMS_XMLQUERY.getXML(CONTEXTO);
         DBMS_XMLQUERY.closeContext(CONTEXTO);
         --dbms_output.put_line( RESULTADO);
         END IF;
  END;

END INFOPALET;

PROCEDURE TCKRESERVAHDR(
  vrut         IN  VARCHAR2,
  vnum_reserva IN  NUMBER,
  out_rsv      OUT SYS_REFCURSOR) AS
BEGIN
  DECLARE
  digito NUMBER(12);
  BEGIN

IF (vnum_reserva = 0) THEN

  SELECT SUBSTR(vrut, 0,LENGTH(vrut)-2) INTO digito FROM dual;

 OPEN out_rsv FOR
  SELECT eh.num_reserva,
         eh.nombre_cli,
         eh.nombre_desp,
         eh.direccion_desp,
         eh.comuna_desp,
         t.fecha, --fecha reserva
         eh.subcanal,
         eh.type_doc,
         eh.num_doc,
         eh.monto,
         eh.num_ordenventa,
         eh.numrsv_padre,
         eh.ciudad_desp,
         eh.region_desp,
         es.desc_estadocab,
        (SELECT valor_num FROM parametros WHERE cod_param = 111) as periodo,
         eh.campo1,
         eh.campo2,
         eh.campo3,
         eh.campo4,
         eh.campo5
  FROM eventotck_hdr eh, estadostckcab es,
       (SELECT MAX(id_evento) id_evento, num_reserva, MIN(fecha) fecha
        FROM eventotck_hdr
        WHERE numrsv_padre IS NULL
          AND (ide = digito)
        GROUP BY num_reserva) t, reserva_hdr rh
  WHERE eh.tck_estadocab = es.tck_estadocab
    AND eh.id_evento = t.id_evento
    AND eh.canal_venta = rh.canal_venta
    AND eh.num_reserva = rh.num_reserva
    AND rh.cod_estadocab NOT IN (1, 2, 3)
    AND eh.numrsv_padre IS NULL
    AND (eh.ide = digito);
ELSE
 OPEN out_rsv FOR
    SELECT eh.num_reserva,
         eh.nombre_cli,
         eh.nombre_desp,
         eh.direccion_desp,
         eh.comuna_desp,
         t.fecha, --fecha reserva
         eh.subcanal,
         eh.type_doc,
         eh.num_doc,
         eh.monto,
         eh.num_ordenventa,
         eh.numrsv_padre,
         eh.ciudad_desp,
         eh.region_desp,
         es.desc_estadocab,
        (SELECT valor_num FROM parametros WHERE cod_param = 111) as periodo,
         eh.campo1,
         eh.campo2,
         eh.campo3,
         eh.campo4,
         eh.campo5
  FROM eventotck_hdr eh, estadostckcab es,
       (SELECT MAX(id_evento) id_evento, num_reserva, MIN(fecha) fecha
        FROM eventotck_hdr
        WHERE numrsv_padre IS NULL
          AND (num_reserva = vnum_reserva)
        GROUP BY num_reserva) t, reserva_hdr rh
  WHERE eh.tck_estadocab = es.tck_estadocab
    AND eh.id_evento = t.id_evento
    AND eh.canal_venta = rh.canal_venta
    AND eh.num_reserva = rh.num_reserva
    AND rh.cod_estadocab NOT IN (1, 2, 3)
    AND eh.numrsv_padre IS NULL
    AND (eh.num_reserva = vnum_reserva);
  END IF;
  END;
END TCKRESERVAHDR;

PROCEDURE TCKRESERVADTL(
  vnum_reserva    IN  eventotck_dtl.num_reserva%TYPE,
  vprd_lvl_number IN  eventotck_dtl.prd_lvl_number%TYPE,
  out_prod        OUT SYS_REFCURSOR,
  out_hist        OUT SYS_REFCURSOR) AS
BEGIN
  OPEN out_prod FOR
  /*
  SELECT ed.num_reserva,
         ed.prd_lvl_number,
         ed.prd_name_full,
         ed.cantidad,
         ed.subtotal,
         ed.rango_horario,
         MAX(ed.fecha_entrega_ori)   AS fecha_entrega_ori, --Fecha Desde - Fecha Compromiso
         MAX(ed.fecha_entrega2)      AS fecha_entrega2, --Fecha Hasta
         MAX(ed.fecha_entrega)       AS fecha_entrega,--Fecha reprogramacion
         MAX(ed.fecha)               AS fecha, --Fecha evento
         MAX(e.desc_estadodet) KEEP(DENSE_RANK LAST ORDER BY ID_EVENTO) AS desc_estadodet,
         MAX(ed.fecha_entrega_cli)   AS fecha_entrega_cli, --No se muestra en aplicacion
         MAX(ed.postventa)           AS postventa,
         MAX(ed.campo1)              AS campo1,
         MAX(ed.campo2)              AS campo2,
         MAX(ed.campo3)              AS campo3,
         MAX(ed.campo4)              AS campo4,
         MAX(ed.campo5)              AS campo5
  FROM eventotck_dtl ed, estadostckdet e
  WHERE ed.tck_estadodet = e.tck_estadodet
    AND ( (ed.num_reserva = vnum_reserva
       AND vprd_lvl_number = '0'
       AND ed.num_reserva IN (SELECT num_reserva
                              FROM eventotck_hdr
                              WHERE numrsv_padre IS NULL))
      OR (ed.num_detalle < 0 AND (ed.num_reserva, ed.prd_lvl_number) IN (SELECT DISTINCT eh.num_reserva, ed2.prd_lvl_number
                                                 FROM eventotck_hdr eh, eventotck_dtl ed2
                                                 WHERE eh.numrsv_padre IS NOT NULL
                                                   AND eh.numrsv_padre    = vnum_reserva
                                                   AND ed2.prd_lvl_number = vprd_lvl_number
                                                   AND vprd_lvl_number   != '0'))
         )
  GROUP BY ed.num_reserva,
           ed.prd_lvl_number,
           ed.prd_name_full,
           ed.cantidad,
           ed.subtotal,
           ed.rango_horario
  ORDER BY prd_lvl_number, fecha DESC;
  */
  SELECT t.num_reserva,
         t.prd_lvl_number,
         t.prd_name_full,
         t.cantidad,
         t.subtotal,
         t.glosa_rango AS rango_horario,
         MAX(t.fecha_entrega_ori) AS fecha_entrega_ori,
         MAX(t.fecha_entrega2) AS fecha_entrega2,
         MAX(t.fecha_entrega) AS fecha_entrega,
         MAX(t.fecha) AS fecha,
         MAX(t.desc_estadodet) keep(dense_rank LAST ORDER BY fecha) AS desc_estadodet,
         MAX(t.fecha_entrega_cli) AS fecha_entrega_cli,
         MAX(t.postventa) AS postventa,
         MAX(t.campo1) AS campo1,
         MAX(t.campo2) AS campo2,
         MAX(t.campo3) AS campo3,
         MAX(t.campo4) AS campo4,
         MAX(t.campo5) AS campo5
  FROM  (SELECT rd.num_reserva,
                 rd.prd_lvl_number,
                 rd.prd_name_full,
                 rd.cantidad,
                 rd.precio * rd.cantidad AS subtotal,
                 rd.glosa_rango,
                 rd.fecha_entrega_ori,
                 rd.fecha_entrega2,
                 rd.fecha_entrega,
                 e.fecha_evento AS fecha,
                 decode(rd.tipo_ped_despacho, 'PED', decode(e.cod_estado, 108, 'Recepcionado en tienda de la zona', 109, 'Recepcionado en tienda de la zona', et.desc_estadodet), et.desc_estadodet) AS desc_estadodet,
                 rd.fecha_entrega_cli,
                 decode(rd.tipo_ped_despacho, 'POS', 1, 0) AS postventa,
                 rd.campo1,
                 rd.campo2,
                 rd.campo3,
                 rd.campo4,
                 rd.campo5,
                 et.tck_estadodet,
                 lag(et.tck_estadodet, 1, NULL) over(ORDER BY rd.prd_lvl_number, e.fecha_evento) AS tck_estadodet_prev,
                 e.cod_estado,
                 e.fecha_desp_ini
            FROM evento e,
                 estadosdet ed,
                 estadostckdet et,
                 reserva_dtl rd,
                 (SELECT DISTINCT rd2.canal_venta,
                                  rd2.num_reserva,
                                  rd2.num_detalle
                    FROM reserva_dtl rd2,
                         reserva_hdr rh2
                   WHERE rd2.canal_venta = rh2.canal_venta
                     AND rd2.num_reserva = rh2.num_reserva
                     AND rh2.numrsv_padre = vnum_reserva
                     AND rd2.prd_lvl_number = vprd_lvl_number
                     AND vprd_lvl_number != '0'
                     AND rd2.num_detalle < 0
                  UNION
                  SELECT DISTINCT rd3.canal_venta,
                                  rd3.num_reserva,
                                  rd3.num_detalle
                    FROM reserva_dtl rd3
                   WHERE rd3.num_reserva = vnum_reserva
                     AND vprd_lvl_number = '0') s
           WHERE e.cod_estado = ed.cod_estadodet
             AND ed.tck_estadodet = et.tck_estadodet
             AND e.canal_venta = rd.canal_venta
             AND e.num_reserva = rd.num_reserva
             AND e.num_detalle = rd.num_detalle
             AND et.tck_estadodet != 0
             AND rd.canal_venta = s.canal_venta
             AND rd.num_reserva = s.num_reserva
             AND rd.num_detalle = s.num_detalle
           ORDER BY rd.prd_lvl_number,
                    e.fecha_evento) t
  GROUP BY  t.num_reserva,
            t.prd_lvl_number,
            t.prd_name_full,
            t.cantidad,
            t.subtotal,
            t.glosa_rango
  ORDER BY  t.prd_lvl_number,
            fecha DESC;

  OPEN out_hist FOR
  /*
  SELECT ed.num_reserva,
         ed.prd_lvl_number,
         ed.prd_name_full,
         ed.cantidad,
         ed.subtotal,
         ed.rango_horario,
         ed.fecha_entrega_ori,
         ed.fecha_entrega2,
         ed.fecha_entrega,
         ed.fecha,
         DECODE(ed.fecha_entrega, NULL, e.desc_estadodet, 'Reprogramado') AS desc_estadodet,
         ed.fecha_entrega_cli,
         ed.postventa,
         ed.campo1,
         ed.campo2,
         ed.campo3,
         ed.campo4,
         ed.campo5
  FROM eventotck_dtl ed, estadostckdet e
  WHERE ed.tck_estadodet = e.tck_estadodet
    AND ((ed.num_reserva = vnum_reserva
      AND vprd_lvl_number = '0'
      AND ed.num_reserva IN (SELECT num_reserva
                             FROM eventotck_hdr
                             WHERE numrsv_padre IS NULL))
      OR (ed.num_detalle < 0 AND (ed.num_reserva, ed.prd_lvl_number) IN (SELECT DISTINCT eh.num_reserva, ed2.prd_lvl_number
                                                 FROM eventotck_hdr eh, eventotck_dtl ed2
                                                 WHERE eh.numrsv_padre IS NOT NULL
                                                   AND eh.numrsv_padre    = vnum_reserva
                                                   AND ed2.prd_lvl_number = vprd_lvl_number
                                                   AND vprd_lvl_number   != '0'))
        )
  ORDER BY fecha desc, fecha_entrega ASC;
  */
  SELECT T.NUM_RESERVA,
       T.PRD_LVL_NUMBER,
       T.PRD_NAME_FULL,
       T.CANTIDAD,
       T.SUBTOTAL,
       T.GLOSA_RANGO AS RANGO_HORARIO,
       T.FECHA_ENTREGA_ORI,
       T.FECHA_ENTREGA2,
       T.FECHA_ENTREGA,
       T.FECHA,
       DECODE(A.RN, 1, T.DESC_ESTADODET, 2, 'Reprogramado') AS DESC_ESTADODET,
       T.FECHA_ENTREGA_CLI,
       T.POSTVENTA,
       T.CAMPO1,
       T.CAMPO2,
       T.CAMPO3,
       T.CAMPO4,
       T.CAMPO5
  FROM (SELECT RD.NUM_RESERVA,
               RD.PRD_LVL_NUMBER,
               RD.PRD_NAME_FULL,
               RD.CANTIDAD,
               RD.PRECIO * RD.CANTIDAD AS SUBTOTAL,
               RD.GLOSA_RANGO,
               RD.FECHA_ENTREGA_ORI,
               RD.FECHA_ENTREGA2,
               RD.FECHA_ENTREGA,
               E.FECHA_EVENTO AS FECHA,
               DECODE(RD.TIPO_PED_DESPACHO,
                      'PED',
                      DECODE(E.COD_ESTADO,
                             108,
                             'Recepcionado en tienda de la zona',
                             109,
                             'Recepcionado en tienda de la zona',
                             ET.DESC_ESTADODET),
                      ET.DESC_ESTADODET) AS DESC_ESTADODET,
               RD.FECHA_ENTREGA_CLI,
               DECODE(RD.TIPO_PED_DESPACHO, 'POS', 1, 0) AS POSTVENTA,
               RD.CAMPO1,
               RD.CAMPO2,
               RD.CAMPO3,
               RD.CAMPO4,
               RD.CAMPO5,
               ET.TCK_ESTADODET,
               LAG(ET.TCK_ESTADODET, 1, NULL) OVER(ORDER BY RD.PRD_LVL_NUMBER, E.FECHA_EVENTO) AS TCK_ESTADODET_PREV,
               E.COD_ESTADO,
               E.FECHA_DESP_INI
          FROM EVENTO E,
               ESTADOSDET ED,
               ESTADOSTCKDET ET,
               RESERVA_DTL RD,
               (SELECT DISTINCT RD2.CANAL_VENTA,
                                RD2.NUM_RESERVA,
                                RD2.NUM_DETALLE
                  FROM RESERVA_DTL RD2, RESERVA_HDR RH2
                 WHERE RD2.CANAL_VENTA = RH2.CANAL_VENTA
                   AND RD2.NUM_RESERVA = RH2.NUM_RESERVA
                   AND RH2.NUMRSV_PADRE = VNUM_RESERVA
                   AND RD2.PRD_LVL_NUMBER = VPRD_LVL_NUMBER
                   AND VPRD_LVL_NUMBER != '0'
                   AND RD2.NUM_DETALLE < 0
                UNION
                SELECT DISTINCT RD3.CANAL_VENTA,
                                RD3.NUM_RESERVA,
                                RD3.NUM_DETALLE
                  FROM RESERVA_DTL RD3
                 WHERE RD3.NUM_RESERVA = VNUM_RESERVA
                   AND VPRD_LVL_NUMBER = '0'

                ) S
         WHERE E.COD_ESTADO = ED.COD_ESTADODET
           AND ED.TCK_ESTADODET = ET.TCK_ESTADODET
           AND E.CANAL_VENTA = RD.CANAL_VENTA
           AND E.NUM_RESERVA = RD.NUM_RESERVA
           AND E.NUM_DETALLE = RD.NUM_DETALLE
           AND ET.TCK_ESTADODET != 0
           AND RD.CANAL_VENTA = S.CANAL_VENTA
           AND RD.NUM_RESERVA = S.NUM_RESERVA
           AND RD.NUM_DETALLE = S.NUM_DETALLE
         ORDER BY RD.PRD_LVL_NUMBER, E.FECHA_EVENTO) T
  JOIN (SELECT ROWNUM RN FROM DUAL CONNECT BY LEVEL < 3) A
    ON CASE
         WHEN T.FECHA_DESP_INI IS NOT NULL THEN
          2
         ELSE
          1
       END >= A.RN
 WHERE ((T.TCK_ESTADODET != T.TCK_ESTADODET_PREV OR A.RN = 2) OR
       TCK_ESTADODET_PREV IS NULL)
 ORDER BY T.FECHA DESC, T.FECHA_ENTREGA ASC, A.RN ASC;
END TCKRESERVADTL;

PROCEDURE ENVIAR_CAMBIOFECHA
  (
  IN_FECHA_PROCESO        IN  DATE,
  IN_AUDIT_NUMBER         IN  NUMBER DEFAULT 0,
  IN_CANT_REG              IN  NUMBER,
  IN_PROCESS_LUW          IN  VARCHAR2 DEFAULT 'T',
  "rs"                    out sys_refcursor,
  OUT_KEYMAES_INICIO      OUT VARCHAR2,
  OUT_KEYMAES_FINAL       OUT varchar2,
  OUT_CUADRAT_NUMLIN_CFECHA  OUT NUMBER,
  OUT_CUADRAT_CAMPO_CFECHA   OUT varchar2,
  OUT_CUADRAT_SUMA_CFECHA    OUT NUMBER,
  OUT_STATUS_CODE         OUT NUMBER,
  OUT_STATUS_MSG          OUT VARCHAR2

  )
AS
BEGIN
DECLARE
    var_cont_reg              NUMBER;

BEGIN
    OUT_STATUS_CODE       := 0;
    OUT_STATUS_MSG        := 'OK';
    var_cont_reg          := 1;
    IF IN_CANT_REG IS NULL THEN
       OUT_STATUS_CODE := 1;
       OUT_STATUS_MSG  := 'DAD: ' ||  'Cantidad de Registros no debe ser nulo';
       RETURN;
    ELSE
        IF IN_CANT_REG < 0 THEN
           OUT_STATUS_CODE := 1;
           OUT_STATUS_MSG  := 'DAD: ' ||  'Cantidad de Registros debe ser mayor a cero';
           RETURN;
        END IF;
    END IF;



    begin
    select max(SEQUENCIA_DAD) into OUT_KEYMAES_FINAL from WLI_STAGE_CAMBIOFECHA
    where SEQUENCIA_DAD in( select SEQUENCIA_DAD  from WLI_STAGE_CAMBIOFECHA
    where (rownum <= IN_CANT_REG or IN_CANT_REG=0)
    and sequencia_dad > IN_AUDIT_NUMBER);

         EXCEPTION
           WHEN NO_DATA_FOUND THEN
                OUT_STATUS_CODE := 0;
                OUT_STATUS_MSG  := 'DAD: ' ||  'No Existen Datos Nuevos en la Tabla WLI_STAGE_CAMBIOFECHA para la seq: '||IN_AUDIT_NUMBER;
          WHEN OTHERS THEN
                OUT_STATUS_CODE := 1;
                OUT_STATUS_MSG  := 'DAD: ' ||  SQLERRM;
                RETURN;
    END;

    BEGIN
     select sum(NUM_RESERVA) ,count(sequencia_dad),'num_reserva'
     into OUT_CUADRAT_SUMA_CFECHA , OUT_CUADRAT_NUMLIN_CFECHA,OUT_CUADRAT_CAMPO_CFECHA from WLI_STAGE_CAMBIOFECHA
    where (sequencia_dad <= OUT_KEYMAES_FINAL)
    and sequencia_dad > IN_AUDIT_NUMBER group by 'num_reserva' ;
             EXCEPTION
           WHEN NO_DATA_FOUND THEN
                OUT_STATUS_CODE := 0;
                 OUT_STATUS_MSG  := 'DAD: ' ||  'No Existen Datos Nuevos en la Tabla WLI_STAGE_CAMBIOFECHA para la seq: '||IN_AUDIT_NUMBER;
            WHEN OTHERS THEN
                OUT_STATUS_CODE := 1;
                OUT_STATUS_MSG  :='DAD: ' ||   SQLERRM;
                RETURN;
    END;


     IF OUT_KEYMAES_FINAL IS NOT NULL THEN
    BEGIN

      INSERT INTO WLI_EVENT_ITFTRAMO_DAD
                (
                  SEQUENCIA_DAD,
                  TIPO_EVENTO,
                  FECHA_AUD_FINAL,
                  ESTADO_DAD,
                  FECHA_CREACION
                )
         VALUES (
                  OUT_KEYMAES_FINAL,
                  'ENVIOFECHA',
                  SYSDATE,--var_fecha_audit_final, -- Fecha fin de tramo (en tabla auditoria)
                  'PU',
                  SYSDATE
                );
      EXCEPTION
           WHEN DUP_VAL_ON_INDEX THEN
                OUT_STATUS_CODE := 1;
                OUT_STATUS_MSG  := SQLERRM;
                RETURN;
           WHEN OTHERS THEN
                OUT_STATUS_CODE := 1;
                OUT_STATUS_MSG  := SQLERRM;
                RETURN;
    END;
    END IF;

    OPEN  "rs" for SELECT DISTINCT
    "CANAL_VENTA" ,
    "NUM_RESERVA" ,
    "NUM_DETALLE"   ,
    "PRD_LVL_NUMBER" ,
    "NUM_ORDENVENTA",
    "SUB_ORDEN"     ,
    "FECHA_DESPACHO" ,
    "FECHA_ENTREGA" ,
    "FECHA"         ,
    "CAMPO1"        ,
    "CAMPO2"        ,
    "CAMPO3"        ,
    "CAMPO4"        ,
    "CAMPO5"
    FROM WLI_STAGE_CAMBIOFECHA
    WHERE (sequencia_dad <= OUT_KEYMAES_FINAL)
    and sequencia_dad > IN_AUDIT_NUMBER;

    IF in_process_luw = 'T' THEN
       COMMIT ;
    END IF;

  end;
END ENVIAR_CAMBIOFECHA;

PROCEDURE ENVIAR_CAMION(
  in_fecha_proceso     IN  DATE,
  in_audit_number      IN  NUMBER DEFAULT 0,
  in_cant_reg          IN  NUMBER,
  in_process_luw       IN  VARCHAR2 DEFAULT 'T',
  rs                   OUT SYS_REFCURSOR,
  out_cuadrat_numlin   OUT NUMBER,
  out_cuadrat_campo    OUT VARCHAR2,
  out_cuadrat_suma     OUT NUMBER,
  out_status_code      OUT NUMBER,
  out_status_msg       OUT VARCHAR2)
AS
BEGIN
DECLARE
   out_keymaes_inicio   NUMBER;
   out_keymaes_final    NUMBER;
BEGIN
   out_status_code := 0;
   out_status_msg := 'OK';


   IF in_cant_reg IS NULL
   THEN
      out_status_code := 1;
      out_status_msg :=
         'DAD: ' || 'Cantidad de Registros no debe ser nulo';
      RETURN;
   ELSE
      IF in_cant_reg < 0
      THEN
         out_status_code := 1;
         out_status_msg :=
            'DAD: ' || 'Cantidad de Registros debe ser mayor a cero';
         RETURN;
      END IF;
   END IF;

   BEGIN
      SELECT MAX (ID)
      INTO out_keymaes_final
      FROM wli_stage_camion
       WHERE id = in_audit_number/*ID in ( select ID  from wli_stage_camion
                                 where
                                 (rownum <= in_cant_reg or in_cant_reg=0)
                                 and id > in_audit_number)*/
                                 ;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         out_status_code := 0;
         out_status_msg :=
               'DAD: '
            || 'No Existen Datos Nuevos en la Tabla WLI_STAGE_CAMION para la seq: '
            || IN_AUDIT_NUMBER;
      WHEN OTHERS
      THEN
         out_status_code := 1;
         out_status_msg := 'DAD: ' || SQLERRM;
         RETURN;
   END;

   BEGIN
        SELECT SUM (centro_costo), COUNT (id), 'centro_costo'
          INTO out_cuadrat_suma, out_cuadrat_numlin, out_cuadrat_campo
          FROM wli_stage_camion
         WHERE id = in_audit_number
      /* id <= out_keymaes_final AND id > in_audit_number */
      GROUP BY 'centro_costo';
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         out_status_code := 0;
         out_status_msg :=
               'DAD: '
            || 'No Existen Datos Nuevos en la Tabla WLI_STAGE_CAMION para la seq: '
            || IN_AUDIT_NUMBER;
      WHEN OTHERS
      THEN
         out_status_code := 1;
         out_status_msg := 'DAD: ' || SQLERRM;
         RETURN;
   END;
 /*
   BEGIN
      INSERT INTO wli_event_itftramo_dad(
                  sequencia_dad,
                  tipo_evento,
                  fecha_aud_final,
                  estado_dad,
                  fecha_creacion)
         VALUES(out_keymaes_final,
                'CAMIONASIG',
                SYSDATE,--var_fecha_audit_final, -- Fecha fin de tramo (en tabla auditoria)
                'PU',
                SYSDATE)
      ;
      EXCEPTION
           WHEN DUP_VAL_ON_INDEX THEN
                out_status_code := 1;
                out_status_msg  := SQLERRM;
                RETURN;
           WHEN OTHERS THEN
                out_status_code := 1;
                out_status_msg  := SQLERRM;
                RETURN;
    END;
    */
    OPEN rs FOR
    SELECT patente, centro_costo, accion
    FROM wli_stage_camion
    WHERE id = in_audit_number/* id <= out_keymaes_final
                             AND id > in_audit_number*/
    ;

   IF in_process_luw = 'T' THEN
     COMMIT;
   END IF;
END;
END ENVIAR_CAMION;

PROCEDURE ENVIAR_DESP_RETRASADOS(
  IN_FECHA_DESPACHO        IN  DATE,
  IN_FECHA_PROCESO         IN  DATE,
  IN_AUDIT_NUMBER          IN  NUMBER DEFAULT 0,
  IN_CANT_REG              IN  NUMBER,
  IN_PROCESS_LUW           IN  VARCHAR2 DEFAULT 'T',
  "rs"                     OUT SYS_REFCURSOR,
  OUT_KEYMAES_INICIO       OUT VARCHAR2,
  OUT_KEYMAES_FINAL        OUT VARCHAR2,
  OUT_CUADRAT_NUMLIN       OUT NUMBER,
  OUT_CUADRAT_CAMPO        OUT VARCHAR2,
  OUT_CUADRAT_SUMA         OUT NUMBER,
  OUT_STATUS_CODE          OUT NUMBER,
  OUT_STATUS_MSG           OUT VARCHAR2)
AS
BEGIN
DECLARE
   var_cont_reg   NUMBER;

BEGIN
   OUT_STATUS_CODE  := 0;
   OUT_STATUS_MSG   := 'OK';
   var_cont_reg     := 1;

   IF IN_CANT_REG IS NULL
   THEN
      OUT_STATUS_CODE := 1;
      OUT_STATUS_MSG :=
         'DAD: ' || 'Cantidad de Registros no debe ser nulo';
      RETURN;
   ELSE
      IF IN_CANT_REG < 0
      THEN
         OUT_STATUS_CODE := 1;
         OUT_STATUS_MSG :=
            'DAD: ' || 'Cantidad de Registros debe ser mayor a cero';
         RETURN;
      END IF;
   END IF;

   BEGIN
      SELECT MAX (ID)
        INTO OUT_KEYMAES_FINAL
        FROM WLI_STAGE_DESPACHOS_RETRASADOS
       WHERE ID IN
                (SELECT ID
                   FROM WLI_STAGE_DESPACHOS_RETRASADOS
                  WHERE     (ROWNUM <= IN_CANT_REG OR IN_CANT_REG = 0)
                        AND ID > IN_AUDIT_NUMBER);
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         OUT_STATUS_CODE := 0;
         OUT_STATUS_MSG :=
               'DAD: '
            || 'No Existen Datos Nuevos en la Tabla WLI_STAGE_DESPACHOS_RETRASADOS para la seq: '
            || IN_AUDIT_NUMBER;
      WHEN OTHERS
      THEN
         OUT_STATUS_CODE := 1;
         OUT_STATUS_MSG := 'DAD: ' || SQLERRM;
         RETURN;
   END;

   BEGIN
      SELECT SUM (CC_DESPACHA), COUNT (ID), 'cc_despacha'
        INTO OUT_CUADRAT_SUMA,
             OUT_CUADRAT_NUMLIN,
             OUT_CUADRAT_CAMPO
        FROM WLI_STAGE_DESPACHOS_RETRASADOS
       WHERE (ID <= OUT_KEYMAES_FINAL) AND ID > IN_AUDIT_NUMBER
       AND ENVIADO = 0;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         OUT_STATUS_CODE := 0;
         OUT_STATUS_MSG :=
               'DAD: '
            || 'No Existen Datos Nuevos en la Tabla WLI_STAGE_DESPACHOS_RETRASADOS para la seq: '
            || IN_AUDIT_NUMBER;
      WHEN OTHERS
      THEN
         OUT_STATUS_CODE := 1;
         OUT_STATUS_MSG := 'DAD: ' || SQLERRM;
         RETURN;
   END;

   IF OUT_KEYMAES_FINAL IS NOT NULL
   THEN
      BEGIN
         INSERT INTO WLI_EVENT_ITFTRAMO_DAD (SEQUENCIA_DAD,
                                             TIPO_EVENTO,
                                             FECHA_AUD_FINAL,
                                             ESTADO_DAD,
                                             FECHA_CREACION)
              VALUES (OUT_KEYMAES_FINAL,
                      'DESPRETRAS',
                      SYSDATE, --var_fecha_audit_final, -- Fecha fin de tramo (en tabla auditoria)
                      'PU',
                      SYSDATE);
      EXCEPTION
         WHEN DUP_VAL_ON_INDEX
         THEN
            OUT_STATUS_CODE := 1;
            OUT_STATUS_MSG :=
                  'DAD: '
               || 'El Correlativo ya fue procesado valide la tabla WLI_EVENT_ITFTRAMO_DAD';
            RETURN;
         WHEN COLLECTION_IS_NULL
         THEN
            OUT_STATUS_CODE := 1;
            OUT_STATUS_MSG := 'DAD: ' || 'No hay Datos a Procesar';
            RETURN;
         WHEN OTHERS
         THEN
            OUT_STATUS_CODE := 1;
            OUT_STATUS_MSG := 'DAD: ' || SQLERRM;
            RETURN;
      END;
   END IF;

   OPEN "rs" FOR
      SELECT TIPO_DESPACHO,
             NUM_RESERVA,
             RUT,
             NOMBRE_CLI,
             CC_DESPACHA,
             CC_ORIGEN, -- cc vende AGREGADO
             CC_RECIBE,
             FECHA_PAGO,
             FECHA_DESPACHO,
             COD_ESTADOCAB,
             NUM_ORDENVENTA,
             NUM_DOC,
             DESCRIPCION_CC,
             ES_BODEGA
        FROM WLI_STAGE_DESPACHOS_RETRASADOS
       WHERE     ID <= OUT_KEYMAES_FINAL
             AND ID > IN_AUDIT_NUMBER
             AND ENVIADO = 0;

   UPDATE WLI_STAGE_DESPACHOS_RETRASADOS
      SET ENVIADO = 1
    WHERE ID <= OUT_KEYMAES_FINAL AND ID > IN_AUDIT_NUMBER;

   IF IN_PROCESS_LUW = 'T'
   THEN
      COMMIT;
   END IF;
END;
END ENVIAR_DESP_RETRASADOS;

PROCEDURE ENVIAR_DESP_VALIDOS(
  IN_FECHA_DESPACHO        IN  DATE,
  IN_FECHA_PROCESO         IN  DATE,
  IN_AUDIT_NUMBER          IN  NUMBER DEFAULT 0,
  IN_CANT_REG              IN  NUMBER,
  IN_PROCESS_LUW           IN  VARCHAR2 DEFAULT 'T',
  "rs"                     OUT SYS_REFCURSOR,
  OUT_KEYMAES_INICIO       OUT VARCHAR2,
  OUT_KEYMAES_FINAL        OUT VARCHAR2,
  OUT_CUADRAT_NUMLIN       OUT NUMBER,
  OUT_CUADRAT_CAMPO        OUT VARCHAR2,
  OUT_CUADRAT_SUMA         OUT NUMBER,
  OUT_STATUS_CODE          OUT NUMBER,
  OUT_STATUS_MSG           OUT VARCHAR2)
AS
BEGIN
DECLARE
   var_cont_reg   NUMBER;
BEGIN
   OUT_STATUS_CODE := 0;
   OUT_STATUS_MSG := 'OK';
   var_cont_reg := 1;

   IF IN_CANT_REG IS NULL
   THEN
      OUT_STATUS_CODE := 1;
      OUT_STATUS_MSG :=
         'DAD: ' || 'Cantidad de Registros no debe ser nulo';
      RETURN;
   ELSE
      IF IN_CANT_REG < 0
      THEN
         OUT_STATUS_CODE := 1;
         OUT_STATUS_MSG :=
            'DAD: ' || 'Cantidad de Registros debe ser mayor a cero';
         RETURN;
      END IF;
   END IF;

   BEGIN
      SELECT MAX (ID)
        INTO OUT_KEYMAES_FINAL
        FROM WLI_STAGE_DESPACHOS_VALIDOS
       WHERE ID IN
                (SELECT ID
                   FROM WLI_STAGE_DESPACHOS_VALIDOS
                  WHERE     (ROWNUM <= IN_CANT_REG OR IN_CANT_REG = 0)
                        AND ID > IN_AUDIT_NUMBER);
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         OUT_STATUS_CODE := 0;
         OUT_STATUS_MSG :=
               'DAD: '
            || 'No Existen Datos Nuevos en la Tabla WLI_STAGE_DESPACHOS_VALIDOS para la seq: '
            || IN_AUDIT_NUMBER;
      WHEN OTHERS
      THEN
         OUT_STATUS_CODE := 1;
         OUT_STATUS_MSG := 'DAD: ' || SQLERRM;
         RETURN;
   END;

   BEGIN
      SELECT SUM (CC_DESPACHA), COUNT (ID), 'cc_despacha'
        INTO OUT_CUADRAT_SUMA,
             OUT_CUADRAT_NUMLIN,
             OUT_CUADRAT_CAMPO
        FROM WLI_STAGE_DESPACHOS_VALIDOS
       WHERE (ID <= OUT_KEYMAES_FINAL) AND ID > IN_AUDIT_NUMBER
       AND ENVIADO = 0;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         OUT_STATUS_CODE := 0;
         OUT_STATUS_MSG :=
               'DAD: '
            || 'No Existen Datos Nuevos en la Tabla WLI_STAGE_DESPACHOS_VALIDOS para la seq: '
            || IN_AUDIT_NUMBER;
      WHEN OTHERS
      THEN
         OUT_STATUS_CODE := 1;
         OUT_STATUS_MSG := 'DAD: ' || SQLERRM;
         RETURN;
   END;

   IF OUT_KEYMAES_FINAL IS NOT NULL
   THEN
      BEGIN
         INSERT INTO WLI_EVENT_ITFTRAMO_DAD (SEQUENCIA_DAD,
                                             TIPO_EVENTO,
                                             FECHA_AUD_FINAL,
                                             ESTADO_DAD,
                                             FECHA_CREACION)
              VALUES (OUT_KEYMAES_FINAL,
                      'DESPVALIDO',
                      SYSDATE, --var_fecha_audit_final, -- Fecha fin de tramo (en tabla auditoria)
                      'PU',
                      SYSDATE);
      EXCEPTION
         WHEN DUP_VAL_ON_INDEX
         THEN
            OUT_STATUS_CODE := 1;
            OUT_STATUS_MSG :=
                  'DAD: '
               || 'El Correlativo ya fue procesado valide la tabla WLI_EVENT_ITFTRAMO_DAD';
            RETURN;
         WHEN COLLECTION_IS_NULL
         THEN
            OUT_STATUS_CODE := 1;
            OUT_STATUS_MSG := 'DAD: ' || 'No hay Datos a Procesar';
            RETURN;
         WHEN OTHERS
         THEN
            OUT_STATUS_CODE := 1;
            OUT_STATUS_MSG := 'DAD: ' || SQLERRM;
            RETURN;
      END;
   END IF;

   OPEN "rs" FOR
      SELECT TIPO_DESPACHO,
             NUM_RESERVA,
             RUT_CLIENTE,
             NOMBRE_DESP,
             CC_DESPACHA,
             FECHA_PAGO,
             FECHA_DESPACHO,
             COD_ESTADOCAB,
             DESCRIPCION_CC,
             ES_BODEGA,
             FONO_DESP,
             NUM_ORDENVENTA
        FROM WLI_STAGE_DESPACHOS_VALIDOS
       WHERE     ID <= OUT_KEYMAES_FINAL
             AND ID > IN_AUDIT_NUMBER
             AND ENVIADO = 0;

   UPDATE WLI_STAGE_DESPACHOS_VALIDOS
      SET ENVIADO = 1,
          fecha_envio_sms = SYSDATE
    WHERE ID <= OUT_KEYMAES_FINAL AND ID > IN_AUDIT_NUMBER;

   IF IN_PROCESS_LUW = 'T'
   THEN
      COMMIT;
   END IF;
END;
END ENVIAR_DESP_VALIDOS;

PROCEDURE PRC_UBICACIONESGEO(
  out_status_code  OUT NUMBER,
  out_status_msg   OUT VARCHAR2)
AS
  fecha DATE := SYSDATE;
BEGIN
  out_status_code := 0;
  out_status_msg  := 'OK';

  BEGIN
    DELETE FROM stg_out_ubicacionesgeo;

    INSERT INTO stg_out_ubicacionesgeo
    SELECT fecha,
           c_divgeo,
           c_tipdg,
           adivgeo_ori,
           c_refdivgeo,
           DECODE(c_tipdg, 4, cobertura, NULL) cobertura,
           'T'
    FROM (
        SELECT u.c_divgeo,
               1 c_tipdg,
               u.adivgeo_ori,
               u.c_refdivgeo,
               CASE WHEN u.c_divgeo = to_char(p.valor_num) THEN 1 ELSE 0 END cobertura
        FROM ubicaciones_geo u, parametros p
        WHERE cod_param = 0
          AND u.c_tipdg = 'P'
        UNION
        SELECT DISTINCT
               c_divgeo,
               DECODE(c_tipdg, 'R', 2, 'CI', 3, 'CO', 4, 5) c_tipdg,
               adivgeo_ori,
               c_refdivgeo,
               MAX(NVL(cob.cobertura, 0)) cobertura
        FROM   ubicaciones_geo u,
               (SELECT DISTINCT
                       DECODE(cco.ccosto, NULL, 0, 1) cobertura,
                       com.c_tipdg    com_ctipdg, com.c_divgeo    com_cdivgeo, com.a_divgeo       comuna,
                       ciu.c_tipdg    ciu_ctipdg, ciu.c_divgeo    ciu_cdivgeo, ciu.a_divgeo       ciudad,
                       ciu.c_reftipdg reg_ctipdg, ciu.c_refdivgeo reg_cdivgeo, ciu.arefdivgeo_ori region
                FROM ubicaciones_geo com,
                     ubicaciones_geo ciu,
                     cc_comuna cco
                WHERE com.a_refdivgeo = ciu.a_divgeo AND com.c_reftipdg = ciu.c_tipdg
                  AND com.c_tipdg    = 'CO'
                  AND ciu.c_tipdg    = 'CI'
                  AND ciu.c_reftipdg = 'R'
                  AND com.adivgeo_ori = cco.comuna (+)
                  AND com.c_tipdg     = cco.comuna_tipo (+)
                  AND com.a_divgeo != 'COIHAIQUE'
                  AND ciu.a_divgeo != 'COIHAIQUE'
               ) cob
        WHERE (  u.c_tipdg    = 'CO' AND u.c_tipdg    = cob.com_ctipdg AND u.c_divgeo    = cob.com_cdivgeo AND u.a_divgeo    = cob.comuna
             AND u.c_reftipdg = 'CI' AND u.c_reftipdg = cob.ciu_ctipdg AND u.c_refdivgeo = cob.ciu_cdivgeo AND u.a_refdivgeo = cob.ciudad)
          OR  (  u.c_tipdg    = 'CI' AND u.c_tipdg    = cob.ciu_ctipdg AND u.c_divgeo    = cob.ciu_cdivgeo AND u.a_divgeo    = cob.ciudad
             AND u.c_reftipdg = 'R'  AND u.c_reftipdg = cob.reg_ctipdg AND u.c_refdivgeo = cob.reg_cdivgeo AND u.a_refdivgeo = cob.region)
          OR  (  u.c_tipdg    = 'R'  AND u.c_tipdg    = cob.reg_ctipdg AND u.c_divgeo    = cob.reg_cdivgeo AND u.a_divgeo    = cob.region)
        GROUP BY c_divgeo, c_negocio, c_tipdg, a_divgeo, a_refdivgeo, c_refdivgeo, c_reftipdg, cod_iata, adivgeo_ori, arefdivgeo_ori
    )
--    ORDER BY DECODE(c_tipdg, 'P', 1, 'R', 2, 'CI', 3, 'CO', 4, 5), to_number(c_divgeo), a_divgeo
    ORDER BY c_tipdg, to_number(c_divgeo), adivgeo_ori
    ;

  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      out_status_code := 0;
      out_status_msg := 'DAD: No existen datos para el servicio.';
      RETURN;
    WHEN OTHERS THEN
      out_status_code := 1;
      out_status_msg  := 'DAD: ' || SQLERRM;
      RETURN;
  END;
  COMMIT;

END PRC_UBICACIONESGEO;

PROCEDURE PRC_COBERTURACC(
  out_status_code  OUT NUMBER,
  out_status_msg   OUT VARCHAR2)
AS
  fecha    DATE   := SYSDATE;
BEGIN
  out_status_code := 0;
  out_status_msg  := 'OK';

  BEGIN
    DELETE FROM stg_out_coberturacc;

    INSERT INTO stg_out_coberturacc
    SELECT fecha,
       '1' tipo_loc,
       (SELECT valor_num FROM parametros WHERE cod_param = 0) cod_divgeo,
       SUM(qry.tipo_serv) AS id_grupo,
       SUM(qry.tipo_serv) AS tipo_serv,
       qry.ccosto ccosto
       FROM (
        -- Proveedores con capacidad de despacho
        SELECT 1 tipo_serv,
               to_number(TRIM('10000' || pr.cod_prov)) ccosto
          FROM prov_comuna pc,
               proveedor   pr
         WHERE pc.cod_prov(+) = pr.cod_prov
           AND (pc.comuna_tipo = 'CO'
           AND pc.tiempo_desp IS NOT NULL
           AND nvl(pc.desactivado, 0) = 0
           AND nvl(pr.desactivado, 0) = 0)
           --Proveedores que hereden servicio despacho desde bodegas
           OR (pr.cod_prov IN (SELECT pc.cod_prov
                                FROM prov_cc pc,
                                     (SELECT * FROM cc_comuna WHERE canal_venta = 23) ccom,
                                     cc_entrega ce,
                                     centro_costo cc,
                                     proveedor pr
                               WHERE pc.ccosto = cc.org_lvl_number
                                 AND pc.cod_prov = pr.cod_prov
                                 AND ccom.ccosto = cc.org_lvl_number
                                 AND ce.ccosto = cc.org_lvl_number
                                 AND ccom.canal_venta = 23
                                 AND ce.canal_venta = 23
                                 AND ccom.comuna_tipo = 'CO'
                                 AND nvl(cc.despacha,0) = 1
                                 AND nvl(cc.cant_desp,0) > 0
                                 AND (nvl(ce.dd_sala,'F') = 'T' OR nvl(ce.dd_const,'F') = 'T')
                                 AND pc.tiempo_desp IS NOT NULL
                                 AND nvl(pc.desactivado,0) = 0
                                 AND nvl(cc.desactivado,0) = 0
                                 AND nvl(pr.desactivado,0) = 0
                                 AND nvl(cc.desactivado, 0) = 0
                                 AND nvl(ccom.desactivado, 0) = 0
                               GROUP BY pc.cod_prov))
         GROUP BY pr.cod_prov
        UNION
        -- CC (Bodegas y Tiendas) con capacidad de despacho y/o retiro
        SELECT SUM(DISTINCT decode(decode(nvl(cc.despacha,0), 0, 0, nvl(decode(ccom.canal_venta, NULL, 0, cc.cant_desp), 0)), 0, 0,
               (CASE WHEN nvl(ce.dd_sala, 'F') = 'T' OR nvl(ce.dd_const, 'F') = 'T' THEN 1 ELSE 0 END))
               + decode(decode(nvl(cc.tienda_rt,0), 0, 0, nvl(cc.cant_rt, 0)), 0, 0,
               CASE WHEN nvl(ce.rt_sala, 'F') = 'T' OR nvl(ce.rt_const, 'F') = 'T' THEN 2 ELSE 0 END)) tipo_serv,
               cc.org_lvl_number ccosto
          FROM (SELECT * FROM cc_comuna WHERE canal_venta = 23) ccom,
               cc_entrega ce,
               centro_costo cc
         WHERE ccom.ccosto(+) = cc.org_lvl_number
           AND ce.ccosto = cc.org_lvl_number
           AND (ccom.canal_venta = 23 OR ccom.canal_venta IS NULL)
           AND ce.canal_venta = 23
           AND (ccom.comuna_tipo = 'CO' OR ccom.comuna_tipo IS NULL)
           AND ((nvl(cc.despacha,0) = 1 AND nvl(cc.cant_desp,0) > 0 AND (nvl(ce.dd_sala,'F') = 'T' OR nvl(ce.dd_const,'F') = 'T'))
           OR (nvl(cc.tienda_rt,0) = 1 AND nvl(cc.cant_rt,0) > 0 AND (nvl(ce.rt_sala,'F') = 'T' OR nvl(ce.rt_const,'F') = 'T')))
           AND nvl(cc.desactivado, 0) = 0
           AND nvl(ccom.desactivado, 0) = 0
         GROUP BY cc.org_lvl_number
        UNION
        -- Bodegas - Herencia de capacidad de retiro desde Tiendas
        SELECT 2 tipo_serv,
               cc.org_lvl_number ccosto
          FROM cc_tienda    ct,
               centro_costo cc
         WHERE cc.org_is_store = 'T'
           AND nvl(cc.desactivado, 0) = 0
           AND nvl(ct.desactivado, 0) = 0
           AND cc.org_lvl_number = ct.ccosto
           AND ct.tienda IN (SELECT cc.org_lvl_number
                               FROM (SELECT com.a_divgeo    comuna,
                                            ciu.a_divgeo    ciudad,
                                            reg.a_divgeo    region,
                                            ciu.adivgeo_ori ciudad_ori,
                                            com.adivgeo_ori comuna_ori
                                       FROM ubicaciones_geo com,
                                            ubicaciones_geo ciu,
                                            ubicaciones_geo reg
                                      WHERE com.a_refdivgeo = ciu.a_divgeo
                                        AND ciu.a_refdivgeo = reg.a_divgeo
                                        AND com.c_tipdg = 'CO'
                                        AND ciu.c_tipdg = 'CI'
                                        AND reg.c_tipdg = 'R'
                                        AND ciu.c_divgeo > 0
                                        AND com.c_divgeo > 0
                                        AND reg.c_divgeo > 0) comu,
                                    cc_entrega ce,
                                    centro_costo cc
                              WHERE ce.ccosto = cc.org_lvl_number
                                AND ce.canal_venta = 23
                                AND nvl(cc.org_is_store, 'F') = 'F'
                                AND (nvl(cc.tienda_rt, 0) = 1 AND
                                    nvl(cc.cant_rt,0) > 0 AND
                                    (nvl(ce.rt_sala, 'F') = 'T' OR
                                    nvl(ce.rt_const, 'F') = 'T'))
                                AND nvl(cc.desactivado, 0) = 0
                                AND comu.comuna_ori = cc.bas_addr_2
                                AND comu.ciudad_ori = TRIM(cc.bas_city)
                                AND comu.region = cc.bas_addr_3
                              GROUP BY cc.org_lvl_number)
         GROUP BY cc.org_lvl_number
        UNION
        -- Proveedores - Herencia de capacidad de Retiro desde Bodegas
        SELECT 2 tipo_serv,
               to_number(TRIM('10000' || pr.cod_prov)) ccosto
          FROM prov_cc   pc,
               proveedor pr
         WHERE pc.cod_prov = pr.cod_prov
           AND nvl(pr.desactivado, 0) = 0
           AND nvl(pc.desactivado, 0) = 0
           AND pc.tiempo_desp IS NOT NULL
           AND pc.ccosto IN
               (SELECT cc.org_lvl_number
                  FROM cc_tienda    ct,
                       centro_costo cc
                 WHERE cc.org_is_store = 'T'
                   AND nvl(cc.desactivado, 0) = 0
                   AND nvl(ct.desactivado, 0) = 0
                   AND cc.org_lvl_number = ct.ccosto
                   AND ct.tienda IN
                       (SELECT cc.org_lvl_number
                          FROM centro_costo cc,
                               (SELECT com.a_divgeo    comuna,
                                       ciu.a_divgeo    ciudad,
                                       reg.a_divgeo    region,
                                       ciu.adivgeo_ori ciudad_ori,
                                       com.adivgeo_ori comuna_ori
                                  FROM ubicaciones_geo com,
                                       ubicaciones_geo ciu,
                                       ubicaciones_geo reg
                                 WHERE com.a_refdivgeo = ciu.a_divgeo
                                   AND ciu.a_refdivgeo = reg.a_divgeo
                                   AND com.c_tipdg = 'CO'
                                   AND ciu.c_tipdg = 'CI'
                                   AND reg.c_tipdg = 'R'
                                   AND ciu.c_divgeo > 0
                                   AND com.c_divgeo > 0
                                   AND reg.c_divgeo > 0) comu
                         WHERE nvl(cc.org_is_store, 'F') = 'F'
                           AND nvl(cc.tienda_rt, 0) = 1
                           AND nvl(cc.desactivado, 0) = 0
                           AND cc.bas_addr_2 = comu.comuna_ori
                           AND TRIM(cc.bas_city) = comu.ciudad_ori
                           AND cc.bas_addr_3 = comu.region
                         GROUP BY cc.org_lvl_number)
                 GROUP BY cc.org_lvl_number)
         GROUP BY pr.cod_prov) qry
    WHERE qry.tipo_serv > 0
    GROUP BY qry.ccosto
    ORDER BY tipo_serv, ccosto;

  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      out_status_code := 0;
      out_status_msg := 'DAD: No existen datos para el servicio.';
      RETURN;
    WHEN OTHERS THEN
      out_status_code := 1;
      out_status_msg  := 'DAD: ' || SQLERRM;
      RETURN;
  END;
  COMMIT;

END PRC_COBERTURACC;

PROCEDURE PRC_CAPACIDADES
(
   out_status_code OUT NUMBER,
   out_status_msg  OUT VARCHAR2
) AS
   fecha              DATE := SYSDATE;
   desactiva          NUMBER;
   activa_purga       NUMBER;
   fecha_purga        DATE;
   hora_purga         NUMBER;
   hora_actual        NUMBER;
   flag               NUMBER;
BEGIN
   out_status_code := 0;
   out_status_msg  := 'OK';
   BEGIN

     --INI: purga de la tabla RESERVA_CAP
     BEGIN
       SELECT valor_num,
              TO_DATE(valor_str,  'DD/MM/YYYY')
         INTO activa_purga,
              fecha_purga
         FROM parametros
        WHERE cod_param = 221;

        --Hora en que se realiza la purga de la tabla
        SELECT valor_num INTO hora_purga  FROM parametros WHERE cod_param = 222;

        --Se obtiene la hora actual (respeta formato HH24)
        SELECT extract(hour FROM CAST(to_char(SYSDATE, 'DD-MON-YYYY HH24:MI:SS') AS TIMESTAMP))
          INTO hora_actual
          FROM dual;

        IF activa_purga = 1 AND fecha_purga < trunc(SYSDATE) AND hora_actual = hora_purga THEN

          DELETE FROM reserva_cap rc WHERE rc.fecha_despacho < trunc(SYSDATE);

          UPDATE parametros
             SET valor_str = to_char(trunc(SYSDATE), 'DD/MM/YYYY')
           WHERE cod_param = 221;

        END IF;
      EXCEPTION
         WHEN OTHERS THEN NULL;
      END;
     --FIN: purga de la tabla RESERVA_CAP


     --Limpia la tabla completa de salida
     DELETE FROM stg_out_capacidad_despacho;

     --Se obtiene el valor parametrico para determinar que query utilizara
     SELECT valor_num INTO desactiva FROM parametros WHERE cod_param = 77;

       --Si desactiva es 0, extrae la informacion en querys distintas
     IF (desactiva = 0) THEN

         --Inserta los registros en la tabla
         INSERT INTO stg_out_capacidad_despacho
            (centro_costo,
             dia,
             zona,
             rango,
             es_rt,
             canal,
             cantidad_rsv,
             proveedor)
            --Zona  : = NULL
            --Rango : = NR
         SELECT rc.cc_despacha AS ccosto,
                rc.fecha_despacho AS dia,
                NULL AS zona,
                'NR' AS rango,
                rc.es_rt AS tipo,
                rc.canal_venta AS canal,
                COUNT(DISTINCT rc.num_reserva) AS cant,
                '0' AS proveedor
           FROM reserva_cap rc
          WHERE rc.fecha_despacho >= trunc(SYSDATE)
          GROUP BY rc.canal_venta,
                   rc.cc_despacha,
                   rc.fecha_despacho,
                   rc.es_rt
          UNION ALL
         --Zona  : =  NULL
         --Rango : <> NR
         SELECT rc.cc_despacha,
                rc.fecha_despacho,
                NULL,
                rc.cod_rangohora,
                rc.es_rt,
                rc.canal_venta,
                COUNT(DISTINCT rc.num_reserva) AS cantidad_rsv,
                '0'
           FROM reserva_cap rc
          WHERE cod_rangohora != 'NR'
            AND rc.fecha_despacho >= trunc(SYSDATE)
          GROUP BY rc.canal_venta,
                   rc.cc_despacha,
                   rc.cod_rangohora,
                   rc.fecha_despacho,
                   rc.es_rt
         UNION ALL
         --Zona  : <> NULL
         --Rango : =  NR
         SELECT rc.cc_despacha,
                rc.fecha_despacho,
                cc.cod_zona,
                'NR',
                rc.es_rt,
                rc.canal_venta,
                COUNT(DISTINCT rc.num_reserva) AS cantidad_rsv,
                '0'
           FROM reserva_cap rc,
                cc_comuna   cc
          WHERE rc.comuna_desp = cc.comuna
            AND rc.canal_venta = cc.canal_venta
            AND rc.cc_despacha = cc.ccosto
            AND cc.cod_zona IS NOT NULL
            AND rc.fecha_despacho >= trunc(SYSDATE)
          GROUP BY rc.canal_venta,
                   cc.cod_zona,
                   rc.cc_despacha,
                   rc.fecha_despacho,
                   rc.es_rt
         UNION ALL
         --Zona  : <> NULL
         --Rango : <> NR
         SELECT rc.cc_despacha,
                rc.fecha_despacho,
                cc.cod_zona,
                rc.cod_rangohora,
                rc.es_rt,
                rc.canal_venta,
                COUNT(DISTINCT rc.num_reserva) AS cantidad_rsv,
                '0'
           FROM reserva_cap rc,
                cc_comuna   cc
          WHERE rc.comuna_desp = cc.comuna
            AND rc.canal_venta = cc.canal_venta
            AND rc.cc_despacha = cc.ccosto
            AND cc.cod_zona IS NOT NULL
            AND cod_rangohora != 'NR'
            AND rc.fecha_despacho >= trunc(SYSDATE)
          GROUP BY rc.canal_venta,
                   rc.cod_rangohora,
                   cc.cod_zona,
                   rc.cc_despacha,
                   rc.fecha_despacho,
                   rc.es_rt;

      --Si desactiva es 1, va a realizar la consulta a la tabla RESERVA_DTL y RESERVA_HDR
      ELSIF (desactiva = 1) THEN

         --Inserta los registros en la tabla
         INSERT INTO stg_out_capacidad_despacho
            (centro_costo,
             dia,
             zona,
             rango,
             es_rt,
             canal,
             cantidad_rsv,
             proveedor)
            SELECT /*+ Parallel(10) */
            DISTINCT cc.ccosto AS ccosto,
                     rd.fecha_despacho AS dia,
                     cc.cod_zona AS zona,
                     rn.cod_rangohora AS rango,
                     decode(rh.tipo_rt, 'RT', 1, 'STS', 1, 0) AS tipo,
                     cc.canal_venta AS canal,
                     COUNT(DISTINCT CASE
                              WHEN rd.canal_venta = cc.canal_venta
                                   AND rd.cod_rangohora = rn.cod_rangohora
                                   AND (rh.comuna_desp = cc.comuna OR
                                   cc.cod_zona IS NULL) THEN
                               rd.num_reserva
                              WHEN rd.canal_venta = cc.canal_venta
                                   AND rn.cod_rangohora = 'NR'
                                   AND (rh.comuna_desp = cc.comuna OR
                                   cc.cod_zona IS NULL) THEN
                               rd.num_reserva
                              ELSE
                               NULL
                           END) AS cant,
                     '0' AS proveedor
              FROM reserva_dtl rd,
                   reserva_hdr rh,
                   (SELECT DISTINCT c1.ccosto,
                                    c1.comuna,
                                    c1.cod_zona,
                                    c1.canal_venta
                      FROM cc_comuna c1
                     WHERE c1.desactivado = 0
                    UNION ALL
                    SELECT c3.org_lvl_number,
                           c3.bas_addr_2,
                           NULL,
                           cv.canal_venta
                      FROM centro_costo c3,
                           (SELECT canal_venta
                              FROM canalventa cv
                             WHERE cv.desactivado = 0) cv
                     WHERE c3.desactivado = 0
                    ) cc,
                   rango_hora rn
             WHERE rd.canal_venta = rh.canal_venta
               AND rd.num_reserva = rh.num_reserva
               AND rn.desactivado = 0
               AND rd.cc_despacha = cc.ccosto
               AND rd.num_detalle > 0
               AND rd.fecha_despacho >= trunc(SYSDATE)
               AND rd.cod_estadodet NOT IN (2, 3, 18, 23, 123)
             GROUP BY cc.ccosto,
                      rd.fecha_despacho,
                      cc.cod_zona,
                      rn.cod_rangohora,
                      decode(rh.tipo_rt, 'RT', 1, 'STS', 1, 0),
                     cc.canal_venta
            ;
      --Si desactiva es 2, va a realizar la consulta a la tabla RESERVA_CAP
      ELSIF desactiva = 2 THEN

         --Inserta los registros en la tabla
         INSERT INTO stg_out_capacidad_despacho
            (centro_costo,
             dia,
             zona,
             rango,
             es_rt,
             canal,
             cantidad_rsv,
             proveedor)

      SELECT /*+Parallel(10)*/
            DISTINCT cc.ccosto AS ccosto,
                     rd.fecha_despacho AS dia,
                     cc.cod_zona AS zona,
                     rn.cod_rangohora AS rango,
                     rd.es_rt AS es_rt,
                     cc.canal_venta AS canal,
                     COUNT(DISTINCT CASE
                              WHEN rd.canal_venta = cc.canal_venta
                                   AND rd.cod_rangohora = rn.cod_rangohora
                                   AND (rd.comuna_desp = cc.comuna OR
                                   cc.cod_zona IS NULL) THEN
                               rd.num_reserva
                              WHEN rd.canal_venta = cc.canal_venta
                                   AND rn.cod_rangohora = 'NR'
                                   AND (rd.comuna_desp = cc.comuna OR
                                   cc.cod_zona IS NULL) THEN
                               rd.num_reserva
                              ELSE
                               NULL
                           END) AS cant,
                     '0' AS proveedor
              FROM reserva_cap rd,
                   (SELECT DISTINCT c1.ccosto,
                                    c1.comuna,
                                    c1.cod_zona,
                                    c1.canal_venta
                      FROM cc_comuna c1
                     WHERE c1.desactivado = 0
                    UNION ALL
                    SELECT c3.org_lvl_number,
                           c3.bas_addr_2,
                           NULL,
                           cv.canal_venta
                      FROM centro_costo c3,
                           (SELECT canal_venta
                              FROM canalventa cv
                             WHERE cv.desactivado = 0) cv
                     WHERE c3.desactivado = 0
                    ) cc,
                   rango_hora rn
             WHERE rn.desactivado = 0
               AND rd.cc_despacha = cc.ccosto
               AND rd.fecha_despacho >= trunc(SYSDATE)
               AND rd.cod_estadodet NOT IN (2, 3, 18, 23, 123)
             GROUP BY cc.ccosto,
                      rd.fecha_despacho,
                      cc.cod_zona,
                      rn.cod_rangohora,
                      rd.es_rt,
                      cc.canal_venta
                      ;
      END IF;
      SELECT VALOR_NUM INTO flag FROM PARAMETROS WHERE COD_PARAM = 501 ;
      IF(flag = 1) THEN
        DBMS_OUTPUT.PUT_LINE('entre');
      PRC_CALCULA_TPO_PROCESO_PROV();
      END IF;
   EXCEPTION
      WHEN no_data_found THEN
         out_status_code := 0;
         out_status_msg  := 'DAD: No existen datos para el servicio.';
         RETURN;
      WHEN OTHERS THEN
         out_status_code := 1;
         out_status_msg  := 'DAD: ' || SQLERRM;
         RETURN;
   END;
   COMMIT;
END PRC_CAPACIDADES;

END DADEXPORT;


/
