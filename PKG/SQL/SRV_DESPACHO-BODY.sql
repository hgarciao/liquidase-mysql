--------------------------------------------------------
--  File created - Thursday-June-21-2018   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Package Body SRV_DESPACHO
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE BODY "DADCORPDS"."SRV_DESPACHO" AS

  PROCEDURE despachos_activos_cliente
(
    rut               IN  NUMBER,
    in_process_luw    IN  CHAR DEFAULT 'T',
    rsDespachos       OUT SYS_REFCURSOR,
    rsRetiros         OUT SYS_REFCURSOR,
    out_status_code   OUT NUMBER,
    out_status_msg    OUT VARCHAR
)
AS
  var_estados_activos VARCHAR(1000);
BEGIN
    out_status_msg:='OK';
    out_status_code:=0;


  begin
    SELECT valor_str INTO var_estados_activos FROM PARAMETROS WHERE COD_PARAM=181;
  exception
    when no_data_found then
      out_status_msg:='NO EXISTE EL PARAMETRO 181 EN LA TABLA PARAMETROS';
      out_status_code:=-1;
      RETURN;
  end;



          OPEN rsDespachos FOR
          SELECT * FROM (  SELECT
                hdr.num_reserva AS num_reserva,
                dtl.cod_estadodet AS codigo_estado,
                dtl.cod_rangohora AS codigo_rango,
                est.desc_estadodet AS descripcion_estado,
                CASE
                  WHEN NVL(hdr.numrsv_padre,0) > 0 THEN
                    1
                  ELSE
                    0
                  END AS es_post_venta,
                dtl.fecha_entrega AS fecha_entrega,
                dtl.fecha_entrega2 AS fecha_entrega2,
                NULL AS hora_entrega_estimada,
                hr.hora_ini AS hora_inicio,
                hr.hora_fin AS hora_fin,
                 CASE
              WHEN NVL(hdr.numrsv_padre,0) > 0 THEN
                1 -- Qué quieren hacer? Esto solo identifica las reservas de pos venta (nada más).
              WHEN (dtl.bloqueado = 1) THEN
                1 --Reeservas blqoueadas no se despachan
              --Estados que identifican reservas anuladas, expiradas o con NCR.
              WHEN (dtl.cod_estadodet in ( 2, --anulado
                                           3, --expirado
                                          18, --NCR
                                          23, --anulado DAD
                                          123 --anulado DAD
                   ) ) THEN 1
              WHEN (dtl.cod_estadodet not in (12, 13, 52, 112, 113) and trunc(dtl.fecha_entrega) <= trunc(sysdate) and trunc(sysdate) <= trunc (dtl.fecha_entrega2)) THEN
                 1
              ELSE
                0
            END AS llamada_call_center,
                  ROW_NUMBER() OVER  (  PARTITION BY hdr.num_reserva, dtl.fecha_entrega ORDER BY hdr.num_reserva, dtl.fecha_entrega ) AS opcion
          FROM
            reserva_dtl dtl,reserva_hdr hdr, estadosdet est, rango_hora hr
          WHERE
          1=1
            and dtl.num_reserva=hdr.num_reserva
            and dtl.canal_venta=hdr.canal_venta
            AND dtl.cod_estadodet=est.cod_estadodet
            and hdr.ide=rut
            AND dtl.cod_rangohora=hr.cod_rangohora
            and hdr.tipo_rt IS NULL
            AND dtl.fecha_despacho > sysdate-30
            AND hdr.cod_estadocab NOT IN (1, 2, 3)
            and dtl.cod_estadodet IN  (SELECT REGEXP_SUBSTR(var_estados_activos, '[^,]+', 1, LEVEL)
                  FROM DUAL   CONNECT BY REGEXP_SUBSTR(var_estados_activos, '[^,]+', 1, LEVEL) IS NOT NULL)
                 )WHERE opcion=1
                  ORDER BY fecha_entrega ASC;




          OPEN rsRetiros FOR
             SELECT * FROM ( SELECT
                hdr.num_reserva AS num_reserva,
                dtl.cod_estadodet AS codigo_estado,
                 dtl.cod_rangohora AS codigo_rango,
                 est.desc_estadodet AS descripcion_estado,
                 dtl.fecha_entrega AS fecha_entrega,
                 hr.hora_ini AS hora_inicio,
                 hr.hora_fin AS hora_fin,
                 CASE
                  WHEN NVL(hdr.numrsv_padre,0) > 0 THEN
                    1 -- Qué quieren hacer? Esto solo identifica las reservas de pos venta (nada más).
                  WHEN (dtl.bloqueado = 1) THEN
                    1 --Reeservas blqoueadas no se despachan
                  --Estados que identifican reservas anuladas, expiradas o con NCR.
                  WHEN (dtl.cod_estadodet in ( 2, --anulado
                                               3, --expirado
                                              18, --NCR
                                              23, --anulado DAD
                                              123 --anulado DAD
                       )) THEN 1
--DESARROLLO NO DEBE ESTAR EN TEST-31/08/2016
--                   WHEN (dtl.cod_estadodet not in (12, 13, 52, 112, 113) and trunc(dtl.fecha_entrega) <= trunc(sysdate) and trunc(sysdate) <= trunc (dtl.fecha_entrega2)) THEN
--                 1
                  ELSE
                    0
                END AS llamada_call_center,
                  ROW_NUMBER() OVER  (  PARTITION BY hdr.num_reserva, dtl.fecha_entrega ORDER BY hdr.num_reserva, dtl.fecha_entrega ) AS opcion
          FROM
            reserva_dtl dtl,reserva_hdr hdr, estadosdet est, rango_hora hr
          WHERE
          1=1
            and dtl.num_reserva=hdr.num_reserva
            and dtl.canal_venta=hdr.canal_venta
            AND dtl.cod_estadodet=est.cod_estadodet
            and hdr.ide=rut
            AND dtl.cod_rangohora=hr.cod_rangohora
            and hdr.tipo_rt IN ('RT', 'STS')
            AND dtl.fecha_despacho > sysdate-30
            AND hdr.cod_estadocab NOT IN (1, 2, 3)
            and dtl.cod_estadodet IN  (SELECT REGEXP_SUBSTR(var_estados_activos, '[^,]+', 1, LEVEL)
                  FROM DUAL   CONNECT BY REGEXP_SUBSTR(var_estados_activos, '[^,]+', 1, LEVEL) IS NOT NULL)
                  )WHERE opcion=1
                  ORDER BY fecha_entrega ASC;


END despachos_activos_cliente;



 PROCEDURE cobertura_consultar(
  xml               IN  XMLTYPE,
  in_process_luw    IN  CHAR DEFAULT 'T',
  out_status_code   OUT NUMBER,
  out_status_msg    OUT VARCHAR,
  rs                OUT SYS_REFCURSOR)
AS
BEGIN
DECLARE
  id_xml           NUMBER (12);
  doc              dbms_xmldom.domdocument;
  node             dbms_xmldom.domnode;
  hijo             dbms_xmldom.domnode;
  hijo2            dbms_xmldom.domnode;
  nodelist         dbms_xmldom.domnodelist;
  hijoslist        dbms_xmldom.domnodelist;
  nodoactual       dbms_xmldom.domnode;
  numnodes         NUMBER;
  numhijos         NUMBER;
  cuadrat_suma     NUMBER;
  cuadrat_numlin   NUMBER;
  pais             VARCHAR2(10);
  vartipostock     VARCHAR2(10);
  varorigencc      VARCHAR2(10);
  vardespachacc    VARCHAR2(10);
  varcanalventa    VARCHAR2(10);
  varcomuna        VARCHAR2(30);
  varcomuna2       VARCHAR2(30);
  varciudad        VARCHAR2(30);
  varregion        VARCHAR2(30);
  varcodcomuna     VARCHAR2(10);
  varcodciudad     VARCHAR2(10);
  varcodregion     VARCHAR2(10);
  varaccion        VARCHAR2(10);
  strcomuna        VARCHAR2(30);
  strciudad        VARCHAR2(30);
  strregion        VARCHAR2(30);
  vartiendas       VARCHAR2(30);
  varcodproveedor  VARCHAR2(10);
  consultasql      VARCHAR2(32000);
  seq              NUMBER;
  repetidos        NUMBER;
  CDSTGO           NUMBER;
  varTiendaRetiro  NUMBER;
  varsin_cobertura NUMBER;
  varInvalidaVATRT NUMBER := 0;

BEGIN
  repetidos       := 0;
  out_status_code := 1;

/*
VAT1: de acuerdo a la comuna entrega la tienda mas cercana para considerar sus precios, en caso de ser bodega entrega el de Internet.
VAT2: entrega todas las tiendas de RT con sus bodegas que las alimentan (solo stock S).
FLE1: consulta RT para flete, entrega los datos de las tiendas del campo ComunaDespacho, similar a VAT2.
FLE2: consulta DD para flete, gual que Consuslta Disponibilidad, excepto para "sin cobertura".
*/
  DELETE FROM tmp_lod;
  SELECT seq_lod.NEXTVAL INTO seq              FROM DUAL;
  SELECT valor_str       INTO pais             FROM parametros   WHERE cod_param = 0;
  SELECT valor_num       INTO CDSTGO           FROM parametros   WHERE cod_param = 1;
  SELECT valor_num       INTO varsin_cobertura FROM parametros   WHERE cod_param = 7;
  SELECT valor_num       INTO varInvalidaVATRT FROM parametros   WHERE cod_param = 8;

  IF (pais = 'CL') THEN
    strcomuna := 'Comuna ';
    strciudad := 'Ciudad ';
    strregion := 'Region ';
  ELSIF (pais = 'AR') THEN
    strcomuna := 'Localidad ';
    strciudad := 'Partido ';
    strregion := 'Provincia ';
  ELSIF (pais = 'PE') THEN
    strcomuna := 'Depto ';
    strciudad := 'Provincia ';
    strregion := 'Distrito ';
  ELSE
    out_status_code := 1;
    out_status_msg := 'DAD: Falta definir pais ';
    RETURN;
  END IF;

  doc  := dbms_xmldom.newdomdocument (xmltype (xml.getclobval ()));
  node := dbms_xmldom.makenode (doc);
  nodelist := xslprocessor.selectnodes (node, '/lugarOptimo', 'xmlns="http://datos.sodimac.cl/dad/lugaroptimo"');
  numnodes := dbms_xmldom.getlength (nodelist);
  cuadrat_suma   := 0;
  cuadrat_numlin := 0;

  FOR rowcounter IN 0 .. numnodes - 1
  LOOP
    nodoactual       := dbms_xmldom.item (nodelist, rowcounter);
    hijo             := xslprocessor.selectsinglenode(nodoactual, 'canalVenta');
    hijo             := xmldom.getfirstchild (hijo);
    varcanalventa    := dbms_xmldom.getnodevalue (hijo);
    hijo             := xslprocessor.selectsinglenode(nodoactual, 'codComuna');
    hijo             := xmldom.getfirstchild (hijo);
    varcodcomuna     := dbms_xmldom.getnodevalue(hijo);
    hijo             := xslprocessor.selectsinglenode(nodoactual, 'comuna');
    hijo             := xmldom.getfirstchild (hijo);
    varcomuna        := dbms_xmldom.getnodevalue(hijo);
    hijo             := xslprocessor.selectsinglenode(nodoactual, 'codCiudad');
    hijo             := xmldom.getfirstchild (hijo);
    varcodciudad     := dbms_xmldom.getnodevalue(hijo);
    hijo             := xslprocessor.selectsinglenode(nodoactual, 'ciudad');
    hijo             := xmldom.getfirstchild (hijo);
    varciudad        := dbms_xmldom.getnodevalue(hijo);
    hijo             := xslprocessor.selectsinglenode(nodoactual, 'codRegion');
    hijo             := xmldom.getfirstchild (hijo);
    varcodregion     := dbms_xmldom.getnodevalue(hijo);
    hijo             := xslprocessor.selectsinglenode(nodoactual, 'region');
    hijo             := xmldom.getfirstchild (hijo);
    varregion        := dbms_xmldom.getnodevalue(hijo);
    hijo             := xslprocessor.selectsinglenode(nodoactual, 'accion');
    hijo             := xmldom.getfirstchild (hijo);
    varaccion        := dbms_xmldom.getnodevalue(hijo);
    hijo             := xslprocessor.selectsinglenode(nodoactual, 'tiendaRetiro');
    hijo             := xmldom.getfirstchild (hijo);
    varTiendaRetiro  := dbms_xmldom.getnodevalue(hijo);

    --Si vienen codigos de ubicaciones se transforman en glosas
    BEGIN
      IF (varcodcomuna IS NOT NULL OR varcodcomuna != '') THEN
        SELECT q.a_divgeo
        INTO varcomuna
        FROM ubicaciones_geo q
        WHERE q.c_tipdg = 'CO'
        AND q.c_divgeo = varcodcomuna
        ;
      END IF;
      IF (varcodciudad IS NOT NULL OR varcodciudad != '') THEN
        SELECT q.a_divgeo
        INTO varciudad
        FROM ubicaciones_geo q
        WHERE q.c_tipdg = 'CI'
        AND q.c_divgeo = varcodciudad
        ;
      END IF;
      IF (varcodregion IS NOT NULL OR varcodregion != '') THEN
        SELECT q.a_divgeo
        INTO varregion
        FROM ubicaciones_geo q
        WHERE q.c_tipdg = 'R'
        AND q.c_divgeo = varcodregion
        ;
      END IF;

      EXCEPTION
        WHEN OTHERS THEN
          out_status_code := 1;
          out_status_msg := 'DAD: No existe'
                         || ' ' || strcomuna || ': ' || varcodcomuna
                         || ' ' || strciudad || ': ' || varcodciudad
                         || ' ' || strregion || ': ' || varcodregion;
          RETURN;
     END;

    varcomuna2 := varcomuna;
    BEGIN
      IF (varaccion = 'VAT2') THEN
        IF (varregion IS NOT NULL) THEN
           SELECT a_divgeo
            INTO varcomuna2
            FROM ubicaciones_geo
            WHERE adivgeo_ori = varregion
              AND c_tipdg     = 'R';
        END IF;

      ELSIF  (varaccion = 'FLE1') THEN
        IF (varcomuna IS NOT NULL) THEN
           vartiendas := varcomuna;
        END IF;

      ELSE
        IF (INSTR(varcomuna2, '>') = 0) THEN
          SELECT com.a_divgeo
          INTO varcomuna2
          FROM ubicaciones_geo com, ubicaciones_geo ciu
          WHERE com.arefdivgeo_ori = ciu.adivgeo_ori
            AND com.c_tipdg = 'CO'
            AND ciu.c_tipdg = 'CI'
            AND com.adivgeo_ori = varcomuna2
          ORDER BY com.a_divgeo;

        ELSE
          SELECT com.a_divgeo
          INTO varcomuna2
          FROM ubicaciones_geo com, ubicaciones_geo ciu
          WHERE com.a_refdivgeo = ciu.a_divgeo
            AND com.c_tipdg = 'CO'
            AND ciu.c_tipdg = 'CI'
            AND com.a_divgeo = varcomuna2
          ORDER BY com.a_divgeo;
        END IF;
      END IF;

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          out_status_code := 1;
          out_status_msg := 'DAD: No existe'
                         || ' ' || strcomuna || ': ' || varcomuna
                         || ' ' || strciudad || ': ' || varciudad
                         || ' ' || strregion || ': ' || varregion;
          RETURN;
        WHEN TOO_MANY_ROWS THEN --comuna/ciudad repetida
          BEGIN
            SELECT com.a_divgeo
            INTO varcomuna2
            FROM ubicaciones_geo com, ubicaciones_geo ciu
            WHERE com.arefdivgeo_ori = ciu.adivgeo_ori
              AND com.c_tipdg = 'CO'
              AND ciu.c_tipdg = 'CI'
              AND (  (com.a_divgeo    IN (SUBSTR(varcomuna2 || ' >' || varciudad, 1, 30)) --comuna repetida + ciudad unica
                  AND com.a_refdivgeo IN(varciudad)
                  AND ciu.a_refdivgeo IN (varregion))
                OR   (com.a_divgeo    IN (varcomuna2) --comuna unica + ciudad repetida
                  AND com.a_refdivgeo IN(SUBSTR(varciudad || ' >' || varregion, 1, 30))
                  AND ciu.a_refdivgeo IN (varregion))
                OR   (com.a_divgeo    IN (SUBSTR(varcomuna2 || ' >' || SUBSTR(varciudad || ' >' || varregion, 1, 30), 1, 30)) --comuna repetida + ciudad repetida
                  AND com.a_refdivgeo IN(SUBSTR(varciudad || ' >' || varregion, 1, 30))
                  AND ciu.a_refdivgeo IN (varregion)))
            ;
          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              out_status_code := 1;
              out_status_msg := 'DAD: No existe '
                         || ' ' || strcomuna || ': ' || varcomuna
                         || ' ' || strciudad || ': ' || varciudad
                         || ' ' || strregion || ': ' || varregion;
              RETURN;
          END;
    END;

    hijoslist     := xslprocessor.selectnodes(nodoactual, 'stock');
    numhijos      := dbms_xmldom.getlength (hijoslist);
    FOR rowct IN 0 .. numhijos - 1 LOOP
      out_status_code := 0;
      nodoactual      := dbms_xmldom.item (hijoslist, rowct);
      hijo2           := xslprocessor.selectsinglenode (nodoactual, 'tipoStock');
      hijo2           := xmldom.getfirstchild (hijo2);
      vartipostock    := dbms_xmldom.getnodevalue (hijo2);
      hijo2           := xslprocessor.selectsinglenode (nodoactual, 'origenCC');
      hijo2           := xmldom.getfirstchild (hijo2);
      varorigencc     := NVL(TRIM(dbms_xmldom.getnodevalue (hijo2)), 0);
      hijo2           := xslprocessor.selectsinglenode (nodoactual, 'despachaCC');
      hijo2           := xmldom.getfirstchild (hijo2);
      vardespachacc   := NVL(TRIM(dbms_xmldom.getnodevalue (hijo2)), 0);
      hijo2           := xslprocessor.selectsinglenode (nodoactual, 'codproveedor');
      hijo2           := xmldom.getfirstchild (hijo2);
      varcodproveedor := dbms_xmldom.getnodevalue (hijo2);

      BEGIN
        SELECT canal_venta INTO varcanalventa FROM canalventa WHERE canal_venta = varcanalventa;
        EXCEPTION
          WHEN NO_DATA_FOUND THEN
            out_status_code := 1;
            out_status_msg := 'DAD: No existe canal de venta'
                            || ' ' || strcomuna || ': ' || varcomuna
                            || ' OrigenCC: '            || varorigencc
                            || ' DespachaCC: '          || vardespachacc
                            || ' CanalVenta: '          || varcanalventa;
            RETURN;
      END;

--***** VAT1: de acuerdo a la comuna entrega la tienda mas cercana para considerar sus precios, en caso de ser bodega entrega CV 72.
      IF (varaccion = 'VAT1') THEN
        IF (vartipostock != 'S') THEN
          out_status_code := 1;
          out_status_msg := 'DAD: Criterio de consulta no valido ';
          RETURN;

        ELSE
          BEGIN
            INSERT INTO tmp_lod
            --Tienda mas cercana para utilizar sus precios
            SELECT * FROM (
              SELECT seq,
                     varcanalventa            AS "codigoCanalVenta",
                     ccom.comuna              AS "comunaDespacho",
                     NULL                     AS "regionDespacho",
                     0                        AS "codigoTransporte",
                     vartipostock             AS "tipoStock",
                     0                        AS "origenCC",
                     NVL(vardespachacc, '')   AS "despachaCC",
                     DECODE(org_is_store,
                            'F', CAST(ccd.org_lvl_number AS VARCHAR2(10)), --tienda
                            CASE --bodega
                              WHEN ccom.tiempo_desp = 0 THEN vardespachacc
                              ELSE (SELECT to_char(valor_num) FROM parametros WHERE cod_param = 14) --bodega->Internet
                            END)              AS "origenCCLod",
                     DECODE(org_is_store,
                            'F', CAST(ccd.org_lvl_number AS VARCHAR2(10)), --tienda
                            CASE --bodega
                              WHEN ccom.tiempo_desp = 0 THEN vardespachacc
                              ELSE (SELECT to_char(valor_num) FROM parametros WHERE cod_param = 14) --bodega->Internet
                            END)              AS "despachaCCLod",
                     0                        AS "viaCCLOD",
                     eccd.cod_tipocc          AS tipocc_origen,
                     eccd.cod_tipocc          AS tipocc_despacha,
                     ''                       AS tipocc_via,
                     0                        AS "tiempoProcesoOrigen",
                     0                        AS "tiempoViajeOrigen",
                     0                        AS "tiempoProcesoCCVia",
                     0                        AS "tiempoViajeCCVia",
                     ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                     ccom.tiempo_desp         AS "tiempoViajeDespacho",
                     ccom.frec_abastece       AS "frecuenciaViaje",
                     ''                       AS diasccorigen,
                     NVL(z.dias_despzona, ccd.dias_despacho) AS "diasCCDespacha",
                     ccd.dias_despacho        AS diasccvia,
                     ccd.cant_desp            AS "cantidadDespachada",
                     0                        AS "prioridad",
                     ''                       AS codproveedor,
                     0                        AS tpo_prov_proceso,
                     0                        AS tpo_prov_viaje,
                     0                        AS dias_prov_despacha,
                     0                        AS cap_prov_diaria,
                     ccd.recibe_psala         AS cc_origen_tipo_sala,
                     ccd.recibe_pconst        AS cc_origen_tipo_const,
                     ''                       AS cc_despacha_tipo_sala,
                     ''                       AS cc_despacha_tipo_const,
                     ''                       AS cc_via_tipo_sala,
                     ''                       AS cc_via_tipo_const,
                     ccd.express              AS serv_express,
                     ccd.cant_express         AS cap_desp_express,
                     ccd.hora_corte           AS hora_corte,
                     ''                       AS ciudadDespacho,
                     ''                       AS regionlod,
                     ''                       AS ciudadlod,
                     ''                       AS comunalod,
                     ''                       AS nombreTienda,
                     ''                       AS direccionTienda,
                     0                        AS tienda_rt,
                     0                        AS cant_rt,
                     ''                       AS dias_rt,
                     ''                       AS hora_corte_rt,
                     ccd.cant_desp1           AS cant_desp1,
                     ccd.cant_desp2           AS cant_desp2,
                     ccd.cant_desp3           AS cant_desp3,
                     ccd.cant_desp4           AS cant_desp4,
                     ccd.cant_desp5           AS cant_desp5,
                     ccd.cant_desp6           AS cant_desp6,
                     ccd.cant_desp7           AS cant_desp7,
                     ccd.cant_rt1             AS cant_rt1,
                     ccd.cant_rt2             AS cant_rt2,
                     ccd.cant_rt3             AS cant_rt3,
                     ccd.cant_rt4             AS cant_rt4,
                     ccd.cant_rt5             AS cant_rt5,
                     ccd.cant_rt6             AS cant_rt6,
                     ccd.cant_rt7             AS cant_rt7,
                     0                        AS cobertura
              FROM cc_comuna ccom,
                   centro_costo ccd,
                   zona z,
                   cc_entrega eccd
              WHERE ccom.comuna              = varcomuna2
                AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
                AND eccd.ccosto              = ccd.org_lvl_number
                AND eccd.canal_venta         = varcanalventa
                AND (ccom.ccosto             = vardespachacc OR   eccd.cod_tipocc IN ('H', 'HI'))--H(I) -> cc_origen (!)= cc_despacha
                AND ccom.ccosto              = ccd.org_lvl_number
                AND ccom.ccosto              = z.ccosto (+)
                AND ccom.cod_zona            = z.cod_zona (+)
                AND NVL(ccd.desactivado, 0)  = 0
                AND NVL(ccom.desactivado, 0) = 0
                AND ccom.tiempo_desp IS NOT NULL
                AND ccom.ccosto IN (SELECT org_lvl_number FROM (SELECT cc.org_lvl_number
                                    FROM cc_comuna ccom, centro_costo cc,cc_entrega ecc
                                    WHERE ccom.comuna              = varcomuna2
                                      AND ecc.ccosto               = cc.org_lvl_number
                                      AND ecc.canal_venta          = varcanalventa
                                      AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
                                      AND ccom.ccosto              = cc.org_lvl_number
                                      AND NVL(cc.desactivado, 0)   = 0
                                      AND NVL(ccom.desactivado, 0) = 0
                                      AND ccom.tiempo_desp IS NOT NULL
                                      AND (cc.org_lvl_number = vardespachacc OR ecc.cod_tipocc  IN ('H', 'HI'))
                                    ORDER BY CASE
                                               WHEN cc.org_is_store = 'T' AND cc.org_lvl_number = CDSTGO THEN -1000000
                                               WHEN cc.org_is_store = 'T' THEN -cc.cant_desp
                                               ELSE 0
                                             END,
                                             CASE
                                               WHEN cc.org_lvl_number = vardespachacc THEN -1
                                               ELSE 0
                                             END,
                                             CASE
                                               WHEN ecc.cod_tipocc  = 'HI' THEN -cc.cant_desp*1000
                                               WHEN ecc.cod_tipocc   = 'H'  THEN -cc.cant_desp
                                               ELSE 0
                                             END))
              ORDER BY
                   DECODE(ccd.org_is_store, 'F', 0, 1),
                   ccom.tiempo_desp,
                   CASE
                     WHEN ccom.ccosto = vardespachacc THEN 0
                     ELSE 1
                   END,
                   CASE
                     WHEN "TIPOCC_DESPACHA" = 'HI' THEN -ccd.cant_desp*1000
                     WHEN "TIPOCC_DESPACHA" = 'H'  THEN -ccd.cant_desp
                     ELSE 0
                   END
            ) WHERE rownum = 1 --Se entrega sólo 1 opcion
            ;
            IF SQL%ROWCOUNT = 0 THEN
              out_status_code := 1;
              out_status_msg  := 'DAD: No hay datos para estos registros'
                            || ' Accion: '            || varaccion
                            || ' ' || strcomuna || ': ' || varcomuna
                            || ' DespachaCC: '          || vardespachacc
                            || ' CanalVenta: '          || varcanalventa;
              RETURN;
            END IF;
          EXCEPTION
            WHEN DUP_VAL_ON_INDEX THEN
              NULL;
            WHEN OTHERS THEN
              out_status_code := 1;
              out_status_msg  := SQLERRM;
              RETURN;
          END;
        END IF;

--***** VAT2: solicita las tiendas RT con sus bodegas que las alimentan (solo stock S).
      ELSIF (varaccion = 'VAT2') THEN
        IF (vartipostock IN ('S')) THEN
          IF varInvalidaVATRT = 0 THEN --Se realiza para evitar que VAT haga RT debido a activación de Internet RT
          BEGIN
            INSERT INTO tmp_lod
            --Tiendas RT
            SELECT DISTINCT seq,
                   varcanalventa            AS "codigoCanalVenta",
                   ccd.bas_addr_2           AS "comunaDespacho",
                   varregion                AS "regionDespacho",
                   0                        AS "codigoTransporte",
                   vartipostock             AS "tipoStock",
                   ''                       AS "origenCC",
                   NVL(vardespachacc, '')   AS "despachaCC",
                   ccd.org_lvl_number       AS "origenCCLod",
                   ccd.org_lvl_number       AS "despachaCCLod",
                   0                        AS "viaCCLod",
                   eccd.cod_tipocc          AS tipocc_origen,
                   eccd.cod_tipocc          AS tipocc_despacha,
                   ''                       AS tipocc_via,
                   0                        AS "tiempoProcesoOrigen",
                   0                        AS "tiempoViajeOrigen",
                   0                        AS "tiempoProcesoCCVia",
                   0                        AS "tiempoViajeCCVia",
                   ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                   ''                       AS "tiempoViajeDespacho",
                   ''                       AS "frecuenciaViaje",
                   ''                       AS diasccorigen,
                   ccd.dias_despacho        AS "diasCCDespacha",
                   ''                       AS diasccvia,
                   ccd.cant_desp            AS "cantidadDespachada",
                   0                        AS "prioridad",
                   ''                       AS codproveedor,
                   0                        AS tpo_prov_proceso,
                   0                        AS tpo_prov_viaje,
                   ''                       AS dias_prov_despacha,
                   0                        AS cap_prov_diaria,
                   ''                       AS cc_origen_tipo_sala,
                   ''                       AS cc_origen_tipo_const,
                   ccd.recibe_psala         AS cc_despacha_tipo_sala,
                   ccd.recibe_pconst        AS cc_despacha_tipo_const,
                   ''                       AS cc_via_tipo_sala,
                   ''                       AS cc_via_tipo_const,
                   ccd.express              AS serv_express,
                   ccd.cant_express         AS cap_desp_express,
                   ccd.hora_corte           AS hora_corte,
                   ccr.ciudad               AS ciudadDespacho,
                   ccr.region               AS regionlod,
                   ccr.ciudad               AS ciudadlod,
                   ccr.comuna               AS comunalod,
                   ccd.org_name_full        AS nombreTienda,
                   ccd.bas_addr_1           AS direccionTienda,
                   ccd.tienda_rt            AS tienda_rt,
                   ccd.cant_rt              AS cant_rt,
                   ccd.dias_rt              AS dias_rt,
                   ccd.hora_corte_rt        AS hora_corte_rt,
                   ccd.cant_desp1           AS cant_desp1,
                   ccd.cant_desp2           AS cant_desp2,
                   ccd.cant_desp3           AS cant_desp3,
                   ccd.cant_desp4           AS cant_desp4,
                   ccd.cant_desp5           AS cant_desp5,
                   ccd.cant_desp6           AS cant_desp6,
                   ccd.cant_desp7           AS cant_desp7,
                   ccd.cant_rt1             AS cant_rt1,
                   ccd.cant_rt2             AS cant_rt2,
                   ccd.cant_rt3             AS cant_rt3,
                   ccd.cant_rt4             AS cant_rt4,
                   ccd.cant_rt5             AS cant_rt5,
                   ccd.cant_rt6             AS cant_rt6,
                   ccd.cant_rt7             AS cant_rt7,
                   0                        AS cobertura
            FROM centro_costo ccd,cc_entrega eccd,
                 (--Comunas/Ciudades/Regiones
                  SELECT com.adivgeo_ori comuna, ciu.adivgeo_ori ciudad, reg.a_divgeo region
                  FROM ubicaciones_geo com, ubicaciones_geo ciu, ubicaciones_geo reg
                  WHERE com.a_refdivgeo = ciu.a_divgeo AND ciu.a_refdivgeo = reg.a_divgeo
                    AND com.c_tipdg = 'CO' AND ciu.c_tipdg = 'CI' AND reg.c_tipdg = 'R') ccr
            WHERE (ccr.region = varregion OR varregion IS NULL)
              AND eccd.ccosto              = ccd.org_lvl_number
              AND eccd.canal_venta         = varcanalventa
              AND (ccd.org_lvl_number     = vardespachacc OR eccd.cod_tipocc  IN ('HI')) --H(I) -> cc_origen (!)= cc_despacha
              AND ccd.bas_addr_2          = ccr.comuna
              AND NVL(ccd.desactivado, 0) = 0
              AND ccd.org_is_store        = 'F'
              AND ccd.tienda_rt           = 1
              AND NVL(ccd.cant_rt, 0)     > 0
            UNION
            --Tiendas RT alimentadas por otros CC (cc_tienda)
            SELECT DISTINCT seq,
                   varcanalventa            AS "codigoCanalVenta",
                   ccd.bas_addr_2           AS "comunaDespacho",
                   varregion                AS "regionDespacho",
                   0                        AS "codigoTransporte",
                   vartipostock             AS "tipoStock",
                   ''                       AS "origenCC",
                   NVL(vardespachacc, '')   AS "despachaCC",
                   NVL(cct.ccosto, ccd.org_lvl_number) AS "origenCCLod",
                   ccd.org_lvl_number       AS "despachaCCLod",
                   0                        AS "viaCCLod",
                   ecco.cod_tipocc          AS tipocc_origen,
                   eccd.cod_tipocc          AS tipocc_despacha,
                   ''                       AS tipocc_via,
                   cco.tpo_procesoprod      AS "tiempoProcesoOrigen",
                   cct.tiempo_desp          AS "tiempoViajeOrigen",
                   0                        AS "tiempoProcesoCCVia",
                   0                        AS "tiempoViajeCCVia",
                   ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                   ''                       AS "tiempoViajeDespacho",
                   ''                       AS "frecuenciaViaje",
                   cco.dias_despacho        AS diasccorigen,
                   ccd.dias_despacho        AS "diasCCDespacha",
                   ''                       AS diasccvia,
                   ccd.cant_desp            AS "cantidadDespachada",
                   0                        AS "prioridad",
                   ''                       AS codproveedor,
                   0                        AS tpo_prov_proceso,
                   0                        AS tpo_prov_viaje,
                   ''                       AS dias_prov_despacha,
                   0                        AS cap_prov_diaria,
                   cco.recibe_psala         AS cc_origen_tipo_sala,
                   cco.recibe_pconst        AS cc_origen_tipo_const,
                   ccd.recibe_psala         AS cc_despacha_tipo_sala,
                   ccd.recibe_pconst        AS cc_despacha_tipo_const,
                   ''                       AS cc_via_tipo_sala,
                   ''                       AS cc_via_tipo_const,
                   ccd.express              AS serv_express,
                   ccd.cant_express         AS cap_desp_express,
                   ccd.hora_corte           AS hora_corte,
                   ccr.ciudad               AS ciudadDespacho,
                   ccr.region               AS regionlod,
                   ccr.ciudad               AS ciudadlod,
                   ccr.comuna               AS comunalod,
                   ccd.org_name_full        AS nombreTienda,
                   ccd.bas_addr_1           AS direccionTienda,
                   ccd.tienda_rt            AS tienda_rt,
                   ccd.cant_rt              AS cant_rt,
                   ccd.dias_rt              AS dias_rt,
                   ccd.hora_corte_rt        AS hora_corte_rt,
                   ccd.cant_desp1           AS cant_desp1,
                   ccd.cant_desp2           AS cant_desp2,
                   ccd.cant_desp3           AS cant_desp3,
                   ccd.cant_desp4           AS cant_desp4,
                   ccd.cant_desp5           AS cant_desp5,
                   ccd.cant_desp6           AS cant_desp6,
                   ccd.cant_desp7           AS cant_desp7,
                   ccd.cant_rt1             AS cant_rt1,
                   ccd.cant_rt2             AS cant_rt2,
                   ccd.cant_rt3             AS cant_rt3,
                   ccd.cant_rt4             AS cant_rt4,
                   ccd.cant_rt5             AS cant_rt5,
                   ccd.cant_rt6             AS cant_rt6,
                   ccd.cant_rt7             AS cant_rt7,
                   0                        AS cobertura
            FROM centro_costo cco,
                 centro_costo ccd,
                 cc_tienda cct,
                 cc_entrega ecco,
                 cc_entrega eccd,
                 (--Comunas/Ciudades/Regiones
                  SELECT com.adivgeo_ori comuna, ciu.adivgeo_ori ciudad, reg.a_divgeo region
                  FROM ubicaciones_geo com, ubicaciones_geo ciu, ubicaciones_geo reg
                  WHERE com.a_refdivgeo = ciu.a_divgeo AND ciu.a_refdivgeo = reg.a_divgeo
                    AND com.c_tipdg = 'CO' AND ciu.c_tipdg = 'CI' AND reg.c_tipdg = 'R') ccr
            WHERE (ccr.region = varregion OR varregion IS NULL)
              AND ecco.ccosto              = cco.org_lvl_number
              AND ecco.canal_venta         = varcanalventa
              AND eccd.ccosto              = ccd.org_lvl_number
              AND eccd.canal_venta         = varcanalventa
              AND (ccd.org_lvl_number     = vardespachacc OR eccd.cod_tipocc IN ('H', 'HI')) --H(I) -> cc_origen (!)= cc_despacha
              AND cct.ccosto              = cco.org_lvl_number
              AND cct.tienda              = ccd.org_lvl_number
              AND ccd.bas_addr_2          = ccr.comuna
              AND NVL(cco.desactivado, 0) = 0
              AND NVL(ccd.desactivado, 0) = 0
              AND NVL(cct.desactivado, 0) = 0
              AND ccd.org_is_store        = 'F'
              AND ccd.tienda_rt           = 1
              AND NVL(ccd.cant_rt, 0)     > 0
            ;
            IF SQL%ROWCOUNT = 0 THEN
              out_status_code := 1;
              out_status_msg  := 'DAD: No hay datos para estos registros'
                            || ' Accion: '              || varaccion
                            || ' ' || strregion || ': ' || varregion
                            || ' DespachaCC: '          || vardespachacc
                            || ' CanalVenta: '          || varcanalventa;
              RETURN;
            END IF;
          EXCEPTION
            WHEN DUP_VAL_ON_INDEX THEN
              NULL;
            WHEN OTHERS THEN
              out_status_code := 1;
              out_status_msg  := SQLERRM;
              RETURN;
          END;
          ELSE
            out_status_code := 1;
            out_status_msg  := 'DAD: No hay datos para estos registros'
                          || ' Accion: '              || varaccion
                          || ' ' || strregion || ': ' || varregion
                          || ' DespachaCC: '          || vardespachacc
                          || ' CanalVenta: '          || varcanalventa;
            RETURN;
          END IF;
        ELSE
          out_status_code := 1;
          out_status_msg := 'DAD: Criterio de consulta no valido ';
          RETURN;
        END IF;

--***** FLE1: solicita los datos de las tiendas RT del campo varcomuna (comuna despacho); similar a VAT2.
      ELSIF (varaccion = 'FLE1') THEN
        IF (vartipostock IN ('S')) THEN
          BEGIN
            INSERT INTO tmp_lod
            --Tiendas RT
            SELECT DISTINCT seq,
                   varcanalventa            AS "codigoCanalVenta",
                   ccd.bas_addr_2           AS "comunaDespacho",
                   ccr.region               AS "regionDespacho",
                   0                        AS "codigoTransporte",
                   vartipostock             AS "tipoStock",
                   ''                       AS "origenCC",
                   NVL(vardespachacc, '')   AS "despachaCC",
                   ccd.org_lvl_number       AS "origenCCLod",
                   ccd.org_lvl_number       AS "despachaCCLod",
                   0                        AS "viaCCLod",
                   eccd.cod_tipocc          AS tipocc_origen,
                   eccd.cod_tipocc          AS tipocc_despacha,
                   ''                       AS tipocc_via,
                   0                        AS "tiempoProcesoOrigen",
                   0                        AS "tiempoViajeOrigen",
                   0                        AS "tiempoProcesoCCVia",
                   0                        AS "tiempoViajeCCVia",
                   ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                   ''                       AS "tiempoViajeDespacho",
                   ''                       AS "frecuenciaViaje",
                   ''                       AS diasccorigen,
                   ccd.dias_despacho        AS "diasCCDespacha",
                   ''                       AS diasccvia,
                   ccd.cant_desp            AS "cantidadDespachada",
                   0                        AS "prioridad",
                   ''                       AS codproveedor,
                   0                        AS tpo_prov_proceso,
                   0                        AS tpo_prov_viaje,
                   ''                       AS dias_prov_despacha,
                   0                        AS cap_prov_diaria,
                   ccd.recibe_psala         AS cc_origen_tipo_sala,
                   ccd.recibe_pconst        AS cc_origen_tipo_const,
                   ccd.recibe_psala         AS cc_despacha_tipo_sala,
                   ccd.recibe_pconst        AS cc_despacha_tipo_const,
                   ''                       AS cc_via_tipo_sala,
                   ''                       AS cc_via_tipo_const,
                   ccd.express              AS serv_express,
                   ccd.cant_express         AS cap_desp_express,
                   ccd.hora_corte           AS hora_corte,
                   ccr.ciudad               AS ciudadDespacho,
                   ccr.region               AS regionlod,
                   ccr.ciudad               AS ciudadlod,
                   ccr.comuna               AS comunalod,
                   ccd.org_name_full        AS nombreTienda,
                   ccd.bas_addr_1           AS direccionTienda,
                   DECODE(ccd.tienda_rt, 1, 2, ccd.tienda_rt) AS tienda_rt,
                   ccd.cant_rt              AS cant_rt,
                   ccd.dias_rt              AS dias_rt,
                   ccd.hora_corte_rt        AS hora_corte_rt,
                   ccd.cant_desp1           AS cant_desp1,
                   ccd.cant_desp2           AS cant_desp2,
                   ccd.cant_desp3           AS cant_desp3,
                   ccd.cant_desp4           AS cant_desp4,
                   ccd.cant_desp5           AS cant_desp5,
                   ccd.cant_desp6           AS cant_desp6,
                   ccd.cant_desp7           AS cant_desp7,
                   ccd.cant_rt1             AS cant_rt1,
                   ccd.cant_rt2             AS cant_rt2,
                   ccd.cant_rt3             AS cant_rt3,
                   ccd.cant_rt4             AS cant_rt4,
                   ccd.cant_rt5             AS cant_rt5,
                   ccd.cant_rt6             AS cant_rt6,
                   ccd.cant_rt7             AS cant_rt7,
                   0                        AS cobertura
            FROM centro_costo ccd,cc_entrega eccd,
                 (--Comunas/Ciudades/Regiones
                  SELECT com.adivgeo_ori comuna, ciu.adivgeo_ori ciudad, reg.a_divgeo region
                  FROM ubicaciones_geo com, ubicaciones_geo ciu, ubicaciones_geo reg
                  WHERE com.a_refdivgeo = ciu.a_divgeo AND ciu.a_refdivgeo = reg.a_divgeo
                    AND com.c_tipdg = 'CO' AND ciu.c_tipdg = 'CI' AND reg.c_tipdg = 'R') ccr
            WHERE ccd.org_lvl_number IN (SELECT regexp_substr(vartiendas,'[^,]+', 1, LEVEL) AS listado
                                         FROM dual
                                         CONNECT BY regexp_substr(vartiendas, '[^,]+', 1, LEVEL) IS NOT NULL)
               AND eccd.ccosto              = ccd.org_lvl_number
              AND eccd.canal_venta         = varcanalventa
              AND (ccd.org_lvl_number     = vardespachacc OR eccd.cod_tipocc IN ('HI')) --H(I) -> cc_origen (!)= cc_despacha
              -- se valida si el tipo de producto puede ser retirado en tienda
              AND ccd.bas_addr_2           = ccr.comuna
              AND NVL(ccd.desactivado, 0)  = 0
              AND ccd.org_is_store         = 'F'
              AND ccd.tienda_rt            = 1
              AND NVL(ccd.cant_rt, 0)      > 0
            UNION
            --Tiendas RT alimentadas por otros CC (cc_tienda)
            SELECT DISTINCT seq,
                   varcanalventa            AS "codigoCanalVenta",
                   ccd.bas_addr_2           AS "comunaDespacho",
                   ccr.region               AS "regionDespacho",
                   0                        AS "codigoTransporte",
                   vartipostock             AS "tipoStock",
                   ''                       AS "origenCC",
                   NVL(vardespachacc, '')   AS "despachaCC",
                   NVL(cct.ccosto, ccd.org_lvl_number) AS "origenCCLod",
                   ccd.org_lvl_number       AS "despachaCCLod",
                   0                        AS "viaCCLod",
                   ecco.cod_tipocc          AS tipocc_origen,
                   eccd.cod_tipocc          AS tipocc_despacha,
                   ''                       AS tipocc_via,
                   cco.tpo_procesoprod      AS "tiempoProcesoOrigen",
                   cct.tiempo_desp          AS "tiempoViajeOrigen",
                   0                        AS "tiempoProcesoCCVia",
                   0                        AS "tiempoViajeCCVia",
                   ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                   ''                       AS "tiempoViajeDespacho",
                   ''                       AS "frecuenciaViaje",
                   cco.dias_despacho        AS diasccorigen,
                   ccd.dias_despacho        AS "diasCCDespacha",
                   ''                       AS diasccvia,
                   ccd.cant_desp            AS "cantidadDespachada",
                   0                        AS "prioridad",
                   ''                       AS codproveedor,
                   0                        AS tpo_prov_proceso,
                   0                        AS tpo_prov_viaje,
                   ''                       AS dias_prov_despacha,
                   0                        AS cap_prov_diaria,
                   cco.recibe_psala         AS cc_origen_tipo_sala,
                   cco.recibe_pconst        AS cc_origen_tipo_const,
                   ccd.recibe_psala         AS cc_despacha_tipo_sala,
                   ccd.recibe_pconst        AS cc_despacha_tipo_const,
                   ''                       AS cc_via_tipo_sala,
                   ''                       AS cc_via_tipo_const,
                   ccd.express              AS serv_express,
                   ccd.cant_express         AS cap_desp_express,
                   ccd.hora_corte           AS hora_corte,
                   ccr.ciudad               AS ciudadDespacho,
                   ccr.region               AS regionlod,
                   ccr.ciudad               AS ciudadlod,
                   ccr.comuna               AS comunalod,
                   ccd.org_name_full        AS nombreTienda,
                   ccd.bas_addr_1           AS direccionTienda,
                   DECODE(ccd.tienda_rt, 1, 2, ccd.tienda_rt) AS tienda_rt,
                   ccd.cant_rt              AS cant_rt,
                   ccd.dias_rt              AS dias_rt,
                   ccd.hora_corte_rt        AS hora_corte_rt,
                   ccd.cant_desp1           AS cant_desp1,
                   ccd.cant_desp2           AS cant_desp2,
                   ccd.cant_desp3           AS cant_desp3,
                   ccd.cant_desp4           AS cant_desp4,
                   ccd.cant_desp5           AS cant_desp5,
                   ccd.cant_desp6           AS cant_desp6,
                   ccd.cant_desp7           AS cant_desp7,
                   ccd.cant_rt1             AS cant_rt1,
                   ccd.cant_rt2             AS cant_rt2,
                   ccd.cant_rt3             AS cant_rt3,
                   ccd.cant_rt4             AS cant_rt4,
                   ccd.cant_rt5             AS cant_rt5,
                   ccd.cant_rt6             AS cant_rt6,
                   ccd.cant_rt7             AS cant_rt7,
                   0                        AS cobertura
            FROM centro_costo cco,
                 centro_costo ccd,
                 cc_tienda cct,
                 cc_entrega eccd,
                 cc_entrega ecco,
                 (--Comunas/Ciudades/Regiones
                  SELECT com.adivgeo_ori comuna, ciu.adivgeo_ori ciudad, reg.a_divgeo region
                  FROM ubicaciones_geo com, ubicaciones_geo ciu, ubicaciones_geo reg
                  WHERE com.a_refdivgeo = ciu.a_divgeo AND ciu.a_refdivgeo = reg.a_divgeo
                    AND com.c_tipdg = 'CO' AND ciu.c_tipdg = 'CI' AND reg.c_tipdg = 'R') ccr
            WHERE ccd.org_lvl_number IN (SELECT regexp_substr(vartiendas,'[^,]+', 1, LEVEL) AS listado
                                         FROM dual
                                         CONNECT BY regexp_substr(vartiendas, '[^,]+', 1, LEVEL) IS NOT NULL)
              AND ecco.ccosto              = cco.org_lvl_number
              AND ecco.canal_venta         = varcanalventa
              AND eccd.ccosto              = ccd.org_lvl_number
              AND eccd.canal_venta         = varcanalventa
              AND (ccd.org_lvl_number      = vardespachacc OR eccd.cod_tipocc IN ('H', 'HI')) --H(I) -> cc_origen (!)= cc_despacha
              AND cct.ccosto               = cco.org_lvl_number
              AND cct.tienda               = ccd.org_lvl_number
              AND ccd.bas_addr_2           = ccr.comuna
              AND NVL(cco.desactivado, 0)  = 0
              AND NVL(ccd.desactivado, 0)  = 0
              AND NVL(cct.desactivado, 0)  = 0
              AND ccd.org_is_store         = 'F'
              AND ccd.tienda_rt            = 1
              AND NVL(ccd.cant_rt, 0)      > 0
            ;
            IF SQL%ROWCOUNT = 0 THEN
              out_status_code := 1;
              out_status_msg  := 'DAD: No hay datos para estos registros'
                            || ' Accion: '              || varaccion
                            || ' ' || strregion || ': ' || varregion
                            || ' DespachaCC: '          || vardespachacc
                            || ' CanalVenta: '          || varcanalventa;
              RETURN;
            END IF;
          EXCEPTION
            WHEN DUP_VAL_ON_INDEX THEN
              NULL;
            WHEN OTHERS THEN
              out_status_code := 1;
              out_status_msg  := SQLERRM;
              RETURN;
          END;

        ELSE
          out_status_code := 1;
          out_status_msg := 'DAD: Criterio de consulta no valido ';
          RETURN;
        END IF;

-- != VAT y FLE
--***** Stock Proveedor y Cod_Prov
      ELSIF (vartipostock = 'P') THEN
        BEGIN
          INSERT INTO tmp_lod
          --Proveedores que despachan a clientes
          SELECT seq,
                 varcanalventa           AS "codigoCanalVenta",
                 pc.comuna               AS "comunaDespacho",
                 NULL                    AS "regionDespacho",
                 0                       AS "codigoTransporte",
                 vartipostock            AS "tipoStock",
                 ''                      AS "origenCC",
                 ''                      AS "despachaCC",
                 p.recibe_oc             AS "origenCCLod",
                 ''                      AS "despachaCCLod",
                 ''                      AS "viaCCLOD",
                 ''                      AS tipocc_origen,
                 ''                      AS tipocc_despacha,
                 ''                      AS tipocc_via,
                 0                       AS "tiempoProcesoOrigen",
                 0                       AS "tiempoViajeOrigen",
                 0                       AS "tiempoProcesoCCVia",
                 0                       AS "tiempoViajeCCVia",
                 0                       AS "tiempoProcesoDespacho",
                 0                       AS "tiempoViajeDespacho",
                 0                       AS "frecuenciaViaje",
                 ''                      AS diasccorigen,
                 ''                      AS "diasCCDespacha",
                 ''                      AS diasccvia,
                 0                       AS "cantidadDespachada",
                 0                       AS "prioridad",
                 varcodproveedor         AS  codproveedor,
                 p.tpo_procesoprod       AS tpo_prov_proceso,
                 pc.tiempo_desp          AS tpo_prov_viaje,
                 p.dias_despacho         AS dias_prov_despacha,
                 p.cant_desp             AS cap_prov_diaria,
                 ''                      AS cc_origen_tipo_sala,
                 ''                      AS cc_origen_tipo_const,
                 ''                      AS cc_despacha_tipo_sala,
                 ''                      AS cc_despacha_tipo_const,
                 ''                      AS cc_via_tipo_sala,
                 ''                      AS cc_via_tipo_const,
                 ''                      AS serv_express,
                 0                       AS cap_desp_express,
                 ''                      AS hora_corte,
                 ''                      AS ciudadDespacho,
                 ''                      AS regionlod,
                 ''                      AS ciudadlod,
                 ''                      AS comunalod,
                 ''                      AS nombreTienda,
                 ''                      AS direccionTienda,
                 0                       AS tienda_rt,
                 0                       AS cant_rt,
                 ''                      AS dias_rt,
                 ''                      AS hora_corte_rt,
                 null                    AS cant_desp1,
                 null                    AS cant_desp2,
                 null                    AS cant_desp3,
                 null                    AS cant_desp4,
                 null                    AS cant_desp5,
                 null                    AS cant_desp6,
                 null                    AS cant_desp7,
                 null                    AS cant_rt1,
                 null                    AS cant_rt2,
                 null                    AS cant_rt3,
                 null                    AS cant_rt4,
                 null                    AS cant_rt5,
                 null                    AS cant_rt6,
                 null                    AS cant_rt7,
                 0                        AS cobertura
          FROM prov_comuna pc,
               proveedor p
          WHERE pc.comuna              = varcomuna2
            AND pc.cod_prov            = CAST(varcodproveedor AS CHAR(10))
            AND pc.cod_prov            = p.cod_prov
            AND NVL(p.desactivado, 0)  = 0
            AND NVL(pc.desactivado, 0) = 0
            AND pc.tiempo_desp IS NOT NULL
          UNION
          --Proveedores que entregan a Sodimac para su despacho
          SELECT DISTINCT seq,
                 varcanalventa            AS "codigoCanalVenta",
                 ccom.comuna              AS "comunaDespacho",
                 NULL                     AS "regionDespacho",
                 0                        AS "codigoTransporte",
                 vartipostock             AS "tipoStock",
                 ''                       AS "origenCC",
                 ''                       AS "despachaCC",
                 ccd.org_lvl_number       AS "origenCCLod",
                 CAST(ccd.org_lvl_number AS VARCHAR2(10)) AS "despachaCCLod",
                 ''                       AS "viaCCLOD",
                 eccd.cod_tipocc          AS tipocc_origen,
                 eccd.cod_tipocc          AS tipocc_despacha,
                 ''                       AS tipocc_via,
                 0                        AS "tiempoProcesoOrigen",
                 0                        AS "tiempoViajeOrigen",
                 0                        AS "tiempoProcesoCCVia",
                 0                        AS "tiempoViajeCCVia",
                 ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                 ccom.tiempo_desp         AS "tiempoViajeDespacho",
                 ccom.frec_abastece       AS "frecuenciaViaje",
                 ''                       AS diasccorigen,
                 NVL(z.dias_despzona, ccd.dias_despacho) AS "diasCCDespacha",
                 ccd.dias_despacho        AS diasccvia,
                 ccd.cant_desp            AS "cantidadDespachada",
                 0                        AS "prioridad",
                 varcodproveedor          AS codproveedor,
                 p.tpo_procesoprod        AS tpo_prov_proceso,
                 pc.tiempo_desp           AS tpo_prov_viaje,
                 p.dias_despacho          AS dias_prov_despacha,
                 p.cant_desp              AS cap_prov_diaria,
                 ccd.recibe_psala         AS cc_origen_tipo_sala,
                 ccd.recibe_pconst        AS cc_origen_tipo_const,
                 ccd.recibe_psala         AS cc_despacha_tipo_sala,
                 ccd.recibe_pconst        AS cc_despacha_tipo_const,
                 ''                       AS cc_via_tipo_sala,
                 ''                       AS cc_via_tipo_const,
                 ccd.express              AS serv_express,
                 ccd.cant_express         AS cap_desp_express,
                 ccd.hora_corte           AS hora_corte,
                 ''                       AS ciudadDespacho,
                 ''                       AS regionlod,
                 ''                       AS ciudadlod,
                 ''                       AS comunalod,
                 ''                       AS nombreTienda,
                 ''                       AS direccionTienda,
                 ccd.tienda_rt            AS tienda_rt,
                 ccd.cant_rt              AS cant_rt,
                 ccd.dias_rt              AS dias_rt,
                 ccd.hora_corte_rt        AS hora_corte_rt,
                 ccd.cant_desp1           AS cant_desp1,
                 ccd.cant_desp2           AS cant_desp2,
                 ccd.cant_desp3           AS cant_desp3,
                 ccd.cant_desp4           AS cant_desp4,
                 ccd.cant_desp5           AS cant_desp5,
                 ccd.cant_desp6           AS cant_desp6,
                 ccd.cant_desp7           AS cant_desp7,
                 ccd.cant_rt1             AS cant_rt1,
                 ccd.cant_rt2             AS cant_rt2,
                 ccd.cant_rt3             AS cant_rt3,
                 ccd.cant_rt4             AS cant_rt4,
                 ccd.cant_rt5             AS cant_rt5,
                 ccd.cant_rt6             AS cant_rt6,
                 ccd.cant_rt7             AS cant_rt7,
                 0                        AS cobertura
          FROM cc_comuna ccom,
               prov_cc pc,
               centro_costo ccd,
               proveedor p,
               zona z,
               cc_entrega eccd
          WHERE ccom.comuna              = varcomuna2
            AND eccd.ccosto              = ccd.org_lvl_number
            AND eccd.canal_venta         = varcanalventa
            AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
            AND pc.cod_prov              = CAST (varcodproveedor AS CHAR(10))
            AND p.cod_prov               = pc.cod_prov
            AND ccd.org_lvl_number       = ccom.ccosto
            AND ccd.org_lvl_number       = pc.ccosto
            AND ccom.ccosto              = z.ccosto (+)
            AND ccom.cod_zona            = z.cod_zona (+)
            AND NVL(ccd.desactivado, 0)  = 0
            AND NVL(ccom.desactivado, 0) = 0
            AND NVL(p.desactivado, 0)    = 0
            AND NVL(pc.desactivado, 0)   = 0
            AND NVL(ccd.cant_desp, 0)    > 0
            AND ccom.tiempo_desp IS NOT NULL
            AND ccom.ccosto IN (SELECT org_lvl_number FROM (SELECT cc.org_lvl_number
                                FROM cc_comuna ccom, centro_costo cc, cc_entrega ecc
                                WHERE ccom.comuna              = varcomuna2
                                  AND ecc.ccosto               = cc.org_lvl_number
                                  AND ecc.canal_venta          = varcanalventa
                                  AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
                                  AND ccom.ccosto              = cc.org_lvl_number
                                  AND(ccom.ccosto = vardespachacc OR NVL(varorigencc, 0) = 0)
                                  AND NVL(cc.desactivado, 0)   = 0
                                  AND NVL(ccom.desactivado, 0) = 0
                                  AND cc.despacha              = 1
                                  AND NVL(cc.cant_desp, 0)     > 0
                                  AND ccom.tiempo_desp IS NOT NULL
                                  AND (cc.org_lvl_number = vardespachacc OR ecc.cod_tipocc IN ('H', 'HI'))
                                ORDER BY CASE
                                           WHEN cc.org_is_store = 'T' AND cc.org_lvl_number = CDSTGO THEN -1000000
                                           WHEN cc.org_is_store = 'T' THEN -cc.cant_desp
                                           ELSE 0
                                         END,
                                         CASE
                                           WHEN  ecc.cod_tipocc = 'HI' THEN -cc.cant_desp*1000
                                           WHEN  ecc.cod_tipocc = 'H'  THEN -cc.cant_desp
                                           ELSE 0
                                         END))
          UNION
          --CC que despachan alimentados por otros CC (cc_tienda)
          SELECT DISTINCT seq,
                 varcanalventa            AS "codigoCanalVenta",
                 ccom.comuna              AS "comunaDespacho",
                 NULL                     AS "regionDespacho",
                 0                        AS "codigoTransporte",
                 vartipostock             AS "tipoStock",
                 ''                       AS "origenCC",
                 ''                       AS "despachaCC",
                 cct.ccosto               AS "origenCCLod",
                 CAST(cct.tienda AS VARCHAR2(10)) AS "despachaCCLod",
                 ''                       AS "viaCCLod",
                 ecco.cod_tipocc          AS tipocc_origen,
                 eccd.cod_tipocc          AS tipocc_despacha,
                 ''                       AS tipocc_via,
                 cco.tpo_procesoprod      AS "tiempoProcesoOrigen",
                 cct.tiempo_desp          AS "tiempoViajeOrigen",
                 0                        AS "tiempoProcesoCCVia",
                 0                        AS "tiempoViajeCCVia",
                 ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                 ccom.tiempo_desp         AS "tiempoViajeDespacho",
                 ccom.frec_abastece       AS "frecuenciaViaje",
                 cco.dias_despacho        AS diasccorigen,
                 NVL(z.dias_despzona, ccd.dias_despacho) AS "diasCCDespacha",
                 ccd.dias_despacho        AS diasccvia,
                 ccd.cant_desp            AS "cantidadDespachada",
                 0                        AS "prioridad",
                 varcodproveedor          AS codproveedor,
                 p.tpo_procesoprod        AS tpo_prov_proceso,
                 pc.tiempo_desp           AS tpo_prov_viaje,
                 p.dias_despacho          AS dias_prov_despacha,
                 p.cant_desp              AS cap_prov_diaria,
                 cco.recibe_psala         AS cc_origen_tipo_sala,
                 cco.recibe_pconst        AS cc_origen_tipo_const,
                 ccd.recibe_psala         AS cc_despacha_tipo_sala,
                 ccd.recibe_pconst        AS cc_despacha_tipo_const,
                 ''                       AS cc_via_tipo_sala,
                 ''                       AS cc_via_tipo_const,
                 ccd.express              AS serv_express,
                 ccd.cant_express         AS cap_desp_express,
                 ccd.hora_corte           AS hora_corte,
                 ''                       AS ciudadDespacho,
                 ''                       AS regionlod,
                 ''                       AS ciudadlod,
                 ''                       AS comunalod,
                 ''                       AS nombreTienda,
                 ''                       AS direccionTienda,
                 ccd.tienda_rt            AS tienda_rt,
                 ccd.cant_rt              AS cant_rt,
                 ccd.dias_rt              AS dias_rt,
                 ccd.hora_corte_rt        AS hora_corte_rt,
                 ccd.cant_desp1           AS cant_desp1,
                 ccd.cant_desp2           AS cant_desp2,
                 ccd.cant_desp3           AS cant_desp3,
                 ccd.cant_desp4           AS cant_desp4,
                 ccd.cant_desp5           AS cant_desp5,
                 ccd.cant_desp6           AS cant_desp6,
                 ccd.cant_desp7           AS cant_desp7,
                 ccd.cant_rt1             AS cant_rt1,
                 ccd.cant_rt2             AS cant_rt2,
                 ccd.cant_rt3             AS cant_rt3,
                 ccd.cant_rt4             AS cant_rt4,
                 ccd.cant_rt5             AS cant_rt5,
                 ccd.cant_rt6             AS cant_rt6,
                 ccd.cant_rt7             AS cant_rt7,
                 0                        AS cobertura
          FROM cc_comuna ccom,
               prov_cc pc,
               cc_tienda cct,
               centro_costo cco,
               centro_costo ccd,
               proveedor p,
               zona z,
               cc_entrega ecco,
               cc_entrega eccd
          WHERE ccom.comuna              = varcomuna2
            AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
            AND ecco.ccosto              = cco.org_lvl_number
            AND ecco.canal_venta         = varcanalventa
            AND eccd.ccosto              = ccd.org_lvl_number
            AND eccd.canal_venta         = varcanalventa
            AND pc.cod_prov              = CAST (varcodproveedor AS CHAR(10))
            AND cct.ccosto               = cco.org_lvl_number
            AND cct.tienda               = ccd.org_lvl_number
            AND cct.tienda               = ccom.ccosto
            AND cco.org_lvl_number       = pc.ccosto
            AND p.cod_prov               = pc.cod_prov
            AND ccom.ccosto              = z.ccosto (+)
            AND ccom.cod_zona            = z.cod_zona (+)
            AND NVL(cco.desactivado, 0)  = 0
            AND NVL(ccd.desactivado, 0)  = 0
            AND NVL(ccom.desactivado, 0) = 0
            AND NVL(cct.desactivado, 0)  = 0
            AND NVL(p.desactivado, 0)    = 0
            AND NVL(pc.desactivado, 0)   = 0
            AND ccom.tiempo_desp IS NOT NULL
            AND ccom.ccosto IN (SELECT org_lvl_number FROM (SELECT cc.org_lvl_number
                                FROM cc_comuna ccom, centro_costo cc, cc_entrega ecc
                                WHERE ccom.comuna              = varcomuna2
                                  AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
                                  AND ecc.ccosto               = cc.org_lvl_number
                                  AND ecc.canal_venta          = varcanalventa
                                  AND ccom.ccosto              = cc.org_lvl_number
                                  AND(ccom.ccosto = vardespachacc OR NVL(varorigencc, 0) = 0)
                                  AND NVL(cc.desactivado, 0)   = 0
                                  AND NVL(ccom.desactivado, 0) = 0
                                  AND cc.despacha              = 1
                                  AND NVL(cc.cant_desp, 0)     > 0
                                  AND ccom.tiempo_desp IS NOT NULL
                                  AND (cc.org_lvl_number = vardespachacc OR ecc.cod_tipocc  IN ('H', 'HI'))
                                ORDER BY CASE
                                           WHEN cc.org_is_store = 'T' AND cc.org_lvl_number = CDSTGO THEN -1000000
                                           WHEN cc.org_is_store = 'T' THEN -cc.cant_desp
                                           ELSE 0
                                         END,
                                         CASE
                                            WHEN ecc.cod_tipocc = 'HI' THEN -cc.cant_desp*1000
                                            WHEN ecc.cod_tipocc = 'H'  THEN -cc.cant_desp
                                           ELSE 0
                                         END))
          ;
          IF SQL%ROWCOUNT = 0 THEN
            --Para Consulta Disp se envía SIN-COBERTURA (debe estar activo
            IF (varsin_cobertura = 1 AND (varorigencc IS NULL OR varorigencc = '' OR varorigencc = 0)) THEN
              INSERT INTO tmp_lod
              --Se evita error, se envía registro en blanco.
              SELECT seq,
                     varcanalventa           AS "codigoCanalVenta",
                     varcomuna2              AS "comunaDespacho",
                     varregion               AS "regionDespacho",
                     0                       AS "codigoTransporte",
                     vartipostock            AS "tipoStock",
                     varorigencc             AS "origenCC",
                     NVL(vardespachacc, '')  AS "despachaCC",
                     ''                      AS "origenCCLod",
                     ''                      AS "despachaCCLod",
                     ''                      AS "viaCCLOD",
                     ''                      AS tipocc_origen,
                     ''                      AS tipocc_despacha,
                     ''                      AS tipocc_via,
                     0                       AS "tiempoProcesoOrigen",
                     0                       AS "tiempoViajeOrigen",
                     0                       AS "tiempoProcesoCCVia",
                     0                       AS "tiempoViajeCCVia",
                     0                       AS "tiempoProcesoDespacho",
                     0                       AS "tiempoViajeDespacho",
                     0                       AS "frecuenciaViaje",
                     ''                      AS diasccorigen,
                     ''                      AS "diasCCDespacha",
                     ''                      AS diasccvia,
                     0                       AS "cantidadDespachada",
                     0                       AS "prioridad",
                     varcodproveedor         AS codproveedor,
                     0                       AS tpo_prov_proceso,
                     0                       AS tpo_prov_viaje,
                     ''                      AS dias_prov_despacha,
                     0                       AS cap_prov_diaria,
                     ''                      AS cc_origen_tipo_sala,
                     ''                      AS cc_origen_tipo_const,
                     ''                      AS cc_despacha_tipo_sala,
                     ''                      AS cc_despacha_tipo_const,
                     ''                      AS cc_via_tipo_sala,
                     ''                      AS cc_via_tipo_const,
                     ''                      AS serv_express,
                     0                       AS cap_desp_express,
                     ''                      AS hora_corte,
                     varciudad               AS ciudadDespacho,
                     ''                      AS regionlod,
                     ''                      AS ciudadlod,
                     ''                      AS comunalod,
                     ''                      AS nombreTienda,
                     ''                      AS direccionTienda,
                     0                       AS tienda_rt,
                     0                       AS cant_rt,
                     ''                      AS dias_rt,
                     ''                      AS hora_corte_rt,
                     null                    AS cant_desp1,
                     null                    AS cant_desp2,
                     null                    AS cant_desp3,
                     null                    AS cant_desp4,
                     null                    AS cant_desp5,
                     null                    AS cant_desp6,
                     null                    AS cant_desp7,
                     null                    AS cant_rt1,
                     null                    AS cant_rt2,
                     null                    AS cant_rt3,
                     null                    AS cant_rt4,
                     null                    AS cant_rt5,
                     null                    AS cant_rt6,
                     null                    AS cant_rt7,
                     1                       AS cobertura
              FROM dual
              ;
            ELSE
              out_status_code := 1;
              out_status_msg := 'DAD: No hay Datos para Estos Registros '
                            || ' ' || strcomuna || ': ' || varcomuna
                            || ' Proveedor: '           || varcodproveedor
                            || ' CanalVenta: '          || varcanalventa;
              RETURN;
            END IF;
          END IF;
        EXCEPTION
          WHEN DUP_VAL_ON_INDEX THEN
            NULL;
          WHEN OTHERS THEN
            out_status_code := 1;
            out_status_msg := SQLERRM;
            RETURN;
        END;

--***** Stock Sodimac sin CC_Origen y CC_Despacha es opcional (el CC_Despacha se toma como quien realiza la consulta/venta)
      ELSIF (vartipostock = 'S' AND (varorigencc IS NULL OR varorigencc = '' OR varorigencc = 0)) THEN
        BEGIN
          INSERT INTO tmp_lod
          --CC que despachan
          SELECT DISTINCT seq,
                 varcanalventa            AS "codigoCanalVenta",
                 ccom.comuna              AS "comunaDespacho",
                 NULL                     AS "regionDespacho",
                 0                        AS "codigoTransporte",
                 vartipostock             AS "tipoStock",
                 ''                       AS "origenCC",
                 NVL(vardespachacc, '')   AS "despachaCC",
                 ccd.org_lvl_number       AS "origenCCLod",
                 ccd.org_lvl_number       AS "despachaCCLod",
                 0                        AS "viaCCLOD",
                 eccd.cod_tipocc          AS tipocc_origen,
                 eccd.cod_tipocc          AS tipocc_despacha,
                 ''                       AS tipocc_via,
                 0                        AS "tiempoProcesoOrigen", --Se define que el CCS/CCI se demora 0 dias
                 0                        AS "tiempoViajeOrigen",   --Se define que el CCS/CCI se demora 0 dias
                 0                        AS "tiempoProcesoCCVia",
                 0                        AS "tiempoViajeCCVia",
                 ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                 ccom.tiempo_desp         AS "tiempoViajeDespacho",
                 ccom.frec_abastece       AS "frecuenciaViaje",
                 ''                       AS diasccorigen,
                 NVL(z.dias_despzona, ccd.dias_despacho) AS "diasCCDespacha",
                 ccd.dias_despacho        AS diasccvia,
                 ccd.cant_desp            AS "cantidadDespachada",
                 0                        AS "prioridad",
                 ''                       AS codproveedor,
                 0                        AS tpo_prov_proceso,
                 0                        AS tpo_prov_viaje,
                 ''                       AS dias_prov_despacha,
                 0                        AS cap_prov_diaria,
                 ccd.recibe_psala         AS cc_origen_tipo_sala,
                 ccd.recibe_pconst        AS cc_origen_tipo_const,
                 ccd.recibe_psala         AS cc_despacha_tipo_sala,
                 ccd.recibe_pconst        AS cc_despacha_tipo_const,
                 ''                       AS cc_via_tipo_sala,
                 ''                       AS cc_via_tipo_const,
                 ccd.express              AS serv_express,
                 ccd.cant_express         AS cap_desp_express,
                 ccd.hora_corte           AS hora_corte,
                 ''                       AS ciudadDespacho,
                 ''                       AS regionlod,
                 ''                       AS ciudadlod,
                 ''                       AS comunalod,
                 ''                       AS nombreTienda,
                 ''                       AS direccionTienda,
                 ccd.tienda_rt            AS tienda_rt,
                 ccd.cant_rt              AS cant_rt,
                 ccd.dias_rt              AS dias_rt,
                 ccd.hora_corte_rt        AS hora_corte_rt,
                 ccd.cant_desp1           AS cant_desp1,
                 ccd.cant_desp2           AS cant_desp2,
                 ccd.cant_desp3           AS cant_desp3,
                 ccd.cant_desp4           AS cant_desp4,
                 ccd.cant_desp5           AS cant_desp5,
                 ccd.cant_desp6           AS cant_desp6,
                 ccd.cant_desp7           AS cant_desp7,
                 ccd.cant_rt1             AS cant_rt1,
                 ccd.cant_rt2             AS cant_rt2,
                 ccd.cant_rt3             AS cant_rt3,
                 ccd.cant_rt4             AS cant_rt4,
                 ccd.cant_rt5             AS cant_rt5,
                 ccd.cant_rt6             AS cant_rt6,
                 ccd.cant_rt7             AS cant_rt7,
                 0                        AS cobertura
          FROM cc_comuna ccom,
               centro_costo ccd,
               zona z,
               cc_entrega eccd
          WHERE ccom.comuna              = varcomuna2
            AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
            AND eccd.ccosto              = ccd.org_lvl_number
            AND eccd.canal_venta         = varcanalventa
            AND (ccom.ccosto             = vardespachacc OR eccd.cod_tipocc IN ('HI')) --HI -> cc_origen = cc_despacha
            AND ccom.ccosto              = ccd.org_lvl_number
            AND ccom.ccosto              = z.ccosto (+)
            AND ccom.cod_zona            = z.cod_zona (+)
            AND NVL(ccd.desactivado, 0)  = 0
            AND NVL(ccom.desactivado, 0) = 0
            AND ccom.tiempo_desp IS NOT NULL
            AND ccom.ccosto IN (SELECT org_lvl_number FROM (SELECT cc.org_lvl_number
                                FROM cc_comuna ccom, centro_costo cc, cc_entrega ecc
                                WHERE ccom.comuna              = varcomuna2
                                  AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
                                  AND ecc.ccosto               = cc.org_lvl_number
                                  AND ecc.canal_venta          = varcanalventa
                                  AND ccom.ccosto              = cc.org_lvl_number
                                  AND NVL(cc.desactivado, 0)   = 0
                                  AND NVL(ccom.desactivado, 0) = 0
                                  AND cc.despacha              = 1
                                  AND NVL(cc.cant_desp, 0)     > 0
                                  AND ccom.tiempo_desp IS NOT NULL
                                  AND (cc.org_lvl_number = vardespachacc OR ecc.cod_tipocc IN ('H', 'HI'))
                                ORDER BY CASE
                                           WHEN cc.org_is_store = 'T' AND cc.org_lvl_number = CDSTGO THEN -1000000
                                           WHEN cc.org_is_store = 'T' THEN -cc.cant_desp
                                           ELSE 0
                                         END,
                                         CASE
                                           WHEN cc.org_lvl_number = vardespachacc THEN -1
                                           ELSE 0
                                         END,
                                         CASE
                                           WHEN ecc.cod_tipocc = 'HI' THEN -cc.cant_desp*1000
                                           WHEN ecc.cod_tipocc = 'H'  THEN -cc.cant_desp
                                           ELSE 0
                                         END))
          UNION
          --CC que despachan con sus CCS relacionados (no se muestran los CCI)
          SELECT DISTINCT seq,
                 varcanalventa            AS "codigoCanalVenta",
                 ccom.comuna              AS "comunaDespacho",
                 NULL                     AS "regionDespacho",
                 0                        AS "codigoTransporte",
                 vartipostock             AS "tipoStock",
                 ''                       AS "origenCC",
                 NVL(vardespachacc, '')   AS "despachaCC",
                 mlod.cc_origen           AS "origenCCLod",
                 mlod.cc_despacha         AS "despachaCCLod",
                 0                        AS "viaCCLod",
                 ecco.cod_tipocc          AS tipocc_origen,
                 eccd.cod_tipocc          AS tipocc_despacha,
                 ''                       AS tipocc_via,
                 0                        AS "tiempoProcesoOrigen", --Se define que el CCS/CCI se demora 0 dias
                 0                        AS "tiempoViajeOrigen",   --Se define que el CCS/CCI se demora 0 dias
                 0                        AS "tiempoProcesoCCVia",
                 0                        AS "tiempoViajeCCVia",
                 ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                 ccom.tiempo_desp         AS "tiempoViajeDespacho",
                 ccom.frec_abastece       AS "frecuenciaViaje",
                 cco.dias_despacho        AS diasccorigen,
                 NVL(z.dias_despzona, ccd.dias_despacho) AS "diasCCDespacha",
                 ccd.dias_despacho        AS diasccvia,
                 ccd.cant_desp            AS "cantidadDespachada",
                 0                        AS "prioridad",
                 ''                       AS codproveedor,
                 0                        AS tpo_prov_proceso,
                 0                        AS tpo_prov_viaje,
                 ''                       AS dias_prov_despacha,
                 0                        AS cap_prov_diaria,
                 cco.recibe_psala         AS cc_origen_tipo_sala,
                 cco.recibe_pconst        AS cc_origen_tipo_const,
                 ccd.recibe_psala         AS cc_despacha_tipo_sala,
                 ccd.recibe_pconst        AS cc_despacha_tipo_const,
                 ''                       AS cc_via_tipo_sala,
                 ''                       AS cc_via_tipo_const,
                 ccd.express              AS serv_express,
                 ccd.cant_express         AS cap_desp_express,
                 ccd.hora_corte           AS hora_corte,
                 ''                       AS ciudadDespacho,
                 ''                       AS regionlod,
                 ''                       AS ciudadlod,
                 ''                       AS comunalod,
                 ''                       AS nombreTienda,
                 ''                       AS direccionTienda,
                 ccd.tienda_rt            AS tienda_rt,
                 ccd.cant_rt              AS cant_rt,
                 ccd.dias_rt              AS dias_rt,
                 ccd.hora_corte_rt        AS hora_corte_rt,
                 ccd.cant_desp1           AS cant_desp1,
                 ccd.cant_desp2           AS cant_desp2,
                 ccd.cant_desp3           AS cant_desp3,
                 ccd.cant_desp4           AS cant_desp4,
                 ccd.cant_desp5           AS cant_desp5,
                 ccd.cant_desp6           AS cant_desp6,
                 ccd.cant_desp7           AS cant_desp7,
                 ccd.cant_rt1             AS cant_rt1,
                 ccd.cant_rt2             AS cant_rt2,
                 ccd.cant_rt3             AS cant_rt3,
                 ccd.cant_rt4             AS cant_rt4,
                 ccd.cant_rt5             AS cant_rt5,
                 ccd.cant_rt6             AS cant_rt6,
                 ccd.cant_rt7             AS cant_rt7,
                 0                        AS cobertura
          FROM cc_comuna ccom,
               centro_costo cco,
               centro_costo ccd,
               matrizcc mlod,
               zona z,
               cc_entrega ecco,
               cc_entrega eccd
          WHERE ccom.comuna              = varcomuna2
            AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
            AND ecco.ccosto              = cco.org_lvl_number
            AND ecco.canal_venta         = varcanalventa
            AND eccd.ccosto              = ccd.org_lvl_number
            AND eccd.canal_venta         = varcanalventa
            AND mlod.cc_origen           = cco.org_lvl_number
            AND mlod.cc_despacha         = ccd.org_lvl_number
            AND mlod.cc_despacha         = ccom.ccosto
            AND ccom.ccosto              = z.ccosto (+)
            AND ccom.cod_zona            = z.cod_zona (+)
            AND NVL(cco.desactivado, 0)  = 0
            AND NVL(ccd.desactivado, 0)  = 0
            AND NVL(ccom.desactivado, 0) = 0
            AND NVL(mlod.desactivado, 0) = 0
            AND ccom.tiempo_desp IS NOT NULL
            AND ((mlod.cod_tiporigen IN (2) AND varcanalventa = 23)
              OR (mlod.cod_tiporigen IN (2) AND mlod.cc_origen IN (SELECT org_lvl_number FROM centro_costo WHERE org_is_store = 'T'))
                )
            AND mlod.cc_despacha IN (SELECT org_lvl_number FROM (SELECT cc.org_lvl_number
                                     FROM cc_comuna ccom, centro_costo cc, cc_entrega ecc
                                     WHERE ccom.comuna              = varcomuna2
                                       AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
                                       AND ecc.ccosto               = cc.org_lvl_number
                                       AND ecc.canal_venta          = varcanalventa
                                       AND ccom.ccosto              = cc.org_lvl_number
                                       AND NVL(cc.desactivado, 0)   = 0
                                       AND NVL(ccom.desactivado, 0) = 0
                                       AND cc.despacha              = 1
                                       AND NVL(cc.cant_desp, 0)     > 0
                                       AND ccom.tiempo_desp IS NOT NULL
                                       AND (cc.org_lvl_number = vardespachacc OR ecc.cod_tipocc IN ('H', 'HI'))
                                     ORDER BY CASE
                                                WHEN cc.org_is_store = 'T' AND cc.org_lvl_number = CDSTGO THEN -1000000
                                                WHEN cc.org_is_store = 'T' THEN -cc.cant_desp
                                                ELSE 0
                                              END,
                                              CASE
                                                WHEN cc.org_lvl_number = vardespachacc THEN -1
                                                ELSE 0
                                              END,
                                              CASE
                                                WHEN ecc.cod_tipocc = 'HI' THEN -cc.cant_desp*1000
                                                WHEN ecc.cod_tipocc = 'H'  THEN -cc.cant_desp
                                                ELSE 0
                                              END))
          UNION
          --CC que despachan alimentados por otros CC (cc_tienda)
          SELECT DISTINCT seq,
                 varcanalventa            AS "codigoCanalVenta",
                 ccom.comuna              AS "comunaDespacho",
                 NULL                     AS "regionDespacho",
                 0                        AS "codigoTransporte",
                 vartipostock             AS "tipoStock",
                 ''                       AS "origenCC",
                 NVL(vardespachacc, '')   AS "despachaCC",
                 cct.ccosto               AS "origenCCLod",
                 cct.tienda               AS "despachaCCLod",
                 0                        AS "viaCCLod",
                 ecco.cod_tipocc          AS tipocc_origen,
                 eccd.cod_tipocc          AS tipocc_despacha,
                 ''                       AS tipocc_via,
                 cco.tpo_procesoprod      AS "tiempoProcesoOrigen",
                 cct.tiempo_desp          AS "tiempoViajeOrigen",
                 0                        AS "tiempoProcesoCCVia",
                 0                        AS "tiempoViajeCCVia",
                 ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                 ccom.tiempo_desp         AS "tiempoViajeDespacho",
                 ccom.frec_abastece       AS "frecuenciaViaje",
                 cco.dias_despacho        AS diasccorigen,
                 NVL(z.dias_despzona, ccd.dias_despacho) AS "diasCCDespacha",
                 ccd.dias_despacho        AS diasccvia,
                 ccd.cant_desp            AS "cantidadDespachada",
                 0                        AS "prioridad",
                 ''                       AS codproveedor,
                 0                        AS tpo_prov_proceso,
                 0                        AS tpo_prov_viaje,
                 ''                       AS dias_prov_despacha,
                 0                        AS cap_prov_diaria,
                 cco.recibe_psala         AS cc_origen_tipo_sala,
                 cco.recibe_pconst        AS cc_origen_tipo_const,
                 ccd.recibe_psala         AS cc_despacha_tipo_sala,
                 ccd.recibe_pconst        AS cc_despacha_tipo_const,
                 ''                       AS cc_via_tipo_sala,
                 ''                       AS cc_via_tipo_const,
                 ccd.express              AS serv_express,
                 ccd.cant_express         AS cap_desp_express,
                 ccd.hora_corte           AS hora_corte,
                 ''                       AS ciudadDespacho,
                 ''                       AS regionlod,
                 ''                       AS ciudadlod,
                 ''                       AS comunalod,
                 ''                       AS nombreTienda,
                 ''                       AS direccionTienda,
                 ccd.tienda_rt            AS tienda_rt,
                 ccd.cant_rt              AS cant_rt,
                 ccd.dias_rt              AS dias_rt,
                 ccd.hora_corte_rt        AS hora_corte_rt,
                 ccd.cant_desp1           AS cant_desp1,
                 ccd.cant_desp2           AS cant_desp2,
                 ccd.cant_desp3           AS cant_desp3,
                 ccd.cant_desp4           AS cant_desp4,
                 ccd.cant_desp5           AS cant_desp5,
                 ccd.cant_desp6           AS cant_desp6,
                 ccd.cant_desp7           AS cant_desp7,
                 ccd.cant_rt1             AS cant_rt1,
                 ccd.cant_rt2             AS cant_rt2,
                 ccd.cant_rt3             AS cant_rt3,
                 ccd.cant_rt4             AS cant_rt4,
                 ccd.cant_rt5             AS cant_rt5,
                 ccd.cant_rt6             AS cant_rt6,
                 ccd.cant_rt7             AS cant_rt7,
                 0                        AS cobertura
          FROM cc_comuna ccom,
               centro_costo cco,
               centro_costo ccd,
               cc_tienda cct,
               zona z,
               cc_entrega ecco,
               cc_entrega eccd
          WHERE ccom.comuna              = varcomuna2
            AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
            AND ecco.ccosto              = cco.org_lvl_number
            AND ecco.canal_venta         = varcanalventa
            AND eccd.ccosto              = ccd.org_lvl_number
            AND eccd.canal_venta         = varcanalventa
            AND (ccd.org_lvl_number      = vardespachacc OR eccd.cod_tipocc IN ('H', 'HI')) --H(I) -> cc_origen (!)= cc_despacha
            AND cct.ccosto               = cco.org_lvl_number
            AND cct.tienda               = ccd.org_lvl_number
            AND cct.tienda               = ccom.ccosto
            AND ccom.ccosto              = z.ccosto (+)
            AND ccom.cod_zona            = z.cod_zona (+)
            AND NVL(cco.desactivado, 0)  = 0
            AND NVL(ccd.desactivado, 0)  = 0
            AND NVL(ccom.desactivado, 0) = 0
            AND NVL(cct.desactivado, 0)  = 0
            AND ccom.tiempo_desp IS NOT NULL
            AND ccd.org_lvl_number IN (SELECT org_lvl_number FROM (SELECT cc.org_lvl_number
                                       FROM cc_comuna ccom, centro_costo cc, cc_entrega ecc
                                       WHERE ccom.comuna              = varcomuna2
                                         AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
                                         AND ecc.ccosto               = cc.org_lvl_number
                                         AND ecc.canal_venta          = varcanalventa
                                         AND ccom.ccosto              = cc.org_lvl_number
                                         AND NVL(cc.desactivado, 0)   = 0
                                         AND NVL(ccom.desactivado, 0) = 0
                                         AND cc.despacha              = 1
                                         AND NVL(cc.cant_desp, 0)     > 0
                                         AND ccom.tiempo_desp IS NOT NULL
                                         AND (cc.org_lvl_number = vardespachacc OR ecc.cod_tipocc IN ('H', 'HI'))
                                       ORDER BY CASE
                                                  WHEN cc.org_is_store = 'T' AND cc.org_lvl_number = CDSTGO THEN -1000000
                                                  WHEN cc.org_is_store = 'T' THEN -cc.cant_desp
                                                  ELSE 0
                                                END,
                                                CASE
                                                  WHEN cc.org_lvl_number = vardespachacc THEN -1
                                                  ELSE 0
                                                END,
                                                CASE
                                                  WHEN ecc.cod_tipocc  = 'HI' THEN -cc.cant_desp*1000
                                                  WHEN ecc.cod_tipocc  = 'H'  THEN -cc.cant_desp
                                                  ELSE 0
                                                END))
/*LA VIA NO SE USA          UNION
          --CC que despachan alimentados por otros CC (cc_tienda) con sus CCS relacionados (no se muestran los CCI)
          SELECT DISTINCT seq,
                 varcanalventa            AS "codigoCanalVenta",
                 ccom.comuna              AS "comunaDespacho",
                 NULL                     AS "regionDespacho",
                 0                        AS "codigoTransporte",
                 vartipostock             AS "tipoStock",
                 ''                       AS "origenCC",
                 NVL(vardespachacc, '')   AS "despachaCC",
                 mlod.cc_origen           AS "origenCCLod",
                 cct.tienda               AS "despachaCCLod",
                 cct.ccosto               AS "viaCCLod",
                 cco.cod_tipocc           AS tipocc_origen,
                 ccd.cod_tipocc           AS tipocc_despacha,
                 ccv.cod_tipocc           AS tipocc_via,
                 0                        AS "tiempoProcesoOrigen", --Se define que el CCS/CCI se demora 0 d?as
                 0                        AS "tiempoViajeOrigen",   --Se define que el CCS/CCI se demora 0 d?as
                 ccv.tpo_procesoprod      AS "tiempoProcesoCCVia",
                 cct.tiempo_desp          AS "tiempoViajeCCVia",
                 ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                 ccom.tiempo_desp         AS "tiempoViajeDespacho",
                 ccom.frec_abastece       AS "frecuenciaViaje",
                 cco.dias_despacho        AS diasccorigen,
                 NVL(z.dias_despzona, ccd.dias_despacho) AS "diasCCDespacha",
                 ccv.dias_despacho        AS diasccvia,
                 ccd.cant_desp            AS "cantidadDespachada",
                 0                        AS "prioridad",
                 ''                       AS codproveedor,
                 0                        AS tpo_prov_proceso,
                 0                        AS tpo_prov_viaje,
                 ''                       AS dias_prov_despacha,
                 0                        AS cap_prov_diaria,
                 cco.recibe_psala         AS cc_origen_tipo_sala,
                 cco.recibe_pconst        AS cc_origen_tipo_const,
                 ccd.recibe_psala         AS cc_despacha_tipo_sala,
                 ccd.recibe_pconst        AS cc_despacha_tipo_const,
                 ccv.recibe_psala         AS cc_via_tipo_sala,
                 ccv.recibe_pconst        AS cc_via_tipo_const,
                 ccd.express              AS serv_express,
                 ccd.cant_express         AS cap_desp_express,
                 ccd.hora_corte           AS hora_corte,
                 ''                       AS ciudadDespacho,
                 ''                       AS regionlod,
                 ''                       AS ciudadlod,
                 ''                       AS comunalod,
                 ''                       AS nombreTienda,
                 ''                       AS direccionTienda,
                 ccd.tienda_rt            AS tienda_rt,
                 ccd.cant_rt              AS cant_rt,
                 ccd.dias_rt              AS dias_rt,
                 ccd.hora_corte_rt        AS hora_corte_rt,
                 ccd.cant_desp1           AS cant_desp1,
                 ccd.cant_desp2           AS cant_desp2,
                 ccd.cant_desp3           AS cant_desp3,
                 ccd.cant_desp4           AS cant_desp4,
                 ccd.cant_desp5           AS cant_desp5,
                 ccd.cant_desp6           AS cant_desp6,
                 ccd.cant_desp7           AS cant_desp7,
                 ccd.cant_rt1             AS cant_rt1,
                 ccd.cant_rt2             AS cant_rt2,
                 ccd.cant_rt3             AS cant_rt3,
                 ccd.cant_rt4             AS cant_rt4,
                 ccd.cant_rt5             AS cant_rt5,
                 ccd.cant_rt6             AS cant_rt6,
                 ccd.cant_rt7             AS cant_rt7
          FROM cc_comuna ccom,
               centro_costo cco,
               centro_costo ccd,
               centro_costo ccv,
               cc_tienda cct,
               matrizcc mlod,
               zona z
          WHERE ccom.comuna              = varcomuna2
            AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
--            AND (ccd.org_lvl_number      = vardespachacc OR ccd.cod_tipocc IN ('H', 'HI'))--HI -> cc_origen = cc_despacha
            AND (ccd.org_lvl_number = vardespachacc
             OR (varcanalventa = 23 AND ccd.cod_tipocc_internet IN ('H', 'HI'))
             OR (varcanalventa = 25 AND ccd.cod_tipocc_ssee     IN ('H', 'HI'))
             OR (varcanalventa = 26 AND ccd.cod_tipocc_uxpos    IN ('H', 'HI'))
             OR (varcanalventa = 27 AND ccd.cod_tipocc_vat      IN ('H', 'HI'))
                )
            AND mlod.cc_origen           = cco.org_lvl_number
            AND mlod.cc_despacha         = ccv.org_lvl_number
            AND cct.ccosto               = ccv.org_lvl_number
            AND cct.tienda               = ccd.org_lvl_number
            AND cct.tienda               = ccom.ccosto
            AND ccom.ccosto              = z.ccosto (+)
            AND ccom.cod_zona            = z.cod_zona (+)
            AND NVL(cco.desactivado, 0)  = 0
            AND NVL(ccd.desactivado, 0)  = 0
            AND NVL(ccv.desactivado, 0)  = 0
            AND NVL(ccom.desactivado, 0) = 0
            AND NVL(cct.desactivado, 0)  = 0
            AND NVL(mlod.desactivado, 0) = 0
            AND ccom.tiempo_desp IS NOT NULL
            AND ((mlod.cod_tiporigen IN (2) AND varcanalventa = 23)
              OR (mlod.cod_tiporigen IN (2) AND mlod.cc_origen IN (SELECT org_lvl_number FROM centro_costo WHERE org_is_store = 'T'))
                )
            AND ccd.org_lvl_number IN (SELECT org_lvl_number FROM (SELECT cc.org_lvl_number
                                       FROM cc_comuna ccom, centro_costo cc
                                       WHERE ccom.comuna              = varcomuna2
                                         AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
                                         AND ccom.ccosto              = cc.org_lvl_number
                                         AND NVL(cc.desactivado, 0)   = 0
                                         AND NVL(ccom.desactivado, 0) = 0
                                         AND cc.despacha              = 1
                                         AND NVL(cc.cant_desp, 0)     > 0
                                         AND ccom.tiempo_desp IS NOT NULL
--                                         AND (cc.org_lvl_number = vardespachacc OR cc.cod_tipocc IN ('H', 'HI'))
                                         AND (cc.org_lvl_number = vardespachacc
                                          OR (varcanalventa = 23 AND cc.cod_tipocc_internet IN ('H', 'HI'))
                                          OR (varcanalventa = 25 AND cc.cod_tipocc_ssee     IN ('H', 'HI'))
                                          OR (varcanalventa = 26 AND cc.cod_tipocc_uxpos    IN ('H', 'HI'))
                                          OR (varcanalventa = 27 AND cc.cod_tipocc_vat      IN ('H', 'HI'))
                                             )
                                       ORDER BY CASE
                                                  WHEN cc.org_is_store = 'T' AND cc.org_lvl_number = CDSTGO THEN -1000000
                                                  WHEN cc.org_is_store = 'T' THEN -cc.cant_desp
                                                  ELSE 0
                                                END,
                                                CASE
                                                  WHEN cc.org_lvl_number = vardespachacc THEN -1
                                                  ELSE 0
                                                END,
                                                CASE
                                                  WHEN cc.cod_tipocc = 'HI' THEN -cc.cant_desp*1000
                                                  WHEN cc.cod_tipocc = 'H'  THEN -cc.cant_desp
                                                  ELSE 0
                                                END))*/
/*            UNION
          --CC con servicio RT (se disponibilizan las tiendas RT)
          SELECT DISTINCT seq,
                 varcanalventa            AS "codigoCanalVenta",
                 varcomuna2               AS "comunaDespacho",
                 NULL                     AS "regionDespacho",
                 0                        AS "codigoTransporte",
                 vartipostock             AS "tipoStock",
                 ''                       AS "origenCC",
                 NVL(vardespachacc, '')   AS "despachaCC",
                 ccd.org_lvl_number       AS "origenCCLod",
                 ccd.org_lvl_number       AS "despachaCCLod",
                 0                        AS "viaCCLOD",
                 ccd.cod_tipocc           AS tipocc_origen,
                 ccd.cod_tipocc           AS tipocc_despacha,
                 ''                       AS tipocc_via,
                 0                        AS "tiempoProcesoOrigen", --Se define que el CCS/CCI se demora 0 dias
                 0                        AS "tiempoViajeOrigen",   --Se define que el CCS/CCI se demora 0 dias
                 0                        AS "tiempoProcesoCCVia",
                 0                        AS "tiempoViajeCCVia",
                 ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                 0                        AS "tiempoViajeDespacho",
                 0                        AS "frecuenciaViaje",
                 ''                       AS diasccorigen,
                 ccd.dias_despacho        AS "diasCCDespacha",
                 ccd.dias_despacho        AS diasccvia,
                 ccd.cant_desp            AS "cantidadDespachada",
                 0                        AS "prioridad",
                 ''                       AS codproveedor,
                 0                        AS tpo_prov_proceso,
                 0                        AS tpo_prov_viaje,
                 ''                       AS dias_prov_despacha,
                 0                        AS cap_prov_diaria,
                 ccd.recibe_psala         AS cc_origen_tipo_sala,
                 ccd.recibe_pconst        AS cc_origen_tipo_const,
                 ''                       AS cc_despacha_tipo_sala,
                 ''                       AS cc_despacha_tipo_const,
                 ''                       AS cc_via_tipo_sala,
                 ''                       AS cc_via_tipo_const,
                 ccd.express              AS serv_express,
                 ccd.cant_express         AS cap_desp_express,
                 ccd.hora_corte_rt        AS hora_corte,
                 ''                       AS ciudadDespacho,
                 ''                       AS regionlod,
                 ''                       AS ciudadlod,
                 ''                       AS comunalod,
                 ''                       AS nombreTienda,
                 ''                       AS direccionTienda,
                 cc.tienda_rt             AS tienda_rt,
                 cc.cant_rt               AS cant_rt,
                 cc.dias_rt               AS dias_rt,
                 cc.hora_corte_rt         AS hora_corte_rt,
                 ccd.cant_desp1           AS cant_desp1,
                 ccd.cant_desp2           AS cant_desp2,
                 ccd.cant_desp3           AS cant_desp3,
                 ccd.cant_desp4           AS cant_desp4,
                 ccd.cant_desp5           AS cant_desp5,
                 ccd.cant_desp6           AS cant_desp6,
                 ccd.cant_desp7           AS cant_desp7,
                 ccd.cant_rt1             AS cant_rt1,
                 ccd.cant_rt2             AS cant_rt2,
                 ccd.cant_rt3             AS cant_rt3,
                 ccd.cant_rt4             AS cant_rt4,
                 ccd.cant_rt5             AS cant_rt5,
                 ccd.cant_rt6             AS cant_rt6,
                 ccd.cant_rt7             AS cant_rt7
          FROM centro_costo ccd
          WHERE cc.bas_addr_2          = varcomuna2
            AND NVL(cc.desactivado, 0) = 0
            AND cc.tienda_rt           = 1
            AND NVL(cc.cant_rt, 0)     > 0
--            AND (cc.org_lvl_number     = vardespachacc OR cc.cod_tipocc IN ('HI')) --H(I) -> cc_origen (!)= cc_despacha
            AND (cc.org_lvl_number = vardespachacc
             OR (varcanalventa = 23 AND ccd.cod_tipocc_internet IN ('HI'))
             OR (varcanalventa = 25 AND ccd.cod_tipocc_ssee     IN ('HI'))
             OR (varcanalventa = 26 AND ccd.cod_tipocc_uxpos    IN ('HI'))
             OR (varcanalventa = 27 AND ccd.cod_tipocc_vat      IN ('HI'))
                )
          UNION
          --CC con servicio RT  alimentados por otros CC (cc_tienda)
          SELECT DISTINCT seq,
                 varcanalventa            AS "codigoCanalVenta",
                 varcomuna2               AS "comunaDespacho",
                 NULL                     AS "regionDespacho",
                 0                        AS "codigoTransporte",
                 vartipostock             AS "tipoStock",
                 ''                       AS "origenCC",
                 NVL(vardespachacc, '')   AS "despachaCC",
                 cct.ccosto               AS "origenCCLod",
                 cct.tienda               AS "despachaCCLod",
                 0                        AS "viaCCLod",
                 cco.cod_tipocc           AS tipocc_origen,
                 ccd.cod_tipocc           AS tipocc_despacha,
                 ''                       AS tipocc_via,
                 cco.tpo_procesoprod      AS "tiempoProcesoOrigen",
                 cct.tiempo_desp          AS "tiempoViajeOrigen",
                 0                        AS "tiempoProcesoCCVia",
                 0                        AS "tiempoViajeCCVia",
                 ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                 0                        AS "tiempoViajeDespacho",
                 0                        AS "frecuenciaViaje",
                 cco.dias_despacho        AS diasccorigen,
                 ccd.dias_despacho        AS "diasCCDespacha",
                 ccd.dias_despacho        AS diasccvia,
                 ccd.cant_desp            AS "cantidadDespachada",
                 0                        AS "prioridad",
                 ''                       AS codproveedor,
                 0                        AS tpo_prov_proceso,
                 0                        AS tpo_prov_viaje,
                 ''                       AS dias_prov_despacha,
                 0                        AS cap_prov_diaria,
                 cco.recibe_psala         AS cc_origen_tipo_sala,
                 cco.recibe_pconst        AS cc_origen_tipo_const,
                 ccd.recibe_psala         AS cc_despacha_tipo_sala,
                 ccd.recibe_pconst        AS cc_despacha_tipo_const,
                 ''                       AS cc_via_tipo_sala,
                 ''                       AS cc_via_tipo_const,
                 ccd.express              AS serv_express,
                 ccd.cant_express         AS cap_desp_express,
                 ccd.hora_corte_rt        AS hora_corte,
                 ''                       AS ciudadDespacho,
                 ''                       AS regionlod,
                 ''                       AS ciudadlod,
                 ''                       AS comunalod,
                 ''                       AS nombreTienda,
                 ''                       AS direccionTienda,
                 ccd.tienda_rt            AS tienda_rt,
                 ccd.cant_rt              AS cant_rt,
                 ccd.dias_rt              AS dias_rt,
                 ccd.hora_corte_rt        AS hora_corte_rt,
                 ccd.cant_desp1           AS cant_desp1,
                 ccd.cant_desp2           AS cant_desp2,
                 ccd.cant_desp3           AS cant_desp3,
                 ccd.cant_desp4           AS cant_desp4,
                 ccd.cant_desp5           AS cant_desp5,
                 ccd.cant_desp6           AS cant_desp6,
                 ccd.cant_desp7           AS cant_desp7,
                 ccd.cant_rt1             AS cant_rt1,
                 ccd.cant_rt2             AS cant_rt2,
                 ccd.cant_rt3             AS cant_rt3,
                 ccd.cant_rt4             AS cant_rt4,
                 ccd.cant_rt5             AS cant_rt5,
                 ccd.cant_rt6             AS cant_rt6,
                 ccd.cant_rt7             AS cant_rt7
          FROM centro_costo cco,
               centro_costo ccd,
               cc_tienda cct
          WHERE ccd.bas_addr_2          = varcomuna2
            AND ccd.tienda_rt           = 1
            AND NVL(ccd.cant_rt, 0)     > 0
            AND cct.ccosto              = cco.org_lvl_number
            AND cct.tienda              = ccd.org_lvl_number
            AND NVL(cco.desactivado, 0) = 0
            AND NVL(ccd.desactivado, 0) = 0
            AND NVL(cct.desactivado, 0) = 0
--            AND (ccd.org_lvl_number     = vardespachacc OR ccd.cod_tipocc IN ('H', 'HI')) --H(I) -> cc_origen (!)= cc_despacha
            AND (ccd.org_lvl_number = vardespachacc
             OR (varcanalventa = 23 AND ccd.cod_tipocc_internet IN ('H', 'HI'))
             OR (varcanalventa = 25 AND ccd.cod_tipocc_ssee     IN ('H', 'HI'))
             OR (varcanalventa = 26 AND ccd.cod_tipocc_uxpos    IN ('H', 'HI'))
             OR (varcanalventa = 27 AND ccd.cod_tipocc_vat      IN ('H', 'HI'))
                )
*/
          ;
          IF SQL%ROWCOUNT = 0 THEN
            --Para Consulta Disp se envía SIN-COBERTURA (debe estar activo
            IF (varsin_cobertura = 1 AND (varorigencc IS NULL OR varorigencc = '' OR varorigencc = 0)) THEN
              INSERT INTO tmp_lod
              --Se evita error, se envía registro en blanco.
              SELECT seq,
                     varcanalventa           AS "codigoCanalVenta",
                     varcomuna2              AS "comunaDespacho",
                     varregion               AS "regionDespacho",
                     0                       AS "codigoTransporte",
                     vartipostock            AS "tipoStock",
                     varorigencc             AS "origenCC",
                     NVL(vardespachacc, '')  AS "despachaCC",
                     ''                      AS "origenCCLod",
                     ''                      AS "despachaCCLod",
                     ''                      AS "viaCCLOD",
                     ''                      AS tipocc_origen,
                     ''                      AS tipocc_despacha,
                     ''                      AS tipocc_via,
                     0                       AS "tiempoProcesoOrigen",
                     0                       AS "tiempoViajeOrigen",
                     0                       AS "tiempoProcesoCCVia",
                     0                       AS "tiempoViajeCCVia",
                     0                       AS "tiempoProcesoDespacho",
                     0                       AS "tiempoViajeDespacho",
                     0                       AS "frecuenciaViaje",
                     ''                      AS diasccorigen,
                     ''                      AS "diasCCDespacha",
                     ''                      AS diasccvia,
                     0                       AS "cantidadDespachada",
                     0                       AS "prioridad",
                     varcodproveedor         AS codproveedor,
                     0                       AS tpo_prov_proceso,
                     0                       AS tpo_prov_viaje,
                     ''                      AS dias_prov_despacha,
                     0                       AS cap_prov_diaria,
                     ''                      AS cc_origen_tipo_sala,
                     ''                      AS cc_origen_tipo_const,
                     ''                      AS cc_despacha_tipo_sala,
                     ''                      AS cc_despacha_tipo_const,
                     ''                      AS cc_via_tipo_sala,
                     ''                      AS cc_via_tipo_const,
                     ''                      AS serv_express,
                     0                       AS cap_desp_express,
                     ''                      AS hora_corte,
                     varciudad               AS ciudadDespacho,
                     ''                      AS regionlod,
                     ''                      AS ciudadlod,
                     ''                      AS comunalod,
                     ''                      AS nombreTienda,
                     ''                      AS direccionTienda,
                     0                       AS tienda_rt,
                     0                       AS cant_rt,
                     ''                      AS dias_rt,
                     ''                      AS hora_corte_rt,
                     null                    AS cant_desp1,
                     null                    AS cant_desp2,
                     null                    AS cant_desp3,
                     null                    AS cant_desp4,
                     null                    AS cant_desp5,
                     null                    AS cant_desp6,
                     null                    AS cant_desp7,
                     null                    AS cant_rt1,
                     null                    AS cant_rt2,
                     null                    AS cant_rt3,
                     null                    AS cant_rt4,
                     null                    AS cant_rt5,
                     null                    AS cant_rt6,
                     null                    AS cant_rt7,
                     1                       AS cobertura
              FROM dual
              ;
            ELSE
              out_status_code := 1;
              out_status_msg := 'DAD: No hay Datos para Estos Registros '
                              || ' ' || strcomuna || ': ' || varcomuna
                              || ' OrigenCC: '            || varorigencc
                              || ' DespachaCC: '          || vardespachacc
                              || ' CanalVenta: '          || varcanalventa;
              RETURN;
            END IF;
          END IF;
        EXCEPTION
          WHEN DUP_VAL_ON_INDEX THEN
            NULL;
          WHEN OTHERS THEN
            out_status_code := 1;
            out_status_msg := SQLERRM;
            RETURN;
        END;

--***** Stock Sodimac con CC_Origen = CC_Despacha
      ELSIF (vartipostock = 'S' AND varorigencc = vardespachacc AND vardespachacc IS NOT NULL) THEN
        BEGIN
          INSERT INTO tmp_lod
          --CC que despachan
          SELECT DISTINCT seq,
                 varcanalventa            AS "codigoCanalVenta",
                 ccom.comuna              AS "comunaDespacho",
                 NULL                     AS "regionDespacho",
                 0                        AS "codigoTransporte",
                 vartipostock             AS "tipoStock",
                 varorigencc              AS "origenCC",
                 vardespachacc            AS "despachaCC",
                 ccd.org_lvl_number       AS "origenCCLod",
                 ccd.org_lvl_number       AS "despachaCCLod",
                 0                        AS "viaCCLOD",
                eccd.cod_tipocc          AS tipocc_origen,
                 eccd.cod_tipocc          AS tipocc_despacha,
                 ''                       AS tipocc_via,
                 0                        AS "tiempoProcesoOrigen", --Se define que el CCS/CCI se demora 0 dias
                 0                        AS "tiempoViajeOrigen",   --Se define que el CCS/CCI se demora 0 dias
                 0                        AS "tiempoProcesoCCVia",
                 0                        AS "tiempoViajeCCVia",
                 ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                 ccom.tiempo_desp         AS "tiempoViajeDespacho",
                 ccom.frec_abastece       AS "frecuenciaViaje",
                 ''                       AS diasccorigen,
                 NVL(z.dias_despzona, ccd.dias_despacho) AS "diasCCDespacha",
                 ccd.dias_despacho        AS diasccvia,
                 ccd.cant_desp            AS "cantidadDespachada",
                 0                        AS "prioridad",
                 ''                       AS codproveedor,
                 0                        AS tpo_prov_proceso,
                 0                        AS tpo_prov_viaje,
                 ''                       AS dias_prov_despacha,
                 0                        AS cap_prov_diaria,
                 ccd.recibe_psala         AS cc_origen_tipo_sala,
                 ccd.recibe_pconst        AS cc_origen_tipo_const,
                 ''                       AS cc_despacha_tipo_sala,
                 ''                       AS cc_despacha_tipo_const,
                 ''                       AS cc_via_tipo_sala,
                 ''                       AS cc_via_tipo_const,
                 ccd.express              AS serv_express,
                 ccd.cant_express         AS cap_desp_express,
                 ccd.hora_corte           AS hora_corte,
                 ''                       AS ciudadDespacho,
                 ''                       AS regionlod,
                 ''                       AS ciudadlod,
                 ''                       AS comunalod,
                 ''                       AS nombreTienda,
                 ''                       AS direccionTienda,
                 ccd.tienda_rt            AS tienda_rt,
                 ccd.cant_rt              AS cant_rt,
                 ccd.dias_rt              AS dias_rt,
                 ccd.hora_corte_rt        AS hora_corte_rt,
                 ccd.cant_desp1           AS cant_desp1,
                 ccd.cant_desp2           AS cant_desp2,
                 ccd.cant_desp3           AS cant_desp3,
                 ccd.cant_desp4           AS cant_desp4,
                 ccd.cant_desp5           AS cant_desp5,
                 ccd.cant_desp6           AS cant_desp6,
                 ccd.cant_desp7           AS cant_desp7,
                 ccd.cant_rt1             AS cant_rt1,
                 ccd.cant_rt2             AS cant_rt2,
                 ccd.cant_rt3             AS cant_rt3,
                 ccd.cant_rt4             AS cant_rt4,
                 ccd.cant_rt5             AS cant_rt5,
                 ccd.cant_rt6             AS cant_rt6,
                 ccd.cant_rt7             AS cant_rt7,
                 0                        AS cobertura
          FROM cc_comuna ccom,
               centro_costo ccd,
               zona z,
               cc_entrega eccd
          WHERE ccom.comuna              = varcomuna2
            AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
            AND eccd.ccosto              = ccd.org_lvl_number
            AND eccd.canal_venta         = varcanalventa
            AND ccom.ccosto              = vardespachacc
            AND ccom.ccosto              = ccd.org_lvl_number
            AND ccom.ccosto              = z.ccosto (+)
            AND ccom.cod_zona            = z.cod_zona (+)
            AND NVL(ccd.desactivado, 0)  = 0
            AND NVL(ccom.desactivado, 0) = 0
            AND ccom.tiempo_desp IS NOT NULL
          UNION
          --CC que despachan con sus CCS relacionados (no se muestran los CCI)
          SELECT DISTINCT seq,
                 varcanalventa            AS "codigoCanalVenta",
                 ccom.comuna              AS "comunaDespacho",
                 NULL                     AS "regionDespacho",
                 0                        AS "codigoTransporte",
                 vartipostock             AS "tipoStock",
                 varorigencc              AS "origenCC",
                 vardespachacc            AS "despachaCC",
                 mlod.cc_origen           AS "origenCCLod",
                 mlod.cc_despacha         AS "despachaCCLod",
                 0                        AS "viaCCLod",
                 ecco.cod_tipocc          AS tipocc_origen,
                 eccd.cod_tipocc          AS tipocc_despacha,
                 ''                       AS tipocc_via,
                 0                        AS "tiempoProcesoOrigen", --Se define que el CCS/CCI se demora 0 dias
                 0                        AS "tiempoViajeOrigen",   --Se define que el CCS/CCI se demora 0 dias
                 0                        AS "tiempoProcesoCCVia",
                 0                        AS "tiempoViajeCCVia",
                 ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                 ccom.tiempo_desp         AS "tiempoViajeDespacho",
                 ccom.frec_abastece       AS "frecuenciaViaje",
                 cco.dias_despacho        AS diasccorigen,
                 NVL(z.dias_despzona, ccd.dias_despacho) AS "diasCCDespacha",
                 ccd.dias_despacho        AS diasccvia,
                 ccd.cant_desp            AS "cantidadDespachada",
                 0                        AS "prioridad",
                 ''                       AS codproveedor,
                 0                        AS tpo_prov_proceso,
                 0                        AS tpo_prov_viaje,
                 ''                       AS dias_prov_despacha,
                 0                        AS cap_prov_diaria,
                 cco.recibe_psala         AS cc_origen_tipo_sala,
                 cco.recibe_pconst        AS cc_origen_tipo_const,
                 ccd.recibe_psala         AS cc_despacha_tipo_sala,
                 ccd.recibe_pconst        AS cc_despacha_tipo_const,
                 ''                       AS cc_via_tipo_sala,
                 ''                       AS cc_via_tipo_const,
                 ccd.express              AS serv_express,
                 ccd.cant_express         AS cap_desp_express,
                 ccd.hora_corte           AS hora_corte,
                 ''                       AS ciudadDespacho,
                 ''                       AS regionlod,
                 ''                       AS ciudadlod,
                 ''                       AS comunalod,
                 ''                       AS nombreTienda,
                 ''                       AS direccionTienda,
                 ccd.tienda_rt            AS tienda_rt,
                 ccd.cant_rt              AS cant_rt,
                 ccd.dias_rt              AS dias_rt,
                 ccd.hora_corte_rt        AS hora_corte_rt,
                 ccd.cant_desp1           AS cant_desp1,
                 ccd.cant_desp2           AS cant_desp2,
                 ccd.cant_desp3           AS cant_desp3,
                 ccd.cant_desp4           AS cant_desp4,
                 ccd.cant_desp5           AS cant_desp5,
                 ccd.cant_desp6           AS cant_desp6,
                 ccd.cant_desp7           AS cant_desp7,
                 ccd.cant_rt1             AS cant_rt1,
                 ccd.cant_rt2             AS cant_rt2,
                 ccd.cant_rt3             AS cant_rt3,
                 ccd.cant_rt4             AS cant_rt4,
                 ccd.cant_rt5             AS cant_rt5,
                 ccd.cant_rt6             AS cant_rt6,
                 ccd.cant_rt7             AS cant_rt7,
                 0                        AS cobertura
          FROM cc_comuna ccom,
               centro_costo cco,
               centro_costo ccd,
               matrizcc mlod,
               zona z,
               cc_entrega ecco,
               cc_entrega eccd
          WHERE ccom.comuna              = varcomuna2
            AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
            AND ecco.ccosto              = cco.org_lvl_number
            AND ecco.canal_venta         = varcanalventa
            AND eccd.ccosto              = ccd.org_lvl_number
            AND eccd.canal_venta         = varcanalventa
            AND mlod.cc_origen          <> varorigencc
            AND mlod.cc_despacha         = vardespachacc
            AND mlod.cc_origen           = cco.org_lvl_number
            AND mlod.cc_despacha         = ccd.org_lvl_number
            AND mlod.cc_despacha         = ccom.ccosto
            AND ccom.ccosto              = z.ccosto (+)
            AND ccom.cod_zona            = z.cod_zona (+)
            AND NVL(cco.desactivado, 0)  = 0
            AND NVL(ccd.desactivado, 0)  = 0
            AND NVL(ccom.desactivado, 0) = 0
            AND NVL(mlod.desactivado, 0) = 0
            AND ccom.tiempo_desp IS NOT NULL
            AND ((mlod.cod_tiporigen IN (2) AND varcanalventa = 23)
              OR (mlod.cod_tiporigen IN (2) AND mlod.cc_origen IN (SELECT org_lvl_number FROM centro_costo WHERE org_is_store = 'T'))
                )
/*          UNION
          --CC con servicio RT (se disponibilizan las tiendas RT)
          SELECT DISTINCT seq,
                 varcanalventa            AS "codigoCanalVenta",
                 varcomuna2               AS "comunaDespacho",
                 NULL                     AS "regionDespacho",
                 0                        AS "codigoTransporte",
                 vartipostock             AS "tipoStock",
                 varorigencc              AS "origenCC",
                 vardespachacc            AS "despachaCC",
                 ccd.org_lvl_number       AS "origenCCLod",
                 ccd.org_lvl_number       AS "despachaCCLod",
                 0                        AS "viaCCLOD",
                 ccd.cod_tipocc           AS tipocc_origen,
                 ccd.cod_tipocc           AS tipocc_despacha,
                 ''                       AS tipocc_via,
                 0                        AS "tiempoProcesoOrigen", --Se define que el CCS/CCI se demora 0 dias
                 0                        AS "tiempoViajeOrigen",   --Se define que el CCS/CCI se demora 0 dias
                 0                        AS "tiempoProcesoCCVia",
                 0                        AS "tiempoViajeCCVia",
                 ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                 0                        AS "tiempoViajeDespacho",
                 0                        AS "frecuenciaViaje",
                 ''                       AS diasccorigen,
                 ccd.dias_despacho        AS "diasCCDespacha",
                 ccd.dias_despacho        AS diasccvia,
                 ccd.cant_desp            AS "cantidadDespachada",
                 0                        AS "prioridad",
                 ''                       AS codproveedor,
                 0                        AS tpo_prov_proceso,
                 0                        AS tpo_prov_viaje,
                 ''                       AS dias_prov_despacha,
                 0                        AS cap_prov_diaria,
                 ccd.recibe_psala         AS cc_origen_tipo_sala,
                 ccd.recibe_pconst        AS cc_origen_tipo_const,
                 ''                       AS cc_despacha_tipo_sala,
                 ''                       AS cc_despacha_tipo_const,
                 ''                       AS cc_via_tipo_sala,
                 ''                       AS cc_via_tipo_const,
                 ccd.express              AS serv_express,
                 ccd.cant_express         AS cap_desp_express,
                 ccd.hora_corte_rt        AS hora_corte,
                 ''                       AS ciudadDespacho,
                 ''                       AS regionlod,
                 ''                       AS ciudadlod,
                 ''                       AS comunalod,
                 ''                       AS nombreTienda,
                 ''                       AS direccionTienda,
                 ccd.tienda_rt            AS tienda_rt,
                 ccd.cant_rt              AS cant_rt,
                 ccd.dias_rt              AS dias_rt,
                 ccd.hora_corte_rt        AS hora_corte_rt,
                 ccd.cant_desp1           AS cant_desp1,
                 ccd.cant_desp2           AS cant_desp2,
                 ccd.cant_desp3           AS cant_desp3,
                 ccd.cant_desp4           AS cant_desp4,
                 ccd.cant_desp5           AS cant_desp5,
                 ccd.cant_desp6           AS cant_desp6,
                 ccd.cant_desp7           AS cant_desp7,
                 ccd.cant_rt1             AS cant_rt1,
                 ccd.cant_rt2             AS cant_rt2,
                 ccd.cant_rt3             AS cant_rt3,
                 ccd.cant_rt4             AS cant_rt4,
                 ccd.cant_rt5             AS cant_rt5,
                 ccd.cant_rt6             AS cant_rt6,
                 ccd.cant_rt7             AS cant_rt7
          FROM centro_costo cc
          WHERE cc.bas_addr_2          = varcomuna2
            AND NVL(cc.desactivado, 0) = 0
            AND cc.tienda_rt           = 1
            AND NVL(cc.cant_rt, 0)     > 0
--            AND (cc.org_lvl_number     = vardespachacc OR cc.cod_tipocc IN ('HI')) --H(I) -> cc_origen (!)= cc_despacha
            AND (cc.org_lvl_number = vardespachacc
             OR (varcanalventa = 23 AND ccd.cod_tipocc_internet IN ('HI'))
             OR (varcanalventa = 25 AND ccd.cod_tipocc_ssee     IN ('HI'))
             OR (varcanalventa = 26 AND ccd.cod_tipocc_uxpos    IN ('HI'))
             OR (varcanalventa = 27 AND ccd.cod_tipocc_vat      IN ('HI'))
                )
*/
          ;
          IF SQL%ROWCOUNT = 0 THEN
            out_status_code := 1;
            out_status_msg := 'DAD: No hay Datos para Estos Registros '
                            || ' ' || strcomuna || ': ' || varcomuna
                            || ' OrigenCC: '            || varorigencc
                            || ' DespachaCC: '          || vardespachacc
                            || ' CanalVenta: '          || varcanalventa;
            RETURN;
          END IF;
        EXCEPTION
          WHEN DUP_VAL_ON_INDEX THEN
            NULL;
          WHEN OTHERS THEN
            out_status_code := 1;
            out_status_msg := SQLERRM;
            RETURN;
        END;

--***** Stock Sodimac con CC_Origen != CC_Despacha
      ELSIF (vartipostock = 'S' AND varorigencc <> vardespachacc AND varorigencc IS NOT NULL) THEN
        BEGIN
          INSERT INTO tmp_lod
          --CC que despachan con sus CCS/CCI relacionados
          SELECT DISTINCT seq,
                 varcanalventa            AS "codigoCanalVenta",
                 ccom.comuna              AS "comunaDespacho",
                 NULL                     AS "regionDespacho",
                 0                        AS "codigoTransporte",
                 vartipostock             AS "tipoStock",
                 varorigencc              AS "origenCC",
                 vardespachacc            AS "despachaCC",
                 mlod.cc_origen           AS "origenCCLod",
                 mlod.cc_despacha         AS "despachaCCLod",
                 0                        AS "viaCCLod",
                 ecco.cod_tipocc          AS tipocc_origen,
                 eccd.cod_tipocc          AS tipocc_despacha,
                 ''                       AS tipocc_via,
                 0                        AS "tiempoProcesoOrigen", --Se define que el CCS/CCI se demora 0 dias
                 0                        AS "tiempoViajeOrigen",   --Se define que el CCS/CCI se demora 0 dias
                 0                        AS "tiempoProcesoCCVia",
                 0                        AS "tiempoViajeCCVia",
                 ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                 ccom.tiempo_desp         AS "tiempoViajeDespacho",
                 ccom.frec_abastece       AS "frecuenciaViaje",
                 cco.dias_despacho        AS diasccorigen,
                 NVL(z.dias_despzona, ccd.dias_despacho) AS "diasCCDespacha",
                 ccd.dias_despacho        AS diasccvia,
                 ccd.cant_desp            AS "cantidadDespachada",
                 0                        AS "prioridad",
                 ''                       AS codproveedor,
                 0                        AS tpo_prov_proceso,
                 0                        AS tpo_prov_viaje,
                 ''                       AS dias_prov_despacha,
                 0                        AS cap_prov_diaria,
                 cco.recibe_psala         AS cc_origen_tipo_sala,
                 cco.recibe_pconst        AS cc_origen_tipo_const,
                 ccd.recibe_psala         AS cc_despacha_tipo_sala,
                 ccd.recibe_pconst        AS cc_despacha_tipo_const,
                 ''                       AS cc_via_tipo_sala,
                 ''                       AS cc_via_tipo_const,
                 ccd.express              AS serv_express,
                 ccd.cant_express         AS cap_desp_express,
                 ccd.hora_corte           AS hora_corte,
                 ''                       AS ciudadDespacho,
                 ''                       AS regionlod,
                 ''                       AS ciudadlod,
                 ''                       AS comunalod,
                 ''                       AS nombreTienda,
                 ''                       AS direccionTienda,
                 ccd.tienda_rt            AS tienda_rt,
                 ccd.cant_rt              AS cant_rt,
                 ccd.dias_rt              AS dias_rt,
                 ccd.hora_corte_rt        AS hora_corte_rt,
                 ccd.cant_desp1           AS cant_desp1,
                 ccd.cant_desp2           AS cant_desp2,
                 ccd.cant_desp3           AS cant_desp3,
                 ccd.cant_desp4           AS cant_desp4,
                 ccd.cant_desp5           AS cant_desp5,
                 ccd.cant_desp6           AS cant_desp6,
                 ccd.cant_desp7           AS cant_desp7,
                 ccd.cant_rt1             AS cant_rt1,
                 ccd.cant_rt2             AS cant_rt2,
                 ccd.cant_rt3             AS cant_rt3,
                 ccd.cant_rt4             AS cant_rt4,
                 ccd.cant_rt5             AS cant_rt5,
                 ccd.cant_rt6             AS cant_rt6,
                 ccd.cant_rt7             AS cant_rt7,
                 0                        AS cobertura
          FROM cc_comuna ccom,
               centro_costo cco,
               centro_costo ccd,
               matrizcc mlod,
               zona z,
               cc_entrega ecco,
               cc_entrega eccd
          WHERE ccom.comuna              = varcomuna2
            AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
            AND ecco.ccosto              = cco.org_lvl_number
            AND ecco.canal_venta         = varcanalventa
            AND eccd.ccosto              = ccd.org_lvl_number
            AND eccd.canal_venta         = varcanalventa
            AND mlod.cc_origen           = varorigencc
            AND mlod.cc_despacha         = vardespachacc
            AND mlod.cc_origen           = cco.org_lvl_number
            AND mlod.cc_despacha         = ccd.org_lvl_number
            AND mlod.cc_despacha         = ccom.ccosto
            AND ccom.ccosto              = z.ccosto (+)
            AND ccom.cod_zona            = z.cod_zona (+)
            AND NVL(cco.desactivado, 0)  = 0
            AND NVL(ccd.desactivado, 0)  = 0
            AND NVL(ccom.desactivado, 0) = 0
            AND NVL(mlod.desactivado, 0) = 0
            AND ccom.tiempo_desp IS NOT NULL
            AND ((mlod.cod_tiporigen IN (2, 3) AND varcanalventa = 23)
              OR (mlod.cod_tiporigen IN (2)    AND mlod.cc_origen IN (SELECT org_lvl_number FROM centro_costo WHERE org_is_store = 'T'))
                )
          UNION
          --CC que despachan alimentados por otros CC (cc_tienda)
          SELECT DISTINCT seq,
                 varcanalventa            AS "codigoCanalVenta",
                 ccom.comuna              AS "comunaDespacho",
                 NULL                     AS "regionDespacho",
                 0                        AS "codigoTransporte",
                 vartipostock             AS "tipoStock",
                 varorigencc              AS "origenCC",
                 vardespachacc            AS "despachaCC",
                 cct.ccosto               AS "origenCCLod",
                 cct.tienda               AS "despachaCCLod",
                 0                        AS "viaCCLod",
                 ecco.cod_tipocc          AS tipocc_origen,
                 eccd.cod_tipocc          AS tipocc_despacha,
                 ''                       AS tipocc_via,
                 cco.tpo_procesoprod      AS "tiempoProcesoOrigen",
                 cct.tiempo_desp          AS "tiempoViajeOrigen",
                 0                        AS "tiempoProcesoCCVia",
                 0                        AS "tiempoViajeCCVia",
                 ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                 ccom.tiempo_desp         AS "tiempoViajeDespacho",
                 ccom.frec_abastece       AS "frecuenciaViaje",
                 cco.dias_despacho        AS diasccorigen,
                 NVL(z.dias_despzona, ccd.dias_despacho) AS "diasCCDespacha",
                 ccd.dias_despacho        AS diasccvia,
                 ccd.cant_desp            AS "cantidadDespachada",
                 0                        AS "prioridad",
                 ''                       AS codproveedor,
                 0                        AS tpo_prov_proceso,
                 0                        AS tpo_prov_viaje,
                 ''                       AS dias_prov_despacha,
                 0                        AS cap_prov_diaria,
                 cco.recibe_psala         AS cc_origen_tipo_sala,
                 cco.recibe_pconst        AS cc_origen_tipo_const,
                 ccd.recibe_psala         AS cc_despacha_tipo_sala,
                 ccd.recibe_pconst        AS cc_despacha_tipo_const,
                 ''                       AS cc_via_tipo_sala,
                 ''                       AS cc_via_tipo_const,
                 ccd.express              AS serv_express,
                 ccd.cant_express         AS cap_desp_express,
                 ccd.hora_corte           AS hora_corte,
                 ''                       AS ciudadDespacho,
                 ''                       AS regionlod,
                 ''                       AS ciudadlod,
                 ''                       AS comunalod,
                 ''                       AS nombreTienda,
                 ''                       AS direccionTienda,
                 ccd.tienda_rt            AS tienda_rt,
                 ccd.cant_rt              AS cant_rt,
                 ccd.dias_rt              AS dias_rt,
                 ccd.hora_corte_rt        AS hora_corte_rt,
                 ccd.cant_desp1           AS cant_desp1,
                 ccd.cant_desp2           AS cant_desp2,
                 ccd.cant_desp3           AS cant_desp3,
                 ccd.cant_desp4           AS cant_desp4,
                 ccd.cant_desp5           AS cant_desp5,
                 ccd.cant_desp6           AS cant_desp6,
                 ccd.cant_desp7           AS cant_desp7,
                 ccd.cant_rt1             AS cant_rt1,
                 ccd.cant_rt2             AS cant_rt2,
                 ccd.cant_rt3             AS cant_rt3,
                 ccd.cant_rt4             AS cant_rt4,
                 ccd.cant_rt5             AS cant_rt5,
                 ccd.cant_rt6             AS cant_rt6,
                 ccd.cant_rt7             AS cant_rt7,
                 0                        AS cobertura
          FROM cc_comuna ccom,
               centro_costo cco,
               centro_costo ccd,
               cc_tienda cct,
               zona z,
               cc_entrega ecco,
               cc_entrega eccd
          WHERE ccom.comuna              = varcomuna2
          AND ecco.ccosto              = cco.org_lvl_number
            AND ecco.canal_venta         = varcanalventa
            AND eccd.ccosto              = ccd.org_lvl_number
            AND eccd.canal_venta         = varcanalventa
            AND cct.ccosto               = varorigencc
            AND cct.tienda               = vardespachacc
            AND cct.ccosto               = cco.org_lvl_number
            AND cct.tienda               = ccd.org_lvl_number
            AND cct.tienda               = ccom.ccosto
            AND ccom.ccosto              = z.ccosto (+)
            AND ccom.cod_zona            = z.cod_zona (+)
            AND NVL(cco.desactivado, 0)  = 0
            AND NVL(ccd.desactivado, 0)  = 0
            AND NVL(ccom.desactivado, 0) = 0
            AND NVL(cct.desactivado, 0)  = 0
            AND ccom.tiempo_desp IS NOT NULL
/*LA VIA NO SE USA          UNION
          --CC que despachan alimentados por otros CC (cc_tienda) con sus CCS relacionados (no se muestran los CCI)
          SELECT DISTINCT seq,
                 varcanalventa            AS "codigoCanalVenta",
                 ccom.comuna              AS "comunaDespacho",
                 NULL                     AS "regionDespacho",
                 0                        AS "codigoTransporte",
                 vartipostock             AS "tipoStock",
                 varorigencc              AS "origenCC",
                 vardespachacc            AS "despachaCC",
                 mlod.cc_origen           AS "origenCCLod",
                 cct.tienda               AS "despachaCCLod",
                 cct.ccosto               AS "viaCCLod",
                 cco.cod_tipocc           AS tipocc_origen,
                 ccd.cod_tipocc           AS tipocc_despacha,
                 ccv.cod_tipocc           AS tipocc_via,
                 0                        AS "tiempoProcesoOrigen", --Se define que el CCS/CCI se demora 0 dias
                 0                        AS "tiempoViajeOrigen",   --Se define que el CCS/CCI se demora 0 dias
                 ccv.tpo_procesoprod      AS "tiempoProcesoCCVia",
                 cct.tiempo_desp          AS "tiempoViajeCCVia",
                 ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                 ccom.tiempo_desp         AS "tiempoViajeDespacho",
                 ccom.frec_abastece       AS "frecuenciaViaje",
                 cco.dias_despacho        AS diasccorigen,
                 NVL(z.dias_despzona, ccd.dias_despacho) AS "diasCCDespacha",
                 ccv.dias_despacho        AS diasccvia,
                 ccd.cant_desp            AS "cantidadDespachada",
                 0                        AS "prioridad",
                 ''                       AS codproveedor,
                 0                        AS tpo_prov_proceso,
                 0                        AS tpo_prov_viaje,
                 ''                       AS dias_prov_despacha,
                 0                        AS cap_prov_diaria,
                 cco.recibe_psala         AS cc_origen_tipo_sala,
                 cco.recibe_pconst        AS cc_origen_tipo_const,
                 ccd.recibe_psala         AS cc_despacha_tipo_sala,
                 ccd.recibe_pconst        AS cc_despacha_tipo_const,
                 ccv.recibe_psala         AS cc_via_tipo_sala,
                 ccv.recibe_pconst        AS cc_via_tipo_const,
                 ccd.express              AS serv_express,
                 ccd.cant_express         AS cap_desp_express,
                 ccd.hora_corte           AS hora_corte,
                 ''                       AS ciudadDespacho,
                 ''                       AS regionlod,
                 ''                       AS ciudadlod,
                 ''                       AS comunalod,
                 ''                       AS nombreTienda,
                 ''                       AS direccionTienda,
                 ccd.tienda_rt            AS tienda_rt,
                 ccd.cant_rt              AS cant_rt,
                 ccd.dias_rt              AS dias_rt,
                 ccd.hora_corte_rt        AS hora_corte_rt,
                 ccd.cant_desp1           AS cant_desp1,
                 ccd.cant_desp2           AS cant_desp2,
                 ccd.cant_desp3           AS cant_desp3,
                 ccd.cant_desp4           AS cant_desp4,
                 ccd.cant_desp5           AS cant_desp5,
                 ccd.cant_desp6           AS cant_desp6,
                 ccd.cant_desp7           AS cant_desp7,
                 ccd.cant_rt1             AS cant_rt1,
                 ccd.cant_rt2             AS cant_rt2,
                 ccd.cant_rt3             AS cant_rt3,
                 ccd.cant_rt4             AS cant_rt4,
                 ccd.cant_rt5             AS cant_rt5,
                 ccd.cant_rt6             AS cant_rt6,
                 ccd.cant_rt7             AS cant_rt7
          FROM cc_comuna ccom,
               centro_costo cco,
               centro_costo ccd,
               centro_costo ccv,
               cc_tienda cct,
               matrizcc mlod,
               zona z
          WHERE ccom.comuna              = varcomuna2
            AND ccom.canal_venta         = varcanalventa --Cobertura por canal de venta
            AND cct.ccosto               = varorigencc
            AND cct.tienda               = vardespachacc
            AND mlod.cc_origen           = cco.org_lvl_number
            AND mlod.cc_despacha         = ccv.org_lvl_number
            AND cct.ccosto               = ccv.org_lvl_number
            AND cct.tienda               = ccd.org_lvl_number
            AND cct.tienda               = ccom.ccosto
            AND ccom.ccosto              = z.ccosto (+)
            AND ccom.cod_zona            = z.cod_zona (+)
            AND NVL(cco.desactivado, 0)  = 0
            AND NVL(ccd.desactivado, 0)  = 0
            AND NVL(ccv.desactivado, 0)  = 0
            AND NVL(ccom.desactivado, 0) = 0
            AND NVL(cct.desactivado, 0)  = 0
            AND NVL(mlod.desactivado, 0) = 0
            AND ccom.tiempo_desp IS NOT NULL
            AND ((mlod.cod_tiporigen IN (2) AND varcanalventa = 23)
              OR (mlod.cod_tiporigen IN (2) AND mlod.cc_origen IN (SELECT org_lvl_number FROM centro_costo WHERE org_is_store = 'T'))
                )*/
--              AND ccd.cod_tipocc IN ('H', 'HI') --ERROR
/*          UNION
          --CC con servicio RT alimentados por otros CC (cc_tienda)
          SELECT DISTINCT seq,
                 varcanalventa            AS "codigoCanalVenta",
                 varcomuna2               AS "comunaDespacho",
                 NULL                     AS "regionDespacho",
                 0                        AS "codigoTransporte",
                 vartipostock             AS "tipoStock",
                 varorigencc              AS "origenCC",
                 vardespachacc            AS "despachaCC",
                 cct.ccosto               AS "origenCCLod",
                 cct.tienda               AS "despachaCCLod",
                 0                        AS "viaCCLod",
                 cco.cod_tipocc           AS tipocc_origen,
                 ccd.cod_tipocc           AS tipocc_despacha,
                 ''                       AS tipocc_via,
                 cco.tpo_procesoprod      AS "tiempoProcesoOrigen",
                 cct.tiempo_desp          AS "tiempoViajeOrigen",
                 0                        AS "tiempoProcesoCCVia",
                 0                        AS "tiempoViajeCCVia",
                 ccd.tpo_procesoprod      AS "tiempoProcesoDespacho",
                 0                        AS "tiempoViajeDespacho",
                 0                        AS "frecuenciaViaje",
                 cco.dias_despacho        AS diasccorigen,
                 ccd.dias_despacho        AS "diasCCDespacha",
                 ccd.dias_despacho        AS diasccvia,
                 ccd.cant_desp            AS "cantidadDespachada",
                 0                        AS "prioridad",
                 ''                       AS codproveedor,
                 0                        AS tpo_prov_proceso,
                 0                        AS tpo_prov_viaje,
                 ''                       AS dias_prov_despacha,
                 0                        AS cap_prov_diaria,
                 cco.recibe_psala         AS cc_origen_tipo_sala,
                 cco.recibe_pconst        AS cc_origen_tipo_const,
                 ccd.recibe_psala         AS cc_despacha_tipo_sala,
                 ccd.recibe_pconst        AS cc_despacha_tipo_const,
                 ''                       AS cc_via_tipo_sala,
                 ''                       AS cc_via_tipo_const,
                 ccd.express              AS serv_express,
                 ccd.cant_express         AS cap_desp_express,
                 ccd.hora_corte_rt        AS hora_corte,
                 ''                       AS ciudadDespacho,
                 ''                       AS regionlod,
                 ''                       AS ciudadlod,
                 ''                       AS comunalod,
                 ''                       AS nombreTienda,
                 ''                       AS direccionTienda,
                 ccd.tienda_rt            AS tienda_rt,
                 ccd.cant_rt              AS cant_rt,
                 ccd.dias_rt              AS dias_rt,
                 ccd.hora_corte_rt        AS hora_corte_rt,
                 ccd.cant_desp1           AS cant_desp1,
                 ccd.cant_desp2           AS cant_desp2,
                 ccd.cant_desp3           AS cant_desp3,
                 ccd.cant_desp4           AS cant_desp4,
                 ccd.cant_desp5           AS cant_desp5,
                 ccd.cant_desp6           AS cant_desp6,
                 ccd.cant_desp7           AS cant_desp7,
                 ccd.cant_rt1             AS cant_rt1,
                 ccd.cant_rt2             AS cant_rt2,
                 ccd.cant_rt3             AS cant_rt3,
                 ccd.cant_rt4             AS cant_rt4,
                 ccd.cant_rt5             AS cant_rt5,
                 ccd.cant_rt6             AS cant_rt6,
                 ccd.cant_rt7             AS cant_rt7
          FROM centro_costo cco,
               centro_costo ccd,
               cc_tienda cct
          WHERE ccd.bas_addr_2          = varcomuna2
            AND ccd.tienda_rt           = 1
            AND NVL(ccd.cant_rt, 0)     > 0
            AND cct.ccosto              = cco.org_lvl_number
            AND cct.tienda              = ccd.org_lvl_number
            AND NVL(cco.desactivado, 0) = 0
            AND NVL(ccd.desactivado, 0) = 0
            AND NVL(cct.desactivado, 0) = 0
--            AND (ccd.org_lvl_number     = vardespachacc OR ccd.cod_tipocc IN ('H', 'HI')) --H(I) -> cc_origen (!)= cc_despacha
            AND (ccd.org_lvl_number = vardespachacc
             OR (varcanalventa = 23 AND ccd.cod_tipocc_internet IN ('H', 'HI'))
             OR (varcanalventa = 25 AND ccd.cod_tipocc_ssee     IN ('H', 'HI'))
             OR (varcanalventa = 26 AND ccd.cod_tipocc_uxpos    IN ('H', 'HI'))
             OR (varcanalventa = 27 AND ccd.cod_tipocc_vat      IN ('H', 'HI'))
                )
*/
          ;
          IF SQL%ROWCOUNT = 0 THEN
            out_status_code := 1;
            out_status_msg := 'DAD: No hay Datos para Estos Registros '
                            || ' ' || strcomuna || ': ' || varcomuna
                            || ' OrigenCC: '            || varorigencc
                            || ' DespachaCC: '          || vardespachacc
                            || ' CanalVenta: '          || varcanalventa;
            RETURN;
          END IF;
        EXCEPTION
          WHEN DUP_VAL_ON_INDEX THEN
            NULL;
          WHEN OTHERS THEN
            out_status_code := 1;
            out_status_msg := SQLERRM;
            RETURN;
        END;

--***** Criterio de consulta no valdio
      ELSE
        out_status_code := 1;
        out_status_msg := 'DAD: Criterio de consulta no valido ';
        RETURN;
      END IF;
    END LOOP;
  END LOOP;

--* Se prespara cursor de salida con todas las consultas ejecutas
  OPEN  rs FOR
  SELECT codigocanalventa,
         comunadespacho,
         tipostock,
         origencc,
         despachacc,
         origencclod,
         despachacclod,
         viacclod,
         tipocc_origen,
         tipocc_despacha,
         tipocc_via,
         /*tiempoprocesoorigen,
         tiempoviajeorigen,
         tiempoprocesoccvia,
         tiempoviajeccvia,
         tiempoprocesodespacho,
         tiempoviajedespacho,
         frecuenciaviaje,
         diasccorigen,
         diasccdespacha,
         diasccvia,
         cantidaddespachada,*/
         prioridad,
         codproveedor,
         /*tpo_prov_proceso,
         tpo_prov_viaje,
         dias_prov_despacha,
         cap_prov_diaria,*/
         cc_origen_tipo_sala,
         cc_origen_tipo_const,
         cc_despacha_tipo_sala,
         cc_despacha_tipo_const,
         cc_via_tipo_sala,
         cc_via_tipo_const,
         serv_express,
         /*cap_desp_express,
         hora_corte,
         ciudaddespacho,*/
         regionlod,
         ciudadlod,
         comunalod,
         nombretienda,
         direcciontienda,
         regiondespacho,
         tienda_rt,
         /*cant_rt,
         dias_rt,
         hora_corte_rt,*/
         cobertura
         /*cant_desp1,
         cant_desp2,
         cant_desp3,
         cant_desp4,
         cant_desp5,
         cant_desp6,
         cant_desp7,
         cant_rt1,
         cant_rt2,
         cant_rt3,
         cant_rt4,
         cant_rt5,
         cant_rt6,
         cant_rt7*/
  FROM (SELECT DISTINCT codigocanalventa,
               NVL(SUBSTR(comunadespacho, 0, INSTR(comunadespacho, '>')-2), comunadespacho) AS comunadespacho,
               regiondespacho,
               codigotransporte,
               tipostock,
               origencc,
               despachacc,
               origencclod,
               despachacclod,
               viacclod,
               tipocc_origen,
               tipocc_despacha,
               ' '             AS tipocc_via,
               /*tiempoprocesoorigen,
               tiempoviajeorigen,
               tiempoprocesoccvia,
               tiempoviajeccvia,
               tiempoprocesodespacho,
               tiempoviajedespacho,
               frecuenciaviaje,
               NVL(diasccorigen, ' ')            AS diasccorigen,
               NVL(diasccdespacha, ' ')          AS diasccdespacha,
               NVL(diasccvia, ' ')               AS diasccvia,
               cantidaddespachada,*/
               1 AS prioridad,
               NVL(codproveedor, ' ')            AS codproveedor,
               /*tpo_prov_proceso,
               tpo_prov_viaje,
               dias_prov_despacha,
               cap_prov_diaria,*/
               cc_origen_tipo_sala,
               cc_origen_tipo_const,
               NVL(cc_despacha_tipo_sala, ' ')   AS cc_despacha_tipo_sala,
               NVL(cc_despacha_tipo_const, ' ')  AS cc_despacha_tipo_const,
               ' '        AS cc_via_tipo_sala,
               ' '       AS cc_via_tipo_const,
               0 serv_express,
               /*cap_desp_express,
               hora_corte,
               ciudaddespacho,*/
               regionlod,
               ciudadlod,
               comunalod,
               nombretienda,
               direcciontienda,
               tienda_rt,
               /*cant_rt,
               dias_rt,
               hora_corte_rt,*/
               cobertura
               /*cant_desp1,
               cant_desp2,
               cant_desp3,
               cant_desp4,
               cant_desp5,
               cant_desp6,
               cant_desp7,
               cant_rt1,
               cant_rt2,
               cant_rt3,
               cant_rt4,
               cant_rt5,
               cant_rt6,
               cant_rt7*/
        FROM tmp_lod
        WHERE sesion_id = seq) tmp_lod2
  ;
  IF in_process_luw = 'T' THEN
    COMMIT;
  END IF;
END;
END cobertura_consultar;

PROCEDURE flete_calcular
(
    "xml"          IN SYS.XMLTYPE,
    in_process_luw IN CHAR DEFAULT 'T',
    "rs" OUT SYS_REFCURSOR,
    OUT_STATUS_CODE OUT NUMBER,
    OUT_STATUS_MSG OUT VARCHAR)
AS
BEGIN

  DECLARE
    doc dbms_xmldom.DOMDocument;
    node dbms_xmldom.DOMNode;
    hijo dbms_xmldom.DOMNode;
    hijo2 dbms_xmldom.DOMNode;
    nodeList dbms_xmldom.DOMNodeList;
    hijosList dbms_xmldom.DOMNodeList;
    numNodes NUMBER;
    numHijos NUMBER;
    nodoActual dbms_xmldom.DOMNode;
    varCanalVenta          VARCHAR2(4000);
    varComunaDespacho      VARCHAR2(4000);
    varTipoAgrupacionEnvio VARCHAR2(4000);
    varRetiroODespacho     VARCHAR2(4000);
    varIsRecalcular        VARCHAR2(4000);
    varNumeroItem          VARCHAR2(4000);
    varCantidad            VARCHAR2(4000);
    varCodProducto         VARCHAR2(4000);
    varTipoStockProducto   VARCHAR2(4000);
    varCodProveedor        VARCHAR2(4000);
    varOrigenStock         VARCHAR2(4000);
    varTipoOrigenStock     VARCHAR2(4000);
    varOrigenDespacho      VARCHAR2(4000);
    varTipoProducto        VARCHAR2(4000);
    varPrdKilo             VARCHAR2(4000);
    varPrdM3               VARCHAR2(4000);
    varFechaEntrega        DATE;
    varFechaRecepcion      DATE;
    varFechaInicioProceso  DATE;
    varFechaDespacho       DATE;
    varTipoPrecio          VARCHAR2(4000);
    varCodTiendaVenta      VARCHAR2(4000);
    varIdGrupo             VARCHAR2(4000);
    varSubOrden            VARCHAR2(4000);
    varNumOrdenVenta       VARCHAR2(4000);
    varPrioridad           VARCHAR2(4000);
    varSecuencia           VARCHAR2(4000);
    varSaldo               VARCHAR2(4000);
    varSaldoSeguridad      VARCHAR2(4000);
    varDiasFabProveedor    VARCHAR2(4000);
    varOpcional1sku        VARCHAR2(4000);
    varOpcional2sku        VARCHAR2(4000);
    var_seq                NUMBER;
    varFechaDesde          DATE:=SYSDATE-1;
    varFechaHasta          DATE;
    varCodServicio         VARCHAR2(4000);
    varCodRango            VARCHAR2(4000);
    varFechaDespachoDesde  DATE;
    varFechaDespachoHasta  DATE;
    hayfilas number:=0;
    var_tipoAgrupacionEnvio    number;
  BEGIN
--    dbms_output.put_line('*************************************************');
    SELECT SEQ_FLETE.NEXTVAL INTO var_seq FROM DUAL;
    OUT_STATUS_CODE:=0;
    doc            := dbms_xmldom.newDomDocument(xmltype("xml".getclobval()));
    node           := dbms_xmldom.makeNode(doc);
    nodeList       := xslprocessor.selectNodes(node, '/flete','xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"');
    numNodes       := dbms_xmldom.getLength(nodeList);
    FOR rowCounter IN 0..numNodes-1
    LOOP
      nodoActual             := dbms_xmldom.item(nodeList, rowCounter);
      hijo                   := xslprocessor.selectSingleNode(nodoActual,'canalVenta');
      hijo                   := xmldom.getFirstChild(hijo);
      varCanalVenta          := dbms_xmldom.getnodevalue(hijo);
      hijo                   := xslprocessor.selectSingleNode(nodoActual,'comunaDespacho');
      hijo                   := xmldom.getFirstChild(hijo);
      varComunaDespacho      := dbms_xmldom.getnodevalue(hijo);
      hijo                   := xslprocessor.selectSingleNode(nodoActual,'codTiendaVenta');
      hijo                   := xmldom.getFirstChild(hijo);
      varCodTiendaVenta      := dbms_xmldom.getnodevalue(hijo);
      hijo                   := xslprocessor.selectSingleNode(nodoActual,'tipoAgrupacionEnvio');
      hijo                   := xmldom.getFirstChild(hijo);
      varTipoAgrupacionEnvio := dbms_xmldom.getnodevalue(hijo);
      hijo                   := xslprocessor.selectSingleNode(nodoActual,'retiroODespacho');
      hijo                   := xmldom.getFirstChild(hijo);
      varRetiroODespacho     := dbms_xmldom.getnodevalue(hijo);
      hijo                   := xslprocessor.selectSingleNode(nodoActual,'isRecalcular');
      hijo                   := xmldom.getFirstChild(hijo);
      varIsRecalcular        := dbms_xmldom.getnodevalue(hijo);
      hijo                   := xslprocessor.selectSingleNode(nodoActual,'numOrdenVenta');
      hijo                   := xmldom.getFirstChild(hijo);
      varNumOrdenVenta       := dbms_xmldom.getnodevalue(hijo);
      hijo                   := xslprocessor.selectSingleNode(nodoActual,'subOrden');
      hijo                   := xmldom.getFirstChild(hijo);
      varSubOrden            := dbms_xmldom.getnodevalue(hijo);
      hijo                   := xslprocessor.selectSingleNode(nodoActual,'idGrupo');
      hijo                   := xmldom.getFirstChild(hijo);
      varIdGrupo             := dbms_xmldom.getnodevalue(hijo);

      BEGIN
        SELECT tipo_precio
        INTO varTipoPrecio
        FROM canalventa
        WHERE canal_venta = varCanalVenta;
      EXCEPTION
      WHEN OTHERS THEN
        OUT_STATUS_CODE := 1;
        OUT_STATUS_MSG  := 'DAD: No Existen Datos para estos parametros, canal_venta='|| varCanalVenta;
        RETURN;
      END;
      hijosList := xslprocessor.selectNodes(nodoActual, 'items');
      numHijos  := dbms_xmldom.getLength(hijosList);

      IF varTipoAgrupacionEnvio = 1 and varIsRecalcular <> 1 and varRetiroODespacho <> 1 THEN
          FOR rowCT IN 0..numHijos-1
            LOOP
            nodoActual            := dbms_xmldom.item(hijosList, rowCT);
            hijo2                   := xslprocessor.selectSingleNode(nodoActual,'fechaEntrega');
            hijo2                   := xmldom.getFirstChild(hijo2);
            IF varFechaDesde< TO_DATE(dbms_xmldom.getnodevalue(hijo2),'DD/MM/YYYY HH24:MI') THEN
               varFechaDesde := TO_DATE(dbms_xmldom.getnodevalue(hijo2),'DD/MM/YYYY  HH24:MI');
            END IF;
          END LOOP;
      END IF;

      IF varIsRecalcular = 1 and varRetiroODespacho <> 1 THEN
          FOR rowCT IN 0..numHijos-1
            LOOP
            nodoActual            := dbms_xmldom.item(hijosList, rowCT);
            hijo2                   := xslprocessor.selectSingleNode(nodoActual,'fechaDespachoDesde');
            hijo2                   := xmldom.getFirstChild(hijo2);
            IF varFechaDesde<TO_DATE(dbms_xmldom.getnodevalue(hijo2),'DD/MM/YYYY HH24:MI') THEN
              varFechaDesde             := TO_DATE(dbms_xmldom.getnodevalue(hijo2),'DD/MM/YYYY HH24:MI');
            END IF;
          END LOOP;
      END IF;

      FOR rowCT IN 0..numHijos-1
      LOOP
        nodoActual            := dbms_xmldom.item(hijosList, rowCT);
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'numeroItem');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varNumeroItem         := dbms_xmldom.getnodevalue(hijo2);
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'cantidad');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varCantidad           := dbms_xmldom.getnodevalue(hijo2);
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'codProducto');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varCodProducto        := dbms_xmldom.getnodevalue(hijo2);
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'tipoStockProducto');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varTipoStockProducto  := dbms_xmldom.getnodevalue(hijo2);
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'codProveedor');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varCodProveedor       := dbms_xmldom.getnodevalue(hijo2);
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'origenStock');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varOrigenStock        := dbms_xmldom.getnodevalue(hijo2);
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'tipoOrigenStock');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varTipoOrigenStock    := dbms_xmldom.getnodevalue(hijo2);
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'origenDespacho');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varOrigenDespacho     := dbms_xmldom.getnodevalue(hijo2);
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'tipoProducto');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varTipoProducto       := dbms_xmldom.getnodevalue(hijo2);
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'prdKilo');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varPrdKilo            := dbms_xmldom.getnodevalue(hijo2);
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'prdM3');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varPrdM3              := dbms_xmldom.getnodevalue(hijo2);

        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'fechaEntrega');
        hijo2                 := xmldom.getFirstChild(hijo2);

        varFechaEntrega       := TO_DATE(dbms_xmldom.getnodevalue(hijo2),'DD/MM/YYYY HH24:MI');

        IF varTipoAgrupacionEnvio = 1 and varIsRecalcular <> 1  and varRetiroODespacho <> 1  THEN
           varFechaEntrega:=varFechaDesde;
        END IF;

        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'fechaRecepcion');
        hijo2                 := xmldom.getFirstChild(hijo2);
        BEGIN
        varFechaRecepcion     := TO_DATE(dbms_xmldom.getnodevalue(hijo2),'DD/MM/YYYY HH24:MI');
        EXCEPTION when OTHERS THEN
        varFechaRecepcion:=NULL;
        END;
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'fechaInicioProceso');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varFechaInicioProceso := TO_DATE(dbms_xmldom.getnodevalue(hijo2),'DD/MM/YYYY HH24:MI');

        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'fechaDespacho');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varFechaDespacho      := TO_DATE(dbms_xmldom.getnodevalue(hijo2),'DD/MM/YYYY HH24:MI');
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'prioridad');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varPrioridad          := dbms_xmldom.getnodevalue(hijo2);
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'secuencia');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varSecuencia          := dbms_xmldom.getnodevalue(hijo2);
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'saldo');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varSaldo              := dbms_xmldom.getnodevalue(hijo2);
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'saldoSeguridad');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varSaldoSeguridad     := dbms_xmldom.getnodevalue(hijo2);
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'diasFabProveedor');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varDiasFabProveedor   := dbms_xmldom.getnodevalue(hijo2);

        IF varIsRecalcular = 1 THEN
            varFechaEntrega := varFechaDesde;

            hijo2                   := xslprocessor.selectSingleNode(nodoActual,'tipoServicio');
            hijo2                   := xmldom.getFirstChild(hijo2);
            varCodServicio             := dbms_xmldom.getnodevalue(hijo2);

            hijo2                   := xslprocessor.selectSingleNode(nodoActual,'idRango');
            hijo2                   := xmldom.getFirstChild(hijo2);
            varCodRango             := dbms_xmldom.getnodevalue(hijo2);

            hijo2                   := xslprocessor.selectSingleNode(nodoActual,'fechaDespachoDesde');
            hijo2                   := xmldom.getFirstChild(hijo2);
            varFechaDespachoDesde   := TO_DATE(dbms_xmldom.getnodevalue(hijo2),'DD/MM/YYYY HH24:MI');

            hijo2                   := xslprocessor.selectSingleNode(nodoActual,'fechaDespachoHasta');
            hijo2                   := xmldom.getFirstChild(hijo2);
            varFechaDespachoHasta   := TO_DATE(dbms_xmldom.getnodevalue(hijo2),'DD/MM/YYYY HH24:MI');

        END IF;

        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'opcional1sku');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varOpcional1sku       := dbms_xmldom.getnodevalue(hijo2);
        hijo2                 := xslprocessor.selectSingleNode(nodoActual,'opcional2sku');
        hijo2                 := xmldom.getFirstChild(hijo2);
        varOpcional2sku       := dbms_xmldom.getnodevalue(hijo2);

        IF trunc(varFechaEntrega) < TRUNC(SYSDATE) THEN
          OUT_STATUS_CODE := 1;
          OUT_STATUS_MSG  := 'DAD: la fecha de entrega no puede ser menor a hoy';
          RETURN;
        END IF;
          --calculamos las posibles fechas que tiene el servicio dependiendo del centro de costo


         -- ITFFLETESERVICIO2FECHAS(trunc(varFechaEntrega),varOrigenDespacho,var_seq,varRetiroODespacho,varCodProveedor);

          calculo_fecha.servicios_flete_consultar(TRUNC(varFechaEntrega),TRUNC(varFechaDespacho),varOrigenDespacho,var_seq,varRetiroODespacho,varCodProveedor,varComunaDespacho,varCanalVenta);



          BEGIN
          INSERT
          INTO TMP_CALCULAFLETE
            (
              ID,
              CANALVENTA,
              NUMORDENVENTA,
              TIPOAGRUPACIONENVIO,
              SUBORDEN,
              CODTIENDAVENTA,
              IDGRUPO,
              FECHAINICIOPROCESO,
              NUMEROITEM,
              CODPRODUCTO,
              CANTIDAD,
              TIPOSTOCK,
              CODPROVEEDOR,
              CENCOSORIGENSTOCK,
              TIPOORIGENSTOCK,
              CENCOSDESPACHO,
              PRIORIDAD,
              SECUENCIA,
              SALDO,
              SALDOSEGURIDAD,
              TIPOPRODUCTO,
              PRDKILO,
              PRDM3,
              FECHAENTREGA,
              FECHAENTREGAMINIMA,
              FECHARECEPCION,
              FECHADESPACHO,
              DIASFABPROVEEDOR,
              OPCIONAL1,
              OPCIONAL2,
              TIPOSERVICIO,
              FECHADESDE,
              FECHAHASTA,
              IDRANGO,
              RANGOHORARIO,
              PRECIOFLETE,
              COSTOFLETE,
              COD_SERVICIO,
              COMUNA
            )
          SELECT var_seq,
            varCanalVenta,
            varNumOrdenVenta,
            varTipoAgrupacionEnvio,
            varSubOrden,
            varCodTiendaVenta,
            varIdGrupo,
            varFechaInicioProceso,
            varNumeroItem,
            varCodProducto,
            varCantidad,
            varTipoStockProducto,
            varCodProveedor,
            varOrigenStock,
            varTipoOrigenStock,
            CCOSTO,
            varPrioridad,
            varSecuencia,
            varSaldo,
            varSaldoSeguridad,
            varTipoProducto,
            varPrdKilo,
            varPrdM3,
            HOY,
            varFechaEntrega,
            varFechaRecepcion,
            varFechaDespacho,
            varDiasFabProveedor,
            varOpcional1sku,
            varOpcional2sku,
            varRetiroODespacho,
            FECHA_DESDE,
            FECHA_HASTA,
            COD_HORA,
            RANGO,
            0,
            0,
            COD_SERVICIO,
            varComunaDespacho
          FROM TMP_CALCULOFECHA
          WHERE CCOSTO      =           varOrigenDespacho
          AND ID                       =var_seq
          AND (COD_SERVICIO = varCodServicio OR varCodServicio IS NULL)
          AND (COD_HORA = varCodRango OR varCodRango IS NULL)
          AND (FECHA_DESDE = varFechaDespachoDesde OR varFechaDespachoDesde IS NULL)
          AND (FECHA_HASTA = varFechaDespachoHasta OR varFechaDespachoHasta IS NULL)
        ;
          IF (SQL%ROWCOUNT)=0 THEN
            RAISE_APPLICATION_ERROR(-20002, 'NO HAY DATOS');
          END IF;
        EXCEPTION
        WHEN OTHERS THEN
          OUT_STATUS_CODE := 1;
          OUT_STATUS_MSG  := 'DAD: No Existen Datos para el Item='|| varNumeroItem || ' origenDespacho='||varOrigenDespacho;
          RETURN;
        END;
      END LOOP;



          IF varIsRecalcular = 1 THEN
             ITFFLETEPRECIOSRECALCULAR(var_seq);
          ELSE
             ITFFLETESERVICIOPRECIOS(var_seq);
          END IF;

    -- ITFFLETEPRECIOSrecalcular(var_seq);

        --  ITFFLETESERVICIOPRECIOS(var_seq);

       --    ITFFLETEPRECIOSrecalcular(var_seq);

          UPDATE TMP_CALCULAFLETE
          SET PRDKILO =  ROUND((select sum(PRDKILO * CANTIDAD)   FROM (SELECT  DISTINCT NUMEROITEM ,CODPRODUCTO,PRDM3 ,PRDKILO, CANTIDAD  FROM TMP_CALCULAFLETE WHERE ID = VAR_SEQ)),2),
              PRDM3 =  ROUND((select sum(PRDM3   * CANTIDAD)   FROM (SELECT  DISTINCT NUMEROITEM ,CODPRODUCTO,PRDM3 ,PRDKILO, CANTIDAD  FROM TMP_CALCULAFLETE WHERE ID = VAR_SEQ)),2)
          WHERE ID = VAR_SEQ;

          BEGIN
          OPEN "rs" FOR SELECT ID,
          CANALVENTA,
          NUMORDENVENTA,
          TIPOAGRUPACIONENVIO,
          SUBORDEN,
          CODTIENDAVENTA,
          IDGRUPO,
          FECHAINICIOPROCESO, -- TO_CHAR(TRUNC(FECHAINICIOPROCESO),'DD/MM/YYYY') FECHAINICIOPROCESO,
          NUMEROITEM,
          CODPRODUCTO,
          CANTIDAD,
          TIPOSTOCK,
          CODPROVEEDOR,
          CENCOSORIGENSTOCK,
          TIPOORIGENSTOCK,
          CENCOSDESPACHO,
          PRIORIDAD,
          SECUENCIA,
          SALDO,
          SALDOSEGURIDAD,
          TIPOPRODUCTO,
          PRDKILO,
          PRDM3,
          FECHAENTREGA, --TO_CHAR(TRUNC(FECHAENTREGA),'DD/MM/YYYY') FECHAENTREGA,
          FECHAENTREGAMINIMA, --TO_CHAR(TRUNC(FECHAENTREGA),'DD/MM/YYYY') FECHAENTREGA,
          FECHARECEPCION, --TO_CHAR(TRUNC(FECHARECEPCION),'DD/MM/YYYY') FECHARECEPCION,
          FECHADESPACHO, --TO_CHAR(TRUNC(FECHADESPACHO),'DD/MM/YYYY') FECHADESPACHO,
          DIASFABPROVEEDOR,
          OPCIONAL1,
          OPCIONAL2,
          TIPOSERVICIO,
          FECHADESDE ,
          FECHAHASTA,
          IDRANGO,
          RANGOHORARIO,
          PRECIOFLETE,
          COSTOFLETE,
          COD_SERVICIO,
          COMUNA FROM TMP_CALCULAFLETE WHERE ID=VAR_SEQ
          order by COD_SERVICIO,RANGOHORARIO, FECHADESDE ,FECHAHASTA desc;
        EXCEPTION
        WHEN OTHERS THEN
          OUT_STATUS_CODE := 1;
          OUT_STATUS_MSG  := 'DAD: No Existen Datos para estos parametros, origenDespacho='|| varOrigenDespacho;
          RETURN;
        END;

    END LOOP;
  END;
END flete_calcular;


PROCEDURE despachos_retrasados (in_fecha          IN     DATE,
                                   in_process_luw    IN     CHAR DEFAULT 'T',
                                   out_status_code      OUT NUMBER,
                                   out_status_msg       OUT VARCHAR)
   AS
      var_cc_activos               VARCHAR (1000);
      var_cod_estadodet_rt         VARCHAR (1000);
      var_cod_estadodet_despacho   VARCHAR (1000);
   BEGIN
      out_status_msg := 'OK';
      out_status_code := 0;


      BEGIN
         SELECT valor_str
           INTO var_cc_activos
           FROM PARAMETROS
          WHERE COD_PARAM = 182;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            out_status_msg :=
               'NO EXISTE EL PARAMETRO 182 EN LA TABLA PARAMETROS';
            out_status_code := -1;
            RETURN;
      END;

      BEGIN
         SELECT valor_str
           INTO var_cod_estadodet_rt
           FROM PARAMETROS
          WHERE COD_PARAM = 183;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            out_status_msg :=
               'NO EXISTE EL PARAMETRO 183 EN LA TABLA PARAMETROS';
            out_status_code := -1;
            RETURN;
      END;

      BEGIN
         SELECT valor_str
           INTO var_cod_estadodet_despacho
           FROM PARAMETROS
          WHERE COD_PARAM = 184;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            out_status_msg :=
               'NO EXISTE EL PARAMETRO 184 EN LA TABLA PARAMETROS';
            out_status_code := -1;
            RETURN;
      END;


      INSERT INTO WLI_STAGE_DESPACHOS_RETRASADOS
           SELECT TIPO_DESPACHO,
                  NUM_RESERVA,
                  NUM_ORDENVENTA,
                  NUM_DOC,
                  RUT,
                  NOMBRE_CLI,
                  CC_DESPACHA,
                  CC_RECIBE,
                  FECHA_PAGO,
                  FECHA_DESPACHO,
                  COD_ESTADOCAB,
                  ENVIADO,
                  OPCION,
                  DESCRIPCION_CC,
                  ES_BODEGA,
                  ID,
                  CC_ORIGEN -- cc vende AGREGADO
             FROM ( -- SELECT PARA OBTENER DESPACHOS RETRASADOS PARA RETIRO EN TIENDA
                   SELECT dtl.TIPO_DESPACHO AS TIPO_DESPACHO,
                          hdr.num_reserva AS NUM_RESERVA,
                          hdr.num_ordenventa as NUM_ORDENVENTA,
                          hdr.num_doc as NUM_DOC,
                          hdr.IDE || '-' || hdr.IDE_DV AS RUT,
                          hdr.NOMBRE_CLI AS NOMBRE_CLI,
                          dtl.CC_DESPACHA AS CC_DESPACHA,
                          dtl.CC_RECIBE AS CC_RECIBE,
                          hdr.FECHA_PAGO AS FECHA_PAGO,
                          dtl.FECHA_DESPACHO AS FECHA_DESPACHO,
                          hdr.COD_ESTADOCAB AS COD_ESTADOCAB,
                          0 AS ENVIADO,
                          ROW_NUMBER ()
                          OVER (
                             PARTITION BY hdr.num_reserva, dtl.FECHA_DESPACHO
                             ORDER BY hdr.num_reserva, dtl.FECHA_DESPACHO)
                             AS OPCION,
                          cc.ORG_NAME_FULL AS DESCRIPCION_CC,
                          cc.ORG_IS_STORE AS ES_BODEGA,
                          0 AS ID,
                          dtl.CC_ORIGEN AS CC_ORIGEN -- cc vende AGREGADO
                     FROM RESERVA_DTL dtl,
                          RESERVA_HDR hdr,
                          ESTADOSDET est,
                          CENTRO_COSTO cc
                    WHERE     1 = 1
                          AND dtl.NUM_RESERVA = hdr.NUM_RESERVA
                          AND dtl.CANAL_VENTA = hdr.CANAL_VENTA
                          AND dtl.COD_ESTADODET = est.COD_ESTADODET
                          AND dtl.CC_DESPACHA = cc.ORG_LVL_NUMBER
                          AND (hdr.TIPO_RT = 'RT' OR hdr.TIPO_RT = 'STS')
                          AND dtl.BLOQUEADO = 0
                          AND est.COD_ESTADODET IN
                                 (    SELECT REGEXP_SUBSTR (var_cod_estadodet_rt,
                                                            '[^,]+',
                                                            1,
                                                            LEVEL)
                                        FROM DUAL
                                  CONNECT BY REGEXP_SUBSTR (
                                                var_cod_estadodet_rt,
                                                '[^,]+',
                                                1,
                                                LEVEL)
                                                IS NOT NULL)
                          AND dtl.FECHA_DESPACHO <= in_fecha
                          AND dtl.CC_DESPACHA IN
                                 (    SELECT REGEXP_SUBSTR (var_cc_activos,
                                                            '[^,]+',
                                                            1,
                                                            LEVEL)
                                        FROM DUAL
                                  CONNECT BY REGEXP_SUBSTR (var_cc_activos,
                                                            '[^,]+',
                                                            1,
                                                            LEVEL)
                                                IS NOT NULL)
                   UNION
                   -- SELECT PARA OBTENER DESPACHOS RETRASADOS PARA DESPACHOS EN TIENDA
                   SELECT dtl.TIPO_DESPACHO AS TIPO_DESPACHO,
                          hdr.num_reserva AS NUM_RESERVA,
                          hdr.num_ordenventa as NUM_ORDENVENTA,
                          hdr.num_doc as NUM_DOC,
                          hdr.IDE || '-' || hdr.IDE_DV AS RUT,
                          hdr.NOMBRE_CLI AS NOMBRE_CLI,
                          dtl.CC_DESPACHA AS CC_DESPACHA,
                          dtl.CC_RECIBE AS CC_RECIBE,
                          hdr.FECHA_PAGO AS FECHA_PAGO,
                          dtl.FECHA_DESPACHO AS FECHA_DESPACHO,
                          hdr.COD_ESTADOCAB AS COD_ESTADOCAB,
                          0 AS ENVIADO,
                          ROW_NUMBER ()
                          OVER (
                             PARTITION BY hdr.num_reserva, dtl.FECHA_DESPACHO
                             ORDER BY hdr.num_reserva, dtl.FECHA_DESPACHO)
                             AS OPCION,
                          cc.ORG_NAME_FULL AS DESCRIPCION_CC,
                          cc.ORG_IS_STORE AS ES_BODEGA,
                          0 AS ID,
                          dtl.CC_ORIGEN AS CC_ORIGEN -- cc vende AGREGADO
                     FROM RESERVA_DTL dtl,
                          RESERVA_HDR hdr,
                          ESTADOSDET est,
                          CC_COMUNA comuna,
                          HOJARUTA hr,
                          PRERUTEO_RESERVADET prDet,
                          CENTRO_COSTO cc
                    WHERE     1 = 1
                          AND dtl.num_reserva = hdr.num_reserva
                          AND dtl.CANAL_VENTA = hdr.CANAL_VENTA
                          AND dtl.cod_estadodet = est.cod_estadodet
                          AND dtl.CC_DESPACHA = comuna.CCOSTO
                          AND hdr.COMUNA_DESP = comuna.COMUNA
                          AND dtl.NUM_RESERVA = prDet.NUM_RESERVA
                          AND prDet.COD_HOJARUTA = hr.COD_HOJARUTA
                          AND dtl.CC_DESPACHA = cc.ORG_LVL_NUMBER
                          AND (hdr.TIPO_RT IS NULL)
                          AND dtl.BLOQUEADO = 0
                          AND est.COD_ESTADODET IN
                                 (    SELECT REGEXP_SUBSTR (
                                                var_cod_estadodet_despacho,
                                                '[^,]+',
                                                1,
                                                LEVEL)
                                        FROM DUAL
                                  CONNECT BY REGEXP_SUBSTR (
                                                var_cod_estadodet_despacho,
                                                '[^,]+',
                                                1,
                                                LEVEL)
                                                IS NOT NULL)
                          AND dtl.FECHA_DESPACHO + comuna.TIEMPO_DESP >
                                 dtl.FECHA_ENTREGA
                          AND dtl.FECHA_DESPACHO <= in_fecha
                          AND dtl.CC_DESPACHA IN
                                 (    SELECT REGEXP_SUBSTR (var_cc_activos,
                                                            '[^,]+',
                                                            1,
                                                            LEVEL)
                                        FROM DUAL
                                  CONNECT BY REGEXP_SUBSTR (var_cc_activos,
                                                            '[^,]+',
                                                            1,
                                                            LEVEL)
                                                IS NOT NULL)) PR
            WHERE opcion = 1
            AND (PR.NUM_RESERVA, PR.FECHA_DESPACHO) NOT IN (
                                                        SELECT RE.NUM_RESERVA,RE.FECHA_DESPACHO
                                                        FROM WLI_STAGE_DESPACHOS_RETRASADOS RE
                                                              WHERE RE.NUM_RESERVA = PR.NUM_RESERVA
                                                              AND RE.FECHA_DESPACHO = PR.FECHA_DESPACHO)
         ORDER BY fecha_despacho ASC;


     IF(in_process_luw = 'T') then
       commit work;
     END IF;

   END despachos_retrasados;


   PROCEDURE despachos_validos (
     in_fecha          IN  DATE,
     in_process_luw    IN  CHAR DEFAULT 'T',
     out_status_code   OUT NUMBER,
     out_status_msg    OUT VARCHAR)
   AS
      var_cc_activos                   VARCHAR (1000);
      var_cod_estadodet_desp_validos   VARCHAR (1000);
   BEGIN
      out_status_msg   := 'OK';
      out_status_code  := 0;

      --Solo se validan si existen los parametros, en la query se usan SELECT directos
      BEGIN
         SELECT valor_str
           INTO var_cc_activos
           FROM PARAMETROS
          WHERE COD_PARAM = 182;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            out_status_msg   := 'NO EXISTE EL PARAMETRO 182 EN LA TABLA PARAMETROS';
            out_status_code  := -1;
            RETURN;
      END;
      BEGIN
         SELECT valor_str
           INTO var_cod_estadodet_desp_validos
           FROM PARAMETROS
          WHERE COD_PARAM = 185;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            out_status_msg   := 'NO EXISTE EL PARAMETRO 185 EN LA TABLA PARAMETROS';
            out_status_code  := -1;
            RETURN;
      END;
      BEGIN
         SELECT valor_str
           INTO var_cod_estadodet_desp_validos
           FROM PARAMETROS
          WHERE COD_PARAM = 189;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            out_status_msg   := 'NO EXISTE EL PARAMETRO 189 EN LA TABLA PARAMETROS';
            out_status_code  := -1;
            RETURN;
      END;

      BEGIN
        DECLARE
          CURSOR C1 IS
          SELECT TIPO_DESPACHO,
                 NUM_RESERVA,
                 RUT_CLIENTE,
                 NOMBRE_DESP,
                 CC_DESPACHA,
                 FECHA_PAGO,
                 FECHA_DESPACHO,
                 COD_ESTADOCAB,
                 ENVIADO,
                 OPCION,
                 DESCRIPCION_CC,
                 ES_BODEGA,
                 ID,
                 FONO_DESP,
                 NUM_ORDENVENTA
          FROM( --SELECT PARA OBTENER RT VALIDOS PARA ENTREGA A CLIENTE
                SELECT DISTINCT 'RT' AS TIPO_DESPACHO,
                                HDR.NUM_RESERVA AS NUM_RESERVA,
                                HDR.IDE || '-' || HDR.IDE_DV AS RUT_CLIENTE,
                                HDR.NOMBRE_DESP AS NOMBRE_DESP,
                                DTL.CC_DESPACHA AS CC_DESPACHA,
                                HDR.FECHA_PAGO AS FECHA_PAGO,
                                DTL.FECHA_DESPACHO AS FECHA_DESPACHO,
                                HDR.COD_ESTADOCAB AS COD_ESTADOCAB,
                                0 AS ENVIADO,
                                ROW_NUMBER ()
                                  OVER (
                                     PARTITION BY hdr.num_reserva,
                                                  dtl.FECHA_DESPACHO
                                     ORDER BY
                                        hdr.num_reserva, dtl.FECHA_DESPACHO)
                                     AS OPCION,
                                CC.ORG_NAME_FULL AS DESCRIPCION_CC,
                                CC.ORG_IS_STORE AS ES_BODEGA,
                                0 AS ID,
                                FORMATEA_CELULAR(HDR.FONO_DESP, HDR.FONO2_DESP, HDR.FONO_CLI) AS FONO_DESP,
                                HDR.NUM_ORDENVENTA AS NUM_ORDENVENTA
                  FROM RESERVA_DTL DTL,
                       RESERVA_HDR HDR,
                       CENTRO_COSTO CC
                 WHERE DTL.CANAL_VENTA = HDR.CANAL_VENTA
                   AND DTL.NUM_RESERVA = HDR.NUM_RESERVA
                   AND DTL.CC_DESPACHA = CC.ORG_LVL_NUMBER
                   AND HDR.TIPO_RT IN ('RT', 'STS')
                   AND DTL.BLOQUEADO     = 0
                   AND hdr.cod_estadocab = 6
                   AND hdr.rt_preparado  = 1
                   /*AND DTL.CC_DESPACHA IN
                       (SELECT REGEXP_SUBSTR(VAR_CC_ACTIVOS, '[^,]+', 1, LEVEL)
                          FROM DUAL
                        CONNECT BY REGEXP_SUBSTR(VAR_CC_ACTIVOS, '[^,]+', 1, LEVEL) IS NOT NULL)*/
                UNION
                -- SELECT PARA OBTENER DESPACHOS QUE SERÁN ENTREGADOS HOY
                SELECT DISTINCT
                       (CASE
                          WHEN SUBSTR(CAST (HDR.NUM_RESERVA AS VARCHAR(20)),0,1 ) = '9' THEN 'DADRT'
                          ELSE
                            (CASE
                               WHEN (HDR.NUMRSV_PADRE IS NOT NULL AND HDR.TIPO_RESERVA = 'P') THEN 'DADCAMBIO'
                               ELSE 'DAD'
                             END)
                        END)                              AS TIPO_DESPACHO,
                       HDR.NUM_RESERVA                    AS NUM_RESERVA,
                       HDR.IDE || '-' || HDR.IDE_DV       AS RUT_CLIENTE,
                       HDR.NOMBRE_DESP                    AS NOMBRE_DESP,
                       DTL.CC_DESPACHA                    AS CC_DESPACHA,
                       HDR.FECHA_PAGO                     AS FECHA_PAGO,
                       DTL.FECHA_DESPACHO                 AS FECHA_DESPACHO,
                       HDR.COD_ESTADOCAB                  AS COD_ESTADOCAB,
                       0                                  AS ENVIADO,
                       ROW_NUMBER ()
                         OVER (PARTITION BY hdr.num_reserva,
                                            dtl.FECHA_DESPACHO
                               ORDER BY hdr.num_reserva,
                                        dtl.FECHA_DESPACHO) AS OPCION,
                       CC.ORG_NAME_FULL                   AS DESCRIPCION_CC,
                       CC.ORG_IS_STORE                    AS ES_BODEGA,
                       0                                  AS ID,
                       FORMATEA_CELULAR(HDR.FONO_DESP, HDR.FONO2_DESP, HDR.FONO_CLI) AS FONO_DESP,
                       HDR.NUM_ORDENVENTA                 AS NUM_ORDENVENTA
                  FROM RESERVA_DTL  DTL,
                       RESERVA_HDR  HDR,
                       CC_COMUNA    COMUNA,
                       CENTRO_COSTO CC
                 WHERE DTL.CANAL_VENTA = HDR.CANAL_VENTA
                   AND DTL.NUM_RESERVA = HDR.NUM_RESERVA
                   AND DTL.CC_DESPACHA = CC.ORG_LVL_NUMBER
                   AND DTL.CANAL_VENTA = COMUNA.CANAL_VENTA
                   AND DTL.CC_DESPACHA = COMUNA.CCOSTO
                   AND HDR.COMUNA_DESP = COMUNA.COMUNA
                   AND HDR.COMUNA_DESPTIPO = COMUNA.COMUNA_TIPO
                   AND HDR.COMUNA_DESPTIPO = 'CO'
                   AND HDR.TIPO_RT IS NULL
                   AND DTL.BLOQUEADO   = 0
                   AND DTL.COD_ESTADODET IN
                       (SELECT REGEXP_SUBSTR((SELECT valor_str FROM parametros WHERE cod_param = 185), '[^,]+', 1, LEVEL)
                          FROM DUAL
                        CONNECT BY REGEXP_SUBSTR((SELECT valor_str FROM parametros WHERE cod_param = 185), '[^,]+', 1, LEVEL) IS NOT NULL)
                   AND DTL.FECHA_REPARTO + COMUNA.TIEMPO_DESP >=  TRUNC(SYSDATE)
                   AND dtl.cod_hojaruta IN (
                       SELECT hr.cod_hojaruta
                       FROM hojaruta hr,
                            transportista t
                       WHERE hr.cod_transp   = t.cod_transp
                         AND hr.cod_hojaruta = dtl.cod_hojaruta
                         AND t.courier       = 0
                      )
                   AND ((CC.ORG_IS_STORE = 'T'
                     AND HDR.REGION_DESP IN
                         (SELECT u.a_divgeo
                          FROM ubicaciones_geo u
                          WHERE u.c_tipdg = 'R'
                          AND u.c_divgeo IN
                              (SELECT REGEXP_SUBSTR((SELECT valor_str FROM parametros WHERE cod_param = 189), '[^,]+', 1, LEVEL)
                              FROM DUAL
                              CONNECT BY REGEXP_SUBSTR((SELECT valor_str FROM parametros WHERE cod_param = 189), '[^,]+', 1, LEVEL) IS NOT NULL)
                         )
                     )
                     OR dtl.CC_DESPACHA IN(
                        SELECT REGEXP_SUBSTR ((SELECT valor_str FROM PARAMETROS WHERE COD_PARAM = 182), '[^,]+', 1, LEVEL)
                        FROM DUAL
                        CONNECT BY REGEXP_SUBSTR ((SELECT valor_str FROM PARAMETROS WHERE COD_PARAM = 182), '[^,]+', 1, LEVEL)
                        IS NOT NULL)
                   )
                )
          WHERE OPCION = 1
            /*AND DTL.CC_DESPACHA IN (
                SELECT REGEXP_SUBSTR(VAR_CC_ACTIVOS, '[^,]+', 1, LEVEL)
                  FROM DUAL
                CONNECT BY REGEXP_SUBSTR(VAR_CC_ACTIVOS, '[^,]+', 1, LEVEL) IS NOT NULL)*/
          ;
          C1_ROW     C1%ROWTYPE;
          C1_COUNT   NUMBER;
        BEGIN
          C1_COUNT := 0;
          FOR I IN C1 LOOP
            BEGIN
              INSERT INTO WLI_STAGE_DESPACHOS_VALIDOS
                     (TIPO_DESPACHO,
                      NUM_RESERVA,
                      RUT_CLIENTE,
                      NOMBRE_DESP,
                      CC_DESPACHA,
                      FECHA_PAGO,
                      FECHA_DESPACHO,
                      COD_ESTADOCAB,
                      ENVIADO,
                      OPCION,
                      DESCRIPCION_CC,
                      ES_BODEGA,
                      ID,
                      FONO_DESP,
                      NUM_ORDENVENTA)
              VALUES (I.TIPO_DESPACHO,
                      I.NUM_RESERVA,
                      I.RUT_CLIENTE,
                      I.NOMBRE_DESP,
                      I.CC_DESPACHA,
                      I.FECHA_PAGO,
                      I.FECHA_DESPACHO,
                      I.COD_ESTADOCAB,
                      I.ENVIADO,
                      I.OPCION,
                      I.DESCRIPCION_CC,
                      I.ES_BODEGA,
                      I.ID,
                      I.FONO_DESP,
                      I.NUM_ORDENVENTA)
              ;
            EXCEPTION
              WHEN DUP_VAL_ON_INDEX THEN
                --DBMS_OUTPUT.PUT_LINE ('OUT_STATUS_MSG = ' || SQLERRM );
                GOTO code_label;
            END;

            IF C1_COUNT = 1 THEN
              C1_COUNT := 0;
              IF IN_PROCESS_LUW = 'T' THEN
                COMMIT;
              END IF;
              -- COMMIT;
            END IF;

            <<code_label>>
            C1_COUNT := C1_COUNT + 1;

          END LOOP;
        END;

        IF IN_PROCESS_LUW = 'T' THEN
          COMMIT;
        END IF;
      END;
   END despachos_validos;

END SRV_DESPACHO;


/
