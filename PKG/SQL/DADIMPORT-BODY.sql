--------------------------------------------------------
--  File created - Thursday-June-21-2018   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Package Body DADIMPORT
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE BODY "DADCORPDS"."DADIMPORT" is
PROCEDURE ACTUALIZAR_RESERVA(
    "xml"           IN SYS.XMLTYPE,
    in_process_luw  IN VARCHAR2,
    OUT_STATUS_CODE OUT NUMBER,
    OUT_STATUS_MSG  OUT VARCHAR)
AS
BEGIN
  DECLARE
    id_xml              NUMBER(12);
    insertSql           VARCHAR2(10000);
    valuesSql           VARCHAR2(10000);
    querySql            VARCHAR2(10000);
    updateSQL1          VARCHAR2(10000);
    updateSQL2          VARCHAR2(10000);
    whereSQL            VARCHAR2(10000);
    filasAfectadas      NUMBER;
    doc                 dbms_xmldom.DOMDocument;
    node                dbms_xmldom.DOMNode;
    hijo                dbms_xmldom.DOMNode;
    nodeList            dbms_xmldom.DOMNodeList;
    hijosList           dbms_xmldom.DOMNodeList;
    numNodesCab         NUMBER;
    numNodesDet         NUMBER;
    numNodes            NUMBER;
    numHijos            NUMBER;
    nodoActual          dbms_xmldom.DOMNode;
    nodeList2           dbms_xmldom.DOMNodeList;
    numNodes2           NUMBER;
    rowCounter          NUMBER;
    rowCT               NUMBER;
    nombre              VARCHAR2(100);
    "tipoDeDato"        VARCHAR2(100);
    valor               VARCHAR2(500);
    fechaOri_enMax      VARCHAR2(500);
    fecha_entrega_desde VARCHAR2(500);
    fecha_into          DATE;
    esPK                NUMBER;
    cuadraturaCab       NUMBER;
    cuadraturaDet       NUMBER;
    cuadrat_numlin      NUMBER;
    "Error"             VARCHAR2(10000);
    canalVenta          VARCHAR2(1000);
    num_rsv             VARCHAR2(20);
    detalle             VARCHAR2(1000);
    estado              VARCHAR2(10);
    estado_nvo          VARCHAR2(10);
    cambia_estado       NUMBER := 0;
    vnum_pkt            NUMBER := 0;
    bloqueo_det         NUMBER := 0;
    vtype_doc           VARCHAR2(3);
    vcc_despacha        NUMBER := 0;
    vdatos_hua          VARCHAR2(50) := '';
    vnumrsv_padre       NUMBER := 0;
    vnum_rsv_all        VARCHAR2(200) := '';
    vnum_doc            VARCHAR2(200);
    vcomuna             VARCHAR2(30);
    vciudad             VARCHAR2(30);
    vregion             VARCHAR2(30);
    NC_PARAM            VARCHAR2(20);
    NC_DESB_PARAM       VARCHAR2(20);
    var_cod_rango       VARCHAR2(200);
    var_glosa_rango     VARCHAR2(200);
    varEstadoCab        NUMBER;
    varEstadoDet        NUMBER;
    vcodcomuna          VARCHAR2(40);
    vusuariopostventa   evento.login_usuario%TYPE := NULL;
    vcodMotivoPV        reserva_dtl.cod_motivopv%TYPE := NULL;

  BEGIN
  OUT_STATUS_CODE:=0;
  SELECT valor_str INTO NC_PARAM FROM parametros WHERE cod_param = 70;
  SELECT valor_str INTO NC_DESB_PARAM FROM parametros WHERE cod_param = 72;
    --EXECUTE IMMEDIATE 'alter session set NLS_NUMERIC_CHARACTERS=''.,'';';
   -- EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_NUMERIC_CHARACTERS = .,';
    doc            := dbms_xmldom.newDomDocument(xmltype("xml".getclobval()));
    node           := dbms_xmldom.makeNode(doc);
    nodeList       := xslprocessor.selectNodes(node, '/dadActRsvDoc/dadActRsvCab','xmlns="http://openuri.org/i/dadActRsvDoc"');
    numNodesCab    := dbms_xmldom.getLength(nodeList);
    FOR rowCounter IN 0..numNodesCab-1
    LOOP
      vcomuna  := ' ';
      vciudad  := ' ';
      vregion  := ' ';
      nodoActual := dbms_xmldom.item(nodeList, rowCounter);
      insertSql  := 'INSERT INTO RESERVA_HDR(';
      valuesSql  := ' VALUES(';
      updateSQL1 := 'UPDATE RESERVA_HDR SET ';
      whereSQL   := ' WHERE ';

      hijosList  := xslprocessor.selectNodes(nodoActual, '*');
      numHijos   := dbms_xmldom.getLength(hijosList);
      FOR rowCT IN 0..numHijos-1
      LOOP
        valor := NULL;

        hijo   := dbms_xmldom.item(hijosList, rowCT);
        nombre := UPPER(dbms_xmldom.getnodename(hijo));

        IF nombre = 'CANAL_VENTA' THEN
          valor         := dbms_xmldom.getnodevalue(xmldom.getFirstChild(hijo));
          cuadraturaCab := cuadraturaCab+to_number(valor);
        END IF;

        IF nombre = 'NUM_RESERVA' THEN
          num_rsv      := dbms_xmldom.getnodevalue(xmldom.getFirstChild(hijo));
          vnum_rsv_all := vnum_rsv_all || num_rsv ||';';
        END IF;

        --Para AR/PE se analiza si comuna/ciudad esta repetido
        IF nombre = 'COMUNA_DESP' THEN
          hijo    := xmldom.getFirstChild(hijo);
          valor   := TRIM(dbms_xmldom.getnodevalue(hijo));
          vcomuna := valor;

          --Si vienen codigos de ubicaciones se transforman en glosas
          BEGIN
            SELECT q.a_divgeo
            INTO valor
            FROM ubicaciones_geo q
            WHERE q.c_tipdg  = 'CO'
              AND q.c_divgeo = vcodcomuna
            ;
          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              null;
          END;
          vcomuna := valor;

        ELSIF nombre = 'CIUDAD_DESP' THEN
          hijo    := xmldom.getFirstChild(hijo);
          valor   := TRIM(dbms_xmldom.getnodevalue(hijo));
          vciudad := valor;
        ELSIF nombre = 'REGION_DESP' THEN
          hijo    := xmldom.getFirstChild(hijo);
          valor   := TRIM(dbms_xmldom.getnodevalue(hijo));
          vregion := valor;
        END IF;
        IF (vcomuna != ' ' AND vciudad != ' ' AND vregion != ' ') THEN
          BEGIN
       SELECT UPPER(vcomuna), UPPER(vciudad), UPPER(vregion)
                                  INTO vcomuna, vciudad, vregion
                                  FROM dual
                                  ;

            SELECT com.a_divgeo, com.a_refdivgeo, ciu.a_refdivgeo
            INTO vcomuna, vciudad, vregion
                                        FROM ubicaciones_geo com, ubicaciones_geo ciu
                                        WHERE com.arefdivgeo_ori = ciu.adivgeo_ori
                                               AND com.c_tipdg = 'CO'
                                               AND ciu.c_tipdg = 'CI'
                                               AND com.c_divgeo > 0
                                               AND ciu.c_divgeo > 0
                                               AND ciu.arefdivgeo_ori = vregion
                                               AND ((INSTR(vciudad, '>') = 0 AND ciu.adivgeo_ori = vciudad)
                                                      OR (INSTR(vciudad, '>') > 0 AND ciu.a_divgeo    = vciudad))
                                               AND ((INSTR(vcomuna, '>') = 0 AND com.adivgeo_ori = vcomuna)
                                                      OR (INSTR(vcomuna, '>') > 0 AND com.a_divgeo    = vcomuna))
                                        ORDER BY com.a_divgeo;

            EXCEPTION
              WHEN TOO_MANY_ROWS THEN --comuna/ciudad repetida
                SELECT com.a_divgeo, com.a_refdivgeo, ciu.a_refdivgeo
                INTO vcomuna, vciudad, vregion
                FROM ubicaciones_geo com, ubicaciones_geo ciu
                WHERE com.arefdivgeo_ori = ciu.adivgeo_ori
                  AND com.c_tipdg  = 'CO'
                  AND ciu.c_tipdg  = 'CI'
                                                            --AND com.c_divgeo > 0 --Cuando se crea la reserva se debe poder buscar los negativos
                                                            --AND ciu.c_divgeo > 0 --Cuando se crea la reserva se debe poder buscar los negativos
                  AND (  (com.a_divgeo IN (SUBSTR(vcomuna || ' >' || vciudad, 1, 30)) --comuna repetida + ciudad unica
                      AND com.a_refdivgeo IN(vciudad)
                      AND ciu.a_refdivgeo IN (vregion))
                    OR   (com.a_divgeo IN (vcomuna) --comuna unica + ciudad repetida
                      AND com.a_refdivgeo IN(SUBSTR(vciudad || ' >' || vregion, 1, 30))
                      AND ciu.a_refdivgeo IN (vregion))
                    OR   (com.a_divgeo IN (SUBSTR(vcomuna || ' >' || SUBSTR(vciudad || ' >' || vregion, 1, 30), 1, 30)) --comuna repetida + ciudad repetida
                      AND com.a_refdivgeo IN(SUBSTR(vciudad || ' >' || vregion, 1, 30))
                      AND ciu.a_refdivgeo IN (vregion)));
          END;
          insertSql := insertSql ||'COMUNA_DESP'||', ';
          valuesSql := valuesSql ||'''' || vcomuna ||'''' ||', ';
          insertSql :=insertSql ||'CIUDAD_DESP'||', ';
          valuesSql :=valuesSql ||'''' || vciudad ||'''' ||', ';
          insertSql :=insertSql ||'REGION_DESP'||', ';
          valuesSql :=valuesSql ||'''' || vregion ||'''' ||', ';
          vcomuna  := ' ';
          vciudad  := ' ';
          vregion  := ' ';
        END IF;

        IF nombre = 'CODIGO_ESTADO' THEN
          hijo      := xmldom.getFirstChild(hijo);
          valor     := dbms_xmldom.getnodevalue(hijo);
          valor     := REPLACE(valor,',','.');
          insertSql := insertSql ||'COD_ESTADOCAB'||', ';
          valuesSql := valuesSql ||'''' || valor ||'''' ||', ';
          varEstadoCab := valor;
        ELSIF nombre = 'DESC_ESTADO' OR nombre ='SUB_ORDEN' THEN
          NULL;

        ELSIF nombre = 'CIUDAD_DESP' OR nombre ='REGION_DESP' THEN
          NULL;
/*
        ELSIF nombre = 'COMUNA_DESP' THEN
          hijo      := xmldom.getFirstChild(hijo);
          valor     := dbms_xmldom.getnodevalue(hijo);
          insertSql := insertSql ||'COMUNA_DESP'||', ';
          valuesSql := valuesSql ||'''' || trim(valor) ||'''' ||', ';
          SELECT A_REFDIVGEO INTO valor FROM UBICACIONES_GEO WHERE c_tipdg = 'CO' AND A_DIVGEO = TRIM(valor);
          insertSql :=insertSql ||'CIUDAD_DESP'||', ';
          valuesSql :=valuesSql ||'''' || valor ||'''' ||', ';
          SELECT A_REFDIVGEO INTO valor FROM UBICACIONES_GEO WHERE c_tipdg = 'CI' AND A_DIVGEO = valor;
          insertSql :=insertSql ||'REGION_DESP'||', ';
          valuesSql :=valuesSql ||'''' || valor ||'''' ||', ';
*/
        ELSIF nombre = 'NUM_DOC' THEN
            hijo       := xmldom.getFirstChild(hijo);
            valor      := dbms_xmldom.getnodevalue(hijo);
            insertSql  := insertSql ||'NUM_DOC'||', ';
            valuesSql  := valuesSql ||'''' || valor ||'''' ||', ';
            updateSQL1 := updateSQL1 ||'NUM_DOC'||'=' ||'''' || valor ||'''' ||', ';

        ELSIF nombre = 'TYPE_DOC' THEN
          hijo       := xmldom.getFirstChild(hijo);
          valor      := dbms_xmldom.getnodevalue(hijo);
          insertSql  := insertSql ||'TYPE_DOC'||', ';
          valuesSql  := valuesSql ||'''' || valor ||'''' ||', ';
          updateSQL1 := updateSQL1 ||'TYPE_DOC'||'=' ||'''' || valor ||'''' ||', ';
          BEGIN
            vtype_doc := NULL;
            SELECT type_doc INTO vtype_doc FROM reserva_hdr WHERE num_reserva = num_rsv;
          EXCEPTION
          WHEN NO_DATA_FOUND THEN
            vtype_doc := NULL;
          END;
          --Se detecta si la reserva llega con NCA
          bloqueo_det := 0;
          IF (valor = NC_PARAM) AND (NVL(vtype_doc, ' ') != NC_PARAM) THEN
              --Bloquear todos los detalles
              bloqueo_det := 1;
          ELSIF (valor != NC_PARAM) AND (vtype_doc = NC_PARAM) THEN
              --DesBloquear todos los detalles
              bloqueo_det := -1;
          END IF;

        ELSIF nombre = 'TIPO_RETIRO' THEN
          hijo       := xmldom.getFirstChild(hijo);
          valor      := dbms_xmldom.getnodevalue(hijo);
          insertSql  := insertSql ||'TIPO_RT'||', ';
          valuesSql  := valuesSql ||'''' || valor ||'''' ||', ';
     --INCORPORACION DE NUEVOS CAMPOS PARA LA CREACION DE LA RESERVA RH:2017/06/27
     ELSIF nombre = 'TYPE_IDE' THEN
          hijo       := xmldom.getFirstChild(hijo);
          valor      := dbms_xmldom.getnodevalue(hijo);
          insertSql  := insertSql ||'TYPE_IDE'||', ';
     updateSQL1 := updateSQL1 ||'TYPE_IDE'||'=' ||'''' || valor ||'''' ||', ';
          valuesSql  := valuesSql ||'''' || valor ||'''' ||', ';
     ELSIF nombre = 'IDE' THEN
          hijo       := xmldom.getFirstChild(hijo);
          valor      := dbms_xmldom.getnodevalue(hijo);
          insertSql  := insertSql ||'IDE'||', ';
     updateSQL1 := updateSQL1 ||'IDE'||'=' ||'''' || valor ||'''' ||', ';
          valuesSql  := valuesSql ||'''' || valor ||'''' ||', ';
     ELSIF nombre = 'IDE_DV' THEN
          hijo       := xmldom.getFirstChild(hijo);
          valor      := dbms_xmldom.getnodevalue(hijo);
          insertSql  := insertSql ||'IDE_DV'||', ';
     updateSQL1 := updateSQL1 ||'IDE_DV'||'=' ||'''' || valor ||'''' ||', ';
          valuesSql  := valuesSql ||'''' || valor ||'''' ||', ';
     ELSIF nombre = 'NOMBRE_CLI' THEN
          hijo       := xmldom.getFirstChild(hijo);
          valor      := dbms_xmldom.getnodevalue(hijo);
          insertSql  := insertSql ||'NOMBRE_CLI'||', ';
     updateSQL1 := updateSQL1 ||'NOMBRE_CLI'||'=' ||'''' || valor ||'''' ||', ';
          valuesSql  := valuesSql ||'''' || valor ||'''' ||', ';
     ELSIF nombre = 'FONO_CLI' THEN
          hijo       := xmldom.getFirstChild(hijo);
          valor      := dbms_xmldom.getnodevalue(hijo);
          insertSql  := insertSql ||'FONO_CLI'||', ';
     updateSQL1 := updateSQL1 ||'FONO_CLI'||'=' ||'''' || valor ||'''' ||', ';
          valuesSql  := valuesSql ||'''' || valor ||'''' ||', ';
     ELSIF nombre = 'NOMBRE_DESP' THEN
          hijo       := xmldom.getFirstChild(hijo);
          valor      := dbms_xmldom.getnodevalue(hijo);
          insertSql  := insertSql ||'NOMBRE_DESP'||', ';
     updateSQL1 := updateSQL1 ||'NOMBRE_DESP'||'=' ||'''' || valor ||'''' ||', ';
          valuesSql  := valuesSql ||'''' || valor ||'''' ||', ';
     ELSIF nombre = 'FONO_DESP' THEN
          hijo       := xmldom.getFirstChild(hijo);
          valor      := dbms_xmldom.getnodevalue(hijo);
          insertSql  := insertSql ||'FONO_DESP'||', ';
     updateSQL1 := updateSQL1 ||'FONO_DESP'||'=' ||'''' || valor ||'''' ||', ';
          valuesSql  := valuesSql ||'''' || valor ||'''' ||', ';
     ELSIF nombre = 'FONO2_DESP' THEN
          hijo       := xmldom.getFirstChild(hijo);
          valor      := dbms_xmldom.getnodevalue(hijo);
          insertSql  := insertSql ||'FONO2_DESP'||', ';
     updateSQL1 := updateSQL1 ||'FONO2_DESP'||'=' ||'''' || valor ||'''' ||', ';
          valuesSql  := valuesSql ||'''' || valor ||'''' ||', ';
     -- FIN DE NUEVOS CAMPOS RH:2017/06/27

        ELSIF nombre = 'MARCA_GUIA' THEN
          hijo       := xmldom.getFirstChild(hijo);
          valor      := dbms_xmldom.getnodevalue(hijo);
          insertSql  := insertSql ||'MARCA_GUIA'||', ';
          valuesSql  := valuesSql ||'''' || valor ||'''' ||', ';
          updateSQL1 := updateSQL1 ||'MARCA_GUIA'||'=' ||'''' || valor ||'''' ||', ';
        ELSIF nombre = 'CAMPO10' THEN
          hijo      := xmldom.getFirstChild(hijo);
          valor     := dbms_xmldom.getnodevalue(hijo);
          valor     := REPLACE(valor,',','.');
          IF valor IS NOT NULL THEN
            insertSql := insertSql ||'CAMPO10'||', ';
            valuesSql := valuesSql ||'''' || valor ||'''' ||', ';
            BEGIN
              SELECT to_number(NVL(valor, 0)) INTO vnumrsv_padre FROM DUAL;
            EXCEPTION
            WHEN OTHERS THEN
              vnumrsv_padre := 0;
            END;
            IF vnumrsv_padre != 0 THEN
              insertSql := insertSql ||'NUMRSV_PADRE'||', ';
              valuesSql := valuesSql || vnumrsv_padre ||', ';
            END IF;
          END IF;

        ELSE
          BEGIN
            SELECT data_type "Tipo de datos"
            INTO "tipoDeDato"
            FROM all_tab_columns
            WHERE table_name  = 'RESERVA_HDR'
              AND column_name = nombre;
          EXCEPTION
          WHEN OTHERS THEN
            "tipoDeDato" := NULL;
          END;

          IF "tipoDeDato"  ='CHAR' OR "tipoDeDato"='VARCHAR2' THEN
            IF (nombre NOT IN ('COMUNA_DESP', 'CIUDAD_DESP', 'REGION_DESP')) THEN
              /*IF (nombre != 'COMUNA_DESP') THEN
              hijo      := xmldom.getFirstChild(hijo);
              valor     := dbms_xmldom.getnodevalue(hijo);
              insertSql := insertSql || nombre ||', ';
              valuesSql := valuesSql ||'''' || UPPER(valor) ||'''' ||', ';

            ELSE*/
              hijo      := xmldom.getFirstChild(hijo);
              valor     := dbms_xmldom.getnodevalue(hijo);
              valor     := REPLACE(valor,'''','''''');
              insertSql := insertSql || nombre ||', ';
              valuesSql := valuesSql ||'''' || UPPER(valor) ||'''' ||', ';
            END IF;

          END IF;
          IF "tipoDeDato"='NUMBER' THEN
            hijo        := xmldom.getFirstChild(hijo);
            valor       := dbms_xmldom.getnodevalue(hijo);
            valor       := REPLACE(valor,',','.');
            insertSql   := insertSql || nombre ||', ' ;
            valuesSql   := valuesSql || valor ||', ';

          END IF;
          IF "tipoDeDato"='DATE' THEN
            hijo        := xmldom.getFirstChild(hijo);
            valor       := dbms_xmldom.getnodevalue(hijo);
            IF valor IS NOT NULL THEN
              insertSql   := insertSql || nombre ||', ';
              valuesSql   := valuesSql || 'to_date(''' ||  valor ||''', ''DD/MM/YYYY HH24:MI''), ';
            END IF;
           END IF;
          END IF;

        IF "tipoDeDato" IS NULL THEN
          out_status_code := 1;
          out_status_msg  := 'DAD: ' || 'no existe el campo '||nombre || ' para la Nro Reserva: '|| num_rsv;
          RETURN ;
        END IF;
      END LOOP;
      querySql  := SUBSTR(insertSql,0,LENGTH(insertSql)-2) || ')' || SUBSTR(valuesSql,0,LENGTH(valuesSql)-2) || ')';
      whereSQL := whereSQL ||'NUM_RESERVA '||'=' || num_rsv;
       -- DBMS_OUTPUT.PUT_LINE('querySql = ' || querySql);
      BEGIN
       --DBMS_OUTPUT.PUT_LINE('QUERY INSERT HEADER = ' ||querySql);
        EXECUTE IMMEDIATE querySql;
        BEGIN
          --La post venta toma su documento de venta de la reserva madre.
          UPDATE reserva_hdr rh
            SET rh.num_doc  = (SELECT rm.num_doc
                               FROM reserva_hdr rm
                               WHERE rm.num_reserva = rh.numrsv_padre),
                rh.type_doc = (SELECT rm.type_doc
                               FROM reserva_hdr rm
                               WHERE rm.num_reserva = rh.numrsv_padre)
--ERROR VH                rh.cod_estadocab= DECODE(varEstadoCab,1,varEstadoCab,2,varEstadoCab,3,varEstadoCab,4,varEstadoCab,rh.cod_estadocab)
            WHERE rh.num_doc IS NULL AND rh.numrsv_padre IS NOT NULL
              AND num_reserva IN (SELECT regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) AS listado
                                  FROM dual CONNECT BY regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) IS NOT NULL);
        EXCEPTION
        WHEN NO_DATA_FOUND THEN
          vtype_doc := NULL;
        END;
      EXCEPTION
      WHEN DUP_VAL_ON_INDEX THEN
        BEGIN
            --DBMS_OUTPUT.PUT_LINE('QUERY UPDATE HEADER= ' || SUBSTR(updateSQL1,0,LENGTH(updateSQL1)-2) || SUBSTR(whereSQL,0,LENGTH(whereSQL)-0));
            EXECUTE IMMEDIATE SUBSTR(updateSQL1,0,LENGTH(updateSQL1)-2) || SUBSTR(whereSQL,0,LENGTH(whereSQL)-0);
        EXCEPTION
        WHEN OTHERS THEN
            out_status_code := 1;
            out_status_msg  := 'DAD: (update)' ||  SQLERRM   || ' para la Nro Reserva: '|| num_rsv;
            RETURN ;
        END;
      WHEN OTHERS THEN
       --DBMS_OUTPUT.PUT_LINE('querySql = ' || querySql);
        out_status_code := 1;
        out_status_msg  :=  'DAD: ' || SQLERRM  || ' para la Nro Reserva: '|| num_rsv;
        RETURN ;
      END;
      IF bloqueo_det = 1 THEN
        UPDATE reserva_hdr SET bloqueo = NC_PARAM WHERE num_reserva = num_rsv;
      ELSIF bloqueo_det = -1 THEN
        UPDATE reserva_hdr SET bloqueo = NC_DESB_PARAM WHERE num_reserva = num_rsv;
      END IF;
    END LOOP;
    nodeList       := xslprocessor.selectNodes(node, '/dadActRsvDoc/dadActRsvDet','xmlns="http://openuri.org/i/dadActRsvDoc"');
    numNodesDet    := dbms_xmldom.getLength(nodeList);
   -- IF numNodesDet = 0 THEN
     --DBMS_OUTPUT.PUT_LINE('querySql = ');
  --  RETURN;
   -- END IF;
    FOR rowCounter IN 0..numNodesDet-1
    LOOP
      nodoActual := dbms_xmldom.item(nodeList, rowCounter);
      insertSql  := 'INSERT INTO RESERVA_DTL (';
      valuesSql  := ' VALUES(';
      updateSQL1  := 'UPDATE RESERVA_DTL SET ';
      updateSQL2  := 'UPDATE RESERVA_DTL SET ';
      whereSQL   := ' WHERE ';
      hijosList  := xslprocessor.selectNodes(nodoActual, '*');
      numHijos   := dbms_xmldom.getLength(hijosList);
      FOR rowCT  IN 0..numHijos-1
      LOOP
        hijo   := dbms_xmldom.item(hijosList, rowCT);
        nombre := UPPER(dbms_xmldom.getnodename(hijo));
        IF nombre = 'CANTIDAD' THEN
          valor         := dbms_xmldom.getnodevalue(xmldom.getFirstChild(hijo));
          cuadraturaDet := cuadraturaDet+to_number(valor);
        END IF;

        IF nombre = 'CANAL_VENTA' THEN
          canalVenta := dbms_xmldom.getnodevalue(xmldom.getFirstChild(hijo));
        ELSIF nombre = 'NUM_RESERVA' THEN
          num_rsv := dbms_xmldom.getnodevalue(xmldom.getFirstChild(hijo));
        ELSIF nombre = 'NUMERO_DETALLE' THEN
          detalle := dbms_xmldom.getnodevalue(xmldom.getFirstChild(hijo));
        END IF;

        IF nombre ='CC_DESPACHA' THEN
          hijo         := xmldom.getFirstChild(hijo);
          valor        := dbms_xmldom.getnodevalue(hijo);
          insertSql    := insertSql ||'CC_DESPACHA'||', ';
          valuesSql    := valuesSql || valor ||', ';
          updateSQL1   := updateSQL1 ||'CC_DESPACHA'||'=' || valor ||', ';
          vcc_despacha := valor;

        ELSIF nombre    ='CODIGO_ESTADO' THEN
          hijo       := xmldom.getFirstChild(hijo);
          valor      := dbms_xmldom.getnodevalue(hijo);
          insertSql  := insertSql ||'COD_ESTADODET'||', ';
          valuesSql  := valuesSql || valor ||', ';
          updateSQL1 := updateSQL1 ||'COD_ESTADODET'||'=' || valor ||', ';
          varEstadoDet := valor;

        ELSIF nombre    ='DATOS_HUA' THEN
          hijo       := xmldom.getFirstChild(hijo);
          valor      := dbms_xmldom.getnodevalue(hijo);
          insertSql  := insertSql ||'DATOS_HUA'||', ';
          valuesSql  := valuesSql ||'''' || REPLACE(valor,' ','') ||'''' ||', ';
          updateSQL1 := updateSQL1 ||'DATOS_HUA'||'=' ||'''' || REPLACE(valor,' ','') || '''' ||', ';
          updateSQL2 := updateSQL2 ||'DATOS_HUA'||'=' ||'''' || REPLACE(valor,' ','') || '''' ||', ';
          insertSql  := insertSql ||'CAMPO4'||', ';
          valuesSql  := valuesSql ||'''' || valor ||'''' ||', ';
          updateSQL1 := updateSQL1 ||'CAMPO4'||'=' ||'''' || valor ||'''' ||', ';
          updateSQL2 := updateSQL2 ||'CAMPO4'||'=' ||'''' || valor ||'''' ||', ';
          vdatos_hua := valor;

        ELSIF  nombre ='DESC_ESTADO' THEN
          NULL;

        ELSIF nombre ='NUMERO_DETALLE' THEN
          hijo      := xmldom.getFirstChild(hijo);
          valor     := dbms_xmldom.getnodevalue(hijo);
          insertSql :=insertSql ||'NUM_DETALLE'||', ';
          valuesSql :=valuesSql || valor ||', ';
          whereSQL := whereSQL ||'ABS(NUM_DETALLE)'||'=' || valor ||' AND  ';

        ELSIF nombre ='FECHA_ENTREGA' THEN --------------ENTREGA ORI
          hijo      := xmldom.getFirstChild(hijo);
          valor     := dbms_xmldom.getnodevalue(hijo);
          fecha_entrega_desde := dbms_xmldom.getnodevalue(hijo);
          IF valor IS NOT NULL THEN
            insertSql :=insertSql ||'FECHA_ENTREGA'||', ';
            valuesSql :=valuesSql || 'to_date(''' || valor||''', ''DD/MM/YYYY HH24:MI''), ';
            insertSql :=insertSql ||'FECHA_ENTREGA_ORI'||', ';
            valuesSql :=valuesSql || 'to_date(''' || valor||''', ''DD/MM/YYYY HH24:MI''), ';
          END IF;

    -- MANTENCION 434: PERMITIR ACTUALIZACION DE FECHA ENTREGA CLIENTE DE ODBMS 18/10/17 RH
    ELSIF nombre    ='FECHA_ENTREGA_CLI' THEN --------------FECHA ENTREGA CLI
          hijo       := xmldom.getFirstChild(hijo);
          valor      := dbms_xmldom.getnodevalue(hijo);
      IF valor IS NOT NULL THEN
      insertSql  := insertSql ||'FECHA_ENTREGA_CLI'||', ';
            valuesSql  := valuesSql || 'to_date(''' || valor||''', ''DD/MM/YYYY HH24:MI''), ';
      updateSQL1 := updateSQL1 ||'FECHA_ENTREGA_CLI = to_date(''' || valor||''', ''DD/MM/YYYY HH24:MI''), ';
      END IF;
    -- FIN MANTENCION 434
        ELSIF nombre ='FECHA_DESPACHO' THEN --------------DESPACHO ORI
          hijo      := xmldom.getFirstChild(hijo);
          valor     := dbms_xmldom.getnodevalue(hijo);
          fechaOri_enMax := dbms_xmldom.getnodevalue(hijo);
          IF valor IS NOT NULL THEN
            insertSql :=insertSql ||'FECHA_DESPACHO'||', ';
            valuesSql :=valuesSql || 'to_date(''' || valor||''', ''DD/MM/YYYY HH24:MI''), ';
            insertSql :=insertSql ||'FECHA_DESPACHO_ORI'||', ';
            valuesSql :=valuesSql || 'to_date(''' || valor||''', ''DD/MM/YYYY HH24:MI''), ';
          END IF;

        ELSIF nombre ='COD_RANGOHORA' THEN
          NULL;

        ELSIF nombre ='GLOSA_RANGO' THEN
          NULL;

        ELSIF nombre ='RANGO_HORARIO' THEN --Parche por Ventana Horario ausente en paises
          hijo      := xmldom.getFirstChild(hijo);
          valor     := dbms_xmldom.getnodevalue(hijo);
          valor     := NVL(valor, 'NR');
          insertSql :=insertSql ||'RANGO_HORARIO'||', ';
          valuesSql :=valuesSql ||'''' || valor ||'''' ||', ';

          BEGIN
            SELECT q.cod_rangohora
            INTO valor
            FROM rango_hora q
            WHERE q.cod_rangohora = valor
            ;
          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              valor := 'NR';
          END;

          insertSql :=insertSql ||'COD_RANGOHORA'||', ';
          valuesSql :=valuesSql ||'''' || valor ||'''' ||', ';
          var_cod_rango := valor;
          DBMS_OUTPUT.PUT_LINE('COD_RANGOHORA= '|| valor );

          BEGIN
              SELECT HORA_INI || ' a ' ||  HORA_FIN INTO var_glosa_rango
                FROM RANGO_HORA
                WHERE
                COD_RANGOHORA = var_cod_rango ;
                EXCEPTION
          WHEN NO_DATA_FOUND THEN
                var_glosa_rango := 'NA';
          END;
          DBMS_OUTPUT.PUT_LINE('GLOSA_RANGO= '|| var_glosa_rango );
          insertSql :=insertSql ||'GLOSA_RANGO'||', ';
          valuesSql :=valuesSql ||'''' || var_glosa_rango ||'''' ||', ';

        ELSIF nombre ='GLOSA_RANGO' THEN --------------DESPACHO ORI
          NULL;

        ELSIF nombre ='CC_ORIGEN_STOCK' THEN
          hijo      := xmldom.getFirstChild(hijo);
          valor     := dbms_xmldom.getnodevalue(hijo);
          insertSql :=insertSql ||'CC_ORIGEN'||', ';
          valuesSql :=valuesSql || valor ||', ';

        ELSIF nombre ='VENDOR_NUMBER' THEN
          hijo      := xmldom.getFirstChild(hijo);
          valor     := dbms_xmldom.getnodevalue(hijo);
          insertSql :=insertSql ||'VENDOR_NUMBER'||', ';
          valuesSql :=valuesSql ||'''' || NVL(valor, '0') ||'''' ||', ';

        ELSIF nombre ='TIPO_PED_DESPACHO' THEN
          hijo       := xmldom.getFirstChild(hijo);
          valor      := dbms_xmldom.getnodevalue(hijo);
          SELECT DECODE(numrsv_padre, NULL, valor, 'POS')
          INTO valor
          FROM reserva_hdr
          WHERE canal_venta = canalVenta
            AND num_reserva = num_rsv;
          insertSql  := insertSql ||'TIPO_PED_DESPACHO'||', ';
          valuesSql  := valuesSql ||'''' || valor ||'''' ||', ';
          updateSQL1 := updateSQL1 ||'TIPO_PED_DESPACHO'||'=' ||'''' || valor ||'''' ||', ';
        ELSIF nombre ='CAMPO1' THEN --------------FECHA_MAXIMA_ENTREGA
          hijo      := xmldom.getFirstChild(hijo);
          valor     := dbms_xmldom.getnodevalue(hijo);
          BEGIN
            IF valor IS NOT NULL THEN
              --Para saber en que formato viene dd-mm-yyyy o yyyy-mm-dd
              SELECT to_date(substr(trim(valor),1,10), 'YYYY-MM-DD') INTO fecha_into FROM dual;
              --Si viene con formato yyyy-mm-dd realizar:
              insertSql :=insertSql ||'CAMPO1'||', ';
              valuesSql :=valuesSql || 'trim(''' || valor||'''), ';
              insertSql :=insertSql ||'FECHA_ENTREGA2'||', ';
              valuesSql :=valuesSql || 'to_date(substr(trim(''' || valor||'''),1,10), ''YYYY-MM-DD''), ';
            ELSE
              --Si viene nulo insertar fecha_desde
              insertSql :=insertSql ||'CAMPO1'||', ';
              valuesSql :=valuesSql || 'TRIM(''' || fecha_entrega_desde||'''), ';
              insertSql :=insertSql ||'FECHA_ENTREGA2'||', ';
              valuesSql :=valuesSql || 'to_date(''' || fecha_entrega_desde||''', ''DD/MM/YYYY HH24:MI''), ';
            END IF;
          EXCEPTION
          WHEN OTHERS THEN
            --Si viene con el formato dd-mm-yyyy
            insertSql :=insertSql ||'CAMPO1'||', ';
            valuesSql :=valuesSql || 'trim(''' || valor||'''), ';
            insertSql :=insertSql ||'FECHA_ENTREGA2'||', ';
            valuesSql :=valuesSql || 'to_date(SUBSTR(TRIM(''' || valor||'''), 1, 10), ''DD-MM-YYYY''), ';
          END;

        ELSIF  nombre ='CAMPO2' THEN --CCS Bodega
          hijo       := xmldom.getFirstChild(hijo);
          valor      := dbms_xmldom.getnodevalue(hijo);
          insertSql  := insertSql ||'CAMPO2'||', ';
          valuesSql  := valuesSql ||'''' || valor ||'''' ||', ';
          insertSql  := insertSql ||'NUM_DISTRO'||', ';
          valuesSql  := valuesSql || NVL(valor, 0) ||', ';
          updateSQL1 := updateSQL1 ||'CAMPO2'||'=' ||'''' || valor ||'''' ||', ';
          updateSQL2 := updateSQL2 ||'CAMPO2'||'=' ||'''' || valor ||'''' ||', ';
          updateSQL1 := updateSQL1 ||'NUM_DISTRO'||'=' || NVL(valor, 0) ||', ';

        ELSIF nombre ='CAMPO3' THEN --------------FECHA_MAXIMA_DESPACHO
          hijo      := xmldom.getFirstChild(hijo);
          valor     := dbms_xmldom.getnodevalue(hijo);
          IF valor IS NOT NULL THEN
            insertSql :=insertSql ||'CAMPO3'||', ';
            valuesSql :=valuesSql || 'trim(''' || valor||'''), ';
            insertSql :=insertSql ||'FECHA_DESPACHO2'||', ';
            valuesSql :=valuesSql || 'to_date(trim(''' || valor||'''), ''DD-MM-YYYY''), ';
          ELSE
            --FECHA DESPACHO ORI CUANDO NO VIENE FECHA MAX DESPACHO
            insertSql :=insertSql ||'CAMPO3'||', ';
            valuesSql :=valuesSql || 'trim(''' || fechaOri_enMax||'''), ';
            insertSql :=insertSql ||'FECHA_DESPACHO2'||', ';
            valuesSql :=valuesSql || 'to_date(trim(''' || fechaOri_enMax||'''), ''DD/MM/YYYY HH24:MI''), ';
          END IF;

        ELSIF  nombre ='CAMPO4' THEN
          NULL;

        ELSIF nombre ='CAMPO5' THEN
          hijo       := xmldom.getFirstChild(hijo);
          valor      := dbms_xmldom.getnodevalue(hijo);
          insertSql  := insertSql ||'CAMPO5'||', ';
          valuesSql  := valuesSql ||'''' || valor ||'''' ||', ';
          insertSql  := insertSql ||'NUM_GD'||', ';
          valuesSql  := valuesSql || NVL(valor, 0) ||', ';
          updateSQL1 := updateSQL1 ||'CAMPO5'||'=' ||'''' || valor ||'''' ||', ';
          updateSQL2 := updateSQL2 ||'CAMPO5'||'=' ||'''' || valor ||'''' ||', ';
          BEGIN
            SELECT NVL(num_gd, 0)
            INTO valor
            FROM reserva_dtl
            WHERE canal_venta = canalVenta
              AND num_reserva = num_rsv
              AND num_detalle = detalle;
          EXCEPTION
          WHEN NO_DATA_FOUND THEN
            valor := 0;
          END;
          IF valor = 0 THEN
            valor      := dbms_xmldom.getnodevalue(hijo);
            updateSQL1 := updateSQL1 ||'NUM_GD'||'=' || NVL(valor, 0) ||', ';
            updateSQL2 := updateSQL2 ||'NUM_GD'||'=' || NVL(valor, 0) ||', ';
          END IF;

        ELSE
          BEGIN
            SELECT data_type "Tipo de datos"
            INTO "tipoDeDato"
            FROM all_tab_columns
            WHERE table_name  = 'RESERVA_DTL'
              AND column_name = nombre;
          EXCEPTION
          WHEN OTHERS THEN
            "tipoDeDato" := NULL;
          END;
         /* SELECT SUM(1) "PK"
          INTO esPK
          FROM all_cons_columns
          WHERE table_name = 'RESERVA_DTL'
           and CONSTRAINT_NAME ='PK_RESERVA_DTL'
          AND column_name  =nombre;*/
          IF "tipoDeDato"  ='CHAR' OR "tipoDeDato"='VARCHAR2' THEN
            hijo          := xmldom.getFirstChild(hijo);
            valor         := dbms_xmldom.getnodevalue(hijo);
            --valor         := REPLACE(valor,',','.');
            --valor         := REPLACE(valor,'''','');
            valor         := REPLACE(valor,'''','''''');
            insertSql     :=insertSql || nombre ||', ';
            valuesSql     :=valuesSql ||'''' || valor ||'''' ||', ';

            IF UPPER(nombre) = 'DESC_ESTADO_ODBMS' OR
               UPPER(nombre)= 'DESC_ESTADO_DESPACHO' or
               UPPER(nombre)= 'TIPO_PED_ODBMS' OR
               UPPER(nombre)= 'NUM_PED_ODBMS' OR
               UPPER(nombre)= 'TIPO_PED_DESPACHO' OR
               UPPER(nombre)= 'NUM_PED_DESPACHO' OR
               UPPER(nombre)= 'NUM_PKT' OR
               UPPER(nombre)= 'CANTIDAD_ENTREGA_CLI' OR
               UPPER(nombre)= 'NUM_GD_PROV' OR
               UPPER(nombre)= 'NUM_SERIEGD_PROV' OR
               UPPER(nombre)= 'NUM_FACTURA_PROV' OR
               UPPER(nombre)= 'IDE_RECIVE' OR
               UPPER(nombre)= 'IDE_DV_RECIVE' OR
               UPPER(nombre)= 'NOMBRE_RECIVE' OR
               UPPER(nombre)= 'IDE_CHOFER' OR
               UPPER(nombre)= 'IDE_DV_CHOFER' OR
               UPPER(nombre)= 'NOMBRE_CHOFER'
              THEN
              updateSQL1   := updateSQL1 || nombre ||'='||'''' || UPPER(valor) ||'''' ||', ';
            END IF;

            IF UPPER(nombre) = 'DESC_ESTADO_ODBMS' or UPPER(nombre)= 'DESC_ESTADO_DESPACHO' THEN
              updateSQL2   := updateSQL2 || nombre ||'='||'''' || UPPER(valor) ||'''' ||', ';
            END IF;
          END IF;

          IF "tipoDeDato"='NUMBER' THEN
            hijo        := xmldom.getFirstChild(hijo);
            valor       := dbms_xmldom.getnodevalue(hijo);
            --valor       := REPLACE(valor,'.',',');
            insertSql   :=insertSql || nombre ||', ';
            valuesSql   :=valuesSql || valor ||', ';

            IF UPPER(nombre) = 'DESC_ESTADO_ODBMS' OR
               UPPER(nombre)= 'DESC_ESTADO_DESPACHO' or
               UPPER(nombre)= 'TIPO_PED_ODBMS' OR
               UPPER(nombre)= 'NUM_PED_ODBMS' OR
               UPPER(nombre)= 'TIPO_PED_DESPACHO' OR
               UPPER(nombre)= 'NUM_PED_DESPACHO' OR
               UPPER(nombre)= 'NUM_PKT' OR
               UPPER(nombre)= 'CANTIDAD_ENTREGA_CLI' OR
               UPPER(nombre)= 'NUM_GD_PROV' OR
               UPPER(nombre)= 'NUM_SERIEGD_PROV' OR
               UPPER(nombre)= 'NUM_FACTURA_PROV' OR
               UPPER(nombre)= 'IDE_RECIVE' OR
               UPPER(nombre)= 'IDE_DV_RECIVE' OR
               UPPER(nombre)= 'NOMBRE_RECIVE' OR
               UPPER(nombre)= 'IDE_CHOFER' OR
               UPPER(nombre)= 'IDE_DV_CHOFER' OR
               UPPER(nombre)= 'NOMBRE_CHOFER'
              THEN
              updateSQL1   := updateSQL1 || nombre ||'='||'''' || UPPER(valor) ||'''' ||', ';
            END IF;

            IF UPPER(nombre) = 'DESC_ESTADO_ODBMS' or UPPER(nombre)= 'DESC_ESTADO_DESPACHO' THEN
              updateSQL2   := updateSQL2 || nombre ||'='||'''' || UPPER(valor) ||'''' ||', ';
            END IF;

            IF nombre='CANAL_VENTA' OR nombre='NUM_RESERVA' OR nombre='NUM_DETALLE' THEN
              whereSQL := whereSQL || nombre ||'=' || valor ||' AND  ';
            END IF;
          END IF;

          IF "tipoDeDato"='DATE' THEN
            hijo        := xmldom.getFirstChild(hijo);
            valor       := dbms_xmldom.getnodevalue(hijo);
            IF valor IS NOT NULL THEN
              insertSql   :=insertSql || nombre ||', ';
              valuesSql   :=valuesSql || 'to_date(''' || valor||''', ''DD/MM/YYYY HH24:MI''), ';
            END IF;
          END IF;
        END IF;

        IF "tipoDeDato" IS NULL THEN
          out_status_code := 1;
          out_status_msg  :=  'DAD: ' || 'no existe el campo '||nombre  || ' para la Nro Reserva: '|| num_rsv || 'con Nro Detalle: ' ||detalle;
          RETURN ;
        END IF;
      END LOOP;
      --Se ingresan las HUA no presentes en DAD
      BEGIN
        IF (TRIM(SPLIT(vdatos_hua, ';', 1)) IS NOT NULL AND --Se agrego departamento....
            TRIM(SPLIT(vdatos_hua, ';', 2)) IS NOT NULL AND
            TRIM(SPLIT(vdatos_hua, ';', 3)) IS NOT NULL AND
            TRIM(SPLIT(vdatos_hua, ';', 4)) IS NOT NULL)
          THEN
          INSERT INTO plm(cc,depto, pasillo, lineal, metro)
          SELECT DISTINCT vcc_despacha, TRIM(SPLIT(vdatos_hua, ';', 1)), TRIM(SPLIT(vdatos_hua, ';', 2)), TRIM(SPLIT(vdatos_hua, ';', 3)), TRIM(SPLIT(vdatos_hua, ';', 4))
          FROM dual
          WHERE (vcc_despacha,TRIM(SPLIT(vdatos_hua, ';', 1)), TRIM(SPLIT(vdatos_hua, ';', 2)), TRIM(NVL(SPLIT(vdatos_hua, ';', 3), '0')), TRIM(NVL(SPLIT(vdatos_hua, ';', 4), 0))) NOT IN
               (SELECT cc, NVL(depto, 0), NVL(pasillo, 0), NVL(lineal, ''), NVL(metro, 0) FROM plm);
        END IF;
      EXCEPTION
      WHEN OTHERS THEN
        out_status_msg  := 'DAD: ' ||  SQLERRM   || ' para la Nro Reserva: '|| num_rsv || ' con Nro Detalle: ' ||detalle;
        NULL; --Si hay problemas con la HUA no se rechaza.
      END;

      --INI: obtiene el usuario de la post venta (en caso que existiera)
      BEGIN
        SELECT rd.usuario_postventa
          INTO vusuariopostventa
          FROM reserva_hdr rh,
               reserva_hdr rm,
               reserva_dtl rd
         WHERE rh.canal_venta  = rm.canal_venta
           AND rh.numrsv_padre = rm.num_reserva
           AND rm.canal_venta  = rd.canal_venta
           AND rm.num_reserva  = rd.num_reserva
           AND rh.num_reserva  = num_rsv
           AND rd.num_detalle  = detalle
           AND ROWNUM = 1;

        insertSql     :=insertSql || 'USUARIO_EVENTO' ||', ';
        valuesSql     :=valuesSql ||'''' || TRIM(vusuariopostventa) ||'''' ||', ';

      EXCEPTION
       WHEN OTHERS THEN
         NULL;
      END;
      --FIN: obtiene el usuario de la post venta (en caso que existiera)
      --INI: obtiene el motivo de la post venta
      BEGIN
        SELECT rd.cod_motivopv
          INTO vcodMotivoPV
          FROM reserva_hdr rh,
               reserva_hdr rm,
               reserva_dtl rd
         WHERE rh.canal_venta  = rm.canal_venta
           AND rh.numrsv_padre = rm.num_reserva
           AND rm.canal_venta  = rd.canal_venta
           AND rm.num_reserva  = rd.num_reserva
           AND rh.num_reserva  = num_rsv
           AND rd.num_detalle  = detalle
           AND ROWNUM = 1;
        insertSql     :=insertSql || 'COD_MOTIVOPV' ||', ';
        valuesSql     :=valuesSql ||'''' || TRIM(vcodMotivoPV) ||'''' ||', ';
      EXCEPTION
       WHEN OTHERS THEN
         NULL;
      END;
      --FIN: obtiene el motivo de la post venta

      querySql  := SUBSTR(insertSql,0,LENGTH(insertSql)-2) || ')' || SUBSTR(valuesSql,0,LENGTH(valuesSql)-2) || ')';
      -- updateSQL := SUBSTR(updateSQL,0,LENGTH(updateSQL)-2) || SUBSTR(whereSQL,0,LENGTH(whereSQL)-5) ;
      -- DBMS_OUTPUT.PUT_LINE('querySql = ' || querySql);
      -- DBMS_OUTPUT.PUT_LINE('updateSQL = ' || updateSQL);
      BEGIN
       --DBMS_OUTPUT.PUT_LINE('QUERY INSERT  DETAIL= ' ||querySql);
        EXECUTE IMMEDIATE querySql;
        --A la post venta se agrega un delta de dos dias en caso de no existir delta.
        BEGIN
          UPDATE reserva_dtl
            SET fecha_despacho2   = fecha_despacho2 + DECODE(fecha_despacho, fecha_despacho2, (SELECT valor_num FROM parametros WHERE cod_param = 12), 0),
                fecha_entrega2    = fecha_entrega2  + DECODE(fecha_entrega,  fecha_entrega2,  (SELECT valor_num FROM parametros WHERE cod_param = 12), 0)
--ERROR VH                cod_estadodet= DECODE(varEstadoDet,1,varEstadoDet,2,varEstadoDet,3,varEstadoDet,4,varEstadoDet,cod_estadodet)
            WHERE num_reserva IN (SELECT num_reserva
                                  FROM reserva_hdr
                                  WHERE numrsv_padre IS NOT NULL
                                    AND num_reserva IN (SELECT regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) AS listado
                                                        FROM dual CONNECT BY regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) IS NOT NULL))
          ;
        EXCEPTION
        WHEN NO_DATA_FOUND THEN
          vtype_doc := NULL; --Solo captura el error
        END;
      EXCEPTION
      WHEN DUP_VAL_ON_INDEX THEN
        BEGIN
            SELECT cod_estadodet INTO estado FROM reserva_dtl WHERE canal_venta = canalventa AND num_reserva = num_rsv AND num_detalle = detalle;
            IF (CAST (estado AS NUMBER) IN (1, 2, 3, 5, 6, 7, 8, 9, 50, 51, 52, 61, 71, 106, 107)) THEN
              --DBMS_OUTPUT.PUT_LINE('updateSQL1 = ' || SUBSTR(updateSQL1,0,LENGTH(updateSQL1)-2) || SUBSTR(whereSQL,0,LENGTH(whereSQL)-5));
              --DBMS_OUTPUT.PUT_LINE('QUERY UPDATE DETAIL= ' || SUBSTR(updateSQL1,0,LENGTH(updateSQL1)-2) || SUBSTR(whereSQL,0,LENGTH(whereSQL)-5));
              EXECUTE IMMEDIATE SUBSTR(updateSQL1,0,LENGTH(updateSQL1)-2) || SUBSTR(whereSQL,0,LENGTH(whereSQL)-5);
              --INI: Gestionar reservas preruteadas y confirmadas para pasar de estado 9 a 10.
              cambia_estado := 0;
              BEGIN
                estado_nvo := 0;
                SELECT cod_estadodet, NVL(num_pkt, 0)
                INTO   estado_nvo, vnum_pkt
                FROM preruteo_reservadet pr, reserva_dtl rd
                WHERE pr.canal_venta = rd.canal_venta
                  AND pr.num_reserva = rd.num_reserva
                  AND pr.num_detalle = rd.num_detalle
                  AND pr.canal_venta = canalVenta
                  AND pr.num_reserva = num_rsv
                  AND pr.num_detalle = detalle
                  AND en_jaula = 1
                  AND confirmado = 1; --Tiene q estar confirmada para generar cambio automatico
              EXCEPTION WHEN NO_DATA_FOUND THEN
                estado_nvo := 0;
              END;
              IF (estado_nvo = 9 AND (CAST(estado AS NUMBER) IN (1,2,3,5, 6, 7, 8, 50, 61, 71))) THEN
                  UPDATE reserva_dtl
                  SET cod_estadodet = 10
                  WHERE num_reserva      = num_rsv
                    AND ABS(num_detalle) = detalle;
                  --Enviar pkt a WMOS
                  INSERT INTO wli_stage_preruteo(num_pkt, num_jaula, wharehouse, campo1, campo2, campo3)
                  SELECT vnum_pkt, pr.num_jaula, cc.wharehouse, NULL, NULL, NULL
                  FROM preruteo_reservadet pr, centro_costo cc, reserva_dtl rd
                  WHERE pr.num_reserva    = num_rsv
                    AND pr.num_detalle    = detalle
                    AND pr.en_jaula       = 1
                    AND pr.confirmado     = 1
                    AND pr.cod_hojaruta IS NULL
                    AND pr.num_reserva    = rd.num_reserva
                    AND pr.num_detalle    = rd.num_detalle
                    AND pr.canal_venta    = rd.canal_venta
                    AND cc.org_lvl_number = DECODE((SELECT org_is_store FROM centro_costo cc2 WHERE cc2.org_lvl_number = rd.cc_despacha),
                                                   'T', rd.cc_despacha,
                                                   rd.cc_origen)
                    GROUP BY vnum_pkt, pr.num_jaula, cc.wharehouse;
              END IF;
              --FIN: Gestionar reservas preuteadas y confirmadas para pasar de estado 9 a 10.
            ELSE
              --DBMS_OUTPUT.PUT_LINE('updateSQL2 = ' || SUBSTR(updateSQL2,0,LENGTH(updateSQL2)-2) || SUBSTR(whereSQL,0,LENGTH(whereSQL)-5));
              EXECUTE IMMEDIATE SUBSTR(updateSQL2,0,LENGTH(updateSQL2)-2) || SUBSTR(whereSQL,0,LENGTH(whereSQL)-5);
            END IF;
        EXCEPTION
        WHEN OTHERS THEN
          out_status_code := 1;
          out_status_msg  := 'DAD: (update)' ||  SQLERRM   || ' para la Nro Reserva: '|| num_rsv || ' con Nro Detalle: ' ||detalle;
          RETURN ;
        END;
      WHEN OTHERS THEN
        -- DBMS_OUTPUT.PUT_LINE('querySql = ' || querySql);
        out_status_code := 1;
        out_status_msg  :=  'DAD: (Insert) ' || SQLERRM  || ' para la Nro Reserva: '|| num_rsv || ' con Nro Detalle: ' ||detalle;
        RETURN ;
      END;
    END LOOP;
    --Gestion NCA
    UPDATE reserva_dtl
       SET bloqueado = 1 --Se bloquea siempre independiente del estado del detalle
     WHERE num_reserva IN (SELECT num_reserva
                           FROM reserva_hdr
                           WHERE bloqueo = NC_PARAM
                             AND num_reserva IN (SELECT regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) AS listado
                                                 FROM dual CONNECT BY regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) IS NOT NULL))
    ;
    UPDATE reserva_dtl
       SET bloqueado = 0 --Se desbloquea siempre independiente del estado del detalle
     WHERE num_reserva IN (SELECT num_reserva
                           FROM reserva_hdr
                           WHERE bloqueo = NC_DESB_PARAM
                             AND num_reserva IN (SELECT regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) AS listado
                                                 FROM dual CONNECT BY regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) IS NOT NULL))
    ;
    INSERT INTO evento(id_evento, cod_tipoevento, fecha_evento, canal_venta, num_reserva, cod_estado, num_detalle, sub_orden, login_usuario)
    SELECT seq_eventos.nextval, a.*
    FROM (SELECT DECODE(bloqueo, NC_PARAM, 3, NC_DESB_PARAM, 4) tipo_evento,
          SYSDATE,
          rd.canal_venta,
          rd.num_reserva,
          rd.cod_estadodet,
          rd.num_detalle,
          rd.sub_orden,
          'ODBMS: ' || bloqueo
          FROM reserva_hdr rh, reserva_dtl rd, estadosdet e
          WHERE rh.canal_venta   = rd.canal_venta
            AND rh.num_reserva   = rd.num_reserva
            AND rd.cod_estadodet = e.cod_estadodet
            AND rh.bloqueo IN ((SELECT valor_str FROM parametros WHERE cod_param IN (70, 72)))
            AND NVL(DECODE(e.ncr, 'C', 'T', e.ncr), 'T') = 'T' --Condicion para NCR
            AND rh.num_reserva IN (SELECT regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) AS listado
                                   FROM dual CONNECT BY regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) IS NOT NULL)
          ORDER BY num_reserva, num_detalle) a;

    UPDATE reserva_hdr
       SET bloqueo = ''
     WHERE bloqueo IS NOT NULL
       AND num_reserva IN (SELECT regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) AS listado
                          FROM dual CONNECT BY regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) IS NOT NULL);

    --Se crean los retiros de las reservas de cambio.
    INSERT INTO reserva_dtl(canal_venta, num_reserva, num_detalle, num_ordenventa, sub_orden, prd_lvl_number, prioridad, vendor_number,
                            cantidad, datos_hua, num_reserva_b2b, usuario_evento, tipo_ped_despacho, tipo_despacho, tipo_stock, prd_name_full, cc_origen,
                            cc_despacha, prd_kilo, prd_m3, precio, num_pkt, cod_estadodet, fecha_ini_proceso, fecha_despacho, fecha_despacho2,
                            fecha_despacho_ori, fecha_entrega, fecha_entrega2, fecha_entrega_ori, fecha_recep_oc,
    /*Datos ceros o nulos*/ num_gd, express, cc_via, cc_envia, cc_recibe, num_ped_odbms, prima_pendiente, cant_desp, cant_ret, cant_trf, bloqueado,
    /*campos agregados por ventana horario*/ cod_rangohora, glosa_rango,cod_servicio,
    /*motivo post venta*/ cod_motivopv)
    SELECT rm.canal_venta, rm.prima_pendiente, -rm.num_detalle, rm.num_ordenventa, rm.sub_orden, rm.prd_lvl_number, rm.prioridad, rm.vendor_number,
           rm.cant_ret cantidad, rd1.datos_hua, rd1.num_reserva_b2b, rd1.usuario_evento, 'POS' tipo_ped_despacho, 'R' tipo_despacho, 'S' tipo_stock,
           NVL(rd1.prd_name_full, rm.prd_name_full), NVL(rd1.cc_despacha, rm.cc_despacha), NVL(rd1.cc_despacha, rm.cc_despacha) cc_despacha,
           NVL(rd1.prd_kilo, rm.prd_kilo), NVL(rd1.prd_m3, rm.prd_m3), NVL(rd1.precio, rm.precio), NVL(rd1.num_pkt, 0),
           CASE
             WHEN (rm.canal_venta, rm.prima_pendiente, rm.num_detalle) IN (
                   SELECT canal_venta, num_reserva, num_detalle
                   FROM reserva_dtl rd2
                   WHERE rd2.canal_venta = rd1.canal_venta
                     AND rd2.num_reserva = rd1.num_reserva
                     AND rd2.num_detalle = rd1.num_detalle) THEN rd1.cod_estadodet
             ELSE DECODE((SELECT org_is_store FROM centro_costo WHERE org_lvl_number = rm.cc_despacha),
                         'F', 110, --Tienda
                         9)        --Bodega
           END AS cod_estadodet,
           CASE
             WHEN (rm.canal_venta, rm.prima_pendiente, rm.num_detalle) IN (
                   SELECT canal_venta, num_reserva, num_detalle
                   FROM reserva_dtl rd2
                   WHERE rd2.canal_venta = rd1.canal_venta
                     AND rd2.num_reserva = rd1.num_reserva
                     AND rd2.num_detalle = rd1.num_detalle) THEN rd1.fecha_ini_proceso
             ELSE
               (SELECT MIN(rd3.fecha_ini_proceso)
                FROM reserva_dtl rd3
                WHERE rd3.canal_venta = rm.canal_venta AND rd3.num_reserva = rm.prima_pendiente
                GROUP BY rd3.canal_venta, rd3.num_reserva)
           END AS fecha_ini_proceso,
           CASE
             WHEN (rm.canal_venta, rm.prima_pendiente, rm.num_detalle) IN (
                   SELECT canal_venta, num_reserva, num_detalle
                   FROM reserva_dtl rd2
                   WHERE rd2.canal_venta = rd1.canal_venta
                     AND rd2.num_reserva = rd1.num_reserva
                     AND rd2.num_detalle = rd1.num_detalle) THEN rd1.fecha_despacho
             ELSE
               (SELECT MIN(rd3.fecha_despacho)
                FROM reserva_dtl rd3
                WHERE rd3.canal_venta = rm.canal_venta AND rd3.num_reserva = rm.prima_pendiente
                GROUP BY rd3.canal_venta, rd3.num_reserva)
           END AS fecha_despacho,
           CASE
             WHEN (rm.canal_venta, rm.prima_pendiente, rm.num_detalle) IN (
                   SELECT canal_venta, num_reserva, num_detalle
                   FROM reserva_dtl rd2
                   WHERE rd2.canal_venta = rd1.canal_venta
                     AND rd2.num_reserva = rd1.num_reserva
                     AND rd2.num_detalle = rd1.num_detalle) THEN rd1.fecha_despacho2
             ELSE
               (SELECT MIN(rd3.fecha_despacho2)
                FROM reserva_dtl rd3
                WHERE rd3.canal_venta = rm.canal_venta AND rd3.num_reserva = rm.prima_pendiente
                GROUP BY rd3.canal_venta, rd3.num_reserva)
           END AS fecha_despacho2,
           CASE
             WHEN (rm.canal_venta, rm.prima_pendiente, rm.num_detalle) IN (
                   SELECT canal_venta, num_reserva, num_detalle
                   FROM reserva_dtl rd2
                   WHERE rd2.canal_venta = rd1.canal_venta
                     AND rd2.num_reserva = rd1.num_reserva
                     AND rd2.num_detalle = rd1.num_detalle) THEN rd1.fecha_despacho_ori
             ELSE
               (SELECT MIN(rd3.fecha_despacho_ori)
                FROM reserva_dtl rd3
                WHERE rd3.canal_venta = rm.canal_venta AND rd3.num_reserva = rm.prima_pendiente
                GROUP BY rd3.canal_venta, rd3.num_reserva)
           END AS fecha_despacho_ori,
           CASE
             WHEN (rm.canal_venta, rm.prima_pendiente, rm.num_detalle) IN (
                   SELECT canal_venta, num_reserva, num_detalle
                   FROM reserva_dtl rd2
                   WHERE rd2.canal_venta = rd1.canal_venta
                     AND rd2.num_reserva = rd1.num_reserva
                     AND rd2.num_detalle = rd1.num_detalle) THEN rd1.fecha_entrega
             ELSE
               (SELECT MIN(rd3.fecha_entrega)
                FROM reserva_dtl rd3
                WHERE rd3.canal_venta = rm.canal_venta AND rd3.num_reserva = rm.prima_pendiente
                GROUP BY rd3.canal_venta, rd3.num_reserva)
           END AS fecha_entrega,
           CASE
             WHEN (rm.canal_venta, rm.prima_pendiente, rm.num_detalle) IN (
                   SELECT canal_venta, num_reserva, num_detalle
                   FROM reserva_dtl rd2
                   WHERE rd2.canal_venta = rd1.canal_venta
                     AND rd2.num_reserva = rd1.num_reserva
                     AND rd2.num_detalle = rd1.num_detalle) THEN rd1.fecha_entrega2
             ELSE
               (SELECT MIN(rd3.fecha_entrega2)
                FROM reserva_dtl rd3
                WHERE rd3.canal_venta = rm.canal_venta AND rd3.num_reserva = rm.prima_pendiente
                GROUP BY rd3.canal_venta, rd3.num_reserva)
           END AS fecha_entrega2,
           CASE
             WHEN (rm.canal_venta, rm.prima_pendiente, rm.num_detalle) IN (
                   SELECT canal_venta, num_reserva, num_detalle
                   FROM reserva_dtl rd2
                   WHERE rd2.canal_venta = rd1.canal_venta
                     AND rd2.num_reserva = rd1.num_reserva
                     AND rd2.num_detalle = rd1.num_detalle) THEN rd1.fecha_entrega_ori
             ELSE
               (SELECT MIN(rd3.fecha_entrega_ori)
                FROM reserva_dtl rd3
                WHERE rd3.canal_venta = rm.canal_venta AND rd3.num_reserva = rm.prima_pendiente
                GROUP BY rd3.canal_venta, rd3.num_reserva)
           END AS fecha_entrega_ori,
           CASE
             WHEN (rm.canal_venta, rm.prima_pendiente, rm.num_detalle) IN (
                   SELECT canal_venta, num_reserva, num_detalle
                   FROM reserva_dtl rd2
                   WHERE rd2.canal_venta = rd1.canal_venta
                     AND rd2.num_reserva = rd1.num_reserva
                     AND rd2.num_detalle = rd1.num_detalle) THEN rd1.fecha_recep_oc
             ELSE
               (SELECT MIN(rd3.fecha_recep_oc)
                FROM reserva_dtl rd3
                WHERE rd3.canal_venta = rm.canal_venta AND rd3.num_reserva = rm.prima_pendiente
                GROUP BY rd3.canal_venta, rd3.num_reserva)
           END AS fecha_recep_oc,
           --Datos ceros o nulos
           0 num_gd, 0 express, 0 AS cc_via, 0 cc_envia, 0 cc_recibe, 0 num_ped_odbms, NULL prima_pendiete, NULL cant_desp,
           NULL cant_ret, NULL cant_trf, 0 AS bloqueado,
           /*campos agregados por ventana horaria*/
           rd1.cod_rangohora,
           rd1.glosa_rango,
           rd1.cod_servicio,
           rm.cod_motivopv
    FROM reserva_dtl rd1,
      (SELECT rd2.*
        FROM reserva_dtl rd2,
             (SELECT rd.canal_venta,
                     rd.num_reserva,
                     rh.numrsv_padre
                FROM reserva_dtl rd,
                     reserva_hdr rh,
                     (SELECT regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) AS rsv
                        FROM dual
                      CONNECT BY regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) IS NOT NULL) l
               WHERE rd.canal_venta = rh.canal_venta
                 AND rd.num_reserva = rh.num_reserva
                 AND rh.num_reserva = l.rsv
                 AND rd.num_detalle > 0) t
       WHERE rd2.canal_venta = t.canal_venta
         AND rd2.num_reserva = t.numrsv_padre
         AND rd2.prima_pendiente = t.num_reserva
         AND nvl(rd2.cant_re_desp, 0) = 0
      ) rm
    WHERE rd1.canal_venta (+)= rm.canal_venta
      AND rd1.num_reserva (+)= rm.prima_pendiente
      AND rd1.num_detalle (+)= rm.num_detalle
    ORDER BY rm.canal_venta, rm.num_reserva, rm.num_detalle
    ;

   --set tipo reserva para las reservas de redespacho
   UPDATE reserva_hdr rh
      SET tipo_reserva =
          (SELECT CASE
                     WHEN SUM(nvl(rd.cant_ret, 0) + nvl(rd.cant_desp, 0)) > 0 THEN 'P'
                     WHEN SUM(nvl(rd.cant_re_desp, 0)) > 0                    THEN 'R'
                     ELSE                                                          'N'
                  END
             FROM reserva_dtl rd,
                  reserva_hdr rm
            WHERE rd.canal_venta = rm.canal_venta
              AND rd.num_reserva = rm.num_reserva
              AND rd.prima_pendiente = rh.num_reserva
              AND rm.canal_venta = rh.canal_venta
              AND rm.num_reserva = rh.numrsv_padre)
    WHERE rh.num_reserva IN
          (SELECT regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) AS num_reserva
             FROM dual
           CONNECT BY regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) IS NOT NULL)
      AND EXISTS (SELECT 1
             FROM reserva_dtl rd,
                  reserva_hdr rm
            WHERE rd.canal_venta = rm.canal_venta
              AND rd.num_reserva = rm.num_reserva
              AND rd.prima_pendiente = rh.num_reserva
              AND rm.canal_venta = rh.canal_venta
              AND rm.num_reserva = rh.numrsv_padre);
   --se resetea el campo prima pendiente en caso que se inserte una rsv hija (cambio o redespacho)
   UPDATE reserva_dtl rm
      SET rm.prima_pendiente   = 0,
          rm.cant_desp         = 0,
          rm.cant_ret          = 0,
          rm.cant_re_desp      = 0,
          rm.usuario_postventa = '',
          rm.cod_motivopv      = null
    WHERE (rm.canal_venta, rm.num_reserva, rm.prima_pendiente) IN
         --busca la rsv padre a traves de las reservas que se han insertado (vnum_rsv_all)
          (SELECT rd.canal_venta,
                  rh.numrsv_padre,
                  rd.num_reserva
             FROM reserva_dtl rd,
                  reserva_hdr rh,
                  (SELECT regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) AS rsv
                     FROM dual
                   CONNECT BY regexp_substr(vnum_rsv_all, '[^;]+', 1, LEVEL) IS NOT NULL) l
            WHERE rd.canal_venta = rh.canal_venta
              AND rd.num_reserva = rh.num_reserva
              AND rh.num_reserva = l.rsv
              AND rd.num_detalle > 0)
     ;

    -------------------------- CUADRATURA CABECERA ---------------------------------
    doc               := dbms_xmldom.newDomDocument(xmltype("xml".getclobval()));
    node              := dbms_xmldom.makeNode(doc);
    nodoActual        := xslprocessor.selectSingleNode(node, '/dadActRsvDoc/dadActRsvDocChk/cuadrat_numlin_ActRsvCab','xmlns="http://openuri.org/i/dadActRsvDoc"');
    nodoActual        := xmldom.getFirstChild(nodoActual);
    valor             := dbms_xmldom.getnodevalue(nodoActual);
    --DBMS_OUTPUT.PUT_LINE('numNodesCab = ' || numNodesCab);
   -- DBMS_OUTPUT.PUT_LINE('to_number(valor) = ' || to_number(valor));
      IF numNodesCab    =0  THEN
      out_status_code := 1;
      out_status_msg  :=  'DAD: ' || 'XML MAL FORMADO';
      RETURN ;
    END IF;
    IF numNodesCab    <>to_number(valor) THEN
      out_status_code := 1;
      out_status_msg  :=  'DAD: ' || 'Error con la Cantidad de Registros en la Cabecera';
      RETURN ;
    END IF;
    nodoActual         := xslprocessor.selectSingleNode(node, '/dadActRsvDoc/dadActRsvDocChk/cuadrat_suma_ActRsvCab','xmlns="http://openuri.org/i/dadActRsvDoc"');
    nodoActual         := xmldom.getFirstChild(nodoActual);
    valor              := dbms_xmldom.getnodevalue(nodoActual);
    IF to_number(valor)<>cuadraturaCab THEN
      out_status_code  := 1;
      out_status_msg   :=  'DAD: ' || 'Error con la Cuadratura con la Cabecera ';
      RETURN ;
    END IF;
    -------------------------- CUADRATURA DETALLE---------------------------------
    doc               := dbms_xmldom.newDomDocument(xmltype("xml".getclobval()));
    node              := dbms_xmldom.makeNode(doc);
    nodoActual        := xslprocessor.selectSingleNode(node, '/dadActRsvDoc/dadActRsvDocChk/cuadrat_numlin_ActRsvCab','xmlns="http://openuri.org/i/dadActRsvDoc"');
    nodoActual        := xmldom.getFirstChild(nodoActual);
    valor             := dbms_xmldom.getnodevalue(nodoActual);
    --DBMS_OUTPUT.PUT_LINE('numNodesDet = ' || numNodesDet);
    --DBMS_OUTPUT.PUT_LINE('to_number(valor) = ' || to_number(valor));
    IF numNodesDet    <>to_number(valor) THEN
      out_status_code := 1;
      out_status_msg  :=  'DAD: ' || 'Error con la Cantidad de Registros en el Detalle';
      RETURN ;
    END IF;
    nodoActual         := xslprocessor.selectSingleNode(node, '/dadActRsvDoc/dadActRsvDocChk/cuadrat_suma_ActRsvCab','xmlns="http://openuri.org/i/dadActRsvDoc"');
    nodoActual         := xmldom.getFirstChild(nodoActual);
    valor              := dbms_xmldom.getnodevalue(nodoActual);
    IF to_number(valor)<>cuadraturaCab THEN
      out_status_code  := 1;
      out_status_msg   :=  'DAD: ' || 'Error con la Cuadratura con el Detalle';
      RETURN ;
    END IF;
  END;
END ACTUALIZAR_RESERVA;

PROCEDURE INFROMA_CIERRE_CARGA(
  "xml"             IN SYS.XMLTYPE,
  in_process_luw    IN VARCHAR2,
  OUT_STATUS_CODE   OUT NUMBER,
  OUT_STATUS_MSG    OUT VARCHAR)
AS
BEGIN
  DECLARE
    id_xml           NUMBER(12);
    doc              dbms_xmldom.DOMDocument;
    node             dbms_xmldom.DOMNode;
    hijo             dbms_xmldom.DOMNode;
    hijo2            dbms_xmldom.DOMNode;
    nodeList         dbms_xmldom.DOMNodeList;
    hijosList        dbms_xmldom.DOMNodeList;
    numNodes         NUMBER;
    numHijos         NUMBER;
    varCodPRT        NUMBER;
    nodoActual       dbms_xmldom.DOMNode;
    cuadrat_suma     NUMBER;
    cuadrat_numlin   NUMBER;
    varCodEstadoPkt  VARCHAR2(10000);
    varNumPkt        VARCHAR2(10000);
    varBumGd         VARCHAR2(10000);
    valor            VARCHAR2(10000);
    varArrayPKT      VARCHAR2(10000);
    "xmlGdd"         SYS.Xmltype;
    "hayRSVRT"       NUMBER:=0;
    "rs"             PCKTYPES.FOLIOSRS;

  BEGIN
    OUT_STATUS_CODE:=0;
    doc            := dbms_xmldom.newDomDocument(xmltype("xml".getclobval()));
    node           := dbms_xmldom.makeNode(doc);
    nodeList       := xslprocessor.selectNodes(node, '/dadCieCarDoc/dadCieCarItem','xmlns="http://openuri.org/i/dadCieCarDoc"');
    numNodes       := dbms_xmldom.getLength(nodeList);
    cuadrat_suma   :=0;
    cuadrat_numlin :=0;

    FOR rowCounter IN 0..numNodes-1 LOOP
      nodoActual      := dbms_xmldom.item(nodeList, rowCounter);
      hijo            := xslprocessor.selectSingleNode(nodoActual,'num_gd');
      hijo            := xmldom.getFirstChild(hijo);
      varBumGd        := dbms_xmldom.getnodevalue(hijo);
      hijo            := xslprocessor.selectSingleNode(nodoActual,'num_pkt');
      hijo            := xmldom.getFirstChild(hijo);
      varNumPkt       := dbms_xmldom.getnodevalue(hijo);
      hijo            := xslprocessor.selectSingleNode(nodoActual,'cod_estadopkt');
      hijo            := xmldom.getFirstChild(hijo);
      varCodEstadoPkt := dbms_xmldom.getnodevalue(hijo);
      cuadrat_suma    :=  cuadrat_suma  + varBumGd;

      BEGIN
        -- PARA LOS QUE INFROMA WMOS
        UPDATE reserva_dtl
        SET num_gd         = DECODE(NVL(num_gd, 0), 0, varBumGd, num_gd),
            cod_estadopkt  = varCodEstadoPkt,
            --cod_estadodet  = (SELECT estadospkt.cod_estadodet FROM estadospkt WHERE estadospkt.cod_estadopkt = varCodEstadoPkt)
            cod_estadodet  = 11,
            usuario_evento = 'WMOS_INFCICARGA'
        WHERE num_pkt = cast (varNumPkt as number)
          AND cod_estadodet = 10
          AND (reserva_dtl.canal_venta, reserva_dtl.num_reserva, reserva_dtl.num_detalle) IN (
               SELECT preruteo_reservadet.canal_venta, preruteo_reservadet.num_reserva, preruteo_reservadet.num_detalle
               FROM preruteo_reservadet
               WHERE preruteo_reservadet.en_jaula   = 1
                 AND preruteo_reservadet.confirmado = 1)
        ;
        IF SQL%ROWCOUNT > 0 THEN
          varArrayPKT := varArrayPKT  || varNumPkt||' , ';
          "hayRSVRT"  := 1;
        END IF;

        SELECT "xml"
        INTO "xmlGdd"
        FROM (SELECT XMLElement("GuiaDeDespacho",
                                XMLElement("CodHojaRuta",0),
                                XMLElement("Usuario",''),
                                XMLElement("RutChofer",''),
                                XMLElement("Patente",''),
                                XMLAgg(XMLElement("Reserva",
                                                  XMLElement("CentroCosto",pre.ccosto),
                                                  XMLElement("NroReserva",pre.num_reserva),
                                                  XMLElement("NroDetalle",pre.num_detalle)
                                                  )
                                      )
                                ) AS "xml"
              FROM preruteo_reservadet pre
              INNER JOIN reserva_dtl res
                  ON  res.canal_venta = pre.canal_venta
                  AND res.num_reserva = pre.num_reserva
                  AND res.num_detalle = pre.num_detalle
              --WHERE res.num_pkt =
              WHERE res.cod_estadodet = 11
                AND res.bloqueado      = 0
                AND pre.confirmado     = 1
                AND pre.en_jaula       = 1
                AND res.tipo_despacho = 'R' AND res.num_detalle < 0
                --CONDICION QUE  SEA UNA RESERRVA DE CAMBIO
                AND ABS(res.num_detalle) IN (
                    SELECT dtl.num_detalle
                    FROM reserva_dtl dtl
                    WHERE dtl.canal_venta = pre.canal_venta
                      AND dtl.num_reserva = pre.num_reserva
                      AND dtl.num_pkt = CAST(varNumPkt AS NUMBER)
                      --AND ABS(dtl.num_detalle) = ABS(pre.num_detalle)
                      AND dtl.num_detalle > 0)
             )
        ;

        IF "hayRSVRT" > 0 THEN
            "PCKGENERAGDDSAVE"("xmlGdd","rs" );
        END IF;
        --La carga puede tener PKT q no esten en DAD.
        --IF SQL%ROWCOUNT = 0 THEN
                --OUT_STATUS_CODE := 1;
                --OUT_STATUS_MSG  :=  'DAD: ' || 'No hay filas Afectadas para el Nro PKT :' ||varNumPkt ;
                --RETURN;
        --END IF;

      EXCEPTION
        WHEN OTHERS THEN
          OUT_STATUS_CODE := 1;
          OUT_STATUS_MSG  :=  'DAD: ' || SQLERRM;
          RETURN;
      END;

    END LOOP;

    varArrayPKT :=SUBSTR(varArrayPKT,0,LENGTH(varArrayPKT)-2);

    doc               := dbms_xmldom.newDomDocument(xmltype("xml".getclobval()));
    node              := dbms_xmldom.makeNode(doc);
    nodoActual        := xslprocessor.selectSingleNode(node, '/dadCieCarDoc/dadCieCarDocChk/cuadrat_numlin','xmlns="http://openuri.org/i/dadCieCarDoc"');
    nodoActual        := xmldom.getFirstChild(nodoActual);
    valor             := dbms_xmldom.getnodevalue(nodoActual);
    IF numNodes    <>to_number(valor) THEN
      out_status_code := 1;
      out_status_msg  :=  'DAD: ' || 'Error con la Cantidad de Registros en la Cabecera';
      RETURN ;
    END IF;
    nodoActual         := xslprocessor.selectSingleNode(node, '/dadCieCarDoc/dadCieCarDocChk/cuadrat_suma','xmlns="http://openuri.org/i/dadCieCaraDoc"');
    nodoActual         := xmldom.getFirstChild(nodoActual);
    valor              := dbms_xmldom.getnodevalue(nodoActual);
    IF to_number(valor) <> cuadrat_suma THEN
      out_status_code  := 1;
      out_status_msg   :=  'DAD: ' || 'Error con la Cuadratura con la Cabecera ';
      RETURN ;
    END IF;

    -- para los que no informa wmos
    -- QUERY PARA VOLVER AL ESTADO 9 LOS NO INFORMADOS
    -- Se comenta por problemas en produccion y a la espera de lo desarrollado por xintec

    /*if varArrayPKT is not null then
          execute immediate   'update RESERVA_DTL set RESERVA_DTL.cod_estadodet= 9, usuario_evento = ''WMOS_INFCICARGA''
              where ( RESERVA_DTL.NUM_PKT )  NOT IN ( '||varArrayPKT||'  )
              and cod_estadodet=10
              AND  ( RESERVA_DTL.CANAL_VENTA , RESERVA_DTL.NUM_RESERVA  , RESERVA_DTL.NUM_DETALLE  ) IN
              (SELECT PRERUTEO_RESERVADET.CANAL_VENTA, PRERUTEO_RESERVADET.NUM_RESERVA,PRERUTEO_RESERVADET.NUM_DETALLE
              FROM PRERUTEO_RESERVADET WHERE  PRERUTEO_RESERVADET.EN_JAULA=0   AND  (PRERUTEO_RESERVADET.COD_PRERUTEO,PRERUTEO_RESERVADET.NUM_JAULA ) IN (
              SELECT PRERUTEO_RESERVADET.COD_PRERUTEO , PRERUTEO_RESERVADET.NUM_JAULA FROM PRERUTEO_RESERVADET ,RESERVA_DTL
              WHERE    RESERVA_DTL.NUM_PKT IN (  '||varArrayPKT||' )
              AND RESERVA_DTL.CANAL_VENTA  = PRERUTEO_RESERVADET.CANAL_VENTA
                  AND RESERVA_DTL.NUM_RESERVA     = PRERUTEO_RESERVADET.NUM_RESERVA
                  AND RESERVA_DTL.NUM_DETALLE     = PRERUTEO_RESERVADET.NUM_DETALLE) )';

        --QUERY PARA PONER EN ESTADO 0 EL CAMPO EN JAULA A LOS NO INFORMADOS POR WMOS
        \+
        SOLE SE SACA DE LA JAULA EN la confirmacion de hoja ruta

          execute immediate  '  update PRERUTEO_RESERVADET SET EN_JAULA=0
              where    ( PRERUTEO_RESERVADET.CANAL_VENTA , PRERUTEO_RESERVADET.NUM_RESERVA  , PRERUTEO_RESERVADET.NUM_DETALLE  ) IN
              (SELECT RESERVA_DTL.CANAL_VENTA, RESERVA_DTL.NUM_RESERVA,RESERVA_DTL.NUM_DETALLE
              FROM RESERVA_DTL WHERE RESERVA_DTL.NUM_PKT  NOT IN (  '||varArrayPKT||' ))
              AND (PRERUTEO_RESERVADET.COD_PRERUTEO,PRERUTEO_RESERVADET.NUM_JAULA) IN

           ( SELECT PRERUTEO_RESERVADET.COD_PRERUTEO,PRERUTEO_RESERVADET.NUM_JAULA FROM PRERUTEO_RESERVADET ,RESERVA_DTL
              WHERE    RESERVA_DTL.NUM_PKT IN (  '||varArrayPKT||' )
              AND RESERVA_DTL.CANAL_VENTA  = PRERUTEO_RESERVADET.CANAL_VENTA
                  AND RESERVA_DTL.NUM_RESERVA     = PRERUTEO_RESERVADET.NUM_RESERVA
                  AND RESERVA_DTL.NUM_DETALLE     = PRERUTEO_RESERVADET.NUM_DETALLE) ';+\
      end if;*/

  END;
END INFROMA_CIERRE_CARGA;

PROCEDURE INGRESAR_CENTRO_COSTO (
    "xml"            IN  SYS.XMLTYPE,
    in_process_luw   IN  VARCHAR2,
    OUT_STATUS_CODE  OUT NUMBER,
    OUT_STATUS_MSG   OUT VARCHAR )
AS
BEGIN
   DECLARE
    id_xml         NUMBER(12);
    doc dbms_xmldom.DOMDocument;
    node dbms_xmldom.DOMNode;
    hijo dbms_xmldom.DOMNode;
    hijo2 dbms_xmldom.DOMNode;
    nodeList dbms_xmldom.DOMNodeList;
    hijosList dbms_xmldom.DOMNodeList;
    numNodes NUMBER;
    numHijos NUMBER;
    varCodPRT NUMBER;
    nodoActual dbms_xmldom.DOMNode;
    cuadrat_suma          NUMBER;
    cuadrat_numlin        NUMBER;
    valor          VARCHAR2(500);

    varOrg_lvl_number    NUMBER;
    varOrg_name_short    VARCHAR2(1000);
    varOrg_name_full    VARCHAR2(1000);
    varOrg_lvl_active    VARCHAR2(1000);
    varBas_addr_1    VARCHAR2(1000);
    varBas_addr_2    VARCHAR2(1000);
    varBas_addr_3    VARCHAR2(1000);
    varBas_city    VARCHAR2(1000);
    varBas_state    VARCHAR2(1000);
    varBas_phone_numb    VARCHAR2(1000);
    varBas_fax_number    VARCHAR2(1000);
    varBas_contact_name    VARCHAR2(1000);
    varBas_email    VARCHAR2(1000);
    varOrg_is_store    VARCHAR2(1000);
    varOrg_tipo_bod    VARCHAR2(1000);
    varDespacha    NUMBER;
    varTpo_procesoprod    NUMBER;
    varDias_despacho    VARCHAR2(1000);
    varTipo_stocks    NUMBER;
    varTipo_stockp    NUMBER;
    varCant_desp    NUMBER;
    varDesactivado    NUMBER;

  BEGIN
    OUT_STATUS_CODE:=0;
    doc            := dbms_xmldom.newDomDocument(xmltype("xml".getclobval()));
    node           := dbms_xmldom.makeNode(doc);
    nodeList       := xslprocessor.selectNodes(node, '/dadStMaDoc/dadStMaItem','xmlns="http://openuri.org/i/dadStMaDoc"');
    numNodes       := dbms_xmldom.getLength(nodeList);
    cuadrat_suma   :=0;
    cuadrat_numlin :=0;
    FOR rowCounter IN 0..numNodes-1
    LOOP
      nodoActual      := dbms_xmldom.item(nodeList, rowCounter);

      hijo            := xslprocessor.selectSingleNode(nodoActual,'org_lvl_number');
      hijo            := xmldom.getFirstChild(hijo);
      varOrg_lvl_number:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'org_name_short');
      hijo            := xmldom.getFirstChild(hijo);
      varOrg_name_short:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'org_name_full');
      hijo            := xmldom.getFirstChild(hijo);
      varOrg_name_full:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'org_lvl_active');
      hijo            := xmldom.getFirstChild(hijo);
      varOrg_lvl_active:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'bas_addr_1');
      hijo            := xmldom.getFirstChild(hijo);
      varBas_addr_1:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'bas_addr_2');
      hijo            := xmldom.getFirstChild(hijo);
      varBas_addr_2:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'bas_addr_3');
      hijo            := xmldom.getFirstChild(hijo);
      varBas_addr_3:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'bas_city');
      hijo            := xmldom.getFirstChild(hijo);
      varBas_city:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'bas_state');
      hijo            := xmldom.getFirstChild(hijo);
      varBas_state:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'bas_phone_numb');
      hijo            := xmldom.getFirstChild(hijo);
      varBas_phone_numb:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'bas_fax_number');
      hijo            := xmldom.getFirstChild(hijo);
      varBas_fax_number:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'bas_contact_name');
      hijo            := xmldom.getFirstChild(hijo);
      varBas_contact_name:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'bas_email');
      hijo            := xmldom.getFirstChild(hijo);
      varBas_email:= dbms_xmldom.getnodevalue(hijo);

      /*
      hijo            := xslprocessor.selectSingleNode(nodoActual,'org_is_store');
      hijo            := xmldom.getFirstChild(hijo);
      varOrg_is_store:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'org_tipo_bod');
      hijo            := xmldom.getFirstChild(hijo);
      varOrg_tipo_bod:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'despacha');
      hijo            := xmldom.getFirstChild(hijo);
      varDespacha:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'tpo_procesoprod');
      hijo            := xmldom.getFirstChild(hijo);
      varTpo_procesoprod:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'dias_despacho');
      hijo            := xmldom.getFirstChild(hijo);
      varDias_despacho:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'tipo_stocks');
      hijo            := xmldom.getFirstChild(hijo);
      varTipo_stocks:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'tipo_stockp');
      hijo            := xmldom.getFirstChild(hijo);
      varTipo_stockp:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'cant_desp');
      hijo            := xmldom.getFirstChild(hijo);
      varCant_desp:= dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'desactivado');
      hijo            := xmldom.getFirstChild(hijo);
      varDesactivado:= dbms_xmldom.getnodevalue(hijo); */


      cuadrat_suma    :=  cuadrat_suma  + varOrg_lvl_number;
      BEGIN
      insert into centro_costo (org_lvl_number,  org_name_short,  org_name_full,  org_lvl_active,  bas_addr_1,  bas_addr_2,  bas_addr_3,  bas_city,  bas_state,  bas_phone_numb,  bas_fax_number,  bas_contact_name,  bas_email, nombre_cc)  --org_is_store,  org_tipo_bod, despacha,  tpo_procesoprod,  dias_despacho,  tipo_stocks,  tipo_stockp,  cant_desp,  desactivado)
              values (varOrg_lvl_number,  varOrg_name_short,  varOrg_name_full,  varOrg_lvl_active,  varBas_addr_1,  varBas_addr_2,  varBas_addr_3,  varBas_city,  varBas_state,  varBas_phone_numb,  varBas_fax_number,  varBas_contact_name,  varBas_email, varOrg_name_full);  --varOrg_is_store,  varOrg_tipo_bod, varDespacha,  varTpo_procesoprod,  varDias_despacho,  varTipo_stocks,  varTipo_stockp,  varCant_desp,  varDesactivado);
      EXCEPTION
      when DUP_VAL_ON_INDEX THEN
            BEGIN
              UPDATE centro_costo
              SET org_lvl_number = varOrg_lvl_number,
                  org_name_short = varOrg_name_short,
                  org_name_full = varOrg_name_full,
                  org_lvl_active = varOrg_lvl_active,
                  bas_addr_1 = varBas_addr_1,
                  --bas_addr_2 = varBas_addr_2,
                  --bas_addr_3 = varBas_addr_3,
                  --bas_city = varBas_city,
                  bas_state = varBas_state,
                  bas_phone_numb = varBas_phone_numb,
                  bas_fax_number = varBas_fax_number,
                  bas_contact_name = varBas_contact_name,
                  bas_email = varBas_email
                  --org_is_store = varOrg_is_store,
                  --org_tipo_bod = varOrg_tipo_bod
                  --despacha = varDespacha,
                  --tpo_procesoprod = varTpo_procesoprod,
                  --dias_despacho = varDias_despacho,
                  --tipo_stocks = varTipo_stocks,
                  --tipo_stockp = varTipo_stockp,
                  --cant_desp = varCant_desp,
                  --desactivado = varDesactivado
              WHERE org_lvl_number = varOrg_lvl_number;
            EXCEPTION
            WHEN OTHERS THEN
              OUT_STATUS_CODE := 1;
              OUT_STATUS_MSG  :=  'DAD: ' || SQLERRM;
              RETURN;
            END;
      END;
    END LOOP;

    --CUADRATURA CON RESPECTO A LA CANTIDAD DE LINEAS
    doc               := dbms_xmldom.newDomDocument(xmltype("xml".getclobval()));
    node              := dbms_xmldom.makeNode(doc);
    nodoActual        := xslprocessor.selectSingleNode(node, '/dadStMaDoc/dadStMaDocChk/cuadrat_numlin','xmlns="http://openuri.org/i/dadStMaDoc"');
    nodoActual        := xmldom.getFirstChild(nodoActual);
    valor             := dbms_xmldom.getnodevalue(nodoActual);
    IF numNodes    <>to_number(valor) THEN
      out_status_code := 1;
      out_status_msg  :=  'DAD: ' || 'Error en el proceso de cuadratura, cantidad de lineas incorrecta.';
      RETURN ;
    END IF;
    --CUADRATURA CON RESPECTO A LA SUMA
    nodoActual         := xslprocessor.selectSingleNode(node, '/dadStMaDoc/dadStMaDocChk/cuadrat_suma','xmlns="http://openuri.org/i/dadStMaDoc"');
    nodoActual         := xmldom.getFirstChild(nodoActual);
    valor              := dbms_xmldom.getnodevalue(nodoActual);
    IF to_number(valor) <> cuadrat_suma THEN
      out_status_code  := 1;
      out_status_msg   :=  'DAD: ' || 'Error en el proceso de cuadratura, la suma no coincide.';
      RETURN ;
    END IF;

  END;
END INGRESAR_CENTRO_COSTO;

PROCEDURE ADD_REQUEST(
    varNumReserva     IN NUMBER,
    varCodTipoReq     IN NUMBER,
    varCcReq    IN NUMBER,
    varObs IN VARCHAR2,
    varLoginUsr IN VARCHAR2,
    OUT_STATUS_CODE OUT NUMBER,
    OUT_STATUS_MSG OUT VARCHAR
)
AS
  BEGIN
  DECLARE
    varCanalVenta NUMBER;

    BEGIN
    OUT_STATUS_CODE       := 0;
    OUT_STATUS_MSG        := 'OK';
      BEGIN
          SELECT canal_venta INTO varCanalVenta FROM reserva_hdr WHERE num_reserva = varNumReserva;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          OUT_STATUS_MSG:= 'El numero de reserva ingresado no existe';
          OUT_STATUS_CODE:=1;
          RETURN;
        WHEN OTHERS THEN
          OUT_STATUS_MSG:= SQLERRM;
          OUT_STATUS_CODE:=1;
          RETURN;
      END;

      INSERT INTO requerimiento (ID_REQ,CANAL_VENTA, NUM_RESERVA,COD_TIPOREQ,CC_REQ,FECHA_REQ,OBSERVACIONES,LOGIN_USUARIO)
      VALUES (SEQ_TREQUERIMIENTO.NEXTVAL, varCanalVenta, varNumReserva,varCodTipoReq,varCcReq, to_date(sysdate,'DD/MM/YY'), varObs, varLoginUsr);

    END;
  END ADD_REQUEST;

-- Procedimiento para aplicar nota de credito por anulacion de pendientes
-- accion 1: valida NCR y bloquea el/los detalle(s) involucrados
-- accion 0: valida y aplica la NCR (cambia el estado)
-- accion 2: valida y aplica la NCR (cambia el estado)
PROCEDURE ACTUALIZA_ESTADO_RESERVA(
  "xml"             IN SYS.XMLTYPE,
  "accion"          IN NUMBER,
  in_process_luw    IN VARCHAR2,
  OUT_REPROCESA_NCR OUT NUMBER,
  OUT_STATUS_CODE   OUT NUMBER,
  OUT_STATUS_MSG    OUT VARCHAR) AS
BEGIN

  DECLARE
    doc              dbms_xmldom.domdocument;
    node             dbms_xmldom.domnode;
    hijo             dbms_xmldom.domnode;
    nodoactual       dbms_xmldom.domnode;
    nodelist         dbms_xmldom.domnodelist;
    numnodes         NUMBER;
    varcanalVenta    RESERVA_DTL.CANAL_VENTA%TYPE;
    reserva          RESERVA_DTL.NUM_RESERVA%TYPE;
    reserva_hija     RESERVA_DTL.NUM_RESERVA%TYPE;
    var_rsv_hija     RESERVA_DTL.NUM_RESERVA%TYPE;
    detalle          RESERVA_DTL.NUM_DETALLE%TYPE;
    estado           RESERVA_DTL.COD_ESTADODET%TYPE;
    numncr           reserva_dtl.num_ncr%TYPE;
    usuario          reserva_dtl.usuario_evento%TYPE;
    sku              reserva_dtl.prd_lvl_number%TYPE;
    cant             reserva_dtl.cantidad%TYPE;
    cant_desp        reserva_dtl.cantidad%TYPE;
    cant_hija        reserva_dtl.cantidad%TYPE;
    cant_ncr         reserva_dtl.cant_ncr%TYPE;
    vsub_orden       reserva_dtl.sub_orden%TYPE;
    estado_act       reserva_dtl.cod_estadodet%TYPE;
    estado_act_hija  reserva_dtl.cod_estadodet%TYPE;
    var_num_ncr      reserva_dtl.num_ncr%TYPE;
    tipoNcr          reserva_dtl.tipo_ncr%TYPE;
    hijas            NUMBER;
    var_cant_ncr     NUMBER;
    vncr             estadosdet.ncr%TYPE;
    vdetiene_ncr     estadosdet.detiene_ncr%TYPE;
    var_tipo_reserva reserva_hdr.tipo_reserva%TYPE;

  BEGIN
    doc              := dbms_xmldom.newDomDocument(xmltype("xml".getclobval()));
    node             := dbms_xmldom.makeNode(doc);
    nodeList         := xslprocessor.selectNodes(node, '/dad:dadActRsvPorNCRDoc/dad:dadActRsvPorNCRDocDet','xmlns:dad="http://openuri.org/i/dadActRsvPorNCRDoc"');
    numNodes         := dbms_xmldom.getLength(nodeList);
    out_status_code  := 0;
    out_status_msg   := 'OK';
    hijas            := 0;
    var_cant_ncr     := 0;
    vncr             := '';
    reserva_hija     := 0;
    estado_act_hija  := 0;
    var_rsv_hija     := 0;
    var_tipo_reserva := '';

    --INI: recorre cada linea de detalle del xml
    FOR rowcounter IN 0 .. numnodes - 1
    LOOP
        hijas             := 0;
        var_cant_ncr      := 0;
        vncr              := '';
        OUT_REPROCESA_NCR := 0;

        nodoactual := dbms_xmldom.item (nodelist, rowcounter);
        hijo := xslprocessor.selectsinglenode (nodoactual, 'canal_venta','xmlns:dad="http://openuri.org/i/dadActRsvPorNCRDoc"');
        hijo := xmldom.getfirstchild (hijo);
        varcanalventa := dbms_xmldom.getnodevalue (hijo);

        hijo := xslprocessor.selectsinglenode (nodoactual, 'num_reserva','xmlns:dad="http://openuri.org/i/dadActRsvPorNCRDoc"');
        hijo := xmldom.getfirstchild (hijo);
        reserva := dbms_xmldom.getnodevalue (hijo);

        hijo :=  xslprocessor.selectsinglenode (nodoactual, 'num_detalle','xmlns:dad="http://openuri.org/i/dadActRsvPorNCRDoc"');
        hijo := xmldom.getfirstchild (hijo);
        detalle := dbms_xmldom.getnodevalue (hijo);

        hijo :=  xslprocessor.selectsinglenode (nodoactual, 'cod_estadodet','xmlns:dad="http://openuri.org/i/dadActRsvPorNCRDoc"');
        hijo := xmldom.getfirstchild (hijo);
        estado := dbms_xmldom.getnodevalue (hijo);

        hijo :=  xslprocessor.selectsinglenode (nodoactual, 'num_ncr','xmlns:dad="http://openuri.org/i/dadActRsvPorNCRDoc"');
        hijo := xmldom.getfirstchild (hijo);
        numncr := dbms_xmldom.getnodevalue (hijo);

        hijo :=  xslprocessor.selectsinglenode (nodoactual, 'usuario','xmlns:dad="http://openuri.org/i/dadActRsvPorNCRDoc"');
        hijo := xmldom.getfirstchild (hijo);
        usuario := dbms_xmldom.getnodevalue (hijo);

        hijo :=  xslprocessor.selectsinglenode (nodoactual, 'sku','xmlns:dad="http://openuri.org/i/dadActRsvPorNCRDoc"');
        hijo := xmldom.getfirstchild (hijo);
        sku := dbms_xmldom.getnodevalue (hijo);

        hijo :=  xslprocessor.selectsinglenode (nodoactual, 'cantidad','xmlns:dad="http://openuri.org/i/dadActRsvPorNCRDoc"');
        hijo := xmldom.getfirstchild (hijo);
        cant := dbms_xmldom.getnodevalue (hijo);

        hijo :=  xslprocessor.selectsinglenode (nodoactual, 'tipoNcr','xmlns:dad="http://openuri.org/i/dadActRsvPorNCRDoc"');
        hijo := xmldom.getfirstchild (hijo);
        tipoNcr := dbms_xmldom.getnodevalue (hijo);

        --INI: validaciones NCR

          --valida que no se haya aplicado anteriormente la NCR
          IF "accion" = 2 THEN

            SELECT COUNT(1)
              INTO var_cant_ncr
              FROM reserva_hdr rh, reserva_dtl dr
             WHERE rh.canal_venta   = dr.canal_venta
               AND rh.num_reserva   = dr.num_reserva
               AND (rh.numrsv_padre = reserva OR rh.num_reserva = reserva)
               AND dr.num_detalle   = abs(detalle)
               AND dr.num_ncr       = numncr
               AND dr.cod_estadoncr > 1;

            IF var_cant_ncr > 0 THEN
              out_status_code := -1;
              out_status_msg  := 'DAD: NCR Procesada';
              RETURN;
            END IF;

          END IF;

          --valida que en xml de entrada venga el estado Nota Credito
          IF (estado != 18) THEN

            out_status_code := -1;
            out_status_msg  := 'DAD: solo permite NCR para este servicio; reserva ' ||reserva|| ', estado ' ||estado;
            RETURN;

          END IF;

          --cuenta si tiene reservas hijas (retiros) o redespachos
          SELECT COUNT(1)
            INTO hijas
            FROM reserva_hdr rh,
                 reserva_dtl dr
           WHERE rh.canal_venta = dr.canal_venta
             AND rh.num_reserva = dr.num_reserva
             AND rh.numrsv_padre = reserva
             AND ((dr.num_detalle = -detalle) OR
                 (rh.tipo_reserva = 'R' AND dr.num_detalle = detalle))
             AND dr.cod_estadodet NOT IN (23, 123);

          --si no tiene reservas hijas (retiros) o redespachos, analiza la reserva madre
          IF (hijas = 0) THEN

            SELECT NVL(ed.ncr,'T'),
                   cantidad
              INTO vncr,
                   cant_desp
              FROM reserva_dtl rd,
                   estadosdet  ed
             WHERE rd.canal_venta   = varcanalventa
               AND rd.num_reserva   = reserva
               AND rd.num_detalle   = detalle
               AND rd.cod_estadodet = ed.cod_estadodet;

            --Para reservas madres no permite 'F', si permite NULL, 'T' y 'C'
            IF (vncr = 'F') THEN
              out_status_code := 1;
              out_status_msg  := 'DAD: No se puede generar NCR. Validar estado DAD.';
              RETURN;
            END IF;

            --valida que la cantidad del xml sea igual a la cantidad en DAD, no puede ser distinta
            IF (cant_desp != cant) THEN
               out_status_code := 1;
               out_status_msg  := 'DAD: NCR debe ser por total.';
               RETURN;
            END IF;

          --si tiene reservas hijas (retiros) o redespachos, se analiza(n) la(s) reserva(s) hija(s)
          ELSE
            BEGIN

              BEGIN
                 --Obteniendo reserva hija
                 SELECT num_reserva,
                        tipo_reserva
                   INTO var_rsv_hija,
                        var_tipo_reserva
                   FROM (SELECT rh.num_reserva,
                                rh.tipo_reserva
                           FROM reserva_hdr rh,
                                reserva_dtl rd
                          WHERE rd.num_ordenventa = rh.num_ordenventa
                            AND rd.num_reserva = rh.num_reserva
                            AND rd.canal_venta = rh.canal_venta
                            AND rh.numrsv_padre = reserva
                            AND rd.canal_venta = varcanalventa
                            AND abs(rd.num_detalle) = detalle
                            AND rd.cantidad = cant
                            AND rd.cod_estadodet NOT IN (23, 123)
                          ORDER BY fecha_crea DESC)
                  WHERE rownum = 1;
              EXCEPTION
                 WHEN no_data_found THEN
                    out_status_code := 1;
                    out_status_msg  := 'DAD: No se puede generar NCR. Validar cantidad';
                    RETURN;
              END;

              IF (var_tipo_reserva = 'R') THEN

                --Validando estado de la reserva (Redespacho)
                SELECT rh.num_reserva,
                       rd.cantidad
                  INTO reserva_hija,
                       cant_hija
                  FROM reserva_hdr rh,
                       reserva_dtl rd,
                       estadosdet  ed
                 WHERE rd.num_ordenventa = rh.num_ordenventa
                   AND rd.num_reserva = rh.num_reserva
                   AND rd.canal_venta = rh.canal_venta
                   AND rd.cod_estadodet = ed.cod_estadodet
                   AND rh.num_reserva = var_rsv_hija
                   AND rd.num_detalle = detalle
                   --Validando estado redespacho NCR
                   AND (ed.ncr IS NULL OR nvl(ed.ncr, 'F') IN ('C', 'T'))
                   --Validando estado reserva padre
                   AND EXISTS (SELECT NULL
                                 FROM reserva_hdr rh_aux,
                                      reserva_dtl rd_aux,
                                      estadosdet  ed_aux
                                WHERE rd_aux.num_ordenventa = rh_aux.num_ordenventa
                                  AND rd_aux.num_reserva = rh_aux.num_reserva
                                  AND rd_aux.canal_venta = rh_aux.canal_venta
                                  AND rd_aux.cod_estadodet = ed_aux.cod_estadodet
                                  AND rh_aux.num_reserva = rh.numrsv_padre
                                  AND rd_aux.num_detalle = rd.num_detalle
                                  AND nvl(ed_aux.ncr, 'F') IN ('C', 'T'))
                   --Validando (si existe) estado reservas hermanas
                   AND NOT EXISTS (SELECT NULL
                                     FROM reserva_hdr rh_aux,
                                          reserva_dtl rd_aux,
                                          estadosdet  ed_aux
                                    WHERE rd_aux.num_ordenventa = rh_aux.num_ordenventa
                                      AND rd_aux.num_reserva = rh_aux.num_reserva
                                      AND rd_aux.canal_venta = rh_aux.canal_venta
                                      AND rd_aux.cod_estadodet = ed_aux.cod_estadodet
                                      AND rh_aux.numrsv_padre = rh.numrsv_padre
                                      AND rh_aux.num_reserva <> rh.num_reserva
                                      AND rd_aux.num_detalle = rd.num_detalle
                                      AND rh_aux.tipo_reserva = 'R'
                                      AND (ed_aux.ncr IS NULL OR nvl(ed_aux.ncr, 'F') = 'F'));

              ELSE

                --Validando estado de la reserva (Retiro - Cambio/Retiro)
                SELECT dr.num_reserva,
                       dr.cantidad
                  INTO reserva_hija,
                       cant_hija
                  FROM reserva_hdr rh,
                       reserva_dtl dr,
                       reserva_dtl dc,
                       estadosdet  er,
                       estadosdet  ec
                 WHERE rh.canal_venta = dr.canal_venta
                   AND rh.num_reserva = dr.num_reserva
                   AND rh.num_reserva = var_rsv_hija
                   AND dr.num_detalle   = -detalle
                   AND dr.cod_estadodet = er.cod_estadodet
                   AND dr.canal_venta = dc.canal_venta(+)
                   AND dr.num_reserva = dc.num_reserva(+)
                   AND dr.num_detalle = -dc.num_detalle(+)
                   AND dc.cod_estadodet = ec.cod_estadodet(+)
                   AND dr.cod_estadodet NOT IN (23, 123)
                   --Validando estado de linea de retiro
                   AND er.ncr = 'T'
                   --Validando estado ncr despacho (si existe)
                   AND (ec.ncr IS NULL OR nvl(ec.ncr, 'F') IN ('C', 'T'));

              END IF;

               --valida que la cantidad del xml sea igual a la cantidad en DAD
              IF (cant_hija != cant) THEN
                 out_status_code := 1;
                 out_status_msg  := 'DAD: No se puede generar NCR. Validar cantidad DAD';
                 RETURN;
              END IF;

              EXCEPTION
                WHEN NO_DATA_FOUND THEN
                  out_status_code := 1;
                  out_status_msg  := 'DAD: No se puede generar NCR. Validar estado DAD';
                  RETURN;
            END;

          END IF;
        --FIN: validaciones NCR


        --INI: Aplica NCR o bloquea la reserva
        IF NVL("accion", 0) IN (0, 2) THEN--aplica la NCR

          vdetiene_ncr := 0;

          SELECT NVL(detiene_ncr, 0)
            INTO vdetiene_ncr
            FROM reserva_dtl rd,
                 estadosdet ed
           WHERE rd.cod_estadodet = ed.cod_estadodet
             AND canal_venta      = varcanalventa
             AND num_reserva      = reserva
             AND num_detalle      = detalle;
          --Se registra la cantidad de la NCR y se aplica cambio de estado solo si se esta apliando a la reseva madre
          UPDATE reserva_dtl
            SET cod_estadodet  = DECODE(hijas, 0, DECODE(vdetiene_ncr, 0, estado, cod_estadodet), cod_estadodet),
                num_ncr        = numncr,
                cant_ncr       = NVL(cant_ncr, 0) + cant,
                usuario_ncr    = usuario, -- son ambos campos usuario ? solo uno ? condicional?
                usuario_evento = DECODE(hijas, 0, usuario, usuario_evento),   -- son ambos campos usuario ? solo uno ? condicional?
                cod_estadoncr  = DECODE(hijas, 0, 2, 3), -- valido si se ha confirmando NCR o si estoy aplicando
                tipo_ncr       = tipoNcr
          WHERE canal_venta = varcanalventa
            AND num_reserva = reserva
            AND num_detalle = detalle;

          vdetiene_ncr := 0;


          BEGIN
            SELECT NVL(detiene_ncr, 0) --retiro
              INTO vdetiene_ncr
              FROM reserva_dtl rd,
                   estadosdet ed
             WHERE rd.cod_estadodet = ed.cod_estadodet
               AND canal_venta      = varcanalventa
               AND num_reserva      = reserva
               AND num_detalle      = -detalle;
          EXCEPTION
            WHEN no_data_found THEN
              NULL;
          END;

          UPDATE reserva_dtl
             SET cod_estadodet  = DECODE(cod_estadodet, 21, cod_estadodet, 121, cod_estadodet, DECODE(vdetiene_ncr, 0, estado, cod_estadodet)),
                 num_ncr        = numncr,
                 cant_ncr       = NVL(cant_ncr, 0) + cant,
                 usuario_ncr    = usuario,
                 usuario_evento = decode(cod_estadodet, 21, usuario_evento, 121, usuario_evento, usuario),
                 cod_estadoncr  = DECODE(cod_estadodet, 21, 2, 121, 2, 3), -- valido si se ha confirmando NCR o si estoy aplicando
                 tipo_ncr       = tipoNcr
          WHERE canal_venta     = varcanalventa
            AND num_reserva     = reserva_hija
            AND num_detalle     = -detalle;

        ELSIF "accion" = 1 THEN --bloquea la reserva

          UPDATE reserva_dtl
             SET num_ncr       = numncr,
                 usuario_ncr   = usuario,
                 bloqueado     = 1,
                 cod_estadoncr = 1
           WHERE canal_venta   = varcanalventa
             AND num_reserva   = reserva_hija
             AND num_detalle   = -detalle;

          UPDATE reserva_dtl
             SET num_ncr       = numncr,
                 usuario_ncr   = usuario,
                 bloqueado     = 1,
                 cod_estadoncr = 1
           WHERE canal_venta   = varcanalventa
             AND num_reserva   = reserva
             AND num_detalle   = detalle;

        END IF;
        --FIN: Aplica NCR

    END LOOP;
    --FIN: recorre cada linea de detalle del xml

    IF (out_status_code = 0 AND IN_PROCESS_LUW = 'T') THEN
      COMMIT;
    END IF;
  END;
END ACTUALIZA_ESTADO_RESERVA;

PROCEDURE ACTUALIZAR_ESTADOS_UNIGIS(
  IN_XML           IN  XMLTYPE,
  IN_PROCESS_LUW   IN  VARCHAR2 DEFAULT 'T',
  OUT_STATUS_CODE  OUT NUMBER,
  OUT_STATUS_MSG   OUT VARCHAR2) AS
BEGIN
  DECLARE
  --VARIABLES PARA EL MANEJO DEL XML
  DOC             DBMS_XMLDOM.DOMDOCUMENT;
  NODE            DBMS_XMLDOM.DOMNODE;
  HIJO            DBMS_XMLDOM.DOMNODE;
  NODELIST        DBMS_XMLDOM.DOMNODELIST;
  HIJOSLIST       DBMS_XMLDOM.DOMNODELIST;
  NUMNODES        NUMBER;
  NUMHIJOS        NUMBER;
  ROWCOUNTER      NUMBER;
  NODOACTUAL      DBMS_XMLDOM.DOMNODE;

  HIJOPRODUCTO        DBMS_XMLDOM.DOMNODE;
  HIJOSPRODUCTOSLIST  DBMS_XMLDOM.DOMNODELIST;
  NODOACTUALPRODUCTO  DBMS_XMLDOM.DOMNODE;
  NUMPRODUCTONODES    DBMS_XMLDOM.DOMNODE;
  NUMPRODUCTOSHIJOS   NUMBER;
  ROWPRODUCTOCOUNTER  NUMBER;

  HIJOMOTIVONOENTREGA       DBMS_XMLDOM.DOMNODE;
  HIJOSMOTIVONOENTREGALIST  DBMS_XMLDOM.DOMNODELIST;
  NODOACTUALMOTIVONOENTREGA DBMS_XMLDOM.DOMNODE;
  NUMMOTIVONOENTREGANODES   DBMS_XMLDOM.DOMNODE;
  NUMMOTIVONOENTREGAHIJOS   NUMBER;
  ROWMOTIVONOENTREGACOUNTER NUMBER;

  --VARIABLES DE CambioEstado
  NUMRESERVA NUMBER;
  CODESTADOCAB NUMBER;
  FECHAEVENTO DATE;
  IDVIAJE NUMBER;

  --VARIABLES DE Producto
  CANTIDADTOTAL       NUMBER;
  CANTIDADENTREGADA   NUMBER;
  NUMDETALLE          NUMBER;
  CODESTADODET        NUMBER;
  vcod_estadodet_fin  NUMBER;
  vcod_estadodet_ini  NUMBER;
  vcod_motivodev      NUMBER;

  --VARIABLES DE MotivoNoEntrega
  CODPATENTE VARCHAR2(8);
  NUMLATITUD VARCHAR2(30);
  NUMLONGITUD VARCHAR2(30);
  CODMOTIVODEV NUMBER;
  RUTRECIBE NUMBER;
  DVRUTRECIBE CHAR(1);
  CODPARENTESCO NUMBER;
  NUMBULTOS NUMBER;
  RUTTRANSPORTISTA NUMBER;
  DVRUTTRANSPORTISTA CHAR(1);

  -- VARIABLES AUXILIARES
  AUX VARCHAR2(4000);
  CANALVENTA NUMBER;

  BEGIN
    OUT_STATUS_CODE := 0;
    OUT_STATUS_MSG  := 'OK';

    DOC            := DBMS_XMLDOM.NEWDOMDOCUMENT(IN_XML);
    NODE           := DBMS_XMLDOM.MAKENODE(DOC);
    NODELIST       := XSLPROCESSOR.SELECTNODES(NODE, '/dadActualizarEstadoOrdenesDoc/CambioEstado','xmlns="http://openuri.org/i/dadActualizarEstadoOrdenesDoc"');
    NUMNODES       := DBMS_XMLDOM.GETLENGTH(NODELIST);

    --inicio loop CambioEstado****************************************************
    --******************************************************************************
    FOR ROWCOUNTER IN 0..NUMNODES-1
    LOOP

    NODOACTUAL      := DBMS_XMLDOM.ITEM(NODELIST, ROWCOUNTER);

    HIJO            := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'numReserva');
    HIJO            := XMLDOM.GETFIRSTCHILD(HIJO);
    NUMRESERVA      := DBMS_XMLDOM.GETNODEVALUE(HIJO);

    HIJO            := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'codEstadoCab');
    HIJO            := XMLDOM.GETFIRSTCHILD(HIJO);
    CODESTADOCAB      := DBMS_XMLDOM.GETNODEVALUE(HIJO);

    HIJO            := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'fechaEvento');
    HIJO            := XMLDOM.GETFIRSTCHILD(HIJO);
    AUX             := DBMS_XMLDOM.GETNODEVALUE(HIJO);
    FECHAEVENTO     := TO_DATE(AUX,'DD/MM/YYYY');
    /*IF AUX IS NULL OR AUX = '' THEN
        FECHAEVENTO := TO_DATE(AUX,'DD/MM/YYYY');
    END IF;*/

    HIJO            := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'idViaje');
    HIJO            := XMLDOM.GETFIRSTCHILD(HIJO);
    IDVIAJE     := DBMS_XMLDOM.GETNODEVALUE(HIJO);

    HIJOSPRODUCTOSLIST := XSLPROCESSOR.SELECTNODES(NODOACTUAL, 'Producto');
    NUMPRODUCTOSHIJOS  := DBMS_XMLDOM.GETLENGTH(HIJOSPRODUCTOSLIST);
    END LOOP;
    --fin loop CambioEstado*********************************************************
    --********************************************************************************

    HIJOSMOTIVONOENTREGALIST       := XSLPROCESSOR.SELECTNODES(NODE, '/dadActualizarEstadoOrdenesDoc/MotivoNoEntrega','xmlns="http://openuri.org/i/dadActualizarEstadoOrdenesDoc"');
    NUMMOTIVONOENTREGAHIJOS       := DBMS_XMLDOM.GETLENGTH(NODELIST);
    --HIJOSMOTIVONOENTREGALIST  := XSLPROCESSOR.SELECTNODES(NODOACTUALMOTIVONOENTREGA, '/dadActualizarEstadoOrdenesDoc/MotivoNoEntrega','xmlns:dad="http://openuri.org/i/dadActualizarEstadoOrdenesDoc"');
    --NUMMOTIVONOENTREGAHIJOS   := DBMS_XMLDOM.GETLENGTH(HIJOSMOTIVONOENTREGALIST);

    --inicio loop MotivoNoEntrega****************************************************
    --******************************************************************************
    FOR ROWMOTIVOCOUNTER IN 0..NUMMOTIVONOENTREGAHIJOS-1
    LOOP

    NODOACTUALMOTIVONOENTREGA       := DBMS_XMLDOM.ITEM(HIJOSMOTIVONOENTREGALIST, ROWMOTIVOCOUNTER);

    HIJOMOTIVONOENTREGA             := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUALMOTIVONOENTREGA,'codPatente');
    HIJOMOTIVONOENTREGA             := XMLDOM.GETFIRSTCHILD(HIJOMOTIVONOENTREGA);
    CODPATENTE                      := DBMS_XMLDOM.GETNODEVALUE(HIJOMOTIVONOENTREGA);

    HIJOMOTIVONOENTREGA             := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUALMOTIVONOENTREGA,'numLatitud');
    HIJOMOTIVONOENTREGA             := XMLDOM.GETFIRSTCHILD(HIJOMOTIVONOENTREGA);
    NUMLATITUD                      := DBMS_XMLDOM.GETNODEVALUE(HIJOMOTIVONOENTREGA);

    HIJOMOTIVONOENTREGA             := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUALMOTIVONOENTREGA,'numLongitud');
    HIJOMOTIVONOENTREGA             := XMLDOM.GETFIRSTCHILD(HIJOMOTIVONOENTREGA);
    NUMLONGITUD                     := DBMS_XMLDOM.GETNODEVALUE(HIJOMOTIVONOENTREGA);

    HIJOMOTIVONOENTREGA             := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUALMOTIVONOENTREGA,'codMotivoDev');
    HIJOMOTIVONOENTREGA             := XMLDOM.GETFIRSTCHILD(HIJOMOTIVONOENTREGA);
    CODMOTIVODEV                    := DBMS_XMLDOM.GETNODEVALUE(HIJOMOTIVONOENTREGA);

    HIJOMOTIVONOENTREGA             := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUALMOTIVONOENTREGA,'rutRecibe');
    HIJOMOTIVONOENTREGA             := XMLDOM.GETFIRSTCHILD(HIJOMOTIVONOENTREGA);
    RUTRECIBE                       := DBMS_XMLDOM.GETNODEVALUE(HIJOMOTIVONOENTREGA);

    HIJOMOTIVONOENTREGA             := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUALMOTIVONOENTREGA,'dvRutRecibe');
    HIJOMOTIVONOENTREGA             := XMLDOM.GETFIRSTCHILD(HIJOMOTIVONOENTREGA);
    DVRUTRECIBE                     := DBMS_XMLDOM.GETNODEVALUE(HIJOMOTIVONOENTREGA);

    HIJOMOTIVONOENTREGA             := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUALMOTIVONOENTREGA,'codParentesco');
    HIJOMOTIVONOENTREGA             := XMLDOM.GETFIRSTCHILD(HIJOMOTIVONOENTREGA);
    CODPARENTESCO                   := DBMS_XMLDOM.GETNODEVALUE(HIJOMOTIVONOENTREGA);

    HIJOMOTIVONOENTREGA             := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUALMOTIVONOENTREGA,'numBultos');
    HIJOMOTIVONOENTREGA             := XMLDOM.GETFIRSTCHILD(HIJOMOTIVONOENTREGA);
    NUMBULTOS                       := DBMS_XMLDOM.GETNODEVALUE(HIJOMOTIVONOENTREGA);

    HIJOMOTIVONOENTREGA             := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUALMOTIVONOENTREGA,'rutTransportista');
    HIJOMOTIVONOENTREGA             := XMLDOM.GETFIRSTCHILD(HIJOMOTIVONOENTREGA);
    RUTTRANSPORTISTA                := DBMS_XMLDOM.GETNODEVALUE(HIJOMOTIVONOENTREGA);

    HIJOMOTIVONOENTREGA             := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUALMOTIVONOENTREGA,'dvRutTransportista');
    HIJOMOTIVONOENTREGA             := XMLDOM.GETFIRSTCHILD(HIJOMOTIVONOENTREGA);
    DVRUTTRANSPORTISTA              := DBMS_XMLDOM.GETNODEVALUE(HIJOMOTIVONOENTREGA);
    END LOOP;
    --fin loop MotivoNoEntrega*********************************************************
    --********************************************************************************

    --inicio loop Producto****************************************************
    --******************************************************************************
    FOR ROWPRODUCTOCOUNTER IN 0..NUMPRODUCTOSHIJOS-1
    LOOP

    NODOACTUALPRODUCTO      := DBMS_XMLDOM.ITEM(HIJOSPRODUCTOSLIST, ROWPRODUCTOCOUNTER);

    HIJOPRODUCTO            := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUALPRODUCTO,'cantidadTotal');
    HIJOPRODUCTO            := XMLDOM.GETFIRSTCHILD(HIJOPRODUCTO);
    CANTIDADTOTAL           := DBMS_XMLDOM.GETNODEVALUE(HIJOPRODUCTO);

    HIJOPRODUCTO            := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUALPRODUCTO,'cantidadEntregada');
    HIJOPRODUCTO            := XMLDOM.GETFIRSTCHILD(HIJOPRODUCTO);
    CANTIDADENTREGADA       := DBMS_XMLDOM.GETNODEVALUE(HIJOPRODUCTO);

    HIJOPRODUCTO            := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUALPRODUCTO,'numDetalle');
    HIJOPRODUCTO            := XMLDOM.GETFIRSTCHILD(HIJOPRODUCTO);
    NUMDETALLE              := DBMS_XMLDOM.GETNODEVALUE(HIJOPRODUCTO);

    HIJOPRODUCTO            := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUALPRODUCTO,'codEstadoDet');
    HIJOPRODUCTO            := XMLDOM.GETFIRSTCHILD(HIJOPRODUCTO);
    CODESTADODET            := DBMS_XMLDOM.GETNODEVALUE(HIJOPRODUCTO);

    BEGIN
        vcod_estadodet_ini := 0;
        vcod_estadodet_fin := 0;
        vcod_motivodev     := NULL;

        SELECT rd.canal_venta, rd.cod_estadodet
        INTO CANALVENTA, vcod_estadodet_ini
        FROM reserva_dtl rd, reserva_hdr rh
        WHERE rd.canal_venta = rh.canal_venta
          AND rd.num_reserva = rh.num_reserva
          AND rd.num_reserva = NUMRESERVA
          AND rd.num_detalle = NUMDETALLE
        ;

        EXCEPTION
           WHEN NO_DATA_FOUND THEN
                OUT_STATUS_CODE := 1;
                OUT_STATUS_MSG  := 'DAD: Reserva no existe en DAD o no tiene canal de venta asociado';
                RETURN;
    END;

    INSERT INTO EVENTO_UNIGIS2 (ID_EVENTO,
                                FECHA,
                                FECHA_EVENTO,
                                CANAL_VENTA,
                                NUM_RESERVA,
                                NUM_DETALLE,
                                COD_ESTADOCAB,
                                ID_RUTAUNI,
                                PRD_LVL_NUMBER,
                                COD_ESTADODET,
                                CANT_TOTAL,
                                CANT_ENTREGADA,
                                COD_MOTIVODEV,
                                TIPO_EVENTO,
                                PATENTE,
                                LATITUD,
                                LONGITUD,
                                RUTRECIBE,
                                DVRUTRECIBE,
                                COD_PARENTESCO,
                                BULTOS,
                                FLAG_AUTORIZA,
                                RUT_TRANSP,
                                DV_RUTTRANSP)
    VALUES (SEQ_EVENTO_UNIGIS2.NEXTVAL,
            SYSDATE,
            FECHAEVENTO,
            CANALVENTA,
            NUMRESERVA,
            NUMDETALLE,
            CODESTADOCAB,
            IDVIAJE,
            NULL,               --PRD_LVL_NUMBER SKU
            CODESTADODET,       --No viene el estado de detalle, se usa el motivo de reingreso para identificarlo
                                --(0 significa se entrego/retiro ok)
            CANTIDADTOTAL,
            CANTIDADENTREGADA,
            CODMOTIVODEV,       --No debe usarse
            NULL,               --TIPO_EVENTO
            CODPATENTE,
            NUMLATITUD,
            NUMLONGITUD,
            RUTRECIBE,
            DVRUTRECIBE,
            CODPARENTESCO,
            NUMBULTOS,
            NULL,               --FLAG_AUTORIZA
            RUTTRANSPORTISTA,
            DVRUTTRANSPORTISTA
    );

    IF CODESTADOCAB IN (11, 16) THEN
      vcod_motivodev := NULL;
      SELECT DECODE(CODESTADODET, 11, 11, 16, 16, vcod_estadodet_ini)
      INTO vcod_estadodet_fin
      FROM DUAL
      ;
--    ELSIF CODESTADOCAB = 12 THEN
--      vcod_estadodet_fin := 12;
--      vcod_motivodev     := NULL;
    ELSIF CODESTADOCAB IN (15, 13, 21) THEN
      IF CODESTADODET != 0 THEN
        vcod_estadodet_fin := 15;
        vcod_motivodev     := CODESTADODET;
      ELSE --CODESTADODET = 0
        vcod_motivodev := NULL;
        IF NUMDETALLE > 0 THEN
          vcod_estadodet_fin := 13;
        ELSE --NUMDETALLE < 0
          vcod_estadodet_fin := 21;
        END IF;
      END IF;
    ELSE
      OUT_STATUS_CODE := 1;
      OUT_STATUS_MSG  := 'DAD: Estado invalido CODESTADOCAB = ' || CODESTADOCAB;
      RETURN;
    END IF;

    IF (vcod_estadodet_ini != vcod_estadodet_fin) THEN
      UPDATE reserva_dtl
      SET usuario_evento = 'UNIGIS',
          cod_motivodev  = vcod_motivodev,
          --cod_estadodet  = vcod_estadodet_fin
          cod_estadodet  = CASE
                             WHEN cod_estadodet > 100 THEN DECODE(vcod_estadodet_fin, 123, 23, vcod_estadodet_fin) + 100
                             ELSE vcod_estadodet_fin
                           END
      WHERE (canal_venta, num_reserva, num_detalle) IN (
            SELECT dtl.CANAL_VENTA,
                   dtl.NUM_RESERVA,
                   dtl.NUM_DETALLE
            FROM preruteo_reservadet pre, reserva_dtl dtl
            WHERE pre.canal_venta = dtl.canal_venta
              AND pre.num_detalle = dtl.num_detalle
              AND pre.num_reserva = dtl.num_reserva
              AND pre.num_reserva = NUMRESERVA
              AND pre.num_detalle = NUMDETALLE
              AND ((pre.informado = 0     AND vcod_estadodet_ini IN (12, 112)          AND vcod_estadodet_fin IN (15, 115, 13, 113, 21, 121))
                OR (pre.informado = 0     AND vcod_estadodet_ini IN (13, 113, 21, 121) AND vcod_estadodet_fin IN (15, 115))
                OR (pre.informado = 0     AND vcod_estadodet_ini IN (15, 115)          AND vcod_estadodet_fin IN (13, 113, 21, 121))
                OR (pre.informado IS NULL AND vcod_estadodet_ini IN (11)               AND vcod_estadodet_fin IN (16))
                OR (pre.informado IS NULL AND vcod_estadodet_ini IN (16)               AND vcod_estadodet_fin IN (11)))
              AND ((0 = (SELECT q.valor_num FROM parametros q WHERE q.cod_param = 170))
                OR (1 = (SELECT q.valor_num FROM parametros q WHERE q.cod_param = 170) AND pre.id_viaje = IDVIAJE))
            )
      ;
      IF (vcod_estadodet_fin IN (13, 113)) THEN --13 y 113
        --Si esta "PreEntregado" se autogenera el "Entregado"
        UPDATE reserva_dtl
        SET usuario_evento    = 'UNIGIS',
            cod_estadodet     = DECODE(cod_estadodet, 13, 14, 114),
            fecha_entrega_cli = SYSDATE
        WHERE (canal_venta, num_reserva, num_detalle) IN (
              SELECT dtl.CANAL_VENTA,
                     dtl.NUM_RESERVA,
                     dtl.NUM_DETALLE
              FROM preruteo_reservadet pre, reserva_dtl dtl
              WHERE pre.canal_venta = dtl.canal_venta
                AND pre.num_detalle = dtl.num_detalle
                AND pre.num_reserva = dtl.num_reserva
                AND pre.num_reserva = NUMRESERVA
                and pre.num_detalle = NUMDETALLE
                AND ((pre.informado = 0     AND vcod_estadodet_ini IN (12, 112)     AND vcod_estadodet_fin IN (13, 113))
                  OR (pre.informado = 0     AND vcod_estadodet_ini IN (15, 115)     AND vcod_estadodet_fin IN (13, 113)))
                AND ((0 = (SELECT q.valor_num FROM parametros q WHERE q.cod_param = 170))
                  OR (1 = (SELECT q.valor_num FROM parametros q WHERE q.cod_param = 170) AND pre.id_viaje = IDVIAJE))
              )
        ;
        UPDATE preruteo_reservadet pr
        SET pr.informado = 1
        WHERE pr.num_reserva = NUMRESERVA
          AND pr.num_detalle = NUMDETALLE
        ;
      END IF;
    END IF;

    END LOOP;
    --fin loop Producto**************************************************************
    --********************************************************************************

    EXCEPTION
      WHEN OTHERS THEN
        OUT_STATUS_CODE := 1;
        OUT_STATUS_MSG  := 'DAD: ' || SQLERRM;
        RETURN;

  END;
END ACTUALIZAR_ESTADOS_UNIGIS;

PROCEDURE PREPARAR_UNIGIS_STAGE(
  IN_XML           IN XMLTYPE,
  IN_PROCESS_LUW   IN VARCHAR2 DEFAULT 'T',
  OUT_STATUS_CODE  OUT NUMBER,
  OUT_STATUS_MSG   OUT VARCHAR2)
IS
  --VARIABLES PARA EL MANEJO DEL XML
  DOC                  DBMS_XMLDOM.DOMDOCUMENT;
  NODE                 DBMS_XMLDOM.DOMNODE;
  HIJO                 DBMS_XMLDOM.DOMNODE;
  NODE_LIST            DBMS_XMLDOM.DOMNODELIST;
  HIJOS_LIST           DBMS_XMLDOM.DOMNODELIST;
  NODO_ACTUAL          DBMS_XMLDOM.DOMNODE;
  NUM_NODES            NUMBER;
  NUM_HIJOS            NUMBER;
  ROW_COUNTER          NUMBER;
  --VARIABLES DE PreRuteo
  VAR_CCOSTO           NUMBER;
  VAR_NUM_JAULA        NUMBER;

  VAR_SEQUENCIA        NUMBER;
  VAR_RESERVA_DETALLE  NUMBER;
  VAR_EXISTE_DETALLE   NUMBER(5) := 0;
  VAR_ES_DETALLE       NUMBER;
  seqmax               NUMBER;

BEGIN
    OUT_STATUS_CODE := 0;
    OUT_STATUS_MSG  := 'OK';

    DOC        := DBMS_XMLDOM.NEWDOMDOCUMENT(IN_XML);
    NODE       := DBMS_XMLDOM.MAKENODE(DOC);
    NODE_LIST  := XSLPROCESSOR.SELECTNODES(NODE, '/dadCrearOrdenesReservasDoc/PreRuteo','xmlns="http://openuri.org/i/dadCrearOrdenesReservasDoc"');
    NUM_NODES  := DBMS_XMLDOM.GETLENGTH(NODE_LIST);

    --inicio loop PreRuteo****************************************************
    --******************************************************************************
    FOR ROW_COUNTER IN 0..NUM_NODES-1
    LOOP
      nodo_actual     := DBMS_XMLDOM.ITEM(NODE_LIST, ROW_COUNTER);
      hijo            := XSLPROCESSOR.SELECTSINGLENODE(nodo_actual,'cCosto');
      hijo            := XMLDOM.GETFIRSTCHILD(hijo);
      var_ccosto      := DBMS_XMLDOM.GETNODEVALUE(hijo);
      hijo            := XSLPROCESSOR.SELECTSINGLENODE(nodo_actual,'numJaula');
      hijo            := XMLDOM.GETFIRSTCHILD(hijo);
      var_num_jaula   := DBMS_XMLDOM.GETNODEVALUE(hijo);

--Revisar
--     1) Armar CURSOR de detalles de la jaula 0 a enviar.
--     2) Recorrer CURSOR
--       2.1) Detalle pregunta por si su reserva tiene un detalle en MI jaula no enviado a UNIGIS en STAGE
--           2.1.1) NO: Pide secuencia y inserta en cabecera
--           2.1.2) SI: pedir esa secuencia
--       2.2) Insertar detalle con secuencia

      FOR DETALLE IN (
        SELECT pr.*, rd.cc_despacha, cc.org_name_full, cc.org_is_store
        FROM preruteo_reservadet pr,
             reserva_dtl rd,
             centro_costo cc
        WHERE pr.canal_venta = rd.canal_venta
          AND pr.num_reserva = rd.num_reserva
          AND pr.num_detalle = rd.num_detalle
          AND rd.cc_despacha = cc.org_lvl_number
          AND pr.ccosto      = var_ccosto
          AND pr.num_jaula   = var_num_jaula -- 0
          AND pr.en_jaula    = 1
          AND (pr.cod_estadouni IS NULL OR pr.cod_estadouni IN (1, 4)) --Sin enviar, 'Por Enviar', 'Error'
        ORDER BY pr.num_reserva, pr.num_detalle
      )
      LOOP
        --Validar condiciones para envio a UNIGIS, y pregunto si es el mismo detalle Por Enviar
        --Debo validar la reserva NO EL DETALLE
        BEGIN
          SELECT COUNT(pr.num_detalle)
          INTO VAR_EXISTE_DETALLE
          FROM wli_stage_uni_hdr wld,
               preruteo_reservadet pr
          WHERE wld.canal_venta       = pr.canal_venta
            AND wld.num_reserva       = pr.num_reserva
            AND wld.cod_estadouni     = pr.cod_estadouni
            --AND wld.sequencia_dad     = pr.sequencia_dad
            AND pr.ccosto             = DETALLE.ccosto
            AND pr.num_jaula          = DETALLE.num_jaula
            AND pr.canal_venta        = DETALLE.canal_venta
            AND pr.num_reserva        = DETALLE.num_reserva
            --AND pr.num_detalle        = DETALLE.num_detalle
            AND pr.en_jaula           = 1
            AND pr.cod_estadouni      = 1 --????????
          GROUP BY pr.num_reserva
          ;
          EXCEPTION
             WHEN NO_DATA_FOUND THEN
               VAR_EXISTE_DETALLE := 0;
        END;

        IF (VAR_EXISTE_DETALLE > 0) THEN  --Reservas pendientes por enviar a UNIGIS de mi jaula (para usar misma secuencia de envio)
          BEGIN
            SELECT DISTINCT MAX(wld.sequencia_dad)
            INTO  VAR_SEQUENCIA
            FROM  wli_stage_uni_hdr wld,
                  preruteo_reservadet pr
            WHERE wld.canal_venta      = pr.canal_venta
              AND wld.num_reserva      = pr.num_reserva
              AND wld.cod_estadouni    = pr.cod_estadouni
              AND wld.sequencia_dad    = pr.sequencia_dad
              AND pr.ccosto            = DETALLE.ccosto
              AND pr.num_jaula         = DETALLE.num_jaula
              AND pr.canal_venta       = DETALLE.canal_venta
              AND pr.num_reserva       = DETALLE.num_reserva
              --AND pr.num_detalle       = DETALLE.num_detalle
              AND pr.en_jaula          = 1
              AND pr.cod_estadouni     = 1
            ;
            --EXCEPTION
            --   WHEN NO_DATA_FOUND THEN
            --     VAR_SEQUENCIA := 0;
          END;
          BEGIN
            SELECT NVL(SUM(DECODE(pr.num_detalle, DETALLE.num_detalle, 1, 0)), 0)
            INTO VAR_ES_DETALLE
            FROM wli_stage_uni_dtl wld,
                 preruteo_reservadet pr
            WHERE wld.canal_venta       = pr.canal_venta
              AND wld.num_reserva       = pr.num_reserva
              AND wld.num_detalle       = pr.num_detalle
              AND wld.sequencia_dad     = pr.sequencia_dad
              AND wld.cod_estadouni     = pr.cod_estadouni
              AND pr.ccosto             = DETALLE.ccosto
              AND pr.num_jaula          = DETALLE.num_jaula
              AND pr.canal_venta        = DETALLE.canal_venta
              AND pr.num_reserva        = DETALLE.num_reserva
              AND pr.num_detalle        = DETALLE.num_detalle
              AND pr.en_jaula           = 1
              AND wld.cod_estadouni     = 1
            ;
            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                VAR_ES_DETALLE := 0;
          END;
          IF (VAR_ES_DETALLE > 0) THEN --El detalle por enviar soy yo? De ser asi entonces actualizo
            --Se reenvia reserva-detalle. NO va el UPDATE a 11
            NULL;
            /*UPDATE WLI_STAGE_UNI_DTL WLD
            SET WLD.cod_estadodet = 11
            WHERE WLD.CANAL_VENTA = DETALLE.CANAL_VENTA
            AND   WLD.NUM_RESERVA = DETALLE.NUM_RESERVA
            AND   WLD.NUM_DETALLE = DETALLE.NUM_DETALLE
            AND   wld.sequencia_dad = VAR_SEQUENCIA
            ;
            UPDATE Wli_Stage_Uni_HDR WLH
            SET WLH.cod_estadocab = 11
            WHERE WLh.CANAL_VENTA = DETALLE.CANAL_VENTA
            AND   WLh.NUM_RESERVA = DETALLE.NUM_RESERVA
            AND   wlh.sequencia_dad = VAR_SEQUENCIA
            ;*/
          ELSE
            INSERT INTO wli_stage_uni_dtl(
                sequencia_dad,
                cod_estadouni,
                canal_venta,
                num_reserva,
                num_detalle,
                cod_estadodet,
                prd_lvl_number,
                prd_name_full,
                cantidad,
                prd_m3_uni,
                prd_m3_tot,
                prd_kilo_uni,
                prd_kilo_tot,
                bultos_uni,
                bultos_tot,
                alto,
                ancho,
                profundidad,
                apilable,
                rotacion,
                pallets)
              SELECT VAR_SEQUENCIA,
                     1, --Por Enviar
                     rd.canal_venta,
                     rd.num_reserva,
                     rd.num_detalle,
                     rd.cod_estadodet,
                     rd.prd_lvl_number,
                     rd.prd_name_full,
                     ROUND(rd.cantidad),
                     rd.prd_m3,
                     rd.prd_m3, --rd.prd_m3 * rd.cantidad,
                     rd.prd_kilo,
                     rd.prd_kilo, --rd.prd_kilo * rd.cantidad,
                     0, --NVL(rd.bultos, 1),
                     0, --NVL(rd.bultos, 1), --NVL(rd.bultos, 1) * rd.cantidad,
                     NULL alto,
                     NULL ancho,
                     NULL profundidad,
                     NULL apilable,
                     NULL rotacion,
                     NULL pallets
              FROM preruteo_reservadet pr,
                   reserva_dtl rd
              WHERE pr.canal_venta    = rd.canal_venta
                AND pr.num_reserva    = rd.num_reserva
                AND pr.num_detalle    = rd.num_detalle
                AND rd.canal_venta    = DETALLE.canal_venta
                AND rd.num_reserva    = DETALLE.num_reserva
                AND rd.num_detalle    = DETALLE.num_detalle
                AND pr.ccosto         = DETALLE.ccosto
                AND pr.num_jaula      = 0
                AND pr.en_jaula       = 1
                --AND NVL(pr.cod_estadouni, 0)  = 0
                AND pr.informado     IS NULL
            ;
          END IF;

        ELSE
          SELECT q.sequencia_dad
          INTO seqmax
          FROM wli_event_itftramo_dad q
          WHERE q.tipo_evento = 'ENVIAORDEN'
            AND q.estado_dad  = 'PU'
          ;
          SELECT seq_wlistage_uni.nextval INTO var_sequencia FROM DUAL;
          --Evita BUG de la secuencia, ya que en ciertas ocasiones entrega valores menores (hay correo de Adessa)
          WHILE var_sequencia <= seqmax  LOOP
            SELECT seq_wlistage_uni.nextval INTO var_sequencia FROM DUAL;
          END LOOP;

          INSERT INTO wli_stage_uni_hdr(
              --fecha,           --por defecto SYSDATE
              sequencia_dad,
              cod_estadouni,
              ccosto, --SI
              canal_venta, --SI
              num_reserva, --SI
              orden, --SI NULL
              num_ordendet, --SI NULL
              tipo_idorden,-- SI
              tipo_orden, --SI NULL
              cod_estadocab, --SI
              fechapactada, --SI
              type_ide, -- SI NULL
              ide, --SI
              ide_dv, --SI
              nombre_cli, --SI
              direccion_cli, --SI
              comuna_cli, --SI
              ciudad_cli, --SI
              region_cli, --SI
              latitud_cli, --SI
              longitud_cli, --SI
              calle_cli, --SI
              numero_cli, --SI NULL
              entre_calle_cli, --SI
              barrio_cli, --SI NULL
              fono_cli, --SI
              fono2_cli, --SI NULL
              fono3_cli, -- SI NULL
              email_cli, -- SI
              domicilio_ext_cli, --SI NULL
              domicilio_desc_cli, --SI NULL
              tiempo_espera_cli, -- SI
              descripcion_desp, --SI NULL
              nombre_desp, --SI
              direccion_desp,  --SI
              calle_desp, --SI NULL
              numero_desp, --SI NULL
              ref_calle_desp, -- SI NULL
              barrio_desp, --SI NULL
              comuna_desp, --SI
              ciudad_desp, --SI
              region_desp, --SI
              ini_hora_plani,
              fin_hora_plani,
              ini_hora_desp1,
              fin_hora_desp1,
              ini_hora_desp2,
              fin_hora_desp2,
              ini_hora1,
              fin_hora1,
              ini_hora2,
              fin_hora2,
              tiempo_espera_desp,
              prd_kilo, -- NO
              prd_m3, --NO
              bultos, --NO
              pallets,
              latitud_desp, -- SI
              longitud_desp, --SI
              observacion, --SI
              email_desp, --SI
              fono_desp, --SI
              fono2_desp, --SI
              fono3_desp, --SI NULL
              campo1, -- SI NULL
              campo2, -- SI NULL
              campo3, -- SI NULL
              campo4, -- SI NULL
              id_rutauni, --SI
              ccosto_cc1, --SI
              ccosto_desc_cc1,  --SI
              direccion_cc1, --SI
              numero_cc1, -- SI NULL
              bassaddr1_cc1,
              bassaddr2_cc1,
              bassaddr3_cc1,
              ini_hora_cc1, --SI NULL
              fin_hora_cc1, --SI NULL
              tiempoespera_cc1,
              latitud_cc1, --SI NULL
              longitud_cc1, --SI NULL
              calle_cc1, --SI NULL
              ref_calle_cc1, --SI NULL
              barrio_cc1, --SI NULL
              comuna_cc1,
              ciudad_cc1,
              region_cc1,
              ccosto_cc2,  --SI NULL
              ccosto_desc_cc2, --SI NULL
              direccion_cc2, --SI NULL
              numero_cc2,--SI NULL
              bassaddr1_cc2,
              bassaddr2_cc2,
              bassaddr3_cc2,
              ini_hora_cc2,--SI NULL
              fin_hora_cc2,--SI NULL
              tiempoespera_cc2,
              latitud_cc2,--SI NULL
              longitud_cc2,--SI NULL
              calle_cc2,--SI NULL
              ref_calle_cc2,--SI NULL
              barrio_cc2,--SI NULL
              comuna_cc2,
              ciudad_cc2,
              region_cc2,
              cc_despacha,
              org_name_full,
              org_is_store)
          SELECT DISTINCT
                VAR_SEQUENCIA,
                1, --cod_estadouni --Por Enviar
                pr.ccosto,
                rh.canal_venta,
                rh.num_reserva,
                NULL, --orden
                NULL, --num_ordendet,
                rh.tipo_reserva, --tipo_idorden
                NULL, --tipo_orden
                rh.cod_estadocab, --ATENCION: UNIGIS solo trabaja a nivel de cabecera
                rd.fecha_despacho, --TRUNC(SYSDATE + 1), --rd.fecha_entrega, --fechapactada
                rh.type_ide,
                rh.ide,
                rh.ide_dv,
                rh.nombre_cli,
                rh.direccion_cli,
                rh.comuna_cli,
                rh.ciudad_cli,
                rh.region_cli,
                NULL, --latitud_cli
                NULL, --longitud_cli
                NULL, --calle_cli
                NULL, --numero_cli
                NULL, --entre_calle_cli
                NULL, --barrio_cli
                NULL, --rh.fono_cli,
                NULL, --fono2_cli
                NULL, --fono3_cli
                rh.e_mail, --e_mail_cli
                NULL, --domicilio_ext_cli
                NULL, --domicilio_desc_cli
                NULL, --tiempo_espera_cli
                NULL, --descripcion_desp
                rh.nombre_desp,
                rh.direccion_desp,
                NULL, --calle_desp
                NULL, --numero_desp
                NULL, --ref_calle_desp
                NULL, --barrio_desp
                rh.comuna_desp,
                rh.ciudad_desp,
                rh.region_desp,
                NULL, --ini_hora_plani
                NULL, --fin_hora_plani
                NULL, --ini_hora_desp1,
                NULL, --fin_hora_desp1,
                NULL, --ini_hora_desp2 --
                NULL, --fin_hora_desp2
                pr.inicio_horario_1, --ini_hora1 --pr.inicio_horario_1
                pr.fin_horario_1, --fin_hora1 --pr.fin_horario_1
                pr.inicio_horario_2, --ini_hora2 --pr.inicio_horario_2
                pr.fin_horario_2, --fin_hora2 --pr.fin_horario_2
                NULL, --tiempo_espera_desp
                0, --rd.prd_kilo,  --null prd_kilo,
                0, --rd.prd_m3, --null prd_m3,
                0, --NVL(rd.bultos, 1), --null bultos,
                NULL pallets,
                rh.latitud, --latitud_desp,
                rh.longitud, --longitud_desp,
                rh.observacion,
                rh.e_mail, --e_mail_desp,
                rh.fono_desp,
                DECODE(rh.fono2_desp, NULL, rh.fono_cli, rh.fono2_desp),
                NULL, --fono3_desp
                DECODE(DETALLE.org_is_store, 'F', DETALLE.org_name_full, NULL), --DETALLE.org_name_full, --VARCHAR1 (comodin)
                NULL, --VARCHAR2 (comodin)
                DECODE(DETALLE.org_is_store, 'F', DETALLE.cc_despacha,   NULL), --DETALLE.cc_despacha,   --INT1     (comodin)
                NULL, --INT2     (comodin)
                pr.id_rutauni,
                DECODE(DETALLE.org_is_store, 'F', DETALLE.cc_despacha,   cc.org_lvl_number), --cc.org_lvl_number, --ccosto_cc1
                DECODE(DETALLE.org_is_store, 'F', DETALLE.org_name_full, cc.org_name_full),  --cc.org_name_full,  --ccosto_desc_cc1
                NULL, --direccion_cc1
                NULL, --numero_cc1
                cc.bas_addr_1, --bassaddr1_cc1
                cc.bas_addr_2, --bassaddr2_cc1
                cc.bas_addr_3, --bassaddr3_cc1
                NULL, --ini_hora_cc1
                NULL, --fin_hora_cc1
                NULL, --tiempo_espera_cc1
                NULL, --cc.latitud, --latitud_cc1
                NULL, --cc.longitud, --longitud_cc1
                NULL, --calle_cc1
                NULL, --ref_calle_cc1
                NULL, --barrio_cc1
                NULL, --cc.comuna, --comuna_cc1
                NULL, --cc.ciudad, --comuna_cc1
                NULL, --cc.region, --region_cc1
                NULL, --ccosto_cc2
                NULL, --ccosto_desc_cc2
                NULL, --direccion_cc2
                NULL, --numero_cc2
                NULL, --bassaddr2_cc2
                NULL, --bassaddr2_cc2
                NULL, --bassaddr3_cc2
                NULL, --ini_hora_cc2
                NULL, --fin_hora_cc2
                NULL, --tiempo_espera_cc2
                NULL, --latitud_cc2
                NULL, --longitud_cc2
                NULL, --calle_cc2
                NULL, --ref_calle_cc2
                NULL, --barrio_cc2
                NULL, --comuna_cc2
                NULL, --comuna_cc2
                NULL, --region_cc2
                DETALLE.cc_despacha,
                DETALLE.org_name_full,
                DETALLE.org_is_store
          FROM preruteo_reservadet pr,
               reserva_dtl rd,
               reserva_hdr rh,
               centro_costo cc
          WHERE rd.canal_venta    = rh.canal_venta
            AND rd.num_reserva    = rh.num_reserva
            AND pr.canal_venta    = rd.canal_venta
            AND pr.num_reserva    = rd.num_reserva
            AND pr.num_detalle    = rd.num_detalle
            AND pr.ccosto         = cc.org_lvl_number
            AND pr.ccosto         = DETALLE.ccosto
            AND pr.num_jaula      = DETALLE.num_jaula
            AND pr.canal_venta    = DETALLE.canal_venta
            AND pr.num_reserva    = DETALLE.num_reserva
            AND pr.num_detalle    = DETALLE.num_detalle
            AND pr.en_jaula       = 1
        --    AND pr.cod_estadouni  = 0
            AND pr.informado     IS NULL
          ;
          INSERT INTO wli_stage_uni_dtl(
              sequencia_dad,
              cod_estadouni,
              canal_venta,
              num_reserva,
              num_detalle,
              cod_estadodet,
              prd_lvl_number,
              prd_name_full,
              cantidad,
              prd_m3_uni,
              prd_m3_tot,
              prd_kilo_uni,
              prd_kilo_tot,
              bultos_uni,
              bultos_tot,
              alto,
              ancho,
              profundidad,
              apilable,
              rotacion,
              pallets)
          SELECT
              VAR_SEQUENCIA,
              1, --Por Enviar
              rd.canal_venta,
              rd.num_reserva,
              rd.num_detalle,
              rd.cod_estadodet,
              rd.prd_lvl_number,
              rd.prd_name_full,
              ROUND(rd.cantidad),
              rd.prd_m3,
              rd.prd_m3, --rd.prd_m3 * rd.cantidad,
              rd.prd_kilo,
              rd.prd_kilo, --rd.prd_kilo * rd.cantidad,
              0, --NVL(rd.bultos, 1),
              0, --NVL(rd.bultos, 1) * rd.cantidad,
              NULL alto,
              NULL ancho,
              NULL profundidad,
              NULL apilable,
              NULL rotacion,
              NULL pallets
          FROM preruteo_reservadet pr,
               reserva_dtl rd
          WHERE pr.canal_venta    = rd.canal_venta
            AND pr.num_reserva    = rd.num_reserva
            AND pr.num_detalle    = rd.num_detalle
            AND pr.ccosto         = DETALLE.ccosto
            AND pr.num_jaula      = DETALLE.num_jaula
            AND pr.canal_venta    = DETALLE.canal_venta
            AND pr.num_reserva    = DETALLE.num_reserva
            AND pr.num_detalle    = DETALLE.num_detalle
            AND pr.en_jaula       = 1
            --AND NVL(pr.cod_estadouni, 0)  = 0
            AND pr.informado IS NULL
          ;
        END IF;

        UPDATE preruteo_reservadet pr
        SET pr.cod_estadouni    = 1, --Por Enviar
            pr.err_estadouni    = NULL,
            pr.sequencia_dad    = VAR_SEQUENCIA
        WHERE pr.ccosto         = DETALLE.ccosto
          AND pr.num_jaula      = DETALLE.num_jaula
          AND pr.canal_venta    = DETALLE.canal_venta
          AND pr.num_reserva    = DETALLE.num_reserva
          AND pr.num_detalle    = DETALLE.num_detalle
          AND pr.en_jaula       = 1
          AND pr.informado IS NULL
        ;

      END LOOP;
    END LOOP;

    EXCEPTION
      WHEN OTHERS THEN
        OUT_STATUS_CODE := 1;
        OUT_STATUS_MSG  := 'DAD: ' || SQLERRM;
        RETURN;
END;

PROCEDURE CONFIRMAR_UNIGIS(
  IN_XML           IN XMLTYPE,
  IN_PROCESS_LUW   IN VARCHAR2 DEFAULT 'T',
  OUT_STATUS_CODE  OUT NUMBER,
  OUT_STATUS_MSG   OUT VARCHAR2)
IS
    --VARIABLES PARA EL MANEJO DEL XML
  DOC             DBMS_XMLDOM.DOMDOCUMENT;
  NODE            DBMS_XMLDOM.DOMNODE;
  HIJO            DBMS_XMLDOM.DOMNODE;
  NODELIST        DBMS_XMLDOM.DOMNODELIST;
  HIJOSLIST       DBMS_XMLDOM.DOMNODELIST;
  NUMNODES        NUMBER;
  NUMHIJOS        NUMBER;
  ROWCOUNTER      NUMBER;
  NODOACTUAL      DBMS_XMLDOM.DOMNODE;

  HIJO_DETALLES         DBMS_XMLDOM.DOMNODE;
  HIJOS_DETALLES_LIST   DBMS_XMLDOM.DOMNODELIST;
  NODOACTUAL_DETALLE    DBMS_XMLDOM.DOMNODE;
  NUM_DETALLES_NODES    DBMS_XMLDOM.DOMNODE;
  NUM_DETALLES_HIJOS    NUMBER;
  ROW_DETALLES_COUNTER  NUMBER;

  --VARIABLES DE reserva
  vCANAL_VENTA        NUMBER;
  vNUM_RESERVA        NUMBER;
  vCODIGO             NUMBER;
  vNOMBRE_ERROR       VARCHAR(255);
  vDESCRIPCION_ERROR  VARCHAR(2000);
  vDESC_ESTADO_UNI    VARCHAR(255);
  vCODIGO_VAR NUMBER;
  --VARIABLES DE detalles
  vNUM_DETALLE        NUMBER;

  -- VARIABLES AUXILIARES
  AUX VARCHAR2(4000);

  BEGIN
    OUT_STATUS_CODE := 0;
    OUT_STATUS_MSG  := 'OK';

    DOC            := DBMS_XMLDOM.NEWDOMDOCUMENT(IN_XML);
    NODE           := DBMS_XMLDOM.MAKENODE(DOC);
    NODELIST       := XSLPROCESSOR.SELECTNODES(NODE, '/response/reserva');
    NUMNODES       := DBMS_XMLDOM.GETLENGTH(NODELIST);

    --inicio loop reserva*************************************************************
    --********************************************************************************
    FOR ROWCOUNTER IN 0..NUMNODES-1
    LOOP

    NODOACTUAL              := DBMS_XMLDOM.ITEM(NODELIST, ROWCOUNTER);

    HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'canal_venta');
    HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
    vCANAL_VENTA            := DBMS_XMLDOM.GETNODEVALUE(HIJO);

    HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'num_reserva');
    HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
    vNUM_RESERVA            := DBMS_XMLDOM.GETNODEVALUE(HIJO);

    HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'codigo');
    HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
    vCODIGO                 := DBMS_XMLDOM.GETNODEVALUE(HIJO);

    HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'nombre_error');
    HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
    vNOMBRE_ERROR           := DBMS_XMLDOM.GETNODEVALUE(HIJO);

    HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'descripcion_error');
    HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
    vDESCRIPCION_ERROR      := DBMS_XMLDOM.GETNODEVALUE(HIJO);

    HIJOS_DETALLES_LIST     := XSLPROCESSOR.SELECTNODES(NODOACTUAL, 'detalles');
    NUM_DETALLES_HIJOS      := DBMS_XMLDOM.GETLENGTH(HIJOS_DETALLES_LIST);
    END LOOP;
    --fin loop reserva****************************************************************
    --********************************************************************************

    --inicio loop detalles************************************************************
    --********************************************************************************

    FOR ROW_DETALLES_COUNTER IN 0..NUM_DETALLES_HIJOS-1
    LOOP

    NODOACTUAL_DETALLE          := DBMS_XMLDOM.ITEM(HIJOS_DETALLES_LIST, ROW_DETALLES_COUNTER);

    HIJO_DETALLES               := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL_DETALLE,'num_detalle');
    HIJO_DETALLES               := XMLDOM.GETFIRSTCHILD(HIJO_DETALLES);
    vNUM_DETALLE                := DBMS_XMLDOM.GETNODEVALUE(HIJO_DETALLES);

    -- SI vCODIGO ES DISTINTO DE 1, VIENE CON ERROR DESDE UNIGIS
    IF (vCODIGO = 1)THEN
      vCODIGO_VAR := 3; -- RECIBIDO
    ELSE
      vCODIGO_VAR := 4; -- ERROR ENVIO
    END IF;

    UPDATE preruteo_reservadet p
          SET p.cod_estadouni    = vCODIGO_VAR, -- Recibido/Error
              p.err_estadouni    = DECODE(vCODIGO_VAR, 3, NULL, vNOMBRE_ERROR || ' ' || vDESCRIPCION_ERROR)
          WHERE p.canal_venta    = vCANAL_VENTA
            AND p.num_reserva    = vNUM_RESERVA
            AND p.num_detalle    = vNUM_DETALLE
      AND p.en_jaula       = 1
    ;
    SELECT  EST.DESC_ESTADOUNI INTO vDESC_ESTADO_UNI
      FROM    ESTADOSUNI EST
      WHERE   EST.COD_ESTADOUNI = vCODIGO_VAR
    ;
    UPDATE wli_stage_uni_dtl w
          SET w.cod_estadouni     = vCODIGO_VAR, -- Recibido/Error
              w.desc_estadouni    = vDESC_ESTADO_UNI ,
              w.err_estadouni     = DECODE(vCODIGO_VAR, 3, NULL, vNOMBRE_ERROR || ' ' || vDESCRIPCION_ERROR)
          WHERE (w.canal_venta, w.num_reserva, w.num_detalle, w.sequencia_dad) IN (
                 SELECT p.canal_venta, p.num_reserva, p.num_detalle, p.sequencia_dad
                 FROM preruteo_reservadet p
                 WHERE p.canal_venta    = vCANAL_VENTA
                   AND p.num_reserva    = vNUM_RESERVA
                   AND p.num_detalle    = vNUM_DETALLE
             AND p.en_jaula       = 1)
    ;
    /*UPDATE wli_stage_uni_dtl wsd
    SET   wsd.cod_estadouni   = vCODIGO_VAR, --Recibido/Error
          wsd.DESC_ESTADOUNI  = vDESC_ESTADO_UNI
    WHERE wsd.num_reserva     = vNUM_RESERVA
      AND wsd.num_detalle     = vNUM_DETALLE
      AND wsd.CANAL_VENTA     = vCANAL_VENTA
    ;*/
    UPDATE wli_stage_uni_hdr   w
    SET w.cod_estadouni   = vCODIGO_VAR, --Recibido/Error
        w.desc_estadouni  = vDESC_ESTADO_UNI
    WHERE (w.canal_venta, w.num_reserva, w.sequencia_dad) IN (
           SELECT p.canal_venta, p.num_reserva, p.sequencia_dad
           FROM preruteo_reservadet p
           WHERE p.canal_venta    = vCANAL_VENTA
             AND p.num_reserva    = vNUM_RESERVA
             AND p.num_detalle    = vNUM_DETALLE
             AND p.en_jaula       = 1)
    ;
    END LOOP;
    --fin loop detalles***************************************************************
    --********************************************************************************

   EXCEPTION
      WHEN OTHERS THEN
        OUT_STATUS_CODE := 1;
        OUT_STATUS_MSG  := 'DAD: ' || SQLERRM;
        RETURN;

END CONFIRMAR_UNIGIS;

PROCEDURE REGISTRAR_PRERUTEO_UNIGIS(
  IN_XML           IN  XMLTYPE,
  IN_PROCESS_LUW   IN  VARCHAR2 DEFAULT 'T',
  OUT_STATUS_CODE  OUT NUMBER,
  OUT_STATUS_MSG   OUT VARCHAR2) AS
BEGIN
DECLARE
  --VARIABLES PARA EL MANEJO DEL XML
  DOC                             DBMS_XMLDOM.DOMDOCUMENT;
  NODE                            DBMS_XMLDOM.DOMNODE;
  HIJO                            DBMS_XMLDOM.DOMNODE;
  NODELIST                        DBMS_XMLDOM.DOMNODELIST;
  HIJOSLIST                       DBMS_XMLDOM.DOMNODELIST;
  NUMNODES                        NUMBER;
  NUMHIJOS                        NUMBER;
  ROWCOUNTER                      NUMBER;
  NODOACTUAL                      DBMS_XMLDOM.DOMNODE;

  HIJO_LISTA_ORDEN                DBMS_XMLDOM.DOMNODE;
  HIJOS_LISTAS_ORDEN_LIST         DBMS_XMLDOM.DOMNODELIST;
  NODOACTUAL_LISTA_ORDEN          DBMS_XMLDOM.DOMNODE;
  NUM_LISTA_ORDEN_NODES           DBMS_XMLDOM.DOMNODE;
  NUM_LISTAS_ORDEN_HIJOS          NUMBER;
  ROW_LISTA_ORDEN_COUNTER         NUMBER;

  HIJO_ORDEN_DATOS                DBMS_XMLDOM.DOMNODE;
  HIJOS_ORDEN_DATOS_LIST          DBMS_XMLDOM.DOMNODELIST;
  NODOACTUAL_ORDEN_DATOS          DBMS_XMLDOM.DOMNODE;
  NUM_ORDEN_DATOS_NODES           DBMS_XMLDOM.DOMNODE;
  NUM_ORDEN_DATOS_HIJOS           NUMBER;
  ROW_ORDEN_DATOS_COUNTER         NUMBER;

  HIJO_ITEM_ORDEN                 DBMS_XMLDOM.DOMNODE;
  HIJOS_ITEM_ORDEN_LIST           DBMS_XMLDOM.DOMNODELIST;
  NODOACTUAL_ITEM_ORDEN           DBMS_XMLDOM.DOMNODE;
  NUM_ITEM_ORDEN_NODES            DBMS_XMLDOM.DOMNODE;
  NUM_ITEM_ORDEN_HIJOS            NUMBER;
  ROW_ITEM_ORDEN_COUNTER          NUMBER;

  HIJO_LISTA_ITEM_PROD            DBMS_XMLDOM.DOMNODE;
  HIJOS_LISTA_ITEM_PROD_LIST      DBMS_XMLDOM.DOMNODELIST;
  NODOACTUAL_LISTA_ITEM_PROD      DBMS_XMLDOM.DOMNODE;
  NUM_LISTA_ITEM_PROD_NODES       DBMS_XMLDOM.DOMNODE;
  NUM_LISTA_ITEM_PROD_HIJOS       NUMBER;
  ROW_LISTA_ITEM_PROD_COUNTER     NUMBER;

  HIJO_PRODUCTO                   DBMS_XMLDOM.DOMNODE;
  HIJOS_PRODUCTO_LIST             DBMS_XMLDOM.DOMNODELIST;
  NODOACTUAL_PRODUCTO             DBMS_XMLDOM.DOMNODE;
  NUM_PRODUCTO_NODES              DBMS_XMLDOM.DOMNODE;
  NUM_PRODUCTO_HIJOS              NUMBER;
  ROW_PRODUCTO_COUNTER            NUMBER;

  HIJO_ETIQUETA                   DBMS_XMLDOM.DOMNODE;
  HIJOS_ETIQUETA_LIST             DBMS_XMLDOM.DOMNODELIST;
  NODOACTUAL_ETIQUETA             DBMS_XMLDOM.DOMNODE;
  NUM_ETIQUETA_NODES              DBMS_XMLDOM.DOMNODE;
  NUM_ETIQUETA_HIJOS              NUMBER;
  ROW_ETIQUETA_COUNTER            NUMBER;

  HIJO_VEHICULO                   DBMS_XMLDOM.DOMNODE;
  HIJOS_VEHICULO_LIST             DBMS_XMLDOM.DOMNODELIST;
  NODOACTUAL_VEHICULO             DBMS_XMLDOM.DOMNODE;
  NUM_VEHICULO_NODES              DBMS_XMLDOM.DOMNODE;
  NUM_VEHICULO_HIJOS              NUMBER;
  ROW_VEHICULO_COUNTER            NUMBER;

  HIJO_DEPOSITO_SALIDA            DBMS_XMLDOM.DOMNODE;
  HIJOS_DEPOSITO_SALIDA_LIST      DBMS_XMLDOM.DOMNODELIST;
  NODOACTUAL_DEPOSITO_SALIDA      DBMS_XMLDOM.DOMNODE;
  NUM_DEPOSITO_SALIDA_NODES       DBMS_XMLDOM.DOMNODE;
  NUM_DEPOSITO_SALIDA_HIJOS       NUMBER;
  ROW_DEPOSITO_SALIDA_COUNTER     NUMBER;

  HIJO_DEPOSITO_LLEGADA           DBMS_XMLDOM.DOMNODE;
  HIJOS_DEPOSITO_LLEGADA_LIST     DBMS_XMLDOM.DOMNODELIST;
  NODOACTUAL_DEPOSITO_LLEGADA     DBMS_XMLDOM.DOMNODE;
  NUM_DEPOSITO_LLEGADA_NODES      DBMS_XMLDOM.DOMNODE;
  NUM_DEPOSITO_LLEGADA_HIJOS      NUMBER;
  ROW_DEPOSITO_LLEGADA_COUNTER    NUMBER;

  HIJOS_ITEM_PRODUCTO_LIST      DBMS_XMLDOM.DOMNODELIST;
  ROW_ITEM_PRODUCTO_COUNTER     NUMBER;
  NUM_ITEM_PRODUCTO             NUMBER;
  NODOACTUAL_ITEM_PRODUCTO           DBMS_XMLDOM.DOMNODE;
  --VARIABLES DE OrdenDatos
  vCCOSTO                 NUMBER;
  vNUM_RESERVA            NUMBER;
  vNUM_JAULA              NUMBER;
  vSEQ_ENTREGA            NUMBER;
  vINICIOHORARIO1         PRERUTEO_RESERVADET.INICIO_HORARIO_1%TYPE;
  vINICIOHORARIO2         PRERUTEO_RESERVADET.INICIO_HORARIO_2%TYPE;
  vFINHORARIO1            PRERUTEO_RESERVADET.FIN_HORARIO_1%TYPE;
  vFINHORARIO2            PRERUTEO_RESERVADET.FIN_HORARIO_2%TYPE;
  vINICIOVISITA           PRERUTEO_RESERVADET.INICIO_VISITA%TYPE;
  vFINVISITA              PRERUTEO_RESERVADET.FIN_VISITA%TYPE;
  vINICIOVISITA_FECHA     PRERUTEO_RESERVADET.INICIO_VISITA_FECHA%TYPE;
  vFINVISITA_FECHA        PRERUTEO_RESERVADET.FIN_VISITA_FECHA%TYPE;
  vTIEMPOESPERA           PRERUTEO_RESERVADET.TIEMPO_ESPERA%TYPE;
  vLATITUD                RESERVA_HDR.LATITUD%TYPE;
  vLONGITUD               RESERVA_HDR.LONGITUD%TYPE;
  vIDRUTAUNI              PRERUTEO_RESERVADET.ID_RUTAUNI%TYPE;

  vNUM_DETALLE            NUMBER;
  vCCOSTO_SALIDA          NUMBER;

  -- VARIABLES AUXILIARES
  vAUX VARCHAR2(4000);
  vcod_estadodet NUMBER := 0;
  vcant_desp     NUMBER := 0;

  /* xml para gdd */
  docSalida             xmldom.DOMDocument;
  main_node             xmldom.DOMNode;
  root_GuiaDeDespacho   xmldom.DOMElement;
  root_node             xmldom.DOMNode;
  reserva_elmt          xmldom.DOMElement;
  reserva_node          xmldom.DOMNode;
  item_elmt             xmldom.DOMElement;
  item_node             xmldom.DOMNode;
  item_text             xmldom.DOMText;
  varEsSoloRetiro       VARCHAR2(10);
  VAR_SEQUENCIA         NUMBER;
  vgdactivas_bod        NUMBER := 0;
  "rs"                  "PCKTYPES"."FOLIOSRS";

  BEGIN
    OUT_STATUS_CODE := 0;
    OUT_STATUS_MSG  := 'OK';
    DOC             := DBMS_XMLDOM.NEWDOMDOCUMENT(IN_XML);
    NODE            := DBMS_XMLDOM.MAKENODE(DOC);
    NODELIST        := XSLPROCESSOR.SELECTNODES(NODE, '/dadRegistrarPreRuteoDoc','xmlns="http://openuri.org/i/dadRegistrarPreRuteoDoc"');
    NUMNODES        := DBMS_XMLDOM.GETLENGTH(NODELIST);

    --inicio loop dadRegistrarPreRuteoDoc ********************************************
    --********************************************************************************
    FOR ROWCOUNTER IN 0..NUMNODES-1 LOOP
      NODOACTUAL                  := DBMS_XMLDOM.ITEM(NODELIST, ROWCOUNTER);
      HIJOS_LISTAS_ORDEN_LIST     := XSLPROCESSOR.SELECTNODES(NODOACTUAL, 'ListaOrden');
      NUM_LISTAS_ORDEN_HIJOS      := DBMS_XMLDOM.GETLENGTH(HIJOS_LISTAS_ORDEN_LIST);
    END LOOP;
    --fin loop dadRegistrarPreRuteoDoc************************************************
    --********************************************************************************

    --inicio loop ListaOrden**********************************************************
    --********************************************************************************
    FOR ROW_LISTA_ORDEN_COUNTER IN 0..NUM_LISTAS_ORDEN_HIJOS-1 LOOP
      NODOACTUAL_LISTA_ORDEN      := DBMS_XMLDOM.ITEM(HIJOS_LISTAS_ORDEN_LIST, ROW_LISTA_ORDEN_COUNTER);
      HIJOS_ORDEN_DATOS_LIST      := XSLPROCESSOR.SELECTNODES(NODOACTUAL_LISTA_ORDEN, 'OrdenDatos');
      NUM_ORDEN_DATOS_HIJOS      := DBMS_XMLDOM.GETLENGTH(HIJOS_ORDEN_DATOS_LIST);

      --inicio loop OrdenDatos********************************************************
      --******************************************************************************
      FOR ROW_ORDEN_DATOS_COUNTER IN 0..NUM_ORDEN_DATOS_HIJOS-1 LOOP
        NODOACTUAL_ORDEN_DATOS    := DBMS_XMLDOM.ITEM(HIJOS_ORDEN_DATOS_LIST, ROW_ORDEN_DATOS_COUNTER);

        HIJO_ORDEN_DATOS          := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL_ORDEN_DATOS,'cCosto');
        HIJO_ORDEN_DATOS          := XMLDOM.GETFIRSTCHILD(HIJO_ORDEN_DATOS);
        vCCOSTO                   := DBMS_XMLDOM.GETNODEVALUE(HIJO_ORDEN_DATOS);

        HIJO_ORDEN_DATOS          := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL_ORDEN_DATOS,'numReserva');
        HIJO_ORDEN_DATOS          := XMLDOM.GETFIRSTCHILD(HIJO_ORDEN_DATOS);
        vNUM_RESERVA              := DBMS_XMLDOM.GETNODEVALUE(HIJO_ORDEN_DATOS);

        HIJO_ORDEN_DATOS          := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL_ORDEN_DATOS,'numJaula');
        HIJO_ORDEN_DATOS          := XMLDOM.GETFIRSTCHILD(HIJO_ORDEN_DATOS);
        vNUM_JAULA                := DBMS_XMLDOM.GETNODEVALUE(HIJO_ORDEN_DATOS);

        HIJO_ORDEN_DATOS          := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL_ORDEN_DATOS,'secuenciaEntrega');
        HIJO_ORDEN_DATOS          := XMLDOM.GETFIRSTCHILD(HIJO_ORDEN_DATOS);
        vSEQ_ENTREGA              := DBMS_XMLDOM.GETNODEVALUE(HIJO_ORDEN_DATOS);

        HIJO_ORDEN_DATOS          := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL_ORDEN_DATOS,'inicioHorario1');
        HIJO_ORDEN_DATOS          := XMLDOM.GETFIRSTCHILD(HIJO_ORDEN_DATOS);
        vINICIOHORARIO1           := DBMS_XMLDOM.GETNODEVALUE(HIJO_ORDEN_DATOS);

        HIJO_ORDEN_DATOS          := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL_ORDEN_DATOS,'inicioHorario2');
        HIJO_ORDEN_DATOS          := XMLDOM.GETFIRSTCHILD(HIJO_ORDEN_DATOS);
        vINICIOHORARIO2           := DBMS_XMLDOM.GETNODEVALUE(HIJO_ORDEN_DATOS);

        HIJO_ORDEN_DATOS          := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL_ORDEN_DATOS,'finHorario1');
        HIJO_ORDEN_DATOS          := XMLDOM.GETFIRSTCHILD(HIJO_ORDEN_DATOS);
        vFINHORARIO1              := DBMS_XMLDOM.GETNODEVALUE(HIJO_ORDEN_DATOS);

        HIJO_ORDEN_DATOS          := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL_ORDEN_DATOS,'finHorario2');
        HIJO_ORDEN_DATOS          := XMLDOM.GETFIRSTCHILD(HIJO_ORDEN_DATOS);
        vFINHORARIO2              := DBMS_XMLDOM.GETNODEVALUE(HIJO_ORDEN_DATOS);

        HIJO_ORDEN_DATOS          := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL_ORDEN_DATOS,'inicioVisita');
        HIJO_ORDEN_DATOS          := XMLDOM.GETFIRSTCHILD(HIJO_ORDEN_DATOS);
        vINICIOVISITA_FECHA       := TO_DATE(DBMS_XMLDOM.GETNODEVALUE(HIJO_ORDEN_DATOS), 'dd/mm/yyyy hh24:mi:ss');

        HIJO_ORDEN_DATOS          := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL_ORDEN_DATOS,'finVisita');
        HIJO_ORDEN_DATOS          := XMLDOM.GETFIRSTCHILD(HIJO_ORDEN_DATOS);
        vFINVISITA_FECHA          := TO_DATE(DBMS_XMLDOM.GETNODEVALUE(HIJO_ORDEN_DATOS), 'dd/mm/yyyy hh24:mi:ss');

        HIJO_ORDEN_DATOS          := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL_ORDEN_DATOS,'tiempoEspera');
        HIJO_ORDEN_DATOS          := XMLDOM.GETFIRSTCHILD(HIJO_ORDEN_DATOS);
        vTIEMPOESPERA             := DBMS_XMLDOM.GETNODEVALUE(HIJO_ORDEN_DATOS);

        HIJO_ORDEN_DATOS          := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL_ORDEN_DATOS,'latitud');
        HIJO_ORDEN_DATOS          := XMLDOM.GETFIRSTCHILD(HIJO_ORDEN_DATOS);
        vLATITUD                  := DBMS_XMLDOM.GETNODEVALUE(HIJO_ORDEN_DATOS);

        HIJO_ORDEN_DATOS          := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL_ORDEN_DATOS,'longitud');
        HIJO_ORDEN_DATOS          := XMLDOM.GETFIRSTCHILD(HIJO_ORDEN_DATOS);
        vLONGITUD                  := DBMS_XMLDOM.GETNODEVALUE(HIJO_ORDEN_DATOS);

        HIJO_ORDEN_DATOS          := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL_ORDEN_DATOS,'idRutaUni');
        HIJO_ORDEN_DATOS          := XMLDOM.GETFIRSTCHILD(HIJO_ORDEN_DATOS);
        vIDRUTAUNI                := DBMS_XMLDOM.GETNODEVALUE(HIJO_ORDEN_DATOS);

        HIJOS_ITEM_ORDEN_LIST     := XSLPROCESSOR.SELECTNODES(NODOACTUAL_ORDEN_DATOS, 'ItemOrden');
        NUM_ITEM_ORDEN_HIJOS      := DBMS_XMLDOM.GETLENGTH(HIJOS_ITEM_ORDEN_LIST);

        docSalida      := xmldom.newDOMDocument;
        main_node      := xmldom.makeNode(docSalida);
        root_GuiaDeDespacho := xmldom.createElement(docSalida , 'GuiaDeDespacho');
        root_node := xmldom.appendChild(main_node, xmldom.makeNode(root_GuiaDeDespacho));

        UPDATE reserva_hdr rh
        SET rh.latitud   = vLATITUD,
            rh.longitud  = vLONGITUD
        WHERE rh.num_reserva = vNUM_RESERVA
        ;
        --SELECT TO_NUMBER(TO_CHAR(to_date(vINICIOVISITA_FECHA, 'dd/mm/yyyy hh24:mi:ss'), 'HH24MI'))
        SELECT TO_NUMBER(TO_CHAR(vINICIOVISITA_FECHA, 'HH24MI')),
               TO_NUMBER(TO_CHAR(vFINVISITA_FECHA,    'HH24MI'))
        INTO vINICIOVISITA, vFINVISITA
        FROM dual
        ;
        --inicio loop ItemOrden*********************************************************
        --******************************************************************************
        FOR ROW_ITEM_ORDEN_COUNTER IN 0..NUM_ITEM_ORDEN_HIJOS-1 LOOP
          NODOACTUAL_ITEM_ORDEN           := DBMS_XMLDOM.ITEM(HIJOS_ITEM_ORDEN_LIST, ROW_ITEM_ORDEN_COUNTER);

          HIJOS_ITEM_PRODUCTO_LIST        := XSLPROCESSOR.SELECTNODES(NODOACTUAL_ITEM_ORDEN, 'ListaItemProducto');
          NUM_ITEM_PRODUCTO               := DBMS_XMLDOM.GETLENGTH(HIJOS_ITEM_PRODUCTO_LIST);

          HIJOS_DEPOSITO_SALIDA_LIST      := XSLPROCESSOR.SELECTNODES(NODOACTUAL_ITEM_ORDEN, 'DepositoSalida');
          NUM_DEPOSITO_SALIDA_HIJOS       := DBMS_XMLDOM.GETLENGTH(HIJOS_DEPOSITO_SALIDA_LIST);

          --inicio loop DepositoSalida****************************************************
          FOR ROW_DEPOSITO_SALIDA_COUNTER IN 0..NUM_DEPOSITO_SALIDA_HIJOS-1 LOOP
            NODOACTUAL_DEPOSITO_SALIDA      := DBMS_XMLDOM.ITEM(HIJOS_DEPOSITO_SALIDA_LIST, ROW_DEPOSITO_SALIDA_COUNTER);
            HIJO_DEPOSITO_SALIDA            := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL_DEPOSITO_SALIDA,'cCosto');
            HIJO_DEPOSITO_SALIDA            := XMLDOM.GETFIRSTCHILD(HIJO_DEPOSITO_SALIDA);
            vCCOSTO_SALIDA                  := DBMS_XMLDOM.GETNODEVALUE(HIJO_DEPOSITO_SALIDA);
          END LOOP;
          --fin loop DepositoSalida*******************************************************

          --inicio loop ListaItemProducto****************************************************
          --******************************************************************************
          FOR ROW_ITEM_PRODUCTO_COUNTER IN 0..NUM_ITEM_PRODUCTO-1 LOOP
            NODOACTUAL_ITEM_PRODUCTO  := DBMS_XMLDOM.ITEM(HIJOS_ITEM_PRODUCTO_LIST, ROW_ITEM_PRODUCTO_COUNTER);
            HIJO_ITEM_ORDEN           := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL_ITEM_PRODUCTO,'numDetalle');
            HIJO_ITEM_ORDEN           := XMLDOM.GETFIRSTCHILD(HIJO_ITEM_ORDEN);
            vNUM_DETALLE              := DBMS_XMLDOM.GETNODEVALUE(HIJO_ITEM_ORDEN);

            --Se obtiene tipo de reserva
            SELECT DECODE(COUNT(tipo_despacho),
                          1, MAX(tipo_despacho),
                          2, DECODE(MAX(tipo_despacho),
                                    'R', 'C/R',
                                    'D', 'C/D',
                                    NULL),
                          NULL)
            INTO varEsSoloRetiro
            FROM reserva_dtl
            WHERE num_reserva      = vNUM_RESERVA
              AND ABS(num_detalle) = ABS(vNUM_DETALLE)
            GROUP BY canal_venta, num_reserva
            ;
            BEGIN
              --Se valida que la reserva este en jaula y que su destino CC-Jaula exista
              SELECT COUNT(*)
              INTO vcant_desp
              FROM jaula j
              WHERE (j.ccosto = vCCOSTO AND j.num_jaula = vNUM_JAULA)
                AND  (0 < (
                     SELECT COUNT(*)
                     FROM preruteo_reservadet p
                     WHERE p.en_jaula         = 1
                       AND p.num_reserva      = vNUM_RESERVA
                       AND ABS(p.num_detalle) = ABS(vNUM_DETALLE)
                    ))
              ;

            EXCEPTION
              WHEN OTHERS THEN
                vcant_desp := 0;
            END;
            --Se identifica si la reserva esta en jaula o en ruta
            IF (vcant_desp > 0) THEN
              --Reserva esta en jaula
              UPDATE preruteo_reservadet pr
              SET --Las reservas deben permanecer en la misma bodega pero las tiendas se mueven desde el CC 0
                  pr.ccosto              = DECODE(pr.ccosto, 0, vCCOSTO, pr.ccosto),
                  pr.confirmado          = 1,
                  pr.NUM_JAULA           = vNUM_JAULA,
                  pr.secuencia_entrega   = vSEQ_ENTREGA,
                  pr.inicio_horario_1    = vINICIOHORARIO1,
                  pr.inicio_horario_2    = vINICIOHORARIO2,
                  pr.fin_horario_1       = vFINHORARIO1,
                  pr.fin_horario_2       = vFINHORARIO2,
                  pr.inicio_visita       = vINICIOVISITA,
                  pr.fin_visita          = vFINVISITA,
                  pr.inicio_visita_fecha = vINICIOVISITA_FECHA,
                  pr.fin_visita_fecha    = vFINVISITA_FECHA,
                  pr.tiempo_espera       = vTIEMPOESPERA,
                  pr.id_rutauni          = vIDRUTAUNI
              WHERE pr.num_reserva      = vNUM_RESERVA
                AND ABS(pr.num_detalle) = ABS(vNUM_DETALLE)
                AND pr.en_jaula         = 1
                --AND pr.CCOSTO = vCCOSTO_SALIDA
              ;
              --Se registra la fecha/hora de entrega original
              UPDATE reserva_dtl rd
              SET rd.fecha_visita_ini     = vINICIOVISITA_FECHA,
                  rd.fecha_visita_ini_ori = vINICIOVISITA_FECHA,
                  rd.fecha_visita_fin     = vFINVISITA_FECHA,
                  rd.fecha_visita_fin_ori = vFINVISITA_FECHA
              WHERE rd.num_reserva      = vNUM_RESERVA
                AND ABS(rd.num_detalle) = ABS(vNUM_DETALLE)
              ;
              --INI: Gestionar reservas preruteadas y confirmadas para pasar de estado 9 a 10.
              BEGIN
                vcod_estadodet := 0;
                vcant_desp     := 0;
                SELECT cod_estadodet,
                       SUM(CASE
                             WHEN rd.num_detalle > 0 THEN 1
                             ELSE 0
                           END)
                INTO vcod_estadodet, vcant_desp
                FROM preruteo_reservadet pr,
                     reserva_dtl rd
                WHERE pr.canal_venta      = rd.canal_venta
                  AND pr.num_reserva      = rd.num_reserva
                  AND pr.num_detalle      = rd.num_detalle
                  AND pr.num_reserva      = vNUM_RESERVA
                  AND ABS(pr.num_detalle) = ABS(vNUM_DETALLE)
                  AND pr.en_jaula         = 1
                  AND confirmado          = 1 --Tiene q estar confirmada para generar cambio automatico
                GROUP BY cod_estadodet
                ;
              EXCEPTION
                WHEN NO_DATA_FOUND THEN
                  vcod_estadodet := 0;
              END;
              --Se autoconfirma el preruteo (DAD -> WMS)
              IF (vcod_estadodet = 9) THEN
                UPDATE reserva_dtl
                SET cod_estadodet  = DECODE(vcant_desp, 1, 10, 11),
                    usuario_evento = 'UNIGIS'
                WHERE num_reserva      = vNUM_RESERVA
                  AND ABS(num_detalle) = ABS(vNUM_DETALLE)
                ;
                --Enviar pkt a WMOS
                INSERT INTO wli_stage_preruteo(num_pkt, num_jaula, wharehouse, campo1, campo2, campo3)
                SELECT DISTINCT rd.num_pkt, pr.num_jaula, cc.wharehouse, NULL, NULL, NULL
                FROM preruteo_reservadet pr, reserva_dtl rd, centro_costo cc
                WHERE pr.canal_venta      = rd.canal_venta
                  AND pr.num_reserva      = rd.num_reserva
                  AND pr.num_detalle      = rd.num_detalle
                  AND pr.num_reserva      = vNUM_RESERVA
                  AND ABS(pr.num_detalle) = ABS(vNUM_DETALLE) --Puede llegar el -1 antes que el 1
                  AND pr.num_detalle      > 0                 --Para enviar solo el 1
                  AND pr.en_jaula         = 1
                  AND pr.confirmado       = 1
                  AND rd.cod_estadopkt   != 90
                  AND rd.num_pkt IS NOT NULL
                  AND pr.cod_hojaruta IS NULL
                  AND cc.org_lvl_number = DECODE((SELECT org_is_store
                                                  FROM centro_costo cc2
                                                  WHERE cc2.org_lvl_number = rd.cc_despacha),
                                                 'T', rd.cc_despacha,
                                                 rd.cc_origen)
                ;

              END IF;

              --Solo se generan GDR para bodega y si el detalle no tiene una GDR activa
              vgdactivas_bod := 0;
              SELECT COUNT(*)
              INTO vgdactivas_bod
              FROM gdd_dtl gd,
                   gdd_hdr gh
              WHERE gd.ccosto_gd    = gh.ccosto_gd
                AND gd.num_gd       = gh.num_gd
                AND gd.num_reserva  = vNUM_RESERVA
                AND gd.num_detalle  = vNUM_DETALLE
                AND gh.estado_gd    = 1
                AND 'T' = (SELECT org_is_store
                           FROM reserva_dtl rd,
                                centro_costo cc
                           WHERE cc.org_lvl_number = rd.cc_despacha
                             AND rd.num_reserva    = vNUM_RESERVA
                             AND rd.num_detalle    = vNUM_DETALLE
                          )
              ;
              --Solo para bodega y cuando es retiro
              IF (varEsSoloRetiro = 'R' AND vgdactivas_bod = 0) THEN
                SELECT SEQ_WLISTAGE_UNI.NEXTVAL INTO VAR_SEQUENCIA FROM DUAL;
                -- CREAMOS LA CABECERA EN LA STG DE L ENVIO DE UNIGIS
                INSERT INTO wli_stage_uni_hdr(
                        --fecha,           --por defecto SYSDATE
                        sequencia_dad,
                        cod_estadouni,
                        ccosto, --SI
                        canal_venta, --SI
                        num_reserva, --SI
                        orden, --SI NULL
                        num_ordendet, --SI NULL
                        tipo_idorden,-- SI
                        tipo_orden, --SI NULL
                        cod_estadocab, --SI
                        fechapactada, --SI
                        type_ide, -- SI NULL
                        ide, --SI
                        ide_dv, --SI
                        nombre_cli, --SI
                        direccion_cli, --SI
                        comuna_cli, --SI
                        ciudad_cli, --SI
                        region_cli, --SI
                        latitud_cli, --SI
                        longitud_cli, --SI
                        calle_cli, --SI
                        numero_cli, --SI NULL
                        entre_calle_cli, --SI
                        barrio_cli, --SI NULL
                        fono_cli, --SI
                        fono2_cli, --SI NULL
                        fono3_cli, -- SI NULL
                        email_cli, -- SI
                        domicilio_ext_cli, --SI NULL
                        domicilio_desc_cli, --SI NULL
                        tiempo_espera_cli, -- SI
                        descripcion_desp, --SI NULL
                        nombre_desp, --SI
                        direccion_desp,  --SI
                        calle_desp, --SI NULL
                        numero_desp, --SI NULL
                        ref_calle_desp, -- SI NULL
                        barrio_desp, --SI NULL
                        comuna_desp, --SI
                        ciudad_desp, --SI
                        region_desp, --SI
                        ini_hora_plani,
                        fin_hora_plani,
                        ini_hora_desp1,
                        fin_hora_desp1,
                        ini_hora_desp2,
                        fin_hora_desp2,
                        ini_hora1,
                        fin_hora1,
                        ini_hora2,
                        fin_hora2,
                        tiempo_espera_desp,
                        prd_kilo, -- SI
                        prd_m3, --SI
                        bultos, --SI
                        pallets,
                        latitud_desp, -- SI
                        longitud_desp, --SI
                        observacion, --SI
                        email_desp, --SI
                        fono_desp, --SI
                        fono2_desp, --SI
                        fono3_desp, --SI NULL
                        campo1, -- SI NULL
                        campo2, -- SI NULL
                        campo3, -- SI NULL
                        campo4, -- SI NULL
                        id_rutauni, --SI
                        ccosto_cc1, --SI
                        ccosto_desc_cc1,  --SI
                        direccion_cc1, --SI
                        numero_cc1, -- SI NULL
                        bassaddr1_cc1,
                        bassaddr2_cc1,
                        bassaddr3_cc1,
                        ini_hora_cc1, --SI NULL
                        fin_hora_cc1, --SI NULL
                        tiempoespera_cc1,
                        latitud_cc1, --SI NULL
                        longitud_cc1, --SI NULL
                        calle_cc1, --SI NULL
                        ref_calle_cc1, --SI NULL
                        barrio_cc1, --SI NULL
                        comuna_cc1,
                        ciudad_cc1,
                        region_cc1,
                        ccosto_cc2,  --SI NULL
                        ccosto_desc_cc2, --SI NULL
                        direccion_cc2, --SI NULL
                        numero_cc2,--SI NULL
                        bassaddr1_cc2,
                        bassaddr2_cc2,
                        bassaddr3_cc2,
                        ini_hora_cc2,--SI NULL
                        fin_hora_cc2,--SI NULL
                        tiempoespera_cc2,
                        latitud_cc2,--SI NULL
                        longitud_cc2,--SI NULL
                        calle_cc2,--SI NULL
                        ref_calle_cc2,--SI NULL
                        barrio_cc2,--SI NULL
                        comuna_cc2,
                        ciudad_cc2,
                        region_cc2)
                SELECT DISTINCT
                       VAR_SEQUENCIA,
                       1, --cod_estadouni --Por Enviar
                       pr.ccosto,
                       rh.canal_venta,
                       rh.num_reserva,
                       null, --orden
                       null, --num_ordendet,
                       rh.tipo_reserva, --tipo_idorden
                       null, --tipo_orden
                       DECODE(NVL(rd.bloqueado,0),1,18,rd.cod_estadodet),--rh.cod_estadocab, --ATENCION: UNIGIS solo trabaja a nivel de cabecera
                       rd.fecha_despacho, --TRUNC(SYSDATE + 1), --:NEW.fecha_entrega, --fechapactada
                       rh.type_ide,
                       rh.ide,
                       rh.ide_dv,
                       rh.nombre_cli,
                       rh.direccion_cli,
                       rh.comuna_cli,
                       rh.ciudad_cli,
                       rh.region_cli,
                       null, --latitud_cli
                       null, --longitud_cli
                       null, --calle_cli
                       null, --numero_cli
                       null, --entre_calle_cli
                       null, --barrio_cli
                       null, --rh.fono_cli,
                       null, --fono2_cli
                       null, --fono3_cli
                       rh.e_mail, --e_mail_cli
                       null, --domicilio_ext_cli
                       null, --domicilio_desc_cli
                       null, --tiempo_espera_cli
                       null, --descripcion_desp
                       rh.nombre_desp,
                       rh.direccion_desp,
                       null, --calle_desp
                       null, --numero_desp
                       null, --ref_calle_desp
                       null, --barrio_desp
                       rh.comuna_desp,
                       rh.ciudad_desp,
                       rh.region_desp,
                       null, --ini_hora_plani
                       null, --fin_hora_plani
                       null, --ini_hora_desp1,
                       null, --fin_hora_desp1,
                       null, --ini_hora_desp2 --
                       null, --fin_hora_desp2
                       pr.inicio_horario_1, --ini_hora1 --pr.inicio_horario_1
                       pr.fin_horario_1, --fin_hora1 --pr.fin_horario_1
                       pr.inicio_horario_2, --ini_hora2 --pr.inicio_horario_2
                       pr.fin_horario_2, --fin_hora2 --pr.fin_horario_2
                       null, --tiempo_espera_desp
                       0, --:NEW.prd_kilo,  --null prd_kilo,
                       0, --:NEW.prd_m3, --null prd_m3,
                       0, --NVL(:NEW.bultos, 1), --null bultos,
                       null pallets,
                       rh.latitud, --latitud_desp,
                       rh.longitud, --longitud_desp,
                       rh.observacion,
                       rh.e_mail, --e_mail_desp,
                       rh.fono_desp,
                       DECODE(rh.fono2_desp, NULL, rh.fono_cli, rh.fono2_desp),
                       null, --fono3_desp
                       null, --campo1
                       null, --campo2
                       null, --campo3
                       null, --campo4
                       pr.id_rutauni,
                       cc.org_lvl_number, --ccosto_cc1
                       cc.org_name_full, --ccosto_desc_cc1
                       null, --direccion_cc1
                       null, --numero_cc1
                       cc.bas_addr_1, --bassaddr1_cc1
                       cc.bas_addr_2, --bassaddr2_cc1
                       cc.bas_addr_3, --bassaddr3_cc1
                       null, --ini_hora_cc1
                       null, --fin_hora_cc1
                       null, --tiempo_espera_cc1
                       null, --cc.latitud, --latitud_cc1
                       null, --cc.longitud, --longitud_cc1
                       null, --calle_cc1
                       null, --ref_calle_cc1
                       null, --barrio_cc1
                       null, --cc.comuna, --comuna_cc1
                       null, --cc.ciudad, --comuna_cc1
                       null, --cc.region, --region_cc1
                       null, --ccosto_cc2
                       null, --ccosto_desc_cc2
                       null, --direccion_cc2
                       null, --numero_cc2
                       null, --bassaddr2_cc2
                       null, --bassaddr2_cc2
                       null, --bassaddr3_cc2
                       null, --ini_hora_cc2
                       null, --fin_hora_cc2
                       null, --tiempo_espera_cc2
                       null, --latitud_cc2
                       null, --longitud_cc2
                       null, --calle_cc2
                       null, --ref_calle_cc2
                       null, --barrio_cc2
                       null, --comuna_cc2
                       null, --comuna_cc2
                       null --region_cc2
                FROM preruteo_reservadet pr,
                     reserva_hdr rh,
                     reserva_dtl rd,
                     centro_costo cc
                WHERE pr.canal_venta    = rh.canal_venta
                  AND pr.num_reserva    = rh.num_reserva
                  AND pr.canal_venta    = rd.canal_venta
                  AND pr.num_reserva    = rd.num_reserva
                  AND pr.num_reserva    = vNUM_RESERVA
                  AND pr.num_detalle    = vNUM_DETALLE
                  AND rd.num_detalle    = vNUM_DETALLE
                  AND pr.ccosto         = cc.org_lvl_number
                  /* se validan estas dos condiciones */
                  AND pr.en_jaula       = 1
                  AND pr.informado     IS NULL
                      /* se validan estas dos condiciones */
                ;

                INSERT INTO wli_stage_uni_dtl(
                      sequencia_dad,
                      cod_estadouni,
                      canal_venta,
                      num_reserva,
                      num_detalle,
                      cod_estadodet,
                      prd_lvl_number,
                      prd_name_full,
                      cantidad,
                      prd_m3_uni,
                      prd_m3_tot,
                      prd_kilo_uni,
                      prd_kilo_tot,
                      bultos_uni,
                      bultos_tot,
                      alto,
                      ancho,
                      profundidad,
                      apilable,
                      rotacion,
                      pallets,
                      num_gdd)
                SELECT  VAR_SEQUENCIA,
                        1, --Por Enviar
                        rd.canal_venta,
                        rd.num_reserva,
                        rd.num_detalle,
                        DECODE(NVL(rd.bloqueado,0),1,18,rd.cod_estadodet),
                        rd.prd_lvl_number,
                        rd.prd_name_full,
                        ROUND(rd.cantidad),
                        rd.prd_m3,
                        rd.prd_m3, --:NEW.prd_m3 * :NEW.cantidad,
                        rd.prd_kilo,
                        rd.prd_kilo, --:NEW.prd_kilo * :NEW.cantidad,
                        0, --NVL(:NEW.bultos, 1),
                        0, --NVL(:NEW.bultos, 1) * :NEW.cantidad,
                        NULL alto,
                        NULL ancho,
                        NULL profundidad,
                        NULL apilable,
                        NULL rotacion,
                        NULL pallets,
                        (SELECT MAX(gdd.num_gd)
                         FROM gdd_dtl gdd
                         WHERE pr.canal_venta    = gdd.canal_venta
                           AND pr.num_detalle    = gdd.num_detalle
                           AND rd.num_reserva    = gdd.num_reserva) AS num_gd
                FROM preruteo_reservadet pr,
                     reserva_dtl rd
                WHERE pr.num_reserva    = vNUM_RESERVA
                  AND pr.num_detalle    = vNUM_DETALLE
                  AND rd.num_reserva    = vNUM_RESERVA
                  AND rd.num_detalle    = vNUM_DETALLE
                  /* se validan estas dos condiciones */
                  AND pr.en_jaula       = 1
                  AND pr.informado     IS NULL
                ;

                reserva_elmt := xmldom.createElement(docSalida, 'Reserva');
                reserva_node := xmldom.appendChild(root_node, xmldom.makeNode(reserva_elmt) );
                item_elmt := xmldom.createElement(docSalida, 'CentroCosto');
                item_node := xmldom.appendChild(reserva_node, xmldom.makeNode(item_elmt));
                item_text := xmldom.createTextNode(docSalida, vCCOSTO_SALIDA);
                item_node := xmldom.appendChild(item_node, xmldom.makeNode(item_text));
                item_elmt := xmldom.createElement(docSalida, 'NroReserva');
                item_node := xmldom.appendChild(reserva_node, xmldom.makeNode(item_elmt));
                item_text := xmldom.createTextNode(docSalida, vNUM_RESERVA);
                item_node := xmldom.appendChild(item_node, xmldom.makeNode(item_text));
                item_elmt := xmldom.createElement(docSalida, 'NroDetalle');
                item_node := xmldom.appendChild(reserva_node, xmldom.makeNode(item_elmt));
                item_text := xmldom.createTextNode(docSalida, vNUM_DETALLE);
                item_node := xmldom.appendChild(item_node, xmldom.makeNode(item_text));

              END IF;
              --FIN: Gestionar reservas preuteadas y confirmadas para pasar de estado 9 a 10.

            ELSE
              --Reserva esta en reparto
              --Corresponde una actualizacion de la fecha/hora de entrega
              UPDATE reserva_dtl rd
              SET rd.fecha_visita_ini = vINICIOVISITA_FECHA,
                  rd.fecha_visita_fin = vFINVISITA_FECHA
              WHERE rd.num_reserva      = vNUM_RESERVA
                AND ABS(rd.num_detalle) = ABS(vNUM_DETALLE)
                AND rd.cod_estadodet IN (12, 112)
              ;

            END IF;
          END LOOP;
          --fin loop ListaItemProducto************************************************************
        END LOOP;
        --fin loop ItemOrden************************************************************

        PCKGENERAGDDSAVE(dbms_xmldom.getXmlType(docSalida),"rs");

        MERGE INTO wli_stage_uni_dtl wli
        USING (SELECT gd.num_gd,
                      q.rowid AS rid
                 FROM gdd_dtl           gd,
                      wli_stage_uni_dtl q
                WHERE gd.num_reserva = vnum_reserva
                  AND gd.num_detalle = q.num_detalle
                  AND gd.num_reserva = q.num_reserva
                  AND NOT EXISTS
                (SELECT 1 --busca el numero maximo de guia por reserva
                         FROM gdd_dtl mxg
                        WHERE mxg.num_reserva = gd.num_reserva
                          AND mxg.num_gd > gd.num_gd)
                  AND NOT EXISTS
                (SELECT 1 --busca el numero maximo de secuencia por detalle
                         FROM wli_stage_uni_dtl mxw
                        WHERE mxw.num_reserva = q.num_reserva
                          AND mxw.num_detalle = q.num_detalle
                          AND mxw.sequencia_dad > q.sequencia_dad)) aux
        ON (wli.rowid = aux.rid AND wli.cod_estadouni = 1 AND wli.num_detalle < 0)
        WHEN MATCHED THEN
           UPDATE SET wli.num_gdd = aux.num_gd;

      END LOOP;
      --fin loop OrdenDatos***********************************************************
    END LOOP;
    --fin loop ListaOrden*************************************************************

    --Se generan las GD de retiro para UNIGIS

  EXCEPTION
    WHEN OTHERS THEN
      OUT_STATUS_CODE := 1;
      OUT_STATUS_MSG  := 'DAD: ' || SQLERRM;
      RETURN;
  END;
END REGISTRAR_PRERUTEO_UNIGIS;

PROCEDURE CONFIRMAR_HOJARUTA_UNIGIS(
  IN_XML           IN  XMLTYPE,
  IN_PROCESS_LUW   IN  VARCHAR2 DEFAULT 'T',
  OUT_STATUS_CODE  OUT NUMBER,
  OUT_STATUS_MSG   OUT VARCHAR2) AS
BEGIN
  DECLARE
  --VARIABLES PARA EL MANEJO DEL XML
  DOC             DBMS_XMLDOM.DOMDOCUMENT;
  NODE            DBMS_XMLDOM.DOMNODE;
  HIJO            DBMS_XMLDOM.DOMNODE;
  NODELIST        DBMS_XMLDOM.DOMNODELIST;
  HIJOSLIST       DBMS_XMLDOM.DOMNODELIST;
  NUMNODES        NUMBER;
  NUMHIJOS        NUMBER;
  ROWCOUNTER      NUMBER;
  NODOACTUAL      DBMS_XMLDOM.DOMNODE;

  --VARIABLES DE Ruta
  vCCOSTO                NUMBER;
  vNUM_JAULA             NUMBER;
  vID_RUTA_UNI           NUMBER;
  vID_VIAJE              VARCHAR (20);
  vTIPO_DOCUMENTO        VARCHAR (30);
  vRUT_CHOFER            VARCHAR (15);
  vPATENTE               VARCHAR (15);
  vCOD_TRANSP            VARCHAR (15);
  vFECHA_INI_UNI         VARCHAR (15);
  vFECHA_FIN_UNI         VARCHAR (15);
  vINFO_ADICIONAL_UNO    VARCHAR (15);
  vINFO_ADICIONAL_DOS    VARCHAR (15);
  vCOD_ESTADO_VIAJE      VARCHAR (15);

  -- VARIABLES AUXILIARES
  vAUX      VARCHAR2(4000);
  V_EMPNO   HOJARUTA.COD_HOJARUTA%TYPE;

  BEGIN
    OUT_STATUS_CODE := 0;
    OUT_STATUS_MSG  := 'OK';

    DOC            := DBMS_XMLDOM.NEWDOMDOCUMENT(IN_XML);
    NODE           := DBMS_XMLDOM.MAKENODE(DOC);
    NODELIST       := XSLPROCESSOR.SELECTNODES(NODE, '/Ruta','xmlns="http://openuri.org/i/dadHojaRutaConfirmarDoc"');
    NUMNODES       := DBMS_XMLDOM.GETLENGTH(NODELIST);

    --inicio loop Ruta**************************************************************
    --******************************************************************************
    FOR ROWCOUNTER IN 0..NUMNODES-1 LOOP

      NODOACTUAL              := DBMS_XMLDOM.ITEM(NODELIST, ROWCOUNTER);

      HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'cCosto');
      HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
      vCCOSTO                 := DBMS_XMLDOM.GETNODEVALUE(HIJO);

      HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'numJaula');
      HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
      vNUM_JAULA              := DBMS_XMLDOM.GETNODEVALUE(HIJO);

      HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'idRutaUni');
      HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
      vID_RUTA_UNI            := DBMS_XMLDOM.GETNODEVALUE(HIJO);

      HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'idViaje');
      HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
      vID_VIAJE               := DBMS_XMLDOM.GETNODEVALUE(HIJO);

      HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'tipoDocumento');
      HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
      vTIPO_DOCUMENTO         := DBMS_XMLDOM.GETNODEVALUE(HIJO);

      HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'rutChofer');
      HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
      vRUT_CHOFER             := DBMS_XMLDOM.GETNODEVALUE(HIJO);

      HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'patente');
      HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
      vPATENTE                := TRIM(DBMS_XMLDOM.GETNODEVALUE(HIJO));

      HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'codTransp');
      HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
      vCOD_TRANSP             := TRIM(DBMS_XMLDOM.GETNODEVALUE(HIJO));

      HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'fechaIniUni');
      HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
      vFECHA_INI_UNI          := DBMS_XMLDOM.GETNODEVALUE(HIJO);

      HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'fechaFinUni');
      HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
      vFECHA_FIN_UNI          := DBMS_XMLDOM.GETNODEVALUE(HIJO);

      HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'informacionAdicionalUno');
      HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
      vINFO_ADICIONAL_UNO     := DBMS_XMLDOM.GETNODEVALUE(HIJO);

      HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'informacionAdicionalDos');
      HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
      vINFO_ADICIONAL_DOS     := DBMS_XMLDOM.GETNODEVALUE(HIJO);

      HIJO                    := XSLPROCESSOR.SELECTSINGLENODE(NODOACTUAL,'codigoEstadoViaje');
      HIJO                    := XMLDOM.GETFIRSTCHILD(HIJO);
      vCOD_ESTADO_VIAJE       := DBMS_XMLDOM.GETNODEVALUE(HIJO);

      vid_ruta_uni := NULL;
      --Transfiere reservas bloqueadas al Muelle para luego sacarlas de la jaula (reservas en jaula en estado 11 y bloquedas)
      UPDATE reserva_dtl rd
      SET rd.cod_estadodet = 16,
          rd.usuario_evento = 'UNIGIS'
      WHERE (rd.CANAL_VENTA, rd.NUM_RESERVA, rd.NUM_DETALLE) IN
            (SELECT RD.CANAL_VENTA,
                    RD.NUM_RESERVA,
                    RD.NUM_DETALLE
               FROM  PRERUTEO_RESERVADET PR, RESERVA_DTL RD
               WHERE RD.CANAL_VENTA   = PR.CANAL_VENTA
                 AND RD.NUM_RESERVA   = PR.NUM_RESERVA
                 AND RD.NUM_DETALLE   = PR.NUM_DETALLE
                 AND PR.CCOSTO        = vCCOSTO
                 AND PR.NUM_JAULA     = vNUM_JAULA
                 AND PR.EN_JAULA      = 1
                 AND RD.COD_ESTADODET = 11
                 AND RD.BLOQUEADO     = 1)
      ;

      SELECT SEQ_HOJARUTA.NEXTVAL INTO V_EMPNO FROM DUAL;

      --Se genera la HR
      INSERT INTO HOJARUTA(
             COD_HOJARUTA,
             ID_RUTAUNI,
             COD_TRANSP,
             RUT_CHOFER,
             PATENTE,
             FECHA,
             USUARIO_CREA,
             OBSERVACIONES,
             CANTIDAD,
             PESO,
             VOLUMEN)
      VALUES(V_EMPNO,
             vID_VIAJE,
             vCOD_TRANSP,
             vRUT_CHOFER,
             vPATENTE,
             SYSDATE,
             'UNIGIS',
             '',
             (SELECT SUM(RD.CANTIDAD)
                FROM PRERUTEO_RESERVADET PR, RESERVA_DTL RD
               WHERE RD.CANAL_VENTA = PR.CANAL_VENTA
                 AND RD.NUM_RESERVA = PR.NUM_RESERVA
                 AND RD.NUM_DETALLE = PR.NUM_DETALLE
                 AND PR.CCOSTO = vCCOSTO
                 AND PR.NUM_JAULA = vNUM_JAULA
                 AND PR.EN_JAULA = 1
                 AND PR.CONFIRMADO = 1
                 AND RD.COD_ESTADODET = 11),
             (SELECT SUM(RD.PRD_KILO * RD.CANTIDAD)
                FROM PRERUTEO_RESERVADET PR, RESERVA_DTL RD
               WHERE RD.CANAL_VENTA = PR.CANAL_VENTA
                 AND RD.NUM_RESERVA = PR.NUM_RESERVA
                 AND RD.NUM_DETALLE = PR.NUM_DETALLE
                 AND PR.CCOSTO = vCCOSTO
                 AND PR.NUM_JAULA = vNUM_JAULA
                 AND PR.EN_JAULA = 1
                 AND PR.CONFIRMADO = 1
                 AND RD.COD_ESTADODET = 11),
             (SELECT SUM(RD.PRD_M3 * RD.CANTIDAD)
                FROM PRERUTEO_RESERVADET PR, RESERVA_DTL RD
               WHERE RD.CANAL_VENTA = PR.CANAL_VENTA
                 AND RD.NUM_RESERVA = PR.NUM_RESERVA
                 AND RD.NUM_DETALLE = PR.NUM_DETALLE
                 AND PR.CCOSTO = vCCOSTO
                 AND PR.NUM_JAULA = vNUM_JAULA
                 AND PR.EN_JAULA = 1
                 AND PR.CONFIRMADO = 1
                 AND RD.COD_ESTADODET = 11))
      ;
      --Se asigna transportista a las GDR de UNIGIS
      UPDATE gdd_hdr g
      SET g.cod_hojaruta  = V_EMPNO,
          g.patente       = vPATENTE,
          g.rut_chofer    = vRUT_CHOFER
      WHERE (g.ccosto_gd, g.num_gd) IN
            (SELECT DISTINCT gh.ccosto_gd, gh.num_gd
             FROM preruteo_reservadet pr,
                  reserva_dtl         rd,
                  gdd_dtl             gd,
                  gdd_hdr             gh
             WHERE pr.canal_venta   = rd.canal_venta
               AND pr.num_reserva   = rd.num_reserva
               AND pr.num_detalle   = rd.num_detalle
               AND gd.canal_venta   = rd.canal_venta
               AND gd.num_reserva   = rd.num_reserva
               AND gd.num_detalle   = rd.num_detalle
               AND gd.ccosto_gd     = gh.ccosto_gd
               AND gd.num_gd        = gh.num_gd
               AND pr.ccosto        = vccosto
               AND pr.num_jaula     = vnum_jaula
               AND pr.en_jaula      = 1
               AND pr.confirmado    = 1
               AND rd.cod_estadodet = 11
               AND gh.estado_gd     = 1
               AND gh.cod_hojaruta  IS NULL
               AND gh.patente       IS NULL
               AND gh.rut_chofer    IS NULL)
      ;
      --Asigna la HR a las reservas que saldran
      UPDATE PRERUTEO_RESERVADET pr
         SET COD_HOJARUTA  = V_EMPNO,
             EN_JAULA      = 0,
             INFORMADO     = 0,
             pr.id_rutauni = vid_ruta_uni,
             pr.id_viaje   = vid_viaje
       WHERE (CANAL_VENTA, NUM_RESERVA, NUM_DETALLE, COD_PRERUTEO) IN
             (SELECT RD.CANAL_VENTA,
                     RD.NUM_RESERVA,
                     RD.NUM_DETALLE,
                     PR.COD_PRERUTEO
                FROM PRERUTEO_RESERVADET PR, RESERVA_DTL RD
               WHERE RD.CANAL_VENTA   = PR.CANAL_VENTA
                 AND RD.NUM_RESERVA   = PR.NUM_RESERVA
                 AND RD.NUM_DETALLE   = PR.NUM_DETALLE
                 AND PR.CCOSTO        = vCCOSTO
                 AND PR.NUM_JAULA     = vNUM_JAULA
                 AND PR.EN_JAULA      = 1
                 AND PR.CONFIRMADO    = 1
                 AND RD.COD_ESTADODET = 11)
      ;
      --Se pasan a En Ruta las reservas de la HR
      UPDATE RESERVA_DTL
         SET COD_ESTADODET  = 12,
             COD_HOJARUTA   = V_EMPNO,
             USUARIO_EVENTO = 'UNIGIS',
             FECHA_DESPACHO = CASE WHEN FECHA_DESPACHO > SYSDATE THEN
                                SYSDATE
                              ELSE
                                FECHA_DESPACHO
                               END
       WHERE (CANAL_VENTA, NUM_RESERVA, NUM_DETALLE) IN
             (SELECT PR.CANAL_VENTA, PR.NUM_RESERVA, PR.NUM_DETALLE
                FROM PRERUTEO_RESERVADET PR
               WHERE PR.COD_HOJARUTA = V_EMPNO
              --   AND pr.ccosto       = pccosto
              --   AND pr.num_jaula    = pjaula
              )
      ;
      --Se sacan de la jaula las Reingreso y Muelle DAD
      UPDATE PRERUTEO_RESERVADET
         SET EN_JAULA  = 0,
             INFORMADO = 0
       WHERE (CANAL_VENTA, NUM_RESERVA, NUM_DETALLE, COD_PRERUTEO) IN
             (SELECT RES.CANAL_VENTA,
                     RES.NUM_RESERVA,
                     RES.NUM_DETALLE,
                     PRE.COD_PRERUTEO
                FROM PRERUTEO_RESERVADET PRE, RESERVA_DTL RES
               WHERE PRE.NUM_RESERVA = RES.NUM_RESERVA
                 AND PRE.NUM_DETALLE = RES.NUM_DETALLE
                 AND PRE.CANAL_VENTA = RES.CANAL_VENTA
                 AND PRE.CCOSTO      = vCCOSTO
                 AND PRE.NUM_JAULA   = vNUM_JAULA
                 AND PRE.EN_JAULA    = 1
                 AND PRE.CONFIRMADO  = 1
                 AND RES.COD_ESTADODET IN (15, 16))
      ;
      --En WMOS se limpian las jaulas de los PKT no informados.
      INSERT INTO wli_stage_preruteo(num_pkt, num_jaula, wharehouse, campo1, campo2, campo3)
        SELECT DISTINCT rd.num_pkt, 0, wharehouse, NULL, NULL, NULL
          FROM preruteo_reservadet pre, reserva_dtl rd, centro_costo cc
         WHERE rd.canal_venta    = pre.canal_venta
           AND rd.num_reserva    = pre.num_reserva
           AND rd.num_detalle    = pre.num_detalle
           AND cc.org_lvl_number = DECODE((SELECT org_is_store FROM centro_costo cc2 WHERE cc2.org_lvl_number = rd.cc_despacha),
                                          'T', rd.cc_despacha,
                                          rd.cc_origen)
           AND pre.ccosto        = vCCOSTO
           AND pre.num_jaula     = vNUM_JAULA
           AND pre.en_jaula      = 1
           AND pre.confirmado    = 1
           AND rd.cod_estadodet IN (10)
           AND (cod_estadopkt IS NULL OR cod_estadopkt != 90)
           AND rd.cc_origen IN
               (SELECT org_lvl_number
                FROM centro_costo
                WHERE recibe_preruteo = 'T')
      ;
      -- Las reservas no procesadas se pasan de 10 a 9
      UPDATE RESERVA_DTL
         SET COD_ESTADODET  = 9,
             USUARIO_EVENTO = 'UNIGIS'
       WHERE (COD_ESTADOPKT IS NULL OR COD_ESTADOPKT != 90)
         AND (CANAL_VENTA, NUM_RESERVA, NUM_DETALLE) IN
             (SELECT RES.CANAL_VENTA, RES.NUM_RESERVA, RES.NUM_DETALLE
                FROM PRERUTEO_RESERVADET PRE, RESERVA_DTL RES
               WHERE PRE.NUM_RESERVA = RES.NUM_RESERVA
                 AND PRE.NUM_DETALLE = RES.NUM_DETALLE
                 AND PRE.CANAL_VENTA = RES.CANAL_VENTA
                 AND PRE.CCOSTO      = vCCOSTO
                 AND PRE.NUM_JAULA   = vNUM_JAULA
                 AND PRE.EN_JAULA    = 1
                 AND PRE.CONFIRMADO  = 1
                 AND RES.COD_ESTADODET IN (10))
      ;
      -- Las reservas no procesadas se sacan de la jaula
      UPDATE PRERUTEO_RESERVADET
         SET EN_JAULA = 0
       WHERE (CANAL_VENTA, NUM_RESERVA, NUM_DETALLE) IN
             (SELECT RES.CANAL_VENTA, RES.NUM_RESERVA, RES.NUM_DETALLE
                FROM PRERUTEO_RESERVADET PRE, RESERVA_DTL RES
               WHERE PRE.NUM_RESERVA = RES.NUM_RESERVA
                 AND PRE.NUM_DETALLE = RES.NUM_DETALLE
                 AND PRE.CANAL_VENTA = RES.CANAL_VENTA
                 AND PRE.CCOSTO      = vCCOSTO
                 AND PRE.NUM_JAULA   = vNUM_JAULA
                 AND PRE.EN_JAULA    = 1
                 AND PRE.CONFIRMADO  = 1
              -- AND RES.COD_ESTADODET IN (5, 6, 7, 8, 9, 50, 14, 18, 23, 123));
                 AND RES.COD_ESTADODET NOT IN (12))
      ;
      -- Se libera la jaula de unigis siempre y cuando esta este marcada comom unigis
      UPDATE jaula j
      SET j.jaula_unigis = 0
      WHERE j.num_jaula    = vNUM_JAULA
        AND j.ccosto       = vCCOSTO
        AND j.jaula_unigis = 1
      ;

    END LOOP;
    --fin loop Ruta***********************************************************
    --********************************************************************************

  EXCEPTION
    WHEN OTHERS THEN
      OUT_STATUS_CODE := 1;
      OUT_STATUS_MSG  := 'DAD: ' || SQLERRM;
      RETURN;

  END;

  IF IN_PROCESS_LUW = 'T' THEN
       COMMIT ;
  END IF;

END CONFIRMAR_HOJARUTA_UNIGIS;

PROCEDURE CONSULTA_RESERVA(
  pcanal_venta    IN NUMBER,
  pnum_reserva     IN NUMBER,
  pnum_ordenventa IN NUMBER,
  in_process_luw  IN VARCHAR2,
  OUT_STATUS_CODE OUT NUMBER,
  OUT_STATUS_MSG  OUT VARCHAR,
  out_cursor_reserva_hdr           OUT sys_refcursor,
  out_cursor_reserva_dtl           OUT sys_refcursor,
  out_cursor_reservaPrima_hdr      OUT sys_refcursor,
  out_cursor_reservaPrima_dtl      OUT sys_refcursor) AS
BEGIN
DECLARE
  /*
  * PADRE
  * CURSOR CABECERA
  */
  BEGIN
    OPEN out_cursor_reserva_hdr FOR
    SELECT
    CANAL_VENTA,
    NUM_RESERVA,
    COD_ESTADOCAB,
    ORG_LVL_NUMBER,
    NUM_ORDENVENTA,
    FECHA_RESERVA,
    FECHA_PAGO,
    ID_VENDEDOR,
    TYPE_DOC,
    NUM_DOC,
    MONTO,
    DOC_DATE,
    NUM_CAJA,
    TYPE_IDE,
    IDE,
    IDE_DV,
    REGION_CLI,
    CIUDAD_CLI,
    COMUNA_CLI,
    NOMBRE_CLI,
    DIRECCION_CLI,
    FONO_CLI,
    REGION_DESP,
    CIUDAD_DESP,
    COMUNA_DESP,
    DIRECCION_DESP,
    NOMBRE_DESP,
    FONO_DESP,
    FONO2_DESP,
    OBSERVACION,
    DESACTIVADO,
    MARCA_GUIA,
    CAMPO1,
    CAMPO2,
    CAMPO3,
    CAMPO4,
    CAMPO5,
    CAMPO6,
    CAMPO7,
    CAMPO8,
    CAMPO9,
    CAMPO10,
    NUMRSV_PADRE
    FROM reserva_hdr h
    WHERE h.canal_venta = pcanal_venta
      AND (h.num_reserva = pnum_reserva OR (h.num_ordenventa = pnum_ordenventa AND NVL(h.numrsv_padre, 0) = 0))
      AND h.cod_estadocab NOT IN (1,2,3);

  END;
  /*
  * PADRE
  * CURSOR DETALLE
  */
  BEGIN
    OPEN out_cursor_reserva_dtl FOR
    SELECT
    d.CANAL_VENTA,
    d.NUM_RESERVA,
    d.NUM_DETALLE,
    d.NUM_ORDENVENTA,
    d.SUB_ORDEN,
    d.PRD_LVL_NUMBER,
    d.PRD_NAME_FULL,
    d.COD_ESTADODET,
    d.CC_ORIGEN,
    d.CC_DESPACHA,
    d.CC_VIA,
    d.PRD_KILO,
    d.PRD_M3,
    d.TIPO_STOCK,
    d.PRIORIDAD,
    d.VENDOR_NUMBER,
    d.FECHA_RECEP_OC,
    d.FECHA_INI_PROCESO,
    d.FECHA_DESPACHO,
    d.FECHA_ENTREGA,
    d.FECHA_ENTREGA2,
    d.FECHA_ENTREGA_ORI,
    d.CANTIDAD,
    d.PRECIO,
    d.TIPO_PED_ODBMS,
    d.NUM_PED_ODBMS,
    d.DESC_ESTADO_ODBMS,
    d.TIPO_PED_DESPACHO,
    d.NUM_PED_DESPACHO,
    d.DESC_ESTADO_DESPACHO,
    d.NUM_PKT,
    d.COD_ESTADOPKT,
    d.CC_ENVIA,
    d.CC_RECIBE,
    d.NUM_GD,
    d.LPN_CASE_NBR,
    d.COD_MOTIVODEV,
    d.COMPLEMENTOS,
    d.FECHA_ENTREGA_CLI,
    d.BLOQUEADO,
    d.ENVOLTURA,
    d.TARJETA_MSJ,
    d.CAMPO1,
    d.CAMPO2,
    d.CAMPO3,
    d.CAMPO4,
    d.CAMPO5,
    d.CAMPO6,
    d.CAMPO7,
    d.CAMPO8,
    d.CAMPO9,
    d.CAMPO10,
    d.PENDIENTE,
    d.FECHA_DESPACHO_ORI,
    d.DATOS_HUA,
    d.CANTIDAD_ENTREGA_CLI,
    d.NUM_GD_PROV,
    d.NUM_SERIEGD_PROV,
    d.NUM_FACTURA_PROV,
    d.IDE_RECIVE,
    d.IDE_DV_RECIVE,
    d.NOMBRE_RECIVE,
    d.IDE_CHOFER,
    d.IDE_DV_CHOFER,
    d.NOMBRE_CHOFER,
    d.TIPO_DESPACHO,
    d.EXPRESS,
    d.NUM_RESERVA_B2B,
    d.PRIMA_PENDIENTE,
    d.CANT_DESP,
    d.CANT_RET,
    d.CANT_TRF,
    d.FECHA_RECEP
    FROM reserva_dtl d, reserva_hdr h
    WHERE d.canal_venta = pcanal_venta
      AND d.num_reserva = h.num_reserva
      AND d.canal_venta = h.canal_venta
      AND (d.num_reserva = pnum_reserva OR (d.num_ordenventa = pnum_ordenventa AND NVL(h.numrsv_padre, 0) = 0))
      AND h.cod_estadocab NOT IN (1,2,3);
  END;
  /*
  * PRIMA
  * CURSOR CABECERA
  */
  BEGIN
    OPEN out_cursor_reservaPrima_hdr FOR
    SELECT
    CANAL_VENTA,
    NUM_RESERVA,
    COD_ESTADOCAB,
    ORG_LVL_NUMBER,
    NUM_ORDENVENTA,
    FECHA_RESERVA,
    FECHA_PAGO,
    ID_VENDEDOR,
    TYPE_DOC,
    NUM_DOC,
    MONTO,
    DOC_DATE,
    NUM_CAJA,
    TYPE_IDE,
    IDE,
    IDE_DV,
    REGION_CLI,
    CIUDAD_CLI,
    COMUNA_CLI,
    NOMBRE_CLI,
    DIRECCION_CLI,
    FONO_CLI,
    REGION_DESP,
    CIUDAD_DESP,
    COMUNA_DESP,
    DIRECCION_DESP,
    NOMBRE_DESP,
    FONO_DESP,
    FONO2_DESP,
    OBSERVACION,
    DESACTIVADO,
    MARCA_GUIA,
    CAMPO1,
    CAMPO2,
    CAMPO3,
    CAMPO4,
    CAMPO5,
    CAMPO6,
    CAMPO7,
    CAMPO8,
    CAMPO9,
    CAMPO10,
    NUMRSV_PADRE
    FROM reserva_hdr h
    WHERE h.canal_venta = pcanal_venta
      AND h.num_reserva IN (SELECT h2.num_reserva FROM reserva_hdr h2
                            WHERE h2.numrsv_padre = pnum_reserva
                              OR (h2.num_ordenventa = pnum_ordenventa AND NVL(h2.numrsv_padre, 0) != 0))
      AND h.cod_estadocab NOT IN (1,2,3);
  END;
  /*
  * PRIMA
  * CURSOR DETALLE
  */
  BEGIN
    OPEN out_cursor_reservaPrima_dtl FOR
    SELECT
    d.CANAL_VENTA,
    d.NUM_RESERVA,
    d.NUM_DETALLE,
    d.NUM_ORDENVENTA,
    d.SUB_ORDEN,
    d.PRD_LVL_NUMBER,
    d.PRD_NAME_FULL,
    DECODE(d.COD_ESTADODET, 17, 22, 117, 122, d.COD_ESTADODET) AS COD_ESTADODET, --Parche por logica en Integracion
    d.CC_ORIGEN,
    d.CC_DESPACHA,
    d.CC_VIA,
    d.PRD_KILO,
    d.PRD_M3,
    d.TIPO_STOCK,
    d.PRIORIDAD,
    d.VENDOR_NUMBER,
    d.FECHA_RECEP_OC,
    d.FECHA_INI_PROCESO,
    d.FECHA_DESPACHO,
    d.FECHA_ENTREGA,
    d.FECHA_ENTREGA2,
    d.FECHA_ENTREGA_ORI,
    d.CANTIDAD,
    d.PRECIO,
    d.TIPO_PED_ODBMS,
    d.NUM_PED_ODBMS,
    d.DESC_ESTADO_ODBMS,
    d.TIPO_PED_DESPACHO,
    d.NUM_PED_DESPACHO,
    d.DESC_ESTADO_DESPACHO,
    d.NUM_PKT,
    d.COD_ESTADOPKT,
    d.CC_ENVIA,
    d.CC_RECIBE,
    d.NUM_GD,
    d.LPN_CASE_NBR,
    d.COD_MOTIVODEV,
    d.COMPLEMENTOS,
    d.FECHA_ENTREGA_CLI,
    d.BLOQUEADO,
    d.ENVOLTURA,
    d.TARJETA_MSJ,
    d.CAMPO1,
    d.CAMPO2,
    d.CAMPO3,
    d.CAMPO4,
    d.CAMPO5,
    d.CAMPO6,
    d.CAMPO7,
    d.CAMPO8,
    d.CAMPO9,
    d.CAMPO10,
    d.PENDIENTE,
    d.FECHA_DESPACHO_ORI,
    d.DATOS_HUA,
    d.CANTIDAD_ENTREGA_CLI,
    d.NUM_GD_PROV,
    d.NUM_SERIEGD_PROV,
    d.NUM_FACTURA_PROV,
    d.IDE_RECIVE,
    d.IDE_DV_RECIVE,
    d.NOMBRE_RECIVE,
    d.IDE_CHOFER,
    d.IDE_DV_CHOFER,
    d.NOMBRE_CHOFER,
    d.TIPO_DESPACHO,
    d.EXPRESS,
    d.NUM_RESERVA_B2B,
    d.PRIMA_PENDIENTE,
    d.CANT_DESP,
    d.CANT_RET,
    d.CANT_TRF,
    d.FECHA_RECEP
    FROM reserva_dtl d, reserva_hdr h
    WHERE d.canal_venta = pcanal_venta
      AND d.canal_venta = h.canal_venta
      AND d.num_reserva = h.num_reserva
      AND d.num_reserva IN (SELECT h2.num_reserva FROM reserva_hdr h2
                            WHERE h2.numrsv_padre = pnum_reserva
                              OR (h2.num_ordenventa = pnum_ordenventa AND NVL(h2.numrsv_padre, 0) != 0))
      AND h.cod_estadocab NOT IN (1,2,3);


  END;
  OUT_STATUS_CODE:=0;
  OUT_STATUS_MSG:='OK';
  IF in_process_luw = 'T' THEN
       COMMIT ;
  END IF;
END CONSULTA_RESERVA;

PROCEDURE CARGA_FOLIOS(
  PID                 IN  GDD_RANGE.ID%TYPE,
  pFACILITY_ID        IN  GDD_RANGE.FACILITY_ID%TYPE,
  pCOMPANY_ID         IN  GDD_RANGE.COMPANY_ID%TYPE,
  pSTART_DOC_NUMBER   IN  GDD_RANGE.START_DOC_NUMBER%TYPE,
  pEND_DOC_NUMBER     IN  GDD_RANGE.END_DOC_NUMBER%TYPE,
  pCURRENT_DOC_NUMBER IN  GDD_RANGE.CURRENT_DOC_NUMBER%TYPE,
  pXML                IN  GDD_RANGE.XML%TYPE,
  pSTATUS_ID          IN  GDD_RANGE.STATUS_ID%TYPE,
  pMOD_USER           IN  GDD_RANGE.MOD_USER%TYPE,
  OUT_STATUS_CODE     OUT NUMBER,
  OUT_STATUS_MSG      OUT VARCHAR) AS
BEGIN
  BEGIN
    INSERT INTO GDD_RANGE(ID,
                          FACILITY_ID,
                          COMPANY_ID,
                          START_DOC_NUMBER,
                          END_DOC_NUMBER,
                          CURRENT_DOC_NUMBER,
                          XML,
                          STATUS_ID,
                          CREATE_TS,
                          MOD_TS,
                          MOD_USER)
    VALUES(PID,
           pFACILITY_ID,
           pCOMPANY_ID,
           pSTART_DOC_NUMBER,
           pEND_DOC_NUMBER,
           pCURRENT_DOC_NUMBER,
           pXML,
           9, -- rango de folio aprobado pero no utilizado
           sysdate(),
           null,
           pMOD_USER);
  END;
END CARGA_FOLIOS;

PROCEDURE ENTREGA_DOCUMENTO(
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
  OUT_STATUS_MSG          OUT VARCHAR) AS
BEGIN
  DECLARE
  hoja_ruta               NUMBER;
  c_hoja_ruta             NUMBER;
  reserva                 NUMBER;
  n_reserva               NUMBER;
  h_reserva               NUMBER;
  o_servicio              NUMBER;
  c_motivoc               NUMBER;
  cod_evento              NUMBER;
  var_error               NUMBER;
  cant_bultos             NUMBER;
  bults                   NUMBER;
  estado_dad              NUMBER;
  err_num                 NUMBER;
  cant_det                 NUMBER;
  det_12                  NUMBER;
  det_no_12               NUMBER;
  transportist            NUMBER;
  estado_courier          VARCHAR2(50);
  usuario                 VARCHAR2(50);
  err_msg                 VARCHAR2(255);
  msg_error               VARCHAR2(100);
  msg_error2              VARCHAR2(300);
  linea_msg               varchar2(255);
  default_error           VARCHAR2(7);
  estados_invalidos       EXCEPTION;
  no_bultos               EXCEPTION;
  fecha_entrega_c         DATE;
  BEGIN
    -- inicializar reservas
    var_error  := 0;
    msg_error  := '';
    msg_error2 := '';
    -- se guarda id_evento
    --SELECT seq_evento_courier.nextval INTO cod_evento FROM dual;

    -- Se valida que las variables vengan con los datos correspondientes
    IF nfolio_docto IS NOT NULL THEN
      reserva := to_number(nfolio_docto, '999999999999');
    ELSE
      var_error := 1;
      msg_error := 'sin Reserva,';
    END IF;
    IF nlatitud IS NOT NULL THEN
       hoja_ruta := nlatitud;
    ELSE
       var_error := 1;
       msg_error := msg_error || 'sin HojaRuta,';
    END IF;
    IF nlongitud IS NOT NULL THEN
       o_servicio := nlongitud;
    ELSE
       var_error := 1;
       msg_error := msg_error || 'sin OrdServ,';
    END IF;
    IF cnoentrega IS NOT NULL THEN
      c_motivoc := to_number(cnoentrega, '99');
    ELSE
      var_error := 1;
      msg_error := msg_error || 'sin CodNoEntrega,';
    END IF;
/*    IF in_cpatente IS NULL THEN
      var_error := 1;
      msg_error := msg_error || 'sin Patente,';
    END IF;*/
    IF devento IS NULL THEN
      var_error := 1;
      msg_error := msg_error || 'sin FechaEvento,';
    END IF;
    IF nrut_ent IS NULL THEN
      var_error := 1;
      msg_error := msg_error || 'sin RUT,';
    END IF;
    IF xdvrut_ent IS NULL THEN
      var_error := 1;
      msg_error := msg_error || 'sin DigitoVerif,';
    END IF;
/*    IF cparentesco IS NULL THEN
      var_error := 1;
      msg_error := msg_error || 'sin Parentesco,';
    END IF;*/
    IF nbultos IS NULL THEN
      var_error := 1;
      msg_error := msg_error || 'sin Bultos,';
    ELSE
      cant_bultos := to_number(nbultos,'99');
    END IF;
    -- se valida la cantidad de bultos
/*    IF cautoriza IS NULL THEN
      var_error := 1;
      msg_error := msg_error || 'sin Autoriza';
    END IF;*/

    IF (var_error = 1) THEN
      -- insertar datos en tabla evento_courier junto con registrar el error
      INSERT INTO evento_courier
      (id_evento,
       fecha_evento,
       num_reserva,
       cod_hojaruta,
       cod_transp,
       fecha_evento_courier,
       cod_no_entrega,
       tipo_documento,
       orden_servicio,
       ide,
       ide_dv,
       parentesco,
       bultos,
       autorizacion,
       patente_courier,
       estado_trans,
       obs_trans
      )
      SELECT seq_evento_courier.nextval,
             SYSDATE,
             decode(nfolio_docto,null,null,to_number(nfolio_docto, '999999999999')),
             decode(nlatitud,NULL,NULL,nlatitud),
             decode(nlatitud, NULL,NULL, (SELECT cod_transp FROM hojaruta WHERE cod_hojaruta = nlatitud)),
             decode(devento,NULL,NULL,devento),--solo se registra
             decode(cnoentrega,null,null,to_number(cnoentrega, '99')),
             decode(ctpdoc,NULL,NULL,ctpdoc),
             decode(nlongitud,NULL, NULL,nlongitud),
             decode(nrut_ent,null,null,to_number(nrut_ent,'999999999999')),
             decode(xdvrut_ent,null,null,trim(xdvrut_ent)),
             decode(cparentesco,NULL,NULL,cparentesco),
             decode(nbultos,NULL,NULL,nbultos),
             decode(cautoriza,NULL,NULL,cautoriza),
             in_cpatente,
             'ERROR',
             msg_error
      FROM   dual;

      -- respuesta a courier informando error
      OUT_STATUS_MSG:= RPAD('2', 2, ' ') || SUBSTR(RPAD(msg_error, 30, ' '), 1, 30);
      RETURN;
    END IF;

    -- se valida si la reserva realmente existe
    SELECT decode(COUNT(*),0,-1)
    INTO   n_reserva
    FROM   reserva_dtl rd,
           reserva_hdr rh
    WHERE  rd.canal_venta = rh.canal_venta
    AND    rd.num_reserva = rh.num_reserva
    AND    rd.num_reserva = reserva;
    IF n_reserva = -1 THEN
      var_error := 1;
      msg_error := 'Reserva ' || reserva || ' no existe, ';
    END IF;

    -- se valida si la hr existe
    SELECT decode(COUNT(*),0,-1)
    INTO   c_hoja_ruta
    FROM   hojaruta
    WHERE  cod_hojaruta = hoja_ruta;
    IF c_hoja_ruta = -1 THEN
      var_error := 1;
      msg_error := msg_error || 'HojaRuta ' || hoja_ruta || ' no existe, ';
    END IF;

    IF (var_error = 1) THEN
      -- insertar datos en tabla evento_courier junto con registrar el error
      INSERT INTO evento_courier
      (id_evento,
       fecha_evento,
       num_reserva,
       cod_hojaruta,
       cod_transp,
       fecha_evento_courier,
       cod_no_entrega,
       tipo_documento,
       orden_servicio,
       ide,
       ide_dv,
       parentesco,
       bultos,
       autorizacion,
       patente_courier,
       estado_trans,
       obs_trans
      )
      SELECT seq_evento_courier.nextval,
             SYSDATE,
             decode(nfolio_docto,null,null,to_number(nfolio_docto, '999999999999')),
             decode(nlatitud,NULL,NULL,nlatitud),
             decode(nlatitud, NULL,NULL, (SELECT cod_transp FROM hojaruta WHERE cod_hojaruta = nlatitud)),
             decode(devento,NULL,NULL,devento),--solo se registra
             decode(cnoentrega,null,null,to_number(cnoentrega, '99')),
             decode(ctpdoc,NULL,NULL,ctpdoc),
             decode(nlongitud,NULL, NULL,nlongitud),
             decode(nrut_ent,null,null,to_number(nrut_ent,'999999999999')),
             decode(xdvrut_ent,null,null,trim(xdvrut_ent)),
             decode(cparentesco,NULL,NULL,cparentesco),
             decode(nbultos,NULL,NULL,nbultos),
             decode(cautoriza,NULL,NULL,cautoriza),
             in_cpatente,
             'ERROR',
             msg_error
      FROM   dual;

      -- respuesta a courier informando error
      OUT_STATUS_MSG:= RPAD('2', 2, ' ') || SUBSTR(RPAD(msg_error, 30, ' '), 1, 30);
      RETURN;
    END IF;

    -- se valida si la reserva pertenece a la hoja de ruta informada
    SELECT decode(COUNT(*),0,-1)
    INTO   h_reserva
    FROM   reserva_dtl rd,
           reserva_hdr rh
    WHERE  rd.canal_venta   = rh.canal_venta
    AND    rd.num_reserva   = rh.num_reserva
    AND    rd.cod_hojaruta  = hoja_ruta
    AND    rd.num_reserva   = reserva;
    IF h_reserva = -1 THEN
      var_error := 1;
      msg_error := 'Reserva ' || reserva || ' no pertenece a la HojaRuta ' || hoja_ruta || ', ';
    END IF;

    -- Se valida si cod de no entrega realmente existe
    SELECT decode(COUNT(*),0,-1)
    INTO   c_motivoc
    FROM   motivo_courier mc
    WHERE  mc.cod_motivoc = cnoentrega;
    IF c_motivoc = -1 THEN
      var_error := 1;
      msg_error := msg_error || 'cnoentrega ' || cnoentrega || ' no existe, ';
    ELSE
      SELECT mc.cod_motivoc
      INTO   c_motivoc
      FROM   motivo_courier mc
      WHERE  mc.cod_motivoc = cnoentrega;
    END IF;

    IF var_error = 1 THEN
      -- insertar datos en tabla evento_courier junto con registrar el error
      INSERT INTO evento_courier
      (id_evento,
       fecha_evento,
       num_reserva,
       cod_hojaruta,
       cod_transp,
       fecha_evento_courier,
       cod_no_entrega,
       tipo_documento,
       orden_servicio,
       ide,
       ide_dv,
       parentesco,
       bultos,
       autorizacion,
       patente_courier,
       estado_trans,
       obs_trans
      )
      SELECT seq_evento_courier.nextval,
             SYSDATE,
             decode(nfolio_docto,null,null,to_number(nfolio_docto, '999999999999')),
             decode(nlatitud,NULL,NULL,nlatitud),
             decode(nlatitud, NULL,NULL, (SELECT cod_transp FROM hojaruta WHERE cod_hojaruta = nlatitud)),
             decode(devento,NULL,NULL,devento),--solo se registra
             decode(cnoentrega,null,null,to_number(cnoentrega, '99')),
             decode(ctpdoc,NULL,NULL,ctpdoc),
             decode(nlongitud,NULL, NULL,nlongitud),
             decode(nrut_ent,null,null,to_number(nrut_ent,'999999999999')),
             decode(xdvrut_ent,null,null,trim(xdvrut_ent)),
             decode(cparentesco,NULL,NULL,cparentesco),
             decode(nbultos,NULL,NULL,nbultos),
             decode(cautoriza,NULL,NULL,cautoriza),
             in_cpatente,
             'ERROR',
             msg_error
      FROM   dual;
      -- respuesta a courier informando error
      OUT_STATUS_MSG:= RPAD('2', 2, ' ') || SUBSTR(RPAD(msg_error, 30, ' '), 1, 30);
      RETURN;

    ELSE
      --INI posiblemente eliminar
      /*
      -- se valida estado de la reserva en dad
      SELECT COUNT(*) cant,
             SUM(DECODE(cod_estadodet, 12, 1, 0)) det1,
             SUM(DECODE(cod_estadodet, 12, 0, 1)) det2
      INTO   cant_det,
             det_12,
             det_no_12
      FROM   reserva_dtl rd,
             reserva_hdr rh
      WHERE  rd.canal_venta  = rh.canal_venta
      AND    rd.num_reserva  = rh.num_reserva
      AND    rd.num_reserva  = reserva
      AND    rd.cod_hojaruta = hoja_ruta;
      IF cant_det <> det_12 THEN
        RAISE estados_invalidos;
      END IF;
      */
      --FIN posiblemente eliminar

      -- calcular si se produce cambio de estado de acuerdo al motivo enviado por courier
      SELECT mc.cod_estadodet
      INTO   estado_dad
      FROM   motivo_courier mc
      WHERE  mc.cod_motivoc = c_motivoc;

      IF estado_dad = 12 THEN
        -- se valida estado de la reserva en dad
        SELECT COUNT(*) cant,
               SUM(DECODE(cod_estadodet, 12, 1, 0)) det1,
               SUM(DECODE(cod_estadodet, 12, 0, 1)) det2
        INTO   cant_det,
               det_12,
               det_no_12
        FROM   reserva_dtl rd,
               reserva_hdr rh
        WHERE  rd.canal_venta  = rh.canal_venta
        AND    rd.num_reserva  = rh.num_reserva
        AND    rd.num_reserva  = reserva
        AND    rd.cod_hojaruta = hoja_ruta;
        IF cant_det <> det_12 THEN
           --Identificar que hay cambios de estados manuales
          RAISE estados_invalidos;
        END IF;

        -- se calcula el cod_transportista y usuario
        SELECT cod_transp
        INTO   transportist
        FROM   hojaruta
        WHERE  cod_hojaruta = hoja_ruta;

        SELECT SUBSTR('COURIER:' || lower(trim(tr.empresa)), 1, 30)
        INTO   usuario
        FROM   transportista tr
        WHERE  tr.cod_transp = transportist;

        SELECT mc.desc_motivoc
        INTO   estado_courier
        FROM   motivo_courier mc
        WHERE  mc.cod_motivoc = c_motivoc;

        -- se actualiza evento_courier
        INSERT INTO evento_courier
        (id_evento,
         fecha_evento,
         num_reserva,
         cod_hojaruta,
         cod_transp,
         fecha_evento_courier,
         cod_no_entrega,
         tipo_documento,
         orden_servicio,
         ide,
         ide_dv,
         parentesco,
         bultos,
         autorizacion,
         patente_courier,
         estado_trans,
         obs_trans
        )
        SELECT seq_evento_courier.nextval,
               SYSDATE,
               decode(nfolio_docto,null,null,to_number(nfolio_docto, '999999999999')),
               decode(nlatitud,NULL,NULL,hoja_ruta),
               decode(nlatitud, NULL,NULL, transportist),
               decode(devento,NULL,NULL,devento),--solo se registra
               decode(cnoentrega,null,null,to_number(c_motivoc, '99')),
               decode(ctpdoc,NULL,NULL,ctpdoc),
               decode(nlongitud,NULL, NULL,o_servicio),
               decode(nrut_ent,null,null,to_number(nrut_ent,'999999999999')),
               decode(xdvrut_ent,null,null,trim(xdvrut_ent)),
               decode(cparentesco,NULL,NULL,cparentesco),
               decode(nbultos,NULL,NULL,cant_bultos),
               decode(cautoriza,NULL,NULL,cautoriza),
               in_cpatente,
               'OK',
               'PROCEDA'
        FROM   dual;

        -- se actualiza os_courier, estado_courier, usuario en reserva_dtl
        UPDATE reserva_dtl rd1
        SET    rd1.courier_os     = o_servicio,
               rd1.courier_estado = TRIM(estado_courier),
               rd1.usuario_evento = TRIM(usuario)
        WHERE  rd1.num_reserva  = reserva
          AND  rd1.cod_hojaruta = hoja_ruta;

        -- se actualiza evento dad
        INSERT INTO evento
        (id_evento,
         cod_tipoevento,
         canal_venta,
         num_reserva,
         num_detalle,
         fecha_evento,
         cod_estado,
         login_usuario,
         courier_os,
         courier_estado
        )
        SELECT seq_eventos.nextval,
               2,
               rd.canal_venta,
               rd.num_reserva,
               rd.num_detalle,
               SYSDATE,
               rd.cod_estadodet,
               usuario,
               o_servicio,
               estado_courier
        FROM   reserva_dtl rd
        WHERE  rd.num_reserva  = reserva
        AND    rd.cod_hojaruta = hoja_ruta
        ;

       -- Se responde a courier
       OUT_STATUS_MSG:= RPAD('1', 2, ' ') || SUBSTR(RPAD('PROCEDA', 30, ' '), 1, 30);
       RETURN;

     ELSIF estado_dad = 15 THEN
        -- se valida estado de la reserva en dad
        SELECT COUNT(*) cant,
               SUM(DECODE(cod_estadodet, 12, 1, 0)) det1,
               SUM(DECODE(cod_estadodet, 12, 0, 1)) det2
        INTO   cant_det,
               det_12,
               det_no_12
        FROM   reserva_dtl rd,
               reserva_hdr rh
        WHERE  rd.canal_venta  = rh.canal_venta
        AND    rd.num_reserva  = rh.num_reserva
        AND    rd.num_reserva  = reserva
        AND    rd.cod_hojaruta = hoja_ruta;
        IF cant_det <> det_12 THEN
          RAISE estados_invalidos;
        END IF;

        -- se calcula el cod_transportista y usuario
        SELECT cod_transp
        INTO   transportist
        FROM   hojaruta
        WHERE  cod_hojaruta = hoja_ruta;

        SELECT SUBSTR('COURIER:' || lower(trim(tr.empresa)), 1, 30)
        INTO   usuario
        FROM   transportista tr
        WHERE  tr.cod_transp = transportist;

        SELECT mc.desc_motivoc
        INTO   estado_courier
        FROM   motivo_courier mc
        WHERE  mc.cod_motivoc = c_motivoc;

        -- se actualiza evento_courier
        INSERT INTO evento_courier
        (id_evento,
         fecha_evento,
         num_reserva,
         cod_hojaruta,
         cod_transp,
         fecha_evento_courier,
         cod_no_entrega,
         tipo_documento,
         orden_servicio,
         ide,
         ide_dv,
         parentesco,
         bultos,
         autorizacion,
         patente_courier,
         estado_trans,
         obs_trans
        )
        SELECT seq_evento_courier.nextval,
               SYSDATE,
               decode(nfolio_docto,null,null,to_number(nfolio_docto, '999999999999')),
               decode(nlatitud,NULL,NULL,hoja_ruta),
               decode(nlatitud, NULL,NULL, transportist),
               decode(devento,NULL,NULL,devento),--solo se registra
               decode(cnoentrega,null,null,to_number(c_motivoc, '99')),
               decode(ctpdoc,NULL,NULL,ctpdoc),
               decode(nlongitud,NULL, NULL,o_servicio),
               decode(nrut_ent,null,null,to_number(nrut_ent,'999999999999')),
               decode(xdvrut_ent,null,null,trim(xdvrut_ent)),
               decode(cparentesco,NULL,NULL,cparentesco),
               decode(nbultos,NULL,NULL,cant_bultos),
               decode(cautoriza,NULL,NULL,cautoriza),
               in_cpatente,
               'OK',
               'PROCEDA'
        FROM   dual;

        -- se actualiza os_courier, estado_courier, usuario en reserva_dtl
        UPDATE reserva_dtl rd1
        SET    rd1.courier_os     = o_servicio,
               rd1.courier_estado = trim(estado_courier),
               rd1.usuario_evento = trim(usuario),
               rd1.cod_estadodet  = 15
        WHERE  rd1.num_reserva  = reserva
          AND  rd1.cod_hojaruta = hoja_ruta;

       -- Se responde a courier
       OUT_STATUS_MSG:= RPAD('1', 2, ' ') || SUBSTR(RPAD('PROCEDA', 30, ' '), 1, 30);
       RETURN;

      ELSIF estado_dad = 14 THEN
        -- se valida estado de la reserva en dad
        SELECT COUNT(*) cant,
               SUM(DECODE(cod_estadodet, 12, 1, 14, 1, 0)) det1,
               SUM(DECODE(cod_estadodet, 12, 0, 14, 0, 1)) det2
        INTO   cant_det,
               det_12,   --y 14
               det_no_12 --y 14
        FROM   reserva_dtl rd,
               reserva_hdr rh
        WHERE  rd.canal_venta  = rh.canal_venta
        AND    rd.num_reserva  = rh.num_reserva
        AND    rd.num_reserva  = reserva
        AND    rd.cod_hojaruta = hoja_ruta;
        IF cant_det <> det_12 THEN
          RAISE estados_invalidos;
        END IF;

        -- se calcula el cod_transportista y usuario
        SELECT cod_transp
        INTO   transportist
        FROM   hojaruta
        WHERE  cod_hojaruta = hoja_ruta;

        SELECT SUBSTR('COURIER:' || lower(trim(tr.empresa)), 1, 30)
        INTO   usuario
        FROM   transportista tr
        WHERE  tr.cod_transp = transportist;

        SELECT mc.desc_motivoc
        INTO   estado_courier
        FROM   motivo_courier mc
        WHERE  mc.cod_motivoc = c_motivoc;

        -- se actualiza evento_courier
        INSERT INTO evento_courier
        (id_evento,
         fecha_evento,
         num_reserva,
         cod_hojaruta,
         cod_transp,
         fecha_evento_courier,
         cod_no_entrega,
         tipo_documento,
         orden_servicio,
         ide,
         ide_dv,
         parentesco,
         bultos,
         autorizacion,
         patente_courier,
         estado_trans,
         obs_trans
        )
        SELECT seq_evento_courier.nextval,
               SYSDATE,
               decode(nfolio_docto,null,null,to_number(nfolio_docto, '999999999999')),
               decode(nlatitud,NULL,NULL,hoja_ruta),
               decode(nlatitud, NULL,NULL, transportist),
               decode(devento,NULL,NULL,devento),--solo se registra
               decode(cnoentrega,null,null,to_number(c_motivoc, '99')),
               decode(ctpdoc,NULL,NULL,ctpdoc),
               decode(nlongitud,NULL, NULL,o_servicio),
               decode(nrut_ent,null,null,to_number(nrut_ent,'999999999999')),
               decode(xdvrut_ent,null,null,trim(xdvrut_ent)),
               decode(cparentesco,NULL,NULL,cparentesco),
               decode(nbultos,NULL,NULL,cant_bultos),
               decode(cautoriza,NULL,NULL,cautoriza),
               in_cpatente,
               'OK',
               'ENTREGAR'
        FROM   dual;

         BEGIN
            IF devento IS NOT NULL THEN
              --Para saber en que formato viene dd-mm-yyyy o yyyy-mm-dd
              SELECT to_date(substr(trim(decode(devento,NULL,NULL,devento)),1,10), 'YYYY-MM-DD')
              INTO fecha_entrega_c
              FROM dual;

            END IF;
          EXCEPTION
          WHEN OTHERS THEN
            SELECT to_date(substr(trim(decode(devento,NULL,NULL,devento)),1,10), 'DD-MM-YYYY')
            INTO fecha_entrega_c
            FROM dual;
          END;

        -- se actualiza os_courier, estado_courier, usuario en reserva_dtl
        UPDATE reserva_dtl rd1
        SET    rd1.courier_os     = o_servicio,
               rd1.courier_estado = trim(estado_courier),
               rd1.usuario_evento = trim(usuario),
               rd1.cod_estadodet  = CASE
                                      WHEN rd1.num_detalle > 0 THEN 14
                                      WHEN rd1.num_detalle < 0 THEN 21
                                      ELSE rd1.cod_estadodet
                                    END,
               rd1.fecha_entrega_cli = CASE
                                         WHEN rd1.num_detalle > 0 THEN fecha_entrega_c
                                         ELSE rd1.fecha_entrega_cli
                                      END
        WHERE  rd1.num_reserva   = reserva
          AND  rd1.cod_hojaruta  = hoja_ruta
          AND  rd1.cod_estadodet = 12;
        -- evento se actualiza en trg_reservadtl

        --Se deja procesada la HR incluyendo una posible hoja de ruta activa
        UPDATE preruteo_reservadet
        SET    informado = 1
        WHERE  num_reserva          = reserva
          AND  confirmado           = 1
          AND  NVL(informado, 0)    = 0
          AND  NVL(cod_hojaruta,0) <> 0
          AND  ((num_reserva, num_detalle) IN (SELECT num_reserva, num_detalle
                                               FROM   reserva_dtl
                                               WHERE  num_reserva =  reserva
                                                 AND  cod_estadodet IN (14))); --sacar AQUI
        --Cerrar guia de despacho DAD.
        UPDATE gdd_hdr
        SET    estado_gd = 0
        WHERE  num_gd IN ( SELECT gdd_dtl.num_gd
                           FROM   gdd_dtl,
                                  gdd_hdr gh,
                                  reserva_dtl rd
                           WHERE  gdd_dtl.num_gd      = gh.num_gd
                           AND    gdd_dtl.canal_venta = rd.canal_venta
                           AND    gdd_dtl.num_reserva = rd.num_reserva
                           AND    gdd_dtl.num_detalle = rd.num_detalle
                           AND    rd.num_reserva      = reserva
                           AND    rd.cod_hojaruta     = hoja_ruta
                           AND    gh.estado_gd        = 1);
      END IF;
    END IF;
    -- se responde mensaje de cambio de estado  ok
    OUT_STATUS_MSG:= RPAD('1', 2, ' ') || SUBSTR(RPAD('ENTREGAR', 30, ' '), 1, 30);

  -- Captura de errores y excepciones personalizadas
  EXCEPTION

    WHEN estados_invalidos THEN
      -- insertar tabla evento_courier y registrar error responder a courier
      msg_error2 := 'Error: Al menos un estado de los detalles de la reserva ha cambiado';
--      msg_error2 := 'Detalles de reserva ha cambiado';
      INSERT INTO evento_courier
      (id_evento,
       fecha_evento,
       num_reserva,
       cod_hojaruta,
       cod_transp,
       fecha_evento_courier,
       cod_no_entrega,
       tipo_documento,
       orden_servicio,
       ide,
       ide_dv,
       parentesco,
       bultos,
       autorizacion,
       patente_courier,
       estado_trans,
       obs_trans
      )
      SELECT seq_evento_courier.nextval,
             SYSDATE,
             decode(nfolio_docto,null,null,to_number(nfolio_docto, '999999999999')),
             decode(nlatitud,NULL,NULL,nlatitud),
             decode(nlatitud, NULL,NULL, (SELECT cod_transp FROM hojaruta WHERE cod_hojaruta = nlatitud)),
             decode(devento,NULL,NULL,devento),--solo se registra
             decode(cnoentrega,null,null,to_number(cnoentrega, '99')),
             decode(ctpdoc,NULL,NULL,ctpdoc),
             decode(nlongitud,NULL, NULL,nlongitud),
             decode(nrut_ent,null,null,to_number(nrut_ent,'999999999999')),
             decode(xdvrut_ent,null,null,trim(xdvrut_ent)),
             decode(cparentesco,NULL,NULL,cparentesco),
             decode(nbultos,NULL,NULL,nbultos),
             decode(cautoriza,NULL,NULL,cautoriza),
             in_cpatente,
             'ERROR',
             msg_error2
      FROM   dual;
      -- respuesta a courier informando error
      OUT_STATUS_MSG:= RPAD('2', 2, ' ') || SUBSTR(RPAD(msg_error2, 30, ' '), 1, 30);

    WHEN no_bultos THEN
      -- insertar tabla evento_courier y registrar error responder a courier
      msg_error2 := 'Error: Bultos informados no coinciden con los registrados';
--      msg_error2 := 'Bultos no coinciden';
      INSERT INTO evento_courier
      (id_evento,
       fecha_evento,
       num_reserva,
       cod_hojaruta,
       cod_transp,
       fecha_evento_courier,
       cod_no_entrega,
       tipo_documento,
       orden_servicio,
       ide,
       ide_dv,
       parentesco,
       bultos,
       autorizacion,
       patente_courier,
       estado_trans,
       obs_trans
      )
      SELECT seq_evento_courier.nextval,
             SYSDATE,
             decode(nfolio_docto,null,null,to_number(nfolio_docto, '999999999999')),
             decode(nlatitud,NULL,NULL,nlatitud),
             decode(nlatitud, NULL,NULL, (SELECT cod_transp FROM hojaruta WHERE cod_hojaruta = nlatitud)),
             decode(devento,NULL,NULL,devento),--solo se registra
             decode(cnoentrega,null,null,to_number(cnoentrega, '99')),
             decode(ctpdoc,NULL,NULL,ctpdoc),
             decode(nlongitud,NULL, NULL,nlongitud),
             decode(nrut_ent,null,null,to_number(nrut_ent,'999999999999')),
             decode(xdvrut_ent,null,null,trim(xdvrut_ent)),
             decode(cparentesco,NULL,NULL,cparentesco),
             decode(nbultos,NULL,NULL,nbultos),
             decode(cautoriza,NULL,NULL,cautoriza),
             in_cpatente,
             'ERROR',
             msg_error2
      FROM   dual;
      -- respuesta a courier informando error
      OUT_STATUS_MSG:= RPAD('2', 2, ' ') || SUBSTR(RPAD(msg_error2, 30, ' '), 1, 30);

    WHEN OTHERS THEN
      -- capturar errores sql
      err_num   := SQLCODE;
      err_msg   := SQLERRM;
      linea_msg := dbms_utility.format_error_backtrace;
      -- insertar tabla evento_courier y registrar error responder a courier
      msg_error2 := 'Error: ' || to_char(err_num) || ', ' || err_msg || ', ' || linea_msg;
      INSERT INTO evento_courier
      (id_evento,
       fecha_evento,
       num_reserva,
       cod_hojaruta,
       cod_transp,
       fecha_evento_courier,
       cod_no_entrega,
       tipo_documento,
       orden_servicio,
       ide,
       ide_dv,
       parentesco,
       bultos,
       autorizacion,
       patente_courier,
       estado_trans,
       obs_trans
      )
      SELECT seq_evento_courier.nextval,
             SYSDATE,
             decode(nfolio_docto,null,null,to_number(nfolio_docto, '999999999999')),
             decode(nlatitud,NULL,NULL,nlatitud),
             decode(nlatitud, NULL,NULL, (SELECT cod_transp FROM hojaruta WHERE cod_hojaruta = nlatitud)),
             decode(devento,NULL,NULL,devento),--solo se registra
             decode(cnoentrega,null,null,to_number(cnoentrega, '99')),
             decode(ctpdoc,NULL,NULL,ctpdoc),
             decode(nlongitud,NULL, NULL,nlongitud),
             decode(nrut_ent,null,null,to_number(nrut_ent,'999999999999')),
             decode(xdvrut_ent,null,null,trim(xdvrut_ent)),
             decode(cparentesco,NULL,NULL,cparentesco),
             decode(nbultos,NULL,NULL,nbultos),
             decode(cautoriza,NULL,NULL,cautoriza),
             in_cpatente,
             'ERROR',
             msg_error2
      FROM   dual;
      -- respuesta a courier informando error
      OUT_STATUS_MSG:= RPAD('2', 2, ' ') || SUBSTR(RPAD(msg_error2, 30, ' '), 1, 30);
  END;
END ENTREGA_DOCUMENTO;

END DADIMPORT;


/
