--------------------------------------------------------
--  File created - Thursday-June-21-2018   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Package Body CALCULO_FECHA
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE BODY "DADCORPDS"."CALCULO_FECHA" AS

     FUNCTION restringo_calendario
  (
    "dias_cc"             IN  VARCHAR2,
    "dias_transeferencia" IN  VARCHAR2
  ) RETURN  VARCHAR2  AS
  BEGIN
  DECLARE
  "nuevo_calendario"     VARCHAR2(7):=NULL;
  "sigla_dia"    CHAR(1);


   BEGIN

   FOR i IN REVERSE 1..LENGTH("dias_cc") LOOP

    "sigla_dia":=SUBSTR("dias_cc",i,1);
   -- DBMS_OUTPUT.PUT_LINE ('"sigla_dia": '|| "sigla_dia");
     IF  INSTR("dias_transeferencia","sigla_dia") > 0 THEN
      "nuevo_calendario" :="nuevo_calendario" || "sigla_dia";

     END IF;
  END LOOP;

   -- DBMS_OUTPUT.PUT_LINE ('"nuevo_calendario": '|| "nuevo_calendario");
        RETURN  "nuevo_calendario"; -- NO SE TRABAJA


       -- RETURN  "dias_transeferencia";
    END;
  END restringo_calendario;

  PROCEDURE fecha_calcular
  (
  In_accion           IN  NUMBER,
  in_process_luw      IN  VARCHAR2 DEFAULT 'T',
  in_xml              IN  XMLTYPE,
  "out_cursor_fecha"  out sys_refcursor,
  "out_statuscode"    OUT NUMBER,
  "out_statusmsg"     OUT VARCHAR2
  ) AS
  BEGIN
       DECLARE

    --VARIABLES PARA EL MANEJO DEL XML
    doc             dbms_xmldom.DOMDocument;
    node            dbms_xmldom.DOMNode;
    hijo            dbms_xmldom.DOMNode;
    nodeList        dbms_xmldom.DOMNodeList;
    hijosList       dbms_xmldom.DOMNodeList;
    numNodes        NUMBER;
    numHijos        NUMBER;
    rowCounter      NUMBER;
    nodoActual      dbms_xmldom.DOMNode;

    hijoCobertura       dbms_xmldom.DOMNode;
    hijosCoberturaList  dbms_xmldom.DOMNodeList;
    nodoActualCobertura dbms_xmldom.DOMNode;
    numCoberturaNodes   dbms_xmldom.DOMNode;
    numCoberturaHijos   NUMBER;
    rowCoberturaCounter NUMBER;

    hijoProducto        dbms_xmldom.DOMNode;
    hijosProductosList  dbms_xmldom.DOMNodeList;
    nodoActualProducto  dbms_xmldom.DOMNode;
    numProductoNodes    dbms_xmldom.DOMNode;
    numProductosHijos   NUMBER;
    rowProductoCounter  NUMBER;

     --VARIABLES DE LA CABECERA DE LA RESERVA

    canalVenta      NUMBER;
    region          VARCHAR2(4000);
    ciudad          VARCHAR2(4000);
    comunaDespacho  VARCHAR2(4000);
    fechaVenta      DATE;
    fechaEntrega    DATE;
     --VARIABLES DE LA COBERTURA

    cenCosOrigen    NUMBER;
    cenCosDespacho  NUMBER;
    cenCosVia       NUMBER;
    tipoStock       VARCHAR2(4000);
    codProveedor    CHAR(10);
     --VARIABLES DE LOS SKU
    codProducto     VARCHAR2(4000);
    tipoProducto    VARCHAR2(4000);
    diasFabricacion NUMBER;
    rango           VARCHAR2(4000);
    tipoServicio    VARCHAR2(4000);
    item            NUMBER;
    var_id_session        NUMBER;
    -- VARIABLES AXILIARES PARA EL CALCULO
    aux                VARCHAR2(4000);

    tiempo_proceso      NUMBER;
    tiempo_viaje        NUMBER;
    tiempo_despacho     NUMBER;
    dias_trabajo        VARCHAR2(7);
    dias_despacho       VARCHAR2(7);
    dias_despacho_zona  VARCHAR2(7);
    saldo               NUMBER;
    cantidad            NUMBER;


    tiempo_proceso_proveedor      NUMBER;
    dias_trabajo_proveedor        VARCHAR2(7);
    dias_despacho_proveedor       VARCHAR2(7);
    capacidad_proveedor_proveedor NUMBER;
    tiempo_viaje_proveedor        NUMBER;
    tiempo_proceso_cc_origen      NUMBER;
    tiempo_proceso_cc_despacha    NUMBER;
    dias_despacho_cc_despacha     VARCHAR2(7);
    dias_trabajo_cc_despacha      VARCHAR2(7);
    dias_trabajo_cc_orgigen       VARCHAR2(7);
    dias_despacho_cc_origen       VARCHAR2(7);
    tiempo_viaje_origen_destino   VARCHAR2(7);


    tiempo_despacho_proveedor     NUMBER;


    tiempo_despacho_proveedor_com     NUMBER;


    capacidad_proveedor           NUMBER;
    capacidad_reserva_cc          NUMBER;
    capacidad_l_cc                NUMBER;
    capacidad_m_cc                NUMBER;
    capacidad_w_cc                NUMBER;
    capacidad_j_cc                NUMBER;
    capacidad_v_cc                NUMBER;
    capacidad_s_cc                NUMBER;
    capacidad_d_cc                NUMBER;
    capacidad_reserva_zona        NUMBER;
    capacidad_l_zona              NUMBER;
    capacidad_m_zona              NUMBER;
    capacidad_w_zona              NUMBER;
    capacidad_j_zona              NUMBER;
    capacidad_v_zona              NUMBER;
    capacidad_s_zona              NUMBER;
    capacidad_d_zona              NUMBER;
    capacidad_reserva_rango       NUMBER;
    capacidad_l_rango             NUMBER;
    capacidad_m_rango             NUMBER;
    capacidad_w_rango             NUMBER;
    capacidad_j_rango             NUMBER;
    capacidad_v_rango             NUMBER;
    capacidad_s_rango             NUMBER;
    capacidad_d_rango             NUMBER;
    capacidad_reserva_zona_rango  NUMBER;
    capacidad_l_zona_rango        NUMBER;
    capacidad_m_zona_rango        NUMBER;
    capacidad_w_zona_rango        NUMBER;
    capacidad_j_zona_rango        NUMBER;
    capacidad_v_zona_rango        NUMBER;
    capacidad_s_zona_rango        NUMBER;
    capacidad_d_zona_rango        NUMBER;
   capacidad_reserva_cc_origen NUMBER;
    dias_calendario VARCHAR2(7);
    hora_corte      VARCHAR2(5);

    var_fecha_inicio_proceso  DATE;
    var_fecha_inicio_viaje    DATE;
    var_fecha_fin_proceso     DATE;

    var_fecha_traslado        DATE;
    var_fecha_despacho        DATE;
    var_fecha_entrega         DATE;
    var_fecha_viaje_real      DATE;
    var_hoy                   DATE:=sysdate;
    var_hoy_corte             DATE:=sysdate;
    retiro_en_tienda          NUMBER;
    capacidad_canal           NUMBER;
    var_accion                NUMBER;
    var_id                    NUMBER;
    var_glosa                 VARCHAR2(4000);
    dias_despacho_transferencia   VARCHAR2(7);
    dias_viaje_transferencia      VARCHAR2(7);
    dias_proceso_transferencia    VARCHAR2(7);
    dias_courier              VARCHAR2(7);
    dias_transferencia_cc     VARCHAR2(7);
    es_courier_org_igual_desp NUMBER;
    courier_mayor_a           NUMBER;
    cc_origen_es_bodega       VARCHAR2(1);
    cc_despacha_es_bodega     VARCHAR2(1);
    tipo_cc_despacha          VARCHAR2(10);
    var_tienda_venta          NUMBER;
    var_status_cobertura          NUMBER:=0;
    CDSTGO           NUMBER;
    BEGIN
    "out_statuscode":=0;
    "out_statusmsg":='OK';

    --OBTENGO LA CONFIGURACION DEL CALENDARIO
    SELECT valor_str INTO dias_calendario  FROM parametros WHERE cod_param=62;

    --OBTENGO LOS DIAS QUE DESPACHA UN CURIER ESTA CONDICION SE UTILIZA CUANDO LOS DIAS DE VIAJE SON MAYOR A 1
    SELECT valor_str INTO dias_courier  FROM parametros WHERE cod_param=153;

    --OBTENGO EL CD DE SANTIAGO
    SELECT valor_num       INTO CDSTGO  FROM parametros   WHERE cod_param = 1;

    --OBTENGO LOS DIAS CUANDO SE PUEDE REALIZAR UNA TRANSFERENCIA
    SELECT valor_num INTO es_courier_org_igual_desp  FROM parametros WHERE cod_param=154;

    --OBTENGO LOS DESDE CUANTOS DIAS PUEDE DESPACHART EL COURIER
    SELECT valor_num INTO courier_mayor_a  FROM parametros WHERE cod_param=155;

    --OBTENGO LOS DIAS CUANDO SE PUEDE DESPACHAR UNA TRANSFERENCIA
    SELECT valor_str INTO dias_despacho_transferencia  FROM parametros WHERE cod_param=158;

      --OBTENGO LOS DIAS CUANDO SE PUEDE VIAJAR EN UNA TRANSFERENCIA
    SELECT valor_str INTO dias_viaje_transferencia  FROM parametros WHERE cod_param=159;

      --OBTENGO LOS DIAS CUANDO SE PUEDE TRABAJAR EN UNA TRANSFERENCIA
    SELECT valor_str INTO dias_proceso_transferencia  FROM parametros WHERE cod_param=160;


    SELECT seq_motor_fecha.nextval INTO var_id_session FROM DUAL;

    doc            := dbms_xmldom.newDomDocument(in_xml);
    node           := dbms_xmldom.makeNode(doc);
    nodeList       := xslprocessor.selectNodes(node, '/reserva');
    numNodes       := dbms_xmldom.getLength(nodeList);

   FOR rowCounter IN 0..numNodes-1
    LOOP

      nodoActual      := dbms_xmldom.item(nodeList, rowCounter);

      hijo            := xslprocessor.selectSingleNode(nodoActual,'canalVenta');
      hijo            := xmldom.getFirstChild(hijo);
      canalVenta      := dbms_xmldom.getnodevalue(hijo);

      hijo            := xslprocessor.selectSingleNode(nodoActual,'region');
      hijo            := xmldom.getFirstChild(hijo);
      region          := dbms_xmldom.getnodevalue(hijo);

      hijo            := xslprocessor.selectSingleNode(nodoActual,'ciudad');
      hijo            := xmldom.getFirstChild(hijo);
      ciudad          := dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'comuna');
      hijo            := xmldom.getFirstChild(hijo);
      comunaDespacho  := dbms_xmldom.getnodevalue(hijo);


      hijo            := xslprocessor.selectSingleNode(nodoActual,'fechaVenta');
      hijo            := xmldom.getFirstChild(hijo);
      aux             := dbms_xmldom.getnodevalue(hijo);
      IF aux IS NULL OR aux = '' THEN
          fechaVenta := to_Date(aux,'DD/MM/YYYY');
      END IF;



      hijo              := xslprocessor.selectSingleNode(nodoActual,'esRT');
      hijo              := xmldom.getFirstChild(hijo);
      retiro_en_tienda  := dbms_xmldom.getnodevalue(hijo);


      hijo              := xslprocessor.selectSingleNode(nodoActual,'tiendaVenta');
      hijo              := xmldom.getFirstChild(hijo);
      var_tienda_venta  := dbms_xmldom.getnodevalue(hijo);

      /* IF retiro_en_tienda = 1 THEN
          retiro_en_tienda:=0;
       ELSE
          retiro_en_tienda:=1;
       END IF;*/

      hijosCoberturaList := xslprocessor.selectNodes(nodoActual, 'cobertura');
      numCoberturaHijos  := dbms_xmldom.getLength(hijosCoberturaList);





      FOR rowCoberturaCounter IN 0..numCoberturaHijos-1
      LOOP

        var_id                :=  0;
        var_glosa             :=  'OK' ;

        nodoActualCobertura   := dbms_xmldom.item(hijosCoberturaList, rowCoberturaCounter);

        hijoCobertura         := xslprocessor.selectSingleNode(nodoActualCobertura,'tipoStock');
        hijoCobertura         := xmldom.getFirstChild(hijoCobertura);
        tipoStock             := dbms_xmldom.getnodevalue(hijoCobertura);

        hijoCobertura         := xslprocessor.selectSingleNode(nodoActualCobertura,'cenCosOrigen');
        hijoCobertura         := xmldom.getFirstChild(hijoCobertura);
        cenCosOrigen          := dbms_xmldom.getnodevalue(hijoCobertura);

        hijoCobertura         := xslprocessor.selectSingleNode(nodoActualCobertura,'cenCosVia');
        hijoCobertura         := xmldom.getFirstChild(hijoCobertura);
        cenCosVia             := dbms_xmldom.getnodevalue(hijoCobertura);

        hijoCobertura         := xslprocessor.selectSingleNode(nodoActualCobertura,'cenCosDespacho');
        hijoCobertura         := xmldom.getFirstChild(hijoCobertura);
        cenCosDespacho        := dbms_xmldom.getnodevalue(hijoCobertura);

        hijoCobertura         := xslprocessor.selectSingleNode(nodoActualCobertura,'codProveedor');
        hijoCobertura         := xmldom.getFirstChild(hijoCobertura);
        codProveedor          := dbms_xmldom.getnodevalue(hijoCobertura);

        hijosProductosList    := xslprocessor.selectNodes(nodoActualCobertura, 'producto');
        numProductosHijos     := dbms_xmldom.getLength(hijosProductosList);




       IF tipoStock = 'S' AND    (NVL(cenCosOrigen,0) = 0 and NVL(cenCosDespacho,0) = 0) THEN
                 var_id:=1;
                var_status_cobertura:=1;
                var_glosa:='DAD: Sin Cobertura ';
                goto end_loop_cobertura;
      END IF;
       -- SI ES PROVEEDOR OBTENGO LOS DATOS DE DESPACHO ASOCIADOS AL PROVEEDOR
       IF tipoStock = 'P'  THEN
          BEGIN
            SELECT
                tpo_procesoprod,
                dias_trabajo,
                dias_despacho,
                NVL(cant_desp,0),
                NVL(tiempo_desp,0)
            INTO
                tiempo_proceso_proveedor,
                dias_trabajo_proveedor,
                dias_despacho_proveedor,
                capacidad_proveedor_proveedor,
                tiempo_viaje_proveedor
            FROM proveedor  LEFT JOIN prov_cc ON proveedor.cod_prov=prov_cc.cod_prov
                                                 AND ccosto=cenCosOrigen -- AND NVL(cenCosDespacho,0)=0
            WHERE proveedor.cod_prov=codProveedor;

            EXCEPTION
             WHEN NO_DATA_FOUND THEN
                var_id:=1;
                var_status_cobertura:=1;
                var_glosa:='DAD: Proveedor no Existe ' || codProveedor;
                goto end_loop_cobertura;
             END;

            IF capacidad_proveedor_proveedor = 0 THEN
                var_id:=1;
                var_status_cobertura:=1;
                var_glosa:='DAD: Proveedor ' || codProveedor || ' tiene capacidad 0';
                goto end_loop_cobertura;
            END IF;

                 -- VALIDO SI NO ESTA CORRECTAMENTE CONFIGURADO EL PROVEEDOR
            IF dias_despacho_proveedor IS NULL OR dias_trabajo_proveedor IS NULL THEN
                var_id:=1;
                var_status_cobertura:=1;
                var_glosa:='DAD: El Proveedor ' || codProveedor || ' No tiene Configurado los dias de Despacho o Trabajo';
                goto end_loop_cobertura;
            END IF;


            IF NVL(cenCosDespacho,0) = 0 THEN

             BEGIN
                  SELECT
                    tiempo_desp
                  INTO
                    tiempo_despacho_proveedor_com
                  FROM prov_comuna
                  WHERE comuna=comunaDespacho
                    AND cod_prov=codProveedor;
              EXCEPTION
             WHEN NO_DATA_FOUND THEN
                 var_id:=1;
                 var_status_cobertura:=1;
                var_glosa:='DAD: el Proveedor  ' || codProveedor || ' No despacha a la comuna ' || comunaDespacho;
                goto end_loop_cobertura;
             END;

            END IF;

         END IF;



        -- OBTENGO TIEMPO DE PROCESO Y DIAS DE TRABAJO DEL ORIGEN
            IF NVL(cenCosOrigen,0) > 0 THEN

            BEGIN
              SELECT
                  tpo_procesoprod,
                  DECODE(retiro_en_tienda,  1,  dias_trabajo_rt,  dias_trabajo), -- si es RT seteo los valores de RT
                  DECODE(retiro_en_tienda,  1,  dias_rt,          dias_despacho),
                  DECODE(retiro_en_tienda,  1,  hora_corte_rt,    hora_corte),
                  org_is_store,
                  cant_desp
              INTO
                  tiempo_proceso_cc_origen,
                  dias_trabajo_cc_orgigen,
                  dias_despacho_cc_origen,
                  hora_corte,
                  cc_origen_es_bodega,
                  capacidad_reserva_cc_origen
              FROM centro_costo
              WHERE org_lvl_number=cenCosOrigen;

              IF dias_trabajo_cc_orgigen IS NULL OR dias_despacho_cc_origen IS NULL  THEN
                var_id:=1;
                var_status_cobertura:=1;
                var_glosa:='DAD: Centro de Costo Origen ' || cenCosOrigen || ' No tiene Configurado los dias de Trabajo o Despacho';
                goto end_loop_cobertura;


            END IF;
            EXCEPTION
             WHEN NO_DATA_FOUND THEN
                var_id:=1;
                var_status_cobertura:=1;
                var_glosa:='DAD: Centro de Costo Origen NO Existe (centro_costo) ' || cenCosOrigen;
              goto end_loop_cobertura;
             END;

        END IF;


              -- OBTENGO TIEMPO DE TRASLADO
        IF cenCosOrigen <> cenCosDespacho  and (NVL(cenCosOrigen,0) > 0 and NVL(cenCosDespacho,0) > 0) THEN

            BEGIN
              SELECT
                  tiempo_desp
                INTO
                  tiempo_viaje_origen_destino
              FROM cc_tienda
              WHERE ccosto=cenCosOrigen
                AND tienda=cenCosDespacho;
            EXCEPTION
               WHEN NO_DATA_FOUND THEN
               IF canalVenta <> 23 THEN
                 var_id:=1;
                 var_status_cobertura:=1;
                 var_glosa:='DAD: No esta habilitada la transferencia entre el CC ' || cenCosOrigen || ' y el ' || cenCosDespacho;
                 goto end_loop_cobertura;
               ELSE

                SELECT COUNT(1)
                INTO tiempo_viaje_origen_destino
                FROM MATRIZCC
                WHERE nvl(DESACTIVADO,0)= 0 AND
                  CC_ORIGEN=cenCosOrigen AND
                  CC_DESPACHA = cenCosDespacho;

                IF tiempo_viaje_origen_destino = 0 /*AND c_origen_es_bodega ='T' AND cc_despacha_es_bodega='F'*/ THEN
                 var_id:=1;
                 var_status_cobertura:=1;
                 var_glosa:='DAD: No esta habilitada la transferencia entre el CC ' || cenCosOrigen || ' y el ' || cenCosDespacho;
                 goto end_loop_cobertura;
                END IF;

                tiempo_viaje_origen_destino:=0;


               END IF;



              END;


            ELSE
                tiempo_viaje:=0;
            END IF;


          -- OBTENGO LAS CAPACIDADES DEL CENTRO DE COSTO DE DESPACHO
          IF NVL(cenCosDespacho,0)>0 THEN
           BEGIN
                SELECT
                    tpo_procesoprod,
                    dias_despacho,
                    dias_trabajo,
                    DECODE(retiro_en_tienda,1,cant_rt,cant_desp),
                    DECODE(retiro_en_tienda,1,cant_rt1,cant_desp1),
                    DECODE(retiro_en_tienda,1,cant_rt2,cant_desp2),
                    DECODE(retiro_en_tienda,1,cant_rt3,cant_desp3),
                    DECODE(retiro_en_tienda,1,cant_rt4,cant_desp4),
                    DECODE(retiro_en_tienda,1,cant_rt5,cant_desp5),
                    DECODE(retiro_en_tienda,1,cant_rt6,cant_desp6),
                    DECODE(retiro_en_tienda,1,cant_rt7,cant_desp7),
                    org_is_store,
                    cod_tipocc
                INTO
                    tiempo_proceso_cc_despacha,
                    dias_despacho_cc_despacha,
                    dias_trabajo_cc_despacha,
                    capacidad_reserva_cc,
                    capacidad_l_cc,
                    capacidad_m_cc,
                    capacidad_w_cc,
                    capacidad_j_cc,
                    capacidad_v_cc,
                    capacidad_s_cc,
                    capacidad_d_cc,
                    cc_despacha_es_bodega,
                    tipo_cc_despacha
                FROM centro_costo
                WHERE org_lvl_number=cenCosDespacho;
            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                var_id:=1;
                var_status_cobertura:=1;
                var_glosa:='DAD: El Centro Costo de Despacho NO Existe ' || cenCosDespacho;
                goto end_loop_cobertura; --RETURN;
             END;


            IF capacidad_reserva_cc = 0 THEN
                var_id:=1;
                var_status_cobertura:=1;
                var_glosa:='DAD: El Centro Costo ' || cenCosDespacho || ' tiene capacidad 0';
                goto end_loop_cobertura;
            END IF;
              IF dias_despacho_cc_despacha IS  null OR dias_trabajo_cc_despacha IS NULL  THEN
                var_id:=1;
                var_status_cobertura:=1;
                var_glosa:='DAD: El Centro de Costo Despacho ' || cenCosDespacho || ' No tiene Configurado los dias de Despacho o Trabajo';
                goto end_loop_cobertura;--RETURN;
              END IF;


              /*
          -- NUEVA CAPACIDAD SEG??A???A?N CANAL
            IF capacidad_canal > 0 THEN

                capacidad_reserva_cc:=capacidad_canal;
                capacidad_l_cc:=capacidad_canal;
                capacidad_m_cc:=capacidad_canal;
                capacidad_w_cc:=capacidad_canal;
                capacidad_j_cc:=capacidad_canal;
                capacidad_v_cc:=capacidad_canal;
                capacidad_s_cc:=capacidad_canal;
                capacidad_d_cc:=capacidad_canal;

            END IF;*/


           END IF;




        -- CAPACIDADES DE DESPACHO POR ZONA/COMUNA
        IF NVL(cenCosDespacho,0)>0 THEN
         BEGIN
            SELECT
                  DECODE(retiro_en_tienda,1,0,tiempo_desp),
                  DECODE(retiro_en_tienda,1,NULL,dias_despzona),  -- NO APLICA
                  DECODE(retiro_en_tienda,1,NULL,capacidad),
                  DECODE(retiro_en_tienda,1,NULL,capacidad_l),
                  DECODE(retiro_en_tienda,1,NULL,capacidad_m),
                  DECODE(retiro_en_tienda,1,NULL,capacidad_w),
                  DECODE(retiro_en_tienda,1,NULL,capacidad_j),
                  DECODE(retiro_en_tienda,1,NULL,capacidad_v),
                  DECODE(retiro_en_tienda,1,NULL,capacidad_s),
                  DECODE(retiro_en_tienda,1,NULL,capacidad_d)
              INTO
                  tiempo_despacho, --tiempo despacho a la zona
                  dias_despacho_zona,
                  capacidad_reserva_zona,
                  capacidad_l_zona,
                  capacidad_m_zona,
                  capacidad_w_zona,
                  capacidad_j_zona,
                  capacidad_v_zona,
                  capacidad_s_zona,
                  capacidad_d_zona
            FROM  cc_comuna  left join zona ON
                                      zona.cod_zona=cc_comuna.cod_zona
                                      AND  zona.ccosto=cc_comuna.ccosto
            WHERE canal_venta=canalVenta
              AND comuna=comunaDespacho
              AND cc_comuna.ccosto=cenCosDespacho;
               EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                    var_id:=1;
                    var_status_cobertura:=1;
                    var_glosa:='DAD: Para el Centro Costo de Despacho ' || cenCosDespacho ||', Canal Venta '|| canalVenta ||' y Comuna de Despacho '|| comunaDespacho|| ' NO hay Cobertura';
                   --  RETURN;
                   goto end_loop_cobertura;
                 END;


            IF capacidad_reserva_zona = 0 THEN
                var_id:=1;
                var_status_cobertura:=1;
                var_glosa:='DAD: El Centro Costo ' || cenCosDespacho || ',l Canal Venta '|| canalVenta ||' y Comuna de Despacho '|| comunaDespacho|| ' tiene capacidad 0';
                goto end_loop_cobertura;
            END IF;
          END IF;







         var_hoy:= to_date(to_char(sysdate,'DD/MM/YYYY HH24:MI'),'DD/MM/YYYY HH24:MI');
         var_hoy_corte:= to_date(to_char(sysdate,'DD/MM/YYYY') || ' ' || hora_corte, 'DD/MM/YYYY HH24:MI');


         IF var_hoy > var_hoy_corte THEN
            var_hoy := to_date(to_char(sysdate+1,'DD/MM/YYYY') || ' 00:00'  ,'DD/MM/YYYY HH24:MI'); -- HOY ES MA??A?`ANA
         ELSE
            var_hoy := to_date(to_char(sysdate,'DD/MM/YYYY') || ' 00:00'  ,'DD/MM/YYYY HH24:MI');   -- HOY ES HOY
         END IF;



        <<end_loop_cobertura>>
        FOR rowProductosCounter IN 0..numProductosHijos-1
        LOOP

            var_fecha_inicio_proceso  :=  NULL;
            var_fecha_traslado        :=  NULL;
            var_fecha_despacho        :=  NULL;
            var_fecha_entrega         :=  NULL;
            var_fecha_viaje_real      :=  NULL;

            nodoActualProducto   := dbms_xmldom.item(hijosProductosList, rowProductosCounter);

            hijoProducto         := xslprocessor.selectSingleNode(nodoActualProducto,'codProducto');
            hijoProducto         := xmldom.getFirstChild(hijoProducto);
            codProducto          := dbms_xmldom.getnodevalue(hijoProducto);

            hijoProducto         := xslprocessor.selectSingleNode(nodoActualProducto,'tipoProducto');
            hijoProducto         := xmldom.getFirstChild(hijoProducto);
            tipoProducto         := dbms_xmldom.getnodevalue(hijoProducto);

            hijoProducto         := xslprocessor.selectSingleNode(nodoActualProducto,'diasFabricacion');
            hijoProducto         := xmldom.getFirstChild(hijoProducto);
            diasFabricacion      := dbms_xmldom.getnodevalue(hijoProducto);

            hijoProducto         := xslprocessor.selectSingleNode(nodoActualProducto,'rango');
            hijoProducto         := xmldom.getFirstChild(hijoProducto);
            rango                := dbms_xmldom.getnodevalue(hijoProducto);

            hijoProducto         := xslprocessor.selectSingleNode(nodoActualProducto,'tipoServicio');
            hijoProducto         := xmldom.getFirstChild(hijoProducto);
            tipoServicio         := dbms_xmldom.getnodevalue(hijoProducto);



            hijoProducto         := xslprocessor.selectSingleNode(nodoActualProducto,'saldo');
            hijoProducto         := xmldom.getFirstChild(hijoProducto);
            saldo                := dbms_xmldom.getnodevalue(hijoProducto);

            hijoProducto         := xslprocessor.selectSingleNode(nodoActualProducto,'cantidad');
            hijoProducto         := xmldom.getFirstChild(hijoProducto);
            cantidad             := dbms_xmldom.getnodevalue(hijoProducto);



            hijoProducto         := xslprocessor.selectSingleNode(nodoActualProducto,'item');
            hijoProducto         := xmldom.getFirstChild(hijoProducto);
            item                 := dbms_xmldom.getnodevalue(hijoProducto);


            hijoProducto         := xslprocessor.selectSingleNode(nodoActualProducto,'fechaEntrega');
            hijoProducto         := xmldom.getFirstChild(hijoProducto);
            aux                  := dbms_xmldom.getnodevalue(hijoProducto);

            IF aux IS NOT NULL OR trim(aux) <> '' THEN
                fechaEntrega := to_Date(SUBSTR(aux,0,10),'DD/MM/YYYY');
            END IF;
            dbms_output.put_line('fechaEntrega'||fechaEntrega);
              -- OBTENGO LAS CAPACIDADES DEL CENTRO DE COSTO DE ORIGEN

         IF NVL(cenCosDespacho,0)>0 THEN
         BEGIN
           SELECT
                  DECODE(retiro_en_tienda,1,capacidad_rt,capacidad) --SI ES RT UTILIZO LA CAPACIDAD DE RETIRO EN TIENDA
                /*  capacidad_l,
                  capacidad_m,
                  capacidad_w,
                  capacidad_j,
                  capacidad_v,
                  capacidad_s,
                  capacidad_d*/
              INTO
                  capacidad_reserva_rango
                  /*capacidad_l_rango,
                  capacidad_m_rango,
                  capacidad_w_rango,
                  capacidad_j_rango,
                  capacidad_v_rango,
                  capacidad_s_rango,
                  capacidad_d_rango*/
              FROM cc_rango_hora --O utilizar cc_servicio_hora
              WHERE ccosto=cenCosDespacho
              AND cod_rangohora=rango
              AND ROWNUM <= 1;
            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                  dbms_output.put_line('NO HAY CONCIDENCIAS CC '||cenCosDespacho||' RANGO '||rango);
                  capacidad_reserva_rango:=NULL;
                  capacidad_l_rango:=NULL;
                  capacidad_m_rango:=NULL;
                  capacidad_w_rango:=NULL;
                  capacidad_j_rango:=NULL;
                  capacidad_v_rango:=NULL;
                  capacidad_s_rango:=NULL;
                  capacidad_d_rango:=NULL;
             END;

                  dbms_output.put_line('capacidad_reserva_rango * '||capacidad_reserva_rango);
             END IF;


       IF capacidad_reserva_rango = 0 THEN
                var_id:=1;
                var_glosa:='DAD: El Centro Costo ' || cenCosDespacho || ' para el rango '|| rango || ' tiene capacidad 0';
             --   goto end_loop_cobertura;
       END IF;


     -- CAPACIDADES DE DESPACHO POR RANGO HORARIO


        IF NVL(cenCosDespacho,0)>0 THEN
         BEGIN
            SELECT
                  DECODE(retiro_en_tienda,1,NULL,capacidad)
                 /*, capacidad_l,
                  capacidad_m,
                  capacidad_w,
                  capacidad_j,
                  capacidad_v,
                  capacidad_s,
                  capacidad_d*/
              INTO
                   capacidad_reserva_zona_rango
                 /*, capacidad_l_zona_rango,
                  capacidad_m_zona_rango,
                  capacidad_w_zona_rango,
                  capacidad_j_zona_rango,
                  capacidad_v_zona_rango,
                  capacidad_s_zona_rango,
                  capacidad_d_zona_rango*/
            FROM  cc_comuna  left join cc_zona_rango_hora ON
                                      cc_zona_rango_hora.cod_zona=cc_comuna.cod_zona
                                      AND  cc_zona_rango_hora.ccosto=cc_comuna.ccosto
                                      AND cc_zona_rango_hora.cod_rangohora=rango
            WHERE 1=1
              AND canal_venta=canalVenta
              AND comuna=comunaDespacho
              AND cc_comuna.ccosto=cenCosDespacho;
              EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                      dbms_output.put_line('NO HAY CONCIDENCIAS CC '||cenCosDespacho||' RANGO '||rango);
                      -- capacidad_reserva_zona_rango:=NULL;
                        var_id:=1;
                        var_glosa:='DAD: El Centro de Costo ' || cenCosDespacho || ' no tiene cobertura para la comuna ' || comunaDespacho ;
                        capacidad_reserva_zona_rango:=NULL;
                        --GOTO end_loop;
                 END;

              IF capacidad_reserva_zona_rango = 0 THEN
                var_id:=1;
                var_glosa:='DAD: El Centro de Costo ' || cenCosDespacho || ' no tiene cobertura para la comuna ' || comunaDespacho  || ' para el rango '|| rango || ' tiene capacidad 0';
                --goto end_loop_cobertura;
              END IF;

        END IF;

          IF (NVL(saldo,0) - NVL( cantidad,0))  <  0 or NVL(saldo,0)=0 THEN
            var_id:=1;
            var_glosa:='Producto Sin Saldo';
          END IF;

        IF NVL(var_id,0)!=0 OR NVL(var_status_cobertura,0)!=0 THEN
            GOTO end_loop;
         END IF;


         var_accion:=In_accion;
         IF var_accion = 2 THEN
          var_fecha_entrega  :=fechaEntrega ;
          dbms_output.put_line('var_fecha_entrega '||var_fecha_entrega);
         regresion(retiro_en_tienda,NVL(var_hoy,SYSDATE), canalVenta, tipoStock, diasFabricacion, cenCosOrigen, cenCosDespacho, cc_origen_es_bodega, cc_despacha_es_bodega, tiempo_proceso_proveedor, tiempo_viaje_proveedor, tiempo_despacho_proveedor_com, tiempo_despacho, tiempo_proceso_cc_origen, tiempo_proceso_cc_despacha,   tiempo_viaje_origen_destino, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_trabajo_proveedor, dias_despacho_proveedor, dias_despacho_cc_despacha, dias_despacho_zona, dias_trabajo_cc_orgigen, dias_despacho_cc_origen,dias_trabajo_cc_despacha, dias_viaje_transferencia, dias_despacho_transferencia, dias_proceso_transferencia, dias_calendario, dias_courier, courier_mayor_a, comunaDespacho, rango, fechaEntrega, var_fecha_inicio_proceso, var_fecha_traslado, var_fecha_despacho, var_fecha_entrega, var_id, var_glosa);
         var_fecha_viaje_real:=var_fecha_traslado;
          dbms_output.put_line('FIN  DEL CALCULO ');
         END IF;

       <<end_mejor_fecha>>
        IF var_accion = 1 THEN


             IF tipoStock = 'P'  THEN

                 -- dbms_output.put_line( 'diasFabricacion= ' || diasFabricacion);
                 -- dbms_output.put_line( 'tiempo_proceso_proveedor= ' ||tiempo_proceso_proveedor);
                 -- dbms_output.put_line( 'tiempo_viaje_proveedor= ' ||tiempo_viaje_proveedor);
                 -- dbms_output.put_line( 'dias_trabajo_proveedor= ' || dias_trabajo_proveedor);
                 -- dbms_output.put_line( 'dias_despacho_proveedor= ' || dias_despacho_proveedor);
                 IF NVL(cenCosDespacho,0)=0 THEN
                    var_fecha_inicio_proceso := fecha_inicio_proceso( 1,NULL, 'P', diasFabricacion,tiempo_proceso_proveedor,tiempo_viaje_proveedor,dias_trabajo_proveedor,dias_trabajo_proveedor,dias_calendario,var_hoy) ;
                 ELSE
                   var_fecha_inicio_proceso := fecha_inicio_proceso( 1,cenCosOrigen, 'P', diasFabricacion,tiempo_proceso_proveedor,tiempo_viaje_proveedor,dias_trabajo_proveedor,dias_despacho_proveedor,dias_calendario,var_hoy) ;
                 END IF;
                 var_fecha_traslado:=var_fecha_inicio_proceso;
               --  dbms_output.put_line( 'var_fecha_inicio_proceso= ' || TO_CHAR(var_fecha_inicio_proceso,'DD/MM/YYYY'));


              IF NVL(cenCosDespacho,0)=0 THEN


                   var_fecha_despacho :=  var_fecha_inicio_proceso;
                   var_fecha_traslado:=var_fecha_inicio_proceso;
                   var_fecha_inicio_proceso :=  var_hoy;
                   var_fecha_entrega  :=  fecha_entrega(1,  NULL, tiempo_despacho_proveedor_com ,dias_despacho_proveedor, dias_calendario, var_fecha_despacho);

                  dbms_output.put_line( 'var_fecha_inicio_proceso= ' || TO_CHAR(var_fecha_inicio_proceso,'DD/MM/YYYY'));
                  dbms_output.put_line( 'var_fecha_despacho= ' || TO_CHAR(var_fecha_despacho,'DD/MM/YYYY'));
                  dbms_output.put_line( 'var_fecha_entrega= ' || TO_CHAR(var_fecha_entrega,'DD/MM/YYYY'));
                  -- CONTINUE;
                  GOTO end_loop;
                 END IF;

            ELSE

              var_fecha_inicio_proceso := fecha_inicio_proceso( 1,cenCosOrigen, 'S', diasFabricacion,0,0,dias_trabajo_cc_orgigen,dias_trabajo_cc_orgigen,dias_calendario,var_hoy) ;
              var_fecha_traslado:=var_fecha_inicio_proceso;
                -- var_fecha_inicio_proceso:= var_hoy;
              dbms_output.put_line( 'var_fecha_inicio_proceso= ' || TO_CHAR(var_fecha_inicio_proceso,'DD/MM/YYYY'));
            END IF;



             IF NVL(cenCosOrigen,0) > 0 THEN

                IF cc_origen_es_bodega ='T' AND cc_despacha_es_bodega='F' THEN

                    dias_transferencia_cc:=restringo_calendario(dias_trabajo_cc_orgigen,dias_proceso_transferencia);
                    IF dias_transferencia_cc IS NULL THEN
                      var_id:=1;
                      var_glosa:='IMPOSIBLE Hacer una trasferencias valide Caldendario de trasnferencia (Proceso) y CC '|| cenCosOrigen;
                      GOTO end_loop  ;
                    END IF;

                    var_fecha_traslado:=    fecha_traslado  (1,cenCosOrigen, tiempo_proceso_cc_origen, dias_transferencia_cc, dias_calendario,var_fecha_inicio_proceso);
                    var_fecha_inicio_viaje:=var_fecha_traslado;

                   dbms_output.put_line('FECHA EN QUE EL PRODUCTO TERMINA DE PROCESARCE CON EL CALENDARIO DE TRASFERENCIA=' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));
                 ELSE
                  var_fecha_traslado:=    fecha_traslado  (1,cenCosOrigen, tiempo_proceso_cc_origen, dias_trabajo_cc_orgigen, dias_calendario,var_fecha_inicio_proceso);
                  var_fecha_inicio_viaje:=var_fecha_traslado;
                  dbms_output.put_line('FECHA EN QUE EL PRODUCTO TERMINA DE PROCESARCE CON EL CALENDARIO DE CC DE ORIGEN=' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));
                  END IF;
                  dbms_output.put_line( 'var_fecha_traslado= ' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));
            END IF;



             IF cenCosOrigen <> cenCosDespacho  and (NVL(cenCosOrigen,0) > 0 and NVL(cenCosDespacho,0) > 0) THEN
                 var_fecha_viaje_real:=var_fecha_traslado;
                 dbms_output.put_line( 'var_fecha_viaje_real= ' || TO_CHAR(var_fecha_viaje_real,'DD/MM/YYYY'));

                 --OBTENGO LA FECHA EN QUE EL PRODUCTO LLEGA AL DESTINO
                 IF cc_origen_es_bodega ='T' AND cc_despacha_es_bodega='F' THEN

                     dias_transferencia_cc:=restringo_calendario(dias_despacho_cc_despacha,dias_despacho_transferencia);
                     IF dias_transferencia_cc IS NULL THEN
                      var_id:=1;
                      var_glosa:='IMPOSIBLE Hacer una trasferencias valide Caldendario de trasnferencia (Despacho) y CC '|| cenCosOrigen;
                      GOTO end_loop  ;
                    END IF;



                 -- OBTENGO EL DIA EN QUE PUEDO REALIZAR LA TRASNFERENCIA
                     var_fecha_traslado:=    fecha_traslado  (1,cenCosOrigen, 0, dias_transferencia_cc, dias_calendario,var_fecha_traslado);
                     dbms_output.put_line( 'DIA EN QUE PUEDO REALIZAR LA TRASNFERENCIA= ' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));



                    dias_transferencia_cc:=restringo_calendario(dias_despacho_cc_despacha,dias_viaje_transferencia);
                    IF dias_transferencia_cc IS NULL THEN
                      var_id:=1;
                      var_glosa:='IMPOSIBLE Hacer una trasferencias valide Caldendario de trasnferencia (viaje) y CC '|| cenCosOrigen;
                      GOTO end_loop  ;
                    END IF;

                   var_fecha_traslado:=    fecha_traslado  (1,cenCosOrigen, tiempo_viaje_origen_destino, dias_transferencia_cc, dias_calendario,var_fecha_traslado);


                   dbms_output.put_line('FECHA EN QUE EL PRODUCTO LLEGA AL DESTINO CON EL CALENDARIO DE TRASFERENCIA=' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));
                 ELSE

                    var_fecha_traslado:=    fecha_traslado  (1,cenCosOrigen, 0, dias_despacho_cc_origen, dias_calendario,var_fecha_traslado);
                     dbms_output.put_line( 'DIA EN QUE PUEDO REALIZAR LA TRASNFERENCIA= ' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));

                   var_fecha_traslado:=    fecha_traslado  (1,cenCosOrigen, tiempo_viaje_origen_destino, dias_despacho_cc_despacha, dias_calendario,var_fecha_traslado);
                   dbms_output.put_line( 'FECHA EN QUE EL PRODUCTO LLEGA AL DESTINO CON EL CALENDARIO DEL CC= ' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));

                 END IF;
                 -- OBTENGO CUANDO EL PRODUCTO TERMINA DE PREPARARSE
                 var_fecha_traslado:=    fecha_traslado  (1,cenCosOrigen, tiempo_proceso_cc_despacha,  dias_trabajo_cc_despacha, dias_calendario,var_fecha_traslado);
                 dbms_output.put_line( 'FECHA DEL PRODUCTO TERMINA DE PREPARARSE= ' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));


                 dbms_output.put_line( 'var_fecha_traslado_FIN= ' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));
          END IF;






           --   dbms_output.put_line( 'capacidad_reserva_cc= ' || capacidad_reserva_cc);
           -- dbms_output.put_line( 'capacidad_reserva_zona= ' || capacidad_reserva_zona);
           -- dbms_output.put_line( 'capacidad_reserva_rango= ' || capacidad_reserva_rango);
           -- dbms_output.put_line( 'capacidad_w_zona_rango= ' || capacidad_w_zona_rango);


            IF retiro_en_tienda = 1 THEN
              var_fecha_despacho:=  calcula_fecha_segun_cap_retiro(1,canalVenta, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, cenCosDespacho, comunaDespacho, rango, var_fecha_traslado);
            ELSE
              var_fecha_despacho:=  calcula_fecha_segun_cap_despa(1,canalVenta, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, cenCosDespacho, comunaDespacho, rango, var_fecha_traslado);
            END IF;
            --dbms_output.put_line( 'var_fecha_despacho= ' || TO_CHAR(var_fecha_despacho,'DD/MM/YYYY'));


           -- dbms_output.put_line( 'dias_despacho_cc_despacha= ' || dias_despacho_cc_despacha);
           -- dbms_output.put_line( 'dias_calendario= ' ||dias_calendario);

            -- VALIDO SI SE APLICA REGLA
            if tiempo_despacho >= courier_mayor_a THEN

              dbms_output.put_line( '********* SE APLICA CONDICION COURIER *******' );
               dbms_output.put_line( 'dias_courier= ' ||dias_courier);
              var_fecha_entrega:=  fecha_entrega_courier(1, cenCosDespacho, tiempo_despacho,dias_courier              , dias_calendario, var_fecha_despacho);

            ELSE
              var_fecha_entrega:=  fecha_entrega(1, cenCosDespacho, tiempo_despacho,dias_despacho_cc_despacha, dias_calendario, var_fecha_despacho);
            END IF;
            dbms_output.put_line( 'var_fecha_entrega= ' || TO_CHAR(var_fecha_entrega,'DD/MM/YYYY'));

        END IF;




        -- CALCULA SOLO FECHA DESPACHO Y FECHA ENTREGA
        IF var_accion = 3 THEN



         IF tipoStock = 'P'  THEN


                 IF NVL(cenCosDespacho,0)=0 THEN


                   var_fecha_despacho :=  fecha_entrega(1,  NULL, 0 ,dias_despacho_proveedor, dias_calendario, var_hoy);

                   var_fecha_entrega  :=  fecha_entrega(1,  NULL, tiempo_despacho_proveedor_com ,dias_despacho_proveedor, dias_calendario, var_fecha_despacho);

                   GOTO end_loop;
                 END IF;
           ELSE

           IF retiro_en_tienda = 1 THEN
              var_fecha_despacho:=  calcula_fecha_segun_cap_retiro(1,canalVenta, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, cenCosDespacho, comunaDespacho, rango, var_hoy);
           ELSE
              var_fecha_despacho:=  calcula_fecha_segun_cap_despa(1,canalVenta, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, cenCosDespacho, comunaDespacho, rango, var_hoy);
           END IF;
            -- VALIDO SI SE APLICA REGLA
            if tiempo_despacho >= courier_mayor_a THEN

              dbms_output.put_line( '********* SE APLICA CONDICION COURIER *******' );
              dbms_output.put_line( 'dias_courier= ' ||dias_courier);
              var_fecha_entrega:=  fecha_entrega(1, cenCosDespacho, tiempo_despacho,dias_courier, dias_calendario, var_fecha_despacho);

            ELSE
              var_fecha_entrega:=  fecha_entrega(1, cenCosDespacho, tiempo_despacho,dias_despacho_cc_despacha, dias_calendario, var_fecha_despacho);
            END IF;

            END IF;


        END IF;

        -- CALCULA SOLO FECHA DESPACHO Y FECHA ENTREGA
        IF var_accion = 4 THEN


         var_fecha_despacho  :=fechaEntrega ;
         IF tipoStock = 'P'  THEN


                 IF NVL(cenCosDespacho,0)=0 THEN


                   var_fecha_despacho :=  fecha_entrega(1,  NULL, 0 ,dias_despacho_proveedor, dias_calendario, var_fecha_despacho);

                   var_fecha_entrega  :=  fecha_entrega(1,  NULL, tiempo_despacho_proveedor_com ,dias_despacho_proveedor, dias_calendario, var_fecha_despacho);

                   GOTO end_loop;
                 END IF;
           ELSE

            IF retiro_en_tienda = 1 THEN
                var_fecha_despacho:=  calcula_fecha_segun_cap_retiro(-1,canalVenta, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, cenCosDespacho, comunaDespacho, rango, var_fecha_despacho);
            else
                var_fecha_despacho:=  calcula_fecha_segun_cap_despa(-1,canalVenta, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, cenCosDespacho, comunaDespacho, rango, var_fecha_despacho);
            END IF;
            -- VALIDO SI SE APLICA REGLA
            if tiempo_despacho >= courier_mayor_a THEN

              dbms_output.put_line( '********* SE APLICA CONDICION COURIER *******' );
              dbms_output.put_line( 'dias_courier= ' ||dias_courier);
              var_fecha_entrega:=  fecha_entrega(1, cenCosDespacho, tiempo_despacho,dias_courier, dias_calendario, var_fecha_despacho);

            ELSE
              var_fecha_entrega:=  fecha_entrega(1, cenCosDespacho, tiempo_despacho,dias_despacho_cc_despacha, dias_calendario, var_fecha_despacho);
            END IF;

            END IF;


        END IF;


        <<end_loop>>
        INSERT
          INTO TMP_MOTOR_FECHA
            (
              ID_SESSION,
              TIPO_STOCK,
              CC_ORIGEN,
              CC_VIAJE,
              CC_DESPACHO,
              COD_PROVEEDOR,
              SKU,
              FECHA_INICIO_PROCESO,
              FECHA_VIAJE,
              FECHA_DESPACHO,
              FECHA_ENTREGA,
              ID,
              GLOSA,
              SALDO,
              CANTIDAD,
              ITEM,
              TMP_ULTIMA_MILLA,
              IS_STORE_CC_DESPACHA,
              CAPACIDAD,
              TIPO_CC_DESPACHA,
              TPO_CC_PROCESO_DESP,
              DIAS_DESP,
              TPO_CC_PROCESO_ORIGEN,
              DIAS_ORIGEN
            )
            VALUES
            (
             var_id_session,
              tipoStock,
              cenCosOrigen,
              cenCosVia,
              cenCosDespacho,
              codProveedor,
              codProducto,
              var_fecha_inicio_proceso,
              var_fecha_viaje_real,
              var_fecha_despacho,
              var_fecha_entrega,
              var_id,
              var_glosa,
              saldo,
              cantidad,
              item,
              nvl(tiempo_despacho,9999),
              cc_despacha_es_bodega,
              NVL(capacidad_reserva_cc *1000 + capacidad_reserva_cc_origen ,0) ,
              tipo_cc_despacha,
              tiempo_proceso_cc_despacha,
              LENGTH(dias_despacho_cc_despacha),
              tiempo_proceso_cc_origen,
              LENGTH(dias_despacho_cc_origen)

            );
            var_id:=0;
            var_glosa:='';



        END LOOP;
        var_status_cobertura:=0;
       -- if nvl(var_id,0)>0 then

    END LOOP;
   END LOOP;

  OPEN "out_cursor_fecha" FOR

select  *
from (
SELECT
    T.ID_SESSION,
    T.TIPO_STOCK as tipoStock,
    T.CC_ORIGEN as cenCosOrigen,
    T.CC_VIAJE as cenCosVia,
    T.CC_DESPACHO as cenCosDespacho,
    T.COD_PROVEEDOR as codProveedor,
    T.SKU as codProducto,
    T.FECHA_INICIO_PROCESO as fechaInicioProceso,
    T.FECHA_VIAJE as fechaViaje,
    T.FECHA_DESPACHO as fechaDespacho,
    T.FECHA_ENTREGA as fechaEntrega,
    NVL(id,0) as id,
    NVL(glosa,'OK') as glosa,
    SALDO,
    (CASE CANTIDAD
    WHEN 0 THEN  ROW_NUMBER() OVER  (  PARTITION BY T.ITEM ORDER BY SALDO DESC, nvl(TMP_ULTIMA_MILLA*100,0) ASC )
    ELSE
    ROW_NUMBER() OVER  (  PARTITION BY T.ITEM ORDER BY
    -- SIN PRIORIDAD CUANDO NO HAY SALDO
    CASE   WHEN (nvl( nvl(T.SALDO+0.001,0)- T.CANTIDAD,0) )  > 0 THEN 0 ELSE 1 END,
    -- PRIORIDAD DESPACHOS DIRECTOS DE PROVEEDOR
    DECODE(NVL(CC_DESPACHO,0),0,1),
    -- RPIORIDAD TIEMPO ULTIMA MILLA
    TMP_ULTIMA_MILLA,
    -- DESPCHA LA BODEGA
    DECODE(IS_STORE_CC_DESPACHA,'T',0,1),
    -- PRIORIZO EL CD DE SANTIAGO
    DECODE(CC_DESPACHO,CDSTGO,0,1),
    -- SI ES BODEGA PRIORIZO SU CAPACIDAD
    DECODE(IS_STORE_CC_DESPACHA,'T',CAPACIDAD,0) DESC,
    -- PRIORIDAD SI DESPACHA LA TIENDA QUE VENDE
    DECODE(var_tienda_venta,CC_DESPACHO,0,1),
    -- TIPO CC
    DECODE(IS_STORE_CC_DESPACHA,'F',DECODE(TIPO_CC_DESPACHA,'HI',0,'H',1,2),-1),
    -- PRIORIDADES DESPACHOS DIRECTOS (SIN TRANFERENCIA)
    DECODE(CC_ORIGEN,CC_DESPACHO,0,1),
    -- PRIORIZA LA CAPACIDAD DE LOS DESPACHOS
    CAPACIDAD DESC,
    --MENOR TIEMPO PROCESO CC DESPACHO
    TPO_CC_PROCESO_DESP,
    -- MAYOR CANTIDAD DE DIAS DE DESPACHO
    DIAS_DESP DESC,
    --MENOR TIEMPO PROCESO CC ORIGEN
    TPO_CC_PROCESO_ORIGEN,
    -- MAYOR CANTIDAD DE DIAS DE DESPACHO DEL CC DE ORIGEN
    DIAS_ORIGEN DESC,
    -- CODNCIION FINAL IRREPETIBLE
    T.CC_DESPACHO ASC )
    END
    ) as i,
    item
  FROM
    TMP_MOTOR_FECHA T WHERE T.id_session=var_id_session
    )
    where i=1;
              --DECODE(cc_despacha_es_bodega,'T',DECODE(cenCosOrigen,cenCosDespacho,0,1),DECODE(cenCosOrigen,cenCosDespacho,DECODE(var_tienda_venta,cenCosDespacho,2,3),DECODE(var_tienda_venta,cenCosDespacho,4,5)))
              /* PRIORIDAD
                0-> BODEGA SIN TRANSFERENCIA
                1-> BODEGA CON TRANSFERENCIA
                2-> DESPACHA LA TIENDA QUE SIN TRANSFERENCIA VENDE
                3-> DESPACHA TIENDA SIN TRANSFERENCIA
                4-> DESPACHA LA TIENDA QUE VENDA CON TRANSFERENCIA
                5-> DESPACHA TIENDA CON TRANSFERENCIA
              */
  END;
  END;




  FUNCTION valida_dia
  (
    "dias_trabajo"          IN  VARCHAR2,
    "dias_calendario"       IN  VARCHAR2,
    "cc"                    IN  NUMBER,
    "dia"                   IN  DATE
  )
  RETURN  NUMBER
  AS
  BEGIN
  DECLARE
  sigla_dia     CHAR(1);
  es_feriado    NUMBER;
   BEGIN

--dbms_output.put_line( '"dias_trabajo"  '||"dias_trabajo"   );
    --EVALUO QUE DIA ES HOY
    sigla_dia:=  SUBSTR("dias_calendario", TO_CHAR(TRUNC("dia"),'D'),1) ;
  --  dbms_output.put_line( 'sigla_dia '||sigla_dia  );
     -- VALIDO SI ES POSIBLE OPERAR HOY
     IF  INSTR("dias_trabajo",sigla_dia) > 0 THEN

     -- VALIDO SI ES FERIADO
        SELECT COUNT(1) INTO es_feriado
         FROM dias_feriados
              WHERE TRUNC(FECHA) = TRUNC("dia")
              AND (nvl(centro_costo,0)=0 OR centro_costo="cc");

        IF es_feriado > 0 THEN
           -- dbms_output.put_line( 'DIA FERIADO'  );
            RETURN 1; -- NO SE TRABAJA
        END IF;
      --  dbms_output.put_line( 'TRABAJA CC'  );
        RETURN 0;  -- SE TRABAJA
     ELSE
     --dbms_output.put_line( ' ****NO TRABAJA CC ********'  );
        RETURN 1; -- NO SE TRABAJA
     END IF;


    END;
  END valida_dia;


  FUNCTION valida_capacidad_despacho
  (
    "canal_venta"               IN  NUMBER,
    "capacidad_cc"              IN  NUMBER,
    "capacidad_cc_zona"         IN  NUMBER,
    "capacidad_cc_rango"        IN  NUMBER,
    "capacidad_cc_zona_rango"   IN  NUMBER,
    "cc"                        IN  NUMBER,
    "dia"                       IN  DATE,
    "comuna"                    IN  VARCHAR2,
    "rango"                     IN  VARCHAR2
  )  RETURN  NUMBER
  AS
  BEGIN
  DECLARE
    "cantidad_reservada_cc"      NUMBER;
    "cantidad_reservada_comuna"  NUMBER;
    "cantidad_reservada_rango"   NUMBER;
    "cantidad_reservada_CR"      NUMBER;
    "capacidad_asegurada"        NUMBER;
    "cantidad_reservada_canal"   NUMBER;
    "habilitar_calculo_capacidad" NUMBER;
    --v_star_time NUMBER;
    --v_rest_time NUMBER;
  BEGIN
  dbms_output.put_line( '"dia" = ' || "dia"  );
  --validamos si ejecutamos el calculo de capacidad
  SELECT p.valor_num
  INTO   "habilitar_calculo_capacidad"
  FROM   parametros p
  WHERE  p.cod_param = 171;

  IF ( "habilitar_calculo_capacidad" = 0 ) THEN
    RETURN 1;
  END IF;

 /*  dbms_output.put_line( 'capacidad_cc = ' || "capacidad_cc"  );
    dbms_output.put_line( 'capacidad_cc_zona = ' || "capacidad_cc_zona"  );
     dbms_output.put_line( 'capacidad_cc_rango = ' || "capacidad_cc_rango"  );
      dbms_output.put_line( 'capacidad_cc_zona_rango = ' || "capacidad_cc_zona_rango"  );*/
  -- VALIDO CUANTAS RESERVAS DE LOS DISTINTOS CANALES TENGO
  --v_star_time := SYS.DBMS_UTILITY.GET_TIME;
    BEGIN
         SELECT  /*+ INDEX(RESERVA_DTL IND_RSV_DTL_FECDESP)*/ COUNT(DISTINCT reserva_dtl.num_reserva)
           INTO "cantidad_reservada_canal"
           FROM reserva_dtl,
                reserva_hdr
          WHERE reserva_hdr.canal_venta NOT IN   ( SELECT DISTINCT  canal_venta FROM canal_cc_capacidad WHERE   canal_venta <>  "canal_venta"  AND "dia" BETWEEN fecha_desde AND fecha_hasta AND centro_costo = "cc")
            AND reserva_hdr.canal_venta=reserva_dtl.canal_venta
            AND reserva_hdr.num_reserva=reserva_dtl.num_reserva
            AND cc_despacha = "cc"
            AND reserva_dtl.fecha_despacho=trunc("dia")
            --AND reserva_dtl.cod_estadodet NOT IN (117,111,3,2,23,123, 18, 19, 17,117, 22,122, 21, 121, 14,53,141,114,13,113,12,112)
            AND reserva_dtl.cod_estadodet IN (1,4,5,6,7,8,9,10,11,15,16,50,51,52,61,71,106,107,108,109,110,115,124)
            AND reserva_hdr.tipo_rt IS NULL;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
          "cantidad_reservada_canal":=0;
     END;
DBMS_OUTPUT.PUT_LINE('Cap.  x CC = ' || "capacidad_cc");
 DBMS_OUTPUT.PUT_LINE('RSV/Canal = ' || "cantidad_reservada_canal");
  --v_rest_time := SYS.DBMS_UTILITY.GET_TIME                         - v_star_time ;
 --DBMS_OUTPUT.PUT_LINE(v_rest_time ||'ms');
       BEGIN
          SELECT
              SUM(1)
          INTO
              "capacidad_asegurada"
          FROM canal_cc_capacidad
          WHERE   canal_venta <>  "canal_venta"
              AND "dia" BETWEEN fecha_desde AND fecha_hasta
              AND centro_costo = "cc"
              AND ("capacidad_cc"-"cantidad_reservada_canal") <= capacidad;
          EXCEPTION WHEN NO_DATA_FOUND THEN
              "capacidad_asegurada":=0;
      END;



 -- VALIDO CAPACIDAD ASEGURADA
  IF "capacidad_asegurada">0 THEN
  DBMS_OUTPUT.PUT_LINE('no hay capacidad x Canal ' );
    RETURN 1;
  END IF;



  --v_star_time := SYS.DBMS_UTILITY.GET_TIME;
  -- OBTENGO TODAS LAS RESERVAS DE UN CENTRO DE COSTO
    BEGIN
         SELECT  /*+ INDEX(RESERVA_DTL IND_RSV_DTL_FECDESP)*/ COUNT(DISTINCT reserva_dtl.num_reserva)  INTO "cantidad_reservada_cc"
           FROM reserva_dtl,reserva_hdr
          WHERE 1=1
            AND reserva_hdr.canal_venta=reserva_dtl.canal_venta
            AND reserva_hdr.num_reserva=reserva_dtl.num_reserva
            AND fecha_despacho=trunc("dia")
            AND cc_despacha = "cc"
            --AND reserva_dtl.cod_estadodet NOT IN (117,111,3,2,23,123, 18, 19, 17,117, 22,122, 21, 121, 14,53,141,114,13,113,12,112)
            AND reserva_dtl.cod_estadodet IN (1,4,5,6,7,8,9,10,11,15,16,50,51,52,61,71,106,107,108,109,110,115,124)
            AND reserva_hdr.tipo_rt IS NULL;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
          "cantidad_reservada_cc":=0;
     END;
     --v_rest_time := SYS.DBMS_UTILITY.GET_TIME                         - v_star_time ;
 --DBMS_OUTPUT.PUT_LINE('Query Capacidad '||v_rest_time ||'ms');
  --   dbms_output.put_line( 'cantidad_reservada_cc = ' || "cantidad_reservada_cc"  );
   -- NMO TENGO CAPACIDAD POR CENTRO DE COSTO
    IF "capacidad_cc" <= "cantidad_reservada_cc"  THEN
        RETURN 1;
    END IF;

    -- OBTENGO TODAS LAS RESERVAS DE UNA ZONA
    BEGIN
         SELECT  /*+ INDEX(RESERVA_DTL IND_RSV_DTL_FECDESP)*/ COUNT(DISTINCT reserva_dtl.num_reserva)  INTO "cantidad_reservada_comuna"
           FROM reserva_dtl,reserva_hdr , cc_comuna com,cc_comuna zona
          WHERE 1=1
            AND reserva_hdr.canal_venta = reserva_dtl.canal_venta
            AND reserva_hdr.num_reserva = reserva_dtl.num_reserva
            AND fecha_despacho  = trunc("dia")
            AND cc_despacha = "cc"
            AND com.canal_venta=  zona.canal_venta
            AND com.canal_venta= reserva_dtl.canal_venta
            AND com.ccosto=  zona.ccosto
            AND com.ccosto= reserva_dtl.cc_despacha
            AND com.comuna = "comuna"
            AND com.cod_zona = zona.cod_zona
            AND reserva_hdr.comuna_desp= zona.comuna
            AND com.comuna_tipo='CO'
            AND zona.comuna_tipo='CO'
            AND com.canal_venta=reserva_hdr.canal_venta
            AND zona.canal_venta=reserva_hdr.canal_venta
            --AND reserva_dtl.cod_estadodet NOT IN (117,111,3,2,23,123, 18, 19, 17,117, 22,122, 21, 121, 14,53,141,114,13,113,12,112)
            AND reserva_dtl.cod_estadodet IN (1,4,5,6,7,8,9,10,11,15,16,50,51,52,61,71,106,107,108,109,110,115,124)
            AND reserva_hdr.tipo_rt IS NULL;
   EXCEPTION
      WHEN NO_DATA_FOUND THEN
        "cantidad_reservada_comuna":=0;
   END;
   -- RETURN 0;
  -- dbms_output.put_line( 'cantidad_reservada_comuna = ' || "cantidad_reservada_comuna"  );

   -- NO TENGO CAPACIDAD POR CENTRO DE COSTO Y ZONA
   IF "capacidad_cc_zona"  <= "cantidad_reservada_comuna" AND "capacidad_cc_zona" IS NOT NULL THEN
      RETURN 1;
    END IF;


    --  dbms_output.put_line( 'VALIDO CAPACIDAD POR RANGO '  );
   --  dbms_output.put_line( 'rango = ' || "rango"  );



   -- oOBTENGO CAPACIDAD POR RANGO HORARIO
     BEGIN
           SELECT /*+ INDEX(RESERVA_DTL IND_RSV_DTL_FECDESP)*/  COUNT(DISTINCT reserva_dtl.num_reserva)  INTO "cantidad_reservada_rango"
             FROM reserva_dtl,reserva_hdr
            WHERE 1=1
              AND reserva_hdr.canal_venta = reserva_dtl.canal_venta
              AND reserva_hdr.num_reserva = reserva_dtl.num_reserva
              AND fecha_despacho  = trunc("dia")
              AND cc_despacha = "cc"
              AND ( nvl("rango",'RN') = 'RN' OR cod_rangohora = "rango" )
              --AND reserva_dtl.cod_estadodet NOT IN (117,111,3,2,23,123, 18, 19, 17,117, 22,122, 21, 121, 14,53,141,114,13,113,12,112)
              AND reserva_dtl.cod_estadodet IN (1,4,5,6,7,8,9,10,11,15,16,50,51,52,61,71,106,107,108,109,110,115,124)
              AND reserva_hdr.tipo_rt IS NULL;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
         "cantidad_reservada_rango":=0;
    END;


  --  dbms_output.put_line( 'cantidad_reservada_rango = ' || "cantidad_reservada_rango"  );

   -- NO TENGO CAPACIDAD POR RANGO HORARIO
     IF "capacidad_cc_rango"  <= "cantidad_reservada_rango" AND "capacidad_cc_rango" IS NOT NULL THEN
      RETURN 1;
    END IF;


    BEGIN
         SELECT /*+ INDEX(RESERVA_DTL IND_RSV_DTL_FECDESP)*/  COUNT(DISTINCT reserva_dtl.num_reserva)  INTO "cantidad_reservada_CR"
           FROM reserva_dtl,reserva_hdr , cc_comuna com,cc_comuna zona
          WHERE 1=1
            AND reserva_hdr.canal_venta = reserva_dtl.canal_venta
            AND reserva_hdr.num_reserva = reserva_dtl.num_reserva
            AND fecha_despacho  = trunc("dia")
            AND cc_despacha = "cc"
            --AND reserva_dtl.cod_estadodet NOT IN (117,111,3,2,23,123, 18, 19, 17,117, 22,122, 21, 121, 14,53,141,114,13,113,12,112)
            AND reserva_dtl.cod_estadodet IN (1,4,5,6,7,8,9,10,11,15,16,50,51,52,61,71,106,107,108,109,110,115,124)
            AND ( nvl("rango",'RN') = 'RN' OR cod_rangohora = "rango" )
            AND com.canal_venta=  zona.canal_venta
            AND com.canal_venta= reserva_dtl.canal_venta
            AND com.ccosto=  zona.ccosto
            AND com.ccosto= reserva_dtl.cc_despacha
            AND com.comuna = "comuna"
            AND com.cod_zona = zona.cod_zona
            AND reserva_hdr.comuna_desp= zona.comuna
            AND com.comuna_tipo='CO'
            AND zona.comuna_tipo='CO'
            AND com.canal_venta=reserva_hdr.canal_venta
            AND zona.canal_venta=reserva_hdr.canal_venta
            AND reserva_hdr.tipo_rt IS NULL;
      EXCEPTION
      WHEN NO_DATA_FOUND THEN
        "cantidad_reservada_CR":=0;
    END;



  --  dbms_output.put_line( 'cantidad_reservada_CR = ' || "cantidad_reservada_CR"  );
  --  dbms_output.put_line( 'capacidad_cc_zona_rango = ' || "capacidad_cc_zona_rango"  );

    IF NVL("capacidad_cc_zona_rango",0)  <= "cantidad_reservada_CR" AND "capacidad_cc_zona_rango" IS NOT NULL THEN
      RETURN 1;
    END IF;
     -- dbms_output.put_line( 'HAY CAPACIDAD!!!!'  );
      RETURN 0;


    END;
  END valida_capacidad_despacho;


  FUNCTION valida_capacidad_retiro
  (
    "canal_venta"               IN  NUMBER,
    "capacidad_cc"              IN  NUMBER,
    "capacidad_cc_zona"         IN  NUMBER,
    "capacidad_cc_rango"        IN  NUMBER,
    "capacidad_cc_zona_rango"   IN  NUMBER,
    "cc"                        IN  NUMBER,
    "dia"                       IN  DATE,
    "comuna"                    IN  VARCHAR2,
    "rango"                     IN  VARCHAR2
  )  RETURN  NUMBER
  AS
  BEGIN
  DECLARE
    "cantidad_reservada_cc"      NUMBER;
    "cantidad_reservada_comuna"  NUMBER;
    "cantidad_reservada_rango"   NUMBER;
    "cantidad_reservada_CR"      NUMBER;
    "capacidad_asegurada"        NUMBER;
    "cantidad_reservada_canal"   NUMBER;
    "habilitar_calculo_capacidad" NUMBER;
  BEGIN

  --validamos si ejecutamos el calculo de capacidad
  SELECT p.valor_num
  INTO   "habilitar_calculo_capacidad"
  FROM   parametros p
  WHERE  p.cod_param = 171;

  IF ( "habilitar_calculo_capacidad" = 0 ) THEN
    RETURN 1;
  END IF;

  -- OBTENGO TODAS LAS RESERVAS DE UN CENTRO DE COSTO
    BEGIN
        SELECT /*+ INDEX(RESERVA_DTL IND_RSV_DTL_FECDESP)*/ COUNT(DISTINCT reserva_dtl.num_reserva)  INTO "cantidad_reservada_cc"
        FROM reserva_dtl,reserva_hdr
        WHERE 1=1
            AND reserva_hdr.canal_venta=reserva_dtl.canal_venta
            AND reserva_hdr.num_reserva=reserva_dtl.num_reserva
            AND fecha_despacho=trunc("dia")
            AND cc_despacha = "cc"
            --AND reserva_dtl.cod_estadodet NOT IN (117,111,3,2,23,123, 18, 19, 17,117, 22,122, 21, 121, 14,53,141,114,13,113,12,112)
            AND reserva_dtl.cod_estadodet IN (1,4,5,6,7,8,9,10,11,15,16,50,51,52,61,71,106,107,108,109,110,115,124)
            AND reserva_hdr.tipo_rt IN ('RT','STS');
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
          "cantidad_reservada_cc":=0;
     END;
    -- dbms_output.put_line( '"dia" = ' || "dia"  );
    -- dbms_output.put_line( 'cantidad_reservada_cc = ' || "cantidad_reservada_cc"  );
   -- NMO TENGO CAPACIDAD POR CENTRO DE COSTO
    IF "capacidad_cc" <= "cantidad_reservada_cc"  THEN
        RETURN 1;
    END IF;



    --  dbms_output.put_line( 'VALIDO CAPACIDAD POR RANGO '  );
   --  dbms_output.put_line( 'rango = ' || "rango"  );



   -- oOBTENGO CAPACIDAD POR RANGO HORARIO
     BEGIN
        SELECT /*+ INDEX(RESERVA_DTL IND_RSV_DTL_FECDESP)*/ COUNT(DISTINCT reserva_dtl.num_reserva)  INTO "cantidad_reservada_rango"
            FROM reserva_dtl,reserva_hdr
          WHERE 1=1
              AND reserva_hdr.canal_venta = reserva_dtl.canal_venta
              AND reserva_hdr.num_reserva = reserva_dtl.num_reserva
              AND fecha_despacho  = trunc("dia")
              AND cc_despacha = "cc"
              AND ( nvl("rango",'RN') = 'RN' OR cod_rangohora = "rango" )
              --AND reserva_dtl.cod_estadodet NOT IN (117,111,3,2,23,123, 18, 19, 17,117, 22,122, 21, 121, 14,53,141,114,13,113,12,112)
              AND reserva_dtl.cod_estadodet IN (1,4,5,6,7,8,9,10,11,15,16,50,51,52,61,71,106,107,108,109,110,115,124)
              AND reserva_hdr.tipo_rt IN ('RT','STS');
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
         "cantidad_reservada_rango":=0;
    END;


  --  dbms_output.put_line( 'cantidad_reservada_rango = ' || "cantidad_reservada_rango"  );

   -- NO TENGO CAPACIDAD POR RANGO HORARIO
     IF "capacidad_cc_rango"  <= "cantidad_reservada_rango" AND "capacidad_cc_rango" IS NOT NULL THEN
      RETURN 1;
    END IF;

    --  dbms_output.put_line( 'HAY CAPACIDAD!!!!'  );
      RETURN 0;


    END;
  END valida_capacidad_retiro;



  FUNCTION  fecha_inicio_proceso
  (
    "accion"                    IN  NUMBER, -- DIRECCION DEL TIEMPO [ 1 --> FUTURO.   -1 --> PASADO (REGRESION)]
    "cc"                        IN  NUMBER,
    "tipoStock"                 IN  VARCHAR2, -- P o S
    "dias_fabricacion"          IN  NUMBER,
    "dias_proceso"              IN  NUMBER,
    "dias_viaje"                IN  NUMBER,
    "dias_trabajo"              IN  VARCHAR2,
    "dias_despacho"             IN  VARCHAR2,
    "dias_calendario"           IN  VARCHAR2,
    "dia"                       IN  DATE
  ) RETURN  DATE
  AS
  BEGIN
    DECLARE
      "fecha_inicio_proceso"  DATE;
    BEGIN
    /* dbms_output.put_line( 'dias_proceso ' || "dias_proceso");
      dbms_output.put_line( 'dias_fabricacion : ' || "dias_proceso");
       dbms_output.put_line( 'dias_trabajo ' || "dias_trabajo");
      dbms_output.put_line( 'DIA ' || "dia");*/
       --calculo los dias de preparaci??A???A?n

     "fecha_inicio_proceso":= calcula_fecha_valida( "accion" ,"cc", "dias_proceso"+"dias_fabricacion" , "dias_trabajo" ,"dias_calendario"  ,"dia")   ;
      --SI ES PROVEEDOR CALCULO LOS DIAS DE ENVIO HACIA EL CENTRO DE COSTO

      dbms_output.put_line( 'fecha_inicio_proceso ' || "fecha_inicio_proceso");
      IF "tipoStock" = 'P' AND NVL("cc",0)>0 THEN

        "fecha_inicio_proceso":= calcula_fecha_valida( "accion" , "cc", "dias_viaje" , "dias_despacho" ,"dias_calendario"  ,"fecha_inicio_proceso")   ;
      END IF;

      RETURN "fecha_inicio_proceso";
    END;
  END fecha_inicio_proceso;


  FUNCTION  calcula_fecha_valida
  (
    "direccion"                 IN  NUMBER, -- DIRECCION DEL TIEMPO [ 1 --> FUTURO.   -1 --> PASADO (REGRESION)]
    "cc"                        IN  NUMBER,
    "dias_proceso"              IN  NUMBER,
    "dias_trabajo"              IN  VARCHAR2,
    "dias_calendario"           IN  VARCHAR2,
    "dia"                       IN  DATE
  ) RETURN  DATE
    AS
  BEGIN
   DECLARE
      cantidad_dias     NUMBER:=0;
    BEGIN


   -- dbms_output.put_line( '"nro dias proceso" : ' || "dias_proceso");
    --dbms_output.put_line( '"dias_trabajo" : ' || "dias_trabajo");
    --dbms_output.put_line( 'Se Trabaja ' || valida_dia("dias_trabajo" ,"dias_calendario" ,"cc", "dia"));

      IF  valida_dia("dias_trabajo",  "dias_calendario",  "cc", "dia")=0 AND "dias_proceso"<=0 THEN
    --    dbms_output.put_line( 'Fecha Final: ' || to_CHAR("dia",'DD/MM/YYYY'));
        RETURN "dia";
      END IF;

      IF  valida_dia("dias_trabajo" ,"dias_calendario" ,"cc", "dia")=1 THEN
        RETURN calcula_fecha_valida( "direccion" ,"cc","dias_proceso", "dias_trabajo" ,"dias_calendario"  ,"dia" +"direccion"  );
      END IF;

      IF "dias_proceso" <> 0 THEN
        RETURN calcula_fecha_valida( "direccion" ,"cc","dias_proceso" - 1 ,"dias_trabajo" ,"dias_calendario"  ,"dia" +"direccion"  );
      END IF;

    END;
    END calcula_fecha_valida;


     FUNCTION  fecha_traslado
  (
    "accion"                    IN  NUMBER, -- DIRECCION DEL TIEMPO [ 1 --> FUTURO.   -1 --> PASADO (REGRESION)]
    "cc"                        IN  NUMBER,
    "dias_proceso"              IN  NUMBER,
    "dias_trabajo"              IN  VARCHAR2,
    "dias_calendario"           IN  VARCHAR2,
    "dia"                       IN DATE
  ) RETURN  DATE
  AS BEGIN

      RETURN calcula_fecha_valida( "accion" ,"cc", "dias_proceso", "dias_trabajo" ,"dias_calendario"  ,"dia" );

  END fecha_traslado;


     FUNCTION  fecha_despacho
  (
    "accion"                    IN  NUMBER, -- DIRECCION DEL TIEMPO [ 1 --> FUTURO.   -1 --> PASADO (REGRESION)]
    "dias_fabricacion"          IN  NUMBER,
    "dias_proceso"              IN  NUMBER,
    "dias_trabajo"              IN  VARCHAR2,
    "dias_calendario"           IN  VARCHAR2,
    "dia"                       IN DATE
  ) RETURN  DATE
  AS BEGIN
      RETURN SYSDATE;
  END fecha_despacho;

     FUNCTION  fecha_entrega
  (
    "accion"                    IN  NUMBER, -- DIRECCION DEL TIEMPO [ 1 --> FUTURO.   -1 --> PASADO (REGRESION)]
    "cc"                        IN  NUMBER,
    "dias_envio"                IN  NUMBER,
    "dias_despacho"             IN  VARCHAR2,
    "dias_calendario"           IN  VARCHAR2,
    "dia"                       IN  DATE
  ) RETURN  DATE
  AS BEGIN
  DECLARE
  ct_dia NUMBER:="dias_envio";
  "dia_valido" DATE:="dia";
  BEGIN


 -- dbms_output.put_line('dias_envio::: ' || "dias_envio" );
  --dbms_output.put_line('dias_despacho::: ' || "dias_despacho" );
 --  dbms_output.put_line('dias_calendario:::: ' || "dias_calendario"  );
   WHILE TRUE LOOP

--dbms_output.put_line('ct_dia: ' || ct_dia );



   IF  valida_dia (nvl("dias_despacho",0) ,"dias_calendario" ,"cc","dia_valido" )=0 THEN
     -- dbms_output.put_line( 'Trabaja  : ' || to_CHAR("dia_valido",'DD/MM/YYYY'));





      IF  ct_dia <= 0 THEN
        RETURN "dia_valido";
      END IF;
       ct_dia:= ct_dia -1;
       "dia_valido" := "dia_valido"+"accion";
   ELSE
   -- dbms_output.put_line( 'NOOOOOO Trabaja : ' || to_CHAR("dia_valido",'DD/MM/YYYY'));

    "dia_valido":="dia_valido"+"accion";
   END IF;

  END LOOP;

     END;
  END fecha_entrega;

  FUNCTION  fecha_entrega_courier
  (
    "accion"                    IN  NUMBER, -- DIRECCION DEL TIEMPO [ 1 --> FUTURO.   -1 --> PASADO (REGRESION)]
    "cc"                        IN  NUMBER,
    "dias_envio"                IN  NUMBER,
    "dias_despacho"             IN  VARCHAR2,
    "dias_calendario"           IN  VARCHAR2,
    "dia"                       IN  DATE
  ) RETURN  DATE
  AS BEGIN
  DECLARE
  ct_dia NUMBER:="dias_envio";
  "dia_valido" DATE:="dia";
  BEGIN


--  dbms_output.put_line('dias_envio::: ' || "dias_envio" );
 -- dbms_output.put_line('dias_despacho::: ' || "dias_despacho" );
 -- dbms_output.put_line('dias_calendario:::: ' || "dias_calendario"  );
  --ct_dia:= ct_dia -1;
  --"dia_valido" := "dia_valido"+"accion";
  ct_dia:= ct_dia -1;
    IF  valida_dia (nvl("dias_despacho",0) ,"dias_calendario" ,"cc","dia_valido" )=0 AND "dias_envio" =0 THEN
      RETURN "dia_valido";
    END IF;
   WHILE TRUE LOOP

--dbms_output.put_line('ct_dia: ' || ct_dia );

 "dia_valido" := "dia_valido"+"accion";

   IF  valida_dia (nvl("dias_despacho",0) ,"dias_calendario" ,"cc","dia_valido" )=0 THEN
     -- dbms_output.put_line( 'Trabaja  : ' || to_CHAR("dia_valido",'DD/MM/YYYY'));


      IF  ct_dia <= 0 THEN
        RETURN "dia_valido";
      END IF;
      ct_dia:= ct_dia -1;
   END IF;

  END LOOP;

     END;
  END fecha_entrega_courier;

    FUNCTION  calcula_fecha_segun_cap_despa
  (
    "accion"                    IN  NUMBER, -- direccion
    "canal_venta"               IN  NUMBER, -- direccion
    "capacidad_reserva_cc"      IN  NUMBER,
    "capacidad_l_cc"            IN  NUMBER,
    "capacidad_m_cc"            IN  NUMBER,
    "capacidad_w_cc"            IN  NUMBER,
    "capacidad_j_cc"            IN  NUMBER,
    "capacidad_v_cc"            IN  NUMBER,
    "capacidad_s_cc"            IN  NUMBER,
    "capacidad_d_cc"            IN  NUMBER,
    "capacidad_reserva_zona"    IN  NUMBER,
    "capacidad_l_zona"          IN  NUMBER,
    "capacidad_m_zona"          IN  NUMBER,
    "capacidad_w_zona"          IN  NUMBER,
    "capacidad_j_zona"          IN  NUMBER,
    "capacidad_v_zona"          IN  NUMBER,
    "capacidad_s_zona"          IN  NUMBER,
    "capacidad_d_zona"          IN  NUMBER,
    "capacidad_reserva_rango"   IN  NUMBER,
    "capacidad_l_rango"         IN  NUMBER,
    "capacidad_m_rango"         IN  NUMBER,
    "capacidad_w_rango"         IN  NUMBER,
    "capacidad_j_rango"         IN  NUMBER,
    "capacidad_v_rango"         IN  NUMBER,
    "capacidad_s_rango"         IN  NUMBER,
    "capacidad_d_rango"         IN  NUMBER,
    "capacidad_reserva_zona_rango"  IN  NUMBER,
    "capacidad_l_zona_rango"    IN  NUMBER,
    "capacidad_m_zona_rango"    IN  NUMBER,
    "capacidad_w_zona_rango"    IN  NUMBER,
    "capacidad_j_zona_rango"    IN  NUMBER,
    "capacidad_v_zona_rango"    IN  NUMBER,
    "capacidad_s_zona_rango"    IN  NUMBER,
    "capacidad_d_zona_rango"    IN  NUMBER,
    "dias_despacho"             IN  VARCHAR2,
    "dias_despacho_zona"        IN  VARCHAR2,
    "dias_calendario"           IN  VARCHAR2,
    "cc"                        IN  NUMBER,
    "comuna"                    IN  VARCHAR2,
    "rango"                     IN  VARCHAR2 ,
    "dia"                       IN  DATE
  ) RETURN  DATE
   AS BEGIN
   DECLARE
      "sigla_dia" char(1);
      "capacidad_cc" NUMBER;
      "capacidad_zona" NUMBER;
      "capacidad_rango" NUMBER;
      "capacidad_zona_rango" NUMBER;
      "es_dia_valido" DATE:="dia";
   BEGIN

 WHILE TRUE LOOP

      "sigla_dia":=  SUBSTR("dias_calendario", TO_CHAR(TRUNC("es_dia_valido"),'D'),1) ;
     -- dbms_output.put_line( ' sigla_dia: ' || "sigla_dia");
     --dbms_output.put_line( ' capacidad_cc: ' || "capacidad_cc_123");
    -- dbms_output.put_line( ' capacidad_zona: ' || "capacidad_zona");
    -- dbms_output.put_line( ' capacidad_rango: ' || "capacidad_rango");
    -- dbms_output.put_line( ' capacidad_zona_rango: ' || "capacidad_zona_rango");
    -- dbms_output.put_line( ' dias_despacho: ' || "dias_despacho");
    -- dbms_output.put_line( ' dias_despacho_zona: ' || "dias_despacho_zona");
    -- dbms_output.put_line( ' dias_calendario: ' || "dias_calendario");

     SELECT NVL(DECODE("sigla_dia",'L',"capacidad_l_cc",'M',"capacidad_m_cc",'W',"capacidad_w_cc",'J',"capacidad_j_cc",'V',"capacidad_v_cc",'S',"capacidad_s_cc",'D',"capacidad_d_cc"),"capacidad_reserva_cc") INTO "capacidad_cc" FROM DUAL;

    -- SELECT NVL(DECODE("sigla_dia",'L',"capacidad_l_zona",'M',"capacidad_m_zona",'W',"capacidad_w_zona",'J',"capacidad_j_zona",'V',"capacidad_v_zona",'S',"capacidad_s_zona",'D',"capacidad_d_zona"),"capacidad_reserva_zona") INTO "capacidad_zona" FROM DUAL;

     --SELECT NVL(DECODE("sigla_dia",'L',"capacidad_l_rango",'M',"capacidad_m_rango",'W',"capacidad_w_rango",'J',"capacidad_j_rango",'V',"capacidad_v_rango",'S',"capacidad_s_rango",'D',"capacidad_d_rango"),"capacidad_reserva_rango") INTO "capacidad_rango" FROM DUAL;

     --SELECT NVL(DECODE("sigla_dia",'L',"capacidad_l_zona_rango",'M',"capacidad_m_zona_rango",'W',"capacidad_w_zona_rango",'J',"capacidad_j_zona_rango",'V',"capacidad_v_zona_rango",'S',"capacidad_s_zona_rango",'D',"capacidad_d_zona_rango"),"capacidad_reserva_zona_rango") INTO "capacidad_zona_rango" FROM DUAL;

     "capacidad_rango"      :=  "capacidad_reserva_rango";
     "capacidad_zona"       :=  "capacidad_reserva_zona";
     "capacidad_zona_rango" :=  "capacidad_reserva_zona_rango";

     --dbms_output.put_line( ' capacidad_cc: ' || "capacidad_cc_123");
     --dbms_output.put_line( ' capacidad_zona: ' || "capacidad_zona");
     --dbms_output.put_line( ' capacidad_rango: ' || "capacidad_rango");
     --dbms_output.put_line( ' capacidad_zona_rango: ' || "capacidad_zona_rango");
     --dbms_output.put_line( ' dias_despacho: ' || "dias_despacho");
     --dbms_output.put_line( ' dias_despacho_zona: ' || "dias_despacho_zona");
     --dbms_output.put_line( ' dias_calendario: ' || "dias_calendario");

 --dbms_output.put_line( '************************************************************');
   --  dbms_output.put_line( ' capacidad_cc: ' || "capacidad_cc_123");
 --dbms_output.put_line( '"es_dia_valido"'||"es_dia_valido");
 --validar si es capacidad_reserva_cc o capacidad_cc
    IF  valida_capacidad_despacho( "canal_venta","capacidad_cc", "capacidad_zona", "capacidad_rango" , "capacidad_zona_rango","cc","es_dia_valido","comuna","rango")= 0 AND valida_dia("dias_despacho", "dias_calendario" ,"cc", "es_dia_valido" )=0 AND (valida_dia( nvl("dias_despacho_zona","dias_despacho"), "dias_calendario" ,"cc", "es_dia_valido" )=0 ) THEN
        RETURN "es_dia_valido";
    END IF;

    "es_dia_valido":="es_dia_valido"+ "accion" ;

 END LOOP;


      RETURN SYSDATE;
  END;
  END calcula_fecha_segun_cap_despa;

    FUNCTION  calcula_fecha_segun_cap_retiro
  (
    "accion"                    IN  NUMBER, -- direccion
    "canal_venta"               IN  NUMBER, -- direccion
    "capacidad_reserva_cc"      IN  NUMBER,
    "capacidad_l_cc"            IN  NUMBER,
    "capacidad_m_cc"            IN  NUMBER,
    "capacidad_w_cc"            IN  NUMBER,
    "capacidad_j_cc"            IN  NUMBER,
    "capacidad_v_cc"            IN  NUMBER,
    "capacidad_s_cc"            IN  NUMBER,
    "capacidad_d_cc"            IN  NUMBER,
    "capacidad_reserva_zona"    IN  NUMBER,
    "capacidad_l_zona"          IN  NUMBER,
    "capacidad_m_zona"          IN  NUMBER,
    "capacidad_w_zona"          IN  NUMBER,
    "capacidad_j_zona"          IN  NUMBER,
    "capacidad_v_zona"          IN  NUMBER,
    "capacidad_s_zona"          IN  NUMBER,
    "capacidad_d_zona"          IN  NUMBER,
    "capacidad_reserva_rango"   IN  NUMBER,
    "capacidad_l_rango"         IN  NUMBER,
    "capacidad_m_rango"         IN  NUMBER,
    "capacidad_w_rango"         IN  NUMBER,
    "capacidad_j_rango"         IN  NUMBER,
    "capacidad_v_rango"         IN  NUMBER,
    "capacidad_s_rango"         IN  NUMBER,
    "capacidad_d_rango"         IN  NUMBER,
    "capacidad_reserva_zona_rango"  IN  NUMBER,
    "capacidad_l_zona_rango"    IN  NUMBER,
    "capacidad_m_zona_rango"    IN  NUMBER,
    "capacidad_w_zona_rango"    IN  NUMBER,
    "capacidad_j_zona_rango"    IN  NUMBER,
    "capacidad_v_zona_rango"    IN  NUMBER,
    "capacidad_s_zona_rango"    IN  NUMBER,
    "capacidad_d_zona_rango"    IN  NUMBER,
    "dias_despacho"             IN  VARCHAR2,
    "dias_despacho_zona"        IN  VARCHAR2,
    "dias_calendario"           IN  VARCHAR2,
    "cc"                        IN  NUMBER,
    "comuna"                    IN  VARCHAR2,
    "rango"                     IN  VARCHAR2 ,
    "dia"                       IN  DATE
  ) RETURN  DATE
   AS BEGIN
   DECLARE
      "sigla_dia" char(1);
      "capacidad_cc" NUMBER;
      "capacidad_zona" NUMBER;
      "capacidad_rango" NUMBER;
      "capacidad_zona_rango" NUMBER;
      "es_dia_valido" DATE:="dia";
   BEGIN

 WHILE TRUE LOOP
      "sigla_dia":=  SUBSTR("dias_calendario", TO_CHAR(TRUNC("es_dia_valido"),'D'),1) ;

     SELECT NVL(DECODE("sigla_dia",'L',"capacidad_l_cc",'M',"capacidad_m_cc",'W',"capacidad_w_cc",'J',"capacidad_j_cc",'V',"capacidad_v_cc",'S',"capacidad_s_cc",'D',"capacidad_d_cc"),"capacidad_reserva_cc") INTO "capacidad_cc" FROM DUAL;

    -- SELECT NVL(DECODE("sigla_dia",'L',"capacidad_l_zona",'M',"capacidad_m_zona",'W',"capacidad_w_zona",'J',"capacidad_j_zona",'V',"capacidad_v_zona",'S',"capacidad_s_zona",'D',"capacidad_d_zona"),"capacidad_reserva_zona") INTO "capacidad_zona" FROM DUAL;

     --SELECT NVL(DECODE("sigla_dia",'L',"capacidad_l_rango",'M',"capacidad_m_rango",'W',"capacidad_w_rango",'J',"capacidad_j_rango",'V',"capacidad_v_rango",'S',"capacidad_s_rango",'D',"capacidad_d_rango"),"capacidad_reserva_rango") INTO "capacidad_rango" FROM DUAL;

     --SELECT NVL(DECODE("sigla_dia",'L',"capacidad_l_zona_rango",'M',"capacidad_m_zona_rango",'W',"capacidad_w_zona_rango",'J',"capacidad_j_zona_rango",'V',"capacidad_v_zona_rango",'S',"capacidad_s_zona_rango",'D',"capacidad_d_zona_rango"),"capacidad_reserva_zona_rango") INTO "capacidad_zona_rango" FROM DUAL;

     "capacidad_rango"      :=  "capacidad_reserva_rango";
     "capacidad_zona"       :=  "capacidad_reserva_zona";
     "capacidad_zona_rango" :=  "capacidad_reserva_zona_rango";

   --  dbms_output.put_line( ' capacidad_cc: ' || "capacidad_cc");
   --  dbms_output.put_line( ' capacidad_zona: ' || "capacidad_zona");
    -- dbms_output.put_line( ' capacidad_rango: ' || "capacidad_rango");
    -- dbms_output.put_line( ' capacidad_zona_rango: ' || "capacidad_zona_rango");
    -- dbms_output.put_line( ' dias_despacho: ' || "dias_despacho");
   --  dbms_output.put_line( ' dias_despacho_zona: ' || "dias_despacho_zona");
   --  dbms_output.put_line( ' dias_calendario: ' || "dias_calendario");



    IF  valida_capacidad_retiro( "canal_venta","capacidad_cc", "capacidad_zona", "capacidad_rango" , "capacidad_zona_rango","cc","es_dia_valido","comuna","rango")= 0 AND valida_dia("dias_despacho", "dias_calendario" ,"cc", "es_dia_valido" )=0 THEN
        RETURN "es_dia_valido";
    END IF;

    "es_dia_valido":="es_dia_valido"+ "accion" ;

 END LOOP;


      RETURN SYSDATE;
  END;
  END calcula_fecha_segun_cap_retiro;






  PROCEDURE servicios_flete_consultar(
    var_fecha_entrega     IN DATE,
    var_fecha_despacho    IN DATE,
    "cc"                  IN NUMBER,
    "varSeq"              IN VARCHAR2,
    "tipoDespacho"        IN NUMBER,
    "codProveedor"        IN NUMBER,
    "comuna"              IN VARCHAR2,
    "canal_venta"         IN NUMBER)
AS
BEGIN
  DECLARE
    CURSOR "crServicios"
    IS
      SELECT "codigo",
        "centroCosto",
        "descripcion",
        "tipoServicio",
        "diasInicio",
        "diasDelta",
        "rangoDias",
        "observacion",
        "desactivado",
        "repeticiones",
        "precio",
        "costo",
       -- "diasDespacho",
        rh.cod_rangohora AS cod_rangohora,
        rh.hora_fin      AS hora_fin,
        rh.hora_ini      AS hora_ini,
        S.FIJO
      FROM FLTSERVICIOCCVIEW,
        cc_servicio_hora CSH ,
        rango_hora RH,
        SERVICIO S,
        serv_hora
      WHERE "centroCosto"  =nvl("cc",0)
      AND rh.cod_rangohora = csh.cod_rangohora
      AND csh.ccosto       = "centroCosto"
      AND csh.cod_servicio = "codigo"
      AND "tipoServicio"= DECODE("tipoDespacho",1,'R',2,'D','')
      AND S.DESACTIVADO =0
      AND "desactivado"=0
      and serv_hora.cod_servicio="codigo"
      AND serv_hora.cod_rangohora=rh.cod_rangohora
      AND s.cod_servicio="codigo"
      ORDER BY "codigo";




    var_codigo              FLTSERVICIOCCVIEW."codigo"%TYPE;
    var_centroCosto         FLTSERVICIOCCVIEW."centroCosto"%TYPE;
    var_descripcion         FLTSERVICIOCCVIEW."descripcion"%TYPE;
    var_tipoServicio        FLTSERVICIOCCVIEW."tipoServicio"%TYPE;
    var_diasInicio          FLTSERVICIOCCVIEW."diasInicio"%TYPE;
    var_diasDelta           FLTSERVICIOCCVIEW."diasDelta"%TYPE;
    var_diasDelta2          FLTSERVICIOCCVIEW."diasDelta"%TYPE;
    var_rangoDias           FLTSERVICIOCCVIEW."rangoDias"%TYPE;
    var_observacion         FLTSERVICIOCCVIEW."observacion"%TYPE;
    var_desactivado         FLTSERVICIOCCVIEW."desactivado"%TYPE;
    var_repeticiones        FLTSERVICIOCCVIEW."repeticiones"%TYPE;
    var_precio              FLTSERVICIOCCVIEW."precio"%TYPE;
    var_costo               FLTSERVICIOCCVIEW."costo"%TYPE;
 --   var_diasDespacho FLTSERVICIOCCVIEW."diasDespacho"%TYPE;
    var_express             SERVICIO.FIJO%TYPE;
    var_cod_rangohora       rango_hora.cod_rangohora%TYPE;
    var_hora_fin            rango_hora.hora_fin%TYPE;
    var_hora_ini            rango_hora.hora_ini%TYPE;
    VAR_DIAS                VARCHAR2(1000);
    VAR_DAYFI               VARCHAR2(1000);
    VAR_DIAS_INI            NUMBER;
    VAR_DIAS_DELTA          NUMBER;
    VAR_DIAS_FECHA          DATE;
    VAR_DIAFINAL            NUMBER;
    VAR_FECHA_MINIMA        DATE;
    VAR_FECHA_E             DATE;
    VAR_FECHA_MINIMA2       DATE;
    VAR_FECHA_MAXIMA        DATE;
    dias_calendario         CHAR(7);
    VAR_ACTIVEDAY           CHAR(7);
    rowCT                   NUMBER:=0;
    contador                NUMBER:=0;
    ctDia                   NUMBER:=0;
    var_primera_ejecucion   NUMBER:=0;
    var_es_peor_fecha       NUMBER:=0;
    ctRep                   NUMBER:=0;
    dia                     DATE;
    var_fecha_entrega_valida    DATE;
    var_fecha_despacho_valida   DATE;

    tiempo_despacho     NUMBER;

    dias_despacho_proveedor       VARCHAR2(7);
    dias_despacho_cc_despacha     VARCHAR2(7);
    dias_trabajo_cc_despacha      VARCHAR2(7);
    dias_despacho_zona            VARCHAR2(7);
    "sigla_dia"                   VARCHAR2(7);
    capacidad_reserva_cc          NUMBER;
    capacidad_reserva_cc_real          NUMBER;
    capacidad_l_cc                NUMBER;
    capacidad_m_cc                NUMBER;
    capacidad_w_cc                NUMBER;
    capacidad_j_cc                NUMBER;
    capacidad_v_cc                NUMBER;
    capacidad_s_cc                NUMBER;
    capacidad_d_cc                NUMBER;
    capacidad_reserva_zona        NUMBER;
    capacidad_l_zona              NUMBER;
    capacidad_m_zona              NUMBER;
    capacidad_w_zona              NUMBER;
    capacidad_j_zona              NUMBER;
    capacidad_v_zona              NUMBER;
    capacidad_s_zona              NUMBER;
    capacidad_d_zona              NUMBER;
    capacidad_reserva_rango       NUMBER;
    capacidad_l_rango             NUMBER;
    capacidad_m_rango             NUMBER;
    capacidad_w_rango             NUMBER;
    capacidad_j_rango             NUMBER;
    capacidad_v_rango             NUMBER;
    capacidad_s_rango             NUMBER;
    capacidad_d_rango             NUMBER;
    capacidad_reserva_zona_rango  NUMBER;
    capacidad_l_zona_rango        NUMBER;
    capacidad_m_zona_rango        NUMBER;
    capacidad_w_zona_rango        NUMBER;
    capacidad_j_zona_rango        NUMBER;
    capacidad_v_zona_rango        NUMBER;
    capacidad_s_zona_rango        NUMBER;
    capacidad_d_zona_rango        NUMBER;
    dias_courier              VARCHAR2(7);
    courier_mayor_a           NUMBER;
    puedo_despachar               NUMBER;
    TYPE array_dias_despacho_type IS TABLE OF NUMBER INDEX BY VARCHAR2(8);
    array_dias_despacho             array_dias_despacho_type;

  BEGIN
  --  dbms_output.put_line('cc :'||"cc");
    dbms_output.put_line('"Ejecuta Flete"');


   DELETE tmp_calculofecha WHERE id="varSeq";
--   SELECT VAR_ALLDAY

   SELECT valor_str INTO dias_calendario  FROM parametros WHERE cod_param=62;


   SELECT valor_str INTO dias_courier  FROM parametros WHERE cod_param=153;


    --OBTENGO LOS DESDE CUANTOS DIAS PUEDE DESPACHART EL COURIER
    SELECT valor_num INTO courier_mayor_a  FROM parametros WHERE cod_param=155;
 --dbms_output.put_line('>>>>>>>>>>>>>>>>>>>>');
   -- VAR_ALLDAY:='DLMWJVS';
    OPEN "crServicios";
    LOOP
      FETCH "crServicios"
      INTO var_codigo,
        var_centroCosto,
        var_descripcion,
        var_tipoServicio,
        var_diasInicio,
        var_diasDelta,
        var_rangoDias,
        var_observacion,
        var_desactivado,
        var_repeticiones,
        var_precio,
        var_costo,
    --    var_diasDespacho,
        var_cod_rangohora,
        var_hora_fin,
        var_hora_ini,
        var_express;
      EXIT
    WHEN "crServicios"%NOTFOUND;
    dbms_output.put_line('LOOP :');

  --  dbms_output.put_line('*********************');
      var_primera_ejecucion:=0;
      var_es_peor_fecha:=0;



    IF NVL("cc",0) = 0 THEN


      dbms_output.put_line('codProveedor: ' || "codProveedor");

       BEGIN
            SELECT
                dias_despacho
            INTO
                dias_despacho_proveedor
            FROM proveedor
            WHERE proveedor.cod_prov=CAST("codProveedor" AS CHAR(10));
       EXCEPTION
          WHEN NO_DATA_FOUND THEN
             NULL;
       END;


              BEGIN
                  SELECT
                    tiempo_desp
                  INTO
                    tiempo_despacho
                  FROM prov_comuna
                  WHERE comuna="comuna"
                    AND cod_prov=CAST("codProveedor" AS CHAR(10));
              EXCEPTION
             WHEN NO_DATA_FOUND THEN
              NULL;
             END;






   ELSE


           BEGIN
                SELECT
                    dias_despacho,
                    dias_trabajo,
                    DECODE("tipoDespacho",1,cant_rt,cant_desp),
                    DECODE("tipoDespacho",1,cant_rt1,cant_desp1),
                    DECODE("tipoDespacho",1,cant_rt2,cant_desp2),
                    DECODE("tipoDespacho",1,cant_rt3,cant_desp3),
                    DECODE("tipoDespacho",1,cant_rt4,cant_desp4),
                    DECODE("tipoDespacho",1,cant_rt5,cant_desp5),
                    DECODE("tipoDespacho",1,cant_rt6,cant_desp6),
                    DECODE("tipoDespacho",1,cant_rt7,cant_desp7)
                INTO
                    dias_despacho_cc_despacha,
                    dias_trabajo_cc_despacha,
                    capacidad_reserva_cc_real,
                    capacidad_l_cc,
                    capacidad_m_cc,
                    capacidad_w_cc,
                    capacidad_j_cc,
                    capacidad_v_cc,
                    capacidad_s_cc,
                    capacidad_d_cc
                FROM centro_costo
                WHERE org_lvl_number="cc";
            EXCEPTION
              WHEN NO_DATA_FOUND THEN
               NULL;
             END;

             IF capacidad_reserva_cc = 0 THEN
                GOTO end_loop;
             END IF;
             dbms_output.put_line('FLT cap_rsv_cc' || capacidad_reserva_cc);
               -- OBTENGO LAS CAPACIDADES DEL CENTRO DE COSTO DE ORIGEN
        BEGIN
           SELECT
                DECODE("tipoDespacho",1, capacidad_rt,capacidad), --SI ES RT UTILIZO LA CAPACIDAD DE RETIRO EN TIENDA
                DECODE("tipoDespacho",1, NULL, capacidad_l),
                DECODE("tipoDespacho",1, NULL, capacidad_m),
                DECODE("tipoDespacho",1, NULL, capacidad_w),
                DECODE("tipoDespacho",1, NULL, capacidad_j),
                DECODE("tipoDespacho",1, NULL, capacidad_v),
                DECODE("tipoDespacho",1, NULL, capacidad_s),
                DECODE("tipoDespacho",1, NULL, capacidad_d)
              INTO
                  capacidad_reserva_rango,
                  capacidad_l_rango,
                  capacidad_m_rango,
                  capacidad_w_rango,
                  capacidad_j_rango,
                  capacidad_v_rango,
                  capacidad_s_rango,
                  capacidad_d_rango
              FROM cc_rango_hora --O utilizar cc_servicio_hora
              WHERE ccosto="cc"
              AND cod_rangohora=var_cod_rangohora
              AND ROWNUM <= 1;
            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                  dbms_output.put_line('NO EXISTE RESTRICCION DE CENTRO DE COSTO RANGO');
                  NULL;
             END;

             dbms_output.put_line('FLT cap_rsv_rh' || capacidad_reserva_rango);
             IF capacidad_reserva_rango = 0 THEN
                GOTO end_loop;
             END IF;

            BEGIN
               SELECT
                   DECODE("tipoDespacho", 1, NULL, tiempo_desp),
                   DECODE("tipoDespacho", 1, NULL, dias_despzona),  -- NO APLICA
                   DECODE("tipoDespacho", 1, NULL, capacidad   ),
                   DECODE("tipoDespacho", 1, NULL, capacidad_l ),
                   DECODE("tipoDespacho", 1, NULL, capacidad_m ),
                   DECODE("tipoDespacho", 1, NULL, capacidad_w ),
                   DECODE("tipoDespacho", 1, NULL, capacidad_j ),
                   DECODE("tipoDespacho", 1, NULL, capacidad_v ),
                   DECODE("tipoDespacho", 1, NULL, capacidad_s ),
                   DECODE("tipoDespacho", 1, NULL, capacidad_d )
              INTO
                  tiempo_despacho, --tiempo despacho a la zona
                  dias_despacho_zona,
                  capacidad_reserva_zona,
                  capacidad_l_zona,
                  capacidad_m_zona,
                  capacidad_w_zona,
                  capacidad_j_zona,
                  capacidad_v_zona,
                  capacidad_s_zona,
                  capacidad_d_zona
            FROM  cc_comuna  left join zona ON
                                      zona.cod_zona=cc_comuna.cod_zona
                                      AND  zona.ccosto=cc_comuna.ccosto
            WHERE canal_venta="canal_venta"
              AND comuna= "comuna"
              AND cc_comuna.ccosto="cc";
             EXCEPTION
                 WHEN NO_DATA_FOUND THEN
                 tiempo_despacho:=0;
                 dbms_output.put_line('NO EXISTE RESTRICCION PARA LA ZONA');
             END;

             dbms_output.put_line('FLT cap RSV ZONA' || capacidad_reserva_zona);
             IF capacidad_reserva_zona = 0 THEN
                GOTO end_loop;
             END IF;


           BEGIN

                  SELECT
              capacidad
          INTO
              capacidad_reserva_zona_rango
        FROM  cc_comuna  left join cc_zona_rango_hora ON
                                  cc_zona_rango_hora.cod_zona=cc_comuna.cod_zona
                                  AND  cc_zona_rango_hora.ccosto=cc_comuna.ccosto
                                  AND cc_zona_rango_hora.cod_rangohora=var_cod_rangohora
        WHERE 1=1
          AND canal_venta="canal_venta"
          AND comuna="comuna"
          AND cc_comuna.ccosto="cc";
                  EXCEPTION
                 WHEN NO_DATA_FOUND THEN
                 dbms_output.put_line('NO EXISTE RESTRICCION PARA EL RANGO HORARIO POR ZONA');
             END;


             IF capacidad_reserva_zona_rango = 0 THEN
                GOTO end_loop;
             END IF;

            dbms_output.put_line('FLT cap_rsv_rh_zona' || capacidad_reserva_zona_rango);
        END IF;


      VAR_FECHA_MINIMA2:=null;
      -- para las fechas programadas (rango dia = 1)
      VAR_FECHA_MINIMA:=var_fecha_entrega;

     --valida si se utiliza el calendario del curier

        /*
         IF tiempo_despacho >= courier_mayor_a  THEN
            dias_despacho_cc_despacha:= dias_courier;
         END IF;
          */
       array_dias_despacho.DELETE;

      -- condicion fechas programadas (dia a dia)
      IF var_rangoDias=0 THEN
      --var_diasDelta:=var_diasDelta * ;
          ctDia        :=0;

       --por un periodo de tiempo se valida si cada dia es posible despachar
        WHILE ctDia < var_diasDelta
        LOOP

           dia:=TRUNC(SYSDATE + ctDia + var_diasInicio);

          "sigla_dia":=  SUBSTR(dias_calendario, TO_CHAR(dia ,'D'),1) ;
          SELECT NVL(DECODE("sigla_dia",'L',capacidad_l_cc,'M',capacidad_m_cc,'W',capacidad_w_cc,'J',capacidad_j_cc,'V',capacidad_v_cc,'S',capacidad_s_cc,'D',capacidad_d_cc,capacidad_reserva_cc_real),capacidad_reserva_cc_real) INTO capacidad_reserva_cc FROM DUAL;

          --por defecto el dia es valido
          VAR_DIAS:=0;
          -- veo si el centro de costo despacha cada uno e los dias pertenecientes al rango



          -- veo el dia minimo de entreg

          IF var_es_peor_fecha=0 and var_fecha_entrega<=dia THEN
            var_es_peor_fecha:=1;
            VAR_FECHA_E:=dia;
          ELSIF var_es_peor_fecha=1 and var_fecha_entrega<=dia THEN
            VAR_FECHA_E:=VAR_FECHA_MINIMA;
          ELSE
            VAR_FECHA_E:=var_fecha_entrega;
          END IF;



          --  dbms_output.put_line( ' dias_calendario: ' || dias_calendario);
          --  dbms_output.put_line( ' dias_despacho_cc_despacha: ' || dias_despacho_cc_despacha);
          --  dbms_output.put_line( ' dias_despacho_zona: ' || dias_despacho_zona);


         dia:=SYSDATE + var_diasInicio + ctDia;

         -- VALIDO EL DIA DE ENTREGA PARA UN CENTRO DE COSTO QUE NO TENGRA RETIRO EN TIENDA
        IF NVL("cc",0) > 0 AND "tipoDespacho" <> 1  THEN

            IF tiempo_despacho >= courier_mayor_a THEN
                 -- dbms_output.put_line( '********* SE APLICA CONDICION COURIER *******' );
                  var_fecha_entrega_valida:=  fecha_entrega_courier(-1,  NVL("cc",0), 0, dias_courier, dias_calendario,dia);
                ELSE
                  var_fecha_entrega_valida :=  fecha_entrega(-1, NVL("cc",0),0 , dias_despacho_cc_despacha, dias_calendario, dia);
            END IF;


             IF TRUNC(dia)=TRUNC(var_fecha_entrega_valida) THEN

                IF TRUNC(var_fecha_entrega_valida) >= TRUNC(var_fecha_entrega) THEN

                  IF tiempo_despacho >= courier_mayor_a THEN
                     var_fecha_despacho_valida :=  fecha_entrega_courier(-1, NVL("cc",0),tiempo_despacho, dias_courier             , dias_calendario, var_fecha_entrega_valida);
                  ELSE
                      var_fecha_despacho_valida := fecha_entrega(-1,        NVL("cc",0),tiempo_despacho, dias_despacho_cc_despacha, dias_calendario, var_fecha_entrega_valida);
                  END IF;

                  IF TRUNC(var_fecha_despacho_valida) >= var_fecha_despacho THEN

                    var_fecha_despacho_valida:=  calcula_fecha_segun_cap_despa(-1,"canal_venta",capacidad_reserva_cc_real, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, NULL, NULL, NULL, NULL, NULL, NULL, NULL, capacidad_reserva_rango, NULL, NULL, NULL, NULL, NULL, NULL, NULL, capacidad_reserva_zona_rango, NULL, NULL, NULL, NULL, NULL, NULL, NULL, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, "cc", "comuna", var_cod_rangohora, var_fecha_despacho_valida);

                  BEGIN
                  IF array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')) IS NULL THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                  END IF;
                  EXCEPTION
                      WHEN NO_DATA_FOUND THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                  END;



                    IF TRUNC(var_fecha_despacho_valida) >= TRUNC(var_fecha_despacho) and array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD'))=1 THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 0;
                      var_dias:=1;
                    ELSE
                      var_dias:=0;
                    END IF;

                  ELSE
                    var_dias:=0;
                  END IF;

                ELSE
                  var_dias:=0;
                END IF;

            ELSE
             var_dias:=0;
            END IF;

            --CENTRO DE COSTO Y RETIRA EN TIENDA
          ELSIF NVL("cc",0) > 0 AND "tipoDespacho" = 1  THEN

            var_fecha_entrega_valida :=  fecha_entrega(-1, NVL("cc",0),0 , dias_despacho_cc_despacha, dias_calendario, dia);


           IF TRUNC(dia)=TRUNC(var_fecha_entrega_valida) THEN



               var_fecha_despacho_valida:=  calcula_fecha_segun_cap_retiro(-1,"canal_venta",capacidad_reserva_cc, NULL, NULL, NULL, NULL, NULL, NULL, NULL, capacidad_reserva_zona, NULL, NULL, NULL, NULL, NULL, NULL, NULL, capacidad_reserva_rango, NULL, NULL, NULL, NULL, NULL, NULL, NULL, capacidad_reserva_zona_rango, NULL, NULL, NULL, NULL, NULL, NULL, NULL, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, "cc", "comuna", var_cod_rangohora, var_fecha_entrega_valida);


                IF TRUNC(var_fecha_despacho_valida) >= var_fecha_despacho THEN

                BEGIN
                IF array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')) IS NULL THEN
                    array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                END IF;
                EXCEPTION
                    WHEN NO_DATA_FOUND THEN
                    array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                END;



                  IF TRUNC(var_fecha_despacho_valida) >= TRUNC(var_fecha_despacho) and array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD'))=1 THEN
                    array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 0;
                    var_dias:=1;
                  ELSE
                    var_dias:=0;
                  END IF;

                ELSE
                  var_dias:=0;
                END IF;


          ELSE
           var_dias:=0;
          END IF;
        -- DESPACHOS DE PROVEEDOR
          ELSE


                  var_fecha_entrega_valida :=  fecha_entrega(-1, NVL("cc",0),0 , dias_despacho_proveedor, dias_calendario, dia);


             IF TRUNC(dia)=TRUNC(var_fecha_entrega_valida) THEN




                IF TRUNC(var_fecha_entrega_valida) >= TRUNC(var_fecha_entrega) THEN


                      var_fecha_despacho_valida := fecha_entrega(-1,        NVL("cc",0),tiempo_despacho, dias_despacho_proveedor, dias_calendario, var_fecha_entrega_valida);


                  IF TRUNC(var_fecha_despacho_valida) >= var_fecha_despacho THEN


                  BEGIN
                  IF array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')) IS NULL THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                  END IF;
                  EXCEPTION
                      WHEN NO_DATA_FOUND THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                  END;



                    IF TRUNC(var_fecha_despacho_valida) >= TRUNC(var_fecha_despacho) and array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD'))=1 THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 0;
                      var_dias:=1;
                    ELSE
                      var_dias:=0;
                    END IF;

                  ELSE
                    var_dias:=0;
                  END IF;

                ELSE
                  var_dias:=0;
                END IF;

            ELSE
             var_dias:=0;
            END IF;

          END IF;



          IF VAR_DIAS=1 AND  var_fecha_entrega <= dia   THEN

          BEGIN
                INSERT
                INTO TMP_CALCULOFECHA
                  (
                    CCOSTO,
                    COD_SERVICIO,
                    COD_HORA,
                    HOY,
                    FECHA_DESDE,
                    FECHA_HASTA,
                    RANGO,
                    ES_RANGO,
                    ID
                  )
                  VALUES
                  (
                    var_centroCosto,
                    var_codigo,
                    var_cod_rangohora,
                    VAR_FECHA_E,
                    TRUNC(dia),
                    TRUNC(dia),
                    var_hora_ini || ' - ' || var_hora_fin,
                    var_rangoDias,
                    "varSeq"
                  );
              EXCEPTION
              WHEN DUP_VAL_ON_INDEX THEN
               null;
              END;

          ELSE


           -- si el servicio no es fijo ofrezco mas dias
              IF VAR_EXPRESS=0 THEN
                 var_diasDelta:=var_diasDelta+1;
              END IF;

            END IF;


              ctDia := ctDia +1;

        END LOOP;

        -- para un rango de dias NO PROGRAMADO (rangodias=0)
      ELSE

        VAR_DIAS_INI:=0;
        ctRep:=0;
        var_diasDelta2:=var_diasDelta;
       -- dia:=TRUNC(SYSDATE + ctDia + var_diasInicio);
       dia:=NULL;
        ctDia       :=0;

      if trunc(var_fecha_entrega)>=trunc(sysdate+ ctDia+var_diasInicio) then
          dia:=var_fecha_entrega;
        else
          dia:=trunc(sysdate+ ctDia+var_diasInicio);--SYSDATE + var_diasInicio;
       end if;

       var_diasInicio:=1;

      -- se valida el primer dia de entrega !!!!
      WHILE ctDia < var_diasInicio
            LOOP


              "sigla_dia":=  SUBSTR(dias_calendario, TO_CHAR( dia  ,'D'),1) ;
               SELECT NVL(DECODE("sigla_dia",'L',capacidad_l_cc,'M',capacidad_m_cc,'W',capacidad_w_cc,'J',capacidad_j_cc,'V',capacidad_v_cc,'S',capacidad_s_cc,'D',capacidad_d_cc,capacidad_reserva_cc_real),capacidad_reserva_cc_real) INTO capacidad_reserva_cc FROM DUAL;


             IF NVL("cc",0) > 0 AND "tipoDespacho" <> 1  THEN

            IF tiempo_despacho >= courier_mayor_a THEN
                 -- dbms_output.put_line( '********* SE APLICA CONDICION COURIER *******' );
                  var_fecha_entrega_valida:=  fecha_entrega_courier(-1,  NVL("cc",0), 0, dias_courier, dias_calendario,dia);
                ELSE
                  var_fecha_entrega_valida :=  fecha_entrega(-1, NVL("cc",0),0 , dias_despacho_cc_despacha, dias_calendario, dia);
            END IF;


             IF TRUNC(dia)=TRUNC(var_fecha_entrega_valida) THEN

                IF TRUNC(var_fecha_entrega_valida) >= TRUNC(var_fecha_entrega) THEN

                  IF tiempo_despacho >= courier_mayor_a THEN
                     var_fecha_despacho_valida :=  fecha_entrega_courier(-1, NVL("cc",0),tiempo_despacho, dias_courier             , dias_calendario, var_fecha_entrega_valida);
                  ELSE
                      var_fecha_despacho_valida := fecha_entrega(-1,        NVL("cc",0),tiempo_despacho, dias_despacho_cc_despacha, dias_calendario, var_fecha_entrega_valida);
                  END IF;

                  IF TRUNC(var_fecha_despacho_valida) >= var_fecha_despacho THEN

                    var_fecha_despacho_valida:=  calcula_fecha_segun_cap_despa(-1,"canal_venta",capacidad_reserva_cc_real, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, NULL, NULL, NULL, NULL, NULL, NULL, NULL, capacidad_reserva_rango, NULL, NULL, NULL, NULL, NULL, NULL, NULL, capacidad_reserva_zona_rango, NULL, NULL, NULL, NULL, NULL, NULL, NULL, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, "cc", "comuna", var_cod_rangohora, var_fecha_despacho_valida);

                  BEGIN
                  IF array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')) IS NULL THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                  END IF;
                  EXCEPTION
                      WHEN NO_DATA_FOUND THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                  END;



                    IF TRUNC(var_fecha_despacho_valida) >= TRUNC(var_fecha_despacho) and array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD'))=1 THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 0;
                      var_dias:=1;
                    ELSE
                      var_dias:=0;
                    END IF;

                  ELSE
                    var_dias:=0;
                  END IF;

                ELSE
                  var_dias:=0;
                END IF;

            ELSE
             var_dias:=0;
            END IF;

            --CENTRO DE COSTO Y RETIRA EN TIENDA
          ELSIF NVL("cc",0) > 0 AND "tipoDespacho" = 1  THEN

            var_fecha_entrega_valida :=  fecha_entrega(-1, NVL("cc",0),0 , dias_despacho_cc_despacha, dias_calendario, dia);


           IF TRUNC(dia)=TRUNC(var_fecha_entrega_valida) THEN

            var_fecha_despacho_valida:=  calcula_fecha_segun_cap_retiro(-1,"canal_venta",capacidad_reserva_cc, NULL, NULL, NULL, NULL, NULL, NULL, NULL, capacidad_reserva_zona, NULL, NULL, NULL, NULL, NULL, NULL, NULL, capacidad_reserva_rango, NULL, NULL, NULL, NULL, NULL, NULL, NULL, capacidad_reserva_zona_rango, NULL, NULL, NULL, NULL, NULL, NULL, NULL, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, "cc", "comuna", var_cod_rangohora, var_fecha_entrega_valida);


                IF TRUNC(var_fecha_despacho_valida) >= var_fecha_despacho THEN

                BEGIN
                IF array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')) IS NULL THEN
                    array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                END IF;
                EXCEPTION
                    WHEN NO_DATA_FOUND THEN
                    array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                END;



                  IF TRUNC(var_fecha_despacho_valida) >= TRUNC(var_fecha_despacho) and array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD'))=1 THEN
                    array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 0;
                    var_dias:=1;
                  ELSE
                    var_dias:=0;
                  END IF;

                ELSE
                  var_dias:=0;
                END IF;


          ELSE
           var_dias:=0;
          END IF;
        -- DESPACHOS DE PROVEEDOR
          ELSE


              var_fecha_entrega_valida :=  fecha_entrega(-1, NVL("cc",0),0 , dias_despacho_proveedor, dias_calendario, dia);


             IF TRUNC(dia)=TRUNC(var_fecha_entrega_valida) THEN



                IF TRUNC(var_fecha_entrega_valida) >= TRUNC(var_fecha_entrega) THEN


                   var_fecha_despacho_valida := fecha_entrega(-1,        NVL("cc",0),tiempo_despacho, dias_despacho_proveedor, dias_calendario, var_fecha_entrega_valida);


                  IF TRUNC(var_fecha_despacho_valida) >= var_fecha_despacho THEN


                  BEGIN
                  IF array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')) IS NULL THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                  END IF;
                  EXCEPTION
                      WHEN NO_DATA_FOUND THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                  END;



                    IF TRUNC(var_fecha_despacho_valida) >= TRUNC(var_fecha_despacho) and array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD'))=1 THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 0;
                      var_dias:=1;
                    ELSE
                      var_dias:=0;
                    END IF;

                  ELSE
                    var_dias:=0;
                  END IF;

                ELSE
                  var_dias:=0;
                END IF;

            ELSE
             var_dias:=0;
            END IF;

          END IF;

               dia:=dia+1;
             ctDia   := ctDia+1;

             IF var_dias = 0 THEN
                var_diasInicio:=var_diasInicio+1;
             END IF;
            END LOOP;

        -- le resto el ultimo dia que se le sumo
          dia:=dia-1;

          --dia   := trunc(sysdate+ var_diasInicio) ;
        while ctRep < var_repeticiones
        LOOP



            var_diasDelta:=var_diasDelta2;


            ctDia       :=1;
            VAR_FECHA_MINIMA:=null;
         ---     dia:=trunc(sysdate+var_diasInicio+ctDia);
            WHILE ctDia < var_diasDelta
            LOOP


              IF VAR_FECHA_MINIMA IS NULL  THEN
                VAR_FECHA_MINIMA:=dia;
              END IF;


              IF var_es_peor_fecha=0 and var_fecha_entrega<=dia THEN
                var_es_peor_fecha:=1;
                VAR_FECHA_E:=dia;
              ELSIF var_es_peor_fecha=1 and var_fecha_entrega<=dia THEN
                VAR_FECHA_E:=VAR_FECHA_MINIMA;
              ELSE
                VAR_FECHA_E:=var_fecha_entrega;
              END IF;


                dia   := dia+1;



               --OBTENGO LA CAPACIDAD DIARIA DE DESPACHOS EGUN EL DIA QUE ESTOY
               "sigla_dia":=  SUBSTR(dias_calendario, TO_CHAR(dia ,'D'),1) ;
               SELECT NVL(DECODE("sigla_dia",'L',capacidad_l_cc,'M',capacidad_m_cc,'W',capacidad_w_cc,'J',capacidad_j_cc,'V',capacidad_v_cc,'S',capacidad_s_cc,'D',capacidad_d_cc,capacidad_reserva_cc_real),capacidad_reserva_cc_real) INTO capacidad_reserva_cc FROM DUAL;


         -- VALIDO EL DIA DE ENTREGA PARA UN CENTRO DE COSTO QUE NO TENGRA RETIRO EN TIENDA
        IF NVL("cc",0) > 0 AND "tipoDespacho" <> 1  THEN

            IF tiempo_despacho >= courier_mayor_a THEN
                 -- dbms_output.put_line( '********* SE APLICA CONDICION COURIER *******' );
                  var_fecha_entrega_valida:=  fecha_entrega_courier(-1,  NVL("cc",0), 0, dias_courier, dias_calendario,dia);
                ELSE
                  var_fecha_entrega_valida :=  fecha_entrega(-1, NVL("cc",0),0 , dias_despacho_cc_despacha, dias_calendario, dia);
            END IF;


             IF TRUNC(dia)=TRUNC(var_fecha_entrega_valida) THEN

                IF TRUNC(var_fecha_entrega_valida) >= TRUNC(var_fecha_entrega) THEN

                  IF tiempo_despacho >= courier_mayor_a THEN
                     var_fecha_despacho_valida :=  fecha_entrega_courier(-1, NVL("cc",0),tiempo_despacho, dias_courier             , dias_calendario, var_fecha_entrega_valida);
                  ELSE
                      var_fecha_despacho_valida := fecha_entrega(-1,        NVL("cc",0),tiempo_despacho, dias_despacho_cc_despacha, dias_calendario, var_fecha_entrega_valida);
                  END IF;

                  IF TRUNC(var_fecha_despacho_valida) >= var_fecha_despacho THEN

                    var_fecha_despacho_valida:=  calcula_fecha_segun_cap_despa(-1,"canal_venta",capacidad_reserva_cc_real, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, NULL, NULL, NULL, NULL, NULL, NULL, NULL, capacidad_reserva_rango, NULL, NULL, NULL, NULL, NULL, NULL, NULL, capacidad_reserva_zona_rango, NULL, NULL, NULL, NULL, NULL, NULL, NULL, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, "cc", "comuna", var_cod_rangohora, var_fecha_despacho_valida);

                  BEGIN
                  IF array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')) IS NULL THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                  END IF;
                  EXCEPTION
                      WHEN NO_DATA_FOUND THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                  END;



                    IF TRUNC(var_fecha_despacho_valida) >= TRUNC(var_fecha_despacho) and array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD'))=1 THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 0;
                      var_dias:=1;
                    ELSE
                      var_dias:=0;
                    END IF;

                  ELSE
                    var_dias:=0;
                  END IF;

                ELSE
                  var_dias:=0;
                END IF;

            ELSE
             var_dias:=0;
            END IF;

            --CENTRO DE COSTO Y RETIRA EN TIENDA
          ELSIF NVL("cc",0) > 0 AND "tipoDespacho" = 1  THEN

            var_fecha_entrega_valida :=  fecha_entrega(-1, NVL("cc",0),0 , dias_despacho_cc_despacha, dias_calendario, dia);


           IF TRUNC(dia)=TRUNC(var_fecha_entrega_valida) THEN



               var_fecha_despacho_valida:=  calcula_fecha_segun_cap_retiro(-1,"canal_venta",capacidad_reserva_cc, NULL, NULL, NULL, NULL, NULL, NULL, NULL, capacidad_reserva_zona, NULL, NULL, NULL, NULL, NULL, NULL, NULL, capacidad_reserva_rango, NULL, NULL, NULL, NULL, NULL, NULL, NULL, capacidad_reserva_zona_rango, NULL, NULL, NULL, NULL, NULL, NULL, NULL, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, "cc", "comuna", var_cod_rangohora, var_fecha_entrega_valida);


                IF TRUNC(var_fecha_despacho_valida) >= var_fecha_despacho THEN

                BEGIN
                IF array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')) IS NULL THEN
                    array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                END IF;
                EXCEPTION
                    WHEN NO_DATA_FOUND THEN
                    array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                END;



                  IF TRUNC(var_fecha_despacho_valida) >= TRUNC(var_fecha_despacho) and array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD'))=1 THEN
                    array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 0;
                    var_dias:=1;
                  ELSE
                    var_dias:=0;
                  END IF;

                ELSE
                  var_dias:=0;
                END IF;


          ELSE
           var_dias:=0;
          END IF;
        -- DESPACHOS DE PROVEEDOR
          ELSE


              var_fecha_entrega_valida :=  fecha_entrega(-1, NVL("cc",0),0 , dias_despacho_proveedor, dias_calendario, dia);


             IF TRUNC(dia)=TRUNC(var_fecha_entrega_valida) THEN



                IF TRUNC(var_fecha_entrega_valida) >= TRUNC(var_fecha_entrega) THEN


                   var_fecha_despacho_valida := fecha_entrega(-1,        NVL("cc",0),tiempo_despacho, dias_despacho_proveedor, dias_calendario, var_fecha_entrega_valida);


                  IF TRUNC(var_fecha_despacho_valida) >= var_fecha_despacho THEN


                  BEGIN
                  IF array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')) IS NULL THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                  END IF;
                  EXCEPTION
                      WHEN NO_DATA_FOUND THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 1;
                  END;



                    IF TRUNC(var_fecha_despacho_valida) >= TRUNC(var_fecha_despacho) and array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD'))=1 THEN
                      array_dias_despacho(to_char(var_fecha_despacho_valida,'YYYYMMDD')):= 0;
                      var_dias:=1;
                    ELSE
                      var_dias:=0;
                    END IF;

                  ELSE
                    var_dias:=0;
                  END IF;

                ELSE
                  var_dias:=0;
                END IF;

            ELSE
             var_dias:=0;
            END IF;

          END IF;




         -- ctDia := ctDia +1;



             dbms_output.put_line( ' dias_calendario: ' || dias_calendario);
             dbms_output.put_line( ' dias_despacho_cc_despacha: ' || dias_despacho_cc_despacha);
             dbms_output.put_line( ' dias_despacho_zona: ' || dias_despacho_zona);


              -- valido si puedo despachar ese dia






              IF(VAR_DIAS =0 AND  VAR_EXPRESS = 0  ) THEN
              --  contador     :=contador     +1;
               var_diasDelta:=var_diasDelta+1;
               null;
              END IF;



               ctDia := ctDia +1;

            END LOOP;


           -- dia:=dia;
       /*
            VAR_FECHA_MAXIMA:=dia;--VAR_FECHA_MINIMA+var_diasDelta;


            IF VAR_FECHA_MINIMA2  IS NULL  THEN
               VAR_FECHA_MINIMA2:=VAR_FECHA_MINIMA;



            END IF;
*/

             BEGIN


                INSERT
                INTO TMP_CALCULOFECHA
                  (
                    CCOSTO,
                    COD_SERVICIO,
                    COD_HORA,
                    HOY,
                    FECHA_DESDE,
                    FECHA_HASTA,
                    RANGO,
                    ES_RANGO,
                    ID
                  )
                  VALUES
                  (
                    var_centroCosto,
                    var_codigo,
                    var_cod_rangohora,
                    VAR_FECHA_E,
                    TRUNC(VAR_FECHA_MINIMA),
                    TRUNC(dia),--VAR_FECHA_MAXIMA ,
                    var_hora_ini
                    || ' - '
                    || var_hora_fin,
                    var_rangoDias,
                    "varSeq"
                  );
             EXCEPTION
              WHEN DUP_VAL_ON_INDEX THEN
                NULL;
              END;
        --    END IF;

            ctRep:=ctRep+1;
         --   VAR_FECHA_MINIMA2:=VAR_FECHA_MAXIMA;
           -- var_diasDelta:=var_diasDelta +var_diasDelta;
          END LOOP;
      END IF;
     <<end_loop>>
      NULL;

    END LOOP;
    CLOSE "crServicios" ;
  END;
END servicios_flete_consultar;


  PROCEDURE regresion
  (
  var_es_RT             IN NUMBER,
  var_hoy               IN DATE,
  canalVenta            IN NUMBER,
  tipoStock             IN VARCHAR2,
  diasFabricacion       IN NUMBER,
  cenCosOrigen          IN NUMBER,
  cenCosDespacho        IN NUMBER,
  cc_origen_es_bodega   IN VARCHAR2,
  cc_despacha_es_bodega IN VARCHAR2,
--  var_fecha_despacho           IN  DATE,


  tiempo_proceso_proveedor       IN NUMBER,
  tiempo_viaje_proveedor         IN NUMBER,
  tiempo_despacho_proveedor_com  IN NUMBER,
  tiempo_despacho                IN NUMBER,
  tiempo_proceso_cc_origen       IN NUMBER,
  tiempo_proceso_cc_despacha     IN NUMBER,
  tiempo_viaje_origen_destino    IN NUMBER,

    capacidad_reserva_cc      IN  NUMBER,
    capacidad_l_cc            IN  NUMBER,
    capacidad_m_cc            IN  NUMBER,
    capacidad_w_cc            IN  NUMBER,
    capacidad_j_cc            IN  NUMBER,
    capacidad_v_cc            IN  NUMBER,
    capacidad_s_cc            IN  NUMBER,
    capacidad_d_cc            IN  NUMBER,
    capacidad_reserva_zona    IN  NUMBER,
    capacidad_l_zona          IN  NUMBER,
    capacidad_m_zona          IN  NUMBER,
    capacidad_w_zona          IN  NUMBER,
    capacidad_j_zona          IN  NUMBER,
    capacidad_v_zona          IN  NUMBER,
    capacidad_s_zona          IN  NUMBER,
    capacidad_d_zona          IN  NUMBER,
    capacidad_reserva_rango   IN  NUMBER,
    capacidad_l_rango         IN  NUMBER,
    capacidad_m_rango         IN  NUMBER,
    capacidad_w_rango         IN  NUMBER,
    capacidad_j_rango         IN  NUMBER,
    capacidad_v_rango         IN  NUMBER,
    capacidad_s_rango         IN  NUMBER,
    capacidad_d_rango         IN  NUMBER,
    capacidad_reserva_zona_rango  IN  NUMBER,
    capacidad_l_zona_rango    IN  NUMBER,
    capacidad_m_zona_rango    IN  NUMBER,
    capacidad_w_zona_rango    IN  NUMBER,
    capacidad_j_zona_rango    IN  NUMBER,
    capacidad_v_zona_rango    IN  NUMBER,
    capacidad_s_zona_rango    IN  NUMBER,
    capacidad_d_zona_rango    IN  NUMBER,

  dias_trabajo_proveedor         IN VARCHAR2,
  dias_despacho_proveedor        IN VARCHAR2,
  dias_despacho_cc_despacha      IN VARCHAR2,
  dias_despacho_zona             IN VARCHAR2,
  dias_trabajo_cc_orgigen        IN VARCHAR2,
  dias_despacho_cc_origen        IN VARCHAR2,

 dias_trabajo_cc_despacha       IN VARCHAR2,
  dias_viaje_transferencia       IN VARCHAR2,
  dias_despacho_transferencia    IN VARCHAR,
  dias_proceso_transferencia     IN VARCHAR2,
  dias_calendario                IN VARCHAR2,
  dias_courier                   IN VARCHAR2,
  courier_mayor_a                IN NUMBER,

  comunaDespacho                 IN VARCHAR2,
  rango                          IN VARCHAR2,
  var_es_fecha_entrega_valida    IN DATE,   --fecha a validar



   var_fecha_inicio_proceso OUT DATE,
   var_fecha_traslado       OUT date,
   var_fecha_despacho       OUT DATE,
   var_fecha_entrega        OUT DATE,



   var_id OUT NUMBER,
   var_glosa OUT VARCHAR2
  ) AS
  BEGIN

  DECLARE
  fechaEntrega DATE;
  dias_transferencia_cc   VARCHAR2(7);
  var_fecha_viaje_real DATE;

   var_fecha_inicio_proceso_tmp DATE;
  BEGIN
  dbms_output.put_line('***********************************' || var_es_fecha_entrega_valida ||'***********************************' );

 /* dbms_output.put_line( '********************************');
  dbms_output.put_line( '********************************');
  dbms_output.put_line( '********************************');
  dbms_output.put_line( '********************************');
  dbms_output.put_line( '********************************');
  dbms_output.put_line( '********************************');*/

         IF tipoStock = 'P' AND  NVL(cenCosDespacho,0)=0 THEN




          dbms_output.put_line('****************************** PRODUCTO TIPO PROVEEDOR ****************************' );


              IF NVL(cenCosDespacho,0)=0 THEN
                  dbms_output.put_line( 'var_es_fecha_entrega_valida= ' || TO_CHAR(var_es_fecha_entrega_valida,'DD/MM/YYYY'));


                 dbms_output.put_line('valido si la fecha de entrega es valida');
                  var_fecha_entrega :=  fecha_entrega(1,0 ,0,dias_despacho_proveedor,dias_calendario ,var_es_fecha_entrega_valida);
           --        dbms_output.put_line( 'var_fecha_entrega= ' || TO_CHAR(var_fecha_entrega,'DD/MM/YYYY'));
                  dbms_output.put_line('ME LLAMO A MI MISMO CON UN DIA MAS');
                  IF TRUNC(var_fecha_entrega) != TRUNC(var_es_fecha_entrega_valida) THEN

                    dbms_output.put_line('ME LLAMO A MI MISMO CON UN DIA MAS');
                     -- ME LLAMO A MI MISMO CON UN DIA MAS
                     regresion(var_es_RT,NVL(var_hoy,SYSDATE), canalVenta, tipoStock, diasFabricacion, cenCosOrigen, cenCosDespacho, cc_origen_es_bodega, cc_despacha_es_bodega, tiempo_proceso_proveedor, tiempo_viaje_proveedor, tiempo_despacho_proveedor_com, tiempo_despacho, tiempo_proceso_cc_origen, tiempo_proceso_cc_despacha,  tiempo_viaje_origen_destino, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_trabajo_proveedor, dias_despacho_proveedor, dias_despacho_cc_despacha, dias_despacho_zona, dias_trabajo_cc_orgigen, dias_despacho_cc_origen,dias_trabajo_cc_despacha, dias_viaje_transferencia, dias_despacho_transferencia, dias_proceso_transferencia, dias_calendario, dias_courier, courier_mayor_a, comunaDespacho, rango, var_es_fecha_entrega_valida+1, var_fecha_inicio_proceso, var_fecha_traslado, var_fecha_despacho, var_fecha_entrega, var_id, var_glosa);

                  END IF;




              --    dbms_output.put_line( 'var_fecha_entrega= ' || TO_CHAR(var_fecha_entrega,'DD/MM/YYYY'));

                  var_fecha_despacho :=  fecha_entrega(-1,tiempo_despacho_proveedor_com ,0,dias_despacho_proveedor,dias_calendario ,var_fecha_entrega);
              --    dbms_output.put_line( 'var_fecha_despacho= ' || TO_CHAR(var_fecha_despacho,'DD/MM/YYYY'));
              --    dbms_output.put_line( '*************** FECHA DE DESPACHO CALCULADA CON EXITO*****************');

                  var_fecha_inicio_proceso := fecha_inicio_proceso( -1, 0, 'P', diasFabricacion,tiempo_proceso_proveedor,0,dias_trabajo_proveedor,dias_trabajo_proveedor,dias_calendario,var_fecha_despacho) ;
                --  var_fecha_inicio_proceso := fecha_inicio_proceso( -1, 0, 'P', diasFabricacion,tiempo_proceso_proveedor,0,dias_trabajo_proveedor,dias_despacho_proveedor,dias_calendario,var_fecha_despacho) ;
           --      dbms_output.put_line( 'var_fecha_inicio_proceso= ' || TO_CHAR(var_fecha_inicio_proceso,'DD/MM/YYYY'));
              --    dbms_output.put_line( 'var_fecha_inicio_proceso= ' || TO_CHAR(var_fecha_inicio_proceso,'DD/MM/YYYY'));
             --     dbms_output.put_line( '*************** FECHA DE INICIO PROCESO CALCULADA CON EXITO*****************');

              IF trunc(var_fecha_inicio_proceso) >= TRUNC(var_hoy) THEN

            --  dbms_output.put_line( 'EXITO!!!! ' );
               RETURN;-- FECHA VALIDADA!!!
           --     dbms_output.put_line( 'ZZZZZZZZZZZZZZZZZZZ!!!! ' );
              ELSE
                dbms_output.put_line( 'FECHA PROPUESTA INVALIDA var_fecha_inicio_proceso MENOR A LA FECHA DE HOY ' );
                dbms_output.put_line('ME LLAMO A MI MISMO CON UN DIA MAS');
                regresion(var_es_RT,NVL(var_hoy,SYSDATE), canalVenta, tipoStock, diasFabricacion, cenCosOrigen, cenCosDespacho, cc_origen_es_bodega, cc_despacha_es_bodega, tiempo_proceso_proveedor, tiempo_viaje_proveedor, tiempo_despacho_proveedor_com, tiempo_despacho, tiempo_proceso_cc_origen, tiempo_proceso_cc_despacha,  tiempo_viaje_origen_destino, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_trabajo_proveedor, dias_despacho_proveedor, dias_despacho_cc_despacha, dias_despacho_zona, dias_trabajo_cc_orgigen, dias_despacho_cc_origen,dias_trabajo_cc_despacha, dias_viaje_transferencia, dias_despacho_transferencia, dias_proceso_transferencia, dias_calendario, dias_courier, courier_mayor_a, comunaDespacho, rango, var_es_fecha_entrega_valida+1, var_fecha_inicio_proceso, var_fecha_traslado, var_fecha_despacho, var_fecha_entrega, var_id, var_glosa);
              END IF;

              END IF;

              END IF;
              IF tipoStock = 'P' AND  NVL(cenCosDespacho,0)>0  THEN
              /***************************************************
              ***************************************************
              ******* ES PROVEEDOR PERO DESPACHA EL CC **********
              ***************************************************
              ***************************************************/

              dbms_output.put_line( '********* SE  PROVEEDOR PERO DESPACHA EL CC  *******' );
           if tiempo_despacho >= courier_mayor_a THEN

              dbms_output.put_line( '********* SE APLICA CONDICION COURIER *******' );
              dbms_output.put_line( 'dias_courier= ' ||dias_courier);
              var_fecha_entrega:=  fecha_entrega_courier(-1, cenCosDespacho, 0, dias_courier, dias_calendario, var_es_fecha_entrega_valida);

            ELSE
              var_fecha_entrega :=  fecha_entrega(-1,cenCosDespacho,0 , dias_despacho_cc_despacha, dias_calendario, var_es_fecha_entrega_valida);
            END IF;




               IF TRUNC(var_fecha_entrega) != TRUNC(var_es_fecha_entrega_valida) THEN
                   dbms_output.put_line('ME LLAMO A MI MISMO CON UN DIA MAS');
                   regresion(var_es_RT,NVL(var_hoy,SYSDATE), canalVenta, tipoStock, diasFabricacion, cenCosOrigen, cenCosDespacho, cc_origen_es_bodega, cc_despacha_es_bodega, tiempo_proceso_proveedor, tiempo_viaje_proveedor, tiempo_despacho_proveedor_com, tiempo_despacho, tiempo_proceso_cc_origen, tiempo_proceso_cc_despacha,  tiempo_viaje_origen_destino, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_trabajo_proveedor, dias_despacho_proveedor, dias_despacho_cc_despacha, dias_despacho_zona, dias_trabajo_cc_orgigen, dias_despacho_cc_origen,dias_trabajo_cc_despacha, dias_viaje_transferencia, dias_despacho_transferencia, dias_proceso_transferencia, dias_calendario, dias_courier, courier_mayor_a, comunaDespacho, rango, var_es_fecha_entrega_valida+1, var_fecha_inicio_proceso, var_fecha_traslado, var_fecha_despacho, var_fecha_entrega, var_id, var_glosa);
              END IF;
                 --dbms_output.put_line( 'var_fecha_entrega= ' || TO_CHAR(var_fecha_entrega,'DD/MM/YYYY'));


                 if tiempo_despacho >= courier_mayor_a THEN

              dbms_output.put_line( '********* SE APLICA CONDICION COURIER *******' );
              dbms_output.put_line( 'dias_courier= ' ||dias_courier);
              var_fecha_despacho:=  fecha_entrega_courier(-1,cenCosDespacho,tiempo_despacho , dias_courier, dias_calendario, var_fecha_entrega);

            ELSE
               var_fecha_despacho:=  fecha_entrega(-1,cenCosDespacho,tiempo_despacho , dias_despacho_cc_despacha, dias_calendario, var_fecha_entrega);
            END IF;



               --  dbms_output.put_line( 'var_fecha_despacho 1 = ' || TO_CHAR(var_fecha_despacho,'DD/MM/YYYY'));

                 IF  var_es_RT= 1 THEN
                    var_fecha_traslado:=  calcula_fecha_segun_cap_retiro(-1,canalVenta, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, cenCosDespacho, comunaDespacho, rango, var_fecha_despacho);
                 ELSE
                    var_fecha_traslado:=  calcula_fecha_segun_cap_despa(-1,canalVenta, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, cenCosDespacho, comunaDespacho, rango, var_fecha_despacho);
                 END IF;

                -- dbms_output.put_line( 'var_fecha_traslado 2= ' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));

            --     dbms_output.put_line( 'var_fecha_traslado 1= ' ||var_fecha_traslado);



              IF cenCosOrigen <> cenCosDespacho  and (NVL(cenCosOrigen,0) > 0 and NVL(cenCosDespacho,0) > 0) THEN


              --      dbms_output.put_line( '   IF cenCosOrigen <> cenCosDespacho  and (NVL(cenCosOrigen,0) > 0 and NVL(cenCosDespacho,0) > 0) THEN            ' );




              --MUEVO EL PRODUCTO DE LA BODEGA A TIENDA
               IF cc_origen_es_bodega ='T' AND cc_despacha_es_bodega='F' THEN

                dias_transferencia_cc:=restringo_calendario(dias_despacho_cc_origen,dias_viaje_transferencia);
                IF dias_transferencia_cc IS NULL THEN
                  var_id:=1;
                  var_glosa:='IMPOSIBLE Hacer una trasferencias valide Caldendario de trasnferencia (var_fecha_inicio_proceso) y CC '|| cenCosOrigen;
                  RETURN;
                END IF;


                    var_fecha_traslado:=    fecha_traslado  (-1,cenCosOrigen, tiempo_viaje_origen_destino, dias_transferencia_cc, dias_calendario,var_fecha_traslado);
                  --  dbms_output.put_line( 'var_fecha_traslado= ' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));

                 ELSE

                     var_fecha_traslado:=    fecha_traslado  (-1,cenCosOrigen,  tiempo_viaje_origen_destino , dias_despacho_cc_origen, dias_despacho_cc_origen,var_fecha_traslado);


                 END IF;






                      -- como es una trasferencia utilizo el calendario de trasferencias
                  --var_fecha_traslado:=    fecha_traslado  (-1,cenCosOrigen, tiempo_viaje_origen_destino, dias_despacho_transferencia, dias_calendario,var_fecha_traslado);

               --VEO QUE PIDA PUEDO SALIR A REALIZAR LA TRANFERENCIA
                 IF cc_origen_es_bodega ='T' AND cc_despacha_es_bodega='F' THEN

                    dias_transferencia_cc:=restringo_calendario(dias_despacho_cc_origen,dias_despacho_transferencia);
                    IF dias_transferencia_cc IS NULL THEN
                      var_id:=1;
                      var_glosa:='IMPOSIBLE Hacer una trasferencias valide Caldendario de trasnferencia (var_fecha_inicio_proceso) y CC '|| cenCosOrigen;
                      RETURN;
                    END IF;


                    var_fecha_traslado:=    fecha_traslado  (-1,cenCosOrigen, 0, dias_transferencia_cc, dias_calendario,var_fecha_traslado);
                   -- dbms_output.put_line( 'var_fecha_traslado= ' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));

                 ELSE

                    var_fecha_traslado:=    fecha_traslado  (-1,cenCosOrigen, 0, dias_despacho_cc_origen, dias_calendario,var_fecha_traslado);
                   -- dbms_output.put_line( 'var_fecha_traslado= ' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));


                 END IF;


                                     dbms_output.put_line( 'var_fecha_traslado 3= ' ||var_fecha_traslado);
                 --var_fecha_traslado:=    fecha_traslado  (-1,cenCosOrigen, tiempo_viaje_origen_destino, dias_transferencia, dias_calendario,var_fecha_traslado);









                 IF cc_origen_es_bodega ='T' AND cc_despacha_es_bodega='F' THEN

                    dias_transferencia_cc:=restringo_calendario(dias_trabajo_cc_despacha,dias_proceso_transferencia);
                    IF dias_transferencia_cc IS NULL THEN
                      var_id:=1;
                      var_glosa:='IMPOSIBLE Hacer una trasferencias valide Caldendario de trasnferencia (var_fecha_inicio_proceso) y CC '|| cenCosOrigen;
                      RETURN;
                    END IF;


                     var_fecha_traslado:=    fecha_traslado  (-1, cenCosDespacho, tiempo_proceso_cc_origen, dias_transferencia_cc, dias_calendario,var_fecha_traslado);
                   -- dbms_output.put_line( 'var_fecha_inicio_proceso= ' || TO_CHAR(var_fecha_inicio_proceso,'DD/MM/YYYY'));

                 ELSE

                  var_fecha_traslado:=    fecha_traslado  (-1, cenCosDespacho, tiempo_proceso_cc_origen, dias_trabajo_cc_despacha, dias_calendario,var_fecha_traslado);
                  --  dbms_output.put_line( 'var_fecha_inicio_proceso= ' || TO_CHAR(var_fecha_inicio_proceso,'DD/MM/YYYY'));
               -- var_fecha_inicio_proceso := fecha_inicio_proceso( -1,cenCosOrigen, 'P', diasFabricacion,0,0,dias_trabajo_cc_orgigen,dias_trabajo_cc_orgigen,dias_calendario,var_fecha_inicio_proceso) ;
           --    dbms_output.put_line( 'var_fecha_inicio_proceso STOCK= ' || TO_CHAR(var_fecha_inicio_proceso,'DD/MM/YYYY'));



                          END IF;
                    var_fecha_viaje_real:=var_fecha_traslado;

                   --   dbms_output.put_line( 'var_fecha_traslado_Fin= ' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));
                  END IF;


                IF NVL(cenCosDespacho,0) > 0 THEN


                          var_fecha_traslado:=    fecha_traslado  (-1, cenCosDespacho, tiempo_proceso_cc_Despacha, dias_trabajo_cc_despacha, dias_calendario,var_fecha_traslado);




                ELSE

                  null;
               --    var_fecha_inicio_proceso:=    fecha_traslado  (-1, cenCosDespacho, tiempo_proceso_cc_origen, dias_transferencia_cc, dias_calendario,var_fecha_inicio_proceso);


                END IF;


                var_fecha_inicio_proceso     := fecha_inicio_proceso( -1,cenCosDespacho, 'P', 0                             , 0                       ,0                     ,dias_trabajo_proveedor,dias_despacho_proveedor,dias_calendario,var_fecha_traslado) ;
                var_fecha_inicio_proceso_tmp := fecha_inicio_proceso( -1,cenCosDespacho, 'P', diasFabricacion               , tiempo_proceso_proveedor,tiempo_viaje_proveedor,dias_trabajo_proveedor,dias_despacho_proveedor,dias_calendario,var_fecha_inicio_proceso) ;


                 /*       var_fecha_inicio_proceso     := fecha_inicio_proceso( -1,cenCosDespacho, 'P', 0              ,diasFabricacion,0,dias_trabajo_proveedor,dias_despacho_proveedor,dias_calendario,var_fecha_traslado) ;
                var_fecha_inicio_proceso_tmp := fecha_inicio_proceso( -1,cenCosDespacho, 'P', diasFabricacion,tiempo_proceso_proveedor,tiempo_viaje_proveedor,dias_trabajo_proveedor,dias_despacho_proveedor,dias_calendario,var_fecha_inicio_proceso) ;
                */
                var_fecha_traslado:=var_fecha_viaje_real;


                -- FECHA VALIDA
                IF trunc(var_fecha_inicio_proceso_tmp) >= TRUNC(var_hoy) THEN
                RETURN;--  GOTO end_loop;
                ELSE
               -- dbms_output.put_line( 'FECHA PROPUESTA INVALIDA var_fecha_inicio_proceso MENOR A LA FECHA DE HOY ' );
                dbms_output.put_line('ME LLAMO A MI MISMO CON UN DIA MAS');
                regresion(var_es_RT,NVL(var_hoy,SYSDATE), canalVenta, tipoStock, diasFabricacion, cenCosOrigen, cenCosDespacho, cc_origen_es_bodega, cc_despacha_es_bodega, tiempo_proceso_proveedor, tiempo_viaje_proveedor, tiempo_despacho_proveedor_com, tiempo_despacho, tiempo_proceso_cc_origen, tiempo_proceso_cc_despacha,  tiempo_viaje_origen_destino, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_trabajo_proveedor, dias_despacho_proveedor, dias_despacho_cc_despacha, dias_despacho_zona, dias_trabajo_cc_orgigen, dias_despacho_cc_origen,dias_trabajo_cc_despacha, dias_viaje_transferencia, dias_despacho_transferencia, dias_proceso_transferencia, dias_calendario, dias_courier, courier_mayor_a, comunaDespacho, rango, var_es_fecha_entrega_valida+1, var_fecha_inicio_proceso, var_fecha_traslado, var_fecha_despacho, var_fecha_entrega, var_id, var_glosa);

                END IF;

              END IF;
         IF tipoStock='S' THEN

            dbms_output.put_line('****************************** PRODUCTO TIPO STOCK ****************************' );
         /**********************************************************
         ***********************************************************
         **** VALIDA FECHA DE DESPACHO PARA PRODUCTOS DE STOCK *****
         ***********************************************************
         **********************************************************/

             -- se valida si es posible entregar
             IF tiempo_despacho >= courier_mayor_a  THEN

              dbms_output.put_line( '********* SE APLICA CONDICION COURIER *******' );
          --    dbms_output.put_line( 'dias_courier= ' ||dias_courier);
               var_fecha_entrega:=  fecha_entrega_courier(-1,  cenCosDespacho, 0,dias_courier, dias_calendario,  var_es_fecha_entrega_valida);
              -- calcula_fecha_segun_capacidad(-1,canalVenta, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, cenCosDespacho, comunaDespacho, rango,  var_es_fecha_entrega_valida);
              --fecha_entrega(-1,  cenCosDespacho, 0,dias_courier, dias_calendario,  fechaEntrega);

            ELSE
              dbms_output.put_line( 'SE APLICA CONDICION NORMAL '  );
               var_fecha_entrega:=  fecha_entrega(-1,  cenCosDespacho, 0, dias_despacho_cc_despacha /*dias_courier*/, dias_calendario,  var_es_fecha_entrega_valida);

            --  var_fecha_entrega:=  calcula_fecha_segun_capacidad(-1,canalVenta, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, cenCosDespacho, comunaDespacho, rango,  var_es_fecha_entrega_valida);

            END IF;


            -- SE VALIDA SI LA FECHA DE ENTREGA ES VALIDA ES VALIDA
            IF TRUNC(var_fecha_entrega) <> TRUNC(var_es_fecha_entrega_valida) THEN
                dbms_output.put_line( 'FECHA PROPUESTA INVALIDA' );
                dbms_output.put_line('ME LLAMO A MI MISMO CON UN DIA MAS');
                regresion(var_es_RT,NVL(var_hoy,SYSDATE), canalVenta, tipoStock, diasFabricacion, cenCosOrigen, cenCosDespacho, cc_origen_es_bodega, cc_despacha_es_bodega, tiempo_proceso_proveedor, tiempo_viaje_proveedor, tiempo_despacho_proveedor_com, tiempo_despacho, tiempo_proceso_cc_origen,tiempo_proceso_cc_despacha, tiempo_viaje_origen_destino, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_trabajo_proveedor, dias_despacho_proveedor, dias_despacho_cc_despacha, dias_despacho_zona, dias_trabajo_cc_orgigen, dias_despacho_cc_origen,dias_trabajo_cc_despacha, dias_viaje_transferencia, dias_despacho_transferencia, dias_proceso_transferencia, dias_calendario, dias_courier, courier_mayor_a, comunaDespacho, rango, var_es_fecha_entrega_valida+1, var_fecha_inicio_proceso, var_fecha_traslado, var_fecha_despacho, var_fecha_entrega, var_id, var_glosa);

            END IF;


            -- SE VALIDA SI ES POSIBLE DESPACHAR
            IF tiempo_despacho >= courier_mayor_a  THEN

              dbms_output.put_line( '********* SE APLICA CONDICION COURIER *******' );
          --    dbms_output.put_line( 'dias_courier= ' ||dias_courier);
              var_fecha_despacho:=  fecha_entrega_courier(-1,  cenCosDespacho, tiempo_despacho,dias_courier, dias_calendario,  var_fecha_entrega);

            ELSE
              dbms_output.put_line( 'SE APLICA CONDICION NORMAL '  );
              var_fecha_despacho:=  fecha_entrega(-1,  cenCosDespacho, tiempo_despacho,dias_despacho_cc_despacha, dias_calendario,  var_fecha_entrega);

            END IF;
           -- dbms_output.put_line( 'var_fecha_entrega= ' || TO_CHAR(var_fecha_entrega,'DD/MM/YYYY'));





               --  dbms_output.put_line( 'var_fecha_despacho= ' || TO_CHAR(var_fecha_despacho,'DD/MM/YYYY'));
                IF var_es_RT=1 THEN
                 var_fecha_traslado:=  calcula_fecha_segun_cap_retiro(-1,canalVenta, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, cenCosDespacho, comunaDespacho, rango, var_fecha_despacho);
                ELSE
                  var_fecha_traslado:=  calcula_fecha_segun_cap_despa(-1,canalVenta, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_despacho_cc_despacha, dias_despacho_zona, dias_calendario, cenCosDespacho, comunaDespacho, rango, var_fecha_despacho);
                END IF;
               --  dbms_output.put_line( 'var_fecha_traslado= ' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));

                  IF cenCosOrigen <> cenCosDespacho THEN
                  --var_fecha_traslado:=    fecha_traslado  (-1,cenCosOrigen, tiempo_viaje_origen_destino,     dias_transferencia_cc, dias_calendario,var_fecha_traslado);
                  var_fecha_traslado:=    fecha_traslado  (-1,cenCosDespacho, tiempo_proceso_cc_despacha,     dias_trabajo_cc_despacha, dias_calendario,var_fecha_traslado);


                    IF cc_origen_es_bodega ='T' AND cc_despacha_es_bodega='F' THEN

                    dias_transferencia_cc:=restringo_calendario(dias_despacho_cc_origen,dias_viaje_transferencia);
                    IF dias_transferencia_cc IS NULL THEN
                      var_id:=1;
                      var_glosa:='IMPOSIBLE Hacer una trasferencias valide Caldendario de trasnferencia (dias_viaje_transferencia) y CC '|| cenCosOrigen;
                      RETURN;
                    END IF;


                    var_fecha_traslado:=    fecha_traslado  (-1,cenCosOrigen, tiempo_viaje_origen_destino,     dias_transferencia_cc, dias_calendario,var_fecha_traslado);
                  --  dbms_output.put_line( 'var_fecha_traslado= ' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));

                 ELSE

                     var_fecha_traslado:=    fecha_traslado  (-1,cenCosOrigen, tiempo_viaje_origen_destino , dias_despacho_cc_origen, dias_calendario,var_fecha_traslado);
                  --    dbms_output.put_line( 'var_fecha_traslado_Inicio= ' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));


                 END IF;


                 --valido cuando salgo a despachar
                  IF cc_origen_es_bodega ='T' AND cc_despacha_es_bodega='F' THEN

                    dias_transferencia_cc:=restringo_calendario(dias_despacho_cc_origen,dias_despacho_transferencia);
                    IF dias_transferencia_cc IS NULL THEN
                      var_id:=1;
                      var_glosa:='IMPOSIBLE Hacer una trasferencias valide Caldendario de trasnferencia (var_fecha_inicio_proceso) y CC '|| cenCosOrigen;
                      RETURN;
                    END IF;


                    var_fecha_traslado:=    fecha_traslado  (-1,cenCosOrigen, 0, dias_transferencia_cc, dias_calendario,var_fecha_traslado);
                   -- dbms_output.put_line( 'var_fecha_traslado= ' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));

                 ELSE

                    var_fecha_traslado:=    fecha_traslado  (-1,cenCosOrigen, 0, dias_despacho_cc_origen, dias_calendario,var_fecha_traslado);
                   -- dbms_output.put_line( 'var_fecha_traslado= ' || TO_CHAR(var_fecha_traslado,'DD/MM/YYYY'));


                 END IF;


                   var_fecha_viaje_real:=var_fecha_traslado;

                  END IF;


                  IF nvl(cenCosOrigen,0) > 0 THEN


                   IF cc_origen_es_bodega ='T' AND cc_despacha_es_bodega='F' THEN

                    dias_transferencia_cc:=restringo_calendario(dias_trabajo_cc_orgigen,dias_proceso_transferencia);
                    IF dias_transferencia_cc IS NULL THEN
                      var_id:=1;
                      var_glosa:='IMPOSIBLE Hacer una trasferencias valide Caldendario de trasnferencia (var_fecha_inicio_proceso) y CC '|| cenCosOrigen;
                      RETURN;
                    END IF;


                     var_fecha_traslado:=    fecha_traslado  (-1, cenCosDespacho, tiempo_proceso_cc_origen, dias_transferencia_cc, dias_calendario,var_fecha_traslado);
                   -- dbms_output.put_line( 'var_fecha_inicio_proceso= ' || TO_CHAR(var_fecha_inicio_proceso,'DD/MM/YYYY'));

                 ELSE

                    var_fecha_traslado:=    fecha_traslado  (-1, cenCosDespacho, tiempo_proceso_cc_origen, dias_trabajo_cc_orgigen, dias_calendario,var_fecha_traslado);
                  --  dbms_output.put_line( 'var_fecha_inicio_proceso= ' || TO_CHAR(var_fecha_inicio_proceso,'DD/MM/YYYY'));


                  END IF;


              --      var_fecha_inicio_proceso:=    fecha_traslado  (-1,cenCosOrigen, tiempo_proceso_cc_origen,dias_trabajo_cc_orgigen/* dias_despacho_cc_origen*/, dias_calendario,var_fecha_traslado);
                --    dbms_output.put_line( 'var_fecha_inicio_proceso= ' || TO_CHAR(var_fecha_inicio_proceso,'DD/MM/YYYY'));
                  END IF;

                      var_fecha_inicio_proceso     := fecha_inicio_proceso( -1,cenCosDespacho, 'S', diasFabricacion, 0                       ,0,dias_trabajo_cc_orgigen,dias_trabajo_cc_orgigen,dias_calendario, var_fecha_traslado) ;
                    --  var_fecha_inicio_proceso_tmp := fecha_inicio_proceso( -1,cenCosDespacho, 'S', diasFabricacion, tiempo_proceso_cc_origen,0,dias_trabajo_cc_orgigen,dias_trabajo_cc_orgigen,dias_calendario, var_fecha_inicio_proceso) ;
                  var_fecha_traslado:=var_fecha_viaje_real;
             IF  trunc(var_fecha_inicio_proceso) >= TRUNC(var_hoy) THEN
                RETURN;--  GOTO end_loop;
                ELSE
                dbms_output.put_line( 'FECHA PROPUESTA INVALIDA var_fecha_inicio_proceso MENOR A LA FECHA DE HOY ' );
                dbms_output.put_line('ME LLAMO A MI MISMO CON UN DIA MAS');
                regresion(var_es_RT,NVL(var_hoy,SYSDATE), canalVenta, tipoStock, diasFabricacion, cenCosOrigen, cenCosDespacho, cc_origen_es_bodega, cc_despacha_es_bodega, tiempo_proceso_proveedor, tiempo_viaje_proveedor, tiempo_despacho_proveedor_com, tiempo_despacho, tiempo_proceso_cc_origen,tiempo_proceso_cc_despacha, tiempo_viaje_origen_destino, capacidad_reserva_cc, capacidad_l_cc, capacidad_m_cc, capacidad_w_cc, capacidad_j_cc, capacidad_v_cc, capacidad_s_cc, capacidad_d_cc, capacidad_reserva_zona, capacidad_l_zona, capacidad_m_zona, capacidad_w_zona, capacidad_j_zona, capacidad_v_zona, capacidad_s_zona, capacidad_d_zona, capacidad_reserva_rango, capacidad_l_rango, capacidad_m_rango, capacidad_w_rango, capacidad_j_rango, capacidad_v_rango, capacidad_s_rango, capacidad_d_rango, capacidad_reserva_zona_rango, capacidad_l_zona_rango, capacidad_m_zona_rango, capacidad_w_zona_rango, capacidad_j_zona_rango, capacidad_v_zona_rango, capacidad_s_zona_rango, capacidad_d_zona_rango, dias_trabajo_proveedor, dias_despacho_proveedor, dias_despacho_cc_despacha, dias_despacho_zona, dias_trabajo_cc_orgigen, dias_despacho_cc_origen,dias_trabajo_cc_despacha, dias_viaje_transferencia, dias_despacho_transferencia, dias_proceso_transferencia, dias_calendario, dias_courier, courier_mayor_a, comunaDespacho, rango, var_es_fecha_entrega_valida+1, var_fecha_inicio_proceso, var_fecha_traslado, var_fecha_despacho, var_fecha_entrega, var_id, var_glosa);

                END IF;
         end if;
         NULL;
      END;
  END regresion;

END CALCULO_FECHA;


/
