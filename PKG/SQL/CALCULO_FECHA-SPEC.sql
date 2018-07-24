--------------------------------------------------------
--  File created - Thursday-June-21-2018   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Package CALCULO_FECHA
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE "DADCORPDS"."CALCULO_FECHA" AS

  var_centro_costo NUMBER;
  var_canal_venta  NUMBER;

     FUNCTION restringo_calendario
  (
    "dias_cc"             IN  VARCHAR2,
    "dias_transeferencia" IN  VARCHAR2
  ) RETURN  VARCHAR2;

  PROCEDURE fecha_calcular
  (
  In_accion           IN  NUMBER,
  in_process_luw      IN  VARCHAR2 DEFAULT 'T',
  in_xml              IN  XMLTYPE,
  "out_cursor_fecha"  out sys_refcursor,
  "out_statuscode"    OUT NUMBER,
  "out_statusmsg"     OUT VARCHAR2
  );


   FUNCTION valida_dia
  (
    "dias_trabajo"          IN  VARCHAR2,
    "dias_calendario"       IN  VARCHAR2,
    "cc"                    IN  NUMBER,
    "dia"                   IN  DATE
  ) RETURN  NUMBER;


  FUNCTION  valida_capacidad_despacho
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
  ) RETURN  NUMBER;


  FUNCTION  valida_capacidad_retiro
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
  ) RETURN  NUMBER;

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
  ) RETURN  DATE;

     FUNCTION  calcula_fecha_valida
  (
    "direccion"                 IN  NUMBER, -- DIRECCION DEL TIEMPO [ 1 --> FUTURO.   -1 --> PASADO (REGRESION)]
    "cc"                        IN  NUMBER,
    "dias_proceso"              IN  NUMBER,
    "dias_trabajo"              IN  VARCHAR2,
    "dias_calendario"           IN  VARCHAR2,
    "dia"                       IN DATE
  ) RETURN  DATE;


     FUNCTION  fecha_traslado
  (
    "accion"                    IN  NUMBER, -- DIRECCION DEL TIEMPO [ 1 --> FUTURO.   -1 --> PASADO (REGRESION)]
    "cc"                        IN  NUMBER,
    "dias_proceso"              IN  NUMBER,
    "dias_trabajo"              IN  VARCHAR2,
    "dias_calendario"           IN  VARCHAR2,
    "dia"                       IN DATE
  ) RETURN  DATE;


     FUNCTION  fecha_entrega
  (
    "accion"                    IN  NUMBER, -- DIRECCION DEL TIEMPO [ 1 --> FUTURO.   -1 --> PASADO (REGRESION)]
    "cc"                        IN  NUMBER,
    "dias_envio"                IN  NUMBER,
    "dias_despacho"             IN  VARCHAR2,
    "dias_calendario"           IN  VARCHAR2,
    "dia"                       IN DATE
  ) RETURN  DATE;


       FUNCTION  fecha_entrega_courier
  (
    "accion"                    IN  NUMBER, -- DIRECCION DEL TIEMPO [ 1 --> FUTURO.   -1 --> PASADO (REGRESION)]
    "cc"                        IN  NUMBER,
    "dias_envio"                IN  NUMBER,
    "dias_despacho"             IN  VARCHAR2,
    "dias_calendario"           IN  VARCHAR2,
    "dia"                       IN DATE
  ) RETURN  DATE;


   FUNCTION  calcula_fecha_segun_cap_despa
  (
    "accion"                    IN  NUMBER, -- 1 CAPACIDAD DIARIA, 2 CAPACIDAD DIARIA POR COMUNA, 3 CAPACIDAD DIARIA POR RANGO
    "canal_venta"               IN  NUMBER,
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
  ) RETURN  DATE;



   FUNCTION  calcula_fecha_segun_cap_retiro
  (
    "accion"                    IN  NUMBER, -- 1 CAPACIDAD DIARIA, 2 CAPACIDAD DIARIA POR COMUNA, 3 CAPACIDAD DIARIA POR RANGO
    "canal_venta"               IN  NUMBER,
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
  ) RETURN  DATE;



     PROCEDURE servicios_flete_consultar(
    var_fecha_entrega     IN DATE,
    var_fecha_despacho    IN DATE,
    "cc"                  IN NUMBER,
    "varSeq"              IN VARCHAR2,
    "tipoDespacho"        IN NUMBER,
    "codProveedor"        IN NUMBER,
    "comuna"              IN VARCHAR2,
    "canal_venta"         IN NUMBER);

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
  );

END CALCULO_FECHA;


/
