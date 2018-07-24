--------------------------------------------------------
--  File created - Thursday-June-21-2018   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Package GESTYPES
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE "DADCORPDS"."GESTYPES" 
as
  type "HOJARUTAENCRS" is ref cursor return "HOJARUTAENCVIEW"%rowtype;
  type "HOJARUTARS" is ref cursor return "HOJARUTAVIEW"%rowtype;
  type "HOJARUTARS2" is ref cursor return "HOJARUTAVIEW2"%rowtype;
  type "HOJARUTADETRS" is ref cursor return "HOJARUTADETVIEW"%rowtype;
  type "ESTADOSDETRS" is ref cursor return "ESTADOSDETVIEW"%rowtype;
  type "MUELLEDADRS"  is ref cursor return "MUELLEDADVIEW"%rowtype;
  type "MOTIVODETRS" is ref cursor return "MOTIVODEVVIEW"%rowtype;
  type "HOJARUTARENDICIONRS" is ref cursor return "HOJARUTARENDICIONVIEW"%rowtype;
  type "MUELLEDADESTADODETRS" is ref cursor return "MUELLEDADESTADOSDETVIEW"%rowtype;
  type "RESERVATRANSFERENCIADTLRS" is ref cursor return "RESERVATRANSFERENCIADTLVIEW"%rowtype;
  type "IMPRESORASRS" is ref cursor return "IMPRESORASVIEW"%rowtype;
end;


/
