--------------------------------------------------------
--  File created - Thursday-June-21-2018   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Package BPMTYPES
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE "DADCORPDS"."BPMTYPES" 
as
  type "RESERVABPMRS" is ref cursor return "RESERVABPMVIEW"%rowtype;
  type "RESERVADETBPMRS" is ref cursor return "RESERVADETBPMVIEW_b"%rowtype;
end;


/
