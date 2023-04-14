CLASS /mbtools/cx_logger_disp_prof DEFINITION
  PUBLIC
  INHERITING FROM /mbtools/cx_logger
  CREATE PUBLIC.

************************************************************************
* abap logger
*
* Copyright 2017 Eric Peterson <https://github.com/ABAP-Logger/ABAP-Logger>
* SPDX-License-Identifier: MIT
************************************************************************

  PUBLIC SECTION.
    CONSTANTS zcx_logger_display_profile TYPE sotr_conc VALUE 'B9D98DB24EAF1EDD8ED3241224D60A6A' ##NO_TEXT.

    METHODS constructor
    IMPORTING
      textid LIKE textid OPTIONAL
      previous LIKE previous OPTIONAL
      info TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cx_logger_disp_prof IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        textid = textid
        previous = previous
        info = info.
    IF textid IS INITIAL.
      me->textid = zcx_logger_display_profile.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
