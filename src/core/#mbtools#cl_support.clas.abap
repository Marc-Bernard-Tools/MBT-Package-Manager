CLASS /mbtools/cl_support DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Marc Bernard Tools - Support
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************
  PUBLIC SECTION.

    METHODS initialize
      IMPORTING
        !iv_all_tools   TYPE abap_bool
        !iv_all_bundles TYPE abap_bool
        !iv_all_passes  TYPE abap_bool.
    METHODS screen.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_all_tools TYPE abap_bool.
    DATA mv_all_bundles TYPE abap_bool.
    DATA mv_all_passes TYPE abap_bool.
ENDCLASS.



CLASS /mbtools/cl_support IMPLEMENTATION.


  METHOD initialize.

    mv_all_tools = iv_all_tools.
    mv_all_bundles = iv_all_bundles.
    mv_all_passes = iv_all_passes.

  ENDMETHOD.


  METHOD screen.

    DATA lv_show TYPE abap_bool.

    LOOP AT SCREEN.
      lv_show = abap_true.

      IF screen-name = 'P_TITLE' AND
        ( mv_all_tools = abap_true OR mv_all_bundles = abap_true OR mv_all_passes = abap_true ).
        lv_show = abap_false.
      ELSEIF ( mv_all_bundles = abap_true OR mv_all_passes = abap_true ) AND
        ( screen-name = 'P_ACT' OR screen-name = 'P_DEACT' OR
          screen-name = 'P_ADDONL' OR screen-name = 'P_ADDOFF' OR
          screen-name = 'P_REMOVE' OR
          screen-name = 'P_CHECK' OR screen-name = 'P_UPDATE' OR
          screen-name = 'P_UNINST' ).
        lv_show = abap_false.
      ENDIF.

      IF lv_show = abap_true.
        screen-input = '1'.
      ELSE.
        screen-input = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
