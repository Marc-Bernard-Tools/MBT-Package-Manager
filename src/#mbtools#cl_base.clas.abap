************************************************************************
* /MBTOOLS/CL_BASE
* MBT Base
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_base DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS initialize
      IMPORTING
        !iv_all_tools   TYPE abap_bool
        !iv_all_bundles TYPE abap_bool .
    METHODS screen .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_all_tools TYPE abap_bool .
    DATA mv_all_bundles TYPE abap_bool .
ENDCLASS.



CLASS /MBTOOLS/CL_BASE IMPLEMENTATION.


  METHOD initialize.

    mv_all_tools = iv_all_tools.
    mv_all_bundles = iv_all_bundles.

  ENDMETHOD.


  METHOD screen.

    DATA lv_show TYPE abap_bool.

    LOOP AT SCREEN.
      lv_show = abap_true.

      IF screen-name = 'P_TITLE' AND
        ( mv_all_tools = abap_true OR mv_all_bundles = abap_true ).
        lv_show = abap_false.
      ELSEIF screen-name = 'P_ACT' AND mv_all_bundles = abap_true.
        lv_show = abap_false.
      ELSEIF screen-name = 'P_DEACT' AND mv_all_bundles = abap_true.
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
