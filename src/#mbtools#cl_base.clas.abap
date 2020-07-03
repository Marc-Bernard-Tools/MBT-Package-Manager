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
        !iv_all TYPE abap_bool .
    METHODS screen .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_all TYPE abap_bool .
ENDCLASS.



CLASS /MBTOOLS/CL_BASE IMPLEMENTATION.


  METHOD initialize.

    mv_all = iv_all.

  ENDMETHOD.


  METHOD screen.

    DATA lv_show TYPE abap_bool.

    LOOP AT SCREEN.
      lv_show = abap_true.

      IF screen-name = 'P_TITLE' AND mv_all = abap_true.
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
