*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/BC_SCREEN_PBO_PAI
*&---------------------------------------------------------------------*

MODULE pbo_100 OUTPUT.

  SET PF-STATUS 'MAIN'.
  SET TITLEBAR  'MAIN'.

  /mbtools/cl_screen=>banner( iv_show = abap_false ).

  go_app->pbo( ).

ENDMODULE.                 " PBO_0100  OUTPUT

MODULE pai_100 INPUT.

  go_app->pai( iv_ok_code = gv_ok_code ).

  CLEAR gv_ok_code.

ENDMODULE.                 " PAI_0100  INPUT
