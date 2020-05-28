*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/BC_SCREEN_PBO_PAI
*&---------------------------------------------------------------------*

MODULE pbo_100 OUTPUT.

  SET PF-STATUS 'MAIN'.
  SET TITLEBAR  'MAIN'.

  /mbtools/cl_screen=>banner( i_show = abap_false ).

  gr_app->pbo( ).

ENDMODULE.                 " PBO_0100  OUTPUT

MODULE pai_100 INPUT.

  gr_app->pai( i_ok_code = g_ok_code ).

  CLEAR g_ok_code.

ENDMODULE.                 " PAI_0100  INPUT
