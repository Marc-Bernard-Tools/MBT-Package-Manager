*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/BC_SCREEN_AT_OUTPUT
*&---------------------------------------------------------------------*

/mbtools/cl_screen=>banner(
  iv_tool = go_tool->get_id( )
  iv_top  = 4
  iv_left = 20 ).

go_app->screen( ).
