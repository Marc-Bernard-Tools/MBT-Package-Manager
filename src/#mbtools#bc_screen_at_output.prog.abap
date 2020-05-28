*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/BC_SCREEN_AT_OUTPUT
*&---------------------------------------------------------------------*

  /mbtools/cl_screen=>banner(
    i_tool = gr_tool->get_id( )
    i_top  = 4
    i_left = 20 ).

  gr_app->screen( ).
