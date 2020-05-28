*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/BC_SCREEN_INIT
*&---------------------------------------------------------------------*

  CREATE OBJECT gr_app.
  CREATE OBJECT gr_tool EXPORTING i_tool = gr_app.

  /mbtools/cl_screen=>init(
    EXPORTING
      ir_tool     = gr_tool
    IMPORTING
      e_text      = scr_t001
      e_about     = scr_tab9
      e_title     = scr_t900
      e_version   = scr_t901
      e_copyright = scr_t902
      e_docu      = b_docu
      e_tool      = b_tool
      e_home      = b_home ).
