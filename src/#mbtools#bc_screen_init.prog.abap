*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/BC_SCREEN_INIT
*&---------------------------------------------------------------------*

CREATE OBJECT go_app.
CREATE OBJECT go_tool EXPORTING io_tool = go_app.

/mbtools/cl_screen=>init(
  EXPORTING
    ir_tool      = go_tool
  IMPORTING
    ev_text      = scr_t001
    ev_about     = scr_tab9
    ev_title     = scr_t900
    ev_version   = scr_t901
    ev_copyright = scr_t902
    ev_docu      = b_docu
    ev_tool      = b_tool
    ev_home      = b_home ).
