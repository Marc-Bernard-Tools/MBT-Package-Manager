************************************************************************
* /MBTOOLS/BC_TOOL_REGISTER
* MBT Tool Registation
*
* (c) Marc Bernard Tools 2020
* last update: 2020-02-11
************************************************************************

REPORT /mbtools/bc_tool_register.

TABLES:
  sscrfields, icon, icont.

*-----------------------------------------------------------------------

* Main
SELECTION-SCREEN:
  BEGIN OF SCREEN 200 AS SUBSCREEN,
    BEGIN OF BLOCK b200 WITH FRAME,
      COMMENT /1(77) scr_t200,
      COMMENT /1(77) scr_t201,
  END OF BLOCK b200,
  BEGIN OF BLOCK b210 WITH FRAME.
PARAMETERS:
  p_all   AS CHECKBOX DEFAULT 'X' USER-COMMAND all,
  p_title TYPE string MEMORY ID /mbtools/tool LOWER CASE.
SELECTION-SCREEN:
  END OF BLOCK b210,
  BEGIN OF BLOCK b220 WITH FRAME.
PARAMETERS:
  p_reg   RADIOBUTTON GROUP g1 DEFAULT 'X',
  p_unreg RADIOBUTTON GROUP g1.
SELECTION-SCREEN:
    END OF BLOCK b220,
  END OF SCREEN 200.

*-----------------------------------------------------------------------

INCLUDE /mbtools/bc_screen_about_tab.

*-----------------------------------------------------------------------

* Header
SELECTION-SCREEN:
  BEGIN OF BLOCK scr_header,
    SKIP,
    SKIP,
    COMMENT /3(77) scr_t001 FOR FIELD p_title,
    SKIP,
  END OF BLOCK scr_header,
  BEGIN OF TABBED BLOCK scr_tab FOR 22 LINES,
    TAB (40) scr_tab2 USER-COMMAND scr_push2 DEFAULT SCREEN 0200,
    TAB (40) scr_tab9 USER-COMMAND scr_push9 DEFAULT SCREEN 0900,
  END OF BLOCK scr_tab.

*-----------------------------------------------------------------------

INCLUDE /mbtools/bc_screen_data.

DATA:
  gr_app    TYPE REF TO /mbtools/cl_base,
  gv_tool   TYPE string,
  gv_action TYPE string,
  gv_flag   TYPE abap_bool.

*-----------------------------------------------------------------------

INITIALIZATION.

  INCLUDE /mbtools/bc_screen_init.

  scr_tab2 = /mbtools/cl_screen=>header(
    iv_icon = icon_tools
    iv_text = 'Tools' ).

  scr_t200 = 'Select "All Tools" or one particular tool'.
  scr_t201 = 'that you want to register or unregister'.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN.

  INCLUDE /mbtools/bc_screen_at_select.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN OUTPUT.

  gr_app->initialize( i_all = p_all ).

  INCLUDE /mbtools/bc_screen_at_output.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_title.

  CALL METHOD /mbtools/cl_tools=>f4_tools
    EXPORTING
      i_pattern = p_title
    RECEIVING
      r_title   = p_title.

START-OF-SELECTION.

  /mbtools/cl_screen=>banner( i_show = abap_false ).

  IF p_all = abap_true.

    IF p_reg = abap_true.

      CALL METHOD /mbtools/cl_tools=>register_all
        RECEIVING
          r_registered = gv_flag.

      gv_tool   = 'Tools were'.
      gv_action = 'registered'.

    ELSE.

      CALL METHOD /mbtools/cl_tools=>unregister_all
        RECEIVING
          r_unregistered = gv_flag.

      gv_tool   = 'Tools were'.
      gv_action = 'unregistered'.

    ENDIF.

  ELSE.

    gr_tool = /mbtools/cl_tools=>get_tool( i_title = p_title ).

    IF p_reg = abap_true.

      CALL METHOD gr_tool->register
        RECEIVING
          r_registered = gv_flag.

      gv_tool   = 'Tool was'.
      gv_action = 'registered'.

    ELSE.

      CALL METHOD gr_tool->unregister
        RECEIVING
          r_unregistered = gv_flag.

      gv_tool   = 'Tool was'.
      gv_action = 'unregistered'.

    ENDIF.

  ENDIF.

  IF gv_flag = abap_true.
    MESSAGE |{ gv_tool } { gv_action } successfully| TYPE 'I'.
  ELSE.
    MESSAGE |Error: { gv_tool } not { gv_action } properly| TYPE 'E'.
  ENDIF.
