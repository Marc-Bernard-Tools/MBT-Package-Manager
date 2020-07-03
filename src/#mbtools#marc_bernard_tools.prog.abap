************************************************************************
* /MBTOOLS/MARC_BERNARD_TOOLS
* Marc Bernard Tools
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
REPORT /mbtools/marc_bernard_tools.

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
  p_show  RADIOBUTTON GROUP g1 DEFAULT 'X',
  p_reg   RADIOBUTTON GROUP g1,
  p_unreg RADIOBUTTON GROUP g1,
  p_act   RADIOBUTTON GROUP g1,
  p_deact RADIOBUTTON GROUP g1.
SELECTION-SCREEN:
    END OF BLOCK b220,
  END OF SCREEN 200.

*-----------------------------------------------------------------------

* About
SELECTION-SCREEN:
  BEGIN OF SCREEN 900 AS SUBSCREEN,
    BEGIN OF BLOCK b900 WITH FRAME,
      COMMENT /1(50) scr_t900,
      COMMENT 60(25) scr_t901,
      SKIP,
      COMMENT /1(77) scr_t902,
    END OF BLOCK b900,
    BEGIN OF BLOCK b910 WITH FRAME,
      PUSHBUTTON /1(55) b_docu USER-COMMAND docu,
      SKIP,
      PUSHBUTTON /1(55) b_tool USER-COMMAND tool,
      SKIP,
      PUSHBUTTON /1(55) b_home USER-COMMAND home,
    END OF BLOCK b910,
  END OF SCREEN 900.

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

CONSTANTS:
  c_title TYPE string VALUE /mbtools/cl_tool_bc_icon=>c_tool-title.

DATA:
  gv_ok_code TYPE sy-ucomm,
  go_tool    TYPE REF TO /mbtools/cl_tools,
  go_screen  TYPE REF TO /mbtools/cl_screen,
  go_app     TYPE REF TO /mbtools/cl_base,
  gv_tool    TYPE string,
  gv_action  TYPE string,
  gv_flag    TYPE abap_bool.

*-----------------------------------------------------------------------

INITIALIZATION.

  CREATE OBJECT go_app.

  go_tool   = /mbtools/cl_tools=>factory( c_title ).
  go_screen = /mbtools/cl_screen=>factory( c_title ).

  go_screen->init(
    IMPORTING
      ev_text      = scr_t001
      ev_about     = scr_tab9
      ev_title     = scr_t900
      ev_version   = scr_t901
      ev_copyright = scr_t902
      ev_docu      = b_docu
      ev_tool      = b_tool
      ev_home      = b_home ).

  scr_tab2 = go_screen->header(
    iv_icon = icon_tools
    iv_text = 'Tools' ).

  scr_t200 = 'Select "All Tools" or one particular tool and the'.
  scr_t201 = 'action you want to perform'.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN.

  go_app->screen( ).

  go_screen->ucomm( sscrfields-ucomm ).

*-----------------------------------------------------------------------

AT SELECTION-SCREEN OUTPUT.

  go_app->initialize( iv_all = p_all ).

  go_screen->banner( iv_top  = 4
                     iv_left = 20 ).

  go_app->screen( ).

*-----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_title.

  /mbtools/cl_tools=>f4_tools(
    EXPORTING
      iv_pattern = p_title
    RECEIVING
      rv_title   = p_title ).

*-----------------------------------------------------------------------

START-OF-SELECTION.

  LOG-POINT ID /mbtools/bc SUBKEY c_title FIELDS sy-datum sy-uzeit sy-uname.

  go_screen->banner( iv_show = abap_false ).

  IF p_show = abap_true.

    SUBMIT /mbtools/registry VIA SELECTION-SCREEN AND RETURN.

    RETURN.

  ELSEIF p_all = abap_true.

    gv_tool = 'Tools were'.

    CASE abap_true.
      WHEN p_reg.

        gv_flag   = /mbtools/cl_tools=>run_action( /mbtools/cl_tools=>c_action-register ).
        gv_action = 'registered'.

      WHEN p_unreg.

        gv_flag   = /mbtools/cl_tools=>run_action( /mbtools/cl_tools=>c_action-unregister ).
        gv_action = 'unregistered'.

      WHEN p_act.

        gv_flag   = /mbtools/cl_tools=>run_action( /mbtools/cl_tools=>c_action-activate ).
        gv_action = 'activated'.

      WHEN p_deact.

        gv_flag   = /mbtools/cl_tools=>run_action( /mbtools/cl_tools=>c_action-deactivate ).
        gv_action = 'deactivated'.

    ENDCASE.

  ELSE.

    go_tool = /mbtools/cl_tools=>factory( p_title ).
    gv_tool = 'Tool was'.

    CASE abap_true.
      WHEN p_reg.

        gv_flag   = go_tool->register( ).
        gv_action = 'registered'.

      WHEN p_unreg.

        gv_flag   = go_tool->unregister( ).
        gv_action = 'unregistered'.

      WHEN p_act.

        gv_flag   = go_tool->activate( ).
        gv_action = 'activated'.

      WHEN p_deact.

        gv_flag   = go_tool->deactivate( ).
        gv_action = 'deactivated'.

    ENDCASE.

  ENDIF.

  IF gv_flag = abap_true.
    MESSAGE |{ gv_tool } { gv_action } successfully| TYPE 'S'.
  ELSE.
    MESSAGE |Error: { gv_tool } not { gv_action } properly| TYPE 'E' DISPLAY LIKE 'I'.
  ENDIF.
