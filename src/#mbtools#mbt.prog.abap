REPORT /mbtools/mbt.

************************************************************************
* Marc Bernard Tools
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************

TABLES:
  sscrfields.

PARAMETERS: p_mode TYPE string NO-DISPLAY.

*-----------------------------------------------------------------------
* Main Screen
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF SCREEN 1001.
* Dummy for triggering screen on Java SAP GUI
SELECTION-SCREEN END OF SCREEN 1001.

*-----------------------------------------------------------------------
* Password Screen
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF SCREEN 1002 TITLE sc_title.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) sc_url FOR FIELD p_url.
PARAMETERS: p_url TYPE char255 LOWER CASE VISIBLE LENGTH 70 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) sc_user FOR FIELD p_user.
PARAMETERS: p_user TYPE char255 LOWER CASE VISIBLE LENGTH 70 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) sc_pass FOR FIELD p_pass.
PARAMETERS: p_pass TYPE char255 LOWER CASE VISIBLE LENGTH 70 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF SCREEN 1002.

*-----------------------------------------------------------------------
* Main & Password Dialogs
*-----------------------------------------------------------------------
CLASS lcl_main DEFINITION FINAL.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_dynnr,
        main     TYPE c LENGTH 4 VALUE '1001',
        password TYPE c LENGTH 4 VALUE '1002',
      END OF c_dynnr.

    CLASS-METHODS main_run.
    CLASS-METHODS main_screen_init.
    CLASS-METHODS main_screen_output.
    CLASS-METHODS main_screen_exit.

    CLASS-METHODS password_popup
      IMPORTING
        iv_url  TYPE string
      CHANGING
        cv_user TYPE string
        cv_pass TYPE string.
    CLASS-METHODS password_screen_init.
    CLASS-METHODS password_screen_output.
    CLASS-METHODS password_screen_event
      IMPORTING
        iv_ucomm TYPE sy-ucomm.

  PRIVATE SECTION.
    CLASS-DATA go_screen TYPE REF TO /mbtools/cl_screen.
    CLASS-DATA gv_confirm TYPE abap_bool.

ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD main_screen_init.

    go_screen = /mbtools/cl_screen=>factory( ).

    go_screen->toolbar( c_dynnr-main ).

  ENDMETHOD.

  METHOD main_run.

    DATA lx_error TYPE REF TO cx_root.

    TRY.
        /mbtools/cl_gui_factory=>get_gui( )->go_home( p_mode ).

        CALL SELECTION-SCREEN c_dynnr-main. " trigger screen
      CATCH cx_root INTO lx_error.
        " unexpected
        MESSAGE lx_error TYPE 'I' DISPLAY LIKE 'E'.
        BREAK-POINT ID /mbtools/bc.
    ENDTRY.

  ENDMETHOD.

  METHOD main_screen_output.

    DATA: lt_ucomm TYPE TABLE OF sy-ucomm.

    PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.

    APPEND 'CRET' TO lt_ucomm.  "Button Execute
    APPEND 'SPOS' TO lt_ucomm.  "Button Save

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
      TABLES
        p_exclude = lt_ucomm.

  ENDMETHOD.

  METHOD main_screen_exit.

    CASE sy-ucomm.
      WHEN 'CBAC' OR 'CCAN'.  "Back/Cancel
        TRY.
            IF /mbtools/cl_gui_factory=>get_gui( )->back( ) = abap_true. " end of stack
              /mbtools/cl_gui_factory=>get_gui( )->free( ). " Graceful shutdown
            ELSE.
              LEAVE TO SCREEN c_dynnr-main.
            ENDIF.
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.
    ENDCASE.

  ENDMETHOD.

  METHOD password_popup.

    password_screen_init( ).

    CLEAR p_pass.
    p_url      = iv_url.
    p_user     = cv_user.
    gv_confirm = abap_false.

    CONCATENATE sc_title iv_url INTO sc_title SEPARATED BY space.

    CALL SELECTION-SCREEN c_dynnr-password STARTING AT 5 5 ENDING AT 60 8.

    IF gv_confirm = abap_true.
      cv_user = p_user.
      cv_pass = p_pass.
    ELSE.
      CLEAR: cv_user, cv_pass.
    ENDIF.

    CLEAR: p_url, p_user, p_pass.

  ENDMETHOD.

  METHOD password_screen_init.

    sc_title = 'Login'(001).
    sc_url   = 'URL'(002).
    sc_user  = 'User'(003).
    sc_pass  = 'Password'(004).

  ENDMETHOD.

  METHOD password_screen_output.

    DATA lt_ucomm TYPE TABLE OF sy-ucomm.

    ASSERT sy-dynnr = c_dynnr-password.

    LOOP AT SCREEN.
      IF screen-name = 'P_URL'.
        screen-input       = '0'.
        screen-intensified = '1'.
        screen-display_3d  = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'P_PASS'.
        screen-invisible   = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    " Program RSSYSTDB, GUI Status %_CSP
    PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.

    APPEND 'NONE' TO lt_ucomm.  "Button Check
    APPEND 'SPOS' TO lt_ucomm.  "Save as Variant

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
      TABLES
        p_exclude = lt_ucomm.

    IF p_user IS NOT INITIAL.
      SET CURSOR FIELD 'P_PASS'.
    ENDIF.

  ENDMETHOD.

  METHOD password_screen_event.

    ASSERT sy-dynnr = c_dynnr-password.

    " CRET   - F8
    " OTHERS - simulate Enter press
    CASE iv_ucomm.
      WHEN 'CRET'.
        gv_confirm = abap_true.
      WHEN OTHERS. "TODO REFACTOR !!! A CLUTCH !
        " This will work unless any new specific logic appear
        " for other commands. The problem is that the password dialog
        " does not have Enter event (or I don't know how to activate it ;)
        " so Enter issues previous command from previous screen
        " But for now this works :) Fortunately Esc produces another flow
        gv_confirm = abap_true.
        LEAVE TO SCREEN 0.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.

FORM password_popup
  USING
    pv_url  TYPE string
  CHANGING
    cv_user TYPE string
    cv_pass TYPE string.

  lcl_main=>password_popup(
    EXPORTING
      iv_url  = pv_url
    CHANGING
      cv_user = cv_user
      cv_pass = cv_pass ).

ENDFORM.

**********************************************************************

INITIALIZATION.

  DATA lx_error TYPE REF TO cx_root.

  " Initialize MBT application
  TRY.
      /mbtools/cl_setup_db=>install( ).
      /mbtools/cl_setup=>install( ).
    CATCH cx_root INTO lx_error.
      " unexpected
      MESSAGE lx_error TYPE 'I' DISPLAY LIKE 'E'.
      STOP.
  ENDTRY.

  " Register all installed passes, bundles, and tools
  /mbtools/cl_tool_manager=>action_passes( /mbtools/if_actions=>tool_register ).
  /mbtools/cl_tool_manager=>action_bundles( /mbtools/if_actions=>tool_register ).
  /mbtools/cl_tool_manager=>action_tools( /mbtools/if_actions=>tool_register ).

  " If MBT Package Manager is the only installed tool, then activate it
  IF /mbtools/cl_tool_manager=>is_base_only( ) = abap_true.
    /mbtools/cl_tool_manager=>factory( )->activate( ).
  ENDIF.

  lcl_main=>main_screen_init( ).

AT SELECTION-SCREEN.

  CASE sy-dynnr.
    WHEN lcl_main=>c_dynnr-password.
      lcl_main=>password_screen_event( sscrfields-ucomm ).
    WHEN OTHERS.
      ASSERT 0 = 0.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.

  CASE sy-dynnr.
    WHEN lcl_main=>c_dynnr-main.
      lcl_main=>main_screen_output( ).
    WHEN lcl_main=>c_dynnr-password.
      lcl_main=>password_screen_output( ).
    WHEN OTHERS.
      ASSERT 0 = 0.
  ENDCASE.

AT SELECTION-SCREEN ON EXIT-COMMAND.

  CASE sy-dynnr.
    WHEN lcl_main=>c_dynnr-main.
      lcl_main=>main_screen_exit( ).
    WHEN OTHERS.
      ASSERT 0 = 0.
  ENDCASE.

START-OF-SELECTION.

  lcl_main=>main_run( ).
