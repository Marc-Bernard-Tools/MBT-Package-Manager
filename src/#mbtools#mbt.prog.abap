************************************************************************
* /MBTOOLS/MARC_BERNARD_TOOLS
* Marc Bernard Tools
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
REPORT /mbtools/mbt.

TABLES:
  sscrfields.

*-----------------------------------------------------------------------
* Main Screen
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF SCREEN 1001.
* Dummy for triggering screen on Java SAP GUI
SELECTION-SCREEN END OF SCREEN 1001.

*-----------------------------------------------------------------------
* Password Screen
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF SCREEN 1002 TITLE s_title.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) s_url FOR FIELD p_url.
PARAMETERS: p_url TYPE string LOWER CASE VISIBLE LENGTH 50 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) s_user FOR FIELD p_user.
PARAMETERS: p_user TYPE string LOWER CASE VISIBLE LENGTH 50 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) s_pass FOR FIELD p_pass.
PARAMETERS: p_pass TYPE string LOWER CASE VISIBLE LENGTH 50 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF SCREEN 1002.

*-----------------------------------------------------------------------
* Main Dialog
*-----------------------------------------------------------------------
CLASS lcl_main DEFINITION FINAL.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_dynnr,
        main     TYPE c LENGTH 4 VALUE '1001',
        password TYPE c LENGTH 4 VALUE '1002',
      END OF c_dynnr.

    CLASS-METHODS main_run.
    CLASS-METHODS main_screen_output.
    CLASS-METHODS main_screen_exit
      EXCEPTIONS
        /mbtools/cx_exception.

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
    CLASS-DATA gv_confirm TYPE abap_bool.

ENDCLASS.

*-----------------------------------------------------------------------
* Main Dialog
*-----------------------------------------------------------------------
CLASS lcl_main IMPLEMENTATION.

  METHOD main_run.

    TRY.
*        zcl_abapgit_services_abapgit=>prepare_gui_startup( ).
*        zcl_abapgit_ui_factory=>get_gui( )->go_home( ).
        CALL SELECTION-SCREEN c_dynnr-main. " trigger screen
      CATCH cx_root.
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
      WHEN 'CBAC'.  "Back
*        IF zcl_abapgit_ui_factory=>get_gui( )->back( ) = abap_true. " end of stack
*          zcl_abapgit_ui_factory=>get_gui( )->free( ). " Graceful shutdown
*        ELSE.
        LEAVE TO SCREEN c_dynnr-main.
*        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD password_popup.

    CLEAR p_pass.
    p_url      = iv_url.
    p_user     = cv_user.
    gv_confirm = abap_false.

    CLEAR s_title.
    CONCATENATE 'Login' iv_url INTO s_title SEPARATED BY space.

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

    s_title = 'Login'     ##NO_TEXT.
    s_url   = 'URL'       ##NO_TEXT.
    s_user  = 'User'      ##NO_TEXT.
    s_pass  = 'Password'  ##NO_TEXT.

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

  " Remove toolbar on html screen
  /mbtools/cl_screen=>toolbar( iv_show  = abap_false
                               iv_cprog = sy-cprog
                               iv_dynnr = lcl_main=>c_dynnr-main ).
  lcl_main=>password_screen_init( ).

START-OF-SELECTION.

  lcl_main=>main_run( ).

AT SELECTION-SCREEN OUTPUT.

  " Hide Execute button from screen
  CASE sy-dynnr.
    WHEN lcl_main=>c_dynnr-main.
      lcl_main=>main_screen_output( ).
    WHEN lcl_main=>c_dynnr-password.
      lcl_main=>password_screen_output( ).
  ENDCASE.

AT SELECTION-SCREEN ON EXIT-COMMAND.

  " SAP back command re-direction
  lcl_main=>main_screen_exit( ).

AT SELECTION-SCREEN.

  CASE sy-dynnr.
    WHEN lcl_main=>c_dynnr-password.
      lcl_main=>password_screen_event( sscrfields-ucomm ).
  ENDCASE.