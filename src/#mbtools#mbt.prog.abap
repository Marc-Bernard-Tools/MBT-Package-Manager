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

INCLUDE /mbtools/mbt_main.

**********************************************************************

INITIALIZATION.

  lcl_main=>main_screen_init( ).

AT SELECTION-SCREEN.

  CASE sy-dynnr.
    WHEN lcl_main=>c_dynnr-password.
      lcl_main=>password_screen_event( sscrfields-ucomm ).
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.

  CASE sy-dynnr.
    WHEN lcl_main=>c_dynnr-main.
      lcl_main=>main_screen_output( ).
    WHEN lcl_main=>c_dynnr-password.
      lcl_main=>password_screen_output( ).
  ENDCASE.

AT SELECTION-SCREEN ON EXIT-COMMAND.

  CASE sy-dynnr.
    WHEN lcl_main=>c_dynnr-main.
      lcl_main=>main_screen_exit( ).
  ENDCASE.

START-OF-SELECTION.

  lcl_main=>main_run( ).
