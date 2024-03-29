REPORT /mbtools/mbt_support.

************************************************************************
* Marc Bernard Tools - Support
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************

TABLES:
  sscrfields.

*-----------------------------------------------------------------------

" Function Keys
SELECTION-SCREEN FUNCTION KEY: 1, 2.

*-----------------------------------------------------------------------

* Main
SELECTION-SCREEN:
  BEGIN OF SCREEN 200 AS SUBSCREEN,
    BEGIN OF BLOCK b200 WITH FRAME,
      COMMENT /1(77) sc_t200,
      COMMENT /1(77) sc_t201,
  END OF BLOCK b200,
  BEGIN OF BLOCK b210 WITH FRAME.
PARAMETERS:
  p_pass  RADIOBUTTON GROUP g0 DEFAULT 'X' USER-COMMAND all,
  p_bund  RADIOBUTTON GROUP g0,
  p_all   RADIOBUTTON GROUP g0,
  p_sel   RADIOBUTTON GROUP g0,
  p_title TYPE string MEMORY ID /mbtools/tool LOWER CASE.
SELECTION-SCREEN:
  END OF BLOCK b210,
  BEGIN OF BLOCK b220 WITH FRAME.
PARAMETERS:
  p_sync   RADIOBUTTON GROUP g1 DEFAULT 'X',
  p_reg    RADIOBUTTON GROUP g1,
  p_unreg  RADIOBUTTON GROUP g1,
  p_act    RADIOBUTTON GROUP g1,
  p_deact  RADIOBUTTON GROUP g1,
  p_check  RADIOBUTTON GROUP g1,
  p_addonl RADIOBUTTON GROUP g1,
  p_addoff RADIOBUTTON GROUP g1,
  p_remove RADIOBUTTON GROUP g1,
  p_update RADIOBUTTON GROUP g1,
  p_uninst RADIOBUTTON GROUP g1.
SELECTION-SCREEN:
    END OF BLOCK b220,
  END OF SCREEN 200.

*-----------------------------------------------------------------------

* About
SELECTION-SCREEN:
  BEGIN OF SCREEN 900 AS SUBSCREEN,
    BEGIN OF BLOCK b900 WITH FRAME,
      COMMENT /1(50) sc_t900,
      COMMENT 60(25) sc_t901,
      SKIP,
      COMMENT /1(77) sc_t902,
    END OF BLOCK b900,
    BEGIN OF BLOCK b910 WITH FRAME,
      PUSHBUTTON /1(55) sc_docu USER-COMMAND docu,
      SKIP,
      PUSHBUTTON /1(55) sc_tool USER-COMMAND tool,
      SKIP,
      PUSHBUTTON /1(55) sc_lice USER-COMMAND lice,
      SKIP,
      PUSHBUTTON /1(55) sc_home USER-COMMAND home,
    END OF BLOCK b910,
  END OF SCREEN 900.

*-----------------------------------------------------------------------

* Header
SELECTION-SCREEN:
  BEGIN OF BLOCK scr_header,
    SKIP,
    SKIP,
    COMMENT /3(77) sc_t001 FOR FIELD p_title,
    SKIP,
  END OF BLOCK scr_header,
  BEGIN OF TABBED BLOCK sc_tab FOR 24 LINES,
    TAB (40) sc_tab2 USER-COMMAND sc_push2 DEFAULT SCREEN 200,
    TAB (40) sc_tab9 USER-COMMAND sc_push9 DEFAULT SCREEN 900,
  END OF BLOCK sc_tab.

*-----------------------------------------------------------------------

DATA:
  go_tool   TYPE REF TO /mbtools/cl_tool,
  go_screen TYPE REF TO /mbtools/cl_screen,
  go_app    TYPE REF TO /mbtools/cl_support,
  gx_exc    TYPE REF TO /mbtools/cx_exception,
  gv_tool   TYPE string,
  gv_action TYPE string,
  gv_msg    TYPE string,
  gv_answer TYPE c LENGTH 1,
  gv_flag   TYPE abap_bool.

*-----------------------------------------------------------------------

INITIALIZATION.

  gv_flag = /mbtools/cl_utilities=>get_user_parameter( '/MBTOOLS/SUPPORT' ).
  IF gv_flag IS INITIAL.
    MESSAGE 'This program should only be used by MBT Support' TYPE 'E' ##NO_TEXT.
    RETURN.
  ENDIF.

  CREATE OBJECT go_app.

  go_tool   = /mbtools/cl_tool_manager=>factory( ).
  go_screen = /mbtools/cl_screen=>factory( ).

  go_screen->init(
    IMPORTING
      ev_text      = sc_t001
      ev_about     = sc_tab9
      ev_title     = sc_t900
      ev_version   = sc_t901
      ev_copyright = sc_t902
      ev_docu      = sc_docu
      ev_tool      = sc_tool
      ev_home      = sc_home
      ev_lice      = sc_lice ).

*-----------------------------------------------------------------------

* Function Keys
  sscrfields-functxt_01 = icon_biw_info_cube && 'Registry'(001).
  sscrfields-functxt_02 = icon_settings && 'Setup'(002).

*-----------------------------------------------------------------------

  sc_tab2 = go_screen->header(
    iv_icon = icon_tools
    iv_text = 'Tools'(003) ).

  sc_t200 = 'Select all bundles, all tools, or one particular tool and the'(004).
  sc_t201 = 'action you want to perform'(005).

*-----------------------------------------------------------------------

AT SELECTION-SCREEN.

  IF p_pass = abap_true OR p_bund = abap_true.
    IF p_act = abap_true.
      p_act  = abap_false.
    ENDIF.
    IF p_deact = abap_true.
      p_deact = abap_false.
    ENDIF.
  ENDIF.

  go_app->screen( ).

  go_screen->ucomm( sscrfields-ucomm ).

  CASE sscrfields-ucomm.

*   Function Keys
    WHEN 'FC01'. " Registry
      SUBMIT /mbtools/registry VIA SELECTION-SCREEN AND RETURN. "#EC CI_SUBMIT
      CLEAR sscrfields-ucomm.

    WHEN 'FC02'. " Setup
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = /mbtools/cl_tool_bc=>c_tool-title
          text_question         = 'Are you sure you want to overwrite the setup?'(006)
          text_button_1         = 'Yes'(007)
          text_button_2         = 'No'(008)
          default_button        = '2'
          display_cancel_button = 'X'
        IMPORTING
          answer                = gv_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc = 0 AND gv_answer = '1'.
        /mbtools/cl_setup=>install( abap_true ).
      ENDIF.
      CLEAR sscrfields-ucomm.

  ENDCASE.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN OUTPUT.

  go_app->initialize( iv_all_bundles = p_bund
                      iv_all_tools   = p_all
                      iv_all_passes  = p_pass ).

  go_screen->banner( iv_top  = 4
                     iv_left = 20 ).

  go_app->screen( ).

*-----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_title.

  p_title = /mbtools/cl_tool_manager=>f4(
    iv_pattern        = p_title
    iv_get_passes     = abap_true
    iv_get_bundles    = abap_true
    iv_get_tools      = abap_true
    iv_get_extensions = abap_true
    iv_admin          = abap_true ).

*-----------------------------------------------------------------------

START-OF-SELECTION.

  LOG-POINT ID /mbtools/bc SUBKEY /mbtools/cl_tool_bc=>c_tool-title FIELDS sy-datum sy-uzeit sy-uname.

  go_screen->banner( abap_false ).

  CLEAR: gv_msg, gv_action, gv_flag.

  CASE abap_true.

    WHEN p_sync.

      gv_tool = 'Registry and Installer were'(009).
      gv_action = 'synchronized'(010).

      gv_flag = /mbtools/cl_tool_manager=>action_tools( /mbtools/if_actions=>tool_sync ).

    WHEN p_pass.

      gv_tool = 'Passes were'(023).

      CASE abap_true.
        WHEN p_reg.

          gv_flag   = /mbtools/cl_tool_manager=>action_passes( /mbtools/if_actions=>tool_register ).
          gv_action = 'registered'(012).

        WHEN p_unreg.

          gv_flag   = /mbtools/cl_tool_manager=>action_passes( /mbtools/if_actions=>tool_unregister ).
          gv_action = 'unregistered'(013).

      ENDCASE.

    WHEN p_bund.

      gv_tool = 'Bundles were'(011).

      CASE abap_true.
        WHEN p_reg.

          gv_flag   = /mbtools/cl_tool_manager=>action_bundles( /mbtools/if_actions=>tool_register ).
          gv_action = 'registered'(012).

        WHEN p_unreg.

          gv_flag   = /mbtools/cl_tool_manager=>action_bundles( /mbtools/if_actions=>tool_unregister ).
          gv_action = 'unregistered'(013).

      ENDCASE.

    WHEN p_all.

      gv_tool = 'Tools were'(014).

      CASE abap_true.
        WHEN p_reg.

          gv_flag   = /mbtools/cl_tool_manager=>action_tools( /mbtools/if_actions=>tool_register ).
          gv_action = 'registered'(012).

        WHEN p_unreg.

          gv_flag   = /mbtools/cl_tool_manager=>action_tools( /mbtools/if_actions=>tool_unregister ).
          gv_action = 'unregistered'(013).

        WHEN p_act.

          gv_flag   = /mbtools/cl_tool_manager=>action_tools( /mbtools/if_actions=>tool_activate ).
          gv_action = 'activated'(015).

        WHEN p_deact.

          gv_flag   = /mbtools/cl_tool_manager=>action_tools( /mbtools/if_actions=>tool_deactivate ).
          gv_action = 'deactivated'(016).

        WHEN p_addonl.

          gv_flag   = /mbtools/cl_tool_manager=>action_tools( /mbtools/if_actions=>repo_add_online ).
          gv_action = 'added to abapGit'(022).

        WHEN p_addoff.

          gv_flag   = /mbtools/cl_tool_manager=>action_tools( /mbtools/if_actions=>repo_add_offline ).
          gv_action = 'added to abapGit'(022).

        WHEN p_remove.

          gv_flag   = /mbtools/cl_tool_manager=>action_tools( /mbtools/if_actions=>repo_remove ).
          gv_action = 'removed from abapGit'(021).

        WHEN p_check.

          gv_flag   = /mbtools/cl_tool_manager=>action_tools( /mbtools/if_actions=>tool_check ).
          gv_action = 'version checked'(017).

        WHEN p_update.

          gv_flag   = /mbtools/cl_tool_manager=>action_tools( /mbtools/if_actions=>tool_update ).
          gv_action = 'updated'(018).

        WHEN p_uninst.

          gv_flag   = /mbtools/cl_tool_manager=>action_tools( /mbtools/if_actions=>tool_uninstall ).
          gv_action = 'uninstalled'(019).

      ENDCASE.

    WHEN p_sel.

      TRY.
          go_tool = /mbtools/cl_tool_manager=>factory( p_title ).
          gv_tool = 'Tool was'(020).

          CASE abap_true.
            WHEN p_reg.

              gv_flag   = go_tool->register( ).
              gv_action = 'registered'(012).

            WHEN p_unreg.

              gv_flag   = go_tool->unregister( ).
              gv_action = 'unregistered'(013).

            WHEN p_act.

              gv_flag   = go_tool->activate( ).
              gv_action = 'activated'(015).

            WHEN p_deact.

              gv_flag   = go_tool->deactivate( ).
              gv_action = 'deactivated'(016).

            WHEN p_addonl.

              gv_flag   = go_tool->repo_add_online( ).
              gv_action = 'added to abapGit'(022).

            WHEN p_addoff.

              gv_flag   = go_tool->repo_add_offline( ).
              gv_action = 'added to abapGit'(022).

            WHEN p_remove.

              gv_flag   = go_tool->repo_remove( ).
              gv_action = 'removed from abapGit'(021).

            WHEN p_check.

              gv_flag   = go_tool->check_version( abap_true ).
              gv_action = 'version checked'(017).

            WHEN p_update.

              gv_flag   = /mbtools/cl_tool_manager=>update( go_tool ).
              gv_action = 'updated'(018).

            WHEN p_uninst.

              gv_flag   = /mbtools/cl_tool_manager=>uninstall( go_tool ).
              gv_action = 'uninstalled'(019).

          ENDCASE.

        CATCH /mbtools/cx_exception INTO gx_exc.
          gv_flag = abap_false.
          gv_msg = gx_exc->get_text( ).
      ENDTRY.

    WHEN OTHERS.
      ASSERT 1 = 2.
  ENDCASE.

  IF gv_flag = abap_true.
    gv_msg = |{ gv_tool } { gv_action } successfully|.
    MESSAGE gv_msg TYPE 'S'.
  ELSE.
    gv_msg = |Error: { gv_tool } not { gv_action } properly. { gv_msg }|.
    MESSAGE gv_msg TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
