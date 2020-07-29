CLASS /mbtools/cl_gui_page_main DEFINITION
  PUBLIC
  INHERITING FROM /mbtools/cl_gui_page
  FINAL
  CREATE PUBLIC .
************************************************************************
* MBT GUI Page Main
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************

  PUBLIC SECTION.

    INTERFACES /mbtools/if_gui_hotkeys .

    METHODS constructor
      RAISING
        /mbtools/cx_exception .

    METHODS /mbtools/if_gui_event_handler~on_event
        REDEFINITION .
  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION.

  PRIVATE SECTION.
    METHODS render_scripts
      RETURNING
        VALUE(ro_html) TYPE REF TO /mbtools/cl_html
      RAISING
        /mbtools/cx_exception.

    METHODS build_main_menu
      RETURNING VALUE(ro_menu) TYPE REF TO /mbtools/cl_html_toolbar.


ENDCLASS.



CLASS /MBTOOLS/CL_GUI_PAGE_MAIN IMPLEMENTATION.


  METHOD /mbtools/if_gui_event_handler~on_event.

    CASE iv_action.

      WHEN /mbtools/if_definitions=>c_action-go_home.
        ev_state = /mbtools/cl_gui=>c_event_state-re_render.

      WHEN OTHERS.

        super->/mbtools/if_gui_event_handler~on_event(
          EXPORTING
            iv_action    = iv_action
            iv_getdata   = iv_getdata
            it_postdata  = it_postdata
          IMPORTING
            ei_page      = ei_page
            ev_state     = ev_state ).

    ENDCASE.

  ENDMETHOD.


  METHOD /mbtools/if_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Main'.

    ls_hotkey_action-description = |Add Tool|.
    ls_hotkey_action-action      = /mbtools/if_definitions=>c_action-go_home.
    ls_hotkey_action-hotkey      = |a|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Settings|.
    ls_hotkey_action-action      = /mbtools/if_definitions=>c_action-go_settings.
    ls_hotkey_action-hotkey      = |s|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD build_main_menu.

    DATA:
      lo_tools_menu TYPE REF TO /mbtools/cl_html_toolbar,
      lo_help_menu  TYPE REF TO /mbtools/cl_html_toolbar.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-main'.

    CREATE OBJECT lo_tools_menu.

    lo_tools_menu->add( iv_txt = 'Add Tool'
                        iv_act = /mbtools/if_definitions=>c_action-go_home ) ##NO_TEXT.
    lo_tools_menu->add( iv_txt = 'Check for Updates'
                        iv_act = /mbtools/if_definitions=>c_action-go_settings ) ##NO_TEXT.
    lo_tools_menu->add( iv_txt = 'Update All Tools'
                        iv_act = /mbtools/if_definitions=>c_action-go_settings ) ##NO_TEXT.

    ro_menu->add( iv_txt = 'Tools'
                  io_sub = lo_tools_menu ) ##NO_TEXT.


    CREATE OBJECT lo_help_menu.

    lo_help_menu->add( iv_txt = 'Documentation'
                       iv_act = /mbtools/if_definitions=>c_action-go_home ) ##NO_TEXT.

    lo_help_menu->add( iv_txt = 'Support'
                       iv_act = /mbtools/if_definitions=>c_action-go_home ) ##NO_TEXT.

    ro_menu->add( iv_txt = 'Help'
                  io_sub = lo_help_menu ) ##NO_TEXT.

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_menu  = build_main_menu( ).
    ms_control-page_title = 'Marc Bernard Tools'.
  ENDMETHOD.


  METHOD render_content.

    gui_services( )->get_hotkeys_ctl( )->register_hotkeys( me ).

    CREATE OBJECT ri_html TYPE /mbtools/cl_html.

    ri_html->add( |Hello World!| ).

  ENDMETHOD.


  METHOD render_scripts.

    CREATE OBJECT ro_html.

    ro_html->/mbtools/if_html~set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).

  ENDMETHOD.
ENDCLASS.
