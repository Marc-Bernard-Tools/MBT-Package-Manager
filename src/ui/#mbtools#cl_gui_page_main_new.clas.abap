CLASS /mbtools/cl_gui_page_main_new DEFINITION
  PUBLIC
  INHERITING FROM /mbtools/cl_gui_component
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

    INTERFACES /mbtools/if_gui_event_handler.
    INTERFACES /mbtools/if_gui_renderable.
    INTERFACES /mbtools/if_gui_hotkeys.

    METHODS constructor
      RAISING
        /mbtools/cx_exception .

    CLASS-METHODS create
      RETURNING
        VALUE(ri_page) TYPE REF TO /mbtools/if_gui_renderable
      RAISING
        /mbtools/cx_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS build_menu
      RETURNING
        VALUE(ro_menu) TYPE REF TO /mbtools/cl_html_toolbar.

ENDCLASS.



CLASS /MBTOOLS/CL_GUI_PAGE_MAIN_NEW IMPLEMENTATION.


  METHOD /mbtools/if_gui_event_handler~on_event.

    CASE iv_action.

      WHEN /mbtools/if_definitions=>c_action-go_back.
        ev_state = /mbtools/cl_gui=>c_event_state-go_back.

      WHEN /mbtools/if_definitions=>c_action-go_home.
        ev_state = /mbtools/cl_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.


  METHOD /mbtools/if_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Main'.

    ls_hotkey_action-description = |Add Tool|.
    ls_hotkey_action-action      = /mbtools/if_definitions=>c_action-go_home.
    ls_hotkey_action-hotkey      = |a|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Quit|.
    ls_hotkey_action-action      = /mbtools/if_definitions=>c_action-quit.
    ls_hotkey_action-hotkey      = |q|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Settings|.
    ls_hotkey_action-action      = /mbtools/if_definitions=>c_action-go_settings.
    ls_hotkey_action-hotkey      = |s|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD /mbtools/if_gui_renderable~render.

    gui_services( )->register_event_handler( me ).
    gui_services( )->get_hotkeys_ctl( )->register_hotkeys( me ).

    ri_html = /mbtools/cl_html=>create( ).

    ri_html->add( |Hello World!| ).

  ENDMETHOD.


  METHOD build_menu.

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
  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO /mbtools/cl_gui_page_main_new.

    CREATE OBJECT lo_component.

    ri_page = /mbtools/cl_gui_page=>create(
      iv_page_title      = 'Marc Bernard Tools'
      io_page_menu       = build_menu( )
      ii_child_component = lo_component ).

  ENDMETHOD.
ENDCLASS.
