CLASS /mbtools/cl_gui_page_main DEFINITION
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

    INTERFACES /mbtools/if_gui_event_handler .
    INTERFACES /mbtools/if_gui_renderable .
    INTERFACES /mbtools/if_gui_hotkeys .

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
    DATA mo_asset_manager TYPE REF TO /mbtools/cl_gui_asset_manager.

    CLASS-METHODS build_menu
      RETURNING
        VALUE(ro_menu) TYPE REF TO /mbtools/cl_html_toolbar .
    METHODS register_thumbnail
      IMPORTING
        !io_tool         TYPE REF TO /mbtools/cl_tools
      RETURNING
        VALUE(rv_result) TYPE string
      RAISING
        /mbtools/cx_exception .
    METHODS render_actions
      IMPORTING
        !io_tool       TYPE REF TO /mbtools/cl_tools
      RETURNING
        VALUE(ri_html) TYPE REF TO /mbtools/if_html
      RAISING
        /mbtools/cx_exception .
    METHODS render_bundle
      IMPORTING
        !iv_title      TYPE string
      RETURNING
        VALUE(ri_html) TYPE REF TO /mbtools/if_html
      RAISING
        /mbtools/cx_exception .
    METHODS render_bundles
      RETURNING
        VALUE(ri_html) TYPE REF TO /mbtools/if_html
      RAISING
        /mbtools/cx_exception .
    METHODS render_tool
      IMPORTING
        !iv_title      TYPE string
      RETURNING
        VALUE(ri_html) TYPE REF TO /mbtools/if_html
      RAISING
        /mbtools/cx_exception .
ENDCLASS.



CLASS /MBTOOLS/CL_GUI_PAGE_MAIN IMPLEMENTATION.


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

    DATA lt_assets TYPE /mbtools/if_gui_asset_manager=>ty_web_assets.

    FIELD-SYMBOLS <ls_asset> LIKE LINE OF lt_assets.

    gui_services( )->register_event_handler( me ).
    gui_services( )->get_hotkeys_ctl( )->register_hotkeys( me ).

    ri_html = /mbtools/cl_html=>create( ).

    ri_html->add( render_bundles( ) ).

    IF mo_asset_manager IS BOUND.
      lt_assets = mo_asset_manager->/mbtools/if_gui_asset_manager~get_all_assets( ).
      LOOP AT lt_assets ASSIGNING <ls_asset> WHERE is_cacheable = abap_true.
        gui_services( )->cache_asset(
          iv_xdata   = <ls_asset>-content
          iv_url     = <ls_asset>-url
          iv_type    = <ls_asset>-type
          iv_subtype = <ls_asset>-subtype ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD build_menu.

    DATA:
      lo_tools_menu   TYPE REF TO /mbtools/cl_html_toolbar,
      lo_support_menu TYPE REF TO /mbtools/cl_html_toolbar,
      lo_help_menu    TYPE REF TO /mbtools/cl_html_toolbar.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-main'.

    CREATE OBJECT lo_tools_menu.

    lo_tools_menu->add(
      iv_txt = 'Add Tool'
      iv_act = /mbtools/if_definitions=>c_action-tool_install
    )->add(
      iv_txt = 'Check for Updates'
      iv_act = /mbtools/if_definitions=>c_action-tools_check
    )->add(
      iv_txt = 'Update All Tools'
      iv_act = /mbtools/if_definitions=>c_action-tools_update ) ##NO_TEXT.

    CREATE OBJECT lo_support_menu.

    lo_support_menu->add(
      iv_txt = 'FAQ'
      iv_act = /mbtools/if_definitions=>c_action-go_faq
    )->add(
      iv_txt = 'Documentation'
      iv_act = /mbtools/if_definitions=>c_action-mbt_docs
    )->add(
      iv_txt = 'Ticket'
      iv_act = /mbtools/if_definitions=>c_action-mbt_support ) ##NO_TEXT.

    CREATE OBJECT lo_help_menu.

    lo_help_menu->add(
      iv_txt = 'Website'
      iv_act = /mbtools/if_definitions=>c_action-mbt_website
    )->add(
      iv_txt = 'About'
      iv_act = /mbtools/if_definitions=>c_action-go_about ) ##NO_TEXT.

    ro_menu->add(
      iv_txt = 'Tools'
      io_sub = lo_tools_menu
    )->add(
      iv_txt = 'Support'
      io_sub = lo_support_menu
    )->add(
      iv_txt = 'MBT'
      io_sub = lo_help_menu ) ##NO_TEXT.

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    CREATE OBJECT mo_asset_manager.
  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO /mbtools/cl_gui_page_main.

    CREATE OBJECT lo_component.

    ri_page = /mbtools/cl_gui_page=>create(
      iv_has_logo        = abap_false
      iv_has_banner      = abap_true
      iv_page_title      = ''
      io_page_menu       = build_menu( )
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD register_thumbnail.

    rv_result = 'img/' && io_tool->get_name( ) && '.jpg'.

    mo_asset_manager->register_asset(
      iv_url       = rv_result
      iv_type      = 'image/jpg'
      iv_mime_name = io_tool->get_thumbnail( ) ).

  ENDMETHOD.


  METHOD render_actions.

    ri_html = /mbtools/cl_html=>create( ).

    IF io_tool->has_launch( ) = abap_true.
      ri_html->add_a(
        iv_act = |{ /mbtools/if_definitions=>c_action-tool_launch }?name={ io_tool->get_name( ) }|
        iv_txt = /mbtools/cl_html=>icon( iv_name  = 'rocket/black'
                                         iv_hint  = 'Launch tool' ) ).
    ENDIF.

    IF io_tool->is_bundle( ) = abap_false.
      ri_html->add_a(
        iv_act = |{ /mbtools/if_definitions=>c_action-tool_docs }?name={ io_tool->get_name( ) }|
        iv_txt = /mbtools/cl_html=>icon( iv_name  = 'question/black'
                                         iv_hint  = 'Display tool documentation' ) ).
    ENDIF.

    ri_html->add_a(
      iv_act = |{ /mbtools/if_definitions=>c_action-tool_info }?name={ io_tool->get_name( ) }|
      iv_txt = /mbtools/cl_html=>icon( iv_name  = 'info/black'
                                       iv_hint  = 'Show more information about tool' ) ).

    IF io_tool->is_bundle( ) = abap_false.
      IF /mbtools/cl_switches=>is_active( io_tool->get_title( ) ) = abap_false.
        ri_html->add_a(
          iv_act = |{ /mbtools/if_definitions=>c_action-tool_activate }?name={ io_tool->get_name( ) }|
          iv_txt = /mbtools/cl_html=>icon( iv_name  = 'fire-alt/black'
                                           iv_hint  = 'Activate tool' ) ).
      ELSE.
        ri_html->add_a(
          iv_act = |{ /mbtools/if_definitions=>c_action-tool_deactivate }?name={ io_tool->get_name( ) }|
          iv_txt = /mbtools/cl_html=>icon( iv_name  = 'snowflake/black'
                                           iv_hint  = 'Deactivate tool' ) ).
      ENDIF.

      ri_html->add_a(
        iv_act = |{ /mbtools/if_definitions=>c_action-tool_uninstall }?name={ io_tool->get_name( ) }|
        iv_txt = /mbtools/cl_html=>icon( iv_name  = 'trash-alt/black'
                                         iv_hint  = 'Uninstall tool' ) ).
    ENDIF.

  ENDMETHOD.


  METHOD render_bundle.

    DATA:
      lo_bundle TYPE REF TO /mbtools/cl_tools,
      ls_tool   TYPE /mbtools/tool_with_text,
      lt_tools  TYPE TABLE OF /mbtools/tool_with_text.

    lo_bundle = /mbtools/cl_tools=>factory( iv_title ).

    lt_tools = /mbtools/cl_tools=>get_tools( iv_bundle_id   = lo_bundle->build_manifest( )-bundle_id
                                             iv_get_bundles = abap_false
                                             iv_get_tools   = abap_true ).

    IF lt_tools IS INITIAL.
      RETURN.
    ENDIF.

    ri_html = /mbtools/cl_html=>create( ).

    ri_html->add( render_tool( |{ iv_title }| ) ).

    ri_html->add( '<div class="tools">' ).
    ri_html->add( '<ul>' ).
    LOOP AT lt_tools INTO ls_tool.
      ri_html->add( '<li>' ).
      ri_html->add( render_tool( |{ ls_tool-name }| ) ).
      ri_html->add( '</li>' ).
    ENDLOOP.
    ri_html->add( '</ul>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_bundles.

    DATA:
      ls_bundle  TYPE /mbtools/tool_with_text,
      lt_bundles TYPE TABLE OF /mbtools/tool_with_text.

    lt_bundles = /mbtools/cl_tools=>get_tools( iv_get_bundles = abap_true
                                               iv_get_tools   = abap_false ).

    ri_html = /mbtools/cl_html=>create( ).

    ri_html->add( '<div class="bundles">' ).
    ri_html->add( '<ul>' ).
    LOOP AT lt_bundles INTO ls_bundle.
      ri_html->add( '<li>' ).
      ri_html->add( render_bundle( |{ ls_bundle-name }| ) ).
      ri_html->add( '</li>' ).
    ENDLOOP.
    ri_html->add( '</ul>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_tool.

    DATA:
      lo_tool  TYPE REF TO /mbtools/cl_tools,
      lv_state TYPE string,
      lv_img   TYPE string.

    lo_tool = /mbtools/cl_tools=>factory( iv_title ).

    IF lo_tool->is_bundle( ) = abap_false AND /mbtools/cl_switches=>is_active( iv_title ) = abap_false.
      lv_state = | class="inactive"|.
    ENDIF.

    lv_img = |<img src="{ register_thumbnail( lo_tool ) }" alt="{ lo_tool->get_title( ) }">|.

    ri_html = /mbtools/cl_html=>create( ).

    ri_html->add( |<table><tr{ lv_state }>| ).

    ri_html->add( '<td class="thumbnail">' && lv_img && '</td>' ).

    ri_html->add( '<td><span class="title">' && lo_tool->get_title( ) && '</span><br>'
      && lo_tool->get_description( ) && '</td>' ).

    ri_html->add( '<td class="actions">' ).
    ri_html->add( render_actions( lo_tool ) ).
    ri_html->add( '</td>' ).

    ri_html->add( '</tr></table>' ).

  ENDMETHOD.
ENDCLASS.
