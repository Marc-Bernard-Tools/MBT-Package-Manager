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
      IMPORTING
        iv_admin TYPE abap_bool OPTIONAL
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS create
      IMPORTING
        iv_admin       TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(ri_page) TYPE REF TO /mbtools/if_gui_renderable
      RAISING
        /mbtools/cx_exception .
  PROTECTED SECTION.
private section.

  data MV_ADMIN type ABAP_BOOL .
  data MO_ASSET_MANAGER type ref to /MBTOOLS/CL_GUI_ASSET_MANAGER .

  methods GET_TOOL_FROM_PARAM
    importing
      !IV_NAME type STRING
    returning
      value(RO_TOOL) type ref to /MBTOOLS/CL_TOOLS
    raising
      /MBTOOLS/CX_EXCEPTION .
  methods VALIDATE_TOOL
    importing
      !IV_ACTION type CLIKE
      !IO_TOOL type ref to /MBTOOLS/CL_TOOLS
    raising
      /MBTOOLS/CX_EXCEPTION .
  class-methods BUILD_MENU
    importing
      !IV_ADMIN type ABAP_BOOL optional
    returning
      value(RO_MENU) type ref to /MBTOOLS/CL_HTML_TOOLBAR .
  methods REGISTER_HEADER
    raising
      /MBTOOLS/CX_EXCEPTION .
  methods REGISTER_THUMBNAIL
    importing
      !IO_TOOL type ref to /MBTOOLS/CL_TOOLS
    returning
      value(RV_RESULT) type STRING
    raising
      /MBTOOLS/CX_EXCEPTION .
  methods RENDER_ACTIONS
    importing
      !IO_TOOL type ref to /MBTOOLS/CL_TOOLS
    returning
      value(RI_HTML) type ref to /MBTOOLS/IF_HTML
    raising
      /MBTOOLS/CX_EXCEPTION .
  methods RENDER_BUNDLE
    importing
      !IV_TITLE type STRING
    returning
      value(RI_HTML) type ref to /MBTOOLS/IF_HTML
    raising
      /MBTOOLS/CX_EXCEPTION .
  methods RENDER_BUNDLES
    returning
      value(RI_HTML) type ref to /MBTOOLS/IF_HTML
    raising
      /MBTOOLS/CX_EXCEPTION .
  methods RENDER_TOOLS
    returning
      value(RI_HTML) type ref to /MBTOOLS/IF_HTML
    raising
      /MBTOOLS/CX_EXCEPTION .
  methods RENDER_TOOL
    importing
      !IV_TITLE type STRING
    returning
      value(RI_HTML) type ref to /MBTOOLS/IF_HTML
    raising
      /MBTOOLS/CX_EXCEPTION .
ENDCLASS.



CLASS /MBTOOLS/CL_GUI_PAGE_MAIN IMPLEMENTATION.


  METHOD /mbtools/if_gui_event_handler~on_event.

    DATA:
      lo_tool TYPE REF TO /mbtools/cl_tools.

    lo_tool = get_tool_from_param( io_parameters->get( 'name' ) ).

    validate_tool( io_tool   = lo_tool
                   iv_action = iv_action ).

    CASE iv_action.

      WHEN /mbtools/if_actions=>tools_check ##TODO.
        ev_state = /mbtools/cl_gui=>c_event_state-re_render.

      WHEN /mbtools/if_actions=>tools_update ##TODO.
        ev_state = /mbtools/cl_gui=>c_event_state-re_render.

      WHEN /mbtools/if_actions=>tools_license ##TODO.
        ev_state = /mbtools/cl_gui=>c_event_state-re_render.

      WHEN /mbtools/if_actions=>tool_docs.
        /mbtools/cl_utilities=>call_browser( lo_tool->get_url_docs( ) ).
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_actions=>tool_info.
        /mbtools/cl_utilities=>call_browser( lo_tool->get_url_tool( ) ).
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_actions=>tool_changelog ##TODO.
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_actions=>tool_activate.
        lo_tool->activate( ).
        ev_state = /mbtools/cl_gui=>c_event_state-re_render.

      WHEN /mbtools/if_actions=>tool_deactivate.
        lo_tool->deactivate( ).
        ev_state = /mbtools/cl_gui=>c_event_state-re_render.

      WHEN /mbtools/if_actions=>tool_install.
        lo_tool->register( ).
        ev_state = /mbtools/cl_gui=>c_event_state-re_render.

      WHEN /mbtools/if_actions=>tool_uninstall.
        lo_tool->unregister( ).
        ev_state = /mbtools/cl_gui=>c_event_state-re_render.

      WHEN /mbtools/if_actions=>tool_launch.
        lo_tool->launch( ).
        ev_state = /mbtools/cl_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.


  METHOD /mbtools/if_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Main'.

    ls_hotkey_action-description = |Add Tool|.
    ls_hotkey_action-action      = /mbtools/if_actions=>tool_install.
    ls_hotkey_action-hotkey      = |a|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Check for Updates|.
    ls_hotkey_action-action      = /mbtools/if_actions=>tools_check.
    ls_hotkey_action-hotkey      = |c|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Maintain License Keys|.
    ls_hotkey_action-action      = /mbtools/if_actions=>tools_license.
    ls_hotkey_action-hotkey      = |l|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Quit|.
    ls_hotkey_action-action      = /mbtools/if_actions=>quit.
    ls_hotkey_action-hotkey      = |q|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Update All Tools|.
    ls_hotkey_action-action      = /mbtools/if_actions=>tools_update.
    ls_hotkey_action-hotkey      = |u|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Exit Admin|.
    ls_hotkey_action-action      = /mbtools/if_actions=>go_home.
    ls_hotkey_action-hotkey      = |x|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD /mbtools/if_gui_renderable~render.

    DATA lt_assets TYPE /mbtools/if_gui_asset_manager=>ty_web_assets.

    FIELD-SYMBOLS <ls_asset> LIKE LINE OF lt_assets.

    gui_services( )->register_event_handler( me ).
    gui_services( )->get_hotkeys_ctl( )->register_hotkeys( me ).

    register_header( ).

    ri_html = /mbtools/cl_html=>create( ).

    IF mv_admin = abap_true.
      ri_html->add( render_bundles( ) ).
    ELSE.
      ri_html->add( render_tools( ) ).
    ENDIF.

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
      iv_act = /mbtools/if_actions=>tool_install
    )->add(
      iv_txt = 'Check for Updates'
      iv_act = /mbtools/if_actions=>tools_check
    )->add(
      iv_txt = 'Update All Tools'
      iv_act = /mbtools/if_actions=>tools_update
    )->add(
      iv_txt = 'Maintain License Keys'
      iv_act = /mbtools/if_actions=>tools_license ) ##NO_TEXT.

    CREATE OBJECT lo_support_menu.

    lo_support_menu->add(
      iv_txt = 'FAQ'
      iv_act = /mbtools/if_actions=>go_faq
    )->add(
      iv_txt = 'Documentation'
      iv_act = /mbtools/if_actions=>mbt_docs
    )->add(
      iv_txt = 'Ticket'
      iv_act = /mbtools/if_actions=>mbt_support ) ##NO_TEXT.

    CREATE OBJECT lo_help_menu.

    IF iv_admin = abap_false.
      lo_help_menu->add(
        iv_txt = 'Admin'
        iv_act = /mbtools/if_actions=>go_admin ).
    ELSE.
      lo_help_menu->add(
        iv_txt = 'Exit Admin'
        iv_act = /mbtools/if_actions=>go_home ).
    ENDIF.

    lo_help_menu->add(
      iv_txt = 'Website'
      iv_act = /mbtools/if_actions=>mbt_website
    )->add(
      iv_txt = 'About'
      iv_act = /mbtools/if_actions=>go_about ) ##NO_TEXT.

    IF iv_admin = abap_true.
      ro_menu->add(
        iv_txt = 'Tools'
        io_sub = lo_tools_menu
      ).
    ENDIF.

    ro_menu->add(
      iv_txt = 'Support'
      io_sub = lo_support_menu
    )->add(
      iv_txt = 'MBT'
      io_sub = lo_help_menu ) ##NO_TEXT.

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    CREATE OBJECT mo_asset_manager.

    mv_admin = iv_admin.
  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO /mbtools/cl_gui_page_main.

    CREATE OBJECT lo_component EXPORTING iv_admin = iv_admin.

    IF iv_admin = abap_true.

      ri_page = /mbtools/cl_gui_page=>create(
        iv_has_logo        = abap_true
        iv_has_banner      = abap_false
        iv_page_title      = 'Administration'
        io_page_menu       = build_menu( iv_admin )
        ii_child_component = lo_component ).

    ELSE.

      ri_page = /mbtools/cl_gui_page=>create(
        iv_has_logo        = abap_false
        iv_has_banner      = abap_true
        iv_page_title      = ''
        io_page_menu       = build_menu( iv_admin )
        ii_child_component = lo_component ).

    ENDIF.

  ENDMETHOD.


  METHOD get_tool_from_param.

    DATA lv_name TYPE string.

    IF iv_name IS INITIAL.
      RETURN.
    ENDIF.

    lv_name = iv_name.
    REPLACE ALL OCCURRENCES OF '_' IN lv_name WITH ` `.
    ro_tool = /mbtools/cl_tools=>factory( lv_name ).
    IF NOT ro_tool IS BOUND.
      /mbtools/cx_exception=>raise( |Tool { lv_name } could not be instanciated| ).
    ENDIF.

  ENDMETHOD.


  METHOD register_header.

    mo_asset_manager->register_asset(
      iv_url       = 'img/logo_header.png'
      iv_type      = 'image/png'
      iv_mime_name = '/MBTOOLS/LOGO_HEADER' ).

    mo_asset_manager->register_asset(
      iv_url       = 'img/banner_header.png'
      iv_type      = 'image/png'
      iv_mime_name = '/MBTOOLS/BANNER_HEADER' ).

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

    IF mv_admin = abap_false.
      " User
      IF io_tool->has_launch( ) = abap_true.
        ri_html->add_a(
          iv_act = |{ /mbtools/if_actions=>tool_launch }?name={ io_tool->get_name( ) }|
          iv_txt = /mbtools/cl_html=>icon( iv_name  = 'rocket/black'
                                           iv_hint  = 'Launch tool' ) ).
      ENDIF.

      IF io_tool->is_bundle( ) = abap_false.
        ri_html->add_a(
          iv_act = |{ /mbtools/if_actions=>tool_docs }?name={ io_tool->get_name( ) }|
          iv_txt = /mbtools/cl_html=>icon( iv_name  = 'question/black'
                                           iv_hint  = 'Display tool documentation' ) ).
      ENDIF.
    ELSE.
      " Admin
      IF io_tool->is_bundle( ) = abap_false.
        IF io_tool->is_active( ) = abap_false.
          ri_html->add_a(
            iv_act = |{ /mbtools/if_actions=>tool_activate }?name={ io_tool->get_name( ) }|
            iv_txt = /mbtools/cl_html=>icon( iv_name  = 'fire-alt/black'
                                             iv_hint  = 'Activate tool' ) ).
          ri_html->add_a(
            iv_act = |{ /mbtools/if_actions=>tool_uninstall }?name={ io_tool->get_name( ) }|
            iv_txt = /mbtools/cl_html=>icon( iv_name  = 'trash-alt/black'
                                             iv_hint  = 'Uninstall tool' ) ).
        ELSE.
          ri_html->add_a(
            iv_act = |{ /mbtools/if_actions=>tool_deactivate }?name={ io_tool->get_name( ) }|
            iv_txt = /mbtools/cl_html=>icon( iv_name  = 'snowflake/black'
                                             iv_hint  = 'Deactivate tool' ) ).
        ENDIF.

      ENDIF.

      ri_html->add_a(
        iv_act = |{ /mbtools/if_actions=>tool_info }?name={ io_tool->get_name( ) }|
        iv_txt = /mbtools/cl_html=>icon( iv_name  = 'globe/black'
                                         iv_hint  = 'Show more information about tool' ) ).
    ENDIF.

  ENDMETHOD.


  METHOD render_bundle.

    DATA:
      lo_bundle TYPE REF TO /mbtools/cl_tools,
      ls_tool   TYPE /mbtools/tool_with_text,
      lt_tools  TYPE TABLE OF /mbtools/tool_with_text.

    ri_html = /mbtools/cl_html=>create( ).

    lo_bundle = /mbtools/cl_tools=>factory( iv_title ).

    lt_tools = /mbtools/cl_tools=>get_tools( iv_bundle_id   = lo_bundle->get_bundle_id( )
                                             iv_get_bundles = abap_false
                                             iv_get_tools   = abap_true
                                             iv_admin       = mv_admin ).

    IF lt_tools IS INITIAL.
      RETURN.
    ENDIF.

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
                                               iv_get_tools   = abap_false
                                               iv_admin       = mv_admin ).

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
      lv_class TYPE string,
      lv_img   TYPE string.

    lo_tool = /mbtools/cl_tools=>factory( iv_title ).

    lv_class = 'tool-row'.
    IF lo_tool->is_bundle( ) = abap_false AND lo_tool->is_active( ) = abap_false.
      lv_class = lv_class && | tool-inactive|.
    ENDIF.

    lv_img = |<img src="{ register_thumbnail( lo_tool ) }" alt="{ lo_tool->get_title( ) }">|.

    ri_html = /mbtools/cl_html=>create( ).

    ri_html->add( |<table><tr class="{ lv_class }">| ).

    ri_html->add( |<td class="tool-thumbnail">{ lv_img }</td>| ).

    ri_html->add( '<td>' ).
    ri_html->add( |<span class="title">{ lo_tool->get_title( ) }</span><br>| &&
      |<span class="description">{ lo_tool->get_description( ) }</span>| ).
    IF mv_admin = abap_true AND lo_tool->is_bundle( ) = abap_false.
      ri_html->add( |<br><span class="tool-details">Version: { lo_tool->get_version( ) } \| Last update: {
        lo_tool->get_last_update( ) } ago</span>| ).
    ENDIF.
    ri_html->add( '</td>' ).

    ri_html->add( '<td class="tool-actions">' ).
    ri_html->add( render_actions( lo_tool ) ).
    ri_html->add( '</td>' ).

    ri_html->add( '</tr></table>' ).

  ENDMETHOD.


  METHOD render_tools.

    DATA:
      ls_tool  TYPE /mbtools/tool_with_text,
      lt_tools TYPE TABLE OF /mbtools/tool_with_text.

    lt_tools = /mbtools/cl_tools=>get_tools( ).

    ri_html = /mbtools/cl_html=>create( ).

    ri_html->add( '<div class="bundles">' ). "format like bundles (w/o indent)
    ri_html->add( '<ul>' ).

    IF lt_tools IS INITIAL.
      ri_html->add( '<li>' ).
      ri_html->add( '<h2>Welcome to Marc Bernard Tools!</h2>' ).
      ri_html->add( |<p>There are currently no active tools in your system. Go to | ).
      ri_html->add_a( iv_txt = 'Adminstration'
                      iv_act = /mbtools/if_actions=>go_admin ).
      ri_html->add( | to add new tools or activate any tools that are already installed.| ).
      ri_html->add( '</li>' ).
    ELSE.
      LOOP AT lt_tools INTO ls_tool.
        ri_html->add( '<li>' ).
        ri_html->add( render_tool( |{ ls_tool-name }| ) ).
        ri_html->add( '</li>' ).
      ENDLOOP.
    ENDIF.

    ri_html->add( '</ul>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD validate_tool.

    " Some actions are not allowed or need confirmation for MBT Base
    DATA lv_answer TYPE c.

    CASE iv_action.
      WHEN /mbtools/if_actions=>tool_deactivate.

        IF io_tool->is_base( ) = abap_true.
          IF io_tool->is_last_tool( ) = abap_true.
            /mbtools/cl_gui_factory=>get_popups( )->popup_to_confirm(
              EXPORTING
                iv_titlebar       = 'Deactivate Tool'
                iv_text_question  = |Are you sure you want to deactivate { io_tool->get_title( ) }?|
                iv_default_button = '2'
              RECEIVING
                rv_answer         = lv_answer ).
            IF lv_answer <> '1'.
              /mbtools/cx_exception=>raise( 'Action cancelled' ).
            ENDIF.
          ELSE.
            /mbtools/cx_exception=>raise( |You have to deactivate all other tools first,| &&
                                          | before you can deactivate { io_tool->get_title( ) }| ).
          ENDIF.
        ENDIF.

      WHEN /mbtools/if_actions=>tool_uninstall.

        IF io_tool->is_base( ) = abap_true AND io_tool->is_last_tool( ) = abap_false.
          /mbtools/cx_exception=>raise( |You have to uninstall all other tools first,| &&
                                        | before you can uninstall { io_tool->get_title( ) }| ).
        ENDIF.

        /mbtools/cl_gui_factory=>get_popups( )->popup_to_confirm(
          EXPORTING
            iv_titlebar       = 'Uninstall Tool'
            iv_text_question  = |Are you sure you want to uninstall { io_tool->get_title( ) }?|
            iv_default_button = '2'
          RECEIVING
            rv_answer         = lv_answer ).
        IF lv_answer <> '1'.
          /mbtools/cx_exception=>raise( 'Action cancelled' ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
