CLASS /mbtools/cl_gui_page_main DEFINITION
  PUBLIC
  INHERITING FROM /mbtools/cl_gui_component
  FINAL
  CREATE PUBLIC .

************************************************************************
* Marc Bernard Tools - GUI Page Main
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-or-later
************************************************************************
  PUBLIC SECTION.

    INTERFACES /mbtools/if_gui_event_handler .
    INTERFACES /mbtools/if_gui_renderable .
    INTERFACES /mbtools/if_gui_hotkeys .

    CONSTANTS:
      BEGIN OF c_mode,
        user    TYPE c VALUE 'U',
        admin   TYPE c VALUE 'A',
        license TYPE c VALUE 'L',
      END OF c_mode .

    METHODS constructor
      IMPORTING
        !iv_mode TYPE abap_bool OPTIONAL
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS create
      IMPORTING
        !iv_mode       TYPE c OPTIONAL
      RETURNING
        VALUE(ri_page) TYPE REF TO /mbtools/if_gui_renderable
      RAISING
        /mbtools/cx_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_mode TYPE c.
    DATA mo_asset_manager TYPE REF TO /mbtools/if_gui_asset_manager.

    METHODS get_description_admin
      IMPORTING
        !io_tool       TYPE REF TO /mbtools/cl_tool
      RETURNING
        VALUE(rv_text) TYPE string
      RAISING
        /mbtools/cx_exception.
    METHODS get_description_license
      IMPORTING
        !io_tool       TYPE REF TO /mbtools/cl_tool
      RETURNING
        VALUE(rv_text) TYPE string
      RAISING
        /mbtools/cx_exception.
    METHODS get_update_admin
      IMPORTING
        !io_tool       TYPE REF TO /mbtools/cl_tool
      RETURNING
        VALUE(rv_text) TYPE string
      RAISING
        /mbtools/cx_exception.
    METHODS restart.
    METHODS get_tool_from_param
      IMPORTING
        !iv_name       TYPE string
      RETURNING
        VALUE(ro_tool) TYPE REF TO /mbtools/cl_tool
      RAISING
        /mbtools/cx_exception.
    METHODS validate_tool
      IMPORTING
        !iv_action TYPE clike
        !io_tool   TYPE REF TO /mbtools/cl_tool
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS build_menu
      IMPORTING
        !iv_mode       TYPE c OPTIONAL
      RETURNING
        VALUE(ro_menu) TYPE REF TO /mbtools/cl_html_toolbar.
    METHODS register_thumbnail
      IMPORTING
        !io_tool         TYPE REF TO /mbtools/cl_tool
      RETURNING
        VALUE(rv_result) TYPE string
      RAISING
        /mbtools/cx_exception.
    METHODS render_actions
      IMPORTING
        !io_tool       TYPE REF TO /mbtools/cl_tool
      RETURNING
        VALUE(ri_html) TYPE REF TO /mbtools/if_html
      RAISING
        /mbtools/cx_exception.
    METHODS render_bundle
      IMPORTING
        !iv_title      TYPE string
      RETURNING
        VALUE(ri_html) TYPE REF TO /mbtools/if_html
      RAISING
        /mbtools/cx_exception.
    METHODS render_bundles
      RETURNING
        VALUE(ri_html) TYPE REF TO /mbtools/if_html
      RAISING
        /mbtools/cx_exception.
    METHODS render_tools
      RETURNING
        VALUE(ri_html) TYPE REF TO /mbtools/if_html
      RAISING
        /mbtools/cx_exception.
    METHODS render_tool
      IMPORTING
        !iv_title      TYPE string
      RETURNING
        VALUE(ri_html) TYPE REF TO /mbtools/if_html
      RAISING
        /mbtools/cx_exception.
    METHODS render_tool_details
      RETURNING
        VALUE(ri_html) TYPE REF TO /mbtools/if_html
      RAISING
        /mbtools/cx_exception.
ENDCLASS.



CLASS /mbtools/cl_gui_page_main IMPLEMENTATION.


  METHOD /mbtools/if_gui_event_handler~on_event.

    DATA lo_tool TYPE REF TO /mbtools/cl_tool.

    IF ii_event->mv_action <> /mbtools/if_actions=>tool_install.
      lo_tool = get_tool_from_param( ii_event->get_param( 'name' ) ).

      validate_tool(
        io_tool   = lo_tool
        iv_action = ii_event->mv_action ).
    ENDIF.

    CASE ii_event->mv_action.

      WHEN /mbtools/if_actions=>tools_check.
        IF /mbtools/cl_tool_manager=>action_tools( /mbtools/if_actions=>tools_check ) = abap_true.
          MESSAGE 'Check for latest versions completed' TYPE 'S'.
        ENDIF.
        rs_handled-state = /mbtools/cl_gui=>c_event_state-re_render.

      WHEN /mbtools/if_actions=>tools_update.
        IF /mbtools/cl_tool_manager=>action_tools( /mbtools/if_actions=>tool_update ) = abap_true.
          MESSAGE 'Update to latest versions completed' TYPE 'S'.
          restart( ).
        ENDIF.
        rs_handled-state = /mbtools/cl_gui=>c_event_state-re_render.

      WHEN /mbtools/if_actions=>tool_docs.
        /mbtools/cl_utilities=>call_browser( lo_tool->get_url_docs( ) ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_actions=>tool_info.
        /mbtools/cl_utilities=>call_browser( lo_tool->get_url_tool( ) ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_actions=>tool_launch.
        lo_tool->launch( ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-re_render.

      WHEN /mbtools/if_actions=>tool_activate.
        lo_tool->activate( ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-re_render.

      WHEN /mbtools/if_actions=>tool_deactivate.
        lo_tool->deactivate( ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-re_render.

      WHEN /mbtools/if_actions=>tool_install.
        IF /mbtools/cl_tool_manager=>install( ii_event->get_param( 'name' ) ) = abap_true.
          restart( ).
        ENDIF.
        rs_handled-state = /mbtools/cl_gui=>c_event_state-re_render.

      WHEN /mbtools/if_actions=>tool_update.
        /mbtools/cl_tool_manager=>update( lo_tool ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-re_render.

      WHEN /mbtools/if_actions=>tool_uninstall.
        IF /mbtools/cl_tool_manager=>uninstall( lo_tool ) = abap_true.
          restart( ).
        ENDIF.
        rs_handled-state = /mbtools/cl_gui=>c_event_state-re_render.

      WHEN /mbtools/if_actions=>tool_changelog.
        rs_handled-state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_actions=>tool_license.
        IF ii_event->get_param( 'action' ) = /mbtools/if_actions=>license_add AND
           ii_event->get_param( 'license' ) IS NOT INITIAL.
          IF lo_tool->license_add( ii_event->get_param( 'license' ) ) = abap_true.
            MESSAGE 'License saved and activated successfully' TYPE 'S'.
          ENDIF.
        ELSEIF lo_tool->license_remove( ) = abap_true.
          MESSAGE 'License deactivated and removed successfully' TYPE 'S'.
        ENDIF.
        rs_handled-state = /mbtools/cl_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.


  METHOD /mbtools/if_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Main'.

    ls_hotkey_action-description = |Add <u>N</u>ew Tool|.
    ls_hotkey_action-action      = /mbtools/if_actions=>tool_install.
    ls_hotkey_action-hotkey      = |n|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |<u>C</u>heck for Updates|.
    ls_hotkey_action-action      = /mbtools/if_actions=>tool_check.
    ls_hotkey_action-hotkey      = |c|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |<u>U</u>pdate All Tools|.
    ls_hotkey_action-action      = /mbtools/if_actions=>tool_update.
    ls_hotkey_action-hotkey      = |u|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Edit <u>L</u>icense Keys|.
    ls_hotkey_action-action      = /mbtools/if_actions=>go_license.
    ls_hotkey_action-hotkey      = |l|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |<u>A</u>dministration|.
    ls_hotkey_action-action      = /mbtools/if_actions=>go_admin.
    ls_hotkey_action-hotkey      = |a|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |<u>Q</u>uit|.
    ls_hotkey_action-action      = /mbtools/if_actions=>quit.
    ls_hotkey_action-hotkey      = |q|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |<u>E</u>xit Admin|.
    ls_hotkey_action-action      = /mbtools/if_actions=>go_home.
    ls_hotkey_action-hotkey      = |x|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD /mbtools/if_gui_renderable~render.

    gui_services( )->register_event_handler( me ).
    gui_services( )->get_hotkeys_ctl( )->register_hotkeys( me ).

    ri_html = /mbtools/cl_html=>create( ).

    CASE mv_mode.
      WHEN c_mode-user.
        ri_html->add( render_tools( ) ).
        ri_html->add( render_tool_details( ) ).
      WHEN c_mode-admin.
        ri_html->add( render_bundles( ) ).
        ri_html->add( render_tool_details( ) ).
      WHEN c_mode-license.
        ri_html->add( render_bundles( ) ).
    ENDCASE.

    gui_services( )->cache_all_assets( mo_asset_manager ).

  ENDMETHOD.


  METHOD build_menu.

    DATA:
      lo_tools_menu   TYPE REF TO /mbtools/cl_html_toolbar,
      lo_support_menu TYPE REF TO /mbtools/cl_html_toolbar,
      lo_bar_menu     TYPE REF TO /mbtools/cl_html_toolbar.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-main'.

    CREATE OBJECT lo_support_menu.

    lo_support_menu->add(
      iv_txt = 'FAQ'
      iv_act = /mbtools/if_actions=>go_faq
    )->add(
      iv_txt = 'Documentation'
      iv_act = /mbtools/if_actions=>mbt_docs
    )->add(
      iv_txt = 'Ticket'
      iv_act = /mbtools/if_actions=>mbt_support ).

    CREATE OBJECT lo_bar_menu.

    CASE iv_mode.

      WHEN c_mode-user.

        lo_bar_menu->add(
          iv_txt = 'Admin'
          iv_act = /mbtools/if_actions=>go_admin ).

      WHEN c_mode-admin.

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
          iv_txt = 'Edit License Keys'
          iv_act = /mbtools/if_actions=>go_license ).

        ro_menu->add(
          iv_txt = 'Tools'
          io_sub = lo_tools_menu ).

        lo_bar_menu->add(
          iv_txt = 'Exit Admin'
          iv_act = /mbtools/if_actions=>go_home
        )->add(
          iv_txt = 'Registry'
          iv_act = /mbtools/if_actions=>run_transaction && '?transaction=/MBTOOLS/REG' ).

      WHEN c_mode-license.

        lo_bar_menu->add(
          iv_txt = 'Exit License Keys'
          iv_act = /mbtools/if_actions=>go_admin ).

    ENDCASE.

    lo_bar_menu->add(
      iv_txt = 'Website'
      iv_typ = /mbtools/if_html=>c_action_type-url
      iv_act = /mbtools/if_definitions=>c_www_home
    )->add(
      iv_txt = 'About'
      iv_act = /mbtools/if_actions=>go_about ).

    ro_menu->add(
      iv_txt = 'Support'
      io_sub = lo_support_menu
    )->add(
      iv_txt   = /mbtools/cl_gui_buttons=>admin( )
      iv_title = 'MBT'
      io_sub   = lo_bar_menu ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    CREATE OBJECT mo_asset_manager TYPE /mbtools/cl_gui_asset_manager.

    mv_mode = iv_mode.

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO /mbtools/cl_gui_page_main.

    CREATE OBJECT lo_component EXPORTING iv_mode = iv_mode.

    CASE iv_mode.

      WHEN c_mode-user.

        ri_page = /mbtools/cl_gui_page=>create(
          iv_has_logo        = abap_false
          iv_has_banner      = abap_true
          iv_page_title      = ''
          io_page_menu       = build_menu( c_mode-user )
          ii_child_component = lo_component ).

      WHEN c_mode-admin.

        ri_page = /mbtools/cl_gui_page=>create(
          iv_has_logo        = abap_true
          iv_has_banner      = abap_false
          iv_page_title      = 'Administration'
          io_page_menu       = build_menu( c_mode-admin )
          ii_child_component = lo_component ).

      WHEN c_mode-license.

        ri_page = /mbtools/cl_gui_page=>create(
          iv_has_logo        = abap_true
          iv_has_banner      = abap_false
          iv_page_title      = 'License Keys'
          io_page_menu       = build_menu( c_mode-license )
          ii_child_component = lo_component ).

    ENDCASE.

  ENDMETHOD.


  METHOD get_description_admin.

    DATA:
      li_html        TYPE REF TO /mbtools/if_html,
      lv_version     TYPE string,
      lv_description TYPE string.

    li_html = /mbtools/cl_html=>create( ).

    IF io_tool->get_new_version( ) IS INITIAL.
      lv_version = |toggleDisplay('changelog-{ io_tool->get_name( ) }')|.
      lv_version = li_html->a( iv_act = lv_version
                               iv_typ = /mbtools/if_html=>c_action_type-onclick
                               iv_txt = io_tool->get_version( ) ).
    ELSE.
      lv_version = io_tool->get_version( ).
    ENDIF.

    lv_description = |toggleDisplay('details-{ io_tool->get_name( ) }')|.
    lv_description = li_html->a( iv_act = lv_description
                                 iv_typ = /mbtools/if_html=>c_action_type-onclick
                                 iv_txt = |View description| ).

    rv_text = |Version: { lv_version } \| { lv_description } \| Last update: { io_tool->get_last_update( ) }|.

  ENDMETHOD.


  METHOD get_description_license.

    DATA ls_license TYPE /mbtools/if_definitions=>ty_license.

    ls_license = io_tool->get_license( ).

    IF ls_license-valid = abap_true.

      IF ls_license-expire IS NOT INITIAL.
        rv_text = /mbtools/cl_datetime=>get_long_date( ls_license-expire ).
        rv_text = |expires <span class="has-mbt-green-color">{ rv_text }</span>|.
      ELSE.
        rv_text = |<span class="has-mbt-green-color">does not expire</span>|.
      ENDIF.
      rv_text = |Your license key { rv_text }|.

    ELSEIF ls_license-key IS NOT INITIAL.

      rv_text = |<span class="has-mbt-red-color">expired</span>|.
      rv_text = |Your license key has { rv_text }. Please enter a valid key.|.

    ELSE.

      rv_text = 'To receive updates, please enter your valid license key'.

    ENDIF.

  ENDMETHOD.


  METHOD get_tool_from_param.

    DATA lv_name TYPE string.

    IF iv_name IS INITIAL.
      RETURN.
    ENDIF.

    lv_name = iv_name.
    REPLACE ALL OCCURRENCES OF '_' IN lv_name WITH ` `.

    ro_tool = /mbtools/cl_tool_manager=>factory( lv_name ).
    IF ro_tool IS NOT BOUND.
      /mbtools/cx_exception=>raise( |Tool { lv_name } could not be instanciated| ).
    ENDIF.

  ENDMETHOD.


  METHOD get_update_admin.

    DATA:
      li_html         TYPE REF TO /mbtools/if_html,
      lv_version      TYPE string,
      lv_install_time TYPE string,
      lv_update_time  TYPE string,
      lv_changelog    TYPE string,
      lv_update       TYPE string.

    li_html = /mbtools/cl_html=>create( ).

    lv_version = io_tool->get_new_version( ).

    IF lv_version IS INITIAL.

      lv_install_time = io_tool->get_install_time( abap_true ).
      lv_update_time = io_tool->get_last_update( abap_true ).

      IF strlen( lv_update_time ) >= 8 AND lv_update_time(8) = sy-datum.
        rv_text = li_html->icon(
          iv_name = 'check/green'
          iv_hint = 'Tool updated' ) && |Updated today|.
      ELSEIF strlen( lv_install_time ) >= 8 AND lv_install_time(8) = sy-datum.
        rv_text = li_html->icon(
          iv_name = 'check/green'
          iv_hint = 'Tool installed' ) && |Installed today|.
      ENDIF.

    ELSE.

      lv_changelog = |toggleDisplay('changelog-{ io_tool->get_name( ) }')|.
      lv_changelog = li_html->a(
        iv_act = lv_changelog
        iv_typ = /mbtools/if_html=>c_action_type-onclick
        iv_txt = |View version { lv_version } details| ).

      IF io_tool->is_licensed( ) = abap_true.
        lv_update = | or | && li_html->a(
          iv_act = |{ /mbtools/if_actions=>tool_update }?name={ io_tool->get_name( ) }|
          iv_txt = |update now| ).
      ELSE.
        lv_update = |. Automatic update is unavailable. | && li_html->a(
          iv_act = |{ /mbtools/if_actions=>go_license }|
          iv_txt = |Enter valid license key for automatic updates| ).
      ENDIF.

      rv_text = li_html->icon(
        iv_name = 'recycle/orange'
        iv_hint = 'Update tool' ) && |There is a new version available. |.
      rv_text = rv_text && |{ lv_changelog }{ lv_update }.|.

    ENDIF.

  ENDMETHOD.


  METHOD register_thumbnail.

    rv_result = 'img/' && io_tool->get_name( ) && '.jpg'.

    mo_asset_manager->register_asset(
      iv_url       = rv_result
      iv_type      = 'image/jpg'
      iv_mime_name = io_tool->get_thumbnail( ) ).

  ENDMETHOD.


  METHOD render_actions.

    DATA:
      lo_form   TYPE REF TO /mbtools/cl_html_form,
      lo_values TYPE REF TO /mbtools/cl_string_map.

    ri_html = /mbtools/cl_html=>create( ).

    CASE mv_mode.

      WHEN c_mode-user.

        IF io_tool->has_launch( ) = abap_true.
          ri_html->add_a(
            iv_act = |{ /mbtools/if_actions=>tool_launch }?name={ io_tool->get_name( ) }|
            iv_txt = ri_html->icon(
              iv_name  = 'rocket/black'
              iv_hint  = 'Launch tool' ) ).
        ENDIF.

        IF io_tool->is_bundle( ) = abap_false.
          ri_html->add_a(
            iv_act = |toggleDisplay('details-{ io_tool->get_name( ) }')|
            iv_typ = /mbtools/if_html=>c_action_type-onclick
            iv_txt = ri_html->icon(
              iv_name  = 'info/black'
              iv_hint  = 'View tool infos' ) ).

          ri_html->add_a(
            iv_act = |{ /mbtools/if_actions=>tool_docs }?name={ io_tool->get_name( ) }|
            iv_txt = ri_html->icon(
              iv_name  = 'question/black'
              iv_hint  = 'Display tool documentation' ) ).
        ENDIF.

      WHEN c_mode-admin.

        IF io_tool->is_bundle( ) = abap_false.
          IF io_tool->is_active( ) = abap_false.
            ri_html->add_a(
              iv_act = |{ /mbtools/if_actions=>tool_activate }?name={ io_tool->get_name( ) }|
              iv_txt = ri_html->icon(
                iv_name  = 'fire-alt/black'
                iv_hint  = 'Activate tool' ) ).
            ri_html->add_a(
              iv_act = |{ /mbtools/if_actions=>tool_uninstall }?name={ io_tool->get_name( ) }|
              iv_txt = ri_html->icon(
                iv_name  = 'trash-alt/black'
                iv_hint  = 'Uninstall tool' ) ).
          ELSE.
            ri_html->add_a(
              iv_act = |{ /mbtools/if_actions=>tool_deactivate }?name={ io_tool->get_name( ) }|
              iv_txt = ri_html->icon(
                iv_name  = 'snowflake/black'
                iv_hint  = 'Deactivate tool' ) ).
          ENDIF.

        ENDIF.

        ri_html->add_a(
          iv_act = |{ /mbtools/if_actions=>tool_info }?name={ io_tool->get_name( ) }|
          iv_txt = ri_html->icon(
            iv_name  = 'globe/black'
            iv_hint  = 'Show more information about tool' ) ).

      WHEN c_mode-license.

        IF io_tool->is_bundle( ) = abap_false.
          lo_form = /mbtools/cl_html_form=>create(
            iv_form_id     = |license-{ io_tool->get_name( ) }|
            iv_form_action = /mbtools/if_actions=>tool_license ).

          CREATE OBJECT lo_values.

          lo_values->set(
            iv_key = 'name'
            iv_val = io_tool->get_name( ) ).
          lo_values->set(
            iv_key = 'license'
            iv_val = io_tool->get_license( )-key ).

          lo_form->hidden( 'name' ).

          lo_form->text(
            iv_name        = 'license'
            iv_min         = 32
            iv_max         = 32
            iv_required    = abap_false
            iv_label       = ''
            iv_hint        = 'You can find the license key in your account area on the MBT website'
            iv_placeholder = 'License Key' ).

          lo_form->command(
            iv_cmd_type = /mbtools/cl_html_form=>c_cmd_type-button
            iv_action   = /mbtools/if_actions=>license_add
            iv_label    = ri_html->icon(
              iv_name  = 'save/black'
              iv_hint  = 'Save and activate license key' ) ).

          lo_form->command(
            iv_cmd_type = /mbtools/cl_html_form=>c_cmd_type-button
            iv_action   = /mbtools/if_actions=>license_remove
            iv_label    = ri_html->icon(
              iv_name  = 'trash-alt/black'
              iv_hint  = 'Deactivate and remove license key' ) ).

          ri_html->add( lo_form->render(
            iv_form_class = 'tool-license'
            io_values     = lo_values ) ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD render_bundle.

    DATA:
      lo_bundle TYPE REF TO /mbtools/cl_tool,
      ls_tool   TYPE /mbtools/if_tool=>ty_manifest,
      lt_tools  TYPE /mbtools/if_tool=>ty_manifests.

    ri_html = /mbtools/cl_html=>create( ).

    lo_bundle = /mbtools/cl_tool_manager=>factory( iv_title ).

    lt_tools = /mbtools/cl_tool_manager=>select(
      iv_bundle_id = lo_bundle->get_bundle_id( )
      iv_admin     = abap_true ).

    IF lt_tools IS INITIAL.
      RETURN.
    ENDIF.

    ri_html->add( render_tool( iv_title ) ).

    ri_html->add( '<div class="tools">' ).
    ri_html->add( '<ul>' ).
    LOOP AT lt_tools INTO ls_tool.
      ri_html->add( '<li>' ).
      ri_html->add( render_tool( ls_tool-name ) ).
      ri_html->add( '</li>' ).
    ENDLOOP.
    ri_html->add( '</ul>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_bundles.

    DATA:
      ls_bundle  TYPE /mbtools/if_tool=>ty_manifest,
      lt_bundles TYPE /mbtools/if_tool=>ty_manifests.

    lt_bundles = /mbtools/cl_tool_manager=>select(
      iv_get_bundles = abap_true
      iv_get_tools   = abap_false
      iv_admin       = abap_true ).

    ri_html = /mbtools/cl_html=>create( ).

    ri_html->add( '<div class="intro">' ).
    CASE mv_mode.
      WHEN c_mode-admin.
        ri_html->add( 'You can activate, deactivate, or uninstall any of the installed tools below.' &&
                      ' Use the "Tools" menu to add a new tool or update any of your exsting tools.' ).
      WHEN c_mode-license.
        ri_html->add( 'Enter your license keys to receive updates for purchased tools.' &&
                      ' If your license key has expired, please renew your license.' ). "##TODO
    ENDCASE.
    ri_html->add( '</div>' ).

    ri_html->add( '<div class="bundles">' ).
    ri_html->add( '<ul>' ).

    LOOP AT lt_bundles INTO ls_bundle.
      ri_html->add( '<li>' ).
      ri_html->add( render_bundle( ls_bundle-name ) ).
      ri_html->add( '</li>' ).
    ENDLOOP.

    ri_html->add( '</ul>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_tool.

    DATA:
      lo_tool        TYPE REF TO /mbtools/cl_tool,
      lv_description TYPE string,
      lv_update      TYPE string,
      lv_class       TYPE string,
      lv_img         TYPE string.

    lo_tool = /mbtools/cl_tool_manager=>factory( iv_title ).

    lv_class = 'tool-row'.
    IF lo_tool->is_bundle( ) = abap_false AND lo_tool->is_active( ) = abap_false.
      lv_class = lv_class && | inactive|.
    ELSE.
      lv_class = lv_class && | active|.
    ENDIF.

    lv_img = |<img src="{ register_thumbnail( lo_tool ) }" alt="{ lo_tool->get_title( ) }">|.

    ri_html = /mbtools/cl_html=>create( ).

    ri_html->add( |<table><tr class="{ lv_class }">| ).

    ri_html->add( |<td class="tool-thumbnail">{ lv_img }</td>| ).

    ri_html->add( '<td class="tool-details">' ).
    ri_html->add( |<span class="title">{ lo_tool->get_title( ) }</span>| ).

    CASE mv_mode.
      WHEN c_mode-user.
        lv_description = lo_tool->get_description( ).

      WHEN c_mode-admin.
        IF lo_tool->is_bundle( ) = abap_false.
          lv_description = get_description_admin( lo_tool ).
          lv_update = get_update_admin( lo_tool ).
        ENDIF.

      WHEN c_mode-license.
        IF lo_tool->is_bundle( ) = abap_false.
          lv_description = get_description_license( lo_tool ).
        ENDIF.
    ENDCASE.

    IF lv_description IS NOT INITIAL.
      ri_html->add( |<br><span class="description">{ lv_description }</span>| ).
    ENDIF.
    IF lv_update IS NOT INITIAL.
      lv_class = 'update'.
      IF lv_update NS 'href'.
        lv_class = 'update_success'.
      ENDIF.
      ri_html->add( |<br><br><span class="{ lv_class }">{ lv_update }</span>| ).
    ENDIF.
    ri_html->add( '</td>' ).

    ri_html->add( '<td class="tool-actions">' ).
    ri_html->add( render_actions( lo_tool ) ).
    ri_html->add( '</td>' ).

    ri_html->add( '</tr></table>' ).

  ENDMETHOD.


  METHOD render_tools.

    DATA:
      ls_tool  TYPE /mbtools/if_tool=>ty_manifest,
      lt_tools TYPE /mbtools/if_tool=>ty_manifests.

    lt_tools = /mbtools/cl_tool_manager=>select( ).

    ri_html = /mbtools/cl_html=>create( ).

    ri_html->add( '<div class="bundles">' ). "format like bundles (w/o indent)
    ri_html->add( '<ul>' ).

    IF lt_tools IS INITIAL.
      ri_html->add( '<li>' ).
      ri_html->add( '<h2>Welcome to Marc Bernard Tools!</h2>' ).
      ri_html->add( |<p>There are currently no active tools in your system. Go to | ).
      ri_html->add_a(
        iv_txt = 'Administration'
        iv_act = /mbtools/if_actions=>go_admin ).
      ri_html->add( | to add new tools or activate any tools that are already installed.| ).
      ri_html->add( '</li>' ).
    ELSE.
      LOOP AT lt_tools INTO ls_tool.
        ri_html->add( '<li>' ).
        ri_html->add( render_tool( ls_tool-name ) ).
        ri_html->add( '</li>' ).
      ENDLOOP.
    ENDIF.

    ri_html->add( '</ul>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_tool_details.

    DATA:
      lo_tool  TYPE REF TO /mbtools/cl_tool,
      ls_tool  TYPE /mbtools/if_tool=>ty_manifest,
      lt_tools TYPE /mbtools/if_tool=>ty_manifests,
      lv_html  TYPE string,
      li_html  TYPE REF TO /mbtools/if_html.

    ri_html = /mbtools/cl_html=>create( ).

    CASE mv_mode.
      WHEN c_mode-user.
        lt_tools = /mbtools/cl_tool_manager=>select( ).
      WHEN c_mode-admin.
        lt_tools = /mbtools/cl_tool_manager=>select( iv_admin = abap_true ).
      WHEN c_mode-license.
        RETURN.
    ENDCASE.

    LOOP AT lt_tools INTO ls_tool.
      lo_tool = ls_tool-manager.

      " Description
      li_html = /mbtools/cl_html=>create( ).

      lv_html = lo_tool->get_html_description( ).
      IF lv_html IS INITIAL.
        li_html->add( 'Details become available after first update run' ).
        li_html->add( '(see Admin > Tools > Check for Updates)' ).
      ELSE.
        li_html->add( lv_html ).
      ENDIF.

      ri_html->add( /mbtools/cl_html_lib=>render_infopanel(
        iv_div_id  = |details-{ lo_tool->get_name( ) }|
        iv_title   = |Description of { lo_tool->get_title( ) }|
        ii_content = li_html ) ).

      " Changelog
      li_html = /mbtools/cl_html=>create( ).

      lv_html = lo_tool->get_html_changelog( ).
      IF lv_html IS NOT INITIAL.
        li_html->add( lv_html ).

        ri_html->add( /mbtools/cl_html_lib=>render_infopanel(
          iv_div_id  = |changelog-{ lo_tool->get_name( ) }|
          iv_title   = |Changelog for { lo_tool->get_title( ) }|
          ii_content = li_html ) ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD restart.

    /mbtools/cl_utilities=>set_user_parameter(
      iv_parameter = '/MBTOOLS/MODE'
      iv_value     = /mbtools/if_actions=>go_admin ).

    SUBMIT /mbtools/mbt.

  ENDMETHOD.


  METHOD validate_tool.

    " Some actions are not allowed or need confirmation for MBT Base
    DATA lv_answer TYPE c.

    CASE iv_action.
      WHEN /mbtools/if_actions=>tool_deactivate.

        IF io_tool->is_base( ) = abap_true.
          IF /mbtools/cl_tool_manager=>is_base_only( ) = abap_true.
            lv_answer = /mbtools/cl_gui_factory=>get_popups( )->popup_to_confirm(
              iv_titlebar       = 'Deactivate Tool'
              iv_text_question  = |Are you sure you want to deactivate { io_tool->get_title( ) }?|
              iv_default_button = '2' ).
            IF lv_answer <> '1'.
              /mbtools/cx_exception=>raise( 'Action cancelled' ).
            ENDIF.
          ELSE.
            /mbtools/cx_exception=>raise( |You have to deactivate all other tools first,| &&
                                          | before you can deactivate { io_tool->get_title( ) }| ).
          ENDIF.
        ENDIF.

      WHEN /mbtools/if_actions=>tool_uninstall.

        IF io_tool->is_base( ) = abap_true AND /mbtools/cl_tool_manager=>is_base_only( ) = abap_false.
          /mbtools/cx_exception=>raise( |You have to uninstall all other tools first,| &&
                                        | before you can uninstall { io_tool->get_title( ) }| ).
        ENDIF.

        lv_answer = /mbtools/cl_gui_factory=>get_popups( )->popup_to_confirm(
          iv_titlebar       = 'Uninstall Tool'
          iv_text_question  = |Are you sure you want to uninstall { io_tool->get_title( ) }?|
          iv_default_button = '2' ).
        IF lv_answer <> '1'.
          /mbtools/cx_exception=>raise( 'Action cancelled' ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
