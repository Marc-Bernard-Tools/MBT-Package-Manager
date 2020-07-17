************************************************************************
* /MBTOOLS/CL_GUI_PAGE
* MBT GUI Page
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************
CLASS /mbtools/cl_gui_page DEFINITION
  PUBLIC
  INHERITING FROM /mbtools/cl_gui_component
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /mbtools/if_gui_renderable .
    INTERFACES /mbtools/if_gui_event_handler .
    INTERFACES /mbtools/if_gui_error_handler .

    METHODS constructor
      RAISING
        /mbtools/cx_exception .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_control,
        page_title TYPE string,
        page_menu  TYPE REF TO /mbtools/cl_html_toolbar,
      END OF  ty_control .

    DATA ms_control TYPE ty_control .

    METHODS render_content
          ABSTRACT
      RETURNING
        VALUE(ri_html) TYPE REF TO /mbtools/if_html
      RAISING
        /mbtools/cx_exception .
  PRIVATE SECTION.

    DATA mx_error TYPE REF TO /mbtools/cx_exception .
    DATA mo_exception_viewer TYPE REF TO /mbtools/cl_exception_viewer .

    METHODS render_deferred_parts
      IMPORTING
        !ii_html          TYPE REF TO /mbtools/if_html
        !iv_part_category TYPE string
      RAISING
        /mbtools/cx_exception .
    METHODS html_head
      RETURNING
        VALUE(ri_html) TYPE REF TO /mbtools/if_html .
    METHODS header
      RETURNING
        VALUE(ri_html) TYPE REF TO /mbtools/if_html .
    METHODS footer
      RETURNING
        VALUE(ri_html) TYPE REF TO /mbtools/if_html .
    METHODS render_link_hints
      IMPORTING
        !ii_html TYPE REF TO /mbtools/if_html
      RAISING
        /mbtools/cx_exception .
    METHODS render_command_palettes
      IMPORTING
        !ii_html TYPE REF TO /mbtools/if_html
      RAISING
        /mbtools/cx_exception .
    METHODS render_hotkey_overview
      RETURNING
        VALUE(ro_html) TYPE REF TO /mbtools/if_html
      RAISING
        /mbtools/cx_exception .
    METHODS render_error_message_box
      RETURNING
        VALUE(ro_html) TYPE REF TO /mbtools/cl_html
      RAISING
        /mbtools/cx_exception .
    METHODS scripts
      RETURNING
        VALUE(ro_html) TYPE REF TO /mbtools/cl_html
      RAISING
        /mbtools/cx_exception .
ENDCLASS.



CLASS /MBTOOLS/CL_GUI_PAGE IMPLEMENTATION.


  METHOD /mbtools/if_gui_error_handler~handle_error.

    mx_error = ix_error.
    rv_handled = abap_true.

  ENDMETHOD.


  METHOD /mbtools/if_gui_event_handler~on_event.

    CASE iv_action.
      WHEN /mbtools/if_definitions=>c_action-goto_source.

        IF mo_exception_viewer IS BOUND.
          mo_exception_viewer->goto_source( ).
        ENDIF.
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_definitions=>c_action-show_callstack.

        IF mo_exception_viewer IS BOUND.
          mo_exception_viewer->show_callstack( ).
        ENDIF.
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_definitions=>c_action-goto_message.

        IF mo_exception_viewer IS BOUND.
          mo_exception_viewer->goto_message( ).
        ENDIF.
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.


  METHOD /mbtools/if_gui_renderable~render.

    DATA: li_script TYPE REF TO /mbtools/if_html.

    gui_services( )->register_event_handler( me ).

    " Real page
    CREATE OBJECT ri_html TYPE /mbtools/cl_html.

    ri_html->add( '<!DOCTYPE html>' ).                      "#EC NOTEXT
    ri_html->add( '<html lang="en">' ).                     "#EC NOTEXT
    ri_html->add( html_head( ) ).

    ri_html->add( '<body>' ).                               "#EC NOTEXT
    ri_html->add( '<div class="outer" id="top">' ).         "#EC NOTEXT
    ri_html->add( '<div class="wrapper">' ).                "#EC NOTEXT

    ri_html->add( header( ) ).

    ri_html->add( '<div id="main" class="main">' ).         "#EC NOTEXT
    ri_html->add( render_content( ) ). " TODO -> render child
    ri_html->add( '</div><!--main-->' ).                               "#EC NOTEXT

    ri_html->add( render_hotkey_overview( ) ).
    ri_html->add( render_error_message_box( ) ).

    render_deferred_parts(
      ii_html          = ri_html
      iv_part_category = c_html_parts-hidden_forms ).

    ri_html->add( footer( ) ).

    ri_html->add( '</div><!--wrapper-->' ).                               "#EC NOTEXT
    ri_html->add( '</div><!--outer-->' ).                               "#EC NOTEXT

    li_script = scripts( ).

    IF li_script IS BOUND AND li_script->is_empty( ) = abap_false.
      ri_html->add( '<script type="text/javascript">' ).
      ri_html->add( li_script ).
      ri_html->add( 'confirmInitialized();' ).
      ri_html->add( '</script>' ).
    ENDIF.

    ri_html->add( '</body>' ).                              "#EC NOTEXT
    ri_html->add( '</html>' ).                              "#EC NOTEXT

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

  ENDMETHOD.


  METHOD footer.

    CREATE OBJECT ri_html TYPE /mbtools/cl_html.

    ri_html->add( '<div id="footer" class="footer">' ).

    ri_html->add( '<table class="w100"><tr>' ).

    ri_html->add( '<td class="w40"></td>' ).  " spacer

    ri_html->add( '<td class="center">' ).
    ri_html->add( '<div class="logo">' ).
    ri_html->add( '<img src="img/logo.png" alt="MBT Logo">' ).
    ri_html->add( '</div>' ).
    ri_html->add( |<div class="version">{ /mbtools/cl_tool_bc=>c_tool-version }</div>| ).
    ri_html->add( '</td>' ).

    ri_html->add( '<td id="debug-output" class="w40"></td>' ).

    ri_html->add( '</tr></table>' ).

    ri_html->add( '</div><!--footer-->' ).

  ENDMETHOD.


  METHOD header.

    CREATE OBJECT ri_html TYPE /mbtools/cl_html.

    ri_html->add( '<div id="header" class="header">' ).

    ri_html->add( '<div class="logo">' ).
    ri_html->add( '<img src="img/logo.png" alt="MBT Logo">' ).
    ri_html->add( '</div>' ).

    ri_html->add( |<div class="page-title"><!--span class="spacer">&#x25BA;</span-->{ ms_control-page_title }</div>| ).

    IF ms_control-page_menu IS BOUND.
      ri_html->add( ms_control-page_menu->render( iv_right = abap_true ) ).
    ENDIF.

    ri_html->add( '</div><!--header-->' ).

  ENDMETHOD.


  METHOD html_head.

    CREATE OBJECT ri_html TYPE /mbtools/cl_html.

    ri_html->add( '<head>' ).                               "#EC NOTEXT

    ri_html->add( '<meta http-equiv="content-type" content="text/html; charset=utf-8">' ). "#EC NOTEXT
    ri_html->add( '<meta http-equiv="X-UA-Compatible" content="IE=11,10,9,8" />' ). "#EC NOTEXT

    ri_html->add( '<title>Marc Bernard Tools</title>' ).    "#EC NOTEXT
    ri_html->add( '<link rel="stylesheet" type="text/css" href="css/common.css">' ).
    ri_html->add( '<link rel="stylesheet" type="text/css" href="css/icons.css">' ).

    ri_html->add( '<script type="text/javascript" src="js/common.js"></script>' ). "#EC NOTEXT

    ri_html->add( '</head>' ).                              "#EC NOTEXT

  ENDMETHOD.


  METHOD render_command_palettes.

    ii_html->add( 'var gCommandPalette = new CommandPalette(enumerateToolbarActions, {' ).
    ii_html->add( '  toggleKey: "F1",' ).
    ii_html->add( '  hotkeyDescription: "Command ..."' ).
    ii_html->add( '});' ).

  ENDMETHOD.


  METHOD render_deferred_parts.

    DATA lt_parts TYPE /mbtools/if_html=>ty_table_of.
    DATA li_part LIKE LINE OF lt_parts.

    lt_parts = gui_services( )->get_html_parts( )->get_parts( iv_part_category ).

    LOOP AT lt_parts INTO li_part.
      ii_html->add( li_part ).
    ENDLOOP.

  ENDMETHOD.


  METHOD render_error_message_box.

    " You should remember that the we have to instantiate ro_html even
    " it's overwritten further down. Because ADD checks whether it's
    " bound.
    CREATE OBJECT ro_html.

    " You should remember that we render the message panel only
    " if we have an error.
    IF mx_error IS NOT BOUND.
      RETURN.
    ENDIF.

    ro_html = /mbtools/cl_html_lib=>render_error_message_box( mx_error ).

    " You should remember that the exception viewer dispatches the events of
    " error message panel
    CREATE OBJECT mo_exception_viewer
      EXPORTING
        ix_error = mx_error.

    " You should remember that we render the message panel just once
    " for each exception/error text.
    CLEAR:
      mx_error.

  ENDMETHOD.


  METHOD render_hotkey_overview.

    DATA lo_hotkeys_component TYPE REF TO /mbtools/if_gui_renderable.

    lo_hotkeys_component ?= gui_services( )->get_hotkeys_ctl( ). " Mmmm ...
    ro_html = lo_hotkeys_component->render( ).

  ENDMETHOD.


  METHOD render_link_hints ##TODO.

  ENDMETHOD.


  METHOD scripts.

    CREATE OBJECT ro_html.

    render_deferred_parts(
      ii_html          = ro_html
      iv_part_category = c_html_parts-scripts ).

    render_link_hints( ro_html ).
    render_command_palettes( ro_html ).

  ENDMETHOD.
ENDCLASS.
