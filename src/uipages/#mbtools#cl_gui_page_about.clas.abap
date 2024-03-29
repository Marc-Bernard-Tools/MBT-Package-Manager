CLASS /mbtools/cl_gui_page_about DEFINITION
  PUBLIC
  INHERITING FROM /mbtools/cl_gui_component
  FINAL
  CREATE PUBLIC.

************************************************************************
* Marc Bernard Tools - GUI Page About
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************
  PUBLIC SECTION.

    INTERFACES /mbtools/if_gui_event_handler.
    INTERFACES /mbtools/if_gui_renderable.
    INTERFACES /mbtools/if_gui_hotkeys.

    METHODS constructor
      RAISING
        /mbtools/cx_exception.

    CLASS-METHODS create
      RETURNING
        VALUE(ri_page) TYPE REF TO /mbtools/if_gui_renderable
      RAISING
        /mbtools/cx_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS build_menu
      RETURNING
        VALUE(ro_menu) TYPE REF TO /mbtools/cl_html_toolbar.

    METHODS render_about
      RETURNING
        VALUE(ri_html) TYPE REF TO /mbtools/if_html
      RAISING
        /mbtools/cx_exception.

ENDCLASS.



CLASS /mbtools/cl_gui_page_about IMPLEMENTATION.


  METHOD /mbtools/if_gui_event_handler~on_event.
    RETURN.
  ENDMETHOD.


  METHOD /mbtools/if_gui_hotkeys~get_hotkey_actions.
    RETURN.
  ENDMETHOD.


  METHOD /mbtools/if_gui_renderable~render.

    gui_services( )->register_event_handler( me ).
    gui_services( )->get_hotkeys_ctl( )->register_hotkeys( me ).

    ri_html = /mbtools/cl_html=>create( ).

    ri_html->add( render_about( ) ).

  ENDMETHOD.


  METHOD build_menu.

    DATA:
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

    lo_bar_menu->add(
      iv_txt = 'Home'
      iv_act = /mbtools/if_actions=>go_home
    )->add(
      iv_txt = 'Website'
      iv_act = /mbtools/if_actions=>mbt_website
    )->add(
      iv_txt = 'About'
      iv_act = /mbtools/if_actions=>go_about ).

    ro_menu->add(
      iv_txt = 'Support'
      io_sub = lo_support_menu
    )->add(
      iv_txt = /mbtools/cl_html=>icon( iv_name = 'bars/grey' )
      io_sub = lo_bar_menu ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO /mbtools/cl_gui_page_about.

    CREATE OBJECT lo_component.

    ri_page = /mbtools/cl_gui_page=>create(
      iv_has_logo        = abap_true
      iv_has_banner      = abap_false
      iv_page_title      = 'About'
      io_page_menu       = build_menu( )
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD render_about.

    ri_html = /mbtools/cl_html=>create( ).

    ri_html->add( '<div class="about auto-center wmax600px">' ).

    ri_html->add( '<img src="img/banner_header.png" alt="MBT Banner">' ).

    ri_html->add( |<p>Version: { /mbtools/cl_tool_bc=>c_tool-version }</p>| ).

    " Avoid harvest
    ri_html->add( |<p>Copyright { /mbtools/if_special_chars=>c_copyright } { sy-datum(4) } Marc Bernard Tools</p>| ).

    ri_html->add( |<p>{ ri_html->a( iv_act = /mbtools/if_actions=>mbt_website
                                    iv_txt = /mbtools/if_definitions=>c_www_home ) }</p>| ).

    ri_html->add( |<form method="post">| ).

    ri_html->add( |<input type="submit" name="action" value="License Terms" formaction="sapevent:{
      /mbtools/if_actions=>mbt_license }">| ).

    ri_html->add( |<input type="submit" name="action" value="Back" formaction="sapevent:{
      /mbtools/if_actions=>go_back }" class="main">| ).

    ri_html->add( |</form>| ).

    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
