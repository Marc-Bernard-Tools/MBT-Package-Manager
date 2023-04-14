CLASS /mbtools/cl_gui_factory DEFINITION
  PUBLIC
  CREATE PRIVATE.

************************************************************************
* Marc Bernard Tools - GUI  Factory
*
* Copyright 2014 abapGit Contributors <http://abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS get_asset_manager
      RETURNING
        VALUE(ri_asset_man) TYPE REF TO /mbtools/if_gui_asset_manager
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS get_popups
      RETURNING
        VALUE(ri_popups) TYPE REF TO /mbtools/if_popups .
    CLASS-METHODS get_gui_functions
      RETURNING
        VALUE(ri_gui_functions) TYPE REF TO /mbtools/if_gui_functions .
    CLASS-METHODS get_gui
      RETURNING
        VALUE(ro_gui) TYPE REF TO /mbtools/cl_gui
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS get_gui_services
      RETURNING
        VALUE(ri_gui_services) TYPE REF TO /mbtools/if_gui_services
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS get_frontend_services
      RETURNING
        VALUE(ri_fe_serv) TYPE REF TO /mbtools/if_frontend_services .
    CLASS-METHODS get_html_viewer
      RETURNING
        VALUE(ri_viewer) TYPE REF TO /mbtools/if_html_viewer .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA gi_popups TYPE REF TO /mbtools/if_popups .
    CLASS-DATA gi_gui_functions TYPE REF TO /mbtools/if_gui_functions .
    CLASS-DATA gi_html_viewer TYPE REF TO /mbtools/if_html_viewer .
    CLASS-DATA go_gui TYPE REF TO /mbtools/cl_gui .
    CLASS-DATA gi_fe_services TYPE REF TO /mbtools/if_frontend_services .
    CLASS-DATA gi_gui_services TYPE REF TO /mbtools/if_gui_services.

ENDCLASS.



CLASS /mbtools/cl_gui_factory IMPLEMENTATION.


  METHOD get_asset_manager.

    DATA li_asset_man TYPE REF TO /mbtools/if_gui_asset_manager.

    li_asset_man = /mbtools/cl_gui_asset_manager=>create( ).

    li_asset_man->register_asset(
      iv_url       = 'css/common.css'
      iv_type      = 'text/css'
      iv_mime_name = '/MBTOOLS/MBT_COMMON_CSS' ).

    li_asset_man->register_asset(
      iv_url       = 'js/common.js'
      iv_type      = 'text/javascript'
      iv_mime_name = '/MBTOOLS/MBT_COMMON_JS' ).

    li_asset_man->register_asset(
      iv_url       = 'css/fontawesome.css'
      iv_type      = 'text/css'
      iv_mime_name = '/MBTOOLS/MBT_FONT_AWESOME_CSS' ).

    li_asset_man->register_asset(
      iv_url       = 'fonts/fa-solid-900.woff2'
      iv_type      = 'application/x-font-woff'
      iv_mime_name = '/MBTOOLS/MBT_FONT_AWESOME' ).

    li_asset_man->register_asset(
      iv_url       = 'fonts/fa-solid-900.eot?#iefix'
      iv_type      = 'application/vnd.ms-fontobject'
      iv_mime_name = '/MBTOOLS/MBT_FONT_AWESOME_EOT' ). "for IE

    li_asset_man->register_asset(
      iv_url       = 'fonts/opensans.woff2'
      iv_type      = 'application/x-font-woff'
      iv_mime_name = '/MBTOOLS/MBT_FONT_OPEN_SANS' ).

    li_asset_man->register_asset(
      iv_url       = 'fonts/righteous.woff2'
      iv_type      = 'application/x-font-woff'
      iv_mime_name = '/MBTOOLS/MBT_FONT_RIGHTEOUS' ).

    li_asset_man->register_asset(
      iv_url       = 'img/logo.png'
      iv_type      = 'image/png'
      iv_mime_name = '/MBTOOLS/LOGO' ).

    li_asset_man->register_asset(
      iv_url       = 'img/banner.png'
      iv_type      = 'image/png'
      iv_mime_name = '/MBTOOLS/BANNER' ).

    li_asset_man->register_asset(
       iv_url       = 'img/background.jpg'
       iv_type      = 'image/jpg'
       iv_mime_name = '/MBTOOLS/BACKGROUND' ).

    li_asset_man->register_asset(
      iv_url       = 'img/logo_header.png'
      iv_type      = 'image/png'
      iv_mime_name = '/MBTOOLS/LOGO_HEADER' ).

    li_asset_man->register_asset(
      iv_url       = 'img/banner_header.png'
      iv_type      = 'image/png'
      iv_mime_name = '/MBTOOLS/BANNER_HEADER' ).

    ri_asset_man = li_asset_man.

  ENDMETHOD.


  METHOD get_frontend_services.

    IF gi_fe_services IS INITIAL.
      CREATE OBJECT gi_fe_services TYPE /mbtools/cl_frontend_services.
    ENDIF.

    ri_fe_serv = gi_fe_services.

  ENDMETHOD.


  METHOD get_gui.

    DATA:
      li_hotkey_ctl TYPE REF TO /mbtools/if_gui_hotkey_ctl,
      li_router     TYPE REF TO /mbtools/if_gui_event_handler,
      li_asset_man  TYPE REF TO /mbtools/if_gui_asset_manager.

    DATA lo_html_preprocessor TYPE REF TO /mbtools/cl_gui_html_processor.

    IF go_gui IS INITIAL.
      li_asset_man = get_asset_manager( ).

      CREATE OBJECT lo_html_preprocessor EXPORTING ii_asset_man = li_asset_man.
      lo_html_preprocessor->preserve_css( 'css/icons.css' ).
      lo_html_preprocessor->preserve_css( 'css/common.css' ).

      CREATE OBJECT li_router TYPE /mbtools/cl_gui_router.
      CREATE OBJECT li_hotkey_ctl TYPE /mbtools/cl_hotkeys.

      CREATE OBJECT go_gui
        EXPORTING
          io_component      = li_router
          ii_hotkey_ctl     = li_hotkey_ctl
          ii_html_processor = lo_html_preprocessor
          ii_asset_man      = li_asset_man.
    ENDIF.

    ro_gui = go_gui.

  ENDMETHOD.


  METHOD get_gui_functions.

    IF gi_gui_functions IS INITIAL.
      CREATE OBJECT gi_gui_functions TYPE /mbtools/cl_gui_functions.
    ENDIF.

    ri_gui_functions = gi_gui_functions.

  ENDMETHOD.


  METHOD get_gui_services.

    IF gi_gui_services IS NOT BOUND.
      gi_gui_services ?= get_gui( ).
    ENDIF.

    ri_gui_services = gi_gui_services.

  ENDMETHOD.


  METHOD get_html_viewer.

    IF gi_html_viewer IS BOUND.
      ri_viewer = gi_html_viewer.
      RETURN.
    ENDIF.

    CREATE OBJECT ri_viewer TYPE /mbtools/cl_html_viewer.

  ENDMETHOD.


  METHOD get_popups.

    IF gi_popups IS INITIAL.
      CREATE OBJECT gi_popups TYPE /mbtools/cl_popups.
    ENDIF.

    ri_popups = gi_popups.

  ENDMETHOD.
ENDCLASS.
