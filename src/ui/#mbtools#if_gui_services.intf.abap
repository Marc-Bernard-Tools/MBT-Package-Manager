INTERFACE /mbtools/if_gui_services
  PUBLIC.

************************************************************************
* Marc Bernard Tools - GUI Services
*
* Copyright 2014 abapGit Contributors <http://abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  METHODS cache_asset
    IMPORTING
      !iv_text      TYPE string OPTIONAL
      !iv_xdata     TYPE xstring OPTIONAL
      !it_xdata     TYPE lvc_t_mime OPTIONAL
      !iv_size      TYPE i OPTIONAL
      !iv_url       TYPE string OPTIONAL
      !iv_type      TYPE c
      !iv_subtype   TYPE c
    RETURNING
      VALUE(rv_url) TYPE string
    RAISING
      /mbtools/cx_exception.

  METHODS cache_all_assets
    IMPORTING
      !ii_asset_manager TYPE REF TO /mbtools/if_gui_asset_manager.

  METHODS register_event_handler
    IMPORTING
      !ii_event_handler TYPE REF TO /mbtools/if_gui_event_handler.

  METHODS get_current_page_name
    RETURNING
      VALUE(rv_page_name) TYPE string.

  METHODS get_hotkeys_ctl
    RETURNING
      VALUE(ri_hotkey_ctl) TYPE REF TO /mbtools/if_gui_hotkey_ctl.

  METHODS get_html_parts
    RETURNING
      VALUE(ro_parts) TYPE REF TO /mbtools/cl_html_parts.

ENDINTERFACE.
