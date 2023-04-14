INTERFACE /mbtools/if_gui_asset_manager
  PUBLIC.

************************************************************************
* Marc Bernard Tools - GUI Asset Manager
*
* Copyright 2014 abapGit Contributors <http://abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  TYPES:
    BEGIN OF ty_web_asset,
      url          TYPE string,
      type         TYPE c LENGTH 50,
      subtype      TYPE c LENGTH 50,
      size         TYPE i,
      content      TYPE xstring,
      mime_content TYPE lvc_t_mime,
      is_cacheable TYPE abap_bool,
    END OF ty_web_asset.
  TYPES:
    ty_web_assets TYPE STANDARD TABLE OF ty_web_asset WITH DEFAULT KEY.

  METHODS get_all_assets
    RETURNING
      VALUE(rt_assets) TYPE ty_web_assets
    RAISING
      /mbtools/cx_exception.

  METHODS get_asset
    IMPORTING
      !iv_url         TYPE string
    RETURNING
      VALUE(rs_asset) TYPE ty_web_asset
    RAISING
      /mbtools/cx_exception.

  METHODS get_text_asset
    IMPORTING
      !iv_url            TYPE string
      !iv_assert_subtype TYPE string OPTIONAL
    RETURNING
      VALUE(rv_asset)    TYPE string
    RAISING
      /mbtools/cx_exception.

  METHODS register_asset
    IMPORTING
      !iv_url       TYPE string
      !iv_type      TYPE string
      !iv_cachable  TYPE abap_bool DEFAULT abap_true
      !iv_mime_name TYPE csequence OPTIONAL
      !iv_base64    TYPE string OPTIONAL
      !iv_inline    TYPE string OPTIONAL
    RAISING
      /mbtools/cx_exception.

ENDINTERFACE.
