INTERFACE /mbtools/if_gui_asset_manager
  PUBLIC .
************************************************************************
* MBT GUI Asset Manager
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************

  TYPES:
    BEGIN OF ty_web_asset,
      url          TYPE w3url,
      type         TYPE char50,
      subtype      TYPE char50,
      content      TYPE xstring,
      is_cacheable TYPE abap_bool,
    END OF ty_web_asset .
  TYPES:
    ty_web_assets TYPE STANDARD TABLE OF ty_web_asset WITH DEFAULT KEY .

  METHODS get_all_assets
    RETURNING
      VALUE(rt_assets) TYPE ty_web_assets
    RAISING
      /mbtools/cx_exception .

  METHODS get_asset
    IMPORTING
      !iv_url         TYPE string
    RETURNING
      VALUE(rs_asset) TYPE ty_web_asset
    RAISING
      /mbtools/cx_exception .

  METHODS get_text_asset
    IMPORTING
      !iv_url            TYPE string
      !iv_assert_subtype TYPE string OPTIONAL
    RETURNING
      VALUE(rv_asset)    TYPE string
    RAISING
      /mbtools/cx_exception .

ENDINTERFACE.
