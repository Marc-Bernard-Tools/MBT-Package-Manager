INTERFACE /mbtools/if_html_viewer
  PUBLIC.

************************************************************************
* Marc Bernard Tools - HTML Viewer
*
* Copyright 2014 abapGit Contributors <http://abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  CONSTANTS c_id_sapevent TYPE i VALUE 1 ##NO_TEXT.

  EVENTS sapevent
    EXPORTING
      VALUE(action)      TYPE c OPTIONAL
      VALUE(frame)       TYPE c OPTIONAL
      VALUE(getdata)     TYPE c OPTIONAL
      VALUE(postdata)    TYPE cnht_post_data_tab OPTIONAL
      VALUE(query_table) TYPE cnht_query_table OPTIONAL.

  METHODS load_data
    IMPORTING
      !iv_url          TYPE csequence OPTIONAL
      !iv_type         TYPE csequence DEFAULT 'text'
      !iv_subtype      TYPE csequence DEFAULT 'html'
      !iv_size         TYPE i DEFAULT 0
    EXPORTING
      !ev_assigned_url TYPE string
    CHANGING
      !ct_data_table   TYPE STANDARD TABLE
    RAISING
      /mbtools/cx_exception.

  METHODS set_registered_events
    IMPORTING
      !it_events TYPE cntl_simple_events
    RAISING
      /mbtools/cx_exception.

  METHODS show_url
    IMPORTING
      !iv_url TYPE csequence
    RAISING
      /mbtools/cx_exception.

  METHODS free.

  METHODS close_document.

  METHODS get_url
    RETURNING
      VALUE(rv_url) TYPE string.

  METHODS back.

ENDINTERFACE.
