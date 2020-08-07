INTERFACE /mbtools/if_popups
  PUBLIC .

************************************************************************
* MBT Popups
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************

  METHODS popup_search_help
    IMPORTING
      !iv_tab_field   TYPE string
    RETURNING
      VALUE(rv_value) TYPE ddshretval-fieldval
    RAISING
      /mbtools/cx_exception .

  METHODS popup_to_confirm
    IMPORTING
      !iv_titlebar              TYPE clike
      !iv_text_question         TYPE clike
      !iv_text_button_1         TYPE clike DEFAULT 'Yes'
      !iv_icon_button_1         TYPE icon-name DEFAULT space
      !iv_text_button_2         TYPE clike DEFAULT 'No'
      !iv_icon_button_2         TYPE icon-name DEFAULT space
      !iv_default_button        TYPE char1 DEFAULT '1'
      !iv_display_cancel_button TYPE char1 DEFAULT abap_true
    RETURNING
      VALUE(rv_answer)          TYPE char1
    RAISING
      /mbtools/cx_exception .

  METHODS popup_to_inform
    IMPORTING
      !iv_titlebar     TYPE clike
      !iv_text_message TYPE clike
    RAISING
      /mbtools/cx_exception .

  METHODS popup_to_select_transports
    RETURNING
      VALUE(rt_trkorr) TYPE trwbo_request_headers .

  METHODS popup_to_select_from_list
    IMPORTING
      !it_list               TYPE STANDARD TABLE
      !iv_title              TYPE lvc_title DEFAULT space
      !iv_header_text        TYPE csequence DEFAULT space
      !iv_start_column       TYPE i DEFAULT 2
      !iv_end_column         TYPE i DEFAULT 65
      !iv_start_line         TYPE i DEFAULT 8
      !iv_end_line           TYPE i DEFAULT 20
      !iv_striped_pattern    TYPE abap_bool DEFAULT abap_false
      !iv_optimize_col_width TYPE abap_bool DEFAULT abap_true
      !iv_selection_mode     TYPE salv_de_constant DEFAULT if_salv_c_selection_mode=>multiple
      !iv_select_column_text TYPE csequence DEFAULT space
      !it_columns_to_display TYPE /mbtools/if_definitions=>ty_alv_column_tt
    EXPORTING
      VALUE(et_list)         TYPE STANDARD TABLE
    RAISING
      /mbtools/cx_exception .

  METHODS popup_transport_request
    IMPORTING
      !is_transport_type  TYPE /mbtools/if_definitions=>ty_transport_type
    RETURNING
      VALUE(rv_transport) TYPE trkorr
    RAISING
      /mbtools/cx_exception .
ENDINTERFACE.
