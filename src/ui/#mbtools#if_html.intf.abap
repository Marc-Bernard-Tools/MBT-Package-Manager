INTERFACE /mbtools/if_html
  PUBLIC .
************************************************************************
* MBT HTML
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************

  CONSTANTS:
    BEGIN OF c_action_type,
      sapevent  TYPE c VALUE 'E',
      url       TYPE c VALUE 'U',
      onclick   TYPE c VALUE 'C',
      separator TYPE c VALUE 'S',
      dummy     TYPE c VALUE '_',
    END OF c_action_type .
  CONSTANTS:
    BEGIN OF c_html_opt,
      strong   TYPE c VALUE 'E',
      cancel   TYPE c VALUE 'C',
      crossout TYPE c VALUE 'X',
    END OF c_html_opt .

  DATA mv_chunk_title TYPE string READ-ONLY . " Primarily for debug of posponed html parts

  METHODS a
    IMPORTING
      !iv_txt       TYPE string
      !iv_act       TYPE string
      !iv_typ       TYPE c DEFAULT c_action_type-sapevent
      !iv_opt       TYPE clike OPTIONAL
      !iv_class     TYPE string OPTIONAL
      !iv_id        TYPE string OPTIONAL
      !iv_style     TYPE string OPTIONAL
      !iv_title     TYPE string OPTIONAL
    RETURNING
      VALUE(rv_str) TYPE string .
  METHODS add
    IMPORTING
      !ig_chunk TYPE any .
  METHODS add_a
    IMPORTING
      !iv_txt   TYPE string
      !iv_act   TYPE string
      !iv_typ   TYPE c DEFAULT c_action_type-sapevent
      !iv_opt   TYPE clike OPTIONAL
      !iv_class TYPE string OPTIONAL
      !iv_id    TYPE string OPTIONAL
      !iv_style TYPE string OPTIONAL
      !iv_title TYPE string OPTIONAL .
  METHODS add_checkbox
    IMPORTING
      !iv_id      TYPE string
      !iv_checked TYPE abap_bool OPTIONAL .
  METHODS add_icon
    IMPORTING
      !iv_name    TYPE string
      !iv_hint    TYPE string OPTIONAL
      !iv_class   TYPE string OPTIONAL
      !iv_onclick TYPE string OPTIONAL .
  METHODS icon
    IMPORTING
      !iv_name      TYPE string
      !iv_hint      TYPE string OPTIONAL
      !iv_class     TYPE string OPTIONAL
      !iv_onclick   TYPE string OPTIONAL
    RETURNING
      VALUE(rv_str) TYPE string .
  METHODS is_empty
    RETURNING
      VALUE(rv_yes) TYPE abap_bool .
  METHODS render
    IMPORTING
      !iv_no_indent_jscss TYPE abap_bool OPTIONAL
    RETURNING
      VALUE(rv_html)      TYPE string .
  METHODS set_title
    IMPORTING
      !iv_title TYPE string .
ENDINTERFACE.
