INTERFACE /mbtools/if_ajson_writer
  PUBLIC .

************************************************************************
* abap json (AJSON)
*
* Original Author: Copyright (c) 2020 Alexander Tsybulsky
* https://github.com/sbcgua/ajson
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************

  METHODS clear
    RAISING
      /mbtools/cx_ajson_error.

  METHODS set
    IMPORTING
      iv_path TYPE string
      iv_val TYPE any
      iv_ignore_empty TYPE abap_bool DEFAULT abap_true
      iv_node_type TYPE string OPTIONAL
    RAISING
      /mbtools/cx_ajson_error.

  METHODS set_boolean
    IMPORTING
      iv_path TYPE string
      iv_val TYPE any
    RAISING
      /mbtools/cx_ajson_error.

  METHODS set_string
    IMPORTING
      iv_path TYPE string
      iv_val TYPE clike
    RAISING
      /mbtools/cx_ajson_error.

  METHODS set_integer
    IMPORTING
      iv_path TYPE string
      iv_val TYPE i
    RAISING
      /mbtools/cx_ajson_error.

  METHODS set_date
    IMPORTING
      iv_path TYPE string
      iv_val TYPE d
    RAISING
      /mbtools/cx_ajson_error.

  METHODS set_timestamp
    IMPORTING
      iv_path TYPE string
      iv_val TYPE timestamp
    RAISING
      /mbtools/cx_ajson_error.

  METHODS set_null
    IMPORTING
      iv_path TYPE string
    RAISING
      /mbtools/cx_ajson_error.

  METHODS delete
    IMPORTING
      iv_path TYPE string
    RAISING
      /mbtools/cx_ajson_error.

  METHODS touch_array
    IMPORTING
      iv_path TYPE string
      iv_clear TYPE abap_bool DEFAULT abap_false
    RAISING
      /mbtools/cx_ajson_error.

  METHODS push
    IMPORTING
      iv_path TYPE string
      iv_val TYPE any
    RAISING
      /mbtools/cx_ajson_error.

  METHODS stringify
    IMPORTING
      iv_indent TYPE i DEFAULT 0
    RETURNING
      VALUE(rv_json) TYPE string
    RAISING
      /mbtools/cx_ajson_error.

ENDINTERFACE.
