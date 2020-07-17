************************************************************************
* /MBTOOLS/IF_AJSON_READER
* MBT AJSON Reader Interface
*
* Original Author: Copyright (c) 2020 Alexander Tsybulsky
* https://github.com/sbcgua/ajson
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************
INTERFACE /mbtools/if_ajson_reader
  PUBLIC .


  METHODS exists
    IMPORTING
      !iv_path         TYPE string
    RETURNING
      VALUE(rv_exists) TYPE abap_bool .
  METHODS members
    IMPORTING
      !iv_path          TYPE string
    RETURNING
      VALUE(rt_members) TYPE string_table .
  METHODS value
    IMPORTING
      !iv_path        TYPE string
    RETURNING
      VALUE(rv_value) TYPE string .
  METHODS value_boolean
    IMPORTING
      !iv_path        TYPE string
    RETURNING
      VALUE(rv_value) TYPE abap_bool .
  METHODS value_integer
    IMPORTING
      !iv_path        TYPE string
    RETURNING
      VALUE(rv_value) TYPE i .
  METHODS value_number
    IMPORTING
      !iv_path        TYPE string
    RETURNING
      VALUE(rv_value) TYPE f .
  METHODS value_string
    IMPORTING
      !iv_path        TYPE string
    RETURNING
      VALUE(rv_value) TYPE string .
  METHODS slice
    IMPORTING
      !iv_path       TYPE string
    RETURNING
      VALUE(ri_json) TYPE REF TO /mbtools/if_ajson_reader .
  METHODS to_abap
    EXPORTING
      !ev_container TYPE any
    RAISING
      /mbtools/cx_ajson_error .
ENDINTERFACE.
