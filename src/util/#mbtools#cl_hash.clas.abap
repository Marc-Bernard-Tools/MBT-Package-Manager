CLASS /mbtools/cl_hash DEFINITION
  PUBLIC
  CREATE PUBLIC .

************************************************************************
* Marc Bernard Tools - Hash Functions
*
* Copyright 2014 abapGit Contributors <http://www.abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS sha1
      IMPORTING
        !iv_type       TYPE csequence
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_sha1) TYPE /mbtools/if_definitions=>ty_sha1
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS sha1_blob
      IMPORTING
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_sha1) TYPE /mbtools/if_definitions=>ty_sha1
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS sha1_raw
      IMPORTING
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_sha1) TYPE /mbtools/if_definitions=>ty_sha1
      RAISING
        /mbtools/cx_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_hash IMPLEMENTATION.


  METHOD sha1.

    DATA: lv_len     TYPE i,
          lv_char10  TYPE c LENGTH 10,
          lv_string  TYPE string,
          lv_xstring TYPE xstring.


    lv_len = xstrlen( iv_data ).
    lv_char10 = lv_len.
    CONDENSE lv_char10.
    CONCATENATE iv_type lv_char10 INTO lv_string SEPARATED BY space.
    lv_xstring = /mbtools/cl_convert=>string_to_xstring_utf8( lv_string ).

    lv_string = lv_xstring.
    CONCATENATE lv_string '00' INTO lv_string.
    lv_xstring = lv_string.

    CONCATENATE lv_xstring iv_data INTO lv_xstring IN BYTE MODE.

    rv_sha1 = sha1_raw( lv_xstring ).

  ENDMETHOD.


  METHOD sha1_blob.
    rv_sha1 = sha1( iv_type = 'blob'
                    iv_data = iv_data ).
  ENDMETHOD.


  METHOD sha1_raw.

    DATA: lv_hash  TYPE string,
          lv_key   TYPE xstring,
          lx_error TYPE REF TO cx_abap_message_digest.
    TRY.
        cl_abap_hmac=>calculate_hmac_for_raw(
      EXPORTING
        if_key        = lv_key
        if_data       = iv_data
      IMPORTING
        ef_hmacstring = lv_hash ).
      CATCH cx_abap_message_digest INTO lx_error.
        /mbtools/cx_exception=>raise_with_text( lx_error ).
    ENDTRY.

    rv_sha1 = lv_hash.
    TRANSLATE rv_sha1 TO LOWER CASE.

  ENDMETHOD.
ENDCLASS.
