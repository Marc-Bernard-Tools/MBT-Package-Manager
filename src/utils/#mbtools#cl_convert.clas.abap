CLASS /mbtools/cl_convert DEFINITION
  PUBLIC
  CREATE PUBLIC .

************************************************************************
* Marc Bernard Tools - Conversions
*
* Copyright 2014 abapGit Contributors <http://abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS base64_to_xstring
      IMPORTING
        iv_base64      TYPE string
      RETURNING
        VALUE(rv_xstr) TYPE xstring.

    CLASS-METHODS bintab_to_xstring
      IMPORTING
        it_bintab      TYPE lvc_t_mime
        iv_size        TYPE i
      RETURNING
        VALUE(rv_xstr) TYPE xstring.

    CLASS-METHODS string_to_tab
      IMPORTING
        !iv_str TYPE string
      EXPORTING
        ev_size TYPE i
        et_tab  TYPE STANDARD TABLE.

    CLASS-METHODS string_to_xstring
      IMPORTING
        iv_str         TYPE string
      RETURNING
        VALUE(rv_xstr) TYPE xstring.

    CLASS-METHODS string_to_xstring_utf8
      IMPORTING
        !iv_string        TYPE string
      RETURNING
        VALUE(rv_xstring) TYPE xstring
      RAISING
        /mbtools/cx_exception.

    CLASS-METHODS xstring_to_bintab
      IMPORTING
        iv_xstr   TYPE xstring
      EXPORTING
        ev_size   TYPE i
        et_bintab TYPE lvc_t_mime.

    CLASS-METHODS xstring_to_string_utf8
      IMPORTING
        !iv_data         TYPE xsequence
        !iv_length       TYPE i OPTIONAL
      RETURNING
        VALUE(rv_string) TYPE string
      RAISING
        /mbtools/cx_exception.

    CLASS-METHODS split_string
      IMPORTING
        !iv_string      TYPE string
      RETURNING
        VALUE(rt_lines) TYPE string_table .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_convert_out TYPE REF TO cl_abap_conv_out_ce.
    CLASS-DATA go_convert_in TYPE REF TO cl_abap_conv_in_ce.

ENDCLASS.



CLASS /mbtools/cl_convert IMPLEMENTATION.


  METHOD base64_to_xstring.

    CALL FUNCTION 'SSFC_BASE64_DECODE'
      EXPORTING
        b64data = iv_base64
      IMPORTING
        bindata = rv_xstr
      EXCEPTIONS
        OTHERS  = 1.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD bintab_to_xstring.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = iv_size
      IMPORTING
        buffer       = rv_xstr
      TABLES
        binary_tab   = it_bintab
      EXCEPTIONS
        failed       = 1 ##FM_SUBRC_OK.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD split_string.

    FIND FIRST OCCURRENCE OF cl_abap_char_utilities=>cr_lf IN iv_string.

    " Convert string into table depending on separator type CR_LF vs. LF
    IF sy-subrc = 0.
      SPLIT iv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE rt_lines.
    ELSE.
      SPLIT iv_string AT cl_abap_char_utilities=>newline INTO TABLE rt_lines.
    ENDIF.

  ENDMETHOD.


  METHOD string_to_tab.

    CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
      EXPORTING
        text      = iv_str
      IMPORTING
        length    = ev_size
      TABLES
        ftext_tab = et_tab
      EXCEPTIONS
        OTHERS    = 1.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD string_to_xstring.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = iv_str
      IMPORTING
        buffer = rv_xstr
      EXCEPTIONS
        OTHERS = 1.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD string_to_xstring_utf8.

    DATA lx_error TYPE REF TO cx_root.

    TRY.
        IF go_convert_out IS INITIAL.
          go_convert_out = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
        ENDIF.

        go_convert_out->convert(
          EXPORTING
            data   = iv_string
          IMPORTING
            buffer = rv_xstring ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type INTO lx_error.
        /mbtools/cx_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD xstring_to_bintab.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = iv_xstr
      IMPORTING
        output_length = ev_size
      TABLES
        binary_tab    = et_bintab.

  ENDMETHOD.


  METHOD xstring_to_string_utf8.

    DATA:
      lx_error  TYPE REF TO cx_root,
      lv_length TYPE i.

    lv_length = iv_length.
    IF lv_length <= 0.
      lv_length = xstrlen( iv_data ).
    ENDIF.

    TRY.
        IF go_convert_in IS INITIAL.
          go_convert_in = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
        ENDIF.

        go_convert_in->convert(
          EXPORTING
            input = iv_data
            n     = lv_length
          IMPORTING
            data  = rv_string ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type INTO lx_error.
        /mbtools/cx_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
