CLASS /mbtools/cl_aphp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Marc Bernard Tools - PHP
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS serialize
      RAISING
        /mbtools/cx_ajson_error.
    CLASS-METHODS unserialize
      IMPORTING
        !iv_data        TYPE string
        !iv_precision   TYPE i DEFAULT 2
        !iv_ignore_len  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ro_ajson) TYPE REF TO /mbtools/cl_ajson
      RAISING
        /mbtools/cx_ajson_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA gv_ignore_len TYPE abap_bool.

    CLASS-METHODS unserialize_array
      IMPORTING
        !iv_path  TYPE string
        !iv_data  TYPE string
        !io_ajson TYPE REF TO /mbtools/cl_ajson
      RAISING
        /mbtools/cx_ajson_error.
    CLASS-METHODS unserialize_object
      IMPORTING
        !iv_path  TYPE string
        !iv_data  TYPE string
        !io_ajson TYPE REF TO /mbtools/cl_ajson
      RAISING
        /mbtools/cx_ajson_error ##NEEDED.
    CLASS-METHODS unserialize_reference
      IMPORTING
        !iv_path  TYPE string
        !iv_data  TYPE string
        !io_ajson TYPE REF TO /mbtools/cl_ajson
      RAISING
        /mbtools/cx_ajson_error ##NEEDED.
    CLASS-METHODS unserialize_int
      IMPORTING
        !iv_type  TYPE c
        !iv_path  TYPE string
        !iv_val   TYPE string
        !io_ajson TYPE REF TO /mbtools/cl_ajson
      RAISING
        /mbtools/cx_ajson_error.
    CLASS-METHODS unserialize_string
      IMPORTING
        !iv_type  TYPE c
        !iv_path  TYPE string
        !iv_key   TYPE string
        !iv_val   TYPE string
        !io_ajson TYPE REF TO /mbtools/cl_ajson
      RAISING
        /mbtools/cx_ajson_error.
    CLASS-METHODS strip
      IMPORTING
        !iv_val       TYPE string
        !iv_char      TYPE c
      RETURNING
        VALUE(rv_val) TYPE string .
    CLASS-METHODS get_string
      IMPORTING
        !iv_data      TYPE string
      RETURNING
        VALUE(rv_val) TYPE string
      RAISING
        /mbtools/cx_ajson_error.
    CLASS-METHODS get_integer
      IMPORTING
        !iv_data      TYPE string
      RETURNING
        VALUE(rv_val) TYPE i .
    CLASS-METHODS get_float
      IMPORTING
        !iv_data      TYPE string
        !iv_precision TYPE i DEFAULT 2
      RETURNING
        VALUE(rv_val) TYPE f .
    CLASS-METHODS get_boolean
      IMPORTING
        !iv_data      TYPE string
      RETURNING
        VALUE(rv_val) TYPE abap_bool
      RAISING
        /mbtools/cx_ajson_error.
ENDCLASS.



CLASS /mbtools/cl_aphp IMPLEMENTATION.


  METHOD get_boolean.

    DATA lv_val TYPE string.

    " b:0;  or  b:1;
    lv_val = shift_right( val = iv_data
                          sub = ';' ).
    IF sy-subrc <> 0.
      /mbtools/cx_ajson_error=>raise( |Data error type "b"| ).
    ENDIF.

    rv_val = boolc( lv_val <> '0' ).

  ENDMETHOD.


  METHOD get_float.

    DATA lv_val TYPE string.

    " d:<value>;
    lv_val = shift_right( val = iv_data
                          sub = ';' ).
    rv_val = round( val = lv_val
                    dec = iv_precision ).

  ENDMETHOD.


  METHOD get_integer.

    DATA lv_val TYPE string.

    " i:<value>;
    lv_val = shift_right( val = iv_data
                          sub = ';' ).
    rv_val = lv_val.

  ENDMETHOD.


  METHOD get_string.

    DATA:
      lv_len_str TYPE string,
      lv_len_int TYPE i.

    " s:<len>:"<value>";
    SPLIT iv_data AT ':' INTO lv_len_str rv_val.
    IF sy-subrc <> 0.
      /mbtools/cx_ajson_error=>raise( |Data error type "s"| ).
    ENDIF.
    lv_len_int = lv_len_str.
    rv_val = shift_right( val = rv_val
                          sub = ';' ).
    rv_val = strip( iv_val  = rv_val
                    iv_char = '"' ).
    REPLACE ALL OCCURRENCES OF '\"' IN rv_val WITH '"'.

    IF strlen( rv_val ) <> lv_len_int AND gv_ignore_len IS INITIAL.
      /mbtools/cx_ajson_error=>raise( |Data error type "s"; incorrect length| ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize.

    " Takes JSON string and returns PHP-serialized value
    " https://www.php.net/manual/en/function.serialize.php

    " TODO
    /mbtools/cx_ajson_error=>raise( |Not implemented yet| ).

  ENDMETHOD.


  METHOD strip.

    rv_val = shift_left( val = iv_val
                         sub = iv_char ).
    IF iv_char = '{'.
      rv_val = shift_right( val = rv_val
                            sub = '}' ).
    ELSEIF iv_char = '['.
      rv_val = shift_right( val = rv_val
                            sub = ']' ).
    ELSE.
      rv_val = shift_right( val = rv_val
                            sub = iv_char ).
    ENDIF.

  ENDMETHOD.


  METHOD unserialize.

    " Takes PHP-serialized value and returns corresponding JSON string
    " https://www.php.net/manual/en/function.serialize.php

    DATA:
      lv_data TYPE string,
      lv_path TYPE string,
      lv_type TYPE c.

    gv_ignore_len = iv_ignore_len.

    ro_ajson = /mbtools/cl_ajson=>create_empty( ).

    IF iv_data IS INITIAL.
      RETURN.
    ENDIF.

    lv_data = iv_data.

    IF lv_data CS ':'.
      SPLIT lv_data AT ':' INTO lv_type lv_data.
    ELSE.
      lv_type = lv_data(1).
    ENDIF.

    lv_path = '/' && lv_type.

    CASE lv_type.
      WHEN 's'. "string
        ro_ajson->set_string( iv_path = lv_path
                              iv_val  = get_string( lv_data ) ).
      WHEN 'i'. "integer
        ro_ajson->set_integer( iv_path = lv_path
                               iv_val  = get_integer( lv_data ) ).
      WHEN 'd'. "float
        ro_ajson->set( iv_path = lv_path
                       iv_val  = get_float( iv_data      = lv_data
                                            iv_precision = iv_precision ) ).
      WHEN 'b'. "boolean
        ro_ajson->set_boolean( iv_path = lv_path
                               iv_val  = get_boolean( lv_data ) ).
      WHEN 'N'. "null
        ro_ajson->set_null( lv_path ).
      WHEN 'a'. "array
        unserialize_array( iv_path  = lv_path
                           iv_data  = lv_data
                           io_ajson = ro_ajson ).
      WHEN 'O'. "object
        unserialize_object( iv_path  = lv_path
                            iv_data  = lv_data
                            io_ajson = ro_ajson ).
      WHEN 'R'. "reference
        unserialize_reference( iv_path  = lv_path
                               iv_data  = lv_data
                               io_ajson = ro_ajson ).
      WHEN OTHERS. "unknown
        /mbtools/cx_ajson_error=>raise( |Unknown type "{ lv_type }"| ).
    ENDCASE.

  ENDMETHOD.


  METHOD unserialize_array.

    DATA:
      lv_data     TYPE string,
      lv_key_type TYPE c LENGTH 1,
      lv_val_type TYPE c LENGTH 1,
      lv_len      TYPE i,
      lv_key      TYPE string,
      lv_val      TYPE string.

    " a:<len>:{<key>;<value>;...}
    SPLIT iv_data AT ':' INTO lv_key lv_data.
    IF sy-subrc <> 0.
      /mbtools/cx_ajson_error=>raise( |Data error type "a"| ).
    ENDIF.

    lv_len = lv_key.
    lv_data = strip( iv_val  = lv_data
                     iv_char = '{' ).

    io_ajson->touch_array( iv_path ).

    DO lv_len TIMES.
      " i:<key>;  or  s:len:"<key>";
      SPLIT lv_data AT ';' INTO lv_key lv_data.
      IF sy-subrc <> 0.
        /mbtools/cx_ajson_error=>raise( |Data error type "a"| ).
      ENDIF.
      SPLIT lv_data AT ';' INTO lv_val lv_data. " TODO: assumes ; does not occur in value
      IF sy-subrc <> 0.
        /mbtools/cx_ajson_error=>raise( |Data error type "a"| ).
      ENDIF.
      SPLIT lv_key AT ':' INTO lv_key_type lv_key.
      IF sy-subrc <> 0.
        /mbtools/cx_ajson_error=>raise( |Data error type "a"| ).
      ENDIF.
      SPLIT lv_val AT ':' INTO lv_val_type lv_val.
      IF sy-subrc <> 0.
        /mbtools/cx_ajson_error=>raise( |Data error type "a"| ).
      ENDIF.

      lv_key = lv_key && ';'.
      lv_val = lv_val && ';'.

      CASE lv_key_type.
        WHEN 'i'. "integer
          unserialize_int(
            iv_type  = lv_val_type
            iv_path  = iv_path
            iv_val   = lv_val
            io_ajson = io_ajson ).
        WHEN 's'. "string
          unserialize_string(
            iv_type  = lv_val_type
            iv_path  = iv_path
            iv_key   = lv_key
            iv_val   = lv_val
            io_ajson = io_ajson ).
        WHEN OTHERS.
          /mbtools/cx_ajson_error=>raise( |Data error type "a"; expected "i" or "s"| ).
      ENDCASE.
    ENDDO.


  ENDMETHOD.


  METHOD unserialize_int.

    CASE iv_type.
      WHEN 's'. "string
        io_ajson->push( iv_path = iv_path
                        iv_val  = get_string( iv_val ) ).
      WHEN 'i'. "integer
        io_ajson->push( iv_path = iv_path
                        iv_val  = get_integer( iv_val ) ).
      WHEN 'd'. "float
        io_ajson->push( iv_path = iv_path
                        iv_val  = get_float( iv_val ) ).
      WHEN 'b'. "boolean
        io_ajson->push( iv_path = iv_path
                        iv_val  = get_boolean( iv_val ) ).
      WHEN 'a'. "array
        unserialize_array( iv_path  = iv_path && '/a'
                           iv_data  = iv_val
                           io_ajson = io_ajson ).
      WHEN OTHERS.
        /mbtools/cx_ajson_error=>raise( |Not implemented type "{ iv_type }"| ).
    ENDCASE.

  ENDMETHOD.


  METHOD unserialize_object.

    " O:strlen(object name):object name:object size:
    " {s:strlen(property name):property name:property definition;...(repeated per property)}

    " TODO
    /mbtools/cx_ajson_error=>raise( |Not implemented type "O"| ).

  ENDMETHOD.


  METHOD unserialize_reference.

    " R:...

    " TODO
    /mbtools/cx_ajson_error=>raise( |Not implemented type "R"| ).

  ENDMETHOD.


  METHOD unserialize_string.

    TYPES:
      BEGIN OF ty_array_string,
        key TYPE string,
        val TYPE string,
      END OF ty_array_string,
      BEGIN OF ty_array_int,
        key TYPE string,
        val TYPE i,
      END OF ty_array_int,
      BEGIN OF ty_array_float,
        key TYPE string,
        val TYPE f,
      END OF ty_array_float,
      BEGIN OF ty_array_bool,
        key TYPE string,
        val TYPE abap_bool,
      END OF ty_array_bool.

    DATA:
      ls_array_string TYPE ty_array_string,
      ls_array_int    TYPE ty_array_int,
      ls_array_float  TYPE ty_array_float,
      ls_array_bool   TYPE ty_array_bool.

    CASE iv_type.
      WHEN 's'. "string
        ls_array_string-key = get_string( iv_key ).
        ls_array_string-val = get_string( iv_val ).
        io_ajson->push( iv_path = iv_path
                        iv_val  = ls_array_string ).
      WHEN 'i'. "integer
        ls_array_int-key = get_string( iv_key ).
        ls_array_int-val = get_integer( iv_val ).
        io_ajson->push( iv_path = iv_path
                        iv_val  = ls_array_int ).
      WHEN 'd'. "float
        ls_array_float-key = get_string( iv_key ).
        ls_array_float-val = get_float( iv_val ).
        io_ajson->push( iv_path = iv_path
                        iv_val  = ls_array_float ).
      WHEN 'b'. "boolean
        ls_array_bool-key = get_string( iv_key ).
        ls_array_bool-val = get_boolean( iv_val ).
        io_ajson->push( iv_path = iv_path
                        iv_val  = ls_array_bool ).
      WHEN 'a'. "array
        unserialize_array( iv_path  = iv_path && '/a'
                           iv_data  = iv_val
                           io_ajson = io_ajson ).
      WHEN OTHERS.
        /mbtools/cx_ajson_error=>raise( |Not implemented type "{ iv_type }"| ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
