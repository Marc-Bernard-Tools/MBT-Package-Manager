CLASS /mbtools/cl_url DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
************************************************************************
* MBT Login Manager
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************

  PUBLIC SECTION.

    CLASS-METHODS validate
      IMPORTING
        !iv_url TYPE string
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS host
      IMPORTING
        !iv_url        TYPE string
      RETURNING
        VALUE(rv_host) TYPE string
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS name
      IMPORTING
        !iv_url        TYPE string
        !iv_validate   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_name) TYPE string
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS path_name
      IMPORTING
        !iv_url             TYPE string
      RETURNING
        VALUE(rv_path_name) TYPE string
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS is_mbt
      IMPORTING
        !iv_url       TYPE string
      RETURNING
        VALUE(rv_mbt) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS regex
      IMPORTING
        !iv_url   TYPE string
      EXPORTING
        !ev_host  TYPE string
        !ev_path  TYPE string
        !ev_name  TYPE string
        !ev_query TYPE string
      RAISING
        /mbtools/cx_exception .
ENDCLASS.



CLASS /MBTOOLS/CL_URL IMPLEMENTATION.


  METHOD host.

    regex( EXPORTING iv_url = iv_url
           IMPORTING ev_host = rv_host ).

  ENDMETHOD.


  METHOD is_mbt.

    IF iv_url CS 'marcbernardtools.com'.
      rv_mbt = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD name.

    DATA: lv_path TYPE string.

    TRY.
        regex( EXPORTING iv_url = iv_url
               IMPORTING ev_name = rv_name
                         ev_path = lv_path ).

        IF rv_name IS INITIAL.
          FIND REGEX '([\w-]+)/$' IN lv_path SUBMATCHES rv_name.
          IF sy-subrc <> 0.
            /mbtools/cx_exception=>raise( 'Malformed URL' ) ##NO_TEXT.
          ENDIF.
        ENDIF.

      CATCH /mbtools/cx_exception.
        IF iv_validate = abap_true.
          /mbtools/cx_exception=>raise( 'Malformed URL' ) ##NO_TEXT.
        ELSE.
          rv_name = 'URL error' ##NO_TEXT.
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD path_name.

    DATA: lv_host TYPE string ##NEEDED.

    FIND REGEX '(.*://[^/]*)(.*)' IN iv_url
      SUBMATCHES lv_host rv_path_name.

  ENDMETHOD.


  METHOD regex.

    FIND REGEX '(https?://[^/]*)(.*/)([^\.]*)?(.*)' IN iv_url
      SUBMATCHES ev_host ev_path ev_name ev_query.
    IF sy-subrc <> 0.
      /mbtools/cx_exception=>raise( 'Malformed URL' ) ##NO_TEXT.
    ENDIF.

  ENDMETHOD.


  METHOD validate.

    name( iv_url      = iv_url
          iv_validate = abap_true ).

  ENDMETHOD.
ENDCLASS.
