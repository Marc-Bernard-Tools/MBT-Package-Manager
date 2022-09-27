CLASS /mbtools/cl_http_digest DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* Marc Bernard Tools - HTTP Digest Authentication
*
* Copyright 2014 abapGit Contributors <http://abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !ii_client   TYPE REF TO if_http_client
        !iv_username TYPE string
        !iv_password TYPE string
      RAISING
        /mbtools/cx_exception .
    METHODS run
      IMPORTING
        !ii_client TYPE REF TO if_http_client
      RAISING
        /mbtools/cx_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mv_ha1      TYPE string,
          mv_username TYPE string,
          mv_realm    TYPE string,
          mv_qop      TYPE string,
          mv_nonce    TYPE string.

    CLASS-DATA: gv_nc TYPE n LENGTH 8.

    CLASS-METHODS:
      md5
        IMPORTING
                  iv_data        TYPE string
        RETURNING
                  VALUE(rv_hash) TYPE string
        RAISING   /mbtools/cx_exception.

    METHODS:
      hash
        IMPORTING
                  iv_qop             TYPE string
                  iv_nonce           TYPE string
                  iv_uri             TYPE string
                  iv_method          TYPE string
                  iv_cnonse          TYPE string
        RETURNING
                  VALUE(rv_response) TYPE string
        RAISING   /mbtools/cx_exception,
      parse
        IMPORTING
          ii_client TYPE REF TO if_http_client.

ENDCLASS.



CLASS /mbtools/cl_http_digest IMPLEMENTATION.


  METHOD constructor.

    parse( ii_client ).

    mv_ha1 = md5( |{ iv_username }:{ mv_realm }:{ iv_password }| ).

    mv_username = iv_username.

  ENDMETHOD.


  METHOD hash.

    DATA: lv_ha2 TYPE string.

    lv_ha2 = md5( |{ iv_method }:{ iv_uri }| ).

    ASSERT iv_cnonse IS NOT INITIAL.

    rv_response = md5( |{ mv_ha1 }:{ iv_nonce }:{ gv_nc }:{ iv_cnonse }:{ iv_qop }:{ lv_ha2 }| ).

  ENDMETHOD.


  METHOD md5.

    DATA: lv_xstr TYPE xstring,
          lv_hash TYPE xstring.

    lv_xstr = /mbtools/cl_convert=>string_to_xstring_utf8( iv_data ).

    CALL FUNCTION 'CALCULATE_HASH_FOR_RAW'
      EXPORTING
        alg            = 'MD5'
        data           = lv_xstr
      IMPORTING
        hashxstring    = lv_hash
      EXCEPTIONS
        unknown_alg    = 1
        param_error    = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      /mbtools/cx_exception=>raise( 'Error from CALCULATE_HASH_FOR_RAW' ) ##NO_TEXT.
    ENDIF.

    rv_hash = lv_hash.
    TRANSLATE rv_hash TO LOWER CASE.

  ENDMETHOD.


  METHOD parse.

    DATA: lv_value TYPE string.

    lv_value = ii_client->response->get_header_field( 'www-authenticate' ) ##NO_TEXT.

    FIND REGEX 'realm="([\w ]+)"' IN lv_value SUBMATCHES mv_realm ##NO_TEXT ##SUBRC_OK.
    FIND REGEX 'qop="(\w+)"' IN lv_value SUBMATCHES mv_qop ##NO_TEXT ##SUBRC_OK.
    FIND REGEX 'nonce="([\w=/+\$]+)"' IN lv_value SUBMATCHES mv_nonce ##NO_TEXT ##SUBRC_OK.

  ENDMETHOD.


  METHOD run.

    DATA: lv_response TYPE string,
          lv_method   TYPE string,
          lv_cnonce   TYPE string,
          lv_uri      TYPE string,
          lv_auth     TYPE string.

    ASSERT mv_nonce IS NOT INITIAL.

    lv_method = ii_client->request->get_header_field( '~request_method' ).
    lv_uri = ii_client->request->get_header_field( '~request_uri' ).

    CALL FUNCTION 'GENERAL_GET_RANDOM_STRING'
      EXPORTING
        number_chars  = 24
      IMPORTING
        random_string = lv_cnonce.

    lv_response = hash(
      iv_qop    = mv_qop
      iv_nonce  = mv_nonce
      iv_uri    = lv_uri
      iv_method = lv_method
      iv_cnonse = lv_cnonce ).

* client response
    lv_auth = |Digest username="{ mv_username
      }", realm="{ mv_realm
      }", nonce="{ mv_nonce
      }", uri="{ lv_uri
      }", qop={ mv_qop
      }, nc={ gv_nc
      }, cnonce="{ lv_cnonce
      }", response="{ lv_response }"|.

    ii_client->request->set_header_field(
      name  = 'Authorization'
      value = lv_auth ) ##NO_TEXT.

  ENDMETHOD.
ENDCLASS.
