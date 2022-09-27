CLASS /mbtools/cl_http_client DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* Marc Bernard Tools - HTTP Client
*
* Copyright 2014 abapGit Contributors <http://abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_multipart,
        ctype TYPE string,
        cdata TYPE string,
      END OF ty_multipart.
    TYPES:
      ty_multiparts TYPE STANDARD TABLE OF ty_multipart WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING
        !ii_client TYPE REF TO if_http_client.
    METHODS close.
    METHODS set_digest
      IMPORTING
        !io_digest TYPE REF TO /mbtools/cl_http_digest.
    METHODS send_receive_close
      IMPORTING
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_data) TYPE xstring
      RAISING
        /mbtools/cx_exception.
    METHODS get_data
      RETURNING
        VALUE(rv_value) TYPE xstring.
    METHODS get_cdata
      RETURNING
        VALUE(rv_value) TYPE string.
    METHODS check_http_200
      RAISING
        /mbtools/cx_exception.
    METHODS check_smart_response
      IMPORTING
        !iv_expected_content_type TYPE string OPTIONAL
        !iv_content_regex         TYPE string OPTIONAL
      RAISING
        /mbtools/cx_exception.
    METHODS send_receive
      RAISING
        /mbtools/cx_exception.
    METHODS set_headers
      IMPORTING
        !iv_url TYPE string
      RAISING
        /mbtools/cx_exception.
    METHODS get_multipart
      RETURNING
        VALUE(rt_multipart) TYPE ty_multiparts.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mi_client TYPE REF TO if_http_client,
          mo_digest TYPE REF TO /mbtools/cl_http_digest.

ENDCLASS.



CLASS /mbtools/cl_http_client IMPLEMENTATION.


  METHOD check_http_200.

    DATA: lv_code TYPE i,
          lv_text TYPE string.

    mi_client->response->get_status( IMPORTING code = lv_code ).

    CASE lv_code.
      WHEN 200 OR 201.
        RETURN. " Success, OK
      WHEN 302.
        /mbtools/cx_exception=>raise( 'Resource access temporarily redirected. Check the URL (HTTP 302)' ) ##NO_TEXT.
      WHEN 401.
        /mbtools/cx_exception=>raise( 'Unauthorized access to resource. Check your credentials (HTTP 401)' ) ##NO_TEXT.
      WHEN 403.
        /mbtools/cx_exception=>raise( 'Access to resource forbidden (HTTP 403)' ) ##NO_TEXT.
      WHEN 404.
        /mbtools/cx_exception=>raise( 'Resource not found. Check the URL (HTTP 404)' ) ##NO_TEXT.
      WHEN 407.
        /mbtools/cx_exception=>raise( 'Proxy authentication required. Check your credentials (HTTP 407)' ) ##NO_TEXT.
      WHEN 408.
        /mbtools/cx_exception=>raise( 'Request timeout (HTTP 408)' ) ##NO_TEXT.
      WHEN 415.
        /mbtools/cx_exception=>raise( 'Unsupported media type (HTTP 415)' ) ##NO_TEXT.
      WHEN 426.
        /mbtools/cx_exception=>raise( 'Upgrade required. Check HTTP protocol version (HTTP 426)' ) ##NO_TEXT.
      WHEN OTHERS.
        lv_text = mi_client->response->get_cdata( ).
        /mbtools/cx_exception=>raise( |HTTP error code: { lv_code }, { lv_text }| ).
    ENDCASE.

  ENDMETHOD.


  METHOD check_smart_response.

    DATA: lv_content_type TYPE string.
    DATA: lv_data         TYPE string.

    IF iv_expected_content_type IS NOT INITIAL.
      lv_content_type = mi_client->response->get_content_type( ).
      IF lv_content_type CS ';'.
        SPLIT lv_content_type AT ';' INTO lv_content_type lv_data.
      ENDIF.
      IF lv_content_type <> iv_expected_content_type.
        /mbtools/cx_exception=>raise( 'Wrong content-type sent by server' ) ##NO_TEXT.
      ENDIF.
    ENDIF.

    IF iv_content_regex IS NOT INITIAL.
      lv_data = mi_client->response->get_cdata( ).
      FIND REGEX iv_content_regex IN lv_data.
      IF sy-subrc <> 0.
        /mbtools/cx_exception=>raise( 'Wrong content sent by server' ) ##NO_TEXT.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD close.
    mi_client->close( ).
  ENDMETHOD.


  METHOD constructor.
    mi_client = ii_client.
  ENDMETHOD.


  METHOD get_cdata.
    rv_value = mi_client->response->get_cdata( ).
  ENDMETHOD.


  METHOD get_data.
    rv_value = mi_client->response->get_data( ).
  ENDMETHOD.


  METHOD get_multipart.

    DATA:
      li_part TYPE REF TO if_http_entity.

    FIELD-SYMBOLS:
      <ls_multipart> LIKE LINE OF rt_multipart.

    DO mi_client->response->num_multiparts( ) TIMES.
      li_part = mi_client->response->get_multipart( sy-index ).

      APPEND INITIAL LINE TO rt_multipart ASSIGNING <ls_multipart>.
      <ls_multipart>-ctype = li_part->get_content_type( ).
      <ls_multipart>-cdata = li_part->get_cdata( ).
    ENDDO.

  ENDMETHOD.


  METHOD send_receive.

    DATA: lv_text    TYPE string,
          lv_code    TYPE i,
          lv_message TYPE string.

    mi_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).

    IF sy-subrc = 0.
      mi_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
    ENDIF.

    IF sy-subrc <> 0.
      " in case of HTTP_COMMUNICATION_FAILURE
      " make sure:
      " a) SSL is setup properly in STRUST
      " b) no firewalls
      " check trace file in transaction SMICM

      mi_client->get_last_error(
        IMPORTING
          code    = lv_code
          message = lv_message ).

      lv_text = |HTTP error { lv_code } occured: { lv_message }|.

      /mbtools/cx_exception=>raise( lv_text ).
    ENDIF.

  ENDMETHOD.


  METHOD send_receive_close.

* do not use set_cdata as it modifies the Content-Type header field
    mi_client->request->set_data( iv_data ).
    send_receive( ).
    check_http_200( ).
    rv_data = mi_client->response->get_data( ).
    mi_client->close( ).

  ENDMETHOD.


  METHOD set_digest.
    mo_digest = io_digest.
  ENDMETHOD.


  METHOD set_headers.

    mi_client->request->set_header_field(
      name  = '~request_method'
      value = 'POST' ).

    mi_client->request->set_header_field(
      name  = '~request_uri'
      value = iv_url ).

    IF mo_digest IS BOUND.
      mo_digest->run( mi_client ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
