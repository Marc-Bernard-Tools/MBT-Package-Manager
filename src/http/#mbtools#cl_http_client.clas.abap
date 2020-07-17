************************************************************************
* /MBTOOLS/CL_HTTP_CLIENT
* MBT HTTP Client
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************
CLASS /mbtools/cl_http_client DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !ii_client TYPE REF TO if_http_client .
    METHODS close .
    METHODS set_digest
      IMPORTING
        !io_digest TYPE REF TO /mbtools/cl_http_digest .
    METHODS send_receive_close
      IMPORTING
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_data) TYPE xstring
      RAISING
        /mbtools/cx_exception .
    METHODS get_cdata
      RETURNING
        VALUE(rv_value) TYPE string .
    METHODS check_http_200
      RAISING
        /mbtools/cx_exception .
    METHODS check_smart_response
      IMPORTING
        !iv_expected_content_type TYPE string
        !iv_content_regex         TYPE string
      RAISING
        /mbtools/cx_exception .
    METHODS send_receive
      RAISING
        /mbtools/cx_exception .
    METHODS set_headers
      IMPORTING
        !iv_url TYPE string
      RAISING
        /mbtools/cx_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mi_client TYPE REF TO if_http_client,
          mo_digest TYPE REF TO /mbtools/cl_http_digest.

ENDCLASS.



CLASS /MBTOOLS/CL_HTTP_CLIENT IMPLEMENTATION.


  METHOD check_http_200.

    DATA: lv_code TYPE i,
          lv_text TYPE string.

    mi_client->response->get_status( IMPORTING code = lv_code ).

    CASE lv_code.
      WHEN 200.
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


  METHOD send_receive.

    DATA: lv_text    TYPE string,
          lv_code    TYPE i,
          lv_message TYPE string.

    mi_client->send( ).
    mi_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).

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
