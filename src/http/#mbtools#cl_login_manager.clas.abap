CLASS /mbtools/cl_login_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Marc Bernard Tools - Login Manager
*
* Copyright 2014 abapGit Contributors <http://abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS load
      IMPORTING
        !iv_uri                 TYPE string
      RETURNING
        VALUE(rv_authorization) TYPE string
      RAISING
        /mbtools/cx_exception.

    CLASS-METHODS save
      IMPORTING
        !iv_uri           TYPE string
        !iv_authorization TYPE string
      RAISING
        /mbtools/cx_exception.

    CLASS-METHODS clear.

    CLASS-METHODS set
      IMPORTING
        !iv_uri        TYPE string
        !iv_username   TYPE string
        !iv_password   TYPE string
      RETURNING
        VALUE(rv_auth) TYPE string
      RAISING
        /mbtools/cx_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_auth,
        uri           TYPE string,
        authorization TYPE string,
      END OF ty_auth.

    CLASS-DATA:
      gt_auth TYPE TABLE OF ty_auth WITH DEFAULT KEY.

    CLASS-METHODS append
      IMPORTING
        !iv_uri  TYPE string
        !iv_auth TYPE string
      RAISING
        /mbtools/cx_exception.

ENDCLASS.



CLASS /mbtools/cl_login_manager IMPLEMENTATION.


  METHOD append.

    FIELD-SYMBOLS: <ls_auth> LIKE LINE OF gt_auth.

    READ TABLE gt_auth WITH KEY uri = /mbtools/cl_url=>host( iv_uri )
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO gt_auth ASSIGNING <ls_auth>.
      <ls_auth>-uri           = /mbtools/cl_url=>host( iv_uri ).
      <ls_auth>-authorization = iv_auth.
    ENDIF.

  ENDMETHOD.


  METHOD clear.

    CLEAR gt_auth.

  ENDMETHOD.


  METHOD load.

    DATA ls_auth LIKE LINE OF gt_auth.

    READ TABLE gt_auth INTO ls_auth WITH KEY uri = /mbtools/cl_url=>host( iv_uri ).
    IF sy-subrc = 0.
      rv_authorization = ls_auth-authorization.
    ENDIF.

  ENDMETHOD.


  METHOD save.

    IF iv_authorization IS NOT INITIAL.
      append( iv_uri  = iv_uri
              iv_auth = iv_authorization ).
    ENDIF.

  ENDMETHOD.


  METHOD set.

    DATA: lv_concat TYPE string.

    ASSERT iv_uri IS NOT INITIAL.

    IF iv_username IS INITIAL OR iv_password IS INITIAL.
      RETURN.
    ENDIF.

    CONCATENATE iv_username ':' iv_password INTO lv_concat.

    rv_auth = cl_http_utility=>if_http_utility~encode_base64( lv_concat ).

    CONCATENATE 'Basic' rv_auth INTO rv_auth
      SEPARATED BY space ##NO_TEXT.

    append( iv_uri  = iv_uri
            iv_auth = rv_auth ).

  ENDMETHOD.
ENDCLASS.
