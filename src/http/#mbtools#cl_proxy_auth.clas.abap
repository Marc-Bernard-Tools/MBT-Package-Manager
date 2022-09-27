CLASS /mbtools/cl_proxy_auth DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* Marc Bernard Tools - Proxy Authentication
*
* Copyright 2014 abapGit Contributors <http://abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !ii_client TYPE REF TO if_http_client
      RAISING
        /mbtools/cx_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: gv_username TYPE string,
                gv_password TYPE string.

    CLASS-METHODS: enter RAISING /mbtools/cx_exception.

ENDCLASS.



CLASS /mbtools/cl_proxy_auth IMPLEMENTATION.


  METHOD enter.

    /mbtools/cl_password_dialog=>popup(
      EXPORTING
        iv_url  = 'Proxy Authentication'
      CHANGING
        cv_user = gv_username
        cv_pass = gv_password ) ##NO_TEXT.

    IF gv_username IS INITIAL OR gv_password IS INITIAL.
      /mbtools/cx_exception=>raise( 'Proxy authentication failed' ) ##NO_TEXT.
    ENDIF.

  ENDMETHOD.


  METHOD run.

    IF gv_username IS INITIAL OR gv_password IS INITIAL.
      enter( ).
    ENDIF.

    ii_client->authenticate(
      proxy_authentication = abap_true
      username             = gv_username
      password             = gv_password ).

  ENDMETHOD.
ENDCLASS.
