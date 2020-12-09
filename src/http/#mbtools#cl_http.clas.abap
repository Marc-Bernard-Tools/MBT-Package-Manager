CLASS /mbtools/cl_http DEFINITION
  PUBLIC
  CREATE PUBLIC .

************************************************************************
* MBT HTTP
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************
  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_scheme,
        digest TYPE string VALUE 'Digest' ##NO_TEXT,
      END OF c_scheme .

    CLASS-METHODS class_constructor .
    CLASS-METHODS get_agent
      RETURNING
        VALUE(rv_agent) TYPE string .
    CLASS-METHODS create_by_url
      IMPORTING
        !iv_url          TYPE string
        !iv_request      TYPE string DEFAULT 'GET'
        !iv_content      TYPE string OPTIONAL
      RETURNING
        VALUE(ro_client) TYPE REF TO /mbtools/cl_http_client
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS ping
      IMPORTING
        !iv_url          TYPE string
        !iv_regex        TYPE string OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
  PROTECTED SECTION.

    CLASS-METHODS check_auth_requested
      IMPORTING
        !ii_client               TYPE REF TO if_http_client
      RETURNING
        VALUE(rv_auth_requested) TYPE abap_bool
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS is_local_system
      IMPORTING
        !iv_url        TYPE string
      RETURNING
        VALUE(rv_bool) TYPE abap_bool .
    CLASS-METHODS acquire_login_details
      IMPORTING
        !ii_client       TYPE REF TO if_http_client
        !io_client       TYPE REF TO /mbtools/cl_http_client
        !iv_url          TYPE string
      RETURNING
        VALUE(rv_scheme) TYPE string
      RAISING
        /mbtools/cx_exception .
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_login,
        user TYPE string VALUE 'LoginUser',
      END OF c_login .
    CLASS-DATA mo_settings TYPE REF TO /mbtools/cl_registry .
ENDCLASS.



CLASS /mbtools/cl_http IMPLEMENTATION.


  METHOD acquire_login_details.

    DATA: lv_default_user TYPE string,
          lv_user         TYPE string,
          lv_pass         TYPE string,
          lo_digest       TYPE REF TO /mbtools/cl_http_digest.


    lv_default_user = mo_settings->get_value( c_login-user ).
    lv_user         = lv_default_user.

    /mbtools/cl_password_dialog=>popup(
      EXPORTING
        iv_url  = iv_url
      CHANGING
        cv_user = lv_user
        cv_pass = lv_pass ).

    IF lv_user IS INITIAL.
      /mbtools/cx_exception=>raise( 'Request unauthorized (HTTP 401)' ) ##NO_TEXT.
    ENDIF.

    IF lv_user <> lv_default_user.
      TRY.
          mo_settings->set_value( iv_key = c_login-user
                                  iv_value = lv_user ).
          mo_settings->save( ).
        CATCH cx_root ##NO_HANDLER.
          " Ignore
      ENDTRY.
    ENDIF.

    rv_scheme = ii_client->response->get_header_field( 'www-authenticate' ).
    FIND REGEX '^(\w+)' IN rv_scheme SUBMATCHES rv_scheme.

    CASE rv_scheme.
      WHEN c_scheme-digest.
* https://en.wikipedia.org/wiki/Digest_access_authentication
* e.g. used by https://www.gerritcodereview.com/
        CREATE OBJECT lo_digest
          EXPORTING
            ii_client   = ii_client
            iv_username = lv_user
            iv_password = lv_pass.
        lo_digest->run( ii_client ).
        io_client->set_digest( lo_digest ).
      WHEN OTHERS.
* https://en.wikipedia.org/wiki/Basic_access_authentication
        ii_client->authenticate(
          username = lv_user
          password = lv_pass ).
    ENDCASE.

  ENDMETHOD.


  METHOD check_auth_requested.

    DATA: lv_code TYPE i.

    ii_client->response->get_status( IMPORTING code = lv_code ).
    IF lv_code = 401.
      rv_auth_requested = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD class_constructor.

    mo_settings = /mbtools/cl_tools=>factory( )->get_settings( ).

  ENDMETHOD.


  METHOD create_by_url.

    DATA: lv_scheme              TYPE string,
          li_client              TYPE REF TO if_http_client,
          lo_proxy_configuration TYPE REF TO /mbtools/cl_proxy_config,
          lv_text                TYPE string.

    CREATE OBJECT lo_proxy_configuration.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = iv_url
        ssl_id             = 'ANONYM'
        proxy_host         = lo_proxy_configuration->get_proxy_host( )
        proxy_service      = lo_proxy_configuration->get_proxy_port( )
      IMPORTING
        client             = li_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          lv_text = 'Error creating HTTPS connection. Check SSL setup in transaction STRUST' ##NO_TEXT.
        WHEN 2.
          lv_text = 'Error creating HTTPS connection. Check service setup in transaction SMICM' ##NO_TEXT.
        WHEN OTHERS.
          lv_text = 'Error creating HTTP/HTTPS client' ##NO_TEXT.
      ENDCASE.
      /mbtools/cx_exception=>raise( lv_text ).
    ENDIF.

    IF lo_proxy_configuration->get_proxy_authentication( ) = abap_true.
      /mbtools/cl_proxy_auth=>run( li_client ).
    ENDIF.

    CREATE OBJECT ro_client
      EXPORTING
        ii_client = li_client.

    IF is_local_system( iv_url ) = abap_true.
      li_client->send_sap_logon_ticket( ).
    ENDIF.

    li_client->request->set_compression( ).
    li_client->request->set_cdata( '' ).
    li_client->request->set_header_field(
        name  = '~request_method'
        value = iv_request ).
    li_client->request->set_header_field(
        name  = 'user-agent'
        value = get_agent( ) ).                             "#EC NOTEXT
    li_client->request->set_header_field(
        name  = '~request_uri'
        value = iv_url ).
    IF NOT iv_content IS INITIAL.
      li_client->request->set_header_field(
          name  = 'Content-type'
          value = iv_content ).                             "#EC NOTEXT
    ENDIF.

    " Disable internal auth dialog (due to its unclarity)
    li_client->propertytype_logon_popup = if_http_client=>co_disabled.

    /mbtools/cl_login_manager=>load(
      iv_uri    = iv_url
      ii_client = li_client ).

    ro_client->send_receive( ).

    IF check_auth_requested( li_client ) = abap_true.
      lv_scheme = acquire_login_details( ii_client = li_client
                                         io_client = ro_client
                                         iv_url    = iv_url ).
      ro_client->send_receive( ).
    ENDIF.

    ro_client->check_http_200( ).

    IF lv_scheme <> c_scheme-digest.
      /mbtools/cl_login_manager=>save( iv_uri    = iv_url
                                       ii_client = li_client ).
    ENDIF.

  ENDMETHOD.


  METHOD get_agent.

    rv_agent = |MBT { /mbtools/cl_tool_bc=>c_tool-version })|.

  ENDMETHOD.


  METHOD is_local_system.

    DATA: lv_host TYPE string,
          lt_list TYPE TABLE OF icm_sinfo2.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.

    CALL FUNCTION 'ICM_GET_INFO2'
      TABLES
        servlist    = lt_list
      EXCEPTIONS
        icm_error   = 1
        icm_timeout = 2
        OTHERS      = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO lt_list ASSIGNING <ls_list>.
    <ls_list>-hostname = 'localhost'.

    FIND REGEX 'https?://([^/^:]*)' IN iv_url
      SUBMATCHES lv_host.

    READ TABLE lt_list WITH KEY hostname = lv_host TRANSPORTING NO FIELDS ##WARN_OK.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD ping.

    DATA:
      lo_client    TYPE REF TO /mbtools/cl_http_client,
      lx_exception TYPE REF TO /mbtools/cx_exception.

    TRY.
        lo_client = create_by_url( iv_url = iv_url ).

        IF iv_regex IS SUPPLIED.
          lo_client->check_smart_response(
              iv_expected_content_type = 'text/html*'
              iv_content_regex         = iv_regex ).
        ENDIF.

        lo_client->close( ).

        rv_result = abap_true.
      CATCH /mbtools/cx_exception INTO lx_exception.
        IF lo_client IS BOUND.
          lo_client->close( ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
