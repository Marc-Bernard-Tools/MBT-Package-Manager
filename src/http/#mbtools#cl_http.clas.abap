CLASS /mbtools/cl_http DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* MBT HTTP
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_multipart,
        ctype TYPE string,
        cdata TYPE string,
      END OF ty_multipart.
    TYPES:
      ty_multiparts TYPE STANDARD TABLE OF ty_multipart WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF c_scheme,
        digest TYPE string VALUE 'Digest' ##NO_TEXT,
      END OF c_scheme.

    CLASS-METHODS class_constructor.
    CLASS-METHODS get_agent
      RETURNING
        VALUE(rv_agent) TYPE string.
    CLASS-METHODS create_by_url
      IMPORTING
        !iv_url          TYPE string
        !iv_request      TYPE string DEFAULT if_http_request=>co_request_method_get
        !iv_content      TYPE string OPTIONAL
      RETURNING
        VALUE(ro_client) TYPE REF TO /mbtools/cl_http_client
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS create_by_destination
      IMPORTING
        !iv_destination  TYPE rfcdest
        !iv_path         TYPE string
        !iv_request      TYPE string DEFAULT if_http_request=>co_request_method_get
        !iv_content      TYPE string OPTIONAL
        !iv_accept       TYPE string OPTIONAL
        !it_multipart    TYPE ty_multiparts OPTIONAL
      RETURNING
        VALUE(ro_client) TYPE REF TO /mbtools/cl_http_client
      RAISING
        /mbtools/cx_exception.
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
    CLASS-DATA go_settings TYPE REF TO /mbtools/cl_registry .
ENDCLASS.



CLASS /mbtools/cl_http IMPLEMENTATION.


  METHOD acquire_login_details.

    DATA: lv_default_user TYPE string,
          lv_user         TYPE string,
          lv_pass         TYPE string,
          lo_digest       TYPE REF TO /mbtools/cl_http_digest.


    lv_default_user = go_settings->get_value( c_login-user ).
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
          go_settings->set_value( iv_key = c_login-user
                                  iv_value = lv_user ).
          go_settings->save( ).
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

    go_settings = /mbtools/cl_tools=>factory( )->get_settings( ).

  ENDMETHOD.


  METHOD create_by_destination.

    DATA:
      li_client    TYPE REF TO if_http_client,
      li_part      TYPE REF TO if_http_entity,
      ls_multipart TYPE ty_multipart,
      lv_text      TYPE string.

    cl_http_client=>create_by_destination(
      EXPORTING
        destination              = iv_destination
      IMPORTING
        client                   = li_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6 ).
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          lv_text = |Check RFC destination { iv_destination } in transaction SM59|.
        WHEN 2.
          lv_text = |RFC destination { iv_destination } not found|.
        WHEN 3.
          lv_text = |No authorization to use RFC destination { iv_destination }|.
        WHEN OTHERS.
          lv_text = |RFC destination { iv_destination }, Error { sy-subrc }|.
      ENDCASE.
      /mbtools/cx_exception=>raise( 'Error creating connection:'(000) && lv_text ) ##NO_TEXT.
    ENDIF.

    CREATE OBJECT ro_client
      EXPORTING
        ii_client = li_client.

    li_client->request->set_compression( ).
    li_client->request->set_cdata( '' ).
    li_client->request->set_header_field(
      name  = '~request_method'
      value = iv_request ) ##NO_TEXT.
    li_client->request->set_header_field(
      name  = 'user-agent'
      value = get_agent( ) ) ##NO_TEXT.
    li_client->request->set_header_field(
      name  = '~request_uri'
      value = iv_path ) ##NO_TEXT.
    IF iv_accept IS NOT INITIAL.
      li_client->request->set_header_field(
        name  = 'Accept'
        value = iv_accept ) ##NO_TEXT.
    ELSEIF it_multipart IS NOT INITIAL.
      li_client->request->set_header_field(
        name  = 'Accept'
        value = 'multipart/mixed' ) ##NO_TEXT.
    ENDIF.
    IF iv_content IS NOT INITIAL.
      li_client->request->set_header_field(
        name  = 'Content-type'
        value = iv_content ) ##NO_TEXT.
    ELSEIF it_multipart IS NOT INITIAL.
      li_client->request->set_header_field(
        name  = 'Content-type'
        value = 'multipart/mixed' ) ##NO_TEXT.
    ENDIF.

    " Disable internal auth dialog (due to its unclarity)
    li_client->propertytype_logon_popup = if_http_client=>co_disabled.

    " Multiparts
    LOOP AT it_multipart INTO ls_multipart.
      li_part = li_client->request->add_multipart( ).
*      li_part->set_header_field(
*        name  = '~request_method'
*        value = 'GET' )
      li_part->set_header_field(
        name  = 'Content-type'
        value = 'application/http' ) ##NO_TEXT.
      li_part->set_header_field(
        name  = 'Accept'
        value = ls_multipart-ctype ) ##NO_TEXT.
      li_part->append_cdata( ls_multipart-cdata ).
    ENDLOOP.

    ro_client->send_receive( ).

    ro_client->check_http_200( ).

  ENDMETHOD.


  METHOD create_by_url.

    DATA:
      lv_scheme       TYPE string,
      li_client       TYPE REF TO if_http_client,
      lo_proxy_config TYPE REF TO /mbtools/cl_proxy_config,
      lv_text         TYPE string.

    CREATE OBJECT lo_proxy_config.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = iv_url
        ssl_id             = 'ANONYM'
        proxy_host         = lo_proxy_config->get_proxy_host( )
        proxy_service      = lo_proxy_config->get_proxy_port( )
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

    IF lo_proxy_config->get_proxy_authentication( ) = abap_true.
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
    IF iv_content IS NOT INITIAL.
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
ENDCLASS.
