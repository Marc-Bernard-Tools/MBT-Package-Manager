CLASS /mbtools/cl_edd DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Marc Bernard Tools - Easy Digital Downloads API
*
* https://docs.easydigitaldownloads.com/article/384-software-licensing-api
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_product,
        id            TYPE i,
        license       TYPE string,
        version       TYPE string,
        description   TYPE string,
        changelog_url TYPE string,
        changelog     TYPE string,
        download_url  TYPE string,
      END OF ty_product.
    TYPES:
      ty_products TYPE STANDARD TABLE OF ty_product WITH KEY id.

    CONSTANTS c_name TYPE string VALUE 'MBT_EDD_API' ##NO_TEXT ##NEEDED.
    CONSTANTS:
      BEGIN OF c_edd_action,
        activate   TYPE string VALUE 'activate_license' ##NO_TEXT,
        deactivate TYPE string VALUE 'deactivate_license' ##NO_TEXT,
        check      TYPE string VALUE 'check_license' ##NO_TEXT,
        version    TYPE string VALUE 'get_version' ##NO_TEXT,
      END OF c_edd_action.
    CONSTANTS:
      BEGIN OF c_edd_param,
        action TYPE string VALUE '$action$' ##NO_TEXT,
        id     TYPE string VALUE '$id$' ##NO_TEXT,
        key    TYPE string VALUE '$key$' ##NO_TEXT,
        url    TYPE string VALUE '$url$' ##NO_TEXT,
        sysid  TYPE string VALUE '$sysid$' ##NO_TEXT,
        sysno  TYPE string VALUE '$sysno$' ##NO_TEXT,
      END OF c_edd_param.

    CLASS-METHODS class_constructor.
    CLASS-METHODS activate_license
      IMPORTING
        !iv_id      TYPE string
        !iv_license TYPE string
      EXPORTING
        !ev_valid   TYPE abap_bool
        !ev_expire  TYPE d
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS deactivate_license
      IMPORTING
        !iv_id           TYPE string
        !iv_license      TYPE string
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS check_license
      IMPORTING
        !iv_id      TYPE string
        !iv_license TYPE string
      EXPORTING
        !ev_valid   TYPE abap_bool
        !ev_expire  TYPE d
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS get_version
      IMPORTING
        !iv_id            TYPE string
        !iv_license       TYPE string
      EXPORTING
        !ev_version       TYPE string
        !ev_description   TYPE string
        !ev_changelog_url TYPE string
        !ev_changelog     TYPE string
        !ev_download_url  TYPE string
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS get_versions
      CHANGING
        !ct_products TYPE ty_products
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS activate_pass
      IMPORTING
        !iv_license TYPE string
      EXPORTING
        !ev_valid   TYPE abap_bool
        !ev_expire  TYPE d
        !ev_id      TYPE string
      RAISING
        /mbtools/cx_exception.
  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-DATA gi_log TYPE REF TO /mbtools/if_logger.

    CLASS-METHODS _get_data
      IMPORTING
        !iv_path       TYPE string
        !iv_check      TYPE string DEFAULT '"success"'
      RETURNING
        VALUE(rv_data) TYPE string
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS _get_endpoint
      IMPORTING
        !iv_action         TYPE string
        !iv_id             TYPE string
        !iv_license        TYPE string
      RETURNING
        VALUE(rv_endpoint) TYPE string
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS _get_endpoint_products
      IMPORTING
        !iv_action         TYPE string
        !it_products       TYPE ty_products
      RETURNING
        VALUE(rv_endpoint) TYPE string
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS _get_json
      IMPORTING
        !iv_data       TYPE string
      RETURNING
        VALUE(ro_json) TYPE REF TO /mbtools/if_ajson
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS _adjust_html
      IMPORTING
        !iv_html         TYPE string
        !iv_headers      TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_result) TYPE string.
ENDCLASS.



CLASS /mbtools/cl_edd IMPLEMENTATION.


  METHOD activate_license.

*{
*    "success": true,
*    "license": "valid",
*    "item_id": false (or Item ID if passed)
*    "item_name": "EDD Product Name",
*    "license_limit": 0,
*    "site_count": 2,
*    "expires": "2020-06-30 23:59:59",
*    "activations_left": "unlimited",
*    "checksum": "<MD$ Checksum>",
*    "payment_id": 12345,
*    "customer_name": "John Doe",
*    "customer_email": "john@sample.org",
*    "price_id": "2"
*}

    DATA:
      lv_endpoint TYPE string,
      lv_data     TYPE string,
      lo_json     TYPE REF TO /mbtools/if_ajson.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    CLEAR: ev_valid, ev_expire.

    gi_log->i( |EDD API ActivateLicense for ID { iv_id }| ).

    lv_endpoint = _get_endpoint(
      iv_action  = c_edd_action-activate
      iv_id      = iv_id
      iv_license = iv_license ).

    lv_data = _get_data( lv_endpoint ).

    lo_json = _get_json( lv_data ).

    IF lo_json->get_boolean( '/success' ) <> abap_true.
      CASE lo_json->get_string( '/error' ).
        WHEN 'missing'.
          /mbtools/cx_exception=>raise( 'License doesn''t exist'(010) ).
        WHEN 'missing_url'.
          /mbtools/cx_exception=>raise( 'URL not provided'(011) ).
        WHEN 'license_not_activable'.
          /mbtools/cx_exception=>raise( 'Attempting to activate a bundle''s parent license'(012) ).
        WHEN 'disabled'.
          /mbtools/cx_exception=>raise( 'License key revoked'(013) ).
        WHEN 'no_activations_left'.
          /mbtools/cx_exception=>raise( 'No activations left'(014) ).
        WHEN 'expired'.
          /mbtools/cx_exception=>raise( 'License has expired'(015) ).
        WHEN 'key_mismatch'.
          /mbtools/cx_exception=>raise( 'License is not valid for this product'(016) ).
        WHEN 'invalid_item_id'.
          /mbtools/cx_exception=>raise( 'Invalid item ID'(017) ).
        WHEN 'item_name_mismatch'.
          /mbtools/cx_exception=>raise( 'License is not valid for this product'(018) ).
        WHEN OTHERS.
          /mbtools/cx_exception=>raise( 'License is not valid for this product'(018) ).
      ENDCASE.
    ENDIF.

    IF lo_json->get_string( '/license' ) = 'valid' ##NO_TEXT.
      ev_valid = abap_true.
    ENDIF.

    lv_data = lo_json->get_string( '/expires' ) ##NO_TEXT.

    ev_expire = lv_data(4) && lv_data+5(2) && lv_data+8(2).

  ENDMETHOD.


  METHOD activate_pass.

    " Try activation of different passes to find if one matches
    " Start with 'highest' and work the way down to 'lowest'
    ev_id = /mbtools/cl_access_pass=>c_pass-business.

    DO 3 TIMES.
      TRY.
          activate_license(
            EXPORTING
              iv_id      = ev_id
              iv_license = iv_license
            IMPORTING
              ev_valid   = ev_valid
              ev_expire  = ev_expire ).

          IF ev_valid = abap_true.
            EXIT.
          ENDIF.
        CATCH /mbtools/cx_exception ##NO_HANDLER.
      ENDTRY.

      IF ev_id = /mbtools/cl_access_pass=>c_pass-business.
        ev_id = /mbtools/cl_access_pass=>c_pass-professional.
      ELSEIF ev_id = /mbtools/cl_access_pass=>c_pass-professional.
        ev_id = /mbtools/cl_access_pass=>c_pass-starter.
      ENDIF.
    ENDDO.

    IF ev_valid = abap_false.
      ev_id = 0.
    ENDIF.

  ENDMETHOD.


  METHOD check_license.

*{
*    "success": true,
*    "license": "valid",
*    "item_id": false (or Item ID if passed)
*    "item_name": "EDD Product Name",
*    "license_limit": 0,
*    "site_count": 2,
*    "expires": "2020-06-30 23:59:59",
*    "activations_left": "unlimited",
*    "checksum": "<MD$ Checksum>",
*    "payment_id": 12345,
*    "customer_name": "John Doe",
*    "customer_email": "john@sample.org",
*    "price_id": "2"
*}

    DATA:
      lv_endpoint TYPE string,
      lv_data     TYPE string,
      lo_json     TYPE REF TO /mbtools/if_ajson.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    CLEAR: ev_valid, ev_expire.

    gi_log->i( |EDD API CheckLicense for ID { iv_id }| ).

    lv_endpoint = _get_endpoint(
      iv_action  = c_edd_action-check
      iv_id      = iv_id
      iv_license = iv_license ).

    lv_data = _get_data( lv_endpoint ).

    lo_json = _get_json( lv_data ).

    IF lo_json->get_boolean( '/success' ) <> abap_true.
      CASE lo_json->get_string( '/error' ).
        WHEN 'disabled'.
          /mbtools/cx_exception=>raise( 'License key revoked'(001) ).
        WHEN 'expired'.
          /mbtools/cx_exception=>raise( 'License has expired'(002) ).
        WHEN 'key_mismatch'.
          /mbtools/cx_exception=>raise( 'License is not valid for this product'(003) ).
        WHEN 'invalid_item_id'.
          /mbtools/cx_exception=>raise( 'Invalid item ID'(004) ).
        WHEN 'item_name_mismatch'.
          /mbtools/cx_exception=>raise( 'License is not valid for this product'(005) ).
        WHEN OTHERS.
          /mbtools/cx_exception=>raise( 'License is not valid for this product'(006) ).
      ENDCASE.
    ENDIF.

    IF lo_json->get_string( '/license' ) = 'valid' ##NO_TEXT.
      ev_valid = abap_true.
    ENDIF.

    lv_data = lo_json->get_date( 'expires' ) ##NO_TEXT.

    ev_expire = lv_data(4) && lv_data+5(2) && lv_data+8(2).

  ENDMETHOD.


  METHOD class_constructor.

    gi_log = /mbtools/cl_logger_factory=>create_log(
               object    = '/MBTOOLS/'
               subobject = 'EDD' ).

  ENDMETHOD.


  METHOD deactivate_license.

    DATA:
      lv_endpoint TYPE string,
      lv_data     TYPE string,
      lo_json     TYPE REF TO /mbtools/if_ajson.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    gi_log->i( |EDD API DeactivateLicense for ID { iv_id }| ).

    lv_endpoint = _get_endpoint(
      iv_action  = c_edd_action-deactivate
      iv_id      = iv_id
      iv_license = iv_license ).

    lv_data = _get_data( lv_endpoint ).

    lo_json = _get_json( lv_data ).

    IF lo_json->get_boolean( '/success' ) <> abap_true.
      " Probably wasn't a valid license in the first place. Ignore errors.
      rv_result = abap_true.
    ENDIF.

    rv_result = abap_true.

  ENDMETHOD.


  METHOD get_version.

*{
*  "new_version": "2.0",
*  "stable_version": "2.0",
*  "name": "Restrict Content Pro",
*  "slug": "restrict-content-pro",
*  "url": "https://edd.com/downloads/restrict-content-pro/?changelog=1",
*  "last_updated": "2017-01-03 11:59:46",
*  "homepage": "https://edd.com/downloads/restrict-content-pro/",
*  "package": "",
*  "download_link": "",
*  "sections": "...",
*  "banners": "..."
*}

    DATA:
      lv_endpoint TYPE string,
      lv_data     TYPE string,
      lv_sections TYPE string,
      lo_json     TYPE REF TO /mbtools/if_ajson,
      lx_error    TYPE REF TO /mbtools/cx_ajson_error.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    CLEAR: ev_version, ev_description, ev_changelog_url, ev_changelog, ev_download_url.

    gi_log->i( |EDD API GetVersion for ID { iv_id }| ).

    lv_endpoint = _get_endpoint(
      iv_action  = c_edd_action-version
      iv_id      = iv_id
      iv_license = iv_license ).

    lv_data = _get_data(
      iv_path  = lv_endpoint
      iv_check = '"new_version"' ).

    lo_json = _get_json( lv_data ).

    ev_version = lo_json->get_string( '/new_version' ).
    ev_changelog_url = lo_json->get_string( '/url' ).
    ev_download_url = lo_json->get_string( '/download_link' ).
    lv_sections = lo_json->get_string( '/sections' ).
    REPLACE ALL OCCURRENCES OF '&nbsp;' IN lv_sections WITH ` `.

    TRY.
        lo_json = /mbtools/cl_aphp=>unserialize(
          iv_data       = lv_sections
          iv_ignore_len = abap_true ).

        IF lo_json->get_string( '/a/1/key' ) = 'description'.
          ev_description = lo_json->get_string( '/a/1/val' ).
          ev_description = _adjust_html( ev_description ).
          IF ev_description CS '<p>Requirements'.
            ev_description = ev_description(sy-fdpos).
          ENDIF.
          IF ev_description CS '<p>Screenshots'.
            ev_description = ev_description(sy-fdpos).
          ENDIF.
        ENDIF.
        IF lo_json->get_string( '/a/2/key' ) = 'changelog'.
          ev_changelog = lo_json->get_string( '/a/2/val' ).
          ev_changelog = _adjust_html(
            iv_html    = ev_changelog
            iv_headers = abap_true ).
        ENDIF.
      CATCH /mbtools/cx_ajson_error INTO lx_error.
        /mbtools/cx_exception=>raise( lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_versions.

    DATA:
      lv_endpoint TYPE string,
      lv_data     TYPE string,
      lv_sections TYPE string,
      lo_json_all TYPE REF TO /mbtools/if_ajson,
      lo_json     TYPE REF TO /mbtools/if_ajson,
      lx_error    TYPE REF TO /mbtools/cx_ajson_error.

    FIELD-SYMBOLS:
      <ls_product> LIKE LINE OF ct_products.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    gi_log->i( |EDD API GetVersion for { lines( ct_products ) } products| ).

    lv_endpoint = _get_endpoint_products(
      iv_action   = c_edd_action-version
      it_products = ct_products ).

    lv_data = _get_data(
      iv_path  = lv_endpoint
      iv_check = '"new_version"' ).

    lo_json_all = _get_json( lv_data ).

    LOOP AT ct_products ASSIGNING <ls_product>.

      lo_json = lo_json_all->slice( condense( |/{ <ls_product>-id }| ) ).

      <ls_product>-version = lo_json->get_string( '/new_version' ).
      <ls_product>-changelog_url = lo_json->get_string( '/url' ).
      <ls_product>-download_url = lo_json->get_string( '/download_link' ).
      lv_sections = lo_json->get_string( '/sections' ).
      REPLACE ALL OCCURRENCES OF '&nbsp;' IN lv_sections WITH ` `.

      TRY.
          lo_json = /mbtools/cl_aphp=>unserialize(
            iv_data       = lv_sections
            iv_ignore_len = abap_true ).

          IF lo_json->get_string( '/a/1/key' ) = 'description'.
            <ls_product>-description = lo_json->get_string( '/a/1/val' ).
            <ls_product>-description = _adjust_html( <ls_product>-description ).
            IF <ls_product>-description CS '<p>Requirements'.
              <ls_product>-description = <ls_product>-description(sy-fdpos).
            ENDIF.
            IF <ls_product>-description CS '<p>Screenshots'.
              <ls_product>-description = <ls_product>-description(sy-fdpos).
            ENDIF.
          ENDIF.
          IF lo_json->get_string( '/a/2/key' ) = 'changelog'.
            <ls_product>-changelog = lo_json->get_string( '/a/2/val' ).
            <ls_product>-changelog = _adjust_html(
              iv_html    = <ls_product>-changelog
              iv_headers = abap_true ).
          ENDIF.
        CATCH /mbtools/cx_ajson_error INTO lx_error.
          /mbtools/cx_exception=>raise( lx_error->get_text( ) ).
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD _adjust_html.

    rv_result = iv_html.

    REPLACE ALL OCCURRENCES OF 'href="/' IN rv_result WITH 'href="' && /mbtools/if_definitions=>c_www_home.

    IF iv_headers = abap_true.
      REPLACE ALL OCCURRENCES OF '<p>' IN rv_result WITH '<h4>'.
      REPLACE ALL OCCURRENCES OF '</p>' IN rv_result WITH '</h4>'.
    ENDIF.

  ENDMETHOD.


  METHOD _get_data.

    DATA:
      lv_rfcdest TYPE rfcdest,
      lo_timer   TYPE REF TO /mbtools/cl_timer,
      lo_client  TYPE REF TO /mbtools/cl_http_client,
      lx_error   TYPE REF TO /mbtools/cx_exception.

    CREATE OBJECT lo_timer.

    lo_timer->start( ).

    lv_rfcdest = /mbtools/cl_setup=>get_rfc_destination( ).

    gi_log->i( |RFC Destination { lv_rfcdest }| ).
    gi_log->i( |Endpoint { iv_path }| ).

    TRY.
        lo_client = /mbtools/cl_http=>create_by_destination(
          iv_destination = lv_rfcdest
          iv_path        = iv_path
          iv_accept      = 'application/x-www-form-urlencoded' ).

        lo_client->check_smart_response(
          iv_expected_content_type = 'application/json'
          iv_content_regex         = iv_check ).

        rv_data = lo_client->get_cdata( ).

        lo_client->close( ).
      CATCH /mbtools/cx_exception INTO lx_error.
        IF lo_client IS BOUND.
          lo_client->close( ).
        ENDIF.
        gi_log->e( lx_error ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    gi_log->i( lo_timer->end( ) ).

  ENDMETHOD.


  METHOD _get_endpoint.

    DATA: lv_system_no TYPE slic_sysid.

    " https://marcbernardtools.com/?edd_action={request type}&item_id={id}&license={key}
    " &url=SystemID_{system_id}_SystemNumber_{system_number}
    rv_endpoint = '/?edd_action=' && c_edd_param-action && '&item_id=' && c_edd_param-id.
    rv_endpoint = rv_endpoint && '&license=' && c_edd_param-key.
    rv_endpoint = rv_endpoint && '&url=SystemID_' && c_edd_param-sysid && '_SystemNumber_' && c_edd_param-sysno.

    " Get system number
    CALL FUNCTION 'SLIC_GET_SYSTEM_ID'
      IMPORTING
        systemid = lv_system_no.

    SHIFT lv_system_no LEFT DELETING LEADING '0'.

    IF ( lv_system_no CS 'INITIAL' OR lv_system_no NA '0123456789' ) AND lv_system_no <> 'DEMOSYSTEM'.
      /mbtools/cx_exception=>raise( 'Initial system number (transaction SLICENSE)' ) ##NO_TEXT.
    ENDIF.

    REPLACE c_edd_param-action WITH iv_action    INTO rv_endpoint.
    REPLACE c_edd_param-id     WITH iv_id        INTO rv_endpoint.
    REPLACE c_edd_param-key    WITH iv_license   INTO rv_endpoint.
    REPLACE c_edd_param-sysid  WITH sy-sysid     INTO rv_endpoint.
    REPLACE c_edd_param-sysno  WITH lv_system_no INTO rv_endpoint.

    CONDENSE rv_endpoint NO-GAPS.

  ENDMETHOD.


  METHOD _get_endpoint_products.

    DATA:
      lv_system_no TYPE slic_sysid,
      ls_products  LIKE LINE OF it_products.

    " Get system number
    CALL FUNCTION 'SLIC_GET_SYSTEM_ID'
      IMPORTING
        systemid = lv_system_no.

    SHIFT lv_system_no LEFT DELETING LEADING '0'.

    IF ( lv_system_no CS 'INITIAL' OR lv_system_no NA '0123456789' ) AND lv_system_no <> 'DEMOSYSTEM'.
      /mbtools/cx_exception=>raise( 'Initial system number (transaction SLICENSE)' ) ##NO_TEXT.
    ENDIF.

    " http://yoursite.com/?edd_action={request_type}&products[{id}][item_id]={id}&products[{id}][license]={key}
    " &products[{id}][url]=SystemID_{system_id}_SystemNumber_{system_number}
    rv_endpoint = '/?edd_action=' && c_edd_param-action.
    REPLACE c_edd_param-action WITH iv_action INTO rv_endpoint.

    LOOP AT it_products INTO ls_products.
      rv_endpoint = rv_endpoint && '&products[' && c_edd_param-id && '][item_id]=' && c_edd_param-id.
      rv_endpoint = rv_endpoint && '&products[' && c_edd_param-id && '][license]=' && c_edd_param-key.
      rv_endpoint = rv_endpoint && '&products[' && c_edd_param-id && '][url]=SystemID_' &&
                    c_edd_param-sysid && '_SystemNumber_' && c_edd_param-sysno.

      REPLACE ALL OCCURRENCES OF c_edd_param-id  IN rv_endpoint WITH condense( |{ ls_products-id }| ).
      REPLACE ALL OCCURRENCES OF c_edd_param-key IN rv_endpoint WITH ls_products-license.
    ENDLOOP.

    REPLACE ALL OCCURRENCES OF c_edd_param-sysid IN rv_endpoint WITH sy-sysid.
    REPLACE ALL OCCURRENCES OF c_edd_param-sysno IN rv_endpoint WITH lv_system_no.

    CONDENSE rv_endpoint NO-GAPS.

  ENDMETHOD.


  METHOD _get_json.

    DATA lx_error TYPE REF TO /mbtools/cx_ajson_error.

    TRY.
        ro_json = /mbtools/cl_ajson=>parse( iv_data ).
      CATCH /mbtools/cx_ajson_error INTO lx_error.
        /mbtools/cx_exception=>raise( 'Error parsing response from MBT website:'(000) && lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
