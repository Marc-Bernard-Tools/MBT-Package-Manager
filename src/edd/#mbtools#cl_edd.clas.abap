CLASS /mbtools/cl_edd DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* MBT Easy Digital Downloads API
*
* https://docs.easydigitaldownloads.com/article/384-software-licensing-api
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
  PUBLIC SECTION.

    CONSTANTS c_name TYPE string VALUE 'MBT_EDD_API' ##NEEDED.
    CONSTANTS c_edd_host TYPE string VALUE 'https://marcbernardtools.com/' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF c_edd_action,
        activate   TYPE string VALUE 'activate_license' ##NO_TEXT,
        deactivate TYPE string VALUE 'deactivate_license' ##NO_TEXT,
        check      TYPE string VALUE 'check_license' ##NO_TEXT,
        version    TYPE string VALUE 'get_version' ##NO_TEXT,
      END OF c_edd_action .
    CONSTANTS:
      BEGIN OF c_edd_param,
        action TYPE string VALUE '$action$' ##NO_TEXT,
        id     TYPE string VALUE '$id$' ##NO_TEXT,
        key    TYPE string VALUE '$key$' ##NO_TEXT,
        url    TYPE string VALUE '$url$' ##NO_TEXT,
        sysid  TYPE string VALUE '$sysid$' ##NO_TEXT,
        sysno  TYPE string VALUE '$sysno$' ##NO_TEXT,
      END OF c_edd_param .

    CLASS-METHODS activate_license
      IMPORTING
        !iv_id      TYPE string
        !iv_license TYPE string
      EXPORTING
        !ev_valid   TYPE abap_bool
        !ev_expire  TYPE d
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS deactivate_license
      IMPORTING
        !iv_id           TYPE string
        !iv_license      TYPE string
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS check_license
      IMPORTING
        !iv_id      TYPE string
        !iv_license TYPE string
      EXPORTING
        !ev_valid   TYPE abap_bool
        !ev_expire  TYPE d
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS get_version
      IMPORTING
        !iv_id             TYPE string
        !iv_license        TYPE string
      EXPORTING
        !ev_version        TYPE string
        !ev_changelog_url  TYPE string
        !ev_changelog_html TYPE string
        !ev_download_url   TYPE string
      RAISING
        /mbtools/cx_exception .
  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-METHODS get_data
      IMPORTING
        !iv_url        TYPE string
        !iv_check      TYPE string DEFAULT '"success"'
      RETURNING
        VALUE(rv_data) TYPE string
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS get_endpoint
      IMPORTING
        !iv_action         TYPE string
        !iv_id             TYPE string
        !iv_license        TYPE string
      RETURNING
        VALUE(rv_endpoint) TYPE string
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS get_json
      IMPORTING
        !iv_data       TYPE string
      RETURNING
        VALUE(ro_json) TYPE REF TO /mbtools/if_ajson_reader
      RAISING
        /mbtools/cx_exception .
ENDCLASS.



CLASS /MBTOOLS/CL_EDD IMPLEMENTATION.


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
      lo_json     TYPE REF TO /mbtools/if_ajson_reader.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    lv_endpoint = get_endpoint( iv_action  = c_edd_action-activate
                                iv_id      = iv_id
                                iv_license = iv_license ).

    lv_data = get_data( lv_endpoint ).

    lo_json = get_json( lv_data ).

    IF lo_json->get_boolean( '/success' ) <> abap_true.
      CASE lo_json->get_string( '/error' ).
        WHEN 'missing'.
          /mbtools/cx_exception=>raise( 'License doesn''t exist' ).
        WHEN 'missing_url'.
          /mbtools/cx_exception=>raise( 'URL not provided' ).
        WHEN 'license_not_activable'.
          /mbtools/cx_exception=>raise( 'Attempting to activate a bundle''s parent license' ).
        WHEN 'disabled'.
          /mbtools/cx_exception=>raise( 'License key revoked' ).
        WHEN 'no_activations_left'.
          /mbtools/cx_exception=>raise( 'No activations left' ).
        WHEN 'expired'.
          /mbtools/cx_exception=>raise( 'License has expired' ).
        WHEN 'key_mismatch'.
          /mbtools/cx_exception=>raise( 'License is not valid for this product' ).
        WHEN 'invalid_item_id'.
          /mbtools/cx_exception=>raise( 'Invalid item ID' ).
        WHEN 'item_name_mismatch'.
          /mbtools/cx_exception=>raise( 'License is not valid for this product' ).
        WHEN OTHERS.
          /mbtools/cx_exception=>raise( 'License is not valid for this product' ).
      ENDCASE.
    ENDIF.

    IF lo_json->get_string( '/license' ) = 'valid'.
      ev_valid = abap_true.
    ENDIF.

    ev_expire = lo_json->get_date( '/expires' ).

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
      lo_json     TYPE REF TO /mbtools/if_ajson_reader.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    lv_endpoint = get_endpoint( iv_action  = c_edd_action-check
                                iv_id      = iv_id
                                iv_license = iv_license ).

    lv_data = get_data( lv_endpoint ).

    lo_json = get_json( lv_data ).

    IF lo_json->get_boolean( '/success' ) <> abap_true.
      CASE lo_json->get_string( '/error' ).
        WHEN 'disabled'.
          /mbtools/cx_exception=>raise( 'License key revoked' ).
        WHEN 'expired'.
          /mbtools/cx_exception=>raise( 'License has expired' ).
        WHEN 'key_mismatch'.
          /mbtools/cx_exception=>raise( 'License is not valid for this product' ).
        WHEN 'invalid_item_id'.
          /mbtools/cx_exception=>raise( 'Invalid item ID' ).
        WHEN 'item_name_mismatch'.
          /mbtools/cx_exception=>raise( 'License is not valid for this product' ).
        WHEN OTHERS.
          /mbtools/cx_exception=>raise( 'License is not valid for this product' ).
      ENDCASE.
    ENDIF.

    IF lo_json->get_string( '/license' ) = 'valid'.
      ev_valid = abap_true.
    ENDIF.

    ev_expire = lo_json->get_date( 'expires' ).

  ENDMETHOD.


  METHOD deactivate_license.

    DATA:
      lv_endpoint TYPE string,
      lv_data     TYPE string,
      lo_json     TYPE REF TO /mbtools/if_ajson_reader.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    lv_endpoint = get_endpoint( iv_action  = c_edd_action-deactivate
                                iv_id      = iv_id
                                iv_license = iv_license ).

    lv_data = get_data( lv_endpoint ).

    lo_json = get_json( lv_data ).

    IF lo_json->get_boolean( '/success' ) <> abap_true.
      " Probably wasn't a valid license in the first place. Ignore errors.
      rv_result = abap_true.
    ENDIF.

    rv_result = abap_true.

  ENDMETHOD.


  METHOD get_data.

    DATA:
      lo_client    TYPE REF TO /mbtools/cl_http_client,
      lx_exception TYPE REF TO /mbtools/cx_exception.

    TRY.
        lo_client = /mbtools/cl_http=>create_by_url( iv_url     = iv_url
                                                     iv_request = 'GET'
                                                     iv_content = 'application/x-www-form-urlencoded' ).

        lo_client->check_smart_response(
          iv_expected_content_type = 'application/json'
          iv_content_regex         = iv_check ).

        rv_data = lo_client->get_cdata( ).

        lo_client->close( ).
      CATCH /mbtools/cx_exception INTO lx_exception.
        IF lo_client IS BOUND.
          lo_client->close( ).
        ENDIF.
        /mbtools/cx_exception=>raise( lx_exception->get_text( ) ).
    ENDTRY.


  ENDMETHOD.


  METHOD get_endpoint.

    DATA: lv_system_no TYPE slic_sysid.

    " http://yoursite.com/?edd_action={request type}&item_id={id}&license={key}
    " &url=SystemID_{system_id}_SystemNumber_{system_number}
    rv_endpoint = c_edd_host && '?edd_action=' && c_edd_param-action && '&item_id=' && c_edd_param-id.
    rv_endpoint = rv_endpoint && '&license=' && c_edd_param-key.
    rv_endpoint = rv_endpoint && '&url=SystemID_' && c_edd_param-sysid && '_SystemNumber_' && c_edd_param-sysno.

    " Get system number
    CALL FUNCTION 'SLIC_GET_SYSTEM_ID'
      IMPORTING
        systemid = lv_system_no.

    SHIFT lv_system_no LEFT DELETING LEADING '0'.

    IF lv_system_no CS'INITIAL' OR lv_system_no NA '0123456789'.
      /mbtools/cx_exception=>raise( 'Initial system number (transaction SLICENSE)' ) ##NO_TEXT.
    ENDIF.

    REPLACE c_edd_param-action WITH iv_action    INTO rv_endpoint.
    REPLACE c_edd_param-id     WITH iv_id        INTO rv_endpoint.
    REPLACE c_edd_param-key    WITH iv_license   INTO rv_endpoint.
    REPLACE c_edd_param-sysid  WITH sy-sysid     INTO rv_endpoint.
    REPLACE c_edd_param-sysno  WITH lv_system_no INTO rv_endpoint.

    CONDENSE rv_endpoint NO-GAPS.

  ENDMETHOD.


  METHOD get_json.

    DATA lx_exception TYPE REF TO /mbtools/cx_ajson_error.

    TRY.
        ro_json = /mbtools/cl_ajson=>parse( iv_data ).
      CATCH /mbtools/cx_ajson_error INTO lx_exception.
        /mbtools/cx_exception=>raise( 'Error parsing response from MBT website:' && lx_exception->get_text( ) ).
    ENDTRY.

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
      lo_json     TYPE REF TO /mbtools/if_ajson_reader.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    lv_endpoint = get_endpoint( iv_action  = c_edd_action-version
                                iv_id      = iv_id
                                iv_license = iv_license ).

    lv_data = get_data( iv_url   = lv_endpoint
                        iv_check = '"new_version"' ).

    lo_json = get_json( lv_data ).

    ev_version = lo_json->get_string( '/new_version' ).

    ev_changelog_url = lo_json->get_string( '/url' ).

    ev_download_url = lo_json->get_string( '/download_link' ).

    ev_changelog_html = '' ##TODO.

  ENDMETHOD.
ENDCLASS.
