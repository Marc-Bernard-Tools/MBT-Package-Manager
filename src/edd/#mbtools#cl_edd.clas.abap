************************************************************************
* /MBTOOLS/CL_EDD
* MBT Easy Digital Downloads API
*
* https://docs.easydigitaldownloads.com/article/384-software-licensing-api
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
class /MBTOOLS/CL_EDD definition
  public
  final
  create public .

public section.

  constants C_NAME type STRING value 'MBT_EDD_API' ##NO_TEXT.
  constants C_VERSION type STRING value '1.0.0' ##NO_TEXT.
  constants C_EDD_HOST type STRING value 'https://marcbernardtools.com/' ##NO_TEXT.
  constants:
    BEGIN OF c_action,
        activate   TYPE string VALUE 'activate_license' ##NO_TEXT,
        deactivate TYPE string VALUE 'deactivate_license' ##NO_TEXT,
        check      TYPE string VALUE 'check_license' ##NO_TEXT,
        version    TYPE string VALUE 'get_version' ##NO_TEXT,
      END OF c_action .
  constants:
    BEGIN OF c_param,
        action TYPE string VALUE '$action$' ##NO_TEXT,
        id     TYPE string VALUE '$id$' ##NO_TEXT,
        key    TYPE string VALUE '$key$' ##NO_TEXT,
        url    TYPE string VALUE '$url$' ##NO_TEXT,
        system TYPE string VALUE '$system$' ##NO_TEXT,
      END OF c_param .

  class-methods ACTIVATE_LICENSE
    importing
      !IV_ID type STRING
      !IV_LICENSE type STRING
    exporting
      !EV_VALID type ABAP_BOOL
      !EV_EXPIRE type D
    raising
      /MBTOOLS/CX_EXCEPTION .
  class-methods DEACTIVATE_LICENSE
    importing
      !IV_ID type STRING
      !IV_LICENSE type STRING
    exporting
      !EV_VALID type ABAP_BOOL
      !EV_EXPIRE type D
    raising
      /MBTOOLS/CX_EXCEPTION .
  class-methods CHECK_LICENSE
    importing
      !IV_ID type STRING
      !IV_LICENSE type STRING
    exporting
      !EV_VALID type ABAP_BOOL
      !EV_EXPIRE type D
    raising
      /MBTOOLS/CX_EXCEPTION .
  class-methods GET_VERSION
    importing
      !IV_ID type STRING
      !IV_LICENSE type STRING
    exporting
      !EV_VALID type ABAP_BOOL
      !EV_EXPIRE type D
    raising
      /MBTOOLS/CX_EXCEPTION .
  PROTECTED SECTION.
private section.

  class-methods GET_DATA
    importing
      !IV_URL type STRING
    returning
      value(RV_DATA) type STRING
    raising
      /MBTOOLS/CX_EXCEPTION .
  class-methods GET_ENDPOINT
    importing
      !IV_ACTION type STRING
      !IV_ID type STRING
      !IV_LICENSE type STRING
    returning
      value(RV_ENDPOINT) type STRING
    raising
      /MBTOOLS/CX_EXCEPTION .
ENDCLASS.



CLASS /MBTOOLS/CL_EDD IMPLEMENTATION.


  METHOD activate_license.

    DATA:
      lv_endpoint TYPE string,
      lv_data     TYPE string.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    lv_endpoint = get_endpoint( iv_action  = c_action-activate
                                iv_id      = iv_id
                                iv_license = iv_license ).

    lv_data = get_data( lv_endpoint ).

##TODO.

  ENDMETHOD.


  METHOD check_license.

    DATA:
      lv_endpoint TYPE string,
      lv_data     TYPE string.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    lv_endpoint = get_endpoint( iv_action  = c_action-check
                                iv_id      = iv_id
                                iv_license = iv_license ).

    lv_data = get_data( lv_endpoint ).

##TODO.

  ENDMETHOD.


  METHOD deactivate_license.

    DATA:
      lv_endpoint TYPE string,
      lv_data     TYPE string.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    lv_endpoint = get_endpoint( iv_action  = c_action-deactivate
                                iv_id      = iv_id
                                iv_license = iv_license ).

    lv_data = get_data( lv_endpoint ).

##TODO.

  ENDMETHOD.


  METHOD get_data.

    DATA:
      lo_client TYPE REF TO /mbtools/cl_http_client.

    lo_client = /mbtools/cl_http=>create_by_url( iv_url     = iv_url
                                                 iv_request = 'POST'
                                                 iv_content = 'application/x-www-form-urlencoded' ).

    lo_client->check_smart_response(
        iv_expected_content_type = 'application/json'
        iv_content_regex         = '"success"' ).

    rv_data = lo_client->get_cdata( ).

    lo_client->close( ).

  ENDMETHOD.


  METHOD get_endpoint.

    DATA:
      lv_subrc       TYPE sy-subrc,
      lv_system_host TYPE string,
      lv_system_id   TYPE slic_sysid.

    " http://yoursite.com/?edd_action={request type}&item_id={id}&license={key}
    " &url={url of the site being licensed}/{system number}
    rv_endpoint = c_edd_host && '?edd_action=' && c_param-action && '&item_id=' && c_param-id.
    rv_endpoint = rv_endpoint && '&license=' && c_param-key && '&url=' && c_param-url && '/' && c_param-system.

    REPLACE c_param-action WITH iv_action  INTO rv_endpoint.
    REPLACE c_param-id     WITH iv_id      INTO rv_endpoint.
    REPLACE c_param-key    WITH iv_license INTO rv_endpoint.

    CALL FUNCTION 'SPFL_PARAMETER_GET_VALUE'
      EXPORTING
        name  = 'SAPDBHOST'
      IMPORTING
        value = lv_system_host
        rc    = lv_subrc.
    IF lv_subrc <> 0 OR lv_system_host IS INITIAL.
      /mbtools/cx_exception=>raise( 'Error getting system host (SAPDBHOST)' ) ##NO_TEXT.
    ENDIF.

    REPLACE c_param-url WITH lv_system_host INTO rv_endpoint.

    CALL FUNCTION 'SLIC_GET_SYSTEM_ID'
      IMPORTING
        systemid = lv_system_id.

    IF lv_system_id CS'INITIAL' OR lv_system_id NA '0123456789'.
      /mbtools/cx_exception=>raise( 'Initial system number (transaction SLICENSE)' ) ##NO_TEXT.
    ENDIF.

    REPLACE c_param-system WITH lv_system_id INTO rv_endpoint.
    CONDENSE rv_endpoint NO-GAPS.

  ENDMETHOD.


  METHOD get_version.

    DATA:
      lv_endpoint TYPE string,
      lv_data     TYPE string.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    lv_endpoint = get_endpoint( iv_action  = c_action-version
                                iv_id      = iv_id
                                iv_license = iv_license ).

    lv_data = get_data( lv_endpoint ).

##TODO.

  ENDMETHOD.
ENDCLASS.
