************************************************************************
* /MBTOOLS/CL_EDD
* MBT Easy Digital Downloads API
*
* https://docs.easydigitaldownloads.com/article/384-software-licensing-api
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_edd DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_name TYPE string VALUE 'MBT_EDD_API' ##NO_TEXT.
    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NO_TEXT.

    CONSTANTS c_edd_host TYPE string VALUE 'https://marcbernardtools.com/' ##NO_TEXT.
    CONSTANTS c_action_activate TYPE string VALUE 'activatev_license' ##NO_TEXT.
    CONSTANTS c_action_deactivate TYPE string VALUE 'deactivatev_license' ##NO_TEXT.
    CONSTANTS c_action_check TYPE string VALUE 'check_license' ##NO_TEXT.
    CONSTANTS c_action_version TYPE string VALUE 'get_version' ##NO_TEXT.
    CONSTANTS c_param_action TYPE string VALUE '$action$' ##NO_TEXT.
    CONSTANTS c_param_id TYPE string VALUE '$id$' ##NO_TEXT.
    CONSTANTS c_param_key TYPE string VALUE '$key$' ##NO_TEXT.
    CONSTANTS c_param_url TYPE string VALUE '$url$' ##NO_TEXT.
    CONSTANTS c_param_system TYPE string VALUE '$system$' ##NO_TEXT.

    CLASS-METHODS activatev_license
      IMPORTING
        !iv_id      TYPE string
        !iv_license TYPE string
      EXPORTING
        !ev_valid   TYPE abap_bool
        !ev_expire  TYPE d
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS deactivatev_license
      IMPORTING
        !iv_id      TYPE string
        !iv_license TYPE string
      EXPORTING
        !ev_valid   TYPE abap_bool
        !ev_expire  TYPE d
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
        !iv_id      TYPE string
        !iv_license TYPE string
      EXPORTING
        !ev_valid   TYPE abap_bool
        !ev_expire  TYPE d
      RAISING
        /mbtools/cx_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS get_endpoint
      IMPORTING
        !iv_action         TYPE string
        !iv_id             TYPE string
        !iv_license        TYPE string
      RETURNING
        VALUE(rv_endpoint) TYPE string
      RAISING
        /mbtools/cx_exception .
ENDCLASS.



CLASS /MBTOOLS/CL_EDD IMPLEMENTATION.


  METHOD activatev_license.

    DATA:
      lv_endpoint TYPE string.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    CALL METHOD get_endpoint
      EXPORTING
        iv_action   = c_action_activate
        iv_id       = iv_id
        iv_license  = iv_license
      RECEIVING
        rv_endpoint = lv_endpoint.

  ENDMETHOD.


  METHOD check_license.

    DATA:
      lv_endpoint TYPE string.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    CALL METHOD get_endpoint
      EXPORTING
        iv_action   = c_action_check
        iv_id       = iv_id
        iv_license  = iv_license
      RECEIVING
        rv_endpoint = lv_endpoint.

  ENDMETHOD.


  METHOD deactivatev_license.

    DATA:
      lv_endpoint TYPE string.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    CALL METHOD get_endpoint
      EXPORTING
        iv_action   = c_action_deactivate
        iv_id       = iv_id
        iv_license  = iv_license
      RECEIVING
        rv_endpoint = lv_endpoint.

  ENDMETHOD.


  METHOD get_endpoint.

    DATA:
      lv_subrc       TYPE sy-subrc,
      lv_system_host TYPE string,
      lv_system_id   TYPE slic_sysid.

*   http://yoursite.com/?edd_action={request type}&item_id={id}&license={key}&url={url of the site being licensed}/{system number}
    rv_endpoint = c_edd_host && '?edd_action=' && c_param_action && '&item_id=' && c_param_id.
    rv_endpoint = rv_endpoint && '&license=' && c_param_key && '&url=' && c_param_url && '/' && c_param_system.

    REPLACE c_param_action WITH iv_action  INTO rv_endpoint.
    REPLACE c_param_id     WITH iv_id      INTO rv_endpoint.
    REPLACE c_param_key    WITH iv_license INTO rv_endpoint.

    CALL FUNCTION 'SPFL_PARAMETER_GET_VALUE'
      EXPORTING
        name  = 'SAPDBHOST'
      IMPORTING
        value = lv_system_host
        rc    = lv_subrc.
    IF lv_subrc <> 0 OR lv_system_host IS INITIAL.
      /mbtools/cx_exception=>raise( 'Error getting system host (SAPDBHOST)' ) ##NO_TEXT.
    ENDIF.

    REPLACE c_param_url WITH lv_system_host INTO rv_endpoint.

    CALL FUNCTION 'SLIC_GET_SYSTEM_ID'
      IMPORTING
        systemid = lv_system_id.

    IF lv_system_id = 'INITIAL_SYSTEM_IDX' OR lv_system_id NA '0123456789'.
      /mbtools/cx_exception=>raise( 'Initial system number (transaction SLICENSE)' ) ##NO_TEXT.
    ENDIF.

    REPLACE c_param_system WITH lv_system_id INTO rv_endpoint.
    CONDENSE rv_endpoint NO-GAPS.

  ENDMETHOD.


  METHOD get_version.

    DATA:
      lv_endpoint TYPE string.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    CALL METHOD get_endpoint
      EXPORTING
        iv_action   = c_action_version
        iv_id       = iv_id
        iv_license  = iv_license
      RECEIVING
        rv_endpoint = lv_endpoint.

  ENDMETHOD.
ENDCLASS.
