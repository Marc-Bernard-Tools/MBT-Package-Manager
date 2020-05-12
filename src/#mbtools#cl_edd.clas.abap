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
    CONSTANTS co_edd_host TYPE string VALUE 'https://marcbernardtools.com/' ##NO_TEXT.
    CONSTANTS co_action_activate TYPE string VALUE 'activate_license' ##NO_TEXT.
    CONSTANTS co_action_deactivate TYPE string VALUE 'deactivate_license' ##NO_TEXT.
    CONSTANTS co_action_check TYPE string VALUE 'check_license' ##NO_TEXT.
    CONSTANTS co_action_version TYPE string VALUE 'get_version' ##NO_TEXT.
    CONSTANTS co_param_action TYPE string VALUE '$action$' ##NO_TEXT.
    CONSTANTS co_param_id TYPE string VALUE '$id$' ##NO_TEXT.
    CONSTANTS co_param_key TYPE string VALUE '$key$' ##NO_TEXT.
    CONSTANTS co_param_url TYPE string VALUE '$url$' ##NO_TEXT.
    CONSTANTS co_param_system TYPE string VALUE '$system$' ##NO_TEXT.

    CLASS-METHODS activate_license
      IMPORTING
        !i_id      TYPE string
        !i_license TYPE string
      EXPORTING
        !e_valid   TYPE abap_bool
        !e_expire  TYPE d
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS deactivate_license
      IMPORTING
        !i_id      TYPE string
        !i_license TYPE string
      EXPORTING
        !e_valid   TYPE abap_bool
        !e_expire  TYPE d
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS check_license
      IMPORTING
        !i_id      TYPE string
        !i_license TYPE string
      EXPORTING
        !e_valid   TYPE abap_bool
        !e_expire  TYPE d
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS get_version
      IMPORTING
        !i_id      TYPE string
        !i_license TYPE string
      EXPORTING
        !e_valid   TYPE abap_bool
        !e_expire  TYPE d
      RAISING
        /mbtools/cx_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS get_endpoint
      IMPORTING
        !i_action         TYPE string
        !i_id             TYPE string
        !i_license        TYPE string
      RETURNING
        VALUE(r_endpoint) TYPE string
      RAISING
        /mbtools/cx_exception .
ENDCLASS.



CLASS /MBTOOLS/CL_EDD IMPLEMENTATION.


  METHOD activate_license.

    DATA:
      endpoint TYPE string.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    CALL METHOD get_endpoint
      EXPORTING
        i_action   = co_action_activate
        i_id       = i_id
        i_license  = i_license
      RECEIVING
        r_endpoint = endpoint.

  ENDMETHOD.


  METHOD check_license.

    DATA:
      endpoint TYPE string.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    CALL METHOD get_endpoint
      EXPORTING
        i_action   = co_action_check
        i_id       = i_id
        i_license  = i_license
      RECEIVING
        r_endpoint = endpoint.

  ENDMETHOD.


  METHOD deactivate_license.

    DATA:
      endpoint TYPE string.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    CALL METHOD get_endpoint
      EXPORTING
        i_action   = co_action_deactivate
        i_id       = i_id
        i_license  = i_license
      RECEIVING
        r_endpoint = endpoint.

  ENDMETHOD.


  METHOD get_endpoint.

    DATA:
      subrc       TYPE sy-subrc,
      system_host TYPE string,
      system_id   TYPE slic_sysid.

*   http://yoursite.com/?edd_action={request type}&item_id={id}&license={key}&url={url of the site being licensed}/{system number}
    r_endpoint = co_edd_host && '?edd_action=' && co_param_action && '&item_id=' && co_param_id.
    r_endpoint = r_endpoint && '&license=' && co_param_key && '&url=' && co_param_url && '/' && co_param_system.

    REPLACE co_param_action WITH i_action  INTO r_endpoint.
    REPLACE co_param_id     WITH i_id      INTO r_endpoint.
    REPLACE co_param_key    WITH i_license INTO r_endpoint.

    CALL FUNCTION 'SPFL_PARAMETER_GET_VALUE'
      EXPORTING
        name  = 'SAPDBHOST'
      IMPORTING
        value = system_host
        rc    = subrc.
    IF subrc <> 0 OR system_host IS INITIAL.
      /mbtools/cx_exception=>raise( 'Error getting system host (SAPDBHOST)' ) ##NO_TEXT.
    ENDIF.

    REPLACE co_param_url WITH system_host INTO r_endpoint.

    CALL FUNCTION 'SLIC_GET_SYSTEM_ID'
      IMPORTING
        systemid = system_id.

    IF system_id = 'INITIAL_SYSTEM_IDX' OR system_id NA '0123456789'.
      /mbtools/cx_exception=>raise( 'Initial system number (transaction SLICENSE)' ) ##NO_TEXT.
    ENDIF.

    REPLACE co_param_system WITH system_id INTO r_endpoint.
    CONDENSE r_endpoint NO-GAPS.

  ENDMETHOD.


  METHOD get_version.

    DATA:
      endpoint TYPE string.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    CALL METHOD get_endpoint
      EXPORTING
        i_action   = co_action_version
        i_id       = i_id
        i_license  = i_license
      RECEIVING
        r_endpoint = endpoint.

  ENDMETHOD.
ENDCLASS.
