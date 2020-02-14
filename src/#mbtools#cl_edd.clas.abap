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

  constants CO_EDD_HOST type STRING value 'https://marcbernardtools.com/' ##NO_TEXT.
  constants CO_ACTION_ACTIVATE type STRING value 'activate_license' ##NO_TEXT.
  constants CO_ACTION_DEACTIVATE type STRING value 'deactivate_license' ##NO_TEXT.
  constants CO_ACTION_CHECK type STRING value 'check_license' ##NO_TEXT.
  constants CO_ACTION_VERSION type STRING value 'get_version' ##NO_TEXT.
  constants CO_PARAM_ACTION type STRING value '$action$' ##NO_TEXT.
  constants CO_PARAM_ID type STRING value '$id$' ##NO_TEXT.
  constants CO_PARAM_KEY type STRING value '$key$' ##NO_TEXT.
  constants CO_PARAM_URL type STRING value '$url$' ##NO_TEXT.
  constants CO_PARAM_SYSTEM type STRING value '$system$' ##NO_TEXT.

  class-methods ACTIVATE_LICENSE
    importing
      !I_ID type STRING
      !I_LICENSE type STRING
    exporting
      !E_VALID type ABAP_BOOL
      !E_EXPIRE type D
    raising
      /MBTOOLS/CX_EXCEPTION .
  class-methods DEACTIVATE_LICENSE
    importing
      !I_ID type STRING
      !I_LICENSE type STRING
    exporting
      !E_VALID type ABAP_BOOL
      !E_EXPIRE type D
    raising
      /MBTOOLS/CX_EXCEPTION .
  class-methods CHECK_LICENSE
    importing
      !I_ID type STRING
      !I_LICENSE type STRING
    exporting
      !E_VALID type ABAP_BOOL
      !E_EXPIRE type D
    raising
      /MBTOOLS/CX_EXCEPTION .
  class-methods GET_VERSION
    importing
      !I_ID type STRING
      !I_LICENSE type STRING
    exporting
      !E_VALID type ABAP_BOOL
      !E_EXPIRE type D
    raising
      /MBTOOLS/CX_EXCEPTION .
  PROTECTED SECTION.
private section.

  class-methods GET_ENDPOINT
    importing
      !I_ACTION type STRING
      !I_ID type STRING
      !I_LICENSE type STRING
    returning
      value(R_ENDPOINT) type STRING
    raising
      /MBTOOLS/CX_EXCEPTION .
ENDCLASS.



CLASS /MBTOOLS/CL_EDD IMPLEMENTATION.


  METHOD activate_license.

    DATA:
      endpoint TYPE string.

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

    CALL METHOD get_endpoint
      EXPORTING
        i_action   = co_action_deactivate
        i_id       = i_id
        i_license  = i_license
      RECEIVING
        r_endpoint = endpoint.

  ENDMETHOD.


  METHOD GET_ENDPOINT.

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

  ENDMETHOD.


  METHOD get_version.

    DATA:
      endpoint TYPE string.

    CALL METHOD get_endpoint
      EXPORTING
        i_action   = co_action_version
        i_id       = i_id
        i_license  = i_license
      RECEIVING
        r_endpoint = endpoint.

  ENDMETHOD.
ENDCLASS.
