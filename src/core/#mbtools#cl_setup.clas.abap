CLASS /mbtools/cl_setup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Marc Bernard Tools - Setup
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-or-later
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS class_constructor.
    CLASS-METHODS install
      IMPORTING
        !iv_force TYPE abap_bool DEFAULT abap_false
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS uninstall
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS get_rfc_destination
      RETURNING
        VALUE(rv_result) TYPE rfcdest
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS get_ssl_client
      RETURNING
        VALUE(rv_result) TYPE ssfapplssl
      RAISING
        /mbtools/cx_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_sslc TYPE psecontext VALUE 'SSLC' ##NO_TEXT.
    CONSTANTS c_anonym TYPE ssfappl VALUE 'ANONYM' ##NO_TEXT.
    CONSTANTS c_id TYPE ssfid VALUE 'CN=%SID SSL client SSL Client (Standard), OU=%ORG, O=MBT, C=CA' ##NO_TEXT.
    CONSTANTS c_org TYPE string VALUE /mbtools/if_definitions=>c_mbt ##NO_TEXT.
    CONSTANTS c_subject TYPE string VALUE 'CN=*.marcbernardtools.com' ##NO_TEXT.
    CLASS-DATA go_settings TYPE REF TO /mbtools/cl_registry.
    CLASS-DATA gv_force TYPE abap_bool.
    CLASS-DATA gv_drop TYPE abap_bool.

    CLASS-METHODS _application_log.
    CLASS-METHODS _certificates.
    CLASS-METHODS _get_certificate_mbt
      RETURNING
        VALUE(rt_result) TYPE /mbtools/cl_strust=>ty_certificate.
    CLASS-METHODS _rfc_destinations
      RAISING
        /mbtools/cx_exception.
ENDCLASS.



CLASS /mbtools/cl_setup IMPLEMENTATION.


  METHOD class_constructor.

    go_settings = /mbtools/cl_tool_manager=>factory( )->get_settings( ).

  ENDMETHOD.


  METHOD get_rfc_destination.

    IF go_settings IS BOUND.
      rv_result = go_settings->get_value( /mbtools/cl_tool=>c_reg-key_rfcdest ).
    ENDIF.

    IF rv_result IS INITIAL.
      rv_result = /mbtools/if_definitions=>c_rfcdest.
    ENDIF.

  ENDMETHOD.


  METHOD get_ssl_client.

    IF go_settings IS BOUND.
      rv_result = go_settings->get_value( /mbtools/cl_tool=>c_reg-key_ssl_client ).
    ENDIF.

    IF rv_result IS INITIAL.
      rv_result = c_anonym.
    ENDIF.

  ENDMETHOD.


  METHOD install.

    gv_force = iv_force.
    gv_drop  = abap_false.

    _application_log( ).

    IF /mbtools/cl_mbt=>is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    _rfc_destinations( ).

    _certificates( ).

  ENDMETHOD.


  METHOD uninstall.

    gv_force = abap_true.
    gv_drop  = abap_true.

    _application_log( ).

    _rfc_destinations( ).

    _certificates( ).

  ENDMETHOD.


  METHOD _application_log.

    DATA:
      ls_balobj  TYPE balobj,
      ls_balobjt TYPE balobjt,
      ls_balsub  TYPE balsub,
      lt_balsub  TYPE STANDARD TABLE OF balsub WITH DEFAULT KEY,
      ls_balsubt TYPE balsubt,
      lt_balsubt TYPE STANDARD TABLE OF balsubt WITH DEFAULT KEY.

    SELECT SINGLE object FROM balobj INTO ls_balobj WHERE object = /mbtools/if_definitions=>c_namespace.
    IF sy-subrc = 0.
      IF gv_force = abap_true OR gv_drop = abap_true.
        DELETE FROM balobj  WHERE object = /mbtools/if_definitions=>c_namespace.
        ASSERT sy-subrc >= 0.
        DELETE FROM balobjt WHERE object = /mbtools/if_definitions=>c_namespace.
        ASSERT sy-subrc >= 0.
        DELETE FROM balsub  WHERE object = /mbtools/if_definitions=>c_namespace.
        ASSERT sy-subrc >= 0.
        DELETE FROM balsubt WHERE object = /mbtools/if_definitions=>c_namespace.
        ASSERT sy-subrc >= 0.
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    IF gv_drop = abap_true.
      RETURN.
    ENDIF.

    ls_balobj-object  = /mbtools/if_definitions=>c_namespace.
    INSERT balobj FROM ls_balobj.
    ASSERT sy-subrc >= 0.

    ls_balobjt-spras  = sy-langu.
    ls_balobjt-object = /mbtools/if_definitions=>c_namespace.
    ls_balobjt-objtxt = /mbtools/if_definitions=>c_mbt.
    INSERT balobjt FROM ls_balobjt.
    ASSERT sy-subrc >= 0.

    ls_balsub-object    = /mbtools/if_definitions=>c_namespace.
    ls_balsub-subobject = 'EDD'.
    APPEND ls_balsub TO lt_balsub.
    ls_balsub-subobject = 'INIT'.
    APPEND ls_balsub TO lt_balsub.
    ls_balsub-subobject = 'LOG'.
    APPEND ls_balsub TO lt_balsub.

    INSERT balsub FROM TABLE lt_balsub.
    ASSERT sy-subrc >= 0.

    ls_balsubt-spras     = sy-langu.
    ls_balsubt-object    = /mbtools/if_definitions=>c_namespace.
    ls_balsubt-subobject = 'EDD'.
    ls_balsubt-subobjtxt = 'Log for EDD API'(002).
    APPEND ls_balsubt TO lt_balsubt.
    ls_balsubt-subobject = 'INST'.
    ls_balsubt-subobjtxt = 'Log for MBT Installer'(003).
    APPEND ls_balsubt TO lt_balsubt.
    ls_balsubt-subobject = 'LOG'.
    ls_balsubt-subobjtxt = 'General Log'(004).
    APPEND ls_balsubt TO lt_balsubt.

    INSERT balsubt FROM TABLE lt_balsubt.
    ASSERT sy-subrc >= 0.

  ENDMETHOD.


  METHOD _certificates.

    DATA:
      lv_applic TYPE ssfappl,
      lo_strust TYPE REF TO /mbtools/cl_strust,
      lx_error  TYPE REF TO /mbtools/cx_exception.

    TRY.
        lv_applic = get_ssl_client( ).

        CREATE OBJECT lo_strust
          EXPORTING
            iv_context = c_sslc
            iv_applic  = lv_applic.

        IF /mbtools/cl_mbt=>is_mbt_system( ) = abap_true.
          lo_strust->load(
            iv_create = abap_true
            iv_id     = c_id
            iv_org    = c_org ).
        ELSE.
          lo_strust->load( iv_create = abap_true ).
        ENDIF.

        lo_strust->get_own_certificate( ).

        lo_strust->get_certificate_list( ).

        IF gv_drop = abap_true.
          lo_strust->remove( c_subject ).
        ELSE.
          " Root and intermediate certificates
          " lo_strust->add( _get_certificate_ca( ) )
          " lo_strust->add( _get_certificate_ica( ) )
          lo_strust->add( _get_certificate_mbt( ) ).
          lo_strust->update( ).
        ENDIF.

      CATCH /mbtools/cx_exception INTO lx_error.
        MESSAGE lx_error TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD _get_certificate_mbt.

    " subject=CN = *.marcbernardtools.com
    " issuer=C = US, O = DigiCert Inc, CN = RapidSSL TLS DV RSA Mixed SHA256 2020 CA-1
    " notBefore=Dec  2 00:00:00 2021 GMT
    " notAfter=Dec  1 23:59:59 2022 GMT

    APPEND '-----BEGIN CERTIFICATE-----' TO rt_result.
    APPEND 'MIIGpTCCBY2gAwIBAgIQBrWAPHKvSN4DbXv5pnydyDANBgkqhkiG9w0BAQsFADBZ' TO rt_result.
    APPEND 'MQswCQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMTMwMQYDVQQDEypS' TO rt_result.
    APPEND 'YXBpZFNTTCBUTFMgRFYgUlNBIE1peGVkIFNIQTI1NiAyMDIwIENBLTEwHhcNMjEx' TO rt_result.
    APPEND 'MjAyMDAwMDAwWhcNMjIxMjAxMjM1OTU5WjAhMR8wHQYDVQQDDBYqLm1hcmNiZXJu' TO rt_result.
    APPEND 'YXJkdG9vbHMuY29tMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAyi0A' TO rt_result.
    APPEND '0pzk/ayqqHw/QCLUX1994tUj2oZd3YbaWXtsuwE0PkgB+KWGs71yZhUK5kFlUZLX' TO rt_result.
    APPEND 'cHNdoBI6SUHMOecvrJVwXSZIGfN8dKPhV4wH3yX/tVllN3WN9HaP9z+WnxNbHPCK' TO rt_result.
    APPEND 'TWgoI2jL8lDlog2yAMWyogiHfAPWRmcJpsxXlgUIL3aUmPIB6x8QNTzieH28whnj' TO rt_result.
    APPEND 'lC1Jp7JtyLjNIyjncHv/pkDXPkYnfK4uxPcg35rmT+V8gWNOFybSzzDQfH5xtI0p' TO rt_result.
    APPEND 'uBWDjeU/GDDn5MG/YEov+rJdbEyFa64tNgXfZfJ/gOhkoybAu6TjLiaPFril9r2p' TO rt_result.
    APPEND 'KstgiwWpjA9MQ0ShQwIDAQABo4IDnzCCA5swHwYDVR0jBBgwFoAUpI3lvnx55HAj' TO rt_result.
    APPEND 'bS4pNK0jWNz1MX8wHQYDVR0OBBYEFGmVRfco54gMFO4hgw7dDU3Krc5kMDcGA1Ud' TO rt_result.
    APPEND 'EQQwMC6CFioubWFyY2Jlcm5hcmR0b29scy5jb22CFG1hcmNiZXJuYXJkdG9vbHMu' TO rt_result.
    APPEND 'Y29tMA4GA1UdDwEB/wQEAwIFoDAdBgNVHSUEFjAUBggrBgEFBQcDAQYIKwYBBQUH' TO rt_result.
    APPEND 'AwIwgZsGA1UdHwSBkzCBkDBGoESgQoZAaHR0cDovL2NybDMuZGlnaWNlcnQuY29t' TO rt_result.
    APPEND 'L1JhcGlkU1NMVExTRFZSU0FNaXhlZFNIQTI1NjIwMjBDQS0xLmNybDBGoESgQoZA' TO rt_result.
    APPEND 'aHR0cDovL2NybDQuZGlnaWNlcnQuY29tL1JhcGlkU1NMVExTRFZSU0FNaXhlZFNI' TO rt_result.
    APPEND 'QTI1NjIwMjBDQS0xLmNybDA+BgNVHSAENzA1MDMGBmeBDAECATApMCcGCCsGAQUF' TO rt_result.
    APPEND 'BwIBFhtodHRwOi8vd3d3LmRpZ2ljZXJ0LmNvbS9DUFMwgYUGCCsGAQUFBwEBBHkw' TO rt_result.
    APPEND 'dzAkBggrBgEFBQcwAYYYaHR0cDovL29jc3AuZGlnaWNlcnQuY29tME8GCCsGAQUF' TO rt_result.
    APPEND 'BzAChkNodHRwOi8vY2FjZXJ0cy5kaWdpY2VydC5jb20vUmFwaWRTU0xUTFNEVlJT' TO rt_result.
    APPEND 'QU1peGVkU0hBMjU2MjAyMENBLTEuY3J0MAkGA1UdEwQCMAAwggF+BgorBgEEAdZ5' TO rt_result.
    APPEND 'AgQCBIIBbgSCAWoBaAB3AEalVet1+pEgMLWiiWn0830RLEF0vv1JuIWr8vxw/m1H' TO rt_result.
    APPEND 'AAABfXwMzXcAAAQDAEgwRgIhALMaIotYG3JL8IoIUMMVDL3C/IWxWf6jbBqW2j1/' TO rt_result.
    APPEND 'dmyDAiEAu/aZVSgFoVHYrq3fCjjt2F6acwdvW8s1VostzvHNmUcAdgBRo7D1/QF5' TO rt_result.
    APPEND 'nFZtuDd4jwykeswbJ8v3nohCmg3+1IsF5QAAAX18DM1dAAAEAwBHMEUCIDPOurlB' TO rt_result.
    APPEND 'sGz1jDH8ybDQZdvVq2PxWpt0DERJ0T6fBgniAiEA5HaEU7HbDi6cAaBupUZt9A2E' TO rt_result.
    APPEND 'VZ+ZyKmsyduqdyr+IsIAdQBByMqx3yJGShDGoToJQodeTjGLGwPr60vHaPCQYpYG' TO rt_result.
    APPEND '9gAAAX18DM1PAAAEAwBGMEQCIEwWZkB0a3z7IxcpWfC1Qyfza2LoKgKwazyfuFKh' TO rt_result.
    APPEND 'CDCgAiA/h2botmYPojXJtvWHNvtBscuaxhfJ6rfv/oRWmcTQGjANBgkqhkiG9w0B' TO rt_result.
    APPEND 'AQsFAAOCAQEAsMQYLfR16m1FR44brzsCEn7iqPo6rCXNe3+OdGFXgorqqYEcB56f' TO rt_result.
    APPEND 'LWf3v4WS4Cv4qe8PqVAORDrQA7vvOY5YUBo/MwNDKhuKgX0FWnPcAk2mXsPlHVD+' TO rt_result.
    APPEND '0Wte5LQ69RrrljS5JMhHx6lxDBoJ2xNCoYKvnaU2pM/96mjFThGTrc5fjV0aCkV6' TO rt_result.
    APPEND 'U8v2a29DkjjkNcwrKSxMas5mthLDj45C9YfKWb7KE2tJyDtpPW+wAHiZOE/D6Cqd' TO rt_result.
    APPEND 'otXalaJVRqVVQwUfVnibX9hP2P9+bT2pe4vK7Em8UVxYRvOUhutBJy+lDKTlJW6R' TO rt_result.
    APPEND 'Oo9q8UScXXu4kI3nvdVWm7s3yGCKufmP4g==' TO rt_result.
    APPEND '-----END CERTIFICATE-----' TO rt_result.

  ENDMETHOD.


  METHOD _rfc_destinations.

    DATA:
      lo_dest_factory TYPE REF TO cl_dest_factory,
      lx_dest_api     TYPE REF TO cx_dest_api,
      lv_rfc_exists   TYPE abap_bool,
      lv_action       TYPE c LENGTH 1,
      lv_applic       TYPE ssfapplssl,
      lv_rfcdest      TYPE rfcdest,
      lv_description  TYPE rfcdoc_d,
      lv_server       TYPE rfchost_ext.

    TRY.
        lv_applic = get_ssl_client( ).
        lv_rfcdest = get_rfc_destination( ).

        CREATE OBJECT lo_dest_factory.
        lv_rfc_exists = lo_dest_factory->exists( lv_rfcdest ).
      CATCH cx_dest_api INTO lx_dest_api.
        /mbtools/cx_exception=>raise( lx_dest_api->get_text( ) ).
    ENDTRY.

    IF lv_rfc_exists = abap_true.
      IF gv_force = abap_true OR gv_drop = abap_true.
        lv_action = 'D'. " delete existing
      ELSE.
        RETURN.
      ENDIF.
    ELSE.
      lv_action = 'I'. " create new https
    ENDIF.

    IF lv_action = 'D'.
      CALL FUNCTION 'RFC_MODIFY_HTTP_DEST_TO_EXT'
        EXPORTING
          destination                = lv_rfcdest
          action                     = lv_action
          authority_check            = 'X'
        EXCEPTIONS
          authority_not_available    = 1
          destination_already_exist  = 2
          destination_not_exist      = 3
          destination_enqueue_reject = 4
          information_failure        = 5
          trfc_entry_invalid         = 6
          internal_failure           = 7
          snc_information_failure    = 8
          snc_internal_failure       = 9
          destination_is_locked      = 10
          invalid_parameter          = 11
          OTHERS                     = 12.
      IF sy-subrc <> 0.
        /mbtools/cx_exception=>raise_t100( ).
      ELSE.
        lv_action = 'I'.
      ENDIF.
    ENDIF.

    IF gv_drop = abap_true.
      RETURN.
    ENDIF.

    IF lv_action = 'I'.
      lv_description = /mbtools/if_definitions=>c_mbt.
      lv_server      = /mbtools/if_definitions=>c_domain.

      " Create HTTPS Destination to External Server (Type "G")
      CALL FUNCTION 'RFC_MODIFY_HTTP_DEST_TO_EXT'
        EXPORTING
          destination                = lv_rfcdest
          action                     = lv_action
          authority_check            = abap_true
          servicenr                  = '443'
          server                     = lv_server
          logon_method               = 'A'
          description                = lv_description
          sslapplic                  = lv_applic
          ssl                        = abap_true
        EXCEPTIONS
          authority_not_available    = 1
          destination_already_exist  = 2
          destination_not_exist      = 3
          destination_enqueue_reject = 4
          information_failure        = 5
          trfc_entry_invalid         = 6
          internal_failure           = 7
          snc_information_failure    = 8
          snc_internal_failure       = 9
          destination_is_locked      = 10
          invalid_parameter          = 11
          OTHERS                     = 12.
      IF sy-subrc <> 0.
        /mbtools/cx_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
