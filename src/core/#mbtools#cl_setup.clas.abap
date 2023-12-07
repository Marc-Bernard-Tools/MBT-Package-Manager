CLASS /mbtools/cl_setup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Marc Bernard Tools - Setup
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
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

    CONSTANTS:
      c_sslc    TYPE psecontext VALUE 'SSLC' ##NO_TEXT,
      c_anonym  TYPE ssfappl VALUE 'ANONYM' ##NO_TEXT,
      c_id      TYPE ssfid VALUE 'CN=%SID SSL client SSL Client (Standard), OU=%ORG, O=MBT, C=CA' ##NO_TEXT,
      c_org     TYPE string VALUE /mbtools/if_definitions=>c_mbt ##NO_TEXT,
      c_subject TYPE string VALUE 'CN=*.marcbernardtools.com' ##NO_TEXT.

    CLASS-DATA:
      go_settings TYPE REF TO /mbtools/cl_registry,
      gv_force    TYPE abap_bool,
      gv_drop     TYPE abap_bool.

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
    ls_balsub-subobject = 'INST'.
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
    " issuer=C = US, O = DigiCert Inc, OU = www.digicert.com, CN = RapidSSL TLS RSA CA G1
    " notBefore=Nov 20 00:00:00 2023 GMT
    " notAfter=Dec  4 23:59:59 2024 GMT

    APPEND '-----BEGIN CERTIFICATE-----' TO rt_result.
    APPEND 'MIIGQDCCBSigAwIBAgIQCNqWSvYNNa9hfOzsk89rUjANBgkqhkiG9w0BAQsFADBg' TO rt_result.
    APPEND 'MQswCQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3' TO rt_result.
    APPEND 'd3cuZGlnaWNlcnQuY29tMR8wHQYDVQQDExZSYXBpZFNTTCBUTFMgUlNBIENBIEcx' TO rt_result.
    APPEND 'MB4XDTIzMTEyMDAwMDAwMFoXDTI0MTIwNDIzNTk1OVowITEfMB0GA1UEAwwWKi5t' TO rt_result.
    APPEND 'YXJjYmVybmFyZHRvb2xzLmNvbTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoC' TO rt_result.
    APPEND 'ggEBAMsjkkR9w3KJ19ef/8ntnqmqJB90ESQjqEOhFxqkXIdjLW6ob1NGstMD6E6v' TO rt_result.
    APPEND 'A4sln07ZrWeep3hdldYImkOJ3Xt6uclsIfK/NLRQLZZBeITmxpJ3ynqZgtfwcRel' TO rt_result.
    APPEND 'xcWr4qPMVVxITx7NTPYW2Itvxx3w7WJySioZS4QRAjNdWJafSXUoIaQIih94gN7X' TO rt_result.
    APPEND 'fUt2cm3jKN9dIxhwdDVDjuvPRSaq2GD25gQw20Z5PvZtIxSpdSWNCEhocEAkkBVO' TO rt_result.
    APPEND '1I7mEa/iA5sRxjW4aosYcyKX4R/vGRDmz3ARM+CTjJAsfPZkL9+J6G0DMj8UuVtk' TO rt_result.
    APPEND 'xxWglgl4J6pDRAeWgu7JtH0OP4UCAwEAAaOCAzMwggMvMB8GA1UdIwQYMBaAFAzb' TO rt_result.
    APPEND 'bIJJD0pnCrgU7nrESFKI61Y4MB0GA1UdDgQWBBT3F2dgDgO3ZdINnLSKbpHhsNnY' TO rt_result.
    APPEND 'kzA3BgNVHREEMDAughYqLm1hcmNiZXJuYXJkdG9vbHMuY29tghRtYXJjYmVybmFy' TO rt_result.
    APPEND 'ZHRvb2xzLmNvbTA+BgNVHSAENzA1MDMGBmeBDAECATApMCcGCCsGAQUFBwIBFhto' TO rt_result.
    APPEND 'dHRwOi8vd3d3LmRpZ2ljZXJ0LmNvbS9DUFMwDgYDVR0PAQH/BAQDAgWgMB0GA1Ud' TO rt_result.
    APPEND 'JQQWMBQGCCsGAQUFBwMBBggrBgEFBQcDAjA/BgNVHR8EODA2MDSgMqAwhi5odHRw' TO rt_result.
    APPEND 'Oi8vY2RwLnJhcGlkc3NsLmNvbS9SYXBpZFNTTFRMU1JTQUNBRzEuY3JsMHYGCCsG' TO rt_result.
    APPEND 'AQUFBwEBBGowaDAmBggrBgEFBQcwAYYaaHR0cDovL3N0YXR1cy5yYXBpZHNzbC5j' TO rt_result.
    APPEND 'b20wPgYIKwYBBQUHMAKGMmh0dHA6Ly9jYWNlcnRzLnJhcGlkc3NsLmNvbS9SYXBp' TO rt_result.
    APPEND 'ZFNTTFRMU1JTQUNBRzEuY3J0MAwGA1UdEwEB/wQCMAAwggF8BgorBgEEAdZ5AgQC' TO rt_result.
    APPEND 'BIIBbASCAWgBZgB1AHb/iD8KtvuVUcJhzPWHujS0pM27KdxoQgqf5mdMWjp0AAAB' TO rt_result.
    APPEND 'i+prpXkAAAQDAEYwRAIgRmuHMGxs5+YbIOBq9lQDEGoTasVb8dZiajVRnHgzBjoC' TO rt_result.
    APPEND 'ICwvhghliCTAc5V/MEu33k43Cs/zeoyBQyM6Zu4YVU9vAHUASLDja9qmRzQP5WoC' TO rt_result.
    APPEND '+p0w6xxSActW3SyB2bu/qznYhHMAAAGL6mulRAAABAMARjBEAiAjL6bIlTEgrrvy' TO rt_result.
    APPEND 'Ozi+CaPvdE3lQf+VNqhH6Fr+QjS0WgIgYqaDEdHupvboESdt3riXcH6bkwhPWjYt' TO rt_result.
    APPEND 'C2Z5Yy63Gx8AdgDatr9rP7W2Ip+bwrtca+hwkXFsu1GEhTS9pD0wSNf7qwAAAYvq' TO rt_result.
    APPEND 'a6UkAAAEAwBHMEUCIQDSsjnYLKz1DHf/tbXgmNCGNzI9l/g9F4d7VxSx2/1RpgIg' TO rt_result.
    APPEND 'R77/B7HsjUfYCsnCRLx9uGkfIu/MGx21PRif/hTe8aAwDQYJKoZIhvcNAQELBQAD' TO rt_result.
    APPEND 'ggEBAIg/nlyxXFEEvW1v6c68HkIB7UOJk3gq9caZXBb96tsQtLeRVDyLi4UbeSdb' TO rt_result.
    APPEND 'l+aQhLUFox/51TyBhLIs89DsKt0/qhvglO66+K/Z9CKZ28QYyOWFiNoZ0jpwNdAn' TO rt_result.
    APPEND '9vm5bPDX+06U5L+Lmrqm0PgUoMHH0VVIPFbTt//o7p1dElEE919WBb3uXsJZ/tSJ' TO rt_result.
    APPEND '5/N6fAV+puUccKGhKS1WqOrO5yTQPicEI72Zg/nXrOPOF3Fji+MD2v0wBBNTmto+' TO rt_result.
    APPEND '+l6JmIj9V0scTjKFM5uN7UW5BanwULy4frsSraTiF9owpHBmQzGIY+XNhPjFU/mI' TO rt_result.
    APPEND 'YRI7fsPjG8ahNuWUP9JMnAGaHTU=' TO rt_result.
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
