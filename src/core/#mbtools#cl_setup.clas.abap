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

    CLASS-METHODS _settings.

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


  METHOD get_rfc_destination.

    _settings( ).

    IF go_settings IS BOUND.
      rv_result = go_settings->get_value( /mbtools/cl_tool=>c_reg-key_rfcdest ).
    ENDIF.

    IF rv_result IS INITIAL.
      rv_result = /mbtools/if_definitions=>c_rfcdest.
    ENDIF.

  ENDMETHOD.


  METHOD get_ssl_client.

    _settings( ).

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

    _settings( ).

    IF /mbtools/cl_mbt=>is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    _rfc_destinations( ).

    _certificates( ).

  ENDMETHOD.


  METHOD uninstall.

    gv_force = abap_true.
    gv_drop  = abap_true.

    _settings( ).

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

    CALL FUNCTION 'DB_COMMIT'.

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
    " issuer=CN=Sectigo RSA Domain Validation Secure Server CA, O=Sectigo Limited,
    "        L=Salford, SP=Greater Manchester, C=GB
    " notBefore=Nov 14 00:00:00 2024 GMT
    " notAfter=Nov 29 23:59:59 2025 GMT

    APPEND '-----BEGIN CERTIFICATE-----' TO rt_result.
    APPEND 'MIIGSjCCBTKgAwIBAgIQWo+DT/WooZSe/KVFZRpj6zANBgkqhkiG9w0BAQsFADCB' TO rt_result.
    APPEND 'jzELMAkGA1UEBhMCR0IxGzAZBgNVBAgTEkdyZWF0ZXIgTWFuY2hlc3RlcjEQMA4G' TO rt_result.
    APPEND 'A1UEBxMHU2FsZm9yZDEYMBYGA1UEChMPU2VjdGlnbyBMaW1pdGVkMTcwNQYDVQQD' TO rt_result.
    APPEND 'Ey5TZWN0aWdvIFJTQSBEb21haW4gVmFsaWRhdGlvbiBTZWN1cmUgU2VydmVyIENB' TO rt_result.
    APPEND 'MB4XDTI0MTExNDAwMDAwMFoXDTI1MTEyOTIzNTk1OVowITEfMB0GA1UEAwwWKi5t' TO rt_result.
    APPEND 'YXJjYmVybmFyZHRvb2xzLmNvbTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoC' TO rt_result.
    APPEND 'ggEBALUVPPPxjo2a/QY4DqbD/gA9yRlwUxW22HQmzEaeNGVZ+o53frGgQ+mnk05n' TO rt_result.
    APPEND 's0GM8jlHFhmjn3jxR06ELLQRF3SPA5UdRCgzJ2A6Ru7zK44TeTmSop/ZrqgMBVe6' TO rt_result.
    APPEND '+8wGicdlvEV0LJhHoRkBUUSLq7aFaVK4hRnnK1uYrQi8JaTWYZBD89gq276s+t89' TO rt_result.
    APPEND 'jBjY0h/SLmLDllgAlq4OdoDn1pUJyjhWLOFf9kk2chOUUnxGr035e3mligQ9iKr9' TO rt_result.
    APPEND '9NzSSWtHbcfpIloJupgk3rnhS2pyKwZDR6Xbj04xEAi7Ksw+vOoMM/iJUwJiu6SO' TO rt_result.
    APPEND 'iVsLHQ50WI35ys+DpG9yYPHt5K8CAwEAAaOCAw0wggMJMB8GA1UdIwQYMBaAFI2M' TO rt_result.
    APPEND 'XsRUrYrhd+mb+ZsF4bgBjWHhMB0GA1UdDgQWBBT7PeILH3G4K3vMGFc/XMPkcPZY' TO rt_result.
    APPEND 'eTAOBgNVHQ8BAf8EBAMCBaAwDAYDVR0TAQH/BAIwADAdBgNVHSUEFjAUBggrBgEF' TO rt_result.
    APPEND 'BQcDAQYIKwYBBQUHAwIwSQYDVR0gBEIwQDA0BgsrBgEEAbIxAQICBzAlMCMGCCsG' TO rt_result.
    APPEND 'AQUFBwIBFhdodHRwczovL3NlY3RpZ28uY29tL0NQUzAIBgZngQwBAgEwgYQGCCsG' TO rt_result.
    APPEND 'AQUFBwEBBHgwdjBPBggrBgEFBQcwAoZDaHR0cDovL2NydC5zZWN0aWdvLmNvbS9T' TO rt_result.
    APPEND 'ZWN0aWdvUlNBRG9tYWluVmFsaWRhdGlvblNlY3VyZVNlcnZlckNBLmNydDAjBggr' TO rt_result.
    APPEND 'BgEFBQcwAYYXaHR0cDovL29jc3Auc2VjdGlnby5jb20wNwYDVR0RBDAwLoIWKi5t' TO rt_result.
    APPEND 'YXJjYmVybmFyZHRvb2xzLmNvbYIUbWFyY2Jlcm5hcmR0b29scy5jb20wggF9Bgor' TO rt_result.
    APPEND 'BgEEAdZ5AgQCBIIBbQSCAWkBZwB1AN3cyjSV1+EWBeeVMvrHn/g9HFDf2wA6FBJ2' TO rt_result.
    APPEND 'Ciysu8gqAAABkyktxosAAAQDAEYwRAIgMyD0emvh61HUuIWnjw/t9R9i5ruZc9JM' TO rt_result.
    APPEND 'g9lHSqUwqjkCIHnOa78Nr2jEqCzQudwx5qXg82pGZzfL2mbWjOGDE6IjAHYAzPsP' TO rt_result.
    APPEND 'aoVxCWX+lZtTzumyfCLphVwNl422qX5UwP5MDbAAAAGTKS3GbgAABAMARzBFAiEA' TO rt_result.
    APPEND '25dZpkbBl69b0RXZJemMgsLM579fMbevO1gHqgKiWYECIB4FmksQ7F+5uBtNrA/6' TO rt_result.
    APPEND 'JBnTE6TeMPUK1aH8shFNxnktAHYAEvFONL1TckyEBhnDjz96E/jntWKHiJxtMAWE' TO rt_result.
    APPEND '6+WGJjoAAAGTKS3GTQAABAMARzBFAiEA/crOBHguPHgWEOkLz6fw/tcQhMgNa0ui' TO rt_result.
    APPEND 'ED949f9n3PsCIGmpofvmlsJ+jrYEbb+hLJF7u77WOvwHC2B7izCbpkLwMA0GCSqG' TO rt_result.
    APPEND 'SIb3DQEBCwUAA4IBAQAjusRcmsyWJYvvIMV322ztBMdcSe1ncovjX8n3H/Jkc0hG' TO rt_result.
    APPEND 'lxiwezWmXqBVLzguQ6jinB4gLyOmSG8/bE3wO56nIPDb3OUdEC4V7eGgnJE1XPJ5' TO rt_result.
    APPEND 'W6o6JGn5eEzTw9M7jRy9GDOLJxJlBe117qQxeoYEKP9ijHH+/GgZd5gwNNOtFYcs' TO rt_result.
    APPEND 'pSL/2Xn8SWSh6dYKDQp6DnSxP6ZpsLy+0UvaPXTKOhUgiVXWsCUZPnKufIkgxKXQ' TO rt_result.
    APPEND 'Di1Zub4ga4Xcv6ANHbAmeVmIUXW4nqPyE1xwdwL4MwrkBv7SJcJYv8xl8VeEWYEO' TO rt_result.
    APPEND 'Df2TseUUPkANsRf5dMqvJ6FgxTZb4czJXlcPCrBM' TO rt_result.
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


  METHOD _settings.

    IF go_settings IS INITIAL.
      go_settings = /mbtools/cl_tool_manager=>factory( )->get_settings( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
