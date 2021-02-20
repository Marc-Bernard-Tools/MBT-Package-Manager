CLASS /mbtools/cl_setup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

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
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_sslc TYPE psecontext VALUE 'SSLC' ##NO_TEXT.
    CONSTANTS c_anonym TYPE ssfappl VALUE 'ANONYM' ##NO_TEXT.
    CONSTANTS c_id TYPE ssfid VALUE 'CN=%SID SSL client SSL Client (Standard), OU=%ORG, O=MBT, C=CA' ##NO_TEXT.
    CONSTANTS c_org TYPE string VALUE /mbtools/if_definitions=>c_mbt ##NO_TEXT.
    CONSTANTS c_subject TYPE string VALUE 'CN=marcbernardtools.com' ##NO_TEXT.
    CLASS-DATA go_settings TYPE REF TO /mbtools/cl_registry.
    CLASS-DATA gv_force TYPE abap_bool.
    CLASS-DATA gv_drop TYPE abap_bool.

    CLASS-METHODS _application_log.
    CLASS-METHODS _certificates.
    CLASS-METHODS _get_certificate_ca
      RETURNING
        VALUE(rt_result) TYPE /mbtools/cl_strust=>ty_certificate.
    CLASS-METHODS _get_certificate_ica
      RETURNING
        VALUE(rt_result) TYPE /mbtools/cl_strust=>ty_certificate.
    CLASS-METHODS _get_certificate_mbt
      RETURNING
        VALUE(rt_result) TYPE /mbtools/cl_strust=>ty_certificate.
    CLASS-METHODS _rfc_destinations
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS _get_rfc_destination
      RETURNING
        VALUE(rv_result) TYPE rfcdest
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS _get_ssl_client
      RETURNING
        VALUE(rv_result) TYPE ssfapplssl
      RAISING
        /mbtools/cx_exception.
ENDCLASS.



CLASS /mbtools/cl_setup IMPLEMENTATION.


  METHOD class_constructor.

    go_settings = /mbtools/cl_tools=>factory( )->get_settings( ).

  ENDMETHOD.


  METHOD install.

    gv_force = iv_force.
    gv_drop  = abap_false.

    _application_log( ).

    IF /mbtools/cl_mbt=>is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    _certificates( ).

    _rfc_destinations( ).

  ENDMETHOD.


  METHOD uninstall.

    gv_force = abap_true.
    gv_drop  = abap_true.

    _application_log( ).

    _certificates( ).

    _rfc_destinations( ).

  ENDMETHOD.


  METHOD _application_log.

    DATA:
      ls_balobj  TYPE balobj,
      ls_balobjt TYPE balobjt,
      ls_balsub  TYPE balsub,
      lt_balsub  TYPE STANDARD TABLE OF balsub WITH DEFAULT KEY,
      ls_balsubt TYPE balsubt,
      lt_balsubt TYPE STANDARD TABLE OF balsubt WITH DEFAULT KEY.

    SELECT SINGLE * FROM balobj INTO ls_balobj WHERE object = /mbtools/if_definitions=>c_namespace.
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
        lv_applic = _get_ssl_client( ).

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
          lo_strust->add( _get_certificate_ca( ) ).
          lo_strust->add( _get_certificate_ica( ) ).
          lo_strust->add( _get_certificate_mbt( ) ).
          lo_strust->update( ).
        ENDIF.

      CATCH /mbtools/cx_exception INTO lx_error.
        MESSAGE lx_error TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD _get_certificate_ca.

    " subject=C = US, O = DigiCert Inc, OU = www.digicert.com, CN = DigiCert Global Root CA
    " issuer=C = US, O = DigiCert Inc, OU = www.digicert.com, CN = DigiCert Global Root CA
    " notBefore=Nov 10 00:00:00 2006 GMT
    " notAfter=Nov 10 00:00:00 2031 GMT

    APPEND '-----BEGIN CERTIFICATE-----' TO rt_result.
    APPEND 'MIIDrzCCApegAwIBAgIQCDvgVpBCRrGhdWrJWZHHSjANBgkqhkiG9w0BAQUFADBh' TO rt_result.
    APPEND 'MQswCQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3' TO rt_result.
    APPEND 'd3cuZGlnaWNlcnQuY29tMSAwHgYDVQQDExdEaWdpQ2VydCBHbG9iYWwgUm9vdCBD' TO rt_result.
    APPEND 'QTAeFw0wNjExMTAwMDAwMDBaFw0zMTExMTAwMDAwMDBaMGExCzAJBgNVBAYTAlVT' TO rt_result.
    APPEND 'MRUwEwYDVQQKEwxEaWdpQ2VydCBJbmMxGTAXBgNVBAsTEHd3dy5kaWdpY2VydC5j' TO rt_result.
    APPEND 'b20xIDAeBgNVBAMTF0RpZ2lDZXJ0IEdsb2JhbCBSb290IENBMIIBIjANBgkqhkiG' TO rt_result.
    APPEND '9w0BAQEFAAOCAQ8AMIIBCgKCAQEA4jvhEXLeqKTTo1eqUKKPC3eQyaKl7hLOllsB' TO rt_result.
    APPEND 'CSDMAZOnTjC3U/dDxGkAV53ijSLdhwZAAIEJzs4bg7/fzTtxRuLWZscFs3YnFo97' TO rt_result.
    APPEND 'nh6Vfe63SKMI2tavegw5BmV/Sl0fvBf4q77uKNd0f3p4mVmFaG5cIzJLv07A6Fpt' TO rt_result.
    APPEND '43C/dxC//AH2hdmoRBBYMql1GNXRor5H4idq9Joz+EkIYIvUX7Q6hL+hqkpMfT7P' TO rt_result.
    APPEND 'T19sdl6gSzeRntwi5m3OFBqOasv+zbMUZBfHWymeMr/y7vrTC0LUq7dBMtoM1O/4' TO rt_result.
    APPEND 'gdW7jVg/tRvoSSiicNoxBN33shbyTApOB6jtSj1etX+jkMOvJwIDAQABo2MwYTAO' TO rt_result.
    APPEND 'BgNVHQ8BAf8EBAMCAYYwDwYDVR0TAQH/BAUwAwEB/zAdBgNVHQ4EFgQUA95QNVbR' TO rt_result.
    APPEND 'TLtm8KPiGxvDl7I90VUwHwYDVR0jBBgwFoAUA95QNVbRTLtm8KPiGxvDl7I90VUw' TO rt_result.
    APPEND 'DQYJKoZIhvcNAQEFBQADggEBAMucN6pIExIK+t1EnE9SsPTfrgT1eXkIoyQY/Esr' TO rt_result.
    APPEND 'hMAtudXH/vTBH1jLuG2cenTnmCmrEbXjcKChzUyImZOMkXDiqw8cvpOp/2PV5Adg' TO rt_result.
    APPEND '06O/nVsJ8dWO41P0jmP6P6fbtGbfYmbW0W5BjfIttep3Sp+dWOIrWcBAI+0tKIJF' TO rt_result.
    APPEND 'PnlUkiaY4IBIqDfv8NZ5YBberOgOzW6sRBc4L0na4UU+Krk2U886UAb3LujEV0ls' TO rt_result.
    APPEND 'YSEY1QSteDwsOoBrp+uvFRTp2InBuThs4pFsiv9kuXclVzDAGySj4dzp30d8tbQk' TO rt_result.
    APPEND 'CAUw7C29C79Fv1C5qfPrmAESrciIxpg0X40KPMbp1ZWVbd4=' TO rt_result.
    APPEND '-----END CERTIFICATE-----' TO rt_result.

  ENDMETHOD.


  METHOD _get_certificate_ica.

    " subject=C = US, O = DigiCert Inc, OU = www.digicert.com, CN = Encryption Everywhere DV TLS CA - G1
    " issuer=C = US, O = DigiCert Inc, OU = www.digicert.com, CN = DigiCert Global Root CA
    " notBefore=Nov 27 12:46:10 2017 GMT
    " notAfter=Nov 27 12:46:10 2027 GMT

    APPEND '-----BEGIN CERTIFICATE-----' TO rt_result.
    APPEND 'MIIEqjCCA5KgAwIBAgIQAnmsRYvBskWr+YBTzSybsTANBgkqhkiG9w0BAQsFADBh' TO rt_result.
    APPEND 'MQswCQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3' TO rt_result.
    APPEND 'd3cuZGlnaWNlcnQuY29tMSAwHgYDVQQDExdEaWdpQ2VydCBHbG9iYWwgUm9vdCBD' TO rt_result.
    APPEND 'QTAeFw0xNzExMjcxMjQ2MTBaFw0yNzExMjcxMjQ2MTBaMG4xCzAJBgNVBAYTAlVT' TO rt_result.
    APPEND 'MRUwEwYDVQQKEwxEaWdpQ2VydCBJbmMxGTAXBgNVBAsTEHd3dy5kaWdpY2VydC5j' TO rt_result.
    APPEND 'b20xLTArBgNVBAMTJEVuY3J5cHRpb24gRXZlcnl3aGVyZSBEViBUTFMgQ0EgLSBH' TO rt_result.
    APPEND 'MTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBALPeP6wkab41dyQh6mKc' TO rt_result.
    APPEND 'oHqt3jRIxW5MDvf9QyiOR7VfFwK656es0UFiIb74N9pRntzF1UgYzDGu3ppZVMdo' TO rt_result.
    APPEND 'lbxhm6dWS9OK/lFehKNT0OYI9aqk6F+U7cA6jxSC+iDBPXwdF4rs3KRyp3aQn6pj' TO rt_result.
    APPEND 'pp1yr7IB6Y4zv72Ee/PlZ/6rK6InC6WpK0nPVOYR7n9iDuPe1E4IxUMBH/T33+3h' TO rt_result.
    APPEND 'yuH3dvfgiWUOUkjdpMbyxX+XNle5uEIiyBsi4IvbcTCh8ruifCIi5mDXkZrnMT8n' TO rt_result.
    APPEND 'wfYCV6v6kDdXkbgGRLKsR4pucbJtbKqIkUGxuZI2t7pfewKRc5nWecvDBZf3+p1M' TO rt_result.
    APPEND 'pA8CAwEAAaOCAU8wggFLMB0GA1UdDgQWBBRVdE+yck/1YLpQ0dfmUVyaAYca1zAf' TO rt_result.
    APPEND 'BgNVHSMEGDAWgBQD3lA1VtFMu2bwo+IbG8OXsj3RVTAOBgNVHQ8BAf8EBAMCAYYw' TO rt_result.
    APPEND 'HQYDVR0lBBYwFAYIKwYBBQUHAwEGCCsGAQUFBwMCMBIGA1UdEwEB/wQIMAYBAf8C' TO rt_result.
    APPEND 'AQAwNAYIKwYBBQUHAQEEKDAmMCQGCCsGAQUFBzABhhhodHRwOi8vb2NzcC5kaWdp' TO rt_result.
    APPEND 'Y2VydC5jb20wQgYDVR0fBDswOTA3oDWgM4YxaHR0cDovL2NybDMuZGlnaWNlcnQu' TO rt_result.
    APPEND 'Y29tL0RpZ2lDZXJ0R2xvYmFsUm9vdENBLmNybDBMBgNVHSAERTBDMDcGCWCGSAGG' TO rt_result.
    APPEND '/WwBAjAqMCgGCCsGAQUFBwIBFhxodHRwczovL3d3dy5kaWdpY2VydC5jb20vQ1BT' TO rt_result.
    APPEND 'MAgGBmeBDAECATANBgkqhkiG9w0BAQsFAAOCAQEAK3Gp6/aGq7aBZsxf/oQ+TD/B' TO rt_result.
    APPEND 'SwW3AU4ETK+GQf2kFzYZkby5SFrHdPomunx2HBzViUchGoofGgg7gHW0W3MlQAXW' TO rt_result.
    APPEND 'M0r5LUvStcr82QDWYNPaUy4taCQmyaJ+VB+6wxHstSigOlSNF2a6vg4rgexixeiV' TO rt_result.
    APPEND '4YSB03Yqp2t3TeZHM9ESfkus74nQyW7pRGezj+TC44xCagCQQOzzNmzEAP2SnCrJ' TO rt_result.
    APPEND 'sNE2DpRVMnL8J6xBRdjmOsC3N6cQuKuRXbzByVBjCqAA8t1L0I+9wXJerLPyErjy' TO rt_result.
    APPEND 'rMKWaBFLmfK/AHNF4ZihwPGOc7w6UHczBZXH5RFzJNnww+WnKuTPI0HfnVH8lg==' TO rt_result.
    APPEND '-----END CERTIFICATE-----' TO rt_result.

  ENDMETHOD.


  METHOD _get_certificate_mbt.

    " subject=CN = marcbernardtools.com
    " issuer=C = US, O = DigiCert Inc, OU = www.digicert.com, CN = Encryption Everywhere DV TLS CA - G1
    " notBefore=Feb  5 00:00:00 2021 GMT
    " notAfter=Feb  4 23:59:59 2022 GMT

    APPEND '-----BEGIN CERTIFICATE-----' TO rt_result.
    APPEND 'MIIFnDCCBISgAwIBAgIQBZeXovPOlrfpuWF5fi887jANBgkqhkiG9w0BAQsFADBu' TO rt_result.
    APPEND 'MQswCQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3' TO rt_result.
    APPEND 'd3cuZGlnaWNlcnQuY29tMS0wKwYDVQQDEyRFbmNyeXB0aW9uIEV2ZXJ5d2hlcmUg' TO rt_result.
    APPEND 'RFYgVExTIENBIC0gRzEwHhcNMjEwMjA1MDAwMDAwWhcNMjIwMjA0MjM1OTU5WjAf' TO rt_result.
    APPEND 'MR0wGwYDVQQDExRtYXJjYmVybmFyZHRvb2xzLmNvbTCCASIwDQYJKoZIhvcNAQEB' TO rt_result.
    APPEND 'BQADggEPADCCAQoCggEBAOneZOHKp7ROZYou0wXTectzYkZ9D5eopYGvx8F7xqk4' TO rt_result.
    APPEND 'jBqhoEoDxMYvsxllq5ObXGn+Akq+v7W3PNdjq3PCw4lPjXhRRDP9m0cYuGB54ea7' TO rt_result.
    APPEND 'Dgk7R+2vV0xL+6mfdphmW9qs1g5ArYfxIjsYD0iJqXhmwItHMQAvXe4NRqxUfQUC' TO rt_result.
    APPEND 'qAIViLSNGNY7ch1xRU5wkPiOP2CGJN0vLDIWYOWWKWgi6QNq7hkVwRncb3fdr+tH' TO rt_result.
    APPEND 'KO+Vn7uh6ZnUp5/OzLnVkb/c4Tusi4imA2Mwoyy5AIWpcQnuEBVHTPq12uMmwcIW' TO rt_result.
    APPEND 'twYxh7wF18Ep67Q2tAXBvf2BbXrLZL8L4ZpjpKIYJgcCAwEAAaOCAoMwggJ/MB8G' TO rt_result.
    APPEND 'A1UdIwQYMBaAFFV0T7JyT/VgulDR1+ZRXJoBhxrXMB0GA1UdDgQWBBTsmfSLbqqf' TO rt_result.
    APPEND 'T76P0bO68KitWLsgUDA5BgNVHREEMjAwghRtYXJjYmVybmFyZHRvb2xzLmNvbYIY' TO rt_result.
    APPEND 'd3d3Lm1hcmNiZXJuYXJkdG9vbHMuY29tMA4GA1UdDwEB/wQEAwIFoDAdBgNVHSUE' TO rt_result.
    APPEND 'FjAUBggrBgEFBQcDAQYIKwYBBQUHAwIwPgYDVR0gBDcwNTAzBgZngQwBAgEwKTAn' TO rt_result.
    APPEND 'BggrBgEFBQcCARYbaHR0cDovL3d3dy5kaWdpY2VydC5jb20vQ1BTMIGABggrBgEF' TO rt_result.
    APPEND 'BQcBAQR0MHIwJAYIKwYBBQUHMAGGGGh0dHA6Ly9vY3NwLmRpZ2ljZXJ0LmNvbTBK' TO rt_result.
    APPEND 'BggrBgEFBQcwAoY+aHR0cDovL2NhY2VydHMuZGlnaWNlcnQuY29tL0VuY3J5cHRp' TO rt_result.
    APPEND 'b25FdmVyeXdoZXJlRFZUTFNDQS1HMS5jcnQwCQYDVR0TBAIwADCCAQMGCisGAQQB' TO rt_result.
    APPEND '1nkCBAIEgfQEgfEA7wB2ACl5vvCeOTkh8FZzn2Old+W+V32cYAr4+U1dJlwlXceE' TO rt_result.
    APPEND 'AAABd3RHCWoAAAQDAEcwRQIhAKF8xFk+Wt3/f76IZC1SmksREmVs5tBEH5Gq5Ovq' TO rt_result.
    APPEND '/28XAiB8BuB/PuahN7JzkcRrl8DhLUWl0NAvqZVqw5hQn9cT+wB1ACJFRQdZVSRW' TO rt_result.
    APPEND 'lj+hL/H3bYbgIyZjrcBLf13Gg1xu4g8CAAABd3RHCZ8AAAQDAEYwRAIgOVfNC0Gy' TO rt_result.
    APPEND 'TalwfvXU5pmMvEwx6YqDxVnLqxxU1cmF1/ICIBD3U+u/4ftr+CWy7XBTN0BpkJ8/' TO rt_result.
    APPEND '5dt+BZT2Vz4j9C9eMA0GCSqGSIb3DQEBCwUAA4IBAQAV+Dna6jC6uWaYgFLwqO9t' TO rt_result.
    APPEND '2KrV/vs7+Um+9ZdQnTmadrAnibZk4bRWBgwrURnvYgF91OhcJWMTgZRCciRm6ekJ' TO rt_result.
    APPEND 'EMLj94j5a8w34J7IDxEZmcVlxwr5O6KyG8oIA6BJZWM93GQipu+yro+QxzWOuA6K' TO rt_result.
    APPEND 'dTyOI0wkyFIDyGJiu7m15elbdTDtC8bLQN/nsWSO/8yG618Zh7yqPLCHVLV9WPOT' TO rt_result.
    APPEND 'oLgOlYpeyQ3EElFMBdQAO7phV4moFScjH+5xQTfSJJg1xVnF+mq/H1a7E3ASKw13' TO rt_result.
    APPEND 'HSbqF6XUB5+k2U2qznGJd3fvSS0sTDjuFAcwOb8Xjcy+LQefzgMRSWJxdLRkQ8dt' TO rt_result.
    APPEND '-----END CERTIFICATE-----' TO rt_result.

  ENDMETHOD.


  METHOD _get_rfc_destination.

    IF go_settings IS BOUND.
      rv_result = go_settings->get_value( /mbtools/cl_tools=>c_reg-key_rfcdest ).
    ENDIF.

    IF rv_result IS INITIAL.
      rv_result = /mbtools/if_definitions=>c_rfcdest.
    ENDIF.

  ENDMETHOD.


  METHOD _get_ssl_client.

    IF go_settings IS BOUND.
      rv_result = go_settings->get_value( /mbtools/cl_tools=>c_reg-key_ssl_client ).
    ENDIF.

    IF rv_result IS INITIAL.
      rv_result = c_anonym.
    ENDIF.

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
      lv_server       TYPE rfchost_ext,
      lv_path         TYPE string.

    TRY.
        lv_applic = _get_ssl_client( ).
        lv_rfcdest = _get_rfc_destination( ).

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
      lv_path        = '/' && /mbtools/if_definitions=>c_www_ping.

      " Create HTTPS Destination to External Server (Type "G")
      CALL FUNCTION 'RFC_MODIFY_HTTP_DEST_TO_EXT'
        EXPORTING
          destination                = lv_rfcdest
          action                     = lv_action
          authority_check            = abap_true
          servicenr                  = '443'
          server                     = lv_server
          path_prefix                = lv_path
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
