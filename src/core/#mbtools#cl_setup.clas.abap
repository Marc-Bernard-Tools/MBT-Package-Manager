CLASS /mbtools/cl_setup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor .
    CLASS-METHODS install
      IMPORTING
        !iv_force TYPE abap_bool DEFAULT abap_false
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS uninstall
      RAISING
        /mbtools/cx_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_ssla TYPE psecontext VALUE 'SSLA' ##NO_TEXT.
    CONSTANTS c_anonym TYPE ssfappl VALUE 'ANONYM' ##NO_TEXT.
    CONSTANTS c_id TYPE ssfid
      VALUE 'CN=%SID SSL client SSL Client (Standard), OU=Marc Bernard Tools, O=MBT, C=CA' ##NO_TEXT.
    CONSTANTS c_subject TYPE string
      VALUE 'CN=www.marcbernardtools.com' ##NO_TEXT.

    CLASS-DATA go_settings TYPE REF TO /mbtools/cl_registry.
    CLASS-DATA gv_force TYPE abap_bool.
    CLASS-DATA gv_drop TYPE abap_bool.

    CLASS-METHODS _application_log.
    CLASS-METHODS _certificates.
    CLASS-METHODS _certificate_ca
      RETURNING
        VALUE(rt_result) TYPE /mbtools/cl_strust=>ty_certificate.
    CLASS-METHODS _certificate_ica
      RETURNING
        VALUE(rt_result) TYPE /mbtools/cl_strust=>ty_certificate.
    CLASS-METHODS _certificate_mbt
      RETURNING
        VALUE(rt_result) TYPE /mbtools/cl_strust=>ty_certificate.
    CLASS-METHODS _rfc_destination
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS _rfc_destination_mbt
      RETURNING
        VALUE(rv_result) TYPE rfcdest
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS _ssl_client
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

    _certificates( ).

    _rfc_destination( ).

  ENDMETHOD.


  METHOD uninstall.

    gv_force = abap_true.
    gv_drop  = abap_true.

    _application_log( ).

    _certificates( ).

    _rfc_destination( ).

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
        lv_applic = _ssl_client( ).

        CREATE OBJECT lo_strust
          EXPORTING
            iv_context = c_ssla
            iv_applic  = lv_applic.

        lo_strust->load(
          iv_create = abap_true
          iv_id     = c_id ).

        lo_strust->get_own_certificate( ).

        lo_strust->get_certificate_list( ).

        IF gv_drop = abap_true.
          lo_strust->remove( c_subject ).
        ELSE.
          lo_strust->add( _certificate_ca( ) ).
          lo_strust->add( _certificate_ica( ) ).
          lo_strust->add( _certificate_mbt( ) ).
          lo_strust->update( ).
        ENDIF.

      CATCH /mbtools/cx_exception INTO lx_error.
        MESSAGE lx_error TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD _certificate_ca.

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


  METHOD _certificate_ica.

    " subject=C = US, O = DigiCert Inc, OU = www.digicert.com, CN = RapidSSL RSA CA 2018
    " issuer=C = US, O = DigiCert Inc, OU = www.digicert.com, CN = DigiCert Global Root CA
    " notBefore=Nov  6 12:23:33 2017 GMT
    " notAfter=Nov  6 12:23:33 2027 GMT

    APPEND '-----BEGIN CERTIFICATE-----' TO rt_result.
    APPEND 'MIIEsTCCA5mgAwIBAgIQCKWiRs1LXIyD1wK0u6tTSTANBgkqhkiG9w0BAQsFADBh' TO rt_result.
    APPEND 'MQswCQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3' TO rt_result.
    APPEND 'd3cuZGlnaWNlcnQuY29tMSAwHgYDVQQDExdEaWdpQ2VydCBHbG9iYWwgUm9vdCBD' TO rt_result.
    APPEND 'QTAeFw0xNzExMDYxMjIzMzNaFw0yNzExMDYxMjIzMzNaMF4xCzAJBgNVBAYTAlVT' TO rt_result.
    APPEND 'MRUwEwYDVQQKEwxEaWdpQ2VydCBJbmMxGTAXBgNVBAsTEHd3dy5kaWdpY2VydC5j' TO rt_result.
    APPEND 'b20xHTAbBgNVBAMTFFJhcGlkU1NMIFJTQSBDQSAyMDE4MIIBIjANBgkqhkiG9w0B' TO rt_result.
    APPEND 'AQEFAAOCAQ8AMIIBCgKCAQEA5S2oihEo9nnpezoziDtx4WWLLCll/e0t1EYemE5n' TO rt_result.
    APPEND '+MgP5viaHLy+VpHP+ndX5D18INIuuAV8wFq26KF5U0WNIZiQp6mLtIWjUeWDPA28' TO rt_result.
    APPEND 'OeyhTlj9TLk2beytbtFU6ypbpWUltmvY5V8ngspC7nFRNCjpfnDED2kRyJzO8yoK' TO rt_result.
    APPEND 'MFz4J4JE8N7NA1uJwUEFMUvHLs0scLoPZkKcewIRm1RV2AxmFQxJkdf7YN9Pckki' TO rt_result.
    APPEND 'f2Xgm3b48BZn0zf0qXsSeGu84ua9gwzjzI7tbTBjayTpT+/XpWuBVv6fvarI6bik' TO rt_result.
    APPEND 'KB859OSGQuw73XXgeuFwEPHTIRoUtkzu3/EQ+LtwznkkdQIDAQABo4IBZjCCAWIw' TO rt_result.
    APPEND 'HQYDVR0OBBYEFFPKF1n8a8ADIS8aruSqqByCVtp1MB8GA1UdIwQYMBaAFAPeUDVW' TO rt_result.
    APPEND '0Uy7ZvCj4hsbw5eyPdFVMA4GA1UdDwEB/wQEAwIBhjAdBgNVHSUEFjAUBggrBgEF' TO rt_result.
    APPEND 'BQcDAQYIKwYBBQUHAwIwEgYDVR0TAQH/BAgwBgEB/wIBADA0BggrBgEFBQcBAQQo' TO rt_result.
    APPEND 'MCYwJAYIKwYBBQUHMAGGGGh0dHA6Ly9vY3NwLmRpZ2ljZXJ0LmNvbTBCBgNVHR8E' TO rt_result.
    APPEND 'OzA5MDegNaAzhjFodHRwOi8vY3JsMy5kaWdpY2VydC5jb20vRGlnaUNlcnRHbG9i' TO rt_result.
    APPEND 'YWxSb290Q0EuY3JsMGMGA1UdIARcMFowNwYJYIZIAYb9bAECMCowKAYIKwYBBQUH' TO rt_result.
    APPEND 'AgEWHGh0dHBzOi8vd3d3LmRpZ2ljZXJ0LmNvbS9DUFMwCwYJYIZIAYb9bAEBMAgG' TO rt_result.
    APPEND 'BmeBDAECATAIBgZngQwBAgIwDQYJKoZIhvcNAQELBQADggEBAH4jx/LKNW5ZklFc' TO rt_result.
    APPEND 'YWs8Ejbm0nyzKeZC2KOVYR7P8gevKyslWm4Xo4BSzKr235FsJ4aFt6yAiv1eY0tZ' TO rt_result.
    APPEND '/ZN18bOGSGStoEc/JE4ocIzr8P5Mg11kRYHbmgYnr1Rxeki5mSeb39DGxTpJD4kG' TO rt_result.
    APPEND 'hs5lXNoo4conUiiJwKaqH7vh2baryd8pMISag83JUqyVGc2tWPpO0329/CWq2kry' TO rt_result.
    APPEND 'qv66OSMjwulUz0dXf4OHQasR7CNfIr+4KScc6ABlQ5RDF86PGeE6kdwSQkFiB/cQ' TO rt_result.
    APPEND 'ysNyq0jEDQTkfa2pjmuWtMCNbBnhFXBYejfubIhaUbEv2FOQB3dCav+FPg5eEveX' TO rt_result.
    APPEND 'TVyMnGo=' TO rt_result.
    APPEND '-----END CERTIFICATE-----' TO rt_result.

  ENDMETHOD.


  METHOD _certificate_mbt.

    " subject=CN = www.marcbernardtools.com
    " issuer=C = US, O = DigiCert Inc, OU = www.digicert.com, CN = RapidSSL RSA CA 2018
    " notBefore=Apr 22 00:00:00 2020 GMT
    " notAfter=May 22 12:00:00 2021 GMT

    APPEND '-----BEGIN CERTIFICATE-----' TO rt_result.
    APPEND 'MIIF1DCCBLygAwIBAgIQATJkw4xH+8FiK57gpyKEcDANBgkqhkiG9w0BAQsFADBe' TO rt_result.
    APPEND 'MQswCQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3' TO rt_result.
    APPEND 'd3cuZGlnaWNlcnQuY29tMR0wGwYDVQQDExRSYXBpZFNTTCBSU0EgQ0EgMjAxODAe' TO rt_result.
    APPEND 'Fw0yMDA0MjIwMDAwMDBaFw0yMTA1MjIxMjAwMDBaMCMxITAfBgNVBAMTGHd3dy5t' TO rt_result.
    APPEND 'YXJjYmVybmFyZHRvb2xzLmNvbTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoC' TO rt_result.
    APPEND 'ggEBALF4nTvL6fKjQVtI220TN+BlsL/6KuNdNSNSKSYtAViHgN26lhvSU5mJjTXQ' TO rt_result.
    APPEND 'Y5PIz9zYz3pjNGusq+9ssrkeIMbVfgmQStDE+O4bMdYeDPYaUUqR641hf4yMuqiD' TO rt_result.
    APPEND 'waB3UKQrYiTRy2VotyXfbmEg5qT2wPqg2AuJQSSTrwQ4l9AEIGXiAxFb7AuZ1rEl' TO rt_result.
    APPEND 'hpa6ca92FLbzgzrbjyDY/VJMtWzOKhyoM6T7nk1Xe6WdcMVfnm0Sg1tKia///KcC' TO rt_result.
    APPEND 'v80frNG6EBRsSUcX6YpUp+7LVDkF0ecTcyA4u6WI8vIMvnYXy6e7j6NoqtoM7U2Z' TO rt_result.
    APPEND 'dsXPT1V1AgftGYyavmD9TUd16tkCAwEAAaOCAscwggLDMB8GA1UdIwQYMBaAFFPK' TO rt_result.
    APPEND 'F1n8a8ADIS8aruSqqByCVtp1MB0GA1UdDgQWBBT9XyncinvLZLnmROKAPZrGa70D' TO rt_result.
    APPEND '7jA5BgNVHREEMjAwghh3d3cubWFyY2Jlcm5hcmR0b29scy5jb22CFG1hcmNiZXJu' TO rt_result.
    APPEND 'YXJkdG9vbHMuY29tMA4GA1UdDwEB/wQEAwIFoDAdBgNVHSUEFjAUBggrBgEFBQcD' TO rt_result.
    APPEND 'AQYIKwYBBQUHAwIwPgYDVR0fBDcwNTAzoDGgL4YtaHR0cDovL2NkcC5yYXBpZHNz' TO rt_result.
    APPEND 'bC5jb20vUmFwaWRTU0xSU0FDQTIwMTguY3JsMEwGA1UdIARFMEMwNwYJYIZIAYb9' TO rt_result.
    APPEND 'bAECMCowKAYIKwYBBQUHAgEWHGh0dHBzOi8vd3d3LmRpZ2ljZXJ0LmNvbS9DUFMw' TO rt_result.
    APPEND 'CAYGZ4EMAQIBMHUGCCsGAQUFBwEBBGkwZzAmBggrBgEFBQcwAYYaaHR0cDovL3N0' TO rt_result.
    APPEND 'YXR1cy5yYXBpZHNzbC5jb20wPQYIKwYBBQUHMAKGMWh0dHA6Ly9jYWNlcnRzLnJh' TO rt_result.
    APPEND 'cGlkc3NsLmNvbS9SYXBpZFNTTFJTQUNBMjAxOC5jcnQwCQYDVR0TBAIwADCCAQUG' TO rt_result.
    APPEND 'CisGAQQB1nkCBAIEgfYEgfMA8QB3APZclC/RdzAiFFQYCDCUVo7jTRMZM7/fDC8g' TO rt_result.
    APPEND 'C8xO8WTjAAABcaH/0QcAAAQDAEgwRgIhAO+7sTa6FFxlub8Gpp7t5hIQxH9qBCWU' TO rt_result.
    APPEND 'lCPQgIIfc5W3AiEA04KZqoDWJ/2CvQpDeNGBgWnyqAgxoTz+2YXe3NVIn+0AdgBc' TO rt_result.
    APPEND '3EOS/uarRUSxXprUVuYQN/vV+kfcoXOUsl7m9scOygAAAXGh/9EtAAAEAwBHMEUC' TO rt_result.
    APPEND 'IDdF6dx+LYd2PgITcwQ8qzwWQuBsuOiVOEZqY+eNsvzsAiEAiG0UwxFO8jVgtmwN' TO rt_result.
    APPEND 'YlGsSIPWP7D/ohKE+RykkeK2mZkwDQYJKoZIhvcNAQELBQADggEBACR7v//cv19G' TO rt_result.
    APPEND 'TxuKaB8i38uiJQzXg7owLu/rPm9LYzlQnxnGW7IrOILukIWuHxo7vSFim9vb5B55' TO rt_result.
    APPEND '4KdaDviclrVuNTVN+0WEKS9Cft0Yvwk2cdcrjylVUzon+MIbeLr2zFmzLW0USifp' TO rt_result.
    APPEND 'sdatrVc9rVE2Lfv3M4eV+L65Qavmn2XJNcX4Drgfy+1jM22iMauRJJ+a8+TZ50qz' TO rt_result.
    APPEND '/4YlQ3ru1lJka66oSow+1yfTl2nH1xlb3IVmtTkhA/C+6GY3yST4/EjAB4IWv9s5' TO rt_result.
    APPEND '/EwOZ5cBOL5b+m6F8bW3n3g4YKJScjWO5A2dYlqzkFNB76vxvr+NPRAScn2jxa1F' TO rt_result.
    APPEND 'sTblQLv/XXc=' TO rt_result.
    APPEND '-----END CERTIFICATE-----' TO rt_result.

  ENDMETHOD.


  METHOD _rfc_destination.

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
        lv_applic = _ssl_client( ).
        lv_rfcdest = _rfc_destination_mbt( ).

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


  METHOD _rfc_destination_mbt.

    IF go_settings IS BOUND.
      rv_result = go_settings->get_value( /mbtools/cl_tools=>c_reg-key_rfcdest ).
    ENDIF.

    IF rv_result IS INITIAL.
      rv_result = /mbtools/if_definitions=>c_rfcdest.
    ENDIF.

  ENDMETHOD.


  METHOD _ssl_client.

    IF go_settings IS BOUND.
      rv_result = go_settings->get_value( /mbtools/cl_tools=>c_reg-key_ssl_client ).
    ENDIF.

    IF rv_result IS INITIAL.
      rv_result = c_anonym.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
