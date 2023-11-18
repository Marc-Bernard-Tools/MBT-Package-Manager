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
    " issuer=C = US, O = DigiCert Inc, CN = RapidSSL TLS DV RSA Mixed SHA256 2020 CA-1
    " notBefore=Nov 18 00:00:00 2022 GMT
    " notAfter=Dec  3 23:59:59 2023 GMT

    APPEND '-----BEGIN CERTIFICATE-----' TO rt_result.
    APPEND 'MIIHrDCCBZSgAwIBAgIQCR1zSMxn49heB5UgINrEjDANBgkqhkiG9w0BAQsFADBc' TO rt_result.
    APPEND 'MQswCQYDVQQGEwJVUzEXMBUGA1UEChMORGlnaUNlcnQsIEluYy4xNDAyBgNVBAMT' TO rt_result.
    APPEND 'K1JhcGlkU1NMIEdsb2JhbCBUTFMgUlNBNDA5NiBTSEEyNTYgMjAyMiBDQTEwHhcN' TO rt_result.
    APPEND 'MjIxMTE4MDAwMDAwWhcNMjMxMjAzMjM1OTU5WjAhMR8wHQYDVQQDDBYqLm1hcmNi' TO rt_result.
    APPEND 'ZXJuYXJkdG9vbHMuY29tMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA' TO rt_result.
    APPEND 'l4OeYBB75/DWhLdyHXnsLytqeWqg+HmHDjuEpeHKo8xeicvFFayrsA3zXl7OVYew' TO rt_result.
    APPEND 'VLhlyBezHSp2KHaROTqsqQWIEsDvDWOPm2thcVzonRxEwoEtzquJhtCJ6nSxM7dL' TO rt_result.
    APPEND 'JrlWv5I1Mn3YS9rlupjNcOA60gYlOaDneKjjjh+bizbImq9/Xl4HO6WxzESwNmjr' TO rt_result.
    APPEND 'oEbt3J4AsXt3UN4sUTboPXo4aRcCuj7ABipTTGaWwGGM0MvJl4ak2WMRShN+SMM3' TO rt_result.
    APPEND 'rYPc3WmpkJKCqwgAhMSDsNf+DZrKXi8GKUH5JcYzEgNiCbmaXcZtM8MD7iIyLo+5' TO rt_result.
    APPEND 'tJQCsey2OLHvAkhog8aBRwIDAQABo4IDozCCA58wHwYDVR0jBBgwFoAU8JyF/aKf' TO rt_result.
    APPEND 'fY/JaLvV1IlNHb7TkP8wHQYDVR0OBBYEFNIjh+uU+D0VpvVrHeyUHuXvgeBMMDcG' TO rt_result.
    APPEND 'A1UdEQQwMC6CFioubWFyY2Jlcm5hcmR0b29scy5jb22CFG1hcmNiZXJuYXJkdG9v' TO rt_result.
    APPEND 'bHMuY29tMA4GA1UdDwEB/wQEAwIFoDAdBgNVHSUEFjAUBggrBgEFBQcDAQYIKwYB' TO rt_result.
    APPEND 'BQUHAwIwgZ8GA1UdHwSBlzCBlDBIoEagRIZCaHR0cDovL2NybDMuZGlnaWNlcnQu' TO rt_result.
    APPEND 'Y29tL1JhcGlkU1NMR2xvYmFsVExTUlNBNDA5NlNIQTI1NjIwMjJDQTEuY3JsMEig' TO rt_result.
    APPEND 'RqBEhkJodHRwOi8vY3JsNC5kaWdpY2VydC5jb20vUmFwaWRTU0xHbG9iYWxUTFNS' TO rt_result.
    APPEND 'U0E0MDk2U0hBMjU2MjAyMkNBMS5jcmwwPgYDVR0gBDcwNTAzBgZngQwBAgEwKTAn' TO rt_result.
    APPEND 'BggrBgEFBQcCARYbaHR0cDovL3d3dy5kaWdpY2VydC5jb20vQ1BTMIGHBggrBgEF' TO rt_result.
    APPEND 'BQcBAQR7MHkwJAYIKwYBBQUHMAGGGGh0dHA6Ly9vY3NwLmRpZ2ljZXJ0LmNvbTBR' TO rt_result.
    APPEND 'BggrBgEFBQcwAoZFaHR0cDovL2NhY2VydHMuZGlnaWNlcnQuY29tL1JhcGlkU1NM' TO rt_result.
    APPEND 'R2xvYmFsVExTUlNBNDA5NlNIQTI1NjIwMjJDQTEuY3J0MAkGA1UdEwQCMAAwggF8' TO rt_result.
    APPEND 'BgorBgEEAdZ5AgQCBIIBbASCAWgBZgB1AOg+0No+9QY1MudXKLyJa8kD08vREWvs' TO rt_result.
    APPEND '62nhd31tBr1uAAABhIidYQAAAAQDAEYwRAIgZsOmMcCMB1OZ5mNsCuueVMzWxOVo' TO rt_result.
    APPEND '2JMbZ2t7IH2fBN0CIAeJXwgejTgXApkDNbkXvuAzOH6V9g/wwf8gQdP77NEPAHYA' TO rt_result.
    APPEND 's3N3B+GEUPhjhtYFqdwRCUp5LbFnDAuH3PADDnk2pZoAAAGEiJ1hXwAABAMARzBF' TO rt_result.
    APPEND 'AiEA+F+KFItSxzzcU+OUqii8jfRvs+LjL5fh77yzKTGWIoICICPhhMQ6uPgYTHAf' TO rt_result.
    APPEND 'Mc+LTJ5Qz9n+E6s2G6WTvDz6wpSSAHUAtz77JN+cTbp18jnFulj0bF38Qs96nzXE' TO rt_result.
    APPEND 'nh0JgSXttJkAAAGEiJ1hFQAABAMARjBEAiA4AjgnI7+IDqZL0oOdjyzpBZ0o5l5+' TO rt_result.
    APPEND 'ak0wQxfAG1H6lAIgJpW8mID/Nk0E076JWTN+fbOSZKma2Xz6jFr2A7GmgzgwDQYJ' TO rt_result.
    APPEND 'KoZIhvcNAQELBQADggIBABLB8/qJKVsx1XQ09rYD3X5Kz+crb1k3ra8GUmy+WH43' TO rt_result.
    APPEND 'e4r8EbZwUV7VgE1uy2XjEM7ez7yXPZpkn+qe2MIOO66a+t7RPx/yYcUDeEPsZKeg' TO rt_result.
    APPEND 'jNvQyzmv1joRhQXcT5LitUPAAcKQxIuWpmLsQLu5Bjk3JMRwdvsYksLqgt8BqKsO' TO rt_result.
    APPEND 'o3Z8FDdeQW9bBHVBjzbRcjujyIXTul1S0bt1Tx6Xzusm1JgR0BumcsMr3GqJG2kn' TO rt_result.
    APPEND 'kpMb95zKGS7sWATA1a28Sh013KlHtVWfpcdoUA9ixZomtDQErwraeX1yFXPfVQpl' TO rt_result.
    APPEND '90USk7aPvm+9Oqj6+UKMrT+yPIDy3SLAH3wz+KJzouHgFbeV7z2P8DAqqr+bv7uH' TO rt_result.
    APPEND 'jhjRhqyiGBPmpKO1GIhk90zYgn30xTylfR/2uyQbQEMV/K0NBOeaBKd5JuHj+XjM' TO rt_result.
    APPEND '/jWhSQMQCKz4jt/dC30GK6RF3h4zQr8QAD2CpGEpu3Vnt3Jk8w4abXZ47i+NzUmu' TO rt_result.
    APPEND 'L+1lGgosvB4sa21ATZRqmwAz3e/5WGyoJemwkU+eKwet+Lunz+2iJSGMNQiGSBlZ' TO rt_result.
    APPEND '4v3xctYElBw8pyZZqWyKGjUp8ymHsTixtlfjYAEsrucILYqoMqjuEt1UDiJaEoSN' TO rt_result.
    APPEND '+ak0UU59wxMRqOfVHipk0SBxqKN1uBBpZVYLJb8Yv9oBZvsgPudvn6DItX33BMRw' TO rt_result.
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
