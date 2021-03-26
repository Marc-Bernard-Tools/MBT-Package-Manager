CLASS /mbtools/cl_mbt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.
    CLASS-METHODS is_offline
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS is_online
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS is_mbt_system
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA go_settings TYPE REF TO /mbtools/cl_registry.
ENDCLASS.



CLASS /mbtools/cl_mbt IMPLEMENTATION.


  METHOD class_constructor.

    go_settings = /mbtools/cl_tools=>factory( )->get_settings( ).

  ENDMETHOD.


  METHOD is_mbt_system.

    DATA lv_host TYPE string.

    lv_host = /mbtools/cl_utilities=>get_profile_parameter( 'SAPLOCALHOST' ).

    rv_result = boolc( lv_host CP 'MBT*' ).

  ENDMETHOD.


  METHOD is_offline.

    IF go_settings IS NOT INITIAL.
      rv_result = go_settings->get_value( /mbtools/cl_tools=>c_reg-key_offline ).
    ENDIF.

  ENDMETHOD.


  METHOD is_online.

    DATA:
      lv_offline TYPE abap_bool,
      lv_rfcdest TYPE rfcdest,
      lv_path    TYPE string,
      lo_client  TYPE REF TO /mbtools/cl_http_client,
      lx_error   TYPE REF TO /mbtools/cx_exception.

    TRY.
        IF go_settings IS BOUND.
          lv_offline = go_settings->get_value( /mbtools/cl_tools=>c_reg-key_offline ).
        ENDIF.

        IF lv_offline IS INITIAL.

          lv_rfcdest = /mbtools/cl_setup=>get_rfc_destination( ).
          lv_path    = '/' && /mbtools/if_definitions=>c_www_ping.

          lo_client = /mbtools/cl_http=>create_by_destination(
            iv_destination = lv_rfcdest
            iv_path        = lv_path ).

          lo_client->check_smart_response(
            iv_expected_content_type = 'text/html'
            iv_content_regex         = /mbtools/if_definitions=>c_mbt ).

          lo_client->close( ).

          rv_result = abap_true.

        ENDIF.

      CATCH /mbtools/cx_exception INTO lx_error.
        IF lo_client IS BOUND.
          lo_client->close( ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
