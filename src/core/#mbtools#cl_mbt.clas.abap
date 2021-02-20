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

    rv_result = boolc( cl_spfl_profile_parameter=>saplocalhost CP 'MBT*' ).

  ENDMETHOD.


  METHOD is_offline.

    IF go_settings IS NOT INITIAL.
      rv_result = go_settings->get_value( /mbtools/cl_tools=>c_reg-key_offline ).
    ENDIF.

  ENDMETHOD.


  METHOD is_online.

    DATA lv_offline TYPE abap_bool.

    IF go_settings IS BOUND.
      lv_offline = go_settings->get_value( /mbtools/cl_tools=>c_reg-key_offline ).
    ENDIF.

    IF lv_offline IS INITIAL.
      rv_result = /mbtools/cl_http=>ping( iv_url   = /mbtools/if_definitions=>c_www_home
                                                  && /mbtools/if_definitions=>c_www_ping
                                          iv_regex = /mbtools/if_definitions=>c_mbt ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
