CLASS /mbtools/cl_mbt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS is_online
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS is_mbt_system
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_mbt IMPLEMENTATION.


  METHOD is_mbt_system.

    DATA lv_value TYPE spfl_parameter_value.

    lv_value = cl_spfl_profile_parameter=>get_value( 'SAPGLOBALHOST' ).

    rv_result = boolc( lv_value CP 'MBT*' ).

  ENDMETHOD.


  METHOD is_online.

    rv_result = /mbtools/cl_http=>ping( iv_url   = /mbtools/if_definitions=>c_www_home
                                                && /mbtools/if_definitions=>c_www_ping
                                        iv_regex = /mbtools/if_definitions=>c_mbt ).

  ENDMETHOD.
ENDCLASS.
