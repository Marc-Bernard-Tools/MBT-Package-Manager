CLASS /mbtools/cl_mbt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS is_online
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_mbt IMPLEMENTATION.


  METHOD is_online.

    rv_result = /mbtools/cl_http=>ping( iv_url   = /mbtools/if_definitions=>c_www_home
                                                && /mbtools/if_definitions=>c_www_ping
                                        iv_regex = 'Marc Bernard Tools' ).

  ENDMETHOD.
ENDCLASS.
