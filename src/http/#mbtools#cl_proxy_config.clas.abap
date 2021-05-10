CLASS /mbtools/cl_proxy_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* MBT Proxy Configuration
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************
  PUBLIC SECTION.

    METHODS constructor .
    METHODS get_proxy_host
      RETURNING
        VALUE(rv_proxy_host) TYPE string .
    METHODS get_proxy_port
      RETURNING
        VALUE(rv_port) TYPE string .
    METHODS get_proxy_authentication
      RETURNING
        VALUE(rv_auth) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_proxy,
        host TYPE string VALUE 'ProxyHost',
        port TYPE string VALUE 'ProxyPort',
        auth TYPE string VALUE 'ProxyAuthentication',
      END OF c_proxy.

    DATA mo_settings TYPE REF TO /mbtools/cl_registry.

ENDCLASS.



CLASS /mbtools/cl_proxy_config IMPLEMENTATION.


  METHOD constructor.

    mo_settings = /mbtools/cl_tool_manager=>factory( )->get_settings( ).

  ENDMETHOD.


  METHOD get_proxy_authentication.

    CHECK mo_settings IS BOUND.

    rv_auth = mo_settings->get_value( c_proxy-auth ).

  ENDMETHOD.


  METHOD get_proxy_host.

    CHECK mo_settings IS BOUND.

    rv_proxy_host = mo_settings->get_value( c_proxy-host ).

  ENDMETHOD.


  METHOD get_proxy_port.

    CHECK mo_settings IS BOUND.

    rv_port = mo_settings->get_value( c_proxy-port ).

    CONDENSE rv_port.

  ENDMETHOD.
ENDCLASS.
