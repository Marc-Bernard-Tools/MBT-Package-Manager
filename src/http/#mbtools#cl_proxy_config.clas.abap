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
    METHODS get_proxy_url
      IMPORTING
        !iv_repo_url        TYPE csequence OPTIONAL
      RETURNING
        VALUE(rv_proxy_url) TYPE string .
    METHODS get_proxy_port
      IMPORTING
        !iv_repo_url   TYPE csequence OPTIONAL
      RETURNING
        VALUE(rv_port) TYPE string .
    METHODS get_proxy_authentication
      IMPORTING
        !iv_repo_url   TYPE csequence OPTIONAL
      RETURNING
        VALUE(rv_auth) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_proxy,
        url  TYPE string VALUE 'ProxyURL',
        port TYPE string VALUE 'ProxyPort',
        auth TYPE string VALUE 'ProxyAuthentication',
      END OF c_proxy.

    DATA mo_settings TYPE REF TO /mbtools/cl_registry.

ENDCLASS.



CLASS /MBTOOLS/CL_PROXY_CONFIG IMPLEMENTATION.


  METHOD constructor.

    mo_settings = /mbtools/cl_tools=>factory( )->get_settings( ).

  ENDMETHOD.


  METHOD get_proxy_authentication.

    rv_auth = mo_settings->get_value( c_proxy-auth ).

  ENDMETHOD.


  METHOD get_proxy_port.

    rv_port = mo_settings->get_value( c_proxy-port ).

    CONDENSE rv_port.

  ENDMETHOD.


  METHOD get_proxy_url.

    rv_proxy_url = mo_settings->get_value( c_proxy-url ).

  ENDMETHOD.
ENDCLASS.
