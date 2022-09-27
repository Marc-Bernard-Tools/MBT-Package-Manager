CLASS /mbtools/cl_gui_component DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

************************************************************************
* Marc Bernard Tools - GUI Component
*
* Copyright 2014 abapGit Contributors <http://abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_html_parts,
        scripts      TYPE string VALUE 'scripts',
        hidden_forms TYPE string VALUE 'hidden_forms',
      END OF c_html_parts .
  PROTECTED SECTION.

    METHODS register_deferred_script
      IMPORTING
        ii_part TYPE REF TO /mbtools/if_html
      RAISING
        /mbtools/cx_exception.
    METHODS gui_services
      RETURNING
        VALUE(ri_gui_services) TYPE REF TO /mbtools/if_gui_services
      RAISING
        /mbtools/cx_exception.

  PRIVATE SECTION.
    DATA mi_gui_services TYPE REF TO /mbtools/if_gui_services.
ENDCLASS.



CLASS /mbtools/cl_gui_component IMPLEMENTATION.


  METHOD gui_services.
    IF mi_gui_services IS NOT BOUND.
      mi_gui_services = /mbtools/cl_gui_factory=>get_gui_services( ).
    ENDIF.
    ri_gui_services = mi_gui_services.
  ENDMETHOD.


  METHOD register_deferred_script.
    gui_services( )->get_html_parts( )->add_part(
      iv_collection = c_html_parts-scripts
      ii_part       = ii_part ).
  ENDMETHOD.
ENDCLASS.
