CLASS /mbtools/cl_tool_bc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* MBT Package Manager
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************

  PUBLIC SECTION.

    INTERFACES /mbtools/if_tool.

    CONSTANTS:
      BEGIN OF c_tool,
        version      TYPE string VALUE '1.4.4' ##NO_TEXT,
        title        TYPE string VALUE 'MBT Package Manager' ##NO_TEXT,
        description  TYPE string VALUE 'Package Manager for Marc Bernard Tools' ##NO_TEXT,
        bundle_id    TYPE i VALUE 0,
        download_id  TYPE i VALUE 4873,
        has_launch   TYPE abap_bool VALUE abap_true,
        mbt_command  TYPE string VALUE 'PM',
        mbt_shortcut TYPE string VALUE 'MBT',
      END OF c_tool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_tool_bc IMPLEMENTATION.


  METHOD /mbtools/if_tool~install.
    " Perform setup that was not included in installation
    /mbtools/cl_setup=>install( ).
  ENDMETHOD.


  METHOD /mbtools/if_tool~launch.
    /mbtools/cl_sap=>run_program( '/MBTOOLS/MBT' ).
  ENDMETHOD.


  METHOD /mbtools/if_tool~title.
    rv_title = c_tool-title.
  ENDMETHOD.


  METHOD /mbtools/if_tool~tool.
    MOVE-CORRESPONDING c_tool TO rs_tool.
  ENDMETHOD.


  METHOD /mbtools/if_tool~uninstall.
    RETURN.
  ENDMETHOD.
ENDCLASS.
