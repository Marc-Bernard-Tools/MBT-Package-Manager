CLASS /mbtools/cl_access_pass DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* MBT Access Pass
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************
  PUBLIC SECTION.

    INTERFACES /mbtools/if_tool.

    CONSTANTS:
      BEGIN OF c_pass,
        starter      TYPE i VALUE 4480,
        professional TYPE i VALUE 4540,
        business     TYPE i VALUE 4542,
      END OF c_pass.

    CONSTANTS:
      BEGIN OF c_pass_description,
        starter      TYPE string VALUE 'Starter Pass',
        professional TYPE string VALUE 'Professional Pass',
        business     TYPE string VALUE 'Business Pass',
      END OF c_pass_description.

    CONSTANTS:
      BEGIN OF c_tool,
        version     TYPE string VALUE '1.0.0' ##NO_TEXT,
        title       TYPE string VALUE 'Access Pass' ##NO_TEXT,
        is_pass     TYPE c VALUE abap_true,
        bundle_id   TYPE i VALUE -1,
        download_id TYPE i VALUE 0,
        description TYPE string
        VALUE 'Enable automatic checking for and installation of updates' ##NO_TEXT,
      END OF c_tool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_access_pass IMPLEMENTATION.


  METHOD /mbtools/if_tool~install.
    RETURN.
  ENDMETHOD.


  METHOD /mbtools/if_tool~launch.
    RETURN.
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
