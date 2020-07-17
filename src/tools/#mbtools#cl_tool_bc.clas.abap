************************************************************************
* /MBTOOLS/CL_TOOL_BC
* MBT Base
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_tool_bc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /mbtools/if_manifest .

    CONSTANTS:
      BEGIN OF c_tool,
        version     TYPE string VALUE '1.0.0' ##NO_TEXT,
        title       TYPE string VALUE 'MBT Base' ##NO_TEXT,
        description TYPE string VALUE 'Essential Tools for SAP® Customers & Partners' ##NO_TEXT,
        bundle_id   TYPE i VALUE 0,
        download_id TYPE i VALUE 4480,
      END OF c_tool.

    METHODS constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES mbt_manifest
      FOR /mbtools/if_manifest~descriptor .

    DATA mo_tool TYPE REF TO /mbtools/cl_tools .
ENDCLASS.



CLASS /MBTOOLS/CL_TOOL_BC IMPLEMENTATION.


  METHOD constructor.
    CREATE OBJECT mo_tool EXPORTING io_tool = me.
    mbt_manifest = mo_tool->mbt_manifest.
  ENDMETHOD.
ENDCLASS.
