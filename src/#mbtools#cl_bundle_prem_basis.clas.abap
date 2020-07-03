************************************************************************
* /MBTOOLS/CL_BUNDLE_PREM_BASIS
* MBT Premium Basis
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_bundle_prem_basis DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /mbtools/if_manifest .

    CONSTANTS:
      BEGIN OF c_tool,
        version     TYPE string VALUE '1.0.0' ##NO_TEXT,
        title       TYPE string VALUE 'MBT Premium Basis' ##NO_TEXT,
        is_bundle   TYPE c VALUE abap_true,
        bundle_id   TYPE i VALUE 1,
        download_id TYPE i VALUE 4540,
        description TYPE string
        VALUE 'Improved user experience and productivity with enhancements for SAP Basis' ##NO_TEXT,
      END OF c_tool .

    METHODS constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES mbt_manifest
      FOR /mbtools/if_manifest~descriptor .

    DATA mo_tool TYPE REF TO /mbtools/cl_tools .
ENDCLASS.



CLASS /MBTOOLS/CL_BUNDLE_PREM_BASIS IMPLEMENTATION.


  METHOD constructor .
    CREATE OBJECT mo_tool EXPORTING io_tool = me.
    mbt_manifest = mo_tool->mbt_manifest.
  ENDMETHOD.
ENDCLASS.
