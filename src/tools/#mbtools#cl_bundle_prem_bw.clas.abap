CLASS /mbtools/cl_bundle_prem_bw DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* MBT Premium BW
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

  PUBLIC SECTION.

    INTERFACES /mbtools/if_manifest .

    ALIASES mbt_manifest
      FOR /mbtools/if_manifest~descriptor .

    CONSTANTS:
      BEGIN OF c_tool,
        version     TYPE string VALUE '1.0.0' ##NO_TEXT,
        title       TYPE string VALUE 'MBT Premium BW' ##NO_TEXT,
        is_bundle   TYPE c VALUE abap_true,
        bundle_id   TYPE i VALUE 2,
        download_id TYPE i VALUE 4542,
        description TYPE string VALUE 'World-class tools and enhancements for SAP BW and SAP BW/4HANA' ##NO_TEXT,
      END OF c_tool .

    METHODS constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_tool TYPE REF TO /mbtools/cl_tools .

ENDCLASS.



CLASS /MBTOOLS/CL_BUNDLE_PREM_BW IMPLEMENTATION.


  METHOD constructor .
    CREATE OBJECT mo_tool EXPORTING io_tool = me.
    mbt_manifest = mo_tool->mbt_manifest.
  ENDMETHOD.
ENDCLASS.
