************************************************************************
* /MBTOOLS/CL_BUNDLE_FREE
* MBT Free Version
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_bundle_free DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /mbtools/if_manifest .

    CONSTANTS:
      BEGIN OF c_tool,
        version     TYPE string VALUE '1.0.0' ##NO_TEXT,
        title       TYPE string VALUE 'MBT Free Version' ##NO_TEXT,
        is_bundle   TYPE c VALUE abap_true,
        bundle_id   TYPE i VALUE 0,
        download_id TYPE i VALUE 4480,
        description TYPE string
        VALUE 'Everything you need to get started with Marc Bernard Tools' ##NO_TEXT,
      END OF c_tool .

    METHODS constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES mbt_manifest
      FOR /mbtools/if_manifest~descriptor .

    DATA mo_tool TYPE REF TO /mbtools/cl_tools .
ENDCLASS.



CLASS /MBTOOLS/CL_BUNDLE_FREE IMPLEMENTATION.


  METHOD constructor .
    CREATE OBJECT mo_tool EXPORTING io_tool = me.
    mbt_manifest = mo_tool->mbt_manifest.
  ENDMETHOD.
ENDCLASS.
