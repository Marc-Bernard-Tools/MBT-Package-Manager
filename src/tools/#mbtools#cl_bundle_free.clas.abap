CLASS /mbtools/cl_bundle_free DEFINITION
  PUBLIC
  INHERITING FROM /mbtools/cl_bundle
  FINAL
  CREATE PUBLIC .

************************************************************************
* MBT Free Tools
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_tool,
        version     TYPE string VALUE '1.0.0' ##NO_TEXT,
        title       TYPE string VALUE 'MBT Free Tools' ##NO_TEXT,
        is_bundle   TYPE c VALUE abap_true,
        bundle_id   TYPE i VALUE 0,
        download_id TYPE i VALUE 4480,
        description TYPE string
        VALUE 'Everything you need to get started with Marc Bernard Tools' ##NO_TEXT,
      END OF c_tool .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_bundle_free IMPLEMENTATION.
ENDCLASS.
