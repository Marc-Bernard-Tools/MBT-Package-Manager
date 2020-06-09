************************************************************************
* /MBTOOLS/CL_BASE
* MBT Base
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_base DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_apack_manifest .
    INTERFACES /mbtools/if_manifest .

    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NO_TEXT.
    CONSTANTS c_title TYPE string VALUE 'Marc Bernard Tools' ##NO_TEXT.
    CONSTANTS c_description TYPE string VALUE 'Essential Tools for SAP® Customers & Partners by Marc Bernard Tools' ##NO_TEXT.
    CONSTANTS c_bundle_id TYPE i VALUE 0 ##NO_TEXT.
    CONSTANTS c_download_id TYPE i VALUE 4480 ##NO_TEXT.

    METHODS constructor .
    METHODS initialize
      IMPORTING
        !iv_all TYPE abap_bool .
    METHODS screen .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES apack_manifest
      FOR if_apack_manifest~descriptor .
    ALIASES mbt_manifest
      FOR /mbtools/if_manifest~descriptor .

    DATA mo_tool TYPE REF TO /mbtools/cl_tools .
    DATA mv_all TYPE abap_bool .
ENDCLASS.



CLASS /MBTOOLS/CL_BASE IMPLEMENTATION.


  METHOD constructor.
    CREATE OBJECT mo_tool EXPORTING io_tool = me.

    apack_manifest = mo_tool->apack_manifest.
    mbt_manifest   = mo_tool->mbt_manifest.
  ENDMETHOD.


  METHOD initialize.

    mv_all = iv_all.

  ENDMETHOD.


  METHOD screen.

    DATA lv_show TYPE abap_bool.

    LOOP AT SCREEN.
      lv_show = abap_true.

      IF screen-name = 'P_TITLE' AND mv_all = abap_true.
        lv_show = abap_false.
      ENDIF.

      IF lv_show = abap_true.
        screen-input = '1'.
      ELSE.
        screen-input = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
