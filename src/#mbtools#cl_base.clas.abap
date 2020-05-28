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
    CONSTANTS c_download_id TYPE i VALUE 4480 ##NO_TEXT.

    METHODS constructor .
    METHODS initialize
      IMPORTING
        !i_all TYPE abap_bool.
    METHODS screen .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES apack_manifest
      FOR if_apack_manifest~descriptor .
    ALIASES mbt_manifest
      FOR /mbtools/if_manifest~descriptor .

    DATA mr_tool TYPE REF TO /mbtools/cl_tools .
    DATA m_all TYPE abap_bool.
ENDCLASS.



CLASS /MBTOOLS/CL_BASE IMPLEMENTATION.


  METHOD constructor.
    CREATE OBJECT mr_tool EXPORTING i_tool = me.

    apack_manifest = mr_tool->apack_manifest.
    mbt_manifest   = mr_tool->mbt_manifest.
  ENDMETHOD.


  METHOD initialize.

    m_all = i_all.

  ENDMETHOD.


  METHOD screen.

    DATA l_show TYPE abap_bool.

    LOOP AT SCREEN.
      l_show = abap_true.

      IF screen-name = 'P_TITLE' AND m_all = abap_true.
        l_show = abap_false.
      ENDIF.

      IF l_show = abap_true.
*        screen-active = '1'.
        screen-input = '1'.
      ELSE.
*        screen-active = '0'.
        screen-input = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
