INTERFACE /mbtools/if_tool
  PUBLIC .

************************************************************************
* MBT Tool
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

  TYPES:
    " In case of changes, also adjust structure /mbtools/manifest
    BEGIN OF ty_manifest,
      id          TYPE i,
      bundle_id   TYPE i,
      is_bundle   TYPE abap_bool,
      name        TYPE string,
      version     TYPE string,
      title       TYPE string,
      description TYPE string,
      namespace   TYPE namespace,
      package     TYPE devclass,
      class       TYPE seoclsname,
      git_url     TYPE string,
      has_launch  TYPE abap_bool,
      command     TYPE string,
      shortcut    TYPE string,
    END OF ty_manifest.

  DATA ms_manifest TYPE ty_manifest READ-ONLY.

  METHODS launch
    RAISING
      /mbtools/cx_exception.

ENDINTERFACE.
