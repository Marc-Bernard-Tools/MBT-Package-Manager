INTERFACE /mbtools/if_tool
  PUBLIC.

************************************************************************
* MBT Tool
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

  TYPES ty_manifest TYPE /mbtools/manifest.
  TYPES:
    ty_manifests TYPE STANDARD TABLE OF ty_manifest WITH DEFAULT KEY.

  DATA ms_manifest TYPE ty_manifest READ-ONLY.

  METHODS install
    RAISING
      /mbtools/cx_exception.
  METHODS launch
    RAISING
      /mbtools/cx_exception.
  METHODS uninstall
    RAISING
      /mbtools/cx_exception.

ENDINTERFACE.
