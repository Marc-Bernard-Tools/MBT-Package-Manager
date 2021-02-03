INTERFACE /mbtools/if_tool
  PUBLIC.

************************************************************************
* MBT Tool
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

  TYPES ty_manifest TYPE /mbtools/manifest.

  DATA ms_manifest TYPE ty_manifest READ-ONLY.

  METHODS launch
    RAISING
      /mbtools/cx_exception.

ENDINTERFACE.
