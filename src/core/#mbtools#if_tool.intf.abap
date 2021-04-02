INTERFACE /mbtools/if_tool
  PUBLIC.


************************************************************************
* MBT Tool
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
  TYPES ty_tool TYPE /mbtools/tool.

  METHODS title
    RETURNING
      VALUE(rv_title) TYPE string.
  METHODS tool
    RETURNING
      VALUE(rs_tool) TYPE ty_tool.
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
