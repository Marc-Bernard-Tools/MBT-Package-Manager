INTERFACE /mbtools/if_tool
  PUBLIC.


************************************************************************
* Marc Bernard Tools - Tool Interface
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-or-later
************************************************************************
  TYPES ty_tool TYPE /mbtools/tool.

  TYPES ty_manifest TYPE /mbtools/manifest.
  TYPES ty_manifests TYPE /mbtools/manifests.

  TYPES ty_tool_with_text TYPE /mbtools/tool_with_text ##NEEDED.
  TYPES ty_tools_with_text TYPE /mbtools/tools_with_text.

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
