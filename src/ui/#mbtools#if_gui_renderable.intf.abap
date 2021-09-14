INTERFACE /mbtools/if_gui_renderable
  PUBLIC .

************************************************************************
* Marc Bernard Tools - GUI Renderable
*
* Copyright 2014 abapGit Contributors <http://www.abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  METHODS render
    RETURNING
      VALUE(ri_html) TYPE REF TO /mbtools/if_html
    RAISING
      /mbtools/cx_exception .

ENDINTERFACE.
