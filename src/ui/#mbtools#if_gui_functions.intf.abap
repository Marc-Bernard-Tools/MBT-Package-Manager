INTERFACE /mbtools/if_gui_functions
  PUBLIC .

************************************************************************
* Marc Bernard Tools - GUI Functions
*
* Copyright 2014 abapGit Contributors <http://www.abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  METHODS gui_is_available
    RETURNING
      VALUE(rv_gui_is_available) TYPE abap_bool .

  METHODS is_sapgui_for_java
    RETURNING
      VALUE(rv_result) TYPE abap_bool .

ENDINTERFACE.
