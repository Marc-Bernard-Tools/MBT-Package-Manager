INTERFACE /mbtools/if_gui_error_handler
  PUBLIC .

************************************************************************
* Marc Bernard Tools - GUI Error Handler
*
* Copyright 2014 abapGit Contributors <http://www.abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  METHODS handle_error
    IMPORTING
      !ix_error         TYPE REF TO /mbtools/cx_exception
    RETURNING
      VALUE(rv_handled) TYPE abap_bool .

ENDINTERFACE.
