INTERFACE /mbtools/if_gui_html_processor
  PUBLIC .

************************************************************************
* Marc Bernard Tools - GUI HTML Processor
*
* Copyright 2014 abapGit Contributors <http://abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  METHODS process
    IMPORTING
      !iv_html         TYPE string
      !ii_gui_services TYPE REF TO /mbtools/if_gui_services
    RETURNING
      VALUE(rv_html)   TYPE string
    RAISING
      /mbtools/cx_exception .

ENDINTERFACE.
