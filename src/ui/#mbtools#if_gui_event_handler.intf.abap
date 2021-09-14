INTERFACE /mbtools/if_gui_event_handler
  PUBLIC .

************************************************************************
* Marc Bernard Tools - GUI Error Handler
*
* Copyright 2014 abapGit Contributors <http://www.abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  TYPES:
    BEGIN OF ty_handling_result,
      page  TYPE REF TO /mbtools/if_gui_renderable,
      state TYPE i,
    END OF ty_handling_result.

  METHODS on_event
    IMPORTING
      ii_event          TYPE REF TO /mbtools/if_gui_event
    RETURNING
      VALUE(rs_handled) TYPE ty_handling_result
    RAISING
      /mbtools/cx_exception.

ENDINTERFACE.
