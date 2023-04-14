INTERFACE /mbtools/if_logger_disp_prof
  PUBLIC.

************************************************************************
* abap logger
*
* Copyright 2017 Eric Peterson <https://github.com/ABAP-Logger/ABAP-Logger>
* SPDX-License-Identifier: MIT
************************************************************************

  METHODS set
    IMPORTING
      i_detlevel   TYPE clike OPTIONAL
      i_no_tree    TYPE clike OPTIONAL
      i_popup      TYPE clike OPTIONAL
      i_single_log TYPE clike OPTIONAL
      i_standard   TYPE clike DEFAULT abap_true
    RETURNING
      VALUE(r_self) TYPE REF TO /mbtools/if_logger_disp_prof.
  METHODS get
    RETURNING
      VALUE(r_display_profile) TYPE bal_s_prof.
  METHODS set_grid
    IMPORTING
      i_grid_mode  TYPE clike
    RETURNING
      VALUE(r_self) TYPE REF TO /mbtools/if_logger_disp_prof.
  METHODS set_value
    IMPORTING
      i_fld        TYPE clike
      i_val        TYPE any
    RETURNING
      VALUE(r_self) TYPE REF TO /mbtools/if_logger_disp_prof.
  METHODS set_context_tree
    IMPORTING
      i_context_structure TYPE clike
      i_under_log         TYPE clike DEFAULT space.
  METHODS set_context_message
    IMPORTING
      i_context_structure TYPE clike.
ENDINTERFACE.
