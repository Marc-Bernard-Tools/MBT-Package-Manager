INTERFACE /mbtools/if_gui_event
  PUBLIC.

************************************************************************
* Marc Bernard Tools - GUI Event
*
* Copyright 2014 abapGit Contributors <http://abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  DATA mv_action TYPE string READ-ONLY.
  DATA mv_getdata TYPE string READ-ONLY.
  DATA mt_postdata TYPE cnht_post_data_tab READ-ONLY.
  DATA mi_gui_services TYPE REF TO /mbtools/if_gui_services READ-ONLY.
  DATA mv_current_page_name TYPE string READ-ONLY.

  METHODS get_param
    IMPORTING
      !iv_key         TYPE string
    RETURNING
      VALUE(rv_value) TYPE string.
  METHODS get_params
    CHANGING
      !cs_params TYPE any.
ENDINTERFACE.
