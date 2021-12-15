INTERFACE /mbtools/if_logger_collection
  PUBLIC .

************************************************************************
* abap logger
*
* Copyright 2017 Eric Peterson <https://github.com/ABAP-Logger/ABAP-Logger>
* SPDX-License-Identifier: MIT
************************************************************************
  METHODS add_logger
    IMPORTING
      ii_logger TYPE REF TO /mbtools/if_logger.
  METHODS display_logs
    IMPORTING
      iv_display_profile_head_size TYPE i DEFAULT 125
      iv_display_profile_tree_size TYPE i DEFAULT 25.
  METHODS display_logs_using_profile
    IMPORTING
      is_display_profile TYPE bal_s_prof.
ENDINTERFACE.
