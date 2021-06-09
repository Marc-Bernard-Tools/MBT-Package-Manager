INTERFACE /mbtools/if_logger_collection
  PUBLIC .

************************************************************************
* MBT Logger
*
* Original Author: Copyright (c) 2017 Eric Peterson
* https://github.com/epeterson320/ABAP-Logger
*
* Released under MIT License: https://opensource.org/licenses/MIT
*
* Last update: 2021-06-07
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
