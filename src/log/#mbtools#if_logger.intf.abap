INTERFACE /mbtools/if_logger
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
  DATA mv_handle TYPE balloghndl READ-ONLY .
  DATA mv_db_number TYPE balognr READ-ONLY .
  DATA ms_header TYPE bal_s_log READ-ONLY .
  DATA mv_timestamp TYPE timestampl READ-ONLY .

  METHODS add
    IMPORTING
      !iv_obj_to_log    TYPE any OPTIONAL
      !iv_context       TYPE simple OPTIONAL
      !iv_callback_form TYPE csequence OPTIONAL
      !iv_callback_prog TYPE csequence OPTIONAL
      !iv_callback_fm   TYPE csequence OPTIONAL
      !iv_type          TYPE symsgty OPTIONAL
      !iv_importance    TYPE balprobcl OPTIONAL
        PREFERRED PARAMETER iv_obj_to_log
    RETURNING
      VALUE(ro_self)    TYPE REF TO /mbtools/if_logger .
  METHODS a
    IMPORTING
      !iv_obj_to_log    TYPE any OPTIONAL
      !iv_context       TYPE simple OPTIONAL
      !iv_callback_form TYPE csequence OPTIONAL
      !iv_callback_prog TYPE csequence OPTIONAL
      !iv_callback_fm   TYPE csequence OPTIONAL
      !iv_importance    TYPE balprobcl OPTIONAL
        PREFERRED PARAMETER iv_obj_to_log
    RETURNING
      VALUE(ro_self)    TYPE REF TO /mbtools/if_logger .
  METHODS e
    IMPORTING
      !iv_obj_to_log    TYPE any OPTIONAL
      !iv_context       TYPE simple OPTIONAL
      !iv_callback_form TYPE csequence OPTIONAL
      !iv_callback_prog TYPE csequence OPTIONAL
      !iv_callback_fm   TYPE csequence OPTIONAL
      !iv_importance    TYPE balprobcl OPTIONAL
        PREFERRED PARAMETER iv_obj_to_log
    RETURNING
      VALUE(ro_self)    TYPE REF TO /mbtools/if_logger .
  METHODS w
    IMPORTING
      !iv_obj_to_log    TYPE any OPTIONAL
      !iv_context       TYPE simple OPTIONAL
      !iv_callback_form TYPE csequence OPTIONAL
      !iv_callback_prog TYPE csequence OPTIONAL
      !iv_callback_fm   TYPE csequence OPTIONAL
      !iv_importance    TYPE balprobcl OPTIONAL
        PREFERRED PARAMETER iv_obj_to_log
    RETURNING
      VALUE(ro_self)    TYPE REF TO /mbtools/if_logger .
  METHODS i
    IMPORTING
      !iv_obj_to_log    TYPE any OPTIONAL
      !iv_context       TYPE simple OPTIONAL
      !iv_callback_form TYPE csequence OPTIONAL
      !iv_callback_prog TYPE csequence OPTIONAL
      !iv_callback_fm   TYPE csequence OPTIONAL
      !iv_importance    TYPE balprobcl OPTIONAL
        PREFERRED PARAMETER iv_obj_to_log
    RETURNING
      VALUE(ro_self)    TYPE REF TO /mbtools/if_logger .
  METHODS s
    IMPORTING
      !iv_obj_to_log    TYPE any OPTIONAL
      !iv_context       TYPE simple OPTIONAL
      !iv_callback_form TYPE csequence OPTIONAL
      !iv_callback_prog TYPE csequence OPTIONAL
      !iv_callback_fm   TYPE csequence OPTIONAL
      !iv_importance    TYPE balprobcl OPTIONAL
        PREFERRED PARAMETER iv_obj_to_log
    RETURNING
      VALUE(ro_self)    TYPE REF TO /mbtools/if_logger .
  METHODS has_errors
    RETURNING
      VALUE(rv_yes) TYPE abap_bool .
  METHODS has_warnings
    RETURNING
      VALUE(rv_yes) TYPE abap_bool .
  METHODS is_empty
    RETURNING
      VALUE(rv_yes) TYPE abap_bool .
  METHODS length
    RETURNING
      VALUE(rv_length) TYPE i .
  "! Saves the log on demand. Intended to be called at the
  "! end of the log processing so that logs can be saved depending
  "! on other criteria, like the existence of error messages.
  "! If there are no error messages, it may not be desirable to save
  "! a log.
  "! If auto save is enabled, save will do nothing.
  METHODS save .
  METHODS export_to_table
    RETURNING
      VALUE(rt_bapiret) TYPE bapirettab .
  METHODS fullscreen .
  METHODS popup
    IMPORTING
      !is_profile TYPE bal_s_prof OPTIONAL .
  METHODS timer_start .
  "! Time in milliseconds (1/1000s)
  METHODS timer_end
    RETURNING
      VALUE(rv_result) TYPE timestampl .
ENDINTERFACE.
