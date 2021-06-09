CLASS /mbtools/cl_logger_settings DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS /mbtools/cl_logger_factory .

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

  PUBLIC SECTION.

    INTERFACES /mbtools/if_logger_settings .

    METHODS constructor .

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mv_auto_save TYPE abap_bool.
    DATA mv_expiry_date TYPE aldate_del .
    DATA mv_must_be_kept_until_expiry TYPE del_before.
    DATA mv_max_exception_drill_down TYPE i.
    DATA mv_use_2nd_db_connection TYPE /mbtools/if_logger_settings=>ty_flag.
ENDCLASS.



CLASS /mbtools/cl_logger_settings IMPLEMENTATION.


  METHOD /mbtools/if_logger_settings~get_autosave.
    rv_auto_save = mv_auto_save.
  ENDMETHOD.


  METHOD /mbtools/if_logger_settings~get_expiry_date.
    rv_expiry_date = mv_expiry_date.
  ENDMETHOD.


  METHOD /mbtools/if_logger_settings~get_max_exception_drill_down.
    rv_levels = mv_max_exception_drill_down.
  ENDMETHOD.


  METHOD /mbtools/if_logger_settings~get_must_be_kept_until_expiry.
    rv_must_be_kept_until_expiry = mv_must_be_kept_until_expiry.
  ENDMETHOD.


  METHOD /mbtools/if_logger_settings~get_usage_of_secondary_db_conn.
    rv_2nd_db_connection_enabled = mv_use_2nd_db_connection.
  ENDMETHOD.


  METHOD /mbtools/if_logger_settings~set_autosave.
    mv_auto_save = iv_auto_save.
    ro_self = me.
  ENDMETHOD.


  METHOD /mbtools/if_logger_settings~set_expiry_date.
    mv_expiry_date = iv_expiry_date.
    ro_self = me.
  ENDMETHOD.


  METHOD /mbtools/if_logger_settings~set_expiry_in_days.
    IF iv_num_days > 0.
      mv_expiry_date = sy-datum + iv_num_days.
    ENDIF.
    ro_self = me.
  ENDMETHOD.


  METHOD /mbtools/if_logger_settings~set_max_exception_drill_down.
    IF iv_levels >= 0.
      mv_max_exception_drill_down = iv_levels.
    ENDIF.
    ro_self = me.
  ENDMETHOD.


  METHOD /mbtools/if_logger_settings~set_must_be_kept_until_expiry.
    mv_must_be_kept_until_expiry = iv_must_be_kept_until_expiry.
    ro_self = me.
  ENDMETHOD.


  METHOD /mbtools/if_logger_settings~set_usage_of_secondary_db_conn.
    mv_use_2nd_db_connection = iv_use_2nd_db_connection.
    ro_self = me.
  ENDMETHOD.


  METHOD constructor.
    mv_must_be_kept_until_expiry = abap_false.
    mv_max_exception_drill_down = 10.
    mv_use_2nd_db_connection = abap_true.
    mv_auto_save = abap_true.
  ENDMETHOD.
ENDCLASS.
