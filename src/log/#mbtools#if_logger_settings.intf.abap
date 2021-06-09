INTERFACE /mbtools/if_logger_settings
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
  TYPES ty_flag TYPE c LENGTH 1.
  "! Is the log automatically saved when adding messages?
  "! See setter for more details.
  METHODS get_autosave
    RETURNING
      VALUE(rv_auto_save) TYPE abap_bool .
  "! Set to true if the log is automatically saved when adding messages.
  "!
  "! If auto save is disabled, the save() method has to be called manually
  "! to write the data to the database (it commits the LUW).
  "! If auto save is enabled, the save() method has no effect.
  "! By default, auto save is enabled.
  "!
  "! Be careful with enabled auto save when processing mass data because it
  "! can decrease system performance significantly.
  METHODS set_autosave
    IMPORTING
      !iv_auto_save  TYPE abap_bool
    RETURNING
      VALUE(ro_self) TYPE REF TO /mbtools/if_logger_settings .
  "! Get the earliest date on which the log can be deleted.
  "! See setter for more details.
  METHODS get_expiry_date
    RETURNING
      VALUE(rv_expiry_date) TYPE aldate_del .
  "! Set the earliest date on which the log can be deleted.
  "! By default the log does not expire.
  "!
  "! Further information: https://launchpad.support.sap.com/#/notes/195157
  METHODS set_expiry_date
    IMPORTING
      !iv_expiry_date TYPE aldate_del
    RETURNING
      VALUE(ro_self)  TYPE REF TO /mbtools/if_logger_settings .
  "! Set the number of days after which the log can be deleted.
  "! By default the log does not expire.
  "!
  "! Further information: https://launchpad.support.sap.com/#/notes/195157
  METHODS set_expiry_in_days
    IMPORTING
      !iv_num_days   TYPE i
    RETURNING
      VALUE(ro_self) TYPE REF TO /mbtools/if_logger_settings .
  "! Does the log have to be kept until the expiry date is reached?
  "! See setter for more details.
  METHODS get_must_be_kept_until_expiry
    RETURNING
      VALUE(rv_must_be_kept_until_expiry) TYPE del_before .
  "! Set to true if log must be kept until the expiry date is reached. It
  "! cannot be deleted before (in transaction SLG2).
  "! The default is false.
  "!
  "! Further information: https://launchpad.support.sap.com/#/notes/195157
  METHODS set_must_be_kept_until_expiry
    IMPORTING
      !iv_must_be_kept_until_expiry TYPE del_before
    RETURNING
      VALUE(ro_self)                TYPE REF TO /mbtools/if_logger_settings .
  METHODS get_max_exception_drill_down
    RETURNING
      VALUE(rv_levels) TYPE i .
  METHODS set_max_exception_drill_down
    IMPORTING
      !iv_levels     TYPE i
    RETURNING
      VALUE(ro_self) TYPE REF TO /mbtools/if_logger_settings .
  "! Is a secondary database connection used to write the log entries to the database?
  "! See setter for more details.
  METHODS get_usage_of_secondary_db_conn
    RETURNING
      VALUE(rv_2nd_db_connection_enabled) TYPE ty_flag.
  "! Set to true if secondary database connection should be used to write the log entries to the database.
  "! This is important if main program does a rollback (on purpose or after a dump).
  "! The default is true.
  METHODS set_usage_of_secondary_db_conn
    IMPORTING
      !iv_use_2nd_db_connection TYPE ty_flag
    RETURNING
      VALUE(ro_self)            TYPE REF TO /mbtools/if_logger_settings .
ENDINTERFACE.
