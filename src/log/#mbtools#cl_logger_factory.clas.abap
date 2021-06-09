CLASS /mbtools/cl_logger_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

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

    "! Starts a new log.
    CLASS-METHODS create_log
      IMPORTING
        !iv_object      TYPE csequence DEFAULT '/MBTOOLS/'
        !iv_subobject   TYPE csequence DEFAULT 'LOG'
        !iv_description TYPE csequence OPTIONAL
        !iv_context     TYPE simple OPTIONAL
        !io_settings    TYPE REF TO /mbtools/if_logger_settings OPTIONAL
          PREFERRED PARAMETER iv_subobject
      RETURNING
        VALUE(ro_log)   TYPE REF TO /mbtools/if_logger .
    "! Reopens an already existing log.
    CLASS-METHODS open_log
      IMPORTING
        !iv_object                   TYPE csequence DEFAULT '/MBTOOLS/'
        !iv_subobject                TYPE csequence
        !iv_description              TYPE csequence OPTIONAL
        !iv_create_if_does_not_exist TYPE abap_bool DEFAULT abap_false
        !io_settings                 TYPE REF TO /mbtools/if_logger_settings OPTIONAL
      RETURNING
        VALUE(ro_log)                TYPE REF TO /mbtools/if_logger .
    "! Creates a settings object which can be modified. It can be pass on
    "! the creation of the logger to change its behavior.
    CLASS-METHODS create_settings
      RETURNING
        VALUE(ro_settings) TYPE REF TO /mbtools/if_logger_settings .

    CLASS-METHODS create_collection
      RETURNING
        VALUE(ro_collection) TYPE REF TO /mbtools/if_logger_collection .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_logger_factory IMPLEMENTATION.


  METHOD create_collection.
    CREATE OBJECT ro_collection TYPE /mbtools/cl_logger_collection.
  ENDMETHOD.


  METHOD create_log.

    DATA: lo_log TYPE REF TO /mbtools/cl_logger.

    FIELD-SYMBOLS <lv_context_val> TYPE c.

    CREATE OBJECT lo_log.
    lo_log->/mbtools/if_logger~ms_header-object    = iv_object.
    lo_log->/mbtools/if_logger~ms_header-subobject = iv_subobject.
    lo_log->/mbtools/if_logger~ms_header-extnumber = iv_description.

    IF io_settings IS BOUND.
      lo_log->mo_settings = io_settings.
    ELSE.
      CREATE OBJECT lo_log->mo_settings TYPE /mbtools/cl_logger_settings.
    ENDIF.

* Special case: Logger can work without object - but then
* the data cannot be written to the database.
    IF iv_object IS INITIAL OR iv_subobject IS INITIAL.
      lo_log->mo_settings->set_autosave( abap_false ).
      lo_log->/mbtools/if_logger~ms_header-object    = '/MBTOOLS/'.
      lo_log->/mbtools/if_logger~ms_header-subobject = 'LOG'.
    ENDIF.

* Use secondary database connection to write data to database even if
* main program does a rollback (e. g. during a dump).
    IF lo_log->mo_settings->get_usage_of_secondary_db_conn( ) = abap_true.
      lo_log->mv_sec_connection     = abap_true.
      lo_log->mv_sec_connect_commit = abap_true.
    ENDIF.

* Set deletion date and set if log can be deleted before deletion date is reached.
    lo_log->/mbtools/if_logger~ms_header-aldate_del = lo_log->mo_settings->get_expiry_date( ).
    lo_log->/mbtools/if_logger~ms_header-del_before = lo_log->mo_settings->get_must_be_kept_until_expiry( ).

    IF iv_context IS SUPPLIED AND iv_context IS NOT INITIAL.
      lo_log->/mbtools/if_logger~ms_header-context-tabname =
        cl_abap_typedescr=>describe_by_data( iv_context )->get_ddic_header( )-tabname.
      ASSIGN iv_context TO <lv_context_val> CASTING.
      lo_log->/mbtools/if_logger~ms_header-context-value = <lv_context_val>.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = lo_log->/mbtools/if_logger~ms_header
      IMPORTING
        e_log_handle = lo_log->/mbtools/if_logger~mv_handle.

* BAL_LOG_CREATE will fill in some additional header data.
* This FM updates our instance attribute to reflect that.
    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = lo_log->/mbtools/if_logger~mv_handle
      IMPORTING
        e_s_log      = lo_log->/mbtools/if_logger~ms_header.

    ro_log = lo_log.

  ENDMETHOD.


  METHOD create_settings.

    CREATE OBJECT ro_settings TYPE /mbtools/cl_logger_settings.

  ENDMETHOD.


  METHOD open_log.

    DATA:
      ls_filter             TYPE bal_s_lfil,
      ls_desc_filter        TYPE bal_s_extn,
      ls_obj_filter         TYPE bal_s_obj,
      ls_subobj_filter      TYPE bal_s_sub,
      lt_found_headers      TYPE balhdr_t,
      ls_most_recent_header TYPE balhdr,
      "lt_handles_loaded     TYPE bal_t_logh,
      lo_log                TYPE REF TO /mbtools/cl_logger.

    ls_desc_filter-option = ls_subobj_filter-option = ls_obj_filter-option = 'EQ'.
    ls_desc_filter-sign   = ls_subobj_filter-sign = ls_obj_filter-sign = 'I'.

    ls_obj_filter-low = iv_object.
    APPEND ls_obj_filter TO ls_filter-object.
    ls_subobj_filter-low = iv_subobject.
    APPEND ls_subobj_filter TO ls_filter-subobject.
    IF iv_description IS SUPPLIED.
      ls_desc_filter-low = iv_description.
      APPEND ls_desc_filter TO ls_filter-extnumber.
    ENDIF.

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_s_log_filter = ls_filter
      IMPORTING
        e_t_log_header = lt_found_headers
      EXCEPTIONS
        log_not_found  = 1.

    IF sy-subrc = 1.
      IF iv_create_if_does_not_exist = abap_true.
        ro_log = /mbtools/cl_logger=>new( iv_object      = iv_object
                                          iv_subobject   = iv_subobject
                                          iv_description = iv_description ).
      ENDIF.
      RETURN.
    ENDIF.

* Delete all but the last row.  Keep the found_headers table this way
* so we can pass it to BAL_DB_LOAD.
    IF lines( lt_found_headers ) > 1.
      DELETE lt_found_headers TO ( lines( lt_found_headers ) - 1 ).
    ENDIF.
    READ TABLE lt_found_headers INDEX 1 INTO ls_most_recent_header.

    CREATE OBJECT lo_log.
    lo_log->/mbtools/if_logger~mv_db_number = ls_most_recent_header-lognumber.
    lo_log->/mbtools/if_logger~mv_handle    = ls_most_recent_header-log_handle.

    IF io_settings IS BOUND.
      lo_log->mo_settings = io_settings.
    ELSE.
      CREATE OBJECT lo_log->mo_settings TYPE /mbtools/cl_logger_settings.
    ENDIF.

    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header = lt_found_headers.

    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = lo_log->/mbtools/if_logger~mv_handle
      IMPORTING
        e_s_log      = lo_log->/mbtools/if_logger~ms_header.

    ro_log = lo_log.

  ENDMETHOD.
ENDCLASS.
