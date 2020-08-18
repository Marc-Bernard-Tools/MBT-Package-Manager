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
* Last update: 2020-08-17
************************************************************************
  PUBLIC SECTION.

    "! Starts a new log.
    CLASS-METHODS create_log
      IMPORTING
        !iv_object      TYPE csequence DEFAULT '/MBTOOLS/'
        !iv_subobject   TYPE csequence
        !iv_description TYPE csequence OPTIONAL
        !iv_context     TYPE simple OPTIONAL
        !io_settings    TYPE REF TO /mbtools/if_logger_settings OPTIONAL
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
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /MBTOOLS/CL_LOGGER_FACTORY IMPLEMENTATION.


  METHOD create_log.

    DATA: lo_log TYPE REF TO /mbtools/cl_logger.
    FIELD-SYMBOLS <context_val> TYPE c.

    CREATE OBJECT lo_log.
    lo_log->header-object    = iv_object.
    lo_log->header-subobject = iv_subobject.
    lo_log->header-extnumber = iv_description.

    IF io_settings IS BOUND.
      lo_log->mo_settings = io_settings.
    ELSE.
      CREATE OBJECT lo_log->mo_settings TYPE /mbtools/cl_logger_settings.
    ENDIF.

* Special case: Logger can work without object - but then
* the data cannot be written to the database.
    IF iv_object IS INITIAL.
      lo_log->mo_settings->set_autosave( abap_false ).
    ENDIF.

* Use secondary database connection to write data to database even if
* main program does a rollback (e. g. during a dump).
    IF lo_log->mo_settings->get_usage_of_secondary_db_conn( ) = abap_true.
      lo_log->mv_sec_connection     = abap_true.
      lo_log->mv_sec_connect_commit = abap_true.
    ENDIF.

* Set deletion date and set if log can be deleted before deletion date is reached.
    lo_log->header-aldate_del = lo_log->mo_settings->get_expiry_date( ).
    lo_log->header-del_before = lo_log->mo_settings->get_must_be_kept_until_expiry( ).

    IF iv_context IS SUPPLIED AND iv_context IS NOT INITIAL.
      lo_log->header-context-tabname =
        cl_abap_typedescr=>describe_by_data( iv_context )->get_ddic_header( )-tabname.
      ASSIGN iv_context TO <context_val> CASTING.
      lo_log->header-context-value = <context_val>.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = lo_log->header
      IMPORTING
        e_log_handle = lo_log->handle.

* BAL_LOG_CREATE will fill in some additional header data.
* This FM updates our instance attribute to reflect that.
    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = lo_log->handle
      IMPORTING
        e_s_log      = lo_log->header.

    ro_log = lo_log.

  ENDMETHOD.


  METHOD create_settings.

    CREATE OBJECT ro_settings TYPE /mbtools/cl_logger_settings.

  ENDMETHOD.


  METHOD open_log.

    DATA: filter             TYPE bal_s_lfil,
          desc_filter        TYPE bal_s_extn,
          obj_filter         TYPE bal_s_obj,
          subobj_filter      TYPE bal_s_sub,

          found_headers      TYPE balhdr_t,
          most_recent_header TYPE balhdr,
          handles_loaded     TYPE bal_t_logh.
    DATA: lo_log             TYPE REF TO /mbtools/cl_logger.

    desc_filter-option = subobj_filter-option = obj_filter-option = 'EQ'.
    desc_filter-sign   = subobj_filter-sign = obj_filter-sign = 'I'.

    obj_filter-low = iv_object.
    APPEND obj_filter TO filter-object.
    subobj_filter-low = iv_subobject.
    APPEND subobj_filter TO filter-subobject.
    IF iv_description IS SUPPLIED.
      desc_filter-low = iv_description.
      APPEND desc_filter TO filter-extnumber.
    ENDIF.

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_s_log_filter = filter
      IMPORTING
        e_t_log_header = found_headers
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
    IF lines( found_headers ) > 1.
      DELETE found_headers TO ( lines( found_headers ) - 1 ).
    ENDIF.
    READ TABLE found_headers INDEX 1 INTO most_recent_header.

    CREATE OBJECT lo_log.
    lo_log->db_number = most_recent_header-lognumber.
    lo_log->handle    = most_recent_header-log_handle.

    IF io_settings IS BOUND.
      lo_log->mo_settings = io_settings.
    ELSE.
      CREATE OBJECT lo_log->mo_settings TYPE /mbtools/cl_logger_settings.
    ENDIF.

    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header = found_headers.

    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = lo_log->handle
      IMPORTING
        e_s_log      = lo_log->header.

    ro_log = lo_log.

  ENDMETHOD.
ENDCLASS.
