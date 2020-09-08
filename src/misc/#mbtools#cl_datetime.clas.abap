CLASS /mbtools/cl_datetime DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* MBT Date Time
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
  PUBLIC SECTION.

    CONSTANTS c_minute_in_seconds TYPE i VALUE 60 ##NO_TEXT.
    CONSTANTS c_hour_in_seconds TYPE i VALUE 3600 ##NO_TEXT.
    CONSTANTS c_day_in_seconds TYPE i VALUE 86400 ##NO_TEXT.
    CONSTANTS c_week_in_seconds TYPE i VALUE 604800 ##NO_TEXT.
    CONSTANTS c_month_in_seconds TYPE i VALUE 2592000 ##NO_TEXT.
    CONSTANTS c_year_in_seconds TYPE i VALUE 31536000 ##NO_TEXT.
    CONSTANTS c_week_in_days TYPE i VALUE 7 ##NEEDED ##NO_TEXT.
    CONSTANTS c_month_in_days TYPE i VALUE 30 ##NEEDED ##NO_TEXT.
    CONSTANTS c_year_in_days TYPE i VALUE 365 ##NEEDED ##NO_TEXT.

    CLASS-METHODS human_date_diff
      IMPORTING
        !iv_from         TYPE d
        !iv_to           TYPE d OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE string .
    CLASS-METHODS human_time_diff
      IMPORTING
        !iv_from         TYPE timestamp
        !iv_to           TYPE timestamp OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE string .
    CLASS-METHODS get_long_date
      IMPORTING
        !iv_date         TYPE d
      RETURNING
        VALUE(rv_result) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS print
      IMPORTING
        !iv_single       TYPE string
        !iv_plural       TYPE string
        !iv_number       TYPE numeric
      RETURNING
        VALUE(rv_result) TYPE string .
ENDCLASS.



CLASS /MBTOOLS/CL_DATETIME IMPLEMENTATION.


  METHOD get_long_date.

    DATA:
      ls_month_name  TYPE t247,
      lt_month_names TYPE TABLE OF t247.

    CALL FUNCTION 'MONTH_NAMES_GET'
      EXPORTING
        language    = sy-langu
      TABLES
        month_names = lt_month_names.

    READ TABLE lt_month_names INTO ls_month_name INDEX ( iv_date+4(2) ).
    CHECK sy-subrc = 0.

    CONCATENATE ls_month_name-ltx iv_date+6(2) INTO rv_result SEPARATED BY space.
    CONCATENATE rv_result ',' INTO rv_result.
    CONCATENATE rv_result iv_date(4) INTO rv_result SEPARATED BY space.

  ENDMETHOD.


  METHOD human_date_diff.

    DATA: lv_from TYPE timestamp,
          lv_to   TYPE timestamp.

    lv_from = iv_from && '000000'.

    IF iv_to IS INITIAL.
      lv_to = sy-datum && '000000'.
    ELSE.
      lv_to = iv_to && '000000'.
    ENDIF.

    rv_result = human_time_diff( iv_from = lv_from
                                 iv_to   = lv_to ).
  ENDMETHOD.


  METHOD human_time_diff.

    DATA: lv_to   TYPE timestamp,
          lv_diff TYPE i,
          lv_val  TYPE i.

    IF iv_to IS INITIAL.
      GET TIME STAMP FIELD lv_to.
    ELSE.
      lv_to = iv_to.
    ENDIF.

    lv_diff = cl_abap_timestamp_util=>get_instance( )->tstmp_seconds_between( iv_timestamp0 = iv_from
                                                                              iv_timestamp1 = lv_to ).

    IF lv_diff < c_minute_in_seconds.
      lv_val = lv_diff.
      IF lv_val <= 1.
        lv_val = 1.
      ENDIF.
      rv_result = print( iv_single = 'second'
                         iv_plural = 'seconds'
                         iv_number  = lv_val ).
    ELSEIF lv_diff < c_hour_in_seconds AND lv_diff >= c_minute_in_seconds.
      lv_val = lv_diff / c_minute_in_seconds.
      IF ( lv_val <= 1 ).
        lv_val = 1.
      ENDIF.
      rv_result = print( iv_single = 'minute'
                         iv_plural = 'minutes'
                         iv_number  = lv_val ).
    ELSEIF lv_diff < c_day_in_seconds AND lv_diff >= c_hour_in_seconds.
      lv_val = lv_diff / c_hour_in_seconds.
      IF ( lv_val <= 1 ).
        lv_val = 1.
      ENDIF.
      rv_result = print( iv_single = 'hour'
                         iv_plural = 'hours'
                         iv_number  = lv_val ).
    ELSEIF lv_diff < c_week_in_seconds AND lv_diff >= c_day_in_seconds.
      lv_val = lv_diff / c_day_in_seconds.
      IF ( lv_val <= 1 ).
        lv_val = 1.
      ENDIF.
      rv_result = print( iv_single = 'day'
                         iv_plural = 'days'
                         iv_number  = lv_val ).
    ELSEIF lv_diff < c_month_in_seconds AND lv_diff >= c_week_in_seconds.
      lv_val = lv_diff / c_week_in_seconds.
      IF ( lv_val <= 1 ).
        lv_val = 1.
      ENDIF.
      rv_result = print( iv_single = 'week'
                         iv_plural = 'weeks'
                         iv_number  = lv_val ).
    ELSEIF lv_diff < c_year_in_seconds AND lv_diff >= c_month_in_seconds.
      lv_val = lv_diff / c_month_in_seconds.
      IF ( lv_val <= 1 ).
        lv_val = 1.
      ENDIF.
      rv_result = print( iv_single = 'month'
                         iv_plural = 'months'
                         iv_number  = lv_val ).
    ELSEIF lv_diff >= c_year_in_seconds.
      lv_val = lv_diff / c_year_in_seconds.
      IF ( lv_val <= 1 ).
        lv_val = 1.
      ENDIF.
      rv_result = print( iv_single = 'year'
                         iv_plural = 'years'
                         iv_number  = lv_val ).
    ENDIF.

  ENDMETHOD.


  METHOD print.

    data: lv_number TYPE c LENGTH 40.

    WRITE iv_number TO lv_number LEFT-JUSTIFIED.

    IF iv_number = 1.
      rv_result = |{ lv_number } { iv_single }|.
    ELSE.
      rv_result = |{ lv_number } { iv_plural }|.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
