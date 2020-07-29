CLASS /mbtools/cl_progress DEFINITION
  PUBLIC
  FINAL
  CREATE PROTECTED .
************************************************************************
* MBT Progress Indicator
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************

  PUBLIC SECTION.

    INTERFACES /mbtools/if_progress .

    CLASS-METHODS set_instance
      IMPORTING
        !ii_progress TYPE REF TO /mbtools/if_progress .
    CLASS-METHODS get_instance
      IMPORTING
        !iv_total          TYPE i
      RETURNING
        VALUE(ri_progress) TYPE REF TO /mbtools/if_progress .
  PROTECTED SECTION.

    DATA mv_total TYPE i .
    CLASS-DATA gi_progress TYPE REF TO /mbtools/if_progress .

    METHODS calc_pct
      IMPORTING
        !iv_current   TYPE i
      RETURNING
        VALUE(rv_pct) TYPE i .
  PRIVATE SECTION.

    DATA mv_cv_time_next TYPE sy-uzeit .
    DATA mv_cv_datum_next TYPE sy-datum .
ENDCLASS.



CLASS /MBTOOLS/CL_PROGRESS IMPLEMENTATION.


  METHOD /mbtools/if_progress~set_total.

    IF iv_total > 0.
      mv_total = iv_total.
    ELSE.
      mv_total = 100.
    ENDIF.

    CLEAR mv_cv_time_next.
    CLEAR mv_cv_datum_next.

  ENDMETHOD.


  METHOD /mbtools/if_progress~show.

    DATA: lv_pct  TYPE i,
          lv_time TYPE t.

    CONSTANTS: lc_wait_secs TYPE i VALUE 2.

    GET TIME.
    lv_time = sy-uzeit.
    IF mv_cv_time_next IS INITIAL AND mv_cv_datum_next IS INITIAL.
      mv_cv_time_next  = lv_time.
      mv_cv_datum_next = sy-datum.
    ENDIF.

    "We only do a progress indication if enough time has passed
    IF lv_time >= mv_cv_time_next
        AND sy-datum = mv_cv_datum_next
        OR sy-datum > mv_cv_datum_next.

      lv_pct = calc_pct( iv_current ).

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = lv_pct
          text       = iv_text.

      mv_cv_time_next = lv_time + lc_wait_secs.

    ENDIF.
    IF sy-datum > mv_cv_datum_next.
      mv_cv_datum_next = sy-datum.
    ENDIF.
    IF mv_cv_time_next < lv_time.
      mv_cv_datum_next = sy-datum + 1.
    ENDIF.

  ENDMETHOD.


  METHOD calc_pct.

    DATA: lv_f TYPE f.

    lv_f = ( iv_current / mv_total ) * 100.
    rv_pct = lv_f.

    IF rv_pct = 100.
      rv_pct = 99.
    ELSEIF rv_pct = 0.
      rv_pct = 1.
    ENDIF.

  ENDMETHOD.


  METHOD get_instance.

* max one progress indicator at a time is supported

    IF gi_progress IS INITIAL.
      CREATE OBJECT gi_progress TYPE /mbtools/cl_progress.
    ENDIF.

    gi_progress->set_total( iv_total ).

    ri_progress = gi_progress.

  ENDMETHOD.


  METHOD set_instance.

    gi_progress = ii_progress.

  ENDMETHOD.
ENDCLASS.
