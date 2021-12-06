CLASS lcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    DATA mo_timer TYPE REF TO /mbtools/cl_timer.

    METHODS:
      setup,
      run_timer FOR TESTING.

ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_timer.
  ENDMETHOD.

  METHOD run_timer.

    CONSTANTS: lc_title TYPE string VALUE 'Total:'.

    DATA:
      lv_result TYPE string,
      lv_regex  TYPE string.

    mo_timer->start( ).

    WAIT UP TO 1 SECONDS.

    lv_result = mo_timer->end( lc_title ).

    lv_regex = |{ lc_title } 1.[0-9][0-9] seconds|.

    FIND REGEX lv_regex IN lv_result.

    cl_aunit_assert=>assert_subrc(
      exp = 0
      act = sy-subrc
      msg = 'Did not return right measurement' ).

  ENDMETHOD.
ENDCLASS.
