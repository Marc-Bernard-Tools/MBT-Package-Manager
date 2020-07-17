CLASS ltcl_edd DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      get_version FOR TESTING
        RAISING /mbtools/cx_exception.

ENDCLASS.

CLASS ltcl_edd IMPLEMENTATION.

  METHOD get_version.

    DATA:
      lv_valid  TYPE abap_bool,
      lv_expire TYPE d.

    /mbtools/cl_edd=>get_version(
      EXPORTING
        iv_id = ''
        iv_license = ''
      IMPORTING
        ev_valid = lv_valid
        ev_expire = lv_expire ).

    cl_abap_unit_assert=>assert_equals(
      act = ''
      exp = '' ).

  ENDMETHOD.

ENDCLASS.
