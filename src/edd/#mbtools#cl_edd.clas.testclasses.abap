CLASS ltcl_edd DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      get_version FOR TESTING
        RAISING /mbtools/cx_exception.

ENDCLASS.

CLASS ltcl_edd IMPLEMENTATION.

  METHOD get_version.

    DATA:
      lv_version   TYPE string,
      lv_changelog TYPE string,
      lv_download  TYPE string.

    /mbtools/cl_edd=>get_version(
      EXPORTING
        iv_id = ''
        iv_license = ''
      IMPORTING
        ev_version = lv_version
        ev_changelog = lv_changelog
        ev_download  = lv_download ).

    cl_abap_unit_assert=>assert_equals(
      act = ''
      exp = '' ).

  ENDMETHOD.

ENDCLASS.
