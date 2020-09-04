CLASS ltcl_edd DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      get_version FOR TESTING
        RAISING /mbtools/cx_exception.

ENDCLASS.

CLASS ltcl_edd IMPLEMENTATION.

  METHOD get_version.

    DATA:
      lv_version        TYPE string,
      lv_changelog_url  TYPE string,
      lv_changelog_html TYPE string,
      lv_download_url   TYPE string.

    /mbtools/cl_edd=>get_version(
      EXPORTING
        iv_id             = ''
        iv_license        = ''
      IMPORTING
        ev_version        = lv_version
        ev_changelog_url  = lv_changelog_url
        ev_changelog_html = lv_changelog_html
        ev_download_url   = lv_download_url ).

    cl_abap_unit_assert=>assert_equals(
      act = ''
      exp = '' ).

  ENDMETHOD.

ENDCLASS.
