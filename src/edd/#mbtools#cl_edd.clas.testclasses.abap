CLASS ltcl_edd DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      get_version FOR TESTING
        RAISING /mbtools/cx_exception.

ENDCLASS.

CLASS ltcl_edd IMPLEMENTATION.

  METHOD get_version.

    DATA:
      lv_version       TYPE string,
      lv_description   TYPE string,
      lv_changelog_url TYPE string,
      lv_changelog     TYPE string,
      lv_download_url  TYPE string.

    /mbtools/cl_edd=>get_version(
      EXPORTING
        iv_id             = '4413'
        iv_license        = ''
      IMPORTING
        ev_version        = lv_version
        ev_description    = lv_description
        ev_changelog_url  = lv_changelog_url
        ev_changelog      = lv_changelog
        ev_download_url   = lv_download_url ).

    cl_abap_unit_assert=>assert_equals(
      act = ''
      exp = '' ).

  ENDMETHOD.

ENDCLASS.
