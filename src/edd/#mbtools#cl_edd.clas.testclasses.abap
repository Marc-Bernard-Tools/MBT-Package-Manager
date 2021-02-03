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
      lv_download_url  TYPE string,
      lv_act           TYPE string.

    " MBT Icon Browser
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

    lv_act = lv_version(2).
    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = '1.' ).

    lv_act = lv_description(3).
    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = '<p>' ).

    lv_act = lv_changelog_url(56).
    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = 'https://marcbernardtools.com/downloads/mbt-icon-browser/' ).

    lv_act = lv_changelog(4).
    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = '<h4>' ).

    lv_act = lv_download_url.
    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = '' ).

  ENDMETHOD.

ENDCLASS.
