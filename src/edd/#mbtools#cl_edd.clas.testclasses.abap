CLASS ltcl_edd DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      get_version FOR TESTING
        RAISING /mbtools/cx_exception,
      get_versions FOR TESTING
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
        iv_id            = '4413'
        iv_license       = ''
      IMPORTING
        ev_version       = lv_version
        ev_description   = lv_description
        ev_changelog_url = lv_changelog_url
        ev_changelog     = lv_changelog
        ev_download_url  = lv_download_url ).

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

    " no license, no download URL
    lv_act = lv_download_url.
    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = '' ).

  ENDMETHOD.

  METHOD get_versions.

    DATA:
      ls_product  TYPE /mbtools/cl_edd=>ty_product,
      lt_products TYPE /mbtools/cl_edd=>ty_products,
      lv_act      TYPE string.

    ls_product-id = '4873'. " MBT Base
    INSERT ls_product INTO TABLE lt_products.
    ls_product-id = '4413'. " MBT Icon Browser
    INSERT ls_product INTO TABLE lt_products.
    ls_product-id = '5227'. " MBT Listcube
    INSERT ls_product INTO TABLE lt_products.
    ls_product-id = '3635'. " MBT LOL
    INSERT ls_product INTO TABLE lt_products.
    ls_product-id = '4409'. " MBT Command Field
    INSERT ls_product INTO TABLE lt_products.
    ls_product-id = '5142'. " MBT Note Assistant
    INSERT ls_product INTO TABLE lt_products.
    ls_product-id = '4411'. " MBT Transport Request
    INSERT ls_product INTO TABLE lt_products.
    ls_product-id = '4845'. " MBT System Monitor
    INSERT ls_product INTO TABLE lt_products.
    ls_product-id = '5426'. " MBT Object Duplicator
    INSERT ls_product INTO TABLE lt_products.

    /mbtools/cl_edd=>get_versions( CHANGING ct_products = lt_products ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_products )
      exp = 9 ).

    " Check results for one product (MBT Icon Browser)
    READ TABLE lt_products INTO ls_product WITH KEY id = '4413'.
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    lv_act = ls_product-version(2).
    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = '1.' ).

    lv_act = ls_product-description(3).
    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = '<p>' ).

    lv_act = ls_product-changelog_url(56).
    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = 'https://marcbernardtools.com/downloads/mbt-icon-browser/' ).

    lv_act = ls_product-changelog(4).
    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = '<h4>' ).

    " no license, no download URL
    lv_act = ls_product-download_url.
    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = '' ).

  ENDMETHOD.

ENDCLASS.
