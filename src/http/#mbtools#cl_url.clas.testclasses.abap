CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      repo_host FOR TESTING RAISING /mbtools/cx_exception,
      repo_name1 FOR TESTING RAISING /mbtools/cx_exception,
      repo_name2 FOR TESTING RAISING /mbtools/cx_exception,
      repo_name3 FOR TESTING RAISING /mbtools/cx_exception,
      repo_name4 FOR TESTING RAISING /mbtools/cx_exception,
      repo_name5 FOR TESTING RAISING /mbtools/cx_exception,
      repo_error FOR TESTING,
      url_validate1 FOR TESTING,
      url_validate2 FOR TESTING,
      url_validate3 FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD repo_error.

    TRY.
        /mbtools/cl_url=>host( 'not a real url' ).
        cl_abap_unit_assert=>fail( ).
      CATCH /mbtools/cx_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD repo_host.

    DATA: lv_host TYPE string.

    lv_host = /mbtools/cl_url=>host( 'https://github.com/larshp/Foobar.git' ).

    cl_abap_unit_assert=>assert_equals(
        exp = 'https://github.com'
        act = lv_host ).

  ENDMETHOD.

  METHOD repo_name1.

    DATA: lv_name TYPE string.

    lv_name = /mbtools/cl_url=>name( 'https://github.com/larshp/Foobar.git' ).

    cl_abap_unit_assert=>assert_equals(
        exp = 'Foobar.git'
        act = lv_name ).

  ENDMETHOD.

  METHOD repo_name2.

    DATA: lv_name TYPE string.

    lv_name = /mbtools/cl_url=>name( 'https://git.hanatrial.ondemand.com/p12345trial/yay' ).

    cl_abap_unit_assert=>assert_equals(
        exp = 'yay'
        act = lv_name ).

  ENDMETHOD.

  METHOD repo_name3.

    DATA: lv_name TYPE string.

    lv_name = /mbtools/cl_url=>name( 'https://github.com/larshp/Foobar/' ).

    cl_abap_unit_assert=>assert_equals(
        exp = 'Foobar'
        act = lv_name ).

  ENDMETHOD.

  METHOD repo_name4.

    DATA: lv_name TYPE string.

    lv_name = /mbtools/cl_url=>name( 'https://github.com/larshp/foo-bar/' ).

    cl_abap_unit_assert=>assert_equals(
        exp = 'foo-bar'
        act = lv_name ).

  ENDMETHOD.

  METHOD repo_name5.

    DATA: lv_name TYPE string.

    lv_name = /mbtools/cl_url=>name( 'https://github.com/larshp/foo_bar/' ).

    cl_abap_unit_assert=>assert_equals(
        exp = 'foo_bar'
        act = lv_name ).

  ENDMETHOD.

  METHOD url_validate1.

    TRY.
        /mbtools/cl_url=>validate( 'http://github.com/larshp/Foobar.git' ).
      CATCH /mbtools/cx_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD url_validate2.

    TRY.
        /mbtools/cl_url=>validate( 'https://github.com/larshp/Foobar.git' ).
      CATCH /mbtools/cx_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD url_validate3.

    TRY.
        /mbtools/cl_url=>validate( 'XYZ://github.com/larshp/Foobar.git' ).
        cl_abap_unit_assert=>fail( ).
      CATCH /mbtools/cx_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


ENDCLASS.
