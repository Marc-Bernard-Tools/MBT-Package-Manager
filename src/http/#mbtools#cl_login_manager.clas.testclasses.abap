CLASS ltcl_login_manager DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    CONSTANTS: c_username TYPE string VALUE 'Aladdin' ##NO_TEXT,
               c_password TYPE string VALUE 'OpenSesame' ##NO_TEXT.

    METHODS:
      setup,
      teardown,
      encoding FOR TESTING
        RAISING zcx_abapgit_exception,
      same_server FOR TESTING
        RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_login_manager IMPLEMENTATION.

  METHOD setup.
    /mbtools/cl_login_manager=>clear( ).
  ENDMETHOD.

  METHOD teardown.
    /mbtools/cl_login_manager=>clear( ).
  ENDMETHOD.

  METHOD encoding.

    DATA: lv_auth TYPE string.

    TRY.
        lv_auth = /mbtools/cl_login_manager=>set(
          iv_uri      = 'https://github.com/larshp/abapGit.git'
          iv_username = c_username
          iv_password = c_password ).

        cl_abap_unit_assert=>assert_equals(
          act = lv_auth
          exp = 'Basic QWxhZGRpbjpPcGVuU2VzYW1l' ).
      CATCH /mbtools/cx_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD same_server.

    CONSTANTS: lc_github1 TYPE string VALUE 'https://github.com/larshp/abapGit.git',
               lc_github2 TYPE string VALUE 'https://github.com/larshp/Foobar.git'.

    DATA: lv_auth1 TYPE string,
          lv_auth2 TYPE string.

    TRY.
        /mbtools/cl_login_manager=>set(
          iv_uri      = lc_github1
          iv_username = c_username
          iv_password = c_password ).

        lv_auth1 = /mbtools/cl_login_manager=>load( lc_github1 ).
        lv_auth2 = /mbtools/cl_login_manager=>load( lc_github2 ).

        cl_abap_unit_assert=>assert_equals(
          act = lv_auth1
          exp = lv_auth2 ).
      CATCH /mbtools/cx_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
