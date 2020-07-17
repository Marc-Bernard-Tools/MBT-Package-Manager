
CLASS ltcl_html DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS c_newline TYPE abap_char1 VALUE cl_abap_char_utilities=>newline ##NO_TEXT.

    DATA: mo_html TYPE REF TO /mbtools/if_html.

    METHODS:
      indent1 FOR TESTING RAISING /mbtools/cx_exception,
      indent2 FOR TESTING RAISING /mbtools/cx_exception,
      indent3 FOR TESTING RAISING /mbtools/cx_exception,
      indent4 FOR TESTING RAISING /mbtools/cx_exception,
      style1  FOR TESTING RAISING /mbtools/cx_exception.

    METHODS:
      setup.

ENDCLASS.


CLASS ltcl_html IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_html TYPE /mbtools/cl_html.
  ENDMETHOD.

  METHOD indent1.

    DATA lv_exp TYPE string.

    mo_html->add( '<td>' ).
    mo_html->add( 'hello world' ).
    mo_html->add( '</td>' ).

    lv_exp = '<td>' && c_newline &&
             '  hello world' && c_newline &&
             '</td>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.

  METHOD indent2.

    DATA lv_exp TYPE string.

    mo_html->add( '<td>' ).
    mo_html->add( '<input name="comment" type="text">' ).
    mo_html->add( '</td>' ).

    lv_exp = '<td>' && c_newline &&
             '  <input name="comment" type="text">' && c_newline &&
             '</td>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.

  METHOD indent3.

    DATA lv_exp TYPE string.

    mo_html->add( '<td>' ).
    mo_html->add( '<textarea name="body" rows="10" cols="72"></textarea>' ).
    mo_html->add( '</td>' ).

    lv_exp = '<td>' && c_newline &&
             '  <textarea name="body" rows="10" cols="72"></textarea>' && c_newline &&
             '</td>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.

  METHOD indent4.

    DATA lv_exp TYPE string.

    mo_html->add( '<td>' ).
    mo_html->add( 'foo<br>bar' ).
    mo_html->add( '</td>' ).

    lv_exp = '<td>' && c_newline &&
             '  foo<br>bar' && c_newline &&
             '</td>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.

  METHOD style1.

    DATA lv_exp TYPE string.

    mo_html->add( '<style type="text/css">' ).
    mo_html->add( '.class1 { color: red }' ).
    mo_html->add( '.class2 {' ).
    mo_html->add( 'color: red' ).
    mo_html->add( '}' ).
    mo_html->add( '</style>' ).

    lv_exp = '<style type="text/css">' && c_newline &&
             '  .class1 { color: red }' && c_newline &&
             '  .class2 {' && c_newline &&
             '    color: red' && c_newline &&
             '  }' && c_newline &&
             '</style>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.

ENDCLASS.
