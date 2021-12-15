CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      sha1 FOR TESTING RAISING /mbtools/cx_exception,
      sha1_raw_valid FOR TESTING RAISING /mbtools/cx_exception,
      sha1_raw_invalid FOR TESTING RAISING /mbtools/cx_exception.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD sha1.

    DATA: lv_sha1 TYPE /mbtools/if_definitions=>ty_sha1.

    lv_sha1 = /mbtools/cl_hash=>sha1(
      iv_type = 'commit'
      iv_data = '112211221122' ) ##LITERAL.

    cl_abap_unit_assert=>assert_equals(
      act = lv_sha1
      exp = 'af2261a340c5188baf86a64a581d22012303023c' ).

  ENDMETHOD.


  METHOD sha1_raw_valid.

    DATA: lv_sha1  TYPE /mbtools/if_definitions=>ty_sha1,
          lv_input TYPE xstring.

    lv_input = 'C5188BAF86A64A581D2201' ##LITERAL.
    lv_sha1 = /mbtools/cl_hash=>sha1_raw( lv_input ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_sha1
      exp = '0ec2eba75071f87988ced3237cae5ec7c5efd795' ).

  ENDMETHOD.

  METHOD sha1_raw_invalid.

    DATA: lv_sha1  TYPE /mbtools/if_definitions=>ty_sha1,
          lv_input TYPE xstring.

    lv_input = 'LOREM_IPSUM' ##LITERAL.
    lv_sha1 = /mbtools/cl_hash=>sha1_raw( lv_input ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_sha1
      exp = 'da39a3ee5e6b4b0d3255bfef95601890afd80709' ).

  ENDMETHOD.

ENDCLASS.
