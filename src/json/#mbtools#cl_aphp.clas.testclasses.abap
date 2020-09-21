CLASS ltcl_aphp DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.

    METHODS deserialize FOR TESTING.

ENDCLASS.

CLASS /mbtools/cl_aphp DEFINITION LOCAL FRIENDS ltcl_aphp.

CLASS ltcl_aphp IMPLEMENTATION.

  METHOD deserialize.

    DATA:
      lv_php    TYPE string,
      lo_json   TYPE REF TO /mbtools/if_ajson_writer,
      lo_json_r TYPE REF TO /mbtools/cl_ajson,
      lv_exp    TYPE string,
      lv_json   TYPE string.

    TRY.
        lv_php = 's:11:"hello world";'.
        lo_json = /mbtools/cl_aphp=>unserialize( lv_php ).
        lo_json_r ?= lo_json.
        lv_json = lo_json_r->stringify( ).

        cl_abap_unit_assert=>assert_equals(
          act = lv_json
          exp = '{"s":"hello world"}' ).

        " Integer
        lv_php = 'i:26111968;'.
        lo_json = /mbtools/cl_aphp=>unserialize( lv_php ).
        lo_json_r ?= lo_json.
        lv_json = lo_json_r->stringify( ).

        cl_abap_unit_assert=>assert_equals(
          act = lv_json
          exp = '{"i":26111968}' ).

        " Float
        lv_php = 'd:789.34;'.
        lo_json = /mbtools/cl_aphp=>unserialize( lv_php ).
        lo_json_r ?= lo_json.
        lv_json = lo_json_r->stringify( ).

        cl_abap_unit_assert=>assert_equals(
          act = lv_json
          exp = '{"d":789.34000000000003}' ).   " needs rounding in /mbtools/cl_ajson

        " Boolean
        lv_php = 'b:0;'.
        lo_json = /mbtools/cl_aphp=>unserialize( lv_php ).
        lo_json_r ?= lo_json.
        lv_json = lo_json_r->stringify( ).

        cl_abap_unit_assert=>assert_equals(
          act = lv_json
          exp = '{"b":false}' ).

        lv_php = 'b:1;'.
        lo_json = /mbtools/cl_aphp=>unserialize( lv_php ).
        lo_json_r ?= lo_json.
        lv_json = lo_json_r->stringify( ).

        cl_abap_unit_assert=>assert_equals(
          act = lv_json
          exp = '{"b":true}' ).

        " Array (enum, string)
        lv_php = 'a:3:{i:0;s:4:"Math";i:1;s:8:"Language";i:2;s:7:"Science";}'.
        lo_json = /mbtools/cl_aphp=>unserialize( lv_php ).
        lo_json_r ?= lo_json.
        lv_json = lo_json_r->stringify( ).

        cl_abap_unit_assert=>assert_equals(
          act = lv_json
          exp = '{"a":["Math","Language","Science"]}' ).

        " Array (enum, Mixed)
        lv_php = 'a:5:{i:0;s:5:"hello";i:1;i:42;i:2;d:789.34;i:3;s:5:"apple";i:4;b:0;}'.
        lo_json = /mbtools/cl_aphp=>unserialize( lv_php ).
        lo_json_r ?= lo_json.
        lv_json = lo_json_r->stringify( ).

        cl_abap_unit_assert=>assert_equals(
          act = lv_json
          exp = '{"a":["hello",42,789.34000000000003,"apple",false]}' ).

        " Array (string, int)
        lv_php = 'a:3:{s:4:"Math";i:1;s:8:"Language";i:2;s:7:"Science";i:3;}'.
        lo_json = /mbtools/cl_aphp=>unserialize( lv_php ).
        lo_json_r ?= lo_json.
        lv_json = lo_json_r->stringify( ).

        lv_exp = '{"a":[{"key":"Math","val":1},{"key":"Language","val":2},{"key":"Science","val":3}]}'.

        cl_abap_unit_assert=>assert_equals(
          act = lv_json
          exp = lv_exp ).

        " Array (string, mixed)
        lv_php = 'a:3:{s:4:"Math";i:42;s:8:"Language";d:789.34;s:7:"Science";b:1;}'.
        lo_json = /mbtools/cl_aphp=>unserialize( lv_php ).
        lo_json_r ?= lo_json.
        lv_json = lo_json_r->stringify( ).

        lv_exp = '{"a":[{"key":"Math","val":42},{"key":"Language","val":789.34000000000003},'
                 && '{"key":"Science","val":true}]}'.

        cl_abap_unit_assert=>assert_equals(
          act = lv_json
          exp = lv_exp ).

        " Nested array
        lv_php = 'a:4:{i:0;s:5:"hello";i:1;i:42;i:2;a:2:{i:0;i:1;i:1;s:3:"two";};i:3;s:5:"apple";}'.
        lo_json = /mbtools/cl_aphp=>unserialize( lv_php ).
        lo_json_r ?= lo_json.
        lv_json = lo_json_r->stringify( ).

        cl_abap_unit_assert=>assert_equals(
          act = lv_json
          exp = '{"a":["hello",42,"a":[1,"two"],"apple"]}' ).

      CATCH /mbtools/cx_ajson_error.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
