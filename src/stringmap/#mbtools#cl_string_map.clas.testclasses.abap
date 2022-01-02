CLASS ltcl_string_map DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_struc,
        a TYPE string,
        b TYPE abap_bool,
        c TYPE i,
      END OF ty_struc.

    METHODS get_set_has FOR TESTING.
    METHODS size_empty_clear FOR TESTING.
    METHODS delete FOR TESTING.
    METHODS keys_values FOR TESTING.
    METHODS case_insensitive FOR TESTING.
    METHODS set_clike FOR TESTING.

    METHODS strict FOR TESTING.
    METHODS freeze FOR TESTING.

    METHODS from_struc FOR TESTING.
    METHODS from_to_struc_negative FOR TESTING.
    METHODS from_entries FOR TESTING.
    METHODS from_string FOR TESTING.
    METHODS from_map FOR TESTING.

    METHODS to_struc FOR TESTING.
    METHODS to_string FOR TESTING.

    METHODS create_from FOR TESTING.
    METHODS case_insensitive_create FOR TESTING.

ENDCLASS.

CLASS ltcl_string_map IMPLEMENTATION.

  METHOD create_from.

    DATA lx_e TYPE REF TO cx_root.
    DATA lo_src TYPE REF TO /mbtools/cl_string_map.
    DATA lo_cut TYPE REF TO /mbtools/cl_string_map.
    DATA: BEGIN OF ls_dummy, a TYPE string VALUE '1', END OF ls_dummy.

    lo_src = /mbtools/cl_string_map=>create( ).
    lo_src->set(
      iv_key = 'A'
      iv_val = '1' ).

    TRY.
        /mbtools/cl_string_map=>create( 12345 ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'Incorrect input for string_map=>create, typekind I'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.
        /mbtools/cl_string_map=>create( me ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'Incorrect string map instance to copy from'
          act = lx_e->get_text( ) ).
    ENDTRY.

    " From obj
    lo_cut = /mbtools/cl_string_map=>create( lo_src ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lo_cut->size( ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = '1'
      act = lo_cut->get( 'A' ) ).

    " From tab
    lo_cut = /mbtools/cl_string_map=>create( lo_src->mt_entries ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lo_cut->size( ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = '1'
      act = lo_cut->get( 'A' ) ).

    " From struc
    lo_cut = /mbtools/cl_string_map=>create( ls_dummy ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lo_cut->size( ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = '1'
      act = lo_cut->get( 'A' ) ).

    " From string
    lo_cut = /mbtools/cl_string_map=>create( 'x=y' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'x' )
      exp = 'y' ).

    " From another map
    lo_cut = /mbtools/cl_string_map=>create( /mbtools/cl_string_map=>create( 'x=1' ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'x' )
      exp = '1' ).

  ENDMETHOD.

  METHOD freeze.

    DATA lt_entries TYPE /mbtools/cl_string_map=>tty_entries.
    DATA ls_dummy TYPE syst.
    DATA lx_e TYPE REF TO cx_root.
    DATA lo_cut TYPE REF TO /mbtools/cl_string_map.
    FIELD-SYMBOLS <l> LIKE LINE OF lt_entries.

    lo_cut = /mbtools/cl_string_map=>create( ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' )->freeze( ).

    TRY.
        lo_cut->set(
          iv_key = 'A'
          iv_val = '2' ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'String map is read only'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.
        lo_cut->set(
          iv_key = 'B'
          iv_val = '2' ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'String map is read only'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.
        lo_cut->delete( 'A' ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'String map is read only'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.
        lo_cut->clear( ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'String map is read only'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.

        APPEND INITIAL LINE TO lt_entries ASSIGNING <l>.
        <l>-k = 'a'.
        lo_cut->from_entries( lt_entries ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'String map is read only'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.
        lo_cut->from_struc( ls_dummy ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'String map is read only'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.
        lo_cut->from_string( 'x=y' ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'String map is read only'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.
        lo_cut->from_map( /mbtools/cl_string_map=>create( iv_from = 'x=y' ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'String map is read only'
          act = lx_e->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD get_set_has.

    DATA lo_cut TYPE REF TO /mbtools/cl_string_map.
    lo_cut = /mbtools/cl_string_map=>create( ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = 'B'
      iv_val = 'bvalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'A' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'bvalue'
      act = lo_cut->get( 'B' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = ''
      act = lo_cut->get( 'C' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = lo_cut->has( 'C' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = lo_cut->has( 'A' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = lo_cut->has( 'a' ) ). " case sensitive

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'newvalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'newvalue'
      act = lo_cut->get( 'A' ) ).

  ENDMETHOD.

  METHOD size_empty_clear.

    DATA lo_cut TYPE REF TO /mbtools/cl_string_map.
    lo_cut = /mbtools/cl_string_map=>create( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lo_cut->size( ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = lo_cut->is_empty( ) ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lo_cut->size( ) ).

    lo_cut->set(
      iv_key = 'B'
      iv_val = 'bvalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lo_cut->size( ) ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'newvalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lo_cut->size( ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = lo_cut->is_empty( ) ).

    lo_cut->clear( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lo_cut->size( ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = lo_cut->is_empty( ) ).

  ENDMETHOD.

  METHOD delete.

    DATA lo_cut TYPE REF TO /mbtools/cl_string_map.
    lo_cut = /mbtools/cl_string_map=>create( ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'A' ) ).

    lo_cut->delete( 'A' ).

    cl_abap_unit_assert=>assert_equals(
      exp = ''
      act = lo_cut->get( 'A' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = lo_cut->has( 'A' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lo_cut->size( ) ).

  ENDMETHOD.

  METHOD keys_values.

    DATA lo_cut TYPE REF TO /mbtools/cl_string_map.
    DATA lt_exp TYPE string_table.
    lo_cut = /mbtools/cl_string_map=>create( ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = 'B'
      iv_val = 'bvalue' ).

    CLEAR lt_exp.
    APPEND 'A' TO lt_exp.
    APPEND 'B' TO lt_exp.

    cl_abap_unit_assert=>assert_equals(
      exp = lt_exp
      act = lo_cut->keys( ) ).

    CLEAR lt_exp.
    APPEND 'avalue' TO lt_exp.
    APPEND 'bvalue' TO lt_exp.

    cl_abap_unit_assert=>assert_equals(
      exp = lt_exp
      act = lo_cut->values( ) ).

  ENDMETHOD.

  METHOD to_struc.

    DATA ls_struc_act TYPE ty_struc.
    DATA ls_struc_exp TYPE ty_struc.
    DATA lo_cut TYPE REF TO /mbtools/cl_string_map.
    lo_cut = /mbtools/cl_string_map=>create( ).

    lo_cut->set(
      iv_key = 'a'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = 'b'
      iv_val = 'X' ).
    lo_cut->set(
      iv_key = 'c'
      iv_val = '123' ).

    lo_cut->to_struc( CHANGING cs_container = ls_struc_act ).

    ls_struc_exp-a = 'avalue'.
    ls_struc_exp-b = abap_true.
    ls_struc_exp-c = 123.

    cl_abap_unit_assert=>assert_equals(
      exp = ls_struc_exp
      act = ls_struc_act ).

  ENDMETHOD.

  METHOD from_struc.

    DATA ls_struc TYPE ty_struc.
    DATA lo_cut TYPE REF TO /mbtools/cl_string_map.
    lo_cut = /mbtools/cl_string_map=>create( ).

    ls_struc-a = 'avalue'.
    ls_struc-b = abap_true.
    ls_struc-c = 123.

    lo_cut->set(
      iv_key = 'z'
      iv_val = 'xyz' ).

    lo_cut = lo_cut->from_struc( ls_struc ).

    cl_abap_unit_assert=>assert_equals(
      exp = 4
      act = lo_cut->size( ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'A' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'X'
      act = lo_cut->get( 'B' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = '123'
      act = lo_cut->get( 'C' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'xyz'
      act = lo_cut->get( 'z' ) ).

  ENDMETHOD.

  METHOD strict.

    DATA ls_struc_act TYPE ty_struc.
    DATA ls_struc_exp TYPE ty_struc.
    DATA lx_e TYPE REF TO cx_root.
    DATA lo_struc TYPE REF TO /mbtools/cl_string_map.
    DATA lo_cut TYPE REF TO /mbtools/cl_string_map.
    lo_cut = /mbtools/cl_string_map=>create( ).

    lo_cut->set(
      iv_key = 'a'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = 'b'
      iv_val = 'X' ).
    lo_cut->set(
      iv_key = 'c'
      iv_val = '123' ).
    lo_cut->set(
      iv_key = 'z'
      iv_val = 'xyz' ).

    ls_struc_exp-a = 'avalue'.
    ls_struc_exp-b = abap_true.
    ls_struc_exp-c = 123.

    TRY.
        lo_cut->to_struc( CHANGING cs_container = ls_struc_act ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'Component Z not found in target'
          act = lx_e->get_text( ) ).
    ENDTRY.

    lo_struc = lo_cut->strict( abap_false ).
    lo_struc->to_struc( CHANGING cs_container = ls_struc_act ).

    cl_abap_unit_assert=>assert_equals(
      exp = ls_struc_exp
      act = ls_struc_act ).

  ENDMETHOD.

  METHOD from_to_struc_negative.

    DATA lt_dummy TYPE string_table.
    DATA lx_e TYPE REF TO cx_root.
    DATA lo_cut TYPE REF TO /mbtools/cl_string_map.
    lo_cut = /mbtools/cl_string_map=>create( ).

    TRY.
        lo_cut->from_struc( lt_dummy ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'Only structures supported'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.
        lo_cut->to_struc( CHANGING cs_container = lt_dummy ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'Only structures supported'
          act = lx_e->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD from_entries.

    TYPES:
      BEGIN OF ty_pair,
        key TYPE string,
        val TYPE string,
      END OF ty_pair.

    DATA lt_entries TYPE TABLE OF ty_pair.
    DATA ls_entry LIKE LINE OF lt_entries.
    DATA lo_cut TYPE REF TO /mbtools/cl_string_map.
    lo_cut = /mbtools/cl_string_map=>create( ).

    ls_entry-key = 'A'.
    ls_entry-val = 'avalue'.
    APPEND ls_entry TO lt_entries.

    ls_entry-key = 'B'.
    ls_entry-val = '123'.
    APPEND ls_entry TO lt_entries.

    lo_cut->set(
      iv_key = 'z'
      iv_val = 'xyz' ).

    lo_cut = lo_cut->from_entries( lt_entries ).

    cl_abap_unit_assert=>assert_equals(
      exp = 3
      act = lo_cut->size( ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'A' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = '123'
      act = lo_cut->get( 'B' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'xyz'
      act = lo_cut->get( 'z' ) ).

  ENDMETHOD.

  METHOD case_insensitive.

    DATA lt_exp_keys TYPE string_table.
    DATA lo_cut TYPE REF TO /mbtools/cl_string_map.
    lo_cut = /mbtools/cl_string_map=>create( iv_case_insensitive = abap_true ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = 'b'
      iv_val = 'bvalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'A' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'a' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'bvalue'
      act = lo_cut->get( 'B' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'bvalue'
      act = lo_cut->get( 'b' ) ).

    cl_abap_unit_assert=>assert_true( lo_cut->has( 'A' ) ).
    cl_abap_unit_assert=>assert_true( lo_cut->has( 'a' ) ).
    cl_abap_unit_assert=>assert_true( lo_cut->has( 'B' ) ).
    cl_abap_unit_assert=>assert_true( lo_cut->has( 'b' ) ).
    cl_abap_unit_assert=>assert_false( lo_cut->has( 'c' ) ).

    APPEND 'A' TO lt_exp_keys.
    APPEND 'B' TO lt_exp_keys.

    cl_abap_unit_assert=>assert_equals(
      exp = lt_exp_keys
      act = lo_cut->keys( ) ).

    lo_cut->delete( 'a' ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lo_cut->size( ) ).

    lo_cut->delete( 'B' ).
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lo_cut->size( ) ).

  ENDMETHOD.

  METHOD case_insensitive_create.

    DATA lo_src TYPE REF TO /mbtools/cl_string_map.
    DATA lo_cut TYPE REF TO /mbtools/cl_string_map.
    DATA lt_exp_keys TYPE string_table.

    lo_src = /mbtools/cl_string_map=>create( ).
    lo_src->set(
      iv_key = 'A'
      iv_val = '1' ).
    lo_src->set(
      iv_key = 'b'
      iv_val = '2' ).
    lo_src->freeze( ).

    lo_cut = /mbtools/cl_string_map=>create(
      iv_from             = lo_src
      iv_case_insensitive = abap_true ).


    APPEND 'A' TO lt_exp_keys.
    APPEND 'B' TO lt_exp_keys.

    cl_abap_unit_assert=>assert_equals(
      exp = lt_exp_keys
      act = lo_cut->keys( ) ).

  ENDMETHOD.

  METHOD set_clike.

    DATA lv_char TYPE c LENGTH 10.
    DATA lv_numc TYPE n LENGTH 4.
    DATA lo_cut TYPE REF TO /mbtools/cl_string_map.
    lo_cut = /mbtools/cl_string_map=>create( ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = `B`
      iv_val = `bvalue` ).

    lv_char = 'C'.
    lo_cut->set(
      iv_key = lv_char
      iv_val = lv_char ).

    lv_numc = '123'.
    lo_cut->set(
      iv_key = lv_numc
      iv_val = lv_numc ).

    cl_abap_unit_assert=>assert_equals(
      exp = 4
      act = lo_cut->size( ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'C'
      act = lo_cut->get( 'C' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = '0123'
      act = lo_cut->get( '0123' ) ).

  ENDMETHOD.

  METHOD from_string.

    DATA lx_e TYPE REF TO lcx_error.
    DATA lo_cut TYPE REF TO /mbtools/cl_string_map.
    lo_cut = /mbtools/cl_string_map=>create( ).

    lo_cut->set(
      iv_key = 'z'
      iv_val = 'xyz' ).

    lo_cut = lo_cut->from_string( 'a = avalue, b = some data, c = space   space' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->size( )
      exp = 4 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'a' )
      exp = 'avalue' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'b' )
      exp = 'some data' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'c' )
      exp = 'space   space' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'z' )
      exp = 'xyz' ).

    TRY.
        lo_cut->from_string( `x=y,  ` ).
      CATCH lcx_error INTO lx_e.
        cl_abap_unit_assert=>assert_char_cp(
          act = lx_e->get_text( )
          exp = 'Empty key*' ).
    ENDTRY.

  ENDMETHOD.

  METHOD to_string.

    DATA lo_cut TYPE REF TO /mbtools/cl_string_map.
    lo_cut = /mbtools/cl_string_map=>create( ).

    lo_cut->from_string( 'a = avalue, b = some data, c = space   space' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->to_string( )
      exp = 'a=avalue,b=some data,c=space   space' ).

  ENDMETHOD.

  METHOD from_map.

    DATA lo_src TYPE REF TO /mbtools/cl_string_map.
    DATA lo_cut TYPE REF TO /mbtools/cl_string_map.

    lo_src = /mbtools/cl_string_map=>create( ).
    lo_src->set(
      iv_key = 'a'
      iv_val = '1' ).
    lo_src->set(
      iv_key = 'b'
      iv_val = '2' ).
    lo_src->freeze( ).

    " Empty map
    lo_cut = /mbtools/cl_string_map=>create( ).
    lo_cut = lo_cut->from_map( lo_src ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->size( )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'a' )
      exp = '1' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'b' )
      exp = '2' ).

    " Existing values + overwrite
    lo_cut = /mbtools/cl_string_map=>create( ).
    lo_cut->set(
      iv_key = 'a'
      iv_val = 'x' ).
    lo_cut->set(
      iv_key = 'c'
      iv_val = '3' ).

    lo_cut->from_map( lo_src ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->size( )
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'a' )
      exp = '1' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'b' )
      exp = '2' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'c' )
      exp = '3' ).

  ENDMETHOD.

ENDCLASS.
