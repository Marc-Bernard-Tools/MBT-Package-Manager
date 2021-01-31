CLASS /mbtools/cl_switches DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* MBT Switches
*
* WARNING: DO NOT CHANGE!
* Syntax errors in this class may impact system stability
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_tool,
        mbt_base              TYPE string VALUE 'MBT Base' ##NO_TEXT,
        mbt_command_field     TYPE string VALUE 'MBT Command Field' ##NO_TEXT,
        mbt_transport_request TYPE string VALUE 'MBT Transport Request' ##NO_TEXT,
        mbt_note_assistant    TYPE string VALUE 'MBT Note Assistant' ##NO_TEXT,
        mbt_system_monitor    TYPE string VALUE 'MBT System Monitor' ##NO_TEXT,
        mbt_listcube          TYPE string VALUE 'MBT Listcube' ##NO_TEXT,
      END OF c_tool.

    CLASS-METHODS class_constructor.
    CLASS-METHODS is_active
      IMPORTING
        !iv_title        TYPE string
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS is_debug
      IMPORTING
        !iv_title        TYPE string
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS is_trace
      IMPORTING
        !iv_title        TYPE string
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_reg,
        " Registry Switches
        switches   TYPE string VALUE 'Switches' ##NO_TEXT,
        key_active TYPE string VALUE 'Active' ##NO_TEXT,
        key_debug  TYPE string VALUE 'Debug' ##NO_TEXT,
        key_trace  TYPE string VALUE 'Trace' ##NO_TEXT,
      END OF c_reg.

    CLASS-DATA go_reg_root TYPE REF TO /mbtools/cl_registry.

    CLASS-METHODS check_switch
      IMPORTING
        !iv_title        TYPE string
        !iv_key          TYPE string
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
ENDCLASS.



CLASS /mbtools/cl_switches IMPLEMENTATION.


  METHOD check_switch.

    DATA:
      lv_name      TYPE string,
      ls_bundle    TYPE /mbtools/cl_registry=>ty_keyobj,
      lt_bundles   TYPE /mbtools/cl_registry=>ty_keyobjs,
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry,
      lt_users     TYPE TABLE OF string,
      lv_value     TYPE string.

    lv_name = iv_title.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_name WITH `_`.

    TRY.
        lt_bundles = go_reg_root->get_subentries( ).

        LOOP AT lt_bundles INTO ls_bundle.
          " Is tool installed?
          lo_reg_tool = ls_bundle-value->get_subentry( lv_name ).
          IF lo_reg_tool IS BOUND.
            EXIT.
          ENDIF.
        ENDLOOP.
        CHECK lo_reg_tool IS BOUND.

        " Switches entry
        lo_reg_entry = lo_reg_tool->get_subentry( c_reg-switches ).
        CHECK lo_reg_entry IS BOUND.

        " Is switch active?
        lv_value = lo_reg_entry->get_value( iv_key ).
        IF lv_value = abap_true.
          rv_result = abap_true.
        ELSEIF lv_value = cl_abap_syst=>get_user_name( ) ##USER_OK.
          rv_result = abap_true.
        ELSEIF lv_value CS ','.
          SPLIT lv_value AT ',' INTO TABLE lt_users.
          FIND sy-uname IN TABLE lt_users.
          IF sy-subrc = 0.
            rv_result = abap_true.
          ENDIF.
        ENDIF.

      CATCH cx_root.
        rv_result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD class_constructor.

    TRY.
        " Get root of registry
        go_reg_root = /mbtools/cl_registry=>get_root( ).

      CATCH /mbtools/cx_exception.
        " MBT Base is not installed properly. Contact Marc Bernard Tools
        ASSERT 0 = 1.
    ENDTRY.

  ENDMETHOD.


  METHOD is_active.

    rv_result = check_switch( iv_title = iv_title
                              iv_key   = c_reg-key_active ).

  ENDMETHOD.


  METHOD is_debug.

    rv_result = check_switch( iv_title = iv_title
                              iv_key   = c_reg-key_debug ).

  ENDMETHOD.


  METHOD is_trace.

    rv_result = check_switch( iv_title = iv_title
                              iv_key   = c_reg-key_trace ).

  ENDMETHOD.
ENDCLASS.
