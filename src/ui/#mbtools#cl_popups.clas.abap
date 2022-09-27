CLASS /mbtools/cl_popups DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS /mbtools/cl_gui_factory.

************************************************************************
* Marc Bernard Tools - Popups
*
* Copyright 2014 abapGit Contributors <http://abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES /mbtools/if_popups.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_fieldname_selected TYPE lvc_fname VALUE `SELECTED` ##NO_TEXT.
    DATA mo_select_list_popup TYPE REF TO cl_salv_table.
    DATA mr_table TYPE REF TO data.
    DATA mv_cancel TYPE abap_bool VALUE abap_false ##NO_TEXT.
    DATA mo_table_descr TYPE REF TO cl_abap_tabledescr.

    METHODS create_new_table
      IMPORTING
        !it_list TYPE STANDARD TABLE.
    METHODS get_selected_rows
      EXPORTING
        !et_list TYPE INDEX TABLE.
    METHODS on_select_list_link_click
        FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        !row
        !column.
    METHODS on_select_list_function_click
        FOR EVENT added_function OF cl_salv_events_table
      IMPORTING
        !e_salv_function.
    METHODS on_double_click
        FOR EVENT double_click OF cl_salv_events_table
      IMPORTING
        !row
        !column.
ENDCLASS.



CLASS /mbtools/cl_popups IMPLEMENTATION.


  METHOD /mbtools/if_popups~popup_search_help.

    DATA lt_ret TYPE TABLE OF ddshretval.
    DATA ls_ret LIKE LINE OF lt_ret.
    DATA lv_tabname TYPE dfies-tabname.
    DATA lv_fieldname TYPE dfies-fieldname.

    SPLIT iv_tab_field AT '-' INTO lv_tabname lv_fieldname.
    lv_tabname = to_upper( lv_tabname ).
    lv_fieldname = to_upper( lv_fieldname ).

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname    = lv_tabname
        fieldname  = lv_fieldname
      TABLES
        return_tab = lt_ret
      EXCEPTIONS
        OTHERS     = 5.

    IF sy-subrc <> 0.
      /mbtools/cx_exception=>raise( |F4IF_FIELD_VALUE_REQUEST error [{ iv_tab_field }]| ).
    ENDIF.

    IF lines( lt_ret ) > 0.
      READ TABLE lt_ret INDEX 1 INTO ls_ret.
      ASSERT sy-subrc = 0.
      rv_value = ls_ret-fieldval.
    ENDIF.

  ENDMETHOD.


  METHOD /mbtools/if_popups~popup_to_confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = iv_titlebar
        text_question         = iv_text_question
        text_button_1         = iv_text_button_1
        icon_button_1         = iv_icon_button_1
        text_button_2         = iv_text_button_2
        icon_button_2         = iv_icon_button_2
        default_button        = iv_default_button
        display_cancel_button = iv_display_cancel_button
      IMPORTING
        answer                = rv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.                        "#EC NOTEXT
    IF sy-subrc <> 0.
      /mbtools/cx_exception=>raise( 'error from POPUP_TO_CONFIRM' ).
    ENDIF.

  ENDMETHOD.


  METHOD /mbtools/if_popups~popup_to_inform.

    DATA: lv_line1 TYPE c LENGTH 70,
          lv_line2 TYPE c LENGTH 70.

    lv_line1 = iv_text_message.
    IF strlen( iv_text_message ) > 70.
      lv_line2 = iv_text_message+70.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = iv_titlebar
        txt1  = lv_line1
        txt2  = lv_line2.

  ENDMETHOD.


  METHOD /mbtools/if_popups~popup_to_select_from_list.

    DATA: lv_pfstatus     TYPE sypfkey,
          lo_events       TYPE REF TO cl_salv_events_table,
          lo_columns      TYPE REF TO cl_salv_columns_table,
          lt_columns      TYPE salv_t_column_ref,
          ls_column       TYPE salv_s_column_ref,
          lo_column       TYPE REF TO cl_salv_column_list,
          lo_table_header TYPE REF TO cl_salv_form_text.

    FIELD-SYMBOLS: <lt_table>             TYPE STANDARD TABLE,
                   <ls_column_to_display> TYPE /mbtools/if_definitions=>ty_alv_column.

    CLEAR: et_list.

    create_new_table( it_list ).

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = mo_select_list_popup
                                CHANGING  t_table      = <lt_table> ).

        CASE iv_selection_mode.
          WHEN if_salv_c_selection_mode=>single.
            lv_pfstatus = '110'.

          WHEN OTHERS.
            lv_pfstatus = '102'.

        ENDCASE.

        mo_select_list_popup->set_screen_status( pfstatus = lv_pfstatus
                                                 report   = 'SAPMSVIM' ).

        mo_select_list_popup->set_screen_popup( start_column = iv_start_column
                                                end_column   = iv_end_column
                                                start_line   = iv_start_line
                                                end_line     = iv_end_line ).

        lo_events = mo_select_list_popup->get_event( ).

        SET HANDLER on_select_list_link_click FOR lo_events.
        SET HANDLER on_select_list_function_click FOR lo_events.
        SET HANDLER on_double_click FOR lo_events.

        IF iv_title CN ' _0'.
          mo_select_list_popup->get_display_settings( )->set_list_header( iv_title ).
        ENDIF.

        IF iv_header_text CN ' _0'.
          CREATE OBJECT lo_table_header
            EXPORTING
              text = iv_header_text.
          mo_select_list_popup->set_top_of_list( lo_table_header ).
        ENDIF.

        mo_select_list_popup->get_display_settings( )->set_striped_pattern( iv_striped_pattern ).
        mo_select_list_popup->get_selections( )->set_selection_mode( iv_selection_mode ).

        lo_columns = mo_select_list_popup->get_columns( ).
        lt_columns = lo_columns->get( ).
        lo_columns->set_optimize( iv_optimize_col_width ).

        LOOP AT lt_columns INTO ls_column.

          lo_column ?= ls_column-r_column.

          IF iv_selection_mode = if_salv_c_selection_mode=>multiple
            AND ls_column-columnname = c_fieldname_selected.
            lo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
            lo_column->set_output_length( 20 ).
            lo_column->set_short_text( |{ iv_select_column_text }| ).
            lo_column->set_medium_text( |{ iv_select_column_text }| ).
            lo_column->set_long_text( |{ iv_select_column_text }| ).
            CONTINUE.
          ENDIF.

          READ TABLE it_columns_to_display
            ASSIGNING <ls_column_to_display>
            WITH KEY name = ls_column-columnname.

          CASE sy-subrc.
            WHEN 0.
              IF <ls_column_to_display>-text CN ' _0'.
                lo_column->set_short_text( |{ <ls_column_to_display>-text }| ).
                lo_column->set_medium_text( |{ <ls_column_to_display>-text }| ).
                lo_column->set_long_text( |{ <ls_column_to_display>-text }| ).
              ENDIF.

              IF <ls_column_to_display>-length > 0.
                lo_column->set_output_length( <ls_column_to_display>-length ).
              ENDIF.

            WHEN OTHERS.
              " Hide column
              lo_column->set_technical( abap_true ).

          ENDCASE.

        ENDLOOP.

        mo_select_list_popup->display( ).

      CATCH cx_salv_msg.
        /mbtools/cx_exception=>raise( 'Error from POPUP_TO_SELECT_FROM_LIST' ).
    ENDTRY.

    IF mv_cancel = abap_true.
      mv_cancel = abap_false.
      RAISE EXCEPTION TYPE /mbtools/cx_cancel.
    ENDIF.

    get_selected_rows( IMPORTING et_list = et_list ).

    CLEAR: mo_select_list_popup,
           mr_table,
           mo_table_descr.

  ENDMETHOD.


  METHOD /mbtools/if_popups~popup_to_select_transports.

* todo, method to be renamed, it only returns one transport

    DATA: lv_trkorr TYPE e070-trkorr,
          ls_trkorr LIKE LINE OF rt_trkorr.


    CALL FUNCTION 'TR_F4_REQUESTS'
      IMPORTING
        ev_selected_request = lv_trkorr.

    IF lv_trkorr IS NOT INITIAL.
      ls_trkorr-trkorr = lv_trkorr.
      APPEND ls_trkorr TO rt_trkorr.
    ENDIF.

  ENDMETHOD.


  METHOD /mbtools/if_popups~popup_transport_request.

    DATA: lt_e071  TYPE STANDARD TABLE OF e071,
          lt_e071k TYPE STANDARD TABLE OF e071k.

    CALL FUNCTION 'TRINT_ORDER_CHOICE'
      EXPORTING
        wi_order_type          = is_transport_type-request
        wi_task_type           = is_transport_type-task
      IMPORTING
        we_order               = rv_transport
      TABLES
        wt_e071                = lt_e071
        wt_e071k               = lt_e071k
      EXCEPTIONS
        no_correction_selected = 1
        display_mode           = 2
        object_append_error    = 3
        recursive_call         = 4
        wrong_order_type       = 5
        OTHERS                 = 6.

    IF sy-subrc = 1.
      RAISE EXCEPTION TYPE /mbtools/cx_cancel.
    ELSEIF sy-subrc > 1.
      /mbtools/cx_exception=>raise( |Error from TRINT_ORDER_CHOICE { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD create_new_table.

    " create and populate a table on the fly derived from
    " it_data with a select column

    DATA: lr_struct        TYPE REF TO data,
          lt_components    TYPE cl_abap_structdescr=>component_table,
          lo_struct_descr  TYPE REF TO cl_abap_structdescr,
          lo_struct_descr2 TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <lt_table>     TYPE STANDARD TABLE,
                   <ls_component> TYPE abap_componentdescr,
                   <lg_line>      TYPE data,
                   <lg_data>      TYPE any.

    mo_table_descr ?= cl_abap_tabledescr=>describe_by_data( it_list ).
    lo_struct_descr ?= mo_table_descr->get_table_line_type( ).
    lt_components = lo_struct_descr->get_components( ).

    INSERT INITIAL LINE INTO lt_components ASSIGNING <ls_component> INDEX 1.
    ASSERT sy-subrc = 0.

    <ls_component>-name = c_fieldname_selected.
    <ls_component>-type ?= cl_abap_datadescr=>describe_by_name( 'FLAG' ).

    lo_struct_descr2 = cl_abap_structdescr=>create( lt_components ).
    mo_table_descr = cl_abap_tabledescr=>create( lo_struct_descr2 ).

    CREATE DATA mr_table TYPE HANDLE mo_table_descr.
    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_struct TYPE HANDLE lo_struct_descr2.
    ASSIGN lr_struct->* TO <lg_line>.
    ASSERT sy-subrc = 0.

    LOOP AT it_list ASSIGNING <lg_data>.
      CLEAR <lg_line>.
      MOVE-CORRESPONDING <lg_data> TO <lg_line>.
      INSERT <lg_line> INTO TABLE <lt_table>.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_selected_rows.

    DATA: lv_condition     TYPE string,
          lr_exporting     TYPE REF TO data,
          lo_selections    TYPE REF TO cl_salv_selections,
          lt_selected_rows TYPE salv_t_row.

    FIELD-SYMBOLS: <lg_exporting>    TYPE any,
                   <lt_table>        TYPE STANDARD TABLE,
                   <lg_line>         TYPE any,
                   <lv_selected>     TYPE abap_bool,
                   <lv_selected_row> TYPE LINE OF salv_t_row.

    CLEAR et_list.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    lo_selections = mo_select_list_popup->get_selections( ).

    IF lo_selections->get_selection_mode( ) = if_salv_c_selection_mode=>single.

      lt_selected_rows = lo_selections->get_selected_rows( ).

      LOOP AT lt_selected_rows ASSIGNING <lv_selected_row>.

        READ TABLE <lt_table>
          ASSIGNING <lg_line>
          INDEX <lv_selected_row>.
        CHECK sy-subrc = 0.

        ASSIGN COMPONENT c_fieldname_selected
           OF STRUCTURE <lg_line>
           TO <lv_selected>.
        CHECK sy-subrc = 0.

        <lv_selected> = abap_true.

      ENDLOOP.

    ENDIF.

    lv_condition = |{ c_fieldname_selected } = ABAP_TRUE|.

    CREATE DATA lr_exporting LIKE LINE OF et_list.
    ASSIGN lr_exporting->* TO <lg_exporting>.
    ASSERT sy-subrc = 0.

    LOOP AT <lt_table> ASSIGNING <lg_line> WHERE (lv_condition).
      CLEAR <lg_exporting>.
      MOVE-CORRESPONDING <lg_line> TO <lg_exporting>.
      APPEND <lg_exporting> TO et_list.
    ENDLOOP.

  ENDMETHOD.


  METHOD on_double_click.

    DATA: lo_selections    TYPE REF TO cl_salv_selections.

    lo_selections = mo_select_list_popup->get_selections( ).

    IF lo_selections->get_selection_mode( ) = if_salv_c_selection_mode=>single.
      mo_select_list_popup->close_screen( ).
    ENDIF.

  ENDMETHOD.


  METHOD on_select_list_function_click.

    FIELD-SYMBOLS: <lt_table>    TYPE STANDARD TABLE,
                   <lg_line>     TYPE any,
                   <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    CASE e_salv_function.
      WHEN 'O.K.'.
        mv_cancel = abap_false.
        mo_select_list_popup->close_screen( ).

      WHEN 'ABR'.
        "Canceled: clear list to overwrite nothing
        CLEAR <lt_table>.
        mv_cancel = abap_true.
        mo_select_list_popup->close_screen( ).

      WHEN 'SALL'.
        LOOP AT <lt_table> ASSIGNING <lg_line>.

          ASSIGN COMPONENT c_fieldname_selected
                 OF STRUCTURE <lg_line>
                 TO <lv_selected>.
          ASSERT sy-subrc = 0.

          <lv_selected> = abap_true.

        ENDLOOP.

        mo_select_list_popup->refresh( ).

      WHEN 'DSEL'.
        LOOP AT <lt_table> ASSIGNING <lg_line>.

          ASSIGN COMPONENT c_fieldname_selected
                 OF STRUCTURE <lg_line>
                 TO <lv_selected>.
          ASSERT sy-subrc = 0.

          <lv_selected> = abap_false.

        ENDLOOP.

        mo_select_list_popup->refresh( ).

      WHEN OTHERS.
        CLEAR <lt_table>.
        mo_select_list_popup->close_screen( ).
    ENDCASE.

  ENDMETHOD.


  METHOD on_select_list_link_click.

    FIELD-SYMBOLS: <lt_table>    TYPE STANDARD TABLE,
                   <lg_line>     TYPE any,
                   <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    READ TABLE <lt_table> ASSIGNING <lg_line> INDEX row.
    IF sy-subrc = 0.

      ASSIGN COMPONENT c_fieldname_selected
             OF STRUCTURE <lg_line>
             TO <lv_selected>.
      ASSERT sy-subrc = 0.

      IF <lv_selected> = abap_true.
        <lv_selected> = abap_false.
      ELSE.
        <lv_selected> = abap_true.
      ENDIF.

    ENDIF.

    mo_select_list_popup->refresh( ).

  ENDMETHOD.
ENDCLASS.
