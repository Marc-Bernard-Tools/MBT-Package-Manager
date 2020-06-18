************************************************************************
* /MBTOOLS/CL_REGISTRY_UI
* MBT Registry
*
* Implementation of a registry for storing arbitrary values (similar
* to the MS Windows registry)
*
* Original Author: (c) Martin Ceronio (2015), http://ceronio.net
* Released under MIT License: https://opensource.org/licenses/MIT
*
* Ported to namespace and enhanced by Marc Bernard Tools
************************************************************************
CLASS /mbtools/cl_registry_ui DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS icon .

    TYPES:
      " Table for registry entries on tree
      BEGIN OF ty_tab,
        key       TYPE string,
        reg_entry TYPE REF TO /mbtools/cl_registry,
      END OF ty_tab .

    " For tree control:
    CLASS-DATA gr_tree TYPE REF TO cl_gui_alv_tree .
    CLASS-DATA gr_tree_toolbar TYPE REF TO cl_gui_toolbar .
    CLASS-DATA gs_node_layout TYPE lvc_s_layn .   "Layout for new nodes
    CLASS-DATA:
      " Container for ALV tree data:
      gt_tab TYPE TABLE OF ty_tab .
    " For maintaining registry values in an an entry (ALV control):
    CLASS-DATA gr_table TYPE REF TO cl_gui_alv_grid .
    CLASS-DATA:
      gt_value   TYPE STANDARD TABLE OF /mbtools/cl_registry=>ty_keyval .
    CLASS-DATA:
      gt_value_ori TYPE STANDARD TABLE OF /mbtools/cl_registry=>ty_keyval .            "Original data
    " For splitter container
    CLASS-DATA gr_splitter TYPE REF TO cl_gui_easy_splitter_container .
    " For registry access:
    CLASS-DATA gr_reg_root TYPE REF TO /mbtools/cl_registry .
    CLASS-DATA gr_sel_reg_entry TYPE REF TO /mbtools/cl_registry .   "Selected reg. entry
    CLASS-DATA gv_sel_node_key TYPE lvc_nkey .   "Tree node key of currently selected node
    " Read-only flag
    CLASS-DATA gv_read_only TYPE abap_bool .

    CLASS-METHODS create_tree
      IMPORTING
        ir_splitter TYPE REF TO cl_gui_easy_splitter_container.
  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-METHODS handle_node_expand
        FOR EVENT expand_nc OF cl_gui_alv_tree
      IMPORTING
        !node_key
        !sender .
    CLASS-METHODS handle_table_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
        !e_object
        !e_interactive  ##NEEDED.
    CLASS-METHODS handle_table_command
        FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING
        !e_ucomm .
    CLASS-METHODS handle_node_selected
        FOR EVENT selection_changed OF cl_gui_alv_tree
      IMPORTING
        !node_key .
    CLASS-METHODS handle_tree_command
        FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING
        !fcode .
    CLASS-METHODS create_table
      RAISING
        cx_salv_msg .
    CLASS-METHODS refresh_subnodes
      IMPORTING
        !iv_nkey TYPE lvc_nkey .
    CLASS-METHODS save_values .
    CLASS-METHODS add_node
      IMPORTING
        !iv_nkey     TYPE lvc_nkey
        !io_regentry TYPE REF TO /mbtools/cl_registry .
    CLASS-METHODS value_input_dialog
      IMPORTING
        !iv_title      TYPE csequence
      EXPORTING
        !ev_value      TYPE string
        !ev_returncode TYPE c .
ENDCLASS.



CLASS /MBTOOLS/CL_REGISTRY_UI IMPLEMENTATION.


  METHOD add_node.

    " Add single node to tree
    DATA: lv_node_text TYPE lvc_value.
    DATA: ls_node_layout TYPE lvc_s_layn. "Layout for new nodes
    DATA: ls_tab TYPE ty_tab.

    IF io_regentry IS NOT BOUND.
      MESSAGE 'Error building tree'(014) TYPE 'E'.
    ENDIF.

    ls_tab-reg_entry = io_regentry.

    " Add node as folder always
    ls_node_layout-isfolder = abap_true.

    " Add expander only if there are more sub-entries
    IF lines( io_regentry->get_subentry_keys( ) ) > 0.
      ls_node_layout-expander = abap_true.
    ELSE.
      ls_node_layout-expander = abap_false.
    ENDIF.

    lv_node_text = io_regentry->mv_entry_id.

    gr_tree->add_node(
      EXPORTING
        i_relat_node_key     = iv_nkey
        i_relationship       = cl_gui_column_tree=>relat_last_child
        is_outtab_line       = ls_tab
        i_node_text          = lv_node_text
        is_node_layout       = ls_node_layout
      EXCEPTIONS
        relat_node_not_found = 1
        node_not_found       = 2
        OTHERS               = 3 ).
    IF sy-subrc <> 0.
      MESSAGE 'Error building tree'(014) TYPE 'E'.
    ENDIF.

  ENDMETHOD.                    "add_node


  METHOD create_table.

    " Initialize table for showing values in a registry entry
    DATA: lt_fcat TYPE lvc_t_fcat.
    DATA: ls_fcat TYPE lvc_s_fcat.

    CREATE OBJECT gr_table
      EXPORTING
        i_parent          = gr_splitter->bottom_right_container
        i_appl_events     = abap_true    " Register Events as Application Events
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " Add fields to catalog
    ls_fcat-fieldname = 'KEY'.
    ls_fcat-edit      = abap_true.
    ls_fcat-key       = abap_true.
    ls_fcat-scrtext_s = 'Key'(001).
    ls_fcat-outputlen = 40. "Because colwidth opt is not always great "<<<CHG
    APPEND ls_fcat TO lt_fcat.
    ls_fcat-fieldname = 'VALUE'.
    ls_fcat-edit      = abap_true.
    ls_fcat-key       = abap_false.
    ls_fcat-scrtext_s = 'Value'(002).
    ls_fcat-outputlen = 100. "Because colwidth opt is not always great "<<<CHG
    APPEND ls_fcat TO lt_fcat.

    gr_table->set_table_for_first_display(
      CHANGING
        it_outtab                     = gt_value[]
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " Toolbar to hold only functions for editing
    SET HANDLER handle_table_toolbar FOR gr_table.
    SET HANDLER handle_table_command FOR gr_table.
    gr_table->set_toolbar_interactive( ).

  ENDMETHOD.                    "create_table


  METHOD create_tree.

    " Initialize tree showing the registry hierarchy
    DATA: lt_fcat TYPE lvc_t_fcat.
    DATA: ls_fcat TYPE lvc_s_fcat.
    DATA: lt_event TYPE cntl_simple_events,
          ls_event TYPE cntl_simple_event.
    DATA: lx_exc TYPE REF TO /mbtools/cx_exception.

    gr_splitter = ir_splitter.

    TRY.
        gr_reg_root = /mbtools/cl_registry=>get_root( ).
      CATCH /mbtools/cx_exception INTO lx_exc.
        MESSAGE lx_exc->get_text( ) TYPE 'I'.
        RETURN.
    ENDTRY.

    " Create tree
    CREATE OBJECT gr_tree
      EXPORTING
        parent                      = gr_splitter->top_left_container
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
        item_selection              = abap_false
        no_toolbar                  = abap_false
        no_html_header              = abap_true
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7
        OTHERS                      = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " Add key so that there is *something* in the field catalog
    ls_fcat-fieldname = 'KEY'.
    ls_fcat-no_out    = abap_true.
    APPEND ls_fcat TO lt_fcat.

    gr_tree->set_table_for_first_display(
      CHANGING
        it_outtab       = gt_tab
        it_fieldcatalog = lt_fcat ).

    " Get handle on tree toolbar
    DATA: lt_ttb TYPE ttb_button.
    DATA: ls_ttb TYPE stb_button.
    gr_tree->get_toolbar_object( IMPORTING er_toolbar = gr_tree_toolbar ).
    gr_tree_toolbar->delete_all_buttons( ).

    " Add custom buttons for registry entry operations
    ls_ttb-function = 'INSE'. "Insert entry
    ls_ttb-icon     = '@17@'. "ICON_INSERT_ROW
    APPEND ls_ttb TO lt_ttb.
    ls_ttb-function = 'DELE'. "Delete entry
    ls_ttb-icon     = '@18@'. "ICON_DELETE_ROW
    APPEND ls_ttb TO lt_ttb.
    ls_ttb-function = 'COPY'. "Copy Entry
    ls_ttb-icon     = '@14@'. "ICON_COPY_OBJECT
    APPEND ls_ttb TO lt_ttb.
*>>>INS
    ls_ttb-function = 'EXPORT'. "Export Entry
    ls_ttb-icon     = icon_export.
    APPEND ls_ttb TO lt_ttb.
    ls_ttb-function = 'INFO'. "Info
    ls_ttb-icon     = icon_information.
    APPEND ls_ttb TO lt_ttb.
*<<<INS
    gr_tree_toolbar->add_button_group(
      EXPORTING
        data_table       = lt_ttb
      EXCEPTIONS
        dp_error         = 1
        cntb_error_fcode = 2
        OTHERS           = 3 ).
    IF sy-subrc <> 0.
      MESSAGE 'Error when setting up registry toolbar'(005) TYPE 'E'.
    ENDIF.

    " Add root node
    add_node( iv_nkey     = ''
              io_regentry = gr_reg_root ).

    " Register events and set handlers
    ls_event-eventid = cl_gui_simple_tree=>eventid_selection_changed.
    ls_event-appl_event = 'X'.
    APPEND ls_event TO lt_event.
    ls_event-eventid = cl_gui_simple_tree=>eventid_expand_no_children.
    ls_event-appl_event = 'X'.
    APPEND ls_event TO lt_event.

    gr_tree->set_registered_events(
      EXPORTING
        events                    = lt_event
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3 ).
    IF sy-subrc <> 0.
      MESSAGE 'Error registering events for tree'(019) TYPE 'E'.
    ENDIF.

    SET HANDLER handle_node_expand FOR gr_tree.
    SET HANDLER handle_node_selected FOR gr_tree.
    SET HANDLER handle_tree_command FOR gr_tree_toolbar.

*>>>INS
    " Expand root node
    handle_node_expand(
        node_key = '          1'
        sender   = gr_tree  ).

    gr_tree->expand_node(
      EXPORTING
        i_node_key          = '          1'
      EXCEPTIONS
        failed              = 1
        illegal_level_count = 2
        cntl_system_error   = 3
        node_not_found      = 4
        cannot_expand_leaf  = 5
        OTHERS              = 6 ).
    IF sy-subrc <> 0.
      MESSAGE 'Error expanding root node'(013) TYPE 'E'.
    ENDIF.
*<<<INS

    gr_tree->frontend_update( ).
  ENDMETHOD.                    "create_tree


  METHOD handle_node_expand.

    " Expand nodes of the registry tree to add sub-entries
    DATA: lr_reg_entry TYPE REF TO /mbtools/cl_registry.
    DATA: ls_tab TYPE ty_tab.
    DATA: lx_exc TYPE REF TO /mbtools/cx_exception.
    DATA: lt_sub_entries TYPE /mbtools/cl_registry=>ty_keyobjs.
    DATA: ls_sub_entry TYPE /mbtools/cl_registry=>ty_keyobj.

    sender->get_outtab_line(
      EXPORTING
        i_node_key     = node_key
      IMPORTING
        e_outtab_line  = ls_tab    " Line of Outtab
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " Add sub-entries to selected node in tree
    TRY.
        lt_sub_entries = ls_tab-reg_entry->get_subentries( ).
      CATCH /mbtools/cx_exception INTO lx_exc.
        MESSAGE lx_exc->get_text( ) TYPE 'I'.
        RETURN.
    ENDTRY.

    LOOP AT lt_sub_entries INTO ls_sub_entry.

      lr_reg_entry = ls_sub_entry-value.

      add_node( iv_nkey     = node_key
                io_regentry = lr_reg_entry ).

    ENDLOOP.

  ENDMETHOD.                              "handle_node_expand


  METHOD handle_node_selected.

    " Handle selection of a node in the tree
    DATA: ls_tab TYPE ty_tab.

    " Check whether data has changed before
    DATA: lv_answer TYPE c.
    DATA: ls_layout TYPE lvc_s_layo.

    " Check for changed data. The CHECK_CHANGED_DATA() method and
    " neither the DATA_CHANGED or DATA_CHANGED_FINISHED
    " events of CL_GUI_ALV_GRID seem to fit the bill, so we keep our own copy
    " of the original data and compare it
    IF gr_table IS BOUND.
      " Refresh data in local table (GT_VALUE)
      gr_table->check_changed_data( ).

      IF gt_value NE gt_value_ori.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirm data loss'(017)
            text_question         = 'Data has changed. Save first?'(018)
            display_cancel_button = abap_false
          IMPORTING
            answer                = lv_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          " Not going to happen; not exporting a text
        ENDIF.

        IF lv_answer = '1'. "Save data before moving on
          save_values( ).
        ENDIF.

      ENDIF.
    ELSE.
      " Set up the table
      TRY.
          create_table( ).
        CATCH cx_salv_msg.
          BREAK-POINT ##NO_BREAK.
      ENDTRY.
    ENDIF.

    gr_tree->get_outtab_line(
      EXPORTING
        i_node_key     = node_key
      IMPORTING
        e_outtab_line  = ls_tab    " Line of Outtab
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " Read the values of the selected registry entry
    gt_value =  ls_tab-reg_entry->get_values( ).
    gt_value_ori = gt_value. "Store last values

*>>>INS
    " Read-only registry entries
    IF ls_tab-reg_entry->mv_entry_id CS '^'.
      gr_table->set_ready_for_input( 0 ).
      gv_read_only = abap_true.
    ELSE.
      gr_table->set_ready_for_input( 1 ).
      gv_read_only = abap_false.
    ENDIF.
*<<<INS

    " Ensure column widths are correct on every update
    " Settings on table
    gr_table->get_frontend_layout( IMPORTING es_layout = ls_layout ).
    ls_layout-edit = abap_false.
    gr_table->set_frontend_layout( ls_layout ).

    gr_table->refresh_table_display( ).
    " Keep track of selected reg. entry for update
    gr_sel_reg_entry = ls_tab-reg_entry.
    gv_sel_node_key  = node_key.

  ENDMETHOD.                    "handle_node_selected


  METHOD handle_table_command.

    " Save current values
    IF e_ucomm = 'SAVE'.
      save_values( ).
    ENDIF.

  ENDMETHOD.                    "handle_table_command


  METHOD handle_table_toolbar.

    " Modify toolbar entries for table/grid
    DATA: ls_tbe TYPE stb_button.

    " Keep only the local editing features
    LOOP AT e_object->mt_toolbar INTO ls_tbe.
      IF ls_tbe-function(7) NE '&LOCAL&'.
        DELETE e_object->mt_toolbar.
      ENDIF.
    ENDLOOP.

    " Add a function for saving the values
    ls_tbe-function  = 'SAVE'.
    ls_tbe-icon      = '@2L@'.
    ls_tbe-butn_type = '0'.
    ls_tbe-quickinfo = 'Save'(022).
*>>>INS
    ls_tbe-disabled = gv_read_only.
*<<<INS
    APPEND ls_tbe TO e_object->mt_toolbar.

  ENDMETHOD.                    "handle_table_toolbar


  METHOD handle_tree_command.

    " Handle commands to the tree toolbar
    DATA: lv_new_key TYPE string.
    DATA: lr_reg_entry TYPE REF TO /mbtools/cl_registry.
    DATA: lv_node_key TYPE lvc_nkey.
    DATA: lv_rc TYPE char1.
    DATA: lx_exc TYPE REF TO /mbtools/cx_exception.
    DATA: lr_parent TYPE REF TO /mbtools/cl_registry.

    IF gr_sel_reg_entry IS NOT BOUND.
      MESSAGE 'Select a node from the tree first'(003) TYPE 'I'. "<<<CHG
      RETURN.
    ENDIF.

    CASE fcode.
      WHEN 'INSE'.
        " Create a new node under selected node, or copy a registry node on the same level

        " Dialog to capture name of new node
        value_input_dialog(
          EXPORTING
            iv_title      = 'New registry entry key'(007)
          IMPORTING
            ev_value      = lv_new_key
            ev_returncode = lv_rc ).

        " Add the new key to the current registry entry if the user accepts
        IF lv_rc = space.
          TRY.
              " Update the tree by adding the new node
              gr_sel_reg_entry->add_subentry( lv_new_key ).
              refresh_subnodes( gv_sel_node_key ).

            CATCH /mbtools/cx_exception INTO lx_exc.
              MESSAGE lx_exc->get_text( ) TYPE 'I'.
              RETURN.
          ENDTRY.
        ENDIF.

      WHEN 'COPY'.
        " Copy the selected node at the same level

        " Dialog to capture name of new node
        value_input_dialog(
          EXPORTING
            iv_title      = 'Target registry entry key'(006)
          IMPORTING
            ev_value      = lv_new_key
            ev_returncode = lv_rc ).

        " deep copy of source to target node
        IF lv_rc = space.
          TRY.
              lr_parent = gr_sel_reg_entry->get_parent( ).
              lr_parent->copy_subentry( iv_source_key = gr_sel_reg_entry->mv_entry_id
                                        iv_target_key = lv_new_key ).

              " Get the parent node in the tree to refresh it
              gr_tree->get_parent(
                EXPORTING
                  i_node_key        = gv_sel_node_key
                IMPORTING
                  e_parent_node_key = lv_node_key ).

              " Refresh the parent node
              refresh_subnodes( lv_node_key ).

            CATCH /mbtools/cx_exception INTO lx_exc.
              MESSAGE lx_exc->get_text( ) TYPE 'I'.
              RETURN.
          ENDTRY.
        ENDIF.

      WHEN 'DELE'.
        " Delete the selected node from the registry

        " Prevent deleting of the root entity, which would fail anyway when we try get its parent
        IF gr_sel_reg_entry->mv_internal_key = /mbtools/cl_registry=>c_registry_root.
          MESSAGE 'Root node cannot be deleted'(012) TYPE 'I'. "<<<CHG
          RETURN.
        ENDIF.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirm deletion'(009)
            text_question         = 'Are you sure you want to delete the selected entry?'(010)
            display_cancel_button = abap_false
          IMPORTING
            answer                = lv_rc
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          BREAK-POINT ID /mbtools/bc.
        ENDIF.

        " Check that the user selected OK on the confirmation
        CHECK lv_rc = '1'.

        TRY.
            lr_reg_entry = gr_sel_reg_entry->get_parent( ).

            CHECK lr_reg_entry IS BOUND.

            lr_reg_entry->remove_subentry( gr_sel_reg_entry->mv_entry_id ).
          CATCH /mbtools/cx_exception INTO lx_exc.
            MESSAGE lx_exc->get_text( ) TYPE 'I'.
            RETURN.
        ENDTRY.

        " Get the parent node in the tree to refresh it
        gr_tree->get_parent(
          EXPORTING
            i_node_key        = gv_sel_node_key
          IMPORTING
            e_parent_node_key = lv_node_key ).

        " Refresh the parent node
        refresh_subnodes( lv_node_key ).

*>>>INS
      WHEN 'EXPORT'.
        "   Export registry branch to file

        CONSTANTS:
          c_registry_title TYPE string VALUE 'MBT Registry 1.0' ##NO_TEXT.

        DATA:
          l_file     TYPE string,
          l_path     TYPE string,
          l_fullpath TYPE string,
          l_action   TYPE i,
          lt_file    TYPE string_table.

        CLEAR lt_file.

        APPEND c_registry_title TO lt_file.
        APPEND '' TO lt_file.

        TRY.
            gr_sel_reg_entry->export(
              CHANGING
                ct_file = lt_file ).
          CATCH /mbtools/cx_exception INTO lx_exc.
            MESSAGE lx_exc->get_text( ) TYPE 'I'.
            RETURN.
        ENDTRY.

        cl_gui_frontend_services=>file_save_dialog(
          EXPORTING
            window_title              = |Registry Export|
            default_extension         = 'reg'
            default_file_name         = 'mbt_registry'
          CHANGING
            filename                  = l_file
            path                      = l_path
            fullpath                  = l_fullpath
            user_action               = l_action
          EXCEPTIONS
            cntl_error                = 1
            error_no_gui              = 2
            not_supported_by_gui      = 3
            invalid_default_file_name = 4
            OTHERS                    = 5 ).
        IF sy-subrc <> 0 OR l_action = cl_gui_frontend_services=>action_cancel.
          RETURN.
        ENDIF.

        cl_gui_frontend_services=>gui_download(
          EXPORTING
            filename                = l_fullpath
          CHANGING
            data_tab                = lt_file
          EXCEPTIONS
            file_write_error        = 1
            no_batch                = 2
            gui_refuse_filetransfer = 3
            invalid_type            = 4
            no_authority            = 5
            unknown_error           = 6
            header_not_allowed      = 7
            separator_not_allowed   = 8
            filesize_not_allowed    = 9
            header_too_long         = 10
            dp_error_create         = 11
            dp_error_send           = 12
            dp_error_write          = 13
            unknown_dp_error        = 14
            access_denied           = 15
            dp_out_of_memory        = 16
            disk_full               = 17
            dp_timeout              = 18
            file_not_found          = 19
            dataprovider_exception  = 20
            control_flush_error     = 21
            not_supported_by_gui    = 22
            error_no_gui            = 23
            OTHERS                  = 24 ).
        IF sy-subrc <> 0.
          MESSAGE e001(/mbtools/bc) DISPLAY LIKE 'I'
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      WHEN 'INFO'.
        " Show last changed at, on, by
        MESSAGE i001(/mbtools/bc) WITH
          gr_sel_reg_entry->ms_regs-chdate
          gr_sel_reg_entry->ms_regs-chtime
          gr_sel_reg_entry->ms_regs-chname.
*<<<INS
    ENDCASE.

  ENDMETHOD.                    "handle_tree_command


  METHOD refresh_subnodes.

    " Delete and refresh subnodes of a node
    DATA: ls_tab TYPE ty_tab.
    DATA: ls_subentry TYPE /mbtools/cl_registry=>ty_keyval.
    DATA: lr_reg_entry TYPE REF TO /mbtools/cl_registry.
    DATA: lt_children TYPE lvc_t_nkey.
    DATA: lv_nkey TYPE lvc_nkey.
    DATA: lx_exc TYPE REF TO /mbtools/cx_exception.

    " Delete subnodes of node. This means: getting all children and deleting
    " them individually!
    gr_tree->get_children(
      EXPORTING
        i_node_key         = iv_nkey
      IMPORTING
        et_children        = lt_children
      EXCEPTIONS
        historic_error     = 1
        node_key_not_found = 2
        OTHERS             = 3 ).
    IF sy-subrc <> 0.
      MESSAGE 'Error building tree'(014) TYPE 'E'.
    ENDIF.

    LOOP AT lt_children INTO lv_nkey.

      gr_tree->delete_subtree(
        EXPORTING
          i_node_key                = lv_nkey
          i_update_parents_expander = abap_true
        EXCEPTIONS
          node_key_not_in_model     = 1
          OTHERS                    = 2 ).
      IF sy-subrc <> 0.
        MESSAGE 'Error building tree'(014) TYPE 'E'.
      ENDIF.

    ENDLOOP.

    " With the children deleted, proceed to re-add registry entries

    " Get the registry entry on the node
    gr_tree->get_outtab_line(
      EXPORTING
        i_node_key     = iv_nkey
      IMPORTING
        e_outtab_line  = ls_tab
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc NE 0.
      MESSAGE 'Error building tree'(014) TYPE 'E'.
    ENDIF.

    " Add a subnode for each sub-entry
    LOOP AT ls_tab-reg_entry->mt_sub_entries INTO ls_subentry.
      TRY.
          lr_reg_entry = ls_tab-reg_entry->get_subentry( ls_subentry-key ).
        CATCH /mbtools/cx_exception INTO lx_exc.
          MESSAGE lx_exc->get_text( ) TYPE 'I'.
          RETURN.
      ENDTRY.
      add_node( iv_nkey     = iv_nkey
                io_regentry = lr_reg_entry ).
    ENDLOOP.

    " Expand parent node
    gr_tree->expand_node(
      EXPORTING
        i_node_key          = iv_nkey
      EXCEPTIONS
        failed              = 1
        illegal_level_count = 2
        cntl_system_error   = 3
        node_not_found      = 4
        cannot_expand_leaf  = 5
        OTHERS              = 6 ).
    IF sy-subrc <> 0.
      MESSAGE 'Error building tree'(014) TYPE 'E'.
    ENDIF.

    " Update tree display
    gr_table->refresh_table_display( ).

  ENDMETHOD.                    "refresh_subnodes


  METHOD save_values.

    DATA: lt_value TYPE /mbtools/cl_registry=>ty_keyvals.
    DATA: ls_value TYPE /mbtools/cl_registry=>ty_keyval.
    DATA: lx_exc TYPE REF TO /mbtools/cx_exception.

    " Save current values in table to currently selected reg. node
    IF gr_table IS BOUND AND gr_sel_reg_entry IS BOUND.

      " Normalize the values; duplicate keys are overwritten, with possible loss of data!
      LOOP AT gt_value INTO ls_value.
        INSERT ls_value INTO TABLE lt_value.
      ENDLOOP.

      TRY.
          gr_sel_reg_entry->set_values( lt_value ).
          gr_sel_reg_entry->save( ).
          MESSAGE 'Values saved'(021) TYPE 'S'. "<<<INS
          gt_value_ori = gt_value. "Store last values again "<<<INS
        CATCH /mbtools/cx_exception.
          MESSAGE 'Values have been overwritten since last change and are refreshed'(004) TYPE 'I'.
          TRY.
              gr_sel_reg_entry->reload( ).
            CATCH /mbtools/cx_exception INTO lx_exc.
              MESSAGE lx_exc->get_text( ) TYPE 'I'.
              RETURN.
          ENDTRY.
          gt_value = gr_sel_reg_entry->get_values( ).
      ENDTRY.

      gr_table->refresh_table_display( ).

    ENDIF.

  ENDMETHOD.


  METHOD value_input_dialog.

    " Get single value from user
    DATA: lt_fld TYPE TABLE OF sval.
    DATA: ls_fld TYPE sval.

    ls_fld-tabname = 'OJFIELDS'.
    ls_fld-fieldname = 'INPUT'.
    APPEND ls_fld TO lt_fld.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = iv_title
      IMPORTING
        returncode      = ev_returncode
      TABLES
        fields          = lt_fld
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE 'Error during request for value'(008) TYPE 'E'.
    ENDIF.

    READ TABLE lt_fld INTO ls_fld INDEX 1.
    ev_value = ls_fld-value.

  ENDMETHOD.                    "value_input_dialog
ENDCLASS.
