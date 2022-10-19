CLASS /mbtools/cl_tree DEFINITION
  PUBLIC
  CREATE PUBLIC .

************************************************************************
* MBT Tree
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************
  PUBLIC SECTION.

    METHODS handle_node_double_click
      FOR EVENT node_double_click OF cl_gui_alv_tree
      IMPORTING
        !node_key .
    METHODS handle_item_double_click
      FOR EVENT item_double_click OF cl_gui_alv_tree
      IMPORTING
        !node_key
        !fieldname .
    METHODS constructor .
    METHODS pbo .
    METHODS pai
      IMPORTING
        !iv_ok_code TYPE sy-ucomm .
    METHODS display .
    METHODS download .
    METHODS print .
    METHODS destroy .
    METHODS expand
      IMPORTING
        VALUE(iv_level) TYPE i .
    METHODS expand_all .
    METHODS add_top_node
      IMPORTING
        VALUE(iv_title) TYPE csequence
        VALUE(iv_icon)  TYPE icon_d OPTIONAL
        VALUE(iv_text)  TYPE any OPTIONAL
        VALUE(iv_value) TYPE any OPTIONAL
        VALUE(iv_type)  TYPE csequence OPTIONAL .
    METHODS add_sub_node
      IMPORTING
        VALUE(iv_title) TYPE csequence
        VALUE(iv_icon)  TYPE icon_d OPTIONAL
        VALUE(iv_text)  TYPE any OPTIONAL
        VALUE(iv_value) TYPE any OPTIONAL
        VALUE(iv_type)  TYPE csequence OPTIONAL .
    METHODS add_detail
      IMPORTING
        VALUE(iv_title)  TYPE csequence
        VALUE(iv_icon)   TYPE icon_d OPTIONAL
        VALUE(iv_text)   TYPE any OPTIONAL
        VALUE(iv_value)  TYPE any OPTIONAL
        VALUE(iv_level)  TYPE i OPTIONAL
        VALUE(iv_sign)   TYPE abap_bool DEFAULT abap_false
        VALUE(iv_hidden) TYPE abap_bool DEFAULT abap_false
        VALUE(iv_type)   TYPE csequence OPTIONAL .
    METHODS pick_node .
    METHODS find_node .
    METHODS set_key
      IMPORTING
        !iv_key TYPE lvc_nkey .
    METHODS get_key
      RETURNING
        VALUE(rv_key) TYPE lvc_nkey .
    METHODS next_key .
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mv_container_name TYPE char25 VALUE 'GO_TREE_CONTAINER' ##NO_TEXT.
    DATA mo_custom_container TYPE REF TO cl_gui_custom_container.
    DATA mo_tree TYPE REF TO cl_gui_alv_tree.
    DATA mv_tree_structure TYPE tabname VALUE '/MBTOOLS/TREE_CONTROL' ##NO_TEXT.
    DATA ms_outtab TYPE /mbtools/tree_control.
    DATA:
      mt_outtab TYPE STANDARD TABLE OF /mbtools/tree_control.
    DATA mv_node_key TYPE lvc_nkey.
    DATA mv_relat_key TYPE lvc_nkey.
    DATA mt_fieldcat TYPE lvc_t_fcat.
    DATA mt_item_layout TYPE lvc_t_layi.
    DATA mt_typtab TYPE lvc_t_chit.
    DATA mv_tree_level TYPE i VALUE 0 ##NO_TEXT.
    DATA mv_done TYPE abap_bool.

    METHODS _init.
    METHODS _add_node
      IMPORTING
        VALUE(is_outtab) TYPE any
        VALUE(iv_icon)   TYPE icon_d OPTIONAL
        VALUE(iv_color)  TYPE i OPTIONAL
        VALUE(iv_level)  TYPE i OPTIONAL
        VALUE(iv_sign)   TYPE abap_bool DEFAULT abap_false
        VALUE(iv_hidden) TYPE abap_bool DEFAULT abap_false.
    METHODS _add
      IMPORTING
        VALUE(iv_title)  TYPE csequence
        VALUE(iv_icon)   TYPE icon_d OPTIONAL
        VALUE(iv_text)   TYPE any OPTIONAL
        VALUE(iv_value)  TYPE any OPTIONAL
        VALUE(iv_color)  TYPE i OPTIONAL
        VALUE(iv_level)  TYPE i OPTIONAL
        VALUE(iv_sign)   TYPE abap_bool DEFAULT abap_false
        VALUE(iv_hidden) TYPE abap_bool DEFAULT abap_false
        VALUE(iv_type)   TYPE csequence OPTIONAL.
ENDCLASS.



CLASS /mbtools/cl_tree IMPLEMENTATION.


  METHOD add_detail.

    _add(
      iv_icon   = iv_icon
      iv_title  = iv_title
      iv_text   = iv_text
      iv_value  = iv_value
      iv_color  = 3
      iv_level  = iv_level
      iv_sign   = iv_sign
      iv_hidden = iv_hidden
      iv_type   = iv_type ).

  ENDMETHOD.


  METHOD add_sub_node.

    _add(
      iv_icon   = iv_icon
      iv_title  = iv_title
      iv_text   = iv_text
      iv_value  = iv_value
      iv_color  = 2
      iv_level  = 1
      iv_sign   = space
      iv_hidden = space
      iv_type   = iv_type ).

  ENDMETHOD.


  METHOD add_top_node.

    " Link to root
    CLEAR mv_relat_key.

    _add(
      iv_icon   = iv_icon
      iv_title  = iv_title
      iv_text   = iv_text
      iv_value  = iv_value
      iv_color  = 1
      iv_level  = 0
      iv_sign   = space
      iv_hidden = space
      iv_type   = iv_type ).

    " Initialize relationship counter
    mv_relat_key = 1.

  ENDMETHOD.


  METHOD constructor.

    mv_tree_level = 2.

    _init( ).

  ENDMETHOD.


  METHOD destroy.

    " Destroy tree control
    IF mo_tree IS NOT INITIAL.

      mo_tree->free( ).
      CLEAR mo_tree.

    ENDIF.

    " Destroy tree container
    IF mo_custom_container IS NOT INITIAL.

      mo_custom_container->free( ).
      CLEAR mo_custom_container.

    ENDIF.

  ENDMETHOD.


  METHOD display.

    DATA:
      lv_root TYPE lvc_nkey.

    CHECK mv_done IS INITIAL.

    " Calculate totals (is this really necessary?)
    mo_tree->update_calculations( no_frontend_update = abap_true ).

    " Expand tree
    lv_root = 1.
    IF mv_tree_level IS INITIAL.
      mo_tree->expand_node(
        EXPORTING
          i_node_key          = lv_root
          i_expand_subtree    = abap_true
        EXCEPTIONS
          failed              = 1
          illegal_level_count = 2
          cntl_system_error   = 3
          node_not_found      = 4
          cannot_expand_leaf  = 5
          OTHERS              = 6 ).
    ELSE.
      mo_tree->expand_node(
        EXPORTING
          i_node_key          = lv_root
          i_level_count       = mv_tree_level
        EXCEPTIONS
          failed              = 1
          illegal_level_count = 2
          cntl_system_error   = 3
          node_not_found      = 4
          cannot_expand_leaf  = 5
          OTHERS              = 6 ).
    ENDIF.
    IF sy-subrc <> 0.
      IF sy-subrc = 4.
        add_top_node( iv_icon  = icon_dummy
                      iv_title = 'No selection'(002) ).
      ELSE.
        MESSAGE e000 WITH 'Error in EXPAND_NODE' ##NO_TEXT.
      ENDIF.
    ENDIF.

    " Optimize column width
    mo_tree->column_optimize( ).

    " Finally display tree on frontend
    mo_tree->frontend_update( ).

    cl_gui_cfw=>flush( ).

    mv_done = abap_true.

  ENDMETHOD.


  METHOD download.

    MESSAGE i000 WITH 'Download the View Using "System > List > Save"'(t01).

    mo_tree->set_function_code(
      EXPORTING
        i_ucomm            = cl_gui_alv_tree=>mc_fc_print_prev
      EXCEPTIONS
        function_not_found = 1
        OTHERS             = 2 ).
    IF sy-subrc <> 0.
      MESSAGE i000 WITH 'Function not available'(t02).
    ENDIF.

  ENDMETHOD.


  METHOD expand.

    mv_tree_level = iv_level.

  ENDMETHOD.


  METHOD expand_all.

    mv_tree_level = 0.

  ENDMETHOD.


  METHOD find_node.

    mo_tree->set_function_code(
      EXPORTING
        i_ucomm            = cl_gui_alv_tree=>mc_fc_find
      EXCEPTIONS
        function_not_found = 1
        OTHERS             = 2 ).
    IF sy-subrc <> 0.
      MESSAGE i000 WITH 'Function not available'(t02).
    ENDIF.

  ENDMETHOD.


  METHOD get_key.

    rv_key = mv_relat_key.

  ENDMETHOD.


  METHOD handle_item_double_click.

    " this method handles the item double click event of the tree control instance
    handle_node_double_click( node_key ).

  ENDMETHOD.


  METHOD handle_node_double_click.

    " this method handles the node double click event of the tree control instance
    DATA:
      lv_value    LIKE ms_outtab-value,
      lv_exit     TYPE abap_bool,
      ls_typtab   TYPE lvc_s_chit,
      lv_nrobj    TYPE nrobj,
      lv_mode     TYPE c LENGTH 1,
      lv_compuid  TYPE sysuuid_25,
      lv_vnam     TYPE rszvnam,
      lv_provider TYPE rsinfoprov,
      lv_srvtype  TYPE rsplf_srvtypenm,
      lo_lpogui   TYPE REF TO cl_rslpo_gui,
      lv_lpo      TYPE rslponame.

    READ TABLE mt_outtab INTO ms_outtab
      WITH KEY node_key = node_key.
    CHECK sy-subrc = 0.

    lv_value = ms_outtab-value.

    READ TABLE mt_typtab INTO ls_typtab
      WITH KEY nodekey = node_key.                      "#EC CI_HASHSEQ
    CHECK sy-subrc = 0.

    lv_exit = /mbtools/cl_sap=>show_object( iv_object   = ls_typtab-fieldname
                                            iv_obj_name = lv_value ).
    IF lv_exit = abap_true.
      RETURN.
    ENDIF.

    CASE ls_typtab-fieldname.

      WHEN /mbtools/if_objects=>c_icon.
        /mbtools/cl_sap=>show_icon( lv_value ).

      WHEN /mbtools/if_objects=>c_sel_object.
        lv_compuid = lv_value.
        CALL FUNCTION 'RSPLW_SOB_MAINTAIN'
          EXPORTING
            i_compuid = lv_compuid
            i_fcode   = 'DISPLAY'.

      WHEN /mbtools/if_objects=>c_variable.
        lv_vnam = lv_value.
        CALL FUNCTION 'RSPLW_VAR_DISPLAY'
          EXPORTING
            i_vnam = lv_vnam.

      WHEN /mbtools/if_objects=>c_plan_provider
        OR /mbtools/if_objects=>c_char_relationship
        OR /mbtools/if_objects=>c_data_slice.
        lv_provider = lv_value.
        CALL FUNCTION 'RSPLW_PROV_MAINTAIN'
          EXPORTING
            i_provider = lv_provider
            i_mode     = 'DISPLAY'.

      WHEN /mbtools/if_objects=>c_plan_service_type.
        lv_srvtype = lv_value.
        TRY.
            CALL FUNCTION 'RSPLFD_PLST_MAINT'
              EXPORTING
                i_srvtypenm = lv_srvtype
                i_display   = abap_true.
          CATCH cx_rs_version_not_found.                "#EC NO_HANDLER
          CATCH cx_rs_msg.                              "#EC NO_HANDLER
        ENDTRY.

      WHEN /mbtools/if_objects=>c_lpo.
        TRY.
            lv_lpo = lv_value.
            CREATE OBJECT lo_lpogui.
            lo_lpogui->show_ui( i_lpo = lv_lpo ).
          CATCH cx_rslpo_root.                          "#EC NO_HANDLER
        ENDTRY.

      WHEN /mbtools/if_objects=>c_hierarchy.
        /mbtools/cl_sap=>run_transaction( 'RSH1' ).

      WHEN /mbtools/if_objects=>c_query.
        SET PARAMETER ID 'GID' FIELD lv_value.
        /mbtools/cl_sap=>run_transaction( 'RSRT' ).

      WHEN /mbtools/if_objects=>c_ctrt.
        SET PARAMETER ID 'NBR' FIELD lv_value.
        /mbtools/cl_sap=>run_transaction( 'RSCUR' ).

      WHEN /mbtools/if_objects=>c_uomt.
        SET PARAMETER ID 'RSUOM' FIELD lv_value.
        /mbtools/cl_sap=>run_transaction( 'RSUOM' ).

      WHEN /mbtools/if_objects=>c_thjt.
        " No parameter
        /mbtools/cl_sap=>run_transaction( 'RSTHJTMAINT' ).

      WHEN /mbtools/if_objects=>c_user_id.
        SET PARAMETER ID 'XUS' FIELD lv_value.
        /mbtools/cl_sap=>run_transaction( 'SU01' ).

      WHEN /mbtools/if_objects=>c_number_range.
        lv_nrobj = lv_value.
        lv_mode  = 'U'.
        CALL FUNCTION 'NUMBER_RANGE_OBJECT_MAINTAIN'
          EXPORTING
            display_only     = ' '
            object           = lv_nrobj
            mode             = lv_mode
          EXCEPTIONS
            object_exists    = 1
            object_missing   = 2
            object_not_found = 3
            OTHERS           = 4.
        CHECK sy-subrc = 0.

      WHEN OTHERS.
        " Not implemented yet
        RETURN.
    ENDCASE.

  ENDMETHOD.


  METHOD next_key.

    mv_relat_key = mv_node_key - 1.

  ENDMETHOD.


  METHOD pai.

    CASE iv_ok_code.

        " Finish program
      WHEN 'BACK' OR 'EXIT' OR 'CANC'.
        destroy( ).
        LEAVE TO SCREEN 0.

        " Pick node/item
      WHEN 'PICK'.
        pick_node( ).

        " Find node/item
      WHEN 'FIND'.
        find_node( ).

        " Download
      WHEN 'DOWN'.
        download( ).

        " Print
      WHEN 'PRINT'.
        print( ).

        " Dispatch to tree control
      WHEN OTHERS.
        cl_gui_cfw=>dispatch( ).

    ENDCASE.

    cl_gui_cfw=>flush( ).

  ENDMETHOD.


  METHOD pbo.

    display( ).

  ENDMETHOD.


  METHOD pick_node.

    DATA:
      ls_selected_nodes TYPE lvc_s_nkey,
      lt_selected_nodes TYPE lvc_t_nkey,
      lv_node_key       TYPE lvc_nkey,
      lv_fieldname      TYPE lvc_fname.                     "#EC NEEDED

    " Get node selection
    mo_tree->get_selected_nodes(
      CHANGING
        ct_selected_nodes = lt_selected_nodes
      EXCEPTIONS
        cntl_system_error = 1
        dp_error          = 2
        failed            = 3
        OTHERS            = 4 ).
    IF sy-subrc <> 0.
      MESSAGE e000 WITH 'Error in PICK_NODE Selected Nodes' ##NO_TEXT.
    ENDIF.

    CASE lines( lt_selected_nodes ).
      WHEN 0.
        " No node selected, now check item selection
        mo_tree->get_selected_item(
          IMPORTING
            e_selected_node   = lv_node_key
            e_fieldname       = lv_fieldname
          EXCEPTIONS
            no_item_selection = 1
            cntl_system_error = 2
            failed            = 3
            OTHERS            = 4 ).

        CASE sy-subrc.
          WHEN 0.
            handle_node_double_click( lv_node_key ).
          WHEN 1.
            MESSAGE i227(0h).
        ENDCASE.

      WHEN 1.
        " Exactly one node selected
        READ TABLE lt_selected_nodes INTO ls_selected_nodes INDEX 1.
        IF sy-subrc = 0.
          handle_node_double_click( ls_selected_nodes-node_key ).
        ENDIF.

      WHEN OTHERS.
        " Too many nodes selected
        MESSAGE i227(0h).

    ENDCASE.

  ENDMETHOD.


  METHOD print.

    mo_tree->set_function_code(
      EXPORTING
        i_ucomm            = cl_gui_alv_tree=>mc_fc_print_back
      EXCEPTIONS
        function_not_found = 1
        OTHERS             = 2 ).
    IF sy-subrc <> 0.
      MESSAGE i000 WITH 'Function not available'(t02).
    ENDIF.

  ENDMETHOD.


  METHOD set_key.

    mv_relat_key = iv_key.

  ENDMETHOD.


  METHOD _add.

    DATA:
      ls_typtab TYPE lvc_s_chit,
      lv_type   TYPE c.

    " Node types
    ls_typtab-nodekey   = mv_node_key.
    ls_typtab-fieldname = iv_type.
    INSERT ls_typtab INTO TABLE mt_typtab.

    " Output data
    " - convert text and value to type char 255
    "   although ALV tree currently limits output to char 128
    " - format numeric data
    CLEAR ms_outtab.
    ms_outtab-object = iv_title.

    DESCRIBE FIELD iv_text TYPE lv_type.
    IF lv_type CA 'bspdfDT'.
      WRITE iv_text TO ms_outtab-text LEFT-JUSTIFIED.
    ELSE.
      ms_outtab-text = iv_text.
    ENDIF.

    ms_outtab-value = iv_value.
    DESCRIBE FIELD iv_value TYPE lv_type.
    IF lv_type CA 'bspdf'.
      SHIFT ms_outtab-value LEFT DELETING LEADING space.
    ENDIF.

    " Add node to tree
    _add_node(
      is_outtab = ms_outtab
      iv_icon   = iv_icon
      iv_color  = iv_color
      iv_level  = iv_level
      iv_sign   = iv_sign
      iv_hidden = iv_hidden ).

  ENDMETHOD.


  METHOD _add_node.

    DATA:
      lv_node_text   TYPE lvc_value,
      ls_node_layout TYPE lvc_s_layn.

    FIELD-SYMBOLS:
      <lv_field>       TYPE any,
      <ls_fieldcat>    TYPE lvc_s_fcat,
      <ls_item_layout> TYPE lvc_s_layi.

    " Out data
    ms_outtab = is_outtab.
    ms_outtab-node_key   = mv_node_key.
    ms_outtab-relatkey   = mv_relat_key.
    ms_outtab-tree_level = iv_level.

    IF iv_sign = abap_true.
      LOOP AT mt_fieldcat ASSIGNING <ls_fieldcat> FROM 4.
        ASSIGN COMPONENT <ls_fieldcat>-fieldname OF STRUCTURE ms_outtab TO <lv_field>.
        IF sy-subrc = 0 AND <lv_field> IS NOT INITIAL.
          CONCATENATE '- (' <lv_field> ')' INTO <lv_field> SEPARATED BY space.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Output layout
    LOOP AT mt_item_layout ASSIGNING <ls_item_layout>.
      IF iv_hidden IS INITIAL.
        CASE iv_color.
          WHEN 0.
            <ls_item_layout>-style = cl_gui_column_tree=>style_inherited.
          WHEN 1.
            <ls_item_layout>-style = cl_gui_column_tree=>style_default.
          WHEN 2.
            <ls_item_layout>-style = cl_gui_column_tree=>style_intensified.
          WHEN OTHERS.
            <ls_item_layout>-style = cl_gui_column_tree=>style_default.
        ENDCASE.
      ELSE.
        <ls_item_layout>-style = cl_gui_column_tree=>style_inactive.
      ENDIF.

      IF <ls_item_layout>-fieldname = 'TEXT' AND ms_outtab-text CS '3.x'.
        <ls_item_layout>-t_image = icon_parameter.
      ELSE.
        <ls_item_layout>-t_image = ''.
      ENDIF.
    ENDLOOP.

    " Get node text
    ASSIGN COMPONENT 3 OF STRUCTURE ms_outtab TO <lv_field>.
    IF sy-subrc = 0.
      lv_node_text = <lv_field>.
    ENDIF.

    " Get node icon
    CLEAR ls_node_layout.

    IF iv_icon BETWEEN icon_equal_green AND icon_pattern_exclude_red.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name                  = iv_icon
        IMPORTING
          result                = ls_node_layout-n_image
        EXCEPTIONS
          icon_not_found        = 1
          outputfield_too_short = 2
          OTHERS                = 3 ##FM_SUBRC_OK.
    ELSE.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name                  = iv_icon
          info                  = lv_node_text
        IMPORTING
          result                = ls_node_layout-n_image
        EXCEPTIONS
          icon_not_found        = 1
          outputfield_too_short = 2
          OTHERS                = 3 ##FM_SUBRC_OK.
    ENDIF.

    ls_node_layout-exp_image = ls_node_layout-n_image.

    " Add node to tree
    mo_tree->add_node(
      EXPORTING
        i_relat_node_key     = mv_relat_key
        i_relationship       = cl_gui_column_tree=>relat_last_child
        is_outtab_line       = ms_outtab
        is_node_layout       = ls_node_layout
        it_item_layout       = mt_item_layout
        i_node_text          = lv_node_text
      IMPORTING
        e_new_node_key       = mv_node_key
      EXCEPTIONS
        relat_node_not_found = 1
        node_not_found       = 2
        OTHERS               = 3 ).
    IF sy-subrc <> 0.
      MESSAGE e000 WITH 'Error in ADD_NODE' ##NO_TEXT.
    ENDIF.

    mv_node_key = mv_node_key + 1.

  ENDMETHOD.


  METHOD _init.

    CONSTANTS: lc_width_header TYPE i VALUE 60.

    DATA:
      ls_event            TYPE cntl_simple_event,
      lt_events           TYPE cntl_simple_events,
      ls_variant          TYPE disvariant,
      ls_hierarchy_header TYPE treev_hhdr,
      lt_exclude          TYPE ui_functions,
      ls_exclude          TYPE LINE OF ui_functions.

    FIELD-SYMBOLS:
      <ls_fieldcat>    TYPE lvc_s_fcat,
      <ls_item_layout> TYPE lvc_s_layi.

    " Create a container for the tree control
    CHECK mo_tree IS INITIAL.

    " Link the container the custom control on the dynpro
    IF sy-batch IS INITIAL.
      CREATE OBJECT mo_custom_container
        EXPORTING
          container_name              = mv_container_name
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.
      IF sy-subrc <> 0.
        MESSAGE e000 WITH 'Error in INIT Create Container' ##NO_TEXT.
      ENDIF.
    ENDIF.

    " Create a tree control
    CREATE OBJECT mo_tree
      EXPORTING
        parent                      = mo_custom_container
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
        item_selection              = abap_true
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
      MESSAGE e000 WITH 'Error in INIT Create Tree' ##NO_TEXT.
    ENDIF.

    " Define the events which will be passed to the backend
    ls_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.

    ls_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.

    mo_tree->set_registered_events(
      EXPORTING
        events                    = lt_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3 ).
    IF sy-subrc <> 0.
      MESSAGE e000 WITH 'Error in INIT Register Events' ##NO_TEXT.
    ENDIF.

    " Assign event handlers in the application class to each desired event
    SET HANDLER handle_node_double_click FOR mo_tree.
    SET HANDLER handle_item_double_click FOR mo_tree.

    " Get field catalog
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = mv_tree_structure
        i_bypassing_buffer     = space
      CHANGING
        ct_fieldcat            = mt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE e000 WITH 'Error in INIT Field Catalog Merge' ##NO_TEXT.
    ENDIF.

    " Build column layout
    CLEAR mt_item_layout.
    LOOP AT mt_fieldcat ASSIGNING <ls_fieldcat>.
      " Hide the key columns and level
      IF sy-tabix <= 3 OR <ls_fieldcat>-fieldname = 'TREE_LEVEL'.
        <ls_fieldcat>-no_out = abap_true.
      ENDIF.

      APPEND INITIAL LINE TO mt_item_layout ASSIGNING <ls_item_layout>.
      <ls_item_layout>-fieldname = <ls_fieldcat>-fieldname.
      <ls_item_layout>-class     = cl_gui_column_tree=>item_class_text.
    ENDLOOP.

    " Use current program name for saving variants
    CLEAR ls_variant.
    ls_variant-report = sy-repid.

    " Setup the hierarchy header
    CLEAR ls_hierarchy_header.
    ls_hierarchy_header-t_image = icon_sap.
    ls_hierarchy_header-heading = 'Objects'(001).
    ls_hierarchy_header-width   = lc_width_header.

    " Exclude SUM button from toolbar
    CLEAR ls_exclude.
    ls_exclude = cl_gui_alv_tree=>mc_fc_calculate.
    APPEND ls_exclude TO lt_exclude.

    " Create empty tree-control
    " Note: mt_outtab must be empty, since we add nodes dynamically
    mo_tree->set_table_for_first_display(
      EXPORTING
        is_hierarchy_header  = ls_hierarchy_header
        i_save               = 'A'
        is_variant           = ls_variant
        it_toolbar_excluding = lt_exclude
      CHANGING
        it_outtab            = mt_outtab
        it_fieldcatalog      = mt_fieldcat ).

    " Initialize node counter
    mv_node_key = 1.

  ENDMETHOD.
ENDCLASS.
