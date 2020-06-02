************************************************************************
* /MBTOOLS/CL_TREE
* MBT Tree
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_tree DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS icon .

    DATA gv_container_name TYPE char25 VALUE 'TREE_CONTAINER' ##NO_TEXT.
    DATA gr_custom_container TYPE REF TO cl_gui_custom_container .
    DATA gr_tree TYPE REF TO cl_gui_alv_tree .

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
        !i_ok_code TYPE sy-ucomm .
    METHODS display .
    METHODS download .
    METHODS print .
    METHODS destroy .
    METHODS expand
      IMPORTING
        VALUE(i_level) TYPE i .
    METHODS expand_all .
    METHODS add_top_node
      IMPORTING
        VALUE(i_title) TYPE csequence
        VALUE(i_icon)  TYPE icon_d OPTIONAL
        VALUE(i_text)  TYPE any OPTIONAL
        VALUE(i_value) TYPE any OPTIONAL
        VALUE(i_type)  TYPE csequence OPTIONAL .
    METHODS add_sub_node
      IMPORTING
        VALUE(i_title) TYPE csequence
        VALUE(i_icon)  TYPE icon_d OPTIONAL
        VALUE(i_text)  TYPE any OPTIONAL
        VALUE(i_value) TYPE any OPTIONAL
        VALUE(i_type)  TYPE csequence OPTIONAL .
    METHODS add_detail
      IMPORTING
        VALUE(i_title)  TYPE csequence
        VALUE(i_icon)   TYPE icon_d OPTIONAL
        VALUE(i_text)   TYPE any OPTIONAL
        VALUE(i_value)  TYPE any OPTIONAL
        VALUE(i_level)  TYPE i OPTIONAL
        VALUE(i_sign)   TYPE abap_bool DEFAULT abap_false
        VALUE(i_hidden) TYPE abap_bool DEFAULT abap_false
        VALUE(i_type)   TYPE csequence OPTIONAL .
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

    DATA mv_tree_structure TYPE tabname VALUE '/MBTOOLS/TREE_CONTROL' ##NO_TEXT.
    DATA ms_outtab TYPE /mbtools/tree_control .
    DATA:
      mt_outtab TYPE STANDARD TABLE OF /mbtools/tree_control .
    DATA mv_node_key TYPE lvc_nkey .
    DATA mv_relat_key TYPE lvc_nkey .
    DATA mt_fieldcat TYPE lvc_t_fcat .
    DATA mt_item_layout TYPE lvc_t_layi .
    DATA mt_typtab TYPE lvc_t_chit .
    DATA mv_tree_level TYPE i .
    DATA mv_done TYPE abap_bool .

    METHODS init .
    METHODS add_node
      IMPORTING
        VALUE(is_outtab) TYPE any
        VALUE(i_icon)    TYPE icon_d OPTIONAL
        VALUE(i_color)   TYPE i OPTIONAL
        VALUE(i_level)   TYPE i OPTIONAL
        VALUE(i_sign)    TYPE abap_bool DEFAULT abap_false
        VALUE(i_hidden)  TYPE abap_bool DEFAULT abap_false .
    METHODS add
      IMPORTING
        VALUE(i_title)  TYPE csequence
        VALUE(i_icon)   TYPE icon_d OPTIONAL
        VALUE(i_text)   TYPE any OPTIONAL
        VALUE(i_value)  TYPE any OPTIONAL
        VALUE(i_color)  TYPE i OPTIONAL
        VALUE(i_level)  TYPE i OPTIONAL
        VALUE(i_sign)   TYPE abap_bool DEFAULT abap_false
        VALUE(i_hidden) TYPE abap_bool DEFAULT abap_false
        VALUE(i_type)   TYPE csequence OPTIONAL .
ENDCLASS.



CLASS /MBTOOLS/CL_TREE IMPLEMENTATION.


  METHOD add.

    DATA:
      ls_typtab TYPE lvc_s_chit,
      l_type    TYPE c.

    " Node types
    ls_typtab-nodekey   = mv_node_key.
    ls_typtab-fieldname = i_type.
    INSERT ls_typtab INTO TABLE mt_typtab.

    " Output data
    " - convert text and value to type char 255
    "   although ALV tree currently limits output to char 128
    " - format numeric data
    CLEAR ms_outtab.
    ms_outtab-object = i_title.

    DESCRIBE FIELD i_text TYPE l_type.
    IF l_type CA 'bspdfDT'.
      WRITE i_text TO ms_outtab-text LEFT-JUSTIFIED.
    ELSE.
      ms_outtab-text = i_text.
    ENDIF.

    DESCRIBE FIELD i_value TYPE l_type.
    IF l_type CA 'bspdf'.
      ms_outtab-value = i_value.
      SHIFT ms_outtab-value LEFT DELETING LEADING space.
    ELSE.
      ms_outtab-value = i_value.
    ENDIF.

    " Add node to tree
    CALL METHOD add_node
      EXPORTING
        is_outtab = ms_outtab
        i_icon    = i_icon
        i_color   = i_color
        i_level   = i_level
        i_sign    = i_sign
        i_hidden  = i_hidden.

  ENDMETHOD.


  METHOD add_detail .

    CALL METHOD add
      EXPORTING
        i_icon   = i_icon
        i_title  = i_title
        i_text   = i_text
        i_value  = i_value
        i_color  = 3
        i_level  = i_level
        i_sign   = i_sign
        i_hidden = i_hidden
        i_type   = i_type.

  ENDMETHOD.


  METHOD add_node.

    DATA:
      l_node_text    TYPE lvc_value,
      ls_node_layout TYPE lvc_s_layn.

    FIELD-SYMBOLS:
      <l_field>        TYPE any,
      <ls_fieldcat>    TYPE lvc_s_fcat,
      <ls_item_layout> TYPE lvc_s_layi.

    " Out data
    ms_outtab = is_outtab.
    ms_outtab-node_key = mv_node_key.
    ms_outtab-relatkey = mv_relat_key.

    IF i_sign = abap_true.
      LOOP AT mt_fieldcat ASSIGNING <ls_fieldcat> FROM 4.
        ASSIGN COMPONENT <ls_fieldcat>-fieldname OF STRUCTURE ms_outtab TO <l_field>.
        IF sy-subrc = 0 AND NOT <l_field> IS INITIAL.
          CONCATENATE '- (' <l_field> ')' INTO <l_field> SEPARATED BY space.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Output layout
    LOOP AT mt_item_layout ASSIGNING <ls_item_layout>.
      IF i_hidden IS INITIAL.
        CASE i_color.
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
    ENDLOOP.

    " Get node text
    ASSIGN COMPONENT 3 OF STRUCTURE ms_outtab TO <l_field>.
    IF sy-subrc = 0.
      l_node_text = <l_field>.
    ENDIF.

    " Get node icon
    CLEAR ls_node_layout.

    IF i_icon BETWEEN icon_equal_green AND icon_pattern_exclude_red.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name                  = i_icon
        IMPORTING
          result                = ls_node_layout-n_image
        EXCEPTIONS
          icon_not_found        = 0
          outputfield_too_short = 0
          OTHERS                = 0.
    ELSE.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name                  = i_icon
          info                  = l_node_text
        IMPORTING
          result                = ls_node_layout-n_image
        EXCEPTIONS
          icon_not_found        = 0
          outputfield_too_short = 0
          OTHERS                = 0.
    ENDIF.

    ls_node_layout-exp_image = ls_node_layout-n_image.

    " Add node to tree
    CALL METHOD gr_tree->add_node
      EXPORTING
        i_relat_node_key     = mv_relat_key
        i_relationship       = cl_gui_column_tree=>relat_last_child
        is_outtab_line       = ms_outtab
        is_node_layout       = ls_node_layout
        it_item_layout       = mt_item_layout
        i_node_text          = l_node_text
      IMPORTING
        e_new_node_key       = mv_node_key
      EXCEPTIONS
        relat_node_not_found = 1
        node_not_found       = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      MESSAGE e000 WITH 'Error in ADD_NODE' ##NO_TEXT.
    ENDIF.

    mv_node_key = mv_node_key + 1.

  ENDMETHOD.


  METHOD add_sub_node .

    CALL METHOD add
      EXPORTING
        i_icon   = i_icon
        i_title  = i_title
        i_text   = ''
        i_value  = i_value
        i_color  = 2
        i_level  = 0
        i_sign   = space
        i_hidden = space
        i_type   = i_type.

  ENDMETHOD.


  METHOD add_top_node .

    " Link to root
    CLEAR mv_relat_key.

    CALL METHOD add
      EXPORTING
        i_icon   = i_icon
        i_title  = i_title
        i_text   = i_text
        i_value  = i_value
        i_color  = 1
        i_level  = 0
        i_sign   = space
        i_hidden = space
        i_type   = i_type.

    " Initialize relationship counter
    mv_relat_key = 1.

  ENDMETHOD.


  METHOD constructor.
    mv_tree_level = 2.

    init( ).
  ENDMETHOD.


  METHOD destroy .

    " Destroy tree control
    IF NOT gr_tree IS INITIAL.

      CALL METHOD gr_tree->free.
      CLEAR gr_tree.

    ENDIF.

    " Destroy tree container
    IF NOT gr_custom_container IS INITIAL.

      CALL METHOD gr_custom_container->free.
      CLEAR gr_custom_container.

    ENDIF.

  ENDMETHOD.


  METHOD display .

    DATA:
      l_root TYPE lvc_nkey.

    CHECK mv_done IS INITIAL.

    " Calculate totals (is this really necessary?)
    CALL METHOD gr_tree->update_calculations
      EXPORTING
        no_frontend_update = abap_true.

    " Expand tree
    l_root = 1.
    IF mv_tree_level IS INITIAL.
      CALL METHOD gr_tree->expand_node
        EXPORTING
          i_node_key          = l_root
          i_expand_subtree    = abap_true
        EXCEPTIONS
          failed              = 1
          illegal_level_count = 2
          cntl_system_error   = 3
          node_not_found      = 4
          cannot_expand_leaf  = 5
          OTHERS              = 6.
    ELSE.
      CALL METHOD gr_tree->expand_node
        EXPORTING
          i_node_key          = l_root
          i_level_count       = mv_tree_level
        EXCEPTIONS
          failed              = 1
          illegal_level_count = 2
          cntl_system_error   = 3
          node_not_found      = 4
          cannot_expand_leaf  = 5
          OTHERS              = 6.
    ENDIF.
    IF sy-subrc <> 0.
      IF sy-subrc = 4.
        CALL METHOD add_top_node
          EXPORTING
            i_icon  = icon_dummy
            i_title = 'No selection'(002).
      ELSE.
        MESSAGE e000 WITH 'Error in EXPAND_NODE' ##NO_TEXT.
      ENDIF.
    ENDIF.

    " Optimize column width
    CALL METHOD gr_tree->column_optimize.

    " Finally display tree on frontend
    CALL METHOD gr_tree->frontend_update.

    CALL METHOD cl_gui_cfw=>flush.

    mv_done = abap_true.

  ENDMETHOD.


  METHOD download .

    MESSAGE i000 WITH 'Download the View Using "System > List > Save"'(t01).

    CALL METHOD gr_tree->set_function_code
      EXPORTING
        i_ucomm            = cl_gui_alv_tree=>mc_fc_print_prev
      EXCEPTIONS
        function_not_found = 1
        OTHERS             = 2.

  ENDMETHOD.


  METHOD expand.
    mv_tree_level = i_level.
  ENDMETHOD.


  METHOD expand_all.
    mv_tree_level = 0.
  ENDMETHOD.


  METHOD find_node .

    CALL METHOD gr_tree->set_function_code
      EXPORTING
        i_ucomm            = cl_gui_alv_tree=>mc_fc_find
      EXCEPTIONS
        function_not_found = 1
        OTHERS             = 2.

  ENDMETHOD.


  METHOD get_key.
    rv_key = mv_relat_key.
  ENDMETHOD.


  METHOD handle_item_double_click.

    " this method handles the item double click event of the tree control instance
    CALL METHOD me->handle_node_double_click
      EXPORTING
        node_key = node_key.

  ENDMETHOD.


  METHOD handle_node_double_click.

    " this method handles the node double click event of the tree control instance
    DATA:
      l_value    LIKE ms_outtab-value,
      ls_typtab  TYPE lvc_s_chit,
      l_nrobj    TYPE nrobj,
      l_iobjnm   TYPE rsiobjnm,
      l_mode     TYPE c LENGTH 1,
      l_compuid  TYPE sysuuid_25,
      l_vnam     TYPE rszvnam,
      l_service  TYPE rsplf_srvnm,
      l_plseq    TYPE rspls_seqnm,
      l_provider TYPE rsinfoprov,
      l_srvtype  TYPE rsplf_srvtypenm,
      l_r_lpogui TYPE REF TO cl_rslpo_gui,
      l_lpo      TYPE rslponame,
      l_tcode    TYPE sy-tcode,
      l_length   TYPE i.

    READ TABLE mt_outtab INTO ms_outtab
      WITH KEY node_key = node_key.
    CHECK sy-subrc = 0.

    l_value = ms_outtab-value.

    READ TABLE mt_typtab INTO ls_typtab
      WITH KEY nodekey = node_key.                      "#EC CI_HASHSEQ
    CHECK sy-subrc = 0.

    IF /mbtools/cl_sap=>show_object( i_object   = ls_typtab-fieldname
                                     i_obj_name = l_value ).
      RETURN.
    ENDIF.

    CASE ls_typtab-fieldname.
      WHEN /mbtools/if_objects=>c_dimension.
        l_length = strlen( l_value ) - 1.
        l_value = l_value(l_length).
        /mbtools/cl_sap=>show_object(
          i_object   = /mbtools/if_objects=>c_infocube
          i_obj_name = l_value ).

      WHEN /mbtools/if_objects=>c_aggrlevel.
        l_provider = l_value.
        CALL FUNCTION 'RSPLW_ALVL_MAINTAIN'
          EXPORTING
            i_aggrlevel = l_provider
            i_fcode     = 'DISPLAY'.

      WHEN /mbtools/if_objects=>c_sel_object.
        l_compuid = l_value.
        CALL FUNCTION 'RSPLW_SOB_MAINTAIN'
          EXPORTING
            i_compuid = l_compuid
            i_fcode   = 'DISPLAY'.

      WHEN /mbtools/if_objects=>c_variable.
        l_vnam = l_value.
        CALL FUNCTION 'RSPLW_VAR_DISPLAY'
          EXPORTING
            i_vnam = l_vnam.

      WHEN /mbtools/if_objects=>c_plan_provider
        OR /mbtools/if_objects=>c_char_relationship
        OR /mbtools/if_objects=>c_data_slice.
        l_provider = l_value.
        CALL FUNCTION 'RSPLW_PROV_MAINTAIN'
          EXPORTING
            i_provider = l_provider
            i_mode     = 'DISPLAY'.

      WHEN /mbtools/if_objects=>c_plan_service_type.
        l_srvtype = l_value.
        TRY.
            CALL FUNCTION 'RSPLFD_PLST_MAINT'
              EXPORTING
                i_srvtypenm = l_srvtype
                i_display   = rs_c_true.
          CATCH cx_rs_version_not_found.                "#EC NO_HANDLER
          CATCH cx_rs_msg.                              "#EC NO_HANDLER
        ENDTRY.

      WHEN /mbtools/if_objects=>c_plan_service.
        l_service = l_value.
        CALL FUNCTION 'RSPLW_PLFCT_MAINTAIN'
          EXPORTING
            i_service = l_service
            i_fcode   = 'DISPLAY'.

      WHEN /mbtools/if_objects=>c_plan_sequence.
        l_plseq = l_value.
        CALL FUNCTION 'RSPLW_SEQ_MAINTAIN'
          EXPORTING
            i_plseq = l_plseq
            i_fcode = 'DISPLAY'.

      WHEN /mbtools/if_objects=>c_lpo.
        TRY.
            l_lpo = l_value.
            CREATE OBJECT l_r_lpogui.
            CALL METHOD l_r_lpogui->show_ui
              EXPORTING
                i_lpo = l_lpo.
          CATCH cx_rslpo_root.                          "#EC NO_HANDLER
        ENDTRY.

      WHEN /mbtools/if_objects=>c_infoobject.
        l_iobjnm = l_value.
        CALL FUNCTION 'RSD_IOBJNM_PARSE'
          EXPORTING
            i_iobjnm = l_iobjnm
          IMPORTING
            e_iobjnm = l_iobjnm.
        SET PARAMETER ID 'RSC' FIELD l_iobjnm.
        /mbtools/cl_utilities=>call_transaction( iv_tcode = 'RSD1' ).

      WHEN /mbtools/if_objects=>c_hierarchy.
        /mbtools/cl_utilities=>call_transaction( iv_tcode = 'RSH1' ).

      WHEN /mbtools/if_objects=>c_query.
        SET PARAMETER ID 'GID' FIELD l_value.
        /mbtools/cl_utilities=>call_transaction( iv_tcode = 'RSRT' ).

      WHEN /mbtools/if_objects=>c_ctrt.
        SET PARAMETER ID 'NBR' FIELD l_value.
        /mbtools/cl_utilities=>call_transaction( iv_tcode = 'RSCUR' ).

      WHEN /mbtools/if_objects=>c_uomt.
        SET PARAMETER ID 'RSUOM' FIELD l_value.
        /mbtools/cl_utilities=>call_transaction( iv_tcode = 'RSUOM' ).

      WHEN /mbtools/if_objects=>c_thjt.
*      SET PARAMETER ID 'XXX' FIELD l_value.
        /mbtools/cl_utilities=>call_transaction( iv_tcode = 'RSTHJTMAINT' ).

      WHEN /mbtools/if_objects=>c_user_id.
        SET PARAMETER ID 'XUS' FIELD l_value.
        /mbtools/cl_utilities=>call_transaction( iv_tcode = 'SU01' ).

      WHEN /mbtools/if_objects=>c_role.
        SET PARAMETER ID 'PROFILE_GENERATOR' FIELD l_value.
        /mbtools/cl_utilities=>call_transaction( iv_tcode = 'PFCG' ).

      WHEN /mbtools/if_objects=>c_number_range.
        l_nrobj = l_value.
        l_mode  = 'U'.
        CALL FUNCTION 'NUMBER_RANGE_OBJECT_MAINTAIN'
          EXPORTING
            display_only     = ' '
            object           = l_nrobj
            mode             = l_mode
          EXCEPTIONS
            object_exists    = 1
            object_missing   = 2
            object_not_found = 3
            OTHERS           = 4.
        CHECK sy-subrc = 0.

      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.


  METHOD init .

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
    CHECK gr_tree IS INITIAL.

    " Link the container the custom control on the dynpro
    IF sy-batch IS INITIAL.
      CREATE OBJECT gr_custom_container
        EXPORTING
          container_name              = gv_container_name
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
    CREATE OBJECT gr_tree
      EXPORTING
        parent                      = gr_custom_container
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

    CALL METHOD gr_tree->set_registered_events
      EXPORTING
        events                    = lt_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.
    IF sy-subrc <> 0.
      MESSAGE e000 WITH 'Error in INIT Register Events' ##NO_TEXT.
    ENDIF.

    " Assign event handlers in the application class to each desired event
    SET HANDLER me->handle_node_double_click FOR gr_tree.
    SET HANDLER me->handle_item_double_click FOR gr_tree.

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
    REFRESH mt_item_layout.
    LOOP AT mt_fieldcat ASSIGNING <ls_fieldcat>.
      " Hide the key columns
      IF sy-tabix <= 3.
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
    ls_hierarchy_header-width   = 60.

    " Exclude SUM button from toolbar
    CLEAR ls_exclude.
    ls_exclude = cl_gui_alv_tree=>mc_fc_calculate.
    APPEND ls_exclude TO lt_exclude.

    " Create empty tree-control
    " Note: mt_outtab must be empty, since we add nodes dynamically
    CALL METHOD gr_tree->set_table_for_first_display
      EXPORTING
        is_hierarchy_header  = ls_hierarchy_header
        i_save               = 'A'
        is_variant           = ls_variant
        it_toolbar_excluding = lt_exclude
      CHANGING
        it_outtab            = mt_outtab
        it_fieldcatalog      = mt_fieldcat.

    " Initialize node counter
    mv_node_key = 1.

  ENDMETHOD.


  METHOD next_key.
    mv_relat_key = mv_node_key - 1.
  ENDMETHOD.


  METHOD pai.
    CASE i_ok_code.

        " Finish program
      WHEN 'BACK' OR 'EXIT' OR 'CANC'.
        CALL METHOD destroy.
        LEAVE TO SCREEN 0.

        " Pick node/item
      WHEN 'PICK'.
        CALL METHOD pick_node.

        " Find node/item
      WHEN 'FIND'.
        CALL METHOD find_node.

        " Download
      WHEN 'DOWN'.
        CALL METHOD download.

        " Print
      WHEN 'PRINT'.
        CALL METHOD print.

        " Dispatch to tree control
      WHEN OTHERS.
        CALL METHOD cl_gui_cfw=>dispatch.

    ENDCASE.

    CALL METHOD cl_gui_cfw=>flush.
  ENDMETHOD.


  METHOD pbo.
    display( ).
  ENDMETHOD.


  METHOD pick_node .

    DATA:
      ls_selected_nodes TYPE lvc_s_nkey,
      lt_selected_nodes TYPE lvc_t_nkey,
      l_node_key        TYPE lvc_nkey,
      l_fieldname       TYPE lvc_fname.                     "#EC NEEDED

    " Get node selection
    CALL METHOD gr_tree->get_selected_nodes
      CHANGING
        ct_selected_nodes = lt_selected_nodes
      EXCEPTIONS
        cntl_system_error = 1
        dp_error          = 2
        failed            = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      MESSAGE e000 WITH 'Error in PICK_NODE Selected Nodes' ##NO_TEXT.
    ENDIF.

    CASE lines( lt_selected_nodes ).
      WHEN 0.
        " No node selected, now check item selection
        CALL METHOD gr_tree->get_selected_item
          IMPORTING
            e_selected_node   = l_node_key
            e_fieldname       = l_fieldname
          EXCEPTIONS
            no_item_selection = 1
            cntl_system_error = 2
            failed            = 3
            OTHERS            = 4.

        CASE sy-subrc.
          WHEN 0.
            CALL METHOD me->handle_node_double_click
              EXPORTING
                node_key = l_node_key.
          WHEN 1.
            MESSAGE i227(0h).
        ENDCASE.

      WHEN 1.
        " Exactly one node selected
        READ TABLE lt_selected_nodes INTO ls_selected_nodes INDEX 1.

        CALL METHOD me->handle_node_double_click
          EXPORTING
            node_key = ls_selected_nodes-node_key.

      WHEN OTHERS.
        " Too many nodes selected
        MESSAGE i227(0h).

    ENDCASE.

  ENDMETHOD.


  METHOD print .

    CALL METHOD gr_tree->set_function_code
      EXPORTING
        i_ucomm            = cl_gui_alv_tree=>mc_fc_print_back
      EXCEPTIONS
        function_not_found = 1
        OTHERS             = 2.

  ENDMETHOD.


  METHOD set_key.
    mv_relat_key = iv_key.
  ENDMETHOD.
ENDCLASS.
