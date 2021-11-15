CLASS /mbtools/cl_tool_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Marc Bernard Tools - Tool Manager
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-or-later
************************************************************************
  PUBLIC SECTION.

    " Length of MBT Installer package name
    CONSTANTS c_name_length TYPE i VALUE 90 ##NO_TEXT.

    " Constructor
    CLASS-METHODS class_constructor.
    CLASS-METHODS init
      IMPORTING
        !iv_title TYPE string OPTIONAL.
    " Class Get
    CLASS-METHODS factory
      IMPORTING
        !iv_title      TYPE csequence DEFAULT /mbtools/cl_tool_bc=>c_tool-title
      RETURNING
        VALUE(ro_tool) TYPE REF TO /mbtools/cl_tool.
    CLASS-METHODS manifests
      RETURNING
        VALUE(rt_manifests) TYPE /mbtools/if_tool=>ty_manifests.
    CLASS-METHODS select
      IMPORTING
        VALUE(iv_pattern)        TYPE csequence OPTIONAL
        VALUE(iv_bundle_id)      TYPE i DEFAULT -1
        VALUE(iv_get_bundles)    TYPE abap_bool DEFAULT abap_false
        VALUE(iv_get_tools)      TYPE abap_bool DEFAULT abap_true
        VALUE(iv_get_extensions) TYPE abap_bool DEFAULT abap_false
        VALUE(iv_admin)          TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_manifests)      TYPE /mbtools/if_tool=>ty_manifests.
    CLASS-METHODS list
      IMPORTING
        VALUE(iv_pattern)        TYPE csequence OPTIONAL
        VALUE(iv_bundle_id)      TYPE i DEFAULT -1
        VALUE(iv_get_bundles)    TYPE abap_bool DEFAULT abap_false
        VALUE(iv_get_tools)      TYPE abap_bool DEFAULT abap_true
        VALUE(iv_get_extensions) TYPE abap_bool DEFAULT abap_false
        VALUE(iv_admin)          TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_tools)          TYPE /mbtools/if_tool=>ty_tools_with_text.
    CLASS-METHODS f4
      IMPORTING
        VALUE(iv_pattern)        TYPE csequence OPTIONAL
        VALUE(iv_bundle_id)      TYPE i DEFAULT -1
        VALUE(iv_get_bundles)    TYPE abap_bool DEFAULT abap_false
        VALUE(iv_get_tools)      TYPE abap_bool DEFAULT abap_true
        VALUE(iv_get_extensions) TYPE abap_bool DEFAULT abap_false
        VALUE(iv_admin)          TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_title)          TYPE string.
    CLASS-METHODS action_tools
      IMPORTING
        !iv_action       TYPE string
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS action_bundles
      IMPORTING
        !iv_action       TYPE string
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS install
      IMPORTING
        !iv_title        TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS check
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS update
      IMPORTING
        !io_tool         TYPE REF TO /mbtools/cl_tool
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS uninstall
      IMPORTING
        !io_tool         TYPE REF TO /mbtools/cl_tool
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS sync
      IMPORTING
        !io_tool         TYPE REF TO /mbtools/cl_tool
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS is_base_only
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        key      TYPE string,
        instance TYPE REF TO /mbtools/cl_tool,
      END OF ty_instance.
    TYPES:
    " Sync with zif_abapinst_definitions
      ty_name TYPE c LENGTH c_name_length.
    TYPES ty_pack TYPE devclass.
    TYPES:
      BEGIN OF ty_content,
        name TYPE ty_name,
        pack TYPE ty_pack,
        json TYPE string,
      END OF ty_content.
    TYPES:
      BEGIN OF ty_version,
        major           TYPE i,
        minor           TYPE i,
        patch           TYPE i,
        prerelase       TYPE string,
        prerelase_patch TYPE i,
      END OF ty_version.
    TYPES:
      BEGIN OF ty_inst,
        name            TYPE ty_name,
        pack            TYPE devclass,
        version         TYPE string,
        sem_version     TYPE ty_version,
        description     TYPE string,
        source_type     TYPE string,
        source_name     TYPE string,
        transport       TYPE trkorr,
        folder_logic    TYPE string,
        installed_langu TYPE sy-langu,
        installed_by    TYPE xubname,
        installed_at    TYPE timestamp,
        updated_by      TYPE xubname,
        updated_at      TYPE timestamp,
        status          TYPE sy-msgty,
      END OF ty_inst.
    TYPES:
      ty_classes TYPE STANDARD TABLE OF seoclsname WITH DEFAULT KEY.

    CLASS-DATA:
      gt_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY key.
    CLASS-DATA gi_log TYPE REF TO /mbtools/if_logger.

    CLASS-METHODS _clean_title
      IMPORTING
        !iv_title        TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE string.
    CLASS-METHODS _get_implementations
      IMPORTING
        VALUE(iv_quiet)   TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_classes) TYPE ty_classes.
    CLASS-METHODS _sync_json
      IMPORTING
        !is_inst          TYPE ty_inst
      RETURNING
        VALUE(rs_content) TYPE ty_content
      RAISING
        /mbtools/cx_exception.
ENDCLASS.



CLASS /mbtools/cl_tool_manager IMPLEMENTATION.


  METHOD action_bundles.

    DATA:
      ls_manifest  TYPE /mbtools/if_tool=>ty_manifest,
      lt_manifests TYPE /mbtools/if_tool=>ty_manifests,
      li_progress  TYPE REF TO /mbtools/if_progress,
      lv_result    TYPE abap_bool.

    " Just bundles
    lt_manifests = select(
      iv_get_bundles = abap_true
      iv_get_tools   = abap_false
      iv_admin       = abap_true ).

    rv_result = abap_true.

    li_progress = /mbtools/cl_progress=>get_instance( lines( lt_manifests ) ).

    LOOP AT lt_manifests INTO ls_manifest.

      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |Run action for { ls_manifest-name }| ).

      " Register, unregister
      TRY.
          CASE iv_action.
            WHEN /mbtools/if_actions=>tool_register.
              lv_result = ls_manifest-manager->register( ).
            WHEN /mbtools/if_actions=>tool_unregister.
              lv_result = ls_manifest-manager->unregister( ).
            WHEN OTHERS.
              " unknow action
              ASSERT 0 = 1.
          ENDCASE.
        CATCH /mbtools/cx_exception ##NO_HANDLER.
      ENDTRY.

      IF lv_result = abap_false.
        rv_result = abap_false.
      ENDIF.

    ENDLOOP.

    li_progress->hide( ).

  ENDMETHOD.


  METHOD action_tools.

    DATA:
      ls_manifest  TYPE /mbtools/if_tool=>ty_manifest,
      lt_manifests TYPE /mbtools/if_tool=>ty_manifests,
      li_progress  TYPE REF TO /mbtools/if_progress,
      lv_result    TYPE abap_bool.

    IF iv_action = /mbtools/if_actions=>tools_check.
      rv_result = check( ).
      RETURN.
    ENDIF.

    gi_log->timer_start( ).

    " Just tools (no bundles)
    lt_manifests = select( iv_admin = abap_true ).

    rv_result = abap_true.

    li_progress = /mbtools/cl_progress=>get_instance( lines( lt_manifests ) ).

    gi_log->i( |Run action { iv_action } for { lines( lt_manifests ) } tools| ).

    LOOP AT lt_manifests INTO ls_manifest.

      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |Run action for { ls_manifest-name }| ).

      gi_log->i( ls_manifest-name ).

      TRY.
          CASE iv_action.
            WHEN /mbtools/if_actions=>tool_register.
              lv_result = ls_manifest-manager->register( ).
            WHEN /mbtools/if_actions=>tool_unregister.
              lv_result = ls_manifest-manager->unregister( ).
            WHEN /mbtools/if_actions=>tool_activate.
              lv_result = ls_manifest-manager->activate( ).
            WHEN /mbtools/if_actions=>tool_deactivate.
              lv_result = ls_manifest-manager->deactivate( ).
            WHEN /mbtools/if_actions=>tool_check.
              lv_result = ls_manifest-manager->check_version( abap_true ).
            WHEN /mbtools/if_actions=>tool_install.
              lv_result = install( ls_manifest-name ).
            WHEN /mbtools/if_actions=>tool_update.
              lv_result = update( ls_manifest-manager ).
            WHEN /mbtools/if_actions=>tool_uninstall.
              lv_result = uninstall( ls_manifest-manager ).
            WHEN /mbtools/if_actions=>tool_sync.
              lv_result = sync( ls_manifest-manager ).
            WHEN OTHERS.
              " unknow action
              ASSERT 0 = 1.
          ENDCASE.
        CATCH /mbtools/cx_exception.
          lv_result = abap_false.
      ENDTRY.

      IF lv_result = abap_false.
        rv_result = abap_false.
      ENDIF.

    ENDLOOP.

    li_progress->hide( ).

    gi_log->timer_end( ).

  ENDMETHOD.


  METHOD check.

    DATA:
      ls_manifest  TYPE /mbtools/if_tool=>ty_manifest,
      lt_manifests TYPE /mbtools/if_tool=>ty_manifests,
      ls_product   TYPE /mbtools/cl_edd=>ty_product,
      lt_products  TYPE /mbtools/cl_edd=>ty_products,
      li_progress  TYPE REF TO /mbtools/if_progress,
      lv_result    TYPE abap_bool.

    gi_log->timer_start( ).

    " Just tools (no bundles)
    lt_manifests = select( iv_admin = abap_true ).

    rv_result = abap_true.

    li_progress = /mbtools/cl_progress=>get_instance( lines( lt_manifests ) ).

    li_progress->show(
      iv_current = sy-tabix
      iv_text    = |Run check version for all tools| ).

    gi_log->i( |Run check version for { lines( lt_manifests ) } tools| ).

    LOOP AT lt_manifests INTO ls_manifest.
      ls_product-id      = ls_manifest-download_id.
      ls_product-license = ls_manifest-manager->get_license( )-key.
      INSERT ls_product INTO TABLE lt_products.
    ENDLOOP.

    TRY.
        /mbtools/cl_edd=>get_versions( CHANGING ct_products = lt_products ).

        LOOP AT lt_manifests INTO ls_manifest.
          READ TABLE lt_products INTO ls_product WITH TABLE KEY id = ls_manifest-download_id.
          ASSERT sy-subrc = 0.

          lv_result = ls_manifest-manager->update_version(
            iv_force         = abap_true
            iv_version       = ls_product-version
            iv_description   = ls_product-description
            iv_changelog_url = ls_product-changelog_url
            iv_changelog     = ls_product-changelog
            iv_download_url  = ls_product-download_url ).

          IF lv_result = abap_false.
            rv_result = abap_false.
          ENDIF.
        ENDLOOP.

      CATCH /mbtools/cx_exception.
        rv_result = abap_false.
    ENDTRY.

    li_progress->hide( ).

    gi_log->timer_end( ).

  ENDMETHOD.


  METHOD class_constructor.

    LOG-POINT ID /mbtools/bc SUBKEY /mbtools/cl_tool_bc=>c_tool-title FIELDS sy-datum sy-uzeit sy-uname.

    gi_log = /mbtools/cl_logger_factory=>create_log( 'LOG' ).

    TRY.
        " Initialize tool instances
        init( ).

      CATCH /mbtools/cx_exception.
        " MBT Base is not installed properly. Contact Marc Bernard Tools
        ASSERT 0 = 1.
    ENDTRY.

  ENDMETHOD.


  METHOD f4.

    DATA:
      lt_tools  TYPE /mbtools/if_tool=>ty_tools_with_text,
      ls_return TYPE ddshretval,
      lt_return TYPE TABLE OF ddshretval.

    lt_tools = list(
      iv_pattern        = iv_pattern
      iv_bundle_id      = iv_bundle_id
      iv_get_bundles    = iv_get_bundles
      iv_get_tools      = iv_get_tools
      iv_get_extensions = iv_get_extensions
      iv_admin          = iv_admin ).

    " Show F4-Popup
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'NAME'
        window_title    = 'Tools'(001)
        value_org       = 'S'
      TABLES
        value_tab       = lt_tools
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    LOOP AT lt_return INTO ls_return.
      rv_title = ls_return-fieldval.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD factory.

    DATA ls_instance LIKE LINE OF gt_instances.

    READ TABLE gt_instances INTO ls_instance WITH TABLE KEY key = _clean_title( iv_title ).
    ASSERT sy-subrc = 0.

    ro_tool = ls_instance-instance.

  ENDMETHOD.


  METHOD init.

    DATA:
      ls_instance        LIKE LINE OF gt_instances,
      lt_implementations TYPE ty_classes,
      lv_implementation  TYPE LINE OF ty_classes,
      li_tool            TYPE REF TO /mbtools/if_tool.

    IF iv_title IS INITIAL.
      CLEAR gt_instances.
    ELSE.
      DELETE gt_instances WHERE key = iv_title.
    ENDIF.

    lt_implementations = _get_implementations( ).

    LOOP AT lt_implementations INTO lv_implementation.  "#EC CI_NOORDER

      CLEAR ls_instance.

      TRY.
          " Get instance of tool
          CREATE OBJECT li_tool TYPE (lv_implementation).
          IF li_tool IS NOT BOUND.
            CONTINUE. "ignore
          ENDIF.

          IF iv_title IS NOT INITIAL AND iv_title <> li_tool->title( ).
            CONTINUE.
          ENDIF.

          ls_instance-key = li_tool->title( ).

          CREATE OBJECT ls_instance-instance EXPORTING io_tool = li_tool.

          INSERT ls_instance INTO TABLE gt_instances.

        CATCH cx_root.
          CONTINUE. "ignore
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD install.

    DATA lv_file TYPE string.

    lv_file = iv_title && '-x.y.z.zip'.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_file WITH '-'.

    SUBMIT /mbtools/mbt_installer
      WITH p_zip_f = abap_true
      WITH p_zip_s = abap_false
      WITH p_zip_i = abap_false
      WITH p_file_f = lv_file
      WITH p_file_s = ''
      WITH p_file_i = ''
      VIA SELECTION-SCREEN AND RETURN.

    init( ).

    rv_result = abap_true.

  ENDMETHOD.


  METHOD is_base_only.

    DATA lt_manifests TYPE /mbtools/if_tool=>ty_manifests.

    " Get all installed and active tools
    lt_manifests = select( ).

    IF lt_manifests IS INITIAL.
      " This means there's only MBT Base left as the last tool
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD list.

    DATA:
      ls_manifest  TYPE /mbtools/if_tool=>ty_manifest,
      lt_manifests TYPE /mbtools/if_tool=>ty_manifests,
      ls_tool      LIKE LINE OF rt_tools.

    lt_manifests = select(
      iv_pattern        = iv_pattern
      iv_bundle_id      = iv_bundle_id
      iv_get_bundles    = iv_get_bundles
      iv_get_tools      = iv_get_tools
      iv_get_extensions = iv_get_extensions
      iv_admin          = iv_admin ).

    LOOP AT lt_manifests INTO ls_manifest.

      CLEAR ls_tool.
      ls_tool-name        = ls_manifest-title.
      ls_tool-version     = ls_manifest-version.
      ls_tool-description = ls_manifest-description.
      INSERT ls_tool INTO TABLE rt_tools.

    ENDLOOP.

    SORT rt_tools BY name AS TEXT.

  ENDMETHOD.


  METHOD manifests.

    DATA:
      ls_instance LIKE LINE OF gt_instances,
      ls_manifest TYPE /mbtools/if_tool=>ty_manifest.

    LOOP AT gt_instances INTO ls_instance.
      ls_manifest = ls_instance-instance->get_manifest( ).
      INSERT ls_manifest INTO TABLE rt_manifests.
    ENDLOOP.

    SORT rt_manifests BY name.

  ENDMETHOD.


  METHOD select.

    DATA:
      ls_instance LIKE LINE OF gt_instances,
      ls_manifest TYPE /mbtools/if_tool=>ty_manifest,
      lo_tool     TYPE REF TO /mbtools/cl_tool.

    LOOP AT gt_instances INTO ls_instance.

      lo_tool = ls_instance-instance.

      " Filter by bundle
      IF iv_bundle_id >= 0 AND lo_tool->get_bundle_id( ) <> iv_bundle_id.
        CONTINUE.
      ENDIF.

      " Filter by bundle/tool/extension type
      IF lo_tool->is_bundle( ) = abap_true.
        IF iv_get_bundles = abap_false.
          CONTINUE.
        ENDIF.
      ELSEIF lo_tool->is_extension( ) = abap_true.
        IF iv_get_extensions = abap_false.
          CONTINUE.
        ENDIF.
      ELSE.
        IF iv_get_tools = abap_false.
          CONTINUE.
        ENDIF.
        " Filter by admin
        IF iv_admin = abap_false.
          " No inactive
          IF lo_tool->is_active( ) = abap_false.
            CONTINUE.
          ENDIF.
          " No MBT Base
          IF lo_tool->is_base( ) = abap_true.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

      " Filter by pattern
      IF iv_pattern IS NOT INITIAL AND lo_tool->get_title( ) NP iv_pattern.
        CONTINUE.
      ENDIF.

      ls_manifest = lo_tool->get_manifest( ).
      INSERT ls_manifest INTO TABLE rt_manifests.

    ENDLOOP.

    SORT rt_manifests BY bundle_id name is_extension.

  ENDMETHOD.


  METHOD sync.

    " Installer persistence
    CONSTANTS lc_tabname TYPE tabname VALUE 'ZMBTINST'.

    DATA:
      lv_name TYPE string,
      ls_inst TYPE ty_inst,
      ls_cont TYPE ty_content.

    lv_name = io_tool->get_name( ).
    SELECT SINGLE * FROM (lc_tabname) INTO ls_inst WHERE name = lv_name.
    IF sy-subrc = 0.
      ls_inst-pack            = io_tool->get_package( ).
      ls_inst-version         = io_tool->get_version( ).
      ls_inst-sem_version     = /mbtools/cl_version=>convert_string_to_version( ls_inst-version ).
      ls_inst-description     = io_tool->get_description( ).
      ls_inst-source_type     = 'INTERNET'.
      ls_inst-source_name     = io_tool->get_url_download( ).
      ls_inst-transport       = ''.
      ls_inst-folder_logic    = 'PREFIX'.
      ls_inst-installed_langu = 'E'.
      ls_inst-installed_by    = sy-uname.
      ls_inst-installed_at    = io_tool->get_last_update( abap_true ).
      ls_inst-status          = 'I'.
      " Update
      ls_cont = _sync_json( ls_inst ).
      UPDATE (lc_tabname) FROM ls_cont.
      IF sy-subrc <> 0.
        /mbtools/cx_exception=>raise( 'Error updating MBT Installer persistence'(003) ).
      ENDIF.
    ELSE.
      ls_inst-name            = io_tool->get_name( ).
      ls_inst-pack            = io_tool->get_package( ).
      ls_inst-version         = io_tool->get_version( ).
      ls_inst-sem_version     = /mbtools/cl_version=>convert_string_to_version( ls_inst-version ).
      ls_inst-description     = io_tool->get_description( ).
      ls_inst-source_type     = 'INTERNET'.
      ls_inst-source_name     = io_tool->get_url_download( ).
      ls_inst-transport       = ''.
      ls_inst-folder_logic    = 'PREFIX'.
      ls_inst-installed_langu = 'E'.
      ls_inst-installed_by    = sy-uname.
      ls_inst-installed_at    = io_tool->get_last_update( abap_true ).
      ls_inst-status          = 'I'.
      " Insert
      ls_cont = _sync_json( ls_inst ).
      INSERT (lc_tabname) FROM ls_cont.
      IF sy-subrc <> 0.
        /mbtools/cx_exception=>raise( 'Error inserting MBT Installer persistence'(002) ).
      ENDIF.
    ENDIF.

    rv_result = abap_true.

  ENDMETHOD.


  METHOD uninstall.

    DATA:
      lo_popup  TYPE REF TO /mbtools/if_popups,
      lv_name   TYPE string,
      lv_pack   TYPE string,
      lv_answer TYPE sy-input.

    lo_popup = /mbtools/cl_gui_factory=>get_popups( ).

    TRY.
        lv_answer = lo_popup->popup_to_confirm(
          iv_titlebar       = 'Marc Bernard Tools - Uninstall'
          iv_text_question  = |Are you sure, you want to uninstall { io_tool->get_name( ) }?|
          iv_default_button = '2' ).

        IF lv_answer = '1'.
          lv_name = io_tool->get_title( ).
          lv_pack = io_tool->get_package( ).

          SUBMIT /mbtools/mbt_installer
            WITH p_drop_n = lv_name
            WITH p_drop_p = lv_pack
            AND RETURN.

          " Unregister tool
          rv_result = io_tool->unregister( ).

          IF rv_result = abap_true.
            init( ).
          ENDIF.
        ENDIF.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD update.

    DATA:
      lv_url  TYPE string,
      lv_file TYPE string.

    IF io_tool->get_new_version( ) IS INITIAL.
      RETURN.
    ENDIF.

    lv_url = io_tool->get_url_download( ).

    " Safety checks
    IF lv_url IS INITIAL.
      RETURN.
    ELSEIF lv_url NS /mbtools/if_definitions=>c_www_home.
      RETURN.
    ENDIF.

    TRY.
        lv_file = /mbtools/cl_url=>name( lv_url ).
      CATCH /mbtools/cx_exception ##NO_HANDLER.
    ENDTRY.

    IF /mbtools/cl_mbt=>is_online( ) = abap_true.
      " URL and no selection screen
      SUBMIT /mbtools/mbt_installer
        WITH p_file_f = lv_file
        WITH p_file_i = lv_url
        WITH p_file_s = lv_file
        WITH p_zip_f  = abap_false
        WITH p_zip_i  = abap_true
        WITH p_zip_s  = abap_false
        AND RETURN.
    ELSE.
      " Local file and via selection screen
      SUBMIT /mbtools/mbt_installer
        WITH p_file_f = lv_file
        WITH p_file_i = lv_url
        WITH p_file_s = lv_file
        WITH p_zip_f  = abap_true
        WITH p_zip_i  = abap_false
        WITH p_zip_s  = abap_false
        VIA SELECTION-SCREEN AND RETURN.
    ENDIF.

    " Register tool
    rv_result = io_tool->register( ).

    IF rv_result = abap_true.
      init( ).
    ENDIF.

  ENDMETHOD.


  METHOD _clean_title.

    " Input could be title or name of tool
    rv_result = iv_title.
    IF iv_title CA '_'.
      REPLACE ALL OCCURRENCES OF '_' IN rv_result WITH ` `.
    ENDIF.

  ENDMETHOD.


  METHOD _get_implementations.

    DATA:
      ls_intkey  TYPE seoclskey,
      ls_impkeys TYPE seor_implementing_key,
      lt_impkeys TYPE seor_implementing_keys.

    " Get all classes that implement the MBT Interface
    ls_intkey-clsname = /mbtools/if_definitions=>c_interface.

    CALL FUNCTION 'SEO_INTERFACE_IMPLEM_GET_ALL'
      EXPORTING
        intkey       = ls_intkey
      IMPORTING
        impkeys      = lt_impkeys
      EXCEPTIONS
        not_existing = 1
        OTHERS       = 2.
    IF sy-subrc = 0.
      LOOP AT lt_impkeys INTO ls_impkeys.
        INSERT ls_impkeys-clsname INTO TABLE rt_classes.
      ENDLOOP.
    ENDIF.

    IF rt_classes IS INITIAL AND iv_quiet IS INITIAL.
      " There are no tools installed
      MESSAGE s002(/mbtools/bc).
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD _sync_json.

    DATA lo_json TYPE REF TO /mbtools/cl_ajson.

    TRY.
        lo_json = /mbtools/cl_ajson=>create_empty( ).
        lo_json->set( iv_path = '/'
                      iv_val  = is_inst ).

        rs_content-name = is_inst-name.
        rs_content-pack = is_inst-pack.
        rs_content-json = lo_json->stringify( 2 ).
      CATCH /mbtools/cx_ajson_error.
        /mbtools/cx_exception=>raise( 'Error converting JSON persistency' ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
