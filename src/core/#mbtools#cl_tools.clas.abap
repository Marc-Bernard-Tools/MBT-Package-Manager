CLASS /mbtools/cl_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* MBT Tool Manager
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
  PUBLIC SECTION.

    TYPES ty_manifest TYPE /mbtools/manifest.
    TYPES:
      ty_manifests TYPE STANDARD TABLE OF ty_manifest WITH DEFAULT KEY.
    TYPES ty_tool TYPE /mbtools/tool_with_text.
    TYPES:
      ty_tools TYPE STANDARD TABLE OF ty_tool WITH DEFAULT KEY.

    CONSTANTS:
      " Global Constant
      BEGIN OF c_reg,
        " Registry General (read-only in Registry Browser)
        general              TYPE string VALUE '.General' ##NO_TEXT,
        key_name             TYPE string VALUE 'Name' ##NO_TEXT,
        key_class            TYPE string VALUE 'Class' ##NO_TEXT,
        key_title            TYPE string VALUE 'Title' ##NO_TEXT,
        key_description      TYPE string VALUE 'Description' ##NO_TEXT,
        key_version          TYPE string VALUE 'Version' ##NO_TEXT,
        key_namespace        TYPE string VALUE 'Namespace' ##NO_TEXT,
        key_package          TYPE string VALUE 'Package' ##NO_TEXT,
        " Registry Properties (read-only in Registry Browser)
        properties           TYPE string VALUE '.Properties' ##NO_TEXT,
        key_install_time     TYPE string VALUE 'InstallTimestamp' ##NO_TEXT,
        key_install_user     TYPE string VALUE 'InstallUser' ##NO_TEXT,
        key_update_time      TYPE string VALUE 'UpdateTimestamp' ##NO_TEXT,
        key_update_user      TYPE string VALUE 'UpdateUser' ##NO_TEXT,
        " Registry Switches
        switches             TYPE string VALUE 'Switches' ##NO_TEXT,
        key_active           TYPE string VALUE 'Active' ##NO_TEXT,
        key_debug            TYPE string VALUE 'Debug' ##NO_TEXT,
        key_trace            TYPE string VALUE 'Trace' ##NO_TEXT,
        " Registry License (read-only in Registry Browser)
        license              TYPE string VALUE '.License' ##NO_TEXT,
        key_lic_id           TYPE string VALUE 'ID' ##NO_TEXT,
        key_lic_bundle       TYPE string VALUE 'BundleID' ##NO_TEXT,
        key_lic_key          TYPE string VALUE 'LicenseKey' ##NO_TEXT,
        key_lic_valid        TYPE string VALUE 'LicenseValid' ##NO_TEXT,
        key_lic_expire       TYPE string VALUE 'LicenseExpiration' ##NO_TEXT,
        " Settings
        settings             TYPE string VALUE 'Settings' ##NO_TEXT,
        key_offline          TYPE string VALUE 'IsOffline' ##NO_TEXT,
        key_rfcdest          TYPE string VALUE 'RFCDestination' ##NO_TEXT,
        key_ssl_client       TYPE string VALUE 'SSLClient' ##NO_TEXT,
        " Update
        update               TYPE string VALUE '.Update' ##NO_TEXT,
        key_new_version      TYPE string VALUE 'NewVersion' ##NO_TEXT,
        key_description_html TYPE string VALUE 'DescriptionHTML' ##NO_TEXT,
        key_changelog_url    TYPE string VALUE 'ChangelogURL' ##NO_TEXT,
        key_changelog_html   TYPE string VALUE 'ChangelogHTML' ##NO_TEXT,
        key_download_url     TYPE string VALUE 'DownloadURL' ##NO_TEXT,
      END OF c_reg.
    " Length of MBT Installer package name
    CONSTANTS c_name_length TYPE i VALUE 90 ##NO_TEXT.
    " Evaluation
    CONSTANTS c_eval_days TYPE i VALUE 60 ##NO_TEXT.
    CONSTANTS c_eval_users TYPE i VALUE 10 ##NO_TEXT.

    " Constructor
    CLASS-METHODS class_constructor.
    METHODS constructor
      IMPORTING
        !io_tool TYPE REF TO /mbtools/if_tool.
    CLASS-METHODS init
      IMPORTING
        !iv_title TYPE string OPTIONAL.
    " Class Get
    CLASS-METHODS factory
      IMPORTING
        !iv_title      TYPE csequence DEFAULT /mbtools/cl_tool_bc=>c_tool-title
      RETURNING
        VALUE(ro_tool) TYPE REF TO /mbtools/cl_tools.
    CLASS-METHODS manifests
      RETURNING
        VALUE(rt_manifests) TYPE ty_manifests.
    CLASS-METHODS select
      IMPORTING
        VALUE(iv_pattern)     TYPE csequence OPTIONAL
        VALUE(iv_bundle_id)   TYPE i DEFAULT -1
        VALUE(iv_get_bundles) TYPE abap_bool DEFAULT abap_false
        VALUE(iv_get_tools)   TYPE abap_bool DEFAULT abap_true
        VALUE(iv_admin)       TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_manifests)   TYPE ty_manifests.
    CLASS-METHODS list
      IMPORTING
        VALUE(iv_pattern)     TYPE csequence OPTIONAL
        VALUE(iv_bundle_id)   TYPE i DEFAULT -1
        VALUE(iv_get_bundles) TYPE abap_bool DEFAULT abap_false
        VALUE(iv_get_tools)   TYPE abap_bool DEFAULT abap_true
        VALUE(iv_admin)       TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_tools)       TYPE ty_tools.
    CLASS-METHODS f4
      IMPORTING
        VALUE(iv_pattern)     TYPE csequence OPTIONAL
        VALUE(iv_bundle_id)   TYPE i DEFAULT -1
        VALUE(iv_get_bundles) TYPE abap_bool DEFAULT abap_false
        VALUE(iv_get_tools)   TYPE abap_bool DEFAULT abap_true
        VALUE(iv_admin)       TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_title)       TYPE string.
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
    CLASS-METHODS update
      IMPORTING
        !io_tool         TYPE REF TO /mbtools/cl_tools
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS uninstall
      IMPORTING
        !io_tool         TYPE REF TO /mbtools/cl_tools
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS sync
      IMPORTING
        !io_tool         TYPE REF TO /mbtools/cl_tools
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS is_base_only
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    " Tool Register/Unregister
    METHODS register
      IMPORTING
        !iv_update       TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    METHODS unregister
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    " Tool Activate/Deactivate
    METHODS activate
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        /mbtools/cx_exception.
    METHODS deactivate
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        /mbtools/cx_exception.
    METHODS is_active
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    METHODS is_debug
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    METHODS is_trace
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    METHODS is_base
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    METHODS is_bundle
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    METHODS has_launch
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    METHODS launch.
    METHODS get_license
      IMPORTING
        !iv_param        TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.
    " Tool License
    METHODS is_licensed
      IMPORTING
        !iv_check_eval   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    METHODS license_add
      IMPORTING
        !iv_license      TYPE string
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        /mbtools/cx_exception.
    METHODS license_remove
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        /mbtools/cx_exception.
    METHODS check_version
      IMPORTING
        !iv_force        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        /mbtools/cx_exception.
    " Tool Get
    METHODS get_id
      RETURNING
        VALUE(rv_id) TYPE string.
    METHODS get_slug
      RETURNING
        VALUE(rv_slug) TYPE string.
    METHODS get_name
      RETURNING
        VALUE(rv_name) TYPE string.
    METHODS get_title
      RETURNING
        VALUE(rv_title) TYPE string.
    METHODS get_version
      RETURNING
        VALUE(rv_version) TYPE string.
    METHODS get_bundle_id
      RETURNING
        VALUE(rv_result) TYPE i.
    METHODS get_download_id
      RETURNING
        VALUE(rv_result) TYPE i.
    METHODS get_html_changelog
      RETURNING
        VALUE(rv_result) TYPE string.
    METHODS get_description
      RETURNING
        VALUE(rv_description) TYPE string.
    METHODS get_html_description
      RETURNING
        VALUE(rv_result) TYPE string.
    METHODS get_class
      RETURNING
        VALUE(rv_class) TYPE string.
    METHODS get_package
      RETURNING
        VALUE(rv_package) TYPE devclass.
    METHODS get_url_repo
      RETURNING
        VALUE(rv_url) TYPE string.
    METHODS get_url_tool
      RETURNING
        VALUE(rv_url) TYPE string.
    METHODS get_url_docs
      RETURNING
        VALUE(rv_url) TYPE string.
    METHODS get_url_download
      RETURNING
        VALUE(rv_result) TYPE string.
    METHODS get_url_changelog
      RETURNING
        VALUE(rv_result) TYPE string.
    METHODS get_settings
      RETURNING
        VALUE(ro_reg) TYPE REF TO /mbtools/cl_registry.
    METHODS get_new_version
      RETURNING
        VALUE(rv_result) TYPE string.
    METHODS get_thumbnail
      RETURNING
        VALUE(rv_thumbnail) TYPE string.
    METHODS get_install_time
      IMPORTING
        !iv_internal     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_result) TYPE string.
    METHODS get_last_update
      IMPORTING
        !iv_internal     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_result) TYPE string.
    METHODS get_command
      RETURNING
        VALUE(rv_result) TYPE string.
    METHODS get_shortcut
      RETURNING
        VALUE(rv_result) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        key      TYPE string,
        instance TYPE REF TO /mbtools/cl_tools,
      END OF ty_instance.

    " Sync with zif_abapinst_definitions
    TYPES:
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
    CLASS-DATA go_reg_root TYPE REF TO /mbtools/cl_registry.

    DATA ms_manifest TYPE ty_manifest.

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
    CLASS-METHODS _get_reg_bundle
      IMPORTING
        !iv_bundle_id    TYPE i
      RETURNING
        VALUE(ro_result) TYPE REF TO /mbtools/cl_registry.
    CLASS-METHODS _get_reg_tool
      IMPORTING
        !iv_name         TYPE string
      RETURNING
        VALUE(ro_result) TYPE REF TO /mbtools/cl_registry.
    CLASS-METHODS _sync_json
      IMPORTING
        !is_inst          TYPE ty_inst
      RETURNING
        VALUE(rs_content) TYPE ty_content
      RAISING
        /mbtools/cx_exception.
    METHODS _determine_class
      RETURNING
        VALUE(rv_class) TYPE string.
    METHODS _determine_package
      RETURNING
        VALUE(rv_package) TYPE devclass.
ENDCLASS.



CLASS /mbtools/cl_tools IMPLEMENTATION.


  METHOD action_bundles.

    DATA:
      ls_manifest  TYPE ty_manifest,
      lt_manifests TYPE ty_manifests,
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
      ls_manifest  TYPE ty_manifest,
      lt_manifests TYPE ty_manifests,
      li_progress  TYPE REF TO /mbtools/if_progress,
      lv_result    TYPE abap_bool.

    " Just tools (no bundles)
    lt_manifests = select( iv_admin = abap_true ).

    rv_result = abap_true.

    li_progress = /mbtools/cl_progress=>get_instance( lines( lt_manifests ) ).

    LOOP AT lt_manifests INTO ls_manifest.

      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |Run action for { ls_manifest-name }| ).

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
        CATCH /mbtools/cx_exception ##NO_HANDLER.
      ENDTRY.

      IF lv_result = abap_false.
        rv_result = abap_false.
      ENDIF.

    ENDLOOP.

    li_progress->hide( ).

  ENDMETHOD.


  METHOD activate.

    DATA:
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry.

    CHECK ms_manifest-is_bundle IS INITIAL.

    " Is tool already registered?
    lo_reg_tool = _get_reg_tool( ms_manifest-name ).
    IF lo_reg_tool IS NOT BOUND.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    " Switches
    lo_reg_entry = lo_reg_tool->get_subentry( c_reg-switches ).
    IF lo_reg_entry IS BOUND.
      lo_reg_entry->set_value( iv_key   = c_reg-key_active
                               iv_value = abap_true ).
      lo_reg_entry->save( ).
    ENDIF.

    /mbtools/cl_switches=>init( ms_manifest-title ).

    rv_result = abap_true.

  ENDMETHOD.


  METHOD check_version.

    DATA:
      lo_reg_tool      TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry     TYPE REF TO /mbtools/cl_registry,
      lv_license       TYPE string,
      lv_id            TYPE string,
      lv_version       TYPE string,
      lv_description   TYPE string,
      lv_changelog_url TYPE string,
      lv_changelog     TYPE string,
      lv_download_url  TYPE string.

    " Is tool or bundle registered?
    IF is_bundle( ) IS INITIAL.
      lo_reg_tool = _get_reg_tool( ms_manifest-name ).
    ELSE.
      lo_reg_tool = go_reg_root->get_subentry( ms_manifest-name ).
    ENDIF.
    CHECK lo_reg_tool IS BOUND.

    " Get license
    lo_reg_entry = lo_reg_tool->get_subentry( c_reg-license ).
    CHECK lo_reg_entry IS BOUND.

    lv_id = lo_reg_entry->get_value( c_reg-key_lic_id ).

    lv_license = lo_reg_entry->get_value( c_reg-key_lic_key ).

    " Get version info via call to EDD API on MBT
    /mbtools/cl_edd=>get_version(
      EXPORTING
        iv_id             = lv_id
        iv_license        = lv_license
      IMPORTING
        ev_version        = lv_version
        ev_description    = lv_description
        ev_changelog_url  = lv_changelog_url
        ev_changelog      = lv_changelog
        ev_download_url   = lv_download_url ).

    " If newer version is available, save info
    IF /mbtools/cl_version=>compare( iv_current = ms_manifest-version
                                     iv_compare = lv_version ) < 0
      OR lv_version = '1.0.0' OR iv_force = abap_true.

      lo_reg_entry = lo_reg_tool->get_subentry( c_reg-update ).
      CHECK lo_reg_entry IS BOUND.

      lo_reg_entry->set_value( iv_key   = c_reg-key_new_version
                               iv_value = lv_version ).
      lo_reg_entry->set_value( iv_key   = c_reg-key_description_html
                               iv_value = lv_description ).
      lo_reg_entry->set_value( iv_key   = c_reg-key_changelog_url
                               iv_value = lv_changelog_url ).
      lo_reg_entry->set_value( iv_key   = c_reg-key_changelog_html
                               iv_value = lv_changelog ).
      lo_reg_entry->set_value( iv_key   = c_reg-key_download_url
                               iv_value = lv_download_url ).

      lo_reg_entry->save( ).

    ENDIF.

    rv_result = abap_true.

  ENDMETHOD.


  METHOD class_constructor.

    LOG-POINT ID /mbtools/bc SUBKEY /mbtools/cl_tool_bc=>c_tool-title FIELDS sy-datum sy-uzeit sy-uname.

    TRY.
        " Get root of registry
        go_reg_root = /mbtools/cl_registry=>get_root( ).

        " Initialize tool instances
        init( ).

      CATCH /mbtools/cx_exception.
        " MBT Base is not installed properly. Contact Marc Bernard Tools
        ASSERT 0 = 1.
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    MOVE-CORRESPONDING io_tool->tool( ) TO ms_manifest.

    ms_manifest-manager   = me.
    ms_manifest-tool      = io_tool.
    ms_manifest-namespace = /mbtools/if_definitions=>c_namespace.
    ms_manifest-id        = get_id( ).
    ms_manifest-name      = get_name( ).
    ms_manifest-slug      = get_slug( ).
    ms_manifest-git_url   = get_url_repo( ).
    ms_manifest-class     = _determine_class( ).
    ms_manifest-package   = _determine_package( ).

  ENDMETHOD.


  METHOD deactivate.

    DATA:
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry.

    CHECK ms_manifest-is_bundle IS INITIAL.

    " Is tool already registered?
    lo_reg_tool = _get_reg_tool( ms_manifest-name ).
    IF lo_reg_tool IS NOT BOUND.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    " Switches
    lo_reg_entry = lo_reg_tool->get_subentry( c_reg-switches ).
    IF lo_reg_entry IS BOUND.
      lo_reg_entry->set_value( iv_key   = c_reg-key_active
                               iv_value = abap_false ).
      lo_reg_entry->save( ).
    ENDIF.

    /mbtools/cl_switches=>init( ms_manifest-title ).

    rv_result = abap_true.

  ENDMETHOD.


  METHOD f4.

    DATA:
      lt_tools  TYPE ty_tools,
      ls_return TYPE ddshretval,
      lt_return TYPE TABLE OF ddshretval.

    lt_tools = list(
      iv_pattern     = iv_pattern
      iv_bundle_id   = iv_bundle_id
      iv_get_bundles = iv_get_bundles
      iv_get_tools   = iv_get_tools
      iv_admin       = iv_admin ).

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


  METHOD get_bundle_id.

    rv_result = ms_manifest-bundle_id.

  ENDMETHOD.


  METHOD get_class.
    rv_class = ms_manifest-class.
  ENDMETHOD.


  METHOD get_command.
    rv_result = ms_manifest-mbt_command.
  ENDMETHOD.


  METHOD get_description.
    rv_description = ms_manifest-description.
  ENDMETHOD.


  METHOD get_download_id.
    rv_result = ms_manifest-download_id.
  ENDMETHOD.


  METHOD get_html_changelog.

    DATA:
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry.

    TRY.
        " Is tool installed?
        lo_reg_tool = _get_reg_tool( ms_manifest-name ).
        CHECK lo_reg_tool IS BOUND.

        " Update
        lo_reg_entry = lo_reg_tool->get_subentry( c_reg-update ).
        CHECK lo_reg_entry IS BOUND.

        rv_result = lo_reg_entry->get_value( c_reg-key_changelog_html ).

      CATCH cx_root.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD get_html_description.

    DATA:
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry.

    TRY.
        " Is tool installed?
        lo_reg_tool = _get_reg_tool( ms_manifest-name ).
        CHECK lo_reg_tool IS BOUND.

        " Update
        lo_reg_entry = lo_reg_tool->get_subentry( c_reg-update ).
        CHECK lo_reg_entry IS BOUND.

        rv_result = lo_reg_entry->get_value( c_reg-key_description_html ).

      CATCH cx_root.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD get_id.

    " Upper case, Underscore, Namespaced
    rv_id = to_upper( /mbtools/if_definitions=>c_namespace && ms_manifest-title ).

    REPLACE ALL OCCURRENCES OF ` ` IN rv_id WITH '_'.

  ENDMETHOD.


  METHOD get_install_time.

    DATA:
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry,
      lv_install   TYPE timestamp.

    CHECK ms_manifest-is_bundle IS INITIAL.

    TRY.
        " Is tool already registered?
        lo_reg_tool = _get_reg_tool( ms_manifest-name ).
        IF lo_reg_tool IS NOT BOUND.
          RETURN.
        ENDIF.

        " Properties
        lo_reg_entry = lo_reg_tool->get_subentry( c_reg-properties ).
        IF lo_reg_entry IS BOUND.
          lv_install = lo_reg_entry->get_value( c_reg-key_install_time ).
          IF iv_internal = abap_true.
            rv_result = lv_install.
          ELSE.
            rv_result = /mbtools/cl_datetime=>human_time_diff( lv_install ) && ' ago'.
          ENDIF.
        ELSEIF iv_internal = abap_false.
          rv_result = 'never'.
        ENDIF.

      CATCH cx_root.
        IF iv_internal = abap_false.
          rv_result = 'n/a'.
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD get_last_update.

    DATA:
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry,
      lv_update    TYPE timestamp.

    CHECK ms_manifest-is_bundle IS INITIAL.

    TRY.
        " Is tool already registered?
        lo_reg_tool = _get_reg_tool( ms_manifest-name ).
        IF lo_reg_tool IS NOT BOUND.
          RETURN.
        ENDIF.

        " Properties
        lo_reg_entry = lo_reg_tool->get_subentry( c_reg-properties ).
        IF lo_reg_entry IS BOUND.
          lv_update = lo_reg_entry->get_value( c_reg-key_update_time ).
          IF iv_internal = abap_true.
            rv_result = lv_update.
          ELSEIF lv_update IS INITIAL.
            rv_result = 'never'.
          ELSE.
            rv_result = /mbtools/cl_datetime=>human_time_diff( lv_update ) && ' ago'.
          ENDIF.
        ELSEIF iv_internal = abap_false.
          rv_result = 'never'.
        ENDIF.

      CATCH cx_root.
        IF iv_internal = abap_false.
          rv_result = 'n/a'.
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD get_license.

    DATA:
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry.

    TRY.
        " Is tool already registered?
        IF is_bundle( ) IS INITIAL.
          lo_reg_tool = _get_reg_tool( ms_manifest-name ).
        ELSE.
          lo_reg_tool = go_reg_root->get_subentry( ms_manifest-name ).
        ENDIF.
        CHECK lo_reg_tool IS BOUND.

        " License
        lo_reg_entry = lo_reg_tool->get_subentry( c_reg-license ).
        CHECK lo_reg_entry IS BOUND.

        rv_result = lo_reg_entry->get_value( iv_param ).

      CATCH cx_root.
        rv_result = ''.
    ENDTRY.

  ENDMETHOD.


  METHOD get_name.

    " Note: This name determines the sort order

    " Mixed case, underscore
    rv_name = ms_manifest-title.

    REPLACE ALL OCCURRENCES OF ` ` IN rv_name WITH '_'.

  ENDMETHOD.


  METHOD get_new_version.

    DATA:
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry.

    CHECK ms_manifest-is_bundle IS INITIAL.

    TRY.
        " Is tool installed?
        lo_reg_tool = _get_reg_tool( ms_manifest-name ).
        CHECK lo_reg_tool IS BOUND.

        " Update
        lo_reg_entry = lo_reg_tool->get_subentry( c_reg-update ).
        CHECK lo_reg_entry IS BOUND.

        rv_result = lo_reg_entry->get_value( c_reg-key_new_version ).

        " If current version is same or newer, then reset registry value
        IF rv_result IS NOT INITIAL AND
          /mbtools/cl_version=>compare( iv_current = ms_manifest-version
                                        iv_compare = rv_result ) >= 0.

          CLEAR rv_result.
          lo_reg_entry->set_value( iv_key   = c_reg-key_new_version
                                   iv_value = rv_result ).

          lo_reg_entry->save( ).
        ENDIF.

      CATCH cx_root.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD get_package.
    rv_package = ms_manifest-package.
  ENDMETHOD.


  METHOD get_settings.

    DATA lo_reg_tool TYPE REF TO /mbtools/cl_registry.

    CHECK ms_manifest-is_bundle IS INITIAL.

    TRY.
        " Is tool installed?
        lo_reg_tool = _get_reg_tool( ms_manifest-name ).
        CHECK lo_reg_tool IS BOUND.

        " Settings
        ro_reg = lo_reg_tool->get_subentry( c_reg-settings ).

      CATCH cx_root.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD get_shortcut.
    rv_result = ms_manifest-mbt_shortcut.
  ENDMETHOD.


  METHOD get_slug.

    " Lower case, dash
    rv_slug = to_lower( ms_manifest-title ).

    REPLACE ALL OCCURRENCES OF ` ` IN rv_slug WITH '-'.

  ENDMETHOD.


  METHOD get_thumbnail.
    rv_thumbnail = ms_manifest-id && '_TN'.
  ENDMETHOD.


  METHOD get_title.
    rv_title = ms_manifest-title.
  ENDMETHOD.


  METHOD get_url_changelog.

    DATA:
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry.

    CHECK ms_manifest-is_bundle IS INITIAL.

    TRY.
        " Is tool installed?
        lo_reg_tool = _get_reg_tool( ms_manifest-name ).
        CHECK lo_reg_tool IS BOUND.

        " Update
        lo_reg_entry = lo_reg_tool->get_subentry( c_reg-update ).
        CHECK lo_reg_entry IS BOUND.

        rv_result = lo_reg_entry->get_value( c_reg-key_changelog_url ).

      CATCH cx_root.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD get_url_docs.

    " Link to documentation page on marcbernardtools.com
    rv_url = /mbtools/if_definitions=>c_www_home && /mbtools/if_definitions=>c_www_tool_docs &&
             ms_manifest-slug && '/'.

  ENDMETHOD.


  METHOD get_url_download.

    DATA:
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry.

    CHECK ms_manifest-is_bundle IS INITIAL.

    TRY.
        " Is tool installed?
        lo_reg_tool = _get_reg_tool( ms_manifest-name ).
        CHECK lo_reg_tool IS BOUND.

        " Update
        lo_reg_entry = lo_reg_tool->get_subentry( c_reg-update ).
        CHECK lo_reg_entry IS BOUND.

        rv_result = lo_reg_entry->get_value( c_reg-key_download_url ).

      CATCH cx_root.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD get_url_repo.

    " Link to repository on GitHub.com
    rv_url = 'https://' && /mbtools/if_definitions=>c_github && '/' && ms_manifest-name.

    REPLACE ALL OCCURRENCES OF `_` IN rv_url WITH '-'.

  ENDMETHOD.


  METHOD get_url_tool.

    " Link to tool page on marcbernardtools.com
    rv_url = /mbtools/if_definitions=>c_www_home && /mbtools/if_definitions=>c_www_tool_download &&
             ms_manifest-slug && '/'.

  ENDMETHOD.


  METHOD get_version.
    rv_version = ms_manifest-version.
  ENDMETHOD.


  METHOD has_launch.
    rv_result = ms_manifest-has_launch.
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


  METHOD is_active.
    rv_result = /mbtools/cl_switches=>is_active( ms_manifest-title ).
  ENDMETHOD.


  METHOD is_base.

    " Is this MBT Base?
    IF ms_manifest-title = /mbtools/cl_tool_bc=>c_tool-title.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD is_base_only.

    DATA lt_manifests TYPE ty_manifests.

    " Get all installed and active tools
    lt_manifests = select( ).

    IF lt_manifests IS INITIAL.
      " This means there's only MBT Base left as the last tool
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD is_bundle.
    rv_result = ms_manifest-is_bundle.
  ENDMETHOD.


  METHOD is_debug.
    rv_result = /mbtools/cl_switches=>is_debug( ms_manifest-title ).
  ENDMETHOD.


  METHOD is_licensed.

    DATA:
      lo_reg_tool   TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry  TYPE REF TO /mbtools/cl_registry,
      lv_date_from  TYPE d,
      lv_user_count TYPE i,
      lv_value      TYPE string,
      lv_expire     TYPE string.

    IF iv_check_eval = abap_true.
      " No license required for systems with few users
      " (dialog users who have logged on during evaluation period)
      lv_date_from = sy-datum - c_eval_days.

      SELECT COUNT(*) FROM usr02 INTO lv_user_count
        WHERE ustyp = 'A' AND trdat BETWEEN lv_date_from AND sy-datum
          AND ( bname <> 'DDIC' AND bname NOT LIKE '%SUPPORT%' ). "#EC CI_BYPASS "#EC CI_GENBUFF
      IF sy-subrc = 0 AND lv_user_count <= c_eval_users.
        rv_result = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    TRY.
        " Is tool already registered?
        IF is_bundle( ) IS INITIAL.
          lo_reg_tool = _get_reg_tool( ms_manifest-name ).
        ELSE.
          lo_reg_tool = go_reg_root->get_subentry( ms_manifest-name ).
        ENDIF.
        CHECK lo_reg_tool IS BOUND.

        " Properties
        lo_reg_entry = lo_reg_tool->get_subentry( c_reg-properties ).
        CHECK lo_reg_entry IS BOUND.

        IF iv_check_eval = abap_true.
          " No license required during evaluation period
          lv_value = lo_reg_entry->get_value( c_reg-key_install_time ).
          lv_date_from = lv_value(8) + c_eval_days.
          IF lv_date_from >= sy-datum.
            rv_result = abap_true.
            RETURN.
          ENDIF.
        ENDIF.

        " License
        lo_reg_entry = lo_reg_tool->get_subentry( c_reg-license ).
        CHECK lo_reg_entry IS BOUND.

        " Is license valid?
        lv_value = lo_reg_entry->get_value( c_reg-key_lic_valid ).
        lv_expire = lo_reg_entry->get_value( c_reg-key_lic_expire ).
        IF lv_value = abap_true AND lv_expire >= sy-datum.
          rv_result = abap_true.
        ENDIF.

      CATCH cx_root.
        rv_result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD is_trace.
    rv_result = /mbtools/cl_switches=>is_trace( ms_manifest-title ).
  ENDMETHOD.


  METHOD launch.

    IF has_launch( ) = abap_true.

      TRY.
          ms_manifest-tool->launch( ).
        CATCH /mbtools/cx_exception ##NO_HANDLER.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD license_add.

    DATA:
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry,
      lv_license   TYPE string,
      lv_id        TYPE string,
      lv_valid     TYPE abap_bool,
      lv_expire    TYPE d.

    " Is tool or bundle registered?
    IF is_bundle( ) IS INITIAL.
      lo_reg_tool = _get_reg_tool( ms_manifest-name ).
    ELSE.
      lo_reg_tool = go_reg_root->get_subentry( ms_manifest-name ).
    ENDIF.
    CHECK lo_reg_tool IS BOUND.

    " License
    lo_reg_entry = lo_reg_tool->get_subentry( c_reg-license ).
    CHECK lo_reg_entry IS BOUND.

    lv_id = lo_reg_entry->get_value( c_reg-key_lic_id ).

    lv_license = escape( val    = to_lower( iv_license )
                         format = cl_abap_format=>e_url_full ).

    " Activate license via call to EDD API on MBT
    /mbtools/cl_edd=>activate_license(
      EXPORTING
        iv_id      = lv_id
        iv_license = lv_license
      IMPORTING
        ev_valid   = lv_valid
        ev_expire  = lv_expire ).

    lo_reg_entry->set_value( iv_key   = c_reg-key_lic_key
                             iv_value = lv_license ).
    lo_reg_entry->set_value( iv_key   = c_reg-key_lic_valid
                             iv_value = lv_valid ).
    lo_reg_entry->set_value( iv_key   = c_reg-key_lic_expire
                             iv_value = lv_expire ).

    lo_reg_entry->save( ).

    IF lv_valid = abap_true AND lv_expire >= sy-datum.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD license_remove.

    DATA:
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry,
      lv_license   TYPE string,
      lv_id        TYPE string.

    " Is tool or bundle registered?
    IF is_bundle( ) IS INITIAL.
      lo_reg_tool = _get_reg_tool( ms_manifest-name ).
    ELSE.
      lo_reg_tool = go_reg_root->get_subentry( ms_manifest-name ).
    ENDIF.
    CHECK lo_reg_tool IS BOUND.

    " Get license
    lo_reg_entry = lo_reg_tool->get_subentry( c_reg-license ).
    CHECK lo_reg_entry IS BOUND.

    lv_id = lo_reg_entry->get_value( c_reg-key_lic_id ).

    lv_license = lo_reg_entry->get_value( c_reg-key_lic_key ).

    " Deactivate license via call to EDD API on MBT
    rv_result = /mbtools/cl_edd=>deactivate_license(
      iv_id      = lv_id
      iv_license = lv_license ).

    " Remove license key (back to defaults, same as REGISTER)
    lo_reg_entry->set_value( c_reg-key_lic_key ).
    lo_reg_entry->set_value( c_reg-key_lic_valid ).
    lo_reg_entry->set_value( iv_key   = c_reg-key_lic_expire
                             iv_value = '99991231' ).

    lo_reg_entry->save( ).

  ENDMETHOD.


  METHOD list.

    DATA:
      ls_manifest  TYPE ty_manifest,
      lt_manifests TYPE ty_manifests,
      ls_tool      LIKE LINE OF rt_tools.

    lt_manifests = select(
      iv_pattern     = iv_pattern
      iv_bundle_id   = iv_bundle_id
      iv_get_bundles = iv_get_bundles
      iv_get_tools   = iv_get_tools
      iv_admin       = iv_admin ).

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

    DATA ls_instance LIKE LINE OF gt_instances.

    LOOP AT gt_instances INTO ls_instance.
      INSERT ls_instance-instance->ms_manifest INTO TABLE rt_manifests.
    ENDLOOP.

    SORT rt_manifests BY name.

  ENDMETHOD.


  METHOD register.

    DATA:
      lo_reg_tool   TYPE REF TO /mbtools/cl_registry,
      lo_reg_bundle TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry  TYPE REF TO /mbtools/cl_registry,
      lv_timestamp  TYPE timestamp.

    TRY.
        " Is tool already registered?
        IF is_bundle( ) IS INITIAL.
          lo_reg_tool = _get_reg_tool( ms_manifest-name ).
        ELSE.
          lo_reg_tool = go_reg_root->get_subentry( ms_manifest-name ).
        ENDIF.
        IF lo_reg_tool IS BOUND.
          rv_result = abap_true.
          IF iv_update = abap_false.
            RETURN.
          ENDIF.
        ELSE.
          " Create registry entries
          IF is_bundle( ) IS INITIAL.
            lo_reg_bundle = _get_reg_bundle( ms_manifest-bundle_id ).
            IF lo_reg_bundle IS BOUND.
              lo_reg_tool = lo_reg_bundle->add_subentry( ms_manifest-name ).
            ELSE.
              " Bundle must be installed before tool
              RETURN. ">>>
            ENDIF.
          ELSE.
            lo_reg_tool = go_reg_root->add_subentry( ms_manifest-name ).
          ENDIF.
          CHECK lo_reg_tool IS BOUND.
        ENDIF.

        " General
        IF rv_result = abap_true.
          lo_reg_entry = lo_reg_tool->get_subentry( c_reg-general ).
        ELSE.
          lo_reg_entry = lo_reg_tool->add_subentry( c_reg-general ).
        ENDIF.
        IF lo_reg_entry IS BOUND.
          lo_reg_entry->set_value( iv_key   = c_reg-key_name
                                   iv_value = ms_manifest-name ).
          lo_reg_entry->set_value( iv_key   = c_reg-key_version
                                   iv_value = ms_manifest-version ).
          lo_reg_entry->set_value( iv_key   = c_reg-key_title
                                   iv_value = ms_manifest-title ).
          lo_reg_entry->set_value( iv_key   = c_reg-key_description
                                   iv_value = ms_manifest-description ).
          IF ms_manifest-is_bundle IS INITIAL.
            lo_reg_entry->set_value( iv_key   = c_reg-key_namespace
                                     iv_value = /mbtools/if_definitions=>c_namespace ).
            lo_reg_entry->set_value( iv_key   = c_reg-key_package
                                     iv_value = ms_manifest-package ).
            lo_reg_entry->set_value( iv_key   = c_reg-key_class
                                     iv_value = ms_manifest-class ).
          ENDIF.
          lo_reg_entry->save( ).
        ENDIF.

        " Properties
        IF rv_result = abap_true.
          lo_reg_entry = lo_reg_tool->get_subentry( c_reg-properties ).
        ELSE.
          lo_reg_entry = lo_reg_tool->add_subentry( c_reg-properties ).
        ENDIF.
        IF lo_reg_entry IS BOUND.
          GET TIME STAMP FIELD lv_timestamp.
          IF rv_result = abap_true.
            IF iv_update = abap_true.
              lo_reg_entry->set_value( iv_key = c_reg-key_update_time
                                       iv_value = lv_timestamp ).
              lo_reg_entry->set_value( iv_key = c_reg-key_update_user
                                       iv_value = sy-uname ).
            ENDIF.
          ELSE.
            lo_reg_entry->set_value( iv_key   = c_reg-key_install_time
                                     iv_value = lv_timestamp ).
            lo_reg_entry->set_value( iv_key   = c_reg-key_install_user
                                     iv_value = sy-uname ).
            lo_reg_entry->set_value( c_reg-key_update_time ).
            lo_reg_entry->set_value( c_reg-key_update_user ).
          ENDIF.
          lo_reg_entry->save( ).
        ENDIF.

        " Initialize on first run
        IF rv_result = abap_false.
          " Switches
          IF ms_manifest-is_bundle IS INITIAL.
            lo_reg_entry = lo_reg_tool->add_subentry( c_reg-switches ).
            IF lo_reg_entry IS BOUND.
              lo_reg_entry->set_value( c_reg-key_active ).
              lo_reg_entry->set_value( c_reg-key_debug ).
              lo_reg_entry->set_value( c_reg-key_trace ).
              lo_reg_entry->save( ).
            ENDIF.
          ENDIF.

          " License
          lo_reg_entry = lo_reg_tool->add_subentry( c_reg-license ).
          IF lo_reg_entry IS BOUND.
            lo_reg_entry->set_value( iv_key   = c_reg-key_lic_id
                                     iv_value = ms_manifest-download_id ).
            lo_reg_entry->set_value( iv_key   = c_reg-key_lic_bundle
                                     iv_value = ms_manifest-bundle_id ).
            lo_reg_entry->set_value( iv_key   = c_reg-key_lic_expire
                                     iv_value = '99991231' ).
            lo_reg_entry->set_value( c_reg-key_lic_key ).
            lo_reg_entry->set_value( c_reg-key_lic_valid ).
            lo_reg_entry->save( ).
          ENDIF.

          " Settings
          IF ms_manifest-is_bundle IS INITIAL.
            lo_reg_entry = lo_reg_tool->add_subentry( c_reg-settings ).
          ENDIF.

          " Update
          lo_reg_entry = lo_reg_tool->add_subentry( c_reg-update ).
          IF lo_reg_entry IS BOUND.
            lo_reg_entry->set_value( c_reg-key_new_version ).
            lo_reg_entry->set_value( c_reg-key_description_html ).
            lo_reg_entry->set_value( c_reg-key_changelog_html ).
            lo_reg_entry->set_value( c_reg-key_changelog_url ).
            lo_reg_entry->set_value( c_reg-key_download_url ).
            lo_reg_entry->save( ).
          ENDIF.
        ENDIF.

        " Save
        lo_reg_tool->save( ).

        " Setup Install
        TRY.
            ms_manifest-tool->install( ).
          CATCH /mbtools/cx_exception ##NO_HANDLER.
        ENDTRY.

        rv_result = abap_true.

      CATCH cx_root.
        rv_result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD select.

    DATA:
      ls_instance LIKE LINE OF gt_instances,
      lo_tool     TYPE REF TO /mbtools/cl_tools.

    LOOP AT gt_instances INTO ls_instance.

      lo_tool = ls_instance-instance.

      " Filter by bundle
      IF iv_bundle_id >= 0 AND lo_tool->get_bundle_id( ) <> iv_bundle_id.
        CONTINUE.
      ENDIF.

      " Filter by bundle/tool type
      IF lo_tool->is_bundle( ) = abap_true.
        IF iv_get_bundles = abap_false.
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

      INSERT lo_tool->ms_manifest INTO TABLE rt_manifests.

    ENDLOOP.

    SORT rt_manifests BY name AS TEXT.

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
          io_tool->unregister( ).

          rv_result = abap_true.
        ENDIF.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD unregister.

    DATA:
      lo_reg_bundle TYPE REF TO /mbtools/cl_registry,
      lo_reg_tool   TYPE REF TO /mbtools/cl_registry,
      lt_entries    TYPE /mbtools/cl_registry=>ty_keyobjs.

    TRY.
        " Is tool still registered?
        IF is_bundle( ) IS INITIAL.
          lo_reg_tool = _get_reg_tool( ms_manifest-name ).
        ELSE.
          lo_reg_tool = go_reg_root->get_subentry( ms_manifest-name ).
        ENDIF.
        IF lo_reg_tool IS NOT BOUND.
          rv_result = abap_true.
          RETURN.
        ENDIF.

        " Get bundle
        lo_reg_bundle = _get_reg_bundle( ms_manifest-bundle_id ).
        IF lo_reg_bundle IS NOT BOUND.
          RETURN. ">>>
        ENDIF.

        " Remove registry branch
        IF is_bundle( ) IS INITIAL.
          lo_reg_bundle->remove_subentry( ms_manifest-name ).
        ELSE.
          " Check if any tools are still registered
          lt_entries = lo_reg_bundle->get_subentries( ).
          LOOP AT lt_entries TRANSPORTING NO FIELDS WHERE key CP 'MBT*'. "#EC CI_SORTSEQ
            EXIT.
          ENDLOOP.
          IF sy-subrc <> 0.
            go_reg_root->remove_subentry( ms_manifest-name ).
          ELSE.
            RETURN. ">>>
          ENDIF.
        ENDIF.

        " Setup Uninstall
        TRY.
            ms_manifest-tool->uninstall( ).
          CATCH /mbtools/cx_exception ##NO_HANDLER.
        ENDTRY.

        init( ).

        rv_result = abap_true.

      CATCH cx_root.
        rv_result = abap_false.
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
    io_tool->register( ).

    init( ).

    rv_result = abap_true.

  ENDMETHOD.


  METHOD _clean_title.

    " Input could be title or name of tool
    rv_result = iv_title.
    IF iv_title CA '_'.
      REPLACE ALL OCCURRENCES OF '_' IN rv_result WITH ` `.
    ENDIF.

  ENDMETHOD.


  METHOD _determine_class.

    DATA lo_class_desc TYPE REF TO cl_abap_typedescr.

    lo_class_desc = cl_abap_classdescr=>describe_by_object_ref( ms_manifest-tool ).
    rv_class = lo_class_desc->get_relative_name( ).

  ENDMETHOD.


  METHOD _determine_package.

    DATA lv_class TYPE string.

    lv_class = get_class( ).

    SELECT SINGLE devclass FROM tadir INTO rv_package
      WHERE pgmid = 'R3TR' AND object = 'CLAS' AND obj_name = lv_class.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD _get_implementations.

    " Get all classes that implement the MBT Interface
    SELECT clsname FROM seometarel INTO TABLE rt_classes
      WHERE version = '1' AND refclsname = /mbtools/if_definitions=>c_interface. "#EC CI_GENBUFF
    IF sy-subrc = 0.
      " Add sub-classes
      SELECT clsname FROM seometarel APPENDING TABLE rt_classes
        FOR ALL ENTRIES IN rt_classes
        WHERE version = '1' AND refclsname = rt_classes-table_line. "#EC CI_GENBUFF
      ASSERT sy-subrc >= 0.
    ENDIF.

    IF rt_classes IS INITIAL AND iv_quiet IS INITIAL.
      " There are no tools installed
      MESSAGE s002(/mbtools/bc).
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD _get_reg_bundle.

    DATA:
      lv_bundle_id TYPE i,
      ls_bundle    TYPE /mbtools/cl_registry=>ty_keyobj,
      lt_bundles   TYPE /mbtools/cl_registry=>ty_keyobjs.

    TRY.
        lt_bundles = go_reg_root->get_subentries( ).

        LOOP AT lt_bundles INTO ls_bundle.
          " Is bundle installed?
          lv_bundle_id = ls_bundle-value->get_subentry( c_reg-license )->get_value( c_reg-key_lic_bundle ).
          IF lv_bundle_id = iv_bundle_id.
            ro_result = ls_bundle-value.
            EXIT.
          ENDIF.
        ENDLOOP.

      CATCH cx_root.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD _get_reg_tool.

    DATA:
      ls_bundle  TYPE /mbtools/cl_registry=>ty_keyobj,
      lt_bundles TYPE /mbtools/cl_registry=>ty_keyobjs.

    TRY.
        lt_bundles = go_reg_root->get_subentries( ).

        LOOP AT lt_bundles INTO ls_bundle.
          " Is tool installed?
          ro_result = ls_bundle-value->get_subentry( iv_name ).
          IF ro_result IS BOUND.
            EXIT.
          ENDIF.
        ENDLOOP.

      CATCH cx_root.
        RETURN.
    ENDTRY.

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
