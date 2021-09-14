CLASS /mbtools/cl_tool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Marc Bernard Tools - Tool Class
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-or-later
************************************************************************
  PUBLIC SECTION.

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
    " Evaluation
    CONSTANTS c_eval_days TYPE i VALUE 60 ##NO_TEXT.
    CONSTANTS c_eval_users TYPE i VALUE 10 ##NO_TEXT.

    CLASS-METHODS class_constructor.
    METHODS constructor
      IMPORTING
        !io_tool TYPE REF TO /mbtools/if_tool.
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
      RETURNING
        VALUE(rs_result) TYPE /mbtools/if_definitions=>ty_license.
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
    METHODS update_version
      IMPORTING
        !iv_force         TYPE abap_bool DEFAULT abap_false
        !iv_version       TYPE string
        !iv_description   TYPE string
        !iv_changelog_url TYPE string
        !iv_changelog     TYPE string
        !iv_download_url  TYPE string
      RETURNING
        VALUE(rv_result)  TYPE abap_bool
      RAISING
        /mbtools/cx_exception.
    METHODS get_manifest
      RETURNING
        VALUE(rs_manifest) TYPE /mbtools/manifest.
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

    CLASS-DATA go_reg_root TYPE REF TO /mbtools/cl_registry.
    DATA ms_manifest TYPE /mbtools/manifest.

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
    METHODS _determine_class
      RETURNING
        VALUE(rv_class) TYPE string.
    METHODS _determine_package
      RETURNING
        VALUE(rv_package) TYPE devclass.
ENDCLASS.



CLASS /mbtools/cl_tool IMPLEMENTATION.


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
        iv_id            = lv_id
        iv_license       = lv_license
      IMPORTING
        ev_version       = lv_version
        ev_description   = lv_description
        ev_changelog_url = lv_changelog_url
        ev_changelog     = lv_changelog
        ev_download_url  = lv_download_url ).

    rv_result = update_version(
      iv_force         = iv_force
      iv_version       = lv_version
      iv_description   = lv_description
      iv_changelog_url = lv_changelog_url
      iv_changelog     = lv_changelog
      iv_download_url  = lv_download_url ).

  ENDMETHOD.


  METHOD class_constructor.

    LOG-POINT ID /mbtools/bc SUBKEY /mbtools/cl_tool_bc=>c_tool-title FIELDS sy-datum sy-uzeit sy-uname.

    TRY.
        " Get root of registry
        go_reg_root = /mbtools/cl_registry=>get_root( ).

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

        rs_result-key    = lo_reg_entry->get_value( c_reg-key_lic_key ).
        rs_result-valid  = lo_reg_entry->get_value( c_reg-key_lic_valid ).
        rs_result-expire = lo_reg_entry->get_value( c_reg-key_lic_expire ).

      CATCH cx_root.
        CLEAR rs_result.
    ENDTRY.

  ENDMETHOD.


  METHOD get_manifest.
    rs_manifest = ms_manifest.
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


  METHOD is_active.
    rv_result = /mbtools/cl_switches=>is_active( ms_manifest-title ).
  ENDMETHOD.


  METHOD is_base.

    " Is this MBT Base?
    IF ms_manifest-title = /mbtools/cl_tool_bc=>c_tool-title.
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

          /mbtools/cl_switches=>init( ms_manifest-title ).
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

        rv_result = abap_true.

      CATCH cx_root.
        rv_result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD update_version.

    DATA:
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry.

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

    " If newer version is available, save info
    IF /mbtools/cl_version=>compare( iv_current = ms_manifest-version
                                     iv_compare = iv_version ) < 0
      OR iv_version = '1.0.0' OR iv_force = abap_true.

      lo_reg_entry = lo_reg_tool->get_subentry( c_reg-update ).
      CHECK lo_reg_entry IS BOUND.

      lo_reg_entry->set_value( iv_key   = c_reg-key_new_version
                               iv_value = iv_version ).
      lo_reg_entry->set_value( iv_key   = c_reg-key_description_html
                               iv_value = iv_description ).
      lo_reg_entry->set_value( iv_key   = c_reg-key_changelog_url
                               iv_value = iv_changelog_url ).
      lo_reg_entry->set_value( iv_key   = c_reg-key_changelog_html
                               iv_value = iv_changelog ).
      lo_reg_entry->set_value( iv_key   = c_reg-key_download_url
                               iv_value = iv_download_url ).

      lo_reg_entry->save( ).

    ENDIF.

    rv_result = abap_true.

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
ENDCLASS.
