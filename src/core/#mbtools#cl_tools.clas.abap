CLASS /mbtools/cl_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* MBT Tool Manager
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
  PUBLIC SECTION.

    " Global Constant
    CONSTANTS:
      BEGIN OF c_reg,
        " Registry General
        general            TYPE string VALUE '.General' ##NO_TEXT,
        key_name           TYPE string VALUE 'Name' ##NO_TEXT,
        key_class          TYPE string VALUE 'Class' ##NO_TEXT,
        key_title          TYPE string VALUE 'Title' ##NO_TEXT,
        key_description    TYPE string VALUE 'Description' ##NO_TEXT,
        key_version        TYPE string VALUE 'Version' ##NO_TEXT,
        key_namespace      TYPE string VALUE 'Namespace' ##NO_TEXT,
        key_package        TYPE string VALUE 'Package' ##NO_TEXT,
        " Registry Properties
        properties         TYPE string VALUE '.Properties' ##NO_TEXT,
        key_install_time   TYPE string VALUE 'InstallTimestamp' ##NO_TEXT,
        key_install_user   TYPE string VALUE 'InstallUser' ##NO_TEXT,
        key_uninstall_time TYPE string VALUE 'UninstallTimestamp' ##NO_TEXT,
        key_uninstall_user TYPE string VALUE 'UninstallUser' ##NO_TEXT,
        key_update_time    TYPE string VALUE 'UpdateTimestamp' ##NO_TEXT,
        key_update_user    TYPE string VALUE 'UpdateUser' ##NO_TEXT,
        " Registry Switches
        switches           TYPE string VALUE 'Switches' ##NO_TEXT,
        key_active         TYPE string VALUE 'Active' ##NO_TEXT,
        key_debug          TYPE string VALUE 'Debug' ##NO_TEXT,
        key_trace          TYPE string VALUE 'Trace' ##NO_TEXT,
        " Registry License
        license            TYPE string VALUE '.License' ##NO_TEXT,
        key_lic_id         TYPE string VALUE 'ID' ##NO_TEXT,
        key_lic_bundle     TYPE string VALUE 'BundleID' ##NO_TEXT,
        key_lic_key        TYPE string VALUE 'LicenseKey' ##NO_TEXT,
        key_lic_valid      TYPE string VALUE 'LicenseValid' ##NO_TEXT,
        key_lic_expire     TYPE string VALUE 'LicenseExpiration' ##NO_TEXT,
        " Settings
        settings           TYPE string VALUE 'Settings' ##NO_TEXT,
      END OF c_reg .
    " Evaluation
    CONSTANTS c_eval_days TYPE i VALUE 60 ##NO_TEXT.
    CONSTANTS c_eval_users TYPE i VALUE 10 ##NO_TEXT.
    CONSTANTS:
      " Actions
      BEGIN OF c_action,
        register   TYPE string VALUE 'register' ##NO_TEXT,
        unregister TYPE string VALUE 'unregister' ##NO_TEXT,
        activate   TYPE string VALUE 'activate' ##NO_TEXT,
        deactivate TYPE string VALUE 'deactivate' ##NO_TEXT,
      END OF c_action .
    DATA mbt_manifest TYPE /mbtools/if_manifest=>ty_descriptor READ-ONLY .

    " Constructor
    CLASS-METHODS class_constructor .
    METHODS constructor
      IMPORTING
        !io_tool TYPE REF TO object .
    " Class Get
    CLASS-METHODS factory
      IMPORTING
        VALUE(iv_title) TYPE csequence
      RETURNING
        VALUE(ro_tool)  TYPE REF TO /mbtools/cl_tools .
    CLASS-METHODS get_tools
      IMPORTING
        VALUE(iv_pattern)     TYPE csequence OPTIONAL
        VALUE(iv_bundle_id)   TYPE i DEFAULT -1
        VALUE(iv_get_bundles) TYPE abap_bool DEFAULT abap_false
        VALUE(iv_get_tools)   TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_tools)       TYPE /mbtools/tools_with_text .
    CLASS-METHODS f4_tools
      IMPORTING
        VALUE(iv_pattern)     TYPE csequence OPTIONAL
        VALUE(iv_bundle_id)   TYPE i DEFAULT -1
        VALUE(iv_get_bundles) TYPE abap_bool DEFAULT abap_false
        VALUE(iv_get_tools)   TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rv_title)       TYPE string .
    " Class Actions
    CLASS-METHODS run_action
      IMPORTING
        VALUE(iv_action) TYPE string
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
    " Class Manifests
    CLASS-METHODS get_manifests
      RETURNING
        VALUE(rt_manifests) TYPE /mbtools/manifests .
    " Tool Manifest
    METHODS build_manifest
      RETURNING
        VALUE(rs_manifest) TYPE /mbtools/if_manifest=>ty_descriptor .
    " Tool Register/Unregister
    METHODS register
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
    METHODS unregister
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
    " Tool Activate/Deactivate
    METHODS activate
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
    METHODS deactivate
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
    METHODS is_bundle
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
    " Tool License
    METHODS is_licensed
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
    METHODS license_add
      IMPORTING
        VALUE(iv_license) TYPE string
      RETURNING
        VALUE(rv_result)  TYPE abap_bool .
    METHODS license_remove
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
    " Tool Get
    METHODS get_id
      RETURNING
        VALUE(rv_id) TYPE string .
    METHODS get_slug
      RETURNING
        VALUE(rv_slug) TYPE string .
    METHODS get_name
      RETURNING
        VALUE(rv_name) TYPE string .
    METHODS get_title
      RETURNING
        VALUE(rv_title) TYPE string .
    METHODS get_version
      RETURNING
        VALUE(rv_version) TYPE string .
    METHODS get_bundle_id
      RETURNING
        VALUE(rv_result) TYPE string .
    METHODS get_description
      RETURNING
        VALUE(rv_description) TYPE string .
    METHODS get_class
      RETURNING
        VALUE(rv_class) TYPE string .
    METHODS get_package
      RETURNING
        VALUE(rv_package) TYPE devclass .
    METHODS get_url_repo
      RETURNING
        VALUE(rv_url) TYPE string .
    METHODS get_url_tool
      RETURNING
        VALUE(rv_url) TYPE string .
    METHODS get_url_docs
      RETURNING
        VALUE(rv_url) TYPE string .
    METHODS get_settings
      RETURNING
        VALUE(ro_reg) TYPE REF TO /mbtools/cl_registry .
    METHODS get_thumbnail
      RETURNING
        VALUE(rv_thumbnail) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_classes TYPE STANDARD TABLE OF seoclsname WITH DEFAULT KEY .

    CLASS-DATA go_reg_root TYPE REF TO /mbtools/cl_registry .
    DATA mo_tool TYPE REF TO object .
    DATA mv_id TYPE /mbtools/if_manifest=>ty_descriptor-id .
    DATA mv_bundle_id TYPE /mbtools/if_manifest=>ty_descriptor-bundle_id .
    DATA mv_is_bundle TYPE /mbtools/if_manifest=>ty_descriptor-is_bundle .
    DATA mv_title TYPE /mbtools/if_manifest=>ty_descriptor-title .
    DATA mv_name TYPE /mbtools/if_manifest=>ty_descriptor-name .
    DATA mv_version TYPE /mbtools/if_manifest=>ty_descriptor-version .
    DATA mv_description TYPE /mbtools/if_manifest=>ty_descriptor-description .

    CLASS-METHODS get_implementations
      IMPORTING
        VALUE(iv_quiet)   TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_classes) TYPE ty_classes .
    CLASS-METHODS get_reg_bundle
      IMPORTING
        !iv_bundle_id    TYPE i
      RETURNING
        VALUE(ro_result) TYPE REF TO /mbtools/cl_registry .
    CLASS-METHODS get_reg_tool
      IMPORTING
        !iv_name         TYPE string
      RETURNING
        VALUE(ro_result) TYPE REF TO /mbtools/cl_registry .
ENDCLASS.



CLASS /MBTOOLS/CL_TOOLS IMPLEMENTATION.


  METHOD activate.

    DATA:
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry.

    CHECK mv_is_bundle IS INITIAL.

    TRY.
        " Is tool already registered?
        lo_reg_tool = get_reg_tool( mv_name ).
        IF NOT lo_reg_tool IS BOUND.
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

        rv_result = abap_true.

      CATCH cx_root.
        rv_result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD build_manifest.

    rs_manifest-id          = mv_id.
    rs_manifest-bundle_id   = mv_bundle_id.
    rs_manifest-is_bundle   = mv_is_bundle.
    rs_manifest-name        = mv_name.
    rs_manifest-version     = mv_version.
    rs_manifest-title       = mv_title.
    rs_manifest-description = mv_description.

    IF mv_is_bundle IS INITIAL.
      rs_manifest-namespace   = /mbtools/if_definitions=>c_namespace.
      rs_manifest-package     = get_package( ).
      rs_manifest-class       = get_class( ).
      " APACK fields
      rs_manifest-group_id    = /mbtools/if_definitions=>c_github.
      rs_manifest-artifact_id = mv_name.
      rs_manifest-git_url     = get_url_repo( ).
    ENDIF.

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

    FIELD-SYMBOLS:
      <lv_id>          TYPE /mbtools/if_manifest=>ty_descriptor-id,
      <lv_bundle_id>   TYPE /mbtools/if_manifest=>ty_descriptor-bundle_id,
      <lv_is_bundle>   TYPE /mbtools/if_manifest=>ty_descriptor-is_bundle,
      <lv_title>       TYPE /mbtools/if_manifest=>ty_descriptor-title,
      <lv_version>     TYPE /mbtools/if_manifest=>ty_descriptor-version,
      <lv_description> TYPE /mbtools/if_manifest=>ty_descriptor-description.

    mo_tool = io_tool.

    " Each tool class must include four constants
    ASSIGN mo_tool->('C_TOOL-DOWNLOAD_ID') TO <lv_id>.
    ASSERT sy-subrc = 0. " constant is missing
    mv_id = <lv_id>.

    ASSIGN mo_tool->('C_TOOL-BUNDLE_ID') TO <lv_bundle_id>.
    ASSERT sy-subrc = 0. " constant is missing
    mv_bundle_id = <lv_bundle_id>.

    ASSIGN mo_tool->('C_TOOL-TITLE') TO <lv_title>.
    ASSERT sy-subrc = 0. " constant is missing
    mv_title = <lv_title>.
    mv_name = get_name( ).

    ASSIGN mo_tool->('C_TOOL-VERSION') TO <lv_version>.
    ASSERT sy-subrc = 0. " constant is missing
    mv_version = <lv_version>.

    ASSIGN mo_tool->('C_TOOL-DESCRIPTION') TO <lv_description>.
    ASSERT sy-subrc = 0. " constant is missing
    mv_description = <lv_description>.

    ASSIGN mo_tool->('C_TOOL-IS_BUNDLE') TO <lv_is_bundle>.
    IF sy-subrc = 0. " constant is optional
      mv_is_bundle = <lv_is_bundle>.
    ENDIF.

    " Build the full manifest based on these constants
    mbt_manifest   = build_manifest( ).

  ENDMETHOD.


  METHOD deactivate.

    DATA:
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry.

    CHECK mv_is_bundle IS INITIAL.

    TRY.
        " Is tool already registered?
        lo_reg_tool = get_reg_tool( mv_name ).
        IF NOT lo_reg_tool IS BOUND.
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

        rv_result = abap_true.

      CATCH cx_root.
        rv_result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD f4_tools.

    DATA:
      lt_tools  TYPE TABLE OF /mbtools/tool_with_text,
      ls_return TYPE ddshretval,
      lt_return TYPE TABLE OF ddshretval.

    lt_tools = get_tools( iv_pattern     = iv_pattern
                          iv_bundle_id   = iv_bundle_id
                          iv_get_bundles = iv_get_bundles
                          iv_get_tools   = iv_get_tools ).

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
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.
    LOOP AT lt_return INTO ls_return.
      rv_title = ls_return-fieldval.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD factory.

    DATA:
      lv_implementation  TYPE seoclsname,
      lt_implementations TYPE ty_classes,
      lo_tool            TYPE REF TO object,
      lo_manifest        TYPE REF TO /mbtools/if_manifest.

    lt_implementations = get_implementations( ).

    LOOP AT lt_implementations INTO lv_implementation.

      TRY.
         " Get instance of tool
          CREATE OBJECT lo_tool TYPE (lv_implementation).
          IF lo_tool IS BOUND.
            lo_manifest ?= lo_tool.
          ELSE.
            CONTINUE. "ignore
          ENDIF.

          IF lo_manifest->descriptor-title = iv_title.
            CREATE OBJECT ro_tool EXPORTING io_tool = lo_tool.
            RETURN.
          ENDIF.

        CATCH cx_root.
          CONTINUE. "ignore
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_bundle_id.

    rv_result = mv_bundle_id.

  ENDMETHOD.


  METHOD get_class.

    DATA:
      lo_class_desc TYPE REF TO cl_abap_typedescr.

    CHECK mv_is_bundle IS INITIAL.

    lo_class_desc = cl_abap_classdescr=>describe_by_object_ref( mo_tool ).
    rv_class = lo_class_desc->get_relative_name( ).

  ENDMETHOD.


  METHOD get_description.

    rv_description = mv_description.

  ENDMETHOD.


  METHOD get_id.

    " Upper case, Underscore, Namespaced
    rv_id =  to_upper( /mbtools/if_definitions=>c_namespace && mv_title ).

    REPLACE ALL OCCURRENCES OF ` ` IN rv_id WITH '_'.

  ENDMETHOD.


  METHOD get_implementations.

    " Get all classes that implement the MBT Manifest
    SELECT clsname FROM seometarel INTO TABLE rt_classes
      WHERE version    = '1'
        AND refclsname = /mbtools/if_definitions=>c_manifest.
    IF sy-subrc <> 0 AND iv_quiet IS INITIAL.
      " There are no tools installed
      MESSAGE s002(/mbtools/bc).
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD get_manifests.

    DATA:
      lv_implementation  TYPE seoclsname,
      lt_implementations TYPE ty_classes,
      lo_tool            TYPE REF TO object,
      lo_manifest        TYPE REF TO /mbtools/if_manifest,
      ls_manifest_descr  TYPE /mbtools/manifest.

    lt_implementations = get_implementations( ).

    LOOP AT lt_implementations INTO lv_implementation.

      TRY.
          " Get instance of tool
          CREATE OBJECT lo_tool TYPE (lv_implementation).
          IF lo_tool IS BOUND.
            lo_manifest ?= lo_tool.
          ELSE.
            CONTINUE. "ignore
          ENDIF.

          CLEAR ls_manifest_descr.
          MOVE-CORRESPONDING lo_manifest->descriptor TO ls_manifest_descr.
          INSERT ls_manifest_descr INTO TABLE rt_manifests.

        CATCH cx_root.
          CONTINUE. "ignore
      ENDTRY.

    ENDLOOP.

    SORT rt_manifests BY name.

  ENDMETHOD.


  METHOD get_name.

    " Note: This name determines the sort order

    " Mixed case, underscore
    rv_name = mv_title.

    REPLACE ALL OCCURRENCES OF ` ` IN rv_name WITH '_'.

  ENDMETHOD.


  METHOD get_package.

    DATA:
      lv_class TYPE string.

    CHECK mv_is_bundle IS INITIAL.

    lv_class = get_class( ).

    SELECT SINGLE devclass FROM tadir INTO rv_package
      WHERE pgmid = 'R3TR' AND object = 'CLAS' AND obj_name = lv_class.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD get_reg_bundle.

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


  METHOD get_reg_tool.

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


  METHOD get_settings.

    DATA:
     lo_reg_tool TYPE REF TO /mbtools/cl_registry.

    CHECK mv_is_bundle IS INITIAL.

    TRY.
        " Is tool installed?
        lo_reg_tool = get_reg_tool( mv_name ).
        CHECK lo_reg_tool IS BOUND.

        " Settings
        ro_reg = lo_reg_tool->get_subentry( c_reg-settings ).

      CATCH cx_root.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD get_slug.

    " Lower case, dash
    rv_slug = to_lower( mv_title ).

    REPLACE ALL OCCURRENCES OF ` ` IN rv_slug WITH '-'.

  ENDMETHOD.


  METHOD get_thumbnail.

    rv_thumbnail = get_id( ) && '_TN'.

  ENDMETHOD.


  METHOD get_title.

    rv_title = mv_title.

  ENDMETHOD.


METHOD get_tools.

    DATA:
      lv_implementation  TYPE seoclsname,
      lt_implementations TYPE ty_classes,
      lo_tool            TYPE REF TO object,
      lo_manifest        TYPE REF TO /mbtools/if_manifest,
      ls_tool_with_text  TYPE /mbtools/tool_with_text.

    FIELD-SYMBOLS:
      <ls_tool> TYPE /mbtools/tool_with_text.

    lt_implementations = get_implementations( ).

    LOOP AT lt_implementations INTO lv_implementation.

      TRY.
          " Get instance of tool
          CREATE OBJECT lo_tool TYPE (lv_implementation).
          IF lo_tool IS BOUND.
            lo_manifest ?= lo_tool.
          ELSE.
            CONTINUE. "ignore
          ENDIF.

          IF iv_bundle_id >= 0 AND lo_manifest->descriptor-bundle_id <> iv_bundle_id.
            CONTINUE.
          ENDIF.

          IF lo_manifest->descriptor-is_bundle = abap_true.
            IF iv_get_bundles = abap_false.
              CONTINUE.
            ENDIF.
          ELSE. "tool
            IF iv_get_tools = abap_false.
              CONTINUE.
            ENDIF.
          ENDIF.

          IF iv_pattern IS INITIAL OR lo_manifest->descriptor-title CP iv_pattern.
            CLEAR ls_tool_with_text.
            ls_tool_with_text-name        = lo_manifest->descriptor-title.
            ls_tool_with_text-version     = lo_manifest->descriptor-version.
            ls_tool_with_text-description = lo_manifest->descriptor-description.
            INSERT ls_tool_with_text INTO TABLE rt_tools.
          ENDIF.

        CATCH cx_root.
          CONTINUE. "ignore
      ENDTRY.

    ENDLOOP.

    SORT rt_tools BY name AS TEXT.

  ENDMETHOD.


  METHOD get_url_docs.

    " Link to documentation page on marcbernardtools.com
    rv_url = /mbtools/if_definitions=>c_home && 'docs/' && get_slug( ) && '/'.

  ENDMETHOD.


  METHOD get_url_repo.

    " Link to repository on GitHub.com
    rv_url = 'https://' && /mbtools/if_definitions=>c_github && '/' && mv_name && '.git'.

  ENDMETHOD.


  METHOD get_url_tool.

    " Link to tool page on marcbernardtools.com
    rv_url = /mbtools/if_definitions=>c_home && 'downloads/' && get_slug( ) && '/'.

  ENDMETHOD.


  METHOD get_version.

    rv_version = mv_version.

  ENDMETHOD.


  METHOD is_bundle.

    rv_result = boolc( mbt_manifest-is_bundle = abap_true ).

  ENDMETHOD.


  METHOD is_licensed.

    DATA:
      lo_reg_tool   TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry  TYPE REF TO /mbtools/cl_registry,
      lv_date_from  TYPE d,
      lv_user_count TYPE i,
      lv_value      TYPE string,
      lv_expire     TYPE string.

    " No license required for systems with few users
    " (dialog users who have logged on during evaluation period)
    lv_date_from = sy-datum - c_eval_days.

    SELECT COUNT(*) FROM usr02 INTO lv_user_count
      WHERE ustyp = 'A' AND trdat BETWEEN lv_date_from AND sy-datum
        AND ( bname <> 'DDIC' AND bname NOT LIKE '%SUPPORT%' ).
    IF lv_user_count <= c_eval_users.
      rv_result = abap_true.
      RETURN.
    ENDIF.

    TRY.
        " Is tool installed?
        lo_reg_tool = get_reg_tool( mv_name ).
        CHECK lo_reg_tool IS BOUND.

        " Properties
        lo_reg_entry = lo_reg_tool->get_subentry( c_reg-properties ).
        CHECK lo_reg_entry IS BOUND.

        " No license required during evaluation period
        lv_value = lo_reg_entry->get_value( c_reg-key_install_time ).
        lv_date_from = lv_value(8) + c_eval_days.
        IF lv_date_from >= sy-datum.
          rv_result = abap_true.
          RETURN.
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


  METHOD license_add.

    DATA:
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry,
      lv_id        TYPE string,
      lv_valid     TYPE abap_bool,
      lv_expire    TYPE d.

    TRY.
        " Is tool installed?
        lo_reg_tool = get_reg_tool( mv_name ).
        CHECK lo_reg_tool IS BOUND.

        " License
        lo_reg_entry = lo_reg_tool->get_subentry( c_reg-license ).
        CHECK lo_reg_entry IS BOUND.

        lv_id = lo_reg_entry->get_value( c_reg-key_lic_id ).

        " Is license valid?
        lo_reg_entry->set_value( iv_key   = c_reg-key_lic_key
                                 iv_value = iv_license ).

        /mbtools/cl_edd=>check_license(
          EXPORTING
            iv_id      = lv_id
            iv_license = iv_license
          IMPORTING
            ev_valid   = lv_valid
            ev_expire  = lv_expire ).

        lo_reg_entry->set_value( iv_key   = c_reg-key_lic_valid
                                 iv_value = lv_valid ).
        lo_reg_entry->set_value( iv_key   = c_reg-key_lic_expire
                                 iv_value = lv_expire ).

        lo_reg_entry->save( ).

        IF lv_valid = abap_true AND lv_expire >= sy-datum.
          rv_result = abap_true.
        ENDIF.

      CATCH cx_root.
        rv_result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD license_remove.

    DATA:
      lo_reg_tool  TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry TYPE REF TO /mbtools/cl_registry.

    TRY.
        " Is tool installed?
        lo_reg_tool = get_reg_tool( mv_name ).
        CHECK lo_reg_tool IS BOUND.

        " License
        lo_reg_entry = lo_reg_tool->get_subentry( c_reg-license ).
        CHECK lo_reg_entry IS BOUND.

        " Remove license key
        lo_reg_entry->set_value( c_reg-key_lic_key ).
        lo_reg_entry->set_value( c_reg-key_lic_valid ).
        lo_reg_entry->set_value( iv_key   = c_reg-key_lic_expire
                                 iv_value = '99991231' ).

        lo_reg_entry->save( ).

        rv_result = abap_true.

      CATCH cx_root.
        rv_result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD register.

    DATA:
      lo_reg_tool   TYPE REF TO /mbtools/cl_registry,
      lo_reg_bundle TYPE REF TO /mbtools/cl_registry,
      lo_reg_entry  TYPE REF TO /mbtools/cl_registry,
      lv_timestamp  TYPE timestamp.

    TRY.
        " Is tool already registered?
        IF mv_is_bundle IS INITIAL.
          lo_reg_tool = get_reg_tool( mv_name ).
        ELSE.
          lo_reg_tool = go_reg_root->get_subentry( mv_name ).
        ENDIF.
        IF lo_reg_tool IS BOUND.
          rv_result = abap_true.
        ELSE.
          " Create registry entries
          IF mv_is_bundle IS INITIAL.
            lo_reg_bundle = get_reg_bundle( mv_bundle_id ).
            IF lo_reg_bundle IS BOUND.
              lo_reg_tool = lo_reg_bundle->add_subentry( mv_name ).
            ELSE.
              " Bundle must be installed before tool
              RETURN. ">>>
            ENDIF.
          ELSE.
            lo_reg_tool = go_reg_root->add_subentry( mv_name ).
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
                                   iv_value = mv_name ).
          lo_reg_entry->set_value( iv_key   = c_reg-key_version
                                   iv_value = mv_version ).
          lo_reg_entry->set_value( iv_key   = c_reg-key_title
                                   iv_value = mv_title ).
          lo_reg_entry->set_value( iv_key   = c_reg-key_description
                                   iv_value = mv_description ).
          IF mv_is_bundle IS INITIAL.
            lo_reg_entry->set_value( iv_key   = c_reg-key_namespace
                                     iv_value = /mbtools/if_definitions=>c_namespace ).
            lo_reg_entry->set_value( iv_key   = c_reg-key_package
                                     iv_value = get_package( ) ).
            lo_reg_entry->set_value( iv_key   = c_reg-key_class
                                     iv_value = get_class( ) ).
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
            lo_reg_entry->set_value( iv_key = c_reg-key_update_time
                                     iv_value = lv_timestamp ).
            lo_reg_entry->set_value( iv_key = c_reg-key_update_user
                                     iv_value = sy-uname ).
          ELSE.
            lo_reg_entry->set_value( iv_key   = c_reg-key_install_time
                                     iv_value = lv_timestamp ).
            lo_reg_entry->set_value( iv_key   = c_reg-key_install_user
                                     iv_value = sy-uname ).
            lo_reg_entry->set_value( c_reg-key_update_time ).
            lo_reg_entry->set_value( c_reg-key_update_user ).
          ENDIF.
          lo_reg_entry->set_value( c_reg-key_uninstall_time ).
          lo_reg_entry->set_value( c_reg-key_uninstall_user ).
          lo_reg_entry->save( ).
        ENDIF.

        IF rv_result = abap_false.
*         Switches
          IF mv_is_bundle IS INITIAL.
            lo_reg_entry = lo_reg_tool->add_subentry( c_reg-switches ).
            IF lo_reg_entry IS BOUND.
              lo_reg_entry->set_value( c_reg-key_active ).
              lo_reg_entry->set_value( c_reg-key_debug  ).
              lo_reg_entry->set_value( c_reg-key_trace  ).
              lo_reg_entry->save( ).
            ENDIF.
          ENDIF.

          " License
          lo_reg_entry = lo_reg_tool->add_subentry( c_reg-license ).
          IF lo_reg_entry IS BOUND.
            lo_reg_entry->set_value( iv_key   = c_reg-key_lic_id
                                     iv_value = mv_id ).
            lo_reg_entry->set_value( iv_key   = c_reg-key_lic_bundle
                                     iv_value = mv_bundle_id ).
            lo_reg_entry->set_value( iv_key   = c_reg-key_lic_expire
                                     iv_value = '99991231' ).
            lo_reg_entry->set_value( c_reg-key_lic_key ).
            lo_reg_entry->set_value( c_reg-key_lic_valid ).
            lo_reg_entry->save( ).
          ENDIF.

          " Settings
          IF mv_is_bundle IS INITIAL.
            lo_reg_entry = lo_reg_tool->add_subentry( c_reg-settings ).
          ENDIF.
        ENDIF.

        " Save
        lo_reg_tool->save( ).

        rv_result = abap_true.

      CATCH cx_root.
        rv_result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD run_action.

    DATA:
      ls_tool   TYPE /mbtools/tool_with_text,
      lt_tools  TYPE TABLE OF /mbtools/tool_with_text,
      lv_result TYPE abap_bool.

    lt_tools = get_tools( ).

    rv_result = abap_true.

    LOOP AT lt_tools INTO ls_tool.

      CASE iv_action.
        WHEN c_action-register.
          lv_result = factory( ls_tool-name )->register( ).
        WHEN c_action-unregister.
          lv_result = factory( ls_tool-name )->unregister( ).
        WHEN c_action-activate.
          lv_result = factory( ls_tool-name )->activate( ).
        WHEN c_action-deactivate.
          lv_result = factory( ls_tool-name )->deactivate( ).
        WHEN OTHERS.
          " unknow action
          ASSERT 0 = 1.
      ENDCASE.

      IF lv_result = abap_false.
        rv_result = abap_false.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD unregister.

    DATA:
      lo_reg_bundle TYPE REF TO /mbtools/cl_registry,
      lo_reg_tool   TYPE REF TO /mbtools/cl_registry,
      ls_entry      TYPE /mbtools/cl_registry=>ty_keyobj ##NEEDED,
      lt_entries    TYPE /mbtools/cl_registry=>ty_keyobjs.

    TRY.
        " Is tool still registered?
        IF mv_is_bundle IS INITIAL.
          lo_reg_tool = get_reg_tool( mv_name ).
        ELSE.
          lo_reg_tool = go_reg_root->get_subentry( mv_name ).
        ENDIF.
        IF NOT lo_reg_tool IS BOUND.
          rv_result = abap_true.
          RETURN.
        ENDIF.

        " Get bundle
        lo_reg_bundle = get_reg_bundle( mv_bundle_id ).
        IF NOT lo_reg_bundle IS BOUND.
          RETURN. ">>>
        ENDIF.

        " Remove registry branch
        IF mv_is_bundle IS INITIAL.
          lo_reg_bundle->remove_subentry( mv_name ).
        ELSE.
          " Check if any tools are still registered
          lt_entries = lo_reg_bundle->get_subentries( ).
          LOOP AT lt_entries INTO ls_entry WHERE key CP 'MBT*'.
            EXIT.
          ENDLOOP.
          IF sy-subrc <> 0.
            go_reg_root->remove_subentry( mv_name ).
          ELSE.
            RETURN. ">>>
          ENDIF.
        ENDIF.

        rv_result = abap_true.

      CATCH cx_root.
        rv_result = abap_false.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
