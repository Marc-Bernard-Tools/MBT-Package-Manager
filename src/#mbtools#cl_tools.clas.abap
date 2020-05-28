************************************************************************
* /MBTOOLS/CL_TOOLS
* MBT Tools Manager
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_apack_manifest .
    INTERFACES /mbtools/if_manifest .

    ALIASES apack_manifest
      FOR if_apack_manifest~descriptor .
    ALIASES mbt_manifest
      FOR /mbtools/if_manifest~descriptor .

*   Global Constant
    CONSTANTS c_github TYPE string VALUE 'github.com/mbtools' ##NO_TEXT.
    CONSTANTS c_home TYPE string VALUE 'https://marcbernardtools.com/' ##NO_TEXT.
    CONSTANTS c_terms TYPE string VALUE 'https://marcbernardtools.com/company/terms-software/' ##NO_TEXT.
    CONSTANTS c_namespace TYPE devclass VALUE '/MBTOOLS/' ##NO_TEXT.
    CONSTANTS c_manifest TYPE seoclsname VALUE '/MBTOOLS/IF_MANIFEST' ##NO_TEXT.
*   Tool Constants
    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NO_TEXT.
    CONSTANTS c_title TYPE string VALUE 'Marc Bernard Tools' ##NO_TEXT.
    CONSTANTS c_description TYPE string VALUE 'Essential Tools for SAP® Customers & Partners by Marc Bernard Tools' ##NO_TEXT.
    CONSTANTS c_download_id TYPE i VALUE 4480 ##NO_TEXT.
*   Registry General
    CONSTANTS co_reg_general TYPE string VALUE 'General^' ##NO_TEXT.
    CONSTANTS co_key_name TYPE string VALUE 'Name' ##NO_TEXT.
    CONSTANTS co_key_object TYPE string VALUE 'Object' ##NO_TEXT.
    CONSTANTS co_key_title TYPE string VALUE 'Title' ##NO_TEXT.
    CONSTANTS co_key_description TYPE string VALUE 'Description' ##NO_TEXT.
    CONSTANTS co_key_version TYPE string VALUE 'Version' ##NO_TEXT.
    CONSTANTS co_key_namespace TYPE string VALUE 'Namespace' ##NO_TEXT.
    CONSTANTS co_key_package TYPE string VALUE 'Package' ##NO_TEXT.
*   Registry Properties
    CONSTANTS co_reg_properties TYPE string VALUE 'Properties^' ##NO_TEXT.
    CONSTANTS co_key_install_time TYPE string VALUE 'InstallTimestamp' ##NO_TEXT.
    CONSTANTS co_key_install_user TYPE string VALUE 'InstallUser' ##NO_TEXT.
    CONSTANTS co_key_uninstall_time TYPE string VALUE 'UninstallTimestamp' ##NO_TEXT.
    CONSTANTS co_key_uninstall_user TYPE string VALUE 'UninstallUser' ##NO_TEXT.
    CONSTANTS co_key_update_time TYPE string VALUE 'UpdateTimestamp' ##NO_TEXT.
    CONSTANTS co_key_update_user TYPE string VALUE 'UpdateUser' ##NO_TEXT.
*   Registry Switches
    CONSTANTS co_reg_switches TYPE string VALUE 'Switches' ##NO_TEXT.
    CONSTANTS co_key_active TYPE string VALUE 'Active' ##NO_TEXT.
    CONSTANTS co_key_debug TYPE string VALUE 'Debug' ##NO_TEXT.
    CONSTANTS co_key_trace TYPE string VALUE 'Trace' ##NO_TEXT.
*   Registry License
    CONSTANTS co_reg_license TYPE string VALUE 'License^' ##NO_TEXT.
    CONSTANTS co_key_lic_id TYPE string VALUE 'ID' ##NO_TEXT.
    CONSTANTS co_key_lic_key TYPE string VALUE 'LicenseKey' ##NO_TEXT.
    CONSTANTS co_key_lic_valid TYPE string VALUE 'LicenseValid' ##NO_TEXT.
    CONSTANTS co_key_lic_expire TYPE string VALUE 'LicenseExpiration' ##NO_TEXT.
*   Evaluation
    CONSTANTS co_eval_days TYPE i VALUE 30 ##NO_TEXT.
    CONSTANTS co_eval_users TYPE i VALUE 10 ##NO_TEXT.

    CLASS-METHODS class_constructor .
    METHODS constructor .
    CLASS-METHODS register
      IMPORTING
        VALUE(i_object)     TYPE string
      RETURNING
        VALUE(r_registered) TYPE abap_bool .
    CLASS-METHODS register_all
      RETURNING
        VALUE(r_registered) TYPE abap_bool .
    CLASS-METHODS unregister
      IMPORTING
        VALUE(i_object)       TYPE string
      RETURNING
        VALUE(r_unregistered) TYPE abap_bool .
    CLASS-METHODS unregister_all
      RETURNING
        VALUE(r_unregistered) TYPE abap_bool .
    CLASS-METHODS activate
      IMPORTING
        VALUE(i_object)    TYPE string
      RETURNING
        VALUE(r_activated) TYPE abap_bool .
    CLASS-METHODS deactivate
      IMPORTING
        VALUE(i_object)      TYPE string
      RETURNING
        VALUE(r_deactivated) TYPE abap_bool .
    CLASS-METHODS is_active
      IMPORTING
        VALUE(i_title)  TYPE string
      RETURNING
        VALUE(r_active) TYPE abap_bool .
    CLASS-METHODS is_licensed
      IMPORTING
        VALUE(i_title)    TYPE string
      RETURNING
        VALUE(r_licensed) TYPE abap_bool .
    CLASS-METHODS license_add
      IMPORTING
        VALUE(i_title)    TYPE string
        VALUE(i_license)  TYPE string
      RETURNING
        VALUE(r_licensed) TYPE abap_bool .
    CLASS-METHODS license_remove
      IMPORTING
        VALUE(i_title)   TYPE string
      RETURNING
        VALUE(r_removed) TYPE abap_bool .
    CLASS-METHODS get_slug
      IMPORTING
        !i_title      TYPE string
      RETURNING
        VALUE(r_slug) TYPE string .
    CLASS-METHODS get_name
      IMPORTING
        !i_title      TYPE string
      RETURNING
        VALUE(r_name) TYPE string .
    CLASS-METHODS get_package
      IMPORTING
        !i_class         TYPE REF TO object
      RETURNING
        VALUE(r_package) TYPE devclass .
    CLASS-METHODS get_class
      IMPORTING
        VALUE(i_title) TYPE string OPTIONAL
        VALUE(i_name)  TYPE string OPTIONAL
      RETURNING
        VALUE(r_class) TYPE devclass .
    CLASS-METHODS get_url_repo
      IMPORTING
        !i_title     TYPE string
      RETURNING
        VALUE(r_url) TYPE string .
    CLASS-METHODS get_url_tool
      IMPORTING
        !i_title     TYPE string
      RETURNING
        VALUE(r_url) TYPE string .
    CLASS-METHODS get_url_docs
      IMPORTING
        !i_title     TYPE string
      RETURNING
        VALUE(r_url) TYPE string .
    CLASS-METHODS build_apack_manifest
      IMPORTING
        !i_class          TYPE REF TO object
      RETURNING
        VALUE(r_manifest) TYPE zif_apack_manifest=>ty_descriptor .
    CLASS-METHODS build_mbt_manifest
      IMPORTING
        !i_class          TYPE REF TO object
      RETURNING
        VALUE(r_manifest) TYPE /mbtools/if_manifest=>ty_descriptor .
    CLASS-METHODS get_tools
      IMPORTING
        VALUE(i_pattern) TYPE string OPTIONAL
      RETURNING
        VALUE(r_tools)   TYPE /mbtools/tools_with_text .
    CLASS-METHODS f4_tools
      IMPORTING
        VALUE(i_pattern) TYPE string OPTIONAL
      RETURNING
        VALUE(r_name)    TYPE string .
    CLASS-METHODS get_manifests
      RETURNING
        VALUE(r_manifests) TYPE /mbtools/manifests .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA reg_root TYPE REF TO /mbtools/cl_registry .
ENDCLASS.



CLASS /MBTOOLS/CL_TOOLS IMPLEMENTATION.


  METHOD activate.

    DATA:
      tool      TYPE REF TO object,
      manifest  TYPE REF TO /mbtools/if_manifest,
      reg_tool  TYPE REF TO /mbtools/cl_registry,
      reg_entry TYPE REF TO /mbtools/cl_registry.

    TRY.
        " Get instance of tool
        CREATE OBJECT tool TYPE (i_object).
        IF tool IS BOUND.
          manifest ?= tool.
        ELSE.
          r_activated = abap_false.
          RETURN.
        ENDIF.

        " Is tool already registered?
        reg_tool = reg_root->get_subentry( manifest->descriptor-name ).
        IF NOT reg_tool IS BOUND.
          r_activated = abap_false.
          RETURN.
        ENDIF.

        " Switches
        reg_entry = reg_tool->add_subentry( co_reg_switches ).
        IF reg_entry IS BOUND.
          reg_entry->set_value( key = co_key_active value = abap_true ).
          reg_entry->save( ).
        ENDIF.

        r_activated = abap_true.

      CATCH cx_root.
        r_activated = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD build_apack_manifest.

    FIELD-SYMBOLS: <title> TYPE any.

    ASSIGN i_class->('C_TITLE') TO <title>.
    ASSERT sy-subrc = 0. " constant is missing

    r_manifest = VALUE #(
      group_id       = c_github
      artifact_id    = get_name( <title> )
      version        = c_version
      git_url        = get_url_repo( <title> )
*      target_package = get_package( i_class )
    ).

  ENDMETHOD.


  METHOD build_mbt_manifest.

    DATA:
      class_desc TYPE REF TO cl_abap_typedescr,
      class_name TYPE string.

    FIELD-SYMBOLS:
      <id>          TYPE i,
      <title>       TYPE string,
      <version>     TYPE string,
      <description> TYPE string.

    ASSIGN i_class->('C_DOWNLOAD_ID') TO <id>.
    ASSERT sy-subrc = 0. " constant is missing

    ASSIGN i_class->('C_TITLE') TO <title>.
    ASSERT sy-subrc = 0. " constant is missing

    ASSIGN i_class->('C_VERSION') TO <version>.
    ASSERT sy-subrc = 0. " constant is missing

    ASSIGN i_class->('C_DESCRIPTION') TO <description>.
    ASSERT sy-subrc = 0. " constant is missing

    class_desc = cl_abap_classdescr=>describe_by_object_ref( i_class ).
    class_name = class_desc->get_relative_name( ).

    r_manifest = VALUE #(
      id          = <id>
      name        = get_name( <title> )
      version     = <version>
      title       = <title>
      description = <description>
      namespace   = c_namespace
      package     = get_package( i_class )
      class       = class_name
    ).

  ENDMETHOD.


  METHOD class_constructor.

    LOG-POINT ID /mbtools/bc SUBKEY c_title FIELDS sy-datum sy-uzeit sy-uname.

    " Get root of registry
    reg_root = /mbtools/cl_registry=>get_root( ).

  ENDMETHOD.


  METHOD constructor.
    apack_manifest = build_apack_manifest( me ).
    mbt_manifest = build_mbt_manifest( me ).

    " We want this to be first in the registry
    " I.e. MARC_BERNARD_TOOLS is before MBT_... in the sort order but Marc_Bernard_Tools isn't
    mbt_manifest-name = to_upper( mbt_manifest-name ).
  ENDMETHOD.


  METHOD deactivate.

    DATA:
      tool      TYPE REF TO object,
      manifest  TYPE REF TO /mbtools/if_manifest,
      reg_tool  TYPE REF TO /mbtools/cl_registry,
      reg_entry TYPE REF TO /mbtools/cl_registry.

    TRY.
        " Get instance of tool
        CREATE OBJECT tool TYPE (i_object).
        IF tool IS BOUND.
          manifest ?= tool.
        ELSE.
          r_deactivated = abap_false.
          RETURN.
        ENDIF.

        " Is tool already registered?
        reg_tool = reg_root->get_subentry( manifest->descriptor-name ).
        IF NOT reg_tool IS BOUND.
          r_deactivated = abap_false.
          RETURN.
        ENDIF.

        " Switches
        reg_entry = reg_tool->add_subentry( co_reg_switches ).
        IF reg_entry IS BOUND.
          reg_entry->set_value( key = co_key_active value = abap_false ).
          reg_entry->save( ).
        ENDIF.

        r_deactivated = abap_true.

      CATCH cx_root.
        r_deactivated = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD f4_tools.

    DATA:
      tools   TYPE TABLE OF /mbtools/tool_with_text,
      return  TYPE ddshretval,
      returns TYPE TABLE OF ddshretval.

    tools = get_tools( i_pattern = i_pattern ).

    " Show F4-Popup
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'NAME'
        window_title    = 'Tools'
        value_org       = 'S'
      TABLES
        value_tab       = tools
        return_tab      = returns
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.
    LOOP AT returns INTO return.
      r_name = return-fieldval.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_class.

    DATA:
      tool            TYPE REF TO object,
      manifest        TYPE REF TO /mbtools/if_manifest,
      implementation  TYPE seoclsname,
      implementations TYPE TABLE OF seoclsname.

    " Get all classes that implement the MBT Manifest
    SELECT clsname FROM seometarel INTO TABLE implementations
      WHERE seometarel~version    = '1'
        AND seometarel~refclsname = c_manifest.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT implementations INTO implementation.

      TRY.
          " Get instance of tool
          CREATE OBJECT tool TYPE (implementation).
          IF tool IS BOUND.
            manifest ?= tool.
          ELSE.
            CONTINUE. "ignore
          ENDIF.

          IF manifest->descriptor-title = i_title OR manifest->descriptor-name = i_name.
            r_class = implementation.
            RETURN.
          ENDIF.

        CATCH cx_root.
          CONTINUE. "ignore
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_manifests.

    DATA:
      implementation  TYPE seoclsname,
      implementations TYPE TABLE OF seoclsname,
      tool            TYPE REF TO object,
      manifest        TYPE REF TO /mbtools/if_manifest,
      manifest_descr  TYPE /mbtools/manifest.

    " Get all classes that implement the MBT Manifest
    SELECT clsname FROM seometarel INTO TABLE implementations
      WHERE seometarel~version    = '1'
        AND seometarel~refclsname = c_manifest.
    IF sy-subrc <> 0.
      MESSAGE s000(/mbtools/bc) WITH 'No tools found'.
      RETURN.
    ENDIF.

    LOOP AT implementations INTO implementation.

      TRY.
          " Get instance of tool
          CREATE OBJECT tool TYPE (implementation).
          IF tool IS BOUND.
            manifest ?= tool.
          ELSE.
            CONTINUE. "ignore
          ENDIF.

          CLEAR manifest_descr.
          MOVE-CORRESPONDING manifest->descriptor TO manifest_descr.
          INSERT manifest_descr INTO TABLE r_manifests.

        CATCH cx_root.
          CONTINUE. "ignore
      ENDTRY.

    ENDLOOP.

    SORT r_manifests BY name.

  ENDMETHOD.


  METHOD get_name.

    r_name = i_title.

    REPLACE ALL OCCURRENCES OF ` ` IN r_name WITH '_'.

  ENDMETHOD.


  METHOD get_package.

    DATA:
      class_desc TYPE REF TO cl_abap_typedescr,
      class_name TYPE string.

    class_desc = cl_abap_classdescr=>describe_by_object_ref( i_class ).

    class_name = class_desc->get_relative_name( ).

    SELECT SINGLE devclass FROM tadir INTO r_package
      WHERE pgmid = 'R3TR' AND object = 'CLAS' AND obj_name = class_name.
    ASSERT sy-subrc = 0.  " If it dumps, you didn't pass a reference to a class

  ENDMETHOD.


  METHOD get_slug.

    r_slug = to_lower( i_title ).

    REPLACE ALL OCCURRENCES OF space IN r_slug WITH '-'.

  ENDMETHOD.


  METHOD get_tools.

    DATA:
      implementation  TYPE seoclsname,
      implementations TYPE TABLE OF seoclsname,
      tool            TYPE REF TO object,
      manifest        TYPE REF TO /mbtools/if_manifest,
      tool_with_text  TYPE /mbtools/tool_with_text.

    " Get all classes that implement the MBT Manifest
    SELECT clsname FROM seometarel INTO TABLE implementations
      WHERE seometarel~version    = '1'
        AND seometarel~refclsname = c_manifest.
    IF sy-subrc <> 0.
      MESSAGE s000(/mbtools/bc) WITH 'No tools found'.
      RETURN.
    ENDIF.

    LOOP AT implementations INTO implementation.

      TRY.
          " Get instance of tool
          CREATE OBJECT tool TYPE (implementation).
          IF tool IS BOUND.
            manifest ?= tool.
          ELSE.
            CONTINUE. "ignore
          ENDIF.

          CLEAR tool_with_text.
          tool_with_text-name = manifest->descriptor-name.

          IF i_pattern IS INITIAL OR tool_with_text-name CP i_pattern.
            tool_with_text-version     = manifest->descriptor-version.
            tool_with_text-description = manifest->descriptor-description.
            INSERT tool_with_text INTO TABLE r_tools.
          ENDIF.

        CATCH cx_root.
          CONTINUE. "ignore
      ENDTRY.

    ENDLOOP.

    SORT r_tools BY name.

  ENDMETHOD.


  METHOD get_url_docs.

    " Link to documentation page on marcbernardtools.com
    r_url = c_home && 'docs/' && get_slug( i_title ) && '/'.

  ENDMETHOD.


  METHOD get_url_repo.

    " Link to repository on GitHub.com
    r_url = 'https://' && c_github && '/' && get_name( i_title ) && '.git'.

  ENDMETHOD.


  METHOD get_url_tool.

    " Link to tool page on marcbernardtools.com
    r_url = c_home && 'downloads/' && get_slug( i_title ) && '/'.

  ENDMETHOD.


  METHOD is_active.

    DATA:
      reg_tool  TYPE REF TO /mbtools/cl_registry,
      reg_entry TYPE REF TO /mbtools/cl_registry,
      val       TYPE string.

    TRY.
        " Is tool installed?
        reg_tool = reg_root->get_subentry( get_name( i_title ) ).
        CHECK reg_tool IS BOUND.

        " Switches entry
        reg_entry = reg_tool->get_subentry( co_reg_switches ).
        CHECK reg_entry IS BOUND.

        " Is tool active?
        val = reg_entry->get_value( co_key_active ).
        IF val = abap_true OR val CS sy-uname.
          r_active = abap_true.
        ENDIF.

      CATCH cx_root.
        r_active = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD is_licensed.

    DATA:
      reg_tool   TYPE REF TO /mbtools/cl_registry,
      reg_entry  TYPE REF TO /mbtools/cl_registry,
      date_from  TYPE d,
      user_count TYPE i,
      val        TYPE string,
      expire     TYPE string.

    " No license required for systems with few users
    " (dialog users who have logged on during evaluation period)
    date_from = sy-datum - co_eval_days.

    SELECT COUNT(*) FROM usr02 INTO user_count
      WHERE ustyp = 'A' AND trdat BETWEEN date_from AND sy-datum
        AND ( bname <> 'DDIC' AND bname NOT LIKE '%SUPPORT%' ).
    IF user_count <= co_eval_users.
      r_licensed = abap_true.
      RETURN.
    ENDIF.

    TRY.
        " Is tool installed?
        reg_tool = reg_root->get_subentry( get_name( i_title ) ).
        CHECK reg_tool IS BOUND.

        " Properties
        reg_entry = reg_tool->get_subentry( co_reg_properties ).
        CHECK reg_entry IS BOUND.

        " No license required during evaluation period
        val = reg_entry->get_value( co_key_install_time ).
        date_from = val(8) + co_eval_days.
        IF date_from >= sy-datum.
          r_licensed = abap_true.
          RETURN.
        ENDIF.

        " License
        reg_entry = reg_tool->get_subentry( co_reg_license ).
        CHECK reg_entry IS BOUND.

        " Is license valid?
        val = reg_entry->get_value( co_key_lic_valid ).
        expire = reg_entry->get_value( co_key_lic_expire ).
        IF val = abap_true AND expire >= sy-datum.
          r_licensed = abap_true.
        ENDIF.

      CATCH cx_root.
        r_licensed = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD license_add.

    DATA:
      reg_tool  TYPE REF TO /mbtools/cl_registry,
      reg_entry TYPE REF TO /mbtools/cl_registry,
      id        TYPE string,
      valid     TYPE abap_bool,
      expire    TYPE d.

    TRY.
        " Is tool installed?
        reg_tool = reg_root->get_subentry( get_name( i_title ) ).
        CHECK reg_tool IS BOUND.

        " License
        reg_entry = reg_tool->get_subentry( co_reg_license ).
        CHECK reg_entry IS BOUND.

        id = reg_entry->get_value( key = co_key_lic_id ).

        " Is license valid?
        reg_entry->set_value( key = co_key_lic_key value = i_license ).

        CALL METHOD /mbtools/cl_edd=>check_license
          EXPORTING
            i_id      = id
            i_license = i_license
          IMPORTING
            e_valid   = valid
            e_expire  = expire.

        reg_entry->set_value( key = co_key_lic_valid value = valid ).
        reg_entry->set_value( key = co_key_lic_expire value = expire ).

        reg_entry->save( ).

        IF valid = abap_true AND expire >= sy-datum.
          r_licensed = abap_true.
        ENDIF.

      CATCH cx_root.
        r_licensed = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD license_remove.

    DATA:
      reg_tool  TYPE REF TO /mbtools/cl_registry,
      reg_entry TYPE REF TO /mbtools/cl_registry.

    TRY.
        " Is tool installed?
        reg_tool = reg_root->get_subentry( get_name( i_title ) ).
        CHECK reg_tool IS BOUND.

        " License
        reg_entry = reg_tool->get_subentry( co_reg_license ).
        CHECK reg_entry IS BOUND.

        " Remove license key
        reg_entry->set_value( key = co_key_lic_key value = '' ).
        reg_entry->set_value( key = co_key_lic_valid value = '' ).
        reg_entry->set_value( key = co_key_lic_expire value = '99991231' ).

        reg_entry->save( ).

        r_removed = abap_true.

      CATCH cx_root.
        r_removed = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD register.

    DATA:
      tool      TYPE REF TO object,
      manifest  TYPE REF TO /mbtools/if_manifest,
      reg_tool  TYPE REF TO /mbtools/cl_registry,
      reg_entry TYPE REF TO /mbtools/cl_registry,
      timestamp TYPE timestamp.

    TRY.
        " Get instance of tool
        CREATE OBJECT tool TYPE (i_object).
        IF tool IS BOUND.
          manifest ?= tool.
        ELSE.
          r_registered = abap_false.
          RETURN.
        ENDIF.

        " Is tool already registered?
        reg_tool = reg_root->get_subentry( manifest->descriptor-name ).
        IF reg_tool IS BOUND.
          r_registered = abap_true.
        ELSE.
          " Create registry entries
          reg_tool = reg_root->add_subentry( manifest->descriptor-name ).
          CHECK reg_tool IS BOUND.
        ENDIF.

        " General
        IF r_registered = abap_true.
          reg_entry = reg_tool->get_subentry( co_reg_general ).
        ELSE.
          reg_entry = reg_tool->add_subentry( co_reg_general ).
        ENDIF.
        IF reg_entry IS BOUND.
          reg_entry->set_value( key = co_key_object      value = i_object ).
          reg_entry->set_value( key = co_key_name        value = manifest->descriptor-name ).
          reg_entry->set_value( key = co_key_package     value = manifest->descriptor-package ).
          reg_entry->set_value( key = co_key_namespace   value = manifest->descriptor-namespace ).
          reg_entry->set_value( key = co_key_version     value = manifest->descriptor-version ).
          reg_entry->set_value( key = co_key_title       value = manifest->descriptor-title ).
          reg_entry->set_value( key = co_key_description value = manifest->descriptor-description ).
          reg_entry->save( ).
        ENDIF.

        " Properties
        IF r_registered = abap_true.
          reg_entry = reg_tool->get_subentry( co_reg_properties ).
        ELSE.
          reg_entry = reg_tool->add_subentry( co_reg_properties ).
        ENDIF.
        IF reg_entry IS BOUND.
          GET TIME STAMP FIELD timestamp.
          IF r_registered = abap_true.
            reg_entry->set_value( key = co_key_update_time value = timestamp ).
            reg_entry->set_value( key = co_key_update_user value = sy-uname ).
          ELSE.
            reg_entry->set_value( key = co_key_install_time value = timestamp ).
            reg_entry->set_value( key = co_key_install_user value = sy-uname ).
            reg_entry->set_value( key = co_key_update_time  value = '' ).
            reg_entry->set_value( key = co_key_update_user  value = '' ).
          ENDIF.
          reg_entry->set_value( key = co_key_uninstall_time value = '' ).
          reg_entry->set_value( key = co_key_uninstall_user value = '' ).
          reg_entry->save( ).
        ENDIF.

        IF r_registered = abap_false.
*         Switches
          reg_entry = reg_tool->add_subentry( co_reg_switches ).
          IF reg_entry IS BOUND.
            reg_entry->set_value( key = co_key_active value = '' ).
            reg_entry->set_value( key = co_key_debug  value = '' ).
            reg_entry->set_value( key = co_key_trace  value = '' ).
            reg_entry->save( ).
          ENDIF.

          " License
          reg_entry = reg_tool->add_subentry( co_reg_license ).
          IF reg_entry IS BOUND.
            reg_entry->set_value( key = co_key_lic_id     value = manifest->descriptor-id ).
            reg_entry->set_value( key = co_key_lic_expire value = '99991231' ).
            reg_entry->set_value( key = co_key_lic_key    value = '' ).
            reg_entry->set_value( key = co_key_lic_valid  value = '' ).
            reg_entry->save( ).
          ENDIF.
        ENDIF.

        " Save
        reg_tool->save( ).

        r_registered = abap_true.

      CATCH cx_root.
        r_registered = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD register_all.

    DATA:
      name       TYPE string,
      tool       TYPE /mbtools/tool_with_text,
      tools      TYPE TABLE OF /mbtools/tool_with_text,
      registered TYPE abap_bool.

    tools = get_tools( ).

    r_registered = abap_true.

    LOOP AT tools INTO tool.
      name = tool-name.
      registered = register( i_object = get_class( i_name = name ) ).
      IF registered = abap_false.
        r_registered = abap_false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD unregister.

    DATA:
      tool     TYPE REF TO object,
      manifest TYPE REF TO /mbtools/if_manifest,
      reg_tool TYPE REF TO /mbtools/cl_registry.

    TRY.
        " Get instance of tool
        CREATE OBJECT tool TYPE (i_object).
        IF tool IS BOUND.
          manifest ?= tool.
        ELSE.
          r_unregistered = abap_false.
          RETURN.
        ENDIF.

        " Is tool still registered?
        reg_tool = reg_root->get_subentry( manifest->descriptor-name ).
        IF NOT reg_tool IS BOUND.
          r_unregistered = abap_true.
          RETURN.
        ENDIF.

        " Remove registry branch
        reg_root->remove_subentry( manifest->descriptor-name ).
        CHECK reg_tool IS BOUND.

        r_unregistered = abap_true.

      CATCH cx_root.
        r_unregistered = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD unregister_all.

    DATA:
      name         TYPE string,
      tool         TYPE /mbtools/tool_with_text,
      tools        TYPE TABLE OF /mbtools/tool_with_text,
      unregistered TYPE abap_bool.

    tools = get_tools( ).

    r_unregistered = abap_true.

    LOOP AT tools INTO tool.
      name = tool-name.
      unregistered = unregister( i_object = get_class( i_name = name ) ).
      IF unregistered = abap_false.
        r_unregistered = abap_false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
