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

    CONSTANTS c_name TYPE string VALUE 'MARC_BERNARD_TOOLS' ##NO_TEXT.
    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NO_TEXT.
    CONSTANTS c_title TYPE string VALUE 'Marc Bernard Tools' ##NO_TEXT.
    CONSTANTS c_description TYPE string VALUE 'Essential Tools for SAP® Customers & Partners by Marc Bernard Tools' ##NO_TEXT.
    CONSTANTS c_home TYPE string VALUE 'https://marcbernardtools.com/' ##NO_TEXT.
    CONSTANTS c_uri TYPE string VALUE 'https://marcbernardtools.com/downloads/marc-bernard-tools/' ##NO_TEXT.
    CONSTANTS c_terms TYPE string VALUE 'https://marcbernardtools.com/company/terms-software/' ##NO_TEXT.
    CONSTANTS c_package TYPE devclass VALUE '/MBTOOLS/BC' ##NO_TEXT.

*   Registry General
    CONSTANTS co_reg_general TYPE string VALUE 'General^' ##NO_TEXT.
    CONSTANTS co_key_name TYPE string VALUE 'Name' ##NO_TEXT.
    CONSTANTS co_key_object TYPE string VALUE 'Object' ##NO_TEXT.
    CONSTANTS co_key_title TYPE string VALUE 'Title' ##NO_TEXT.
    CONSTANTS co_key_description TYPE string VALUE 'Description' ##NO_TEXT.
    CONSTANTS co_key_uri TYPE string VALUE 'URI' ##NO_TEXT.
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
    CLASS-METHODS unregister
      IMPORTING
        VALUE(i_object)       TYPE string
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
        VALUE(i_name)   TYPE string
      RETURNING
        VALUE(r_active) TYPE abap_bool .
    CLASS-METHODS is_licensed
      IMPORTING
        VALUE(i_name)     TYPE string
      RETURNING
        VALUE(r_licensed) TYPE abap_bool .
    CLASS-METHODS license_add
      IMPORTING
        VALUE(i_name)     TYPE string
        VALUE(i_license)  TYPE string
      RETURNING
        VALUE(r_licensed) TYPE abap_bool .
    CLASS-METHODS license_remove
      IMPORTING
        VALUE(i_name)    TYPE string
      RETURNING
        VALUE(r_removed) TYPE abap_bool .
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
*       Get instance of tool
        CREATE OBJECT tool TYPE (i_object).
        IF tool IS BOUND.
          manifest ?= tool.
        ELSE.
          r_activated = abap_false.
          RETURN.
        ENDIF.

*       Is tool already registered?
        reg_tool = reg_root->get_subentry( manifest->descriptor-name ).
        IF NOT reg_tool IS BOUND.
          r_activated = abap_false.
          RETURN.
        ENDIF.

*       Switches
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


  METHOD class_constructor.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

*   Get root of registry
    reg_root = /mbtools/cl_registry=>get_root( ).

  ENDMETHOD.


  METHOD constructor.
    " APACK
    apack_manifest = VALUE #(
      group_id    = 'github.com/mbtools/marc-bernard-tools'
      artifact_id = 'com.marcbernardtools.abap.bc'
      version     = c_version
      git_url     = 'https://github.com/mbtools/marc-bernard-tools.git'
    ) ##NO_TEXT.
    " MBT
    mbt_manifest = VALUE #(
      id          = 1
      name        = c_name
      version     = c_version
      title       = c_title
      description = c_description
      mbt_url     = 'https://marcbernardtools.com/tool/marc-bernard-tools/'
      namespace   = '/MBTOOLS/'
      package     = '/MBTOOLS/BC'
    ) ##NO_TEXT.
  ENDMETHOD.


  METHOD deactivate.

    DATA:
      tool      TYPE REF TO object,
      manifest  TYPE REF TO /mbtools/if_manifest,
      reg_tool  TYPE REF TO /mbtools/cl_registry,
      reg_entry TYPE REF TO /mbtools/cl_registry.

    TRY.
*       Get instance of tool
        CREATE OBJECT tool TYPE (i_object).
        IF tool IS BOUND.
          manifest ?= tool.
        ELSE.
          r_deactivated = abap_false.
          RETURN.
        ENDIF.

*       Is tool already registered?
        reg_tool = reg_root->get_subentry( manifest->descriptor-name ).
        IF NOT reg_tool IS BOUND.
          r_deactivated = abap_false.
          RETURN.
        ENDIF.

*       Switches
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


  METHOD is_active.

    DATA:
      reg_tool  TYPE REF TO /mbtools/cl_registry,
      reg_entry TYPE REF TO /mbtools/cl_registry,
      val       TYPE string.

    TRY.
*       Is tool installed?
        reg_tool = reg_root->get_subentry( i_name ).
        CHECK reg_tool IS BOUND.

*       Switches entry
        reg_entry = reg_tool->get_subentry( co_reg_switches ).
        CHECK reg_entry IS BOUND.

*       Is tool active
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

*   No license required for systems with few users
*   (dialog users who have logged on during evaluation period)
    date_from = sy-datum - co_eval_days.

    SELECT COUNT(*) FROM usr02 INTO user_count
      WHERE ustyp = 'A' AND trdat BETWEEN date_from AND sy-datum
        AND ( bname <> 'DDIC' AND bname NOT LIKE '%SUPPORT%' ).
    IF user_count <= co_eval_users.
      r_licensed = abap_true.
      RETURN.
    ENDIF.

    TRY.
*       Is tool installed?
        reg_tool = reg_root->get_subentry( i_name ).
        CHECK reg_tool IS BOUND.

*       Properties
        reg_entry = reg_tool->get_subentry( co_reg_properties ).
        CHECK reg_entry IS BOUND.

*       No license required during evaluation period
        val = reg_entry->get_value( co_key_install_time ).
        date_from = val(8) + co_eval_days.
        IF date_from >= sy-datum.
          r_licensed = abap_true.
          RETURN.
        ENDIF.

*       License
        reg_entry = reg_tool->get_subentry( co_reg_license ).
        CHECK reg_entry IS BOUND.

*       Is license valid
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
*       Is tool installed?
        reg_tool = reg_root->get_subentry( i_name ).
        CHECK reg_tool IS BOUND.


*       License
        reg_entry = reg_tool->get_subentry( co_reg_license ).
        CHECK reg_entry IS BOUND.

        id = reg_entry->get_value( key = co_key_lic_id ).

*       Is license valid
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
*       Is tool installed?
        reg_tool = reg_root->get_subentry( i_name ).
        CHECK reg_tool IS BOUND.

*       License
        reg_entry = reg_tool->get_subentry( co_reg_license ).
        CHECK reg_entry IS BOUND.

*       Remove license key
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
*       Get instance of tool
        CREATE OBJECT tool TYPE (i_object).
        IF tool IS BOUND.
          manifest ?= tool.
        ELSE.
          r_registered = abap_false.
          RETURN.
        ENDIF.

*       Is tool already registered?
        reg_tool = reg_root->get_subentry( manifest->descriptor-name ).
        IF reg_tool IS BOUND.
          r_registered = abap_true.
        ELSE.
*         Create registry entries
          reg_tool = reg_root->add_subentry( manifest->descriptor-name ).
          CHECK reg_tool IS BOUND.
        ENDIF.

*       General
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
          reg_entry->set_value( key = co_key_uri         value = manifest->descriptor-mbt_url ).
          reg_entry->save( ).
        ENDIF.

*       Properties
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

*         License
          reg_entry = reg_tool->add_subentry( co_reg_license ).
          IF reg_entry IS BOUND.
            reg_entry->set_value( key = co_key_lic_id     value = manifest->descriptor-id ).
            reg_entry->set_value( key = co_key_lic_expire value = '99991231' ).
            reg_entry->set_value( key = co_key_lic_key    value = '' ).
            reg_entry->set_value( key = co_key_lic_valid  value = '' ).
            reg_entry->save( ).
          ENDIF.
        ENDIF.

*       Save
        reg_tool->save( ).

        r_registered = abap_true.

      CATCH cx_root.
        r_registered = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD unregister.

    DATA:
      tool     TYPE REF TO object,
      manifest TYPE REF TO /mbtools/if_manifest,
      reg_tool TYPE REF TO /mbtools/cl_registry.

    TRY.
*       Get instance of tool
        CREATE OBJECT tool TYPE (i_object).
        IF tool IS BOUND.
          manifest ?= tool.
        ELSE.
          r_unregistered = abap_false.
          RETURN.
        ENDIF.

*       Is tool still registered?
        reg_tool = reg_root->get_subentry( manifest->descriptor-name ).
        IF NOT reg_tool IS BOUND.
          r_unregistered = abap_true.
          RETURN.
        ENDIF.

*       Remove registry branch
        reg_root->remove_subentry( manifest->descriptor-name ).
        CHECK reg_tool IS BOUND.

        r_unregistered = abap_true.

      CATCH cx_root.
        r_unregistered = abap_false.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
