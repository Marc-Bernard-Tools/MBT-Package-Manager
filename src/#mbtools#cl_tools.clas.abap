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

    " Global Constant
    CONSTANTS c_github TYPE string VALUE 'github.com/mbtools' ##NO_TEXT.
    CONSTANTS c_home TYPE string VALUE 'https://marcbernardtools.com/' ##NO_TEXT.
    CONSTANTS c_terms TYPE string VALUE 'https://marcbernardtools.com/company/terms-software/' ##NO_TEXT.
    CONSTANTS c_namespace TYPE devclass VALUE '/MBTOOLS/' ##NO_TEXT.
    CONSTANTS c_manifest TYPE seoclsname VALUE '/MBTOOLS/IF_MANIFEST' ##NO_TEXT.
    " Registry General
    CONSTANTS co_reg_general TYPE string VALUE 'General^' ##NO_TEXT.
    CONSTANTS co_key_name TYPE string VALUE 'Name' ##NO_TEXT.
    CONSTANTS co_key_class TYPE string VALUE 'Class' ##NO_TEXT.
    CONSTANTS co_key_title TYPE string VALUE 'Title' ##NO_TEXT.
    CONSTANTS co_key_description TYPE string VALUE 'Description' ##NO_TEXT.
    CONSTANTS co_key_version TYPE string VALUE 'Version' ##NO_TEXT.
    CONSTANTS co_key_namespace TYPE string VALUE 'Namespace' ##NO_TEXT.
    CONSTANTS co_key_package TYPE string VALUE 'Package' ##NO_TEXT.
    " Registry Properties
    CONSTANTS co_reg_properties TYPE string VALUE 'Properties^' ##NO_TEXT.
    CONSTANTS co_key_install_time TYPE string VALUE 'InstallTimestamp' ##NO_TEXT.
    CONSTANTS co_key_install_user TYPE string VALUE 'InstallUser' ##NO_TEXT.
    CONSTANTS co_key_uninstall_time TYPE string VALUE 'UninstallTimestamp' ##NO_TEXT.
    CONSTANTS co_key_uninstall_user TYPE string VALUE 'UninstallUser' ##NO_TEXT.
    CONSTANTS co_key_update_time TYPE string VALUE 'UpdateTimestamp' ##NO_TEXT.
    CONSTANTS co_key_update_user TYPE string VALUE 'UpdateUser' ##NO_TEXT.
    " Registry Switches
    CONSTANTS co_reg_switches TYPE string VALUE 'Switches' ##NO_TEXT.
    CONSTANTS co_key_active TYPE string VALUE 'Active' ##NO_TEXT.
    CONSTANTS co_key_debug TYPE string VALUE 'Debug' ##NO_TEXT.
    CONSTANTS co_key_trace TYPE string VALUE 'Trace' ##NO_TEXT.
    " Registry License
    CONSTANTS co_reg_license TYPE string VALUE 'License^' ##NO_TEXT.
    CONSTANTS co_key_lic_id TYPE string VALUE 'ID' ##NO_TEXT.
    CONSTANTS co_key_lic_key TYPE string VALUE 'LicenseKey' ##NO_TEXT.
    CONSTANTS co_key_lic_valid TYPE string VALUE 'LicenseValid' ##NO_TEXT.
    CONSTANTS co_key_lic_expire TYPE string VALUE 'LicenseExpiration' ##NO_TEXT.
    " Evaluation
    CONSTANTS co_eval_days TYPE i VALUE 30 ##NO_TEXT.
    CONSTANTS co_eval_users TYPE i VALUE 10 ##NO_TEXT.

    DATA apack_manifest TYPE /mbtools/if_apack_manifest=>ty_descriptor READ-ONLY.
    DATA mbt_manifest TYPE /mbtools/if_manifest=>ty_descriptor READ-ONLY.

    " Constructor
    CLASS-METHODS class_constructor .
    METHODS constructor
      IMPORTING
        !i_tool TYPE REF TO object .
    " Class Get
    CLASS-METHODS get_tool
      IMPORTING
        VALUE(i_title) TYPE csequence
      RETURNING
        VALUE(r_tool)  TYPE REF TO /mbtools/cl_tools .
    CLASS-METHODS get_tools
      IMPORTING
        VALUE(i_pattern) TYPE csequence OPTIONAL
      RETURNING
        VALUE(r_tools)   TYPE /mbtools/tools_with_text .
    CLASS-METHODS f4_tools
      IMPORTING
        VALUE(i_pattern) TYPE csequence OPTIONAL
      RETURNING
        VALUE(r_title)   TYPE string .
    " Class Regiser/Unregister
    CLASS-METHODS register_all
      RETURNING
        VALUE(r_registered) TYPE abap_bool .
    CLASS-METHODS unregister_all
      RETURNING
        VALUE(r_unregistered) TYPE abap_bool .
    " Class Manifests
    CLASS-METHODS get_manifests
      RETURNING
        VALUE(r_manifests) TYPE /mbtools/manifests .
    " Tool Manifest
    METHODS build_apack_manifest
      RETURNING
        VALUE(r_manifest) TYPE /mbtools/if_apack_manifest=>ty_descriptor .
    METHODS build_mbt_manifest
      RETURNING
        VALUE(r_manifest) TYPE /mbtools/if_manifest=>ty_descriptor .
    " Tool Register/Unregister
    METHODS register
      RETURNING
        VALUE(r_registered) TYPE abap_bool .
    METHODS unregister
      RETURNING
        VALUE(r_unregistered) TYPE abap_bool .
    " Tool Activate/Deactivate
    METHODS activate
      RETURNING
        VALUE(r_activated) TYPE abap_bool .
    METHODS deactivate
      RETURNING
        VALUE(r_deactivated) TYPE abap_bool .
    METHODS is_active
      RETURNING
        VALUE(r_active) TYPE abap_bool .
    " Tool License
    METHODS is_licensed
      RETURNING
        VALUE(r_licensed) TYPE abap_bool .
    METHODS license_add
      IMPORTING
        VALUE(i_license)  TYPE string
      RETURNING
        VALUE(r_licensed) TYPE abap_bool .
    METHODS license_remove
      RETURNING
        VALUE(r_removed) TYPE abap_bool .
    " Tool Get
    METHODS get_id
      RETURNING
        VALUE(r_id) TYPE string .
    METHODS get_slug
      RETURNING
        VALUE(r_slug) TYPE string .
    METHODS get_name
      RETURNING
        VALUE(r_name) TYPE string .
    METHODS get_title
      RETURNING
        VALUE(r_title) TYPE string .
    METHODS get_version
      RETURNING
        VALUE(r_version) TYPE string .
    METHODS get_description
      RETURNING
        VALUE(r_description) TYPE string .
    METHODS get_class
      RETURNING
        VALUE(r_class) TYPE string .
    METHODS get_package
      RETURNING
        VALUE(r_package) TYPE devclass .
    METHODS get_url_repo
      RETURNING
        VALUE(r_url) TYPE string .
    METHODS get_url_tool
      RETURNING
        VALUE(r_url) TYPE string .
    METHODS get_url_docs
      RETURNING
        VALUE(r_url) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_classes TYPE STANDARD TABLE OF seoclsname WITH DEFAULT KEY .

    CLASS-DATA reg_root TYPE REF TO /mbtools/cl_registry .
    DATA mr_tool TYPE REF TO object .
    DATA m_id TYPE /mbtools/if_manifest=>ty_descriptor-name .
    DATA m_title TYPE /mbtools/if_manifest=>ty_descriptor-title .
    DATA m_name TYPE /mbtools/if_manifest=>ty_descriptor-name .
    DATA m_version TYPE /mbtools/if_manifest=>ty_descriptor-version .
    DATA m_description TYPE /mbtools/if_manifest=>ty_descriptor-description .


    CLASS-METHODS get_implementations
      IMPORTING
        VALUE(i_quiet)    TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_classes) TYPE ty_classes .
ENDCLASS.



CLASS /MBTOOLS/CL_TOOLS IMPLEMENTATION.


  METHOD activate.

    DATA:
      reg_tool  TYPE REF TO /mbtools/cl_registry,
      reg_entry TYPE REF TO /mbtools/cl_registry.

    TRY.
        " Is tool already registered?
        reg_tool = reg_root->get_subentry( m_name ).
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

    r_manifest-group_id       = c_github.
    r_manifest-artifact_id    = m_name.
    r_manifest-version        = m_version.
    r_manifest-git_url        = get_url_repo( ).
*   r_manifest-target_package = get_package( ).

  ENDMETHOD.


  METHOD build_mbt_manifest.

    r_manifest-id          = m_id.
    r_manifest-name        = m_name.
    r_manifest-version     = m_version.
    r_manifest-title       = m_title.
    r_manifest-description = m_description.
    r_manifest-namespace   = c_namespace.
    r_manifest-package     = get_package( ).
    r_manifest-class       = get_class( ).

  ENDMETHOD.


  METHOD class_constructor.

    LOG-POINT ID /mbtools/bc SUBKEY /mbtools/cl_base=>c_title FIELDS sy-datum sy-uzeit sy-uname.

    " APACK interface
    /mbtools/cl_apack_manifest=>run( ).

    " Get root of registry
    reg_root = /mbtools/cl_registry=>get_root( ).

  ENDMETHOD.


  METHOD constructor.

    FIELD-SYMBOLS:
      <id>          TYPE i,
      <title>       TYPE string,
      <version>     TYPE string,
      <description> TYPE string.

    mr_tool = i_tool.

    " Each tool class must include four constants
    ASSIGN mr_tool->('C_DOWNLOAD_ID') TO <id>.
    ASSERT sy-subrc = 0. " constant is missing
    m_id = <id>.

    ASSIGN mr_tool->('C_TITLE') TO <title>.
    ASSERT sy-subrc = 0. " constant is missing
    m_title = <title>.
    m_name = get_name( ).

    ASSIGN mr_tool->('C_VERSION') TO <version>.
    ASSERT sy-subrc = 0. " constant is missing
    m_version = <version>.

    ASSIGN mr_tool->('C_DESCRIPTION') TO <description>.
    ASSERT sy-subrc = 0. " constant is missing
    m_description = <description>.

    " Build the full manifests based on these constants
    apack_manifest = build_apack_manifest( ).
    mbt_manifest   = build_mbt_manifest( ).

  ENDMETHOD.


  METHOD deactivate.

    DATA:
      reg_tool  TYPE REF TO /mbtools/cl_registry,
      reg_entry TYPE REF TO /mbtools/cl_registry.

    TRY.
        " Is tool already registered?
        reg_tool = reg_root->get_subentry( m_name ).
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
      r_title = return-fieldval.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_class.

    DATA: class_desc TYPE REF TO cl_abap_typedescr.

    class_desc = cl_abap_classdescr=>describe_by_object_ref( mr_tool ).
    r_class = class_desc->get_relative_name( ).

  ENDMETHOD.


  METHOD get_description.

    r_description = m_description.

  ENDMETHOD.


  METHOD get_id.

    " Upper case, Underscore, Namespaced
    r_id =  to_upper( c_namespace && m_title ).

    REPLACE ALL OCCURRENCES OF ` ` IN r_id WITH '_'.

  ENDMETHOD.


  METHOD get_implementations.

    " Get all classes that implement the MBT Manifest (except for the MBT Tool Manager)
    SELECT clsname FROM seometarel INTO TABLE rt_classes
      WHERE version    = '1'
        AND refclsname = c_manifest.
    IF sy-subrc <> 0 AND i_quiet IS INITIAL.
      MESSAGE s000(/mbtools/bc) WITH 'No tools found'.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD get_manifests.

    DATA:
      implementation TYPE seoclsname,
      tool           TYPE REF TO object,
      manifest       TYPE REF TO /mbtools/if_manifest,
      manifest_descr TYPE /mbtools/manifest.

    LOOP AT get_implementations( ) INTO implementation.

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

    " Mixed case, underscore
    r_name = m_title.

    REPLACE ALL OCCURRENCES OF ` ` IN r_name WITH '_'.

    " We want the base class to be first in the registry
    " 'Marc_Bernard_Tools' would be after 'MBT Tool XYZ' in the sort order but isn't
    IF m_title = /mbtools/cl_base=>c_title.
      r_name = to_upper( r_name ).
    ENDIF.

  ENDMETHOD.


  METHOD get_package.

    DATA: class TYPE string.

    class = get_class( ).

    SELECT SINGLE devclass FROM tadir INTO r_package
      WHERE pgmid = 'R3TR' AND object = 'CLAS' AND obj_name = class.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD get_slug.

    " Lower case, dash
    r_slug = to_lower( m_title ).

    REPLACE ALL OCCURRENCES OF ` ` IN r_slug WITH '-'.

  ENDMETHOD.


  METHOD get_title.

    r_title = m_title.

  ENDMETHOD.


  METHOD get_tool.

    DATA:
      implementation TYPE seoclsname,
      tool           TYPE REF TO object,
      manifest       TYPE REF TO /mbtools/if_manifest.

    LOOP AT get_implementations( ) INTO implementation.

      TRY.
          " Get instance of tool
          CREATE OBJECT tool TYPE (implementation).
          IF tool IS BOUND.
            manifest ?= tool.
          ELSE.
            CONTINUE. "ignore
          ENDIF.

          IF manifest->descriptor-title = i_title.
            CREATE OBJECT r_tool EXPORTING i_tool = tool.
            RETURN.
          ENDIF.

        CATCH cx_root.
          CONTINUE. "ignore
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_tools.

    DATA:
      implementation TYPE seoclsname,
      tool           TYPE REF TO object,
      manifest       TYPE REF TO /mbtools/if_manifest,
      tool_with_text TYPE /mbtools/tool_with_text.

    FIELD-SYMBOLS:
      <tool> TYPE /mbtools/tool_with_text.

    LOOP AT get_implementations( ) INTO implementation.

      TRY.
          " Get instance of tool
          CREATE OBJECT tool TYPE (implementation).
          IF tool IS BOUND.
            manifest ?= tool.
          ELSE.
            CONTINUE. "ignore
          ENDIF.

          CLEAR tool_with_text.

          " Change base tool so it will be first in sort order
          IF manifest->descriptor-title = /mbtools/cl_base=>c_title.
            tool_with_text-name = to_upper( manifest->descriptor-title ).
          ELSE.
            tool_with_text-name = manifest->descriptor-title.
          ENDIF.

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

    " Change name of base tool back to what it was
    READ TABLE r_tools ASSIGNING <tool> INDEX 1.
    IF sy-subrc = 0.
      <tool>-name = /mbtools/cl_base=>c_title.
    ENDIF.

  ENDMETHOD.


  METHOD get_url_docs.

    " Link to documentation page on marcbernardtools.com
    r_url = c_home && 'docs/' && get_slug( ) && '/'.

  ENDMETHOD.


  METHOD get_url_repo.

    " Link to repository on GitHub.com
    r_url = 'https://' && c_github && '/' && m_name && '.git'.

  ENDMETHOD.


  METHOD get_url_tool.

    " Link to tool page on marcbernardtools.com
    r_url = c_home && 'downloads/' && get_slug( ) && '/'.

  ENDMETHOD.


  METHOD get_version.

    r_version = m_version.

  ENDMETHOD.


  METHOD is_active.

    DATA:
      reg_tool  TYPE REF TO /mbtools/cl_registry,
      reg_entry TYPE REF TO /mbtools/cl_registry,
      val       TYPE string.

    TRY.
        " Is tool installed?
        reg_tool = reg_root->get_subentry( m_name ).
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
        reg_tool = reg_root->get_subentry( m_name ).
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
        reg_tool = reg_root->get_subentry( m_name ).
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
        reg_tool = reg_root->get_subentry( m_name ).
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
      reg_tool  TYPE REF TO /mbtools/cl_registry,
      reg_entry TYPE REF TO /mbtools/cl_registry,
      timestamp TYPE timestamp.

    TRY.
        " Is tool already registered?
        reg_tool = reg_root->get_subentry( m_name ).
        IF reg_tool IS BOUND.
          r_registered = abap_true.
        ELSE.
          " Create registry entries
          reg_tool = reg_root->add_subentry( m_name ).
          CHECK reg_tool IS BOUND.
        ENDIF.

        " General
        IF r_registered = abap_true.
          reg_entry = reg_tool->get_subentry( co_reg_general ).
        ELSE.
          reg_entry = reg_tool->add_subentry( co_reg_general ).
        ENDIF.
        IF reg_entry IS BOUND.
          reg_entry->set_value( key = co_key_name        value = m_name ).
          reg_entry->set_value( key = co_key_version     value = m_version ).
          reg_entry->set_value( key = co_key_title       value = m_title ).
          reg_entry->set_value( key = co_key_description value = m_description ).
          reg_entry->set_value( key = co_key_namespace   value = c_namespace ).
          reg_entry->set_value( key = co_key_package     value = get_package( ) ).
          reg_entry->set_value( key = co_key_class       value = get_class( ) ).
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
            reg_entry->set_value( key = co_key_lic_id     value = m_id ).
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
      tool       TYPE /mbtools/tool_with_text,
      tools      TYPE TABLE OF /mbtools/tool_with_text,
      registered TYPE abap_bool.

    tools = get_tools( ).

    r_registered = abap_true.

    LOOP AT tools INTO tool.
      registered = get_tool( tool-name )->register( ).
      IF registered = abap_false.
        r_registered = abap_false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD unregister.

    DATA:
      reg_tool TYPE REF TO /mbtools/cl_registry.

    TRY.
        " Is tool still registered?
        reg_tool = reg_root->get_subentry( m_name ).
        IF NOT reg_tool IS BOUND.
          r_unregistered = abap_true.
          RETURN.
        ENDIF.

        " Remove registry branch
        reg_root->remove_subentry( m_name ).
        CHECK reg_tool IS BOUND.

        r_unregistered = abap_true.

      CATCH cx_root.
        r_unregistered = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD unregister_all.

    DATA:
      tool         TYPE /mbtools/tool_with_text,
      tools        TYPE TABLE OF /mbtools/tool_with_text,
      unregistered TYPE abap_bool.

    tools = get_tools( ).

    r_unregistered = abap_true.

    LOOP AT tools INTO tool.
      unregistered = get_tool( tool-name )->unregister( ).
      IF unregistered = abap_false.
        r_unregistered = abap_false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
