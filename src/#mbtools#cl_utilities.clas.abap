************************************************************************
* /MBTOOLS/CL_UTILITIES
* MBT Utilities
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_str_release_patch,
        release TYPE n LENGTH 3,
        version TYPE n LENGTH 5,
        patch   TYPE n LENGTH 5,
      END OF ty_str_release_patch .

    CONSTANTS:
      BEGIN OF c_property,
        year             TYPE string VALUE 'YEAR',
        month            TYPE string VALUE 'MONTH',
        day              TYPE string VALUE 'DAY',
        hour             TYPE string VALUE 'HOUR',
        minute           TYPE string VALUE 'MINUTE',
        second           TYPE string VALUE 'SECOND',
        database_version TYPE string VALUE 'DATABASE_VERSION',
        database_release TYPE string VALUE 'DATABASE_RELEASE',
        database_patch   TYPE string VALUE 'DATABASE_PATCH',
        dbsl_release     TYPE string VALUE 'DBSL_RELEASE',
        dbsl_patch       TYPE string VALUE 'DBSL_PATCH',
        hana_release     TYPE string VALUE 'HANA_RELEASE',
        hana_sp          TYPE string VALUE 'HANA_SP',
        hana_revision    TYPE string VALUE 'HANA_REVISION',
        hana_patch       TYPE string VALUE 'HANA_PATCH',
        spam_release     TYPE string VALUE 'SPAM_RELEASE',
        spam_version     TYPE string VALUE 'SPAM_VERSION',
        kernel_release   TYPE string VALUE 'KERNEL_RELEASE',
        kernel_patch     TYPE string VALUE 'KERNEL_PATCH',
        kernel_bits      TYPE string VALUE 'KERNEL_BITS',
        unicode          TYPE string VALUE 'UNICODE',
      END OF c_property .
    CONSTANTS c_unknown TYPE string VALUE 'UNKNOWN' ##NO_TEXT.

    CLASS-METHODS call_browser
      IMPORTING
        !iv_url TYPE csequence .
    CLASS-METHODS is_batch
      RETURNING
        VALUE(rv_batch) TYPE abap_bool .
    CLASS-METHODS is_system_modifiable
      RETURNING
        VALUE(rv_modifiable) TYPE abap_bool .
    CLASS-METHODS is_system_test_or_prod
      RETURNING
        VALUE(rv_test_prod) TYPE abap_bool .
    CLASS-METHODS is_snote_allowed
      RETURNING
        VALUE(rv_snote_allowed) TYPE abap_bool .
    CLASS-METHODS is_upgrage_running
      RETURNING
        VALUE(rv_upgrade_running) TYPE abap_bool .
    CLASS-METHODS is_spam_locked
      RETURNING
        VALUE(rv_spam_locked) TYPE abap_bool .
    CLASS-METHODS get_property
      IMPORTING
        VALUE(i_property) TYPE clike
      EXPORTING
        !e_value          TYPE string
        !e_value_float    TYPE f
        !e_value_integer  TYPE i
        !e_subrc          TYPE sy-subrc .
    CLASS-METHODS get_syst_field
      IMPORTING
        VALUE(i_field) TYPE clike
      RETURNING
        VALUE(r_value) TYPE string .
    CLASS-METHODS get_db_release
      EXPORTING
        !es_dbinfo       TYPE dbrelinfo
        !es_hana_release TYPE ty_str_release_patch .
    CLASS-METHODS get_spam_release
      RETURNING
        VALUE(rs_details) TYPE ty_str_release_patch .
    CLASS-METHODS get_kernel_release
      RETURNING
        VALUE(rs_details) TYPE ty_str_release_patch .
    CLASS-METHODS get_swcomp_release
      IMPORTING
        VALUE(i_component) TYPE clike
      RETURNING
        VALUE(r_release)   TYPE string .
    CLASS-METHODS get_swcomp_support_package
      IMPORTING
        VALUE(i_component)       TYPE clike
      RETURNING
        VALUE(r_support_package) TYPE string .
    CLASS-METHODS get_profile_parameter
      IMPORTING
        VALUE(i_parameter) TYPE clike
      RETURNING
        VALUE(r_value)     TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA:
      mt_cvers TYPE SORTED TABLE OF cvers WITH UNIQUE KEY component .
ENDCLASS.



CLASS /MBTOOLS/CL_UTILITIES IMPLEMENTATION.


  METHOD call_browser.

    DATA:
      lv_url_c TYPE c LENGTH 500.

    CHECK NOT iv_url IS INITIAL.

    lv_url_c = iv_url.

    CALL FUNCTION 'CALL_BROWSER'
      EXPORTING
        url                    = lv_url_c
        new_window             = abap_true
      EXCEPTIONS
        frontend_not_supported = 1
        frontend_error         = 2
        prog_not_found         = 3
        no_batch               = 4
        unspecified_error      = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE 'S'.
    ENDIF.

  ENDMETHOD.                    "call_browser


  METHOD get_db_release.

    DATA:
      l_release  TYPE n LENGTH 3,
      l_text_1   TYPE string,
      l_text_2   TYPE string,
      l_text_3   TYPE string,
      l_hana_rel TYPE i,
      l_hana_sps TYPE i.

    CLEAR: es_dbinfo, es_hana_release.

    CALL FUNCTION 'DB_DBRELINFO'
      IMPORTING
        dbinfo = es_dbinfo.

    IF es_dbinfo-dbsys = 'HDB'.
*     First number in version is release, third one is revision level
      FIND FIRST OCCURRENCE OF REGEX '(\d+)\.(\d+)\.(\d+)\.(\d*)\.\d*' IN es_dbinfo-srvrel
        SUBMATCHES l_text_1 l_text_2 es_hana_release-version es_hana_release-patch.
      IF sy-subrc = 0.
        l_hana_rel = l_text_1.
        l_hana_sps = l_text_2. "= 0 (except SAP-internally)

        CASE l_hana_rel.
          WHEN 1.
            IF es_hana_release-version = 0.
              l_hana_sps = 0.
            ELSEIF es_hana_release-version BETWEEN 1 AND 10.
              l_hana_sps = 1.
            ELSEIF es_hana_release-version BETWEEN 11 AND 18.
              l_hana_sps = 2.
            ELSEIF es_hana_release-version BETWEEN 19 AND 27.
              l_hana_sps = 3.
            ELSEIF es_hana_release-version BETWEEN 28 AND 44.
              l_hana_sps = 4.
            ELSEIF es_hana_release-version BETWEEN 45 AND 59.
              l_hana_sps = 5.
            ELSE.
              l_hana_sps = es_hana_release-version DIV 10.
            ENDIF.
          WHEN OTHERS.
            l_hana_sps = es_hana_release-version DIV 10.
        ENDCASE.

        l_release = 100 * l_hana_rel + l_hana_sps.
        es_hana_release-release = l_release.
        IF es_hana_release-patch > 1000. "it s the changelog for old revisions
          es_hana_release-patch = 0.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "get_db_release


  METHOD get_kernel_release.

*   Kernel Info retrival copied from FuGrp SHSY Module get_kinfo
    TYPES:
      BEGIN OF lty_kernel_info,
        key  TYPE c LENGTH 21,
        data TYPE c LENGTH 400,
      END OF lty_kernel_info.

    DATA:
      lt_kernel_info TYPE STANDARD TABLE OF lty_kernel_info,
      lr_kernel_info TYPE REF TO            lty_kernel_info.

*   Kernel Release Information
    CALL 'SAPCORE' ID 'ID' FIELD 'VERSION'
                   ID 'TABLE' FIELD lt_kernel_info.       "#EC CI_CCALL

    READ TABLE lt_kernel_info REFERENCE INTO lr_kernel_info INDEX 12.
    rs_details-release = lr_kernel_info->data.

    READ TABLE lt_kernel_info REFERENCE INTO lr_kernel_info INDEX 15.
    rs_details-patch = lr_kernel_info->data.

*   32- or 64-bit Kernel
    READ TABLE lt_kernel_info REFERENCE INTO lr_kernel_info INDEX 3.
    IF lr_kernel_info->data CS '64'.
      rs_details-version = 64.
    ELSE.
      rs_details-version = 32.
    ENDIF.

  ENDMETHOD.                    "get_kernel_release


  METHOD get_profile_parameter.

    DATA:
      lt_parameters TYPE spfl_parameter_list_t,
      l_subrc       TYPE sy-subrc.

    FIELD-SYMBOLS:
      <ls_parameter> TYPE spfl_parameter_list.

    CALL METHOD cl_spfl_profile_parameter=>get_all_parameter
      IMPORTING
        parameter_sub = lt_parameters.

    LOOP AT lt_parameters ASSIGNING <ls_parameter>.
      TRANSLATE <ls_parameter>-name TO UPPER CASE.
    ENDLOOP.

    SORT lt_parameters BY name.

    READ TABLE lt_parameters ASSIGNING <ls_parameter>
      WITH KEY name = i_parameter BINARY SEARCH.
    IF sy-subrc = 0.
      IF NOT <ls_parameter>-user_value IS INITIAL.
        r_value = <ls_parameter>-user_value.
      ELSE.
        r_value = <ls_parameter>-default_value.
      ENDIF.
    ELSE.
      r_value = c_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD get_property.

    DATA:
      l_property        TYPE string,
      l_version         TYPE string,
      ls_dbinfo         TYPE dbrelinfo,
      ls_hana_release   TYPE ty_str_release_patch,
      ls_kernel_release TYPE ty_str_release_patch,
      ls_spam_release   TYPE ty_str_release_patch,
      lr_db_con         TYPE REF TO cl_dba_dbcon.

    CLEAR: e_value, e_value_float, e_value_integer, e_subrc.

    l_property = i_property.
    TRANSLATE l_property TO UPPER CASE.

    TRY.
        CASE l_property.
          WHEN c_property-year.
            e_value = sy-datum+0(4).
          WHEN c_property-month.
            e_value = sy-datum+4(2).
          WHEN c_property-day.
            e_value = sy-datum+6(2).
          WHEN c_property-hour.
            e_value = sy-uzeit+0(2).
          WHEN c_property-minute.
            e_value = sy-uzeit+2(2).
          WHEN c_property-second.
            e_value = sy-uzeit+4(2).
          WHEN c_property-database_version OR c_property-database_release OR c_property-database_patch
            OR c_property-dbsl_release OR c_property-dbsl_patch.
            CALL METHOD get_db_release
              IMPORTING
                es_dbinfo = ls_dbinfo.
            IF l_property = c_property-database_version.
              l_version = ls_dbinfo-srvrel.
            ELSEIF l_property = c_property-database_release.
              FIND FIRST OCCURRENCE OF REGEX '(\d+)\.\d+\.*' IN ls_dbinfo-srvrel
                SUBMATCHES l_version.
            ELSEIF l_property = c_property-database_patch.
              FIND FIRST OCCURRENCE OF REGEX '\d+\.(\d+)\.*' IN ls_dbinfo-srvrel
                SUBMATCHES l_version.
            ELSEIF l_property = c_property-dbsl_release.
              SPLIT ls_dbinfo-dbsl_vers AT '.' INTO l_version sy-lisel.
            ELSE.
              SPLIT ls_dbinfo-dbsl_vers AT '.' INTO sy-lisel l_version.
            ENDIF.
            IF sy-subrc = 0.
              e_value = l_version.
            ELSE.
              e_subrc = 2.
            ENDIF.
          WHEN c_property-hana_release OR c_property-hana_sp OR c_property-hana_revision OR c_property-hana_patch.
            CALL METHOD get_db_release
              IMPORTING
                es_hana_release = ls_hana_release.
            IF l_property = c_property-hana_release.
              e_value = ls_hana_release-release DIV 100.
            ELSEIF l_property = c_property-hana_sp.
              e_value = ls_hana_release-release MOD 100.
            ELSEIF l_property = c_property-hana_revision.
              e_value = ls_hana_release-version.
            ELSE.
              e_value = ls_hana_release-patch.
            ENDIF.
          WHEN c_property-spam_release OR c_property-spam_version.
            ls_spam_release = get_spam_release( ).
            IF l_property = c_property-spam_release.
              e_value = ls_spam_release-release.
            ELSE.
              e_value = ls_spam_release-version.
            ENDIF.
          WHEN c_property-kernel_release OR c_property-kernel_patch OR c_property-kernel_bits.
            ls_kernel_release = get_kernel_release( ).
            IF l_property = c_property-kernel_release.
              e_value = ls_kernel_release-release.
            ELSEIF l_property = c_property-kernel_patch.
              e_value = ls_kernel_release-patch.
            ELSE.
              e_value = ls_kernel_release-version.
            ENDIF.
          WHEN c_property-unicode.
            IF cl_abap_char_utilities=>charsize = 1.
              e_value = 0.
            ELSE.
              e_value = 1.
            ENDIF.
          WHEN OTHERS.
            e_value = get_syst_field( l_property ).

            IF e_value = c_unknown.
              e_value = get_swcomp_release( l_property ).
            ENDIF.

            IF e_value = c_unknown.
              e_value = get_swcomp_support_package( l_property ).
            ENDIF.

            IF e_value = c_unknown.
              e_value = get_profile_parameter( l_property ).
            ENDIF.

            IF e_value = c_unknown.
              e_subrc = 4.
            ENDIF.
        ENDCASE.
      CATCH cx_root.
        e_subrc = 8.
    ENDTRY.

    IF e_subrc = 0.
      SHIFT e_value LEFT DELETING LEADING space.
      TRY.
          e_value_integer = e_value.
          e_value_float = e_value.
        CATCH cx_root.
          "not a numeric value, just ignore
          e_subrc = 0.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD get_spam_release.

    CONSTANTS:
      lc_spam_vers_func TYPE funcname VALUE 'SPAM_VERSION'.

    DATA:
      lv_spam_vers TYPE n LENGTH 4.

    TRY.
        CALL FUNCTION lc_spam_vers_func
          IMPORTING
            version = lv_spam_vers.

        rs_details-release = sy-saprl.                    "#EC SAPRL_OK
        rs_details-version = lv_spam_vers.
      CATCH cx_sy_dyn_call_illegal_func.
    ENDTRY.

  ENDMETHOD.                    "get_spam_release


  METHOD get_swcomp_release.

    DATA ls_cvers TYPE cvers.

    IF mt_cvers IS INITIAL.
      SELECT * FROM cvers INTO TABLE mt_cvers.
    ENDIF.

    READ TABLE mt_cvers INTO ls_cvers WITH TABLE KEY
      component = i_component.
    IF sy-subrc = 0.
      r_release = ls_cvers-release.
    ELSE.
      r_release = c_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD get_swcomp_support_package.

    DATA ls_cvers TYPE cvers.

    IF mt_cvers IS INITIAL.
      SELECT * FROM cvers INTO TABLE mt_cvers.
    ENDIF.

    ls_cvers-component = i_component.
    REPLACE '_SP' IN ls_cvers-component WITH ''.

    READ TABLE mt_cvers INTO ls_cvers WITH TABLE KEY
      component = ls_cvers-component.
    IF sy-subrc = 0.
      r_support_package = ls_cvers-extrelease.
    ELSE.
      r_support_package = c_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD get_syst_field.

    DATA lv_field TYPE fieldname.

    FIELD-SYMBOLS <value> TYPE any.

    lv_field = i_field.

    TRANSLATE lv_field TO UPPER CASE.

    IF lv_field CS 'SY-' OR lv_field CS 'SYST-'.
      REPLACE 'SY-' WITH '' INTO lv_field.
      REPLACE 'SYST-' WITH '' INTO lv_field.
      CONDENSE lv_field NO-GAPS.

      ASSIGN COMPONENT lv_field OF STRUCTURE syst TO <value>.
      IF sy-subrc = 0.
        TRY.
            r_value = <value>.
          CATCH cx_root.
            r_value = c_unknown.
        ENDTRY.
      ELSE.
        r_value = c_unknown.
      ENDIF.
    ELSE.
      r_value = c_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD is_batch.

    IF sy-binpt = abap_true OR sy-batch = abap_true.
      rv_batch = abap_true.
    ELSE.
      rv_batch = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_snote_allowed.

    CALL FUNCTION 'OCS_CHECK_RUNNING_UPGRADE_4_NA'
      IMPORTING
        ev_snote_allowed = rv_snote_allowed.

  ENDMETHOD.                    "is_spam_in_progress


  METHOD is_spam_locked.

    DATA:
      ls_sema TYPE pat10.

    CALL FUNCTION 'OCS_QUEUE_SEMAPHORE'
      EXPORTING
        iv_tool        = 'SPAM'
        iv_read_only   = abap_true
      IMPORTING
        ev_locked      = rv_spam_locked
      CHANGING
        cs_sema        = ls_sema
      EXCEPTIONS
        foreign_lock   = 1
        internal_error = 2
        OTHERS         = 3.
    CHECK sy-subrc = 0. "ignore errors

  ENDMETHOD.                    "is_spam_in_progress


  METHOD is_system_modifiable.

    DATA:
      l_systemedit TYPE tadir-edtflag.

    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
        systemedit    = l_systemedit
      EXCEPTIONS
        no_systemname = 1
        no_systemtype = 2
        OTHERS        = 3.
    IF sy-subrc <> 0 OR l_systemedit = 'N'. "not modifiable
      rv_modifiable = abap_false.
    ELSE.
      rv_modifiable = abap_true.
    ENDIF.

  ENDMETHOD.                    "is_system_modifiable


  METHOD is_system_test_or_prod.

    DATA:
      l_client_role TYPE cccategory.

    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
        system_client_role = l_client_role
      EXCEPTIONS
        no_systemname      = 1
        no_systemtype      = 2
        OTHERS             = 3.
    IF sy-subrc <> 0 OR l_client_role CA 'PTS'. "prod/test/sap reference
      rv_test_prod = abap_true.
    ELSE.
      rv_test_prod = abap_false.
    ENDIF.

  ENDMETHOD.                    "is_system_test_or_prod


  METHOD is_upgrage_running.

    CALL FUNCTION 'OCS_CHECK_RUNNING_UPGRADE_4_NA'
      IMPORTING
        ev_upg_running = rv_upgrade_running.

  ENDMETHOD.                    "is_spam_in_progress
ENDCLASS.
