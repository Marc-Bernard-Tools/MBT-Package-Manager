CLASS /mbtools/cl_sap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.


************************************************************************
* MBT SAP
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************
  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_pgmid,
        head  TYPE pgmid VALUE 'HEAD',
        r3tr  TYPE pgmid VALUE 'R3TR',
        limu  TYPE pgmid VALUE 'LIMU',
        basis TYPE pgmid VALUE 'ZZ01',
      END OF c_pgmid.

    TYPES:
      BEGIN OF ty_domain_value,
        domvalue_l TYPE domvalue_l,
        valpos     TYPE valpos,
        appval     TYPE ddappval,
        ddtext     TYPE val_text,
      END OF ty_domain_value,
      ty_domain_values TYPE STANDARD TABLE OF ty_domain_value WITH DEFAULT KEY.

    CLASS-METHODS class_constructor.

    CLASS-METHODS get_object_wo_namespace
      IMPORTING
        !iv_obj_name     TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE /mbtools/if_definitions=>ty_name.

    CLASS-METHODS get_namespace
      IMPORTING
        !iv_obj_name     TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE namespace.

    CLASS-METHODS get_object_text
      IMPORTING
        VALUE(iv_object) TYPE csequence
      RETURNING
        VALUE(rv_text)   TYPE ddtext.

    CLASS-METHODS get_object_texts
      RETURNING
        VALUE(rt_object_texts) TYPE /mbtools/if_definitions=>ty_object_texts.

    CLASS-METHODS get_text_from_domain
      IMPORTING
        !iv_domain     TYPE any DEFAULT 'YESNO'
        !iv_value      TYPE any
      EXPORTING
        VALUE(ev_text) TYPE clike.

    CLASS-METHODS get_values_from_domain
      IMPORTING
        !iv_domain       TYPE any
      RETURNING
        VALUE(rt_values) TYPE ty_domain_values.

    CLASS-METHODS is_devc_deleted
      IMPORTING
        !iv_obj_name     TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_fugr_deleted
      IMPORTING
        !iv_obj_name     TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_prog_deleted
      IMPORTING
        !iv_obj_name     TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_sap_note
      IMPORTING
        !iv_input        TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_tobj_deleted
      IMPORTING
        !iv_obj_name     TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS object_name_check
      IMPORTING
        !iv_input        TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE string.

    CLASS-METHODS show_icon
      IMPORTING
        !iv_icon       TYPE csequence
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

    CLASS-METHODS show_object
      IMPORTING
        !iv_pgmid        TYPE csequence DEFAULT c_pgmid-r3tr
        !iv_object       TYPE csequence
        !iv_obj_name     TYPE csequence
        !iv_line_number  TYPE i OPTIONAL
        !iv_sub_obj_name TYPE csequence OPTIONAL
      RETURNING
        VALUE(rv_exit)   TYPE abap_bool.

    CLASS-METHODS run_transaction
      IMPORTING
        !iv_tcode      TYPE csequence
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

    CLASS-METHODS run_program
      IMPORTING
        !iv_program    TYPE csequence
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.
  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS:
      c_note_min TYPE cwbntnumm VALUE '1',
      c_note_max TYPE cwbntnumm VALUE '4999999'
      .
    CLASS-DATA gt_object_texts TYPE /mbtools/if_definitions=>ty_object_texts.

    CLASS-METHODS _map_object
      IMPORTING
        !iv_pgmid    TYPE csequence DEFAULT c_pgmid-r3tr
        !iv_object   TYPE csequence
        !iv_obj_name TYPE csequence
      EXPORTING
        !ev_pgmid    TYPE e071-pgmid
        !ev_object   TYPE e071-object
        !ev_obj_name TYPE e071-obj_name.

    CLASS-METHODS _jump_basis
      IMPORTING
        !iv_pgmid    TYPE csequence DEFAULT c_pgmid-basis
        !iv_object   TYPE csequence
        !iv_obj_name TYPE csequence.

ENDCLASS.



CLASS /mbtools/cl_sap IMPLEMENTATION.


  METHOD class_constructor.

    DATA:
      ls_object_text TYPE /mbtools/if_definitions=>ty_object_text.

    " Read standard texts of object
    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = gt_object_texts.

    " Add texts for non-transportable objects (or from previous releases)
    ls_object_text-pgmid  = c_pgmid-head.
    ls_object_text-object = 'SYST'.
    ls_object_text-text   = 'System Head'(107).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-pgmid  = c_pgmid-r3tr.
    ls_object_text-object = 'LSYS'.
    ls_object_text-text   = 'Source System'(100).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = 'ADMS'.
    ls_object_text-text   = 'BPC DM Selection'(101).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = 'DRRU'.
    ls_object_text-text   = 'Remodeling Rule (SAP Delivery)'(102).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = 'CPAK'.
    ls_object_text-text   = 'Class (ABAP Objects)'(103).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = 'BMED'.
    ls_object_text-text   = '?'.
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = 'SLDB'.
    ls_object_text-text   = 'Logical Database'(104).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = 'ECSC'.
    ls_object_text-text   = 'eCATT System'(105).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = 'SOTL'.
    ls_object_text-text   = 'Concept (Online Text Repository) - Long Texts'(106).
    COLLECT ls_object_text INTO gt_object_texts.

    " Add Basis Objects
    ls_object_text-pgmid  = c_pgmid-basis.
    ls_object_text-object = /mbtools/if_objects=>c_basis-activity.
    ls_object_text-text   = 'Activity'(110).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = /mbtools/if_objects=>c_basis-client.
    ls_object_text-text   = 'Client'(111).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = /mbtools/if_objects=>c_basis-devclass.
    ls_object_text-text   = 'Package'(112).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = /mbtools/if_objects=>c_basis-owner.
    ls_object_text-text   = 'Owner'(113).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = /mbtools/if_objects=>c_basis-project.
    ls_object_text-text   = 'Project'(114).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = /mbtools/if_objects=>c_basis-request.
    ls_object_text-text   = 'Transport Request'(115).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = /mbtools/if_objects=>c_basis-system.
    ls_object_text-text   = 'System'(116).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = /mbtools/if_objects=>c_basis-target_group.
    ls_object_text-text   = 'Target Group'(117).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = /mbtools/if_objects=>c_basis-translayer.
    ls_object_text-text   = 'Transport Layer'(118).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = /mbtools/if_objects=>c_basis-transport_target.
    ls_object_text-text   = 'Transport Target'(119).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = /mbtools/if_objects=>c_basis-user.
    ls_object_text-text   = 'User'(120).
    COLLECT ls_object_text INTO gt_object_texts.

    " Add Workbench Development Objects
    SELECT type AS object type AS type singular AS text FROM euobjt APPENDING TABLE gt_object_texts
      WHERE spras = sy-langu ORDER BY type singular ##SUBRC_OK. "#EC CI_GENBUFF

    SORT gt_object_texts.
    DELETE ADJACENT DUPLICATES FROM gt_object_texts.

  ENDMETHOD.


  METHOD get_namespace.

    DATA lv_rest TYPE string.

    IF iv_obj_name CS '/'.
      SPLIT iv_obj_name+1 AT '/' INTO rv_result lv_rest.
      rv_result = '/' && rv_result.
    ELSE.
      rv_result = ''.
    ENDIF.

  ENDMETHOD.


  METHOD get_object_text.

    DATA:
      ls_object_text TYPE /mbtools/if_definitions=>ty_object_text.

    READ TABLE gt_object_texts INTO ls_object_text
      WITH KEY object = iv_object.
    IF sy-subrc = 0.
      rv_text = ls_object_text-text.
    ENDIF.

  ENDMETHOD.


  METHOD get_object_texts.

    rt_object_texts = gt_object_texts.

  ENDMETHOD.


  METHOD get_object_wo_namespace.

    DATA lv_rest TYPE string.

    IF iv_obj_name CS '/'.
      SPLIT iv_obj_name+1 AT '/' INTO lv_rest rv_result.
    ELSE.
      rv_result = iv_obj_name.
    ENDIF.

  ENDMETHOD.


  METHOD get_text_from_domain.

    DATA:
      lv_domain   TYPE ddobjname,
      ls_doma     TYPE dd01v,
      ls_values   TYPE dd07v,
      lt_values   TYPE TABLE OF dd07v WITH DEFAULT KEY,
      lt_values_n TYPE TABLE OF dd07v WITH DEFAULT KEY.

    lv_domain = iv_domain. "casting

    CALL FUNCTION 'DD_DOMA_GET'
      EXPORTING
        domain_name   = lv_domain
      IMPORTING
        dd01v_wa_a    = ls_doma
      TABLES
        dd07v_tab_a   = lt_values
        dd07v_tab_n   = lt_values_n
      EXCEPTIONS
        illegal_value = 1
        op_failure    = 2
        OTHERS        = 3.
    IF sy-subrc = 0.
      " Fixed domain value?
      READ TABLE lt_values INTO ls_values WITH KEY
        ddlanguage = sy-langu
        domvalue_l = iv_value.
      IF sy-subrc = 0.
        ev_text = ls_values-ddtext.
      ELSE.
        " Use formatted value as text
        CASE ls_doma-datatype(3).
          WHEN 'D16' OR 'D34' OR 'DEC' OR 'FLT' OR 'INT' OR 'CUR' OR 'QUA'.
            WRITE iv_value TO ev_text LEFT-JUSTIFIED.
          WHEN 'DAT'.
            WRITE iv_value TO ev_text USING EDIT MASK '==____-__-__'. "iso
          WHEN 'TIM'.
            WRITE iv_value TO ev_text USING EDIT MASK '==__:__:__'. "iso
          WHEN 'CHA' OR 'STR' OR 'CLN' OR 'NUM'.
            ev_text = iv_value.
          WHEN 'CUK'.
            SELECT SINGLE ltext FROM tcurt INTO ev_text
              WHERE spras = sy-langu AND waers = iv_value.
            IF sy-subrc <> 0.
              ev_text = 'Not found'(003).
            ENDIF.
          WHEN 'UNI'.
            SELECT SINGLE msehl FROM t006a INTO ev_text
              WHERE spras = sy-langu AND msehi = iv_value.
            IF sy-subrc <> 0.
              ev_text = 'Not found'(003).
            ENDIF.
          WHEN OTHERS.
            ev_text = 'No text'(002).
        ENDCASE.
      ENDIF.
    ELSE.
      ev_text = 'No text'(002).
    ENDIF.

  ENDMETHOD.


  METHOD get_values_from_domain.

    " Return value for a domain from fixed values or value table
    " Note: no ranges for fixed values
    DATA:
      lv_valtab  TYPE tabname,
      lv_valfld  TYPE fieldname,
      lv_txttab  TYPE tabname,
      lv_txtfld  TYPE fieldname,
      lv_field   TYPE fieldname,
      lv_len     TYPE intlen,
      lv_maxlen  TYPE i,
      lv_columns TYPE string,
      lv_tables  TYPE string,
      lv_where   TYPE string,
      lv_order   TYPE string,
      lv_valpos  TYPE valpos,
      ls_value   TYPE ty_domain_value.

    SELECT SINGLE entitytab FROM dd01l INTO lv_valtab
      WHERE domname = iv_domain AND as4local = 'A' AND as4vers = '0000'.

    IF sy-subrc = 0 AND lv_valtab IS INITIAL.

      " Get fix values from domain (no ranges)
      SELECT a~domvalue_l a~valpos a~appval b~ddtext INTO TABLE rt_values
        FROM dd07l AS a LEFT OUTER JOIN dd07t AS b
        ON a~domname = b~domname AND a~valpos = b~valpos AND b~ddlanguage = sy-langu
        WHERE a~domname = iv_domain AND a~as4local = 'A' AND a~as4vers = '0000'
        ORDER BY a~domvalue_l a~valpos.                "#EC CI_BUFFJOIN
      ASSERT sy-subrc >= 0.

    ELSE.

      " Get values from table (text table must include LANGU field)
      SELECT SINGLE fieldname FROM dd03l INTO lv_valfld
        WHERE tabname = lv_valtab AND rollname = iv_domain
          AND as4local = 'A' AND as4vers = '0000' ##WARN_OK.
      IF sy-subrc = 0.
        SELECT SINGLE tabname FROM dd08l INTO lv_txttab
          WHERE checktable = lv_valtab AND frkart = 'TEXT' ##WARN_OK.
        IF sy-subrc = 0.
          lv_maxlen = 0.
          SELECT fieldname intlen FROM dd03l INTO (lv_field, lv_len)
            WHERE tabname = lv_txttab AND inttype = 'C'
              AND as4local = 'A' AND as4vers = '0000'
              ORDER BY fieldname ##WARN_OK.
            IF lv_len > lv_maxlen.
              lv_txtfld = lv_field.
              lv_maxlen = lv_len.
            ENDIF.
          ENDSELECT.
          IF sy-subrc = 0.
            lv_columns = 'p~&1 t~&2' ##NO_TEXT.
            REPLACE '&1' WITH lv_valfld INTO lv_columns.
            REPLACE '&2' WITH lv_txtfld INTO lv_columns.
            lv_tables  = '&1 AS p JOIN &2 AS t ON p~&3 = t~&4' ##NO_TEXT.
            REPLACE '&1' WITH lv_valtab INTO lv_tables.
            REPLACE '&2' WITH lv_txttab INTO lv_tables.
            REPLACE '&3' WITH lv_valfld INTO lv_tables.
            REPLACE '&4' WITH lv_valfld INTO lv_tables.
            lv_where   = 'LANGU = ''&1''' ##NO_TEXT.
            REPLACE '&1' WITH sy-langu INTO lv_where.
            lv_order = 'p~&1' ##NO_TEXT.
            REPLACE '&1' WITH lv_valfld INTO lv_order.
          ENDIF.
        ENDIF.
        IF lv_columns IS INITIAL.
          lv_columns = lv_valfld.
          lv_tables  = lv_valtab.
          lv_where   = ''.
        ENDIF.
        lv_valpos = 0.
        SELECT (lv_columns) FROM (lv_tables)
          INTO (ls_value-domvalue_l, ls_value-ddtext)
          WHERE (lv_where) ORDER BY (lv_order).
          lv_valpos = lv_valpos + 1.
          ls_value-valpos = lv_valpos.
          APPEND ls_value TO rt_values.
        ENDSELECT.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD is_devc_deleted.

    DATA:
      lv_devclass TYPE devclass.

    SELECT SINGLE devclass FROM tdevc INTO lv_devclass
      WHERE devclass = iv_obj_name.

    rv_result = boolc( sy-subrc <> 0 ).

  ENDMETHOD.


  METHOD is_fugr_deleted.

    DATA:
      lv_area      TYPE rs38l-area,
      lv_namespace TYPE rs38l-namespace,
      lv_group     TYPE rs38l-area,
      lv_program   TYPE program.

    lv_area = iv_obj_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        complete_area                = lv_area
      IMPORTING
        namespace                    = lv_namespace
        group                        = lv_group
      EXCEPTIONS
        include_not_exists           = 1
        group_not_exists             = 2
        no_selections                = 3
        no_function_include          = 4
        no_function_pool             = 5
        delimiter_wrong_position     = 6
        no_customer_function_group   = 7
        no_customer_function_include = 8
        reserved_name_customer       = 9
        namespace_too_long           = 10
        area_length_error            = 11
        OTHERS                       = 12.
    IF sy-subrc <> 0.
      rv_result = abap_true.
      RETURN. "assume deleted
    ENDIF.

    CONCATENATE lv_namespace 'SAPL' lv_group INTO lv_program.

    SELECT SINGLE name FROM trdir INTO lv_program
      WHERE name = lv_program.
    IF sy-subrc <> 0.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD is_prog_deleted.

    DATA: lv_program TYPE progname.

    SELECT SINGLE name FROM trdir INTO lv_program
      WHERE name = iv_obj_name.
    IF sy-subrc <> 0.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD is_sap_note.

    " Interpret any number between 1 and 4999999 as an SAP Note
    rv_result = boolc( iv_input CO '0123456789'  AND strlen( iv_input ) <= 10 AND
                       iv_input BETWEEN c_note_min AND c_note_max ).

  ENDMETHOD.


  METHOD is_tobj_deleted.

    DATA:
      lv_objectname TYPE objh-objectname,
      lv_type_pos   TYPE i.

    lv_type_pos = strlen( iv_obj_name ) - 1.

    SELECT SINGLE objectname FROM objh INTO lv_objectname
      WHERE objectname = iv_obj_name(lv_type_pos)
        AND objecttype = iv_obj_name+lv_type_pos.       "#EC CI_GENBUFF

    rv_result = boolc( sy-subrc <> 0 ).

  ENDMETHOD.


  METHOD object_name_check.

    DATA:
      lv_number      TYPE i,
      lv_note_number TYPE cwbntnumm.

    rv_result = iv_input.
    CONDENSE rv_result NO-GAPS.

    " Format SAP Notes with leading zeros
    IF is_sap_note( rv_result ) = abap_true.
      " Adjust to numc10
      lv_number      = rv_result.
      lv_note_number = lv_number.
      rv_result      = lv_note_number.
    ENDIF.

  ENDMETHOD.


  METHOD run_program.

    DATA: ls_trdir_entry TYPE trdir.

    " Check if executable program exists
    SELECT SINGLE * FROM trdir INTO ls_trdir_entry
      WHERE name = iv_program AND subc = '1'.
    IF sy-subrc = 0.
      " Run program with authorization check
      CALL FUNCTION 'SUBMIT_REPORT'
        EXPORTING
          report           = ls_trdir_entry-name
          rdir             = ls_trdir_entry
          ret_via_leave    = abap_false
        EXCEPTIONS
          just_via_variant = 1
          no_submit_auth   = 2
          OTHERS           = 3.
      CASE sy-subrc.
        WHEN 0.
          rv_exit = abap_true.
        WHEN 1.
          MESSAGE i623(db) WITH iv_program.
        WHEN 2.
          MESSAGE i149(00) WITH iv_program.
        WHEN OTHERS.
          MESSAGE i000(/mbtools/bc) WITH 'Unknown error'(004) iv_program '' ''.
      ENDCASE.
    ELSE.
      MESSAGE i541(00) WITH iv_program.
    ENDIF.

  ENDMETHOD.


  METHOD run_transaction.

    DATA: lv_tcode TYPE sy-tcode.

    SELECT SINGLE tcode FROM tstc INTO lv_tcode WHERE tcode = iv_tcode.
    IF sy-subrc = 0.
      CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
        EXPORTING
          tcode  = lv_tcode
        EXCEPTIONS
          ok     = 0
          not_ok = 2
          OTHERS = 3.
      CASE sy-subrc.
        WHEN 0.
          CALL TRANSACTION lv_tcode.                     "#EC CI_CALLTA
          rv_exit = abap_true.
        WHEN 2.
          MESSAGE i172(00) WITH iv_tcode.
        WHEN OTHERS.
          MESSAGE i000(/mbtools/bc) WITH 'Unknown error'(004) iv_tcode '' ''.
      ENDCASE.
    ELSE.
      MESSAGE i010(01) WITH iv_tcode.
    ENDIF.

  ENDMETHOD.


  METHOD show_icon.

    CONSTANTS: lc_icon_browser TYPE progname VALUE '/MBTOOLS/ICON_BROWSER'.

    DATA: lv_name TYPE trdir-name.

    " Check if executable program exists
    SELECT SINGLE name FROM trdir INTO lv_name
      WHERE name = lc_icon_browser AND subc = '1'.
    IF sy-subrc = 0.
      cl_sabe=>auth_check_prognam(
        EXPORTING
          i_scenario_name = 'BC_GENERIC_REPORT_START'
          i_program_name  = lc_icon_browser
          i_action        = 'SUBMIT'
        EXCEPTIONS
          not_authorized  = 1
          OTHERS          = 2 ).
      IF sy-subrc <> 0.
        MESSAGE i149(00) WITH lc_icon_browser.
        RETURN.
      ENDIF.

      SUBMIT (lc_icon_browser)
        WITH p_disp_i = abap_false
        WITH p_disp_n = abap_false
        WITH p_disp_p = abap_true
        WITH s_icon   = iv_icon
        AND RETURN.                                      "#EC CI_SUBMIT

      rv_exit = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD show_object.

    DATA:
      lv_pgmid         TYPE /mbtools/if_definitions=>ty_pgmid,
      lv_object        TYPE /mbtools/if_definitions=>ty_object,
      lv_obj_name      TYPE /mbtools/if_definitions=>ty_name,
      lv_sub_obj_name  TYPE /mbtools/if_definitions=>ty_name,
      lv_e071_pgmid    TYPE e071-pgmid,
      lv_e071_object   TYPE e071-object,
      lv_e071_obj_name TYPE e071-obj_name.

    " Check if object exist (maybe as part object)
    READ TABLE gt_object_texts TRANSPORTING NO FIELDS
      WITH KEY pgmid = iv_pgmid object = iv_object.
    IF sy-subrc = 0.
      lv_pgmid  = iv_pgmid.
    ELSE.
      READ TABLE gt_object_texts TRANSPORTING NO FIELDS
        WITH KEY pgmid = c_pgmid-limu object = iv_object.
      IF sy-subrc = 0.
        lv_pgmid = c_pgmid-limu.
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    " Basis objects
    IF iv_pgmid = c_pgmid-basis.
      _jump_basis(
        iv_pgmid    = iv_pgmid
        iv_object   = iv_object
        iv_obj_name = iv_obj_name ).

      RETURN.
    ENDIF.

    lv_object       = iv_object.
    lv_obj_name     = iv_obj_name.
    lv_sub_obj_name = iv_sub_obj_name.

    IF iv_line_number IS NOT INITIAL AND iv_sub_obj_name IS NOT INITIAL.
      " Workbench tools with source position (new window)
      CALL FUNCTION 'RS_TOOL_ACCESS'
        EXPORTING
          operation           = 'SHOW'
          object_type         = lv_object
          object_name         = lv_obj_name
          include             = lv_sub_obj_name
          position            = iv_line_number
          in_new_window       = abap_true
        EXCEPTIONS
          not_executed        = 1
          invalid_object_type = 2
          OTHERS              = 3.
    ELSE.
      " Workbench tools (same window)
      CALL FUNCTION 'RS_TOOL_ACCESS'
        EXPORTING
          operation           = 'SHOW'
          object_type         = lv_object
          object_name         = lv_obj_name
        EXCEPTIONS
          not_executed        = 1
          invalid_object_type = 2
          OTHERS              = 3.
    ENDIF.
    IF sy-subrc = 0.
      rv_exit = abap_true.
      RETURN.
    ENDIF.

    " Transport tools
    _map_object(
      EXPORTING
        iv_pgmid    = lv_pgmid
        iv_object   = iv_object
        iv_obj_name = iv_obj_name
      IMPORTING
        ev_pgmid    = lv_e071_pgmid
        ev_object   = lv_e071_object
        ev_obj_name = lv_e071_obj_name ).

    CALL FUNCTION 'TR_OBJECT_JUMP_TO_TOOL'
      EXPORTING
        iv_action         = 'SHOW'
        iv_pgmid          = lv_e071_pgmid
        iv_object         = lv_e071_object
        iv_obj_name       = lv_e071_obj_name
      EXCEPTIONS
        jump_not_possible = 1
        OTHERS            = 2.
    IF sy-subrc = 0.
      rv_exit = abap_true.
    ELSE.
      MESSAGE s000 WITH 'Navigation not available'(001).
    ENDIF.

  ENDMETHOD.


  METHOD _jump_basis.

    DATA:
      lv_activity   TYPE tractivity,
      lv_client     TYPE mandt,
      lv_devclass   TYPE devclass,
      lv_trkorr     TYPE trkorr,
      lv_project    TYPE trkorr_p,
      lv_system     TYPE sysname,
      lv_translayer TYPE devlayer,
      lv_username   TYPE xubname.

    CASE iv_object.
      WHEN /mbtools/if_objects=>c_basis-activity.
        lv_activity = iv_obj_name.

        CALL FUNCTION 'TR_SHOW_ACTIVITY'
          EXPORTING
            iv_activity = lv_activity.

      WHEN /mbtools/if_objects=>c_basis-client.
        lv_client = iv_obj_name.

        CALL FUNCTION 'TR_SHOW_CLIENT'
          EXPORTING
            iv_client = lv_client.

      WHEN /mbtools/if_objects=>c_basis-devclass.
        lv_devclass = iv_obj_name.

        CALL FUNCTION 'TR_SHOW_DEVCLASS'
          EXPORTING
            iv_devclass = lv_devclass.

      WHEN /mbtools/if_objects=>c_basis-owner.
        lv_trkorr = iv_obj_name.

        CALL FUNCTION 'TR_SHOW_OWNER'
          EXPORTING
            iv_trkorr = lv_trkorr.

      WHEN /mbtools/if_objects=>c_basis-project.
        lv_project = iv_obj_name.

        CALL FUNCTION 'TR_SHOW_PROJECT'
          EXPORTING
            iv_project = lv_project.

      WHEN /mbtools/if_objects=>c_basis-request.
        lv_trkorr = iv_obj_name.

        CALL FUNCTION 'TR_SHOW_REQUEST'
          EXPORTING
            iv_trkorr = lv_trkorr
            iv_popup  = abap_true.

      WHEN /mbtools/if_objects=>c_basis-system.
        lv_system = iv_obj_name.

        CALL FUNCTION 'TR_SHOW_SYSTEM'
          EXPORTING
            iv_system = lv_system.

      WHEN /mbtools/if_objects=>c_basis-translayer.
        lv_translayer = iv_obj_name.

        CALL FUNCTION 'TR_SHOW_TRANSLAYER'
          EXPORTING
            iv_translayer = lv_translayer.

      WHEN /mbtools/if_objects=>c_basis-user.
        lv_username = iv_obj_name.

        CALL FUNCTION 'TR_SHOW_USER'
          EXPORTING
            iv_username = lv_username.

    ENDCASE.

  ENDMETHOD.


  METHOD _map_object.

    DATA:
      lv_len    TYPE i,
      lv_iobjnm TYPE rsiobjnm.

    ev_pgmid    = iv_pgmid.
    ev_object   = iv_object.
    ev_obj_name = iv_obj_name.

    CASE iv_object.
      WHEN /mbtools/if_objects=>c_dimension.
        " Map Dimension to Base InfoCube
        lv_len = strlen( iv_obj_name ) - 1.
        ev_object   = /mbtools/if_objects=>c_infocube.
        ev_obj_name = iv_obj_name(lv_len).

      WHEN /mbtools/if_objects=>c_infoobject.
        " Map InfoSet and Attribute Fields to Base InfoObject
        lv_iobjnm = iv_obj_name.
        CALL FUNCTION 'RSD_IOBJNM_PARSE'
          EXPORTING
            i_iobjnm = lv_iobjnm
          IMPORTING
            e_iobjnm = lv_iobjnm.
        ev_obj_name = lv_iobjnm.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
