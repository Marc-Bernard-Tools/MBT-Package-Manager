************************************************************************
* /MBTOOLS/CL_SAP
* MBT SAP
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
class /MBTOOLS/CL_SAP definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_domain_value,
        domvalue_l TYPE domvalue_l,
        valpos     TYPE valpos,
        appval     TYPE ddappval,
        ddtext     TYPE val_text,
      END OF ty_domain_value .
  types:
    ty_domain_values TYPE STANDARD TABLE OF ty_domain_value WITH DEFAULT KEY .

  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_OBJECT_WO_NAMESPACE
    importing
      !IV_OBJ_NAME type CSEQUENCE
    returning
      value(RV_RESULT) type /MBTOOLS/IF_DEFINITIONS=>TY_NAME .
  class-methods GET_NAMESPACE
    importing
      !IV_OBJ_NAME type CSEQUENCE
    returning
      value(RV_RESULT) type NAMESPACE .
  class-methods GET_OBJECT_TEXT
    importing
      value(IV_OBJECT) type CSEQUENCE
    returning
      value(RV_TEXT) type DDTEXT .
  class-methods GET_OBJECT_TEXTS
    returning
      value(RT_OBJECT_TEXTS) type /MBTOOLS/IF_DEFINITIONS=>TY_OBJECT_TEXTS .
  class-methods GET_TEXT_FROM_DOMAIN
    importing
      !IV_DOMAIN type ANY default 'YESNO'
      !IV_VALUE type ANY
    exporting
      value(EV_TEXT) type CLIKE .
  class-methods GET_VALUES_FROM_DOMAIN
    importing
      !IV_DOMAIN type ANY
    returning
      value(RT_VALUES) type TY_DOMAIN_VALUES .
  class-methods IS_DEVC_DELETED
    importing
      !IV_OBJ_NAME type CSEQUENCE
    returning
      value(RV_RESULT) type ABAP_BOOL .
  class-methods IS_FUGR_DELETED
    importing
      !IV_OBJ_NAME type CSEQUENCE
    returning
      value(RV_RESULT) type ABAP_BOOL .
  class-methods IS_SAP_NOTE
    importing
      !IV_INPUT type CSEQUENCE
    returning
      value(RV_RESULT) type ABAP_BOOL .
  class-methods IS_TOBJ_DELETED
    importing
      !IV_OBJ_NAME type CSEQUENCE
    returning
      value(RV_RESULT) type ABAP_BOOL .
  class-methods OBJECT_NAME_CHECK
    importing
      !IV_INPUT type CSEQUENCE
    returning
      value(RV_RESULT) type STRING .
  class-methods SHOW_OBJECT
    importing
      !IV_PGMID type CSEQUENCE default 'R3TR'
      !IV_OBJECT type CSEQUENCE
      !IV_OBJ_NAME type CSEQUENCE
    returning
      value(RV_EXIT) type ABAP_BOOL .
  class-methods RUN_TRANSACTION
    importing
      !IV_TCODE type CSEQUENCE
    returning
      value(RV_EXIT) type ABAP_BOOL .
  class-methods RUN_PROGRAM
    importing
      !IV_PROGRAM type CSEQUENCE
    returning
      value(RV_EXIT) type ABAP_BOOL .
  PROTECTED SECTION.

private section.

  constants C_NOTE_MIN type CWBNTNUMM value '1' ##NO_TEXT.
  constants C_NOTE_MAX type CWBNTNUMM value '3999999' ##NO_TEXT.
  class-data GT_OBJECT_TEXTS type /MBTOOLS/IF_DEFINITIONS=>TY_OBJECT_TEXTS .

  class-methods MAP_OBJECT
    importing
      !IV_PGMID type CSEQUENCE default 'R3TR'
      !IV_OBJECT type CSEQUENCE
      !IV_OBJ_NAME type CSEQUENCE
    exporting
      !EV_PGMID type E071-PGMID
      !EV_OBJECT type E071-OBJECT
      !EV_OBJ_NAME type E071-OBJ_NAME .
ENDCLASS.



CLASS /MBTOOLS/CL_SAP IMPLEMENTATION.


  METHOD class_constructor.

    DATA:
      ls_object_text TYPE /mbtools/if_definitions=>ty_object_text.

    " Read standard texts of object
    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = gt_object_texts.

    " Add texts for non-transportable objects (or from previous releases)
    ls_object_text-pgmid  = 'R3TR'.
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
    ls_object_text-text   = 'Logical Databases'(104).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = 'ECSC'.
    ls_object_text-text   = 'eCATT System'(105).
    COLLECT ls_object_text INTO gt_object_texts.
    ls_object_text-object = 'SOTL'.
    ls_object_text-text   = 'Concept (Online Text Repository) - Long Texts'(106).
    COLLECT ls_object_text INTO gt_object_texts.

    " Add Workbench Development Objects
    SELECT type singular FROM euobjt INTO (ls_object_text-object, ls_object_text-text)
      WHERE spras = sy-langu.
      COLLECT ls_object_text INTO gt_object_texts.
    ENDSELECT.

    SORT gt_object_texts.

  ENDMETHOD.


  METHOD get_namespace.

    IF iv_obj_name CS '/'.
      SPLIT iv_obj_name+1 AT '/' INTO rv_result sy-lisel.
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

    IF iv_obj_name CS '/'.
      SPLIT iv_obj_name+1 AT '/' INTO sy-lisel rv_result.
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
      WHERE domname = iv_domain AND as4local = 'A'.

    IF sy-subrc = 0 AND lv_valtab IS INITIAL.

      " Get fix values from domain (no ranges)
      SELECT a~domvalue_l a~valpos a~appval b~ddtext INTO TABLE rt_values
        FROM dd07l AS a LEFT OUTER JOIN dd07t AS b
        ON a~domname = b~domname AND a~valpos = b~valpos AND b~ddlanguage = sy-langu
        WHERE a~domname = iv_domain AND a~as4local = 'A'.

    ELSE.

      " Get values from table (text table must include LANGU field)
      SELECT SINGLE fieldname FROM dd03l INTO lv_valfld
        WHERE tabname = lv_valtab AND rollname = iv_domain AND as4local = 'A'.
      IF sy-subrc = 0.
        SELECT SINGLE tabname FROM dd08l INTO lv_txttab
          WHERE checktable = lv_valtab AND frkart = 'TEXT'.
        IF sy-subrc = 0.
          lv_maxlen = 0.
          SELECT fieldname intlen FROM dd03l INTO (lv_field, lv_len)
            WHERE tabname = lv_txttab AND inttype = 'C' AND as4local = 'A'.
            IF lv_len > lv_maxlen.
              lv_txtfld = lv_field.
              lv_maxlen = lv_len.
            ENDIF.
          ENDSELECT.
          IF sy-subrc = 0.
            lv_columns = 'p~&1 t~&2'.
            REPLACE '&1' WITH lv_valfld INTO lv_columns.
            REPLACE '&2' WITH lv_txtfld INTO lv_columns.
            lv_tables  = '&1 AS p JOIN &2 AS t ON p~&3 = t~&4'.
            REPLACE '&1' WITH lv_valtab INTO lv_tables.
            REPLACE '&2' WITH lv_txttab INTO lv_tables.
            REPLACE '&3' WITH lv_valfld INTO lv_tables.
            REPLACE '&4' WITH lv_valfld INTO lv_tables.
            lv_where   = 'LANGU = ''&1'''.
            REPLACE '&1' WITH sy-langu INTO lv_where.
            lv_order = 'p~&1'.
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
          ADD 1 TO lv_valpos.
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


  METHOD is_sap_note.

    " Interpret any number between 1 and 4999999 as an SAP Note
    IF iv_input CO '0123456789'  AND strlen( iv_input ) <= 10 AND
       iv_input BETWEEN c_note_min AND c_note_max.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.

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


  METHOD map_object.

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
          ret_via_leave    = abap_true
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
          MESSAGE i001(00) WITH 'Unknown error'(004) iv_program.
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
          CALL TRANSACTION lv_tcode.
          rv_exit = abap_true.
        WHEN 2.
          MESSAGE i172(00) WITH iv_tcode.
        WHEN OTHERS.
          MESSAGE i001(00) WITH 'Unknown error'(004) iv_tcode.
      ENDCASE.
    ELSE.
      MESSAGE i010(01) WITH iv_tcode.
    ENDIF.

  ENDMETHOD.


  METHOD show_object.

    DATA:
      lv_pgmid         TYPE /mbtools/if_definitions=>ty_pgmid,
      lv_object        TYPE /mbtools/if_definitions=>ty_object,
      lv_obj_name      TYPE /mbtools/if_definitions=>ty_name,
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
        WITH KEY pgmid = 'LIMU' object = iv_object.
      IF sy-subrc = 0.
        lv_pgmid = 'LIMU'.
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    lv_object   = iv_object.
    lv_obj_name = iv_obj_name.

    " First try: workbench tools
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_type         = lv_object
        object_name         = lv_obj_name
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.
    IF sy-subrc = 0.
      rv_exit = abap_true.
      RETURN.
    ENDIF.

    " Second try: transport tool
    map_object(
      EXPORTING
        iv_pgmid    = iv_pgmid
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
ENDCLASS.
