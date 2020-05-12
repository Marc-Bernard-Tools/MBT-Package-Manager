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
    ty_object_texts TYPE STANDARD TABLE OF ko100 WITH DEFAULT KEY .

  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_OBJECT_WO_NAMESPACE
    importing
      !I_OBJ_NAME type CSEQUENCE
    returning
      value(R_RESULT) type SOBJ_NAME .
  class-methods GET_NAMESPACE
    importing
      !I_OBJ_NAME type CSEQUENCE
    returning
      value(R_RESULT) type NAMESPACE .
  class-methods GET_OBJECT_TEXT
    importing
      value(I_OBJECT) type TROBJTYPE
    returning
      value(R_TEXT) type DDTEXT .
  class-methods GET_OBJECT_TEXTS
    returning
      value(R_OBJECT_TEXTS) type TY_OBJECT_TEXTS .
  class-methods IS_DEVC_DELETED
    importing
      !I_OBJ_NAME type CSEQUENCE
    returning
      value(R_RESULT) type ABAP_BOOL .
  class-methods IS_FUGR_DELETED
    importing
      !I_OBJ_NAME type CSEQUENCE
    returning
      value(R_RESULT) type ABAP_BOOL .
  class-methods IS_SAP_NOTE
    importing
      !I_INPUT type CSEQUENCE
    returning
      value(R_RESULT) type ABAP_BOOL .
  class-methods IS_TOBJ_DELETED
    importing
      !I_OBJ_NAME type CSEQUENCE
    returning
      value(R_RESULT) type ABAP_BOOL .
  class-methods OBJECT_NAME_CHECK
    importing
      !I_INPUT type CSEQUENCE
    returning
      value(R_RESULT) type STRING .
  class-methods SHOW_OBJECT
    importing
      !I_PGMID type TADIR-PGMID
      !I_OBJECT type TADIR-OBJECT
      !I_OBJ_NAME type TADIR-OBJ_NAME
    returning
      value(R_EXIT) type ABAP_BOOL .
  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS c_note_min TYPE cwbntnumm VALUE '1' ##NO_TEXT.
    CONSTANTS c_note_max TYPE cwbntnumm VALUE '3999999' ##NO_TEXT.

    CLASS-DATA object_texts TYPE ty_object_texts .

ENDCLASS.



CLASS /MBTOOLS/CL_SAP IMPLEMENTATION.


  METHOD class_constructor.

    DATA object_text TYPE ko100.

    " Read standard texts of object
    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = object_texts.

    " Add texts for non-transportable objects (or from previous releases)
    object_text-pgmid  = 'R3TR'.
    object_text-object = 'LSYS'.
    object_text-text   = 'Source System'.
    COLLECT object_text INTO object_texts.
    object_text-object = 'ADMS'.
    object_text-text   = 'BPC DM Selection'.
    COLLECT object_text INTO object_texts.
    object_text-object = 'DRRU'.
    object_text-text   = 'Remodeling Rule (SAP Delivery)'.
    COLLECT object_text INTO object_texts.
    object_text-object = 'CPAK'.
    object_text-text   = 'Class (ABAP Objects)'.
    COLLECT object_text INTO object_texts.
    object_text-object = 'BMED'.
    object_text-text   = '?'.
    COLLECT object_text INTO object_texts.
    object_text-object = 'SLDB'.
    object_text-text   = 'Logical Databases'.
    COLLECT object_text INTO object_texts.
    object_text-object = 'ECSC'.
    object_text-text   = 'eCATT System'.
    COLLECT object_text INTO object_texts.
    object_text-object = 'SOTL'.
    object_text-text   = 'Concept (Online Text Repository) - Long Texts'.
    COLLECT object_text INTO object_texts.
*    object_text-object = ''.
*    object_text-text   = ''.
*    COLLECT object_text INTO object_texts.

    " Add Workbench Development Objects
    SELECT type singular FROM euobjt INTO (object_text-object, object_text-text)
      WHERE spras = sy-langu.
      COLLECT object_text INTO object_texts.
    ENDSELECT.

    SORT object_texts.

  ENDMETHOD.


  METHOD get_namespace.

    DATA: obj_name TYPE string.

    IF i_obj_name CS '/'.
      SPLIT i_obj_name+1 AT '/' INTO r_result obj_name.
      r_result = '/' && r_result.
    ELSE.
      r_result = ''.
    ENDIF.

  ENDMETHOD.


  METHOD get_object_text.

    DATA object_text TYPE ko100.

    READ TABLE object_texts INTO object_text
      WITH KEY object = i_object.
    IF sy-subrc = 0.
      r_text = object_text-text.
    ENDIF.

  ENDMETHOD.


  METHOD get_object_texts.

    r_object_texts = object_texts.

  ENDMETHOD.


  METHOD get_object_wo_namespace.

    DATA: namespace TYPE namespace.

    IF i_obj_name CS '/'.
      SPLIT i_obj_name+1 AT '/' INTO namespace r_result.
    ELSE.
      r_result = i_obj_name.
    ENDIF.

  ENDMETHOD.


  METHOD is_devc_deleted.

    DATA: devclass TYPE devclass.

    SELECT SINGLE devclass FROM tdevc INTO devclass
      WHERE devclass = i_obj_name.

    r_result = boolc( sy-subrc <> 0 ).

  ENDMETHOD.


  METHOD is_fugr_deleted.

    DATA: area      TYPE rs38l-area,
          namespace TYPE rs38l-namespace,
          group     TYPE rs38l-area,
          program   TYPE program.

    area = i_obj_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        complete_area                = area
      IMPORTING
        namespace                    = namespace
        group                        = group
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
      r_result = abap_true.
      RETURN. "assume deleted
    ENDIF.

    CONCATENATE namespace 'SAPL' group INTO program.

    SELECT SINGLE name FROM trdir INTO program
      WHERE name = program.
    IF sy-subrc <> 0.
      r_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD is_sap_note.

    " Interpret any number between 1 and 4999999 as an SAP Note
    IF i_input CO '0123456789'  AND strlen( i_input ) <= 10 AND
       i_input BETWEEN c_note_min AND c_note_max.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_tobj_deleted.

    DATA:
      objectname TYPE objh-objectname,
      type_pos   TYPE i.

    type_pos = strlen( i_obj_name ) - 1.

    SELECT SINGLE objectname FROM objh INTO objectname
      WHERE objectname = i_obj_name(type_pos)
      AND objecttype = i_obj_name+type_pos.             "#EC CI_GENBUFF

    r_result = boolc( sy-subrc <> 0 ).

  ENDMETHOD.


  METHOD object_name_check.

    DATA:
      number      TYPE i,
      note_number TYPE cwbntnumm.

    r_result = i_input.
    CONDENSE r_result NO-GAPS.

    " Format SAP Notes with leading zeros
    IF is_sap_note( r_result ).
      " Adjust to numc10
      number      = r_result.
      note_number = number.
      r_result    = note_number.
    ENDIF.

  ENDMETHOD.


  METHOD show_object.

    DATA:
      e071_obj_name TYPE e071-obj_name.

    " First try: workbench tools
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = i_obj_name
        object_type         = i_object
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.
    IF sy-subrc = 0.
      r_exit = abap_true.
      RETURN.
    ENDIF.

    " Second try: transport tool
    e071_obj_name = i_obj_name.

    CALL FUNCTION 'TR_OBJECT_JUMP_TO_TOOL'
      EXPORTING
        iv_action         = 'SHOW'
        iv_pgmid          = i_pgmid
        iv_object         = i_object
        iv_obj_name       = e071_obj_name
      EXCEPTIONS
        jump_not_possible = 1
        OTHERS            = 2.
    IF sy-subrc = 0.
      r_exit = abap_true.
    ELSE.
      MESSAGE s000 WITH 'Navigation not available'(001).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
