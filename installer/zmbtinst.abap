REPORT zmbtinst.

************************************************************************
* MBT Installer
*
* This program installs and uninstalls any Marc Bernard Tool
*
* (c) MBT 2021 https://marcbernardtools.com/
************************************************************************

CONSTANTS:
  c_version TYPE string VALUE '1.0.0',
  c_home    TYPE string VALUE 'https://marcbernardtools.com/' ##NO_TEXT,
  c_github  TYPE string VALUE 'github.com' ##NO_TEXT.

INTERFACE zif_abapgit_definitions DEFERRED.
INTERFACE zif_abapgit_sap_package DEFERRED.
INTERFACE zif_abapgit_dot_abapgit DEFERRED.
INTERFACE zif_abapgit_environment DEFERRED.
INTERFACE zif_abapgit_log DEFERRED.
INTERFACE zif_abapgit_longtexts DEFERRED.
INTERFACE zif_abapgit_lxe_texts DEFERRED.
INTERFACE zif_abapgit_object DEFERRED.
INTERFACE zif_abapgit_xml_input DEFERRED.
INTERFACE zif_abapgit_xml_output DEFERRED.
INTERFACE zif_abapgit_lang_definitions DEFERRED.
INTERFACE zif_abapgit_oo_object_fnc DEFERRED.
INTERFACE zif_abapgit_comparator DEFERRED.
INTERFACE zif_abapgit_progress DEFERRED.
INTERFACE zif_abapgit_tadir DEFERRED.
INTERFACE zif_abapinst_definitions DEFERRED.
INTERFACE zif_abapgit_objects DEFERRED.
INTERFACE zif_ajson_reader DEFERRED.
INTERFACE zif_ajson_writer DEFERRED.
INTERFACE zif_ajson DEFERRED.
INTERFACE zif_ajson_mapping DEFERRED.
INTERFACE zif_abapgit_persistence DEFERRED.
INTERFACE zif_abapgit_exit DEFERRED.
INTERFACE zif_abapgit_gui_functions DEFERRED.
INTERFACE zif_abapgit_object_enho DEFERRED.
INTERFACE zif_abapgit_object_enhs DEFERRED.
INTERFACE zif_abapgit_version DEFERRED.
"! abapGit general error
CLASS zcx_abapgit_exception DEFINITION

  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF gc_section_text,
        cause           TYPE string VALUE `Cause`,
        system_response TYPE string VALUE `System response`,
        what_to_do      TYPE string VALUE `Procedure`,
        sys_admin       TYPE string VALUE `System administration`,
      END OF gc_section_text .
    CONSTANTS:
      BEGIN OF gc_section_token,
        cause           TYPE string VALUE `&CAUSE&`,
        system_response TYPE string VALUE `&SYSTEM_RESPONSE&`,
        what_to_do      TYPE string VALUE `&WHAT_TO_DO&`,
        sys_admin       TYPE string VALUE `&SYS_ADMIN&`,
      END OF gc_section_token .
    DATA msgv1 TYPE symsgv READ-ONLY .
    DATA msgv2 TYPE symsgv READ-ONLY .
    DATA msgv3 TYPE symsgv READ-ONLY .
    DATA msgv4 TYPE symsgv READ-ONLY .
    DATA mt_callstack TYPE abap_callstack READ-ONLY .

    "! Raise exception with text
    "! @parameter iv_text | Text
    "! @parameter ix_previous | Previous exception
    "! @raising zcx_abapgit_exception | Exception
    CLASS-METHODS raise
      IMPORTING
        !iv_text     TYPE clike
        !ix_previous TYPE REF TO cx_root OPTIONAL
      RAISING
        zcx_abapgit_exception .
    "! Raise exception with T100 message
    "! <p>
    "! Will default to sy-msg* variables. These need to be set right before calling this method.
    "! </p>
    "! @parameter iv_msgid | Message ID
    "! @parameter iv_msgno | Message number
    "! @parameter iv_msgv1 | Message variable 1
    "! @parameter iv_msgv2 | Message variable 2
    "! @parameter iv_msgv3 | Message variable 3
    "! @parameter iv_msgv4 | Message variable 4
    "! @raising zcx_abapgit_exception | Exception
    CLASS-METHODS raise_t100
      IMPORTING
        VALUE(iv_msgid) TYPE symsgid DEFAULT sy-msgid
        VALUE(iv_msgno) TYPE symsgno DEFAULT sy-msgno
        VALUE(iv_msgv1) TYPE symsgv DEFAULT sy-msgv1
        VALUE(iv_msgv2) TYPE symsgv DEFAULT sy-msgv2
        VALUE(iv_msgv3) TYPE symsgv DEFAULT sy-msgv3
        VALUE(iv_msgv4) TYPE symsgv DEFAULT sy-msgv4
        !ix_previous    TYPE REF TO cx_root OPTIONAL
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS raise_with_text
      IMPORTING
        !ix_previous TYPE REF TO cx_root
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgv1    TYPE symsgv OPTIONAL
        !msgv2    TYPE symsgv OPTIONAL
        !msgv3    TYPE symsgv OPTIONAL
        !msgv4    TYPE symsgv OPTIONAL .

    METHODS get_source_position
        REDEFINITION .
    METHODS if_message~get_longtext
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS gc_generic_error_msg TYPE string VALUE `An error occured (ZCX_ABAPGIT_EXCEPTION)` ##NO_TEXT.

    CLASS-METHODS split_text_to_symsg
      IMPORTING
        !iv_text      TYPE string
      RETURNING
        VALUE(rs_msg) TYPE symsg .
    METHODS save_callstack .
    METHODS itf_to_string
      IMPORTING
        !it_itf          TYPE tline_tab
      RETURNING
        VALUE(rv_result) TYPE string .
    METHODS get_t100_longtext_itf
      RETURNING
        VALUE(rt_itf) TYPE tline_tab .
    METHODS remove_empty_section
      IMPORTING
        !iv_tabix_from TYPE i
        !iv_tabix_to   TYPE i
      CHANGING
        !ct_itf        TYPE tline_tab .
    METHODS replace_section_head_with_text
      CHANGING
        !cs_itf TYPE tline .
ENDCLASS.



CLASS ZCX_ABAPGIT_EXCEPTION IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).

    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
    me->msgv3 = msgv3.
    me->msgv4 = msgv4.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    save_callstack( ).

  ENDMETHOD.


  METHOD get_source_position.

    FIELD-SYMBOLS: <ls_callstack> TYPE abap_callstack_line.

    READ TABLE mt_callstack ASSIGNING <ls_callstack>
                            INDEX 1.
    IF sy-subrc = 0.
      program_name = <ls_callstack>-mainprogram.
      include_name = <ls_callstack>-include.
      source_line  = <ls_callstack>-line.
    ELSE.
      super->get_source_position(
        IMPORTING
          program_name = program_name
          include_name = include_name
          source_line  = source_line   ).
    ENDIF.

  ENDMETHOD.


  METHOD get_t100_longtext_itf.

    DATA: lv_docu_key TYPE doku_obj.

    FIELD-SYMBOLS <lv_msgv> TYPE any.

    lv_docu_key = if_t100_message~t100key-msgid && if_t100_message~t100key-msgno.

    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id     = 'NA'
        langu  = sy-langu
        object = lv_docu_key
        typ    = 'E'
      TABLES
        line   = rt_itf
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc = 0.
      ASSIGN me->(if_t100_message~t100key-attr1) TO <lv_msgv>.
      IF sy-subrc = 0.
        REPLACE ALL OCCURRENCES OF '&V1&' IN TABLE rt_itf WITH <lv_msgv>.
      ENDIF.
      ASSIGN me->(if_t100_message~t100key-attr2) TO <lv_msgv>.
      IF sy-subrc = 0.
        REPLACE ALL OCCURRENCES OF '&V2&' IN TABLE rt_itf WITH <lv_msgv>.
      ENDIF.
      ASSIGN me->(if_t100_message~t100key-attr3) TO <lv_msgv>.
      IF sy-subrc = 0.
        REPLACE ALL OCCURRENCES OF '&V3&' IN TABLE rt_itf WITH <lv_msgv>.
      ENDIF.
      ASSIGN me->(if_t100_message~t100key-attr4) TO <lv_msgv>.
      IF sy-subrc = 0.
        REPLACE ALL OCCURRENCES OF '&V4&' IN TABLE rt_itf WITH <lv_msgv>.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD if_message~get_longtext.

    result = super->get_longtext( ).

    IF if_t100_message~t100key IS NOT INITIAL.

      result = itf_to_string( get_t100_longtext_itf( ) ).

    ENDIF.

  ENDMETHOD.


  METHOD itf_to_string.

    CONSTANTS: lc_format_section TYPE string VALUE 'U1'.

    DATA:
      lt_stream      TYPE TABLE OF tdline,
      lt_string      TYPE TABLE OF string,
      lv_string      LIKE LINE OF lt_string,
      lt_itf         TYPE tline_tab,
      lv_has_content TYPE abap_bool,
      lv_tabix_from  TYPE syst-tabix,
      lv_tabix_to    TYPE syst-tabix.

    FIELD-SYMBOLS: <ls_itf_section>      TYPE tline,
                   <ls_itf_section_item> TYPE tline.

    lt_itf = it_itf.

    " You should remember that we replace the U1 format because
    " that preserves the section header of longtexts.
    LOOP AT lt_itf ASSIGNING <ls_itf_section>
                   WHERE tdformat = lc_format_section.

      CLEAR:
        lv_has_content,
        lv_tabix_to.

      lv_tabix_from = sy-tabix.

      LOOP AT lt_itf ASSIGNING <ls_itf_section_item>
                     FROM sy-tabix + 1.

        IF <ls_itf_section_item>-tdformat = lc_format_section.
          lv_tabix_to = sy-tabix.
          EXIT.
        ELSEIF <ls_itf_section_item>-tdline IS NOT INITIAL.
          lv_has_content = abap_true.
        ENDIF.

      ENDLOOP.

      IF lv_has_content = abap_false.
        remove_empty_section(
          EXPORTING
            iv_tabix_from = lv_tabix_from
            iv_tabix_to   = lv_tabix_to
          CHANGING
            ct_itf        = lt_itf ).
        CONTINUE.
      ENDIF.

      replace_section_head_with_text( CHANGING cs_itf = <ls_itf_section> ).

    ENDLOOP.

    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      EXPORTING
        lf           = 'X'
      IMPORTING
        stream_lines = lt_string
      TABLES
        itf_text     = lt_itf
        text_stream  = lt_stream.

    LOOP AT lt_string INTO lv_string.
      IF sy-tabix = 1.
        rv_result = lv_string.
      ELSE.
        CONCATENATE rv_result lv_string
                    INTO rv_result
                    SEPARATED BY cl_abap_char_utilities=>newline.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD raise.

    DATA lv_text TYPE string.

    IF iv_text IS INITIAL.
      lv_text = gc_generic_error_msg.
    ELSE.
      lv_text = iv_text.
    ENDIF.

    split_text_to_symsg( lv_text ).

    raise_t100( ix_previous = ix_previous ).

  ENDMETHOD.


  METHOD raise_t100.
    DATA: ls_t100_key TYPE scx_t100key.

    ls_t100_key-msgid = iv_msgid.
    ls_t100_key-msgno = iv_msgno.
    ls_t100_key-attr1 = 'MSGV1'.
    ls_t100_key-attr2 = 'MSGV2'.
    ls_t100_key-attr3 = 'MSGV3'.
    ls_t100_key-attr4 = 'MSGV4'.

    IF iv_msgid IS INITIAL.
      CLEAR ls_t100_key.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_abapgit_exception
      EXPORTING
        textid   = ls_t100_key
        msgv1    = iv_msgv1
        msgv2    = iv_msgv2
        msgv3    = iv_msgv3
        msgv4    = iv_msgv4
        previous = ix_previous.
  ENDMETHOD.


  METHOD raise_with_text.
    raise(
      iv_text = ix_previous->get_text( )
      ix_previous = ix_previous ).
  ENDMETHOD.


  METHOD remove_empty_section.
    IF iv_tabix_to BETWEEN iv_tabix_from AND lines( ct_itf ).
      DELETE ct_itf FROM iv_tabix_from TO iv_tabix_to.
    ELSE.
      DELETE ct_itf FROM iv_tabix_from.
    ENDIF.
  ENDMETHOD.


  METHOD replace_section_head_with_text.

    CASE cs_itf-tdline.
      WHEN gc_section_token-cause.
        cs_itf-tdline = gc_section_text-cause.
      WHEN gc_section_token-system_response.
        cs_itf-tdline = gc_section_text-system_response.
      WHEN gc_section_token-what_to_do.
        cs_itf-tdline = gc_section_text-what_to_do.
      WHEN gc_section_token-sys_admin.
        cs_itf-tdline = gc_section_text-sys_admin.
    ENDCASE.

  ENDMETHOD.


  METHOD save_callstack.

    FIELD-SYMBOLS: <ls_callstack> TYPE abap_callstack_line.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = mt_callstack.

    " You should remember that the first lines are from zcx_abapgit_exception
    " and are removed so that highest level in the callstack is the position where
    " the exception is raised.
    "
    " For the merged report it's hard to do that, because zcx_abapgit_exception
    " isn't visible in the callstack. Therefore we have to check the Events.
    LOOP AT mt_callstack ASSIGNING <ls_callstack>.

      IF <ls_callstack>-mainprogram CP |ZCX_ABAPGIT_EXCEPTION*| " full
      OR <ls_callstack>-blockname = `SAVE_CALLSTACK` " merged
      OR <ls_callstack>-blockname = `CONSTRUCTOR` " merged
      OR <ls_callstack>-blockname CP `RAISE*`. "merged
        DELETE TABLE mt_callstack FROM <ls_callstack>.
      ELSE.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD split_text_to_symsg.

    CONSTANTS:
      lc_length_of_msgv           TYPE i VALUE 50,
      lc_offset_of_last_character TYPE i VALUE 49.

    DATA:
      lv_text    TYPE c LENGTH 200,
      lv_rest    TYPE c LENGTH 200,
      ls_msg     TYPE symsg,
      lv_msg_var TYPE c LENGTH lc_length_of_msgv,
      lv_index   TYPE sy-index.

    lv_text = iv_text.

    DO 4 TIMES.

      lv_index = sy-index.

      CALL FUNCTION 'TEXT_SPLIT'
        EXPORTING
          length = lc_length_of_msgv
          text   = lv_text
        IMPORTING
          line   = lv_msg_var
          rest   = lv_rest.

      IF lv_msg_var+lc_offset_of_last_character(1) = space OR
         lv_text+lc_length_of_msgv(1) = space.
        " keep the space at the beginning of the rest
        " because otherwise it's lost
        lv_rest = | { lv_rest }|.
      ENDIF.

      lv_text = lv_rest.

      CASE lv_index.
        WHEN 1.
          ls_msg-msgv1 = lv_msg_var.
        WHEN 2.
          ls_msg-msgv2 = lv_msg_var.
        WHEN 3.
          ls_msg-msgv3 = lv_msg_var.
        WHEN 4.
          ls_msg-msgv4 = lv_msg_var.
      ENDCASE.

    ENDDO.

    " Set syst using generic error message
    MESSAGE e001(00) WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO sy-lisel.

    rs_msg = ls_msg.

  ENDMETHOD.
ENDCLASS.
CLASS zcx_abapgit_cancel DEFINITION

  INHERITING FROM zcx_abapgit_exception
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS ZCX_ABAPGIT_CANCEL IMPLEMENTATION.
ENDCLASS.
CLASS zcx_abapgit_not_found DEFINITION

  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_abapgit_not_found IMPLEMENTATION.
ENDCLASS.
"! abapinst general error
CLASS zcx_abapinst_exception DEFINITION

  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF gc_section_text,
        cause           TYPE string VALUE `Cause`,
        system_response TYPE string VALUE `System response`,
        what_to_do      TYPE string VALUE `Procedure`,
        sys_admin       TYPE string VALUE `System administration`,
      END OF gc_section_text .
    CONSTANTS:
      BEGIN OF gc_section_token,
        cause           TYPE string VALUE `&CAUSE&`,
        system_response TYPE string VALUE `&SYSTEM_RESPONSE&`,
        what_to_do      TYPE string VALUE `&WHAT_TO_DO&`,
        sys_admin       TYPE string VALUE `&SYS_ADMIN&`,
      END OF gc_section_token .
    DATA msgv1 TYPE symsgv READ-ONLY .
    DATA msgv2 TYPE symsgv READ-ONLY .
    DATA msgv3 TYPE symsgv READ-ONLY .
    DATA msgv4 TYPE symsgv READ-ONLY .
    DATA mt_callstack TYPE abap_callstack READ-ONLY .

    "! Raise exception with text
    "! @parameter iv_text | Text
    "! @parameter ix_previous | Previous exception
    "! @raising zcx_abapinst_exception | Exception
    CLASS-METHODS raise
      IMPORTING
        !iv_text     TYPE clike
        !ix_previous TYPE REF TO cx_root OPTIONAL
      RAISING
        zcx_abapinst_exception .
    "! Raise exception with T100 message
    "! <p>
    "! Will default to sy-msg* variables. These need to be set right before calling this method.
    "! </p>
    "! @parameter iv_msgid | Message ID
    "! @parameter iv_msgno | Message number
    "! @parameter iv_msgv1 | Message variable 1
    "! @parameter iv_msgv2 | Message variable 2
    "! @parameter iv_msgv3 | Message variable 3
    "! @parameter iv_msgv4 | Message variable 4
    "! @raising zcx_abapinst_exception | Exception
    CLASS-METHODS raise_t100
      IMPORTING
        VALUE(iv_msgid) TYPE symsgid DEFAULT sy-msgid
        VALUE(iv_msgno) TYPE symsgno DEFAULT sy-msgno
        VALUE(iv_msgv1) TYPE symsgv DEFAULT sy-msgv1
        VALUE(iv_msgv2) TYPE symsgv DEFAULT sy-msgv2
        VALUE(iv_msgv3) TYPE symsgv DEFAULT sy-msgv3
        VALUE(iv_msgv4) TYPE symsgv DEFAULT sy-msgv4
        !ix_previous    TYPE REF TO cx_root OPTIONAL
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS raise_with_text
      IMPORTING
        !ix_previous TYPE REF TO cx_root
      RAISING
        zcx_abapinst_exception .
    METHODS constructor
      IMPORTING
        !textid       LIKE if_t100_message=>t100key OPTIONAL
        !previous     LIKE previous OPTIONAL
        !msgv1        TYPE symsgv OPTIONAL
        !msgv2        TYPE symsgv OPTIONAL
        !msgv3        TYPE symsgv OPTIONAL
        !msgv4        TYPE symsgv OPTIONAL
        !mt_callstack TYPE abap_callstack OPTIONAL .

    METHODS get_source_position
        REDEFINITION .
    METHODS if_message~get_longtext
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS gc_generic_error_msg TYPE string VALUE 'An error occured (ZCX_ABAPINST_EXCEPTION)' ##NO_TEXT.

    CLASS-METHODS split_text_to_symsg
      IMPORTING
        !iv_text TYPE string .
    METHODS save_callstack .
    METHODS itf_to_string
      IMPORTING
        !it_itf          TYPE tline_tab
      RETURNING
        VALUE(rv_result) TYPE string .
    METHODS get_t100_longtext_itf
      RETURNING
        VALUE(rt_itf) TYPE tline_tab .
    METHODS remove_empty_section
      IMPORTING
        !iv_tabix_from TYPE i
        !iv_tabix_to   TYPE i
      CHANGING
        !ct_itf        TYPE tline_tab .
    METHODS replace_section_head_with_text
      CHANGING
        !cs_itf TYPE tline .
ENDCLASS.



CLASS zcx_abapinst_exception IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
    me->msgv3 = msgv3.
    me->msgv4 = msgv4.
    me->mt_callstack = mt_callstack.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.


  METHOD get_source_position.

    FIELD-SYMBOLS <ls_callstack> TYPE abap_callstack_line.

    READ TABLE mt_callstack ASSIGNING <ls_callstack> INDEX 1.
    IF sy-subrc = 0.
      program_name = <ls_callstack>-mainprogram.
      include_name = <ls_callstack>-include.
      source_line  = <ls_callstack>-line.
    ELSE.
      super->get_source_position(
        IMPORTING
          program_name = program_name
          include_name = include_name
          source_line  = source_line ).
    ENDIF.

  ENDMETHOD.


  METHOD get_t100_longtext_itf.

    DATA lv_docu_key TYPE doku_obj.

    FIELD-SYMBOLS <lv_msgv> TYPE any.

    lv_docu_key = if_t100_message~t100key-msgid && if_t100_message~t100key-msgno.

    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id     = 'NA'
        langu  = sy-langu
        object = lv_docu_key
        typ    = 'E'
      TABLES
        line   = rt_itf
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc = 0.
      ASSIGN me->(if_t100_message~t100key-attr1) TO <lv_msgv>.
      IF sy-subrc = 0.
        REPLACE ALL OCCURRENCES OF '&V1&' IN TABLE rt_itf WITH <lv_msgv>.
      ENDIF.
      ASSIGN me->(if_t100_message~t100key-attr2) TO <lv_msgv>.
      IF sy-subrc = 0.
        REPLACE ALL OCCURRENCES OF '&V2&' IN TABLE rt_itf WITH <lv_msgv>.
      ENDIF.
      ASSIGN me->(if_t100_message~t100key-attr3) TO <lv_msgv>.
      IF sy-subrc = 0.
        REPLACE ALL OCCURRENCES OF '&V3&' IN TABLE rt_itf WITH <lv_msgv>.
      ENDIF.
      ASSIGN me->(if_t100_message~t100key-attr4) TO <lv_msgv>.
      IF sy-subrc = 0.
        REPLACE ALL OCCURRENCES OF '&V4&' IN TABLE rt_itf WITH <lv_msgv>.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD if_message~get_longtext.

    result = super->get_longtext( ).

    IF if_t100_message~t100key IS NOT INITIAL.

      result = itf_to_string( get_t100_longtext_itf( ) ).

    ENDIF.

  ENDMETHOD.


  METHOD itf_to_string.

    CONSTANTS lc_format_section TYPE string VALUE 'U1'.

    DATA:
      lt_stream      TYPE TABLE OF tdline,
      lt_string      TYPE TABLE OF string,
      lv_string      LIKE LINE OF lt_string,
      lt_itf         TYPE tline_tab,
      lv_has_content TYPE abap_bool,
      lv_tabix_from  TYPE syst-tabix,
      lv_tabix_to    TYPE syst-tabix.

    FIELD-SYMBOLS:
      <ls_itf_section>      TYPE tline,
      <ls_itf_section_item> TYPE tline.

    lt_itf = it_itf.

    " You should remember that we replace the U1 format because
    " that preserves the section header of longtexts.
    LOOP AT lt_itf ASSIGNING <ls_itf_section> WHERE tdformat = lc_format_section.

      CLEAR:
        lv_has_content,
        lv_tabix_to.

      lv_tabix_from = sy-tabix.

      LOOP AT lt_itf ASSIGNING <ls_itf_section_item> FROM sy-tabix + 1.

        IF <ls_itf_section_item>-tdformat = lc_format_section.
          lv_tabix_to = sy-tabix.
          EXIT.
        ELSEIF <ls_itf_section_item>-tdline IS NOT INITIAL.
          lv_has_content = abap_true.
        ENDIF.

      ENDLOOP.

      IF lv_has_content = abap_false.
        remove_empty_section(
          EXPORTING
            iv_tabix_from = lv_tabix_from
            iv_tabix_to   = lv_tabix_to
          CHANGING
            ct_itf        = lt_itf ).
        CONTINUE.
      ENDIF.

      replace_section_head_with_text( CHANGING cs_itf = <ls_itf_section> ).

    ENDLOOP.

    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      EXPORTING
        lf           = abap_true
      IMPORTING
        stream_lines = lt_string
      TABLES
        itf_text     = lt_itf
        text_stream  = lt_stream.

    LOOP AT lt_string INTO lv_string.
      IF sy-tabix = 1.
        rv_result = lv_string.
      ELSE.
        CONCATENATE rv_result lv_string INTO rv_result
          SEPARATED BY cl_abap_char_utilities=>newline.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD raise.

    DATA lv_text TYPE string.

    IF iv_text IS INITIAL.
      lv_text = gc_generic_error_msg.
    ELSE.
      lv_text = iv_text.
    ENDIF.

    split_text_to_symsg( lv_text ).

    raise_t100( ix_previous = ix_previous ).

  ENDMETHOD.


  METHOD raise_t100.

    DATA ls_t100_key TYPE scx_t100key.

    ls_t100_key-msgid = iv_msgid.
    ls_t100_key-msgno = iv_msgno.
    ls_t100_key-attr1 = 'MSGV1'.
    ls_t100_key-attr2 = 'MSGV2'.
    ls_t100_key-attr3 = 'MSGV3'.
    ls_t100_key-attr4 = 'MSGV4'.

    IF iv_msgid IS INITIAL.
      CLEAR ls_t100_key.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_abapinst_exception
      EXPORTING
        textid   = ls_t100_key
        msgv1    = iv_msgv1
        msgv2    = iv_msgv2
        msgv3    = iv_msgv3
        msgv4    = iv_msgv4
        previous = ix_previous.

  ENDMETHOD.


  METHOD raise_with_text.
    raise(
      iv_text     = ix_previous->get_text( )
      ix_previous = ix_previous ).
  ENDMETHOD.


  METHOD remove_empty_section.
    IF iv_tabix_to BETWEEN iv_tabix_from AND lines( ct_itf ).
      DELETE ct_itf FROM iv_tabix_from TO iv_tabix_to.
    ELSE.
      DELETE ct_itf FROM iv_tabix_from.
    ENDIF.
  ENDMETHOD.


  METHOD replace_section_head_with_text.

    CASE cs_itf-tdline.
      WHEN gc_section_token-cause.
        cs_itf-tdline = gc_section_text-cause.
      WHEN gc_section_token-system_response.
        cs_itf-tdline = gc_section_text-system_response.
      WHEN gc_section_token-what_to_do.
        cs_itf-tdline = gc_section_text-what_to_do.
      WHEN gc_section_token-sys_admin.
        cs_itf-tdline = gc_section_text-sys_admin.
    ENDCASE.

  ENDMETHOD.


  METHOD save_callstack.

    FIELD-SYMBOLS <ls_callstack> TYPE abap_callstack_line.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = mt_callstack.

    " You should remember that the first lines are from zcx_abapinst_exception
    " and are removed so that highest level in the callstack is the position where
    " the exception is raised.
    " For the merged report it's hard to do that, because zcx_abapinst_exception
    " isn't visible in the callstack. Therefore we have to check the events.
    LOOP AT mt_callstack ASSIGNING <ls_callstack>.

      IF <ls_callstack>-mainprogram CP 'ZCX_ABAPINST_EXCEPTION*' " full
          OR <ls_callstack>-blockname = 'SAVE_CALLSTACK' " merged
          OR <ls_callstack>-blockname = 'CONSTRUCTOR' " merged
          OR <ls_callstack>-blockname CP 'RAISE*'. "merged
        DELETE TABLE mt_callstack FROM <ls_callstack>.
      ELSE.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD split_text_to_symsg.

    CONSTANTS:
      lc_length_of_msgv           TYPE i VALUE 50,
      lc_offset_of_last_character TYPE i VALUE 49.

    DATA:
      lv_text    TYPE c LENGTH 200,
      lv_rest    TYPE c LENGTH 200,
      ls_msg     TYPE symsg,
      lv_msg_var TYPE c LENGTH lc_length_of_msgv,
      lv_index   TYPE sy-index.

    lv_text = iv_text.

    DO 4 TIMES.

      lv_index = sy-index.

      CALL FUNCTION 'TEXT_SPLIT'
        EXPORTING
          length = lc_length_of_msgv
          text   = lv_text
        IMPORTING
          line   = lv_msg_var
          rest   = lv_rest.

      IF lv_msg_var+lc_offset_of_last_character(1) = space OR
         lv_text+lc_length_of_msgv(1) = space.
        " Keep the space at the beginning of the rest because otherwise it's lost
        lv_rest = | { lv_rest }|.
      ENDIF.

      lv_text = lv_rest.

      CASE lv_index.
        WHEN 1.
          ls_msg-msgv1 = lv_msg_var.
        WHEN 2.
          ls_msg-msgv2 = lv_msg_var.
        WHEN 3.
          ls_msg-msgv3 = lv_msg_var.
        WHEN 4.
          ls_msg-msgv4 = lv_msg_var.
      ENDCASE.

    ENDDO.

    " Set syst using generic error message
    MESSAGE e001(00) WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO sy-lisel.

  ENDMETHOD.
ENDCLASS.
class ZCX_AJSON_ERROR definition

  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  types:
    ty_rc type c length 4 .

  constants:
    begin of ZCX_AJSON_ERROR,
      msgid type symsgid value '00',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'A1',
      attr2 type scx_attrname value 'A2',
      attr3 type scx_attrname value 'A3',
      attr4 type scx_attrname value 'A4',
    end of ZCX_AJSON_ERROR .
  data RC type TY_RC read-only .
  data MESSAGE type STRING read-only .
  data LOCATION type STRING read-only .
  data A1 type SYMSGV read-only .
  data A2 type SYMSGV read-only .
  data A3 type SYMSGV read-only .
  data A4 type SYMSGV read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !RC type TY_RC optional
      !MESSAGE type STRING optional
      !LOCATION type STRING optional
      !A1 type SYMSGV optional
      !A2 type SYMSGV optional
      !A3 type SYMSGV optional
      !A4 type SYMSGV optional .
  class-methods RAISE
    importing
      !IV_MSG type STRING
      !IV_LOCATION type STRING optional
    raising
      ZCX_AJSON_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCX_AJSON_ERROR IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->RC = RC .
me->MESSAGE = MESSAGE .
me->LOCATION = LOCATION .
me->A1 = A1 .
me->A2 = A2 .
me->A3 = A3 .
me->A4 = A4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_AJSON_ERROR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
endmethod.


method raise.

  data:
    begin of ls_msg,
      a1 like a1,
      a2 like a1,
      a3 like a1,
      a4 like a1,
    end of ls_msg.

  if iv_location is initial.
    ls_msg = iv_msg.
  else.
    data lv_tmp type string.
    lv_tmp = iv_msg && | @{ iv_location }|.
    ls_msg = lv_tmp.
  endif.

  raise exception type zcx_ajson_error
    exporting
      textid   = zcx_ajson_error
      message  = iv_msg
      location = iv_location
      a1       = ls_msg-a1
      a2       = ls_msg-a2
      a3       = ls_msg-a3
      a4       = ls_msg-a4.

endmethod.
ENDCLASS.
CLASS zcl_abapgit_adt_link DEFINITION DEFERRED.
CLASS zcl_abapgit_convert DEFINITION DEFERRED.
CLASS zcl_abapgit_default_transport DEFINITION DEFERRED.
CLASS zcl_abapgit_dependencies DEFINITION DEFERRED.
CLASS zcl_abapgit_dot_abapgit DEFINITION DEFERRED.
CLASS zcl_abapgit_environment DEFINITION DEFERRED.
CLASS zcl_abapgit_exit DEFINITION DEFERRED.
CLASS zcl_abapgit_folder_logic DEFINITION DEFERRED.
CLASS zcl_abapgit_free_sel_dialog DEFINITION DEFERRED.
CLASS zcl_abapgit_gui_functions DEFINITION DEFERRED.
CLASS zcl_abapgit_hash DEFINITION DEFERRED.
CLASS zcl_abapgit_language DEFINITION DEFERRED.
CLASS zcl_abapgit_log DEFINITION DEFERRED.
CLASS zcl_abapgit_longtexts DEFINITION DEFERRED.
CLASS zcl_abapgit_lxe_texts DEFINITION DEFERRED.
CLASS zcl_abapgit_objects_activation DEFINITION DEFERRED.
CLASS zcl_abapgit_objects_super DEFINITION DEFERRED.
CLASS zcl_abapgit_objects_bridge DEFINITION DEFERRED.
CLASS zcl_abapgit_xml DEFINITION DEFERRED.
CLASS zcl_abapgit_xml_input DEFINITION DEFERRED.
CLASS zcl_abapgit_objects_files DEFINITION DEFERRED.
CLASS zcl_abapgit_xml_output DEFINITION DEFERRED.
CLASS zcl_abapgit_objects_program DEFINITION DEFERRED.
CLASS zcl_abapgit_object_acid DEFINITION DEFERRED.
CLASS zcl_abapgit_object_avar DEFINITION DEFERRED.
CLASS zcl_abapgit_oo_base DEFINITION DEFERRED.
CLASS zcl_abapgit_oo_class DEFINITION DEFERRED.
CLASS zcl_abapgit_object_clas DEFINITION DEFERRED.
CLASS zcl_abapgit_object_devc DEFINITION DEFERRED.
CLASS zcl_abapgit_object_doma DEFINITION DEFERRED.
CLASS zcl_abapgit_object_dsys DEFINITION DEFERRED.
CLASS zcl_abapgit_object_dtel DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enhc DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho_badi DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho_hook DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho_class DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho_intf DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho_wdyc DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho_fugr DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho_wdyn DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho_clif DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enhs_badi_d DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enhs_hook_d DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enhs DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enqu DEFINITION DEFERRED.
CLASS zcl_abapgit_object_fugr DEFINITION DEFERRED.
CLASS zcl_abapgit_object_idoc DEFINITION DEFERRED.
CLASS zcl_abapgit_object_intf DEFINITION DEFERRED.
CLASS zcl_abapgit_object_msag DEFINITION DEFERRED.
CLASS zcl_abapgit_object_nspc DEFINITION DEFERRED.
CLASS zcl_abapgit_object_para DEFINITION DEFERRED.
CLASS zcl_abapgit_object_prog DEFINITION DEFERRED.
CLASS zcl_abapgit_object_tabl_compar DEFINITION DEFERRED.
CLASS zcl_abapgit_object_tabl DEFINITION DEFERRED.
CLASS zcl_abapgit_object_tobj DEFINITION DEFERRED.
CLASS zcl_abapgit_object_tran DEFINITION DEFERRED.
CLASS zcl_abapgit_object_ttyp DEFINITION DEFERRED.
CLASS zcl_abapgit_object_w3xx_super DEFINITION DEFERRED.
CLASS zcl_abapgit_object_w3ht DEFINITION DEFERRED.
CLASS zcl_abapgit_object_w3mi DEFINITION DEFERRED.
CLASS zcl_abapgit_oo_interface DEFINITION DEFERRED.
CLASS zcl_abapgit_oo_factory DEFINITION DEFERRED.
CLASS zcl_abapgit_oo_serializer DEFINITION DEFERRED.
CLASS zcl_abapgit_path DEFINITION DEFERRED.
CLASS zcl_abapgit_progress DEFINITION DEFERRED.
CLASS zcl_abapgit_sap_package DEFINITION DEFERRED.
CLASS zcl_abapgit_skip_objects DEFINITION DEFERRED.
CLASS zcl_abapgit_sotr_handler DEFINITION DEFERRED.
CLASS zcl_abapgit_tadir DEFINITION DEFERRED.
CLASS zcl_abapgit_url DEFINITION DEFERRED.
CLASS zcl_abapgit_version DEFINITION DEFERRED.
CLASS zcl_abapgit_xml_pretty DEFINITION DEFERRED.
CLASS zcl_abapinst_factory DEFINITION DEFERRED.
CLASS zcl_abapinst_file DEFINITION DEFERRED.
CLASS zcl_abapinst_file_status DEFINITION DEFERRED.
CLASS zcl_abapinst_popups DEFINITION DEFERRED.
CLASS zcl_abapinst_installer DEFINITION DEFERRED.
CLASS zcl_abapinst_log_viewer DEFINITION DEFERRED.
CLASS zcl_abapinst_objects DEFINITION DEFERRED.
CLASS zcl_abapinst_persistence DEFINITION DEFERRED.
CLASS zcl_abapinst_screen DEFINITION DEFERRED.
CLASS zcl_abapinst_setup DEFINITION DEFERRED.
CLASS zcl_abapinst_textpool DEFINITION DEFERRED.
CLASS zcl_ajson DEFINITION DEFERRED.
CLASS zcl_ajson_mapping DEFINITION DEFERRED.
CLASS zcl_ajson_utilities DEFINITION DEFERRED.
INTERFACE zif_abapgit_definitions
   .


  TYPES:
    ty_type    TYPE c LENGTH 6 .
  TYPES:
    ty_bitbyte TYPE c LENGTH 8 .
  TYPES:
    ty_sha1    TYPE c LENGTH 40 .
  TYPES: ty_sha1_tt TYPE STANDARD TABLE OF ty_sha1 WITH DEFAULT KEY .
  TYPES:
    ty_adler32 TYPE x LENGTH 4 .
  TYPES:
    BEGIN OF ty_file_signature,
      path     TYPE string,
      filename TYPE string,
      sha1     TYPE ty_sha1,
    END OF ty_file_signature .
  TYPES:
    ty_file_signatures_tt TYPE STANDARD TABLE OF
           ty_file_signature WITH DEFAULT KEY .
  TYPES:
    ty_file_signatures_ts TYPE SORTED TABLE OF
           ty_file_signature WITH UNIQUE KEY path filename .
  TYPES:
    BEGIN OF ty_file.
      INCLUDE TYPE ty_file_signature.
  TYPES: data TYPE xstring,
    END OF ty_file .
  TYPES:
    ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY .
  TYPES:
    ty_string_tt TYPE STANDARD TABLE OF string WITH DEFAULT KEY .
  TYPES ty_git_branch_type TYPE c LENGTH 2 .
  TYPES:
    BEGIN OF ty_git_branch,
      sha1         TYPE ty_sha1,
      name         TYPE string,
      type         TYPE ty_git_branch_type,
      is_head      TYPE abap_bool,
      display_name TYPE string,
    END OF ty_git_branch .
  TYPES:
    ty_git_branch_list_tt TYPE STANDARD TABLE OF ty_git_branch WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_git_tag,
      sha1         TYPE ty_sha1,
      object       TYPE ty_sha1,
      name         TYPE string,
      type         TYPE ty_git_branch_type,
      display_name TYPE string,
      tagger_name  TYPE string,
      tagger_email TYPE string,
      message      TYPE string,
      body         TYPE string,
    END OF ty_git_tag .
  TYPES:
    ty_git_tag_list_tt TYPE STANDARD TABLE OF ty_git_tag WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_hotkey,
      ui_component TYPE string,
      action       TYPE string,
      hotkey       TYPE string,
    END OF ty_hotkey .
  TYPES:
    ty_hotkey_tt TYPE STANDARD TABLE OF ty_hotkey
                    WITH NON-UNIQUE DEFAULT KEY
                    WITH NON-UNIQUE SORTED KEY action
                         COMPONENTS ui_component action.
  TYPES:
    BEGIN OF ty_git_user,
      name  TYPE string,
      email TYPE string,
    END OF ty_git_user .
  TYPES:
    BEGIN OF ty_comment,
      committer TYPE ty_git_user,
      author    TYPE ty_git_user,
      comment   TYPE string,
    END OF ty_comment .
  TYPES:
    BEGIN OF ty_item,
      obj_type TYPE tadir-object,
      obj_name TYPE tadir-obj_name,
      devclass TYPE devclass,
      inactive TYPE abap_bool,
    END OF ty_item .
  TYPES:
    ty_items_tt TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY .
  TYPES:
    ty_items_ts TYPE SORTED TABLE OF ty_item WITH UNIQUE KEY obj_type obj_name .
  TYPES:
    BEGIN OF ty_file_item,
      file TYPE ty_file,
      item TYPE ty_item,
    END OF ty_file_item .
  TYPES:
    ty_files_item_tt TYPE STANDARD TABLE OF ty_file_item WITH DEFAULT KEY .
  TYPES:
    ty_yes_no         TYPE c LENGTH 1,
    ty_yes_no_partial TYPE c LENGTH 1.
  TYPES:
    BEGIN OF ty_overwrite.
      INCLUDE TYPE ty_item.
  TYPES: decision TYPE ty_yes_no,
    END OF ty_overwrite .
  TYPES:
    ty_overwrite_tt TYPE STANDARD TABLE OF ty_overwrite WITH DEFAULT KEY
                              WITH UNIQUE HASHED KEY object_type_and_name
                                   COMPONENTS obj_type obj_name .
  TYPES:
    BEGIN OF ty_requirements,
      met      TYPE ty_yes_no,
      decision TYPE ty_yes_no,
    END OF ty_requirements .
  TYPES:
    BEGIN OF ty_dependencies,
      met TYPE ty_yes_no,
    END OF ty_dependencies .
  TYPES:
    BEGIN OF ty_transport_type,
      request TYPE trfunction,
      task    TYPE trfunction,
    END OF ty_transport_type .
  TYPES:
    BEGIN OF ty_transport,
      required  TYPE abap_bool,
      transport TYPE trkorr,
      type      TYPE ty_transport_type,
    END OF ty_transport .
  TYPES:
    BEGIN OF ty_deserialize_checks,
      overwrite       TYPE ty_overwrite_tt,
      warning_package TYPE ty_overwrite_tt,
      requirements    TYPE ty_requirements,
      dependencies    TYPE ty_dependencies,
      transport       TYPE ty_transport,
    END OF ty_deserialize_checks .
  TYPES:
    BEGIN OF ty_delete_checks,
      transport TYPE ty_transport,
    END OF ty_delete_checks .
  TYPES:
    BEGIN OF ty_metadata,
      class        TYPE string,
      version      TYPE string,
      delete_tadir TYPE abap_bool,
      ddic         TYPE abap_bool,
    END OF ty_metadata .
  TYPES:
    BEGIN OF ty_repo_file,
      path       TYPE string,
      filename   TYPE string,
      is_changed TYPE abap_bool,
      rstate     TYPE c LENGTH 1,
      lstate     TYPE c LENGTH 1,
    END OF ty_repo_file .
  TYPES:
    ty_repo_file_tt TYPE STANDARD TABLE OF ty_repo_file WITH DEFAULT KEY .
  TYPES:
    ty_chmod TYPE c LENGTH 6 .
  TYPES:
    BEGIN OF ty_object,
      sha1    TYPE ty_sha1,
      type    TYPE ty_type,
      data    TYPE xstring,
      adler32 TYPE ty_adler32,
      index   TYPE i,
    END OF ty_object .
  TYPES:
    ty_objects_tt TYPE STANDARD TABLE OF ty_object WITH DEFAULT KEY
      WITH NON-UNIQUE SORTED KEY sha COMPONENTS sha1
      WITH NON-UNIQUE SORTED KEY type COMPONENTS type sha1 .
  TYPES:
    BEGIN OF ty_tadir,
      pgmid    TYPE tadir-pgmid,
      object   TYPE tadir-object,
      obj_name TYPE tadir-obj_name,
      devclass TYPE tadir-devclass,
      korrnum  TYPE tadir-korrnum,
      delflag  TYPE tadir-delflag,
      genflag  TYPE tadir-genflag,
      path     TYPE string,
    END OF ty_tadir .
  TYPES:
    ty_tadir_tt TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_result,
      obj_type TYPE tadir-object,
      obj_name TYPE tadir-obj_name,
      inactive TYPE abap_bool,
      path     TYPE string,
      filename TYPE string,
      package  TYPE devclass,
      match    TYPE abap_bool,
      lstate   TYPE c LENGTH 1,
      rstate   TYPE c LENGTH 1,
      packmove TYPE abap_bool,
    END OF ty_result .
  TYPES:
    ty_results_tt TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY .
  TYPES:
    ty_results_ts_path TYPE HASHED TABLE OF ty_result WITH UNIQUE KEY path filename .
  TYPES:
    BEGIN OF ty_stage_files,
      local  TYPE ty_files_item_tt,
      remote TYPE ty_files_tt,
      status TYPE ty_results_ts_path,
    END OF ty_stage_files .
  TYPES:
    BEGIN OF ty_tpool.
      INCLUDE TYPE textpool.
  TYPES: split TYPE c LENGTH 8.
  TYPES: END OF ty_tpool .
  TYPES:
    ty_tpool_tt TYPE STANDARD TABLE OF ty_tpool WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_sotr,
      header  TYPE sotr_head,
      entries TYPE sotr_text_tt,
    END OF ty_sotr .
  TYPES:
    ty_sotr_tt TYPE STANDARD TABLE OF ty_sotr WITH DEFAULT KEY .
  TYPES:
    ty_sotr_use_tt TYPE STANDARD TABLE OF sotr_use WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_obj_attribute,
      cmpname   TYPE seocmpname,
      attkeyfld TYPE seokeyfld,
      attbusobj TYPE seobusobj,
    END OF ty_obj_attribute .
  TYPES:
    ty_obj_attribute_tt TYPE STANDARD TABLE OF ty_obj_attribute WITH DEFAULT KEY
                             WITH NON-UNIQUE SORTED KEY cmpname COMPONENTS cmpname .
  TYPES:
    BEGIN OF ty_transport_to_branch,
      branch_name TYPE string,
      commit_text TYPE string,
    END OF ty_transport_to_branch .
  TYPES:
    BEGIN OF ty_create,
      name   TYPE string,
      parent TYPE string,
    END OF ty_create .
  TYPES:
    BEGIN OF ty_commit,
      sha1       TYPE ty_sha1,
      parent1    TYPE ty_sha1,
      parent2    TYPE ty_sha1,
      author     TYPE string,
      email      TYPE string,
      time       TYPE string,
      message    TYPE string,
      body       TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      branch     TYPE string,
      merge      TYPE string,
      tags       TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      create     TYPE STANDARD TABLE OF ty_create WITH DEFAULT KEY,
      compressed TYPE abap_bool,
    END OF ty_commit .
  TYPES:
    ty_commit_tt TYPE STANDARD TABLE OF ty_commit WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_diff,
      patch_flag TYPE abap_bool,
      new_num    TYPE c LENGTH 6,
      new        TYPE string,
      result     TYPE c LENGTH 1,
      old_num    TYPE c LENGTH 6,
      old        TYPE string,
      short      TYPE abap_bool,
      beacon     TYPE i,
    END OF ty_diff .
  TYPES:
    ty_diffs_tt TYPE STANDARD TABLE OF ty_diff WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_count,
      insert TYPE i,
      delete TYPE i,
      update TYPE i,
    END OF ty_count .
  TYPES:
    BEGIN OF ty_expanded,
      path  TYPE string,
      name  TYPE string,
      sha1  TYPE ty_sha1,
      chmod TYPE ty_chmod,
    END OF ty_expanded .
  TYPES:
    ty_expanded_tt TYPE STANDARD TABLE OF ty_expanded WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_ancestor,
      commit TYPE ty_sha1,
      tree   TYPE ty_sha1,
      time   TYPE string,
      body   TYPE string,
    END OF ty_ancestor .
  TYPES:
    BEGIN OF ty_repo_item,
      obj_type TYPE tadir-object,
      obj_name TYPE tadir-obj_name,
      inactive TYPE abap_bool,
      sortkey  TYPE i,
      path     TYPE string,
      is_dir   TYPE abap_bool,
      changes  TYPE i,
      lstate   TYPE c LENGTH 1,
      rstate   TYPE c LENGTH 1,
      files    TYPE ty_repo_file_tt,
    END OF ty_repo_item .
  TYPES:
    ty_repo_item_tt TYPE STANDARD TABLE OF ty_repo_item WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_s_user_settings,
      max_lines              TYPE i,
      adt_jump_enabled       TYPE abap_bool,
      show_default_repo      TYPE abap_bool,
      link_hints_enabled     TYPE abap_bool,
      link_hint_key          TYPE c LENGTH 1,
      hotkeys                TYPE ty_hotkey_tt,
      parallel_proc_disabled TYPE abap_bool,
      icon_scaling           TYPE c LENGTH 1,
      ui_theme               TYPE string,
      hide_sapgui_hint       TYPE abap_bool,
      activate_wo_popup      TYPE abap_bool,
    END OF ty_s_user_settings .
  TYPES:
    ty_dokil_tt TYPE STANDARD TABLE OF dokil
                         WITH NON-UNIQUE DEFAULT KEY .
  TYPES:
    BEGIN OF ty_col_spec,
      tech_name      TYPE string,
      display_name   TYPE string,
      css_class      TYPE string,
      add_tz         TYPE abap_bool,
      title          TYPE string,
      allow_order_by TYPE abap_bool,
    END OF ty_col_spec,
    ty_col_spec_tt TYPE STANDARD TABLE OF ty_col_spec
                      WITH NON-UNIQUE KEY tech_name.
  TYPES:
    ty_proxy_bypass_url       TYPE c LENGTH 255,
    ty_range_proxy_bypass_url TYPE RANGE OF ty_proxy_bypass_url.
  TYPES:
    BEGIN OF ty_version,
      major           TYPE i,
      minor           TYPE i,
      patch           TYPE i,
      prerelase       TYPE string,
      prerelase_patch TYPE i,
    END OF ty_version.
  TYPES: BEGIN OF ty_alv_column,
           name   TYPE string,
           text   TYPE string,
           length TYPE lvc_outlen,
         END OF ty_alv_column,
         ty_alv_column_tt TYPE TABLE OF ty_alv_column WITH DEFAULT KEY.
  TYPES:
    ty_deserialization_step TYPE string.
  TYPES:
    ty_deserialization_step_tt TYPE STANDARD TABLE OF ty_deserialization_step
                                          WITH DEFAULT KEY .
  TYPES:
    ty_object_type_range TYPE RANGE OF trobjtype,
    ty_object_name_range TYPE RANGE OF sobj_name.
  CONSTANTS:
    BEGIN OF c_git_branch_type,
      branch          TYPE ty_git_branch_type VALUE 'HD',
      lightweight_tag TYPE ty_git_branch_type VALUE 'TG',
      annotated_tag   TYPE ty_git_branch_type VALUE 'AT',
      other           TYPE ty_git_branch_type VALUE 'ZZ',
    END OF c_git_branch_type .
  CONSTANTS c_head_name TYPE string VALUE 'HEAD' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF c_git_branch,
      master       TYPE string VALUE 'refs/heads/master',
      prefix       TYPE string VALUE 'refs/',
      heads_prefix TYPE string VALUE 'refs/heads/',
      heads        TYPE string VALUE 'refs/heads/*',
      tags_prefix  TYPE string VALUE 'refs/tags/',
      tags         TYPE string VALUE 'refs/tags/*',
    END OF c_git_branch.
  CONSTANTS:
    BEGIN OF c_diff,
      insert TYPE c LENGTH 1 VALUE 'I',
      delete TYPE c LENGTH 1 VALUE 'D',
      update TYPE c LENGTH 1 VALUE 'U',
    END OF c_diff .
  CONSTANTS:
    BEGIN OF c_type,
      commit TYPE ty_type VALUE 'commit',                   "#EC NOTEXT
      tree   TYPE ty_type VALUE 'tree',                     "#EC NOTEXT
      ref_d  TYPE ty_type VALUE 'ref_d',                    "#EC NOTEXT
      tag    TYPE ty_type VALUE 'tag',                      "#EC NOTEXT
      blob   TYPE ty_type VALUE 'blob',                     "#EC NOTEXT
    END OF c_type .
  CONSTANTS:
    BEGIN OF c_state, " https://git-scm.com/docs/git-status
      unchanged TYPE c LENGTH 1 VALUE '',
      added     TYPE c LENGTH 1 VALUE 'A',
      modified  TYPE c LENGTH 1 VALUE 'M',
      deleted   TYPE c LENGTH 1 VALUE 'D',
      mixed     TYPE c LENGTH 1 VALUE '*',
    END OF c_state .
  CONSTANTS:
    BEGIN OF c_chmod,
      file       TYPE ty_chmod VALUE '100644',
      executable TYPE ty_chmod VALUE '100755',
      dir        TYPE ty_chmod VALUE '40000 ',
    END OF c_chmod .
  CONSTANTS c_crlf TYPE c LENGTH 2 VALUE cl_abap_char_utilities=>cr_lf ##NO_TEXT.
  CONSTANTS c_newline TYPE c LENGTH 1 VALUE cl_abap_char_utilities=>newline ##NO_TEXT.
  CONSTANTS c_english TYPE spras VALUE 'E' ##NO_TEXT.
  CONSTANTS c_root_dir TYPE string VALUE '/' ##NO_TEXT.
  CONSTANTS c_dot_abapgit TYPE string VALUE '.abapgit.xml' ##NO_TEXT.
  CONSTANTS c_author_regex TYPE string VALUE '^(.+) <(.*)> (\d{10})\s?.\d{4}$' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF c_action,
      repo_refresh                  TYPE string VALUE 'repo_refresh',
      repo_remove                   TYPE string VALUE 'repo_remove',
      repo_settings                 TYPE string VALUE 'repo_settings',
      repo_local_settings           TYPE string VALUE 'repo_local_settings',
      repo_switch                   TYPE string VALUE 'repo_switch',
      repo_packaging                TYPE string VALUE 'repo_packaging',
      repo_infos                    TYPE string VALUE 'repo_infos',
      repo_purge                    TYPE string VALUE 'repo_purge',
      repo_newonline                TYPE string VALUE 'repo_newonline',
      repo_newoffline               TYPE string VALUE 'repo_newoffline',
      repo_add_all_obj_to_trans_req TYPE string VALUE 'repo_add_all_obj_to_trans_req',
      repo_remote_attach            TYPE string VALUE 'repo_remote_attach',
      repo_remote_detach            TYPE string VALUE 'repo_remote_detach',
      repo_remote_change            TYPE string VALUE 'repo_remote_change',
      repo_refresh_checksums        TYPE string VALUE 'repo_refresh_checksums',
      repo_toggle_fav               TYPE string VALUE 'repo_toggle_fav',
      repo_transport_to_branch      TYPE string VALUE 'repo_transport_to_branch',
      repo_syntax_check             TYPE string VALUE 'repo_syntax_check',
      repo_code_inspector           TYPE string VALUE 'repo_code_inspector',
      repo_open_in_master_lang      TYPE string VALUE 'repo_open_in_master_lang',
      repo_log                      TYPE string VALUE 'repo_log',
      abapgit_home                  TYPE string VALUE 'abapgit_home',
      zip_import                    TYPE string VALUE 'zip_import',
      zip_export                    TYPE string VALUE 'zip_export',
      zip_package                   TYPE string VALUE 'zip_package',
      zip_transport                 TYPE string VALUE 'zip_transport',
      zip_object                    TYPE string VALUE 'zip_object',
      performance_test              TYPE string VALUE 'performance_test',
      ie_devtools                   TYPE string VALUE 'ie_devtools',
      git_pull                      TYPE string VALUE 'git_pull',
      git_reset                     TYPE string VALUE 'git_reset',
      git_checkout_commit           TYPE string VALUE 'git_checkout_commit',
      git_branch_create             TYPE string VALUE 'git_branch_create',
      git_branch_switch             TYPE string VALUE 'git_branch_switch',
      git_branch_delete             TYPE string VALUE 'git_branch_delete',
      git_tag_create                TYPE string VALUE 'git_tag_create',
      git_tag_delete                TYPE string VALUE 'git_tag_delete',
      git_tag_switch                TYPE string VALUE 'git_tag_switch',
      git_commit                    TYPE string VALUE 'git_commit',
      db_display                    TYPE string VALUE 'db_display',
      db_edit                       TYPE string VALUE 'db_edit',
      bg_update                     TYPE string VALUE 'bg_update',
      go_explore                    TYPE string VALUE 'go_explore',
      go_repo                       TYPE string VALUE 'go_repo',
      go_db                         TYPE string VALUE 'go_db',
      go_background                 TYPE string VALUE 'go_background',
      go_background_run             TYPE string VALUE 'go_background_run',
      go_repo_diff                  TYPE string VALUE 'go_repo_diff',
      go_file_diff                  TYPE string VALUE 'go_fill_diff',
      go_stage                      TYPE string VALUE 'go_stage',
      go_commit                     TYPE string VALUE 'go_commit',
      go_branch_overview            TYPE string VALUE 'go_branch_overview',
      go_tag_overview               TYPE string VALUE 'go_tag_overview',
      go_debuginfo                  TYPE string VALUE 'go_debuginfo',
      go_settings                   TYPE string VALUE 'go_settings',
      go_settings_personal          TYPE string VALUE 'go_settings_personal',
      go_tutorial                   TYPE string VALUE 'go_tutorial',
      go_patch                      TYPE string VALUE 'go_patch',
      jump                          TYPE string VALUE 'jump',
      jump_transport                TYPE string VALUE 'jump_transport',
      jump_user                     TYPE string VALUE 'jump_user',
      url                           TYPE string VALUE 'url',
      goto_source                   TYPE string VALUE 'goto_source',
      show_callstack                TYPE string VALUE 'show_callstack',
      change_order_by               TYPE string VALUE 'change_order_by',
      goto_message                  TYPE string VALUE 'goto_message',
      direction                     TYPE string VALUE 'direction',
      documentation                 TYPE string VALUE 'documentation',
      changelog                     TYPE string VALUE 'changelog',
    END OF c_action.
  CONSTANTS c_spagpa_param_repo_key TYPE c LENGTH 20 VALUE 'REPO_KEY' ##NO_TEXT.
  CONSTANTS c_spagpa_param_package TYPE c LENGTH 20 VALUE 'PACKAGE' ##NO_TEXT.
  CONSTANTS gc_yes TYPE ty_yes_no VALUE 'Y'.
  CONSTANTS gc_no TYPE ty_yes_no VALUE 'N'.
  CONSTANTS gc_partial TYPE ty_yes_no_partial VALUE 'P'.

  TYPES:
    ty_method TYPE c LENGTH 1 .
  TYPES:
    BEGIN OF ty_stage,
      file   TYPE ty_file,
      method TYPE ty_method,
      status TYPE ty_result,
    END OF ty_stage .
  TYPES:
    ty_stage_tt TYPE SORTED TABLE OF ty_stage
          WITH UNIQUE KEY file-path file-filename .

  CONSTANTS:
    BEGIN OF c_method,
      add    TYPE ty_method VALUE 'A',
      rm     TYPE ty_method VALUE 'R',
      ignore TYPE ty_method VALUE 'I',
      skip   TYPE ty_method VALUE '?',
    END OF c_method .

  TYPES:
    ty_languages TYPE STANDARD TABLE OF laiso WITH DEFAULT KEY.
  TYPES:
    BEGIN OF ty_i18n_params,
      main_language         TYPE sy-langu,
      main_language_only    TYPE abap_bool,
      translation_languages TYPE ty_languages,
    END OF ty_i18n_params .

ENDINTERFACE.
INTERFACE zif_abapgit_sap_package
   .


  TYPES:
    ty_devclass_tt TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY .

  METHODS create
    IMPORTING
      !is_package TYPE scompkdtln
    RAISING
      zcx_abapgit_exception .
  METHODS create_local
    RAISING
      zcx_abapgit_exception .
  METHODS list_subpackages
    RETURNING
      VALUE(rt_list) TYPE ty_devclass_tt .
  METHODS list_superpackages
    RETURNING
      VALUE(rt_list) TYPE ty_devclass_tt
    RAISING
      zcx_abapgit_exception .
  METHODS read_parent
    RETURNING
      VALUE(rv_parentcl) TYPE tdevc-parentcl
    RAISING
      zcx_abapgit_exception .
  METHODS create_child
    IMPORTING
      !iv_child TYPE devclass
    RAISING
      zcx_abapgit_exception .
  METHODS exists
    RETURNING
      VALUE(rv_bool) TYPE abap_bool .
  METHODS are_changes_recorded_in_tr_req
    RETURNING
      VALUE(rv_are_changes_rec_in_tr_req) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS get_transport_type
    RETURNING
      VALUE(rs_transport_type) TYPE zif_abapgit_definitions=>ty_transport_type
    RAISING
      zcx_abapgit_exception .
  METHODS get_transport_layer
    RETURNING
      VALUE(rv_transport_layer) TYPE devlayer
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
INTERFACE zif_abapgit_dot_abapgit .

  TYPES:
    BEGIN OF ty_requirement,
      component   TYPE dlvunit,
      min_release TYPE saprelease,
      min_patch   TYPE sappatchlv,
    END OF ty_requirement .
  TYPES:
    ty_requirement_tt TYPE STANDARD TABLE OF ty_requirement WITH DEFAULT KEY .

  " Former APACK
  TYPES:
    BEGIN OF ty_dependency,
      name           TYPE string,
      version        TYPE string,
      sem_version    TYPE zif_abapgit_definitions=>ty_version,
      git_url        TYPE string,
      target_package TYPE devclass,
    END OF ty_dependency,

    ty_dependencies TYPE STANDARD TABLE OF ty_dependency WITH NON-UNIQUE DEFAULT KEY,

    BEGIN OF ty_descriptor,
      name              TYPE string,
      version           TYPE string,
      sem_version       TYPE zif_abapgit_definitions=>ty_version,
      description       TYPE string,
      git_url           TYPE string,
      target_package    TYPE devclass,
    END OF ty_descriptor,

    BEGIN OF ty_packaging.
      INCLUDE TYPE ty_descriptor.
    TYPES:
      dependencies TYPE ty_dependencies,
    END OF ty_packaging.

  TYPES:
    BEGIN OF ty_dot_abapgit,
      master_language TYPE spras,
      i18n_languages  TYPE zif_abapgit_definitions=>ty_languages,
      starting_folder TYPE string,
      folder_logic    TYPE string,
      ignore          TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      requirements    TYPE ty_requirement_tt,
      packaging       TYPE ty_packaging,
    END OF ty_dot_abapgit .

  CONSTANTS:
    BEGIN OF c_folder_logic,
      prefix TYPE string VALUE 'PREFIX',
      full   TYPE string VALUE 'FULL',
    END OF c_folder_logic .

ENDINTERFACE.
INTERFACE zif_abapgit_environment
  .
  METHODS is_sap_cloud_platform
    RETURNING
      VALUE(rv_result) TYPE abap_bool.
  METHODS is_merged
    RETURNING
      VALUE(rv_result) TYPE abap_bool.
  METHODS is_repo_object_changes_allowed
    RETURNING
      VALUE(rv_result) TYPE abap_bool.
  METHODS compare_with_inactive
    RETURNING
      VALUE(rv_result) TYPE abap_bool.
  METHODS is_restart_required
    RETURNING
      VALUE(rv_result) TYPE abap_bool.
  METHODS is_sap_object_allowed
    RETURNING
      VALUE(rv_allowed) TYPE abap_bool.
ENDINTERFACE.
INTERFACE zif_abapgit_log
   .


  TYPES:
    BEGIN OF ty_log_out,
      type      TYPE sy-msgty,
      text      TYPE string,
      obj_type  TYPE tadir-object,
      obj_name  TYPE tadir-obj_name,
      exception TYPE REF TO cx_root,
    END OF ty_log_out .
  TYPES:
    ty_log_outs TYPE STANDARD TABLE OF ty_log_out
                WITH NON-UNIQUE DEFAULT KEY .
  TYPES:
    BEGIN OF ty_msg,
      text TYPE string,
      type TYPE sy-msgty,
    END OF ty_msg .
  TYPES:
    ty_msgs TYPE STANDARD TABLE OF ty_msg
                          WITH NON-UNIQUE DEFAULT KEY .
  TYPES:
    BEGIN OF ty_item_status_out,
      item     TYPE zif_abapgit_definitions=>ty_item,
      status   TYPE sy-msgty,
      messages TYPE ty_msgs,
    END OF ty_item_status_out .
  TYPES:
    ty_item_status_outs TYPE SORTED TABLE OF ty_item_status_out
                        WITH UNIQUE KEY item-obj_type item-obj_name .

  METHODS add
    IMPORTING
      !iv_msg  TYPE csequence
      !iv_type TYPE sy-msgty DEFAULT 'E'
      !iv_rc   TYPE sy-subrc OPTIONAL
      !is_item TYPE zif_abapgit_definitions=>ty_item OPTIONAL
      !ix_exc  TYPE REF TO cx_root OPTIONAL .
  METHODS add_error
    IMPORTING
      !iv_msg  TYPE csequence
      !is_item TYPE zif_abapgit_definitions=>ty_item OPTIONAL .
  METHODS add_info
    IMPORTING
      !iv_msg  TYPE csequence
      !is_item TYPE zif_abapgit_definitions=>ty_item OPTIONAL .
  METHODS add_warning
    IMPORTING
      !iv_msg  TYPE csequence
      !is_item TYPE zif_abapgit_definitions=>ty_item OPTIONAL .
  METHODS add_success
    IMPORTING
      !iv_msg  TYPE csequence
      !is_item TYPE zif_abapgit_definitions=>ty_item OPTIONAL .
  METHODS add_exception
    IMPORTING
      !ix_exc  TYPE REF TO cx_root
      !is_item TYPE zif_abapgit_definitions=>ty_item OPTIONAL .
  METHODS clear .
  METHODS count
    RETURNING
      VALUE(rv_count) TYPE i .
  METHODS has_rc
    IMPORTING
      !iv_rc        TYPE sy-subrc
    RETURNING
      VALUE(rv_yes) TYPE abap_bool .
  METHODS get_messages
    RETURNING
      VALUE(rt_msg) TYPE ty_log_outs .
  METHODS get_item_status
    RETURNING
      VALUE(rt_item_status) TYPE ty_item_status_outs .
  METHODS get_status
    RETURNING
      VALUE(rv_status) TYPE sy-msgty .
  METHODS get_title
    RETURNING
      VALUE(rv_title) TYPE string .
  METHODS set_title
    IMPORTING
      !iv_title TYPE csequence .
ENDINTERFACE.
INTERFACE zif_abapgit_longtexts
   .


  METHODS changed_by
    IMPORTING
      !iv_object_name TYPE sobj_name
      !iv_longtext_id TYPE dokil-id
      !it_dokil       TYPE zif_abapgit_definitions=>ty_dokil_tt OPTIONAL
    RETURNING
      VALUE(rv_user)  TYPE xubname
    RAISING
      zcx_abapgit_exception .
  METHODS serialize
    IMPORTING
      !iv_longtext_name TYPE string DEFAULT 'LONGTEXTS'
      !iv_object_name   TYPE sobj_name
      !iv_longtext_id   TYPE dokil-id
      !it_dokil         TYPE zif_abapgit_definitions=>ty_dokil_tt OPTIONAL
      !ii_xml           TYPE REF TO zif_abapgit_xml_output
    RAISING
      zcx_abapgit_exception .
  METHODS deserialize
    IMPORTING
      !iv_longtext_name TYPE string DEFAULT 'LONGTEXTS'
      !ii_xml           TYPE REF TO zif_abapgit_xml_input
      !iv_main_language TYPE langu
    RAISING
      zcx_abapgit_exception .
  METHODS delete
    IMPORTING
      !iv_object_name TYPE sobj_name
      !iv_longtext_id TYPE dokil-id
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
INTERFACE zif_abapgit_lxe_texts
   .

  TYPES:
    BEGIN OF ty_lxe_i18n,
      source_lang TYPE lxeisolang,
      target_lang TYPE lxeisolang,
      custmnr     TYPE lxecustmnr,
      objtype     TYPE trobjtype,
      objname     TYPE lxeobjname,
      text_pairs  TYPE STANDARD TABLE OF lxe_pcx_s1 WITH DEFAULT KEY,
    END OF ty_lxe_i18n .
  TYPES:
    ty_tlxe_i18n TYPE STANDARD TABLE OF ty_lxe_i18n WITH DEFAULT KEY .

  METHODS serialize
    IMPORTING
      !iv_lxe_text_name TYPE string DEFAULT 'LXE_TEXTS'
      !iv_object_type   TYPE trobjtype
      !iv_object_name   TYPE sobj_name
      !ii_xml           TYPE REF TO zif_abapgit_xml_output
    RAISING
      zcx_abapgit_exception .
  METHODS deserialize
    IMPORTING
      !iv_lxe_text_name TYPE string DEFAULT 'LXE_TEXTS'
      !iv_object_type   TYPE trobjtype OPTIONAL
      !iv_object_name   TYPE sobj_name OPTIONAL
      !ii_xml           TYPE REF TO zif_abapgit_xml_input
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
INTERFACE zif_abapgit_object
   .

  DATA mo_files TYPE REF TO zcl_abapgit_objects_files .

  CONSTANTS:
    BEGIN OF gc_step_id,
      abap TYPE zif_abapgit_definitions=>ty_deserialization_step VALUE `ABAP`,
      ddic TYPE zif_abapgit_definitions=>ty_deserialization_step VALUE `DDIC`,
      late TYPE zif_abapgit_definitions=>ty_deserialization_step VALUE `LATE`,
    END OF gc_step_id.

  CONSTANTS c_abap_version_sap_cp TYPE progdir-uccheck VALUE '5' ##NO_TEXT.
  CONSTANTS c_abap_version_default TYPE progdir-uccheck VALUE 'X' ##NO_TEXT.

  METHODS serialize
    IMPORTING
      !io_xml TYPE REF TO zif_abapgit_xml_output
    RAISING
      zcx_abapgit_exception .
  METHODS deserialize
    IMPORTING
      !iv_package TYPE devclass
      !io_xml     TYPE REF TO zif_abapgit_xml_input
      !iv_step    TYPE zif_abapgit_definitions=>ty_deserialization_step
      !ii_log     TYPE REF TO zif_abapgit_log
    RAISING
      zcx_abapgit_exception .
  METHODS delete
    IMPORTING
      iv_package TYPE devclass
    RAISING
      zcx_abapgit_exception .
  METHODS exists
    RETURNING
      VALUE(rv_bool) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS is_locked
    RETURNING
      VALUE(rv_is_locked) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS is_active
    RETURNING
      VALUE(rv_active) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS changed_by
    RETURNING
      VALUE(rv_user) TYPE xubname
    RAISING
      zcx_abapgit_exception .
  METHODS jump
    RAISING
      zcx_abapgit_exception .
  METHODS get_metadata
    RETURNING
      VALUE(rs_metadata) TYPE zif_abapgit_definitions=>ty_metadata .
  METHODS get_comparator
    RETURNING
      VALUE(ri_comparator) TYPE REF TO zif_abapgit_comparator
    RAISING
      zcx_abapgit_exception .
  METHODS get_deserialize_steps
    RETURNING
      VALUE(rt_steps) TYPE zif_abapgit_definitions=>ty_deserialization_step_tt .
ENDINTERFACE.
INTERFACE zif_abapgit_xml_input
   .
  METHODS read
    IMPORTING
      !iv_name TYPE clike
    CHANGING
      !cg_data TYPE any
    RAISING
      zcx_abapgit_exception .
  METHODS get_raw
    RETURNING
      VALUE(ri_raw) TYPE REF TO if_ixml_document .
* todo, add read_xml to match add_xml in lcl_xml_output
  METHODS get_metadata
    RETURNING
      VALUE(rs_metadata) TYPE zif_abapgit_definitions=>ty_metadata .
ENDINTERFACE.
INTERFACE zif_abapgit_xml_output
   .


  METHODS add
    IMPORTING
      !iv_name TYPE clike
      !ig_data TYPE any
    RAISING
      zcx_abapgit_exception .
  METHODS set_raw
    IMPORTING
      !ii_raw TYPE REF TO if_ixml_element .
  METHODS add_xml
    IMPORTING
      !iv_name TYPE clike
      !ii_xml  TYPE REF TO if_ixml_element .
  METHODS render
    IMPORTING
      !iv_normalize TYPE abap_bool DEFAULT abap_true
      !is_metadata  TYPE zif_abapgit_definitions=>ty_metadata OPTIONAL
    RETURNING
      VALUE(rv_xml) TYPE string .
  METHODS i18n_params
    IMPORTING
      !is_i18n_params       TYPE zif_abapgit_definitions=>ty_i18n_params OPTIONAL
    RETURNING
      VALUE(rs_i18n_params) TYPE zif_abapgit_definitions=>ty_i18n_params .
ENDINTERFACE.
INTERFACE zif_abapgit_lang_definitions
   .

  TYPES: BEGIN OF ty_i18n_tpool,
           language TYPE langu,
           textpool TYPE zif_abapgit_definitions=>ty_tpool_tt,
         END OF ty_i18n_tpool,
         ty_i18n_tpools TYPE STANDARD TABLE OF ty_i18n_tpool.

  TYPES: BEGIN OF ty_i18n_line,
           language TYPE langu,
           lines    TYPE tlinetab,
         END OF ty_i18n_line,
         ty_i18n_lines TYPE STANDARD TABLE OF ty_i18n_line.

  TYPES: ty_langus TYPE STANDARD TABLE OF langu.

ENDINTERFACE.
INTERFACE zif_abapgit_oo_object_fnc .

  CONSTANTS:
    BEGIN OF c_parts,
      locals_def  TYPE string VALUE 'locals_def',
      locals_imp  TYPE string VALUE 'locals_imp',
      macros      TYPE string VALUE 'macros',
      testclasses TYPE string VALUE 'testclasses',
    END OF c_parts.

  TYPES: BEGIN OF ty_includes,
           programm TYPE programm,
         END OF ty_includes,
         ty_includes_tt TYPE STANDARD TABLE OF ty_includes WITH DEFAULT KEY.

  TYPES:
    ty_seocompotx_tt TYPE STANDARD TABLE OF seocompotx WITH DEFAULT KEY .

  METHODS:
    create
      IMPORTING
        iv_package    TYPE devclass
        iv_overwrite  TYPE abap_bool DEFAULT abap_true
        it_attributes TYPE zif_abapgit_definitions=>ty_obj_attribute_tt OPTIONAL
      CHANGING
        cg_properties TYPE any
      RAISING
        zcx_abapgit_exception,
    generate_locals
      IMPORTING
        is_key                   TYPE seoclskey
        it_local_definitions     TYPE seop_source_string OPTIONAL
        it_local_implementations TYPE seop_source_string OPTIONAL
        it_local_macros          TYPE seop_source_string OPTIONAL
        it_local_test_classes    TYPE seop_source_string OPTIONAL
      RAISING
        zcx_abapgit_exception,
    deserialize_source
      IMPORTING
        is_key    TYPE seoclskey
        it_source TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING
        zcx_abapgit_exception
        cx_sy_dyn_call_error,
    insert_text_pool
      IMPORTING
        iv_class_name TYPE seoclsname
        it_text_pool  TYPE textpool_table
        iv_language   TYPE spras
        iv_state      TYPE c DEFAULT 'I'
      RAISING
        zcx_abapgit_exception,
    update_descriptions
      IMPORTING
        is_key          TYPE seoclskey
        it_descriptions TYPE ty_seocompotx_tt,
    add_to_activation_list
      IMPORTING
        is_item TYPE zif_abapgit_definitions=>ty_item
      RAISING
        zcx_abapgit_exception,
    create_sotr
      IMPORTING
        iv_object_name TYPE sobj_name
        iv_package     TYPE devclass
        ii_xml         TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception,
    create_documentation
      IMPORTING
        it_lines         TYPE tlinetab
        iv_object_name   TYPE dokhl-object
        iv_language      TYPE spras
        iv_no_masterlang TYPE abap_bool OPTIONAL
      RAISING
        zcx_abapgit_exception,
    get_includes
      IMPORTING
        iv_object_name     TYPE sobj_name
      RETURNING
        VALUE(rt_includes) TYPE ty_includes_tt
      RAISING
        zcx_abapgit_exception,
    exists
      IMPORTING
        is_object_name   TYPE seoclskey
      RETURNING
        VALUE(rv_exists) TYPE abap_bool,
    serialize_abap
      IMPORTING
        is_class_key     TYPE seoclskey
        iv_type          TYPE seop_include_ext_app OPTIONAL
      RETURNING
        VALUE(rt_source) TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING
        zcx_abapgit_exception
        cx_sy_dyn_call_error,
    get_skip_test_classes
      RETURNING
        VALUE(rv_skip) TYPE abap_bool,
    get_class_properties
      IMPORTING
        is_class_key               TYPE seoclskey
      RETURNING
        VALUE(rs_class_properties) TYPE vseoclass
      RAISING
        zcx_abapgit_exception,
    get_interface_properties
      IMPORTING
        is_interface_key               TYPE seoclskey
      RETURNING
        VALUE(rs_interface_properties) TYPE vseointerf
      RAISING
        zcx_abapgit_exception,
    read_text_pool
      IMPORTING
        iv_class_name       TYPE seoclsname
        iv_language         TYPE spras
      RETURNING
        VALUE(rt_text_pool) TYPE textpool_table,
    read_documentation
      IMPORTING
        iv_class_name   TYPE seoclsname
        iv_language     TYPE spras
      RETURNING
        VALUE(rt_lines) TYPE tlinetab,
    read_sotr
      IMPORTING
        iv_object_name TYPE sobj_name
        ii_xml         TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception,
    read_descriptions
      IMPORTING
        iv_obejct_name         TYPE seoclsname
        iv_language            TYPE spras OPTIONAL
      RETURNING
        VALUE(rt_descriptions) TYPE ty_seocompotx_tt,
    delete
      IMPORTING
        is_deletion_key TYPE seoclskey
      RAISING
        zcx_abapgit_exception,
    read_superclass
      IMPORTING
        iv_classname         TYPE seoclsname
      RETURNING
        VALUE(rv_superclass) TYPE seoclsname,
    read_attributes
      IMPORTING
        iv_object_name       TYPE seoclsname
      RETURNING
        VALUE(rt_attributes) TYPE zif_abapgit_definitions=>ty_obj_attribute_tt.
ENDINTERFACE.
INTERFACE zif_abapgit_comparator
   .


  TYPES:
    BEGIN OF ty_result,
      text TYPE string,
    END OF ty_result .

  METHODS compare
    IMPORTING
      !ii_remote       TYPE REF TO zif_abapgit_xml_input
      !ii_log          TYPE REF TO zif_abapgit_log
    RETURNING
      VALUE(rs_result) TYPE ty_result
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
INTERFACE zif_abapgit_progress
   .


  METHODS show
    IMPORTING
      !iv_current TYPE i
      !iv_text    TYPE csequence .
  METHODS set_total
    IMPORTING
      !iv_total TYPE i .
  METHODS off .
ENDINTERFACE.
INTERFACE zif_abapgit_tadir
   .


  METHODS get_object_package
    IMPORTING
      !iv_pgmid          TYPE tadir-pgmid DEFAULT 'R3TR'
      !iv_object         TYPE tadir-object
      !iv_obj_name       TYPE tadir-obj_name
    RETURNING
      VALUE(rv_devclass) TYPE tadir-devclass
    RAISING
      zcx_abapgit_exception .
  METHODS read
    IMPORTING
      !iv_package            TYPE tadir-devclass
      !iv_ignore_subpackages TYPE abap_bool DEFAULT abap_false
      !iv_only_local_objects TYPE abap_bool DEFAULT abap_false
      !io_dot                TYPE REF TO zcl_abapgit_dot_abapgit OPTIONAL
      !ii_log                TYPE REF TO zif_abapgit_log OPTIONAL
    RETURNING
      VALUE(rt_tadir)        TYPE zif_abapgit_definitions=>ty_tadir_tt
    RAISING
      zcx_abapgit_exception .
  METHODS read_single
    IMPORTING
      !iv_pgmid       TYPE tadir-pgmid DEFAULT 'R3TR'
      !iv_object      TYPE tadir-object
      !iv_obj_name    TYPE tadir-obj_name
    RETURNING
      VALUE(rs_tadir) TYPE zif_abapgit_definitions=>ty_tadir.
ENDINTERFACE.
INTERFACE zif_abapinst_definitions
   .

  CONSTANTS c_version TYPE string VALUE '1.0.0' ##NO_TEXT.

  CONSTANTS c_tabname TYPE tabname VALUE 'ZABAPINST' ##NO_TEXT.
  CONSTANTS c_lock TYPE viewname VALUE 'EZABAPINST' ##NO_TEXT.
  CONSTANTS c_english TYPE sy-langu VALUE 'E' ##NO_TEXT.
  CONSTANTS c_prog_developer TYPE progname VALUE 'ZABAPINST_DEV' ##NO_TEXT.
  CONSTANTS c_prog_standalone TYPE progname VALUE 'ZABAPINST' ##NO_TEXT.

  CONSTANTS c_url_docs TYPE string VALUE 'https://github.com/abapGit/abapinst' ##NO_TEXT.
  CONSTANTS c_url_license TYPE string VALUE 'https://github.com/abapGit/abapinst/blob/master/LICENSE' ##NO_TEXT.
  CONSTANTS c_url_repo TYPE string VALUE 'https://github.com/abapGit/abapinst' ##NO_TEXT.

  " Avoids warning due to key length
  CONSTANTS c_name_length TYPE i VALUE 90 ##NO_TEXT.

  TYPES:
    ty_name TYPE c LENGTH c_name_length .
  TYPES ty_pack TYPE devclass .
  TYPES:
    ty_base     TYPE c LENGTH 80 .
  TYPES:
    ty_base_tab TYPE TABLE OF ty_base .
  TYPES:
    BEGIN OF ty_content,
      name TYPE ty_name,
      pack TYPE ty_pack,
      json TYPE string,
    END OF ty_content .
  TYPES:
    ty_contents TYPE SORTED TABLE OF ty_content WITH UNIQUE KEY name pack .
  TYPES:
    BEGIN OF ty_version,
      major           TYPE i,
      minor           TYPE i,
      patch           TYPE i,
      prerelase       TYPE string,
      prerelase_patch TYPE i,
    END OF ty_version .
  TYPES:
    BEGIN OF ty_inst,
      name            TYPE ty_name,
      pack            TYPE devclass,
      version         TYPE string,
      sem_version     TYPE ty_version,
      status          TYPE icon_d,
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
    END OF ty_inst .
  TYPES:
    ty_list TYPE STANDARD TABLE OF ty_inst WITH KEY name pack .


ENDINTERFACE.
INTERFACE zif_abapgit_objects
  .

  TYPES:
    BEGIN OF ty_serialization,
      files TYPE zif_abapgit_definitions=>ty_files_tt,
      item  TYPE zif_abapgit_definitions=>ty_item,
    END OF ty_serialization .
  TYPES:
    BEGIN OF ty_deserialization,
      obj     TYPE REF TO zif_abapgit_object,
      xml     TYPE REF TO zif_abapgit_xml_input,
      package TYPE devclass,
      item    TYPE zif_abapgit_definitions=>ty_item,
    END OF ty_deserialization .
  TYPES:
    ty_deserialization_tt TYPE STANDARD TABLE OF ty_deserialization WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_step_data,
      step_id      TYPE zif_abapgit_definitions=>ty_deserialization_step,
      order        TYPE i,
      descr        TYPE string,
      is_ddic      TYPE abap_bool,
      syntax_check TYPE abap_bool,
      objects      TYPE ty_deserialization_tt,
    END OF ty_step_data .
  TYPES:
    ty_step_data_tt TYPE STANDARD TABLE OF ty_step_data
                                WITH DEFAULT KEY .

ENDINTERFACE.
interface zif_ajson_reader
   .

  methods exists
    importing
      iv_path type string
    returning
      value(rv_exists) type abap_bool.
  methods members
    importing
      iv_path type string
    returning
      value(rt_members) type string_table.
  methods get
    importing
      iv_path type string
    returning
      value(rv_value) type string.
  methods get_node_type
    importing
      iv_path type string
    returning
      value(rv_node_type) type string.
  methods get_boolean
    importing
      iv_path type string
    returning
      value(rv_value) type abap_bool.
  methods get_integer
    importing
      iv_path type string
    returning
      value(rv_value) type i.
  methods get_number
    importing
      iv_path type string
    returning
      value(rv_value) type f.
  methods get_date
    importing
      iv_path type string
    returning
      value(rv_value) type d.
  methods get_string
    importing
      iv_path type string
    returning
      value(rv_value) type string.
  methods slice
    importing
      iv_path type string
    returning
      value(ri_json) type ref to zif_ajson_reader.
  methods to_abap
    exporting
      ev_container type any
    raising
      zcx_ajson_error.
  methods array_to_string_table
    importing
      iv_path type string
    returning
      value(rt_string_table) type string_table
    raising
      zcx_ajson_error.

endinterface.
interface zif_ajson_writer
   .

  methods clear
    raising
      zcx_ajson_error.

  methods set
    importing
      iv_path type string
      iv_val type any
      iv_ignore_empty type abap_bool default abap_true
      iv_node_type type string optional
    raising
      zcx_ajson_error.

  methods set_boolean
    importing
      iv_path type string
      iv_val type any
    raising
      zcx_ajson_error.

  methods set_string
    importing
      iv_path type string
      iv_val type clike
    raising
      zcx_ajson_error.

  methods set_integer
    importing
      iv_path type string
      iv_val type i
    raising
      zcx_ajson_error.

  methods set_date
    importing
      iv_path type string
      iv_val type d
    raising
      zcx_ajson_error.

  methods set_null
    importing
      iv_path type string
    raising
      zcx_ajson_error.

  methods delete
    importing
      iv_path type string
    raising
      zcx_ajson_error.

  methods touch_array
    importing
      iv_path type string
      iv_clear type abap_bool default abap_false
    raising
      zcx_ajson_error.

  methods push
    importing
      iv_path type string
      iv_val type any
    raising
      zcx_ajson_error.

  methods stringify
    importing
      iv_indent type i default 0
    returning
      value(rv_json) type string
    raising
      zcx_ajson_error.

endinterface.
interface zif_ajson
   .

  constants version type string value 'v1.0.3'.
  constants origin type string value 'https://github.com/sbcgua/ajson'.

  interfaces zif_ajson_reader.
  interfaces zif_ajson_writer.

  constants:
    begin of node_type,
      boolean type string value 'bool',
      string  type string value 'str',
      number  type string value 'num',
      null    type string value 'null',
      array   type string value 'array',
      object  type string value 'object',
    end of node_type.

  types:
    begin of ty_node,
      path type string,
      name type string,
      type type string,
      value type string,
      index type i,
      order type i,
      children type i,
    end of ty_node .
  types:
    ty_nodes_tt type standard table of ty_node with key path name .
  types:
    ty_nodes_ts type sorted table of ty_node
      with unique key path name
      with non-unique sorted key array_index components path index
      with non-unique sorted key item_order components path order .
  types:
    begin of ty_path_name,
      path type string,
      name type string,
    end of ty_path_name.

  " DATA

  data mt_json_tree type ty_nodes_ts read-only.

  " METHODS

  methods freeze.
  methods keep_item_order.

  " METHODS (merged from reader/writer), maybe will completely move to this IF in future !

  aliases:
    exists for zif_ajson_reader~exists,
    members for zif_ajson_reader~members,
    get for zif_ajson_reader~get,
    get_boolean for zif_ajson_reader~get_boolean,
    get_integer for zif_ajson_reader~get_integer,
    get_number for zif_ajson_reader~get_number,
    get_date for zif_ajson_reader~get_date,
    get_string for zif_ajson_reader~get_string,
    slice for zif_ajson_reader~slice,
    to_abap for zif_ajson_reader~to_abap,
    array_to_string_table for zif_ajson_reader~array_to_string_table.

  aliases:
    clear for zif_ajson_writer~clear,
    set for zif_ajson_writer~set,
    set_boolean for zif_ajson_writer~set_boolean,
    set_string for zif_ajson_writer~set_string,
    set_integer for zif_ajson_writer~set_integer,
    set_date for zif_ajson_writer~set_date,
    set_null for zif_ajson_writer~set_null,
    delete for zif_ajson_writer~delete,
    touch_array for zif_ajson_writer~touch_array,
    push for zif_ajson_writer~push,
    stringify for zif_ajson_writer~stringify.

endinterface.
interface zif_ajson_mapping
  .

  types:
    begin of ty_mapping_field,
      abap type string,
      json type string,
    end of ty_mapping_field,
    ty_mapping_fields type standard table of ty_mapping_field
      with unique sorted key abap components abap
      with unique sorted key json components json.

  methods to_abap
    importing
      !iv_path         type string
      !iv_name         type string
    returning
      value(rv_result) type string.

  methods to_json
    importing
      !iv_path         type string
      !iv_name         type string
    returning
      value(rv_result) type string.

endinterface.
INTERFACE zif_abapgit_persistence .

  TYPES:
    ty_type  TYPE c LENGTH 12 .
  TYPES:
    ty_value TYPE c LENGTH 12 .
  TYPES:
    BEGIN OF ty_content,
      type     TYPE ty_type,
      value    TYPE ty_value,
      data_str TYPE string,
    END OF ty_content .
  TYPES:
    ty_contents TYPE SORTED TABLE OF ty_content WITH UNIQUE KEY type value .

  TYPES: BEGIN OF ty_local_checksum,
           item  TYPE zif_abapgit_definitions=>ty_item,
           files TYPE zif_abapgit_definitions=>ty_file_signatures_tt,
         END OF ty_local_checksum.

  TYPES:
    BEGIN OF ty_local_settings,
      display_name                 TYPE string,
      ignore_subpackages           TYPE abap_bool,
      write_protected              TYPE abap_bool,
      only_local_objects           TYPE abap_bool,
      code_inspector_check_variant TYPE sci_chkv,
      block_commit                 TYPE abap_bool,
      serialize_master_lang_only   TYPE abap_bool,
    END OF ty_local_settings.

  TYPES: ty_local_checksum_tt TYPE STANDARD TABLE OF ty_local_checksum WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_repo_xml,
           url             TYPE string,
           branch_name     TYPE string,
           selected_commit TYPE zif_abapgit_definitions=>ty_sha1,
           package         TYPE devclass,
           created_by      TYPE xubname,
           created_at      TYPE timestampl,
           deserialized_by TYPE xubname,
           deserialized_at TYPE timestampl,
           offline         TYPE abap_bool,
           switched_origin TYPE string,
           local_checksums TYPE ty_local_checksum_tt,
           dot_abapgit     TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit,
           head_branch     TYPE string,   " HEAD symref of the repo, master branch
           local_settings  TYPE ty_local_settings,
         END OF ty_repo_xml.

  TYPES:
    BEGIN OF ty_repo_meta_mask,
      url             TYPE abap_bool,
      branch_name     TYPE abap_bool,
      selected_commit TYPE abap_bool,
      package         TYPE abap_bool,
      created_by      TYPE abap_bool,
      created_at      TYPE abap_bool,
      deserialized_by TYPE abap_bool,
      deserialized_at TYPE abap_bool,
      offline         TYPE abap_bool,
      switched_origin TYPE abap_bool,
      local_checksums TYPE abap_bool,
      dot_abapgit     TYPE abap_bool,
      head_branch     TYPE abap_bool,
      local_settings  TYPE abap_bool,
    END OF ty_repo_meta_mask.

  TYPES: BEGIN OF ty_repo,
           key TYPE ty_value.
      INCLUDE TYPE ty_repo_xml.
  TYPES: END OF ty_repo.
  TYPES: ty_repos TYPE STANDARD TABLE OF ty_repo WITH DEFAULT KEY.
  TYPES: ty_repo_keys TYPE STANDARD TABLE OF ty_repo-key WITH DEFAULT KEY.

ENDINTERFACE.
INTERFACE zif_abapgit_exit
   .


  TYPES:
    ty_icm_sinfo2_tt TYPE STANDARD TABLE OF icm_sinfo2 WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_ci_repo,
      name      TYPE string,
      clone_url TYPE string,
    END OF ty_ci_repo .
  TYPES:
    ty_ci_repos TYPE TABLE OF ty_ci_repo .

  METHODS change_local_host
    CHANGING
      !ct_hosts TYPE ty_icm_sinfo2_tt .
  METHODS allow_sap_objects
    RETURNING
      VALUE(rv_allowed) TYPE abap_bool .
  METHODS change_proxy_url
    IMPORTING
      !iv_repo_url  TYPE csequence
    CHANGING
      !cv_proxy_url TYPE string .
  METHODS change_proxy_port
    IMPORTING
      !iv_repo_url   TYPE csequence
    CHANGING
      !cv_proxy_port TYPE string .
  METHODS change_proxy_authentication
    IMPORTING
      !iv_repo_url             TYPE csequence
    CHANGING
      !cv_proxy_authentication TYPE abap_bool .
  METHODS create_http_client
    IMPORTING
      !iv_url          TYPE string
    RETURNING
      VALUE(ri_client) TYPE REF TO if_http_client
    RAISING
      zcx_abapgit_exception .
  METHODS http_client
    IMPORTING
      !iv_url    TYPE string
      !ii_client TYPE REF TO if_http_client .
  METHODS change_tadir
    IMPORTING
      !iv_package TYPE devclass
      !ii_log     TYPE REF TO zif_abapgit_log
    CHANGING
      !ct_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt .
  METHODS get_ssl_id
    RETURNING
      VALUE(rv_ssl_id) TYPE ssfapplssl .
  METHODS custom_serialize_abap_clif
    IMPORTING
      !is_class_key    TYPE seoclskey
    RETURNING
      VALUE(rt_source) TYPE zif_abapgit_definitions=>ty_string_tt
    RAISING
      zcx_abapgit_exception .
  METHODS deserialize_postprocess
    IMPORTING
      !is_step TYPE zif_abapgit_objects=>ty_step_data
      !ii_log  TYPE REF TO zif_abapgit_log .
  METHODS get_ci_tests
    IMPORTING
      !iv_object   TYPE tadir-object
    CHANGING
      !ct_ci_repos TYPE ty_ci_repos .
  METHODS adjust_display_commit_url
    IMPORTING
      !iv_repo_url    TYPE csequence
      !iv_repo_name   TYPE csequence
      !iv_repo_key    TYPE csequence
      !iv_commit_hash TYPE zif_abapgit_definitions=>ty_sha1
    CHANGING
      !cv_display_url TYPE csequence
    RAISING
      zcx_abapgit_exception .
  METHODS pre_calculate_repo_status
    IMPORTING
      is_repo_meta TYPE zif_abapgit_persistence=>ty_repo
    CHANGING
      !ct_local  TYPE zif_abapgit_definitions=>ty_files_item_tt
      !ct_remote TYPE zif_abapgit_definitions=>ty_files_tt
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
INTERFACE zif_abapgit_gui_functions
  .

  METHODS:
    gui_is_available
      RETURNING
        VALUE(rv_gui_is_available) TYPE abap_bool,

    is_sapgui_for_java
      RETURNING
        VALUE(rv_result) TYPE abap_bool,

    is_sapgui_for_windows
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

ENDINTERFACE.
INTERFACE zif_abapgit_object_enho .

  METHODS:
    deserialize
      IMPORTING ii_xml     TYPE REF TO zif_abapgit_xml_input
                iv_package TYPE devclass
      RAISING   zcx_abapgit_exception,
    serialize
      IMPORTING ii_xml      TYPE REF TO zif_abapgit_xml_output
                ii_enh_tool TYPE REF TO if_enh_tool
      RAISING   zcx_abapgit_exception.

ENDINTERFACE.
INTERFACE zif_abapgit_object_enhs .

  METHODS:
    deserialize
      IMPORTING ii_xml           TYPE REF TO zif_abapgit_xml_input
                iv_package       TYPE devclass
                ii_enh_spot_tool TYPE REF TO if_enh_spot_tool
      RAISING   zcx_abapgit_exception,

    serialize
      IMPORTING ii_xml           TYPE REF TO zif_abapgit_xml_output
                ii_enh_spot_tool TYPE REF TO if_enh_spot_tool
      RAISING   zcx_abapgit_exception.

ENDINTERFACE.
INTERFACE zif_abapgit_version
   .

  CONSTANTS gc_xml_version TYPE string VALUE 'v1.0.0' ##NO_TEXT.
  CONSTANTS gc_abap_version TYPE string VALUE '1.106.0' ##NO_TEXT.

ENDINTERFACE.
CLASS zcl_abapgit_adt_link DEFINITION

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS generate
      IMPORTING
        !iv_obj_name     TYPE zif_abapgit_definitions=>ty_item-obj_name
        !iv_obj_type     TYPE zif_abapgit_definitions=>ty_item-obj_type
        !iv_sub_obj_name TYPE zif_abapgit_definitions=>ty_item-obj_name OPTIONAL
        !iv_line_number  TYPE i OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE string
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
      get_adt_objects_and_names
        IMPORTING
          iv_obj_name       TYPE zif_abapgit_definitions=>ty_item-obj_name
          iv_obj_type       TYPE zif_abapgit_definitions=>ty_item-obj_type
        EXPORTING
          eo_adt_uri_mapper TYPE REF TO object
          eo_adt_objectref  TYPE REF TO object
          ev_program        TYPE progname
          ev_include        TYPE progname
        RAISING
          zcx_abapgit_exception.

    CLASS-METHODS:
      is_adt_jump_possible
        IMPORTING io_object                      TYPE REF TO cl_wb_object
                  io_adt                         TYPE REF TO object
        RETURNING VALUE(rv_is_adt_jump_possible) TYPE abap_bool
        RAISING   zcx_abapgit_exception.
ENDCLASS.
CLASS zcl_abapgit_convert DEFINITION

  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS bitbyte_to_int
      IMPORTING
        !iv_bits      TYPE clike
      RETURNING
        VALUE(rv_int) TYPE i .
    CLASS-METHODS x_to_bitbyte
      IMPORTING
        !iv_x             TYPE x
      RETURNING
        VALUE(rv_bitbyte) TYPE zif_abapgit_definitions=>ty_bitbyte .
    CLASS-METHODS string_to_xstring_utf8
      IMPORTING
        !iv_string        TYPE string
      RETURNING
        VALUE(rv_xstring) TYPE xstring .
    CLASS-METHODS xstring_to_string_utf8
      IMPORTING
        !iv_data         TYPE xsequence
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS string_to_xstring_utf8_bom
      IMPORTING
        !iv_string        TYPE string
      RETURNING
        VALUE(rv_xstring) TYPE xstring .
    CLASS-METHODS xstring_to_int
      IMPORTING
        !iv_xstring TYPE xstring
      RETURNING
        VALUE(rv_i) TYPE i
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS int_to_xstring4
      IMPORTING
        !iv_i             TYPE i
      RETURNING
        VALUE(rv_xstring) TYPE xstring .
    CLASS-METHODS split_string
      IMPORTING
        !iv_string      TYPE string
      RETURNING
        VALUE(rt_lines) TYPE string_table .
    CLASS-METHODS conversion_exit_isola_output
      IMPORTING
        !iv_spras       TYPE spras
      RETURNING
        VALUE(rv_spras) TYPE laiso .
    CLASS-METHODS string_to_xstring
      IMPORTING
        !iv_str        TYPE string
      RETURNING
        VALUE(rv_xstr) TYPE xstring .
    CLASS-METHODS string_to_tab
      IMPORTING
        !iv_str  TYPE string
      EXPORTING
        !ev_size TYPE i
        !et_tab  TYPE STANDARD TABLE .
    CLASS-METHODS base64_to_xstring
      IMPORTING
        !iv_base64     TYPE string
      RETURNING
        VALUE(rv_xstr) TYPE xstring .
    CLASS-METHODS xstring_to_bintab
      IMPORTING
        !iv_xstr   TYPE xstring
      EXPORTING
        !ev_size   TYPE i
        !et_bintab TYPE STANDARD TABLE .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_convert_out TYPE REF TO cl_abap_conv_out_ce .
    CLASS-DATA go_convert_in TYPE REF TO cl_abap_conv_in_ce .
ENDCLASS.
CLASS zcl_abapgit_default_transport DEFINITION

  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(ro_instance) TYPE REF TO zcl_abapgit_default_transport
        RAISING
          zcx_abapgit_exception.

    METHODS:
      constructor
        RAISING
          zcx_abapgit_exception,

      set
        IMPORTING
          iv_transport TYPE trkorr
        RAISING
          zcx_abapgit_exception,

      reset
        RAISING
          zcx_abapgit_exception,
      get
        RETURNING
          VALUE(rs_default_task) TYPE e070use
        RAISING
          zcx_abapgit_exception .


  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_instance TYPE REF TO zcl_abapgit_default_transport .
    DATA mv_is_set_by_abapgit TYPE abap_bool .
    DATA ms_save TYPE e070use .

    METHODS store
      RAISING
        zcx_abapgit_exception .
    METHODS restore
      RAISING
        zcx_abapgit_exception .
    METHODS set_internal
      IMPORTING
        !iv_transport TYPE trkorr
      RAISING
        zcx_abapgit_exception .
    METHODS clear
      IMPORTING
        !is_default_task TYPE e070use
      RAISING
        zcx_abapgit_exception .
ENDCLASS.
CLASS zcl_abapgit_dependencies DEFINITION

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS resolve
      CHANGING
        !ct_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_dependency,
        depname  TYPE dd02l-tabname,
        deptyp   TYPE c LENGTH 4,
        deplocal TYPE dd02l-as4local,
        refname  TYPE dd02l-tabname,
        reftyp   TYPE c LENGTH 4,
        kind     TYPE c LENGTH 1,
      END OF ty_dependency .
    TYPES:
      ty_dedenpencies TYPE STANDARD TABLE OF ty_dependency
                                 WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      BEGIN OF ty_item,
        obj_type TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        devclass TYPE devclass,
      END OF ty_item .

    CLASS-METHODS resolve_ddic
      CHANGING
        !ct_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_ddls_dependencies
      IMPORTING
        iv_ddls_name         TYPE tadir-obj_name
      RETURNING
        VALUE(rt_dependency) TYPE ty_dedenpencies.
    CLASS-METHODS resolve_packages
      CHANGING
        ct_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.
ENDCLASS.
CLASS zcl_abapgit_dot_abapgit DEFINITION

  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS build_default
      RETURNING
        VALUE(ro_dot_abapgit) TYPE REF TO zcl_abapgit_dot_abapgit .
    CLASS-METHODS deserialize
      IMPORTING
        !iv_xstr              TYPE xstring
      RETURNING
        VALUE(ro_dot_abapgit) TYPE REF TO zcl_abapgit_dot_abapgit
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      IMPORTING
        !is_data TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit .
    METHODS serialize
      RETURNING
        VALUE(rv_xstr) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    METHODS to_file
      RETURNING
        VALUE(rs_file) TYPE zif_abapgit_definitions=>ty_file
      RAISING
        zcx_abapgit_exception.
    METHODS get_data
      RETURNING
        VALUE(rs_data) TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit .
    METHODS add_ignore
      IMPORTING
        !iv_path     TYPE string
        !iv_filename TYPE string .
    METHODS is_ignored
      IMPORTING
        !iv_path          TYPE string
        !iv_filename      TYPE string
      RETURNING
        VALUE(rv_ignored) TYPE abap_bool .
    METHODS remove_ignore
      IMPORTING
        !iv_path     TYPE string
        !iv_filename TYPE string .
    METHODS get_starting_folder
      RETURNING
        VALUE(rv_path) TYPE string .
    METHODS get_folder_logic
      RETURNING
        VALUE(rv_logic) TYPE string .
    METHODS set_folder_logic
      IMPORTING
        !iv_logic TYPE string .
    METHODS set_starting_folder
      IMPORTING
        !iv_path TYPE string .
    METHODS get_master_language
      RETURNING
        VALUE(rv_language) TYPE spras .
    METHODS get_main_language
      RETURNING
        VALUE(rv_language) TYPE spras .
    METHODS get_i18n_languages
      RETURNING
        VALUE(rt_languages) TYPE zif_abapgit_definitions=>ty_languages
      RAISING
        zcx_abapgit_exception .
    METHODS set_i18n_languages
      IMPORTING
        VALUE(it_languages) TYPE zif_abapgit_definitions=>ty_languages
      RAISING
        zcx_abapgit_exception .
    METHODS get_signature
      RETURNING
        VALUE(rs_signature) TYPE zif_abapgit_definitions=>ty_file_signature
      RAISING
        zcx_abapgit_exception .
    METHODS get_requirements
      RETURNING
        VALUE(rt_requirements) TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt .
    METHODS set_requirements
      IMPORTING
        !it_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt .
    METHODS get_packaging
      RETURNING
        VALUE(rt_packaging) TYPE zif_abapgit_dot_abapgit=>ty_packaging .
    METHODS set_packaging
      IMPORTING
        !it_packaging TYPE zif_abapgit_dot_abapgit=>ty_packaging .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: ms_data TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit.

    CLASS-METHODS:
      to_xml
        IMPORTING is_data       TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit
        RETURNING VALUE(rv_xml) TYPE string
        RAISING   zcx_abapgit_exception,
      from_xml
        IMPORTING iv_xml         TYPE string
        RETURNING VALUE(rs_data) TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit.

ENDCLASS.
CLASS zcl_abapgit_environment DEFINITION

  FINAL
  CREATE PRIVATE
   FRIENDS zcl_abapinst_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_environment .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_cloud TYPE abap_bool VALUE abap_undefined ##NO_TEXT.
    DATA mv_is_merged TYPE abap_bool VALUE abap_undefined ##NO_TEXT.
    DATA mv_client_modifiable TYPE abap_bool VALUE abap_undefined ##NO_TEXT.
ENDCLASS.
CLASS zcl_abapgit_exit DEFINITION

  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING VALUE(ri_exit) TYPE REF TO zif_abapgit_exit.

    INTERFACES: zif_abapgit_exit.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_exit TYPE REF TO zif_abapgit_exit .
ENDCLASS.
CLASS zcl_abapgit_folder_logic DEFINITION

  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS package_to_path
      IMPORTING
        !iv_top        TYPE devclass
        !io_dot        TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_package    TYPE devclass
      RETURNING
        VALUE(rv_path) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS path_to_package
      IMPORTING
        !iv_top                  TYPE devclass
        !io_dot                  TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_path                 TYPE string
        !iv_create_if_not_exists TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rv_package)        TYPE devclass
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_folder_logic .
  PROTECTED SECTION.

    METHODS get_parent
      IMPORTING
        !iv_package      TYPE devclass
      RETURNING
        VALUE(rv_parent) TYPE devclass
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_devclass_info,
        devclass  TYPE devclass,
        namespace TYPE namespace,
        parentcl  TYPE parentcl,
      END OF ty_devclass_info .
    TYPES:
      ty_devclass_info_tt TYPE SORTED TABLE OF ty_devclass_info
        WITH UNIQUE KEY devclass .
    DATA mt_parent TYPE ty_devclass_info_tt .
ENDCLASS.
"! Free Selections Dialog
CLASS zcl_abapgit_free_sel_dialog DEFINITION

  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_free_sel_field,
        name             TYPE fieldname,
        only_parameter   TYPE abap_bool,
        param_obligatory TYPE abap_bool,
        value            TYPE string,
        value_range      TYPE rsds_selopt_t,
        ddic_tabname     TYPE tabname,
        ddic_fieldname   TYPE fieldname,
        text             TYPE rsseltext,
      END OF ty_free_sel_field,
      ty_free_sel_field_tab TYPE STANDARD TABLE OF ty_free_sel_field WITH DEFAULT KEY.

    TYPES: ty_syst_title TYPE c LENGTH 70.

    METHODS:
      constructor IMPORTING iv_title      TYPE ty_syst_title OPTIONAL
                            iv_frame_text TYPE ty_syst_title OPTIONAL,
      set_fields CHANGING ct_fields TYPE ty_free_sel_field_tab,
      show RAISING zcx_abapgit_cancel
                   zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      ty_field_text_tab TYPE STANDARD TABLE OF rsdstexts WITH DEFAULT KEY.
    METHODS:
      convert_input_fields EXPORTING et_default_values TYPE rsds_trange
                                     es_restriction    TYPE sscr_restrict_ds
                                     et_fields         TYPE rsdsfields_t
                                     et_field_texts    TYPE ty_field_text_tab,
      free_selections_init IMPORTING it_default_values TYPE rsds_trange
                                     is_restriction    TYPE sscr_restrict_ds
                           EXPORTING ev_selection_id   TYPE dynselid
                           CHANGING  ct_fields         TYPE rsdsfields_t
                                     ct_field_texts    TYPE ty_field_text_tab
                           RAISING   zcx_abapgit_exception,
      free_selections_dialog IMPORTING iv_selection_id  TYPE dynselid
                             EXPORTING et_result_ranges TYPE rsds_trange
                             CHANGING  ct_fields        TYPE rsdsfields_t
                             RAISING   zcx_abapgit_cancel
                                       zcx_abapgit_exception,
      validate_results IMPORTING it_result_ranges TYPE rsds_trange
                       RAISING   zcx_abapgit_exception,
      transfer_results_to_input IMPORTING it_result_ranges TYPE rsds_trange.
    DATA:
      mr_fields     TYPE REF TO ty_free_sel_field_tab,
      mv_title      TYPE ty_syst_title,
      mv_frame_text TYPE ty_syst_title.
ENDCLASS.
CLASS zcl_abapgit_gui_functions DEFINITION

  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_gui_functions.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS zcl_abapgit_hash DEFINITION

  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS adler32
      IMPORTING
        !iv_xstring        TYPE xstring
      RETURNING
        VALUE(rv_checksum) TYPE zif_abapgit_definitions=>ty_adler32 .
    CLASS-METHODS sha1
      IMPORTING
        !iv_type       TYPE zif_abapgit_definitions=>ty_type
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS sha1_commit
      IMPORTING
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS sha1_tree
      IMPORTING
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS sha1_tag
      IMPORTING
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS sha1_blob
      IMPORTING
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .


    CLASS-METHODS sha1_raw
      IMPORTING
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
*----------------------------------------------------------------------*
* This helper class is used to set and restore the current language.
* As some of the SAP functions used rely on SY-LANGU containing the
* main language, this class is used to temporarily change and then
* restore the value of SY-LANGU.
*----------------------------------------------------------------------*
CLASS zcl_abapgit_language DEFINITION

  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor .
    CLASS-METHODS restore_login_language .
    CLASS-METHODS set_current_language
      IMPORTING
        !iv_language TYPE langu .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gv_login_language TYPE langu .
ENDCLASS.
CLASS zcl_abapgit_log DEFINITION

  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_log .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_msg,
        text TYPE string,
        type TYPE sy-msgty,
      END OF ty_msg .
    TYPES:
      BEGIN OF ty_log, "in order of occurrence
        msg       TYPE ty_msg,
        rc        TYPE sy-subrc,
        item      TYPE zif_abapgit_definitions=>ty_item,
        exception TYPE REF TO cx_root,
      END OF ty_log .

    DATA:
      mt_log TYPE STANDARD TABLE OF ty_log WITH DEFAULT KEY .
    DATA mv_title TYPE string .

    METHODS get_messages_status
      IMPORTING
        !it_msg          TYPE zif_abapgit_log=>ty_msgs
      RETURNING
        VALUE(rv_status) TYPE sy-msgty .
  PRIVATE SECTION.
ENDCLASS.
CLASS zcl_abapgit_longtexts DEFINITION

  CREATE PRIVATE
   FRIENDS zcl_abapinst_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_longtexts .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_longtext,
        dokil TYPE dokil,
        head  TYPE thead,
        lines TYPE tline_tab,
      END OF ty_longtext .
    TYPES:
      ty_longtexts TYPE STANDARD TABLE OF ty_longtext WITH NON-UNIQUE DEFAULT KEY .

    METHODS read
      IMPORTING
        !iv_object_name     TYPE sobj_name
        !iv_longtext_id     TYPE dokil-id
        !it_dokil           TYPE zif_abapgit_definitions=>ty_dokil_tt
        !iv_main_lang_only  TYPE abap_bool DEFAULT abap_false
        !iv_clear_fields    TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_longtexts) TYPE ty_longtexts
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

    CONSTANTS c_docu_state_active TYPE dokstate VALUE 'A' ##NO_TEXT.
ENDCLASS.
CLASS zcl_abapgit_lxe_texts DEFINITION

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_lxe_texts .

    CLASS-METHODS get_translation_languages
      IMPORTING
        !iv_main_language   TYPE spras
        !it_i18n_languages  TYPE zif_abapgit_definitions=>ty_languages
      RETURNING
        VALUE(rt_languages) TYPE zif_abapgit_definitions=>ty_languages
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS get_installed_languages
      RETURNING
        VALUE(rt_languages) TYPE zif_abapgit_definitions=>ty_languages
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS convert_lang_string_to_table
      IMPORTING
        !iv_langs              TYPE string
        !iv_skip_main_language TYPE spras OPTIONAL
      RETURNING
        VALUE(rt_languages)    TYPE zif_abapgit_definitions=>ty_languages
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS convert_table_to_lang_string
      IMPORTING
        !it_languages   TYPE zif_abapgit_definitions=>ty_languages
      RETURNING
        VALUE(rv_langs) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS detect_unsupported_languages
      IMPORTING
        !it_languages TYPE zif_abapgit_definitions=>ty_languages
      RETURNING
        VALUE(rt_unsupported_languages) TYPE zif_abapgit_definitions=>ty_languages
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gt_installed_languages_cache TYPE zif_abapgit_definitions=>ty_languages.

    METHODS
      get_lang_iso4
        IMPORTING
          iv_src         TYPE laiso
        RETURNING
          VALUE(rv_iso4) TYPE lxeisolang
        RAISING
          zcx_abapgit_exception.
    METHODS
      get_lxe_object_list
        IMPORTING
          iv_object_type     TYPE trobjtype
          iv_object_name     TYPE sobj_name
        RETURNING
          VALUE(rt_obj_list) TYPE lxe_tt_colob .
    CLASS-METHODS
      langu_to_laiso_safe
        IMPORTING
          iv_langu TYPE sy-langu
        RETURNING
          VALUE(rv_laiso) TYPE laiso
        RAISING
          zcx_abapgit_exception.
    CLASS-METHODS
      check_langs_versus_installed
        IMPORTING
          it_languages TYPE zif_abapgit_definitions=>ty_languages
          it_installed TYPE zif_abapgit_definitions=>ty_languages
        EXPORTING
          et_intersection TYPE zif_abapgit_definitions=>ty_languages
          et_missfits TYPE zif_abapgit_definitions=>ty_languages.

ENDCLASS.
CLASS zcl_abapgit_objects_activation DEFINITION

  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS add
      IMPORTING
        !iv_type   TYPE trobjtype
        !iv_name   TYPE clike
        !iv_delete TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS add_item
      IMPORTING
        !is_item TYPE zif_abapgit_definitions=>ty_item
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS activate
      IMPORTING
        !iv_ddic TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS clear .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_classes,
        object  TYPE trobjtype,
        clsname TYPE seoclsname,
      END OF ty_classes.

    CLASS-DATA:
      gt_classes TYPE STANDARD TABLE OF ty_classes WITH DEFAULT KEY .
    CLASS-DATA:
      gt_objects TYPE TABLE OF dwinactiv .

    CLASS-METHODS update_where_used .
    CLASS-METHODS use_new_activation_logic
      RETURNING
        VALUE(rv_use_new_activation_logic) TYPE abap_bool .
    CLASS-METHODS activate_new
      IMPORTING
        !iv_ddic TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS activate_old
      IMPORTING
        !iv_ddic TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS activate_ddic
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS show_activation_errors
      IMPORTING
        !iv_logname TYPE ddmass-logname
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS is_ddic_type
      IMPORTING
        !iv_obj_type     TYPE trobjtype
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
ENDCLASS.
CLASS zcl_abapgit_objects_super DEFINITION

  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_user_unknown TYPE xubname VALUE 'UNKNOWN'.

    METHODS constructor
      IMPORTING
        !is_item     TYPE zif_abapgit_definitions=>ty_item
        !iv_language TYPE spras .
    CLASS-METHODS jump_adt
      IMPORTING
        !iv_obj_name     TYPE zif_abapgit_definitions=>ty_item-obj_name
        !iv_obj_type     TYPE zif_abapgit_definitions=>ty_item-obj_type
        !iv_sub_obj_name TYPE zif_abapgit_definitions=>ty_item-obj_name OPTIONAL
        !iv_sub_obj_type TYPE zif_abapgit_definitions=>ty_item-obj_type OPTIONAL
        !iv_line_number  TYPE i OPTIONAL
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

    DATA ms_item TYPE zif_abapgit_definitions=>ty_item .
    DATA mv_language TYPE spras .

    METHODS get_metadata
      RETURNING
        VALUE(rs_metadata) TYPE zif_abapgit_definitions=>ty_metadata .
    METHODS corr_insert
      IMPORTING
        !iv_package      TYPE devclass
        !ig_object_class TYPE any OPTIONAL
      RAISING
        zcx_abapgit_exception .
    METHODS tadir_insert
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    METHODS jump_se11
      RAISING
        zcx_abapgit_exception .
    METHODS exists_a_lock_entry_for
      IMPORTING
        !iv_lock_object               TYPE string
        !iv_argument                  TYPE seqg3-garg OPTIONAL
      RETURNING
        VALUE(rv_exists_a_lock_entry) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS set_default_package
      IMPORTING
        !iv_package TYPE devclass .
    METHODS serialize_longtexts
      IMPORTING
        !ii_xml         TYPE REF TO zif_abapgit_xml_output
        !iv_longtext_id TYPE dokil-id OPTIONAL
        !it_dokil       TYPE zif_abapgit_definitions=>ty_dokil_tt OPTIONAL
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_longtexts
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    METHODS delete_longtexts
      IMPORTING
        !iv_longtext_id TYPE dokil-id
      RAISING
        zcx_abapgit_exception .
    METHODS is_active
      RETURNING
        VALUE(rv_active) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS delete_ddic
      IMPORTING
        VALUE(iv_objtype)              TYPE string
        VALUE(iv_no_ask)               TYPE abap_bool DEFAULT abap_true
        VALUE(iv_no_ask_delete_append) TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_lxe_texts
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_lxe_texts
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
ENDCLASS.
CLASS zcl_abapgit_objects_bridge DEFINITION  FINAL CREATE PUBLIC INHERITING FROM zcl_abapgit_objects_super.

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.

    METHODS constructor
      IMPORTING is_item TYPE zif_abapgit_definitions=>ty_item
      RAISING   cx_sy_create_object_error.

    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mo_plugin TYPE REF TO object.

    " Metadata with late_deser to stay compatible with old bridge
    TYPES:
      BEGIN OF ty_metadata,
        class        TYPE string,
        version      TYPE string,
        late_deser   TYPE abap_bool,
        delete_tadir TYPE abap_bool,
        ddic         TYPE abap_bool,
      END OF ty_metadata .

    TYPES: BEGIN OF ty_s_objtype_map,
             obj_typ      TYPE trobjtype,
             plugin_class TYPE seoclsname,
           END OF ty_s_objtype_map,
           ty_t_objtype_map TYPE SORTED TABLE OF ty_s_objtype_map WITH UNIQUE KEY obj_typ.

    CLASS-DATA gt_objtype_map TYPE ty_t_objtype_map.

ENDCLASS.
CLASS zcl_abapgit_xml DEFINITION

  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_filename TYPE string OPTIONAL.
  PROTECTED SECTION.
    DATA: mi_ixml     TYPE REF TO if_ixml,
          mi_xml_doc  TYPE REF TO if_ixml_document,
          ms_metadata TYPE zif_abapgit_definitions=>ty_metadata,
          mv_filename TYPE string.

    CONSTANTS: c_abapgit_tag             TYPE string VALUE 'abapGit' ##NO_TEXT,
               c_attr_version            TYPE string VALUE 'version' ##NO_TEXT,
               c_attr_serializer         TYPE string VALUE 'serializer' ##NO_TEXT,
               c_attr_serializer_version TYPE string VALUE 'serializer_version' ##NO_TEXT.

    METHODS to_xml
      IMPORTING iv_normalize  TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(rv_xml) TYPE string.

    METHODS parse
      IMPORTING iv_xml TYPE string
      RAISING   zcx_abapgit_exception.
  PRIVATE SECTION.

    METHODS error
      IMPORTING ii_parser TYPE REF TO if_ixml_parser
      RAISING   zcx_abapgit_exception.
    METHODS display_version_mismatch
      RAISING zcx_abapgit_exception.
    METHODS show_parser_errors
      IMPORTING ii_parser TYPE REF TO if_ixml_parser.
    METHODS raise_exception_for
      IMPORTING
        ii_error TYPE REF TO if_ixml_parse_error
      RAISING
        zcx_abapgit_exception.

ENDCLASS.
CLASS zcl_abapgit_xml_input DEFINITION

  INHERITING FROM zcl_abapgit_xml
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_xml_input.

    METHODS constructor
      IMPORTING
        !iv_xml      TYPE clike
        !iv_filename TYPE string OPTIONAL
      RAISING
        zcx_abapgit_exception .

  PRIVATE SECTION.
    METHODS: fix_xml.

ENDCLASS.
CLASS zcl_abapgit_objects_files DEFINITION

  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !is_item TYPE zif_abapgit_definitions=>ty_item
        !iv_path TYPE string OPTIONAL .
    METHODS add_string
      IMPORTING
        !iv_extra  TYPE clike OPTIONAL
        !iv_ext    TYPE string
        !iv_string TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS read_string
      IMPORTING
        !iv_extra        TYPE clike OPTIONAL
        !iv_ext          TYPE string
      RETURNING
        VALUE(rv_string) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS add_xml
      IMPORTING
        !iv_extra     TYPE clike OPTIONAL
        !ii_xml       TYPE REF TO zif_abapgit_xml_output
        !iv_normalize TYPE abap_bool DEFAULT abap_true
        !is_metadata  TYPE zif_abapgit_definitions=>ty_metadata OPTIONAL
      RAISING
        zcx_abapgit_exception .
    METHODS read_xml
      IMPORTING
        !iv_extra     TYPE clike OPTIONAL
      RETURNING
        VALUE(ri_xml) TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    METHODS read_abap
      IMPORTING
        !iv_extra      TYPE clike OPTIONAL
        !iv_error      TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_abap) TYPE abaptxt255_tab
      RAISING
        zcx_abapgit_exception .
    METHODS add_abap
      IMPORTING
        !iv_extra TYPE clike OPTIONAL
        !it_abap  TYPE STANDARD TABLE
      RAISING
        zcx_abapgit_exception .
    METHODS add
      IMPORTING
        !is_file TYPE zif_abapgit_definitions=>ty_file .
    METHODS add_raw
      IMPORTING
        !iv_extra TYPE clike OPTIONAL
        !iv_ext   TYPE string
        !iv_data  TYPE xstring
      RAISING
        zcx_abapgit_exception .
    METHODS read_raw
      IMPORTING
        !iv_extra      TYPE clike OPTIONAL
        !iv_ext        TYPE string
      RETURNING
        VALUE(rv_data) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    METHODS get_files
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt .
    METHODS set_files
      IMPORTING
        !it_files TYPE zif_abapgit_definitions=>ty_files_tt .
    METHODS get_accessed_files
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_file_signatures_tt .
    METHODS contains
      IMPORTING
        !iv_extra         TYPE clike OPTIONAL
        !iv_ext           TYPE string
      RETURNING
        VALUE(rv_present) TYPE abap_bool .
    METHODS get_file_pattern
      RETURNING
        VALUE(rv_pattern) TYPE string .
  PROTECTED SECTION.

    METHODS read_file
      IMPORTING
        !iv_filename   TYPE string
        !iv_error      TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rv_data) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    METHODS filename
      IMPORTING
        !iv_extra          TYPE clike OPTIONAL
        !iv_ext            TYPE string
      RETURNING
        VALUE(rv_filename) TYPE string .
  PRIVATE SECTION.

    DATA ms_item TYPE zif_abapgit_definitions=>ty_item .
    DATA mt_accessed_files TYPE zif_abapgit_definitions=>ty_file_signatures_tt .
    DATA mt_files TYPE zif_abapgit_definitions=>ty_files_tt .
    DATA mv_path TYPE string .
ENDCLASS.
CLASS zcl_abapgit_xml_output DEFINITION

  INHERITING FROM zcl_abapgit_xml
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_xml_output.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mi_raw TYPE REF TO if_ixml_element .
    DATA ms_i18n_params TYPE zif_abapgit_definitions=>ty_i18n_params .

    METHODS build_asx_node
      RETURNING
        VALUE(ri_element) TYPE REF TO if_ixml_element .
ENDCLASS.
CLASS zcl_abapgit_objects_program DEFINITION  INHERITING FROM zcl_abapgit_objects_super.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_progdir,
             name    TYPE progdir-name,
             state   TYPE progdir-state,
             sqlx    TYPE progdir-sqlx,
             edtx    TYPE progdir-edtx,
             varcl   TYPE progdir-varcl,
             dbapl   TYPE progdir-dbapl,
             dbna    TYPE progdir-dbna,
             clas    TYPE progdir-clas,
             type    TYPE progdir-type,
             occurs  TYPE progdir-occurs,
             subc    TYPE progdir-subc,
             appl    TYPE progdir-appl,
             secu    TYPE progdir-secu,
             cnam    TYPE progdir-cnam,
             cdat    TYPE progdir-cdat,
             unam    TYPE progdir-unam,
             udat    TYPE progdir-udat,
             vern    TYPE progdir-vern,
             levl    TYPE progdir-levl,
             rstat   TYPE progdir-rstat,
             rmand   TYPE progdir-rmand,
             rload   TYPE progdir-rload,
             fixpt   TYPE progdir-fixpt,
             sset    TYPE progdir-sset,
             sdate   TYPE progdir-sdate,
             stime   TYPE progdir-stime,
             idate   TYPE progdir-idate,
             itime   TYPE progdir-itime,
             ldbname TYPE progdir-ldbname,
             uccheck TYPE progdir-uccheck,
           END OF ty_progdir.

    METHODS serialize_program
      IMPORTING io_xml     TYPE REF TO zif_abapgit_xml_output OPTIONAL
                is_item    TYPE zif_abapgit_definitions=>ty_item
                io_files   TYPE REF TO zcl_abapgit_objects_files
                iv_program TYPE programm OPTIONAL
                iv_extra   TYPE clike OPTIONAL
      RAISING   zcx_abapgit_exception.

    METHODS read_progdir
      IMPORTING iv_program        TYPE programm
      RETURNING VALUE(rs_progdir) TYPE ty_progdir.

    METHODS deserialize_program
      IMPORTING is_progdir TYPE ty_progdir
                it_source  TYPE abaptxt255_tab
                it_tpool   TYPE textpool_table
                iv_package TYPE devclass
      RAISING   zcx_abapgit_exception.

  PROTECTED SECTION.

    TYPES:
      ty_spaces_tt TYPE STANDARD TABLE OF i WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_dynpro,
        header     TYPE rpy_dyhead,
        containers TYPE dycatt_tab,
        fields     TYPE dyfatc_tab,
        flow_logic TYPE swydyflow,
        spaces     TYPE ty_spaces_tt,
      END OF ty_dynpro .
    TYPES:
      ty_dynpro_tt TYPE STANDARD TABLE OF ty_dynpro WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_cua,
        adm TYPE rsmpe_adm,
        sta TYPE STANDARD TABLE OF rsmpe_stat WITH DEFAULT KEY,
        fun TYPE STANDARD TABLE OF rsmpe_funt WITH DEFAULT KEY,
        men TYPE STANDARD TABLE OF rsmpe_men WITH DEFAULT KEY,
        mtx TYPE STANDARD TABLE OF rsmpe_mnlt WITH DEFAULT KEY,
        act TYPE STANDARD TABLE OF rsmpe_act WITH DEFAULT KEY,
        but TYPE STANDARD TABLE OF rsmpe_but WITH DEFAULT KEY,
        pfk TYPE STANDARD TABLE OF rsmpe_pfk WITH DEFAULT KEY,
        set TYPE STANDARD TABLE OF rsmpe_staf WITH DEFAULT KEY,
        doc TYPE STANDARD TABLE OF rsmpe_atrt WITH DEFAULT KEY,
        tit TYPE STANDARD TABLE OF rsmpe_titt WITH DEFAULT KEY,
        biv TYPE STANDARD TABLE OF rsmpe_buts WITH DEFAULT KEY,
      END OF ty_cua .

    METHODS serialize_dynpros
      IMPORTING
        !iv_program_name TYPE programm
      RETURNING
        VALUE(rt_dynpro) TYPE ty_dynpro_tt
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_cua
      IMPORTING
        !iv_program_name TYPE programm
      RETURNING
        VALUE(rs_cua)    TYPE ty_cua
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_dynpros
      IMPORTING
        !it_dynpros TYPE ty_dynpro_tt
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_textpool
      IMPORTING
        !iv_program    TYPE programm
        !it_tpool      TYPE textpool_table
        !iv_language   TYPE langu OPTIONAL
        !iv_is_include TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_cua
      IMPORTING
        !iv_program_name TYPE programm
        !is_cua          TYPE ty_cua
      RAISING
        zcx_abapgit_exception .
    METHODS is_any_dynpro_locked
      IMPORTING
        !iv_program                    TYPE programm
      RETURNING
        VALUE(rv_is_any_dynpro_locked) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS is_cua_locked
      IMPORTING
        !iv_program             TYPE programm
      RETURNING
        VALUE(rv_is_cua_locked) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS is_text_locked
      IMPORTING
        !iv_program              TYPE programm
      RETURNING
        VALUE(rv_is_text_locked) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS add_tpool
      IMPORTING
        !it_tpool       TYPE textpool_table
      RETURNING
        VALUE(rt_tpool) TYPE zif_abapgit_definitions=>ty_tpool_tt .
    CLASS-METHODS read_tpool
      IMPORTING
        !it_tpool       TYPE zif_abapgit_definitions=>ty_tpool_tt
      RETURNING
        VALUE(rt_tpool) TYPE zif_abapgit_definitions=>ty_tpool_tt .
  PRIVATE SECTION.
    METHODS:
      uncondense_flow
        IMPORTING it_flow        TYPE swydyflow
                  it_spaces      TYPE ty_spaces_tt
        RETURNING VALUE(rt_flow) TYPE swydyflow.

    CLASS-METHODS auto_correct_cua_adm
      IMPORTING
        is_cua TYPE ty_cua
      CHANGING
        cs_adm TYPE rsmpe_adm.


ENDCLASS.
CLASS zcl_abapgit_object_acid DEFINITION  INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS: create_object
      RETURNING VALUE(ro_aab) TYPE REF TO cl_aab_id
      RAISING   zcx_abapgit_exception.

ENDCLASS.
CLASS zcl_abapgit_object_avar DEFINITION

  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS: create_object
      RETURNING VALUE(ro_aab_var) TYPE REF TO cl_aab_variant
      RAISING   zcx_abapgit_exception.

ENDCLASS.
CLASS zcl_abapgit_oo_base DEFINITION

  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_oo_object_fnc .
  PROTECTED SECTION.
    CLASS-METHODS:
      convert_attrib_to_vseoattrib
        IMPORTING iv_clsname           TYPE seoclsname
                  it_attributes        TYPE zif_abapgit_definitions=>ty_obj_attribute_tt
        RETURNING VALUE(rt_vseoattrib) TYPE seoo_attributes_r.

  PRIVATE SECTION.

    DATA mv_skip_test_classes TYPE abap_bool .
ENDCLASS.
CLASS zcl_abapgit_oo_class DEFINITION

  INHERITING FROM zcl_abapgit_oo_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_abapgit_oo_object_fnc~create
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~create_sotr
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~delete
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~generate_locals
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~get_class_properties
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~get_includes
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~insert_text_pool
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~read_sotr
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~read_text_pool
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~deserialize_source
        REDEFINITION .
  PROTECTED SECTION.
    TYPES: ty_char1 TYPE c LENGTH 1,
           ty_char2 TYPE c LENGTH 2.

  PRIVATE SECTION.

    CLASS-METHODS update_source_index
      IMPORTING
        !iv_clsname TYPE csequence
        !io_scanner TYPE REF TO cl_oo_source_scanner_class .
    CLASS-METHODS update_report
      IMPORTING
        !iv_program       TYPE programm
        !it_source        TYPE string_table
      RETURNING
        VALUE(rv_updated) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS generate_classpool
      IMPORTING
        !iv_name TYPE seoclsname
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS update_meta
      IMPORTING
        !iv_name     TYPE seoclsname
        !iv_exposure TYPE seoexpose
        !it_source   TYPE rswsourcet
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS determine_method_include
      IMPORTING
        !iv_name          TYPE seoclsname
        !iv_method        TYPE seocpdname
      RETURNING
        VALUE(rv_program) TYPE programm
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS init_scanner
      IMPORTING
        !it_source        TYPE zif_abapgit_definitions=>ty_string_tt
        !iv_name          TYPE seoclsname
      RETURNING
        VALUE(ro_scanner) TYPE REF TO cl_oo_source_scanner_class
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS update_full_class_include
      IMPORTING
        !iv_classname TYPE seoclsname
        !it_source    TYPE string_table
        !it_methods   TYPE cl_oo_source_scanner_class=>type_method_implementations .
    CLASS-METHODS create_report
      IMPORTING
        !iv_program      TYPE programm
        !it_source       TYPE string_table
        !iv_extension    TYPE ty_char2
        !iv_program_type TYPE ty_char1
        !iv_version      TYPE r3state .
    CLASS-METHODS update_cs_number_of_methods
      IMPORTING
        !iv_classname              TYPE seoclsname
        !iv_number_of_impl_methods TYPE i .
    CLASS-METHODS delete_report
      IMPORTING
        !iv_program TYPE programm .
ENDCLASS.
CLASS zcl_abapgit_object_clas DEFINITION

  INHERITING FROM zcl_abapgit_objects_program
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .

    ALIASES mo_files
      FOR zif_abapgit_object~mo_files .

    METHODS constructor
      IMPORTING
        !is_item     TYPE zif_abapgit_definitions=>ty_item
        !iv_language TYPE spras .
  PROTECTED SECTION.
    DATA: mi_object_oriented_object_fct TYPE REF TO zif_abapgit_oo_object_fnc,
          mv_skip_testclass             TYPE abap_bool,
          mv_classpool_name             TYPE progname.
    METHODS:
      deserialize_abap
        IMPORTING ii_xml     TYPE REF TO zif_abapgit_xml_input
                  iv_package TYPE devclass
        RAISING   zcx_abapgit_exception,
      deserialize_docu
        IMPORTING ii_xml TYPE REF TO zif_abapgit_xml_input
        RAISING   zcx_abapgit_exception,
      deserialize_tpool
        IMPORTING ii_xml TYPE REF TO zif_abapgit_xml_input
        RAISING   zcx_abapgit_exception,
      deserialize_sotr
        IMPORTING ii_ml      TYPE REF TO zif_abapgit_xml_input
                  iv_package TYPE devclass
        RAISING   zcx_abapgit_exception,
      serialize_xml
        IMPORTING ii_xml TYPE REF TO zif_abapgit_xml_output
        RAISING   zcx_abapgit_exception,
      serialize_attr
        IMPORTING
          !ii_xml     TYPE REF TO zif_abapgit_xml_output
          !iv_clsname TYPE seoclsname
        RAISING
          zcx_abapgit_exception,
      serialize_descr
        IMPORTING
          !ii_xml     TYPE REF TO zif_abapgit_xml_output
          !iv_clsname TYPE seoclsname
        RAISING
          zcx_abapgit_exception,
      serialize_docu
        IMPORTING
          !ii_xml              TYPE REF TO zif_abapgit_xml_output
          !it_langu_additional TYPE zif_abapgit_lang_definitions=>ty_langus OPTIONAL
          !iv_clsname          TYPE seoclsname
        RAISING
          zcx_abapgit_exception,
      serialize_tpool
        IMPORTING
          !ii_xml              TYPE REF TO zif_abapgit_xml_output
          !it_langu_additional TYPE zif_abapgit_lang_definitions=>ty_langus OPTIONAL
          !iv_clsname          TYPE seoclsname
        RAISING
          zcx_abapgit_exception,
      serialize_sotr
        IMPORTING
          !ii_xml TYPE REF TO zif_abapgit_xml_output
        RAISING
          zcx_abapgit_exception,
      source_apack_replacement
        CHANGING
          !ct_source TYPE seop_source_string
        RAISING
          zcx_abapgit_exception,
      repo_apack_replacement
        CHANGING
          !ct_source TYPE seop_source_string
        RAISING
          zcx_abapgit_exception.

  PRIVATE SECTION.
    METHODS:
      is_class_locked
        RETURNING VALUE(rv_is_class_locked) TYPE abap_bool
        RAISING   zcx_abapgit_exception.
    METHODS interface_replacement
      IMPORTING
        !iv_from_interface TYPE seoclsname
        !iv_to_interface   TYPE seoclsname
      CHANGING
        !ct_source         TYPE seop_source_string .
ENDCLASS.
CLASS zcl_abapgit_object_devc DEFINITION
  INHERITING FROM zcl_abapgit_objects_super
  FINAL.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_object.
    ALIASES:
      mo_files FOR zif_abapgit_object~mo_files.
    METHODS:
      constructor IMPORTING is_item     TYPE zif_abapgit_definitions=>ty_item
                            iv_language TYPE spras.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_local_devclass TYPE devclass .

    METHODS get_package
      RETURNING
        VALUE(ri_package) TYPE REF TO if_package
      RAISING
        zcx_abapgit_exception .
    METHODS update_pinf_usages
      IMPORTING
        !ii_package    TYPE REF TO if_package
        !it_usage_data TYPE scomppdata
      RAISING
        zcx_abapgit_exception .
    METHODS set_lock
      IMPORTING
        !ii_package TYPE REF TO if_package
        !iv_lock    TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS is_empty
      IMPORTING
        !iv_package_name   TYPE devclass
      RETURNING
        VALUE(rv_is_empty) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS load_package
      IMPORTING
        !iv_package_name  TYPE devclass
      RETURNING
        VALUE(ri_package) TYPE REF TO if_package
      RAISING
        zcx_abapgit_exception .
    METHODS is_local
      IMPORTING
        !iv_package_name   TYPE devclass
      RETURNING
        VALUE(rv_is_local) TYPE abap_bool .
    METHODS remove_obsolete_tadir
      IMPORTING
        !iv_package_name TYPE devclass .
ENDCLASS.
CLASS zcl_abapgit_object_doma DEFINITION  INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_dd01_text,
        ddlanguage TYPE dd01v-ddlanguage,
        ddtext     TYPE dd01v-ddtext,
      END OF ty_dd01_text .
    TYPES:
      BEGIN OF ty_dd07_text,
        valpos     TYPE dd07v-valpos,
        ddlanguage TYPE dd07v-ddlanguage,
        domvalue_l TYPE dd07v-domvalue_l,
        domvalue_h TYPE dd07v-domvalue_h,
        ddtext     TYPE dd07v-ddtext,
        domval_ld  TYPE dd07v-domval_ld,
        domval_hd  TYPE dd07v-domval_hd,
      END OF ty_dd07_text .
    TYPES:
      ty_dd01_texts TYPE STANDARD TABLE OF ty_dd01_text .
    TYPES:
      ty_dd07_texts TYPE STANDARD TABLE OF ty_dd07_text .

    CONSTANTS c_longtext_id_doma TYPE dokil-id VALUE 'DO' ##NO_TEXT.

    METHODS serialize_texts
      IMPORTING
        !ii_xml   TYPE REF TO zif_abapgit_xml_output
        !it_dd07v TYPE dd07v_tab
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_texts
      IMPORTING
        !ii_xml   TYPE REF TO zif_abapgit_xml_input
        !is_dd01v TYPE dd01v
        !it_dd07v TYPE dd07v_tab
      RAISING
        zcx_abapgit_exception .
ENDCLASS.
CLASS zcl_abapgit_object_dsys DEFINITION  INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object.

    METHODS constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS: c_typ TYPE dokhl-typ VALUE 'E',
               c_id  TYPE dokhl-id VALUE 'HY'.

    DATA: mv_doc_object  TYPE sobj_name.

    TYPES: BEGIN OF ty_data,
             doctitle TYPE dsyst-doktitle,
             head     TYPE thead,
             lines    TYPE tline_tab,
           END OF ty_data.

    METHODS deserialize_dsys
      IMPORTING
        ii_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.

    METHODS get_main_lang
      RETURNING
        VALUE(rv_language) TYPE spras.

ENDCLASS.
CLASS zcl_abapgit_object_dtel DEFINITION  INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_dd04_text,
        ddlanguage TYPE dd04t-ddlanguage,
        ddtext     TYPE dd04t-ddtext,
        reptext    TYPE dd04t-reptext,
        scrtext_s  TYPE dd04t-scrtext_s,
        scrtext_m  TYPE dd04t-scrtext_m,
        scrtext_l  TYPE dd04t-scrtext_l,
      END OF ty_dd04_text .
    TYPES:
      ty_dd04_texts TYPE STANDARD TABLE OF ty_dd04_text .

    CONSTANTS c_longtext_id_dtel TYPE dokil-id VALUE 'DE' ##NO_TEXT.

    METHODS serialize_texts
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_texts
      IMPORTING
        !ii_xml   TYPE REF TO zif_abapgit_xml_input
        !is_dd04v TYPE dd04v
      RAISING
        zcx_abapgit_exception .
ENDCLASS.
CLASS zcl_abapgit_object_enhc DEFINITION

  INHERITING FROM zcl_abapgit_objects_super.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mv_composite_id TYPE enhcompositename.

ENDCLASS.
CLASS zcl_abapgit_object_enho_badi DEFINITION .

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        is_item  TYPE zif_abapgit_definitions=>ty_item
        io_files TYPE REF TO zcl_abapgit_objects_files.
    INTERFACES: zif_abapgit_object_enho.

  PRIVATE SECTION.
    DATA: ms_item  TYPE zif_abapgit_definitions=>ty_item.

ENDCLASS.
CLASS zcl_abapgit_object_enho_hook DEFINITION .
  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        is_item  TYPE zif_abapgit_definitions=>ty_item
        io_files TYPE REF TO zcl_abapgit_objects_files.

    INTERFACES: zif_abapgit_object_enho.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_spaces,
             full_name TYPE string.
    TYPES: spaces TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
           END OF ty_spaces.

    TYPES: ty_spaces_tt TYPE STANDARD TABLE OF ty_spaces WITH DEFAULT KEY.

    DATA: ms_item TYPE zif_abapgit_definitions=>ty_item.
    DATA: mo_files TYPE REF TO zcl_abapgit_objects_files.

    METHODS hook_impl_deserialize
      IMPORTING it_spaces TYPE ty_spaces_tt
      CHANGING  ct_impl   TYPE enh_hook_impl_it
      RAISING   zcx_abapgit_exception.

ENDCLASS.
CLASS zcl_abapgit_object_enho_class DEFINITION .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          is_item  TYPE zif_abapgit_definitions=>ty_item
          io_files TYPE REF TO zcl_abapgit_objects_files.
    INTERFACES: zif_abapgit_object_enho.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      serialize_includes
        IMPORTING
          io_class TYPE REF TO cl_enh_tool_class
        RAISING
          zcx_abapgit_exception,
      deserialize_includes
        IMPORTING
          ii_xml   TYPE REF TO zif_abapgit_xml_input
          io_class TYPE REF TO cl_enh_tool_class
        RAISING
          zcx_abapgit_exception.

    DATA: ms_item TYPE zif_abapgit_definitions=>ty_item.
    DATA: mo_files TYPE REF TO zcl_abapgit_objects_files.

ENDCLASS.
CLASS zcl_abapgit_object_enho_intf DEFINITION .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          is_item  TYPE zif_abapgit_definitions=>ty_item
          io_files TYPE REF TO zcl_abapgit_objects_files.
    INTERFACES: zif_abapgit_object_enho.

  PRIVATE SECTION.
    DATA: ms_item  TYPE zif_abapgit_definitions=>ty_item,
          mo_files TYPE REF TO zcl_abapgit_objects_files.

ENDCLASS.
CLASS zcl_abapgit_object_enho_wdyc DEFINITION .

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        is_item  TYPE zif_abapgit_definitions=>ty_item
        io_files TYPE REF TO zcl_abapgit_objects_files.
    INTERFACES: zif_abapgit_object_enho.

  PRIVATE SECTION.
    DATA: ms_item  TYPE zif_abapgit_definitions=>ty_item.

ENDCLASS.
CLASS zcl_abapgit_object_enho_fugr DEFINITION .

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        is_item  TYPE zif_abapgit_definitions=>ty_item
        io_files TYPE REF TO zcl_abapgit_objects_files.
    INTERFACES: zif_abapgit_object_enho.

  PRIVATE SECTION.
    DATA: ms_item  TYPE zif_abapgit_definitions=>ty_item,
          mo_files TYPE REF TO zcl_abapgit_objects_files.

ENDCLASS.
CLASS zcl_abapgit_object_enho_wdyn DEFINITION .

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        is_item  TYPE zif_abapgit_definitions=>ty_item
        io_files TYPE REF TO zcl_abapgit_objects_files.
    INTERFACES: zif_abapgit_object_enho.

  PRIVATE SECTION.
    DATA: ms_item  TYPE zif_abapgit_definitions=>ty_item.

ENDCLASS.
CLASS zcl_abapgit_object_enho DEFINITION  INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:
      factory
        IMPORTING
          iv_tool        TYPE enhtooltype
        RETURNING
          VALUE(ri_enho) TYPE REF TO zif_abapgit_object_enho
        RAISING
          zcx_abapgit_exception.

ENDCLASS.
CLASS zcl_abapgit_object_enho_clif DEFINITION

  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS deserialize
      IMPORTING
        !io_xml  TYPE REF TO zif_abapgit_xml_input
        !io_clif TYPE REF TO cl_enh_tool_clif
      RAISING
        zcx_abapgit_exception
        cx_enh_root .
    CLASS-METHODS serialize
      IMPORTING
        !io_xml   TYPE REF TO zif_abapgit_xml_output
        !io_files TYPE REF TO zcl_abapgit_objects_files
        !io_clif  TYPE REF TO cl_enh_tool_clif
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.
CLASS zcl_abapgit_object_enhs_badi_d DEFINITION .

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_object_enhs.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS zcl_abapgit_object_enhs_hook_d DEFINITION .

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_object_enhs.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_hook_defifnition,
             pgmid     TYPE pgmid,
             obj_name  TYPE trobj_name,
             obj_type  TYPE trobjtype,
             main_type TYPE trobjtype,
             main_name TYPE eu_aname,
             program   TYPE progname,
             def_hooks TYPE enh_hook_def_ext_it,
           END OF ty_hook_defifnition.

ENDCLASS.
CLASS zcl_abapgit_object_enhs DEFINITION  INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      factory
        IMPORTING
          iv_tool        TYPE enhtooltype
        RETURNING
          VALUE(ri_enho) TYPE REF TO zif_abapgit_object_enhs
        RAISING
          zcx_abapgit_exception.

ENDCLASS.
CLASS zcl_abapgit_object_enqu DEFINITION

  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .

    ALIASES mo_files
      FOR zif_abapgit_object~mo_files .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ty_dd27p TYPE STANDARD TABLE OF dd27p WITH DEFAULT KEY.
    METHODS _clear_dd27p_fields CHANGING ct_dd27p TYPE ty_dd27p.

ENDCLASS.
CLASS zcl_abapgit_object_fugr DEFINITION  INHERITING FROM zcl_abapgit_objects_program FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_rs38l_incl_tt TYPE STANDARD TABLE OF rs38l_incl WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_function,
        funcname          TYPE rs38l_fnam,
        global_flag       TYPE rs38l-global,
        remote_call       TYPE rs38l-remote,
        update_task       TYPE rs38l-utask,
        short_text        TYPE tftit-stext,
        remote_basxml     TYPE rs38l-basxml_enabled,
        import            TYPE STANDARD TABLE OF rsimp WITH DEFAULT KEY,
        changing          TYPE STANDARD TABLE OF rscha WITH DEFAULT KEY,
        export            TYPE STANDARD TABLE OF rsexp WITH DEFAULT KEY,
        tables            TYPE STANDARD TABLE OF rstbl WITH DEFAULT KEY,
        exception         TYPE STANDARD TABLE OF rsexc WITH DEFAULT KEY,
        documentation     TYPE STANDARD TABLE OF rsfdo WITH DEFAULT KEY,
        exception_classes TYPE abap_bool,
      END OF ty_function .
    TYPES:
      ty_function_tt TYPE STANDARD TABLE OF ty_function WITH DEFAULT KEY .
    TYPES:
      ty_sobj_name_tt TYPE STANDARD TABLE OF sobj_name  WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_tpool_i18n,
        language TYPE langu,
        textpool TYPE zif_abapgit_definitions=>ty_tpool_tt,
      END OF ty_tpool_i18n .
    TYPES:
      ty_tpools_i18n TYPE STANDARD TABLE OF ty_tpool_i18n .

    DATA mt_includes_cache TYPE ty_sobj_name_tt .

    METHODS check_rfc_parameters
      IMPORTING
        !is_function TYPE ty_function
      RAISING
        zcx_abapgit_exception .
    METHODS update_where_used
      IMPORTING
        !it_includes TYPE ty_sobj_name_tt .
    METHODS main_name
      RETURNING
        VALUE(rv_program) TYPE program
      RAISING
        zcx_abapgit_exception .
    METHODS functions
      RETURNING
        VALUE(rt_functab) TYPE ty_rs38l_incl_tt
      RAISING
        zcx_abapgit_exception .
    METHODS includes
      RETURNING
        VALUE(rt_includes) TYPE ty_sobj_name_tt
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_functions
      RETURNING
        VALUE(rt_functions) TYPE ty_function_tt
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_functions
      IMPORTING
        !it_functions TYPE ty_function_tt
        !ii_log       TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_xml
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_xml
      IMPORTING
        !ii_xml     TYPE REF TO zif_abapgit_xml_input
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_includes
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_includes
      IMPORTING
        !ii_xml     TYPE REF TO zif_abapgit_xml_input
        !iv_package TYPE devclass
        !ii_log     TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception .
    METHODS is_function_group_locked
      RETURNING
        VALUE(rv_is_functions_group_locked) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS is_any_include_locked
      RETURNING
        VALUE(rv_is_any_include_locked) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS is_any_function_module_locked
      RETURNING
        VALUE(rv_any_function_module_locked) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS get_abap_version
      IMPORTING
        !ii_xml                TYPE REF TO zif_abapgit_xml_input
      RETURNING
        VALUE(rv_abap_version) TYPE progdir-uccheck
      RAISING
        zcx_abapgit_exception .
    METHODS update_func_group_short_text
      IMPORTING
        !iv_group      TYPE rs38l-area
        !iv_short_text TYPE tftit-stext .
    METHODS serialize_texts
      IMPORTING
        !iv_prog_name TYPE programm
        !ii_xml       TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_texts
      IMPORTING
        !iv_prog_name TYPE programm
        !ii_xml       TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
ENDCLASS.
CLASS zcl_abapgit_object_idoc DEFINITION  INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.
    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras.
    CLASS-METHODS clear_idoc_segement_fields CHANGING cg_structure TYPE any.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_idoc,
        attributes TYPE edi_iapi01,
        t_syntax   TYPE STANDARD TABLE OF edi_iapi02 WITH NON-UNIQUE DEFAULT KEY,
      END OF ty_idoc.

    DATA: mv_idoctyp TYPE edi_iapi00-idoctyp.

    CLASS-METHODS clear_idoc_segement_field
      IMPORTING iv_fieldname TYPE csequence
      CHANGING cg_structure TYPE any.

ENDCLASS.
CLASS zcl_abapgit_object_intf DEFINITION  FINAL INHERITING FROM zcl_abapgit_objects_program.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.
    METHODS constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.
  PROTECTED SECTION.

    METHODS deserialize_proxy
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_abap
      IMPORTING
        !ii_xml     TYPE REF TO zif_abapgit_xml_input
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_docu
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

    DATA mi_object_oriented_object_fct TYPE REF TO zif_abapgit_oo_object_fnc .

    METHODS serialize_xml
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
ENDCLASS.
CLASS zcl_abapgit_object_msag DEFINITION  INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_t100_text,
        sprsl TYPE t100-sprsl,
        msgnr TYPE t100-msgnr,
        text  TYPE t100-text,
      END OF ty_t100_text .
    TYPES:
      ty_t100_texts TYPE STANDARD TABLE OF ty_t100_text .
    TYPES:
      ty_t100s      TYPE STANDARD TABLE OF t100
                           WITH NON-UNIQUE DEFAULT KEY .

    METHODS serialize_texts
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_texts
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_longtexts_msag
      IMPORTING
        !it_t100 TYPE ty_t100s
        !ii_xml  TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS delete_msgid
      IMPORTING
        !iv_message_id TYPE arbgb .
    METHODS free_access_permission
      IMPORTING
        !iv_message_id TYPE arbgb .
    METHODS delete_documentation
      IMPORTING
        !iv_message_id TYPE arbgb .
ENDCLASS.
CLASS zcl_abapgit_object_nspc DEFINITION

  INHERITING FROM zcl_abapgit_objects_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_nspc,
        namespace  TYPE trnspacet-namespace,
        replicense TYPE trnspacet-replicense,
        sscrflag   TYPE trnspacet-sscrflag,
        sapflag    TYPE trnspacet-sapflag,
        gen_only   TYPE trnspacet-gen_only,
      END OF ty_nspc .
    TYPES:
      BEGIN OF ty_nspc_text,
        spras     TYPE trnspacett-spras,
        descriptn TYPE trnspacett-descriptn,
        owner     TYPE trnspacett-owner,
      END OF ty_nspc_text .
    TYPES:
      ty_nspc_texts TYPE STANDARD TABLE OF ty_nspc_text .

    METHODS serialize_texts
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_texts
      IMPORTING
        !ii_xml       TYPE REF TO zif_abapgit_xml_input
        !iv_namespace TYPE namespace
      RAISING
        zcx_abapgit_exception .
    METHODS add_to_transport
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
ENDCLASS.
CLASS zcl_abapgit_object_para DEFINITION  INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS unlock
      IMPORTING
        !iv_paramid TYPE memoryid .
ENDCLASS.
CLASS zcl_abapgit_object_prog DEFINITION  INHERITING FROM zcl_abapgit_objects_program FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_tpool_i18n,
             language TYPE langu,
             textpool TYPE zif_abapgit_definitions=>ty_tpool_tt,
           END OF ty_tpool_i18n,
           ty_tpools_i18n TYPE STANDARD TABLE OF ty_tpool_i18n.
    CONSTANTS: c_longtext_id_prog TYPE dokil-id VALUE 'RE'.

    METHODS:
      serialize_texts
        IMPORTING ii_xml TYPE REF TO zif_abapgit_xml_output
        RAISING   zcx_abapgit_exception,
      deserialize_texts
        IMPORTING ii_xml TYPE REF TO zif_abapgit_xml_input
        RAISING   zcx_abapgit_exception,
      is_program_locked
        RETURNING
          VALUE(rv_is_program_locked) TYPE abap_bool
        RAISING
          zcx_abapgit_exception.

ENDCLASS.
CLASS zcl_abapgit_object_tabl_compar DEFINITION

  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_comparator .

    METHODS constructor
      IMPORTING
        !ii_local TYPE REF TO zif_abapgit_xml_input.
  PROTECTED SECTION.

    TYPES:
      ty_founds  TYPE STANDARD TABLE OF rsfindlst
                           WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      ty_seu_obj TYPE STANDARD TABLE OF seu_obj
                           WITH NON-UNIQUE DEFAULT KEY .

    DATA mi_local TYPE REF TO zif_abapgit_xml_input.

    METHODS get_where_used_recursive
      IMPORTING
        !iv_object_name      TYPE csequence
        !iv_depth            TYPE i
        !iv_object_type      TYPE euobj-id
        !it_scope            TYPE ty_seu_obj
      RETURNING
        VALUE(rt_founds_all) TYPE ty_founds
      RAISING
        zcx_abapgit_exception .
    METHODS is_structure_used_in_db_table
      IMPORTING
        !iv_object_name                       TYPE dd02v-tabname
      RETURNING
        VALUE(rv_is_structure_used_in_db_tab) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS validate
      IMPORTING
        !ii_remote_version TYPE REF TO zif_abapgit_xml_input
        !ii_local_version  TYPE REF TO zif_abapgit_xml_input
        !ii_log            TYPE REF TO zif_abapgit_log
      RETURNING
        VALUE(rv_message)  TYPE string
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

ENDCLASS.
CLASS zcl_abapgit_object_tabl DEFINITION

  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .

    ALIASES mo_files
      FOR zif_abapgit_object~mo_files .
  PROTECTED SECTION.
    TYPES: BEGIN OF ty_segment_definition,
             segmentheader     TYPE edisegmhd,
             segmentdefinition TYPE edisegmdef,
             segmentstructures TYPE STANDARD TABLE OF edisegstru WITH DEFAULT KEY,
           END OF ty_segment_definition.
    TYPES: ty_segment_definitions TYPE STANDARD TABLE OF ty_segment_definition WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_tabl_extras,
             tddat TYPE tddat,
           END OF ty_tabl_extras.

    "! get additional data like table authorization group
    "! @parameter iv_tabname | name of the table
    METHODS read_extras IMPORTING iv_tabname            TYPE ddobjname
                        RETURNING VALUE(rs_tabl_extras) TYPE ty_tabl_extras.

    "! Update additional data
    "! @parameter iv_tabname | name of the table
    "! @parameter is_tabl_extras | additional table data
    METHODS update_extras IMPORTING iv_tabname     TYPE ddobjname
                                    is_tabl_extras TYPE ty_tabl_extras.

    "! Delete additional data
    "! @parameter iv_tabname | name of the table
    METHODS delete_extras IMPORTING iv_tabname TYPE ddobjname.

    "! Serialize IDoc Segment type/definition if exits
    "! @parameter io_xml | XML writer
    "! @raising zcx_abapgit_exception | Exceptions
    METHODS serialize_idoc_segment IMPORTING io_xml TYPE REF TO zif_abapgit_xml_output
                                   RAISING   zcx_abapgit_exception.

    "! Deserialize IDoc Segment type/definition if exits
    "! @parameter io_xml | XML writer
    "! @parameter iv_package | Target package
    "! @parameter rv_deserialized | It's a segment and was desserialized
    "! @raising zcx_abapgit_exception | Exceptions
    METHODS deserialize_idoc_segment IMPORTING io_xml                 TYPE REF TO zif_abapgit_xml_input
                                               iv_package             TYPE devclass
                                     RETURNING VALUE(rv_deserialized) TYPE abap_bool
                                     RAISING   zcx_abapgit_exception.
    "! Delete the IDoc Segment type if exists
    "! @parameter rv_deleted | It's a segment and was deleted
    "! @raising zcx_abapgit_exception | Exceptions
    METHODS delete_idoc_segment RETURNING VALUE(rv_deleted) TYPE abap_bool
                                RAISING   zcx_abapgit_exception.
  PRIVATE SECTION.

    TYPES:
      ty_dd03p_tt TYPE STANDARD TABLE OF dd03p .
    TYPES:
      BEGIN OF ty_dd02_text,
        ddlanguage TYPE dd02t-ddlanguage,
        ddtext     TYPE dd02t-ddtext,
      END OF ty_dd02_text .
    TYPES:
      ty_dd02_texts TYPE STANDARD TABLE OF ty_dd02_text .

    CONSTANTS c_longtext_id_tabl TYPE dokil-id VALUE 'TB' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF c_s_dataname,
        segment_definition TYPE string VALUE 'SEGMENT_DEFINITION',
        tabl_extras        TYPE string VALUE 'TABL_EXTRAS',
      END OF c_s_dataname .

    METHODS deserialize_indexes
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    METHODS clear_dd03p_fields
      CHANGING
        !ct_dd03p TYPE ty_dd03p_tt .
    "! Check if structure is an IDoc segment
    "! @parameter rv_is_idoc_segment | It's an IDoc segment or not
    METHODS is_idoc_segment
      RETURNING
        VALUE(rv_is_idoc_segment) TYPE abap_bool .
    METHODS clear_dd03p_fields_common
      CHANGING
        !cs_dd03p TYPE dd03p .
    METHODS clear_dd03p_fields_dataelement
      CHANGING
        !cs_dd03p TYPE dd03p .
    METHODS serialize_texts
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_texts
      IMPORTING
        !io_xml   TYPE REF TO zif_abapgit_xml_input
        !is_dd02v TYPE dd02v
      RAISING
        zcx_abapgit_exception .
    METHODS is_db_table_category
      IMPORTING
        !iv_tabclass               TYPE dd02l-tabclass
      RETURNING
        VALUE(rv_is_db_table_type) TYPE dd02l-tabclass .
ENDCLASS.
CLASS zcl_abapgit_object_tobj DEFINITION  INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_tobj,
             tddat TYPE tddat,
             tvdir TYPE tvdir,
             tvimf TYPE STANDARD TABLE OF tvimf WITH DEFAULT KEY,
           END OF ty_tobj.

    METHODS:
      read_extra IMPORTING iv_tabname     TYPE vim_name
                 RETURNING VALUE(rs_tobj) TYPE ty_tobj,
      update_extra IMPORTING is_tobj TYPE ty_tobj,
      delete_extra IMPORTING iv_tabname TYPE vim_name.

ENDCLASS.
CLASS zcl_abapgit_object_tran DEFINITION

  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object .

    ALIASES mo_files
      FOR zif_abapgit_object~mo_files .
  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      ty_param_values TYPE STANDARD TABLE OF rsparam
                                     WITH NON-UNIQUE DEFAULT KEY ,
      ty_tstca        TYPE STANDARD TABLE OF tstca
                                     WITH DEFAULT KEY.

    CONSTANTS:
      c_oo_program   TYPE c LENGTH 9 VALUE '\PROGRAM=' ##NO_TEXT,
      c_oo_class     TYPE c LENGTH 7 VALUE '\CLASS=' ##NO_TEXT,
      c_oo_method    TYPE c LENGTH 8 VALUE '\METHOD=' ##NO_TEXT,
      c_oo_tcode     TYPE tcode VALUE 'OS_APPLICATION' ##NO_TEXT,
      c_oo_frclass   TYPE c LENGTH 30 VALUE 'CLASS' ##NO_TEXT,
      c_oo_frmethod  TYPE c LENGTH 30 VALUE 'METHOD' ##NO_TEXT,
      c_oo_frupdtask TYPE c LENGTH 30 VALUE 'UPDATE_MODE' ##NO_TEXT,
      c_oo_synchron  TYPE c VALUE 'S' ##NO_TEXT,
      c_oo_asynchron TYPE c VALUE 'U' ##NO_TEXT,
      c_true         TYPE c VALUE 'X' ##NO_TEXT,
      c_false        TYPE c VALUE space ##NO_TEXT,
      BEGIN OF c_variant_type,
        dialog     TYPE rglif-docutype VALUE 'D' ##NO_TEXT,
        report     TYPE rglif-docutype VALUE 'R' ##NO_TEXT,
        variant    TYPE rglif-docutype VALUE 'V' ##NO_TEXT,
        parameters TYPE rglif-docutype VALUE 'P' ##NO_TEXT,
        object     TYPE rglif-docutype VALUE 'O' ##NO_TEXT,
      END OF c_variant_type.

    DATA:
      mt_bcdata TYPE STANDARD TABLE OF bdcdata .

    METHODS transaction_read
      IMPORTING
        iv_transaction TYPE tcode
      EXPORTING
        es_transaction TYPE tstc
        es_gui_attr    TYPE tstcc
      RAISING
        zcx_abapgit_exception.
    METHODS shift_param
      CHANGING
        !ct_rsparam TYPE s_param
        !cs_tstcp   TYPE tstcp .
    METHODS add_data
      IMPORTING
        !iv_fnam TYPE bdcdata-fnam
        !iv_fval TYPE clike .
    METHODS call_se93
      RAISING
        zcx_abapgit_exception .
    METHODS set_oo_parameters
      IMPORTING
        !it_rsparam TYPE s_param
      CHANGING
        !cs_rsstcd  TYPE rsstcd .
    METHODS split_parameters
      CHANGING
        !ct_rsparam TYPE s_param
        !cs_rsstcd  TYPE rsstcd
        !cs_tstcp   TYPE tstcp
        !cs_tstc    TYPE tstc .
    METHODS split_parameters_comp
      IMPORTING
        !ig_type  TYPE any
        !ig_param TYPE any
      CHANGING
        !cg_value TYPE any .
    METHODS serialize_texts
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_texts
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_oo_transaction
      IMPORTING
        !iv_package TYPE devclass
        !is_tstc    TYPE tstc
        !is_tstcc   TYPE tstcc
        !is_tstct   TYPE tstct
        !is_rsstcd  TYPE rsstcd
      RAISING
        zcx_abapgit_exception .
    METHODS save_authorizations
      IMPORTING
        iv_transaction    TYPE tstc-tcode
        it_authorizations TYPE ty_tstca
      RAISING
        zcx_abapgit_exception.
    METHODS clear_functiongroup_globals.
    METHODS is_variant_transaction IMPORTING is_tstcp                      TYPE tstcp
                                   RETURNING VALUE(rv_variant_transaction) TYPE abap_bool.
ENDCLASS.
CLASS zcl_abapgit_object_ttyp DEFINITION  INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS zcl_abapgit_object_w3xx_super DEFINITION

  INHERITING FROM zcl_abapgit_objects_super
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .

    TYPES:
      ty_wwwparams_tt TYPE STANDARD TABLE OF wwwparams WITH DEFAULT KEY .

    CONSTANTS:
      BEGIN OF c_param_names,
        version  TYPE w3_name VALUE 'version',
        fileext  TYPE w3_name VALUE 'fileextension',
        filesize TYPE w3_name VALUE 'filesize',
        filename TYPE w3_name VALUE 'filename',
        mimetype TYPE w3_name VALUE 'mimetype',
      END OF c_param_names .

    METHODS constructor
      IMPORTING
        !is_item     TYPE zif_abapgit_definitions=>ty_item
        !iv_language TYPE spras .
  PROTECTED SECTION.
    TYPES ty_bdcdata TYPE STANDARD TABLE OF bdcdata
                           WITH NON-UNIQUE DEFAULT KEY.

    METHODS get_metadata REDEFINITION.

    METHODS change_bdc_jump_data ABSTRACT
      CHANGING
        ct_bdcdata TYPE ty_bdcdata.

  PRIVATE SECTION.

    DATA ms_key TYPE wwwdatatab.

    METHODS get_ext
      IMPORTING it_params     TYPE ty_wwwparams_tt
      RETURNING VALUE(rv_ext) TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS normalize_params
      IMPORTING iv_size   TYPE i
      CHANGING  ct_params TYPE ty_wwwparams_tt  " Param table to patch
      RAISING   zcx_abapgit_exception.

    METHODS strip_params
      CHANGING ct_params TYPE ty_wwwparams_tt
      RAISING  zcx_abapgit_exception.

    METHODS find_param
      IMPORTING it_params       TYPE ty_wwwparams_tt
                iv_name         TYPE w3_name
      RETURNING VALUE(rv_value) TYPE string
      RAISING   zcx_abapgit_exception.

ENDCLASS.
CLASS zcl_abapgit_object_w3ht DEFINITION  INHERITING FROM zcl_abapgit_object_w3xx_super FINAL.

  PROTECTED SECTION.
    METHODS: change_bdc_jump_data REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.
CLASS zcl_abapgit_object_w3mi DEFINITION  INHERITING FROM zcl_abapgit_object_w3xx_super FINAL.

  PROTECTED SECTION.
    METHODS: change_bdc_jump_data REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.
CLASS zcl_abapgit_oo_interface DEFINITION

  INHERITING FROM zcl_abapgit_oo_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_abapgit_oo_object_fnc~create
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~delete
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~get_includes
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~get_interface_properties
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~deserialize_source
        REDEFINITION .
  PROTECTED SECTION.
    TYPES: ty_char1 TYPE c LENGTH 1,
           ty_char2 TYPE c LENGTH 2.
  PRIVATE SECTION.

    CLASS-METHODS update_report
      IMPORTING
        !iv_program       TYPE programm
        !it_source        TYPE string_table
      RETURNING
        VALUE(rv_updated) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS update_meta
      IMPORTING
        !iv_name   TYPE seoclsname
        !it_source TYPE rswsourcet
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS init_scanner
      IMPORTING
        !it_source        TYPE zif_abapgit_definitions=>ty_string_tt
        !iv_name          TYPE seoclsname
      RETURNING
        VALUE(ro_scanner) TYPE REF TO cl_oo_source_scanner_interface
      RAISING
        zcx_abapgit_exception .
ENDCLASS.
CLASS zcl_abapgit_oo_factory DEFINITION .

  PUBLIC SECTION.
    CLASS-METHODS:
      make
        IMPORTING
          iv_object_type                   TYPE tadir-object
        RETURNING
          VALUE(ri_object_oriented_object) TYPE REF TO zif_abapgit_oo_object_fnc.
  PRIVATE SECTION.

    CLASS-DATA gi_object_oriented_object TYPE REF TO zif_abapgit_oo_object_fnc .
ENDCLASS.
CLASS zcl_abapgit_oo_serializer DEFINITION

  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS serialize_abap_clif_source
      IMPORTING
        !is_class_key    TYPE seoclskey
      RETURNING
        VALUE(rt_source) TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING
        zcx_abapgit_exception
        cx_sy_dyn_call_error .
    METHODS are_test_classes_skipped
      RETURNING
        VALUE(rv_return) TYPE abap_bool .
    METHODS serialize_locals_imp
      IMPORTING
        !is_clskey       TYPE seoclskey
      RETURNING
        VALUE(rt_source) TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_locals_def
      IMPORTING
        !is_clskey       TYPE seoclskey
      RETURNING
        VALUE(rt_source) TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_testclasses
      IMPORTING
        !is_clskey       TYPE seoclskey
      RETURNING
        VALUE(rt_source) TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_macros
      IMPORTING
        !is_clskey       TYPE seoclskey
      RETURNING
        VALUE(rt_source) TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_skip_testclass TYPE abap_bool .

    METHODS calculate_skip_testclass
      IMPORTING
        !it_source               TYPE zif_abapgit_definitions=>ty_string_tt
      RETURNING
        VALUE(rv_skip_testclass) TYPE abap_bool .
    METHODS serialize_abap_old
      IMPORTING
        !is_clskey       TYPE seoclskey
      RETURNING
        VALUE(rt_source) TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_abap_new
      IMPORTING
        !is_clskey       TYPE seoclskey
      RETURNING
        VALUE(rt_source) TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING
        zcx_abapgit_exception
        cx_sy_dyn_call_error .
    METHODS remove_signatures
      CHANGING
        !ct_source TYPE zif_abapgit_definitions=>ty_string_tt .
    METHODS read_include
      IMPORTING
        !is_clskey       TYPE seoclskey
        !iv_type         TYPE seop_include_ext_app
      RETURNING
        VALUE(rt_source) TYPE seop_source_string .
    METHODS reduce
      CHANGING
        !ct_source TYPE zif_abapgit_definitions=>ty_string_tt .
ENDCLASS.
CLASS zcl_abapgit_path DEFINITION

  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS split_file_location
      IMPORTING iv_fullpath TYPE string
      EXPORTING ev_path     TYPE string
                ev_filename TYPE string.

    CLASS-METHODS is_root
      IMPORTING iv_path       TYPE string
      RETURNING VALUE(rv_yes) TYPE abap_bool.

    CLASS-METHODS is_subdir
      IMPORTING iv_path       TYPE string
                iv_parent     TYPE string
      RETURNING VALUE(rv_yes) TYPE abap_bool.

    CLASS-METHODS change_dir
      IMPORTING iv_cur_dir     TYPE string
                iv_cd          TYPE string
      RETURNING VALUE(rv_path) TYPE string.

    CLASS-METHODS get_filename_from_syspath
      IMPORTING iv_path            TYPE string
      RETURNING VALUE(rv_filename) TYPE string.

ENDCLASS.
CLASS zcl_abapgit_progress DEFINITION

  FINAL
  CREATE PROTECTED .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_progress .

    CLASS-METHODS set_instance
      IMPORTING
        !ii_progress TYPE REF TO zif_abapgit_progress .
    CLASS-METHODS get_instance
      IMPORTING
        !iv_total          TYPE i
      RETURNING
        VALUE(ri_progress) TYPE REF TO zif_abapgit_progress .
  PROTECTED SECTION.

    DATA mv_total TYPE i .
    CLASS-DATA gi_progress TYPE REF TO zif_abapgit_progress .

    METHODS calc_pct
      IMPORTING
        !iv_current   TYPE i
      RETURNING
        VALUE(rv_pct) TYPE i .
  PRIVATE SECTION.

    DATA mv_cv_time_next TYPE sy-uzeit .
    DATA mv_cv_datum_next TYPE sy-datum .
ENDCLASS.
CLASS zcl_abapgit_sap_package DEFINITION
     CREATE PRIVATE
     FRIENDS zcl_abapinst_factory.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_package TYPE devclass.

    INTERFACES: zif_abapgit_sap_package.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mv_package TYPE devclass.

ENDCLASS.
CLASS zcl_abapgit_skip_objects DEFINITION  FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      skip_sadl_generated_objects
        IMPORTING
          it_tadir        TYPE zif_abapgit_definitions=>ty_tadir_tt
          ii_log          TYPE REF TO zif_abapgit_log OPTIONAL
        RETURNING
          VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      has_sadl_superclass
        IMPORTING
          is_class         TYPE zif_abapgit_definitions=>ty_tadir
        RETURNING
          VALUE(rv_return) TYPE abap_bool.

ENDCLASS.
CLASS zcl_abapgit_sotr_handler DEFINITION

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_sotr_use_tt TYPE STANDARD TABLE OF sotr_use WITH DEFAULT KEY .

    CLASS-METHODS read_sotr
      IMPORTING
        !iv_pgmid    TYPE pgmid
        !iv_object   TYPE trobjtype
        !iv_obj_name TYPE csequence
        !io_xml      TYPE REF TO zif_abapgit_xml_output OPTIONAL
        !iv_language TYPE spras OPTIONAL
      EXPORTING
        !et_sotr     TYPE zif_abapgit_definitions=>ty_sotr_tt
        !et_sotr_use TYPE zif_abapgit_definitions=>ty_sotr_use_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS create_sotr
      IMPORTING
        !iv_package TYPE devclass
        !io_xml     TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
    CLASS-METHODS get_sotr_usage
      IMPORTING
        !iv_pgmid          TYPE pgmid
        !iv_object         TYPE trobjtype
        !iv_obj_name       TYPE csequence
      RETURNING
        VALUE(rt_sotr_use) TYPE ty_sotr_use_tt.

    CLASS-METHODS get_sotr_4_concept
      IMPORTING
        !iv_concept    TYPE sotr_conc
      RETURNING
        VALUE(rs_sotr) TYPE zif_abapgit_definitions=>ty_sotr .
  PRIVATE SECTION.
ENDCLASS.
CLASS zcl_abapgit_tadir DEFINITION

  FINAL
  CREATE PRIVATE
   FRIENDS zcl_abapinst_factory .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_tadir .

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS exists
      IMPORTING
        !is_item         TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_exists) TYPE abap_bool .
    METHODS check_exists
      IMPORTING
        !it_tadir       TYPE zif_abapgit_definitions=>ty_tadir_tt
      RETURNING
        VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS build
      IMPORTING
        !iv_package            TYPE tadir-devclass
        !io_dot                TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_ignore_subpackages TYPE abap_bool DEFAULT abap_false
        !iv_only_local_objects TYPE abap_bool DEFAULT abap_false
        !ii_log                TYPE REF TO zif_abapgit_log OPTIONAL
      RETURNING
        VALUE(rt_tadir)        TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS select_objects
      IMPORTING
        !iv_package            TYPE tadir-devclass
        !iv_ignore_subpackages TYPE abap_bool DEFAULT abap_false
        !iv_only_local_objects TYPE abap_bool
      EXPORTING
        !et_packages           TYPE zif_abapgit_sap_package=>ty_devclass_tt
        !et_tadir              TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS skip_objects
      IMPORTING
        !iv_package TYPE tadir-devclass
        !io_dot     TYPE REF TO zcl_abapgit_dot_abapgit
        !ii_log     TYPE REF TO zif_abapgit_log OPTIONAL
      CHANGING
        !ct_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS add_local_packages
      IMPORTING
        !it_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt
      CHANGING
        !ct_tadir    TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS add_namespaces
      IMPORTING
        !iv_package TYPE devclass
      CHANGING
        !ct_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS determine_path
      IMPORTING
        !iv_package TYPE tadir-devclass
        !io_dot     TYPE REF TO zcl_abapgit_dot_abapgit
      CHANGING
        !ct_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS adjust_objects
      IMPORTING
        !iv_package TYPE tadir-devclass
        !io_dot     TYPE REF TO zcl_abapgit_dot_abapgit
      CHANGING
        !ct_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
ENDCLASS.
CLASS zcl_abapgit_url DEFINITION

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS validate
      IMPORTING
        !iv_url TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS host
      IMPORTING
        !iv_url        TYPE string
      RETURNING
        VALUE(rv_host) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS name
      IMPORTING
        !iv_url        TYPE string
        !iv_validate   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_name) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS path_name
      IMPORTING
        !iv_url             TYPE string
      RETURNING
        VALUE(rv_path_name) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS is_abapgit_repo
      IMPORTING
        !iv_url           TYPE string
      RETURNING
        VALUE(rv_abapgit) TYPE abap_bool .
    CLASS-METHODS url_address
      IMPORTING
        !iv_url          TYPE string
      RETURNING
        VALUE(rv_adress) TYPE string
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS regex
      IMPORTING
        !iv_url  TYPE string
      EXPORTING
        !ev_host TYPE string
        !ev_path TYPE string
        !ev_name TYPE string
      RAISING
        zcx_abapgit_exception .
ENDCLASS.
CLASS zcl_abapgit_version DEFINITION

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS normalize
      IMPORTING
        !iv_version       TYPE string
      RETURNING
        VALUE(rv_version) TYPE string .
    CLASS-METHODS conv_str_to_version
      IMPORTING
        !iv_version       TYPE csequence
      RETURNING
        VALUE(rs_version) TYPE zif_abapgit_definitions=>ty_version
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS check_dependant_version
      IMPORTING
        !is_current   TYPE zif_abapgit_definitions=>ty_version
        !is_dependant TYPE zif_abapgit_definitions=>ty_version
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS compare
      IMPORTING
        !iv_a            TYPE string OPTIONAL
        !iv_b            TYPE string OPTIONAL
        !is_a            TYPE zif_abapgit_definitions=>ty_version OPTIONAL
        !is_b            TYPE zif_abapgit_definitions=>ty_version OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE i .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS version_to_numeric
      IMPORTING
        !iv_version       TYPE string
      RETURNING
        VALUE(rv_version) TYPE i.
ENDCLASS.
CLASS zcl_abapgit_xml_pretty DEFINITION

  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS print
      IMPORTING
        !iv_xml           TYPE string
        !iv_ignore_errors TYPE abap_bool DEFAULT abap_true
        !iv_unpretty      TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_xml)     TYPE string
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS zcl_abapinst_factory DEFINITION

  CREATE PRIVATE .

  " This is a replacement for ZCL_ABAPGIT_FACTORY
  "
  " Using ZCL_ABAPGIT_FACTORY would drag in many other dependencies
  " which are completely unnecessary for abapinst (like the abapGit UI layer).

  " Note: This class will show errors when trying to activated it!
  "
  " One must ignore errors like 'An instance of the class
  " "ZCL_ABAPGIT_..." cannot be created outside the class.'
  " and activate the class anyway.
  "
  " Once the class is embedded into the standalone program as a local class,
  " it will work just fine.
  PUBLIC SECTION.

    CLASS-METHODS get_tadir
      RETURNING
        VALUE(ri_tadir) TYPE REF TO zif_abapgit_tadir .
    CLASS-METHODS get_sap_package
      IMPORTING
        !iv_package           TYPE devclass
      RETURNING
        VALUE(ri_sap_package) TYPE REF TO zif_abapgit_sap_package .
    CLASS-METHODS get_environment
      RETURNING
        VALUE(ri_environment) TYPE REF TO zif_abapgit_environment .
    CLASS-METHODS get_longtexts
      RETURNING
        VALUE(ri_longtexts) TYPE REF TO zif_abapgit_longtexts .
    CLASS-METHODS get_gui_functions
      RETURNING
        VALUE(ri_gui_functions) TYPE REF TO zif_abapgit_gui_functions .
    CLASS-METHODS get_lxe_texts
      RETURNING
        VALUE(ri_lxe_texts) TYPE REF TO zif_abapgit_lxe_texts .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_sap_package,
        package  TYPE devclass,
        instance TYPE REF TO zif_abapgit_sap_package,
      END OF ty_sap_package .
    TYPES:
      ty_sap_packages TYPE HASHED TABLE OF ty_sap_package
                                        WITH UNIQUE KEY package .

    CLASS-DATA gi_tadir TYPE REF TO zif_abapgit_tadir .
    CLASS-DATA gt_sap_package TYPE ty_sap_packages .
    CLASS-DATA gi_environment TYPE REF TO zif_abapgit_environment .
    CLASS-DATA gi_longtext TYPE REF TO zif_abapgit_longtexts .
    CLASS-DATA gi_gui_functions TYPE REF TO zcl_abapgit_gui_functions .
    CLASS-DATA gi_lxe_texts TYPE REF TO zif_abapgit_lxe_texts .
ENDCLASS.
CLASS zcl_abapinst_file DEFINITION

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS load_internet
      IMPORTING
        !iv_url            TYPE string
        !iv_user           TYPE string
        !iv_password       TYPE string
        !iv_proxy_host     TYPE string
        !iv_proxy_port     TYPE string
        !iv_proxy_user     TYPE string
        !iv_proxy_password TYPE string
      RETURNING
        VALUE(rv_file)     TYPE xstring
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS load_local
      IMPORTING
        !iv_filename   TYPE csequence
      RETURNING
        VALUE(rv_file) TYPE xstring
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS load_server
      IMPORTING
        !iv_filename   TYPE csequence
      RETURNING
        VALUE(rv_file) TYPE xstring
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS virus_scan
      IMPORTING
        !iv_data TYPE xstring
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS unzip
      IMPORTING
        !iv_xstr        TYPE xstring
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapinst_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS _filename
      IMPORTING
        !iv_str      TYPE string
      EXPORTING
        !ev_path     TYPE string
        !ev_filename TYPE string
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS _normalize_path
      CHANGING
        !ct_files TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapinst_exception .
ENDCLASS.
CLASS zcl_abapinst_file_status DEFINITION

  FINAL
  CREATE PUBLIC .

  " This is a replacement for ZCL_ABAPGIT_FILE_STATUS
  "
  " Using ZCL_ABAPGIT_FILE_STATUS would drag in many other dependencies
  " which are completely unnecessary for abapinst (like the abapGit repo layer).

  PUBLIC SECTION.

    CLASS-METHODS status
      IMPORTING
        !iv_package         TYPE devclass
        !it_local           TYPE zif_abapgit_definitions=>ty_files_item_tt
        !it_local_checksums TYPE zif_abapgit_definitions=>ty_file_signatures_tt
        !it_remote          TYPE zif_abapgit_definitions=>ty_files_tt
        !io_dot             TYPE REF TO zcl_abapgit_dot_abapgit
        !ii_log             TYPE REF TO zif_abapgit_log OPTIONAL
      RETURNING
        VALUE(rt_results)   TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS identify_object
      IMPORTING
        !iv_filename TYPE string
        !iv_path     TYPE string
        !iv_devclass TYPE devclass OPTIONAL
        !io_dot      TYPE REF TO zcl_abapgit_dot_abapgit
      EXPORTING
        !es_item     TYPE zif_abapgit_definitions=>ty_item
        !ev_is_xml   TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS calculate_status
      IMPORTING
        !iv_devclass      TYPE devclass
        !io_dot           TYPE REF TO zcl_abapgit_dot_abapgit
        !it_local         TYPE zif_abapgit_definitions=>ty_files_item_tt
        !it_remote        TYPE zif_abapgit_definitions=>ty_files_tt
        !it_cur_state     TYPE zif_abapgit_definitions=>ty_file_signatures_tt
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS run_checks
      IMPORTING
        !ii_log     TYPE REF TO zif_abapgit_log
        !it_results TYPE zif_abapgit_definitions=>ty_results_tt
        !io_dot     TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_top     TYPE devclass
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS build_existing
      IMPORTING
        !is_local        TYPE zif_abapgit_definitions=>ty_file_item
        !is_remote       TYPE zif_abapgit_definitions=>ty_file
        !it_state        TYPE zif_abapgit_definitions=>ty_file_signatures_ts
      RETURNING
        VALUE(rs_result) TYPE zif_abapgit_definitions=>ty_result .
    CLASS-METHODS build_new_local
      IMPORTING
        !is_local        TYPE zif_abapgit_definitions=>ty_file_item
      RETURNING
        VALUE(rs_result) TYPE zif_abapgit_definitions=>ty_result .
    CLASS-METHODS build_new_remote
      IMPORTING
        !iv_devclass     TYPE devclass
        !io_dot          TYPE REF TO zcl_abapgit_dot_abapgit
        !is_remote       TYPE zif_abapgit_definitions=>ty_file
        !it_items        TYPE zif_abapgit_definitions=>ty_items_ts
        !it_state        TYPE zif_abapgit_definitions=>ty_file_signatures_ts
      RETURNING
        VALUE(rs_result) TYPE zif_abapgit_definitions=>ty_result
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_object_package
      IMPORTING
        !iv_object         TYPE tadir-object
        !iv_obj_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rv_devclass) TYPE devclass
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS check_package_move
      IMPORTING
        !ii_log     TYPE REF TO zif_abapgit_log
        !it_results TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS check_files_folder
      IMPORTING
        !ii_log     TYPE REF TO zif_abapgit_log
        !it_results TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS check_package_folder
      IMPORTING
        !ii_log     TYPE REF TO zif_abapgit_log
        !it_results TYPE zif_abapgit_definitions=>ty_results_tt
        !io_dot     TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_top     TYPE devclass
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS check_multiple_files
      IMPORTING
        !ii_log     TYPE REF TO zif_abapgit_log
        !it_results TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
ENDCLASS.
CLASS zcl_abapinst_popups DEFINITION

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_alv_column,
        name   TYPE string,
        text   TYPE string,
        length TYPE lvc_outlen,
        key    TYPE abap_bool,
      END OF ty_alv_column .
    TYPES:
      ty_alv_column_tt TYPE STANDARD TABLE OF ty_alv_column WITH KEY name .

    CONSTANTS c_default_column TYPE lvc_fname VALUE `DEFAULT_COLUMN` ##NO_TEXT.

    METHODS popup_to_enter_packaging
      IMPORTING
        !iv_name            TYPE csequence OPTIONAL
        !iv_version         TYPE csequence OPTIONAL
      RETURNING
        VALUE(rs_packaging) TYPE zif_abapgit_dot_abapgit=>ty_packaging
      RAISING
        zcx_abapinst_exception .
    METHODS popup_to_confirm
      IMPORTING
        !iv_title                 TYPE csequence
        !iv_question              TYPE csequence
        !iv_text_button_1         TYPE csequence DEFAULT 'Yes'
        !iv_icon_button_1         TYPE icon-name DEFAULT space
        !iv_text_button_2         TYPE csequence DEFAULT 'No'
        !iv_icon_button_2         TYPE icon-name DEFAULT space
        !iv_default_button        TYPE sy-input DEFAULT '1'
        !iv_display_cancel_button TYPE sy-input DEFAULT abap_true
      RETURNING
        VALUE(rv_answer)          TYPE sy-input
      RAISING
        zcx_abapinst_exception .
    METHODS popup_to_select_from_list
      IMPORTING
        !it_list               TYPE STANDARD TABLE
        !iv_title              TYPE lvc_title DEFAULT space
        !iv_header_text        TYPE csequence DEFAULT space
        !iv_start_column       TYPE i DEFAULT 2
        !iv_end_column         TYPE i DEFAULT 65
        !iv_start_line         TYPE i DEFAULT 8
        !iv_end_line           TYPE i DEFAULT 20
        !iv_striped_pattern    TYPE abap_bool DEFAULT abap_false
        !iv_optimize_col_width TYPE abap_bool DEFAULT abap_true
        !iv_selection_mode     TYPE salv_de_constant DEFAULT if_salv_c_selection_mode=>multiple
        !iv_select_column_text TYPE csequence DEFAULT space
        !it_columns_to_display TYPE ty_alv_column_tt
      EXPORTING
        VALUE(et_list)         TYPE STANDARD TABLE
      RAISING
        zcx_abapinst_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_fieldname_selected TYPE lvc_fname VALUE `SELECTED` ##NO_TEXT.
    DATA mo_select_list_popup TYPE REF TO cl_salv_table .
    DATA mr_table TYPE REF TO data .
    DATA mv_cancel TYPE abap_bool VALUE abap_false ##NO_TEXT.
    DATA mo_table_descr TYPE REF TO cl_abap_tabledescr .

    METHODS _create_new_table
      IMPORTING
        !it_list TYPE STANDARD TABLE .
    METHODS _get_selected_rows
      EXPORTING
        !et_list TYPE INDEX TABLE .
    METHODS _on_select_list_link_click
        FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        !row
        !column .
    METHODS _on_select_list_function_click
        FOR EVENT added_function OF cl_salv_events_table
      IMPORTING
        !e_salv_function .
    METHODS _on_double_click
        FOR EVENT double_click OF cl_salv_events_table
      IMPORTING
        !row
        !column .
ENDCLASS.
CLASS zcl_abapinst_installer DEFINITION

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF ty_enum_zip,
        local    TYPE i VALUE 0,
        internet TYPE i VALUE 1,
        server   TYPE i VALUE 2,
        data     TYPE i VALUE 3,
      END OF ty_enum_zip .
    CONSTANTS:
      BEGIN OF ty_enum_package,
        default       TYPE i VALUE 0,
        local         TYPE i VALUE 1,
        transportable TYPE i VALUE 2,
      END OF ty_enum_package .
    CONSTANTS:
      BEGIN OF ty_enum_transport,
        prompt   TYPE i VALUE 0,
        existing TYPE i VALUE 1,
      END OF ty_enum_transport .
    CONSTANTS:
      BEGIN OF ty_enum_folder_logic,
        default TYPE i VALUE 0,
        prefix  TYPE i VALUE 1,
        full    TYPE i VALUE 2,
      END OF ty_enum_folder_logic .

    CLASS-METHODS init
      IMPORTING
        !iv_tabname TYPE tabname OPTIONAL
        !iv_lock    TYPE viewname OPTIONAL
        !iv_name    TYPE string OPTIONAL
        !iv_names   TYPE string OPTIONAL .
    CLASS-METHODS install
      IMPORTING
        !iv_enum_zip          TYPE i OPTIONAL
        !iv_name              TYPE char255 OPTIONAL
        !iv_data              TYPE xstring OPTIONAL
        !iv_enum_package      TYPE i OPTIONAL
        !iv_package           TYPE devclass OPTIONAL
        !iv_dlvunit           TYPE dlvunit OPTIONAL
        !iv_devlayer          TYPE devlayer OPTIONAL
        !iv_enum_transport    TYPE i OPTIONAL
        !iv_transport         TYPE trkorr OPTIONAL
        !iv_user              TYPE char255 OPTIONAL
        !iv_password          TYPE char255 OPTIONAL
        !iv_proxy_host        TYPE char255 OPTIONAL
        !iv_proxy_service     TYPE char5 OPTIONAL
        !iv_proxy_user        TYPE char255 OPTIONAL
        !iv_proxy_password    TYPE char255 OPTIONAL
        !iv_enum_folder_logic TYPE i OPTIONAL
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS uninstall
      IMPORTING
        !iv_name TYPE zif_abapinst_definitions=>ty_name
        !iv_pack TYPE zif_abapinst_definitions=>ty_pack
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS check
      IMPORTING
        !iv_name         TYPE zif_abapinst_definitions=>ty_name OPTIONAL
        !iv_pack         TYPE zif_abapinst_definitions=>ty_pack OPTIONAL
        !is_sem_version  TYPE zif_abapinst_definitions=>ty_version OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS list
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS f4
      RETURNING
        VALUE(rs_inst) TYPE zif_abapinst_definitions=>ty_inst
      RAISING
        zcx_abapinst_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_db TYPE REF TO zcl_abapinst_persistence .
    CLASS-DATA gt_remote TYPE zif_abapgit_definitions=>ty_files_tt .
    CLASS-DATA gs_inst TYPE zif_abapinst_definitions=>ty_inst .
    CLASS-DATA go_dot TYPE REF TO zcl_abapgit_dot_abapgit .
    CLASS-DATA gi_log TYPE REF TO zif_abapgit_log .
    CLASS-DATA gs_packaging TYPE zif_abapgit_dot_abapgit=>ty_packaging .
    CLASS-DATA gv_name TYPE string .
    CLASS-DATA gv_names TYPE string .
    CLASS-DATA:
      gt_clmcus TYPE STANDARD TABLE OF clmcus WITH DEFAULT KEY .
    CONSTANTS c_success TYPE sy-msgty VALUE 'S' ##NO_TEXT.
    CONSTANTS c_warning TYPE sy-msgty VALUE 'W' ##NO_TEXT.
    CONSTANTS c_error TYPE sy-msgty VALUE 'E' ##NO_TEXT.

    CLASS-METHODS _clear .
    CLASS-METHODS _nothing_found
      IMPORTING
        !it_list         TYPE ANY TABLE
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
    CLASS-METHODS _files
      IMPORTING
        !iv_enum_zip       TYPE i
        !iv_name           TYPE char255 OPTIONAL
        !iv_data           TYPE xstring OPTIONAL
        !iv_user           TYPE char255 OPTIONAL
        !iv_password       TYPE char255 OPTIONAL
        !iv_proxy_host     TYPE char255 OPTIONAL
        !iv_proxy_service  TYPE char5 OPTIONAL
        !iv_proxy_user     TYPE char255 OPTIONAL
        !iv_proxy_password TYPE char255 OPTIONAL
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS _packaging
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS _sap_package
      IMPORTING
        !iv_enum_package TYPE i
        !iv_package      TYPE devclass OPTIONAL
        !iv_dlvunit      TYPE dlvunit OPTIONAL
        !iv_devlayer     TYPE devlayer OPTIONAL
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS _check
      IMPORTING
        !iv_force TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS _folder_logic
      IMPORTING
        !iv_enum_folder_logic TYPE i
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS _transport
      IMPORTING
        !iv_enum_transport TYPE i
        !iv_transport      TYPE trkorr OPTIONAL
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS _transport_check
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS _transport_reset .
    CLASS-METHODS _namespaces
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS _confirm_messages .
    CLASS-METHODS _restore_messages .
    CLASS-METHODS _save
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS _load
      IMPORTING
        !iv_name TYPE zif_abapinst_definitions=>ty_name
        !iv_pack TYPE zif_abapinst_definitions=>ty_pack
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS _delete
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS _check_uninstalled
      IMPORTING
        !it_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt .
    CLASS-METHODS _log_start .
    CLASS-METHODS _log_end
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS _final_message
      IMPORTING
        !iv_type TYPE string .
    CLASS-METHODS _find_remote_dot_abapgit
      IMPORTING
        !it_remote    TYPE zif_abapgit_definitions=>ty_files_tt
      RETURNING
        VALUE(ro_dot) TYPE REF TO zcl_abapgit_dot_abapgit
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS _find_remote_dot_apack
      IMPORTING
        !it_remote    TYPE zif_abapgit_definitions=>ty_files_tt
      RETURNING
        VALUE(ro_dot) TYPE REF TO zcl_abapgit_dot_abapgit
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS _find_remote_namespaces
      RETURNING
        VALUE(rt_remote) TYPE zif_abapgit_definitions=>ty_files_tt .
    CLASS-METHODS _uninstall_sotr
      IMPORTING
        !it_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt .
ENDCLASS.
CLASS zcl_abapinst_log_viewer DEFINITION

  FINAL
  CREATE PUBLIC .

  " This is a replacement for ZCL_ABAPGIT_LOG_VIEWER
  "
  " Using ZCL_ABAPGIT_OBJECTS would drag in other dependencies
  " which are completely unnecessary for abapinst (like the abapGit UI layer).

  PUBLIC SECTION.

    CLASS-METHODS show_log
      IMPORTING
        !ii_log TYPE REF TO zif_abapgit_log .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_log_out,
        type      TYPE icon_d,
        msg       TYPE string,
        obj_type  TYPE trobjtype,
        obj_name  TYPE sobj_name,
        exception TYPE REF TO cx_root,
        longtext  TYPE icon_d,
        t100      TYPE icon_d,
        source    TYPE icon_d,
        callstack TYPE icon_d,
        cell_type TYPE salv_t_int4_column,
      END OF ty_log_out .
    TYPES:
      ty_log_outs TYPE STANDARD TABLE OF ty_log_out
                                  WITH NON-UNIQUE DEFAULT KEY .

    CLASS-DATA gt_log TYPE ty_log_outs .

    CLASS-METHODS prepare_log_for_display
      IMPORTING
        !ii_log           TYPE REF TO zif_abapgit_log
      RETURNING
        VALUE(rt_log_out) TYPE ty_log_outs .
    CLASS-METHODS show_longtext
      IMPORTING
        !is_log TYPE ty_log_out
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS on_link_click
        FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        !row
        !column .
    CLASS-METHODS dispatch
      IMPORTING
        !is_log    TYPE ty_log_out
        !iv_column TYPE salv_de_column
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS calculate_cell_type .
ENDCLASS.
CLASS zcl_abapinst_objects DEFINITION

  CREATE PUBLIC .

  " This is a replacement for ZCL_ABAPGIT_OBJECTS
  "
  " Using ZCL_ABAPGIT_OBJECTS would drag in many other dependencies
  " which are completely unnecessary for abapinst (like the abapGit repo layer).
  "
  " The serialize logic has been removed as well since the complete abapGit package
  " is being deployed. This simplifies a lot but could be added back in case a
  " delta logic is required.

  PUBLIC SECTION.

    TYPES:
      ty_types_tt TYPE SORTED TABLE OF tadir-object WITH UNIQUE KEY table_line .

    CLASS-METHODS deserialize
      IMPORTING
        !iv_package              TYPE devclass
        !iv_language             TYPE spras
        !iv_transport            TYPE trkorr OPTIONAL
        !it_local                TYPE zif_abapgit_definitions=>ty_files_item_tt OPTIONAL
        !it_local_checksums      TYPE zif_abapgit_definitions=>ty_file_signatures_tt OPTIONAL
        !it_remote               TYPE zif_abapgit_definitions=>ty_files_tt
        !io_dot                  TYPE REF TO zcl_abapgit_dot_abapgit
        !ii_log                  TYPE REF TO zif_abapgit_log
      RETURNING
        VALUE(rt_accessed_files) TYPE zif_abapgit_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS delete
      IMPORTING
        !it_tadir     TYPE zif_abapgit_definitions=>ty_tadir_tt
        !iv_transport TYPE trkorr OPTIONAL
        !ii_log       TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS is_supported
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_native_only TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_bool)  TYPE abap_bool .
    CLASS-METHODS exists
      IMPORTING
        !is_item       TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_bool) TYPE abap_bool .
    CLASS-METHODS supported_list
      RETURNING
        VALUE(rt_types) TYPE ty_types_tt .
    CLASS-METHODS is_active
      IMPORTING
        !is_item         TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_active) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_obj_serializer_item,
        item     TYPE zif_abapgit_definitions=>ty_item,
        metadata TYPE zif_abapgit_definitions=>ty_metadata,
      END OF ty_obj_serializer_item .
    TYPES:
      ty_obj_serializer_map
                TYPE SORTED TABLE OF ty_obj_serializer_item WITH UNIQUE KEY item .

    CLASS-DATA gt_obj_serializer_map TYPE ty_obj_serializer_map .
    CLASS-DATA gt_supported_obj_types TYPE ty_types_tt .

    CLASS-METHODS files_to_deserialize
      IMPORTING
        !iv_package         TYPE devclass
        !it_local           TYPE zif_abapgit_definitions=>ty_files_item_tt
        !it_local_checksums TYPE zif_abapgit_definitions=>ty_file_signatures_tt
        !it_remote          TYPE zif_abapgit_definitions=>ty_files_tt
        !io_dot             TYPE REF TO zcl_abapgit_dot_abapgit
        !ii_log             TYPE REF TO zif_abapgit_log OPTIONAL
      RETURNING
        VALUE(rt_results)   TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS prioritize_deser
      IMPORTING
        !it_results       TYPE zif_abapgit_definitions=>ty_results_tt
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt .
    CLASS-METHODS class_name
      IMPORTING
        !is_item             TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_class_name) TYPE string .
    CLASS-METHODS update_package_tree
      IMPORTING
        !iv_package TYPE devclass .
    CLASS-METHODS delete_object
      IMPORTING
        !iv_package TYPE devclass
        !is_item    TYPE zif_abapgit_definitions=>ty_item
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS compare_remote_to_local
      IMPORTING
        !ii_object TYPE REF TO zif_abapgit_object
        !it_remote TYPE zif_abapgit_definitions=>ty_files_tt
        !is_result TYPE zif_abapgit_definitions=>ty_result
        !ii_log    TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS deserialize_objects
      IMPORTING
        !is_step  TYPE zif_abapgit_objects=>ty_step_data
        !ii_log   TYPE REF TO zif_abapgit_log
      CHANGING
        !ct_files TYPE zif_abapgit_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS check_objects_locked
      IMPORTING
        !iv_language TYPE spras
        !it_items    TYPE zif_abapgit_definitions=>ty_items_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS create_object
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_language    TYPE spras
        !is_metadata    TYPE zif_abapgit_definitions=>ty_metadata OPTIONAL
        !iv_native_only TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_obj)   TYPE REF TO zif_abapgit_object
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS map_tadir_to_items
      IMPORTING
        !it_tadir       TYPE zif_abapgit_definitions=>ty_tadir_tt
      RETURNING
        VALUE(rt_items) TYPE zif_abapgit_definitions=>ty_items_tt .
    CLASS-METHODS map_results_to_items
      IMPORTING
        !it_results     TYPE zif_abapgit_definitions=>ty_results_tt
      RETURNING
        VALUE(rt_items) TYPE zif_abapgit_definitions=>ty_items_tt .
    CLASS-METHODS filter_files_to_deserialize
      IMPORTING
        !it_results       TYPE zif_abapgit_definitions=>ty_results_tt
        !ii_log           TYPE REF TO zif_abapgit_log OPTIONAL
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt .
    CLASS-METHODS adjust_namespaces
      IMPORTING
        !it_results       TYPE zif_abapgit_definitions=>ty_results_tt
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt .
    CLASS-METHODS get_deserialize_steps
      RETURNING
        VALUE(rt_steps) TYPE zif_abapgit_objects=>ty_step_data_tt .
    CLASS-METHODS check_main_package
      IMPORTING
        !iv_package  TYPE devclass
        !iv_obj_type TYPE tadir-object
      RAISING
        zcx_abapgit_exception .
ENDCLASS.
CLASS zcl_abapinst_persistence DEFINITION

  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_tabname TYPE tabname DEFAULT zif_abapinst_definitions=>c_tabname
        !iv_lock    TYPE viewname DEFAULT zif_abapinst_definitions=>c_lock .
    CLASS-METHODS get_instance
      IMPORTING
        !iv_tabname  TYPE tabname DEFAULT zif_abapinst_definitions=>c_tabname
        !iv_lock     TYPE viewname DEFAULT zif_abapinst_definitions=>c_lock
      RETURNING
        VALUE(ro_db) TYPE REF TO zcl_abapinst_persistence .
    METHODS select
      IMPORTING
        !iv_name       TYPE zif_abapinst_definitions=>ty_name OPTIONAL
        !iv_pack       TYPE zif_abapinst_definitions=>ty_pack OPTIONAL
      RETURNING
        VALUE(rs_inst) TYPE zif_abapinst_definitions=>ty_inst
      RAISING
        zcx_abapinst_exception .
    METHODS insert
      IMPORTING
        !is_inst TYPE zif_abapinst_definitions=>ty_inst
      RAISING
        zcx_abapinst_exception ##SHADOW[INSERT].
    METHODS update
      IMPORTING
        !is_inst TYPE zif_abapinst_definitions=>ty_inst
      RAISING
        zcx_abapinst_exception .
    METHODS delete
      IMPORTING
        !iv_name TYPE zif_abapinst_definitions=>ty_name
        !iv_pack TYPE zif_abapinst_definitions=>ty_pack
      RAISING
        zcx_abapinst_exception .
    METHODS list
      RETURNING
        VALUE(rt_list) TYPE zif_abapinst_definitions=>ty_list
      RAISING
        zcx_abapinst_exception .
    METHODS list_by_name
      IMPORTING
        !iv_name       TYPE zif_abapinst_definitions=>ty_name
        !iv_pack       TYPE zif_abapinst_definitions=>ty_pack OPTIONAL
      RETURNING
        VALUE(rt_list) TYPE zif_abapinst_definitions=>ty_list
      RAISING
        zcx_abapinst_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_db TYPE REF TO zcl_abapinst_persistence .
    DATA mv_update_function TYPE funcname .
    DATA mv_tabname TYPE tabname .
    DATA mv_lock TYPE viewname .

    METHODS _update_function
      RETURNING
        VALUE(rv_funcname) TYPE funcname .
    METHODS _content_to_list
      IMPORTING
        !it_content    TYPE zif_abapinst_definitions=>ty_contents
      RETURNING
        VALUE(rt_list) TYPE zif_abapinst_definitions=>ty_list
      RAISING
        zcx_abapinst_exception .
    METHODS _content_to_inst
      IMPORTING
        !is_content    TYPE zif_abapinst_definitions=>ty_content
      RETURNING
        VALUE(rs_inst) TYPE zif_abapinst_definitions=>ty_inst
      RAISING
        zcx_abapinst_exception .
    METHODS _list_to_content
      IMPORTING
        !is_inst          TYPE zif_abapinst_definitions=>ty_inst
      RETURNING
        VALUE(rs_content) TYPE zif_abapinst_definitions=>ty_content
      RAISING
        zcx_abapinst_exception .
    METHODS _lock
      IMPORTING
        !iv_name TYPE zif_abapinst_definitions=>ty_name
        !iv_pack TYPE zif_abapinst_definitions=>ty_pack
        !iv_mode TYPE enqmode DEFAULT 'E'
      RAISING
        zcx_abapinst_exception .
ENDCLASS.
CLASS zcl_abapinst_screen DEFINITION

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS modify
      IMPORTING
        !iv_options TYPE abap_bool
        !iv_zip_i   TYPE abap_bool
        !iv_zip_f   TYPE abap_bool
        !iv_zip_s   TYPE abap_bool
        !iv_sap_l   TYPE abap_bool
        !iv_sap_t   TYPE abap_bool
        !iv_tsp_e   TYPE abap_bool
        !iv_conn_o  TYPE abap_bool
        !iv_prox_o  TYPE abap_bool
        !iv_mbt     TYPE abap_bool DEFAULT abap_false .
    CLASS-METHODS header
      IMPORTING
        !iv_icon         TYPE icon_d
        !iv_text         TYPE clike
      RETURNING
        VALUE(rv_header) TYPE fieldname .
    CLASS-METHODS icon
      IMPORTING
        !iv_name       TYPE clike
        !iv_text       TYPE clike
        !iv_info       TYPE clike
      RETURNING
        VALUE(rv_icon) TYPE string .
    CLASS-METHODS browser
      IMPORTING
        !iv_url TYPE csequence .
    CLASS-METHODS f4_file
      RETURNING
        VALUE(rv_file) TYPE char255 .
    CLASS-METHODS f4_transport
      IMPORTING
        !iv_package         TYPE devclass
        !iv_layer           TYPE devlayer OPTIONAL
        !iv_transport       TYPE trkorr OPTIONAL
      RETURNING
        VALUE(rv_transport) TYPE trkorr .
    CLASS-METHODS banner
      IMPORTING
        !iv_show TYPE abap_bool DEFAULT abap_true
        !iv_id   TYPE csequence OPTIONAL
        !iv_top  TYPE i DEFAULT 4
        !iv_left TYPE i DEFAULT 20
        !it_base TYPE zif_abapinst_definitions=>ty_base_tab OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: ty_url TYPE c LENGTH 255.

    CLASS-DATA go_banner_dock TYPE REF TO cl_gui_docking_container .
    CLASS-DATA go_banner TYPE REF TO cl_gui_picture .
    CLASS-DATA gv_banner_url TYPE ty_url.
ENDCLASS.
CLASS zcl_abapinst_setup DEFINITION

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !iv_tabname TYPE tabname DEFAULT zif_abapinst_definitions=>c_tabname
        !iv_lock    TYPE viewname DEFAULT zif_abapinst_definitions=>c_lock
        !iv_text    TYPE ddtext DEFAULT 'Generated by abapinst'
      RAISING
        zcx_abapinst_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gv_text TYPE string VALUE 'Generated by abapInst' ##NO_TEXT.
    CLASS-DATA gv_tabname TYPE tabname .
    CLASS-DATA gv_lock TYPE viewname .

    CLASS-METHODS _table_create
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS _table_exists
      RETURNING
        VALUE(rv_exists) TYPE abap_bool .
    CLASS-METHODS _lock_create
      RAISING
        zcx_abapinst_exception .
    CLASS-METHODS _lock_exists
      RETURNING
        VALUE(rv_exists) TYPE abap_bool .
    CLASS-METHODS _get_package
      RETURNING
        VALUE(rv_package) TYPE devclass .
ENDCLASS.
CLASS zcl_abapinst_textpool DEFINITION

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_program TYPE progname .
    METHODS set
      IMPORTING
        !iv_param TYPE csequence .
    METHODS save .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_program TYPE progname .
    DATA:
      mt_text_old TYPE TABLE OF textpool .
    DATA:
      mt_text_new TYPE TABLE OF textpool .

    METHODS _load .
ENDCLASS.
class zcl_ajson definition

  create private .

  public section.

    interfaces zif_ajson_reader .
    interfaces zif_ajson_writer .
    interfaces zif_ajson .

    aliases:
      exists for zif_ajson_reader~exists,
      members for zif_ajson_reader~members,
      get for zif_ajson_reader~get,
      get_boolean for zif_ajson_reader~get_boolean,
      get_integer for zif_ajson_reader~get_integer,
      get_number for zif_ajson_reader~get_number,
      get_date for zif_ajson_reader~get_date,
      get_string for zif_ajson_reader~get_string,
      slice for zif_ajson_reader~slice,
      to_abap for zif_ajson_reader~to_abap,
      array_to_string_table for zif_ajson_reader~array_to_string_table.

    aliases:
      clear for zif_ajson_writer~clear,
      set for zif_ajson_writer~set,
      set_boolean for zif_ajson_writer~set_boolean,
      set_string for zif_ajson_writer~set_string,
      set_integer for zif_ajson_writer~set_integer,
      set_date for zif_ajson_writer~set_date,
      set_null for zif_ajson_writer~set_null,
      delete for zif_ajson_writer~delete,
      touch_array for zif_ajson_writer~touch_array,
      push for zif_ajson_writer~push,
      stringify for zif_ajson_writer~stringify.

    aliases:
      mt_json_tree for zif_ajson~mt_json_tree,
      keep_item_order for zif_ajson~keep_item_order,
      freeze for zif_ajson~freeze.

    class-methods parse
      importing
        !iv_json           type string
        !iv_freeze         type abap_bool default abap_false
        !ii_custom_mapping type ref to zif_ajson_mapping optional
      returning
        value(ro_instance) type ref to zcl_ajson
      raising
        zcx_ajson_error .

    class-methods create_empty
      importing
        !ii_custom_mapping type ref to zif_ajson_mapping optional
      returning
        value(ro_instance) type ref to zcl_ajson.

  protected section.

  private section.

    types:
      tty_node_stack type standard table of ref to zif_ajson=>ty_node with default key.

    data mv_read_only type abap_bool.
    data mi_custom_mapping type ref to zif_ajson_mapping.
    data mv_keep_item_order type abap_bool.

    methods get_item
      importing
        iv_path        type string
      returning
        value(rv_item) type ref to zif_ajson=>ty_node.
    methods prove_path_exists
      importing
        iv_path              type string
      returning
        value(rt_node_stack) type tty_node_stack
      raising
        zcx_ajson_error.
    methods delete_subtree
      importing
        iv_path           type string
        iv_name           type string
      returning
        value(rv_deleted) type abap_bool.

endclass.
class lcl_mapping_fields definition.

  public section.
    interfaces zif_ajson_mapping.

    aliases to_abap for zif_ajson_mapping~to_abap.
    aliases to_json for zif_ajson_mapping~to_json.

    methods constructor
      importing
        it_mapping_fields type zif_ajson_mapping~ty_mapping_fields optional.

  protected section.

  private section.
    data mt_mapping_fields type zif_ajson_mapping~ty_mapping_fields.

endclass.


class lcl_mapping_to_upper definition.

  public section.
    interfaces zif_ajson_mapping.

    methods constructor
      importing
        it_mapping_fields type zif_ajson_mapping~ty_mapping_fields optional.

  protected section.

  private section.
    data mi_mapping_fields type ref to zif_ajson_mapping.

endclass.


class lcl_mapping_to_lower definition.

  public section.
    interfaces zif_ajson_mapping.

    methods constructor
      importing
        it_mapping_fields type zif_ajson_mapping~ty_mapping_fields optional.

  protected section.

  private section.
    data mi_mapping_fields type ref to zif_ajson_mapping.

endclass.


class lcl_mapping_camel definition.

  public section.
    interfaces zif_ajson_mapping.

    methods constructor
      importing
        it_mapping_fields   type zif_ajson_mapping~ty_mapping_fields optional
        iv_first_json_upper type abap_bool default abap_true.

  protected section.

  private section.
    data mv_first_json_upper type abap_bool.
    data mi_mapping_fields type ref to zif_ajson_mapping.

endclass.
class zcl_ajson_mapping definition

  final
  create public.

  public section.
    class-methods create_camel_case
      importing
        it_mapping_fields   type zif_ajson_mapping=>ty_mapping_fields optional
        iv_first_json_upper type abap_bool default abap_true
      returning
        value(ri_mapping)   type ref to zif_ajson_mapping.

    class-methods create_upper_case
      importing
        it_mapping_fields type zif_ajson_mapping=>ty_mapping_fields optional
      returning
        value(ri_mapping) type ref to zif_ajson_mapping.

    class-methods create_lower_case
      importing
        it_mapping_fields type zif_ajson_mapping=>ty_mapping_fields optional
      returning
        value(ri_mapping) type ref to zif_ajson_mapping.

    class-methods create_field_mapping
      importing
        it_mapping_fields type zif_ajson_mapping=>ty_mapping_fields
      returning
        value(ri_mapping) type ref to zif_ajson_mapping.

  protected section.

  private section.

endclass.
class zcl_ajson_utilities definition

  create public .

  public section.

    methods diff
      importing
        !iv_json_a type string optional
        !iv_json_b type string optional
        !io_json_a type ref to zif_ajson optional
        !io_json_b type ref to zif_ajson optional
      exporting
        !eo_insert type ref to zif_ajson
        !eo_delete type ref to zif_ajson
        !eo_change type ref to zif_ajson
      raising
        zcx_ajson_error .
    methods sort
      importing
        !iv_json         type string optional
        !io_json         type ref to zif_ajson optional
      returning
        value(rv_sorted) type string
      raising
        zcx_ajson_error .
  protected section.

  private section.

    data mo_json_a type ref to zif_ajson .
    data mo_json_b type ref to zif_ajson .
    data mo_insert type ref to zif_ajson_writer .
    data mo_delete type ref to zif_ajson_writer .
    data mo_change type ref to zif_ajson_writer .

    methods diff_a_b
      importing
        !iv_path type string
      raising
        zcx_ajson_error .
    methods diff_b_a
      importing
        !iv_path type string
      raising
        zcx_ajson_error .
    methods delete_empty_nodes
      importing
        !io_json type ref to zif_ajson
      raising
        zcx_ajson_error .
ENDCLASS.



CLASS ZCL_ABAPGIT_ADT_LINK IMPLEMENTATION.


  METHOD generate.

    DATA: lv_adt_link       TYPE string.
    DATA: lo_adt_uri_mapper TYPE REF TO object ##needed.
    DATA: lo_adt_objref     TYPE REF TO object ##needed.
    DATA: lo_adt_sub_objref TYPE REF TO object ##needed.
    DATA: lv_program        TYPE progname.
    DATA: lv_include        TYPE progname.
    FIELD-SYMBOLS: <lv_uri> TYPE string.

    get_adt_objects_and_names(
          EXPORTING
            iv_obj_name       = iv_obj_name
            iv_obj_type       = iv_obj_type
          IMPORTING
            eo_adt_uri_mapper = lo_adt_uri_mapper
            eo_adt_objectref  = lo_adt_objref
            ev_program        = lv_program
            ev_include        = lv_include ).

    TRY.
        IF iv_sub_obj_name IS NOT INITIAL.

          IF ( lv_program <> iv_obj_name AND lv_include IS INITIAL ) OR
             ( lv_program = lv_include AND iv_sub_obj_name IS NOT INITIAL ).
            lv_include = iv_sub_obj_name.
          ENDIF.

          CALL METHOD lo_adt_uri_mapper->('IF_ADT_URI_MAPPER~MAP_INCLUDE_TO_OBJREF')
            EXPORTING
              program     = lv_program
              include     = lv_include
              line        = iv_line_number
              line_offset = 0
              end_line    = iv_line_number
              end_offset  = 1
            RECEIVING
              result      = lo_adt_sub_objref.
          IF lo_adt_sub_objref IS NOT INITIAL.
            lo_adt_objref = lo_adt_sub_objref.
          ENDIF.

        ENDIF.

        ASSIGN ('LO_ADT_OBJREF->REF_DATA-URI') TO <lv_uri>.
        ASSERT sy-subrc = 0.

        CONCATENATE 'adt://' sy-sysid <lv_uri> INTO lv_adt_link.

        rv_result = lv_adt_link.
      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_adt_objects_and_names.

    DATA lv_obj_type       TYPE trobjtype.
    DATA lv_obj_name       TYPE trobj_name.
    DATA lo_object         TYPE REF TO cl_wb_object.
    DATA lo_adt            TYPE REF TO object.
    FIELD-SYMBOLS <lv_uri> TYPE string.

    lv_obj_name = iv_obj_name.
    lv_obj_type = iv_obj_type.

    TRY.
        cl_wb_object=>create_from_transport_key(
          EXPORTING
            p_object    = lv_obj_type
            p_obj_name  = lv_obj_name
          RECEIVING
            p_wb_object = lo_object
          EXCEPTIONS
            OTHERS      = 1 ).
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
        ENDIF.

        CALL METHOD ('CL_ADT_TOOLS_CORE_FACTORY')=>('GET_INSTANCE')
          RECEIVING
            result = lo_adt.

        IF is_adt_jump_possible( io_object = lo_object
                                 io_adt    = lo_adt ) = abap_false.
          zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
        ENDIF.

        CALL METHOD lo_adt->('IF_ADT_TOOLS_CORE_FACTORY~GET_URI_MAPPER')
          RECEIVING
            result = eo_adt_uri_mapper.

        CALL METHOD eo_adt_uri_mapper->('IF_ADT_URI_MAPPER~MAP_WB_OBJECT_TO_OBJREF')
          EXPORTING
            wb_object = lo_object
          RECEIVING
            result    = eo_adt_objectref.

        ASSIGN ('EO_ADT_OBJECTREF->REF_DATA-URI') TO <lv_uri>.
        ASSERT sy-subrc = 0.

        CALL METHOD eo_adt_uri_mapper->('IF_ADT_URI_MAPPER~MAP_OBJREF_TO_INCLUDE')
          EXPORTING
            uri     = <lv_uri>
          IMPORTING
            program = ev_program
            include = ev_include.

      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.


  METHOD is_adt_jump_possible.

    DATA: lo_wb_request         TYPE REF TO cl_wb_request,
          lo_adt_uri_mapper_vit TYPE REF TO object,
          lv_vit_wb_request     TYPE abap_bool.

    cl_wb_request=>create_from_object_ref(
      EXPORTING
        p_wb_object       = io_object
      RECEIVING
        p_wb_request      = lo_wb_request
      EXCEPTIONS
        illegal_operation = 1
        cancelled         = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDIF.

    TRY.
        CALL METHOD io_adt->('IF_ADT_TOOLS_CORE_FACTORY~GET_URI_MAPPER_VIT')
          RECEIVING
            result = lo_adt_uri_mapper_vit.

        CALL METHOD lo_adt_uri_mapper_vit->('IF_ADT_URI_MAPPER_VIT~IS_VIT_WB_REQUEST')
          EXPORTING
            wb_request = lo_wb_request
          RECEIVING
            result     = lv_vit_wb_request.

        rv_is_adt_jump_possible = boolc( NOT lv_vit_wb_request = abap_true ).

      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_CONVERT IMPLEMENTATION.


  METHOD base64_to_xstring.

    rv_xstr = cl_http_utility=>decode_x_base64( iv_base64 ).

  ENDMETHOD.


  METHOD bitbyte_to_int.

    DATA: lv_bitbyte TYPE string,
          lv_len     TYPE i,
          lv_offset  TYPE i.

    lv_bitbyte = iv_bits.
    SHIFT lv_bitbyte LEFT DELETING LEADING '0 '.
    lv_len     = strlen( lv_bitbyte ).
    lv_offset  = lv_len - 1.

    rv_int = 0.
    DO lv_len TIMES.

      IF sy-index = 1.
        "Intialize
        IF lv_bitbyte+lv_offset(1) = '1'.
          rv_int = 1.
        ENDIF.
      ELSEIF lv_bitbyte+lv_offset(1) = '1'.
        rv_int = rv_int + ( 2 ** ( sy-index - 1 ) ).
      ENDIF.

      lv_offset = lv_offset - 1. "Move Cursor

    ENDDO.

  ENDMETHOD.


  METHOD conversion_exit_isola_output.

    cl_gdt_conversion=>language_code_outbound(
      EXPORTING
        im_value = iv_spras
      IMPORTING
        ex_value = rv_spras ).

    TRANSLATE rv_spras TO UPPER CASE.

  ENDMETHOD.


  METHOD int_to_xstring4.
* returns xstring of length 4 containing the integer value iv_i

    DATA lv_x TYPE x LENGTH 4.

    lv_x = iv_i.
    rv_xstring = lv_x.

  ENDMETHOD.


  METHOD split_string.

    FIND FIRST OCCURRENCE OF cl_abap_char_utilities=>cr_lf IN iv_string.

    " Convert string into table depending on separator type CR_LF vs. LF
    IF sy-subrc = 0.
      SPLIT iv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE rt_lines.
    ELSE.
      SPLIT iv_string AT cl_abap_char_utilities=>newline INTO TABLE rt_lines.
    ENDIF.

  ENDMETHOD.


  METHOD string_to_tab.

    DATA lv_length TYPE i.
    DATA lv_iterations TYPE i.
    DATA lv_offset TYPE i.

    FIELD-SYMBOLS <lg_line> TYPE any.


    CLEAR et_tab.
    ev_size = strlen( iv_str ).

    APPEND INITIAL LINE TO et_tab ASSIGNING <lg_line>.
    <lg_line> = iv_str.
    DESCRIBE FIELD <lg_line> LENGTH lv_length IN CHARACTER MODE.
    lv_iterations = ev_size DIV lv_length.

    DO lv_iterations TIMES.
      lv_offset = sy-index * lv_length.
      APPEND INITIAL LINE TO et_tab ASSIGNING <lg_line>.
      <lg_line> = iv_str+lv_offset.
    ENDDO.

  ENDMETHOD.


  METHOD string_to_xstring.

    rv_xstr = string_to_xstring_utf8( iv_str ).

  ENDMETHOD.


  METHOD string_to_xstring_utf8.

    TRY.
        IF go_convert_out IS INITIAL.
          go_convert_out = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
        ENDIF.

        go_convert_out->convert(
          EXPORTING data = iv_string
          IMPORTING buffer = rv_xstring ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.                  "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.


  METHOD string_to_xstring_utf8_bom.

    DATA: lv_hex     TYPE x LENGTH 1 VALUE '23',
          lv_hex_bom TYPE x LENGTH 3 VALUE 'EFBBBF'.

    rv_xstring = string_to_xstring_utf8( iv_string ).

    "unicode systems always add the byte order mark to the xml, while non-unicode does not
    "in class ZCL_ABAPGIT_XML~TO_XML byte order mark was added to XML as #
    "In non-unicode systems zcl_abapgit_convert=>xstring_to_string_utf8( cl_abap_char_utilities=>byte_order_mark_utf8 )
    "has result # as HEX 23 and not HEX EFBBBF.
    "So we have to remove 23 first and add EFBBBF after to serialized string
    IF rv_xstring(3) <> cl_abap_char_utilities=>byte_order_mark_utf8
    AND rv_xstring(1) = lv_hex.
      REPLACE FIRST OCCURRENCE
        OF lv_hex IN rv_xstring WITH lv_hex_bom IN BYTE MODE.
      ASSERT sy-subrc = 0.
    ENDIF.

  ENDMETHOD.


  METHOD xstring_to_bintab.

    DATA lv_length TYPE i.
    DATA lv_iterations TYPE i.
    DATA lv_offset TYPE i.

    FIELD-SYMBOLS <lg_line> TYPE any.


    CLEAR et_bintab.
    ev_size = xstrlen( iv_xstr ).

    APPEND INITIAL LINE TO et_bintab ASSIGNING <lg_line>.
    <lg_line> = iv_xstr.
    DESCRIBE FIELD <lg_line> LENGTH lv_length IN BYTE MODE.
    lv_iterations = ev_size DIV lv_length.

    DO lv_iterations TIMES.
      lv_offset = sy-index * lv_length.
      APPEND INITIAL LINE TO et_bintab ASSIGNING <lg_line>.
      <lg_line> = iv_xstr+lv_offset.
    ENDDO.

  ENDMETHOD.


  METHOD xstring_to_int.

* use the built-in type conversion
    rv_i = iv_xstring.

  ENDMETHOD.


  METHOD xstring_to_string_utf8.

    TRY.
        IF go_convert_in IS INITIAL.
          go_convert_in = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
        ENDIF.

        go_convert_in->convert(
          EXPORTING
            input = iv_data
            n     = xstrlen( iv_data )
          IMPORTING
            data  = rv_string ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.                  "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.


  METHOD x_to_bitbyte.

    CLEAR rv_bitbyte.

    GET BIT 1 OF iv_x INTO rv_bitbyte+0(1).
    GET BIT 2 OF iv_x INTO rv_bitbyte+1(1).
    GET BIT 3 OF iv_x INTO rv_bitbyte+2(1).
    GET BIT 4 OF iv_x INTO rv_bitbyte+3(1).
    GET BIT 5 OF iv_x INTO rv_bitbyte+4(1).
    GET BIT 6 OF iv_x INTO rv_bitbyte+5(1).
    GET BIT 7 OF iv_x INTO rv_bitbyte+6(1).
    GET BIT 8 OF iv_x INTO rv_bitbyte+7(1).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_default_transport IMPLEMENTATION.


  METHOD clear.

    CALL FUNCTION 'TR_TASK_RESET'
      EXPORTING
        iv_username      = is_default_task-username
        iv_order         = is_default_task-ordernum
        iv_task          = is_default_task-tasknum
        iv_dialog        = abap_false
      EXCEPTIONS
        invalid_username = 1
        invalid_order    = 2
        invalid_task     = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    store( ).

  ENDMETHOD.


  METHOD get.

    DATA: lt_e070use TYPE STANDARD TABLE OF e070use.

    CALL FUNCTION 'TR_TASK_GET'
      TABLES
        tt_e070use       = lt_e070use
      EXCEPTIONS
        invalid_username = 1
        invalid_category = 2
        invalid_client   = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_e070use INTO rs_default_task
                          INDEX 1.

  ENDMETHOD.


  METHOD get_instance.

    IF go_instance IS NOT BOUND.
      CREATE OBJECT go_instance.
    ENDIF.

    ro_instance = go_instance.

  ENDMETHOD.


  METHOD reset.

    DATA: ls_default_task TYPE e070use.

    IF mv_is_set_by_abapgit = abap_false.
      " if the default transport request task isn't set
      " by us there is nothing to do.
      RETURN.
    ENDIF.

    CLEAR mv_is_set_by_abapgit.

    ls_default_task = get( ).

    IF ls_default_task IS NOT INITIAL.

      clear( ls_default_task ).

    ENDIF.

    restore( ).

  ENDMETHOD.


  METHOD restore.

    IF ms_save IS INITIAL.
      " There wasn't a default transport request before
      " so we needn't restore anything.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TR_TASK_SET'
      EXPORTING
        iv_order          = ms_save-ordernum
        iv_task           = ms_save-tasknum
      EXCEPTIONS
        invalid_username  = 1
        invalid_category  = 2
        invalid_client    = 3
        invalid_validdays = 4
        invalid_order     = 5
        invalid_task      = 6
        OTHERS            = 7.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD set.

    " checks whether object changes of the package are rerorded in transport
    " requests. If true then we set the default task, so that no annoying
    " transport request popups are shown while deserializing.

    IF mv_is_set_by_abapgit = abap_true.
      " the default transport request task is already set by us
      " -> no reason to do it again.
      RETURN.
    ENDIF.

    IF iv_transport IS INITIAL.
      zcx_abapgit_exception=>raise( |No transport request was supplied| ).
    ENDIF.

    set_internal( iv_transport ).

    mv_is_set_by_abapgit = abap_true.

  ENDMETHOD.


  METHOD set_internal.

    CALL FUNCTION 'TR_TASK_SET'
      EXPORTING
        iv_order          = iv_transport
        iv_validdays      = 1
      EXCEPTIONS
        invalid_username  = 1
        invalid_category  = 2
        invalid_client    = 3
        invalid_validdays = 4
        invalid_order     = 5
        invalid_task      = 6
        OTHERS            = 7.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD store.

    ms_save = get( ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_dependencies IMPLEMENTATION.


  METHOD get_ddls_dependencies.

    DATA: lt_ddls_name TYPE TABLE OF ddsymtab,
          ls_ddls_name TYPE ddsymtab.

    ls_ddls_name-name = iv_ddls_name.
    INSERT ls_ddls_name INTO TABLE lt_ddls_name.

    PERFORM ('DDLS_GET_DEP') IN PROGRAM ('RADMASDL')
                             TABLES lt_ddls_name rt_dependency.

  ENDMETHOD.


  METHOD resolve.

    DATA: lv_tabclass TYPE dd02l-tabclass.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF ct_tadir.

    " misuse field KORRNUM to fix deletion sequence
    " higher value means later deletion

    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      CASE <ls_tadir>-object.
        WHEN 'DEVC'.
          " Packages last
          <ls_tadir>-korrnum = '999000'.
        WHEN 'DOMA'.
          <ls_tadir>-korrnum = '900000'.
        WHEN 'PARA'.
          " PARA after DTEL
          <ls_tadir>-korrnum = '810000'.
        WHEN 'DTEL'.
          <ls_tadir>-korrnum = '800000'.
        WHEN 'SHLP'.
          " SHLP after TABL
          <ls_tadir>-korrnum = '760000'.
        WHEN 'TTYP' OR 'TABL' OR 'VIEW'.
          SELECT SINGLE tabclass FROM dd02l
            INTO lv_tabclass
            WHERE tabname = <ls_tadir>-obj_name
            AND as4local = 'A'
            AND as4vers = '0000'.
          IF sy-subrc = 0 AND lv_tabclass = 'APPEND'.
            " delete append structures before database tables
            <ls_tadir>-korrnum = '730000'.
          ELSE.
            <ls_tadir>-korrnum = '750000'.
          ENDIF.
        WHEN 'ENQU'.
          " ENQU before TABL
          <ls_tadir>-korrnum = '725000'.
        WHEN 'DDLS'.
          " DDLS after DCLS but before other DDIC
          <ls_tadir>-korrnum = '720000'.
        WHEN 'AUTH'.
          " AUTH after DCLS
          <ls_tadir>-korrnum = '715000'.
        WHEN 'SUSO'.
          " SUSO after DCLS
          <ls_tadir>-korrnum = '710000'.
        WHEN 'DCLS'.
          " AUTH and SUSO after DCLS
          <ls_tadir>-korrnum = '705000'.
        WHEN 'IASP'.
          <ls_tadir>-korrnum = '552000'.
        WHEN 'IARP'.
          <ls_tadir>-korrnum = '551000'.
        WHEN 'IATU'.
          <ls_tadir>-korrnum = '550000'.
        WHEN 'SUSC'.
          <ls_tadir>-korrnum = '500000'.
        WHEN 'ACID'.
          " ACID after PROG/FUGR/CLAS
          <ls_tadir>-korrnum = '300000'.
        WHEN 'FUGR'.
          <ls_tadir>-korrnum = '260000'.
        WHEN 'PROG'.
          " delete includes after main programs
          SELECT COUNT(*) FROM reposrc
            WHERE progname = <ls_tadir>-obj_name
            AND r3state = 'A'
            AND subc = 'I'.
          IF sy-subrc = 0.
            <ls_tadir>-korrnum = '250000'.
          ELSE.
            <ls_tadir>-korrnum = '240000'.
          ENDIF.
        WHEN 'INTF'.
          <ls_tadir>-korrnum = '230000'.
        WHEN 'CLAS'.
          <ls_tadir>-korrnum = '220000'.
        WHEN 'IDOC'.
          <ls_tadir>-korrnum = '200000'.
        WHEN 'WDCA'.
          <ls_tadir>-korrnum = '174000'.
        WHEN 'WDYA'.
          <ls_tadir>-korrnum = '173000'.
        WHEN 'WDCC'.
          <ls_tadir>-korrnum = '172000'.
        WHEN 'WDYN'.
          <ls_tadir>-korrnum = '171000'.
        WHEN 'IEXT'.
          <ls_tadir>-korrnum = '150000'.
        WHEN OTHERS.
          <ls_tadir>-korrnum = '100000'.
      ENDCASE.
    ENDLOOP.

    resolve_ddic( CHANGING ct_tadir = ct_tadir ).
    resolve_packages( CHANGING ct_tadir = ct_tadir ).

    SORT ct_tadir BY korrnum ASCENDING.

  ENDMETHOD.


  METHOD resolve_ddic.
* this will make sure the deletion sequence of structures/tables work
* in case they have dependencies with .INCLUDE

    TYPES: BEGIN OF ty_edge,
             from TYPE ty_item,
             to   TYPE ty_item,
           END OF ty_edge.

    DATA: lt_nodes        TYPE TABLE OF ty_item,
          lt_edges        TYPE TABLE OF ty_edge,
          lt_findstrings  TYPE TABLE OF rsfind,
          lv_plus         TYPE i VALUE 1,
          lv_find_obj_cls TYPE euobj-id,
          lv_index        TYPE i,
          lv_before       TYPE i,
          lt_founds       TYPE TABLE OF rsfindlst,
          lt_scope        TYPE STANDARD TABLE OF seu_obj,
          lt_dependency   TYPE ty_dedenpencies.

    FIELD-SYMBOLS: <ls_tadir_ddls>      TYPE zif_abapgit_definitions=>ty_tadir,
                   <ls_dependency>      TYPE ty_dependency,
                   <ls_tadir_dependent> TYPE zif_abapgit_definitions=>ty_tadir,
                   <ls_tadir>           LIKE LINE OF ct_tadir,
                   <ls_edge>            LIKE LINE OF lt_edges,
                   <ls_found>           LIKE LINE OF lt_founds,
                   <ls_node>            LIKE LINE OF lt_nodes.

    " build nodes
    LOOP AT ct_tadir ASSIGNING <ls_tadir>
        WHERE object = 'TABL'
        OR object = 'VIEW'
        OR object = 'TTYP'.
      APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
      <ls_node>-obj_name = <ls_tadir>-obj_name.
      <ls_node>-obj_type = <ls_tadir>-object.
    ENDLOOP.

    APPEND 'TABL' TO lt_scope.
    APPEND 'VIEW' TO lt_scope.
    APPEND 'STRU' TO lt_scope.
    APPEND 'TTYP' TO lt_scope.

    " build edges
    LOOP AT lt_nodes ASSIGNING <ls_node>.

      CLEAR lt_findstrings.
      APPEND <ls_node>-obj_name TO lt_findstrings.
      lv_find_obj_cls = <ls_node>-obj_type.

      CALL FUNCTION 'RS_EU_CROSSREF'
        EXPORTING
          i_find_obj_cls           = lv_find_obj_cls
        TABLES
          i_findstrings            = lt_findstrings
          o_founds                 = lt_founds
          i_scope_object_cls       = lt_scope
        EXCEPTIONS
          not_executed             = 1
          not_found                = 2
          illegal_object           = 3
          no_cross_for_this_object = 4
          batch                    = 5
          batchjob_error           = 6
          wrong_type               = 7
          object_not_exist         = 8
          OTHERS                   = 9.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      LOOP AT lt_founds ASSIGNING <ls_found>.
        APPEND INITIAL LINE TO lt_edges ASSIGNING <ls_edge>.
        <ls_edge>-from = <ls_node>.

        <ls_edge>-to-obj_name   = <ls_found>-object.
        CASE <ls_found>-object_cls.
          WHEN 'DS'
              OR 'DT'.
            <ls_edge>-to-obj_type = 'TABL'.
          WHEN 'DV'.
            <ls_edge>-to-obj_type = 'VIEW'.
          WHEN 'DA'.
            <ls_edge>-to-obj_type = 'TTYP'.
          WHEN OTHERS.
            zcx_abapgit_exception=>raise( 'resolve_ddic, unknown object_cls' ).
        ENDCASE.
      ENDLOOP.

    ENDLOOP.

    " build DDLS edges
    SORT ct_tadir. "binary search
    LOOP AT ct_tadir ASSIGNING <ls_tadir_ddls>
                     WHERE object = 'DDLS'.

      CLEAR: lt_dependency.

      APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
      <ls_node>-obj_name = <ls_tadir_ddls>-obj_name.
      <ls_node>-obj_type = <ls_tadir_ddls>-object.

      lt_dependency = get_ddls_dependencies( <ls_tadir_ddls>-obj_name ).

      LOOP AT lt_dependency ASSIGNING <ls_dependency>
                            WHERE deptyp = 'DDLS'
                            AND refname = <ls_tadir_ddls>-obj_name.

        READ TABLE ct_tadir ASSIGNING <ls_tadir_dependent>
                            WITH KEY pgmid    = 'R3TR'
                                     object   = 'DDLS'
                                     obj_name = <ls_dependency>-depname
                            BINARY SEARCH.
        CHECK sy-subrc = 0.

        APPEND INITIAL LINE TO lt_edges ASSIGNING <ls_edge>.
        <ls_edge>-from = <ls_node>.
        <ls_edge>-to-obj_name = <ls_dependency>-depname.
        <ls_edge>-to-obj_type = 'DDLS'.

      ENDLOOP.

    ENDLOOP.

    DO.
      lv_before = lines( lt_nodes ).
      LOOP AT lt_nodes ASSIGNING <ls_node>.
        lv_index = sy-tabix.
        READ TABLE lt_edges WITH KEY
          from-obj_name = <ls_node>-obj_name
          from-obj_type = <ls_node>-obj_type
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          LOOP AT ct_tadir ASSIGNING <ls_tadir>
              WHERE obj_name = <ls_node>-obj_name
              AND object = <ls_node>-obj_type.
            <ls_tadir>-korrnum = <ls_tadir>-korrnum + lv_plus.
            CONDENSE <ls_tadir>-korrnum.
          ENDLOOP.
          DELETE lt_edges
            WHERE to-obj_name = <ls_node>-obj_name
            AND to-obj_type = <ls_node>-obj_type.
          DELETE lt_nodes INDEX lv_index.
          EXIT. " make sure the sequence is fixed
        ENDIF.
      ENDLOOP.
      IF lv_before = lines( lt_nodes ).
        EXIT.
      ENDIF.
      lv_plus = lv_plus + 1.
    ENDDO.

  ENDMETHOD.


  METHOD resolve_packages.

    DATA: lt_subpackages TYPE zif_abapgit_sap_package=>ty_devclass_tt.

    FIELD-SYMBOLS: <ls_tadir>            LIKE LINE OF ct_tadir,
                   <lv_subpackage>       LIKE LINE OF lt_subpackages,
                   <ls_tadir_subpackage> LIKE LINE OF ct_tadir.

    " List subpackage before corresponding superpackage

    LOOP AT ct_tadir ASSIGNING <ls_tadir>
                     WHERE object = 'DEVC'.

      lt_subpackages = zcl_abapinst_factory=>get_sap_package( |{ <ls_tadir>-obj_name }| )->list_subpackages( ).

      LOOP AT lt_subpackages ASSIGNING <lv_subpackage>.

        READ TABLE ct_tadir ASSIGNING <ls_tadir_subpackage>
                            WITH KEY object   = 'DEVC'
                                     obj_name = <lv_subpackage>.
        IF sy-subrc = 0.
          <ls_tadir_subpackage>-korrnum = condense( |{ <ls_tadir_subpackage>-korrnum - 1 }| ).
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_dot_abapgit IMPLEMENTATION.


  METHOD add_ignore.

    DATA: lv_name TYPE string.

    FIELD-SYMBOLS: <lv_ignore> LIKE LINE OF ms_data-ignore.


    lv_name = iv_path && iv_filename.

    READ TABLE ms_data-ignore FROM lv_name TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO ms_data-ignore ASSIGNING <lv_ignore>.
    <lv_ignore> = lv_name.

  ENDMETHOD.


  METHOD build_default.

    DATA: ls_data TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit.


    ls_data-master_language = sy-langu.
    ls_data-starting_folder = '/src/'.
    ls_data-folder_logic    = zif_abapgit_dot_abapgit=>c_folder_logic-prefix.

    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.


  METHOD constructor.
    ms_data = is_data.
  ENDMETHOD.


  METHOD deserialize.

    DATA: lv_xml  TYPE string,
          ls_data TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit.


    lv_xml = zcl_abapgit_convert=>xstring_to_string_utf8( iv_xstr ).

    ls_data = from_xml( lv_xml ).

    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.


  METHOD from_xml.

    DATA: lv_xml TYPE string.

    lv_xml = iv_xml.

    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML lv_xml
      RESULT data = rs_data.

* downward compatibility
    IF rs_data-folder_logic IS INITIAL.
      rs_data-folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-prefix.
    ENDIF.

  ENDMETHOD.


  METHOD get_data.
    rs_data = ms_data.
  ENDMETHOD.


  METHOD get_folder_logic.
    rv_logic = ms_data-folder_logic.
  ENDMETHOD.


  METHOD get_i18n_languages.
    rt_languages = ms_data-i18n_languages.
  ENDMETHOD.


  METHOD get_main_language.
    rv_language = ms_data-master_language.
  ENDMETHOD.


  METHOD get_master_language.
    " todo, transition to get_main_language()
    rv_language = ms_data-master_language.
  ENDMETHOD.


  METHOD get_packaging.
    rt_packaging = ms_data-packaging.
  ENDMETHOD.


  METHOD get_requirements.
    rt_requirements = ms_data-requirements.
  ENDMETHOD.


  METHOD get_signature.

    rs_signature-path     = zif_abapgit_definitions=>c_root_dir.
    rs_signature-filename = zif_abapgit_definitions=>c_dot_abapgit.
    rs_signature-sha1     = zcl_abapgit_hash=>sha1_blob( serialize( ) ).

  ENDMETHOD.


  METHOD get_starting_folder.
    rv_path = ms_data-starting_folder.
  ENDMETHOD.


  METHOD is_ignored.

    DATA: lv_name     TYPE string,
          lv_starting TYPE string,
          lv_dot      TYPE string,
          lv_ignore   TYPE string.


    lv_name = iv_path && iv_filename.

    CONCATENATE ms_data-starting_folder '*' INTO lv_starting.

    " Always allow .abapgit.xml and .apack-manifest.xml
    CONCATENATE '/' zif_abapgit_definitions=>c_dot_abapgit INTO lv_dot.
    IF lv_name = lv_dot.
      RETURN.
    ENDIF.
    CONCATENATE '/' '.apack-manifest.xml' INTO lv_dot.
    IF lv_name = lv_dot.
      RETURN.
    ENDIF.

    " Ignore all files matching pattern in ignore list
    LOOP AT ms_data-ignore INTO lv_ignore.
      IF lv_name CP lv_ignore.
        rv_ignored = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Ignore all files outside of starting folder tree
    IF ms_data-starting_folder <> '/' AND NOT lv_name CP lv_starting.
      rv_ignored = abap_true.
    ENDIF.

    IF iv_path = '/data/'.
      rv_ignored = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD remove_ignore.

    DATA: lv_name TYPE string.


    lv_name = iv_path && iv_filename.

    DELETE TABLE ms_data-ignore FROM lv_name.

  ENDMETHOD.


  METHOD serialize.

    DATA: lv_xml  TYPE string,
          lv_mark TYPE string.

    lv_xml = to_xml( ms_data ).

    "unicode systems always add the byte order mark to the xml, while non-unicode does not
    "this code will always add the byte order mark if it is not in the xml
    lv_mark = zcl_abapgit_convert=>xstring_to_string_utf8( cl_abap_char_utilities=>byte_order_mark_utf8 ).
    IF lv_xml(1) <> lv_mark.
      CONCATENATE lv_mark lv_xml INTO lv_xml.
    ENDIF.

    rv_xstr = zcl_abapgit_convert=>string_to_xstring_utf8_bom( lv_xml ).

  ENDMETHOD.


  METHOD set_folder_logic.
    ms_data-folder_logic = iv_logic.
  ENDMETHOD.


  METHOD set_i18n_languages.
    ms_data-i18n_languages = it_languages.
  ENDMETHOD.


  METHOD set_packaging.
    ms_data-packaging = it_packaging.
  ENDMETHOD.


  METHOD set_requirements.
    ms_data-requirements = it_requirements.
  ENDMETHOD.


  METHOD set_starting_folder.
    ms_data-starting_folder = iv_path.
  ENDMETHOD.


  METHOD to_file.
    rs_file-path     = zif_abapgit_definitions=>c_root_dir.
    rs_file-filename = zif_abapgit_definitions=>c_dot_abapgit.
    rs_file-data     = serialize( ).
    rs_file-sha1     = zcl_abapgit_hash=>sha1_blob( rs_file-data ).
  ENDMETHOD.


  METHOD to_xml.

    CALL TRANSFORMATION id
      OPTIONS initial_components = 'suppress'
      SOURCE data = is_data
      RESULT XML rv_xml.

    rv_xml = zcl_abapgit_xml_pretty=>print( rv_xml ).

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN rv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_environment IMPLEMENTATION.


  METHOD zif_abapgit_environment~compare_with_inactive.
    rv_result = zif_abapgit_environment~is_sap_cloud_platform( ).
  ENDMETHOD.


  METHOD zif_abapgit_environment~is_merged.
    DATA lr_marker TYPE REF TO data ##NEEDED.

    IF mv_is_merged = abap_undefined.
      TRY.
          CREATE DATA lr_marker TYPE REF TO ('LIF_ABAPMERGE_MARKER').
          "No exception --> marker found
          mv_is_merged = abap_true.

        CATCH cx_sy_create_data_error.
          mv_is_merged = abap_false.
      ENDTRY.
    ENDIF.
    rv_result = mv_is_merged.
  ENDMETHOD.


  METHOD zif_abapgit_environment~is_repo_object_changes_allowed.
    DATA lv_ind TYPE t000-ccnocliind.

    IF mv_client_modifiable = abap_undefined.
      SELECT SINGLE ccnocliind FROM t000 INTO lv_ind
             WHERE mandt = sy-mandt.
      IF sy-subrc = 0
          AND ( lv_ind = ' ' OR lv_ind = '1' ). "check changes allowed
        mv_client_modifiable = abap_true.
      ELSE.
        mv_client_modifiable = abap_false.
      ENDIF.
    ENDIF.
    rv_result = mv_client_modifiable.
  ENDMETHOD.


  METHOD zif_abapgit_environment~is_restart_required.
    " This method will be used in the context of SAP Cloud Platform:
    " Pull/Push operations are executed in background jobs.
    " In case of the respective application server needs to be restarted,
    " it is required to terminae the background job and reschedule again.
    rv_result = abap_false.
    TRY.
        CALL METHOD ('CL_APJ_SCP_TOOLS')=>('IS_RESTART_REQUIRED')
          RECEIVING
            restart_required = rv_result.
      CATCH cx_sy_dyn_call_illegal_method cx_sy_dyn_call_illegal_class.
        rv_result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_environment~is_sap_cloud_platform.
    IF mv_cloud = abap_undefined.
      TRY.
          CALL METHOD ('CL_COS_UTILITIES')=>('IS_SAP_CLOUD_PLATFORM')
            RECEIVING
              rv_is_sap_cloud_platform = mv_cloud.
        CATCH cx_sy_dyn_call_illegal_method.
          mv_cloud = abap_false.
      ENDTRY.
    ENDIF.
    rv_result = mv_cloud.
  ENDMETHOD.


  METHOD zif_abapgit_environment~is_sap_object_allowed.

    rv_allowed = cl_enh_badi_def_utility=>is_sap_system( ).
    IF rv_allowed = abap_true.
      RETURN.
    ENDIF.

    rv_allowed = zcl_abapgit_exit=>get_instance( )->allow_sap_objects( ).

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_EXIT IMPLEMENTATION.


  METHOD get_instance.

    IF gi_exit IS INITIAL.
      TRY.
        CATCH cx_sy_create_object_error ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    CREATE OBJECT ri_exit TYPE zcl_abapgit_exit.

  ENDMETHOD.


  METHOD zif_abapgit_exit~adjust_display_commit_url.

    TRY.
        gi_exit->adjust_display_commit_url(
          EXPORTING
            iv_repo_url           = iv_repo_url
            iv_repo_name          = iv_repo_name
            iv_repo_key           = iv_repo_key
            iv_commit_hash        = iv_commit_hash
          CHANGING
            cv_display_url        = cv_display_url ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~allow_sap_objects.

    TRY.
        rv_allowed = gi_exit->allow_sap_objects( ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_local_host.

    TRY.
        gi_exit->change_local_host( CHANGING ct_hosts = ct_hosts ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_authentication.

    TRY.
        gi_exit->change_proxy_authentication(
          EXPORTING
            iv_repo_url             = iv_repo_url
          CHANGING
            cv_proxy_authentication = cv_proxy_authentication ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_port.

    TRY.
        gi_exit->change_proxy_port(
          EXPORTING
            iv_repo_url   = iv_repo_url
          CHANGING
            cv_proxy_port = cv_proxy_port ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_url.

    TRY.
        gi_exit->change_proxy_url(
          EXPORTING
            iv_repo_url  = iv_repo_url
          CHANGING
            cv_proxy_url = cv_proxy_url ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_tadir.

    TRY.
        gi_exit->change_tadir(
          EXPORTING
            iv_package = iv_package
            ii_log     = ii_log
          CHANGING
            ct_tadir   = ct_tadir ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~create_http_client.

    TRY.
        ri_client = gi_exit->create_http_client( iv_url ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~custom_serialize_abap_clif.
    TRY.
        rt_source = gi_exit->custom_serialize_abap_clif( is_class_key ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_exit~deserialize_postprocess.

    TRY.
        gi_exit->deserialize_postprocess( is_step = is_step
                                          ii_log  = ii_log ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~get_ci_tests.

    TRY.
        gi_exit->get_ci_tests(
          EXPORTING
            iv_object   = iv_object
          CHANGING
            ct_ci_repos = ct_ci_repos ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~get_ssl_id.

    TRY.
        rv_ssl_id = gi_exit->get_ssl_id( ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

    IF rv_ssl_id IS INITIAL.
      rv_ssl_id = 'ANONYM'.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~http_client.

    TRY.
        gi_exit->http_client(
          iv_url    = iv_url
          ii_client = ii_client ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~pre_calculate_repo_status.

    TRY.
        gi_exit->pre_calculate_repo_status(
          EXPORTING
            is_repo_meta = is_repo_meta
          CHANGING
            ct_local  = ct_local
            ct_remote = ct_remote ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_folder_logic IMPLEMENTATION.


  METHOD get_instance.
    CREATE OBJECT ro_instance.
  ENDMETHOD.


  METHOD get_parent.
    DATA: ls_parent LIKE LINE OF mt_parent.

    "Determine Parent Package
    READ TABLE mt_parent INTO ls_parent
      WITH TABLE KEY devclass = iv_package.
    IF sy-subrc <> 0.
      rv_parent = zcl_abapinst_factory=>get_sap_package( iv_package )->read_parent( ).
      ls_parent-devclass = iv_package.
      ls_parent-parentcl = rv_parent.
      INSERT ls_parent INTO TABLE mt_parent.
    ELSE.
      rv_parent = ls_parent-parentcl.
    ENDIF.
  ENDMETHOD.


  METHOD package_to_path.

    DATA: lv_len          TYPE i,
          lv_path         TYPE string,
          lv_message      TYPE string,
          lv_parentcl     TYPE tdevc-parentcl,
          lv_folder_logic TYPE string.

    IF iv_top = iv_package.
      rv_path = io_dot->get_starting_folder( ).
    ELSE.
      lv_parentcl = get_parent( iv_package ).

      IF lv_parentcl IS INITIAL.
        zcx_abapgit_exception=>raise( |error, expected parent package, { iv_package }| ).
      ELSE.
        lv_folder_logic = io_dot->get_folder_logic( ).
        CASE lv_folder_logic.
          WHEN zif_abapgit_dot_abapgit=>c_folder_logic-full.
            lv_len = 0.
            IF iv_package(1) = '$'.
              lv_len = 1.
            ENDIF.
          WHEN zif_abapgit_dot_abapgit=>c_folder_logic-prefix.
            lv_len = strlen( lv_parentcl ).

            IF iv_package(lv_len) <> lv_parentcl.
* if abapGit project is installed in package ZZZ, all subpackages should be named
* ZZZ_something. This will define the folder name in the zip file to be "something",
* similarily with online projects. Alternatively change to FULL folder logic
              lv_message = 'PREFIX: Unexpected package naming (' && iv_package && ')'
                           && 'you might switch to FULL folder logic'.
              zcx_abapgit_exception=>raise( lv_message ).
            ENDIF.
          WHEN OTHERS.
            zcx_abapgit_exception=>raise( |Invalid folder logic: { lv_folder_logic }| ).
        ENDCASE.

        lv_path = iv_package+lv_len.
        IF strlen( lv_path ) = 0.
          zcx_abapgit_exception=>raise( |Folder logic: length = 0, parent: {
            lv_parentcl }, child: { iv_package }| ).
        ENDIF.

        IF lv_path(1) = '_'.
          lv_path = lv_path+1.
        ENDIF.
        IF strlen( lv_path ) = 0.
          zcx_abapgit_exception=>raise( |Folder logic: length = 0, parent: {
            lv_parentcl }, child: { iv_package }| ).
        ENDIF.

        TRANSLATE lv_path USING '/#'.
        TRANSLATE lv_path TO LOWER CASE.
        CONCATENATE lv_path '/' INTO lv_path.

        rv_path = package_to_path( iv_top     = iv_top
                                   io_dot     = io_dot
                                   iv_package = lv_parentcl ).

        CONCATENATE rv_path lv_path INTO rv_path.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD path_to_package.

    DATA: lv_length               TYPE i,
          lv_parent               TYPE devclass,
          ls_package              TYPE scompkdtln,
          lv_new                  TYPE string,
          lv_path                 TYPE string,
          lv_absolute_name        TYPE string,
          lt_unique_package_names TYPE HASHED TABLE OF devclass WITH UNIQUE KEY table_line.

    lv_length  = strlen( io_dot->get_starting_folder( ) ).
    IF lv_length > strlen( iv_path ).
* treat as not existing locally
      RETURN.
    ENDIF.
    lv_path    = iv_path+lv_length.
    lv_parent  = iv_top.
    rv_package = iv_top.

    " Automatically create package using minimal properties
    " Details will be updated during deserialization
    IF iv_create_if_not_exists = abap_true.
      IF iv_top(1) = '$'.
        zcl_abapinst_factory=>get_sap_package( iv_top )->create_local( ).
      ELSE.
        ls_package-devclass = ls_package-ctext = iv_top.
        ls_package-as4user = cl_abap_syst=>get_user_name( ).
        zcl_abapinst_factory=>get_sap_package( iv_top )->create( ls_package ).
      ENDIF.
    ENDIF.

    INSERT iv_top INTO TABLE lt_unique_package_names.

    WHILE lv_path CA '/'.
      SPLIT lv_path AT '/' INTO lv_new lv_path.

      CASE io_dot->get_folder_logic( ).
        WHEN zif_abapgit_dot_abapgit=>c_folder_logic-full.
          lv_absolute_name = lv_new.
          TRANSLATE lv_absolute_name USING '#/'.
          IF iv_top(1) = '$'.
            CONCATENATE '$' lv_absolute_name INTO lv_absolute_name.
          ENDIF.
        WHEN zif_abapgit_dot_abapgit=>c_folder_logic-prefix.
          CONCATENATE rv_package '_' lv_new INTO lv_absolute_name.
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.

      TRANSLATE lv_absolute_name TO UPPER CASE.

      IF strlen( lv_absolute_name ) > 30.
        zcx_abapgit_exception=>raise( |Package { lv_absolute_name } exceeds ABAP 30-characters-name limit| ).
      ENDIF.

      rv_package = lv_absolute_name.
      READ TABLE lt_unique_package_names TRANSPORTING NO FIELDS
        WITH TABLE KEY table_line = rv_package.
      IF sy-subrc = 0.
        zcx_abapgit_exception=>raise( |Package { rv_package } has a subpackage with the same name| ).
      ELSE.
        INSERT rv_package INTO TABLE lt_unique_package_names.
      ENDIF.

      IF zcl_abapinst_factory=>get_sap_package( rv_package )->exists( ) = abap_false AND
          iv_create_if_not_exists = abap_true.

        zcl_abapinst_factory=>get_sap_package( lv_parent )->create_child( rv_package ).
      ENDIF.

      lv_parent = rv_package.
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_FREE_SEL_DIALOG IMPLEMENTATION.


  METHOD constructor.
    mv_title = iv_title.
    mv_frame_text = iv_frame_text.
  ENDMETHOD.


  METHOD convert_input_fields.
    CONSTANTS: lc_only_eq_optlist_name TYPE c LENGTH 10 VALUE 'ONLYEQ'.
    DATA: ls_parameter_opt_list TYPE sscr_opt_list.
    FIELD-SYMBOLS: <ls_input_field>            TYPE ty_free_sel_field,
                   <lt_input_fields>           TYPE ty_free_sel_field_tab,
                   <ls_free_sel_field>         TYPE rsdsfields,
                   <ls_restriction_ass>        TYPE sscr_ass_ds,
                   <ls_text>                   TYPE rsdstexts,
                   <ls_default_value>          TYPE rsds_range,
                   <ls_default_value_range>    TYPE rsds_frange,
                   <ls_default_val_range_line> TYPE rsdsselopt.

    ASSERT mr_fields IS BOUND.
    ASSIGN mr_fields->* TO <lt_input_fields>.

    LOOP AT <lt_input_fields> ASSIGNING <ls_input_field>.
      APPEND INITIAL LINE TO et_fields ASSIGNING <ls_free_sel_field>.
      <ls_free_sel_field>-fieldname = <ls_input_field>-ddic_fieldname.
      <ls_free_sel_field>-tablename = <ls_input_field>-ddic_tabname.

      IF <ls_input_field>-only_parameter = abap_true.
        IF es_restriction IS INITIAL.
          ls_parameter_opt_list-name = lc_only_eq_optlist_name.
          ls_parameter_opt_list-options-eq = abap_true.
          APPEND ls_parameter_opt_list TO es_restriction-opt_list_tab.
        ENDIF.

        APPEND INITIAL LINE TO es_restriction-ass_tab ASSIGNING <ls_restriction_ass>.
        <ls_restriction_ass>-kind = 'S'.
        <ls_restriction_ass>-fieldname = <ls_input_field>-ddic_fieldname.
        <ls_restriction_ass>-tablename = <ls_input_field>-ddic_tabname.
        <ls_restriction_ass>-sg_main = 'I'.
        <ls_restriction_ass>-sg_addy = 'N'.
        <ls_restriction_ass>-op_main = lc_only_eq_optlist_name.
      ENDIF.

      IF <ls_input_field>-text IS NOT INITIAL.
        APPEND INITIAL LINE TO et_field_texts ASSIGNING <ls_text>.
        <ls_text>-fieldname = <ls_input_field>-ddic_fieldname.
        <ls_text>-tablename = <ls_input_field>-ddic_tabname.
        <ls_text>-text = <ls_input_field>-text.
      ENDIF.

      IF <ls_input_field>-value IS NOT INITIAL OR <ls_input_field>-value_range IS NOT INITIAL.
        READ TABLE et_default_values WITH KEY tablename = <ls_input_field>-ddic_tabname
                                     ASSIGNING <ls_default_value>.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO et_default_values ASSIGNING <ls_default_value>.
          <ls_default_value>-tablename = <ls_input_field>-ddic_tabname.
        ENDIF.

        APPEND INITIAL LINE TO <ls_default_value>-frange_t ASSIGNING <ls_default_value_range>.
        <ls_default_value_range>-fieldname = <ls_input_field>-ddic_fieldname.

        IF <ls_input_field>-value IS NOT INITIAL.
          APPEND INITIAL LINE TO <ls_default_value_range>-selopt_t ASSIGNING <ls_default_val_range_line>.
          <ls_default_val_range_line>-sign = 'I'.
          <ls_default_val_range_line>-option = 'EQ'.
          <ls_default_val_range_line>-low = <ls_input_field>-value.
        ELSEIF <ls_input_field>-value_range IS NOT INITIAL.
          <ls_default_value_range>-selopt_t = <ls_input_field>-value_range.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD free_selections_dialog.
    CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
      EXPORTING
        selection_id    = iv_selection_id
        title           = mv_title
        frame_text      = mv_frame_text
        status          = 1
        as_window       = abap_true
        no_intervals    = abap_true
        tree_visible    = abap_false
      IMPORTING
        field_ranges    = et_result_ranges
      TABLES
        fields_tab      = ct_fields
      EXCEPTIONS
        internal_error  = 1
        no_action       = 2
        selid_not_found = 3
        illegal_status  = 4
        OTHERS          = 5.
    CASE sy-subrc.
      WHEN 0 ##NEEDED.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_abapgit_cancel.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( |Error from FREE_SELECTIONS_DIALOG: { sy-subrc }| ).
    ENDCASE.
  ENDMETHOD.


  METHOD free_selections_init.
    CALL FUNCTION 'FREE_SELECTIONS_INIT'
      EXPORTING
        kind                     = 'F'
        field_ranges_int         = it_default_values
        restriction              = is_restriction
      IMPORTING
        selection_id             = ev_selection_id
      TABLES
        fields_tab               = ct_fields
        field_texts              = ct_field_texts
      EXCEPTIONS
        fields_incomplete        = 1
        fields_no_join           = 2
        field_not_found          = 3
        no_tables                = 4
        table_not_found          = 5
        expression_not_supported = 6
        incorrect_expression     = 7
        illegal_kind             = 8
        area_not_found           = 9
        inconsistent_area        = 10
        kind_f_no_fields_left    = 11
        kind_f_no_fields         = 12
        too_many_fields          = 13
        dup_field                = 14
        field_no_type            = 15
        field_ill_type           = 16
        dup_event_field          = 17
        node_not_in_ldb          = 18
        area_no_field            = 19
        OTHERS                   = 20.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from FREE_SELECTIONS_INIT: { sy-subrc }| ).
    ENDIF.
  ENDMETHOD.


  METHOD set_fields.
    GET REFERENCE OF ct_fields INTO mr_fields.
  ENDMETHOD.


  METHOD show.
    DATA: lt_default_values   TYPE rsds_trange,
          ls_restriction      TYPE sscr_restrict_ds,
          lt_fields           TYPE rsdsfields_t,
          lt_field_texts      TYPE ty_field_text_tab,
          lv_repeat_dialog    TYPE abap_bool VALUE abap_true,
          lv_selection_id     TYPE dynselid,
          lt_results          TYPE rsds_trange,
          lx_validation_error TYPE REF TO zcx_abapgit_exception.

    convert_input_fields(
      IMPORTING
        et_default_values = lt_default_values
        es_restriction    = ls_restriction
        et_fields         = lt_fields
        et_field_texts    = lt_field_texts ).

    WHILE lv_repeat_dialog = abap_true.
      lv_repeat_dialog = abap_false.

      free_selections_init(
        EXPORTING
          it_default_values = lt_default_values
          is_restriction    = ls_restriction
        IMPORTING
          ev_selection_id   = lv_selection_id
        CHANGING
          ct_fields         = lt_fields
          ct_field_texts    = lt_field_texts ).

      free_selections_dialog(
        EXPORTING
          iv_selection_id  = lv_selection_id
        IMPORTING
          et_result_ranges = lt_results
        CHANGING
          ct_fields        = lt_fields ).

      TRY.
          validate_results( lt_results ).
        CATCH zcx_abapgit_exception INTO lx_validation_error.
          lv_repeat_dialog = abap_true.
          lt_default_values = lt_results.
          MESSAGE lx_validation_error TYPE 'I' DISPLAY LIKE 'E'.
          CONTINUE.
      ENDTRY.

      transfer_results_to_input( lt_results ).
    ENDWHILE.
  ENDMETHOD.


  METHOD transfer_results_to_input.
    FIELD-SYMBOLS: <ls_input_field>          TYPE ty_free_sel_field,
                   <lt_input_fields>         TYPE ty_free_sel_field_tab,
                   <ls_result_range_for_tab> TYPE rsds_range,
                   <ls_result_range_line>    TYPE rsds_frange,
                   <ls_selopt_line>          TYPE rsdsselopt.

    ASSIGN mr_fields->* TO <lt_input_fields>.
    ASSERT sy-subrc = 0.

    LOOP AT <lt_input_fields> ASSIGNING <ls_input_field>.
      READ TABLE it_result_ranges WITH KEY tablename = <ls_input_field>-ddic_tabname
                                  ASSIGNING <ls_result_range_for_tab>.
      IF sy-subrc = 0.
        READ TABLE <ls_result_range_for_tab>-frange_t WITH KEY fieldname = <ls_input_field>-ddic_fieldname
                                                      ASSIGNING <ls_result_range_line>.
        IF sy-subrc = 0 AND <ls_result_range_line>-selopt_t IS NOT INITIAL.
          IF <ls_input_field>-only_parameter = abap_true.
            ASSERT lines( <ls_result_range_line>-selopt_t ) = 1.

            READ TABLE <ls_result_range_line>-selopt_t INDEX 1 ASSIGNING <ls_selopt_line>.
            ASSERT sy-subrc = 0.

            ASSERT <ls_selopt_line>-sign = 'I' AND
                   <ls_selopt_line>-option = 'EQ' AND
                   <ls_selopt_line>-high IS INITIAL.

            <ls_input_field>-value = <ls_selopt_line>-low.
          ELSE.
            <ls_input_field>-value_range = <ls_result_range_line>-selopt_t.
          ENDIF.
        ELSE.
          CLEAR: <ls_input_field>-value, <ls_input_field>-value_range.
        ENDIF.
      ELSE.
        CLEAR: <ls_input_field>-value, <ls_input_field>-value_range.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD validate_results.
    DATA: ls_error_msg      TYPE symsg,
          lv_ddut_fieldname TYPE fnam_____4,
          lv_value          TYPE rsdsselop_.
    FIELD-SYMBOLS: <ls_result_range_for_tab> TYPE rsds_range,
                   <ls_result_range_line>    TYPE rsds_frange,
                   <ls_input_field>          TYPE ty_free_sel_field,
                   <lt_input_fields>         TYPE ty_free_sel_field_tab,
                   <ls_selopt_line>          TYPE rsdsselopt.

    ASSIGN mr_fields->* TO <lt_input_fields>.
    ASSERT sy-subrc = 0.

    LOOP AT it_result_ranges ASSIGNING <ls_result_range_for_tab>.
      LOOP AT <ls_result_range_for_tab>-frange_t ASSIGNING <ls_result_range_line>.
        READ TABLE <lt_input_fields> WITH KEY ddic_tabname = <ls_result_range_for_tab>-tablename
                                              ddic_fieldname = <ls_result_range_line>-fieldname
                                     ASSIGNING <ls_input_field>.
        ASSERT sy-subrc = 0.
        IF <ls_input_field>-only_parameter = abap_false.
          CONTINUE.
        ENDIF.

        CASE lines( <ls_result_range_line>-selopt_t ).
          WHEN 0.
            CLEAR lv_value.
          WHEN 1.
            READ TABLE <ls_result_range_line>-selopt_t INDEX 1 ASSIGNING <ls_selopt_line>.
            ASSERT sy-subrc = 0.
            lv_value = <ls_selopt_line>-low.
          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.

        CLEAR ls_error_msg.
        lv_ddut_fieldname = <ls_input_field>-ddic_fieldname.

        CALL FUNCTION 'DDUT_INPUT_CHECK'
          EXPORTING
            tabname            = <ls_input_field>-ddic_tabname
            fieldname          = lv_ddut_fieldname
            value              = lv_value
            accept_all_initial = abap_true
            value_list         = 'S'
          IMPORTING
            msgid              = ls_error_msg-msgid
            msgty              = ls_error_msg-msgty
            msgno              = ls_error_msg-msgno
            msgv1              = ls_error_msg-msgv1
            msgv2              = ls_error_msg-msgv2
            msgv3              = ls_error_msg-msgv3
            msgv4              = ls_error_msg-msgv4.
        IF ls_error_msg IS NOT INITIAL.
          zcx_abapgit_exception=>raise_t100(
            iv_msgid = ls_error_msg-msgid
            iv_msgno = ls_error_msg-msgno
            iv_msgv1 = ls_error_msg-msgv1
            iv_msgv2 = ls_error_msg-msgv2
            iv_msgv3 = ls_error_msg-msgv3
            iv_msgv4 = ls_error_msg-msgv4 ).
        ELSEIF <ls_input_field>-param_obligatory = abap_true AND lv_value IS INITIAL.
          zcx_abapgit_exception=>raise( |Field '{ <ls_input_field>-name }' is obligatory| ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_FUNCTIONS IMPLEMENTATION.


  METHOD zif_abapgit_gui_functions~gui_is_available.

    CALL FUNCTION 'GUI_IS_AVAILABLE'
      IMPORTING
        return = rv_gui_is_available.

  ENDMETHOD.


  METHOD zif_abapgit_gui_functions~is_sapgui_for_java.

    CALL FUNCTION 'GUI_HAS_JAVABEANS'
      IMPORTING
        return = rv_result.

  ENDMETHOD.

  METHOD zif_abapgit_gui_functions~is_sapgui_for_windows.
    DATA: lv_platform TYPE i.

    cl_gui_frontend_services=>get_platform(
      RECEIVING
        platform             = lv_platform
      EXCEPTIONS
        error_no_gui         = 1
        cntl_error           = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      rv_result = abap_false.
    ENDIF.

    CASE lv_platform.
      WHEN cl_gui_frontend_services=>platform_nt351 OR
           cl_gui_frontend_services=>platform_nt40 OR
           cl_gui_frontend_services=>platform_nt50 OR
           cl_gui_frontend_services=>platform_windows95 OR
           cl_gui_frontend_services=>platform_windows98 OR
           cl_gui_frontend_services=>platform_windowsxp.
        " Everything after Windows XP is reported as Windows XP
        rv_result = abap_true.
      WHEN OTHERS.
        rv_result = abap_false.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_HASH IMPLEMENTATION.


  METHOD adler32.

    CONSTANTS: lc_adler TYPE i VALUE 65521,
               lc_max_b TYPE i VALUE 1800000000.

    DATA: lv_index TYPE i,
          lv_a     TYPE i VALUE 1,
          lv_b     TYPE i VALUE 0,
          lv_x     TYPE x LENGTH 2,
          lv_ca    TYPE c LENGTH 4,
          lv_cb    TYPE c LENGTH 4,
          lv_char8 TYPE c LENGTH 8.


    DO xstrlen( iv_xstring ) TIMES.
      lv_index = sy-index - 1.

      lv_a = lv_a + iv_xstring+lv_index(1).
      lv_b = lv_b + lv_a.

* delay the MOD operation until the integer might overflow
* articles describe 5552 additions are allowed, but this assumes unsigned integers
* instead of allowing a fixed number of additions before running MOD, then
* just compare value of lv_b, this is 1 operation less than comparing and adding
      IF lv_b > lc_max_b.
        lv_a = lv_a MOD lc_adler.
        lv_b = lv_b MOD lc_adler.
      ENDIF.
    ENDDO.

    lv_a = lv_a MOD lc_adler.
    lv_b = lv_b MOD lc_adler.

    lv_x = lv_a.
    lv_ca = lv_x.

    lv_x = lv_b.
    lv_cb = lv_x.

    CONCATENATE lv_cb lv_ca INTO lv_char8.

    rv_checksum = lv_char8.

  ENDMETHOD.


  METHOD sha1.

    DATA: lv_len     TYPE i,
          lv_char10  TYPE c LENGTH 10,
          lv_string  TYPE string,
          lv_xstring TYPE xstring.


    lv_len = xstrlen( iv_data ).
    lv_char10 = lv_len.
    CONDENSE lv_char10.
    CONCATENATE iv_type lv_char10 INTO lv_string SEPARATED BY space.
    lv_xstring = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

    lv_string = lv_xstring.
    CONCATENATE lv_string '00' INTO lv_string.
    lv_xstring = lv_string.

    CONCATENATE lv_xstring iv_data INTO lv_xstring IN BYTE MODE.

    rv_sha1 = sha1_raw( lv_xstring ).

  ENDMETHOD.


  METHOD sha1_blob.
    rv_sha1 = sha1( iv_type = zif_abapgit_definitions=>c_type-blob
                    iv_data = iv_data ).
  ENDMETHOD.


  METHOD sha1_commit.
    rv_sha1 = sha1( iv_type = zif_abapgit_definitions=>c_type-commit
                    iv_data = iv_data ).
  ENDMETHOD.


  METHOD sha1_raw.

    DATA: lv_hash  TYPE string,
          lv_key   TYPE xstring,
          lx_error TYPE REF TO cx_abap_message_digest.
    TRY.
        cl_abap_hmac=>calculate_hmac_for_raw(
      EXPORTING
        if_key        = lv_key
        if_data       = iv_data
      IMPORTING
        ef_hmacstring = lv_hash ).
      CATCH cx_abap_message_digest INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    rv_sha1 = lv_hash.
    TRANSLATE rv_sha1 TO LOWER CASE.

  ENDMETHOD.


  METHOD sha1_tag.
    rv_sha1 = sha1( iv_type = zif_abapgit_definitions=>c_type-tag
                    iv_data = iv_data ).
  ENDMETHOD.


  METHOD sha1_tree.
    rv_sha1 = sha1( iv_type = zif_abapgit_definitions=>c_type-tree
                    iv_data = iv_data ).
  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_language IMPLEMENTATION.


  METHOD class_constructor.

    DATA lv_dummy TYPE string.

    GET LOCALE LANGUAGE gv_login_language COUNTRY lv_dummy MODIFIER lv_dummy.

  ENDMETHOD.


  METHOD restore_login_language.

    SET LOCALE LANGUAGE gv_login_language.

  ENDMETHOD.


  METHOD set_current_language.

    SET LOCALE LANGUAGE iv_language.

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_LOG IMPLEMENTATION.


  METHOD get_messages_status.

    DATA lr_msg TYPE REF TO zif_abapgit_log=>ty_msg.
    rv_status = 'S'.
    LOOP AT it_msg REFERENCE INTO lr_msg.
      CASE lr_msg->type.
        WHEN 'E' OR 'A' OR 'X'.
          rv_status = 'E'. "not okay
          EXIT.
        WHEN 'W'.
          rv_status = 'W'. "maybe
          CONTINUE.
        WHEN 'S' OR 'I'.
          IF rv_status <> 'W'.
            rv_status = 'S'. "okay
          ENDIF.
          CONTINUE.
        WHEN OTHERS. "unknown
          CONTINUE.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_log~add.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF mt_log.

    APPEND INITIAL LINE TO mt_log ASSIGNING <ls_log>.
    <ls_log>-msg-text  = iv_msg.
    <ls_log>-msg-type  = iv_type.
    <ls_log>-rc        = iv_rc.
    <ls_log>-item      = is_item.
    <ls_log>-exception = ix_exc.

  ENDMETHOD.


  METHOD zif_abapgit_log~add_error.

    zif_abapgit_log~add(
      iv_msg  = iv_msg
      iv_type = 'E'
      is_item = is_item ).

  ENDMETHOD.


  METHOD zif_abapgit_log~add_exception.

    DATA lx_exc TYPE REF TO cx_root.
    DATA lv_msg TYPE string.
    lx_exc ?= ix_exc.
    DO.
      lv_msg = lx_exc->get_text( ).
      zif_abapgit_log~add( iv_msg  = lv_msg
                           iv_type = 'E'
                           is_item = is_item
                           ix_exc  = lx_exc ).
      IF lx_exc->previous IS BOUND.
        lx_exc = lx_exc->previous.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD zif_abapgit_log~add_info.

    zif_abapgit_log~add(
      iv_msg  = iv_msg
      iv_type = 'I'
      is_item = is_item ).

  ENDMETHOD.


  METHOD zif_abapgit_log~add_success.

    zif_abapgit_log~add(
      iv_msg  = iv_msg
      iv_type = 'S'
      is_item = is_item ).

  ENDMETHOD.


  METHOD zif_abapgit_log~add_warning.

    zif_abapgit_log~add(
      iv_msg  = iv_msg
      iv_type = 'W'
      is_item = is_item ).

  ENDMETHOD.


  METHOD zif_abapgit_log~clear.
    CLEAR mt_log.
  ENDMETHOD.


  METHOD zif_abapgit_log~count.
    rv_count = lines( mt_log ).
  ENDMETHOD.


  METHOD zif_abapgit_log~get_item_status.

    DATA lr_log         TYPE REF TO ty_log.
    DATA ls_msg         TYPE zif_abapgit_log=>ty_msg.
    DATA ls_item_status TYPE zif_abapgit_log=>ty_item_status_out.
    DATA lr_item_status TYPE REF TO zif_abapgit_log=>ty_item_status_out.

    "collect all message for all objects
    LOOP AT mt_log REFERENCE INTO lr_log.
      CLEAR ls_item_status.
      ls_item_status-item = lr_log->item.
      READ TABLE rt_item_status REFERENCE INTO lr_item_status
           WITH KEY item-obj_type = ls_item_status-item-obj_type
                    item-obj_name = ls_item_status-item-obj_name.
      IF sy-subrc <> 0.
        INSERT ls_item_status INTO TABLE rt_item_status.
        GET REFERENCE OF ls_item_status INTO lr_item_status.
      ENDIF.
      CLEAR ls_msg.
      ls_msg-type = lr_log->msg-type.
      ls_msg-text = lr_log->msg-text.
      INSERT ls_msg INTO TABLE lr_item_status->messages.
    ENDLOOP.

    "determine object status from object messages
    LOOP AT rt_item_status REFERENCE INTO lr_item_status.
      lr_item_status->status = get_messages_status( lr_item_status->messages ).
      IF lr_item_status->messages IS INITIAL.
        CLEAR ls_msg.
        ls_msg-type = 'I'.
        ls_msg-text = 'No message'.
        INSERT ls_msg INTO TABLE lr_item_status->messages.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_log~get_messages.
    DATA ls_msg TYPE zif_abapgit_log~ty_log_out.
    FIELD-SYMBOLS <ls_log> TYPE ty_log.
    LOOP AT mt_log ASSIGNING <ls_log>.
      ls_msg-type      = <ls_log>-msg-type.
      ls_msg-text      = <ls_log>-msg-text.
      ls_msg-obj_type  = <ls_log>-item-obj_type.
      ls_msg-obj_name  = <ls_log>-item-obj_name.
      ls_msg-exception = <ls_log>-exception.
      APPEND ls_msg TO rt_msg.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_abapgit_log~get_status.

    DATA lr_log TYPE REF TO ty_log.
    rv_status = 'S'.
    LOOP AT mt_log REFERENCE INTO lr_log.
      CASE lr_log->msg-type.
        WHEN 'E' OR 'A' OR 'X'.
          rv_status = 'E'. "not okay
          EXIT.
        WHEN 'W'.
          rv_status = 'W'. "maybe
          CONTINUE.
        WHEN 'S' OR 'I'.
          IF rv_status <> 'W'.
            rv_status = 'S'. "okay
          ENDIF.
          CONTINUE.
        WHEN OTHERS. "unknown
          CONTINUE.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_log~get_title.
    rv_title = mv_title.
    IF rv_title IS INITIAL.
      rv_title = 'Log'.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_log~has_rc.
* todo, this method is only used in unit tests

    READ TABLE mt_log WITH KEY rc = iv_rc TRANSPORTING NO FIELDS.
    rv_yes = boolc( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD zif_abapgit_log~set_title.
    mv_title = iv_title.
  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_longtexts IMPLEMENTATION.


  METHOD read.

    DATA: ls_longtext TYPE ty_longtext,
          lt_dokil    TYPE zif_abapgit_definitions=>ty_dokil_tt.

    FIELD-SYMBOLS: <ls_dokil> LIKE LINE OF lt_dokil.

    IF lines( it_dokil ) > 0.

      lt_dokil = it_dokil.

    ELSEIF iv_longtext_id IS NOT INITIAL.
      IF iv_main_lang_only = abap_true.
        SELECT * FROM dokil
                 INTO TABLE lt_dokil
                 WHERE id     = iv_longtext_id
                 AND object = iv_object_name
                 AND masterlang = abap_true
                 ORDER BY PRIMARY KEY.
      ELSE.
        SELECT * FROM dokil
                 INTO TABLE lt_dokil
                 WHERE id     = iv_longtext_id
                 AND object = iv_object_name
                 ORDER BY PRIMARY KEY.
      ENDIF.
    ELSE.

      zcx_abapgit_exception=>raise( |serialize_longtexts parameter error| ).

    ENDIF.

    LOOP AT lt_dokil ASSIGNING <ls_dokil>
                     WHERE txtlines > 0.

      CLEAR: ls_longtext.

      ls_longtext-dokil = <ls_dokil>.

      CALL FUNCTION 'DOCU_READ'
        EXPORTING
          id      = <ls_dokil>-id
          langu   = <ls_dokil>-langu
          object  = <ls_dokil>-object
          typ     = <ls_dokil>-typ
          version = <ls_dokil>-version
        IMPORTING
          head    = ls_longtext-head
        TABLES
          line    = ls_longtext-lines.

      IF iv_clear_fields = abap_true.
        CLEAR: ls_longtext-head-tdfuser,
               ls_longtext-head-tdfreles,
               ls_longtext-head-tdfdate,
               ls_longtext-head-tdftime,
               ls_longtext-head-tdluser,
               ls_longtext-head-tdlreles,
               ls_longtext-head-tdldate,
               ls_longtext-head-tdltime.
      ENDIF.

      INSERT ls_longtext INTO TABLE rt_longtexts.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_longtexts~changed_by.

    DATA: lt_longtexts TYPE ty_longtexts.
    FIELD-SYMBOLS: <ls_longtext> TYPE ty_longtext.

    lt_longtexts = read( iv_object_name  = iv_object_name
                         iv_longtext_id  = iv_longtext_id
                         it_dokil        = it_dokil
                         iv_clear_fields = abap_false ).

    READ TABLE lt_longtexts INDEX 1 ASSIGNING <ls_longtext>.
    IF sy-subrc = 0.
      rv_user = <ls_longtext>-head-tdluser.
      IF rv_user IS INITIAL.
        rv_user = <ls_longtext>-head-tdfuser.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_longtexts~delete.

    DATA: lt_dokil TYPE zif_abapgit_definitions=>ty_dokil_tt.
    FIELD-SYMBOLS: <ls_dokil> TYPE dokil.

    SELECT * FROM dokil
      INTO TABLE lt_dokil
      WHERE id     = iv_longtext_id
      AND   object = iv_object_name.

    LOOP AT lt_dokil ASSIGNING <ls_dokil>.

      CALL FUNCTION 'DOCU_DEL'
        EXPORTING
          id       = <ls_dokil>-id
          langu    = <ls_dokil>-langu
          object   = <ls_dokil>-object
          typ      = <ls_dokil>-typ
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_longtexts~deserialize.

    DATA: lt_longtexts    TYPE ty_longtexts,
          lv_no_main_lang TYPE dokil-masterlang.
    FIELD-SYMBOLS: <ls_longtext> TYPE ty_longtext.

    ii_xml->read(
      EXPORTING
        iv_name = iv_longtext_name
      CHANGING
        cg_data = lt_longtexts ).

    LOOP AT lt_longtexts ASSIGNING <ls_longtext>.

      lv_no_main_lang = boolc( iv_main_language <> <ls_longtext>-dokil-langu ).

      CALL FUNCTION 'DOCU_UPDATE'
        EXPORTING
          head          = <ls_longtext>-head
          state         = c_docu_state_active
          typ           = <ls_longtext>-dokil-typ
          version       = <ls_longtext>-dokil-version
          no_masterlang = lv_no_main_lang
        TABLES
          line          = <ls_longtext>-lines.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_longtexts~serialize.

    DATA lt_longtexts TYPE ty_longtexts.
    DATA lt_dokil LIKE it_dokil.
    DATA lv_main_lang_only TYPE abap_bool.

    lt_dokil = it_dokil.
    lv_main_lang_only = ii_xml->i18n_params( )-main_language_only.
    IF lv_main_lang_only = abap_true.
      DELETE lt_dokil WHERE masterlang <> abap_true.
    ENDIF.

    lt_longtexts = read( iv_object_name    = iv_object_name
                         iv_longtext_id    = iv_longtext_id
                         it_dokil          = lt_dokil
                         iv_main_lang_only = lv_main_lang_only ).

    ii_xml->add( iv_name = iv_longtext_name
                 ig_data = lt_longtexts ).

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_LXE_TEXTS IMPLEMENTATION.


  METHOD check_langs_versus_installed.

    DATA lt_installed_hash TYPE HASHED TABLE OF laiso WITH UNIQUE KEY table_line.
    FIELD-SYMBOLS <lv_lang> LIKE LINE OF it_languages.

    CLEAR: et_intersection, et_missfits.
    lt_installed_hash = it_installed.

    LOOP AT it_languages ASSIGNING <lv_lang>.
      READ TABLE lt_installed_hash WITH KEY table_line = <lv_lang> TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        APPEND <lv_lang> TO et_intersection.
      ELSE.
        APPEND <lv_lang> TO et_missfits.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD convert_lang_string_to_table.

    DATA:
      lt_langs_str TYPE string_table,
      lv_laiso     TYPE laiso,
      lv_langu     TYPE spras,
      lv_skip_main_lang_iso TYPE laiso.

    FIELD-SYMBOLS:
      <lv_str>  LIKE LINE OF lt_langs_str.

    " Keep * as indicator for 'all installed languages'
    IF iv_langs = '*'.
      APPEND iv_langs TO rt_languages.
      RETURN.
    ENDIF.

    " Convert string of 2-letter ISO languages into table of sy-langu codes
    SPLIT iv_langs AT ',' INTO TABLE lt_langs_str.

    LOOP AT lt_langs_str ASSIGNING <lv_str>.
      lv_laiso = condense( to_upper( <lv_str> ) ).
      APPEND lv_laiso TO rt_languages.
    ENDLOOP.

    IF iv_skip_main_language IS NOT INITIAL.
      lv_skip_main_lang_iso = langu_to_laiso_safe( iv_skip_main_language ).
      DELETE rt_languages WHERE table_line = lv_skip_main_lang_iso.
    ENDIF.

    SORT rt_languages.
    DELETE ADJACENT DUPLICATES FROM rt_languages.

  ENDMETHOD.


  METHOD convert_table_to_lang_string.

    DATA:
      lt_langs_str TYPE string_table.

    FIELD-SYMBOLS:
      <lv_lang> LIKE LINE OF it_languages,
      <lv_str>  TYPE string.

    " Convert table of sy-langu codes into string of 2-letter ISO languages
    LOOP AT it_languages ASSIGNING <lv_lang>.
      " Keep * as indicator for 'all installed languages'
      IF <lv_lang> = '*'.
        CLEAR lt_langs_str.
        APPEND '*' TO lt_langs_str.
        EXIT.
      ENDIF.

      APPEND INITIAL LINE TO lt_langs_str ASSIGNING <lv_str>.
      <lv_str> = <lv_lang>.
    ENDLOOP.

    CONCATENATE LINES OF lt_langs_str INTO rv_langs SEPARATED BY ','.

  ENDMETHOD.


  METHOD detect_unsupported_languages.

    check_langs_versus_installed(
      EXPORTING
        it_languages = it_languages
        it_installed = get_installed_languages( )
      IMPORTING
        et_missfits = rt_unsupported_languages ).

  ENDMETHOD.


  METHOD get_installed_languages.

    DATA:
      lv_index               TYPE i,
      lv_langu               TYPE sy-langu,
      lv_laiso               TYPE laiso,
      lv_installed_languages TYPE string.

    IF gt_installed_languages_cache IS INITIAL.
      CALL FUNCTION 'SYSTEM_INSTALLED_LANGUAGES'
        IMPORTING
          languages       = lv_installed_languages
        EXCEPTIONS
          sapgparam_error = 1                " Error requesting profile parameter
          OTHERS          = 2.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Fail to get system SYSTEM_INSTALLED_LANGUAGES' ).
      ENDIF.

      DO strlen( lv_installed_languages ) TIMES.
        lv_index = sy-index - 1.
        lv_langu = lv_installed_languages+lv_index(1).
        lv_laiso = langu_to_laiso_safe( lv_langu ).
        APPEND lv_laiso TO gt_installed_languages_cache.
      ENDDO.
    ENDIF.

    rt_languages = gt_installed_languages_cache.

  ENDMETHOD.


  METHOD get_lang_iso4.

    DATA lv_lang_iso639 TYPE i18_a_langiso2.
    DATA lv_country TYPE land1.

    cl_i18n_languages=>sap2_to_iso639_1(
      EXPORTING
        im_lang_sap2   = iv_src
      IMPORTING
        ex_lang_iso639 = lv_lang_iso639
        ex_country     = lv_country
      EXCEPTIONS
        no_assignment  = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Failed to convert [{ iv_src }] lang to iso639| ).
    ENDIF.

    CONCATENATE lv_lang_iso639 lv_country INTO rv_iso4.

  ENDMETHOD.


  METHOD get_lxe_object_list.

    DATA lv_object_name TYPE trobj_name.

    lv_object_name = iv_object_name.

    CALL FUNCTION 'LXE_OBJ_EXPAND_TRANSPORT_OBJ'
      EXPORTING
        pgmid           = 'R3TR'
        object          = iv_object_type
        obj_name        = lv_object_name
      TABLES
        ex_colob        = rt_obj_list
      EXCEPTIONS
        unknown_object  = 1
        unknown_ta_type = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      RETURN. " Ignore error and return empty list
    ENDIF.

  ENDMETHOD.


  METHOD get_translation_languages.

    " Returns a list of translation languages for serialization
    " If the setting is initial, no translations shall be serialized
    " If the setting is `*`, all all installed system languages shall be serialized
    " Else, the setting shall contain all languages to be serialized

    DATA lv_main_lang_laiso TYPE laiso.

    IF it_i18n_languages IS NOT INITIAL.
      READ TABLE it_i18n_languages TRANSPORTING NO FIELDS WITH KEY table_line = '*'.
      IF sy-subrc = 0.
        rt_languages = get_installed_languages( ).
      ELSE.
        check_langs_versus_installed(
          EXPORTING
            it_languages = it_i18n_languages
            it_installed = get_installed_languages( )
          IMPORTING
            et_intersection = rt_languages ).
      ENDIF.
    ENDIF.

    " Remove main language from translation languages
    lv_main_lang_laiso = langu_to_laiso_safe( iv_main_language ).
    DELETE rt_languages WHERE table_line = lv_main_lang_laiso.

  ENDMETHOD.


  METHOD langu_to_laiso_safe.

    cl_i18n_languages=>sap1_to_sap2(
      EXPORTING
        im_lang_sap1  = iv_langu
      RECEIVING
        re_lang_sap2  = rv_laiso
      EXCEPTIONS
        no_assignment = 1
        OTHERS        = 2 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Could not convert lang [{ iv_langu }] to ISO| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_lxe_texts~deserialize.

    DATA:
      lt_lxe_texts      TYPE zif_abapgit_lxe_texts=>ty_tlxe_i18n,
      ls_lxe_item       TYPE zif_abapgit_lxe_texts=>ty_lxe_i18n,
      lt_text_pairs_tmp LIKE ls_lxe_item-text_pairs.

    ii_xml->read( EXPORTING iv_name = iv_lxe_text_name
                  CHANGING  cg_data = lt_lxe_texts ).

    LOOP AT lt_lxe_texts INTO ls_lxe_item.
      " Call Read first for buffer prefill
      CLEAR: lt_text_pairs_tmp.
      CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
        EXPORTING
          s_lang    = ls_lxe_item-source_lang
          t_lang    = ls_lxe_item-target_lang
          custmnr   = ls_lxe_item-custmnr
          objtype   = ls_lxe_item-objtype
          objname   = ls_lxe_item-objname
          read_only = abap_false
        TABLES
          lt_pcx_s1 = lt_text_pairs_tmp.

      "Call actual Write FM
      CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_WRITE'
        EXPORTING
          s_lang    = ls_lxe_item-source_lang
          t_lang    = ls_lxe_item-target_lang
          custmnr   = ls_lxe_item-custmnr
          objtype   = ls_lxe_item-objtype
          objname   = ls_lxe_item-objname
        TABLES
          lt_pcx_s1 = ls_lxe_item-text_pairs.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_lxe_texts~serialize.

    DATA:
      lt_obj_list      TYPE lxe_tt_colob,
      lv_main_lang     TYPE lxeisolang,
      lt_languages     TYPE zif_abapgit_definitions=>ty_languages,
      lt_lxe_texts     TYPE zif_abapgit_lxe_texts=>ty_tlxe_i18n,
      ls_lxe_text_item TYPE zif_abapgit_lxe_texts=>ty_lxe_i18n.

    FIELD-SYMBOLS:
      <lv_language>   LIKE LINE OF lt_languages,
      <lv_lxe_object> LIKE LINE OF lt_obj_list.

    lt_obj_list = get_lxe_object_list(
                    iv_object_name = iv_object_name
                    iv_object_type = iv_object_type ).

    IF lt_obj_list IS INITIAL.
      RETURN.
    ENDIF.

    " Get list of languages that need to be serialized (already resolves * and installed languages)
    lv_main_lang = get_lang_iso4( langu_to_laiso_safe( ii_xml->i18n_params( )-main_language ) ).
    lt_languages = ii_xml->i18n_params( )-translation_languages.

    LOOP AT lt_obj_list ASSIGNING <lv_lxe_object>.
      CLEAR ls_lxe_text_item.
      ls_lxe_text_item-custmnr = <lv_lxe_object>-custmnr.
      ls_lxe_text_item-objtype = <lv_lxe_object>-objtype.
      ls_lxe_text_item-objname = <lv_lxe_object>-objname.

      LOOP AT lt_languages ASSIGNING <lv_language>.
        ls_lxe_text_item-source_lang = lv_main_lang.
        ls_lxe_text_item-target_lang = get_lang_iso4( <lv_language> ).
        IF ls_lxe_text_item-source_lang = ls_lxe_text_item-target_lang.
          CONTINUE. " if source = target -> skip
        ENDIF.

        CLEAR ls_lxe_text_item-text_pairs.
        CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
          EXPORTING
            s_lang    = ls_lxe_text_item-source_lang
            t_lang    = ls_lxe_text_item-target_lang
            custmnr   = ls_lxe_text_item-custmnr
            objtype   = ls_lxe_text_item-objtype
            objname   = ls_lxe_text_item-objname
          TABLES
            lt_pcx_s1 = ls_lxe_text_item-text_pairs.

        IF ls_lxe_text_item-text_pairs IS NOT INITIAL.
          APPEND ls_lxe_text_item TO lt_lxe_texts.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    ii_xml->add( iv_name = iv_lxe_text_name
                 ig_data = lt_lxe_texts ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_objects_activation IMPLEMENTATION.


  METHOD activate.

    " Make sure that all changes are committed since any activation error will lead to a rollback
    COMMIT WORK AND WAIT.

    IF use_new_activation_logic( ) = abap_true.
      activate_new( iv_ddic ).
    ELSE.
      activate_old( iv_ddic ).
    ENDIF.

    update_where_used( ).

  ENDMETHOD.


  METHOD activate_ddic.

    DATA: lt_gentab     TYPE STANDARD TABLE OF dcgentb,
          lv_rc         TYPE sy-subrc,
          ls_gentab     LIKE LINE OF lt_gentab,
          lt_deltab     TYPE STANDARD TABLE OF dcdeltb,
          lt_action_tab TYPE STANDARD TABLE OF dctablres,
          lv_logname    TYPE ddmass-logname.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF gt_objects.


    LOOP AT gt_objects ASSIGNING <ls_object>.
      " Filter types supported by mass activation
      IF is_ddic_type( <ls_object>-object ) = abap_false.
        CONTINUE.
      ENDIF.
      ls_gentab-tabix = sy-tabix.
      ls_gentab-type = <ls_object>-object.
      ls_gentab-name = <ls_object>-obj_name.
      IF ls_gentab-type = 'INDX' OR ls_gentab-type = 'XINX' OR ls_gentab-type = 'MCID'.
        CALL FUNCTION 'DD_E071_TO_DD'
          EXPORTING
            object   = <ls_object>-object
            obj_name = <ls_object>-obj_name
          IMPORTING
            name     = ls_gentab-name
            id       = ls_gentab-indx.
      ENDIF.
      INSERT ls_gentab INTO TABLE lt_gentab.
    ENDLOOP.

    IF lt_gentab IS NOT INITIAL.

      lv_logname = |ABAPGIT_{ sy-datum }_{ sy-uzeit }|.

      CALL FUNCTION 'DD_MASS_ACT_C3'
        EXPORTING
          ddmode         = 'O'
          medium         = 'T' " transport order
          device         = 'T' " saves to table DDRPH?
          version        = 'M' " activate newest
          logname        = lv_logname
          write_log      = abap_true
          log_head_tail  = abap_true
          t_on           = space
          prid           = 1
        IMPORTING
          act_rc         = lv_rc
        TABLES
          gentab         = lt_gentab
          deltab         = lt_deltab
          cnvtab         = lt_action_tab
        EXCEPTIONS
          access_failure = 1
          no_objects     = 2
          locked         = 3
          internal_error = 4
          OTHERS         = 5.

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      IF lv_rc > 0.
        show_activation_errors( lv_logname ).
      ENDIF.

      " Remove objects from activation queue to avoid double activation in activate_old
      LOOP AT lt_gentab INTO ls_gentab.
        DELETE gt_objects WHERE object = ls_gentab-type AND obj_name = ls_gentab-name.
      ENDLOOP.
      DELETE gt_objects WHERE object = 'INDX' OR object = 'XINX' OR object = 'MCID'.

    ENDIF.

  ENDMETHOD.


  METHOD activate_new.

    DATA: li_progress TYPE REF TO zif_abapgit_progress.

    IF gt_objects IS INITIAL.
      RETURN.
    ENDIF.

    li_progress = zcl_abapgit_progress=>get_instance( 100 ).

    IF iv_ddic = abap_true.

      li_progress->show( iv_current = 98
                         iv_text    = 'Activating DDIC' ).

      activate_ddic( ).

    ELSE.

      li_progress->show( iv_current = 98
                         iv_text    = 'Activating non DDIC' ).

      activate_old( ).

    ENDIF.

  ENDMETHOD.


  METHOD activate_old.

    DATA: lv_popup TYPE abap_bool,
          lv_no_ui TYPE abap_bool.

    IF gt_objects IS NOT INITIAL.


      lv_no_ui = boolc( lv_popup = abap_false ).

      TRY.
          CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
            EXPORTING
              activate_ddic_objects  = iv_ddic
              with_popup             = lv_popup
              ui_decoupled           = lv_no_ui
            TABLES
              objects                = gt_objects
            EXCEPTIONS
              excecution_error       = 1
              cancelled              = 2
              insert_into_corr_error = 3
              OTHERS                 = 4 ##SUBRC_OK.
        CATCH cx_sy_dyn_call_param_not_found.
          CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
            EXPORTING
              activate_ddic_objects  = iv_ddic
              with_popup             = lv_popup
            TABLES
              objects                = gt_objects
            EXCEPTIONS
              excecution_error       = 1
              cancelled              = 2
              insert_into_corr_error = 3
              OTHERS                 = 4 ##SUBRC_OK.
      ENDTRY.
      CASE sy-subrc.
        WHEN 1 OR 3 OR 4.
          zcx_abapgit_exception=>raise_t100( ).
        WHEN 2.
          zcx_abapgit_exception=>raise( 'Activation cancelled. Check the inactive objects.' ).
      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD add.

* function group SEWORKINGAREA
* function module RS_INSERT_INTO_WORKING_AREA
* class CL_WB_ACTIVATION_WORK_AREA

    FIELD-SYMBOLS: <ls_object>  TYPE dwinactiv,
                   <ls_classes> LIKE LINE OF gt_classes.

    IF iv_type = 'CLAS' OR iv_type = 'INTF'.
      APPEND INITIAL LINE TO gt_classes ASSIGNING <ls_classes>.
      <ls_classes>-object  = iv_type.
      <ls_classes>-clsname = iv_name.
    ELSE.
      APPEND INITIAL LINE TO gt_objects ASSIGNING <ls_object>.
      <ls_object>-object     = iv_type.
      <ls_object>-obj_name   = iv_name.
      <ls_object>-delet_flag = iv_delete.
    ENDIF.

  ENDMETHOD.


  METHOD add_item.
    add( iv_type = is_item-obj_type
         iv_name = is_item-obj_name ).
  ENDMETHOD.


  METHOD clear.
    CLEAR gt_objects.
    CLEAR gt_classes.
  ENDMETHOD.


  METHOD is_ddic_type.

    " Determine if object can be handled by mass activation (see RADMASUTC form ma_tab_check)

    CONSTANTS:
      lc_domain     TYPE c LENGTH 9  VALUE 'DOMA DOMD',
      lc_types      TYPE c LENGTH 50 VALUE 'DTEL DTED TABL TABD SQLT SQLD TTYP TTYD VIEW VIED',
      lc_technset   TYPE c LENGTH 24 VALUE 'TABT VIET SQTT INDX XINX',
      lc_f4_objects TYPE c LENGTH 35 VALUE 'SHLP SHLD MCOB MCOD MACO MACD MCID',
      lc_enqueue    TYPE c LENGTH 9  VALUE 'ENQU ENQD',
      lc_sqsc       TYPE c LENGTH 4  VALUE 'SQSC',
      lc_stob       TYPE c LENGTH 4  VALUE 'STOB',
      lc_ntab       TYPE c LENGTH 14 VALUE 'NTTT NTTB NTDT',
      lc_ddls       TYPE c LENGTH 4  VALUE 'DDLS',
      lc_switches   TYPE c LENGTH 24 VALUE 'SF01 SF02 SFSW SFBS SFBF',
      lc_enhd       TYPE c LENGTH 4  VALUE 'ENHD'.

    rv_result = abap_true.
    IF lc_domain   NS iv_obj_type AND lc_types      NS iv_obj_type AND
       lc_technset NS iv_obj_type AND lc_f4_objects NS iv_obj_type AND
       lc_enqueue  NS iv_obj_type AND lc_sqsc       NS iv_obj_type AND
       lc_stob     NS iv_obj_type AND lc_ntab       NS iv_obj_type AND
       lc_ddls     NS iv_obj_type AND
       lc_switches NS iv_obj_type AND iv_obj_type <> lc_enhd.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD show_activation_errors.

    DATA: lt_lines      TYPE STANDARD TABLE OF trlog,
          lv_logname_db TYPE ddprh-protname,
          li_log        TYPE REF TO zif_abapgit_log.

    FIELD-SYMBOLS: <ls_line> LIKE LINE OF lt_lines.


    lv_logname_db = iv_logname.

    CALL FUNCTION 'TR_READ_LOG'
      EXPORTING
        iv_log_type   = 'DB'
        iv_logname_db = lv_logname_db
      TABLES
        et_lines      = lt_lines
      EXCEPTIONS
        invalid_input = 1
        access_error  = 2
        OTHERS        = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    DELETE lt_lines WHERE severity <> 'E'.

    CREATE OBJECT li_log TYPE zcl_abapgit_log.
    li_log->set_title( 'Activation Errors' ).

    LOOP AT lt_lines ASSIGNING <ls_line>.
      li_log->add( <ls_line>-line ).
    ENDLOOP.

    IF li_log->count( ) > 0.
    ENDIF.

  ENDMETHOD.


  METHOD update_where_used.

    DATA: ls_class    LIKE LINE OF gt_classes,
          lo_cross    TYPE REF TO cl_wb_crossreference,
          lv_include  TYPE programm,
          li_progress TYPE REF TO zif_abapgit_progress.


    li_progress = zcl_abapgit_progress=>get_instance( lines( gt_classes ) ).

    LOOP AT gt_classes INTO ls_class.
      IF sy-tabix MOD 20 = 0.
        li_progress->show(
          iv_current = sy-tabix
          iv_text    = 'Updating where-used lists' ).
      ENDIF.

      CASE ls_class-object.
        WHEN 'CLAS'.
          lv_include = cl_oo_classname_service=>get_classpool_name( ls_class-clsname ).
        WHEN 'INTF'.
          lv_include = cl_oo_classname_service=>get_interfacepool_name( ls_class-clsname ).
      ENDCASE.

      CREATE OBJECT lo_cross
        EXPORTING
          p_name    = lv_include
          p_include = lv_include.

      lo_cross->index_actualize( ).
    ENDLOOP.

  ENDMETHOD.


  METHOD use_new_activation_logic.

    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = 'DD_MASS_ACT_C3'
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    IF sy-subrc = 0.
      rv_use_new_activation_logic = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_objects_super IMPLEMENTATION.


  METHOD constructor.
    ms_item = is_item.
    ASSERT NOT ms_item IS INITIAL.
    mv_language = iv_language.
    ASSERT NOT mv_language IS INITIAL.
  ENDMETHOD.


  METHOD corr_insert.

    DATA: lv_object       TYPE string,
          lv_object_class TYPE string.

    IF ig_object_class IS NOT INITIAL.
      lv_object_class = ig_object_class.
      IF ig_object_class = 'DICT'.
        CONCATENATE ms_item-obj_type ms_item-obj_name INTO lv_object.
      ELSE.
        lv_object = ms_item-obj_name.
      ENDIF.
    ELSE.
      lv_object_class = ms_item-obj_type.
      lv_object       = ms_item-obj_name.
    ENDIF.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = lv_object
        object_class        = lv_object_class
        devclass            = iv_package
        master_language     = mv_language
        global_lock         = abap_true
        mode                = 'I'
        suppress_dialog     = abap_true
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_ddic.

    DATA: lv_objname TYPE rsedd0-ddobjname,
          lv_objtype TYPE rsedd0-ddobjtype.

    lv_objname = ms_item-obj_name.
    lv_objtype = iv_objtype.

    TRY.
        CALL FUNCTION 'RS_DD_DELETE_OBJ'
          EXPORTING
            no_ask               = iv_no_ask
            objname              = lv_objname
            objtype              = lv_objtype
            no_ask_delete_append = iv_no_ask_delete_append
          EXCEPTIONS
            not_executed         = 1
            object_not_found     = 2
            object_not_specified = 3
            permission_failure   = 4
            dialog_needed        = 5
            OTHERS               = 6.
      CATCH cx_sy_dyn_call_param_not_found.
        " no_ask_delete_append not available in lower releases
        CALL FUNCTION 'RS_DD_DELETE_OBJ'
          EXPORTING
            no_ask               = iv_no_ask
            objname              = lv_objname
            objtype              = lv_objtype
          EXCEPTIONS
            not_executed         = 1
            object_not_found     = 2
            object_not_specified = 3
            permission_failure   = 4
            dialog_needed        = 5
            OTHERS               = 6.
    ENDTRY.

    IF sy-subrc = 5.
      zcx_abapgit_exception=>raise( |Object { ms_item-obj_type } { ms_item-obj_name
                                    } has dependencies and must be deleted manually| ).
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error deleting { ms_item-obj_type } { ms_item-obj_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_longtexts.

    zcl_abapinst_factory=>get_longtexts( )->delete(
        iv_longtext_id = iv_longtext_id
        iv_object_name = ms_item-obj_name  ).

  ENDMETHOD.


  METHOD deserialize_longtexts.

    zcl_abapinst_factory=>get_longtexts( )->deserialize(
      ii_xml           = ii_xml
      iv_main_language = mv_language ).

  ENDMETHOD.


  METHOD deserialize_lxe_texts.

    zcl_abapinst_factory=>get_lxe_texts( )->deserialize(
      iv_object_type = ms_item-obj_type
      iv_object_name = ms_item-obj_name
      ii_xml         = ii_xml ).

  ENDMETHOD.


  METHOD exists_a_lock_entry_for.

    DATA: lt_lock_entries TYPE STANDARD TABLE OF seqg3.

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        guname                = '*'
        garg                  = iv_argument
      TABLES
        enq                   = lt_lock_entries
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_lock_entries TRANSPORTING NO FIELDS
                               WITH KEY gobj = iv_lock_object.
    IF sy-subrc = 0.
      rv_exists_a_lock_entry = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_metadata.

    DATA: lv_class TYPE string.

    lv_class = cl_abap_classdescr=>describe_by_object_ref( me )->get_relative_name( ).

    REPLACE FIRST OCCURRENCE OF 'ZCL_ABAPGIT' IN lv_class WITH 'LCL'.

    rs_metadata-class = lv_class.
    rs_metadata-version = 'v1.0.0'.

  ENDMETHOD.


  METHOD is_active.

    DATA: lt_messages    TYPE STANDARD TABLE OF sprot_u WITH DEFAULT KEY,
          lt_e071_tadirs TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
          ls_e071_tadir  LIKE LINE OF lt_e071_tadirs.

    ms_item-inactive = abap_false.

    ls_e071_tadir-object   = ms_item-obj_type.
    ls_e071_tadir-obj_name = ms_item-obj_name.
    INSERT ls_e071_tadir INTO TABLE lt_e071_tadirs.

    CALL FUNCTION 'RS_INACTIVE_OBJECTS_WARNING'
      EXPORTING
        suppress_protocol         = abap_false
        with_program_includes     = abap_false
        suppress_dictionary_check = abap_false
      TABLES
        p_e071                    = lt_e071_tadirs
        p_xmsg                    = lt_messages.

    IF lt_messages IS NOT INITIAL.
      ms_item-inactive = abap_true.
    ENDIF.

    rv_active = boolc( ms_item-inactive = abap_false ).
  ENDMETHOD.


  METHOD jump_adt.

    DATA: lv_adt_link TYPE string,
          lx_error    TYPE REF TO cx_root.

    TRY.

        lv_adt_link = zcl_abapgit_adt_link=>generate(
          iv_obj_name     = iv_obj_name
          iv_obj_type     = iv_obj_type
          iv_sub_obj_name = iv_sub_obj_name
          iv_line_number  = iv_line_number ).

        cl_gui_frontend_services=>execute(
          EXPORTING  document = lv_adt_link
          EXCEPTIONS OTHERS   = 1 ).

        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( |ADT Jump Error - failed to open link { lv_adt_link }. Subrc={ sy-subrc }| ).
        ENDIF.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise( iv_text = 'ADT Jump Error'
                                      ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD jump_se11.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = ms_item-obj_name
        object_type         = ms_item-obj_type
        devclass            = ms_item-devclass
        in_new_window       = abap_true
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Jump to SE11 failed (subrc={ sy-subrc } ).| ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize_longtexts.

    zcl_abapinst_factory=>get_longtexts( )->serialize(
        iv_object_name = ms_item-obj_name
        iv_longtext_id = iv_longtext_id
        it_dokil       = it_dokil
        ii_xml         = ii_xml  ).

  ENDMETHOD.


  METHOD serialize_lxe_texts.

    IF ii_xml->i18n_params( )-main_language_only = abap_true OR
       ii_xml->i18n_params( )-translation_languages IS INITIAL.
      RETURN.
    ENDIF.

    zcl_abapinst_factory=>get_lxe_texts( )->serialize(
      iv_object_type = ms_item-obj_type
      iv_object_name = ms_item-obj_name
      ii_xml         = ii_xml ).

  ENDMETHOD.


  METHOD set_default_package.

    " In certain cases we need to set the package package via ABAP memory
    " because we can't supply it via the APIs.
    "
    " Set default package, see function module RS_CORR_INSERT FORM get_current_devclass.
    "
    " We use ABAP memory instead the SET parameter because it is
    " more reliable. SET parameter doesn't work when multiple objects
    " are deserialized which uses the ABAP memory mechanism.
    " We don't need to reset the memory as it is done in above mentioned form routine.

    EXPORT current_devclass FROM iv_package TO MEMORY ID 'EUK'.

  ENDMETHOD.


  METHOD tadir_insert.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus       = abap_false
        wi_tadir_pgmid      = 'R3TR'
        wi_tadir_object     = ms_item-obj_type
        wi_tadir_obj_name   = ms_item-obj_name
        wi_tadir_author     = sy-uname
        wi_tadir_devclass   = iv_package
        wi_tadir_masterlang = mv_language
        iv_delflag          = abap_false
      EXCEPTIONS
        OTHERS              = 1.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECTS_BRIDGE IMPLEMENTATION.


  METHOD class_constructor.

    DATA lt_plugin_class    TYPE STANDARD TABLE OF seoclsname WITH DEFAULT KEY.
    DATA lv_plugin_class    LIKE LINE OF lt_plugin_class.
    DATA lo_plugin          TYPE REF TO object.
    DATA lt_plugin_obj_type TYPE objtyptable.
    DATA ls_objtype_map     LIKE LINE OF gt_objtype_map.


    SELECT clsname
      FROM seometarel
      INTO TABLE lt_plugin_class
      WHERE refclsname LIKE 'ZCL_ABAPGITP_OBJECT%'
      AND version = '1'
      ORDER BY clsname.                                   "#EC CI_SUBRC

    CLEAR gt_objtype_map.
    LOOP AT lt_plugin_class INTO lv_plugin_class
        WHERE table_line <> 'ZCL_ABAPGITP_OBJECT_BY_SOBJ'.
* have the generic plugin only as fallback
      TRY.
          CREATE OBJECT lo_plugin TYPE (lv_plugin_class).
        CATCH cx_sy_create_object_error.
          CONTINUE. ">>>>>>>>>>>>>>
      ENDTRY.

      CALL METHOD lo_plugin->('GET_SUPPORTED_OBJ_TYPES')
        IMPORTING
          rt_obj_type = lt_plugin_obj_type.

      ls_objtype_map-plugin_class = lv_plugin_class.
      LOOP AT lt_plugin_obj_type INTO ls_objtype_map-obj_typ.
        INSERT ls_objtype_map INTO TABLE gt_objtype_map.
        IF sy-subrc <> 0.
* No exception in class-contructor possible.
* Anyway, a shortdump is more appropriate in this case
          ASSERT 'There must not be' =
            |multiple abapGit-Plugins for the same object type {
            ls_objtype_map-obj_typ }|.
        ENDIF.
      ENDLOOP.
    ENDLOOP. "at plugins

* and the same for the generic plugin if exists
* have the generic plugin only as fallback
    LOOP AT lt_plugin_class INTO lv_plugin_class
        WHERE table_line = 'ZCL_ABAPGITP_OBJECT_BY_SOBJ'.
      CREATE OBJECT lo_plugin TYPE (lv_plugin_class).

      CALL METHOD lo_plugin->('GET_SUPPORTED_OBJ_TYPES')
        RECEIVING
          rt_obj_type = lt_plugin_obj_type.

      ls_objtype_map-plugin_class = lv_plugin_class.
      LOOP AT lt_plugin_obj_type INTO ls_objtype_map-obj_typ.
        INSERT ls_objtype_map INTO TABLE gt_objtype_map. "knowingly ignore the subrc
      ENDLOOP.
    ENDLOOP. "at plugins

  ENDMETHOD.


  METHOD constructor.

    DATA ls_objtype_map LIKE LINE OF gt_objtype_map.

    super->constructor( is_item = is_item
                        iv_language = zif_abapgit_definitions=>c_english ).

*    determine the responsible plugin
    READ TABLE gt_objtype_map INTO ls_objtype_map
      WITH TABLE KEY obj_typ = is_item-obj_type.
    IF sy-subrc = 0.
      CREATE OBJECT mo_plugin TYPE (ls_objtype_map-plugin_class).

      CALL METHOD mo_plugin->('SET_ITEM')
        EXPORTING
          iv_obj_type = is_item-obj_type
          iv_obj_name = is_item-obj_name.
    ELSE.
      RAISE EXCEPTION TYPE cx_sy_create_object_error
        EXPORTING
          classname = 'LCL_OBJECTS_BRIDGE'.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    DATA lx_plugin TYPE REF TO cx_static_check.

    TRY.
        CALL METHOD mo_plugin->('ZIF_ABAPGITP_PLUGIN~DELETE').
      CATCH cx_static_check INTO lx_plugin.
        zcx_abapgit_exception=>raise( lx_plugin->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lx_plugin        TYPE REF TO cx_static_check.

    TRY.
        CALL METHOD mo_plugin->('WRAP_DESERIALIZE')
          EXPORTING
            iv_package = iv_package
            io_xml     = io_xml.
      CATCH cx_static_check INTO lx_plugin.
        zcx_abapgit_exception=>raise( lx_plugin->get_text( ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    CALL METHOD mo_plugin->('ZIF_ABAPGITP_PLUGIN~EXISTS')
      RECEIVING
        rv_bool = rv_bool.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.

    DATA ls_meta TYPE ty_metadata.

    CALL METHOD mo_plugin->('ZIF_ABAPGITP_PLUGIN~GET_METADATA')
      RECEIVING
        rs_metadata = ls_meta.

    IF ls_meta-late_deser = abap_true.
      APPEND zif_abapgit_object=>gc_step_id-late TO rt_steps.
    ELSEIF ls_meta-ddic = abap_true.
      APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
    ELSE.
      APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.

    DATA ls_meta TYPE ty_metadata.

    CALL METHOD mo_plugin->('ZIF_ABAPGITP_PLUGIN~GET_METADATA')
      RECEIVING
        rs_metadata = ls_meta.

    MOVE-CORRESPONDING ls_meta TO rs_metadata.

  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL METHOD mo_plugin->('ZIF_ABAPGITP_PLUGIN~JUMP').

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    CALL METHOD mo_plugin->('WRAP_SERIALIZE')
      EXPORTING
        io_xml = io_xml.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_xml IMPLEMENTATION.


  METHOD constructor.
    mi_ixml     = cl_ixml=>create( ).
    mi_xml_doc  = mi_ixml->create_document( ).
    mv_filename = iv_filename.
  ENDMETHOD.


  METHOD display_version_mismatch.

    DATA: lv_version TYPE string.
    DATA: lv_file    TYPE string.

    lv_version = |abapGit version: { zif_abapgit_version=>gc_abap_version }|.
    IF mv_filename IS NOT INITIAL.
      lv_file = |File: { mv_filename }|.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'abapGit XML version mismatch'
        txt1  = 'abapGit XML version mismatch'
        txt2  = 'See https://docs.abapgit.org/other-xml-mismatch.html'
        txt3  = lv_version
        txt4  = lv_file.

    IF mv_filename IS INITIAL.
      zcx_abapgit_exception=>raise( 'abapGit XML version mismatch' ).
    ELSE.
      zcx_abapgit_exception=>raise( |abapGit XML version mismatch in file { mv_filename }| ).
    ENDIF.

  ENDMETHOD.


  METHOD error.

    IF ii_parser->num_errors( ) <> 0.

      IF zcl_abapinst_factory=>get_gui_functions( )->gui_is_available( ) = abap_true.
        show_parser_errors( ii_parser ).
      ELSE.
        raise_exception_for( ii_parser->get_error( 0 ) ).
      ENDIF.

    ENDIF.

    IF mv_filename IS INITIAL.
      zcx_abapgit_exception=>raise( |Error while parsing XML| ).
    ELSE.
      zcx_abapgit_exception=>raise( |Error while parsing XML file { mv_filename }| ).
    ENDIF.

  ENDMETHOD.


  METHOD parse.

    DATA: li_stream_factory TYPE REF TO if_ixml_stream_factory,
          li_istream        TYPE REF TO if_ixml_istream,
          li_element        TYPE REF TO if_ixml_element,
          li_version        TYPE REF TO if_ixml_node,
          li_parser         TYPE REF TO if_ixml_parser.


    ASSERT NOT iv_xml IS INITIAL.

    li_stream_factory = mi_ixml->create_stream_factory( ).
    li_istream = li_stream_factory->create_istream_string( iv_xml ).
    li_parser = mi_ixml->create_parser( stream_factory = li_stream_factory
                                        istream        = li_istream
                                        document       = mi_xml_doc ).
    li_parser->add_strip_space_element( ).
    IF li_parser->parse( ) <> 0.
      error( li_parser ).
    ENDIF.

    li_istream->close( ).


    li_element = mi_xml_doc->find_from_name_ns( depth = 0
                                                name = c_abapgit_tag ).
    li_version = li_element->if_ixml_node~get_attributes(
      )->get_named_item_ns( c_attr_version ).
    IF li_version->get_value( ) <> zif_abapgit_version=>gc_xml_version.
      display_version_mismatch( ).
    ENDIF.

* buffer serializer metadata. Git node will be removed lateron
    ms_metadata-class   = li_element->get_attribute_ns( c_attr_serializer ).
    ms_metadata-version = li_element->get_attribute_ns( c_attr_serializer_version ).

  ENDMETHOD.


  METHOD to_xml.
* will render to codepage UTF-16

    DATA: li_ostream       TYPE REF TO if_ixml_ostream,
          li_renderer      TYPE REF TO if_ixml_renderer,
          lv_mark          TYPE string,
          li_streamfactory TYPE REF TO if_ixml_stream_factory.


    li_streamfactory = mi_ixml->create_stream_factory( ).

    li_ostream = li_streamfactory->create_ostream_cstring( rv_xml ).

    li_renderer = mi_ixml->create_renderer( ostream  = li_ostream
                                            document = mi_xml_doc ).
    li_renderer->set_normalizing( iv_normalize ).

    li_renderer->render( ).

    "unicode systems always add the byte order mark to the xml, while non-unicode does not
    "this code will always add the byte order mark if it is not in the xml
    lv_mark = zcl_abapgit_convert=>xstring_to_string_utf8( cl_abap_char_utilities=>byte_order_mark_utf8 ).
    IF rv_xml(1) <> lv_mark.
      CONCATENATE lv_mark rv_xml INTO rv_xml.
    ENDIF.

  ENDMETHOD.

  METHOD show_parser_errors.

    DATA lv_error TYPE i.
    DATA lv_column TYPE string.
    DATA lv_line TYPE string.
    DATA lv_reason TYPE string.
    DATA lv_txt1 TYPE string.
    DATA lv_txt2 TYPE string.
    DATA lv_txt3 TYPE string.
    DATA lv_txt4 TYPE string.
    DATA lv_times TYPE i.
    DATA li_error TYPE REF TO if_ixml_parse_error.

    lv_times = ii_parser->num_errors( ).

    DO lv_times TIMES.
      lv_error = sy-index - 1.
      li_error = ii_parser->get_error( lv_error ).

      lv_column = li_error->get_column( ).
      lv_line   = li_error->get_line( ).
      lv_reason = li_error->get_reason( ).

      IF mv_filename IS NOT INITIAL.
        lv_txt1 = |File: { mv_filename }|.
        lv_txt2 = |Column: { lv_column }|.
        lv_txt3 = |Line: { lv_line }|.
        lv_txt4 = lv_reason.
      ELSE.
        lv_txt1 = |Column: { lv_column }|.
        lv_txt2 = |Line: { lv_line }|.
        lv_txt3 = lv_reason.
        CLEAR lv_txt4.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Error from XML parser'
          txt1  = lv_txt1
          txt2  = lv_txt2
          txt3  = lv_txt3
          txt4  = lv_txt4.
    ENDDO.

  ENDMETHOD.


  METHOD raise_exception_for.
    DATA lv_message TYPE string.

    lv_message = |XML parser error: { ii_error->get_reason( ) }, | &&
                 |Line { ii_error->get_line( ) } | &&
                 |Col. { ii_error->get_column( ) }|.

    IF mv_filename IS NOT INITIAL.
      lv_message = lv_message && | File { mv_filename }|.
    ENDIF.

    zcx_abapgit_exception=>raise( lv_message ).

  ENDMETHOD.

ENDCLASS.



CLASS zcl_abapgit_xml_input IMPLEMENTATION.


  METHOD constructor.

    super->constructor( iv_filename ).
    parse( iv_xml ).
    fix_xml( ).

  ENDMETHOD.


  METHOD fix_xml.

    DATA: li_git  TYPE REF TO if_ixml_element,
          li_abap TYPE REF TO if_ixml_node.


    li_git ?= mi_xml_doc->find_from_name_ns( depth = 0
                                             name = c_abapgit_tag ).
    li_abap = li_git->get_first_child( ).

    mi_xml_doc->get_root( )->remove_child( li_git ).
    mi_xml_doc->get_root( )->append_child( li_abap ).

  ENDMETHOD.


  METHOD zif_abapgit_xml_input~get_metadata.
    rs_metadata = ms_metadata.
  ENDMETHOD.


  METHOD zif_abapgit_xml_input~get_raw.
    ri_raw = mi_xml_doc.
  ENDMETHOD.


  METHOD zif_abapgit_xml_input~read.

    DATA: lx_error TYPE REF TO cx_transformation_error,
          lt_rtab  TYPE abap_trans_resbind_tab.

    FIELD-SYMBOLS: <ls_rtab> LIKE LINE OF lt_rtab.

    ASSERT NOT iv_name IS INITIAL.

    CLEAR cg_data. "Initialize result to avoid problems with empty values

    APPEND INITIAL LINE TO lt_rtab ASSIGNING <ls_rtab>.
    <ls_rtab>-name = iv_name.
    GET REFERENCE OF cg_data INTO <ls_rtab>-value.

    TRY.
        CALL TRANSFORMATION id
          OPTIONS value_handling = 'accept_data_loss'
          SOURCE XML mi_xml_doc
          RESULT (lt_rtab).
      CATCH cx_transformation_error INTO lx_error.
        IF mv_filename IS INITIAL.
          zcx_abapgit_exception=>raise( lx_error->if_message~get_text( ) ).
        ELSE.
          zcx_abapgit_exception=>raise( |File { mv_filename }: { lx_error->if_message~get_text( ) }| ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_objects_files IMPLEMENTATION.


  METHOD add.
    APPEND is_file TO mt_files.
  ENDMETHOD.


  METHOD add_abap.

    DATA: lv_source TYPE string,
          ls_file   TYPE zif_abapgit_definitions=>ty_file.


    CONCATENATE LINES OF it_abap INTO lv_source SEPARATED BY zif_abapgit_definitions=>c_newline.
* when editing files via eg. GitHub web interface it adds a newline at end of file
    lv_source = lv_source && zif_abapgit_definitions=>c_newline.

    ls_file-path = '/'.
    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = 'abap' ).
    ls_file-data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_source ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.


  METHOD add_raw.

    DATA: ls_file TYPE zif_abapgit_definitions=>ty_file.

    ls_file-path     = '/'.
    ls_file-data     = iv_data.
    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = iv_ext ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.


  METHOD add_string.

    DATA: ls_file TYPE zif_abapgit_definitions=>ty_file.


    ls_file-path = '/'.
    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = iv_ext ).
    ls_file-data = zcl_abapgit_convert=>string_to_xstring_utf8( iv_string ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.


  METHOD add_xml.

    DATA: lv_xml  TYPE string,
          ls_file TYPE zif_abapgit_definitions=>ty_file.

    lv_xml = ii_xml->render( iv_normalize = iv_normalize
                             is_metadata = is_metadata ).
    ls_file-path = '/'.

    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = 'xml' ).

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN lv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.
    ASSERT sy-subrc = 0.

    ls_file-data = zcl_abapgit_convert=>string_to_xstring_utf8_bom( lv_xml ).

    APPEND ls_file TO mt_files.
  ENDMETHOD.


  METHOD constructor.
    ms_item = is_item.
    mv_path = iv_path.
  ENDMETHOD.


  METHOD contains.
    DATA: lv_filename TYPE string.

    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = iv_ext ).

    IF mv_path IS NOT INITIAL.
      READ TABLE mt_files TRANSPORTING NO FIELDS WITH KEY path     = mv_path
                                                          filename = lv_filename.
    ELSE.
      READ TABLE mt_files TRANSPORTING NO FIELDS WITH KEY filename = lv_filename.
    ENDIF.

    IF sy-subrc = 0.
      rv_present = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD filename.

    DATA: lv_obj_name TYPE string.


    lv_obj_name = ms_item-obj_name.

    " The counter part to this logic must be maintained in ZCL_ABAPGIT_FILE_STATUS->IDENTIFY_OBJECT
    IF ms_item-obj_type = 'DEVC'.
      " Packages have a fixed filename so that the repository can be installed to a different
      " package(-hierarchy) on the client and not show up as a different package in the repo.
      lv_obj_name = 'package'.
    ELSE.
      " Some characters in object names cause problems when identifying the object later
      " -> we escape these characters here
      " cl_http_utility=>escape_url doesn't do dots but escapes slash which we use for namespaces
      " -> we escape just some selected characters
      REPLACE ALL OCCURRENCES OF `%` IN lv_obj_name WITH '%25'.
      REPLACE ALL OCCURRENCES OF `#` IN lv_obj_name WITH '%23'.
      REPLACE ALL OCCURRENCES OF `.` IN lv_obj_name WITH '%2e'.
      REPLACE ALL OCCURRENCES OF `=` IN lv_obj_name WITH '%3d'.
      REPLACE ALL OCCURRENCES OF `?` IN lv_obj_name WITH '%3f'.
      REPLACE ALL OCCURRENCES OF `<` IN lv_obj_name WITH '%3c'.
      REPLACE ALL OCCURRENCES OF `>` IN lv_obj_name WITH '%3e'.
    ENDIF.

    IF iv_extra IS INITIAL.
      CONCATENATE lv_obj_name '.' ms_item-obj_type
        INTO rv_filename.
    ELSE.
      CONCATENATE lv_obj_name '.' ms_item-obj_type '.' iv_extra
        INTO rv_filename.
    ENDIF.

    IF iv_ext IS NOT INITIAL.
      CONCATENATE rv_filename '.' iv_ext
        INTO rv_filename.
    ENDIF.

    " handle namespaces
    REPLACE ALL OCCURRENCES OF '/' IN rv_filename WITH '#'.
    TRANSLATE rv_filename TO LOWER CASE.

  ENDMETHOD.


  METHOD get_accessed_files.
    rt_files = mt_accessed_files.
  ENDMETHOD.


  METHOD get_files.
    rt_files = mt_files.
  ENDMETHOD.


  METHOD get_file_pattern.
    rv_pattern = filename( iv_ext = '*' ).
    " Escape special characters for use with 'covers pattern' (CP)
    REPLACE ALL OCCURRENCES OF '#' IN rv_pattern WITH '##'.
    REPLACE ALL OCCURRENCES OF '+' IN rv_pattern WITH '#+'.
  ENDMETHOD.


  METHOD read_abap.

    DATA: lv_filename TYPE string,
          lv_data     TYPE xstring,
          lv_abap     TYPE string.


    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = 'abap' ).

    lv_data = read_file( iv_filename = lv_filename
                         iv_error    = iv_error ).

    IF lv_data IS INITIAL. " Post-handling of iv_error = false
      RETURN.
    ENDIF.

    lv_abap = zcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).

    SPLIT lv_abap AT zif_abapgit_definitions=>c_newline INTO TABLE rt_abap.

  ENDMETHOD.


  METHOD read_file.

    FIELD-SYMBOLS: <ls_file>     LIKE LINE OF mt_files,
                   <ls_accessed> LIKE LINE OF mt_accessed_files.


    IF mv_path IS NOT INITIAL.
      READ TABLE mt_files ASSIGNING <ls_file> WITH KEY path     = mv_path
                                                       filename = iv_filename.
    ELSE.
      READ TABLE mt_files ASSIGNING <ls_file> WITH KEY filename = iv_filename.
    ENDIF.

    IF sy-subrc <> 0.
      IF iv_error = abap_true.
        zcx_abapgit_exception=>raise( |File not found: { iv_filename }| ).
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    " Update access table
    READ TABLE mt_accessed_files TRANSPORTING NO FIELDS
      WITH KEY path = <ls_file>-path filename = <ls_file>-filename.
    IF sy-subrc > 0. " Not found ? -> Add
      APPEND INITIAL LINE TO mt_accessed_files ASSIGNING <ls_accessed>.
      MOVE-CORRESPONDING <ls_file> TO <ls_accessed>.
    ENDIF.

    rv_data = <ls_file>-data.

  ENDMETHOD.


  METHOD read_raw.

    DATA: lv_filename TYPE string.

    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = iv_ext ).

    rv_data = read_file( lv_filename ).

  ENDMETHOD.


  METHOD read_string.

    DATA: lv_filename TYPE string,
          lv_data     TYPE xstring.

    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = iv_ext ).

    lv_data = read_file( lv_filename ).

    rv_string = zcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).

  ENDMETHOD.


  METHOD read_xml.

    DATA: lv_filename TYPE string,
          lv_data     TYPE xstring,
          lv_xml      TYPE string.

    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = 'xml' ).

    lv_data = read_file( lv_filename ).

    lv_xml = zcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).

    CREATE OBJECT ri_xml
      TYPE zcl_abapgit_xml_input
      EXPORTING
        iv_xml      = lv_xml
        iv_filename = lv_filename.

  ENDMETHOD.


  METHOD set_files.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files.

    CLEAR mt_files.

    " Set only files matching the pattern for this object
    " If a path has been defined in the constructor, then the path has to match, too
    LOOP AT it_files ASSIGNING <ls_file> WHERE filename CP get_file_pattern( ).
      IF mv_path IS INITIAL.
        INSERT <ls_file> INTO TABLE mt_files.
      ELSEIF mv_path = <ls_file>-path.
        INSERT <ls_file> INTO TABLE mt_files.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_xml_output IMPLEMENTATION.


  METHOD build_asx_node.

    DATA: li_attr TYPE REF TO if_ixml_attribute.


    ri_element = mi_xml_doc->create_element_ns(
      name   = 'abap'
      prefix = 'asx' ).

    li_attr = mi_xml_doc->create_attribute_ns( 'version' ).
    li_attr->if_ixml_node~set_value( '1.0' ).
    ri_element->set_attribute_node_ns( li_attr ).

    li_attr = mi_xml_doc->create_attribute_ns(
      name   = 'asx'
      prefix = 'xmlns' ).
    li_attr->if_ixml_node~set_value( 'http://www.sap.com/abapxml' ).
    ri_element->set_attribute_node_ns( li_attr ).

  ENDMETHOD.


  METHOD zif_abapgit_xml_output~add.

    DATA: li_node TYPE REF TO if_ixml_node,
          li_doc  TYPE REF TO if_ixml_document,
          lt_stab TYPE abap_trans_srcbind_tab.

    FIELD-SYMBOLS: <ls_stab> LIKE LINE OF lt_stab.


    ASSERT NOT iv_name IS INITIAL.

    IF ig_data IS INITIAL.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO lt_stab ASSIGNING <ls_stab>.
    <ls_stab>-name = iv_name.
    GET REFERENCE OF ig_data INTO <ls_stab>-value.

    li_doc = cl_ixml=>create( )->create_document( ).

    CALL TRANSFORMATION id
      OPTIONS initial_components = 'suppress'
      SOURCE (lt_stab)
      RESULT XML li_doc.

    li_node = mi_xml_doc->get_root( )->get_first_child( ).
    IF li_node IS BOUND.
      mi_xml_doc->get_root( )->get_first_child( )->get_first_child( )->append_child(
        li_doc->get_root( )->get_first_child( )->get_first_child( )->get_first_child( ) ).
    ELSE.
      mi_xml_doc->get_root( )->append_child( li_doc->get_root( )->get_first_child( ) ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_xml_output~add_xml.

    DATA: li_element TYPE REF TO if_ixml_element.

    li_element = mi_xml_doc->create_element( iv_name ).
    li_element->append_child( ii_xml ).

    mi_xml_doc->get_root( )->get_first_child( )->get_first_child( )->append_child( li_element ).

  ENDMETHOD.


  METHOD zif_abapgit_xml_output~i18n_params.

    IF is_i18n_params IS SUPPLIED.
      ms_i18n_params = is_i18n_params.
    ENDIF.

    rs_i18n_params = ms_i18n_params.

  ENDMETHOD.


  METHOD zif_abapgit_xml_output~render.

    DATA: li_git  TYPE REF TO if_ixml_element,
          li_abap TYPE REF TO if_ixml_element.


    IF mi_raw IS INITIAL.
      li_abap ?= mi_xml_doc->get_root( )->get_first_child( ).
      mi_xml_doc->get_root( )->remove_child( li_abap ).
      IF li_abap IS INITIAL.
        li_abap = build_asx_node( ).
      ENDIF.
    ELSE.
      li_abap = mi_raw.
    ENDIF.

    li_git = mi_xml_doc->create_element( c_abapgit_tag ).
    li_git->set_attribute( name = c_attr_version
                           value = zif_abapgit_version=>gc_xml_version ).
    IF NOT is_metadata IS INITIAL.
      li_git->set_attribute( name  = c_attr_serializer
                             value = is_metadata-class ).
      li_git->set_attribute( name  = c_attr_serializer_version
                             value = is_metadata-version ).
    ENDIF.
    li_git->append_child( li_abap ).
    mi_xml_doc->get_root( )->append_child( li_git ).

    rv_xml = to_xml( iv_normalize ).

  ENDMETHOD.


  METHOD zif_abapgit_xml_output~set_raw.
    mi_raw = ii_raw.
  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_objects_program IMPLEMENTATION.


  METHOD add_tpool.

    FIELD-SYMBOLS: <ls_tpool_in>  LIKE LINE OF it_tpool,
                   <ls_tpool_out> LIKE LINE OF rt_tpool.


    LOOP AT it_tpool ASSIGNING <ls_tpool_in>.
      APPEND INITIAL LINE TO rt_tpool ASSIGNING <ls_tpool_out>.
      MOVE-CORRESPONDING <ls_tpool_in> TO <ls_tpool_out>.
      IF <ls_tpool_out>-id = 'S'.
        <ls_tpool_out>-split = <ls_tpool_out>-entry.
        <ls_tpool_out>-entry = <ls_tpool_out>-entry+8.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD auto_correct_cua_adm.
    " issue #1807 automatic correction of CUA interfaces saved incorrectly in the past (ADM was not saved in the XML)

    CONSTANTS:
      lc_num_n_space TYPE string VALUE ' 0123456789',
      lc_num_only    TYPE string VALUE '0123456789'.

    FIELD-SYMBOLS:
      <ls_pfk> TYPE rsmpe_pfk,
      <ls_act> TYPE rsmpe_act,
      <ls_men> TYPE rsmpe_men.

    IF cs_adm IS NOT INITIAL
        AND cs_adm-actcode CO lc_num_n_space
        AND cs_adm-mencode CO lc_num_n_space
        AND cs_adm-pfkcode CO lc_num_n_space. "Check performed in form check_adm of include LSMPIF03
      RETURN.
    ENDIF.

    LOOP AT is_cua-act ASSIGNING <ls_act>.
      IF <ls_act>-code+6(14) IS INITIAL AND <ls_act>-code(6) CO lc_num_only.
        cs_adm-actcode = <ls_act>-code.
      ENDIF.
    ENDLOOP.

    LOOP AT is_cua-men ASSIGNING <ls_men>.
      IF <ls_men>-code+6(14) IS INITIAL AND <ls_men>-code(6) CO lc_num_only.
        cs_adm-mencode = <ls_men>-code.
      ENDIF.
    ENDLOOP.

    LOOP AT is_cua-pfk ASSIGNING <ls_pfk>.
      IF <ls_pfk>-code+6(14) IS INITIAL AND <ls_pfk>-code(6) CO lc_num_only.
        cs_adm-pfkcode = <ls_pfk>-code.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize_cua.

    DATA: ls_tr_key TYPE trkey,
          ls_adm    TYPE rsmpe_adm.


    IF lines( is_cua-sta ) = 0
        AND lines( is_cua-fun ) = 0
        AND lines( is_cua-men ) = 0
        AND lines( is_cua-mtx ) = 0
        AND lines( is_cua-act ) = 0
        AND lines( is_cua-but ) = 0
        AND lines( is_cua-pfk ) = 0
        AND lines( is_cua-set ) = 0
        AND lines( is_cua-doc ) = 0
        AND lines( is_cua-tit ) = 0
        AND lines( is_cua-biv ) = 0.
      RETURN.
    ENDIF.

    SELECT SINGLE devclass INTO ls_tr_key-devclass
      FROM tadir
      WHERE pgmid = 'R3TR'
      AND object = ms_item-obj_type
      AND obj_name = ms_item-obj_name.                  "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'not found in tadir' ).
    ENDIF.

    ls_tr_key-obj_type = ms_item-obj_type.
    ls_tr_key-obj_name = ms_item-obj_name.
    ls_tr_key-sub_type = 'CUAD'.
    ls_tr_key-sub_name = iv_program_name.


    ls_adm = is_cua-adm.
    auto_correct_cua_adm( EXPORTING is_cua = is_cua CHANGING cs_adm = ls_adm ).

    sy-tcode = 'SE41' ##write_ok. " evil hack, workaround to handle fixes in note 2159455
    CALL FUNCTION 'RS_CUA_INTERNAL_WRITE'
      EXPORTING
        program   = iv_program_name
        language  = mv_language
        tr_key    = ls_tr_key
        adm       = ls_adm
        state     = 'I'
      TABLES
        sta       = is_cua-sta
        fun       = is_cua-fun
        men       = is_cua-men
        mtx       = is_cua-mtx
        act       = is_cua-act
        but       = is_cua-but
        pfk       = is_cua-pfk
        set       = is_cua-set
        doc       = is_cua-doc
        tit       = is_cua-tit
        biv       = is_cua-biv
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
* if moving code from SAPlink, see https://github.com/abapGit/abapGit/issues/562
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    zcl_abapgit_objects_activation=>add(
      iv_type = 'CUAD'
      iv_name = iv_program_name ).

  ENDMETHOD.


  METHOD deserialize_dynpros.

    CONSTANTS lc_rpyty_force_off TYPE c LENGTH 1 VALUE '/'.

    DATA: lv_name   TYPE dwinactiv-obj_name,
          ls_dynpro LIKE LINE OF it_dynpros.

    FIELD-SYMBOLS: <ls_field> TYPE rpy_dyfatc.

* ls_dynpro is changed by the function module, a field-symbol will cause
* the program to dump since it_dynpros cannot be changed
    LOOP AT it_dynpros INTO ls_dynpro.

      " todo: kept for compatibility, remove after grace period #3680
      ls_dynpro-flow_logic = uncondense_flow(
        it_flow = ls_dynpro-flow_logic
        it_spaces = ls_dynpro-spaces ).


      LOOP AT ls_dynpro-fields ASSIGNING <ls_field>.
* if the DDIC element has a PARAMETER_ID and the flag "from_dict" is active
* the import will enable the SET-/GET_PARAM flag. In this case: "force off"
        IF <ls_field>-param_id IS NOT INITIAL
            AND <ls_field>-from_dict = abap_true.
          IF <ls_field>-set_param IS INITIAL.
            <ls_field>-set_param = lc_rpyty_force_off.
          ENDIF.
          IF <ls_field>-get_param IS INITIAL.
            <ls_field>-get_param = lc_rpyty_force_off.
          ENDIF.
        ENDIF.

* If the previous conditions are met the value 'F' will be taken over
* during de-serialization potentially overlapping other fields in the screen,
* we set the tag to the correct value 'X'
        IF <ls_field>-type = 'CHECK'
            AND <ls_field>-from_dict = abap_true
            AND <ls_field>-text IS INITIAL
            AND <ls_field>-modific IS INITIAL.
          <ls_field>-modific = 'X'.
        ENDIF.

        "fix for issue #2747:
        IF <ls_field>-foreignkey IS INITIAL.
          <ls_field>-foreignkey = lc_rpyty_force_off.
        ENDIF.

      ENDLOOP.

      CALL FUNCTION 'RPY_DYNPRO_INSERT'
        EXPORTING
          header                 = ls_dynpro-header
          suppress_exist_checks  = abap_true
        TABLES
          containers             = ls_dynpro-containers
          fields_to_containers   = ls_dynpro-fields
          flow_logic             = ls_dynpro-flow_logic
        EXCEPTIONS
          cancelled              = 1
          already_exists         = 2
          program_not_exists     = 3
          not_executed           = 4
          missing_required_field = 5
          illegal_field_value    = 6
          field_not_allowed      = 7
          not_generated          = 8
          illegal_field_position = 9
          OTHERS                 = 10.
      IF sy-subrc <> 2 AND sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
* todo, RPY_DYNPRO_UPDATE?

      CONCATENATE ls_dynpro-header-program ls_dynpro-header-screen
        INTO lv_name RESPECTING BLANKS.
      ASSERT NOT lv_name IS INITIAL.

      zcl_abapgit_objects_activation=>add(
        iv_type = 'DYNP'
        iv_name = lv_name ).

    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize_program.

    DATA: lv_exists      TYPE abap_bool,
          lt_empty_src   LIKE it_source,
          lv_progname    TYPE reposrc-progname,
          ls_tpool       LIKE LINE OF it_tpool,
          lv_title       TYPE rglif-title,
          ls_progdir_new TYPE progdir.

    FIELD-SYMBOLS: <lg_any> TYPE any.


    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = is_progdir-name
        object_class        = 'ABAP'
        devclass            = iv_package
        master_language     = mv_language
        mode                = 'I'
        suppress_dialog     = abap_true
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE it_tpool INTO ls_tpool WITH KEY id = 'R'.
    IF sy-subrc = 0.
* there is a bug in RPY_PROGRAM_UPDATE, the header line of TTAB is not
* cleared, so the title length might be inherited from a different program.
      ASSIGN ('(SAPLSIFP)TTAB') TO <lg_any>.
      IF sy-subrc = 0.
        CLEAR <lg_any>.
      ENDIF.

      lv_title = ls_tpool-entry.
    ENDIF.

    SELECT SINGLE progname FROM reposrc INTO lv_progname
      WHERE progname = is_progdir-name
      AND r3state = 'A'.
    lv_exists = boolc( sy-subrc = 0 ).

    IF lv_exists = abap_true.
      zcl_abapgit_language=>set_current_language( mv_language ).

      CALL FUNCTION 'RPY_PROGRAM_UPDATE'
        EXPORTING
          program_name     = is_progdir-name
          title_string     = lv_title
          save_inactive    = 'I'
        TABLES
          source_extended  = it_source
        EXCEPTIONS
          cancelled        = 1
          permission_error = 2
          not_found        = 3
          OTHERS           = 4.

      IF sy-subrc <> 0.
        zcl_abapgit_language=>restore_login_language( ).

        IF sy-msgid = 'EU' AND sy-msgno = '510'.
          zcx_abapgit_exception=>raise( 'User is currently editing program' ).
        ELSEIF sy-msgid = 'EU' AND sy-msgno = '522'.
* for generated table maintenance function groups, the author is set to SAP* instead of the user which
* generates the function group. This hits some standard checks, pulling new code again sets the author
* to the current user which avoids the check
          zcx_abapgit_exception=>raise( |Delete function group and pull again, { is_progdir-name } (EU522)| ).
        ELSE.
          zcx_abapgit_exception=>raise_t100( ).
        ENDIF.
      ENDIF.

      zcl_abapgit_language=>restore_login_language( ).
    ELSEIF strlen( is_progdir-name ) > 30.
* function module RPY_PROGRAM_INSERT cannot handle function group includes
      " special treatment for extensions
      " if the program name exceeds 30 characters it is not a usual
      " ABAP program but might be some extension, which requires the internal
      " addition EXTENSION TYPE, see
      " http://help.sap.com/abapdocu_751/en/abapinsert_report_internal.htm#!ABAP_ADDITION_1@1@
      " This e.g. occurs in case of transportable Code Inspector variants (ending with ===VC)
      INSERT REPORT is_progdir-name
        FROM it_source
        STATE 'I'
        EXTENSION TYPE is_progdir-name+30.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'error from INSERT REPORT .. EXTENSION TYPE' ).
      ENDIF.
    ELSE.
      INSERT REPORT is_progdir-name
        FROM it_source
        STATE 'I'
        PROGRAM TYPE is_progdir-subc.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    IF NOT it_tpool[] IS INITIAL.
      INSERT TEXTPOOL is_progdir-name
        FROM it_tpool
        LANGUAGE mv_language
        STATE 'I'.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'error from INSERT TEXTPOOL' ).
      ENDIF.
    ENDIF.

    CALL FUNCTION 'READ_PROGDIR'
      EXPORTING
        i_progname = is_progdir-name
        i_state    = 'I'
      IMPORTING
        e_progdir  = ls_progdir_new
      EXCEPTIONS
        not_exists = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |not found in PROGDIR. Subrc = { sy-subrc }| ).
    ENDIF.

* todo, package?

    ls_progdir_new-ldbname = is_progdir-ldbname.
    ls_progdir_new-dbna    = is_progdir-dbna.
    ls_progdir_new-dbapl   = is_progdir-dbapl.
    ls_progdir_new-rload   = is_progdir-rload.
    ls_progdir_new-fixpt   = is_progdir-fixpt.
    ls_progdir_new-varcl   = is_progdir-varcl.
    ls_progdir_new-appl    = is_progdir-appl.
    ls_progdir_new-rstat   = is_progdir-rstat.
    ls_progdir_new-sqlx    = is_progdir-sqlx.
    ls_progdir_new-uccheck = is_progdir-uccheck.
    ls_progdir_new-clas    = is_progdir-clas.

    CALL FUNCTION 'UPDATE_PROGDIR'
      EXPORTING
        i_progdir    = ls_progdir_new
        i_progname   = ls_progdir_new-name
        i_state      = ls_progdir_new-state
      EXCEPTIONS
        not_executed = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |PROG, error inserting. Subrc = { sy-subrc }| ).
    ENDIF.

    SELECT SINGLE * FROM progdir INTO ls_progdir_new
      WHERE name = ls_progdir_new-name
      AND state = ls_progdir_new-state.
    IF sy-subrc = 0 AND is_progdir-varcl = space AND ls_progdir_new-varcl = abap_true.
* function module UPDATE_PROGDIR does not update VARCL
      UPDATE progdir SET varcl = is_progdir-varcl
        WHERE name = ls_progdir_new-name
        AND state = ls_progdir_new-state.                 "#EC CI_SUBRC
    ENDIF.

    zcl_abapgit_objects_activation=>add(
      iv_type = 'REPS'
      iv_name = is_progdir-name ).

  ENDMETHOD.


  METHOD deserialize_textpool.

    DATA lv_language TYPE langu.
    DATA lv_state    TYPE c.
    DATA lv_delete   TYPE abap_bool.

    IF iv_language IS INITIAL.
      lv_language = mv_language.
    ELSE.
      lv_language = iv_language.
    ENDIF.

    IF lv_language = mv_language.
      lv_state = 'I'. "Textpool in main language needs to be activated
    ELSE.
      lv_state = 'A'. "Translations are always active
    ENDIF.

    IF it_tpool IS INITIAL.
      IF iv_is_include = abap_false OR lv_state = 'A'.
        DELETE TEXTPOOL iv_program "Remove initial description from textpool if
          LANGUAGE lv_language     "original program does not have a textpool
          STATE lv_state.

        lv_delete = abap_true.
      ELSE.
        INSERT TEXTPOOL iv_program "In case of includes: Deletion of textpool in
          FROM it_tpool            "main language cannot be activated because
          LANGUAGE lv_language     "this woul activate the deletion of the textpool
          STATE lv_state.          "of the mail program -> insert empty textpool
      ENDIF.
    ELSE.
      IF lines( it_tpool ) = 1 AND lv_language = mv_language.
        READ TABLE it_tpool WITH KEY id = 'R' TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          RETURN. "No action because description in main language is already there
        ENDIF.
      ENDIF.

      INSERT TEXTPOOL iv_program
        FROM it_tpool
        LANGUAGE lv_language
        STATE lv_state.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'error from INSERT TEXTPOOL' ).
      ENDIF.
    ENDIF.

    IF lv_state = 'I'. "Textpool in main language needs to be activated
      zcl_abapgit_objects_activation=>add(
        iv_type   = 'REPT'
        iv_name   = iv_program
        iv_delete = lv_delete ).
    ENDIF.
  ENDMETHOD.


  METHOD is_any_dynpro_locked.

    DATA: lt_dynpros TYPE ty_dynpro_tt,
          lv_object  TYPE seqg3-garg.

    FIELD-SYMBOLS: <ls_dynpro> TYPE ty_dynpro.

    lt_dynpros = serialize_dynpros( iv_program ).

    LOOP AT lt_dynpros ASSIGNING <ls_dynpro>.

      lv_object = |{ <ls_dynpro>-header-screen }{ <ls_dynpro>-header-program }|.

      IF exists_a_lock_entry_for( iv_lock_object = 'ESCRP'
                                  iv_argument    = lv_object ) = abap_true.
        rv_is_any_dynpro_locked = abap_true.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD is_cua_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = |CU{ iv_program }|.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_cua_locked = exists_a_lock_entry_for( iv_lock_object = 'ESCUAPAINT'
                                                iv_argument    = lv_object ).

  ENDMETHOD.


  METHOD is_text_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = |*{ iv_program }|.

    rv_is_text_locked = exists_a_lock_entry_for( iv_lock_object = 'EABAPTEXTE'
                                                 iv_argument    = lv_object ).

  ENDMETHOD.


  METHOD read_progdir.

    DATA: ls_sapdir TYPE progdir.


    CALL FUNCTION 'READ_PROGDIR'
      EXPORTING
        i_progname = iv_program
        i_state    = 'A'
      IMPORTING
        e_progdir  = ls_sapdir.
    MOVE-CORRESPONDING ls_sapdir TO rs_progdir.

    CLEAR: rs_progdir-edtx,
           rs_progdir-cnam,
           rs_progdir-cdat,
           rs_progdir-unam,
           rs_progdir-udat,
           rs_progdir-levl,
           rs_progdir-vern,
           rs_progdir-rmand,
           rs_progdir-sdate,
           rs_progdir-stime,
           rs_progdir-idate,
           rs_progdir-itime,
           rs_progdir-varcl,
           rs_progdir-state.

  ENDMETHOD.


  METHOD read_tpool.

    FIELD-SYMBOLS: <ls_tpool_in>  LIKE LINE OF it_tpool,
                   <ls_tpool_out> LIKE LINE OF rt_tpool.


    LOOP AT it_tpool ASSIGNING <ls_tpool_in>.
      APPEND INITIAL LINE TO rt_tpool ASSIGNING <ls_tpool_out>.
      MOVE-CORRESPONDING <ls_tpool_in> TO <ls_tpool_out>.
      IF <ls_tpool_out>-id = 'S'.
        CONCATENATE <ls_tpool_in>-split <ls_tpool_in>-entry
          INTO <ls_tpool_out>-entry
          RESPECTING BLANKS.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_cua.

    CALL FUNCTION 'RS_CUA_INTERNAL_FETCH'
      EXPORTING
        program         = iv_program_name
        language        = mv_language
        state           = 'A'
      IMPORTING
        adm             = rs_cua-adm
      TABLES
        sta             = rs_cua-sta
        fun             = rs_cua-fun
        men             = rs_cua-men
        mtx             = rs_cua-mtx
        act             = rs_cua-act
        but             = rs_cua-but
        pfk             = rs_cua-pfk
        set             = rs_cua-set
        doc             = rs_cua-doc
        tit             = rs_cua-tit
        biv             = rs_cua-biv
      EXCEPTIONS
        not_found       = 1
        unknown_version = 2
        OTHERS          = 3.
    IF sy-subrc > 1.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize_dynpros.
    DATA: ls_header               TYPE rpy_dyhead,
          lt_containers           TYPE dycatt_tab,
          lt_fields_to_containers TYPE dyfatc_tab,
          lt_flow_logic           TYPE swydyflow,
          lt_d020s                TYPE TABLE OF d020s,
          lt_fieldlist_int        TYPE TABLE OF d021s. "internal format

    FIELD-SYMBOLS: <ls_d020s>       LIKE LINE OF lt_d020s,
                   <lv_outputstyle> TYPE scrpostyle,
                   <ls_container>   LIKE LINE OF lt_containers,
                   <ls_field>       LIKE LINE OF lt_fields_to_containers,
                   <ls_dynpro>      LIKE LINE OF rt_dynpro,
                   <ls_field_int>   LIKE LINE OF lt_fieldlist_int.

    "#2746: relevant flag values (taken from include MSEUSBIT)
    CONSTANTS: lc_flg1ddf TYPE x VALUE '20',
               lc_flg3fku TYPE x VALUE '08',
               lc_flg3for TYPE x VALUE '04',
               lc_flg3fdu TYPE x VALUE '02'.


    CALL FUNCTION 'RS_SCREEN_LIST'
      EXPORTING
        dynnr     = ''
        progname  = iv_program_name
      TABLES
        dynpros   = lt_d020s
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc = 2.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    SORT lt_d020s BY dnum ASCENDING.

* loop dynpros and skip generated selection screens
    LOOP AT lt_d020s ASSIGNING <ls_d020s>
        WHERE type <> 'S' AND type <> 'W' AND type <> 'J'
        AND NOT dnum IS INITIAL.

      CALL FUNCTION 'RPY_DYNPRO_READ'
        EXPORTING
          progname             = iv_program_name
          dynnr                = <ls_d020s>-dnum
        IMPORTING
          header               = ls_header
        TABLES
          containers           = lt_containers
          fields_to_containers = lt_fields_to_containers
          flow_logic           = lt_flow_logic
        EXCEPTIONS
          cancelled            = 1
          not_found            = 2
          permission_error     = 3
          OTHERS               = 4.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      "#2746: we need the dynpro fields in internal format:
      FREE lt_fieldlist_int.

      CALL FUNCTION 'RPY_DYNPRO_READ_NATIVE'
        EXPORTING
          progname  = iv_program_name
          dynnr     = <ls_d020s>-dnum
        TABLES
          fieldlist = lt_fieldlist_int.


      LOOP AT lt_fields_to_containers ASSIGNING <ls_field>.
* output style is a NUMC field, the XML conversion will fail if it contains invalid value
* field does not exist in all versions
        ASSIGN COMPONENT 'OUTPUTSTYLE' OF STRUCTURE <ls_field> TO <lv_outputstyle>.
        IF sy-subrc = 0 AND <lv_outputstyle> = '  '.
          CLEAR <lv_outputstyle>.
        ENDIF.

        "2746: we apply the same logic as in SAPLWBSCREEN
        "for setting or unsetting the foreignkey field:
        UNASSIGN <ls_field_int>.
        READ TABLE lt_fieldlist_int ASSIGNING <ls_field_int> WITH KEY fnam = <ls_field>-name.
        IF <ls_field_int> IS ASSIGNED.
          IF <ls_field_int>-flg1 O lc_flg1ddf AND
              <ls_field_int>-flg3 O lc_flg3for AND
              <ls_field_int>-flg3 Z lc_flg3fdu AND
              <ls_field_int>-flg3 Z lc_flg3fku.
            <ls_field>-foreignkey = 'X'.
          ELSE.
            CLEAR <ls_field>-foreignkey.
          ENDIF.
        ENDIF.
      ENDLOOP.

      LOOP AT lt_containers ASSIGNING <ls_container>.
        IF <ls_container>-c_resize_v = abap_false.
          CLEAR <ls_container>-c_line_min.
        ENDIF.
        IF <ls_container>-c_resize_h = abap_false.
          CLEAR <ls_container>-c_coln_min.
        ENDIF.
      ENDLOOP.

      APPEND INITIAL LINE TO rt_dynpro ASSIGNING <ls_dynpro>.
      <ls_dynpro>-header     = ls_header.
      <ls_dynpro>-containers = lt_containers.
      <ls_dynpro>-fields     = lt_fields_to_containers.

      <ls_dynpro>-flow_logic = lt_flow_logic.

    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_program.

    DATA: ls_progdir      TYPE ty_progdir,
          lv_program_name TYPE programm,
          lt_dynpros      TYPE ty_dynpro_tt,
          ls_cua          TYPE ty_cua,
          lt_source       TYPE TABLE OF abaptxt255,
          lt_tpool        TYPE textpool_table,
          ls_tpool        LIKE LINE OF lt_tpool,
          li_xml          TYPE REF TO zif_abapgit_xml_output.

    IF iv_program IS INITIAL.
      lv_program_name = is_item-obj_name.
    ELSE.
      lv_program_name = iv_program.
    ENDIF.

    zcl_abapgit_language=>set_current_language( mv_language ).

    CALL FUNCTION 'RPY_PROGRAM_READ'
      EXPORTING
        program_name     = lv_program_name
        with_includelist = abap_false
        with_lowercase   = abap_true
      TABLES
        source_extended  = lt_source
        textelements     = lt_tpool
      EXCEPTIONS
        cancelled        = 1
        not_found        = 2
        permission_error = 3
        OTHERS           = 4.

    IF sy-subrc = 2.
      zcl_abapgit_language=>restore_login_language( ).
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcl_abapgit_language=>restore_login_language( ).
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    zcl_abapgit_language=>restore_login_language( ).

    ls_progdir = read_progdir( lv_program_name ).

    IF io_xml IS BOUND.
      li_xml = io_xml.
    ELSE.
      CREATE OBJECT li_xml TYPE zcl_abapgit_xml_output.
    ENDIF.

    li_xml->add( iv_name = 'PROGDIR'
                 ig_data = ls_progdir ).
    IF ls_progdir-subc = '1' OR ls_progdir-subc = 'M'.
      lt_dynpros = serialize_dynpros( lv_program_name ).
      li_xml->add( iv_name = 'DYNPROS'
                   ig_data = lt_dynpros ).

      ls_cua = serialize_cua( lv_program_name ).
      IF NOT ls_cua IS INITIAL.
        li_xml->add( iv_name = 'CUA'
                     ig_data = ls_cua ).
      ENDIF.
    ENDIF.

    READ TABLE lt_tpool WITH KEY id = 'R' INTO ls_tpool.
    IF sy-subrc = 0 AND ls_tpool-key = '' AND ls_tpool-length = 0.
      DELETE lt_tpool INDEX sy-tabix.
    ENDIF.

    li_xml->add( iv_name = 'TPOOL'
                 ig_data = add_tpool( lt_tpool ) ).

    IF NOT io_xml IS BOUND.
      io_files->add_xml( iv_extra = iv_extra
                         ii_xml   = li_xml ).
    ENDIF.

    io_files->add_abap( iv_extra = iv_extra
                        it_abap  = lt_source ).

  ENDMETHOD.


  METHOD uncondense_flow.

    DATA: lv_spaces LIKE LINE OF it_spaces.

    FIELD-SYMBOLS: <ls_flow>   LIKE LINE OF it_flow,
                   <ls_output> LIKE LINE OF rt_flow.


    LOOP AT it_flow ASSIGNING <ls_flow>.
      APPEND INITIAL LINE TO rt_flow ASSIGNING <ls_output>.
      <ls_output>-line = <ls_flow>-line.

      READ TABLE it_spaces INDEX sy-tabix INTO lv_spaces.
      IF sy-subrc = 0.
        SHIFT <ls_output>-line RIGHT BY lv_spaces PLACES IN CHARACTER MODE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_acid IMPLEMENTATION.


  METHOD create_object.

    DATA: lv_name TYPE aab_id_name.


    lv_name = ms_item-obj_name.

    CREATE OBJECT ro_aab
      EXPORTING
        im_name          = lv_name
      EXCEPTIONS
        name_not_allowed = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
* looks like "changed by user" is not stored in the database
    rv_user = c_user_unknown.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lo_aab TYPE REF TO cl_aab_id.


    lo_aab = create_object( ).
    lo_aab->enqueue(
      EXCEPTIONS
        foreign_lock = 1
        system_error = 2
        cts_error    = 3
        OTHERS       = 4 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
    lo_aab->delete(
      EXCEPTIONS
        prop_error       = 1
        propt_error      = 2
        act_error        = 3
        cts_error        = 4
        cts_devclass     = 5
        id_not_found     = 6
        no_authorization = 7
        id_still_used    = 8
        where_used_error = 9
        OTHERS           = 10 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
    lo_aab->dequeue( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_description TYPE aab_id_descript,
          lo_aab         TYPE REF TO cl_aab_id.


    io_xml->read( EXPORTING iv_name = 'DESCRIPTION'
                  CHANGING  cg_data = lv_description ).

    lo_aab = create_object( ).

    lo_aab->enqueue(
      EXCEPTIONS
        foreign_lock = 1
        system_error = 2
        cts_error    = 3
        OTHERS       = 4 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lo_aab->set_descript(
      EXPORTING
        im_descript      = lv_description
      EXCEPTIONS
        no_authorization = 1
        OTHERS           = 2 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    tadir_insert( iv_package ).

    lo_aab->save(
      EXCEPTIONS
        no_descript_specified = 1
        no_changes_found      = 2
        prop_error            = 3
        propt_error           = 4
        act_error             = 5
        cts_error             = 6
        sync_attributes_error = 7
        action_canceled       = 8
        OTHERS                = 9 ).
    IF sy-subrc >= 3.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lo_aab->dequeue( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_state TYPE abap_bool,
          lo_aab   TYPE REF TO cl_aab_id.


    lo_aab = create_object( ).

    lo_aab->get_state( IMPORTING ex_state = lv_state ).
    rv_bool = boolc( lv_state = abap_true ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'ACID'
        in_new_window = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lo_aab         TYPE REF TO cl_aab_id,
          lv_description TYPE aab_id_descript.


    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_aab = create_object( ).

    lo_aab->get_descript(
      IMPORTING ex_descript = lv_description
      EXCEPTIONS no_description_found = 1 ).

    io_xml->add( iv_name = 'DESCRIPTION'
                 ig_data = lv_description ).

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_AVAR IMPLEMENTATION.


  METHOD create_object.

    DATA: lv_name TYPE aab_var_name.

    lv_name = ms_item-obj_name.

    CREATE OBJECT ro_aab_var
      EXPORTING
        im_name          = lv_name
        im_local         = ''
      EXCEPTIONS
        name_not_allowed = 1
        user_not_valid   = 2
        no_authorization = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lo_aab TYPE REF TO cl_aab_variant.

    lo_aab = create_object( ).
    lo_aab->get_author( IMPORTING ex_author = rv_user ).

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lo_aab TYPE REF TO cl_aab_variant.

    lo_aab = create_object( ).
    lo_aab->enqueue( ).
    lo_aab->delete(
      EXCEPTIONS
        var_not_found    = 1
        prop_error       = 2
        propt_error      = 3
        var_id_error     = 4
        no_authorization = 5
        cts_error        = 6
        cts_devclass     = 7
        OTHERS           = 8 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error deleting AVAR { ms_item-obj_name }| ).
    ENDIF.
    lo_aab->dequeue( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_possible    TYPE abap_bool,
          lv_description TYPE aab_var_descript,
          ls_is          TYPE aab_var_obj_act,
          lt_ids         TYPE aab_var_obj_act_tab,
          lo_aab         TYPE REF TO cl_aab_variant.

    " AVAR can only be created in transportable packages
    lv_possible = zcl_abapinst_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ).
    IF lv_possible = abap_false.
      zcx_abapgit_exception=>raise( |Global activation variants require a transportable package| ).
    ENDIF.

    " Create AVAR with description and object (id) list
    io_xml->read( EXPORTING iv_name = 'DESCRIPTION'
                  CHANGING  cg_data = lv_description ).

    io_xml->read( EXPORTING iv_name = 'IDS'
                  CHANGING  cg_data = lt_ids ).

    lo_aab = create_object( ).
    lo_aab->enqueue( ).
    lo_aab->set_descript(
      EXPORTING
        im_descript      = lv_description
      EXCEPTIONS
        no_authorization = 1 ).
    IF sy-subrc <> 0.
      lo_aab->dequeue( ).
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_ids INTO ls_is.
      lo_aab->set_id(
        EXPORTING
          im_name              = ls_is-name
          im_object            = ls_is-object
          im_actmode           = ls_is-actmode
        EXCEPTIONS
          no_authorization     = 1
          id_not_exists        = 2
          id_not_transportable = 3
          OTHERS               = 4 ).
      IF sy-subrc <> 0.
        lo_aab->dequeue( ).
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.

    tadir_insert( iv_package ).

    lo_aab->save(
      EXCEPTIONS
        no_descript_specified = 1
        prop_error            = 2
        propt_error           = 3
        var_id_error          = 4
        no_changes_found      = 5
        cts_error             = 6 ).
    IF sy-subrc <> 0.
      lo_aab->dequeue( ).
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
    lo_aab->dequeue( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_state TYPE abap_bool,
          lo_aab   TYPE REF TO cl_aab_variant.

    lo_aab = create_object( ).

    lo_aab->get_state( IMPORTING ex_state = lv_state ).
    rv_bool = boolc( lv_state = abap_true ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    zcx_abapgit_exception=>raise( |Jump to AVAR is not supported| ).
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lo_aab         TYPE REF TO cl_aab_variant,
          lt_ids         TYPE aab_var_obj_act_tab,
          lv_description TYPE aab_var_descript.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_aab = create_object( ).

    lo_aab->get_descript(
      IMPORTING
        ex_descript = lv_description
      EXCEPTIONS
        no_descript_found = 1 ).
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'DESCRIPTION'
                   ig_data = lv_description ).
    ENDIF.

    lo_aab->get_ids( IMPORTING ex_ids = lt_ids ).

    io_xml->add( iv_name = 'IDS'
                 ig_data = lt_ids ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_oo_base IMPLEMENTATION.


  METHOD convert_attrib_to_vseoattrib.
    FIELD-SYMBOLS: <ls_attribute>  LIKE LINE OF it_attributes,
                   <ls_vseoattrib> LIKE LINE OF rt_vseoattrib.

    LOOP AT it_attributes ASSIGNING <ls_attribute>.
      INSERT INITIAL LINE INTO TABLE rt_vseoattrib ASSIGNING <ls_vseoattrib>.
      MOVE-CORRESPONDING <ls_attribute> TO <ls_vseoattrib>.
      <ls_vseoattrib>-clsname = iv_clsname.
      UNASSIGN <ls_vseoattrib>.
    ENDLOOP.
    UNASSIGN <ls_attribute>.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~add_to_activation_list.
    zcl_abapgit_objects_activation=>add_item( is_item ).
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~create.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~create_documentation.
    CALL FUNCTION 'DOCU_UPD'
      EXPORTING
        id            = 'CL'
        langu         = iv_language
        object        = iv_object_name
        no_masterlang = iv_no_masterlang
      TABLES
        line          = it_lines
      EXCEPTIONS
        ret_code      = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~create_sotr.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~delete.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~deserialize_source.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~exists.
    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = is_object_name
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.
    rv_exists = boolc( sy-subrc <> 2 ).
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~generate_locals.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_class_properties.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_includes.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_interface_properties.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_skip_test_classes.
    rv_skip = mv_skip_test_classes.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~insert_text_pool.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~read_attributes.
    SELECT cmpname attbusobj attkeyfld
      FROM seocompodf
      INTO CORRESPONDING FIELDS OF TABLE rt_attributes
      WHERE clsname = iv_object_name
        AND ( attbusobj <> space OR attkeyfld <> space )
        AND version = '1'
      ORDER BY PRIMARY KEY.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~read_descriptions.
    IF iv_language IS INITIAL.
      " load all languages
      SELECT * FROM seocompotx INTO TABLE rt_descriptions
             WHERE clsname   = iv_obejct_name
               AND descript <> ''
             ORDER BY PRIMARY KEY.                        "#EC CI_SUBRC
    ELSE.
      " load main language
      SELECT * FROM seocompotx INTO TABLE rt_descriptions
              WHERE clsname   = iv_obejct_name
                AND langu = iv_language
                AND descript <> ''
              ORDER BY PRIMARY KEY.                       "#EC CI_SUBRC
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~read_documentation.
    DATA: lv_state  TYPE dokstate,
          lv_object TYPE dokhl-object,
          lt_lines  TYPE tlinetab.

    lv_object = iv_class_name.

    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id                = 'CL'
        langu             = iv_language
        object            = lv_object
      IMPORTING
        dokstate          = lv_state
      TABLES
        line              = lt_lines
      EXCEPTIONS
        no_docu_on_screen = 1
        no_docu_self_def  = 2
        no_docu_temp      = 3
        ret_code          = 4
        OTHERS            = 5.
    IF sy-subrc = 0 AND lv_state = 'R'.
      rt_lines = lt_lines.
    ELSE.
      CLEAR rt_lines.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~read_sotr.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~read_superclass.
    SELECT SINGLE refclsname FROM vseoextend INTO rv_superclass
      WHERE clsname = iv_classname.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~read_text_pool.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~serialize_abap.
    DATA lo_oo_serializer TYPE REF TO zcl_abapgit_oo_serializer.
    CREATE OBJECT lo_oo_serializer.
    CASE iv_type.
      WHEN seop_ext_class_locals_def.
        rt_source = lo_oo_serializer->serialize_locals_def( is_class_key ).
      WHEN seop_ext_class_locals_imp.
        rt_source = lo_oo_serializer->serialize_locals_imp( is_class_key ).
      WHEN seop_ext_class_macros.
        rt_source = lo_oo_serializer->serialize_macros( is_class_key ).
      WHEN seop_ext_class_testclasses.
        rt_source = lo_oo_serializer->serialize_testclasses( is_class_key ).
        mv_skip_test_classes = lo_oo_serializer->are_test_classes_skipped( ).
      WHEN OTHERS.
        rt_source = lo_oo_serializer->serialize_abap_clif_source( is_class_key ).
    ENDCASE.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~update_descriptions.
    DELETE FROM seocompotx WHERE clsname = is_key-clsname. "#EC CI_SUBRC
    INSERT seocompotx FROM TABLE it_descriptions.         "#EC CI_SUBRC
  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_oo_class IMPLEMENTATION.


  METHOD create_report.
    INSERT REPORT iv_program FROM it_source EXTENSION TYPE iv_extension STATE iv_version PROGRAM TYPE iv_program_type.
    ASSERT sy-subrc = 0.
  ENDMETHOD.


  METHOD delete_report.
    DELETE REPORT iv_program ##SUBRC_OK.
  ENDMETHOD.


  METHOD determine_method_include.

    DATA: ls_mtdkey TYPE seocpdkey.


    ls_mtdkey-clsname = iv_name.
    ls_mtdkey-cpdname = iv_method.

    cl_oo_classname_service=>get_method_include(
      EXPORTING
        mtdkey              = ls_mtdkey
      RECEIVING
        result              = rv_program
      EXCEPTIONS
        method_not_existing = 1 ).
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_METHOD_GENERATE_INCLUDE'
      EXPORTING
        suppress_mtdkey_check          = abap_true
        mtdkey                         = ls_mtdkey
      EXCEPTIONS
        not_existing                   = 1
        model_only                     = 2
        include_existing               = 3
        method_imp_not_generated       = 4
        method_imp_not_initialised     = 5
        _internal_class_not_existing   = 6
        _internal_method_overflow      = 7
        cancelled                      = 8
        method_is_abstract_implemented = 9
        method_is_final_implemented    = 10
        internal_error_insert_report   = 11
        OTHERS                         = 12.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    rv_program = cl_oo_classname_service=>get_method_include( ls_mtdkey ).

  ENDMETHOD.


  METHOD generate_classpool.

    DATA: ls_clskey TYPE seoclskey.

    ls_clskey-clsname = iv_name.

    CALL FUNCTION 'SEO_CLASS_GENERATE_CLASSPOOL'
      EXPORTING
        clskey                        = ls_clskey
        suppress_corr                 = abap_true
      EXCEPTIONS
        not_existing                  = 1
        model_only                    = 2
        class_pool_not_generated      = 3
        class_stment_not_generated    = 4
        locals_not_generated          = 5
        macros_not_generated          = 6
        public_sec_not_generated      = 7
        protected_sec_not_generated   = 8
        private_sec_not_generated     = 9
        typeref_not_generated         = 10
        class_pool_not_initialised    = 11
        class_stment_not_initialised  = 12
        locals_not_initialised        = 13
        macros_not_initialised        = 14
        public_sec_not_initialised    = 15
        protected_sec_not_initialised = 16
        private_sec_not_initialised   = 17
        typeref_not_initialised       = 18
        _internal_class_overflow      = 19
        OTHERS                        = 20.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD init_scanner.

    DATA: lx_exc       TYPE REF TO cx_root,
          lv_message   TYPE string,
          lv_classname TYPE abap_abstypename.
    FIELD-SYMBOLS: <lv_line> TYPE i.

    TRY.
        ro_scanner = cl_oo_source_scanner_class=>create_class_scanner(
          clif_name = iv_name
          source    = it_source ).
        ro_scanner->scan( ).
      CATCH cx_clif_scan_error.
        zcx_abapgit_exception=>raise( 'error initializing CLAS scanner' ).
      CATCH cx_root INTO lx_exc.
        lv_classname = cl_abap_classdescr=>get_class_name( lx_exc ).
        IF lv_classname = '\CLASS=CX_OO_CLIF_SCAN_ERROR_DETAIL'.
          ASSIGN lx_exc->('SOURCE_POSITION-LINE') TO <lv_line>.
          ASSERT sy-subrc = 0.
          lv_message = |{ lx_exc->get_text( ) }, line { <lv_line> }|.
        ELSE.
          lv_message = lx_exc->get_text( ).
        ENDIF.
        zcx_abapgit_exception=>raise( lv_message ).
    ENDTRY.

  ENDMETHOD.


  METHOD update_cs_number_of_methods.

    " Indirect access to keep downward compatibility
    DATA lr_cache_entry TYPE REF TO data.

    FIELD-SYMBOLS: <lg_cache_entry> TYPE any,
                   <lg_field>       TYPE any.


    TRY.
        CREATE DATA lr_cache_entry TYPE ('SEO_CS_CACHE').
      CATCH cx_sy_create_data_error.
* does not exist in some older systems
        RETURN.
    ENDTRY.

    ASSIGN lr_cache_entry->* TO <lg_cache_entry>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'CLSNAME' OF STRUCTURE <lg_cache_entry>
           TO <lg_field>.
    ASSERT sy-subrc = 0.
    <lg_field> = iv_classname.

    ASSIGN COMPONENT 'NO_OF_METHOD_IMPLS' OF STRUCTURE <lg_cache_entry>
           TO <lg_field>.
    ASSERT sy-subrc = 0.
    <lg_field> = iv_number_of_impl_methods.

    MODIFY ('SEO_CS_CACHE') FROM <lg_cache_entry>.

  ENDMETHOD.


  METHOD update_full_class_include.

    CONSTANTS: lc_class_source_extension TYPE c LENGTH 2 VALUE 'CS',
               lc_include_program_type   TYPE c LENGTH 1 VALUE 'I',
               lc_active_version         TYPE r3state VALUE 'A'.


    create_report( iv_program      = cl_oo_classname_service=>get_cs_name( iv_classname )
                   it_source       = it_source
                   iv_extension    = lc_class_source_extension
                   iv_program_type = lc_include_program_type
                   iv_version      = lc_active_version ).

    " Assuming that all methods that were scanned are implemented
    update_cs_number_of_methods( iv_classname              = iv_classname
                                 iv_number_of_impl_methods = lines( it_methods ) ).

  ENDMETHOD.


  METHOD update_meta.

    DATA: lo_update     TYPE REF TO cl_oo_class_section_source,
          ls_clskey     TYPE seoclskey,
          lv_scan_error TYPE abap_bool.


    ls_clskey-clsname = iv_name.

    TRY.
        CALL FUNCTION 'SEO_BUFFER_REFRESH'
          EXPORTING
            cifkey  = ls_clskey
            version = seoc_version_active.
        CREATE OBJECT lo_update TYPE ('CL_OO_CLASS_SECTION_SOURCE')
          EXPORTING
            clskey                        = ls_clskey
            exposure                      = iv_exposure
            state                         = 'A'
            source                        = it_source
            suppress_constrctr_generation = abap_true
          EXCEPTIONS
            class_not_existing            = 1
            read_source_error             = 2
            OTHERS                        = 3 ##SUBRC_OK.
      CATCH cx_sy_dyn_call_param_not_found.
* downport to 702, see https://github.com/abapGit/abapGit/issues/933
* this will READ REPORT instead of using it_source, which should be okay
        CREATE OBJECT lo_update TYPE cl_oo_class_section_source
          EXPORTING
            clskey             = ls_clskey
            exposure           = iv_exposure
            state              = 'A'
          EXCEPTIONS
            class_not_existing = 1
            read_source_error  = 2
            OTHERS             = 3.
    ENDTRY.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lo_update->set_dark_mode( abap_true ).
    TRY.
        CALL METHOD lo_update->('SET_AMDP_SUPPORT')
          EXPORTING
            enabled = abap_true.
      CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
* AMDP not supported in this system, ignore error
    ENDTRY.
    lo_update->scan_section_source(
      RECEIVING
        scan_error             = lv_scan_error
      EXCEPTIONS
        scan_abap_source_error = 1
        OTHERS                 = 2 ).
    IF sy-subrc <> 0 OR lv_scan_error = abap_true.
      zcx_abapgit_exception=>raise( |CLAS, error while scanning source. Subrc = { sy-subrc }| ).
    ENDIF.

* this will update the SEO* database tables
    lo_update->revert_scan_result( ).

    IF iv_exposure = seoc_exposure_public.
      generate_classpool( iv_name ).
    ENDIF.

  ENDMETHOD.


  METHOD update_report.

    DATA: lt_old TYPE string_table.

    READ REPORT iv_program INTO lt_old.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Fatal error. Include { iv_program } should have been created previously!| ).
    ENDIF.

    IF lt_old <> it_source.
      INSERT REPORT iv_program FROM it_source.
      ASSERT sy-subrc = 0.
      rv_updated = abap_true.
    ELSE.
      rv_updated = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD update_source_index.

    CONSTANTS:
      lc_version_active   TYPE r3state VALUE 'A',
      lc_version_inactive TYPE r3state VALUE 'I'.

    "    dynamic invocation, IF_OO_SOURCE_POS_INDEX_HELPER doesn't exist in 702.
    DATA lo_index_helper TYPE REF TO object.

    TRY.
        CREATE OBJECT lo_index_helper TYPE ('CL_OO_SOURCE_POS_INDEX_HELPER').

        CALL METHOD lo_index_helper->('IF_OO_SOURCE_POS_INDEX_HELPER~CREATE_INDEX_WITH_SCANNER')
          EXPORTING
            class_name = iv_clsname
            version    = lc_version_active
            scanner    = io_scanner.

        CALL METHOD lo_index_helper->('IF_OO_SOURCE_POS_INDEX_HELPER~DELETE_INDEX')
          EXPORTING
            class_name = iv_clsname
            version    = lc_version_inactive.

      CATCH cx_root.
        " it's probably okay to no update the index
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~create.

    DATA: lt_vseoattrib TYPE seoo_attributes_r.
    FIELD-SYMBOLS: <lv_clsname> TYPE seoclsname.

* same as in super class, but with "version = seoc_version_active"

    ASSIGN COMPONENT 'CLSNAME' OF STRUCTURE cg_properties TO <lv_clsname>.
    ASSERT sy-subrc = 0.

    lt_vseoattrib = convert_attrib_to_vseoattrib(
                      iv_clsname    = <lv_clsname>
                      it_attributes = it_attributes ).

    TRY.
        CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = iv_overwrite
            version         = seoc_version_active
            suppress_dialog = abap_true " Parameter missing in 702
          CHANGING
            class           = cg_properties
            attributes      = lt_vseoattrib
          EXCEPTIONS
            existing        = 1
            is_interface    = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
      CATCH cx_sy_dyn_call_param_not_found.
        CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = iv_overwrite
            version         = seoc_version_active
          CHANGING
            class           = cg_properties
            attributes      = lt_vseoattrib
          EXCEPTIONS
            existing        = 1
            is_interface    = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
    ENDTRY.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~create_sotr.
    zcl_abapgit_sotr_handler=>create_sotr(
      iv_package = iv_package
      io_xml     = ii_xml ).
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~delete.
    CALL FUNCTION 'SEO_CLASS_DELETE_COMPLETE'
      EXPORTING
        clskey       = is_deletion_key
      EXCEPTIONS
        not_existing = 1
        is_interface = 2
        db_error     = 3
        no_access    = 4
        other        = 5
        OTHERS       = 6.
    IF sy-subrc = 1.
* ignore deletion of objects that does not exist
* this can happen when the SXCI object is deleted before the implementing CLAS
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~deserialize_source.

    DATA: lv_updated TYPE abap_bool,
          lv_program TYPE program,
          lo_scanner TYPE REF TO cl_oo_source_scanner_class,
          lt_methods TYPE cl_oo_source_scanner_class=>type_method_implementations,
          lv_method  LIKE LINE OF lt_methods,
          lt_public  TYPE seop_source_string,
          lt_source  TYPE seop_source_string.

    "Buffer needs to be refreshed,
    "otherwise standard SAP CLIF_SOURCE reorder methods alphabetically
    CALL FUNCTION 'SEO_BUFFER_INIT'.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        cifkey  = is_key
        version = seoc_version_inactive.

    lo_scanner = init_scanner(
      it_source = it_source
      iv_name   = is_key-clsname ).

* public
    lt_public = lo_scanner->get_public_section_source( ).
    IF lt_public IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_pubsec_name( is_key-clsname ).
      lv_updated = update_report( iv_program = lv_program
                                  it_source  = lt_public ).
      IF lv_updated = abap_true.
        update_meta( iv_name     = is_key-clsname
                     iv_exposure = seoc_exposure_public
                     it_source   = lt_public ).
      ENDIF.
    ENDIF.

* protected
    lt_source = lo_scanner->get_protected_section_source( ).
    IF lt_source IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_prosec_name( is_key-clsname ).
      lv_updated = update_report( iv_program = lv_program
                                  it_source  = lt_source ).
      IF lv_updated = abap_true.
        update_meta( iv_name     = is_key-clsname
                     iv_exposure = seoc_exposure_protected
                     it_source   = lt_source ).
      ENDIF.
    ENDIF.

* private
    lt_source = lo_scanner->get_private_section_source( ).
    IF lt_source IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_prisec_name( is_key-clsname ).
      lv_updated = update_report( iv_program = lv_program
                                  it_source  = lt_source ).
      IF lv_updated = abap_true.
        update_meta( iv_name     = is_key-clsname
                     iv_exposure = seoc_exposure_private
                     it_source   = lt_source ).
      ENDIF.
    ENDIF.

* methods
    lt_methods = lo_scanner->get_method_implementations( ).

    LOOP AT lt_methods INTO lv_method.
      TRY.
          lt_source = lo_scanner->get_method_impl_source( lv_method ).
        CATCH cx_oo_clif_component.
          zcx_abapgit_exception=>raise( 'error from GET_METHOD_IMPL_SOURCE' ).
      ENDTRY.
      lv_program = determine_method_include(
        iv_name   = is_key-clsname
        iv_method = lv_method ).

      update_report(
        iv_program = lv_program
        it_source  = lt_source ).
    ENDLOOP.

* full class include
    update_full_class_include( iv_classname = is_key-clsname
                               it_source    = it_source
                               it_methods   = lt_methods ).

    update_source_index(
      iv_clsname = is_key-clsname
      io_scanner = lo_scanner ).

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~generate_locals.

    DATA: lv_program TYPE programm.

    IF lines( it_local_definitions ) > 0.
      lv_program = cl_oo_classname_service=>get_ccdef_name( is_key-clsname ).
      update_report( iv_program = lv_program
                     it_source  = it_local_definitions ).
    ENDIF.

    IF lines( it_local_implementations ) > 0.
      lv_program = cl_oo_classname_service=>get_ccimp_name( is_key-clsname ).
      update_report( iv_program = lv_program
                     it_source  = it_local_implementations ).
    ENDIF.

    IF lines( it_local_macros ) > 0.
      lv_program = cl_oo_classname_service=>get_ccmac_name( is_key-clsname ).
      update_report( iv_program = lv_program
                     it_source  = it_local_macros ).
    ENDIF.

    lv_program = cl_oo_classname_service=>get_ccau_name( is_key-clsname ).
    IF lines( it_local_test_classes ) > 0.
      update_report( iv_program = lv_program
                     it_source  = it_local_test_classes ).
    ELSE.
      " Drop the include to remove left-over test classes
      delete_report( lv_program ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_class_properties.
    CALL FUNCTION 'SEO_CLIF_GET'
      EXPORTING
        cifkey       = is_class_key
        version      = seoc_version_active
      IMPORTING
        class        = rs_class_properties
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc = 1.
      RETURN. " in case only inactive version exists
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_includes.
* note: includes returned might not exist
* method cl_oo_classname_service=>GET_ALL_CLASS_INCLUDES does not exist in 702

    DATA: lv_class_name TYPE seoclsname,
          lt_methods    TYPE seop_methods_w_include.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF lt_methods.

    lv_class_name = iv_object_name.

    APPEND cl_oo_classname_service=>get_ccdef_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_ccmac_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_ccimp_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_cl_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_ccau_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_pubsec_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_prosec_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_prisec_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_classpool_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_ct_name( lv_class_name ) TO rt_includes.

* skip the CS include, as it is sometimes generated on the fly instead of
* when the methods are changed

    cl_oo_classname_service=>get_all_method_includes(
      EXPORTING
        clsname            = lv_class_name
      RECEIVING
        result             = lt_methods
      EXCEPTIONS
        class_not_existing = 1 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Class { lv_class_name } not existing| ).
    ENDIF.

    LOOP AT lt_methods ASSIGNING <ls_method>.
      APPEND <ls_method>-incname TO rt_includes.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~insert_text_pool.
    DATA: lv_cp TYPE program.

    lv_cp = cl_oo_classname_service=>get_classpool_name( iv_class_name ).

    INSERT TEXTPOOL lv_cp
      FROM it_text_pool
      LANGUAGE iv_language
      STATE iv_state.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from INSERT TEXTPOOL' ).
    ENDIF.

    zcl_abapgit_objects_activation=>add( iv_type = 'REPT'
                                         iv_name = lv_cp ).
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~read_sotr.
    zcl_abapgit_sotr_handler=>read_sotr(
      iv_pgmid    = 'LIMU'
      iv_object   = 'CPUB'
      iv_obj_name = iv_object_name
      io_xml      = ii_xml ).
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~read_text_pool.
    DATA: lv_cp TYPE program.

    lv_cp = cl_oo_classname_service=>get_classpool_name( iv_class_name ).
    READ TEXTPOOL lv_cp INTO rt_text_pool LANGUAGE iv_language. "#EC CI_READ_REP
  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_clas IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    CREATE OBJECT mi_object_oriented_object_fct TYPE zcl_abapgit_oo_class.

    mv_classpool_name = cl_oo_classname_service=>get_classpool_name( |{ is_item-obj_name }| ).

  ENDMETHOD.


  METHOD deserialize_abap.

    DATA: ls_vseoclass             TYPE vseoclass,
          lt_source                TYPE seop_source_string,
          lt_local_definitions     TYPE seop_source_string,
          lt_local_implementations TYPE seop_source_string,
          lt_local_macros          TYPE seop_source_string,
          lt_test_classes          TYPE seop_source_string,
          lt_descriptions          TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
          ls_class_key             TYPE seoclskey,
          lt_attributes            TYPE zif_abapgit_definitions=>ty_obj_attribute_tt.


    lt_source = mo_files->read_abap( ).

    lt_local_definitions = mo_files->read_abap( iv_extra = zif_abapgit_oo_object_fnc=>c_parts-locals_def
                                                iv_error = abap_false ).

    lt_local_implementations = mo_files->read_abap( iv_extra = zif_abapgit_oo_object_fnc=>c_parts-locals_imp
                                                    iv_error = abap_false ).

    lt_local_macros = mo_files->read_abap( iv_extra = zif_abapgit_oo_object_fnc=>c_parts-macros
                                           iv_error = abap_false ).

    lt_test_classes = mo_files->read_abap( iv_extra = zif_abapgit_oo_object_fnc=>c_parts-testclasses
                                           iv_error = abap_false ).

    ls_class_key-clsname = ms_item-obj_name.

    ii_xml->read( EXPORTING iv_name = 'VSEOCLASS'
                  CHANGING  cg_data = ls_vseoclass ).

    ii_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING  cg_data = lt_attributes ).

    " Remove code for test classes if they have been deleted
    IF ls_vseoclass-with_unit_tests = abap_false.
      CLEAR lt_test_classes.
    ENDIF.

    mi_object_oriented_object_fct->create(
      EXPORTING
        iv_package    = iv_package
        it_attributes = lt_attributes
      CHANGING
        cg_properties = ls_vseoclass ).

    mi_object_oriented_object_fct->generate_locals(
      is_key                   = ls_class_key
      it_local_definitions     = lt_local_definitions
      it_local_implementations = lt_local_implementations
      it_local_macros          = lt_local_macros
      it_local_test_classes    = lt_test_classes ).

    repo_apack_replacement( CHANGING ct_source = lt_source ).
    mi_object_oriented_object_fct->deserialize_source(
      is_key    = ls_class_key
      it_source = lt_source ).

    ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS'
                  CHANGING cg_data = lt_descriptions ).

    mi_object_oriented_object_fct->update_descriptions(
      is_key          = ls_class_key
      it_descriptions = lt_descriptions ).

    mi_object_oriented_object_fct->add_to_activation_list( ms_item ).

  ENDMETHOD.


  METHOD deserialize_docu.

    DATA: lt_lines      TYPE tlinetab,
          lv_object     TYPE dokhl-object,
          lt_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_lines,
          ls_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_line.

    ii_xml->read( EXPORTING iv_name = 'LINES'
                  CHANGING cg_data = lt_lines ).

    IF lines( lt_lines ) = 0.
      RETURN.
    ENDIF.

    lv_object = ms_item-obj_name.

    mi_object_oriented_object_fct->create_documentation(
      it_lines       = lt_lines
      iv_object_name = lv_object
      iv_language    = mv_language ).

    ii_xml->read( EXPORTING iv_name = 'I18N_LINES'
                  CHANGING cg_data = lt_i18n_lines ).

    LOOP AT lt_i18n_lines INTO ls_i18n_lines.
      mi_object_oriented_object_fct->create_documentation(
        it_lines         = ls_i18n_lines-lines
        iv_object_name   = lv_object
        iv_language      = ls_i18n_lines-language
        iv_no_masterlang = abap_true ).
    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize_sotr.
    "OTR stands for Online Text Repository
    mi_object_oriented_object_fct->create_sotr(
      iv_object_name = ms_item-obj_name
      iv_package     = iv_package
      ii_xml         = ii_ml ).
  ENDMETHOD.


  METHOD deserialize_tpool.

    DATA: lv_clsname    TYPE seoclsname,
          lt_tpool_ext  TYPE zif_abapgit_definitions=>ty_tpool_tt,
          lt_tpool      TYPE textpool_table,
          lt_i18n_tpool TYPE zif_abapgit_lang_definitions=>ty_i18n_tpools,
          ls_i18n_tpool TYPE zif_abapgit_lang_definitions=>ty_i18n_tpool.


    ii_xml->read( EXPORTING iv_name = 'TPOOL'
                  CHANGING cg_data = lt_tpool_ext ).
    lt_tpool = read_tpool( lt_tpool_ext ).

    IF lines( lt_tpool ) = 0.
      RETURN.
    ENDIF.

    lv_clsname = ms_item-obj_name.

    mi_object_oriented_object_fct->insert_text_pool(
      iv_class_name = lv_clsname
      it_text_pool  = lt_tpool
      iv_language   = mv_language ).

    ii_xml->read( EXPORTING iv_name = 'I18N_TPOOL'
                  CHANGING  cg_data = lt_i18n_tpool ).

    LOOP AT lt_i18n_tpool INTO ls_i18n_tpool.
      lt_tpool = read_tpool( ls_i18n_tpool-textpool ).
      mi_object_oriented_object_fct->insert_text_pool(
        iv_class_name = lv_clsname
        it_text_pool  = lt_tpool
        iv_language   = ls_i18n_tpool-language
        iv_state      = 'A' ).
    ENDLOOP.

  ENDMETHOD.


  METHOD interface_replacement.

    DATA lv_tabix TYPE sy-tabix.

    FIELD-SYMBOLS <lv_source> LIKE LINE OF ct_source.

    FIND REGEX '^\s*INTERFACES(:| )\s*' && iv_from_interface && '\s*.' IN TABLE ct_source MATCH LINE lv_tabix.
    IF sy-subrc = 0.
      READ TABLE ct_source ASSIGNING <lv_source> INDEX lv_tabix.
      ASSERT sy-subrc = 0.

      REPLACE FIRST OCCURRENCE OF iv_from_interface IN <lv_source>
                             WITH iv_to_interface IGNORING CASE.

      REPLACE ALL OCCURRENCES OF iv_from_interface && '~descriptor' IN TABLE ct_source
                            WITH iv_to_interface && '~descriptor' IGNORING CASE.
    ENDIF.

  ENDMETHOD.


  METHOD is_class_locked.

    DATA: lv_argument TYPE seqg3-garg.

    lv_argument = ms_item-obj_name.
    OVERLAY lv_argument WITH '=============================='.
    lv_argument = lv_argument && '*'.

    rv_is_class_locked = exists_a_lock_entry_for( iv_lock_object = 'ESEOCLASS'
                                                  iv_argument    = lv_argument ).

  ENDMETHOD.


  METHOD repo_apack_replacement.

    DATA lv_apack TYPE seoclsname.

    " Check if SAP-version of APACK manifest exists
    SELECT SINGLE clsname INTO lv_apack
      FROM seoclass
      WHERE clsname = 'IF_APACK_MANIFEST'.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    " If not, replace with abapGit version
    interface_replacement(
      EXPORTING
        iv_from_interface = 'if_apack_manifest'
        iv_to_interface   = 'zif_apack_manifest'
      CHANGING
        ct_source         = ct_source ).

  ENDMETHOD.


  METHOD serialize_attr.

    DATA: lt_attributes TYPE zif_abapgit_definitions=>ty_obj_attribute_tt.

    lt_attributes = mi_object_oriented_object_fct->read_attributes( iv_clsname ).
    IF lines( lt_attributes ) = 0.
      RETURN.
    ENDIF.

    ii_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = lt_attributes ).

  ENDMETHOD.


  METHOD serialize_descr.

    DATA: lt_descriptions TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
          lv_language     TYPE spras.

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      lv_language = mv_language.
    ENDIF.

    lt_descriptions = mi_object_oriented_object_fct->read_descriptions(
      iv_obejct_name = iv_clsname
      iv_language = lv_language ).

    IF lines( lt_descriptions ) = 0.
      RETURN.
    ENDIF.

    ii_xml->add( iv_name = 'DESCRIPTIONS'
                 ig_data = lt_descriptions ).

  ENDMETHOD.


  METHOD serialize_docu.

    DATA: lt_lines      TYPE tlinetab,
          lv_langu      TYPE langu,
          lt_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_lines,
          ls_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_line.

    lt_lines = mi_object_oriented_object_fct->read_documentation(
      iv_class_name = iv_clsname
      iv_language   = mv_language ).
    IF lines( lt_lines ) > 0.
      ii_xml->add( iv_name = 'LINES'
                   ig_data = lt_lines ).
    ENDIF.

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      RETURN.
    ENDIF.

    LOOP AT it_langu_additional INTO lv_langu.
      CLEAR: ls_i18n_lines.

      lt_lines = mi_object_oriented_object_fct->read_documentation(
        iv_class_name = iv_clsname
        iv_language   = lv_langu ).

      ls_i18n_lines-language = lv_langu.
      ls_i18n_lines-lines    = lt_lines.
      INSERT ls_i18n_lines INTO TABLE lt_i18n_lines.

    ENDLOOP.

    IF lines( lt_i18n_lines ) > 0.
      ii_xml->add( iv_name = 'I18N_LINES'
                   ig_data = lt_i18n_lines ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize_sotr.
    mi_object_oriented_object_fct->read_sotr(
      iv_object_name = ms_item-obj_name
      ii_xml         = ii_xml ).
  ENDMETHOD.


  METHOD serialize_tpool.

    DATA: lt_tpool      TYPE textpool_table,
          lv_langu      TYPE langu,
          lt_i18n_tpool TYPE zif_abapgit_lang_definitions=>ty_i18n_tpools,
          ls_i18n_tpool TYPE zif_abapgit_lang_definitions=>ty_i18n_tpool.

    lt_tpool = mi_object_oriented_object_fct->read_text_pool(
      iv_class_name = iv_clsname
      iv_language   = mv_language ).
    ii_xml->add( iv_name = 'TPOOL'
                 ig_data = add_tpool( lt_tpool ) ).

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      RETURN.
    ENDIF.

    LOOP AT it_langu_additional INTO lv_langu.
      CLEAR: ls_i18n_tpool.

      lt_tpool = mi_object_oriented_object_fct->read_text_pool(
            iv_class_name = iv_clsname
            iv_language   = lv_langu ).

      ls_i18n_tpool-language = lv_langu.
      ls_i18n_tpool-textpool = add_tpool( lt_tpool ).
      INSERT ls_i18n_tpool INTO TABLE lt_i18n_tpool.

    ENDLOOP.

    IF lines( lt_i18n_tpool ) > 0.
      ii_xml->add( iv_name = 'I18N_TPOOL'
                   ig_data = lt_i18n_tpool ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize_xml.

    DATA: ls_vseoclass        TYPE vseoclass,
          ls_clskey           TYPE seoclskey,
          lt_langu_additional TYPE zif_abapgit_lang_definitions=>ty_langus.

    ls_clskey-clsname = ms_item-obj_name.

    "If class was deserialized with a previous versions of abapGit and current language was different
    "from main language at this time, this call would return SY-LANGU as main language. To fix
    "these objects, set SY-LANGU to main language temporarily.
    zcl_abapgit_language=>set_current_language( mv_language ).

    TRY.
        ls_vseoclass = mi_object_oriented_object_fct->get_class_properties( ls_clskey ).

      CLEANUP.
        zcl_abapgit_language=>restore_login_language( ).

    ENDTRY.

    zcl_abapgit_language=>restore_login_language( ).

    CLEAR: ls_vseoclass-uuid,
           ls_vseoclass-author,
           ls_vseoclass-createdon,
           ls_vseoclass-changedby,
           ls_vseoclass-changedon,
           ls_vseoclass-r3release,
           ls_vseoclass-chgdanyby,
           ls_vseoclass-chgdanyon,
           ls_vseoclass-clsfinal,
           ls_vseoclass-clsabstrct,
           ls_vseoclass-exposure,
           ls_vseoclass-version.

    IF mv_skip_testclass = abap_true.
      CLEAR ls_vseoclass-with_unit_tests.
    ENDIF.

    " Table d010tinf stores info. on languages in which program is maintained
    " Select all active translations of program texts
    " Skip main language - it was already serialized
    SELECT DISTINCT language
      INTO TABLE lt_langu_additional
      FROM d010tinf
      WHERE r3state  = 'A'
        AND prog     = mv_classpool_name
        AND language <> mv_language.

    ii_xml->add( iv_name = 'VSEOCLASS'
                 ig_data = ls_vseoclass ).

    serialize_tpool( ii_xml              = ii_xml
                     iv_clsname          = ls_clskey-clsname
                     it_langu_additional = lt_langu_additional ).

    IF ls_vseoclass-category = seoc_category_exception.
      serialize_sotr( ii_xml ).
    ENDIF.

    serialize_docu( ii_xml              = ii_xml
                    iv_clsname          = ls_clskey-clsname
                    it_langu_additional = lt_langu_additional ).

    serialize_descr( ii_xml     = ii_xml
                     iv_clsname = ls_clskey-clsname ).

    serialize_attr( ii_xml     = ii_xml
                    iv_clsname = ls_clskey-clsname ).

  ENDMETHOD.


  METHOD source_apack_replacement.

    DATA lv_clsname TYPE seoclsname.

    " Check if abapGit version of APACK manifest is used
    SELECT SINGLE clsname INTO lv_clsname
      FROM seometarel
      WHERE clsname    = ms_item-obj_name
        AND refclsname = 'ZIF_APACK_MANIFEST'
        AND version    = '1'.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " If yes, replace with SAP-version
    interface_replacement(
      EXPORTING
        iv_from_interface = 'zif_apack_manifest'
        iv_to_interface   = 'if_apack_manifest'
      CHANGING
        ct_source         = ct_source ).

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    TYPES: BEGIN OF ty_includes,
             programm TYPE programm,
           END OF ty_includes.

    TYPES: BEGIN OF ty_reposrc,
             unam  TYPE reposrc-unam,
             udat  TYPE reposrc-udat,
             utime TYPE reposrc-utime,
           END OF ty_reposrc.

    DATA: lt_reposrc  TYPE STANDARD TABLE OF ty_reposrc,
          ls_reposrc  LIKE LINE OF lt_reposrc,
          lt_includes TYPE STANDARD TABLE OF ty_includes.

    lt_includes = mi_object_oriented_object_fct->get_includes( ms_item-obj_name ).
    ASSERT lines( lt_includes ) > 0.

    SELECT unam udat utime FROM reposrc
      INTO TABLE lt_reposrc
      FOR ALL ENTRIES IN lt_includes
      WHERE progname = lt_includes-programm
      AND r3state = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ELSE.
      SORT lt_reposrc BY udat DESCENDING utime DESCENDING.
      READ TABLE lt_reposrc INDEX 1 INTO ls_reposrc.
      ASSERT sy-subrc = 0.
      rv_user = ls_reposrc-unam.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    DATA: ls_clskey TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    mi_object_oriented_object_fct->delete( ls_clskey ).
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_clskey TYPE seoclskey.

    deserialize_abap( ii_xml     = io_xml
                      iv_package = iv_package ).

    deserialize_tpool( io_xml ).

    deserialize_sotr( ii_ml     = io_xml
                      iv_package = iv_package ).

    deserialize_docu( io_xml ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.
    DATA: ls_class_key TYPE seoclskey.
    ls_class_key-clsname = ms_item-obj_name.

    rv_bool = mi_object_oriented_object_fct->exists( ls_class_key ).
  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    IF is_class_locked( ) = abap_true OR is_text_locked( mv_classpool_name ) = abap_true.
      rv_is_locked = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'CLAS'
        in_new_window = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lt_source    TYPE seop_source_string,
          ls_class_key TYPE seoclskey.

    ls_class_key-clsname = ms_item-obj_name.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_active
        force   = abap_true.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_inactive
        force   = abap_true.

    lt_source = mi_object_oriented_object_fct->serialize_abap( ls_class_key ).

    source_apack_replacement( CHANGING ct_source = lt_source ).

    mo_files->add_abap( lt_source ).

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_locals_def ).
    IF lines( lt_source ) > 0.
      mo_files->add_abap( iv_extra = zif_abapgit_oo_object_fnc=>c_parts-locals_def
                          it_abap  = lt_source ).
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_locals_imp ).
    IF lines( lt_source ) > 0.
      mo_files->add_abap( iv_extra = zif_abapgit_oo_object_fnc=>c_parts-locals_imp
                          it_abap  = lt_source ).
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key            = ls_class_key
      iv_type                 = seop_ext_class_testclasses ).

    mv_skip_testclass = mi_object_oriented_object_fct->get_skip_test_classes( ).
    IF lines( lt_source ) > 0 AND mv_skip_testclass = abap_false.
      mo_files->add_abap( iv_extra = zif_abapgit_oo_object_fnc=>c_parts-testclasses
                          it_abap  = lt_source ).
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_macros ).
    IF lines( lt_source ) > 0.
      mo_files->add_abap( iv_extra = zif_abapgit_oo_object_fnc=>c_parts-macros
                          it_abap  = lt_source ).
    ENDIF.

    serialize_xml( io_xml ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_devc IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_item     = is_item
                        iv_language = iv_language ).
    IF is_item-devclass IS NOT INITIAL.
      mv_local_devclass = is_item-devclass.
    ELSE.
      mv_local_devclass = is_item-obj_name.
    ENDIF.
  ENDMETHOD.


  METHOD get_package.
    IF zif_abapgit_object~exists( ) = abap_true.
      ri_package = load_package( mv_local_devclass ).
    ENDIF.
  ENDMETHOD.


  METHOD is_empty.

    DATA: lv_object_name TYPE tadir-obj_name,
          lt_subpackages TYPE zif_abapgit_sap_package=>ty_devclass_tt.

    lt_subpackages = zcl_abapinst_factory=>get_sap_package( iv_package_name )->list_subpackages( ).

    IF lines( lt_subpackages ) > 0.
      rv_is_empty = abap_false.
      RETURN.
    ENDIF.

    " Ignore the SOTR if is linked to the current SAP package (DEVC)
    SELECT SINGLE obj_name
           FROM tadir
           INTO lv_object_name
           WHERE pgmid = 'R3TR'
           AND NOT ( ( object = 'DEVC' OR object = 'SOTR' ) AND obj_name = iv_package_name )
           AND devclass = iv_package_name.
    rv_is_empty = boolc( sy-subrc <> 0 ).

  ENDMETHOD.


  METHOD is_local.

    DATA lv_dlvunit TYPE tdevc-dlvunit.

    SELECT SINGLE dlvunit FROM tdevc INTO lv_dlvunit
        WHERE devclass = iv_package_name AND intsys <> 'SAP'.
    IF sy-subrc = 0 AND lv_dlvunit = 'LOCAL'.
      rv_is_local = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD load_package.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = iv_package_name
        i_force_reload             = abap_true
      IMPORTING
        e_package                  = ri_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5
        OTHERS                     = 6 ).
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD remove_obsolete_tadir.

    DATA:
      lv_pack  TYPE devclass,
      lt_pack  TYPE STANDARD TABLE OF devclass,
      ls_tadir TYPE zif_abapgit_definitions=>ty_tadir,
      lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt,
      ls_item  TYPE zif_abapgit_definitions=>ty_item.

    " TADIR entries must remain for transportable packages
    IF is_local( iv_package_name ) = abap_false.
      RETURN.
    ENDIF.

    " Clean-up sub packages first
    SELECT devclass FROM tdevc INTO TABLE lt_pack WHERE parentcl = iv_package_name.

    LOOP AT lt_pack INTO lv_pack.
      remove_obsolete_tadir( lv_pack ).
    ENDLOOP.

    " Remove TADIR entries for objects that do not exist anymore
    SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE lt_tadir
      WHERE devclass = iv_package_name ##TOO_MANY_ITAB_FIELDS.

    LOOP AT lt_tadir INTO ls_tadir.
      ls_item-obj_type = ls_tadir-object.
      ls_item-obj_name = ls_tadir-obj_name.

      IF zcl_abapinst_objects=>exists( ls_item ) = abap_false.
        CALL FUNCTION 'TR_TADIR_INTERFACE'
          EXPORTING
            wi_delete_tadir_entry = abap_true
            wi_tadir_pgmid        = 'R3TR'
            wi_tadir_object       = ls_tadir-object
            wi_tadir_obj_name     = ls_tadir-obj_name
            wi_test_modus         = abap_false
          EXCEPTIONS
            OTHERS                = 1 ##FM_SUBRC_OK.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_lock.

    DATA: lv_changeable TYPE abap_bool.

    ii_package->get_changeable( IMPORTING e_changeable = lv_changeable ).
    IF lv_changeable <> iv_lock.
      TRY.
          CALL METHOD ii_package->('SET_CHANGEABLE')
            EXPORTING
              i_changeable                = iv_lock
              i_suppress_dialog           = abap_true " Parameter missing in 702
            EXCEPTIONS
              object_locked_by_other_user = 1
              permission_failure          = 2
              object_already_changeable   = 3
              object_already_unlocked     = 4
              object_just_created         = 5
              object_deleted              = 6
              object_modified             = 7
              object_not_existing         = 8
              object_invalid              = 9
              unexpected_error            = 10
              OTHERS                      = 11.
        CATCH cx_sy_dyn_call_param_not_found.
          ii_package->set_changeable(
            EXPORTING
              i_changeable                = iv_lock
            EXCEPTIONS
              object_locked_by_other_user = 1
              permission_failure          = 2
              object_already_changeable   = 3
              object_already_unlocked     = 4
              object_just_created         = 5
              object_deleted              = 6
              object_modified             = 7
              object_not_existing         = 8
              object_invalid              = 9
              unexpected_error            = 10
              OTHERS                      = 11 ).
      ENDTRY.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    TRY.
        CALL METHOD ii_package->('SET_PERMISSIONS_CHANGEABLE')
          EXPORTING
            i_changeable                = iv_lock
            i_suppress_dialog           = abap_true " Parameter missing in 702
          EXCEPTIONS
            object_already_changeable   = 1
            object_already_unlocked     = 2
            object_locked_by_other_user = 3
            object_modified             = 4
            object_just_created         = 5
            object_deleted              = 6
            permission_failure          = 7
            object_invalid              = 8
            unexpected_error            = 9
            OTHERS                      = 10.
      CATCH cx_sy_dyn_call_param_not_found.
        ii_package->set_permissions_changeable(
          EXPORTING
            i_changeable                = iv_lock
          EXCEPTIONS
            object_already_changeable   = 1
            object_already_unlocked     = 2
            object_locked_by_other_user = 3
            object_modified             = 4
            object_just_created         = 5
            object_deleted              = 6
            permission_failure          = 7
            object_invalid              = 8
            unexpected_error            = 9
            OTHERS                      = 10 ).
    ENDTRY.
    IF ( sy-subrc = 1 AND iv_lock = abap_true ) OR ( sy-subrc = 2 AND iv_lock = abap_false ).
      " There's no getter to find out beforehand...
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD update_pinf_usages.
    DATA: lt_current_permissions TYPE tpak_permission_to_use_list,
          li_usage               TYPE REF TO if_package_permission_to_use,
          ls_data_sign           TYPE scomppsign,
          ls_add_permission_data TYPE pkgpermdat,
          lt_handled             TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
    FIELD-SYMBOLS: <ls_usage_data> LIKE LINE OF it_usage_data.

    " Get the current permissions
    ii_package->get_permissions_to_use(
      IMPORTING
        e_permissions    = lt_current_permissions
      EXCEPTIONS
        object_invalid   = 1
        unexpected_error = 2
        OTHERS           = 3 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ls_data_sign-err_sever = abap_true.

    " New permissions
    LOOP AT it_usage_data ASSIGNING <ls_usage_data>.
      READ TABLE lt_current_permissions
           WITH KEY table_line->package_interface_name = <ls_usage_data>-intf_name
           INTO li_usage.

      IF sy-subrc = 0 AND li_usage IS BOUND.
        INSERT sy-tabix INTO TABLE lt_handled.

        " Permission already exists, update attributes
        li_usage->set_all_attributes(
          EXPORTING
            i_permission_data     = <ls_usage_data>
            i_data_sign           = ls_data_sign
          EXCEPTIONS
            object_not_changeable = 1
            object_invalid        = 2
            intern_err            = 3
            OTHERS                = 4 ).
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise_t100( ).
        ENDIF.

      ELSE.
        " Permission does not exist yet, add it
        MOVE-CORRESPONDING <ls_usage_data> TO ls_add_permission_data.
        ii_package->add_permission_to_use(
          EXPORTING
            i_pkg_permission_data   = ls_add_permission_data
          EXCEPTIONS
            object_not_changeable   = 1
            object_access_error     = 2
            object_already_existing = 3
            object_invalid          = 4
            unexpected_error        = 5
            OTHERS                  = 6 ).
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise_t100( ).
        ENDIF.

      ENDIF.

      FREE li_usage.
    ENDLOOP.

    " Delete missing usages
    LOOP AT lt_current_permissions INTO li_usage.
      READ TABLE lt_handled WITH TABLE KEY table_line = sy-tabix TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      li_usage->delete(
        EXCEPTIONS
          object_not_changeable = 1
          object_invalid        = 2
*          deletion_not_allowed  = 3 downport, does not exist in 7.30
          intern_err            = 4
          OTHERS                = 5 ).
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = get_package( )->changed_by.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: li_package TYPE REF TO if_package,
          lv_package TYPE devclass.

    " Package deletion is a bit tricky. A package can only be deleted if there are no objects
    " contained in it. This includes subpackages, so first the leaf packages need to be deleted.
    " Unfortunately deleted objects that are still contained in an unreleased transport request
    " also count towards the contained objects counter.
    " -> Currently we delete only empty packages
    "
    " If objects are deleted, the TADIR entry is deleted when the transport request is released.
    " So before we can delete the package, the transport which deletes the objects
    " in the package has to be released.

    lv_package = ms_item-obj_name.

    remove_obsolete_tadir( lv_package ).

    IF is_empty( lv_package ) = abap_true.

      li_package = load_package( lv_package ).

      IF li_package IS NOT BOUND.
        RETURN.
      ENDIF.

      set_lock( ii_package = li_package
                iv_lock    = abap_true ).

      TRY.
          CALL METHOD li_package->('DELETE')
            EXPORTING
              i_suppress_dialog     = abap_true  " Parameter missing in 702
            EXCEPTIONS
              object_not_empty      = 1
              object_not_changeable = 2
              object_invalid        = 3
              intern_err            = 4
              OTHERS                = 5.

        CATCH cx_sy_dyn_call_param_not_found.

          li_package->delete(
            EXCEPTIONS
              object_not_empty      = 1
              object_not_changeable = 2
              object_invalid        = 3
              intern_err            = 4
              OTHERS                = 5 ).

      ENDTRY.

      IF sy-subrc <> 0.
        set_lock( ii_package = li_package
                  iv_lock    = abap_false ).
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      TRY.
          CALL METHOD li_package->('SAVE')
            EXPORTING
              i_suppress_dialog     = abap_true
            EXCEPTIONS
              object_invalid        = 1
              object_not_changeable = 2
              cancelled_in_corr     = 3
              permission_failure    = 4
              unexpected_error      = 5
              intern_err            = 6
              OTHERS                = 7.

        CATCH cx_sy_dyn_call_param_not_found.

          li_package->save(
            EXCEPTIONS
              object_invalid        = 1
              object_not_changeable = 2
              cancelled_in_corr     = 3
              permission_failure    = 4
              unexpected_error      = 5
              intern_err            = 6
              OTHERS                = 7 ).

      ENDTRY.
      IF sy-subrc <> 0.
        set_lock( ii_package = li_package
                  iv_lock    = abap_false ).
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
    DATA: li_package      TYPE REF TO if_package,
          ls_package_data TYPE scompkdtln,
          ls_data_sign    TYPE scompksign,
          lt_usage_data   TYPE scomppdata,
          ls_save_sign    TYPE paksavsign.

    FIELD-SYMBOLS: <ls_usage_data> TYPE scomppdtln.


    mv_local_devclass = iv_package.

    io_xml->read(
      EXPORTING
        iv_name = 'DEVC'
      CHANGING
        cg_data = ls_package_data ).

    li_package = get_package( ).

    " Swap out repository package name with the local installation package name
    ls_package_data-devclass = mv_local_devclass.
    IF li_package IS BOUND.
      ls_package_data-pdevclass = li_package->transport_layer.
    ENDIF.

    " Parent package is not changed. Assume the folder logic already created the package and set
    " the hierarchy before.
    CLEAR ls_package_data-parentcl.

* Fields not set:
* korrflag
* dlvunit
* parentcl
* cli_check
* intprefx
    ls_data_sign-ctext            = abap_true.
    ls_data_sign-as4user          = abap_true.
    ls_data_sign-pdevclass        = abap_true.
    ls_data_sign-comp_posid       = abap_true.
    ls_data_sign-component        = abap_true.
    ls_data_sign-perminher        = abap_true.
    ls_data_sign-packtype         = abap_true.
    ls_data_sign-restricted       = abap_true.
    ls_data_sign-mainpack         = abap_true.
    ls_data_sign-srv_check        = abap_true.
    ls_data_sign-ext_alias        = abap_true.
    ls_data_sign-project_guid     = abap_true.
    ls_data_sign-project_id       = abap_true.
    ls_data_sign-project_passdown = abap_true.

    IF ls_package_data-ctext IS INITIAL.
      ls_package_data-ctext = mv_local_devclass.
    ENDIF.
    IF ls_package_data-dlvunit IS INITIAL.
      ls_package_data-dlvunit = 'HOME'.
    ENDIF.

    ls_package_data-as4user = cl_abap_syst=>get_user_name( ).

    IF li_package IS BOUND.
      " Package already exists, change it
      set_lock( ii_package = li_package
                iv_lock    = abap_true ).

      li_package->set_all_attributes(
        EXPORTING
          i_package_data             = ls_package_data
          i_data_sign                = ls_data_sign
        EXCEPTIONS
          object_not_changeable      = 1
          object_deleted             = 2
          object_invalid             = 3
          short_text_missing         = 4
          author_not_existing        = 5
          local_package              = 6
          software_component_invalid = 7
          layer_invalid              = 8
          korrflag_invalid           = 9
          component_not_existing     = 10
          component_missing          = 11
          authorize_failure          = 12
          prefix_in_use              = 13
          unexpected_error           = 14
          intern_err                 = 15
*          wrong_mainpack_value       = 16  downport, does not exist in 7.30
*          superpackage_invalid       = 17  downport, does not exist in 7.30
          OTHERS                     = 18 ).
      IF sy-subrc <> 0.
        set_lock( ii_package = li_package
                  iv_lock    = abap_false ).
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ELSE.
      " Package does not exist yet, create it
      " This shouldn't really happen, because the folder logic initially creates the packages.
      cl_package_factory=>create_new_package(
        IMPORTING
          e_package                  = li_package
        CHANGING
          c_package_data             = ls_package_data
        EXCEPTIONS
          object_already_existing    = 1
          object_just_created        = 2
          not_authorized             = 3
          wrong_name_prefix          = 4
          undefined_name             = 5
          reserved_local_name        = 6
          invalid_package_name       = 7
          short_text_missing         = 8
          software_component_invalid = 9
          layer_invalid              = 10
          author_not_existing        = 11
          component_not_existing     = 12
          component_missing          = 13
          prefix_in_use              = 14
          unexpected_error           = 15
          intern_err                 = 16
          no_access                  = 17
*          invalid_translation_depth  = 18 downport, does not exist in 7.30
*          wrong_mainpack_value       = 19 downport, does not exist in 7.30
*          superpackage_invalid       = 20 downport, does not exist in 7.30
*          error_in_cts_checks        = 21 downport, does not exist in 7.31
          OTHERS                     = 22 ).
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    " Load package interface usages
    TRY.
        io_xml->read(
          EXPORTING
            iv_name = 'PERMISSION'
          CHANGING
            cg_data = lt_usage_data ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
        " No permissions saved
    ENDTRY.

    LOOP AT lt_usage_data ASSIGNING <ls_usage_data>.
      <ls_usage_data>-client_pak = mv_local_devclass.
    ENDLOOP.

    update_pinf_usages( ii_package    = li_package
                        it_usage_data = lt_usage_data ).

    ls_save_sign-pack = ls_save_sign-permis = ls_save_sign-elems = ls_save_sign-interf = abap_true.
    li_package->save_generic(
      EXPORTING
        i_save_sign           = ls_save_sign
      EXCEPTIONS
        cancelled_in_corr     = 1
        permission_failure    = 2
        object_not_changeable = 3
        object_invalid        = 4
        OTHERS                = 5 ).
    IF sy-subrc <> 0.
      set_lock( ii_package = li_package
                iv_lock    = abap_false ).
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    set_lock( ii_package = li_package
              iv_lock    = abap_false ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.
    " Check remote package if deserialize has not been called before this
    IF mv_local_devclass IS INITIAL.
      rv_bool = abap_false.
    ELSE.
      cl_package_helper=>check_package_existence(
        EXPORTING
          i_package_name          = mv_local_devclass
        IMPORTING
          e_package_exists        = rv_bool
        EXCEPTIONS
          intern_err              = 1
          OTHERS                  = 2 ).
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = |DV{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = lv_object ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = ms_item-obj_name
        object_type         = 'DEVC'
        in_new_window       = abap_true
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.
    DATA: ls_package_data TYPE scompkdtln,
          li_package      TYPE REF TO if_package,
          lt_intf_usages  TYPE tpak_permission_to_use_list,
          lt_usage_data   TYPE scomppdata,
          ls_usage_data   TYPE scomppdtln,
          li_usage        TYPE REF TO if_package_permission_to_use.

    FIELD-SYMBOLS: <lg_field> TYPE any.


    li_package = get_package( ).
    IF li_package IS NOT BOUND.
      zcx_abapgit_exception=>raise( |Could not find package to serialize.| ).
    ENDIF.

    li_package->get_all_attributes(
      IMPORTING
        e_package_data  = ls_package_data
      EXCEPTIONS
        object_invalid  = 1
        package_deleted = 2
        intern_err      = 3
        OTHERS          = 4 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CLEAR: ls_package_data-devclass,
           ls_package_data-parentcl.

    " Clear administrative data to prevent diffs
    CLEAR: ls_package_data-created_by,
           ls_package_data-created_on,
           ls_package_data-changed_by,
           ls_package_data-changed_on,
           ls_package_data-as4user.

    " Clear text descriptions that might be localized
    CLEAR: ls_package_data-comp_text,
           ls_package_data-dlvu_text,
           ls_package_data-layer_text.

    " Clear obsolete fields
    CLEAR: ls_package_data-intfprefx,
           ls_package_data-cli_check.

    ASSIGN COMPONENT 'TRANSLATION_DEPTH_TEXT'
           OF STRUCTURE ls_package_data
           TO <lg_field>.
    IF sy-subrc = 0.
      CLEAR: <lg_field>.
    ENDIF.

    ASSIGN COMPONENT 'TRANSLATION_GRAPH_DEPTH_TEXT'
           OF STRUCTURE ls_package_data
           TO <lg_field>.
    IF sy-subrc = 0.
      CLEAR: <lg_field>.
    ENDIF.

    " Clear things related to local installation package
    CLEAR: ls_package_data-namespace,
           ls_package_data-dlvunit,
           ls_package_data-pdevclass.

    " Not usable on customer systems
    ASSIGN COMPONENT 'TRANSLATION_DEPTH'
           OF STRUCTURE ls_package_data
           TO <lg_field>.
    IF sy-subrc = 0.
      CLEAR: <lg_field>.
    ENDIF.

    ASSIGN COMPONENT 'TRANSLATION_GRAPH_DEPTH'
           OF STRUCTURE ls_package_data
           TO <lg_field>.
    IF sy-subrc = 0.
      CLEAR: <lg_field>.
    ENDIF.

    CLEAR: ls_package_data-korrflag.

    io_xml->add( iv_name = 'DEVC'
                 ig_data = ls_package_data ).

    " Save package interface usages
    li_package->get_permissions_to_use(
      IMPORTING
        e_permissions    = lt_intf_usages
      EXCEPTIONS
        object_invalid   = 1
        unexpected_error = 2
        OTHERS           = 3 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_intf_usages INTO li_usage.
      li_usage->get_all_attributes(
        IMPORTING
          e_permission_data = ls_usage_data
        EXCEPTIONS
          object_invalid    = 1
          intern_err        = 2
          OTHERS            = 3 ).
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      CLEAR: ls_usage_data-pack_name, ls_usage_data-client_pak.

      APPEND ls_usage_data TO lt_usage_data.
    ENDLOOP.

    IF lt_usage_data IS NOT INITIAL.
      io_xml->add( iv_name = 'PERMISSION'
                   ig_data = lt_usage_data ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_doma IMPLEMENTATION.


  METHOD deserialize_texts.

    DATA: lv_name       TYPE ddobjname,
          lv_valpos     TYPE valpos,
          ls_dd01v_tmp  TYPE dd01v,
          lt_dd07v_tmp  TYPE TABLE OF dd07v,
          lt_i18n_langs TYPE TABLE OF langu,
          lt_dd01_texts TYPE ty_dd01_texts,
          lt_dd07_texts TYPE ty_dd07_texts.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd07v>     LIKE LINE OF it_dd07v,
                   <ls_dd01_text> LIKE LINE OF lt_dd01_texts,
                   <ls_dd07_text> LIKE LINE OF lt_dd07_texts.

    lv_name = ms_item-obj_name.

    ii_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    ii_xml->read( EXPORTING iv_name = 'DD01_TEXTS'
                  CHANGING  cg_data = lt_dd01_texts ).

    ii_xml->read( EXPORTING iv_name = 'DD07_TEXTS'
                  CHANGING  cg_data = lt_dd07_texts ).

    SORT lt_i18n_langs.
    SORT lt_dd07_texts BY ddlanguage. " Optimization

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.

      " Domain description
      ls_dd01v_tmp = is_dd01v.
      READ TABLE lt_dd01_texts ASSIGNING <ls_dd01_text> WITH KEY ddlanguage = <lv_lang>.
      IF sy-subrc > 0.
        zcx_abapgit_exception=>raise( |DD01_TEXTS cannot find lang { <lv_lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <ls_dd01_text> TO ls_dd01v_tmp.

      " Domain values
      lt_dd07v_tmp = it_dd07v.
      LOOP AT lt_dd07v_tmp ASSIGNING <ls_dd07v>.
        lv_valpos = <ls_dd07v>-valpos.
        " it_dd07v was potentially renumbered so lookup by value
        READ TABLE lt_dd07_texts ASSIGNING <ls_dd07_text>
          WITH KEY ddlanguage = <lv_lang> domvalue_l = <ls_dd07v>-domvalue_l domvalue_h = <ls_dd07v>-domvalue_h.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING <ls_dd07_text> TO <ls_dd07v>.
          <ls_dd07v>-valpos = lv_valpos.
          DELETE lt_dd07_texts INDEX sy-tabix. " Optimization
        ELSE.
          " no translation -> keep entry but clear texts
          <ls_dd07v>-ddlanguage = <lv_lang>.
          CLEAR: <ls_dd07v>-ddtext, <ls_dd07v>-domval_ld, <ls_dd07v>-domval_hd.
        ENDIF.
      ENDLOOP.

      CALL FUNCTION 'DDIF_DOMA_PUT'
        EXPORTING
          name              = lv_name
          dd01v_wa          = ls_dd01v_tmp
        TABLES
          dd07v_tab         = lt_dd07v_tmp
        EXCEPTIONS
          doma_not_found    = 1
          name_inconsistent = 2
          doma_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_texts.

    DATA: lv_name       TYPE ddobjname,
          lv_index      TYPE i,
          ls_dd01v      TYPE dd01v,
          lt_dd07v      TYPE TABLE OF dd07v,
          lt_i18n_langs TYPE TABLE OF langu,
          lt_dd01_texts TYPE ty_dd01_texts,
          lt_dd07_texts TYPE ty_dd07_texts.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd07v>     LIKE LINE OF lt_dd07v,
                   <ls_dd07v_tmp> LIKE LINE OF lt_dd07v,
                   <ls_dd01_text> LIKE LINE OF lt_dd01_texts,
                   <ls_dd07_text> LIKE LINE OF lt_dd07_texts.

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      RETURN.
    ENDIF.

    lv_name = ms_item-obj_name.

    " Collect additional languages, skip main lang - it was serialized already
    SELECT DISTINCT ddlanguage AS langu INTO TABLE lt_i18n_langs
      FROM dd01v
      WHERE domname = lv_name
      AND ddlanguage <> mv_language.                      "#EC CI_SUBRC

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      lv_index = sy-tabix.

      CALL FUNCTION 'DDIF_DOMA_GET'
        EXPORTING
          name          = lv_name
          langu         = <lv_lang>
        IMPORTING
          dd01v_wa      = ls_dd01v
        TABLES
          dd07v_tab     = lt_dd07v
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0 OR ls_dd01v-ddlanguage IS INITIAL.
        DELETE lt_i18n_langs INDEX lv_index. " Don't save this lang
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lt_dd01_texts ASSIGNING <ls_dd01_text>.
      MOVE-CORRESPONDING ls_dd01v TO <ls_dd01_text>.

      " Process main language entries and find corresponding translation
      LOOP AT it_dd07v ASSIGNING <ls_dd07v> WHERE NOT ddlanguage IS INITIAL.
        APPEND INITIAL LINE TO lt_dd07_texts ASSIGNING <ls_dd07_text>.
        READ TABLE lt_dd07v ASSIGNING <ls_dd07v_tmp>
          WITH KEY ddlanguage = <lv_lang> domvalue_l = <ls_dd07v>-domvalue_l domvalue_h = <ls_dd07v>-domvalue_h.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING <ls_dd07v_tmp> TO <ls_dd07_text>.
        ELSE.
          " no translation -> keep entry but clear texts
          MOVE-CORRESPONDING <ls_dd07v> TO <ls_dd07_text>.
          <ls_dd07_text>-ddlanguage = <lv_lang>.
          CLEAR: <ls_dd07_text>-ddtext, <ls_dd07_text>-domval_ld, <ls_dd07_text>-domval_hd.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    SORT lt_i18n_langs ASCENDING.
    SORT lt_dd01_texts BY ddlanguage ASCENDING.
    SORT lt_dd07_texts BY valpos ASCENDING ddlanguage ASCENDING.

    IF lines( lt_i18n_langs ) > 0.
      ii_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      ii_xml->add( iv_name = 'DD01_TEXTS'
                   ig_data = lt_dd01_texts ).

      ii_xml->add( iv_name = 'DD07_TEXTS'
                   ig_data = lt_dd07_texts ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd01l INTO rv_user
      WHERE domname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    delete_ddic( iv_objtype              = 'D'
                 iv_no_ask_delete_append = abap_true ).

    delete_longtexts( c_longtext_id_doma ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

* package SEDD
* package SDIC

* fm TR_TADIR_INTERFACE
* fm RS_CORR_INSERT ?

    DATA: lv_name  TYPE ddobjname,
          ls_dd01v TYPE dd01v,
          lt_dd07v TYPE TABLE OF dd07v.

    FIELD-SYMBOLS <ls_dd07v> TYPE dd07v.

    io_xml->read( EXPORTING iv_name = 'DD01V'
                  CHANGING cg_data = ls_dd01v ).
    io_xml->read( EXPORTING iv_name = 'DD07V_TAB'
                  CHANGING cg_data = lt_dd07v ).

    corr_insert( iv_package = iv_package
                 ig_object_class = 'DICT' ).

    lv_name = ms_item-obj_name. " type conversion

    LOOP AT lt_dd07v ASSIGNING <ls_dd07v>.
      <ls_dd07v>-domname = lv_name.
      <ls_dd07v>-valpos = sy-tabix.
    ENDLOOP.

    CALL FUNCTION 'DDIF_DOMA_PUT'
      EXPORTING
        name              = lv_name
        dd01v_wa          = ls_dd01v
      TABLES
        dd07v_tab         = lt_dd07v
      EXCEPTIONS
        doma_not_found    = 1
        name_inconsistent = 2
        doma_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    deserialize_texts( ii_xml   = io_xml
                       is_dd01v = ls_dd01v
                       it_dd07v = lt_dd07v ).

    deserialize_longtexts( io_xml ).

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_domname TYPE dd01l-domname.

    SELECT SINGLE domname FROM dd01l INTO lv_domname
      WHERE domname = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-ddic = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESDICT'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    jump_se11( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_name    TYPE ddobjname,
          ls_dd01v   TYPE dd01v,
          lv_masklen TYPE c LENGTH 4,
          lt_dd07v   TYPE TABLE OF dd07v.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = lv_name
        langu         = mv_language
      IMPORTING
        dd01v_wa      = ls_dd01v
      TABLES
        dd07v_tab     = lt_dd07v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF ls_dd01v IS INITIAL.
      zcx_abapgit_exception=>raise( |No active version found for { ms_item-obj_type } { ms_item-obj_name }| ).
    ENDIF.

    CLEAR: ls_dd01v-as4user,
           ls_dd01v-as4date,
           ls_dd01v-as4time,
           ls_dd01v-appexist.

* make sure XML serialization does not dump if the field contains invalid data
* note that this is a N field, so '' is not valid
    IF ls_dd01v-authclass = ''.
      CLEAR ls_dd01v-authclass.
    ENDIF.
    lv_masklen = ls_dd01v-masklen.
    IF lv_masklen = '' OR NOT lv_masklen CO '0123456789'.
      CLEAR ls_dd01v-masklen.
    ENDIF.

    DELETE lt_dd07v WHERE appval = abap_true.

    SORT lt_dd07v BY
      valpos ASCENDING
      ddlanguage ASCENDING.

    io_xml->add( iv_name = 'DD01V'
                 ig_data = ls_dd01v ).
    io_xml->add( iv_name = 'DD07V_TAB'
                 ig_data = lt_dd07v ).

    serialize_texts( ii_xml   = io_xml
                     it_dd07v = lt_dd07v ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_doma ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_dsys IMPLEMENTATION.


  METHOD constructor.

    DATA: lv_prefix    TYPE namespace,
          lv_bare_name TYPE progname.

    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    IF ms_item-obj_name(1) = '/'.
      CALL FUNCTION 'RS_NAME_SPLIT_NAMESPACE'
        EXPORTING
          name_with_namespace    = ms_item-obj_name
        IMPORTING
          namespace              = lv_prefix
          name_without_namespace = lv_bare_name.
    ELSE.
      lv_bare_name = ms_item-obj_name.
    ENDIF.

    mv_doc_object = |{ lv_bare_name+0(4) }{ lv_prefix }{ lv_bare_name+4(*) }|.

  ENDMETHOD.


  METHOD deserialize_dsys.

    DATA: ls_data      TYPE ty_data,
          ls_docu_info TYPE dokil,
          lv_version   TYPE dokvers,
          lv_doku_obj  TYPE doku_obj.

    lv_doku_obj = mv_doc_object.
    ii_xml->read( EXPORTING iv_name = 'DSYS'
                  CHANGING cg_data = ls_data ).

    CALL FUNCTION 'DOCU_INIT'
      EXPORTING
        id     = c_id
        langu  = mv_language
        object = lv_doku_obj
        typ    = c_typ
      IMPORTING
        xdokil = ls_docu_info.

    lv_version = ls_docu_info-version.

    CALL FUNCTION 'DOCU_UPDATE'
      EXPORTING
        head    = ls_data-head
        state   = 'A'
        typ     = c_typ
        version = lv_version
      TABLES
        line    = ls_data-lines.

  ENDMETHOD.


  METHOD get_main_lang.

    SELECT SINGLE langu FROM dokil INTO rv_language
      WHERE id = c_id
      AND object = mv_doc_object
      AND masterlang = abap_true.

    IF sy-subrc <> 0.
      rv_language = mv_language.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    rv_user = zcl_abapinst_factory=>get_longtexts( )->changed_by(
      iv_object_name = mv_doc_object
      iv_longtext_id = c_id ).

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    zcl_abapinst_factory=>get_longtexts( )->delete(
      iv_object_name = mv_doc_object
      iv_longtext_id = c_id ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_metadata TYPE zif_abapgit_definitions=>ty_metadata.

    ls_metadata = io_xml->get_metadata( ).

    CASE ls_metadata-version.

      WHEN 'v1.0.0'.
        deserialize_dsys( io_xml ).

      WHEN 'v2.0.0'.
        zcl_abapinst_factory=>get_longtexts( )->deserialize(
          ii_xml           = io_xml
          iv_main_language = mv_language ).

      WHEN OTHERS.
        zcx_abapgit_exception=>raise( 'unsupported DSYS version' ).

    ENDCASE.

    tadir_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_count TYPE i.

    SELECT SINGLE COUNT( * ) FROM dokil INTO lv_count
           WHERE id   = c_id
           AND object = mv_doc_object.                  "#EC CI_GENBUFF

    rv_bool = boolc( lv_count > 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.
    rs_metadata-version = 'v2.0.0'.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA lv_lang TYPE sy-langu.

    lv_lang = get_main_lang( ).

    CALL FUNCTION 'DSYS_EDIT'
      EXPORTING
        dokclass         = mv_doc_object+0(4)
        dokname          = mv_doc_object+4(*)
        doklangu         = lv_lang
      EXCEPTIONS
        class_unknown    = 1
        object_not_found = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    zcl_abapinst_factory=>get_longtexts( )->serialize(
      iv_object_name = mv_doc_object
      iv_longtext_id = c_id
      ii_xml         = io_xml ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_dtel IMPLEMENTATION.


  METHOD deserialize_texts.

    DATA: lv_name       TYPE ddobjname,
          ls_dd04v_tmp  TYPE dd04v,
          lt_i18n_langs TYPE TABLE OF langu,
          lt_dd04_texts TYPE ty_dd04_texts.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd04_text> LIKE LINE OF lt_dd04_texts.


    lv_name = ms_item-obj_name.

    ii_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    ii_xml->read( EXPORTING iv_name = 'DD04_TEXTS'
                  CHANGING  cg_data = lt_dd04_texts ).

    SORT lt_i18n_langs.
    SORT lt_dd04_texts BY ddlanguage. " Optimization

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.

      " Data element description
      ls_dd04v_tmp = is_dd04v.
      READ TABLE lt_dd04_texts ASSIGNING <ls_dd04_text> WITH KEY ddlanguage = <lv_lang>.
      IF sy-subrc > 0.
        zcx_abapgit_exception=>raise( |DD04_TEXTS cannot find lang { <lv_lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <ls_dd04_text> TO ls_dd04v_tmp.
      CALL FUNCTION 'DDIF_DTEL_PUT'
        EXPORTING
          name              = lv_name
          dd04v_wa          = ls_dd04v_tmp
        EXCEPTIONS
          dtel_not_found    = 1
          name_inconsistent = 2
          dtel_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_texts.

    DATA: lv_name       TYPE ddobjname,
          lv_index      TYPE i,
          ls_dd04v      TYPE dd04v,
          lt_dd04_texts TYPE ty_dd04_texts,
          lt_i18n_langs TYPE TABLE OF langu.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd04_text> LIKE LINE OF lt_dd04_texts.

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      RETURN.
    ENDIF.

    lv_name = ms_item-obj_name.

    " Collect additional languages, skip main lang - it was serialized already
    SELECT DISTINCT ddlanguage AS langu INTO TABLE lt_i18n_langs
      FROM dd04v
      WHERE rollname = lv_name
      AND ddlanguage <> mv_language.                      "#EC CI_SUBRC

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      lv_index = sy-tabix.
      CALL FUNCTION 'DDIF_DTEL_GET'
        EXPORTING
          name          = lv_name
          langu         = <lv_lang>
        IMPORTING
          dd04v_wa      = ls_dd04v
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0 OR ls_dd04v-ddlanguage IS INITIAL.
        DELETE lt_i18n_langs INDEX lv_index. " Don't save this lang
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lt_dd04_texts ASSIGNING <ls_dd04_text>.
      MOVE-CORRESPONDING ls_dd04v TO <ls_dd04_text>.

    ENDLOOP.

    SORT lt_i18n_langs ASCENDING.
    SORT lt_dd04_texts BY ddlanguage ASCENDING.

    IF lines( lt_i18n_langs ) > 0.
      ii_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      ii_xml->add( iv_name = 'DD04_TEXTS'
                   ig_data = lt_dd04_texts ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd04l INTO rv_user
      WHERE rollname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    delete_ddic( 'E' ).

    delete_longtexts( c_longtext_id_dtel ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_dd04v TYPE dd04v,
          lv_name  TYPE ddobjname.


    io_xml->read( EXPORTING iv_name = 'DD04V'
                  CHANGING cg_data = ls_dd04v ).

    " DDIC Step: Replace REF TO class/interface with generic reference to avoid cyclic dependency
    IF iv_step = zif_abapgit_object=>gc_step_id-ddic AND ls_dd04v-datatype = 'REF'.
      ls_dd04v-rollname = 'OBJECT'.
    ELSEIF iv_step = zif_abapgit_object=>gc_step_id-late AND ls_dd04v-datatype <> 'REF'.
      RETURN. " already active
    ENDIF.

    corr_insert( iv_package = iv_package
                 ig_object_class = 'DICT' ).

    lv_name = ms_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_DTEL_PUT'
      EXPORTING
        name              = lv_name
        dd04v_wa          = ls_dd04v
      EXCEPTIONS
        dtel_not_found    = 1
        name_inconsistent = 2
        dtel_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    deserialize_texts( ii_xml   = io_xml
                       is_dd04v = ls_dd04v ).

    deserialize_longtexts( io_xml ).

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_rollname TYPE dd04l-rollname.

    lv_rollname = ms_item-obj_name.

    " Check nametab because it's fast
    CALL FUNCTION 'DD_GET_NAMETAB_HEADER'
      EXPORTING
        tabname   = lv_rollname
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      " Check for inactive or modified versions
      SELECT SINGLE rollname FROM dd04l INTO lv_rollname
        WHERE rollname = lv_rollname.
    ENDIF.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
    APPEND zif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-ddic = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESDICT'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    jump_se11( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.
* fm DDIF_DTEL_GET bypasses buffer, so SELECTs are
* done directly from here

    DATA: lv_name  TYPE ddobjname,
          ls_dd04v TYPE dd04v.

    lv_name = ms_item-obj_name.

    SELECT SINGLE * FROM dd04l
      INTO CORRESPONDING FIELDS OF ls_dd04v
      WHERE rollname = lv_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc <> 0 OR ls_dd04v IS INITIAL.
      zcx_abapgit_exception=>raise( |No active version found for { ms_item-obj_type } { ms_item-obj_name }| ).
    ENDIF.

    SELECT SINGLE * FROM dd04t
      INTO CORRESPONDING FIELDS OF ls_dd04v
      WHERE rollname = lv_name
      AND ddlanguage = mv_language
      AND as4local = 'A'
      AND as4vers = '0000'.

    CLEAR: ls_dd04v-as4user,
           ls_dd04v-as4date,
           ls_dd04v-as4time.

    IF ls_dd04v-refkind = 'D'.
* clear values inherited from domain
      CLEAR: ls_dd04v-datatype,
             ls_dd04v-leng,
             ls_dd04v-decimals,
             ls_dd04v-outputlen,
             ls_dd04v-valexi,
             ls_dd04v-lowercase,
             ls_dd04v-signflag,
             ls_dd04v-convexit,
             ls_dd04v-entitytab.
    ENDIF.

    IF ls_dd04v-routputlen = ''.
* numeric field, make sure it is initial or XML serilization will dump
      CLEAR ls_dd04v-routputlen.
    ENDIF.
    IF ls_dd04v-authclass = ''.
      CLEAR ls_dd04v-authclass.
    ENDIF.

    io_xml->add( iv_name = 'DD04V'
                 ig_data = ls_dd04v ).

    serialize_texts( io_xml ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_dtel ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_enhc IMPLEMENTATION.


  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_composite_id = ms_item-obj_name.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE changedby INTO rv_user FROM enhcompheader
      WHERE enhcomposite = ms_item-obj_name AND version = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lx_error      TYPE REF TO cx_enh_root,
          li_enh_object TYPE REF TO if_enh_object.

    TRY.
        li_enh_object = cl_enh_factory=>load_enhancement_composite(
          name = mv_composite_id
          lock = abap_true ).

        li_enh_object->delete( nevertheless_delete = abap_true
                               run_dark            = abap_true ).
        li_enh_object->unlock( ).

      CATCH cx_enh_root INTO lx_error.
        zcx_abapgit_exception=>raise( lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lx_error            TYPE REF TO cx_enh_root,
          li_enh_composite    TYPE REF TO if_enh_composite,
          lv_package          TYPE devclass,
          lt_composite_childs TYPE enhcompositename_it,
          lt_enh_childs       TYPE enhname_it,
          lv_longtext_id      TYPE enhdocuobject,
          lv_shorttext        TYPE string.

    FIELD-SYMBOLS: <lv_composite_child> TYPE enhcompositename,
                   <lv_enh_child>       LIKE LINE OF lt_enh_childs.

    lv_package = iv_package.


    io_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING  cg_data = lv_shorttext ).
    io_xml->read( EXPORTING iv_name = 'COMPOSITE_CHILDS'
                  CHANGING  cg_data = lt_composite_childs ).
    io_xml->read( EXPORTING iv_name = 'ENH_CHILDS'
                  CHANGING  cg_data = lt_enh_childs ).
    io_xml->read( EXPORTING iv_name = 'LONGTEXT_ID'
                  CHANGING  cg_data = lv_longtext_id ).

    TRY.
        cl_enh_factory=>create_enhancement_composite(
          EXPORTING
            name      = mv_composite_id
            run_dark  = abap_true
          IMPORTING
            composite = li_enh_composite
          CHANGING
            devclass  = lv_package ).

        li_enh_composite->if_enh_object_docu~set_shorttext( lv_shorttext ).

        LOOP AT lt_composite_childs ASSIGNING <lv_composite_child>.
          li_enh_composite->add_composite_child( <lv_composite_child> ).
        ENDLOOP.

        LOOP AT lt_enh_childs ASSIGNING <lv_enh_child>.
          li_enh_composite->add_enh_child( <lv_enh_child> ).
        ENDLOOP.

        li_enh_composite->set_longtext_id( lv_longtext_id ).

        li_enh_composite->if_enh_object~save( ).
        li_enh_composite->if_enh_object~activate( ).
        li_enh_composite->if_enh_object~unlock( ).

        zcl_abapgit_sotr_handler=>create_sotr(
          iv_package = iv_package
          io_xml     = io_xml ).

      CATCH cx_enh_root INTO lx_error.
        zcx_abapgit_exception=>raise( lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    TRY.
        cl_enh_factory=>load_enhancement_composite(
          name = mv_composite_id
          lock = abap_false ).
        rv_bool = abap_true.
      CATCH cx_enh_root.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    DATA: lv_argument TYPE seqg3-garg.

    lv_argument = |{ mv_composite_id }|.
    OVERLAY lv_argument WITH '                                  '.
    lv_argument = |{ lv_argument }*|.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = |E_ENHANCE|
                                            iv_argument    = lv_argument ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = ms_item-obj_type
        in_new_window = abap_true
      EXCEPTIONS
        OTHERS        = 1.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lx_error            TYPE REF TO cx_enh_root,
          li_enh_composite    TYPE REF TO if_enh_composite,
          lt_composite_childs TYPE enhcompositename_it,
          lt_enh_childs       TYPE enhname_it,
          lv_longtext_id      TYPE enhdocuobject,
          lv_shorttext        TYPE string.

    TRY.
        li_enh_composite = cl_enh_factory=>load_enhancement_composite(
          name = mv_composite_id
          lock = abap_false ).

        lv_shorttext = li_enh_composite->if_enh_object_docu~get_shorttext( ).

        lt_composite_childs = li_enh_composite->get_composite_childs( ).
        lt_enh_childs       = li_enh_composite->get_enh_childs( ).
        lv_longtext_id      = li_enh_composite->get_longtext_id( ).

        io_xml->add( iv_name = 'SHORTTEXT'
                     ig_data = lv_shorttext ).
        io_xml->add( iv_name = 'COMPOSITE_CHILDS'
                     ig_data = lt_composite_childs ).
        io_xml->add( iv_name = 'ENH_CHILDS'
                     ig_data = lt_enh_childs ).
        io_xml->add( iv_name = 'LONGTEXT_ID'
                     ig_data = lv_longtext_id ).

        zcl_abapgit_sotr_handler=>read_sotr(
          iv_pgmid    = 'R3TR'
          iv_object   = ms_item-obj_type
          iv_obj_name = ms_item-obj_name
          io_xml      = io_xml ).

      CATCH cx_enh_root INTO lx_error.
        zcx_abapgit_exception=>raise( lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

CLASS zcl_abapgit_object_enho_badi IMPLEMENTATION.

  METHOD constructor.
    ms_item = is_item.
  ENDMETHOD.

  METHOD zif_abapgit_object_enho~serialize.

    DATA: lo_badi_impl TYPE REF TO cl_enh_tool_badi_impl,
          lv_spot_name TYPE enhspotname,
          lv_shorttext TYPE string,
          lt_impl      TYPE enh_badi_impl_data_it.

    FIELD-SYMBOLS: <ls_impl>   LIKE LINE OF lt_impl,
                   <ls_values> LIKE LINE OF <ls_impl>-filter_values,
                   <ls_filter> LIKE LINE OF <ls_impl>-filters.


    lo_badi_impl ?= ii_enh_tool.

    lv_shorttext = lo_badi_impl->if_enh_object_docu~get_shorttext( ).
    lv_spot_name = lo_badi_impl->get_spot_name( ).
    lt_impl      = lo_badi_impl->get_implementations( ).

    LOOP AT lt_impl ASSIGNING <ls_impl>.
* make sure the XML serialization does not dump, field type = N
      LOOP AT <ls_impl>-filter_values ASSIGNING <ls_values>.
        IF <ls_values>-filter_numeric_value1 CA space.
          CLEAR <ls_values>-filter_numeric_value1.
        ENDIF.
      ENDLOOP.
      LOOP AT <ls_impl>-filters ASSIGNING <ls_filter>.
        IF <ls_filter>-filter_numeric_value1 CA space.
          CLEAR <ls_filter>-filter_numeric_value1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    ii_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    ii_xml->add( ig_data = lv_shorttext
                 iv_name = 'SHORTTEXT' ).
    ii_xml->add( iv_name = 'SPOT_NAME'
                 ig_data = lv_spot_name ).
    ii_xml->add( iv_name = 'IMPL'
                 ig_data = lt_impl ).

  ENDMETHOD.

  METHOD zif_abapgit_object_enho~deserialize.

    DATA: lv_spot_name TYPE enhspotname,
          lv_shorttext TYPE string,
          lv_enhname   TYPE enhname,
          lo_badi      TYPE REF TO cl_enh_tool_badi_impl,
          li_tool      TYPE REF TO if_enh_tool,
          lv_package   TYPE devclass,
          lt_impl      TYPE enh_badi_impl_data_it.

    FIELD-SYMBOLS: <ls_impl> LIKE LINE OF lt_impl.


    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data  = lv_shorttext ).
    ii_xml->read( EXPORTING iv_name = 'SPOT_NAME'
                  CHANGING cg_data  = lv_spot_name ).
    ii_xml->read( EXPORTING iv_name = 'IMPL'
                  CHANGING cg_data  = lt_impl ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = cl_abstract_enh_tool_redef=>credefinition
            enhtooltype = cl_enh_tool_badi_impl=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).
        lo_badi ?= li_tool.

        lo_badi->set_spot_name( lv_spot_name ).
        lo_badi->if_enh_object_docu~set_shorttext( lv_shorttext ).
        LOOP AT lt_impl ASSIGNING <ls_impl>.
          lo_badi->add_implementation( <ls_impl> ).
        ENDLOOP.
        lo_badi->if_enh_object~save( run_dark = abap_true ).
        lo_badi->if_enh_object~unlock( ).
      CATCH cx_enh_root.
        zcx_abapgit_exception=>raise( 'error deserializing ENHO badi' ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS zcl_abapgit_object_enho_hook IMPLEMENTATION.

  METHOD constructor.
    ms_item = is_item.
    mo_files = io_files.
  ENDMETHOD.

  METHOD zif_abapgit_object_enho~serialize.

    DATA: lv_shorttext       TYPE string,
          lo_hook_impl       TYPE REF TO cl_enh_tool_hook_impl,
          ls_original_object TYPE enh_hook_admin,
          lt_spaces          TYPE ty_spaces_tt,
          lt_enhancements    TYPE enh_hook_impl_it.

    FIELD-SYMBOLS: <ls_enhancement> LIKE LINE OF lt_enhancements.


    lo_hook_impl ?= ii_enh_tool.

    lv_shorttext = lo_hook_impl->if_enh_object_docu~get_shorttext( ).
    lo_hook_impl->get_original_object(
      IMPORTING
        pgmid     = ls_original_object-pgmid
        obj_name  = ls_original_object-org_obj_name
        obj_type  = ls_original_object-org_obj_type
        main_type = ls_original_object-org_main_type
        main_name = ls_original_object-org_main_name
        program   = ls_original_object-programname ).
    ls_original_object-include_bound = lo_hook_impl->get_include_bound( ).
    lt_enhancements = lo_hook_impl->get_hook_impls( ).

    LOOP AT lt_enhancements ASSIGNING <ls_enhancement>.
      CLEAR: <ls_enhancement>-extid,
             <ls_enhancement>-id.
    ENDLOOP.

    ii_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    ii_xml->add( ig_data = lv_shorttext
                 iv_name = 'SHORTTEXT' ).
    ii_xml->add( ig_data = ls_original_object
                 iv_name = 'ORIGINAL_OBJECT' ).
    ii_xml->add( iv_name = 'ENHANCEMENTS'
                 ig_data = lt_enhancements ).
    ii_xml->add( iv_name = 'SPACES'
                 ig_data = lt_spaces ).

  ENDMETHOD.

  METHOD hook_impl_deserialize.

    FIELD-SYMBOLS: <ls_impl>   LIKE LINE OF ct_impl,
                   <lv_line>   TYPE string,
                   <lv_space>  TYPE i,
                   <ls_spaces> LIKE LINE OF it_spaces.


    LOOP AT ct_impl ASSIGNING <ls_impl>.
      READ TABLE it_spaces ASSIGNING <ls_spaces> WITH KEY full_name = <ls_impl>-full_name.
      IF sy-subrc = 0.
        LOOP AT <ls_impl>-source ASSIGNING <lv_line>.
          READ TABLE <ls_spaces>-spaces ASSIGNING <lv_space> INDEX sy-tabix.
          IF sy-subrc = 0 AND <lv_space> > 0.
            DO <lv_space> TIMES.
              CONCATENATE space <lv_line> INTO <lv_line> RESPECTING BLANKS.
            ENDDO.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD zif_abapgit_object_enho~deserialize.

    DATA: lv_shorttext       TYPE string,
          lo_hook_impl       TYPE REF TO cl_enh_tool_hook_impl,
          li_tool            TYPE REF TO if_enh_tool,
          lv_enhname         TYPE enhname,
          lv_package         TYPE devclass,
          ls_original_object TYPE enh_hook_admin,
          lt_spaces          TYPE ty_spaces_tt,
          lt_enhancements    TYPE enh_hook_impl_it,
          lx_enh_root        TYPE REF TO cx_enh_root.

    FIELD-SYMBOLS: <ls_enhancement> LIKE LINE OF lt_enhancements.


    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data  = lv_shorttext ).
    ii_xml->read( EXPORTING iv_name = 'ORIGINAL_OBJECT'
                  CHANGING cg_data  = ls_original_object ).
    ii_xml->read( EXPORTING iv_name = 'ENHANCEMENTS'
                  CHANGING cg_data  = lt_enhancements ).
    ii_xml->read( EXPORTING iv_name = 'SPACES'
                  CHANGING cg_data  = lt_spaces ).

    " todo: kept for compatibility, remove after grace period #3680
    hook_impl_deserialize( EXPORTING it_spaces = lt_spaces
                           CHANGING ct_impl    = lt_enhancements ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = cl_abstract_enh_tool_redef=>credefinition
            enhtooltype = cl_enh_tool_hook_impl=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).
        lo_hook_impl ?= li_tool.

        lo_hook_impl->if_enh_object_docu~set_shorttext( lv_shorttext ).
        lo_hook_impl->set_original_object(
            pgmid       = ls_original_object-pgmid
            obj_name    = ls_original_object-org_obj_name
            obj_type    = ls_original_object-org_obj_type
            program     = ls_original_object-programname
            main_type   = ls_original_object-org_main_type
            main_name   = ls_original_object-org_main_name ).
        lo_hook_impl->set_include_bound( ls_original_object-include_bound ).

        LOOP AT lt_enhancements ASSIGNING <ls_enhancement>.
          lo_hook_impl->add_hook_impl(
              overwrite        = <ls_enhancement>-overwrite
              method           = <ls_enhancement>-method
              enhmode          = <ls_enhancement>-enhmode
              full_name        = <ls_enhancement>-full_name
              source           = <ls_enhancement>-source
              spot             = <ls_enhancement>-spotname
              parent_full_name = <ls_enhancement>-parent_full_name ).
        ENDLOOP.
        lo_hook_impl->if_enh_object~save( run_dark = abap_true ).
        lo_hook_impl->if_enh_object~unlock( ).
      CATCH cx_enh_root INTO lx_enh_root.
        zcx_abapgit_exception=>raise( lx_enh_root->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.



CLASS zcl_abapgit_object_enho_class IMPLEMENTATION.


  METHOD constructor.
    ms_item = is_item.
    mo_files = io_files.
  ENDMETHOD.


  METHOD deserialize_includes.

    DATA: lt_tab_methods TYPE enhnewmeth_tab,
          lv_editorder   TYPE n LENGTH 3,
          lv_methname    TYPE seocpdname,
          lt_abap        TYPE rswsourcet,
          lx_enh         TYPE REF TO cx_enh_root,
          lv_new_em      TYPE abap_bool,
          lt_files       TYPE zif_abapgit_definitions=>ty_files_tt.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF lt_tab_methods,
                   <ls_file>   TYPE zif_abapgit_definitions=>ty_file.

    ii_xml->read( EXPORTING iv_name = 'TAB_METHODS'
                  CHANGING cg_data = lt_tab_methods ).

    lv_new_em = abap_false.
    lt_files = mo_files->get_files( ).
    LOOP AT lt_files ASSIGNING <ls_file>
        WHERE filename CS 'enho.em_'.
      lv_new_em = abap_true.
      EXIT.
    ENDLOOP.

    SORT lt_tab_methods BY meth_header-editorder.
    LOOP AT lt_tab_methods ASSIGNING <ls_method>.

      lv_methname = <ls_method>-methkey-cmpname.
      IF lv_new_em = abap_true.
        lt_abap = mo_files->read_abap( iv_extra = 'em_' && lv_methname ).
      ELSE.
        " old way
        lv_editorder = <ls_method>-meth_header-editorder.
        lt_abap = mo_files->read_abap( iv_extra = 'em' && lv_editorder ).
      ENDIF.

      TRY.
          io_class->add_change_new_method_source(
              clsname    = <ls_method>-methkey-clsname
              methname   = lv_methname
              methsource = lt_abap ).
        CATCH cx_enh_mod_not_allowed cx_enh_is_not_enhanceable INTO lx_enh.
          zcx_abapgit_exception=>raise( iv_text = 'Error deserializing ENHO method include'
                                        ix_previous = lx_enh ).
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_includes.

    DATA: lt_includes TYPE enhnewmeth_tabincl_plus_enha,
          lt_source   TYPE TABLE OF abaptxt255,
          lv_include  TYPE programm.

    FIELD-SYMBOLS: <ls_include> LIKE LINE OF lt_includes.


    lt_includes = io_class->get_enh_method_includes( ).
    LOOP AT lt_includes ASSIGNING <ls_include>.
      lv_include = io_class->if_enh_tool~get_name( ).
      TRANSLATE lv_include USING ' ='.
      lv_include+30 = 'EM'.
      lv_include+32(8) = <ls_include>-includenr.

      CALL FUNCTION 'RPY_PROGRAM_READ'
        EXPORTING
          program_name     = lv_include
          with_includelist = abap_false
          with_lowercase   = abap_true
        TABLES
          source_extended  = lt_source
        EXCEPTIONS
          cancelled        = 1
          not_found        = 2
          permission_error = 3
          OTHERS           = 4.
      IF sy-subrc = 0.
        mo_files->add_abap( iv_extra = |EM_{ <ls_include>-cpdname }|
                            it_abap  = lt_source ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_object_enho~deserialize.

    DATA: lo_enh_class TYPE REF TO cl_enh_tool_class,
          lt_owr       TYPE enhmeth_tabkeys,
          lt_pre       TYPE enhmeth_tabkeys,
          lt_post      TYPE enhmeth_tabkeys,
          lt_source    TYPE rswsourcet,
          li_tool      TYPE REF TO if_enh_tool,
          lv_shorttext TYPE string,
          lv_class     TYPE seoclsname,
          lv_enhname   TYPE enhname,
          lv_package   TYPE devclass.


    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data  = lv_shorttext ).
    ii_xml->read( EXPORTING iv_name = 'OWR_METHODS'
                  CHANGING cg_data  = lt_owr ).
    ii_xml->read( EXPORTING iv_name = 'PRE_METHODS'
                  CHANGING cg_data  = lt_pre ).
    ii_xml->read( EXPORTING iv_name = 'POST_METHODS'
                  CHANGING cg_data  = lt_post ).
    ii_xml->read( EXPORTING iv_name = 'CLASS'
                  CHANGING cg_data  = lv_class ).
    lt_source = mo_files->read_abap( ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = ''
            enhtooltype = cl_enh_tool_class=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).
        lo_enh_class ?= li_tool.

        lo_enh_class->if_enh_object_docu~set_shorttext( lv_shorttext ).
        lo_enh_class->set_class( lv_class ).
        lo_enh_class->set_owr_methods( version     = 'I'
                                       owr_methods = lt_owr ).
        lo_enh_class->set_pre_methods( version     = 'I'
                                       pre_methods = lt_pre ).
        lo_enh_class->set_post_methods( version      = 'I'
                                        post_methods = lt_post ).
        lo_enh_class->set_eimp_include( version     = 'I'
                                        eimp_source = lt_source ).

        zcl_abapgit_object_enho_clif=>deserialize(
          io_xml  = ii_xml
          io_clif = lo_enh_class ).

        deserialize_includes(
          ii_xml   = ii_xml
          io_class = lo_enh_class ).

        lo_enh_class->if_enh_object~save( run_dark = abap_true ).
        lo_enh_class->if_enh_object~unlock( ).
      CATCH cx_enh_root.
        zcx_abapgit_exception=>raise( 'error deserializing ENHO class' ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object_enho~serialize.

    DATA: lo_enh_class TYPE REF TO cl_enh_tool_class,
          lt_owr       TYPE enhmeth_tabkeys,
          lt_pre       TYPE enhmeth_tabkeys,
          lt_post      TYPE enhmeth_tabkeys,
          lt_source    TYPE rswsourcet,
          lv_class     TYPE seoclsname,
          lv_shorttext TYPE string.


    lo_enh_class ?= ii_enh_tool.

    lv_shorttext = lo_enh_class->if_enh_object_docu~get_shorttext( ).
    lt_owr = lo_enh_class->get_owr_methods( ).
    lt_pre = lo_enh_class->get_pre_methods( ).
    lt_post = lo_enh_class->get_post_methods( ).
    lt_source = lo_enh_class->get_eimp_include( ).
    lo_enh_class->get_class( IMPORTING class_name = lv_class ).

    ii_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    ii_xml->add( ig_data = lv_shorttext
                 iv_name = 'SHORTTEXT' ).
    ii_xml->add( iv_name = 'CLASS'
                 ig_data = lv_class ).
    ii_xml->add( iv_name = 'OWR_METHODS'
                 ig_data = lt_owr ).
    ii_xml->add( iv_name = 'PRE_METHODS'
                 ig_data = lt_pre ).
    ii_xml->add( iv_name = 'POST_METHODS'
                 ig_data = lt_post ).

    mo_files->add_abap( lt_source ).

    zcl_abapgit_object_enho_clif=>serialize(
      io_xml   = ii_xml
      io_files = mo_files
      io_clif  = lo_enh_class ).

    serialize_includes( lo_enh_class ).

  ENDMETHOD.
ENDCLASS.

CLASS zcl_abapgit_object_enho_intf IMPLEMENTATION.

  METHOD constructor.
    ms_item  = is_item.
    mo_files = io_files.
  ENDMETHOD.

  METHOD zif_abapgit_object_enho~serialize.

    DATA: lo_enh_intf  TYPE REF TO cl_enh_tool_intf,
          lv_class     TYPE seoclsname,
          lv_shorttext TYPE string.


    lo_enh_intf ?= ii_enh_tool.

    lv_shorttext = lo_enh_intf->if_enh_object_docu~get_shorttext( ).
    lo_enh_intf->get_class( IMPORTING class_name = lv_class ).

    ii_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    ii_xml->add( ig_data = lv_shorttext
                 iv_name = 'SHORTTEXT' ).
    ii_xml->add( iv_name = 'CLASS'
                 ig_data = lv_class ).

    zcl_abapgit_object_enho_clif=>serialize(
      io_xml  = ii_xml
      io_files = mo_files
      io_clif = lo_enh_intf ).

  ENDMETHOD.

  METHOD zif_abapgit_object_enho~deserialize.

    DATA: lo_enh_intf  TYPE REF TO cl_enh_tool_intf,
          li_tool      TYPE REF TO if_enh_tool,
          lv_shorttext TYPE string,
          lv_class     TYPE seoclsname,
          lv_enhname   TYPE enhname,
          lv_package   TYPE devclass.


    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data  = lv_shorttext ).
    ii_xml->read( EXPORTING iv_name = 'CLASS'
                  CHANGING cg_data  = lv_class ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = ''
            enhtooltype = cl_enh_tool_intf=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).
        lo_enh_intf ?= li_tool.

        lo_enh_intf->if_enh_object_docu~set_shorttext( lv_shorttext ).
        lo_enh_intf->set_class( lv_class ).

        zcl_abapgit_object_enho_clif=>deserialize(
          io_xml  = ii_xml
          io_clif = lo_enh_intf ).

        lo_enh_intf->if_enh_object~save( run_dark = abap_true ).
        lo_enh_intf->if_enh_object~unlock( ).
      CATCH cx_enh_root.
        zcx_abapgit_exception=>raise( 'error deserializing ENHO interface' ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS zcl_abapgit_object_enho_wdyc IMPLEMENTATION.

  METHOD constructor.
    ms_item = is_item.
  ENDMETHOD.

  METHOD zif_abapgit_object_enho~deserialize.

    DATA: lv_enhname TYPE enhname,
          lo_wdyconf TYPE REF TO cl_wdr_cfg_enhancement,
          li_tool    TYPE REF TO if_enh_tool,
          ls_obj     TYPE wdy_config_key,
          lv_xml     TYPE string,
          lt_data    TYPE wdy_cfg_expl_data_tab,
          lv_package TYPE devclass.

    ii_xml->read( EXPORTING iv_name = 'ORIGINAL_OBJECT'
                  CHANGING  cg_data = ls_obj ).

    ii_xml->read( EXPORTING iv_name = 'ENHANCEMENT_DATA'
                  CHANGING  cg_data = lv_xml ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = ''
            enhtooltype = cl_wdr_cfg_enhancement=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).

        lo_wdyconf ?= li_tool.

        CALL METHOD cl_wdr_cfg_persistence_utils=>('COMP_XML_TO_TABLES')
          EXPORTING
            xml_content   = lv_xml
          IMPORTING
            expl_data_tab = lt_data.

* only works on new ABAP versions, parameters differ between versions
        CALL METHOD lo_wdyconf->('SET_ENHANCEMENT_DATA')
          EXPORTING
            p_enh_data = lt_data.

        lo_wdyconf->if_enh_object~save( run_dark = abap_true ).
        lo_wdyconf->if_enh_object~unlock( ).
      CATCH cx_enh_root cx_static_check.
        zcx_abapgit_exception=>raise( 'error deserializing ENHO wdyconf' ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_abapgit_object_enho~serialize.

    DATA: lo_wdyconf  TYPE REF TO cl_wdr_cfg_enhancement,
          lt_data     TYPE wdy_cfg_expl_data_tab,
          ls_outline  TYPE wdy_cfg_outline_data,
          ls_obj      TYPE wdy_config_key,
          li_document TYPE REF TO if_ixml_document,
          li_element  TYPE REF TO if_ixml_element.


    lo_wdyconf ?= ii_enh_tool.

    ls_obj = lo_wdyconf->get_original_object( ).
    ii_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    ii_xml->add( iv_name = 'ORIGINAL_OBJECT'
                 ig_data = ls_obj ).

* only works on new ABAP versions, parameters differ between versions
    CALL METHOD lo_wdyconf->('GET_ENHANCEMENT_DATA')
      EXPORTING
        p_scope    = 1
      IMPORTING
        p_enh_data = lt_data.

    CALL METHOD cl_wdr_cfg_persistence_utils=>('COMP_TABLES_TO_XML')
      EXPORTING
        outline_data  = ls_outline
        expl_data_tab = lt_data
      IMPORTING
        element       = li_element
      CHANGING
        document      = li_document.

    ii_xml->add_xml( iv_name = 'ENHANCEMENT_DATA'
                     ii_xml = li_element ).

  ENDMETHOD.

ENDCLASS.



CLASS zcl_abapgit_object_enho_fugr IMPLEMENTATION.


  METHOD constructor.
    ms_item = is_item.
    mo_files = io_files.
  ENDMETHOD.


  METHOD zif_abapgit_object_enho~deserialize.

    DATA: lo_fugrdata  TYPE REF TO cl_enh_tool_fugr,
          ls_enha_data TYPE enhfugrdata,
          li_tool      TYPE REF TO if_enh_tool,
          lv_tool      TYPE enhtooltype,
          lv_package   TYPE devclass.

    FIELD-SYMBOLS: <ls_fuba> TYPE enhfugrfuncdata.

    ii_xml->read(
      EXPORTING
        iv_name = 'TOOL'
      CHANGING
        cg_data = lv_tool ).

    ii_xml->read(
      EXPORTING
        iv_name = 'FUGRDATA'
      CHANGING
        cg_data = ls_enha_data ).

    lv_package = iv_package.

    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = |{ ms_item-obj_name }|
            enhtype     = ''
            enhtooltype = lv_tool
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).

        lo_fugrdata ?= li_tool.

        lo_fugrdata->set_fugr( ls_enha_data-fugr ).

        LOOP AT ls_enha_data-enh_fubas ASSIGNING <ls_fuba>.

          lo_fugrdata->set_func_data( func_name     = <ls_fuba>-fuba
                                      func_enhadata = <ls_fuba> ).

        ENDLOOP.

        lo_fugrdata->if_enh_object~save( run_dark = abap_true ).
        lo_fugrdata->if_enh_object~unlock( ).

      CATCH cx_enh_root.
        zcx_abapgit_exception=>raise( |error deserializing ENHO fugrdata { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object_enho~serialize.

    DATA: lo_fugrdata  TYPE REF TO cl_enh_tool_fugr,
          lv_fugr_name TYPE rs38l-area,
          ls_enha_data TYPE enhfugrdata.

    FIELD-SYMBOLS: <ls_docuobj> TYPE enhfugrparamdocu.


    lo_fugrdata ?= ii_enh_tool.

    lo_fugrdata->get_fugr( IMPORTING fugr_name = lv_fugr_name ).

    TRY.
        lo_fugrdata->get_all_data_for_fugr(
          EXPORTING
            fugr_name = lv_fugr_name
          IMPORTING
            enha_data = ls_enha_data ).

        LOOP AT ls_enha_data-docuobjs ASSIGNING <ls_docuobj>.
          CLEAR: <ls_docuobj>-shorttext,
                 <ls_docuobj>-longtext.
        ENDLOOP.

      CATCH cx_enh_not_found.
        zcx_abapgit_exception=>raise( |error deserializing ENHO fugrdata { ms_item-obj_name }| ).
    ENDTRY.

    ii_xml->add( iv_name = 'TOOL'
                 ig_data = lo_fugrdata->if_enh_tool~get_tool( ) ).

    ii_xml->add( iv_name = 'FUGRDATA'
                 ig_data = ls_enha_data ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_enho_wdyn IMPLEMENTATION.


  METHOD constructor.
    ms_item = is_item.
  ENDMETHOD.


  METHOD zif_abapgit_object_enho~deserialize.

    DATA: ls_enh_data  TYPE enhwdyn,
          li_tool      TYPE REF TO if_enh_tool,
          lo_wdyn      TYPE REF TO cl_enh_tool_wdy,
          lv_tool_type TYPE enhtooltype,
          lv_package   TYPE devclass.

    FIELD-SYMBOLS: <ls_controller_data> TYPE enhwdyc,
                   <ls_view_data>       TYPE enhwdyv.


    ii_xml->read(
      EXPORTING
        iv_name = 'TOOL'
      CHANGING
        cg_data = lv_tool_type ).

    ii_xml->read(
      EXPORTING
        iv_name = 'COMPONENT_DATA'
      CHANGING
        cg_data = ls_enh_data ).

    lv_package = iv_package.

    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = |{ ms_item-obj_name }|
            enhtype     = ''
            enhtooltype = lv_tool_type
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).

        lo_wdyn ?= li_tool.

        lo_wdyn->initialize( ls_enh_data-component_name ).

        lo_wdyn->set_component_data( ls_enh_data-component_data ).

        LOOP AT ls_enh_data-controller_data ASSIGNING <ls_controller_data>.

          lo_wdyn->set_controller_data( p_controller_name = <ls_controller_data>-controller_name
                                        p_enh_data        = <ls_controller_data> ).

        ENDLOOP.

        LOOP AT ls_enh_data-view_data ASSIGNING <ls_view_data>.

          lo_wdyn->set_view_data( p_view_name = <ls_view_data>-view_name
                                  p_enh_data  = <ls_view_data> ).

        ENDLOOP.

        lo_wdyn->if_enh_object~save( run_dark = abap_true ).
        lo_wdyn->if_enh_object~unlock( ).

      CATCH cx_root.
        zcx_abapgit_exception=>raise( |error deserializing ENHO wdyn { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object_enho~serialize.

    DATA: lo_wdyn           TYPE REF TO cl_enh_tool_wdy,
          lv_component_name TYPE wdy_component_name,
          ls_enh_data       TYPE enhwdyn.


    lo_wdyn ?= ii_enh_tool.
    lv_component_name = lo_wdyn->get_component_name( ).

    TRY.
        lo_wdyn->get_all_data_for_comp(
          EXPORTING
            p_component_name = lv_component_name
          IMPORTING
            p_enh_data       = ls_enh_data ).

        ii_xml->add( iv_name = 'TOOL'
                     ig_data = ii_enh_tool->get_tool( ) ).

        ii_xml->add( iv_name = 'COMPONENT_DATA'
                     ig_data = ls_enh_data ).

      CATCH cx_enh_not_found.
        zcx_abapgit_exception=>raise( |error serializing ENHO wdyn { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_enho IMPLEMENTATION.


  METHOD factory.

    CASE iv_tool.
      WHEN cl_enh_tool_badi_impl=>tooltype.
        CREATE OBJECT ri_enho TYPE zcl_abapgit_object_enho_badi
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN cl_enh_tool_hook_impl=>tooltype.
        CREATE OBJECT ri_enho TYPE zcl_abapgit_object_enho_hook
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN cl_enh_tool_class=>tooltype.
        CREATE OBJECT ri_enho TYPE zcl_abapgit_object_enho_class
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN cl_enh_tool_intf=>tooltype.
        CREATE OBJECT ri_enho TYPE zcl_abapgit_object_enho_intf
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN cl_wdr_cfg_enhancement=>tooltype.
        CREATE OBJECT ri_enho TYPE zcl_abapgit_object_enho_wdyc
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN 'FUGRENH'.
        CREATE OBJECT ri_enho TYPE zcl_abapgit_object_enho_fugr
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN 'WDYENH'.
        CREATE OBJECT ri_enho TYPE zcl_abapgit_object_enho_wdyn
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( |Unsupported ENHO type { iv_tool }| ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lv_enh_id   TYPE enhname,
          lt_log      TYPE enh_log_it,
          li_log_obj  TYPE REF TO if_enh_log,
          ls_enhlog   TYPE enhlog,
          lv_lines    TYPE i,
          lt_enhlog   TYPE STANDARD TABLE OF enhlog WITH DEFAULT KEY,
          li_enh_tool TYPE REF TO if_enh_tool.


    lv_enh_id = ms_item-obj_name.
    TRY.
        li_enh_tool = cl_enh_factory=>get_enhancement(
          enhancement_id   = lv_enh_id
          bypassing_buffer = abap_true ).
      CATCH cx_enh_root.
        rv_user = c_user_unknown.
        RETURN.
    ENDTRY.

    lt_log = li_enh_tool->get_log( ).

    LOOP AT lt_log INTO li_log_obj.
      ls_enhlog = li_log_obj->get_enhlog( ).
      APPEND ls_enhlog TO lt_enhlog.
    ENDLOOP.

    lv_lines = lines( lt_enhlog ).
    READ TABLE lt_enhlog INTO ls_enhlog INDEX lv_lines.
    IF sy-subrc = 0.
      rv_user = ls_enhlog-loguser.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_enh_id     TYPE enhname,
          li_enh_object TYPE REF TO if_enh_object.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_enh_id = ms_item-obj_name.
    TRY.
        li_enh_object = cl_enh_factory=>get_enhancement(
          enhancement_id = lv_enh_id
          lock           = abap_true ).
        li_enh_object->delete( nevertheless_delete = abap_true
                               run_dark            = abap_true ).
        li_enh_object->unlock( ).
      CATCH cx_enh_root.
        zcx_abapgit_exception=>raise( 'Error deleting ENHO' ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_tool     TYPE enhtooltype,
          li_enho     TYPE REF TO zif_abapgit_object_enho.

    IF zif_abapgit_object~exists( ) = abap_true.
      zif_abapgit_object~delete( iv_package ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TOOL'
                  CHANGING cg_data = lv_tool ).

    li_enho = factory( lv_tool ).

    li_enho->deserialize( ii_xml     = io_xml
                          iv_package = iv_package ).

    zcl_abapgit_sotr_handler=>create_sotr(
      iv_package = iv_package
      io_xml     = io_xml ).

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_enh_id TYPE enhname.


    lv_enh_id = ms_item-obj_name.
    TRY.
        cl_enh_factory=>get_enhancement(
          enhancement_id   = lv_enh_id
          bypassing_buffer = abap_true ).
        rv_bool = abap_true.
      CATCH cx_enh_root.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    DATA: lv_object TYPE seqg3-garg.

    lv_object = |{ ms_item-obj_type }{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_ENHANCE'
                                            iv_argument    = lv_object ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'ENHO'
        in_new_window = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_enh_id   TYPE enhname,
          li_enho     TYPE REF TO zif_abapgit_object_enho,
          li_enh_tool TYPE REF TO if_enh_tool.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_enh_id = ms_item-obj_name.
    TRY.
        li_enh_tool = cl_enh_factory=>get_enhancement(
          enhancement_id   = lv_enh_id
          bypassing_buffer = abap_true ).
      CATCH cx_enh_root.
        zcx_abapgit_exception=>raise( 'Error from CL_ENH_FACTORY' ).
    ENDTRY.

    li_enho = factory( li_enh_tool->get_tool( ) ).

    li_enho->serialize( ii_xml      = io_xml
                        ii_enh_tool = li_enh_tool ).

    zcl_abapgit_sotr_handler=>read_sotr(
      iv_pgmid    = 'R3TR'
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name
      io_xml      = io_xml
      iv_language = mv_language ).

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_ENHO_CLIF IMPLEMENTATION.


  METHOD deserialize.

    DATA: lt_tab_attributes TYPE enhclasstabattrib,
          lt_tab_types      TYPE enhtype_tab,
          lt_tab_methods    TYPE enhnewmeth_tab,
          ls_type_line      TYPE vseotype,
          ls_header         TYPE vseomethod,
          ls_param          TYPE vseomepara,
          ls_exc            TYPE vseoexcep,
          lt_tab_eventdata  TYPE enhevent_tab,
          ls_event_line     TYPE vseoevent,
          ls_event_param    TYPE vseoeparam.

    FIELD-SYMBOLS: <ls_type>        LIKE LINE OF lt_tab_types,
                   <ls_method>      LIKE LINE OF lt_tab_methods,
                   <ls_param>       LIKE LINE OF <ls_method>-meth_param,
                   <ls_event>       LIKE LINE OF lt_tab_eventdata,
                   <ls_exc>         LIKE LINE OF <ls_method>-meth_exc,
                   <ls_event_param> LIKE LINE OF <ls_event>-event_param.


    io_xml->read( EXPORTING iv_name = 'TAB_ATTRIBUTES'
                  CHANGING cg_data = lt_tab_attributes ).
    io_xml->read( EXPORTING iv_name = 'TAB_TYPES'
                  CHANGING cg_data = lt_tab_types ).
    io_xml->read( EXPORTING iv_name = 'TAB_METHODS'
                  CHANGING cg_data = lt_tab_methods ).
    io_xml->read( EXPORTING iv_name = 'TAB_EVENTDATA'
                  CHANGING cg_data = lt_tab_eventdata ).

    LOOP AT lt_tab_types ASSIGNING <ls_type>.
      MOVE-CORRESPONDING <ls_type> TO ls_type_line.
      TRY.
          io_clif->add_change_enha_type( type_line = ls_type_line ).
        CATCH cx_enh_mod_not_allowed
        cx_enh_is_not_enhanceable.
          " TODO
      ENDTRY.
    ENDLOOP.

    io_clif->set_enhattributes( lt_tab_attributes ).

* SAP standard SET_ENH_NEW_METHOS does not work

    LOOP AT lt_tab_methods ASSIGNING <ls_method>.

      MOVE-CORRESPONDING <ls_method>-meth_header TO ls_header.

      io_clif->add_change_new_enh_method(
        methkey       = <ls_method>-methkey
        method_header = ls_header ).

* parameters
      LOOP AT <ls_method>-meth_param ASSIGNING <ls_param>.
        MOVE-CORRESPONDING <ls_param> TO ls_param.
        io_clif->add_change_enh_methparam(
          methname   = <ls_method>-methkey-cmpname
          param_line = ls_param ).
      ENDLOOP.

* exceptions
      LOOP AT <ls_method>-meth_exc ASSIGNING <ls_exc>.
        MOVE-CORRESPONDING <ls_exc> TO ls_exc.
        io_clif->add_change_enh_methexc(
          methname    = <ls_method>-methkey-cmpname
          except_line = ls_exc ).
      ENDLOOP.

    ENDLOOP.

    " events are renumbered based on
    LOOP AT lt_tab_eventdata ASSIGNING <ls_event>.

      MOVE-CORRESPONDING <ls_event>-event_header TO ls_event_line.

      io_clif->add_change_enha_event(
        event_key  = <ls_event>-eventkey
        event_line = ls_event_line ).

* parameters
      LOOP AT <ls_event>-event_param ASSIGNING <ls_event_param>.
        MOVE-CORRESPONDING <ls_event_param> TO ls_event_param.
        io_clif->add_change_enh_eventparam(
          eventname   = <ls_event>-eventkey-cmpname
          event_param = ls_event_param ).
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD serialize.

    DATA: lt_tab_attributes TYPE enhclasstabattrib,
          lt_tab_types      TYPE enhtype_tab,
          lt_tab_methods    TYPE enhnewmeth_tab,
          lt_tab_eventdata  TYPE enhevent_tab,
          lv_editorder      TYPE i.

    FIELD-SYMBOLS: <ls_attr>        LIKE LINE OF lt_tab_attributes,
                   <ls_type>        LIKE LINE OF lt_tab_types,
                   <ls_meth>        LIKE LINE OF lt_tab_methods,
                   <ls_param>       LIKE LINE OF <ls_meth>-meth_param,
                   <ls_event>       LIKE LINE OF lt_tab_eventdata,
                   <ls_event_param> LIKE LINE OF <ls_event>-event_param.


    io_clif->get_enhattributes( IMPORTING tab_attributes = lt_tab_attributes ).

    io_clif->get_enhatypes( IMPORTING tab_types = lt_tab_types ).

    io_clif->get_enh_new_methodes( IMPORTING tab_methodes = lt_tab_methods ).

    io_clif->get_enhevents( IMPORTING tab_eventdata = lt_tab_eventdata ).

    LOOP AT lt_tab_attributes ASSIGNING <ls_attr>.
      CLEAR: <ls_attr>-author,
             <ls_attr>-createdon,
             <ls_attr>-changedby,
             <ls_attr>-changedon,
             <ls_attr>-descript_id.
    ENDLOOP.

    LOOP AT lt_tab_types ASSIGNING <ls_type>.
      CLEAR: <ls_type>-author,
             <ls_type>-createdon,
             <ls_type>-changedby,
             <ls_type>-changedon,
             <ls_type>-descript_id.
    ENDLOOP.

    lv_editorder = 0.
    SORT lt_tab_methods BY meth_header-editorder.
    LOOP AT lt_tab_methods ASSIGNING <ls_meth>.
      CLEAR: <ls_meth>-meth_header-author,
             <ls_meth>-meth_header-createdon,
             <ls_meth>-meth_header-changedby,
             <ls_meth>-meth_header-changedon,
             <ls_meth>-meth_header-descript_id.
      lv_editorder = lv_editorder + 1.
      <ls_meth>-meth_header-editorder = lv_editorder.
      LOOP AT <ls_meth>-meth_param ASSIGNING <ls_param>.
        CLEAR: <ls_param>-author,
               <ls_param>-createdon,
               <ls_param>-changedby,
               <ls_param>-changedon,
               <ls_param>-descript_id.
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_tab_eventdata ASSIGNING <ls_event>.
      CLEAR: <ls_event>-event_header-author,
             <ls_event>-event_header-createdon,
             <ls_event>-event_header-changedby,
             <ls_event>-event_header-changedon,
             <ls_event>-event_header-descript_id.
      LOOP AT <ls_event>-event_param ASSIGNING <ls_event_param>.
        CLEAR: <ls_event_param>-author,
               <ls_event_param>-createdon,
               <ls_event_param>-changedby,
               <ls_event_param>-changedon,
               <ls_event_param>-descript_id.
      ENDLOOP.
    ENDLOOP.

    io_xml->add( iv_name = 'TAB_ATTRIBUTES'
                 ig_data = lt_tab_attributes ).
    io_xml->add( iv_name = 'TAB_TYPES'
                 ig_data = lt_tab_types ).
    io_xml->add( iv_name = 'TAB_METHODS'
                 ig_data = lt_tab_methods ).
    io_xml->add( iv_name = 'TAB_EVENTDATA'
                 ig_data = lt_tab_eventdata ).

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_ENHS_BADI_D IMPLEMENTATION.


  METHOD zif_abapgit_object_enhs~deserialize.

    DATA: lv_parent          TYPE enhspotcompositename,
          lt_enh_badi        TYPE enh_badi_data_it,
          lo_badidef_tool    TYPE REF TO cl_enh_tool_badi_def,
          lv_enh_shorttext   TYPE string,
          li_enh_object      TYPE REF TO if_enh_object,
          li_enh_object_docu TYPE REF TO if_enh_object_docu,
          lv_text            TYPE string,
          lx_error           TYPE REF TO cx_enh_root.

    FIELD-SYMBOLS: <ls_enh_badi> LIKE LINE OF lt_enh_badi.

    ii_xml->read( EXPORTING iv_name = 'PARENT_COMP'
                  CHANGING  cg_data = lv_parent ).

    ii_xml->read( EXPORTING iv_name = 'BADI_DATA'
                  CHANGING  cg_data = lt_enh_badi ).

    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING  cg_data = lv_enh_shorttext ).

    li_enh_object ?= ii_enh_spot_tool.
    li_enh_object_docu ?= ii_enh_spot_tool.

    TRY.
        li_enh_object_docu->set_shorttext( lv_enh_shorttext ).

        lo_badidef_tool ?= ii_enh_spot_tool.

        LOOP AT lt_enh_badi ASSIGNING <ls_enh_badi>.
          lo_badidef_tool->add_badi_def( <ls_enh_badi> ).
        ENDLOOP.

        li_enh_object->save( ).
        li_enh_object->activate( ).
        li_enh_object->unlock( ).

      CATCH cx_enh_root INTO lx_error.
        lv_text = lx_error->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object_enhs~serialize.

    DATA: lv_spot_name       TYPE enhspotname,
          lv_parent          TYPE enhspotcompositename,
          lt_enh_badi        TYPE enh_badi_data_it,
          lo_badidef_tool    TYPE REF TO cl_enh_tool_badi_def,
          lv_enh_shorttext   TYPE string,
          li_enh_object_docu TYPE REF TO if_enh_object_docu.

    lo_badidef_tool ?= ii_enh_spot_tool.

    li_enh_object_docu ?= ii_enh_spot_tool.
    lv_enh_shorttext = li_enh_object_docu->get_shorttext( ).

    "get parent = composite enhs (ENHC)
    lv_parent = cl_r3standard_persistence=>enh_find_parent_composite( lv_spot_name ).
    "get subsequent BADI definitions
    lt_enh_badi = lo_badidef_tool->get_badi_defs( ).

    ii_xml->add( ig_data = ii_enh_spot_tool->get_tool( )
                 iv_name = 'TOOL' ).

    ii_xml->add( ig_data = lv_enh_shorttext
                 iv_name = 'SHORTTEXT' ).

    ii_xml->add( ig_data = lv_parent
                 iv_name = 'PARENT_COMP' ).

    ii_xml->add( ig_data = lt_enh_badi
                 iv_name = 'BADI_DATA' ).

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_ENHS_HOOK_D IMPLEMENTATION.


  METHOD zif_abapgit_object_enhs~deserialize.

    DATA: lv_enh_shorttext       TYPE string,
          ls_enh_hook_definition TYPE enh_hook_def,
          ls_hook_definition     TYPE ty_hook_defifnition,
          li_enh_object          TYPE REF TO if_enh_object,
          li_enh_object_docu     TYPE REF TO if_enh_object_docu,
          lo_hookdef_tool        TYPE REF TO cl_enh_tool_hook_def,
          lx_error               TYPE REF TO cx_enh_root,
          lv_text                TYPE string.

    FIELD-SYMBOLS: <ls_hook_definition> TYPE enh_hook_def_ext.

    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING  cg_data = lv_enh_shorttext ).

    ii_xml->read( EXPORTING iv_name = 'BADI_DATA'
                  CHANGING  cg_data = ls_hook_definition ).

    li_enh_object ?= ii_enh_spot_tool.
    li_enh_object_docu ?= ii_enh_spot_tool.

    TRY.
        li_enh_object_docu->set_shorttext( lv_enh_shorttext ).

        lo_hookdef_tool ?= ii_enh_spot_tool.

        lo_hookdef_tool->set_original_object( pgmid     = ls_hook_definition-pgmid
                                              obj_name  = ls_hook_definition-obj_name
                                              obj_type  = ls_hook_definition-obj_type
                                              program   = ls_hook_definition-program
                                              main_type = ls_hook_definition-main_type
                                              main_name = ls_hook_definition-main_name ).

        LOOP AT ls_hook_definition-def_hooks ASSIGNING <ls_hook_definition>.
          MOVE-CORRESPONDING <ls_hook_definition> TO ls_enh_hook_definition.
          lo_hookdef_tool->add_hook_def( ls_enh_hook_definition ).
        ENDLOOP.

        li_enh_object->save( ).
        li_enh_object->activate( ).
        li_enh_object->unlock( ).

      CATCH cx_enh_root INTO lx_error.
        lv_text = lx_error->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object_enhs~serialize.

    DATA: lo_hookdef_tool    TYPE REF TO cl_enh_tool_hook_def,
          lv_enh_shorttext   TYPE string,
          li_enh_object_docu TYPE REF TO if_enh_object_docu,
          ls_hook_definition TYPE ty_hook_defifnition.

    lo_hookdef_tool ?= ii_enh_spot_tool.

    li_enh_object_docu ?= ii_enh_spot_tool.
    lv_enh_shorttext = li_enh_object_docu->get_shorttext( ).

    ls_hook_definition-def_hooks = lo_hookdef_tool->get_hook_defs( ).

    lo_hookdef_tool->get_original_object(
      IMPORTING
        pgmid     = ls_hook_definition-pgmid
        obj_name  = ls_hook_definition-obj_name
        obj_type  = ls_hook_definition-obj_type
        main_type = ls_hook_definition-main_type
        main_name = ls_hook_definition-main_name
        program   = ls_hook_definition-program ).

    ii_xml->add( ig_data = ii_enh_spot_tool->get_tool( )
                 iv_name = 'TOOL' ).

    ii_xml->add( ig_data = lv_enh_shorttext
                 iv_name = 'SHORTTEXT' ).

    ii_xml->add( ig_data = ls_hook_definition
                 iv_name = 'BADI_DATA' ).

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_ENHS IMPLEMENTATION.


  METHOD factory.

    CASE iv_tool.
      WHEN cl_enh_tool_badi_def=>tooltype.
        CREATE OBJECT ri_enho TYPE zcl_abapgit_object_enhs_badi_d.
      WHEN cl_enh_tool_hook_def=>tool_type.
        CREATE OBJECT ri_enho TYPE zcl_abapgit_object_enhs_hook_d.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( |ENHS: Unsupported tool { iv_tool }| ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lv_spot_name TYPE enhspotname,
          li_spot_ref  TYPE REF TO if_enh_spot_tool.

    lv_spot_name = ms_item-obj_name.

    TRY.
        li_spot_ref = cl_enh_factory=>get_enhancement_spot( lv_spot_name ).
        li_spot_ref->get_attributes( IMPORTING changedby = rv_user ).

      CATCH cx_enh_root.
        rv_user = c_user_unknown.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_spot_name  TYPE enhspotname,
          lx_enh_root   TYPE REF TO cx_enh_root,
          li_enh_object TYPE REF TO if_enh_object.

    lv_spot_name  = ms_item-obj_name.

    TRY.
        li_enh_object ?= cl_enh_factory=>get_enhancement_spot( spot_name = lv_spot_name
                                                               lock      = abap_true ).

        li_enh_object->delete( nevertheless_delete = abap_true
                               run_dark            = abap_true ).

        li_enh_object->unlock( ).

      CATCH cx_enh_root INTO lx_enh_root.
        zcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_parent    TYPE enhspotcompositename,
          lv_spot_name TYPE enhspotname,
          lv_tool      TYPE enhspottooltype,
          lv_package   LIKE iv_package,
          lx_enh_root  TYPE REF TO cx_enh_root,
          li_spot_ref  TYPE REF TO if_enh_spot_tool,
          li_enhs      TYPE REF TO zif_abapgit_object_enhs.

    IF zif_abapgit_object~exists( ) = abap_true.
      zif_abapgit_object~delete( iv_package ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TOOL'
                  CHANGING  cg_data = lv_tool ).

    lv_spot_name = ms_item-obj_name.
    lv_package   = iv_package.

    TRY.
        cl_enh_factory=>create_enhancement_spot(
          EXPORTING
            spot_name      = lv_spot_name
            tooltype       = lv_tool
            dark           = abap_false
            compositename  = lv_parent
          IMPORTING
            spot           = li_spot_ref
          CHANGING
            devclass       = lv_package ).

      CATCH cx_enh_root INTO lx_enh_root.
        zcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

    li_enhs = factory( lv_tool ).

    li_enhs->deserialize( ii_xml           = io_xml
                          iv_package       = iv_package
                          ii_enh_spot_tool = li_spot_ref ).

    zcl_abapgit_sotr_handler=>create_sotr(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_spot_name TYPE enhspotname,
          li_spot_ref  TYPE REF TO if_enh_spot_tool.

    lv_spot_name = ms_item-obj_name.

    TRY.
        li_spot_ref = cl_enh_factory=>get_enhancement_spot( lv_spot_name ).

        rv_bool = abap_true.

      CATCH cx_enh_root.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'ENHS'
        in_new_window = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_spot_name TYPE enhspotname,
          li_spot_ref  TYPE REF TO if_enh_spot_tool,
          li_enhs      TYPE REF TO zif_abapgit_object_enhs,
          lx_enh_root  TYPE REF TO cx_enh_root.

    lv_spot_name = ms_item-obj_name.

    TRY.
        li_spot_ref = cl_enh_factory=>get_enhancement_spot( lv_spot_name ).

      CATCH cx_enh_root INTO lx_enh_root.
        zcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

    li_enhs = factory( li_spot_ref->get_tool( ) ).

    li_enhs->serialize( ii_xml           = io_xml
                        ii_enh_spot_tool = li_spot_ref ).

    zcl_abapgit_sotr_handler=>read_sotr(
      iv_pgmid    = 'R3TR'
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name
      io_xml      = io_xml
      iv_language = mv_language ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_enqu IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd25l
      INTO rv_user
      WHERE viewname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers  = '0000'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    delete_ddic( 'L' ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd25v TYPE dd25v,
          lt_dd26e TYPE TABLE OF dd26e,
          lt_dd27p TYPE ty_dd27p.


    io_xml->read( EXPORTING iv_name = 'DD25V'
                  CHANGING cg_data = ls_dd25v ).
    io_xml->read( EXPORTING iv_name = 'DD26E_TABLE'
                  CHANGING cg_data = lt_dd26e ).
    io_xml->read( EXPORTING iv_name = 'DD27P_TABLE'
                  CHANGING cg_data = lt_dd27p ).

    corr_insert( iv_package = iv_package
                 ig_object_class = 'DICT' ).

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_ENQU_PUT'
      EXPORTING
        name              = lv_name
        dd25v_wa          = ls_dd25v
      TABLES
        dd26e_tab         = lt_dd26e
        dd27p_tab         = lt_dd27p
      EXCEPTIONS
        enqu_not_found    = 1
        name_inconsistent = 2
        enqu_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_viewname TYPE dd25l-viewname.

    SELECT SINGLE viewname FROM dd25l INTO lv_viewname
      WHERE viewname = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-ddic = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    jump_se11( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd25v TYPE dd25v,
          lt_dd26e TYPE TABLE OF dd26e,
          lt_dd27p TYPE ty_dd27p.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_ENQU_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = mv_language
      IMPORTING
        dd25v_wa      = ls_dd25v
      TABLES
        dd26e_tab     = lt_dd26e
        dd27p_tab     = lt_dd27p
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF ls_dd25v IS INITIAL.
      zcx_abapgit_exception=>raise( |No active version found for { ms_item-obj_type } { ms_item-obj_name }| ).
    ENDIF.

    CLEAR: ls_dd25v-as4user,
           ls_dd25v-as4date,
           ls_dd25v-as4time,
           ls_dd25v-as4local,
           ls_dd25v-as4vers.

    _clear_dd27p_fields( CHANGING ct_dd27p = lt_dd27p ).

    io_xml->add( iv_name = 'DD25V'
                 ig_data = ls_dd25v ).
    io_xml->add( ig_data = lt_dd26e
                 iv_name = 'DD26E_TABLE' ).
    io_xml->add( ig_data = lt_dd27p
                 iv_name = 'DD27P_TABLE' ).

  ENDMETHOD.


  METHOD _clear_dd27p_fields.

    FIELD-SYMBOLS <ls_dd27p> TYPE dd27p.

    LOOP AT ct_dd27p ASSIGNING <ls_dd27p>.
      "taken from table
      CLEAR <ls_dd27p>-headlen.
      CLEAR <ls_dd27p>-scrlen1.
      CLEAR <ls_dd27p>-scrlen2.
      CLEAR <ls_dd27p>-scrlen3.
      CLEAR <ls_dd27p>-intlen.
      CLEAR <ls_dd27p>-outputlen.
      CLEAR <ls_dd27p>-flength.
      CLEAR <ls_dd27p>-ddtext.
      CLEAR <ls_dd27p>-reptext.
      CLEAR <ls_dd27p>-scrtext_s.
      CLEAR <ls_dd27p>-scrtext_m.
      CLEAR <ls_dd27p>-scrtext_l.
      CLEAR <ls_dd27p>-rollname.
      CLEAR <ls_dd27p>-rollnamevi.
      CLEAR <ls_dd27p>-entitytab.
      CLEAR <ls_dd27p>-datatype.
      CLEAR <ls_dd27p>-inttype.
      CLEAR <ls_dd27p>-ddlanguage.
      CLEAR <ls_dd27p>-domname.
      CLEAR <ls_dd27p>-signflag.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_fugr IMPLEMENTATION.


  METHOD check_rfc_parameters.

* function module RS_FUNCTIONMODULE_INSERT does the same deep down, but the right error
* message is not returned to the user, this is a workaround to give a proper error
* message to the user

    DATA: ls_parameter TYPE rsfbpara,
          lt_fupa      TYPE rsfb_param,
          ls_fupa      LIKE LINE OF lt_fupa.


    IF is_function-remote_call = 'R'.
      cl_fb_parameter_conversion=>convert_parameter_old_to_fupa(
        EXPORTING
          functionname = is_function-funcname
          import       = is_function-import
          export       = is_function-export
          change       = is_function-changing
          tables       = is_function-tables
          except       = is_function-exception
        IMPORTING
          fupararef    = lt_fupa ).

      LOOP AT lt_fupa INTO ls_fupa WHERE paramtype = 'I' OR paramtype = 'E' OR paramtype = 'C' OR paramtype = 'T'.
        cl_fb_parameter_conversion=>convert_intern_to_extern(
          EXPORTING
            parameter_db  = ls_fupa
          IMPORTING
            parameter_vis = ls_parameter ).

        CALL FUNCTION 'RS_FB_CHECK_PARAMETER_REMOTE'
          EXPORTING
            parameter             = ls_parameter
            basxml_enabled        = is_function-remote_basxml
          EXCEPTIONS
            not_remote_compatible = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise_t100( ).
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD deserialize_functions.

    DATA: lv_include   TYPE rs38l-include,
          lv_area      TYPE rs38l-area,
          lv_group     TYPE rs38l-area,
          lv_namespace TYPE rs38l-namespace,
          lt_source    TYPE TABLE OF abaptxt255,
          lv_msg       TYPE string,
          lx_error     TYPE REF TO zcx_abapgit_exception,
          lv_corrnum   TYPE e070use-ordernum.

    FIELD-SYMBOLS: <ls_func> LIKE LINE OF it_functions.


    lv_corrnum = zcl_abapgit_default_transport=>get_instance( )->get( )-ordernum.

    LOOP AT it_functions ASSIGNING <ls_func>.

      lt_source = mo_files->read_abap( iv_extra = <ls_func>-funcname ).

      lv_area = ms_item-obj_name.

      CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
        EXPORTING
          complete_area = lv_area
        IMPORTING
          namespace     = lv_namespace
          group         = lv_group
        EXCEPTIONS
          OTHERS        = 12.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
        ii_log->add_error( iv_msg  = |Function module { <ls_func>-funcname }: { lv_msg }|
                           is_item = ms_item ).
        CONTINUE. "with next function module
      ENDIF.

      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname           = <ls_func>-funcname
        IMPORTING
          include            = lv_include
        EXCEPTIONS
          function_not_exist = 1.
      IF sy-subrc = 0.
* delete the function module to make sure the parameters are updated
* havent found a nice way to update the paramters
        CALL FUNCTION 'FUNCTION_DELETE'
          EXPORTING
            funcname                 = <ls_func>-funcname
            suppress_success_message = abap_true
          EXCEPTIONS
            error_message            = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
          ii_log->add_error( iv_msg = |Function module { <ls_func>-funcname }: { lv_msg }|
                             is_item = ms_item ).
          CONTINUE. "with next function module
        ENDIF.
      ENDIF.

      TRY.
          check_rfc_parameters( <ls_func> ).
        CATCH zcx_abapgit_exception INTO lx_error.
          ii_log->add_error(
            iv_msg  = |Function module { <ls_func>-funcname }: { lx_error->get_text( ) }|
            is_item = ms_item ).
          CONTINUE. "with next function module
      ENDTRY.

      CALL FUNCTION 'RS_FUNCTIONMODULE_INSERT'
        EXPORTING
          funcname                = <ls_func>-funcname
          function_pool           = lv_group
          interface_global        = <ls_func>-global_flag
          remote_call             = <ls_func>-remote_call
          short_text              = <ls_func>-short_text
          update_task             = <ls_func>-update_task
          exception_class         = <ls_func>-exception_classes
          namespace               = lv_namespace
          remote_basxml_supported = <ls_func>-remote_basxml
          corrnum                 = lv_corrnum
        IMPORTING
          function_include        = lv_include
        TABLES
          import_parameter        = <ls_func>-import
          export_parameter        = <ls_func>-export
          tables_parameter        = <ls_func>-tables
          changing_parameter      = <ls_func>-changing
          exception_list          = <ls_func>-exception
          parameter_docu          = <ls_func>-documentation
        EXCEPTIONS
          double_task             = 1
          error_message           = 2
          function_already_exists = 3
          invalid_function_pool   = 4
          invalid_name            = 5
          too_many_functions      = 6
          no_modify_permission    = 7
          no_show_permission      = 8
          enqueue_system_failure  = 9
          canceled_in_corr        = 10
          OTHERS                  = 11.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
        ii_log->add_error( iv_msg = |Function module { <ls_func>-funcname }: { lv_msg }|
                           is_item = ms_item ).
        CONTINUE.  "with next function module
      ENDIF.

      INSERT REPORT lv_include FROM lt_source.
      ii_log->add_success( iv_msg = |Function module { <ls_func>-funcname } imported|
                           is_item = ms_item ).
    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize_includes.

    DATA: lo_xml       TYPE REF TO zif_abapgit_xml_input,
          ls_progdir   TYPE ty_progdir,
          lt_includes  TYPE ty_sobj_name_tt,
          lt_tpool     TYPE textpool_table,
          lt_tpool_ext TYPE zif_abapgit_definitions=>ty_tpool_tt,
          lt_source    TYPE TABLE OF abaptxt255,
          lx_exc       TYPE REF TO zcx_abapgit_exception.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF lt_includes.


    tadir_insert( iv_package ).

    ii_xml->read( EXPORTING iv_name = 'INCLUDES'
                  CHANGING cg_data = lt_includes ).

    LOOP AT lt_includes ASSIGNING <lv_include>.

      "ignore simple transformation includes (as long as they remain in existing repositories)
      IF strlen( <lv_include> ) = 33 AND <lv_include>+30(3) = 'XTI'.
        ii_log->add_warning( iv_msg = |Simple Transformation include { <lv_include> } ignored|
                             is_item = ms_item ).
        CONTINUE.
      ENDIF.

      TRY.
          lt_source = mo_files->read_abap( iv_extra = <lv_include> ).

          lo_xml = mo_files->read_xml( <lv_include> ).

          lo_xml->read( EXPORTING iv_name = 'PROGDIR'
                        CHANGING cg_data = ls_progdir ).

          lo_xml->read( EXPORTING iv_name = 'TPOOL'
                        CHANGING cg_data = lt_tpool_ext ).
          lt_tpool = read_tpool( lt_tpool_ext ).

          deserialize_program( is_progdir = ls_progdir
                               it_source  = lt_source
                               it_tpool   = lt_tpool
                               iv_package = iv_package ).

          deserialize_textpool( iv_program    = <lv_include>
                                it_tpool      = lt_tpool
                                iv_is_include = abap_true ).

          ii_log->add_success( iv_msg = |Include { ls_progdir-name } imported|
                               is_item = ms_item ).

        CATCH zcx_abapgit_exception INTO lx_exc.
          ii_log->add_exception( ix_exc = lx_exc
                                 is_item = ms_item ).
          CONTINUE.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize_texts.
    DATA: lt_tpool_i18n TYPE ty_tpools_i18n,
          lt_tpool      TYPE textpool_table.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF lt_tpool_i18n.
    ii_xml->read( EXPORTING iv_name = 'I18N_TPOOL'
                  CHANGING  cg_data = lt_tpool_i18n ).

    LOOP AT lt_tpool_i18n ASSIGNING <ls_tpool>.
      lt_tpool = read_tpool( <ls_tpool>-textpool ).
      deserialize_textpool( iv_program  = iv_prog_name
                            iv_language = <ls_tpool>-language
                            it_tpool    = lt_tpool ).
    ENDLOOP.
  ENDMETHOD.


  METHOD deserialize_xml.

    DATA: lv_complete     TYPE rs38l-area,
          lv_namespace    TYPE rs38l-namespace,
          lv_areat        TYPE tlibt-areat,
          lv_stext        TYPE tftit-stext,
          lv_group        TYPE rs38l-area,
          lv_abap_version TYPE trdir-uccheck,
          lv_corrnum      TYPE e070use-ordernum.

    lv_abap_version = get_abap_version( ii_xml ).
    lv_complete = ms_item-obj_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        complete_area                = lv_complete
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
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ii_xml->read( EXPORTING iv_name = 'AREAT'
                  CHANGING cg_data = lv_areat ).
    lv_stext = lv_areat.
    lv_corrnum = zcl_abapgit_default_transport=>get_instance( )->get( )-ordernum.

    CALL FUNCTION 'RS_FUNCTION_POOL_INSERT'
      EXPORTING
        function_pool           = lv_group
        short_text              = lv_stext
        namespace               = lv_namespace
        devclass                = iv_package
        unicode_checks          = lv_abap_version
        corrnum                 = lv_corrnum
        suppress_corr_check     = abap_false
      EXCEPTIONS
        name_already_exists     = 1
        name_not_correct        = 2
        function_already_exists = 3
        invalid_function_pool   = 4
        invalid_name            = 5
        too_many_functions      = 6
        no_modify_permission    = 7
        no_show_permission      = 8
        enqueue_system_failure  = 9
        canceled_in_corr        = 10
        undefined_error         = 11
        OTHERS                  = 12.

    CASE sy-subrc.
      WHEN 0.
        " Everything is ok
      WHEN 1 OR 3.
        " If the function group exists we need to manually update the short text
        update_func_group_short_text( iv_group      = lv_group
                                      iv_short_text = lv_stext ).
      WHEN OTHERS.
        zcx_abapgit_exception=>raise_t100( ).
    ENDCASE.

  ENDMETHOD.


  METHOD functions.

    DATA: lv_area TYPE rs38l-area.
    FIELD-SYMBOLS: <ls_functab> TYPE LINE OF ty_rs38l_incl_tt.

    lv_area = ms_item-obj_name.


    CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
      EXPORTING
        function_pool           = lv_area
      TABLES
        functab                 = rt_functab
      EXCEPTIONS
        function_pool_not_found = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

* The result can also contain function which are lowercase.
    LOOP AT rt_functab ASSIGNING <ls_functab>.
      TRANSLATE <ls_functab> TO UPPER CASE.
    ENDLOOP.

    SORT rt_functab BY funcname ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_functab COMPARING funcname.

  ENDMETHOD.


  METHOD get_abap_version.

    DATA: lt_includes TYPE ty_sobj_name_tt,
          ls_progdir  TYPE ty_progdir,
          lo_xml      TYPE REF TO zif_abapgit_xml_input.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF lt_includes.

    ii_xml->read( EXPORTING iv_name = 'INCLUDES'
                  CHANGING cg_data = lt_includes ).

    LOOP AT lt_includes ASSIGNING <lv_include>.

      lo_xml = mo_files->read_xml( <lv_include> ).

      lo_xml->read( EXPORTING iv_name = 'PROGDIR'
                    CHANGING cg_data = ls_progdir ).

      IF ls_progdir-uccheck IS INITIAL.
        CONTINUE.
      ELSEIF rv_abap_version IS INITIAL.
        rv_abap_version = ls_progdir-uccheck.
        CONTINUE.
      ELSEIF rv_abap_version <> ls_progdir-uccheck.
*** All includes need to have the same ABAP language version
        zcx_abapgit_exception=>raise( 'different ABAP Language Versions' ).
      ENDIF.
    ENDLOOP.

    IF rv_abap_version IS INITIAL.
      rv_abap_version = 'X'.
    ENDIF.

  ENDMETHOD.


  METHOD includes.

    TYPES: BEGIN OF ty_reposrc,
             progname TYPE reposrc-progname,
           END OF ty_reposrc.

    DATA: lt_reposrc        TYPE STANDARD TABLE OF ty_reposrc WITH DEFAULT KEY,
          ls_reposrc        LIKE LINE OF lt_reposrc,
          lv_program        TYPE program,
          lv_maintviewname  LIKE LINE OF rt_includes,
          lv_offset_ns      TYPE i,
          lv_tabix          LIKE sy-tabix,
          lt_functab        TYPE ty_rs38l_incl_tt,
          lt_tadir_includes TYPE HASHED TABLE OF objname WITH UNIQUE KEY table_line.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF rt_includes,
                   <ls_func>    LIKE LINE OF lt_functab.


    IF lines( mt_includes_cache ) > 0.
      rt_includes = mt_includes_cache.
      RETURN.
    ENDIF.

    lv_program = main_name( ).
    lt_functab = functions( ).

    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program      = lv_program
      TABLES
        includetab   = rt_includes
      EXCEPTIONS
        not_existent = 1
        no_program   = 2
        OTHERS       = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from RS_GET_ALL_INCLUDES' ).
    ENDIF.

    LOOP AT lt_functab ASSIGNING <ls_func>.
      DELETE TABLE rt_includes FROM <ls_func>-include.
    ENDLOOP.

* handle generated maintenance views
    IF ms_item-obj_name(1) <> '/'.
      "FGroup name does not contain a namespace
      lv_maintviewname = |L{ ms_item-obj_name }T00|.
    ELSE.
      "FGroup name contains a namespace
      lv_offset_ns = find( val = ms_item-obj_name+1
                           sub = '/' ).
      lv_offset_ns = lv_offset_ns + 2.
      lv_maintviewname = |{ ms_item-obj_name(lv_offset_ns) }L{ ms_item-obj_name+lv_offset_ns }T00|.
    ENDIF.

    READ TABLE rt_includes WITH KEY table_line = lv_maintviewname TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND lv_maintviewname TO rt_includes.
    ENDIF.

    IF lines( rt_includes ) > 0.
      " check which includes have their own tadir entry
      " these includes might reside in a different package or might be shared between multiple function groups
      " or other programs and are hence no part of the to serialized FUGR object
      " they will be handled as individual objects when serializing their package
      " in addition, referenced XTI includes referencing (simple) transformations must be ignored
      SELECT obj_name
        INTO TABLE lt_tadir_includes
        FROM tadir
        FOR ALL ENTRIES IN rt_includes
        WHERE pgmid      = 'R3TR'
              AND object = 'PROG'
              AND obj_name = rt_includes-table_line.
      LOOP AT rt_includes ASSIGNING <lv_include>.
        " skip autogenerated includes from Table Maintenance Generator
        IF <lv_include> CP 'LSVIM*'.
          DELETE rt_includes INDEX sy-tabix.
          CONTINUE.
        ENDIF.
        READ TABLE lt_tadir_includes WITH KEY table_line = <lv_include> TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          DELETE rt_includes.
          CONTINUE.
        ENDIF.
        IF strlen( <lv_include> ) = 33 AND <lv_include>+30(3) = 'XTI'.
          "ignore referenced (simple) transformation includes
          DELETE rt_includes.
          CONTINUE.
        ENDIF.
      ENDLOOP.

      IF lines( rt_includes ) > 0.
        SELECT progname FROM reposrc
          INTO TABLE lt_reposrc
          FOR ALL ENTRIES IN rt_includes
          WHERE progname = rt_includes-table_line
          AND r3state = 'A'.
      ENDIF.
      SORT lt_reposrc BY progname ASCENDING.
    ENDIF.

    LOOP AT rt_includes ASSIGNING <lv_include>.
      lv_tabix = sy-tabix.

* make sure the include exists
      READ TABLE lt_reposrc INTO ls_reposrc
        WITH KEY progname = <lv_include> BINARY SEARCH.
      IF sy-subrc <> 0.
        DELETE rt_includes INDEX lv_tabix.
      ENDIF.

    ENDLOOP.

    APPEND lv_program TO rt_includes.

    mt_includes_cache = rt_includes.

  ENDMETHOD.


  METHOD is_any_function_module_locked.

    DATA: lt_functions TYPE ty_rs38l_incl_tt.

    FIELD-SYMBOLS: <ls_function> TYPE rs38l_incl.

    TRY.
        lt_functions = functions( ).
      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

    LOOP AT lt_functions ASSIGNING <ls_function>.

      IF exists_a_lock_entry_for( iv_lock_object = 'ESFUNCTION'
                                  iv_argument    = |{ <ls_function>-funcname }| ) = abap_true.
        rv_any_function_module_locked = abap_true.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD is_any_include_locked.

    DATA: lt_includes TYPE ty_sobj_name_tt.
    FIELD-SYMBOLS: <lv_include> TYPE sobj_name.

    TRY.
        lt_includes = includes( ).
      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

    LOOP AT lt_includes ASSIGNING <lv_include>.

      IF exists_a_lock_entry_for( iv_lock_object = 'ESRDIRE'
                                  iv_argument    = |{ <lv_include> }| ) = abap_true.
        rv_is_any_include_locked = abap_true.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD is_function_group_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = |FG{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_functions_group_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                                            iv_argument    = lv_object ).

  ENDMETHOD.


  METHOD main_name.

    DATA: lv_area      TYPE rs38l-area,
          lv_namespace TYPE rs38l-namespace,
          lv_group     TYPE rs38l-area.


    lv_area = ms_item-obj_name.

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
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CONCATENATE lv_namespace 'SAPL' lv_group INTO rv_program.

  ENDMETHOD.


  METHOD serialize_functions.

    DATA:
      lt_source     TYPE TABLE OF rssource,
      lt_functab    TYPE ty_rs38l_incl_tt,
      lt_new_source TYPE rsfb_source,
      ls_function   LIKE LINE OF rt_functions.

    FIELD-SYMBOLS: <ls_func>          LIKE LINE OF lt_functab,
                   <ls_documentation> TYPE LINE OF ty_function-documentation.

    lt_functab = functions( ).

    LOOP AT lt_functab ASSIGNING <ls_func>.
* fm RPY_FUNCTIONMODULE_READ does not support source code
* lines longer than 72 characters
      CLEAR ls_function.
      MOVE-CORRESPONDING <ls_func> TO ls_function.

      CLEAR lt_new_source.
      CLEAR lt_source.

      CALL FUNCTION 'RPY_FUNCTIONMODULE_READ_NEW'
        EXPORTING
          functionname            = <ls_func>-funcname
        IMPORTING
          global_flag             = ls_function-global_flag
          remote_call             = ls_function-remote_call
          update_task             = ls_function-update_task
          short_text              = ls_function-short_text
          remote_basxml_supported = ls_function-remote_basxml
        TABLES
          import_parameter        = ls_function-import
          changing_parameter      = ls_function-changing
          export_parameter        = ls_function-export
          tables_parameter        = ls_function-tables
          exception_list          = ls_function-exception
          documentation           = ls_function-documentation
          source                  = lt_source
        CHANGING
          new_source              = lt_new_source
        EXCEPTIONS
          error_message           = 1
          function_not_found      = 2
          invalid_name            = 3
          OTHERS                  = 4.
      IF sy-subrc = 2.
        CONTINUE.
      ELSEIF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Error from RPY_FUNCTIONMODULE_READ_NEW' ).
      ENDIF.

      LOOP AT ls_function-documentation ASSIGNING <ls_documentation>.
        CLEAR <ls_documentation>-index.
      ENDLOOP.

      SELECT SINGLE exten3 INTO ls_function-exception_classes FROM enlfdir
        WHERE funcname = <ls_func>-funcname.              "#EC CI_SUBRC

      APPEND ls_function TO rt_functions.

      IF NOT lt_new_source IS INITIAL.
        mo_files->add_abap( iv_extra = <ls_func>-funcname
                            it_abap  = lt_new_source ).
      ELSE.
        mo_files->add_abap( iv_extra = <ls_func>-funcname
                            it_abap  = lt_source ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_includes.

    DATA: lt_includes TYPE ty_sobj_name_tt.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF lt_includes.


    lt_includes = includes( ).

    LOOP AT lt_includes ASSIGNING <lv_include>.

* todo, filename is not correct, a include can be used in several programs
      serialize_program( is_item    = ms_item
                         io_files   = mo_files
                         iv_program = <lv_include>
                         iv_extra   = <lv_include> ).

    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_texts.
    DATA: lt_tpool_i18n TYPE ty_tpools_i18n,
          lt_tpool      TYPE textpool_table.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF lt_tpool_i18n.

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Table d010tinf stores info. on languages in which program is maintained
    " Select all active translations of program texts
    " Skip main language - it was already serialized
    SELECT DISTINCT language
      INTO CORRESPONDING FIELDS OF TABLE lt_tpool_i18n
      FROM d010tinf
      WHERE r3state = 'A'
      AND prog = iv_prog_name
      AND language <> mv_language ##TOO_MANY_ITAB_FIELDS.

    SORT lt_tpool_i18n BY language ASCENDING.
    LOOP AT lt_tpool_i18n ASSIGNING <ls_tpool>.
      READ TEXTPOOL iv_prog_name
        LANGUAGE <ls_tpool>-language
        INTO lt_tpool.
      <ls_tpool>-textpool = add_tpool( lt_tpool ).
    ENDLOOP.

    IF lines( lt_tpool_i18n ) > 0.
      ii_xml->add( iv_name = 'I18N_TPOOL'
                   ig_data = lt_tpool_i18n ).
    ENDIF.
  ENDMETHOD.


  METHOD serialize_xml.

    DATA: lt_includes TYPE ty_sobj_name_tt,
          lv_areat    TYPE tlibt-areat.


    SELECT SINGLE areat INTO lv_areat
      FROM tlibt
      WHERE spras = mv_language
      AND area = ms_item-obj_name.        "#EC CI_GENBUFF "#EC CI_SUBRC

    lt_includes = includes( ).

    ii_xml->add( iv_name = 'AREAT'
                 ig_data = lv_areat ).
    ii_xml->add( iv_name = 'INCLUDES'
                 ig_data = lt_includes ).

  ENDMETHOD.


  METHOD update_func_group_short_text.

    " We update the short text directly.
    " SE80 does the same in
    "   Program SAPLSEUF / LSEUFF07
    "   FORM GROUP_CHANGE

    UPDATE tlibt SET areat = iv_short_text
                 WHERE spras = mv_language
                 AND   area  = iv_group.

  ENDMETHOD.


  METHOD update_where_used.
* make extra sure the where-used list is updated after deletion
* Experienced some problems with the T00 include
* this method just tries to update everything

    DATA: lv_include LIKE LINE OF it_includes,
          lo_cross   TYPE REF TO cl_wb_crossreference.


    LOOP AT it_includes INTO lv_include.

      CREATE OBJECT lo_cross
        EXPORTING
          p_name    = lv_include
          p_include = lv_include.

      lo_cross->index_actualize( ).

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    TYPES: BEGIN OF ty_stamps,
             user TYPE xubname,
             date TYPE d,
             time TYPE t,
           END OF ty_stamps.

    DATA: lt_stamps   TYPE STANDARD TABLE OF ty_stamps WITH DEFAULT KEY,
          lv_program  TYPE program,
          lt_includes TYPE ty_sobj_name_tt.

    FIELD-SYMBOLS: <ls_stamp>   LIKE LINE OF lt_stamps,
                   <lv_include> LIKE LINE OF lt_includes.


    lv_program = main_name( ).

    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program      = lv_program
      TABLES
        includetab   = lt_includes
      EXCEPTIONS
        not_existent = 1
        no_program   = 2
        OTHERS       = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from RS_GET_ALL_INCLUDES' ).
    ENDIF.

    SELECT unam AS user udat AS date utime AS time FROM reposrc
      APPENDING CORRESPONDING FIELDS OF TABLE lt_stamps
      WHERE progname = lv_program
      AND   r3state = 'A'.                                "#EC CI_SUBRC

    LOOP AT lt_includes ASSIGNING <lv_include>.
      SELECT unam AS user udat AS date utime AS time FROM reposrc
        APPENDING CORRESPONDING FIELDS OF TABLE lt_stamps
        WHERE progname = <lv_include>
        AND   r3state = 'A'.                              "#EC CI_SUBRC
    ENDLOOP.

    SELECT unam AS user udat AS date utime AS time FROM repotext " Program text pool
      APPENDING CORRESPONDING FIELDS OF TABLE lt_stamps
      WHERE progname = lv_program
      AND   r3state = 'A'.                                "#EC CI_SUBRC

    SELECT vautor AS user vdatum AS date vzeit AS time FROM eudb         " GUI
      APPENDING CORRESPONDING FIELDS OF TABLE lt_stamps
      WHERE relid = 'CU'
      AND   name  = lv_program
      AND   srtf2 = 0 ##TOO_MANY_ITAB_FIELDS.

* Screens: username not stored in D020S database table

    SORT lt_stamps BY date DESCENDING time DESCENDING.

    READ TABLE lt_stamps INDEX 1 ASSIGNING <ls_stamp>.
    IF sy-subrc = 0.
      rv_user = <ls_stamp>-user.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_area     TYPE rs38l-area,
          lt_includes TYPE ty_sobj_name_tt,
          lv_corrnum  TYPE e070use-ordernum.


    lt_includes = includes( ).

    lv_area = ms_item-obj_name.
    lv_corrnum = zcl_abapgit_default_transport=>get_instance( )->get( )-ordernum.

    CALL FUNCTION 'RS_FUNCTION_POOL_DELETE'
      EXPORTING
        area                   = lv_area
        suppress_popups        = abap_true
        skip_progress_ind      = abap_true
        corrnum                = lv_corrnum
      EXCEPTIONS
        canceled_in_corr       = 1
        enqueue_system_failure = 2
        function_exist         = 3
        not_executed           = 4
        no_modify_permission   = 5
        no_show_permission     = 6
        permission_failure     = 7
        pool_not_exist         = 8
        cancelled              = 9
        OTHERS                 = 10.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    update_where_used( lt_includes ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_program_name TYPE programm,
          lt_functions    TYPE ty_function_tt,
          lt_dynpros      TYPE ty_dynpro_tt,
          ls_cua          TYPE ty_cua.

    deserialize_xml(
      ii_xml     = io_xml
      iv_package = iv_package ).

    io_xml->read( EXPORTING iv_name = 'FUNCTIONS'
                  CHANGING cg_data = lt_functions ).
    deserialize_functions(
      it_functions = lt_functions
      ii_log       = ii_log ).

    deserialize_includes(
      ii_xml     = io_xml
      iv_package = iv_package
      ii_log     = ii_log ).

    lv_program_name = main_name( ).

    deserialize_texts( iv_prog_name = lv_program_name
                       ii_xml       = io_xml ).

    deserialize_lxe_texts( io_xml ).

    io_xml->read( EXPORTING iv_name = 'DYNPROS'
                  CHANGING cg_data = lt_dynpros ).
    deserialize_dynpros( lt_dynpros ).

    io_xml->read( EXPORTING iv_name = 'CUA'
                  CHANGING cg_data = ls_cua ).
    deserialize_cua( iv_program_name = lv_program_name
                     is_cua = ls_cua ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_pool  TYPE tlibg-area.


    lv_pool = ms_item-obj_name.
    CALL FUNCTION 'RS_FUNCTION_POOL_EXISTS'
      EXPORTING
        function_pool   = lv_pool
      EXCEPTIONS
        pool_not_exists = 1.
    rv_bool = boolc( sy-subrc <> 1 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    DATA: lv_program TYPE program.

    lv_program = main_name( ).

    IF is_function_group_locked( )        = abap_true
    OR is_any_include_locked( )           = abap_true
    OR is_any_function_module_locked( )   = abap_true
    OR is_any_dynpro_locked( lv_program ) = abap_true
    OR is_cua_locked( lv_program )        = abap_true
    OR is_text_locked( lv_program )       = abap_true.

      rv_is_locked = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'FUGR'
        in_new_window = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

* function group SEUF
* function group SIFP
* function group SUNI

    DATA: lt_functions    TYPE ty_function_tt,
          ls_progdir      TYPE ty_progdir,
          lv_program_name TYPE programm,
          lt_dynpros      TYPE ty_dynpro_tt,
          ls_cua          TYPE ty_cua.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    serialize_xml( io_xml ).

    lt_functions = serialize_functions( ).
    io_xml->add( iv_name = 'FUNCTIONS'
                 ig_data = lt_functions ).

    serialize_includes( ).

    lv_program_name = main_name( ).
    ls_progdir = read_progdir( lv_program_name ).

    IF io_xml->i18n_params( )-translation_languages IS INITIAL.
      " Old I18N option
      serialize_texts( iv_prog_name = lv_program_name
                       ii_xml       = io_xml ).
    ELSE.
      " New LXE option
      serialize_lxe_texts( io_xml ).
    ENDIF.

    IF ls_progdir-subc = 'F'.
      lt_dynpros = serialize_dynpros( lv_program_name ).
      io_xml->add( iv_name = 'DYNPROS'
                   ig_data = lt_dynpros ).

      ls_cua = serialize_cua( lv_program_name ).
      io_xml->add( iv_name = 'CUA'
                   ig_data = ls_cua ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_IDOC IMPLEMENTATION.


  METHOD clear_idoc_segement_field.

    FIELD-SYMBOLS <lg_any_field> TYPE any.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cg_structure TO <lg_any_field>.
    IF sy-subrc = 0.
      CLEAR <lg_any_field>.
    ENDIF.

  ENDMETHOD.


  METHOD clear_idoc_segement_fields.

    clear_idoc_segement_field( EXPORTING iv_fieldname = 'DEVC'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'PLAST'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'PWORK'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'PRESP'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'CREDATE'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'CRETIME'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'LDATE'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'LTIME'
                               CHANGING  cg_structure = cg_structure ).
  ENDMETHOD.


  METHOD constructor.

    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    mv_idoctyp = ms_item-obj_name.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: ls_attributes TYPE edi_iapi01.

    CALL FUNCTION 'IDOCTYPE_READ'
      EXPORTING
        pi_idoctyp       = mv_idoctyp
      IMPORTING
        pe_attributes    = ls_attributes
      EXCEPTIONS
        object_not_found = 1
        db_error         = 2
        no_authority     = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    rv_user = ls_attributes-plast.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    CALL FUNCTION 'IDOCTYPE_DELETE'
      EXPORTING
        pi_idoctyp          = mv_idoctyp
      EXCEPTIONS
        object_not_found    = 1
        lock_error          = 2
        action_not_possible = 3
        transport_error     = 4
        db_error            = 5
        no_authority        = 6
        OTHERS              = 7.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_idoc       TYPE ty_idoc,
          ls_attributes TYPE edi_iapi05.

    io_xml->read(
      EXPORTING
        iv_name = 'IDOC'
      CHANGING
        cg_data = ls_idoc ).

    MOVE-CORRESPONDING ls_idoc-attributes TO ls_attributes.

    CALL FUNCTION 'IDOCTYPE_CREATE'
      EXPORTING
        pi_idoctyp    = mv_idoctyp
        pi_devclass   = iv_package
        pi_attributes = ls_attributes
      TABLES
        pt_syntax     = ls_idoc-t_syntax
      EXCEPTIONS
        OTHERS        = 1.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    CALL FUNCTION 'IDOCTYPE_READ'
      EXPORTING
        pi_idoctyp       = mv_idoctyp
      EXCEPTIONS
        object_not_found = 1
        db_error         = 2
        no_authority     = 3
        OTHERS           = 4.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMSED5'.
    <ls_bdcdata>-dynpro   = '0010'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'SED5STRUC-OBJECT'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'SED5STRUC-SELECT_ORG'.
    <ls_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=DISP'.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode     = 'WE30'
        mode_val  = 'E'
      TABLES
        using_tab = lt_bdcdata
      EXCEPTIONS
        OTHERS    = 1.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_idoc TYPE ty_idoc.

    CALL FUNCTION 'IDOCTYPE_READ'
      EXPORTING
        pi_idoctyp       = mv_idoctyp
      IMPORTING
        pe_attributes    = ls_idoc-attributes
      TABLES
        pt_syntax        = ls_idoc-t_syntax
      EXCEPTIONS
        object_not_found = 1
        db_error         = 2
        no_authority     = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    clear_idoc_segement_fields( CHANGING cg_structure = ls_idoc-attributes ).

    io_xml->add( iv_name = 'IDOC'
                 ig_data = ls_idoc ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_intf IMPLEMENTATION.


  METHOD constructor.
    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).
    mi_object_oriented_object_fct = zcl_abapgit_oo_factory=>make( ms_item-obj_type ).
  ENDMETHOD.


  METHOD deserialize_abap.
    DATA: ls_vseointerf   TYPE vseointerf,
          lt_source       TYPE seop_source_string,
          lt_descriptions TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
          ls_clskey       TYPE seoclskey.


    ls_clskey-clsname = ms_item-obj_name.
    lt_source = mo_files->read_abap( ).
    ii_xml->read( EXPORTING iv_name = 'VSEOINTERF'
                  CHANGING cg_data = ls_vseointerf ).

    mi_object_oriented_object_fct->create(
      EXPORTING
        iv_package    = iv_package
      CHANGING
        cg_properties = ls_vseointerf ).

    mi_object_oriented_object_fct->deserialize_source(
      is_key               = ls_clskey
      it_source            = lt_source ).

    ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS'
                  CHANGING cg_data = lt_descriptions ).

    mi_object_oriented_object_fct->update_descriptions(
      is_key          = ls_clskey
      it_descriptions = lt_descriptions ).

    mi_object_oriented_object_fct->add_to_activation_list( ms_item ).
  ENDMETHOD.


  METHOD deserialize_docu.

    DATA: lt_lines  TYPE tlinetab,
          lv_object TYPE dokhl-object.

    ii_xml->read( EXPORTING iv_name = 'LINES'
                  CHANGING cg_data = lt_lines ).

    IF lines( lt_lines ) = 0.
      RETURN.
    ENDIF.

    lv_object = ms_item-obj_name.

    mi_object_oriented_object_fct->create_documentation(
      it_lines       = lt_lines
      iv_object_name = lv_object
      iv_language    = mv_language ).
  ENDMETHOD.


  METHOD deserialize_proxy.

    DATA: lv_transport    TYPE e070use-ordernum,
          li_proxy_object TYPE REF TO if_px_main,
          lv_name         TYPE prx_r3name,
          lx_proxy_fault  TYPE REF TO cx_proxy_fault.

    lv_transport = zcl_abapgit_default_transport=>get_instance(
                                               )->get( )-ordernum.

    lv_name = ms_item-obj_name.

    TRY.
        li_proxy_object = cl_pxn_factory=>create(
                              application  = 'PROXY_UI'
                              display_only = abap_false
                              saveable     = abap_true
                          )->if_pxn_factory~load_by_abap_name(
                              object   = ms_item-obj_type
                              obj_name = lv_name ).

        li_proxy_object->activate(
          EXPORTING
            activate_all     = abap_true
          CHANGING
            transport_number = lv_transport ).

        li_proxy_object->dequeue( ).

      CATCH cx_proxy_fault INTO lx_proxy_fault.
        zcx_abapgit_exception=>raise( iv_text     = |{ lx_proxy_fault->get_text( ) }|
                                      ix_previous = lx_proxy_fault ).
    ENDTRY.

  ENDMETHOD.


  METHOD serialize_xml.
    DATA:
      lt_descriptions TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
      ls_vseointerf   TYPE vseointerf,
      ls_clskey       TYPE seoclskey,
      lt_lines        TYPE tlinetab,
      lv_language     TYPE spras.


    ls_clskey-clsname = ms_item-obj_name.

    ls_vseointerf = mi_object_oriented_object_fct->get_interface_properties( ls_clskey ).

    CLEAR: ls_vseointerf-uuid,
           ls_vseointerf-author,
           ls_vseointerf-createdon,
           ls_vseointerf-changedby,
           ls_vseointerf-changedon,
           ls_vseointerf-chgdanyby,
           ls_vseointerf-chgdanyon,
           ls_vseointerf-r3release,
           ls_vseointerf-version.

    io_xml->add( iv_name = 'VSEOINTERF'
                 ig_data = ls_vseointerf ).

    lt_lines = mi_object_oriented_object_fct->read_documentation(
      iv_class_name = ls_clskey-clsname
      iv_language   = mv_language ).
    IF lines( lt_lines ) > 0.
      io_xml->add( iv_name = 'LINES'
                   ig_data = lt_lines ).
    ENDIF.

    IF io_xml->i18n_params( )-main_language_only = abap_true.
      lv_language = mv_language.
    ENDIF.

    lt_descriptions = mi_object_oriented_object_fct->read_descriptions(
      iv_obejct_name = ls_clskey-clsname
      iv_language = lv_language ).

    IF lines( lt_descriptions ) > 0.
      io_xml->add( iv_name = 'DESCRIPTIONS'
                   ig_data = lt_descriptions ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    TYPES: BEGIN OF ty_includes,
             programm TYPE programm,
           END OF ty_includes.

    TYPES: BEGIN OF ty_reposrc,
             unam  TYPE reposrc-unam,
             udat  TYPE reposrc-udat,
             utime TYPE reposrc-utime,
           END OF ty_reposrc.

    DATA: lt_reposrc  TYPE STANDARD TABLE OF ty_reposrc,
          ls_reposrc  LIKE LINE OF lt_reposrc,
          lt_includes TYPE STANDARD TABLE OF ty_includes.

    lt_includes = mi_object_oriented_object_fct->get_includes( ms_item-obj_name ).
    ASSERT lines( lt_includes ) > 0.

    SELECT unam udat utime FROM reposrc
      INTO TABLE lt_reposrc
      FOR ALL ENTRIES IN lt_includes
      WHERE progname = lt_includes-programm
      AND r3state = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ELSE.
      SORT lt_reposrc BY udat DESCENDING utime DESCENDING.
      READ TABLE lt_reposrc INDEX 1 INTO ls_reposrc.
      ASSERT sy-subrc = 0.
      rv_user = ls_reposrc-unam.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    DATA: ls_clskey     TYPE seoclskey,
          ls_vseointerf TYPE vseointerf.

    ls_clskey-clsname = ms_item-obj_name.
    ls_vseointerf = mi_object_oriented_object_fct->get_interface_properties( ls_clskey ).

    IF ls_vseointerf-clsproxy = abap_true.
      " Proxy interfaces are managed via SPRX
      RETURN.
    ENDIF.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    mi_object_oriented_object_fct->delete( ls_clskey ).
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_vseointerf TYPE vseointerf.

    io_xml->read( EXPORTING iv_name = 'VSEOINTERF'
                  CHANGING cg_data = ls_vseointerf ).

    IF ls_vseointerf-clsproxy = abap_true.
      " Proxy interfaces are managed via SPRX
      deserialize_proxy( ).
      RETURN.
    ENDIF.

    deserialize_abap( ii_xml     = io_xml
                      iv_package = iv_package ).

    deserialize_docu( io_xml ).
  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: ls_class_key TYPE seoclskey,
          lv_category  TYPE seoclassdf-category.

    ls_class_key-clsname = ms_item-obj_name.

    rv_bool = mi_object_oriented_object_fct->exists( ls_class_key ).

    IF rv_bool = abap_true.
      SELECT SINGLE category FROM seoclassdf INTO lv_category
        WHERE clsname = ls_class_key-clsname
        AND ( version = '1'
        OR version = '0' ) ##warn_ok.                   "#EC CI_GENBUFF
      IF sy-subrc = 0 AND lv_category = seoc_category_webdynpro_class.
        rv_bool = abap_false.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = |{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '==============================P'.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESEOCLASS'
                                            iv_argument    = lv_object ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'INTF'
        in_new_window = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lt_source        TYPE seop_source_string,
          ls_interface_key TYPE seoclskey.

    ls_interface_key-clsname = ms_item-obj_name.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_active
        force   = abap_true.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_inactive
        force   = abap_true.

    lt_source = mi_object_oriented_object_fct->serialize_abap( ls_interface_key ).

    mo_files->add_abap( lt_source ).

    serialize_xml( io_xml ).
  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_msag IMPLEMENTATION.


  METHOD delete_documentation.
    DATA: lv_key_s TYPE dokhl-object.

    CLEAR lv_key_s.
    CALL FUNCTION 'DOCU_OBJECT_NAME_CONCATENATE'
      EXPORTING
        docu_id  = 'NA'
        element  = iv_message_id
        addition = '   '
      IMPORTING
        object   = lv_key_s
      EXCEPTIONS
        OTHERS   = 0.

    CALL FUNCTION 'DOKU_DELETE_ALL'
      EXPORTING
        doku_id                        = 'NA'
        doku_object                    = lv_key_s
        generic_use                    = 'X'
        suppress_authority             = space
        suppress_enqueue               = space
        suppress_transport             = space
      EXCEPTIONS
        header_without_text            = 01
        index_without_header           = 02
        no_authority_for_devclass_xxxx = 03
        no_docu_found                  = 04
        object_is_already_enqueued     = 05
        object_is_enqueued_by_corr     = 06
        user_break                     = 07.

  ENDMETHOD.


  METHOD delete_msgid.

    delete_documentation( iv_message_id ).

    DELETE FROM t100a WHERE arbgb = iv_message_id.
    IF sy-subrc = 0 OR sy-subrc = 4.
      CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
        EXPORTING
          object    = iv_message_id
          operation = 'DELETE'
          program   = space
          type      = 'CN'.
      DELETE FROM t100o WHERE arbgb = iv_message_id.
      DELETE FROM t100t WHERE arbgb = iv_message_id.    "#EC CI_NOFIRST
      DELETE FROM t100u WHERE arbgb = iv_message_id.
      DELETE FROM t100x WHERE arbgb = iv_message_id.
      DELETE FROM t100 WHERE arbgb = iv_message_id.
    ENDIF.


  ENDMETHOD.


  METHOD deserialize_texts.

    DATA: lv_msg_id     TYPE rglif-message_id,
          ls_t100       TYPE t100,
          lt_t100t      TYPE TABLE OF t100t,
          lt_t100_texts TYPE ty_t100_texts,
          lt_t100u      TYPE TABLE OF t100u.

    FIELD-SYMBOLS: <ls_t100_text> TYPE ty_t100_text.


    lv_msg_id = ms_item-obj_name.

    SELECT * FROM t100u INTO TABLE lt_t100u
      WHERE arbgb = lv_msg_id ORDER BY PRIMARY KEY.     "#EC CI_GENBUFF

    ii_xml->read( EXPORTING iv_name = 'T100_TEXTS'
                  CHANGING  cg_data = lt_t100_texts ).

    ii_xml->read( EXPORTING iv_name = 'T100T'
                  CHANGING  cg_data = lt_t100t ).

    MODIFY t100t FROM TABLE lt_t100t.                     "#EC CI_SUBRC

    LOOP AT lt_t100_texts ASSIGNING <ls_t100_text>.
      "check if message exists
      READ TABLE lt_t100u TRANSPORTING NO FIELDS
        WITH KEY arbgb = lv_msg_id msgnr = <ls_t100_text>-msgnr BINARY SEARCH.
      CHECK sy-subrc = 0. "if original message doesn't exist no translations added

      MOVE-CORRESPONDING <ls_t100_text> TO ls_t100.
      ls_t100-arbgb = lv_msg_id.
      MODIFY t100 FROM ls_t100.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'MSAG: Table T100 modify failed' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD free_access_permission.
    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        mode         = 'FREE'
        object       = iv_message_id
        object_class = 'T100'.
  ENDMETHOD.


  METHOD serialize_longtexts_msag.

    DATA: lv_doku_object_name           TYPE dokhl-object,
          lt_doku_object_names          TYPE STANDARD TABLE OF dokhl-object
                          WITH NON-UNIQUE DEFAULT KEY,
          lt_dokil            TYPE zif_abapgit_definitions=>ty_dokil_tt,
          ls_dokil            LIKE LINE OF lt_dokil.

    FIELD-SYMBOLS: <ls_t100>  TYPE t100.

    IF lines( it_t100 ) = 0.
      RETURN.
    ENDIF.

    LOOP AT it_t100 ASSIGNING <ls_t100>.

      lv_doku_object_name = <ls_t100>-arbgb && <ls_t100>-msgnr.
      INSERT lv_doku_object_name INTO TABLE lt_doku_object_names.

    ENDLOOP.

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      SELECT * FROM dokil
        INTO TABLE lt_dokil
        FOR ALL ENTRIES IN lt_doku_object_names
        WHERE id = 'NA'
        AND object = lt_doku_object_names-table_line
        AND masterlang = abap_true
        ORDER BY PRIMARY KEY.
    ELSE.
      SELECT * FROM dokil
        INTO TABLE lt_dokil
        FOR ALL ENTRIES IN lt_doku_object_names
        WHERE id = 'NA'
        AND object = lt_doku_object_names-table_line
        ORDER BY PRIMARY KEY.
    ENDIF.

    CLEAR ls_dokil-dokstate.
    MODIFY lt_dokil FROM ls_dokil TRANSPORTING dokstate WHERE dokstate IS NOT INITIAL.

    IF lines( lt_dokil ) > 0.
      serialize_longtexts( ii_xml   = ii_xml
                           it_dokil = lt_dokil ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize_texts.

    DATA: lv_msg_id     TYPE rglif-message_id,
          lt_t100_texts TYPE ty_t100_texts,
          lt_t100t      TYPE TABLE OF t100t,
          lt_i18n_langs TYPE TABLE OF langu.

    lv_msg_id = ms_item-obj_name.

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      RETURN. " skip
    ENDIF.

    " Collect additional languages
    " Skip main lang - it has been already serialized
    SELECT DISTINCT sprsl AS langu INTO TABLE lt_i18n_langs
      FROM t100t
      WHERE arbgb = lv_msg_id
      AND sprsl <> mv_language.          "#EC CI_BYPASS "#EC CI_GENBUFF

    SORT lt_i18n_langs ASCENDING.

    IF lines( lt_i18n_langs ) > 0.

      SELECT * FROM t100t INTO CORRESPONDING FIELDS OF TABLE lt_t100t
        WHERE sprsl <> mv_language
        AND arbgb = lv_msg_id.                          "#EC CI_GENBUFF

      SELECT * FROM t100 INTO CORRESPONDING FIELDS OF TABLE lt_t100_texts
        FOR ALL ENTRIES IN lt_i18n_langs
        WHERE sprsl = lt_i18n_langs-table_line
        AND arbgb = lv_msg_id
        ORDER BY PRIMARY KEY.             "#EC CI_SUBRC "#EC CI_GENBUFF

      SORT lt_t100t BY sprsl ASCENDING.
      SORT lt_t100_texts BY sprsl msgnr ASCENDING.

      ii_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      ii_xml->add( iv_name = 'T100T'
                   ig_data = lt_t100t ).

      ii_xml->add( iv_name = 'T100_TEXTS'
                   ig_data = lt_t100_texts ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE lastuser FROM t100a INTO rv_user
      WHERE arbgb = ms_item-obj_name.                   "#EC CI_GENBUFF
    IF sy-subrc <> 0 OR rv_user = ''.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    DATA: ls_t100a          TYPE t100a,
          lv_frozen         TYPE abap_bool,
          lv_message_id     TYPE arbgb,
          lv_access_granted TYPE abap_bool.

* parameter SUPPRESS_DIALOG doesnt exist in all versions of FM RS_DELETE_MESSAGE_ID
* replaced with a copy
    lv_message_id = ms_item-obj_name.
    IF ms_item-obj_name = space.
      zcx_abapgit_exception=>raise( 'Error from (copy of) RS_DELETE_MESSAGE_ID' )."blank message id
    ENDIF.

    SELECT SINGLE * FROM t100a INTO ls_t100a WHERE arbgb = ms_item-obj_name.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from (copy of) RS_DELETE_MESSAGE_ID' )."not found
    ENDIF.

    CLEAR lv_frozen.
    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        authority_check = 'X'
        global_lock     = 'X'
        mode            = 'MODIFY'
        object          = lv_message_id
        object_class    = 'T100'
      IMPORTING
        frozen          = lv_frozen
      EXCEPTIONS
        OTHERS          = 1.

    IF sy-subrc <> 0 OR lv_frozen <> space.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lv_access_granted = abap_true.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        global_lock        = 'X'
        object             = lv_message_id
        object_class       = 'MSAG'
        mode               = 'D'
        suppress_dialog    = abap_true
      EXCEPTIONS
        cancelled          = 01
        permission_failure = 02.

    IF sy-subrc <> 0.
      IF lv_access_granted = abap_true.
        free_access_permission( lv_message_id ).
      ENDIF.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    delete_msgid( lv_message_id ).

    IF lv_access_granted = abap_true.
      free_access_permission( lv_message_id ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
* fm RPY_MESSAGE_ID_INSERT almost works, but not in older versions

    DATA: ls_t100a  TYPE t100a,
          ls_t100t  TYPE t100t,
          ls_t100u  TYPE t100u,
          lt_t100   TYPE TABLE OF t100,
          lt_before TYPE TABLE OF t100u.

    FIELD-SYMBOLS: <ls_t100> LIKE LINE OF lt_t100.


    io_xml->read( EXPORTING iv_name = 'T100A'
                  CHANGING cg_data = ls_t100a ).
    io_xml->read( EXPORTING iv_name = 'T100'
                  CHANGING cg_data = lt_t100 ).

    corr_insert( iv_package ).

    SELECT * FROM t100u INTO TABLE lt_before
      WHERE arbgb = ls_t100a-arbgb ORDER BY msgnr. "#EC CI_GENBUFF "#EC CI_BYPASS

    LOOP AT lt_t100 ASSIGNING <ls_t100>.
      DELETE lt_before WHERE msgnr = <ls_t100>-msgnr.
      MODIFY t100 FROM <ls_t100>.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'MSAG: Table T100 modify failed' ).
      ENDIF.
      CLEAR ls_t100u.
      MOVE-CORRESPONDING <ls_t100> TO ls_t100u ##enh_ok.
      ls_t100u-name    = sy-uname.
      ls_t100u-datum   = sy-datum.
      ls_t100u-selfdef = '3'.
      MODIFY t100u FROM ls_t100u.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'MSAG: Table T100U modify failed' ).
      ENDIF.
    ENDLOOP.

    ls_t100a-masterlang = mv_language.
    ls_t100a-lastuser = sy-uname.
    ls_t100a-respuser = sy-uname.
    ls_t100a-ldate = sy-datum.
    ls_t100a-ltime = sy-uzeit.
    MODIFY t100a FROM ls_t100a.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'MSAG: Table T100A modify failed' ).
    ENDIF.

    ls_t100t-sprsl = mv_language.
    ls_t100t-arbgb = ls_t100a-arbgb.
    ls_t100t-stext = ls_t100a-stext.
    MODIFY t100t FROM ls_t100t.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'MSAG: Table T100T modify failed' ).
    ENDIF.

    LOOP AT lt_before INTO ls_t100u.
      DELETE FROM t100 WHERE arbgb = ls_t100u-arbgb
        AND msgnr = ls_t100u-msgnr.                       "#EC CI_SUBRC

      DELETE FROM t100u WHERE arbgb = ls_t100u-arbgb
        AND msgnr = ls_t100u-msgnr.                       "#EC CI_SUBRC
    ENDLOOP.

    deserialize_longtexts( io_xml ).

    deserialize_texts( io_xml ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_arbgb TYPE t100a-arbgb.


    SELECT SINGLE arbgb FROM t100a INTO lv_arbgb
      WHERE arbgb = ms_item-obj_name.                   "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    DATA: lv_argument TYPE seqg3-garg.

    lv_argument   = |{ ms_item-obj_name }|.
    OVERLAY lv_argument WITH '                     '.
    lv_argument = lv_argument && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = |ES_MSGSI|
                                            iv_argument    = lv_argument ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'MSAG'
        in_new_window = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_msg_id TYPE rglif-message_id,
          ls_inf    TYPE t100a,
          lt_source TYPE ty_t100s.


    lv_msg_id = ms_item-obj_name.

    SELECT SINGLE * FROM t100a INTO ls_inf
      WHERE arbgb = lv_msg_id.                          "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CLEAR ls_inf-respuser.

    SELECT * FROM t100 INTO TABLE lt_source
      WHERE sprsl = mv_language
      AND arbgb = lv_msg_id
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF

    CLEAR: ls_inf-lastuser,
           ls_inf-ldate,
           ls_inf-ltime.

    io_xml->add( iv_name = 'T100A'
                 ig_data = ls_inf ).
    io_xml->add( ig_data = lt_source
                 iv_name = 'T100' ).

    serialize_longtexts_msag( it_t100 = lt_source
                              ii_xml  = io_xml ).

    serialize_texts( io_xml ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_nspc IMPLEMENTATION.


  METHOD add_to_transport.

    DATA: li_sap_package TYPE REF TO zif_abapgit_sap_package.

    li_sap_package = zcl_abapinst_factory=>get_sap_package( iv_package ).

    IF li_sap_package->are_changes_recorded_in_tr_req( ) = abap_true.
      corr_insert( iv_package ).
    ENDIF.

  ENDMETHOD.


  METHOD deserialize_texts.

    DATA:
      ls_trnspacett TYPE trnspacett,
      lt_i18n_langs TYPE TABLE OF langu,
      lt_nspc_texts TYPE ty_nspc_texts.

    FIELD-SYMBOLS:
      <lv_lang>      LIKE LINE OF lt_i18n_langs,
      <ls_nspc_text> LIKE LINE OF lt_nspc_texts.

    ii_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    ii_xml->read( EXPORTING iv_name = 'NSPC_TEXTS'
                  CHANGING  cg_data = lt_nspc_texts ).

    SORT lt_i18n_langs.
    SORT lt_nspc_texts BY spras. " Optimization

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      ls_trnspacett-namespace = iv_namespace.
      READ TABLE lt_nspc_texts ASSIGNING <ls_nspc_text> WITH KEY spras = <lv_lang>.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |NSPC_TEXTS cannot find lang { <lv_lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <ls_nspc_text> TO ls_trnspacett.

      MODIFY trnspacett FROM ls_trnspacett.
      IF sy-subrc <> 0.
        INSERT trnspacett FROM ls_trnspacett.
      ENDIF.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error upserting text for namespace| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_texts.

    DATA:
      ls_trnspacett TYPE trnspacett,
      lt_nspc_texts TYPE ty_nspc_texts,
      lt_i18n_langs TYPE TABLE OF langu.

    FIELD-SYMBOLS:
      <lv_lang>      LIKE LINE OF lt_i18n_langs,
      <ls_nspc_text> LIKE LINE OF lt_nspc_texts.

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Collect additional languages, skip main lang - it was serialized already
    SELECT DISTINCT spras AS langu FROM trnspacett INTO TABLE lt_i18n_langs
      WHERE namespace = ms_item-obj_name AND spras <> mv_language. "#EC CI_SUBRC

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      SELECT SINGLE * FROM trnspacett INTO ls_trnspacett
        WHERE namespace = ms_item-obj_name AND spras = <lv_lang>.
      IF sy-subrc = 0.
        APPEND INITIAL LINE TO lt_nspc_texts ASSIGNING <ls_nspc_text>.
        MOVE-CORRESPONDING ls_trnspacett TO <ls_nspc_text>.
      ENDIF.
    ENDLOOP.

    SORT lt_i18n_langs ASCENDING.
    SORT lt_nspc_texts BY spras ASCENDING.

    IF lines( lt_i18n_langs ) > 0.
      ii_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      ii_xml->add( iv_name = 'NSPC_TEXTS'
                   ig_data = lt_nspc_texts ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    SELECT SINGLE changeuser FROM trnspacet INTO rv_user
       WHERE namespace = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    RETURN. " not supported
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA:
      ls_nspc       TYPE ty_nspc,
      ls_nspc_text  TYPE ty_nspc_text,
      lv_modifiable TYPE abap_bool,
      ls_trnspacet  TYPE trnspacet,
      ls_trnspacett TYPE trnspacett.

    io_xml->read( EXPORTING iv_name = 'NSPC'
                  CHANGING  cg_data = ls_nspc ).

    io_xml->read( EXPORTING iv_name = 'NSPC_TEXT'
                  CHANGING  cg_data = ls_nspc_text ).

    add_to_transport( iv_package ).

    SELECT SINGLE * FROM trnspacet INTO ls_trnspacet WHERE namespace = ls_nspc-namespace.
    IF sy-subrc = 0.
      " For existing namespace, check if it's modifiable (SE03)
      SELECT SINGLE editflag FROM trnspace INTO lv_modifiable WHERE namespace = ls_nspc-namespace.
      IF sy-subrc = 0 AND lv_modifiable = abap_false.
        zcx_abapgit_exception=>raise( |Namespace is not modifiable| ).
      ENDIF.

      " keep existing role
      ls_trnspacet-replicense = ls_nspc-replicense.
      ls_trnspacet-sscrflag   = ls_nspc-sscrflag.
      ls_trnspacet-sapflag    = ls_nspc-sapflag.
      ls_trnspacet-gen_only   = ls_nspc-gen_only.
      ls_trnspacet-changeuser = sy-uname.
      ls_trnspacet-changedate = sy-datum.
      MODIFY trnspacet FROM ls_trnspacet.
    ELSE.
      MOVE-CORRESPONDING ls_nspc TO ls_trnspacet.
      ls_trnspacet-role       = 'C'. " customer repair license
      ls_trnspacet-changeuser = sy-uname.
      ls_trnspacet-changedate = sy-datum.
      INSERT trnspacet FROM ls_trnspacet.
    ENDIF.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error upserting namespace| ).
    ENDIF.

    SELECT SINGLE * FROM trnspacett INTO ls_trnspacett
      WHERE namespace = ls_nspc-namespace AND spras = mv_language.
    IF sy-subrc = 0.
      ls_trnspacett-descriptn = ls_nspc_text-descriptn.
      ls_trnspacett-owner     = ls_nspc_text-owner.
      MODIFY trnspacett FROM ls_trnspacett.
    ELSE.
      MOVE-CORRESPONDING ls_nspc_text TO ls_trnspacett.
      ls_trnspacett-namespace = ls_nspc-namespace.
      INSERT trnspacett FROM ls_trnspacett.
    ENDIF.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error upserting text for namespace| ).
    ENDIF.

    deserialize_texts( ii_xml       = io_xml
                       iv_namespace = ls_nspc-namespace ).

    " Fill trnspace and trnspacel tables
    CALL FUNCTION 'TR_ACTIVATE_NAMESPACE'
      EXPORTING
        iv_namespace         = ls_nspc-namespace
      EXCEPTIONS
        deletion_not_allowed = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error activating namespace| ).
    ENDIF.

    " Make namespace modifiable
    UPDATE trnspace SET editflag = abap_true WHERE namespace = ls_nspc-namespace.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA lv_namespace TYPE trnspace-namespace.

    lv_namespace = ms_item-obj_name.

    CALL FUNCTION 'TR_CHECK_NAMESPACE'
      EXPORTING
        iv_namespace        = lv_namespace
      EXCEPTIONS
        namespace_not_valid = 1
        OTHERS              = 2.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = zif_abapgit_object~exists( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Launch general maintenance for namespaces
    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action                       = 'S'
        view_name                    = 'V_TRNSPACE'
        no_warning_for_clientindep   = 'X'
        variant_for_selection        = 'STANDARD'
      EXCEPTIONS
        client_reference             = 1
        foreign_lock                 = 2
        invalid_action               = 3
        no_clientindependent_auth    = 4
        no_database_function         = 5
        no_editor_function           = 6
        no_show_auth                 = 7
        no_tvdir_entry               = 8
        no_upd_auth                  = 9
        only_show_allowed            = 10
        system_failure               = 11
        unknown_field_in_dba_sellist = 12
        view_not_found               = 13
        OTHERS                       = 14.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA:
      ls_nspc       TYPE ty_nspc,
      ls_nspc_text  TYPE ty_nspc_text.

    SELECT SINGLE * FROM trnspacet INTO CORRESPONDING FIELDS OF ls_nspc
      WHERE namespace = ms_item-obj_name.

    SELECT SINGLE * FROM trnspacett INTO CORRESPONDING FIELDS OF ls_nspc_text
      WHERE namespace = ms_item-obj_name AND spras = mv_language.

    io_xml->add( iv_name = 'NSPC'
                 ig_data = ls_nspc ).

    io_xml->add( iv_name = 'NSPC_TEXT'
                 ig_data = ls_nspc_text ).

    serialize_texts( io_xml ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_para IMPLEMENTATION.


  METHOD unlock.

    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        mode         = 'FREE'
        object       = iv_paramid
        object_class = 'PARA'.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
* looks like "changed by user" is not stored in the database
    rv_user = c_user_unknown.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    " We can't use FM RS_PARAMETER_DELETE because of the popup to confirm
    "Therefore we have to reimplement most of the FMs logic

    DATA: lv_paramid   TYPE tpara-paramid,
          ls_transpkey TYPE trkey.

    lv_paramid = ms_item-obj_name.

    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        global_lock              = abap_true
        language_upd_exit        = 'RS_PARAMETER_LANGUAGE_EXIT'    " Name FuBa for maintenance language change
        object                   = lv_paramid
        object_class             = ms_item-obj_type
        suppress_language_check  = space
      EXCEPTIONS
        canceled_in_corr         = 1
        enqueued_by_user         = 2
        enqueue_system_failure   = 3
        illegal_parameter_values = 4
        locked_by_author         = 5
        no_modify_permission     = 6
        no_show_permission       = 7
        permission_failure       = 8
        request_language_denied  = 9
        OTHERS                   = 10.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    SELECT COUNT(*) FROM cross
      WHERE ( type = 'P' OR type = 'Q' ) AND name = lv_paramid.
    IF sy-subrc = 0.
      unlock( lv_paramid ).
      zcx_abapgit_exception=>raise( 'PARA: Parameter is still used' ).
    ELSE.
      SELECT COUNT(*) FROM dd04l BYPASSING BUFFER
        WHERE memoryid = lv_paramid
        AND as4local = 'A'.
      IF sy-subrc = 0.
        unlock( lv_paramid ).
        zcx_abapgit_exception=>raise( 'PARA: Parameter is still used' ).
      ENDIF.
    ENDIF.
    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        global_lock         = abap_true
        object              = lv_paramid
        object_class        = 'PARA'
        mode                = 'D'
        suppress_dialog     = abap_true
      IMPORTING
        transport_key       = ls_transpkey
      EXCEPTIONS
        cancelled           = 01
        permission_failure  = 02
        unknown_objectclass = 03.

    IF sy-subrc = 0.
      DELETE FROM tpara WHERE paramid = lv_paramid.
      DELETE FROM tparat WHERE paramid = lv_paramid.

      IF sy-subrc = 0.
        CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
          EXPORTING
            object    = lv_paramid
            operation = 'DELETE'
            type      = 'CR'.
      ENDIF.
    ELSE.
      unlock( lv_paramid ).
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    unlock( lv_paramid ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
* see fm RS_PARAMETER_ADD and RS_PARAMETER_EDIT

    DATA: lv_mode   TYPE c LENGTH 1,
          ls_tpara  TYPE tpara,
          ls_tparat TYPE tparat.


    SELECT SINGLE * FROM tpara INTO ls_tpara
      WHERE paramid = ms_item-obj_name.                 "#EC CI_GENBUFF
    IF sy-subrc = 0.
      lv_mode = 'M'.
    ELSE.
      lv_mode = 'I'.
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TPARA'
                  CHANGING cg_data = ls_tpara ).
    io_xml->read( EXPORTING iv_name = 'TPARAT'
                  CHANGING cg_data = ls_tparat ).

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = ms_item-obj_name
        object_class        = 'PARA'
        mode                = lv_mode
        global_lock         = abap_true
        devclass            = iv_package
        master_language     = mv_language
        suppress_dialog     = abap_true
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    MODIFY tpara FROM ls_tpara.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    MODIFY tparat FROM ls_tparat.                         "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_paramid TYPE tpara-paramid.


    SELECT SINGLE paramid FROM tpara INTO lv_paramid
      WHERE paramid = ms_item-obj_name.                 "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
* Data elements can refer to PARA objects
    rs_metadata-ddic = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    DATA: lv_argument TYPE seqg3-garg.

    lv_argument = |PA{ ms_item-obj_name }|.
    OVERLAY lv_argument WITH '                                          '.
    lv_argument = lv_argument && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = lv_argument ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'PARA'
        in_new_window = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_tpara  TYPE tpara,
          ls_tparat TYPE tparat.


    SELECT SINGLE * FROM tpara INTO ls_tpara
      WHERE paramid = ms_item-obj_name.                 "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tparat INTO ls_tparat
      WHERE paramid = ms_item-obj_name
      AND sprache = mv_language.          "#EC CI_GENBUFF "#EC CI_SUBRC

    io_xml->add( iv_name = 'TPARA'
                 ig_data = ls_tpara ).
    io_xml->add( iv_name = 'TPARAT'
                 ig_data = ls_tparat ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_prog IMPLEMENTATION.


  METHOD deserialize_texts.

    DATA: lt_tpool_i18n TYPE ty_tpools_i18n,
          lt_tpool      TYPE textpool_table.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF lt_tpool_i18n.


    ii_xml->read( EXPORTING iv_name = 'I18N_TPOOL'
                  CHANGING  cg_data = lt_tpool_i18n ).

    LOOP AT lt_tpool_i18n ASSIGNING <ls_tpool>.
      lt_tpool = read_tpool( <ls_tpool>-textpool ).
      deserialize_textpool( iv_program  = ms_item-obj_name
                            iv_language = <ls_tpool>-language
                            it_tpool    = lt_tpool ).
    ENDLOOP.

  ENDMETHOD.


  METHOD is_program_locked.

    rv_is_program_locked = exists_a_lock_entry_for( iv_lock_object = 'ESRDIRE'
                                                    iv_argument    = |{ ms_item-obj_name }| ).

  ENDMETHOD.


  METHOD serialize_texts.

    DATA: lt_tpool_i18n TYPE ty_tpools_i18n,
          lt_tpool      TYPE textpool_table.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF lt_tpool_i18n.

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Table d010tinf stores info. on languages in which program is maintained
    " Select all active translations of program texts
    " Skip main language - it was already serialized
    SELECT DISTINCT language
      INTO CORRESPONDING FIELDS OF TABLE lt_tpool_i18n
      FROM d010tinf
      WHERE r3state = 'A'
      AND prog = ms_item-obj_name
      AND language <> mv_language ##TOO_MANY_ITAB_FIELDS.

    SORT lt_tpool_i18n BY language ASCENDING.
    LOOP AT lt_tpool_i18n ASSIGNING <ls_tpool>.
      READ TEXTPOOL ms_item-obj_name
        LANGUAGE <ls_tpool>-language
        INTO lt_tpool.
      <ls_tpool>-textpool = add_tpool( lt_tpool ).
    ENDLOOP.

    IF lines( lt_tpool_i18n ) > 0.
      ii_xml->add( iv_name = 'I18N_TPOOL'
                   ig_data = lt_tpool_i18n ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    SELECT SINGLE unam FROM reposrc INTO rv_user
      WHERE progname = ms_item-obj_name
      AND r3state = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA:
      lv_program    LIKE sy-repid,
      lv_obj_name   TYPE e071-obj_name,
      lv_corrnumber TYPE e071-trkorr.

    lv_program = ms_item-obj_name.
    lv_corrnumber = zcl_abapgit_default_transport=>get_instance( )->get( )-ordernum.

    CALL FUNCTION 'RS_DELETE_PROGRAM'
      EXPORTING
        corrnumber                 = lv_corrnumber
        program                    = lv_program
        suppress_popup             = abap_true
        mass_delete_call           = abap_true
        tadir_devclass             = iv_package
        force_delete_used_includes = abap_true
      EXCEPTIONS
        enqueue_lock               = 1
        object_not_found           = 2
        permission_failure         = 3
        reject_deletion            = 4
        OTHERS                     = 5.
    IF sy-subrc = 2.
      " Drop also any inactive code that is left in REPOSRC
      DELETE REPORT lv_program ##SUBRC_OK.

      " Remove inactive objects from work area
      lv_obj_name = lv_program.

      CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
        EXPORTING
          object                 = 'REPS'
          obj_name               = lv_obj_name
          immediate              = 'X'
          actualize_working_area = 'X'.

      CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
        EXPORTING
          object                 = 'REPT'
          obj_name               = lv_obj_name
          immediate              = 'X'
          actualize_working_area = 'X'.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    delete_longtexts( c_longtext_id_prog ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_program_name TYPE programm,
          ls_progdir      TYPE ty_progdir,
          lt_tpool        TYPE textpool_table,
          lt_dynpros      TYPE ty_dynpro_tt,
          lt_tpool_ext    TYPE zif_abapgit_definitions=>ty_tpool_tt,
          ls_cua          TYPE ty_cua,
          lt_source       TYPE abaptxt255_tab.

    " Add R3TR PROG to transport first, otherwise we get several LIMUs
    corr_insert( iv_package ).

    lv_program_name = ms_item-obj_name.

    lt_source = mo_files->read_abap( ).

    io_xml->read( EXPORTING iv_name = 'TPOOL'
                  CHANGING cg_data = lt_tpool_ext ).
    lt_tpool = read_tpool( lt_tpool_ext ).

    io_xml->read( EXPORTING iv_name = 'PROGDIR'
                  CHANGING cg_data  = ls_progdir ).
    deserialize_program( is_progdir = ls_progdir
                         it_source  = lt_source
                         it_tpool   = lt_tpool
                         iv_package = iv_package ).

    io_xml->read( EXPORTING iv_name = 'DYNPROS'
                  CHANGING cg_data  = lt_dynpros ).
    deserialize_dynpros( lt_dynpros ).

    io_xml->read( EXPORTING iv_name = 'CUA'
                  CHANGING cg_data  = ls_cua ).
    deserialize_cua( iv_program_name = lv_program_name
                     is_cua = ls_cua ).

    " Texts deserializing (English)
    deserialize_textpool( iv_program = lv_program_name
                          it_tpool   = lt_tpool ).

    " Texts deserializing (translations)
    deserialize_texts( io_xml ).
    deserialize_lxe_texts( io_xml ).

    deserialize_longtexts( io_xml ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_progname TYPE reposrc-progname.

    SELECT SINGLE progname FROM reposrc INTO lv_progname
      WHERE progname = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    IF is_program_locked( ) = abap_true
        OR is_any_dynpro_locked( ms_item-obj_name ) = abap_true
        OR is_cua_locked( ms_item-obj_name ) = abap_true
        OR is_text_locked( ms_item-obj_name ) = abap_true.

      rv_is_locked = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'PROG'
        in_new_window = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

* see SAP note 1025291, run report DELETE_TADIR_FOR_EIMP_INCLUDE to clean bad TADIR entries
    ASSERT NOT ms_item-obj_name CP '*=E'.

    serialize_program( io_xml   = io_xml
                       is_item  = ms_item
                       io_files = mo_files ).

    " Texts serializing (translations)
    IF io_xml->i18n_params( )-translation_languages IS INITIAL.
      " Old I18N option
      serialize_texts( io_xml ).
    ELSE.
      " New LXE option
      serialize_lxe_texts( io_xml ).
    ENDIF.

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_prog ).

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_TABL_COMPAR IMPLEMENTATION.


  METHOD constructor.

    mi_local = ii_local.

  ENDMETHOD.


  METHOD get_where_used_recursive.

    DATA: lt_findstrings TYPE string_table,
          lt_founds      TYPE STANDARD TABLE OF rsfindlst,
          lt_scope       TYPE ty_seu_obj,
          lv_findstring  LIKE LINE OF lt_findstrings.

    FIELD-SYMBOLS: <ls_found> TYPE rsfindlst.

    lt_scope = it_scope.

    lv_findstring = iv_object_name.
    INSERT lv_findstring INTO TABLE lt_findstrings.

    DO iv_depth TIMES.

      CLEAR: lt_founds.

      CALL FUNCTION 'RS_EU_CROSSREF'
        EXPORTING
          i_find_obj_cls           = iv_object_type
          no_dialog                = 'X'
        TABLES
          i_findstrings            = lt_findstrings
          o_founds                 = lt_founds
          i_scope_object_cls       = lt_scope
        EXCEPTIONS
          not_executed             = 1
          not_found                = 2
          illegal_object           = 3
          no_cross_for_this_object = 4
          batch                    = 5
          batchjob_error           = 6
          wrong_type               = 7
          object_not_exist         = 8
          OTHERS                   = 9.

      IF sy-subrc = 1 OR sy-subrc = 2 OR lines( lt_founds ) = 0.
        EXIT.
      ELSEIF sy-subrc > 2.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      INSERT LINES OF lt_founds INTO TABLE rt_founds_all.

      CLEAR: lt_findstrings.

      LOOP AT lt_founds ASSIGNING <ls_found>.

        lv_findstring = <ls_found>-object.
        INSERT lv_findstring INTO TABLE lt_findstrings.

      ENDLOOP.

    ENDDO.

  ENDMETHOD.


  METHOD is_structure_used_in_db_table.

    DATA: lt_scope  TYPE ty_seu_obj,
          lt_founds TYPE ty_founds.

    APPEND 'TABL' TO lt_scope.
    APPEND 'STRU' TO lt_scope.

    lt_founds = get_where_used_recursive( iv_object_name = iv_object_name
                                          iv_object_type = 'STRU'
                                          it_scope       = lt_scope
                                          iv_depth       = 5 ).

    DELETE lt_founds WHERE object_cls <> 'DT'.

    rv_is_structure_used_in_db_tab = boolc( lines( lt_founds ) > 0 ).

  ENDMETHOD.


  METHOD validate.

    DATA: lt_previous_table_fields TYPE TABLE OF dd03p,
          ls_previous_table_field  LIKE LINE OF lt_previous_table_fields,
          lt_current_table_fields  TYPE TABLE OF dd03p,
          ls_current_table_field   LIKE LINE OF lt_current_table_fields,
          ls_dd02v                 TYPE dd02v,
          ls_item                  TYPE zif_abapgit_definitions=>ty_item,
          lv_inconsistent          TYPE abap_bool.

    FIELD-SYMBOLS <lv_is_gtt> TYPE abap_bool.

    ii_remote_version->read(
      EXPORTING
        iv_name = 'DD02V'
      CHANGING
        cg_data = ls_dd02v ).

    " We only want to compare transparent tables, or structures used in transparent tables
    IF ls_dd02v-tabclass <> 'TRANSP' AND is_structure_used_in_db_table( ls_dd02v-tabname ) = abap_false.
      RETURN.
    ENDIF.

    " No comparison for global temporary tables
    ASSIGN COMPONENT 'IS_GTT' OF STRUCTURE ls_dd02v TO <lv_is_gtt>.
    IF sy-subrc = 0 AND <lv_is_gtt> = abap_true.
      RETURN.
    ENDIF.

    ii_remote_version->read(
      EXPORTING
        iv_name       = 'DD03P_TABLE'
      CHANGING
        cg_data       = lt_previous_table_fields ).

    ii_local_version->read(
      EXPORTING
        iv_name       = 'DD03P_TABLE'
      CHANGING
        cg_data       = lt_current_table_fields ).

    ls_item-obj_name = ls_dd02v-tabname.
    ls_item-obj_type = 'TABL'.

    LOOP AT lt_previous_table_fields INTO ls_previous_table_field.
      READ TABLE lt_current_table_fields WITH KEY fieldname = ls_previous_table_field-fieldname
        INTO ls_current_table_field.
      IF sy-subrc = 0.
        IF ls_current_table_field-rollname <> ls_previous_table_field-rollname.
          IF ls_current_table_field-rollname IS NOT INITIAL AND ls_previous_table_field-rollname IS NOT INITIAL.
            ii_log->add_info(
              iv_msg  = |Field { ls_previous_table_field-fieldname }: | &
                        |Data element changed from { ls_previous_table_field-rollname } | &
                        |to { ls_current_table_field-rollname }|
              is_item = ls_item ).
          ELSEIF ls_current_table_field-rollname IS NOT INITIAL.
            ii_log->add_info(
              iv_msg  = |Field { ls_previous_table_field-fieldname }: | &
                        |Data type changed from internal type | &
                        |{ ls_previous_table_field-inttype }(length { ls_previous_table_field-intlen }) | &
                        |to data element { ls_current_table_field-rollname }|
              is_item = ls_item ).
          ELSEIF ls_previous_table_field-rollname IS NOT INITIAL.
            ii_log->add_info(
              iv_msg  = |Field { ls_previous_table_field-fieldname }: | &
                        |Data type changed from date element { ls_previous_table_field-rollname } | &
                        |to internal type | &
                        |{ ls_current_table_field-inttype }(length { ls_current_table_field-intlen })|
              is_item = ls_item ).
          ENDIF.
          "TODO: perform several other checks, e.g. field length truncated, ...
          lv_inconsistent = abap_true.
        ENDIF.
      ELSE.
        ii_log->add_info( iv_msg = |Field { ls_previous_table_field-fieldname } removed|
                          is_item = ls_item ).
        lv_inconsistent = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_inconsistent = abap_true.
      rv_message = |Database Table { ls_dd02v-tabname }: Fields were changed. This may lead to inconsistencies!|.
    ENDIF.

    IF NOT rv_message IS INITIAL.
      rv_message = |Database Table { ls_dd02v-tabname }: { rv_message }|.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_comparator~compare.

    rs_result-text = validate(
      ii_remote_version = ii_remote
      ii_local_version  = mi_local
      ii_log            = ii_log ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_tabl IMPLEMENTATION.


  METHOD clear_dd03p_fields.

    CONSTANTS lc_comptype_dataelement TYPE comptype VALUE 'E'.

    DATA: lv_masklen TYPE c LENGTH 4.

    FIELD-SYMBOLS: <ls_dd03p> TYPE dd03p.

* remove nested structures
    DELETE ct_dd03p WHERE depth <> '00'.
* remove fields from .INCLUDEs
    DELETE ct_dd03p WHERE adminfield <> '0'.

    LOOP AT ct_dd03p ASSIGNING <ls_dd03p> WHERE NOT rollname IS INITIAL.

      clear_dd03p_fields_common( CHANGING cs_dd03p = <ls_dd03p> ).

      lv_masklen = <ls_dd03p>-masklen.
      IF lv_masklen = '' OR NOT lv_masklen CO '0123456789'.
* make sure the field contains valid data, or the XML will dump
        CLEAR <ls_dd03p>-masklen.
      ENDIF.

      IF <ls_dd03p>-comptype = lc_comptype_dataelement.
        clear_dd03p_fields_dataelement( CHANGING cs_dd03p = <ls_dd03p> ).
      ENDIF.

      IF <ls_dd03p>-shlporigin = 'D'.
* search help from domain
        CLEAR: <ls_dd03p>-shlpfield,
               <ls_dd03p>-shlpname.
      ENDIF.

* XML output assumes correct field content
      IF <ls_dd03p>-routputlen = '      '.
        CLEAR <ls_dd03p>-routputlen.
      ENDIF.

    ENDLOOP.

    " Clear position to avoid issues with include structures that contain different number of fields
    LOOP AT ct_dd03p ASSIGNING <ls_dd03p>.
      CLEAR: <ls_dd03p>-position, <ls_dd03p>-tabname, <ls_dd03p>-ddlanguage.
    ENDLOOP.

  ENDMETHOD.


  METHOD clear_dd03p_fields_common.

    CLEAR: cs_dd03p-ddlanguage,
           cs_dd03p-dtelmaster,
           cs_dd03p-logflag,
           cs_dd03p-ddtext,
           cs_dd03p-reservedte,
           cs_dd03p-reptext,
           cs_dd03p-scrtext_s,
           cs_dd03p-scrtext_m,
           cs_dd03p-scrtext_l.

  ENDMETHOD.


  METHOD clear_dd03p_fields_dataelement.

* type specified via data element
    CLEAR: cs_dd03p-domname,
           cs_dd03p-inttype,
           cs_dd03p-intlen,
           cs_dd03p-mask,
           cs_dd03p-memoryid,
           cs_dd03p-headlen,
           cs_dd03p-scrlen1,
           cs_dd03p-scrlen2,
           cs_dd03p-scrlen3,
           cs_dd03p-datatype,
           cs_dd03p-leng,
           cs_dd03p-outputlen,
           cs_dd03p-deffdname,
           cs_dd03p-convexit,
           cs_dd03p-entitytab,
           cs_dd03p-dommaster,
           cs_dd03p-domname3l,
           cs_dd03p-decimals,
           cs_dd03p-lowercase,
           cs_dd03p-signflag.

  ENDMETHOD.


  METHOD delete_extras.

    DELETE FROM tddat WHERE tabname = iv_tabname.

  ENDMETHOD.


  METHOD delete_idoc_segment.

    DATA lv_segment_type        TYPE edilsegtyp.
    DATA lv_result              LIKE sy-subrc.

    IF is_idoc_segment( ) = abap_false.
      rv_deleted = abap_false.
      RETURN. "previous XML version or no IDoc segment
    ENDIF.

    rv_deleted = abap_true.
    lv_segment_type = ms_item-obj_name.

    CALL FUNCTION 'SEGMENT_DELETE'
      EXPORTING
        segmenttyp = lv_segment_type
      IMPORTING
        result     = lv_result
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0 OR lv_result <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.


  METHOD deserialize_idoc_segment.

    DATA lv_result              LIKE sy-subrc.
    DATA lt_segment_definitions TYPE ty_segment_definitions.
    DATA lv_package             TYPE devclass.
    DATA lv_uname               TYPE sy-uname.
    FIELD-SYMBOLS <ls_segment_definition> TYPE ty_segment_definition.

    rv_deserialized = abap_false.

    TRY.

        io_xml->read( EXPORTING iv_name = c_s_dataname-segment_definition
                      CHANGING  cg_data = lt_segment_definitions ).

      CATCH zcx_abapgit_exception.
        RETURN. "previous XML version or no IDoc segment
    ENDTRY.

    IF lines( lt_segment_definitions ) = 0.
      RETURN. "no IDoc segment
    ENDIF.

    rv_deserialized = abap_true.

    lv_package = iv_package.

    LOOP AT lt_segment_definitions ASSIGNING <ls_segment_definition>.
      <ls_segment_definition>-segmentheader-presp =
        <ls_segment_definition>-segmentheader-pwork = cl_abap_syst=>get_user_name( ).

      CALL FUNCTION 'SEGMENT_READ'
        EXPORTING
          segmenttyp = <ls_segment_definition>-segmentheader-segtyp
        IMPORTING
          result     = lv_result
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc <> 0 OR lv_result <> 0.
        CALL FUNCTION 'SEGMENT_CREATE'
          IMPORTING
            segmentdefinition = <ls_segment_definition>-segmentdefinition
          TABLES
            segmentstructure  = <ls_segment_definition>-segmentstructures
          CHANGING
            segmentheader     = <ls_segment_definition>-segmentheader
            devclass          = lv_package
          EXCEPTIONS
            OTHERS            = 1.
      ELSE.

        CALL FUNCTION 'SEGMENT_MODIFY'
          CHANGING
            segmentheader = <ls_segment_definition>-segmentheader
            devclass      = lv_package
          EXCEPTIONS
            OTHERS        = 1.
        IF sy-subrc = 0.
          CALL FUNCTION 'SEGMENTDEFINITION_MODIFY'
            TABLES
              segmentstructure  = <ls_segment_definition>-segmentstructures
            CHANGING
              segmentdefinition = <ls_segment_definition>-segmentdefinition
            EXCEPTIONS
              OTHERS            = 1.
        ENDIF.
      ENDIF.

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

    lv_uname = cl_abap_syst=>get_user_name( ).

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus       = abap_false
        wi_tadir_pgmid      = 'R3TR'
        wi_tadir_object     = ms_item-obj_type
        wi_tadir_obj_name   = ms_item-obj_name
        wi_tadir_author     = lv_uname
        wi_tadir_devclass   = iv_package
        wi_tadir_masterlang = mv_language
        iv_set_edtflag      = abap_true
        iv_delflag          = abap_false
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.


  METHOD deserialize_indexes.

    DATA:
      lv_tname     TYPE trobj_name,
      lt_dd12v     TYPE dd12vtab,
      ls_dd12v     LIKE LINE OF lt_dd12v,
      lt_dd17v     TYPE dd17vtab,
      ls_dd17v     LIKE LINE OF lt_dd17v,
      lt_secondary LIKE lt_dd17v.

    io_xml->read( EXPORTING iv_name = 'DD12V'
                  CHANGING cg_data = lt_dd12v ).
    io_xml->read( EXPORTING iv_name = 'DD17V'
                  CHANGING cg_data = lt_dd17v ).

    LOOP AT lt_dd12v INTO ls_dd12v.

* todo, call corr_insert?

      CLEAR lt_secondary.
      LOOP AT lt_dd17v INTO ls_dd17v
          WHERE sqltab = ls_dd12v-sqltab AND indexname = ls_dd12v-indexname.
        APPEND ls_dd17v TO lt_secondary.
      ENDLOOP.

      CALL FUNCTION 'DDIF_INDX_PUT'
        EXPORTING
          name              = ls_dd12v-sqltab
          id                = ls_dd12v-indexname
          dd12v_wa          = ls_dd12v
        TABLES
          dd17v_tab         = lt_secondary
        EXCEPTIONS
          indx_not_found    = 1
          name_inconsistent = 2
          indx_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      CALL FUNCTION 'DD_DD_TO_E071'
        EXPORTING
          type     = 'INDX'
          name     = ls_dd12v-sqltab
          id       = ls_dd12v-indexname
        IMPORTING
          obj_name = lv_tname.

      zcl_abapgit_objects_activation=>add( iv_type = 'INDX'
                                           iv_name = lv_tname ).

    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize_texts.

    DATA: lv_name       TYPE ddobjname,
          ls_dd02v_tmp  TYPE dd02v,
          lt_i18n_langs TYPE TABLE OF langu,
          lt_dd02_texts TYPE ty_dd02_texts.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd02_text> LIKE LINE OF lt_dd02_texts.

    lv_name = ms_item-obj_name.

    io_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    io_xml->read( EXPORTING iv_name = 'DD02_TEXTS'
                  CHANGING  cg_data = lt_dd02_texts ).

    SORT lt_i18n_langs.
    SORT lt_dd02_texts BY ddlanguage. " Optimization

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.

      " Table description
      ls_dd02v_tmp = is_dd02v.
      READ TABLE lt_dd02_texts ASSIGNING <ls_dd02_text> WITH KEY ddlanguage = <lv_lang>.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |DD02_TEXTS cannot find lang { <lv_lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <ls_dd02_text> TO ls_dd02v_tmp.
      CALL FUNCTION 'DDIF_TABL_PUT'
        EXPORTING
          name              = lv_name
          dd02v_wa          = ls_dd02v_tmp
        EXCEPTIONS
          tabl_not_found    = 1
          name_inconsistent = 2
          tabl_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_db_table_category.

    " values from domain TABCLASS
    rv_is_db_table_type = boolc( iv_tabclass = 'TRANSP'
                              OR iv_tabclass = 'CLUSTER'
                              OR iv_tabclass = 'POOL' ).

  ENDMETHOD.


  METHOD is_idoc_segment.

    DATA lv_segment_type TYPE edilsegtyp.

    lv_segment_type = ms_item-obj_name.

    SELECT SINGLE segtyp
           FROM edisegment
           INTO lv_segment_type
           WHERE segtyp = lv_segment_type.
    rv_is_idoc_segment = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD read_extras.

    SELECT SINGLE * FROM tddat INTO rs_tabl_extras-tddat WHERE tabname = iv_tabname.

  ENDMETHOD.


  METHOD serialize_idoc_segment.

    DATA lv_segment_type        TYPE edilsegtyp.
    DATA lv_result              LIKE sy-subrc.
    DATA lv_devclass            TYPE devclass.
    DATA lt_segmentdefinitions  TYPE STANDARD TABLE OF edisegmdef.
    DATA ls_segment_definition  TYPE ty_segment_definition.
    DATA lt_segment_definitions TYPE ty_segment_definitions.
    FIELD-SYMBOLS: <ls_segemtndefinition> TYPE edisegmdef.

    IF is_idoc_segment( ) = abap_false.
      RETURN.
    ENDIF.

    lv_segment_type = ms_item-obj_name.
    CALL FUNCTION 'SEGMENT_READ'
      EXPORTING
        segmenttyp        = lv_segment_type
      IMPORTING
        result            = lv_result
      TABLES
        segmentdefinition = lt_segmentdefinitions
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0 OR lv_result <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_segmentdefinitions ASSIGNING <ls_segemtndefinition>.
      CLEAR ls_segment_definition.
      CALL FUNCTION 'SEGMENTDEFINITION_READ'
        EXPORTING
          segmenttyp           = <ls_segemtndefinition>-segtyp
        IMPORTING
          result               = lv_result
          devclass             = lv_devclass
          segmentheader        = ls_segment_definition-segmentheader
          segmentdefinition    = ls_segment_definition-segmentdefinition
        TABLES
          segmentstructure     = ls_segment_definition-segmentstructures
        CHANGING
          version              = <ls_segemtndefinition>-version
        EXCEPTIONS
          no_authority         = 1
          segment_not_existing = 2
          OTHERS               = 3.
      IF sy-subrc <> 0 OR lv_result <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      zcl_abapgit_object_idoc=>clear_idoc_segement_fields(
                                 CHANGING cg_structure = ls_segment_definition-segmentdefinition ).
      zcl_abapgit_object_idoc=>clear_idoc_segement_fields(
                                 CHANGING cg_structure = ls_segment_definition-segmentheader ).

      APPEND ls_segment_definition TO lt_segment_definitions.
    ENDLOOP.

    io_xml->add( iv_name = c_s_dataname-segment_definition
                 ig_data = lt_segment_definitions ).

  ENDMETHOD.


  METHOD serialize_texts.

    DATA: lv_name       TYPE ddobjname,
          lv_index      TYPE i,
          ls_dd02v      TYPE dd02v,
          lt_dd02_texts TYPE ty_dd02_texts,
          lt_i18n_langs TYPE TABLE OF langu.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd02_text> LIKE LINE OF lt_dd02_texts.

    IF io_xml->i18n_params( )-main_language_only = abap_true.
      RETURN.
    ENDIF.

    lv_name = ms_item-obj_name.

    " Collect additional languages, skip main lang - it was serialized already
    SELECT DISTINCT ddlanguage AS langu INTO TABLE lt_i18n_langs
      FROM dd02v
      WHERE tabname = lv_name
      AND ddlanguage <> mv_language.                      "#EC CI_SUBRC

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      lv_index = sy-tabix.
      CALL FUNCTION 'DDIF_TABL_GET'
        EXPORTING
          name          = lv_name
          langu         = <lv_lang>
        IMPORTING
          dd02v_wa      = ls_dd02v
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0 OR ls_dd02v-ddlanguage IS INITIAL.
        DELETE lt_i18n_langs INDEX lv_index. " Don't save this lang
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lt_dd02_texts ASSIGNING <ls_dd02_text>.
      MOVE-CORRESPONDING ls_dd02v TO <ls_dd02_text>.

    ENDLOOP.

    SORT lt_i18n_langs ASCENDING.
    SORT lt_dd02_texts BY ddlanguage ASCENDING.

    IF lines( lt_i18n_langs ) > 0.
      io_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      io_xml->add( iv_name = 'DD02_TEXTS'
                   ig_data = lt_dd02_texts ).
    ENDIF.

  ENDMETHOD.


  METHOD update_extras.

    IF is_tabl_extras-tddat IS INITIAL.
      delete_extras( iv_tabname ).
    ELSE.
      MODIFY tddat FROM is_tabl_extras-tddat.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    TYPES: BEGIN OF ty_data,
             as4user TYPE dd02l-as4user,
             as4date TYPE dd02l-as4date,
             as4time TYPE dd02l-as4time,
           END OF ty_data.

    DATA: lt_data TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY,
          ls_data LIKE LINE OF lt_data.


    SELECT as4user as4date as4time
      FROM dd02l INTO TABLE lt_data
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.

    SELECT as4user as4date as4time
      APPENDING TABLE lt_data
      FROM dd09l
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.

    SELECT as4user as4date as4time
      APPENDING TABLE lt_data
      FROM dd12l
      WHERE sqltab = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.

    SORT lt_data BY as4date DESCENDING as4time DESCENDING.

    READ TABLE lt_data INDEX 1 INTO ls_data.
    IF sy-subrc = 0.
      rv_user = ls_data-as4user.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_objname TYPE rsedd0-ddobjname,
          lv_no_ask  TYPE abap_bool,
          lv_subrc   TYPE sy-subrc,
          BEGIN OF ls_dd02l,
            tabname  TYPE dd02l-tabname,
            tabclass TYPE dd02l-tabclass,
            sqltab   TYPE dd02l-sqltab,
          END OF ls_dd02l.

    IF zif_abapgit_object~exists( ) = abap_false.
      " Proxies e.g. delete on its own, nothing todo here then.
      RETURN.
    ENDIF.

    lv_objname = ms_item-obj_name.

    IF delete_idoc_segment( ) = abap_false.

      lv_no_ask = abap_true.
      SELECT SINGLE tabname tabclass sqltab FROM dd02l
        INTO CORRESPONDING FIELDS OF ls_dd02l
        WHERE tabname = ms_item-obj_name
        AND as4local = 'A'
        AND as4vers = '0000'.
      IF sy-subrc = 0 AND is_db_table_category( ls_dd02l-tabclass ) = abap_true.

        CALL FUNCTION 'DD_EXISTS_DATA'
          EXPORTING
            reftab          = ls_dd02l-sqltab
            tabclass        = ls_dd02l-tabclass
            tabname         = ls_dd02l-tabname
          IMPORTING
            subrc           = lv_subrc
          EXCEPTIONS
            missing_reftab  = 1
            sql_error       = 2
            buffer_overflow = 3
            unknown_error   = 4
            OTHERS          = 5.

        IF sy-subrc = 0 AND lv_subrc = 0.
          lv_no_ask = abap_false.
        ENDIF.

      ENDIF.

      delete_ddic( iv_objtype = 'T'
                   iv_no_ask  = lv_no_ask ).

      delete_longtexts( c_longtext_id_tabl ).

      delete_extras( lv_objname ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_name   TYPE ddobjname,
          ls_dd02v  TYPE dd02v,
          ls_dd09l  TYPE dd09l,
          lt_dd03p  TYPE TABLE OF dd03p,
          lt_dd05m  TYPE TABLE OF dd05m,
          lt_dd08v  TYPE TABLE OF dd08v,
          lt_dd35v  TYPE TABLE OF dd35v,
          lt_dd36m  TYPE dd36mttyp,
          lv_refs   TYPE abap_bool,
          ls_extras TYPE ty_tabl_extras.

    FIELD-SYMBOLS: <ls_dd03p>      TYPE dd03p,
                   <ls_dd05m>      TYPE dd05m,
                   <ls_dd08v>      TYPE dd08v,
                   <ls_dd35v>      TYPE dd35v,
                   <ls_dd36m>      TYPE dd36m,
                   <lg_roworcolst> TYPE any.

    lv_name = ms_item-obj_name. " type conversion

    IF deserialize_idoc_segment( io_xml     = io_xml
                                 iv_package = iv_package ) = abap_false.

      io_xml->read( EXPORTING iv_name = 'DD02V'
                    CHANGING cg_data = ls_dd02v ).
      io_xml->read( EXPORTING iv_name = 'DD09L'
                    CHANGING cg_data = ls_dd09l ).
      io_xml->read( EXPORTING iv_name  = 'DD03P_TABLE'
                    CHANGING cg_data = lt_dd03p ).
      ASSIGN COMPONENT 'ROWORCOLST' OF STRUCTURE ls_dd09l TO <lg_roworcolst>.
      IF sy-subrc = 0 AND <lg_roworcolst> IS INITIAL.
        <lg_roworcolst> = 'C'. "Reverse fix from serialize
      ENDIF.

      " DDIC Step: Replace REF TO class/interface with generic reference to avoid cyclic dependency
      LOOP AT lt_dd03p ASSIGNING <ls_dd03p> WHERE datatype = 'REF'.
        IF iv_step = zif_abapgit_object=>gc_step_id-ddic.
          <ls_dd03p>-rollname = 'OBJECT'.
        ELSE.
          lv_refs = abap_true.
        ENDIF.
      ENDLOOP.

      " Number fields sequentially and fill table name
      LOOP AT lt_dd03p ASSIGNING <ls_dd03p>.
        <ls_dd03p>-position   = sy-tabix.
        <ls_dd03p>-tabname    = lv_name.
        <ls_dd03p>-ddlanguage = mv_language.
      ENDLOOP.

      io_xml->read( EXPORTING iv_name = 'DD05M_TABLE'
                    CHANGING cg_data = lt_dd05m ).
      io_xml->read( EXPORTING iv_name = 'DD08V_TABLE'
                    CHANGING cg_data = lt_dd08v ).
      io_xml->read( EXPORTING iv_name = 'DD35V_TALE'
                    CHANGING cg_data = lt_dd35v ).
      io_xml->read( EXPORTING iv_name = 'DD36M'
                    CHANGING cg_data = lt_dd36m ).

      LOOP AT lt_dd05m ASSIGNING <ls_dd05m>.
        <ls_dd05m>-tabname = lv_name.
      ENDLOOP.
      LOOP AT lt_dd08v ASSIGNING <ls_dd08v>.
        <ls_dd08v>-tabname = lv_name.
        <ls_dd08v>-ddlanguage = mv_language.
      ENDLOOP.
      LOOP AT lt_dd35v ASSIGNING <ls_dd35v>.
        <ls_dd35v>-tabname = lv_name.
      ENDLOOP.
      LOOP AT lt_dd36m ASSIGNING <ls_dd36m>.
        <ls_dd36m>-tabname = lv_name.
      ENDLOOP.

      " DDIC Step: Remove references to search helps and foreign keys
      IF iv_step = zif_abapgit_object=>gc_step_id-ddic.
        CLEAR: lt_dd08v, lt_dd35v, lt_dd36m.
      ENDIF.

      IF iv_step = zif_abapgit_object=>gc_step_id-late AND lv_refs = abap_false
        AND lines( lt_dd35v ) = 0 AND lines( lt_dd08v ) = 0.
        RETURN. " already active
      ENDIF.

      corr_insert( iv_package = iv_package
                   ig_object_class = 'DICT' ).

      CALL FUNCTION 'DDIF_TABL_PUT'
        EXPORTING
          name              = lv_name
          dd02v_wa          = ls_dd02v
          dd09l_wa          = ls_dd09l
        TABLES
          dd03p_tab         = lt_dd03p
          dd05m_tab         = lt_dd05m
          dd08v_tab         = lt_dd08v
          dd35v_tab         = lt_dd35v
          dd36m_tab         = lt_dd36m
        EXCEPTIONS
          tabl_not_found    = 1
          name_inconsistent = 2
          tabl_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      zcl_abapgit_objects_activation=>add_item( ms_item ).

      deserialize_indexes( io_xml ).

      deserialize_texts( io_xml   = io_xml
                         is_dd02v = ls_dd02v ).

      deserialize_longtexts( io_xml ).

      io_xml->read( EXPORTING iv_name = c_s_dataname-tabl_extras
                    CHANGING cg_data = ls_extras ).
      update_extras( iv_tabname     = lv_name
                     is_tabl_extras = ls_extras ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_tabname TYPE dd02l-tabname.

    lv_tabname = ms_item-obj_name.

    " Check nametab because it's fast
    CALL FUNCTION 'DD_GET_NAMETAB_HEADER'
      EXPORTING
        tabname   = lv_tabname
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      " Check for new, inactive, or modified versions that might not be in nametab
      SELECT SINGLE tabname FROM dd02l INTO lv_tabname
        WHERE tabname = lv_tabname.
    ENDIF.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.

    DATA: li_local_version_output TYPE REF TO zif_abapgit_xml_output,
          li_local_version_input  TYPE REF TO zif_abapgit_xml_input.


    CREATE OBJECT li_local_version_output TYPE zcl_abapgit_xml_output.

    zif_abapgit_object~serialize( li_local_version_output ).

    CREATE OBJECT li_local_version_input
      TYPE zcl_abapgit_xml_input
      EXPORTING
        iv_xml = li_local_version_output->render( ).

    CREATE OBJECT ri_comparator TYPE zcl_abapgit_object_tabl_compar
      EXPORTING
        ii_local = li_local_version_input.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
    APPEND zif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-ddic = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESDICT'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    jump_se11( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_name   TYPE ddobjname,
          ls_dd02v  TYPE dd02v,
          ls_dd09l  TYPE dd09l,
          lt_dd03p  TYPE ty_dd03p_tt,
          lt_dd05m  TYPE TABLE OF dd05m,
          lt_dd08v  TYPE TABLE OF dd08v,
          lt_dd12v  TYPE dd12vtab,
          lt_dd17v  TYPE dd17vtab,
          lt_dd35v  TYPE TABLE OF dd35v,
          lv_index  LIKE sy-index,
          lt_dd36m  TYPE dd36mttyp,
          ls_extras TYPE ty_tabl_extras.

    FIELD-SYMBOLS: <ls_dd12v>      LIKE LINE OF lt_dd12v,
                   <ls_dd05m>      LIKE LINE OF lt_dd05m,
                   <ls_dd08v>      LIKE LINE OF lt_dd08v,
                   <ls_dd35v>      LIKE LINE OF lt_dd35v,
                   <ls_dd36m>      LIKE LINE OF lt_dd36m,
                   <lg_roworcolst> TYPE any.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = lv_name
        langu         = mv_language
      IMPORTING
        dd02v_wa      = ls_dd02v
        dd09l_wa      = ls_dd09l
      TABLES
        dd03p_tab     = lt_dd03p
        dd05m_tab     = lt_dd05m
        dd08v_tab     = lt_dd08v
        dd12v_tab     = lt_dd12v
        dd17v_tab     = lt_dd17v
        dd35v_tab     = lt_dd35v
        dd36m_tab     = lt_dd36m
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from DDIF_TABL_GET' ).
    ENDIF.

    IF ls_dd02v IS INITIAL.
      zcx_abapgit_exception=>raise( |No active version found for { ms_item-obj_type } { ms_item-obj_name }| ).
    ENDIF.

    CLEAR: ls_dd02v-as4user,
           ls_dd02v-as4date,
           ls_dd02v-as4time.

* reset numeric field, so XML does not crash
    IF ls_dd02v-prozpuff = ''.
      CLEAR ls_dd02v-prozpuff.
    ENDIF.
    IF ls_dd02v-datmin = ''.
      CLEAR ls_dd02v-datmin.
    ENDIF.
    IF ls_dd02v-datmax = ''.
      CLEAR ls_dd02v-datmax.
    ENDIF.
    IF ls_dd02v-datavg = ''.
      CLEAR ls_dd02v-datavg.
    ENDIF.

    CLEAR: ls_dd09l-as4user,
           ls_dd09l-as4date,
           ls_dd09l-as4time.

    ASSIGN COMPONENT 'ROWORCOLST' OF STRUCTURE ls_dd09l TO <lg_roworcolst>.
    IF sy-subrc = 0 AND <lg_roworcolst> = 'C'.
      CLEAR <lg_roworcolst>. "To avoid diff errors. This field doesn't exists in all releases
    ENDIF.


    LOOP AT lt_dd12v ASSIGNING <ls_dd12v>.
      CLEAR: <ls_dd12v>-as4user,
             <ls_dd12v>-as4date,
             <ls_dd12v>-as4time.
    ENDLOOP.

    clear_dd03p_fields( CHANGING ct_dd03p = lt_dd03p ).

* remove foreign keys inherited from .INCLUDEs
    DELETE lt_dd08v WHERE noinherit = 'N'.
    LOOP AT lt_dd05m ASSIGNING <ls_dd05m>.
      CLEAR <ls_dd05m>-tabname.
      lv_index = sy-tabix.
      READ TABLE lt_dd08v WITH KEY fieldname = <ls_dd05m>-fieldname TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        DELETE lt_dd05m INDEX lv_index.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_dd08v ASSIGNING <ls_dd08v>.
      CLEAR: <ls_dd08v>-tabname, <ls_dd08v>-ddlanguage.
    ENDLOOP.
    LOOP AT lt_dd35v ASSIGNING <ls_dd35v>.
      CLEAR <ls_dd35v>-tabname.
    ENDLOOP.

* remove inherited search helps
    DELETE lt_dd35v WHERE shlpinher = abap_true.
    LOOP AT lt_dd36m ASSIGNING <ls_dd36m>.
      CLEAR <ls_dd36m>-tabname.
      lv_index = sy-tabix.
      READ TABLE lt_dd35v WITH KEY fieldname = <ls_dd36m>-fieldname TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        DELETE lt_dd36m INDEX lv_index.
      ENDIF.
    ENDLOOP.

    io_xml->add( iv_name = 'DD02V'
                 ig_data = ls_dd02v ).
    IF NOT ls_dd09l IS INITIAL.
      io_xml->add( iv_name = 'DD09L'
                   ig_data = ls_dd09l ).
    ENDIF.
    io_xml->add( iv_name = 'DD03P_TABLE'
                 ig_data = lt_dd03p ).
    io_xml->add( iv_name = 'DD05M_TABLE'
                 ig_data = lt_dd05m ).
    io_xml->add( iv_name = 'DD08V_TABLE'
                 ig_data = lt_dd08v ).
    io_xml->add( iv_name = 'DD12V'
                 ig_data = lt_dd12v ).
    io_xml->add( iv_name = 'DD17V'
                 ig_data = lt_dd17v ).
    io_xml->add( iv_name = 'DD35V_TALE'
                 ig_data = lt_dd35v ).
    io_xml->add( iv_name = 'DD36M'
                 ig_data = lt_dd36m ).

    serialize_texts( io_xml ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_tabl ).

    serialize_idoc_segment( io_xml ).

    ls_extras = read_extras( lv_name ).
    io_xml->add( iv_name = c_s_dataname-tabl_extras
                 ig_data = ls_extras ).

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_TOBJ IMPLEMENTATION.


  METHOD delete_extra.

    DELETE FROM tddat WHERE tabname = iv_tabname.
    DELETE FROM tvdir WHERE tabname = iv_tabname.
    DELETE FROM tvimf WHERE tabname = iv_tabname.

  ENDMETHOD.


  METHOD read_extra.

    SELECT SINGLE * FROM tddat INTO rs_tobj-tddat WHERE tabname = iv_tabname.

    SELECT SINGLE * FROM tvdir INTO rs_tobj-tvdir WHERE tabname = iv_tabname.
    CLEAR: rs_tobj-tvdir-gendate, rs_tobj-tvdir-gentime, rs_tobj-tvdir-devclass.

    SELECT * FROM tvimf INTO TABLE rs_tobj-tvimf WHERE tabname = iv_tabname
      ORDER BY PRIMARY KEY.

  ENDMETHOD.


  METHOD update_extra.
    DATA: lt_current_tvimf TYPE STANDARD TABLE OF tvimf.
    FIELD-SYMBOLS: <ls_tvimf> TYPE tvimf.

    MODIFY tddat FROM is_tobj-tddat.
    MODIFY tvdir FROM is_tobj-tvdir.

    SELECT * INTO TABLE lt_current_tvimf
      FROM tvimf
      WHERE tabname = is_tobj-tddat-tabname
      ORDER BY PRIMARY KEY.

    LOOP AT lt_current_tvimf ASSIGNING <ls_tvimf>.
      READ TABLE is_tobj-tvimf WITH KEY tabname = <ls_tvimf>-tabname
                                        event   = <ls_tvimf>-event
                               TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        DELETE FROM tvimf
          WHERE tabname = <ls_tvimf>-tabname
          AND event = <ls_tvimf>-event.
      ENDIF.
    ENDLOOP.

    MODIFY tvimf FROM TABLE is_tobj-tvimf.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lv_type_pos TYPE i.

    lv_type_pos = strlen( ms_item-obj_name ) - 1.

    SELECT SINGLE luser FROM objh INTO rv_user
      WHERE objectname = ms_item-obj_name(lv_type_pos)
      AND objecttype = ms_item-obj_name+lv_type_pos.    "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: ls_objh     TYPE objh,
          lv_type_pos TYPE i.

    lv_type_pos = strlen( ms_item-obj_name ) - 1.

    ls_objh-objectname = ms_item-obj_name(lv_type_pos).
    ls_objh-objecttype = ms_item-obj_name+lv_type_pos.

    CALL FUNCTION 'OBJ_GENERATE'
      EXPORTING
        iv_objectname         = ls_objh-objectname
        iv_objecttype         = ls_objh-objecttype
        iv_maint_mode         = 'D'
      EXCEPTIONS
        illegal_call          = 1
        object_not_found      = 2
        generate_error        = 3
        transport_error       = 4
        object_enqueue_failed = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from OBJ_GENERATE' ).
    ENDIF.

    delete_extra( ls_objh-objectname ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_objh  TYPE objh,
          ls_objt  TYPE objt,
          lt_objs  TYPE tt_objs,
          lt_objsl TYPE tt_objsl,
          lt_objm  TYPE tt_objm,
          ls_tobj  TYPE ty_tobj.


    io_xml->read( EXPORTING iv_name = 'OBJH'
                  CHANGING cg_data = ls_objh ).
    io_xml->read( EXPORTING iv_name = 'OBJT'
                  CHANGING cg_data = ls_objt ).
    io_xml->read( EXPORTING iv_name = 'OBJS'
                  CHANGING cg_data = lt_objs ).
    io_xml->read( EXPORTING iv_name = 'OBJSL'
                  CHANGING cg_data = lt_objsl ).
    io_xml->read( EXPORTING iv_name = 'OBJM'
                  CHANGING cg_data = lt_objm ).

    CALL FUNCTION 'OBJ_GENERATE'
      EXPORTING
        iv_objectname         = ls_objh-objectname
        iv_objecttype         = ls_objh-objecttype
        iv_maint_mode         = 'I'
        iv_objecttext         = ls_objt-ddtext
        iv_objcateg           = ls_objh-objcateg
        iv_objtransp          = ls_objh-objtransp
        iv_devclass           = iv_package
      TABLES
        tt_v_obj_s            = lt_objs
        tt_objm               = lt_objm
      EXCEPTIONS
        illegal_call          = 1
        object_not_found      = 2
        generate_error        = 3
        transport_error       = 4
        object_enqueue_failed = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
* TOBJ has to be saved/generated after the DDIC tables have been
* activated - fixed with late deserialization
      zcx_abapgit_exception=>raise( 'error from OBJ_GENERATE' ).
    ENDIF.

    CALL FUNCTION 'OBJ_SET_IMPORTABLE'
      EXPORTING
        iv_objectname         = ls_objh-objectname
        iv_objecttype         = ls_objh-objecttype
        iv_importable         = ls_objh-importable
      EXCEPTIONS
        object_not_defined    = 1
        invalid               = 2
        transport_error       = 3
        object_enqueue_failed = 4
        OTHERS                = 5.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from OBJ_SET_IMPORTABLE' ).
    ENDIF.

* fm OBJ_GENERATE takes the defaults from the DDIC object
* set OBJTRANSP directly, should be okay looking at the code in OBJ_SET_IMPORTABLE
* locking has been done in OBJ_SET_IMPORTABLE plus recording of transport
    UPDATE objh SET objtransp = ls_objh-objtransp
      WHERE objectname = ls_objh-objectname
      AND objecttype = ls_objh-objecttype.

    io_xml->read( EXPORTING iv_name = 'TOBJ'
                  CHANGING cg_data = ls_tobj ).
    ls_tobj-tvdir-gendate = sy-datum.
    ls_tobj-tvdir-gentime = sy-uzeit.
    ls_tobj-tvdir-devclass = iv_package.

    update_extra( ls_tobj ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_objectname TYPE objh-objectname,
          lv_type_pos   TYPE i.

    lv_type_pos = strlen( ms_item-obj_name ) - 1.

    SELECT SINGLE objectname FROM objh INTO lv_objectname
      WHERE objectname = ms_item-obj_name(lv_type_pos)
      AND objecttype = ms_item-obj_name+lv_type_pos.    "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA: lv_object_name TYPE e071-obj_name.

    lv_object_name = ms_item-obj_name.

    CALL FUNCTION 'TR_OBJECT_JUMP_TO_TOOL'
      EXPORTING
        iv_pgmid          = 'R3TR'
        iv_object         = ms_item-obj_type
        iv_obj_name       = lv_object_name
      EXCEPTIONS
        jump_not_possible = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Jump not possible. Subrc={ sy-subrc } from TR_OBJECT_JUMP_TO_TOOL| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_objh     TYPE objh,
          ls_objt     TYPE objt,
          lt_objs     TYPE tt_objs,
          lt_objsl    TYPE tt_objsl,
          lt_objm     TYPE tt_objm,
          ls_tobj     TYPE ty_tobj,
          lv_type_pos TYPE i.

    lv_type_pos = strlen( ms_item-obj_name ) - 1.

    ls_objh-objectname = ms_item-obj_name(lv_type_pos).
    ls_objh-objecttype = ms_item-obj_name+lv_type_pos.

    CALL FUNCTION 'CTO_OBJECT_GET'
      EXPORTING
        iv_objectname      = ls_objh-objectname
        iv_objecttype      = ls_objh-objecttype
        iv_language        = mv_language
        iv_sel_objt        = abap_true
        iv_sel_objs        = abap_true
        iv_sel_objsl       = abap_true
        iv_sel_objm        = abap_true
      IMPORTING
        es_objh            = ls_objh
        es_objt            = ls_objt
      TABLES
        tt_objs            = lt_objs
        tt_objsl           = lt_objsl
        tt_objm            = lt_objm
      EXCEPTIONS
        object_not_defined = 1
        OTHERS             = 2.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CLEAR: ls_objh-luser,
           ls_objh-ldate.

    io_xml->add( iv_name = 'OBJH'
                 ig_data = ls_objh ).
    io_xml->add( iv_name = 'OBJT'
                 ig_data = ls_objt ).
    io_xml->add( iv_name = 'OBJS'
                 ig_data = lt_objs ).
    io_xml->add( iv_name = 'OBJSL'
                 ig_data = lt_objsl ).
    io_xml->add( iv_name = 'OBJM'
                 ig_data = lt_objm ).

    ls_tobj = read_extra( ls_objh-objectname ).

    io_xml->add( iv_name = 'TOBJ'
                 ig_data = ls_tobj ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_tran IMPLEMENTATION.


  METHOD add_data.

    DATA: ls_bcdata LIKE LINE OF mt_bcdata.

    ls_bcdata-fnam = iv_fnam.
    ls_bcdata-fval = iv_fval.
    APPEND ls_bcdata TO mt_bcdata.

  ENDMETHOD.


  METHOD call_se93.

    DATA: lt_message TYPE STANDARD TABLE OF bdcmsgcoll.

    FIELD-SYMBOLS: <ls_message> TYPE bdcmsgcoll.


    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      EXPORTING
        tcode     = 'SE93'
        mode_val  = 'N'
      TABLES
        using_tab = mt_bcdata
        mess_tab  = lt_message
      EXCEPTIONS
        OTHERS    = 1.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error deserializing { ms_item-obj_type } { ms_item-obj_name }| ).
    ENDIF.

    LOOP AT lt_message ASSIGNING <ls_message> WHERE msgtyp CA 'EAX'.
      MESSAGE ID <ls_message>-msgid
        TYPE <ls_message>-msgtyp
        NUMBER <ls_message>-msgnr
        WITH <ls_message>-msgv1 <ls_message>-msgv2 <ls_message>-msgv3 <ls_message>-msgv4
        INTO sy-msgli.
      zcx_abapgit_exception=>raise_t100( ).
    ENDLOOP.

  ENDMETHOD.


  METHOD clear_functiongroup_globals.
    TYPES ty_param_vari TYPE abap_bool.

    DATA lt_error_list TYPE STANDARD TABLE OF rsmp_check WITH DEFAULT KEY.
    FIELD-SYMBOLS <lv_param_vari> TYPE ty_param_vari.

    " only way to clear global fields in function group
    CALL FUNCTION 'RS_TRANSACTION_INCONSISTENCIES'
      EXPORTING
        transaction_code = 'ZTHISTCODENEVEREXIST'
      TABLES
        error_list       = lt_error_list
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      "Expected - fine

      " but there is no other way to clear this field
      ASSIGN ('(SAPLSEUK)PARAM_VARI') TO <lv_param_vari>.
      IF sy-subrc = 0.
        CLEAR <lv_param_vari>.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD deserialize_oo_transaction.

    " You should remember that we don't use batch input just for fun,
    " but because FM RPY_TRANSACTION_INSERT doesn't support OO transactions.

    DATA: ls_bcdata  TYPE bdcdata.


    CLEAR mt_bcdata.

    ls_bcdata-program  = 'SAPLSEUK'.
    ls_bcdata-dynpro   = '0390'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam = 'TSTC-TCODE'
              iv_fval = is_tstc-tcode ).

    IF zif_abapgit_object~exists( ) = abap_true.

      add_data( iv_fnam = 'BDC_OKCODE'
                iv_fval = '=CHNG' ).

    ELSE.

      add_data( iv_fnam = 'BDC_OKCODE'
                iv_fval = '=ADD' ).

    ENDIF.

    ls_bcdata-program  = 'SAPLSEUK'.
    ls_bcdata-dynpro   = '0300'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam     = 'TSTCT-TTEXT'
              iv_fval     = is_tstct-ttext ).

    add_data( iv_fnam     = 'RSSTCD-S_CLASS'
              iv_fval     = 'X' ).

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=ENTR' ).

    ls_bcdata-program  = 'SAPLSEUK'.
    ls_bcdata-dynpro   = '0360'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam     = 'RSSTCD-S_TRFRAME'
              iv_fval     = is_rsstcd-s_trframe ).

    add_data( iv_fnam     = 'RSSTCD-S_UPDTASK'
              iv_fval     = is_rsstcd-s_updtask ).

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=TR_FRAMEWORK' ).

    ls_bcdata-program  = 'SAPLSEUK'.
    ls_bcdata-dynpro   = '0360'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam     = 'RSSTCD-CLASSNAME'
              iv_fval     = is_rsstcd-classname ).

    add_data( iv_fnam     = 'RSSTCD-METHOD'
              iv_fval     = is_rsstcd-method ).

    IF is_rsstcd-s_local IS NOT INITIAL.
      add_data( iv_fnam     = 'RSSTCD-S_LOCAL'
                iv_fval     = is_rsstcd-s_local ).
    ENDIF.

    IF is_rsstcd-s_updlok IS NOT INITIAL.
      add_data( iv_fnam     = 'RSSTCD-S_UPDLOK'
                iv_fval     = is_rsstcd-s_updlok ).
    ENDIF.

    add_data( iv_fnam     = 'TSTC-PGMNA'
              iv_fval     = is_tstc-pgmna ).

    IF is_tstcc-s_webgui = '2'.

      add_data( iv_fnam     = 'G_IAC_EWT'
                iv_fval     = abap_true ).

      add_data( iv_fnam = 'BDC_OKCODE'
                iv_fval = 'MAKE_PROFI' ).

      ls_bcdata-program  = 'SAPLSEUK'.
      ls_bcdata-dynpro   = '0360'.
      ls_bcdata-dynbegin = 'X'.
      APPEND ls_bcdata TO mt_bcdata.

    ELSEIF is_tstcc-s_webgui IS NOT INITIAL.

      add_data( iv_fnam     = 'TSTCC-S_WEBGUI'
                iv_fval     = is_tstcc-s_webgui ).

    ENDIF.

    IF is_tstcc-s_pervas IS NOT INITIAL.
      add_data( iv_fnam     = 'TSTCC-S_PERVAS'
                iv_fval     = is_tstcc-s_pervas ).
    ENDIF.

    IF is_tstcc-s_service IS NOT INITIAL.
      add_data( iv_fnam     = 'TSTCC-S_SERVICE'
                iv_fval     = is_tstcc-s_service ).
    ENDIF.

    IF is_tstcc-s_platin IS NOT INITIAL.
      add_data( iv_fnam     = 'TSTCC-S_PLATIN'
                iv_fval     = is_tstcc-s_platin ).
    ENDIF.

    IF is_tstcc-s_win32 IS NOT INITIAL.
      add_data( iv_fnam     = 'TSTCC-S_WIN32'
                iv_fval     = is_tstcc-s_win32 ).
    ENDIF.

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=WB_SAVE' ).

    ls_bcdata-program  = 'SAPLSTRD'.
    ls_bcdata-dynpro   = '0100'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam     = 'KO007-L_DEVCLASS'
              iv_fval     = iv_package ).

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=ADD' ).

    ls_bcdata-program  = 'BDC_OKCODE'.
    ls_bcdata-dynpro   = '0360'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=WB_BACK' ).

    ls_bcdata-program  = 'BDC_OKCODE'.
    ls_bcdata-dynpro   = '0360'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=WB_BACK' ).

    call_se93( ).

  ENDMETHOD.


  METHOD deserialize_texts.

    DATA lt_tpool_i18n TYPE TABLE OF tstct.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF lt_tpool_i18n.


    " Read XML-files data
    io_xml->read( EXPORTING iv_name = 'I18N_TPOOL'
                  CHANGING  cg_data = lt_tpool_i18n ).

    " Force t-code name (security reasons)
    LOOP AT lt_tpool_i18n ASSIGNING <ls_tpool>.
      <ls_tpool>-tcode = ms_item-obj_name.
    ENDLOOP.

    IF lines( lt_tpool_i18n ) > 0.
      MODIFY tstct FROM TABLE lt_tpool_i18n.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Update of t-code translations failed' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD is_variant_transaction.

    rv_variant_transaction = boolc( is_tstcp-param(1) = '@' ).

  ENDMETHOD.


  METHOD save_authorizations.

    CONSTANTS: lc_hex_chk TYPE x VALUE '04'.
    DATA: ls_transaction TYPE tstc.

    transaction_read( EXPORTING iv_transaction = iv_transaction
                      IMPORTING es_transaction = ls_transaction ).

    DELETE FROM tstca WHERE tcode = iv_transaction.

    IF ls_transaction IS NOT INITIAL.
      INSERT tstca FROM TABLE it_authorizations.
      ls_transaction-cinfo = ls_transaction-cinfo + lc_hex_chk.
      UPDATE tstc SET cinfo = ls_transaction-cinfo WHERE tcode = ls_transaction-tcode.
    ENDIF.

  ENDMETHOD.


  METHOD serialize_texts.

    DATA lt_tpool_i18n TYPE TABLE OF tstct.

    IF io_xml->i18n_params( )-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Skip main language - it was already serialized
    " Don't serialize t-code itself
    SELECT sprsl ttext
      INTO CORRESPONDING FIELDS OF TABLE lt_tpool_i18n
      FROM tstct
      WHERE sprsl <> mv_language
      AND   tcode = ms_item-obj_name ##TOO_MANY_ITAB_FIELDS. "#EC CI_GENBUFF

    IF lines( lt_tpool_i18n ) > 0.
      SORT lt_tpool_i18n BY sprsl ASCENDING.
      io_xml->add( iv_name = 'I18N_TPOOL'
                   ig_data = lt_tpool_i18n ).
    ENDIF.

  ENDMETHOD.


  METHOD set_oo_parameters.

    DATA: ls_param LIKE LINE OF it_rsparam.

    IF cs_rsstcd-call_tcode = c_oo_tcode.
      cs_rsstcd-s_trframe = c_true.
      LOOP AT it_rsparam INTO ls_param.
        CASE ls_param-field.
          WHEN c_oo_frclass.
            cs_rsstcd-classname = ls_param-value.
          WHEN c_oo_frmethod.
            cs_rsstcd-method   = ls_param-value.
          WHEN c_oo_frupdtask.
            IF ls_param-value = c_oo_synchron.
              cs_rsstcd-s_upddir  = c_true.
              cs_rsstcd-s_updtask = c_false.
              cs_rsstcd-s_updlok  = c_false.
            ELSEIF ls_param-value = c_oo_asynchron.
              cs_rsstcd-s_upddir  = c_false.
              cs_rsstcd-s_updtask = c_true.
              cs_rsstcd-s_updlok  = c_false.
            ELSE.
              cs_rsstcd-s_upddir  = c_false.
              cs_rsstcd-s_updtask = c_false.
              cs_rsstcd-s_updlok  = c_true.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD shift_param.

    DATA: ls_param  LIKE LINE OF ct_rsparam,
          lv_length TYPE i.

    FIELD-SYMBOLS <lg_f> TYPE any.


    DO 254 TIMES.
      IF cs_tstcp-param = space.
        EXIT.
      ENDIF.
      CLEAR ls_param.
      IF cs_tstcp-param CA '='.
        CHECK sy-fdpos <> 0.
        ASSIGN cs_tstcp-param(sy-fdpos) TO <lg_f>.
        ls_param-field = <lg_f>.
        IF ls_param-field(1) = space.
          SHIFT ls_param-field.
        ENDIF.
        sy-fdpos = sy-fdpos + 1.
        SHIFT cs_tstcp-param BY sy-fdpos PLACES.
        IF cs_tstcp-param CA ';'.
          IF sy-fdpos <> 0.
            ASSIGN cs_tstcp-param(sy-fdpos) TO <lg_f>.
            ls_param-value = <lg_f>.
            IF ls_param-value(1) = space.
              SHIFT ls_param-value.
            ENDIF.
          ENDIF.
          sy-fdpos = sy-fdpos + 1.
          SHIFT cs_tstcp-param BY sy-fdpos PLACES.
          APPEND ls_param TO ct_rsparam.
        ELSE.
          lv_length = strlen( cs_tstcp-param ).
          CHECK lv_length > 0.
          ASSIGN cs_tstcp-param(lv_length) TO <lg_f>.
          ls_param-value = <lg_f>.
          IF ls_param-value(1) = space.
            SHIFT ls_param-value.
          ENDIF.
          lv_length = lv_length + 1.
          SHIFT cs_tstcp-param BY lv_length PLACES.
          APPEND ls_param TO ct_rsparam.
        ENDIF.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD split_parameters.
* see subroutine split_parameters in include LSEUKF01

    DATA: lv_off       TYPE i,
          lv_param_beg TYPE i.


    CLEAR cs_rsstcd-s_vari.

    IF cs_tstcp-param(1) = '\'.             " OO-Transaktion ohne FR
      split_parameters_comp( EXPORTING ig_type = c_oo_program
                                       ig_param = cs_tstcp-param
                             CHANGING  cg_value = cs_tstc-pgmna ).
      split_parameters_comp( EXPORTING ig_type = c_oo_class
                                       ig_param = cs_tstcp-param
                             CHANGING  cg_value = cs_rsstcd-classname ).
      split_parameters_comp( EXPORTING ig_type = c_oo_method
                                       ig_param = cs_tstcp-param
                             CHANGING  cg_value = cs_rsstcd-method ).

      IF NOT cs_tstc-pgmna IS INITIAL.
        cs_rsstcd-s_local = c_true.
      ENDIF.
      RETURN.
    ELSEIF cs_tstcp-param(1) = '@'.         " Transaktionsvariante
      cs_rsstcd-s_vari = c_true.
      IF cs_tstcp-param(2) = '@@'.
        cs_rsstcd-s_ind_vari = c_true.
        lv_off = 2.
      ELSE.
        CLEAR cs_rsstcd-s_ind_vari.
        lv_off = 1.
      ENDIF.
      IF cs_tstcp-param CA ' '.
      ENDIF.
      sy-fdpos = sy-fdpos - lv_off.
      IF sy-fdpos > 0.
        cs_rsstcd-call_tcode = cs_tstcp-param+lv_off(sy-fdpos).
        sy-fdpos = sy-fdpos + 1 + lv_off.
        cs_rsstcd-variant = cs_tstcp-param+sy-fdpos.
      ENDIF.
    ELSEIF cs_tstcp-param(1) = '/'.
      cs_rsstcd-st_tcode = c_true.
      cs_rsstcd-st_prog  = space.
      IF cs_tstcp-param+1(1) = '*'.
        cs_rsstcd-st_skip_1 = c_true.
      ELSE.
        CLEAR cs_rsstcd-st_skip_1.
      ENDIF.
      IF cs_tstcp-param CA ' '.
      ENDIF.
      lv_param_beg = sy-fdpos + 1.
      sy-fdpos = sy-fdpos - 2.
      IF sy-fdpos > 0.
        cs_rsstcd-call_tcode = cs_tstcp-param+2(sy-fdpos).
      ENDIF.
      SHIFT cs_tstcp-param BY lv_param_beg PLACES.
    ELSE.
      cs_rsstcd-st_tcode = space.
      cs_rsstcd-st_prog  = c_true.
    ENDIF.

    shift_param(
      CHANGING ct_rsparam = ct_rsparam
               cs_tstcp   = cs_tstcp ).

    set_oo_parameters(
      EXPORTING it_rsparam = ct_rsparam
      CHANGING cs_rsstcd = cs_rsstcd ).

  ENDMETHOD.


  METHOD split_parameters_comp.
    DATA: lv_off TYPE i.

    IF ig_param CS ig_type.
      lv_off = sy-fdpos + strlen( ig_type ).
      cg_value = ig_param+lv_off.
      IF cg_value CA '\'.
        CLEAR cg_value+sy-fdpos.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD transaction_read.

    DATA: lt_tcodes   TYPE TABLE OF tstc,
          lt_gui_attr TYPE TABLE OF tstcc.

    CLEAR: es_transaction, es_gui_attr.

    CALL FUNCTION 'RPY_TRANSACTION_READ'
      EXPORTING
        transaction      = iv_transaction
      TABLES
        tcodes           = lt_tcodes
        gui_attributes   = lt_gui_attr
      EXCEPTIONS
        permission_error = 1
        cancelled        = 2
        not_found        = 3
        object_not_found = 4
        OTHERS           = 5.
    IF sy-subrc = 4 OR sy-subrc = 3.
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_tcodes INDEX 1 INTO es_transaction.
    ASSERT sy-subrc = 0.
    READ TABLE lt_gui_attr INDEX 1 INTO es_gui_attr.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_transaction TYPE tstc-tcode.


    lv_transaction = ms_item-obj_name.

    CALL FUNCTION 'RPY_TRANSACTION_DELETE'
      EXPORTING
        transaction      = lv_transaction
      EXCEPTIONS
        not_excecuted    = 1
        object_not_found = 0
        OTHERS           = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    CONSTANTS: lc_hex_tra TYPE x VALUE '00',
*               lc_hex_men TYPE x VALUE '01',
               lc_hex_par TYPE x VALUE '02',
               lc_hex_rep TYPE x VALUE '80',
*               lc_hex_rpv TYPE x VALUE '10',
               lc_hex_obj TYPE x VALUE '08'.

    DATA: lv_dynpro       TYPE d020s-dnum,
          ls_tstc         TYPE tstc,
          lv_type         TYPE rglif-docutype,
          ls_tstct        TYPE tstct,
          ls_tstcc        TYPE tstcc,
          ls_tstcp        TYPE tstcp,
          lt_tstca        TYPE ty_tstca,
          lt_param_values TYPE ty_param_values,
          ls_rsstcd       TYPE rsstcd.


    IF zif_abapgit_object~exists( ) = abap_true.
      zif_abapgit_object~delete( iv_package ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TSTC'
                  CHANGING cg_data = ls_tstc ).
    io_xml->read( EXPORTING iv_name = 'TSTCC'
                  CHANGING cg_data = ls_tstcc ).
    io_xml->read( EXPORTING iv_name = 'TSTCT'
                  CHANGING cg_data = ls_tstct ).
    io_xml->read( EXPORTING iv_name = 'TSTCP'
                  CHANGING cg_data = ls_tstcp ).
    io_xml->read( EXPORTING iv_name = 'AUTHORIZATIONS'
                  CHANGING cg_data = lt_tstca ).

    lv_dynpro = ls_tstc-dypno.

    IF     ls_tstc-cinfo O lc_hex_rep.
      lv_type = c_variant_type-report.
    ELSEIF ls_tstc-cinfo O lc_hex_obj.
      lv_type = c_variant_type-object.
    ELSEIF ls_tstc-cinfo O lc_hex_par.
      IF is_variant_transaction( ls_tstcp ) = abap_true.
        lv_type = c_variant_type-variant.
      ELSE.
        lv_type = c_variant_type-parameters.
      ENDIF.
    ELSEIF ls_tstc-cinfo O lc_hex_tra.
      lv_type = c_variant_type-dialog.
    ELSE.
      zcx_abapgit_exception=>raise( 'Transaction, unknown CINFO' ).
    ENDIF.

    IF ls_tstcp IS NOT INITIAL.
      split_parameters( CHANGING ct_rsparam = lt_param_values
                                 cs_rsstcd  = ls_rsstcd
                                 cs_tstcp   = ls_tstcp
                                 cs_tstc    = ls_tstc ).
    ENDIF.

    CASE lv_type.
      WHEN c_variant_type-object.

        deserialize_oo_transaction( iv_package      = iv_package
                                    is_tstc         = ls_tstc
                                    is_tstcc        = ls_tstcc
                                    is_tstct        = ls_tstct
                                    is_rsstcd       = ls_rsstcd ).

      WHEN OTHERS.

        clear_functiongroup_globals( ).

        CALL FUNCTION 'RPY_TRANSACTION_INSERT'
          EXPORTING
            transaction             = ls_tstc-tcode
            program                 = ls_tstc-pgmna
            dynpro                  = lv_dynpro
            language                = mv_language
            development_class       = iv_package
            transaction_type        = lv_type
            shorttext               = ls_tstct-ttext
            called_transaction      = ls_rsstcd-call_tcode
            called_transaction_skip = ls_rsstcd-st_skip_1
            variant                 = ls_rsstcd-variant
            cl_independend          = ls_rsstcd-s_ind_vari
            html_enabled            = ls_tstcc-s_webgui
            java_enabled            = ls_tstcc-s_platin
            wingui_enabled          = ls_tstcc-s_win32
          TABLES
            param_values            = lt_param_values
          EXCEPTIONS
            cancelled               = 1
            already_exist           = 2
            permission_error        = 3
            name_not_allowed        = 4
            name_conflict           = 5
            illegal_type            = 6
            object_inconsistent     = 7
            db_access_error         = 8
            OTHERS                  = 9.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise_t100( ).
        ENDIF.

    ENDCASE.

    IF lt_tstca IS NOT INITIAL.
      save_authorizations( iv_transaction    = ls_tstc-tcode
                           it_authorizations = lt_tstca ).
    ENDIF.

    " Texts deserializing (translations)
    deserialize_texts( io_xml ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_tcode TYPE tstc-tcode.


    SELECT SINGLE tcode FROM tstc INTO lv_tcode
      WHERE tcode = ms_item-obj_name.                   "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = |TN{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = lv_object ).


  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.


    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPLSEUK'.
    <ls_bdcdata>-dynpro   = '0390'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=SHOW'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'TSTC-TCODE'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                 = 'SE93'
        mode_val              = 'E'
      TABLES
        using_tab             = lt_bdcdata
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        resource_failure      = 3
        OTHERS                = 4
        ##fm_subrc_ok.    "#EC CI_SUBRC

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_transaction TYPE tstc-tcode,
          ls_tcode       TYPE tstc,
          ls_tstct       TYPE tstct,
          ls_tstcp       TYPE tstcp,
          lt_tstca       TYPE ty_tstca,
          ls_gui_attr    TYPE tstcc.


    lv_transaction = ms_item-obj_name.

    transaction_read( EXPORTING iv_transaction = lv_transaction
                      IMPORTING es_transaction = ls_tcode
                                es_gui_attr    = ls_gui_attr ).
    IF ls_tcode IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tstct INTO ls_tstct
      WHERE sprsl = mv_language
      AND tcode = lv_transaction.         "#EC CI_SUBRC "#EC CI_GENBUFF

    SELECT SINGLE * FROM tstcp INTO ls_tstcp
      WHERE tcode = lv_transaction.       "#EC CI_SUBRC "#EC CI_GENBUFF

    SELECT * FROM tstca INTO TABLE lt_tstca
      WHERE tcode = lv_transaction.
    IF sy-subrc <> 0.
      CLEAR: lt_tstca.
    ENDIF.

    io_xml->add( iv_name = 'TSTC'
                 ig_data = ls_tcode ).
    io_xml->add( iv_name = 'TSTCC'
                 ig_data = ls_gui_attr ).
    io_xml->add( iv_name = 'TSTCT'
                 ig_data = ls_tstct ).
    IF ls_tstcp IS NOT INITIAL.
      io_xml->add( iv_name = 'TSTCP'
                   ig_data = ls_tstcp ).
    ENDIF.
    io_xml->add( iv_name = 'AUTHORIZATIONS'
                 ig_data = lt_tstca ).

    " Texts serializing (translations)
    serialize_texts( io_xml ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_ttyp IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd40l INTO rv_user
      WHERE typename = ms_item-obj_name
      AND as4local = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    delete_ddic( 'A' ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_name  TYPE ddobjname,
          lt_dd42v TYPE dd42v_tab,
          lt_dd43v TYPE dd43v_tab,
          ls_dd40v TYPE dd40v,
          lv_msg   TYPE string.

    io_xml->read( EXPORTING iv_name = 'DD40V'
                  CHANGING cg_data = ls_dd40v ).

    " DDIC Step: Replace REF TO class/interface with generic reference to avoid cyclic dependency
    IF iv_step = zif_abapgit_object=>gc_step_id-ddic AND ls_dd40v-datatype = 'REF'.
      ls_dd40v-rowtype = 'OBJECT'.
    ELSEIF iv_step = zif_abapgit_object=>gc_step_id-late AND ls_dd40v-datatype <> 'REF'.
      RETURN. " already active
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'DD42V'
                  CHANGING cg_data = lt_dd42v ).
    io_xml->read( EXPORTING iv_name = 'DD43V'
                  CHANGING cg_data = lt_dd43v ).

    corr_insert( iv_package = iv_package
                 ig_object_class = 'DICT' ).

    lv_name = ms_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_TTYP_PUT'
      EXPORTING
        name              = lv_name
        dd40v_wa          = ls_dd40v
      TABLES
        dd42v_tab         = lt_dd42v
        dd43v_tab         = lt_dd43v
      EXCEPTIONS
        ttyp_not_found    = 1
        name_inconsistent = 2
        ttyp_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      lv_msg = |Error in DDIF_TTYP_PUT on object { lv_name }|.

      CASE sy-subrc.
        WHEN 1.
          lv_msg = lv_msg && | (TTYP_NOT_FOUND)|.
        WHEN 2.
          lv_msg = lv_msg && | (NAME_INCONSISTENT)|.
        WHEN 3.
          lv_msg = lv_msg && | (TTYP_INCONSISTENT)|.
        WHEN 4.
          lv_msg = lv_msg && | (PUT_FAILURE)|.
        WHEN 5.
          lv_msg = lv_msg && | (PUT_REFUSED)|.
        WHEN OTHERS.
      ENDCASE.

      zcx_abapgit_exception=>raise( lv_msg ).
    ENDIF.

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_typename TYPE dd40l-typename.

    SELECT SINGLE typename FROM dd40l INTO lv_typename
      WHERE typename = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
    APPEND zif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-ddic = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESDICT'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    jump_se11( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_name  TYPE ddobjname,
          lt_dd42v TYPE dd42v_tab,
          lt_dd43v TYPE dd43v_tab,
          ls_dd40v TYPE dd40v.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_TTYP_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = mv_language
      IMPORTING
        dd40v_wa      = ls_dd40v
      TABLES
        dd42v_tab     = lt_dd42v
        dd43v_tab     = lt_dd43v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF ls_dd40v IS INITIAL.
      zcx_abapgit_exception=>raise( |No active version found for { ms_item-obj_type } { ms_item-obj_name }| ).
    ENDIF.

    CLEAR: ls_dd40v-as4user,
           ls_dd40v-as4date,
           ls_dd40v-as4time.

    IF NOT ls_dd40v-rowkind IS INITIAL.
      CLEAR ls_dd40v-typelen.
    ENDIF.

    io_xml->add( iv_name = 'DD40V'
                 ig_data = ls_dd40v ).
    io_xml->add( iv_name = 'DD42V'
                 ig_data = lt_dd42v ).
    io_xml->add( iv_name = 'DD43V'
                 ig_data = lt_dd43v ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_w3xx_super IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_item = is_item
                        iv_language = iv_language ).
    ms_key-relid = ms_item-obj_type+2(2).
    ms_key-objid = ms_item-obj_name.
  ENDMETHOD.


  METHOD find_param.

    FIELD-SYMBOLS <ls_param> LIKE LINE OF it_params.


    READ TABLE it_params ASSIGNING <ls_param> WITH KEY name = iv_name.
    IF sy-subrc > 0.
      zcx_abapgit_exception=>raise( |W3xx: Cannot find { iv_name } for { ms_key-objid }| ).
    ENDIF.

    rv_value = <ls_param>-value.

  ENDMETHOD.


  METHOD get_ext.

    rv_ext = find_param( it_params = it_params
                         iv_name = c_param_names-fileext ).
    SHIFT rv_ext LEFT DELETING LEADING '.'.

  ENDMETHOD.


  METHOD get_metadata.
    rs_metadata              = super->get_metadata( ).
    rs_metadata-version      = 'v2.0.0'. " Serialization v2, separate data file
    rs_metadata-delete_tadir = abap_true.
  ENDMETHOD.


  METHOD normalize_params.

    FIELD-SYMBOLS <ls_param> LIKE LINE OF ct_params.

    " Ensure filesize param exists
    READ TABLE ct_params ASSIGNING <ls_param> WITH KEY name = c_param_names-filesize.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO ct_params ASSIGNING <ls_param>.
      <ls_param>-name  = c_param_names-filesize.
    ENDIF.

    LOOP AT ct_params ASSIGNING <ls_param>.
      <ls_param>-relid = ms_key-relid. " Ensure param key = object key
      <ls_param>-objid = ms_key-objid.
      IF <ls_param>-name = c_param_names-filesize. " Patch filesize = real file size
        <ls_param>-value = iv_size.
        CONDENSE <ls_param>-value.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD strip_params.

    FIELD-SYMBOLS <ls_param> LIKE LINE OF ct_params.

    " Remove path from filename
    find_param( it_params = ct_params
                iv_name = c_param_names-filename ). " Check exists
    READ TABLE ct_params ASSIGNING <ls_param> WITH KEY name = c_param_names-filename.
    <ls_param>-value = zcl_abapgit_path=>get_filename_from_syspath( |{ <ls_param>-value }| ).

    " Clear version & filesize
    DELETE ct_params WHERE name = c_param_names-version.
    DELETE ct_params WHERE name = c_param_names-filesize.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE chname INTO rv_user
      FROM wwwdata
      WHERE relid = ms_key-relid
      AND objid = ms_key-objid
      AND srtf2 = 0.

    IF sy-subrc IS NOT INITIAL OR rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    CALL FUNCTION 'WWWDATA_DELETE'
      EXPORTING
        key               = ms_key
      EXCEPTIONS
        wrong_object_type = 1
        delete_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      zcx_abapgit_exception=>raise( 'Cannot delete W3xx data' ).
    ENDIF.

    CALL FUNCTION 'WWWPARAMS_DELETE_ALL'
      EXPORTING
        key          = ms_key
      EXCEPTIONS
        delete_error = 1.

    IF sy-subrc IS NOT INITIAL.
      zcx_abapgit_exception=>raise( 'Cannot delete W3xx params' ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA lv_base64str TYPE string.
    DATA lt_w3params  TYPE STANDARD TABLE OF wwwparams.
    DATA lv_xstring   TYPE xstring.
    DATA lt_w3mime    TYPE STANDARD TABLE OF w3mime.
    DATA lt_w3html    TYPE STANDARD TABLE OF w3html.
    DATA lv_size      TYPE i.
    DATA lv_tadir_obj TYPE tadir-object.


    io_xml->read( EXPORTING iv_name = 'TEXT'
                  CHANGING  cg_data = ms_key-text ).

    io_xml->read( EXPORTING iv_name = 'PARAMS'
                  CHANGING  cg_data = lt_w3params ).

    CASE io_xml->get_metadata( )-version.
      WHEN 'v1.0.0'.
        io_xml->read( EXPORTING iv_name = 'DATA'
                      CHANGING  cg_data = lv_base64str ).
        lv_xstring = cl_http_utility=>decode_x_base64( lv_base64str ).
      WHEN 'v2.0.0'.
        lv_xstring = zif_abapgit_object~mo_files->read_raw( iv_extra = 'data'
                                                    iv_ext   = get_ext( lt_w3params ) ).
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( 'W3xx: Unknown serializer version' ).
    ENDCASE.

    CASE ms_key-relid.
      WHEN 'MI'.
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = lv_xstring
          IMPORTING
            output_length = lv_size
          TABLES
            binary_tab    = lt_w3mime.
      WHEN 'HT'.
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = lv_xstring
          IMPORTING
            output_length = lv_size
          TABLES
            binary_tab    = lt_w3mime.

        CALL FUNCTION 'SCMS_BINARY_TO_TEXT'
          EXPORTING
            input_length  = lv_size
          IMPORTING
            output_length = lv_size
          TABLES
            binary_tab    = lt_w3mime
            text_tab      = lt_w3html
          EXCEPTIONS
            failed        = 1.
        IF sy-subrc IS NOT INITIAL.
          zcx_abapgit_exception=>raise( 'Cannot update W3xx params' ).
        ENDIF.

        CLEAR lt_w3mime.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( 'Wrong W3xx type' ).
    ENDCASE.

    " Update size of file based on actual data file size, prove param object name
    normalize_params( EXPORTING iv_size   = lv_size
                      CHANGING  ct_params = lt_w3params ).

    CALL FUNCTION 'WWWPARAMS_UPDATE'
      TABLES
        params       = lt_w3params
      EXCEPTIONS
        update_error = 1.

    IF sy-subrc IS NOT INITIAL.
      zcx_abapgit_exception=>raise( 'Cannot update W3xx params' ).
    ENDIF.

    ms_key-tdate    = sy-datum.
    ms_key-ttime    = sy-uzeit.
    ms_key-chname   = sy-uname.
    ms_key-devclass = iv_package.

    CALL FUNCTION 'WWWDATA_EXPORT'
      EXPORTING
        key               = ms_key
      TABLES
        mime              = lt_w3mime
        html              = lt_w3html
      EXCEPTIONS
        wrong_object_type = 1
        export_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      zcx_abapgit_exception=>raise( 'Cannot upload W3xx data' ).
    ENDIF.

    CONCATENATE 'W3' ms_key-relid INTO lv_tadir_obj.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid                 = 'R3TR'
        wi_tadir_object                = lv_tadir_obj
        wi_tadir_devclass              = iv_package
        wi_tadir_obj_name              = ms_key-objid
        wi_test_modus                  = space
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 99.
    IF sy-subrc IS NOT INITIAL.
      zcx_abapgit_exception=>raise( 'Cannot update TADIR for W3xx' ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    SELECT SINGLE objid INTO ms_key-objid
      FROM wwwdata
      WHERE relid = ms_key-relid
      AND objid = ms_key-objid
      AND srtf2 = 0.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = |{ ms_item-obj_type+2(2) }{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_WWW_HTML'
                                            iv_argument    = lv_object ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA: ls_bdcdata TYPE bdcdata,
          lt_bdcdata TYPE ty_bdcdata.

    ls_bdcdata-program  = 'SAPMWWW0'.
    ls_bdcdata-dynpro   = '0100'.
    ls_bdcdata-dynbegin = 'X'.
    APPEND ls_bdcdata TO lt_bdcdata.

    change_bdc_jump_data( CHANGING ct_bdcdata = lt_bdcdata ).

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = 'BDC_OKCODE'.
    ls_bdcdata-fval = '=CRO1'.
    APPEND ls_bdcdata TO lt_bdcdata.

    ls_bdcdata-program  = 'RSWWWSHW'.
    ls_bdcdata-dynpro   = '1000'.
    ls_bdcdata-dynbegin = 'X'.
    APPEND ls_bdcdata TO lt_bdcdata.

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam     = 'SO_OBJID-LOW'.
    ls_bdcdata-fval     = ms_item-obj_name.
    APPEND ls_bdcdata TO lt_bdcdata.

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = 'BDC_OKCODE'.
    ls_bdcdata-fval = '=ONLI'.
    APPEND ls_bdcdata TO lt_bdcdata.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode     = 'SMW0'
        mode_val  = 'E'
      TABLES
        using_tab = lt_bdcdata
      EXCEPTIONS
        OTHERS    = 1.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from ABAP4_CALL_TRANSACTION, SE35' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA lt_w3mime    TYPE STANDARD TABLE OF w3mime.
    DATA lt_w3html    TYPE STANDARD TABLE OF w3html.
    DATA lt_w3params  TYPE STANDARD TABLE OF wwwparams.
    DATA lv_xstring   TYPE xstring.
    DATA lv_size      TYPE i.

    SELECT SINGLE * INTO CORRESPONDING FIELDS OF ms_key
      FROM wwwdata
      WHERE relid = ms_key-relid
      AND objid = ms_key-objid
      AND srtf2 = 0.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ms_key
      TABLES
        mime              = lt_w3mime
        html              = lt_w3html
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      zcx_abapgit_exception=>raise( 'Cannot read W3xx data' ).
    ENDIF.

    CALL FUNCTION 'WWWPARAMS_READ_ALL'
      EXPORTING
        type             = ms_key-relid
        objid            = ms_key-objid
      TABLES
        params           = lt_w3params
      EXCEPTIONS
        entry_not_exists = 1.

    IF sy-subrc IS NOT INITIAL.
      zcx_abapgit_exception=>raise( 'Cannot read W3xx data' ).
    ENDIF.

    lv_size = find_param( it_params = lt_w3params
                          iv_name = c_param_names-filesize ).
    " Clean params (remove version, filesize & clear filename from path)
    strip_params( CHANGING  ct_params = lt_w3params ).

    CASE ms_key-relid.
      WHEN 'MI'.
        CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
          EXPORTING
            input_length = lv_size
          IMPORTING
            buffer       = lv_xstring
          TABLES
            binary_tab   = lt_w3mime
          EXCEPTIONS
            failed       = 1.
      WHEN 'HT'.
        CALL FUNCTION 'SCMS_TEXT_TO_XSTRING'
          IMPORTING
            buffer   = lv_xstring
          TABLES
            text_tab = lt_w3html
          EXCEPTIONS
            failed   = 1.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( 'Wrong W3xx type' ).
    ENDCASE.

    IF sy-subrc IS NOT INITIAL.
      zcx_abapgit_exception=>raise( 'Cannot convert W3xx to xstring' ).
    ENDIF.

    io_xml->add( iv_name = 'NAME'
                 ig_data = ms_key-objid ).

    io_xml->add( iv_name = 'TEXT'
                 ig_data = ms_key-text ).

    io_xml->add( iv_name = 'PARAMS'
                 ig_data = lt_w3params ).

    " Seriazation v2, separate data file. 'extra' added to prevent conflict with .xml
    zif_abapgit_object~mo_files->add_raw( iv_data  = lv_xstring
                                  iv_extra = 'data'
                                  iv_ext   = get_ext( lt_w3params ) ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_w3ht IMPLEMENTATION.


  METHOD change_bdc_jump_data.

    DATA: ls_bdcdata LIKE LINE OF ct_bdcdata.

    ls_bdcdata-fnam = 'RADIO_HT'.
    ls_bdcdata-fval = 'X'.
    APPEND ls_bdcdata TO ct_bdcdata.

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = 'RADIO_MI'.
    ls_bdcdata-fval = ' '.
    APPEND ls_bdcdata TO ct_bdcdata.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_w3mi IMPLEMENTATION.


  METHOD change_bdc_jump_data.

    DATA: ls_bdcdata LIKE LINE OF ct_bdcdata.

    ls_bdcdata-fnam = 'RADIO_HT'.
    ls_bdcdata-fval = ' '.
    APPEND ls_bdcdata TO ct_bdcdata.

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = 'RADIO_MI'.
    ls_bdcdata-fval = 'X'.
    APPEND ls_bdcdata TO ct_bdcdata.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_oo_interface IMPLEMENTATION.


  METHOD init_scanner.

    DATA: lx_exc       TYPE REF TO cx_root,
          lv_message   TYPE string,
          lv_classname TYPE abap_abstypename.

    FIELD-SYMBOLS: <lv_line> TYPE i.

    TRY.
        ro_scanner = cl_oo_source_scanner_interface=>create_interface_scanner(
          clif_name = iv_name
          source    = it_source ).
        ro_scanner->scan( ).
      CATCH cx_clif_scan_error.
        zcx_abapgit_exception=>raise( 'error initializing INTF scanner' ).
      CATCH cx_root INTO lx_exc.
        lv_classname = cl_abap_classdescr=>get_class_name( lx_exc ).
        IF lv_classname = '\CLASS=CX_OO_CLIF_SCAN_ERROR_DETAIL'.
          ASSIGN lx_exc->('SOURCE_POSITION-LINE') TO <lv_line>.
          ASSERT sy-subrc = 0.
          lv_message = |{ lx_exc->get_text( ) }, line { <lv_line> }|.
        ELSE.
          lv_message = lx_exc->get_text( ).
        ENDIF.
        zcx_abapgit_exception=>raise( lv_message ).
    ENDTRY.

  ENDMETHOD.


  METHOD update_meta.

    DATA: lo_update     TYPE REF TO cl_oo_interface_section_source,
          ls_clskey     TYPE seoclskey,
          lv_scan_error TYPE abap_bool.


    ls_clskey-clsname = iv_name.

    TRY.
        CALL FUNCTION 'SEO_BUFFER_REFRESH'
          EXPORTING
            cifkey  = ls_clskey
            version = seoc_version_active.
        CREATE OBJECT lo_update TYPE ('CL_OO_INTERFACE_SECTION_SOURCE')
          EXPORTING
            intkey                        = ls_clskey
            state                         = 'A'
            source                        = it_source
          EXCEPTIONS
            interface_not_existing        = 1
            read_source_error             = 2
            OTHERS                        = 3 ##SUBRC_OK.
      CATCH cx_sy_dyn_call_param_not_found.
* downport to 702, see https://github.com/abapGit/abapGit/issues/933
* this will READ REPORT instead of using it_source, which should be okay
        CREATE OBJECT lo_update TYPE cl_oo_interface_section_source
          EXPORTING
            intkey                 = ls_clskey
            state                  = 'A'
          EXCEPTIONS
            interface_not_existing = 1
            read_source_error      = 2
            OTHERS                 = 3.
    ENDTRY.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lo_update->set_dark_mode( abap_true ).

    lo_update->scan_section_source(
      RECEIVING
        scan_error             = lv_scan_error
      EXCEPTIONS
        scan_abap_source_error = 1
        OTHERS                 = 2 ).
    IF sy-subrc <> 0 OR lv_scan_error = abap_true.
      zcx_abapgit_exception=>raise( |INTF, error while scanning source. Subrc = { sy-subrc }| ).
    ENDIF.

* this will update the SEO* database tables
    lo_update->revert_scan_result( ).

  ENDMETHOD.


  METHOD update_report.

    DATA: lt_old TYPE string_table.

    READ REPORT iv_program INTO lt_old.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Fatal error. Include { iv_program } should have been created previously!| ).
    ENDIF.

    IF lt_old <> it_source.
      INSERT REPORT iv_program FROM it_source.
      ASSERT sy-subrc = 0.
      rv_updated = abap_true.
    ELSE.
      rv_updated = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~create.
    DATA: lt_vseoattrib TYPE seoo_attributes_r.
    FIELD-SYMBOLS: <lv_clsname> TYPE seoclsname.

    ASSIGN COMPONENT 'CLSNAME' OF STRUCTURE cg_properties TO <lv_clsname>.
    ASSERT sy-subrc = 0.

    lt_vseoattrib = convert_attrib_to_vseoattrib(
                      iv_clsname    = <lv_clsname>
                      it_attributes = it_attributes ).

    TRY.
        CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = iv_overwrite
            version         = seoc_version_active
            suppress_dialog = abap_true " Parameter missing in 702
          CHANGING
            interface       = cg_properties
            attributes      = lt_vseoattrib
          EXCEPTIONS
            existing        = 1
            is_class        = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
      CATCH cx_sy_dyn_call_param_not_found.
        CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = iv_overwrite
            version         = seoc_version_active
          CHANGING
            interface       = cg_properties
            attributes      = lt_vseoattrib
          EXCEPTIONS
            existing        = 1
            is_class        = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
    ENDTRY.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~delete.
    CALL FUNCTION 'SEO_INTERFACE_DELETE_COMPLETE'
      EXPORTING
        intkey       = is_deletion_key
      EXCEPTIONS
        not_existing = 1
        is_class     = 2
        db_error     = 3
        no_access    = 4
        other        = 5
        OTHERS       = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~deserialize_source.

    DATA: lv_updated TYPE abap_bool,
          lv_program TYPE program,
          lo_scanner TYPE REF TO cl_oo_source_scanner_interface,
          lt_public  TYPE seop_source_string.

    "Buffer needs to be refreshed,
    "otherwise standard SAP CLIF_SOURCE reorder methods alphabetically
    CALL FUNCTION 'SEO_BUFFER_INIT'.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        cifkey  = is_key
        version = seoc_version_inactive.

    lo_scanner = init_scanner(
      it_source = it_source
      iv_name   = is_key-clsname ).

    lt_public = lo_scanner->get_interface_section_source( ).
    IF lt_public IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_intfsec_name( is_key-clsname ).
      lv_updated = update_report( iv_program = lv_program
                                  it_source  = lt_public ).
      IF lv_updated = abap_true.
        update_meta( iv_name   = is_key-clsname
                     it_source = lt_public ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_includes.
    DATA lv_interface_name TYPE seoclsname.
    lv_interface_name = iv_object_name.
    APPEND cl_oo_classname_service=>get_interfacepool_name( lv_interface_name ) TO rt_includes.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_interface_properties.
    CALL FUNCTION 'SEO_CLIF_GET'
      EXPORTING
        cifkey       = is_interface_key
        version      = seoc_version_active
      IMPORTING
        interface    = rs_interface_properties
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc = 1.
      RETURN. " in case only inactive version exists
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_oo_factory IMPLEMENTATION.


  METHOD make.
    IF gi_object_oriented_object IS BOUND.
      ri_object_oriented_object = gi_object_oriented_object.
      RETURN.
    ENDIF.
    IF iv_object_type = 'CLAS'.
      CREATE OBJECT ri_object_oriented_object TYPE zcl_abapgit_oo_class.
    ELSEIF iv_object_type = 'INTF'.
      CREATE OBJECT ri_object_oriented_object TYPE zcl_abapgit_oo_interface.
    ENDIF.
  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_oo_serializer IMPLEMENTATION.


  METHOD are_test_classes_skipped.
    rv_return = mv_skip_testclass.
  ENDMETHOD.


  METHOD calculate_skip_testclass.

    DATA: lv_line1 LIKE LINE OF it_source,
          lv_line2 LIKE LINE OF it_source.

* when creating classes in Eclipse it automatically generates the
* testclass include, but it is not needed, so skip to avoid
* creating an extra file in the repository.
* Also remove it if the content is manually removed, but
* the class still thinks it contains tests

    rv_skip_testclass = abap_false.
    IF lines( it_source ) = 2.
      READ TABLE it_source INDEX 1 INTO lv_line1.
      ASSERT sy-subrc = 0.
      READ TABLE it_source INDEX 2 INTO lv_line2.
      ASSERT sy-subrc = 0.
      IF strlen( lv_line1 ) >= 3 AND lv_line1(3) = '*"*' AND lv_line2 IS INITIAL.
        rv_skip_testclass = abap_true.
      ENDIF.
    ELSEIF lines( it_source ) = 1.
      READ TABLE it_source INDEX 1 INTO lv_line1.
      ASSERT sy-subrc = 0.
      IF lv_line1 IS INITIAL
          OR ( strlen( lv_line1 ) >= 3 AND lv_line1(3) = '*"*' )
          OR ( strlen( lv_line1 ) = 1 AND lv_line1(1) = '*' ).
        rv_skip_testclass = abap_true.
      ENDIF.
    ELSEIF lines( it_source ) = 0.
      rv_skip_testclass = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD read_include.

    DATA: ls_include TYPE progstruc.


    ASSERT iv_type = seop_ext_class_locals_def
      OR iv_type = seop_ext_class_locals_imp
      OR iv_type = seop_ext_class_macros
      OR iv_type = seop_ext_class_testclasses.

    ls_include-rootname = is_clskey-clsname.
    TRANSLATE ls_include-rootname USING ' ='.
    ls_include-categorya = iv_type(1).
    ls_include-codea = iv_type+1(4).

* it looks like there is an issue in function module SEO_CLASS_GET_INCLUDE_SOURCE
* on 750 kernels, where the READ REPORT without STATE addition does not
* return the active version, this method is a workaround for this issue
    READ REPORT ls_include INTO rt_source STATE 'A'.

  ENDMETHOD.


  METHOD reduce.

    DATA: lv_source LIKE LINE OF ct_source,
          lv_found  TYPE abap_bool.


* skip files that only contain the standard comments
    lv_found = abap_false.
    LOOP AT ct_source INTO lv_source.
      IF strlen( lv_source ) >= 3 AND lv_source(3) <> '*"*'.
        lv_found = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_found = abap_false.
      CLEAR ct_source.
    ENDIF.

  ENDMETHOD.


  METHOD remove_signatures.

* signatures messes up in CL_OO_SOURCE when deserializing and serializing
* within same session

    DATA: lv_begin  TYPE string,
          lv_end    TYPE string,
          lv_remove TYPE abap_bool,
          lv_source LIKE LINE OF ct_source.

    "@TODO: Put under test
    CONCATENATE '* <SIGNATURE>------------------------------------'
      '---------------------------------------------------+'
      INTO lv_begin.

    CONCATENATE '* +------------------------------------------------'
      '--------------------------------------</SIGNATURE>'
      INTO lv_end.

    lv_remove = abap_false.
    LOOP AT ct_source INTO lv_source.
      IF lv_source = lv_begin.
        lv_remove = abap_true.
      ENDIF.
      IF lv_remove = abap_true.
        DELETE ct_source INDEX sy-tabix.
      ENDIF.
      IF lv_source = lv_end.
        lv_remove = abap_false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_abap_clif_source.
    rt_source = zcl_abapgit_exit=>get_instance( )->custom_serialize_abap_clif( is_class_key ).
    IF rt_source IS NOT INITIAL.
      RETURN.
    ENDIF.

    TRY.
        rt_source = serialize_abap_new( is_class_key ).
      CATCH cx_sy_dyn_call_error.
        rt_source = serialize_abap_old( is_class_key ).
    ENDTRY.
  ENDMETHOD.


  METHOD serialize_abap_new.

    DATA: lo_source   TYPE REF TO object,
          lo_instance TYPE REF TO object.

* do not call the class/methods statically, as it will
* give syntax errors on old versions
    CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
      RECEIVING
        result = lo_instance.

    CALL METHOD lo_instance->('CREATE_CLIF_SOURCE')
      EXPORTING
        clif_name = is_clskey-clsname
        version   = 'A'
      RECEIVING
        result    = lo_source.

    CALL METHOD lo_source->('GET_SOURCE')
      IMPORTING
        source = rt_source.

  ENDMETHOD.


  METHOD serialize_abap_old.
* for old ABAP AS versions
    DATA: lo_source TYPE REF TO cl_oo_source.

    CREATE OBJECT lo_source
      EXPORTING
        clskey             = is_clskey
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lo_source->read( 'A' ).
    rt_source = lo_source->get_old_source( ).
    remove_signatures( CHANGING ct_source = rt_source ).

  ENDMETHOD.


  METHOD serialize_locals_def.

    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_locals_def ).

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.


  METHOD serialize_locals_imp.

    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_locals_imp ).

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.


  METHOD serialize_macros.

    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_macros ).

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.


  METHOD serialize_testclasses.

    DATA ls_vseoclass TYPE vseoclass.

    CALL FUNCTION 'SEO_CLIF_GET'
      EXPORTING
        cifkey       = is_clskey
        version      = seoc_version_active
      IMPORTING
        class        = ls_vseoclass
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc <> 0 OR ls_vseoclass-with_unit_tests = abap_false.
      mv_skip_testclass = abap_true.
      RETURN.
    ENDIF.

    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_testclasses ).

    mv_skip_testclass = calculate_skip_testclass( rt_source ).

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_PATH IMPLEMENTATION.


  METHOD change_dir.

    DATA: lv_last TYPE i,
          lv_temp TYPE string.

    lv_last = strlen( iv_cur_dir ) - 1.

    IF iv_cd = '' OR iv_cd = '.'. " No change
      rv_path = iv_cur_dir.
    ELSEIF iv_cd+0(1) = '/'.      " Absolute path
      rv_path = iv_cd.
    ELSEIF iv_cd = '..'.          " CD back
      IF iv_cur_dir = '/' OR iv_cur_dir = ''. " Back from root = root
        rv_path = iv_cur_dir.
      ELSE.
        lv_temp = reverse( iv_cur_dir ).
        IF lv_temp+0(1) = '/'.
          SHIFT lv_temp BY 1 PLACES LEFT.
        ENDIF.
        SHIFT lv_temp UP TO '/' LEFT.
        rv_path = reverse( lv_temp ).
      ENDIF.
    ELSEIF iv_cur_dir+lv_last(1) = '/'.  " Append cd to cur_dir separated by /
      rv_path = iv_cur_dir && iv_cd.
    ELSE.
      rv_path = iv_cur_dir && '/' && iv_cd.
    ENDIF.

    " TODO: improve logic and cases

  ENDMETHOD.


  METHOD get_filename_from_syspath.

    DATA: lv_split TYPE c LENGTH 1,
          lv_index TYPE i,
          lt_split TYPE TABLE OF string.

    " filename | c:\filename | /dir/filename | \\server\filename
    IF iv_path CA '/'.
      lv_split = '/'.
    ELSE.
      lv_split = '\'.
    ENDIF.

    SPLIT iv_path AT lv_split INTO TABLE lt_split.

    lv_index = lines( lt_split ).

    READ TABLE lt_split INDEX lv_index INTO rv_filename.

  ENDMETHOD.


  METHOD is_root.
    rv_yes = boolc( iv_path = '/' ).
  ENDMETHOD.


  METHOD is_subdir.

    DATA lv_len  TYPE i.
    DATA lv_last TYPE i.

    lv_len  = strlen( iv_parent ).
    lv_last = lv_len - 1.
    rv_yes  = boolc( strlen( iv_path ) > lv_len
                 AND iv_path+0(lv_len) = iv_parent
                 AND ( iv_parent+lv_last(1) = '/' OR iv_path+lv_len(1) = '/' ) ).

  ENDMETHOD.


  METHOD split_file_location.

    DATA: lv_cnt TYPE i,
          lv_len TYPE i.

    FIND FIRST OCCURRENCE OF REGEX '^/(.*/)?' IN iv_fullpath
      MATCH COUNT lv_cnt
      MATCH LENGTH lv_len.

    IF lv_cnt > 0.
      ev_path     = iv_fullpath+0(lv_len).
      ev_filename = iv_fullpath+lv_len.
    ELSE.
      CLEAR ev_path.
      ev_filename = iv_fullpath.
    ENDIF.

    ev_filename = cl_http_utility=>unescape_url( escaped = ev_filename ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_progress IMPLEMENTATION.


  METHOD calc_pct.

    DATA: lv_f TYPE f.

    lv_f = ( iv_current / mv_total ) * 100.
    rv_pct = lv_f.

    IF rv_pct = 100.
      rv_pct = 99.
    ELSEIF rv_pct = 0.
      rv_pct = 1.
    ENDIF.

  ENDMETHOD.


  METHOD get_instance.

* max one progress indicator at a time is supported

    IF gi_progress IS INITIAL.
      CREATE OBJECT gi_progress TYPE zcl_abapgit_progress.
    ENDIF.

    gi_progress->set_total( iv_total ).

    ri_progress = gi_progress.

  ENDMETHOD.


  METHOD set_instance.

    gi_progress = ii_progress.

  ENDMETHOD.


  METHOD zif_abapgit_progress~off.

    " Clear the status bar
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'.

  ENDMETHOD.


  METHOD zif_abapgit_progress~set_total.

    mv_total = iv_total.

    CLEAR mv_cv_time_next.
    CLEAR mv_cv_datum_next.

  ENDMETHOD.


  METHOD zif_abapgit_progress~show.

    DATA: lv_pct  TYPE i,
          lv_time TYPE t.

    CONSTANTS: lc_wait_secs TYPE i VALUE 2.

    GET TIME.
    lv_time = sy-uzeit.
    IF mv_cv_time_next IS INITIAL AND mv_cv_datum_next IS INITIAL.
      mv_cv_time_next  = lv_time.
      mv_cv_datum_next = sy-datum.
    ENDIF.

    "We only do a progress indication if enough time has passed
    IF lv_time >= mv_cv_time_next
        AND sy-datum = mv_cv_datum_next
        OR sy-datum > mv_cv_datum_next.

      lv_pct = calc_pct( iv_current ).

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = lv_pct
          text       = iv_text.
      mv_cv_time_next = lv_time + lc_wait_secs.

    ENDIF.
    IF sy-datum > mv_cv_datum_next.
      mv_cv_datum_next = sy-datum.
    ENDIF.
    IF mv_cv_time_next < lv_time.
      mv_cv_datum_next = sy-datum + 1.
    ENDIF.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_sap_package IMPLEMENTATION.


  METHOD constructor.
    mv_package = iv_package.
  ENDMETHOD.


  METHOD zif_abapgit_sap_package~are_changes_recorded_in_tr_req.

    DATA: li_package TYPE REF TO if_package.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = mv_package
      IMPORTING
        e_package                  = li_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5
        OTHERS                     = 6 ).

    CASE sy-subrc.
      WHEN 0.
        rv_are_changes_rec_in_tr_req = li_package->wbo_korr_flag.
      WHEN 1.
        " For new packages, derive from package name
        rv_are_changes_rec_in_tr_req = boolc( mv_package(1) <> '$' ).
      WHEN OTHERS.
        zcx_abapgit_exception=>raise_t100( ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_sap_package~create.

    DATA: lv_err     TYPE string,
          li_package TYPE REF TO if_package,
          ls_package LIKE is_package.


    ASSERT NOT is_package-devclass IS INITIAL.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = is_package-devclass
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5 ).
    IF sy-subrc = 0.
      " Package already exists. We assume this is fine. Its properties might be changed later at
      " DEVC deserialization.
      RETURN.
    ENDIF.

    ls_package = is_package.

    " Set software component to 'HOME' if none is set at this point.
    " Otherwise SOFTWARE_COMPONENT_INVALID will be raised.
    IF ls_package-dlvunit IS INITIAL.
      ls_package-dlvunit = 'HOME'.
    ENDIF.

    " For transportable packages, get default transport and layer
    IF ls_package-devclass(1) <> '$' AND ls_package-pdevclass IS INITIAL.
      ls_package-pdevclass = zif_abapgit_sap_package~get_transport_layer( ).
    ENDIF.

    cl_package_factory=>create_new_package(
      EXPORTING
        i_reuse_deleted_object     = abap_true
*        i_suppress_dialog          = abap_true " does not exist in 730
      IMPORTING
        e_package                  = li_package
      CHANGING
        c_package_data             = ls_package
      EXCEPTIONS
        object_already_existing    = 1
        object_just_created        = 2
        not_authorized             = 3
        wrong_name_prefix          = 4
        undefined_name             = 5
        reserved_local_name        = 6
        invalid_package_name       = 7
        short_text_missing         = 8
        software_component_invalid = 9
        layer_invalid              = 10
        author_not_existing        = 11
        component_not_existing     = 12
        component_missing          = 13
        prefix_in_use              = 14
        unexpected_error           = 15
        intern_err                 = 16
        no_access                  = 17
*        invalid_translation_depth  = 18
*        wrong_mainpack_value       = 19
*        superpackage_invalid       = 20
*        error_in_cts_checks        = 21
        OTHERS                     = 18 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    li_package->save(
*      EXPORTING
*        i_suppress_dialog     = abap_true    " Controls whether popups can be transmitted
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        cancelled_in_corr     = 3
        permission_failure    = 4
        unexpected_error      = 5
        intern_err            = 6
        OTHERS                = 7 ).
    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_err.

      " Here we have to delete the package,
      " otherwise it would remain in the memory
      " and cannot created again in this session.
      li_package->delete(
        EXCEPTIONS
          object_not_empty      = 1
          object_not_changeable = 2
          object_invalid        = 3
          intern_err            = 4
          OTHERS                = 5 ).

      zcx_abapgit_exception=>raise( lv_err ).

    ENDIF.

    li_package->set_changeable( abap_false ).

  ENDMETHOD.


  METHOD zif_abapgit_sap_package~create_child.

    DATA: li_parent TYPE REF TO if_package,
          ls_child  TYPE scompkdtln.


    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = mv_package
      IMPORTING
        e_package                  = li_parent
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ls_child-devclass  = iv_child.
    ls_child-dlvunit   = li_parent->software_component.
    ls_child-component = li_parent->application_component.
    ls_child-ctext     = iv_child.
    ls_child-parentcl  = mv_package.
    ls_child-pdevclass = li_parent->transport_layer.
    ls_child-as4user   = sy-uname.

    zif_abapgit_sap_package~create( ls_child ).

  ENDMETHOD.


  METHOD zif_abapgit_sap_package~create_local.

    DATA: ls_package TYPE scompkdtln.


    ls_package-devclass  = mv_package.
    ls_package-ctext     = mv_package.
    ls_package-parentcl  = '$TMP'.
    ls_package-dlvunit   = 'LOCAL'.
    ls_package-as4user   = sy-uname.

    zif_abapgit_sap_package~create( ls_package ).

  ENDMETHOD.


  METHOD zif_abapgit_sap_package~exists.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = mv_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5 ).
    rv_bool = boolc( sy-subrc <> 1 ).

  ENDMETHOD.


  METHOD zif_abapgit_sap_package~get_transport_layer.

    " Get default transport layer
    CALL FUNCTION 'TR_GET_TRANSPORT_TARGET'
      EXPORTING
        iv_use_default             = abap_true
        iv_get_layer_only          = abap_true
      IMPORTING
        ev_layer                   = rv_transport_layer
      EXCEPTIONS
        wrong_call                 = 1
        invalid_input              = 2
        cts_initialization_failure = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      " Return empty layer (i.e. "local workbench request" for the package)
      CLEAR rv_transport_layer.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_sap_package~get_transport_type.

    DATA:
      lv_pkg_name TYPE e071-obj_name,
      lv_obj_name TYPE tadir-obj_name,
      lv_role     TYPE trnrole.

    lv_pkg_name = lv_obj_name = mv_package.

    CALL FUNCTION 'TR_GET_REQUEST_TYPE'
      EXPORTING
        iv_pgmid          = 'R3TR'
        iv_object         = 'DEVC'
        iv_obj_name       = lv_pkg_name
      IMPORTING
        ev_request_type   = rs_transport_type-request
        ev_task_type      = rs_transport_type-task
      EXCEPTIONS
        no_request_needed = 1
        invalid_object    = 2
        system_error      = 3
        OTHERS            = 4.

    CASE sy-subrc.
      WHEN 0 OR 1.
        RETURN.
      WHEN 2.
        " For new packages, set to workbench request
        rs_transport_type-request = 'K'.

        CALL FUNCTION 'TR_GET_NAMESPACE_AND_ROLE'
          EXPORTING
            iv_pgmid                   = 'R3TR'
            iv_object                  = 'DEVC'
            iv_objname                 = lv_obj_name
          IMPORTING
            ev_role                    = lv_role
          EXCEPTIONS
            namespace_not_existing     = 1
            invalid_object             = 2
            namespace_not_determinable = 3
            OTHERS                     = 4.
        IF sy-subrc = 0 AND lv_role = 'C'.
          " Namespace with repair license requires repair task
          rs_transport_type-task = 'R'.
        ELSE.
          " Otherweise use correction task
          rs_transport_type-task = 'S'.
        ENDIF.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise_t100( ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_sap_package~list_subpackages.

    DATA: lt_list     LIKE rt_list.

    SELECT devclass FROM tdevc
      INTO TABLE lt_list
      WHERE parentcl = mv_package
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF

    rt_list = lt_list.
    WHILE lines( lt_list ) > 0.

      SELECT devclass FROM tdevc
        INTO TABLE lt_list
        FOR ALL ENTRIES IN lt_list
        WHERE parentcl = lt_list-table_line
        ORDER BY PRIMARY KEY.             "#EC CI_SUBRC "#EC CI_GENBUFF
      APPEND LINES OF lt_list TO rt_list.

    ENDWHILE.

  ENDMETHOD.


  METHOD zif_abapgit_sap_package~list_superpackages.

    DATA: lt_list   LIKE rt_list,
          lv_parent TYPE tdevc-parentcl.


    APPEND mv_package TO rt_list.

    lv_parent = zif_abapgit_sap_package~read_parent( ).

    IF sy-subrc = 0 AND NOT lv_parent IS INITIAL.
      lt_list = zcl_abapinst_factory=>get_sap_package( lv_parent )->list_superpackages( ).
      APPEND LINES OF lt_list TO rt_list.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_sap_package~read_parent.

    SELECT SINGLE parentcl FROM tdevc INTO rv_parentcl
      WHERE devclass = mv_package.                      "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Inconsistent package structure! Cannot find parent for { mv_package }| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_SKIP_OBJECTS IMPLEMENTATION.


  METHOD has_sadl_superclass.

    DATA: li_oo_functions TYPE REF TO zif_abapgit_oo_object_fnc,
          lv_class_name   TYPE seoclsname,
          lv_superclass   TYPE seoclsname.


    li_oo_functions = zcl_abapgit_oo_factory=>make( is_class-object ).
    lv_class_name = is_class-obj_name.
    lv_superclass = li_oo_functions->read_superclass( lv_class_name ).
    IF lv_superclass = 'CL_SADL_GTK_EXPOSURE_MPC'.
      rv_return = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD skip_sadl_generated_objects.

    DATA: ls_tadir_class     LIKE LINE OF rt_tadir,
          ls_tadir           LIKE LINE OF rt_tadir,
          lt_candidates      LIKE rt_tadir,
          lt_lines_to_delete TYPE zif_abapgit_definitions=>ty_tadir_tt.

    lt_candidates = it_tadir.
    DELETE lt_candidates WHERE object <> 'CLAS' OR genflag = abap_false.

    LOOP AT it_tadir INTO ls_tadir WHERE object = 'DDLS'.
      LOOP AT lt_candidates INTO ls_tadir_class
          WHERE object = 'CLAS' AND obj_name CS ls_tadir-obj_name.
        IF has_sadl_superclass( ls_tadir_class ) = abap_true.
          APPEND ls_tadir_class TO lt_lines_to_delete.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    rt_tadir = it_tadir.
    DELETE ADJACENT DUPLICATES FROM lt_lines_to_delete.
    LOOP AT lt_lines_to_delete INTO ls_tadir_class.
      DELETE TABLE rt_tadir FROM ls_tadir_class.
      IF ii_log IS BOUND.
        ii_log->add(
          iv_msg = |{ ls_tadir_class-obj_name } skipped: generated by SADL|
          iv_type = 'W' ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_sotr_handler IMPLEMENTATION.


  METHOD create_sotr.

    DATA:
      lt_objects  TYPE sotr_objects,
      ls_paket    TYPE sotr_pack,
      lv_object   LIKE LINE OF lt_objects,
      lt_sotr     TYPE zif_abapgit_definitions=>ty_sotr_tt,
      lt_sotr_use TYPE zif_abapgit_definitions=>ty_sotr_use_tt.

    FIELD-SYMBOLS: <ls_sotr> LIKE LINE OF lt_sotr.

    io_xml->read( EXPORTING iv_name = 'SOTR'
                  CHANGING cg_data = lt_sotr ).
    io_xml->read( EXPORTING iv_name = 'SOTR_USE'
                  CHANGING cg_data = lt_sotr_use ).

    LOOP AT lt_sotr ASSIGNING <ls_sotr>.
      CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
        EXPORTING
          object_vector    = <ls_sotr>-header-objid_vec
        IMPORTING
          objects          = lt_objects
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      READ TABLE lt_objects INDEX 1 INTO lv_object.
      ASSERT sy-subrc = 0.

      ls_paket-paket = iv_package.

      CALL FUNCTION 'SOTR_CREATE_CONCEPT'
        EXPORTING
          paket                         = ls_paket
          crea_lan                      = <ls_sotr>-header-crea_lan
          alias_name                    = <ls_sotr>-header-alias_name
          object                        = lv_object
          entries                       = <ls_sotr>-entries
          concept_default               = <ls_sotr>-header-concept
        EXCEPTIONS
          package_missing               = 1
          crea_lan_missing              = 2
          object_missing                = 3
          paket_does_not_exist          = 4
          alias_already_exist           = 5
          object_type_not_found         = 6
          langu_missing                 = 7
          identical_context_not_allowed = 8
          text_too_long                 = 9
          error_in_update               = 10
          no_master_langu               = 11
          error_in_concept_id           = 12
          alias_not_allowed             = 13
          tadir_entry_creation_failed   = 14
          internal_error                = 15
          error_in_correction           = 16
          user_cancelled                = 17
          no_entry_found                = 18
          OTHERS                        = 19.
      IF sy-subrc <> 0 AND sy-subrc <> 5.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'SOTR_USAGE_MODIFY'
      EXPORTING
        sotr_usage = lt_sotr_use.

  ENDMETHOD.


  METHOD get_sotr_4_concept.

    DATA: ls_header  TYPE sotr_head,
          lt_entries TYPE sotr_text_tt.

    FIELD-SYMBOLS: <ls_entry> LIKE LINE OF lt_entries.

    CALL FUNCTION 'SOTR_GET_CONCEPT'
      EXPORTING
        concept        = iv_concept
      IMPORTING
        header         = ls_header
      TABLES
        entries        = lt_entries
      EXCEPTIONS
        no_entry_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CLEAR: ls_header-paket,
           ls_header-crea_name,
           ls_header-crea_tstut,
           ls_header-chan_name,
           ls_header-chan_tstut,
           ls_header-system_id.

    LOOP AT lt_entries ASSIGNING <ls_entry>.
      CLEAR: <ls_entry>-version,
             <ls_entry>-crea_name,
             <ls_entry>-crea_tstut,
             <ls_entry>-chan_name,
             <ls_entry>-chan_tstut.
    ENDLOOP.

    rs_sotr-header  = ls_header.
    rs_sotr-entries = lt_entries.

  ENDMETHOD.


  METHOD get_sotr_usage.

    DATA: lv_obj_name TYPE trobj_name.

    lv_obj_name = iv_obj_name.

    " Objects with multiple components
    IF iv_pgmid = 'LIMU' AND ( iv_object = 'WDYV' OR iv_object = 'WAPP' ).
      lv_obj_name+30 = '%'.
    ENDIF.

    CALL FUNCTION 'SOTR_USAGE_READ'
      EXPORTING
        pgmid          = iv_pgmid
        object         = iv_object
        obj_name       = lv_obj_name
      IMPORTING
        sotr_usage     = rt_sotr_use
      EXCEPTIONS
        no_entry_found = 1
        error_in_pgmid = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      SORT rt_sotr_use.
    ENDIF.

  ENDMETHOD.


  METHOD read_sotr.

    FIELD-SYMBOLS <ls_sotr_use> TYPE sotr_use.

    DATA lv_sotr TYPE zif_abapgit_definitions=>ty_sotr.

    " Known SOTR usage...
    " LIMU: CPUB, WAPP, WDYV
    " R3TR: ENHC, ENHO, ENHS, ENSC, SCGR, SMIF, WDYA, WEBI, WEBS

    et_sotr_use = get_sotr_usage( iv_pgmid    = iv_pgmid
                                  iv_object   = iv_object
                                  iv_obj_name = iv_obj_name ).

    LOOP AT et_sotr_use ASSIGNING <ls_sotr_use> WHERE concept IS NOT INITIAL.
      lv_sotr = get_sotr_4_concept( <ls_sotr_use>-concept ).

      IF io_xml IS BOUND AND
         io_xml->i18n_params( )-main_language_only = abap_true AND
         iv_language IS SUPPLIED.
        DELETE lv_sotr-entries WHERE langu <> iv_language.
        CHECK lv_sotr-entries IS NOT INITIAL.
      ENDIF.

      INSERT lv_sotr INTO TABLE et_sotr.
    ENDLOOP.

    IF io_xml IS BOUND.
      io_xml->add( iv_name = 'SOTR'
                   ig_data = et_sotr ).
      io_xml->add( iv_name = 'SOTR_USE'
                   ig_data = et_sotr_use ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_tadir IMPLEMENTATION.


  METHOD add_local_packages.

    FIELD-SYMBOLS:
      <lv_package> LIKE LINE OF it_packages,
      <ls_tadir>   LIKE LINE OF ct_tadir.

    LOOP AT it_packages ASSIGNING <lv_package>.

      " Local packages are not in TADIR, only in TDEVC, act as if they were
      IF <lv_package> CP '$*'. " OR <package> CP 'T*' ).
        APPEND INITIAL LINE TO ct_tadir ASSIGNING <ls_tadir>.
        <ls_tadir>-pgmid    = 'R3TR'.
        <ls_tadir>-object   = 'DEVC'.
        <ls_tadir>-obj_name = <lv_package>.
        <ls_tadir>-devclass = <lv_package>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD add_namespaces.

    DATA:
      lv_name      TYPE progname,
      lv_namespace TYPE namespace.

    FIELD-SYMBOLS:
      <ls_tadir> LIKE LINE OF ct_tadir,
      <ls_nspc>  LIKE LINE OF ct_tadir.

    LOOP AT ct_tadir ASSIGNING <ls_tadir> WHERE obj_name(1) = '/'.

      " Namespaces are not in TADIR, but are necessary for creating objects in transportable packages
      lv_name = <ls_tadir>-obj_name.

      CALL FUNCTION 'RS_NAME_SPLIT_NAMESPACE'
        EXPORTING
          name_with_namespace = lv_name
        IMPORTING
          namespace           = lv_namespace
        EXCEPTIONS
          delimiter_error     = 1
          OTHERS              = 2.

      IF sy-subrc = 0 AND lv_namespace IS NOT INITIAL.
        READ TABLE ct_tadir TRANSPORTING NO FIELDS
          WITH KEY pgmid = 'R3TR' object = 'NSPC' obj_name = lv_namespace.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO ct_tadir ASSIGNING <ls_nspc>.
          <ls_nspc>-pgmid    = 'R3TR'.
          <ls_nspc>-object   = 'NSPC'.
          <ls_nspc>-obj_name = lv_namespace.
          <ls_nspc>-devclass = iv_package.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD adjust_objects.

    " Todo, replace with solution that will work with any object type (might depend on iv_package and io_dot)

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF ct_tadir.

    LOOP AT ct_tadir ASSIGNING <ls_tadir>.

      IF <ls_tadir>-object = 'SICF'.
        " Replace the internal GUID with a hash of the path
        TRY.
            CALL METHOD ('ZCL_ABAPGIT_OBJECT_SICF')=>read_sicf_url
              EXPORTING
                iv_obj_name = <ls_tadir>-obj_name
              RECEIVING
                rv_hash     = <ls_tadir>-obj_name+15.

          CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
            " SICF might not be supported in some systems, assume this code is not called
        ENDTRY.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD build.

    DATA lt_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt.

    select_objects(
      EXPORTING
        iv_package            = iv_package
        iv_ignore_subpackages = iv_ignore_subpackages
        iv_only_local_objects = iv_only_local_objects
      IMPORTING
        et_tadir              = rt_tadir
        et_packages           = lt_packages ).

    skip_objects(
      EXPORTING
        iv_package = iv_package
        io_dot     = io_dot
        ii_log     = ii_log
      CHANGING
        ct_tadir   = rt_tadir ).

    add_local_packages(
      EXPORTING
        it_packages = lt_packages
      CHANGING
        ct_tadir    = rt_tadir ).

    add_namespaces(
      EXPORTING
        iv_package = iv_package
      CHANGING
        ct_tadir   = rt_tadir ).

    determine_path(
      EXPORTING
        iv_package = iv_package
        io_dot     = io_dot
      CHANGING
        ct_tadir   = rt_tadir ).

    adjust_objects(
      EXPORTING
        iv_package = iv_package
        io_dot     = io_dot
      CHANGING
        ct_tadir   = rt_tadir ).

  ENDMETHOD.


  METHOD check_exists.

    DATA: li_progress TYPE REF TO zif_abapgit_progress,
          ls_item     TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


    li_progress = zcl_abapgit_progress=>get_instance( lines( it_tadir ) ).

* rows from database table TADIR are not removed for
* transportable objects until the transport is released
    LOOP AT it_tadir ASSIGNING <ls_tadir>.
      IF sy-tabix MOD 200 = 0.
        li_progress->show(
          iv_current = sy-tabix
          iv_text    = |Check object exists { <ls_tadir>-object } { <ls_tadir>-obj_name }| ).
      ENDIF.

      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.
      ls_item-devclass = <ls_tadir>-devclass.

      IF exists( ls_item ) = abap_true.
        APPEND <ls_tadir> TO rt_tadir.
      ENDIF.
    ENDLOOP.

    li_progress->off( ).

  ENDMETHOD.


  METHOD determine_path.

    DATA:
      lv_path         TYPE string,
      lo_folder_logic TYPE REF TO zcl_abapgit_folder_logic,
      lv_last_package TYPE devclass VALUE cl_abap_char_utilities=>horizontal_tab.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF ct_tadir.

    lo_folder_logic = zcl_abapgit_folder_logic=>get_instance( ).

    LOOP AT ct_tadir ASSIGNING <ls_tadir>.

      IF lv_last_package <> <ls_tadir>-devclass.
        "Change in Package
        lv_last_package = <ls_tadir>-devclass.

        IF NOT io_dot IS INITIAL.
          lv_path = lo_folder_logic->package_to_path(
            iv_top     = iv_package
            io_dot     = io_dot
            iv_package = <ls_tadir>-devclass ).
        ENDIF.
      ENDIF.

      <ls_tadir>-path = lv_path.

    ENDLOOP.
  ENDMETHOD.


  METHOD exists.

    IF is_item IS INITIAL.
      RETURN.
    ENDIF.

    IF zcl_abapinst_objects=>is_supported( is_item ) = abap_false.
      rv_exists = abap_true.
      RETURN.
    ENDIF.

    rv_exists = zcl_abapinst_objects=>exists( is_item ).

  ENDMETHOD.


  METHOD select_objects.

    DATA:
      lt_excludes  TYPE RANGE OF trobjtype,
      ls_exclude   LIKE LINE OF lt_excludes,
      lt_srcsystem TYPE RANGE OF tadir-srcsystem,
      ls_srcsystem LIKE LINE OF lt_srcsystem.

    " Determine packages to read
    IF iv_ignore_subpackages = abap_false.
      et_packages = zcl_abapinst_factory=>get_sap_package( iv_package )->list_subpackages( ).
    ENDIF.
    INSERT iv_package INTO et_packages INDEX 1.

    " Exclude object types with tadir entries that are included elsewhere
    ls_exclude-sign   = 'I'.
    ls_exclude-option = 'EQ'.
    ls_exclude-low    = 'SOTR'. " automatically create for sap packages (DEVC)
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low    = 'SFB1'. " covered by business function sets (SFBS)
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low    = 'SFB2'. " covered by business functions (SFBF)
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low    = 'STOB'. " auto generated by core data services (DDLS)
    APPEND ls_exclude TO lt_excludes.

    " Limit to objects belonging to this system
    IF iv_only_local_objects = abap_true.
      ls_srcsystem-sign   = 'I'.
      ls_srcsystem-option = 'EQ'.
      ls_srcsystem-low    = sy-sysid.
      APPEND ls_srcsystem TO lt_srcsystem.
    ENDIF.

    IF et_packages IS NOT INITIAL.
      SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE et_tadir
        FOR ALL ENTRIES IN et_packages
        WHERE devclass = et_packages-table_line
        AND pgmid      = 'R3TR'
        AND object     NOT IN lt_excludes
        AND delflag    = abap_false
        AND srcsystem  IN lt_srcsystem
        ORDER BY PRIMARY KEY ##TOO_MANY_ITAB_FIELDS. "#EC CI_GENBUFF "#EC CI_SUBRC
    ENDIF.

    SORT et_tadir BY devclass pgmid object obj_name.

  ENDMETHOD.


  METHOD skip_objects.

    " Todo, replace with solution that will work with any object type (might depend on iv_package and io_dot)

    DATA lo_skip_objects TYPE REF TO zcl_abapgit_skip_objects.

    CREATE OBJECT lo_skip_objects.

    ct_tadir = lo_skip_objects->skip_sadl_generated_objects(
      it_tadir = ct_tadir
      ii_log   = ii_log ).

  ENDMETHOD.


  METHOD zif_abapgit_tadir~get_object_package.

    DATA: ls_tadir TYPE zif_abapgit_definitions=>ty_tadir,
          ls_item  TYPE zif_abapgit_definitions=>ty_item.

    ls_tadir = zif_abapgit_tadir~read_single(
      iv_pgmid    = iv_pgmid
      iv_object   = iv_object
      iv_obj_name = iv_obj_name ).

    IF ls_tadir-delflag = 'X'.
      RETURN. "Mark for deletion -> return nothing
    ENDIF.

    ls_item-obj_type = ls_tadir-object.
    ls_item-obj_name = ls_tadir-obj_name.
    ls_item-devclass = ls_tadir-devclass.
    IF exists( ls_item ) = abap_false.
      RETURN.
    ENDIF.

    rv_devclass = ls_tadir-devclass.

  ENDMETHOD.


  METHOD zif_abapgit_tadir~read.

    DATA: li_exit TYPE REF TO zif_abapgit_exit.

    " Start recursion
    " hmm, some problems here, should TADIR also build path?
    rt_tadir = build(
      iv_package            = iv_package
      io_dot                = io_dot
      iv_ignore_subpackages = iv_ignore_subpackages
      iv_only_local_objects = iv_only_local_objects
      ii_log                = ii_log ).

    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->change_tadir(
      EXPORTING
        iv_package = iv_package
        ii_log     = ii_log
      CHANGING
        ct_tadir   = rt_tadir ).

    rt_tadir = check_exists( rt_tadir ).

  ENDMETHOD.


  METHOD zif_abapgit_tadir~read_single.

    SELECT SINGLE * FROM tadir INTO CORRESPONDING FIELDS OF rs_tadir
      WHERE pgmid = iv_pgmid
      AND object = iv_object
      AND obj_name = iv_obj_name.                         "#EC CI_SUBRC

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_url IMPLEMENTATION.


  METHOD host.

    regex( EXPORTING iv_url = iv_url
           IMPORTING ev_host = rv_host ).

  ENDMETHOD.


  METHOD is_abapgit_repo.

    IF iv_url CS 'github.com' AND ( iv_url CP '*/abapGit' OR iv_url CP '*/abapGit.git' ).
      rv_abapgit = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD name.

    DATA: lv_path TYPE string.

    TRY.
        regex( EXPORTING iv_url = iv_url
               IMPORTING ev_name = rv_name
                         ev_path = lv_path ).

        IF rv_name IS INITIAL.
          FIND REGEX '([\w-]+)/$' IN lv_path SUBMATCHES rv_name.
          IF sy-subrc <> 0.
            zcx_abapgit_exception=>raise( 'Malformed URL' ).
          ENDIF.
        ENDIF.

      CATCH zcx_abapgit_exception.
        IF iv_validate = abap_true.
          zcx_abapgit_exception=>raise( 'Malformed URL' ).
        ELSE.
          rv_name = 'URL error (fix repo with "Advanced > Change Remote")'.
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD path_name.

    DATA: lv_host TYPE string ##NEEDED.

    FIND REGEX '(.*://[^/]*)(.*)' IN iv_url
      SUBMATCHES lv_host rv_path_name.

  ENDMETHOD.


  METHOD regex.

    FIND REGEX '(https?://[^/]*)(.*/)(.*)\.git$' IN iv_url
      SUBMATCHES ev_host ev_path ev_name.
    IF sy-subrc <> 0.
      FIND REGEX '(https?://[^/]*)(.*/)(.*)$' IN iv_url
        SUBMATCHES ev_host ev_path ev_name.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Malformed URL' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD validate.

    name( iv_url      = iv_url
          iv_validate = abap_true ).

  ENDMETHOD.


  METHOD url_address.

    DATA:
      lv_host TYPE string,
      lv_path TYPE string,
      lv_name TYPE string,
      lv_len  TYPE i.

    regex( EXPORTING iv_url  = iv_url
           IMPORTING ev_host = lv_host
                     ev_path = lv_path
                     ev_name = lv_name ).

    IF lv_path IS INITIAL AND lv_name IS INITIAL.
      zcx_abapgit_exception=>raise( 'Malformed URL' ).
    ELSEIF lv_name IS INITIAL.
      lv_len = strlen( lv_path ) - 1.
      IF lv_path+lv_len(1) = '/'.
        lv_path = lv_path(lv_len).
      ENDIF.
    ENDIF.

    rv_adress = |{ lv_host }{ lv_path }{ lv_name }|.

  ENDMETHOD.


ENDCLASS.



CLASS zcl_abapgit_version IMPLEMENTATION.


  METHOD check_dependant_version.

    CONSTANTS: lc_message TYPE string VALUE 'Current version is older than required'.

    IF is_dependant-major > is_current-major.
      zcx_abapgit_exception=>raise( lc_message ).
    ELSEIF is_dependant-major < is_current-major.
      RETURN.
    ENDIF.

    IF is_dependant-minor > is_current-minor.
      zcx_abapgit_exception=>raise( lc_message ).
    ELSEIF is_dependant-minor < is_current-minor.
      RETURN.
    ENDIF.

    IF is_dependant-patch > is_current-patch.
      zcx_abapgit_exception=>raise( lc_message ).
    ELSEIF is_dependant-patch < is_current-patch.
      RETURN.
    ENDIF.

    IF is_current-prerelase IS INITIAL.
      RETURN.
    ENDIF.

    CASE is_current-prerelase.
      WHEN 'rc'.
        IF is_dependant-prerelase = ''.
          zcx_abapgit_exception=>raise( lc_message ).
        ENDIF.

      WHEN 'beta'.
        IF is_dependant-prerelase = '' OR is_dependant-prerelase = 'rc'.
          zcx_abapgit_exception=>raise( lc_message ).
        ENDIF.

      WHEN 'alpha'.
        IF is_dependant-prerelase = '' OR is_dependant-prerelase = 'rc' OR is_dependant-prerelase = 'beta'.
          zcx_abapgit_exception=>raise( lc_message ).
        ENDIF.

    ENDCASE.

    IF is_dependant-prerelase = is_current-prerelase AND is_dependant-prerelase_patch > is_current-prerelase_patch.
      zcx_abapgit_exception=>raise( lc_message ).
    ENDIF.

  ENDMETHOD.


  METHOD compare.

    DATA: ls_version_a TYPE zif_abapgit_definitions=>ty_version,
          ls_version_b TYPE zif_abapgit_definitions=>ty_version.

    TRY.
        IF is_a IS NOT INITIAL.
          ls_version_a = is_a.
        ELSE.
          ls_version_a = conv_str_to_version( iv_a ).
        ENDIF.

        IF is_b IS NOT INITIAL.
          ls_version_b = is_b.
        ELSE.
          ls_version_b = conv_str_to_version( iv_b ).
        ENDIF.
      CATCH zcx_abapgit_exception.
        rv_result = 0.
        RETURN.
    ENDTRY.

    IF ls_version_a = ls_version_b.
      rv_result = 0.
    ELSE.
      TRY.
          check_dependant_version( is_current   = ls_version_a
                                   is_dependant = ls_version_b ).
          rv_result = 1.
        CATCH zcx_abapgit_exception.
          rv_result = -1.
          RETURN.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD conv_str_to_version.

    DATA: lt_segments TYPE STANDARD TABLE OF string,
          lt_parts    TYPE STANDARD TABLE OF string,
          lv_segment  TYPE string.

    SPLIT iv_version AT '-' INTO TABLE lt_segments.

    READ TABLE lt_segments INTO lv_segment INDEX 1. " Version
    IF sy-subrc <> 0.   " No version
      RETURN.
    ENDIF.

    SPLIT lv_segment AT '.' INTO TABLE lt_parts.

    LOOP AT lt_parts INTO lv_segment.

      TRY.
          CASE sy-tabix.
            WHEN 1.
              rs_version-major = lv_segment.
            WHEN 2.
              rs_version-minor = lv_segment.
            WHEN 3.
              rs_version-patch = lv_segment.
          ENDCASE.
        CATCH cx_sy_conversion_no_number.
          zcx_abapgit_exception=>raise( 'Incorrect format for Semantic Version' ).
      ENDTRY.

    ENDLOOP.

    READ TABLE lt_segments INTO lv_segment INDEX 2. " Pre-release Version
    IF sy-subrc <> 0.   " No version
      RETURN.
    ENDIF.

    SPLIT lv_segment AT '.' INTO TABLE lt_parts.

    LOOP AT lt_parts INTO lv_segment.

      CASE sy-tabix.
        WHEN 1.
          rs_version-prerelase = lv_segment.
          TRANSLATE rs_version-prerelase TO LOWER CASE.
        WHEN 2.
          rs_version-prerelase_patch = lv_segment.
      ENDCASE.

    ENDLOOP.

    IF rs_version-prerelase <> 'rc' AND rs_version-prerelase <> 'beta' AND rs_version-prerelase <> 'alpha'.
      zcx_abapgit_exception=>raise( 'Incorrect format for Semantic Version' ).
    ENDIF.

  ENDMETHOD.


  METHOD normalize.

    " Internal program version should be in format "XXX.XXX.XXX" or "vXXX.XXX.XXX"
    CONSTANTS:
      lc_version_pattern    TYPE string VALUE '^v?(\d{1,3}\.\d{1,3}\.\d{1,3})\s*$',
      lc_prerelease_pattern TYPE string VALUE '^((rc|beta|alpha)\.\d{1,3})\s*$'.

    DATA: lv_version      TYPE string,
          lv_prerelease   TYPE string,
          lv_version_n    TYPE string,
          lv_prerelease_n TYPE string.

    SPLIT iv_version AT '-' INTO lv_version lv_prerelease.

    FIND FIRST OCCURRENCE OF REGEX lc_version_pattern
      IN lv_version SUBMATCHES lv_version_n.

    IF lv_prerelease IS NOT INITIAL.

      FIND FIRST OCCURRENCE OF REGEX lc_prerelease_pattern
        IN lv_prerelease SUBMATCHES lv_prerelease_n.

    ENDIF.

    IF lv_version_n IS INITIAL.
      RETURN.
    ENDIF.

    rv_version = lv_version_n.

    IF lv_prerelease_n IS NOT INITIAL.
      CONCATENATE rv_version '-' lv_prerelease_n INTO rv_version.
    ENDIF.

  ENDMETHOD.


  METHOD version_to_numeric.

    DATA: lv_major   TYPE n LENGTH 4,
          lv_minor   TYPE n LENGTH 4,
          lv_release TYPE n LENGTH 4.

    SPLIT iv_version AT '.' INTO lv_major lv_minor lv_release.

    " Calculated value of version number, empty version will become 0 which is OK
    rv_version = lv_major * 1000000 + lv_minor * 1000 + lv_release.

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_XML_PRETTY IMPLEMENTATION.


  METHOD print.

    DATA: li_ixml           TYPE REF TO if_ixml,
          li_xml_doc        TYPE REF TO if_ixml_document,
          li_stream_factory TYPE REF TO if_ixml_stream_factory,
          li_istream        TYPE REF TO if_ixml_istream,
          li_parser         TYPE REF TO if_ixml_parser,
          li_ostream        TYPE REF TO if_ixml_ostream,
          li_renderer       TYPE REF TO if_ixml_renderer.


    ASSERT NOT iv_xml IS INITIAL.

    li_ixml    = cl_ixml=>create( ).
    li_xml_doc = li_ixml->create_document( ).

    li_stream_factory = li_ixml->create_stream_factory( ).
    li_istream        = li_stream_factory->create_istream_string( iv_xml ).
    li_parser         = li_ixml->create_parser( stream_factory = li_stream_factory
                                                istream        = li_istream
                                                document       = li_xml_doc ).
    li_parser->set_normalizing( abap_true ).
    IF li_parser->parse( ) <> 0.
      IF iv_ignore_errors = abap_true.
        rv_xml = iv_xml.
        RETURN.
      ELSE.
        zcx_abapgit_exception=>raise( 'error parsing xml' ).
      ENDIF.
    ENDIF.
    li_istream->close( ).


    li_ostream  = li_stream_factory->create_ostream_cstring( rv_xml ).

    li_renderer = li_ixml->create_renderer( ostream  = li_ostream
                                            document = li_xml_doc ).

    li_renderer->set_normalizing( boolc( iv_unpretty = abap_false ) ).

    li_renderer->render( ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapinst_factory IMPLEMENTATION.


  METHOD get_environment.
    IF gi_environment IS NOT BOUND.
      CREATE OBJECT gi_environment TYPE zcl_abapgit_environment.
    ENDIF.
    ri_environment = gi_environment.
  ENDMETHOD.


  METHOD get_gui_functions.

    IF gi_gui_functions IS INITIAL.
      CREATE OBJECT gi_gui_functions TYPE zcl_abapgit_gui_functions.
    ENDIF.

    ri_gui_functions = gi_gui_functions.

  ENDMETHOD.


  METHOD get_longtexts.

    IF gi_longtext IS NOT BOUND.
      CREATE OBJECT gi_longtext TYPE zcl_abapgit_longtexts.
    ENDIF.
    ri_longtexts = gi_longtext.

  ENDMETHOD.


  METHOD get_lxe_texts.

    IF gi_lxe_texts IS NOT BOUND.
      CREATE OBJECT gi_lxe_texts TYPE zcl_abapgit_lxe_texts.
    ENDIF.
    ri_lxe_texts = gi_lxe_texts.

  ENDMETHOD.


  METHOD get_sap_package.

    DATA: ls_sap_package TYPE ty_sap_package.
    FIELD-SYMBOLS: <ls_sap_package> TYPE ty_sap_package.

    READ TABLE gt_sap_package ASSIGNING <ls_sap_package>
                              WITH TABLE KEY package = iv_package.
    IF sy-subrc <> 0.

      ls_sap_package-package = iv_package.
      CREATE OBJECT ls_sap_package-instance TYPE zcl_abapgit_sap_package
        EXPORTING
          iv_package = iv_package.

      INSERT ls_sap_package
             INTO TABLE gt_sap_package
             ASSIGNING <ls_sap_package>.

    ENDIF.

    ri_sap_package = <ls_sap_package>-instance.

  ENDMETHOD.


  METHOD get_tadir.

    IF gi_tadir IS INITIAL.
      CREATE OBJECT gi_tadir TYPE zcl_abapgit_tadir.
    ENDIF.

    ri_tadir = gi_tadir.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapinst_file IMPLEMENTATION.


  METHOD load_internet.

    DATA:
      li_client  TYPE REF TO if_http_client,
      lx_error   TYPE REF TO zcx_abapgit_exception,
      lv_url     TYPE string,
      lv_code    TYPE i,
      lv_message TYPE string,
      lv_reason  TYPE string.

    TRY.
        lv_url = zcl_abapgit_url=>host( iv_url ).
      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abapinst_exception=>raise( lx_error->get_text( ) ).
    ENDTRY.

    IF iv_proxy_host IS NOT INITIAL AND iv_proxy_port IS NOT INITIAL.
      cl_http_client=>create_by_url(
        EXPORTING
          url                = lv_url
          ssl_id             = 'ANONYM'
          proxy_host         = iv_proxy_host
          proxy_service      = iv_proxy_port
        IMPORTING
          client             = li_client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).
    ELSE.
      cl_http_client=>create_by_url(
        EXPORTING
          url                = lv_url
          ssl_id             = 'ANONYM'
        IMPORTING
          client             = li_client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).
    ENDIF.
    IF sy-subrc <> 0.
      zcx_abapinst_exception=>raise( |Error creating HTTP client (check certificates in STRUST)| ).
    ENDIF.

    IF iv_proxy_user IS NOT INITIAL AND iv_proxy_password IS NOT INITIAL.
      li_client->authenticate(
        proxy_authentication = abap_true
        username             = iv_proxy_user
        password             = iv_proxy_password ).
    ENDIF.

    IF iv_user IS NOT INITIAL AND iv_password IS NOT INITIAL.
      li_client->authenticate(
        username = iv_user
        password = iv_password ).
    ENDIF.

    cl_http_utility=>set_request_uri(
      request = li_client->request
      uri     = iv_url ).

    li_client->request->set_method( 'GET' ).
    li_client->request->set_compression( ).
    li_client->request->set_header_field(
      name  = 'content-type'
      value = 'application/zip' ).

    li_client->send(
      EXPORTING
        timeout                    = '6000'
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).
    IF sy-subrc  = 0.
      li_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
    ENDIF.

    IF sy-subrc <> 0.
      li_client->get_last_error(
        IMPORTING
          code    = lv_code
          message = lv_message ).

      zcx_abapinst_exception=>raise( |{ lv_code } { lv_message }| ).
    ENDIF.

    li_client->response->get_status(
      IMPORTING
        code   = lv_code
        reason = lv_reason ).
    IF lv_code <> 200.
      zcx_abapinst_exception=>raise( |{ lv_code } { lv_reason }| ).
    ENDIF.

    rv_file = li_client->response->get_data( ).

    IF rv_file IS INITIAL.
      zcx_abapinst_exception=>raise( 'Error downloading file. No data returned.' ).
    ENDIF.

    li_client->close( ).

  ENDMETHOD.


  METHOD load_local.

    TYPES:
      ty_hex TYPE x LENGTH 255.

    DATA:
      lt_data   TYPE TABLE OF ty_hex WITH DEFAULT KEY,
      lv_length TYPE i.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = |{ iv_filename }|
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_length
      CHANGING
        data_tab                = lt_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19 ).
    IF sy-subrc <> 0.
      zcx_abapinst_exception=>raise_t100( ).
    ENDIF.

    CONCATENATE LINES OF lt_data INTO rv_file IN BYTE MODE.
    rv_file = rv_file(lv_length).

  ENDMETHOD.


  METHOD load_server.

    DATA:
      lv_eps_inbox TYPE eps2path,
      lv_filename  TYPE file_name,
      lv_filesize  TYPE i,
      lv_data      TYPE x LENGTH 1024,
      lt_data      LIKE TABLE OF lv_data.

    CALL FUNCTION 'EPS_GET_DIRECTORY_PATH'
      EXPORTING
        eps_subdir             = 'in'
      IMPORTING
        ev_long_dir_name       = lv_eps_inbox
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        OTHERS                 = 4.
    IF sy-subrc <> 0.
      zcx_abapinst_exception=>raise( |Error getting EPS directory from server| ).
    ENDIF.

    IF lv_eps_inbox CA '\'.
      lv_filename = lv_eps_inbox && '\' && iv_filename.
    ELSE.
      lv_filename = lv_eps_inbox && '/' && iv_filename.
    ENDIF.

    CALL FUNCTION 'SCMS_UPLOAD'
      EXPORTING
        filename = lv_filename
        binary   = abap_true
        frontend = abap_false
      IMPORTING
        filesize = lv_filesize
      TABLES
        data     = lt_data
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      zcx_abapinst_exception=>raise( |Error loading file from server: { lv_filename }| ).
    ENDIF.

    CONCATENATE LINES OF lt_data INTO rv_file IN BYTE MODE.
    rv_file = rv_file(lv_filesize).

  ENDMETHOD.


  METHOD unzip.

    DATA:
      lo_zip  TYPE REF TO cl_abap_zip,
      lv_data TYPE xstring.

    FIELD-SYMBOLS:
      <ls_zipfile> LIKE LINE OF lo_zip->files,
      <ls_file>    LIKE LINE OF rt_files.

    CREATE OBJECT lo_zip.

    lo_zip->load(
      EXPORTING
        zip             = iv_xstr
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2 ).
    IF sy-subrc <> 0.
      zcx_abapinst_exception=>raise( 'Error loading ZIP' ).
    ENDIF.

    LOOP AT lo_zip->files ASSIGNING <ls_zipfile>.

      lo_zip->get(
        EXPORTING
          name                    = <ls_zipfile>-name
        IMPORTING
          content                 = lv_data
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3 ).
      IF sy-subrc <> 0.
        zcx_abapinst_exception=>raise( 'Error getting file from ZIP' ).
      ENDIF.

      APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.

      _filename(
        EXPORTING
          iv_str      = <ls_zipfile>-name
        IMPORTING
          ev_path     = <ls_file>-path
          ev_filename = <ls_file>-filename ).

      <ls_file>-data = lv_data.

      TRY.
          <ls_file>-sha1 = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>c_type-blob
                                                   iv_data = <ls_file>-data ).
        CATCH zcx_abapgit_exception.
          zcx_abapinst_exception=>raise( 'Error during hashing' ).
      ENDTRY.

    ENDLOOP.

    DELETE rt_files WHERE filename IS INITIAL.

    _normalize_path( CHANGING ct_files = rt_files ).

  ENDMETHOD.


  METHOD virus_scan.

    DATA:
      lo_scanner  TYPE REF TO cl_vsi,
      lv_scanrc   TYPE vscan_scanrc,
      lv_msg      TYPE string ##NEEDED,
      ls_message  TYPE bapiret2,
      lt_bapiret2 TYPE vscan_bapiret2_t.

    " Data was download from Internet and uploaded here
    " so we will use the HTTP_UPLOAD profile
    cl_vsi=>get_instance(
      EXPORTING
        if_profile         = '/SIHTTP/HTTP_UPLOAD'
      IMPORTING
        eo_instance        = lo_scanner
      EXCEPTIONS
        profile_not_active = 1
        OTHERS             = 2 ).
    CASE sy-subrc.
      WHEN 0.
        " Perform virus scan
        lo_scanner->if_vscan_instance~scan_bytes(
          EXPORTING
            if_data             = iv_data
          IMPORTING
            ef_scanrc           = lv_scanrc
            et_bapiret          = lt_bapiret2
          EXCEPTIONS
            not_available       = 1
            configuration_error = 2
            internal_error      = 3
            OTHERS              = 4 ).
        " Severe errors of the scanner (NOT: Virus found) are reported
        " as exceptions and must be reported as technical errors
        IF sy-subrc <> 0.
          zcx_abapinst_exception=>raise_t100( ).
        ENDIF.

        " Result of virus scan
        " Any scan error or virus infection will be reported there
        IF lv_scanrc <> 0.
          LOOP AT lt_bapiret2 INTO ls_message WHERE type = 'E'.
            MESSAGE ID ls_message-id TYPE 'E' NUMBER ls_message-number
              WITH ls_message-message_v1 ls_message-message_v2 ls_message-message_v3 ls_message-message_v4
              INTO lv_msg.
            zcx_abapinst_exception=>raise_t100( ).
          ENDLOOP.
        ENDIF.

      WHEN 1.
        " No Virus Scan active --> nothing to do
      WHEN 2.
        " Error getting scanner. Reporting needed
        zcx_abapinst_exception=>raise_t100( ).
    ENDCASE.

  ENDMETHOD.


  METHOD _filename.

    IF iv_str CA '/'.
      FIND REGEX '(.*/)(.*)' IN iv_str
        SUBMATCHES ev_path ev_filename.
      IF sy-subrc <> 0.
        zcx_abapinst_exception=>raise( 'Malformed path' ).
      ENDIF.
      IF ev_path <> '/'.
        CONCATENATE '/' ev_path INTO ev_path.
      ENDIF.
    ELSE.
      ev_path = '/'.
      ev_filename = iv_str.
    ENDIF.
    TRANSLATE ev_filename TO LOWER CASE.

  ENDMETHOD.


  METHOD _normalize_path.
* removes first folder from path if needed

    DATA:
      lt_split  TYPE TABLE OF string,
      lv_needed TYPE abap_bool,
      lv_length TYPE i,
      lv_split  LIKE LINE OF lt_split.

    FIELD-SYMBOLS:
      <ls_file> LIKE LINE OF ct_files.

    READ TABLE ct_files INDEX 1 ASSIGNING <ls_file>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SPLIT <ls_file>-path AT '/' INTO TABLE lt_split.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    READ TABLE lt_split INDEX 2 INTO lv_split.
    IF sy-subrc <> 0 OR strlen( lv_split ) = 0.
      RETURN.
    ENDIF.

    CONCATENATE '/' lv_split '/*' INTO lv_split.

    lv_needed = abap_true.
    LOOP AT ct_files ASSIGNING <ls_file>.
      IF <ls_file>-path NP lv_split.
        lv_needed = abap_false.
        EXIT. " current loop
      ENDIF.
    ENDLOOP.

    IF lv_needed = abap_true.
      lv_length = strlen( lv_split ) - 2.
      LOOP AT ct_files ASSIGNING <ls_file>.
        <ls_file>-path = <ls_file>-path+lv_length.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapinst_file_status IMPLEMENTATION.


  METHOD build_existing.

    DATA: ls_file_sig LIKE LINE OF it_state.

    " Item
    rs_result-obj_type = is_local-item-obj_type.
    rs_result-obj_name = is_local-item-obj_name.
    rs_result-package  = is_local-item-devclass.

    " File
    rs_result-path     = is_local-file-path.
    rs_result-filename = is_local-file-filename.

    IF is_local-file-sha1 = is_remote-sha1.
      rs_result-match = abap_true.
      RETURN.
    ENDIF.

    " Match against current state
    READ TABLE it_state INTO ls_file_sig
      WITH KEY path = is_local-file-path
      filename = is_local-file-filename
      BINARY SEARCH.

    IF sy-subrc = 0.
      IF ls_file_sig-sha1 <> is_local-file-sha1.
        rs_result-lstate = zif_abapgit_definitions=>c_state-modified.
      ENDIF.
      IF ls_file_sig-sha1 <> is_remote-sha1.
        rs_result-rstate = zif_abapgit_definitions=>c_state-modified.
      ENDIF.
      rs_result-match = boolc( rs_result-lstate IS INITIAL
        AND rs_result-rstate IS INITIAL ).
    ELSE.
      " This is a strange situation. As both local and remote exist
      " the state should also be present. Maybe this is a first run of the code.
      " In this case just compare hashes directly and mark both changed
      " the user will presumably decide what to do after checking the actual diff
      rs_result-match = boolc( is_local-file-sha1 = is_remote-sha1 ).
      IF rs_result-match = abap_false.
        rs_result-lstate = zif_abapgit_definitions=>c_state-modified.
        rs_result-rstate = zif_abapgit_definitions=>c_state-modified.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD build_new_local.

    " Item
    rs_result-obj_type = is_local-item-obj_type.
    rs_result-obj_name = is_local-item-obj_name.
    rs_result-package  = is_local-item-devclass.

    " File
    rs_result-path     = is_local-file-path.
    rs_result-filename = is_local-file-filename.

    " Match
    rs_result-match    = abap_false.
    rs_result-lstate   = zif_abapgit_definitions=>c_state-added.

  ENDMETHOD.


  METHOD build_new_remote.

    DATA: ls_item     LIKE LINE OF it_items,
          ls_file_sig LIKE LINE OF it_state.

    " Common and default part
    rs_result-path     = is_remote-path.
    rs_result-filename = is_remote-filename.
    rs_result-match    = abap_false.
    rs_result-rstate   = zif_abapgit_definitions=>c_state-added.

    identify_object( EXPORTING iv_filename = is_remote-filename
                               iv_path     = is_remote-path
                               iv_devclass = iv_devclass
                               io_dot      = io_dot
                     IMPORTING es_item     = ls_item ).

    " Check if in item index + get package
    READ TABLE it_items INTO ls_item
      WITH KEY obj_type = ls_item-obj_type obj_name = ls_item-obj_name
      BINARY SEARCH.

    IF sy-subrc = 0.

      " Completely new (xml, abap) and new file in an existing object
      rs_result-obj_type = ls_item-obj_type.
      rs_result-obj_name = ls_item-obj_name.
      rs_result-package  = ls_item-devclass.

      READ TABLE it_state INTO ls_file_sig
        WITH KEY path = is_remote-path filename = is_remote-filename
        BINARY SEARCH.

      " Existing file but from another package
      " was not added during local file proc as was not in tadir for repo package
      IF sy-subrc = 0.
        IF ls_file_sig-sha1 = is_remote-sha1.
          rs_result-match = abap_true.
          CLEAR rs_result-rstate.
        ELSE.
          rs_result-rstate = zif_abapgit_definitions=>c_state-modified.
        ENDIF.

        " Item is in state and in cache but with no package - it was deleted
        " OR devclass is the same as repo package (see #532)
        IF ls_item-devclass IS INITIAL OR ls_item-devclass = iv_devclass.
          rs_result-match  = abap_false.
          rs_result-lstate = zif_abapgit_definitions=>c_state-deleted.
        ENDIF.
      ENDIF.

    ELSE. " Completely unknown file, probably non-abapgit
      ASSERT 1 = 1. " No action, just follow defaults
    ENDIF.

  ENDMETHOD.


  METHOD calculate_status.

    DATA: lt_remote       LIKE it_remote,
          lv_index        TYPE i,
          lt_items        TYPE zif_abapgit_definitions=>ty_items_tt,
          ls_item         LIKE LINE OF lt_items,
          lv_is_xml       TYPE abap_bool,
          lv_sub_fetched  TYPE abap_bool,
          lt_sub_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt,
          lv_msg          TYPE string,
          lt_items_idx    TYPE zif_abapgit_definitions=>ty_items_ts,
          lt_state_idx    TYPE zif_abapgit_definitions=>ty_file_signatures_ts. " Sorted by path+filename

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF it_remote,
                   <ls_result> LIKE LINE OF rt_results,
                   <ls_state>  LIKE LINE OF it_cur_state,
                   <ls_local>  LIKE LINE OF it_local.


    lt_state_idx = it_cur_state. " Force sort it
    lt_remote    = it_remote.
    SORT lt_remote BY path filename.

    " Skip ignored files
    LOOP AT lt_remote ASSIGNING <ls_remote>.
      lv_index = sy-tabix.
      IF io_dot->is_ignored( iv_path     = <ls_remote>-path
                             iv_filename = <ls_remote>-filename ) = abap_true.
        DELETE lt_remote INDEX lv_index.
      ENDIF.
    ENDLOOP.

    " Process local files and new local files
    LOOP AT it_local ASSIGNING <ls_local>.
      " Skip ignored files
      IF io_dot->is_ignored( iv_path     = <ls_local>-file-path
                             iv_filename = <ls_local>-file-filename ) = abap_true.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO rt_results ASSIGNING <ls_result>.
      IF <ls_local>-item IS NOT INITIAL.
        APPEND <ls_local>-item TO lt_items. " Collect for item index
      ENDIF.

      READ TABLE lt_remote ASSIGNING <ls_remote>
        WITH KEY path = <ls_local>-file-path filename = <ls_local>-file-filename
        BINARY SEARCH.
      IF sy-subrc = 0.  " Exist local and remote
        <ls_result> = build_existing(
          is_local  = <ls_local>
          is_remote = <ls_remote>
          it_state  = lt_state_idx ).
        ASSERT <ls_remote>-sha1 IS NOT INITIAL.
        CLEAR <ls_remote>-sha1. " Mark as processed
      ELSE.             " Only L exists
        <ls_result> = build_new_local( <ls_local> ).
        " Check if same file exists in different location
        READ TABLE lt_remote ASSIGNING <ls_remote>
          WITH KEY filename = <ls_local>-file-filename.
        IF sy-subrc = 0 AND <ls_local>-file-sha1 = <ls_remote>-sha1.
          <ls_result>-packmove = abap_true.
        ELSEIF sy-subrc = 4.
          " Check if file existed before and was deleted remotely
          READ TABLE lt_state_idx ASSIGNING <ls_state>
            WITH KEY path = <ls_local>-file-path filename = <ls_local>-file-filename
            BINARY SEARCH.
          IF sy-subrc = 0.
            IF <ls_local>-file-sha1 = <ls_state>-sha1.
              <ls_result>-lstate = zif_abapgit_definitions=>c_state-unchanged.
            ELSE.
              <ls_result>-lstate = zif_abapgit_definitions=>c_state-modified.
            ENDIF.
            <ls_result>-rstate = zif_abapgit_definitions=>c_state-deleted.
          ENDIF.
        ENDIF.
      ENDIF.
      <ls_result>-inactive = <ls_local>-item-inactive.
    ENDLOOP.

    " Complete item index for unmarked remote files
    LOOP AT lt_remote ASSIGNING <ls_remote> WHERE sha1 IS NOT INITIAL.
      identify_object( EXPORTING iv_filename = <ls_remote>-filename
                                 iv_path     = <ls_remote>-path
                                 io_dot      = io_dot
                                 iv_devclass = iv_devclass
                       IMPORTING es_item     = ls_item
                                 ev_is_xml   = lv_is_xml ).

      CHECK lv_is_xml = abap_true. " only object definitions

      ls_item-devclass = get_object_package(
        iv_object   = ls_item-obj_type
        iv_obj_name = ls_item-obj_name ).

      IF ls_item-devclass IS NOT INITIAL AND iv_devclass <> ls_item-devclass.
        IF lv_sub_fetched = abap_false.
          lt_sub_packages = zcl_abapinst_factory=>get_sap_package( iv_devclass )->list_subpackages( ).
          lv_sub_fetched = abap_true.
          SORT lt_sub_packages BY table_line. "Optimize Read Access
        ENDIF.
        " Make sure the package is under the repo main package
        READ TABLE lt_sub_packages TRANSPORTING NO FIELDS
          WITH KEY table_line = ls_item-devclass
          BINARY SEARCH.
        IF sy-subrc <> 0.
          IF ls_item-obj_type = 'DEVC'.
            " If package already exist but is not included in the package hierarchy of
            " the package assigned to the repository, then a manual change of the package
            " is required i.e. setting a parent package to iv_devclass (or one of its
            " subpackages). We don't this automatically since it's not clear where in the
            " hierarchy the new package should be located. (#4108)
            lv_msg = |Package { ls_item-devclass } is not a subpackage of { iv_devclass
                     }. Assign { ls_item-devclass } to package hierarchy of { iv_devclass
                     } and repeat process.|.
            zcx_abapgit_exception=>raise( lv_msg ).
          ELSE.
            CLEAR ls_item-devclass.
          ENDIF.
        ENDIF.
      ENDIF.

      APPEND ls_item TO lt_items.
    ENDLOOP.

    SORT lt_items DESCENDING. " Default key - type, name, pkg, inactive
    DELETE ADJACENT DUPLICATES FROM lt_items COMPARING obj_type obj_name devclass.
    lt_items_idx = lt_items. " Self protection + UNIQUE records assertion

    " Process new remote files (marked above with empty SHA1)
    LOOP AT lt_remote ASSIGNING <ls_remote> WHERE sha1 IS NOT INITIAL.
      APPEND INITIAL LINE TO rt_results ASSIGNING <ls_result>.
      <ls_result> = build_new_remote( iv_devclass = iv_devclass
                                      io_dot      = io_dot
                                      is_remote   = <ls_remote>
                                      it_items    = lt_items_idx
                                      it_state    = lt_state_idx ).
      " Check if same file exists in different location
      READ TABLE it_local ASSIGNING <ls_local>
        WITH KEY file-filename = <ls_remote>-filename.
      IF sy-subrc = 0.
        <ls_result>-match = abap_false.
        <ls_result>-lstate = zif_abapgit_definitions=>c_state-deleted.
        <ls_result>-rstate = zif_abapgit_definitions=>c_state-unchanged.
        IF <ls_local>-file-sha1 = <ls_remote>-sha1.
          <ls_result>-packmove = abap_true.
        ENDIF.
      ELSEIF sy-subrc = 4.
        " Check if file existed before and was deleted locally
        READ TABLE lt_state_idx ASSIGNING <ls_state>
          WITH KEY path = <ls_remote>-path filename = <ls_remote>-filename
          BINARY SEARCH.
        IF sy-subrc = 0.
          <ls_result>-lstate = zif_abapgit_definitions=>c_state-deleted.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT rt_results BY
      obj_type ASCENDING
      obj_name ASCENDING
      filename ASCENDING
      path ASCENDING.

  ENDMETHOD.


  METHOD check_files_folder.

    DATA:
      ls_item     TYPE zif_abapgit_definitions=>ty_item,
      lt_res_sort LIKE it_results,
      lt_item_idx LIKE it_results.

    FIELD-SYMBOLS:
      <ls_result>     LIKE LINE OF it_results,
      <ls_result_idx> LIKE LINE OF it_results.

    " Collect object index
    lt_res_sort = it_results.
    SORT lt_res_sort BY obj_type ASCENDING obj_name ASCENDING.

    LOOP AT it_results ASSIGNING <ls_result> WHERE NOT obj_type IS INITIAL AND packmove = abap_false.

      IF NOT ( <ls_result>-obj_type = ls_item-obj_type
          AND <ls_result>-obj_name = ls_item-obj_name ).
        APPEND INITIAL LINE TO lt_item_idx ASSIGNING <ls_result_idx>.
        <ls_result_idx>-obj_type = <ls_result>-obj_type.
        <ls_result_idx>-obj_name = <ls_result>-obj_name.
        <ls_result_idx>-path     = <ls_result>-path.
        MOVE-CORRESPONDING <ls_result> TO ls_item.
      ENDIF.

    ENDLOOP.

    LOOP AT it_results ASSIGNING <ls_result>
      WHERE NOT obj_type IS INITIAL AND obj_type <> 'DEVC' AND packmove = abap_false.

      READ TABLE lt_item_idx ASSIGNING <ls_result_idx>
        WITH KEY obj_type = <ls_result>-obj_type obj_name = <ls_result>-obj_name
        BINARY SEARCH. " Sorted above

      IF sy-subrc <> 0 OR <ls_result>-path <> <ls_result_idx>-path. " All paths are same
        ii_log->add( iv_msg = |Files for object { <ls_result>-obj_type } {
                              <ls_result>-obj_name } are not placed in the same folder|
                     iv_type = 'W'
                     iv_rc   = '1' ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_multiple_files.

    DATA:
      lt_res_sort LIKE it_results,
      ls_file     TYPE zif_abapgit_definitions=>ty_file_signature.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    lt_res_sort = it_results.
    SORT lt_res_sort BY filename ASCENDING.

    LOOP AT lt_res_sort ASSIGNING <ls_result> WHERE obj_type <> 'DEVC' AND packmove = abap_false.
      IF <ls_result>-filename IS NOT INITIAL AND <ls_result>-filename = ls_file-filename.
        ii_log->add( iv_msg  = |Multiple files with same filename, { <ls_result>-filename }|
                     iv_type = 'W'
                     iv_rc   = '3' ).
      ENDIF.

      IF <ls_result>-filename IS INITIAL.
        ii_log->add( iv_msg  = |Filename is empty for object { <ls_result>-obj_type } { <ls_result>-obj_name }|
                     iv_type = 'W'
                     iv_rc   = '4' ).
      ENDIF.

      MOVE-CORRESPONDING <ls_result> TO ls_file.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_package_folder.

    DATA:
      lv_path         TYPE string,
      lo_folder_logic TYPE REF TO zcl_abapgit_folder_logic.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    lo_folder_logic = zcl_abapgit_folder_logic=>get_instance( ).

    LOOP AT it_results ASSIGNING <ls_result>
      WHERE NOT package IS INITIAL AND NOT path IS INITIAL AND packmove = abap_false.

      lv_path = lo_folder_logic->package_to_path(
        iv_top     = iv_top
        io_dot     = io_dot
        iv_package = <ls_result>-package ).

      IF lv_path <> <ls_result>-path.
        ii_log->add( iv_msg = |Package and path does not match for object, {
                       <ls_result>-obj_type } { <ls_result>-obj_name }|
                     iv_type = 'W'
                     iv_rc   = '2' ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_package_move.

    DATA:
      lt_move_idx LIKE it_results.

    FIELD-SYMBOLS:
      <ls_result>      LIKE LINE OF it_results,
      <ls_result_move> LIKE LINE OF it_results.

    LOOP AT it_results ASSIGNING <ls_result>
      WHERE lstate = zif_abapgit_definitions=>c_state-added AND packmove = abap_true.

      READ TABLE lt_move_idx TRANSPORTING NO FIELDS
        WITH KEY obj_type = <ls_result>-obj_type obj_name = <ls_result>-obj_name
        BINARY SEARCH. " Sorted since it_result is sorted
      IF sy-subrc <> 0.
        ii_log->add( iv_msg  = |Changed package assignment for object {
                               <ls_result>-obj_type } { <ls_result>-obj_name }|
                     iv_type = 'W'
                     iv_rc   = '5' ).
        APPEND INITIAL LINE TO lt_move_idx ASSIGNING <ls_result_move>.
        <ls_result_move>-obj_type = <ls_result>-obj_type.
        <ls_result_move>-obj_name = <ls_result>-obj_name.
        <ls_result_move>-path     = <ls_result>-path.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_object_package.
    DATA: lv_name    TYPE devclass,
          li_package TYPE REF TO zif_abapgit_sap_package.

    rv_devclass = zcl_abapinst_factory=>get_tadir( )->get_object_package(
      iv_object   = iv_object
      iv_obj_name = iv_obj_name ).
    IF rv_devclass IS INITIAL AND iv_object = 'DEVC' AND iv_obj_name(1) = '$'.
      " local packages usually have no tadir entry
      lv_name = iv_obj_name.
      li_package = zcl_abapinst_factory=>get_sap_package( lv_name ).
      IF li_package->exists( ) = abap_true.
        rv_devclass = lv_name.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD identify_object.

    DATA: lv_name TYPE string,
          lv_type TYPE string,
          lv_ext  TYPE string.

    " Guess object type and name
    SPLIT to_upper( iv_filename ) AT '.' INTO lv_name lv_type lv_ext.

    " Handle namespaces
    REPLACE ALL OCCURRENCES OF '#' IN lv_name WITH '/'.
    REPLACE ALL OCCURRENCES OF '#' IN lv_type WITH '/'.
    REPLACE ALL OCCURRENCES OF '#' IN lv_ext WITH '/'.

    " The counter part to this logic must be maintained in ZCL_ABAPGIT_OBJECTS_FILES->FILENAME
    IF lv_type = 'DEVC'.
      " Try to get a unique package name for DEVC by using the path
      ASSERT lv_name = 'PACKAGE'.
      lv_name = zcl_abapgit_folder_logic=>get_instance( )->path_to_package(
        iv_top                  = iv_devclass
        io_dot                  = io_dot
        iv_create_if_not_exists = abap_false
        iv_path                 = iv_path ).
    ELSE.
      " Get original object name
      lv_name = cl_http_utility=>unescape_url( lv_name ).
    ENDIF.

    CLEAR es_item.
    es_item-obj_type = lv_type.
    es_item-obj_name = lv_name.
    ev_is_xml        = boolc( lv_ext = 'XML' AND strlen( lv_type ) = 4 ).

  ENDMETHOD.


  METHOD run_checks.

    " This method just adds messages to the log. No log, nothing to do here
    IF ii_log IS INITIAL.
      RETURN.
    ENDIF.

    " Find all objects which were assigned to a different package
    check_package_move(
      ii_log     = ii_log
      it_results = it_results ).

    " Check files for one object is in the same folder
    check_files_folder(
      ii_log     = ii_log
      it_results = it_results ).

    " Check that objects are created in package corresponding to folder
    check_package_folder(
      ii_log     = ii_log
      it_results = it_results
        io_dot     = io_dot
      iv_top     = iv_top ).

    " Check for multiple files with same filename
    check_multiple_files(
      ii_log     = ii_log
      it_results = it_results ).

*** abapinst creates namespace automatically
*    " Check if namespaces exist already
*    check_namespace(
*      ii_log     = ii_log
*      it_results = it_results )

  ENDMETHOD.


  METHOD status.

    rt_results = calculate_status(
      iv_devclass  = iv_package
      io_dot       = io_dot
      it_local     = it_local
      it_remote    = it_remote
      it_cur_state = it_local_checksums ).

    run_checks(
      ii_log     = ii_log
      it_results = rt_results
      io_dot     = io_dot
      iv_top     = iv_package ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapinst_popups IMPLEMENTATION.


  METHOD popup_to_confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = iv_title
        text_question         = iv_question
        text_button_1         = iv_text_button_1
        icon_button_1         = iv_icon_button_1
        text_button_2         = iv_text_button_2
        icon_button_2         = iv_icon_button_2
        default_button        = iv_default_button
        display_cancel_button = iv_display_cancel_button
      IMPORTING
        answer                = rv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      zcx_abapinst_exception=>raise( 'Error from POPUP_TO_CONFIRM' ).
    ENDIF.

  ENDMETHOD.


  METHOD popup_to_enter_packaging.

    DATA:
      lt_fields TYPE zcl_abapgit_free_sel_dialog=>ty_free_sel_field_tab,
      lo_dialog TYPE REF TO zcl_abapgit_free_sel_dialog,
      lx_error  TYPE REF TO zcx_abapgit_exception.

    FIELD-SYMBOLS:
      <ls_field> TYPE zcl_abapgit_free_sel_dialog=>ty_free_sel_field.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-name             = 'NAME'.
    <ls_field>-text             = 'Name'.
    <ls_field>-only_parameter   = abap_true.
    <ls_field>-ddic_tabname     = 'E071'.
    <ls_field>-ddic_fieldname   = 'OBJ_NAME'.
    <ls_field>-param_obligatory = abap_true.
    <ls_field>-value            = iv_name.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-name             = 'VERSION'.
    <ls_field>-text             = 'Version'.
    <ls_field>-only_parameter   = abap_true.
    <ls_field>-ddic_tabname     = 'TTREV'.
    <ls_field>-ddic_fieldname   = 'VERSION'.
    <ls_field>-param_obligatory = abap_true.
    <ls_field>-value            = iv_version.

    TRY.
        CREATE OBJECT lo_dialog
          EXPORTING
            iv_title      = |abapinst|
            iv_frame_text = |Packaging Details|.

        lo_dialog->set_fields( CHANGING ct_fields = lt_fields ).
        lo_dialog->show( ).

        LOOP AT lt_fields ASSIGNING <ls_field>.
          CASE <ls_field>-name.
            WHEN 'NAME'.
              rs_packaging-name = <ls_field>-value.
            WHEN 'VERSION'.
              rs_packaging-version = <ls_field>-value.
              rs_packaging-sem_version = zcl_abapgit_version=>conv_str_to_version( rs_packaging-version ).
          ENDCASE.
        ENDLOOP.

      CATCH zcx_abapgit_cancel.
        RETURN.
      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abapinst_exception=>raise( lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD popup_to_select_from_list.

    DATA:
      lv_pfstatus     TYPE sypfkey,
      lo_events       TYPE REF TO cl_salv_events_table,
      lo_columns      TYPE REF TO cl_salv_columns_table,
      lt_columns      TYPE salv_t_column_ref,
      ls_column       TYPE salv_s_column_ref,
      lo_column       TYPE REF TO cl_salv_column_list,
      lo_table_header TYPE REF TO cl_salv_form_text.

    FIELD-SYMBOLS:
      <lt_table>             TYPE STANDARD TABLE,
      <ls_column_to_display> TYPE ty_alv_column.

    CLEAR: et_list.

    _create_new_table( it_list ).

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = mo_select_list_popup
                                CHANGING  t_table = <lt_table> ).

        CASE iv_selection_mode.
          WHEN if_salv_c_selection_mode=>single.
            lv_pfstatus = '110'.

          WHEN OTHERS.
            lv_pfstatus = '102'.

        ENDCASE.

        mo_select_list_popup->set_screen_status( pfstatus = lv_pfstatus
                                                 report = 'SAPMSVIM' ).

        mo_select_list_popup->set_screen_popup( start_column = iv_start_column
                                                end_column   = iv_end_column
                                                start_line   = iv_start_line
                                                end_line     = iv_end_line ).

        lo_events = mo_select_list_popup->get_event( ).

        SET HANDLER _on_select_list_link_click FOR lo_events.
        SET HANDLER _on_select_list_function_click FOR lo_events.
        SET HANDLER _on_double_click FOR lo_events.

        IF iv_title CN ' _0'.
          mo_select_list_popup->get_display_settings( )->set_list_header( iv_title ).
        ENDIF.

        IF iv_header_text CN ' _0'.
          CREATE OBJECT lo_table_header
            EXPORTING
              text = iv_header_text.
          mo_select_list_popup->set_top_of_list( lo_table_header ).
        ENDIF.

        mo_select_list_popup->get_display_settings( )->set_striped_pattern( iv_striped_pattern ).
        mo_select_list_popup->get_selections( )->set_selection_mode( iv_selection_mode ).

        lo_columns = mo_select_list_popup->get_columns( ).
        lt_columns = lo_columns->get( ).
        lo_columns->set_optimize( iv_optimize_col_width ).

        LOOP AT lt_columns INTO ls_column.

          lo_column ?= ls_column-r_column.

          IF iv_selection_mode = if_salv_c_selection_mode=>multiple
            AND ls_column-columnname = c_fieldname_selected.
            lo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
            lo_column->set_output_length( 20 ).
            lo_column->set_short_text( |{ iv_select_column_text }| ).
            lo_column->set_medium_text( |{ iv_select_column_text }| ).
            lo_column->set_long_text( |{ iv_select_column_text }| ).
            CONTINUE.
          ENDIF.

          READ TABLE it_columns_to_display
            ASSIGNING <ls_column_to_display>
            WITH KEY name = ls_column-columnname.

          CASE sy-subrc.
            WHEN 0.
              IF <ls_column_to_display>-text CN ' _0'.
                lo_column->set_short_text( |{ <ls_column_to_display>-text }| ).
                lo_column->set_medium_text( |{ <ls_column_to_display>-text }| ).
                lo_column->set_long_text( |{ <ls_column_to_display>-text }| ).
              ENDIF.

              IF <ls_column_to_display>-length > 0.
                lo_column->set_output_length( <ls_column_to_display>-length ).
              ENDIF.

              lo_column->set_key( <ls_column_to_display>-key ).

            WHEN OTHERS.
              " Hide column
              lo_column->set_technical( abap_true ).

          ENDCASE.

        ENDLOOP.

        mo_select_list_popup->display( ).

      CATCH cx_salv_msg.
        zcx_abapinst_exception=>raise( 'Error from POPUP_TO_SELECT_FROM_LIST' ).
    ENDTRY.

    IF mv_cancel = abap_true.
      mv_cancel = abap_false.
      RETURN.
    ENDIF.

    _get_selected_rows( IMPORTING et_list = et_list ).

    CLEAR: mo_select_list_popup,
           mr_table,
           mo_table_descr.

  ENDMETHOD.


  METHOD _create_new_table.

    " create and populate a table on the fly derived from
    " it_data with a select column

    DATA:
      lr_struct        TYPE REF TO data,
      lt_components    TYPE cl_abap_structdescr=>component_table,
      lo_data_descr    TYPE REF TO cl_abap_datadescr,
      lo_elem_descr    TYPE REF TO cl_abap_elemdescr,
      lo_struct_descr  TYPE REF TO cl_abap_structdescr,
      lo_struct_descr2 TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS:
      <lt_table>     TYPE STANDARD TABLE,
      <ls_component> TYPE abap_componentdescr,
      <lg_line>      TYPE data,
      <lg_data>      TYPE any,
      <lg_value>     TYPE any.

    mo_table_descr ?= cl_abap_tabledescr=>describe_by_data( it_list ).
    lo_data_descr = mo_table_descr->get_table_line_type( ).

    CASE lo_data_descr->kind.
      WHEN cl_abap_elemdescr=>kind_elem.
        lo_elem_descr ?= mo_table_descr->get_table_line_type( ).
        INSERT INITIAL LINE INTO lt_components ASSIGNING <ls_component> INDEX 1.
        <ls_component>-name = c_default_column.
        <ls_component>-type = lo_elem_descr.

      WHEN cl_abap_elemdescr=>kind_struct.
        lo_struct_descr ?= mo_table_descr->get_table_line_type( ).
        lt_components = lo_struct_descr->get_components( ).

    ENDCASE.

    IF lt_components IS INITIAL.
      RETURN.
    ENDIF.

    INSERT INITIAL LINE INTO lt_components ASSIGNING <ls_component> INDEX 1.
    <ls_component>-name = c_fieldname_selected.
    <ls_component>-type ?= cl_abap_datadescr=>describe_by_name( 'FLAG' ).

    lo_struct_descr2 = cl_abap_structdescr=>create( lt_components ).
    mo_table_descr = cl_abap_tabledescr=>create( lo_struct_descr2 ).

    CREATE DATA mr_table TYPE HANDLE mo_table_descr.
    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_struct TYPE HANDLE lo_struct_descr2.
    ASSIGN lr_struct->* TO <lg_line>.
    ASSERT sy-subrc = 0.

    LOOP AT it_list ASSIGNING <lg_data>.
      CLEAR <lg_line>.
      CASE lo_data_descr->kind.
        WHEN cl_abap_elemdescr=>kind_elem.
          ASSIGN COMPONENT c_default_column OF STRUCTURE <lg_data> TO <lg_value>.
          ASSERT <lg_value> IS ASSIGNED.
          <lg_line> = <lg_value>.

        WHEN OTHERS.
          MOVE-CORRESPONDING <lg_data> TO <lg_line>.

      ENDCASE.
      INSERT <lg_line> INTO TABLE <lt_table>.
    ENDLOOP.

  ENDMETHOD.


  METHOD _get_selected_rows.

    DATA:
      lv_condition TYPE string,
      lr_exporting TYPE REF TO data.

    FIELD-SYMBOLS:
      <lg_exporting>    TYPE any,
      <lt_table>        TYPE STANDARD TABLE,
      <lg_line>         TYPE any,
      <lg_value>        TYPE any,
      <lv_selected>     TYPE abap_bool,
      <lv_selected_row> TYPE LINE OF salv_t_row.

    DATA: lo_data_descr    TYPE REF TO cl_abap_datadescr,
          lo_selections    TYPE REF TO cl_salv_selections,
          lt_selected_rows TYPE salv_t_row.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    lo_selections = mo_select_list_popup->get_selections( ).

    IF lo_selections->get_selection_mode( ) = if_salv_c_selection_mode=>single.

      lt_selected_rows = lo_selections->get_selected_rows( ).

      LOOP AT lt_selected_rows ASSIGNING <lv_selected_row>.

        READ TABLE <lt_table>
          ASSIGNING <lg_line>
          INDEX <lv_selected_row>.
        CHECK <lg_line> IS ASSIGNED.

        ASSIGN COMPONENT c_fieldname_selected
           OF STRUCTURE <lg_line>
           TO <lv_selected>.
        CHECK <lv_selected> IS ASSIGNED.

        <lv_selected> = abap_true.

      ENDLOOP.

    ENDIF.

    lv_condition = |{ c_fieldname_selected } = ABAP_TRUE|.

    CREATE DATA lr_exporting LIKE LINE OF et_list.
    ASSIGN lr_exporting->* TO <lg_exporting>.
    ASSERT sy-subrc = 0.

    mo_table_descr ?= cl_abap_tabledescr=>describe_by_data( et_list ).
    lo_data_descr = mo_table_descr->get_table_line_type( ).

    LOOP AT <lt_table> ASSIGNING <lg_line> WHERE (lv_condition).
      CLEAR <lg_exporting>.

      CASE lo_data_descr->kind.
        WHEN cl_abap_elemdescr=>kind_elem.
          ASSIGN COMPONENT c_default_column OF STRUCTURE <lg_line> TO <lg_value>.
          ASSERT sy-subrc = 0.
          <lg_exporting> = <lg_value>.

        WHEN OTHERS.
          MOVE-CORRESPONDING <lg_line> TO <lg_exporting>.

      ENDCASE.
      APPEND <lg_exporting> TO et_list.
    ENDLOOP.

  ENDMETHOD.


  METHOD _on_double_click.

    DATA lo_selections TYPE REF TO cl_salv_selections.

    lo_selections = mo_select_list_popup->get_selections( ).

    IF lo_selections->get_selection_mode( ) = if_salv_c_selection_mode=>single.
      mo_select_list_popup->close_screen( ).
    ENDIF.

  ENDMETHOD.


  METHOD _on_select_list_function_click.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <lg_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    CASE e_salv_function.
      WHEN 'O.K.'.
        mv_cancel = abap_false.
        mo_select_list_popup->close_screen( ).

      WHEN 'ABR'.
        "Canceled: clear list to overwrite nothing
        CLEAR <lt_table>.
        mv_cancel = abap_true.
        mo_select_list_popup->close_screen( ).

      WHEN 'SALL'.
        LOOP AT <lt_table> ASSIGNING <lg_line>.

          ASSIGN COMPONENT c_fieldname_selected
                 OF STRUCTURE <lg_line>
                 TO <lv_selected>.
          ASSERT sy-subrc = 0.

          <lv_selected> = abap_true.

        ENDLOOP.

        mo_select_list_popup->refresh( ).

      WHEN 'DSEL'.
        LOOP AT <lt_table> ASSIGNING <lg_line>.

          ASSIGN COMPONENT c_fieldname_selected
                 OF STRUCTURE <lg_line>
                 TO <lv_selected>.
          ASSERT sy-subrc = 0.

          <lv_selected> = abap_false.

        ENDLOOP.

        mo_select_list_popup->refresh( ).

      WHEN OTHERS.
        CLEAR <lt_table>.
        mo_select_list_popup->close_screen( ).
    ENDCASE.

  ENDMETHOD.


  METHOD _on_select_list_link_click.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <lg_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    READ TABLE <lt_table> ASSIGNING <lg_line> INDEX row.
    IF sy-subrc = 0.

      ASSIGN COMPONENT c_fieldname_selected
             OF STRUCTURE <lg_line>
             TO <lv_selected>.
      ASSERT sy-subrc = 0.

      IF <lv_selected> = abap_true.
        <lv_selected> = abap_false.
      ELSE.
        <lv_selected> = abap_true.
      ENDIF.

    ENDIF.

    mo_select_list_popup->refresh( ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapinst_installer IMPLEMENTATION.


  METHOD check.

    " true:  package already installed (in same or newer version)
    " false: package not installed (or in older version that can be upgraded)

    DATA:
      ls_inst TYPE zif_abapinst_definitions=>ty_inst,
      lv_comp TYPE i.

    init( ).

    IF iv_name IS SUPPLIED AND iv_pack IS SUPPLIED.
      ls_inst = go_db->select( iv_name = iv_name
                               iv_pack = iv_pack ).
    ELSEIF iv_name IS SUPPLIED.
      ls_inst = go_db->select( iv_name = iv_name ).
    ELSE.
      ls_inst = go_db->select( iv_pack = iv_pack ).
    ENDIF.

    IF ls_inst IS INITIAL.
      RETURN. " false
    ENDIF.

    " Version comparison
    IF is_sem_version IS SUPPLIED.
      lv_comp = zcl_abapgit_version=>compare(
        is_a = is_sem_version         " new version
        is_b = ls_inst-sem_version ). " installed version
      IF lv_comp <= 0.
        rv_result = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD f4.

    DATA:
      lt_list     TYPE zif_abapinst_definitions=>ty_list,
      lt_selected LIKE lt_list,
      lo_popup    TYPE REF TO zcl_abapinst_popups,
      lt_columns  TYPE zcl_abapinst_popups=>ty_alv_column_tt,
      lv_question TYPE string,
      lv_answer   TYPE sy-input.

    FIELD-SYMBOLS:
      <ls_column> LIKE LINE OF lt_columns.

    init( ).

    lt_list = go_db->list( ).

    CHECK _nothing_found( lt_list ) IS INITIAL.

    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name   = 'NAME'.
    <ls_column>-text   = 'Name'.
    <ls_column>-length = 30.
    <ls_column>-key    = abap_true.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name   = 'PACK'.
    <ls_column>-text   = 'Package'.
    <ls_column>-length = 30.
    <ls_column>-key    = abap_true.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name   = 'VERSION'.
    <ls_column>-text   = 'Version'.
    <ls_column>-length = 15.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name   = 'DESCRIPTION'.
    <ls_column>-text   = 'Description'.
    <ls_column>-length = 60.

    CREATE OBJECT lo_popup.

    TRY.
        lo_popup->popup_to_select_from_list(
          EXPORTING
            it_list               = lt_list
            iv_title              = sy-title
            iv_header_text        = |Select the { gv_name } that you want to uninstall:|
            iv_end_column         = 150
            iv_striped_pattern    = abap_true
            iv_optimize_col_width = abap_false
            iv_selection_mode     = if_salv_c_selection_mode=>single
            it_columns_to_display = lt_columns
          IMPORTING
            et_list               = lt_selected ).
      CATCH zcx_abapinst_exception.
        RETURN.
    ENDTRY.

    IF lt_selected IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE lt_selected INTO rs_inst INDEX 1.
    ASSERT sy-subrc = 0.

    TRY.
        lv_question = |Are you sure, you want to uninstall "{ rs_inst-description } ({ rs_inst-name })"?|.

        lv_answer = lo_popup->popup_to_confirm(
          iv_title          = sy-title
          iv_question       = lv_question
          iv_default_button = '2' ).

        IF lv_answer <> '1'.
          CLEAR rs_inst.
        ENDIF.

      CATCH zcx_abapinst_exception.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD init.

    IF go_db IS NOT BOUND.
      IF iv_tabname IS INITIAL AND iv_lock IS INITIAL.
        go_db = zcl_abapinst_persistence=>get_instance( ).
      ELSE.
        go_db = zcl_abapinst_persistence=>get_instance(
          iv_tabname = iv_tabname
          iv_lock    = iv_lock ).
      ENDIF.
    ENDIF.

    IF iv_name IS NOT INITIAL OR iv_names IS NOT INITIAL.
      gv_name  = iv_name.
      gv_names = iv_names.
    ENDIF.

  ENDMETHOD.


  METHOD install.

    DATA:
      lx_error TYPE REF TO zcx_abapgit_exception.

    init( ).

    TRY.
        _clear( ).

        _log_start( ).

        _files(
          iv_enum_zip       = iv_enum_zip
          iv_name           = iv_name
          iv_user           = iv_user
          iv_password       = iv_password
          iv_proxy_host     = iv_proxy_host
          iv_proxy_service  = iv_proxy_service
          iv_proxy_user     = iv_proxy_user
          iv_proxy_password = iv_proxy_password ).

        _packaging( ).

        _sap_package(
          iv_enum_package = iv_enum_package
          iv_package      = iv_package ).

        _check( ).

        _folder_logic( iv_enum_folder_logic ).

        _transport(
          iv_enum_transport = iv_enum_transport
          iv_transport      = iv_transport ).

        _confirm_messages( ).

        _namespaces( ).

        zcl_abapinst_objects=>deserialize(
          iv_package   = gs_inst-pack
          iv_language  = gs_inst-installed_langu
          iv_transport = gs_inst-transport
          it_remote    = gt_remote
          io_dot       = go_dot
          ii_log       = gi_log ).

      CATCH zcx_abapgit_exception INTO lx_error.
        _transport_reset( ).

        gi_log->add_exception( lx_error ).
    ENDTRY.

    TRY.
        _log_end( ).

        _save( ).

        _restore_messages( ).

        _final_message( 'Installation' ).

      CATCH zcx_abapgit_exception INTO lx_error.
        ASSERT 1 = 2.
    ENDTRY.

  ENDMETHOD.


  METHOD list.

    DATA:
      lt_list          TYPE zif_abapinst_definitions=>ty_list,
      lo_list          TYPE REF TO cl_salv_table,
      lo_disp_settings TYPE REF TO cl_salv_display_settings,
      lo_functions     TYPE REF TO cl_salv_functions,
      lo_columns       TYPE REF TO cl_salv_columns_table,
      ls_column        TYPE salv_s_column_ref,
      lt_columns       TYPE salv_t_column_ref,
      lo_column        TYPE REF TO cl_salv_column,
      lr_column        TYPE REF TO cl_salv_column_table,
      lx_error         TYPE REF TO cx_salv_error.

    FIELD-SYMBOLS:
      <ls_list> LIKE LINE OF lt_list.

    init( ).

    lt_list = go_db->list( ).

    CHECK _nothing_found( lt_list ) IS INITIAL.

    LOOP AT lt_list ASSIGNING <ls_list>.
      CASE <ls_list>-status.
        WHEN space.
          <ls_list>-status = icon_led_inactive.
        WHEN c_success.
          <ls_list>-status = icon_led_green.
        WHEN c_warning.
          <ls_list>-status = icon_led_yellow.
        WHEN OTHERS.
          <ls_list>-status = icon_led_red.
      ENDCASE.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_list
          CHANGING
            t_table      = lt_list ).

        lo_functions = lo_list->get_functions( ).
        lo_functions->set_all( ).

        lo_columns = lo_list->get_columns( ).
        lo_columns->set_optimize( ).

        lo_column = lo_columns->get_column( 'NAME' ).
        lo_column->set_medium_text( 'Name' ).
        lo_column->set_output_length( 30 ).
        lr_column ?= lo_columns->get_column( 'NAME' ).
        lr_column->set_key( ).

        lo_column = lo_columns->get_column( 'PACK' ).
        lo_column->set_medium_text( 'SAP Package' ).
        lo_column->set_output_length( 30 ).
        lr_column ?= lo_columns->get_column( 'PACK' ).
        lr_column->set_key( ).

        lo_column = lo_columns->get_column( 'VERSION' ).
        lo_column->set_medium_text( 'Version' ).
        lo_column->set_output_length( 12 ).

        lt_columns = lo_columns->get( ).
        LOOP AT lt_columns INTO ls_column WHERE columnname CP 'SEM_VERSION-*'.
          ls_column-r_column->set_technical( ).
        ENDLOOP.

        lo_column = lo_columns->get_column( 'STATUS' ).
        lo_column->set_medium_text( 'Status' ).
        lo_column->set_output_length( 6 ).

        lo_column = lo_columns->get_column( 'DESCRIPTION' ).
        lo_column->set_medium_text( 'Description' ).
        lo_column->set_output_length( 60 ).

        lo_column = lo_columns->get_column( 'SOURCE_TYPE' ).
        lo_column->set_medium_text( 'Type' ).
        lo_column->set_output_length( 10 ).

        lo_column = lo_columns->get_column( 'SOURCE_NAME' ).
        lo_column->set_medium_text( 'Source' ).
        lo_column->set_output_length( 50 ).

        lo_column = lo_columns->get_column( 'TRANSPORT' ).
        lo_column->set_medium_text( 'Transport' ).
        lo_column->set_output_length( 12 ).

        lo_column = lo_columns->get_column( 'FOLDER_LOGIC' ).
        lo_column->set_medium_text( 'Folder Logic' ).
        lo_column->set_output_length( 10 ).

        lo_column = lo_columns->get_column( 'INSTALLED_LANGU' ).
        lo_column->set_medium_text( 'Installed Language' ).
        lo_column->set_output_length( 12 ).

        lo_column = lo_columns->get_column( 'INSTALLED_BY' ).
        lo_column->set_medium_text( 'Installed By' ).
        lo_column->set_output_length( 12 ).

        lo_column = lo_columns->get_column( 'INSTALLED_AT' ).
        lo_column->set_short_text( 'Installed' ).
        lo_column->set_medium_text( 'Installed At' ).
        lo_column->set_output_length( 18 ).

        lo_column = lo_columns->get_column( 'UPDATED_BY' ).
        lo_column->set_medium_text( 'Updated By' ).
        lo_column->set_output_length( 12 ).

        lo_column = lo_columns->get_column( 'UPDATED_AT' ).
        lo_column->set_short_text( 'Updated' ).
        lo_column->set_medium_text( 'Updated At' ).
        lo_column->set_output_length( 18 ).

        lo_disp_settings = lo_list->get_display_settings( ).
        lo_disp_settings->set_list_header( sy-title ).
        lo_disp_settings->set_fit_column_to_table_size( ).

        lo_list->display( ).
      CATCH cx_salv_error INTO lx_error.
        zcx_abapinst_exception=>raise( lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD uninstall.

    DATA:
      lx_error TYPE REF TO zcx_abapgit_exception,
      lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.

    CLEAR: gs_inst, gs_packaging, go_dot.

    init( ).

    TRY.
        _clear( ).

        _log_start( ).

        _load(
          iv_name = iv_name
          iv_pack = iv_pack ).

        _transport( ty_enum_transport-prompt ).

        _confirm_messages( ).

        " A few tries to tackle dependencies
        DO 3 TIMES.
          lt_tadir = zcl_abapinst_factory=>get_tadir( )->read( gs_inst-pack ).

          IF lt_tadir IS NOT INITIAL.
            _uninstall_sotr( lt_tadir ).

            zcl_abapinst_objects=>delete(
              it_tadir     = lt_tadir
              iv_transport = gs_inst-transport
              ii_log       = gi_log ).
          ENDIF.
        ENDDO.

      CATCH zcx_abapgit_exception INTO lx_error.
        _transport_reset( ).

        gi_log->add_exception( lx_error ).
    ENDTRY.

    TRY.
        _log_end( ).

        _check_uninstalled( lt_tadir ).

        IF gs_inst-status = c_success.
          _delete( ).
        ELSE.
          _save( ).
        ENDIF.

        _restore_messages( ).

        _final_message( 'Uninstall' ).

      CATCH zcx_abapgit_exception INTO lx_error.
        ASSERT 1 = 2.
    ENDTRY.

  ENDMETHOD.


  METHOD _check.

    DATA:
      lv_msg      TYPE string,
      lv_question TYPE string,
      lo_popup    TYPE REF TO zcl_abapinst_popups,
      lv_answer   TYPE c LENGTH 1.

    IF check( iv_name        = gs_inst-name
              iv_pack        = gs_inst-pack
              is_sem_version = gs_inst-sem_version ) = abap_true.

      lv_msg = |{ gv_name } is already installed (with same or newer version)|.
      lv_question = lv_msg  && '. Do you want to overwrite it?'.

      IF gs_inst-status = c_success AND iv_force IS INITIAL.
        CREATE OBJECT lo_popup.

        lv_answer = lo_popup->popup_to_confirm(
          iv_title          = sy-title
          iv_question       = lv_question
          iv_default_button = '2' ).

        IF lv_answer <> '1'.
          zcx_abapinst_exception=>raise( lv_msg ).
        ENDIF.
      ENDIF.

    ELSEIF check( iv_pack = gs_inst-pack ) = abap_true.
      zcx_abapinst_exception=>raise( |SAP package { gs_inst-pack } already contains a different { gv_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD _check_uninstalled.

    DATA:
      lv_msg   TYPE string,
      lt_tadir LIKE it_tadir ##NEEDED.

    CHECK it_tadir IS NOT INITIAL.

    SELECT pgmid object obj_name FROM tadir INTO CORRESPONDING FIELDS OF TABLE lt_tadir
      FOR ALL ENTRIES IN it_tadir
      WHERE pgmid    = it_tadir-pgmid
        AND object   = it_tadir-object
        AND obj_name = it_tadir-obj_name ##TOO_MANY_ITAB_FIELDS.
    IF sy-subrc = 0.
      gs_inst-status = c_warning.
      lv_msg = `Some objects could not be uninstalled yet. Release the transport and run the uninstall again` &&
               ` to remove the remaining objects.`.
      MESSAGE lv_msg TYPE 'I'.
    ENDIF.

  ENDMETHOD.


  METHOD _clear.
    CLEAR: gs_inst, gs_packaging, go_dot.
  ENDMETHOD.


  METHOD _confirm_messages.

    " Temporarily suppress certain messages that are not relevant for installation

    CONSTANTS lc_toolflag_set TYPE funcname VALUE 'SCWG_TOOLFLAG_SET'.

    TYPES:
      BEGIN OF ty_message,
        id TYPE symsgid,
        ty TYPE symsgty,
        no TYPE symsgno,
        v1 TYPE symsgv,
        v2 TYPE symsgv,
        v3 TYPE symsgv,
        v4 TYPE symsgv,
      END OF ty_message.

    DATA:
      ls_msg    TYPE ty_message,
      ls_clmcus TYPE clmcus.

    FIELD-SYMBOLS <lt_msg> TYPE STANDARD TABLE.

    " Auto-confirm certain messages (requires SAP Note 1609940)
    PERFORM dummy IN PROGRAM saplstrd IF FOUND.  "load function group STRD
    ASSIGN ('(SAPLSTRD)GT_CONFIRMED_MESSAGES') TO <lt_msg>.
    IF sy-subrc = 0.
      " Object can only be created in package of namespace
      ls_msg-id = 'TR'.
      ls_msg-no = '007'.
      COLLECT ls_msg INTO <lt_msg>.
      " Original system set to "SAP"
      ls_msg-id = 'TR'.
      ls_msg-no = '013'.
      COLLECT ls_msg INTO <lt_msg>.
      " Make repairs in foreign namespaces only if they are urgent
      ls_msg-id = 'TR'.
      ls_msg-no = '852'.
      COLLECT ls_msg INTO <lt_msg>.
      " Make repairs in foreign namespaces only if they are urgent
      ls_msg-id = 'TK'.
      ls_msg-no = '016'.
      COLLECT ls_msg INTO <lt_msg>.
    ENDIF.

    " Set tool flag to avoid messages
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = lc_toolflag_set
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    IF sy-subrc = 0.
      CALL FUNCTION lc_toolflag_set.
    ENDIF.

    " Confirm message about modification mode (DT, CLM_INFORMATION)
    " and backup old state
    SELECT * FROM clmcus INTO TABLE gt_clmcus WHERE username = sy-uname ##SUBRC_OK.
    CHECK sy-subrc >= 0. "abaplint
    ls_clmcus-username = sy-uname.
    ls_clmcus-obj_type = 'CLAS'.
    INSERT clmcus FROM ls_clmcus ##SUBRC_OK.
    CHECK sy-subrc >= 0. "abaplint
    ls_clmcus-obj_type = 'INTF'.
    INSERT clmcus FROM ls_clmcus ##SUBRC_OK.
    CHECK sy-subrc >= 0. "abaplint
    ls_clmcus-obj_type = 'METH'.
    INSERT clmcus FROM ls_clmcus ##SUBRC_OK.
    CHECK sy-subrc >= 0. "abaplint

  ENDMETHOD.


  METHOD _delete.

    go_db->delete(
      iv_name = gs_inst-name
      iv_pack = gs_inst-pack ).

  ENDMETHOD.


  METHOD _files.

    DATA lv_xstr TYPE xstring.

    " Load abapGit ZIP File
    gs_inst-source_name = iv_name.

    CASE iv_enum_zip.
      WHEN ty_enum_zip-internet.
        gs_inst-source_type = 'INTERNET'.
        lv_xstr = zcl_abapinst_file=>load_internet(
                    iv_url            = |{ iv_name }|
                    iv_user           = |{ iv_user }|
                    iv_password       = |{ iv_password }|
                    iv_proxy_host     = |{ iv_proxy_host }|
                    iv_proxy_port     = |{ iv_proxy_service }|
                    iv_proxy_user     = |{ iv_proxy_user }|
                    iv_proxy_password = |{ iv_proxy_password }| ).
      WHEN ty_enum_zip-local.
        gs_inst-source_type = 'LOCAL'.
        lv_xstr = zcl_abapinst_file=>load_local( iv_name ).
      WHEN ty_enum_zip-server.
        gs_inst-source_type = 'SERVER'.
        lv_xstr = zcl_abapinst_file=>load_server( iv_name ).
      WHEN ty_enum_zip-data.
        gs_inst-source_type = 'DATA'.
        lv_xstr = iv_data.
      WHEN OTHERS.
        zcx_abapinst_exception=>raise( |Unknown source for { gv_name }| ).
    ENDCASE.

    " Scan for viruses and unzip
    zcl_abapinst_file=>virus_scan( lv_xstr ).

    gt_remote = zcl_abapinst_file=>unzip( lv_xstr ).

  ENDMETHOD.


  METHOD _final_message.

    DATA lv_msg TYPE string.

    CASE gs_inst-status.
      WHEN c_success.
        lv_msg = |{ iv_type } of "{ gs_inst-name }" successfully completed|.
        MESSAGE lv_msg TYPE c_success.
      WHEN c_warning.
        lv_msg = |{ iv_type } of "{ gs_inst-name }" finished with warnings|.
        MESSAGE lv_msg TYPE c_success DISPLAY LIKE c_warning.
      WHEN c_error.
        lv_msg = |{ iv_type } of "{ gs_inst-name }" finshed with errors|.
        MESSAGE lv_msg TYPE c_success DISPLAY LIKE c_error.
      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.

  ENDMETHOD.


  METHOD _find_remote_dot_abapgit.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF it_remote.

    READ TABLE it_remote ASSIGNING <ls_remote> WITH KEY
      path     = zif_abapgit_definitions=>c_root_dir
      filename = zif_abapgit_definitions=>c_dot_abapgit.
    IF sy-subrc = 0.
      TRY.
          ro_dot = zcl_abapgit_dot_abapgit=>deserialize( <ls_remote>-data ).
        CATCH zcx_abapgit_exception.
          zcx_abapinst_exception=>raise( |Error decoding .abapgit.xml| ).
      ENDTRY.
    ELSE.
      zcx_abapinst_exception=>raise( |Error finding .abapgit.xml - Is this an { gv_name }?| ).
    ENDIF.

  ENDMETHOD.


  METHOD _find_remote_dot_apack.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF it_remote.

    READ TABLE it_remote ASSIGNING <ls_remote> WITH KEY
      path     = zif_abapgit_definitions=>c_root_dir
      filename = '.apack-manifest.xml'.
    IF sy-subrc = 0.
      zcx_abapinst_exception=>raise( |Please migrate APACK to { gv_name } setting| ).
    ENDIF.

  ENDMETHOD.


  METHOD _find_remote_namespaces.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF gt_remote.

    LOOP AT gt_remote ASSIGNING <ls_remote> WHERE filename CP '*.nspc.xml'.
      INSERT <ls_remote> INTO TABLE rt_remote.
    ENDLOOP.

  ENDMETHOD.


  METHOD _folder_logic.

    CASE iv_enum_folder_logic.
      WHEN ty_enum_folder_logic-default.
        gs_inst-folder_logic = go_dot->get_folder_logic( ).
      WHEN ty_enum_folder_logic-prefix.
        gs_inst-folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-prefix.
      WHEN ty_enum_folder_logic-full.
        gs_inst-folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-full.
      WHEN OTHERS.
        zcx_abapinst_exception=>raise( |Unknown folder logic| ).
    ENDCASE.

  ENDMETHOD.


  METHOD _load.

    gs_inst = go_db->select(
      iv_name = iv_name
      iv_pack = iv_pack ).

  ENDMETHOD.


  METHOD _log_end.
    gs_inst-status = gi_log->get_status( ).
    IF gs_inst-status <> c_success.
      zcl_abapinst_log_viewer=>show_log( gi_log ).
    ENDIF.
  ENDMETHOD.


  METHOD _log_start.
    CREATE OBJECT gi_log TYPE zcl_abapgit_log.
    gi_log->set_title( |{ sy-title } Log| ).
  ENDMETHOD.


  METHOD _namespaces.

    DATA:
      lx_error  TYPE REF TO zcx_abapgit_exception,
      lt_remote TYPE zif_abapgit_definitions=>ty_files_tt.

    " Namespaces must be created upfront,
    " otherwise folder_logic->path_to_package will fail
    lt_remote = _find_remote_namespaces( ).

    IF lines( lt_remote ) > 0.
      TRY.
          zcl_abapinst_objects=>deserialize(
            iv_package   = gs_inst-pack
            iv_language  = gs_inst-installed_langu
            iv_transport = gs_inst-transport
            it_remote    = lt_remote
            io_dot       = go_dot
            ii_log       = gi_log ).

          COMMIT WORK.
        CATCH zcx_abapgit_exception INTO lx_error.
          zcx_abapinst_exception=>raise( lx_error->get_text( ) ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD _nothing_found.

    DATA lv_msg TYPE string.

    IF it_list IS INITIAL.
      lv_msg = |No { gv_names } found|.
      MESSAGE lv_msg TYPE c_success.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD _packaging.

    DATA:
      lv_name  TYPE string,
      lo_popup TYPE REF TO zcl_abapinst_popups.

    go_dot = _find_remote_dot_abapgit( gt_remote ).
    gs_inst-installed_langu = go_dot->get_main_language( ).
    gs_packaging = go_dot->get_packaging( ).

    IF gs_packaging IS INITIAL.
      " Check if APACK file exists and ask migrate it abapGit settings
      _find_remote_dot_apack( gt_remote ).

      " Heuristic to get a name proposal
      TRY.
          lv_name = zcl_abapgit_url=>name( gs_inst-source_name ).
          IF lv_name CS 'URL error'.
            lv_name = gs_inst-source_name.
          ENDIF.
        CATCH zcx_abapgit_exception.
          lv_name = gs_inst-source_name.
      ENDTRY.
      REPLACE '.zip' IN lv_name WITH '' IGNORING CASE.
      REPLACE '.git' IN lv_name WITH '' IGNORING CASE.

      CREATE OBJECT lo_popup.
      gs_packaging = lo_popup->popup_to_enter_packaging(
                       iv_name    = lv_name
                       iv_version = '1.0.0' ).

      IF gs_packaging IS INITIAL.
        zcx_abapinst_exception=>raise( |Unable to install without name and version details| ).
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING gs_packaging TO gs_inst.

  ENDMETHOD.


  METHOD _restore_messages.

    DELETE FROM clmcus WHERE username = sy-uname ##SUBRC_OK.
    CHECK sy-subrc >= 0. "abaplint
    INSERT clmcus FROM TABLE gt_clmcus ##SUBRC_OK.
    CHECK sy-subrc >= 0. "abaplint

  ENDMETHOD.


  METHOD _sap_package.

    CASE iv_enum_package.
      WHEN ty_enum_package-default.
        gs_inst-pack = gs_packaging-target_package.
      WHEN ty_enum_package-local.
        IF iv_package(1) <> '$'.
          zcx_abapinst_exception=>raise( |Local package must begin with $| ).
        ENDIF.
        gs_inst-pack = iv_package.
      WHEN ty_enum_package-transportable.
        IF iv_package(1) = '$'.
          zcx_abapinst_exception=>raise( |Transportable package must not begin with $| ).
        ENDIF.
        gs_inst-pack = iv_package.
      WHEN OTHERS.
        zcx_abapinst_exception=>raise( |Unknown type of target package| ).
    ENDCASE.

  ENDMETHOD.


  METHOD _save.

    DATA:
      ls_inst      TYPE zif_abapinst_definitions=>ty_inst,
      lv_timestamp TYPE timestamp.

    GET TIME STAMP FIELD lv_timestamp.

    ls_inst = go_db->select(
      iv_name = gs_inst-name
      iv_pack = gs_inst-pack ).

    IF ls_inst IS INITIAL.
      ls_inst-name            = gs_inst-name.
      ls_inst-pack            = gs_inst-pack.
      ls_inst-version         = gs_inst-version.
      ls_inst-sem_version     = gs_inst-sem_version.
      ls_inst-description     = gs_inst-description.
      ls_inst-source_type     = gs_inst-source_type.
      ls_inst-source_name     = gs_inst-source_name.
      ls_inst-transport       = gs_inst-transport.
      ls_inst-folder_logic    = gs_inst-folder_logic.
      ls_inst-installed_langu = gs_inst-installed_langu.
      ls_inst-installed_by    = sy-uname.
      ls_inst-installed_at    = lv_timestamp.
      ls_inst-status          = gs_inst-status.

      go_db->insert( ls_inst ).
    ELSE.
      ls_inst-version         = gs_inst-version.
      ls_inst-sem_version     = gs_inst-sem_version.
      ls_inst-description     = gs_inst-description.
      ls_inst-source_type     = gs_inst-source_type.
      ls_inst-source_name     = gs_inst-source_name.
      ls_inst-transport       = gs_inst-transport.
      ls_inst-folder_logic    = gs_inst-folder_logic.
      ls_inst-installed_langu = gs_inst-installed_langu.
      ls_inst-updated_by      = sy-uname.
      ls_inst-updated_at      = lv_timestamp.
      ls_inst-status          = gs_inst-status.

      go_db->update( ls_inst ).
    ENDIF.

  ENDMETHOD.


  METHOD _transport.

    DATA lv_trkorr TYPE trkorr.

    CHECK gs_inst-pack(1) <> '$'.

    CASE iv_enum_transport.
      WHEN ty_enum_transport-existing.
        gs_inst-transport = iv_transport.
      WHEN ty_enum_transport-prompt.
        TRY.
            lv_trkorr = zcl_abapgit_default_transport=>get_instance( )->get( )-ordernum.
          CATCH zcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.

        gs_inst-transport = zcl_abapinst_screen=>f4_transport(
          iv_package   = gs_inst-pack
          iv_transport = lv_trkorr ).

        IF gs_inst-transport IS INITIAL.
          zcx_abapinst_exception=>raise( 'No transport selected. Installation cancelled' ).
        ENDIF.
    ENDCASE.

    _transport_check( ).

  ENDMETHOD.


  METHOD _transport_check.

    DATA:
      lv_text            TYPE as4text,
      ls_request_header  TYPE trwbo_request_header,
      lt_request_headers TYPE trwbo_request_headers.

    CHECK gs_inst-pack(1) <> '$'.

    lv_text = gs_inst-name && ':' && gs_inst-description.

    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
      EXPORTING
        iv_trkorr          = gs_inst-transport
      IMPORTING
        et_request_headers = lt_request_headers
      EXCEPTIONS
        invalid_input      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      zcx_abapinst_exception=>raise_t100( ).
    ENDIF.

    " Request Type: Workbench
    READ TABLE lt_request_headers INTO ls_request_header
      WITH KEY trfunction = 'K' trstatus = 'D' korrdev = 'SYST'.
    IF sy-subrc <> 0.
      zcx_abapinst_exception=>raise( |Transport { gs_inst-transport } is not a changeable "workbench request"| ).
    ENDIF.

    " Task Type: Unclassified (ok)
    READ TABLE lt_request_headers TRANSPORTING NO FIELDS
      WITH KEY trfunction = 'X' trstatus = 'D' korrdev = 'SYST'.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    " Task Type: Development
    READ TABLE lt_request_headers TRANSPORTING NO FIELDS
      WITH KEY trfunction = 'S' trstatus = 'D' korrdev = 'SYST'.
    IF sy-subrc <> 0.
      CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
        EXPORTING
          wi_kurztext   = lv_text
          wi_trfunction = 'S'
          wi_strkorr    = ls_request_header-trkorr
        EXCEPTIONS
          OTHERS        = 1.
      IF sy-subrc <> 0.
        zcx_abapinst_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    " Task Type: Repair (for namespaced projects)
    IF gs_inst-pack(1) = '/'.
      READ TABLE lt_request_headers TRANSPORTING NO FIELDS
        WITH KEY trfunction = 'R' trstatus = 'D' korrdev = 'SYST'.
      IF sy-subrc <> 0.
        CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
          EXPORTING
            wi_kurztext   = lv_text
            wi_trfunction = 'R'
            wi_strkorr    = ls_request_header-trkorr
          EXCEPTIONS
            OTHERS        = 1.
        IF sy-subrc <> 0.
          zcx_abapinst_exception=>raise_t100( ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD _transport_reset.

    TRY.
        zcl_abapgit_default_transport=>get_instance( )->reset( ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD _uninstall_sotr.

    " Necessary since older released do not delete SOTR when package is deleted

    DATA:
      lt_sotr_head TYPE STANDARD TABLE OF sotr_head WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_tadir>     LIKE LINE OF it_tadir,
      <ls_sotr_head> LIKE LINE OF lt_sotr_head.

    LOOP AT it_tadir ASSIGNING <ls_tadir> WHERE object = 'DEVC'.

      SELECT * FROM sotr_head INTO TABLE lt_sotr_head
        WHERE paket = <ls_tadir>-obj_name.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      LOOP AT lt_sotr_head ASSIGNING <ls_sotr_head>.
        CALL FUNCTION 'BTFR_DELETE_SINGLE_TEXT'
          EXPORTING
            concept             = <ls_sotr_head>-concept
            flag_string         = abap_true
          EXCEPTIONS
            text_not_found      = 1
            invalid_package     = 2
            text_not_changeable = 3
            text_enqueued       = 4
            no_correction       = 5
            parameter_error     = 6
            OTHERS              = 7.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDLOOP.

      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_test_modus         = abap_false
          wi_delete_tadir_entry = abap_true
          wi_tadir_pgmid        = 'R3TR'
          wi_tadir_object       = 'SOTR'
          wi_tadir_obj_name     = <ls_tadir>-obj_name
        EXCEPTIONS
          OTHERS                = 1.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapinst_log_viewer IMPLEMENTATION.


  METHOD calculate_cell_type.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF gt_log.
    DATA: ls_cell_type LIKE LINE OF <ls_log>-cell_type.

    LOOP AT gt_log ASSIGNING <ls_log>.

      IF <ls_log>-longtext IS NOT INITIAL.
        ls_cell_type-columnname = `LONGTEXT`.
        ls_cell_type-value      = if_salv_c_cell_type=>hotspot.
        INSERT ls_cell_type INTO TABLE <ls_log>-cell_type.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD dispatch.

*** abapinst
    IF iv_column = `LONGTEXT`.

      show_longtext( is_log ).

    ENDIF.

  ENDMETHOD.


  METHOD on_link_click.

    DATA: lx_error TYPE REF TO zcx_abapgit_exception.
    FIELD-SYMBOLS: <ls_log> TYPE ty_log_out.

    IF row IS INITIAL
    OR column IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE gt_log ASSIGNING <ls_log>
                      INDEX row.
    ASSERT sy-subrc = 0.

    TRY.
        dispatch(
            is_log    = <ls_log>
            iv_column = column ).

      CATCH zcx_abapgit_exception INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD prepare_log_for_display.

    DATA: lt_message      TYPE zif_abapgit_log=>ty_log_outs,
          lr_message      TYPE REF TO zif_abapgit_log=>ty_log_out,
          ls_log          TYPE ty_log_out,
          li_t100_message TYPE REF TO if_t100_message,
          lx_abapgit      TYPE REF TO zcx_abapgit_exception.

    lt_message = ii_log->get_messages( ).

    LOOP AT lt_message REFERENCE INTO lr_message.

      CLEAR: ls_log.

      ls_log-msg = lr_message->text.
      ls_log-exception = lr_message->exception.

      CASE lr_message->type.
        WHEN 'E' OR 'A' OR 'X'.
          ls_log-type = icon_led_red.
        WHEN 'W'.
          ls_log-type = icon_led_yellow.
        WHEN 'I' OR 'S'.
          ls_log-type = icon_led_green.
        WHEN OTHERS.
          ls_log-type = icon_led_inactive.
      ENDCASE.

      IF lr_message->exception IS BOUND.

        TRY.
            li_t100_message ?= lr_message->exception.

            IF li_t100_message->t100key IS NOT INITIAL.
              ls_log-t100 = icon_message_information.
            ENDIF.

          CATCH cx_sy_move_cast_error ##NO_HANDLER.
        ENDTRY.

        TRY.
            lx_abapgit ?= lr_message->exception.

            IF lx_abapgit->mt_callstack IS NOT INITIAL.
              ls_log-longtext  = icon_system_help.
              ls_log-callstack = icon_stack.
              ls_log-source    = icon_abap.
            ENDIF.

          CATCH cx_sy_move_cast_error ##NO_HANDLER.
        ENDTRY.

      ENDIF.

      ls_log-obj_type = lr_message->obj_type.
      ls_log-obj_name = lr_message->obj_name.

      INSERT ls_log INTO TABLE rt_log_out.

    ENDLOOP.

  ENDMETHOD.


  METHOD show_log.

    DATA: lr_log         TYPE REF TO ty_log_out,
          lo_alv         TYPE REF TO cl_salv_table,
          lx_error       TYPE REF TO cx_salv_error,
          lo_form_header TYPE REF TO cl_salv_form_header_info,
          lo_columns     TYPE REF TO cl_salv_columns_table,
          lo_column      TYPE REF TO cl_salv_column,
          lo_functions   TYPE REF TO cl_salv_functions_list,
          lv_add_obj_col TYPE abap_bool,
          lo_event       TYPE REF TO cl_salv_events_table.

    gt_log = prepare_log_for_display( ii_log ).

    "check if log contains any object info
    LOOP AT gt_log REFERENCE INTO lr_log.
      IF lr_log->obj_type IS NOT INITIAL OR lr_log->obj_name IS NOT INITIAL.
        lv_add_obj_col = abap_true.
      ENDIF.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = gt_log ).

        lo_functions = lo_alv->get_functions( ).
        lo_functions->set_all( ).

        lo_columns = lo_alv->get_columns( ).

        lo_columns->set_optimize( ).
        lo_columns->set_cell_type_column( |CELL_TYPE| ).

        calculate_cell_type( ).

        lo_column = lo_columns->get_column( |TYPE| ).
        lo_column->set_medium_text( |Type| ).

        lo_column = lo_columns->get_column( |MSG| ).
        lo_column->set_medium_text( |Message| ).

        lo_column = lo_columns->get_column( |LONGTEXT| ).
        lo_column->set_medium_text( |Longtext| ).

        lo_column = lo_columns->get_column( |OBJ_TYPE| ).
        lo_column->set_medium_text( |Object Type| ).

        IF lv_add_obj_col = abap_false.
          lo_column->set_technical( abap_true ).
        ENDIF.

        lo_column = lo_columns->get_column( |OBJ_NAME| ).
        lo_column->set_medium_text( |Object Name| ).

        IF lv_add_obj_col = abap_false.
          lo_column->set_technical( abap_true ).
        ENDIF.

        lo_alv->set_screen_popup( start_column = 10
                                  end_column   = 180
                                  start_line   = 4
                                  end_line     = 25 ).

        CREATE OBJECT lo_form_header
          EXPORTING
            text = ii_log->get_title( ).

        lo_alv->set_top_of_list( lo_form_header ).

        lo_event = lo_alv->get_event( ).

        SET HANDLER on_link_click FOR lo_event.

        lo_alv->display( ).

      CATCH cx_salv_error INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD show_longtext.

    DATA: lx_abapgit TYPE REF TO zcx_abapgit_exception.

    DATA: lv_docu_object TYPE dokhl-object,
          lt_dummy1      TYPE TABLE OF dselc,
          lt_dummy2      TYPE TABLE OF dval,
          ls_help_info   TYPE help_info.

    IF is_log-exception IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        lx_abapgit ?= is_log-exception.
      CATCH cx_sy_move_cast_error.
        RETURN.
    ENDTRY.

    lv_docu_object   = lx_abapgit->if_t100_message~t100key-msgid.
    lv_docu_object+2 = lx_abapgit->if_t100_message~t100key-msgno.

    ls_help_info-call       = 'D'.
    ls_help_info-spras      = sy-langu.
    ls_help_info-messageid  = lx_abapgit->if_t100_message~t100key-msgid.
    ls_help_info-messagenr  = lx_abapgit->if_t100_message~t100key-msgno.
    ls_help_info-message    = is_log-msg.
    ls_help_info-title      = 'Longtext'.
    ls_help_info-docuid     = 'NA'.
    ls_help_info-docuobject = lv_docu_object.
    ls_help_info-msgv1      = lx_abapgit->msgv1.
    ls_help_info-msgv2      = lx_abapgit->msgv2.
    ls_help_info-msgv3      = lx_abapgit->msgv3.
    ls_help_info-msgv4      = lx_abapgit->msgv4.

    CALL FUNCTION 'HELP_START'
      EXPORTING
        help_infos   = ls_help_info
      TABLES
        dynpselect   = lt_dummy1
        dynpvaluetab = lt_dummy2
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc IS NOT INITIAL.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapinst_objects IMPLEMENTATION.


  METHOD adjust_namespaces.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF rt_results.

    rt_results = it_results.

    LOOP AT rt_results ASSIGNING <ls_result>.
      REPLACE ALL OCCURRENCES OF '#' IN <ls_result>-obj_name WITH '/'.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_main_package.

    " check package restrictions, closed package, descriptive or
    " functional package
    cl_pak_object_types=>check_object_type(
      EXPORTING
        i_working_mode         = 'I'
        i_package_name         = iv_package
        i_pgmid                = 'R3TR'
        i_object_type          = iv_obj_type
      EXCEPTIONS
        wrong_object_type      = 1
        package_not_extensible = 2
        package_not_loaded     = 3
        OTHERS                 = 4 ).
    CASE sy-subrc.
      WHEN 0.
        RETURN.
      WHEN 2.
        zcx_abapgit_exception=>raise( |Object type { iv_obj_type } not allowed for package { iv_package }| ).
      WHEN OTHERS.
        zcx_abapgit_exception=>raise_t100( ).
    ENDCASE.

  ENDMETHOD.


  METHOD check_objects_locked.

    DATA: li_obj TYPE REF TO zif_abapgit_object.

    FIELD-SYMBOLS: <ls_item> LIKE LINE OF it_items.

    LOOP AT it_items ASSIGNING <ls_item>.

      " You should remember that we ignore not supported objects here,
      " because otherwise the process aborts which is not desired
      IF is_supported( <ls_item> ) = abap_false.
        CONTINUE.
      ENDIF.

      li_obj = create_object( is_item     = <ls_item>
                              iv_language = iv_language ).

      IF li_obj->is_locked( ) = abap_true.
        zcx_abapgit_exception=>raise( |Object { <ls_item>-obj_type } { <ls_item>-obj_name } |
                                   && |is locked. Action not possible.| ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD class_name.

    CONCATENATE 'ZCL_ABAPGIT_OBJECT_' is_item-obj_type INTO rv_class_name.

  ENDMETHOD.


  METHOD compare_remote_to_local.
* this method is used for comparing local with remote objects
* before pull, this is useful eg. when overwriting a TABL object.
* only the main XML file is used for comparison

    DATA: ls_remote_file      TYPE zif_abapgit_definitions=>ty_file,
          li_remote_version   TYPE REF TO zif_abapgit_xml_input,
          lv_count            TYPE i,
          ls_result           TYPE zif_abapgit_comparator=>ty_result,
          lv_answer           TYPE string,
          li_comparator       TYPE REF TO zif_abapgit_comparator,
          lv_gui_is_available TYPE abap_bool,
          ls_item             TYPE zif_abapgit_definitions=>ty_item.

    FIND ALL OCCURRENCES OF '.' IN is_result-filename MATCH COUNT lv_count.

    IF is_result-filename CS '.XML' AND lv_count = 2.
      IF ii_object->exists( ) = abap_false.
        RETURN.
      ENDIF.

      READ TABLE it_remote WITH KEY filename = is_result-filename INTO ls_remote_file.
      IF sy-subrc <> 0. "if file does not exist in remote, we don't need to validate
        RETURN.
      ENDIF.

      li_comparator = ii_object->get_comparator( ).
      IF li_comparator IS NOT BOUND.
        RETURN.
      ENDIF.

      CREATE OBJECT li_remote_version
        TYPE zcl_abapgit_xml_input
        EXPORTING
          iv_xml      = zcl_abapgit_convert=>xstring_to_string_utf8( ls_remote_file-data )
          iv_filename = ls_remote_file-filename.

      ls_result = li_comparator->compare( ii_remote = li_remote_version
                                          ii_log = ii_log ).
      IF ls_result-text IS INITIAL.
        RETURN.
      ENDIF.

      "log comparison result
      ls_item-obj_type = is_result-obj_type.
      ls_item-obj_name = is_result-obj_name.
      ii_log->add_warning( iv_msg = ls_result-text
                           is_item = ls_item ).

      "continue or abort?
      lv_gui_is_available = zcl_abapinst_factory=>get_gui_functions( )->gui_is_available( ).
      IF lv_gui_is_available = abap_true.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Warning'
            text_question         = ls_result-text
            text_button_1         = 'Abort'
            icon_button_1         = 'ICON_CANCEL'
            text_button_2         = 'Pull anyway'
            icon_button_2         = 'ICON_OKAY'
            default_button        = '2'
            display_cancel_button = abap_false
          IMPORTING
            answer                = lv_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF sy-subrc <> 0 OR lv_answer = 1.
          zcx_abapgit_exception=>raise( |Deserialization for object { is_result-obj_name } | &
                                        |(type { is_result-obj_type }) aborted by user| ).
        ENDIF.
      ELSE.
        zcx_abapgit_exception=>raise( |Deserialization for object { is_result-obj_name } | &
                                      |(type { is_result-obj_type }) aborted, user descision required| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD create_object.

    DATA: lv_message            TYPE string,
          lv_class_name         TYPE string,
          ls_obj_serializer_map LIKE LINE OF gt_obj_serializer_map.


    READ TABLE gt_obj_serializer_map
      INTO ls_obj_serializer_map WITH KEY item = is_item.
    IF sy-subrc = 0.
      lv_class_name = ls_obj_serializer_map-metadata-class.
    ELSEIF is_metadata IS NOT INITIAL.
*        Metadata is provided only on serialization
*        Once this has been triggered, the same serializer shall be used
*        for subsequent processes.
*        Thus, buffer the metadata afterwards
      ls_obj_serializer_map-item      = is_item.
      ls_obj_serializer_map-metadata  = is_metadata.
      INSERT ls_obj_serializer_map INTO TABLE gt_obj_serializer_map.
      lv_class_name = is_metadata-class.
    ELSE.
      lv_class_name = class_name( is_item ).
    ENDIF.

    REPLACE FIRST OCCURRENCE OF 'LCL' IN lv_class_name WITH 'ZCL_ABAPGIT'.

    TRY.
        CREATE OBJECT ri_obj TYPE (lv_class_name)
          EXPORTING
            is_item     = is_item
            iv_language = iv_language.
      CATCH cx_sy_create_object_error.
        lv_message = |Object type { is_item-obj_type } not supported, serialize|.
        IF iv_native_only = abap_false.
          TRY. " 2nd step, try looking for plugins
              CREATE OBJECT ri_obj TYPE zcl_abapgit_objects_bridge
                EXPORTING
                  is_item = is_item.
            CATCH cx_sy_create_object_error.
              zcx_abapgit_exception=>raise( lv_message ).
          ENDTRY.
        ELSE. " No native support? -> fail
          zcx_abapgit_exception=>raise( lv_message ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD delete.

    DATA: ls_item     TYPE zif_abapgit_definitions=>ty_item,
          li_progress TYPE REF TO zif_abapgit_progress,
          lt_tadir    LIKE it_tadir,
          lt_deleted  LIKE it_tadir,
          lt_items    TYPE zif_abapgit_definitions=>ty_items_tt,
          lx_error    TYPE REF TO zcx_abapgit_exception,
          lv_count    TYPE i.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.

    lt_tadir = it_tadir.

    IF iv_transport IS NOT INITIAL.
      zcl_abapgit_default_transport=>get_instance( )->set( iv_transport ).
    ENDIF.

    TRY.
        zcl_abapgit_dependencies=>resolve( CHANGING ct_tadir = lt_tadir ).

        li_progress = zcl_abapgit_progress=>get_instance( lines( lt_tadir ) ).

        lt_items = map_tadir_to_items( lt_tadir ).

        check_objects_locked( iv_language = zif_abapgit_definitions=>c_english
                              it_items    = lt_items ).

      CATCH zcx_abapgit_exception INTO lx_error.
        zcl_abapgit_default_transport=>get_instance( )->reset( ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    lv_count = 1.
    DO.
      CLEAR lt_deleted.
      LOOP AT lt_tadir ASSIGNING <ls_tadir>.
        li_progress->show( iv_current = lv_count
                           iv_text    = |Delete { <ls_tadir>-obj_name }| ).

        CLEAR ls_item.
        ls_item-obj_type = <ls_tadir>-object.
        ls_item-obj_name = <ls_tadir>-obj_name.

        TRY.
            delete_object(
              iv_package = <ls_tadir>-devclass
              is_item    = ls_item ).

            INSERT <ls_tadir> INTO TABLE lt_deleted.
            DELETE lt_tadir.
            lv_count = lv_count + 1.

            " make sure to save object deletions
            COMMIT WORK.
          CATCH zcx_abapgit_exception INTO lx_error.
            IF ii_log IS BOUND.
              ii_log->add_exception( ix_exc  = lx_error
                                     is_item = ls_item ).
              ii_log->add_error( iv_msg = |Deletion of object { ls_item-obj_name } failed|
                                 is_item = ls_item ).
            ENDIF.
        ENDTRY.

      ENDLOOP.

      " Exit if done or nothing else was deleted
      IF lines( lt_tadir ) = 0 OR lines( lt_deleted ) = 0.
        EXIT.
      ENDIF.
    ENDDO.

    zcl_abapgit_default_transport=>get_instance( )->reset( ).

    IF lx_error IS BOUND AND lines( lt_tadir ) > 0.
      zcx_abapgit_exception=>raise( 'Error during uninstall. Check the log.' ).
    ENDIF.

    li_progress->off( ).

  ENDMETHOD.


  METHOD delete_object.

    DATA: li_obj TYPE REF TO zif_abapgit_object.


    IF is_supported( is_item ) = abap_true.
      li_obj = create_object( is_item     = is_item
                              iv_language = zif_abapgit_definitions=>c_english ).

      li_obj->delete( iv_package ).

      IF li_obj->get_metadata( )-delete_tadir = abap_true.
        CALL FUNCTION 'TR_TADIR_INTERFACE'
          EXPORTING
            wi_delete_tadir_entry = abap_true
            wi_tadir_pgmid        = 'R3TR'
            wi_tadir_object       = is_item-obj_type
            wi_tadir_obj_name     = is_item-obj_name
            wi_test_modus         = abap_false
          EXCEPTIONS
            OTHERS                = 1 ##FM_SUBRC_OK.

        " We deliberately ignore the subrc, because throwing an exception would
        " break the deletion of lots of object types. On the other hand we have
        " to catch the exceptions because otherwise messages would directly be issued
        " by the function module and change the control flow. Thus breaking the
        " deletion of TOBJ and other object types.
        " TODO: This is not very clean and has to be improved in the future. See PR 2741.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD deserialize.

    DATA:
      ls_item         TYPE zif_abapgit_definitions=>ty_item,
      li_obj          TYPE REF TO zif_abapgit_object,
      lv_package      TYPE devclass,
      lo_files        TYPE REF TO zcl_abapgit_objects_files,
      lo_xml          TYPE REF TO zif_abapgit_xml_input,
      lt_results      TYPE zif_abapgit_definitions=>ty_results_tt,
      li_progress     TYPE REF TO zif_abapgit_progress,
      lv_path         TYPE string,
      lt_items        TYPE zif_abapgit_definitions=>ty_items_tt,
      lt_steps_id     TYPE zif_abapgit_definitions=>ty_deserialization_step_tt,
      lt_steps        TYPE zif_abapgit_objects=>ty_step_data_tt,
      lx_exc          TYPE REF TO zcx_abapgit_exception,
      lo_folder_logic TYPE REF TO zcl_abapgit_folder_logic.

    FIELD-SYMBOLS:
      <ls_result>  TYPE zif_abapgit_definitions=>ty_result,
      <lv_step_id> TYPE LINE OF zif_abapgit_definitions=>ty_deserialization_step_tt,
      <ls_step>    TYPE LINE OF zif_abapgit_objects=>ty_step_data_tt,
      <ls_deser>   TYPE LINE OF zif_abapgit_objects=>ty_deserialization_tt.

    lt_steps = get_deserialize_steps( ).

    IF iv_transport IS NOT INITIAL.
      zcl_abapgit_default_transport=>get_instance( )->set( iv_transport ).
    ENDIF.

    zcl_abapgit_objects_activation=>clear( ).

    lt_results = files_to_deserialize(
      iv_package         = iv_package
      it_local           = it_local
      it_local_checksums = it_local_checksums
      it_remote          = it_remote
      io_dot             = io_dot
      ii_log             = ii_log ).

    li_progress = zcl_abapgit_progress=>get_instance( lines( lt_results ) ).

    lt_items = map_results_to_items( lt_results ).

    check_objects_locked( iv_language = iv_language
                          it_items    = lt_items ).

    lo_folder_logic = zcl_abapgit_folder_logic=>get_instance( ).

    LOOP AT lt_results ASSIGNING <ls_result>.
      li_progress->show( iv_current = sy-tabix
                         iv_text    = |Deserialize { <ls_result>-obj_name }| ).

      CLEAR ls_item.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.

      "error handling & logging added
      TRY.
          IF ls_item-obj_type <> 'NSPC'.
            " If it does not exist yet, it will be created with this call
            lv_package = lo_folder_logic->path_to_package(
              iv_top  = iv_package
              io_dot  = io_dot
              iv_path = <ls_result>-path ).

            check_main_package(
              iv_package  = lv_package
              iv_obj_type = ls_item-obj_type ).
          ENDIF.

          IF ls_item-obj_type = 'DEVC'.
            " Packages have the same filename across different folders. The path needs to be supplied
            " to find the correct file.
            lv_path = <ls_result>-path.
          ENDIF.

          CREATE OBJECT lo_files
            EXPORTING
              is_item = ls_item
              iv_path = lv_path.
          lo_files->set_files( it_remote ).

          "analyze XML in order to instantiate the proper serializer
          lo_xml = lo_files->read_xml( ).

          li_obj = create_object( is_item     = ls_item
                                  iv_language = iv_language
                                  is_metadata = lo_xml->get_metadata( ) ).

          compare_remote_to_local(
            ii_object = li_obj
            it_remote = it_remote
            is_result = <ls_result>
            ii_log    = ii_log ).

          li_obj->mo_files = lo_files.

          "get required steps for deserialize the object
          lt_steps_id = li_obj->get_deserialize_steps( ).

          LOOP AT lt_steps_id ASSIGNING <lv_step_id>.
            READ TABLE lt_steps WITH KEY step_id = <lv_step_id> ASSIGNING <ls_step>.
            ASSERT sy-subrc = 0.
            IF <ls_step>-is_ddic = abap_true AND li_obj->get_metadata( )-ddic = abap_false.
              " DDIC only for DDIC objects
              zcx_abapgit_exception=>raise( |Step { <lv_step_id> } is only for DDIC objects| ).
            ENDIF.
            APPEND INITIAL LINE TO <ls_step>-objects ASSIGNING <ls_deser>.
            <ls_deser>-item    = ls_item.
            <ls_deser>-obj     = li_obj.
            <ls_deser>-xml     = lo_xml.
            <ls_deser>-package = lv_package.
          ENDLOOP.

          CLEAR: lv_path, lv_package.

        CATCH zcx_abapgit_exception INTO lx_exc.
          ii_log->add_exception( ix_exc = lx_exc
                                 is_item = ls_item ).
          ii_log->add_error( iv_msg = |Import of object { ls_item-obj_name } failed|
                             is_item = ls_item ).
          "object should not be part of any deserialization step
          CONTINUE.
      ENDTRY.

    ENDLOOP.

    "run deserialize for all steps and it's objects
    SORT lt_steps BY order.
    LOOP AT lt_steps ASSIGNING <ls_step>.
      deserialize_objects( EXPORTING is_step = <ls_step>
                                     ii_log  = ii_log
                           CHANGING ct_files = rt_accessed_files ).
    ENDLOOP.

    update_package_tree( iv_package ).

    SORT rt_accessed_files BY path ASCENDING filename ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_accessed_files. " Just in case

    zcl_abapgit_default_transport=>get_instance( )->reset( ).

    li_progress->off( ).

  ENDMETHOD.


  METHOD deserialize_objects.

    DATA: li_progress TYPE REF TO zif_abapgit_progress,
          lx_exc      TYPE REF TO zcx_abapgit_exception.

    FIELD-SYMBOLS: <ls_obj> LIKE LINE OF is_step-objects.


    zcl_abapgit_objects_activation=>clear( ).

    li_progress = zcl_abapgit_progress=>get_instance( lines( is_step-objects ) ).

    LOOP AT is_step-objects ASSIGNING <ls_obj>.
      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |Deserialize { is_step-descr } - { <ls_obj>-item-obj_name }| ).

      TRY.
          <ls_obj>-obj->deserialize( iv_package = <ls_obj>-package
                                     io_xml     = <ls_obj>-xml
                                     iv_step    = is_step-step_id
                                     ii_log     = ii_log ).
          APPEND LINES OF <ls_obj>-obj->mo_files->get_accessed_files( ) TO ct_files.

          ii_log->add_success( iv_msg = |Object { <ls_obj>-item-obj_name } imported|
                               is_item = <ls_obj>-item ).

        CATCH zcx_abapgit_exception INTO lx_exc.
          ii_log->add_exception( ix_exc = lx_exc
                                 is_item = <ls_obj>-item ).
          ii_log->add_error( iv_msg = |Import of object { <ls_obj>-item-obj_name } failed|
                             is_item = <ls_obj>-item ).
      ENDTRY.

    ENDLOOP.

    CASE is_step-step_id.
      WHEN zif_abapgit_object=>gc_step_id-ddic.
        zcl_abapgit_objects_activation=>activate( is_step-is_ddic ).
      WHEN zif_abapgit_object=>gc_step_id-abap.
        zcl_abapgit_objects_activation=>activate( is_step-is_ddic ).
      WHEN zif_abapgit_object=>gc_step_id-late.
        " late can have both DDIC (like TABL with REF TO) and non-DDIC objects
        zcl_abapgit_objects_activation=>activate( abap_true ).
        zcl_abapgit_objects_activation=>activate( abap_false ).
    ENDCASE.

  ENDMETHOD.


  METHOD exists.

    DATA: li_obj TYPE REF TO zif_abapgit_object.


    TRY.
        li_obj = create_object( is_item = is_item
                                iv_language = zif_abapgit_definitions=>c_english ).
        rv_bool = li_obj->exists( ).
      CATCH zcx_abapgit_exception.
* ignore all errors and assume the object exists
        rv_bool = abap_true.
    ENDTRY.

  ENDMETHOD.


  METHOD files_to_deserialize.

    DATA lt_results TYPE zif_abapgit_definitions=>ty_results_tt.

    lt_results = zcl_abapinst_file_status=>status(
      iv_package         = iv_package
      it_local           = it_local
      it_local_checksums = it_local_checksums
      it_remote          = it_remote
      io_dot             = io_dot
      ii_log             = ii_log ).

    rt_results = adjust_namespaces(
                   prioritize_deser(
                     filter_files_to_deserialize(
                       it_results = lt_results
                       ii_log     = ii_log ) ) ).

  ENDMETHOD.


  METHOD filter_files_to_deserialize.

    DATA lt_objects LIKE rt_results.
    DATA lr_object  TYPE REF TO zif_abapgit_definitions=>ty_result.
    DATA ls_item    TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_tabix   TYPE sy-tabix.

    rt_results = it_results.

    "preparation for object logging, sort all file entries by objects
    IF ii_log IS BOUND.
      lt_objects = rt_results.
      SORT lt_objects
        BY obj_type
           obj_name.
      DELETE ADJACENT DUPLICATES FROM lt_objects COMPARING obj_type obj_name.
      DELETE lt_objects WHERE obj_type IS INITIAL AND obj_name IS INITIAL.
    ENDIF.

    "ignore objects w/o changes
    DELETE rt_results WHERE match = abap_true.     " Full match
    "log objects w/o changes
    IF sy-subrc = 0 AND ii_log IS BOUND.
      SORT rt_results BY obj_type obj_name.
      LOOP AT lt_objects REFERENCE INTO lr_object.
        lv_tabix = sy-tabix.
        READ TABLE rt_results WITH KEY obj_type = lr_object->obj_type
                                       obj_name = lr_object->obj_name
                              BINARY SEARCH TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          "all parts of the objects have not changed
          ls_item-devclass = lr_object->package.
          ls_item-obj_type = lr_object->obj_type.
          ls_item-obj_name = lr_object->obj_name.
          ii_log->add_success(
            iv_msg  = |Object { ls_item-obj_name } (type { ls_item-obj_type }) not changed; no import required|
            is_item = ls_item ).
          "ignore object for further messages
          DELETE lt_objects INDEX lv_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "ignore objects w/o object type
    DELETE rt_results WHERE obj_type IS INITIAL.
    "log objects w/o object type
    IF sy-subrc = 0 AND ii_log IS BOUND.
      LOOP AT lt_objects REFERENCE INTO lr_object WHERE obj_type IS INITIAL.
        CHECK lr_object->obj_name IS NOT INITIAL.
        ls_item-devclass = lr_object->package.
        ls_item-obj_type = lr_object->obj_type.
        ls_item-obj_name = lr_object->obj_name.
        ii_log->add_warning(
          iv_msg  = |Object type for { ls_item-obj_name } not defined - will be ignored by abapGit|
          is_item = ls_item ).
      ENDLOOP.
      DELETE lt_objects WHERE obj_type IS INITIAL.
    ENDIF.

    "ignore objects that exists only local
    DELETE rt_results WHERE lstate = zif_abapgit_definitions=>c_state-added AND rstate IS INITIAL.
    "log objects that exists only local
    IF sy-subrc = 0 AND ii_log IS BOUND.
      SORT rt_results BY obj_type obj_name.
      LOOP AT lt_objects REFERENCE INTO lr_object.
        lv_tabix = sy-tabix.
        READ TABLE rt_results WITH KEY obj_type = lr_object->obj_type
                                       obj_name = lr_object->obj_name
                              BINARY SEARCH TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          "all parts exists only local
          "no log message; ignore object for further messages
          DELETE lt_objects INDEX lv_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

*** abapinst (not merged yet)
*    "ignore objects that where deleted remotely
*    "but not class includes with separate file which cleaned up during deserialize
*    LOOP AT rt_results REFERENCE INTO lr_object WHERE rstate = zif_abapgit_definitions=>c_state-deleted
*      lv_tabix = sy-tabix
*      IF zcl_abapgit_oo_base=>is_part_of_class( lr_object->filename ) = abap_false
*        DELETE rt_results INDEX lv_tabix
*      ENDIF
*    ENDLOOP

    SORT rt_results
      BY obj_type ASCENDING
         obj_name ASCENDING
         rstate   DESCENDING  " ensures that non-empty rstate is kept
         lstate   DESCENDING. " ensures that non-empty lstate is kept
    DELETE ADJACENT DUPLICATES FROM rt_results COMPARING obj_type obj_name.

  ENDMETHOD.


  METHOD get_deserialize_steps.
    FIELD-SYMBOLS: <ls_step>    TYPE LINE OF zif_abapgit_objects=>ty_step_data_tt.


    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = zif_abapgit_object=>gc_step_id-ddic.
    <ls_step>-descr        = 'Import DDIC objects'.
    <ls_step>-is_ddic      = abap_true.
    <ls_step>-syntax_check = abap_false.
    <ls_step>-order        = 1.

    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = zif_abapgit_object=>gc_step_id-abap.
    <ls_step>-descr        = 'Import objects main'.
    <ls_step>-is_ddic      = abap_false.
    <ls_step>-syntax_check = abap_false.
    <ls_step>-order        = 2.

    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = zif_abapgit_object=>gc_step_id-late.
    <ls_step>-descr        = 'Import late objects'.
    <ls_step>-is_ddic      = abap_false.
    <ls_step>-syntax_check = abap_true.
    <ls_step>-order        = 3.
  ENDMETHOD.


  METHOD is_active.

    DATA: li_object TYPE REF TO zif_abapgit_object.

    TRY.
        li_object = create_object( is_item     = is_item
                                   iv_language = sy-langu ).

        rv_active = li_object->is_active( ).
      CATCH cx_sy_dyn_call_illegal_method
            cx_sy_ref_is_initial
            zcx_abapgit_exception.
        rv_active = abap_true.
    ENDTRY.
  ENDMETHOD.


  METHOD is_supported.

    TRY.
        create_object( is_item        = is_item
                       iv_language    = zif_abapgit_definitions=>c_english
                       iv_native_only = iv_native_only ).
        rv_bool = abap_true.
      CATCH zcx_abapgit_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD map_results_to_items.

    DATA: ls_item LIKE LINE OF rt_items.
    FIELD-SYMBOLS: <ls_result> TYPE zif_abapgit_definitions=>ty_result.

    LOOP AT it_results ASSIGNING <ls_result>.

      ls_item-devclass = <ls_result>-package.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.
      INSERT ls_item INTO TABLE rt_items.

    ENDLOOP.

  ENDMETHOD.


  METHOD map_tadir_to_items.

    DATA: ls_item LIKE LINE OF rt_items.
    FIELD-SYMBOLS: <ls_tadir> TYPE zif_abapgit_definitions=>ty_tadir.

    LOOP AT it_tadir ASSIGNING <ls_tadir>.

      ls_item-devclass = <ls_tadir>-devclass.
      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.
      INSERT ls_item INTO TABLE rt_items.

    ENDLOOP.

  ENDMETHOD.


  METHOD prioritize_deser.

* todo, refactor this method

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF it_results.

* WEBI has to be handled before SPRX.
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'WEBI'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* SPRX has to be handled before depended objects CLAS/INFT/TABL etc.
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'SPRX'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* XSLT has to be handled before CLAS/PROG
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'XSLT'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* PROG before internet services, as the services might use the screens
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'PROG'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* ISAP has to be handled before ISRP
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'IASP'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* ENHS has to be handled before ENHO
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'ENHS'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* DDLS has to be handled before DCLS
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'DDLS'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* IOBJ has to be handled before ODSO
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'IOBJ'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* TOBJ has to be handled before SCP1
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'TOBJ'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* OTGR has to be handled before CHAR
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'OTGR'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

    LOOP AT it_results ASSIGNING <ls_result>
        WHERE obj_type <> 'IASP'
        AND obj_type <> 'PROG'
        AND obj_type <> 'XSLT'
        AND obj_type <> 'PINF'
        AND obj_type <> 'DEVC'
        AND obj_type <> 'ENHS'
        AND obj_type <> 'DDLS'
        AND obj_type <> 'SPRX'
        AND obj_type <> 'WEBI'
        AND obj_type <> 'IOBJ'
        AND obj_type <> 'TOBJ'
        AND obj_type <> 'OTGR'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* PINF after everything as it can expose objects
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'PINF'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* DEVC after PINF, as it can refer for package interface usage
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'DEVC'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

  ENDMETHOD.


  METHOD supported_list.

    DATA: lt_objects   TYPE STANDARD TABLE OF ko100,
          ls_item      TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF lt_objects.

    IF gt_supported_obj_types IS NOT INITIAL.
      rt_types = gt_supported_obj_types.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = lt_objects
      EXCEPTIONS
        OTHERS         = 1 ##FM_SUBRC_OK.

    LOOP AT lt_objects ASSIGNING <ls_object> WHERE pgmid = 'R3TR'.
      ls_item-obj_type = <ls_object>-object.

      IF is_supported( ls_item ) = abap_true.
        INSERT <ls_object>-object INTO TABLE rt_types.
      ENDIF.
    ENDLOOP.
    gt_supported_obj_types = rt_types.

  ENDMETHOD.


  METHOD update_package_tree.

    DATA: lt_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt,
          lv_package  LIKE LINE OF lt_packages,
          lv_tree     TYPE dirtree-tname.

    " Make sure all deserialized objects are committed
    COMMIT WORK AND WAIT.

    lt_packages = zcl_abapinst_factory=>get_sap_package( iv_package )->list_subpackages( ).
    APPEND iv_package TO lt_packages.

    LOOP AT lt_packages INTO lv_package.
      " Update package tree for SE80
      lv_tree = 'EU_' && lv_package.
      CALL FUNCTION 'WB_TREE_ACTUALIZE'
        EXPORTING
          tree_name              = lv_tree
          without_crossreference = abap_true
          with_tcode_index       = abap_true.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapinst_persistence IMPLEMENTATION.


  METHOD constructor.
    mv_tabname = iv_tabname.
    mv_lock    = iv_lock.
  ENDMETHOD.


  METHOD delete.

    _lock( iv_name = iv_name
           iv_pack = iv_pack ).

    DELETE FROM (mv_tabname)
      WHERE name = iv_name AND pack = iv_pack.
    IF sy-subrc <> 0.
      zcx_abapinst_exception=>raise( 'DB delete failed' ).
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD get_instance.

    IF go_db IS NOT BOUND.
      CREATE OBJECT go_db
        EXPORTING
          iv_tabname = iv_tabname
          iv_lock    = iv_lock.
    ENDIF.
    ro_db = go_db.

  ENDMETHOD.


  METHOD insert.

    DATA ls_content TYPE zif_abapinst_definitions=>ty_content.

    _lock( iv_name = is_inst-name
           iv_pack = is_inst-pack ).

    ls_content = _list_to_content( is_inst ).

    INSERT (mv_tabname) FROM ls_content.
    IF sy-subrc <> 0.
      zcx_abapinst_exception=>raise( 'DB insert failed' ).
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD list.

    DATA lt_content TYPE zif_abapinst_definitions=>ty_contents.

    SELECT * FROM (mv_tabname) INTO TABLE lt_content
      ORDER BY PRIMARY KEY.
    IF sy-subrc = 0.
      rt_list = _content_to_list( lt_content ).
    ENDIF.

  ENDMETHOD.


  METHOD list_by_name.

    DATA lt_content TYPE zif_abapinst_definitions=>ty_contents.

    IF iv_pack IS SUPPLIED.
      SELECT * FROM (mv_tabname) INTO TABLE lt_content
        WHERE name = iv_name AND pack = iv_pack
        ORDER BY PRIMARY KEY.
    ELSE.
      SELECT * FROM (mv_tabname) INTO TABLE lt_content
        WHERE name = iv_name
        ORDER BY PRIMARY KEY.
    ENDIF.

    IF sy-subrc = 0.
      rt_list = _content_to_list( lt_content ).
    ENDIF.

  ENDMETHOD.


  METHOD select.

    DATA ls_content TYPE zif_abapinst_definitions=>ty_content.

    IF iv_name IS SUPPLIED AND iv_pack IS SUPPLIED.
      SELECT SINGLE * FROM (mv_tabname) INTO ls_content
        WHERE name = iv_name AND pack = iv_pack.
    ELSEIF iv_name IS SUPPLIED.
      SELECT SINGLE * FROM (mv_tabname) INTO ls_content
        WHERE name = iv_name.
    ELSE.
      SELECT SINGLE * FROM (mv_tabname) INTO ls_content
        WHERE pack = iv_pack.
    ENDIF.
    IF sy-subrc = 0.
      rs_inst = _content_to_inst( ls_content ).
    ENDIF.

  ENDMETHOD.


  METHOD update.

    DATA ls_content TYPE zif_abapinst_definitions=>ty_content.

    _lock( iv_name = is_inst-name
           iv_pack = is_inst-pack ).

    ls_content = _list_to_content( is_inst ).

    MODIFY (mv_tabname) FROM ls_content.
    IF sy-subrc <> 0.
      zcx_abapinst_exception=>raise( 'DB modify failed' ).
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD _content_to_inst.

    DATA:
      li_ajson TYPE REF TO zif_ajson_reader.

    TRY.
        li_ajson = zcl_ajson=>parse( is_content-json ).
        li_ajson->to_abap( IMPORTING ev_container = rs_inst ).
      CATCH zcx_ajson_error.
        zcx_abapinst_exception=>raise( 'Error converting JSON persistence' ).
    ENDTRY.

    " Validate name and package
    IF rs_inst-name <> is_content-name OR rs_inst-pack <> is_content-pack.
      zcx_abapinst_exception=>raise( 'Inconsistent JSON persistence' ).
    ENDIF.

  ENDMETHOD.


  METHOD _content_to_list.

    DATA ls_list LIKE LINE OF rt_list.

    FIELD-SYMBOLS <ls_content> LIKE LINE OF it_content.

    LOOP AT it_content ASSIGNING <ls_content>.
      ls_list = _content_to_inst( <ls_content> ).
      APPEND ls_list TO rt_list.
    ENDLOOP.

  ENDMETHOD.


  METHOD _list_to_content.

    DATA lo_json TYPE REF TO zcl_ajson.

    TRY.
        lo_json = zcl_ajson=>create_empty( ).
        lo_json->set( iv_path = '/'
                      iv_val  = is_inst ).

        rs_content-name = is_inst-name.
        rs_content-pack = is_inst-pack.
        rs_content-json = lo_json->stringify( 2 ).
      CATCH zcx_ajson_error.
        zcx_abapinst_exception=>raise( 'Error converting JSON persistency' ).
    ENDTRY.

  ENDMETHOD.


  METHOD _lock.

    DATA:
      lv_lock_function         TYPE funcname,
      lv_dummy_update_function TYPE funcname.

    lv_lock_function = 'ENQUEUE_' && mv_lock.

    CALL FUNCTION lv_lock_function
      EXPORTING
        mode_zabapinst = iv_mode
        name           = iv_name
        pack           = iv_pack
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      zcx_abapinst_exception=>raise( |Could not aquire lock for { iv_name } { iv_pack }| ).
    ENDIF.

    lv_dummy_update_function = _update_function( ).

    " Trigger dummy update task to automatically release locks at commit
    CALL FUNCTION lv_dummy_update_function IN UPDATE TASK.

  ENDMETHOD.


  METHOD _update_function.

    IF mv_update_function IS INITIAL.
      mv_update_function = 'CALL_V1_PING'.

      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname = mv_update_function
        EXCEPTIONS
          OTHERS   = 2.

      IF sy-subrc <> 0.
        mv_update_function = 'BANK_OBJ_WORKL_RELEASE_LOCKS'.
      ENDIF.
    ENDIF.

    rv_funcname = mv_update_function.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapinst_screen IMPLEMENTATION.


  METHOD banner.

    DATA:
      lv_base           TYPE string,
      lt_base           TYPE TABLE OF string ##NEEDED,
      lv_xstr           TYPE xstring,
      lv_content_type   TYPE w3param-cont_type,
      lv_content_lenght TYPE w3param-cont_len,
      ls_pic            TYPE w3mime,
      lt_pic            TYPE TABLE OF w3mime,
      ls_query          TYPE w3query,
      lt_query          TYPE TABLE OF w3query,
      lt_html           TYPE TABLE OF w3html,
      lv_return_code    TYPE w3param-ret_code.

    IF go_banner IS BOUND AND iv_show IS INITIAL.
      go_banner->clear_picture( ).
      go_banner->free( ).
      FREE go_banner.
      RETURN.
    ENDIF.

    IF go_banner IS NOT BOUND.
      CREATE OBJECT go_banner EXPORTING parent = go_banner_dock.

      go_banner->set_3d_border( border = 0 ).

      go_banner->set_display_mode( display_mode = cl_gui_picture=>display_mode_normal ).
    ENDIF.

    go_banner->set_position( height = 21
                             left   = iv_left
                             top    = iv_top
                             width  = 500 ).             "#EC NUMBER_OK

    IF gv_banner_url IS INITIAL.
      IF iv_id IS INITIAL.
        lv_base = concat_lines_of( it_base ).
        lv_xstr = cl_http_utility=>decode_x_base64( lv_base ).
        WHILE xstrlen( lv_xstr ) > 255.
          ls_pic-line = lv_xstr(255).
          APPEND ls_pic TO lt_pic.
          lv_xstr = lv_xstr+255(*).
        ENDWHILE.
        IF xstrlen( lv_xstr ) > 0.
          ls_pic-line = lv_xstr(255).
          APPEND ls_pic TO lt_pic.
        ENDIF.
      ELSE.
        ls_query-name  = '_OBJECT_ID'.
        ls_query-value = iv_id.
        APPEND ls_query TO lt_query.

        CALL FUNCTION 'WWW_GET_MIME_OBJECT'
          TABLES
            query_string        = lt_query
            html                = lt_html
            mime                = lt_pic
          CHANGING
            return_code         = lv_return_code
            content_type        = lv_content_type
            content_length      = lv_content_lenght
          EXCEPTIONS
            object_not_found    = 1
            parameter_not_found = 2
            OTHERS              = 3 ##FM_OLDED.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        " Convert to base64 (not used, just fyi in case you want to paste it somewhere)
        LOOP AT lt_pic INTO ls_pic.
          lv_xstr = lv_xstr && ls_pic-line.
        ENDLOOP.
        lv_base = cl_http_utility=>encode_x_base64( lv_xstr ).
        WHILE strlen( lv_base ) > 80.
          APPEND lv_base(80) TO lt_base.
          SHIFT lv_base LEFT BY 80 PLACES.
        ENDWHILE.
        IF strlen( lv_base ) > 0.
          APPEND lv_base TO lt_base.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'DP_CREATE_URL'
        EXPORTING
          type     = 'IMAGE'
          subtype  = cndp_sap_subtype_unknown
          size     = lv_content_lenght
          lifetime = cndp_lifetime_transaction
        TABLES
          data     = lt_pic
        CHANGING
          url      = gv_banner_url
        EXCEPTIONS
          OTHERS   = 1 ##FM_SUBRC_OK.
    ENDIF.

    go_banner->load_picture_from_url( url = gv_banner_url ).

  ENDMETHOD.


  METHOD browser.
    cl_gui_frontend_services=>execute(
      EXPORTING
        document               = |{ iv_url }|
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.


  METHOD f4_file.

    DATA:
      lt_file_table TYPE filetable,
      ls_file_table LIKE LINE OF lt_file_table,
      lv_filter     TYPE string,
      lv_action     TYPE i,
      lv_rc         TYPE i.

    lv_filter = 'ZIP Files (*.ZIP)|*.ZIP|' && cl_gui_frontend_services=>filetype_all.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = 'abapGit Package'
        default_filename        = '*.zip'
        file_filter             = lv_filter
      CHANGING
        file_table              = lt_file_table
        rc                      = lv_rc
        user_action             = lv_action
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF lv_action = cl_gui_frontend_services=>action_cancel.
      RETURN.
    ENDIF.

    READ TABLE lt_file_table INDEX 1 INTO ls_file_table.
    ASSERT sy-subrc = 0.

    rv_file = ls_file_table-filename.

  ENDMETHOD.


  METHOD f4_transport.

    DATA:
      lv_obj_name   TYPE trobj_name,
      lv_order_type TYPE trfunction,
      lv_task_type  TYPE trfunction,
      lv_tarsystem  TYPE tr_target,
      lt_e071       TYPE TABLE OF e071,
      lt_e071k      TYPE TABLE OF e071k.

    lv_obj_name = iv_package.

    CALL FUNCTION 'TRINT_GET_REQUEST_TYPE'
      EXPORTING
        iv_pgmid                   = 'R3TR'
        iv_object                  = 'DEVC'
        iv_obj_name                = lv_obj_name
      IMPORTING
        ev_request_type            = lv_order_type
        ev_task_type               = lv_task_type
        ev_target                  = lv_tarsystem
      EXCEPTIONS
        internal_error             = 1
        no_request_needed          = 2
        cts_initialization_failure = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      lv_order_type = 'K'.
      lv_task_type  = 'S'.

      IF iv_layer IS INITIAL.
        CALL FUNCTION 'TR_GET_TRANSPORT_TARGET'
          EXPORTING
            iv_use_default             = abap_true
          IMPORTING
            ev_target                  = lv_tarsystem
          EXCEPTIONS
            wrong_call                 = 1
            invalid_input              = 2
            cts_initialization_failure = 3
            OTHERS                     = 4.
      ELSE.
        CALL FUNCTION 'TR_GET_TRANSPORT_TARGET'
          EXPORTING
            iv_transport_layer         = iv_layer
          IMPORTING
            ev_target                  = lv_tarsystem
          EXCEPTIONS
            wrong_call                 = 1
            invalid_input              = 2
            cts_initialization_failure = 3
            OTHERS                     = 4.
      ENDIF.
      IF sy-subrc <> 0.
        RETURN. " Ignore
      ENDIF.
    ENDIF.

    CALL FUNCTION 'TRINT_ORDER_CHOICE'
      EXPORTING
        wi_order_type          = lv_order_type
        wi_task_type           = lv_task_type
        wi_client              = sy-mandt
        wi_order               = iv_transport
        iv_tarsystem           = lv_tarsystem
      IMPORTING
        we_order               = rv_transport
      TABLES
        wt_e071                = lt_e071
        wt_e071k               = lt_e071k
      EXCEPTIONS
        no_correction_selected = 1
        display_mode           = 2
        object_append_error    = 3
        recursive_call         = 4
        wrong_order_type       = 5
        OTHERS                 = 6.
    IF sy-subrc <> 0.
      RETURN. " Ignore
    ENDIF.

  ENDMETHOD.


  METHOD header.
    WRITE iv_icon AS ICON TO rv_header.
    rv_header+6 = iv_text.
  ENDMETHOD.


  METHOD icon.

    DATA lv_info TYPE string.

    IF iv_info IS INITIAL.
      lv_info = iv_text.
    ELSE.
      lv_info = iv_info.
    ENDIF.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name   = iv_name
        text   = iv_text
        info   = lv_info
      IMPORTING
        result = rv_icon.

  ENDMETHOD.


  METHOD modify.

    DATA:
      lv_show     TYPE abap_bool,
      lv_input    TYPE abap_bool,
      lv_password TYPE abap_bool.

    LOOP AT SCREEN.
      lv_show = abap_true.
      lv_input = abap_true.
      lv_password = abap_false.

      IF screen-name = 'P_FILE_F'.
        screen-length = 60.
      ENDIF.

      CASE screen-group1.
        WHEN 'T02'. " SAP Package
          lv_show = boolc( iv_options = abap_true AND iv_mbt = abap_false ).
        WHEN 'T03'. " Transport
          lv_show = boolc( iv_options = abap_true AND iv_mbt = abap_false ).
        WHEN 'T04'. " Authentication
          lv_show = boolc( iv_zip_i = abap_true ).
        WHEN 'T05'. " Uninstall (only for SUBMIT)
          lv_show = abap_false.
        WHEN 'T08'. " Options
          lv_show = boolc( iv_options = abap_true AND iv_mbt = abap_false ).
        WHEN 'C11'.
          lv_input = boolc( iv_zip_i = abap_true ).
        WHEN 'C12'.
          lv_input = boolc( iv_zip_f = abap_true ).
        WHEN 'C13'.
          lv_input = boolc( iv_zip_s = abap_true ).
        WHEN 'C21'.
          lv_input = boolc( iv_sap_l = abap_true ).
        WHEN 'C22'.
          lv_input = boolc( iv_sap_t = abap_true ).
        WHEN 'C31'.
          lv_input = boolc( iv_tsp_e = abap_true ).
        WHEN 'C40'.
          lv_input = boolc( iv_conn_o = abap_true ).
        WHEN 'C41'.
          lv_input = boolc( iv_prox_o = abap_true ).
        WHEN 'P40'.
          lv_input = boolc( iv_conn_o = abap_true ).
          lv_password = abap_true.
        WHEN 'P41'.
          lv_input = boolc( iv_prox_o = abap_true ).
          lv_password = abap_true.
      ENDCASE.

      IF lv_show = abap_true.
        screen-active    = '1'.
        screen-invisible = '0'.
      ELSE.
        screen-active    = '0'.
        screen-invisible = '1'.
      ENDIF.

      IF lv_input = abap_true.
        screen-input = '1'.
      ELSE.
        screen-input = '0'.
      ENDIF.

      IF lv_password = abap_true.
        screen-invisible = '1'.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapinst_setup IMPLEMENTATION.


  METHOD run.

    gv_tabname = iv_tabname.
    gv_lock    = iv_lock.
    gv_text    = iv_text.

    IF _table_exists( ) = abap_false.
      _table_create( ).
    ENDIF.

    IF _lock_exists( ) = abap_false.
      _lock_create( ).
    ENDIF.

  ENDMETHOD.


  METHOD _get_package.

    IF sy-cprog CA '/'.
      " Fallback for namespaced installer
      rv_package = '$TMP'.
    ELSE.
      " Get package of main program
      SELECT SINGLE devclass FROM tadir INTO rv_package
        WHERE pgmid = 'R3TR' AND object = 'PROG' AND obj_name = sy-cprog.
      IF sy-subrc <> 0.
        rv_package = '$TMP'. " Fallback
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD _lock_create.

    DATA:
      lv_name  TYPE tadir-obj_name,
      lv_pack  TYPE devclass,
      ls_dd25v TYPE dd25v,
      lt_dd26e TYPE STANDARD TABLE OF dd26e WITH DEFAULT KEY,
      lt_dd27p TYPE STANDARD TABLE OF dd27p WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_dd26e> LIKE LINE OF lt_dd26e,
      <ls_dd27p> LIKE LINE OF lt_dd27p.

    ls_dd25v-viewname   = gv_lock.
    ls_dd25v-aggtype    = 'E'.
    ls_dd25v-roottab    = gv_tabname.
    ls_dd25v-ddlanguage = zif_abapinst_definitions=>c_english.
    ls_dd25v-ddtext     = gv_text && ' - Lock'.

    APPEND INITIAL LINE TO lt_dd26e ASSIGNING <ls_dd26e>.
    <ls_dd26e>-viewname   = ls_dd25v-viewname.
    <ls_dd26e>-tabname    = ls_dd25v-roottab.
    <ls_dd26e>-tabpos     = '0001'.
    <ls_dd26e>-fortabname = ls_dd25v-roottab.
    <ls_dd26e>-enqmode    = 'E'.

    APPEND INITIAL LINE TO lt_dd27p ASSIGNING <ls_dd27p>.
    <ls_dd27p>-viewname  = ls_dd25v-viewname.
    <ls_dd27p>-objpos    = '0001'.
    <ls_dd27p>-viewfield = 'NAME'.
    <ls_dd27p>-tabname   = ls_dd25v-roottab.
    <ls_dd27p>-fieldname = 'NAME'.
    <ls_dd27p>-keyflag   = abap_true.

    APPEND INITIAL LINE TO lt_dd27p ASSIGNING <ls_dd27p>.
    <ls_dd27p>-viewname  = ls_dd25v-viewname.
    <ls_dd27p>-objpos    = '0002'.
    <ls_dd27p>-viewfield = 'PACK'.
    <ls_dd27p>-tabname   = ls_dd25v-roottab.
    <ls_dd27p>-fieldname = 'PACK'.
    <ls_dd27p>-keyflag   = abap_true.

    CALL FUNCTION 'DDIF_ENQU_PUT'
      EXPORTING
        name              = ls_dd25v-viewname
        dd25v_wa          = ls_dd25v
      TABLES
        dd26e_tab         = lt_dd26e
        dd27p_tab         = lt_dd27p
      EXCEPTIONS
        enqu_not_found    = 1
        name_inconsistent = 2
        enqu_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      zcx_abapinst_exception=>raise( |Setup failed: Creating enqueue { ls_dd25v-viewname }| ).
    ENDIF.

    lv_name = ls_dd25v-viewname.
    lv_pack = _get_package( ).

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = 'ENQU'
        wi_tadir_obj_name = lv_name
        wi_set_genflag    = abap_true
        wi_test_modus     = abap_false
        wi_tadir_devclass = lv_pack
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      zcx_abapinst_exception=>raise( |Setup failed: TADIR for { ls_dd25v-viewname }| ).
    ENDIF.

    CALL FUNCTION 'DDIF_ENQU_ACTIVATE'
      EXPORTING
        name        = ls_dd25v-viewname
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0.
      zcx_abapinst_exception=>raise( |Setup failed: Activating enqueue { ls_dd25v-viewname }| ).
    ENDIF.

  ENDMETHOD.


  METHOD _lock_exists.

    DATA: lv_viewname TYPE dd25l-viewname.

    SELECT SINGLE viewname FROM dd25l INTO lv_viewname WHERE viewname = gv_lock ##WARN_OK.
    rv_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD _table_create.

    DATA:
      lv_name  TYPE tadir-obj_name,
      lv_pack  TYPE devclass,
      lv_rc    TYPE sy-subrc,
      ls_dd02v TYPE dd02v,
      ls_dd09l TYPE dd09l,
      lt_dd03p TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_dd03p> LIKE LINE OF lt_dd03p.

    ls_dd02v-tabname    = gv_tabname.
    ls_dd02v-ddlanguage = zif_abapinst_definitions=>c_english.
    ls_dd02v-tabclass   = 'TRANSP'.
    ls_dd02v-ddtext     = gv_text && ' - Persistence'.
    ls_dd02v-contflag   = 'A'.
    ls_dd02v-exclass    = '1'.

    ls_dd09l-tabname  = ls_dd02v-tabname.
    ls_dd09l-as4local = 'A'.
    ls_dd09l-tabkat   = '1'.
    ls_dd09l-tabart   = 'APPL1'.
    ls_dd09l-bufallow = 'N'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = ls_dd02v-tabname.
    <ls_dd03p>-fieldname = 'NAME'.
    <ls_dd03p>-position  = '0001'.
    <ls_dd03p>-keyflag   = abap_true.
    <ls_dd03p>-notnull   = abap_true.
    <ls_dd03p>-datatype  = 'CHAR'.
    <ls_dd03p>-leng      = zif_abapinst_definitions=>c_name_length.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = ls_dd02v-tabname.
    <ls_dd03p>-fieldname = 'PACK'.
    <ls_dd03p>-position  = '0002'.
    <ls_dd03p>-keyflag   = abap_true.
    <ls_dd03p>-notnull   = abap_true.
    <ls_dd03p>-rollname  = 'DEVCLASS'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = ls_dd02v-tabname.
    <ls_dd03p>-fieldname = 'JSON'.
    <ls_dd03p>-position  = '0003'.
    <ls_dd03p>-datatype  = 'STRG'.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = ls_dd02v-tabname
        dd02v_wa          = ls_dd02v
        dd09l_wa          = ls_dd09l
      TABLES
        dd03p_tab         = lt_dd03p
      EXCEPTIONS
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      zcx_abapinst_exception=>raise( |Setup failed: Creating table { ls_dd02v-tabname }| ).
    ENDIF.

    lv_name = ls_dd02v-tabname.
    lv_pack = _get_package( ).

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = 'TABL'
        wi_tadir_obj_name = lv_name
        wi_set_genflag    = abap_true
        wi_test_modus     = abap_false
        wi_tadir_devclass = lv_pack
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      zcx_abapinst_exception=>raise( |Setup failed: TADIR for { ls_dd02v-tabname }| ).
    ENDIF.

    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name        = ls_dd02v-tabname
        auth_chk    = abap_false
      IMPORTING
        rc          = lv_rc
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0 OR lv_rc <> 0.
      zcx_abapinst_exception=>raise( |Setup failed: Activating table { ls_dd02v-tabname }| ).
    ENDIF.

  ENDMETHOD.


  METHOD _table_exists.

    DATA: lv_tabname TYPE dd02l-tabname.

    SELECT SINGLE tabname FROM dd02l INTO lv_tabname WHERE tabname = gv_tabname ##WARN_OK.
    rv_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapinst_textpool IMPLEMENTATION.


  METHOD constructor.
    mv_program = iv_program.
    _load( ).
  ENDMETHOD.


  METHOD save.
    IF mt_text_new <> mt_text_old.
      INSERT TEXTPOOL mv_program FROM mt_text_new LANGUAGE sy-langu.
    ENDIF.
  ENDMETHOD.


  METHOD set.

    DATA:
      ls_param TYPE textpool,
      ls_text  TYPE textpool.

    SPLIT iv_param AT ',' INTO ls_param-id ls_param-key ls_param-entry.
    ASSERT sy-subrc = 0.

    READ TABLE mt_text_new INTO ls_text WITH KEY id = ls_param-id key = ls_param-key.
    IF sy-subrc = 0.
      DELETE mt_text_new WHERE id = ls_param-id AND key = ls_param-key.
    ENDIF.

    CLEAR ls_text.
    ls_text-id = ls_param-id.
    ls_text-key = ls_param-key.
    IF ls_param-id = 'S'.
      ls_text-entry+8 = ls_param-entry.
    ELSE.
      ls_text-entry = ls_param-entry.
    ENDIF.
    ls_text-length = strlen( ls_text-entry ).
    APPEND ls_text TO mt_text_new.

  ENDMETHOD.


  METHOD _load.
    READ TEXTPOOL mv_program INTO mt_text_old LANGUAGE sy-langu.
    mt_text_new = mt_text_old.
  ENDMETHOD.
ENDCLASS.
**********************************************************************
* UTILS
**********************************************************************

class lcl_utils definition final.
  public section.

    class-methods normalize_path
      importing
        iv_path type string
      returning
        value(rv_path) type string.
    class-methods split_path
      importing
        iv_path type string
      returning
        value(rv_path_name) type zif_ajson=>ty_path_name.
    class-methods validate_array_index
      importing
        iv_path type string
        iv_index type string
      returning
        value(rv_index) type i
      raising
        zcx_ajson_error.

endclass.

class lcl_utils implementation.

  method validate_array_index.

    if not iv_index co '0123456789'.
      zcx_ajson_error=>raise( |Cannot add non-numeric key [{ iv_index }] to array [{ iv_path }]| ).
    endif.
    rv_index = iv_index.
    if rv_index = 0.
      zcx_ajson_error=>raise( |Cannot add zero key to array [{ iv_path }]| ).
    endif.

  endmethod.

  method normalize_path.

    rv_path = iv_path.
    if strlen( rv_path ) = 0.
      rv_path = '/'.
    endif.
    if rv_path+0(1) <> '/'.
      rv_path = '/' && rv_path.
    endif.
    if substring( val = rv_path off = strlen( rv_path ) - 1 ) <> '/'.
      rv_path = rv_path && '/'.
    endif.

  endmethod.

  method split_path.

    data lv_offs type i.
    data lv_len type i.
    data lv_trim_slash type i.

    lv_len = strlen( iv_path ).
    if lv_len = 0 or iv_path = '/'.
      return. " empty path is the alias for root item = '' + ''
    endif.

    if substring( val = iv_path off = lv_len - 1 ) = '/'.
      lv_trim_slash = 1. " ignore last '/'
    endif.

    lv_offs = find( val = reverse( iv_path ) sub = '/' off = lv_trim_slash ).
    if lv_offs = -1.
      lv_offs  = lv_len. " treat whole string as the 'name' part
    endif.
    lv_offs = lv_len - lv_offs.

    rv_path_name-path = normalize_path( substring( val = iv_path len = lv_offs ) ).
    rv_path_name-name = substring( val = iv_path off = lv_offs len = lv_len - lv_offs - lv_trim_slash ).

  endmethod.

endclass.


**********************************************************************
* PARSER
**********************************************************************

class lcl_json_parser definition final.
  public section.

    methods parse
      importing
        iv_json type string
      returning
        value(rt_json_tree) type zif_ajson=>ty_nodes_tt
      raising
        zcx_ajson_error.

  private section.

    types:
      ty_stack_tt type standard table of ref to zif_ajson=>ty_node.

    data mt_stack type ty_stack_tt.

    class-methods join_path
      importing
        it_stack type ty_stack_tt
      returning
        value(rv_path) type string.

    methods raise
      importing
        iv_error type string
      raising
        zcx_ajson_error.

    methods _parse
      importing
        iv_json type string
      returning
        value(rt_json_tree) type zif_ajson=>ty_nodes_tt
      raising
        zcx_ajson_error cx_sxml_error.

endclass.

class lcl_json_parser implementation.

  method parse.
    data lx_sxml type ref to cx_sxml_error.
    try.
      rt_json_tree = _parse( iv_json ).
    catch cx_sxml_error into lx_sxml.
      zcx_ajson_error=>raise( `SXML: ` && lx_sxml->get_text( ) ).
    endtry.
  endmethod.

  method _parse.

    data lo_reader type ref to if_sxml_reader.
    data lr_stack_top like line of mt_stack.
    data lo_node type ref to if_sxml_node.
    field-symbols <item> like line of rt_json_tree.

    clear mt_stack.
    if iv_json is initial.
      return.
    endif.
    lo_reader = cl_sxml_string_reader=>create( cl_abap_codepage=>convert_to( iv_json ) ).

    " TODO: self protection, check non-empty, check starting from object ...

    do.
      lo_node = lo_reader->read_next_node( ).
      if lo_node is not bound.
        exit.
      endif.


      case lo_node->type.
        when if_sxml_node=>co_nt_element_open.
          data lt_attributes type if_sxml_attribute=>attributes.
          data lo_attr like line of lt_attributes.
          data lo_open type ref to if_sxml_open_element.
          lo_open ?= lo_node.

          append initial line to rt_json_tree assigning <item>.

          <item>-type = to_lower( lo_open->qname-name ).

          read table mt_stack index 1 into lr_stack_top.
          if sy-subrc = 0.
            <item>-path = join_path( mt_stack ).
            lr_stack_top->children = lr_stack_top->children + 1.

            if lr_stack_top->type = 'array'.
              <item>-name = |{ lr_stack_top->children }|.
              <item>-index = lr_stack_top->children.
            else.
              lt_attributes = lo_open->get_attributes( ).
              loop at lt_attributes into lo_attr.
                if lo_attr->qname-name = 'name' and lo_attr->value_type = if_sxml_value=>co_vt_text.
                  <item>-name = lo_attr->get_value( ).
                endif.
              endloop.
            endif.
          endif.

          get reference of <item> into lr_stack_top.
          insert lr_stack_top into mt_stack index 1.

        when if_sxml_node=>co_nt_element_close.
          data lo_close type ref to if_sxml_close_element.
          lo_close ?= lo_node.

          read table mt_stack index 1 into lr_stack_top.
          delete mt_stack index 1.
          if lo_close->qname-name <> lr_stack_top->type.
            raise( 'Unexpected closing node type' ).
          endif.

        when if_sxml_node=>co_nt_value.
          data lo_value type ref to if_sxml_value_node.
          lo_value ?= lo_node.

          <item>-value = lo_value->get_value( ).

        when others.
          raise( 'Unexpected node type' ).
      endcase.
    enddo.

    if lines( mt_stack ) > 0.
      raise( 'Unexpected end of data' ).
    endif.

  endmethod.

  method join_path.

    field-symbols <ref> like line of it_stack.

    loop at it_stack assigning <ref>.
      rv_path = <ref>->name && '/' && rv_path.
    endloop.

  endmethod.

  method raise.

    zcx_ajson_error=>raise(
      iv_location = join_path( mt_stack )
      iv_msg      = |JSON PARSER: { iv_error } @ { join_path( mt_stack ) }| ).

  endmethod.

endclass.

**********************************************************************
* SERIALIZER
**********************************************************************

class lcl_json_serializer definition final create private.
  public section.

    class-methods stringify
      importing
        it_json_tree type zif_ajson=>ty_nodes_ts
        iv_indent type i default 0
        iv_keep_item_order type abap_bool default abap_false
      returning
        value(rv_json_string) type string
      raising
        zcx_ajson_error.

    class-methods class_constructor.

  private section.

    class-data gv_comma_with_lf type string.

    data mt_json_tree type zif_ajson=>ty_nodes_ts.
    data mv_keep_item_order type abap_bool.
    data mt_buffer type string_table.
    data mv_indent_step type i.
    data mv_level type i.

    class-methods escape
      importing
        iv_unescaped type string
      returning
        value(rv_escaped) type string.

    methods _stringify
      returning
        value(rv_json_string) type string
      raising
        zcx_ajson_error.

    methods stringify_node
      importing
        is_node type zif_ajson=>ty_node
      raising
        zcx_ajson_error.

    methods stringify_set
      importing
        iv_parent_path type string
        iv_array type abap_bool
      raising
        zcx_ajson_error.

endclass.

class lcl_json_serializer implementation.

  method class_constructor.
    gv_comma_with_lf = ',' && cl_abap_char_utilities=>newline.
  endmethod.

  method stringify.

    data lo type ref to lcl_json_serializer.
    create object lo.
    lo->mt_json_tree = it_json_tree.
    lo->mv_indent_step = iv_indent.
    lo->mv_keep_item_order = iv_keep_item_order.
    rv_json_string = lo->_stringify( ).

  endmethod.

  method _stringify.

    field-symbols <n> like line of mt_json_tree.
    read table mt_json_tree assigning <n>
      with key
        path = ''
        name = ''. " Root
    if sy-subrc <> 0.
      return.
    endif.

    stringify_node( <n> ).

    rv_json_string = concat_lines_of( table = mt_buffer ).

  endmethod.

  method stringify_node.

    data lv_item type string.
    data lv_indent_prefix type string.

    if mv_indent_step > 0.
      lv_indent_prefix = repeat( val = ` ` occ = mv_indent_step * mv_level ).
      lv_item = lv_indent_prefix.
    endif.

    if is_node-name is not initial and is_node-index is initial. " Not root, not array item
      if mv_indent_step > 0.
        lv_item = lv_item && |"{ is_node-name }": |.
      else.
        lv_item = |"{ is_node-name }":|.
      endif.
    endif.

    case is_node-type.
      when zif_ajson=>node_type-array.
        lv_item = lv_item && '['.
      when zif_ajson=>node_type-object.
        lv_item = lv_item && '{'.
      when zif_ajson=>node_type-string.
        lv_item = lv_item && |"{ escape( is_node-value ) }"|.
      when zif_ajson=>node_type-boolean or zif_ajson=>node_type-number.
        lv_item = lv_item && is_node-value.
      when zif_ajson=>node_type-null.
        lv_item = lv_item && 'null'.
      when others.
        zcx_ajson_error=>raise(
          iv_msg = |Unexpected type [{ is_node-type }]|
          iv_location = is_node-path && is_node-name ).
    endcase.

    if mv_indent_step > 0
      and ( is_node-type = zif_ajson=>node_type-array or is_node-type = zif_ajson=>node_type-object )
      and is_node-children > 0.
      mv_level = mv_level + 1.
      lv_item = lv_item && cl_abap_char_utilities=>newline.
    endif.

    append lv_item to mt_buffer.

    " finish complex item

    if is_node-type = zif_ajson=>node_type-array or is_node-type = zif_ajson=>node_type-object.
      data lv_children_path type string.
      data lv_tail type string.

      lv_children_path = is_node-path && is_node-name && '/'. " for root: path = '' and name = '', so result is '/'

      case is_node-type.
        when zif_ajson=>node_type-array.
          if is_node-children > 0.
            stringify_set(
              iv_parent_path = lv_children_path
              iv_array       = abap_true ).
          endif.
          lv_tail = ']'.
        when zif_ajson=>node_type-object.
          if is_node-children > 0.
            stringify_set(
              iv_parent_path = lv_children_path
              iv_array       = abap_false ).
          endif.
          lv_tail = '}'.
      endcase.

      if mv_indent_step > 0 and is_node-children > 0.
        lv_tail = lv_indent_prefix && lv_tail.
        mv_level = mv_level - 1.
      endif.
      append lv_tail to mt_buffer.
    endif.

  endmethod.

  method stringify_set.

    data lv_tab_key type string.
    data lv_first_done type abap_bool.
    field-symbols <n> like line of mt_json_tree.

    if iv_array = abap_true.
      lv_tab_key = 'array_index'. " path + index
    elseif mv_keep_item_order = abap_true.
      lv_tab_key = 'item_order'. " path + order
    else.
      lv_tab_key = 'primary_key'. " path + name
    endif.

    loop at mt_json_tree assigning <n> using key (lv_tab_key) where path = iv_parent_path.
      if lv_first_done = abap_false.
        lv_first_done = abap_true.
      elseif mv_indent_step > 0.
        append gv_comma_with_lf to mt_buffer.
      else.
        append ',' to mt_buffer.
      endif.
      stringify_node( <n> ).
    endloop.

    if mv_indent_step > 0 and lv_first_done = abap_true. " only of items were in the list
      append cl_abap_char_utilities=>newline to mt_buffer.
    endif.

  endmethod.

  method escape.

    rv_escaped = iv_unescaped.
    if rv_escaped ca |"\\\t\n\r|.
      " TODO consider performance ...
      " see also https://www.json.org/json-en.html
      rv_escaped = replace(
        val = rv_escaped
        sub = '\'
        with = '\\'
        occ = 0 ).
      rv_escaped = replace(
        val = rv_escaped
        sub = |\n|
        with = '\n'
        occ = 0 ).
      rv_escaped = replace(
        val = rv_escaped
        sub = |\r|
        with = '\r'
        occ = 0 ).
      rv_escaped = replace(
        val = rv_escaped
        sub = |\t|
        with = '\t'
        occ = 0 ).
      rv_escaped = replace(
        val = rv_escaped
        sub = '"'
        with = '\"'
        occ = 0 ).

    endif.

  endmethod.

endclass.


**********************************************************************
* JSON_TO_ABAP
**********************************************************************

class lcl_json_to_abap definition final.
  public section.

    methods find_loc
      importing
        iv_path type string
        iv_name type string optional " not mandatory
        iv_append_tables type abap_bool default abap_false
      returning
        value(r_ref) type ref to data
      raising
        zcx_ajson_error.

    class-methods bind
      importing
        !ii_custom_mapping type ref to zif_ajson_mapping optional
      changing
        c_obj              type any
        co_instance        type ref to lcl_json_to_abap.

    methods to_abap
      importing
        it_nodes type zif_ajson=>ty_nodes_ts
      raising
        zcx_ajson_error.

    methods to_timestamp
      importing
        is_path          type zif_ajson=>ty_node
      returning
        value(rv_result) type timestamp
      raising
        zcx_ajson_error.

  private section.
    data mr_obj type ref to data.
    data mi_custom_mapping type ref to zif_ajson_mapping.

endclass.

class lcl_json_to_abap implementation.

  method bind.
    create object co_instance.
    get reference of c_obj into co_instance->mr_obj.
    co_instance->mi_custom_mapping = ii_custom_mapping.
  endmethod.

  method to_abap.

    data lr_ref type ref to data.
    data lv_type type c.
    data lx type ref to cx_root.
    field-symbols <n> like line of it_nodes.
    field-symbols <value> type any.

    try.
      loop at it_nodes assigning <n> using key array_index.
        lr_ref = find_loc(
          iv_append_tables = abap_true
          iv_path = <n>-path
          iv_name = <n>-name ).
        assign lr_ref->* to <value>.
        assert sy-subrc = 0.
        describe field <value> type lv_type.

        case <n>-type.
          when zif_ajson=>node_type-null.
            " Do nothing
          when zif_ajson=>node_type-boolean.
            <value> = boolc( <n>-value = 'true' ).
          when zif_ajson=>node_type-number.
            <value> = <n>-value.
          when zif_ajson=>node_type-string.
            if lv_type = 'D' and <n>-value is not initial.
              data lv_y type c length 4.
              data lv_m type c length 2.
              data lv_d type c length 2.

              find first occurrence of regex '^(\d{4})-(\d{2})-(\d{2})(T|$)'
                in <n>-value
                submatches lv_y lv_m lv_d.
              if sy-subrc <> 0.
                zcx_ajson_error=>raise(
                  iv_msg      = 'Unexpected date format'
                  iv_location = <n>-path && <n>-name ).
              endif.
              concatenate lv_y lv_m lv_d into <value>.
            elseif lv_type = 'P' and <n>-value is not initial.
              <value> = to_timestamp( is_path = <n> ).
            else.
              <value> = <n>-value.
            endif.
          when zif_ajson=>node_type-object.
            if not lv_type co 'uv'.
              zcx_ajson_error=>raise(
                iv_msg      = 'Expected structure'
                iv_location = <n>-path && <n>-name ).
            endif.
          when zif_ajson=>node_type-array.
            if not lv_type co 'h'.
              zcx_ajson_error=>raise(
                iv_msg      = 'Expected table'
                iv_location = <n>-path && <n>-name ).
            endif.
          when others.
            zcx_ajson_error=>raise(
              iv_msg      = |Unexpected JSON type [{ <n>-type }]|
              iv_location = <n>-path && <n>-name ).
        endcase.

      endloop.
    catch cx_sy_conversion_no_number into lx.
      zcx_ajson_error=>raise(
        iv_msg      = |Source is not a number|
        iv_location = <n>-path && <n>-name ).
    endtry.

  endmethod.

  method find_loc.

    data lt_path type string_table.
    data lv_trace type string.
    data lv_seg like line of lt_path.
    data lv_type type c.
    data lv_size type i.
    data lv_index type i.
    field-symbols <struc> type any.
    field-symbols <table> type standard table.
    field-symbols <value> type any.
    field-symbols <seg> like line of lt_path.

    split iv_path at '/' into table lt_path.
    delete lt_path where table_line is initial.
    if iv_name is not initial.
      append iv_name to lt_path.
    endif.

    r_ref = mr_obj.

    loop at lt_path assigning <seg>.
      lv_trace = lv_trace && '/' && <seg>.

      if mi_custom_mapping is bound.
        lv_seg = mi_custom_mapping->to_abap( iv_path = iv_path iv_name = <seg> ).
      else.
        clear lv_seg.
      endif.

      if lv_seg is initial.
        lv_seg = to_upper( <seg> ).
      else.
        lv_seg = to_upper( lv_seg ).
      endif.

      assign r_ref->* to <struc>.
      assert sy-subrc = 0.
      describe field <struc> type lv_type.

      if lv_type ca 'lr'. " data/obj ref
        " TODO maybe in future
        zcx_ajson_error=>raise(
          iv_msg      = 'Cannot assign to ref'
          iv_location = lv_trace ).

      elseif lv_type = 'h'. " table
        if not lv_seg co '0123456789'.
          zcx_ajson_error=>raise(
            iv_msg      = 'Need index to access tables'
            iv_location = lv_trace ).
        endif.
        lv_index = lv_seg.
        assign r_ref->* to <table>.
        assert sy-subrc = 0.

        lv_size = lines( <table> ).
        if iv_append_tables = abap_true and lv_index = lv_size + 1.
          append initial line to <table>.
        endif.

        read table <table> index lv_index assigning <value>.
        if sy-subrc <> 0.
          zcx_ajson_error=>raise(
            iv_msg      = 'Index not found in table'
            iv_location = lv_trace ).
        endif.

      elseif lv_type ca 'uv'. " structure
        assign component lv_seg of structure <struc> to <value>.
        if sy-subrc <> 0.
          zcx_ajson_error=>raise(
            iv_msg      = 'Path not found'
            iv_location = lv_trace ).
        endif.
      else.
        zcx_ajson_error=>raise(
          iv_msg = 'Target is not deep'
          iv_location = lv_trace ).
      endif.
      get reference of <value> into r_ref.
    endloop.

  endmethod.

  method to_timestamp.

    constants lc_tzone_utc type tznzone value `UTC`.
    constants lc_regex_ts_with_hour type string
        value `^(\d{4})-(\d{2})-(\d{2})(T)(\d{2}):(\d{2}):(\d{2})(\+)(\d{2}):(\d{2})`.
    constants lc_regex_ts_utc type string
        value `^(\d{4})-(\d{2})-(\d{2})(T)(\d{2}):(\d{2}):(\d{2})(Z|$)`.

    data:
      begin of ls_timestamp,
        year         type c length 4,
        month        type c length 2,
        day          type c length 2,
        t            type c length 1,
        hour         type c length 2,
        minute       type c length 2,
        second       type c length 2,
        local_sign   type c length 1,
        local_hour   type c length 2,
        local_minute type c length 2,
      end of ls_timestamp.

    data lv_date type d.
    data lv_time type t.
    data lv_seconds_conv type i.
    data lv_timestamp type timestamp.

    find first occurrence of regex lc_regex_ts_with_hour
      in is_path-value submatches ls_timestamp-year ls_timestamp-month ls_timestamp-day ls_timestamp-t
                                  ls_timestamp-hour ls_timestamp-minute ls_timestamp-second
                                  ls_timestamp-local_sign ls_timestamp-local_hour ls_timestamp-local_minute.

    if sy-subrc = 0.

      lv_seconds_conv = ( ls_timestamp-local_hour * 3600 ) + ( ls_timestamp-local_minute * 60 ).

    else.

      find first occurrence of regex lc_regex_ts_utc
        in is_path-value submatches ls_timestamp-year ls_timestamp-month ls_timestamp-day ls_timestamp-t
                                    ls_timestamp-hour ls_timestamp-minute ls_timestamp-second.

      if sy-subrc <> 0.
        zcx_ajson_error=>raise(
          iv_msg      = 'Unexpected timestamp format'
          iv_location = is_path-path && is_path-name ).
      endif.

    endif.

    concatenate ls_timestamp-year ls_timestamp-month ls_timestamp-day into lv_date.
    concatenate ls_timestamp-hour ls_timestamp-minute ls_timestamp-second into lv_time.

    convert date lv_date time lv_time into time stamp lv_timestamp time zone lc_tzone_utc.

    try.

      case ls_timestamp-local_sign.
        when '-'.
          lv_timestamp = cl_abap_tstmp=>add( tstmp = lv_timestamp secs = lv_seconds_conv ).
        when '+'.
          lv_timestamp = cl_abap_tstmp=>subtractsecs( tstmp = lv_timestamp secs = lv_seconds_conv ).
      endcase.

    catch cx_parameter_invalid_range cx_parameter_invalid_type.
      zcx_ajson_error=>raise(
        iv_msg      = 'Unexpected error calculating timestamp'
        iv_location = is_path-path && is_path-name ).
    endtry.

    rv_result = lv_timestamp.

  endmethod.

endclass.

**********************************************************************
* ABAP_TO_JSON
**********************************************************************

class lcl_abap_to_json definition final.
  public section.

    class-methods convert
      importing
        iv_data            type any
        is_prefix          type zif_ajson=>ty_path_name optional
        iv_array_index     type i default 0
        ii_custom_mapping  type ref to zif_ajson_mapping optional
        iv_keep_item_order type abap_bool default abap_false
      returning
        value(rt_nodes)   type zif_ajson=>ty_nodes_tt
      raising
        zcx_ajson_error.

    class-methods insert_with_type
      importing
        iv_data            type any
        iv_type            type string
        is_prefix          type zif_ajson=>ty_path_name optional
        iv_array_index     type i default 0
        ii_custom_mapping  type ref to zif_ajson_mapping optional
        iv_keep_item_order type abap_bool default abap_false
      returning
        value(rt_nodes)   type zif_ajson=>ty_nodes_tt
      raising
        zcx_ajson_error.

    class-methods class_constructor.

  private section.

    class-data gv_ajson_absolute_type_name type string.
    data mi_custom_mapping type ref to zif_ajson_mapping.
    data mv_keep_item_order type abap_bool.

    methods convert_any
      importing
        iv_data type any
        io_type type ref to cl_abap_typedescr
        is_prefix type zif_ajson=>ty_path_name
        iv_index type i default 0
        iv_item_order type i default 0
      changing
        ct_nodes type zif_ajson=>ty_nodes_tt
      raising
        zcx_ajson_error.

    methods convert_ajson
      importing
        io_json type ref to zcl_ajson
        is_prefix type zif_ajson=>ty_path_name
        iv_index type i default 0
      changing
        ct_nodes type zif_ajson=>ty_nodes_tt.

    methods convert_value
      importing
        iv_data type any
        io_type type ref to cl_abap_typedescr
        is_prefix type zif_ajson=>ty_path_name
        iv_index type i default 0
        iv_item_order type i default 0
      changing
        ct_nodes type zif_ajson=>ty_nodes_tt
      raising
        zcx_ajson_error.

    methods convert_ref
      importing
        iv_data type any
        is_prefix type zif_ajson=>ty_path_name
        iv_index type i default 0
        iv_item_order type i default 0
      changing
        ct_nodes type zif_ajson=>ty_nodes_tt
      raising
        zcx_ajson_error.

    methods convert_struc
      importing
        iv_data type any
        io_type type ref to cl_abap_typedescr
        is_prefix type zif_ajson=>ty_path_name
        iv_index type i default 0
        iv_item_order type i default 0
      changing
        ct_nodes type zif_ajson=>ty_nodes_tt
        cs_root  type zif_ajson=>ty_node optional
      raising
        zcx_ajson_error.

    methods convert_table
      importing
        iv_data type any
        io_type type ref to cl_abap_typedescr
        is_prefix type zif_ajson=>ty_path_name
        iv_index type i default 0
        iv_item_order type i default 0
      changing
        ct_nodes type zif_ajson=>ty_nodes_tt
      raising
        zcx_ajson_error.

    methods insert_value_with_type
      importing
        iv_data type any
        iv_type type string
        io_type type ref to cl_abap_typedescr
        is_prefix type zif_ajson=>ty_path_name
        iv_index type i default 0
        iv_item_order type i default 0
      changing
        ct_nodes type zif_ajson=>ty_nodes_tt
      raising
        zcx_ajson_error.

endclass.

class lcl_abap_to_json implementation.

  method class_constructor.

    data lo_dummy type ref to zcl_ajson.
    data lo_type type ref to cl_abap_refdescr.
    lo_type ?= cl_abap_typedescr=>describe_by_data( lo_dummy ).
    gv_ajson_absolute_type_name = lo_type->get_referenced_type( )->absolute_name.

  endmethod.

  method convert.

    data lo_type type ref to cl_abap_typedescr.
    data lo_converter type ref to lcl_abap_to_json.

    lo_type = cl_abap_typedescr=>describe_by_data( iv_data ).

    create object lo_converter.
    lo_converter->mi_custom_mapping = ii_custom_mapping.
    lo_converter->mv_keep_item_order = iv_keep_item_order.

    lo_converter->convert_any(
      exporting
        iv_data   = iv_data
        io_type   = lo_type
        is_prefix = is_prefix
        iv_index  = iv_array_index
      changing
        ct_nodes = rt_nodes ).

  endmethod.

  method convert_any.

    case io_type->kind.
      when cl_abap_typedescr=>kind_elem.
        convert_value(
          exporting
            iv_data   = iv_data
            io_type   = io_type
            is_prefix = is_prefix
            iv_index  = iv_index
            iv_item_order = iv_item_order
          changing
            ct_nodes = ct_nodes ).

      when cl_abap_typedescr=>kind_struct.
        convert_struc(
          exporting
            iv_data   = iv_data
            io_type   = io_type
            is_prefix = is_prefix
            iv_index  = iv_index
            iv_item_order = iv_item_order
          changing
            ct_nodes = ct_nodes ).

      when cl_abap_typedescr=>kind_table.
        convert_table(
          exporting
            iv_data   = iv_data
            io_type   = io_type
            is_prefix = is_prefix
            iv_index  = iv_index
            iv_item_order = iv_item_order
          changing
            ct_nodes = ct_nodes ).

      when others.

        if io_type->type_kind = cl_abap_typedescr=>typekind_dref.
          convert_ref(
            exporting
              iv_data   = iv_data
              is_prefix = is_prefix
              iv_index  = iv_index
              iv_item_order = iv_item_order
            changing
              ct_nodes = ct_nodes ).

        elseif io_type->type_kind = cl_abap_typedescr=>typekind_oref
          and cl_abap_typedescr=>describe_by_object_ref( iv_data )->absolute_name = gv_ajson_absolute_type_name.
          convert_ajson(
            exporting
              io_json   = iv_data
              is_prefix = is_prefix
              iv_index  = iv_index
            changing
              ct_nodes = ct_nodes ).
        else.
          zcx_ajson_error=>raise( |Unsupported type [{ io_type->type_kind
            }] @{ is_prefix-path && is_prefix-name }| ).
        endif.

    endcase.

  endmethod.

  method convert_ajson.

    field-symbols <n> like line of ct_nodes.

    ct_nodes = io_json->mt_json_tree.

    loop at ct_nodes assigning <n>.
      if <n>-path is initial and <n>-name is initial. " root node
        <n>-path  = is_prefix-path.
        <n>-name  = is_prefix-name.
        <n>-index = iv_index.
      else.
        <n>-path = is_prefix-path && is_prefix-name && <n>-path.
      endif.
    endloop.

  endmethod.

  method convert_value.

    field-symbols <n> like line of ct_nodes.

    append initial line to ct_nodes assigning <n>.

    <n>-path  = is_prefix-path.
    <n>-name  = is_prefix-name.
    <n>-index = iv_index.
    <n>-order = iv_item_order.

    if mi_custom_mapping is bound.
      <n>-name = mi_custom_mapping->to_json( iv_path = is_prefix-path iv_name = is_prefix-name ).
    endif.

    if <n>-name is initial.
      <n>-name  = is_prefix-name.
    endif.

    if io_type->absolute_name = '\TYPE-POOL=ABAP\TYPE=ABAP_BOOL' or io_type->absolute_name = '\TYPE=XFELD'.
      <n>-type = zif_ajson=>node_type-boolean.
      if iv_data is not initial.
        <n>-value = 'true'.
      else.
        <n>-value = 'false'.
      endif.
    elseif io_type->type_kind co 'CNgXyDT'. " Char like, date/time, xstring
      <n>-type = zif_ajson=>node_type-string.
      <n>-value = |{ iv_data }|.
    elseif io_type->type_kind co 'bsI8PaeF'. " Numeric
      <n>-type = zif_ajson=>node_type-number.
      <n>-value = |{ iv_data }|.
    else.
      zcx_ajson_error=>raise( |Unexpected elemetary type [{
        io_type->type_kind }] @{ is_prefix-path && is_prefix-name }| ).
    endif.

  endmethod.

  method convert_ref.

    field-symbols <n> like line of ct_nodes.

    append initial line to ct_nodes assigning <n>.

    <n>-path  = is_prefix-path.
    <n>-name  = is_prefix-name.
    <n>-index = iv_index.
    <n>-order = iv_item_order.

    if mi_custom_mapping is bound.
      <n>-name = mi_custom_mapping->to_json( iv_path = is_prefix-path iv_name = is_prefix-name ).
    endif.

    if <n>-name is initial.
      <n>-name  = is_prefix-name.
    endif.

    if iv_data is initial.
      <n>-type  = zif_ajson=>node_type-null.
      <n>-value = 'null'.
    else.
      " TODO support data references
      zcx_ajson_error=>raise( |Unexpected reference @{ is_prefix-path && is_prefix-name }| ).
    endif.

  endmethod.

  method convert_struc.

    data lo_struc type ref to cl_abap_structdescr.
    data lt_comps type cl_abap_structdescr=>component_table.
    data ls_next_prefix like is_prefix.
    data lv_item_order type i.

    field-symbols <root> like line of ct_nodes.
    field-symbols <c> like line of lt_comps.
    field-symbols <val> type any.

    lo_struc ?= io_type.
    lt_comps = lo_struc->get_components( ).
    " get_components is potentially much slower than lo_struc->components
    " but ! we still need it to identify booleans
    " and rtti seems to cache type descriptions really well (https://github.com/sbcgua/benchmarks.git)
    " the structures will be repeated in real life

    if cs_root is supplied. " call for include structure
      assign cs_root to <root>.
    else. " First call
      append initial line to ct_nodes assigning <root>.
      <root>-path  = is_prefix-path.
      <root>-name  = is_prefix-name.
      <root>-type  = zif_ajson=>node_type-object.
      <root>-index = iv_index.

      if mi_custom_mapping is bound.
        <root>-name = mi_custom_mapping->to_json( iv_path = is_prefix-path iv_name = is_prefix-name ).
      endif.

      if <root>-name is initial.
        <root>-name  = is_prefix-name.
      endif.

      <root>-order = iv_item_order.
    endif.

    ls_next_prefix-path = is_prefix-path && is_prefix-name && '/'.

    loop at lt_comps assigning <c>.

      if <c>-as_include = abap_true.

        convert_struc(
          exporting
            iv_data   = iv_data
            io_type   = <c>-type
            is_prefix = is_prefix
          changing
            cs_root  = <root>
            ct_nodes = ct_nodes ).

      else.

        <root>-children = <root>-children + 1.
        ls_next_prefix-name = to_lower( <c>-name ).
        assign component <c>-name of structure iv_data to <val>.
        assert sy-subrc = 0.

        if mv_keep_item_order = abap_true.
          lv_item_order = <root>-children.
        endif.

        convert_any(
          exporting
            iv_data   = <val>
            io_type   = <c>-type
            is_prefix = ls_next_prefix
            iv_item_order = lv_item_order
          changing
            ct_nodes = ct_nodes ).

      endif.

    endloop.

  endmethod.

  method convert_table.

    data lo_table type ref to cl_abap_tabledescr.
    data lo_ltype type ref to cl_abap_typedescr.
    data ls_next_prefix like is_prefix.

    field-symbols <root> like line of ct_nodes.
    field-symbols <tab> type any table.
    field-symbols <val> type any.

    lo_table ?= io_type.
    lo_ltype = lo_table->get_table_line_type( ).

    append initial line to ct_nodes assigning <root>.
    <root>-path  = is_prefix-path.
    <root>-name  = is_prefix-name.
    <root>-type  = zif_ajson=>node_type-array.
    <root>-index = iv_index.
    <root>-order = iv_item_order.

    if mi_custom_mapping is bound.
      <root>-name = mi_custom_mapping->to_json( iv_path = is_prefix-path iv_name = is_prefix-name ).
    endif.

    if <root>-name is initial.
      <root>-name  = is_prefix-name.
    endif.

    ls_next_prefix-path = is_prefix-path && is_prefix-name && '/'.
    assign iv_data to <tab>.

    loop at <tab> assigning <val>.
      ls_next_prefix-name = to_lower( |{ sy-tabix }| ).

      convert_any(
        exporting
          iv_data   = <val>
          io_type   = lo_ltype
          is_prefix = ls_next_prefix
          iv_index  = <root>-children + 1
        changing
          ct_nodes = ct_nodes ).

      <root>-children = <root>-children + 1.
    endloop.

  endmethod.

  method insert_with_type.

    data lo_type type ref to cl_abap_typedescr.
    data lo_converter type ref to lcl_abap_to_json.

    lo_type = cl_abap_typedescr=>describe_by_data( iv_data ).

    create object lo_converter.
    lo_converter->mi_custom_mapping = ii_custom_mapping.
    lo_converter->mv_keep_item_order = iv_keep_item_order.

    lo_converter->insert_value_with_type(
      exporting
        iv_data   = iv_data
        iv_type   = iv_type
        io_type   = lo_type
        is_prefix = is_prefix
        iv_index  = iv_array_index
      changing
        ct_nodes = rt_nodes ).

  endmethod.

  method insert_value_with_type.

    data lv_prefix type string.

    field-symbols <n> like line of ct_nodes.

    lv_prefix = is_prefix-path && is_prefix-name.
    if io_type->type_kind co 'CNgXyDT'. " Char like, date/time, xstring
      if iv_type = zif_ajson=>node_type-boolean and iv_data <> 'true' and iv_data <> 'false'.
        zcx_ajson_error=>raise( |Unexpected boolean value [{ iv_data }] @{ lv_prefix }| ).
      elseif iv_type = zif_ajson=>node_type-null and iv_data is not initial.
        zcx_ajson_error=>raise( |Unexpected null value [{ iv_data }] @{ lv_prefix }| ).
      elseif iv_type = zif_ajson=>node_type-number and iv_data cn '0123456789. E+-'.
        zcx_ajson_error=>raise( |Unexpected numeric value [{ iv_data }] @{ lv_prefix }| ).
      elseif iv_type <> zif_ajson=>node_type-string and iv_type <> zif_ajson=>node_type-boolean
        and iv_type <> zif_ajson=>node_type-null and iv_type <> zif_ajson=>node_type-number.
        zcx_ajson_error=>raise( |Unexpected type for value [{ iv_type },{ iv_data }] @{ lv_prefix }| ).
      endif.
    elseif io_type->type_kind co 'bsI8PaeF'. " Numeric
      if iv_type <> zif_ajson=>node_type-number.
        zcx_ajson_error=>raise( |Unexpected value for numeric [{ iv_data }] @{ lv_prefix }| ).
      endif.
    else.
      zcx_ajson_error=>raise( |Unexpected type [{ io_type->type_kind }] @{ lv_prefix }| ).
    endif.

    append initial line to ct_nodes assigning <n>.

    <n>-path  = is_prefix-path.
    <n>-name  = is_prefix-name.
    <n>-index = iv_index.
    <n>-value = iv_data.
    <n>-type  = iv_type.
    <n>-order = iv_item_order.

    if mi_custom_mapping is bound.
      <n>-name = mi_custom_mapping->to_json( iv_path = is_prefix-path iv_name = is_prefix-name ).
    endif.

    if <n>-name is initial.
      <n>-name  = is_prefix-name.
    endif.

  endmethod.

endclass.



class zcl_ajson implementation.


  method create_empty.
    create object ro_instance.
    ro_instance->mi_custom_mapping = ii_custom_mapping.
  endmethod.


  method delete_subtree.

    data lv_parent_path type string.
    data lv_parent_path_len type i.
    field-symbols <node> like line of mt_json_tree.
    read table mt_json_tree assigning <node>
      with key
        path = iv_path
        name = iv_name.
    if sy-subrc = 0. " Found ? delete !
      if <node>-children > 0. " only for objects and arrays
        lv_parent_path = iv_path && iv_name && '/'.
        lv_parent_path_len = strlen( lv_parent_path ).
        loop at mt_json_tree assigning <node>.
          if strlen( <node>-path ) >= lv_parent_path_len
            and substring( val = <node>-path len = lv_parent_path_len ) = lv_parent_path.
            delete mt_json_tree index sy-tabix.
          endif.
        endloop.
      endif.

      delete mt_json_tree where path = iv_path and name = iv_name.
      rv_deleted = abap_true.

      data ls_path type zif_ajson=>ty_path_name.
      ls_path = lcl_utils=>split_path( iv_path ).
      read table mt_json_tree assigning <node>
        with key
          path = ls_path-path
          name = ls_path-name.
      if sy-subrc = 0.
        <node>-children = <node>-children - 1.
      endif.
    endif.

  endmethod.


  method get_item.

    field-symbols <item> like line of mt_json_tree.
    data ls_path_name type zif_ajson=>ty_path_name.
    ls_path_name = lcl_utils=>split_path( iv_path ).

    read table mt_json_tree
      assigning <item>
      with key
        path = ls_path_name-path
        name = ls_path_name-name.
    if sy-subrc = 0.
      get reference of <item> into rv_item.
    endif.

  endmethod.


  method parse.

    data lo_parser type ref to lcl_json_parser.

    create object ro_instance.
    create object lo_parser.
    ro_instance->mt_json_tree = lo_parser->parse( iv_json ).
    ro_instance->mi_custom_mapping = ii_custom_mapping.

    if iv_freeze = abap_true.
      ro_instance->freeze( ).
    endif.

  endmethod.


  method prove_path_exists.

    data lt_path type string_table.
    data lr_node like line of rt_node_stack.
    data lr_node_parent like line of rt_node_stack.
    data lv_cur_path type string.
    data lv_cur_name type string.
    data ls_new_node like line of mt_json_tree.

    split iv_path at '/' into table lt_path.
    delete lt_path where table_line is initial.

    do.
      lr_node_parent = lr_node.
      read table mt_json_tree reference into lr_node
        with key
          path = lv_cur_path
          name = lv_cur_name.
      if sy-subrc <> 0. " New node, assume it is always object as it has a named child, use touch_array to init array
        clear ls_new_node.
        if lr_node_parent is not initial. " if has parent
          lr_node_parent->children = lr_node_parent->children + 1.
          if lr_node_parent->type = zif_ajson=>node_type-array.
            ls_new_node-index = lcl_utils=>validate_array_index(
              iv_path  = lv_cur_path
              iv_index = lv_cur_name ).
          endif.
        endif.
        ls_new_node-path = lv_cur_path.
        ls_new_node-name = lv_cur_name.
        ls_new_node-type = zif_ajson=>node_type-object.
        insert ls_new_node into table mt_json_tree reference into lr_node.
      endif.
      insert lr_node into rt_node_stack index 1.
      lv_cur_path = lv_cur_path && lv_cur_name && '/'.
      read table lt_path index sy-index into lv_cur_name.
      if sy-subrc <> 0.
        exit. " no more segments
      endif.
    enddo.

    assert lv_cur_path = iv_path. " Just in case

  endmethod.


  method zif_ajson_reader~array_to_string_table.

    data lv_normalized_path type string.
    data lr_node type ref to zif_ajson=>ty_node.
    field-symbols <item> like line of mt_json_tree.

    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).
    lr_node = get_item( iv_path ).

    if lr_node is initial.
      zcx_ajson_error=>raise( |Path not found: { iv_path }| ).
    endif.
    if lr_node->type <> zif_ajson=>node_type-array.
      zcx_ajson_error=>raise( |Array expected at: { iv_path }| ).
    endif.

    loop at mt_json_tree assigning <item> where path = lv_normalized_path.
      case <item>-type.
        when zif_ajson=>node_type-number or zif_ajson=>node_type-string.
          append <item>-value to rt_string_table.
        when zif_ajson=>node_type-null.
          append '' to rt_string_table.
        when zif_ajson=>node_type-boolean.
          data lv_tmp type string.
          if <item>-value = 'true'.
            lv_tmp = abap_true.
          else.
            clear lv_tmp.
          endif.
          append lv_tmp to rt_string_table.
        when others.
          zcx_ajson_error=>raise( |Cannot convert [{ <item>-type
            }] to string at [{ <item>-path }{ <item>-name }]| ).
      endcase.
    endloop.

  endmethod.


  method zif_ajson_reader~exists.

    data lv_item type ref to zif_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial.
      rv_exists = abap_true.
    endif.

  endmethod.


  method zif_ajson_reader~get.

    data lv_item type ref to zif_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial.
      rv_value = lv_item->value.
    endif.

  endmethod.


  method zif_ajson_reader~get_boolean.

    data lv_item type ref to zif_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is initial or lv_item->type = zif_ajson=>node_type-null.
      return.
    elseif lv_item->type = zif_ajson=>node_type-boolean.
      rv_value = boolc( lv_item->value = 'true' ).
    elseif lv_item->value is not initial.
      rv_value = abap_true.
    endif.

  endmethod.


  method zif_ajson_reader~get_date.

    data lv_item type ref to zif_ajson=>ty_node.
    data lv_y type c length 4.
    data lv_m type c length 2.
    data lv_d type c length 2.

    lv_item = get_item( iv_path ).

    if lv_item is not initial and lv_item->type = zif_ajson=>node_type-string.
      find first occurrence of regex '^(\d{4})-(\d{2})-(\d{2})(T|$)'
        in lv_item->value
        submatches lv_y lv_m lv_d.
      concatenate lv_y lv_m lv_d into rv_value.
    endif.

  endmethod.


  method zif_ajson_reader~get_integer.

    data lv_item type ref to zif_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial and lv_item->type = zif_ajson=>node_type-number.
      rv_value = lv_item->value.
    endif.

  endmethod.


  method zif_ajson_reader~get_node_type.

    data lv_item type ref to zif_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial.
      rv_node_type = lv_item->type.
    endif.

  endmethod.


  method zif_ajson_reader~get_number.

    data lv_item type ref to zif_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial and lv_item->type = zif_ajson=>node_type-number.
      rv_value = lv_item->value.
    endif.

  endmethod.


  method zif_ajson_reader~get_string.

    data lv_item type ref to zif_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial and lv_item->type <> zif_ajson=>node_type-null.
      rv_value = lv_item->value.
    endif.

  endmethod.


  method zif_ajson_reader~members.

    data lv_normalized_path type string.
    field-symbols <item> like line of mt_json_tree.

    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).

    loop at mt_json_tree assigning <item> where path = lv_normalized_path.
      append <item>-name to rt_members.
    endloop.

  endmethod.


  method zif_ajson_reader~slice.

    data lo_section         type ref to zcl_ajson.
    data ls_item            like line of mt_json_tree.
    data lv_normalized_path type string.
    data ls_path_parts      type zif_ajson=>ty_path_name.
    data lv_path_len        type i.

    create object lo_section.
    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).
    lv_path_len        = strlen( lv_normalized_path ).
    ls_path_parts      = lcl_utils=>split_path( lv_normalized_path ).

    loop at mt_json_tree into ls_item.
      " TODO potentially improve performance due to sorted tree (all path started from same prefix go in a row)
      if strlen( ls_item-path ) >= lv_path_len
          and substring( val = ls_item-path len = lv_path_len ) = lv_normalized_path.
        ls_item-path = substring( val = ls_item-path off = lv_path_len - 1 ). " less closing '/'
        insert ls_item into table lo_section->mt_json_tree.
      elseif ls_item-path = ls_path_parts-path and ls_item-name = ls_path_parts-name.
        clear: ls_item-path, ls_item-name. " this becomes a new root
        insert ls_item into table lo_section->mt_json_tree.
      endif.
    endloop.

    ri_json = lo_section.

  endmethod.


  method zif_ajson_reader~to_abap.

    data lo_to_abap type ref to lcl_json_to_abap.

    clear ev_container.
    lcl_json_to_abap=>bind(
      exporting
        ii_custom_mapping = mi_custom_mapping
      changing
        c_obj             = ev_container
        co_instance       = lo_to_abap ).
    lo_to_abap->to_abap( mt_json_tree ).

  endmethod.


  method zif_ajson_writer~clear.

    if mv_read_only = abap_true.
      zcx_ajson_error=>raise( 'This json instance is read only' ).
    endif.

    clear mt_json_tree.

  endmethod.


  method zif_ajson_writer~delete.

    if mv_read_only = abap_true.
      zcx_ajson_error=>raise( 'This json instance is read only' ).
    endif.

    data ls_split_path type zif_ajson=>ty_path_name.
    ls_split_path = lcl_utils=>split_path( iv_path ).

    delete_subtree(
      iv_path = ls_split_path-path
      iv_name = ls_split_path-name ).

  endmethod.


  method zif_ajson_writer~push.

    data lr_parent type ref to zif_ajson=>ty_node.
    data lr_new_node type ref to zif_ajson=>ty_node.

    if mv_read_only = abap_true.
      zcx_ajson_error=>raise( 'This json instance is read only' ).
    endif.

    lr_parent = get_item( iv_path ).

    if lr_parent is initial.
      zcx_ajson_error=>raise( |Path [{ iv_path }] does not exist| ).
    endif.

    if lr_parent->type <> zif_ajson=>node_type-array.
      zcx_ajson_error=>raise( |Path [{ iv_path }] is not array| ).
    endif.

    data lt_new_nodes type zif_ajson=>ty_nodes_tt.
    data ls_new_path type zif_ajson=>ty_path_name.

    ls_new_path-path = lcl_utils=>normalize_path( iv_path ).
    ls_new_path-name = |{ lr_parent->children + 1 }|.

    lt_new_nodes = lcl_abap_to_json=>convert(
      iv_keep_item_order = mv_keep_item_order
      iv_data   = iv_val
      is_prefix = ls_new_path ).
    read table lt_new_nodes index 1 reference into lr_new_node. " assume first record is the array item - not ideal !
    assert sy-subrc = 0.
    lr_new_node->index = lr_parent->children + 1.

    " update data
    lr_parent->children = lr_parent->children + 1.
    insert lines of lt_new_nodes into table mt_json_tree.

  endmethod.


  method zif_ajson_writer~set.

    data ls_split_path type zif_ajson=>ty_path_name.
    data lr_parent type ref to zif_ajson=>ty_node.
    data lt_node_stack type tty_node_stack.

    if mv_read_only = abap_true.
      zcx_ajson_error=>raise( 'This json instance is read only' ).
    endif.

    if iv_val is initial and iv_ignore_empty = abap_true and iv_node_type is initial.
      return. " nothing to assign
    endif.

    if iv_node_type is not initial
      and iv_node_type <> zif_ajson=>node_type-boolean and iv_node_type <> zif_ajson=>node_type-null
      and iv_node_type <> zif_ajson=>node_type-number and iv_node_type <> zif_ajson=>node_type-string.
      zcx_ajson_error=>raise( |Unexpected type { iv_node_type }| ).
    endif.

    ls_split_path = lcl_utils=>split_path( iv_path ).
    if ls_split_path is initial. " Assign root, exceptional processing
      if iv_node_type is not initial.
        mt_json_tree = lcl_abap_to_json=>insert_with_type(
          iv_keep_item_order = mv_keep_item_order
          iv_data            = iv_val
          iv_type            = iv_node_type
          is_prefix          = ls_split_path
          ii_custom_mapping  = mi_custom_mapping ).
      else.
        mt_json_tree = lcl_abap_to_json=>convert(
          iv_keep_item_order = mv_keep_item_order
          iv_data            = iv_val
          is_prefix          = ls_split_path
          ii_custom_mapping  = mi_custom_mapping ).
      endif.
      return.
    endif.

    " Ensure whole path exists
    lt_node_stack = prove_path_exists( ls_split_path-path ).
    read table lt_node_stack index 1 into lr_parent.
    assert sy-subrc = 0.

    " delete if exists with subtree
    delete_subtree(
      iv_path = ls_split_path-path
      iv_name = ls_split_path-name ).

    " convert to json
    data lt_new_nodes type zif_ajson=>ty_nodes_tt.
    data lv_array_index type i.

    if lr_parent->type = zif_ajson=>node_type-array.
      lv_array_index = lcl_utils=>validate_array_index(
        iv_path  = ls_split_path-path
        iv_index = ls_split_path-name ).
    endif.

    if iv_node_type is not initial.
      lt_new_nodes = lcl_abap_to_json=>insert_with_type(
        iv_keep_item_order = mv_keep_item_order
        iv_data            = iv_val
        iv_type            = iv_node_type
        iv_array_index     = lv_array_index
        is_prefix          = ls_split_path
        ii_custom_mapping  = mi_custom_mapping ).
    else.
      lt_new_nodes = lcl_abap_to_json=>convert(
        iv_keep_item_order = mv_keep_item_order
        iv_data            = iv_val
        iv_array_index     = lv_array_index
        is_prefix          = ls_split_path
        ii_custom_mapping  = mi_custom_mapping ).
    endif.

    " update data
    lr_parent->children = lr_parent->children + 1.
    insert lines of lt_new_nodes into table mt_json_tree.

  endmethod.


  method zif_ajson_writer~set_boolean.

    data lv_bool type abap_bool.
    lv_bool = boolc( iv_val is not initial ).
    zif_ajson_writer~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_bool ).

  endmethod.


  method zif_ajson_writer~set_date.

    data lv_val type string.

    if iv_val is not initial.
      lv_val = iv_val+0(4) && '-' && iv_val+4(2) && '-' && iv_val+6(2).
    endif.

    zif_ajson_writer~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_val ).

  endmethod.


  method zif_ajson_writer~set_integer.

    zif_ajson_writer~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = iv_val ).

  endmethod.


  method zif_ajson_writer~set_null.

    data lv_null_ref type ref to data.
    zif_ajson_writer~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_null_ref ).

  endmethod.


  method zif_ajson_writer~set_string.

    data lv_val type string.
    lv_val = iv_val.
    zif_ajson_writer~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_val ).

  endmethod.


  method zif_ajson_writer~stringify.

    rv_json = lcl_json_serializer=>stringify(
      it_json_tree       = mt_json_tree
      iv_keep_item_order = mv_keep_item_order
      iv_indent          = iv_indent ).

  endmethod.


  method zif_ajson_writer~touch_array.

    data lr_node type ref to zif_ajson=>ty_node.
    data ls_new_node like line of mt_json_tree.
    data ls_split_path type zif_ajson=>ty_path_name.

    if mv_read_only = abap_true.
      zcx_ajson_error=>raise( 'This json instance is read only' ).
    endif.

    ls_split_path = lcl_utils=>split_path( iv_path ).
    if ls_split_path is initial. " Assign root, exceptional processing
      ls_new_node-path = ls_split_path-path.
      ls_new_node-name = ls_split_path-name.
      ls_new_node-type = 'array'.
      insert ls_new_node into table mt_json_tree.
      return.
    endif.

    if iv_clear = abap_true.
      delete_subtree(
        iv_path = ls_split_path-path
        iv_name = ls_split_path-name ).
    else.
      lr_node = get_item( iv_path ).
    endif.

    if lr_node is initial. " Or node was cleared

      data lr_parent type ref to zif_ajson=>ty_node.
      data lt_node_stack type tty_node_stack.

      lt_node_stack = prove_path_exists( ls_split_path-path ).
      read table lt_node_stack index 1 into lr_parent.
      assert sy-subrc = 0.
      lr_parent->children = lr_parent->children + 1.

      ls_new_node-path = ls_split_path-path.
      ls_new_node-name = ls_split_path-name.
      ls_new_node-type = zif_ajson=>node_type-array.
      insert ls_new_node into table mt_json_tree.

    elseif lr_node->type <> zif_ajson=>node_type-array.
      zcx_ajson_error=>raise( |Path [{ iv_path }] already used and is not array| ).
    endif.

  endmethod.


  method zif_ajson~freeze.
    mv_read_only = abap_true.
  endmethod.


  method zif_ajson~keep_item_order.
    mv_keep_item_order = abap_true.
  endmethod.
endclass.
class lcl_mapping_fields implementation.


  method constructor.

    data ls_mapping_field like line of mt_mapping_fields.

    loop at it_mapping_fields into ls_mapping_field.
      ls_mapping_field-abap = to_upper( ls_mapping_field-abap ).
      insert ls_mapping_field into table mt_mapping_fields.
    endloop.

  endmethod.


  method zif_ajson_mapping~to_abap.

    data ls_mapping_field like line of mt_mapping_fields.

    read table mt_mapping_fields into ls_mapping_field
      with key json components json = iv_name.
    if sy-subrc = 0.
      rv_result = ls_mapping_field-abap.
    endif.

  endmethod.


  method zif_ajson_mapping~to_json.

    data lv_field type string.
    data ls_mapping_field like line of mt_mapping_fields.

    lv_field = to_upper( iv_name ).

    read table mt_mapping_fields into ls_mapping_field
      with key abap components abap = lv_field.
    if sy-subrc = 0.
      rv_result = ls_mapping_field-json.
    endif.

  endmethod.


endclass.


class lcl_mapping_to_upper implementation.


  method constructor.

    mi_mapping_fields = zcl_ajson_mapping=>create_field_mapping( it_mapping_fields ).

  endmethod.


  method zif_ajson_mapping~to_abap.

    rv_result = mi_mapping_fields->to_abap( iv_path = iv_path iv_name = iv_name ).

  endmethod.


  method zif_ajson_mapping~to_json.

    rv_result = mi_mapping_fields->to_json( iv_path = iv_path iv_name = iv_name ).

    if rv_result is not initial. " Mapping found
      return.
    endif.

    rv_result = to_upper( iv_name ).

  endmethod.


endclass.


class lcl_mapping_to_lower implementation.


  method constructor.

    mi_mapping_fields = zcl_ajson_mapping=>create_field_mapping( it_mapping_fields ).

  endmethod.


  method zif_ajson_mapping~to_abap.

    rv_result = mi_mapping_fields->to_abap( iv_path = iv_path iv_name = iv_name ).

  endmethod.


  method zif_ajson_mapping~to_json.

    rv_result = mi_mapping_fields->to_json( iv_path = iv_path iv_name = iv_name ).

    if rv_result is not initial. " Mapping found
      return.
    endif.

    rv_result = to_lower( iv_name ).

  endmethod.


endclass.


class lcl_mapping_camel implementation.


  method constructor.

    mi_mapping_fields   = zcl_ajson_mapping=>create_field_mapping( it_mapping_fields ).
    mv_first_json_upper = iv_first_json_upper.

  endmethod.


  method zif_ajson_mapping~to_abap.

    rv_result = mi_mapping_fields->to_abap( iv_path = iv_path iv_name = iv_name ).

    if rv_result is not initial. " Mapping found
      return.
    endif.

    rv_result = iv_name.

    replace all occurrences of regex `([a-z])([A-Z])` in rv_result with `$1_$2`.

  endmethod.


  method zif_ajson_mapping~to_json.

    types ty_token type c length 255.
    data lt_tokens type standard table of ty_token.
    data lv_from type i.
    field-symbols <token> like line of lt_tokens.

    rv_result = mi_mapping_fields->to_json( iv_path = iv_path iv_name = iv_name ).

    if rv_result is not initial. " Mapping found
      return.
    endif.

    rv_result = iv_name.

    replace all occurrences of `__` in rv_result with `*`.

    translate rv_result to lower case.
    translate rv_result using `/_:_~_`.

    if mv_first_json_upper = abap_true.
      lv_from = 1.
    else.
      lv_from = 2.
    endif.

    split rv_result at `_` into table lt_tokens.
    loop at lt_tokens assigning <token> from lv_from.
      translate <token>(1) to upper case.
    endloop.

    concatenate lines of lt_tokens into rv_result.
    replace all occurrences of `*` in rv_result with `_`.

  endmethod.


endclass.



class zcl_ajson_mapping implementation.


  method create_camel_case.

    create object ri_mapping type lcl_mapping_camel
      exporting
        it_mapping_fields   = it_mapping_fields
        iv_first_json_upper = iv_first_json_upper.

  endmethod.


  method create_upper_case.

    create object ri_mapping type lcl_mapping_to_upper
      exporting
        it_mapping_fields = it_mapping_fields.

  endmethod.


  method create_lower_case.

    create object ri_mapping type lcl_mapping_to_lower
      exporting
        it_mapping_fields = it_mapping_fields.

  endmethod.


  method create_field_mapping.

    create object ri_mapping type lcl_mapping_fields
      exporting
        it_mapping_fields = it_mapping_fields.

  endmethod.


endclass.



CLASS ZCL_AJSON_UTILITIES IMPLEMENTATION.


  method delete_empty_nodes.

    data ls_json_tree like line of io_json->mt_json_tree.
    data lv_subrc type sy-subrc.

    do.
      loop at io_json->mt_json_tree into ls_json_tree
        where type = 'array' and children = 0.

        io_json->delete( ls_json_tree-path && ls_json_tree-name ).

      endloop.
      lv_subrc = sy-subrc.

      loop at io_json->mt_json_tree into ls_json_tree
        where type = 'object' and children = 0.

        io_json->delete( ls_json_tree-path && ls_json_tree-name ).

      endloop.
      if lv_subrc = 4 and sy-subrc = 4.
        exit. " nothing else to delete
      endif.
    enddo.

  endmethod.


  method diff.

    if boolc( iv_json_a is supplied ) = boolc( io_json_a is supplied ).
      zcx_ajson_error=>raise( 'Either supply JSON string or instance, but not both' ).
    endif.
    if boolc( iv_json_b is supplied ) = boolc( io_json_b is supplied ).
      zcx_ajson_error=>raise( 'Either supply JSON string or instance, but not both' ).
    endif.

    if iv_json_a is supplied.
      mo_json_a = zcl_ajson=>parse( iv_json_a ).
    elseif io_json_a is bound.
      mo_json_a = io_json_a.
    else.
      zcx_ajson_error=>raise( 'Supply either JSON string or instance' ).
    endif.

    if iv_json_b is supplied.
      mo_json_b = zcl_ajson=>parse( iv_json_b ).
    elseif io_json_a is bound.
      mo_json_b = io_json_b.
    else.
      zcx_ajson_error=>raise( 'Supply either JSON string or instance' ).
    endif.

    mo_insert = zcl_ajson=>create_empty( ).
    mo_delete = zcl_ajson=>create_empty( ).
    mo_change = zcl_ajson=>create_empty( ).

    diff_a_b( '/' ).
    diff_b_a( '/' ).

    eo_insert ?= mo_insert.
    eo_delete ?= mo_delete.
    eo_change ?= mo_change.

    delete_empty_nodes( eo_insert ).
    delete_empty_nodes( eo_delete ).
    delete_empty_nodes( eo_change ).

  endmethod.


  method diff_a_b.

    data:
      lv_path_a type string,
      lv_path_b type string.

    field-symbols:
      <node_a> like line of mo_json_a->mt_json_tree,
      <node_b> like line of mo_json_a->mt_json_tree.

    loop at mo_json_a->mt_json_tree assigning <node_a> where path = iv_path.
      lv_path_a = <node_a>-path && <node_a>-name && '/'.

      read table mo_json_b->mt_json_tree assigning <node_b>
        with table key path = <node_a>-path name = <node_a>-name.
      if sy-subrc = 0.
        lv_path_b = <node_b>-path && <node_b>-name && '/'.

        if <node_a>-type = <node_b>-type.
          case <node_a>-type.
            when 'array'.
              mo_insert->touch_array( lv_path_a ).
              mo_change->touch_array( lv_path_a ).
              mo_delete->touch_array( lv_path_a ).
              diff_a_b( lv_path_a ).
            when 'object'.
              diff_a_b( lv_path_a ).
            when others.
              if <node_a>-value <> <node_b>-value.
                " save as changed value
                mo_change->set(
                  iv_path      = lv_path_b
                  iv_val       = <node_b>-value
                  iv_node_type = <node_b>-type ).
              endif.
          endcase.
        else.
          " save changed type as delete + insert
          case <node_a>-type.
            when 'array'.
              mo_delete->touch_array( lv_path_a ).
              diff_a_b( lv_path_a ).
            when 'object'.
              diff_a_b( lv_path_a ).
            when others.
              mo_delete->set(
                iv_path      = lv_path_a
                iv_val       = <node_a>-value
                iv_node_type = <node_a>-type ).
          endcase.
          case <node_b>-type.
            when 'array'.
              mo_insert->touch_array( lv_path_b ).
              diff_b_a( lv_path_b ).
            when 'object'.
              diff_b_a( lv_path_b ).
            when others.
              mo_insert->set(
                iv_path      = lv_path_b
                iv_val       = <node_b>-value
                iv_node_type = <node_b>-type ).
          endcase.
        endif.
      else.
        " save as delete
        case <node_a>-type.
          when 'array'.
            mo_delete->touch_array( lv_path_a ).
            diff_a_b( lv_path_a ).
          when 'object'.
            diff_a_b( lv_path_a ).
          when others.
            mo_delete->set(
              iv_path      = lv_path_a
              iv_val       = <node_a>-value
              iv_node_type = <node_a>-type ).
        endcase.
      endif.
    endloop.

  endmethod.


  method diff_b_a.

    data lv_path type string.

    field-symbols:
      <node_a> like line of mo_json_b->mt_json_tree,
      <node_b> like line of mo_json_b->mt_json_tree.

    loop at mo_json_b->mt_json_tree assigning <node_b> where path = iv_path.
      lv_path = <node_b>-path && <node_b>-name && '/'.

      case <node_b>-type.
        when 'array'.
          mo_insert->touch_array( lv_path ).
          diff_b_a( lv_path ).
        when 'object'.
          diff_b_a( lv_path ).
        when others.
          read table mo_json_a->mt_json_tree assigning <node_a>
            with table key path = <node_b>-path name = <node_b>-name.
          if sy-subrc <> 0.
            " save as insert
            mo_insert->set(
              iv_path      = lv_path
              iv_val       = <node_b>-value
              iv_node_type = <node_b>-type ).
          endif.
      endcase.
    endloop.

  endmethod.


  method sort.

    data lo_json type ref to zif_ajson.

    if boolc( iv_json is supplied ) = boolc( io_json is supplied ).
      zcx_ajson_error=>raise( 'Either supply JSON string or instance, but not both' ).
    endif.

    if iv_json is supplied.
      lo_json = zcl_ajson=>parse( iv_json ).
    elseif io_json is bound.
      lo_json = io_json.
    else.
      zcx_ajson_error=>raise( 'Supply either JSON string or instance' ).
    endif.

    " Nodes are parsed into a sorted table, so no explicit sorting required
    rv_sorted = lo_json->stringify( 2 ).

  endmethod.
ENDCLASS.






*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/MBT_TEMPLATE_1_DEFS
*&---------------------------------------------------------------------*

TABLES:
  sscrfields.

DATA:
  gv_options  TYPE abap_bool,
  gx_error    TYPE REF TO zcx_abapinst_exception,
  go_textpool TYPE REF TO zcl_abapinst_textpool,
  gs_inst     TYPE zif_abapinst_definitions=>ty_inst,
  gt_banner   TYPE zif_abapinst_definitions=>ty_base_tab.
*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/MBT_TEMPLATE_2_BANNER
*&---------------------------------------------------------------------*

CONSTANTS:
  c_banner_id   TYPE string VALUE '/MBTOOLS/MBT_INSTALLER',
  c_title       TYPE string VALUE 'MBT Installer',
  c_url_docs    TYPE string VALUE 'https://marcbernardtools.com/docs/marc-bernard-tools/installation/',
  c_url_license TYPE string VALUE 'https://marcbernardtools.com/company/terms-software/',
  c_url_home    TYPE string VALUE 'https://marcbernardtools.com/'.

CONSTANTS:
  c_tabname TYPE tabname  VALUE 'ZMBTINST',
  c_lock    TYPE viewname VALUE 'EZMBTINST'.

FORM banner.
  INSERT 'iVBORw0KGgoAAAANSUhEUgAAAMgAAAAdCAIAAABgybRVAAAALHRFWHRDcmVhdGlvbiBUaW1lAEZyaSAy' INTO TABLE gt_banner.
  INSERT 'NyBOb3YgMjAyMCAyMDoyNjoxOCAtMDUwMAhjuEsAAAAHdElNRQfkCxwBHSIH0G3NAAAACXBIWXMAAAsS' INTO TABLE gt_banner.
  INSERT 'AAALEgHS3X78AAAABGdBTUEAALGPC/xhBQAAAAZ0Uk5TAO4A7gDuHYxM+gAABeBJREFUeNrtmzFM41YY' INTO TABLE gt_banner.
  INSERT 'x30IxMJwMFCdmJwERYroUKVeUjFhEXFZDql4QrK6BC8kZIhOsuhEFXHKEI4shgVZuslLuqQnX8MU3S1u' INTO TABLE gt_banner.
  INSERT 'VIYoUtoknhiOISwsLLR+thM/J3Gwk5dLAv4NyHnv8fdnv4/v+94HvPD5fLVaDUPK8fHx4uIiWs2ZmZmH' INTO TABLE gt_banner.
  INSERT 'hweEgoqFJEmiNdKlzSxyr8LUPUOuqXhVNBpFKHh+fo7cSJc2M+M2wOVp4jqWy0hwHctlJMyO24Bnj1zg' INTO TABLE gt_banner.
  INSERT 'TvNVcOWPxBgSR6uFUt0RFo4Vip4cJuNbPu1T7eN7+vXBl1D0D/4MGkv/dnD+xfadbi4/XPx1bTW78mNk' INTO TABLE gt_banner.
  INSERT '8/u15WVHxt/+fXlZv7WxcDH488aA71TmEkyuol4GkmJmBMfIWo7nJXBB+JWtR62FUt0JvRwrdPLv57gP' INTO TABLE gt_banner.
  INSERT 'GvBtxT//FzcvUsbOtiKBn1YPbPvWdaPRsJprNLJFwbO+/3Z3zYn5t81m08ayoQ6pFUlSdwYLDKPy3Oiu' INTO TABLE gt_banner.
  INSERT 'sUInvMmr+uGL8ychdMY0itl3lzfjfiUuKOiKWNHDtluBbPd7JfDGyIlg8P1eGhrzxQ+jB68ddoQ8nvWV' INTO TABLE gt_banner.
  INSERT 'FdPIdbGoBbOG8Km8YTtqvfR6vVA0ur2t6wFsackL99KWXo7j3T5rOh0rFFjVr2pqXaVcnJexdmr8uLd6' INTO TABLE gt_banner.
  INSERT 'cG4eWw0oMct+qQVYiex2+s7mq3e/CqpvFa/Ku2v2PGsR/8FUOsmX9T81x1oMbgxaVD2GUg6LGAPKFVku' INTO TABLE gt_banner.
  INSERT 'iKdprQALbCdjYRLvuidYI+ar1c5xfyzD4KCAO61WJH1Myp0mqp1LoPsmmDSvZ2WMoFNJvRhXJkSvaand' INTO TABLE gt_banner.
  INSERT 'B4EFYUXzY6ofCmI+X+1plwXOToW1f8pDb0xvlr9TQlhjeJ1vQC3HsrkqHajoVbGKJFE8q+wNB710mQsH' INTO TABLE gt_banner.
  INSERT 'Wam3CB3JYDhWr/I8bwxKsKK+xEJJ4llww5SQrFIUTwvtpfYoJMIUbzatpVgStScAj1n1lSJ5pmMlZFcf' INTO TABLE gt_banner.
  INSERT 'JqPdcFP+kC/q155Xzo6GY8HsAsYoG8R8Te3oWEhYepUz+vinxFLUQIIUb2GaxAYTrSfAMJ4K8vZ1TYzF' INTO TABLE gt_banner.
  INSERT 'sYrZvaLV3HpkYwocqw1IIBFfLZ9m9a3iqURE2RiZS7e2hCDoQNeB0u8FX71+mqaNyEfQdKBzicwxrClb' INTO TABLE gt_banner.
  INSERT 'hZXhupKCWX4wt4UFFUWBi3kx8TTI6uYSqVjPpoRhm2bXY0xGxGrhoY6ctRvGSztvkCQZ9rfiCp8vZEjj' INTO TABLE gt_banner.
  INSERT '5dN9ul84k8nIXEUvdIjt7upFFnMtL6CFtpDyfdANHQEJtswvJFpeBe7Ro36ihVLGaW91sn6l0xAupqjd' INTO TABLE gt_banner.
  INSERT 'QGyHjbeNh7cJ/bJSkzHc14o9PLUUhkgkuIJs/x71atuvImb3xJkkPYDRkGAS+JCaF/XnSZV6/QiAGOb8' INTO TABLE gt_banner.
  INSERT 'IDSWiLW+f7Rpync3ny6yWr/BWbthgiEjNNYqzCUJrvKV+gyqkfsj1yr6FdGdgbx+xZUdxqwOQSgv9g5W' INTO TABLE gt_banner.
  INSERT 'AzOmiLVsZm33F8qjTxWvRnXy/LaQmVKKsJqUWIazE7eMwNcDI/jYBxasG+cLi2A1BJNVYz0tcEZsMnKh' INTO TABLE gt_banner.
  INSERT 'UIfGamlK20upqgyDCKFutXkIoh2VpJwoM6a+lnE8cIIhaJwo9WBV4DgvgypqjcWxrr+WzVHp61VeaDWx' INTO TABLE gt_banner.
  INSERT 'pqLdYANZ1kKS12vKYYGu7GUkTZ4KY+opTQUH7VZQurGS5ghBMNtuijKDNTMMQR0tWAFB0IPIYfZy9KOM' INTO TABLE gt_banner.
  INSERT 'xbEaQjZrNTdl7QYrlOKl/5kNqpnIWIrg9TgGdY5oAfSTQI3O6uW1EmSC7LCWwYKaaHCJhT9BfawhmKxT' INTO TABLE gt_banner.
  INSERT '4ZS1GwaHSHFQXFBypkBb1mP9ijWCtv4+S/oLCmiKrYlxLM86tX/09klEq0dQ9q473ZAZsSSkaKLnhiuO' INTO TABLE gt_banner.
  INSERT 'p06aVIBMU8xkuJRz1wLVX6nTl1VFZH9x9mIUr+7s7GwUssj/S2dnZ2cUdo4SrXLDcXSNAfSKKu6pcLpA' INTO TABLE gt_banner.
  INSERT '7gAjUFSZmFTo8rRwHctlJLiO5TISZhcWFu7u7tCK3t/fz8/PI7cV7T/Fz83NIbfQpc3/q1uUaEN2fD4A' INTO TABLE gt_banner.
  INSERT 'AAAASUVORK5CYIIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA' INTO TABLE gt_banner.
  INSERT 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA                    ' INTO TABLE gt_banner.
ENDFORM.
*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/MBT_TEMPLATE_3_SCREEN
*&---------------------------------------------------------------------*

" Function Keys
SELECTION-SCREEN FUNCTION KEY: 1, 2.

*-----------------------------------------------------------------------

" Source Package
SELECTION-SCREEN BEGIN OF SCREEN 100 AS SUBSCREEN.

SELECTION-SCREEN BEGIN OF BLOCK b100 WITH FRAME.
SELECTION-SCREEN COMMENT:
   /1(77) scr_t100,
   /1(77) scr_t101.
SELECTION-SCREEN END OF BLOCK b100.

SELECTION-SCREEN BEGIN OF BLOCK b110 WITH FRAME.

PARAMETERS:
  p_zip_f RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND c100.
SELECTION-SCREEN: SKIP, BEGIN OF LINE, POSITION 4,
  COMMENT (22) scr_t103 FOR FIELD p_file_f.
PARAMETERS:
  p_file_f TYPE char255 LOWER CASE
    DEFAULT 'C:\Tmp\Marc_Bernard_Tools-main.zip' MODIF ID c12.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 2.
PARAMETERS:
  p_zip_s RADIOBUTTON GROUP g1.
SELECTION-SCREEN: SKIP, BEGIN OF LINE, POSITION 4,
  COMMENT (22) scr_t104 FOR FIELD p_file_s.
PARAMETERS:
  p_file_s TYPE char255 LOWER CASE
    DEFAULT 'Marc_Bernard_Tools-main.zip' MODIF ID c13.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 2.
PARAMETERS:
  p_zip_i RADIOBUTTON GROUP g1.
SELECTION-SCREEN: SKIP, BEGIN OF LINE, POSITION 4,
  COMMENT (22) scr_t102 FOR FIELD p_file_i.
PARAMETERS:
  p_file_i TYPE char255 LOWER CASE
    DEFAULT 'https://github.com/mbtools/Marc_Bernard_Tools/archive/main.zip' MODIF ID c11.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b110.

SELECTION-SCREEN END OF SCREEN 100.

*-----------------------------------------------------------------------

" Target Package
SELECTION-SCREEN BEGIN OF SCREEN 200 AS SUBSCREEN.

SELECTION-SCREEN BEGIN OF BLOCK b200 WITH FRAME.
SELECTION-SCREEN COMMENT:
   /1(77) scr_t200,
   /1(77) scr_t201.
SELECTION-SCREEN END OF BLOCK b200.

SELECTION-SCREEN BEGIN OF BLOCK b210 WITH FRAME.

PARAMETERS:
  p_sap_d RADIOBUTTON GROUP g2 DEFAULT 'X' USER-COMMAND c200.

SELECTION-SCREEN SKIP 2.
PARAMETERS:
  p_sap_l RADIOBUTTON GROUP g2.
SELECTION-SCREEN: SKIP, BEGIN OF LINE, POSITION 4.
PARAMETERS:
  p_pack_l TYPE devclass DEFAULT '' MODIF ID c21.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 2.
PARAMETERS:
  p_sap_t RADIOBUTTON GROUP g2.
SELECTION-SCREEN: SKIP, BEGIN OF LINE, POSITION 4.
PARAMETERS:
  p_pack_t TYPE pbpackdata-devclass  DEFAULT '' MODIF ID c22,
  p_soft_t TYPE pbpackdata-dlvunit   DEFAULT '' MODIF ID c22,
  p_layr_t TYPE pbpackdata-pdevclass DEFAULT '' MODIF ID c22.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN END OF BLOCK b210.

SELECTION-SCREEN END OF SCREEN 200.

*-----------------------------------------------------------------------

" Transport
SELECTION-SCREEN BEGIN OF SCREEN 300 AS SUBSCREEN.

SELECTION-SCREEN BEGIN OF BLOCK b300 WITH FRAME.
SELECTION-SCREEN COMMENT:
   /1(77) scr_t300,
   /1(77) scr_t301.
SELECTION-SCREEN END OF BLOCK b300.

SELECTION-SCREEN BEGIN OF BLOCK b310 WITH FRAME.

PARAMETERS:
  p_tsp_n RADIOBUTTON GROUP g3 USER-COMMAND c300.

SELECTION-SCREEN SKIP 2.
PARAMETERS:
  p_tsp_e RADIOBUTTON GROUP g3.
SELECTION-SCREEN: SKIP, BEGIN OF LINE, POSITION 4.
PARAMETERS:
  p_req_e TYPE trkorr MODIF ID c31.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN SKIP 6.
SELECTION-SCREEN END OF BLOCK b310.

SELECTION-SCREEN END OF SCREEN 300.

*-----------------------------------------------------------------------

" Authentication
SELECTION-SCREEN BEGIN OF SCREEN 400 AS SUBSCREEN.

SELECTION-SCREEN BEGIN OF BLOCK b400 WITH FRAME.
SELECTION-SCREEN COMMENT:
   /1(77) scr_t400,
   /1(77) scr_t401.
SELECTION-SCREEN END OF BLOCK b400.

SELECTION-SCREEN BEGIN OF BLOCK b410 WITH FRAME.

PARAMETERS:
  p_conn_o AS CHECKBOX USER-COMMAND c400.
SELECTION-SCREEN: SKIP, BEGIN OF LINE, POSITION 4,
  COMMENT (22) scr_t402 FOR FIELD p_conn_u.
PARAMETERS:
  p_conn_u TYPE char255 LOWER CASE MODIF ID c40.
SELECTION-SCREEN: END OF LINE, BEGIN OF LINE, POSITION 4,
  COMMENT (22) scr_t403 FOR FIELD p_conn_p.
PARAMETERS:
  p_conn_p TYPE char255 LOWER CASE MODIF ID p40.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 2.

PARAMETERS:
  p_prox_o AS CHECKBOX USER-COMMAND c410.
SELECTION-SCREEN: SKIP, BEGIN OF LINE, POSITION 4,
  COMMENT (22) scr_t404 FOR FIELD p_prox_h.
PARAMETERS:
  p_prox_h TYPE char255 LOWER CASE MODIF ID c41.
SELECTION-SCREEN: END OF LINE, BEGIN OF LINE, POSITION 4,
  COMMENT (22) scr_t405 FOR FIELD p_prox_s.
PARAMETERS:
  p_prox_s TYPE char5 MODIF ID c41.
SELECTION-SCREEN: END OF LINE, BEGIN OF LINE, POSITION 4,
  COMMENT (22) scr_t406 FOR FIELD p_prox_u.
PARAMETERS:
  p_prox_u TYPE char255 LOWER CASE MODIF ID c41.
SELECTION-SCREEN: END OF LINE, BEGIN OF LINE, POSITION 4,
  COMMENT (22) scr_t407 FOR FIELD p_prox_p.
PARAMETERS:
  p_prox_p TYPE char255 LOWER CASE MODIF ID p41.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK b410.

SELECTION-SCREEN END OF SCREEN 400.

*-----------------------------------------------------------------------

" Uninstall (hidden)
SELECTION-SCREEN BEGIN OF SCREEN 500 AS SUBSCREEN.

SELECTION-SCREEN BEGIN OF BLOCK b500 WITH FRAME.
SELECTION-SCREEN COMMENT:
      /1(77) scr_t500,
      /1(77) scr_t501.
SELECTION-SCREEN END OF BLOCK b500.

SELECTION-SCREEN BEGIN OF BLOCK b510 WITH FRAME.

SELECTION-SCREEN COMMENT:
   /1(77) scr_t510.
SELECTION-SCREEN SKIP.
PARAMETERS:
  p_drop_n TYPE zif_abapinst_definitions=>ty_name.
SELECTION-SCREEN SKIP.
PARAMETERS:
  p_drop_p TYPE zif_abapinst_definitions=>ty_pack.

SELECTION-SCREEN SKIP 8.
SELECTION-SCREEN END OF BLOCK b510.

SELECTION-SCREEN END OF SCREEN 500.

*-----------------------------------------------------------------------

" Options
SELECTION-SCREEN BEGIN OF SCREEN 800 AS SUBSCREEN.

SELECTION-SCREEN BEGIN OF BLOCK b800 WITH FRAME.
SELECTION-SCREEN COMMENT:
      /1(77) scr_t800,
      /1(77) scr_t801.
SELECTION-SCREEN END OF BLOCK b800.

SELECTION-SCREEN BEGIN OF BLOCK b810 WITH FRAME.

SELECTION-SCREEN COMMENT:
   /1(77) scr_t810.
SELECTION-SCREEN SKIP.
PARAMETERS:
  p_fold_d RADIOBUTTON GROUP g8 DEFAULT 'X'.
SELECTION-SCREEN SKIP.
PARAMETERS:
  p_fold_p RADIOBUTTON GROUP g8.
SELECTION-SCREEN SKIP.
PARAMETERS:
  p_fold_f RADIOBUTTON GROUP g8.

SELECTION-SCREEN SKIP 4.
SELECTION-SCREEN END OF BLOCK b810.

SELECTION-SCREEN END OF SCREEN 800.

*-----------------------------------------------------------------------

" About
SELECTION-SCREEN:
  BEGIN OF SCREEN 900 AS SUBSCREEN,
    BEGIN OF BLOCK b900 WITH FRAME,
      COMMENT /1(50) scr_t900,
      COMMENT 60(25) scr_t901,
      SKIP,
      COMMENT /1(77) scr_t902,
    END OF BLOCK b900,
    BEGIN OF BLOCK b910 WITH FRAME,
      PUSHBUTTON /1(55) b_docu USER-COMMAND docu,
      SKIP,
      PUSHBUTTON /1(55) b_lice USER-COMMAND lice,
      SKIP,
      PUSHBUTTON /1(55) b_home USER-COMMAND home,
    END OF BLOCK b910,
  END OF SCREEN 900.

*-----------------------------------------------------------------------

" Header
SELECTION-SCREEN:
  BEGIN OF BLOCK scr_header,
    SKIP,
    SKIP,
    COMMENT /3(77) scr_t001 FOR FIELD p_zip_f,
    SKIP,
  END OF BLOCK scr_header,
  BEGIN OF TABBED BLOCK scr_tab FOR 19 LINES,
    TAB (40) scr_tab1 USER-COMMAND scr_push1
      DEFAULT SCREEN 0100 MODIF ID t01,
    TAB (40) scr_tab2 USER-COMMAND scr_push2
      DEFAULT SCREEN 0200 MODIF ID t02,
    TAB (40) scr_tab3 USER-COMMAND scr_push3
      DEFAULT SCREEN 0300 MODIF ID t03,
    TAB (40) scr_tab4 USER-COMMAND scr_push4
      DEFAULT SCREEN 0400 MODIF ID t04,
    TAB (40) scr_tab5 USER-COMMAND scr_push5
      DEFAULT SCREEN 0500 MODIF ID t05,
    TAB (40) scr_tab8 USER-COMMAND scr_push8
      DEFAULT SCREEN 0800 MODIF ID t08,
    TAB (40) scr_tab9 USER-COMMAND scr_push9
      DEFAULT SCREEN 0900,
  END OF BLOCK scr_tab.
*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/MBT_TEMPLATE_4_INIT
*&---------------------------------------------------------------------*

INITIALIZATION.

* Setup
  TRY.
      zcl_abapinst_setup=>run(
        iv_tabname = c_tabname
        iv_lock    = c_lock
        iv_text    = |Generated by { c_title }| ).

      zcl_abapinst_installer=>init(
        iv_tabname = c_tabname
        iv_lock    = c_lock
        iv_name    = 'Marc Bernard Tool'
        iv_names   = 'Marc Bernard Tools' ).
    CATCH zcx_abapinst_exception INTO gx_error.
      MESSAGE gx_error TYPE 'E' DISPLAY LIKE 'I'.
      STOP.
  ENDTRY.

* Textpool
  CREATE OBJECT go_textpool EXPORTING iv_program = sy-cprog.

  go_textpool->set( 'R,,' && c_title ).
  go_textpool->set( 'S,P_CONN_O,Internet Server' ).
  go_textpool->set( 'S,P_CONN_P,Password' ).
  go_textpool->set( 'S,P_CONN_U,User' ).
  go_textpool->set( 'S,P_DROP_N,abapGit Package' ).
  go_textpool->set( 'S,P_DROP_P,SAP Package' ).
  go_textpool->set( 'S,P_FILE_F,Your Computer' ).
  go_textpool->set( 'S,P_FILE_I,Internet' ).
  go_textpool->set( 'S,P_FILE_S,Application Server' ).
  go_textpool->set( 'S,P_FOLD_D,Default (from abapGit Package)' ).
  go_textpool->set( 'S,P_FOLD_F,Full' ).
  go_textpool->set( 'S,P_FOLD_P,Prefix' ).
  go_textpool->set( 'S,P_LAYR_T,Transport Layer' ).
  go_textpool->set( 'S,P_PACK_L,Local Package' ).
  go_textpool->set( 'S,P_PACK_T,Transportable Package' ).
  go_textpool->set( 'S,P_PROX_H,Host' ).
  go_textpool->set( 'S,P_PROX_O,Internet Proxy' ).
  go_textpool->set( 'S,P_PROX_P,Password' ).
  go_textpool->set( 'S,P_PROX_S,Port' ).
  go_textpool->set( 'S,P_PROX_U,User' ).
  go_textpool->set( 'S,P_REQ_E,Transport Request' ).
  go_textpool->set( 'S,P_SAP_D,Default (from abapGit Package)' ).
  go_textpool->set( 'S,P_SAP_L,Local Package' ).
  go_textpool->set( 'S,P_SAP_T,Transportable Package' ).
  go_textpool->set( 'S,P_SOFT_T,Software Component' ).
  go_textpool->set( 'S,P_TSP_E,Use Existing Transport' ).
  go_textpool->set( 'S,P_TSP_N,Create New Transport' ).
  go_textpool->set( 'S,P_ZIP_F,Local File' ).
  go_textpool->set( 'S,P_ZIP_I,Internet' ).
  go_textpool->set( 'S,P_ZIP_S,Application Server' ).

  go_textpool->save( ).

*-----------------------------------------------------------------------

* Function Keys
  sscrfields-functxt_01 = icon_install_package && 'List Packages'.
  sscrfields-functxt_02 = icon_delete && 'Uninstall Package'.

* Header
  scr_t001 = 'An Installer for Marc Bernard Tools'.

*-----------------------------------------------------------------------

* Source Tab
  scr_tab1 = zcl_abapinst_screen=>header( iv_icon = icon_install_package
                                          iv_text = 'MBT Package' ).

  scr_t100 =
  'Select the source of your MBT installation package.'.
  scr_t101 =
  'The package must be a zip file that contains code and objects of the tool.'.

  scr_t102 = 'URL'.
  scr_t103 = 'File name'.
  scr_t104 = 'File name (EPS Inbox)'.

*-----------------------------------------------------------------------

* Target Tab
  scr_tab2 = zcl_abapinst_screen=>header( iv_icon = icon_package_standard
                                          iv_text = 'SAP Package' ).

  scr_t200 =
  'Select the target SAP package for your installation.'.
  scr_t201 =
  'If the package does not exist, it will be created automatically.'.

*-----------------------------------------------------------------------

* Transport Tab
  scr_tab3 = zcl_abapinst_screen=>header( iv_icon = icon_transport
                                          iv_text = 'Transport' ).

  scr_t300 =
  'When installing into a transportable package, decide if you want to create'.
  scr_t301 =
  'a new transport request or select an existing one.'.

*-----------------------------------------------------------------------

* Authentication Tab
  scr_tab4 = zcl_abapinst_screen=>header( iv_icon = icon_connect
                                          iv_text = 'Authentication' ).

  scr_t400 =
  'When downloading the MBT package from the Internet, you might have to'.
  scr_t401 =
  'authenticate yourself at the server and/or your proxy.'.

  scr_t402 = 'User'.
  scr_t403 = 'Password'.
  scr_t404 = 'Proxy Host'.
  scr_t405 = 'Proxy Port'.
  scr_t406 = 'Proxy User'.
  scr_t407 = 'Proxy Password'.

*-----------------------------------------------------------------------

* Uninstall Tab
  scr_tab5 = zcl_abapinst_screen=>header( iv_icon = icon_delete
                                          iv_text = 'Uninstall' ).

  scr_t500 =
  'Select the MBT package that you want to uninstall'.
  scr_t501 =
  ''.

  scr_t510 = 'Uninstall Options:'.

*-----------------------------------------------------------------------

* Options Tab
  scr_tab8 = zcl_abapinst_screen=>header( iv_icon = icon_icon_list
                                          iv_text = 'Options' ).

  scr_t800 =
  'You can select the folder logic to be used and whether you want only'.
  scr_t801 =
  'the main language of the package to be installed.'.

  scr_t810 = 'Folder Logic:'.

*-----------------------------------------------------------------------

* About Tab
  scr_tab9 = zcl_abapinst_screen=>header( iv_icon = icon_system_help
                                          iv_text = 'About' ).

  scr_t900 = |{ c_title }|.
  scr_t901 = |Version { c_version }|.
  scr_t902 = |Copyright (c) { sy-datum(4) } Marc Bernard Tools|.

  b_docu = zcl_abapinst_screen=>icon( iv_name = icon_system_extended_help
                                      iv_text = 'Documentation'
                                      iv_info = c_github ).
  b_lice = zcl_abapinst_screen=>icon( iv_name = icon_legal_reg
                                      iv_text = 'License Terms'
                                      iv_info = c_github ).
  b_home = zcl_abapinst_screen=>icon( iv_name = icon_url
                                      iv_text = 'MarcBernardTools.com'
                                      iv_info = 'MBT Website' ).

  scr_tab-prog = sy-cprog. " abaplint #1291
*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/MBT_TEMPLATE_5_AT
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN.

  zcl_abapinst_screen=>modify(
    iv_options = gv_options
    iv_zip_i   = p_zip_i
    iv_zip_f   = p_zip_f
    iv_zip_s   = p_zip_s
    iv_sap_l   = p_sap_l
    iv_sap_t   = p_sap_t
    iv_tsp_e   = p_tsp_e
    iv_conn_o  = p_conn_o
    iv_prox_o  = p_prox_o
    iv_mbt     = abap_true ).

  CHECK sy-dynnr <> '1000'.

  CASE sscrfields-ucomm.

*   Function Keys
    WHEN 'FC01'. " List Packages
      zcl_abapinst_screen=>banner( iv_show = abap_false ).
      TRY.
          zcl_abapinst_installer=>list( ).

        CATCH zcx_abapinst_exception INTO gx_error.
          MESSAGE gx_error TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

    WHEN 'FC02'. " Uninstall Package
      zcl_abapinst_screen=>banner( iv_show = abap_false ).
      TRY.
          gs_inst = zcl_abapinst_installer=>f4( ).

          IF gs_inst IS NOT INITIAL.
            zcl_abapinst_installer=>uninstall(
              iv_name = gs_inst-name
              iv_pack = gs_inst-pack ).
          ENDIF.

        CATCH zcx_abapinst_exception INTO gx_error.
          MESSAGE gx_error TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

*   About
    WHEN 'DOCU'. " Documentation
      zcl_abapinst_screen=>browser( c_url_docs ).

    WHEN 'LICE'. " License
      zcl_abapinst_screen=>browser( c_url_license ).

    WHEN 'HOME'. " Website
      zcl_abapinst_screen=>browser( c_url_home ).

  ENDCASE.

AT SELECTION-SCREEN OUTPUT.

  PERFORM banner.

*  zcl_abapinst_screen=>banner( iv_id = c_banner_id )
  zcl_abapinst_screen=>banner( it_base = gt_banner ).

  zcl_abapinst_screen=>modify(
    iv_options = gv_options
    iv_zip_i   = p_zip_i
    iv_zip_f   = p_zip_f
    iv_zip_s   = p_zip_s
    iv_sap_l   = p_sap_l
    iv_sap_t   = p_sap_t
    iv_tsp_e   = p_tsp_e
    iv_conn_o  = p_conn_o
    iv_prox_o  = p_prox_o
    iv_mbt     = abap_true ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file_f.

  p_file_f = zcl_abapinst_screen=>f4_file( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_req_e.

  p_req_e = zcl_abapinst_screen=>f4_transport(
              iv_package = p_pack_t
              iv_layer   = p_layr_t ).
*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/MBT_TEMPLATE_6_START
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  DATA:
    lv_enum_zip          TYPE i,
    lv_name              TYPE char255,
    lv_package           TYPE devclass,
    lv_enum_package      TYPE i,
    lv_enum_transport    TYPE i,
    lv_enum_folder_logic TYPE i.

  CASE abap_true.
    WHEN p_zip_f.
      lv_enum_zip = zcl_abapinst_installer=>ty_enum_zip-local.
      lv_name     = p_file_f.
    WHEN p_zip_i.
      lv_enum_zip = zcl_abapinst_installer=>ty_enum_zip-internet.
      lv_name     = p_file_i.
    WHEN p_zip_s.
      lv_enum_zip = zcl_abapinst_installer=>ty_enum_zip-server.
      lv_name     = p_file_s.
  ENDCASE.

  CASE abap_true.
    WHEN p_sap_d.
      lv_enum_package = zcl_abapinst_installer=>ty_enum_package-default.
    WHEN p_sap_l.
      lv_enum_package = zcl_abapinst_installer=>ty_enum_package-local.
      lv_package      = p_pack_l.
    WHEN p_sap_t.
      lv_enum_package = zcl_abapinst_installer=>ty_enum_package-transportable.
      lv_package      = p_pack_t.
  ENDCASE.

  CASE abap_true.
    WHEN p_tsp_n.
      lv_enum_transport = zcl_abapinst_installer=>ty_enum_transport-prompt.
    WHEN p_tsp_e.
      lv_enum_transport = zcl_abapinst_installer=>ty_enum_transport-existing.
  ENDCASE.

  IF p_conn_o = abap_false.
    CLEAR: p_conn_u, p_conn_p.
  ENDIF.

  IF p_prox_o = abap_false.
    CLEAR: p_prox_h, p_prox_s, p_prox_u, p_prox_p.
  ENDIF.

  CASE abap_true.
    WHEN p_fold_d.
      lv_enum_folder_logic = zcl_abapinst_installer=>ty_enum_folder_logic-default.
    WHEN p_fold_p.
      lv_enum_folder_logic = zcl_abapinst_installer=>ty_enum_folder_logic-prefix.
    WHEN p_fold_f.
      lv_enum_folder_logic = zcl_abapinst_installer=>ty_enum_folder_logic-full.
  ENDCASE.

  zcl_abapinst_screen=>banner( iv_show = abap_false ).

  TRY.
      IF p_drop_n IS INITIAL.

        zcl_abapinst_installer=>install(
          iv_enum_zip          = lv_enum_zip
          iv_name              = lv_name
          iv_enum_package      = lv_enum_package
          iv_package           = lv_package
          iv_dlvunit           = p_soft_t
          iv_devlayer          = p_layr_t
          iv_enum_transport    = lv_enum_transport
          iv_transport         = p_req_e
          iv_user              = p_conn_u
          iv_password          = p_conn_p
          iv_proxy_host        = p_prox_h
          iv_proxy_service     = p_prox_s
          iv_proxy_user        = p_prox_u
          iv_proxy_password    = p_prox_p
          iv_enum_folder_logic = lv_enum_folder_logic ).

      ELSE.

        zcl_abapinst_installer=>uninstall(
          iv_name = p_drop_n
          iv_pack = p_drop_p ).

      ENDIF.
    CATCH zcx_abapinst_exception INTO gx_error.
      MESSAGE gx_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
