REPORT /mbtools/mbt_installer.

************************************************************************
* MBT Installer - Developer Version
*
* This program installs and uninstalls any Marc Bernard Tool
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

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
INTERFACE zif_abapgit_comparator DEFERRED.
INTERFACE zif_abapgit_definitions DEFERRED.
INTERFACE zif_abapgit_dot_abapgit DEFERRED.
INTERFACE zif_abapgit_environment DEFERRED.
INTERFACE zif_abapgit_gui_functions DEFERRED.
INTERFACE zif_abapgit_lang_definitions DEFERRED.
INTERFACE zif_abapgit_log DEFERRED.
INTERFACE zif_abapgit_longtexts DEFERRED.
INTERFACE zif_abapgit_object DEFERRED.
INTERFACE zif_abapgit_objects DEFERRED.
INTERFACE zif_abapgit_object_enho DEFERRED.
INTERFACE zif_abapgit_object_enhs DEFERRED.
INTERFACE zif_abapgit_oo_object_fnc DEFERRED.
INTERFACE zif_abapgit_progress DEFERRED.
INTERFACE zif_abapgit_sap_package DEFERRED.
INTERFACE zif_abapgit_tadir DEFERRED.
INTERFACE zif_abapgit_version DEFERRED.
INTERFACE zif_abapgit_xml_input DEFERRED.
INTERFACE zif_abapgit_xml_output DEFERRED.
CLASS zcl_abapgit_adt_link DEFINITION DEFERRED.
CLASS zcl_abapgit_convert DEFINITION DEFERRED.
CLASS zcl_abapgit_default_transport DEFINITION DEFERRED.
CLASS zcl_abapgit_dependencies DEFINITION DEFERRED.
CLASS zcl_abapgit_dot_abapgit DEFINITION DEFERRED.
CLASS zcl_abapgit_environment DEFINITION DEFERRED.
CLASS zcl_abapgit_folder_logic DEFINITION DEFERRED.
CLASS zcl_abapgit_free_sel_dialog DEFINITION DEFERRED.
CLASS zcl_abapgit_gui_functions DEFINITION DEFERRED.
CLASS zcl_abapgit_hash DEFINITION DEFERRED.
CLASS zcl_abapgit_language DEFINITION DEFERRED.
CLASS zcl_abapgit_log DEFINITION DEFERRED.
CLASS zcl_abapgit_longtexts DEFINITION DEFERRED.
CLASS zcl_abapgit_objects_activation DEFINITION DEFERRED.
CLASS zcl_abapgit_objects_bridge DEFINITION DEFERRED.
CLASS zcl_abapgit_objects_files DEFINITION DEFERRED.
CLASS zcl_abapgit_objects_program DEFINITION DEFERRED.
CLASS zcl_abapgit_objects_super DEFINITION DEFERRED.
CLASS zcl_abapgit_object_acid DEFINITION DEFERRED.
CLASS zcl_abapgit_object_avar DEFINITION DEFERRED.
CLASS zcl_abapgit_object_clas DEFINITION DEFERRED.
CLASS zcl_abapgit_object_devc DEFINITION DEFERRED.
CLASS zcl_abapgit_object_doma DEFINITION DEFERRED.
CLASS zcl_abapgit_object_dtel DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enhc DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho_badi DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho_class DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho_clif DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho_fugr DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho_hook DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho_intf DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho_wdyc DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enho_wdyn DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enhs DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enhs_badi_d DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enhs_hook_d DEFINITION DEFERRED.
CLASS zcl_abapgit_object_enqu DEFINITION DEFERRED.
CLASS zcl_abapgit_object_fugr DEFINITION DEFERRED.
CLASS zcl_abapgit_object_intf DEFINITION DEFERRED.
CLASS zcl_abapgit_object_msag DEFINITION DEFERRED.
CLASS zcl_abapgit_object_para DEFINITION DEFERRED.
CLASS zcl_abapgit_object_prog DEFINITION DEFERRED.
CLASS zcl_abapgit_object_tabl DEFINITION DEFERRED.
CLASS zcl_abapgit_object_tabl_compar DEFINITION DEFERRED.
CLASS zcl_abapgit_object_tobj DEFINITION DEFERRED.
CLASS zcl_abapgit_object_tran DEFINITION DEFERRED.
CLASS zcl_abapgit_object_ttyp DEFINITION DEFERRED.
CLASS zcl_abapgit_object_w3ht DEFINITION DEFERRED.
CLASS zcl_abapgit_object_w3mi DEFINITION DEFERRED.
CLASS zcl_abapgit_object_w3super DEFINITION DEFERRED.
CLASS zcl_abapgit_oo_base DEFINITION DEFERRED.
CLASS zcl_abapgit_oo_class DEFINITION DEFERRED.
CLASS zcl_abapgit_oo_factory DEFINITION DEFERRED.
CLASS zcl_abapgit_oo_interface DEFINITION DEFERRED.
CLASS zcl_abapgit_oo_serializer DEFINITION DEFERRED.
CLASS zcl_abapgit_path DEFINITION DEFERRED.
CLASS zcl_abapgit_progress DEFINITION DEFERRED.
CLASS zcl_abapgit_sap_package DEFINITION DEFERRED.
CLASS zcl_abapgit_skip_objects DEFINITION DEFERRED.
CLASS zcl_abapgit_sotr_handler DEFINITION DEFERRED.
CLASS zcl_abapgit_tadir DEFINITION DEFERRED.
CLASS zcl_abapgit_url DEFINITION DEFERRED.
CLASS zcl_abapgit_version DEFINITION DEFERRED.
CLASS zcl_abapgit_xml DEFINITION DEFERRED.
CLASS zcl_abapgit_xml_input DEFINITION DEFERRED.
CLASS zcl_abapgit_xml_output DEFINITION DEFERRED.
CLASS zcl_abapgit_xml_pretty DEFINITION DEFERRED.
CLASS zcl_abapinst_factory DEFINITION DEFERRED.
CLASS zcl_abapinst_file_status DEFINITION DEFERRED.
CLASS zcl_abapinst_log_viewer DEFINITION DEFERRED.
CLASS zcl_abapinst_objects DEFINITION DEFERRED.
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
      create_package  TYPE abap_bool,
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
      abapgit_install               TYPE string VALUE 'abapgit_install',
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
      go_diff                       TYPE string VALUE 'go_diff',
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
      changed_by                    TYPE string VALUE 'changed_by',
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

ENDINTERFACE.
INTERFACE zif_abapgit_sap_package .

  TYPES: ty_devclass_tt TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY.
  METHODS:
    create
      IMPORTING is_package TYPE scompkdtln
      RAISING   zcx_abapgit_exception,
    create_local
      RAISING zcx_abapgit_exception,
    list_subpackages
      RETURNING VALUE(rt_list) TYPE ty_devclass_tt,
    list_superpackages
      RETURNING VALUE(rt_list) TYPE ty_devclass_tt
      RAISING   zcx_abapgit_exception,
    read_parent
      RETURNING VALUE(rv_parentcl) TYPE tdevc-parentcl
      RAISING   zcx_abapgit_exception,
    create_child
      IMPORTING iv_child TYPE devclass
      RAISING   zcx_abapgit_exception,
    exists
      RETURNING VALUE(rv_bool) TYPE abap_bool,
    are_changes_recorded_in_tr_req
      RETURNING VALUE(rv_are_changes_rec_in_tr_req) TYPE abap_bool
      RAISING   zcx_abapgit_exception,
    get_transport_type
      RETURNING VALUE(rs_transport_type) TYPE zif_abapgit_definitions=>ty_transport_type
      RAISING   zcx_abapgit_exception.

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
ENDINTERFACE.
INTERFACE zif_abapgit_log
   .


  TYPES:
    BEGIN OF ty_log_out,
      type      TYPE symsgty,
      text      TYPE string,
      obj_type  TYPE trobjtype,
      obj_name  TYPE sobj_name,
      exception TYPE REF TO cx_root,
    END OF ty_log_out .
  TYPES:
    ty_log_outs TYPE STANDARD TABLE OF ty_log_out
                WITH NON-UNIQUE DEFAULT KEY .
  TYPES:
    BEGIN OF ty_msg,
      text TYPE string,
      type TYPE symsgty,
    END OF ty_msg .
  TYPES:
    ty_msgs TYPE STANDARD TABLE OF ty_msg
                          WITH NON-UNIQUE DEFAULT KEY .
  TYPES:
    BEGIN OF ty_item_status_out,
      item     TYPE zif_abapgit_definitions=>ty_item,
      status   TYPE symsgty,
      messages TYPE ty_msgs,
    END OF ty_item_status_out .
  TYPES:
    ty_item_status_outs TYPE SORTED TABLE OF ty_item_status_out
                        WITH UNIQUE KEY item-obj_type item-obj_name .

  METHODS add
    IMPORTING
      !iv_msg  TYPE csequence
      !iv_type TYPE symsgty DEFAULT 'E'
      !iv_rc   TYPE balsort OPTIONAL
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
      !iv_rc        TYPE balsort
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
      VALUE(rv_status) TYPE symsgty .
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
      !iv_longtext_name   TYPE string DEFAULT 'LONGTEXTS'
      !ii_xml             TYPE REF TO zif_abapgit_xml_input
      !iv_master_language TYPE langu
    RAISING
      zcx_abapgit_exception .
  METHODS delete
    IMPORTING
      !iv_object_name TYPE sobj_name
      !iv_longtext_id TYPE dokil-id
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
      lead TYPE zif_abapgit_definitions=>ty_deserialization_step VALUE `LEAD`,
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
  TYPES:
    BEGIN OF ty_i18n_params,
      serialize_master_lang_only TYPE abap_bool,
    END OF ty_i18n_params.

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
      iv_serialize_master_lang_only TYPE ty_i18n_params-serialize_master_lang_only OPTIONAL
    RETURNING
      VALUE(rs_params)              TYPE ty_i18n_params.
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
        iv_force                 TYPE abap_bool DEFAULT abap_true
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
      VALUE(rs_tadir) TYPE zif_abapgit_definitions=>ty_tadir
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
INTERFACE zif_abapgit_objects
  .

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
  CONSTANTS gc_abap_version TYPE string VALUE '1.103.0' ##NO_TEXT.

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
*      set_master_language
*        IMPORTING iv_language TYPE spras,
    METHODS get_signature
      RETURNING
        VALUE(rs_signature) TYPE zif_abapgit_definitions=>ty_file_signature
      RAISING
        zcx_abapgit_exception .
    METHODS get_requirements
      RETURNING
        VALUE(rt_requirements) TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt.
    METHODS set_requirements
      IMPORTING
        it_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt.
    METHODS get_packaging
      RETURNING
        VALUE(rt_packaging) TYPE zif_abapgit_dot_abapgit=>ty_packaging.
    METHODS set_packaging
      IMPORTING
        !it_packaging TYPE zif_abapgit_dot_abapgit=>ty_packaging.
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
* master language, this class is used to temporarily change and then
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
        type TYPE symsgty,
      END OF ty_msg .
    TYPES:
      BEGIN OF ty_log, "in order of occurrence
        msg       TYPE ty_msg,
        rc        TYPE balsort,
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
        VALUE(rv_status) TYPE symsgty .
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
      ty_longtexts TYPE STANDARD TABLE OF ty_longtext
                           WITH NON-UNIQUE DEFAULT KEY .

    METHODS read
      IMPORTING
        !iv_object_name      TYPE sobj_name
        !iv_longtext_id      TYPE dokil-id
        !it_dokil            TYPE zif_abapgit_definitions=>ty_dokil_tt
        !iv_master_lang_only TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_longtexts)  TYPE ty_longtexts
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

    CONSTANTS c_docu_state_active TYPE dokstate VALUE 'A' ##NO_TEXT.
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

    CLASS-DATA:
      gt_classes TYPE STANDARD TABLE OF seoclsname WITH DEFAULT KEY .
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
CLASS zcl_abapgit_objects_super DEFINITION  ABSTRACT.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras.

    CLASS-METHODS:
      jump_adt
        IMPORTING iv_obj_name     TYPE zif_abapgit_definitions=>ty_item-obj_name
                  iv_obj_type     TYPE zif_abapgit_definitions=>ty_item-obj_type
                  iv_sub_obj_name TYPE zif_abapgit_definitions=>ty_item-obj_name OPTIONAL
                  iv_sub_obj_type TYPE zif_abapgit_definitions=>ty_item-obj_type OPTIONAL
                  iv_line_number  TYPE i OPTIONAL
        RAISING   zcx_abapgit_exception.

    CONSTANTS: c_user_unknown TYPE xubname VALUE 'UNKNOWN'.

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
      IMPORTING
        !iv_radio TYPE string
        !iv_field TYPE string
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
        VALUE(rv_present) TYPE abap_bool.
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
    DATA ms_i18n_params TYPE zif_abapgit_xml_output~ty_i18n_params .

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
CLASS zcl_abapgit_oo_base DEFINITION  ABSTRACT.
  PUBLIC SECTION.
    INTERFACES: zif_abapgit_oo_object_fnc.

  PROTECTED SECTION.
    CLASS-METHODS:
      convert_attrib_to_vseoattrib
        IMPORTING iv_clsname           TYPE seoclsname
                  it_attributes        TYPE zif_abapgit_definitions=>ty_obj_attribute_tt
        RETURNING VALUE(rt_vseoattrib) TYPE seoo_attributes_r.

  PRIVATE SECTION.
    DATA mv_skip_test_classes TYPE abap_bool.

    METHODS deserialize_abap_source_old
      IMPORTING is_clskey TYPE seoclskey
                it_source TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING   zcx_abapgit_exception.

    METHODS deserialize_abap_source_new
      IMPORTING is_clskey TYPE seoclskey
                it_source TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING   zcx_abapgit_exception
                cx_sy_dyn_call_error.
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
ENDCLASS.
CLASS zcl_abapgit_object_clas DEFINITION  INHERITING FROM zcl_abapgit_objects_program.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.
    METHODS: constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.

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
CLASS zcl_abapgit_object_doma DEFINITION

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
CLASS zcl_abapgit_object_para DEFINITION  INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.
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
      END OF ty_dd02_text,
      ty_dd02_texts TYPE STANDARD TABLE OF ty_dd02_text.

    CONSTANTS c_longtext_id_tabl TYPE dokil-id VALUE 'TB' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF c_s_dataname,
        segment_definition TYPE string VALUE 'SEGMENT_DEFINITION',
        tabl_extras        TYPE string VALUE 'TABL_EXTRAS',
      END OF c_s_dataname.


    METHODS clear_dd03p_fields
      CHANGING
        !ct_dd03p TYPE ty_dd03p_tt .
    "! Check if structure is an IDoc segment
    "! @parameter rv_is_idoc_segment | It's an IDoc segment or not
    METHODS is_idoc_segment
      RETURNING
        VALUE(rv_is_idoc_segment) TYPE abap_bool.
    METHODS clear_dd03p_fields_common
      CHANGING
        !cs_dd03p TYPE dd03p .
    METHODS clear_dd03p_fields_dataelement
      CHANGING
        !cs_dd03p TYPE dd03p .

    METHODS:
      serialize_texts
        IMPORTING io_xml TYPE REF TO zif_abapgit_xml_output
        RAISING   zcx_abapgit_exception,
      deserialize_texts
        IMPORTING io_xml   TYPE REF TO zif_abapgit_xml_input
                  is_dd02v TYPE dd02v
        RAISING   zcx_abapgit_exception,
      is_db_table_category
        IMPORTING iv_tabclass                TYPE dd02l-tabclass
        RETURNING VALUE(rv_is_db_table_type) TYPE dd02l-tabclass.

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
CLASS zcl_abapgit_object_w3super DEFINITION  INHERITING FROM zcl_abapgit_objects_super ABSTRACT.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    TYPES ty_wwwparams_tt TYPE STANDARD TABLE OF wwwparams WITH DEFAULT KEY.

    CONSTANTS: BEGIN OF c_param_names,
                 version  TYPE w3_name VALUE 'version',
                 fileext  TYPE w3_name VALUE 'fileextension',
                 filesize TYPE w3_name VALUE 'filesize',
                 filename TYPE w3_name VALUE 'filename',
                 mimetype TYPE w3_name VALUE 'mimetype',
               END OF c_param_names.

    METHODS constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.

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
CLASS zcl_abapgit_object_w3ht DEFINITION  INHERITING FROM zcl_abapgit_object_w3super FINAL.

  PROTECTED SECTION.
    METHODS: change_bdc_jump_data REDEFINITION.
ENDCLASS.
CLASS zcl_abapgit_object_w3mi DEFINITION  INHERITING FROM zcl_abapgit_object_w3super FINAL.

  PROTECTED SECTION.
    METHODS: change_bdc_jump_data REDEFINITION.
ENDCLASS.
CLASS zcl_abapgit_oo_interface DEFINITION
  INHERITING FROM zcl_abapgit_oo_base.
  PUBLIC SECTION.
    METHODS:
      zif_abapgit_oo_object_fnc~create REDEFINITION,
      zif_abapgit_oo_object_fnc~get_includes REDEFINITION,
      zif_abapgit_oo_object_fnc~get_interface_properties REDEFINITION,
      zif_abapgit_oo_object_fnc~delete REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
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
        !iv_top                TYPE tadir-devclass
        !io_dot                TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_ignore_subpackages TYPE abap_bool DEFAULT abap_false
        !iv_only_local_objects TYPE abap_bool
        !ii_log                TYPE REF TO zif_abapgit_log OPTIONAL
      RETURNING
        VALUE(rt_tadir)        TYPE zif_abapgit_definitions=>ty_tadir_tt
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
ENDCLASS.
CLASS zcl_abapinst_file_status DEFINITION

  FINAL
  CREATE PUBLIC .

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
ENDCLASS.
CLASS zcl_abapinst_log_viewer DEFINITION

  FINAL
  CREATE PUBLIC .

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

  PUBLIC SECTION.

    TYPES:
      ty_types_tt TYPE SORTED TABLE OF tadir-object WITH UNIQUE KEY table_line .
    TYPES:
      BEGIN OF ty_serialization,
        files TYPE zif_abapgit_definitions=>ty_files_tt,
        item  TYPE zif_abapgit_definitions=>ty_item,
      END OF ty_serialization .

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
      END OF ty_obj_serializer_item.
    TYPES:
      ty_obj_serializer_map
              TYPE SORTED TABLE OF ty_obj_serializer_item WITH UNIQUE KEY item.

    CLASS-DATA gt_obj_serializer_map TYPE ty_obj_serializer_map.

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
        zcx_abapgit_exception.
    CLASS-METHODS prioritize_deser
      IMPORTING
        !it_results       TYPE zif_abapgit_definitions=>ty_results_tt
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt.
    CLASS-METHODS class_name
      IMPORTING
        !is_item             TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_class_name) TYPE string.
    CLASS-METHODS update_package_tree
      IMPORTING
        !iv_package TYPE devclass.
    CLASS-METHODS delete_obj
      IMPORTING
        !iv_package TYPE devclass
        !is_item    TYPE zif_abapgit_definitions=>ty_item
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS compare_remote_to_local
      IMPORTING
        !ii_object TYPE REF TO zif_abapgit_object
        !it_remote TYPE zif_abapgit_definitions=>ty_files_tt
        !is_result TYPE zif_abapgit_definitions=>ty_result
        !ii_log    TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS deserialize_objects
      IMPORTING
        !is_step  TYPE zif_abapgit_objects=>ty_step_data
        !ii_log   TYPE REF TO zif_abapgit_log
      CHANGING
        !ct_files TYPE zif_abapgit_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS check_objects_locked
      IMPORTING
        !iv_language TYPE spras
        !it_items    TYPE zif_abapgit_definitions=>ty_items_tt
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS create_object
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_language    TYPE spras
        !is_metadata    TYPE zif_abapgit_definitions=>ty_metadata OPTIONAL
        !iv_native_only TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_obj)   TYPE REF TO zif_abapgit_object
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS map_tadir_to_items
      IMPORTING
        !it_tadir       TYPE zif_abapgit_definitions=>ty_tadir_tt
      RETURNING
        VALUE(rt_items) TYPE zif_abapgit_definitions=>ty_items_tt.
    CLASS-METHODS map_results_to_items
      IMPORTING
        !it_results     TYPE zif_abapgit_definitions=>ty_results_tt
      RETURNING
        VALUE(rt_items) TYPE zif_abapgit_definitions=>ty_items_tt.
    CLASS-METHODS filter_files_to_deserialize
      IMPORTING
        !it_results       TYPE zif_abapgit_definitions=>ty_results_tt
        !ii_log           TYPE REF TO zif_abapgit_log OPTIONAL
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt.
    CLASS-METHODS adjust_namespaces
      IMPORTING
        !it_results       TYPE zif_abapgit_definitions=>ty_results_tt
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt.
    CLASS-METHODS get_deserialize_steps
      RETURNING
        VALUE(rt_steps) TYPE zif_abapgit_objects=>ty_step_data_tt.
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



CLASS ZCL_ABAPGIT_DEFAULT_TRANSPORT IMPLEMENTATION.


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
      zcx_abapgit_exception=>raise( |Error from TR_TASK_RESET. Subrc = { sy-subrc }| ).
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
      zcx_abapgit_exception=>raise( |Error from TR_TASK_GET. Subrc = { sy-subrc }| ).
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
      zcx_abapgit_exception=>raise( |Error from TR_TASK_SET. Subrc = { sy-subrc }| ).
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
      zcx_abapgit_exception=>raise( |Error from TR_TASK_SET. Subrc = { sy-subrc }| ).
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


  METHOD get_master_language.
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



CLASS ZCL_ABAPGIT_ENVIRONMENT IMPLEMENTATION.


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

    IF iv_top(1) = '$' AND iv_create_if_not_exists = abap_true.
      zcl_abapinst_factory=>get_sap_package( iv_top )->create_local( ).
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



CLASS ZCL_ABAPGIT_LANGUAGE IMPLEMENTATION.


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



CLASS ZCL_ABAPGIT_LONGTEXTS IMPLEMENTATION.


  METHOD read.

    DATA: ls_longtext TYPE ty_longtext,
          lt_dokil    TYPE zif_abapgit_definitions=>ty_dokil_tt.

    FIELD-SYMBOLS: <ls_dokil> LIKE LINE OF lt_dokil.

    IF lines( it_dokil ) > 0.

      lt_dokil = it_dokil.

    ELSEIF iv_longtext_id IS NOT INITIAL.
      IF iv_master_lang_only = abap_true.
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

      CLEAR: ls_longtext-head-tdfuser,
             ls_longtext-head-tdfreles,
             ls_longtext-head-tdfdate,
             ls_longtext-head-tdftime,
             ls_longtext-head-tdluser,
             ls_longtext-head-tdlreles,
             ls_longtext-head-tdldate,
             ls_longtext-head-tdltime.

      INSERT ls_longtext INTO TABLE rt_longtexts.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_longtexts~changed_by.

    DATA: lt_longtexts TYPE ty_longtexts.
    FIELD-SYMBOLS: <ls_longtext> TYPE ty_longtext.

    lt_longtexts = read( iv_object_name = iv_object_name
                         iv_longtext_id = iv_longtext_id
                         it_dokil       = it_dokil ).

    READ TABLE lt_longtexts INDEX 1 ASSIGNING <ls_longtext>.
    IF sy-subrc = 0.
      rv_user = <ls_longtext>-head-tdluser.
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

    DATA: lt_longtexts     TYPE ty_longtexts,
          lv_no_masterlang TYPE dokil-masterlang.
    FIELD-SYMBOLS: <ls_longtext> TYPE ty_longtext.

    ii_xml->read(
      EXPORTING
        iv_name = iv_longtext_name
      CHANGING
        cg_data = lt_longtexts ).

    LOOP AT lt_longtexts ASSIGNING <ls_longtext>.

      lv_no_masterlang = boolc( iv_master_language <> <ls_longtext>-dokil-langu ).

      CALL FUNCTION 'DOCU_UPDATE'
        EXPORTING
          head          = <ls_longtext>-head
          state         = c_docu_state_active
          typ           = <ls_longtext>-dokil-typ
          version       = <ls_longtext>-dokil-version
          no_masterlang = lv_no_masterlang
        TABLES
          line          = <ls_longtext>-lines.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_longtexts~serialize.

    DATA lt_longtexts TYPE ty_longtexts.
    DATA lt_dokil LIKE it_dokil.
    DATA lv_master_lang_only TYPE abap_bool.

    lt_dokil = it_dokil.
    lv_master_lang_only = ii_xml->i18n_params( )-serialize_master_lang_only.
    IF lv_master_lang_only = abap_true.
      DELETE lt_dokil WHERE masterlang <> abap_true.
    ENDIF.

    lt_longtexts = read( iv_object_name = iv_object_name
                         iv_longtext_id = iv_longtext_id
                         it_dokil       = lt_dokil
                         iv_master_lang_only = lv_master_lang_only ).

    ii_xml->add( iv_name = iv_longtext_name
                 ig_data = lt_longtexts ).

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECTS_ACTIVATION IMPLEMENTATION.


  METHOD activate.

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
        zcx_abapgit_exception=>raise( 'error from DD_MASS_ACT_C3' ).
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

    FIELD-SYMBOLS: <ls_object> TYPE dwinactiv.

    IF iv_type = 'CLAS'.
      APPEND iv_name TO gt_classes.
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
      zcx_abapgit_exception=>raise( 'error from TR_READ_LOG' ).
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

    DATA: lv_class    LIKE LINE OF gt_classes,
          lo_cross    TYPE REF TO cl_wb_crossreference,
          lv_include  TYPE programm,
          li_progress TYPE REF TO zif_abapgit_progress.


    li_progress = zcl_abapgit_progress=>get_instance( lines( gt_classes ) ).

    LOOP AT gt_classes INTO lv_class.
      IF sy-tabix MOD 20 = 0.
        li_progress->show(
          iv_current = sy-tabix
          iv_text    = 'Updating where-used lists' ).
      ENDIF.

      lv_include = cl_oo_classname_service=>get_classpool_name( lv_class ).

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
        ii_xml             = ii_xml
        iv_master_language = mv_language ).

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
      zcx_abapgit_exception=>raise( |Error from TR_TADIR_INTERFACE (subrc={ sy-subrc } ).| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_objects_bridge IMPLEMENTATION.


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
      AND version = '1'.                                  "#EC CI_SUBRC

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
    mt_files = it_files.
  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_xml_output IMPLEMENTATION.


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


  METHOD zif_abapgit_xml_output~i18n_params.

    IF iv_serialize_master_lang_only IS SUPPLIED.
      ms_i18n_params-serialize_master_lang_only = iv_serialize_master_lang_only.
    ENDIF.

    rs_params = ms_i18n_params.

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
      zcx_abapgit_exception=>raise( |Error from RS_CUA_INTERNAL_WRITE. Subrc = { sy-subrc }| ).
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
    IF sy-subrc = 1.
      zcx_abapgit_exception=>raise( |Error from RS_CORR_INSERT, Cancelled, { sy-msgid }, { sy-msgno }| ).
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from RS_CORR_INSERT, { sy-msgid }, { sy-msgno }| ).
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
          zcx_abapgit_exception=>raise( |PROG { is_progdir-name }, updating error: { sy-msgid } { sy-msgno }| ).
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
        zcx_abapgit_exception=>raise( 'error from INSERT REPORT' ).
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
      zcx_abapgit_exception=>raise( |Error from RS_CUA_INTERNAL_FETCH, { sy-subrc }| ).
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
      zcx_abapgit_exception=>raise( |Error from RS_SCREEN_LIST. Subrc = { sy-subrc }| ).
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
        zcx_abapgit_exception=>raise( |Error while reading dynpro: { sy-subrc }| ).
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
      zcx_abapgit_exception=>raise( |Error reading program with RPY_PROGRAM_READ. Subrc = { sy-subrc }| ).
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



CLASS ZCL_ABAPGIT_OBJECT_ACID IMPLEMENTATION.


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
      zcx_abapgit_exception=>raise( 'error creating CL_AAB_ID object' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
* looks like "changed by user" is not stored in the database
    rv_user = c_user_unknown.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lo_aab TYPE REF TO cl_aab_id.


    lo_aab = create_object( ).
    lo_aab->enqueue( ).
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
      zcx_abapgit_exception=>raise( 'error deleting ACID object' ).
    ENDIF.
    lo_aab->dequeue( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_description TYPE aab_id_descript,
          lo_aab         TYPE REF TO cl_aab_id.


    io_xml->read( EXPORTING iv_name = 'DESCRIPTION'
                  CHANGING cg_data = lv_description ).

    lo_aab = create_object( ).
    lo_aab->enqueue( ).
    lo_aab->set_descript( lv_description ).
    tadir_insert( iv_package ).
    lo_aab->save( ).

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
      zcx_abapgit_exception=>raise( |No authorization for { ls_is-object } { ls_is-name }| ).
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
          id_not_transportable = 3 ).
      CASE sy-subrc.
        WHEN 1.
          zcx_abapgit_exception=>raise( |No authorization for { ls_is-object } { ls_is-name }| ).
        WHEN 2.
          zcx_abapgit_exception=>raise( |{ ls_is-object } { ls_is-name } does not exist| ).
        WHEN 3.
          zcx_abapgit_exception=>raise( |{ ls_is-object } { ls_is-name } must be transportable| ).
      ENDCASE.
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
      zcx_abapgit_exception=>raise( |Error saving AVAR { ms_item-obj_name }| ).
    ENDIF.

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


  METHOD deserialize_abap_source_new.
    DATA: lo_factory  TYPE REF TO object,
          lo_source   TYPE REF TO object,
          lo_settings TYPE REF TO object,
          lr_settings TYPE REF TO data.

    FIELD-SYMBOLS <lg_settings> TYPE any.


    "Buffer needs to be refreshed,
    "otherwise standard SAP CLIF_SOURCE reorder methods alphabetically
    CALL FUNCTION 'SEO_BUFFER_INIT'.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        cifkey  = is_clskey
        version = seoc_version_inactive.

    CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
      RECEIVING
        result = lo_factory.

    "Enable modification mode to avoid exception CX_OO_ACCESS_PERMISSON when
    "dealing with objects in foreign namespaces (namespace role = C)
    CALL METHOD lo_factory->('CREATE_SETTINGS')
      EXPORTING
        modification_mode_enabled = abap_true
      RECEIVING
        result                    = lo_settings.

    CREATE DATA lr_settings TYPE REF TO ('IF_OO_CLIF_SOURCE_SETTINGS').
    ASSIGN lr_settings->* TO <lg_settings>.

    <lg_settings> ?= lo_settings.

    CALL METHOD lo_factory->('CREATE_CLIF_SOURCE')
      EXPORTING
        clif_name = is_clskey-clsname
        settings  = <lg_settings>
      RECEIVING
        result    = lo_source.

    TRY.
        CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~LOCK').
      CATCH cx_oo_access_permission.
        zcx_abapgit_exception=>raise( 'source_new, access permission exception' ).
    ENDTRY.

    CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~SET_SOURCE')
      EXPORTING
        source = it_source.

    CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~SAVE').

    CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~UNLOCK').

  ENDMETHOD.


  METHOD deserialize_abap_source_old.
    "for backwards compatability down to 702

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

    TRY.
        lo_source->access_permission( seok_access_modify ).
        lo_source->set_source( it_source ).
        lo_source->save( ).
        lo_source->access_permission( seok_access_free ).
      CATCH cx_oo_access_permission.
        zcx_abapgit_exception=>raise( 'permission error' ).
      CATCH cx_oo_source_save_failure.
        zcx_abapgit_exception=>raise( 'save failure' ).
    ENDTRY.

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
    TRY.
        deserialize_abap_source_new(
          is_clskey = is_key
          it_source = it_source ).
      CATCH cx_sy_dyn_call_error.
        deserialize_abap_source_old(
          is_clskey = is_key
          it_source = it_source ).
    ENDTRY.
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

* TODO, perhaps move this call to somewhere else, to be done while cleaning up the CLAS deserialization
    zcl_abapgit_objects_activation=>add(
      iv_type = 'CLAS'
      iv_name = is_key-clsname ).

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

    IF lines( it_local_test_classes ) > 0.
      lv_program = cl_oo_classname_service=>get_ccau_name( is_key-clsname ).
      update_report( iv_program = lv_program
                     it_source  = it_local_test_classes ).
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



CLASS ZCL_ABAPGIT_OBJECT_CLAS IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    CREATE OBJECT mi_object_oriented_object_fct TYPE zcl_abapgit_oo_class.

    mv_classpool_name = cl_oo_classname_service=>get_classpool_name( |{ is_item-obj_name }| ).

  ENDMETHOD.


  METHOD deserialize_abap.
* same as in zcl_abapgit_object_clas, but without "mo_object_oriented_object_fct->add_to_activation_list"

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

    lt_local_definitions = mo_files->read_abap( iv_extra = 'locals_def'
                                                iv_error = abap_false ).

    lt_local_implementations = mo_files->read_abap( iv_extra = 'locals_imp'
                                                    iv_error = abap_false ).

    lt_local_macros = mo_files->read_abap( iv_extra = 'macros'
                                           iv_error = abap_false ).

    lt_test_classes = mo_files->read_abap( iv_extra = 'testclasses'
                                           iv_error = abap_false ).

    ls_class_key-clsname = ms_item-obj_name.

    ii_xml->read( EXPORTING iv_name = 'VSEOCLASS'
                  CHANGING  cg_data = ls_vseoclass ).

    ii_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING  cg_data = lt_attributes ).

    mi_object_oriented_object_fct->create(
      EXPORTING
        iv_package    = iv_package
        it_attributes = lt_attributes
      CHANGING
        cg_properties = ls_vseoclass ).

    mi_object_oriented_object_fct->generate_locals(
      is_key                   = ls_class_key
      iv_force                 = abap_true
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

    IF ii_xml->i18n_params( )-serialize_master_lang_only = abap_true.
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

    IF ii_xml->i18n_params( )-serialize_master_lang_only IS NOT INITIAL.
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

    IF ii_xml->i18n_params( )-serialize_master_lang_only IS NOT INITIAL.
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

    " If a method was moved to an interface, abapGit does not remove the old
    " method include and it's necessary to repair the class (#3833)
    " TODO: Remove 2020-11 or replace with general solution
    IF ms_item-obj_name = 'ZCX_ABAPGIT_EXCEPTION'.
      ls_clskey-clsname = ms_item-obj_name.

      CALL FUNCTION 'SEO_CLASS_REPAIR_CLASSPOOL'
        EXPORTING
          clskey       = ls_clskey
        EXCEPTIONS
          not_existing = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error repairing class { ms_item-obj_name }| ).
      ENDIF.
    ENDIF.

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
      mo_files->add_abap( iv_extra = 'locals_def'
                          it_abap  = lt_source ).
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_locals_imp ).
    IF lines( lt_source ) > 0.
      mo_files->add_abap( iv_extra = 'locals_imp'
                          it_abap  = lt_source ).
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key            = ls_class_key
      iv_type                 = seop_ext_class_testclasses ).

    mv_skip_testclass = mi_object_oriented_object_fct->get_skip_test_classes( ).
    IF lines( lt_source ) > 0 AND mv_skip_testclass = abap_false.
      mo_files->add_abap( iv_extra = 'testclasses'
                          it_abap  = lt_source ).
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_macros ).
    IF lines( lt_source ) > 0.
      mo_files->add_abap( iv_extra = 'macros'
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

    SELECT SINGLE obj_name
           FROM tadir
           INTO lv_object_name
           WHERE pgmid = 'R3TR'
           AND NOT ( object = 'DEVC' AND obj_name = iv_package_name )
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
    SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE lt_tadir WHERE devclass = iv_package_name.

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
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    ii_package->set_permissions_changeable(
      EXPORTING
        i_changeable                = iv_lock
* downport, does not exist in 7.30. Let's see if we can get along without it
*        i_suppress_dialog           = abap_true
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

      TRY.
          CALL METHOD li_package->('SET_CHANGEABLE')
            EXPORTING
              i_changeable                = abap_true
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

        CATCH cx_root.
          li_package->set_changeable(
            EXPORTING
              i_changeable                = abap_true
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

        CATCH cx_root.

          li_package->delete(
            EXCEPTIONS
              object_not_empty      = 1
              object_not_changeable = 2
              object_invalid        = 3
              intern_err            = 4
              OTHERS                = 5 ).

      ENDTRY.

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      li_package->save(
        EXPORTING
          i_suppress_dialog     = abap_true
        EXCEPTIONS
          object_invalid        = 1
          object_not_changeable = 2
          cancelled_in_corr     = 3
          permission_failure    = 4
          unexpected_error      = 5
          intern_err            = 6
          OTHERS                = 7 ).

      IF sy-subrc <> 0.
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
                iv_lock = abap_true ).

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
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    set_lock( ii_package = li_package
              iv_lock = abap_false ).
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
    APPEND zif_abapgit_object=>gc_step_id-lead TO rt_steps.
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
        zcx_abapgit_exception=>raise( 'error from DDIF_DOMA_PUT @TEXTS' ).
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

    IF ii_xml->i18n_params( )-serialize_master_lang_only = abap_true.
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
      zcx_abapgit_exception=>raise( 'error from DDIF_DOMA_PUT' ).
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

    jump_se11( iv_radio = 'RSRD1-DOMA'
               iv_field = 'RSRD1-DOMA_VAL' ).

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
    IF sy-subrc <> 0 OR ls_dd01v IS INITIAL.
      zcx_abapgit_exception=>raise( 'error from DDIF_DOMA_GET' ).
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
        zcx_abapgit_exception=>raise( |error from DDIF_DTEL_PUT @TEXTS, { sy-subrc }| ).
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

    IF ii_xml->i18n_params( )-serialize_master_lang_only = abap_true.
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
      zcx_abapgit_exception=>raise( |error from DDIF_DTEL_PUT, { sy-subrc }| ).
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

    jump_se11( iv_radio = 'RSRD1-DDTYPE'
               iv_field = 'RSRD1-DDTYPE_VAL' ).

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
      zcx_abapgit_exception=>raise( 'Not found in DD04L' ).
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



CLASS ZCL_ABAPGIT_OBJECT_ENHC IMPLEMENTATION.


  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_composite_id = ms_item-obj_name.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    rv_user = c_user_unknown.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lx_error      TYPE REF TO cx_enh_root,
          li_enh_object TYPE REF TO if_enh_object.

    TRY.
        li_enh_object = cl_enh_factory=>load_enhancement_composite(
          name = mv_composite_id
          lock = abap_true ).

        li_enh_object->delete( ).
        li_enh_object->save( ).
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
          lx_enh         TYPE REF TO cx_enh_root.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF lt_tab_methods.

    ii_xml->read( EXPORTING iv_name = 'TAB_METHODS'
                  CHANGING cg_data = lt_tab_methods ).

    LOOP AT lt_tab_methods ASSIGNING <ls_method>.

      lv_editorder = <ls_method>-meth_header-editorder.
      lv_methname = <ls_method>-methkey-cmpname.
      lt_abap = mo_files->read_abap( iv_extra = 'em' && lv_editorder ).

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
        mo_files->add_abap( iv_extra = |EM{ <ls_include>-includenr }|
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
          lv_package TYPE devclass.


    ii_xml->read( EXPORTING iv_name = 'ORIGINAL_OBJECT'
                  CHANGING cg_data  = ls_obj ).

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

* todo
* io_xml->read_xml()
* CL_WDR_CFG_PERSISTENCE_UTILS=>COMP_XML_TO_TABLES( )
* lo_wdyconf->set_enhancement_data( )
        ASSERT 0 = 1.

        lo_wdyconf->if_enh_object~save( run_dark = abap_true ).
        lo_wdyconf->if_enh_object~unlock( ).
      CATCH cx_enh_root.
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



CLASS ZCL_ABAPGIT_OBJECT_ENHO IMPLEMENTATION.


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
        li_enh_object->delete( ).
        li_enh_object->save( run_dark = abap_true ).
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
      io_xml      = io_xml ).

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
          lt_tab_eventdata  TYPE enhevent_tab.

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

    LOOP AT lt_tab_methods ASSIGNING <ls_meth>.
      CLEAR: <ls_meth>-meth_header-author,
             <ls_meth>-meth_header-createdon,
             <ls_meth>-meth_header-changedby,
             <ls_meth>-meth_header-changedon,
             <ls_meth>-meth_header-descript_id.
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
      io_xml      = io_xml ).

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
      zcx_abapgit_exception=>raise( 'error from DDIF_ENQU_PUT' ).
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

    jump_se11( iv_radio = 'RSRD1-ENQU'
               iv_field = 'RSRD1-ENQU_VAL' ).

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
      zcx_abapgit_exception=>raise( 'error from DDIF_ENQU_GET' ).
    ENDIF.
    IF ls_dd25v IS INITIAL.
      RETURN. " does not exist in system
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
      zcx_abapgit_exception=>raise( 'error from FUNCTION_INCLUDE_SPLIT' ).
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
        zcx_abapgit_exception=>raise( |error from RS_FUNCTION_POOL_INSERT, code: { sy-subrc }| ).
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
      zcx_abapgit_exception=>raise( |Error from RS_FUNCTION_POOL_CONTENTS for { lv_area }| ).
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
      zcx_abapgit_exception=>raise( 'Error from FUNCTION_INCLUDE_SPLIT' ).
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

    IF ii_xml->i18n_params( )-serialize_master_lang_only = abap_true.
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
      zcx_abapgit_exception=>raise( 'error from RS_FUNCTION_POOL_DELETE' ).
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

    serialize_texts( iv_prog_name = lv_program_name
                     ii_xml       = io_xml ).

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



CLASS ZCL_ABAPGIT_OBJECT_INTF IMPLEMENTATION.


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

    IF io_xml->i18n_params( )-serialize_master_lang_only = abap_true.
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
          ls_dokil            LIKE LINE OF lt_dokil,
          lv_master_lang_only TYPE abap_bool.

    FIELD-SYMBOLS: <ls_t100>  TYPE t100.

    IF lines( it_t100 ) = 0.
      RETURN.
    ENDIF.

    LOOP AT it_t100 ASSIGNING <ls_t100>.

      lv_doku_object_name = <ls_t100>-arbgb && <ls_t100>-msgnr.
      INSERT lv_doku_object_name INTO TABLE lt_doku_object_names.

    ENDLOOP.

    lv_master_lang_only = ii_xml->i18n_params( )-serialize_master_lang_only.
    IF lv_master_lang_only = abap_true.
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

    IF ii_xml->i18n_params( )-serialize_master_lang_only = abap_true.
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
      zcx_abapgit_exception=>raise( 'Error from (copy of) RS_DELETE_MESSAGE_ID' )."can't access
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
      zcx_abapgit_exception=>raise( 'Error from (copy of) RS_DELETE_MESSAGE_ID' )."can't access
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



CLASS ZCL_ABAPGIT_OBJECT_PARA IMPLEMENTATION.


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
      zcx_abapgit_exception=>raise( 'PARA: Parameter is still used' ).
    ELSE.
      SELECT COUNT(*) FROM dd04l BYPASSING BUFFER
        WHERE memoryid = lv_paramid
        AND as4local = 'A'.
      IF sy-subrc = 0.
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
      zcx_abapgit_exception=>raise( 'error from RS_CORR_INSERT' ).
    ENDIF.

    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        mode         = 'FREE'
        object       = lv_paramid
        object_class = 'PARA'.

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
      zcx_abapgit_exception=>raise( 'error from RS_CORR_INSERT, PARA' ).
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

    IF ii_xml->i18n_params( )-serialize_master_lang_only = abap_true.
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

    DATA: lv_program LIKE sy-repid.

    lv_program = ms_item-obj_name.

    CALL FUNCTION 'RS_DELETE_PROGRAM'
      EXPORTING
        program                    = lv_program
        suppress_popup             = abap_true
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
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from RS_DELETE_PROGRAM: { sy-subrc }| ).
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
    serialize_texts( io_xml ).

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

    ii_remote_version->read(
      EXPORTING
        iv_name = 'DD02V'
      CHANGING
        cg_data = ls_dd02v ).

    " We only want to compare transparent tables, or structures used in transparent tables
    IF ls_dd02v-tabclass <> 'TRANSP' AND is_structure_used_in_db_table( ls_dd02v-tabname ) = abap_false.
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
      zcx_abapgit_exception=>raise( 'error from TR_TADIR_INTERFACE' ).
    ENDIF.
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
        zcx_abapgit_exception=>raise( |error from DDIF_TABL_PUT @TEXTS, { sy-subrc }| ).
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

    IF io_xml->i18n_params( )-serialize_master_lang_only = abap_true.
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
      delete_extras( iv_tabname = iv_tabname ).
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

      delete_extras( iv_tabname = lv_objname ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_name      TYPE ddobjname,
          lv_tname     TYPE trobj_name,
          ls_dd02v     TYPE dd02v,
          ls_dd09l     TYPE dd09l,
          lt_dd03p     TYPE TABLE OF dd03p,
          lt_dd05m     TYPE TABLE OF dd05m,
          lt_dd08v     TYPE TABLE OF dd08v,
          lt_dd12v     TYPE dd12vtab,
          lt_dd17v     TYPE dd17vtab,
          ls_dd17v     LIKE LINE OF lt_dd17v,
          lt_secondary LIKE lt_dd17v,
          lt_dd35v     TYPE TABLE OF dd35v,
          lt_dd36m     TYPE dd36mttyp,
          ls_dd12v     LIKE LINE OF lt_dd12v,
          lv_refs      TYPE abap_bool,
          ls_extras    TYPE ty_tabl_extras.

    FIELD-SYMBOLS: <ls_dd03p> TYPE dd03p.

    IF deserialize_idoc_segment( io_xml     = io_xml
                                 iv_package = iv_package ) = abap_false.

      io_xml->read( EXPORTING iv_name = 'DD02V'
                    CHANGING cg_data = ls_dd02v ).
      io_xml->read( EXPORTING iv_name = 'DD09L'
                    CHANGING cg_data = ls_dd09l ).
      io_xml->read( EXPORTING iv_name  = 'DD03P_TABLE'
                    CHANGING cg_data = lt_dd03p ).

      " DDIC Step: Replace REF TO class/interface with generic reference to avoid cyclic dependency
      LOOP AT lt_dd03p ASSIGNING <ls_dd03p> WHERE datatype = 'REF'.
        IF iv_step = zif_abapgit_object=>gc_step_id-ddic.
          <ls_dd03p>-rollname = 'OBJECT'.
        ELSE.
          lv_refs = abap_true.
        ENDIF.
      ENDLOOP.

      io_xml->read( EXPORTING iv_name = 'DD05M_TABLE'
                    CHANGING cg_data = lt_dd05m ).
      io_xml->read( EXPORTING iv_name = 'DD08V_TABLE'
                    CHANGING cg_data = lt_dd08v ).
      io_xml->read( EXPORTING iv_name = 'DD12V'
                    CHANGING cg_data = lt_dd12v ).
      io_xml->read( EXPORTING iv_name = 'DD17V'
                    CHANGING cg_data = lt_dd17v ).
      io_xml->read( EXPORTING iv_name = 'DD35V_TALE'
                    CHANGING cg_data = lt_dd35v ).
      io_xml->read( EXPORTING iv_name = 'DD36M'
                    CHANGING cg_data = lt_dd36m ).

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

      lv_name = ms_item-obj_name. " type conversion

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
        zcx_abapgit_exception=>raise( 'error from DDIF_TABL_PUT' ).
      ENDIF.

      zcl_abapgit_objects_activation=>add_item( ms_item ).

* handle indexes
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
          zcx_abapgit_exception=>raise( 'error from DDIF_INDX_PUT' ).
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

    jump_se11( iv_radio = 'RSRD1-DDTYPE'
               iv_field = 'RSRD1-DDTYPE_VAL' ).

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
      RETURN. " object does not exits
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
      lv_index = sy-tabix.
      READ TABLE lt_dd08v WITH KEY fieldname = <ls_dd05m>-fieldname TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        DELETE lt_dd05m INDEX lv_index.
      ENDIF.
    ENDLOOP.

* remove inherited search helps
    DELETE lt_dd35v WHERE shlpinher = abap_true.
    LOOP AT lt_dd36m ASSIGNING <ls_dd36m>.
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

    ls_extras = read_extras( iv_tabname = lv_name ).
    io_xml->add( iv_name = c_s_dataname-tabl_extras
                 ig_data = ls_extras ).

  ENDMETHOD.
ENDCLASS.



CLASS zcl_abapgit_object_tobj IMPLEMENTATION.


  METHOD delete_extra.

    DELETE FROM tddat WHERE tabname = iv_tabname.
    DELETE FROM tvdir WHERE tabname = iv_tabname.
    DELETE FROM tvimf WHERE tabname = iv_tabname.

  ENDMETHOD.


  METHOD read_extra.

    SELECT SINGLE * FROM tddat INTO rs_tobj-tddat WHERE tabname = iv_tabname.

    SELECT SINGLE * FROM tvdir INTO rs_tobj-tvdir WHERE tabname = iv_tabname.
    CLEAR: rs_tobj-tvdir-gendate, rs_tobj-tvdir-gentime, rs_tobj-tvdir-devclass.

    SELECT * FROM tvimf INTO TABLE rs_tobj-tvimf WHERE tabname = iv_tabname.

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
      zcx_abapgit_exception=>raise( 'error from CTO_OBJECT_GET' ).
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

    IF io_xml->i18n_params( )-serialize_master_lang_only = abap_true.
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



CLASS ZCL_ABAPGIT_OBJECT_TTYP IMPLEMENTATION.


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

    jump_se11( iv_radio = 'RSRD1-DDTYPE'
               iv_field = 'RSRD1-DDTYPE_VAL' ).

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
      RETURN. " does not exist in system
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



CLASS ZCL_ABAPGIT_OBJECT_W3SUPER IMPLEMENTATION.


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



CLASS ZCL_ABAPGIT_OO_SERIALIZER IMPLEMENTATION.


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
      zcx_abapgit_exception=>raise( |Error from CL_OO_SOURCE. Subrc = { sy-subrc }| ).
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


  METHOD zif_abapgit_sap_package~get_transport_type.
    DATA: lv_err_prefix TYPE string,
          lv_pkg_name   TYPE e071-obj_name.

    lv_err_prefix = |TRINT_GET_REQUEST_TYPE(R3TR, DEVC, { mv_package })|.
    lv_pkg_name = mv_package.

    CALL FUNCTION 'TRINT_GET_REQUEST_TYPE'
      EXPORTING
        iv_pgmid                   = 'R3TR'
        iv_object                  = 'DEVC'
        iv_obj_name                = lv_pkg_name
      IMPORTING
        ev_request_type            = rs_transport_type-request
        ev_task_type               = rs_transport_type-task
      EXCEPTIONS
        no_request_needed          = 1
        internal_error             = 2
        cts_initialization_failure = 3.

    CASE sy-subrc.
      WHEN 0.
        " OK!

      WHEN 1.
        zcx_abapgit_exception=>raise( |{ lv_err_prefix }: transport is not needed| ).

      WHEN 2.
        zcx_abapgit_exception=>raise( |{ lv_err_prefix }: internal error| ).

      WHEN 3.
        zcx_abapgit_exception=>raise( |{ lv_err_prefix }: failed to initialized CTS| ).

      WHEN OTHERS.
        zcx_abapgit_exception=>raise( |{ lv_err_prefix }: unrecognized return code| ).
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



CLASS ZCL_ABAPGIT_SOTR_HANDLER IMPLEMENTATION.


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
        zcx_abapgit_exception=>raise( |error from SOTR_OBJECT_GET_OBJECTS. Subrc = { sy-subrc }| ).
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
        zcx_abapgit_exception=>raise( |Error from SOTR_CREATE_CONCEPT. Subrc = { sy-subrc }| ).
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

    " Known SOTR usage...
    " LIMU: CPUB, WAPP, WDYV
    " R3TR: ENHC, ENHO, ENHS, ENSC, SCGR, SMIF, WDYA, WEBI, WEBS

    et_sotr_use = get_sotr_usage( iv_pgmid    = iv_pgmid
                                  iv_object   = iv_object
                                  iv_obj_name = iv_obj_name ).

    LOOP AT et_sotr_use ASSIGNING <ls_sotr_use> WHERE NOT concept IS INITIAL.
      INSERT get_sotr_4_concept( <ls_sotr_use>-concept ) INTO TABLE et_sotr.
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


  METHOD build.

    DATA: lv_path         TYPE string,
          lo_skip_objects TYPE REF TO zcl_abapgit_skip_objects,
          lt_excludes     TYPE RANGE OF trobjtype,
          lt_srcsystem    TYPE RANGE OF tadir-srcsystem,
          ls_srcsystem    LIKE LINE OF lt_srcsystem,
          ls_exclude      LIKE LINE OF lt_excludes,
          lo_folder_logic TYPE REF TO zcl_abapgit_folder_logic,
          lv_last_package TYPE devclass VALUE cl_abap_char_utilities=>horizontal_tab,
          lv_obj_name     TYPE sobj_name,
          lv_name         TYPE progname,
          lv_namespace    TYPE namespace,
          lt_packages     TYPE zif_abapgit_sap_package=>ty_devclass_tt.

    FIELD-SYMBOLS: <ls_tadir>   LIKE LINE OF rt_tadir,
                   <ls_nspc>    LIKE LINE OF rt_tadir,
                   <lv_package> LIKE LINE OF lt_packages.


    "Determine Packages to Read
    IF iv_ignore_subpackages = abap_false.
      lt_packages = zcl_abapinst_factory=>get_sap_package( iv_package )->list_subpackages( ).
    ENDIF.
    INSERT iv_package INTO lt_packages INDEX 1.

    ls_exclude-sign = 'I'.
    ls_exclude-option = 'EQ'.

    ls_exclude-low = 'SOTR'.
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low = 'SFB1'.
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low = 'SFB2'.
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low = 'STOB'. " auto generated by core data services
    APPEND ls_exclude TO lt_excludes.

    IF iv_only_local_objects = abap_true.
      ls_srcsystem-sign   = 'I'.
      ls_srcsystem-option = 'EQ'.
      ls_srcsystem-low    = sy-sysid.
      APPEND ls_srcsystem TO lt_srcsystem.
    ENDIF.

    IF lt_packages IS NOT INITIAL.
      SELECT * FROM tadir
        INTO CORRESPONDING FIELDS OF TABLE rt_tadir
        FOR ALL ENTRIES IN lt_packages
        WHERE devclass = lt_packages-table_line
        AND pgmid = 'R3TR'
        AND object NOT IN lt_excludes
        AND delflag = abap_false
        AND srcsystem IN lt_srcsystem
        ORDER BY PRIMARY KEY ##TOO_MANY_ITAB_FIELDS. "#EC CI_GENBUFF "#EC CI_SUBRC
    ENDIF.

    SORT rt_tadir BY devclass pgmid object obj_name.

    CREATE OBJECT lo_skip_objects.
    rt_tadir = lo_skip_objects->skip_sadl_generated_objects(
      it_tadir = rt_tadir
      ii_log   = ii_log ).

    LOOP AT lt_packages ASSIGNING <lv_package>.
      " Local packages are not in TADIR, only in TDEVC, act as if they were
      IF <lv_package> CP '$*'. " OR <package> CP 'T*' ).
        APPEND INITIAL LINE TO rt_tadir ASSIGNING <ls_tadir>.
        <ls_tadir>-pgmid    = 'R3TR'.
        <ls_tadir>-object   = 'DEVC'.
        <ls_tadir>-obj_name = <lv_package>.
        <ls_tadir>-devclass = <lv_package>.
      ENDIF.
    ENDLOOP.

    LOOP AT rt_tadir ASSIGNING <ls_tadir> WHERE obj_name(1) = '/'.
      " Namespaces are not in TADIR, but are necessary for creating
      " objects in transportable packages
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
        READ TABLE rt_tadir TRANSPORTING NO FIELDS
          WITH KEY pgmid = 'R3TR' object = 'NSPC' obj_name = lv_namespace.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO rt_tadir ASSIGNING <ls_nspc>.
          <ls_nspc>-pgmid    = 'R3TR'.
          <ls_nspc>-object   = 'NSPC'.
          <ls_nspc>-obj_name = lv_namespace.
          <ls_nspc>-devclass = iv_package.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT rt_tadir ASSIGNING <ls_tadir>.

      IF lv_last_package <> <ls_tadir>-devclass.
        "Change in Package
        lv_last_package = <ls_tadir>-devclass.

        IF NOT io_dot IS INITIAL.
          "Reuse given Folder Logic Instance
          IF lo_folder_logic IS NOT BOUND.
            "Get Folder Logic Instance
            lo_folder_logic = zcl_abapgit_folder_logic=>get_instance( ).
          ENDIF.

          lv_path = lo_folder_logic->package_to_path(
            iv_top     = iv_top
            io_dot     = io_dot
            iv_package = <ls_tadir>-devclass ).
        ENDIF.
      ENDIF.

      <ls_tadir>-path = lv_path.

      IF <ls_tadir>-object = 'SICF'.
* replace the internal GUID with a hash of the path
        TRY.
            CALL METHOD ('ZCL_ABAPGIT_OBJECT_SICF')=>read_sicf_url
              EXPORTING
                iv_obj_name = <ls_tadir>-obj_name
              RECEIVING
                rv_hash     = <ls_tadir>-obj_name+15.
          CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
* SICF might not be supported in some systems, assume this code is not called
        ENDTRY.
      ENDIF.
    ENDLOOP.
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


* start recursion
* hmm, some problems here, should TADIR also build path?
    rt_tadir = build( iv_package            = iv_package
                      iv_top                = iv_package
                      io_dot                = io_dot
                      iv_ignore_subpackages = iv_ignore_subpackages
                      iv_only_local_objects = iv_only_local_objects
                      ii_log                = ii_log ).


    rt_tadir = check_exists( rt_tadir ).

  ENDMETHOD.


  METHOD zif_abapgit_tadir~read_single.

* note that SICF has special handling, the obj_name is different in the system than serialized

    SELECT SINGLE * FROM tadir INTO CORRESPONDING FIELDS OF rs_tadir
      WHERE pgmid = iv_pgmid
      AND object = iv_object
      AND obj_name = iv_obj_name.                         "#EC CI_SUBRC

  ENDMETHOD.
ENDCLASS.



CLASS ZCL_ABAPGIT_URL IMPLEMENTATION.


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

    FIND REGEX '(https?://[^/]*)(.*/)([^\.]*)[\.git]?' IN iv_url
      SUBMATCHES ev_host ev_path ev_name.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Malformed URL' ).
    ENDIF.

  ENDMETHOD.


  METHOD validate.

    name( iv_url      = iv_url
          iv_validate = abap_true ).

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
          lt_items        TYPE zif_abapgit_definitions=>ty_items_tt,
          ls_item         LIKE LINE OF lt_items,
          lv_is_xml       TYPE abap_bool,
          lv_sub_fetched  TYPE abap_bool,
          lt_sub_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt,
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
      IF io_dot->is_ignored( iv_path     = <ls_remote>-path
                             iv_filename = <ls_remote>-filename ).
        DELETE lt_remote.
      ENDIF.
    ENDLOOP.

    " Process local files and new local files
    LOOP AT it_local ASSIGNING <ls_local>.
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

      IF NOT ls_item-devclass IS INITIAL AND iv_devclass <> ls_item-devclass.
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
          CLEAR ls_item-devclass.
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
      IF sy-subrc = 0 AND <ls_local>-file-sha1 = <ls_remote>-sha1.
        <ls_result>-packmove = abap_true.
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
      filename ASCENDING.

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

    DATA: lv_path         TYPE string,
          ls_item         TYPE zif_abapgit_definitions=>ty_item,
          ls_file         TYPE zif_abapgit_definitions=>ty_file_signature,
          lt_res_sort     LIKE it_results,
          lt_item_idx     LIKE it_results,
          lt_move_idx     LIKE it_results,
          lo_folder_logic TYPE REF TO zcl_abapgit_folder_logic.

    FIELD-SYMBOLS: <ls_res1> LIKE LINE OF it_results,
                   <ls_res2> LIKE LINE OF it_results.

    " This method just adds messages to the log. No log, nothing to do here
    IF ii_log IS INITIAL.
      RETURN.
    ENDIF.

    " Find all objects which were assigned to a different package
    LOOP AT it_results ASSIGNING <ls_res1>
      WHERE lstate = zif_abapgit_definitions=>c_state-added AND packmove = abap_true.
      READ TABLE lt_move_idx TRANSPORTING NO FIELDS
        WITH KEY obj_type = <ls_res1>-obj_type obj_name = <ls_res1>-obj_name
        BINARY SEARCH. " Sorted since it_result is sorted
      IF sy-subrc <> 0.
        ii_log->add( iv_msg  = |Changed package assignment for object { <ls_res1>-obj_type } { <ls_res1>-obj_name }|
                     iv_type = 'W'
                     iv_rc   = '5' ).
        APPEND INITIAL LINE TO lt_move_idx ASSIGNING <ls_res2>.
        <ls_res2>-obj_type = <ls_res1>-obj_type.
        <ls_res2>-obj_name = <ls_res1>-obj_name.
        <ls_res2>-path     = <ls_res1>-path.
      ENDIF.
    ENDLOOP.

    " Collect object indexe
    lt_res_sort = it_results.
    SORT lt_res_sort BY obj_type ASCENDING obj_name ASCENDING.

    LOOP AT it_results ASSIGNING <ls_res1> WHERE NOT obj_type IS INITIAL AND packmove = abap_false.
      IF NOT ( <ls_res1>-obj_type = ls_item-obj_type
          AND <ls_res1>-obj_name = ls_item-obj_name ).
        APPEND INITIAL LINE TO lt_item_idx ASSIGNING <ls_res2>.
        <ls_res2>-obj_type = <ls_res1>-obj_type.
        <ls_res2>-obj_name = <ls_res1>-obj_name.
        <ls_res2>-path     = <ls_res1>-path.
        MOVE-CORRESPONDING <ls_res1> TO ls_item.
      ENDIF.
    ENDLOOP.

    " Check files for one object is in the same folder
    LOOP AT it_results ASSIGNING <ls_res1>
      WHERE NOT obj_type IS INITIAL AND obj_type <> 'DEVC' AND packmove = abap_false.
      READ TABLE lt_item_idx ASSIGNING <ls_res2>
        WITH KEY obj_type = <ls_res1>-obj_type obj_name = <ls_res1>-obj_name
        BINARY SEARCH. " Sorted above

      IF sy-subrc <> 0 OR <ls_res1>-path <> <ls_res2>-path. " All paths are same
        ii_log->add( iv_msg = |Files for object { <ls_res1>-obj_type } {
                       <ls_res1>-obj_name } are not placed in the same folder|
                     iv_type = 'W'
                     iv_rc   = '1' ).
      ENDIF.
    ENDLOOP.

    " Check that objects are created in package corresponding to folder
    lo_folder_logic = zcl_abapgit_folder_logic=>get_instance( ).
    LOOP AT it_results ASSIGNING <ls_res1>
      WHERE NOT package IS INITIAL AND NOT path IS INITIAL AND packmove = abap_false.
      lv_path = lo_folder_logic->package_to_path(
        iv_top     = iv_top
        io_dot     = io_dot
        iv_package = <ls_res1>-package ).
      IF lv_path <> <ls_res1>-path.
        ii_log->add( iv_msg = |Package and path does not match for object, {
                       <ls_res1>-obj_type } { <ls_res1>-obj_name }|
                     iv_type = 'W'
                     iv_rc   = '2' ).
      ENDIF.
    ENDLOOP.

    " Check for multiple files with same filename
    SORT lt_res_sort BY filename ASCENDING.

    LOOP AT lt_res_sort ASSIGNING <ls_res1> WHERE obj_type <> 'DEVC' AND packmove = abap_false.
      IF <ls_res1>-filename IS NOT INITIAL AND <ls_res1>-filename = ls_file-filename.
        ii_log->add( iv_msg  = |Multiple files with same filename, { <ls_res1>-filename }|
                     iv_type = 'W'
                     iv_rc   = '3' ).
      ENDIF.

      IF <ls_res1>-filename IS INITIAL.
        ii_log->add( iv_msg  = |Filename is empty for object { <ls_res1>-obj_type } { <ls_res1>-obj_name }|
                     iv_type = 'W'
                     iv_rc   = '4' ).
      ENDIF.

      MOVE-CORRESPONDING <ls_res1> TO ls_file.
    ENDLOOP.

  ENDMETHOD.


  METHOD status.

    DATA lv_index LIKE sy-tabix.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF rt_results.

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

    CASE iv_column.
      WHEN `LONGTEXT`.

        show_longtext( is_log ).

    ENDCASE.

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

        IF lv_add_obj_col = abap_true.
          lo_column = lo_columns->get_column( |OBJ_TYPE| ).
          lo_column->set_medium_text( |Object Type| ).

          lo_column = lo_columns->get_column( |OBJ_NAME| ).
          lo_column->set_medium_text( |Object Name| ).
        ELSE.
          "hide object columns
          lo_column = lo_columns->get_column( |OBJ_TYPE| ).
          lo_column->set_technical( abap_true ).

          lo_column = lo_columns->get_column( |OBJ_NAME| ).
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
      IF NOT li_comparator IS BOUND.
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
            delete_obj(
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
      RAISE EXCEPTION lx_error.
    ENDIF.

    li_progress->off( ).

  ENDMETHOD.


  METHOD delete_obj.

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
          " This will create the package if it does not exist
          lv_package = lo_folder_logic->path_to_package(
            iv_top  = iv_package
            io_dot  = io_dot
            iv_path = <ls_result>-path ).

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
      WHEN zif_abapgit_object=>gc_step_id-lead.
        " any errors during lead step will cancel rest of process
        IF ii_log->get_status( ) = 'E'.
          zcx_abapgit_exception=>raise( 'Error during lead phase' ).
        ENDIF.
      WHEN zif_abapgit_object=>gc_step_id-ddic.
        zcl_abapgit_objects_activation=>activate( is_step-is_ddic ).
      WHEN zif_abapgit_object=>gc_step_id-abap.
        zcl_abapgit_objects_activation=>activate( is_step-is_ddic ).
      WHEN zif_abapgit_object=>gc_step_id-late.
        " late can have both DDIC (like TABL with REF TO) and non-DDIC objects
        zcl_abapgit_objects_activation=>activate( abap_true ).
        zcl_abapgit_objects_activation=>activate( abap_false ).
    ENDCASE.

    li_progress->off( ).

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

    DATA:
      lt_results TYPE zif_abapgit_definitions=>ty_results_tt.

    lt_results = zcl_abapinst_file_status=>status(
      iv_package         = iv_package
      it_local           = it_local
      it_local_checksums = it_local_checksums
      it_remote          = it_remote
      io_dot             = io_dot
      ii_log             = ii_log ).

    lt_results = filter_files_to_deserialize(
      it_results = lt_results
      ii_log     = ii_log ).

    rt_results = adjust_namespaces( prioritize_deser( lt_results ) ).

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
    <ls_step>-step_id      = zif_abapgit_object=>gc_step_id-lead.
    <ls_step>-descr        = 'Import lead objects'.
    <ls_step>-is_ddic      = abap_false.
    <ls_step>-syntax_check = abap_false.
    <ls_step>-order        = 0.

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

* NSPC first
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'NSPC'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

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
        AND obj_type <> 'NSPC'
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
          lv_supported TYPE abap_bool,
          ls_item      TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF lt_objects.


    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = lt_objects
      EXCEPTIONS
        OTHERS         = 1 ##FM_SUBRC_OK.

    LOOP AT lt_objects ASSIGNING <ls_object> WHERE pgmid = 'R3TR'.
      ls_item-obj_type = <ls_object>-object.

      lv_supported = is_supported(
        is_item        = ls_item
        iv_native_only = abap_true ).

      IF lv_supported = abap_true.
        INSERT <ls_object>-object INTO TABLE rt_types.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD update_package_tree.

    DATA: lt_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt,
          lv_package  LIKE LINE OF lt_packages,
          lv_tree     TYPE dirtree-tname.


    lt_packages = zcl_abapinst_factory=>get_sap_package( iv_package )->list_subpackages( ).
    APPEND iv_package TO lt_packages.

    LOOP AT lt_packages INTO lv_package.
* update package tree for SE80
      lv_tree = 'EU_' && lv_package.
      CALL FUNCTION 'WB_TREE_ACTUALIZE'
        EXPORTING
          tree_name              = lv_tree
          without_crossreference = abap_true
          with_tcode_index       = abap_true.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.




