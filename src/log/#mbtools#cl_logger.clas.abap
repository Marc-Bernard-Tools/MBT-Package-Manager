CLASS /mbtools/cl_logger DEFINITION
  PUBLIC
  CREATE PRIVATE

  GLOBAL FRIENDS /mbtools/cl_logger_factory .

************************************************************************
* MBT Logger
*
* Original Author: Copyright (c) 2017 Eric Peterson
* https://github.com/epeterson320/ABAP-Logger
*
* Released under MIT License: https://opensource.org/licenses/MIT
*
* Last update: 2020-08-17
************************************************************************
  PUBLIC SECTION.

    INTERFACES /mbtools/if_logger .

    ALIASES db_number
      FOR /mbtools/if_logger~mv_db_number .
    ALIASES handle
      FOR /mbtools/if_logger~mv_handle .
    ALIASES header
      FOR /mbtools/if_logger~ms_header .
    ALIASES a
      FOR /mbtools/if_logger~a .
    ALIASES add
      FOR /mbtools/if_logger~add .
    ALIASES e
      FOR /mbtools/if_logger~e .
    ALIASES export_to_table
      FOR /mbtools/if_logger~export_to_table .
    ALIASES fullscreen
      FOR /mbtools/if_logger~fullscreen .
    ALIASES has_errors
      FOR /mbtools/if_logger~has_errors .
    ALIASES has_warnings
      FOR /mbtools/if_logger~has_warnings .
    ALIASES i
      FOR /mbtools/if_logger~i .
    ALIASES is_empty
      FOR /mbtools/if_logger~is_empty .
    ALIASES length
      FOR /mbtools/if_logger~length .
    ALIASES popup
      FOR /mbtools/if_logger~popup .
    ALIASES s
      FOR /mbtools/if_logger~s .
    ALIASES save
      FOR /mbtools/if_logger~save .
    ALIASES w
      FOR /mbtools/if_logger~w .

    "! Starts a new log.
    "! For backwards compatibility only! Use /MBTOOLS/CL_LOGGER_FACTORY instead.
    CLASS-METHODS new
      IMPORTING
        !iv_object         TYPE csequence DEFAULT '/MBTOOLS/'
        !iv_subobject      TYPE csequence OPTIONAL
        !iv_description    TYPE csequence OPTIONAL
        !iv_context        TYPE simple OPTIONAL
        !iv_auto_save      TYPE abap_bool OPTIONAL
        !iv_second_db_conn TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(ro_log)      TYPE REF TO /mbtools/cl_logger .
    "! Reopens an already existing log.
    "! For backwards compatibility only! Use /MBTOOLS/CL_LOGGER_FACTORY instead.
    CLASS-METHODS open
      IMPORTING
        !iv_object                   TYPE csequence DEFAULT '/MBTOOLS/'
        !iv_subobject                TYPE csequence
        !iv_description              TYPE csequence OPTIONAL
        !iv_create_if_does_not_exist TYPE abap_bool DEFAULT abap_false
        !iv_auto_save                TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(ro_log)                TYPE REF TO /mbtools/cl_logger .
  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
* Local type for hrpad_message as it is not available in an ABAP Development System
      BEGIN OF hrpad_message_field_list_alike,
        scrrprfd TYPE scrrprfd.
    TYPES: END OF hrpad_message_field_list_alike .
    TYPES:
      BEGIN OF hrpad_message_alike,
        cause(32)    TYPE c,                          "original: hrpad_message_cause
        detail_level TYPE ballevel.
        INCLUDE TYPE symsg .
        TYPES: field_list   TYPE STANDARD TABLE OF hrpad_message_field_list_alike
              WITH NON-UNIQUE KEY scrrprfd.
    TYPES: END OF hrpad_message_alike .

    DATA mv_sec_connection TYPE abap_bool .
    DATA mv_sec_connect_commit TYPE abap_bool .
    DATA mo_settings TYPE REF TO /mbtools/if_logger_settings .

    "! Safety limit for previous exception drill down
    METHODS drill_down_into_exception
      IMPORTING
        !io_exception                  TYPE REF TO cx_root
        !iv_type                       TYPE symsgty OPTIONAL
        !iv_importance                 TYPE balprobcl OPTIONAL
      RETURNING
        VALUE(rt_exception_data_table) TYPE tty_exception_data .
    METHODS get_message_handles
      IMPORTING
        !iv_msgtype               TYPE symsgty OPTIONAL
      RETURNING
        VALUE(rt_message_handles) TYPE bal_t_msgh .
    METHODS add_structure
      IMPORTING
        is_structure_to_log TYPE any.
    METHODS get_structure_fields
      IMPORTING
        !iv_prefix           TYPE string OPTIONAL
        !io_data_structure   TYPE REF TO cl_abap_structdescr
      CHANGING
        !ct_structure_fields TYPE cl_abap_structdescr=>component_table.
    METHODS save_log .
ENDCLASS.



CLASS /MBTOOLS/CL_LOGGER IMPLEMENTATION.


  METHOD a.
    ro_self = add(
      iv_obj_to_log    = iv_obj_to_log
      iv_context       = iv_context
      iv_callback_form = iv_callback_form
      iv_callback_prog = iv_callback_prog
      iv_callback_fm   = iv_callback_fm
      iv_type          = 'A'
      iv_importance    = iv_importance ).
  ENDMETHOD.


  METHOD add.

    DATA: detailed_msg         TYPE bal_s_msg,
          exception_data_table TYPE tty_exception_data,
          free_text_msg        TYPE char200,
          ctx_type             TYPE REF TO cl_abap_typedescr,
          ctx_ddic_header      TYPE x030l,
          msg_type             TYPE REF TO cl_abap_typedescr,
          msg_table_type       TYPE REF TO cl_abap_tabledescr,
          log_numbers          TYPE bal_t_lgnm,
          log_handles          TYPE bal_t_logh,
          log_number           TYPE bal_s_lgnm,
          formatted_context    TYPE bal_s_cont,
          formatted_params     TYPE bal_s_parm.

    FIELD-SYMBOLS: <table_of_messages> TYPE ANY TABLE,
                   <message_line>      TYPE any,
                   <bapiret1_msg>      TYPE bapiret1,
                   <bapi_msg>          TYPE bapiret2,
                   <bapi_coru_msg>     TYPE bapi_coru_return,
                   "<bapi_order_msg>    TYPE bapi_order_return,
                   <bdc_msg>           TYPE bdcmsgcoll,
                   <hrpad_msg>         TYPE hrpad_message_alike,
                   "<rcomp_msg>         TYPE rcomp,
                   <iv_context_val>    TYPE any.

    IF iv_context IS NOT INITIAL.
      ASSIGN iv_context TO <iv_context_val>.
      formatted_context-value = <iv_context_val>.
      ctx_type                = cl_abap_typedescr=>describe_by_data( iv_context ).

      CALL METHOD ctx_type->get_ddic_header
        RECEIVING
          p_header     = ctx_ddic_header
        EXCEPTIONS
          not_found    = 1
          no_ddic_type = 2
          OTHERS       = 3.
      IF sy-subrc = 0.
        formatted_context-tabname = ctx_ddic_header-tabname.
      ENDIF.
    ENDIF.

    IF iv_callback_fm IS NOT INITIAL.
      formatted_params-callback-userexitf = iv_callback_fm.
      formatted_params-callback-userexitp = iv_callback_prog.
      formatted_params-callback-userexitt = 'F'.
    ELSEIF iv_callback_form IS NOT INITIAL.
      formatted_params-callback-userexitf = iv_callback_form.
      formatted_params-callback-userexitp = iv_callback_prog.
      formatted_params-callback-userexitt = ' '.
    ENDIF.

    msg_type = cl_abap_typedescr=>describe_by_data( iv_obj_to_log ).

    IF iv_obj_to_log IS NOT SUPPLIED.
      detailed_msg-msgty = sy-msgty.
      detailed_msg-msgid = sy-msgid.
      detailed_msg-msgno = sy-msgno.
      detailed_msg-msgv1 = sy-msgv1.
      detailed_msg-msgv2 = sy-msgv2.
      detailed_msg-msgv3 = sy-msgv3.
      detailed_msg-msgv4 = sy-msgv4.
    ELSEIF msg_type->absolute_name = '\TYPE=BAPIRET1'.
      ASSIGN iv_obj_to_log TO <bapiret1_msg>.
      detailed_msg-msgty = <bapiret1_msg>-type.
      detailed_msg-msgid = <bapiret1_msg>-id.
      detailed_msg-msgno = <bapiret1_msg>-number.
      detailed_msg-msgv1 = <bapiret1_msg>-message_v1.
      detailed_msg-msgv2 = <bapiret1_msg>-message_v2.
      detailed_msg-msgv3 = <bapiret1_msg>-message_v3.
      detailed_msg-msgv4 = <bapiret1_msg>-message_v4.
    ELSEIF msg_type->absolute_name = '\TYPE=BAPIRET2'.
      ASSIGN iv_obj_to_log TO <bapi_msg>.
      detailed_msg-msgty = <bapi_msg>-type.
      detailed_msg-msgid = <bapi_msg>-id.
      detailed_msg-msgno = <bapi_msg>-number.
      detailed_msg-msgv1 = <bapi_msg>-message_v1.
      detailed_msg-msgv2 = <bapi_msg>-message_v2.
      detailed_msg-msgv3 = <bapi_msg>-message_v3.
      detailed_msg-msgv4 = <bapi_msg>-message_v4.
    ELSEIF msg_type->absolute_name = '\TYPE=BAPI_CORU_RETURN'.
      ASSIGN iv_obj_to_log TO <bapi_coru_msg>.
      detailed_msg-msgty = <bapi_coru_msg>-type.
      detailed_msg-msgid = <bapi_coru_msg>-id.
      detailed_msg-msgno = <bapi_coru_msg>-number.
      detailed_msg-msgv1 = <bapi_coru_msg>-message_v1.
      detailed_msg-msgv2 = <bapi_coru_msg>-message_v2.
      detailed_msg-msgv3 = <bapi_coru_msg>-message_v3.
      detailed_msg-msgv4 = <bapi_coru_msg>-message_v4.
*    ELSEIF msg_type->absolute_name = '\TYPE=BAPI_ORDER_RETURN'.
*      ASSIGN iv_obj_to_log TO <bapi_order_msg>.
*      detailed_msg-msgty = <bapi_order_msg>-type.
*      detailed_msg-msgid = <bapi_order_msg>-id.
*      detailed_msg-msgno = <bapi_order_msg>-number.
*      detailed_msg-msgv1 = <bapi_order_msg>-message_v1.
*      detailed_msg-msgv2 = <bapi_order_msg>-message_v2.
*      detailed_msg-msgv3 = <bapi_order_msg>-message_v3.
*      detailed_msg-msgv4 = <bapi_order_msg>-message_v4.
    ELSEIF msg_type->absolute_name = '\TYPE=BDCMSGCOLL'.
      ASSIGN iv_obj_to_log TO <bdc_msg>.
      detailed_msg-msgty = <bdc_msg>-msgtyp.
      detailed_msg-msgid = <bdc_msg>-msgid.
      detailed_msg-msgno = <bdc_msg>-msgnr.
      detailed_msg-msgv1 = <bdc_msg>-msgv1.
      detailed_msg-msgv2 = <bdc_msg>-msgv2.
      detailed_msg-msgv3 = <bdc_msg>-msgv3.
      detailed_msg-msgv4 = <bdc_msg>-msgv4.
    ELSEIF msg_type->absolute_name = '\TYPE=HRPAD_MESSAGE'.
      ASSIGN iv_obj_to_log TO <hrpad_msg>.
      detailed_msg-msgty = <hrpad_msg>-msgty.
      detailed_msg-msgid = <hrpad_msg>-msgid.
      detailed_msg-msgno = <hrpad_msg>-msgno.
      detailed_msg-msgv1 = <hrpad_msg>-msgv1.
      detailed_msg-msgv2 = <hrpad_msg>-msgv2.
      detailed_msg-msgv3 = <hrpad_msg>-msgv3.
      detailed_msg-msgv4 = <hrpad_msg>-msgv4.
*    ELSEIF msg_type->absolute_name = '\TYPE=RCOMP'.
*      ASSIGN iv_obj_to_log TO <rcomp_msg>.
*      detailed_msg-msgty = <rcomp_msg>-msgty.
*      detailed_msg-msgid = <rcomp_msg>-msgid.
*      detailed_msg-msgno = <rcomp_msg>-msgno.
*      detailed_msg-msgv1 = <rcomp_msg>-msgv1.
*      detailed_msg-msgv2 = <rcomp_msg>-msgv2.
*      detailed_msg-msgv3 = <rcomp_msg>-msgv3.
*      detailed_msg-msgv4 = <rcomp_msg>-msgv4.
    ELSEIF msg_type->type_kind = cl_abap_typedescr=>typekind_oref.
      exception_data_table = drill_down_into_exception(
          io_exception   = iv_obj_to_log
          iv_type        = iv_type
          iv_importance  = iv_importance
          ).
    ELSEIF msg_type->type_kind = cl_abap_typedescr=>typekind_table.
      ASSIGN iv_obj_to_log TO <table_of_messages>.
      LOOP AT <table_of_messages> ASSIGNING <message_line>.
        add(
          EXPORTING
           iv_obj_to_log = <message_line>
           iv_context    = iv_context ).
      ENDLOOP.
    ELSEIF msg_type->type_kind = cl_abap_typedescr=>typekind_struct1   "flat structure
        OR msg_type->type_kind = cl_abap_typedescr=>typekind_struct2.  "deep structure (already when string is used)
      add_structure( iv_obj_to_log ).
    ELSE.
      free_text_msg = iv_obj_to_log.
    ENDIF.

    IF free_text_msg IS NOT INITIAL.
      CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
        EXPORTING
          i_log_handle = handle
          i_msgty      = iv_type
          i_probclass  = iv_importance
          i_text       = free_text_msg
          i_s_context  = formatted_context
          i_s_params   = formatted_params.
    ELSEIF exception_data_table IS NOT INITIAL.
      FIELD-SYMBOLS <exception_data> LIKE LINE OF exception_data_table.
      LOOP AT exception_data_table ASSIGNING <exception_data>.
        CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
          EXPORTING
            i_log_handle = handle
            i_s_exc      = <exception_data>.
      ENDLOOP.
    ELSEIF detailed_msg IS NOT INITIAL.
      detailed_msg-context   = formatted_context.
      detailed_msg-params    = formatted_params.
      detailed_msg-probclass = iv_importance.
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = handle
          i_s_msg      = detailed_msg.
    ENDIF.

    IF mo_settings->get_autosave( ) = abap_true.
      save_log( ).
    ENDIF.
    ro_self = me.
  ENDMETHOD.


  METHOD add_structure.
    DATA: msg_type             TYPE REF TO cl_abap_typedescr,
          msg_struct_type      TYPE REF TO cl_abap_structdescr,
          structure_descriptor TYPE REF TO cl_abap_structdescr,
          components           TYPE cl_abap_structdescr=>component_table,
          component            LIKE LINE OF components.
    FIELD-SYMBOLS: <component> TYPE any.

    msg_struct_type ?= cl_abap_typedescr=>describe_by_data( is_structure_to_log ).
    add( '--- Begin of structure ---' ).

    structure_descriptor ?= cl_abap_structdescr=>describe_by_data( is_structure_to_log ).
    get_structure_fields( EXPORTING io_data_structure   = structure_descriptor
                          CHANGING  ct_structure_fields = components  ).

    LOOP AT components INTO component.
      CHECK component-type->kind = cl_abap_typedescr=>kind_elem.
      ASSIGN COMPONENT component-name OF STRUCTURE is_structure_to_log TO <component>.
      IF sy-subrc = 0.
        add( iv_obj_to_log = |{ to_lower( component-name ) } = { <component> }|
             iv_importance = '4' ).
      ENDIF.
    ENDLOOP.

    add( '--- End of structure ---' ).

  ENDMETHOD.


  METHOD drill_down_into_exception.
    DATA: i                  TYPE i VALUE 2,
          previous_exception TYPE REF TO cx_root,
          exceptions         TYPE tty_exception.

    FIELD-SYMBOLS <ex> LIKE LINE OF exceptions.
    APPEND INITIAL LINE TO exceptions ASSIGNING <ex>.
    <ex>-level = 1.
    <ex>-exception = io_exception.

    previous_exception = io_exception.

    WHILE i <= mo_settings->get_max_exception_drill_down( ).
      IF previous_exception->previous IS NOT BOUND.
        EXIT.
      ENDIF.

      previous_exception ?= previous_exception->previous.

      APPEND INITIAL LINE TO exceptions ASSIGNING <ex>.
      <ex>-level = i.
      <ex>-exception = previous_exception.
      i = i + 1.
    ENDWHILE.

    FIELD-SYMBOLS <ret> LIKE LINE OF rt_exception_data_table.
    SORT exceptions BY level DESCENDING. "Display the deepest exception first
    LOOP AT exceptions ASSIGNING <ex>.
      APPEND INITIAL LINE TO rt_exception_data_table ASSIGNING <ret>.
      <ret>-exception = <ex>-exception.
      <ret>-msgty     = iv_type.
      <ret>-probclass = iv_importance.
    ENDLOOP.
  ENDMETHOD.


  METHOD e.
    ro_self = add(
      iv_obj_to_log    = iv_obj_to_log
      iv_context       = iv_context
      iv_callback_form = iv_callback_form
      iv_callback_prog = iv_callback_prog
      iv_callback_fm   = iv_callback_fm
      iv_type          = 'E'
      iv_importance    = iv_importance ).
  ENDMETHOD.


  METHOD export_to_table.
    DATA: message_handles TYPE bal_t_msgh,
          message         TYPE bal_s_msg,
          bapiret2        TYPE bapiret2.

    FIELD-SYMBOLS <msg_handle> TYPE balmsghndl.

    message_handles = get_message_handles( ).

    LOOP AT message_handles ASSIGNING <msg_handle>.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = <msg_handle>
        IMPORTING
          e_s_msg        = message
        EXCEPTIONS
          OTHERS         = 3.
      IF sy-subrc IS INITIAL.
        MESSAGE ID message-msgid
                TYPE message-msgty
                NUMBER message-msgno
                INTO bapiret2-message
                WITH message-msgv1 message-msgv2 message-msgv3 message-msgv4.

        bapiret2-type       = message-msgty.
        bapiret2-id         = message-msgid.
        bapiret2-number     = message-msgno.
        bapiret2-log_no     = <msg_handle>-log_handle.     "last 2 chars missing!!
        bapiret2-log_msg_no = <msg_handle>-msgnumber.
        bapiret2-message_v1 = message-msgv1.
        bapiret2-message_v2 = message-msgv2.
        bapiret2-message_v3 = message-msgv3.
        bapiret2-message_v4 = message-msgv4.
        bapiret2-system     = sy-sysid.
        APPEND bapiret2 TO rt_bapiret.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD fullscreen.

    DATA:
      profile        TYPE bal_s_prof,
      lt_log_handles TYPE bal_t_logh.

    APPEND handle TO lt_log_handles.

    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING
        e_s_display_profile = profile.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = profile
        i_t_log_handle      = lt_log_handles.

  ENDMETHOD.


  METHOD get_message_handles.

    DATA: log_handle TYPE bal_t_logh,
          filter     TYPE bal_s_mfil.

    FIELD-SYMBOLS <f> LIKE LINE OF filter-msgty.

    INSERT handle INTO TABLE log_handle.

    IF iv_msgtype IS NOT INITIAL.
      APPEND INITIAL LINE TO filter-msgty ASSIGNING <f>.
      <f>-sign   = 'I'.
      <f>-option = 'EQ'.
      <f>-low    = iv_msgtype.
    ENDIF.

    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
      EXPORTING
        i_t_log_handle = log_handle
        i_s_msg_filter = filter
      IMPORTING
        e_t_msg_handle = rt_message_handles
      EXCEPTIONS
        msg_not_found  = 0.

  ENDMETHOD.


  METHOD get_structure_fields.
    DATA structure_components TYPE cl_abap_structdescr=>component_table.
    DATA structure_component LIKE LINE OF structure_components.
    DATA substructure TYPE REF TO cl_abap_structdescr.

    structure_components = io_data_structure->get_components( ).
    LOOP AT structure_components INTO structure_component.
      IF structure_component-as_include = 'X' OR structure_component-type->kind = cl_abap_typedescr=>kind_struct.
        substructure ?= structure_component-type.
        get_structure_fields(
          EXPORTING
            iv_prefix           = structure_component-name
            io_data_structure   = substructure
          CHANGING
            ct_structure_fields = ct_structure_fields ).
      ELSE.
        IF NOT iv_prefix IS INITIAL.
          CONCATENATE iv_prefix '-' structure_component-name INTO structure_component-name.
        ENDIF.
        APPEND structure_component TO ct_structure_fields.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD has_errors.

    rv_yes = boolc( lines( get_message_handles( iv_msgtype = 'E' ) ) > 0 ).

  ENDMETHOD.


  METHOD has_warnings.

    rv_yes = boolc( lines( get_message_handles( iv_msgtype = 'W' ) ) > 0 ).

  ENDMETHOD.


  METHOD i.
    ro_self = add(
      iv_obj_to_log    = iv_obj_to_log
      iv_context       = iv_context
      iv_callback_form = iv_callback_form
      iv_callback_prog = iv_callback_prog
      iv_callback_fm   = iv_callback_fm
      iv_type          = 'I'
      iv_importance    = iv_importance ).
  ENDMETHOD.


  METHOD is_empty.

    rv_yes = boolc( length( ) = 0 ).

  ENDMETHOD.


  METHOD length.

    rv_length = lines( get_message_handles( ) ).

  ENDMETHOD.


  METHOD new.

    IF iv_auto_save IS SUPPLIED.
      ro_log ?= /mbtools/cl_logger_factory=>create_log(
        iv_object      = iv_object
        iv_subobject   = iv_subobject
        iv_description = iv_description
        iv_context     = iv_context
        io_settings    = /mbtools/cl_logger_factory=>create_settings(
                         )->set_usage_of_secondary_db_conn( iv_second_db_conn
                         )->set_autosave( iv_auto_save ) ).
    ELSE.
      ro_log ?= /mbtools/cl_logger_factory=>create_log(
        iv_object      = iv_object
        iv_subobject   = iv_subobject
        iv_description = iv_description
        iv_context     = iv_context
        io_settings    = /mbtools/cl_logger_factory=>create_settings(
                         )->set_usage_of_secondary_db_conn( iv_second_db_conn ) ).
    ENDIF.

  ENDMETHOD.


  METHOD open.

    IF iv_auto_save IS SUPPLIED.
      ro_log ?= /mbtools/cl_logger_factory=>open_log(
        iv_object                   = iv_object
        iv_subobject                = iv_subobject
        iv_description              = iv_description
        iv_create_if_does_not_exist = iv_create_if_does_not_exist
        io_settings                 = /mbtools/cl_logger_factory=>create_settings(
                                      )->set_autosave( iv_auto_save ) ).
    ELSE.
      ro_log ?= /mbtools/cl_logger_factory=>open_log(
        iv_object                   = iv_object
        iv_subobject                = iv_subobject
        iv_description              = iv_description
        iv_create_if_does_not_exist = iv_create_if_does_not_exist ).
    ENDIF.

  ENDMETHOD.


  METHOD popup.
* See SBAL_DEMO_04_POPUP for ideas

    DATA:
      profile        TYPE bal_s_prof,
      lt_log_handles TYPE bal_t_logh.

    APPEND handle TO lt_log_handles.

    IF is_profile IS SUPPLIED.
      profile = is_profile.
    ELSE.
      CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
        IMPORTING
          e_s_display_profile = profile.
    ENDIF.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = profile
        i_t_log_handle      = lt_log_handles.

  ENDMETHOD.


  METHOD s.
    ro_self = add(
      iv_obj_to_log    = iv_obj_to_log
      iv_context       = iv_context
      iv_callback_form = iv_callback_form
      iv_callback_prog = iv_callback_prog
      iv_callback_fm   = iv_callback_fm
      iv_type          = 'S'
      iv_importance    = iv_importance ).
  ENDMETHOD.


  METHOD save.
    CHECK mo_settings->get_autosave( ) = abap_false.
    save_log( ).
  ENDMETHOD.


  METHOD save_log.
    DATA log_handles TYPE bal_t_logh.
    DATA log_numbers TYPE bal_t_lgnm.
    DATA log_number TYPE bal_s_lgnm.

    INSERT handle INTO TABLE log_handles.
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle       = log_handles
        i_2th_connection     = mv_sec_connection
        i_2th_connect_commit = mv_sec_connect_commit
      IMPORTING
        e_new_lognumbers     = log_numbers.
    IF db_number IS INITIAL.
      READ TABLE log_numbers INDEX 1 INTO log_number.
      db_number = log_number-lognumber.
    ENDIF.
  ENDMETHOD.


  METHOD w.
    ro_self = add(
      iv_obj_to_log    = iv_obj_to_log
      iv_context       = iv_context
      iv_callback_form = iv_callback_form
      iv_callback_prog = iv_callback_prog
      iv_callback_fm   = iv_callback_fm
      iv_type          = 'W'
      iv_importance    = iv_importance ).
  ENDMETHOD.
ENDCLASS.
