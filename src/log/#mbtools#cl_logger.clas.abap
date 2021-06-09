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
* Last update: 2021-06-07
************************************************************************
  PUBLIC SECTION.

    INTERFACES /mbtools/if_logger .

    "! Starts a new log.
    "! For backwards compatibility only! Use /MBTOOLS/CL_LOGGER_FACTORY instead.
    CLASS-METHODS new
      IMPORTING
        !iv_object         TYPE csequence OPTIONAL
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

*    TYPES:
** Local type for hrpad_message as it is not available in an ABAP Development System
*      BEGIN OF ty_hrpad_message_field_list,
*        scrrprfd TYPE scrrprfd,
*      END OF ty_hrpad_message_field_list.
*    TYPES:
*      BEGIN OF ty_hrpad_message_alike,
*        cause(32)    TYPE c,                          "original: hrpad_message_cause
*        detail_level TYPE ballevel.
*        INCLUDE TYPE symsg .
*    TYPES: field_list TYPE STANDARD TABLE OF ty_hrpad_message_field_list
*                      WITH NON-UNIQUE KEY scrrprfd,
*      END OF ty_hrpad_message_alike .

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
        VALUE(rt_exception_data_table) TYPE ty_exception_data .
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



CLASS /mbtools/cl_logger IMPLEMENTATION.


  METHOD /mbtools/if_logger~a.

    ro_self = /mbtools/if_logger~add(
      iv_obj_to_log    = iv_obj_to_log
      iv_context       = iv_context
      iv_callback_form = iv_callback_form
      iv_callback_prog = iv_callback_prog
      iv_callback_fm   = iv_callback_fm
      iv_type          = 'A'
      iv_importance    = iv_importance ).

  ENDMETHOD.


  METHOD /mbtools/if_logger~add.

    DATA:
      ls_detailed_msg                  TYPE bal_s_msg,
      lt_exception_data_table          TYPE ty_exception_data,
      lv_free_text_msg                 TYPE char200,
      lo_ctx_type                      TYPE REF TO cl_abap_typedescr,
      ls_ctx_ddic_header               TYPE x030l,
      lo_msg_type                      TYPE REF TO cl_abap_typedescr,
      ls_formatted_context             TYPE bal_s_cont,
      ls_formatted_params              TYPE bal_s_parm,
      lv_message_type                  TYPE symsgty,
      ls_replacement_bapi_order TYPE bapiret2.

    FIELD-SYMBOLS:
      <ls_exception_data>    LIKE LINE OF lt_exception_data_table,
      <lt_table_of_messages> TYPE ANY TABLE,
      <ls_message_line>      TYPE any,
      <ls_bapiret1_msg>      TYPE bapiret1,
      <ls_bapi_msg>          TYPE bapiret2,
      <ls_bapi_coru_msg>     TYPE bapi_coru_return,
      <ls_bdc_msg>           TYPE bdcmsgcoll,
      <lv_context_val>       TYPE any.
    "Solution manager doens't have BAPI_ORDER_RETURN, RCOMP, PROTT. Therefore avoid using these concrete types
*                   <bapi_order_msg>    type bapi_order_return,
*                   <rcomp_msg>         type rcomp,
*                   <prott_msg>         type prott,

    IF iv_context IS NOT INITIAL.
      ASSIGN iv_context TO <lv_context_val>.
      ls_formatted_context-value = <lv_context_val>.
      lo_ctx_type                = cl_abap_typedescr=>describe_by_data( iv_context ).

      lo_ctx_type->get_ddic_header(
        RECEIVING
          p_header     = ls_ctx_ddic_header
        EXCEPTIONS
          not_found    = 1
          no_ddic_type = 2
          OTHERS       = 3 ).
      IF sy-subrc = 0.
        ls_formatted_context-tabname = ls_ctx_ddic_header-tabname.
      ENDIF.
    ENDIF.

    IF iv_callback_fm IS NOT INITIAL.
      ls_formatted_params-callback-userexitf = iv_callback_fm.
      ls_formatted_params-callback-userexitp = iv_callback_prog.
      ls_formatted_params-callback-userexitt = 'F'.
    ELSEIF iv_callback_form IS NOT INITIAL.
      ls_formatted_params-callback-userexitf = iv_callback_form.
      ls_formatted_params-callback-userexitp = iv_callback_prog.
      ls_formatted_params-callback-userexitt = ' '.
    ENDIF.

    lo_msg_type = cl_abap_typedescr=>describe_by_data( iv_obj_to_log ).

    IF iv_obj_to_log IS NOT SUPPLIED.
      ls_detailed_msg-msgty = sy-msgty.
      ls_detailed_msg-msgid = sy-msgid.
      ls_detailed_msg-msgno = sy-msgno.
      ls_detailed_msg-msgv1 = sy-msgv1.
      ls_detailed_msg-msgv2 = sy-msgv2.
      ls_detailed_msg-msgv3 = sy-msgv3.
      ls_detailed_msg-msgv4 = sy-msgv4.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=BAPIRET1'.
      ASSIGN iv_obj_to_log TO <ls_bapiret1_msg>.
      ls_detailed_msg-msgty = <ls_bapiret1_msg>-type.
      ls_detailed_msg-msgid = <ls_bapiret1_msg>-id.
      ls_detailed_msg-msgno = <ls_bapiret1_msg>-number.
      ls_detailed_msg-msgv1 = <ls_bapiret1_msg>-message_v1.
      ls_detailed_msg-msgv2 = <ls_bapiret1_msg>-message_v2.
      ls_detailed_msg-msgv3 = <ls_bapiret1_msg>-message_v3.
      ls_detailed_msg-msgv4 = <ls_bapiret1_msg>-message_v4.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=BAPIRET2'.
      ASSIGN iv_obj_to_log TO <ls_bapi_msg>.
      ls_detailed_msg-msgty = <ls_bapi_msg>-type.
      ls_detailed_msg-msgid = <ls_bapi_msg>-id.
      ls_detailed_msg-msgno = <ls_bapi_msg>-number.
      ls_detailed_msg-msgv1 = <ls_bapi_msg>-message_v1.
      ls_detailed_msg-msgv2 = <ls_bapi_msg>-message_v2.
      ls_detailed_msg-msgv3 = <ls_bapi_msg>-message_v3.
      ls_detailed_msg-msgv4 = <ls_bapi_msg>-message_v4.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=BAPI_CORU_RETURN'.
      ASSIGN iv_obj_to_log TO <ls_bapi_coru_msg>.
      ls_detailed_msg-msgty = <ls_bapi_coru_msg>-type.
      ls_detailed_msg-msgid = <ls_bapi_coru_msg>-id.
      ls_detailed_msg-msgno = <ls_bapi_coru_msg>-number.
      ls_detailed_msg-msgv1 = <ls_bapi_coru_msg>-message_v1.
      ls_detailed_msg-msgv2 = <ls_bapi_coru_msg>-message_v2.
      ls_detailed_msg-msgv3 = <ls_bapi_coru_msg>-message_v3.
      ls_detailed_msg-msgv4 = <ls_bapi_coru_msg>-message_v4.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=BDCMSGCOLL'.
      ASSIGN iv_obj_to_log TO <ls_bdc_msg>.
      ls_detailed_msg-msgty = <ls_bdc_msg>-msgtyp.
      ls_detailed_msg-msgid = <ls_bdc_msg>-msgid.
      ls_detailed_msg-msgno = <ls_bdc_msg>-msgnr.
      ls_detailed_msg-msgv1 = <ls_bdc_msg>-msgv1.
      ls_detailed_msg-msgv2 = <ls_bdc_msg>-msgv2.
      ls_detailed_msg-msgv3 = <ls_bdc_msg>-msgv3.
      ls_detailed_msg-msgv4 = <ls_bdc_msg>-msgv4.
*    ELSEIF lo_msg_type->absolute_name = '\TYPE=HRPAD_MESSAGE'.
*      ASSIGN iv_obj_to_log TO <ls_hrpad_msg>.
*      ls_detailed_msg-msgty = <ls_hrpad_msg>-msgty.
*      ls_detailed_msg-msgid = <ls_hrpad_msg>-msgid.
*      ls_detailed_msg-msgno = <ls_hrpad_msg>-msgno.
*      ls_detailed_msg-msgv1 = <ls_hrpad_msg>-msgv1.
*      ls_detailed_msg-msgv2 = <ls_hrpad_msg>-msgv2.
*      ls_detailed_msg-msgv3 = <ls_hrpad_msg>-msgv3.
*      ls_detailed_msg-msgv4 = <ls_hrpad_msg>-msgv4.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=BAPI_ORDER_RETURN'.
      "Solution manager doens't have BAPI_ORDER_RETURN. Therefore avoid using the concrete type
      MOVE-CORRESPONDING iv_obj_to_log TO ls_replacement_bapi_order.
      ls_detailed_msg-msgty = ls_replacement_bapi_order-type.
      ls_detailed_msg-msgid = ls_replacement_bapi_order-id.
      ls_detailed_msg-msgno = ls_replacement_bapi_order-number.
      ls_detailed_msg-msgv1 = ls_replacement_bapi_order-message_v1.
      ls_detailed_msg-msgv2 = ls_replacement_bapi_order-message_v2.
      ls_detailed_msg-msgv3 = ls_replacement_bapi_order-message_v3.
      ls_detailed_msg-msgv4 = ls_replacement_bapi_order-message_v4.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=RCOMP'.
      "Solution manager doens't have RCOMP. Therefore avoid using the concrete type
      MOVE-CORRESPONDING iv_obj_to_log TO ls_detailed_msg.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=PROTT'.
      "Solution manager doens't have PROTT. Therefore avoid using the concrete type
      MOVE-CORRESPONDING iv_obj_to_log TO ls_detailed_msg.
    ELSEIF lo_msg_type->type_kind = cl_abap_typedescr=>typekind_oref.
      IF iv_type IS INITIAL.
        lv_message_type = if_msg_output=>msgtype_error.
      ELSE.
        lv_message_type = iv_type.
      ENDIF.
      lt_exception_data_table = drill_down_into_exception(
        io_exception   = iv_obj_to_log
        iv_type        = lv_message_type
        iv_importance  = iv_importance ).
    ELSEIF lo_msg_type->type_kind = cl_abap_typedescr=>typekind_table.
      ASSIGN iv_obj_to_log TO <lt_table_of_messages>.
      LOOP AT <lt_table_of_messages> ASSIGNING <ls_message_line>.
        /mbtools/if_logger~add( iv_obj_to_log = <ls_message_line>
                                iv_context    = iv_context ).
      ENDLOOP.
    ELSEIF lo_msg_type->type_kind = cl_abap_typedescr=>typekind_struct1   "flat structure
        OR lo_msg_type->type_kind = cl_abap_typedescr=>typekind_struct2.  "deep structure (already when string is used)
      add_structure( iv_obj_to_log ).
    ELSE.
      lv_free_text_msg = iv_obj_to_log.
    ENDIF.

    IF lv_free_text_msg IS NOT INITIAL.
      CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
        EXPORTING
          i_log_handle = /mbtools/if_logger~mv_handle
          i_msgty      = iv_type
          i_probclass  = iv_importance
          i_text       = lv_free_text_msg
          i_s_context  = ls_formatted_context
          i_s_params   = ls_formatted_params.
    ELSEIF lt_exception_data_table IS NOT INITIAL.
      LOOP AT lt_exception_data_table ASSIGNING <ls_exception_data>.
        CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
          EXPORTING
            i_log_handle = /mbtools/if_logger~mv_handle
            i_s_exc      = <ls_exception_data>.
      ENDLOOP.
    ELSEIF ls_detailed_msg IS NOT INITIAL.
      ls_detailed_msg-context   = ls_formatted_context.
      ls_detailed_msg-params    = ls_formatted_params.
      ls_detailed_msg-probclass = iv_importance.
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = /mbtools/if_logger~mv_handle
          i_s_msg      = ls_detailed_msg.
    ENDIF.

    IF mo_settings->get_autosave( ) = abap_true.
      save_log( ).
    ENDIF.

    ro_self = me.

  ENDMETHOD.


  METHOD /mbtools/if_logger~e.

    ro_self = /mbtools/if_logger~add(
      iv_obj_to_log    = iv_obj_to_log
      iv_context       = iv_context
      iv_callback_form = iv_callback_form
      iv_callback_prog = iv_callback_prog
      iv_callback_fm   = iv_callback_fm
      iv_type          = 'E'
      iv_importance    = iv_importance ).

  ENDMETHOD.


  METHOD /mbtools/if_logger~export_to_table.

    DATA:
      lt_message_handles TYPE bal_t_msgh,
      ls_message         TYPE bal_s_msg,
      ls_bapiret2        TYPE bapiret2.

    FIELD-SYMBOLS <ls_msg_handle> TYPE balmsghndl.

    lt_message_handles = get_message_handles( ).

    LOOP AT lt_message_handles ASSIGNING <ls_msg_handle>.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = <ls_msg_handle>
        IMPORTING
          e_s_msg        = ls_message
        EXCEPTIONS
          OTHERS         = 3.
      IF sy-subrc IS INITIAL.
        MESSAGE ID ls_message-msgid
          TYPE ls_message-msgty
          NUMBER ls_message-msgno
          INTO ls_bapiret2-message
          WITH ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.

        ls_bapiret2-type       = ls_message-msgty.
        ls_bapiret2-id         = ls_message-msgid.
        ls_bapiret2-number     = ls_message-msgno.
        ls_bapiret2-log_no     = <ls_msg_handle>-log_handle.     "last 2 chars missing!!
        ls_bapiret2-log_msg_no = <ls_msg_handle>-msgnumber.
        ls_bapiret2-message_v1 = ls_message-msgv1.
        ls_bapiret2-message_v2 = ls_message-msgv2.
        ls_bapiret2-message_v3 = ls_message-msgv3.
        ls_bapiret2-message_v4 = ls_message-msgv4.
        ls_bapiret2-system     = sy-sysid.
        APPEND ls_bapiret2 TO rt_bapiret.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD /mbtools/if_logger~fullscreen.

    DATA:
      ls_profile     TYPE bal_s_prof,
      lt_log_handles TYPE bal_t_logh.

    APPEND /mbtools/if_logger~mv_handle TO lt_log_handles.

    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING
        e_s_display_profile = ls_profile.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = ls_profile
        i_t_log_handle      = lt_log_handles.

  ENDMETHOD.


  METHOD /mbtools/if_logger~has_errors.

    rv_yes = boolc( lines( get_message_handles( iv_msgtype = 'E' ) ) > 0 ).

  ENDMETHOD.


  METHOD /mbtools/if_logger~has_warnings.

    rv_yes = boolc( lines( get_message_handles( iv_msgtype = 'W' ) ) > 0 ).

  ENDMETHOD.


  METHOD /mbtools/if_logger~i.

    ro_self = /mbtools/if_logger~add(
      iv_obj_to_log    = iv_obj_to_log
      iv_context       = iv_context
      iv_callback_form = iv_callback_form
      iv_callback_prog = iv_callback_prog
      iv_callback_fm   = iv_callback_fm
      iv_type          = 'I'
      iv_importance    = iv_importance ).

  ENDMETHOD.


  METHOD /mbtools/if_logger~is_empty.

    rv_yes = boolc( /mbtools/if_logger~length( ) = 0 ).

  ENDMETHOD.


  METHOD /mbtools/if_logger~length.

    rv_length = lines( get_message_handles( ) ).

  ENDMETHOD.


  METHOD /mbtools/if_logger~popup.
* See SBAL_DEMO_04_POPUP for ideas

    DATA:
      ls_profile     TYPE bal_s_prof,
      lt_log_handles TYPE bal_t_logh.

    APPEND /mbtools/if_logger~mv_handle TO lt_log_handles.

    IF is_profile IS SUPPLIED.
      ls_profile = is_profile.
    ELSE.
      CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
        IMPORTING
          e_s_display_profile = ls_profile.
    ENDIF.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = ls_profile
        i_t_log_handle      = lt_log_handles.

  ENDMETHOD.


  METHOD /mbtools/if_logger~s.

    ro_self = /mbtools/if_logger~add(
      iv_obj_to_log    = iv_obj_to_log
      iv_context       = iv_context
      iv_callback_form = iv_callback_form
      iv_callback_prog = iv_callback_prog
      iv_callback_fm   = iv_callback_fm
      iv_type          = 'S'
      iv_importance    = iv_importance ).

  ENDMETHOD.


  METHOD /mbtools/if_logger~save.

    CHECK mo_settings->get_autosave( ) = abap_false.
    save_log( ).

  ENDMETHOD.


  METHOD /mbtools/if_logger~timer_end.

    DATA:
      lv_timestamp TYPE timestampl,
      lv_sec       TYPE p LENGTH 10 DECIMALS 2.

    GET TIME STAMP FIELD lv_timestamp.

    TRY.
        rv_result = cl_abap_tstmp=>subtract(
          tstmp1 = lv_timestamp
          tstmp2 = /mbtools/if_logger~mv_timestamp ).
      CATCH cx_parameter_invalid.
        /mbtools/if_logger~add(
          iv_obj_to_log = |Runtime: Error getting measurement|
          iv_type       = 'W' ).
        RETURN.
    ENDTRY.

    lv_sec = rv_result. " round to 2 decimal places

    /mbtools/if_logger~add(
      iv_obj_to_log = |Runtime: { lv_sec } seconds|
      iv_type       = 'I' ).

  ENDMETHOD.


  METHOD /mbtools/if_logger~timer_start.

    GET TIME STAMP FIELD /mbtools/if_logger~mv_timestamp.

  ENDMETHOD.


  METHOD /mbtools/if_logger~w.

    ro_self = /mbtools/if_logger~add(
      iv_obj_to_log    = iv_obj_to_log
      iv_context       = iv_context
      iv_callback_form = iv_callback_form
      iv_callback_prog = iv_callback_prog
      iv_callback_fm   = iv_callback_fm
      iv_type          = 'W'
      iv_importance    = iv_importance ).

  ENDMETHOD.


  METHOD add_structure.

    DATA:
      lo_msg_struct_type      TYPE REF TO cl_abap_structdescr,
      lo_structure_descriptor TYPE REF TO cl_abap_structdescr,
      lt_components           TYPE cl_abap_structdescr=>component_table,
      ls_component            LIKE LINE OF lt_components.

    FIELD-SYMBOLS: <lv_component> TYPE any.

    lo_msg_struct_type ?= cl_abap_typedescr=>describe_by_data( is_structure_to_log ).
    /mbtools/if_logger~add( '--- Begin of structure ---' ).

    lo_structure_descriptor ?= cl_abap_structdescr=>describe_by_data( is_structure_to_log ).
    get_structure_fields( EXPORTING io_data_structure   = lo_structure_descriptor
                          CHANGING  ct_structure_fields = lt_components ).

    LOOP AT lt_components INTO ls_component.
      CHECK ls_component-type->kind = cl_abap_typedescr=>kind_elem.
      ASSIGN COMPONENT ls_component-name OF STRUCTURE is_structure_to_log TO <lv_component>.
      IF sy-subrc = 0.
        /mbtools/if_logger~add( iv_obj_to_log = |{ to_lower( ls_component-name ) } = { <lv_component> }|
           iv_importance = '4' ).
      ENDIF.
    ENDLOOP.

    /mbtools/if_logger~add( '--- End of structure ---' ).

  ENDMETHOD.


  METHOD drill_down_into_exception.

    DATA:
      lv_i                  TYPE i VALUE 2,
      lx_previous_exception TYPE REF TO cx_root,
      lt_exceptions         TYPE ty_exceptions.

    FIELD-SYMBOLS:
      <ls_ex>  LIKE LINE OF lt_exceptions,
      <ls_ret> LIKE LINE OF rt_exception_data_table.

    APPEND INITIAL LINE TO lt_exceptions ASSIGNING <ls_ex>.
    <ls_ex>-level = 1.
    <ls_ex>-exception = io_exception.

    lx_previous_exception = io_exception.

    WHILE lv_i <= mo_settings->get_max_exception_drill_down( ).
      IF lx_previous_exception->previous IS NOT BOUND.
        EXIT.
      ENDIF.

      lx_previous_exception ?= lx_previous_exception->previous.

      APPEND INITIAL LINE TO lt_exceptions ASSIGNING <ls_ex>.
      <ls_ex>-level = lv_i.
      <ls_ex>-exception = lx_previous_exception.
      lv_i = lv_i + 1.
    ENDWHILE.

    SORT lt_exceptions BY level DESCENDING. "Display the deepest exception first
    LOOP AT lt_exceptions ASSIGNING <ls_ex>.
      APPEND INITIAL LINE TO rt_exception_data_table ASSIGNING <ls_ret>.
      <ls_ret>-exception = <ls_ex>-exception.
      <ls_ret>-msgty     = iv_type.
      <ls_ret>-probclass = iv_importance.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_message_handles.

    DATA: lt_log_handle TYPE bal_t_logh,
          ls_filter     TYPE bal_s_mfil.

    FIELD-SYMBOLS <lv_f> LIKE LINE OF ls_filter-msgty.

    INSERT /mbtools/if_logger~mv_handle INTO TABLE lt_log_handle.

    IF iv_msgtype IS NOT INITIAL.
      APPEND INITIAL LINE TO ls_filter-msgty ASSIGNING <lv_f>.
      <lv_f>-sign   = 'I'.
      <lv_f>-option = 'EQ'.
      <lv_f>-low    = iv_msgtype.
    ENDIF.

    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
      EXPORTING
        i_t_log_handle = lt_log_handle
        i_s_msg_filter = ls_filter
      IMPORTING
        e_t_msg_handle = rt_message_handles
      EXCEPTIONS
        msg_not_found  = 0.

  ENDMETHOD.


  METHOD get_structure_fields.

    DATA:
      lt_structure_components TYPE cl_abap_structdescr=>component_table,
      ls_structure_component  LIKE LINE OF lt_structure_components,
      lo_substructure         TYPE REF TO cl_abap_structdescr.

    lt_structure_components = io_data_structure->get_components( ).
    LOOP AT lt_structure_components INTO ls_structure_component.
      IF ls_structure_component-as_include = 'X' OR ls_structure_component-type->kind = cl_abap_typedescr=>kind_struct.
        lo_substructure ?= ls_structure_component-type.

        get_structure_fields(
          EXPORTING
            iv_prefix           = ls_structure_component-name
            io_data_structure   = lo_substructure
          CHANGING
            ct_structure_fields = ct_structure_fields ).
      ELSE.
        IF iv_prefix IS NOT INITIAL.
          CONCATENATE iv_prefix '-' ls_structure_component-name INTO ls_structure_component-name.
        ENDIF.
        APPEND ls_structure_component TO ct_structure_fields.
      ENDIF.
    ENDLOOP.

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


  METHOD save_log.

    DATA:
      lt_log_handles TYPE bal_t_logh,
      lt_log_numbers TYPE bal_t_lgnm,
      ls_log_number  TYPE bal_s_lgnm.

    INSERT /mbtools/if_logger~mv_handle INTO TABLE lt_log_handles.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle       = lt_log_handles
        i_2th_connection     = mv_sec_connection
        i_2th_connect_commit = mv_sec_connect_commit
      IMPORTING
        e_new_lognumbers     = lt_log_numbers.
    IF /mbtools/if_logger~mv_db_number IS INITIAL.
      READ TABLE lt_log_numbers INDEX 1 INTO ls_log_number.
      /mbtools/if_logger~mv_db_number = ls_log_number-lognumber.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
