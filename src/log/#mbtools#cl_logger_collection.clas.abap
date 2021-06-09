CLASS /mbtools/cl_logger_collection DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS /mbtools/cl_logger_factory.

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
    INTERFACES: /mbtools/if_logger_collection.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mt_loggers TYPE STANDARD TABLE OF REF TO /mbtools/if_logger WITH DEFAULT KEY.
    METHODS get_log_handles
      RETURNING
        VALUE(rt_return) TYPE bal_t_logh.
    METHODS get_display_profile
      IMPORTING
        iv_display_profile_head_size TYPE i
        iv_display_profile_tree_size TYPE i
      RETURNING
        VALUE(rs_return)             TYPE bal_s_prof.

ENDCLASS.



CLASS /mbtools/cl_logger_collection IMPLEMENTATION.


  METHOD /mbtools/if_logger_collection~add_logger.
    APPEND ii_logger TO mt_loggers.
  ENDMETHOD.


  METHOD /mbtools/if_logger_collection~display_logs.
    DATA ls_display_profile TYPE bal_s_prof.

    ls_display_profile = get_display_profile(
      iv_display_profile_head_size = iv_display_profile_head_size
      iv_display_profile_tree_size = iv_display_profile_tree_size ).

    /mbtools/if_logger_collection~display_logs_using_profile( ls_display_profile ).
  ENDMETHOD.


  METHOD /mbtools/if_logger_collection~display_logs_using_profile.

    DATA lt_log_handles TYPE bal_t_logh.

    lt_log_handles = get_log_handles( ).

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile  = is_display_profile
        i_t_log_handle       = lt_log_handles
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      "Todo "Raise Exception Error?
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.
    ENDIF.

  ENDMETHOD.


  METHOD get_display_profile.

    CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
      IMPORTING
        e_s_display_profile = rs_return.

    rs_return-head_size = iv_display_profile_head_size.
    rs_return-tree_size = iv_display_profile_tree_size.
    "interesting fact - I can't remember why I needed to move the hidden columns....
    IF rs_return-mess_fcat IS NOT INITIAL.
      SORT rs_return-mess_fcat BY no_out ASCENDING col_pos DESCENDING.
    ENDIF.

  ENDMETHOD.


  METHOD get_log_handles.

    DATA li_logger TYPE REF TO /mbtools/if_logger.

    LOOP AT mt_loggers INTO li_logger.
      INSERT li_logger->mv_handle INTO TABLE rt_return.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
