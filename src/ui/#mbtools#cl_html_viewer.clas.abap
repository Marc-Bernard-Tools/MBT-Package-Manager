CLASS /mbtools/cl_html_viewer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* Marc Bernard Tools - HTML Viewer
*
* Copyright 2014 abapGit Contributors <http://www.abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES /mbtools/if_html_viewer .

    METHODS constructor .
  PROTECTED SECTION.

    DATA mo_html_viewer TYPE REF TO cl_gui_html_viewer .

    METHODS on_event
        FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING
        !action
        !frame
        !getdata
        !postdata
        !query_table .

  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_html_viewer IMPLEMENTATION.


  METHOD /mbtools/if_html_viewer~back.

    mo_html_viewer->go_back( ).

  ENDMETHOD.


  METHOD /mbtools/if_html_viewer~close_document.

    mo_html_viewer->close_document( ).

  ENDMETHOD.


  METHOD /mbtools/if_html_viewer~free.

    mo_html_viewer->free( ).

  ENDMETHOD.


  METHOD /mbtools/if_html_viewer~get_url.

    mo_html_viewer->get_current_url( IMPORTING url = rv_url ).
    cl_gui_cfw=>flush( ).

  ENDMETHOD.


  METHOD /mbtools/if_html_viewer~load_data.

    mo_html_viewer->load_data(
      EXPORTING
        url                    = iv_url
        type                   = iv_type
        subtype                = iv_subtype
        size                   = iv_size
      IMPORTING
        assigned_url           = ev_assigned_url
      CHANGING
        data_table             = ct_data_table
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4 ).
    IF sy-subrc <> 0.
      /mbtools/cx_exception=>raise( 'Error loading data for HTML viewer' ).
    ENDIF.

  ENDMETHOD.


  METHOD /mbtools/if_html_viewer~set_registered_events.

    mo_html_viewer->set_registered_events(
      EXPORTING
        events                    = it_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3 ).
    IF sy-subrc <> 0.
      /mbtools/cx_exception=>raise( 'Error registering events for HTML viewer' ).
    ENDIF.

  ENDMETHOD.


  METHOD /mbtools/if_html_viewer~show_url.

    mo_html_viewer->show_url(
      EXPORTING
        url                    = iv_url
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4 ).
    IF sy-subrc <> 0.
      /mbtools/cx_exception=>raise( 'Error showing URL in HTML viewer' ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    DATA: lt_events TYPE cntl_simple_events,
          ls_event  LIKE LINE OF lt_events.

    CREATE OBJECT mo_html_viewer
      EXPORTING
        query_table_disabled = abap_true
        parent               = cl_gui_container=>screen0.

    ls_event-eventid    = /mbtools/if_html_viewer=>c_id_sapevent.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.

    mo_html_viewer->set_registered_events( lt_events ).
    SET HANDLER me->on_event FOR mo_html_viewer.

  ENDMETHOD.


  METHOD on_event.

    RAISE EVENT /mbtools/if_html_viewer~sapevent
      EXPORTING
        action      = action
        frame       = frame
        getdata     = getdata
        postdata    = postdata
        query_table = query_table.

  ENDMETHOD.
ENDCLASS.
