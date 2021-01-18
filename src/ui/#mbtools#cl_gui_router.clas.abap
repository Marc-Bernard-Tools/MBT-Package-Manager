CLASS /mbtools/cl_gui_router DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* MBT GUI Router
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************

  PUBLIC SECTION.

    INTERFACES /mbtools/if_gui_event_handler .

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS general_page_routing
      IMPORTING
        !ii_event         TYPE REF TO /mbtools/if_gui_event
      RETURNING
        VALUE(rs_handled) TYPE /mbtools/if_gui_event_handler=>ty_handling_result
      RAISING
        /mbtools/cx_exception .
    METHODS actions_internet
      IMPORTING
        !ii_event         TYPE REF TO /mbtools/if_gui_event
      RETURNING
        VALUE(rs_handled) TYPE /mbtools/if_gui_event_handler=>ty_handling_result
      RAISING
        /mbtools/cx_exception .
    METHODS actions_objects
      IMPORTING
        !ii_event         TYPE REF TO /mbtools/if_gui_event
      RETURNING
        VALUE(rs_handled) TYPE /mbtools/if_gui_event_handler=>ty_handling_result
      RAISING
        /mbtools/cx_exception .
ENDCLASS.



CLASS /mbtools/cl_gui_router IMPLEMENTATION.


  METHOD /mbtools/if_gui_event_handler~on_event.

    rs_handled = general_page_routing( ii_event ).

    IF rs_handled-state IS INITIAL.
      rs_handled = actions_objects( ii_event ).
    ENDIF.

    IF rs_handled-state IS INITIAL.
      rs_handled = actions_internet( ii_event ).
    ENDIF.

    IF rs_handled-state IS INITIAL.
      rs_handled-state = /mbtools/cl_gui=>c_event_state-not_handled.
    ENDIF.

  ENDMETHOD.


  METHOD actions_internet.

    CASE ii_event->mv_action.

      WHEN /mbtools/if_actions=>url.
        " General URL
        /mbtools/cl_utilities=>call_browser( ii_event->get_param( 'url' ) ).

      WHEN /mbtools/if_actions=>mbt_website.
        " Homepage
        /mbtools/cl_utilities=>call_browser( /mbtools/if_definitions=>c_www_home ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_actions=>mbt_portfolio.
        " Portfolio
        /mbtools/cl_utilities=>call_browser(
          /mbtools/if_definitions=>c_www_home && /mbtools/if_definitions=>c_www_portfolio ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_actions=>mbt_docs.
        " Documentation
        /mbtools/cl_utilities=>call_browser(
          /mbtools/if_definitions=>c_www_home && /mbtools/if_definitions=>c_www_docs ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_actions=>mbt_support.
        " Support Ticket
        /mbtools/cl_utilities=>call_browser(
          /mbtools/if_definitions=>c_www_home && /mbtools/if_definitions=>c_www_support ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.


  METHOD actions_objects.

    CASE ii_event->mv_action.

      WHEN /mbtools/if_actions=>show_object.
        /mbtools/cl_sap=>show_object( iv_object   = ii_event->get_param( 'object' )
                                      iv_obj_name = ii_event->get_param( 'object_name' ) ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_actions=>run_program.
        /mbtools/cl_sap=>run_program( ii_event->get_param( 'program' ) ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_actions=>run_transaction.
        /mbtools/cl_sap=>run_transaction( ii_event->get_param( 'transaction' ) ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.


  METHOD general_page_routing.

    CASE ii_event->mv_action.


      WHEN /mbtools/if_actions=>go_home.
        rs_handled-page  = /mbtools/cl_gui_page_main=>create( /mbtools/cl_gui_page_main=>c_mode-user ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-new_page.

      WHEN /mbtools/if_actions=>go_admin.
        rs_handled-page  = /mbtools/cl_gui_page_main=>create( /mbtools/cl_gui_page_main=>c_mode-admin ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-new_page.

      WHEN /mbtools/if_actions=>go_license.
        rs_handled-page  = /mbtools/cl_gui_page_main=>create( /mbtools/cl_gui_page_main=>c_mode-license ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-new_page.

      WHEN /mbtools/if_actions=>go_faq. "##TODO change to built-in/offline FAQ
        /mbtools/cl_utilities=>call_browser(
          /mbtools/if_definitions=>c_www_home && /mbtools/if_definitions=>c_www_faq ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_actions=>go_about.
        rs_handled-page  = /mbtools/cl_gui_page_about=>create( ).
        rs_handled-state = /mbtools/cl_gui=>c_event_state-new_page.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
