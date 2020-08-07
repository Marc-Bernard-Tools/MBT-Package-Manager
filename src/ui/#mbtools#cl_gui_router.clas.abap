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
private section.

  types:
    BEGIN OF ty_event_data,
        action    TYPE string,
        prev_page TYPE string,
        getdata   TYPE string,
        postdata  TYPE cnht_post_data_tab,
        params    TYPE REF TO /mbtools/cl_string_map,
      END OF ty_event_data .

  methods GENERAL_PAGE_ROUTING
    importing
      !IS_EVENT_DATA type TY_EVENT_DATA
    exporting
      !EI_PAGE type ref to /MBTOOLS/IF_GUI_RENDERABLE
      !EV_STATE type I
    raising
      /MBTOOLS/CX_EXCEPTION .
  methods ACTIONS_INTERNET
    importing
      !IS_EVENT_DATA type TY_EVENT_DATA
    exporting
      !EI_PAGE type ref to /MBTOOLS/IF_GUI_RENDERABLE
      !EV_STATE type I
    raising
      /MBTOOLS/CX_EXCEPTION .
  methods ACTIONS_OBJECTS
    importing
      !IS_EVENT_DATA type TY_EVENT_DATA
    exporting
      !EI_PAGE type ref to /MBTOOLS/IF_GUI_RENDERABLE
      !EV_STATE type I
    raising
      /MBTOOLS/CX_EXCEPTION .
ENDCLASS.



CLASS /MBTOOLS/CL_GUI_ROUTER IMPLEMENTATION.


  METHOD /mbtools/if_gui_event_handler~on_event.

    DATA: ls_event_data TYPE ty_event_data.

    ls_event_data-action   = iv_action.
    ls_event_data-getdata  = iv_getdata.
    ls_event_data-postdata = it_postdata.
    ls_event_data-params   = io_parameters.

    general_page_routing(
      EXPORTING
        is_event_data = ls_event_data
      IMPORTING
        ei_page       = ei_page
        ev_state      = ev_state ).

    actions_objects(
      EXPORTING
        is_event_data = ls_event_data
      IMPORTING
        ei_page       = ei_page
        ev_state      = ev_state ).

    actions_internet(
      EXPORTING
        is_event_data = ls_event_data
      IMPORTING
        ei_page       = ei_page
        ev_state      = ev_state ).

    IF ev_state IS INITIAL.
      ev_state = /mbtools/cl_gui=>c_event_state-not_handled.
    ENDIF.

  ENDMETHOD.


  METHOD actions_internet.

    CASE is_event_data-action.

      WHEN /mbtools/if_actions=>url.
        " General URL
        /mbtools/cl_utilities=>call_browser( is_event_data-getdata ).

      WHEN /mbtools/if_actions=>mbt_website.
        " Homepage
        /mbtools/cl_utilities=>call_browser( /mbtools/if_definitions=>c_www_home ).
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_actions=>mbt_portfolio.
        " Portfolio
        /mbtools/cl_utilities=>call_browser(
          /mbtools/if_definitions=>c_www_home && /mbtools/if_definitions=>c_www_portfolio ).
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_actions=>mbt_docs.
        " Documentation
        /mbtools/cl_utilities=>call_browser(
          /mbtools/if_definitions=>c_www_home && /mbtools/if_definitions=>c_www_docs ).
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_actions=>mbt_support.
        " Support Ticket
        /mbtools/cl_utilities=>call_browser(
          /mbtools/if_definitions=>c_www_home && /mbtools/if_definitions=>c_www_support ).
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.


  METHOD actions_objects.

    CASE is_event_data-action.

      WHEN /mbtools/if_actions=>show_object.
        /mbtools/cl_sap=>show_object( iv_object   = is_event_data-params->get( 'object' )
                                      iv_obj_name = is_event_data-params->get( 'object_name' ) ).
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_actions=>run_program.
        /mbtools/cl_sap=>run_program( is_event_data-params->get( 'program' ) ).
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_actions=>run_transaction.
        /mbtools/cl_sap=>run_transaction( is_event_data-params->get( 'transaction' ) ).
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.


  METHOD general_page_routing.

    CASE is_event_data-action.

      WHEN /mbtools/if_actions=>go_home.
        ei_page  = /mbtools/cl_gui_page_main=>create( abap_false ).
        ev_state = /mbtools/cl_gui=>c_event_state-new_page.

      WHEN /mbtools/if_actions=>go_admin.
        ei_page  = /mbtools/cl_gui_page_main=>create( abap_true ).
        ev_state = /mbtools/cl_gui=>c_event_state-new_page.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
