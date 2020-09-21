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

    TYPES:
      BEGIN OF ty_event_data,
        action    TYPE string,
        prev_page TYPE string,
        getdata   TYPE string,
        postdata  TYPE cnht_post_data_tab,
        params    TYPE REF TO /mbtools/cl_string_map,
      END OF ty_event_data .

    METHODS general_page_routing
      IMPORTING
        !is_event_data TYPE ty_event_data
      EXPORTING
        !ei_page       TYPE REF TO /mbtools/if_gui_renderable
        !ev_state      TYPE i
      RAISING
        /mbtools/cx_exception .
    METHODS actions_internet
      IMPORTING
        !is_event_data TYPE ty_event_data
      EXPORTING
        !ei_page       TYPE REF TO /mbtools/if_gui_renderable
        !ev_state      TYPE i
      RAISING
        /mbtools/cx_exception .
    METHODS actions_objects
      IMPORTING
        !is_event_data TYPE ty_event_data
      EXPORTING
        !ei_page       TYPE REF TO /mbtools/if_gui_renderable
        !ev_state      TYPE i
      RAISING
        /mbtools/cx_exception .
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

    CLEAR: ei_page, ev_state.

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

    CLEAR: ei_page, ev_state.

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

    CLEAR: ei_page, ev_state.

    CASE is_event_data-action.

      WHEN /mbtools/if_actions=>go_home.
        ei_page  = /mbtools/cl_gui_page_main=>create( /mbtools/cl_gui_page_main=>c_mode-user ).
        ev_state = /mbtools/cl_gui=>c_event_state-new_page.

      WHEN /mbtools/if_actions=>go_admin.
        ei_page  = /mbtools/cl_gui_page_main=>create( /mbtools/cl_gui_page_main=>c_mode-admin ).
        ev_state = /mbtools/cl_gui=>c_event_state-new_page.

      WHEN /mbtools/if_actions=>go_license.
        ei_page  = /mbtools/cl_gui_page_main=>create( /mbtools/cl_gui_page_main=>c_mode-license ).
        ev_state = /mbtools/cl_gui=>c_event_state-new_page.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
