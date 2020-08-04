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
  methods HELP_ACTIONS
    importing
      !IS_EVENT_DATA type TY_EVENT_DATA
    exporting
      !EI_PAGE type ref to /MBTOOLS/IF_GUI_RENDERABLE
      !EV_STATE type I
    raising
      /MBTOOLS/CX_EXCEPTION .
  methods SAP_GUI_ACTIONS
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

    sap_gui_actions(
      EXPORTING
        is_event_data = ls_event_data
      IMPORTING
        ei_page       = ei_page
        ev_state      = ev_state ).

    help_actions(
      EXPORTING
        is_event_data = ls_event_data
      IMPORTING
        ei_page       = ei_page
        ev_state      = ev_state ).

    IF ev_state IS INITIAL.
      ev_state = /mbtools/cl_gui=>c_event_state-not_handled.
    ENDIF.

  ENDMETHOD.


  METHOD general_page_routing ##TODO.

    CASE is_event_data-action.

      WHEN /mbtools/cl_gui=>c_action-go_home.
        ei_page  = /mbtools/cl_gui_page_main=>create( ).
        ev_state = /mbtools/cl_gui=>c_event_state-new_page.

      WHEN /mbtools/if_definitions=>c_action-go_settings.
*        CREATE OBJECT ei_page TYPE /mbtools/cl_gui_page_settings.
*        ev_state = /mbtools/cl_gui=>c_event_state-new_page.

    ENDCASE.

  ENDMETHOD.


  METHOD help_actions ##TODO.

    CASE is_event_data-action.

      WHEN /mbtools/if_definitions=>c_action-mbt_docs.
        /mbtools/cl_utilities=>call_browser( /mbtools/if_definitions=>c_home && /mbtools/if_definitions=>c_www_docs ).
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_definitions=>c_action-mbt_support.
        /mbtools/cl_utilities=>call_browser( /mbtools/if_definitions=>c_home && /mbtools/if_definitions=>c_www_support ).
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_definitions=>c_action-mbt_website.
        /mbtools/cl_utilities=>call_browser( /mbtools/if_definitions=>c_home ).
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.


  METHOD sap_gui_actions ##TODO.

    CASE is_event_data-action.

      WHEN /mbtools/if_definitions=>c_action-jump.
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

      WHEN /mbtools/if_definitions=>c_action-url.
        /mbtools/cl_utilities=>call_browser( is_event_data-getdata ).
        ev_state = /mbtools/cl_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
