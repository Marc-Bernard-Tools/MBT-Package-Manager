INTERFACE /mbtools/if_gui_event
  PUBLIC .

  DATA mv_action TYPE string READ-ONLY .
  DATA mv_getdata TYPE string READ-ONLY .
  DATA mt_postdata TYPE cnht_post_data_tab READ-ONLY .
  DATA mi_gui_services TYPE REF TO /mbtools/if_gui_services READ-ONLY .

  METHODS query
    RETURNING
      VALUE(ro_string_map) TYPE REF TO /mbtools/cl_string_map
    RAISING
      /mbtools/cx_exception.
  METHODS form_data
    RETURNING
      VALUE(ro_string_map) TYPE REF TO /mbtools/cl_string_map
    RAISING
      /mbtools/cx_exception.
  METHODS get_param
    IMPORTING
      iv_key          TYPE string
    RETURNING
      VALUE(rv_value) TYPE string.
  METHODS get_params
    CHANGING
      is_params TYPE any.

ENDINTERFACE.
