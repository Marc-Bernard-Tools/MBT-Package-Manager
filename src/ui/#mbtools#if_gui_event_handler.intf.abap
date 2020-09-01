INTERFACE /mbtools/if_gui_event_handler
  PUBLIC .


************************************************************************
* MBT GUI Event Handler
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************
  METHODS on_event
    IMPORTING
      !iv_action     TYPE clike
      !iv_getdata    TYPE clike OPTIONAL
      !it_postdata   TYPE cnht_post_data_tab OPTIONAL
      !io_parameters TYPE REF TO /mbtools/cl_string_map OPTIONAL
    EXPORTING
      !ei_page       TYPE REF TO /mbtools/if_gui_renderable
      !ev_state      TYPE i
    RAISING
      /mbtools/cx_exception .
ENDINTERFACE.