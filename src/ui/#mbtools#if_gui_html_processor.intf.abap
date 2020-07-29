INTERFACE /mbtools/if_gui_html_processor
  PUBLIC .
************************************************************************
* MBT GUI HTML Processor
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************

  METHODS process
    IMPORTING
      !iv_html         TYPE string
      !ii_gui_services TYPE REF TO /mbtools/if_gui_services
    RETURNING
      VALUE(rv_html)   TYPE string
    RAISING
      /mbtools/cx_exception .

ENDINTERFACE.
