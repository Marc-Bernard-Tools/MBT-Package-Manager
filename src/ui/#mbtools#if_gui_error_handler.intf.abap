************************************************************************
* /MBTOOLS/IF_GUI_ERROR_HANDLER
* MBT GUI Error Handler
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************
INTERFACE /mbtools/if_gui_error_handler
  PUBLIC .

  METHODS handle_error
    IMPORTING
      !ix_error         TYPE REF TO /mbtools/cx_exception
    RETURNING
      VALUE(rv_handled) TYPE abap_bool .

ENDINTERFACE.
