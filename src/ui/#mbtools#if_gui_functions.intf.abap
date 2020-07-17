************************************************************************
* /MBTOOLS/IF_GUI_FUNCTIONS
* MBT GUI Functions
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************
INTERFACE /mbtools/if_gui_functions
  PUBLIC .

  METHODS gui_is_available
    RETURNING
      VALUE(rv_gui_is_available) TYPE abap_bool .

  METHODS is_sapgui_for_java
    RETURNING
      VALUE(rv_result) TYPE abap_bool .

ENDINTERFACE.
