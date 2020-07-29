INTERFACE /mbtools/if_gui_renderable
  PUBLIC .
************************************************************************
* MBT GUI Renderable
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************

  METHODS render
    RETURNING
      VALUE(ri_html) TYPE REF TO /mbtools/if_html
    RAISING
      /mbtools/cx_exception .

ENDINTERFACE.
