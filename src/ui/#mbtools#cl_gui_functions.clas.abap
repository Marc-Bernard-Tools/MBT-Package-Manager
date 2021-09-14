CLASS /mbtools/cl_gui_functions DEFINITION
  PUBLIC
  CREATE PUBLIC .

************************************************************************
* Marc Bernard Tools - GUI Functions
*
* Copyright 2014 abapGit Contributors <http://www.abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES /mbtools/if_gui_functions .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_gui_functions IMPLEMENTATION.


  METHOD /mbtools/if_gui_functions~gui_is_available.

    CALL FUNCTION 'GUI_IS_AVAILABLE'
      IMPORTING
        return = rv_gui_is_available.

  ENDMETHOD.


  METHOD /mbtools/if_gui_functions~is_sapgui_for_java.

    CALL FUNCTION 'GUI_HAS_JAVABEANS'
      IMPORTING
        return = rv_result.

  ENDMETHOD.
ENDCLASS.
