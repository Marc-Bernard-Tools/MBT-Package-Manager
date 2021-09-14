CLASS /mbtools/cl_gui_buttons DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* Marc Bernard Tools - GUI Buttons
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-or-later
************************************************************************
  PUBLIC SECTION.
    CLASS-METHODS admin
      RETURNING VALUE(rv_html_string) TYPE string.
    CLASS-METHODS help
      RETURNING VALUE(rv_html_string) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_gui_buttons IMPLEMENTATION.


  METHOD admin.
    rv_html_string = /mbtools/cl_html=>icon( 'bars' ).
  ENDMETHOD.


  METHOD help.
    rv_html_string = /mbtools/cl_html=>icon( 'question-circle-solid' ).
  ENDMETHOD.
ENDCLASS.
