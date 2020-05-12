CLASS /mbtools/cl_screen DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
    ty_screen_field TYPE c LENGTH 83.

    CLASS-DATA:
      gv_copyright     TYPE string,
      gv_about         TYPE string,
      gv_documentation TYPE string,
      gv_terms         TYPE string.
    CLASS-METHODS class_constructor.
    CLASS-METHODS icon
      IMPORTING
        VALUE(iv_icon)   TYPE icon_d
        VALUE(iv_text)   TYPE csequence OPTIONAL
        VALUE(iv_quick)  TYPE csequence OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE ty_screen_field.
    CLASS-METHODS header
      IMPORTING
        VALUE(iv_icon)   TYPE icon_d
        VALUE(iv_text)   TYPE csequence OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE ty_screen_field.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /MBTOOLS/CL_SCREEN IMPLEMENTATION.


  METHOD class_constructor.
    gv_copyright      = 'Copyright (c) 2020 Marc Bernard Tools. All right reserved'(000).
    gv_about          = 'About'(001).
    gv_documentation  = 'Documentation'(002).
    gv_terms          = 'Terms'(003).
  ENDMETHOD.


  METHOD header.
    WRITE iv_icon AS ICON TO rv_result.
    rv_result+6 = iv_text.
  ENDMETHOD.


  METHOD icon.
    DATA lv_info TYPE string.

    IF iv_quick IS INITIAL.
      lv_info = iv_text.
    ELSE.
      lv_info = iv_quick.
    ENDIF.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name   = iv_icon
        text   = iv_text
        info   = lv_info
      IMPORTING
        result = rv_result.
  ENDMETHOD.
ENDCLASS.
