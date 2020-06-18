CLASS cx_static_check DEFINITION
  PUBLIC
  INHERITING FROM cx_root
  ABSTRACT
  CREATE PUBLIC .

*"* public components of class CX_STATIC_CHECK
*"* do not include other source files here!!!
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !textid   LIKE textid OPTIONAL
        !previous LIKE previous OPTIONAL .
*"* protected components of class CX_STATIC_CHECK
*"* do not include other source files here!!!
  PROTECTED SECTION.
ENDCLASS.
CLASS cx_static_check IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
ENDCLASS.