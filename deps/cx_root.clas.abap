CLASS cx_root DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

*"* public components of class CX_ROOT
*"* do not include other source files here!!!
  PUBLIC SECTION.

    INTERFACES if_message .
    INTERFACES if_serializable_object .

    ALIASES get_longtext
      FOR if_message~get_longtext .
    ALIASES get_text
      FOR if_message~get_text .

    CONSTANTS cx_root TYPE sotr_conc VALUE '16AA9A3937A9BB56E10000000A11447B'. "#EC NOTEXT
    DATA textid TYPE sotr_conc READ-ONLY .
    DATA previous TYPE REF TO cx_root READ-ONLY .
    DATA kernel_errid TYPE s380errid READ-ONLY .
    TYPE-POOLS abap .
    DATA is_resumable TYPE abap_bool READ-ONLY .

    METHODS constructor
      IMPORTING
        !textid   LIKE textid OPTIONAL
        !previous LIKE previous OPTIONAL .
    METHODS get_source_position
      EXPORTING
        !program_name TYPE syrepid
        !include_name TYPE syrepid
        !source_line  TYPE i .
*"* protected components of class CX_ROOT
*"* do not include other source files here!!!
  PROTECTED SECTION.
ENDCLASS.
CLASS cx_root IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
  METHOD get_source_position.
  ENDMETHOD.
ENDCLASS.