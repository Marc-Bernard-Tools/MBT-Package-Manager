CLASS /mbtools/cx_ajson_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_t100_message.

    TYPES:
      ty_rc TYPE c LENGTH 4.

    CONSTANTS:
      BEGIN OF /mbtools/cx_ajson_error,
        msgid TYPE symsgid VALUE '00',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'A1',
        attr2 TYPE scx_attrname VALUE 'A2',
        attr3 TYPE scx_attrname VALUE 'A3',
        attr4 TYPE scx_attrname VALUE 'A4',
      END OF /mbtools/cx_ajson_error.
    DATA rc TYPE ty_rc READ-ONLY.
    DATA message TYPE string READ-ONLY.
    DATA location TYPE string READ-ONLY.
    DATA a1 TYPE symsgv.
    DATA a2 TYPE symsgv.
    DATA a3 TYPE symsgv.
    DATA a4 TYPE symsgv.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !rc       TYPE ty_rc OPTIONAL
        !message  TYPE string OPTIONAL
        !location TYPE string OPTIONAL
        !a1       TYPE symsgv OPTIONAL
        !a2       TYPE symsgv OPTIONAL
        !a3       TYPE symsgv OPTIONAL
        !a4       TYPE symsgv OPTIONAL.
    CLASS-METHODS raise
      IMPORTING
        !iv_msg      TYPE string
        !iv_location TYPE string OPTIONAL
      RAISING
        /mbtools/cx_ajson_error.

    METHODS if_message~get_longtext
        REDEFINITION.
    METHODS if_message~get_text
        REDEFINITION.
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS /mbtools/cx_ajson_error IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->RC = RC .
me->MESSAGE = MESSAGE .
me->LOCATION = LOCATION .
me->A1 = A1 .
me->A2 = A2 .
me->A3 = A3 .
me->A4 = A4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = /MBTOOLS/CX_AJSON_ERROR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD if_message~get_longtext.
    result = super->get_longtext( preserve_newlines ).
  ENDMETHOD.


  METHOD if_message~get_text.
    result = super->get_text( ).
  ENDMETHOD.


  METHOD raise.

    DATA:
      BEGIN OF ls_msg,
        a1 LIKE a1,
        a2 LIKE a1,
        a3 LIKE a1,
        a4 LIKE a1,
      END OF ls_msg.

    IF iv_location IS INITIAL.
      ls_msg = iv_msg.
    ELSE.
      DATA lv_tmp TYPE string.
      lv_tmp = iv_msg && | @{ iv_location }|.
      ls_msg = lv_tmp.
    ENDIF.

    RAISE EXCEPTION TYPE /mbtools/cx_ajson_error
      EXPORTING
        textid   = /mbtools/cx_ajson_error
        message  = iv_msg
        location = iv_location
        a1       = ls_msg-a1
        a2       = ls_msg-a2
        a3       = ls_msg-a3
        a4       = ls_msg-a4.

  ENDMETHOD.
ENDCLASS.
