CLASS /mbtools/cx_ajson_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

************************************************************************
* abap json (AJSON)
*
* Copyright 2020 Alexander Tsybulsky <https://github.com/sbcgua/ajson>
* SPDX-License-Identifier: MIT
************************************************************************

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    TYPES:
      ty_rc TYPE c LENGTH 4 .

    CONSTANTS:
      BEGIN OF zcx_ajson_error,
      msgid TYPE symsgid VALUE '00',
      msgno TYPE symsgno VALUE '001',
      attr1 TYPE scx_attrname VALUE 'A1',
      attr2 TYPE scx_attrname VALUE 'A2',
      attr3 TYPE scx_attrname VALUE 'A3',
      attr4 TYPE scx_attrname VALUE 'A4',
      END OF zcx_ajson_error .
    DATA rc TYPE ty_rc READ-ONLY .
    DATA message TYPE string READ-ONLY .
    DATA location TYPE string READ-ONLY .
    DATA a1 TYPE symsgv READ-ONLY .
    DATA a2 TYPE symsgv READ-ONLY .
    DATA a3 TYPE symsgv READ-ONLY .
    DATA a4 TYPE symsgv READ-ONLY .

    METHODS constructor
      IMPORTING
      !textid LIKE if_t100_message=>t100key OPTIONAL
      !previous LIKE previous OPTIONAL
      !rc TYPE ty_rc OPTIONAL
      !message TYPE string OPTIONAL
      !location TYPE string OPTIONAL
      !a1 TYPE symsgv OPTIONAL
      !a2 TYPE symsgv OPTIONAL
      !a3 TYPE symsgv OPTIONAL
      !a4 TYPE symsgv OPTIONAL .
    CLASS-METHODS raise
      IMPORTING
      !iv_msg TYPE string
      !iv_location TYPE string OPTIONAL
      RAISING
      /mbtools/cx_ajson_error .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cx_ajson_error IMPLEMENTATION.


  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
      previous = previous
      .
    me->rc = rc .
    me->message = message .
    me->location = location .
    me->a1 = a1 .
    me->a2 = a2 .
    me->a3 = a3 .
    me->a4 = a4 .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = zcx_ajson_error .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
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
      textid   = zcx_ajson_error
      message  = iv_msg
      location = iv_location
      a1       = ls_msg-a1
      a2       = ls_msg-a2
      a3       = ls_msg-a3
      a4       = ls_msg-a4.

  ENDMETHOD.
ENDCLASS.
