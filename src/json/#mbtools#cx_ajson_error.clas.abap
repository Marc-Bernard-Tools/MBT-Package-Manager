class /MBTOOLS/CX_AJSON_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  types:
    ty_rc TYPE c LENGTH 4 .

  constants:
    begin of /MBTOOLS/CX_AJSON_ERROR,
      msgid type symsgid value '00',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'A1',
      attr2 type scx_attrname value 'A2',
      attr3 type scx_attrname value 'A3',
      attr4 type scx_attrname value 'A4',
    end of /MBTOOLS/CX_AJSON_ERROR .
  data RC type TY_RC read-only .
  data MESSAGE type STRING read-only .
  data LOCATION type STRING read-only .
  data A1 type SYMSGV .
  data A2 type SYMSGV .
  data A3 type SYMSGV .
  data A4 type SYMSGV .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !RC type TY_RC optional
      !MESSAGE type STRING optional
      !LOCATION type STRING optional
      !A1 type SYMSGV optional
      !A2 type SYMSGV optional
      !A3 type SYMSGV optional
      !A4 type SYMSGV optional .
  class-methods RAISE
    importing
      !IV_MSG type STRING
      !IV_LOCATION type STRING optional
    raising
      /MBTOOLS/CX_AJSON_ERROR .
protected section.
private section.
ENDCLASS.



CLASS /MBTOOLS/CX_AJSON_ERROR IMPLEMENTATION.


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
