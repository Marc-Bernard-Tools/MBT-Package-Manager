CLASS lcx_error DEFINITION FINAL INHERITING FROM cx_no_check.
  PUBLIC SECTION.

    INTERFACES if_t100_message.
    CONSTANTS:
      BEGIN OF c_error_signature,
        msgid TYPE symsgid VALUE 'SY',
        msgno TYPE symsgno VALUE '002', " &
        attr1 TYPE scx_attrname VALUE 'MSG',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF c_error_signature.
    DATA msg TYPE string READ-ONLY.

    CLASS-METHODS raise
      IMPORTING
        iv_msg TYPE string.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcx_error IMPLEMENTATION.
  METHOD raise.
    DATA lx_errorTYPE REF TO lcx_error.
    CREATE OBJECT lx_error
    lx_error>msg = iv_msg.
    lx_error>if_t100_message~t100key = c_error_signature.
    RAISE EXCEPTION lx_error
  ENDMETHOD.
ENDCLASS.
