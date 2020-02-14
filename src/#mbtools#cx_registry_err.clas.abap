class /MBTOOLS/CX_REGISTRY_ERR definition
  public
  inheriting from CX_DYNAMIC_CHECK
  create public .

public section.

  constants /MBTOOLS/CX_REGISTRY_ERR type SOTR_CONC value '000D3A5598F91EEA8CDD198CF5C7C480' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
protected section.
private section.
ENDCLASS.



CLASS /MBTOOLS/CX_REGISTRY_ERR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = /MBTOOLS/CX_REGISTRY_ERR .
 ENDIF.
  endmethod.
ENDCLASS.
