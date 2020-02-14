class /MBTOOLS/CX_REGISTRY_ENTRY_EX definition
  public
  inheriting from /MBTOOLS/CX_REGISTRY_ERR
  final
  create public .

public section.

  constants /MBTOOLS/CX_REGISTRY_ENTRY_EX type SOTR_CONC value '000D3A5598F91EEA8CDD1810E2078480' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
protected section.
private section.
ENDCLASS.



CLASS /MBTOOLS/CX_REGISTRY_ENTRY_EX IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = /MBTOOLS/CX_REGISTRY_ENTRY_EX .
 ENDIF.
  endmethod.
ENDCLASS.
