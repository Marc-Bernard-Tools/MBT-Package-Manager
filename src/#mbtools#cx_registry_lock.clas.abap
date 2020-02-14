class /MBTOOLS/CX_REGISTRY_LOCK definition
  public
  inheriting from /MBTOOLS/CX_REGISTRY_ERR
  final
  create public .

public section.

  constants /MBTOOLS/CX_REGISTRY_LOCK type SOTR_CONC value '000D3A5598F91EEA8CDD1CDD4367C481' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
protected section.
private section.
ENDCLASS.



CLASS /MBTOOLS/CX_REGISTRY_LOCK IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = /MBTOOLS/CX_REGISTRY_LOCK .
 ENDIF.
  endmethod.
ENDCLASS.
