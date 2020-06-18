*"* components of interface IF_MESSAGE
interface IF_MESSAGE
  public .


  methods GET_TEXT
    returning
      value(RESULT) type STRING .
  methods GET_LONGTEXT
    importing
      value(PRESERVE_NEWLINES) type ABAP_BOOL optional
    returning
      value(RESULT) type STRING .
endinterface.
