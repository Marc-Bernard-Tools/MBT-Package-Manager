*"* components of interface IF_T100_MESSAGE
interface IF_T100_MESSAGE
  public .


  interfaces IF_MESSAGE .

  constants:
    begin of default_textid,
      msgid type symsgid value 'SY',
      msgno type symsgno value '530',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of default_textid.

  data T100KEY type SCX_T100KEY .
endinterface.
