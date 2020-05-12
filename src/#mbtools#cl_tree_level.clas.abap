CLASS /mbtools/cl_tree_level DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      level TYPE i READ-ONLY,
      icon  TYPE icon_d,
      value TYPE /mbtools/tree_control-value,
      text  TYPE /mbtools/tree_control-text.
    METHODS constructor
      IMPORTING
        ir_app TYPE REF TO /mbtools/cl_tree.
    METHODS next.
    METHODS back.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mr_app  TYPE REF TO /mbtools/cl_tree,
      mv_root TYPE lvc_nkey.
ENDCLASS.



CLASS /MBTOOLS/CL_TREE_LEVEL IMPLEMENTATION.


  METHOD back.
    mr_app->set_key( mv_root ).
  ENDMETHOD.


  METHOD constructor.
    mr_app = ir_app.
    mv_root = mr_app->get_key( ).
  ENDMETHOD.


  METHOD next.
    mr_app->next_key( ).
  ENDMETHOD.
ENDCLASS.
