CLASS /mbtools/cl_tree_level DEFINITION
  PUBLIC
  CREATE PUBLIC .
************************************************************************
* MBT Tree Level
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

  PUBLIC SECTION.

    DATA level TYPE i READ-ONLY .
    DATA icon TYPE icon_d ##NEEDED.
    DATA value TYPE /mbtools/tree_control-value ##NEEDED.
    DATA text TYPE /mbtools/tree_control-text ##NEEDED.

    METHODS constructor
      IMPORTING
        !io_tree  TYPE REF TO /mbtools/cl_tree
        !iv_level TYPE i.
    METHODS next .
    METHODS back .

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA:
      mo_tree TYPE REF TO /mbtools/cl_tree,
      mv_root TYPE lvc_nkey.

ENDCLASS.



CLASS /mbtools/cl_tree_level IMPLEMENTATION.


  METHOD back.
    mo_tree->set_key( mv_root ).
    level = level - 1.
  ENDMETHOD.


  METHOD constructor.
    mo_tree = io_tree.
    mv_root = mo_tree->get_key( ).
    level   = iv_level.
  ENDMETHOD.


  METHOD next.
    mo_tree->next_key( ).
    level = level + 1.
  ENDMETHOD.
ENDCLASS.
