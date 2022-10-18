CLASS /mbtools/cl_stack DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* Marc Bernard Tools - Stack
*
* Based on https://blogs.sap.com/2017/03/25/noitab-a-stack/
* by Jacques Nomssi Nzali
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************
  PUBLIC SECTION.
    METHODS push
      IMPORTING
        iv_key TYPE string.
    METHODS pop
      RETURNING
        VALUE(rv_key) TYPE string.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_node,
        key  TYPE string,
        next TYPE REF TO data,
      END OF ty_node.

    DATA mo_top TYPE REF TO ty_node.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_stack IMPLEMENTATION.


  METHOD pop.
    CLEAR rv_key. " design choice: empty stack does not abort processing
    CHECK mo_top IS BOUND.
    rv_key  = mo_top->key.
    mo_top ?= mo_top->next.
  ENDMETHOD.


  METHOD push.
    DATA lo_node TYPE REF TO data.
    lo_node = mo_top.
    CREATE DATA mo_top.
    mo_top->key  = iv_key.
    mo_top->next = lo_node.
  ENDMETHOD.
ENDCLASS.
