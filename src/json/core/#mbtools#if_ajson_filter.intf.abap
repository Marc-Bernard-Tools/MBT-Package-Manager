INTERFACE /mbtools/if_ajson_filter
  PUBLIC.

************************************************************************
* abap json (AJSON)
*
* Copyright 2020 Alexander Tsybulsky <https://github.com/sbcgua/ajson>
* SPDX-License-Identifier: MIT
************************************************************************

  TYPES ty_filter_tab TYPE STANDARD TABLE OF REF TO /mbtools/if_ajson_filter WITH KEY table_line.
  TYPES ty_visit_type TYPE i.

  CONSTANTS:
    BEGIN OF visit_type,
      value TYPE ty_visit_type VALUE 0,
      open  TYPE ty_visit_type VALUE 1,
      close TYPE ty_visit_type VALUE 2,
    END OF visit_type.

  METHODS keep_node
    IMPORTING
      is_node TYPE /mbtools/if_ajson_types=>ty_node
      iv_visit TYPE ty_visit_type DEFAULT visit_type-value
    RETURNING
      VALUE(rv_keep) TYPE abap_bool
    RAISING
      /mbtools/cx_ajson_error.

ENDINTERFACE.
