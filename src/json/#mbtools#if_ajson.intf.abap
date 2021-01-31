INTERFACE /mbtools/if_ajson
  PUBLIC .

************************************************************************
* MBT AJSON Interface
*
* Original Author: Copyright (c) 2020 Alexander Tsybulsky
* https://github.com/sbcgua/ajson
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************

  CONSTANTS version TYPE string VALUE 'v1.0.3'.
  CONSTANTS origin TYPE string VALUE 'https://github.com/sbcgua/ajson'.

  INTERFACES /mbtools/if_ajson_reader.
  INTERFACES /mbtools/if_ajson_writer.

  CONSTANTS:
    BEGIN OF node_type,
      boolean TYPE string VALUE 'bool',
      string  TYPE string VALUE 'str',
      number  TYPE string VALUE 'num',
      null    TYPE string VALUE 'null',
      array   TYPE string VALUE 'array',
      object  TYPE string VALUE 'object',
    END OF node_type.

  TYPES:
    BEGIN OF ty_node,
      path     TYPE string,
      name     TYPE string,
      type     TYPE string,
      value    TYPE string,
      index    TYPE i,
      order    TYPE i,
      children TYPE i,
    END OF ty_node .
  TYPES:
    ty_nodes_tt TYPE STANDARD TABLE OF ty_node WITH KEY path name .
  TYPES:
    ty_nodes_ts TYPE SORTED TABLE OF ty_node
      WITH UNIQUE KEY path name
      WITH NON-UNIQUE SORTED KEY array_index COMPONENTS path index
      WITH NON-UNIQUE SORTED KEY item_order COMPONENTS path order .
  TYPES:
    BEGIN OF ty_path_name,
      path TYPE string,
      name TYPE string,
    END OF ty_path_name.

  " DATA

  DATA mt_json_tree TYPE ty_nodes_ts READ-ONLY.

  " METHODS

  METHODS freeze.
  METHODS keep_item_order.

  " METHODS (merged from reader/writer), maybe will completely move to this IF in future !

  ALIASES:
    exists FOR /mbtools/if_ajson_reader~exists,
    members FOR /mbtools/if_ajson_reader~members,
    get FOR /mbtools/if_ajson_reader~get,
    get_boolean FOR /mbtools/if_ajson_reader~get_boolean,
    get_integer FOR /mbtools/if_ajson_reader~get_integer,
    get_number FOR /mbtools/if_ajson_reader~get_number,
    get_date FOR /mbtools/if_ajson_reader~get_date,
    get_string FOR /mbtools/if_ajson_reader~get_string,
    slice FOR /mbtools/if_ajson_reader~slice,
    to_abap FOR /mbtools/if_ajson_reader~to_abap,
    array_to_string_table FOR /mbtools/if_ajson_reader~array_to_string_table.

  ALIASES:
    clear FOR /mbtools/if_ajson_writer~clear,
    set FOR /mbtools/if_ajson_writer~set,
    set_boolean FOR /mbtools/if_ajson_writer~set_boolean,
    set_string FOR /mbtools/if_ajson_writer~set_string,
    set_integer FOR /mbtools/if_ajson_writer~set_integer,
    set_date FOR /mbtools/if_ajson_writer~set_date,
    set_null FOR /mbtools/if_ajson_writer~set_null,
    delete FOR /mbtools/if_ajson_writer~delete,
    touch_array FOR /mbtools/if_ajson_writer~touch_array,
    push FOR /mbtools/if_ajson_writer~push,
    stringify FOR /mbtools/if_ajson_writer~stringify.

ENDINTERFACE.
