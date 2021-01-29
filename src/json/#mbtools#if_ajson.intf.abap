INTERFACE /mbtools/if_ajson
  PUBLIC .


  INTERFACES /mbtools/if_ajson_reader .
  INTERFACES /mbtools/if_ajson_writer .

  ALIASES array_to_string_table
    FOR /mbtools/if_ajson_reader~array_to_string_table .
  ALIASES clear
    FOR /mbtools/if_ajson_writer~clear .
  ALIASES delete
    FOR /mbtools/if_ajson_writer~delete .
  " METHODS (merged from reader/writer), maybe will completely move to this IF in future !
  ALIASES exists
    FOR /mbtools/if_ajson_reader~exists .
  ALIASES get
    FOR /mbtools/if_ajson_reader~get .
  ALIASES get_boolean
    FOR /mbtools/if_ajson_reader~get_boolean .
  ALIASES get_date
    FOR /mbtools/if_ajson_reader~get_date .
  ALIASES get_integer
    FOR /mbtools/if_ajson_reader~get_integer .
  ALIASES get_number
    FOR /mbtools/if_ajson_reader~get_number .
  ALIASES get_string
    FOR /mbtools/if_ajson_reader~get_string .
  ALIASES members
    FOR /mbtools/if_ajson_reader~members .
  ALIASES push
    FOR /mbtools/if_ajson_writer~push .
  ALIASES set
    FOR /mbtools/if_ajson_writer~set .
  ALIASES set_boolean
    FOR /mbtools/if_ajson_writer~set_boolean .
  ALIASES set_date
    FOR /mbtools/if_ajson_writer~set_date .
  ALIASES set_integer
    FOR /mbtools/if_ajson_writer~set_integer .
  ALIASES set_null
    FOR /mbtools/if_ajson_writer~set_null .
  ALIASES set_string
    FOR /mbtools/if_ajson_writer~set_string .
  ALIASES slice
    FOR /mbtools/if_ajson_reader~slice .
  ALIASES touch_array
    FOR /mbtools/if_ajson_writer~touch_array .
  ALIASES to_abap
    FOR /mbtools/if_ajson_reader~to_abap .

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
    END OF ty_path_name .

************************************************************************
* MBT AJSON Interface
*
* Original Author: Copyright (c) 2020 Alexander Tsybulsky
* https://github.com/sbcgua/ajson
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************
  CONSTANTS version TYPE string VALUE 'v1.0.3' ##NO_TEXT.
  CONSTANTS origin TYPE string VALUE 'https://github.com/sbcgua/ajson' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF node_type,
      boolean TYPE string VALUE 'bool',
      string  TYPE string VALUE 'str',
      number  TYPE string VALUE 'num',
      null    TYPE string VALUE 'null',
      array   TYPE string VALUE 'array',
      object  TYPE string VALUE 'object',
    END OF node_type .
  " DATA
  DATA mt_json_tree TYPE ty_nodes_ts READ-ONLY .

  " METHODS
  METHODS freeze .
  METHODS keep_item_order .
ENDINTERFACE.
