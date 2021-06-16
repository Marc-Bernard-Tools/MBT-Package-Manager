CLASS lcl_mapping_fields DEFINITION.

  PUBLIC SECTION.
    INTERFACES /mbtools/if_ajson_mapping.

    ALIASES to_abap FOR /mbtools/if_ajson_mapping~to_abap.
    ALIASES to_json FOR /mbtools/if_ajson_mapping~to_json.

    METHODS constructor
      IMPORTING
        it_mapping_fields TYPE /mbtools/if_ajson_mapping~ty_mapping_fields OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mt_mapping_fields TYPE /mbtools/if_ajson_mapping~ty_mapping_fields.

ENDCLASS.


CLASS lcl_mapping_to_upper DEFINITION.

  PUBLIC SECTION.
    INTERFACES /mbtools/if_ajson_mapping.

    METHODS constructor
      IMPORTING
        it_mapping_fields TYPE /mbtools/if_ajson_mapping~ty_mapping_fields OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mi_mapping_fields TYPE REF TO /mbtools/if_ajson_mapping.

ENDCLASS.


CLASS lcl_mapping_to_lower DEFINITION.

  PUBLIC SECTION.
    INTERFACES /mbtools/if_ajson_mapping.

    METHODS constructor
      IMPORTING
        it_mapping_fields TYPE /mbtools/if_ajson_mapping~ty_mapping_fields OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mi_mapping_fields TYPE REF TO /mbtools/if_ajson_mapping.

ENDCLASS.


CLASS lcl_mapping_camel DEFINITION.

  PUBLIC SECTION.
    INTERFACES /mbtools/if_ajson_mapping.

    METHODS constructor
      IMPORTING
        it_mapping_fields   TYPE /mbtools/if_ajson_mapping~ty_mapping_fields OPTIONAL
        iv_first_json_upper TYPE abap_bool DEFAULT abap_true.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_first_json_upper TYPE abap_bool.
    DATA mi_mapping_fields TYPE REF TO /mbtools/if_ajson_mapping.

ENDCLASS.
