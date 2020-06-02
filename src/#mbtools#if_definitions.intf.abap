************************************************************************
* /MBTOOLS/IF_DEFINITIONS
* MBT Definitions
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
INTERFACE /mbtools/if_definitions
  PUBLIC .

  TYPES:
    ty_pgmid TYPE adir_key-pgmid. "char 4

  TYPES:
    ty_object       TYPE adir_key-object, "char 4
    ty_objects      TYPE STANDARD TABLE OF ty_object WITH DEFAULT KEY,
    ty_object_range TYPE RANGE OF ty_object.

  TYPES:
    ty_name       TYPE adir_key-obj_name, "char 40
    ty_names      TYPE STANDARD TABLE OF ty_name WITH DEFAULT KEY,
    ty_name_range TYPE RANGE OF ty_name.

  TYPES:
    ty_longname TYPE trobj_name. "char 120

  TYPES:
    ty_tadir_key  TYPE adir_key,
    ty_tadir_keys TYPE STANDARD TABLE OF ty_tadir_key WITH DEFAULT KEY.

  TYPES:
    ty_object_ext  TYPE /mbtools/object_with_icon_text,
    ty_objects_ext TYPE STANDARD TABLE OF ty_object_ext WITH DEFAULT KEY.

  TYPES:
    ty_object_text  TYPE ko100,
    ty_object_texts TYPE STANDARD TABLE OF ko100 WITH DEFAULT KEY.

ENDINTERFACE.
