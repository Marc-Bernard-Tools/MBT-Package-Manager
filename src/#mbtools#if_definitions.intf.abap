************************************************************************
* /MBTOOLS/IF_DEFINITIONS
* MBT Definitions
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
INTERFACE /mbtools/if_definitions
  PUBLIC .

  TYPES:
    ty_icon     TYPE c LENGTH 4, " icon_d
    ty_text     TYPE c LENGTH 60, " ddtext
    ty_longname TYPE c LENGTH 120. "trobj_name

  TYPES:
    BEGIN OF ty_tadir_key, " adir_key
      pgmid    TYPE c LENGTH 4,
      object   TYPE c LENGTH 4,
      obj_name TYPE c LENGTH 40,
    END OF ty_tadir_key,
    ty_tadir_keys TYPE STANDARD TABLE OF ty_tadir_key WITH DEFAULT KEY.

  TYPES:
    ty_pgmid TYPE ty_tadir_key-pgmid.

  TYPES:
    ty_object       TYPE ty_tadir_key-object,
    ty_objects      TYPE STANDARD TABLE OF ty_object WITH DEFAULT KEY,
    ty_object_range TYPE RANGE OF ty_object.

  TYPES:
    ty_name       TYPE ty_tadir_key-obj_name,
    ty_names      TYPE STANDARD TABLE OF ty_name WITH DEFAULT KEY,
    ty_name_range TYPE RANGE OF ty_name.

  TYPES:
    BEGIN OF ty_object_ext, " /mbtools/object_with_icon_text,
      icon     TYPE ty_icon,
      pgmid    TYPE ty_tadir_key-pgmid,
      object   TYPE ty_tadir_key-object,
      obj_name TYPE ty_tadir_key-obj_name,
      text     TYPE ty_text,
    END OF ty_object_ext,
    ty_objects_ext TYPE STANDARD TABLE OF ty_object_ext WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_object_text, " ko100
      pgmid  TYPE ty_tadir_key-pgmid,
      object TYPE ty_tadir_key-object,
      text   TYPE ty_text,
    END OF ty_object_text,
    ty_object_texts TYPE STANDARD TABLE OF ty_object_text WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_regs, " /mbtools/regs
      relid  TYPE c LENGTH 2,
      srtfd  TYPE c LENGTH 22,
      srtf2  TYPE i,
      chdate TYPE d,
      chtime TYPE t,
      chname TYPE c LENGTH 12,
      clustr TYPE indx_clstr,
      clustd TYPE indx_clust,
    END OF ty_regs.

ENDINTERFACE.
