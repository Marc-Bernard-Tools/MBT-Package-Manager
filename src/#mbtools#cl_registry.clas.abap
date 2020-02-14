************************************************************************
* /MBTOOLS/CL_REGISTRY
* MBT Registry
*
* Implementation of a registry for storing arbitrary values (similar
* to the MS Windows registry)
* Author: Martin Ceronio (2015), http://ceronio.net
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************
class /MBTOOLS/CL_REGISTRY definition
  public
  create protected .

public section.

  types:
    BEGIN OF ts_keyval,
        key   TYPE string,
        value TYPE string,
      END OF ts_keyval .
  types:
    tt_keyval TYPE SORTED TABLE OF ts_keyval WITH UNIQUE KEY key .
  types:
* For keeping track of references to sub-entries, we maintain a shadow
* table with the same keys
    BEGIN OF ts_keyobj,
        key   TYPE string,
        value TYPE REF TO /mbtools/cl_registry,
      END OF ts_keyobj .
  types:
    tt_keyobj TYPE SORTED TABLE OF ts_keyobj WITH UNIQUE KEY key .

  constants C_VERSION type STRING value '1.1.0' ##NO_TEXT.
* Predefined key for the registry root:
  class-data REGISTRY_ROOT type INDX_SRTFD read-only value 'REGISTRY_ROOT' ##NO_TEXT.
  data SUB_ENTRIES type TT_KEYVAL read-only .
  data VALUES type TT_KEYVAL read-only .
  data INTERNAL_KEY type INDX_SRTFD read-only .
  data PARENT_KEY type INDX_SRTFD read-only .
  data ENTRY_ID type STRING read-only .                   "User-friendly ID of the subnode

  methods CONSTRUCTOR
    importing
      !INTERNAL_KEY type ANY .
  methods RELOAD .
*      lock raising /mbtools/cx_registry_err,
* Saves entry and all dirty sub-entries
  methods SAVE
    raising
      /MBTOOLS/CX_REGISTRY_ERR .
  methods GET_PARENT
    returning
      value(PARENT) type ref to /MBTOOLS/CL_REGISTRY .
  methods CREATE_BY_PATH
    importing
      !PATH type STRING
    returning
      value(ENTRY) type ref to /MBTOOLS/CL_REGISTRY
    raising
      /MBTOOLS/CX_REGISTRY_ERR .
*--------------------------------------------------------------------*
* Methods dealing with sub-entries of the registry entry
  methods GET_SUBENTRY
    importing
      !KEY type CLIKE
    returning
      value(ENTRY) type ref to /MBTOOLS/CL_REGISTRY .
  methods ADD_SUBENTRY
    importing
      !KEY type CLIKE
    returning
      value(ENTRY) type ref to /MBTOOLS/CL_REGISTRY
    raising
      /MBTOOLS/CX_REGISTRY_ENTRY_EX .
* Removes sub-entry and all entries underneath
  methods REMOVE_SUBENTRY
    importing
      !KEY type CLIKE
    raising
      /MBTOOLS/CX_REGISTRY_ERR .
  methods REMOVE_SUBENTRIES
    raising
      /MBTOOLS/CX_REGISTRY_ERR .
  methods COPY_SUBENTRY
    importing
      !SOURCE_KEY type CLIKE
      !TARGET_KEY type CLIKE
    returning
      value(TARGET_ENTRY) type ref to /MBTOOLS/CL_REGISTRY
    raising
      /MBTOOLS/CX_REGISTRY_ERR .
  methods GET_SUBENTRY_KEYS
    returning
      value(KEYS) type STRING_TABLE .
  methods GET_SUBENTRIES
    returning
      value(SUB_ENTRIES) type TT_KEYOBJ .
* Methods for dealing with values in the registry entry:
* Get keys of all values
  methods GET_VALUE_KEYS
    returning
      value(KEYS) type STRING_TABLE .
* Get all values
  methods GET_VALUES
    returning
      value(VALUES) type TT_KEYVAL .
* Set all values in one go:
  methods SET_VALUES
    importing
      !VALUES type TT_KEYVAL .
* Get single value by key
  methods GET_VALUE
    importing
      !KEY type CLIKE
    returning
      value(VALUE) type STRING
    raising
      /MBTOOLS/CX_REGISTRY_NOENTRY .
* Set/overwrite single value
  methods SET_VALUE
    importing
      !KEY type CLIKE
      !VALUE type ANY .
* Delete single value by key
  methods DELETE_VALUE
    importing
      !KEY type CLIKE .
  class-methods GET_ENTRY_BY_INTERNAL_KEY
    importing
      !KEY type ANY
    returning
      value(ENTRY) type ref to /MBTOOLS/CL_REGISTRY .
  class-methods GET_ROOT
    returning
      value(ROOT) type ref to /MBTOOLS/CL_REGISTRY .
  methods EXPORT
    changing
      !C_FILE type STRING_TABLE .
  methods GET_SUBENTRY_BY_PATH
    importing
      !PATH type STRING
    returning
      value(ENTRY) type ref to /MBTOOLS/CL_REGISTRY
    raising
      /MBTOOLS/CX_REGISTRY_ERR .
protected section.

  data DELETED type ABAP_BOOL .
*    data: sub_entrobj type tt_keyobj.
* Class-wide buffer of instances of registry entries
  class-data REGISTRY_ENTRIES type TT_KEYOBJ .
  data READ_ONLY type ABAP_BOOL .

  methods SET_OPTIMISTIC_LOCK
    raising
      /MBTOOLS/CX_REGISTRY_LOCK .
  methods PROMOTE_LOCK
    raising
      /MBTOOLS/CX_REGISTRY_LOCK .
  methods RELEASE_LOCK .
  methods COPY_SUBENTRY_DEEP
    importing
      !SOURCE type ref to /MBTOOLS/CL_REGISTRY
      !TARGET type ref to /MBTOOLS/CL_REGISTRY .
* Remove the registry entry from the database:
* The DELETE method is protected because you must always delete an entry
* as the sub-entry of its parent so that the link is removed from the
* parent
  methods DELETE
    raising
      /MBTOOLS/CX_REGISTRY_ERR .
  PRIVATE SECTION.
ENDCLASS.



CLASS /MBTOOLS/CL_REGISTRY IMPLEMENTATION.


  METHOD add_subentry.
*--------------------------------------------------------------------*
* ADD_SUBENTRY - add a child entry with new key and save
*--------------------------------------------------------------------*
    DATA: kv TYPE ts_keyval.
    DATA: ko TYPE ts_keyobj. "Shadow table with object references

* Prevent any changes if this entry is marked as deleted
    IF me->deleted = abap_true.
      RAISE EXCEPTION TYPE /mbtools/cx_registry_entry_del.
    ENDIF.

* Check that only allowed characters are used. Will help for making
* sensible paths and string handling in other applications
* Most of all, we want to avoid spaces and slashes (although those
* square and curly brackets could cause problems for JSON...)
    IF NOT key CO 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890@#$%^_+-(){}[]'.
      RAISE EXCEPTION TYPE /mbtools/cx_registry_invalid.
    ENDIF.

* Read internal store of sub-entries
    READ TABLE sub_entries INTO kv WITH KEY key = key.
    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE /mbtools/cx_registry_entry_ex.
    ENDIF.

* Create unique ID for key in INDX for the new entry
    kv-key = key.
    TRY.
        kv-value = cl_system_uuid=>create_uuid_c22_static( ).
      CATCH cx_uuid_error.
        RAISE EXCEPTION TYPE /mbtools/cx_registry_err.
    ENDTRY.
    INSERT kv INTO TABLE sub_entries.

* Create an entry on the database for the new entry
    DATA: lt_empty_vals TYPE tt_keyval.
    DATA: lv_srtfd TYPE indx_srtfd.
    lv_srtfd = kv-value.
    EXPORT values = lt_empty_vals sub_entries = lt_empty_vals
      parent = internal_key entry_id = key
      TO DATABASE /mbtools/indx(zr) ID lv_srtfd.

    CREATE OBJECT entry
      EXPORTING
        internal_key = kv-value.
* Will insert itself into registry entries

** Set current entry as the parent of the new entry
*    entry->parent_key = internal_key.
** Set short ID on the new entry
*    entry->entry_id = key.
** Save the new entry
*    entry->save( ).

* Save the current entry to update the list of sub-keys
    save( ).

  ENDMETHOD.                    "add_subentry


  METHOD constructor.
*--------------------------------------------------------------------*
* CONSTRUCTOR - new instance of registry key
*--------------------------------------------------------------------*

    me->internal_key = internal_key.

* Load the entry from the database
    reload( ).

* Object inserts itself into registry of entries
    DATA: ko TYPE ts_keyobj.
    ko-key = me->internal_key.
    ko-value = me.
    INSERT ko INTO TABLE registry_entries.

  ENDMETHOD.                    "constructor


  METHOD copy_subentry.
*--------------------------------------------------------------------*
* COPY_SUBENTRY - copy a child registry entry at the same level,
*  including all values, by passing a source and target key
*--------------------------------------------------------------------*
    DATA: source_entry TYPE REF TO /mbtools/cl_registry.

* Prevent any changes if this entry is marked as deleted
    IF me->deleted = abap_true.
      RAISE EXCEPTION TYPE /mbtools/cx_registry_entry_del.
    ENDIF.

    source_entry = get_subentry( source_key ).
    IF source_entry IS NOT BOUND.
      RAISE EXCEPTION TYPE /mbtools/cx_registry_noentry.
    ENDIF.
    target_entry = add_subentry( target_key ).

* Using the source and the new target, do a deep copy that includes
* copies of sub-entries and values
    copy_subentry_deep( source = source_entry target = target_entry ).

  ENDMETHOD.                    "copy_subentry


  METHOD copy_subentry_deep.
*--------------------------------------------------------------------*
* COPY_SUBENTRY_DEEP - (protected) - copy a branch of the registry
*         at the same level, including all values
*--------------------------------------------------------------------*
    DATA: ls_subentry TYPE ts_keyval.
    DATA: lr_source TYPE REF TO /mbtools/cl_registry.
    DATA: lr_target TYPE REF TO /mbtools/cl_registry.

* Copy values from source to target
    target->values = source->values.

* Copy sub-entries from source to target
    LOOP AT source->sub_entries INTO ls_subentry.
      lr_source = source->get_subentry( ls_subentry-key ).
      lr_target = target->add_subentry( ls_subentry-key ).
      copy_subentry_deep( source = lr_source target = lr_target ).
    ENDLOOP.

* Ensure that values are also saved
    save( ).

  ENDMETHOD.                    "copy_subentry_deep


  METHOD create_by_path.
*--------------------------------------------------------------------*
* CREATE_BY_PATH - convenience method, analogous to mkdir -p that
* allows you to create a path of registry entries if they do not yet
* exist; paths must be separated by forward slash ('/')
* Sub-entries are created from the current registry entry
*--------------------------------------------------------------------*
    DATA: keys TYPE string_table.
    DATA: key TYPE string.
    DATA: sub_entry TYPE REF TO /mbtools/cl_registry.

    SPLIT path AT '/' INTO TABLE keys.
    DELETE keys WHERE table_line IS INITIAL.

    entry = me.
    LOOP AT keys INTO key.
      sub_entry = entry->get_subentry( key ).
      IF sub_entry IS NOT BOUND.
        sub_entry = entry->add_subentry( key ).
      ENDIF.
      entry = sub_entry.
    ENDLOOP.
* After successful processing of chain, ENTRY will
* contain the last-created node

  ENDMETHOD.                    "create_by_path


  METHOD delete.
*--------------------------------------------------------------------*
* DELETE - delete the current entry from the database and mark it,
*          preventing any further operations on this entry
*--------------------------------------------------------------------*

    DATA: sub_entry TYPE ts_keyval.
    DATA: entry TYPE REF TO /mbtools/cl_registry.

* Prevent any changes if this entry is marked as deleted
    IF me->deleted = abap_true.
      RAISE EXCEPTION TYPE /mbtools/cx_registry_entry_del.
    ENDIF.

* Delete all sub-entries before deleting this entry
    LOOP AT sub_entries INTO sub_entry.
      entry = get_subentry( sub_entry-key ).
      entry->delete( ).
      DELETE sub_entries.
    ENDLOOP.

* Remove DB entry for the current entry
    promote_lock( ).
    DELETE FROM DATABASE /mbtools/indx(zr) ID internal_key.
* Object removes itself from the global table too so that that reference no longer exists
    DELETE registry_entries WHERE key = internal_key.
* Set the object to deleted to prevent any operations on any remaining
* references to the object
    deleted = abap_true.

* Release lock held on this key
    release_lock( ).

  ENDMETHOD.                    "delete


  METHOD delete_value.
*--------------------------------------------------------------------*
* DELETE_VALUE - remove single value by key
*--------------------------------------------------------------------*
    DATA: kv TYPE ts_keyval.
* Prevent any changes if this entry is marked as deleted
    IF me->deleted = abap_true.
      RAISE EXCEPTION TYPE /mbtools/cx_registry_entry_del.
    ENDIF.

    kv-key = key.
    DELETE values WHERE key = kv-key.
  ENDMETHOD.                    "delete_value


  METHOD export.
*>>>INS
    DATA:
      reg_entry TYPE REF TO /mbtools/cl_registry,
      kv        TYPE ts_keyval,
      id        TYPE string,
      file_line TYPE string.

*   Export key header
    reg_entry = me.
    DO.
      id = reg_entry->entry_id.
      IF file_line IS INITIAL.
        file_line = id.
      ELSE.
        CONCATENATE id file_line INTO file_line SEPARATED BY '/'.
      ENDIF.
      IF id = registry_root.
        EXIT.
      ELSE.
        reg_entry = reg_entry->get_parent( ).
      ENDIF.
    ENDDO.
    CONCATENATE '[' file_line ']' INTO file_line.
    APPEND file_line TO c_file.

*   Export key values
    LOOP AT values INTO kv.
      CONCATENATE '"' kv-key '"="' kv-value '"' INTO file_line.
      APPEND file_line TO c_file.
    ENDLOOP.
    APPEND '' TO c_file.

*   Export sub entries
    LOOP AT sub_entries INTO kv.
      reg_entry = get_subentry( kv-key ).

      CALL METHOD reg_entry->export
        CHANGING
          c_file = c_file.
    ENDLOOP.
*<<<INS
  ENDMETHOD.


  METHOD get_entry_by_internal_key.
*--------------------------------------------------------------------*
* GET_ENTRY_BY_INTERNAL_KEY - retrieve reg. entry by internal ID
*--------------------------------------------------------------------*
    DATA: ko TYPE ts_keyobj.

* Search global index of registry entry instances
    READ TABLE registry_entries INTO ko WITH KEY key = key.

    IF sy-subrc = 0.
* Reference already exists; return that
      entry = ko-value.
    ELSE.
* Create new reference to sub-entry
      CREATE OBJECT entry
        EXPORTING
          internal_key = key.
* Will insert itself into registry entries
    ENDIF.
  ENDMETHOD.                    "get_entry_by_internal_key


  METHOD get_parent.
*--------------------------------------------------------------------*
* GET_PARENT - retrieve parent entry of this entry
*--------------------------------------------------------------------*
    parent = get_entry_by_internal_key( parent_key ).
  ENDMETHOD.                    "get_parent


  METHOD get_root.
*--------------------------------------------------------------------*
* GET_ROOT - retrieve root entry of registry
*--------------------------------------------------------------------*

* If the root doesn't exist yet, create it
    DATA: values TYPE tt_keyval.
    DATA: sub_entries TYPE tt_keyval.
    DATA: parent_key TYPE indx_srtfd VALUE space.
    DATA: entry_id TYPE string.
    IMPORT values = values sub_entries = sub_entries
      FROM DATABASE /mbtools/indx(zr) ID registry_root.
    IF sy-subrc NE 0.
      entry_id = registry_root.
      EXPORT values = values sub_entries = sub_entries parent = parent_key entry_id = entry_id
        TO DATABASE /mbtools/indx(zr) ID registry_root.
    ENDIF.

* Retrieve the root entry of the registry
    root = get_entry_by_internal_key( registry_root ).

  ENDMETHOD.                    "get_root


  METHOD get_subentries.
*--------------------------------------------------------------------*
* GET_SUBENTRIES - return immediate children registry entries
*--------------------------------------------------------------------*
    DATA: ko TYPE ts_keyobj.
    DATA: subkeys TYPE string_table.
    DATA: subkey TYPE string.

    subkeys = get_subentry_keys( ).
    LOOP AT subkeys INTO subkey.
      ko-key = subkey.
      ko-value = get_subentry( subkey ).
      INSERT ko INTO TABLE sub_entries.
    ENDLOOP.

  ENDMETHOD.                    "get_subentries


  METHOD get_subentry.
*--------------------------------------------------------------------*
* GET_SUBENTRY - return single child entry by key
*--------------------------------------------------------------------*
    DATA: kv TYPE ts_keyval.
    DATA: ko TYPE ts_keyobj.

* Read internal store of sub-entries
    READ TABLE sub_entries INTO kv WITH KEY key = key.
    IF sy-subrc NE 0.
* Entry does not exist; exit
      RETURN.
    ENDIF.

* Search global index of registry entry instances
    READ TABLE registry_entries INTO ko WITH KEY key =  kv-value.

*    read table sub_entrobj into ko with key key = kv-value.
    IF sy-subrc = 0.
* Reference already exists; return that
      entry = ko-value.
    ELSE.
* Create new reference to sub-entry
      CREATE OBJECT entry
        EXPORTING
          internal_key = kv-value.
* Will insert itself into registry entries
    ENDIF.

  ENDMETHOD.                    "get_subentry


  METHOD get_subentry_by_path.
*>>>INS
*--------------------------------------------------------------------*
* GET_SUBENTRY_BY_PATH - convenience method, analogous to create_by_path
* allows you to get an entry by a path of registry entries;
* paths must be separated by forward slash ('/')
*--------------------------------------------------------------------*
    DATA: keys TYPE string_table.
    DATA: key TYPE string.
    DATA: sub_entry TYPE REF TO /mbtools/cl_registry.

    SPLIT path AT '/' INTO TABLE keys.
    DELETE keys WHERE table_line IS INITIAL.

    entry = me.
    LOOP AT keys INTO key.
      sub_entry = entry->get_subentry( key ).
      IF sub_entry IS NOT BOUND.
        RAISE EXCEPTION TYPE /mbtools/cx_registry_err.
      ENDIF.
      entry = sub_entry.
    ENDLOOP.
* After successful processing of chain, ENTRY will
* contain the last node
*<<<INS
  ENDMETHOD. "get_subentry_by_path.


  METHOD get_subentry_keys.
*--------------------------------------------------------------------*
* GET_SUBENTRY_KEYS - retrieve keys of all child registry entries
*--------------------------------------------------------------------*
    DATA: kv TYPE ts_keyval.
    LOOP AT sub_entries INTO kv.
      APPEND kv-key TO keys.
    ENDLOOP.
  ENDMETHOD.                    "get_subentry_keys


  METHOD get_value.
*--------------------------------------------------------------------*
* GET_VALUE - return a single value by key
*--------------------------------------------------------------------*
    DATA: kv TYPE ts_keyval.
    READ TABLE values INTO kv WITH KEY key = key.
    IF sy-subrc = 0.
      value = kv-value.
    ENDIF.
  ENDMETHOD.                    "get_value


  METHOD get_values.
*--------------------------------------------------------------------*
* GET_VALUES - retrieve all values at once in key+value table
*--------------------------------------------------------------------*
    values = me->values.
  ENDMETHOD.                    "get_values


  METHOD get_value_keys.
*--------------------------------------------------------------------*
* GET_VALUE_KEYS - retrieve keys of all values
*--------------------------------------------------------------------*
    DATA: kv TYPE ts_keyval.
    LOOP AT values INTO kv.
      APPEND kv-key TO keys.
    ENDLOOP.
  ENDMETHOD.                    "get_value_keys


  METHOD promote_lock.
*--------------------------------------------------------------------*
* PROMOTE_LOCK - Get exclusive lock just before saving
*--------------------------------------------------------------------*
    CALL FUNCTION 'ENQUEUE_/MBTOOLS/ESINDX'
      EXPORTING
        mode_/mbtools/indx = 'R'
        relid              = 'ZR'
        srtfd              = internal_key
      EXCEPTIONS
        foreign_lock       = 1
        system_failure     = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /mbtools/cx_registry_lock.
    ENDIF.
  ENDMETHOD.                    "promote_lock


  METHOD release_lock.
*--------------------------------------------------------------------*
* RELEASE_LOCK - called after deleting or before re-acquiring
*--------------------------------------------------------------------*
    CALL FUNCTION 'DEQUEUE_/MBTOOLS/ESINDX'
      EXPORTING
        relid = 'ZR'
        srtfd = internal_key.
  ENDMETHOD.                    "release_lock


  METHOD reload.
*--------------------------------------------------------------------*
* RELOAD - reload values and sub-entries from database, set new lock
*--------------------------------------------------------------------*
    IMPORT values = me->values sub_entries = me->sub_entries parent = parent_key entry_id = entry_id
      FROM DATABASE /mbtools/indx(zr) ID internal_key.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE /mbtools/cx_registry_noentry.
    ENDIF.

    set_optimistic_lock( ).
  ENDMETHOD.                    "reload


  METHOD remove_subentries.
*--------------------------------------------------------------------*
* REMOVE_SUBENTRIES - remove all child entries of this entry
*--------------------------------------------------------------------*
    DATA: kv TYPE ts_keyval.
    DATA: ko TYPE ts_keyobj. "Shadow table with object references

* Prevent any changes if this entry is marked as deleted
    IF me->deleted = abap_true.
      RAISE EXCEPTION TYPE /mbtools/cx_registry_entry_del.
    ENDIF.

    LOOP AT sub_entries INTO kv.
      remove_subentry( kv-key ).
    ENDLOOP.
  ENDMETHOD.                    "remove_subentries


  METHOD remove_subentry.
*--------------------------------------------------------------------*
* REMOVE_SUBENTRY - remove a single child registry entry by key
*--------------------------------------------------------------------*
    DATA: kv TYPE ts_keyval.
    FIELD-SYMBOLS: <ko> TYPE ts_keyobj. "Shadow table with object references
    DATA: sub_entry TYPE REF TO /mbtools/cl_registry.

* Prevent any changes if this entry is marked as deleted
    IF me->deleted = abap_true.
      RAISE EXCEPTION TYPE /mbtools/cx_registry_entry_del.
    ENDIF.

* Read internal store of sub-entries
    READ TABLE sub_entries INTO kv WITH KEY key = key.
    IF sy-subrc NE 0.
* Entry does not exist; exit with error
      RAISE EXCEPTION TYPE /mbtools/cx_registry_noentry.
    ENDIF.

* Remove all sub-entries of the sub-entry before removing the sub-entry
    sub_entry = get_subentry( key ).
    IF sub_entry IS BOUND.

* Delete the sub_entry (which deletes its sub-entries)
      sub_entry->delete( ).
* Remove entry from sub-entry table and shadow table
      kv-key = key.
      DELETE sub_entries WHERE key = kv-key.

      save( ). "Save current entry to remove subentry that has been removed
    ENDIF.

  ENDMETHOD.                    "remove_subentry


  METHOD save.
*--------------------------------------------------------------------*
* SAVE - save the current entry, with concurrency control
*--------------------------------------------------------------------*
* Prevent any changes if this entry is marked as deleted
    IF me->deleted = abap_true.
      RAISE EXCEPTION TYPE /mbtools/cx_registry_entry_del.
    ENDIF.

    promote_lock( ).
    EXPORT values = me->values sub_entries = me->sub_entries parent = parent_key entry_id = entry_id
      TO DATABASE /mbtools/indx(zr) ID internal_key.
    set_optimistic_lock( ).
  ENDMETHOD.                    "save


  METHOD set_optimistic_lock.
*--------------------------------------------------------------------*
* SET_OPTIMISTIC_LOCK - always set when (re-)reading an entry
*--------------------------------------------------------------------*
* Existing lock must be released before acquiring a new one
    release_lock( ).
    CALL FUNCTION 'ENQUEUE_/MBTOOLS/ESINDX'
      EXPORTING
        mode_/mbtools/indx = 'O'
        relid              = 'ZR'
        srtfd              = internal_key
      EXCEPTIONS
        foreign_lock       = 1
        system_failure     = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /mbtools/cx_registry_lock.
    ENDIF.
  ENDMETHOD.                    "set_optimistic_lock


  METHOD set_value.
*--------------------------------------------------------------------*
* SET_VALUE - store a single value by key
*--------------------------------------------------------------------*
    DATA: kv TYPE ts_keyval.

* Prevent any changes if this entry is marked as deleted
    IF me->deleted = abap_true.
      RAISE EXCEPTION TYPE /mbtools/cx_registry_entry_del.
    ENDIF.

* Add the value to set of values if not existing or change if it does exist
    READ TABLE values INTO kv WITH KEY key = key.
    IF sy-subrc NE 0.
      kv-key = key.
      kv-value = value.
      INSERT kv INTO TABLE values.
    ELSE.
      kv-value = value.
      MODIFY TABLE values FROM kv.
    ENDIF.
  ENDMETHOD.                    "set_value


  METHOD set_values.
*--------------------------------------------------------------------*
* SET_VALUES - set all values at once with key+value table
*--------------------------------------------------------------------*
* Prevent any changes if this entry is marked as deleted
    IF me->deleted = abap_true.
      RAISE EXCEPTION TYPE /mbtools/cx_registry_entry_del.
    ENDIF.

    me->values = values.
  ENDMETHOD.                    "set_values
ENDCLASS.