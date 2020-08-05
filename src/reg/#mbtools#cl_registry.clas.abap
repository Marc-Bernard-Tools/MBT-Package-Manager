CLASS /mbtools/cl_registry DEFINITION
  PUBLIC
  CREATE PROTECTED .
************************************************************************
* MBT Registry
*
* WARNING: DO NOT CHANGE!
* Syntax errors in this class may impact system stability
*
* Implementation of a registry for storing arbitrary values (similar
* to the MS Windows registry)
*
* Original Author: (c) Martin Ceronio (2015), http://ceronio.net
* https://ceronio.net/2015/05/arbitrary-value-store-registry-for-abap/
*
* Released under MIT License: https://opensource.org/licenses/MIT
*
* Ported to namespace and enhanced by Marc Bernard Tools
************************************************************************

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_keyval,
        key   TYPE string,
        value TYPE string,
      END OF ty_keyval .
    TYPES:
      ty_keyvals TYPE SORTED TABLE OF ty_keyval WITH UNIQUE KEY key .
    TYPES:
* For keeping track of references to sub-entries, we maintain a shadow
* table with the same keys
      BEGIN OF ty_keyobj,
        key   TYPE string,
        value TYPE REF TO /mbtools/cl_registry,
      END OF ty_keyobj .
    TYPES:
      ty_keyobjs TYPE SORTED TABLE OF ty_keyobj WITH UNIQUE KEY key .

    CONSTANTS c_version TYPE string VALUE '1.2.0' ##NO_TEXT.
    CONSTANTS c_name TYPE string VALUE 'MBT_Registry' ##NO_TEXT.
* Predefined key for the registry root:
    CONSTANTS c_registry_root TYPE indx_srtfd VALUE 'MARC_BERNARD_TOOLS' ##NO_TEXT.
    DATA mt_sub_entries TYPE ty_keyvals READ-ONLY .
    DATA mt_values TYPE ty_keyvals READ-ONLY .
    DATA mv_internal_key TYPE indx_srtfd READ-ONLY .
    DATA mv_parent_key TYPE indx_srtfd READ-ONLY .
    DATA mv_entry_id TYPE string READ-ONLY .                     "User-friendly ID of the subnode
    DATA ms_regs TYPE /mbtools/if_definitions=>ty_regs READ-ONLY .

    METHODS constructor
      IMPORTING
        !ig_internal_key TYPE any
      RAISING
        /mbtools/cx_exception .
    METHODS reload
      RAISING
        /mbtools/cx_exception .
* Saves entry and all dirty sub-entries
    METHODS save
      RAISING
        /mbtools/cx_exception .
    METHODS get_parent
      RETURNING
        VALUE(ro_parent) TYPE REF TO /mbtools/cl_registry
      RAISING
        /mbtools/cx_exception .
    METHODS create_by_path
      IMPORTING
        !iv_path        TYPE string
      RETURNING
        VALUE(ro_entry) TYPE REF TO /mbtools/cl_registry
      RAISING
        /mbtools/cx_exception .
*--------------------------------------------------------------------*
* Methods dealing with sub-entries of the registry entry
    METHODS get_subentry
      IMPORTING
        !iv_key         TYPE clike
      RETURNING
        VALUE(ro_entry) TYPE REF TO /mbtools/cl_registry
      RAISING
        /mbtools/cx_exception .
    METHODS add_subentry
      IMPORTING
        !iv_key         TYPE clike
      RETURNING
        VALUE(ro_entry) TYPE REF TO /mbtools/cl_registry
      RAISING
        /mbtools/cx_exception .
* Removes sub-entry and all entries underneath
    METHODS remove_subentry
      IMPORTING
        !iv_key TYPE clike
      RAISING
        /mbtools/cx_exception .
    METHODS remove_subentries
      RAISING
        /mbtools/cx_exception .
    METHODS copy_subentry
      IMPORTING
        !iv_source_key         TYPE clike
        !iv_target_key         TYPE clike
      RETURNING
        VALUE(ro_target_entry) TYPE REF TO /mbtools/cl_registry
      RAISING
        /mbtools/cx_exception .
    METHODS get_subentry_keys
      RETURNING
        VALUE(rt_keys) TYPE string_table .
    METHODS get_subentries
      RETURNING
        VALUE(rt_sub_entries) TYPE ty_keyobjs
      RAISING
        /mbtools/cx_exception .
* Methods for dealing with values in the registry entry:
* Get keys of all values
    METHODS get_value_keys
      RETURNING
        VALUE(rt_keys) TYPE string_table .
* Get all values
    METHODS get_values
      RETURNING
        VALUE(rt_values) TYPE ty_keyvals .
* Set all values in one go:
    METHODS set_values
      IMPORTING
        !it_values TYPE ty_keyvals
      RAISING
        /mbtools/cx_exception .
* Get single value by key
    METHODS get_value
      IMPORTING
        !iv_key         TYPE clike
      RETURNING
        VALUE(rv_value) TYPE string .
* Set/overwrite single value
    METHODS set_value
      IMPORTING
        !iv_key   TYPE clike
        !iv_value TYPE any OPTIONAL
      RAISING
        /mbtools/cx_exception .
* Delete single value by key
    METHODS delete_value
      IMPORTING
        !iv_key TYPE clike
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS get_entry_by_internal_key
      IMPORTING
        !iv_key         TYPE any
      RETURNING
        VALUE(ro_entry) TYPE REF TO /mbtools/cl_registry
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS get_root
      RETURNING
        VALUE(ro_root) TYPE REF TO /mbtools/cl_registry
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS truncate .
    METHODS export
      CHANGING
        !ct_file TYPE string_table
      RAISING
        /mbtools/cx_exception .
    METHODS get_subentry_by_path
      IMPORTING
        !iv_path        TYPE string
      RETURNING
        VALUE(ro_entry) TYPE REF TO /mbtools/cl_registry
      RAISING
        /mbtools/cx_exception .
  PROTECTED SECTION.

* Class-wide buffer of instances of registry entries
    CLASS-DATA gt_registry_entries TYPE ty_keyobjs .
    DATA mv_deleted TYPE abap_bool .

    METHODS set_optimistic_lock
      RAISING
        /mbtools/cx_exception.
    METHODS promote_lock
      RAISING
        /mbtools/cx_exception.
    METHODS release_lock .
    METHODS copy_subentry_deep
      IMPORTING
        !io_source TYPE REF TO /mbtools/cl_registry
        !io_target TYPE REF TO /mbtools/cl_registry
      RAISING
        /mbtools/cx_exception.

* Remove the registry entry from the database:
* The DELETE method is protected because you must always delete an entry
* as the sub-entry of its parent so that the link is removed from the
* parent
    METHODS delete
      RAISING
        /mbtools/cx_exception.

  PRIVATE SECTION.

    CONSTANTS: c_relid TYPE indx_relid VALUE 'ZR'.

ENDCLASS.



CLASS /MBTOOLS/CL_REGISTRY IMPLEMENTATION.


  METHOD add_subentry.
*--------------------------------------------------------------------*
* ADD_SUBENTRY - add a child entry with new key and save
*--------------------------------------------------------------------*
    DATA: ls_kv TYPE ty_keyval.
    DATA: lt_empty_vals TYPE ty_keyvals.
    DATA: lv_srtfd TYPE indx_srtfd.
    DATA: ls_regs TYPE /mbtools/if_definitions=>ty_regs.

* Prevent any changes if this entry is marked as deleted
    IF mv_deleted = abap_true.
      /mbtools/cx_exception=>raise( 'Registry entry is marked as deleted'(005) ).
    ENDIF.

* Check that only allowed characters are used. Will help for making
* sensible paths and string handling in other applications
* Most of all, we want to avoid spaces and slashes (although those
* square and curly brackets could cause problems for JSON...)
    IF NOT iv_key CO 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890@#$%^_+-().'.
      /mbtools/cx_exception=>raise( 'Invalid registry key'(007) ).
    ENDIF.

* Read internal store of sub-entries
    READ TABLE mt_sub_entries INTO ls_kv WITH KEY key = iv_key.
    IF sy-subrc = 0.
      /mbtools/cx_exception=>raise( 'Registry entry exists already'(011) ).
    ENDIF.

* Create unique ID for key in INDX for the new entry
    ls_kv-key = iv_key.
    TRY.
        ls_kv-value = cl_system_uuid=>create_uuid_c22_static( ).
      CATCH cx_uuid_error.
        /mbtools/cx_exception=>raise( 'UID error'(010) ).
    ENDTRY.
    INSERT ls_kv INTO TABLE mt_sub_entries.

* Create an entry on the database for the new entry
*>>>INS
    ls_regs-chdate = sy-datum.
    ls_regs-chtime = sy-uzeit.
    ls_regs-chname = sy-uname.
*<<<INS
    lv_srtfd = ls_kv-value.

    EXPORT values = lt_empty_vals sub_entries = lt_empty_vals
      parent = mv_internal_key entry_id = iv_key
      TO DATABASE /mbtools/regs(zr) FROM ls_regs ID lv_srtfd.

    CREATE OBJECT ro_entry
      EXPORTING
        ig_internal_key = ls_kv-value.
* Will insert itself into registry entries

* Save the current entry to update the list of sub-keys
    save( ).

  ENDMETHOD.                    "add_subentry


  METHOD constructor.
*--------------------------------------------------------------------*
* CONSTRUCTOR - new instance of registry key
*--------------------------------------------------------------------*
    DATA: ls_ko TYPE ty_keyobj.

    mv_internal_key = ig_internal_key.

* Load the entry from the database
    reload( ).

* Object inserts itself into registry of entries
    ls_ko-key = mv_internal_key.
    ls_ko-value = me.
    INSERT ls_ko INTO TABLE gt_registry_entries.

  ENDMETHOD.                    "constructor


  METHOD copy_subentry.
*--------------------------------------------------------------------*
* COPY_SUBENTRY - copy a child registry entry at the same level,
*  including all values, by passing a source and target key
*--------------------------------------------------------------------*
    DATA: lo_source_entry TYPE REF TO /mbtools/cl_registry.

* Prevent any changes if this entry is marked as deleted
    IF mv_deleted = abap_true.
      /mbtools/cx_exception=>raise( 'Registry entry is marked as deleted'(005) ).
    ENDIF.

    lo_source_entry = get_subentry( iv_source_key ).
    IF lo_source_entry IS NOT BOUND.
      /mbtools/cx_exception=>raise( 'Registry entry does not exist'(006) ).
    ENDIF.
    ro_target_entry = add_subentry( iv_target_key ).

* Using the source and the new target, do a deep copy that includes
* copies of sub-entries and values
    copy_subentry_deep( io_source = lo_source_entry
                        io_target = ro_target_entry ).

  ENDMETHOD.                    "copy_subentry


  METHOD copy_subentry_deep.
*--------------------------------------------------------------------*
* COPY_SUBENTRY_DEEP - (protected) - copy a branch of the registry
*         at the same level, including all values
*--------------------------------------------------------------------*
    DATA: ls_subentry TYPE ty_keyval.
    DATA: lo_source TYPE REF TO /mbtools/cl_registry.
    DATA: lo_target TYPE REF TO /mbtools/cl_registry.

* Copy values from source to target
    io_target->mt_values = io_source->mt_values.

* Copy sub-entries from source to target
    LOOP AT io_source->mt_sub_entries INTO ls_subentry.
      lo_source = io_source->get_subentry( ls_subentry-key ).
      lo_target = io_target->add_subentry( ls_subentry-key ).
      copy_subentry_deep( io_source = lo_source
                          io_target = lo_target ).
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
    DATA: lt_keys TYPE string_table.
    DATA: lv_key TYPE string.
    DATA: lo_sub_entry TYPE REF TO /mbtools/cl_registry.

    SPLIT iv_path AT '/' INTO TABLE lt_keys.
    DELETE lt_keys WHERE table_line IS INITIAL.

    ro_entry = me.
    LOOP AT lt_keys INTO lv_key.
      lo_sub_entry = ro_entry->get_subentry( lv_key ).
      IF lo_sub_entry IS NOT BOUND.
        lo_sub_entry = ro_entry->add_subentry( lv_key ).
      ENDIF.
      ro_entry = lo_sub_entry.
    ENDLOOP.
* After successful processing of chain, ENTRY will
* contain the last-created node

  ENDMETHOD.                    "create_by_path


  METHOD delete.
*--------------------------------------------------------------------*
* DELETE - delete the current entry from the database and mark it,
*          preventing any further operations on this entry
*--------------------------------------------------------------------*

    DATA: ls_sub_entry TYPE ty_keyval.
    DATA: lo_entry TYPE REF TO /mbtools/cl_registry.

* Prevent any changes if this entry is marked as deleted
    IF mv_deleted = abap_true.
      /mbtools/cx_exception=>raise( 'Registry entry is marked as deleted'(005) ).
    ENDIF.

* Delete all sub-entries before deleting this entry
    LOOP AT mt_sub_entries INTO ls_sub_entry.
      lo_entry = get_subentry( ls_sub_entry-key ).
      lo_entry->delete( ).
      DELETE mt_sub_entries.
    ENDLOOP.

* Remove DB entry for the current entry
    promote_lock( ).
    DELETE FROM DATABASE /mbtools/regs(zr) ID mv_internal_key.
* Object removes itself from the global table too so that that reference no longer exists
    DELETE gt_registry_entries WHERE key = mv_internal_key.
* Set the object to deleted to prevent any operations on any remaining
* references to the object
    mv_deleted = abap_true.

* Release lock held on this key
    release_lock( ).

  ENDMETHOD.                    "delete


  METHOD delete_value.
*--------------------------------------------------------------------*
* DELETE_VALUE - remove single value by key
*--------------------------------------------------------------------*
    DATA: ls_kv TYPE ty_keyval.
* Prevent any changes if this entry is marked as deleted
    IF mv_deleted = abap_true.
      /mbtools/cx_exception=>raise( 'Registry entry is marked as deleted'(005) ).
    ENDIF.

    ls_kv-key = iv_key.
    DELETE mt_values WHERE key = ls_kv-key.
  ENDMETHOD.                    "delete_value


  METHOD export.
*>>>INS
    DATA:
      lo_reg_entry TYPE REF TO /mbtools/cl_registry,
      ls_kv        TYPE ty_keyval,
      lv_id        TYPE string,
      lv_file_line TYPE string.

*   Export key header
    lo_reg_entry = me.
    DO.
      lv_id = lo_reg_entry->mv_entry_id.
      IF lv_file_line IS INITIAL.
        lv_file_line = lv_id.
      ELSE.
        CONCATENATE lv_id lv_file_line INTO lv_file_line SEPARATED BY '/'.
      ENDIF.
      IF lv_id = c_registry_root.
        EXIT.
      ELSE.
        lo_reg_entry = lo_reg_entry->get_parent( ).
      ENDIF.
    ENDDO.
    CONCATENATE '[' lv_file_line ']' INTO lv_file_line.
    APPEND lv_file_line TO ct_file.

*   Export key values
    LOOP AT mt_values INTO ls_kv.
      CONCATENATE '"' ls_kv-key '"="' ls_kv-value '"' INTO lv_file_line.
      APPEND lv_file_line TO ct_file.
    ENDLOOP.
    APPEND '' TO ct_file.

*   Export sub entries (recursive)
    LOOP AT mt_sub_entries INTO ls_kv.
      lo_reg_entry = get_subentry( ls_kv-key ).

      lo_reg_entry->export( CHANGING ct_file = ct_file ).
    ENDLOOP.
*<<<INS
  ENDMETHOD.


  METHOD get_entry_by_internal_key.
*--------------------------------------------------------------------*
* GET_ENTRY_BY_INTERNAL_KEY - retrieve reg. entry by internal ID
*--------------------------------------------------------------------*
    DATA: ls_ko TYPE ty_keyobj.

* Search global index of registry entry instances
    READ TABLE gt_registry_entries INTO ls_ko WITH KEY key = iv_key.

    IF sy-subrc = 0.
* Reference already exists; return that
      ro_entry = ls_ko-value.
    ELSE.
* Create new reference to sub-entry
      CREATE OBJECT ro_entry
        EXPORTING
          ig_internal_key = iv_key.
* Will insert itself into registry entries
    ENDIF.
  ENDMETHOD.                    "get_entry_by_internal_key


  METHOD get_parent.
*--------------------------------------------------------------------*
* GET_PARENT - retrieve parent entry of this entry
*--------------------------------------------------------------------*
    ro_parent = get_entry_by_internal_key( mv_parent_key ).
  ENDMETHOD.                    "get_parent


  METHOD get_root.
*--------------------------------------------------------------------*
* GET_ROOT - retrieve root entry of registry
*--------------------------------------------------------------------*

* If the root doesn't exist yet, create it
    DATA: lt_values TYPE ty_keyvals.
    DATA: lt_sub_entries TYPE ty_keyvals.
    DATA: lv_parent_key TYPE indx_srtfd VALUE space.
    DATA: lv_entry_id TYPE string.
    DATA: ls_regs TYPE /mbtools/if_definitions=>ty_regs.

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

    IMPORT values = lt_values sub_entries = lt_sub_entries
      FROM DATABASE /mbtools/regs(zr) ID c_registry_root.
    IF sy-subrc NE 0.
*>>>INS
      ls_regs-chdate = sy-datum.
      ls_regs-chtime = sy-uzeit.
      ls_regs-chname = sy-uname.
*<<<INS
      lv_entry_id = c_registry_root.

      EXPORT values = lt_values sub_entries = lt_sub_entries
        parent = lv_parent_key entry_id = lv_entry_id
        TO DATABASE /mbtools/regs(zr) FROM ls_regs ID c_registry_root.
    ENDIF.

* Retrieve the root entry of the registry
    ro_root = get_entry_by_internal_key( c_registry_root ).

  ENDMETHOD.                    "get_root


  METHOD get_subentries.
*--------------------------------------------------------------------*
* GET_SUBENTRIES - return immediate children registry entries
*--------------------------------------------------------------------*
    DATA: ls_ko TYPE ty_keyobj.
    DATA: lt_subkeys TYPE string_table.
    DATA: lv_subkey TYPE string.

    lt_subkeys = get_subentry_keys( ).

    SORT lt_subkeys. "<<<INS

    LOOP AT lt_subkeys INTO lv_subkey.
      ls_ko-key = lv_subkey.
      ls_ko-value = get_subentry( lv_subkey ).
      INSERT ls_ko INTO TABLE rt_sub_entries. "sorted table
    ENDLOOP.

  ENDMETHOD.                    "get_subentries


  METHOD get_subentry.
*--------------------------------------------------------------------*
* GET_SUBENTRY - return single child entry by key
*--------------------------------------------------------------------*
    DATA: ls_kv TYPE ty_keyval.
    DATA: ls_ko TYPE ty_keyobj.

* Read internal store of sub-entries
    READ TABLE mt_sub_entries INTO ls_kv WITH KEY key = iv_key.
    IF sy-subrc <> 0.
* Entry does not exist; exit
      RETURN.
    ENDIF.

* Search global index of registry entry instances
    READ TABLE gt_registry_entries INTO ls_ko WITH KEY key =  ls_kv-value.
    IF sy-subrc = 0.
* Reference already exists; return that
      ro_entry = ls_ko-value.
    ELSE.
* Create new reference to sub-entry
      CREATE OBJECT ro_entry
        EXPORTING
          ig_internal_key = ls_kv-value.
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
    DATA: lt_keys TYPE string_table.
    DATA: lv_key TYPE string.
    DATA: lo_sub_entry TYPE REF TO /mbtools/cl_registry.

    SPLIT iv_path AT '/' INTO TABLE lt_keys.
    DELETE lt_keys WHERE table_line IS INITIAL.

    ro_entry = me.
    LOOP AT lt_keys INTO lv_key.
      lo_sub_entry = ro_entry->get_subentry( lv_key ).
      IF lo_sub_entry IS NOT BOUND.
        /mbtools/cx_exception=>raise( 'Path error'(008) ).
      ENDIF.
      ro_entry = lo_sub_entry.
    ENDLOOP.
* After successful processing of chain, ENTRY will
* contain the last node
*<<<INS
  ENDMETHOD. "get_subentry_by_path.


  METHOD get_subentry_keys.
*--------------------------------------------------------------------*
* GET_SUBENTRY_KEYS - retrieve keys of all child registry entries
*--------------------------------------------------------------------*
    DATA: ls_kv TYPE ty_keyval.
    LOOP AT mt_sub_entries INTO ls_kv.
      APPEND ls_kv-key TO rt_keys.
    ENDLOOP.
  ENDMETHOD.                    "get_subentry_keys


  METHOD get_value.
*--------------------------------------------------------------------*
* GET_VALUE - return a single value by key
*--------------------------------------------------------------------*
    DATA: ls_kv TYPE ty_keyval.
    READ TABLE mt_values INTO ls_kv WITH KEY key = iv_key.
    IF sy-subrc = 0.
      rv_value = ls_kv-value.
    ENDIF.
  ENDMETHOD.                    "get_value


  METHOD get_values.
*--------------------------------------------------------------------*
* GET_VALUES - retrieve all values at once in key+value table
*--------------------------------------------------------------------*
    rt_values = mt_values.
  ENDMETHOD.                    "get_values


  METHOD get_value_keys.
*--------------------------------------------------------------------*
* GET_VALUE_KEYS - retrieve keys of all values
*--------------------------------------------------------------------*
    DATA: ls_kv TYPE ty_keyval.
    LOOP AT mt_values INTO ls_kv.
      APPEND ls_kv-key TO rt_keys.
    ENDLOOP.
  ENDMETHOD.                    "get_value_keys


  METHOD promote_lock.
*--------------------------------------------------------------------*
* PROMOTE_LOCK - Get exclusive lock just before saving
*--------------------------------------------------------------------*
    CALL FUNCTION 'ENQUEUE_/MBTOOLS/E_REGS'
      EXPORTING
        mode_/mbtools/regs = 'R'
        relid              = c_relid
        srtfd              = mv_internal_key
      EXCEPTIONS
        foreign_lock       = 1
        system_failure     = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      /mbtools/cx_exception=>raise( 'Registry entry is locked'(009) ).
    ENDIF.
  ENDMETHOD.                    "promote_lock


  METHOD release_lock.
*--------------------------------------------------------------------*
* RELEASE_LOCK - called after deleting or before re-acquiring
*--------------------------------------------------------------------*
    CALL FUNCTION 'DEQUEUE_/MBTOOLS/E_REGS'
      EXPORTING
        relid = c_relid
        srtfd = mv_internal_key.
  ENDMETHOD.                    "release_lock


  METHOD reload.
*--------------------------------------------------------------------*
* RELOAD - reload values and sub-entries from database, set new lock
*--------------------------------------------------------------------*
    IMPORT values = mt_values sub_entries = mt_sub_entries parent = mv_parent_key entry_id = mv_entry_id
      FROM DATABASE /mbtools/regs(zr) ID mv_internal_key.
    IF sy-subrc NE 0.
      /mbtools/cx_exception=>raise( 'Registry entry does not exist'(006) ).
    ENDIF.
*>>>INS
    SELECT SINGLE * FROM /mbtools/regs INTO ms_regs
      WHERE relid = 'ZR' AND srtfd = mv_internal_key AND srtf2 = 0.
*<<<INS
    set_optimistic_lock( ).
  ENDMETHOD.                    "reload


  METHOD remove_subentries.
*--------------------------------------------------------------------*
* REMOVE_SUBENTRIES - remove all child entries of this entry
*--------------------------------------------------------------------*
    DATA: ls_kv TYPE ty_keyval.
*    DATA: ko TYPE ty_keyobj. "Shadow table with object references

* Prevent any changes if this entry is marked as deleted
    IF mv_deleted = abap_true.
      /mbtools/cx_exception=>raise( 'Registry entry is marked as deleted'(005) ).
    ENDIF.

    LOOP AT mt_sub_entries INTO ls_kv.
      remove_subentry( ls_kv-key ).
    ENDLOOP.
  ENDMETHOD.                    "remove_subentries


  METHOD remove_subentry.
*--------------------------------------------------------------------*
* REMOVE_SUBENTRY - remove a single child registry entry by key
*--------------------------------------------------------------------*
    DATA: ls_kv TYPE ty_keyval.
    DATA: lo_sub_entry TYPE REF TO /mbtools/cl_registry.

* Prevent any changes if this entry is marked as deleted
    IF mv_deleted = abap_true.
      /mbtools/cx_exception=>raise( 'Registry entry is marked as deleted'(005) ).
    ENDIF.

* Read internal store of sub-entries
    READ TABLE mt_sub_entries INTO ls_kv WITH KEY key = iv_key.
    IF sy-subrc NE 0.
* Entry does not exist; exit with error
      /mbtools/cx_exception=>raise( 'Registry entry does not exist'(006) ).
    ENDIF.

* Remove all sub-entries of the sub-entry before removing the sub-entry
    lo_sub_entry = get_subentry( iv_key ).
    IF lo_sub_entry IS BOUND.

* Delete the sub_entry (which deletes its sub-entries)
      lo_sub_entry->delete( ).
* Remove entry from sub-entry table and shadow table
      ls_kv-key = iv_key.
      DELETE mt_sub_entries WHERE key = ls_kv-key.

      save( ). "Save current entry to remove subentry that has been removed
    ENDIF.

  ENDMETHOD.                    "remove_subentry


  METHOD save.
*--------------------------------------------------------------------*
* SAVE - save the current entry, with concurrency control
*--------------------------------------------------------------------*
    DATA: ls_regs TYPE /mbtools/if_definitions=>ty_regs.
* Prevent any changes if this entry is marked as deleted
    IF mv_deleted = abap_true.
      /mbtools/cx_exception=>raise( 'Registry entry is marked as deleted'(005) ).
    ENDIF.
*>>>INS
    ls_regs-chdate = sy-datum.
    ls_regs-chtime = sy-uzeit.
    ls_regs-chname = sy-uname.
*<<<INS
    promote_lock( ).
    EXPORT values = mt_values sub_entries = mt_sub_entries
      parent = mv_parent_key entry_id = mv_entry_id
      TO DATABASE /mbtools/regs(zr) FROM ls_regs ID mv_internal_key.
    set_optimistic_lock( ).
  ENDMETHOD.                    "save


  METHOD set_optimistic_lock.
*--------------------------------------------------------------------*
* SET_OPTIMISTIC_LOCK - always set when (re-)reading an entry
*--------------------------------------------------------------------*
* Existing lock must be released before acquiring a new one
    release_lock( ).
    CALL FUNCTION 'ENQUEUE_/MBTOOLS/E_REGS'
      EXPORTING
        mode_/mbtools/regs = 'O'
        relid              = 'ZR'
        srtfd              = mv_internal_key
      EXCEPTIONS
        foreign_lock       = 1
        system_failure     = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      /mbtools/cx_exception=>raise( 'Registry entry is locked'(009) ).
    ENDIF.
  ENDMETHOD.                    "set_optimistic_lock


  METHOD set_value.
*--------------------------------------------------------------------*
* SET_VALUE - store a single value by key
*--------------------------------------------------------------------*
    DATA: ls_kv TYPE ty_keyval.

* Prevent any changes if this entry is marked as deleted
    IF mv_deleted = abap_true.
      /mbtools/cx_exception=>raise( 'Registry entry is marked as deleted'(005) ).
    ENDIF.

* Add the value to set of values if not existing or change if it does exist
    READ TABLE mt_values INTO ls_kv WITH KEY key = iv_key.
    IF sy-subrc NE 0.
      ls_kv-key   = iv_key.
      ls_kv-value = iv_value.
      INSERT ls_kv INTO TABLE mt_values.
    ELSE.
      ls_kv-value = iv_value.
      MODIFY TABLE mt_values FROM ls_kv.
    ENDIF.
  ENDMETHOD.                    "set_value


  METHOD set_values.
*--------------------------------------------------------------------*
* SET_VALUES - set all values at once with key+value table
*--------------------------------------------------------------------*
* Prevent any changes if this entry is marked as deleted
    IF mv_deleted = abap_true.
      /mbtools/cx_exception=>raise( 'Registry entry is marked as deleted'(005) ).
    ENDIF.

    mt_values = it_values.
  ENDMETHOD.                    "set_values


  METHOD truncate.

    DATA lv_answer TYPE c.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = c_name
        text_question         = 'Are you sure you want to delete the complete registry?'(001)
        text_button_1         = 'Yes'(002)
        text_button_2         = 'No'(003)
        default_button        = '2'
        display_cancel_button = 'X'
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0 OR lv_answer <> 'J'.
      RETURN.
    ENDIF.

    DELETE FROM /mbtools/regs WHERE relid = 'ZR'.
    IF sy-subrc = 0.
      MESSAGE 'Registry truncated successfully'(004) TYPE 'S'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
