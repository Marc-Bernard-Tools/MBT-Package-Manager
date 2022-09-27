CLASS /mbtools/cl_hotkeys DEFINITION
  PUBLIC
  INHERITING FROM /mbtools/cl_gui_component
  FINAL
  CREATE PUBLIC .

************************************************************************
* Marc Bernard Tools - Hotkeys
*
* Copyright 2014 abapGit Contributors <http://abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES /mbtools/if_gui_hotkey_ctl .
    INTERFACES /mbtools/if_gui_hotkeys .
    INTERFACES /mbtools/if_gui_renderable .

    TYPES:
      BEGIN OF ty_hotkey,
        ui_component TYPE string,
        action       TYPE string,
        hotkey       TYPE string,
      END OF ty_hotkey .
    TYPES:
      ty_hotkeys TYPE STANDARD TABLE OF ty_hotkey
                      WITH NON-UNIQUE DEFAULT KEY
                      WITH NON-UNIQUE SORTED KEY action
                           COMPONENTS ui_component action.

    CONSTANTS c_showhotkeys_action TYPE string VALUE `showHotkeys` ##NO_TEXT.

    CLASS-METHODS get_all_default_hotkeys
      RETURNING
        VALUE(rt_hotkey_actions) TYPE /mbtools/if_gui_hotkeys=>ty_hotkeys_with_descr
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS merge_hotkeys_with_settings
      CHANGING
        !ct_hotkey_actions TYPE /mbtools/if_gui_hotkeys=>ty_hotkeys_with_descr
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS should_show_hint
      RETURNING
        VALUE(rv_yes) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      mt_hotkey_providers TYPE TABLE OF REF TO /mbtools/if_gui_hotkeys .
    CLASS-DATA gv_hint_was_shown TYPE abap_bool .

    CLASS-METHODS filter_relevant_classes
      IMPORTING
        !it_classes       TYPE seo_relkeys
      RETURNING
        VALUE(rt_classes) TYPE seo_relkeys .
    CLASS-METHODS get_class_package
      IMPORTING
        !iv_class_name    TYPE seoclsname
      RETURNING
        VALUE(rv_package) TYPE devclass .
    CLASS-METHODS get_referred_class_name
      IMPORTING
        !io_ref        TYPE any
      RETURNING
        VALUE(rv_name) TYPE seoclsname .
    CLASS-METHODS get_hotkeys_by_class_name
      IMPORTING
        !iv_class_name    TYPE seoclsname
      RETURNING
        VALUE(rt_hotkeys) TYPE /mbtools/if_gui_hotkeys=>ty_hotkeys_with_descr.
    CLASS-METHODS get_hotkeys_from_global_intf
      RETURNING
        VALUE(rt_hotkeys) TYPE /mbtools/if_gui_hotkeys=>ty_hotkeys_with_descr
      RAISING
        /mbtools/cx_exception .
    METHODS render_scripts
      IMPORTING
        !it_hotkeys    TYPE /mbtools/if_gui_hotkeys=>ty_hotkeys_with_descr
      RETURNING
        VALUE(ri_html) TYPE REF TO /mbtools/if_html .
ENDCLASS.



CLASS /mbtools/cl_hotkeys IMPLEMENTATION.


  METHOD /mbtools/if_gui_hotkeys~get_hotkey_actions.

    DATA ls_hotkey LIKE LINE OF rt_hotkey_actions.

    ls_hotkey-ui_component = 'Hotkeys'.
    ls_hotkey-action       = c_showhotkeys_action.
    ls_hotkey-description  = 'Show hotkeys help'.
    ls_hotkey-hotkey       = '?'.
    INSERT ls_hotkey INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD /mbtools/if_gui_hotkey_ctl~get_registered_hotkeys.

    DATA li_hotkey_provider LIKE LINE OF mt_hotkey_providers.
    DATA lt_hotkeys         LIKE rt_registered_hotkeys.
    FIELD-SYMBOLS <ls_hotkey> LIKE LINE OF lt_hotkeys.

    LOOP AT mt_hotkey_providers INTO li_hotkey_provider.
      APPEND LINES OF li_hotkey_provider->get_hotkey_actions( ) TO lt_hotkeys.
    ENDLOOP.

    merge_hotkeys_with_settings( CHANGING ct_hotkey_actions = lt_hotkeys ).

    " Compress duplicates
    LOOP AT lt_hotkeys ASSIGNING <ls_hotkey>.
      READ TABLE rt_registered_hotkeys WITH KEY hotkey = <ls_hotkey>-hotkey TRANSPORTING NO FIELDS.
      IF sy-subrc = 0. " If found command with same hotkey
        DELETE rt_registered_hotkeys INDEX sy-tabix. " Later registered commands enjoys the priority
      ENDIF.
      APPEND <ls_hotkey> TO rt_registered_hotkeys.
    ENDLOOP.

  ENDMETHOD.


  METHOD /mbtools/if_gui_hotkey_ctl~register_hotkeys.
    IF ii_hotkeys IS BOUND.
      APPEND ii_hotkeys TO mt_hotkey_providers.
    ENDIF.
  ENDMETHOD.


  METHOD /mbtools/if_gui_hotkey_ctl~reset.
    CLEAR mt_hotkey_providers.
  ENDMETHOD.


  METHOD /mbtools/if_gui_renderable~render.

    DATA:
      lv_hint               TYPE string,
      lt_registered_hotkeys TYPE /mbtools/if_gui_hotkeys=>ty_hotkeys_with_descr,
      lv_hotkey             TYPE string.

    FIELD-SYMBOLS <ls_hotkey> LIKE LINE OF lt_registered_hotkeys.

    /mbtools/if_gui_hotkey_ctl~register_hotkeys( me ).

    ri_html = /mbtools/cl_html=>create( ).

    lt_registered_hotkeys = /mbtools/if_gui_hotkey_ctl~get_registered_hotkeys( ).
    SORT lt_registered_hotkeys BY hotkey.

    register_deferred_script( render_scripts( lt_registered_hotkeys ) ).

    " Render hotkeys
    ri_html->add( '<ul class="hotkeys">' ).
    LOOP AT lt_registered_hotkeys ASSIGNING <ls_hotkey>.
      ri_html->add( |<li>|
        && |<span class="key-id">{ <ls_hotkey>-hotkey }</span>|
        && |<span class="key-descr">{ <ls_hotkey>-description }</span>|
        && |</li>| ).
    ENDLOOP.
    ri_html->add( '</ul>' ).

    CLEAR lv_hotkey.

    READ TABLE lt_registered_hotkeys ASSIGNING <ls_hotkey>
      WITH KEY action = c_showhotkeys_action.
    IF sy-subrc = 0.
      lv_hotkey = <ls_hotkey>-hotkey.
    ENDIF.

    lv_hint = |Close window with upper right corner 'X'|.
    IF lv_hotkey IS NOT INITIAL.
      lv_hint = lv_hint && | or '{ <ls_hotkey>-hotkey }'|.
    ENDIF.

    ri_html = /mbtools/cl_html_lib=>render_infopanel(
      iv_div_id  = 'hotkeys'
      iv_title   = 'Hotkeys'
      iv_hint    = lv_hint
      ii_content = ri_html ).

    IF lv_hotkey IS NOT INITIAL AND should_show_hint( ) = abap_true.
      ri_html->add( |<div id="hotkeys-hint" class="corner-hint">|
        && |Press '{ <ls_hotkey>-hotkey }' to get keyboard shortcuts list|
        && |</div>| ).
    ENDIF.

  ENDMETHOD.


  METHOD filter_relevant_classes.

    DATA lv_this_class_name TYPE seoclsname.
    DATA lv_this_class_pkg TYPE devclass.
    DATA lv_class_pkg TYPE devclass.
    DATA lo_dummy TYPE REF TO /mbtools/cl_hotkeys.

    FIELD-SYMBOLS <ls_class> LIKE LINE OF it_classes.

    lv_this_class_name = get_referred_class_name( lo_dummy ).
    lv_this_class_pkg = get_class_package( lv_this_class_name ).

    LOOP AT it_classes ASSIGNING <ls_class>.
      lv_class_pkg = get_class_package( <ls_class>-clsname ).
      IF lv_class_pkg = lv_this_class_pkg.
        APPEND <ls_class> TO rt_classes.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_all_default_hotkeys.

    rt_hotkey_actions = get_hotkeys_from_global_intf( ).

  ENDMETHOD.


  METHOD get_class_package.

    SELECT SINGLE devclass FROM tadir
      INTO rv_package
      WHERE pgmid = 'R3TR'
      AND object = 'CLAS'
      AND obj_name = iv_class_name.

  ENDMETHOD.


  METHOD get_hotkeys_by_class_name.

    CALL METHOD (iv_class_name)=>/mbtools/if_gui_hotkeys~get_hotkey_actions
      RECEIVING
        rt_hotkey_actions = rt_hotkeys.

  ENDMETHOD.


  METHOD get_hotkeys_from_global_intf.

    DATA: lt_hotkey_actions LIKE rt_hotkeys,
          lo_interface      TYPE REF TO cl_oo_interface,
          li_dummy          TYPE REF TO /mbtools/if_gui_hotkeys,
          lt_classes        TYPE seo_relkeys.

    FIELD-SYMBOLS: <ls_class> LIKE LINE OF lt_classes.

    TRY.
        lo_interface ?= cl_oo_class=>get_instance( get_referred_class_name( li_dummy ) ).
      CATCH cx_class_not_existent.
        RETURN.
    ENDTRY.

    lt_classes = lo_interface->get_implementing_classes( ).
    lt_classes = filter_relevant_classes( lt_classes ). " For security reasons

    LOOP AT lt_classes ASSIGNING <ls_class>.
      lt_hotkey_actions = get_hotkeys_by_class_name( <ls_class>-clsname ).
      INSERT LINES OF lt_hotkey_actions INTO TABLE rt_hotkeys.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_referred_class_name.

    DATA lo_ref TYPE REF TO cl_abap_refdescr.
    lo_ref ?= cl_abap_typedescr=>describe_by_data( io_ref ).
    rv_name = lo_ref->get_referenced_type( )->get_relative_name( ).

  ENDMETHOD.


  METHOD merge_hotkeys_with_settings.

    DATA lt_user_defined_hotkeys TYPE ty_hotkeys.
    FIELD-SYMBOLS <ls_hotkey> LIKE LINE OF ct_hotkey_actions.
    FIELD-SYMBOLS <ls_user_defined_hotkey> LIKE LINE OF lt_user_defined_hotkeys.

    LOOP AT ct_hotkey_actions ASSIGNING <ls_hotkey>.
      READ TABLE lt_user_defined_hotkeys ASSIGNING <ls_user_defined_hotkey>
        WITH TABLE KEY action COMPONENTS
          ui_component = <ls_hotkey>-ui_component
          action       = <ls_hotkey>-action.
      IF sy-subrc = 0.
        <ls_hotkey>-hotkey = <ls_user_defined_hotkey>-hotkey.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD render_scripts.

    DATA lv_json TYPE string.
    DATA lt_hotkeys TYPE /mbtools/if_gui_hotkeys=>ty_hotkeys_with_descr.

    FIELD-SYMBOLS: <ls_hotkey> LIKE LINE OF lt_hotkeys.

    lv_json = `{`.

    LOOP AT it_hotkeys ASSIGNING <ls_hotkey>.

      IF sy-tabix > 1.
        lv_json = lv_json && |,|.
      ENDIF.

      lv_json = lv_json && |  "{ <ls_hotkey>-hotkey }" : "{ <ls_hotkey>-action }" |.

    ENDLOOP.

    lv_json = lv_json && `}`.

    ri_html = /mbtools/cl_html=>create( ).

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( |setKeyBindings({ lv_json });| ).

  ENDMETHOD.


  METHOD should_show_hint.
    IF gv_hint_was_shown = abap_false.
      rv_yes = abap_true.
      gv_hint_was_shown = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
