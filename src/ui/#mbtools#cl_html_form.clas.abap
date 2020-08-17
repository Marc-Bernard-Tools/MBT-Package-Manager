CLASS /mbtools/cl_html_form DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .
************************************************************************
* MBT HTML Form
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_cmd_type,
        input  TYPE i VALUE 1,
        link   TYPE i VALUE 2,
        button TYPE i VALUE 3,
      END OF c_cmd_type.

    CLASS-METHODS create
      IMPORTING
        !iv_form_id     TYPE string OPTIONAL
        !iv_form_action TYPE string OPTIONAL
      RETURNING
        VALUE(ro_form)  TYPE REF TO /mbtools/cl_html_form .
    METHODS render
      IMPORTING
        !iv_form_class     TYPE string
        !io_values         TYPE REF TO /mbtools/cl_string_map
        !io_validation_log TYPE REF TO /mbtools/cl_string_map OPTIONAL
      RETURNING
        VALUE(ri_html)     TYPE REF TO /mbtools/if_html .
    METHODS command
      IMPORTING
        !iv_label    TYPE string
        !iv_action   TYPE string
        !iv_is_main  TYPE abap_bool DEFAULT abap_false
        !iv_cmd_type TYPE i DEFAULT 1.
    METHODS text
      IMPORTING
        !iv_label       TYPE string
        !iv_name        TYPE string
        !iv_size        TYPE i OPTIONAL
        !iv_hint        TYPE string OPTIONAL
        !iv_required    TYPE abap_bool DEFAULT abap_false
        !iv_placeholder TYPE string OPTIONAL
        !iv_side_action TYPE string OPTIONAL .
    METHODS checkbox
      IMPORTING
        !iv_label TYPE string
        !iv_name  TYPE string
        !iv_hint  TYPE string OPTIONAL .
    METHODS radio
      IMPORTING
        !iv_label         TYPE string
        !iv_name          TYPE string
        !iv_default_value TYPE string OPTIONAL
        !iv_hint          TYPE string OPTIONAL .
    METHODS option
      IMPORTING
        !iv_label TYPE string
        !iv_value TYPE string .
    METHODS start_group
      IMPORTING
        !iv_label TYPE string
        !iv_name  TYPE string
        !iv_hint  TYPE string OPTIONAL .
    METHODS init.
    METHODS hidden
      IMPORTING
        !iv_name TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_subitem,
        label TYPE string,
        value TYPE string,
      END OF ty_subitem.
    TYPES:
      ty_subitems TYPE STANDARD TABLE OF ty_subitem WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_field,
        type          TYPE i,
        name          TYPE string,
        label         TYPE string,
        hint          TYPE string,
        dblclick      TYPE string,
        placeholder   TYPE string,
        required      TYPE string,
        item_class    TYPE string,
        error         TYPE string,
        default_value TYPE string,
        side_action   TYPE string,
        size          TYPE i,
        subitems      TYPE ty_subitems,
*        onclick ???
      END OF ty_field.

    TYPES:
      BEGIN OF ty_command,
        label    TYPE string,
        action   TYPE string,
        is_main  TYPE abap_bool,
        cmd_type TYPE i,
*        onclick ???
      END OF ty_command.

    CONSTANTS:
      BEGIN OF c_field_type,
        text        TYPE i VALUE 1,
        radio       TYPE i VALUE 2,
        checkbox    TYPE i VALUE 3,
        field_group TYPE i VALUE 4,
        hidden      TYPE i VALUE 5,
      END OF c_field_type.

    DATA mt_fields TYPE STANDARD TABLE OF ty_field.
    DATA mt_commands TYPE STANDARD TABLE OF ty_command.
    DATA mv_form_id TYPE string.
    DATA mv_form_action TYPE string.

    CLASS-METHODS render_field
      IMPORTING
        ii_html           TYPE REF TO /mbtools/if_html
        io_values         TYPE REF TO /mbtools/cl_string_map
        io_validation_log TYPE REF TO /mbtools/cl_string_map
        is_field          TYPE ty_field.

    CLASS-METHODS render_command
      IMPORTING
        ii_html TYPE REF TO /mbtools/if_html
        is_cmd  TYPE ty_command.

ENDCLASS.



CLASS /MBTOOLS/CL_HTML_FORM IMPLEMENTATION.


  METHOD checkbox.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = c_field_type-checkbox.
    ls_field-name  = iv_name.
    ls_field-label = iv_label.

    IF iv_hint IS NOT INITIAL.
      ls_field-hint    = | title="{ iv_hint }"|.
    ENDIF.

    APPEND ls_field TO mt_fields.

  ENDMETHOD.


  METHOD command.

    DATA ls_cmd LIKE LINE OF mt_commands.

    ASSERT iv_cmd_type IS INITIAL OR iv_is_main IS INITIAL.

    ls_cmd-label = iv_label.
    ls_cmd-action = iv_action.
    ls_cmd-is_main = iv_is_main.
    ls_cmd-cmd_type = iv_cmd_type.

    APPEND ls_cmd TO mt_commands.

  ENDMETHOD.


  METHOD create.

    DATA lv_ts TYPE timestampl.

    CREATE OBJECT ro_form.
    ro_form->mv_form_id = iv_form_id.
    ro_form->mv_form_action = iv_form_action.

    IF ro_form->mv_form_id IS INITIAL.
      GET TIME STAMP FIELD lv_ts.
      ro_form->mv_form_id = |form_{ lv_ts }|.
    ENDIF.

  ENDMETHOD.


  METHOD hidden.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = c_field_type-hidden.
    ls_field-name  = iv_name.
    APPEND ls_field TO mt_fields.

  ENDMETHOD.


  METHOD init.
    CLEAR mt_fields.
  ENDMETHOD.


  METHOD option.

    FIELD-SYMBOLS <ls_last> LIKE LINE OF mt_fields.
    DATA ls_option LIKE LINE OF <ls_last>-subitems.
    DATA lv_size TYPE i.

    lv_size = lines( mt_fields ).
    ASSERT lv_size > 0. " Exception ? Maybe add zcx_no_check ?

    READ TABLE mt_fields INDEX lv_size ASSIGNING <ls_last>.
    ASSERT sy-subrc = 0.
    ASSERT <ls_last>-type = c_field_type-radio. " Or dropdown - TODO in future

    ls_option-label = iv_label.
    ls_option-value = iv_value.

    APPEND ls_option TO <ls_last>-subitems.

  ENDMETHOD.


  METHOD radio.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = c_field_type-radio.
    ls_field-name  = iv_name.
    ls_field-label = iv_label.
    ls_field-default_value = iv_default_value.

    IF iv_hint IS NOT INITIAL.
      ls_field-hint    = | title="{ iv_hint }"|.
    ENDIF.

    APPEND ls_field TO mt_fields.

  ENDMETHOD.


  METHOD render.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF mt_fields.
    FIELD-SYMBOLS <ls_cmd> LIKE LINE OF mt_commands.
    DATA lv_form_att TYPE string.
    DATA lv_cur_group TYPE string.

    IF mv_form_id IS NOT INITIAL.
      lv_form_att = | id="{ mv_form_id }"|.
    ENDIF.
    IF mv_form_action IS NOT INITIAL.
      lv_form_att = | action="sapevent:{ mv_form_action }"|.
    ENDIF.

    ri_html = /mbtools/cl_html=>create( ).

    ri_html->add( |<div class="{ iv_form_class }">| ).
    ri_html->add( |<form method="post"{ lv_form_att }>| ).
    ri_html->add( |<ul>| ).

    LOOP AT mt_fields ASSIGNING <ls_field>.

      IF <ls_field>-type = c_field_type-field_group.
        IF lv_cur_group IS NOT INITIAL AND lv_cur_group <> <ls_field>-name.
          ri_html->add( '</fieldset>' ).
        ENDIF.
        lv_cur_group = <ls_field>-name.
        ri_html->add( |<fieldset name="{ <ls_field>-name }">| ).
        ri_html->add( |<legend{ <ls_field>-hint }>{ <ls_field>-label }</legend>| ).
        CONTINUE.
      ENDIF.

      render_field(
        ii_html           = ri_html
        io_values         = io_values
        io_validation_log = io_validation_log
        is_field          = <ls_field> ).

      AT LAST.
        IF lv_cur_group IS NOT INITIAL.
          ri_html->add( '</fieldset>' ).
        ENDIF.
      ENDAT.
    ENDLOOP.

    ri_html->add( |<li class="dialog-commands">| ).

    LOOP AT mt_commands ASSIGNING <ls_cmd>.
      render_command(
        ii_html = ri_html
        is_cmd  = <ls_cmd> ).
    ENDLOOP.

    ri_html->add( |</li>| ).

    ri_html->add( |</ul>| ).
    ri_html->add( |</form>| ).
    ri_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_command.

    DATA lv_main_submit TYPE string.

    CASE is_cmd-cmd_type.
      WHEN c_cmd_type-link.

        ii_html->add_a(
          iv_txt   = is_cmd-label
          iv_act   = is_cmd-action
          iv_class = 'dialog-commands' ).

      WHEN c_cmd_type-button.

        ii_html->add( |<button type="submit" name="action" value="{
          is_cmd-action }" class="action-commands">{ is_cmd-label }</button>| ).

      WHEN c_cmd_type-input.

        IF is_cmd-is_main = abap_true.
          lv_main_submit = ' class="main"'.
        ELSE.
          CLEAR lv_main_submit.
        ENDIF.
        ii_html->add( |<input type="submit" value="{
          is_cmd-label }"{ lv_main_submit } formaction="sapevent:{ is_cmd-action }">| ).

      WHEN OTHERS.
        ASSERT 0 = 1.

    ENDCASE.

  ENDMETHOD.


  METHOD render_field.

    DATA lv_opt_id TYPE string.
    DATA lv_error TYPE string.
    DATA lv_value TYPE string.
    DATA lv_size TYPE string.
    DATA lv_checked TYPE string.
    DATA lv_item_class TYPE string.
    FIELD-SYMBOLS <ls_opt> LIKE LINE OF is_field-subitems.

    " Get value and validation error from maps
    lv_value = io_values->get( is_field-name ).
    IF io_validation_log IS BOUND.
      lv_error = io_validation_log->get( is_field-name ).
    ENDIF.

    " Prepare item class
    lv_item_class = is_field-item_class.
    IF lv_error IS NOT INITIAL.
      lv_item_class = condense( lv_item_class && ' error' ).
    ENDIF.
    IF lv_item_class IS NOT INITIAL.
      lv_item_class = | class="{ lv_item_class }"|.
    ENDIF.

    " Size
    IF is_field-size > 0.
      lv_size = | size="{ is_field-size }"|.
    ENDIF.

    " Render field
    ii_html->add( |<li{ lv_item_class }>| ).

    CASE is_field-type.
      WHEN c_field_type-text.

        ii_html->add( |<label for="{ is_field-name }"{ is_field-hint }>{
          is_field-label }{ is_field-required }</label>| ).
        IF lv_error IS NOT INITIAL.
          ii_html->add( |<small>{ lv_error }</small>| ).
        ENDIF.

        IF is_field-side_action IS NOT INITIAL.
          ii_html->add( '<div class="input-container">' ). " Ugly :(
        ENDIF.

        ii_html->add( |<input type="text" name="{ is_field-name }" id="{
          is_field-name }"{ is_field-placeholder } value="{ lv_value }"{
          is_field-dblclick }{ lv_size }>| ).

        IF is_field-side_action IS NOT INITIAL.
          ii_html->add( '</div>' ).
          ii_html->add( '<div class="command-container">' ).
          ii_html->add( |<input type="submit" value="&#x2026;" formaction="sapevent:{ is_field-side_action }">| ).
          ii_html->add( '</div>' ).
        ENDIF.

      WHEN c_field_type-checkbox.

        IF lv_error IS NOT INITIAL.
          ii_html->add( |<small>{ lv_error }</small>| ).
        ENDIF.
        IF lv_value IS NOT INITIAL.
          lv_checked = ' checked'.
        ENDIF.
        ii_html->add( |<input type="checkbox" name="{ is_field-name }" id="{ is_field-name }"{ lv_checked }>| ).
        ii_html->add( |<label for="{ is_field-name }"{ is_field-hint }>{
          is_field-label }{ is_field-required }</label>| ).

      WHEN c_field_type-radio.

        ii_html->add( |<label{ is_field-hint }>{ is_field-label }{ is_field-required }</label>| ).
        IF lv_error IS NOT INITIAL.
          ii_html->add( |<small>{ lv_error }</small>| ).
        ENDIF.
        ii_html->add( |<div class="radio-container">| ).

        LOOP AT is_field-subitems ASSIGNING <ls_opt>.
          CLEAR lv_checked.
          IF lv_value = <ls_opt>-value OR ( lv_value IS INITIAL AND <ls_opt>-value = is_field-default_value ).
            lv_checked = ' checked'.
          ENDIF.
          lv_opt_id = |{ is_field-name }{ sy-tabix }|.
          ii_html->add( |<input type="radio" name="{ is_field-name }" id="{
            lv_opt_id }" value="{ <ls_opt>-value }"{ lv_checked }>| ).
          ii_html->add( |<label for="{ lv_opt_id }">{ <ls_opt>-label }</label>| ).
        ENDLOOP.

        ii_html->add( '</div>' ).

      WHEN c_field_type-hidden.

        ii_html->add( |<input type="hidden" name="{ is_field-name }" id="{
          is_field-name }" value="{ lv_value }">| ).

      WHEN OTHERS.
        ASSERT 1 = 0.
    ENDCASE.

    ii_html->add( '</li>' ).

  ENDMETHOD.


  METHOD start_group.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = c_field_type-field_group.
    ls_field-label = iv_label.
    ls_field-name  = iv_name.

    IF iv_hint IS NOT INITIAL.
      ls_field-hint    = | title="{ iv_hint }"|.
    ENDIF.

    APPEND ls_field TO mt_fields.

  ENDMETHOD.


  METHOD text.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = c_field_type-text.
    ls_field-name  = iv_name.
    ls_field-label = iv_label.
    ls_field-size = iv_size.

    IF iv_hint IS NOT INITIAL.
      ls_field-hint    = | title="{ iv_hint }"|.
    ENDIF.

    IF iv_side_action IS NOT INITIAL AND mv_form_id IS NOT INITIAL.
      ls_field-item_class = 'with-command'.
      ls_field-side_action = iv_side_action.
      ls_field-dblclick = | ondblclick="document.getElementById('{ mv_form_id
        }').action = 'sapevent:{ iv_side_action
        }'; document.getElementById('{ mv_form_id
        }').submit()"|.
    ENDIF.

    IF iv_required = abap_true.
      ls_field-required = ' <em>*</em>'.
    ENDIF.

    IF iv_placeholder IS NOT INITIAL.
      ls_field-placeholder = | placeholder="{ iv_placeholder }"|.
    ENDIF.

    APPEND ls_field TO mt_fields.

  ENDMETHOD.
ENDCLASS.
