class /MBTOOLS/CL_HTML_TOOLBAR definition
  public
  create public .

************************************************************************
* MBT HTML Toolbar
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************
public section.

  methods CONSTRUCTOR
    importing
      !IV_ID type STRING optional .
  methods ADD
    importing
      !IV_TXT type STRING
      !IO_SUB type ref to /MBTOOLS/CL_HTML_TOOLBAR optional
      !IV_TYP type C default /MBTOOLS/IF_HTML=>C_ACTION_TYPE-SAPEVENT
      !IV_ACT type STRING optional
      !IV_ICO type STRING optional
      !IV_CUR type ABAP_BOOL optional
      !IV_OPT type C optional
      !IV_CHK type ABAP_BOOL default ABAP_UNDEFINED
      !IV_AUX type STRING optional
      !IV_ID type STRING optional
      !IV_TITLE type STRING optional
    returning
      value(RO_SELF) type ref to /MBTOOLS/CL_HTML_TOOLBAR .
  methods COUNT
    returning
      value(RV_COUNT) type I .
  methods RENDER
    importing
      !IV_RIGHT type ABAP_BOOL optional
      !IV_SORT type ABAP_BOOL optional
    returning
      value(RI_HTML) type ref to /MBTOOLS/IF_HTML .
  methods RENDER_AS_DROPLIST
    importing
      !IV_LABEL type STRING
      !IV_RIGHT type ABAP_BOOL optional
      !IV_SORT type ABAP_BOOL optional
      !IV_CORNER type ABAP_BOOL optional
      !IV_ACTION type STRING optional
    returning
      value(RI_HTML) type ref to /MBTOOLS/IF_HTML .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_item,
        txt   TYPE string,
        act   TYPE string,
        ico   TYPE string,
        sub   TYPE REF TO /mbtools/cl_html_toolbar,
        opt   TYPE char1,
        typ   TYPE char1,
        cur   TYPE abap_bool,
        chk   TYPE abap_bool,
        aux   TYPE string,
        id    TYPE string,
        title TYPE string,
      END OF ty_item.

    TYPES ty_items TYPE STANDARD TABLE OF ty_item.

    DATA: mt_items TYPE ty_items,
          mv_id    TYPE string.

    METHODS:
      render_items
        IMPORTING
          iv_sort        TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(ri_html) TYPE REF TO /mbtools/if_html .

ENDCLASS.



CLASS /MBTOOLS/CL_HTML_TOOLBAR IMPLEMENTATION.


  METHOD add.
    DATA ls_item TYPE ty_item.

    ASSERT iv_typ = /mbtools/if_html=>c_action_type-separator  " sep doesn't have action
      OR iv_typ = /mbtools/if_html=>c_action_type-onclick      " click may have no action (assigned in JS)
      OR iv_typ = /mbtools/if_html=>c_action_type-dummy        " dummy may have no action
      OR iv_act IS INITIAL AND io_sub IS NOT INITIAL
      OR iv_act IS NOT INITIAL AND io_sub IS INITIAL. " Only one supplied

    ASSERT NOT ( iv_chk <> abap_undefined AND io_sub IS NOT INITIAL ).

    ls_item-txt   = iv_txt.
    ls_item-act   = iv_act.
    ls_item-ico   = iv_ico.
    ls_item-sub   = io_sub.
    ls_item-opt   = iv_opt.
    ls_item-typ   = iv_typ.
    ls_item-cur   = iv_cur.
    ls_item-chk   = iv_chk.
    ls_item-aux   = iv_aux.
    ls_item-id    = iv_id.
    ls_item-title = iv_title.

    APPEND ls_item TO mt_items.

    ro_self = me.

  ENDMETHOD.


  METHOD constructor.
    mv_id = iv_id.
  ENDMETHOD.


  METHOD count.
    rv_count = lines( mt_items ).
  ENDMETHOD.


  METHOD render.

    DATA: lv_class TYPE string.

    CREATE OBJECT ri_html TYPE /mbtools/cl_html.

    lv_class = 'nav-container' ##NO_TEXT.
    IF iv_right = abap_true.
      lv_class = lv_class && ' float-right'.
    ENDIF.

    ri_html->add( |<div class="{ lv_class }">| ).
    ri_html->add( render_items( iv_sort = iv_sort ) ).
    ri_html->add( '</div><!--nav-->' ).

  ENDMETHOD.


  METHOD render_as_droplist.

    DATA: lv_class TYPE string.

    CREATE OBJECT ri_html TYPE /mbtools/cl_html.

    lv_class = 'nav-container' ##NO_TEXT.
    IF iv_right = abap_true.
      lv_class = lv_class && ' float-right'.
    ENDIF.
    IF iv_corner = abap_true.
      lv_class = lv_class && ' corner'.
    ENDIF.

    ri_html->add( |<div class="{ lv_class }">| ).
    ri_html->add( '<ul><li>' ).
    ri_html->add_a( iv_txt = iv_label
                    iv_typ = /mbtools/if_html=>c_action_type-sapevent
                    iv_act = iv_action ).
    ri_html->add( '<div class="minizone"></div>' ).
    ri_html->add( render_items( iv_sort = iv_sort ) ).
    ri_html->add( '</li></ul>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_items.

    DATA: lv_class     TYPE string,
          lv_icon      TYPE string,
          lv_id        TYPE string,
          lv_check     TYPE string,
          lv_aux       TYPE string,
          lv_has_icons TYPE abap_bool.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF mt_items.

    CREATE OBJECT ri_html TYPE /mbtools/cl_html.

    IF iv_sort = abap_true.
      SORT mt_items BY txt ASCENDING AS TEXT.
    ENDIF.

    " Check has icons or check boxes
    LOOP AT mt_items ASSIGNING <ls_item> WHERE ico IS NOT INITIAL OR chk <> abap_undefined.
      lv_has_icons = abap_true.
      lv_class     = ' class="with-icons"'.
      EXIT.
    ENDLOOP.

    IF mv_id IS NOT INITIAL.
      lv_id = | id="{ mv_id }"|.
    ENDIF.

    ri_html->add( |<ul{ lv_id }{ lv_class }>| ).

    " Render items
    LOOP AT mt_items ASSIGNING <ls_item>.
      CLEAR: lv_class, lv_icon.

      IF <ls_item>-typ = /mbtools/if_html=>c_action_type-separator.
        ri_html->add( |<li class="separator">{ <ls_item>-txt }</li>| ).
        CONTINUE.
      ENDIF.

      IF lv_has_icons = abap_true.
        IF <ls_item>-chk = abap_true.
          lv_icon  = /mbtools/cl_html=>icon( 'check/blue' ).
          lv_check = ' data-check="X"'.
        ELSEIF <ls_item>-chk = abap_false.
          lv_icon = /mbtools/cl_html=>icon( 'check/grey' ).
          lv_check = ' data-check=""'.
        ELSE. " abap_undefined -> not a check box
          lv_icon = /mbtools/cl_html=>icon( <ls_item>-ico ).
        ENDIF.
      ENDIF.

      IF <ls_item>-cur = abap_true.
        lv_class = ' class="current-menu-item"'.
      ENDIF.

      IF <ls_item>-aux IS NOT INITIAL.
        lv_aux = | data-aux="{ <ls_item>-aux }"|.
      ENDIF.

      ri_html->add( |<li{ lv_class }{ lv_check }{ lv_aux }>| ).
      IF <ls_item>-sub IS INITIAL.
        ri_html->add_a( iv_txt   = lv_icon && <ls_item>-txt
                        iv_typ   = <ls_item>-typ
                        iv_act   = <ls_item>-act
                        iv_id    = <ls_item>-id
                        iv_opt   = <ls_item>-opt
                        iv_title = <ls_item>-title ).
      ELSE.
        ri_html->add_a( iv_txt   = lv_icon && <ls_item>-txt
                        iv_typ   = /mbtools/if_html=>c_action_type-dummy
                        iv_act   = ''
                        iv_class = 'has-submenu'  "<<<MBT
                        iv_id    = <ls_item>-id
                        iv_opt   = <ls_item>-opt
                        iv_title = <ls_item>-title ).
        ri_html->add( <ls_item>-sub->render_items( iv_sort ) ).
      ENDIF.
      ri_html->add( '</li>' ).

    ENDLOOP.

    ri_html->add( '</ul>' ).

  ENDMETHOD.
ENDCLASS.
