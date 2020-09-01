CLASS /mbtools/cl_gui_asset_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
************************************************************************
* MBT GUI Asset Manager
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************

  PUBLIC SECTION.

    INTERFACES /mbtools/if_gui_asset_manager .

    TYPES:
      BEGIN OF ty_asset_entry.
        INCLUDE TYPE /mbtools/if_gui_asset_manager~ty_web_asset.
    TYPES: mime_name TYPE wwwdatatab-objid,
      END OF ty_asset_entry .
    TYPES:
      ty_asset_register TYPE STANDARD TABLE OF ty_asset_entry WITH KEY url .

    METHODS register_asset
      IMPORTING
        !iv_url       TYPE string
        !iv_type      TYPE string
        !iv_cachable  TYPE abap_bool DEFAULT abap_true
        !iv_mime_name TYPE csequence OPTIONAL
        !iv_base64    TYPE string OPTIONAL
        !iv_inline    TYPE string OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_asset_register TYPE ty_asset_register.

    METHODS get_mime_asset
      IMPORTING
        iv_mime_name    TYPE c
      RETURNING
        VALUE(rv_xdata) TYPE xstring
      RAISING
        /mbtools/cx_exception.

    METHODS load_asset
      IMPORTING
        is_asset_entry  TYPE ty_asset_entry
      RETURNING
        VALUE(rs_asset) TYPE /mbtools/if_gui_asset_manager~ty_web_asset
      RAISING
        /mbtools/cx_exception.

ENDCLASS.



CLASS /MBTOOLS/CL_GUI_ASSET_MANAGER IMPLEMENTATION.


  METHOD /mbtools/if_gui_asset_manager~get_all_assets.

    FIELD-SYMBOLS <ls_a> LIKE LINE OF mt_asset_register.

    LOOP AT mt_asset_register ASSIGNING <ls_a>.
      APPEND load_asset( <ls_a> ) TO rt_assets.
    ENDLOOP.

  ENDMETHOD.


  METHOD /mbtools/if_gui_asset_manager~get_asset.

    FIELD-SYMBOLS <ls_a> LIKE LINE OF mt_asset_register.

    READ TABLE mt_asset_register WITH KEY url = iv_url ASSIGNING <ls_a>.
    IF <ls_a> IS NOT ASSIGNED.
      /mbtools/cx_exception=>raise( |Cannot find GUI asset: { iv_url }| ).
    ENDIF.
    rs_asset = load_asset( <ls_a> ).

  ENDMETHOD.


  METHOD /mbtools/if_gui_asset_manager~get_text_asset.

    DATA ls_asset TYPE /mbtools/if_gui_asset_manager~ty_web_asset.
    ls_asset = /mbtools/if_gui_asset_manager~get_asset( iv_url ).

    IF ls_asset-type <> 'text'.
      /mbtools/cx_exception=>raise( |Not a text asset: { iv_url }| ).
    ENDIF.

    IF iv_assert_subtype IS NOT INITIAL AND ls_asset-subtype <> iv_assert_subtype.
      /mbtools/cx_exception=>raise( |Wrong subtype ({ iv_assert_subtype }): { iv_url }| ).
    ENDIF.

    rv_asset = /mbtools/cl_convert=>xstring_to_string_utf8( ls_asset-content ).

  ENDMETHOD.


  METHOD get_mime_asset.

    DATA: ls_key    TYPE wwwdatatab,
          lv_size_c TYPE wwwparams-value,
          lv_size   TYPE i,
          lt_w3mime TYPE STANDARD TABLE OF w3mime.

    ls_key-relid = 'MI'.
    ls_key-objid = iv_mime_name.

    " Get exact file size
    CALL FUNCTION 'WWWPARAMS_READ'
      EXPORTING
        relid            = ls_key-relid
        objid            = ls_key-objid
        name             = 'filesize'
      IMPORTING
        value            = lv_size_c
      EXCEPTIONS
        entry_not_exists = 1.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_size = lv_size_c.

    " Get binary data
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_w3mime
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    rv_xdata = /mbtools/cl_convert=>bintab_to_xstring(
      iv_size   = lv_size
      it_bintab = lt_w3mime ).

  ENDMETHOD.


  METHOD load_asset.

    MOVE-CORRESPONDING is_asset_entry TO rs_asset.
    IF rs_asset-content IS INITIAL AND is_asset_entry-mime_name IS NOT INITIAL.
      " inline content has the priority
      rs_asset-content = get_mime_asset( is_asset_entry-mime_name ).
    ENDIF.
    IF rs_asset-content IS INITIAL.
      /mbtools/cx_exception=>raise( |failed to load GUI asset: { is_asset_entry-url }| ).
    ENDIF.

  ENDMETHOD.


  METHOD register_asset.

    DATA ls_asset LIKE LINE OF mt_asset_register.

    READ TABLE mt_asset_register TRANSPORTING NO FIELDS
      WITH TABLE KEY url = iv_url.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    SPLIT iv_type AT '/' INTO ls_asset-type ls_asset-subtype.
    ls_asset-url          = iv_url.
    ls_asset-mime_name    = iv_mime_name.
    ls_asset-is_cacheable = iv_cachable.
    IF iv_base64 IS NOT INITIAL.
      ls_asset-content = /mbtools/cl_convert=>base64_to_xstring( iv_base64 ).
    ELSEIF iv_inline IS NOT INITIAL.
      ls_asset-content = /mbtools/cl_convert=>string_to_xstring( iv_inline ).
    ENDIF.

    APPEND ls_asset TO mt_asset_register.

  ENDMETHOD.
ENDCLASS.
