************************************************************************
* /MBTOOLS/CL_SCREEN
* MBT Screen
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
class /MBTOOLS/CL_SCREEN definition
  public
  create public .

public section.
  type-pools CNDP .

  types:
    ty_screen_field TYPE c LENGTH 83 .

  class-data MV_COPYRIGHT type STRING .
  class-data MV_ABOUT type STRING .
  class-data MV_DOCUMENTATION type STRING .
  class-data MV_TOOL_PAGE type STRING .
  class-data MV_WEBSITE type STRING .
  class-data MV_TERMS type STRING .
  class-data MV_VERSION type STRING .

  class-methods CLASS_CONSTRUCTOR .
  class-methods ICON
    importing
      value(IV_ICON) type ICON_D
      value(IV_TEXT) type CSEQUENCE optional
      value(IV_QUICK) type CSEQUENCE optional
    returning
      value(RV_RESULT) type TY_SCREEN_FIELD .
  class-methods HEADER
    importing
      value(IV_ICON) type ICON_D
      value(IV_TEXT) type CSEQUENCE optional
    returning
      value(RV_RESULT) type TY_SCREEN_FIELD .
  class-methods LOGO
    importing
      value(IV_SHOW) type ABAP_BOOL default ABAP_TRUE
      value(IV_TOP) type I optional
      value(IV_LEFT) type I optional .
  class-methods BANNER
    importing
      value(IV_TOOL) type STRING optional
      value(IV_SHOW) type ABAP_BOOL default ABAP_TRUE
      value(IV_TOP) type I optional
      value(IV_LEFT) type I optional .
  class-methods INIT
    importing
      !IR_TOOL type ref to /MBTOOLS/CL_TOOLS
    exporting
      !EV_TEXT type TY_SCREEN_FIELD
      !EV_ABOUT type TY_SCREEN_FIELD
      !EV_TITLE type TY_SCREEN_FIELD
      !EV_VERSION type TY_SCREEN_FIELD
      !EV_COPYRIGHT type TY_SCREEN_FIELD
      !EV_DOCU type TY_SCREEN_FIELD
      !EV_TOOL type TY_SCREEN_FIELD
      !EV_HOME type TY_SCREEN_FIELD .
  PROTECTED SECTION.
private section.

  class-data MO_LOGO_DOCK type ref to CL_GUI_DOCKING_CONTAINER .
  class-data MO_LOGO type ref to CL_GUI_PICTURE .
  class-data MV_LOGO_URL type /MBTOOLS/VALUE .
  class-data MO_BANNER_DOCK type ref to CL_GUI_DOCKING_CONTAINER .
  class-data MO_BANNER type ref to CL_GUI_PICTURE .
  class-data MV_BANNER_URL type /MBTOOLS/VALUE .
ENDCLASS.



CLASS /MBTOOLS/CL_SCREEN IMPLEMENTATION.


  METHOD banner.

    DATA:
      lv_content_type   TYPE w3param-cont_type,
      lv_content_lenght TYPE w3param-cont_len,
      lt_pic            TYPE TABLE OF w3mime,
      ls_query          TYPE w3query,
      lt_query          TYPE TABLE OF w3query,
      lt_html           TYPE TABLE OF w3html,
      lv_return_code    TYPE w3param-ret_code.

    IF mo_banner IS BOUND AND iv_show IS INITIAL.
      CALL METHOD mo_banner->clear_picture.
      CALL METHOD mo_banner->free.
      FREE mo_banner.
      RETURN.
    ENDIF.

    IF NOT mo_banner IS BOUND.
      CREATE OBJECT mo_banner EXPORTING parent = mo_banner_dock.

      CALL METHOD mo_banner->set_3d_border
        EXPORTING
          border = 0.

      CALL METHOD mo_banner->set_display_mode
        EXPORTING
          display_mode = cl_gui_picture=>display_mode_normal.
    ENDIF.

    CALL METHOD mo_banner->set_position
      EXPORTING
        height = 21
        left   = iv_left
        top    = iv_top
        width  = 500.

    IF mv_banner_url IS INITIAL.
      ls_query-name  = '_OBJECT_ID'.
      ls_query-value = iv_tool.
      APPEND ls_query TO lt_query.

      CALL FUNCTION 'WWW_GET_MIME_OBJECT'
        TABLES
          query_string        = lt_query
          html                = lt_html
          mime                = lt_pic
        CHANGING
          return_code         = lv_return_code
          content_type        = lv_content_type
          content_length      = lv_content_lenght
        EXCEPTIONS
          object_not_found    = 1
          parameter_not_found = 2
          OTHERS              = 3 ##FM_OLDED.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      CALL FUNCTION 'DP_CREATE_URL'
        EXPORTING
          type     = 'IMAGE'
          subtype  = cndp_sap_subtype_unknown
          size     = lv_content_lenght
          lifetime = cndp_lifetime_transaction
        TABLES
          data     = lt_pic
        CHANGING
          url      = mv_banner_url
        EXCEPTIONS
          OTHERS   = 1 ##FM_SUBRC_OK.
    ENDIF.

    CALL METHOD mo_banner->load_picture_from_url
      EXPORTING
        url = mv_banner_url.

  ENDMETHOD.


  METHOD class_constructor.
    mv_copyright      = |Copyright © { sy-datum(4) } Marc Bernard Tools. All right reserved.|.
    mv_about          = 'About'(001).
    mv_documentation  = 'Documentation'(002).
    mv_terms          = 'Terms'(003).
    mv_tool_page      = 'Tool Page'(004).
    mv_website        = 'MBT Website'(005).
    mv_version        = 'Version'(006).
  ENDMETHOD.


  METHOD header.
    WRITE iv_icon AS ICON TO rv_result.
    rv_result+6 = iv_text.
  ENDMETHOD.


  METHOD icon.

    DATA:
      lv_info TYPE string.

    IF iv_quick IS INITIAL.
      lv_info = iv_text.
    ELSE.
      lv_info = iv_quick.
    ENDIF.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name   = iv_icon
        text   = iv_text
        info   = lv_info
      IMPORTING
        result = rv_result.

  ENDMETHOD.


  METHOD init.

    ev_text = ir_tool->get_description( ).

    ev_about = header(
      iv_icon = icon_system_help
      iv_text = mv_about ).

    ev_title     = ir_tool->get_title( ).
    ev_version   = mv_version && ` ` && ir_tool->get_version( ).
    ev_copyright = mv_copyright.

    ev_docu = icon(
      iv_icon  = icon_system_extended_help
      iv_text  = mv_documentation
      iv_quick = ir_tool->get_title( ) ).

    ev_tool = icon(
      iv_icon  = icon_tools
      iv_text  = mv_tool_page
      iv_quick = ir_tool->get_title( ) ).

    ev_home = icon(
      iv_icon  = icon_url
      iv_text  = /mbtools/cl_base=>c_title
      iv_quick = mv_website ).

  ENDMETHOD.


  METHOD logo.

    DATA:
      lv_content_type   TYPE w3param-cont_type,
      lv_content_lenght TYPE w3param-cont_len,
      lt_pic            TYPE TABLE OF w3mime,
      ls_query          TYPE w3query,
      lt_query          TYPE TABLE OF w3query,
      lt_html           TYPE TABLE OF w3html,
      lv_return_code    TYPE w3param-ret_code.

    IF mo_logo IS BOUND AND iv_show IS INITIAL.
      CALL METHOD mo_logo->clear_picture.
      CALL METHOD mo_logo->free.
      FREE mo_logo.
      RETURN.
    ENDIF.

    IF NOT mo_logo IS BOUND.
      CREATE OBJECT mo_logo EXPORTING parent = mo_logo_dock.

      CALL METHOD mo_logo->set_3d_border
        EXPORTING
          border = 0.

      CALL METHOD mo_logo->set_display_mode
        EXPORTING
          display_mode = cl_gui_picture=>display_mode_normal_center.
    ENDIF.

    CALL METHOD mo_logo->set_position
      EXPORTING
        height = 27
        left   = iv_left
        top    = iv_top
        width  = 200.

    IF mv_logo_url IS INITIAL.
      ls_query-name  = '_OBJECT_ID'.
      ls_query-value = '/MBTOOLS/LOGO'.
      APPEND ls_query TO lt_query.

      CALL FUNCTION 'WWW_GET_MIME_OBJECT'
        TABLES
          query_string        = lt_query
          html                = lt_html
          mime                = lt_pic
        CHANGING
          return_code         = lv_return_code
          content_type        = lv_content_type
          content_length      = lv_content_lenght
        EXCEPTIONS
          object_not_found    = 1
          parameter_not_found = 2
          OTHERS              = 3 ##FM_OLDED.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      CALL FUNCTION 'DP_CREATE_URL'
        EXPORTING
          type     = 'IMAGE'
          subtype  = cndp_sap_subtype_unknown
          size     = lv_content_lenght
          lifetime = cndp_lifetime_transaction
        TABLES
          data     = lt_pic
        CHANGING
          url      = mv_logo_url
        EXCEPTIONS
          OTHERS   = 1 ##FM_SUBRC_OK.
    ENDIF.

    CALL METHOD mo_logo->load_picture_from_url
      EXPORTING
        url = mv_logo_url.

  ENDMETHOD.
ENDCLASS.
