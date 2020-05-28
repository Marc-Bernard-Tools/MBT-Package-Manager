class /MBTOOLS/CL_SCREEN definition
  public
  create public .

public section.
  type-pools CNDP .

  types:
    ty_screen_field TYPE c LENGTH 83 .

  class-data GV_COPYRIGHT type STRING .
  class-data GV_ABOUT type STRING .
  class-data GV_DOCUMENTATION type STRING .
  class-data GV_TERMS type STRING .

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
      value(I_SHOW) type ABAP_BOOL default ABAP_TRUE
      value(I_TOP) type I optional
      value(I_LEFT) type I optional .
  class-methods BANNER
    importing
      value(I_TOOL) type STRING optional
      value(I_SHOW) type ABAP_BOOL default ABAP_TRUE
      value(I_TOP) type I optional
      value(I_LEFT) type I optional .
  PROTECTED SECTION.
private section.

  class-data GO_LOGO_DOCK type ref to CL_GUI_DOCKING_CONTAINER .
  class-data GO_LOGO type ref to CL_GUI_PICTURE .
  class-data GV_LOGO_URL type /MBTOOLS/VALUE .
  class-data GO_BANNER_DOCK type ref to CL_GUI_DOCKING_CONTAINER .
  class-data GO_BANNER type ref to CL_GUI_PICTURE .
  class-data GV_BANNER_URL type /MBTOOLS/VALUE .
ENDCLASS.



CLASS /MBTOOLS/CL_SCREEN IMPLEMENTATION.


  METHOD banner.

    DATA:
      content_type   TYPE w3param-cont_type,
      content_lenght TYPE w3param-cont_len,
      pic_tab        TYPE TABLE OF w3mime,
      query          TYPE w3query,
      query_table    TYPE TABLE OF w3query,
      html_table     TYPE TABLE OF w3html,
      return_code    TYPE w3param-ret_code.

    IF go_banner IS BOUND AND i_show IS INITIAL.
      CALL METHOD go_banner->clear_picture.
      CALL METHOD go_banner->free.
      FREE go_banner.
      RETURN.
    ENDIF.

    IF NOT go_banner IS BOUND.
      CREATE OBJECT go_banner EXPORTING parent = go_banner_dock.
      CHECK sy-subrc = 0.

      CALL METHOD go_banner->set_3d_border
        EXPORTING
          border = 0.

      CALL METHOD go_banner->set_display_mode
        EXPORTING
          display_mode = cl_gui_picture=>display_mode_normal.
    ENDIF.

    CALL METHOD go_banner->set_position
      EXPORTING
        height = 21
        left   = i_left
        top    = i_top
        width  = 500.

    IF gv_banner_url IS INITIAL.
      query-name  = '_OBJECT_ID'.
      query-value = '/MBTOOLS/' && i_tool.
      APPEND query TO query_table.

      CALL FUNCTION 'WWW_GET_MIME_OBJECT'
        TABLES
          query_string        = query_table
          html                = html_table
          mime                = pic_tab
        CHANGING
          return_code         = return_code
          content_type        = content_type
          content_length      = content_lenght
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
          size     = content_lenght
          lifetime = cndp_lifetime_transaction
        TABLES
          data     = pic_tab
        CHANGING
          url      = gv_banner_url
        EXCEPTIONS
          OTHERS   = 1 ##FM_SUBRC_OK.
    ENDIF.

    CALL METHOD go_banner->load_picture_from_url
      EXPORTING
        url = gv_banner_url.

  ENDMETHOD.


  METHOD class_constructor.
    gv_copyright      = |Copyright © { sy-datum(4) } Marc Bernard Tools. All right reserved.|.
    gv_about          = 'About'(001).
    gv_documentation  = 'Documentation'(002).
    gv_terms          = 'Terms'(003).
  ENDMETHOD.


  METHOD header.
    WRITE iv_icon AS ICON TO rv_result.
    rv_result+6 = iv_text.
  ENDMETHOD.


  METHOD icon.
    DATA lv_info TYPE string.

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


  METHOD logo.

    DATA:
      content_type   TYPE w3param-cont_type,
      content_lenght TYPE w3param-cont_len,
      pic_tab        TYPE TABLE OF w3mime,
      query          TYPE w3query,
      query_table    TYPE TABLE OF w3query,
      html_table     TYPE TABLE OF w3html,
      return_code    TYPE w3param-ret_code.

    IF go_logo IS BOUND AND i_show IS INITIAL.
      CALL METHOD go_logo->clear_picture.
      CALL METHOD go_logo->free.
      FREE go_logo.
      RETURN.
    ENDIF.

    IF NOT go_logo IS BOUND.
      CREATE OBJECT go_logo EXPORTING parent = go_logo_dock.
      CHECK sy-subrc = 0.

      CALL METHOD go_logo->set_3d_border
        EXPORTING
          border = 0.

      CALL METHOD go_logo->set_display_mode
        EXPORTING
          display_mode = cl_gui_picture=>display_mode_normal_center.
    ENDIF.

    CALL METHOD go_logo->set_position
      EXPORTING
        height = 27
        left   = i_left
        top    = i_top
        width  = 200.

    IF gv_logo_url IS INITIAL.
      query-name  = '_OBJECT_ID'.
      query-value = '/MBTOOLS/LOGO'.
      APPEND query TO query_table.

      CALL FUNCTION 'WWW_GET_MIME_OBJECT'
        TABLES
          query_string        = query_table
          html                = html_table
          mime                = pic_tab
        CHANGING
          return_code         = return_code
          content_type        = content_type
          content_length      = content_lenght
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
          size     = content_lenght
          lifetime = cndp_lifetime_transaction
        TABLES
          data     = pic_tab
        CHANGING
          url      = gv_logo_url
        EXCEPTIONS
          OTHERS   = 1 ##FM_SUBRC_OK.
    ENDIF.

    CALL METHOD go_logo->load_picture_from_url
      EXPORTING
        url = gv_logo_url.

  ENDMETHOD.
ENDCLASS.
