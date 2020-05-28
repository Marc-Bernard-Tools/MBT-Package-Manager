************************************************************************
* /MBTOOLS/CL_SCREEN
* MBT Screen
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_screen DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS cndp .

    TYPES:
      ty_screen_field TYPE c LENGTH 83 .

    CLASS-DATA gv_copyright TYPE string .
    CLASS-DATA gv_about TYPE string .
    CLASS-DATA gv_documentation TYPE string .
    CLASS-DATA gv_tool_page TYPE string .
    CLASS-DATA gv_website TYPE string .
    CLASS-DATA gv_terms TYPE string .

    CLASS-METHODS class_constructor .
    CLASS-METHODS icon
      IMPORTING
        VALUE(iv_icon)   TYPE icon_d
        VALUE(iv_text)   TYPE csequence OPTIONAL
        VALUE(iv_quick)  TYPE csequence OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE ty_screen_field .
    CLASS-METHODS header
      IMPORTING
        VALUE(iv_icon)   TYPE icon_d
        VALUE(iv_text)   TYPE csequence OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE ty_screen_field .
    CLASS-METHODS logo
      IMPORTING
        VALUE(i_show) TYPE abap_bool DEFAULT abap_true
        VALUE(i_top)  TYPE i OPTIONAL
        VALUE(i_left) TYPE i OPTIONAL .
    CLASS-METHODS banner
      IMPORTING
        VALUE(i_tool) TYPE string OPTIONAL
        VALUE(i_show) TYPE abap_bool DEFAULT abap_true
        VALUE(i_top)  TYPE i OPTIONAL
        VALUE(i_left) TYPE i OPTIONAL .
    CLASS-METHODS init
      IMPORTING
        !ir_tool     TYPE REF TO /mbtools/cl_tools
      EXPORTING
        !e_text      TYPE ty_screen_field
        !e_about     TYPE ty_screen_field
        !e_title     TYPE ty_screen_field
        !e_version   TYPE ty_screen_field
        !e_copyright TYPE ty_screen_field
        !e_docu      TYPE ty_screen_field
        !e_tool      TYPE ty_screen_field
        !e_home      TYPE ty_screen_field .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_logo_dock TYPE REF TO cl_gui_docking_container .
    CLASS-DATA go_logo TYPE REF TO cl_gui_picture .
    CLASS-DATA gv_logo_url TYPE /mbtools/value .
    CLASS-DATA go_banner_dock TYPE REF TO cl_gui_docking_container .
    CLASS-DATA go_banner TYPE REF TO cl_gui_picture .
    CLASS-DATA gv_banner_url TYPE /mbtools/value .
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
      query-value = i_tool.
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
    gv_tool_page      = 'Tool Page'(004).
    gv_website        = 'MBT Website'(005).
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


  METHOD init.

    e_text = ir_tool->get_description( ).

    e_about = header(
      iv_icon = icon_system_help
      iv_text = 'About' ).

    e_title     = ir_tool->get_title( ).
    e_version   = | Version { ir_tool->get_version( ) } |.
    e_copyright = gv_copyright.

    e_docu = icon(
      iv_icon  = icon_system_extended_help
      iv_text  = gv_documentation
      iv_quick = ir_tool->get_title( ) ).

    e_tool = icon(
      iv_icon  = icon_tools
      iv_text  = gv_tool_page
      iv_quick = ir_tool->get_title( ) ).

    e_home = icon(
      iv_icon  = icon_url
      iv_text  = /mbtools/cl_base=>c_title
      iv_quick = gv_website ).

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
