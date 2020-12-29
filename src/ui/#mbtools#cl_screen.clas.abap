CLASS /mbtools/cl_screen DEFINITION
  PUBLIC
  CREATE PUBLIC .

************************************************************************
* MBT Screen
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
  PUBLIC SECTION.

    TYPES:
      ty_screen_field TYPE c LENGTH 83 .

    CLASS-DATA gv_copyright TYPE string READ-ONLY .
    CLASS-DATA gv_about TYPE string READ-ONLY .
    CLASS-DATA gv_documentation TYPE string READ-ONLY .
    CLASS-DATA gv_tool_page TYPE string READ-ONLY .
    CLASS-DATA gv_website_name TYPE string READ-ONLY .
    CLASS-DATA gv_website_domain TYPE string READ-ONLY .
    CLASS-DATA gv_terms TYPE string READ-ONLY .
    CLASS-DATA gv_version TYPE string READ-ONLY .

    CLASS-METHODS class_constructor .
    METHODS constructor
      IMPORTING
        !iv_title TYPE csequence .
    CLASS-METHODS factory
      IMPORTING
        !iv_title        TYPE csequence OPTIONAL
      RETURNING
        VALUE(ro_screen) TYPE REF TO /mbtools/cl_screen .
    METHODS init
      EXPORTING
        !ev_text      TYPE ty_screen_field
        !ev_about     TYPE ty_screen_field
        !ev_title     TYPE ty_screen_field
        !ev_version   TYPE ty_screen_field
        !ev_copyright TYPE ty_screen_field
        !ev_docu      TYPE ty_screen_field
        !ev_tool      TYPE ty_screen_field
        !ev_home      TYPE ty_screen_field .
    METHODS header
      IMPORTING
        VALUE(iv_icon)   TYPE icon_d
        VALUE(iv_text)   TYPE csequence OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE ty_screen_field .
    METHODS icon
      IMPORTING
        VALUE(iv_icon)   TYPE icon_d
        VALUE(iv_text)   TYPE csequence OPTIONAL
        VALUE(iv_quick)  TYPE csequence OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE ty_screen_field .
    METHODS logo
      IMPORTING
        VALUE(iv_show) TYPE abap_bool DEFAULT abap_true
        VALUE(iv_top)  TYPE i OPTIONAL
        VALUE(iv_left) TYPE i OPTIONAL .
    METHODS banner
      IMPORTING
        VALUE(iv_show) TYPE abap_bool DEFAULT abap_true
        VALUE(iv_top)  TYPE i DEFAULT 4
        VALUE(iv_left) TYPE i DEFAULT 20
          PREFERRED PARAMETER iv_show .
    METHODS ucomm
      IMPORTING
        VALUE(iv_ok_code) TYPE sy-ucomm .
    METHODS toolbar
      IMPORTING
        !iv_dynnr TYPE sy-dynnr
        !iv_cprog TYPE sy-cprog DEFAULT sy-cprog
        !iv_show  TYPE abap_bool DEFAULT abap_false .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_logo_dock TYPE REF TO cl_gui_docking_container .
    CLASS-DATA go_logo TYPE REF TO cl_gui_picture .
    CLASS-DATA gv_logo_url TYPE /mbtools/value .
    CLASS-DATA go_banner_dock TYPE REF TO cl_gui_docking_container .
    CLASS-DATA go_banner TYPE REF TO cl_gui_picture .
    CLASS-DATA gv_banner_url TYPE /mbtools/value .

    DATA mo_tool TYPE REF TO /mbtools/cl_tools.
ENDCLASS.



CLASS /mbtools/cl_screen IMPLEMENTATION.


  METHOD banner.

    DATA:
      lv_content_type   TYPE w3param-cont_type,
      lv_content_lenght TYPE w3param-cont_len,
      lt_pic            TYPE TABLE OF w3mime,
      ls_query          TYPE w3query,
      lt_query          TYPE TABLE OF w3query,
      lt_html           TYPE TABLE OF w3html,
      lv_return_code    TYPE w3param-ret_code.

    IF go_banner IS BOUND AND iv_show IS INITIAL.
      go_banner->clear_picture( ).
      go_banner->free( ).
      FREE go_banner.
      RETURN.
    ENDIF.

    IF go_banner IS NOT BOUND.
      CREATE OBJECT go_banner EXPORTING parent = go_banner_dock.

      go_banner->set_3d_border( border = 0 ).

      go_banner->set_display_mode( display_mode = cl_gui_picture=>display_mode_normal ).
    ENDIF.

    go_banner->set_position( height = 21
                             left   = iv_left
                             top    = iv_top
                             width  = 500 ).             "#EC NUMBER_OK

    IF gv_banner_url IS INITIAL.
      ls_query-name  = '_OBJECT_ID'.
      ls_query-value = mo_tool->get_id( ).
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
          url      = gv_banner_url
        EXCEPTIONS
          OTHERS   = 1 ##FM_SUBRC_OK.
    ENDIF.

    go_banner->load_picture_from_url( url = gv_banner_url ).

  ENDMETHOD.


  METHOD class_constructor.
    gv_copyright      = |Copyright © { sy-datum(4) } Marc Bernard Tools. All right reserved.|.
    gv_about          = 'About'(001).
    gv_documentation  = 'Documentation'(002).
    gv_terms          = 'Terms'(003).
    gv_tool_page      = 'Tool Page'(004).
    gv_website_name   = 'MBT Website'(005).
    gv_website_domain = 'MarcBernardTools.com' ##NO_TEXT.
    gv_version        = 'Version'(006).
  ENDMETHOD.


  METHOD constructor.
    mo_tool = /mbtools/cl_tools=>factory( iv_title ).
  ENDMETHOD.


  METHOD factory.
    CREATE OBJECT ro_screen EXPORTING iv_title = iv_title.
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

    ev_text = mo_tool->get_description( ).

    ev_about = header(
      iv_icon = icon_system_help
      iv_text = gv_about ).

    ev_title     = mo_tool->get_title( ).
    ev_version   = gv_version && ` ` && mo_tool->get_version( ).
    ev_copyright = gv_copyright.

    ev_docu = icon(
      iv_icon  = icon_system_extended_help
      iv_text  = gv_documentation
      iv_quick = mo_tool->get_title( ) ).

    ev_tool = icon(
      iv_icon  = icon_tools
      iv_text  = gv_tool_page
      iv_quick = mo_tool->get_title( ) ).

    ev_home = icon(
      iv_icon  = icon_url
      iv_text  = gv_website_domain
      iv_quick = gv_website_name ).

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

    IF go_logo IS BOUND AND iv_show IS INITIAL.
      go_logo->clear_picture( ).
      go_logo->free( ).
      FREE go_logo.
      RETURN.
    ENDIF.

    IF go_logo IS NOT BOUND.
      CREATE OBJECT go_logo EXPORTING parent = go_logo_dock.

      go_logo->set_3d_border( border = 0 ).

      go_logo->set_display_mode( display_mode = cl_gui_picture=>display_mode_normal_center ).
    ENDIF.

    go_logo->set_position( height = 27
                           left   = iv_left
                           top    = iv_top
                           width  = 200 ).               "#EC NUMBER_OK

    IF gv_logo_url IS INITIAL.
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
          url      = gv_logo_url
        EXCEPTIONS
          OTHERS   = 1 ##FM_SUBRC_OK.
    ENDIF.

    go_logo->load_picture_from_url( url = gv_logo_url ).

  ENDMETHOD.


  METHOD toolbar.

    DATA: ls_header               TYPE rpy_dyhead,
          lt_containers           TYPE dycatt_tab,
          lt_fields_to_containers TYPE dyfatc_tab,
          lt_flow_logic           TYPE swydyflow.

    CALL FUNCTION 'RPY_DYNPRO_READ'
      EXPORTING
        progname             = iv_cprog
        dynnr                = iv_dynnr
      IMPORTING
        header               = ls_header
      TABLES
        containers           = lt_containers
        fields_to_containers = lt_fields_to_containers
        flow_logic           = lt_flow_logic
      EXCEPTIONS
        cancelled            = 1
        not_found            = 2
        permission_error     = 3
        OTHERS               = 4.
    IF sy-subrc IS NOT INITIAL.
      RETURN. " Ignore errors, just exit
    ENDIF.

    IF ls_header-no_toolbar <> iv_show.
      RETURN. " No change required
    ENDIF.

    ls_header-no_toolbar = boolc( iv_show = abap_false ).

    CALL FUNCTION 'RPY_DYNPRO_INSERT'
      EXPORTING
        header                 = ls_header
        suppress_exist_checks  = abap_true
      TABLES
        containers             = lt_containers
        fields_to_containers   = lt_fields_to_containers
        flow_logic             = lt_flow_logic
      EXCEPTIONS
        cancelled              = 1
        already_exists         = 2
        program_not_exists     = 3
        not_executed           = 4
        missing_required_field = 5
        illegal_field_value    = 6
        field_not_allowed      = 7
        not_generated          = 8
        illegal_field_position = 9
        OTHERS                 = 10.
    IF sy-subrc <> 2 AND sy-subrc <> 0.
      RETURN. " Ignore errors, just exit
    ENDIF.

  ENDMETHOD.


  METHOD ucomm.

    CHECK sy-dynnr <> '1000'.

    CASE iv_ok_code.

        " About tab
      WHEN 'DOCU'.
        /mbtools/cl_utilities=>call_browser( mo_tool->get_url_docs( ) ).

      WHEN 'TOOL'.
        /mbtools/cl_utilities=>call_browser( mo_tool->get_url_tool( ) ).

      WHEN 'HOME'.
        /mbtools/cl_utilities=>call_browser( /mbtools/if_definitions=>c_www_home ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
