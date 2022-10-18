CLASS /mbtools/cl_tlogo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* MBT TLOGO
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS class_constructor.
    CLASS-METHODS get_tlogo_from_tlogo_d
      IMPORTING
        !iv_tlogo_d     TYPE rstlogo
      RETURNING
        VALUE(rv_tlogo) TYPE rstlogo.
    CLASS-METHODS get_tlogo_icon
      IMPORTING
        !iv_tlogo      TYPE rstlogo
        !iv_tlogo_sub  TYPE csequence OPTIONAL
        !iv_icon       TYPE icon_d OPTIONAL
      RETURNING
        VALUE(rv_icon) TYPE icon_d.
    CLASS-METHODS get_tlogo_text
      IMPORTING
        !iv_tlogo      TYPE rstlogo
      RETURNING
        VALUE(rv_text) TYPE rstxtlg.
    CLASS-METHODS get_object_text
      IMPORTING
        !iv_tlogo      TYPE rstlogo
        !iv_object     TYPE csequence
      RETURNING
        VALUE(rv_text) TYPE rstxtlg
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS get_tlogo_sub
      IMPORTING
        !iv_tlogo           TYPE rstlogo
        !iv_object          TYPE csequence
      RETURNING
        VALUE(rv_tlogo_sub) TYPE /mbtools/tlogo_sub
      RAISING
        /mbtools/cx_exception.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_tlogo_text,
        tlogo TYPE rstlogo,
        txtlg TYPE rstxtlg,
      END OF ty_tlogo_text,
      BEGIN OF ty_tlogo_cont,
        tlogo_d TYPE rstlogo,
        tlogo   TYPE rstlogo,
      END OF ty_tlogo_cont.

    CLASS-DATA:
      go_repository TYPE REF TO cl_rso_repository,
      gt_tlogo_text TYPE HASHED TABLE OF ty_tlogo_text WITH UNIQUE KEY tlogo,
      gt_tlogo_cont TYPE HASHED TABLE OF ty_tlogo_cont WITH UNIQUE KEY tlogo_d.

ENDCLASS.



CLASS /mbtools/cl_tlogo IMPLEMENTATION.


  METHOD class_constructor.

    DATA:
      ls_tlogo_text TYPE ty_tlogo_text.

    go_repository = cl_rso_repository=>get_repository( ).

    " Get TLOGOs (no content TLOGOs)
    SELECT DISTINCT domvalue_l ddtext INTO TABLE gt_tlogo_text
      FROM dd07t
      WHERE domname    = 'RSTLOGO'
        AND ddlanguage = sy-langu
        AND as4local   = rs_c_objvers-active
      ORDER BY domvalue_l.                               "#EC CI_BYPASS
    ASSERT sy-subrc = 0.

    " Add Meta Object, Application Component Hierarchty, and DataSource
    ls_tlogo_text-tlogo = 'BIMO'.
    ls_tlogo_text-txtlg = 'BI Meta (Transport) Object Type'(004).
    INSERT ls_tlogo_text INTO TABLE gt_tlogo_text.
    ls_tlogo_text-tlogo = 'DSAA'.
    ls_tlogo_text-txtlg = 'Application Component Hierarchty'(003).
    INSERT ls_tlogo_text INTO TABLE gt_tlogo_text.
    ls_tlogo_text-tlogo = 'OSOA'.
    ls_tlogo_text-txtlg = 'OLTP DataSource'(002).
    INSERT ls_tlogo_text INTO TABLE gt_tlogo_text.

    SELECT DISTINCT tlogo_d tlogo INTO TABLE gt_tlogo_cont
      FROM rstlogoprop
      WHERE tlogo_d <> ''
      ORDER BY tlogo_d tlogo.                            "#EC CI_BYPASS
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD get_object_text.

    DATA:
      lv_oltpsource TYPE rsaot_oltpsource,
      ls_object     TYPE rso_s_tlogo,
      lv_txtsh      TYPE rstxtsh,
      lv_txtlg      TYPE rstxtlg.

    CASE iv_tlogo.
      WHEN 'BIMO'.
        " BI Meta (Transport) Object Type
        SELECT SINGLE txtlg FROM rsobjs_obj_typet INTO lv_txtlg
          WHERE langu = sy-langu AND obj_type = iv_object.

      WHEN 'DSAA'.
        " Application component hierarchy
        SELECT SINGLE txtsh txtlg FROM rodsapplt INTO (lv_txtsh, lv_txtlg)
          WHERE langu = sy-langu AND hier = 'APCO' AND applnm = iv_object
            AND objvers = rs_c_objvers-active.

      WHEN 'OSOA'.
        " OLTP DataSource
        lv_oltpsource = iv_object(30).

        SELECT SINGLE txtsh txtlg FROM roosourcet INTO (lv_txtsh, lv_txtlg)
          WHERE langu = sy-langu AND oltpsource = lv_oltpsource
            AND objvers = rs_c_objvers-active.

      WHEN OTHERS.
        " All other objects
        ls_object-tlogo = iv_tlogo.
        ls_object-objnm = iv_object.

        go_repository->get_properties_of_object(
          EXPORTING
            i_s_object       = ls_object
          IMPORTING
            e_txtsh          = lv_txtsh
            e_txtlg          = lv_txtlg
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2 ).
        IF sy-subrc <> 0.
          /mbtools/cx_exception=>raise( |Object { iv_tlogo } { iv_object } not found| ).
        ENDIF.
    ENDCASE.

    " Return longer description
    IF lv_txtlg IS INITIAL.
      IF lv_txtsh IS INITIAL.
        rv_text = 'No text'(001).
      ELSE.
        rv_text = lv_txtsh.
      ENDIF.
    ELSE.
      rv_text = lv_txtlg.
    ENDIF.

  ENDMETHOD.


  METHOD get_tlogo_from_tlogo_d.

    DATA:
      ls_tlogo_cont TYPE ty_tlogo_cont.

    READ TABLE gt_tlogo_cont INTO ls_tlogo_cont
      WITH TABLE KEY tlogo_d = iv_tlogo_d.
    IF sy-subrc = 0.
      rv_tlogo = ls_tlogo_cont-tlogo.
    ENDIF.

  ENDMETHOD.


  METHOD get_tlogo_icon.

    DATA:
      lv_elem_type TYPE rzd1_deftp,
      lv_iobj_type TYPE rsd_iobjtp,
      lv_cube_type TYPE rscubetype,
      lv_lsys_type TYPE rsa_srctype.

    " Get icon
    IF iv_icon IS INITIAL.
      CASE iv_tlogo.
        WHEN 'BIMO'. " BI Meta (Transport) Object Type
          rv_icon = icon_transport.

        WHEN 'PCLA'. " Program Class for BW Generation Tools
          rv_icon = icon_generate.

        WHEN 'DSAA' OR 'DSAD'. " Application component hierarchy
          rv_icon = cl_rso_repository=>get_tlogo_icon( rs_c_tlogo-application ).

        WHEN 'OSOA' OR 'OSOD'. " OLTP DataSource
          CASE iv_tlogo_sub.
            WHEN 'TRAN'.
              rv_icon = cl_rso_repository=>get_tlogo_icon( rs_c_tlogo-datasource ).
            WHEN 'ATTR'.
              rv_icon = icon_master_data_act.
            WHEN 'HIER'.
              rv_icon = icon_hierarchy_act.
            WHEN 'TEXT'.
              rv_icon = icon_text_act.
          ENDCASE.

        WHEN rs_c_tlogo-element OR rs_c_tlogo-d_element.
          IF iv_tlogo_sub IS INITIAL.
            lv_elem_type = 'REP'.
          ELSE.
            lv_elem_type = iv_tlogo_sub.
          ENDIF.

          rv_icon = cl_rso_repository=>get_tlogo_icon( i_tlogo              = iv_tlogo
                                                       i_query_element_type = lv_elem_type ).

          " Additional cases
          CASE lv_elem_type.
            WHEN rzd1_c_deftp-sel_object.
              rv_icon = icon_filter.
            WHEN rzd1_c_deftp-sheet.
              rv_icon = icon_biw_report.
            WHEN rzd1_c_deftp-cell.
              rv_icon = icon_ranking.
            WHEN rzd1_c_deftp-exception.
              rv_icon = icon_bw_exception_monitor.
            WHEN rzd1_c_deftp-condition.
              rv_icon = icon_summarize.
          ENDCASE.

        WHEN rs_c_tlogo-infoobject OR rs_c_tlogo-d_infoobject.
          lv_iobj_type = iv_tlogo_sub.

          rv_icon = cl_rso_repository=>get_tlogo_icon( i_tlogo  = iv_tlogo
                                                       i_iobjtp = lv_iobj_type ).

        WHEN rs_c_tlogo-infocube OR rs_c_tlogo-d_infocube.
          lv_cube_type = iv_tlogo_sub.

          rv_icon = cl_rso_repository=>get_tlogo_icon( i_tlogo    = iv_tlogo
                                                       i_cubetype = lv_cube_type ).

          " Additional cases
          CASE lv_cube_type.
            WHEN rsd_c_cubetype-virtual.
              rv_icon = icon_biw_virtual_info_provider.
            WHEN rsd_c_cubetype-hybrid.
              rv_icon = icon_write_file.
          ENDCASE.

        WHEN rs_c_tlogo-logsys.
          lv_lsys_type = iv_tlogo_sub.

          rv_icon = cl_rsar_srctype=>get_icon( lv_lsys_type ).

        WHEN OTHERS. " Other BW objects
          rv_icon = cl_rso_repository=>get_tlogo_icon( iv_tlogo ).
      ENDCASE.
    ELSE.
      rv_icon = iv_icon.
    ENDIF.

    " Set fallback icon
    IF rv_icon IS INITIAL.
      rv_icon = cl_rso_repository=>get_tlogo_icon( iv_tlogo ).
    ELSEIF rv_icon = icon_content_object.
      rv_icon = cl_rso_repository=>get_tlogo_icon( get_tlogo_from_tlogo_d( iv_tlogo ) ).
    ENDIF.

    IF rv_icon IS INITIAL.
      rv_icon = icon_dummy.
    ENDIF.

  ENDMETHOD.


  METHOD get_tlogo_sub.

    DATA:
      lv_oltpsource TYPE rsaot_oltpsource,
      lv_rlogsys    TYPE rsaot_logsys,
      ls_oltpsource TYPE rsaot_s_osource.

    CASE iv_tlogo.
      WHEN rs_c_tlogo-infoobject.
        SELECT SINGLE iobjtp FROM rsdiobj INTO rv_tlogo_sub
          WHERE iobjnm = iv_object AND objvers = rs_c_objvers-active.
      WHEN rs_c_tlogo-d_infoobject.
        SELECT SINGLE iobjtp FROM rsdiobj INTO rv_tlogo_sub
          WHERE iobjnm = iv_object AND objvers = rs_c_objvers-delivery.
      WHEN rs_c_tlogo-infocube.
        SELECT SINGLE cubetype FROM rsdcube INTO rv_tlogo_sub
          WHERE infocube = iv_object AND objvers = rs_c_objvers-active.
      WHEN rs_c_tlogo-d_infocube.
        SELECT SINGLE cubetype FROM rsdcube INTO rv_tlogo_sub
          WHERE infocube = iv_object AND objvers = rs_c_objvers-delivery.
      WHEN 'OSOA' OR 'OSOD'.
        lv_oltpsource = iv_object(30).
        lv_rlogsys    = iv_object+30(*).

        CALL FUNCTION 'RSA1_SINGLE_OLTPSOURCE_GET'
          EXPORTING
            i_oltpsource   = lv_oltpsource
            i_objvers      = rs_c_objvers-active
            i_rlogsys      = lv_rlogsys
          IMPORTING
            e_s_oltpsource = ls_oltpsource
          EXCEPTIONS
            no_authority   = 1
            not_exist      = 2
            inconsistent   = 3
            OTHERS         = 4.
        IF sy-subrc = 0.
          rv_tlogo_sub = ls_oltpsource-type.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD get_tlogo_text.

    DATA:
      ls_tlogo_text TYPE ty_tlogo_text.

    READ TABLE gt_tlogo_text INTO ls_tlogo_text
      WITH TABLE KEY tlogo = iv_tlogo.
    IF sy-subrc <> 0 OR ls_tlogo_text-txtlg IS INITIAL.
      rv_text = 'No text'(001).
    ELSE.
      rv_text = ls_tlogo_text-txtlg.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
