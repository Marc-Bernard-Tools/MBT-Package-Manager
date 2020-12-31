*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/MBT_TEMPLATE_5_AT
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN.

  zcl_abapinst_screen=>modify(
    iv_options = gv_options
    iv_zip_i   = p_zip_i
    iv_zip_f   = p_zip_f
    iv_zip_s   = p_zip_s
    iv_sap_l   = p_sap_l
    iv_sap_t   = p_sap_t
    iv_tsp_e   = p_tsp_e
    iv_conn_o  = p_conn_o
    iv_prox_o  = p_prox_o
    iv_mbt     = abap_true ).

  CHECK sy-dynnr <> '1000'.

  CASE sscrfields-ucomm.

*   Function Keys
    WHEN 'FC01'. " Show/Hide Options
      IF gv_options = abap_false.
        sscrfields-functxt_01 = icon_collapse && 'Hide Options'.
      ELSE.
        sscrfields-functxt_01 = icon_expand && 'Show Options'.
      ENDIF.
      gv_options = boolc( gv_options = abap_false ).

    WHEN 'FC02'. " List Packages
      zcl_abapinst_screen=>banner( iv_show = abap_false ).
      TRY.
          zcl_abapinst_installer=>list( ).

        CATCH zcx_abapinst_exception INTO gx_error.
          MESSAGE gx_error TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

    WHEN 'FC03'. " Uninstall Package
      TRY.
          gs_inst = zcl_abapinst_screen=>f4_inst( ).

          IF gs_inst IS NOT INITIAL.
            zcl_abapinst_installer=>uninstall(
              iv_name = gs_inst-name
              iv_pack = gs_inst-pack ).
          ENDIF.

        CATCH zcx_abapinst_exception INTO gx_error.
          MESSAGE gx_error TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

*   About
    WHEN 'DOCU'. " Documentation
      zcl_abapinst_screen=>browser( c_url_docs ).

    WHEN 'LICE'. " License
      zcl_abapinst_screen=>browser( c_url_license ).

    WHEN 'REPO'. " Repository
      zcl_abapinst_screen=>browser( c_url_repo ).

  ENDCASE.

AT SELECTION-SCREEN OUTPUT.

  PERFORM banner.

  zcl_abapinst_screen=>banner( it_base = gt_banner ). " iv_id = c_banner_id

  zcl_abapinst_screen=>modify(
    iv_options = gv_options
    iv_zip_i   = p_zip_i
    iv_zip_f   = p_zip_f
    iv_zip_s   = p_zip_s
    iv_sap_l   = p_sap_l
    iv_sap_t   = p_sap_t
    iv_tsp_e   = p_tsp_e
    iv_conn_o  = p_conn_o
    iv_prox_o  = p_prox_o
    iv_mbt     = abap_true ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file_f.

  p_file_f = zcl_abapinst_screen=>f4_file( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_req_e.

  p_req_e = zcl_abapinst_screen=>f4_transport(
              iv_package = p_pack_t
              iv_layer   = p_layr_t ).
