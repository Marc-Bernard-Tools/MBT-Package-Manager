*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/MBT_TEMPLATE_6_START
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  DATA:
    lv_enum_zip          TYPE i,
    lv_name              TYPE char255,
    lv_package           TYPE devclass,
    lv_enum_package      TYPE i,
    lv_enum_transport    TYPE i,
    lv_enum_folder_logic TYPE i.

  CASE abap_true.
    WHEN p_zip_f.
      lv_enum_zip = zcl_abapinst_installer=>ty_enum_zip-local.
      lv_name     = p_file_f.
    WHEN p_zip_i.
      lv_enum_zip = zcl_abapinst_installer=>ty_enum_zip-internet.
      lv_name     = p_file_i.
    WHEN p_zip_s.
      lv_enum_zip = zcl_abapinst_installer=>ty_enum_zip-server.
      lv_name     = p_file_s.
  ENDCASE.

  CASE abap_true.
    WHEN p_sap_d.
      lv_enum_package = zcl_abapinst_installer=>ty_enum_package-default.
    WHEN p_sap_l.
      lv_enum_package = zcl_abapinst_installer=>ty_enum_package-local.
      lv_package      = p_pack_l.
    WHEN p_sap_t.
      lv_enum_package = zcl_abapinst_installer=>ty_enum_package-transportable.
      lv_package      = p_pack_t.
  ENDCASE.

  CASE abap_true.
    WHEN p_tsp_n.
      lv_enum_transport = zcl_abapinst_installer=>ty_enum_transport-prompt.
    WHEN p_tsp_e.
      lv_enum_transport = zcl_abapinst_installer=>ty_enum_transport-existing.
  ENDCASE.

  IF p_conn_o = abap_false.
    CLEAR: p_conn_u, p_conn_p.
  ENDIF.

  IF p_prox_o = abap_false.
    CLEAR: p_prox_h, p_prox_s, p_prox_u, p_prox_p.
  ENDIF.

  CASE abap_true.
    WHEN p_fold_d.
      lv_enum_folder_logic = zcl_abapinst_installer=>ty_enum_folder_logic-default.
    WHEN p_fold_p.
      lv_enum_folder_logic = zcl_abapinst_installer=>ty_enum_folder_logic-prefix.
    WHEN p_fold_f.
      lv_enum_folder_logic = zcl_abapinst_installer=>ty_enum_folder_logic-full.
  ENDCASE.

  TRY.
      zcl_abapinst_installer=>install(
        iv_enum_zip          = lv_enum_zip
        iv_name              = lv_name
        iv_enum_package      = lv_enum_package
        iv_package           = lv_package
        iv_dlvunit           = p_soft_t
        iv_devlayer          = p_layr_t
        iv_enum_transport    = lv_enum_transport
        iv_transport         = p_req_e
        iv_user              = p_conn_u
        iv_password          = p_conn_p
        iv_proxy_host        = p_prox_h
        iv_proxy_service     = p_prox_s
        iv_proxy_user        = p_prox_u
        iv_proxy_password    = p_prox_p
        iv_enum_folder_logic = lv_enum_folder_logic ).

    CATCH zcx_abapinst_exception INTO gx_error.
      MESSAGE gx_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
