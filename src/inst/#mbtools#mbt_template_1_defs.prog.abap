*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/MBT_TEMPLATE_1_DEFS
*&---------------------------------------------------------------------*

TABLES:
  sscrfields.

DATA:
  gv_options  TYPE abap_bool,
  gx_error    TYPE REF TO zcx_abapinst_exception,
  go_textpool TYPE REF TO zcl_abapinst_textpool,
  gs_inst     TYPE zif_abapinst_definitions=>ty_inst,
  gt_banner   TYPE zif_abapinst_definitions=>ty_base_tab.
