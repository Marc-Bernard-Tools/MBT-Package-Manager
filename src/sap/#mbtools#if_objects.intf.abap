INTERFACE /mbtools/if_objects
  PUBLIC .


* Note: All constants must by unique and some constants don't exist in lower releases
* BW-WHM
  CONSTANTS c_multiprov TYPE lvc_fname VALUE rs_c_tlogo-multiprovider ##NO_TEXT.
  CONSTANTS c_infocube TYPE lvc_fname VALUE rs_c_tlogo-infocube ##NO_TEXT.
  CONSTANTS c_ods TYPE lvc_fname VALUE rs_c_tlogo-ods_object ##NO_TEXT.
  CONSTANTS c_infoset TYPE lvc_fname VALUE rs_c_tlogo-infoset ##NO_TEXT.
  CONSTANTS c_infoobject TYPE lvc_fname VALUE rs_c_tlogo-infoobject ##NO_TEXT.
  CONSTANTS c_hierarchy TYPE lvc_fname VALUE 'HIER' ##NO_TEXT. "rs_c_tlogo-hierarchy,
  CONSTANTS c_dimension TYPE lvc_fname VALUE 'DIME' ##NO_TEXT.
  CONSTANTS c_hybridprovider TYPE lvc_fname VALUE 'HYPR' ##NO_TEXT. "rs_c_tlogo-hybridprovider,
  CONSTANTS c_lpo TYPE lvc_fname VALUE 'LPOA' ##NO_TEXT. "rs_c_tlogo-log_partitioned_obj,
  CONSTANTS c_ctrt TYPE lvc_fname VALUE rs_c_tlogo-cur_trans_type ##NO_TEXT.
  CONSTANTS c_uomt TYPE lvc_fname VALUE rs_c_tlogo-uom_trans_type ##NO_TEXT.
  CONSTANTS c_thjt TYPE lvc_fname VALUE rs_c_tlogo-tmphierjoin_type ##NO_TEXT.
* BW-BEX
  CONSTANTS c_element TYPE lvc_fname VALUE rs_c_tlogo-element ##NO_TEXT.
  CONSTANTS c_query TYPE lvc_fname VALUE rzd1_c_deftp-report ##NO_TEXT.
  CONSTANTS c_variable TYPE lvc_fname VALUE rzd1_c_deftp-variable ##NO_TEXT.
  CONSTANTS c_sel_object TYPE lvc_fname VALUE rzd1_c_deftp-sel_object ##NO_TEXT.
* BW-PLA-IP
  CONSTANTS c_plan_provider TYPE lvc_fname VALUE 'BASI' ##NO_TEXT. "rs_c_tlogo-basic_infocube,
  CONSTANTS c_char_relationship TYPE lvc_fname VALUE rs_c_tlogo-characteristic_relationship ##NO_TEXT.
  CONSTANTS c_data_slice TYPE lvc_fname VALUE rs_c_tlogo-data_slices ##NO_TEXT.
  CONSTANTS c_aggrlevel TYPE lvc_fname VALUE rs_c_tlogo-aggrlevel ##NO_TEXT.
  CONSTANTS c_plan_service_type TYPE lvc_fname VALUE rs_c_tlogo-planning_service_type ##NO_TEXT.
  CONSTANTS c_plan_service TYPE lvc_fname VALUE rs_c_tlogo-planning_service ##NO_TEXT.
  CONSTANTS c_plan_sequence TYPE lvc_fname VALUE rs_c_tlogo-planning_sequence ##NO_TEXT.
* BW-PLA-BPS
  CONSTANTS c_bps_profile TYPE lvc_fname VALUE 'BPS_PROFILE' ##NO_TEXT.
  CONSTANTS c_bps_area TYPE lvc_fname VALUE 'BPS_AREA' ##NO_TEXT.
  CONSTANTS c_bps_level TYPE lvc_fname VALUE 'BPS_LEVEL' ##NO_TEXT.
  CONSTANTS c_bps_package TYPE lvc_fname VALUE 'BPS_PACKAGE' ##NO_TEXT.
  CONSTANTS c_bps_method TYPE lvc_fname VALUE 'BPS_METHOD' ##NO_TEXT.
  CONSTANTS c_bps_method_mp TYPE lvc_fname VALUE 'BPS_METHOD_MP' ##NO_TEXT.
  CONSTANTS c_bps_method_doc TYPE lvc_fname VALUE 'BPS_METHOD_DOC' ##NO_TEXT.
  CONSTANTS c_bps_method_bf TYPE lvc_fname VALUE 'BPS_METHOD_BF' ##NO_TEXT.
  CONSTANTS c_bps_layout TYPE lvc_fname VALUE 'BPS_LAYOUT' ##NO_TEXT.
  CONSTANTS c_bps_function TYPE lvc_fname VALUE 'BPS_FUNCTION' ##NO_TEXT.
  CONSTANTS c_bps_param TYPE lvc_fname VALUE 'BPS_PARAM' ##NO_TEXT.
  CONSTANTS c_bps_gps TYPE lvc_fname VALUE 'BPS_GPS' ##NO_TEXT.
  CONSTANTS c_bps_web_interface TYPE lvc_fname VALUE 'BPS_WIF' ##NO_TEXT.
  CONSTANTS c_bps_folder TYPE lvc_fname VALUE 'BPS_FOLDER' ##NO_TEXT.
* Others
  CONSTANTS c_user_id TYPE lvc_fname VALUE rs_c_tlogo-user ##NO_TEXT.
  CONSTANTS c_role TYPE lvc_fname VALUE rs_c_tlogo-activity_group ##NO_TEXT.
  CONSTANTS c_icon TYPE lvc_fname VALUE 'ICON' ##NO_TEXT.
  CONSTANTS c_abap_function TYPE lvc_fname VALUE 'FUNC' ##NO_TEXT.
  CONSTANTS c_abap_program TYPE lvc_fname VALUE 'PROG' ##NO_TEXT.
  CONSTANTS c_abap_class TYPE lvc_fname VALUE 'CLAS' ##NO_TEXT.
  CONSTANTS c_abap_interface TYPE lvc_fname VALUE 'INTF' ##NO_TEXT.
  CONSTANTS c_data_element TYPE lvc_fname VALUE rs_c_obj_dtel ##NO_TEXT.
  CONSTANTS c_table TYPE lvc_fname VALUE rs_c_obj_tabl ##NO_TEXT.
  CONSTANTS c_number_range TYPE lvc_fname VALUE rs_c_obj_nrob ##NO_TEXT.
  CONSTANTS c_enhancement TYPE lvc_fname VALUE 'SMOD' ##NO_TEXT.
  CONSTANTS c_enhancement_proj TYPE lvc_fname VALUE 'CMOD' ##NO_TEXT.
  CONSTANTS c_datasource TYPE lvc_fname VALUE 'OSOA' ##NO_TEXT.
  CONSTANTS c_appl_comp_hier TYPE lvc_fname VALUE 'DSAA' ##NO_TEXT.
ENDINTERFACE.
