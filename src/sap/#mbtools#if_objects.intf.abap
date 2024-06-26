INTERFACE /mbtools/if_objects
  PUBLIC .

************************************************************************
* MBT Objects
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************

* Note: All constants must by unique and some constants don't exist in lower releases
* BW-WHM
  CONSTANTS:
    c_multiprov      TYPE lvc_fname VALUE rs_c_tlogo-multiprovider,
    c_infocube       TYPE lvc_fname VALUE rs_c_tlogo-infocube,
    c_ods            TYPE lvc_fname VALUE rs_c_tlogo-ods_object,
    c_infoset        TYPE lvc_fname VALUE rs_c_tlogo-infoset,
    c_infoobject     TYPE lvc_fname VALUE rs_c_tlogo-infoobject,
    c_hierarchy      TYPE lvc_fname VALUE 'HIER', "rs_c_tlogo-hierarchy,
    c_dimension      TYPE lvc_fname VALUE 'DIME',
    c_hybridprovider TYPE lvc_fname VALUE 'HYPR', "rs_c_tlogo-hybridprovider,
    c_lpo            TYPE lvc_fname VALUE 'LPOA', "rs_c_tlogo-log_partitioned_obj,
    c_ctrt           TYPE lvc_fname VALUE rs_c_tlogo-cur_trans_type,
    c_uomt           TYPE lvc_fname VALUE rs_c_tlogo-uom_trans_type,
    c_thjt           TYPE lvc_fname VALUE rs_c_tlogo-tmphierjoin_type.

* BW-BEX
  CONSTANTS:
    c_element    TYPE lvc_fname VALUE rs_c_tlogo-element,
    c_query      TYPE lvc_fname VALUE rzd1_c_deftp-report,
    c_variable   TYPE lvc_fname VALUE rzd1_c_deftp-variable,
    c_sel_object TYPE lvc_fname VALUE rzd1_c_deftp-sel_object.

* BW-PLA-IP
  CONSTANTS:
    c_plan_provider     TYPE lvc_fname VALUE 'BASI', "rs_c_tlogo-basic_infocube,
    c_char_relationship TYPE lvc_fname VALUE rs_c_tlogo-characteristic_relationship,
    c_data_slice        TYPE lvc_fname VALUE rs_c_tlogo-data_slices,
    c_aggrlevel         TYPE lvc_fname VALUE rs_c_tlogo-aggrlevel,
    c_plan_service_type TYPE lvc_fname VALUE rs_c_tlogo-planning_service_type,
    c_plan_service      TYPE lvc_fname VALUE rs_c_tlogo-planning_service,
    c_plan_sequence     TYPE lvc_fname VALUE rs_c_tlogo-planning_sequence.

* BW-PLA-BPS
  CONSTANTS:
    c_bps_profile       TYPE lvc_fname VALUE 'BPS_PROFILE',
    c_bps_area          TYPE lvc_fname VALUE 'BPS_AREA',
    c_bps_level         TYPE lvc_fname VALUE 'BPS_LEVEL',
    c_bps_package       TYPE lvc_fname VALUE 'BPS_PACKAGE',
    c_bps_method        TYPE lvc_fname VALUE 'BPS_METHOD',
    c_bps_method_mp     TYPE lvc_fname VALUE 'BPS_METHOD_MP',
    c_bps_method_doc    TYPE lvc_fname VALUE 'BPS_METHOD_DOC',
    c_bps_method_bf     TYPE lvc_fname VALUE 'BPS_METHOD_BF',
    c_bps_layout        TYPE lvc_fname VALUE 'BPS_LAYOUT',
    c_bps_function      TYPE lvc_fname VALUE 'BPS_FUNCTION',
    c_bps_param         TYPE lvc_fname VALUE 'BPS_PARAM',
    c_bps_gps           TYPE lvc_fname VALUE 'BPS_GPS',
    c_bps_web_interface TYPE lvc_fname VALUE 'BPS_WIF',
    c_bps_folder        TYPE lvc_fname VALUE 'BPS_FOLDER'.

* Basis
  CONSTANTS:
    BEGIN OF c_basis,
      activity         TYPE lvc_fname VALUE 'ZACT',
      client           TYPE lvc_fname VALUE 'ZCLI',
      devclass         TYPE lvc_fname VALUE 'ZPCK',
      owner            TYPE lvc_fname VALUE 'ZOWN',
      project          TYPE lvc_fname VALUE 'ZPRJ',
      request          TYPE lvc_fname VALUE 'ZREQ',
      system           TYPE lvc_fname VALUE 'ZSYS',
      target_group     TYPE lvc_fname VALUE 'ZGRP',
      translayer       TYPE lvc_fname VALUE 'ZLAY',
      transport_target TYPE lvc_fname VALUE 'ZTGT',
      user             TYPE lvc_fname VALUE 'ZUSR',
    END OF c_basis.

* Others
  CONSTANTS:
    c_user_id          TYPE lvc_fname VALUE rs_c_tlogo-user,
    c_role             TYPE lvc_fname VALUE rs_c_tlogo-activity_group,
    c_icon             TYPE lvc_fname VALUE 'ICON',
    c_abap_function    TYPE lvc_fname VALUE 'FUNC',
    c_abap_program     TYPE lvc_fname VALUE 'PROG',
    c_abap_class       TYPE lvc_fname VALUE 'CLAS',
    c_abap_interface   TYPE lvc_fname VALUE 'INTF',
    c_data_element     TYPE lvc_fname VALUE rs_c_obj_dtel,
    c_table            TYPE lvc_fname VALUE rs_c_obj_tabl,
    c_number_range     TYPE lvc_fname VALUE rs_c_obj_nrob,
    c_enhancement      TYPE lvc_fname VALUE 'SMOD',
    c_enhancement_proj TYPE lvc_fname VALUE 'CMOD',
    c_datasource       TYPE lvc_fname VALUE 'OSOA',
    c_appl_comp_hier   TYPE lvc_fname VALUE 'DSAA'.

ENDINTERFACE.
