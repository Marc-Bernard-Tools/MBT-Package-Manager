REPORT /mbtools/mbt_template.

************************************************************************
* MBT Installer
*
* This program installs and uninstalls any Marc Bernard Tool
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

* @@require devc $ajson
* @@require devc $abapinst_abapgit
* @@require devc $abapinst_dev

* @@require devc $abapgit_objects_enh
* @@require devc $abapgit_objects_oo
* @@require devc $abapgit_objects_texts

* @@require clas zcl_abapgit_objects
* @@require clas zcl_abapgit_objects_bridge
* @@require clas zcl_abapgit_objects_program
* @@require clas zcl_abapgit_objects_super
* @@require intf zif_abapgit_comparator
* @@require intf zif_abapgit_object
* @@require intf zif_abapgit_objects

* @@require clas zcl_abapgit_dependencies
* @@require clas zcl_abapgit_file_status
* @@require clas zcl_abapgit_folder_logic
* @@require clas zcl_abapgit_objects_activation
* @@require clas zcl_abapgit_objects_files
* @@require clas zcl_abapgit_skip_objects
* @@require clas zcl_abapgit_tadir
* @@require intf zif_abapgit_tadir

* @@require clas zcl_abapgit_object_acid
* @@require clas zcl_abapgit_object_avar
* @@require clas zcl_abapgit_object_clas
* @@require clas zcl_abapgit_object_devc
* @@require clas zcl_abapgit_object_doma
* @@require clas zcl_abapgit_object_dtel
* @@require clas zcl_abapgit_object_enh*
* @@require intf zif_abapgit_object_enh*
* @@require clas zcl_abapgit_object_enqu
* @@require clas zcl_abapgit_object_fugr
* @@require clas zcl_abapgit_object_intf
* @@require clas zcl_abapgit_object_msag
* @@require clas zcl_abapgit_object_para
* @@require clas zcl_abapgit_object_prog
* @@require clas zcl_abapgit_object_tabl*
* @@require clas zcl_abapgit_object_tobj
* @@require clas zcl_abapgit_object_tran
* @@require clas zcl_abapgit_object_ttyp
* @@require clas zcl_abapgit_object_w3*

* @@require clas zcl_abapgit_adt_link
* @@require clas zcl_abapgit_convert
* @@require clas zcl_abapgit_default_transport
* @@require intf zif_abapgit_definitions
* @@require clas zcl_abapgit_dot_abapgit
* @@require intf zif_abapgit_dot_abapgit
* @@require clas zcl_abapgit_environment
* @@require intf zif_abapgit_environment
* @@require clas zcx_abapgit_exception
* @@require clas zcl_abapgit_free_sel_dialog
* @@require clas zcl_abapgit_gui_functions
* @@require intf zif_abapgit_gui_functions
* @@require clas zcl_abapgit_hash
* @@require clas zcl_abapgit_language
* @@require clas zcl_abapgit_log
* @@require intf zif_abapgit_log
* @@require clas zcx_abapgit_not_found
* @@require clas zcl_abapgit_path
* @@require clas zcl_abapgit_progress
* @@require intf zif_abapgit_progress
* @@require clas zcl_abapgit_sap_package
* @@require intf zif_abapgit_sap_package
* @@require clas zcl_abapgit_url
* @@require clas zcl_abapgit_version
* @@require intf zif_abapgit_version
* @@require clas zcl_abapgit_xml*
* @@require intf zif_abapgit_xml*
