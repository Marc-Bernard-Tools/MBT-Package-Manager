************************************************************************
* /MBTOOLS/BC_REGISTRY_EXAMPLE
* MBT Registry Browser
*
* Example program to demonstrate the usage of the API for the registry.
* Author: Martin Ceronio (2015), http://ceronio.net
* Released under MIT License: https://opensource.org/licenses/MIT
*
* Last update: 2020-02-07
************************************************************************
REPORT /mbtools/bc_registry_example.

DATA: reg_root TYPE REF TO /mbtools/cl_registry.
DATA: reg_entry TYPE REF TO /mbtools/cl_registry.
DATA: lv_customer TYPE kunnr.
DATA: lv_run_date TYPE d.
DATA: lv_timestamp TYPE timestamp.

START-OF-SELECTION.

* Get the root entry of the registry
  reg_root = /mbtools/cl_registry=>get_root( ).

* If we want to ensure, on startup, that a certain entry exists, we
* could do the following (e.g. in LOAD-OF-PROGRAM):
  reg_root->create_by_path( 'Sales/Enhancements/Process_XYZ' ).

* Retrieval of a specific entry. If we did not have the above line,
* we would have to check that the result of each call to GET_SUBENTRY( )
* to ensure it is bound.
  reg_entry = reg_root->get_subentry( 'Sales' )->get_subentry( 'Enhancements' )->get_subentry( 'Process_XYZ' ).

* Getting a specific value from the entry:
  lv_customer = reg_entry->get_value( 'ProcessCustomer' ).

* Writing values to the entry:
  lv_run_date = sy-datum.
  reg_entry->set_value( key = 'LastRunDate' value = lv_run_date ).
  GET TIME STAMP FIELD lv_timestamp.
  reg_entry->set_value( key = 'LastRunDateTime' value = lv_timestamp ).

* Saving the entry
  reg_entry->save( ).

  BREAK-POINT.

* And remove it again...
  reg_root->remove_subentry( 'Sales' ).
