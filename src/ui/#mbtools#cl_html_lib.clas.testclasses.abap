*"* use this source file for your ABAP unit test classes
CLASS ltcl_normalize_program_name DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      class FOR TESTING RAISING cx_static_check,
      program FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS /mbtools/cl_html_lib DEFINITION LOCAL FRIENDS ltcl_normalize_program_name.

CLASS ltcl_normalize_program_name IMPLEMENTATION.

  METHOD class.

    cl_abap_unit_assert=>assert_equals(
      act = /mbtools/cl_html_lib=>normalize_program_name( '/MBTOOLS/CL_FRONTEND_SERVICES=CP' )
      exp = `/MBTOOLS/CL_FRONTEND_SERVICES` ).

  ENDMETHOD.


  METHOD program.

    cl_abap_unit_assert=>assert_equals(
      act = /mbtools/cl_html_lib=>normalize_program_name( 'ZABAPGIT_FULL' )
      exp = `ZABAPGIT_FULL` ).

    cl_abap_unit_assert=>assert_equals(
      act = /mbtools/cl_html_lib=>normalize_program_name( 'ZSOME_PROG_ENDING_WITH_CP' )
      exp = `ZSOME_PROG_ENDING_WITH_CP` ).

  ENDMETHOD.

ENDCLASS.
