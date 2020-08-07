CLASS ltcl_datetime DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.

    DATA: mv_result TYPE string.

    METHODS test_date_1 FOR TESTING.
    METHODS test_date_2 FOR TESTING.
    METHODS test_date_3 FOR TESTING.
    METHODS test_time_1 FOR TESTING.
    METHODS test_time_2 FOR TESTING.

ENDCLASS.

CLASS ltcl_datetime IMPLEMENTATION.

  METHOD test_date_1.

    mv_result = /mbtools/cl_datetime=>human_date_diff( iv_from = '20200101'
                                                       iv_to   = '20200102' ).

    cl_abap_unit_assert=>assert_equals(
      exp = '1 day'
      act =  mv_result ).

    mv_result = /mbtools/cl_datetime=>human_date_diff( iv_from = '20200101'
                                                       iv_to   = '20200107' ).

    cl_abap_unit_assert=>assert_equals(
      exp = '6 days'
      act =  mv_result ).

  ENDMETHOD.

  METHOD test_date_2.

    mv_result = /mbtools/cl_datetime=>human_date_diff( iv_from = '20200101'
                                                       iv_to   = '20200109' ).

    cl_abap_unit_assert=>assert_equals(
      exp = '1 week'
      act =  mv_result ).

    mv_result = /mbtools/cl_datetime=>human_date_diff( iv_from = '20200101'
                                                       iv_to   = '20200130' ).

    cl_abap_unit_assert=>assert_equals(
      exp = '4 weeks'
      act =  mv_result ).

  ENDMETHOD.

  METHOD test_date_3.

    mv_result = /mbtools/cl_datetime=>human_date_diff( iv_from = '20200101'
                                                       iv_to   = '20201126' ).

    cl_abap_unit_assert=>assert_equals(
      exp = '11 months'
      act =  mv_result ).

    mv_result = /mbtools/cl_datetime=>human_date_diff( iv_from = '20200101'
                                                       iv_to   = '20210101' ).

    cl_abap_unit_assert=>assert_equals(
      exp = '1 year'
      act =  mv_result ).

  ENDMETHOD.

  METHOD test_time_1.

    mv_result = /mbtools/cl_datetime=>human_time_diff( iv_from = '20200101120000'
                                                       iv_to   = '20200101120001' ).

    cl_abap_unit_assert=>assert_equals(
      exp = '1 second'
      act =  mv_result ).

    mv_result = /mbtools/cl_datetime=>human_time_diff( iv_from = '20200101120000'
                                                       iv_to   = '20200101120201' ).

    cl_abap_unit_assert=>assert_equals(
      exp = '2 minutes'
      act =  mv_result ).

    mv_result = /mbtools/cl_datetime=>human_time_diff( iv_from = '20200101120000'
                                                       iv_to   = '20200101130000' ).

    cl_abap_unit_assert=>assert_equals(
      exp = '1 hour'
      act =  mv_result ).

  ENDMETHOD.

  METHOD test_time_2.

    mv_result = /mbtools/cl_datetime=>human_time_diff( iv_from = '20200101120000'
                                                       iv_to   = '20200101133000' ).

    cl_abap_unit_assert=>assert_equals(
      exp = '2 hours'
      act =  mv_result ).

    mv_result = /mbtools/cl_datetime=>human_time_diff( iv_from = '20200101120000'
                                                       iv_to   = '20200103000000' ).

    cl_abap_unit_assert=>assert_equals(
      exp = '2 days'
      act =  mv_result ).

  ENDMETHOD.
ENDCLASS.
