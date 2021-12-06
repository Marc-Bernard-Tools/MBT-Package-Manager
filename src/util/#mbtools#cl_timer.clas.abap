CLASS /mbtools/cl_timer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS start.
    METHODS end
      IMPORTING
        !iv_title        TYPE string OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_timer TYPE timestampl.

ENDCLASS.



CLASS /mbtools/cl_timer IMPLEMENTATION.


  METHOD end.

    DATA:
      lv_timestamp TYPE timestampl,
      lv_runtime   TYPE timestampl,
      lv_sec       TYPE p LENGTH 10 DECIMALS 2.

    GET TIME STAMP FIELD lv_timestamp.

    TRY.
        lv_runtime = cl_abap_tstmp=>subtract(
          tstmp1 = lv_timestamp
          tstmp2 = mv_timer ).
      CATCH cx_parameter_invalid.
        rv_result = 'Error getting runtime measurement'.
        RETURN.
    ENDTRY.

    lv_sec = lv_runtime. " round to 2 decimal places

    IF iv_title IS INITIAL.
      rv_result = |Runtime: { lv_sec } seconds|.
    ELSE.
      rv_result = |{ iv_title } { lv_sec } seconds|.
    ENDIF.

  ENDMETHOD.


  METHOD start.

    GET TIME STAMP FIELD mv_timer.

  ENDMETHOD.
ENDCLASS.
