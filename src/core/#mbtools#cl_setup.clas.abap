CLASS /mbtools/cl_setup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS run
      IMPORTING
        iv_force TYPE abap_bool DEFAULT abap_false
      RAISING
        /mbtools/cx_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA mv_force TYPE abap_bool.

    CLASS-METHODS _application_log .
    CLASS-METHODS _certificates .
    CLASS-METHODS _rfc_destination
      RAISING
        /mbtools/cx_exception.
ENDCLASS.



CLASS /mbtools/cl_setup IMPLEMENTATION.


  METHOD run.

    mv_force = iv_force.

    _application_log( ).

    _certificates( ).

    _rfc_destination( ).

  ENDMETHOD.


  METHOD _application_log.

    DATA:
      ls_balobj  TYPE balobj,
      ls_balobjt TYPE balobjt,
      ls_balsub  TYPE balsub,
      ls_balsubt TYPE balsubt.

    SELECT SINGLE * FROM balobj INTO ls_balobj WHERE object = /mbtools/if_definitions=>c_namespace.
    IF sy-subrc = 0.
      IF mv_force = abap_true.
        DELETE FROM balobj  WHERE object = /mbtools/if_definitions=>c_namespace.
        DELETE FROM balobjt WHERE object = /mbtools/if_definitions=>c_namespace.
        DELETE FROM balsub  WHERE object = /mbtools/if_definitions=>c_namespace.
        DELETE FROM balsubt WHERE object = /mbtools/if_definitions=>c_namespace.
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    ls_balobj-object  = /mbtools/if_definitions=>c_namespace.
    INSERT balobj FROM ls_balobj.

    ls_balobjt-spras  = sy-langu.
    ls_balobjt-object = /mbtools/if_definitions=>c_namespace.
    ls_balobjt-objtxt = /mbtools/if_definitions=>c_mbt.
    INSERT balobjt FROM ls_balobjt.

    ls_balsub-object    = /mbtools/if_definitions=>c_namespace.
    ls_balsub-subobject = 'EDD'.
    INSERT balsub FROM ls_balsub.
    ls_balsub-subobject = 'INIT'.
    INSERT balsub FROM ls_balsub.
    ls_balsub-subobject = 'LOG'.
    INSERT balsub FROM ls_balsub.

    ls_balsubt-spras     = sy-langu.
    ls_balsubt-object    = /mbtools/if_definitions=>c_namespace.
    ls_balsubt-subobject = 'EDD'.
    ls_balsubt-subobjtxt = 'Log for EDD API'(002).
    INSERT balsubt FROM ls_balsubt.
    ls_balsubt-subobject = 'INST'.
    ls_balsubt-subobjtxt = 'Log for MBT Installer'(003).
    INSERT balsubt FROM ls_balsubt.
    ls_balsubt-subobject = 'LOG'.
    ls_balsubt-subobjtxt = 'General Log'(004).
    INSERT balsubt FROM ls_balsubt.

  ENDMETHOD.


  METHOD _certificates.
  ENDMETHOD.


  METHOD _rfc_destination.

    DATA:
      lo_dest_factory TYPE REF TO cl_dest_factory,
      lx_dest_api     TYPE REF TO cx_dest_api,
      lv_rfc_exists   TYPE abap_bool,
      lv_action       TYPE c LENGTH 1.

    TRY.
        CREATE OBJECT lo_dest_factory.
        lv_rfc_exists = lo_dest_factory->exists( /mbtools/if_definitions=>c_rfcdest ).
      CATCH cx_dest_api INTO lx_dest_api.
        /mbtools/cx_exception=>raise( lx_dest_api->get_text( ) ).
    ENDTRY.

    IF lv_rfc_exists = abap_true.
      IF mv_force = abap_true.
        lv_action = 'D'. " delete existing
      ELSE.
        RETURN.
      ENDIF.
    ELSE.
      lv_action = 'I'. " create new https
    ENDIF.

    IF lv_action = 'D'.
      CALL FUNCTION 'RFC_MODIFY_HTTP_DEST_TO_EXT'
        EXPORTING
          destination                = /mbtools/if_definitions=>c_rfcdest
          action                     = lv_action
          authority_check            = 'X'
        EXCEPTIONS
          authority_not_available    = 1
          destination_already_exist  = 2
          destination_not_exist      = 3
          destination_enqueue_reject = 4
          information_failure        = 5
          trfc_entry_invalid         = 6
          internal_failure           = 7
          snc_information_failure    = 8
          snc_internal_failure       = 9
          destination_is_locked      = 10
          invalid_parameter          = 11
          OTHERS                     = 12.
      IF sy-subrc <> 0.
        /mbtools/cx_exception=>raise_t100( ).
      ELSE.
        lv_action = 'I'.
      ENDIF.
    ENDIF.

    IF lv_action = 'I'.
      " Create HTTPS destination
      CALL FUNCTION 'RFC_MODIFY_HTTP_DEST_TO_EXT'
        EXPORTING
          destination                = /mbtools/if_definitions=>c_rfcdest
          action                     = lv_action
          authority_check            = 'X'
          servicenr                  = '443'
          server                     = /mbtools/if_definitions=>c_domain
          path_prefix                = '/info/index.html'
          logon_method               = 'A'
          description                = /mbtools/if_definitions=>c_mbt
          sslapplic                  = 'ANONYM'
          ssl                        = abap_true
        EXCEPTIONS
          authority_not_available    = 1
          destination_already_exist  = 2
          destination_not_exist      = 3
          destination_enqueue_reject = 4
          information_failure        = 5
          trfc_entry_invalid         = 6
          internal_failure           = 7
          snc_information_failure    = 8
          snc_internal_failure       = 9
          destination_is_locked      = 10
          invalid_parameter          = 11
          OTHERS                     = 12.
      IF sy-subrc <> 0.
        /mbtools/cx_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
