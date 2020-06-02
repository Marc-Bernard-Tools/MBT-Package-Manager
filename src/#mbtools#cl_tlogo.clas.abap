************************************************************************
* /MBTOOLS/CL_TLOGO
* MBT TLOGO
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_tlogo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor .
    CLASS-METHODS get_tlogo_from_tlogo_d
      IMPORTING
        !i_tlogo_d     TYPE rstlogo
      RETURNING
        VALUE(r_tlogo) TYPE rstlogo.
    CLASS-METHODS get_tlogo_icon
      IMPORTING
        !i_tlogo      TYPE rstlogo
        !i_icon       TYPE icon_d OPTIONAL
      RETURNING
        VALUE(r_icon) TYPE icon_d.
    CLASS-METHODS get_tlogo_text
      IMPORTING
        !i_tlogo      TYPE rstlogo
      RETURNING
        VALUE(r_text) TYPE rstxtlg
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS get_object_text
      IMPORTING
        !i_tlogo      TYPE rstlogo
        !i_object     TYPE csequence
      RETURNING
        VALUE(r_text) TYPE rstxtlg
      RAISING
        /mbtools/cx_exception.
  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_tlogo_text,
        tlogo TYPE rstlogo,
        txtlg TYPE rstxtlg,
      END OF ty_tlogo_text,
      BEGIN OF ty_tlogo_cont,
        tlogo_d TYPE rstlogo,
        tlogo   TYPE rstlogo,
      END OF ty_tlogo_cont.

    CLASS-DATA:
      mr_repository TYPE REF TO cl_rso_repository,
      mt_tlogo_text TYPE HASHED TABLE OF ty_tlogo_text WITH UNIQUE KEY tlogo,
      mt_tlogo_cont TYPE HASHED TABLE OF ty_tlogo_cont WITH UNIQUE KEY tlogo_d.

ENDCLASS.



CLASS /MBTOOLS/CL_TLOGO IMPLEMENTATION.


  METHOD class_constructor.

    CALL METHOD cl_rso_repository=>get_repository
      RECEIVING
        r_r_repository = mr_repository.

    SELECT DISTINCT domvalue_l ddtext INTO TABLE mt_tlogo_text
      FROM dd07t
      WHERE domname    = 'RSTLOGO'
        AND ddlanguage = sy-langu
        AND as4local   = rs_c_objvers-active.

    SELECT DISTINCT tlogo_d tlogo INTO TABLE mt_tlogo_cont
      FROM rstlogoprop
      WHERE tlogo_d <> ''.

  ENDMETHOD.


  METHOD get_object_text.

    DATA:
      object TYPE rso_s_tlogo,
      txtsh  TYPE rstxtsh,
      txtlg  TYPE rstxtlg.

    object-tlogo = i_tlogo.
    object-objnm = i_object.

    CALL METHOD mr_repository->get_properties_of_object
      EXPORTING
        i_s_object       = object
      IMPORTING
        e_txtsh          = txtsh
        e_txtlg          = txtlg
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      /mbtools/cx_exception=>raise( |Object { i_tlogo } { i_object } not found| ).
    ENDIF.

    " Return longer description
    IF txtlg IS INITIAL.
      IF txtsh IS INITIAL.
        r_text = 'No description'.
      ELSE.
        r_text = txtsh.
      ENDIF.
    ELSE.
      r_text = txtlg.
    ENDIF.

  ENDMETHOD.


  METHOD get_tlogo_from_tlogo_d.

    DATA:
      tlogo_cont TYPE ty_tlogo_cont.

    READ TABLE mt_tlogo_cont INTO tlogo_cont
      WITH TABLE KEY tlogo_d = i_tlogo_d.
    IF sy-subrc = 0.
      r_tlogo = tlogo_cont-tlogo.
    ENDIF.

  ENDMETHOD.


  METHOD get_tlogo_icon.

    DATA:
      tlogo_cont TYPE ty_tlogo_cont.

    " Get icon
    IF i_icon IS INITIAL.
      CASE i_tlogo.
        WHEN 'DSAA' OR 'DSAD'. " Application component hierarchy
          CALL METHOD cl_rso_repository=>get_tlogo_icon
            EXPORTING
              i_tlogo = rs_c_tlogo-application
            RECEIVING
              r_icon  = r_icon.

        WHEN 'OSOA' OR 'OSOD'. " DataSource
          CALL METHOD cl_rso_repository=>get_tlogo_icon
            EXPORTING
              i_tlogo = rs_c_tlogo-datasource
            RECEIVING
              r_icon  = r_icon.

        WHEN OTHERS. " Other BW objects
          CALL METHOD cl_rso_repository=>get_tlogo_icon
            EXPORTING
              i_tlogo = i_tlogo
            RECEIVING
              r_icon  = r_icon.
      ENDCASE.
    ELSE.
      r_icon = i_icon.
    ENDIF.

    " Set fallback icon
    IF r_icon IS INITIAL.
      r_icon = cl_rso_repository=>get_tlogo_icon( i_tlogo ).
    ELSEIF r_icon = icon_content_object.
      r_icon = cl_rso_repository=>get_tlogo_icon( i_tlogo = get_tlogo_from_tlogo_d( i_tlogo ) ).
    ENDIF.

    IF r_icon IS INITIAL.
      r_icon = icon_dummy.
    ENDIF.

  ENDMETHOD.


  METHOD get_tlogo_text.

    DATA tlogo_text TYPE ty_tlogo_text.

    READ TABLE mt_tlogo_text INTO tlogo_text
      WITH TABLE KEY tlogo = i_tlogo.
    IF sy-subrc <> 0.
      /mbtools/cx_exception=>raise( |Object type { i_tlogo } not found| ).
    ENDIF.

    IF tlogo_text-txtlg IS INITIAL.
      r_text = 'No description'.
    ELSE.
      r_text = tlogo_text-txtlg.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
