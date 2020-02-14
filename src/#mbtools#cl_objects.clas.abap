************************************************************************
* /MBTOOLS/CL_ENHANCEMENTS
* MBT Enhancements
*
* (c) MBT 2020 https://marcbernardtools.com/
* Last update: 2020-01-07
************************************************************************
CLASS /mbtools/cl_objects DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NO_TEXT.

    TYPES:
      ty_object_texts TYPE STANDARD TABLE OF ko100 WITH DEFAULT KEY .

    CLASS-METHODS class_constructor .
    CLASS-METHODS get_object_text
      IMPORTING
        VALUE(i_object) TYPE trobjtype
      RETURNING
        VALUE(r_text)   TYPE ddtext .
    CLASS-METHODS get_object_texts
      RETURNING
        VALUE(r_object_texts) TYPE ty_object_texts .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA object_texts TYPE ty_object_texts .
ENDCLASS.



CLASS /MBTOOLS/CL_OBJECTS IMPLEMENTATION.


  METHOD class_constructor.

    DATA object_text TYPE ko100.

*   Read standard texts of object
    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = object_texts.

*   Add texts for non-transportable objects (or from previous releases)
    object_text-pgmid  = 'R3TR'.
    object_text-object = 'LSYS'.
    object_text-text   = 'Source System'.
    COLLECT object_text INTO object_texts.
    object_text-object = 'ADMS'.
    object_text-text   = 'BPC DM Selection'.
    COLLECT object_text INTO object_texts.
    object_text-object = 'DRRU'.
    object_text-text   = 'Remodeling Rule (SAP Delivery)'.
    COLLECT object_text INTO object_texts.
    object_text-object = 'CPAK'.
    object_text-text   = 'Class (ABAP Objects)'.
    COLLECT object_text INTO object_texts.
    object_text-object = 'BMED'.
    object_text-text   = '?'.
    COLLECT object_text INTO object_texts.
    object_text-object = 'SLDB'.
    object_text-text   = 'Logical Databases'.
    COLLECT object_text INTO object_texts.
    object_text-object = 'ECSC'.
    object_text-text   = 'eCATT System'.
    COLLECT object_text INTO object_texts.
    object_text-object = 'SOTL'.
    object_text-text   = 'Concept (Online Text Repository) - Long Texts'.
    COLLECT object_text INTO object_texts.
*    object_text-object = ''.
*    object_text-text   = ''.
*    COLLECT object_text INTO object_texts.

*   Add Workbench Development Objects
    SELECT type singular FROM euobjt INTO (object_text-object, object_text-text)
      WHERE spras = sy-langu.
      COLLECT object_text INTO object_texts.
    ENDSELECT.

    SORT object_texts.

  ENDMETHOD.


  METHOD get_object_text.

    DATA object_text TYPE ko100.

    READ TABLE object_texts INTO object_text
      WITH KEY object = i_object.
    IF sy-subrc = 0.
      r_text = object_text-text.
    ENDIF.

  ENDMETHOD.


  METHOD get_object_texts.

    r_object_texts = object_texts.

  ENDMETHOD.
ENDCLASS.
