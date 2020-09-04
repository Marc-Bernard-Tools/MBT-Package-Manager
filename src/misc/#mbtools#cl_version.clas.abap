CLASS /mbtools/cl_version DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* MBT Version
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS normalize
      IMPORTING
        !iv_version       TYPE string
      RETURNING
        VALUE(rv_version) TYPE string .
    CLASS-METHODS compare
      IMPORTING
        !iv_current      TYPE string
        !iv_compare      TYPE string
      RETURNING
        VALUE(rv_result) TYPE i .
  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-METHODS conv_str_to_version
      IMPORTING
        !iv_version       TYPE csequence
      RETURNING
        VALUE(rs_version) TYPE /mbtools/if_definitions=>ty_version
      RAISING
        /mbtools/cx_exception .
    CLASS-METHODS check_dependant_version
      IMPORTING
        !is_current TYPE /mbtools/if_definitions=>ty_version
        !is_compare TYPE /mbtools/if_definitions=>ty_version
      RAISING
        /mbtools/cx_exception .
ENDCLASS.



CLASS /MBTOOLS/CL_VERSION IMPLEMENTATION.


  METHOD check_dependant_version.

    CONSTANTS: lc_message TYPE string VALUE 'Current version is older than required' ##NO_TEXT.

    IF is_compare-major > is_current-major.
      /mbtools/cx_exception=>raise( lc_message ).
    ELSEIF is_compare-major < is_current-major.
      RETURN.
    ENDIF.

    IF is_compare-minor > is_current-minor.
      /mbtools/cx_exception=>raise( lc_message ).
    ELSEIF is_compare-minor < is_current-minor.
      RETURN.
    ENDIF.

    IF is_compare-patch > is_current-patch.
      /mbtools/cx_exception=>raise( lc_message ).
    ELSEIF is_compare-patch < is_current-patch.
      RETURN.
    ENDIF.

    IF is_current-prerelase IS INITIAL.
      RETURN.
    ENDIF.

    CASE is_current-prerelase.
      WHEN 'rc'.
        IF is_compare-prerelase = ''.
          /mbtools/cx_exception=>raise( lc_message ).
        ENDIF.

      WHEN 'beta'.
        IF is_compare-prerelase = '' OR is_compare-prerelase = 'rc'.
          /mbtools/cx_exception=>raise( lc_message ).
        ENDIF.

      WHEN 'alpha'.
        IF is_compare-prerelase = '' OR is_compare-prerelase = 'rc' OR is_compare-prerelase = 'beta'.
          /mbtools/cx_exception=>raise( lc_message ).
        ENDIF.

    ENDCASE.

    IF is_compare-prerelase = is_current-prerelase AND is_compare-prerelase_patch > is_current-prerelase_patch.
      /mbtools/cx_exception=>raise( lc_message ).
    ENDIF.

  ENDMETHOD.


  METHOD compare.

    " current < compare: -1
    " current = compare: 0
    " current > compare: +1

    DATA: ls_version_a TYPE zif_abapgit_definitions=>ty_version,
          ls_version_b TYPE zif_abapgit_definitions=>ty_version.

    IF iv_current IS INITIAL OR iv_compare IS INITIAL.
      rv_result = 0.
      RETURN.
    ENDIF.

    TRY.
        ls_version_a = conv_str_to_version( iv_current ).
        ls_version_b = conv_str_to_version( iv_compare ).
      CATCH /mbtools/cx_exception.
        rv_result = 0.
        RETURN.
    ENDTRY.

    IF ls_version_a = ls_version_b.
      rv_result = 0.
    ELSE.
      TRY.
          check_dependant_version( is_current = ls_version_a
                                   is_compare = ls_version_b ).
          rv_result = 1.
        CATCH /mbtools/cx_exception.
          rv_result = -1.
          RETURN.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD conv_str_to_version.

    DATA: lt_segments TYPE STANDARD TABLE OF string,
          lt_parts    TYPE STANDARD TABLE OF string,
          lv_segment  TYPE string.

    SPLIT iv_version AT '-' INTO TABLE lt_segments.

    READ TABLE lt_segments INTO lv_segment INDEX 1. " Version
    IF sy-subrc <> 0.   " No version
      RETURN.
    ENDIF.

    SPLIT lv_segment AT '.' INTO TABLE lt_parts.

    LOOP AT lt_parts INTO lv_segment.

      TRY.
          CASE sy-tabix.
            WHEN 1.
              rs_version-major = lv_segment.
            WHEN 2.
              rs_version-minor = lv_segment.
            WHEN 3.
              rs_version-patch = lv_segment.
          ENDCASE.
        CATCH cx_sy_conversion_no_number.
          /mbtools/cx_exception=>raise( |Incorrect format for Semantic Version| ).
      ENDTRY.

    ENDLOOP.

    READ TABLE lt_segments INTO lv_segment INDEX 2. " Pre-release Version
    IF sy-subrc <> 0.   " No version
      RETURN.
    ENDIF.

    SPLIT lv_segment AT '.' INTO TABLE lt_parts.

    LOOP AT lt_parts INTO lv_segment.

      CASE sy-tabix.
        WHEN 1.
          rs_version-prerelase = lv_segment.
          TRANSLATE rs_version-prerelase TO LOWER CASE.
        WHEN 2.
          rs_version-prerelase_patch = lv_segment.
      ENDCASE.

    ENDLOOP.

    IF rs_version-prerelase <> 'rc' AND rs_version-prerelase <> 'beta' AND rs_version-prerelase <> 'alpha'.
      /mbtools/cx_exception=>raise( |Incorrect format for Semantic Version| ).
    ENDIF.

  ENDMETHOD.


  METHOD normalize.

    " Internal program version should be in format "XXX.XXX.XXX" or "vXXX.XXX.XXX"
    CONSTANTS:
      lc_version_pattern    TYPE string VALUE '^v?(\d{1,3}\.\d{1,3}\.\d{1,3})\s*$',
      lc_prerelease_pattern TYPE string VALUE '^((rc|beta|alpha)\.\d{1,3})\s*$'.

    DATA: lv_version      TYPE string,
          lv_prerelease   TYPE string,
          lv_version_n    TYPE string,
          lv_prerelease_n TYPE string.

    SPLIT iv_version AT '-' INTO lv_version lv_prerelease.

    FIND FIRST OCCURRENCE OF REGEX lc_version_pattern
      IN lv_version SUBMATCHES lv_version_n.

    IF lv_prerelease IS NOT INITIAL.

      FIND FIRST OCCURRENCE OF REGEX lc_prerelease_pattern
        IN lv_prerelease SUBMATCHES lv_prerelease_n.

    ENDIF.

    IF lv_version_n IS INITIAL.
      RETURN.
    ENDIF.

    rv_version = lv_version_n.

    IF lv_prerelease_n IS NOT INITIAL.
      CONCATENATE rv_version '-' lv_prerelease_n INTO rv_version.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
