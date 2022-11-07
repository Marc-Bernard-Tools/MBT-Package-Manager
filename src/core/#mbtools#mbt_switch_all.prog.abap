REPORT /mbtools/mbt_switch_all.

************************************************************************
* Marc Bernard Tools - Switch All Enhancements On/Off
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************

* Main
SELECTION-SCREEN:
BEGIN OF BLOCK b100 WITH FRAME,
COMMENT /1(77) sc_t100,
SKIP,
COMMENT /1(77) sc_t101,
END OF BLOCK b100.
SELECTION-SCREEN BEGIN OF BLOCK b200 WITH FRAME.
PARAMETERS:
p_show RADIOBUTTON GROUP g0 DEFAULT 'X',
p_on   RADIOBUTTON GROUP g0,
p_off  RADIOBUTTON GROUP g0.
SELECTION-SCREEN END OF BLOCK b200.

DATA:
  gv_msg  TYPE string,
  gv_enh  TYPE progname,
  gt_enh  TYPE TABLE OF progname,
  gv_ext  TYPE enhincludeextension,
  gt_code TYPE rswsourcet.

FIELD-SYMBOLS:
  <gv_code> TYPE string.

INITIALIZATION.

  sc_t100 = 'Marc Bernard Tools'.
  sc_t101 = 'This program will switch all MBT enhancement implementations on or off.'.

START-OF-SELECTION.

  " Get all includes for MBT enhancement implementations
  SELECT name FROM progdir INTO TABLE gt_enh
    WHERE name LIKE '/MBTOOLS/%' AND subc = 'I'
    ORDER BY name.
  IF sy-subrc <> 0.
    WRITE: / 'No MBT enhancements implemented' COLOR COL_TOTAL.
    RETURN.
  ENDIF.

  " Keep only enhancements (but not for own exception class)
  DELETE gt_enh WHERE table_line+30(1) <> 'E'.
  DELETE gt_enh WHERE table_line CP '/MBTOOLS/BC_EXCEPTION*'.

  LOOP AT gt_enh INTO gv_enh.
    gv_msg = |MBT enhancement implementation { gv_enh }|.
    WRITE: / gv_msg COLOR COL_NORMAL.
    SKIP.

    gv_ext = gv_enh+30(1).

    READ REPORT gv_enh INTO gt_code.
    CHECK sy-subrc = 0.

    LOOP AT gt_code ASSIGNING <gv_code>.
      CASE abap_true.
        WHEN p_on.
          PERFORM code_on CHANGING <gv_code>.
        WHEN p_off.
          PERFORM code_off CHANGING <gv_code>.
        WHEN p_show.
          WRITE: / <gv_code>.
      ENDCASE.
    ENDLOOP.
    IF p_show = abap_true.
      SKIP.
      CONTINUE.
    ENDIF.

    INSERT REPORT gv_enh FROM gt_code
      EXTENSION TYPE gv_ext
      STATE 'A'
      PROGRAM TYPE 'I'.
    IF sy-subrc = 0.
      gv_msg = |MBT enhancement implementation { gv_enh } successfully updated|.
      WRITE: / gv_msg COLOR COL_POSITIVE.
    ELSE.
      gv_msg = |Error updating MBT enhancement implementation { gv_enh }|.
      WRITE: / gv_msg COLOR COL_NEGATIVE.
    ENDIF.

    INSERT REPORT gv_enh FROM gt_code
      EXTENSION TYPE gv_ext
      STATE 'I'
      PROGRAM TYPE 'I'.
    ASSERT sy-subrc = 0.

    ULINE.
  ENDLOOP.

  CASE abap_true.
    WHEN p_on.
      UPDATE badiimpl_enh SET active = abap_true WHERE enhname LIKE '/MBTOOLS/%' OR badi_impl = '/MBTOOLS/%'.
      IF sy-subrc = 0.
        WRITE: / 'MBT enhancement BAdI implementations actived successfully' COLOR COL_POSITIVE.
      ELSE.
        WRITE: / 'Error activating MBT enhancement BAdI implementations' COLOR COL_NEGATIVE.
      ENDIF.
    WHEN p_off.
      UPDATE badiimpl_enh SET active = abap_false WHERE enhname LIKE '/MBTOOLS/%' OR badi_impl = '/MBTOOLS/%'.
      IF sy-subrc = 0.
        WRITE: / 'MBT enhancement BAdI implementations deactived successfully' COLOR COL_POSITIVE.
      ELSE.
        WRITE: / 'Error deactivating MBT enhancement BAdI implementations' COLOR COL_NEGATIVE.
      ENDIF.
  ENDCASE.

FORM code_on CHANGING cv_line TYPE string.
  " Keep open and close statements
  IF cv_line CP 'ENHANCEMENT *' OR cv_line = 'ENDENHANCEMENT.' OR cv_line CP 'METHOD *' OR cv_line = 'ENDMETHOD.'.
    RETURN.
  ENDIF.
  " Remove comment
  IF strlen( cv_line ) >= 3 AND cv_line(3) = '*##'.
    cv_line = cv_line+3.
  ENDIF.
ENDFORM.

FORM code_off CHANGING cv_line TYPE string.
  " Keep open and close statements
  IF cv_line CP 'ENHANCEMENT *' OR cv_line = 'ENDENHANCEMENT.' OR cv_line CP 'METHOD *' OR cv_line = 'ENDMETHOD.'.
    RETURN.
  ENDIF.
  " Add comment
  IF strlen( cv_line ) >= 3 AND cv_line(3) = '*##'.
    RETURN.
  ENDIF.
  cv_line = '*##' && cv_line.
ENDFORM.
