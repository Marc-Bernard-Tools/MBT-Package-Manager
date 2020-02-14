*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_/MBTOOLS/TABLES
*   generation date: 2020-01-07 at 09:44:56
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_/MBTOOLS/TABLES    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
