*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2020-02-07 at 09:17:54
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: /MBTOOLS/VERS...................................*
DATA:  BEGIN OF STATUS_/MBTOOLS/VERS                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/MBTOOLS/VERS                 .
CONTROLS: TCTRL_/MBTOOLS/VERS
            TYPE TABLEVIEW USING SCREEN '2010'.
*.........table declarations:.................................*
TABLES: */MBTOOLS/VERS                 .
TABLES: /MBTOOLS/VERS                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
