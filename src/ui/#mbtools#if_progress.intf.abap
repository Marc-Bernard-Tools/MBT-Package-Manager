INTERFACE /mbtools/if_progress
  PUBLIC .

************************************************************************
* Marc Bernard Tools - Progress Indicator
*
* Copyright 2014 abapGit Contributors <http://abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  METHODS hide.
  METHODS show
    IMPORTING
      !iv_current TYPE i
      !iv_text    TYPE csequence .
  METHODS set_total
    IMPORTING
      !iv_total TYPE i OPTIONAL.
ENDINTERFACE.
