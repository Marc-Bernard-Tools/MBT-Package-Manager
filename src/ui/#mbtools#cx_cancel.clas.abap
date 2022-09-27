CLASS /mbtools/cx_cancel DEFINITION
  PUBLIC
  INHERITING FROM /mbtools/cx_exception
  FINAL
  CREATE PUBLIC .

************************************************************************
* Marc Bernard Tools - Exception for Cancelling Action
*
* Copyright 2014 abapGit Contributors <http://abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgv1    TYPE symsgv OPTIONAL
        !msgv2    TYPE symsgv OPTIONAL
        !msgv3    TYPE symsgv OPTIONAL
        !msgv4    TYPE symsgv OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cx_cancel IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous
        msgv1    = msgv1
        msgv2    = msgv2
        msgv3    = msgv3
        msgv4    = msgv4.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
