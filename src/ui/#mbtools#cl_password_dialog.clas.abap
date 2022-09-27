CLASS /mbtools/cl_password_dialog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* Marc Bernard Tools - Password Popup
*
* Copyright 2014 abapGit Contributors <http://abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS popup
      IMPORTING
        !iv_url  TYPE string
      CHANGING
        !cv_user TYPE string
        !cv_pass TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_password_dialog IMPLEMENTATION.


  METHOD popup.

    PERFORM password_popup
      IN PROGRAM /mbtools/mbt
      USING iv_url
      CHANGING cv_user cv_pass.

  ENDMETHOD.
ENDCLASS.
