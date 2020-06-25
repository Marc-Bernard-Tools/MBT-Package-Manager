************************************************************************
* /MBTOOLS/CL_PASSWORD_DIALOG
* MBT Password Popup
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************
CLASS /mbtools/cl_password_dialog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

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



CLASS /MBTOOLS/CL_PASSWORD_DIALOG IMPLEMENTATION.


  METHOD popup.

    PERFORM password_popup
      IN PROGRAM /mbtools/mbt
      USING iv_url
      CHANGING cv_user cv_pass.

  ENDMETHOD.
ENDCLASS.
