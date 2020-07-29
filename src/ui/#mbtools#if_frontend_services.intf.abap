INTERFACE /mbtools/if_frontend_services
  PUBLIC .
************************************************************************
* MBT Frontend Services
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************

  METHODS file_upload
    IMPORTING
      !iv_path       TYPE string
    RETURNING
      VALUE(rv_xstr) TYPE xstring
    RAISING
      /mbtools/cx_exception .

  METHODS file_download
    IMPORTING
      !iv_path TYPE string
      !iv_xstr TYPE xstring
    RAISING
      /mbtools/cx_exception .

  METHODS show_file_save_dialog
    IMPORTING
      !iv_title            TYPE string
      !iv_extension        TYPE string
      !iv_default_filename TYPE string
    RETURNING
      VALUE(rv_path)       TYPE string
    RAISING
      /mbtools/cx_exception .

  METHODS show_file_open_dialog
    IMPORTING
      !iv_title            TYPE string
      !iv_extension        TYPE string
      !iv_default_filename TYPE string
    RETURNING
      VALUE(rv_path)       TYPE string
    RAISING
      /mbtools/cx_exception .

ENDINTERFACE.
