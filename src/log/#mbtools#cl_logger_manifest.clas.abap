CLASS /mbtools/cl_logger_manifest DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* abap logger
*
* Copyright 2017 Eric Peterson <https://github.com/ABAP-Logger/ABAP-Logger>
* SPDX-License-Identifier: MIT
************************************************************************

  PUBLIC SECTION.

    INTERFACES if_apack_manifest.

    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_logger_manifest IMPLEMENTATION.


  METHOD constructor.

    if_apack_manifest~descriptor-group_id        = 'github.com/ABAP-Logger'.
    if_apack_manifest~descriptor-artifact_id     = 'ABAP-Logger'.
    if_apack_manifest~descriptor-version         = '1.0.0'.
    if_apack_manifest~descriptor-git_url         = 'https://github.com/ABAP-Logger/ABAP-Logger'.

  ENDMETHOD.
ENDCLASS.
