* MBT APACK Manifest
*
* This definition must matches ZCL_ABAPGIT_APACK_MIGRATION as defined
* in abapGit (see https://github.com/larshp/abapGit/blob/master/
* src/apack/zcl_abapgit_apack_migration.clas.abap )
*
* We ship this definition with MBT Base since Marc Bernard Tools shall
* also work without abapGit.
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************
INTERFACE /mbtools/if_apack_manifest
  PUBLIC .

  TYPES:
    BEGIN OF ty_dependency,
      group_id       TYPE string,
      artifact_id    TYPE string,
      version        TYPE string,
      git_url        TYPE string,
      target_package TYPE devclass,
    END OF ty_dependency,
    ty_dependencies    TYPE STANDARD TABLE OF ty_dependency
                       WITH NON-UNIQUE DEFAULT KEY,
    ty_repository_type TYPE string,
    BEGIN OF ty_descriptor,
      group_id        TYPE string,
      artifact_id     TYPE string,
      version         TYPE string,
      repository_type TYPE ty_repository_type,
      git_url         TYPE string,
      target_package  TYPE devclass,
      dependencies    TYPE ty_dependencies,
    END OF ty_descriptor.

  CONSTANTS:
    co_file_name         TYPE string VALUE '.apack-manifest.xml',
    co_abap_git          TYPE ty_repository_type VALUE 'abapGit',
    co_interface_version TYPE i VALUE 1.

  DATA:
    descriptor TYPE ty_descriptor READ-ONLY.

ENDINTERFACE.
