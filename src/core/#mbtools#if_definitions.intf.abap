INTERFACE /mbtools/if_definitions
  PUBLIC.

************************************************************************
* Marc Bernard Tools - Definitions
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-or-later
************************************************************************
  TYPES:
    ty_icon TYPE c LENGTH 4. " icon_d
  TYPES:
    ty_text TYPE c LENGTH 60. " ddtext
  TYPES:
    ty_longname TYPE c LENGTH 120. "trobj_name
  TYPES:
    BEGIN OF ty_tadir_key, " adir_key
      pgmid    TYPE c LENGTH 4,
      object   TYPE c LENGTH 4,
      obj_name TYPE c LENGTH 40,
    END OF ty_tadir_key.
  TYPES:
    ty_tadir_keys TYPE STANDARD TABLE OF ty_tadir_key WITH DEFAULT KEY.
  TYPES:
     ty_pgmid TYPE ty_tadir_key-pgmid.
  TYPES:
   ty_object TYPE ty_tadir_key-object.
  TYPES:
    ty_objects TYPE STANDARD TABLE OF ty_object WITH DEFAULT KEY.
  TYPES:
    ty_object_range TYPE RANGE OF ty_object.
  TYPES:
    ty_name TYPE ty_tadir_key-obj_name.
  TYPES:
    ty_names TYPE STANDARD TABLE OF ty_name WITH DEFAULT KEY.
  TYPES:
    ty_name_range TYPE RANGE OF ty_name.
  TYPES:
    BEGIN OF ty_object_ext, " /mbtools/object_with_icon_text,
      icon     TYPE ty_icon,
      pgmid    TYPE ty_tadir_key-pgmid,
      object   TYPE ty_tadir_key-object,
      obj_name TYPE ty_tadir_key-obj_name,
      text     TYPE ty_text,
    END OF ty_object_ext.
  TYPES:
    ty_objects_ext TYPE STANDARD TABLE OF ty_object_ext WITH DEFAULT KEY.
  TYPES:
    BEGIN OF ty_object_text, " ko100
      pgmid  TYPE ty_tadir_key-pgmid,
      object TYPE ty_tadir_key-object,
      text   TYPE ty_text,
    END OF ty_object_text.
  TYPES:
    ty_object_texts TYPE STANDARD TABLE OF ty_object_text WITH DEFAULT KEY.
  TYPES:
    BEGIN OF ty_regs, " /mbtools/regs
      relid  TYPE c LENGTH 2,
      srtfd  TYPE c LENGTH 22,
      srtf2  TYPE i,
      chdate TYPE d,
      chtime TYPE t,
      chname TYPE c LENGTH 12,
      clustr TYPE indx_clstr,
      clustd TYPE indx_clust,
    END OF ty_regs.
  TYPES:
    BEGIN OF ty_version,
      major           TYPE i,
      minor           TYPE i,
      patch           TYPE i,
      prerelase       TYPE string,
      prerelase_patch TYPE i,
    END OF ty_version.
  TYPES:
    BEGIN OF ty_transport_type,
      request TYPE trfunction,
      task    TYPE trfunction,
    END OF ty_transport_type.
  TYPES:
    BEGIN OF ty_alv_column,
      name   TYPE string,
      text   TYPE string,
      length TYPE lvc_outlen,
    END OF ty_alv_column.
  TYPES:
    ty_alv_column_tt TYPE STANDARD TABLE OF ty_alv_column WITH DEFAULT KEY.
  TYPES:
    ty_sha1 TYPE c LENGTH 40.
  TYPES:
    BEGIN OF ty_file_signature,
      path     TYPE string,
      filename TYPE string,
      sha1     TYPE ty_sha1,
    END OF ty_file_signature.
  TYPES:
    BEGIN OF ty_file.
      INCLUDE TYPE ty_file_signature.
  TYPES: data TYPE xstring,
    END OF ty_file.
  TYPES:
    ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY.
  TYPES:
    BEGIN OF ty_license,
      key    TYPE string,
      valid  TYPE abap_bool,
      expire TYPE d,
    END OF ty_license.

  CONSTANTS c_mbt TYPE string VALUE 'Marc Bernard Tools' ##NO_TEXT.
  CONSTANTS c_namespace TYPE devclass VALUE '/MBTOOLS/' ##NO_TEXT.
  CONSTANTS c_interface TYPE seoclsname VALUE '/MBTOOLS/IF_TOOL' ##NO_TEXT.
  CONSTANTS c_rfcdest TYPE rfcdest VALUE 'MBTOOLS' ##NO_TEXT.
  CONSTANTS c_github TYPE string VALUE 'github.com/Marc-Bernard-Tools' ##NO_TEXT.
  CONSTANTS c_dot_abapgit TYPE string VALUE '.abapgit.xml' ##NO_TEXT.
  CONSTANTS c_domain TYPE string VALUE 'marcbernardtools.com' ##NO_TEXT.
  CONSTANTS c_www_home TYPE string VALUE 'https://marcbernardtools.com/' ##NO_TEXT.
  CONSTANTS c_www_ping TYPE string VALUE 'info/index.html' ##NO_TEXT.
  CONSTANTS c_www_terms TYPE string VALUE 'about/terms-software/' ##NO_TEXT.
  CONSTANTS c_www_tool_docs TYPE string VALUE 'docs/' ##NO_TEXT.
  CONSTANTS c_www_tool_download TYPE string VALUE 'downloads/' ##NO_TEXT.
  CONSTANTS c_www_docs TYPE string VALUE 'support/docs/' ##NO_TEXT.
  CONSTANTS c_www_support TYPE string VALUE 'support/ticket/' ##NO_TEXT.
  CONSTANTS c_www_faq TYPE string VALUE 'support/faq/' ##NO_TEXT.
  CONSTANTS c_www_portfolio TYPE string VALUE 'tools/portfolio/' ##NO_TEXT.
ENDINTERFACE.
