INTERFACE /mbtools/if_gui_hotkeys
  PUBLIC .

************************************************************************
* Marc Bernard Tools - GUI Hotkey Definitions
*
* Copyright 2014 abapGit Contributors <http://www.abapgit.org>
* SPDX-License-Identifier: MIT
************************************************************************
  TYPES:
    BEGIN OF ty_hotkey_with_descr,
      ui_component TYPE string,
      action       TYPE string,
      hotkey       TYPE string,
      description  TYPE string,
    END OF ty_hotkey_with_descr .

  TYPES:
    ty_hotkeys_with_descr TYPE STANDARD TABLE OF ty_hotkey_with_descr
      WITH DEFAULT KEY
      WITH UNIQUE SORTED KEY action COMPONENTS ui_component action .

  CLASS-METHODS get_hotkey_actions " TODO: try to refactor class-method
    RETURNING
      VALUE(rt_hotkey_actions) TYPE ty_hotkeys_with_descr .

ENDINTERFACE.
