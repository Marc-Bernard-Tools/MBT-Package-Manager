INTERFACE /mbtools/if_gui_hotkeys
  PUBLIC .
************************************************************************
* MBT GUI Hotkey Definitions
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
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
