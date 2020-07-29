INTERFACE /mbtools/if_gui_hotkey_ctl
  PUBLIC .
************************************************************************
* MBT GUI Hotkey Controller
*
* Original Author: Copyright (c) 2014 abapGit Contributors
* http://www.abapgit.org
*
* Released under MIT License: https://opensource.org/licenses/MIT
************************************************************************

  METHODS register_hotkeys
    IMPORTING
      !ii_hotkeys TYPE REF TO /mbtools/if_gui_hotkeys .

  METHODS reset .

  METHODS get_registered_hotkeys
    RETURNING
      VALUE(rt_registered_hotkeys) TYPE /mbtools/if_gui_hotkeys=>ty_hotkeys_with_descr
    RAISING
      /mbtools/cx_exception .

ENDINTERFACE.
