CLASS /mbtools/cl_logger_injector DEFINITION
  PUBLIC
  FINAL
  FOR TESTING
  CREATE PUBLIC.

************************************************************************
* abap logger
*
* Copyright 2017 Eric Peterson <https://github.com/ABAP-Logger/ABAP-Logger>
* SPDX-License-Identifier: MIT
************************************************************************

  PUBLIC SECTION.

    CLASS-METHODS set_logger
      IMPORTING
        logger TYPE REF TO /mbtools/if_logger.

    CLASS-METHODS set_settings
      IMPORTING
        settings TYPE REF TO /mbtools/if_logger_settings.

    CLASS-METHODS set_collection
      IMPORTING
        collection TYPE REF TO /mbtools/if_logger_collection.

    CLASS-METHODS set_display_profile
      IMPORTING
        display_profile TYPE REF TO /mbtools/if_logger_disp_prof.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_logger_injector IMPLEMENTATION.


  METHOD set_collection.
    /mbtools/cl_logger_factory=>log_collection = collection.
  ENDMETHOD.


  METHOD set_display_profile.
    /mbtools/cl_logger_factory=>log_display_profile = display_profile.
  ENDMETHOD.


  METHOD set_logger.
    /mbtools/cl_logger_factory=>log_logger = logger.
  ENDMETHOD.


  METHOD set_settings.
    /mbtools/cl_logger_factory=>log_settings = settings.
  ENDMETHOD.
ENDCLASS.
