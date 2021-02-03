CLASS /mbtools/cl_bundle DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

************************************************************************
* MBT Free Tools
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
  PUBLIC SECTION.

    INTERFACES /mbtools/if_tool .

    METHODS constructor .
  PROTECTED SECTION.

    DATA mo_tool TYPE REF TO /mbtools/cl_tools .
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_bundle IMPLEMENTATION.


  METHOD /mbtools/if_tool~launch.
    " Bundles do not have any launcher
    ASSERT 1 = 2.
  ENDMETHOD.


  METHOD constructor.
    CREATE OBJECT mo_tool EXPORTING io_tool = me.
    /mbtools/if_tool~ms_manifest = mo_tool->ms_manifest.
  ENDMETHOD.
ENDCLASS.
