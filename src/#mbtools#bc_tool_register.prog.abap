************************************************************************
* /MBTOOLS/BC_TOOL_REGISTER
* MBT Tool Registation
*
* (c) Marc Bernard Tools 2020
* last update: 2020-02-11
************************************************************************

REPORT /mbtools/bc_tool_register.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS:
  p_name TYPE string MEMORY ID /mbtools/tool LOWER CASE,
  p_all  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS:
  p_reg   RADIOBUTTON GROUP g1 DEFAULT 'X',
  p_unreg RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b2.

DATA:
  g_flag TYPE abap_bool.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_name.

  CALL METHOD /mbtools/cl_tools=>f4_tools
    EXPORTING
      i_pattern = p_name
    RECEIVING
      r_name    = p_name.

START-OF-SELECTION.

  IF p_all = abap_true.
    IF p_reg = abap_true.
      CALL METHOD /mbtools/cl_tools=>register_all
        RECEIVING
          r_registered = g_flag.
    ELSE.
      CALL METHOD /mbtools/cl_tools=>unregister_all
        RECEIVING
          r_unregistered = g_flag.
    ENDIF.
  ELSE.
    IF p_reg = abap_true.
      CALL METHOD /mbtools/cl_tools=>register
        EXPORTING
          i_object     = /mbtools/cl_tools=>get_class( i_name = p_name )
        RECEIVING
          r_registered = g_flag.
    ELSE.
      CALL METHOD /mbtools/cl_tools=>unregister
        EXPORTING
          i_object       = /mbtools/cl_tools=>get_class( i_name = p_name )
        RECEIVING
          r_unregistered = g_flag.
    ENDIF.
  ENDIF.

  IF g_flag = abap_true.
    WRITE: / 'Success' COLOR COL_POSITIVE.
  ELSE.
    WRITE: / 'Error' COLOR COL_NEGATIVE.
  ENDIF.
