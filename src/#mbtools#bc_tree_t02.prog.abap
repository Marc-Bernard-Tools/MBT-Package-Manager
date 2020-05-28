*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/BC_TREE_T02
*&---------------------------------------------------------------------*

MODULE pbo_100 OUTPUT.

  SET PF-STATUS 'MAIN'.

  SET TITLEBAR 'MAIN' WITH g_title_text g_title_value.

  /mbtools/cl_screen=>banner( i_show = abap_false ).

  /mbtools/cl_screen=>logo( i_show = abap_false ).

  CALL METHOD gr_tree->display.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.                 " PBO_0100  OUTPUT

MODULE pai_100 INPUT.

  CASE g_ok_code.

    " Finish program
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      CALL METHOD gr_tree->destroy.
      LEAVE TO SCREEN 0.

    " Pick node/item
    WHEN 'PICK'.
      CALL METHOD gr_tree->pick_node.

    " Find node/item
    WHEN 'FIND'.
      CALL METHOD gr_tree->find_node.

    " Download
    WHEN 'DOWN'.
      CALL METHOD gr_tree->download.

    " Print
    WHEN 'PRINT'.
      CALL METHOD gr_tree->print.

    " Dispatch to tree control
    WHEN OTHERS.
      CALL METHOD cl_gui_cfw=>dispatch.

  ENDCASE.

  CLEAR g_ok_code.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.                 " PAI_0100  INPUT
