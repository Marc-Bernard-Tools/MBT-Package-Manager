*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/BC_SCREEN_AT_SELECT
*&---------------------------------------------------------------------*

  gr_app->screen( ).

  CHECK sy-dynnr <> '1000'.

  CASE sscrfields.

      " About
    WHEN 'DOCU'.
      /mbtools/cl_utilities=>call_browser( gr_tool->get_url_docs( ) ).

    WHEN 'TOOL'.
      /mbtools/cl_utilities=>call_browser( gr_tool->get_url_tool( ) ).

    WHEN 'HOME'.
      /mbtools/cl_utilities=>call_browser( /mbtools/cl_tools=>c_home ).


  ENDCASE.
