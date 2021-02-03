CLASS /mbtools/cl_icon DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* MBT Icons
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

  PUBLIC SECTION.

    CLASS-METHODS get_from_mimetype
      IMPORTING
        !iv_mimetype     TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE icon_d .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_icon IMPLEMENTATION.


  METHOD get_from_mimetype.

    CASE iv_mimetype.
      WHEN 'text/html'.
        rv_result = icon_htm.
      WHEN 'text/xml'.
        rv_result = icon_xml_doc.
      WHEN 'image/gif'.
        rv_result = icon_gif.
      WHEN 'image/jpg' OR 'image/png'.
        rv_result = icon_jpg.
      WHEN 'application/pdf'.
        rv_result = icon_pdf.
      WHEN 'application/x-zip-compressed' OR 'application/zip' OR 'application/vnd.rar'.
        rv_result = icon_include_objects.
      WHEN 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
        OR 'application/msword' OR 'application/rtf'.
        rv_result = icon_doc.
      WHEN 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
        OR 'application/vnd.ms-excel'.
        rv_result = icon_xls.
      WHEN 'application/vnd.openxmlformats-officedocument.presentationml.presentation'
        OR 'application/vnd.ms-powerpoint'.
        rv_result = icon_ppt.
      WHEN OTHERS.
        rv_result = icon_binary_document.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.