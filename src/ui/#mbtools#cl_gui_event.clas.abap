CLASS /mbtools/cl_gui_event DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /mbtools/if_gui_event .

    METHODS constructor
      IMPORTING
        !iv_action   TYPE clike
        !iv_getdata  TYPE clike OPTIONAL
        !it_postdata TYPE cnht_post_data_tab OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_params TYPE REF TO /mbtools/cl_string_map .

    METHODS parse_data
      IMPORTING
        !iv_getdata          TYPE clike OPTIONAL
        !it_postdata         TYPE cnht_post_data_tab OPTIONAL
      RETURNING
        VALUE(ro_parameters) TYPE REF TO /mbtools/cl_string_map
      RAISING
        /mbtools/cx_exception .

ENDCLASS.



CLASS /mbtools/cl_gui_event IMPLEMENTATION.


  METHOD /mbtools/if_gui_event~get_param.

    rv_value = mo_params->get( iv_key ).

  ENDMETHOD.


  METHOD /mbtools/if_gui_event~get_params.

    mo_params->to_struc( CHANGING cs_container = cs_params ).

  ENDMETHOD.


  METHOD constructor.

    /mbtools/if_gui_event~mv_action   = iv_action.
    /mbtools/if_gui_event~mv_getdata  = iv_getdata.
    /mbtools/if_gui_event~mt_postdata = it_postdata.

    TRY.
        mo_params = parse_data( iv_getdata  = /mbtools/if_gui_event~mv_getdata
                                it_postdata = /mbtools/if_gui_event~mt_postdata ).
      CATCH /mbtools/cx_exception.
        ASSERT 0 = 1.
    ENDTRY.

  ENDMETHOD.


  METHOD parse_data.

    DATA:
      lv_buffer TYPE string,
      lo_data   TYPE REF TO cnht_post_data_line,
      lv_count  TYPE n LENGTH 4,
      ls_entry  TYPE /mbtools/cl_string_map=>ty_entry,
      lv_param  TYPE string,
      lt_params TYPE TABLE OF string.

    " Combine get and post data
    lv_buffer = iv_getdata.
    IF iv_getdata IS NOT INITIAL AND it_postdata IS NOT INITIAL.
      CONCATENATE lv_buffer '&' INTO lv_buffer.
    ENDIF.
    LOOP AT it_postdata REFERENCE INTO lo_data.
      IF sy-tabix < lines( it_postdata ).
        " Preserve trailing spaces...
        CONCATENATE lv_buffer '' INTO lv_buffer SEPARATED BY lo_data->*
          IN CHARACTER MODE.
      ELSE.
        " ...but not on last line
        CONCATENATE lv_buffer lo_data->* INTO lv_buffer
          IN CHARACTER MODE.
      ENDIF.
    ENDLOOP.

    CREATE OBJECT ro_parameters.

    IF lv_buffer CS '&'.
      SPLIT lv_buffer AT '&' INTO TABLE lt_params.
    ELSE.
      APPEND lv_buffer TO lt_params.
    ENDIF.

    LOOP AT lt_params INTO lv_param WHERE NOT table_line IS INITIAL.
      lv_count = lv_count + 1.

      CLEAR ls_entry.

      " Split parameter into key and value
      IF lv_param CS '='.
        SPLIT lv_param AT '=' INTO ls_entry-k ls_entry-v.
      ELSE.
        " Unnamed parameters get a running number as key i.e. "param_0001"
        ls_entry-k = 'param_' && lv_count.
        ls_entry-v = lv_param.
      ENDIF.

      " Unescape entry and convert key to lower case without leading/trailing spaces
      ls_entry-k = condense( to_lower( cl_http_utility=>unescape_url( ls_entry-k ) ) ).
      ls_entry-v = cl_http_utility=>unescape_url( ls_entry-v ).

      IF ro_parameters->has( ls_entry-k ) = abap_true AND ls_entry-v <> ro_parameters->get( ls_entry-k ).
        /mbtools/cx_exception=>raise( |Duplicate parameter { ls_entry-k } with different values| ).
      ELSE.
        ro_parameters->set( iv_key = ls_entry-k
                            iv_val = ls_entry-v ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
