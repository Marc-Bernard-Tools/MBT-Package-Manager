  METHOD save_callstack.

    FIELD-SYMBOLS: <ls_callstack> TYPE abap_callstack_line.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = mt_callstack.

    " You should remember that the first lines are from /mbtools/cx_exception
    " and are removed so that highest level in the callstack is the position where
    " the exception is raised.
    "
    " For the merged report it's hard to do that, because /mbtools/cx_exception
    " isn't visible in the callstack. Therefore we have to check the Events.
    LOOP AT mt_callstack ASSIGNING <ls_callstack>.

      IF <ls_callstack>-mainprogram CP |/MBTOOLS/CX_EXCEPTION*|.
        DELETE TABLE mt_callstack FROM <ls_callstack>.
      ELSE.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
