  METHOD call_bapi_bp.

    DATA: lt_return         TYPE mdg_bs_bp_msgmap_t,
          lt_cvis_ei_extern TYPE cvis_ei_extern_t,
          lt_bapireti       TYPE bapiretm.

    APPEND cs_cvis_ei_extern TO lt_cvis_ei_extern.

    cl_md_bp_maintain=>maintain(
      EXPORTING
        i_data   = lt_cvis_ei_extern
*       i_test_run = 'X'
      IMPORTING
        e_return = lt_bapireti ).
    IF line_exists( lt_bapireti[ 1 ]-object_msg[ type = 'A' ] ) OR line_exists( lt_bapireti[ 1 ]-object_msg[ type = 'E' ] ).
      LOOP AT lt_bapireti[ 1 ]-object_msg INTO DATA(ls_msg).
        IF NOT ( ls_msg-id = 'CVI_EI' AND ls_msg-number = '072' ).
          add_return(
            EXPORTING
              iv_type       = ls_msg-type
              iv_id         = ls_msg-id
              iv_number     = ls_msg-number
              iv_message_v1 = ls_msg-message_v1
              iv_message_v2 = ls_msg-message_v2
              iv_message_v3 = ls_msg-message_v3
              iv_message_v4 = ls_msg-message_v4
            CHANGING
              ct_return     = ct_return ).
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
