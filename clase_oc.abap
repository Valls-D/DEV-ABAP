CLASS /vwk/cl_pl0_oc_suministro DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tyt_xlikp         TYPE STANDARD TABLE OF likpvb .
    TYPES:
      tyt_xlips         TYPE STANDARD TABLE OF lipsvb .
    TYPES:
      ty_lfart_range    TYPE RANGE OF lfart .
    TYPES tyt_lfart_trigger TYPE ty_lfart_range .
    TYPES: BEGIN OF ty_hu,
             meins TYPE meins,
             exidv TYPE exidv,
             vemng TYPE vemng,
             vhilm TYPE vhilm,
           END OF ty_hu,
           tyt_hu TYPE TABLE OF ty_hu.

    METHODS constructor
      IMPORTING
        !it_xlikp TYPE tyt_xlikp
        !it_xlips TYPE tyt_xlips .
    METHODS run_process_oc .

*********************************************************
private section.

  constants GC_WBSTK_CONF type WBSTK value 'C' ##NO_TEXT.
  constants:
    gc_vpobj_01   TYPE c LENGTH 2 value '01' ##NO_TEXT.
  constants:
    gc_vpobj_12   TYPE c LENGTH 2 value '12' ##NO_TEXT.
  constants GC_WERKS type /VWK/PL0TBC999-WERKS value '4900' ##NO_TEXT.
  constants GC_LGNUM type /VWK/PL0TBC999-LGNUM value '463' ##NO_TEXT.
  constants GC_ID_LFART type /VWK/PL0TBC999-ID value 'LFART_TRIGGER_ENT_OC' ##NO_TEXT.
  constants GC_SEQUE type /VWK/PL0TBC999-SEQUE value '000' ##NO_TEXT.
  constants:
    gc_mode       TYPE c LENGTH 1 value 'N' ##NO_TEXT.
    " Usados en procesar_altas_stock_sum
  constants GC_MOVIMIENTO type BAPI2017_GM_ITEM_CREATE-MOVE_TYPE value '501' ##NO_TEXT.
  constants GC_GM_CODE type CHAR4 value '03' ##NO_TEXT.
  constants GC_PLANTA type CHAR4 value '4900' ##NO_TEXT.
  constants GC_LGORT_OC01 type CHAR4 value 'OC01' ##NO_TEXT.
  constants GC_C_MODE type CHAR1 value 'N' ##NO_TEXT.         " N = background, E = error display
  constants GC_STATUS_OK type CHAR4 value '@5B@' ##NO_TEXT.         " N = background, E = error display
  constants GC_STATUS_KO type CHAR4 value '@5C@' ##NO_TEXT.         " N = background, E = error display
  constants:
    gc_error TYPE c LENGTH 1 value 'E' ##NO_TEXT.                 " N = background, E = error display
  data GT_XLIKP type TYT_XLIKP .
  data GS_XLIKP type LIKPVB .
  data GT_XLIPS type TYT_XLIPS .
  data GS_XLIPS type LIPSVB .
  data GT_HU type TYT_HU .
  data GT_HU_OC type TYT_HU .
  data GT_LFART_TRIGGER type TYT_LFART_TRIGGER .
  data GS_LOG type /VWK/PL0TOC_SUM .
  data:
    gt_log TYPE TABLE OF /vwk/pl0toc_sum .
  data GS_STOCK type /VWK/PL0_TY_STOCK .
  data GV_VHILM type VHILM .
  data GV_CONTROL type ABAP_BOOLEAN .
  data:
    gt_messages TYPE TABLE OF bdcmsgcoll .
  data GS_MESSAGE type BDCMSGCOLL .
  data:
    gt_bdcdata  TYPE TABLE OF bdcdata .
  data GS_BDCDATA type BDCDATA .

  methods _LOAD_PARAM_INI .
  methods _CONTROL_SET_ERROR .
  methods _CONTROL_SET_TRUE .
  methods _CONTROL_CHECK_CONTINUE
    returning
      value(RV_RETURN) type ABAP_BOOLEAN .
  methods _CHECK_DATA_INI .
  methods _CHECK_CONFIRMACION
    returning
      value(RV_CHECK) type ABAP_BOOLEAN .
  methods _LOAD_PARAM_LFART .
  methods _LOAD_HUS .
  methods _LOG_INIT .
  methods _LOG_SAVE .
  methods _SUMIN_RUN .
  methods _SUMIN_OUTB_DELV .
  methods _SUMIN_OUTB_DELV_EMBAL_2 .
  methods _SUMIN_OUTB_DELV_EMBAL .
  methods _SUMIN_OUTB_DELV_CONTAB .
  methods _SUMIN_OUTB_DELV_SM .
  methods _SUMIN_OUTB_DELV_SM_CHECK
    returning
      value(RV_RETURN) type ABAP_BOOLEAN .
  methods _SUMIN_TRANSFER_ORDER .
  methods _PREPARE_DATA .
  methods _SUMIN_TRANSFER_ORDER_AUX_HU .
    
*************************************************
    METHOD constructor.
    gt_xlikp = it_xlikp.
    gt_xlips = it_xlips.
    gs_xlikp = it_xlikp[ 1 ].
    gs_xlips = it_xlips[ 1 ].
    _control_set_true( ).

    _check_data_ini( ).
    CHECK _control_check_continue( ).

    _load_param_ini( ).
    CHECK _control_check_continue( ).

    _log_init( ).
  ENDMETHOD.
  
************************************************
   METHOD run_process_oc.

                              CHECK _control_check_continue( ).
    _check_confirmacion(  ).  CHECK _control_check_continue( ).
    _load_hus(            ).  CHECK _control_check_continue( ).
    _prepare_data(        ).
    _sumin_run(           ).
    _log_save(            ).

  ENDMETHOD.
  
******************************************************
    METHOD _check_confirmacion.
    IF gs_xlikp-lfart NOT IN gt_lfart_trigger.  _control_set_error( ). RETURN.  ENDIF.
    IF gs_xlikp-wbstk <> gc_wbstk_conf.         _control_set_error( ). RETURN.  ENDIF.
  ENDMETHOD.

***********************************************************  

  METHOD _load_hus.
    " Buscamos HU asociada a la entrega
    SELECT        vekp~meins,
                  vekp~exidv,
                  vepo~vemng,
                  vekp~vhilm
      FROM vekp AS vekp
      INNER JOIN vepo AS vepo ON vepo~venum = vekp~venum
      INTO TABLE @gt_hu
      WHERE vekp~vpobjkey = @gs_xlikp-vbeln
      AND   vekp~vpobj    = @gc_vpobj_01.

    IF sy-subrc <> 0.
      _control_set_error( ).
    ENDIF.
  ENDMETHOD.

*******************************************************
  METHOD _sumin_run.

    _sumin_outb_delv(         ).   _control_check_continue( ).
      _sumin_outb_delv_embal(   ).
    _control_check_continue( ).
    _sumin_outb_delv_contab(  ).   _control_check_continue( ).
    _sumin_outb_delv_sm(      ).   _control_check_continue( ).
    _sumin_transfer_order(    ).

  ENDMETHOD.  
********************************************************
  METHOD _sumin_outb_delv.
    DATA: lt_goodsmvt_item   TYPE STANDARD TABLE OF bapi2017_gm_item_create,
          ls_goodsmvt_item   TYPE bapi2017_gm_item_create,
          ls_goodsmvt_header TYPE bapi2017_gm_head_01,
          ls_goodsmvt_code   TYPE bapi2017_gm_code,
          lt_return          TYPE STANDARD TABLE OF bapiret2,
          lv_delivery        TYPE vbeln.

    CLEAR: ls_goodsmvt_header.
    ls_goodsmvt_header-pstng_date = sy-datum.
    ls_goodsmvt_header-doc_date   = sy-datum.
    ls_goodsmvt_header-pr_uname   = sy-uname.
    ls_goodsmvt_header-header_txt = |Altas Claves Ficticia { gs_stock-matnr } OC|. " TODO: Revisar texto
    ls_goodsmvt_code-gm_code = gc_gm_code.

    REFRESH: lt_goodsmvt_item.
    ls_goodsmvt_item-material  = gs_stock-matnr.
    ls_goodsmvt_item-plant     = gc_planta.
    ls_goodsmvt_item-stge_loc  = gc_lgort_oc01.
    ls_goodsmvt_item-move_type = gc_movimiento.
    ls_goodsmvt_item-entry_qnt = gs_stock-verme.
    ls_goodsmvt_item-entry_uom = gs_stock-meins.
    APPEND ls_goodsmvt_item TO lt_goodsmvt_item.

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = ls_goodsmvt_header
        goodsmvt_code    = ls_goodsmvt_code
      IMPORTING
        materialdocument = lv_delivery
      TABLES
        goodsmvt_item    = lt_goodsmvt_item
        return           = lt_return.

    IF line_exists( lt_return[ type = gc_error ] ) OR NOT line_exists( lt_return[ id = 'L9' number = '514' ] ).
      _control_set_error( ).
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    gs_log-documento_oc = lt_return[ id = 'L9' number = '514' ]-message_v1.
    gs_log-documento_oc = |{ gs_log-documento_oc ALPHA = IN }|.

  ENDMETHOD.  

********************************************************************

*METHOD _sumin_outb_delv_embal_2.
*
*  DATA: ls_vbkok TYPE vbkok,
*        lt_hu    TYPE STANDARD TABLE OF hum_rehang_hu,
*        lt_hus   TYPE STANDARD TABLE OF vekpvb,
*        lt_prot  TYPE TABLE OF prott,
*        lv_err   TYPE char1.
*
*  CALL FUNCTION 'HU_PACKING_REFRESH'.
*
*  CALL FUNCTION 'HU_READ_DELIVERY_AND_INIT'
*    EXPORTING
*      if_delivery = gs_log-documento_oc
*    EXCEPTIONS
*      OTHERS      = 1.
*
*  IF sy-subrc <> 0.
*    _control_set_error( ).
*    RETURN.
*  ENDIF.
*
*  CLEAR ls_vbkok.
*  ls_vbkok-vbeln_vl = gs_log-documento_oc.
*  LOOP AT gt_hu INTO DATA(ls_hu_src).
*    SELECT SINGLE venum
*      FROM vekp
*      WHERE exidv = @ls_hu_src-exidv
*      INTO @DATA(lv_venum).
*    APPEND VALUE hum_rehang_hu(
*      top_hu_external = ls_hu_src-exidv
**      top_hu_internal = ls_hu_src-exidv
*      venum           = lv_venum
*      rfbel           = gs_log-documento_oc
*      rfpos           = '000010'
*    ) TO lt_hu.
*  ENDLOOP.
*
*  CALL FUNCTION 'WS_DELIVERY_UPDATE'
*    EXPORTING
*      vbkok_wa                 = ls_vbkok
*      delivery                 = gs_log-documento_oc
*      synchron                 = 'X'
*      commit                   = ' '
*      nicht_sperren            = space
*      if_database_update       = '1'
*      if_error_messages_send_0 = 'X'
*    IMPORTING
*      ef_error_any_0           = lv_err
*    TABLES
*      it_handling_units        = lt_hu
*      et_created_hus           = lt_hus
*      prot                     = lt_prot
*    EXCEPTIONS
*      error_message            = 1
*      OTHERS                   = 2.
*
*  IF sy-subrc <> 0 OR lv_err IS NOT INITIAL.
*    _control_set_error( ).
*    RETURN.
*  ENDIF.
*
*  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*    EXPORTING
*      wait = 'X'.
*
*ENDMETHOD.



METHOD _sumin_outb_delv_embal_2.

  DATA: ls_vbkok          TYPE vbkok,
        lt_return         TYPE STANDARD TABLE OF bapiret2,
        lt_itemprop       TYPE STANDARD TABLE OF bapihuitmproposal,
        ls_headerprop     TYPE bapihuhdrproposal,
        ls_huheader       TYPE bapihuheader,
        lv_hukey          TYPE bapihukey-hu_exid,
        lt_prot           TYPE STANDARD TABLE OF prott,
        lt_handling_units TYPE STANDARD TABLE OF hum_rehang_hu,
        ls_handling_units TYPE hum_rehang_hu,
        lt_rehang         TYPE STANDARD TABLE OF hum_rehang_hu,
        ls_rehang         TYPE hum_rehang_hu,
        lt_huitems        TYPE STANDARD TABLE OF bapihuitem,
        lv_err            TYPE char1,
        lt_verko_tab      TYPE STANDARD TABLE OF verko,
        ls_verko_tab      TYPE verko,
        lt_verpo_tab      TYPE STANDARD TABLE OF verpo,
        ls_verpo_tab      TYPE verpo.

  CALL FUNCTION 'HU_PACKING_REFRESH'.

  CALL FUNCTION 'HU_READ_DELIVERY_AND_INIT'
    EXPORTING
      if_delivery = gs_log-documento_oc
    EXCEPTIONS
      OTHERS      = 1.

  IF sy-subrc <> 0.
    _control_set_error( ).
    RETURN.
  ENDIF.

  LOOP AT gt_hu INTO DATA(ls_hu_src).

    CLEAR: ls_headerprop, ls_huheader, lv_hukey, lt_return, lt_itemprop, lt_huitems, lt_handling_units, lt_prot.

    ls_headerprop-pack_mat = ls_hu_src-vhilm.

    CALL FUNCTION 'BAPI_HU_CREATE'
      EXPORTING
        headerproposal = ls_headerprop
      IMPORTING
        huheader       = ls_huheader
        hukey          = lv_hukey
      TABLES
        itemsproposal  = lt_itemprop
        return         = lt_return
        huitem         = lt_huitems.

    IF line_exists( lt_return[ type = 'E' ] ) OR lv_hukey IS INITIAL.
      _control_set_error( ).
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    ls_huheader-pack_mat_object = '01'.
    ls_huheader-pack_mat_obj_key = gs_log-documento_oc.

    CALL FUNCTION 'BAPI_HU_CHANGE_HEADER'
      EXPORTING
        hukey     = lv_hukey
        huchanged = ls_huheader
      IMPORTING
        huheader  = ls_huheader
      TABLES
        return    = lt_return.

    IF line_exists( lt_return[ type = 'E' ] ).
      _control_set_error( ).
      RETURN.
    ENDIF.

*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.

*    ls_vbkok-vbeln_vl = gs_log-documento_oc.
*    ls_vbkok-vbeln    = gs_log-documento_oc.
*    ls_vbkok-vbtyp_vl = 'J'.
*
*    ls_handling_units-top_hu_external = ls_huheader-hu_exid.
*    ls_handling_units-top_hu_internal = ls_huheader-hu_id.
**    ls_handling_units-venum           = ls_huheader-venum.
*    ls_handling_units-rfbel           = gs_log-documento_oc.
*    ls_handling_units-rfpos           = '000010'.
*    APPEND ls_handling_units TO lt_handling_units.
*
*    CLEAR: ls_verko_tab, ls_verpo_tab.
*
*    MOVE-CORRESPONDING ls_huheader TO ls_verko_tab.
*    ls_verko_tab-object = '01'.
*    ls_verko_tab-objkey = gs_log-documento_oc.
*    ls_verko_tab-ernam  = sy-uname.
*    APPEND ls_verko_tab TO lt_verko_tab.
*
*    CLEAR ls_verpo_tab.
*    ls_verpo_tab-exidv_ob = ls_huheader-hu_exid.
*    ls_verpo_tab-exidv    = ls_huheader-hu_exid.
*    ls_verpo_tab-vbeln    = gs_log-documento_oc.
*    ls_verpo_tab-posnr    = '900001'.
*    ls_verpo_tab-tmeng    = ls_hu_src-vemng.
*    ls_verpo_tab-velin    = '1'.
*    APPEND ls_verpo_tab TO lt_verpo_tab.
*
*    CALL FUNCTION 'WS_DELIVERY_UPDATE_2'
*      EXPORTING
*        vbkok_wa               = ls_vbkok
*        synchron               = abap_true
*        commit                 = abap_true
*        delivery               = gs_log-documento_oc
*        nicht_sperren_1        = abap_true
*        if_error_messages_send = abap_true
*      TABLES
*        prot                   = lt_prot
*        verko_tab              = lt_verko_tab
*        verpo_tab              = lt_verpo_tab
*        it_handling_units_1    = lt_handling_units
*      EXCEPTIONS
*        error_message          = 1
*        OTHERS                 = 2.

    CLEAR ls_vbkok.
    ls_vbkok-vbeln_vl = gs_log-documento_oc.
    ls_vbkok-vbeln    = gs_log-documento_oc.
    ls_vbkok-vbtyp_vl = '7'.

    CLEAR ls_rehang.
    ls_rehang-top_hu_external = ls_huheader-hu_exid.
    ls_rehang-top_hu_internal = ls_huheader-hu_id.

    SELECT SINGLE venum
      INTO @DATA(lv_venum)
      FROM vekp
      WHERE exidv = @ls_huheader-hu_exid.
    IF sy-subrc <> 0.
      _control_set_error( ).
      RETURN.
    ENDIF.

    ls_rehang-venum           = lv_venum.
    ls_rehang-rfbel           = gs_log-documento_oc.
    ls_rehang-rfpos           = '900001'.
    APPEND ls_rehang TO lt_rehang.

    CALL FUNCTION 'WS_DELIVERY_UPDATE'
      EXPORTING
        vbkok_wa                 = ls_vbkok
        synchron                 = 'X'
        commit                   = ' '
        delivery                 = gs_log-documento_oc
        update_picking           = ' '
        nicht_sperren            = space
        if_database_update       = '1'
        if_error_messages_send_0 = 'X'
      IMPORTING
        ef_error_any_0           = lv_err
      TABLES
        it_handling_units        = lt_rehang
        prot                     = lt_prot
      EXCEPTIONS
        error_message            = 1
        OTHERS                   = 2.

    IF sy-subrc <> 0 OR lv_err IS NOT INITIAL.
      _control_set_error( ).
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ENDLOOP.

ENDMETHOD.



*METHOD _sumin_outb_delv_embal_2.
*
*  DATA: ls_vbkok      TYPE vbkok,
*        lt_pack       TYPE STANDARD TABLE OF repack_hu_wm,
*        ls_pack       TYPE repack_hu_wm,
*        lt_return     TYPE STANDARD TABLE OF bapiret2,
*        ls_headerprop TYPE bapihuhdrproposal,
*        ls_huheader   TYPE bapihuheader,
*        lv_hukey      TYPE bapihukey-hu_exid,
*        lt_prot       TYPE STANDARD TABLE OF prott,
*        lt_hus        TYPE STANDARD TABLE OF vekpvb,
*        lv_err        TYPE char1,
*        lv_desthu     TYPE exidv,
*        lv_idx        TYPE i.
*
*  CALL FUNCTION 'HU_PACKING_REFRESH'.
*
*  CALL FUNCTION 'HU_READ_DELIVERY_AND_INIT'
*    EXPORTING
*      if_delivery = gs_log-documento_oc
*    EXCEPTIONS
*      OTHERS      = 1.
*
*  IF sy-subrc <> 0.
*    _control_set_error( ).
*    RETURN.
*  ENDIF.
*
*  CLEAR ls_vbkok.
*  ls_vbkok-vbeln_vl = gs_log-documento_oc.
*  ls_vbkok-vbeln    = gs_log-documento_oc.
*  ls_vbkok-vbtyp_vl = '7'.
*
*  lv_idx = 0.
*
*  LOOP AT gt_hu INTO DATA(ls_hu_src).
*    lv_idx = lv_idx + 1.
*
*    CLEAR: ls_headerprop, ls_huheader, lv_hukey, lt_return, lv_desthu.
*    CLEAR ls_pack.
*
*    ls_headerprop-pack_mat = ls_hu_src-vhilm.
*
*    CALL FUNCTION 'BAPI_HU_CREATE'
*      EXPORTING
*        headerproposal = ls_headerprop
*      IMPORTING
*        huheader       = ls_huheader
*        hukey          = lv_hukey
*      TABLES
*        return         = lt_return.
*
*    IF line_exists( lt_return[ type = 'E' ] ) OR lv_hukey IS INITIAL.
*      _control_set_error( ).
*      RETURN.
*    ENDIF.
*
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*
*    lv_desthu = lv_hukey.
*
*    ls_pack-desthu   = lv_desthu.
*    ls_pack-sourcehu = space.
*    ls_pack-kzueb    = 'X'.
*    ls_pack-quantity = ls_hu_src-vemng.
*    ls_pack-meins    = gs_stock-meins.
*    ls_pack-matnr    = gs_stock-matnr.
**    ls_pack-charg    = ls_hu_src-charg.
*    ls_pack-werks    = gc_planta.
*    ls_pack-lgort    = gc_lgort_oc01.
*    ls_pack-vbeln_vl = gs_log-documento_oc.
*    ls_pack-posnr_vl = '000010'.
*    ls_pack-object   = '01'.
*    ls_pack-objkey   = gs_log-documento_oc.
*
*    APPEND ls_pack TO lt_pack.
*
*    CALL FUNCTION 'WS_DELIVERY_UPDATE'
*      EXPORTING
*        vbkok_wa                 = ls_vbkok
*        synchron                 = 'X'
*        commit                   = space
*        delivery                 = gs_log-documento_oc
*        update_picking           = space
*        nicht_sperren            = space
*        if_database_update       = '1'
*        if_error_messages_send_0 = 'X'
*      IMPORTING
*        ef_error_any_0           = lv_err
*      TABLES
*        it_packing               = lt_pack
*        et_created_hus           = lt_hus
*        prot                     = lt_prot
*      EXCEPTIONS
*        error_message            = 1
*        OTHERS                   = 2.
*
*    IF sy-subrc <> 0 OR lv_err IS NOT INITIAL.
*      _control_set_error( ).
*      RETURN.
*    ENDIF.
*
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*
*    CLEAR lt_pack.
*
*  ENDLOOP.
*
*ENDMETHOD.
*****************************************************
  METHOD _sumin_outb_delv_embal.
    DATA: ls_hu TYPE ty_hu.
    IF sy-uname = 'MIUSUARIO'.
      CONSTANTS:
        lc_pstyv_zeln TYPE pstyv_vl   VALUE 'ZELN',
        lc_pack_mat   TYPE matnr      VALUE 'FASS',
        lc_vpobj_inb  TYPE vekp-vpobj VALUE '03',  " Inbound delivery
        lc_velin_mat  TYPE vepo-velin VALUE '1'.   " Material

      DATA:
        ls_vbkok           TYPE vbkok,
        lt_return          TYPE STANDARD TABLE OF bapiret2,
        lt_itemprop        TYPE STANDARD TABLE OF bapihuitmproposal,
        ls_itemprop        TYPE bapihuitmproposal,
        ls_headerproposal  TYPE bapihuhdrproposal,
        ls_huheader        TYPE bapihuheader,
        lv_hukey           TYPE bapihukey-hu_exid,
        lt_huitems         TYPE STANDARD TABLE OF bapihuitem,
        lt_prot            TYPE STANDARD TABLE OF prott,
        lt_handling_units  TYPE STANDARD TABLE OF hum_rehang_hu,
        ls_handling_units  TYPE hum_rehang_hu,
        lv_err             TYPE char1,
        lt_verko_tab       TYPE STANDARD TABLE OF verko,
        ls_verko_tab       TYPE verko,
        lt_verpo_tab       TYPE STANDARD TABLE OF verpo,
        ls_verpo_tab       TYPE verpo,
        ls_vekp            TYPE vekp,
        " HUs que se cargarán en el buffer global HU
        lt_venum           TYPE hum_venum_t,
        " HUs devueltas/cargadas por HU_GET_HUS
        lt_header          TYPE hum_hu_header_t,
        ls_header          TYPE vekpvb,
        lt_items           TYPE hum_hu_item_t,
        " Mensajes del framework HU
        lt_messages        TYPE huitem_messages_t,
        " Posición de delivery que queremos embalar
        ls_packing_request TYPE packing_item_hu,
        " Return code interno de HU_PACKING_AND_UNPACKING
        lv_rcode           TYPE sy-subrc,
        " Número devuelto por HU_POST
        lv_number          TYPE vpobjkey,
        " Objeto al que pertenecen las HUs
        ls_object          TYPE hum_object,
        " Posiciones pendientes de packing de la delivery
        lt_v51vp           TYPE vse_t_v51vp.

      "Limpiar buffer
      CALL FUNCTION 'HU_PACKING_REFRESH'.
      REFRESH: lt_verko_tab, lt_verpo_tab, lt_prot, lt_return, lt_messages, lt_header, lt_items, lt_venum.
      CLEAR: lv_err.


      LOOP AT gt_hu INTO DATA(ls_hu_src).

        CLEAR: ls_headerproposal, ls_itemprop, ls_huheader, lv_hukey, ls_vekp,
        ls_verko_tab, ls_verpo_tab.
        CLEAR: lt_return, lt_itemprop, lt_huitems.
        REFRESH: lt_return, lt_itemprop, lt_huitems.

        ls_headerproposal-pack_mat = lc_pack_mat.
        ls_headerproposal-plant    = gs_xlips-werks.

        "Crear HU
        CALL FUNCTION 'BAPI_HU_CREATE'
          EXPORTING
            headerproposal = ls_headerproposal
          IMPORTING
            huheader       = ls_huheader
            hukey          = lv_hukey
          TABLES
            return         = lt_return.

        IF lv_hukey IS INITIAL
        OR line_exists( lt_return[ type = 'E' ] )
        OR line_exists( lt_return[ type = 'A' ] )
        OR line_exists( lt_return[ type = 'X' ] ).

          _control_set_error( ).
          RETURN.
        ENDIF.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        "Asociar cabecera de HU con la inbound delivery
        REFRESH: lt_return.

        ls_huheader-pack_mat_object  = lc_vpobj_inb.
        ls_huheader-pack_mat_obj_key = gs_log-documento_oc.

        CALL FUNCTION 'BAPI_HU_CHANGE_HEADER'
          EXPORTING
            hukey     = lv_hukey
            huchanged = ls_huheader
          IMPORTING
            huheader  = ls_huheader
          TABLES
            return    = lt_return.

        IF line_exists( lt_return[ type = 'E' ] )
        OR line_exists( lt_return[ type = 'A' ] )
        OR line_exists( lt_return[ type = 'X' ] ).

          _control_set_error( ).
          RETURN.
        ENDIF.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        SELECT SINGLE * FROM vekp
        INTO @ls_vekp
        WHERE exidv = @lv_hukey.

        IF sy-subrc <> 0.
          _control_set_error( ).
          RETURN.
        ENDIF.

        MOVE-CORRESPONDING ls_vekp TO ls_verko_tab.
        ls_verko_tab-object = lc_vpobj_inb.
        ls_verko_tab-objkey = gs_log-documento_oc.
        ls_verko_tab-ernam  = sy-uname.
        APPEND ls_verko_tab TO lt_verko_tab.

        CLEAR ls_verpo_tab.

        " HU destino.
        ls_verpo_tab-exidv_ob = ls_vekp-exidv.
        ls_verpo_tab-venum_ob = ls_vekp-venum.
        ls_verpo_tab-velin = lc_velin_mat.
        ls_verpo_tab-vbeln = gs_log-documento_oc.
        ls_verpo_tab-posnr = gs_xlips-posnr.
        ls_verpo_tab-rfbel = gs_log-documento_oc.
        ls_verpo_tab-rfpos = gs_xlips-posnr.
        " Datos materiales.
        ls_verpo_tab-matnr = gs_xlips-matnr.
        ls_verpo_tab-werks = gs_xlips-werks.
        ls_verpo_tab-lgort = gs_xlips-lgort.
        ls_verpo_tab-charg = gs_xlips-charg.
        ls_verpo_tab-tmeng = ls_hu_src-vemng.
        ls_verpo_tab-vrkme = gt_hu[ 1 ]-meins.
        APPEND ls_verpo_tab TO lt_verpo_tab.
      ENDLOOP.

      IF lt_verko_tab IS INITIAL OR lt_verpo_tab IS INITIAL.

        _control_set_error( ).
        RETURN.
      ENDIF.

      "Actualizar la delivery UNA SOLA VEZ con todas las HUs
      ls_vbkok-vbeln    = gs_log-documento_oc.
      ls_vbkok-vbeln_vl = gs_log-documento_oc.
      ls_vbkok-vbtyp_vl = gs_xlikp-vbtyp.

      CALL FUNCTION 'HU_PACKING_REFRESH'.

      REFRESH lt_v51vp.

      CALL FUNCTION 'HU_READ_DELIVERY_AND_INIT'
        EXPORTING
          if_delivery       = gs_log-documento_oc
          if_lock           = abap_true
        TABLES
          et_v51vp          = lt_v51vp
        EXCEPTIONS
          no_delivery_found = 1
          posted            = 2
          display_only      = 3
          error_message     = 99
          OTHERS            = 100.

      IF sy-subrc <> 0.
        ROLLBACK WORK.
        _control_set_error( ).
        RETURN.
      ENDIF.

      REFRESH lt_venum.

      LOOP AT lt_verpo_tab INTO ls_verpo_tab.

        APPEND ls_verpo_tab-venum_ob TO lt_venum.

      ENDLOOP.

      REFRESH: lt_header, lt_items, lt_messages.

      CALL FUNCTION 'HU_GET_HUS'
        EXPORTING
          if_lock_hus = abap_true
          it_venum    = lt_venum
        IMPORTING
          et_header   = lt_header
          et_items    = lt_items
          et_messages = lt_messages
        EXCEPTIONS
          hus_locked  = 1
          no_hu_found = 2
          fatal_error = 3
          OTHERS      = 4.

      IF sy-subrc <> 0
      OR line_exists( lt_messages[ msgty = 'E' ] )
      OR line_exists( lt_messages[ msgty = 'A' ] )
      OR line_exists( lt_messages[ msgty = 'X' ] ).

        DATA:
          lv_ws_subrc TYPE sy-subrc,
          lv_msgid    TYPE symsgid,
          lv_msgno    TYPE symsgno,
          lv_msgty    TYPE symsgty,
          lv_msgv1    TYPE symsgv,
          lv_msgv2    TYPE symsgv,
          lv_msgv3    TYPE symsgv,
          lv_msgv4    TYPE symsgv,
          lv_msg      TYPE string.
        lv_msgid = sy-msgid.
        lv_msgno = sy-msgno.
        lv_msgty = sy-msgty.
        lv_msgv1 = sy-msgv1.
        lv_msgv2 = sy-msgv2.
        lv_msgv3 = sy-msgv3.
        lv_msgv4 = sy-msgv4.

        MESSAGE ID lv_msgid
        TYPE 'S'
        NUMBER lv_msgno
        WITH lv_msgv1
        lv_msgv2
        lv_msgv3
        lv_msgv4
        INTO lv_msg.

        ROLLBACK WORK.
        _control_set_error( ).
        RETURN.
      ENDIF.

      LOOP AT lt_verpo_tab INTO ls_verpo_tab.

        CLEAR: ls_header, ls_packing_request, lv_rcode.

        " Buscar cabecera interna correspondiente a esta HU
        READ TABLE lt_header INTO ls_header WITH KEY venum = ls_verpo_tab-venum_ob.

        IF sy-subrc <> 0.
          ROLLBACK WORK.
          _control_set_error( ).
          RETURN.
        ENDIF.

        DATA(lv_header_index) = sy-tabix.

        ls_packing_request-venum    = ls_verpo_tab-venum_ob.
        ls_packing_request-exidv    = ls_verpo_tab-exidv_ob.
        ls_packing_request-velin    = '1'.
        ls_packing_request-veanz    = 1.
        ls_packing_request-belnr    = gs_log-documento_oc.
        ls_packing_request-posnr    = gs_xlips-posnr.

        ls_packing_request-quantity = ls_verpo_tab-tmeng.

        ls_packing_request-meins    = gs_xlips-meins.
        ls_packing_request-altme    = gs_xlips-meins.

        " Datos adicionales del material
        ls_packing_request-matnr    = gs_xlips-matnr.
        ls_packing_request-werks    = gs_xlips-werks.
        ls_packing_request-charg    = gs_xlips-charg.


        CALL FUNCTION 'HU_PACKING_AND_UNPACKING'
          EXPORTING
            is_packing_request = ls_packing_request
          IMPORTING
            ef_rcode           = lv_rcode
          CHANGING
            cs_header          = ls_header
          EXCEPTIONS
            missing_data       = 1
            hu_not_changeable  = 2
            not_possible       = 3
            customizing        = 4
            weight             = 5
            volume             = 6
            serial_nr          = 7
            fatal_error        = 8
            OTHERS             = 9.

        IF sy-subrc <> 0
        OR lv_rcode <> 0.

          ROLLBACK WORK.
          _control_set_error( ).
          RETURN.

        ENDIF.

        MODIFY lt_header FROM ls_header INDEX lv_header_index.

      ENDLOOP.

      CLEAR ls_object.

      ls_object-object = lc_vpobj_inb.
      ls_object-objkey = gs_log-documento_oc.

      REFRESH: lt_messages.
      CLEAR lv_number.

      CALL FUNCTION 'HU_POST'
        EXPORTING
          if_synchron = abap_true
          if_commit   = space
          is_object   = ls_object
        IMPORTING
          ef_number   = lv_number
          et_messages = lt_messages
          et_header   = lt_header
          et_items    = lt_items.

      IF line_exists( lt_messages[ msgty = 'E' ] )
      OR line_exists( lt_messages[ msgty = 'A' ] )
      OR line_exists( lt_messages[ msgty = 'X' ] ).

        ROLLBACK WORK.
        _control_set_error( ).
        RETURN.

      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.


    ELSE.

      IF 1 = 1.
        REFRESH: gt_messages, gt_bdcdata.
        CLEAR: gs_message.

        " Pantalla inicial
        APPEND VALUE #( program = 'SAPMV50A'  dynpro = '4104' dynbegin = abap_true ) TO gt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_CURSOR'   fval = 'LIKP-VBELN' )                  TO gt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE'   fval = '=VERP_T' )                     TO gt_bdcdata.
        APPEND VALUE #( fnam = 'LIKP-VBELN'   fval = gs_log-documento_oc )           TO gt_bdcdata.

        " Pantalla embalado
        APPEND VALUE #( program = 'SAPLV51G' dynpro = '6000' dynbegin = abap_true ) TO gt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE'  fval = '=ENTR' )                              TO gt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_SUBSCR'  fval = 'SAPLV51G                          6010TAB' ) TO gt_bdcdata.

        DATA lv_index TYPE n LENGTH 2.
        lv_index = sy-tabix.
        LOOP AT gt_hu INTO ls_hu.
          lv_index = sy-tabix.
          ls_hu-vhilm = 'FASS'. "DEU8C8E 28.06.2026
          APPEND VALUE #( fnam = 'BDC_CURSOR'
                          fval = |V51VE-VHILM({ lv_index })| ) TO gt_bdcdata.
          APPEND VALUE #( fnam = |V51VE-VHILM({ lv_index })|
                          fval = ls_hu-vhilm )                  TO gt_bdcdata.
*      APPEND VALUE #( fnam = |V51VE-SELKZ({ lv_index })|
*                      fval = 'X' )                          TO gt_bdcdata.
*      APPEND VALUE #( fnam = |V51VP-SELKZ({ lv_index })|
*                      fval = 'X' )                          TO gt_bdcdata.
          CLEAR:ls_hu.
        ENDLOOP.

        CLEAR: lv_index.
        DATA lv_vemng TYPE char30.
        LOOP AT gt_hu INTO ls_hu.
          lv_vemng = |{ ls_hu-vemng }|.
          CONDENSE lv_vemng NO-GAPS.
          REPLACE ALL OCCURRENCES OF '.' IN lv_vemng WITH ','.
          lv_index = |{ sy-tabix ALIGN = RIGHT WIDTH = 2 PAD = '0' }|.
          APPEND VALUE #( program = 'SAPLV51G' dynpro = '6000' dynbegin = abap_true ) TO gt_bdcdata.
          APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ENTR' )             TO gt_bdcdata.
          APPEND VALUE #( fnam = 'BDC_SUBSCR' fval = 'SAPLV51G                          6010TAB' ) TO gt_bdcdata.
          APPEND VALUE #( fnam = 'BDC_CURSOR'   fval = 'V51VP-TMENG(01)' )                  TO gt_bdcdata.
          APPEND VALUE #( fnam = 'V51VP-TMENG(01)'   fval = lv_vemng )                  TO gt_bdcdata.

          APPEND VALUE #( program = 'SAPLV51G' dynpro = '6000' dynbegin = abap_true ) TO gt_bdcdata.
          APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=HU_VERP' )             TO gt_bdcdata.
          APPEND VALUE #( fnam = 'BDC_SUBSCR' fval = 'SAPLV51G                          6010TAB' ) TO gt_bdcdata.
          APPEND VALUE #( fnam = 'BDC_CURSOR'   fval = 'V51VP-MATNR(01)' )                  TO gt_bdcdata.
          APPEND VALUE #( fnam = |V51VE-SELKZ({ lv_index })|   fval = 'X' )                  TO gt_bdcdata.
          APPEND VALUE #( fnam = 'V51VP-SELKZ(01)'   fval = 'X' )                  TO gt_bdcdata.
          CLEAR:ls_hu.
        ENDLOOP.

        APPEND VALUE #( program = 'SAPLV51G' dynpro = '6000' dynbegin = abap_true ) TO gt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE'          fval = '=SICH' )                              TO gt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_SUBSCR'          fval = 'SAPLV51G                          6010TAB' ) TO gt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_CURSOR'          fval = 'V51VE-EXIDV(01)' )                    TO gt_bdcdata.


        DATA(lv_mode) = gc_mode.
        CALL TRANSACTION 'VL32N' USING gt_bdcdata MODE lv_mode UPDATE 'S' MESSAGES INTO gt_messages.

      ELSE.

********************************************" SERGI PLAN A
*    "Sustitución BI por BAPIs
*
*    DATA ls_headerproposal TYPE bapihuhdrproposal.
*    DATA lt_return TYPE TABLE OF bapiret2.
*    DATA lv_hukey TYPE bapihukey-hu_exid.
*    DATA lt_verko TYPE TABLE OF  verko.
*    DATA lt_verpo TYPE TABLE OF  verpo.
*    DATA ls_verpo TYPE verpo.
*    DATA lt_prot TYPE TABLE OF prott.
*    DATA lv_vemng TYPE char30.
*    DATA lv_index TYPE n LENGTH 2.
*
*    CLEAR: lv_index.
*
*    LOOP AT gt_hu INTO DATA(ls_hu).
*
*      CLEAR: ls_headerproposal, lv_hukey, ls_verpo.
*      REFRESH: lt_return, lt_verko, lt_verpo, lt_prot.
*
**      ls_headerproposal-pack_mat = ls_hu-vhilm.
*      ls_headerproposal-pack_mat = 'FASS'.
*
*      CALL FUNCTION 'BAPI_HU_CREATE'
*        EXPORTING
*          headerproposal = ls_headerproposal
*        IMPORTING
**         HUHEADER       =
*          hukey          = lv_hukey
*        TABLES
**         ITEMSPROPOSAL  =
**         ITEMSSERIALNO  =
*          return         = lt_return
**         HUITEM         =
**         CWM_ITEMSPROPOSAL       =
**         CWM_HUITEM     =
*        .
*
*      lv_vemng = |{ ls_hu-vemng }|.
**      CONDENSE lv_vemng NO-GAPS.
**      REPLACE ALL OCCURRENCES OF '.' IN lv_vemng WITH ','.
**      lv_index = |{ sy-tabix ALIGN = RIGHT WIDTH = 2 PAD = '0' }|.
*
**      ls_verpo-exidv_ob = lv_hukey.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = lv_hukey  " El número de 10 posiciones de la BAPI_HU_CREATE
*        IMPORTING
*          output = ls_verpo-exidv_ob.
*
*      ls_verpo-velin = 1.
*      ls_verpo-vbeln = gs_log-documento_oc.
*      ls_verpo-posnr = '000010'.
*      ls_verpo-tmeng = lv_vemng.
*      ls_verpo-matnr = 'W0WHT000868'.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = ls_verpo-matnr
*        IMPORTING
*          output = ls_verpo-matnr.
*
*
*      ls_verpo-werks = '4900'.
*      ls_verpo-lgort = 'OC01'.
*
*      APPEND ls_verpo TO lt_verpo.
*
*      CALL FUNCTION 'SD_DELIVERY_UPDATE_PACKING'
*        EXPORTING
*          delivery  = gs_log-documento_oc
*          commit    = ' '
*          synchron  = ' '
*        TABLES
*          verko_tab = lt_verko
*          verpo_tab = lt_verpo
*          prot      = lt_prot
**     EXCEPTIONS
**         UPDATE_NOT_POSSIBLE       = 1
**         OTHERS    = 2
*        .
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.
*
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait = 'X'.
*
*
*    ENDLOOP.


********************************************" JOSEP PLAN B
**
*REFRESH gt_messages.
*  CLEAR   gs_message.
*
*  DATA: lv_vbeln  TYPE vbeln_vl,
*        lv_venum  TYPE venum,
*        lv_exidv  TYPE exidv,
*        lt_return TYPE bapiret2_t.
*
*  lv_vbeln = gs_log-documento_oc.
*
*  " 1. Inicializar el buffer de embalaje
*  CALL FUNCTION 'HU_PACKING_REFRESH'.
*
*  " 2. Cargar la entrega en el buffer de embalaje
*  CALL FUNCTION 'HU_GET_HUS_TO_DELIVERY'
*    EXPORTING
*      if_vbeln    = lv_vbeln
*    EXCEPTIONS
*      not_found   = 1
*      input_error = 2
*      OTHERS      = 3.
*  IF sy-subrc <> 0.
**    APPEND VALUE #( type = 'E' id = sy-msgid number = sy-msgno
**                    message_v1 = sy-msgv1 message_v2 = sy-msgv2
**                    message_v3 = sy-msgv3 message_v4 = sy-msgv4 ) TO gt_messages.
*    RETURN.
*  ENDIF.
*
*  " 3 + 4. Por cada HU: crear la HU y embalar su posición/cantidad
*  LOOP AT gt_hu INTO ls_hu.
*
*    " 3. Crear la HU con el material de embalaje FASS
*    CLEAR: lv_venum, lv_exidv.
*    CALL FUNCTION 'HU_CREATE_HU'
*      EXPORTING
*        if_vhilm  = 'FASS'              " material de embalaje
*        if_werks  = '4900'       " centro
**        if_lgort  = ls_hu-lgort         " almacén (si aplica)
*      IMPORTING
*        ef_venum  = lv_venum            " nº interno de HU creada
*        ef_exidv  = lv_exidv            " nº externo de HU
*      EXCEPTIONS
*        error     = 1
*        OTHERS    = 2.
*    IF sy-subrc <> 0.
**      APPEND VALUE #( type = 'E' id = sy-msgid number = sy-msgno
**                      message_v1 = sy-msgv1 message_v2 = sy-msgv2
**                      message_v3 = sy-msgv3 message_v4 = sy-msgv4 ) TO gt_messages.
*      CALL FUNCTION 'HU_PACKING_REFRESH'.   " limpiar buffer ante error
*      RETURN.
*    ENDIF.
*
*    " 4. Embalar la posición de entrega en la HU recién creada
*    CALL FUNCTION 'HU_PACK_ONE_ITEM'
*      EXPORTING
*        if_venum    = lv_venum          " HU destino
*        if_vbeln    = lv_vbeln          " entrega
*        if_posnr    = ls_hu-posnr       " posición de entrega
*        if_quantity = ls_hu-vemng       " cantidad a embalar
*        if_meins    = ls_hu-meins       " unidad de medida
*      EXCEPTIONS
*        error       = 1
*        OTHERS      = 2.
*    IF sy-subrc <> 0.
*      APPEND VALUE #( type = 'E' id = sy-msgid number = sy-msgno
*                      message_v1 = sy-msgv1 message_v2 = sy-msgv2
*                      message_v3 = sy-msgv3 message_v4 = sy-msgv4 ) TO gt_messages.
*      CALL FUNCTION 'HU_PACKING_REFRESH'.
*      RETURN.
*    ENDIF.
*
*  ENDLOOP.
*
*  " 5. Traspasar el embalaje a la entrega (en memoria)
*  CALL FUNCTION 'HU_POST'
*    EXCEPTIONS
*      error  = 1
*      OTHERS = 2.
*  IF sy-subrc <> 0.
*    APPEND VALUE #( type = 'E' id = sy-msgid number = sy-msgno
*                    message_v1 = sy-msgv1 message_v2 = sy-msgv2
*                    message_v3 = sy-msgv3 message_v4 = sy-msgv4 ) TO gt_messages.
*    CALL FUNCTION 'HU_PACKING_REFRESH'.
*    RETURN.
*  ENDIF.
*
*  " 6. Grabar la entrega
*  CALL FUNCTION 'WS_DELIVERY_UPDATE_2'
*    EXPORTING
*      vbkok_wa  = VALUE vbkok( vbeln_vl = lv_vbeln )
*      commit    = abap_false              " controlamos el commit nosotros
*      delivery  = lv_vbeln
*    TABLES
*      prot      = gt_messages.
*
*  READ TABLE gt_messages TRANSPORTING NO FIELDS WITH KEY type = 'E'.
*  IF sy-subrc = 0.
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*  ELSE.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = abap_true.
*  ENDIF.


********************************************" JOSEP PLAN C
        " REVISAR INCLUDE /VWK/LZF0WM_MOMAF01
        "Refresca estructuras HU

*        DATA: lt_messages TYPE STANDARD TABLE OF huitem_messages,
*              lt_items    TYPE huitm_prop,
*              lt_vepo     TYPE hum_hu_item_t.
*        DATA: ls_header      TYPE huhdr_proposal,
*              ls_items       TYPE huitm_proposal,
*              ls_vekp        TYPE vekpvb,
*              ls_plant_stloc TYPE hum_plant_stloc,
*              ls_messages    TYPE huitem_messages.
*        DATA: ls_params TYPE /vwk/zf0crtkgim3,
*              lt_params TYPE STANDARD TABLE OF /vwk/zf0crtkgim3.
*        DATA: "l_lgnum TYPE lgnum,
*          l_lgtyp TYPE lgtyp,
*          l_lgpla TYPE lgpla,
*          l_delay TYPE i,
*          l_id    TYPE zid,
*          l_value TYPE zvalue,
*          l_error TYPE flag.
*
*        CALL FUNCTION 'HU_PACKING_REFRESH'.
*        FREE lt_messages.
*
*        CLEAR ls_plant_stloc.
*        ls_plant_stloc-plant = '4900'.                      "0501
*        ls_plant_stloc-move_stloc = gc_lgnum.
*
*        ls_plant_stloc-stge_loc = l_value(4).                   "230 ó 228
*
*
*        "Inicializa empaquetado antes de crear HU de picking que incluya items
*        CALL FUNCTION 'HU_INITIALIZE_PACKING'
*          EXPORTING
*            if_process     = 'B'
*            is_plant_stloc = ls_plant_stloc
*          IMPORTING
*            et_messages    = lt_messages[]
*          EXCEPTIONS
*            not_possible   = 1
*            OTHERS         = 2.


*   l_lgtyp = l_value(3).                        "902 ó 906
*   l_lgpla = l_value(10).                       "RFID
*
*
*      "Datos CABECERA
*      ls_header-exidv   = is_hu_data-matric.
*      ls_header-exida   = 'C'.
*      ls_header-vhilm   = is_hu_data-tipcon.
*
*      "Datos POSICIÓN
*      FREE lt_items.
*      CLEAR ls_items.
*      ls_items-exidv    = is_hu_data-matric.
*      ls_items-velin    = '1'.
*      ls_items-quantity = is_hu_data-menge.
*      ls_items-matnr    = is_hu_data-matnr.
*      ls_items-werks    = ls_plant_stloc-plant.
*      ls_items-lgort    = ls_plant_stloc-stge_loc.
*      ls_items-umlgo    = ls_plant_stloc-move_stloc.
*      ls_items-lgtyp    = l_lgtyp.
*      ls_items-lgpla    = l_lgpla.
*      ls_items-wdatu    = is_hu_data-fecini.
*      APPEND ls_items TO lt_items.
*
*      "Creación HU
*      FREE lt_messages.
*      CALL FUNCTION 'HU_CREATE_ONE_HU'
*        EXPORTING
*          if_create_hu       = 'X'
*          is_header_proposal = ls_header
*          it_items           = lt_items[]
*        IMPORTING
*          es_header          = ls_vekp
*          et_items           = lt_vepo[]
*          et_messages        = lt_messages[]
*        EXCEPTIONS
*          input_missing      = 1
*          not_possible       = 2
*          header_error       = 3
*          item_error         = 4
*          serial_nr_error    = 5
*          fatal_error        = 6
*          OTHERS             = 7.
*
*      "Graba HU en BD
*      FREE lt_messages.
*      CALL FUNCTION 'HU_POST'
*        EXPORTING
*          if_synchron   = 'X'
*          if_commit     = 'X'
*          if_no_refresh = space
*        IMPORTING
*          et_messages   = lt_messages[].

      ENDIF.
    ENDIF.

    " POR ULTIMO CREA LA OT, se puede ver.
  ENDMETHOD.

*******************************************  

  METHOD _sumin_outb_delv_contab.

    CONSTANTS: lc_pstyv TYPE pstyv_vl VALUE 'ZELP',
               lc_lgort TYPE lgort_d  VALUE 'CON'.

    DATA: ls_vbkok TYPE vbkok,
          lt_vbpok TYPE STANDARD TABLE OF vbpok,
          lt_prot  TYPE STANDARD TABLE OF prott.

    SELECT  *
      INTO TABLE @DATA(lt_lips)
      FROM lips
      WHERE vbeln = @gs_log-documento_oc
        AND pstyv = @lc_pstyv.

    IF sy-subrc <> 0. _control_set_error( ). RETURN. ENDIF.

    ls_vbkok-vbeln_vl = gs_log-documento_oc.

    LOOP AT lt_lips INTO DATA(ls_lips).
      APPEND VALUE #(
                      vbeln_vl = gs_log-documento_oc
                      posnr_vl = ls_lips-posnr
                      kzlgo    = abap_true
                      werks    = ls_lips-werks
                      lgort    = lc_lgort
                      xwmpp    = abap_true
                      lgpla    = ls_lips-lgpla
                      lgtyp    = ls_lips-lgtyp
                    ) TO lt_vbpok.
    ENDLOOP.

    CALL FUNCTION 'WS_DELIVERY_UPDATE'
      EXPORTING
        vbkok_wa  = ls_vbkok
        delivery  = gs_log-documento_oc
        commit    = abap_true
      TABLES
        vbpok_tab = lt_vbpok
        prot      = lt_prot
      EXCEPTIONS
        OTHERS    = 1.

    IF line_exists( lt_prot[ 1 ] ) OR NOT sy-subrc = 0.
      _control_set_error( ).
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDMETHOD.

***********************************************************  

  METHOD _sumin_outb_delv_sm.

    DATA: ls_vbkok TYPE vbkok,
          lt_prot  TYPE TABLE OF prott,
          lv_err   TYPE char1.
    DATA: lv_vbeln TYPE vbeln_vl.


    CLEAR ls_vbkok.
    ls_vbkok-vbeln_vl = gs_log-documento_oc.
    ls_vbkok-vbeln    = gs_log-documento_oc.
    ls_vbkok-wabuc    = 'X'.
    ls_vbkok-wadat_ist = sy-datum.

    DATA(lv_task_sm) = 'OC_SUMIN_TASK_SM' && sy-datum && sy-uzeit.

    CALL FUNCTION 'WS_DELIVERY_UPDATE_2'
      STARTING NEW TASK lv_task_sm
      DESTINATION IN GROUP DEFAULT
      EXPORTING
        vbkok_wa       = ls_vbkok
        delivery       = lv_vbeln
        update_picking = abap_true
        synchron       = abap_true
        commit         = abap_true
      TABLES
        prot           = lt_prot
      EXCEPTIONS
        error_message  = 1
        OTHERS         = 2.

    WAIT UP TO 3 SECONDS.

    IF NOT _sumin_outb_delv_sm_check( ). _control_set_error( ). RETURN. ENDIF.

  ENDMETHOD.

***********************************************************
  METHOD _sumin_outb_delv_sm_check.
    CONSTANTS: lc_wbstk_c TYPE likp-wbstk VALUE 'C'.

    SELECT SINGLE @abap_true
      FROM likp
      INTO @DATA(lv_existe)
      WHERE vbeln = @gs_log-documento_oc
      AND wbstk = @lc_wbstk_c.

    IF sy-subrc = 0. rv_return = abap_true. ENDIF.

  ENDMETHOD.

****************************************************  
  METHOD _sumin_transfer_order.
    CONSTANTS: lc_bwlvs TYPE bwlvs VALUE '998'.

    DATA: lv_tanum           TYPE tanum,
          lv_lenum           TYPE lenum,
          lv_nlpla           TYPE ltap_nlpla,
          lv_nltyp           TYPE ltap_nltyp,
          lv_tanum_formatted TYPE char12,
          mtext              TYPE string,
          lv_index           TYPE sy-tabix.

    REFRESH: gt_messages, gt_bdcdata.
    CLEAR: gs_message, lv_index.

    _sumin_transfer_order_aux_hu( ).
    CHECK gt_hu_oc IS NOT INITIAL.



    lv_nlpla = gs_stock-ubica && gs_stock-grulin.
    lv_nltyp = gs_stock-lgtyp.

    LOOP AT gt_hu_oc ASSIGNING FIELD-SYMBOL(<fs_hu_oc>).
      lv_lenum = <fs_hu_oc>-exidv.
      lv_index = sy-tabix.

      CALL FUNCTION 'L_TO_CREATE_MOVE_SU'
        EXPORTING
          i_lenum               = lv_lenum
          i_bwlvs               = lc_bwlvs
          i_nltyp               = lv_nltyp
          i_nlpla               = lv_nlpla
          i_commit_work         = abap_true
          i_bname               = sy-uname
        IMPORTING
          e_tanum               = lv_tanum
        EXCEPTIONS
          not_confirmed_to      = 1
          foreign_lock          = 2
          bwlvs_wrong           = 3
          betyp_wrong           = 4
          nltyp_wrong           = 5
          nlpla_wrong           = 6
          nltyp_missing         = 7
          nlpla_missing         = 8
          squit_forbidden       = 9
          lgber_wrong           = 10
          xfeld_wrong           = 11
          drukz_wrong           = 12
          ldest_wrong           = 13
          no_stock_on_su        = 14
          su_not_found          = 15
          update_without_commit = 16
          no_authority          = 17
          benum_required        = 18
          ltap_move_su_wrong    = 19
          lenum_wrong           = 20
          OTHERS                = 21.
      IF sy-subrc = 0.
        IF line_exists( gt_hu[ lv_index ] ).
          gs_log-hu_peticion = gt_hu[ lv_index ]-exidv.
        ENDIF.
        gs_log-hu_oc = <fs_hu_oc>-exidv.
        gs_log-tanum = lv_tanum.
        APPEND gs_log TO gt_log.
      ELSE.
        _control_set_error( ). EXIT.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

***************************************************  

  METHOD _sumin_transfer_order_aux_hu.
    REFRESH: gt_hu_oc.
    SELECT        vekp~meins,
                  vekp~exidv,
                  vepo~vemng,
                  vekp~vhilm
      FROM vekp AS vekp
      INNER JOIN vepo AS vepo ON vepo~venum = vekp~venum
      INTO TABLE @gt_hu_oc
      WHERE vekp~vpobjkey = @gs_log-documento_oc
      AND   vekp~vpobj    = @gc_vpobj_12.
  ENDMETHOD.