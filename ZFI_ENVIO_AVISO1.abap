*&---------------------------------------------------------------------*
*& Report  ZFI0172
*&
*&---------------------------------------------------------------------*
*& Envío de Pagos
*&---------------------------------------------------------------------*
*& AUTORES: Ezequiel Mugico
*& FECHA: 27.08.2008
*&---------------------------------------------------------------------*
*& MODIFICADO POR: Jose Alberto Quiroz Vega
*& FECHA: 06.05.2011
*&---------------------------------------------------------------------*
*&
*&
*&
*&---------------------------------------------------------------------*
REPORT  zfi_envio_aviso1.

************************************************************************
*                                                                      *
*               D A T A   D E C L A R A T I O N                        *
*                                                                      *
************************************************************************

*----------------------------------------------------------------------*
* Estructuras                                                            *
*----------------------------------------------------------------------*
TABLES: reguh,
        regup,
        itcpo,
        t001.

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
TYPE-POOLS: slis.
TYPES: BEGIN OF ty_salida,
         estado TYPE icon_int,
         lifnr  TYPE lfa1-lifnr,
         name   TYPE lfa1-name1,
         smtp   TYPE adr6-smtp_addr,
       END OF ty_salida.

DATA: BEGIN OF tab_ausgabe OCCURS 3,   "Spoolnummern der Formulare und
        filename(47) TYPE c,              "Listen und Filename bei DTA
        renum        LIKE regut-renum,    "spool numbers of forms and lists
        dataset      LIKE sy-prdsn,       "and file names (DME)
        name(35)     TYPE c,
        spoolnr      LIKE sy-sponr,
        immed(1)     TYPE c,
        error(1)     TYPE c,
        count        TYPE i,
      END OF tab_ausgabe.

TYPES: BEGIN OF ty_adr6,
         addrnumber TYPE adr6-addrnumber,
         persnumber TYPE adr6-persnumber,
         date_from  TYPE adr6-date_from,
         consnumber TYPE adr6-consnumber,
         smtp_addr  TYPE adr6-smtp_addr,
       END OF ty_adr6.

DATA: t_adr6  TYPE TABLE OF ty_adr6,
      wa_adr6 TYPE ty_adr6.

*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*
CONSTANTS: gc_icon_ko TYPE icon_int VALUE '@S_TL_R@',
           gc_icon_ok TYPE icon_int VALUE '@S_TL_G@'.

*----------------------------------------------------------------------*
* Variables                                                            *
*----------------------------------------------------------------------*
DATA: gv_error  TYPE xfeld,
      gv_nosmtp TYPE i.

*Marta Vall: 13.10.2008 - inicio modificación
DATA: d_subrc       TYPE sy-subrc,
      l_rbetr       TYPE reguh-rbetr,
      importe_total TYPE reguh-rbetr,
      subtotal      TYPE regup-wrbtr,   "Subtotal
      idx(2)        TYPE n VALUE 0,     "Cantidad de facturas en una pàgina
      butxt         LIKE t001-butxt.        "Nombre sociedad emisora
DATA: importe TYPE wrbtr.

DATA: var1 TYPE string,
      var2 TYPE string,
      var3 TYPE string,
      itab TYPE TABLE OF string.

DATA: email     TYPE adr6-smtp_addr,    "Email
      sort_code TYPE lfbk-bankl,        "Sort Code
      account   TYPE lfbk-bankn.        "Account
*Marta Vall: 13.10.2008 - fin modificación

*----------------------------------------------------------------------*
* Estructuras / Areas de trabajo                                       *
*----------------------------------------------------------------------*
DATA: lw_packing_list  TYPE sopcklsti1,
      lw_document_data TYPE sodocchgi1,
      lw_destinatarios TYPE somlreci1,
      lw_texto         TYPE solisti1.
DATA: lt_packing_list  LIKE TABLE OF lw_packing_list,
      lt_destinatarios LIKE TABLE OF lw_destinatarios,
      wa_dest          LIKE lw_destinatarios,
      lt_texto         LIKE TABLE OF lw_texto WITH HEADER LINE.
DATA: gw_salida TYPE ty_salida.
DATA: BEGIN OF gw_pool,
        lifnr TYPE lfb1-lifnr,
        vblnr TYPE reguh-vblnr,
        smtp  TYPE adr6-smtp_addr,
        pdf   TYPE TABLE OF solisti1,
      END OF gw_pool.

*----------------------------------------------------------------------*
* Tablas Internas                                                      *
*----------------------------------------------------------------------*
DATA: gt_reguh  LIKE TABLE OF reguh,
      gt_regup  LIKE TABLE OF regup,
      gt_pool   LIKE TABLE OF gw_pool,
      gt_salida LIKE TABLE OF gw_salida,
      wa_regup  TYPE regup,
      wa_reguh  TYPE reguh.

DATA: BEGIN OF lt_xblnr OCCURS 0,
        laufi LIKE regup-laufi,
        xblnr LIKE regup-xblnr,
        bukrs LIKE regup-zbukr,
        xvorl LIKE regup-xvorl,
        vblnr LIKE regup-vblnr,
        belnr LIKE regup-belnr,
        blart LIKE regup-blart,
        bschl LIKE regup-bschl,
        laufd LIKE regup-laufd,
        wrbtr LIKE regup-wrbtr,
        bldat LIKE regup-bldat,
*Marta Vall: 13.10.2008 - inicio modificación
        ltext LIKE t003t-ltext,
        shkzg LIKE regup-shkzg,
*Marta Vall: 13.10.2008 - fin modificación
*Marta Vall: 27.10.2008 - inicio modificación
        hbkid LIKE regup-hbkid,
        empfk LIKE lfza-empfk,
        lifnr LIKE lfza-lifnr,
        zlsch LIKE regup-zlsch,
        rzawe LIKE reguh-rzawe,
        rwbtr LIKE reguh-rwbtr,
        absbu LIKE reguh-absbu,
*Marta Vall: 27.10.2008 - fin modificación
*** INICIO MODIFICACIÓN EMG 11/11/2008
        sgtxt LIKE regup-sgtxt,
*** FIN MODIFICACIÓN EMG 11/11/2008
      END OF lt_xblnr.

*Marta Vall: 27.10.2008 - inicio modificación
DATA: BEGIN OF lt_importe OCCURS 0,

        bukrs LIKE bseg-bukrs,
        belnr LIKE bseg-belnr,
        gjahr LIKE bseg-gjahr,
        buzei LIKE bseg-buzei,
        fdwbt LIKE bseg-fdwbt,
        wrbtr LIKE bseg-wrbtr,
        bschl LIKE bseg-bschl,
        augbl LIKE bseg-augbl,
      END OF lt_importe.
*Marta Vall: 27.10.2008 - fin modificación
*----------------------------------------------------------------------*
* Clases                                                               *
*----------------------------------------------------------------------*
DATA: rf_alv     TYPE REF TO cl_salv_table,
      rf_func    TYPE REF TO cl_salv_functions,
      rf_columns TYPE REF TO cl_salv_columns_table,
      rf_column  TYPE REF TO cl_salv_column_table.

*----------------------------------------------------------------------*
* Selection Screen, Parameters y, Select-Options                       *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bloque1 WITH FRAME TITLE TEXT-ss1.
PARAMETERS: p_laufd LIKE reguh-laufd OBLIGATORY,
            p_laufi LIKE reguh-laufi OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bloque1.
*** INICIO MODIFICACIÓN EMG 09/12/2008
SELECTION-SCREEN BEGIN OF BLOCK bloque2 WITH FRAME TITLE TEXT-ss2.
PARAMETERS: p_correo TYPE c RADIOBUTTON GROUP rb_1 DEFAULT 'X' USER-COMMAND rbt,
            p_printr TYPE c RADIOBUTTON GROUP rb_1.
*            p_impres TYPE c RADIOBUTTON GROUP rb_1.
PARAMETERS: p_print  TYPE tsp03-padest NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK bloque2.
*** FIN MODIFICACIÓN EMG 09/12/2008
DATA: p_impres TYPE c.
************************************************************************
*                                                                      *
*                             E V E N T S                              *
*                                                                      *
************************************************************************

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN.                                                 *
*----------------------------------------------------------------------*
* Validación de campos de la pantalla de selección                     *
*----------------------------------------------------------------------*
*** INICIO MODIFICACIÓN EMG 09/12/2008
AT SELECTION-SCREEN OUTPUT.
  PERFORM f_modifica_visual.
*** FIN MODIFICACIÓN EMG 09/12/2008

AT SELECTION-SCREEN.
  PERFORM f_chequea_campos.

*----------------------------------------------------------------------*
* INITIALIZATION                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
* Inicialización de datos globales
  PERFORM f_inicializacion.

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Selección de datos
  PERFORM f_seleccion_datos.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Informa que no todos tienen dirección
*** INICIO MODIFICACIÓN EMG 09/12/2008
*  IF gv_error IS INITIAL.
  IF NOT gv_nosmtp IS INITIAL AND
     NOT p_correo  IS INITIAL.
*** FIN MODIFICACIÓN EMG 09/12/2008
    PERFORM f_informa_nosmtp.
  ENDIF.
* Prepara formularios
  IF gv_error IS INITIAL.
    PERFORM f_prepara_formularios.
  ENDIF.
* Envia formularios
*** INICIO MODIFICACIÓN EMG 09/12/2008
*  IF gv_error IS INITIAL.
  IF gv_error IS INITIAL." AND
*     NOT p_correo IS INITIAL.
*** FIN MODIFICACIÓN EMG 09/12/2008

    PERFORM f_envia_formularios.
  ENDIF.
* Imprimimos el resultado
**** INICIO MODIFICACIÓN EMG 09/12/2008
*  IF NOT p_correo IS INITIAL.
**** FIN MODIFICACIÓN EMG 09/12/2008
*    PERFORM f_imprime_salida.
**** INICIO MODIFICACIÓN EMG 09/12/2008
*  ENDIF.
*** FIN MODIFICACIÓN EMG 09/12/2008

* Liberar la memoria
  PERFORM f_liberar.

*----------------------------------------------------------------------*
* TOP-OF-PAGE                                                          *
*----------------------------------------------------------------------*
TOP-OF-PAGE.

*----------------------------------------------------------------------*
* END-OF-PAGE                                                          *
*----------------------------------------------------------------------*
END-OF-PAGE.

*----------------------------------------------------------------------*
* TOP-OF-PAGE DURING LINE-SELECTION                                    *
*----------------------------------------------------------------------*
TOP-OF-PAGE DURING LINE-SELECTION.

*----------------------------------------------------------------------*
* AT LINE-SELECTION                                                    *
*----------------------------------------------------------------------*
AT LINE-SELECTION.

*----------------------------------------------------------------------*
* AT USER-COMMAND                                                      *
*----------------------------------------------------------------------*
AT USER-COMMAND.

*----------------------------------------------------------------------*
* AT PFn                                                               *
*----------------------------------------------------------------------*
  AT pfn.

************************************************************************
*                                                                      *
*                             F O R M S                                *
*                                                                      *
************************************************************************

*&---------------------------------------------------------------------*
*&      Form  f_inicializacion
*&---------------------------------------------------------------------*
FORM f_inicializacion.
  CLEAR: reguh,
          regup,
          itcpo,
          t001.
  CLEAR: gv_error,
         gv_nosmtp,
         gw_pool,
         gw_salida,
         lt_xblnr.
  REFRESH: gt_reguh,
           gt_regup,
           gt_pool,
           gt_salida,
           lt_xblnr.
ENDFORM.                    "f_inicializacion

*&---------------------------------------------------------------------*
*&      Form  f_liberar
*&---------------------------------------------------------------------*
FORM f_liberar.
  FREE: gt_reguh,
        gt_regup,
        gt_pool,
        gt_salida,
        lt_xblnr.
ENDFORM.                    "f_liberar

*&---------------------------------------------------------------------*
*&      Form  f_seleccion_datos
*&---------------------------------------------------------------------*
FORM f_seleccion_datos.

  TYPES: BEGIN OF ty_correo,
           smtp_addr TYPE ad_smtpadr.
  TYPES: END OF ty_correo.

*** Inicio CGR 16/12/2010
  DATA: lt_t030 TYPE t030 OCCURS 0,
        ls_t030 TYPE t030.
*** Fin CGR 16/12/2010
* Declaración
  DATA: lv_smtp       TYPE adr6-smtp_addr,
        lv_addrnumber TYPE adr6-addrnumber.
*** INICIO MODIFICACIÓN EMG 11/11/2008
  DATA: lv_land       TYPE t001-land1.
  DATA: BEGIN OF lw_ret,
          hkont TYPE bsis-hkont,
          shkzg TYPE bsis-shkzg,
          wrbtr TYPE bsis-wrbtr,
          qsskz TYPE bsis-qsskz,
          prctr TYPE bsis-prctr,
        END OF lw_ret.
  DATA: lt_ret   LIKE TABLE OF lw_ret,
        lt_regup LIKE TABLE OF regup.
*** FIN MODIFICACIÓN EMG 11/11/2008
*** INICIO MODIFICACIÓN EMG 16/12/2008
  DATA: lt_regup2 LIKE TABLE OF regup.
*** FIN MODIFICACIÓN EMG 16/12/2008

  DATA: it_correo TYPE STANDARD TABLE OF ty_correo,
        wa_correo TYPE ty_correo.

  DATA: lv_recname TYPE so_recname.

* Inicialización
  CLEAR: lv_smtp,
         lv_addrnumber.

* Recogemos los datos de cabecera del pago
  SELECT * FROM reguh INTO TABLE gt_reguh
           WHERE laufd EQ p_laufd
             AND laufi EQ p_laufi
             AND xvorl EQ space.
  IF sy-subrc EQ 0.
*   Recogemos los datos de los clientes
    CLEAR: reguh.
    LOOP AT gt_reguh INTO reguh.
      IF NOT reguh-lifnr IS INITIAL.
        CLEAR: lv_smtp.

        SELECT lfa1~lifnr,       "Número de cuenta del proveedor o acreedor
               lfa1~adrnr,       "Dirección
               adr6~persnumber,  "Número de persona
               adr6~smtp_addr    "Dirección de correo electrónico
          FROM lfa1
          JOIN adr6
          ON lfa1~adrnr = adr6~addrnumber
          WHERE lfa1~lifnr = @reguh-lifnr
          INTO TABLE @DATA(it_lfa1_adr6).

        IF sy-subrc IS INITIAL.

          SELECT lifnr,    "Número de cuenta del proveedor o acreedor
                 prsnr,    "Número de persona
                 abtnr     "Departamento de persona contacto
            FROM knvk
            FOR ALL ENTRIES IN @it_lfa1_adr6
            WHERE lifnr = @it_lfa1_adr6-lifnr AND
                  prsnr = @it_lfa1_adr6-persnumber AND
                  abtnr = '0001'
            INTO TABLE @DATA(it_knvk).

          IF sy-subrc IS INITIAL.
            LOOP AT it_knvk INTO DATA(wa_knvk).
              READ TABLE it_lfa1_adr6 INTO DATA(wa_lfa1_adr6) WITH KEY lifnr      = wa_knvk-lifnr
                                                                       persnumber = wa_knvk-prsnr.
              IF sy-subrc IS INITIAL.
                wa_correo-smtp_addr = wa_lfa1_adr6-smtp_addr.
                APPEND wa_correo TO it_correo.
              ENDIF.

            ENDLOOP.
          ENDIF.
          LOOP AT it_lfa1_adr6 INTO wa_lfa1_adr6 WHERE lifnr = reguh-lifnr AND
                                                  persnumber = space.
            wa_correo-smtp_addr = wa_lfa1_adr6-smtp_addr.
            APPEND wa_correo TO it_correo.
          ENDLOOP.

*          IF sy-subrc IS INITIAL.
*            LOOP AT it_knvk INTO DATA(wa_knvk).
*              READ TABLE it_lfa1_adr6 INTO DATA(wa_lfa1_adr6) WITH KEY lifnr      = wa_knvk-lifnr
*                                                                       persnumber = wa_knvk-prsnr.
*              IF sy-subrc IS INITIAL.

*          LOOP AT it_knvk INTO DATA(wa_knvk).
*            READ TABLE it_lfa1_adr6 INTO DATA(wa_lfa1_adr6) WITH KEY lifnr      = wa_knvk-lifnr
*                                                                     persnumber = wa_knvk-prsnr.
*            IF wa_knvk-abtnr >= '0002'.
**            IF wa_knvk-abtnr >= '0002' or wa_knvk-abtnr is initial.
*              DELETE it_lfa1_adr6 INDEX sy-tabix.
*            ENDIF.
*          ENDLOOP.
*
*          LOOP AT it_lfa1_adr6 INTO wa_lfa1_adr6.
*            IF sy-tabix = 1.
*              PERFORM f_smtp_usuario USING    sy-uname
*                       CHANGING lv_recname.
*              wa_correo-smtp_addr = lv_recname.
*              APPEND wa_correo TO it_correo.
*            ENDIF.
*
*            wa_correo-smtp_addr = wa_lfa1_adr6-smtp_addr.
*            APPEND wa_correo TO it_correo.
*          ENDLOOP.

*              ENDIF.

*            ENDLOOP.
*          ENDIF.

        ENDIF.

        IF sy-subrc = 0.

          lv_smtp = wa_correo-smtp_addr.

        ENDIF.

*          ENDSELECT.

        IF it_correo IS INITIAL AND p_correo IS NOT INITIAL.
          ADD 1 TO gv_nosmtp.
          CLEAR: gw_salida.
          gw_salida-estado = gc_icon_ko.
          gw_salida-lifnr  = reguh-lifnr.
          IF NOT gw_salida-lifnr IS INITIAL.
            SELECT SINGLE name1 FROM lfa1 INTO gw_salida-name
                                WHERE lifnr EQ gw_salida-lifnr.
            APPEND gw_salida TO gt_salida.
          ENDIF.
        ELSE.

          IF it_correo IS INITIAL.

            SELECT SINGLE smtp_addr INTO wa_correo-smtp_addr
              FROM usr21
              INNER JOIN adr6
              ON usr21~addrnumber  = adr6~addrnumber
              AND usr21~persnumber = adr6~persnumber
              WHERE usr21~bname = sy-uname.

            CLEAR: gw_pool.
            gw_pool-lifnr = reguh-lifnr.
            gw_pool-vblnr = reguh-vblnr.
            gw_pool-smtp  = wa_correo-smtp_addr.
            APPEND gw_pool TO gt_pool.




          ELSE.
            LOOP AT it_correo INTO wa_correo.
*         Guardamos la dirección del proveedor
              CLEAR: gw_pool.
              gw_pool-lifnr = reguh-lifnr.
              gw_pool-vblnr = reguh-vblnr.
              gw_pool-smtp  = wa_correo-smtp_addr.
              APPEND gw_pool TO gt_pool.
            ENDLOOP.

            SELECT SINGLE smtp_addr INTO wa_correo-smtp_addr
              FROM usr21
              INNER JOIN adr6
              ON usr21~addrnumber  = adr6~addrnumber
              AND usr21~persnumber = adr6~persnumber
              WHERE usr21~bname = sy-uname.

            APPEND wa_correo TO it_correo.

            CLEAR: gw_pool.
            gw_pool-lifnr = reguh-lifnr.
            gw_pool-vblnr = reguh-vblnr.
            gw_pool-smtp  = wa_correo-smtp_addr.
            APPEND gw_pool TO gt_pool.

          ENDIF.
*** INICIO MODIFICACIÓN EMG 11/11/2008
          CLEAR: lv_land, it_correo, wa_correo.
          SELECT SINGLE land1 FROM t001 INTO lv_land
                              WHERE bukrs EQ reguh-zbukr.
          IF sy-subrc NE 0.
            CLEAR: lv_land.
          ENDIF.
*** FIN MODIFICACIÓN EMG 11/11/2008
*         Recogemos los datos de posición del pago
*** INICIO MODIFICACIÓN EMG 16/12/2008
*          SELECT * FROM regup APPENDING TABLE gt_regup
          REFRESH: lt_regup2.
          SELECT * FROM regup INTO TABLE lt_regup2
*** FIN MODIFICACIÓN EMG 16/12/2008
                   WHERE laufd EQ reguh-laufd
                     AND laufi EQ reguh-laufi
                     AND xvorl EQ reguh-xvorl
                     AND zbukr EQ reguh-zbukr
                     AND lifnr EQ reguh-lifnr
                     AND kunnr EQ reguh-kunnr
                     AND empfg EQ reguh-empfg
                     AND vblnr EQ reguh-vblnr.
*** INICIO MODIFICACIÓN EMG 11/11/2008
          IF sy-subrc EQ 0 AND
             lv_land  EQ 'EC'.
            REFRESH: lt_regup.
            CLEAR: regup.
*** INICIO MODIFICACIÓN EMG 16/12/2008
*            LOOP AT gt_regup INTO regup.
            LOOP AT lt_regup2 INTO regup.
*** FIN MODIFICACIÓN EMG 16/12/2008
              CLEAR: regup-sgtxt.
              regup-sgtxt = 'Su Factura'.
*** INICIO MODIFICACIÓN EMG 16/12/2008
*              MODIFY gt_regup FROM regup.
              MODIFY lt_regup2 FROM regup.
*** FIN MODIFICACIÓN EMG 16/12/2008
              IF NOT regup-qsskz IS INITIAL.
                regup-wrbtr = regup-skfbt.
*** INICIO MODIFICACIÓN EMG 16/12/2008
*                MODIFY gt_regup FROM regup.
                MODIFY lt_regup2 FROM regup.
*** FIN MODIFICACIÓN EMG 16/12/2008
                REFRESH: lt_ret.
                SELECT bsis~hkont, bsis~shkzg, bsis~wrbtr, bsis~qsskz, acdoca~prctr
                  FROM bsis
                  INNER JOIN acdoca
                          ON acdoca~rbukrs EQ bsis~bukrs
                         AND acdoca~belnr EQ bsis~belnr
                         AND acdoca~gjahr EQ bsis~gjahr
                         AND acdoca~buzei EQ bsis~buzei
                  WHERE bukrs EQ @regup-bukrs
                    AND bsis~belnr EQ @regup-belnr
                    AND bsis~gjahr EQ @regup-gjahr
                INTO TABLE @lt_ret.
                IF sy-subrc EQ 0.
*** Inicio CGR 16/12/2010
*                  DELETE lt_ret WHERE qsskz IS INITIAL
*                                   OR qsskz EQ 'XX'.
                  CLEAR lw_ret.
                  LOOP AT lt_ret INTO lw_ret.
                    SELECT SINGLE * FROM t030 INTO ls_t030 WHERE
                    konts = lw_ret-hkont.
                    IF sy-subrc NE 0.
                      DELETE lt_ret INDEX sy-tabix.
                    ENDIF.
                  ENDLOOP.
*** Fin CGR 16/12/2010
                  IF NOT lt_ret[] IS INITIAL.
                    CLEAR: lw_ret.
                    LOOP AT lt_ret INTO lw_ret.
                      CLEAR: regup-shkzg,
                             regup-wrbtr.
                      IF lw_ret-shkzg EQ 'S'.
                        regup-shkzg = 'H'.
                      ELSE.
                        regup-shkzg = 'S'.
                      ENDIF.
                      regup-wrbtr = lw_ret-wrbtr.
                      IF lw_ret-hkont EQ '0047510004'.
                        regup-sgtxt = 'Ret IVA'.
                      ELSEIF lw_ret-hkont EQ '0047510006'.
                        regup-sgtxt = 'Ret IR'.
                      ENDIF.
                      APPEND regup TO lt_regup.
                      CLEAR: lw_ret.
                    ENDLOOP.
                  ENDIF.
                ENDIF.
                REFRESH: lt_ret.
                SELECT bsas~hkont, bsas~shkzg, bsas~wrbtr, bsas~qsskz, acdoca~prctr
                  FROM bsas
                  INNER JOIN acdoca
                          ON acdoca~rbukrs EQ bsas~bukrs
                         AND acdoca~belnr EQ bsas~belnr
                         AND acdoca~gjahr EQ bsas~gjahr
                         AND acdoca~buzei EQ bsas~buzei
                  WHERE bukrs EQ @regup-bukrs
                    AND bsas~belnr EQ @regup-belnr
                    AND bsas~gjahr EQ @regup-gjahr
                INTO TABLE @lt_ret.
                IF sy-subrc EQ 0.
*** Inicio CGR 16/12/2010
*                  DELETE lt_ret WHERE qsskz IS INITIAL
*                                   OR qsskz EQ 'XX'.
                  CLEAR lw_ret.
                  LOOP AT lt_ret INTO lw_ret.
                    SELECT SINGLE * FROM t030 INTO ls_t030 WHERE
                    konts = lw_ret-hkont.
                    IF sy-subrc NE 0.
                      DELETE lt_ret INDEX sy-tabix.
                    ENDIF.
                  ENDLOOP.
*** Fin CGR 16/12/2010
                  IF NOT lt_ret[] IS INITIAL.
                    CLEAR: lw_ret.
                    LOOP AT lt_ret INTO lw_ret.
                      CLEAR: regup-shkzg,
                             regup-wrbtr.
                      IF lw_ret-shkzg EQ 'S'.
                        regup-shkzg = 'H'.
                      ELSE.
                        regup-shkzg = 'S'.
                      ENDIF.
                      regup-wrbtr = lw_ret-wrbtr.
                      IF lw_ret-hkont EQ '0047510004'.
                        regup-sgtxt = 'Ret IVA'.
                      ELSEIF lw_ret-hkont EQ '0047510006'.
                        regup-sgtxt = 'Ret IR'.
                      ENDIF.
                      APPEND regup TO lt_regup.
                      CLEAR: lw_ret.
                    ENDLOOP.
                  ENDIF.
                ENDIF.
              ENDIF.
              CLEAR: regup.
            ENDLOOP.
            APPEND LINES OF lt_regup TO gt_regup.
          ENDIF.
*** FIN MODIFICACIÓN EMG 11/11/2008
*** INICIO MODIFICACIÓN EMG 16/12/2008
          APPEND LINES OF lt_regup2 TO gt_regup.
*** FIN MODIFICACIÓN EMG 16/12/2008
        ENDIF.
      ENDIF.
      CLEAR: reguh.
    ENDLOOP.
  ELSE.
*   No existen datos de pagos a enviar.
    MESSAGE i261(zfi01).
    gv_error = 'X'.
  ENDIF.
ENDFORM.                    "f_seleccion_datos

*&---------------------------------------------------------------------*
*&      Form  f_chequea_campos
*&---------------------------------------------------------------------*
FORM f_chequea_campos.
  SELECT COUNT(*) FROM reguh UP TO 1 ROWS
                  WHERE laufd EQ p_laufd
                    AND laufi EQ p_laufi
                    AND xvorl EQ space.
  IF sy-subrc NE 0.
*   No existen datos de pagos a enviar.
    MESSAGE e261(zfi01).
  ENDIF.

*** INICIO MODIFICACIÓN EMG 09/12/2008
  IF NOT p_impres IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group4 EQ '007'. "p_print
        EXIT.
      ENDIF.
    ENDLOOP.
    IF p_print IS INITIAL AND
       screen-active EQ '1'.
      MESSAGE e366(po).                                     "#EC *
    ENDIF.
  ENDIF.
*** FIN MODIFICACIÓN EMG 09/12/2008

ENDFORM.                    "f_chequea_campos

*&---------------------------------------------------------------------*
*&      Form  f_informa_nosmtp
*&---------------------------------------------------------------------*
FORM f_informa_nosmtp.
* Declaración
  DATA: lv_nosmtp    TYPE char10,
        lv_texto     TYPE string,
        lv_respuesta TYPE c.
* Inicialización
  CLEAR: lv_nosmtp,
         lv_texto,
         lv_respuesta.

* Si hay clientes sin dirección de correo
*** Inicio CGR 29/11/2010
*  IF NOT gv_nosmtp IS INITIAL.
  IF NOT gv_nosmtp IS INITIAL AND
     NOT p_correo  IS INITIAL.
*** Fin CGR 29/11/2010
    lv_nosmtp = gv_nosmtp.
    CONDENSE lv_nosmtp.
    CONCATENATE TEXT-p04 lv_nosmtp TEXT-p05 TEXT-p06 INTO lv_texto SEPARATED BY space.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = TEXT-p01
        text_question         = lv_texto
        text_button_1         = TEXT-p02
        icon_button_1         = 'ICON_CHECKED'
        text_button_2         = TEXT-p03
        icon_button_2         = 'ICON_INCOMPLETE'
        default_button        = '2'
        display_cancel_button = space
        iv_quickinfo_button_1 = 'S'
        iv_quickinfo_button_2 = 'N'
      IMPORTING
        answer                = lv_respuesta
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc     NE 0 OR
       lv_respuesta EQ '2'.
      gv_error = 'X'.
      REFRESH: gt_salida.
    ENDIF.
  ENDIF.
ENDFORM.                    "f_informa_nosmtp

*&---------------------------------------------------------------------*
*&      Form  f_prepara_formularios
*&---------------------------------------------------------------------*
FORM f_prepara_formularios.
* Declaración
  DATA: lv_index LIKE sy-tabix.
  DATA: lt_otf TYPE TABLE OF itcoo,
        lt_pdf TYPE TABLE OF solisti1.

*Marta Vall: 13.10.2008 - inicio modificación
  DATA: va_land1    TYPE t001-land1,      "Clave del País
        va_form(16) TYPE c.               "Nombre del formulario
*  DATA: var1 TYPE string,
*        var2 TYPE string,
*        var3 TYPE string,
*        itab TYPE TABLE OF string.
*Marta Vall: 13.10.2008 - fin modificación

* Inicialización
  CLEAR: lv_index.
  REFRESH: lt_otf,
           lt_pdf.

*** INICIO MODIFICACIÓN EMG 11/11/2008
* Ordenamos las posiciones por documentos
  SORT gt_regup BY laufd laufi xvorl zbukr lifnr kunnr empfg vblnr bukrs belnr gjahr.
*** FIN MODIFICACIÓN EMG 11/11/2008

  CLEAR: reguh.
  LOOP AT gt_reguh INTO reguh.
*   Comprobamos que tiene dirección de correo
    CLEAR: gw_pool.
    READ TABLE gt_pool INTO gw_pool WITH KEY lifnr = reguh-lifnr
                                             vblnr = reguh-vblnr.
*** INICIO MODIFICACIÓN EMG 09/12/2008
*    IF sy-subrc EQ 0.
    IF ( sy-subrc EQ 0 AND NOT p_correo IS INITIAL ) OR
       NOT p_impres IS INITIAL.
*** FIN MODIFICACIÓN EMG 09/12/2008
      CLEAR: lv_index.
      lv_index = sy-tabix.
*     Cargamos los parametros de impresión
*** INICIO MODIFICACIÓN EMG 09/12/2008
      IF NOT p_correo IS INITIAL.
*** FIN MODIFICACIÓN EMG 09/12/2008
        CLEAR: itcpo.
        itcpo-tddest  = 'LOCA'.
        itcpo-tdgetotf = 'X'.
*** INICIO MODIFICACIÓN EMG 09/12/2008
      ELSE.
        CLEAR: itcpo.
        itcpo-tddest  = p_print.
        itcpo-tdimmed = 'X'.
      ENDIF.
*** FIN MODIFICACIÓN EMG 09/12/2008

*Marta Vall: 13.10.2008 - inicio modificación
      SELECT SINGLE land1
               INTO va_land1
               FROM t001
              WHERE bukrs EQ reguh-zbukr.

      IF va_land1 EQ 'GB'.
        va_form = 'ZFI_UK_AVTR'.
*** INICIO MODIFICACIÓN EMG 11/11/2008
      ELSEIF va_land1 EQ 'EC'.
        va_form = 'ZFI_EC_AVTR_V2'.
*** FIN MODIFICACIÓN EMG 11/11/2008
      ELSE.
        va_form = 'ZFI_ES_AVTR'.
      ENDIF.
*Marta Vall: 13.10.2008 - fin modificación

*     Abrimos el formulario
      CALL FUNCTION 'OPEN_FORM'
        EXPORTING
          dialog                      = ''
*Marta Vall: 13.10.2008 - inicio modificación
*         form                        = 'ZFI_ES_AVTR'
          form                        = va_form
*Marta Vall: 13.10.2008 - fin modificación
          options                     = itcpo
        EXCEPTIONS
          canceled                    = 1
          device                      = 2
          form                        = 3
          options                     = 4
          unclosed                    = 5
          mail_options                = 6
          archive_error               = 7
          invalid_fax_number          = 8
          more_params_needed_in_batch = 9
          spool_error                 = 10
          codepage                    = 11
          OTHERS                      = 12.
      IF sy-subrc EQ 0.
*       Comenzamos el fomulario
        CALL FUNCTION 'START_FORM'.
*       Cargamos los datos para la cabecera
        CLEAR: t001.
        t001-bukrs = reguh-zbukr.

*Marta: 28.10.2008 - inicio modificación
        regup-lifnr = reguh-lifnr.

        PERFORM f_email.

        PERFORM banco_proveidor_uk.
*Marta: 28.10.2008 - fin modificación

*       Escribimos la cabecera
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            window  = 'INLAND'
            element = '550'.
**** Inicio CGR 30/11/2010
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            window = 'FISCAL'.

**** Fin CGR 30/11/2010
*       Tratamos las posiciones
        CLEAR: regup.
        LOOP AT gt_regup INTO regup WHERE laufd EQ reguh-laufd
                                      AND laufi EQ reguh-laufi
                                      AND xvorl EQ reguh-xvorl
                                      AND zbukr EQ reguh-zbukr
                                      AND lifnr EQ reguh-lifnr
                                      AND kunnr EQ reguh-kunnr
                                      AND empfg EQ reguh-empfg
                                      AND vblnr EQ reguh-vblnr.
*         Cargamos los datos para las posiciones
          CLEAR: lt_xblnr.
          MOVE-CORRESPONDING regup TO lt_xblnr.
*Marta Vall: 13.10.2008 - inicio modificación
* contador de lineas de facturas o abonos dentro de la transferencia *
          IF idx = 30.
            idx = 0.
          ENDIF.
          idx = idx + 1.

          IF lt_xblnr-shkzg EQ 'H'.
            subtotal = subtotal + lt_xblnr-wrbtr.
          ELSE.
            subtotal = subtotal - lt_xblnr-wrbtr.
          ENDIF.

          SELECT SINGLE ltext
                 INTO lt_xblnr-ltext
                 FROM t003t
                 WHERE blart EQ lt_xblnr-blart AND spras EQ sy-langu.

* en el caso de que el documento se de tipo SA, se debe mirar en su
* clave contabilitzadora, si es 31 --> Factura si es 21 --> Abono
          IF lt_xblnr-blart EQ 'SA'.
            IF lt_xblnr-bschl EQ '31'.
              lt_xblnr-ltext = 'Invoice'.
            ELSEIF lt_xblnr-bschl EQ '21'.
              lt_xblnr-ltext = 'Credit Note'.
            ENDIF.
          ELSEIF lt_xblnr-blart EQ 'KA'.
            IF lt_xblnr-bschl EQ '39'.
              lt_xblnr-ltext = 'Down Payment'.
            ENDIF.
          ELSEIF lt_xblnr-blart EQ 'AA'.
            IF lt_xblnr-bschl EQ '31'.
              lt_xblnr-ltext = 'Invoice'.
            ELSEIF lt_xblnr-bschl EQ '21'.
              lt_xblnr-ltext = 'Credit Note'.
            ENDIF.
          ELSEIF lt_xblnr-blart EQ 'ZP'.
            IF lt_xblnr-bschl EQ '31'.
              lt_xblnr-ltext = 'Invoice'.
            ELSEIF lt_xblnr-bschl EQ '21'.
              lt_xblnr-ltext = 'Credit Note'.
            ENDIF.
          ENDIF.

* cojer solo la palabra clave; Factura, Abono....
          SPLIT lt_xblnr-ltext AT space INTO: var1 var2 var3,
                                         TABLE itab.
* si es un pago parcial, pago acreedor, ... pondremos que salga pago parcial
          CONDENSE var1.
          IF var1 EQ 'Payment'.
            CONCATENATE var1 ' Parcial' INTO var1.
          ENDIF.


*Marta Vall: 13.10.2008 - fin modificación

*         Escribimos la posición
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              window  = 'MAIN'
              element = 'FACTURAS'
            EXCEPTIONS
              window  = 1
              element = 2.
          CLEAR: regup.
        ENDLOOP.
*       Escribimos el total
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            window  = 'MAIN'
            element = 'FINAL'
          EXCEPTIONS
            window  = 1
            element = 2.
*       Finalizamos el formulario
        CALL FUNCTION 'END_FORM'.
*       Cerramos el formulario
        REFRESH: lt_otf.
        CALL FUNCTION 'CLOSE_FORM'
          TABLES
            otfdata                  = lt_otf
          EXCEPTIONS
            unopened                 = 1
            bad_pageformat_for_print = 2
            send_error               = 3
            spool_error              = 4
            codepage                 = 5
            OTHERS                   = 6.
        IF sy-subrc EQ 0.
          REFRESH: lt_pdf.
*Marta: 27.10.2008 - inicio modificación
          CLEAR subtotal.
*Marta: 27.10.2008 - fin modificación

*** INICIO MODIFICACIÓN EMG 09/12/2008
          IF NOT p_correo IS INITIAL.
*** FIN MODIFICACIÓN EMG 09/12/2008
*         Convertimos a formato PDF
            PERFORM f_conv_pdf TABLES lt_otf lt_pdf.
            IF NOT lt_pdf[] IS INITIAL.
              gw_pool-pdf[] = lt_pdf[].
              MODIFY gt_pool FROM gw_pool INDEX lv_index.
            ENDIF.
*** INICIO MODIFICACIÓN EMG 09/12/2008
          ENDIF.
*** FIN MODIFICACIÓN EMG 09/12/2008
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: reguh.
  ENDLOOP.
ENDFORM.                    "f_prepara_formularios

*&---------------------------------------------------------------------*
*&      Form  f_conv_pdf
*&---------------------------------------------------------------------*
FORM f_conv_pdf TABLES pt_otf STRUCTURE itcoo
                       pt_pdf STRUCTURE solisti1.
* Declaración
  DATA: lv_tamanio TYPE i.
  DATA: lt_pdf TYPE TABLE OF tline.
* Inicialización
  CLEAR: lv_tamanio.
  REFRESH: lt_pdf.

* Convertimos OTF a PDF
  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
    IMPORTING
      bin_filesize          = lv_tamanio
    TABLES
      otf                   = pt_otf[]
      lines                 = lt_pdf[]
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      err_bad_otf           = 4
      OTHERS                = 5.
  IF sy-subrc EQ 0.
*   Transformamos la longitud de salida
    CALL FUNCTION 'SX_TABLE_LINE_WIDTH_CHANGE'
      EXPORTING
        line_width_dst              = '255'
      TABLES
        content_in                  = lt_pdf[]
        content_out                 = pt_pdf[]
      EXCEPTIONS
        err_line_width_src_too_long = 1
        err_line_width_dst_too_long = 2
        err_conv_failed             = 3
        OTHERS                      = 4.
  ENDIF.
ENDFORM.                    "f_conv_pdf

*&---------------------------------------------------------------------*
*&      Form  f_envia_formularios
*&---------------------------------------------------------------------*
FORM f_envia_formularios.
* Declaración
  DATA: lv_lineas  TYPE i,
        lv_enviado TYPE i,
        lv_total   TYPE i.

* Inicialización
  CLEAR: lv_lineas,
         lv_enviado,
         lv_total,
         lw_packing_list,
         lw_document_data,
         lw_destinatarios,
         lw_texto.
  REFRESH: lt_packing_list,
           lt_destinatarios,
           lt_texto.

  IF p_correo IS INITIAL.
    SORT gt_pool BY vblnr.
    DELETE ADJACENT DUPLICATES FROM gt_pool COMPARING vblnr.
  ENDIF.

  CLEAR: gw_pool.
  LOOP AT gt_pool INTO gw_pool.
    CLEAR: gw_salida.
*   Datos del adjunto (PDF)
    REFRESH: lt_packing_list,
             lt_destinatarios.
    CLEAR: lv_lineas.
    DESCRIBE TABLE gw_pool-pdf LINES lv_lineas.
    CLEAR: lw_packing_list.
    lw_packing_list-transf_bin = 'X'.
    lw_packing_list-head_start = 1.
    lw_packing_list-body_start = 1.
    lw_packing_list-body_num   = lv_lineas.
    lw_packing_list-doc_type   = 'PDF'.
    lw_packing_list-obj_name   = TEXT-001.
    lw_packing_list-obj_descr  = TEXT-001.
    lw_packing_list-doc_size   = lv_lineas * 255.
    APPEND lw_packing_list TO lt_packing_list.
*   Datos
    CLEAR: lw_document_data.
    CONCATENATE TEXT-001 gw_pool-vblnr TEXT-002 gw_pool-lifnr INTO lw_document_data-obj_descr SEPARATED BY space.
*   Destinatario
    CLEAR: lw_destinatarios.
    lw_destinatarios-rec_type = 'U'.
    lw_destinatarios-com_type = 'INT'.
    lw_destinatarios-receiver = gw_pool-smtp.
    APPEND lw_destinatarios TO lt_destinatarios.
    CLEAR: lw_destinatarios.
    lw_destinatarios-rec_type = 'U'.
    lw_destinatarios-com_type = 'INT'.
*    PERFORM f_smtp_usuario USING    sy-uname
*                           CHANGING lw_destinatarios-receiver.
    lw_destinatarios-blind_copy = 'X'.
    IF NOT lw_destinatarios-receiver IS INITIAL.
      APPEND lw_destinatarios TO lt_destinatarios.
    ENDIF.
*   Texto
*    CLEAR: lw_texto.
*    lw_texto-line = 'Prueba'.
*    APPEND lw_texto TO lt_texto.
*   Envio
* Inicio Modificació  03.2011 JAQV
    PERFORM envia_email.
*Fin Modificacion JAQV
*    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
*      EXPORTING
*        document_data              = lw_document_data
*        commit_work                = 'X'
*      TABLES
*        packing_list               = lt_packing_list
*        contents_bin               = gw_pool-pdf
*        contents_txt               = lt_texto
*        receivers                  = lt_destinatarios
*      EXCEPTIONS
*        too_many_receivers         = 1
*        document_not_sent          = 2
*        document_type_not_exist    = 3
*        operation_no_authorization = 4
*        parameter_error            = 5
*        x_error                    = 6
*        enqueue_error              = 7
*        OTHERS                     = 8.
    IF sy-subrc EQ 0.
      ADD 1 TO lv_enviado.
      gw_salida-estado = gc_icon_ok.
    ELSE.
      gw_salida-estado = gc_icon_ko.
    ENDIF.
    gw_salida-lifnr  = gw_pool-lifnr.
    SELECT SINGLE name1 FROM lfa1 INTO gw_salida-name
                        WHERE lifnr EQ gw_salida-lifnr.
    gw_salida-smtp   = gw_pool-smtp.
    APPEND gw_salida TO gt_salida.
    CLEAR: gw_pool.
  ENDLOOP.
* Mostramos los envios
  CLEAR: lv_total.
  DESCRIBE TABLE gt_pool LINES lv_total.
  MESSAGE s262(zfi01) WITH lv_enviado lv_total.
ENDFORM.                    "f_envia_formularios

*&---------------------------------------------------------------------*
*&      Form  f_smtp_usuario
*&---------------------------------------------------------------------*
FORM f_smtp_usuario USING    pi_uname TYPE xubname
                    CHANGING pio_smtp TYPE so_recname.
* Declaración
  DATA: lv_pers TYPE usr21-persnumber,
        lv_addr TYPE usr21-addrnumber.
* Inicializacíon
  CLEAR: lv_pers,
         lv_addr.

* Obtenemos el número de dirección y personal
  SELECT SINGLE persnumber addrnumber FROM usr21 INTO (lv_pers, lv_addr)
                  WHERE bname EQ pi_uname.
  IF sy-subrc EQ 0.
    SELECT smtp_addr FROM adr6 INTO pio_smtp UP TO 1 ROWS
                     WHERE addrnumber EQ lv_addr
                     AND persnumber EQ lv_pers.
    ENDSELECT.
  ENDIF.
ENDFORM.                    "f_smtp_usuario

*&---------------------------------------------------------------------*
*&      Form  f_imprime_salida
*&---------------------------------------------------------------------*
FORM f_imprime_salida.                                      "#EC CALLED
  DATA: lv_cadena TYPE char200.
  CLEAR: lv_cadena.

  IF NOT gt_salida[] IS INITIAL.
*   Cargamos los datos
    cl_salv_table=>factory( IMPORTING r_salv_table = rf_alv
                            CHANGING  t_table      = gt_salida ).
*   Cargamos las funciones
    rf_func = rf_alv->get_functions( ).
    rf_func->set_all( abap_true ).
*   Tratamos las columnas
    rf_columns = rf_alv->get_columns( ).
    rf_column ?= rf_columns->get_column( 'ESTADO' ).
    CLEAR: lv_cadena.
    lv_cadena = TEXT-s01.
    rf_column->set_long_text( lv_cadena(40) ).
    rf_column->set_medium_text( lv_cadena(20) ).
    rf_column->set_short_text( lv_cadena(10) ).
    rf_column->set_optimized( abap_true ).
    rf_column->set_alignment( 3 ).
    rf_column->set_icon( abap_true ).
    rf_column ?= rf_columns->get_column( 'LIFNR' ).
    CLEAR: lv_cadena.
    lv_cadena = TEXT-s02.
    rf_column->set_long_text( lv_cadena(40) ).
    rf_column->set_medium_text( lv_cadena(20) ).
    rf_column->set_short_text( lv_cadena(10) ).
    rf_column->set_optimized( abap_true ).
    rf_column->set_output_length( '000018' ).
    rf_column ?= rf_columns->get_column( 'NAME' ).
    CLEAR: lv_cadena.
    lv_cadena = TEXT-s03.
    rf_column->set_long_text( lv_cadena(40) ).
    rf_column->set_medium_text( lv_cadena(20) ).
    rf_column->set_short_text( lv_cadena(10) ).
    rf_column->set_optimized( abap_true ).
    rf_column->set_output_length( '000003' ).
    rf_column ?= rf_columns->get_column( 'SMTP' ).
    CLEAR: lv_cadena.
    lv_cadena = TEXT-s04.
    rf_column->set_long_text( lv_cadena(40) ).
    rf_column->set_medium_text( lv_cadena(20) ).
    rf_column->set_short_text( lv_cadena(10) ).
    rf_column->set_optimized( abap_true ).
*   Mostramos
    rf_alv->display( ).
  ENDIF.
ENDFORM.                    "f_imprime_salida
*&---------------------------------------------------------------------*
*&      Form  F_EMAIL
*&---------------------------------------------------------------------*
* Email proveedor
*----------------------------------------------------------------------*
* Autor: Marta Vall Armengol
* Fecha: 28.10.2008
*----------------------------------------------------------------------*
FORM f_email .
  DATA: va_direccion LIKE  adr6-addrnumber.    "Dirección (número de sap)

  SELECT SINGLE adrnr
           FROM lfa1
           INTO va_direccion
          WHERE lifnr EQ reguh-lifnr.

  SELECT SINGLE smtp_addr
        FROM adr6
        INTO email
       WHERE addrnumber EQ va_direccion.

ENDFORM.                    " F_EMAIL
*&---------------------------------------------------------------------*
*&      Form  BANCO_PROVEIDOR_UK
*&---------------------------------------------------------------------*
*  Información del Banco proveedor UK
*----------------------------------------------------------------------*
* Autor: Marta Vall Armengol
* Fecha: 28.10.2008
*----------------------------------------------------------------------*
FORM banco_proveidor_uk .

  DATA: va_lifnr(10) TYPE c,
        va_lnrzb     TYPE lfb1-lnrzb.   "proveedor alternativo

*Comprobar si hay proveedor alternativo
  SELECT SINGLE lnrzb
             FROM lfb1
             INTO va_lnrzb
            WHERE lifnr EQ reguh-lifnr
              AND lnrzb NE ''
              AND bukrs EQ reguh-zbukr.

  IF sy-subrc EQ 0.
    va_lifnr = va_lnrzb.
  ELSEIF sy-subrc EQ 4.
    va_lifnr = reguh-lifnr.
  ENDIF.

*Información del banco
  SELECT SINGLE bankl bankn
          INTO (sort_code,
                account)
          FROM lfbk
         WHERE lifnr EQ va_lifnr.
ENDFORM.                    " BANCO_PROVEIDOR_UK

*** INICIO MODIFICACIÓN EMG 09/12/2008
*&---------------------------------------------------------------------*
*&      Form  f_modifica_visual
*&---------------------------------------------------------------------*
FORM f_modifica_visual.
  CLEAR: screen.
  LOOP AT SCREEN.
    IF screen-group4 EQ '007'. "p_print
      IF NOT p_correo IS INITIAL.
        screen-active = 0.
      ELSEIF NOT p_impres IS INITIAL.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    CLEAR: screen.
  ENDLOOP.
ENDFORM.                    "f_modifica_visual
*** FIN MODIFICACIÓN EMG 09/12/2008
*&---------------------------------------------------------------------*
*&      Form  ENVIA_EMAIL
*&---------------------------------------------------------------------*
*       Esta función lo que realiza es una reimpresion del aviso de    *
*   pago, para que se genere una orde de spool y luego mediante  fun - *
*   ciones hace el envio del aviso de pago a partir de ese spool y lo  *
*   convierte en un pdf para adjuntarlo en un correo.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM envia_email .
  TABLES: tsp01, adrc.
  DATA: folder_id      LIKE soodk,
        object_id      LIKE soodk,
        link_folder_id LIKE soodk,
        g_document     LIKE sood4,
        g_folmem_data  LIKE sofm2,
        g_header_data  LIKE sood2,
        g_receive_data LIKE soos6,
        g_ref_document LIKE sood4,
        g_new_parent   LIKE soodk,
        l_folder_id    LIKE sofdk,
        v_email(50),
        lv_name        TYPE tdobname.

  DATA: it_lines TYPE STANDARD TABLE OF tline WITH HEADER LINE.

  DATA: client  LIKE tst01-dclient,
        name    LIKE tst01-dname,
        objtype LIKE rststype-type,
        type    LIKE rststype-type.

  DATA: numbytes   TYPE i,
        arc_idx    LIKE toa_dara,
        pdfspoolid LIKE tsp01-rqident,
        jobname    LIKE tbtcjob-jobname,
        jobcount   LIKE tbtcjob-jobcount,
        is_otf.

  DATA: outbox_flag LIKE sonv-flag VALUE ' ',
        store_flag  LIKE sonv-flag,
        delete_flag LIKE sonv-flag,
        owner       LIKE soud-usrnam,
        on          LIKE sonv-flag VALUE 'X',
        sent_to_all LIKE sonv-flag,
        g_authority LIKE sofa-usracc,
        w_objdes    LIKE sood4-objdes.
  DATA: lt_body_email TYPE STANDARD TABLE OF soli WITH HEADER LINE.

  DATA: c_file     LIKE rlgrap-filename,
        n_spool(6) TYPE n.

  DATA: cancel.

  DATA: desired_type LIKE sood-objtp,
        real_type    LIKE sood-objtp,
        attach_type  LIKE sood-objtp,
        otf          LIKE sood-objtp VALUE 'OTF', " SAPscript Ausgabeformat
        ali          LIKE sood-objtp VALUE 'ALI'. " ABAP lists


  CONSTANTS: ou_fol     LIKE sofh-folrg              VALUE 'O',
             c_objtp    LIKE g_document-objtp    VALUE 'RAW',
             c_file_ext LIKE g_document-file_ext VALUE 'TXT'.

  TYPES: BEGIN OF y_files,
           file(60) TYPE c,
         END OF y_files.

  DATA: lt_files TYPE STANDARD TABLE OF y_files WITH HEADER LINE.
  DATA: lt_rec_tab     LIKE STANDARD TABLE OF soos1 WITH HEADER LINE,
        lt_note_text   LIKE STANDARD TABLE OF soli  WITH HEADER LINE,
        lt_attachments LIKE STANDARD TABLE OF sood5 WITH HEADER LINE.

  DATA: list_fax_mail_number TYPE STANDARD TABLE OF soli WITH HEADER LINE.


  DATA: l_objcont     LIKE soli OCCURS 0 WITH HEADER LINE.
  DATA: l_objhead     LIKE soli OCCURS 0 WITH HEADER LINE.

  DATA: hd_dat  LIKE sood1.


  DATA: src_spoolid LIKE tsp01-rqident.
  DATA: header_mail TYPE so_obj_des,
        object_type TYPE  so_escape.
  READ TABLE gt_reguh INTO wa_reguh WITH KEY vblnr = gw_pool-vblnr.

  DATA: w_wt005t TYPE t005t. "ALML 25.06.2013

* INI MOD DEVK917506 LR 23.01.2019 - Aviso pagos prov.
  DATA: lwa_envio_soc TYPE zfit_envio_soc,
        lt_envio_soc  TYPE STANDARD TABLE OF zfit_envio_soc,
        lt_note       LIKE STANDARD TABLE OF soli  WITH HEADER LINE.

  DATA: lv_cont       TYPE i.
* FIN MOD DEVK917506 LR 23.01.2019 - Aviso pagos prov.

* Reimpresion del aviso de pagos dependiendo si es cheque o transferencia.
  IF wa_reguh-rzawe EQ 'C' OR wa_reguh-rzawe EQ 'D' OR wa_reguh-rzawe EQ 'Q' OR
     wa_reguh-rzawe EQ '6'.
    SUBMIT zrffous_chq
        WITH par_anzp = '0'
        WITH par_avis = 'X'
        WITH par_begl = ' '
        WITH par_zdru = ' '
        WITH par_espr = 'X'
        WITH par_pria = 'LX30'
        WITH par_xdta = ' '
        WITH sel_hbki-low = wa_reguh-hbkid
        WITH sel_hkti-low = wa_reguh-hktid
        WITH sel_zawe-low = wa_reguh-rzawe                  "#EC *
        WITH sel_vbln-low = gw_pool-vblnr
        WITH zw_laufd = wa_reguh-laufd
        WITH zw_laufi = wa_reguh-laufi
        WITH zw_zbukr = wa_reguh-zbukr AND RETURN.
  ELSEIF wa_reguh-rzawe EQ 'I'.
*Ini JGOR 01.06.2017
*    SUBMIT rffoedi1
*    SUBMIT zrffoedi1_172
    SUBMIT rffoedi1
        WITH par_anzp = '0'
        WITH par_avis = 'X'
        WITH par_begl = ' '
        WITH par_zdru = ' '
        WITH par_espr = 'X'
        WITH par_pria = 'LX30'
        WITH par_xdta = ' '
        WITH sel_hbki-low = wa_reguh-hbkid
        WITH sel_hkti-low = wa_reguh-hktid
        WITH sel_zawe-low = wa_reguh-rzawe                  "#EC *
        WITH sel_vbln-low = gw_pool-vblnr
        WITH zw_laufd = wa_reguh-laufd
        WITH zw_laufi = wa_reguh-laufi
        WITH zw_zbukr = wa_reguh-zbukr AND RETURN.
*Fin JGOR 01.06.2017
  ELSE.
    SUBMIT zrffous_t
            WITH par_anzp = '0'
            WITH par_avis = 'X'
            WITH par_begl = ' '
            WITH par_espr = 'X'
            WITH par_pria = 'LX30'
            WITH par_xdta = ' '
            WITH sel_hbki-low = wa_reguh-hbkid
            WITH sel_hkti-low = wa_reguh-hktid
            WITH sel_zawe-low = wa_reguh-rzawe              "#EC *
            WITH sel_vbln-low = gw_pool-vblnr
            WITH zw_laufd = wa_reguh-laufd
            WITH zw_laufi = wa_reguh-laufi
            WITH zw_zbukr = wa_reguh-zbukr AND RETURN.
  ENDIF.
  IMPORT tab_ausgabe FROM MEMORY ID 'TAB_AUSGABE'.
  IF sy-subrc EQ 0.
    IF NOT p_correo IS INITIAL.
* Empieza a recosntruir al documento con el numero de spool generado.
      READ TABLE tab_ausgabe INDEX 1.
      src_spoolid = tab_ausgabe-spoolnr.
      SELECT SINGLE * FROM tsp01 WHERE rqident = src_spoolid.
      IF sy-subrc NE 0.

        RAISE err_no_abap_spooljob. "doesn't exist

      ELSE.
        client = tsp01-rqclient.
        name   = tsp01-rqo1name.

        CALL FUNCTION 'RSTS_GET_ATTRIBUTES'
          EXPORTING
            authority     = 'SP01'
            client        = client
            name          = name
            part          = 1
          IMPORTING
            type          = type
            objtype       = objtype
          EXCEPTIONS
            fb_error      = 1
            fb_rsts_other = 2
            no_object     = 3
            no_permission = 4
            OTHERS        = 5.

        IF objtype(3) = 'OTF'.
          desired_type = otf.
        ELSE.
          desired_type = ali.
        ENDIF.
        CALL FUNCTION 'RSPO_RETURN_SPOOLJOB'
          EXPORTING
            rqident              = src_spoolid
            desired_type         = desired_type
          IMPORTING
            real_type            = real_type
          TABLES
            buffer               = l_objcont
          EXCEPTIONS
            no_such_job          = 14
            type_no_match        = 94
            job_contains_no_data = 54
            no_permission        = 21
            can_not_access       = 21
            read_error           = 54.
        IF sy-subrc EQ 0.
          attach_type = real_type.
        ENDIF.


        CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
          EXPORTING
            owner     = sy-uname
            region    = ou_fol
          IMPORTING
            folder_id = l_folder_id
          EXCEPTIONS
            OTHERS    = 5.
        CLEAR: g_document.
        g_document-foltp     = l_folder_id-foltp.
        g_document-folyr     = l_folder_id-folyr.
        g_document-folno     = l_folder_id-folno.

        g_document-objtp     = c_objtp.
        g_document-objdes    = header_mail.
        g_document-file_ext  = c_file_ext.
        IF wa_reguh-zspra EQ 'E'.
          g_header_data-objdes    = TEXT-003.
        ELSE.
          g_header_data-objdes    = TEXT-004.
        ENDIF.
        CALL FUNCTION 'SO_DOCUMENT_REPOSITORY_MANAGER'
          EXPORTING
            method      = 'SAVE'
            office_user = sy-uname
          IMPORTING
            authority   = g_authority
          TABLES
            objcont     = lt_body_email
            attachments = lt_attachments
          CHANGING
            document    = g_document
            header_data = g_header_data
          EXCEPTIONS
            OTHERS      = 1.

        folder_id-objtp = l_folder_id-foltp.
        folder_id-objyr = l_folder_id-folyr.
        folder_id-objno = l_folder_id-folno.

        object_id-objtp = c_objtp.
        object_id-objyr = g_document-objyr.
        object_id-objno = g_document-objno.

        link_folder_id-objtp = l_folder_id-foltp.
        link_folder_id-objyr = l_folder_id-folyr.
        link_folder_id-objno = l_folder_id-folno.
        REFRESH lt_rec_tab.

        LOOP AT lt_destinatarios INTO wa_dest.
          lt_rec_tab-sel = 'X'.
          lt_rec_tab-recextnam = wa_dest-receiver.
          lt_rec_tab-recesc = 'U'.
          lt_rec_tab-sndart = 'INT'.
          lt_rec_tab-sndpri = 1.
          APPEND lt_rec_tab.
        ENDLOOP.

        lt_files-file = c_file.
        APPEND lt_files.
        IF wa_reguh-zspra EQ 'E'.
          hd_dat-objdes = TEXT-003.
        ELSE.
          hd_dat-objdes = TEXT-004.
        ENDIF.
        CALL FUNCTION 'SO_ATTACHMENT_INSERT'
          EXPORTING
            object_id                  = object_id
            attach_type                = attach_type
            object_hd_change           = hd_dat
            owner                      = sy-uname
          TABLES
            objcont                    = l_objcont
            objhead                    = l_objhead
          EXCEPTIONS
            active_user_not_exist      = 35
            communication_failure      = 71
            object_type_not_exist      = 17
            operation_no_authorization = 21
            owner_not_exist            = 22
            parameter_error            = 23
            substitute_not_active      = 31
            substitute_not_defined     = 32
            system_failure             = 72
            x_error                    = 1000.

        IF sy-subrc > 0.
        ENDIF.
*        ini tomar la sociedad original, no la primera
*        aayala 17.02.2012
*        READ TABLE gt_regup INTO wa_regup INDEX 1.

        READ TABLE gt_regup INTO wa_regup WITH KEY vblnr =  gw_pool-vblnr.
*        fin tomar la sociedad original, no la primera
*        aayala 17.02.2012

        IF sy-subrc EQ 0.

*** INI ALML 01.09.2013
*Para las sig. sociedades y si el idioma del proveedor está en inglés
          IF  wa_regup-bukrs EQ '1100' OR
              wa_regup-bukrs EQ '1101' OR
              wa_regup-bukrs EQ '1102' OR
              wa_regup-bukrs EQ '1103' OR
              wa_regup-bukrs EQ '4300' .

            IF wa_reguh-zspra EQ 'E' OR wa_reguh-zspra EQ 'S'.

              lt_note_text-line = 'Solaya Hospitality Management Group, S. de R.L. de C.V.'.
              APPEND lt_note_text.
              CLEAR lt_note_text.

              lt_note_text-line = 'Avenida Bonampak, Supermanzana 9, Manzana 2, Planta Baja'.
              APPEND lt_note_text.
              CLEAR lt_note_text.

              lt_note_text-line = 'Torre N, Piso 7 al 11 y Torre S Piso 7, Cancún, Quintana Roo'.
              APPEND lt_note_text.
              CLEAR lt_note_text.

              lt_note_text-line = 'México'.
              APPEND lt_note_text.
              CLEAR lt_note_text.

            ENDIF.

          ELSE. "INI ALML 01.09.2013

            SELECT SINGLE * INTO t001
              FROM t001
             WHERE bukrs EQ wa_regup-bukrs.
            IF sy-subrc EQ 0.
              SELECT SINGLE * INTO adrc
                FROM adrc
               WHERE addrnumber = t001-adrnr.
              IF sy-subrc EQ 0.
                lt_note_text-line = t001-butxt.
                APPEND lt_note_text.
                CLEAR lt_note_text.
                CONCATENATE adrc-street adrc-house_num1
                       INTO lt_note_text-line.
                APPEND lt_note_text.
                CLEAR lt_note_text.
                CONCATENATE adrc-city1 adrc-post_code1
                       INTO lt_note_text-line.
                APPEND lt_note_text.
                CLEAR lt_note_text.

***INI ALML 25.06.2013
*              lt_note_text-line = t001-ort01.
                CALL FUNCTION 'T005T_SINGLE_READ'
                  EXPORTING
                    t005t_spras = t001-spras
                    t005t_land1 = adrc-country
                  IMPORTING
                    wt005t      = w_wt005t
                  EXCEPTIONS
                    not_found   = 1
                    OTHERS      = 2.
                IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                ENDIF.
*Denominación de país y Código Postal
                CONCATENATE w_wt005t-landx adrc-post_code1
                 INTO lt_note_text-line SEPARATED BY space.
***FIN ALML 25.06.2013

                APPEND lt_note_text.
                CLEAR lt_note_text.
                lt_note_text-line = adrc-tel_number.
                APPEND lt_note_text.
                CLEAR lt_note_text.
                lt_note_text-line = adrc-fax_number.
                APPEND lt_note_text.
                CLEAR lt_note_text.
              ENDIF.
            ENDIF.
          ENDIF. "ALML 01.09.2013
        ENDIF.

***INI ALML 01.09.2013
        IF wa_regup-bukrs EQ '1100' OR
           wa_regup-bukrs EQ '1101' OR
           wa_regup-bukrs EQ '1102' OR
           wa_regup-bukrs EQ '1103' OR
           wa_regup-bukrs EQ '4300'.

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'ST'
              language                = wa_reguh-zspra
              name                    = 'ZTEXTOAVISO2'
              object                  = 'TEXT'
            TABLES
              lines                   = it_lines
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            LOOP AT it_lines.
              CLEAR lt_note_text.
              lt_note_text-line = it_lines-tdline.
              APPEND lt_note_text.
            ENDLOOP.
          ENDIF.

***INI ALML 20.06.2014
        ELSEIF wa_regup-bukrs EQ '2000' OR
               wa_regup-bukrs EQ '2001' OR
               wa_regup-bukrs EQ '2002' OR
               wa_regup-bukrs EQ '2003'.
          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'ST'
              language                = wa_reguh-zspra
              name                    = 'ZTEXTOAVISO_LEGEN'
              object                  = 'TEXT'
            TABLES
              lines                   = it_lines
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            LOOP AT it_lines.
              CLEAR lt_note_text.
              lt_note_text-line = it_lines-tdline.
              APPEND lt_note_text.
            ENDLOOP.
          ENDIF.

* Incio JLM 06.05.2016

        ELSEIF wa_regup-bukrs EQ '5510'
            OR wa_regup-bukrs = '5500'.

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'ST'
              language                = wa_reguh-zspra
              name                    = 'ZTEXTOAVISO3'
              object                  = 'TEXT'
            TABLES
              lines                   = it_lines
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            LOOP AT it_lines.
              CLEAR lt_note_text.
              lt_note_text-line = it_lines-tdline.
              APPEND lt_note_text.
            ENDLOOP.
          ENDIF.

* IN JC 03.09.2020
        ELSEIF wa_regup-bukrs EQ '7000'
            OR wa_regup-bukrs EQ '7005'
            OR wa_regup-bukrs EQ '8000'
            OR wa_regup-bukrs EQ '8300'
            OR wa_regup-bukrs EQ '8600'
            OR wa_regup-bukrs EQ '8605'.

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'ST'
              language                = wa_reguh-zspra
              name                    = 'ZAHENA'
              object                  = 'TEXT'
            TABLES
              lines                   = it_lines
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            LOOP AT it_lines.
              CLEAR lt_note_text.
              lt_note_text-line = it_lines-tdline.
              APPEND lt_note_text.
            ENDLOOP.
          ENDIF.
* FIN JC 03.09.2020
* Fin JLM 06.05.2016
***FIN ALML 20.06.2014

*       INICIO EAMAYA 07.11.2024
*       Se agregan sociedades.
*        ELSEIF wa_regup-bukrs EQ '1500' OR wa_regup-bukrs EQ '1600' OR wa_regup-bukrs EQ '1700' OR wa_regup-bukrs EQ '1800' OR wa_regup-bukrs EQ '1900'.
        ELSEIF wa_regup-bukrs(2) EQ '15' OR wa_regup-bukrs(2) EQ '16' OR wa_regup-bukrs(2) EQ '17' OR wa_regup-bukrs(2) EQ '18' OR wa_regup-bukrs(2) EQ '19'.
*          CASE wa_regup-bukrs.
*            WHEN '1500'.
*              lv_name = 'ZTEXTOAVISO_VALOZO'.
*            WHEN '1600'.
*              lv_name = 'ZTEXTOAVISO_IRAWADI'.
*            WHEN '1700' OR '1800'.
*              lv_name = 'ZTEXTOAVISO_FUNTAL'.
*            WHEN '1900'.
*              lv_name = 'ZTEXTOAVISO_RANGLERY'.
*          ENDCASE.
          CASE wa_regup-bukrs(2).
            WHEN '15'.
              lv_name = 'ZTEXTOAVISO_VALOZO'.
            WHEN '16'.
              lv_name = 'ZTEXTOAVISO_IRAWADI'.
            WHEN '17' OR '18'.
              lv_name = 'ZTEXTOAVISO_FUNTAL'.
            WHEN '19'.
              lv_name = 'ZTEXTOAVISO_RANGLERY'.
          ENDCASE.
*       FIN EAMAYA 07.11.2024

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'ST'
              language                = wa_reguh-zspra
              name                    = lv_name
              object                  = 'TEXT'
            TABLES
              lines                   = it_lines
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            LOOP AT it_lines.
              CLEAR lt_note_text.
              lt_note_text-line = it_lines-tdline.
              APPEND lt_note_text.
            ENDLOOP.
          ENDIF.

        ELSE.

***FIN ALML 01.09.2013

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'ST'
              language                = wa_reguh-zspra
              name                    = 'ZTEXTOAVISO_LEGEN'
              object                  = 'TEXT'
            TABLES
              lines                   = it_lines
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            LOOP AT it_lines.
              CLEAR lt_note_text.
              lt_note_text-line = it_lines-tdline.
              APPEND lt_note_text.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF. " ALML 01.09.2013



*INI DVBP 14/07/2016
      DATA: lv_bukrs TYPE bukrs,
            lv_texto TYPE zfit_soc_mx-aviso_pago,
            lv_pais  TYPE zfit_soc_mx-pais.

      SELECT SINGLE aviso_pago
                    pais"bukrs
        FROM zfit_soc_mx
        INTO (lv_texto,lv_pais)"lv_bukrs
        WHERE bukrs = wa_regup-zbukr.

      IF sy-subrc EQ 0.

        REFRESH lt_note_text.
        CLEAR lt_note_text.

        REFRESH  it_lines.

        SELECT SINGLE * INTO t001
          FROM t001
         WHERE bukrs EQ wa_regup-zbukr.
        IF sy-subrc EQ 0.
          SELECT SINGLE * INTO adrc
            FROM adrc
           WHERE addrnumber = t001-adrnr.
          IF sy-subrc EQ 0.

            IF wa_regup-bukrs EQ '2000' OR
               wa_regup-bukrs EQ '2001' OR
               wa_regup-bukrs EQ '2002' OR
               wa_regup-bukrs EQ '2003' OR
               wa_regup-bukrs EQ '2004' OR
               wa_regup-bukrs EQ '2005' OR
               wa_regup-bukrs EQ '2006'.

              lt_note_text-line = t001-butxt.

            ELSE.

              lt_note_text-line = lv_texto.

            ENDIF.

            APPEND lt_note_text.
            CLEAR lt_note_text.
            CONCATENATE adrc-street adrc-house_num1
                   INTO lt_note_text-line.
            APPEND lt_note_text.
            CLEAR lt_note_text.
            CONCATENATE adrc-city1 adrc-post_code1
                   INTO lt_note_text-line
              SEPARATED BY space.
            APPEND lt_note_text.
            CLEAR lt_note_text.

***INI ALML 25.06.2013
*              lt_note_text-line = t001-ort01.
            CALL FUNCTION 'T005T_SINGLE_READ'
              EXPORTING
                t005t_spras = t001-spras
                t005t_land1 = adrc-country
              IMPORTING
                wt005t      = w_wt005t
              EXCEPTIONS
                not_found   = 1
                OTHERS      = 2.
            IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            ENDIF.
*Denominación de país y Código Postal
            CONCATENATE w_wt005t-landx adrc-post_code1
             INTO lt_note_text-line SEPARATED BY space.
***FIN ALML 25.06.2013

            APPEND lt_note_text.
            CLEAR lt_note_text.
            lt_note_text-line = adrc-tel_number.
            APPEND lt_note_text.
            CLEAR lt_note_text.
            lt_note_text-line = adrc-fax_number.
            APPEND lt_note_text.
            CLEAR lt_note_text.
          ENDIF.
        ENDIF.

        IF lv_pais = 'MX'."JGOR 27.10.2016

          IF wa_regup-zbukr = '5000' OR
             wa_regup-zbukr = '5100' OR
             wa_regup-zbukr = '5200'.

            CALL FUNCTION 'READ_TEXT'
              EXPORTING
                id                      = 'ST'
                language                = wa_reguh-zspra
                name                    = 'ZTEXTO_AVISO'
                object                  = 'TEXT'
              TABLES
                lines                   = it_lines
              EXCEPTIONS
                id                      = 1
                language                = 2
                name                    = 3
                not_found               = 4
                object                  = 5
                reference_check         = 6
                wrong_access_to_archive = 7
                OTHERS                  = 8.
*Ini JGOR 12.12.2017
          ELSEIF wa_regup-bukrs EQ '2000' OR
                 wa_regup-bukrs EQ '2001' OR
                 wa_regup-bukrs EQ '2002' OR
                 wa_regup-bukrs EQ '2003' OR
                 wa_regup-bukrs EQ '2004' OR
                 wa_regup-bukrs EQ '2005' OR
                 wa_regup-bukrs EQ '2006'.
            CALL FUNCTION 'READ_TEXT'
              EXPORTING
                id                      = 'ST'
                language                = wa_reguh-zspra
                name                    = 'ZTEXTOAVISO_LEGEN2'
                object                  = 'TEXT'
              TABLES
                lines                   = it_lines
              EXCEPTIONS
                id                      = 1
                language                = 2
                name                    = 3
                not_found               = 4
                object                  = 5
                reference_check         = 6
                wrong_access_to_archive = 7
                OTHERS                  = 8.
*              IF sy-subrc <> 0.
*                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*              ELSE.
*                LOOP AT it_lines.
*                  CLEAR lt_note_text.
*                  lt_note_text-line = it_lines-tdline.
*                  APPEND lt_note_text.
*                ENDLOOP.
*              ENDIF.
*Fin JGOR 12.12.2017
          ELSE.

            CALL FUNCTION 'READ_TEXT'
              EXPORTING
                id                      = 'ST'
                language                = wa_reguh-zspra
                name                    = 'ZTEXTOAVISO_MX'
                object                  = 'TEXT'
              TABLES
                lines                   = it_lines
              EXCEPTIONS
                id                      = 1
                language                = 2
                name                    = 3
                not_found               = 4
                object                  = 5
                reference_check         = 6
                wrong_access_to_archive = 7
                OTHERS                  = 8.

          ENDIF.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            LOOP AT it_lines.
              CLEAR lt_note_text.
              lt_note_text-line = it_lines-tdline.
              APPEND lt_note_text.
            ENDLOOP.
          ENDIF.
*Ini JGOR 27.10.2016
        ELSEIF lv_pais = 'DO'.

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'ST'
              language                = wa_reguh-zspra
              name                    = 'ZTEXTOAVISO_DO'
              object                  = 'TEXT'
            TABLES
              lines                   = it_lines
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            LOOP AT it_lines.
              CLEAR lt_note_text.
              lt_note_text-line = it_lines-tdline.
              APPEND lt_note_text.
            ENDLOOP.
          ENDIF.
* Inicio JLM 18.11.2016
        ELSEIF lv_pais = 'INP'.
* Inicio MTCM 22.11.2016
*          REFRESH lt_note_text.
*          CLEAR lt_note_text.
* Fin MTCM 22.11.2016
          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'ST'
              language                = wa_reguh-zspra
              name                    = 'ZTEXTOAVISO_INP'
              object                  = 'TEXT'
            TABLES
              lines                   = it_lines
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            LOOP AT it_lines.
              CLEAR lt_note_text.
              lt_note_text-line = it_lines-tdline.
              APPEND lt_note_text.
            ENDLOOP.
          ENDIF.

        ELSEIF lv_pais = 'LPD'.
* Inicio MTCM 22.11.2016
*          REFRESH lt_note_text.
*          CLEAR lt_note_text.
* Fin MTCM 22.11.2016

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'ST'
              language                = wa_reguh-zspra
              name                    = 'ZTEXTOAVISO_LPD'
              object                  = 'TEXT'
            TABLES
              lines                   = it_lines
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            LOOP AT it_lines.
              CLEAR lt_note_text.
              lt_note_text-line = it_lines-tdline.
              APPEND lt_note_text.
            ENDLOOP.
          ENDIF.
* Fin JLM 18.11.2016

* Inicio JLM 30.03.2017
        ELSEIF lv_pais = 'CAS'.

          REFRESH: it_lines,
                   lt_note_text.

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'ST'
              language                = wa_reguh-zspra
              name                    = 'ZTEXTOAVISO_CAS'
              object                  = 'TEXT'
            TABLES
              lines                   = it_lines
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            LOOP AT it_lines.
              CLEAR lt_note_text.
              lt_note_text-line = it_lines-tdline.
              APPEND lt_note_text.
            ENDLOOP.
          ENDIF.

* Fin JLM 30.03.2017
        ENDIF.
*Fin JGOR 27.10.2016
      ENDIF.

*FIN DVBP 14/07/2016

* INI MOD DEVK917506 LR 23.01.2019 - Aviso pagos prov.

      "Obtener formato para sociedad propia de acuerdo a tabla zfit_envio_soc
      SELECT SINGLE *
        FROM zfit_envio_soc
        INTO lwa_envio_soc
       WHERE bukrs EQ wa_regup-bukrs.
      IF sy-subrc EQ 0.

        "LR 29.01.2019 - Validar flag para tomar sociedad pagadora
        IF lwa_envio_soc-activa_soc IS INITIAL.

          CLEAR lwa_envio_soc.

          "Obtener formato para sociedad pagadora de acuerdo a tabla zfit_envio_soc
          SELECT SINGLE *
            FROM zfit_envio_soc
            INTO lwa_envio_soc
           WHERE bukrs EQ wa_regup-zbukr.

        ENDIF.

        IF sy-subrc EQ  0.

          "LR 29.01.2019 - Cabecera de mail (5 lineas) para adjuntar al inicio del formato
          CLEAR: lt_note,
                 lt_note[],
                 lv_cont.

          DO 5 TIMES.

            lv_cont = lv_cont + 1.

            READ TABLE lt_note_text
            INTO lt_note
            INDEX lv_cont.
            IF sy-subrc EQ 0.
              APPEND lt_note.
            ENDIF.

          ENDDO.

          "Obtener texto de mail de acuerdo a formato
          CASE lwa_envio_soc-formato.
            WHEN 01.
              "Formato México
              REFRESH: it_lines,
                       lt_note_text.

              CALL FUNCTION 'READ_TEXT'
                EXPORTING
                  id                      = 'ST'
                  language                = wa_reguh-zspra
                  name                    = 'ZST_ENVIOMX_HD'
                  object                  = 'TEXT'
                TABLES
                  lines                   = it_lines
                EXCEPTIONS
                  id                      = 1
                  language                = 2
                  name                    = 3
                  not_found               = 4
                  object                  = 5
                  reference_check         = 6
                  wrong_access_to_archive = 7
                  OTHERS                  = 8.
              IF sy-subrc <> 0.
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              ELSE.

                "Agregar lineas de cabecera
                LOOP AT lt_note.
                  CLEAR lt_note_text.
                  lt_note_text-line = lt_note-line.
                  APPEND lt_note_text.
                ENDLOOP.

                LOOP AT it_lines.
                  CLEAR lt_note_text.
                  lt_note_text-line = it_lines-tdline.
                  APPEND lt_note_text.
                ENDLOOP.
              ENDIF.

            WHEN 02.
              "Formato Provamex

              REFRESH: it_lines,
                       lt_note_text.

              CALL FUNCTION 'READ_TEXT'
                EXPORTING
                  id                      = 'ST'
                  language                = wa_reguh-zspra
                  name                    = 'ZST_ENVIOPROVA_HD'
                  object                  = 'TEXT'
                TABLES
                  lines                   = it_lines
                EXCEPTIONS
                  id                      = 1
                  language                = 2
                  name                    = 3
                  not_found               = 4
                  object                  = 5
                  reference_check         = 6
                  wrong_access_to_archive = 7
                  OTHERS                  = 8.
              IF sy-subrc <> 0.
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              ELSE.

                LOOP AT lt_note.
                  CLEAR lt_note_text.
                  lt_note_text-line = lt_note-line.
                  APPEND lt_note_text.
                ENDLOOP.

                LOOP AT it_lines.
                  CLEAR lt_note_text.
                  lt_note_text-line = it_lines-tdline.
                  APPEND lt_note_text.
                ENDLOOP.
              ENDIF.

            WHEN 03.
              "Formato Legendary
              REFRESH: it_lines,
                       lt_note_text.

              CALL FUNCTION 'READ_TEXT'
                EXPORTING
                  id                      = 'ST'
                  language                = wa_reguh-zspra
                  name                    = 'ZST_ENVIOLEGEN_HD'
                  object                  = 'TEXT'
                TABLES
                  lines                   = it_lines
                EXCEPTIONS
                  id                      = 1
                  language                = 2
                  name                    = 3
                  not_found               = 4
                  object                  = 5
                  reference_check         = 6
                  wrong_access_to_archive = 7
                  OTHERS                  = 8.
              IF sy-subrc <> 0.
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              ELSE.

                "Agregar lineas de cabecera
                LOOP AT lt_note.
                  CLEAR lt_note_text.
                  lt_note_text-line = lt_note-line.
                  APPEND lt_note_text.
                ENDLOOP.

                LOOP AT it_lines.
                  CLEAR lt_note_text.
                  lt_note_text-line = it_lines-tdline.
                  APPEND lt_note_text.
                ENDLOOP.
              ENDIF.

            WHEN OTHERS.
          ENDCASE.

        ENDIF.

      ENDIF.

* FIN MOD DEVK917506 LR 23.01.2019 - Aviso pagos prov.

* send email from SAPOFFICE
      CALL FUNCTION 'SO_OBJECT_SEND'
        EXPORTING
          folder_id                  = folder_id
          object_id                  = object_id
          outbox_flag                = outbox_flag
          link_folder_id             = link_folder_id
          owner                      = sy-uname
        TABLES
          receivers                  = lt_rec_tab
          note_text                  = lt_note_text
        EXCEPTIONS
          active_user_not_exist      = 35
          communication_failure      = 71
          component_not_available    = 1
          folder_no_authorization    = 5
          folder_not_exist           = 6
          forwarder_not_exist        = 8
          object_no_authorization    = 13
          object_not_exist           = 14
          object_not_sent            = 15
          operation_no_authorization = 21
          owner_not_exist            = 22
          parameter_error            = 23
          substitute_not_active      = 31
          substitute_not_defined     = 32
          system_failure             = 72
          too_much_receivers         = 73
          user_not_exist             = 35.
      IF sy-subrc EQ 0.
        COMMIT WORK.
        CALL FUNCTION 'SO_DEQUEUE_UPDATE_LOCKS'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " ENVIA_EMAIL