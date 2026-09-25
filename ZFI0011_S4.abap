*&---------------------------------------------------------------------*
*& Report  ZFI0011
*&
*&---------------------------------------------------------------------*
*& Javier Ferrándiz
*& 08/03/2007
*&---------------------------------------------------------------------*


REPORT  ZFI0011.

INCLUDE ZFI0011TOP.
INCLUDE ZFI0011EVT.
INCLUDE ZFI0011PBO.
INCLUDE ZFI0011PAI.
INCLUDE ZFI0011CLS.
INCLUDE ZFI0011F01.

**&---------------------------------------------------------------------*
*&  Include           ZFI0011TOP
*&------ ---------------------------------------------------------------*

TABLES: zfit_sol_cliente,
        kna1,
        zficonv_cli_pms,
        nriv,
        adr6,
        adrc,
        knb1,
        knvi,
        knvl,
        tvkbz,
        tvkbt,
        tvta,
        t001,
        but000,
        but100,
        dfkkbptaxnum,
        cvi_cust_link,
        ztbp001,
        t077d.

DATA:    BEGIN OF dynpfields OCCURS 1.
        INCLUDE STRUCTURE dynpread.
DATA:    END   OF dynpfields.
DATA:    BEGIN OF fields OCCURS 3.
        INCLUDE STRUCTURE help_value.
DATA:    END OF fields.
DATA:    BEGIN OF valuetab OCCURS 50,
           value LIKE dfies-fieldtext,
         END   OF valuetab.

DATA: char1(1)       TYPE c,
      index          TYPE i,
      index4          TYPE char4.

DATA: BEGIN OF t_tblcli OCCURS 0,
  sel.
        INCLUDE STRUCTURE zfit_sol_cliente.
DATA END OF t_tblcli.
DATA lv_ini TYPE i.

DATA: BEGIN OF t_tblcli2 OCCURS 0,
        solnum TYPE zfit_sol_cliente-solnum,
        bukrs TYPE zfit_sol_cliente-bukrs,
        kunnr TYPE zfit_sol_cliente-kunnr,
        bu_group TYPE zfit_sol_cliente-bu_group,
*        anred TYPE zfit_sol_cliente-anred,
        name1 TYPE zfit_sol_cliente-name1,
        name2 TYPE zfit_sol_cliente-name2,
        sort1 TYPE zfit_sol_cliente-sort1,
        direc1 TYPE zfit_sol_cliente-direc1,
        house_num1 TYPE zfit_sol_cliente-house_num1,
        cod_post TYPE zfit_sol_cliente-cod_post,
        poblac1 TYPE zfit_sol_cliente-poblac1,
        pais TYPE zfit_sol_cliente-pais,
        region TYPE zfit_sol_cliente-region,
        langu TYPE zfit_sol_cliente-langu,
        tel TYPE zfit_sol_cliente-tel,
        mob_numb TYPE zfit_sol_cliente-mob_numb,
        fax TYPE zfit_sol_cliente-fax,
        mail TYPE zfit_sol_cliente-mail,
        brsch TYPE zfit_sol_cliente-brsch,
        nif TYPE zfit_sol_cliente-nif,
        nif2 TYPE zfit_sol_cliente-nif2,
        stceg TYPE zfit_sol_cliente-stceg,
        dtams TYPE zfit_sol_cliente-dtams,
        contac_name1 TYPE zfit_sol_cliente-contac_name1,
        contac_name2 TYPE zfit_sol_cliente-contac_name2,
        akont TYPE zfit_sol_cliente-akont,
        fdgrv TYPE zfit_sol_cliente-fdgrv,
        altkn TYPE zfit_sol_cliente-altkn,
        zterm TYPE zfit_sol_cliente-zterm,
        sol_cred TYPE zfit_sol_cliente-sol_cred,
        zahls TYPE zfit_sol_cliente-zahls,
        zwels TYPE zfit_sol_cliente-zwels,
        mahna TYPE zfit_sol_cliente-mahna,
        knrma TYPE zfit_sol_cliente-knrma,
        mansp TYPE zfit_sol_cliente-mansp,
        busab TYPE zfit_sol_cliente-busab,
        vrsnr TYPE zfit_sol_cliente-vrsnr,
        witht TYPE zfit_sol_cliente-witht,
        wt_withcd TYPE zfit_sol_cliente-wt_withcd,
        wt_agent TYPE zfit_sol_cliente-wt_agent,
        wt_agtdf TYPE char10,
        wt_agtdt TYPE char10,
        waers TYPE zfit_sol_cliente-waers,
        sirenha TYPE zfit_sol_cliente-sirenha,
        direc2 TYPE zfit_sol_cliente-direc2,
        poblac2 TYPE zfit_sol_cliente-poblac2,
        cod_post2 TYPE zfit_sol_cliente-cod_post2,
        pais2 TYPE zfit_sol_cliente-pais2,
        comnt TYPE zfit_sol_cliente-comnt,
        modocom TYPE zfit_sol_cliente-modocom,
        zzncftc TYPE zfit_sol_cliente-zzncftc,
        mail2 TYPE zfit_sol_cliente-mail2,
      END OF t_tblcli2.

DATA: BEGIN OF *t_tblcli OCCURS 0,
  sel.
        INCLUDE STRUCTURE zfit_sol_cliente.
DATA END OF *t_tblcli.

RANGES r_bukrs_aut FOR t001-bukrs.
RANGES r_bukrs_naut FOR t001-bukrs.

DATA:     g_tblcli_wa     LIKE t_tblcli. "work area
DATA:     g_tblcli_copied.           "copy flag

CONTROLS: tblcli TYPE TABLEVIEW USING SCREEN 0100.
CONTROLS: tblcli3 TYPE TABLEVIEW USING SCREEN 0200.

DATA ok_code LIKE sy-ucomm.
DATA sel.

DATA  lv_lin TYPE i.

*       Batchinputdata of single transaction
DATA:   bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
*       error session opened (' ' or 'X')
DATA: BEGIN OF lt_log OCCURS 0,
         msgid  LIKE sy-msgid,
         msgtyp  LIKE sy-msgty,
         msgnr  LIKE sy-msgno,
         msgv1  LIKE sy-msgv1,
         msgv2  LIKE sy-msgv2,
         msgv3  LIKE sy-msgv3,
         msgv4  LIKE sy-msgv4,
         lineno LIKE mesg-zeile,
       END OF lt_log.

DATA answer TYPE c.

DATA: ano(4),
      mes(2),
      dia(2).
DATA lv_cursor_field TYPE name_komp.
DATA: lv_subrc TYPE sysubrc.
DATA:    ok-code(5)     TYPE c,
         imp TYPE c,
         mail TYPE c,
         fx TYPE c.
DATA: lv_verif TYPE i,
      lv_verif_t(80).
FIELD-SYMBOLS: <itab>  TYPE ANY,
               <field> TYPE ANY.

"DTT - GAP016_BP
TYPES:
  BEGIN OF ty_result,
    success TYPE abap_bool,
    partner TYPE bu_partner,
    message TYPE string,
    return  TYPE bapiretm,
  END OF ty_result,
  BEGIN OF ty_context,
    valid         TYPE abap_bool,
    message       TYPE string,
    bp_task       TYPE c LENGTH 1,
    customer_task TYPE c LENGTH 1,
    address_task  TYPE c LENGTH 1,
    company_task  TYPE c LENGTH 1,
    partner_guid  TYPE but000-partner_guid,
    address_guid  TYPE but020-address_guid,
    customer      TYPE kunnr,
  END OF ty_context.

CONSTANTS:
  gc_task_insert     TYPE c LENGTH 1 VALUE 'I',
  gc_task_UPDATE     TYPE c LENGTH 1 VALUE 'U',
  gc_task_modify     TYPE c LENGTH 1 VALUE 'M',
  gc_task_delete     TYPE c LENGTH 1 VALUE 'D',
  gc_role_flCU00     TYPE bu_partnerrole VALUE 'FLCU00',
  gc_role_flCU01     TYPE bu_partnerrole VALUE 'FLCU01',
  gc_bp_org          TYPE bu_type VALUE '2',
  gc_rfc_generico_mx TYPE bptaxnumxl VALUE 'XEXX010101000',
  gc_group_zint      TYPE bu_group VALUE 'ZINT',
  gc_koart_debtor    TYPE koart VALUE 'D'.

DATA:
      gv_customer TYPE kunnr.
"DTT - GAP016_BP

PARAMETERS: p_new TYPE check USER-COMMAND new.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
* Inicio modif Genis 19.07.2007
SELECT-OPTIONS so_kunnr FOR zfit_sol_cliente-kunnr NO-DISPLAY.
SELECT-OPTIONS so_kusap FOR zfit_sol_cliente-partner.
* Fin modif Genis 19.07.2007
SELECT-OPTIONS so_fecha FOR zfit_sol_cliente-fecha.
SELECT-OPTIONS so_bukrs FOR knb1-bukrs NO INTERVALS.
SELECT-OPTIONS so_name1 FOR kna1-name1 NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS so_land1 FOR kna1-land1 NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS so_nif FOR kna1-stcd1 NO INTERVALS.
SELECT-OPTIONS so_est FOR zfit_sol_cliente-estado NO-EXTENSION NO INTERVALS DEFAULT 'S'..
** Modificacion Alex Santamaria - 03.11.2011
PARAMETER p_ctmode TYPE c DEFAULT 'N' NO-DISPLAY.
** FIN - Modificacion Alex Santamaria - 03.11.2011
SELECTION-SCREEN END OF BLOCK b1.

PARAMETERS: p_import TYPE check USER-COMMAND import.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-008.
PARAMETERS: p_file LIKE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK b2.

*&---------------------------------------------------------------------*
*&  Include           ZFI0011EVT
*&---------------------------------------------------------------------*

**************************** START-OF-SELECTION *************************

AT SELECTION-SCREEN OUTPUT.
*    SET PF-STATUS '1000'.
  IF p_new = 'X'.
    p_new = 'X'.
    CLEAR t_tblcli.
    REFRESH t_tblcli.
    CLEAR lv_ini.
    CALL SCREEN '0200'.
    CLEAR p_new.
  ENDIF.
  IF p_import = ' '.
    LOOP AT SCREEN.
      IF screen-name CS 'P_FILE' OR screen-name CS 'P_BROW' OR
         screen-name CS 'P_EROW'.
        screen-input = 0.
        screen-invisible = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-name CS 'P_FILE' OR screen-name CS 'P_BROW' OR
         screen-name CS 'P_EROW'.
        screen-input = 1.
        screen-invisible = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.


AT SELECTION-SCREEN .

  if not so_bukrs is INITIAL.
    set PARAMETER ID 'BUK' FIELD so_bukrs-low.
  ENDIF.
  CASE sy-ucomm.

    WHEN 'ONLI'.
*leemos las solicitudes
      CLEAR p_new.
      IF p_import = ' '.
        IF so_bukrs IS INITIAL.
          SET CURSOR FIELD 'SO_BUKRS-LOW'.
          MESSAGE e055(00).
        ENDIF.
        PERFORM extraer_datos.
        PERFORM llamar_dynpro.
      ELSE.
        IF p_file IS INITIAL.
          SET CURSOR FIELD 'P_FILE'.
          MESSAGE e055(00).
        ENDIF.
        PERFORM import_excel.
        PERFORM llamar_dynpro.
      ENDIF.
    WHEN 'NEW'.

    WHEN 'BACK' OR 'LEAV' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = 'C:\'
      mask             = ',*.xls,*.xls.'
      mode             = 'O'
      title            = text-025 "'Seleccione un archivo'
    IMPORTING
      filename         = p_file
      rc               = lv_subrc
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

*&---------------------------------------------------------------------*
*&  Include           ZFI0011PBO
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  tblcli_move  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tblcli_move OUTPUT.

  DATA: wa_tc_tabla TYPE  cxtab_column.

  SELECT SINGLE ddtext FROM dd07v INTO g_tblcli_wa-estadot
                        WHERE domname = 'ZZDCLIEST'
                          AND ddlanguage = sy-langu
                          AND domvalue_l = g_tblcli_wa-estado.

  SELECT SINGLE butxt FROM t001 INTO g_tblcli_wa-butxt
                     WHERE bukrs = g_tblcli_wa-bukrs.

  IF g_tblcli_wa-sort1 IS INITIAL.
    g_tblcli_wa-sort1 = g_tblcli_wa-name1.
  ENDIF.

  MOVE-CORRESPONDING g_tblcli_wa TO t_tblcli.

  PERFORM derivar_akont CHANGING g_tblcli_wa.
  IF tblcli-current_line GT 0.
    MODIFY t_tblcli FROM g_tblcli_wa INDEX tblcli-current_line
    TRANSPORTING akont.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-name = 'G_TBLCLI_WA-SOLNUM'
*    OR screen-name = 'G_TBLCLI_WA-KUNNR'
    OR screen-name = 'G_TBLCLI_WA-FECHA'
    OR screen-name = 'G_TBLCLI_WA-HORA'
    OR screen-name = 'G_TBLCLI_WA-BUKRS'
    OR screen-name = 'G_TBLCLI_WA-BUTXT'
    OR screen-name = 'G_TBLCLI_WA-ESTADOT'
      or screen-name = 'G_TBLCLI_WA-AKONT'
    OR screen-name = 'G_TBLCLI_WA-VBUND' AND
      ( g_tblcli_wa-bu_group = '1020' OR g_tblcli_wa-bu_group = '1910' ).
*      OR
*       screen-name = 'G_TBLCLI_WA-partner' OR
*       screen-name = 'G_TBLCLI_WA-KTOKD'.
      screen-input = ' '.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'G_TBLCLI_WA-BUSAB' OR
       screen-name = 'G_TBLCLI_WA-BRSCH'.
      screen-invisible = 0.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

  IF NOT p_import IS INITIAL.
    LOOP AT tblcli-cols INTO wa_tc_tabla.
      IF
*        wa_tc_tabla-screen-name = 'G_TBLCLI_WA-partner' OR
         wa_tc_tabla-screen-name = 'G_TBLCLI_WA-BZIRK' OR
         wa_tc_tabla-screen-name = 'G_TBLCLI_WA-VKBUR' OR
         wa_tc_tabla-screen-name = 'G_TBLCLI_WA-VKGRP' OR
         wa_tc_tabla-screen-name = 'G_TBLCLI_WA-KDGRP' OR
         wa_tc_tabla-screen-name = 'G_TBLCLI_WA-WAERSD' OR
         wa_tc_tabla-screen-name = 'G_TBLCLI_WA-KONDA' OR
         wa_tc_tabla-screen-name = 'G_TBLCLI_WA-PVKSM' OR
         wa_tc_tabla-screen-name = 'G_TBLCLI_WA-VSORT' OR
         wa_tc_tabla-screen-name = 'G_TBLCLI_WA-KTGRD' OR
         wa_tc_tabla-screen-name = 'G_TBLCLI_WA-INCO1' OR
         wa_tc_tabla-screen-name = 'G_TBLCLI_WA-MRNKZ' OR
         wa_tc_tabla-screen-name = 'G_TBLCLI_WA-PERFK' OR
         wa_tc_tabla-screen-name = 'G_TBLCLI_WA-PERRL' OR
         wa_tc_tabla-screen-name = 'G_TBLCLI_WA-BOKRE' OR
         wa_tc_tabla-screen-name = 'G_TBLCLI_WA-PRFRE' OR
         wa_tc_tabla-screen-name = 'G_TBLCLI_WA-ESTADOT'.
        wa_tc_tabla-invisible = 1.
        MODIFY tblcli-cols FROM wa_tc_tabla.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF g_tblcli_wa-estado = 'R' OR g_tblcli_wa-estado = 'V'
     OR g_tblcli_wa-estado = 'A'.
    LOOP AT SCREEN.
      screen-input = 0.
      IF screen-name = 'G_TBLCLI_WA-SEL' AND g_tblcli_wa-estado = 'A'.
        screen-input = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
  LOOP AT tblcli-cols INTO wa_tc_tabla.
    IF wa_tc_tabla-screen-name = 'G_TBLCLI_WA-NIF2' OR
       wa_tc_tabla-screen-name = 'G_TBLCLI_WA-STCEG'.
      wa_tc_tabla-invisible = 'X'.
      MODIFY tblcli-cols FROM wa_tc_tabla.
    ENDIF.

    IF wa_tc_tabla-screen-name = 'G_TBLCLI_WA-BUSAB' OR
       wa_tc_tabla-screen-name = 'G_TBLCLI_WA-BRSCH'.
      CLEAR wa_tc_tabla-invisible.
      MODIFY tblcli-cols FROM wa_tc_tabla.
    ENDIF.

*** INICIO MODIFICACIÓN EMG 11/09/2008
    IF sy-tabix NE wa_tc_tabla-index.
      wa_tc_tabla-index = sy-tabix.
      MODIFY tblcli-cols FROM wa_tc_tabla.
    ENDIF.
*** FIN MODIFICACIÓN EMG 11/09/2008
  ENDLOOP.

ENDMODULE.                    "TBLCLI_MOVE OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR 'ZFI0011'.
ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '0200'.
  SET TITLEBAR 'ZFI0011'.
ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  init  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init OUTPUT.
  DESCRIBE TABLE t_tblcli LINES lv_lin.

  tblcli-lines = lv_lin.

  IF NOT lv_cursor_field IS INITIAL.
    SET CURSOR FIELD lv_cursor_field LINE lv_verif.
    CLEAR lv_cursor_field.
  ENDIF.
ENDMODULE.                 " init  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'MODOCOM'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.                 " STATUS_9001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  tblcli_move2  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tblcli_move3 OUTPUT.


  SELECT SINGLE ddtext FROM dd07v INTO g_tblcli_wa-estadot
                        WHERE domname = 'ZZDCLIEST'
                          AND ddlanguage = sy-langu
                          AND domvalue_l = g_tblcli_wa-estado.
  SELECT SINGLE butxt FROM t001 INTO g_tblcli_wa-butxt
                     WHERE bukrs = g_tblcli_wa-bukrs.

  g_tblcli_wa-hora = sy-uzeit.
  g_tblcli_wa-fecha = sy-datum.
  IF g_tblcli_wa-sort1 IS INITIAL.
    g_tblcli_wa-sort1 = g_tblcli_wa-name1.
  ENDIF.
  MOVE-CORRESPONDING g_tblcli_wa TO t_tblcli.
  LOOP AT SCREEN.
    IF screen-name = 'G_TBLCLI_WA-SOLNUM' OR
       screen-name = 'G_TBLCLI_WA-KUNNR' OR
      screen-name = 'G_TBLCLI_WA-FECHA' OR
      screen-name = 'G_TBLCLI_WA-HORA' OR
*       screen-name = 'G_TBLCLI_WA-BUKRS' OR
      screen-name = 'G_TBLCLI_WA-BUTXT' OR
       screen-name = 'G_TBLCLI_WA-ESTADOT' OR
     screen-name = 'G_TBLCLI_WA-AKONT' OR
       screen-name = 'G_TBLCLI_WA-VBUND' AND
       ( g_tblcli_wa-bu_group = '1020' OR g_tblcli_wa-bu_group = '1910' ).
*      OR
*       screen-name = 'G_TBLCLI_WA-KUNNRSAP' OR
*       screen-name = 'G_TBLCLI_WA-KTOKD'.
      screen-input = ' '.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


  IF g_tblcli_wa-estado = 'R' OR g_tblcli_wa-estado = 'V'
     OR g_tblcli_wa-estado = 'A'.
    LOOP AT SCREEN.
      screen-input = ''.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
  LOOP AT tblcli3-cols INTO wa_tc_tabla.
    IF wa_tc_tabla-screen-name = 'G_TBLCLI_WA-NIF2' OR
       wa_tc_tabla-screen-name = 'G_TBLCLI_WA-STCEG'.
      wa_tc_tabla-invisible = 1.
      MODIFY tblcli3-cols FROM wa_tc_tabla.
    ENDIF.
  ENDLOOP.

ENDMODULE.                 " tblcli_move2  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  init3  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init3 OUTPUT.

  IF lv_ini IS INITIAL.
    CLEAR t_tblcli.
    APPEND t_tblcli.
    lv_ini = 1.
  ENDIF.

  DESCRIBE TABLE t_tblcli LINES lv_lin.

  tblcli3-lines = lv_lin.

  IF NOT lv_cursor_field IS INITIAL.
    SET CURSOR FIELD lv_cursor_field LINE lv_verif.
    CLEAR lv_cursor_field.
  ENDIF.

ENDMODULE.                 " init3  OUTPUT

*----------------------------------------------------------------------*
***INCLUDE ZFI0011PAI .
*------ ----------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code.

    WHEN 'SAVE'.
      PERFORM grabar_sel.

    WHEN 'RECHAZAR'.
      PERFORM rechazar.

    WHEN 'RECHAZARM'.
      PERFORM rechazarm.

    WHEN 'SEL'.
      LOOP AT t_tblcli.
        t_tblcli-sel = 'X'.
        MODIFY t_tblcli.
      ENDLOOP.

    WHEN 'DESEL'.
      LOOP AT t_tblcli.
        t_tblcli-sel = ' '.
        MODIFY t_tblcli.
      ENDLOOP.
    WHEN 'CREA'.
      lv_verif = 0.

      PERFORM verif.
      IF NOT lv_verif IS INITIAL.   " Linea error

        PERFORM crear_message.

      ELSE.
        PERFORM call_batch.
      ENDIF.
    WHEN 'DESARCHIVA'.
      PERFORM desarchivar.

    WHEN '&XXL'.
      PERFORM download_xls.
  ENDCASE.
  CLEAR ok_code.
ENDMODULE.                 " USER_COMMAND_0100  INPUT'
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.

    WHEN 'SEL'.
      LOOP AT t_tblcli.
        t_tblcli-sel = 'X'.
        MODIFY t_tblcli.
      ENDLOOP.

    WHEN 'DESEL'.
      LOOP AT t_tblcli.
        t_tblcli-sel = ' '.
        MODIFY t_tblcli.
      ENDLOOP.
    WHEN 'NUEVO'.
      PERFORM nuevo.
    WHEN 'CREA'.
      lv_verif = 0.

      PERFORM verif.
      IF NOT lv_verif IS INITIAL.   " Linea error

        PERFORM crear_message.

      ELSE.
        PERFORM call_batch.
*        if sy-subrc = 0.
*          loop at t_tblcli where sel = 'X'.
*            delete t_tblcli.
*          endloop.
*        endif.
      ENDIF.
  ENDCASE.

  CLEAR ok_code.
ENDMODULE.                 " USER_COMMAND_0200  INPUT'
*&---------------------------------------------------------------------*
*&      Module  tratar_datos  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tratar_datos INPUT.

  MODIFY t_tblcli FROM g_tblcli_wa INDEX tblcli-current_line.

ENDMODULE.                 " tratar_datos  INPUT
*----------------------------------------------------------------------*
*  MODULE tratar_datos2 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE tratar_datos2 INPUT.
  IF NOT g_tblcli_wa-waers IS INITIAL.
    SELECT SINGLE waers
      FROM tcurc
      INTO knb1-waers
      WHERE waers = g_tblcli_wa-waers.

    IF sy-subrc <> 0.
      MESSAGE e216(zfi01).
*   No existe la moneda
    ENDIF.
  ENDIF.

  MODIFY t_tblcli FROM g_tblcli_wa INDEX tblcli3-current_line.

ENDMODULE.                 " tratar_datos  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0100 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " EXIT_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0200 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " EXIT_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_KNB1-AKONT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_knb1-akont INPUT.

* Lokale Daten.
  DATA akont LIKE knb1-akont.
  IF t_tblcli-bukrs IS INITIAL.
    SET CURSOR FIELD t_tblcli-bukrs.
    MESSAGE i168(zfi01).
    EXIT.
  ENDIF.
* Inhalt des Feldes 'KNB1-AKONT' vom Dynpro besorgen.
  CLEAR   dynpfields.
  REFRESH dynpfields.
  dynpfields-fieldname = 'T_TBLCLI-AKONT'.
  APPEND dynpfields.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = 'SAPMF02D'
      dynumb     = '0210'
    TABLES
      dynpfields = dynpfields
    EXCEPTIONS
      OTHERS     = 4.
  IF sy-subrc = 0.
    READ TABLE dynpfields INDEX 1.
    akont = dynpfields-fieldvalue.
    TRANSLATE akont TO UPPER CASE.                       "#EC TRANSLANG
  ENDIF.

  LOOP AT SCREEN.
    CHECK screen-name = 'T_TBLCLI-AKONT'.
    IF screen-input = '0'.
      char1 = 'X'.
    ELSE.
      char1 = space.
    ENDIF.
    EXIT.
  ENDLOOP.

* Abstimmkonten anzeigen.
  CALL FUNCTION 'FI_F4_AKONT'
    EXPORTING
      i_bukrs = t_tblcli-bukrs
      i_mitkz = 'D'
      i_akont = akont
      i_xshow = char1
    IMPORTING
      e_akont = *t_tblcli-akont.
  IF NOT *t_tblcli-akont IS INITIAL.
    g_tblcli_wa-akont = *t_tblcli-akont.
  ENDIF.

ENDMODULE.                 " HELP_KNB1-AKONT  INPUT
*&---------------------------------------------------------------------*
*&      Module  help_knvv-vkbur  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_knvv-vkbur INPUT.
  DATA: lv_vkorg TYPE vkorg,
        lv_vtweg TYPE vtweg,
        lv_spart TYPE spart.
  DATA: BEGIN OF inttab OCCURS 10,
          vkbur LIKE tvkbt-vkbur,
          bezei LIKE tvkbt-bezei.
  DATA: END OF inttab.

*------- FIELDS füllen -------------------------------------------------
  CLEAR   fields.
  REFRESH fields.
  fields-tabname    = 'TVKBT'. fields-fieldname  = 'VKBUR'.
  fields-selectflag = 'X'.     APPEND fields.
  fields-tabname    = 'TVKBT'. fields-fieldname  = 'BEZEI'.
  fields-selectflag = ' '.     APPEND fields.

*------- VALUETAB füllen -----------------------------------------------
  REFRESH inttab.
  SELECT SINGLE vkorg  INTO lv_vkorg FROM tvko WHERE bukrs = t_tblcli-bukrs.
  SELECT SINGLE vtweg spart INTO (lv_vtweg, lv_spart) FROM tvta WHERE vkorg = lv_vkorg.
  SELECT * FROM tvkbz WHERE vkorg = lv_vkorg
                      AND   vtweg = lv_vtweg
                      AND   spart = lv_spart.
    LOOP AT inttab WHERE vkbur = tvkbz-vkbur.
      EXIT.
    ENDLOOP.
    CHECK NOT sy-subrc IS INITIAL.
    SELECT * FROM tvkbt WHERE spras = sy-langu
                        AND   vkbur = tvkbz-vkbur.
      MOVE-CORRESPONDING tvkbt TO inttab.
      APPEND inttab.
    ENDSELECT.
  ENDSELECT.
  SELECT * FROM tvta WHERE vkorg = lv_vkorg
                     AND   vtwku = lv_vtweg
                     AND   spaku = lv_spart.
    SELECT * FROM tvkbz WHERE vkorg = lv_vkorg
                        AND   vtweg = tvta-vtweg
                        AND   spart = tvta-spart.
      LOOP AT inttab WHERE vkbur = tvkbz-vkbur.
        EXIT.
      ENDLOOP.
      CHECK NOT sy-subrc IS INITIAL.
      SELECT * FROM tvkbt WHERE spras = sy-langu
                          AND   vkbur = tvkbz-vkbur.
        MOVE-CORRESPONDING tvkbt TO inttab.
        APPEND inttab.
      ENDSELECT.
    ENDSELECT.
  ENDSELECT.
  CLEAR   valuetab.
  REFRESH valuetab.
  LOOP AT inttab.
    valuetab-value = inttab-vkbur. APPEND valuetab.
    valuetab-value = inttab-bezei. APPEND valuetab.
  ENDLOOP.

*------ Status (Anzeigen, Ändern, Hinzufügen) ermitteln ----------------
  LOOP AT SCREEN.
    CHECK screen-name = 'T_TBLCLI-VKBUR'.
    IF screen-input = '0'.
      char1 = 'X'.
    ELSE.
      char1 = space.
    ENDIF.
    EXIT.
  ENDLOOP.

*------ Eingabemöglichkeiten anzeigen ----------------------------------
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
    EXPORTING
      display      = char1
      fieldname    = 'VKBUR'
      tabname      = 'KNVV'
    IMPORTING
      select_value = g_tblcli_wa-vkbur
    TABLES
      fields       = fields
      valuetab     = valuetab.

ENDMODULE.                 " help_knvv-vkbur  INPUT
*&---------------------------------------------------------------------*
*&      Module  TRATAR_DATOS_modocom  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tratar_datos_modocom INPUT.
  CASE ok-code.
    WHEN 'OK'.
      IF imp = 'X' AND mail IS INITIAL AND fx IS INITIAL.
        g_tblcli_wa-modocom = 'I'.
      ELSEIF imp = 'X' AND mail = 'X' AND fx IS INITIAL.
        g_tblcli_wa-modocom = 'IE'.
      ELSEIF imp = 'X' AND mail = 'X' AND fx = 'X'.
        g_tblcli_wa-modocom = 'IEF'.
      ELSEIF imp = 'X' AND mail IS INITIAL AND fx = 'X'.
        g_tblcli_wa-modocom = 'IF'.
      ELSEIF imp  IS INITIAL AND mail = 'X' AND fx = 'X'.
        g_tblcli_wa-modocom = 'EF'.
      ELSEIF imp IS INITIAL AND mail = 'X' AND fx IS INITIAL.
        g_tblcli_wa-modocom = 'E'.
      ELSEIF imp IS INITIAL AND mail IS INITIAL AND fx = 'X'.
        g_tblcli_wa-modocom = 'F'.
      ENDIF.
      LEAVE SCREEN.
  ENDCASE.
ENDMODULE.                 " TRATAR_DATOS_modocom  INPUT
*&---------------------------------------------------------------------*
*&      Module  help_knb1-modocom  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_knb1-modocom INPUT.
  CALL SCREEN 9001 STARTING AT 03 01.
ENDMODULE.                 " help_knb1-modocom  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_cliente_vs_soc  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_cliente_vs_soc INPUT.
*  PERFORM f_check_kunnr_vs_bukrs.
ENDMODULE.                 " check_cliente_vs_soc  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_waers  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_waers INPUT.
  IF NOT g_tblcli_wa-waers IS INITIAL.
    SELECT SINGLE waers
      FROM tcurc
      INTO knb1-waers
      WHERE waers = g_tblcli_wa-waers.

    IF sy-subrc <> 0.
      MESSAGE e216(zfi01).
*   No existe la moneda
    ENDIF.

  ENDIF.

ENDMODULE.                 " check_waers  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_NIF_NIF3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_nif_nif3 INPUT.

  IF g_tblcli_wa-sel = 'X'.

    IF g_tblcli_wa-nif = 'XEXX010101000' AND
       g_tblcli_wa-nif3 IS INITIAL.

      MESSAGE e002(zfi).

    ENDIF.

  ENDIF.

ENDMODULE.

*----------------------------------------------------------------------*
***INCLUDE ZFI0011F01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  extraer_datos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM extraer_datos.

  CLEAR: zfit_sol_cliente,t_tblcli, nriv, kna1.
  REFRESH t_tblcli.
  CLEAR: r_bukrs_aut, r_bukrs_naut.
  REFRESH: r_bukrs_aut, r_bukrs_naut.
  IF sy-uname <> 'DLOPEZPEREZ'.
    SELECT * FROM t001  WHERE bukrs IN so_bukrs.
* valida permiso para la sociedad seleccionada
      AUTHORITY-CHECK OBJECT 'ZAUT_BUKRS'
          ID 'BUKRS' FIELD t001-bukrs
          ID 'ACTVT' FIELD '03'.
      IF sy-subrc <> 0.
        r_bukrs_naut-sign = 'I'.
        r_bukrs_naut-option = 'EQ'.
        r_bukrs_naut-low = t001-bukrs.
        APPEND r_bukrs_naut.
      ELSE.
        r_bukrs_aut-sign = 'I'.
        r_bukrs_aut-option = 'EQ'.
        r_bukrs_aut-low = t001-bukrs.
        APPEND r_bukrs_aut.
      ENDIF.
    ENDSELECT.
    IF NOT r_bukrs_naut[] IS INITIAL.
      LOOP AT r_bukrs_naut.
        MESSAGE i170(zfi01) WITH r_bukrs_naut-low.
*   No dispone de autorización para la sociedad &.
      ENDLOOP.
    ENDIF.
    IF r_bukrs_aut[] IS INITIAL.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  SELECT   *  FROM zfit_sol_cliente
                 WHERE fecha IN so_fecha
                   AND bukrs IN r_bukrs_aut
* Inicio modif Genis 19.07.2007
                   AND kunnr IN so_kunnr
                   AND partner IN so_kusap
* Fin modif Genis 19.07.2007
                   AND name1 IN so_name1
                   AND pais IN so_land1
                   AND nif   IN so_nif
                   AND estado IN so_est.

    MOVE-CORRESPONDING zfit_sol_cliente TO t_tblcli.
    IF t_tblcli-waers IS INITIAL AND t_tblcli-bukrs IS NOT INITIAL.
      SELECT SINGLE waers FROM t001 INTO t_tblcli-waers
                       WHERE bukrs = t_tblcli-bukrs.
    ENDIF.

    APPEND t_tblcli.
  ENDSELECT.



ENDFORM.                    " extraer_datos

*&---------------------------------------------------------------------*
*&      Form  llamar_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM llamar_dynpro.

*Ini-MGGM-07.11.2013 - Se borran facturas del reporte con datos constantes
* sociedad 1102 y fechas del 25 y 27 de mayo del 2013.
  LOOP AT t_tblcli.
    IF t_tblcli-bukrs = '1102' AND ( t_tblcli-fecha = '20130525' OR
                                   t_tblcli-fecha = '20130527' ).
      DELETE t_tblcli.
    ENDIF.
  ENDLOOP.
*Fin-MGGM-07.11.2013
  IF t_tblcli[] IS INITIAL.
    MESSAGE s024(zfi01).
  ELSE.
    SORT t_tblcli BY bukrs partner solnum.
    CALL SCREEN '0100'.
  ENDIF.

ENDFORM.                    " llamar_dynpro

*&---------------------------------------------------------------------*
*&      Form  grabar_sel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM grabar_sel.

  LOOP AT t_tblcli WHERE sel = 'X'.
    MOVE-CORRESPONDING t_tblcli TO zfit_sol_cliente.
    MODIFY zfit_sol_cliente.
    IF sy-subrc = 0.
      MESSAGE s022(zfi01).
    ENDIF.
  ENDLOOP.
  IF sy-subrc <> 0.
    MESSAGE w031(zfi01).
  ENDIF.


ENDFORM.                    " grabar_sel

*&---------------------------------------------------------------------*
*&      Form  CALL_BATCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*INI GAP016_BP
FORM call_batch.

  CLEAR: messtab, lt_log.
  REFRESH: messtab, lt_log.

  LOOP AT t_tblcli WHERE sel = 'X'.

    PERFORM derivar_akont CHANGING t_tblcli.

    IF t_tblcli-bukrs IS NOT INITIAL
    AND t_tblcli-akont IS INITIAL.
      lt_log-msgtyp = 'E'.
      lt_log-msgid  = 'ZFI01'.
      lt_log-msgnr  = '015'.
      lt_log-msgv1  = t_tblcli-bukrs.
      APPEND lt_log.
      CONTINUE.
    ENDIF.

    PERFORM f_check_nif.

    IF answer <> 'J'.
      CONTINUE.
    ENDIF.

    IF t_tblcli-partner IS INITIAL.
      PERFORM f_bp_create CHANGING lv_subrc.
    ELSE.
      PERFORM comprobar_partner.

      IF answer <> 'J'.
        CONTINUE.
      ENDIF.

      IF t_tblcli-bukrs IS NOT INITIAL.
        PERFORM f_bp_extend_flcu00 CHANGING lv_subrc.
      ELSE.
        CLEAR lv_subrc.
      ENDIF.
    ENDIF.

    IF lv_subrc = 0.
      t_tblcli-estado = 'V'.
      t_tblcli-sel = space.

      SELECT SINGLE ddtext
        FROM dd07v
        INTO @t_tblcli-estadot
        WHERE domname    = 'ZZDCLIEST'
          AND ddlanguage = @sy-langu
          AND domvalue_l = @t_tblcli-estado.

      MODIFY t_tblcli.

      MOVE-CORRESPONDING t_tblcli TO zfit_sol_cliente.

      IF p_new IS INITIAL.
        MODIFY zfit_sol_cliente.
      ENDIF.
    ENDIF.

  ENDLOOP.

  CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
    TABLES
      i_message_tab = lt_log.

ENDFORM.
*FIN GAP016_BP
*FORM call_batch.
*
*  CLEAR: messtab, lt_log.
*  REFRESH: messtab, lt_log.
*
*  LOOP AT t_tblcli WHERE sel = 'X'.
*
** Inicio modif Genis 19.07.2007
*    IF p_new IS INITIAL.
*      SELECT SINGLE cli_pms
*        FROM zficonv_cli_pms
*        INTO t_tblcli-kunnr
*        WHERE cli_pms = t_tblcli-kunnr
*          AND hotel = t_tblcli-bukrs.
*
*      IF sy-subrc = 0.
*        lt_log-msgtyp = 'E'.
*        lt_log-msgid = 'ZFI01'.
*        lt_log-msgnr = '112'.
*        lt_log-msgv1 = t_tblcli-kunnr.
*        lt_log-msgv2 = t_tblcli-bukrs.
*        APPEND lt_log.
*        EXIT.
*      ENDIF.
*    ENDIF.
** Fin modif Genis 19.07.2007
*
*    IF t_tblcli-kunnrsap IS INITIAL OR t_tblcli-ktokd = '1010'.
*
**     Validamos el NIF: Si ya existe un cliente con el mismo NIF (Warning)
*      PERFORM f_check_nif.
*      IF answer <> 'J'.
*        CONTINUE.
*      ENDIF.
*
**     SI EL CLIENTE NO ES CONOCIDO... (No existe en SAP)
**** INICIO MODIFICACIÓN EMG 06/10/2008
**      IF t_tblcli-ktokd = 'ZTER'.
*      IF t_tblcli-ktokd EQ '1020' OR
*         t_tblcli-ktokd EQ '1040' OR t_tblcli-ktokd EQ '1910'.
**** FIN MODIFICACIÓN EMG 06/10/2008
**   .... Asignación externa de número sap para los Terceros
*        PERFORM f_number_get_next_debitor CHANGING lv_subrc.
*        IF lv_subrc <> 0.
*          EXIT.
*        ENDIF.
*      ELSE.
**   .... Asignación interna de número sap para el resto
*      ENDIF.
*      IF t_tblcli-ktokd = '1040' AND t_tblcli-kunnrsap IS INITIAL.
*        lt_log-msgtyp = 'E'.
*        lt_log-msgid = 'ZFI01'.
*        lt_log-msgnr = '223'.
*        lt_log-msgv1 = t_tblcli-kunnrsap.
*        APPEND lt_log.
*      ELSE.
*
**----- CREAR CLIENTE DESCONOCIDO VIA CALL TRANSACTION ---------------------------
*        PERFORM f_bdc_create_unknowm_debitor.
*      ENDIF.
*
*    ELSE.
*
**     SI EL CLIENTE ES CONOCIDO... (Existe cliente en Sap)
*      SELECT SINGLE * FROM knb1 WHERE kunnr = t_tblcli-kunnrsap.
*      IF sy-subrc <> 0.
*        lt_log-msgtyp = 'E'.
*        lt_log-msgid = 'ZFI01'.
*        lt_log-msgnr = '084'.
*        lt_log-msgv1 = t_tblcli-kunnrsap.
*        APPEND lt_log.
*      ELSE.
*
** Inicio modif. Genis 19.07.2007
*        SELECT SINGLE kunnr
*          FROM knb1
*          INTO t_tblcli-kunnrsap
*          WHERE kunnr = t_tblcli-kunnrsap
*            AND bukrs = t_tblcli-bukrs.
*
*        IF sy-subrc = 0.
**         Si el cliente ya existe en esa sociedad, modifca datos de sociedad
*          PERFORM f_bdc_modify_company_data.
*
*          READ TABLE messtab WITH KEY msgtyp = 'E'.
*          IF sy-subrc <> 0.
*            t_tblcli-estado = 'V'.
*
*            SELECT SINGLE ddtext
*              FROM dd07v
*              INTO t_tblcli-estadot
*              WHERE domname = 'ZZDCLIEST'
*                AND ddlanguage = sy-langu
*                AND domvalue_l = t_tblcli-estado.
*
*            t_tblcli-sel = ''.
*            MODIFY t_tblcli.
*            MOVE-CORRESPONDING t_tblcli TO zfit_sol_cliente.
*            IF p_new IS INITIAL.
*              MODIFY zfit_sol_cliente.
*            ENDIF.
*
*            MESSAGE s138(zfi01).
**           Se ha actualizado la tabla de conversion PMS
*
**            PERFORM crear_asoc_clientes.
*          ENDIF.
*        ELSE.
**  -------CREAR CLIENTE INFORMADO (AMPLIAR SOCIEDAD) VIA CALL TRANSACT. ----------
*          PERFORM f_bdc_create_known_debitor.
*
**          PERFORM crear_asoc_clientes.
*        ENDIF.
** Fin modif. Genis 19.07.2007
*
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*  CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
*    TABLES
*      i_message_tab = lt_log.
*
*ENDFORM.                    " CALL_BATCH

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
*FORM bdc_dynpro USING program dynpro.
*  CLEAR bdcdata.
*  bdcdata-program  = program.
*  bdcdata-dynpro   = dynpro.
*  bdcdata-dynbegin = 'X'.
*  APPEND bdcdata.
*ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
*FORM bdc_field USING fnam fval.
*  CLEAR bdcdata.
*  bdcdata-fnam = fnam.
*  bdcdata-fval = fval.
*  APPEND bdcdata.
*ENDFORM.                    "BDC_FIELD

*----------------------------------------------------------------------*
*   create batchinput session                                          *
*----------------------------------------------------------------------*
*FORM open_group                                             "#EC CALLED
*    USING i_group    LIKE apqi-groupid
*          i_user     LIKE apqi-userid
*          i_keep     LIKE apqi-qerase
*          i_holddate LIKE apqi-startdate.
** open batchinput group
*  CALL FUNCTION 'BDC_OPEN_GROUP'
*    EXPORTING
*      client   = sy-mandt
*      group    = i_group
*      user     = i_user
*      keep     = i_keep
*      holddate = i_holddate.
*ENDFORM.                    "OPEN_GROUP

*----------------------------------------------------------------------*
*   end batchinput session                                             *
*----------------------------------------------------------------------*
*FORM close_group.                                           "#EC CALLED
** close batchinput group
*  CALL FUNCTION 'BDC_CLOSE_GROUP'.
*ENDFORM.                    "CLOSE_GROUP

*&---------------------------------------------------------------------*
*&      Form  rechazar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM rechazar.
  LOOP AT t_tblcli WHERE sel = 'X'.
    MOVE-CORRESPONDING t_tblcli TO zfit_sol_cliente.
    zfit_sol_cliente-estado = 'A'.
    SELECT SINGLE ddtext FROM dd07v INTO zfit_sol_cliente-estadot
                      WHERE domname = 'ZZDCLIEST'
                        AND ddlanguage = sy-langu
                        AND domvalue_l = zfit_sol_cliente-estado.
    MODIFY zfit_sol_cliente.
    IF sy-subrc = 0.
      DELETE FROM zficonv_cli_pms
        WHERE hotel = t_tblcli-bukrs
          AND cli_pms = t_tblcli-kunnr.
      MESSAGE s022(zfi01).
      t_tblcli-estado = 'A'.
      DELETE t_tblcli.
    ENDIF.
  ENDLOOP.
  IF sy-subrc <> 0.
    MESSAGE w031(zfi01).
  ENDIF.
ENDFORM.                    " rechazar

*&---------------------------------------------------------------------*
*&      Form  f_check_nif
*&---------------------------------------------------------------------*
*   Validamos NIF: Si existe un cliente con el mismo NIF
*   --> Mostraremos un Mensaje para decidir si crear otro
*----------------------------------------------------------------------*
*INI GAP016_BP
FORM f_check_nif.

  DATA:
    lv_msg     TYPE char70,
    lv_partner TYPE bu_partner.

  CLEAR answer.

  IF t_tblcli-nif IS INITIAL.
    answer = 'J'.
    RETURN.
  ENDIF.

  SELECT SINGLE partner
    FROM dfkkbptaxnum
    INTO @lv_partner
    WHERE taxnumxl = @t_tblcli-nif
       OR taxnum   = @t_tblcli-nif.

  IF sy-subrc = 0
  AND lv_partner <> t_tblcli-partner.

    CONCATENATE TEXT-003 lv_partner TEXT-009 t_tblcli-nif
      INTO lv_msg SEPARATED BY space.

    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      EXPORTING
        diagnosetext1  = lv_msg
        textline1      = TEXT-004
        titel          = TEXT-005
        cancel_display = space
      IMPORTING
        answer         = answer.
  ELSE.
    answer = 'J'.
  ENDIF.

ENDFORM.
*FIN GAP016_BP
*FORM f_check_nif.
*
*  DATA lv_msg TYPE char70.
*  CLEAR: answer, kna1.
*
*  SELECT SINGLE * FROM kna1 WHERE stcd1 = t_tblcli-nif.
*  IF sy-subrc = 0.
*    CONCATENATE text-003 kna1-kunnr text-009 t_tblcli-nif  INTO lv_msg SEPARATED BY space.
*    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
*      EXPORTING
*        diagnosetext1  = lv_msg
*        textline1      = text-004
*        titel          = text-005
*        cancel_display = ' '
*      IMPORTING
*        answer         = answer.
*  ELSE.
*    answer = 'J'.
*  ENDIF.
*
*ENDFORM.                    " comprobar_nif

*&---------------------------------------------------------------------*
*&      Form  comprobar_partner
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM comprobar_partner.

  DATA lv_msg TYPE char70.
  CLEAR:  answer, kna1.

*  SELECT SINGLE * FROM kna1 WHERE kunnr = t_tblcli-kunnrsap.
  SELECT SINGLE *
    FROM but000
    WHERE partner = t_tblcli-partner.
  IF sy-subrc <> 0.
    CONCATENATE TEXT-006 t_tblcli-partner INTO lv_msg SEPARATED BY space.
    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      EXPORTING
        diagnosetext1 = lv_msg
        textline1     = TEXT-004
        titel         = TEXT-005
      IMPORTING
        answer        = answer.
  ELSE.
    answer = 'J'.
  ENDIF.

ENDFORM.                    " comprobar_partner

*&---------------------------------------------------------------------*
*&      Form  import_excel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM import_excel.

  DATA: lv_vkorg TYPE vkorg,
        lv_vtweg TYPE vtweg,
        lv_spart TYPE spart,
        cabecera TYPE c.

  CLEAR: t_tblcli, t_tblcli2, lt_log, index, index4.
  REFRESH: t_tblcli, t_tblcli2, lt_log.

* Field symbols.

  DATA: lv_txt   TYPE char80,
        lt_excel TYPE TABLE OF alsmex_tabline,
        ls_excel TYPE alsmex_tabline.

  CONSTANTS:
    lc_beg_col TYPE i VALUE '1',
    lc_end_col TYPE i VALUE '100'.

* Indicamos lo que hacemos por pantalla...
  CONCATENATE TEXT-026 p_file '...' INTO lv_txt
  SEPARATED BY space.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = lv_txt.

* Extraemos los datos del fichero...
  REFRESH lt_excel.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = lc_beg_col
      i_begin_row             = 2
      i_end_col               = lc_end_col
      i_end_row               = 60000
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc = 0.
* Extraemos la estructura de la tabla interna e insertamos los datos.
    ASSIGN t_tblcli2 TO <itab>.
    LOOP AT lt_excel INTO ls_excel.
      ASSIGN COMPONENT ls_excel-col OF STRUCTURE <itab> TO <field>.
      <field> = ls_excel-value.
      IF ls_excel-col = 1.
        APPEND t_tblcli2. CLEAR t_tblcli2.
      ELSE.
        CLEAR t_tblcli2.
        READ TABLE t_tblcli2 INDEX ls_excel-row.
        <field> = ls_excel-value.
        MODIFY t_tblcli2 INDEX ls_excel-row.
        CLEAR t_tblcli2.
      ENDIF.
    ENDLOOP.
  ENDIF.
  LOOP AT t_tblcli2.
    MOVE-CORRESPONDING t_tblcli2 TO t_tblcli.
*****    Comprobamos que no se lea la línea de cabecera
*    IF t_tblcli2-solnum = 'SOLNUM'.
*      cabecera = 'X'.
*      DELETE t_tblcli2.
*      CONTINUE.
*    ENDIF.
    t_tblcli-fecha = sy-datum.
    t_tblcli-hora = sy-uzeit.
    CLEAR: ano, mes, dia.
    ano = t_tblcli2-wt_agtdf+4(4).
    mes = t_tblcli2-wt_agtdf+2(2).
    dia = t_tblcli2-wt_agtdf(2).
    CONCATENATE ano mes dia INTO t_tblcli-wt_agtdf.
    CLEAR: ano, mes, dia.
    ano = t_tblcli2-wt_agtdt+4(4).
    mes = t_tblcli2-wt_agtdt+2(2).
    dia = t_tblcli2-wt_agtdt(2).
    CONCATENATE ano mes dia INTO t_tblcli-wt_agtdt.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = t_tblcli2-bukrs
      IMPORTING
        output = t_tblcli-bukrs.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = t_tblcli2-kunnr
*      IMPORTING
*        output = t_tblcli-kunnr.
    IF t_tblcli-waers IS INITIAL.
      SELECT SINGLE waers FROM t001 INTO t_tblcli-waers
                       WHERE bukrs = t_tblcli-bukrs.
    ENDIF.
    APPEND t_tblcli.
  ENDLOOP.


  LOOP AT t_tblcli.
    IF cabecera = 'X'.
      index = index + 2.
      CLEAR cabecera.
    ELSE.
      index = index + 1.
    ENDIF.


*****    Comprobamos que no esté la solicitud en sap
    SELECT SINGLE * FROM zfit_sol_cliente WHERE solnum = t_tblcli-solnum.
    IF sy-subrc <> 0.
*****    Comprobamos que la sociedad exista
      SELECT SINGLE * FROM t001 WHERE bukrs = t_tblcli-bukrs.
      IF sy-subrc <> 0.
        index4 = index.
        lt_log-msgtyp = 'E'.
        lt_log-msgid = 'ZFI01'.
        lt_log-msgnr = '046'.
        lt_log-msgv1 = t_tblcli-bukrs.
        lt_log-msgv2 = index4.
        APPEND lt_log.
        DELETE t_tblcli.
        CONTINUE.
      ENDIF.
*****    Comprobamos que el grupo de cuentas exista
*INI GAP016_BP
      SELECT SINGLE bu_group
        FROM tb001
        INTO @DATA(lv_bu_group)
        WHERE bu_group = @t_tblcli-bu_group.

      IF sy-subrc <> 0.
        index4 = index.
        lt_log-msgtyp = 'E'.
        lt_log-msgid  = 'ZFI01'.
        lt_log-msgnr  = '046'.
        lt_log-msgv1  = t_tblcli-bu_group.
        lt_log-msgv2  = index4.
        APPEND lt_log.
        DELETE t_tblcli.
        CONTINUE.
      ENDIF.
*FIN GAP016_BP
*      SELECT SINGLE * FROM t077d WHERE ktokd = t_tblcli-ktokd.
*      IF sy-subrc <> 0.
*        index4 = index.
*        lt_log-msgtyp = 'E'.
*        lt_log-msgid = 'ZFI01'.
*        lt_log-msgnr = '046'.
*        lt_log-msgv1 = t_tblcli-ktokd.
*        lt_log-msgv2 = index4.
*        APPEND lt_log.
*        DELETE t_tblcli.
*        CONTINUE.
*      ENDIF.
*****    Comprobamos Org.ventas /Canal distribución/ Sector.
*      SELECT SINGLE vkorg  INTO lv_vkorg FROM tvko WHERE bukrs = t_tblcli-bukrs.
*      SELECT SINGLE vtweg spart INTO (lv_vtweg, lv_spart) FROM tvta WHERE vkorg = lv_vkorg.
*
*      IF lv_vkorg IS INITIAL AND lv_vtweg IS INITIAL AND lv_spart IS INITIAL.
*        index4 = index.
*        lt_log-msgtyp = 'E'.
*        lt_log-msgid = 'ZFI01'.
*        lt_log-msgnr = '049'.
*        lt_log-msgv1 = t_tblcli-bukrs.
*        lt_log-msgv2 = index4.
*        APPEND lt_log.
*        DELETE t_tblcli.
*        CONTINUE.
*      ENDIF.
*****
*INI GAP016_BP
      PERFORM derivar_akont CHANGING t_tblcli.

      IF t_tblcli-bukrs IS NOT INITIAL
      AND t_tblcli-akont IS INITIAL.

        index4 = index.

        CLEAR lt_log.
        lt_log-msgtyp = 'E'.
        lt_log-msgid  = 'ZFI01'.
        lt_log-msgnr  = '015'.
        lt_log-msgv1  = t_tblcli-bukrs.
        lt_log-msgv2  = index4.
        APPEND lt_log.

        DELETE t_tblcli.
        CONTINUE.
*FIN GAP016_BP
        MOVE-CORRESPONDING t_tblcli TO zfit_sol_cliente.
        zfit_sol_cliente-estado = 'S'.
        SELECT SINGLE ddtext FROM dd07v INTO zfit_sol_cliente-estadot
                        WHERE domname = 'ZZDCLIEST'
                          AND ddlanguage = sy-langu
                          AND domvalue_l = zfit_sol_cliente-estado.
        INSERT zfit_sol_cliente.
      ENDIF.
    ENDIF.
*****
  ENDLOOP.

  CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
    TABLES
      i_message_tab = lt_log.

ENDFORM.                    " import_excel

*&---------------------------------------------------------------------*
*&      Form  verif
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verif.

  DATA: wa_t077d TYPE t077d.

  LOOP AT t_tblcli WHERE sel = 'X'.
    IF t_tblcli-bu_group IS INITIAL.
      lv_verif = sy-tabix.
      lv_verif_t = TEXT-024.
      lv_cursor_field = 'g_tblcli_wa-bu_group'.
      EXIT.
    ENDIF.

***INI ALML 27.05.2013
*    IF t_tblcli-ktokd <> '1020' AND"rcd
*       t_tblcli-ktokd <> '1030' AND"rsd
*       t_tblcli-ktokd <> '1050' AND"rcd aayala 16.01.2012
*       t_tblcli-ktokd <> '1010' AND"rcd aayala 16.01.2012
*       t_tblcli-ktokd <> '1060' AND   "Add-MGGM-19.04.2013
**       t_tblcli-ktokd <> '1040' AND"rsd
**       t_tblcli-ktokd <> '1910' AND
**       t_tblcli-ktokd <> 'ZINT' AND
*       t_tblcli-sel = 'X'.
*      lv_verif = sy-tabix.
*      lv_verif_t = text-010.
*      lv_cursor_field = 'g_tblcli_wa-ktokd'.
*      EXIT.
*    ENDIF.

*Verificar si el Gpo de cuentas existe
*INI GAP016_BP
    TRANSLATE t_tblcli-bu_group TO UPPER CASE.

    SELECT SINGLE bu_group
      FROM tb001
      INTO @DATA(lv_bu_group)
      WHERE bu_group = @t_tblcli-bu_group.

    IF sy-subrc <> 0.
      lv_verif = sy-tabix.
      lv_verif_t = TEXT-010.
      lv_cursor_field = 'G_TBLCLI_WA-BU_GROUP'.
      EXIT.
    ENDIF.

    IF t_tblcli-bu_group = 'ZINT'
      AND t_tblcli-vbund IS INITIAL.
      lv_verif = sy-tabix.
      lv_verif_t = TEXT-011.
      lv_cursor_field = 'G_TBLCLI_WA-VBUND'.
      EXIT.
    ENDIF.
*FIN GAP016_BP
*    CLEAR wa_t077d.
*    TRANSLATE t_tblcli-ktokd TO UPPER CASE.
*
*    IF NOT t_tblcli-ktokd IS INITIAL.
*
*      CALL FUNCTION 'T077D_SINGLE_READ'
*        EXPORTING
*          i_ktokd         = t_tblcli-ktokd
*        IMPORTING
*          o_t077d         = wa_t077d
*        EXCEPTIONS
*          not_found       = 1
*          parameter_error = 2
*          OTHERS          = 3.
*      IF sy-subrc <> 0.
*       lv_verif = sy-tabix.
*       lv_verif_t = text-010.
*       lv_cursor_field = 'g_tblcli_wa-ktokd'.
*       EXIT.
*      ENDIF.
*    ENDIF.
***FIN ALML 27.05.2013

*    IF t_tblcli-ktokd = '1040' AND
*      t_tblcli-vbund IS INITIAL OR
**       t_tblcli-ktokd = 'ZINT' AND
**       t_tblcli-vbund IS INITIAL.
*      lv_verif = sy-tabix.
*      lv_verif_t = text-011.
*      lv_cursor_field = 'G_TBLCLI_WA-VBUND'.
*    ENDIF.
    CALL FUNCTION 'TAX_NUMBER_CHECK'
      EXPORTING
        country             = t_tblcli-pais
        natural_person_flag = t_tblcli-stkzn
        tax_code_1          = t_tblcli-nif
      EXCEPTIONS
        not_valid           = 1
        different_fprcd     = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.

      lv_verif = sy-tabix.
      lv_verif_t = TEXT-012.
      lv_cursor_field = 'G_TBLCLI_WA-NIF'.
      EXIT.

    ENDIF.

*    IF t_tblcli-waers IS INITIAL.
*      lv_verif = sy-tabix.
*      lv_verif_t = text-013.
*      EXIT.
*    ENDIF.
    IF t_tblcli-fdgrv IS INITIAL.
      lv_verif = sy-tabix.
      lv_verif_t = TEXT-014.
      lv_cursor_field = 'G_TBLCLI_WA-FDGRV'.
      EXIT.
    ENDIF.
    IF t_tblcli-akont IS INITIAL.
      lv_verif = sy-tabix.
      lv_verif_t = text-015.
      lv_cursor_field = 'G_TBLCLI_WA-AKONT'.
      EXIT.
    ENDIF.
    IF t_tblcli-bukrs IS NOT INITIAL.
      IF t_tblcli-fdgrv IS INITIAL.
        lv_verif = sy-tabix.
        lv_verif_t = TEXT-014.
        lv_cursor_field = 'G_TBLCLI_WA-FDGRV'.
        EXIT.
      ENDIF.
*      PERFORM derivar_akont CHANGING t_tblcli.
*      IF t_tblcli-akont IS INITIAL.
*        lv_verif = sy-tabix.
*        lv_verif_t = TEXT-015.
*        lv_cursor_field = 'G_TBLCLI_WA-BUKRS'.
*        EXIT.
*      ENDIF.
      IF t_tblcli-zterm IS INITIAL.
        lv_verif = sy-tabix.
        lv_verif_t = TEXT-016.
        lv_cursor_field = 'G_TBLCLI_WA-ZTERM'.
        EXIT.
      ENDIF.
      IF t_tblcli-zwels IS INITIAL.
        lv_verif = sy-tabix.
        lv_verif_t = TEXT-017.
        lv_cursor_field = 'G_TBLCLI_WA-ZWELS'.
        EXIT.
      ENDIF.
      IF t_tblcli-pais IS INITIAL.
        lv_verif = sy-tabix.
        lv_verif_t = TEXT-018.
        lv_cursor_field = 'G_TBLCLI_WA-PAIS'.
        EXIT.
      ENDIF.
      IF t_tblcli-name1 IS INITIAL.
        lv_verif = sy-tabix.
        lv_verif_t = TEXT-019.
        lv_cursor_field = 'G_TBLCLI_WA-NAME1'.
        EXIT.
      ENDIF.
      IF t_tblcli-sort1 IS INITIAL.
        lv_verif = sy-tabix.
        lv_verif_t = TEXT-020.
        lv_cursor_field = 'G_TBLCLI_WA-SORT1'.
        EXIT.
      ENDIF.
      IF t_tblcli-poblac1 IS INITIAL.
        lv_verif = sy-tabix.
        lv_verif_t = TEXT-021.
        lv_cursor_field = 'G_TBLCLI_WA-POBLAC1'.
        EXIT.
      ENDIF.
      IF t_tblcli-cod_post IS INITIAL.
        lv_verif = sy-tabix.
        lv_verif_t = TEXT-022.
        lv_cursor_field = 'G_TBLCLI_WA-COD_POST'.
        EXIT.
      ENDIF.
      IF t_tblcli-direc1 IS INITIAL.
        lv_verif = sy-tabix.
        lv_verif_t = TEXT-023.
        lv_cursor_field = 'G_TBLCLI_WA-DIREC1'.
        EXIT.
      ENDIF.
    ELSE.
      CLEAR:
        t_tblcli-akont,
        t_tblcli-fdgrv,
        t_tblcli-zterm,
        t_tblcli-zwels.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " verif

*&---------------------------------------------------------------------*
*&      Form  crear_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM crear_message.
  MESSAGE i021(zfi01) WITH lv_verif lv_verif_t.
ENDFORM.                    " crear_message

*&---------------------------------------------------------------------*
*&      Form  rechazarm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM rechazarm.

  LOOP AT t_tblcli WHERE sel = 'X'.

    MOVE-CORRESPONDING t_tblcli TO zfit_sol_cliente.
    zfit_sol_cliente-estado = 'R'.
    SELECT SINGLE ddtext FROM dd07v INTO zfit_sol_cliente-estadot
                      WHERE domname = 'ZZDCLIEST'
                        AND ddlanguage = sy-langu
                        AND domvalue_l = zfit_sol_cliente-estado.
    MODIFY zfit_sol_cliente.
    IF sy-subrc = 0.
      MESSAGE s022(zfi01).
      t_tblcli-estado = 'R'.
      DELETE t_tblcli.
    ENDIF.

  ENDLOOP.
  IF sy-subrc <> 0.
    MESSAGE w031(zfi01).
  ENDIF.

ENDFORM.                    " rechazarm

*&---------------------------------------------------------------------*
*&      Form  f_number_get_next_debitor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*FORM f_number_get_next_debitor CHANGING y_subrc.
*
*  DATA: lv_cliente_tmp(20) TYPE c.
*
*  CLEAR y_subrc.
*
** Inicio modif Genis 19.07.2007
**  IF t_tblcli-kunnr <> space.
**    SELECT SINGLE cli_sap INTO t_tblcli-kunnrsap
**                               FROM zficonv_cli_pms
**                              WHERE cli_pms = t_tblcli-kunnr
**                                AND hotel = t_tblcli-bukrs.
**    IF sy-subrc <> 0.
** Fin modif Genis 19.07.2007
**  if t_tblcli-ktokd = 'ZTER'.
*  SELECT SINGLE * FROM nriv WHERE object = 'DEBITOR'
*                              AND nrrangenr = 'CU'.
**  ELSEIF t_tblcli-ktokd = 'ZINT'.
**  SELECT SINGLE * FROM nriv WHERE object = 'DEBITOR'
**                              AND nrrangenr = 'ZI'.
**  ENDIF.
*  IF nriv-nrlevel IS INITIAL.
*    nriv-nrlevel = nriv-fromnumber.
**        t_tblcli-kunnrsap = nriv-fromnumber.
*    lv_cliente_tmp = nriv-fromnumber.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = lv_cliente_tmp
*      IMPORTING
*        output = t_tblcli-kunnrsap.
*
*  ELSE.
**        t_tblcli-kunnrsap = nriv-nrlevel + 1.
*    lv_cliente_tmp = nriv-nrlevel + 1.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = lv_cliente_tmp
*      IMPORTING
*        output = t_tblcli-kunnrsap.
*    DO.
*      SELECT SINGLE * FROM kna1 WHERE kunnr = t_tblcli-kunnrsap.
*      IF sy-subrc = 0.
**            t_tblcli-kunnrsap = t_tblcli-kunnrsap + 1.
*        lv_cliente_tmp = lv_cliente_tmp + 1.
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = lv_cliente_tmp
*          IMPORTING
*            output = t_tblcli-kunnrsap.
*
*      ELSE.
*        EXIT.
*      ENDIF.
*    ENDDO.
*    nriv-nrlevel = t_tblcli-kunnrsap.
*  ENDIF.
*  MODIFY nriv. "<---- MODIFICAR TABLA STD RANGO NÚMEROS CLIENTES
** Inicio modif Genis 19.07.2007
**    ELSE.
**      lt_log-msgtyp = 'E'.
**      lt_log-msgid = 'ZFI01'.
**      lt_log-msgnr = '112'.
**      lt_log-msgv1 = t_tblcli-kunnr.
**      lt_log-msgv2 = t_tblcli-bukrs.
**      APPEND lt_log.
**      y_subrc = 4.
**    ENDIF.
**  ENDIF.
** Fin modif Genis 19.07.2007
*
*ENDFORM.                    " f_number_get_next_debitor

*&---------------------------------------------------------------------*
*&      Form  f_bdc_create_unknowm_debitor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*FORM f_bdc_create_unknowm_debitor.
*
*  DATA: lv_vkorg2 TYPE vkorg,
*        lv_vtweg2 TYPE vtweg,
*        lv_spart2 TYPE spart,
*        lv_adrnr TYPE adrnr,
*        lv_name TYPE char40,
*        lv_errorbi(1) TYPE c.
*
*  DATA: lv_vkorg TYPE vkorg,
*        lv_vtweg TYPE vtweg,
*        lv_spart TYPE spart.
*
*  CLEAR  : bdcdata, lv_adrnr.
*  REFRESH: bdcdata.
*
** PANTALLA INICIAL
*  PERFORM f_bdc_screen_0100_header USING '0100'.
*
** ...si es necesario --> se crearán datos de Area de Ventas
**  PERFORM f_bdc_screen_0100_ventas CHANGING lv_vkorg
**                                            lv_vtweg
**                                            lv_spart.
*
**
*  IF t_tblcli-kunnrsap IS INITIAL.
** -- CLIENTE DESCONOCIDO... HAY QUE CREARLO DE CERO ---------------------
*
**   DATOS DE DIRECCIÓN
*    PERFORM f_bdc_screen_0111_address.
*
**   DATOS DE CONTROL
**    PERFORM f_bdc_screen_0120_control.
*
**   MARKETING
**    PERFORM f_bdc_screen_0125.
*
**   PAGOS
*    PERFORM f_bdc_screen_0130_paymnt.
*
**   PERSONA DE CONTACTO
*    PERFORM f_bdc_screen_0360_contact.
*  ELSE.
** -- CLIENTE INFORMADO: HAY QUE AMPLIAR SOCIEDAD -------------------------
*    SELECT SINGLE * FROM zficonv_cli_pms WHERE cli_sap = t_tblcli-kunnrsap.
*    IF sy-subrc <> 0.
**     DATOS DE DIRECCIÓN
*      PERFORM f_bdc_screen_0111_address.
*
**     DATOS DE CONTROL
**      PERFORM f_bdc_screen_0120_control.
*
**     MARKETING
**      PERFORM f_bdc_screen_0125.
*
**     PAGOS
*      PERFORM f_bdc_screen_0130_paymnt.
*
**     PERSONA DE CONTACTO
*      PERFORM f_bdc_screen_0360_contact.
*    ENDIF.
*  ENDIF.
*
** GESTION DE CUENTA
*  PERFORM bdc_dynpro       USING 'SAPMF02D'  '0210'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNB1-AKONT'.
*  PERFORM bdc_field       USING 'KNB1-AKONT'  t_tblcli-akont.
**Ini JGOR 02.11.2017
**  IF t_tblcli-ktokd <> '1020' AND t_tblcli-ktokd <> '1910' and
**t_tblcli-ktokd <> '1010'."AAR 16.01.2012
**    PERFORM bdc_field       USING 'KNB1-VZSKZ'  t_tblcli-vzskz.
**  ENDIF.
**Fin JGOR 02.11.2017
*  PERFORM bdc_field       USING 'KNB1-FDGRV'  t_tblcli-fdgrv.
*  PERFORM bdc_field       USING 'KNB1-ALTKN'  t_tblcli-altkn.
**  IF t_tblcli-ktokd = '1040'.
**    PERFORM bdc_field       USING 'KNB1-VZSKZ'  'Z1'.
**  ENDIF.
*  PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
*
** PAGOS
*  PERFORM bdc_dynpro       USING 'SAPMF02D'  '0215'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNB1-ZTERM'.
*  PERFORM bdc_field       USING 'KNB1-ZTERM'  t_tblcli-zterm.
*  PERFORM bdc_field       USING 'KNB1-ZAHLS'  t_tblcli-zahls.
*  PERFORM bdc_field       USING 'KNB1-ZWELS'  t_tblcli-zwels.
*  PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
*
** CORRESPONDENCIA
*  PERFORM bdc_dynpro       USING 'SAPMF02D'  '0220'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNB5-MAHNA'.
*  PERFORM bdc_field       USING 'KNB5-MAHNA'  t_tblcli-mahna.
*  PERFORM bdc_field       USING 'KNB1-BUSAB'  t_tblcli-busab.
*  PERFORM bdc_field       USING 'KNB5-MANSP'  t_tblcli-mansp.
*  PERFORM bdc_field       USING 'KNB5-KNRMA'  t_tblcli-knrma.
*  PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
*
** SEGUROS
**  PERFORM bdc_dynpro       USING 'SAPMF02D'  '0230'.
**  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNB1-VRSNR'.
**  PERFORM bdc_field       USING 'KNB1-VRSNR'  t_tblcli-vrsnr.
**  PERFORM bdc_field       USING 'BDC_OKCODE'  '=00'.
*
**  IF t_tblcli-ktokd <> '1040'.
*** RETENCION DE IMPUESTOS
**    PERFORM bdc_dynpro       USING 'SAPMF02D'  '0610'.
**    PERFORM bdc_field       USING 'BDC_CURSOR'  'KNBW-WITHT(01)'.
**    PERFORM bdc_field       USING 'KNBW-WITHT(01)'  t_tblcli-witht.
**    PERFORM bdc_field       USING 'KNBW-WT_WITHCD(01)'  t_tblcli-wt_withcd.
**    PERFORM bdc_field       USING 'KNBW-WT_AGENT(01)'   t_tblcli-wt_agent.
**    IF NOT t_tblcli-wt_agtdf IS INITIAL.
**      CLEAR: ano, mes, dia.
**      ano = t_tblcli-wt_agtdf(4).
**      mes = t_tblcli-wt_agtdf+2(2).
**      dia = t_tblcli-wt_agtdf+4(2).
**      CLEAR t_tblcli-wt_agtdf.
**      CONCATENATE dia mes ano INTO t_tblcli-wt_agtdf.
**      PERFORM bdc_field       USING 'KNBW-WT_AGTDF(01)'  t_tblcli-wt_agtdf.
**      IF NOT t_tblcli-wt_agtdf IS INITIAL.
**        CLEAR: ano, mes, dia.
**        ano = t_tblcli-wt_agtdf+4(4).
**        mes = t_tblcli-wt_agtdf+2(2).
**        dia = t_tblcli-wt_agtdf(2).
**        CLEAR t_tblcli-wt_agtdf.
**        CONCATENATE ano mes dia INTO t_tblcli-wt_agtdf.
**      ENDIF.
**    ENDIF.
**    IF NOT t_tblcli-wt_agtdt IS INITIAL.
**      CLEAR: ano, mes, dia.
**      ano = t_tblcli-wt_agtdt(4).
**      mes = t_tblcli-wt_agtdt+2(2).
**      dia = t_tblcli-wt_agtdt+4(2).
**      CLEAR t_tblcli-wt_agtdt.
**      CONCATENATE dia mes ano INTO t_tblcli-wt_agtdt.
**      PERFORM bdc_field       USING 'KNBW-WT_AGTDT(01)'  t_tblcli-wt_agtdt.
**      IF NOT t_tblcli-wt_agtdt IS INITIAL.
**        CLEAR: ano, mes, dia.
**        ano = t_tblcli-wt_agtdt+4(4).
**        mes = t_tblcli-wt_agtdt+2(2).
**        dia = t_tblcli-wt_agtdt(2).
**        CLEAR t_tblcli-wt_agtdt.
**        CONCATENATE ano mes dia INTO t_tblcli-wt_agtdt.
**      ENDIF.
**    ENDIF.
**    PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
**
**    PERFORM bdc_dynpro       USING 'SAPMF02D'  '0610'.
**    PERFORM bdc_field       USING 'BDC_CURSOR'  'KNBW-WITHT(01)'.
**    PERFORM bdc_field       USING 'BDC_OKCODE'  '=AO02'."'=AO01'.JGOR 02.11.2017
**  ENDIF.
*
*  IF NOT lv_vkorg IS INITIAL AND
*     NOT lv_vtweg IS INITIAL AND
*     NOT lv_spart IS INITIAL.
*
**   DATOS AREA DE VENTAS
*    PERFORM bdc_dynpro       USING 'SAPMF02D'  '0310'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'  'KNVV-KALKS'.
*    PERFORM bdc_field       USING 'KNVV-BZIRK'  t_tblcli-bzirk.
*    PERFORM bdc_field       USING 'KNVV-VKBUR'  t_tblcli-vkbur.
*    PERFORM bdc_field       USING 'KNVV-VKGRP'  t_tblcli-vkgrp.
*    PERFORM bdc_field       USING 'KNVV-KDGRP'  t_tblcli-kdgrp.
*    PERFORM bdc_field       USING 'KNVV-WAERS'  t_tblcli-waersd.
**    IF t_tblcli-ktokd <> '1040'. " AND t_tblcli-ktokd <> 'ZINT'.
**      PERFORM bdc_field       USING 'KNVV-KONDA'  t_tblcli-konda.
**    ENDIF.
*    PERFORM bdc_field       USING 'KNVV-PVKSM'  t_tblcli-pvksm.
*    PERFORM bdc_field       USING 'KNVV-VSORT'  t_tblcli-vsort.
*    PERFORM bdc_field       USING 'KNVV-KALKS'  '2'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
*
*    PERFORM bdc_dynpro       USING 'SAPMF02D'  '0320'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'  'KNVV-KTGRD'.
*    PERFORM bdc_field       USING 'KNVV-KTGRD'  '01'.
*    PERFORM bdc_field       USING 'KNVV-ZTERM'  t_tblcli-zterm.
*    PERFORM bdc_field       USING 'KNVV-INCO1'  t_tblcli-inco1.
*    PERFORM bdc_field       USING 'KNVV-MRNKZ'  t_tblcli-mrnkz.
*    PERFORM bdc_field       USING 'KNVV-PERFK'  t_tblcli-perfk.
*    PERFORM bdc_field       USING 'KNVV-PERRL'  t_tblcli-perrl.
*    PERFORM bdc_field       USING 'KNVV-BOKRE'  t_tblcli-bokre.
*    PERFORM bdc_field       USING 'KNVV-PRFRE'  t_tblcli-prfre.
*
*    PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
*
*    PERFORM bdc_dynpro       USING 'SAPMF02D'  '1350'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'  'KNVI-TAXKD(01)'.
*    PERFORM bdc_field       USING 'KNVI-TAXKD(01)'  '1'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'  '=ENTR'.
*
*    PERFORM bdc_dynpro       USING 'SAPMF02D'  '1350'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'  'KNVI-TAXKD(01)'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'  '=ENTR'.
*
*    PERFORM bdc_dynpro       USING 'SAPMF02D'  '0324'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'  'RF02D-KUNNR'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
*  ENDIF.
*
** DATOS ADICIONALES
*  PERFORM bdc_dynpro       USING 'SAPMF02D'  '4000'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNB1-WAERS'.
*  IF t_tblcli-waers IS INITIAL.
*    SELECT SINGLE waers FROM t001 INTO t_tblcli-waers WHERE bukrs = t_tblcli-bukrs.
*  ENDIF.
*  PERFORM bdc_field       USING 'KNB1-WAERS'  t_tblcli-waers.
*  PERFORM bdc_field       USING 'KNB1-SIRENHA'  t_tblcli-sirenha.
*  PERFORM bdc_field       USING 'KNB1-MODOCOM'  t_tblcli-modocom.
*  PERFORM bdc_field       USING 'KNB1-MAIL2'  t_tblcli-mail2.
*  IF NOT t_tblcli-direc2 IS INITIAL.
*    PERFORM bdc_field       USING 'KNB1-DIREC2'  t_tblcli-direc2.
*  ELSE.
*    PERFORM bdc_field       USING 'KNB1-DIREC2'  t_tblcli-direc1.
*  ENDIF.
*  IF NOT t_tblcli-poblac2 IS INITIAL.
*    PERFORM bdc_field       USING 'KNB1-POBLAC2'  t_tblcli-poblac2.
*  ELSE.
*    PERFORM bdc_field       USING 'KNB1-POBLAC2'  t_tblcli-poblac1.
*  ENDIF.
*  IF NOT t_tblcli-cod_post2 IS INITIAL.
*    PERFORM bdc_field       USING 'KNB1-COD_POST2'  t_tblcli-cod_post2.
*  ELSE.
*    PERFORM bdc_field       USING 'KNB1-COD_POST2'  t_tblcli-cod_post.
*  ENDIF.
*  IF NOT t_tblcli-pais2 IS INITIAL.
*    PERFORM bdc_field       USING 'KNB1-PAIS2'  t_tblcli-pais2.
*  ELSE.
*    PERFORM bdc_field       USING 'KNB1-PAIS2'  t_tblcli-pais.
*  ENDIF.
*
*  PERFORM bdc_field       USING 'KNB1-ZZNCFTC'  t_tblcli-zzncftc.
*
*  PERFORM bdc_field       USING 'BDC_OKCODE'  '=UPDA'.
*
*
** LLAMADA A LA CREACIÓN DE CLIENTE
*  CALL TRANSACTION 'XD01' USING bdcdata
*    MODE p_ctmode
*    UPDATE 'S'
*    MESSAGES INTO messtab.
*
*  CLEAR: lv_errorbi.
**Ini JGOR 02.11.2017
**  IF sy-subrc <> 0.
**    lv_errorbi = 'X'.
**  ENDIF.
**Fin JGOR 02.11.2017
*  DELETE messtab WHERE msgid = 'ZFI01'.
*  DELETE messtab WHERE msgid = 'F2' AND msgnr = '036'.
*
*  LOOP AT messtab.
*    AT FIRST.
*      LOOP AT messtab WHERE msgtyp = 'E'.
*      ENDLOOP.
*      IF sy-subrc = 0 OR lv_errorbi = 'X'.
*        lt_log-msgtyp = 'E'.
*        lt_log-msgid = 'ZFI01'.
*        lt_log-msgnr = '080'.
*        lt_log-msgv1 = t_tblcli-kunnr.
*        APPEND lt_log.
*      ELSE.
*        lt_log-msgtyp = 'S'.
*        lt_log-msgid = 'ZFI01'.
*        lt_log-msgnr = '080'.
*        lt_log-msgv1 = t_tblcli-kunnr.
*        APPEND lt_log.
*      ENDIF.
*    ENDAT.
*    MOVE-CORRESPONDING messtab TO lt_log.
*    APPEND lt_log.
*  ENDLOOP.
**aayala recuperamos el numero de cliente nuevo 29.12.2011
** Tratamos de obtener el número de CLIENTE desde el STD
*  DATA lv_kunnr TYPE kunnr.
*  CLEAR lv_kunnr.
*** 1. A través de la tabla de mensajes
*  LOOP AT messtab
*    WHERE msgid = 'F2'
*    OR    msgnr = '171'    "Sociedad
*    OR    msgnr = '172'    "Area de Ventas
*    AND   msgnr = '174'.    "Sociedad/Area de Ventas
**      MESSAGE S171(F2).
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = messtab-msgv1(10) "ALML 27.10.2014(solo tomar 10 caract)
*      IMPORTING
*        output = lv_kunnr.
*
*  ENDLOOP.
*
*  READ TABLE lt_log WITH KEY msgtyp = 'E'.
*  IF sy-subrc <> 0.
**   Damos de alta el registro en la tabla de mapeos pms-sap
*    zficonv_cli_pms-hotel = t_tblcli-bukrs.
*    zficonv_cli_pms-cli_pms = t_tblcli-kunnr.
**aayala recuperamos el numero de cliente nuevo 29.12.2011
*    IF t_tblcli-kunnrsap IS INITIAL.
*      t_tblcli-kunnrsap =  lv_kunnr.
*    ENDIF.
**aayala recuperamos el numero de cliente nuevo 29.12.2011
*    zficonv_cli_pms-cli_sap = t_tblcli-kunnrsap.
*
** Inicio modif Genis 19.07.2007
*    GET PARAMETER ID 'KUN' FIELD zficonv_cli_pms-cli_sap.
** Fin modif Genis 19.07.2007
*
*    IF NOT zficonv_cli_pms-cli_pms IS INITIAL.
*      IF p_new IS INITIAL.
*        INSERT zficonv_cli_pms.
*      ENDIF.
*    ENDIF.
*    t_tblcli-estado = 'V'.
*    SELECT SINGLE ddtext FROM dd07v INTO t_tblcli-estadot
*                  WHERE domname = 'ZZDCLIEST'
*                    AND ddlanguage = sy-langu
*                    AND domvalue_l = t_tblcli-estado.
*    t_tblcli-sel = ''.
*    MODIFY t_tblcli.
*    MOVE-CORRESPONDING t_tblcli TO zfit_sol_cliente.
*    IF p_new IS INITIAL.
*      MODIFY zfit_sol_cliente.
*    ENDIF.
****      Actualizamos campos no actualizables por batch input
*    SELECT SINGLE adrnr INTO lv_adrnr FROM kna1 WHERE kunnr = t_tblcli-kunnrsap.
*
*    SELECT SINGLE * FROM adr6 WHERE addrnumber = lv_adrnr.
*    IF sy-subrc = 0.
*      adr6-smtp_addr = t_tblcli-mail.
*      MODIFY adr6.
*    ENDIF.
*    SELECT SINGLE * FROM adrc WHERE addrnumber = lv_adrnr.
*    IF sy-subrc = 0.
*      adrc-str_suppl1 = t_tblcli-direc2.
*      adrc-house_num1 = t_tblcli-house_num1.
*      adrc-city2 = t_tblcli-poblac2.
*      MODIFY adrc.
*    ENDIF.
*
*  ENDIF.
*  CLEAR messtab.
*  REFRESH messtab.
*
*ENDFORM.                    " f_bdc_create_unknowm_debitor

*&---------------------------------------------------------------------*
*&      Form  f_bdc_create_known_debitor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*FORM f_bdc_create_known_debitor.
*
*  DATA: lv_vkorg2 TYPE vkorg,
*        lv_vtweg2 TYPE vtweg,
*        lv_spart2 TYPE spart,
*        lv_adrnr TYPE adrnr,
*        lv_name TYPE char40.
*
*  DATA: lv_vkorg TYPE vkorg,
*        lv_vtweg TYPE vtweg,
*        lv_spart TYPE spart.
*
*
*  CLEAR  : bdcdata, lv_vkorg, lv_vtweg, lv_spart, lv_vkorg2,
*           lv_vtweg2, lv_spart2, lv_adrnr, lv_name.
*  REFRESH: bdcdata.
*
** PANTALLA INICIAL
*  PERFORM f_bdc_screen_0100_header USING '0100'.
*
** ...si es necesario, se crearán datos de Area de Ventas
*  PERFORM f_bdc_screen_0100_ventas CHANGING lv_vkorg
*                                            lv_vtweg
*                                            lv_spart.
*
*
*  SELECT SINGLE * FROM knb1 WHERE kunnr = t_tblcli-kunnrsap.
*  IF sy-subrc <> 0.
** Si no está creado el cliente para ninguna Sociedad:
** DATOS GENERALES
*
**   DATOS DE DIRECCIÓN
*    PERFORM f_bdc_screen_0111_address.
*
**   DATOS DE CONTROL
**    PERFORM f_bdc_screen_0120_control.
*
**   PAGOS
*    PERFORM f_bdc_screen_0130_paymnt.
*
**   PERSONA DE CONTACTO
*    PERFORM f_bdc_screen_0360_contact.
*
*  ENDIF.
*
** Inicio modif. Genis 19.07.2007
*  PERFORM f_bdc_datos_sociedad.
** Fin modif. Genis 19.07.2007
*
*** DATOS SOCIEDAD
**  PERFORM bdc_dynpro       USING 'SAPMF02D'  '0210'.
**  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNB1-AKONT'.
**  PERFORM bdc_field       USING 'KNB1-AKONT'  t_tblcli-akont.
**  PERFORM bdc_field       USING 'KNB1-FDGRV'  t_tblcli-fdgrv.
**  PERFORM bdc_field       USING 'KNB1-ALTKN'  t_tblcli-altkn.
**  IF t_tblcli-ktokd = 'ZINT'.
**    PERFORM bdc_field       USING 'KNB1-VZSKZ'  'Z1'.
**  ENDIF.
**  PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
**
**  PERFORM bdc_dynpro       USING 'SAPMF02D'  '0215'.
**  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNB1-ZTERM'.
**  PERFORM bdc_field       USING 'KNB1-ZTERM'  t_tblcli-zterm.
**  PERFORM bdc_field       USING 'KNB1-ZAHLS'  t_tblcli-zahls.
**  PERFORM bdc_field       USING 'KNB1-ZWELS'  t_tblcli-zwels.
**  PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
**
**  PERFORM bdc_dynpro       USING 'SAPMF02D'  '0220'.
**  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNB5-MAHNA'.
**  PERFORM bdc_field       USING 'KNB5-MAHNA'  t_tblcli-mahna.
**  PERFORM bdc_field       USING 'KNB1-BUSAB'  t_tblcli-busab.
**  PERFORM bdc_field       USING 'KNB5-MANSP'  t_tblcli-mansp.
**  PERFORM bdc_field       USING 'KNB5-KNRMA'  t_tblcli-knrma.
**  PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
**
**  PERFORM bdc_dynpro       USING 'SAPMF02D'  '0230'.
**  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNB1-VRSNR'.
**  PERFORM bdc_field       USING 'KNB1-VRSNR'  t_tblcli-vrsnr.
**  PERFORM bdc_field       USING 'BDC_OKCODE'  '=00'.
**  IF t_tblcli-ktokd <> 'ZINT'.
**    PERFORM bdc_dynpro       USING 'SAPMF02D'  '0610'.
**    PERFORM bdc_field       USING 'BDC_CURSOR'  'KNBW-WITHT(01)'.
**    PERFORM bdc_field       USING 'KNBW-WITHT(01)'  t_tblcli-witht.
**    PERFORM bdc_field       USING 'KNBW-WT_WITHCD(01)'  t_tblcli-wt_withcd.
**    PERFORM bdc_field       USING 'KNBW-WT_AGENT(01)'   t_tblcli-wt_agent.
**    IF NOT t_tblcli-wt_agtdf IS INITIAL.
**      CLEAR: ano, mes, dia.
**      ano = t_tblcli-wt_agtdf(4).
**      mes = t_tblcli-wt_agtdf+2(2).
**      dia = t_tblcli-wt_agtdf+4(2).
**      CLEAR t_tblcli-wt_agtdf.
**      CONCATENATE dia mes ano INTO t_tblcli-wt_agtdf.
**      PERFORM bdc_field       USING 'KNBW-WT_AGTDF(01)'  t_tblcli-wt_agtdf.
**      IF NOT t_tblcli-wt_agtdf IS INITIAL.
**        CLEAR: ano, mes, dia.
**        ano = t_tblcli-wt_agtdf+4(4).
**        mes = t_tblcli-wt_agtdf+2(2).
**        dia = t_tblcli-wt_agtdf(2).
**        CLEAR t_tblcli-wt_agtdf.
**        CONCATENATE ano mes dia INTO t_tblcli-wt_agtdf.
**      ENDIF.
**    ENDIF.
**    IF NOT t_tblcli-wt_agtdt IS INITIAL.
**      CLEAR: ano, mes, dia.
**      ano = t_tblcli-wt_agtdt(4).
**      mes = t_tblcli-wt_agtdt+2(2).
**      dia = t_tblcli-wt_agtdt+4(2).
**      CLEAR t_tblcli-wt_agtdt.
**      CONCATENATE dia mes ano INTO t_tblcli-wt_agtdt.
**      PERFORM bdc_field       USING 'KNBW-WT_AGTDT(01)'  t_tblcli-wt_agtdt.
**      IF NOT t_tblcli-wt_agtdt IS INITIAL.
**        CLEAR: ano, mes, dia.
**        ano = t_tblcli-wt_agtdt+4(4).
**        mes = t_tblcli-wt_agtdt+2(2).
**        dia = t_tblcli-wt_agtdt(2).
**        CLEAR t_tblcli-wt_agtdt.
**        CONCATENATE ano mes dia INTO t_tblcli-wt_agtdt.
**      ENDIF.
**    ENDIF.
**    PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
**
**
**    PERFORM bdc_dynpro       USING 'SAPMF02D'  '0610'.
**    PERFORM bdc_field       USING 'BDC_CURSOR'  'KNBW-WITHT(01)'.
**    PERFORM bdc_field       USING 'BDC_OKCODE'  '=ENTR'.
**  ENDIF.
*  IF NOT lv_vkorg IS INITIAL AND
*     NOT lv_vtweg IS INITIAL AND
*     NOT lv_spart IS INITIAL.
*    PERFORM bdc_dynpro       USING 'SAPMF02D'  '0310'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'  'KNVV-KALKS'.
*    PERFORM bdc_field       USING 'KNVV-BZIRK'  t_tblcli-bzirk.
*    PERFORM bdc_field       USING 'KNVV-VKBUR'  t_tblcli-vkbur.
*    PERFORM bdc_field       USING 'KNVV-VKGRP'  t_tblcli-vkgrp.
*    PERFORM bdc_field       USING 'KNVV-KDGRP'  t_tblcli-kdgrp.
*    PERFORM bdc_field       USING 'KNVV-WAERS'  t_tblcli-waersd.
**    IF t_tblcli-ktokd <> '1040'. " AND t_tblcli-ktokd <> 'ZINT'.
**      PERFORM bdc_field       USING 'KNVV-KONDA'  t_tblcli-konda.
**    ENDIF.
*    PERFORM bdc_field       USING 'KNVV-PVKSM'  t_tblcli-pvksm.
*    PERFORM bdc_field       USING 'KNVV-VSORT'  t_tblcli-vsort.
*    PERFORM bdc_field       USING 'KNVV-KALKS'  '2'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
*
*    PERFORM bdc_dynpro       USING 'SAPMF02D'  '0320'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'  'KNVV-KTGRD'.
*    PERFORM bdc_field       USING 'KNVV-KTGRD'  '01'.
*    PERFORM bdc_field       USING 'KNVV-ZTERM'  t_tblcli-zterm.
*    PERFORM bdc_field       USING 'KNVV-INCO1'  t_tblcli-inco1.
*    PERFORM bdc_field       USING 'KNVV-MRNKZ'  t_tblcli-mrnkz.
*    PERFORM bdc_field       USING 'KNVV-PERFK'  t_tblcli-perfk.
*    PERFORM bdc_field       USING 'KNVV-PERRL'  t_tblcli-perrl.
*    PERFORM bdc_field       USING 'KNVV-BOKRE'  t_tblcli-bokre.
*    PERFORM bdc_field       USING 'KNVV-PRFRE'  t_tblcli-prfre.
*
*    PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
*
*    PERFORM bdc_dynpro       USING 'SAPMF02D'  '1350'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'  'KNVI-TAXKD(01)'.
*    PERFORM bdc_field       USING 'KNVI-TAXKD(01)'  '1'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'  '=ENTR'.
*
*    PERFORM bdc_dynpro       USING 'SAPMF02D'  '1350'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'  'KNVI-TAXKD(01)'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'  '=ENTR'.
*
*    PERFORM bdc_dynpro       USING 'SAPMF02D'  '0324'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'  'RF02D-KUNNR'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
*  ENDIF.
*
*  PERFORM bdc_dynpro       USING 'SAPMF02D'  '4000'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNB1-WAERS'.
*  IF t_tblcli-waers IS INITIAL.
*    SELECT SINGLE waers FROM t001 INTO t_tblcli-waers WHERE bukrs = t_tblcli-bukrs.
*  ENDIF.
*  PERFORM bdc_field       USING 'KNB1-WAERS'  t_tblcli-waers.
*  PERFORM bdc_field       USING 'KNB1-SIRENHA'  t_tblcli-sirenha.
*  PERFORM bdc_field       USING 'KNB1-MODOCOM'  t_tblcli-modocom.
*  PERFORM bdc_field       USING 'KNB1-MAIL2'  t_tblcli-mail2.
*
*  IF NOT t_tblcli-direc2 IS INITIAL.
*    PERFORM bdc_field       USING 'KNB1-DIREC2'  t_tblcli-direc2.
*  ELSE.
*    PERFORM bdc_field       USING 'KNB1-DIREC2'  t_tblcli-direc1.
*  ENDIF.
*  IF NOT t_tblcli-poblac2 IS INITIAL.
*    PERFORM bdc_field       USING 'KNB1-POBLAC2'  t_tblcli-poblac2.
*  ELSE.
*    PERFORM bdc_field       USING 'KNB1-POBLAC2'  t_tblcli-poblac1.
*  ENDIF.
*  IF NOT t_tblcli-cod_post2 IS INITIAL.
*    PERFORM bdc_field       USING 'KNB1-COD_POST2'  t_tblcli-cod_post2.
*  ELSE.
*    PERFORM bdc_field       USING 'KNB1-COD_POST2'  t_tblcli-cod_post.
*  ENDIF.
*  IF NOT t_tblcli-pais2 IS INITIAL.
*    PERFORM bdc_field       USING 'KNB1-PAIS2'  t_tblcli-pais2.
*  ELSE.
*    PERFORM bdc_field       USING 'KNB1-PAIS2'  t_tblcli-pais.
*  ENDIF.
*  PERFORM bdc_field       USING 'KNB1-ZZNCFTC'  t_tblcli-zzncftc.
*
*  PERFORM bdc_field       USING 'BDC_OKCODE'  '=UPDA'.
*
*  CALL TRANSACTION 'XD01' USING bdcdata
*    MODE p_ctmode
*    UPDATE 'S'
*    MESSAGES INTO messtab.
*
*  DELETE messtab WHERE msgid = 'ZFI01'.
*  DELETE messtab WHERE msgid = 'F2' AND msgnr = '036'.
*
*  LOOP AT messtab.
*    AT FIRST.
*      LOOP AT messtab WHERE msgtyp = 'E'.
*      ENDLOOP.
*      IF sy-subrc = 0.
*        lt_log-msgtyp = 'E'.
*        lt_log-msgid = 'ZFI01'.
*        lt_log-msgnr = '080'.
*        lt_log-msgv1 = t_tblcli-kunnr.
*        APPEND lt_log.
*      ELSE.
*        lt_log-msgtyp = 'S'.
*        lt_log-msgid = 'ZFI01'.
*        lt_log-msgnr = '080'.
*        lt_log-msgv1 = t_tblcli-kunnr.
*        APPEND lt_log.
*      ENDIF.
*    ENDAT.
*    MOVE-CORRESPONDING messtab TO lt_log.
*    APPEND lt_log.
*  ENDLOOP.
*
**    Damos de alta el registro en la tabla de mapeos pms-sap
*  LOOP AT messtab WHERE msgtyp = 'E'.
*  ENDLOOP.
*  IF sy-subrc <> 0.
*    zficonv_cli_pms-hotel = t_tblcli-bukrs.
*    zficonv_cli_pms-cli_pms = t_tblcli-kunnr.
*    zficonv_cli_pms-cli_sap = t_tblcli-kunnrsap.
*    IF NOT zficonv_cli_pms-cli_pms IS INITIAL.
*      IF p_new IS INITIAL.
*        INSERT zficonv_cli_pms.
*      ENDIF.
*    ENDIF.
*    t_tblcli-estado = 'V'.
*    SELECT SINGLE ddtext FROM dd07v INTO t_tblcli-estadot
*                WHERE domname = 'ZZDCLIEST'
*                  AND ddlanguage = sy-langu
*                  AND domvalue_l = t_tblcli-estado.
*    t_tblcli-sel = ''.
*    MODIFY t_tblcli.
*    MOVE-CORRESPONDING t_tblcli TO zfit_sol_cliente.
*    IF p_new IS INITIAL.
*      MODIFY zfit_sol_cliente.
*    ENDIF.
****      Actualizamos campos no actualizables por batch input
*    SELECT SINGLE adrnr INTO lv_adrnr FROM kna1 WHERE kunnr = t_tblcli-kunnrsap.
*    SELECT SINGLE * FROM adr6 WHERE addrnumber = lv_adrnr.
*    IF sy-subrc = 0.
*      adr6-smtp_addr = t_tblcli-mail.
*      MODIFY adr6.
*    ENDIF.
*
*    SELECT SINGLE * FROM adrc WHERE addrnumber = lv_adrnr.
*    IF sy-subrc = 0.
*      adrc-str_suppl1 = t_tblcli-direc2.
*      adrc-house_num1 = t_tblcli-house_num1.
*      adrc-city2 = t_tblcli-poblac2.
*      MODIFY adrc.
*    ENDIF.
*
*  ENDIF.
*  CLEAR messtab.
*  REFRESH messtab.
*
*ENDFORM.                    " f_bdc_create_known_debitor

*&---------------------------------------------------------------------*
*&      Form  f_bdc_screen_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* Inicio modif. Genis 19.07.2007
*FORM f_bdc_screen_0100_header.
*
*  PERFORM bdc_dynpro      USING 'SAPMF02D'    '0100'.
*FORM f_bdc_screen_0100_header USING pi_dynpro.
*
*  PERFORM bdc_dynpro      USING 'SAPMF02D'    pi_dynpro.
** Fin modif. Genis 19.07.2007
*  PERFORM bdc_field       USING 'BDC_CURSOR'  'RF02D-KUNNR'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
*  PERFORM bdc_field       USING 'RF02D-KUNNR'  t_tblcli-kunnrsap.
*  PERFORM bdc_field       USING 'RF02D-BUKRS'  t_tblcli-bukrs.
*
** Inicio modif. Genis 19.07.2007
*  IF pi_dynpro = '0101'.
*    PERFORM bdc_field  USING: 'RF02D-D0210'  'X',
*                              'RF02D-D0215'  'X',
*                              'RF02D-D0220'  'X',
*                              'RF02D-D0230'  'X',
*                              'RF02D-D0610'  'X'.
*  ELSE.
*    PERFORM bdc_field  USING 'RF02D-ktokd'  t_tblcli-ktokd.
*    PERFORM bdc_field  USING 'USE_ZAV'  'X'.
*  ENDIF.
** Fin modif. Genis 19.07.2007
*
*ENDFORM.                    " f_bdc_screen_0100
*
**&---------------------------------------------------------------------*
**&      Form  F_BDC_SCREEn_0100_VENTAS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM f_bdc_screen_0100_ventas CHANGING lv_vkorg
*                                       lv_vtweg
*                                       lv_spart.
*
*  SELECT SINGLE vkorg
*    INTO lv_vkorg
*    FROM tvko
*    WHERE bukrs = t_tblcli-bukrs.
*  IF sy-subrc EQ 0.
*    SELECT SINGLE vtweg spart
*      INTO (lv_vtweg, lv_spart)
*      FROM tvta
*      WHERE vkorg = lv_vkorg.
*    IF sy-subrc EQ 0.
*      PERFORM bdc_field       USING 'RF02D-VKORG'  lv_vkorg.
*      PERFORM bdc_field       USING 'RF02D-VTWEG'  lv_vtweg.
*      PERFORM bdc_field       USING 'RF02D-SPART'  lv_spart.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                    " F_BDC_SCREEn_0100_VENTAS
*
**&---------------------------------------------------------------------*
**&      Form  F_BDC_SCREEn_0111_ADDRESS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM f_bdc_screen_0111_address.
*
*  DATA lv_name TYPE char40.
*
*  PERFORM bdc_dynpro       USING 'SAPMF02D'  '0111'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'  'ADDR1_DATA-NAME1'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
*  CONCATENATE t_tblcli-name1 t_tblcli-name2 INTO lv_name SEPARATED BY space.
*  PERFORM bdc_field       USING 'ADDR1_DATA-NAME1'     lv_name.
*  PERFORM bdc_field       USING 'ADDR1_DATA-SORT1'     t_tblcli-sort1.
*  PERFORM bdc_field       USING 'ADDR1_DATA-SORT2'     t_tblcli-sort2.
*  PERFORM bdc_field       USING 'ADDR1_DATA-STREET'    t_tblcli-direc1.
*  PERFORM bdc_field       USING 'ADDR1_DATA-CITY1'     t_tblcli-poblac1.
*  PERFORM bdc_field       USING 'ADDR1_DATA-COUNTRY'   t_tblcli-pais.
*  PERFORM bdc_field       USING 'ADDR1_DATA-REGION'    t_tblcli-region.
*  PERFORM bdc_field       USING 'ADDR1_DATA-LANGU'     sy-langu.
*  PERFORM bdc_field       USING 'ADDR1_DATA-POST_CODE1'  t_tblcli-cod_post.
*  PERFORM bdc_field       USING 'SZA1_D0100-TEL_NUMBER'  t_tblcli-tel.
*  PERFORM bdc_field       USING 'SZA1_D0100-MOB_NUMBER'  t_tblcli-mob_numb.
*  PERFORM bdc_field       USING 'SZA1_D0100-FAX_NUMBER'  t_tblcli-fax.
*  PERFORM bdc_field       USING 'SZA1_D0100-SMTP_ADDR'   t_tblcli-mail.
*
*
*ENDFORM.                    " F_BDC_SCREEn_0111_ADDRESS
*
**&---------------------------------------------------------------------*
**&      Form  f_bdc_screen_0120_CONTROL
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM f_bdc_screen_0120_control.
*
*  PERFORM bdc_dynpro       USING 'SAPMF02D'  '0120'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNA1-STCD1'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
**  IF t_tblcli-ktokd <> '1020' AND t_tblcli-ktokd <> '1910'.
**    IF t_tblcli-ktokd <> '1020' AND t_tblcli-ktokd <> '1910' and
**     t_tblcli-ktokd <> '1010' .
**    PERFORM bdc_field       USING 'KNA1-VBUND'  t_tblcli-vbund.
**  ENDIF.
**  PERFORM bdc_field       USING 'KNA1-BRSCH'  t_tblcli-brsch.
*  PERFORM bdc_field       USING 'KNA1-STCD1'  t_tblcli-nif.
**  PERFORM bdc_field       USING 'KNA1-STCD2'  t_tblcli-nif2.
*  PERFORM bdc_field       USING 'KNA1-STKZN'  t_tblcli-stkzn.
*  PERFORM bdc_field       USING 'KNA1-STCD3'  t_tblcli-nif3.
**  PERFORM bdc_field       USING 'KNA1-STCEG'  t_tblcli-stceg.
*
*ENDFORM.                    " f_bdc_screen_0120_CONTROL
*
**&---------------------------------------------------------------------*
**&      Form  F_BDC_SCREEN_0360_CONTACT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM f_bdc_screen_0360_contact.
*
*  PERFORM bdc_dynpro      USING 'SAPMF02D'  '0360'.
**   PERFORM bdc_field       USING 'BDC_CURSOR'  'KNVK-ANRED(01)'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNVK-NAMEV(01)'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'  '=VW'.
**  PERFORM bdc_field       USING 'KNVK-NAMEV(01)'  t_tblcli-contac_name2.
**  PERFORM bdc_field       USING 'KNVK-NAME1(01)'  t_tblcli-contac_name1.
**  PERFORM bdc_dynpro       USING 'SAPMF02D'  '0360'.
**  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNVK-NAMEV(01)'.
**  PERFORM bdc_field       USING 'BDC_OKCODE'  '=ENTR'.
*
*ENDFORM.                    " F_BDC_SCREEN_0360_CONTACT
*
**&---------------------------------------------------------------------*
**&      Form  f_bdc_screen_0130_BANK
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM f_bdc_screen_0130_paymnt.
*
*  PERFORM bdc_dynpro       USING 'SAPMF02D'  '0130'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNBK-BANKS(01)'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'  '=VW'.
**  PERFORM bdc_field       USING 'KNA1-DTAMS' t_tblcli-dtams.
*
*ENDFORM.                    " f_bdc_screen_0130_BANK
*
** Inicio modif. Genis 19.07.2007
**&---------------------------------------------------------------------*
**&      Form  f_bdc_modify_company_data
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM f_bdc_modify_company_data.
*
*  CLEAR: bdcdata, bdcdata[].
*  PERFORM f_bdc_screen_0100_header USING '0101'.
*  PERFORM f_bdc_datos_sociedad.
*
*  CLEAR: messtab, messtab[].
*  CALL TRANSACTION 'XD02' USING bdcdata
*    MODE p_ctmode
*    UPDATE 'S'
*    MESSAGES INTO messtab.
*
*  READ TABLE messtab WITH KEY msgtyp = 'E'.
*  IF sy-subrc = 0.
*    lt_log-msgtyp = 'E'.
*  ELSE.
*    lt_log-msgtyp = 'S'.
*  ENDIF.
*  lt_log-msgid = 'ZFI01'.
*  lt_log-msgnr = '080'.
*  lt_log-msgv1 = t_tblcli-kunnr.
*  APPEND lt_log.
*
*  CLEAR: messtab.
*  LOOP AT messtab.
*    MOVE-CORRESPONDING messtab TO lt_log.
*    APPEND lt_log.
*  ENDLOOP.
*
*ENDFORM.                    " f_bdc_modify_company_data
*
**&---------------------------------------------------------------------*
**&      Form  f_bdc_datos_sociedad
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM f_bdc_datos_sociedad.
*
*  PERFORM bdc_dynpro       USING 'SAPMF02D'  '0210'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNB1-AKONT'.
*  PERFORM bdc_field       USING 'KNB1-AKONT'  t_tblcli-akont.
**  IF t_tblcli-ktokd <> '1020' AND t_tblcli-ktokd <> '1910' and
**     t_tblcli-ktokd <> '1010'."AAR 16.01.2012
**    PERFORM bdc_field       USING 'KNB1-VZSKZ'  t_tblcli-vzskz.
**  ENDIF.
*  PERFORM bdc_field       USING 'KNB1-FDGRV'  t_tblcli-fdgrv.
*  PERFORM bdc_field       USING 'KNB1-ALTKN'  t_tblcli-altkn.
**  IF t_tblcli-ktokd = '1040'.
**    PERFORM bdc_field       USING 'KNB1-VZSKZ'  'Z1'.
**  ENDIF.
*  PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
*
*  PERFORM bdc_dynpro       USING 'SAPMF02D'  '0215'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNB1-ZTERM'.
*  PERFORM bdc_field       USING 'KNB1-ZTERM'  t_tblcli-zterm.
*  PERFORM bdc_field       USING 'KNB1-ZAHLS'  t_tblcli-zahls.
*  PERFORM bdc_field       USING 'KNB1-ZWELS'  t_tblcli-zwels.
*  PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
*
*  PERFORM bdc_dynpro       USING 'SAPMF02D'  '0220'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNB5-MAHNA'.
*  PERFORM bdc_field       USING 'KNB5-MAHNA'  t_tblcli-mahna.
*  PERFORM bdc_field       USING 'KNB1-BUSAB'  t_tblcli-busab.
*  PERFORM bdc_field       USING 'KNB5-MANSP'  t_tblcli-mansp.
*  PERFORM bdc_field       USING 'KNB5-KNRMA'  t_tblcli-knrma.
*  PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
*
*  PERFORM bdc_dynpro       USING 'SAPMF02D'  '0230'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNB1-VRSNR'.
*  PERFORM bdc_field       USING 'KNB1-VRSNR'  t_tblcli-vrsnr.
*  PERFORM bdc_field       USING 'BDC_OKCODE'  '=00'.
**  IF t_tblcli-ktokd <> '1040'.
**    PERFORM bdc_dynpro       USING 'SAPMF02D'  '0610'.
**    PERFORM bdc_field       USING 'BDC_CURSOR'  'KNBW-WITHT(01)'.
**    PERFORM bdc_field       USING 'KNBW-WITHT(01)'  t_tblcli-witht.
**    PERFORM bdc_field       USING 'KNBW-WT_WITHCD(01)'  t_tblcli-wt_withcd.
**    PERFORM bdc_field       USING 'KNBW-WT_AGENT(01)'   t_tblcli-wt_agent.
**    IF NOT t_tblcli-wt_agtdf IS INITIAL.
**      CLEAR: ano, mes, dia.
**      ano = t_tblcli-wt_agtdf(4).
**      mes = t_tblcli-wt_agtdf+2(2).
**      dia = t_tblcli-wt_agtdf+4(2).
**      CLEAR t_tblcli-wt_agtdf.
**      CONCATENATE dia mes ano INTO t_tblcli-wt_agtdf.
**      PERFORM bdc_field       USING 'KNBW-WT_AGTDF(01)'  t_tblcli-wt_agtdf.
**      IF NOT t_tblcli-wt_agtdf IS INITIAL.
**        CLEAR: ano, mes, dia.
**        ano = t_tblcli-wt_agtdf+4(4).
**        mes = t_tblcli-wt_agtdf+2(2).
**        dia = t_tblcli-wt_agtdf(2).
**        CLEAR t_tblcli-wt_agtdf.
**        CONCATENATE ano mes dia INTO t_tblcli-wt_agtdf.
**      ENDIF.
**    ENDIF.
**    IF NOT t_tblcli-wt_agtdt IS INITIAL.
**      CLEAR: ano, mes, dia.
**      ano = t_tblcli-wt_agtdt(4).
**      mes = t_tblcli-wt_agtdt+2(2).
**      dia = t_tblcli-wt_agtdt+4(2).
**      CLEAR t_tblcli-wt_agtdt.
**      CONCATENATE dia mes ano INTO t_tblcli-wt_agtdt.
**      PERFORM bdc_field       USING 'KNBW-WT_AGTDT(01)'  t_tblcli-wt_agtdt.
**      IF NOT t_tblcli-wt_agtdt IS INITIAL.
**        CLEAR: ano, mes, dia.
**        ano = t_tblcli-wt_agtdt+4(4).
**        mes = t_tblcli-wt_agtdt+2(2).
**        dia = t_tblcli-wt_agtdt(2).
**        CLEAR t_tblcli-wt_agtdt.
**        CONCATENATE ano mes dia INTO t_tblcli-wt_agtdt.
**      ENDIF.
**    ENDIF.
**    PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
**
**
**    PERFORM bdc_dynpro       USING 'SAPMF02D'  '0610'.
**    PERFORM bdc_field       USING 'BDC_CURSOR'  'KNBW-WITHT(01)'.
**    PERFORM bdc_field       USING 'BDC_OKCODE'  '=AO01'.
**  ENDIF.
*
*ENDFORM.                    " f_bdc_datos_sociedad

*&---------------------------------------------------------------------*
*&      Form  crear_asoc_clientes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM crear_asoc_clientes.

  SELECT SINGLE hotel
    FROM zficonv_cli_pms
    INTO t_tblcli-bukrs
    WHERE hotel = t_tblcli-bukrs
      AND cli_pms = t_tblcli-kunnr.
*      AND cli_sap = t_tblcli-kunnrsap.

  IF sy-subrc <> 0.
    DELETE FROM zficonv_cli_pms
      WHERE hotel = zfit_sol_cliente-bukrs
        AND cli_pms = zfit_sol_cliente-kunnr.

    zficonv_cli_pms-mandt = sy-mandt.
    zficonv_cli_pms-hotel = zfit_sol_cliente-bukrs.
    zficonv_cli_pms-cli_pms = zfit_sol_cliente-kunnr.
*    zficonv_cli_pms-cli_sap = zfit_sol_cliente-kunnrsap.
    INSERT zficonv_cli_pms.
  ENDIF.

ENDFORM.                    " crear_asoc_clientes
* Fin modif. Genis 19.07.2007

*&---------------------------------------------------------------------*
*&      Form  nuevo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM nuevo.
  CLEAR t_tblcli.
  APPEND t_tblcli.
ENDFORM.                    " nuevo

*&---------------------------------------------------------------------*
*&      Form  f_check_kunnr_vs_bukrs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*FORM f_check_kunnr_vs_bukrs.
*
*  DATA: lv_vkorg TYPE vkorg,
*        lv_vtweg TYPE vtweg,
*        lv_spart TYPE spart,
*        save_kunnr_pms TYPE zcli_pms,
*        save_bukrs_dynpro TYPE bukrs.
*
*  save_kunnr_pms = g_tblcli_wa-kunnr.
*  save_bukrs_dynpro = g_tblcli_wa-bukrs.

*  IF g_tblcli_wa-kunnrsap IS NOT INITIAL
*    AND g_tblcli_wa-bukrs  IS NOT INITIAL.
*    SELECT SINGLE *
*      FROM knb1
*      WHERE kunnr = g_tblcli_wa-kunnrsap
*      AND   bukrs = g_tblcli_wa-bukrs.
*    IF sy-subrc EQ 0.
** Si el cliente ya existe para la sociedad --> ERROR
** Inicio modif Genis 19.07.2007
**      MESSAGE e351(zfi01) WITH knb1-kunnr knb1-bukrs.
**      EXIT.
*      SELECT SINGLE cli_pms
*        FROM zficonv_cli_pms
*        INTO g_tblcli_wa-kunnr
*        WHERE hotel = g_tblcli_wa-bukrs
*          AND cli_pms = g_tblcli_wa-kunnr
*          AND cli_sap = g_tblcli_wa-kunnrsap.
*
*      IF sy-subrc = 0.
*        MESSAGE e351(zfi01) WITH knb1-kunnr knb1-bukrs g_tblcli_wa-kunnr.
*        EXIT.
*      ELSE.
*        PERFORM copia_datos_cliente.
*      ENDIF.
** Fin modif Genis 19.07.2007
*    ELSE.
** Inicio modif Genis 19.07.2007
*      PERFORM copia_datos_cliente.
**      SELECT SINGLE *
**        FROM knb1
**        WHERE kunnr = g_tblcli_wa-kunnrsap.
**      IF sy-subrc EQ 0.
*** Copiamos los principales campos de la Sociedad en caso de que
*** el cliente exista en otra sociedad
**        MOVE-CORRESPONDING knb1 TO g_tblcli_wa.
**        SELECT SINGLE *
**          FROM kna1
**          WHERE kunnr = g_tblcli_wa-kunnrsap.
**        IF sy-subrc EQ 0.
**          MOVE-CORRESPONDING kna1 TO g_tblcli_wa.
**        ENDIF.
**        SELECT SINGLE vkorg
**            INTO lv_vkorg
**            FROM tvko
**            WHERE bukrs = t_tblcli-bukrs.
**        IF sy-subrc EQ 0.
**          SELECT SINGLE vtweg spart
**            INTO (lv_vtweg, lv_spart)
**            FROM tvta
**            WHERE vkorg = lv_vkorg.
**          IF sy-subrc EQ 0.
**            SELECT SINGLE *
**              INTO CORRESPONDING FIELDS OF g_tblcli_wa
**              FROM knvv
**              WHERE kunnr = g_tblcli_wa-kunnrsap
**              AND   vkorg = lv_vkorg
**              AND   vtweg = lv_vtweg
**              AND   spart = lv_spart.
**          ENDIF.
**
**        ENDIF.
***       Otros campos que no se llaman igual que en estándar
***.....  añadir aquí los que correspondan
**        g_tblcli_wa-pais     = kna1-land1.
**        g_tblcli_wa-direc1   = kna1-stras.
**        g_tblcli_wa-poblac1  = kna1-ort01.
**        g_tblcli_wa-sort1    = kna1-sortl.
**        g_tblcli_wa-cod_post = kna1-pstlz.
**        g_tblcli_wa-region   = kna1-regio.
**        g_tblcli_wa-nif      = kna1-stcd1.
**        g_tblcli_wa-nif2     = kna1-stcd2.
**        ....
**      ENDIF.
** Fin modif Genis 19.07.2007
*
*    ENDIF.
*  ENDIF.
*  g_tblcli_wa-kunnr = save_kunnr_pms.
*  g_tblcli_wa-bukrs = save_bukrs_dynpro.
*
*ENDFORM.                    " f_check_kunnr_vs_bukrs

*&---------------------------------------------------------------------*
*&      Form  copia_datos_cliente
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*FORM copia_datos_cliente.
**  SELECT SINGLE *
**    FROM knb1
**    WHERE kunnr = g_tblcli_wa-kunnrsap.
*
*  SELECT SINGLE partner
*    FROM but000
*    WHERE partner = @t_tblcli-partner.
*
*  IF sy-subrc EQ 0.
**   Copiamos los principales campos de la Sociedad en caso de que
**   el cliente exista en otra sociedad
*    MOVE-CORRESPONDING but000 TO g_tblcli_wa.
*
**    SELECT SINGLE *
**      FROM kna1
**      WHERE kunnr = g_tblcli_wa-kunnrsap.
**
**    IF sy-subrc EQ 0.
**      MOVE-CORRESPONDING kna1 TO g_tblcli_wa.
**    ENDIF.
*
*    SELECT SINGLE vkorg
*      INTO lv_vkorg
*      FROM tvko
*      WHERE bukrs = t_tblcli-bukrs.
*
*    IF sy-subrc EQ 0.
*      SELECT SINGLE vtweg spart
*        INTO (lv_vtweg, lv_spart)
*        FROM tvta
*        WHERE vkorg = lv_vkorg.
*
*      IF sy-subrc EQ 0.
*        SELECT SINGLE *
*          INTO CORRESPONDING FIELDS OF g_tblcli_wa
*          FROM knvv
*          WHERE kunnr = g_tblcli_wa-kunnrsap
*            AND vkorg = lv_vkorg
*            AND vtweg = lv_vtweg
*            AND spart = lv_spart.
*      ENDIF.
*    ENDIF.
*
**   Otros campos que no se llaman igual que en estándar
**  .....  añadir aquí los que correspondan
*    g_tblcli_wa-pais     = kna1-land1.
*    g_tblcli_wa-direc1   = kna1-stras.
*    g_tblcli_wa-poblac1  = kna1-ort01.
*    g_tblcli_wa-sort1    = kna1-sortl.
*    g_tblcli_wa-cod_post = kna1-pstlz.
*    g_tblcli_wa-stkzn    = kna1-stkzn.
*    g_tblcli_wa-region   = kna1-regio.
*    g_tblcli_wa-nif      = kna1-stcd1.
*    g_tblcli_wa-nif2     = kna1-stcd2.
*    g_tblcli_wa-nif3     = kna1-stcd3.
*  ENDIF.
*
*ENDFORM.                    " copia_datos_cliente
*&---------------------------------------------------------------------*
*&      Form  DESARCHIVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM desarchivar .
  LOOP AT t_tblcli WHERE sel = 'X'.
    IF t_tblcli-partner IS INITIAL.
      MOVE-CORRESPONDING t_tblcli TO zfit_sol_cliente.
      zfit_sol_cliente-estado = 'S'.
      SELECT SINGLE ddtext FROM dd07v INTO zfit_sol_cliente-estadot
                        WHERE domname = 'ZZDCLIEST'
                          AND ddlanguage = sy-langu
                          AND domvalue_l = zfit_sol_cliente-estado.
      MODIFY zfit_sol_cliente.
      IF sy-subrc = 0.
        INSERT zficonv_cli_pms.
*      DELETE FROM zficonv_cli_pms
*        WHERE hotel = t_tblcli-bukrs
*          AND cli_pms = t_tblcli-kunnr.
*      MESSAGE s022(zfi01).
        t_tblcli-estado = 'S'.
        DELETE t_tblcli.
      ENDIF.
    ELSE.
      MESSAGE i273(zfi01) WITH t_tblcli-partner.
    ENDIF.
  ENDLOOP.
  IF sy-subrc <> 0.
    MESSAGE w031(zfi01).
  ENDIF.
ENDFORM.                    " DESARCHIVAR
*&---------------------------------------------------------------------*
*&      Form  F_BDC_SCREEN_0125
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*FORM f_bdc_screen_0125 .
*  PERFORM bdc_dynpro      USING 'SAPMF02D'  '0125'.
**  PERFORM bdc_field       USING 'BDC_CURSOR'  'KNA1-BRSCH'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
***  PERFORM bdc_field       USING 'KNA1-BRSCH'  t_tblcli-brsch.
*ENDFORM.                    " F_BDC_SCREEN_0125

*INI GAP016_BP
FORM derivar_akont
  CHANGING cs_cliente LIKE g_tblcli_wa.

  DATA:
    lv_land_soc TYPE t001-land1,
    lv_nac_ext  TYPE ztbp001-nac_ext,
    lv_group    TYPE ztbp001-bu_group,
    ls_ztbp001 TYPE ztbp001.

  CLEAR cs_cliente-akont.

  IF cs_cliente-bukrs IS INITIAL
  OR cs_cliente-pais  IS INITIAL.
    RETURN.
  ENDIF.

  SELECT SINGLE land1
    FROM t001
    INTO @lv_land_soc
    WHERE bukrs = @cs_cliente-bukrs.

  IF sy-subrc = 0.
    IF cs_cliente-pais = lv_land_soc AND cs_cliente-bu_group <> 'ZINT'.
      ls_ztbp001-nac_ext = 'N'.
      CLEAR: ls_ztbp001-bu_group.
  ELSEIF cs_cliente-pais <> lv_land_soc AND cs_cliente-bu_group <> 'ZINT'.
      ls_ztbp001-nac_ext = 'E'.
      CLEAR: ls_ztbp001-bu_group.
  ELSEIF cs_cliente-pais = lv_land_soc AND cs_cliente-bu_group = 'ZINT'.
      ls_ztbp001-nac_ext = 'N'.
      ls_ztbp001-bu_group = 'ZINT'.
  ELSEIF cs_cliente-pais <> lv_land_soc AND cs_cliente-bu_group = 'ZINT'.
      ls_ztbp001-nac_ext = 'E'.
      ls_ztbp001-bu_group = 'ZINT'.
    ENDIF.
  ENDIF.

  SELECT SINGLE hkont
  FROM ztbp001
  INTO @cs_cliente-akont
  WHERE koart    = @gc_koart_debtor
  AND bu_group = @ls_ztbp001-bu_group
  AND nac_ext  = @ls_ztbp001-nac_ext.

  IF sy-subrc <> 0.
    CLEAR cs_cliente-akont.
  ENDIF.

ENDFORM.

FORM f_bp_create CHANGING cv_subrc TYPE sysubrc.

  DATA:
    ls_bp         TYPE cvis_ei_extern,
    lt_bp         TYPE cvis_ei_extern_t,
    lv_partner    TYPE bu_partner,
    lv_tax_type_1 TYPE dfkkbptaxnum-taxtype,
    lv_tax_type_3 TYPE dfkkbptaxnum-taxtype.

  CLEAR cv_subrc.

  PERFORM derivar_akont CHANGING t_tblcli.
  PERFORM derivar_tipos_nif
    USING    t_tblcli-pais
             t_tblcli-nif
    CHANGING lv_tax_type_1
             lv_tax_type_3.

  CLEAR ls_bp.

  ls_bp-partner-header-object_task = 'I'.

  "Debe comprobarse el nombre exacto del componente según release
  ls_bp-partner-central_data-common-data-bp_control-category = '2'.
  ls_bp-partner-central_data-common-data-bp_control-grouping =
    t_tblcli-bu_group.

  ls_bp-partner-central_data-common-data-bp_centraldata-partnertype =
    t_tblcli-bu_group.

  ls_bp-partner-central_data-common-data-bp_organization-name1 =
    t_tblcli-name1.

  ls_bp-partner-central_data-common-data-bp_organization-name2 =
    t_tblcli-name2.

*  ls_bp-partner-central_data-common-data-bp_centraldata-natpers =
*    t_tblcli-stkzn.

  "Añadir dirección y comunicaciones aquí.
  "Añadir TAXNUMXL/TAXNUM aquí.
  "Añadir cliente FLCU00 únicamente si BUKRS está informado.

  APPEND ls_bp TO lt_bp.

  cl_md_bp_maintain=>validate_single(
    EXPORTING
      i_data = ls_bp
    IMPORTING
      et_return_map = DATA(lt_return_map) ).

*  IF line_exists( lt_return_map[ type = 'E' ] )
*  OR line_exists( lt_return_map[ type = 'A' ] ).
*    cv_subrc = 4.
*    PERFORM convertir_mensajes_bp
*      USING lt_return_map.
*    RETURN.
*  ENDIF.

  cl_md_bp_maintain=>maintain(
    EXPORTING
      i_data = lt_bp
    IMPORTING
      e_return = DATA(lt_return) ).

  "Comprobar errores según la estructura de retorno del release.

  IF cv_subrc = 0.
    COMMIT WORK AND WAIT.

    "Recuperar el partner devuelto por API o por GUID.
    t_tblcli-partner = lv_partner.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.

FORM derivar_tipos_nif
  USING    iv_pais      TYPE land1
           iv_nif       TYPE zfit_sol_cliente-nif
  CHANGING cv_taxtype_1 TYPE dfkkbptaxnum-taxtype
           cv_taxtype_3 TYPE dfkkbptaxnum-taxtype.

  CLEAR:
    cv_taxtype_1,
    cv_taxtype_3.

  IF iv_nif = gc_rfc_generico_mx.
    cv_taxtype_1 = 'MX1'.
  ELSEIF iv_pais IS NOT INITIAL.
    CONCATENATE iv_pais '1'
      INTO cv_taxtype_1.
  ENDIF.

  IF iv_pais IS NOT INITIAL.
    CONCATENATE iv_pais '3'
      INTO cv_taxtype_3.
  ENDIF.

ENDFORM.

FORM f_bp_extend_flcu00 CHANGING cv_subrc TYPE sysubrc.

  CLEAR cv_subrc.

  IF t_tblcli-bukrs IS INITIAL.
    RETURN.
  ENDIF.

  SELECT SINGLE partner
    FROM but000
    INTO @DATA(lv_partner)
    WHERE partner = @t_tblcli-partner.

  IF sy-subrc <> 0.
    cv_subrc = 4.
    RETURN.
  ENDIF.

  PERFORM derivar_akont CHANGING t_tblcli.

ENDFORM.

FORM agregar_mensaje_bp
  USING is_return TYPE bapiret2.

  CLEAR lt_log.

  lt_log-msgtyp = is_return-type.
  lt_log-msgid  = is_return-id.
  lt_log-msgnr  = is_return-number.
  lt_log-msgv1  = is_return-message_v1.
  lt_log-msgv2  = is_return-message_v2.
  lt_log-msgv3  = is_return-message_v3.
  lt_log-msgv4  = is_return-message_v4.

  APPEND lt_log.

ENDFORM.

FORM download_xls.
  DATA:
    lv_filename TYPE string,
    lv_path     TYPE string,
    lv_fullpath TYPE string.

  " Solicitar al usuario dónde guardar el fichero
  cl_gui_frontend_services=>file_save_dialog(
  EXPORTING
    default_extension = 'xlsx'
    default_file_name = 'datos.xlsx'
    file_filter       = 'Excel (*.xlsx)|*.xlsx|'
  CHANGING
    filename          = lv_filename
    path              = lv_path
    fullpath          = lv_fullpath
  EXCEPTIONS
    OTHERS            = 1 ).

  IF sy-subrc <> 0 OR lv_fullpath IS INITIAL.
    RETURN.
  ENDIF.

  DATA:lt_download    TYPE STANDARD TABLE OF zfit_sol_cliente WITH EMPTY KEY,
       lo_salv        TYPE REF TO cl_salv_table,
       lo_result_data TYPE REF TO cl_salv_ex_result_data_table,
       lv_xstring     TYPE xstring.

  TRY.
      " Crear SALV en memoria únicamente para obtener los metadatos
      cl_salv_table=>factory(
      IMPORTING
        r_salv_table = lo_salv
      CHANGING
        t_table      = lt_download ).
      " Obtener el catálogo de campos a partir de la tabla
      DATA(lt_fieldcatalog) =
            cl_salv_controller_metadata=>get_lvc_fieldcatalog(
            r_columns      = lo_salv->get_columns( )
            r_aggregations = lo_salv->get_aggregations( ) ).

      lt_download = CORRESPONDING #( t_tblcli[] ).

      lo_result_data =
      cl_salv_ex_util=>factory_result_data_table(
      r_data         = REF #( lt_download )
      t_fieldcatalog = lt_fieldcatalog ).

      " Generar un XLSX real en formato XSTRING
      cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform(
      EXPORTING
        xml_type      = if_salv_bs_xml=>c_type_xlsx
        xml_version   = cl_salv_bs_a_xml_base=>get_version( )
        r_result_data = lo_result_data
        xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
        gui_type      = if_salv_bs_xml=>c_gui_type_gui
      IMPORTING
        xml           = lv_xstring ).

    CATCH cx_salv_msg INTO DATA(lx_salv).
      MESSAGE lx_salv->get_text( ) TYPE 'E'.
      RETURN.
  ENDTRY.

  DATA(lt_binary) = cl_bcs_convert=>xstring_to_solix(
        iv_xstring = lv_xstring ).

  DATA(lv_size) = xstrlen( lv_xstring ).

  " Descargar la tabla interna mostrada en el Table Control
  cl_gui_frontend_services=>gui_download(
  EXPORTING
    filename     = lv_fullpath
    filetype     = 'BIN'
    bin_filesize = lv_size
  CHANGING
    data_tab     = lt_binary
  EXCEPTIONS
    OTHERS       = 1 ).

  IF sy-subrc <> 0.
    MESSAGE 'Error al descargar el fichero' TYPE 'E'.
  ENDIF.

ENDFORM.

*FIN GAP016_BP

*&---------------------------------------------------------------------*
*& Include          ZFI0011CLS
*&---------------------------------------------------------------------*
CLASS zcl_bp DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS maintain_bp
      CHANGING
        cs_cliente       TYPE zfit_sol_cliente
      RETURNING
        VALUE(rs_result) TYPE ty_result.

  PRIVATE SECTION.

    METHODS determine_context
      CHANGING
        cs_cliente        TYPE zfit_sol_cliente
      RETURNING
        VALUE(rs_context) TYPE ty_context.

    METHODS map_bp_data
      CHANGING
        cs_cliente TYPE zfit_sol_cliente
        cs_context TYPE ty_context
        cs_data    TYPE cvis_ei_extern.

    METHODS map_roles
      CHANGING
        cs_cliente TYPE zfit_sol_cliente
        cs_data    TYPE cvis_ei_extern.

    METHODS map_tax_numbers
      CHANGING
        cs_cliente TYPE zfit_sol_cliente
        cs_data    TYPE cvis_ei_extern.

    METHODS map_tax_number
      IMPORTING
        iv_extended TYPE abap_bool
      CHANGING
        cs_cliente  TYPE zfit_sol_cliente
        cv_taxtype  TYPE dfkkbptaxnum-taxtype
        cv_value    TYPE string
        cs_data     TYPE cvis_ei_extern.

    METHODS map_bp_address
      CHANGING
        cs_cliente TYPE zfit_sol_cliente
        cs_context TYPE ty_context
        cs_data    TYPE cvis_ei_extern.

    METHODS map_bp_communication
      CHANGING
        cs_cliente TYPE zfit_sol_cliente
        cs_context TYPE ty_context
        cs_address TYPE bus_ei_bupa_address.

    METHODS map_company_data
      CHANGING
        cs_cliente TYPE zfit_sol_cliente
        cs_context TYPE ty_context
        cs_data    TYPE cvis_ei_extern.

    METHODS map_dunning_data
      CHANGING
        cs_cliente TYPE zfit_sol_cliente
        cs_context TYPE ty_context
        cs_company TYPE cmds_ei_company.

    METHODS map_withholding_tax
      CHANGING
        cs_cliente TYPE zfit_sol_cliente
        cs_context TYPE ty_context
        cs_company TYPE cmds_ei_company.

    METHODS map_customer_data
      CHANGING
        cs_cliente TYPE zfit_sol_cliente
        cs_context TYPE ty_context
        cs_data    TYPE cvis_ei_extern.

    METHODS call_api
      IMPORTING
        is_data          TYPE cvis_ei_extern
      RETURNING
        VALUE(rt_return) TYPE bapiretm.

    METHODS evaluate_return
      IMPORTING
        it_return        TYPE bapiretm
      RETURNING
        VALUE(rs_result) TYPE ty_result.

ENDCLASS.

CLASS zcl_bp IMPLEMENTATION.

  METHOD determine_context.

    DATA:
          lv_customer TYPE kunnr.

    rs_context-valid = abap_true.

    " BP NUEVO
    IF cs_cliente-partner IS INITIAL.

      rs_context-bp_task  = gc_task_insert.
      rs_context-address_task = gc_task_insert.

      " GUID técnico obligatorio para el nuevo BP
      cl_system_uuid=>if_system_uuid_static~create_uuid_c32(
      RECEIVING
      uuid = rs_context-partner_guid ).

      " GUID de la dirección estándar
      cl_system_uuid=>if_system_uuid_static~create_uuid_c32(
      RECEIVING
      uuid = rs_context-address_guid ).

      " Solo creamos Customer/FLCU00 cuando existe sociedad
      IF cs_cliente-bukrs IS NOT INITIAL.
        rs_context-customer_task = gc_task_insert.
        rs_context-company_task  = gc_task_insert.
      ENDIF.

      RETURN.

    ENDIF.

    " BP EXISTENTE
    SELECT SINGLE partner_guid,
    bu_group
    FROM but000
    WHERE partner = @cs_cliente-partner
    INTO @DATA(ls_but000).

    IF sy-subrc <> 0.

      rs_context-valid = abap_false.
      rs_context-message = |El Business Partner { cs_cliente-partner } NO existe|.

      RETURN.

    ENDIF.

    rs_context-bp_task      = gc_task_update.
    rs_context-partner_guid = ls_but000-partner_guid.

    " La agrupación de un BP existente no debe modificarse
    IF cs_cliente-bu_group IS NOT INITIAL
    AND cs_cliente-bu_group <> ls_but000-bu_group.

      rs_context-valid = abap_false.
      rs_context-message =
      |El BP { cs_cliente-partner } pertenece a la agrupación | &&
      |{ ls_but000-bu_group }, NO a { cs_cliente-bu_group }|.

      RETURN.

    ENDIF.

    " DIRECCIÓN EXISTENTE
    SELECT address_guid,
    addr_valid_to
    FROM but020
    WHERE partner = @cs_cliente-partner
    INTO TABLE @DATA(lt_address).

    IF lt_address IS NOT INITIAL.

      SORT lt_address BY addr_valid_to DESCENDING.

      rs_context-address_guid = lt_address[ 1 ]-address_guid.
      rs_context-address_task = gc_task_update.

    ELSE.

      rs_context-address_task = gc_task_insert.

      cl_system_uuid=>if_system_uuid_static~create_uuid_c32(
      RECEIVING
      uuid = rs_context-address_guid ).

    ENDIF.

    " SIN SOCIEDAD NO HAY FLCU00
    IF cs_cliente-bukrs IS INITIAL.
      RETURN.
    ENDIF.

    " ¿EL BP YA ESTÁ VINCULADO A UN CUSTOMER?
    SELECT SINGLE customer
    FROM cvi_cust_link
    WHERE partner_guid = @ls_but000-partner_guid
    INTO @lv_customer.

    IF sy-subrc <> 0.

      " BP existe, pero todavía no es Customer
      rs_context-customer_task = gc_task_insert.
      rs_context-company_task  = gc_task_insert.

      RETURN.

    ENDIF.

    " BP ya tiene Customer
    " Guardamos el KUNNR asociado al BP
    rs_context-customer = lv_customer.
    rs_context-customer_task = gc_task_update.

    " ¿EXISTE YA EN LA SOCIEDAD?
    SELECT SINGLE @abap_true
    FROM knb1
    WHERE kunnr = @lv_customer
    AND bukrs = @cs_cliente-bukrs
    INTO @DATA(lv_company_exists).

    rs_context-company_task =
    COND #(
    WHEN sy-subrc = 0
    THEN gc_task_update
    ELSE gc_task_insert ).

  ENDMETHOD.

  METHOD maintain_bp.

    DATA:
      ls_data    TYPE cvis_ei_extern,
      ls_context TYPE ty_context.

    " Normalizar el número de BP si viene informado
    IF cs_cliente-partner IS NOT INITIAL.
      cs_cliente-partner = |{ cs_cliente-partner ALPHA = IN }|.
    ENDIF.

    " Determinar si debemos crear/modificar BP, Customer, dirección y sociedad
    ls_context = determine_context(
    CHANGING
      cs_cliente = cs_cliente ).

    IF ls_context-valid = abap_false.
      rs_result-success = abap_false.
      rs_result-message = ls_context-message.

      RETURN.

    ENDIF.

    " Datos generales del Business Partner
    map_bp_data(
    CHANGING
      cs_cliente = cs_cliente
      cs_context = ls_context
      cs_data    = ls_data ).

    " Números fiscales
    map_tax_numbers(
    CHANGING
      cs_cliente = cs_cliente
      cs_data    = ls_data ).

    " Dirección y comunicaciones del BP
    map_bp_address(
    CHANGING
      cs_cliente = cs_cliente
      cs_context = ls_context
      cs_data    = ls_data ).

    " 6. Datos Customer.
    IF cs_cliente-bukrs IS NOT INITIAL.
      " Rol FLCU00
      map_roles(
      CHANGING
        cs_cliente = cs_cliente
        cs_data    = ls_data ).

      " Datos generales Customer / KNA1
      map_customer_data(
      CHANGING
        cs_cliente = cs_cliente
        cs_context = ls_context
        cs_data    = ls_data ).

      " Datos de sociedad Customer / KNB1
      map_company_data(
      CHANGING
        cs_cliente = cs_cliente
        cs_context = ls_context
        cs_data    = ls_data ).

    ENDIF.

    " Ejecutar mantenimiento BP/CVI
    DATA(lt_return) = call_api( is_data = ls_data ).

    " Evaluar mensajes devueltos por CL_MD_BP_MAINTAIN
    rs_result = evaluate_return( it_return = lt_return ).

    rs_result-return = lt_return.

    " Commit o rollback
    IF rs_result-success = abap_true.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      " Si era un BP nuevo, recuperar el número generado
      IF cs_cliente-partner IS INITIAL.

        " Primero intentaremos utilizar el número recuperado
        " desde los mensajes de CL_MD_BP_MAINTAIN
        IF rs_result-partner IS INITIAL.

          " Como respaldo buscamos el BP por el GUID generado
          SELECT SINGLE partner
          FROM but000
          WHERE partner_guid = @ls_context-partner_guid
          INTO @rs_result-partner.

        ENDIF.

        IF rs_result-partner IS NOT INITIAL.
          cs_cliente-partner = rs_result-partner.
        ELSE.

          rs_result-success = abap_false.
          rs_result-message =
          'El BP fue procesado, pero no se pudo recuperar el número generado'.

        ENDIF.
      ELSE.
        " BP existente
        rs_result-partner = cs_cliente-partner.

      ENDIF.
    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ENDMETHOD.

  METHOD map_bp_data.

    " Cabecera del Business Partner
    cs_data-partner-header-object_task = cs_context-bp_task.

    cs_data-partner-header-object_instance-bpartnerguid = cs_context-partner_guid.

    " Si es un BP existente, informamos su número
    IF cs_cliente-partner IS NOT INITIAL.
      cs_data-partner-header-object_instance-bpartner = cs_cliente-partner.
    ENDIF.

    " Datos de control del BP
    " Solo se informa categoría y agrupación durante la creación
    IF cs_context-bp_task = gc_task_insert.

      cs_data-partner-central_data-common-data-bp_control-category = gc_bp_org.
      cs_data-partner-central_data-common-data-bp_control-grouping = cs_cliente-bu_group.

    ENDIF.

    " Clase de interlocutor comercial
    cs_data-partner-central_data-common-data-bp_centraldata-partnertype = cs_cliente-bu_group.

    cs_data-partner-central_data-common-datax-bp_centraldata-partnertype = abap_true.

    " Nombre de la organización
    cs_data-partner-central_data-common-data-bp_organization-name1 = cs_cliente-name1.
    cs_data-partner-central_data-common-datax-bp_organization-name1 = abap_true.
    cs_data-partner-central_data-common-data-bp_organization-name2 = cs_cliente-name2.
    cs_data-partner-central_data-common-datax-bp_organization-name2 = abap_true.

    " Términos de búsqueda
    cs_data-partner-central_data-common-data-bp_centraldata-searchterm1 = cs_cliente-sort1.
    cs_data-partner-central_data-common-datax-bp_centraldata-searchterm1 = abap_true.
    cs_data-partner-central_data-common-data-bp_centraldata-searchterm2 = cs_cliente-sort2.
    cs_data-partner-central_data-common-datax-bp_centraldata-searchterm2 =  abap_true.

    " Persona física a efectos fiscales
    cs_data-partner-central_data-taxnumber-common-data-nat_person = cs_cliente-stkzn.
    cs_data-partner-central_data-taxnumber-common-datax-nat_person = abap_true.

  ENDMETHOD.

  METHOD map_roles.

    FIELD-SYMBOLS:
    <fs_role> TYPE bus_ei_bupa_roles.

    DATA:
          lv_role_exists TYPE abap_bool VALUE abap_false.

    " FLCU00 únicamente se crea si existe Sociedad
    CHECK cs_cliente-bukrs IS NOT INITIAL.

    " Si el BP ya existe, comprobamos si ya tiene FLCU00
    IF cs_cliente-partner IS NOT INITIAL.

      SELECT SINGLE @abap_true
      FROM but100
      WHERE partner = @cs_cliente-partner
      AND rltyp    = @gc_role_flcu00
      INTO @lv_role_exists.

    ENDIF.

    " Si el rol todavía no existe, solicitar su creación
    IF lv_role_exists = abap_false.

      APPEND INITIAL LINE TO cs_data-partner-central_data-role-roles ASSIGNING <fs_role>.

      <fs_role>-task = gc_task_insert.
      <fs_role>-data_key = gc_role_flcu00.

    ENDIF.
  ENDMETHOD.
  METHOD map_tax_numbers.

    DATA:
      lv_taxtype1 TYPE dfkkbptaxnum-taxtype,
      lv_taxtype3 TYPE dfkkbptaxnum-taxtype,
      lv_value    TYPE string.

    " NIF principal -
    IF cs_cliente-nif IS NOT INITIAL.

      IF cs_cliente-nif = gc_rfc_generico_mx.

        lv_taxtype1 = 'MX1'.

      ELSEIF cs_cliente-pais IS NOT INITIAL.

        lv_taxtype1 = |{ cs_cliente-pais }1|.

      ENDIF.

      IF lv_taxtype1 IS NOT INITIAL.

        lv_value = CONV string( cs_cliente-nif ).

        map_tax_number(
        EXPORTING
          iv_extended = abap_true
        CHANGING
          cs_cliente  = cs_cliente
          cv_taxtype  = lv_taxtype1
          cv_value    = lv_value
          cs_data     = cs_data ).
      ENDIF.
    ENDIF.

    " NIF3
    IF cs_cliente-nif3 IS NOT INITIAL AND cs_cliente-pais IS NOT INITIAL.

      lv_taxtype3 = |{ cs_cliente-pais }3|.

      lv_value = CONV string( cs_cliente-nif3 ).

      map_tax_number(
      EXPORTING
        iv_extended = abap_false
      CHANGING
        cs_cliente  = cs_cliente
        cv_taxtype  = lv_taxtype3
        cv_value    = lv_value
        cs_data     = cs_data ).
    ENDIF.
  ENDMETHOD.

  METHOD map_tax_number.

    FIELD-SYMBOLS:
    <fs_tax> TYPE bus_ei_bupa_taxnumber.

    DATA:
          lv_task TYPE c LENGTH 1 VALUE gc_task_insert.

    " Para BP existente comprobar si ya existe el TAXTYPE
    IF cs_cliente-partner IS NOT INITIAL.

      SELECT SINGLE @abap_true
      FROM dfkkbptaxnum
      WHERE partner = @cs_cliente-partner
      AND taxtype = @cv_taxtype
      INTO @DATA(lv_exists).

      IF sy-subrc = 0.
        lv_task = gc_task_update.
      ENDIF.

    ENDIF.
    " Añadir el número fiscal al payload CVI
    APPEND INITIAL LINE TO cs_data-partner-central_data-taxnumber-taxnumbers ASSIGNING <fs_tax>.

    <fs_tax>-task = lv_task.

    " Categoría fiscal: MX1, DO1, DO3, etc.
    <fs_tax>-data_key-taxtype = cv_taxtype.

    " NIF principal -> TAXNUMXL
    " NIF3 -> TAXNUMBER
    IF iv_extended = abap_true.

      <fs_tax>-data_key-taxnumxl = cv_value.
    ELSE.

      <fs_tax>-data_key-taxnumber = cv_value.
    ENDIF.
  ENDMETHOD.

  METHOD map_customer_data.

    CHECK cs_cliente-bukrs IS NOT INITIAL.

    cs_data-customer-header-object_task = cs_context-customer_task.

    IF cs_context-customer IS NOT INITIAL.
      cs_data-customer-header-object_instance-kunnr = cs_context-customer.
    ENDIF.

    IF cs_context-customer_task = gc_task_insert.
      cs_data-ensure_create-create_customer = abap_true.
    ENDIF.

    IF cs_cliente-vbund IS NOT INITIAL.
      cs_data-customer-central_data-central-data-vbund = cs_cliente-vbund.
      cs_data-customer-central_data-central-datax-vbund = abap_true.
    ENDIF.

    IF cs_cliente-brsch IS NOT INITIAL.
      cs_data-customer-central_data-central-data-bran1 = cs_cliente-brsch.
      cs_data-customer-central_data-central-datax-bran1 = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD map_bp_address.

    DATA:
      lv_langu     TYPE spras,
      lv_langu_iso TYPE laiso.

    FIELD-SYMBOLS:
    <fs_address> TYPE bus_ei_bupa_address.

    APPEND INITIAL LINE TO cs_data-partner-central_data-address-addresses ASSIGNING <fs_address>.

    <fs_address>-task = cs_context-address_task.

    <fs_address>-data_key-guid = cs_context-address_guid.
    <fs_address>-data_key-operation = 'XXDFLT'.
    <fs_address>-data-postal-data-city = cs_cliente-poblac1.
    <fs_address>-data-postal-datax-city = abap_true.
    <fs_address>-data-postal-data-street = cs_cliente-direc1.
    <fs_address>-data-postal-datax-street = abap_true.
    <fs_address>-data-postal-data-str_suppl1 = cs_cliente-direc2.
    <fs_address>-data-postal-datax-str_suppl1 = abap_true.
    <fs_address>-data-postal-data-house_no = cs_cliente-house_num1.
    <fs_address>-data-postal-datax-house_no = abap_true.
    <fs_address>-data-postal-data-district = cs_cliente-poblac2.
    <fs_address>-data-postal-datax-district = abap_true.
    <fs_address>-data-postal-data-postl_cod1 = cs_cliente-cod_post.
    <fs_address>-data-postal-datax-postl_cod1 = abap_true.
    <fs_address>-data-postal-data-country = cs_cliente-pais.
    <fs_address>-data-postal-datax-country = abap_true.
    <fs_address>-data-postal-data-region = cs_cliente-region.
    <fs_address>-data-postal-datax-region = abap_true.
    lv_langu = COND #(
    WHEN cs_cliente-langu IS NOT INITIAL
    THEN cs_cliente-langu
    ELSE sy-langu ).

    <fs_address>-data-postal-data-langu = lv_langu.
    <fs_address>-data-postal-datax-langu = abap_true.

    " ISO del idioma
    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
      EXPORTING
        input  = lv_langu
      IMPORTING
        output = lv_langu_iso.

    IF lv_langu_iso IS NOT INITIAL.
      <fs_address>-data-postal-data-languiso = lv_langu_iso.
      <fs_address>-data-postal-datax-langu_iso =  abap_true.
    ENDIF.

    map_bp_communication(
   CHANGING
     cs_cliente = cs_cliente
     cs_context = cs_context
     cs_address = <fs_address> ).

  ENDMETHOD.

  METHOD map_bp_communication.

    FIELD-SYMBOLS:
      <fs_phone>  TYPE bus_ei_bupa_telephone,
      <fs_mobile> TYPE bus_ei_bupa_telephone,
      <fs_fax>    TYPE bus_ei_bupa_fax,
      <fs_smtp>   TYPE bus_ei_bupa_smtp.

    DATA:
      lv_addrnumber        TYPE ad_addrnum,
      lv_phone_task        TYPE c LENGTH 1,
      lv_phone_old         TYPE adr2-tel_number,
      lv_phone_consnumber  TYPE adr2-consnumber,
      lv_mobile_task       TYPE c LENGTH 1,
      lv_mobile_old        TYPE adr2-tel_number,
      lv_mobile_consnumber TYPE adr2-consnumber,
      lv_fax_task          TYPE c LENGTH 1,
      lv_fax_old           TYPE adr3-fax_number,
      lv_fax_consnumber    TYPE adr3-consnumber,
      lv_smtp_task         TYPE c LENGTH 1,
      lv_smtp_old          TYPE adr6-smtp_addr,
      lv_smtp_consnumber   TYPE adr6-consnumber.

    IF cs_cliente-partner IS NOT INITIAL AND cs_context-address_guid IS NOT INITIAL.

      SELECT SINGLE addrnumber
      FROM but020
      WHERE partner      = @cs_cliente-partner
      AND address_guid = @cs_context-address_guid
      INTO @lv_addrnumber.

    ENDIF.

    IF cs_cliente-tel IS NOT INITIAL.

      lv_phone_task = gc_task_insert.

      " Buscar teléfono normal existente
      IF lv_addrnumber IS NOT INITIAL.

        SELECT SINGLE tel_number,
        consnumber
        FROM adr2
        WHERE addrnumber = @lv_addrnumber
        AND persnumber = @space
        AND r3_user    = '1'
        INTO ( @lv_phone_old,
        @lv_phone_consnumber ).

        IF sy-subrc = 0.
          lv_phone_task = gc_task_update.
        ENDIF.

      ENDIF.

      " Solo enviamos el nodo si hay creación o cambio real
      IF lv_phone_task = gc_task_insert  OR lv_phone_old <> cs_cliente-tel.

        APPEND INITIAL LINE TO cs_address-data-communication-phone-phone ASSIGNING <fs_phone>.

        <fs_phone>-contact-task = lv_phone_task.
        <fs_phone>-contact-data-telephone = cs_cliente-tel.
        <fs_phone>-contact-datax-telephone = abap_true.

        " Tipo teléfono normal
        <fs_phone>-contact-data-r_3_user = '1'.
        <fs_phone>-contact-datax-r_3_user = abap_true.

        IF lv_phone_task = gc_task_update.

          " Identificamos exactamente el ADR2 existente
          <fs_phone>-contact-data-consnumber =  lv_phone_consnumber.
          <fs_phone>-contact-datax-consnumber = abap_true.

        ELSE.

          " Nuevo teléfono principal
          <fs_phone>-contact-data-std_no =  abap_true.
          <fs_phone>-contact-datax-std_no =  abap_true.

        ENDIF.
      ENDIF.
    ENDIF.

    IF cs_cliente-mob_numb IS NOT INITIAL.

      lv_mobile_task = gc_task_insert.

      IF lv_addrnumber IS NOT INITIAL.

        SELECT SINGLE tel_number,
        consnumber
        FROM adr2
        WHERE addrnumber = @lv_addrnumber
        AND persnumber = @space
        AND r3_user    = '3'
        INTO ( @lv_mobile_old,
        @lv_mobile_consnumber ).

        IF sy-subrc = 0.
          lv_mobile_task = gc_task_update.
        ENDIF.

      ENDIF.

      IF lv_mobile_task = gc_task_insert OR lv_mobile_old <> cs_cliente-mob_numb.

        APPEND INITIAL LINE  TO cs_address-data-communication-phone-phone ASSIGNING <fs_mobile>.

        <fs_mobile>-contact-task = lv_mobile_task.
        <fs_mobile>-contact-data-telephone = cs_cliente-mob_numb.
        " Indica que es teléfono móvil
        <fs_mobile>-contact-data-r_3_user = '3'.
        <fs_mobile>-contact-datax-telephone = abap_true.
        <fs_mobile>-contact-datax-r_3_user = abap_true.

        IF lv_mobile_task = gc_task_update.

          <fs_mobile>-contact-data-consnumber = lv_mobile_consnumber.
          <fs_mobile>-contact-datax-consnumber = abap_true.

        ENDIF.
      ENDIF.
    ENDIF.

    IF cs_cliente-fax IS NOT INITIAL.

      lv_fax_task = gc_task_insert.

      IF lv_addrnumber IS NOT INITIAL.

        SELECT SINGLE fax_number,
        consnumber
        FROM adr3
        WHERE addrnumber = @lv_addrnumber
        AND persnumber = @space
        AND flgdefault = @abap_true
        INTO ( @lv_fax_old,
        @lv_fax_consnumber ).

        IF sy-subrc = 0.
          lv_fax_task = gc_task_update.
        ENDIF.
      ENDIF.

      IF lv_fax_task = gc_task_insert OR lv_fax_old <> cs_cliente-fax.

        APPEND INITIAL LINE TO cs_address-data-communication-fax-fax ASSIGNING <fs_fax>.

        <fs_fax>-contact-task = lv_fax_task.
        <fs_fax>-contact-data-fax = cs_cliente-fax.
        <fs_fax>-contact-datax-fax = abap_true.

        IF lv_fax_task = gc_task_update.

          <fs_fax>-contact-data-consnumber = lv_fax_consnumber.
          <fs_fax>-contact-datax-consnumber = abap_true.

        ELSE.

          <fs_fax>-contact-data-std_no = abap_true.
          <fs_fax>-contact-datax-std_no = abap_true.

        ENDIF.
      ENDIF.
    ENDIF.

    IF cs_cliente-mail IS NOT INITIAL.

      lv_smtp_task = gc_task_insert.

      IF lv_addrnumber IS NOT INITIAL.

        SELECT SINGLE smtp_addr,
        consnumber
        FROM adr6
        WHERE addrnumber = @lv_addrnumber
        AND persnumber = @space
        AND flgdefault = @abap_true
        INTO ( @lv_smtp_old,
        @lv_smtp_consnumber ).

        IF sy-subrc = 0.
          lv_smtp_task = gc_task_update.
        ENDIF.

      ENDIF.

      IF lv_smtp_task = gc_task_insert OR lv_smtp_old <> cs_cliente-mail.

        APPEND INITIAL LINE TO cs_address-data-communication-smtp-smtp ASSIGNING <fs_smtp>.

        <fs_smtp>-contact-task = lv_smtp_task.
        <fs_smtp>-contact-data-e_mail =  cs_cliente-mail.
        <fs_smtp>-contact-datax-e_mail = abap_true.

        IF lv_smtp_task = gc_task_update.

          <fs_smtp>-contact-data-consnumber = lv_smtp_consnumber.
          <fs_smtp>-contact-datax-consnumber = abap_true.

        ELSE.
          <fs_smtp>-contact-data-std_no = abap_true.
          <fs_smtp>-contact-datax-std_no = abap_true.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD map_company_data.

    FIELD-SYMBOLS:
    <fs_company> TYPE cmds_ei_company.

    DATA:
          lv_akont TYPE akont.

    CHECK cs_cliente-bukrs IS NOT INITIAL.

    cs_data-customer-company_data-current_state = abap_true.

    APPEND INITIAL LINE TO cs_data-customer-company_data-company ASSIGNING <fs_company>.

    <fs_company>-task = cs_context-company_task.

    " Clave de sociedad
    <fs_company>-data_key-bukrs = cs_cliente-bukrs.

    lv_akont = cs_cliente-akont.

    IF lv_akont IS NOT INITIAL.

      lv_akont = |{ lv_akont ALPHA = IN }|.

      <fs_company>-data-akont  = lv_akont.
      <fs_company>-datax-akont = abap_true.

    ENDIF.

    <fs_company>-data-fdgrv = cs_cliente-fdgrv.
    <fs_company>-datax-fdgrv = abap_true.
    <fs_company>-data-altkn = cs_cliente-altkn.
    <fs_company>-datax-altkn = abap_true.
    <fs_company>-data-zterm = cs_cliente-zterm.
    <fs_company>-datax-zterm = abap_true.

    IF cs_context-company_task = gc_task_insert.

      " Nueva sociedad: únicamente informamos bloqueo si viene solicitado.
      IF cs_cliente-zahls IS NOT INITIAL.

        <fs_company>-data-zahls = cs_cliente-zahls.
        <fs_company>-datax-zahls = abap_true.
      ENDIF.

    ELSEIF cs_context-company_task = gc_task_update.

      " Sociedad existente: recuperar bloqueo actual.
      SELECT SINGLE zahls
      FROM knb1
      WHERE kunnr = @cs_context-customer
      AND bukrs = @cs_cliente-bukrs
      INTO @DATA(lv_zahls_actual).

      IF sy-subrc = 0.

        " Solo modificamos ZAHLS si la solicitud trae explícitamente un valor.
        "
        " Si viene vacío, NO enviamos DATAX y por tanto CL_MD_BP_MAINTAIN conserva el bloqueo existente.
        IF cs_cliente-zahls IS NOT INITIAL AND cs_cliente-zahls <> lv_zahls_actual.

          <fs_company>-data-zahls = cs_cliente-zahls.
          <fs_company>-datax-zahls = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

    <fs_company>-data-zwels = cs_cliente-zwels.
    <fs_company>-datax-zwels = abap_true.
    <fs_company>-data-busab = cs_cliente-busab.
    <fs_company>-datax-busab = abap_true.

    IF cs_cliente-zzncftc IS NOT INITIAL.

      <fs_company>-data-zzncftc = cs_cliente-zzncftc.
      <fs_company>-datax-zzncftc = abap_true.
    ENDIF.

    " Datos de reclamaciones
    map_dunning_data(
   CHANGING
     cs_cliente = cs_cliente
     cs_context = cs_context
     cs_company = <fs_company> ).

    " Retenciones
    map_withholding_tax(
   CHANGING
     cs_cliente = cs_cliente
     cs_context = cs_context
     cs_company = <fs_company> ).

  ENDMETHOD.

  METHOD map_dunning_data.

    FIELD-SYMBOLS:
    <fs_dunning> TYPE cmds_ei_dunning.

    DATA:
      lv_task   TYPE c LENGTH 1,
      lv_exists TYPE abap_bool.

    IF cs_context-customer IS NOT INITIAL.

      SELECT SINGLE @abap_true
      FROM knb5
      WHERE kunnr = @cs_context-customer
      AND bukrs = @cs_cliente-bukrs
      AND maber = @space
      INTO @lv_exists.
    ENDIF.

    IF lv_exists = abap_false AND cs_cliente-mahna IS INITIAL
   AND cs_cliente-mansp IS INITIAL AND cs_cliente-knrma IS INITIAL.

      RETURN.
    ENDIF.

    lv_task = COND #(
    WHEN lv_exists = abap_true
    THEN gc_task_update
    ELSE gc_task_insert ).

    APPEND INITIAL LINE TO cs_company-dunning-dunning ASSIGNING <fs_dunning>.

    <fs_dunning>-task = lv_task.
    <fs_dunning>-data_key-maber = space.
    <fs_dunning>-data-mahna = cs_cliente-mahna.
    <fs_dunning>-datax-mahna = abap_true.
    <fs_dunning>-data-mansp = cs_cliente-mansp.
    <fs_dunning>-datax-mansp = abap_true.
    <fs_dunning>-data-knrma = cs_cliente-knrma.
    <fs_dunning>-datax-knrma = abap_true.

  ENDMETHOD.

  METHOD map_withholding_tax.

    FIELD-SYMBOLS:
    <fs_wtax> TYPE cmds_ei_wtax_type.

    DATA:
      lv_task   TYPE c LENGTH 1,
      lv_exists TYPE abap_bool.

    " Sin tipo de retención no tenemos clave de registro KNBW.
    CHECK cs_cliente-witht IS NOT INITIAL.

    " Comprobar si ya existe esta retención
    IF cs_context-customer IS NOT INITIAL.

      SELECT SINGLE @abap_true
      FROM knbw
      WHERE kunnr = @cs_context-customer
      AND bukrs = @cs_cliente-bukrs
      AND witht = @cs_cliente-witht
      INTO @lv_exists.

    ENDIF.

    lv_task = COND #(
    WHEN lv_exists = abap_true
    THEN gc_task_update
    ELSE gc_task_insert ).

    APPEND INITIAL LINE TO cs_company-wtax_type-wtax_type ASSIGNING <fs_wtax>.

    <fs_wtax>-task = lv_task.

    " Tipo de retención
    <fs_wtax>-data_key-witht = cs_cliente-witht.
    <fs_wtax>-data-wt_withcd = cs_cliente-wt_withcd.
    <fs_wtax>-datax-wt_withcd = abap_true.
    <fs_wtax>-data-wt_agent = cs_cliente-wt_agent.
    <fs_wtax>-datax-wt_agent = abap_true.
    IF cs_cliente-wt_agtdf IS NOT INITIAL.

      <fs_wtax>-data-wt_agtdf = cs_cliente-wt_agtdf.
      <fs_wtax>-datax-wt_agtdf = abap_true.
    ENDIF.
    IF cs_cliente-wt_agtdt IS NOT INITIAL.

      <fs_wtax>-data-wt_agtdt = cs_cliente-wt_agtdt.
      <fs_wtax>-datax-wt_agtdt = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD call_api.

    DATA:
          lt_data TYPE cvis_ei_extern_t.

    APPEND is_data TO lt_data.

    cl_md_bp_maintain=>maintain(
   EXPORTING
     i_data   = lt_data
   IMPORTING
     e_return = rt_return ).

  ENDMETHOD.

  METHOD evaluate_return.

    DATA:
      lv_text       TYPE string,
      lv_object_key TYPE string.

    " Por defecto consideramos correcto el procesamiento.
    " Solo E / A / X convierten el resultado en error.
    rs_result-success = abap_true.

    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<fs_return>).
      IF rs_result-partner IS INITIAL AND <fs_return>-object_key IS NOT INITIAL.

        lv_object_key = CONV string( <fs_return>-object_key ).

        CONDENSE lv_object_key NO-GAPS.
        IF strlen( lv_object_key ) <= 10.

          rs_result-partner = |{ lv_object_key ALPHA = IN }|.
        ENDIF.

      ENDIF.
      LOOP AT <fs_return>-object_msg
     ASSIGNING FIELD-SYMBOL(<fs_message>)
     WHERE type = 'E'
     OR type = 'A'
     OR type = 'X'.

        rs_result-success = abap_false.

        CLEAR lv_text.

        MESSAGE ID <fs_message>-id
       TYPE 'S'
       NUMBER <fs_message>-number
       WITH <fs_message>-message_v1
       <fs_message>-message_v2
       <fs_message>-message_v3
       <fs_message>-message_v4
       INTO lv_text.

        IF rs_result-message IS INITIAL.

          rs_result-message = lv_text.
        ELSE.

          rs_result-message = |{ rs_result-message } / { lv_text }|.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    IF rs_result-success = abap_false AND rs_result-message IS INITIAL.

      rs_result-message = 'Error al mantener el Business Partner/Customer'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
