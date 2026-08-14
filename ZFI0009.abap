*&---------------------------------------------------------------------*
*& Modulpool         ZFI0009
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
INCLUDE ZFI0009TOP                              .    " global Data

INCLUDE ZFI0009PBO.

INCLUDE ZFI0009PAI.

INCLUDE ZFI0009FRM.
INCLUDE ZFI0009CLS.

*&---------------------------------------------------------------------*
*& Include ZFI0009TOP                                        Modulpool        ZFI0002
*&
*&---------------------------------------------------------------------*

PROGRAM  ZFI0009.

TABLES: ZFITPROV,
        LFA1,
        LFB1,
        T001K,
        T001W,
        T059Z,
        T001,
        ZFIT_FISCAL_FI,
        ZFITPROVNAV,
        ZFITPROVH,
        ZFIEPROV,
        T020,
        t042z,
        RF02D,                        " Dynpro/Arbeitsfelder Debitor
         *RF02D.

CONTROLS T_1 TYPE TABLEVIEW USING SCREEN 9001.

CONTROLS T_2 TYPE TABLEVIEW USING SCREEN 9002.

DATA LT_T_1 LIKE ZFIEPROV OCCURS 0 WITH HEADER LINE.

DATA LT_T_2 LIKE ZFITPROVH OCCURS 0 WITH HEADER LINE.

DATA LV_COD_INT LIKE LT_T_1-COD_INT.
controls     TCTRL_ZAHLWEGE          TYPE TABLEVIEW USING SCREEN 1215.
DATA:    OK-CODE(5)     TYPE C.
TYPES:   BEGIN OF FCODE,
             OKCODE(5) TYPE C,
         END OF FCODE.
DATA:    ZWCNT(2)       TYPE P VALUE 0,     " Zaehler  (Zahlwege)
         ZAV_READ(1)    TYPE C.
TYPES:   TABLE_OF_FUNCTION_CODES TYPE STANDARD TABLE OF FCODE.
DATA:    EXCTAB TYPE TABLE_OF_FUNCTION_CODES.

DATA: BEGIN OF TABSTRIP_EXTAB OCCURS 0,
          OKCODE LIKE SY-TCODE,
      END OF TABSTRIP_EXTAB.
* Datendeklarationen für Betriebestamm
DATA:       kred_call,                "Kreditor bereits gepuffert
            debi_call,                "Debitor bereits gepuffert
            debi_ex_kred,             "Debitor wird aus Kreditor
                                      "aktualisiert
            kred_ex_debi,             "Kreditor wird aus Debitor
                                      "aktualisier
            nriv_externind LIKE nriv-externind, "Externe Nummernvergabe
            s_retdeb_type  LIKE rf02d-selkz,
            s_retkre_type  LIKE rf02d-selkz,
            debi_ex_kred_no_save.
DATA:    BEGIN OF E042Z OCCURS 10,
           ZLSCH        LIKE T042Z-ZLSCH,   " Zahlweg
           TEXT1        LIKE T042Z-TEXT1,   " Bedeutung des Zahlwegs
           XSELK(1)     TYPE C,             " KZ: X=Zahlweg ausgewaehlt
         END OF E042Z.
DATA:    BEGIN OF A042Z OCCURS 10,
           ZLSCH        LIKE T042Z-ZLSCH,   " Zahlweg
           TEXT1        LIKE T042Z-TEXT1,   " Bedeutung des Zahlwegs
           XSELK(1)     TYPE C,             " KZ: X=Zahlweg ausgewaehlt
         END OF A042Z.
DATA:    REFE1(8)       TYPE P,
         REFE2(8)       TYPE P,
         TFILL          TYPE I,
         INDEX          TYPE I.
DATA:    LOOPC          TYPE I,             " Hilfsfeld Listbildblättern
         SAVE_LOOPC(2)  TYPE P,             " Hilfsfeld Listbildblättern
         LFLAG(1)       TYPE C,             " Flag 'Zeilenselektion'
         LINDEX         TYPE I,
         XMERKEN(1)     TYPE C,
         SAVE_ZWELS     LIKE LFB1-ZWELS.
* Table control
DATA:

  LV_LIN TYPE I,
  LV_INI TYPE I,
  LV_INI_H TYPE I,
  LV_CUR TYPE I,
  LV_CURSOR TYPE I,
  LV_CURSOR_FIELD TYPE NAME_KOMP,
  LV_SUP_D TYPE I.

DATA LV_UCOMM TYPE SY-UCOMM.

* Verificaciones / Confirmaciones
DATA:

  LV_VERIF TYPE I,
  LV_CONFIRM TYPE I,
  LV_VERIF_T(80).

*       Batchinputdata of single transaction
DATA:   BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction

** Inicio CGR 11/02/2010
   DATA: lv_first(1).

   clear: lv_first.
** Fin CGR CGR 11/02/2010

DATA LV_MIGR TYPE I.  " 1: MIGRACION FICH 1 / 2:FICH 2

DATA LV_FICH LIKE RLGRAP-FILENAME.

DATA LT_FICH LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.

DATA: LV_TOTAC TYPE I,
      LV_TOT TYPE I,
      X055_COUNT(3)  TYPE P.
DATA: CRS_FIELD      like rfcu3-fname,
      CRS_LINE       LIKE SY-STEPL.
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

DATA: GV_LIFNR LIKE LFA1-LIFNR,
      CHAR1(1)       TYPE C.
FIELD-SYMBOLS: <F1>, <F2>, <NVAL>, <OVAL>.
DATA:    BEGIN OF SORTTAB OCCURS 10,
           ARG(1)       TYPE C,             " Sortierfeld
         END OF SORTTAB.

RANGES: r_bukrs_aut for t001-bukrs.
RANGES: r_bukrs_naut for t001-bukrs.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS so_bukrs FOR ZFIEPROV-bukrs NO INTERVALS OBLIGATORY.
SELECT-OPTIONS so_fecha FOR ZFIEPROV-fecha.
PARAMETERS so_estad type ZZEESTALTA04.
SELECTION-SCREEN END OF BLOCK b1.
**************************** START-OF-SELECTION *************************


START-OF-SELECTION.


      perform llamar_dynpro.

*----------------------------------------------------------------------*
***INCLUDE ZFI0002PBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.

  SET PF-STATUS '9001'.
  SET TITLEBAR '9001'.

ENDMODULE.                 " STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_PROCESO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_proceso OUTPUT.

  IF lv_ini = 0.

    IF syst-tcode = 'ZFI04_'.

      lv_migr = 1.

    ENDIF.

    IF lv_migr IS INITIAL.
      PERFORM acceso_tablas.
      PERFORM carga_fun_dyn.
    ELSE.
      PERFORM acceso_tablas_m.
      PERFORM carga_migr_dyn.
    ENDIF.

    lv_cursor = 1.
    lv_ini = 1.

  ENDIF.

  IF NOT lv_sup_d IS INITIAL.

    SUPPRESS DIALOG.
    CLEAR lv_sup_d.

  ENDIF.

  DESCRIBE TABLE lt_t_1 LINES lv_lin.

  t_1-lines = lv_lin.

  lv_cur = 1.

  IF NOT lv_cursor IS INITIAL AND lv_cursor_field IS INITIAL.

    CLEAR lv_cursor.
    SET CURSOR 1 1.

  ENDIF.

  IF NOT lv_cursor IS INITIAL AND NOT lv_cursor_field IS INITIAL.

    SET CURSOR FIELD lv_cursor_field LINE lv_verif.
    CLEAR lv_cursor.
    CLEAR lv_cursor_field.

  ENDIF.

ENDMODULE.                 " INIT_PROCESO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_value OUTPUT.

  PERFORM derivar_akont CHANGING zfieprov.

  IF t_1-current_line GT 0.
    MODIFY lt_t_1 FROM zfieprov INDEX t_1-current_line
    TRANSPORTING akont.
  ENDIF.
  LOOP AT SCREEN.
    IF screen-name = 'ZFIEPROV-AKONT'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  IF zfieprov-estado IS INITIAL .
    LOOP AT SCREEN.
      screen-input    = '0'.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.



ENDMODULE.                 " SET_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9002 OUTPUT.

  SET PF-STATUS '9002'.
  SET TITLEBAR '9002'.

ENDMODULE.                 " STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_PROCESO_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_proceso_9002 OUTPUT.

  IF lv_ini_h = 0.

    PERFORM acceso_tablas_hist.

    DESCRIBE TABLE lt_t_2 LINES lv_lin.

    t_2-lines = lv_lin.

    lv_cur = 1.

    lv_ini_h = 1.

  ENDIF.

ENDMODULE.                 " INIT_PROCESO_9002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_VALUE_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_value_9002 OUTPUT.

  LOOP AT SCREEN.

    screen-input    = '0'.
    MODIFY SCREEN.

  ENDLOOP.

ENDMODULE.                 " SET_VALUE_9002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  t042z_lesen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE t042z_lesen OUTPUT.

  DESCRIBE TABLE a042z LINES refe1.
  DESCRIBE TABLE e042z LINES refe2.

  IF  refe1 = 0
  AND refe2 = 0.
    REFRESH: a042z, e042z.
    SELECT * FROM t042z WHERE land1 = 'ES'
                           AND   zlsch NE space.
      IF t042z-xeinz = 'X'.
        CLEAR e042z.
        e042z-zlsch = t042z-zlsch.
        e042z-text1 = t042z-text1.
        APPEND e042z.

*------- Zahlweg fuer Zahlungsausgaenge --------------------------------
      ELSE.
        CLEAR a042z.
        a042z-zlsch = t042z-zlsch.
        a042z-text1 = t042z-text1.
        APPEND a042z.
      ENDIF.
    ENDSELECT.
  ENDIF.
  LOOP AT a042z.
    CLEAR a042z-xselk.
    IF zfieprov-zwels CS a042z-zlsch.
      a042z-xselk = 'X'.
      sorttab-arg = a042z-zlsch.       " uh 08.09.98
      APPEND sorttab.                  " uh 08.09.98
    ELSE.
      a042z-xselk = space.
    ENDIF.
    MODIFY a042z.
  ENDLOOP.
  LOOP AT e042z.
    CLEAR e042z-xselk.
    IF zfieprov-zwels CS e042z-zlsch.
      e042z-xselk = 'X'.
      sorttab-arg = e042z-zlsch.       " uh 08.09.98
      APPEND sorttab.                  " uh 08.09.98
    ELSE.
      e042z-xselk = space.
    ENDIF.
    MODIFY e042z.
  ENDLOOP.

*------ Ausgangszustand bei den Zahlwegen merken -----------------------
  IF  t020-aktyp <> 'A'
  AND xmerken  = 'X'.
    save_zwels = zfieprov-zwels.
    CLEAR xmerken.
  ENDIF.
ENDMODULE.                 " t042z_lesen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  zahlweg_anzeigen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zahlweg_anzeigen OUTPUT.
* Anzahl der dargestellten Zeilen des Table Controls sichern.
  loopc = sy-loopc.                    " uh, 07.09.98

*-----Zeilenzähler für Blätterfunktion initialisieren ----------------*
  lindex = index + sy-stepl - 1.       " uh, 07.09.98

*------- Zahlweg fuer Zahlungsausgaenge anzeigen -----------------------
* READ TABLE a042z INDEX sy-stepl.       " uh, 07.09.98
  READ TABLE a042z INDEX lindex.       " uh, 07.09.98

  IF sy-subrc NE 0.
    LOOP AT SCREEN.
      IF screen-name = 'RF02D-XASEL'
      OR screen-name = 'RF02D-AZSCH'
      OR screen-name = 'RF02D-AZTXT'.
        screen-input     = 0.
        screen-output    = 0.
        screen-invisible = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    rf02d-xasel = a042z-xselk.
    rf02d-azsch = a042z-zlsch.
    rf02d-aztxt = a042z-text1.
  ENDIF.

*------- Zahlweg fuer Zahlungseingaenge anzeigen -----------------------
*  READ TABLE e042z INDEX sy-stepl.            " uh, 07.09.98
  READ TABLE e042z INDEX lindex.       " uh, 07.09.98
  IF sy-subrc NE 0.
    LOOP AT SCREEN.
      IF screen-name = 'RF02D-XESEL'
      OR screen-name = 'RF02D-EZSCH'
      OR screen-name = 'RF02D-EZTXT'.
        screen-input     = 0.
        screen-output    = 0.
        screen-invisible = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    rf02d-xesel = e042z-xselk.
    rf02d-ezsch = e042z-zlsch.
    rf02d-eztxt = e042z-text1.
  ENDIF.
ENDMODULE.                 " zahlweg_anzeigen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TCTRL_ZAHLWEGE_INIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tctrl_zahlwege_init OUTPUT.
* Zeilenanzahl der Darzustellenden Tabelle in Table Control eintragen.
  DESCRIBE TABLE a042z LINES refe1.
  DESCRIBE TABLE e042z LINES refe2.

  IF refe1 GE refe2.
    tfill = refe1.
  ELSE.
    tfill = refe2.
  ENDIF.

  tctrl_zahlwege-lines = tfill.

* Oberste dargestellte Tabellenzeile festlegen.
  IF index LE 0.
    index = 1.
  ENDIF.

  tctrl_zahlwege-top_line = index.
ENDMODULE.                 " TCTRL_ZAHLWEGE_INIT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PFSTATUS_D1215  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pfstatus_d1215 OUTPUT.
  CLEAR ok-code.
  IF t020-aktyp = 'A'.
*   set pf-status 'W'.                                     "MDT 28.12.98
    PERFORM set_pf_status TABLES exctab"MDT 28.12.98
                          USING 'W' ' '.                   "MDT 28.12.98
  ELSE.
*   set pf-status '215Z'.                                  "MDT 28.12.98
    PERFORM set_pf_status TABLES exctab"MDT 28.12.98
                          USING '215Z' ' '.                "MDT 28.12.98
  ENDIF.
ENDMODULE.                 " PFSTATUS_D1215  OUTPUT

*----------------------------------------------------------------------*
***INCLUDE ZFI0002PAI .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module USER_COMMAND_9001 input.

  LV_UCOMM = SY-UCOMM.

  CASE LV_UCOMM.

    WHEN 'STRT'.  " Aprobar

      PERFORM TR_APROB.
      PERFORM TR_DESMARCAR.

    WHEN 'RECH'.   " Rechazar

      PERFORM TR_RECH.
      PERFORM TR_DESMARCAR.

    WHEN 'PRE'.   " Grabar Pre

      PERFORM TR_GRABAR.
      PERFORM TR_DESMARCAR.

    WHEN 'MALL'.  " Marcar

      PERFORM TR_MARCAR.

    WHEN 'RALL'.  " Desmarcar

      PERFORM TR_DESMARCAR.

    WHEN 'CONS'.  " Consulta

      PERFORM TR_CONS.

    WHEN 'REFR'.

      PERFORM TR_REFR.

    WHEN 'HIST'.  " Historial

      PERFORM TR_HIST.
      PERFORM TR_DESMARCAR.

    WHEN 'ACRE'.  " Lista acreedores

      PERFORM TR_ACRE.

    WHEN 'MIG1'.  " Migración FICH 1 / Ctrl-F1

      PERFORM TR_MIGR_1.

    WHEN 'MIG2'.  " Migración FICH 2 / Ctrl-F2

      PERFORM TR_MIGR_2.

   ENDCASE.

endmodule.                 " USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module EXIT_COMMAND_9001 input.

  LEAVE PROGRAM.

endmodule.                 " EXIT_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*&      Module  TRATAR_DATOS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module TRATAR_DATOS input.

  MODIFY LT_T_1 FROM ZFIEPROV INDEX T_1-CURRENT_LINE.

endmodule.                 " TRATAR_DATOS  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZWELS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module ZWELS input.
 PERFORM HELP_KNB1_ZWELS.
endmodule.                 " ZWELS  INPUT
*&---------------------------------------------------------------------*
*&      Module  zahlweg_markieren  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module zahlweg_markieren input.
  CHECK T020-AKTYP NE 'A'.
  CHECK OK-CODE = 'MARK'.

  GET CURSOR LINE CRS_LINE FIELD CRS_FIELD.

  IF CRS_LINE = SY-STEPL.
    IF  CRS_FIELD(11) NE 'RF02D-XASEL'
    AND CRS_FIELD(11) NE 'RF02D-XESEL'.
      CLEAR: OK-CODE, CRS_LINE, CRS_FIELD.
    ENDIF.
    ASSIGN (CRS_FIELD) TO <F1>.
    IF <F1> NE 'X'.
      <F1> = 'X'.
    ELSE.
      <F1> = SPACE.
    ENDIF.
  ENDIF.
endmodule.                 " zahlweg_markieren  INPUT
*&---------------------------------------------------------------------*
*&      Module  zahlweg_update  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module zahlweg_update input.
  CHECK T020-AKTYP NE 'A'.

*  IF sy-stepl = 1.                                      " uh, 07.09.98
*    REFRESH sorttab.                                    " uh, 07.09.98
*  ENDIF.                                                " uh, 07.09.98

* Index der aktuellen Zeile:
  LINDEX = INDEX + SY-STEPL - 1.       " uh, 07.09.98

  IF RF02D-AZSCH NE SPACE.
    A042Z-XSELK = RF02D-XASEL.
    A042Z-ZLSCH = RF02D-AZSCH.
    A042Z-TEXT1 = RF02D-AZTXT.
*   MODIFY a042z INDEX sy-stepl.                         " uh, 07.09.98
    MODIFY A042Z INDEX LINDEX.         " uh, 07.09.98
    IF A042Z-XSELK = 'X'.
      SORTTAB-ARG = A042Z-ZLSCH.
      APPEND SORTTAB.
    ELSE.                              " uh, 07.09.98
      DELETE SORTTAB WHERE ARG = A042Z-ZLSCH.            " uh, 07.09.98
    ENDIF.
  ENDIF.

  IF RF02D-EZSCH NE SPACE.
    E042Z-XSELK = RF02D-XESEL.
    E042Z-ZLSCH = RF02D-EZSCH.
    E042Z-TEXT1 = RF02D-EZTXT.
*   MODIFY e042z INDEX sy-stepl.                         " uh, 07.09.98
    MODIFY E042Z INDEX LINDEX.         " uh, 07.09.98
    IF E042Z-XSELK = 'X'.
      SORTTAB-ARG = E042Z-ZLSCH.
      APPEND SORTTAB.
    ELSE.                              " uh, 07.09.98
      DELETE SORTTAB WHERE ARG = E042Z-ZLSCH.           " uh, 07.09.98
    ENDIF.
  ENDIF.
endmodule.                 " zahlweg_update  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZAHLWEG_LEISTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module ZAHLWEG_LEISTE input.
  CHECK T020-AKTYP NE 'A'.

  CLEAR ZFIEPROV-ZWELS.
  ZWCNT = 0.
  SORT SORTTAB DESCENDING.
  DELETE ADJACENT DUPLICATES FROM SORTTAB.       " uh, 07.09.98
  LOOP AT SORTTAB.
    SHIFT ZFIEPROV-ZWELS RIGHT.
    ZFIEPROV-ZWELS(1) = SORTTAB-ARG.
    ZWCNT         = ZWCNT + 1.
  ENDLOOP.

*------- Wurden mehr als 10 Zahlwege angekreuzt ? ----------------------
  IF ZWCNT > 10.
    SET SCREEN SY-DYNNR.
    LEAVE SCREEN.
  ELSE.
    ZWCNT = 0.
  ENDIF.

  IF OK-CODE = 'MARK'.
    CLEAR: OK-CODE.
    SET SCREEN SY-DYNNR.
    LEAVE SCREEN.
  ENDIF.
endmodule.                 " ZAHLWEG_LEISTE  INPUT
*&---------------------------------------------------------------------*
*&      Module  TCTRL_ZAHLWEGE_BLAETTERN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module TCTRL_ZAHLWEGE_BLAETTERN input.
*Beim Blättern nach Markieren kommt kein ok-code, aber index <> top_line
  IF INDEX NE TCTRL_ZAHLWEGE-TOP_LINE AND OK-CODE IS INITIAL.
    INDEX = TCTRL_ZAHLWEGE-TOP_LINE.
    SET SCREEN SY-DYNNR.
    LEAVE SCREEN.
  ENDIF.

  INDEX = TCTRL_ZAHLWEGE-TOP_LINE.

  IF OK-CODE(2) = 'P-'
  OR OK-CODE(2) = 'P+'.
    PERFORM FENSTER_BLAETTERN.
  ENDIF.
endmodule.                 " TCTRL_ZAHLWEGE_BLAETTERN  INPUT

**----------------------------------------------------------------------*
***INCLUDE ZFI0002FRM.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  TR_APROB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_aprob .

  lv_totac = 0.
  lv_tot = 0.

  lv_verif = 0.

  PERFORM verif.

  IF NOT lv_verif IS INITIAL.   " Linea error

    PERFORM crear_message.

  ELSE.

    PERFORM confirm.

    IF NOT lv_confirm IS INITIAL.   " Confirmado envío

*** INICIO MODIFICACIÓN EMG 12/05/2009
      REFRESH lt_log.
*** FIN MODIFICACIÓN EMG 12/05/2009

      PERFORM tr_aprob_conf.

      PERFORM tr_aprob_conf_modif.
** inicio CGR 10/02/2009
      LOOP AT lt_log.
        IF lt_log-msgv1 = 'LFB1-QLAND'.
          lt_log-msgv1 = 'País de retención'.
        ENDIF.
        MODIFY lt_log.
      ENDLOOP.
** fin CGR 10/02/2009
*** INICIO MODIFICACIÓN EMG 12/05/2009
      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = lt_log.
*** FIN MODIFICACIÓN EMG 12/05/2009

      MESSAGE s025(zfi01) WITH lv_totac lv_tot.

    ENDIF.

  ENDIF.

ENDFORM.                    " TR_APROB
*&---------------------------------------------------------------------*
*&      Form  TR_RECH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_rech .

  DATA: lv_index TYPE i,
        lv_lines TYPE i,
        lv_mes   TYPE i.

  lv_verif = 0.

  PERFORM verif_rech.

  IF NOT lv_verif IS INITIAL.   " Linea error

    PERFORM crear_message.

  ELSE.

    PERFORM ini_cod_int.

    LOOP AT lt_t_1 WHERE sele = 'X'.

      PERFORM tr_rech_sol USING sy-tabix.

      lv_mes = 1.

    ENDLOOP.

    PERFORM fin_cod_int.

    DESCRIBE TABLE lt_t_1 LINES lv_lines.

*   LV_INI = 0.

    IF NOT lv_mes IS INITIAL.
      MESSAGE s023(zfi01).
    ENDIF.
  ENDIF.

ENDFORM.                    " TR_RECH
*&---------------------------------------------------------------------*
*&      Form  TR_GRABAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_grabar .

  DATA: lv_index TYPE i,
        lv_lines TYPE i,
        lv_mes   TYPE i.

  PERFORM ini_cod_int.

  LOOP AT lt_t_1 WHERE sele = 'X'.

    PERFORM tr_grabar_sol USING sy-tabix.

    lv_mes = 1.

  ENDLOOP.

  PERFORM fin_cod_int.

  DESCRIBE TABLE lt_t_1 LINES lv_lines.

  IF lv_lines IS INITIAL.

*    LV_INI = 0.

  ENDIF.

  IF NOT lv_mes IS INITIAL.
    MESSAGE s022(zfi01).
  ENDIF.


ENDFORM.                    " TR_GRABAR
*&---------------------------------------------------------------------*
*&      Form  TR_MARCAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_marcar .

  lt_t_1-sele = 'X'.

  MODIFY lt_t_1 FROM lt_t_1
       TRANSPORTING sele WHERE sele <> 'X'.

ENDFORM.                    " TR_MARCAR
*&---------------------------------------------------------------------*
*&      Form  TR_DESMARCAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_desmarcar .

  CLEAR lt_t_1-sele.
  lv_cursor = 1.

  MODIFY lt_t_1 FROM lt_t_1
       TRANSPORTING sele WHERE sele = 'X'.

ENDFORM.                    " TR_DESMARCAR
*&---------------------------------------------------------------------*
*&      Form  VERIF
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM verif .

  TYPES: BEGIN OF ty_lfa1_lfb1,
           lifnr TYPE lifnr.
  TYPES: END OF ty_lfa1_lfb1.

  DATA: ls_lfa1_lfb1 TYPE ty_lfa1_lfb1.

  DATA: BEGIN OF lt_bukrs OCCURS 0,
          bukrs LIKE zfitprov-bukrs,
        END OF lt_bukrs.

  DATA lv_fail TYPE i.

  LOOP AT lt_t_1 WHERE NOT sele IS INITIAL.

    IF lt_t_1-bukrs IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-BUKRS'.
      PERFORM texto_error USING 'BUKRS' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

    IF lt_t_1-bu_group IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-BU_GROUP'.
      PERFORM texto_error USING 'BU_GROUP' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

    IF lt_t_1-cif IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-CIF'.
      PERFORM texto_error USING 'STCD1' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.


    IF lt_t_1-pais IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-PAIS'.
      PERFORM texto_error USING 'LAND1' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

* Mod validación RNC - AS
    DATA: lc_error TYPE char1,
          lc_text  TYPE char100,
          wa_lfa1  LIKE lfa1.
    CLEAR: lc_error, lc_text, wa_lfa1.
    IF lt_t_1-cif IS NOT INITIAL AND lt_t_1-pais EQ 'DO'.
      IF NOT lt_t_1-partner IS INITIAL.
        SELECT SINGLE *
                 FROM lfa1
                WHERE stcd1 = lt_t_1-cif.
        IF sy-subrc EQ 0.
          SELECT SINGLE *
                   FROM lfa1
                  WHERE stcd1 = lt_t_1-cif AND
                        lifnr = lt_t_1-partner.
          IF sy-subrc NE 0.
            lc_text  = 'El RNC está duplicado, verifíquelo'.
            lc_error = 'X'.
            lv_verif = sy-tabix.
            lv_cursor_field = 'ZFIEPROV-CIF'.
            PERFORM texto_error USING 'STCD1' lc_text CHANGING lv_verif_t.
            EXIT.
          ENDIF.
        ELSE.
          CALL FUNCTION 'ZVALIDACION_RNC'
            EXPORTING
              fisica       = lt_t_1-stkzn
              nif_g        = lt_t_1-cif
            IMPORTING
              nif_correcto = lc_error
              text2        = lc_text.
          IF lc_error EQ 'X'.
            lv_verif = sy-tabix.
            lv_cursor_field = 'ZFIEPROV-CIF'.
            PERFORM texto_error USING 'STCD1' lc_text CHANGING lv_verif_t.
            EXIT.
          ENDIF.
        ENDIF.
      ELSE.
        SELECT SINGLE *
         FROM lfa1
        WHERE stcd1 = lt_t_1-cif.
        IF sy-subrc = 0.
          SELECT SINGLE *
                   FROM lfa1
                  WHERE stcd1 = lt_t_1-cif AND
                        lifnr = lt_t_1-partner.
          IF sy-subrc NE 0.
            lc_text  = 'El RNC está duplicado, verifíquelo'.
            lc_error = 'X'.
            lv_verif = sy-tabix.
            lv_cursor_field = 'ZFIEPROV-CIF'.
            PERFORM texto_error USING 'STCD1' lc_text CHANGING lv_verif_t.
            EXIT.
          ENDIF.
        ELSE.
          CALL FUNCTION 'ZVALIDACION_RNC'
            EXPORTING
              fisica       = lt_t_1-stkzn
              nif_g        = lt_t_1-cif
            IMPORTING
              nif_correcto = lc_error
              text2        = lc_text.
          IF lc_error EQ 'X'.
            lv_verif = sy-tabix.
            lv_cursor_field = 'ZFIEPROV-CIF'.
            PERFORM texto_error USING 'STCD1' lc_text CHANGING lv_verif_t.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSEIF lt_t_1-cif IS NOT INITIAL AND lt_t_1-pais EQ 'MX'.
      IF NOT lt_t_1-partner IS INITIAL.
        SELECT SINGLE *
                 FROM lfa1
                WHERE stcd1 = lt_t_1-cif.
        IF sy-subrc EQ 0.
          SELECT SINGLE *
                   FROM lfa1
                  WHERE stcd1 = lt_t_1-cif AND
                        lifnr = lt_t_1-partner.
          IF sy-subrc NE 0.
            lc_text  = 'El RFC está duplicado, verifíquelo'.
            lc_error = 'X'.
            lv_verif = sy-tabix.
            lv_cursor_field = 'ZFIEPROV-CIF'.
            PERFORM texto_error USING 'STCD1' lc_text CHANGING lv_verif_t.
            EXIT.
          ENDIF.
        ELSE.
          IF NOT lt_t_1-stkzn IS INITIAL AND lt_t_1-stcd3 IS INITIAL.
            lc_text  = 'Incoherencia con RFC o CURP, persona física'.
            lc_error = 'X'.
            lv_verif = sy-tabix.
            lv_cursor_field = 'ZFIEPROV-CIF'.
            PERFORM texto_error USING 'STCD3' lc_text CHANGING lv_verif_t.
            EXIT.
          ELSE.
            CALL FUNCTION 'ZVALIDACION_RFC_CURP'
              EXPORTING
                fisica       = lt_t_1-stkzn
                nif_g        = lt_t_1-cif
                nif_p        = lt_t_1-stcd3
              IMPORTING
                nif_correcto = lc_error
                text2        = lc_text.
            IF lc_error EQ 'X'.
              lv_verif = sy-tabix.
              lv_cursor_field = 'ZFIEPROV-CIF'.
              PERFORM texto_error USING 'STCD1' lc_text CHANGING lv_verif_t.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        SELECT SINGLE *
         FROM lfa1
        WHERE stcd1 = lt_t_1-cif.
        IF sy-subrc = 0.

          SELECT SINGLE *
                   FROM lfa1
                  WHERE stcd1 = lt_t_1-cif
              AND lifnr = lt_t_1-partner.

          IF sy-subrc NE 0.
            lc_text  = 'El RFC está duplicado, verifíquelo'.
            lc_error = 'X'.
            lv_verif = sy-tabix.
            lv_cursor_field = 'ZFIEPROV-CIF'.
            PERFORM texto_error USING 'STCD1' lc_text CHANGING lv_verif_t.
            EXIT.
          ENDIF.
        ELSE.
          IF NOT lt_t_1-stkzn IS INITIAL AND lt_t_1-stcd3 IS INITIAL.
            lc_text  = 'Incoherencia RFC o CURP, persona física'.
            lc_error = 'X'.
            lv_verif = sy-tabix.
            lv_cursor_field = 'ZFIEPROV-CIF'.
            PERFORM texto_error USING 'STCD3' lc_text CHANGING lv_verif_t.
            EXIT.
          ELSE.
            CALL FUNCTION 'ZVALIDACION_RFC_CURP'
              EXPORTING
                fisica       = lt_t_1-stkzn
                nif_g        = lt_t_1-cif
                nif_p        = lt_t_1-stcd3
              IMPORTING
                nif_correcto = lc_error
                text2        = lc_text.
            IF lc_error EQ 'X'.
              lv_verif = sy-tabix.
              lv_cursor_field = 'ZFIEPROV-CIF'.
              PERFORM texto_error USING 'STCD1' lc_text CHANGING lv_verif_t.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'TAX_NUMBER_CHECK'
      EXPORTING
        country             = lt_t_1-pais
        natural_person_flag = ' '
        tax_code_1          = lt_t_1-cif
      EXCEPTIONS
        not_valid           = 1
        different_fprcd     = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-CIF'.
      PERFORM texto_error USING 'STCD1' TEXT-022 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

    IF lt_t_1-name1 IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-NAME1'.
      PERFORM texto_error USING 'AD_NAME1' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

    IF lt_t_1-direc IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-DIREC'.
      PERFORM texto_error USING 'AD_STREET' TEXT-001 CHANGING lv_verif_t.
      EXIT.
*Marta: 27.11.2008 - inicio modificación
*La calle no puede ser mayor de 35 caracteres ya que despues se va al LFA1-SPRAS, eso pasa cuando no hay organización de compras
    ELSE.
      DATA: va_num TYPE i.

      SELECT COUNT(*) FROM t024w WHERE werks EQ lt_t_1-bukrs.
      IF sy-subrc EQ 4.
        va_num = strlen( lt_t_1-direc ).
        IF va_num GT 35.
          lv_verif = sy-tabix.
          lv_verif_t = TEXT-002.
          EXIT.
        ENDIF.
      ENDIF.
*Marta: 27.11.2008 - fin modificación

    ENDIF.

    IF lt_t_1-cod_post IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-COD_POST'.
      PERFORM texto_error USING 'AD_PSTCD1' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

    IF lt_t_1-busq IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-BUSQ'.
      PERFORM texto_error USING 'SORTL' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

    IF lt_t_1-poblac IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-POBLAC'.
      PERFORM texto_error USING 'AD_CITY1' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

    IF lt_t_1-region IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-REGION'.
      PERFORM texto_error USING 'REGIO' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.
*** Inicio ERCMM Mod. se comenta estos campos porque no son obligatorios   29.03.2012 16:00:21
*    IF lt_t_1-tel IS INITIAL.
*
*      lv_verif = sy-tabix.
*      lv_cursor_field = 'ZFIEPROV-TEL'.
*      PERFORM texto_error USING 'AD_TLNMBR1' text-001 CHANGING lv_verif_t.
*      EXIT.
*
*    ENDIF.

*    IF lt_t_1-fax IS INITIAL AND lt_t_1-zwels EQ '3'.
*
*      lv_verif = sy-tabix.
*      lv_cursor_field = 'ZFIEPROV-FAX'.
*      PERFORM texto_error USING 'AD_FXNMBR1' text-025 CHANGING lv_verif_t.
*      EXIT.
*
*    ENDIF.
*** Fin ERCMM  se comenta estos campos porque no son obligatorios  29.03.2012 16:00:21
    IF lt_t_1-spras IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-SPRAS'.
      PERFORM texto_error USING 'SPRAS' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

    IF lt_t_1-fdgrv IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-FDGRV'.
      PERFORM texto_error USING 'FDGRV' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

    IF lt_t_1-akont IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-AKONT'.
      PERFORM texto_error USING 'AKONT' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

*   IF LT_T_1-DZSABE_K IS INITIAL.
*
*     LV_VERIF = SY-TABIX.
*     LV_CURSOR_FIELD = 'ZFIEPROV-DZSABE_K'.
*     PERFORM TEXTO_ERROR USING 'DZSABE_K' TEXT-001 CHANGING LV_VERIF_T.
*     EXIT.
*
*   ENDIF.

    IF lt_t_1-zterm IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-ZTERM'.
      PERFORM texto_error USING 'DZTERM' TEXT-001 CHANGING lv_verif_t.
      EXIT.
*** INICIO MODIFICACIÓN EMG 12/05/2009
    ELSE.
      SELECT COUNT(*) FROM t052 UP TO 1 ROWS
                      WHERE zterm EQ lt_t_1-zterm
                        AND koart EQ 'D'.
      IF sy-subrc EQ 0.
        lv_verif = sy-tabix.
        lv_cursor_field = 'ZFIEPROV-ZTERM'.
        MESSAGE ID 'ZMJE' TYPE 'W' NUMBER 400 INTO lv_verif_t.
        EXIT.
      ENDIF.
*** FIN MODIFICACIÓN EMG 12/05/2009
    ENDIF.

    IF lt_t_1-waers IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-WAERS'.
      PERFORM texto_error USING 'WAERS' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

    IF lt_t_1-ekorg IS INITIAL.
*** INICIO MODIFICACIÓN EMG 12/05/2009
**** INICIO MODIFICACIÓN EMG 28/04/2008
*      IF NOT lt_t_1-bukrs IS INITIAL.
*        SELECT COUNT(*) FROM t024w WHERE werks EQ lt_t_1-bukrs.
*        IF sy-subrc EQ 0.
**** FIN MODIFICACIÓN EMG 28/04/2008
*          lv_verif = sy-tabix.
*          lv_cursor_field = 'ZFIEPROV-EKORG'.
*          PERFORM texto_error USING 'EKORG' text-001 CHANGING lv_verif_t.
*          EXIT.
**** INICIO MODIFICACIÓN EMG 28/04/2008
*        ENDIF.
*      ENDIF.
**** FIN MODIFICACIÓN EMG 28/04/2008
      IF NOT lt_t_1-bukrs IS INITIAL.
        SELECT COUNT(*) FROM t024w UP TO 1 ROWS
                        WHERE werks EQ lt_t_1-bukrs
                          AND ekorg NE space.
        IF sy-subrc EQ 0.
          lv_verif = sy-tabix.
          lv_cursor_field = 'ZFIEPROV-EKORG'.
          MESSAGE ID 'ZMJE' TYPE 'W' NUMBER 401 INTO lv_verif_t.
          EXIT.
        ENDIF.
      ENDIF.
    ELSE.
      IF NOT lt_t_1-bukrs IS INITIAL.
        SELECT COUNT(*) FROM t024w UP TO 1 ROWS
                        WHERE werks EQ lt_t_1-bukrs
                          AND ekorg EQ lt_t_1-ekorg.
        IF sy-subrc NE 0.
          lv_verif = sy-tabix.
          lv_cursor_field = 'ZFIEPROV-EKORG'.
          MESSAGE ID 'ZMJE' TYPE 'W' NUMBER 402 INTO lv_verif_t
                  WITH lt_t_1-bukrs lt_t_1-ekorg.
          EXIT.
        ENDIF.
      ENDIF.
*** FIN MODIFICACIÓN EMG 12/05/2009
    ENDIF.

    IF lt_t_1-zwels IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-ZWELS'.
      PERFORM texto_error USING 'DZWELS' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

    IF lt_t_1-rcomp IS INITIAL AND lt_t_1-bu_group EQ 'ZTGR'.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-RCOMP'.
      PERFORM texto_error USING 'VBUND' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

*) Inicio de modificación Edgar Béjar / ebejar 11.09.2020
*) Se quita la validación de campo obligatorio a Clave de Grupo para darle su uso real.
*) Se comentaron las siguientes líneas
*    IF lt_t_1-c_fisc_mx IS INITIAL."comentado para RCD 1.12.2011 aayala
*      SELECT SINGLE * FROM t001 WHERE bukrs = lt_t_1-bukrs.
*      IF t001-land1 = 'MX'.
*        lv_verif = sy-tabix.
*        lv_cursor_field = 'ZFIEPROV-C_FISC_MX'.
*        PERFORM texto_error USING 'KONZS' text-001 CHANGING lv_verif_t.
*        EXIT.
*      ELSEIF t001-land1 = 'DO'.
*        lt_t_1-c_fisc_mx = '00'.
*        MODIFY lt_t_1.
*      ENDIF.
*    ENDIF.
*) Fin de modificación Edgar Béjar / ebejar 11.09.2020

*Marta: 26.06.2008
* Eliminar la comprobación de retención
* que solo se use cuando haya algo en los campos de retención

*Marta: 25.06.2008
*    IF lt_t_1-pais_r IS INITIAL AND lt_t_1-ktokk EQ 'ZRET'.
*    IF lt_t_1-pais_r IS INITIAL AND lt_t_1-ktokk EQ 'ZTER'.
**Marta: 25.06.2008
*      lv_verif = sy-tabix.
*      lv_cursor_field = 'ZFIEPROV-PAIS_R'.
*      PERFORM texto_error_2 USING 'QLAND' lt_t_1-ktokk text-026 CHANGING lv_verif_t.
*      EXIT.

*    ENDIF.

*Marta: 25.06.2008
*    IF lt_t_1-wt_withcd IS INITIAL AND lt_t_1-ktokk EQ 'ZRET'.
*    IF lt_t_1-wt_withcd IS INITIAL AND lt_t_1-ktokk EQ 'ZTER'.
*Marta: 25.06.2008
*      lv_verif = sy-tabix.
*      lv_cursor_field = 'ZFIEPROV-WT_WITHCD'.
*      PERFORM texto_error_2 USING 'WT_WITHCD' lt_t_1-ktokk text-026 CHANGING lv_verif_t.
*      EXIT.

*    ENDIF.

*Marta: 25.06.2008
*    IF NOT lt_t_1-pais_r IS INITIAL AND lt_t_1-ktokk NE 'ZRET'.
*    IF NOT lt_t_1-pais_r IS INITIAL AND lt_t_1-ktokk NE 'ZTER'.
*Marta: 25.06.2008
*      lv_verif = sy-tabix.
*      lv_cursor_field = 'ZFIEPROV-PAIS_R'.
*      PERFORM texto_error_2 USING 'QLAND' lt_t_1-ktokk text-024 CHANGING lv_verif_t.
*      EXIT.

*    ENDIF.

*Marta: 25.06.2008
*    IF NOT lt_t_1-wt_withcd IS INITIAL AND lt_t_1-ktokk NE 'ZRET'.
*    IF NOT lt_t_1-wt_withcd IS INITIAL AND lt_t_1-ktokk NE 'ZTER'.
*Marta: 25.06.2008
*      lv_verif = sy-tabix.
*      lv_cursor_field = 'ZFIEPROV-WT_WITHCD'.
*      PERFORM texto_error_2 USING 'WT_WITHCD' lt_t_1-ktokk text-024 CHANGING lv_verif_t.
*      EXIT.

*    ENDIF.

*Marta: 26.06.2008

    IF lt_t_1-rcomp IS INITIAL AND lt_t_1-bu_group EQ 'ZTGR'.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-RCOMP'.
      PERFORM texto_error_2 USING 'VBUND' lt_t_1-bu_group TEXT-026 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

    IF NOT lt_t_1-rcomp IS INITIAL AND lt_t_1-bu_group NE 'ZTGR'.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-RCOMP'.
      PERFORM texto_error_2 USING 'VBUND' lt_t_1-bu_group TEXT-024 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

    IF ( lt_t_1-repl EQ 'X' ).

      REFRESH lt_bukrs.

      PERFORM acces_gr_sync TABLES lt_bukrs.

      CLEAR lv_fail.
      PERFORM check_bukrs TABLES lt_bukrs
                          USING lt_t_1-akont
                          CHANGING lv_fail lv_verif_t.

      IF NOT lv_fail IS INITIAL.
        lv_verif = sy-tabix.
      ENDIF.

    ENDIF.

*** INICIO MODIFICACIÓN EMG 22/10/2008
    IF ( ( NOT lt_t_1-pais_r IS INITIAL OR NOT lt_t_1-witht IS INITIAL OR NOT lt_t_1-wt_withcd IS INITIAL ) AND
         (     lt_t_1-pais_r IS INITIAL OR     lt_t_1-witht IS INITIAL OR     lt_t_1-wt_withcd IS INITIAL ) ).
      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-PAIS_R'.
      lv_verif_t = TEXT-028.
      EXIT.
    ENDIF.
    IF NOT lt_t_1-witht IS INITIAL AND
       NOT lt_t_1-wt_withcd IS INITIAL.
      SELECT COUNT(*) FROM t059z UP TO 1 ROWS
                      WHERE witht     EQ lt_t_1-witht
                        AND wt_withcd EQ lt_t_1-wt_withcd.
      IF sy-subrc NE 0 OR
         ( sy-subrc EQ 0 AND sy-dbcnt EQ 0 ).
        lv_verif = sy-tabix.
        lv_cursor_field = 'ZFIEPROV-WITHT'.
        lv_verif_t = TEXT-029.
      ENDIF.
    ENDIF.
*** FIN MODIFICACIÓN EMG 22/10/2008

*** INICIO MODIFICACIÓN EMG 12/05/2009
    IF NOT lt_t_1-brsch IS INITIAL.
      SELECT COUNT(*) FROM t016 WHERE brsch EQ lt_t_1-brsch.
      IF sy-subrc NE 0.
        lv_verif = sy-tabix.
        lv_cursor_field = 'ZFIEPROV-BRSCH'.
        MESSAGE ID 'ZMJE' TYPE 'W' NUMBER 403 INTO lv_verif_t WITH lt_t_1-brsch.
        EXIT.
      ENDIF.
    ENDIF.
*** FIN MODIFICACIÓN EMG 12/05/2009

  ENDLOOP.

ENDFORM.                    " VERIF
*&---------------------------------------------------------------------*
*&      Form  CREAR_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM crear_message .

  MESSAGE i021(zfi01) WITH lv_verif lv_verif_t.

ENDFORM.                    " CREAR_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  CONFIRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM confirm .

  DATA lv_lin TYPE i.

  DATA lv_conf(1).

  DESCRIBE TABLE lt_t_1 LINES lv_lin.

  IF NOT lv_lin IS INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = TEXT-010
        text_question         = TEXT-011
        display_cancel_button = ''
        start_column          = 25
        start_row             = 6
      IMPORTING
        answer                = lv_conf.

    IF lv_conf = '1'.

      lv_confirm = 1.

    ELSE.

      lv_confirm = 0.

    ENDIF.

  ELSE.

    lv_confirm = 1.

  ENDIF.

ENDFORM.                    " CONFIRM
*&---------------------------------------------------------------------*
*&      Form  TR_APROB_CONF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_aprob_conf .

  DATA: lv_index TYPE i.
  DATA: lv_aprob TYPE i.

  DATA: BEGIN OF lt_bukrs OCCURS 0,
          bukrs LIKE zfitprov-bukrs,
        END OF lt_bukrs.

  DATA lv_bukrs LIKE zfitprov-bukrs.

  PERFORM ini_cod_int.

* Solicitudes seleccionados
  LOOP AT lt_t_1 WHERE  sele = 'X' AND
                       ( migr IS INITIAL OR migr EQ 1 ).
    CLEAR gv_lifnr.
    lv_index = sy-tabix.
    lv_aprob = 0.
    lv_tot = lv_tot + 1.

** Inicio CGR 11/02/2010
    CLEAR: lv_first.
** Fin CGR CGR 11/02/2010

* Datos Generales + Sociedad / Sociedad
    PERFORM tr_aprob_sol USING lv_index CHANGING lv_aprob.

    IF NOT lv_aprob IS INITIAL AND NOT lt_t_1-bloq IS INITIAL.
* Bloqueo Sociedad
      PERFORM tr_bloq_soc USING lv_index CHANGING lv_aprob.
    ENDIF.

* Grupos de sincronización / Sociedades ES ( Migración ES )
    IF ( lt_t_1-repl EQ 'X' OR NOT lv_migr IS INITIAL ) AND
       NOT lv_aprob IS INITIAL.

** Inicio CGR 10/02/2010
      IF lv_aprob = -1.
        lv_first = 'X'.
      ENDIF.
** Inicio CGR 10/02/2010

      lv_bukrs = lt_t_1-bukrs.
      REFRESH lt_bukrs.

      IF NOT lv_migr IS INITIAL.
* Migración ES
        PERFORM mig_access_soc_es TABLES lt_bukrs.
      ELSE.
* Grupos de sincronización
        PERFORM acces_gr_sync TABLES lt_bukrs.
      ENDIF.

      LOOP AT lt_bukrs.

        IF lv_aprob IS INITIAL.
          EXIT.
        ENDIF.

        lt_t_1-bukrs = lt_bukrs.

* Sociedad (Modelo)
        CLEAR lv_aprob.
        PERFORM migr_soc_modl USING lv_bukrs lv_index
                              CHANGING lv_aprob.
** Inicio CGR 11/02/2009
        IF lt_t_1-repl = 'X'.

          lv_aprob = '1'.

        ELSE.
** Fin CGR 11/02/2009

          IF lv_aprob IS INITIAL.
            EXIT.
          ENDIF.
** Inicio CGR 11/02/2009
        ENDIF.

** Fin CGR 11/02/2009

        IF NOT lt_t_1-bloq IS INITIAL.
* Bloqueo Sociedad
          CLEAR lv_aprob.
          PERFORM tr_bloq_soc USING lv_index CHANGING lv_aprob.
        ENDIF.

      ENDLOOP.
      lt_t_1-bukrs = lv_bukrs.

    ENDIF.
* inicio CGR
    IF lv_first = 'X' AND lt_t_1-repl = 'X'.
      CLEAR lv_aprob.
    ENDIF.
*fin CGR


    IF lv_aprob IS INITIAL.

      PERFORM tr_grabar_sol USING lv_index.

*** INICIO MODIFICACIÓN EMG 22/10/2008
    ELSEIF lv_aprob EQ -1.
*** FIN MODIFICACIÓN EMG 22/10/2008

    ELSE.

      PERFORM tr_aprob_sol_act USING lv_index.
      lv_totac = lv_totac + 1.

    ENDIF.

  ENDLOOP.

  PERFORM fin_cod_int.

ENDFORM.                    " TR_APROB_CONF
*&---------------------------------------------------------------------*
*&      Form  INI_COD_INT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ini_cod_int .

  DO.

    CALL FUNCTION 'ENQUEUE_EZFIBZFITPROV'
      EXPORTING
        mode_zfitprov  = 'E'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc = 0.

      EXIT.

    ELSE.

      WAIT UP TO 1 SECONDS.

    ENDIF.

  ENDDO.

  lv_cod_int = 0.

  SELECT * FROM zfitprov ORDER BY cod_int DESCENDING.

    lv_cod_int = zfitprov-cod_int.
    EXIT.

  ENDSELECT.

ENDFORM.                    " INI_COD_INT
*&---------------------------------------------------------------------*
*&      Form  FIN_COD_INT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fin_cod_int .

  COMMIT WORK.

  CALL FUNCTION 'DEQUEUE_EZFIBZFITPROV'
    EXPORTING
      mode_zfitprov = 'E'.

ENDFORM.                    " FIN_COD_INT
*&---------------------------------------------------------------------*
*&      Form  TR_CONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_cons .

  lv_confirm = 0.

  PERFORM confirm_fin.

  IF NOT lv_confirm IS INITIAL.

    LEAVE TO TRANSACTION 'ZBP004'.

  ENDIF.

ENDFORM.                    " TR_CONS
*&---------------------------------------------------------------------*
*&      Form  CONFIRM_FIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM confirm_fin .

  DATA lv_lin TYPE i.

  DATA lv_conf(1).

  DESCRIBE TABLE lt_t_1 LINES lv_lin.

  IF NOT lv_lin IS INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = TEXT-013
        text_question         = TEXT-014
        display_cancel_button = ''
        start_column          = 25
        start_row             = 6
      IMPORTING
        answer                = lv_conf.

    IF lv_conf = '1'.

      lv_confirm = 1.

    ELSE.

      lv_confirm = 0.

    ENDIF.

  ELSE.

    lv_confirm = 1.

  ENDIF.

ENDFORM.                    " CONFIRM_FIN
*&---------------------------------------------------------------------*
*&      Form  ACCESO_TABLAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM acceso_tablas .

  DATA lv_index TYPE i.

  REFRESH lt_t_1.
  SELECT * FROM zfitprov
           INTO CORRESPONDING FIELDS OF TABLE lt_t_1
           WHERE  migr EQ 0 AND
                  ( estado EQ 'E' OR ( estado EQ 'P' AND estdes EQ 'C' ) ) AND
                  bukrs IN r_bukrs_aut AND
                  fecha IN so_fecha.
  IF NOT so_estad IS INITIAL.
    LOOP AT lt_t_1 WHERE estado <> so_estad.
      DELETE lt_t_1.
    ENDLOOP.
  ENDIF.
  LOOP AT lt_t_1 WHERE NOT cod_enl IS INITIAL.

    lt_t_1-hist = 'X'.
    MODIFY lt_t_1.

  ENDLOOP.


ENDFORM.                    " ACCESO_TABLAS
*&---------------------------------------------------------------------*
*&      Form  ACCESO_TABLAS_M
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM acceso_tablas_m .

  DATA lv_index TYPE i.

  REFRESH lt_t_1.
  SELECT * FROM zfitprov
           INTO CORRESPONDING FIELDS OF TABLE lt_t_1
           WHERE  migr NE 0 AND
                ( estado EQ 'P' AND
                  estdes EQ 'C' ) .

ENDFORM.                    " ACCESO_TABLAS_M
*&---------------------------------------------------------------------*
*&      Form  TR_RECH_SOL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_rech_sol USING VALUE(l_index).

** ( 3 - 4 )
* Info + State
  MOVE-CORRESPONDING lt_t_1 TO zfitprov.
  zfitprov-estado = 'R'.
  CLEAR zfitprov-estdes..
*

** ( 3 )
* Actualización
  IF lt_t_1-estado = 'P'.
    UPDATE zfitprov.
  ENDIF.
*

** ( 4 )
* Inicialización info central
  IF lt_t_1-estado = 'E'.

* Enlace
    lv_cod_int = lv_cod_int + 1.
    zfitprov-cod_int = lv_cod_int.
    zfitprov-cod_enl = lt_t_1-cod_int.

* Insert
    INSERT zfitprov.

* Paso a Historial
    SELECT SINGLE * FROM zfitprov WHERE cod_int = lt_t_1-cod_int.
    DELETE zfitprov.

    zfitprovh = zfitprov.
    INSERT zfitprovh.

  ENDIF.

* Entrada Dynpro
  DELETE lt_t_1 INDEX l_index.

ENDFORM.                    " TR_RECH_SOL
*&---------------------------------------------------------------------*
*&      Form  TR_GRABAR_SOL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_grabar_sol USING VALUE(l_index).

** ( 5 - 6 )
* Info + State
  MOVE-CORRESPONDING lt_t_1 TO zfitprov.
  zfitprov-estado = 'P'.
  zfitprov-estdes = 'C'.
*

** ( 5 )
* Actualización
  IF lt_t_1-estado = 'P'.
    UPDATE zfitprov.
  ENDIF.
*

** ( 6 )
* Inicialización info central
  IF lt_t_1-estado = 'E'.

* Enlace
    lv_cod_int = lv_cod_int + 1.
    zfitprov-cod_int = lv_cod_int.
    zfitprov-cod_enl = lt_t_1-cod_int.

* Insert
    INSERT zfitprov.

* Paso a Historial
    IF NOT lt_t_1-cod_int IS INITIAL.
      SELECT SINGLE * FROM zfitprov WHERE cod_int = lt_t_1-cod_int.
      DELETE zfitprov.

      zfitprovh = zfitprov.
      INSERT zfitprovh.
    ENDIF.

* Entrada Dynpro
    lt_t_1-estado = 'P'.
    lt_t_1-estdes = 'C'.
    lt_t_1-cod_enl = lt_t_1-cod_int.
    lt_t_1-cod_int = lv_cod_int.

    MODIFY lt_t_1 INDEX l_index.

  ENDIF.

ENDFORM.                    " TR_GRABAR_SOL
*&---------------------------------------------------------------------*
*&      Form  TR_REFR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_refr .

  lv_ini = 0.

ENDFORM.                    " TR_REFR
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9002 INPUT.

  IF sy-ucomm = 'ENTER' OR sy-ucomm = 'CANC'.

    LEAVE TO SCREEN 0.

  ENDIF.

ENDMODULE.                 " USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*&      Form  ACCESO_TABLAS_HIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM acceso_tablas_hist .

  DATA lv_cod_enl LIKE zfitprovh-cod_enl.

  REFRESH lt_t_2.
  CLEAR lt_t_2.

  lv_cod_enl = lt_t_1-cod_enl.

  DO.

    SELECT SINGLE * FROM zfitprovh WHERE cod_int EQ lv_cod_enl.
    IF sy-subrc = 0.

      APPEND zfitprovh TO lt_t_2.
      lv_cod_enl = zfitprovh-cod_enl.

    ELSE.

      EXIT.

    ENDIF.

  ENDDO.

ENDFORM.                    " ACCESO_TABLAS_HIST
*&---------------------------------------------------------------------*
*&      Form  TR_HIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_hist .

  DATA lv_verif TYPE i.

  DATA lv_index TYPE i.

  LOOP AT lt_t_1 WHERE sele = 'X'.

    lv_verif = lv_verif + 1.
    lv_index = sy-tabix.

  ENDLOOP.

  CHECK lv_verif = 1.

  READ TABLE lt_t_1 INDEX lv_index.

  CHECK NOT lt_t_1-hist IS INITIAL.

  lv_ini_h = 0.

  CALL SCREEN '9002'
            STARTING AT 10 10.

ENDFORM.                    " TR_HIST
*&---------------------------------------------------------------------*
*&      Form  CARGA_MIGR_DYN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM carga_migr_dyn .

  DATA le_col TYPE cxtab_column.

  LOOP AT t_1-cols INTO le_col.

    IF le_col-screen-group1 = 'FUN'.

      le_col-invisible = 1.
      MODIFY t_1-cols FROM le_col.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " CARGA_MIGR_DYN
*&---------------------------------------------------------------------*
*&      Form  CARGA_FUN_DYN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM carga_fun_dyn .

  DATA le_col TYPE cxtab_column.

  LOOP AT t_1-cols INTO le_col.

    IF le_col-screen-group1 = 'MIG'.

      le_col-invisible = 1.
      MODIFY t_1-cols FROM le_col.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " CARGA_FUN_DYN
*&---------------------------------------------------------------------*
*&      Form  TR_APROB_SOL_ACT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_aprob_sol_act  USING l_index.

** ( 1 - 2 )
* Info + State
  MOVE-CORRESPONDING lt_t_1 TO zfitprov.
  zfitprov-estado = 'A'.
  CLEAR zfitprov-estdes..
*

** ( 1 )
* Actualización
  IF lt_t_1-estado = 'P'.
    UPDATE zfitprov.
  ENDIF.
*

** ( 2 )
* Inicialización info central
  IF lt_t_1-estado = 'E'.

* Enlace
    lv_cod_int = lv_cod_int + 1.
    zfitprov-cod_int = lv_cod_int.
    zfitprov-cod_enl = lt_t_1-cod_int.

* Insert
    INSERT zfitprov.

* Paso a Historial
    IF NOT lt_t_1-cod_int IS INITIAL.
      SELECT SINGLE * FROM zfitprov WHERE cod_int = lt_t_1-cod_int.
      DELETE zfitprov.

      zfitprovh = zfitprov.
      INSERT zfitprovh.
    ENDIF.

  ENDIF.

* Entrada Dynpro
  DELETE lt_t_1 INDEX l_index.

ENDFORM.                    " TR_APROB_SOL_ACT
*&---------------------------------------------------------------------*
*&      Form  ACT_PROVNAV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM act_provnav .

  MOVE-CORRESPONDING lt_t_1 TO zfitprovnav.
  MODIFY zfitprovnav.

ENDFORM.                    " ACT_PROVNAV
*&---------------------------------------------------------------------*
*&      Form  ACCES_GR_SYNC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM acces_gr_sync  TABLES t_bukrs.

  SELECT SINGLE * FROM zfit_fiscal_fi WHERE bukrs = lt_t_1-bukrs.

  IF NOT zfit_fiscal_fi-codgrsinc IS INITIAL.

    SELECT * FROM zfit_fiscal_fi INTO CORRESPONDING FIELDS
                                OF TABLE t_bukrs
                                WHERE bukrs NE zfit_fiscal_fi-bukrs AND
                                    codgrsinc EQ zfit_fiscal_fi-codgrsinc.
  ENDIF.


ENDFORM.                    " ACCES_GR_SYNC
*&---------------------------------------------------------------------*
*&      Form  MIG_ACCESS_SOC_ES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM mig_access_soc_es  TABLES t_bukrs.

  SELECT * FROM t001 INTO CORRESPONDING FIELDS
                              OF TABLE t_bukrs
                              WHERE bukrs NE lt_t_1-bukrs AND
                                    land1 EQ 'ES'.

ENDFORM.                    " MIG_ACCESS_SOC_ES
*&---------------------------------------------------------------------*
*&      Form  MIGR_SOC_MODL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM migr_soc_modl USING VALUE(l_bukrs)
                         VALUE(l_index)
                   CHANGING l_aprob.

  DATA lv_mess(100).
  DATA lv_quest TYPE string.
  DATA lv_conf(1).

  DATA lv_subrc TYPE i.

  SELECT SINGLE * FROM lfb1 WHERE bukrs = lt_t_1-bukrs AND
                                  lifnr = lt_t_1-partner.

  lv_subrc = sy-subrc.
  IF lv_subrc = 0 AND lv_migr IS INITIAL..

*    MOVE text-017 TO lv_quest.
*    REPLACE '&1' WITH lt_t_1-bukrs INTO lv_quest.
*    REPLACE '&2' WITH lt_t_1-lifnr INTO lv_quest.

*    CALL FUNCTION 'POPUP_TO_CONFIRM'
*      EXPORTING
*        titlebar              = text-015
*        text_question         = lv_quest
*        default_button        = '1'
*        display_cancel_button = ''
*        start_column          = 25
*        start_row             = 6
*      IMPORTING
*        answer                = lv_conf.
*
*    IF lv_conf = '2'.
*      .
** Proceso continua
*      l_aprob = 1.
*
*    ELSE.

    MESSAGE TEXT-017 TYPE 'I'.

    MOVE TEXT-018 TO lv_quest.
    REPLACE '&1' WITH lt_t_1-bukrs INTO lv_quest.
    REPLACE '&2' WITH lt_t_1-partner INTO lv_quest.

    lt_t_1-error = lv_quest.
    MODIFY lt_t_1 INDEX l_index.

*    ENDIF.

    EXIT.

  ENDIF.

  IF lv_subrc = 0 AND NOT lv_migr IS INITIAL.

    l_aprob = 1.
    EXIT.

  ENDIF.

  PERFORM crea_sociedad_mod USING l_bukrs
                             CHANGING l_aprob lv_mess.

  IF l_aprob IS INITIAL.

    lt_t_1-error = lv_mess.
    lt_t_1-bukrs = l_bukrs.
    MODIFY lt_t_1 INDEX l_index.

  ENDIF.

ENDFORM.                    " MIGR_SOC_MODL
*&---------------------------------------------------------------------*
*&      Form  MIGR_SOC_MODIF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM migr_soc_modif USING VALUE(l_index)
                   CHANGING l_aprob.

  DATA lv_mess(100).

  PERFORM modif_sociedad CHANGING l_aprob lv_mess.
  IF l_aprob IS INITIAL.

    lt_t_1-error = lv_mess.
    MODIFY lt_t_1 INDEX l_index.

  ENDIF.

ENDFORM.                    " MIGR_SOC_MODIF
*&---------------------------------------------------------------------*
*&      Form  TR_MIGR_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_migr_1 .

  IF NOT lv_migr IS INITIAL.

    lv_migr = 1.

    PERFORM carga_fich.

    PERFORM entradas_fichero_1.

  ENDIF.

ENDFORM.                                                    " TR_MIGR_1
*&---------------------------------------------------------------------*
*&      Form  TR_MIGR_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_migr_2 .

  IF NOT lv_migr IS INITIAL.

    lv_migr = 2.

    PERFORM carga_fich.

    PERFORM entradas_fichero_2.

  ENDIF.

ENDFORM.                                                    " TR_MIGR_1
*&---------------------------------------------------------------------*
*&      Form  ENTRADAS_FICHERO_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM entradas_fichero_1.

  DATA lt_t_1_cp LIKE zfieprov OCCURS 0 WITH HEADER LINE.
  DATA lv_index TYPE i.
  DATA lv_error TYPE i.
  DATA lv_waers(3).

  DO.

    lv_index = lv_index + 1.
    READ TABLE lt_fich INDEX lv_index.
    IF sy-subrc <> 0 OR lt_fich-value IS INITIAL.

      EXIT.

    ENDIF.

* Total entradas 31

    lv_index = lv_index - 1.

* Migracion
    lt_t_1_cp-estado = 'E'.
    lt_t_1_cp-migr = 1.

* Sociedad
    lt_t_1_cp-bukrs = '0001'.

* Datos Navisión
    PERFORM entradas_fich_navision CHANGING lt_t_1_cp lv_index.   " 4 + 3 entradas

* Datos Generales
    PERFORM entradas_fich_generales CHANGING lt_t_1_cp lv_index.   " 14 entradas

* Datos Generales2
    PERFORM entradas_fich_generales_2 CHANGING lt_t_1_cp lv_index.   " 3 entradas

* Datos Sociedad tratamiento acreedor
    PERFORM entradas_fich_sociedad CHANGING lt_t_1_cp lv_index.   " 15 entradas

    APPEND lt_t_1_cp.

  ENDDO.

  IF lv_error = 1.

    MESSAGE i201(zfi01).

  ELSE.

    APPEND LINES OF lt_t_1_cp TO lt_t_1.
    lv_sup_d = 1.

  ENDIF.

ENDFORM.                    " ENTRADAS_FICHERO_1

*&---------------------------------------------------------------------*
*&      Form  ENTRADAS_FICHERO_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM entradas_fichero_2.

  DATA lt_t_1_cp LIKE zfieprov OCCURS 0 WITH HEADER LINE.
  DATA lv_index TYPE i.
  DATA lv_error TYPE i.
  DATA lv_waers(3).

  DO.

    lv_index = lv_index + 1.
    READ TABLE lt_fich INDEX lv_index.
    IF sy-subrc <> 0 OR lt_fich-value IS INITIAL.

      EXIT.

    ENDIF.

* Total entradas 15

    lv_index = lv_index - 1.

* Migracion
    lt_t_1_cp-estado = 'E'.
    lt_t_1_cp-migr = 2.

* Sociedad
    PERFORM leer_lt_fich CHANGING lv_index lt_t_1_cp-bukrs.   " 1 entradas

* Datos Navisión
    PERFORM entradas_fich_navision CHANGING lt_t_1_cp lv_index.   " 4 entradas

* Datos Sociedad tratamiento acreedor
    PERFORM entradas_fich_sociedad CHANGING lt_t_1_cp lv_index.   " 10 entradas

    APPEND lt_t_1_cp.

  ENDDO.

  IF lv_error = 1.

    MESSAGE i201(zfi01).

  ELSE.

    APPEND LINES OF lt_t_1_cp TO lt_t_1.
    lv_sup_d = 1.

  ENDIF.

ENDFORM.                    " ENTRADAS_FICHERO_2

*&---------------------------------------------------------------------*
*&      Form  LEER_LT_FICH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM leer_lt_fich CHANGING l_index l_value.

  l_index = l_index + 1.
  READ TABLE lt_fich INDEX l_index.
  IF lt_fich-value NE '-' AND lt_fich-value NE '.'.
    l_value = lt_fich-value.
  ENDIF.

ENDFORM.                    " LEER_LT_FICH
*&---------------------------------------------------------------------*
*&      Form  ENTRADAS_FICH_NAVISION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM entradas_fich_navision  CHANGING e_t_1 STRUCTURE zfieprov
                             l_index.

  DATA lv_null TYPE string.

  PERFORM leer_lt_fich CHANGING l_index e_t_1-zzprovenl.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-zzprovmas.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-zzprovnav.
  PERFORM leer_lt_fich CHANGING l_index lv_null.          " DUPLICADO
  PERFORM leer_lt_fich CHANGING l_index e_t_1-zzsistmas.
  PERFORM leer_lt_fich CHANGING l_index lv_null.          " PROV. MASTER
  PERFORM leer_lt_fich CHANGING l_index lv_null.          " SIST. MASTER

*  Montar campo ALTKT

  IF NOT e_t_1-zzprovnav IS INITIAL.
    CONCATENATE e_t_1-zzprovnav e_t_1-zzsistmas INTO e_t_1-altkt.
  ELSE.
    CONCATENATE e_t_1-zzprovmas e_t_1-zzsistmas INTO e_t_1-altkt.
  ENDIF.

*  Fecha migración

  e_t_1-fecha = sy-datum.


ENDFORM.                    " ENTRADAS_FICH_NAVISION
*&---------------------------------------------------------------------*
*&      Form  ENTRADAS_FICH_GENERALES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM entradas_fich_generales  CHANGING e_t_1 STRUCTURE zfieprov
                             l_index.

  PERFORM leer_lt_fich CHANGING l_index e_t_1-name1.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-name2.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-busq.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-direc.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-direc_2.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-cod_post.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-poblac.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-region.

  PERFORM leer_lt_fich CHANGING l_index e_t_1-pais.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-cif.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-tel.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-fax.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-smtp.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-spras.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-c_fisc_mx .

ENDFORM.                    " ENTRADAS_FICH_GENERALES
*&---------------------------------------------------------------------*
*&      Form  ENTRADAS_FICH_GENERALES_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM entradas_fich_generales_2  CHANGING e_t_1 STRUCTURE zfieprov
                             l_index.

  PERFORM leer_lt_fich CHANGING l_index e_t_1-bu_group.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-waers.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-xersy.

ENDFORM.                    " ENTRADAS_FICH_GENERALES_2
*&---------------------------------------------------------------------*
*&      Form  ENTRADAS_FICH_SOCIEDAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM entradas_fich_sociedad  CHANGING e_t_1 STRUCTURE zfieprov
                             l_index.

  IF lv_migr = 1.

    PERFORM leer_lt_fich CHANGING l_index e_t_1-bukrs.  " Sociedad

  ENDIF.

  PERFORM leer_lt_fich CHANGING l_index e_t_1-akont.  " Cta. as.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-rcomp.  " S.G.L.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-fdgrv.  " Gr. tes

  PERFORM leer_lt_fich CHANGING l_index e_t_1-zterm.  " Cond. pago
  PERFORM leer_lt_fich CHANGING l_index e_t_1-zwels.  " Via pago

  PERFORM leer_lt_fich CHANGING l_index e_t_1-dzsabe_k. " Contac

  PERFORM leer_lt_fich CHANGING l_index e_t_1-pais_r.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-wt_withcd.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-bloq.
  PERFORM leer_lt_fich CHANGING l_index e_t_1-bloqj.

ENDFORM.                    " ENTRADAS_FICH_SOCIEDAD
*&---------------------------------------------------------------------*
*&      Form  CARGA_FICH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM carga_fich .

  DATA lv_end_col TYPE i.

  IF lv_migr = 1.

    lv_end_col = 36.

  ELSE.

    lv_end_col = 15.

  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = '*.XLS'
      def_path         = ' '
      mask             = ' '
      mode             = ' '
      title            = ' '
    IMPORTING
      filename         = lv_fich
*     RC               =
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.
  IF sy-subrc <> 0.

  ELSE.

    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = lv_fich
        i_begin_col             = 1
        i_begin_row             = 2
        i_end_col               = lv_end_col
        i_end_row               = 20000
      TABLES
        intern                  = lt_fich
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDFORM.                    " CARGA_FICH
*&---------------------------------------------------------------------*
*&      Form  TR_ACRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_acre .

  DATA lv_help_infos LIKE help_info.
  DATA lt_dynpselect LIKE dselc OCCURS 0.
  DATA lt_dynpvaluetab LIKE dval OCCURS 0.

  lv_help_infos-tabname = 'ZFIEPROV'.
  lv_help_infos-fieldname = 'PARTNER'.

  lv_help_infos-call = 'V'.
  lv_help_infos-object = 'F'.
  lv_help_infos-program = 'ZFI0009'.
  lv_help_infos-dynpro = '9001'.
  lv_help_infos-tabname = 'ZFIEPROV'.
  lv_help_infos-fieldname = 'PARTNER'.
  lv_help_infos-fieldtype = 'CHAR'.
  lv_help_infos-keyword = 'Acreedor'.
  lv_help_infos-fieldlng = '10'.
*  LV_HELP_INFOS-FLDVALUE
*  LV_HELP_INFOS-MCOBJ
  lv_help_infos-spras = 'S'.
  lv_help_infos-menufunct = 'HC'.

  CALL FUNCTION 'HELP_START'
    EXPORTING
      help_infos   = lv_help_infos
*     PROPERTY_BAG =
*   IMPORTING
*     SELECTION    =
*     SELECT_VALUE =
*     RSMDY_RET    =
    TABLES
      dynpselect   = lt_dynpselect
      dynpvaluetab = lt_dynpvaluetab.


ENDFORM.                    " TR_ACRE
*----------------------------------------------------------------------*
***INCLUDE ZFI0009FRMBI1 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  TR_APROB_SOL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_aprob_sol USING VALUE(l_index)
                   CHANGING l_aprob.

  DATA lv_mess(100).
  DATA lv_ch TYPE i.
  DATA lv_lifnr LIKE zfitprov-partner.

  IF NOT lv_migr IS INITIAL.
* Migración
    PERFORM crea_acreedor_conf_m CHANGING l_aprob
                                         lv_ch
                                         lv_lifnr.

  ELSE.
* Funcional
    PERFORM crea_acreedor_conf_v2 CHANGING l_aprob
                                         lv_ch.

  ENDIF.

  CHECK NOT l_aprob IS INITIAL.

  CLEAR l_aprob.
  CLEAR lt_t_1-error.

  IF lv_ch IS INITIAL.
    IF gv_lifnr IS INITIAL.

*---------------------------------------------------------------------*
*           INICIO MODIFICACIÓN
*---------------------------------------------------------------------*
* Autor: Marta Vall Armengol
* Fecha: 04.06.2008
*---------------------------------------------------------------------*
* En el caso de la sociedad 8300 no tiene organización de compras
* por eso vamos a crear directamente el acreedor por la transacción
* fk01
*---------------------------------------------------------------------*

      SELECT COUNT(*) FROM t024w WHERE werks EQ lt_t_1-bukrs.
      IF sy-subrc EQ 0.

        PERFORM crea_acreedor CHANGING l_aprob lv_mess lv_lifnr.

      ENDIF.
*    ELSE.
*      l_aprob = 1.
*    ENDIF.
      WAIT UP TO 2 SECONDS."JGOR 26.01.2018

      SELECT COUNT(*) FROM t024w WHERE werks EQ lt_t_1-bukrs.
      IF sy-subrc EQ 0.
        IF NOT l_aprob IS INITIAL.
          l_aprob = 0.
          IF gv_lifnr IS INITIAL.
            lt_t_1-partner = lv_lifnr.
*            PERFORM tr_tr_act_calle2 USING lv_lifnr."aayala
          ELSE.
            lt_t_1-partner = gv_lifnr.
*        PERFORM tr_tr_act_calle2 USING gv_lifnr.
          ENDIF.



          IF NOT lv_migr IS INITIAL.
* Migración
            PERFORM act_provnav.

          ENDIF.

          PERFORM crea_sociedad CHANGING l_aprob lv_mess.
        ENDIF.

      ELSE.
*      IF NOT l_aprob IS INITIAL.
*        l_aprob = 0.
*        IF gv_lifnr IS INITIAL.
*          lt_t_1-lifnr = lv_lifnr.
*          PERFORM tr_tr_act_calle2 USING lv_lifnr.
*        ELSE.
*          lt_t_1-lifnr = gv_lifnr.
**        PERFORM tr_tr_act_calle2 USING gv_lifnr.
*        ENDIF.



        IF NOT lv_migr IS INITIAL.
* Migración
          PERFORM act_provnav.

        ENDIF.
* societat que no te organització de compres
        PERFORM crea_sociedad_8300 CHANGING l_aprob lv_mess.
*      PERFORM crea_sociedad CHANGING l_aprob lv_mess.
*Marta: 05.06.2008
*    ENDIF.
*  ENDIF.

      ENDIF.
    ENDIF.
    IF l_aprob IS INITIAL.

      lt_t_1-error = lv_mess.
      MODIFY lt_t_1 INDEX l_index.

    ENDIF.

  ELSE.
*** INICIO MODIFICACIÓN EMG 22/10/2008
*    l_aprob = 1.
    PERFORM crea_sociedad CHANGING l_aprob lv_mess.
    IF l_aprob EQ 0.
      CLEAR: l_aprob.
      l_aprob = -1.
    ENDIF.
*** FIN MODIFICACIÓN EMG 22/10/2008
  ENDIF.

ENDFORM.                    " TR_APROB_SOL
*&---------------------------------------------------------------------*
*&      Form  MIGR_TR_APROB_SOL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM migr_tr_aprob_sol USING VALUE(l_index)
                   CHANGING l_aprob.

  DATA lv_mess(100).
  DATA lv_ch TYPE i.
  DATA lv_lifnr LIKE zfitprov-partner.

*  L_APROB = 1
*  LV_CH = 0
*  LV_LIFNR = ''
  PERFORM crea_acreedor_conf_m CHANGING l_aprob
                                        lv_ch
                                        lv_lifnr.

  PERFORM crea_acreedor CHANGING l_aprob lv_mess lv_lifnr.

  IF l_aprob IS INITIAL.

    lt_t_1-error = lv_mess.
    MODIFY lt_t_1 INDEX l_index.

  ENDIF.

  CHECK NOT l_aprob IS INITIAL.

  lt_t_1-partner = lv_lifnr.

  IF NOT lv_migr IS INITIAL.
* Migración
    PERFORM act_provnav.

  ENDIF.

ENDFORM.                    " MITR_TR_APROB_SOL
*&---------------------------------------------------------------------*
*&      Form  CREA_ACREEDOR_CONF
*&---------------------------------------------------------------------*
* FUNCIONALIDAD - HOTEL / CENTRAL
* Verifica NIF y Sociedades creadas
* APROB -> 1 : Usuario acepta en caso de mismo NIF y Sociedad
* CH -> 1 : Mismo NIF y diferente Sociedad
*----------------------------------------------------------------------*
FORM crea_acreedor_conf CHANGING l_ok TYPE i
                                 l_ch TYPE i
                                 l_lifnr TYPE lifnr.

  DATA lv_conf(1).

  DATA lv_quest TYPE string.

  SELECT * UP TO 1 ROWS FROM lfa1 WHERE stcd1 = lt_t_1-cif.
  ENDSELECT.

  IF sy-subrc <> 0.
* No existe acreedor con mismo CIF
    l_ok = 1.

  ENDIF.

  CHECK l_ok IS INITIAL.

  SELECT * UP TO 1 ROWS FROM lfb1 WHERE bukrs = lt_t_1-bukrs AND
                                        lifnr = lfa1-lifnr.
  ENDSELECT.

  IF sy-subrc <> 0.
* Existe acreedor con mismo CIF. La Sociedad no esta creada

    l_lifnr = lfa1-lifnr.
    l_ch = 1.
    l_ok = 1.

  ENDIF.

  CHECK l_ok IS INITIAL.

* Existe acreedor con mismo CIF - Sociedad

  MOVE TEXT-016 TO lv_quest.
  REPLACE '&1' WITH lt_t_1-cif INTO lv_quest.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = TEXT-015
      text_question         = lv_quest
      default_button        = '2'
      display_cancel_button = ''
      start_column          = 25
      start_row             = 6
    IMPORTING
      answer                = lv_conf.

  IF lv_conf = '1'.
* Se crea un nuevo acreedor
    l_ok = 1.

  ENDIF.

ENDFORM.                    " CREA_ACREEDOR_CONF
*&---------------------------------------------------------------------*
*&      Form  CREA_ACREEDOR_CONF_V2
*&---------------------------------------------------------------------*
* FUNCIONALIDAD - HOTEL / CENTRAL
* Verifica NIF y Sociedades creadas
* APROB -> 1 : Usuario acepta en caso de mismo NIF y Sociedad
* CH -> 1 : Mismo NIF y diferente Sociedad
*----------------------------------------------------------------------*
FORM crea_acreedor_conf_v2 CHANGING l_ok TYPE i
                                 l_ch TYPE i.

  DATA lv_conf(1).

  DATA lv_quest TYPE string.

  IF NOT lt_t_1-partner IS INITIAL.

    SELECT * UP TO 1 ROWS FROM lfa1 WHERE lifnr = lt_t_1-partner.
    ENDSELECT.

    IF sy-subrc <> 0.
* No existe el acreedor LIFNR
      MESSAGE i203(zfi01) WITH lt_t_1-partner.
      EXIT.

    ELSE.
* Existe el acreedor LIFNR
      l_ok = 1.
      l_ch = 1.

    ENDIF.

  ELSE.

    SELECT * UP TO 1 ROWS FROM lfa1 WHERE stcd1 = lt_t_1-cif.
    ENDSELECT.

    IF sy-subrc <> 0.
* No existe acreedor con mismo CIF
      l_ok = 1.

    ELSE.
      gv_lifnr = lfa1-lifnr.
    ENDIF.

  ENDIF.

  CHECK l_ok IS INITIAL.
* Existe acreedor con mismo CIF - Confirmación

  MOVE TEXT-016 TO lv_quest.
  REPLACE '&1' WITH lt_t_1-cif INTO lv_quest.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = TEXT-015
      text_question         = lv_quest
      default_button        = '3'
      display_cancel_button = 'X'
      start_column          = 25
      start_row             = 6
    IMPORTING
      answer                = lv_conf.

  IF lv_conf = '1'.
* Se crea un nuevo acreedor
    l_ok = 1.
    CLEAR gv_lifnr.
  ELSEIF lv_conf = '2'.
    l_ok = 1.
  ELSEIF lv_conf = 'A'.
    CLEAR gv_lifnr.
  ENDIF.

ENDFORM.                    " CREA_ACREEDOR_CONF_V2
*&---------------------------------------------------------------------*
*&      Form  CREA_ACREEDOR_CONF_M
*&---------------------------------------------------------------------*
* FUNCIONALIDAD - HOTEL / CENTRAL
* Verifica NIF y Sociedades creadas
* OK -> 1 : MASTER
* CH -> 1 : NO MASTER
*----------------------------------------------------------------------*
FORM crea_acreedor_conf_m CHANGING l_ok TYPE i
                                 l_ch TYPE i
                                 l_lifnr TYPE lifnr.

  DATA lv_conf(1).

  l_ok = 1.

  CLEAR zfitprovnav.
  SELECT * UP TO 1 ROWS FROM zfitprovnav WHERE zzprovenl = lt_t_1-zzprovenl.

  ENDSELECT.
  IF sy-subrc = 0.
* Prov Master / Ya creado

    l_ch = 1.  " El acreedor ya ha sido creado
    lt_t_1-partner = zfitprovnav-lifnr.

  ENDIF.

ENDFORM.                    " CREA_ACREEDOR_CONF_M
*&---------------------------------------------------------------------*
*&      Form  CREA_ACREEDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM crea_acreedor  CHANGING l_crea
                             l_mess
                             l_lifnr.

  DATA lv_mes(3) VALUE '170'.

  REFRESH bdcdata.

  PERFORM crea_acreedor_1.  "DATOS GENERALES

  PERFORM crea_acreedor_2.  "CIF

  PERFORM crea_acreedor_bank.

  PERFORM crea_acreedor_3_.  "CONTACTO

*  IF NOT LT_T_1-WAERS IS INITIAL.
*ini aayala 17.02.2012
*  IF lt_t_1-ktokk NE '2040' AND lt_t_1-ktokk NE '2910'.
*Ini JGOR 26.01.2018
*  IF ( lt_t_1-ktokk EQ '2010' OR
*       lt_t_1-ktokk EQ '2020' OR
*       lt_t_1-ktokk EQ '2030' OR
*       lt_t_1-ktokk EQ '2050' OR
*       lt_t_1-ktokk EQ '2910' ).
*Fin JGOR 26.01.2018
  PERFORM crea_acreedor_compras. "JLM 18.04.2016 DEVK906904
*  ENDIF."JGOR 26.01.2018
*fin aayala 17.02.2012
  lv_mes = '173'.

*  ENDIF.

  PERFORM bdc_field       USING 'BDC_OKCODE'
                              'UPDA'.

  PERFORM call_transaction  USING 'XK01'
                                 'F2'
                                  lv_mes
                           CHANGING l_crea
                                    l_mess
                                    l_lifnr.

ENDFORM.                    " CREA_ACREEDOR
*&---------------------------------------------------------------------*
*&      Form  CREA_ACREEDOR_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM crea_acreedor_1 .

  DATA lv_ekorg LIKE t024e-ekorg.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
  PERFORM bdc_field       USING 'RF02K-KTOKK'
                              lt_t_1-bu_group.

*    PERFORM bdc_field       USING 'RF02K-BUKRS'
*                                lt_t_1-bukrs.

*  IF NOT LT_T_1-WAERS IS INITIAL.
*
*    CLEAR T001K.
*    SELECT * UP TO 1 ROWS FROM T001K WHERE BUKRS = LT_T_1-BUKRS.
*    ENDSELECT.
*
*    CLEAR T001W.
*    SELECT * UP TO 1 ROWS FROM T001W WHERE BWKEY = T001K-BWKEY.
*    ENDSELECT.
*
*    IF SY-SUBRC = 0.
*      LV_EKORG = T001W-EKORG.
*    ELSE.
*      LV_EKORG = LT_T_1-BUKRS.
*    ENDIF.


  lv_ekorg = lt_t_1-ekorg.
*ini aayala 17.02.2012

**********************************************************************
* Inicio JLM 23.06.2016 DEVK906904
*  IF lt_t_1-ktokk NE '2040'  AND lt_t_1-ktokk NE '2910'.
*    PERFORM bdc_field       USING 'RF02K-EKORG'
*                                lv_ekorg.
*  ENDIF.
*Ini JGOR 26.01.2018
*  IF ( lt_t_1-ktokk EQ '2010' OR
*       lt_t_1-ktokk EQ '2020' OR
*       lt_t_1-ktokk EQ '2030' OR
*       lt_t_1-ktokk EQ '2050' OR
*       lt_t_1-ktokk EQ '2910' ).
*Fin JGOR 26.01.2018
  PERFORM bdc_field       USING 'RF02K-EKORG'
                             lv_ekorg.

*  ENDIF."JGOR 26.01.2018
* Fin JLM 23.06.2016DEVK906904
**********************************************************************

  PERFORM bdc_field       USING 'USE_ZAV'
                              'X'.
*  ENDIF.
*************************************************************
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0111'.

  PERFORM bdc_field       USING 'ADDR1_DATA-NAME1'
                              lt_t_1-name1(35).
  PERFORM bdc_field       USING 'ADDR1_DATA-NAME2'
                              lt_t_1-name2(35).
  PERFORM bdc_field       USING 'ADDR1_DATA-SORT1'
                              lt_t_1-busq.
  PERFORM bdc_field       USING 'ADDR1_DATA-STREET'
                              lt_t_1-direc(35).
*** INICIO MODIFICACIÓN EMG 10/12/2008
*  PERFORM bdc_field       USING 'ADDR1_DATA-STR_SUPPL1' lt_t_1-direc_2.
*** FIN MODIFICACIÓN EMG 10/12/2008
  PERFORM bdc_field       USING 'ADDR1_DATA-CITY1'
                              lt_t_1-poblac.
  PERFORM bdc_field       USING 'ADDR1_DATA-POST_CODE1'
                              lt_t_1-cod_post.
  PERFORM bdc_field       USING 'ADDR1_DATA-REGION'
                              lt_t_1-region.
  PERFORM bdc_field       USING 'ADDR1_DATA-COUNTRY'
                              lt_t_1-pais.
  PERFORM bdc_field       USING 'ADDR1_DATA-LANGU'
                              lt_t_1-spras.

  PERFORM bdc_field       USING 'SZA1_D0100-TEL_NUMBER'
                              lt_t_1-tel(27).
  PERFORM bdc_field       USING 'SZA1_D0100-FAX_NUMBER'
                              lt_t_1-fax(27).
  PERFORM bdc_field       USING 'SZA1_D0100-SMTP_ADDR'
                              lt_t_1-smtp.

  PERFORM bdc_field       USING 'BDC_OKCODE'
                               '/00'.

ENDFORM.                    " CREA_ACREEDOR_1
*&---------------------------------------------------------------------*
*&      Form  CREA_ACREEDOR_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM crea_acreedor_2 .

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0120'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
  IF lt_t_1-bu_group = '2060'.

    PERFORM bdc_field       USING 'LFA1-VBUND'
                                lt_t_1-rcomp.

  ENDIF.
  PERFORM bdc_field       USING 'LFA1-KONZS'
                              lt_t_1-c_fisc_mx.
*** INICIO MODIFICACIÓN EMG 12/05/2009
*  IF lt_t_1-ktokk EQ 'ZTGR' ORse elimina para rcd resorts aayala 30.11.2011
*     lt_t_1-ktokk EQ 'ZTER'.
  PERFORM bdc_field       USING 'LFA1-BRSCH' lt_t_1-brsch.
*  ENDIF. se elimina para rcd resorts aayala 30.11.2011
*** FIN MODIFICACIÓN EMG 12/05/2009
  PERFORM bdc_field       USING 'LFA1-STCD1'
                              lt_t_1-cif.
*  ini aayala agregar nif 3 19.12.2011
  IF lt_t_1-stcd3 IS NOT INITIAL.
    PERFORM bdc_field       USING 'LFA1-STCD3'
                               lt_t_1-stcd3.
  ENDIF.
  IF lt_t_1-stkzn IS NOT INITIAL.
    PERFORM bdc_field       USING 'LFA1-STKZN'
                              lt_t_1-stkzn.
  ENDIF.
*  ini aayala agregar nif 3 19.12.2011
*  PERFORM bdc_field       USING 'LFA1-STCD2'                "IA290709
*                            lt_t_1-stcd2.                   "IA290709
ENDFORM.                    " CREA_ACREEDOR_2
*&---------------------------------------------------------------------*
*&      Form  CREA_SOCIEDAD_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM crea_sociedad_3 .
*Marta: 25.06.2008
*  IF lt_t_1-ktokk = 'ZRET'.
*  IF lt_t_1-ktokk = 'ZTER'.
*Marta: 25.06.2008

*** INICIO MODIFICACIÓN EMG 21/10/2008
*  SELECT * UP TO 1 ROWS FROM t059z WHERE land1 = lt_t_1-pais AND
*                                   wt_withcd = lt_t_1-wt_withcd.
*  ENDSELECT.
*** FIN MODIFICACIÓN EMG 21/10/2008
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0220'.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0610'.

  PERFORM bdc_field       USING 'LFB1-QLAND'
                             lt_t_1-pais_r.

  PERFORM bdc_field       USING 'LFBW-WT_WITHCD(01)'
                             lt_t_1-wt_withcd.

  PERFORM bdc_field       USING 'LFBW-WITHT(01)'
*** INICIO MODIFICACIÓN EMG 21/10/2008
*                             t059z-witht.
                             lt_t_1-witht.
*** FIN MODIFICACIÓN EMG 21/10/2008

  PERFORM bdc_field       USING 'LFBW-WT_SUBJCT(01)'
                             'X'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
*  ENDIF.

  PERFORM bdc_field       USING 'BDC_OKCODE'
                              'UPDA'.

ENDFORM.                    " CREA_SOCIEDAD_3
*&---------------------------------------------------------------------*
*&      Form  MODIF_SOCIEDAD_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM modif_sociedad_3 .

*Marta: 25.06.2008
*  IF lt_t_1-ktokk = 'ZRET'.
  IF lt_t_1-bu_group = 'ZTER'.
*Marta: 25.06.2008
    SELECT * UP TO 1 ROWS FROM t059z WHERE land1 = lt_t_1-pais AND
                                     wt_withcd = lt_t_1-wt_withcd.
    ENDSELECT.

    PERFORM bdc_dynpro      USING 'SAPMF02K' '0610'.

    PERFORM bdc_field       USING 'LFB1-QLAND'
                               lt_t_1-pais_r.

    PERFORM bdc_field       USING 'LFBW-WT_WITHCD(01)'
                               lt_t_1-wt_withcd.

    PERFORM bdc_field       USING 'LFBW-WITHT(01)'
                               t059z-witht.

    PERFORM bdc_field       USING 'LFBW-WT_SUBJCT(01)'
                               'X'.

    PERFORM bdc_dynpro      USING 'SAPMF02K' '0610'.

  ENDIF.

  PERFORM bdc_field       USING 'BDC_OKCODE'
                              'UPDA'.

ENDFORM.                    " MODIF_SOCIEDAD_3
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM call_transaction   USING l_tran
                             l_msgid
                             l_msgnr
                       CHANGING l_crea
                             l_mess
                             l_lifnr.
  DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
*       error session opened (' ' or 'X')
  DATA le_t100 LIKE t100.

  DATA lv_msgv1 LIKE messtab-msgv1.

  DATA l_mstring(480).

  DATA lv_ctumode LIKE ctu_params-dismode VALUE 'N'.
  "A: show all dynpros
  "E: show dynpro on error only
  "N: do not display dynpro

  REFRESH messtab.

  DATA opt TYPE ctu_params.

  opt-dismode = lv_ctumode.
  opt-defsize = 'X'.
*Ini JGOR 26.06.2018
*  opt-upmode = 'S'.
  opt-updmode = 'S'.
  opt-racommit = 'X'.
*Fin JGOR 26.06.2018
*  CALL TRANSACTION l_tran USING bdcdata
*                   MODE   lv_ctumode
*                   UPDATE 'S'
*                   MESSAGES INTO messtab.

  CALL TRANSACTION l_tran USING bdcdata
                 OPTIONS FROM opt
                 MESSAGES INTO messtab.

  READ TABLE messtab WITH KEY msgid = l_msgid
                              msgnr = l_msgnr .
* aayala 17.02.2012
  IF sy-subrc NE 0.
    READ TABLE messtab WITH KEY msgid = l_msgid
                               msgnr = '170' .
  ENDIF.
* aayala 17.02.2012
  IF sy-subrc = 0.

    l_crea = 1.
    l_lifnr = messtab-msgv1.
    gv_lifnr = l_lifnr.
    lt_t_1-partner = gv_lifnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = l_lifnr
      IMPORTING
        output = l_lifnr.
*** INICIO MODIFICACIÓN EMG 12/05/2009
    CLEAR: lt_log.
    MOVE-CORRESPONDING messtab TO lt_log.
    APPEND lt_log.
*** FIN MODIFICACIÓN EMG 12/05/2009

  ELSE.

    LOOP AT messtab WHERE msgid = 'F2' AND msgnr = '035'. " Sin modif.

    ENDLOOP.

    IF sy-subrc = 0.

      l_crea = 1.

    ENDIF.

    LOOP AT messtab WHERE msgtyp = 'E'.

*      EXIT.

    ENDLOOP.

    SELECT SINGLE * FROM t100 INTO le_t100
                              WHERE sprsl = messtab-msgspra
                              AND   arbgb = messtab-msgid
                              AND   msgnr = messtab-msgnr.
    IF sy-subrc = 0.
      l_mstring = le_t100-text.
      IF l_mstring CS '&1'.
        REPLACE '&1' WITH messtab-msgv1 INTO l_mstring.
        REPLACE '&2' WITH messtab-msgv2 INTO l_mstring.
        REPLACE '&3' WITH messtab-msgv3 INTO l_mstring.
        REPLACE '&4' WITH messtab-msgv4 INTO l_mstring.
      ELSE.
        REPLACE '&' WITH messtab-msgv1 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv2 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv3 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv4 INTO l_mstring.
      ENDIF.
      CONDENSE l_mstring.
    ELSE.
      MOVE messtab TO l_mstring.
    ENDIF.

    MOVE l_mstring TO l_mess.

  ENDIF.
  CLEAR lt_log.
*** INICIO MODIFICACIÓN EMG 12/05/2009
*  REFRESH lt_log.
*** FIN MODIFICACIÓN EMG 12/05/2009
  LOOP AT messtab WHERE msgtyp = 'E'.
    MOVE-CORRESPONDING messtab TO lt_log.
    APPEND lt_log.
  ENDLOOP.
*** INICIO MODIFICACIÓN EMG 12/05/2009
*  CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
*    TABLES
*      i_message_tab = lt_log.
*** FIN MODIFICACIÓN EMG 12/05/2009
ENDFORM.                    " CALL_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  CREA_ACREEDOR_BANK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM crea_acreedor_bank .


  IF lv_migr = 3.  " Migración 3

    PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'LFBK-BANKS(01)'
                                lt_t_1-land1.

    PERFORM bdc_field       USING 'LFBK-BANKL(01)'
                                lt_t_1-bankk.

    PERFORM bdc_field       USING 'LFBK-BANKN(01)'
                                lt_t_1-bankn.

    PERFORM bdc_field       USING 'LFBK-KOINH(01)'
                                'TIT'.

    PERFORM bdc_field       USING 'LFBK-BKONT(01)'
                                lt_t_1-bkont.

    IF NOT lt_t_1-iban01 IS INITIAL.

      PERFORM bdc_field       USING 'BDC_OKCODE'
                                 'IBAN'.

      PERFORM bdc_dynpro      USING 'SAPLIBMA' '0100'.

      PERFORM bdc_field       USING 'IBAN01'
                                 lt_t_1-iban01.

      PERFORM bdc_field       USING 'IBAN02'
                                 lt_t_1-iban02.

      PERFORM bdc_field       USING 'IBAN03'
                                 lt_t_1-iban03.

      PERFORM bdc_field       USING 'IBAN04'
                                 lt_t_1-iban04.

      PERFORM bdc_field       USING 'IBAN05'
                                 lt_t_1-iban05.

      PERFORM bdc_field       USING 'IBAN06'
                                 lt_t_1-iban06.

      PERFORM bdc_field       USING 'IBAN07'
                                 lt_t_1-iban07.

      PERFORM bdc_field       USING 'IBAN08'
                                 lt_t_1-iban08.

      PERFORM bdc_field       USING 'IBAN09'
                                 lt_t_1-iban09.

*   perform bdc_field       using 'TIBAN-VALID_FROM'
*                              LT_T_1-VALID_FROM.

    ENDIF.

*   perform bdc_dynpro      using 'SAPMF02K' '0130'.

    PERFORM bdc_field       USING 'BDC_OKCODE'
                               'BANK'.

    PERFORM bdc_dynpro      USING 'SAPLBANK' '0100'.

    PERFORM bdc_field       USING 'BNKA-BANKA'
                               'OBLIG'.

    PERFORM bdc_field       USING 'BNKA-SWIFT'
                               ''.
    PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.

  ELSE.

    PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.

    PERFORM bdc_field       USING 'BDC_OKCODE'
                               'ENTR'.

  ENDIF.
ENDFORM.                    " CREA_ACREEDOR_BANK
*&---------------------------------------------------------------------*
*&      Form  CREA_SOCIEDAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM crea_sociedad  CHANGING l_aprob
                             l_mess.

  DATA lv_lifnr TYPE lifnr.
  DATA lv_quest TYPE string.
  DATA lv_conf(1).

  DATA lv_subrc TYPE i.

  SELECT SINGLE * FROM lfb1 WHERE bukrs = lt_t_1-bukrs AND
                                  lifnr = lt_t_1-partner.

  lv_subrc = sy-subrc.
  IF lv_subrc = 0 AND lv_migr IS INITIAL..

*    MOVE text-017 TO lv_quest.
*    REPLACE '&1' WITH lt_t_1-bukrs INTO lv_quest.
*    REPLACE '&2' WITH lt_t_1-lifnr INTO lv_quest.
*
*    CALL FUNCTION 'POPUP_TO_CONFIRM'
*      EXPORTING
*        titlebar              = text-015
*        text_question         = lv_quest
*        default_button        = '1'
*        display_cancel_button = ''
*        start_column          = 25
*        start_row             = 6
*      IMPORTING
*        answer                = lv_conf.
*
*    IF lv_conf = '2'.
*      .
** Proceso continua
*      l_aprob = 1.
*
*    ELSE.

    MOVE TEXT-018 TO lv_quest.
    REPLACE '&1' WITH lt_t_1-bukrs INTO lv_quest.
    REPLACE '&2' WITH lt_t_1-partner INTO lv_quest.
    l_mess = lv_quest.
    MESSAGE l_mess TYPE 'I'.
*    ENDIF.

    EXIT.

  ENDIF.

  IF lv_subrc = 0 AND NOT lv_migr IS INITIAL.

    l_aprob = 1.
    EXIT.

  ENDIF.
*ini seccion comentada porque no aplica para mexico rcd aayala 30.11.2011
*  SELECT SINGLE * FROM lfb1 WHERE bukrs = '5000' AND
*                                  lifnr = lt_t_1-lifnr.
*
*  lv_subrc = sy-subrc.
*  IF lv_subrc <> 0.
*    REFRESH bdcdata.
*
*    PERFORM crea_sociedad_5000.
*
**Marta: 26.06.2008
**Que solo cree retenciones en la sociedad 5000, cuando hablemos sociedades del mismo pais de esta
**o sea sociedades españolas
*    DATA: va_land1 TYPE t001-land1.
*
*    SELECT SINGLE land1
*             FROM t001
*             INTO va_land1
*            WHERE bukrs EQ lt_t_1-bukrs.
*
*    IF va_land1 EQ 'ES'.
*
*      PERFORM crea_sociedad_3.   "ZRET
*
*    ENDIF.
**Marta: 26.06.2008
*
*    PERFORM call_transaction  USING 'FK01'
*                                   'F2'
*                                   '271'
*                             CHANGING l_aprob
*                                      l_mess
*                                      lv_lifnr.
*
*  ENDIF.
*ini seccion comentada porque no aplica para mexico rcd aayala 30.11.2011
  REFRESH bdcdata.

  PERFORM crea_sociedad_1.

  PERFORM crea_sociedad_3.   "ZRET


*  SELECT SINGLE stcd1
*    FROM lfa1
*    WHERE stcd1 = @lt_t_1-cif
*    INTO @DATA(stcd1).

*  IF sy-subrc IS INITIAL.
  PERFORM call_transaction  USING 'FK01'
                                 'F2'
                                 '271'
                           CHANGING l_aprob
                                    l_mess
                                    lv_lifnr.
*  ENDIF.

ENDFORM.                    " CREA_SOCIEDAD
*&---------------------------------------------------------------------*
*&      Form  MODIF_SOCIEDAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM modif_sociedad  CHANGING l_aprob
                             l_mess.

  DATA lv_lifnr TYPE lifnr.

  REFRESH bdcdata.

  PERFORM modif_sociedad_1 USING lt_t_1-partner.

  PERFORM crea_sociedad_3.   "ZRET

  PERFORM call_transaction  USING 'FK02'
                                 'F2'
                                 '271'
                           CHANGING l_aprob
                                    l_mess
                                    lv_lifnr.

ENDFORM.                    " MODIF_SOCIEDAD
*&---------------------------------------------------------------------*
*&      Form  CREA_SOCIEDAD_MOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM crea_sociedad_mod  USING VALUE(l_bukrs)
                    CHANGING l_aprob
                             l_mess.

  DATA lv_lifnr LIKE zfitprov-partner.

  REFRESH bdcdata.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0105'.


  PERFORM bdc_field       USING 'RF02K-BUKRS'
                              lt_t_1-bukrs.

  PERFORM bdc_field       USING 'RF02K-LIFNR'
                              lt_t_1-partner.

  PERFORM bdc_field       USING 'RF02K-REF_BUKRS'
                              l_bukrs.

  PERFORM bdc_field       USING 'RF02K-REF_LIFNR'
                              lt_t_1-partner.

*************************************************************
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0210'.

  IF lt_t_1-bu_group = 'ZTER'.

    PERFORM bdc_dynpro      USING 'SAPMF02K' '0215'.

    PERFORM bdc_dynpro      USING 'SAPMF02K' '0220'.

    SELECT * UP TO 1 ROWS FROM t059z WHERE land1 = lt_t_1-pais AND
                                    wt_withcd = lt_t_1-wt_withcd.
    ENDSELECT.

    PERFORM bdc_dynpro      USING 'SAPMF02K' '0610'.

    PERFORM bdc_field       USING 'LFB1-QLAND'
                              lt_t_1-pais_r.

    PERFORM bdc_field       USING 'LFBW-WT_WITHCD(01)'
                              lt_t_1-wt_withcd.

    PERFORM bdc_field       USING 'LFBW-WITHT(01)'
                              t059z-witht.

    PERFORM bdc_field       USING 'LFBW-WT_SUBJCT(01)'
                              'X'.

  ENDIF.

  PERFORM bdc_field       USING 'BDC_OKCODE'
                              'UPDA'.

  PERFORM call_transaction  USING 'FK01'
                                 'F2'
                                 '271'
                           CHANGING l_aprob
                                    l_mess
                                    lv_lifnr.

ENDFORM.                    " CREA_SOCIEDAD_MOD
*&---------------------------------------------------------------------*
*&      Form  CREA_ACREEDOR_3_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM crea_acreedor_3_ .

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0380'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                'ENTR'.
  PERFORM bdc_field       USING 'KNVK-NAME1(01)'
                              lt_t_1-dzsabe_k.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0380'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              'ENTR'.

ENDFORM.                    " CREA_ACREEDOR_3_
*&---------------------------------------------------------------------*
*&      Form  CREA_ACREEDOR_COMPRAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM crea_acreedor_compras .

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0310'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                'ENTR'.
  PERFORM bdc_field       USING 'LFM1-WAERS'
                              lt_t_1-waers.

  PERFORM bdc_field       USING 'LFM1-ZTERM'
                              lt_t_1-zterm.

  PERFORM bdc_field       USING 'LFM1-WEBRE'
                              'X'.

  PERFORM bdc_field       USING 'LFM1-BOLRE'
                              'X'.

  PERFORM bdc_field       USING 'LFM1-BOIND'
                              'X'.

  PERFORM bdc_field       USING 'LFM1-UMSAE'
                              'X'.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0320'.

  PERFORM bdc_field       USING 'WYT3-PARVW(01)'
                              'DP'."'PR'."'DP'. JGOR 20.07.2017

  PERFORM bdc_field       USING 'WRF02K-GPARN(01)'
                              'INTERNO'.

  PERFORM bdc_field       USING 'WYT3-PARVW(02)'
                              'PR'."'DP'."'PR'. JGOR 20.07.2017
  PERFORM bdc_field       USING 'WRF02K-GPARN(02)'
                              'INTERNO'.

  PERFORM bdc_field       USING 'WYT3-PARVW(03)'
                              'EF'.
  PERFORM bdc_field       USING 'WRF02K-GPARN(03)'
                              'INTERNO'.

ENDFORM.                    " CREA_ACREEDOR_COMPRAS
*&---------------------------------------------------------------------*
*&      Form  CREA_SOCIEDAD_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM crea_sociedad_1.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0105'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.

  PERFORM bdc_field       USING 'RF02K-BUKRS'
                              lt_t_1-bukrs.

  PERFORM bdc_field       USING 'RF02K-LIFNR'
                              lt_t_1-partner.

  PERFORM bdc_field       USING 'RF02K-KTOKK'
                              lt_t_1-bu_group.
*************************************************************
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0210'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
  PERFORM bdc_field       USING 'LFB1-AKONT'
                              lt_t_1-akont.

*  IF LT_T_1-KTOKK = 'ZTGR'.

  PERFORM bdc_field       USING 'LFB1-FDGRV'
                              lt_t_1-fdgrv.

*  ENDIF.

**************************************************************
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0215'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.

  PERFORM bdc_field USING 'LFB1-REPRF' 'X'."aayala 30.11.2011

  PERFORM bdc_field       USING 'LFB1-ZTERM'
                              lt_t_1-zterm.

  PERFORM bdc_field       USING 'LFB1-ZWELS'
                              lt_t_1-zwels.

  IF NOT lt_t_1-bloqj IS INITIAL.

    PERFORM bdc_field       USING 'LFB1-ZAHLS'
                                  'J'.

  ENDIF.

ENDFORM.                    " CREA_SOCIEDAD_1
*&---------------------------------------------------------------------*
*&      Form  MODIF_SOCIEDAD_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM modif_sociedad_1 USING l_lifnr.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0106'.

  PERFORM bdc_field       USING 'RF02K-BUKRS'
                              lt_t_1-bukrs.

  PERFORM bdc_field       USING 'RF02K-LIFNR'
                              l_lifnr.

  PERFORM bdc_field       USING 'RF02K-D0210'
                              'X'.
  PERFORM bdc_field       USING 'RF02K-D0215'
                              'X'.
  PERFORM bdc_field       USING 'RF02K-D0220'
                              'X'.
  PERFORM bdc_field       USING 'RF02K-D0610'
                              'X'.

*************************************************************
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0210'.

  IF NOT lt_t_1-akont IS INITIAL.
    PERFORM bdc_field       USING 'LFB1-AKONT'
                                lt_t_1-akont.
  ENDIF.

**************************************************************
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0215'.
  PERFORM bdc_field USING 'LFB1-REPRF' 'X'."aayala 30.11.2011
  IF NOT lt_t_1-zterm IS INITIAL.
    PERFORM bdc_field       USING 'LFB1-ZTERM'
                              lt_t_1-zterm.
  ENDIF.

  IF NOT lt_t_1-zwels IS INITIAL.
    PERFORM bdc_field       USING 'LFB1-ZWELS'
                              lt_t_1-zwels.
  ENDIF.

ENDFORM.                    " MODIF_SOCIEDAD_1
*&---------------------------------------------------------------------*
*&      Form  TR_BLOQ_SOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_bloq_soc USING VALUE(l_index)
                   CHANGING l_aprob.

  DATA lv_mess(100).
  DATA lv_lifnr LIKE zfitprov-partner.

  REFRESH bdcdata.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0505'.

  PERFORM bdc_field       USING 'RF02K-BUKRS'
                              lt_t_1-bukrs.

  PERFORM bdc_field       USING 'RF02K-LIFNR'
                              lt_t_1-partner.

*************************************************************
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0510'.

  PERFORM bdc_field       USING 'LFB1-SPERR'
                              'X'.

  PERFORM bdc_field       USING 'BDC_OKCODE'
                              'UPDA'.

  PERFORM call_transaction  USING 'FK05'
                                 'F2'
                                 '056'
                           CHANGING l_aprob
                                    lv_mess
                                    lv_lifnr.

  IF l_aprob IS INITIAL.

    lt_t_1-error = lv_mess.
    MODIFY lt_t_1 INDEX l_index.

  ENDIF.

ENDFORM.                    " TR_BLOQ_SOC
*&---------------------------------------------------------------------*
*&      Form  TR_APROB_CONF_MODIF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_aprob_conf_modif .

  DATA: lv_index TYPE i.
  DATA: lv_aprob TYPE i.

  DATA: BEGIN OF lt_bukrs OCCURS 0,
          bukrs LIKE zfitprov-bukrs,
        END OF lt_bukrs.

  DATA lv_bukrs LIKE zfitprov-bukrs.

  PERFORM ini_cod_int.

* Solicitudes seleccionados
  LOOP AT lt_t_1 WHERE  sele = 'X' AND migr EQ 2.

    lv_index = sy-tabix.
    lv_aprob = 0.
    lv_tot = lv_tot + 1.

    CLEAR zfitprovnav.
    SELECT * UP TO 1 ROWS FROM zfitprovnav WHERE zzprovenl = lt_t_1-zzprovenl.
    ENDSELECT.

    lt_t_1-partner = zfitprovnav-lifnr.
    PERFORM migr_soc_modif USING lv_index
                          CHANGING lv_aprob.

    IF NOT lt_t_1-bloq IS INITIAL AND lv_aprob IS INITIAL.
* Bloqueo Sociedad
      CLEAR lv_aprob.
      PERFORM tr_bloq_soc USING lv_index CHANGING lv_aprob.
    ENDIF.

    IF lv_aprob IS INITIAL.

      PERFORM tr_grabar_sol USING lv_index.

    ELSE.

      PERFORM tr_aprob_sol_act USING lv_index.
      lv_totac = lv_totac + 1.

    ENDIF.

  ENDLOOP.

  PERFORM fin_cod_int.

ENDFORM.                    " TR_APROB_CONF_MODIF
*&---------------------------------------------------------------------*
*&      Form  TR_ACT_CALLE2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_tr_act_calle2 USING l_lifnr.


  DATA: lv_addr_sel  LIKE  addr1_sel,
        lv_sadr      LIKE  sadr,
        lv_addr_data LIKE  addr1_data.

  DATA: lv_returncode       LIKE  szad_field-returncode,
        lv_data_has_changed,
        lt_error_table      LIKE addr_error OCCURS 0.

  DATA: BEGIN OF lt_smtp OCCURS 0.
          INCLUDE STRUCTURE adsmtp.
  DATA: END OF lt_smtp.

  SELECT SINGLE * FROM lfa1 WHERE lifnr = l_lifnr.

  lv_addr_sel-addrnumber = lfa1-adrnr.

  CALL FUNCTION 'ADDR_GET'
    EXPORTING
      address_selection = lv_addr_sel
    IMPORTING
      sadr              = lv_sadr
    EXCEPTIONS
      parameter_error   = 1
      address_not_exist = 2
      version_not_exist = 3
      internal_error    = 4
      OTHERS            = 5.
  IF sy-subrc <> 0.
    MESSAGE i209(zfi01) WITH l_lifnr.
    EXIT.
  ENDIF.

  lv_addr_data-name1 = lv_sadr-name1.
  lv_addr_data-name2 = lv_sadr-name2.
  lv_addr_data-city1 = lv_sadr-ort01.
  lv_addr_data-post_code1 = lv_sadr-pstlz.
  lv_addr_data-country = lv_sadr-land1.
  lv_addr_data-langu = lv_sadr-spras.
  lv_addr_data-region = lv_sadr-regio.
  lv_addr_data-street = lt_t_1-direc.
  lv_addr_data-sort1 = lv_sadr-sortl.

  lv_addr_data-str_suppl1 = lt_t_1-direc_2.

  lv_addr_data-date_from = '10101'.
  lv_addr_data-date_to = '99991231'.
  lv_addr_data-time_zone = 'CET'.
  lv_addr_data-langu_crea = lv_sadr-spras.

  CALL FUNCTION 'ADDR_UPDATE'
    EXPORTING
      address_data      = lv_addr_data
      address_number    = lfa1-adrnr
    IMPORTING
      address_data      = lv_addr_data
      returncode        = lv_returncode
      data_has_changed  = lv_data_has_changed
    TABLES
      error_table       = lt_error_table
    EXCEPTIONS
      address_not_exist = 1
      parameter_error   = 2
      version_not_exist = 3
      internal_error    = 4
      OTHERS            = 5.

  IF sy-subrc <> 0.
    MESSAGE i209(zfi01) WITH l_lifnr.
    EXIT.
  ENDIF.


  lt_smtp-smtp_addr = lt_t_1-smtp.
  lt_smtp-updateflag = 'I'.
  APPEND lt_smtp.

  REFRESH lt_error_table.

  CALL FUNCTION 'ADDR_COMM_MAINTAIN'
    EXPORTING
      address_number     = lfa1-adrnr
      table_type         = 'ADSMTP'
      iv_time_dependence = 'X'
    IMPORTING
      returncode         = lv_returncode
    TABLES
      comm_table         = lt_smtp
      error_table        = lt_error_table
    EXCEPTIONS
      parameter_error    = 1
      address_not_exist  = 2
      internal_error     = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
    MESSAGE i209(zfi01) WITH l_lifnr.
    EXIT.
  ENDIF.

  CALL FUNCTION 'ADDR_MEMORY_SAVE'
    EXPORTING
      execute_in_update_task = ' '
    EXCEPTIONS
      address_number_missing = 1
      person_number_missing  = 2
      internal_error         = 3
      database_error         = 4
      reference_missing      = 5
      OTHERS                 = 6.
  IF sy-subrc <> 0.
    MESSAGE i209(zfi01) WITH l_lifnr.
    EXIT.
  ENDIF.

ENDFORM.                    " TR_TR_ACT_CALLE2
*&---------------------------------------------------------------------*
*&      Form  TEXTO_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM texto_error  USING    l_edat
                           l_txto
                  CHANGING l_txt.

  DATA le_dd04t LIKE dd04t.

  SELECT * UP TO 1 ROWS FROM dd04t INTO le_dd04t
                            WHERE rollname = l_edat AND
*Marta: 27.11.2008 - inicio modificación - texto lengua del sistema
*                                  ddlanguage = 'SY-LANGU' AND
                                  ddlanguage = sy-langu AND
*Marta: 27.11.2008 - fin modificación
                                  as4local = 'A'
                        ORDER BY as4vers DESCENDING.
  ENDSELECT.

  l_txt = l_txto.
  REPLACE '&1' WITH le_dd04t-scrtext_m INTO l_txt.

ENDFORM.                    " TEXTO_ERROR
*&---------------------------------------------------------------------*
*&      Form  TEXTO_ERROR_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM texto_error_2  USING    l_edat l_gr
                             l_txto
                    CHANGING l_txt.

  DATA le_dd04t LIKE dd04t.

  l_txt = l_txto.


  SELECT * UP TO 1 ROWS FROM dd04t INTO le_dd04t
                            WHERE rollname = l_edat AND
*Marta: 27.11.2008 - inicio modificación - mensajes con la lengua que toca
*                                  ddlanguage = 'SY-LANGU' AND
                                  ddlanguage = sy-langu AND
*Marta: 27.11.2008 - fin modificación
                                  as4local = 'A'
                        ORDER BY as4vers DESCENDING.
  ENDSELECT.

  REPLACE '&1' WITH le_dd04t-scrtext_m INTO l_txt.

  REPLACE '&2' WITH l_gr INTO l_txt.

ENDFORM.                    " TEXTO_ERROR_2
*&---------------------------------------------------------------------*
*&      Form  CHECK_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_bukrs  TABLES   t_bukrs
                  USING    l_akont
                  CHANGING l_fail
                           l_txt.

  DATA le_skb1 LIKE skb1.

  LOOP AT t_bukrs.

    SELECT SINGLE * FROM skb1 INTO le_skb1 WHERE bukrs = t_bukrs AND "#EC CI_DB_OPERATION_OK[2431747] " DTT ATC CORRECCION – 04/08/2026
                                                 saknr = lt_t_1-akont.
    IF NOT sy-subrc IS INITIAL.

      l_fail = 1.
      MOVE TEXT-023 TO l_txt.
      REPLACE '&1' WITH t_bukrs INTO l_txt.
      REPLACE '&2' WITH lt_t_1-akont INTO l_txt.
      EXIT.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " CHECK_BUKRS
*&---------------------------------------------------------------------*
*&      Form  VERIF_RECH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verif_rech .

  LOOP AT lt_t_1 WHERE NOT sele IS INITIAL.

    IF lt_t_1-comnt_c IS INITIAL.

      lv_verif = sy-tabix.
      PERFORM texto_error USING 'ZZECOMC' TEXT-027 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " VERIF_RECH


*&---------------------------------------------------------------------*
*&      Form  llamar_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM llamar_dynpro .
  CLEAR: r_bukrs_aut, r_bukrs_naut.
  REFRESH: r_bukrs_aut, r_bukrs_naut.
  IF sy-uname <> 'DLOPEZPEREZ'. "DELETE
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
  CALL SCREEN '9001'.


ENDFORM.                    " llamar_dynpro
*&---------------------------------------------------------------------*
*&      Form  HELP_KNB1_ZWELS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM help_knb1_zwels .
  CALL SCREEN 1215 STARTING AT 03 01 ENDING AT 80 25.
ENDFORM.                    " HELP_KNB1_ZWELS
*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EXCTAB  text
*      -->P_0376   text
*      -->P_0377   text
*----------------------------------------------------------------------*
FORM set_pf_status TABLES   p_exctab
                   USING    p_pfkey     LIKE sy-pfkey
                            p_withexcl.

* Ausnahmen Betriebestamm
  PERFORM exceptions_site TABLES p_exctab
                          USING p_pfkey.

  IF p_withexcl IS INITIAL.
    SET PF-STATUS p_pfkey OF PROGRAM 'ZFI0009'.
  ELSE.
    SET PF-STATUS p_pfkey OF PROGRAM 'ZFI0009'
                          EXCLUDING p_exctab.
  ENDIF.
ENDFORM.                    " set_pf_status
*&---------------------------------------------------------------------*
*&      Form  EXCEPTIONS_SITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_EXCTAB  text
*      -->P_P_PFKEY  text
*----------------------------------------------------------------------*
FORM exceptions_site TABLES   p_exctab
                     USING    p_pfkey  LIKE sy-pfkey.

  DATA: s_exctab LIKE tabstrip_extab OCCURS 10 WITH HEADER LINE.
  DATA: s_pfkey LIKE sy-pfkey.

  CHECK NOT debi_call IS INITIAL.
  s_exctab[] = p_exctab[].

  s_pfkey = p_pfkey.
  IF s_pfkey(3) = '340'.
    READ TABLE s_exctab WITH KEY okcode = 'ABTE'.
    IF sy-subrc NE 0.
      s_exctab-okcode = 'ABTE'. APPEND s_exctab.
    ENDIF.

    READ TABLE s_exctab WITH KEY okcode = 'EMPF'.
    IF sy-subrc NE 0.
      s_exctab-okcode = 'EMPF'. APPEND s_exctab.
    ENDIF.
  ENDIF.

  p_exctab[] = s_exctab[].
ENDFORM.                    " EXCEPTIONS_SITE
*&---------------------------------------------------------------------*
*&      Form  FENSTER_BLAETTERN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fenster_blaettern .
  CASE ok-code.
    WHEN 'P-- '.
      CLEAR ok-code.
      index = 1.
      SET SCREEN sy-dynnr.
      LEAVE SCREEN.
    WHEN 'P-  '.
      CLEAR ok-code.
      index = index - loopc.
      IF index LT 1.
        index = 1.
      ENDIF.
      SET SCREEN sy-dynnr.
      LEAVE SCREEN.
    WHEN 'P+  '.
      CLEAR ok-code.
      index = index + loopc.
      IF index GT tfill.
        index = index - loopc.
      ENDIF.
      SET SCREEN sy-dynnr.
      LEAVE SCREEN.
    WHEN 'P++ '.
      CLEAR ok-code.
      IF tfill LE loopc.
        index = 1.
      ELSE.
        index = tfill - loopc + 1.
      ENDIF.
      SET SCREEN sy-dynnr.
      LEAVE SCREEN.
    WHEN OTHERS.
      CLEAR ok-code.
  ENDCASE.
ENDFORM.                    " FENSTER_BLAETTERN
*&---------------------------------------------------------------------*
*&      Module  OKCODE_ENTER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE okcode_enter INPUT.
  IF ok-code = 'ENTR'.
    CLEAR ok-code.
  ENDIF.
ENDMODULE.                 " OKCODE_ENTER  INPUT
*&---------------------------------------------------------------------*
*&      Form  crea_sociedad_5000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM crea_sociedad_5000 .
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0105'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.

  PERFORM bdc_field       USING 'RF02K-BUKRS'
                              '5000'.

  PERFORM bdc_field       USING 'RF02K-LIFNR'
                              lt_t_1-partner.

  PERFORM bdc_field       USING 'RF02K-KTOKK'
                              lt_t_1-bu_group.
*************************************************************
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0210'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
  PERFORM bdc_field       USING 'LFB1-AKONT'
                              lt_t_1-akont.

*  IF LT_T_1-KTOKK = 'ZTGR'.

  PERFORM bdc_field       USING 'LFB1-FDGRV'
                              lt_t_1-fdgrv.

*  ENDIF.

**************************************************************
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0215'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
  PERFORM bdc_field       USING 'LFB1-ZTERM'
                              lt_t_1-zterm.

  PERFORM bdc_field       USING 'LFB1-ZWELS'
                              lt_t_1-zwels.
  PERFORM bdc_field USING 'LFB1-REPRF' 'X'."aayala 30.11.2011
  IF NOT lt_t_1-bloqj IS INITIAL.

    PERFORM bdc_field       USING 'LFB1-ZAHLS'
                                  'J'.

  ENDIF.

ENDFORM.                    " crea_sociedad_5000
*&---------------------------------------------------------------------*
*&      Form  crea_acreedor_8300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_APROB  text
*      <--P_LV_MESS  text
*      <--P_LV_LIFNR  text
*----------------------------------------------------------------------*
FORM crea_acreedor_8300  CHANGING l_crea
                                  l_mess
                                  l_lifnr.

  DATA lv_mes(3) VALUE '170'.

  REFRESH bdcdata.

  PERFORM crea_acreedor_8300_1.  "DATOS GENERALES

  PERFORM crea_acreedor_8300_2.  "CIF

  PERFORM crea_acreedor_8300_bank.    "DATOS BANCO

  PERFORM crea_acreedor_8300_3.  "CONTACTO


  PERFORM bdc_field       USING 'BDC_OKCODE'
                              'UPDA'.

  PERFORM call_transaction  USING 'FK01'
                                 'F2'
                                  lv_mes
                           CHANGING l_crea
                                    l_mess
                                    l_lifnr.

ENDFORM.                    " crea_acreedor_8300
*&---------------------------------------------------------------------*
*&      Form  crea_acreedor_8300_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM crea_acreedor_8300_1 .

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0105'.

  PERFORM bdc_field       USING 'RF02K-BUKRS'
                                lt_t_1-bukrs.
  PERFORM bdc_field       USING 'RF02K-KTOKK'
                                lt_t_1-bu_group.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.


* Datos generales
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0110'.

  PERFORM bdc_field       USING 'LFA1-NAME1'
                              lt_t_1-name1(35).
  PERFORM bdc_field       USING 'LFA1-NAME2'
                              lt_t_1-name2(35).
  PERFORM bdc_field       USING 'LFA1-SORTL'
                              lt_t_1-busq.
  PERFORM bdc_field       USING 'LFA1-STRAS'
                              lt_t_1-direc(35).
  PERFORM bdc_field       USING 'LFA1-ORT01'
                              lt_t_1-poblac.
  PERFORM bdc_field       USING 'LFA1_PSTLZ'
                              lt_t_1-cod_post.
  PERFORM bdc_field       USING 'LFA1-REGIO'
                              lt_t_1-region.
  PERFORM bdc_field       USING 'LFA1-LAND1'
                              lt_t_1-pais.
  PERFORM bdc_field       USING 'LFA1-SPRAS'
                              lt_t_1-spras.
  PERFORM bdc_field       USING 'LFA1-TELF1'
                              lt_t_1-tel(27).
  PERFORM bdc_field       USING 'LFA1-TELX1'
                              lt_t_1-fax(27).
  PERFORM bdc_field       USING 'LFA1-LFURL'
                              lt_t_1-smtp.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                               '/00'.

ENDFORM.                    " crea_acreedor_8300_1
*&---------------------------------------------------------------------*
*&      Form  crea_acreedor_8300_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM crea_acreedor_8300_2 .

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0120'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
  IF lt_t_1-bu_group = '2060'.

    PERFORM bdc_field       USING 'LFA1-VBUND'
                                lt_t_1-rcomp.

  ENDIF.
  PERFORM bdc_field       USING 'LFA1-KONZS'
                              lt_t_1-c_fisc_mx.
*** INICIO MODIFICACIÓN EMG 12/05/2009
*  IF lt_t_1-ktokk EQ 'ZTGR' OR
*     lt_t_1-ktokk EQ 'ZTER'.se elimina para rcd resorts aayala 30.11.2011
  PERFORM bdc_field       USING 'LFA1-BRSCH' lt_t_1-brsch.
*  ENDIF.se elimina para rcd resorts aayala 30.11.2011
*** FIN MODIFICACIÓN EMG 12/05/2009
  PERFORM bdc_field       USING 'LFA1-STCD1'
                              lt_t_1-cif.

ENDFORM.                    " crea_acreedor_8300_2
*&---------------------------------------------------------------------*
*&      Form  crea_acreedor_8300_bank
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM crea_acreedor_8300_bank .

  IF lv_migr = 3.  " Migración 3

    PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'LFBK-BANKS(01)'
                                lt_t_1-land1.

    PERFORM bdc_field       USING 'LFBK-BANKL(01)'
                                lt_t_1-bankk.

    PERFORM bdc_field       USING 'LFBK-BANKN(01)'
                                lt_t_1-bankn.

    PERFORM bdc_field       USING 'LFBK-KOINH(01)'
                                'TIT'.

    PERFORM bdc_field       USING 'LFBK-BKONT(01)'
                                lt_t_1-bkont.

    IF NOT lt_t_1-iban01 IS INITIAL.

      PERFORM bdc_field       USING 'BDC_OKCODE'
                                 'IBAN'.

      PERFORM bdc_dynpro      USING 'SAPLIBMA' '0100'.

      PERFORM bdc_field       USING 'IBAN01'
                                 lt_t_1-iban01.

      PERFORM bdc_field       USING 'IBAN02'
                                 lt_t_1-iban02.

      PERFORM bdc_field       USING 'IBAN03'
                                 lt_t_1-iban03.

      PERFORM bdc_field       USING 'IBAN04'
                                 lt_t_1-iban04.

      PERFORM bdc_field       USING 'IBAN05'
                                 lt_t_1-iban05.

      PERFORM bdc_field       USING 'IBAN06'
                                 lt_t_1-iban06.

      PERFORM bdc_field       USING 'IBAN07'
                                 lt_t_1-iban07.

      PERFORM bdc_field       USING 'IBAN08'
                                 lt_t_1-iban08.

      PERFORM bdc_field       USING 'IBAN09'
                                 lt_t_1-iban09.

*   perform bdc_field       using 'TIBAN-VALID_FROM'
*                              LT_T_1-VALID_FROM.

    ENDIF.

*   perform bdc_dynpro      using 'SAPMF02K' '0130'.

    PERFORM bdc_field       USING 'BDC_OKCODE'
                               'BANK'.

    PERFORM bdc_dynpro      USING 'SAPLBANK' '0100'.

    PERFORM bdc_field       USING 'BNKA-BANKA'
                               'OBLIG'.

    PERFORM bdc_field       USING 'BNKA-SWIFT'
                               ''.
    PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.

  ELSE.

    PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.

    PERFORM bdc_field       USING 'BDC_OKCODE'
                               'ENTR'.

  ENDIF.

ENDFORM.                    " crea_acreedor_8300_bank
*&---------------------------------------------------------------------*
*&      Form  crea_acreedor_8300_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM crea_acreedor_8300_3 .

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0380'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                'ENTR'.
  PERFORM bdc_field       USING 'KNVK-NAME1(01)'
                              lt_t_1-dzsabe_k.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0380'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              'ENTR'.

ENDFORM.                    " crea_acreedor_8300_3
*&---------------------------------------------------------------------*
*&      Form  crea_sociedad_8300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_APROB  text
*      <--P_LV_MESS  text
*----------------------------------------------------------------------*
FORM crea_sociedad_8300  CHANGING l_aprob
                                  l_mess.


  DATA lv_lifnr TYPE lifnr.

*Marta Vall: 16.10.2008 - inico modificación - Crear todas las sociedad
*grupo de sincronización

  DATA:
    BEGIN OF ls_bukrs,
      bukrs LIKE zfitprov-bukrs,
    END OF ls_bukrs.

  DATA: lt_bukrs LIKE ls_bukrs OCCURS 0 WITH HEADER LINE.

*Marta Vall: 16.10.2008 - fin modifiación

  REFRESH lt_bukrs.

  PERFORM acces_gr_sync TABLES lt_bukrs.

  REFRESH bdcdata.

  PERFORM crea_sociedad_1_8300.

  PERFORM crea_sociedad_3.   "ZRET

  PERFORM call_transaction  USING 'FK01'
                                 'F2'
                                 '271'
                           CHANGING l_aprob
                                    l_mess
                                    lv_lifnr.

  PERFORM actualiza_mail USING lv_lifnr lt_t_1-smtp. "ALML

*Marta: 27.11.2008 - inicio modificación
*  PERFORM tr_tr_act_calle2 USING lv_lifnr."aayala
*Marta: 27.11.2008 - fin modifiación


****Marta Vall: 16.10.2008 - inicio modficación
***  IF lt_t_1-pais EQ 'GB'.
***
***
***    LOOP AT lt_bukrs.
***      IF lt_bukrs-bukrs NE lt_t_1-bukrs.
***
***        REFRESH bdcdata.
***
***        PERFORM crear_sociedad_sincronizacion USING lv_lifnr lt_bukrs-bukrs.
***
***        PERFORM crea_sociedad_3.   "ZRET
***
***        PERFORM call_transaction  USING 'FK01'
***                                       'F2'
***                                       '271'
***                                 CHANGING l_aprob
***                                          l_mess
***                                          lv_lifnr.
***      ENDIF.
***
***
***    ENDLOOP.
***
****Crear el mismo proveedor para la sociedad 5000, maestra
***  ENDIF.
****Marta Vall: 16.10.2008 - fin modificación
***

*ini se comento bloque ya que no es necesario crearlo en la sociedad 5000 aayala
*  REFRESH bdcdata.
*
**Marta Vall: 15.10.2008 - inicio modificación - usar el proveedor que ya se ha creado
*  PERFORM crear_sociedad_1_5000 USING lv_lifnr.
**Marta Vall: 15.10.2008 - fin modificación
*
*  PERFORM crea_sociedad_3.
*
*  PERFORM call_transaction  USING 'FK01'
*                                 'F2'
*                                 '271'
*                           CHANGING l_aprob
*                                    l_mess
*                                    lv_lifnr.
*fin se comento bloque ya que no es necesario crearlo en la sociedad 5000 aayala
ENDFORM.                    " crea_sociedad_8300
*&---------------------------------------------------------------------*
*&      Form  crea_sociedad_1_8300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM crea_sociedad_1_8300 .
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0105'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.

  PERFORM bdc_field       USING 'RF02K-BUKRS'
                              lt_t_1-bukrs.

*  PERFORM bdc_field       USING 'RF02K-LIFNR'
*                              lt_t_1-lifnr.

  PERFORM bdc_field       USING 'RF02K-KTOKK'
                              lt_t_1-bu_group.
*************************************************************
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0110'.

  PERFORM bdc_field       USING 'LFA1-NAME1'
*** INICIO MODIFICACIÓN EMG 10/12/2008
*                              lt_t_1-name1.
                              lt_t_1-name1(35).
*** FIN MODIFICACIÓN EMG 10/12/2008
  PERFORM bdc_field       USING 'LFA1-SORTL'
                              lt_t_1-busq.
  PERFORM bdc_field       USING 'LFA1-NAME2'
                              lt_t_1-name2.
  PERFORM bdc_field       USING 'LFA1-STRAS'
                              lt_t_1-direc.
  PERFORM bdc_field       USING 'LFA1-ORT01'
                              lt_t_1-poblac.
  PERFORM bdc_field       USING 'LFA1-PSTLZ'
                              lt_t_1-cod_post.
  PERFORM bdc_field       USING 'LFA1-LAND1'
                              lt_t_1-pais.
  PERFORM bdc_field       USING 'LFA1-REGIO'
                              lt_t_1-region.
  PERFORM bdc_field       USING 'LFA1-SPRAS'
                              lt_t_1-spras.
  PERFORM bdc_field       USING 'LFA1-TELF1'
                              lt_t_1-tel.
  PERFORM bdc_field       USING 'LFA1-TELFX'
                              lt_t_1-fax.
  PERFORM bdc_field       USING 'LFA1-LFURL'
                              lt_t_1-smtp.

*Marta 27.11.2008 / inicio modificacion
  PERFORM bdc_field       USING 'BDC_OKCODE'
                           '/00'.
*Marta 27.11.2008 / fin modifiaci'on

************************************************************

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0120'.
*  aayala condicionar para dominicana 19 de enero de 2012
  IF  lt_t_1-bu_group EQ '2060'.
    PERFORM bdc_field       USING 'LFA1-VBUND' lt_t_1-rcomp. "IA080509
  ENDIF.
  PERFORM bdc_field       USING 'LFA1-STCD1' lt_t_1-cif.

  PERFORM bdc_field       USING 'LFA1-KONZS'
                              lt_t_1-c_fisc_mx.
*** INICIO MODIFICACIÓN EMG 12/05/2009
*  IF lt_t_1-ktokk EQ 'ZTGR' ORse elimina para rcd resorts aayala 30.11.2011
*     lt_t_1-ktokk EQ 'ZTER'.
  PERFORM bdc_field     USING 'LFA1-BRSCH' lt_t_1-brsch.
*  ENDIF.se elimina para rcd resorts aayala 30.11.2011
*** FIN MODIFICACIÓN EMG 12/05/2009
  PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
************************************************************
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.
  PERFORM bdc_field       USING 'LFBK-BANKS(01)'
                              lt_t_1-land1.
  PERFORM bdc_field       USING 'LFBK-BANKL(01)'
                              lt_t_1-bankk.
  PERFORM bdc_field       USING 'LFBK-BANKN(01)'
                              lt_t_1-bankn.
  PERFORM bdc_field       USING 'LFBK-BKONT(01)'
                              lt_t_1-bkont.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=ENTR'.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=ENTR'.
*falta persona de contacto
  PERFORM crea_acreedor_8300_3 .
*************************************************************
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0210'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
  PERFORM bdc_field       USING 'LFB1-AKONT'
                              lt_t_1-akont.

*  IF LT_T_1-KTOKK = 'ZTGR'.

  PERFORM bdc_field       USING 'LFB1-FDGRV'
                              lt_t_1-fdgrv.

*  ENDIF.

**************************************************************
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0215'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
  PERFORM bdc_field       USING 'LFB1-ZTERM'
                              lt_t_1-zterm.

  PERFORM bdc_field       USING 'LFB1-ZWELS'
                              lt_t_1-zwels.
  PERFORM bdc_field USING 'LFB1-REPRF' 'X'."aayala 30.11.2011
  IF NOT lt_t_1-bloqj IS INITIAL.

    PERFORM bdc_field       USING 'LFB1-ZAHLS'
                                  'J'.

  ENDIF.

ENDFORM.                    " crea_sociedad_1_8300
*&---------------------------------------------------------------------*
*&      Form  crear_sociedad_1_5000
*&---------------------------------------------------------------------*
*       Crear un proveedor en la sociedad 5000 cuando no hay
*       organización de compras
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM crear_sociedad_1_5000 USING l_lifnr.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0105'.

  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.

  PERFORM bdc_field       USING 'RF02K-BUKRS'
                                '5000'.

  PERFORM bdc_field       USING 'RF02K-LIFNR'
                                l_lifnr.
*                              lt_t_1-lifnr.

  PERFORM bdc_field       USING 'RF02K-KTOKK'
                                lt_t_1-bu_group.

*Marta Vall: 15.10.2008 - inicio modificación
*Replica del primer proveedor creado

***************************************************************
**  PERFORM bdc_dynpro      USING 'SAPMF02K' '0110'.
**
**  PERFORM bdc_field       USING 'LFA1-NAME1'
**                              lt_t_1-name1.
**  PERFORM bdc_field       USING 'LFA1-SORTL'
**                              lt_t_1-busq.
**  PERFORM bdc_field       USING 'LFA1-NAME2'
**                              lt_t_1-name2.
**  PERFORM bdc_field       USING 'LFA1-STRAS'
**                              lt_t_1-direc.
**  PERFORM bdc_field       USING 'LFA1-ORT01'
**                              lt_t_1-poblac.
**  PERFORM bdc_field       USING 'LFA1-PSTLZ'
**                              lt_t_1-cod_post.
**  PERFORM bdc_field       USING 'LFA1-LAND1'
**                              lt_t_1-pais.
**  PERFORM bdc_field       USING 'LFA1-REGIO'
**                              lt_t_1-region.
**  PERFORM bdc_field       USING 'LFA1-SPRAS'
**                              lt_t_1-spras.
**  PERFORM bdc_field       USING 'LFA1-TELF1'
**                              lt_t_1-tel.
**  PERFORM bdc_field       USING 'LFA1-TELFX'
**                              lt_t_1-fax.
**  PERFORM bdc_field       USING 'LFA1-LFURL'
**                              lt_t_1-smtp.
***************************************************************
**
**  PERFORM bdc_dynpro      USING 'SAPMF02K' '0120'.
**  PERFORM bdc_field       USING 'LFA1-STCD1'
**                              lt_t_1-cif.
**  PERFORM bdc_field       USING 'BDC_OKCODE'
**                              '/00'.
***************************************************************
**  PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.
**  PERFORM bdc_field       USING 'LFBK-BANKS(01)'
**                              lt_t_1-land1.
**  PERFORM bdc_field       USING 'LFBK-BANKL(01)'
**                              lt_t_1-bankk.
**  PERFORM bdc_field       USING 'LFBK-BANKN(01)'
**                              lt_t_1-bankn.
**  PERFORM bdc_field       USING 'LFBK-BKONT(01)'
**                              lt_t_1-bkont.
**  PERFORM bdc_field       USING 'BDC_OKCODE'
**                              '=ENTR'.
**
**  PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.
**  PERFORM bdc_field       USING 'BDC_OKCODE'
**                              '=ENTR'.
**
***************************************************************
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0210'.

  PERFORM bdc_field       USING 'LFB1-AKONT'
                              lt_t_1-akont.

*  IF LT_T_1-KTOKK = 'ZTGR'.

  PERFORM bdc_field       USING 'LFB1-FDGRV'
                              lt_t_1-fdgrv.

  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
*  ENDIF.

****************************************************************
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0215'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
  PERFORM bdc_field       USING 'LFB1-ZTERM'
                              lt_t_1-zterm.

  PERFORM bdc_field       USING 'LFB1-ZWELS'
                              lt_t_1-zwels.
  PERFORM bdc_field USING 'LFB1-REPRF' 'X'."aayala 30.11.2011
  IF NOT lt_t_1-bloqj IS INITIAL.

    PERFORM bdc_field       USING 'LFB1-ZAHLS'
                                  'J'.

  ENDIF.

*Marta Vall: 15.10.2008 - fin modificación
ENDFORM.                    " crear_sociedad_1_5000
*&---------------------------------------------------------------------*
*&      Form  CREAR_SOCIEDAD_SINCRONIZACION
*&---------------------------------------------------------------------*
*  Autor: Marta Vall Armengol
*  Fecha: 16.10.2008
*----------------------------------------------------------------------*
*      -->P_LV_LIFNR  text
*----------------------------------------------------------------------*
FORM crear_sociedad_sincronizacion  USING    l_lifnr l_bukrs.
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0105'.

  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.

  PERFORM bdc_field       USING 'RF02K-BUKRS'
                                l_bukrs.

  PERFORM bdc_field       USING 'RF02K-LIFNR'
                                l_lifnr.
*
  PERFORM bdc_field       USING 'RF02K-KTOKK'
                                lt_t_1-bu_group.


***************************************************************
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0210'.

  PERFORM bdc_field       USING 'LFB1-AKONT'
                              lt_t_1-akont.


  PERFORM bdc_field       USING 'LFB1-FDGRV'
                              lt_t_1-fdgrv.

  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.

****************************************************************
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0215'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
  PERFORM bdc_field       USING 'LFB1-ZTERM'
                              lt_t_1-zterm.

  PERFORM bdc_field       USING 'LFB1-ZWELS'
                              lt_t_1-zwels.
  PERFORM bdc_field USING 'LFB1-REPRF' 'X'."aayala 30.11.2011
  IF NOT lt_t_1-bloqj IS INITIAL.

    PERFORM bdc_field       USING 'LFB1-ZAHLS'
                                  'J'.

  ENDIF.
ENDFORM.                    " CREAR_SOCIEDAD_SINCRONIZACION
*&---------------------------------------------------------------------*
*&      Form  ACTUALIZA_MAIL
*&---------------------------------------------------------------------*
*       Actualiza mail
*----------------------------------------------------------------------*
FORM actualiza_mail  USING p_lifnr
                           p_smtp.

  DATA: lt_smtp  TYPE STANDARD TABLE OF adsmtp WITH HEADER LINE,
        lv_adrnr TYPE lfa1-adrnr.

*Tiempo de espera para encontrar en tablas el acreedor
  WAIT UP TO 3 SECONDS.

  SELECT SINGLE adrnr
    INTO lv_adrnr
    FROM lfa1
    WHERE lifnr EQ p_lifnr.

  IF sy-subrc EQ 0.

    PERFORM addr_comm_get TABLES lt_smtp
                          USING lv_adrnr
                                'ADSMTP'
                                p_smtp.

    READ TABLE lt_smtp WITH KEY flgdefault = 'X'.
    IF sy-subrc EQ 0.
*     actualizar
      IF p_smtp NE lt_smtp-smtp_addr.
        MOVE p_smtp TO lt_smtp-smtp_addr.
        lt_smtp-updateflag = 'U'.  "update
        MODIFY lt_smtp INDEX sy-tabix TRANSPORTING smtp_addr updateflag.
      ELSE.
        EXIT.
      ENDIF.
    ELSE.
*     insertar
      IF p_smtp IS INITIAL.
        EXIT.
      ELSE.
        MOVE: 'X' TO lt_smtp-flgdefault,
              'X' TO lt_smtp-home_flag,
              p_smtp TO lt_smtp-smtp_addr,
              'I' TO lt_smtp-updateflag. "insert
        APPEND lt_smtp.
      ENDIF.
    ENDIF.


    PERFORM addr_comm_maintain TABLES lt_smtp
                               USING lv_adrnr
                                     'ADSMTP'.
  ENDIF.

ENDFORM.                    " ACTUALIZA_MAIL
*&---------------------------------------------------------------------*
*&      Form  ADDR_COMM_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM addr_comm_get  TABLES   p_comm_table
                    USING    p_adrnr TYPE lfa1-adrnr
                             p_table_name TYPE szad_field-table_type
                             p_smtp TYPE adr6-smtp_addr.

  FIELD-SYMBOLS <fs_mail> TYPE adsmtp.

  DATA: ls_commtable TYPE adsmtp.

  CALL FUNCTION 'ADDR_COMM_GET'
    EXPORTING
      address_number    = p_adrnr
      table_type        = p_table_name
    TABLES
      comm_table        = p_comm_table
    EXCEPTIONS
      parameter_error   = 1
      address_not_exist = 2
      internal_error    = 3
      OTHERS            = 4.

ENDFORM.                    " ADDR_COMM_GET
*&---------------------------------------------------------------------*
*&      Form  addr_comm_maintain
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM addr_comm_maintain  TABLES p_comm_table TYPE STANDARD TABLE
                         USING  p_adrnr TYPE lfa1-adrnr
                                p_table_name TYPE szad_field-table_type.

  DATA: lt_error_table TYPE STANDARD TABLE OF addr_error WITH HEADER LINE,
        lv_returncode  LIKE  szad_field-returncode.

  CLEAR p_comm_table.

  CALL FUNCTION 'ADDR_COMM_MAINTAIN'
    EXPORTING
      address_number     = p_adrnr
      table_type         = p_table_name
      iv_time_dependence = 'X'
    IMPORTING
      returncode         = lv_returncode
    TABLES
      comm_table         = p_comm_table
      error_table        = lt_error_table
    EXCEPTIONS
      parameter_error    = 1
      address_not_exist  = 2
      internal_error     = 3
      OTHERS             = 4.

  READ TABLE lt_error_table WITH KEY msg_type = 'E'.
  IF sy-subrc NE 0.

    CALL FUNCTION 'ADDR_MEMORY_SAVE'
      EXPORTING
        execute_in_update_task = ' '
      EXCEPTIONS
        address_number_missing = 1
        person_number_missing  = 2
        internal_error         = 3
        database_error         = 4
        reference_missing      = 5
        OTHERS                 = 6.
    IF sy-subrc <> 0.
    ENDIF.
  ENDIF.
ENDFORM.                    " addr_comm_maintain

*&---------------------------------------------------------------------*
*&      Form  DERIVAR_AKONT
*&---------------------------------------------------------------------*
*       Deriva la cuenta asociada desde la configuración de BP.
*----------------------------------------------------------------------*
FORM derivar_akont CHANGING cs_prov STRUCTURE zfieprov.

  DATA:
        ls_ztbp001 TYPE ztbp001.

  CLEAR cs_prov-akont.
  CHECK cs_prov-bukrs IS NOT INITIAL.
  CHECK cs_prov-pais IS NOT INITIAL.

  SELECT SINGLE land1
  FROM t001
  INTO @DATA(lv_land1)
  WHERE bukrs = @cs_prov-bukrs.

  IF sy-subrc = 0.
    IF cs_prov-pais = lv_land1 AND cs_prov-bu_group <> 'ZINT'.
      ls_ztbp001-nac_ext = 'N'.
      CLEAR: ls_ztbp001-bu_group.
    ELSEIF cs_prov-pais <> lv_land1 AND cs_prov-bu_group <> 'ZINT'.
      ls_ztbp001-nac_ext = 'E'.
      CLEAR: ls_ztbp001-bu_group.
    ELSEIF cs_prov-pais = lv_land1 AND cs_prov-bu_group = 'ZINT'.
      ls_ztbp001-nac_ext = 'N'.
      ls_ztbp001-bu_group = 'ZINT'.
    ELSEIF cs_prov-pais <> lv_land1 AND cs_prov-bu_group = 'ZINT'.
      ls_ztbp001-nac_ext = 'E'.
      ls_ztbp001-bu_group = 'ZINT'.
    ENDIF.
  ENDIF.

  SELECT SINGLE hkont
  FROM ztbp001
  INTO @cs_prov-akont
  WHERE koart    = 'K'
  AND bu_group = @ls_ztbp001-bu_group
  AND nac_ext  = @ls_ztbp001-nac_ext.

ENDFORM.                    " DERIVAR_AKONT

*&---------------------------------------------------------------------*
*& Include          ZFI0009CLS
*&---------------------------------------------------------------------*
CLASS zcl_bp DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS maintain_bp
      CHANGING
        cs_prov          TYPE zfieprov
      RETURNING
        VALUE(rs_result) TYPE ty_result.

  PRIVATE SECTION.
    METHODS determine_context
      CHANGING
        cs_prov           TYPE zfieprov
      RETURNING
        VALUE(rs_context) TYPE ty_context.

    METHODS map_bp_data
      CHANGING
        cs_prov    TYPE zfieprov
        cs_context TYPE ty_context
        cs_data    TYPE cvis_ei_extern.

    METHODS map_roles
      CHANGING
        cs_prov TYPE zfieprov
        cs_data TYPE cvis_ei_extern.

    METHODS map_tax_numbers
      CHANGING
        cs_prov TYPE zfieprov
        cs_data TYPE cvis_ei_extern.

    METHODS map_tax_number
      CHANGING
        cs_prov     TYPE zfieprov
        cv_taxtype  TYPE dfkkbptaxnum-taxtype
        cv_value    TYPE string
        cv_extended TYPE abap_bool
        cs_data     TYPE cvis_ei_extern.

    METHODS map_bp_address
      CHANGING
        cs_prov    TYPE zfieprov
        cs_context TYPE ty_context
        cs_data    TYPE cvis_ei_extern.

    METHODS map_bp_communication
      CHANGING
        cs_prov    TYPE zfieprov
        cv_task    TYPE c
        cs_address TYPE bus_ei_bupa_address.

    METHODS map_industry
      CHANGING
        cs_prov TYPE zfieprov
        cs_data TYPE cvis_ei_extern.

    METHODS map_bank_data
      CHANGING
        cs_prov    TYPE zfieprov
        cs_context TYPE ty_context
        cs_data    TYPE cvis_ei_extern.

    METHODS map_withholding_tax
      CHANGING
        cs_prov    TYPE zfieprov
        cs_company TYPE vmds_ei_company.

    METHODS map_company_data
      CHANGING
        cs_prov TYPE zfieprov
        cv_task TYPE c
        cs_data TYPE cvis_ei_extern.

    METHODS map_purchasing_data
      CHANGING
        cs_prov TYPE zfieprov
        cv_task TYPE c
        cs_data TYPE cvis_ei_extern.

    METHODS map_purchasing_functions
      CHANGING
        cs_prov       TYPE zfieprov
        cs_purchasing TYPE vmds_ei_purchasing.

    METHODS call_api
      IMPORTING
        is_data          TYPE cvis_ei_extern
      RETURNING
        VALUE(rt_return) TYPE bapiretm.

    METHODS evaluate_return
      IMPORTING
        it_return        TYPE bapiretm
      RETURNING
        VALUE(cs_result) TYPE ty_result.

ENDCLASS.
CLASS zcl_bp IMPLEMENTATION.

  METHOD determine_context.

    rs_context-valid = abap_true.

    " BP NUEVO
    IF cs_prov-partner IS INITIAL.

      rs_context-bp_task         = gc_task_insert.
      rs_context-vendor_task     = gc_task_insert.
      rs_context-address_task    = gc_task_insert.
      rs_context-company_task    = gc_task_insert.
      rs_context-purchasing_task = gc_task_insert.

      " GUID técnico del nuevo BP
      cl_system_uuid=>if_system_uuid_static~create_uuid_c32(
      RECEIVING uuid = rs_context-partner_guid ).

      " GUID de la dirección estándar
      cl_system_uuid=>if_system_uuid_static~create_uuid_c32(
      RECEIVING uuid = rs_context-address_guid ).

      RETURN.
    ENDIF.

    " BP EXISTENTE
    SELECT SINGLE  partner_guid, bu_group
    FROM but000
    WHERE partner = @cs_prov-partner
    INTO @DATA(ls_but000).

    IF sy-subrc <> 0.
      rs_context-valid = abap_false.
      rs_context-message = |El Business Partner { cs_prov-partner } NO existe|.

      RETURN.
    ENDIF.

    rs_context-bp_task      = gc_task_update.
    rs_context-partner_guid = ls_but000-partner_guid.

    " La agrupación de un BP existente no se cambia
    IF cs_prov-bu_group IS NOT INITIAL
    AND ls_but000-bu_group <> cs_prov-bu_group.

      rs_context-valid = abap_false.

      rs_context-message = |El BP { cs_prov-partner } pertenece a la agrupación | &&
      |{ ls_but000-bu_group }, NO a { cs_prov-bu_group }|.

      RETURN.
    ENDIF.

    " Supplier existente o nueva extensión Supplier
    SELECT SINGLE ktokk FROM lfa1
    WHERE lifnr = @cs_prov-partner
    INTO @DATA(lv_ktokk).

    IF sy-subrc = 0.

      rs_context-vendor_task = gc_task_update.

      " El grupo de cuentas tampoco debe cambiarse silenciosamente
      IF cs_prov-bu_group IS NOT INITIAL AND lv_ktokk <> cs_prov-bu_group.

        rs_context-valid = abap_false.

        rs_context-message = |El proveedor { cs_prov-partner } tiene grupo de cuentas | &&
        |{ lv_ktokk }, NO { cs_prov-bu_group }|.

        RETURN.
      ENDIF.

    ELSE.
      " BP existe, pero todavía no existe como Supplier
      rs_context-vendor_task = gc_task_insert.
    ENDIF.

    " Dirección actual
    SELECT address_guid, addr_valid_to
    FROM but020
    WHERE partner = @cs_prov-partner
    INTO TABLE @DATA(lt_addresses).

    IF lt_addresses IS NOT INITIAL.

      SORT lt_addresses BY addr_valid_to DESCENDING.

      rs_context-address_guid =  lt_addresses[ 1 ]-address_guid.

      rs_context-address_task = gc_task_update.

    ELSE.

      rs_context-address_task = gc_task_insert.

      cl_system_uuid=>if_system_uuid_static~create_uuid_c32(
      RECEIVING uuid = rs_context-address_guid ).
    ENDIF.

    " Sociedad
    IF cs_prov-bukrs IS NOT INITIAL.

      SELECT SINGLE @abap_true FROM lfb1
      WHERE lifnr = @cs_prov-partner
      AND bukrs = @cs_prov-bukrs
      INTO @DATA(lv_company_exists).

      rs_context-company_task = COND #(
      WHEN sy-subrc = 0
      THEN gc_task_update
      ELSE gc_task_insert ).

    ENDIF.

    " Organización de compras
    IF cs_prov-ekorg IS NOT INITIAL.

      SELECT SINGLE @abap_true FROM lfm1
      WHERE lifnr = @cs_prov-partner
      AND ekorg = @cs_prov-ekorg
      INTO @DATA(lv_purchasing_exists).

      rs_context-purchasing_task = COND #(
      WHEN sy-subrc = 0
      THEN gc_task_update
      ELSE gc_task_insert ).

    ENDIF.

  ENDMETHOD.

  METHOD maintain_bp.
    DATA:
      ls_data    TYPE cvis_ei_extern,
      ls_context TYPE ty_context.

    " Normalizar PARTNER si viene informado
    IF cs_prov-partner IS NOT INITIAL.
      cs_prov-partner = |{ cs_prov-partner ALPHA = IN }|.
    ENDIF.

    " Determinar altas / modificaciones
    ls_context = determine_context( cs_prov = cs_prov ).

    IF ls_context-valid = abap_false.

      rs_result-success = abap_false.
      rs_result-message = ls_context-message.

      RETURN.
    ENDIF.

    " Datos centrales
    map_bp_data(
    CHANGING
      cs_prov = cs_prov
      cs_context = ls_context
      cs_data = ls_data ).

    " Roles FLVN00 / FLVN01
    map_roles(
    CHANGING
      cs_prov = cs_prov
      cs_data = ls_data ).

    " Números fiscales - CIF
    map_tax_numbers(
    CHANGING
      cs_prov = cs_prov
      cs_data = ls_data ).

    " Direccion
    map_bp_address(
    CHANGING
      cs_prov = cs_prov
      cs_context = ls_context
      cs_data = ls_data ).

    " Industria
    map_industry(
    CHANGING
      cs_prov = cs_prov
      cs_data = ls_data ).

    " Banco
    map_bank_data(
    CHANGING
      cs_prov = cs_prov
      cs_context = ls_context
      cs_data = ls_data ).

    " Datos dependientes de sociedad (FLVN00)
    IF cs_prov-bukrs IS NOT INITIAL.
      map_company_data(
      CHANGING
        cs_prov = cs_prov
        cv_task = ls_context-company_task
        cs_data = ls_data ).
    ENDIF.

    " Datos de organización de compras (FLVN01)
    IF cs_prov-ekorg IS NOT INITIAL.
      map_purchasing_data(
      CHANGING
        cs_prov = cs_prov
        cv_task = ls_context-purchasing_task
        cs_data = ls_data ).
    ENDIF.

    " Ejecutar API
    rs_result-return = call_api( ls_data ).

    " El tratamiento de mensajes
    rs_result = evaluate_return( it_return = rs_result-return ).

    IF rs_result-success = abap_true.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      " BP nuevo: recuperar número generado
      IF cs_prov-partner IS INITIAL.

        IF rs_result-partner IS INITIAL.

          SELECT SINGLE partner FROM but000
          WHERE partner_guid = @ls_context-partner_guid
          INTO @rs_result-partner.
        ENDIF.

        IF rs_result-partner IS NOT INITIAL.
          cs_prov-partner = rs_result-partner.
        ELSE.
          rs_result-success = abap_false.
          rs_result-message = 'El BP fue procesado pero no se pudo recuperar el número generado'.
        ENDIF.

      ELSE.
        rs_result-partner = cs_prov-partner.
      ENDIF.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ENDMETHOD.

*--------------------------------------------------------------------*
*& PARTNER - HEADER + CENTRAL_DATA - COMMON
*& VENDOR - CENTRAL_DATA - CENTRAL
*--------------------------------------------------------------------*
  METHOD map_bp_data.

    " Cabecera
    cs_data-partner-header-object_task = cs_context-bp_task.
    cs_data-partner-header-object_instance-bpartnerguid = cs_context-partner_guid.

    " Business Partner
    IF cs_prov-partner IS NOT INITIAL.
      cs_data-partner-header-object_instance-bpartner = cs_prov-partner.
    ENDIF.

    " Datos generales
    " El proveedor siempre como organizacion -> BUT000-TYPE = 2
    IF cs_context-bp_task = gc_task_insert.

      cs_data-partner-central_data-common-data-bp_control-category = gc_bp_org.
      cs_data-partner-central_data-common-data-bp_control-grouping = cs_prov-bu_group.

    ENDIF.

    cs_data-partner-central_data-common-data-bp_centraldata-grouping = cs_prov-bu_group.
    cs_data-partner-central_data-common-datax-bp_centraldata-partnertype = cs_prov-bu_group.
    cs_data-partner-central_data-common-data-bp_organization-name1 = cs_prov-name1.
    cs_data-partner-central_data-common-datax-bp_organization-name1 =  abap_true.
    cs_data-partner-central_data-common-data-bp_organization-name2 = cs_prov-name2.
    cs_data-partner-central_data-common-datax-bp_organization-name2 = abap_true.

    cs_data-partner-central_data-common-data-bp_centraldata-searchterm1 = cs_prov-busq.
    cs_data-partner-central_data-common-datax-bp_centraldata-searchterm1 = abap_true.

    IF cs_prov-stkzn IS NOT INITIAL.
      cs_data-vendor-central_data-central-data-stkzn = cs_prov-stkzn.
      cs_data-vendor-central_data-central-datax-stkzn = abap_true.
    ENDIF.

  ENDMETHOD.

*--------------------------------------------------------------------*
*& PARTNER - CENTRAL_DATA - ROLE
*--------------------------------------------------------------------*
  METHOD map_roles.
    FIELD-SYMBOLS: <fs_role> TYPE bus_ei_bupa_roles.
    DATA: lv_role_exists TYPE abap_bool VALUE abap_false.

    " FLVN00 - Acreedor <=> Sociedad
    IF cs_prov-bukrs IS NOT INITIAL.
      IF cs_prov-partner IS NOT INITIAL.

        SELECT SINGLE @abap_true FROM but100
        WHERE partner = @cs_prov-partner
        AND rltyp   = @gc_role_flvn00
        INTO @lv_role_exists.

      ENDIF.

      IF lv_role_exists = abap_false.
        APPEND INITIAL LINE TO cs_data-partner-central_data-role-roles ASSIGNING <fs_role>.
        <fs_role>-task     = gc_task_insert.
        <fs_role>-data_key = gc_role_flvn00.
      ENDIF.
    ENDIF.

    CLEAR: lv_role_exists.
    " FLVN01 - Proveedor <=> Organización de Compras
    IF cs_prov-ekorg IS NOT INITIAL.
      IF cs_prov-partner IS NOT INITIAL.

        SELECT SINGLE @abap_true
        FROM but100
        WHERE partner = @cs_prov-partner
        AND rltyp   = @gc_role_flvn01
        INTO @lv_role_exists.

      ENDIF.

      IF lv_role_exists = abap_false.
        APPEND INITIAL LINE TO  cs_data-partner-central_data-role-roles ASSIGNING <fs_role>.
        <fs_role>-task     = gc_task_insert.
        <fs_role>-data_key = gc_role_flvn01.
      ENDIF.
    ENDIF.

  ENDMETHOD.

*--------------------------------------------------------------------*
*& PARTNER - CENTRAL_DATA - TAXNUMBER
*--------------------------------------------------------------------*
  METHOD map_tax_numbers.

    FIELD-SYMBOLS: <fs_tax> TYPE bus_ei_bupa_taxnumber.

    " NIF principal
    " ZFIEPROV-CIF -> DFKKBPTAXNUM-TAXNUMXL
    IF cs_prov-cif IS NOT INITIAL.
      APPEND INITIAL LINE TO cs_data-partner-central_data-taxnumber-taxnumbers ASSIGNING <fs_tax>.
      <fs_tax>-task = cv_task.
      " RFC genérico extranjero de México
      IF cs_prov-cif = gc_rfc_ext_mx.
        <fs_tax>-data_key-taxtype = 'MX1'.
      ELSE.
        <fs_tax>-data_key-taxtype = |{ cs_prov-pais }1|.
      ENDIF.
      <fs_tax>-data_key-taxnumxl = cs_prov-cif.
    ENDIF.

    "---------------------------------------------------------------
    " NIF3
    " ZFIEPROV-STCD3 -> DFKKBPTAXNUM-TAXNUM
    "---------------------------------------------------------------
    IF cs_prov-stcd3 IS NOT INITIAL.
      APPEND INITIAL LINE TO cs_data-partner-central_data-taxnumber-taxnumbers ASSIGNING <fs_tax>.
      <fs_tax>-task = cv_task.
      <fs_tax>-data_key-taxtype = |{ cs_prov-pais }3|.
      <fs_tax>-data_key-taxnumber = cs_prov-stcd3.
    ENDIF.

  ENDMETHOD.

*--------------------------------------------------------------------*
*& PARTNER - CENTRAL_DATA - ADDRESS - ADDRESSES - DATA - POSTAL
*--------------------------------------------------------------------*
  METHOD map_bp_address.

    DATA: lv_langu_iso TYPE laiso.
    FIELD-SYMBOLS: <fs_address> TYPE bus_ei_bupa_address.

    APPEND INITIAL LINE TO cs_data-partner-central_data-address-addresses ASSIGNING <fs_address>.
    <fs_address>-task =  cs_context-address_task.
    <fs_address>-data_key-guid = cs_context-address_guid.
    <fs_address>-data_key-operation = 'XXDFLT'.


    <fs_address>-data-postal-data-city = cs_prov-poblac.
    <fs_address>-data-postal-DATAx-city = abap_true.
    <fs_address>-data-postal-data-street = cs_prov-direc.
    <fs_address>-data-postal-DATAx-street = abap_true.
    <fs_address>-data-postal-data-str_suppl1 = cs_prov-direc_2.
    <fs_address>-data-postal-DATAx-str_suppl1 = abap_true.
    <fs_address>-data-postal-data-postl_cod1 = cs_prov-cod_post.
    <fs_address>-data-postal-DATAx-postl_cod1 = abap_true.
    <fs_address>-data-postal-data-region = cs_prov-region.
    <fs_address>-data-postal-DATAx-region = abap_true.
    <fs_address>-data-postal-data-country = cs_prov-pais.
    <fs_address>-data-postal-DATAx-country = abap_true.
    <fs_address>-data-postal-data-langu = cs_prov-spras.
    <fs_address>-data-postal-DATAx-langu = abap_true.

    IF cs_prov-spras IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
        EXPORTING
          input  = cs_prov-spras
        IMPORTING
          output = lv_langu_iso.

      IF lv_langu_iso IS NOT INITIAL.
        <fs_address>-data-postal-data-languiso = lv_langu_iso.
        <fs_address>-data-postal-datax-langu_iso = abap_true.
      ENDIF.
    ENDIF.

    "Comunicacion
    map_bp_communication(
    CHANGING
      cs_prov = cs_prov
      cv_task = cs_context-address_task
     cs_address = <fs_address> ).
  ENDMETHOD.

*--------------------------------------------------------------------*
*& PARTNER - CENTRAL_DATA - ADDRESS - ADDRESSES - DATA - COMMUNICATION
*--------------------------------------------------------------------*
  METHOD map_bp_communication.

    FIELD-SYMBOLS: <fs_phone> TYPE bus_ei_bupa_telephone,
                   <fs_fax>   TYPE bus_ei_bupa_fax,
                   <fs_smtp>  TYPE bus_ei_bupa_smtp.

    IF cs_prov-tel IS NOT INITIAL.
      cs_address-data-communication-phone-current_state = abap_true.
      APPEND INITIAL LINE TO cs_address-data-communication-phone-phone ASSIGNING <fs_phone>.
      <fs_phone>-contact-task = cv_task.
      <fs_phone>-contact-data-telephone = cs_prov-tel.
      <fs_phone>-contact-datax-telephone  = abap_true.
    ENDIF.

    IF cs_prov-fax IS NOT INITIAL.
      cs_address-data-communication-fax-current_state = abap_true.
      APPEND INITIAL LINE TO cs_address-data-communication-fax-fax ASSIGNING <fs_fax>.
      <fs_fax>-contact-task = cv_task.
      <fs_fax>-contact-data-fax = cs_prov-fax.
      <fs_fax>-contact-DATAx-fax = abap_true.
    ENDIF.

    IF cs_prov-smtp IS NOT INITIAL.
      cs_address-data-communication-smtp-current_state = abap_true.
      APPEND INITIAL LINE TO cs_address-data-communication-smtp-smtp ASSIGNING <fs_smtp>.
      <fs_smtp>-contact-task = cv_task.
      <fs_smtp>-contact-data-e_mail = cs_prov-smtp.
      <fs_smtp>-contact-DATAx-e_mail = abap_true.
    ENDIF.
    "si pasa de lleno a vacio los datos, hay que plantear ese proceso
  ENDMETHOD.

*--------------------------------------------------------------------*
*& PARTNER - CENTRAL_DATA - INDUSTRY - INDUSTRIES
*--------------------------------------------------------------------*
  METHOD map_industry.

    FIELD-SYMBOLS:
    <fs_industry> TYPE bus_ei_bupa_industrysector.

    CHECK cs_prov-brsch IS NOT INITIAL.

    " Determinar el sistema de industrias al que pertenece el ramo.
    SELECT SINGLE istype
    FROM tb038a
    WHERE ind_sector = @cs_prov-brsch
    INTO @DATA(lv_istype).

    IF sy-subrc = 0.
      APPEND INITIAL LINE TO cs_data-partner-central_data-industry-industries ASSIGNING <fs_industry>.

      <fs_industry>-task = cv_task.
      <fs_industry>-data_key-keysystem = lv_istype. " Sector industrial
      <fs_industry>-data_key-ind_sector = cs_prov-brsch. " Ramo
      <fs_industry>-data-ind_default =  abap_true.
      <fs_industry>-datax-ind_default = abap_true.
    ENDIF.
  ENDMETHOD.

*--------------------------------------------------------------------*
*& PARTNER - CENTRAL_DATA - BANKDETAIL
*--------------------------------------------------------------------*
  METHOD map_bank_data.

    FIELD-SYMBOLS: <fs_bankdetail> TYPE bus_ei_bupa_bankdetail.
    DATA lv_iban TYPE iban.

    " El programa anterior únicamente informaba los datos bancarios durante la migración 3
    CHECK cs_prov-migr = 3. "Ver si funcionalmente sigue siendo valido
    CHECK cs_context-bp_task = gc_task_insert.

    " Crear detalle bancario del Business Partner
    APPEND INITIAL LINE TO cs_data-partner-central_data-bankdetail-bankdetails ASSIGNING <fs_bankdetail>.

    <fs_bankdetail>-task =   gc_task_insert.

    " País del banco ZFIEPROV-LAND1
    IF cs_prov-land1 IS NOT INITIAL.
      <fs_bankdetail>-data-bank_ctry  = cs_prov-land1.
      <fs_bankdetail>-datax-bank_ctry = abap_true.
    ENDIF.

    " Clave del banco ZFIEPROV-BANKK
    IF cs_prov-bankk IS NOT INITIAL.
      <fs_bankdetail>-data-bank_key  = cs_prov-bankk.
      <fs_bankdetail>-datax-bank_key = abap_true.
    ENDIF.

    " Número de cuenta bancaria ZFIEPROV-BANKN
    IF cs_prov-bankn IS NOT INITIAL.
      <fs_bankdetail>-data-bank_acct  = cs_prov-bankn.
      <fs_bankdetail>-datax-bank_acct = abap_true.
    ENDIF.

    " Clave de control bancaria ZFIEPROV-BKONT
    IF cs_prov-bkont IS NOT INITIAL.
      <fs_bankdetail>-data-ctrl_key  = cs_prov-bkont.
      <fs_bankdetail>-datax-ctrl_key = abap_true.
    ENDIF.

    " Titular de la cuenta.
    " El BDC informa siempre LFBK-KOINH = 'TIT'.
    <fs_bankdetail>-data-accountholder  = 'TIT'.
    <fs_bankdetail>-datax-accountholder = abap_true.

    " IBAN
    " El programa anterior almacenaba el IBAN dividido en nueve
    " campos por limitaciones de la dynpro. La estructura BP recibe
    " directamente el IBAN completo
    IF cs_prov-iban01 IS NOT INITIAL.

      CONCATENATE
      cs_prov-iban01 cs_prov-iban02
      cs_prov-iban03 cs_prov-iban04
      cs_prov-iban05 cs_prov-iban06
      cs_prov-iban07 cs_prov-iban08
      cs_prov-iban09 INTO lv_iban.

      " Eliminar posibles espacios del IBAN
      CONDENSE lv_iban NO-GAPS.

      <fs_bankdetail>-data-iban  = lv_iban.
      <fs_bankdetail>-datax-iban = abap_true.

    ENDIF.

  ENDMETHOD.


*--------------------------------------------------------------------*
*& VENDOR - COMPANY_DATA - COMPANY
*--------------------------------------------------------------------*
  METHOD map_company_data.

    FIELD-SYMBOLS: <fs_company> TYPE vmds_ei_company.

    " Los datos de sociedad solo existen cuando se informa BUKRS.
    CHECK cs_prov-bukrs IS NOT INITIAL.

    APPEND INITIAL LINE TO cs_data-vendor-company_data-company ASSIGNING <fs_company>.

    <fs_company>-task = cv_task.

    " Sociedad
    <fs_company>-data_key-bukrs = cs_prov-bukrs.

    " Cuenta asociada derivada previamente desde ZTBP001
    <fs_company>-data-akont  = cs_prov-akont.
    <fs_company>-datax-akont = abap_true.

    " Grupo de tesorería
    IF cs_prov-fdgrv IS NOT INITIAL.
      <fs_company>-data-fdgrv  = cs_prov-fdgrv.
      <fs_company>-datax-fdgrv = abap_true.
    ENDIF.

    " Condiciones de pago
    IF cs_prov-zterm IS NOT INITIAL.
      <fs_company>-data-zterm  = cs_prov-zterm.
      <fs_company>-datax-zterm = abap_true.
    ENDIF.

    " Vías de pago
    IF cs_prov-zwels IS NOT INITIAL.
      <fs_company>-data-zwels  = cs_prov-zwels.
      <fs_company>-datax-zwels = abap_true.
    ENDIF.

    " Verificación de factura doble
    <fs_company>-data-reprf  = abap_true.
    <fs_company>-datax-reprf = abap_true.

    " País retención de impuesto
    IF cs_prov-pais_r IS NOT INITIAL.
      <fs_company>-data-qland  = cs_prov-pais_r.
      <fs_company>-datax-qland = abap_true.
    ENDIF.

    " Bloqueo de contabilizacion
    IF cs_prov-bloq IS NOT INITIAL.
      <fs_company>-data-sperr  = abap_true.
      <fs_company>-datax-sperr = abap_true.
    ENDIF.

    IF cs_prov-bloqj IS NOT INITIAL.
      <fs_company>-data-zahls  = 'J'.
      <fs_company>-datax-zahls = abap_true.
    ENDIF.

    " Retenciones
    map_withholding_tax(
    CHANGING
      cs_prov = cs_prov
      cv_task = cv_task
      cs_company =  <fs_company> ).

  ENDMETHOD.

*--------------------------------------------------------------------*
*& VENDOR - COMPANY_DATA - COMPANY - WTAX_TYPE
*--------------------------------------------------------------------*
  METHOD map_withholding_tax.

    FIELD-SYMBOLS: <fs_wtax> TYPE vmds_ei_wtax_type.

    " La validación previa de ZFI0009 controla:
    " PAIS_R + WITHT + WT_WITHCD estén todos informados o todos vacíos
    CHECK cs_prov-pais_r IS NOT INITIAL
    AND cs_prov-witht IS NOT INITIAL
    AND cs_prov-wt_withcd IS NOT INITIAL.

    cv_task = gc_task_insert.
    IF cs_prov-partner IS NOT INITIAL.

      SELECT SINGLE @abap_true FROM lfbw
      WHERE lifnr = @cs_prov-partner
      AND bukrs = @cs_prov-bukrs
      AND witht = @cs_prov-witht
      INTO @DATA(lv_exists).
      IF sy-subrc = 0.
        cv_task = gc_task_update.
      ENDIF.
    ENDIF.

    APPEND INITIAL LINE TO cs_company-wtax_type-wtax_type ASSIGNING <fs_wtax>.

    <fs_wtax>-task = cv_task.

    " Tipo de retención ZFIEPROV-WITHT
    <fs_wtax>-data_key-witht = cs_prov-witht.

    " Código de retención ZFIEPROV-WT_WITHCD
    <fs_wtax>-data-wt_withcd  = cs_prov-wt_withcd.
    <fs_wtax>-datax-wt_withcd = abap_true.

    " Sujeto a retención
    " El BDC establecía siempre WT_SUBJCT = X
    <fs_wtax>-data-wt_subjct  = abap_true.
    <fs_wtax>-datax-wt_subjct = abap_true.

  ENDMETHOD.

*--------------------------------------------------------------------*
*& VENDOR - PURCHASING_DATA - PURCHASING
*--------------------------------------------------------------------*
  METHOD map_purchasing_data.

    FIELD-SYMBOLS: <fs_purchasing> TYPE vmds_ei_purchasing.

    CHECK cs_prov-ekorg IS NOT INITIAL.

    APPEND INITIAL LINE TO cs_data-vendor-purchasing_data-purchasing ASSIGNING <fs_purchasing>.

    <fs_purchasing>-task = cv_task.

    " Organización de compras solicitada
    <fs_purchasing>-data_key-ekorg = cs_prov-ekorg.

    " Moneda
    IF cs_prov-waers IS NOT INITIAL.
      <fs_purchasing>-data-waers  = cs_prov-waers.
      <fs_purchasing>-datax-waers = abap_true.
    ENDIF.

    " Condiciones de pago
    IF cs_prov-zterm IS NOT INITIAL.
      <fs_purchasing>-data-zterm  = cs_prov-zterm.
      <fs_purchasing>-datax-zterm = abap_true.
    ENDIF.

    " Verificación de facturas basada en entrada de mercancías
    <fs_purchasing>-data-webre  = abap_true.
    <fs_purchasing>-datax-webre = abap_true.

    " Proveedor sujeto a liquidación posterior
    <fs_purchasing>-data-bolre  = abap_true.
    <fs_purchasing>-datax-bolre = abap_true.

    " Estructura índice activa para liquidación posterior
    <fs_purchasing>-data-boind  = abap_true.
    <fs_purchasing>-datax-boind = abap_true.

    " Ajuste de volumen de negocio necesario
    <fs_purchasing>-data-umsae  = abap_true.
    <fs_purchasing>-datax-umsae = abap_true.

    map_purchasing_functions(
    CHANGING
      cs_prov = cs_prov
      cv_task = cv_task
      cs_purchasing = <fs_purchasing> ).

  ENDMETHOD.

*--------------------------------------------------------------------*
*& VENDOR - PURCHASING_DATA - PURCHASING - FUNCTIONS
*--------------------------------------------------------------------*
  METHOD map_purchasing_functions.

    FIELD-SYMBOLS: <fs_function> TYPE vmds_ei_functions.
    DATA: lv_parvw TYPE parvw,
          lt_wyt3  TYPE STANDARD TABLE OF wyt3.

    " Funciones de interlocutor utilizadas por el proceso antiguo
    DATA(lt_functions) = VALUE string_table(
          ( `DP` )
          ( `PR` )
          ( `EF` ) ).

    LOOP AT lt_functions INTO DATA(lv_function).
      CLEAR: lv_parvw.
      " Conversión de la función externa al código interno SAP
      CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
        EXPORTING
          input  = lv_function
        IMPORTING
          output = lv_parvw.

      IF cs_prov-partner IS NOT INITIAL.

        SELECT ltsnr, werks, parza
        FROM wyt3
        WHERE lifnr = @cs_prov-partner
        AND ekorg = @cs_prov-ekorg
        AND parvw = @cv_parvw
        INTO CORRESPONDING FIELDS OF TABLE lt_wyt3.

        SORT lt_wyt3 BY parza.

      ELSE.
        CLEAR lt_wyt3.
      ENDIF.

      APPEND INITIAL LINE TO cs_purchasing-functions-functions ASSIGNING <fs_function>.
      <fs_function>-data_key-parvw = lv_parvw.

      IF lt_wyt3 IS NOT INITIAL.
        <fs_function>-task = gc_task_update.

        <fs_function>-data_key-ltsnr = lt_wyt3[ 1 ]-ltsnr.
        <fs_function>-data_key-werks = lt_wyt3[ 1 ]-werks.
        <fs_function>-data_key-parza = lt_wyt3[ 1 ]-parza.

      ELSE.
        <fs_function>-task = gc_task_insert.
      ENDIF.

      " No existen subrango ni centro en el BDC anterior
      IF cs_prov-partner IS NOT INITIAL.
        <fs_function>-data-partner = cs_prov-partner.
      ENDIF.
      <fs_function>-datax-partner = abap_true.
    ENDLOOP.
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

    "CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE
  ENDMETHOD.

  METHOD evaluate_return.

    DATA: lv_text TYPE string.

    cs_result-success = abap_true.

    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<fs_return>).

      " Intentar recuperar número generado
      IF cs_result-partner IS INITIAL AND <fs_return>-object_key IS NOT INITIAL.

        DATA(lv_object_key) =  CONV string( <fs_return>-object_key ).

        CONDENSE lv_object_key NO-GAPS.

        IF strlen( lv_object_key ) <= 10.

          cs_result-partner = |{ lv_object_key ALPHA = IN }|.

        ENDIF.

      ENDIF.

      " Todos los errores
      LOOP AT <fs_return>-object_msg ASSIGNING FIELD-SYMBOL(<fs_message>)
      WHERE type = 'E'  OR type = 'A' OR type = 'X'.

        cs_result-success = abap_false.

        CLEAR lv_text.

        MESSAGE ID <fs_message>-id TYPE 'S' NUMBER <fs_message>-number
        WITH <fs_message>-message_v1  <fs_message>-message_v2
        <fs_message>-message_v3 <fs_message>-message_v4
        INTO lv_text.

        IF cs_result-message IS INITIAL.

          cs_result-message =  lv_text.

        ELSE.

          cs_result-message =  |{ cs_result-message } / { lv_text }|.

        ENDIF.
      ENDLOOP.

    ENDLOOP.

    IF cs_result-success = abap_false  AND cs_result-message IS INITIAL.

      cs_result-message =  'Error al mantener el Business Partner'.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
