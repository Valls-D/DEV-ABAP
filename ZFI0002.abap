*&---------------------------------------------------------------------*
*& Modulpool         ZFI0002
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
INCLUDE ZFI0002TOP.
INCLUDE ZFI0002PBO.
INCLUDE ZFI0002PAI.
INCLUDE ZFI0002FRM.

*&---------------------------------------------------------------------*
*& Include ZFI0002TOP                                        Modulpool        ZFI0002
*&
*&---------------------------------------------------------------------*

PROGRAM  ZFI0002.

TABLES: ZFITPROV,
        ZFIEPROV,
        lfa1,
        lfb1,
        T020,
        RF02D,                        " Dynpro/Arbeitsfelder Debitor
         *RF02D,
         t042z,
         T001.
DATA: CRS_FIELD      like rfcu3-fname,
      CRS_LINE       LIKE SY-STEPL.
FIELD-SYMBOLS: <F1>, <F2>, <NVAL>, <OVAL>.
DATA: BEGIN OF TABSTRIP_EXTAB OCCURS 0,
          OKCODE LIKE SY-TCODE,
      END OF TABSTRIP_EXTAB.
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
controls     TCTRL_ZAHLWEGE          TYPE TABLEVIEW USING SCREEN 1215.
DATA:    OK-CODE(5)     TYPE C.
TYPES:   BEGIN OF FCODE,
             OKCODE(5) TYPE C,
         END OF FCODE.
DATA:    ZWCNT(2)       TYPE P VALUE 0,     " Zaehler  (Zahlwege)
         ZAV_READ(1)    TYPE C.
TYPES:   TABLE_OF_FUNCTION_CODES TYPE STANDARD TABLE OF FCODE.
DATA:    EXCTAB TYPE TABLE_OF_FUNCTION_CODES.

CONTROLS T_1 TYPE TABLEVIEW USING SCREEN 9001.

DATA LT_T_1 LIKE ZFIEPROV OCCURS 0 WITH HEADER LINE.

DATA SEL.

DATA LV_COD_INT LIKE LT_T_1-COD_INT.
DATA:    BEGIN OF SORTTAB OCCURS 10,
           ARG(1)       TYPE C,             " Sortierfeld
         END OF SORTTAB.

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
  LV_CURSOR TYPE I,
  LV_CURSOR_FIELD TYPE NAME_KOMP,
  LV_CUR TYPE I,
  LV_ACRE TYPE C.

DATA LV_UCOMM TYPE SY-UCOMM.

* Verificaciones / Confirmaciones
DATA:

  LV_VERIF TYPE I,
  LV_CONFIRM TYPE I,
  LV_VERIF_T(80).


" Datos originales recuperados por cada fila del Table Control
TYPES: BEGIN OF ty_bp_recuperado,
  partner TYPE bu_partner,
  datos   TYPE zfieprov,
END OF ty_bp_recuperado.

DATA gt_bp_recuperado TYPE STANDARD TABLE OF ty_bp_recuperado WITH EMPTY KEY.

*----------------------------------------------------------------------*
***INCLUDE ZFI0002PBO .
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS '9001'.
  SET TITLEBAR '9001'.
ENDMODULE.                 " STATUS_9001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INIT_PROCESO  OUTPUT
*&---------------------------------------------------------------------*

MODULE init_proceso OUTPUT.
  DESCRIBE TABLE lt_t_1 LINES lv_lin.
  IF lv_ini IS INITIAL OR lv_lin IS INITIAL.
    CLEAR lt_t_1.
    APPEND lt_t_1.
    lv_ini = 1.
    lv_lin = 1.
    lv_cursor = 1.
  ENDIF.
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

  " Mantener una entrada de control por cada fila
  WHILE LINES( gt_bp_recuperado ) < LINES( lt_t_1 ).
    APPEND INITIAL LINE TO gt_bp_recuperado.
  ENDWHILE.

ENDMODULE.                 " INIT_PROCESO  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  set_value  OUTPUT
*&---------------------------------------------------------------------*
MODULE set_value OUTPUT.
*  loop at SCREEN.
*   if ( screen-name = 'ZFIEPROV-KTOKK' or
*      screen-name =  'ZFIEPROV-CIF' OR
*      screen-name =  'ZFIEPROV-NAME1' OR
*      screen-name =        'ZFIEPROV-NAME2' OR
*      screen-name =        'ZFIEPROV-DIREC' OR
*      screen-name =        'ZFIEPROV-DIREC_2' OR
*      screen-name =        'ZFIEPROV-COD_POST' OR
*      screen-name =        'ZFIEPROV-POBLAC' OR
*      screen-name =        'ZFIEPROV-PAIS' OR
*      screen-name =        'ZFIEPROV-REGION' OR
*      screen-name =        'ZFIEPROV-SPRAS' OR
*      screen-name =        'ZFIEPROV-TEL' OR
*      screen-name =        'ZFIEPROV-FAX' OR
*      screen-name =        'ZFIEPROV-SMTP' OR
*      screen-name =        'ZFIEPROV-DZSABE_K' ) AND
*      LV_ACRE = 'X'.
*     SCREEN-INPUT = '0'.
*    ELSEIF ( screen-name = 'ZFIEPROV-KTOKK' or
*      screen-name =  'ZFIEPROV-CIF' OR
*      screen-name =  'ZFIEPROV-NAME1' OR
*      screen-name =        'ZFIEPROV-NAME2' OR
*      screen-name =        'ZFIEPROV-DIREC' OR
*      screen-name =        'ZFIEPROV-DIREC_2'  OR
*      screen-name =        'ZFIEPROV-COD_POST' OR
*      screen-name =        'ZFIEPROV-POBLAC' OR
*      screen-name =        'ZFIEPROV-PAIS' OR
*      screen-name =        'ZFIEPROV-REGION' OR
*      screen-name =        'ZFIEPROV-SPRAS' OR
*      screen-name =        'ZFIEPROV-TEL' OR
*      screen-name =        'ZFIEPROV-FAX' OR
*      screen-name =        'ZFIEPROV-SMTP' OR
*      screen-name =        'ZFIEPROV-DZSABE_K' ) AND
*      LV_ACRE = ' '.
*    SCREEN-INPUT = '1'.
*    ENDIF.
*    MODIFY SCREEN.
*   endloop.
  " Recuperar la fila que se está mostrando
  READ TABLE lt_t_1
  INDEX t_1-current_line
  INTO DATA(ls_fila).

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  " Recuperar los valores originales de esa misma fila
  READ TABLE gt_bp_recuperado INDEX t_1-current_line
  INTO DATA(ls_bp).

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  " Determinar si existe una recuperación válida
  DATA(lv_recuperado) = xsdbool(
        ls_bp-partner IS NOT INITIAL
        AND ls_bp-partner = ls_fila-partner ).

  LOOP AT SCREEN.

    " Solo campos pertenecientes a datos generales del BP
    CHECK SCREEN-group1 = 'BPG'
    AND SCREEN-name CP 'ZFIEPROV-*'.

    " Estos campos del grupo se consideran editables por defecto
    SCREEN-INPUT = 1.

    IF lv_recuperado = abap_true.

      SPLIT SCREEN-name AT '-'
      INTO DATA(lv_estructura) DATA(lv_campo).

      " Consultar el valor ORIGINAL, no el actual
      ASSIGN COMPONENT lv_campo OF STRUCTURE ls_bp-datos
      TO FIELD-SYMBOL(<lv_valor>).

      IF sy-subrc = 0 AND <lv_valor> IS NOT INITIAL.

        " Si originalmente tenía valor, bloquear
        SCREEN-INPUT = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " set_value  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  t042z_lesen  OUTPUT
*&---------------------------------------------------------------------*
MODULE t042z_lesen OUTPUT.
  DESCRIBE TABLE a042z LINES refe1.
  DESCRIBE TABLE e042z LINES refe2.
  IF  refe1 = 0
  AND refe2 = 0.
    REFRESH: a042z, e042z.
*Marta Vall: 19.11.2008 - inicio modificación
*Hasta el momento solo cojia las vías de pago de españa
*ahora dependiendo del pais del proveedor a crear cojera las
*vías de pago associadas a este
*    SELECT * FROM t042z WHERE land1 = 'ES'
*                          AND   zlsch NE space.
    READ TABLE lt_t_1 INDEX 1.
    SELECT * FROM t042z WHERE land1 = lt_t_1-pais
                     AND   zlsch NE space.
*Marta Vall: 19.11.2008 - fin modificación
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
ENDMODULE.                    "pfstatus_d1215 OUTPUT

*----------------------------------------------------------------------*
***INCLUDE ZFI0002PAI .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  lv_ucomm = sy-ucomm.
  CASE lv_ucomm.
    WHEN 'STRT'.  " Enviar
      PERFORM tr_enviar.
      PERFORM tr_desmarcar.
    WHEN 'NEW'.   " Nuevo
      PERFORM tr_crear.
    WHEN 'PRE'.   " Grabar Pre
      PERFORM tr_grabar.
      PERFORM tr_desmarcar.
    WHEN 'DELP'.  " Borrar
      PERFORM tr_borrar.
      PERFORM tr_desmarcar.
    WHEN 'MALL'.  " Marcar
      PERFORM tr_marcar.
    WHEN 'RALL'.  " Desmarcar
      PERFORM tr_desmarcar.
    WHEN 'CONS'.  " Consulta
      PERFORM tr_cons.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_9001 INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " EXIT_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*&      Module  TRATAR_DATOS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tratar_datos INPUT.
*  IF zfieprov-partner IS NOT INITIAL AND lv_acre IS INITIAL.
*    SELECT SINGLE ktokk
*                  stcd1
*                  name1
*                  name2
*                  stras
*                  pstlz
*                  ort01
*                  land1
*                  regio
*                  spras
*                  telf1
*                  telfx
*                  lfurl
*                  adrnr
*                  konzs
*        FROM lfa1
*        INTO (zfieprov-bu_group,
*              zfieprov-cif,
*              zfieprov-name1,
*              zfieprov-name2,
*              zfieprov-direc,
*              zfieprov-cod_post,
*              zfieprov-poblac,
*              zfieprov-pais,
*              zfieprov-region,
*              zfieprov-spras,
*              zfieprov-tel,
*              zfieprov-fax,
*              zfieprov-smtp,
*              lv_adrnr,
*              zfieprov-c_fisc_mx )
*          WHERE lifnr = zfieprov-partner.
*    IF sy-subrc <> 0.
*      CLEAR lv_acre.
*      MESSAGE w203(zfi01).
*    ELSE.
*      lv_acre = 'X'.
*      SELECT SINGLE str_suppl1 FROM adrc INTO zfieprov-direc_2
*                    WHERE addrnumber = lv_adrnr.
*      SELECT SINGLE name1 FROM knvk INTO zfieprov-dzsabe_k
*                    WHERE lifnr = zfieprov-partner.
*      SELECT SINGLE zterm zwels FROM lfb1
*                    INTO (zfieprov-zterm, zfieprov-zwels)
*                    WHERE lifnr = zfieprov-partner AND
*                          bukrs = '5000'.
*      IF sy-subrc <> 0.
*        SELECT SINGLE zterm zwels FROM lfb1
*                   INTO (zfieprov-zterm, zfieprov-zwels)
*                   WHERE lifnr = zfieprov-partner.
*      ENDIF.
*    ENDIF.
*  ENDIF.

  DATA lv_adrnr TYPE lfa1-adrnr.

  " Localizar el control de la fila actual
  READ TABLE gt_bp_recuperado INDEX t_1-current_line
  ASSIGNING FIELD-SYMBOL(<ls_bp>).

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  DATA(lv_partner) = CONV bu_partner(
        |{ zfieprov-partner ALPHA = IN }| ).

  IF lv_partner IS INITIAL.

    " Sin BP: no hay datos recuperados que proteger
    CLEAR <ls_bp>.

  ELSEIF <ls_bp>-partner <> lv_partner.

    " BP diferente: descartar la recuperación anterior
    CLEAR: <ls_bp>, lv_adrnr.

    " Recuperación existente de datos generales
    SELECT SINGLE
    ktokk, stcd1, name1, name2,
    stras, pstlz, ort01, land1,
    regio, spras, telf1, telfx,
    adrnr, konzs, stcd3, stkzn
    FROM lfa1
    WHERE lifnr = @lv_partner
    INTO ( @zfieprov-bu_group,
    @zfieprov-cif,
    @zfieprov-name1,
    @zfieprov-name2,
    @zfieprov-direc,
    @zfieprov-cod_post,
    @zfieprov-poblac,
    @zfieprov-pais,
    @zfieprov-region,
    @zfieprov-spras,
    @zfieprov-tel,
    @zfieprov-fax,
    @lv_adrnr,
    @zfieprov-c_fisc_mx,
    @zfieprov-stcd3,
    @zfieprov-stkzn ).

    IF sy-subrc <> 0.

      MESSAGE w203(zfi01).

    ELSE.

      " Recuperar datos adicionales de dirección
      SELECT SINGLE str_suppl1
      FROM adrc
      WHERE addrnumber = @lv_adrnr
      INTO @zfieprov-direc_2.

      " Recuperar correo principal desde ADR6
      CLEAR zfieprov-smtp.

      SELECT SINGLE smtp_addr
      FROM adr6
      WHERE addrnumber = @lv_adrnr
      AND persnumber = @space
      AND flgdefault = @abap_true
      INTO @zfieprov-smtp.

      " Recuperar ramo predeterminado
      CLEAR zfieprov-brsch.

      SELECT SINGLE ind_sector
      FROM but0is
      WHERE partner = @lv_partner
      AND isdef = @abap_true
      INTO @zfieprov-brsch.

      " Persona de contacto
      SELECT SINGLE name1
      FROM knvk
      WHERE lifnr = @lv_partner
      INTO @zfieprov-dzsabe_k.

      " Datos de sociedad
      SELECT SINGLE zterm, zwels
      FROM lfb1
      WHERE lifnr = @lv_partner
*      AND bukrs = '5000'
      INTO ( @zfieprov-zterm, @zfieprov-zwels ).

      IF sy-subrc <> 0.
        SELECT SINGLE zterm, zwels
        FROM lfb1
        WHERE lifnr = @lv_partner
        INTO ( @zfieprov-zterm, @zfieprov-zwels ).
      ENDIF.

      " Completar NIF1, ramo y email desde datos del BP
      PERFORM recuperar_datos_bp CHANGING zfieprov.

      " Registrar BP recuperado
      <ls_bp>-partner = lv_partner.

      " Guardar SOLO valores procedentes de la recuperación
      <ls_bp>-datos = VALUE #(
      bu_group  = zfieprov-bu_group
      cif       = zfieprov-cif
      stcd3     = zfieprov-stcd3
      stkzn     = zfieprov-stkzn
      name1     = zfieprov-name1
      name2     = zfieprov-name2
      direc     = zfieprov-direc
      direc_2   = zfieprov-direc_2
      cod_post  = zfieprov-cod_post
      poblac    = zfieprov-poblac
      pais      = zfieprov-pais
      region    = zfieprov-region
      spras     = zfieprov-spras
      tel       = zfieprov-tel
      fax       = zfieprov-fax
      smtp      = zfieprov-smtp
      brsch     = zfieprov-brsch
      c_fisc_mx = zfieprov-c_fisc_mx
      dzsabe_k  = zfieprov-dzsabe_k ).

    ENDIF.
  ENDIF.

  " Actualizar únicamente la fila procesada
  MODIFY lt_t_1 FROM zfieprov INDEX t_1-current_line.

ENDMODULE.                 " TRATAR_DATOS  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZWELS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zwels INPUT.
  PERFORM help_knb1_zwels.
ENDMODULE.                 " ZWELS  INPUT
*&---------------------------------------------------------------------*
*&      Module  zahlweg_markieren  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zahlweg_markieren INPUT.
  CHECK t020-aktyp NE 'A'.
  CHECK ok-code = 'MARK'.

  GET CURSOR LINE crs_line FIELD crs_field.

  IF crs_line = sy-stepl.
    IF  crs_field(11) NE 'RF02D-XASEL'
    AND crs_field(11) NE 'RF02D-XESEL'.
      CLEAR: ok-code, crs_line, crs_field.
    ENDIF.
    ASSIGN (crs_field) TO <f1>.
    IF <f1> NE 'X'.
      <f1> = 'X'.
    ELSE.
      <f1> = space.
    ENDIF.
  ENDIF.
ENDMODULE.                 " zahlweg_markieren  INPUT
*&---------------------------------------------------------------------*
*&      Module  zahlweg_update  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zahlweg_update INPUT.
  CHECK t020-aktyp NE 'A'.

*  IF sy-stepl = 1.                                      " uh, 07.09.98
*    REFRESH sorttab.                                    " uh, 07.09.98
*  ENDIF.                                                " uh, 07.09.98

* Index der aktuellen Zeile:
  lindex = index + sy-stepl - 1.       " uh, 07.09.98

  IF rf02d-azsch NE space.
    a042z-xselk = rf02d-xasel.
    a042z-zlsch = rf02d-azsch.
    a042z-text1 = rf02d-aztxt.
*   MODIFY a042z INDEX sy-stepl.                         " uh, 07.09.98
    MODIFY a042z INDEX lindex.         " uh, 07.09.98
    IF a042z-xselk = 'X'.
      sorttab-arg = a042z-zlsch.
      APPEND sorttab.
    ELSE.                              " uh, 07.09.98
      DELETE sorttab WHERE arg = a042z-zlsch.            " uh, 07.09.98
    ENDIF.
  ENDIF.

  IF rf02d-ezsch NE space.
    e042z-xselk = rf02d-xesel.
    e042z-zlsch = rf02d-ezsch.
    e042z-text1 = rf02d-eztxt.
*   MODIFY e042z INDEX sy-stepl.                         " uh, 07.09.98
    MODIFY e042z INDEX lindex.         " uh, 07.09.98
    IF e042z-xselk = 'X'.
      sorttab-arg = e042z-zlsch.
      APPEND sorttab.
    ELSE.                              " uh, 07.09.98
      DELETE sorttab WHERE arg = e042z-zlsch.           " uh, 07.09.98
    ENDIF.
  ENDIF.
ENDMODULE.                 " zahlweg_update  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZAHLWEG_LEISTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zahlweg_leiste INPUT.
  CHECK t020-aktyp NE 'A'.

  CLEAR zfieprov-zwels.
  zwcnt = 0.
  SORT sorttab DESCENDING.
  DELETE ADJACENT DUPLICATES FROM sorttab.       " uh, 07.09.98
  LOOP AT sorttab.
    SHIFT zfieprov-zwels RIGHT.
    zfieprov-zwels(1) = sorttab-arg.
    zwcnt         = zwcnt + 1.
  ENDLOOP.

*------- Wurden mehr als 10 Zahlwege angekreuzt ? ----------------------
  IF zwcnt > 10.
    SET SCREEN sy-dynnr.
    LEAVE SCREEN.
  ELSE.
    zwcnt = 0.
  ENDIF.

  IF ok-code = 'MARK'.
    CLEAR: ok-code.
    SET SCREEN sy-dynnr.
    LEAVE SCREEN.
  ENDIF.
ENDMODULE.                 " ZAHLWEG_LEISTE  INPUT
*&---------------------------------------------------------------------*
*&      Module  TCTRL_ZAHLWEGE_BLAETTERN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tctrl_zahlwege_blaettern INPUT.
*Beim Blättern nach Markieren kommt kein ok-code, aber index <> top_line
  IF index NE tctrl_zahlwege-top_line AND ok-code IS INITIAL.
    index = tctrl_zahlwege-top_line.
    SET SCREEN sy-dynnr.
    LEAVE SCREEN.
  ENDIF.

  index = tctrl_zahlwege-top_line.

  IF ok-code(2) = 'P-'
  OR ok-code(2) = 'P+'.
    PERFORM fenster_blaettern.
  ENDIF.
ENDMODULE.                 " TCTRL_ZAHLWEGE_BLAETTERN  INPUT
*&---------------------------------------------------------------------*
*&      Module  VALIDA_NIF  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE valida_nif INPUT.
*  valida curp

  DATA: lv_tamano       TYPE i,
        lv_valido       TYPE char1,
        lv_text1        TYPE char100,
        lv_text2        TYPE char100,
        lv_bp_duplicado TYPE bu_partner.
  CLEAR: lv_valido, lv_text1, lv_text2, lv_bp_duplicado.
  " Normalizar NIF
  CONDENSE zfieprov-cif NO-GAPS.

  " Actualizar la fila del Table Control
  MODIFY lt_t_1 FROM zfieprov INDEX t_1-current_line.

*  DESCRIBE FIELD zfieprov-cif LENGTH lv_tamano in character mode.
  lv_tamano = strlen( zfieprov-cif ).
  IF lv_tamano EQ 13 AND zfieprov-stcd3 IS INITIAL."pedimos el curp.
    MESSAGE e368(00) WITH 'Debe de capturar CURP en campo NIF 3'.
  ENDIF.
  IF lv_tamano EQ 13 AND zfieprov-stcd3 IS NOT INITIAL AND
    zfieprov-stkzn EQ space."pedimos marca persona fisica
    MESSAGE e368(00) WITH 'Debe de capturar indicador persona fisica'.
  ENDIF.
*  usar validacion igual que la transaccion standard
*  aayala 02-01-2012
  IF zfieprov-pais EQ 'MX'.
*    SELECT SINGLE *
*             FROM lfa1
*            WHERE stcd1 = zfieprov-cif.
*    IF sy-subrc = 0.
*      SELECT SINGLE *
*               FROM lfb1
*              WHERE bukrs = zfieprov-bukrs AND
*                    lifnr = lfa1-lifnr.
*      IF sy-subrc = 0.
*        text1 = 'Error en RFC'.
*        text2 = 'El RFC está duplicado, verifíquelo'.
*        valido = 'X'.
*      ENDIF.
*    ELSE.
*     -- validar RFC o CURP
    CALL FUNCTION 'ZVALIDACION_RFC_CURP'
      EXPORTING
        fisica       = zfieprov-stkzn
        nif_g        = zfieprov-cif
        nif_p        = zfieprov-stcd3
      IMPORTING
        nif_correcto = lv_valido
        text1        = lv_text1
        text2        = lv_text2.
*    ENDIF.

    IF lv_valido EQ 'X'.
      MESSAGE e368(00) WITH lv_text1 lv_text2.
    ENDIF.
  ENDIF.

  " Validar duplicados para cualquier país
  " Busca en DFKKBPTAXNUM por tipo de NIF
  " y excluye el BP actual cuando es una ampliación
  PERFORM validar_nif_duplicado
  USING    zfieprov
  CHANGING lv_bp_duplicado.

  IF lv_bp_duplicado IS NOT INITIAL.
    MESSAGE e368(00)
    WITH 'NIF duplicado en BP' lv_bp_duplicado.
  ENDIF.

ENDMODULE.                 " VALIDA_NIF  INPUT

*----------------------------------------------------------------------*
***INCLUDE ZFI0002FRM .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  TR_ENVIAR
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM tr_enviar .

  lv_verif = 0.

  PERFORM verif.

  IF NOT lv_verif IS INITIAL.   " Linea error

    PERFORM crear_message.

  ELSE.

    PERFORM confirm.

    IF NOT lv_confirm IS INITIAL.   " Confirmado envío

      PERFORM tr_enviar_conf.

    ENDIF.

  ENDIF.

ENDFORM.                    " TR_ENVIAR
*&---------------------------------------------------------------------*
*&      Form  TR_CREAR
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM tr_crear .

  CLEAR lt_t_1.
  APPEND lt_t_1.
  APPEND INITIAL LINE TO gt_bp_recuperado.

ENDFORM.                    " TR_CREAR
*&---------------------------------------------------------------------*
*&      Form  TR_GRABAR
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM tr_grabar .

  DATA: lv_index TYPE i,
        lv_lines TYPE i,
        lv_mes   TYPE i.

*Marta: 29.10.2008 - inicio modficación
  DATA: va_land1 TYPE t001-land1.
*Marta: 29.10.2008 - fin modificación

  PERFORM ini_cod_int.

  LOOP AT lt_t_1 WHERE sele = 'X'.

    lv_index = sy-tabix.
*Marta: 29.10.2008 - inicio modificación
*En UK siempre marcaremos en pincho de replicar
    SELECT SINGLE land1
             INTO va_land1
             FROM t001
            WHERE bukrs EQ lt_t_1-bukrs.

    IF va_land1 EQ 'GB'.
      lt_t_1-repl = 'X'.
    ENDIF.
*Marta: 29:10.2008 - fin modificación
    MOVE-CORRESPONDING lt_t_1 TO zfitprov.
    zfitprov-busq = zfitprov-name1.
    zfitprov-fecha = sy-datum.
    zfitprov-estado = 'P'.
    zfitprov-estdes = 'H'.
    lv_cod_int = lv_cod_int + 1.
    zfitprov-cod_int = lv_cod_int.
    INSERT zfitprov.
    DELETE lt_t_1 INDEX lv_index.
    DELETE gt_bp_recuperado INDEX lv_index.
    lv_mes = 1.

  ENDLOOP.

  PERFORM fin_cod_int.

  DESCRIBE TABLE lt_t_1 LINES lv_lines.

  IF lv_lines IS INITIAL.

    lv_ini = 0.

  ENDIF.

  IF NOT lv_mes IS INITIAL.
    MESSAGE s022(zfi01).
  ENDIF.


ENDFORM.                    " TR_GRABAR
*&---------------------------------------------------------------------*
*&      Form  TR_BORRAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_borrar .

  DATA: lv_index TYPE i,
        lv_lines TYPE i,
        lv_mes   TYPE i.

  LOOP AT lt_t_1 WHERE sele = 'X'.

    lv_index = sy-tabix.
    DELETE lt_t_1 INDEX lv_index.
    DELETE gt_bp_recuperado INDEX lv_index.
    lv_mes = 1.

  ENDLOOP.

  DESCRIBE TABLE lt_t_1 LINES lv_lines.

  IF lv_lines IS INITIAL.

    lv_ini = 0.

  ENDIF.

  IF NOT lv_mes IS INITIAL.
    MESSAGE s023(zfi01).
  ENDIF.

ENDFORM.                    " TR_BORRAR
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

    " Verificar que el NIF no pertenece a otro BP
    DATA lv_bp_duplicado TYPE bu_partner.

    CLEAR lv_bp_duplicado.

    PERFORM validar_nif_duplicado USING lt_t_1 CHANGING lv_bp_duplicado.

    IF lv_bp_duplicado IS NOT INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-CIF'.
      lv_verif_t =
      |NIF duplicado en BP { lv_bp_duplicado }|.

      EXIT.
    ENDIF.


* Mod validación RNC - AS
    DATA: lc_error TYPE char1,
          lc_text  TYPE char100.
    CLEAR: lc_error, lc_text.
    IF lt_t_1-cif IS NOT INITIAL AND lt_t_1-pais EQ 'DO'.
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

    CALL FUNCTION 'TAX_NUMBER_CHECK'
      EXPORTING
        country             = lt_t_1-pais
        natural_person_flag = lt_t_1-stkzn
        tax_code_1          = lt_t_1-cif
        tax_code_3          = lt_t_1-stcd3
      EXCEPTIONS
        not_valid           = 1
        different_fprcd     = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-CIF'.
      PERFORM texto_error USING 'STCD1' TEXT-016 CHANGING lv_verif_t.
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
*Marta: 29.10.2008 - inicio mofidifación
*La calle no puede ser mayor de 35 caracteres ya que despues se va al LFA1-SPRAS, eso pasa cuando no hay organización de compras
*    ELSE.
*      DATA: va_num TYPE i.
*
*      SELECT COUNT(*) FROM t024w WHERE werks EQ lt_t_1-bukrs.
*      IF sy-subrc EQ 4.
*        va_num = strlen( lt_t_1-direc ).
*        IF va_num GT 35.
*          DATA(lv_calle_completa) = lt_t_1-direc.
**          CLEAR lt_t_1-direc.
**          lt_t_1-direc = lv_calle_completa+35.
**          lt_t_1-direc_2 = lv_calle_completa(35).
*
**          lv_verif = sy-tabix.
**          lv_verif_t = text-002.
*        ENDIF.
*      ENDIF.
*      EXIT.

*Marta: 29.10.2008 - fin modificación
    ENDIF.

    IF lt_t_1-smtp IS INITIAL.
      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-SMTP'.

      PERFORM texto_error
      USING 'SMTP_ADDR' TEXT-001
      CHANGING lv_verif_t.
      EXIT.
    ENDIF.

    IF lt_t_1-cod_post IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-COD_POST'.
      PERFORM texto_error USING 'AD_PSTCD1' TEXT-001 CHANGING lv_verif_t.
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

    IF lt_t_1-tel IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-TEL'.
      PERFORM texto_error USING 'AD_TLNMBR1' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

    IF lt_t_1-fax IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-FAX'.
      PERFORM texto_error USING 'AD_FXNMBR1' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

    IF lt_t_1-spras IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-SPRAS'.
      PERFORM texto_error USING 'SPRAS' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

*   IF LT_T_1-DZSABE_K IS INITIAL.
*     LV_VERIF = SY-TABIX.
*     LV_CURSOR_FIELD = 'ZFIEPROV-DZSABE_K'.
*     PERFORM TEXTO_ERROR USING 'DZSABE_K' TEXT-001 CHANGING LV_VERIF_T.
*     EXIT.
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

    IF lt_t_1-zwels IS INITIAL.

      lv_verif = sy-tabix.
      lv_cursor_field = 'ZFIEPROV-ZWELS'.
      PERFORM texto_error USING 'DZLSCH' TEXT-001 CHANGING lv_verif_t.
      EXIT.

    ENDIF.

*) Inicio de modificación Edgar Béjar / ebejar 11.09.2020
*) Se quita la validación de campo obligatorio a Clave de Grupo para darle su uso real.
*) Se comentaron las siguientes líneas
*    IF lt_t_1-c_fisc_mx IS INITIAL."se elimino para RSD  01.12.2011 aayala
*      SELECT SINGLE * FROM t001 WHERE bukrs = lt_t_1-bukrs.
*      IF t001-land1 = 'MX'.
*        lv_verif = sy-tabix.
*        lv_cursor_field = 'ZFIEPROV-C_FISC_MX'.
*        PERFORM texto_error USING 'KONZS' text-001 CHANGING lv_verif_t.
*        EXIT.
*      ENDIF.
*    ENDIF.
*) Fin de modificación Edgar Béjar / ebejar 11.09.2020

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
*&      Form  TR_ENVIAR_CONF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tr_enviar_conf .

  DATA:  lv_index TYPE i.
*Marta: 29.10.2008 - inicio modificación
  DATA: va_land1 TYPE t001-land1.
*Marta: 29.10.2008 - fin modificación
  PERFORM ini_cod_int.

  LOOP AT lt_t_1 WHERE  sele = 'X'.

    lv_index = sy-tabix.
*Marta: 29.10.2008 - inicio modificación
*En UK siempre marcaremos en pincho de replicar
    SELECT SINGLE land1
             INTO va_land1
             FROM t001
            WHERE bukrs EQ lt_t_1-bukrs.

    IF va_land1 EQ 'GB'.
      lt_t_1-repl = 'X'.
    ENDIF.
*Marta: 29:10.2008 - fin modificación
    MOVE-CORRESPONDING lt_t_1 TO zfitprov.
    zfitprov-busq = zfitprov-name1.
    zfitprov-fecha = sy-datum.
    lv_cod_int = lv_cod_int + 1.
    zfitprov-cod_int = lv_cod_int.
    zfitprov-estado = 'E'.
    INSERT zfitprov.
    DELETE lt_t_1 INDEX lv_index.
    DELETE gt_bp_recuperado INDEX lv_index.

  ENDLOOP.

  PERFORM fin_cod_int.

ENDFORM.                    " TR_ENVIAR_CONF
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

    LEAVE TO TRANSACTION 'ZBP003'.

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
*&      Form  TEXTO_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM texto_error  USING    l_edat
                           l_txto
                  CHANGING l_txt.

  DATA le_dd04t LIKE dd04t.

*Marta Vall: 19.11.2008 - inicio modificación
  SELECT * UP TO 1 ROWS FROM dd04t INTO le_dd04t
                            WHERE rollname = l_edat AND
*                                  ddlanguage = 'SY-LANGU' AND
                                  ddlanguage = sy-langu AND
                                  as4local = 'A'
                        ORDER BY as4vers DESCENDING.
  ENDSELECT.
*Marta Vall: 19.11.2008 - fin modificación

  l_txt = l_txto.
  REPLACE '&1' WITH le_dd04t-scrtext_m INTO l_txt.

ENDFORM.                    " TEXTO_ERROR

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



FORM recuperar_datos_bp CHANGING cs_prov TYPE zfieprov.

  DATA(lv_partner) = CONV bu_partner(
        |{ cs_prov-partner ALPHA = IN }| ).

  DATA(lv_taxtype) = CONV dfkkbptaxnum-taxtype(
        |{ cs_prov-pais }1| ).

  " Recuperar NIF1 según categoría fiscal
  SELECT SINGLE taxnumxl, taxnum
  FROM dfkkbptaxnum
  WHERE partner = @lv_partner
  AND taxtype = @lv_taxtype
  INTO @DATA(ls_tax).

  IF sy-subrc = 0.
    cs_prov-cif = COND #(
    WHEN ls_tax-taxnumxl IS NOT INITIAL
    THEN ls_tax-taxnumxl
    ELSE ls_tax-taxnum ).
  ENDIF.

  " Recuperar ramo predeterminado del BP
  SELECT SINGLE ind_sector
  FROM but0is
  WHERE partner = @lv_partner
  AND isdef = @abap_true
  INTO @cs_prov-brsch.

  " Recuperar dirección del proveedor
  SELECT SINGLE adrnr
  FROM lfa1
  WHERE lifnr = @lv_partner
  INTO @DATA(lv_adrnr).

  " Recuperar correo electrónico principal
  IF lv_adrnr IS NOT INITIAL.
    SELECT SINGLE smtp_addr
    FROM adr6
    WHERE addrnumber = @lv_adrnr
    AND persnumber = @space
    AND flgdefault = @abap_true
    INTO @cs_prov-smtp.
  ENDIF.

ENDFORM.

FORM validar_nif_duplicado
USING    is_prov TYPE zfieprov
CHANGING cv_duplicado TYPE bu_partner.

  CLEAR cv_duplicado.

  CHECK is_prov-cif IS NOT INITIAL.
  CHECK is_prov-pais IS NOT INITIAL.

  " Misma categoría que utiliza ZFI0009
  DATA(lv_taxtype) = CONV dfkkbptaxnum-taxtype(
        |{ is_prov-pais }1| ).

  " Excluir el BP actual durante una ampliación
  DATA(lv_partner) = CONV bu_partner(
        |{ is_prov-partner ALPHA = IN }| ).

  SELECT SINGLE partner
  FROM dfkkbptaxnum
  WHERE taxtype = @lv_taxtype
  AND ( taxnumxl = @is_prov-cif
  OR taxnum   = @is_prov-cif )
  AND partner <> @lv_partner
  INTO @cv_duplicado.

ENDFORM.