************************************************************************
*                                                                      *
*  Überweisungs-Druckprogramm RFFOUS_T (USA) ACH format                *
*  Print program for bank transfer RFFOUS_T (USA) ACH format           *
*                                                                      *
************************************************************************
*
*----------------------------------------------------------------------*
* Das Programm includiert:                                             *
*                                                                      *
* RFFORI0M  Makrodefinition für den Selektionsbildaufbau               *
* RFFORI00  Deklarationsteil der Zahlungsträger-Druckprogramme         *
* RFFORIUS  Deklararionsteil fuer RFFORIU4                             *
* RFFORIU4  Datenträgeraustausch USA (ACH format)                      *
* RFFORI06  Avis                                                       *
* RFFORI07  Zahlungsbegleitliste                                       *
* RFFORI99  Allgemeine Unterroutinen der Zahlungsträger-Druckprogramme *
*----------------------------------------------------------------------*
* The program includes:                                                *
*                                                                      *
* RFFORI0M  definition of macros                                       *
* RFFORI00  international data definitions                             *
* RFFORIUS  data definitions for RFFORIU4                              *
* RFFORIU4  domestic transfer (DME) USA (ACH format)                   *
* RFFORI06  remittance advice                                          *
* RFFORI07  payment summary list                                       *
* RFFORI99  international subroutines                                  *
*----------------------------------------------------------------------*



*----------------------------------------------------------------------*
* Report Header                                                        *
*----------------------------------------------------------------------*
REPORT rffous_t
  LINE-SIZE 132
  MESSAGE-ID f0
  NO STANDARD PAGE HEADING.



*----------------------------------------------------------------------*
*  Segments                                                            *
*----------------------------------------------------------------------*
TABLES:
  reguh,
  regup,
  rfsdo.


DATA items TYPE hrpayus_remitkey OCCURS 0 WITH HEADER LINE.
DATA: l_err_garn LIKE bapiret2,
      l_dest LIKE tbdestination-rfcdest,
      l_function TYPE rs38l_fnam.

*----------------------------------------------------------------------*
*  Macro definitions                                                   *
*----------------------------------------------------------------------*
INCLUDE zrffori0m.
*INCLUDE RFFORI0M.

INITIALIZATION.

*----------------------------------------------------------------------*
*  Parameters / Select-Options                                         *
*----------------------------------------------------------------------*
  block 1.
  SELECT-OPTIONS:
    sel_zawe FOR  reguh-rzawe,         "Zahlwege / payment methods
    sel_secc FOR  rfsdo-fordsecc,      "Standard Entry Class Code
    sel_uzaw FOR  reguh-uzawe,         "Zahlwegzusatz
    sel_hbki FOR  reguh-hbkid,         "house bank short key
    sel_hkti FOR  reguh-hktid,         "account data short key
    sel_waer FOR  reguh-waers,         "currency
    sel_vbln FOR  reguh-vblnr.         "payment document number
  SELECTION-SCREEN END OF BLOCK 1.

  block 2.
  auswahl: xdta w, avis a, begl b.
  spool_authority.                     "Spoolberechtigung
  SELECTION-SCREEN END OF BLOCK 2.

  block 3.
  PARAMETERS:
    par_unix LIKE rfpdo2-fordnamd,     "Dateiname für DTA und TemSe
    par_dtyp LIKE rfpdo-forddtyp,      "Ausgabeformat und -medium
    par_f_id LIKE dtausfh-fh7,         "ID File modifier
    par_c_id LIKE dtausbh-bh7,         "default: company entry descript.
    par_anzp LIKE rfpdo-fordanzp,      "number of test prints
    par_maxp LIKE rfpdo-fordmaxp,      "number of items in summary list
    par_maxa LIKE rfpdo2-fordmaxa,     "number of addenda records
    par_addn LIKE rfpdo2-fordaddn,     "addenda records
    par_belp LIKE rfpdo-fordbelp,      "payment doc. validation
    par_espr LIKE rfpdo-fordespr,      "texts in reciepient's lang.
    par_isoc LIKE rfpdo-fordisoc.      "currency in ISO code
  SELECTION-SCREEN END OF BLOCK 3.

  PARAMETERS:
    par_anzb LIKE rfpdo2-fordanzb NO-DISPLAY,
    par_zdru LIKE rfpdo-fordzdru  NO-DISPLAY,
    par_priz LIKE rfpdo-fordpriz  NO-DISPLAY,
    par_sofz LIKE rfpdo1-fordsofz NO-DISPLAY,
    par_vari(12) TYPE c           NO-DISPLAY,
    par_sofo(1)  TYPE c           NO-DISPLAY.



*----------------------------------------------------------------------*
*  Vorbelegung der Parameter und Select-Options                        *
*  default values for parameters and select-options                    *
*----------------------------------------------------------------------*
  PERFORM init.
  sel_zawe-low     = 'T'.
  sel_zawe-option  = 'EQ'.
  sel_zawe-sign    = 'I'.
  APPEND sel_zawe.
  CLEAR sel_zawe.
  sel_zawe-low     = 'U'.
  sel_zawe-option  = 'EQ'.
  sel_zawe-sign    = 'I'.
  APPEND sel_zawe.

  par_belp = space.
  par_zdru = space.
  par_xdta = 'X'.
  par_dtyp = '0'.
  par_avis = 'X'.
  par_begl = 'X'.
  par_anzp = 2.
  par_anzb = 2.
  par_espr = space.
  par_isoc = space.
  par_maxp = 9999.
  par_maxa = 9999.
  par_f_id = 'A'.
  par_addn = space.



*----------------------------------------------------------------------*
*  tables / fields / field-groups / AT SELECTION SCREEN                *
*----------------------------------------------------------------------*
  INCLUDE zrffori00_1.
*  INCLUDE RFFORI00.


*- Prüfungen bei DTA ---------------------------------------------------
*- special checks for US DME -------------------------------------------
  IF par_xdta EQ 'X'.                    "Datenträgeraustausch / DME
    IF par_dtyp EQ space.
      par_dtyp = '0'.                    "TemSe
    ENDIF.
    IF par_dtyp NA '01'.
      SET CURSOR FIELD 'PAR_DTYP'.
      MESSAGE e068.                   "this message and the docu must be
    ENDIF.                            "changed. Allowed: 0 = TemSe,
  ENDIF.                              "                  1 = File system



*----------------------------------------------------------------------*
*  Kopfzeilen (nur bei der Zahlungsbegleitliste)                       *
*  batch heading (for the payment summary list)                        *
*----------------------------------------------------------------------*
TOP-OF-PAGE.

  IF flg_begleitl EQ 1.
    PERFORM kopf_zeilen.                                    "RFFORI07
  ENDIF.



*----------------------------------------------------------------------*
*  Felder vorbelegen                                                   *
*  preparations                                                        *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  hlp_auth  = par_auth.                "spool authority

  hlp_temse = '0---------'.


  PERFORM vorbereitung.



*----------------------------------------------------------------------*
*  Unterprogramm Datendefinitionen fuer RFFORIU4                       *
*  data definitions for RFFORIU4                                       *
*----------------------------------------------------------------------*
  INCLUDE zrfforius.
*  INCLUDE RFFORIUS.



*----------------------------------------------------------------------*
*  Daten prüfen und extrahieren                                        *
*  check and extract data                                              *
*----------------------------------------------------------------------*
GET reguh.

  CHECK sel_zawe.
  CHECK reguh-bkref(3) IN sel_secc.
  CHECK sel_uzaw.
  CHECK sel_hbki.
  CHECK sel_hkti.
  CHECK sel_waer.
  CHECK sel_vbln.

  IF reguh-bkref EQ ' ' OR
   ( reguh-bkref(3) NE 'PPD' AND
     reguh-bkref(3) NE 'CCD' AND
     reguh-bkref(3) NE 'CTX' ).
    CASE zw_laufi+5(1).
      WHEN 'P'.
        reguh-bkref(3) = 'PPD'.
      WHEN OTHERS.
        reguh-bkref(3) = 'CCD'.
    ENDCASE.
  ENDIF.
  PERFORM pruefung.
  PERFORM pruefung_betrag USING 10 reguh-rwbtr.
  PERFORM extract_vorbereitung.


GET regup.

  IF reguh-paygr IS INITIAL.
    reguh-paygr = par_c_id.
  ENDIF.
  hlp_sort+0(16) = reguh-paygr.
  hlp_sort+16(8) = reguh-ausfd.
  PERFORM extract.

  IF regup-xblnr(5) = 'HRGRN'.
    items-comp_code = regup-bukrs.
    items-bus_area = regup-gsber.
    items-ref_doc_no = regup-xblnr.
    items-item_text = regup-sgtxt.
    IF items-item_text CN '* '.
      WHILE items-item_text(1) CA '*'.
        SHIFT items-item_text.
      ENDWHILE.
    ENDIF.
    APPEND items.
  ENDIF.


*----------------------------------------------------------------------*
*  Bearbeitung der extrahierten Daten                                  *
*  print forms, DME, remittance advices and lists                      *
*----------------------------------------------------------------------*
END-OF-SELECTION.

  READ TABLE tab_rfc INDEX 1.
  IF sy-subrc NE 0.
    CALL FUNCTION 'HRPAYUS_REMIT_INFO_CREATE_LIST'
      TABLES
        items  = items
      EXCEPTIONS
        OTHERS = 1.
  ELSE.
    l_dest = tab_rfc-dest.
    LOOP AT tab_rfc.
      IF tab_rfc-dest <> l_dest.
        CLEAR l_dest.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF NOT l_dest IS INITIAL.
      CALL FUNCTION 'HRPAYUS_REMIT_INFO_CREATE_LIST'
        DESTINATION
        tab_rfc-dest
        TABLES
          items  = items
        EXCEPTIONS
          OTHERS = 1.
    ENDIF.
  ENDIF.

  IF flg_selektiert NE 0.

    IF par_xdta EQ 'X'.
      PERFORM dme_domestic.                                 "RFFORIU4
    ENDIF.

    IF par_avis EQ 'X'.
      PERFORM avis.                                         "RFFORI06
      IF NOT tab_ausgabe[] IS INITIAL.
        EXPORT tab_ausgabe  TO MEMORY ID 'TAB_AUSGABE'.
      ENDIF.
    ENDIF.

    IF par_begl EQ 'X' AND par_maxp GT 0.
      flg_bankinfo = 2.
      PERFORM begleitliste.                                 "RFFORI07
    ENDIF.

  ENDIF.

  PERFORM error_messages_rffous_t.

  PERFORM fehlermeldungen.

  PERFORM information.



*----------------------------------------------------------------------*
*  Unterprogramm Datenträgeraustausch Inland                           *
*  subroutine for DME                                                  *
*----------------------------------------------------------------------*
  INCLUDE zrfforiu4.
*  INCLUDE RFFORIU4.



*----------------------------------------------------------------------*
*  Unterprogramm Avis ohne Allongeteil                                 *
*  subroutine for remittance advices                                   *
*----------------------------------------------------------------------*
  INCLUDE zrffori06_1.
*  INCLUDE RFFORI06.



*----------------------------------------------------------------------*
*  Unterprogramm Begleitliste                                          *
*  subroutine for the summary list                                     *
*----------------------------------------------------------------------*
  INCLUDE zrffori07_1.
*  INCLUDE RFFORI07.



*----------------------------------------------------------------------*

************************************************************************
*                                                                      *
* Includebaustein RFFORI06 zu den Formulardruckprogrammen RFFOxxxz     *
* mit Unterprogrammen für den Druck des Avises                         *
*                                                                      *
************************************************************************

*----------------------------------------------------------------------*
* FORM AVIS                                                            *
*----------------------------------------------------------------------*
* Druck Avis                                                           *
* Gerufen von END-OF-SELECTION (RFFOxxxz)                              *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM avis.

  DATA:
    l_form  LIKE itcta-tdform,
    l_pages LIKE itctg OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Abarbeiten der extrahierten Daten                                    *
*----------------------------------------------------------------------*
  IF flg_sort NE 2.
    SORT BY avis.
    flg_sort = 2.
  ENDIF.

  LOOP.


*-- Neuer zahlender Buchungskreis --------------------------------------
    AT NEW reguh-zbukr.

      IF NOT reguh-zbukr IS INITIAL.   "FPAYM
        PERFORM buchungskreis_daten_lesen.
      ENDIF.                           "FPAYM

    ENDAT.


*-- Neuer Zahlweg ------------------------------------------------------
    AT NEW reguh-rzawe.

      flg_probedruck = 0.              "für diesen Zahlweg wurde noch
      flg_sgtxt      = 0.              "kein Probedruck durchgeführt

      IF reguh-rzawe NE space.
        PERFORM zahlweg_daten_lesen.

*       Zahlungsformular nur zum Lesen öffnen
        IF NOT t042e-zforn IS INITIAL.
          CALL FUNCTION 'OPEN_FORM'
            EXPORTING
              form     = t042e-zforn
              dialog   = space
              device   = 'SCREEN'
              language = t001-spras
            EXCEPTIONS
              form     = 1.

          IF sy-subrc EQ 0.            "Formular existiert
*           Formular auf Segmenttext (Global &REGUP-SGTXT) untersuchen
            IF par_xdta EQ space.
              IF t042e-xavis NE space AND t042e-anzpo NE 99.
                CALL FUNCTION 'READ_FORM_LINES'
                  EXPORTING
                    element = hlp_ep_element
                  TABLES
                    lines   = tab_element
                  EXCEPTIONS
                    element = 1.
                IF sy-subrc EQ 0.
                  LOOP AT tab_element.
                    IF    tab_element-tdline   CS 'REGUP-SGTXT'
                      AND tab_element-tdformat NE '/*'.
                      flg_sgtxt = 1.   "Global für Segmenttext existiert
                      EXIT.
                    ENDIF.
                  ENDLOOP.
                ENDIF.
              ENDIF.
            ENDIF.
            CALL FUNCTION 'CLOSE_FORM'.
          ENDIF.
        ENDIF.
      ENDIF.

*     Überschrift für den Formularabschluß modifizieren
      t042z-text1 = text_001.

*     Vorschlag für die Druckparameter aufbauen
      PERFORM fill_itcpo USING par_pria
                               'LIST5S'
                               space   "par_sofa via tab_ausgabe!
                               hlp_auth.

      EXPORT itcpo TO MEMORY ID 'RFFORI06_ITCPO'.

    ENDAT.


*-- Neue Hausbank ------------------------------------------------------
    AT NEW reguh-ubnkl.

      PERFORM hausbank_daten_lesen.

*     Felder für Formularabschluß initialisieren
      cnt_avise      = 0.
      cnt_avedi      = 0.
      cnt_avfax      = 0.
      cnt_avmail     = 0.
      sum_abschluss  = 0.
      sum_abschl_edi = 0.
      sum_abschl_fax = 0.
      REFRESH tab_edi_avis.

      flg_druckmodus = 0.
    ENDAT.


*-- Neue Empfängerbank -------------------------------------------------
    AT NEW reguh-zbnkl.

      PERFORM empfbank_daten_lesen.

    ENDAT.


*-- Neue Zahlungsbelegnummer -------------------------------------------
    AT NEW reguh-vblnr.

*     Prüfe, ob Avis auf Papier erzwungen wird
*     Check if advice on paper is forced
      IF flg_papieravis EQ 1.
        reguh-ediav = space.
      ENDIF.

*     Prüfe, ob HR-Formular zu verwenden ist
*     Check if HR-form is to be used
      hrxblnr = regup-xblnr.
      IF ( hlp_laufk EQ 'P' OR
           hrxblnr-txtsl EQ 'HR' AND hrxblnr-txerg EQ 'GRN' )
       AND hrxblnr-xhrfo NE space.
        hlp_xhrfo = 'X'.
      ELSE.
        hlp_xhrfo = space.
      ENDIF.

*     HR-Formular besorgen
*     read HR form
      IF hlp_xhrfo EQ 'X'.
        PERFORM hr_formular_lesen.
      ENDIF.

*     Prüfung, ob Avis erforderlich
      cnt_zeilen = 0.
      IF hlp_xhrfo EQ space.
        IF flg_sgtxt EQ 1.
          cnt_zeilen = reguh-rpost + reguh-rtext.
        ELSE.
          cnt_zeilen = reguh-rpost.
        ENDIF.
      ELSE.
        DESCRIBE TABLE pform LINES cnt_zeilen.
      ENDIF.
      flg_kein_druck = 0.
      IF reguh-ediav EQ 'V'.
*       Avis bereits versendet
        flg_kein_druck = 1.            "kein Druck erforderlich
      ELSEIF reguh-rzawe NE space AND t042e-xsavi IS INITIAL.
*       Avis zu Formular
        IF hlp_zeilen EQ 0 AND par_xdta EQ space.
          IF t042e-xavis EQ space OR cnt_zeilen LE t042e-anzpo.
            flg_kein_druck = 1.        "kein Druck erforderlich
          ENDIF.
*       Avis zum DTA
        ELSE.
          CLEAR tab_kein_avis.
          MOVE-CORRESPONDING reguh TO tab_kein_avis.
          READ TABLE tab_kein_avis.
          IF sy-subrc EQ 0.
            flg_kein_druck = 1.        "kein Druck erforderlich
          ENDIF.
        ENDIF.
      ENDIF.

      PERFORM fpaym USING 1.           "FPAYM
      PERFORM zahlungs_daten_lesen.
      IF reguh-ediav NA ' V' AND hlp_xhrfo EQ space.
        PERFORM summenfelder_initialisieren.
      ENDIF.

*     Schecknummer bei vornumerierten Schecks
      CLEAR regud-chect.
      READ TABLE tab_schecks WITH KEY
        zbukr = reguh-zbukr
        vblnr = reguh-vblnr.
      IF sy-subrc EQ 0.
        regud-chect = tab_schecks-chect.
      ELSEIF flg_schecknum EQ 1.
        IF zw_xvorl EQ space.
          IF hlp_laufk NE 'P'.         "FI-Beleg vorhanden?
            SELECT * FROM payr
              WHERE zbukr EQ reguh-zbukr
              AND   vblnr EQ reguh-vblnr
              AND   gjahr EQ regud-gjahr
              AND   voidr EQ 0.
            ENDSELECT.
            sy-msgv1 = reguh-zbukr.
            sy-msgv2 = regud-gjahr.
            sy-msgv3 = reguh-vblnr.
          ELSE.                        "HR-Abrechnung vorhanden?
            SELECT * FROM payr
              WHERE pernr EQ reguh-pernr
              AND   seqnr EQ reguh-seqnr
              AND   btznr EQ reguh-btznr
              AND   voidr EQ 0.
            ENDSELECT.
            sy-msgv1 = reguh-pernr.
            sy-msgv2 = reguh-seqnr.
            sy-msgv3 = reguh-btznr.
          ENDIF.
          IF sy-subrc EQ 0.
            regud-chect = payr-chect.
          ELSE.
            READ TABLE err_fw_scheck WITH KEY
               zbukr = reguh-zbukr
               vblnr = reguh-vblnr.
            IF sy-subrc NE 0.
              IF sy-batch EQ space.    "check does not exist
                MESSAGE a564(fs) WITH sy-msgv1 sy-msgv2 sy-msgv3.
              ELSE.
                MESSAGE s564(fs) WITH sy-msgv1 sy-msgv2 sy-msgv3.
                MESSAGE s549(fs).
                STOP.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          regud-chect = 'TEST'.
        ENDIF.
      ELSEIF flg_avis EQ 1.
        IF hlp_laufk NE 'P'.         "FI-Beleg vorhanden?
          SELECT * FROM payr
            WHERE zbukr EQ reguh-zbukr
            AND   vblnr EQ reguh-vblnr
            AND   gjahr EQ regud-gjahr
            AND   voidr EQ 0.
          ENDSELECT.
        ELSE.                        "HR-Abrechnung vorhanden?
          SELECT * FROM payr
            WHERE pernr EQ reguh-pernr
            AND   seqnr EQ reguh-seqnr
            AND   btznr EQ reguh-btznr
            AND   voidr EQ 0.
          ENDSELECT.
        ENDIF.
        IF sy-subrc EQ 0.
          regud-chect = payr-chect.
        ENDIF.
      ENDIF.

*     Berechnung Anzahl benötigter Wechsel
      IF reguh-weamx EQ 0.
        regud-wecan = 1.
      ELSE.
        regud-wecan = reguh-weamx.
        IF reguh-wehrs NE 0.
          ADD 1 TO regud-wecan.
        ENDIF.
      ENDIF.

    ENDAT.


*-- Verarbeitung der Einzelposten-Informationen ------------------------
    AT daten.

      PERFORM fpaym USING 2.           "FPAYM
      PERFORM einzelpostenfelder_fuellen.
      IF flg_papieravis EQ 1.
        reguh-ediav = space.
      ENDIF.
      IF hlp_pdfformular IS INITIAL.
*--- REGUD totals only for SAPscript advice
        IF reguh-ediav NA ' V' AND hlp_xhrfo EQ space.
          PERFORM summenfelder_fuellen.
        ENDIF.
      ENDIF.

    ENDAT.


*-- Ende der Zahlungsbelegnummer ---------------------------------------
    AT END OF reguh-vblnr.

      PERFORM fpaym USING 2.           "FPAYM

*     Zahlungsbelegnummer bei Saldo-Null-Mitteilungen und
*     Zahlungsanforderungen nicht ausgeben
      IF ( reguh-rzawe EQ space AND reguh-xvorl EQ space )
        OR t042z-xzanf NE space.
*       save value
        DATA ld_vblnr LIKE reguh-vblnr.
        ld_vblnr = reguh-vblnr.
        reguh-vblnr = space.
      ENDIF.

*     Stets Ausgabe via EDI, falls möglich
      IF flg_papieravis EQ 1.
        reguh-ediav = space.
      ENDIF.
      CLEAR regud-avedn.
      IF reguh-ediav NA ' V' AND hlp_xhrfo EQ space.
        CALL FUNCTION 'FI_EDI_REMADV_PEXR2001_OUT'
          EXPORTING
            reguh_in   = reguh
            regud_in   = regud
            xeinz_in   = regud-xeinz
            i_rcvprt   = p_rcvprt
            i_rcvprn   = p_rcvprn
          IMPORTING
            docnum_out = regud-avedn
          TABLES
            tab_regup  = tab_regup
          EXCEPTIONS
            OTHERS     = 4.
        IF sy-subrc EQ 0.
          ADD 1            TO cnt_avedi.
          ADD reguh-rbetr  TO sum_abschl_edi.
          WRITE:
            cnt_avise      TO regud-avise,
            cnt_avedi      TO regud-avedi,
            cnt_avfax      TO regud-avfax,
            cnt_avmail     TO regud-avmail,
            sum_abschluss  TO regud-summe CURRENCY t001-waers,
            sum_abschl_edi TO regud-suedi CURRENCY t001-waers,
            sum_abschl_fax TO regud-sufax CURRENCY t001-waers,
            sum_abschl_mail TO regud-sumail CURRENCY t001-waers.
          TRANSLATE:
            regud-avise USING ' *',
            regud-avedi USING ' *',
            regud-avfax USING ' *',
            regud-avmail USING ' *',
            regud-summe USING ' *',
            regud-suedi USING ' *',
            regud-sufax USING ' *',
            regud-sumail USING ' *'.
          tab_edi_avis-reguh = reguh.
          tab_edi_avis-regud = regud.
          APPEND tab_edi_avis.
          flg_kein_druck = 1.
*       Ausgabe in der Liste wegen PDF-Formularen ohne Formularabschluss
          CLEAR tab_ausgabe.
          tab_ausgabe-name    = text_097.
          tab_ausgabe-count   = 1.
          COLLECT tab_ausgabe.
        ENDIF.
      ENDIF.

*     Ausgabe auf Fax oder Drucker (nur falls notwendig)
      IF flg_kein_druck EQ 0.

        PERFORM avis_nachrichtenart.
        IF reguh-vblnr = space AND ld_vblnr <> space.
*       we need the value in exit 2050
          reguh-vblnr = ld_vblnr.
        ENDIF.
        PERFORM avis_oeffnen USING 'X'.
        IF ld_vblnr <> space.
*         clear the value again
          reguh-vblnr = space.
          ld_vblnr = space.
        ENDIF.
        PERFORM zahlungs_daten_lesen_hlp.
        PERFORM summenfelder_initialisieren.
        PERFORM avis_schreiben.

      ENDIF.

    ENDAT.


*-- Ende der Hausbank --------------------------------------------------
    AT END OF reguh-ubnkl.

      PERFORM fpaym USING 3.           "FPAYM

*     Anzahl der erzeugten Avise jedes Typs ausgeben
*     - falls SAPscript-Formular verwendet: Formularabschluß
*     - falls PDF-Formular verwendet: nur Ausgabe in INFORMATION_2
      IF hlp_formular = space AND hlp_pdfformular = space.
        hlp_formular = t042b-aforn.
      ENDIF.
      IF NOT hlp_formular IS INITIAL.
        IF hlp_laufk NE '*'.
          IF l_form NE hlp_formular.
            l_form = hlp_formular.
            REFRESH l_pages.
            CALL FUNCTION 'READ_FORM'
              EXPORTING
                form          = l_form
                language      = t001-spras
                throughclient = 'X'
              TABLES
                pages         = l_pages.
          ENDIF.
          READ TABLE l_pages WITH KEY tdpage = 'LAST'.
          IF sy-subrc NE 0.
            CLEAR l_pages-tdpage.
          ENDIF.
        ENDIF.

        IF l_pages-tdpage EQ 'LAST' AND  "Formularabschluß möglich
           ( cnt_avise NE 0              "Formularabschluß erforderlich
          OR cnt_avedi NE 0
          OR cnt_avfax NE 0
          OR cnt_avmail NE 0 ) AND hlp_laufk NE '*'.

*       Formular für den Abschluß öffnen
          SET COUNTRY space.
          CLEAR finaa.
          finaa-nacha = '1'.
          PERFORM avis_oeffnen USING space.

*       Liste aller Avis-Zwischenbelege ausgeben
          IF cnt_avedi NE 0.
            REFRESH tab_elements.
            CALL FUNCTION 'READ_FORM_ELEMENTS'
              EXPORTING
                form     = hlp_formular
                language = t001-spras
              TABLES
                elements = tab_elements
              EXCEPTIONS
                OTHERS   = 3.
            READ TABLE tab_elements WITH KEY
              window  = 'MAIN'
              element = '676'.
            IF sy-subrc EQ 0.
              CALL FUNCTION 'START_FORM'
                EXPORTING
                  form      = hlp_formular
                  language  = t001-spras
                  startpage = 'EDI'.
              CALL FUNCTION 'WRITE_FORM'
                EXPORTING
                  element = '675'
                  type    = 'TOP'
                EXCEPTIONS
                  OTHERS  = 4.
              sic_reguh = reguh.
              sic_regud = regud.
              LOOP AT tab_edi_avis.
                reguh = tab_edi_avis-reguh.
                regud = tab_edi_avis-regud.
                CALL FUNCTION 'WRITE_FORM'
                  EXPORTING
                    element = '676'
                  EXCEPTIONS
                    OTHERS  = 4.
              ENDLOOP.
              CALL FUNCTION 'END_FORM'.
              reguh = sic_reguh.
              regud = sic_regud.
            ENDIF.
          ENDIF.
          IF par_espr NE 'X'.
*       Formular für den Abschluß starten
            CALL FUNCTION 'START_FORM'
              EXPORTING
                form      = hlp_formular
                language  = t001-spras
                startpage = 'LAST'.
          ELSE.
            CALL FUNCTION 'START_FORM'
              EXPORTING
                form      = hlp_formular
                language  = reguh-zspra
                startpage = 'LAST'.

          ENDIF.
*       Ausgabe des Formularabschlusses
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              window = 'SUMMARY'
            EXCEPTIONS
              window = 1.
          IF sy-subrc EQ 1.
            err_element-fname = hlp_formular.
            err_element-fenst = 'SUMMARY'.
            err_element-elemt = space.
            err_element-text  = space.
            COLLECT err_element.
          ENDIF.

*       Formular beenden
          CALL FUNCTION 'END_FORM'.

        ENDIF.
      ENDIF. "NOT hlp_pdfformular IS INITIAL.

      PERFORM avis_schliessen.
      PERFORM force_final_spooljob.

      IF sy-binpt EQ space.
        COMMIT WORK.
      ENDIF.

    ENDAT.


*-- Ende des Zahlwegs --------------------------------------------------
    AT END OF reguh-rzawe.

      FREE MEMORY ID 'RFFORI06_ITCPO'.

    ENDAT.

  ENDLOOP.

ENDFORM.                               "Avis

* declaration for sending mails via attachment
DATA : gt_text_mail       TYPE soli_tab,
       gs_finaa_last      LIKE finaa,
       gs_itcpo_last      LIKE itcpo,
       gs_arc_params_last LIKE arc_params,
       gs_toa_dara_last   LIKE toa_dara,
       gd_lifnr_last      LIKE reguh-lifnr,
       gd_kunnr_last      LIKE reguh-kunnr,
       gd_bukrs_last      LIKE reguh-zbukr,
       gd_use_new_mail    LIKE boole-boole,
       gt_lines           LIKE tline OCCURS 0 WITH HEADER LINE,
       gt_lines_last      LIKE tline OCCURS 0 WITH HEADER LINE,
       fsabe_last         LIKE fsabe.    " note 1436202

*----------------------------------------------------------------------*
* FORM AVIS_NACHRICHTENART                                             *
*----------------------------------------------------------------------*
* Nachrichtenart ermitteln (Druck oder Fax)                            *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM avis_nachrichtenart.

  DATA up_fimsg LIKE fimsg OCCURS 0 WITH HEADER LINE.
  STATICS up_profile LIKE soprd.

* Nachrichtenart ermitteln lassen
  CLEAR finaa.
  finaa-nacha = '1'.
  CALL FUNCTION 'OPEN_FI_PERFORM_00002040_P'
    EXPORTING
      i_reguh = reguh
    TABLES
      t_fimsg = up_fimsg
    CHANGING
      c_finaa = finaa.

  IF finaa-nacha = 'I' AND
    ( finaa-mail_sensitivity <> space OR finaa-mail_importance <> space OR
     finaa-mail_send_prio <> space OR finaa-mail_send_addr <> space OR
     finaa-mail_status_attr <> space OR finaa-mail_body_text <> space OR
     finaa-mail_outbox_link <> space ).
    gd_use_new_mail = 'X'.
  ELSE.
    gd_use_new_mail = space.
  ENDIF.

  IF finaa-mail_send_addr <> space.
    fsabe-intad  = finaa-mail_send_addr.
  ENDIF.

  LOOP AT up_fimsg INTO fimsg.
    PERFORM message USING fimsg-msgno.
  ENDLOOP.

* Nachrichtenart Fax (2) oder Mail (I) prüfen, sonst nur Druck (1)
  CASE finaa-nacha.
    WHEN '2'.
      CALL FUNCTION 'TELECOMMUNICATION_NUMBER_CHECK'
        EXPORTING
          service = 'TELEFAX'
          number  = finaa-tdtelenum
          country = finaa-tdteleland
        EXCEPTIONS
          OTHERS  = 4.
      IF sy-subrc NE 0.
        finaa-nacha = '1'.
        finaa-fornr = t042b-aforn.
      ENDIF.
    WHEN 'I'.
      IF up_profile IS INITIAL.
        CALL FUNCTION 'SO_PROFILE_READ'
          IMPORTING
            profile = up_profile
          EXCEPTIONS
            OTHERS  = 4.
        IF sy-subrc NE 0.
          up_profile-smtp_exist = '-'.
        ENDIF.
      ENDIF.
      IF up_profile-smtp_exist NE 'X' OR finaa-intad IS INITIAL.
        finaa-nacha = '1'.
        finaa-fornr = t042b-aforn.
      ENDIF.
    WHEN OTHERS.
      finaa-nacha = '1'.
  ENDCASE.

ENDFORM.                    "avis_nachrichtenart



*----------------------------------------------------------------------*
* FORM AVIS_OEFFNEN                                                    *
*----------------------------------------------------------------------*
* Avis öffnen und bei Druck Probedruck erledigen                       *
*----------------------------------------------------------------------*
* GENUINE ist gesetzt bei echten Avisen, leer bei Formularabschluß     *
*----------------------------------------------------------------------*
FORM avis_oeffnen USING genuine.

  DATA: up_device         LIKE itcpp-tddevice,
        up_sender         LIKE swotobjid,
        up_recipient      LIKE swotobjid,
        ld_user           LIKE fsabe-usrnam,
        up_save_get_otf   LIKE itcpo-tdgetotf.

* Mail-Sender und -Empfänger ermitteln
  IF finaa-nacha EQ 'I'.
    IF finaa-intuser NE space.
      ld_user = finaa-intuser.
    ELSEIF fsabe-usrnam NE space.
      ld_user = fsabe-usrnam.
    ELSE.
      ld_user = sy-uname.
    ENDIF.
    DATA ld_text_existing.
    IF genuine <> space.
      PERFORM check_mail_text USING hlp_sprache ld_text_existing.
    ENDIF.

    IF ld_text_existing <> space OR gd_use_new_mail <> space.
      up_device = 'PRINTER'.
      gd_use_new_mail = 'X'.
    ELSE.
      up_device = 'MAIL'.
      PERFORM mail_vorbereiten USING    ld_user   finaa-intad
                               CHANGING up_sender up_recipient.
      IF up_sender IS INITIAL.
        IF NOT reguh-pernr IS INITIAL.
          fimsg-msgv1 = reguh-pernr.
          fimsg-msgv2 = reguh-seqnr.
        ELSE.
          fimsg-msgv1 = reguh-zbukr.
          fimsg-msgv2 = reguh-vblnr.
        ENDIF.
        PERFORM message USING '387'.
        finaa-nacha = '1'.
        finaa-fornr = t042b-aforn.
      ENDIF.
    ENDIF.
  ENDIF.


* Formular ermitteln
  IF NOT finaa-fornr IS INITIAL.
    hlp_formular = finaa-fornr.
  ELSE.
    hlp_formular = t042b-aforn.
  ENDIF.

* PDF form / alternative form
  CLEAR hlp_pdfformular.
  IF  finaa-fornr IS INITIAL
  AND hlp_aforn IS INITIAL.
* kein abweichendes SAPscript-Formular aus BTE oder Parameter -> PDF
    IF NOT hlp_apdfaf IS INITIAL.
* PDF Formular vom Selektionsbild
      hlp_pdfformular = hlp_apdfaf.
    ELSEIF NOT t042b-pdfaf IS INITIAL.
* PDF Formular vom zahlenden Buchungskreis
      hlp_pdfformular = t042b-pdfaf.
    ENDIF.
    IF NOT hlp_pdfformular IS INITIAL.
* PDF Formular erzeugen, kein SAPscript-Formular
      CLEAR hlp_formular.
    ENDIF.
  ENDIF.

* Vorschlag für die Druckvorgaben holen und anpassen, Device setzen
  IMPORT itcpo FROM MEMORY ID 'RFFORI06_ITCPO'.
  itcpo-tdgetotf  = space.
  CASE finaa-nacha.
    WHEN '1'.
      up_device = 'PRINTER'.
    WHEN '2'.
      itcpo-tdschedule = finaa-tdschedule.
      itcpo-tdteleland = finaa-tdteleland.
      itcpo-tdtelenum  = finaa-tdtelenum.
      itcpo-tdfaxuser  = finaa-tdfaxuser.
      itcpo-tdsuffix1  = 'FAX'.
      up_device = 'TELEFAX'.
    WHEN 'I'.
      itcpo-tdtitle    = text_096.
      WRITE reguh-zaldt TO txt_zeile DD/MM/YYYY.
      REPLACE '&' WITH txt_zeile INTO itcpo-tdtitle.
      IF ld_text_existing <> space OR itcpo-tdarmod CA '23'
        OR gd_use_new_mail <> space.
        itcpo-tdgetotf  = 'X'.
      ENDIF.
  ENDCASE.
  CLEAR:
    toa_dara,
    arc_params.

* Druckvorgaben modifizieren lassen
  IF genuine EQ 'X'.
    CALL FUNCTION 'OPEN_FI_PERFORM_00002050_P'
      EXPORTING
        i_reguh          = reguh
        i_gjahr          = regud-gjahr
        i_nacha          = finaa-nacha
        i_aforn          = hlp_formular
      CHANGING
        c_itcpo          = itcpo
        c_archive_index  = toa_dara
        c_archive_params = arc_params.
    IF itcpo-tdarmod GT 1 AND par_anzp NE 0.              "#EC PORTABLE
      par_anzp = 0.
      PERFORM message USING '384'.
    ENDIF.
  ENDIF.

* Name des Elements mit dem Anschreiben zusammensetzen
  IF reguh-rzawe EQ 'F' OR reguh-rzawe EQ 'N' OR reguh-rzawe EQ 'H'.
    hlp_element   = '610'.
    hlp_element+4 = reguh-avisg.
    hlp_eletext   = text_611.
  ELSEIF reguh-rzawe EQ 'U'.
    hlp_element   = '610-'.
    hlp_element+4 = 'T'.
    hlp_eletext   = text_610.
    REPLACE '&ZAHLWEG' WITH reguh-rzawe INTO hlp_eletext.
  ELSEIF reguh-rzawe EQ 'D'.
    hlp_element   = '610-'.
    hlp_element+4 = 'C'.
    hlp_eletext   = text_610.
    REPLACE '&ZAHLWEG' WITH reguh-rzawe INTO hlp_eletext.
  ELSE.
    IF reguh-rzawe NE space.
      hlp_element   = '610-'.
      hlp_element+4 = reguh-rzawe.
      hlp_eletext   = text_610.
      REPLACE '&ZAHLWEG' WITH reguh-rzawe INTO hlp_eletext.
    ELSE.
      hlp_element   = '611-'.
      hlp_element+4 = reguh-avisg.
      hlp_eletext   = text_611.
    ENDIF.
  ENDIF.
* Prüfen, ob ein Close/Open_form notwendig ist (Performance)
  IF flg_druckmodus EQ 1 AND hlp_tdprintmod IS INITIAL.
    CHECK finaa-nacha NE '1'
    OR    itcpo-tdarmod NE '1'
    OR NOT hlp_pdfformular IS INITIAL.
  ENDIF.

* Dialog nur, wenn bei Druck der Drucker unbekannt
  IF par_pria EQ space AND finaa-nacha EQ '1'.
    flg_dialog = 'X'.
  ELSE.
    flg_dialog = space.
  ENDIF.

* Neue Spool-Id bei erstem Avis zum Druck oder bei Fax
  IF flg_probedruck EQ 0 OR finaa-nacha NE '1'.
    itcpo-tdnewid = 'X'.
  ELSE.
    itcpo-tdnewid = space.
  ENDIF.

* Formular schließen, falls noch offen vom letzten Avis
  PERFORM avis_schliessen.

  DATA ld_lang LIKE t001-spras.                   " note 1055895
  IF ( finaa-nacha = '2' OR finaa-nacha = 'I' )   " note 1055895
       AND hlp_sprache <> space.                  " note 1055895
    ld_lang = hlp_sprache.                        " note 1055895
  ELSE.                                           " note 1055895
    ld_lang = t001-spras.                         " note 1055895
  ENDIF.                                          " note 1055895

  IF NOT hlp_pdfformular IS INITIAL.
*   save values for sending mail
    gs_finaa_last  = finaa.
    gs_itcpo_last       = itcpo.
    gs_arc_params_last  = arc_params.
    gs_toa_dara_last    = toa_dara.
    gd_lifnr_last       = reguh-lifnr.
    gd_kunnr_last       = reguh-kunnr.
    gd_bukrs_last       = reguh-zbukr.
    gt_lines_last[]     = gt_lines[].
    fsabe_last          = fsabe.    " note 1436202
*--- PDF Avis vorbereiten
    IF  itcpo-tddest IS INITIAL
    AND sy-batch     IS INITIAL
    AND finaa-nacha  EQ '1'.
*--- Drucker fehlt, vorgeschlagene Druckparameter anzeigen
      CALL FUNCTION 'GET_TEXT_PRINT_PARAMETERS'
        EXPORTING
          OPTIONS          = itcpo
          no_print_buttons = 'X'
        IMPORTING
          newoptions       = itcpo
        EXCEPTIONS
          canceled         = 1
          archive_error    = 0
          device           = 0
          OTHERS           = 4.
      IF sy-subrc EQ 0.
*--- letzte Druckparameter merken
        par_pria = itcpo-tddest.
        CLEAR itcpo-tdimmed.
        EXPORT itcpo TO MEMORY ID 'RFFORI06_ITCPO'.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
*--- Mapping fuer PDF
    CALL FUNCTION 'FI_PDF_PRINT_PREPARE'
      EXPORTING
        is_reguh          = reguh
        is_finaa          = finaa
        is_itcpo          = itcpo
        i_langu           = hlp_sprache
      IMPORTING
        es_fpayh          = gs_fpayh
        es_fpayhx         = gs_fpayhx
        es_fpparams       = gs_fpparams
      TABLES
        it_regup          = tab_regup
        et_fpayp          = gt_fpayp
        et_paym_note_text = gt_paym_note_text.
    IF finaa-nacha EQ '2'
    AND NOT finaa-formc IS INITIAL.
*--- Formular fuer SAPscript Fax-Deckblatt oeffnen
      up_save_get_otf = itcpo-tdgetotf.
      itcpo-tdgetotf  = 'X'.
      CALL FUNCTION 'OPEN_FORM'
        EXPORTING
          form     = finaa-formc
          device   = up_device
          language = ld_lang
          OPTIONS  = itcpo
          dialog   = flg_dialog
        IMPORTING
          RESULT   = itcpp
        EXCEPTIONS
          form     = 1
          OTHERS   = 2.
      IF sy-subrc NE 0.
*--- Formular nicht aktiv: faxen ohne Deckblatt
        CLEAR finaa-formc.
      ENDIF.
      itcpo-tdgetotf = up_save_get_otf.
    ENDIF.
  ELSE.
* SAPscript Avis-Formular öffnen
    CALL FUNCTION 'OPEN_FORM'
      EXPORTING
        archive_index  = toa_dara
        archive_params = arc_params
        form           = hlp_formular
        device         = up_device
        language       = ld_lang
        OPTIONS        = itcpo
        dialog         = flg_dialog
        mail_sender    = up_sender
        mail_recipient = up_recipient
      IMPORTING
        RESULT         = itcpp
      EXCEPTIONS
        form           = 1
        mail_options   = 2.
    IF sy-subrc EQ 2.                    "E-Mailen nicht möglich,
      fimsg-msgid = sy-msgid.            "also drucken
      fimsg-msgv1 = sy-msgv1.
      fimsg-msgv2 = sy-msgv2.
      fimsg-msgv3 = sy-msgv3.
      fimsg-msgv4 = sy-msgv4.
      PERFORM message USING sy-msgno.
      IF NOT reguh-pernr IS INITIAL.
        fimsg-msgv1 = reguh-pernr.
        fimsg-msgv2 = reguh-seqnr.
      ELSE.
        fimsg-msgv1 = reguh-zbukr.
        fimsg-msgv2 = reguh-vblnr.
      ENDIF.
      PERFORM message USING '387'.
      CALL FUNCTION 'CLOSE_FORM'
        EXCEPTIONS
          OTHERS = 0.
      finaa-nacha   = '1'.
      up_device     = 'PRINTER'.
      hlp_formular  = t042b-aforn.
      CALL FUNCTION 'OPEN_FORM'
        EXPORTING
          archive_index  = toa_dara
          archive_params = arc_params
          form           = hlp_formular
          device         = up_device
          language       = ld_lang
          OPTIONS        = itcpo
          dialog         = flg_dialog
        IMPORTING
          RESULT         = itcpp
        EXCEPTIONS
          form           = 1.
    ENDIF.
    IF sy-subrc EQ 1.                    "Abbruch:
      IF sy-batch EQ space.              "Formular ist nicht aktiv!
        MESSAGE a069 WITH hlp_formular.
      ELSE.
        MESSAGE s069 WITH hlp_formular.
        MESSAGE s094.
        STOP.
      ENDIF.
    ENDIF.

    CLEAR: hlp_pages, hlp_pages[], hlp_tdprintmod.
    CALL FUNCTION 'LOAD_FORM'
      EXPORTING
        form     = hlp_formular
        language = ld_lang
        printer  = itcpp-tdprinter
      TABLES
        pages    = hlp_pages.

    LOOP AT hlp_pages WHERE tdprintmod CO 'DT'.
      hlp_tdprintmod = hlp_pages-tdprintmod.
    ENDLOOP.

* Druckmodus setzen
    IF finaa-nacha EQ '1' AND itcpo-tdarmod EQ '1'.
      flg_druckmodus = 1.
    ELSE.
      flg_druckmodus = 2.
    ENDIF.
    hlp_nacha_last = finaa-nacha.
    gs_finaa_last  = finaa.
    gs_itcpo_last       = itcpo.
    gs_arc_params_last  = arc_params.
    gs_toa_dara_last    = toa_dara.
    gd_lifnr_last       = reguh-lifnr.
    gd_kunnr_last       = reguh-kunnr.
    gd_bukrs_last       = reguh-zbukr.
    gt_lines_last[]     = gt_lines[].
    fsabe_last          = fsabe.   " note 1436202
"$$
"$$

* letzte Druckparameter merken
    IF finaa-nacha EQ '1'.
      par_pria = itcpp-tddest.
      PERFORM fill_itcpo_from_itcpp.
      EXPORT itcpo TO MEMORY ID 'RFFORI06_ITCPO'.
    ENDIF.

* Probedruck
    IF flg_probedruck EQ 0               "Probedruck noch nicht erledigt
      AND finaa-nacha EQ '1'.
      PERFORM daten_sichern.
      DO par_anzp TIMES.
*     Probedruck-Formular starten
        CALL FUNCTION 'START_FORM'
          EXPORTING
            form     = hlp_formular
            language = t001-spras.
*     Fenster mit Probedruck schreiben
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            window   = 'INFO'
            element  = '605'
            function = 'APPEND'
          EXCEPTIONS
            window   = 1
            element  = 2.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = hlp_element
          EXCEPTIONS
            window  = 1
            element = 2.
        IF sy-subrc NE 0.
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              element = '610'
            EXCEPTIONS
              window  = 1
              element = 2.
        ENDIF.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = '614'
          EXCEPTIONS
            window  = 1
            element = 2.
* alquve
        SELECT SINGLE * FROM t001 INTO *t001
            WHERE bukrs EQ regup-bukrs.
        regud-abstx = *t001-butxt.
        regud-absor = *t001-ort01.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = '615'
          EXCEPTIONS
            window  = 1
            element = 2.
        DO 5 TIMES.
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              element  = '625'
              function = 'APPEND'
            EXCEPTIONS
              window   = 1
              element  = 2.
        ENDDO.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element  = '630'
            function = 'APPEND'
          EXCEPTIONS
            window   = 1
            element  = 2.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            window  = 'TOTAL'
            element = '630'
          EXCEPTIONS
            window  = 1
            element = 2.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            window   = 'INFO'
            element  = '605'
            function = 'DELETE'
          EXCEPTIONS
            window   = 1
            element  = 2.
*     Probedruck-Formular beenden
        CALL FUNCTION 'END_FORM'.
      ENDDO.
      PERFORM daten_zurueck.
      flg_probedruck = 1.                "Probedruck erledigt
    ENDIF.
  ENDIF.

ENDFORM.                    "avis_oeffnen



*----------------------------------------------------------------------*
* FORM AVIS_SCHREIBEN                                                  *
*----------------------------------------------------------------------*
* Avis in Druckform ausgeben                                           *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM avis_schreiben.

  DATA:
    l_xgetpdf           TYPE fpgetpdf,
    ls_sfpjoboutput     TYPE sfpjoboutput,
    l_spoolid           TYPE rspoid,
    lt_advice           TYPE solix_tab,
    l_pdf_len           TYPE i,
    lt_faxcover         TYPE soli_tab WITH HEADER LINE,
    lt_otf              TYPE ssftotf WITH HEADER LINE.

* Faxdeckblatt
  IF finaa-nacha EQ '2' AND finaa-formc NE space.
    PERFORM adresse_lesen USING t001-adrnr.                 "SADR40A
    itcfx-rtitle     = reguh-zanre.
    itcfx-rname1     = reguh-znme1.
    itcfx-rname2     = reguh-znme2.
    itcfx-rname3     = reguh-znme3.
    itcfx-rname4     = reguh-znme4.
    itcfx-rpocode    = reguh-zpstl.
    itcfx-rcity1     = reguh-zort1.
    itcfx-rcity2     = reguh-zort2.
    itcfx-rpocode2   = reguh-zpst2.
    itcfx-rpobox     = reguh-zpfac.
    itcfx-rpoplace   = reguh-zpfor.
    itcfx-rstreet    = reguh-zstra.
    itcfx-rcountry   = reguh-zland.
    itcfx-rregio     = reguh-zregi.
    itcfx-rlangu     = hlp_sprache.
    itcfx-rhomecntry = t001-land1.
    itcfx-rlines     = '9'.
    itcfx-rctitle    = space.
    itcfx-rcfname    = space.
    itcfx-rclname    = space.
    itcfx-rcname1    = finaa-namep.
    itcfx-rcname2    = space.
    itcfx-rcdeptm    = finaa-abtei.
    itcfx-rcfaxnr    = finaa-tdtelenum.
    itcfx-stitle     = sadr-anred.
    itcfx-sname1     = sadr-name1.
    itcfx-sname2     = sadr-name2.
    itcfx-sname3     = sadr-name3.
    itcfx-sname4     = sadr-name4.
    itcfx-spocode    = sadr-pstlz.
    itcfx-scity1     = sadr-ort01.
    itcfx-scity2     = sadr-ort02.
    itcfx-spocode2   = sadr-pstl2.
    itcfx-spobox     = sadr-pfach.
    itcfx-spoplace   = sadr-pfort.
    itcfx-sstreet    = sadr-stras.
    itcfx-scountry   = sadr-land1.
    itcfx-sregio     = sadr-regio.
    itcfx-shomecntry = reguh-zland.
    itcfx-slines     = '9'.
    itcfx-sctitle    = fsabe-salut.
    itcfx-scfname    = fsabe-fname.
    itcfx-sclname    = fsabe-lname.
    itcfx-scname1    = fsabe-namp1.
    itcfx-scname2    = fsabe-namp2.
    itcfx-scdeptm    = fsabe-abtei.
    itcfx-sccostc    = fsabe-kostl.
    itcfx-scroomn    = fsabe-roomn.
    itcfx-scbuild    = fsabe-build.
    CONCATENATE fsabe-telf1 fsabe-tel_exten1
                INTO itcfx-scphonenr1.
    CONCATENATE fsabe-telf2 fsabe-tel_exten2
                INTO itcfx-scphonenr2.
    CONCATENATE fsabe-telfx fsabe-fax_extens
                INTO itcfx-scfaxnr.
    itcfx-header     = t042t-txtko.
    itcfx-footer     = t042t-txtfu.
    itcfx-signature  = t042t-txtun.
    itcfx-tdid       = t042t-txtid.
    itcfx-tdlangu    = hlp_sprache.
    itcfx-subject    = space.
    CALL FUNCTION 'START_FORM'
      EXPORTING
        archive_index = toa_dara
        form          = finaa-formc
        language      = hlp_sprache
        startpage     = 'FIRST'.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        window = 'RECEIVER'.
    CALL FUNCTION 'END_FORM'.
    IF NOT hlp_pdfformular IS INITIAL.
*--- OTF zurueckgeben lassen und zusammen mit PDF faxen
      CALL FUNCTION 'CLOSE_FORM'
        TABLES
          otfdata = lt_otf
        EXCEPTIONS
          OTHERS  = 1.
      IF sy-subrc EQ 0.
        LOOP AT lt_otf.
          lt_faxcover = lt_otf.
          APPEND lt_faxcover.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

  IF NOT hlp_pdfformular IS INITIAL.
    gs_fpayhx-pdfaf = hlp_pdfformular.
    IF finaa-nacha NE '1'.
*--- get PDF for fax or e-mail
      l_xgetpdf = 'X'.
    ENDIF.
*--- PDF Formular erzeugen
    DATA l_xstring_pdf TYPE xstring.
    CALL FUNCTION 'FI_PDF_ADVICE_OUTPUT'
      EXPORTING
        is_fpayh          = gs_fpayh
        is_fpayhx         = gs_fpayhx
        is_fpparams       = gs_fpparams
        is_archive_index  = toa_dara
        i_xget_pdf        = l_xgetpdf
        i_langu           = hlp_sprache
      IMPORTING
        es_sfpjoboutput   = ls_sfpjoboutput
        e_pdf_len         = l_pdf_len
        e_xstring         = l_xstring_pdf
      TABLES
        it_fpayp          = gt_fpayp
        it_paym_note_text = gt_paym_note_text
        et_solix          = lt_advice
      EXCEPTIONS
        pdf_form_invalid  = 1
        pdf_print_error   = 2
        OTHERS            = 9.
    IF sy-subrc <> 0.
      MOVE-CORRESPONDING syst TO fimsg.
      PERFORM message USING sy-msgno.
      CLEAR fimsg.
      fimsg-msgv1 = hlp_pdfformular.
      fimsg-msgv2 = reguh-vblnr.
      PERFORM message USING '398'.
    ELSE.
      LOOP AT ls_sfpjoboutput-spoolids INTO l_spoolid.
        CLEAR tab_ausgabe.
        tab_ausgabe-name    = text_098.
        tab_ausgabe-dataset = itcpo-tddataset.
        tab_ausgabe-spoolnr = l_spoolid.
        tab_ausgabe-immed   = par_sofa.  "ggf. Sofortdruck veranlassen
        tab_ausgabe-count   = 1.
        COLLECT tab_ausgabe.
      ENDLOOP.
      IF finaa-nacha EQ '1'.
*--- Papieravis
        flg_probedruck = 1.
      ENDIF.
*--- Avis per E-Mail oder Fax verschicken
      IF finaa-nacha NE '1' AND gs_fpparams-arcmode <> '1'.
*     we have to archive
        CALL FUNCTION 'ARCHIV_CREATE_OUTGOINGDOCUMENT'
          EXPORTING
            arc_p                    = arc_params
            arc_i                    = toa_dara
            document                 = l_xstring_pdf
          EXCEPTIONS
            error_archiv             = 1
            error_communicationtable = 2
            error_connectiontable    = 3
            error_kernel             = 4
            error_parameter          = 5
            OTHERS                   = 6.
*        Fehlermeldung
        IF sy-subrc <> 0.
          fimsg-msgno = '751'.
          fimsg-msgv1 = sy-subrc.
          IF reguh-lifnr <> space.
            fimsg-msgv2 = reguh-lifnr.
          ELSE.
            fimsg-msgv2 = reguh-kunnr.
          ENDIF.
          fimsg-msgv3 = gd_bukrs_last.
          PERFORM message USING '751'.
        ENDIF.
      ENDIF.
      IF finaa-nacha = 'I'.
        IF gd_use_new_mail IS NOT INITIAL.
          DATA lt_otfdata LIKE itcoo OCCURS 0 WITH HEADER LINE.
          DATA ld_error. ld_error = space.
          REFRESH lt_otfdata[].
          PERFORM send_mail_with_attachm TABLES lt_otfdata lt_advice
                                          USING 'X' CHANGING ld_error.
          IF ld_error = space.
*--- success, for status list
            CLEAR tab_ausgabe.
            tab_ausgabe-name  = text_095.
            tab_ausgabe-count = 1.
            COLLECT tab_ausgabe.
          ENDIF.
        ELSE.
          PERFORM mail_pdf_advice USING lt_advice[] l_pdf_len.
        ENDIF.
      ENDIF.
      PERFORM fax_pdf_advice  USING lt_advice[] lt_faxcover[] l_pdf_len.
    ENDIF.
  ELSE.
* SAPscript Formular starten
    CALL FUNCTION 'START_FORM'
      EXPORTING
        archive_index = toa_dara
        form          = hlp_formular
        language      = hlp_sprache.

    IF hlp_xhrfo EQ space.

*   Fenster Info, Element Unsere Nummer (falls diese gefüllt ist)
      IF reguh-eikto NE space.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            window   = 'INFO'
            element  = '605'
            function = 'APPEND'
          EXCEPTIONS
            window   = 1
            element  = 2.
        IF sy-subrc EQ 2.
          err_element-fname = hlp_formular.
          err_element-fenst = 'INFO'.
          err_element-elemt = '605'.
          err_element-text  = text_605.
          COLLECT err_element.
        ENDIF.
      ENDIF.

*   Fenster Carry Forward, Element Übertrag (außer letzte Seite)
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          window  = 'CARRYFWD'
          element = '635'
        EXCEPTIONS
          window  = 1
          element = 2.
      IF sy-subrc EQ 2.
        err_element-fname = hlp_formular.
        err_element-fenst = 'CARRYFWD'.
        err_element-elemt = '635'.
        err_element-text  = text_635.
        COLLECT err_element.
      ENDIF.

*   Hauptfenster, Element Anschreiben-x (nur auf der ersten Seite)
      IF hlp_element   = '610-C'.
        SELECT SINGLE * FROM t001 INTO *t001
                  WHERE bukrs EQ regup-bukrs.
        regud-abstx = *t001-butxt.
        regud-absor = *t001-ort01.
      ENDIF.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = hlp_element
        EXCEPTIONS
          window  = 1
          element = 2.
      IF sy-subrc EQ 2.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = '610'
          EXCEPTIONS
            window  = 1
            element = 2.
        err_element-fname = hlp_formular.
        err_element-fenst = 'MAIN'.
        err_element-elemt = hlp_element.
        err_element-text  = hlp_eletext.
        COLLECT err_element.
      ENDIF.

*   Hauptfenster, Element Abweichender Zahlungsemfänger
      IF regud-xabwz EQ 'X'.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = '612'
          EXCEPTIONS
            window  = 1
            element = 2.
        IF sy-subrc EQ 2.
          err_element-fname = hlp_formular.
          err_element-fenst = 'MAIN'.
          err_element-elemt = '612'.
          err_element-text  = text_612.
          COLLECT err_element.
        ENDIF.
      ENDIF.

*   Hauptfenster, Element Zahlung erfolgt im Auftrag von
      IF reguh-absbu NE reguh-zbukr.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = '613'
          EXCEPTIONS
            window  = 1
            element = 2.
        IF sy-subrc EQ 2.
          err_element-fname = hlp_formular.
          err_element-fenst = 'MAIN'.
          err_element-elemt = '613'.
          err_element-text  = text_613.
          COLLECT err_element.
        ENDIF.
      ENDIF.

*   Hauptfenster, Element Unterschrift
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = '614'
        EXCEPTIONS
          window  = 1
          element = 2.

*   Hauptfenster, Element Überschrift (nur auf der ersten Seite)
      SELECT SINGLE * FROM t001 INTO *t001
                  WHERE bukrs EQ regup-bukrs.
      regud-abstx = *t001-butxt.
      regud-absor = *t001-ort01.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = '615'
        EXCEPTIONS
          window  = 1
          element = 2.
      IF sy-subrc EQ 2.
        err_element-fname = hlp_formular.
        err_element-fenst = 'MAIN'.
        err_element-elemt = '615'.
        err_element-text  = text_615.
        COLLECT err_element.
      ENDIF.

*   Hauptfenster, Element Überschrift (ab der zweiten Seite oben)
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = '615'
          type    = 'TOP'
        EXCEPTIONS
          window  = 1
          element = 2.             "Fehler bereits oben gemerkt

*   Hauptfenster, Element Übertrag (ab der zweiten Seite oben)
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element  = '620'
          type     = 'TOP'
          function = 'APPEND'
        EXCEPTIONS
          window   = 1
          element  = 2.
      IF sy-subrc EQ 2.
        err_element-fname = hlp_formular.
        err_element-fenst = 'MAIN'.
        err_element-elemt = '620'.
        err_element-text  = text_620.
        COLLECT err_element.
      ENDIF.

    ELSE.

*   HR-Formular ausgeben
*   write HR form
      LOOP AT pform.
        CHECK sy-tabix GT t042e-anzpo.
        regud-txthr = pform-linda.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element  = '625-HR'
            function = 'APPEND'
          EXCEPTIONS
            window   = 1
            element  = 2.
        IF sy-subrc EQ 2.
          err_element-fname = hlp_formular.
          err_element-fenst = 'MAIN'.
          err_element-elemt = '625-HR'.
          err_element-text  = text_625.
          COLLECT err_element.
        ENDIF.
      ENDLOOP.

    ENDIF.

* Ausgabe der Einzelposten
    flg_diff_bukrs = 0.
    LOOP AT tab_regup.

      AT NEW bukrs.
        regup-bukrs = tab_regup-bukrs.
        IF  ( regup-bukrs NE reguh-zbukr OR flg_diff_bukrs EQ 1 )
        AND ( reguh-absbu EQ space OR reguh-absbu EQ reguh-zbukr ).
          flg_diff_bukrs = 1.
          SELECT SINGLE * FROM t001 INTO *t001
            WHERE bukrs EQ regup-bukrs.
          regud-abstx = *t001-butxt.
          regud-absor = *t001-ort01.
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              element = '613'
            EXCEPTIONS
              window  = 1
              element = 2.
          IF sy-subrc EQ 2.
            err_element-fname = hlp_formular.
            err_element-fenst = 'MAIN'.
            err_element-elemt = '613'.
            err_element-text  = text_613.
            COLLECT err_element.
          ENDIF.
        ENDIF.
      ENDAT.

      regup = tab_regup.
      PERFORM einzelpostenfelder_fuellen.

      IF hlp_xhrfo EQ space.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element  = '625'
            function = 'APPEND'
          EXCEPTIONS
            window   = 1
            element  = 2.
        IF sy-subrc EQ 2.
          err_element-fname = hlp_formular.
          err_element-fenst = 'MAIN'.
          err_element-elemt = '625'.
          err_element-text  = text_625.
          COLLECT err_element.
        ENDIF.
      ENDIF.

      PERFORM summenfelder_fuellen.

      IF hlp_xhrfo EQ space.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element  = '625-TX'
            function = 'APPEND'
          EXCEPTIONS
            window   = 1
            element  = 2.
      ENDIF.

      AT END OF bukrs.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element  = '629'
            function = 'APPEND'
          EXCEPTIONS
            window   = 1
            element  = 2.
      ENDAT.
    ENDLOOP.

    PERFORM ziffern_in_worten.

* Summenfelder hochzählen und aufbereiten
    CASE finaa-nacha.
      WHEN '1'.
        ADD 1           TO cnt_avise.
        ADD reguh-rbetr TO sum_abschluss.
      WHEN '2'.
        ADD 1           TO cnt_avfax.
        ADD reguh-rbetr TO sum_abschl_fax.
      WHEN 'I'.
        ADD 1           TO cnt_avmail.
        ADD reguh-rbetr TO sum_abschl_mail.
    ENDCASE.

    WRITE:
      cnt_avise       TO regud-avise,
      cnt_avedi       TO regud-avedi,
      cnt_avfax       TO regud-avfax,
      cnt_avmail      TO regud-avmail,
      sum_abschluss   TO regud-summe  CURRENCY t001-waers,
      sum_abschl_edi  TO regud-suedi  CURRENCY t001-waers,
      sum_abschl_fax  TO regud-sufax  CURRENCY t001-waers,
      sum_abschl_mail TO regud-sumail CURRENCY t001-waers.
    TRANSLATE:
      regud-avise  USING ' *',
      regud-avedi  USING ' *',
      regud-avfax  USING ' *',
      regud-avmail USING ' *',
      regud-summe  USING ' *',
      regud-suedi  USING ' *',
      regud-sufax  USING ' *',
      regud-sumail USING ' *'.

    IF hlp_xhrfo EQ space.

*   Hauptfenster, Element Gesamtsumme (nur auf der letzten Seite)
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element  = '630'
          function = 'APPEND'
        EXCEPTIONS
          window   = 1
          element  = 2.
      IF sy-subrc EQ 2.
        err_element-fname = hlp_formular.
        err_element-fenst = 'MAIN'.
        err_element-elemt = '630'.
        err_element-text  = text_630.
        COLLECT err_element.
      ENDIF.

*   Hauptfenster, Element Bankgebühr (Japan)
      IF reguh-paygr+18(2) EQ '$J'.
        WHILE reguh-paygr(1) EQ 0.
          SHIFT reguh-paygr(10) LEFT.
          IF sy-index > 10. EXIT. ENDIF.
        ENDWHILE.
        SUBTRACT reguh-rspe1 FROM: regud-swnet, sum_abschluss.
        WRITE:
           regud-swnet TO regud-swnes CURRENCY reguh-waers,      "#EC CI_FLDEXT_OK[2610650]
           sum_abschluss  TO regud-summe CURRENCY t001-waers.    "#EC CI_FLDEXT_OK[2610650]
        TRANSLATE:
           regud-swnes USING ' *',
           regud-summe USING ' *'.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = '634'
          EXCEPTIONS
            window  = 1
            element = 2.
        IF sy-subrc EQ 2.
          err_element-fname = hlp_formular.
          err_element-fenst = 'MAIN'.
          err_element-elemt = '634'.
          err_element-text  = text_634.
          COLLECT err_element.
        ENDIF.
      ENDIF.

*   Fenster Carry Forward, Element Übertrag löschen
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          window   = 'CARRYFWD'
          element  = '635'
          function = 'DELETE'
        EXCEPTIONS
          window   = 1
          element  = 2.            "Fehler bereits oben gemerkt

*   Hauptfenster, Element Überschrift löschen
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element  = '615'
          type     = 'TOP'
          function = 'DELETE'
        EXCEPTIONS
          window   = 1
          element  = 2.            "Fehler bereits oben gemerkt

*   Hauptfenster, Element Übertrag löschen
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element  = '620'
          type     = 'TOP'
          function = 'DELETE'
        EXCEPTIONS
          window   = 1
          element  = 2.            "Fehler bereits oben gemerkt

*   Hauptfenster, Element Abschlußtext
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element  = '631'
          function = 'APPEND'
        EXCEPTIONS
          window   = 1
          element  = 2.            "Ausgabe ist freigestellt

*   Fenster Total, Element Gesamtsumme
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          window  = 'TOTAL'
          element = '630'
        EXCEPTIONS
          window  = 1
          element = 2.             "Ausgabe ist freigestellt

    ENDIF.

* Formular beenden
    CALL FUNCTION 'END_FORM'.
  ENDIF.

ENDFORM.                    "avis_schreiben



*----------------------------------------------------------------------*
* FORM AVIS_SCHLIESSEN                                                 *
*----------------------------------------------------------------------*
* Avis schließen und Ausgabetabelle füllen                             *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM avis_schliessen.

  DATA:   lt_otfdata LIKE itcoo           OCCURS 1 WITH HEADER LINE,
          ld_otf_lines TYPE i.

  CHECK flg_druckmodus NE 0.




* Abschluß des Formulars
  CALL FUNCTION 'CLOSE_FORM'
    IMPORTING
      RESULT     = itcpp
    TABLES
      otfdata    = lt_otfdata
    EXCEPTIONS
      send_error = 4.

  DESCRIBE TABLE lt_otfdata LINES ld_otf_lines.

  IF sy-subrc NE 0.                    "E-Mailen nicht möglich,
    fimsg-msgid = sy-msgid.            "und zum Ausdruck ist es
    fimsg-msgv1 = sy-msgv1.            "jetzt zu spät
    fimsg-msgv2 = sy-msgv2.
    fimsg-msgv3 = sy-msgv3.
    fimsg-msgv4 = sy-msgv4.
    PERFORM message USING sy-msgno.
    IF NOT reguh-pernr IS INITIAL.
      fimsg-msgv1 = reguh-pernr.
      fimsg-msgv2 = reguh-seqnr.
    ELSE.
      fimsg-msgv1 = reguh-zbukr.
      fimsg-msgv2 = reguh-vblnr.
    ENDIF.
    PERFORM message USING '388'.
    CALL FUNCTION 'OPEN_FORM'
      EXPORTING
        application = 'TH'
        device      = 'ABAP'.
    CALL FUNCTION 'CLOSE_FORM'.
  ELSE.



    CASE hlp_nacha_last.
"$$
"$$
"$$
      WHEN '1'.
        IF itcpp-tdspoolid NE 0.
          CLEAR tab_ausgabe.
          tab_ausgabe-name    = text_098.
          tab_ausgabe-dataset = itcpp-tddataset.
          tab_ausgabe-spoolnr = itcpp-tdspoolid.
          tab_ausgabe-immed   = par_sofa.
          tab_ausgabe-count   = 1.
          COLLECT tab_ausgabe.
        ENDIF.
      WHEN '2'.
        CLEAR tab_ausgabe.
        tab_ausgabe-name      = text_094.
        tab_ausgabe-dataset   = itcpp-tddataset.
        tab_ausgabe-count     = 1.
        COLLECT tab_ausgabe.
      WHEN 'I'.
        IF ld_otf_lines > 0.
*         display only spool entry in log if mail by sapscript
          CLEAR tab_ausgabe.
          tab_ausgabe-name      = text_095.
          tab_ausgabe-dataset   = itcpp-tddataset.
          tab_ausgabe-count     = 1.
          COLLECT tab_ausgabe.
          IF gs_itcpo_last-tdarmod CA '23'.
            DATA:   lt_otfdata_arc LIKE itcoo OCCURS 1 WITH HEADER LINE.
            lt_otfdata_arc[] = lt_otfdata[].
            CALL FUNCTION 'CONVERT_OTF_AND_ARCHIVE'
              EXPORTING
                arc_p  = gs_arc_params_last
                arc_i  = gs_toa_dara_last
              TABLES
                otf    = lt_otfdata_arc
              EXCEPTIONS
                OTHERS = 1.
            IF sy-subrc <> 0.
              fimsg-msgno = '751'.
              fimsg-msgv1 = sy-subrc.
              IF reguh-lifnr <> space.
                fimsg-msgv2 = gd_lifnr_last.
              ELSE.
                fimsg-msgv2 = gd_kunnr_last.
              ENDIF.
              fimsg-msgv3 = gd_bukrs_last.
              PERFORM message USING '751'.
            ENDIF.
          ENDIF.
          COMMIT WORK.
        ENDIF.
    ENDCASE.
  ENDIF.



  CLEAR flg_druckmodus.

  IF ld_otf_lines > 0.
* send email
    DATA lt_solix    TYPE solix_tab.
    DATA ld_error.
    REFRESH lt_solix[].
    PERFORM send_mail_with_attachm TABLES lt_otfdata lt_solix USING ' ' CHANGING ld_error.
  ENDIF.
ENDFORM.                    "avis_schliessen



*----------------------------------------------------------------------*
* FORM FPAYM                                                           *
*----------------------------------------------------------------------*
* Für die übergreifende Sortierung im Programm RFFOAVIS_FPAYM          *
* wurden Felder initialisiert, die nun wieder bereitgestellt           *
* werden müssen                                                        *
*----------------------------------------------------------------------*
* -> EVENT = 1 erster Aufruf pro Zahlung                               *
*            2 weitere Aufrufe pro Zahlung                             *
*            3 letzter Aufruf pro Lauf                                 *
*----------------------------------------------------------------------*
FORM fpaym USING event.                "FPAYM

  STATICS: up_zbukr LIKE reguh-zbukr,
           up_hbkid LIKE reguh-hbkid,
           up_rzawe LIKE reguh-rzawe,
           up_xavis LIKE reguh-xavis.

  CHECK reguh-zbukr IS INITIAL OR NOT up_xavis IS INITIAL.
  reguh-rzawe = sic_reguh-rzawe.
  reguh-zbukr = sic_reguh-zbukr.
  reguh-ubnks = sic_reguh-ubnks.
  reguh-ubnky = sic_reguh-ubnky.
  reguh-ubnkl = sic_reguh-ubnkl.
  up_xavis    = 'X'.

  IF event EQ 1.
*   Customizing nachlesen (wurde nicht AT NEW erledigt, da Felder leer)
    ON CHANGE OF reguh-zbukr.
      PERFORM buchungskreis_daten_lesen.
    ENDON.
    ON CHANGE OF reguh-ubnks OR reguh-ubnky.
      PERFORM hausbank_daten_lesen.
    ENDON.
    ON CHANGE OF reguh-zbukr OR reguh-rzawe.
      CLEAR: t042e, t042z.
      IF reguh-rzawe NE space.
        PERFORM zahlweg_daten_lesen.
      ELSE.
        regud-aust1 = t001-butxt.
        regud-aust2 = space.
        regud-aust3 = space.
        regud-austo = t001-ort01.
      ENDIF.
      t042z-text1 = text_001.
    ENDON.

*   Buchungskreis, Zahlweg, Hausbank merken für Event 3
    IF up_zbukr IS INITIAL.
      up_zbukr = reguh-zbukr.
      up_rzawe = reguh-rzawe.
      up_hbkid = reguh-hbkid.
    ELSEIF up_zbukr NE reguh-zbukr.
      up_zbukr = '*'.
      up_rzawe = '*'.
      up_hbkid = '*'.
    ENDIF.
    IF up_rzawe NE reguh-rzawe.
      up_rzawe = '*'.
    ENDIF.
    IF up_hbkid NE reguh-hbkid.
      up_hbkid = '*'.
    ENDIF.
  ENDIF.

  IF event EQ 3.
    reguh-zbukr = up_zbukr.
    reguh-rzawe = up_rzawe.
    reguh-hbkid = up_hbkid.
    IF up_hbkid EQ '*'.
      reguh-ubnks = '*'.
      reguh-ubnky = '*'.
      reguh-ubnkl = '*'.
    ENDIF.
  ENDIF.

ENDFORM.                    "fpaym



*----------------------------------------------------------------------*
* FORM MAIL_VORBEREITEN                                                *
*----------------------------------------------------------------------*
* Aus dem Benutzernamen wird das Sender-Objekt, aus der eMail-Adresse  *
* das Empfänger-Objekt erzeugt, welche an SAPscript zu übergeben sind  *
*----------------------------------------------------------------------*
* -> P_UNAME     Benutzer                                              *
* -> P_INTAD     Internet-Adresse                                      *
* <- P_SENDER    Sender-Objekt                                         *
* <- P_RECIPIENT Empfänger-Objekt                                      *
*----------------------------------------------------------------------*
FORM mail_vorbereiten USING    p_uname     LIKE sy-uname
                               p_intad     LIKE finaa-intad
                      CHANGING p_sender    LIKE swotobjid
                               p_recipient LIKE swotobjid.

* Das include <cntn01> enthält die Definitionen der Makrobefehle zum
* Anlegen und Bearbeiten der Container, d.h. für den Zugriff aufs BOR
  INCLUDE <cntn01>.

* Datendeklaration der BOR-Objekte
  DATA: sender         TYPE swc_object,
        recipient      TYPE swc_object.

* Deklaration einer Container-Datenstruktur zur Laufzeit
  swc_container container.

*----------------------------------------------------------------------*
* Anlegen eines Senders (BOR-Objekt-ID)                                *
*----------------------------------------------------------------------*

* Erzeugen einer Objektreferenz auf den Objekttyp 'RECIPIENT'
* Die weitere Verarbeitung findet dann für die Objektreferenz
* 'sender' statt
  swc_create_object sender             " Objektreferenz
                    'RECIPIENT'        " Name eines Objekttyps
                    space.             " objektspezifischer Schlüssel

* Initialisieren des zuvor deklarierten Containers
  swc_clear_container container.       " Container

* Unter dem Elementnamen 'AddressString' wird die Adresse des
* aufrufenden internen Benutzers in die Container-Instanz
* container eingetragen
  swc_set_element container            " Container
                  'AddressString'      " Elementname
                  p_uname.             " Wert des Elements

* Unter dem Elementnamen 'TypeId' wird der Adreßtyp 'interner
* Benutzer' in die Container-Instanz container eingetragen
  swc_set_element container            " Container
                  'TypeId'             " Elementname
                  'B'.                 " Wert des Elements

* Aufruf der Objektmethode 'FindAddress'
  swc_call_method sender               " Objektreferenz
                  'FindAddress'        " Name der Methode
                  container.           " Container

* Fehler: Das Element ist nicht im Container enthalten
  IF sy-subrc NE 0.
    CLEAR: p_sender, p_recipient.
    fimsg-msgid = sy-msgid.
    PERFORM message USING sy-msgno.
    EXIT.
  ENDIF.

* Ermittlung der BOR-Objekt-ID
  swc_object_to_persistent sender
                           p_sender.

*----------------------------------------------------------------------*
* Anlegen eines Empfängers (BOR-Objekt-ID)                             *
*----------------------------------------------------------------------*

* Erzeugen einer Objektreferenz auf den Objekttyp 'RECIPIENT'.
* Die weitere Verarbeitung findet dann für die Objektreferenz
* 'recipient' statt
  swc_create_object recipient          " Objektreferenz
                    'RECIPIENT'        " Name eines Objekttyps
                    space.             " objektspezifischer Schlüssel

* Initialisieren des zuvor deklarierten Containers
  swc_clear_container container.       " Container

* Unter dem Elementnamen 'AddressString' wird die Faxnummer bzw.
* die Internet-Adresse des Empfängers in die Container-Instanz
* container eingetragen
  swc_set_element container            " Container
                  'AddressString'      " Elementname
                  p_intad.

* Unter dem Elementnamen 'TypeId' wird der Adreßtyp 'Mail'
* in die Container-Instanz container eingetragen
  swc_set_element container            " Container
                  'TypeId'             " Elementname
                  'U'.                 " Wert des Elements

* Aufruf der Objektmethode 'CreateAddress'
  swc_call_method recipient            " Objektreferenz
                  'CreateAddress'      " Name der Methode
                  container.           " Container

* Fehler: Anlegen des Adreßteils eines Recipient-Objekts nicht möglich
  IF sy-subrc NE 0.
    CLEAR: p_sender, p_recipient.
    fimsg-msgid = sy-msgid.
    PERFORM message USING sy-msgno.
    EXIT.
  ENDIF.

*----------------------------------------------------------------------*

* Initialisieren des zuvor deklarierten Containers
  swc_clear_container container.       " Container

* Mit dem Attribut 'Deliver' wird die Empfangsbestätigung abgewählt
  swc_set_element container            " Container
                  'Deliver'            " Elementname
                  space.

* Aufruf der Objektmethode 'SetDeliver'
  swc_call_method recipient            " Objektreferenz
                  'SetDeliver'         " Name der Methode
                  container.           " Container

*----------------------------------------------------------------------*

* Initialisieren des zuvor deklarierten Containers
  swc_clear_container container.       " Container

* Mit dem Attribut 'NotDeliver' wird die Bestätigung bei Nicht-Empfang
* angefordert
  swc_set_element container            " Container
                  'NotDeliver'         " Elementname
                  'X'.

* Aufruf der Objektmethode 'SetNotDeliver'
  swc_call_method recipient            " Objektreferenz
                  'SetNotDeliver'      " Name der Methode
                  container.           " Container

*----------------------------------------------------------------------*

* Initialisieren des zuvor deklarierten Containers
  swc_clear_container container.       " Container

* Mit dem Attribut 'Read' wird die Gelesen-Bestätigung abgewählt
  swc_set_element container            " Container
                  'Read'               " Elementname
                  space.

* Aufruf der Objektmethode 'SetRead'
  swc_call_method recipient            " Objektreferenz
                  'SetRead'            " Name der Methode
                  container.           " Container

*----------------------------------------------------------------------*

* Ermittlung der BOR-Objekt-ID
  swc_object_to_persistent recipient
                           p_recipient.

* Initialisieren des zuvor deklarierten Containers
  swc_clear_container container.

ENDFORM.                    "mail_vorbereiten

*&---------------------------------------------------------------------*
*&      Form  mail_pdf_advice
*&---------------------------------------------------------------------*
*       E-mail PDF advice
*&---------------------------------------------------------------------*
*      -->IT_ADVICE     PDF form (output from Adobe server)
*      -->I_PDF_LEN     length of PDF advice in bytes
*----------------------------------------------------------------------*
FORM mail_pdf_advice USING it_advice   TYPE solix_tab
                           i_pdf_len   TYPE i.

  DATA:
    lt_receivers       TYPE TABLE OF somlreci1 WITH HEADER LINE,
    l_user             LIKE soextreci1-receiver,
    ls_send_doc        LIKE sodocchgi1,
    lt_pdf_attach      TYPE TABLE OF sopcklsti1 WITH HEADER LINE.

  CHECK NOT finaa-intad IS INITIAL.
  CHECK finaa-nacha EQ 'I'.

*--- determine E-Mail sender and recipient
  IF fsabe-usrnam EQ space.
    l_user = sy-uname.
  ELSE.
    l_user = fsabe-usrnam.         "Office-User des Sachbearb.
  ENDIF.

  lt_receivers-receiver = finaa-intad.
  lt_receivers-rec_type = 'U'.      "E-mail address
  APPEND lt_receivers.

  ls_send_doc-obj_descr =  itcpo-tdtitle.

  lt_pdf_attach-transf_bin = 'X'.
  lt_pdf_attach-doc_type   = 'PDF'.
  lt_pdf_attach-obj_langu  = reguh-zspra.
  lt_pdf_attach-body_start = 1.
  lt_pdf_attach-doc_size   = i_pdf_len.
  DESCRIBE TABLE it_advice LINES lt_pdf_attach-body_num.
  APPEND lt_pdf_attach.

  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = ls_send_doc
      sender_address             = l_user
    TABLES
      packing_list               = lt_pdf_attach
      contents_hex               = it_advice
      receivers                  = lt_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF sy-subrc EQ 0.
*--- success, for status list
    CLEAR tab_ausgabe.
    tab_ausgabe-name  = text_095.
    tab_ausgabe-count = 1.
    COLLECT tab_ausgabe.
  ELSE.
*--- E-Mail not possible and it's too late to print
    fimsg-msgid = sy-msgid.
    fimsg-msgv1 = sy-msgv1.
    fimsg-msgv2 = sy-msgv2.
    fimsg-msgv3 = sy-msgv3.
    fimsg-msgv4 = sy-msgv4.
    PERFORM message USING sy-msgno.
    IF NOT reguh-pernr IS INITIAL.
      fimsg-msgv1 = reguh-pernr.
      fimsg-msgv2 = reguh-seqnr.
    ELSE.
      fimsg-msgv1 = reguh-zbukr.
      fimsg-msgv2 = reguh-vblnr.
    ENDIF.
    PERFORM message USING '388'.
  ENDIF.

ENDFORM.                    " mail_pdf_advice

*&---------------------------------------------------------------------*
*&      Form  fax_pdf_advice
*&---------------------------------------------------------------------*
*       Send advice as facsimile
*----------------------------------------------------------------------*
*      -->IT_ADVICE     PDF form (output from Adobe server)
*      -->IT_FAXCOVER   cover page for fax
*      -->I_PDF_LEN     length of PDF advice in bytes
*----------------------------------------------------------------------*
FORM fax_pdf_advice  USING    it_advice   TYPE solix_tab
                              it_faxcover TYPE soli_tab
                              i_pdf_len   TYPE i.

  DATA:
    lt_receivers       TYPE TABLE OF somlreci1 WITH HEADER LINE,
    l_user             LIKE soextreci1-receiver,
    ls_send_doc        LIKE sodocchgi1,
    lt_pdf_attach      TYPE TABLE OF sopcklsti1 WITH HEADER LINE,
    l_faxcover_len     LIKE sy-tfill,
    ls_fax_recipient   TYPE sadrfd.

  FIELD-SYMBOLS <fs_receiver> TYPE c.

  CHECK finaa-nacha EQ '2'.
  CHECK ( NOT reguh-ztlfx IS INITIAL ) OR
        ( NOT finaa-tdtelenum IS INITIAL ).

*--- determine fax sender and recipient
  IF finaa-tdfaxuser NE space.
    l_user = finaa-tdfaxuser.
  ELSEIF fsabe-usrnam EQ space.
    l_user = sy-uname.
  ELSE.
    l_user = fsabe-usrnam.         "Office-User des Sachbearb.
  ENDIF.

*--- receiver fuellen mit Faxnummer in Form der Struktur SADRFD
  IF finaa-tdtelenum <> space.
    ls_fax_recipient-rec_fax   = finaa-tdtelenum.
  ELSE.
    ls_fax_recipient-rec_fax   = reguh-ztlfx.
  ENDIF.
  IF finaa-tdteleland <> space.
    ls_fax_recipient-rec_state = finaa-tdteleland.
  ELSE.
    ls_fax_recipient-rec_state = reguh-zland.
  ENDIF.
  ASSIGN ls_fax_recipient TO <fs_receiver> CASTING.
  lt_receivers-receiver = <fs_receiver>.
  lt_receivers-rec_type = 'F'.             "fax number
  lt_receivers-com_type = 'FAX'.
  APPEND lt_receivers.

  ls_send_doc-obj_descr = itcpo-tdtitle.
  ls_send_doc-obj_name  = itcpo-tdtitle.
  ls_send_doc-obj_langu = hlp_sprache.

*--- attach fax cover page
  DESCRIBE TABLE it_faxcover LINES l_faxcover_len.
  IF l_faxcover_len GT 0.
    lt_pdf_attach-doc_type = 'OTF'.
    lt_pdf_attach-obj_langu = hlp_sprache.
    lt_pdf_attach-body_start = 1.
    lt_pdf_attach-body_num = l_faxcover_len.
*    lt_pdf_attach-obj_descr = 'Facsimile-Deckblatt'.
    APPEND lt_pdf_attach.
  ENDIF.
*--- attach PDF advice
  CLEAR lt_pdf_attach.
  lt_pdf_attach-transf_bin = 'X'.
  lt_pdf_attach-doc_type   = 'PDF'.
  lt_pdf_attach-obj_langu  = hlp_sprache.
  lt_pdf_attach-body_start = 1.
  DESCRIBE TABLE it_advice LINES lt_pdf_attach-body_num.
*  lt_pdf_attach-obj_descr  = 'Zahlungsavis (Datei)'.
  lt_pdf_attach-doc_size = i_pdf_len.
  APPEND lt_pdf_attach.

  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = ls_send_doc
      sender_address             = l_user
    TABLES
      packing_list               = lt_pdf_attach[]
      contents_txt               = it_faxcover[]
      contents_hex               = it_advice[]
      receivers                  = lt_receivers[]
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF sy-subrc EQ 0.
*--- success, for status list
    CLEAR tab_ausgabe.
    tab_ausgabe-name  = text_094.
    tab_ausgabe-count = 1.
    COLLECT tab_ausgabe.
  ELSE.
*--- fax not possible and it's too late to print
    fimsg-msgid = sy-msgid.
    fimsg-msgv1 = sy-msgv1.
    fimsg-msgv2 = sy-msgv2.
    fimsg-msgv3 = sy-msgv3.
    fimsg-msgv4 = sy-msgv4.
    PERFORM message USING sy-msgno.
    IF NOT reguh-pernr IS INITIAL.
      fimsg-msgv1 = reguh-pernr.
      fimsg-msgv2 = reguh-seqnr.
    ELSE.
      fimsg-msgv1 = reguh-zbukr.
      fimsg-msgv2 = reguh-vblnr.
    ENDIF.
    PERFORM message USING '388'.
  ENDIF.

ENDFORM.                    " fax_pdf_advice
*&---------------------------------------------------------------------*
*&      Form  force_final_spooljob
*&---------------------------------------------------------------------*
*       Force current spools close to ensure nothing will be appended
*       to that spool anymore
*----------------------------------------------------------------------*
FORM force_final_spooljob .

  IF NOT itcpp-tdspoolid IS INITIAL.
    CALL FUNCTION 'RSPO_FINAL_SPOOLJOB'
      EXPORTING
        rqident = itcpp-tdspoolid
        set     = 'X'
        force   = 'X'
      EXCEPTIONS
        OTHERS  = 4.
    IF sy-subrc NE 0.
      MOVE-CORRESPONDING syst TO fimsg.
      PERFORM message USING fimsg-msgno.
    ENDIF.
  ENDIF.
  IF NOT hlp_pdfspoolid IS INITIAL.
    CALL FUNCTION 'RSPO_FINAL_SPOOLJOB'
      EXPORTING
        rqident = hlp_pdfspoolid
        set     = 'X'
        force   = 'X'
      EXCEPTIONS
        OTHERS  = 4.
    IF sy-subrc NE 0.
      MOVE-CORRESPONDING syst TO fimsg.
      PERFORM message USING fimsg-msgno.
    ENDIF.
  ENDIF.

ENDFORM.                    " force_final_spooljob
*---------------------------------------------------------------------*
*       FORM check_mail_text                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  LD_TEXT_EXISTING                                              *
*---------------------------------------------------------------------*
FORM check_mail_text USING id_langu CHANGING cd_text_existing.
  DATA :
       ld_packing_list LIKE soxpl OCCURS 1 WITH HEADER LINE,
       ld_header LIKE thead,
       ld_lines  LIKE tline OCCURS 0 WITH HEADER LINE,
       ld_name TYPE tdobname,
       ld_no_lines TYPE i,
       selections LIKE  stxh OCCURS 0 WITH HEADER LINE.

  CLEAR gt_lines[].
  IF finaa-namep <> space.
    ld_name = finaa-namep.
  ELSE.
    ld_name = finaa-mail_body_text.
  ENDIF.
  cd_text_existing = space.
  IF ld_name = space.
    EXIT.
  ENDIF.
* read text for mail-body out of SO10
* with selected language
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      object    = 'TEXT'
      id        = 'FIKO'
      name      = ld_name
      language  = id_langu
    IMPORTING
      header    = ld_header
    TABLES
      lines     = ld_lines
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc = 0.
    cd_text_existing = 'X'.
  ELSE.
*     with logon language
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        object    = 'TEXT'
        id        = 'FIKO'
        name      = ld_name
        language  = sy-langu
      IMPORTING
        header    = ld_header
      TABLES
        lines     = ld_lines
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc = 0.
      cd_text_existing = 'X'.
    ELSE.
      SELECT * FROM stxh INTO TABLE selections
                   WHERE tdobject   = 'TEXT'
                     AND tdname     = ld_name
                     AND tdid       = 'FIKO'.
      DESCRIBE TABLE selections LINES ld_no_lines .
*     if unique text ld_name, then with available language
      IF ld_no_lines  = '1'.
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            object    = 'TEXT'
            id        = 'FIKO'
            name      = ld_name
            language  = selections-tdspras
          IMPORTING
            header    = ld_header
          TABLES
            lines     = ld_lines
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
        IF sy-subrc = 0.
          cd_text_existing = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  gt_lines[] = ld_lines[].

ENDFORM.                    "check_mail_text


*&---------------------------------------------------------------------*
*&      Form  send_mail_with_attachm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_OTFDATA text
*----------------------------------------------------------------------*
FORM send_mail_with_attachm   TABLES  it_otfdata STRUCTURE itcoo
                                      it_advice  STRUCTURE solix
                              USING   id_call_from_pdf
                              CHANGING cd_error   LIKE boole-boole.

* Because we get the mail contents via close form,
* the mails are sent when the next reguh-entry is processed, so we do not
* use finaa, but finaa_last.

  DATA: so10_lines TYPE i,
        lt_hotfdata LIKE itcoo OCCURS 1 WITH HEADER LINE,
        htline LIKE tline OCCURS 1 WITH HEADER LINE,
        n_objcont TYPE soli_tab,
        ld_address LIKE finaa-intad,
        ld_addr TYPE adr6-smtp_addr,
        send_request TYPE REF TO cl_bcs,
        document TYPE REF TO cl_document_bcs,
        attachment TYPE REF TO cl_document_bcs,
        sender TYPE REF TO cl_sapuser_bcs,
        internet_recipient TYPE REF TO if_recipient_bcs,
        internet_sender TYPE REF TO if_sender_bcs,
        bcs_exception TYPE REF TO cx_bcs,
        sent_to_all TYPE os_boolean,
        lt_solix    TYPE solix_tab.

  DESCRIBE TABLE gt_lines_last LINES so10_lines.
  CLEAR gt_text_mail[].
  IF so10_lines > 0.
*  convert gt_lines
    PERFORM convert_itf.
*  the result is now in gt_text_mail[]
  ENDIF.

  TRY.
      send_request = cl_bcs=>create_persistent( ).
      IF gs_finaa_last-mail_status_attr = space.
        send_request->set_status_attributes(
        i_requested_status =  'N'
        i_status_mail      =  'N' ).
      ELSE.
        send_request->set_status_attributes(
        i_requested_status =  gs_finaa_last-mail_status_attr
        i_status_mail      =  gs_finaa_last-mail_status_attr ).
      ENDIF.
*     create sender
      IF gs_finaa_last-mail_send_addr <> space.
        ld_addr = gs_finaa_last-mail_send_addr.
        internet_sender = cl_cam_address_bcs=>create_internet_address(
        i_address_string = ld_addr  ).
        CALL METHOD send_request->set_sender
          EXPORTING
            i_sender = internet_sender.
      ELSE.
        DATA: ld_originator TYPE uname.
        IF gs_finaa_last-intuser <> space.
          ld_originator = gs_finaa_last-intuser.
        ELSEIF fsabe_last-usrnam IS INITIAL.    " note 1436202
          ld_originator = sy-uname.
        ELSE.
          ld_originator = fsabe_last-usrnam.  " note 1436202
        ENDIF.
        sender = cl_sapuser_bcs=>create( ld_originator ).
        CALL METHOD send_request->set_sender
          EXPORTING
            i_sender = sender.
      ENDIF.
*     create recipients
      ld_address = gs_finaa_last-intad.
      WHILE ld_address <> space.
        WHILE ld_address(1) = space.
          SHIFT ld_address BY 1 PLACES.
        ENDWHILE.
        SPLIT ld_address AT ' ' INTO ld_addr ld_address.
        internet_recipient = cl_cam_address_bcs=>create_internet_address(
        i_address_string = ld_addr  ).
        CALL METHOD send_request->add_recipient
          EXPORTING
            i_recipient = internet_recipient.
      ENDWHILE.

      document = cl_document_bcs=>create_document(
      i_type    = 'TXT'
      i_text    = gt_text_mail
      i_subject = gs_itcpo_last-tdtitle ).

      IF id_call_from_pdf IS INITIAL.
        PERFORM convert_advice TABLES it_otfdata n_objcont lt_solix.
      ELSE.
        lt_solix[] = it_advice[].
      ENDIF.

      IF gs_finaa_last-textf = 'PDF' OR gs_finaa_last-textf = space.
        attachment = cl_document_bcs=>create_document(
        i_type    = 'PDF'
        i_hex     = lt_solix
        i_subject = gs_itcpo_last-tdtitle ).
      ELSE.
        attachment = cl_document_bcs=>create_document(
        i_type    = 'RAW'
        i_text    = n_objcont
        i_subject = gs_itcpo_last-tdtitle ).
      ENDIF.

      IF gs_finaa_last-mail_sensitivity <> space.
*      'P' is confidential, * 'F' is functional
        document->set_sensitivity( gs_finaa_last-mail_sensitivity ).
      ENDIF.
      IF gs_finaa_last-mail_importance <> space.
        document->set_importance( gs_finaa_last-mail_importance ).
      ENDIF.

      CALL METHOD document->add_document_as_attachment
        EXPORTING
          im_document = attachment.
      send_request->set_document( document ).

      IF gs_finaa_last-mail_send_prio <> space.
        send_request->set_priority( gs_finaa_last-mail_send_prio ).
      ENDIF.

      IF gs_finaa_last-mail_outbox_link <> space.
        send_request->send_request->set_link_to_outbox( EXPORTING i_link_to_outbox = 'X' ).
      ENDIF.

      sent_to_all = send_request->send(
      i_with_error_screen = space ).
      IF sent_to_all = space.
        fimsg-msgno = '750'.
        fimsg-msgv1 = sy-subrc.
        IF reguh-lifnr <> space.
          fimsg-msgv2 = reguh-lifnr.
        ELSE.
          fimsg-msgv2 = reguh-kunnr.
        ENDIF.
        fimsg-msgv3 = reguh-zbukr.
        PERFORM message USING '750'.
      ENDIF.

    CATCH cx_bcs INTO bcs_exception.
      fimsg-msgno = '750'.
      fimsg-msgv1 = sy-subrc.
      IF reguh-lifnr <> space.
        fimsg-msgv2 = reguh-lifnr.
      ELSE.
        fimsg-msgv2 = reguh-kunnr.
      ENDIF.
      fimsg-msgv3 = reguh-zbukr.
      PERFORM message USING '750'.
      cd_error = 'X'.
  ENDTRY.

ENDFORM.                    "send_mail_with_attachm

*&---------------------------------------------------------------------*
*&      Form  convert_itf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM convert_itf.
  DATA : x_objcont TYPE soli_tab WITH HEADER LINE,
        x_objcont_line LIKE soli,
        hltlines TYPE i, so10_lines TYPE i,
        htabix LIKE sy-tabix,
        lp_fle1(2) TYPE p, lp_fle2(2) TYPE p, lp_off1 TYPE p, linecnt TYPE p,
        hfeld(500) TYPE c,
        ltxt_tdtab_c256(256) OCCURS 5 WITH HEADER LINE,
        ltxt_tdtab_x256 TYPE tdtab_x256,
        ls_tdtab_x256   TYPE LINE OF tdtab_x256.
  FIELD-SYMBOLS <cptr>  TYPE c.

* convert gt_lines to destination format
  CALL FUNCTION 'CONVERT_ITF_TO_ASCII'
    EXPORTING
      tabletype         = 'BIN'
    IMPORTING
      x_datatab         = ltxt_tdtab_x256
    TABLES
      itf_lines         = gt_lines_last
    EXCEPTIONS
      invalid_tabletype = 1
      OTHERS            = 2.
  LOOP AT ltxt_tdtab_x256 INTO ls_tdtab_x256.
    ASSIGN ls_tdtab_x256 TO <cptr> CASTING.
    ltxt_tdtab_c256 = <cptr>.
    APPEND ltxt_tdtab_c256.
  ENDLOOP.

  IF cl_abap_char_utilities=>charsize > 1.
    DATA tab_c256(256) OCCURS 5 WITH HEADER LINE.
    DATA : i TYPE i, ld_appended(1) TYPE c.
    LOOP AT ltxt_tdtab_c256.
      i = sy-tabix MOD 2.
      ld_appended = space.
      IF i = 1.                         " uneven
        tab_c256 = ltxt_tdtab_c256.
      ELSE.
        tab_c256+128 = ltxt_tdtab_c256.  " even
        APPEND tab_c256.
        ld_appended = 'X'.
      ENDIF.
    ENDLOOP.
    IF  ld_appended = space.
      APPEND tab_c256.                   " append last line.
    ENDIF.
    ltxt_tdtab_c256[] = tab_c256[].
  ENDIF.

* convert to 255 for call to cl_bcs
  DESCRIBE FIELD  x_objcont_line LENGTH lp_fle2 IN CHARACTER MODE.
  DATA ls_string TYPE string.
  LOOP AT ltxt_tdtab_c256.
    CONCATENATE ls_string ltxt_tdtab_c256 INTO ls_string.
  ENDLOOP.
  WHILE ls_string <> ''.
    x_objcont = ls_string.
    APPEND x_objcont.
    SHIFT ls_string BY lp_fle2 PLACES IN CHARACTER MODE.
  ENDWHILE.
  gt_text_mail[] = x_objcont[].

ENDFORM.                    "convert_itf

*&---------------------------------------------------------------------*
*&      Form  convert_advice
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_OTFDATA text
*      -->N_OBJCONT  text
*----------------------------------------------------------------------*
FORM convert_advice TABLES  it_otfdata STRUCTURE itcoo
                            n_objcont  TYPE soli_tab
                            e_solix    TYPE solix_tab.

  DATA: ld_hformat(10) TYPE c, doc_size(12) TYPE c,
        hltlines TYPE i, so10_lines TYPE i,
        htabix LIKE sy-tabix,
        lp_fle1(2) TYPE p, lp_fle2(2) TYPE p, lp_off1 TYPE p, linecnt TYPE p,
        hfeld(500) TYPE c,
        lt_hotfdata LIKE itcoo OCCURS 1 WITH HEADER LINE,
        htline LIKE tline OCCURS 1 WITH HEADER LINE,
        x_objcont TYPE soli_tab WITH HEADER LINE,
        x_objcont_line LIKE soli,
        ld_binfile TYPE xstring,
        lt_solix   TYPE solix_tab ,
        wa_soli TYPE soli,
        wa_solix TYPE solix,
        i TYPE i, n TYPE i.

  FIELD-SYMBOLS: <ptr_hex> TYPE solix.

* convert data
  LOOP AT it_otfdata INTO lt_hotfdata.
    APPEND lt_hotfdata.
  ENDLOOP.
  ld_hformat = gs_finaa_last-textf.
  IF ld_hformat IS INITIAL OR ld_hformat = 'PDF'.
    ld_hformat = 'PDF'.               "PDF as default
  ELSE.
    ld_hformat = 'ASCII'.
  ENDIF.
  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = ld_hformat
    IMPORTING
      bin_filesize          = doc_size
      bin_file              = ld_binfile
    TABLES
      otf                   = lt_hotfdata
      lines                 = htline
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      OTHERS                = 4.

  n = XSTRLEN( ld_binfile ).
  WHILE i < n.
    wa_solix-line = ld_binfile+i.
    APPEND wa_solix TO lt_solix.
    i = i + 255.
  ENDWHILE.

  e_solix[] = lt_solix[].

  IF ld_hformat <> 'PDF'.
    LOOP AT htline.
      x_objcont = htline-tdline.
      APPEND x_objcont TO n_objcont.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "convert_advice
*  Allgemeine Unterprogramme                                           *
*  international subroutines                                           *
*----------------------------------------------------------------------*
  INCLUDE zrffori99_1.
*  INCLUDE RFFORI99.
