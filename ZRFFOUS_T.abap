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


************************************************************************
*                                                                      *
* Include RFFORI99, used in the payment print reports RFFOxxxz         *
* international subroutines                                            *
*                                                                      *
* subroutine                       called by FORM / by REPORT, INCLUDE *
* -------------------------------------------------------------------- *
* INIT (initialization)                                       RFFOxxxy *
* F4_FORMULAR (value request for layout sets)                 RFFOxxxy *
* DATENTRAEGER_SELEKTIEREN (select data carrier)     RFFODTA0/RFFOEDI2 *
* VORBEREITUNG (preparation)                                  RFFOxxxy *
* PRUEFUNG (checks)                                           RFFOxxxy *
* EXTRACT_VORBEREITUNG (prepare extract at GET REGUH)         RFFOxxxy *
* EXTRACT (extracts data at GET REGUP)                        RFFOxxxy *
* SORTIERUNG (sort)                       EXTRACT/EXTRACT_VORBEREITUNG *
* SORTIERUNG_ASSIGN (help program for sort)                 SORTIERUNG *
* VORZEICHEN_SETZEN (set sign)                    EXTRACT_VORBEREITUNG *
*                                           EINZELPOSTENFELDER_FUELLEN *
* ISOCODE_UMSETZEN (read ISO code)                            RFFORI05 *
*                                            BUCHUNGSKREIS_DATEN_LESEN *
*                                                 ZAHLUNGS_DATEN_LESEN *
* BUCHUNGSKREIS_DATEN_LESEN (read company code data)          RFFORInn *
* ZAHLWEG_DATEN_LESEN (read payment method data)              RFFORInn *
* HAUSBANK_DATEN_LESEN (read house bank data)                 RFFORInn *
* HAUSBANK_KONTO_LESEN (read account data)                    RFFORInn *
* FILL_ITCPO (fill default values for ITCPO)                  RFFORInn *
* MODIFY_ITCPO (modify ITCPO for payment forms on paper)      RFFORInn *
* PRINT_ON  (call new-page print on with appropiate param.)   RFFORInn *
* PRINT_OFF (call new-page print off)                         RFFORInn *
* EMPFBANK_DATEN_LESEN (read payee's bank data)               RFFORInn *
* ZAHLUNGS_DATEN_LESEN (read payment data)                    RFFORInn *
* ZAHLUNGS_DATEN_LESEN_HLP (help program)         ZAHLUNGS_DATEN_LESEN *
* SACHBERARBEITER_KURZINFO (short info field clark)
* HR_REMITTANCE_ACKNOWLEDGEMENT                                EXTRACT *
* HR_REMITTANCE_ACKNOWLEDGE_RFC                          FEHLERMELDUNG *
* HR_FORMULAR_LESEN (read HR form)                         RFFORI01,06 *
* WEISUNGSSCHLUESSEL_LESEN (read instruction keys)            RFFORInn *
* WEISUNGSSCHLUESSEL_UMSETZEN (transpose instruction key)     RFFOxxxy *
* GET_CLEARING_CODE                                           RFFOxxxy *
* ADRESSE_LESEN (read customizing address)                    RFFORInn *
* BANKADRESSE_LESEN (read bank address)                       RFFORI99 *
* ADDR_GET                            ADRESSE_LESEN, BANKADRESSE_LESEN *
* LAENDER_LESEN (read country data)                           RFFORInn *
* ZAHLWEG_EINFUEGEN (insert payment method)                   RFFORInn *
* SUMMENFELDER_INITIALISIEREN (initialize total fields)       RFFORInn *
* EINZELPOSTENFELDER_FUELLEN (fill single item fields)        RFFORInn *
* SUMMENFELDER_FUELLEN (fill total fields)                    RFFORInn *
* DTA_GLOBALS_ERSETZEN (replace globals for DME)           RFFORI04,05 *
* DTA_TEXT_AUFBEREITEN (check text fields of DME)          RFFORI04,05 *
*                                                 DTA_GLOBALS_ERSETZEN *
* DATEN_SICHERN (save data for test print)                    RFFORInn *
* DATEN_ZURUECK (data back after test print)                  RFFORInn *
* ZIFFERN_IN_WORTEN (numbers in words)                        RFFORInn *
* DATUM_IN_DDMMYY (convert date to ddmmyy)                    RFFORInn *
* ABBRUCH_DURCH_UEBERLAUF (form overflow abend)            RFFORI04,05 *
* FEHLERMELDUNGEN (error messages)                            RFFOxxxy *
* MESSAGE (collect of messages)                        FEHLERMELDUNGEN *
*                                                             RFFOxxxy *
* INFORMATION (information)                                   RFFOxxxy *
* INFORMATION_2 (informtaion as accessible write list)        RFFOxxxy *
*                                              ABBRUCH_DURCH_UEBERLAUF *
* AT LINE-SELECTION                                                    *
* BELEGDATEN_SCHREIBEN (append document number)               RFFOxxxy *
* TAB_BELEGE_SCHREIBEN (store table with doc.numbers) DATEI_SCHLIESSEN *
* ZUSATZFELD_FUELLEN (fill add'l field)                       RFFOxxxy *
* TEMSE_OEFFNEN (TemSe open)                                  RFFOxxxy *
* TEMSE_SCHREIBEN (TemSe write)                               RFFOxxxy *
* TEMSE_SCHLIESSEN (TemSe close)                              RFFOxxxy *
* NAECHSTER_INDEX (Next index)                                RFFOxxxy *
* TEMSE_NAME (TemSe name)                                TEMSE_OEFFNEN *
* DATEI_OEFFNEN (Open file)                                   RFFOxxxy *
* DATEI_SCHLIESSEN (Close file)                               RFFOxxxy *
* FUELLEN_REGUT (Fill initial REGUT-fields)                   RFFOxxxy *
* ABSCHLUSS_REGUT (Fill final REGUT-fields)                   RFFOxxxy *
* STORE_ON_FILE (store date either on TemSe or on file-syst)  RFFOxxxy *
* FORM READ_SCB_INDICATOR (reads t015l)                       RFFOxxxy *
* Information_list_heading                               INFORMATION_2 *
* Information_on_dme                                     INFORMATION_2 *
* Information_on_lists                                   INFORMATION_2 *
************************************************************************



*----------------------------------------------------------------------*
* FORM INIT                                                            *
*----------------------------------------------------------------------*
* Belegung der Felder auf dem Selektionsbild aus dem Textpool SAPDBPYF *
*----------------------------------------------------------------------*
* Fill fields on the selection screen from textpool SAPDBPYF           *
*----------------------------------------------------------------------*
FORM init.


  LOOP AT tab_selfields.
    ASSIGN (tab_selfields-field) TO <selfield>.
    PERFORM text(sapdbpyf) USING tab_selfields-text <selfield>.
  ENDLOOP.


ENDFORM.                               "INIT



*----------------------------------------------------------------------*
* FORM F4_FORMULAR                                                     *
*----------------------------------------------------------------------*
* F4 für SAPscript-Formulare                                           *
* F4 for layout sets                                                   *
*----------------------------------------------------------------------*
FORM f4_formular USING layout_set.


* DATA hskeyreport LIKE skeyreport.

* CALL FUNCTION 'DISPLAY_REPORTING_TREE_F4'
*      EXPORTING
*           i_tree_id             = 'SAPF'
*           i_get_standard_client = 'X'
*           i_name_of_first_node  = 'FI-2'
*      IMPORTING
*           e_skeyreport          = hskeyreport
*      EXCEPTIONS
*           OTHERS                = 4.
* IF sy-subrc eq 0 and hskeyreport-report ne space.
*   layout_set = hskeyreport-report.
* ENDIF.
  DATA: hp_form_name LIKE thead-tdform.
  CALL FUNCTION 'DISPLAY_FORM_TREE_F4'
    EXPORTING
      p_tree_name = 'FI-2'
*     P_DISPLAY_MODE  = ' '
*     I_FORM_NAME =
    IMPORTING
      p_form_name = hp_form_name
*     P_FORM_LANGUAGE =
    EXCEPTIONS
*     CANCELLED   = 1
*     PARAMETER_ERROR = 2
*     NOT_FOUND   = 3
      OTHERS      = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  IF sy-subrc EQ 0 AND hp_form_name NE space.
    layout_set = hp_form_name.
  ENDIF.


ENDFORM.                               "F4 Formular



*----------------------------------------------------------------------*
* FORM DATENTRAEGER_SELEKTIEREN                                        *
*----------------------------------------------------------------------*
* Füllen der Parameter für die Selektion der Belege eines Datenträgers *
* fill parameters to select the documents of a data carrier            *
*----------------------------------------------------------------------*
* RENUM - reference number (FDTA)                                      *
* LAUFD - run date                                                     *
* LAUFI - run identification                                           *
* XVORL - will be SPACE if selection was successfull
* VBLNR - range of document numbers                                    *
*----------------------------------------------------------------------*
FORM datentraeger_selektieren TABLES vblnr
                                     sel_pyord
                              USING renum laufd laufi xvorl.


  RANGES up_vblnr FOR reguh-vblnr.
  RANGES up_pyord FOR reguh-vblnr.

  DATA up_renum(10) TYPE n.
  up_renum = renum.
  renum    = up_renum.
  CALL FUNCTION 'GET_DOCUMENTS'
    EXPORTING
      i_belege     = 'X'
      i_refno      = renum
      i_regut      = 'X'
    IMPORTING
      e_regut      = regut
    TABLES
      tab_belege   = tab_belege30a
    EXCEPTIONS
      no_documents = 1
      no_regut     = 2
      wrong_number = 3.
  IF sy-subrc EQ 1.
    CALL FUNCTION 'GET_DOCUMENTS'
      EXPORTING
        i_belege   = space
        i_refno    = renum
        i_regut    = 'X'
      IMPORTING
        e_regut    = regut
      TABLES
        tab_belege = tab_belege30a.
    IF sy-batch EQ space.
      MESSAGE e288 WITH renum regut-laufd regut-laufi.
    ELSE.
      MESSAGE s288 WITH renum regut-laufd regut-laufi.
      STOP.
    ENDIF.
  ELSEIF sy-subrc NE 0.
    IF sy-batch EQ space.
      MESSAGE e287 WITH renum.
    ELSE.
      MESSAGE s287 WITH renum.
      STOP.
    ENDIF.
  ENDIF.
  laufi = regut-laufi.
  laufd = regut-laufd.
  xvorl = space.
  up_vblnr-option = 'EQ'.
  up_vblnr-sign   = 'I'.
  up_vblnr-high   = space.

  up_pyord-option = 'EQ'.
  up_pyord-sign   = 'I'.
  up_pyord-high   = space.

  LOOP AT tab_belege30a.
    IF tab_belege30a-belnr <> space.
      up_vblnr-low  = tab_belege30a-belnr.
      APPEND up_vblnr.
    ELSEIF tab_belege30a-pyord <> space.
      up_pyord-low  = tab_belege30a-pyord.
      APPEND up_pyord.
    ENDIF.
    tab_vblnr_renum       = tab_belege30a.
    tab_vblnr_renum-renum = renum.
    APPEND tab_vblnr_renum.
  ENDLOOP.
  FREE tab_belege30a.
  vblnr[] = up_vblnr[].
  sel_pyord[] = up_pyord[].
ENDFORM.                               "DATENTRAEGER_SELEKTIEREN



*----------------------------------------------------------------------*
* FORM VORBEREITUNG                                                    *
*----------------------------------------------------------------------*
* Vorbereitung der Verarbeitung                                        *
* preparation                                                          *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM vorbereitung.

  IF sy-binpt EQ space.
    COMMIT WORK.
  ENDIF.

* Laufkennung (Applikation) setzen
* Fill application of run
  hlp_laufk = zw_laufi+5(1).

* Probedruck vorbereiten
* prepare test print
  FIELD-SYMBOLS:
    <tabfeld>.

  DATA:
    up_tab        LIKE dfies-tabname,
    up_dfies      LIKE dfies OCCURS 0 WITH HEADER LINE,
    up_feld(20)   TYPE c,
    up_kreuz(132) TYPE c.

  IF par_anzp GT 0.

    CLEAR up_kreuz WITH 'X'.

    DO 5 TIMES.

      CASE sy-index.
        WHEN 1.
          up_tab  = 'FSABE'.
          up_feld = 'XXX_FSABE-'.
        WHEN 2.
          up_tab  = 'REGUD'.
          up_feld = 'XXX_REGUD-'.
        WHEN 3.
          up_tab  = 'REGUH'.
          up_feld = 'XXX_REGUH-'.
        WHEN 4.
          up_tab  = 'REGUP'.
          up_feld = 'XXX_REGUP-'.
        WHEN 5.
          up_tab  = 'SPELL'.
          up_feld = 'XXX_SPELL-'.
      ENDCASE.

      CALL FUNCTION 'DDIF_NAMETAB_GET'
        EXPORTING
          tabname   = up_tab
        TABLES
          dfies_tab = up_dfies
        EXCEPTIONS
          OTHERS    = 4.
      IF sy-subrc NE 0.
        REFRESH up_dfies.
      ENDIF.

      LOOP AT up_dfies.
        up_feld+10 = up_dfies-fieldname.
        ASSIGN (up_feld) TO <tabfeld>.
        CASE up_dfies-inttype.
          WHEN 'C'.
            <tabfeld> = up_kreuz.
          WHEN 'D'.
            <tabfeld> = '19000101'.
          WHEN OTHERS.
            CLEAR <tabfeld>.
        ENDCASE.
      ENDLOOP.

    ENDDO.

    CLEAR xxx_fsabe-salut.

  ENDIF.

* Vorbereitung für die Berechtigungsprüfung ----------------------------
* prepare authorization checks -----------------------------------------
  IF hlp_laufk NE '*'.
    IF hlp_laufk NE 'P'.               "Prüfung für FI-Bestände
      IF zw_xvorl EQ space.            "checks for FI-data
        hlp_fbtch = 25.
      ELSE.
        hlp_fbtch = 15.
      ENDIF.
      CLEAR flg_koart_auth.
      IF sy-tcode = 'ZTR004'. " Tr antigua 'ZFI172'.
        flg_koart_auth-k = 'X'.
        flg_koart_auth-d = 'X'.
        flg_koart_auth-s = 'X'.
      ELSE.
        AUTHORITY-CHECK OBJECT 'F_REGU_KOA'
          ID 'KOART' FIELD 'K'
          ID 'FBTCH' FIELD hlp_fbtch.
        IF sy-subrc EQ 0.
          flg_koart_auth-k = 'X'.
        ENDIF.
        AUTHORITY-CHECK OBJECT 'F_REGU_KOA'
          ID 'KOART' FIELD 'D'
          ID 'FBTCH' FIELD hlp_fbtch.
        IF sy-subrc EQ 0.
          flg_koart_auth-d = 'X'.
        ENDIF.
        AUTHORITY-CHECK OBJECT 'F_REGU_KOA'
          ID 'KOART' FIELD 'S'
          ID 'FBTCH' FIELD hlp_fbtch.
        IF sy-subrc EQ 0.
          flg_koart_auth-s = 'X'.
        ENDIF.
      ENDIF.

      IF flg_koart_auth EQ space.      "Keine Kontoarten-Berechtigung
        IF sy-batch EQ space.          "no account type authority
          MESSAGE a066 WITH hlp_fbtch.
        ELSE.
          MESSAGE s066 WITH hlp_fbtch.
          MESSAGE s094.
          STOP.
        ENDIF.
      ENDIF.
*      ENDIF.
    ELSE.                              "Prüfung für HR-Bestände
      autha-repid = sy-repid.          "checks for HR-data
      CALL FUNCTION 'HR_PROGRAM_CHECK_AUTHORIZATION'
        EXPORTING
          repid = autha-repid
        IMPORTING
          subrc = hlp_subrc.
      IF hlp_subrc NE 0.
        IF sy-batch EQ space.
          MESSAGE a189.
        ELSE.
          MESSAGE s189.
          MESSAGE s094.
          STOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

* Variantennamen merken ------------------------------------------------
* store name of report variant -----------------------------------------
  IF par_vari EQ space.
    par_vari = sy-slset.
  ENDIF.

* Sofortdruckparameter setzen ------------------------------------------
* activate parameters for immediate printing ---------------------------
  IF par_sofo NE space.
    par_sofw = 'X'.
    par_sofz = 'X'.
    par_sofa = 'X'.
    par_sofb = 'X'.
  ENDIF.

* Vorbelegen von Feldern -----------------------------------------------
* fields with fix values -----------------------------------------------
  hlp_maxbetrag = 10000000000000.
  hlp_zeilen    = 0.

  CLEAR txt_uline1 WITH '_'.
  CLEAR txt_uline2 WITH '='.

  CALL FUNCTION 'FI_DME_CHARACTERS'
    IMPORTING
      e_cr   = hlp_cr
      e_lf   = hlp_lf
      e_crlf = hlp_crlf
      e_eof  = hlp_eof.

* Länder mit separatem Bankcode ----------------------------------------
* Countries with separate bank-code ------------------------------------
  REFRESH tab_bankcode.
  tab_bankcode-high   = space.
  tab_bankcode-sign   = 'I'.
  tab_bankcode-option = 'EQ'.
  tab_bankcode-low    = 'A'.   APPEND tab_bankcode.  "Österreich
  tab_bankcode-low    = 'CH'.  APPEND tab_bankcode.  "Schweiz
  tab_bankcode-low    = 'CDN'. APPEND tab_bankcode.  "Kanada
  tab_bankcode-low    = 'D'.   APPEND tab_bankcode.  "Deutschland
  tab_bankcode-low    = 'GB'.  APPEND tab_bankcode.  "Großbritannien
  tab_bankcode-low    = 'FL'.  APPEND tab_bankcode.  "Liechtenstein
  tab_bankcode-low    = 'I'.   APPEND tab_bankcode.  "Italien
  tab_bankcode-low    = 'IRL'. APPEND tab_bankcode.  "Irland
  tab_bankcode-low    = 'IS'.  APPEND tab_bankcode.  "Island
  tab_bankcode-low    = 'USA'. APPEND tab_bankcode.         "USA

* Lesen allgemeiner Texte aus dem Textpool SAPDBPYF --------------------
* Read texts from textpool SAPDBPYF ------------------------------------
  PERFORM text(sapdbpyf) USING:
   001 text_001, 505 text_505, 605 text_605, 800 text_800, 900 text_900,
   002 text_002, 510 text_510, 610 text_610, 801 text_801, 901 text_901,
   003 text_003, 512 text_512, 611 text_611, 802 text_802, 902 text_902,
   004 text_004, 513 text_513, 612 text_612, 803 text_803, 903 text_903,
   005 text_005, 514 text_514, 613 text_613, 804 text_804, 904 text_904,
   006 text_006, 515 text_515, 614 text_614, 805 text_805,
   090 text_090, 520 text_520, 615 text_615, 806 text_806,
   091 text_091, 525 text_525, 620 text_620, 807 text_807,
   092 text_092, 526 text_526, 625 text_625, 809 text_809,
   093 text_093, 530 text_530, 630 text_630, 810 text_810,
   094 text_094, 535 text_535, 634 text_634, 811 text_811,
   095 text_095, 540 text_540, 635 text_635, 812 text_812,
   096 text_096, 545 text_545,               813 text_813,
   097 text_097, 546 text_546,               814 text_814,
   098 text_098, 550 text_550,               815 text_815,
                 555 text_555,               816 text_816,
                                             820 text_820,
                                             821 text_821,
                                             822 text_822,
                                             830 text_830,
                                             831 text_831,
                                             832 text_832,
                                             834 text_834.
  PERFORM text(sapdbpyf) USING:
   080 text_080, 081 text_081, 082 text_082, 083 text_083, 084 text_084,
   085 text_085.

* Prüfung, ob Lauf erfolgreich abgeschlossen ---------------------------
* check that payment run was completed successfully --------------------
  IF hlp_laufk NE '*'.                 "keine Prüfung bei Online-Druck
    SELECT SINGLE * FROM reguv         "online print => no check
      WHERE laufd EQ zw_laufd
      AND   laufi EQ zw_laufi.
    IF zw_xvorl EQ space.
      IF reguv-xecht NE 'X'.
        IF sy-batch EQ space.
          MESSAGE s098 WITH zw_laufd zw_laufi.
          STOP.
        ELSE.
          MESSAGE s098 WITH zw_laufd zw_laufi.
          MESSAGE s094.
          STOP.
        ENDIF.
      ENDIF.
    ELSE.
      IF reguv-xvore NE 'X'.
        IF sy-batch EQ space.
          MESSAGE s099 WITH zw_laufd zw_laufi.
          STOP.
        ELSE.
          MESSAGE s099 WITH zw_laufd zw_laufi.
          MESSAGE s094.
          STOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

* Prüfung, ob Lauf-Id nur für zahllaufübergreifende Zahlungsträger
* check that run id is not reserved for cross payment run media
  CALL FUNCTION 'FIBL_PAYMENT_RUN_MERGE_CHECK'
    EXPORTING
      i_laufd    = zw_laufd
      i_laufi    = zw_laufi
    EXCEPTIONS
      merge_only = 1.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    STOP.
  ENDIF.

* logisches System des Mandanten lesen
* read logical system of client
  SELECT SINGLE * FROM t000 WHERE mandt EQ sy-mandt.
  REFRESH tab_rfc.

* prüfen, ob IBAN-Funktionalität verfügbar
* check that IBAN function is available
  flg_iban = 1.


ENDFORM.                               "VORBEREITUNG



*----------------------------------------------------------------------*
* FORM PRUEFUNG                                                        *
*----------------------------------------------------------------------*
* Prüfung der selektierten Daten                                       *
* Checks of selected data                                              *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM pruefung.


* Keine Ausnahmen behandeln
* no exceptions
  IF reguh-vblnr EQ space.
    REJECT.
  ENDIF.

* Nachlesen der Buchungskreisdaten / des Geschäftsjahres
* read company code data / business year
  ON CHANGE OF reguh-zbukr.
    SELECT SINGLE * FROM t001
      WHERE bukrs EQ reguh-zbukr.
  ENDON.
  ON CHANGE OF reguh-zaldt OR t001-periv.
    CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
      EXPORTING
        i_date  = reguh-zaldt
        i_periv = t001-periv
      IMPORTING
        e_gjahr = regud-gjahr.
    xxx_reguh-zaldt = reguh-zaldt.
  ENDON.

* Berechtigungsprüfungen -----------------------------------------------
* authorization checks -------------------------------------------------
  IF hlp_laufk NA 'P*'.                "Prüfung für FI-Bestände
*    IF sy-tcode <> 'ZFI172'.
    AUTHORITY-CHECK OBJECT 'F_REGU_BUK'"checks for FI-data
      ID 'BUKRS' FIELD reguh-zbukr
      ID 'FBTCH' FIELD hlp_fbtch.
    IF sy-tcode = 'ZTR004'. " Tr antigua 'ZFI172'.
      sy-subrc = 0.
    ENDIF.
    IF sy-subrc NE 0.
      err_auth-autob = 'F_REGU_BUK'.
      err_auth-field = 'BUKRS'.
      err_auth-value = reguh-zbukr.
      err_auth-actvt = hlp_fbtch.
      COLLECT err_auth.
      REJECT.
    ENDIF.
*    ENDIF.
    err_auth-autob = 'F_REGU_KOA'.
    err_auth-field = 'KOART'.
    err_auth-value = space.
    err_auth-actvt = hlp_fbtch.
    IF reguh-lifnr EQ space AND reguh-kunnr EQ space.
      IF flg_koart_auth-s EQ space.
        err_auth-value = 'S'.
      ENDIF.
    ELSEIF reguh-lifnr NE space AND reguh-kunnr NE space.
      IF flg_koart_auth-k EQ space AND flg_koart_auth-d EQ space.
        err_auth-value = 'D / K'.
      ENDIF.
    ELSEIF reguh-lifnr NE space.
      IF flg_koart_auth-k EQ space.
        err_auth-value = 'K'.
      ENDIF.
    ELSEIF flg_koart_auth-d EQ space.
      err_auth-value = 'D'.
    ENDIF.
    IF err_auth-value NE space.
      COLLECT err_auth.
      REJECT.
    ENDIF.
  ENDIF.

* feinere Berechtigunsprüfung für HR
  IF hlp_laufk = 'P' AND payr-pernr NE 0.
    CALL FUNCTION 'HR_CHECK_AUTHORITY_PERNR'
      EXPORTING
        pernr  = payr-pernr
        begda  = payr-laufd
        endda  = payr-laufd
      EXCEPTIONS
        OTHERS = 4.
    IF sy-subrc <> 0.
      err_auth-autob = 'P_ORGIN'.
      err_auth-field = 'PERNR'.
      err_auth-value = reguh-pernr.
      err_auth-actvt = hlp_fbtch.
      COLLECT err_auth.
      REJECT.
    ENDIF.
  ENDIF.

* Prüfung, ob Report für diesen Zahlweg zugelassen ist -----------------
* check that report is valid for this payment method -------------------
  IF flg_avis EQ 0.                    "all reports except RFFOAVIS
    IF reguh-rzawe EQ space.
      REJECT.
    ENDIF.
    CLEAR tab_t042z.
    READ TABLE tab_t042z WITH KEY land1 = t001-land1
                                  zlsch = reguh-rzawe.
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM t042z
        WHERE land1 EQ t001-land1
        AND   zlsch EQ reguh-rzawe.
      IF sy-subrc NE 0.
        IF sy-batch EQ space.
          MESSAGE a350 WITH reguh-rzawe t001-land1.
        ELSE.
          MESSAGE s350 WITH reguh-rzawe t001-land1.
          MESSAGE s094.
          STOP.
        ENDIF.
      ENDIF.
      tab_t042z = t042z.
      IF ( tab_t042z-progn EQ sy-repid OR tab_t042z-progn EQ 'ZFITR008'      "Scotiabank
            OR tab_t042z-progn EQ 'ZFI0256'  OR tab_t042z-progn EQ 'ZRFFOUS_T' )
         OR par_begl EQ 'D'.            "no check for RFFODTA0 / RFFOEDI0
        tab_t042z-xsele = 'X'.
      ELSE.
        tab_t042z-xsele = space.
        err_t042z-land1 = tab_t042z-land1.
        err_t042z-zlsch = tab_t042z-zlsch.
        APPEND err_t042z.
      ENDIF.
      APPEND tab_t042z.
      SORT tab_t042z.
    ENDIF.
    IF tab_t042z-xsele EQ space.
      REJECT.
    ENDIF.
    t042z       = tab_t042z.
    regud-xeinz = tab_t042z-xeinz.
    hlp_xeuro   = tab_t042z-xeuro.
  ELSE.                                "RFFOAVIS only
    IF reguh-rzawe NE space OR reguh-avisg EQ 'V'.
      REJECT.
    ENDIF.
  ENDIF.

* Prüfung, ob Beleg bereits gebucht ist --------------------------------
* check that payment document is updated -------------------------------
  IF par_belp EQ 'X'
    AND reguh-paygr+18(2) NE '$J'      "$J - restart of Japanese DME
    AND hlp_laufk NE 'P'.              "not HR

    IF reguh-pyord EQ space.           "payment document
      SELECT SINGLE * FROM bkpf
        WHERE bukrs EQ reguh-zbukr
        AND   belnr EQ reguh-vblnr
        AND   gjahr EQ regud-gjahr.
    ELSE.                              "payment order
      CLEAR bkpf.
      SELECT SINGLE * FROM pyordh
        WHERE pyord EQ reguh-pyord.
    ENDIF.

    IF sy-subrc NE 0.
      MOVE-CORRESPONDING reguh TO err_nicht_verbucht.
      COLLECT err_nicht_verbucht.
      REJECT.
    ENDIF.
    IF bkpf-stblg NE space.
      fimsg-msgv1 = reguh-zbukr.
      fimsg-msgv2 = reguh-vblnr.
      PERFORM message USING '381'.
      REJECT.
    ENDIF.
  ENDIF.

* Zahlungsbelegverprobung für Abrechnungsergebnisse
* payment document validation for payroll results
  IF par_belp EQ 'X' AND reguh-dorigin EQ 'HR-PY'.
    DATA up_doc1r TYPE doc1r_fpm.
    DATA up_doc1t TYPE doc1t_fpm.
    CALL FUNCTION 'FI_REF_DOCUMENT_FILL'
      EXPORTING
        im_pernr = reguh-pernr
        im_seqnr = reguh-seqnr
        im_btznr = reguh-btznr
      IMPORTING
        ex_doc1r = up_doc1r
        ex_doc1t = up_doc1t.
    CALL FUNCTION 'FI_REF_DOCUMENT_CHECK'
      EXPORTING
        im_doc1r  = up_doc1r
        im_doc1t  = up_doc1t
        im_origin = reguh-dorigin
      EXCEPTIONS
        not_found = 4.
    IF sy-subrc NE 0.
      fimsg-msgid = sy-msgid.
      fimsg-msgv1 = sy-msgv1.
      fimsg-msgv2 = sy-msgv2.
      fimsg-msgv3 = sy-msgv3.
      fimsg-msgv4 = sy-msgv4.
      PERFORM message USING sy-msgno.
      REJECT.
    ENDIF.
  ENDIF.

* Prüfung, ob Beleg mit EDI versendet werden soll
* check whether document should be handled via EDI
  IF zw_edisl EQ space AND reguh-edibn EQ 'X'.
    MOVE-CORRESPONDING reguh TO err_edi.
    APPEND err_edi.
    REJECT.
  ENDIF.

* Abweichender Zahlungsempfänger ---------------------------------------
* alternative payee ----------------------------------------------------
  regud-xabwz   = space.
  IF reguh-empfg(1)    EQ '>' AND      "abweichender Zahlungsempfänger
     reguh-empfg+11(2) NE '>F'.        "im Stamm, aber nicht Filiale
    regud-xabwz = 'X'.
  ENDIF.
  IF reguh-empfg(1)    NE '>' AND      "abweichender Zahlungsempfänger
     reguh-empfg       NE space.       "im Beleg, nicht CPD-Konto
    IF reguh-lifnr NE space.
      SELECT SINGLE * FROM lfa1 WHERE lifnr EQ reguh-lifnr.
      IF sy-subrc EQ 0 AND lfa1-xcpdk EQ space.
        regud-xabwz = 'X'.
      ENDIF.
    ELSE.
      SELECT SINGLE * FROM kna1 WHERE kunnr EQ reguh-kunnr.
      IF sy-subrc EQ 0 AND kna1-xcpdk EQ space.
        regud-xabwz = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.                               "PRUEFUNG



*----------------------------------------------------------------------*
* FORM EXTRACT_VORBEREITUNG                                            *
*----------------------------------------------------------------------*
* Sortierfelder und Vorzeichen setzen                                  *
* Online-Umsetzung alter Daten:                                        *
*    aus Releases < 2.0   Felder REGUH-UBNKY und REGUH-ZBNKY aus       *
*                         Bankleitzahl oder der Kontonummer füllen     *
*    aus Releases < 3.0   Feld REGUH-AUSFD mit Zahlungsdatum füllen,   *
*                         leeres REGUH-ABSBU (z.B. Online-Scheckdruck) *
*                         mit REGUH-ZBUKR füllen                       *
*    ab Releases 4.0      Feld REGUH-KOINH füllen, falls es leer ist   *
*                         Feld REGUH-VBLNR mit PYORD füllen für die    *
*                         richtige Referenz auf dem Zahlungsträger     *
*    seit IBAN            Initialisierung von REGUH-ZBNKN falls        *
*                         Kontonummer technisch                        *
*----------------------------------------------------------------------*
* fill sort fields and sign                                            *
* correct old data online:                                             *
*    release < 2.0        fill REGUH-UBNKY and REGUH-ZBNKY using       *
*                         bank number or account number                *
*    release < 3.0        fill REGUH-AUSFD with payment date           *
*                         fill REGUH-ABSBU with REGUH-ZBUKR if it is   *
*                         empty (e.g. post and print)                  *
*    as of release 4.0    fill REGUH-KOINH if it is empty              *
*                         fill REGUH-VBLNR with PYORD to have the      *
*                         correct reference on the payment medium      *
*    since IBAN           initialize REGUH-ZBNKN if bank account       *
*                         number of recipient is technical             *
*----------------------------------------------------------------------*
FORM extract_vorbereitung.


  PERFORM vorzeichen_setzen USING 'H'.
  PERFORM sortierung USING 'H'.

* Reparatur 2.0 / repair 2.0
  IF reguh-ubnky EQ space.
    IF reguh-ubnkl NE space.
      reguh-ubnky = reguh-ubnkl.
    ELSE.
      reguh-ubnky = reguh-ubknt.
    ENDIF.
  ENDIF.
  IF reguh-zbnky EQ space.
    IF reguh-zbnkl NE space.
      reguh-zbnky = reguh-zbnkl.
    ELSE.
      reguh-zbnky = reguh-zbnkn.
    ENDIF.
  ENDIF.

* Reparatur 3.0 / repair 3.0
  IF reguh-ausfd LT reguh-zaldt.
    MOVE reguh-zaldt TO reguh-ausfd.
  ENDIF.
  IF reguh-absbu EQ space.
    reguh-absbu = reguh-zbukr.
  ENDIF.

* Reparatur 4.0 / repair 4.0
  IF reguh-koinh EQ space.
    reguh-koinh = reguh-znme1.
  ENDIF.
  IF reguh-pyord NE space.
    reguh-vblnr = reguh-pyord.
  ENDIF.

* Reparatur IBAN / repair IBAN
  CALL FUNCTION 'FI_TECH_ACCNO_CHECK_TRY'
    EXPORTING
      i_bankn = reguh-zbnkn
    IMPORTING
      e_xtech = flg_acc_tech.
  IF flg_acc_tech EQ 'X'.
    CLEAR reguh-zbnkn.
  ENDIF.


ENDFORM.                               "Extract Vorbereitung



*----------------------------------------------------------------------*
* FORM EXTRACT                                                         *
*----------------------------------------------------------------------*
* Sortierfelder füllen, Daten extrahieren                              *
* HR über erfolgreiche Zahlung informieren                             *
*----------------------------------------------------------------------*
* fill sort fields and extract data                                    *
* send information about payment to HR (3rd party remittance)          *
*----------------------------------------------------------------------*
FORM extract.


  PERFORM hr_remittance_acknowledgement.
  PERFORM sortierung USING 'P'.
  EXTRACT daten.
  flg_selektiert = 1.


ENDFORM.                               "Extract



*----------------------------------------------------------------------*
* FORM SORTIERUNG                                                      *
*----------------------------------------------------------------------*
* Vorbelegung der Sortierfelder gemäß den Vorgaben des Benutzers       *
* über T021M                                                           *
*----------------------------------------------------------------------*
* Filling the sort-fields with respect to the user's wishes (T021M)    *
*----------------------------------------------------------------------*
* Parameter SATZ bestimmt, welche Sortierfelder zu füllen sind         *
* parameter SATZ says which sort fields have to be filled              *
*----------------------------------------------------------------------*
FORM sortierung USING satz.

  STATICS:
    st_svarh LIKE t042e-svarh,
    st_svarp LIKE t042e-svarp.

* Lesen der Tabelle T021M mit den Sortiervarianten
  ON CHANGE OF reguh-zbukr OR reguh-rzawe.
    IF NOT reguh-rzawe IS INITIAL.
      SELECT SINGLE * FROM t042e
        WHERE zbukr EQ reguh-zbukr
          AND zlsch EQ reguh-rzawe.

      IF t042e-svarh IS INITIAL.
        st_svarh = hlp_svarh.
      ELSE.
        st_svarh = t042e-svarh.
      ENDIF.

      IF t042e-svarp IS INITIAL.
        st_svarp = hlp_svarp.
      ELSE.
        st_svarp = t042e-svarp.
      ENDIF.

    ELSE.
      st_svarh = hlp_svarh.
      st_svarp = hlp_svarp.
    ENDIF.
  ENDON.

  IF hlp_t021m_h-srvar NE st_svarh.
    SELECT SINGLE * FROM t021m INTO hlp_t021m_h
      WHERE progn = 'RFFO*   '
        AND anwnd = 'REGUH'
        AND srvar = st_svarh.
    IF sy-subrc NE 0.
      CLEAR hlp_t021m_h.
    ENDIF.
  ENDIF.
  IF hlp_t021m_p-srvar NE st_svarp.
    SELECT SINGLE * FROM t021m INTO hlp_t021m_p
      WHERE progn = 'RFFO*   '
        AND anwnd = 'REGUP'
        AND srvar = st_svarp.
    IF sy-subrc NE 0.
      CLEAR hlp_t021m_p.
    ENDIF.
  ENDIF.



* Füllen der Sortierfelder
  IF satz EQ 'H'.
    IF NOT reguh-srtf2 IS INITIAL.
      hlp_sorth1 = reguh-srtf2(16).
      hlp_sorth2 = reguh-srtf2+16(16).
      hlp_sorth3 = reguh-srtf2+32(16).
    ELSE.
      t021m = hlp_t021m_h.
      DO 3 TIMES.
        CASE sy-index.
          WHEN 1.
            PERFORM sortierung_assign USING
             t021m-tnam1 t021m-feld1 t021m-offs1 t021m-leng1 hlp_sorth1.
          WHEN 2.
            PERFORM sortierung_assign USING
             t021m-tnam2 t021m-feld2 t021m-offs2 t021m-leng2 hlp_sorth2.
          WHEN 3.
            PERFORM sortierung_assign USING
             t021m-tnam3 t021m-feld3 t021m-offs3 t021m-leng3 hlp_sorth3.
        ENDCASE.
      ENDDO.
    ENDIF.
  ELSE.
    t021m = hlp_t021m_p.
    DO 3 TIMES.
      CASE sy-index.
        WHEN 1.
          PERFORM sortierung_assign USING
            t021m-tnam1 t021m-feld1 t021m-offs1 t021m-leng1 hlp_sortp1.
        WHEN 2.
          PERFORM sortierung_assign USING
            t021m-tnam2 t021m-feld2 t021m-offs2 t021m-leng2 hlp_sortp2.
        WHEN 3.
          PERFORM sortierung_assign USING
            t021m-tnam3 t021m-feld3 t021m-offs3 t021m-leng3 hlp_sortp3.
      ENDCASE.
    ENDDO.
  ENDIF.


ENDFORM.                               "SORTIERUNG



*----------------------------------------------------------------------*
* FORM SORTIERUNG_ASSIGN                                               *
*----------------------------------------------------------------------*
* Hilfsprogamm für die Sortierung                                      *
* help program for sort                                                *
*----------------------------------------------------------------------*
FORM sortierung_assign USING tnam feld offs leng sort.


  FIELD-SYMBOLS <feld>.                "Inhalt des Sortierfeldes
  DATA up_feld(21) TYPE c.             "Name des Sortierfeldes aus T021M
  DATA l_describe TYPE REF TO cl_abap_typedescr.

  CHECK NOT tnam IS INITIAL AND NOT feld IS INITIAL.
  up_feld    = tnam.                   "REGUH oder REGUP
  up_feld+10 = '-'.
  up_feld+11 = feld.                   "UZAWE,ZPST2,XBLNR etc.
  CONDENSE up_feld NO-GAPS.

  IF up_feld EQ 'REGUSRT-HZPST'.       "PLZ Zahlungsempfänger
    IF reguh-zpst2 NE space.
      up_feld = 'REGUH-ZPST2'.
    ELSE.
      up_feld = 'REGUH-ZPSTL'.
    ENDIF.
  ENDIF.

  IF up_feld EQ 'REGUSRT-HPSTL'.                            "PLZ
    IF reguh-pstl2 NE space.
      up_feld = 'REGUH-PSTL2'.
    ELSE.
      up_feld = 'REGUH-PSTLZ'.
    ENDIF.
  ENDIF.

  ASSIGN TABLE FIELD (up_feld) TO <feld>.
  CLEAR sort.                          "Sortierfeld nur füllen, wenn
  CHECK leng NE 0.                     "T021M-Eintrag nicht leer ist

  l_describe = cl_abap_typedescr=>describe_by_data( <feld> ).
  IF NOT l_describe->type_kind EQ 'P'.
    ASSIGN <feld>+offs(leng) TO <feld>.
  ENDIF.
  sort = <feld>.


ENDFORM.                               "SORTIERUNG_ASSIGN



*----------------------------------------------------------------------*
* FORM VORZEICHEN_SETZEN                                               *
*----------------------------------------------------------------------*
* H: Vorzeichen des regulierten Betrags umdrehen (bei Zahlungen)       *
* P: Vorzeichen der Betragsfelder abhängig vom Feld REGUP-SHKZG und    *
*    von T042Z-XEINZ setzen                                            *
*----------------------------------------------------------------------*
* H: change sign of payment amount (if outgoing payment);              *
* P: set sign of invoice amounts regarding REGUP-SHKZG and T042Z-XEINZ *
*----------------------------------------------------------------------*
* Parameter SATZ bestimmt, welche Felder zu behandeln sind (H oder P)  *
* parameter SATZ says which fields have to be maintained (H or P)      *
*----------------------------------------------------------------------*
FORM vorzeichen_setzen USING satz.


  IF satz EQ 'H'.                      "REGUH-Felder behandeln
    "maintain REGUH-data
    IF regud-xeinz EQ space.
      reguh-rbetr = - reguh-rbetr.
      reguh-rskon = - reguh-rskon.
      reguh-rwbtr = - reguh-rwbtr.
      reguh-rwskt = - reguh-rwskt.
    ENDIF.


  ELSE.                                "REGUP-Felder behandeln
    "maintain REGUP-data
    IF ( regud-xeinz EQ space AND regup-shkzg EQ 'H' ) OR
       ( regud-xeinz NE space AND regup-shkzg EQ 'S' ).
      regud-dmbtr = regup-dmbtr.
      regud-wrbtr = regup-wrbtr.
      regud-sknto = regup-sknto.
      regud-wskto = regup-wskto.
      regud-qsteu = regup-qbshh.
      regud-wqste = regup-qbshb.
      IF regup-xanet NE space.
        regud-dmbtr = regup-dmbtr + regup-mwsts.
        regud-wrbtr = regup-wrbtr + regup-wmwst.
      ENDIF.
    ELSE.
      regud-dmbtr = - regup-dmbtr.
      regud-wrbtr = - regup-wrbtr.
      regud-sknto = - regup-sknto.
      regud-wskto = - regup-wskto.
      regud-qsteu = - regup-qbshh.
      regud-wqste = - regup-qbshb.
      regup-qsshb = - regup-qsshb.
      IF regup-xanet NE space.
        regud-dmbtr = - regup-dmbtr - regup-mwsts.
        regud-wrbtr = - regup-wrbtr - regup-wmwst.
      ENDIF.
    ENDIF.

  ENDIF.


ENDFORM.                               "VORZEICHEN_SETZEN



*----------------------------------------------------------------------*
* FORM ISOCODE_UMSETZEN                                                *
*----------------------------------------------------------------------*
* Währungsschlüssel in ISO-Code umsetzen                               *
* read ISO code of currency                                            *
*----------------------------------------------------------------------*
* WAERS - umzusetzende Währungsschlüssel                               *
*         currency code to be maintained                               *
* ISOCD - umgesetzter Währungsschlüssel                                *
*         ISO code of currency                                         *
*----------------------------------------------------------------------*
FORM isocode_umsetzen USING waers isocd.

  sy-subrc = 0.
  IF tcurc-waers NE waers.
    CLEAR tcurc.
    SELECT SINGLE * FROM tcurc
      WHERE waers EQ waers.
  ENDIF.
  IF sy-subrc EQ 0 AND tcurc-isocd NE space.
    isocd = tcurc-isocd.
  ELSE.                                "ISO-Code nicht gefunden
    isocd = waers.                     "ISO code not found
    err_tcurc-waers = waers.
    COLLECT err_tcurc.
  ENDIF.


ENDFORM.                               "ISOCODE_UMSETZEN



*----------------------------------------------------------------------*
* FORM BUCHUNGSKREIS_DATEN_LESEN                                       *
*----------------------------------------------------------------------*
* Tabelle T001 lesen                                                   *
* Ausgabefeld für Hauswährung füllen                                   *
* Brieftexte bereitstellen                                             *
*----------------------------------------------------------------------*
* read table T001                                                      *
* fill print field for local currency                                  *
* read names of standard texts                                         *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM buchungskreis_daten_lesen.


* Tabelle T001 lesen ---------------------------------------------------
* read table T001 ------------------------------------------------------
  CLEAR t001.
  SELECT SINGLE * FROM t001
    WHERE bukrs EQ reguh-zbukr.

  hlp_sprache = t001-spras.
  IF hlp_sprache IS INITIAL.
    fimsg-msgv1 = reguh-zbukr.
    PERFORM message USING '242'.
  ENDIF.

* Avisformular lesen ---------------------------------------------------
* read name of remittance advice form ----------------------------------
  CLEAR t042b.
  SELECT SINGLE * FROM t042b
    WHERE zbukr EQ reguh-zbukr.
  IF sy-subrc NE 0.
    IF sy-batch EQ space.
      MESSAGE a345 WITH reguh-zbukr.
    ELSE.
      MESSAGE s345 WITH reguh-zbukr.
      MESSAGE s094.
      STOP.
    ENDIF.
  ENDIF.
  IF hlp_aforn NE space.
*--- user-entered SAPscript form
    t042b-aforn = hlp_aforn.
  ENDIF.

  IF NOT hlp_apdfaf IS INITIAL.
*--- user-entered PDF form
    t042b-pdfaf = hlp_apdfaf.
    CLEAR t042b-aforn.
    CLEAR hlp_formular.
  ENDIF.

  IF  t042b-aforn EQ space
  AND t042b-pdfaf EQ space
  AND par_avis NE space.
    IF sy-batch EQ space.
      MESSAGE a346 WITH reguh-zbukr.
    ELSE.
      MESSAGE s346 WITH reguh-zbukr.
      MESSAGE s094.
      STOP.
    ENDIF.
  ENDIF.

* Ausgabefeld für die Hauswährung füllen -------------------------------
* fill print field for local currency ----------------------------------
  IF par_isoc EQ 'X'.                  "ISO code
    PERFORM isocode_umsetzen USING t001-waers regud-hwaer.
  ELSE.
    regud-hwaer = t001-waers.
  ENDIF.

* Brieftexte bereitstellen ---------------------------------------------
* read names of standard texts -----------------------------------------
  CLEAR t042t.
  SELECT SINGLE * FROM t042t
    WHERE bukrs EQ reguh-zbukr.
  IF sy-subrc NE 0.
    MOVE-CORRESPONDING reguh TO err_t042t.
    COLLECT err_t042t.
  ENDIF.
  MOVE-CORRESPONDING t042t TO regud.
  regud-txtko = t042t-txtko.
  regud-txtfu = t042t-txtfu.
  regud-txtun = t042t-txtun.
  regud-txtab = t042t-txtab.


ENDFORM.                               "BUCHUNGSKREIS_DATEN_LESEN



*----------------------------------------------------------------------*
* FORM ZAHLWEG_DATEN_LESEN                                             *
*----------------------------------------------------------------------*
* Textschlüssel für OCRA-Zeile bereitstellen                           *
* Formulardaten (Name, maximale Postenanzahl, Austeller) lesen         *
*----------------------------------------------------------------------*
* read key in code line                                                *
* read form data (form name, line items per form, issuer)              *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM zahlweg_daten_lesen.


* Textschlüssel für OCRA-Zeile bereitstellen ---------------------------
* read key in code line ------------------------------------------------
  CLEAR tab_t042z.
  tab_t042z-land1 = t001-land1.
  tab_t042z-zlsch = reguh-rzawe.
  READ TABLE tab_t042z.
  t042z = tab_t042z.
  regud-otxsl = t042z-txtsl.

* Formulardaten (Name, maximale Postenanzahl, Austeller) lesen ---------
* read form data (form name, line items per form, issuer) --------------
  CLEAR t042e.
  SELECT SINGLE * FROM t042e
    WHERE zbukr EQ reguh-zbukr
    AND   zlsch EQ reguh-rzawe.
  IF sy-subrc NE 0.
    IF sy-batch EQ space.
      MESSAGE a347 WITH reguh-rzawe reguh-zbukr.
    ELSE.
      MESSAGE s347 WITH reguh-rzawe reguh-zbukr.
      MESSAGE s094.
      STOP.
    ENDIF.
  ENDIF.
  IF NOT t042e-xsavi IS INITIAL.       "see note 365942
    t042e-xavis = 'X'.
  ENDIF.
  IF hlp_zforn NE space.               "Formular überschreiben, wenn als
    t042e-zforn = hlp_zforn.           "Parameter vorgegeben
  ENDIF.                               "overwrite form name if wished
  IF t042e-zforn EQ space AND par_zdru NE space.
    IF sy-batch EQ space.
      MESSAGE a348 WITH reguh-rzawe reguh-zbukr.
    ELSE.
      MESSAGE s348 WITH reguh-rzawe reguh-zbukr.
      MESSAGE s094.
      STOP.
    ENDIF.
  ENDIF.
  IF t042e-wforn EQ space AND flg_zettel EQ 1 AND
    ( par_xdta NE space OR t042z-xswec NE space ).
    IF sy-batch EQ space.
      MESSAGE a349 WITH reguh-rzawe reguh-zbukr.
    ELSE.
      MESSAGE s349 WITH reguh-rzawe reguh-zbukr.
      MESSAGE s094.
      STOP.
    ENDIF.
  ENDIF.
  regud-aust1 = t042e-aust1.
  regud-aust2 = t042e-aust2.
  regud-aust3 = t042e-aust3.
  regud-austo = t042e-austo.

* Sortierdaten lesen
* read sort data
  SELECT SINGLE * FROM t021m INTO hlp_t021m_h
    WHERE progn = 'RFFO*   '
      AND anwnd = 'REGUH'
      AND srvar = t042e-svarh.
  IF sy-subrc NE 0.
    CLEAR hlp_t021m_h.
  ENDIF.
  SELECT SINGLE * FROM t021m INTO hlp_t021m_p
    WHERE progn = 'RFFO*   '
      AND anwnd = 'REGUP'
      AND srvar = t042e-svarp.
  IF sy-subrc NE 0.
    CLEAR hlp_t021m_p.
  ENDIF.


ENDFORM.                               "ZAHLWEG_DATEN_LESEN



*----------------------------------------------------------------------*
* FORM HAUSBANK_DATEN_LESEN                                            *
*----------------------------------------------------------------------*
* Hausbank-Anschriftsdaten lesen                                       *
* Bankleitzahl ohne Aufbereitungszeichen für OCRA-Zeile speichern      *
*----------------------------------------------------------------------*
* read house bank address                                              *
* store numerical bank number                                          *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM hausbank_daten_lesen.


* Hausbank-Anschriftsdaten lesen ---------------------------------------
* read house bank address ----------------------------------------------
  CLEAR bnka.
  SELECT SINGLE * FROM bnka
    WHERE banks EQ reguh-ubnks
    AND   bankl EQ reguh-ubnky.
  regud-ubnka    = bnka-banka.
  regud-ubstr    = bnka-stras.
  regud-ubort    = bnka-ort01.
  regud-ubank    = bnka-banka.
  regud-ubank+61 = bnka-ort01.
  CONDENSE regud-ubank.
  regud-ubrch    = bnka-brnch.

* Bankleitzahl ohne Aufbereitungszeichen für OCRA-Zeile speichern ------
* store numerical bank number ------------------------------------------
  regud-obnkl = reguh-ubnkl.


ENDFORM.                               "HAUSBANK_DATEN_LESEN



*----------------------------------------------------------------------*
* FORM HAUSBANK_KONTO_LESEN                                            *
*----------------------------------------------------------------------*
* Hausbank-Konto lesen                                                 *
*----------------------------------------------------------------------*
* read account at house bank                                           *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM hausbank_konto_lesen.


* Hausbank-Konto lesen
* read account at house bank
  IF   t012k-bukrs NE reguh-zbukr
    OR t012k-hbkid NE reguh-hbkid
    OR t012k-hktid NE reguh-hktid.

    IF    tab_t012k-bukrs EQ reguh-zbukr
      AND tab_t012k-hbkid EQ reguh-hbkid
      AND tab_t012k-hktid EQ reguh-hktid.

      t012k    = tab_t012k.
      sy-subrc = 0.

    ELSE.

      READ TABLE tab_t012k WITH KEY bukrs = reguh-zbukr
                                    hbkid = reguh-hbkid
                                    hktid = reguh-hktid.
      IF sy-subrc = 0.
        t012k = tab_t012k.
      ELSE.
        SELECT SINGLE * FROM t012k
          WHERE bukrs EQ reguh-zbukr
            AND hbkid EQ reguh-hbkid
            AND hktid EQ reguh-hktid.
        IF sy-subrc = 0.
          tab_t012k = t012k.
          APPEND tab_t012k.
        ELSE.
          IF sy-batch EQ space.
            MESSAGE a095 WITH 'T012K' reguh-zbukr reguh-hbkid
                                      reguh-hktid.
          ELSE.
            MESSAGE s095 WITH 'T012K' reguh-zbukr reguh-hbkid
                                      reguh-hktid.
            MESSAGE s094.
            STOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.                               "HAUSBANK_KONTO_LESEN



*----------------------------------------------------------------------*
* FORM FILL_ITCPO                                                      *
*----------------------------------------------------------------------*
* Füllen der Struktur itcpo                                            *
* fill structure itcpo                                                 *
*----------------------------------------------------------------------*
* p_tddest       - Druckername                 name of printer         *
* p_tddataset    - Datasetname                 dataset name            *
* p_tdimmed      - sofort drucken              print immediatly        *
*----------------------------------------------------------------------*
FORM fill_itcpo USING p_tddest     LIKE itcpo-tddest
                      p_tddataset  LIKE itcpo-tddataset
                      p_tdimmed    LIKE itcpo-tdimmed
                      p_tdautority LIKE itcpo-tdautority.

  CLEAR itcpo.
  itcpo-tdpageslct  = space.           "all pages
  itcpo-tdnewid     = 'X'.             "create new spool dataset
  itcpo-tdcopies    = 1.               "one copy
  itcpo-tddest      = p_tddest.        "name of printer
  itcpo-tdpreview   = space.           "no preview
  itcpo-tdcover     = space.           "no cover page
  itcpo-tddataset   = p_tddataset.     "dataset name
  IF zw_xvorl EQ space.
    itcpo-tdsuffix1 = p_tddest.        "name or printer
  ELSE.
    itcpo-tdsuffix1 = 'TEST'.          "test run
  ENDIF.
  itcpo-tdsuffix2   = par_vari.        "name of report variant
  itcpo-tdimmed     = p_tdimmed.       "print immediately?
  itcpo-tddelete    = space.           "do not delete after print
  itcpo-tdtitle     = t042z-text1.     "title of pop-up-window
  itcpo-tdcovtitle  = t042z-text1.     "title of print-cover
  itcpo-tdautority  = p_tdautority.    "print authority
  itcpo-tdarmod     = 1.               "print only

ENDFORM.                               " FILL_ITCPO



*----------------------------------------------------------------------*
* FORM FILL_ITCPO_FROM_ITCPP                                           *
*----------------------------------------------------------------------*
* Füllen der Struktur ITCPO aus der Resultatstruktur ITCPP             *
* fill structure ITCPO from the result structure ITCPP                 *
*----------------------------------------------------------------------*
FORM fill_itcpo_from_itcpp.

  itcpo-tdpageslct  = space.
  itcpo-tdcopies    = itcpp-tdcopies.
  itcpo-tddest      = itcpp-tddest.
  itcpo-tdpreview   = itcpp-tdpreview.
  itcpo-tdcover     = itcpp-tdcover.
  itcpo-tddataset   = itcpp-tddataset.
  itcpo-tdsuffix1   = itcpp-tdsuffix1.
  itcpo-tdsuffix2   = itcpp-tdsuffix2.
  itcpo-tdimmed     = itcpp-tdimmed.
  itcpo-tddelete    = itcpp-tddelete.
  itcpo-tdtitle     = itcpp-tdtitle.
  itcpo-tdcovtitle  = itcpp-tdcovtitle.
  itcpo-tdautority  = itcpp-tdautority.
  itcpo-tdreceiver  = itcpp-tdreceiver.
  itcpo-tddivision  = itcpp-tddivision.
  itcpo-tdlifetime  = itcpp-tdlifetime.
  itcpo-tdarmod     = 1.

ENDFORM.                               " FILL_ITCPO_FROM_ITCPP



*----------------------------------------------------------------------*
* FORM MODIFY_ITCPO                                                    *
*----------------------------------------------------------------------*
* Modify ITCPO and set archive parameters for optical archiving        *
* Routine can only be used for payment medium on paper !!!             *
* Test prints are not allowed in case of optical archiving !!!         *
*----------------------------------------------------------------------*
FORM modify_itcpo.


  DATA up_repid LIKE sy-repid.
  up_repid = sy-repid.
  CLEAR:
    toa_dara,
    arc_params.
  CALL FUNCTION 'OPEN_FI_PERFORM_00002060_P'
    EXPORTING
      i_reguh          = reguh
      i_gjahr          = regud-gjahr
      i_repid          = up_repid
      i_aforn          = t042e-zforn
    CHANGING
      c_itcpo          = itcpo
      c_archive_index  = toa_dara
      c_archive_params = arc_params.
  IF itcpo-tdarmod GT 1 AND par_anzp NE 0.                "#EC PORTABLE
    par_anzp = 0.
    PERFORM message USING '384'.
  ENDIF.


ENDFORM.                               " MODIFY_ITCPO



*----------------------------------------------------------------------*
* FORM PRINT_ON                                                        *
*----------------------------------------------------------------------*
* new-page print on mit passender Parametrisierung aufrufen            *
* call new-page print on with appropiate parametrization               *
*----------------------------------------------------------------------*
* P_BUKRS        - Buchungskreis               company code            *
* P_COVER_TEXT   - Titel des Spoolauftrags     title of spool request  *
* P_DESTINATION  - Ausgabegerät                output device           *
* P_IMMEDIATELY  - Druck sofort?               print immediately?      *
* P_LIST_DATASET - Name des Spool-Datasets     name of spool dataset   *
*----------------------------------------------------------------------*
FORM print_on USING p_bukrs        LIKE t001-bukrs
                    p_cover_text   TYPE any
                    p_destination  LIKE rfpdo-fordprib
                    p_immediately  LIKE tlsep-sofor
                    p_list_dataset LIKE tlsep-listn.


  DATA: BEGIN OF up_param,
          cpage LIKE tlsep-cpage,
          nllid LIKE tlsep-nllid,
          keeps LIKE tlsep-keeps,
          layot LIKE tlsep-layot,
        END OF up_param.

* TLSEP lesen ---------------------------------------------------------*
* read TLSEP  ---------------------------------------------------------*
  CLEAR sy-spono.                                         "#EC WRITE_OK
  SELECT SINGLE * FROM  tlsep
         WHERE  domai       = 'BUKRS'
         AND    werte       = p_bukrs.

  IF sy-subrc = 0.
    MOVE-CORRESPONDING tlsep TO up_param.
    IF up_param-layot IS INITIAL.
      up_param-layot = 'X_65_132'.
    ENDIF.
  ELSE.
    up_param-cpage = ' '.
    up_param-nllid = 'X'.
    up_param-keeps = 'X'.
    up_param-layot = 'X_65_132'.
  ENDIF.

  IF p_immediately EQ 'X'   AND        "immediate printing requested and
     sy-batch EQ space      AND        "NO batch-processing   BUT:
     p_destination EQ space OR         "printer not specified
     par_begl EQ 'D'        AND
     sy-tcode EQ 'FDTA'.

    DATA valid(1) TYPE c.
    DATA pri_params LIKE pri_params.
    DATA arc_params LIKE arc_params.
    CALL FUNCTION 'GET_PRINT_PARAMETERS'
      EXPORTING
        destination            = p_destination
        immediately            = p_immediately
        new_list_id            = up_param-nllid
        no_dialog              = ' '
        list_name              = p_list_dataset
        line_size              = 132
        layout                 = up_param-layot
        sap_cover_page         = up_param-cpage
      IMPORTING
        out_parameters         = pri_params
        out_archive_parameters = arc_params
        valid                  = valid.
    IF valid = 'X'.
      NEW-PAGE PRINT ON PARAMETERS pri_params
                        ARCHIVE PARAMETERS arc_params NO DIALOG.
    ENDIF.
  ELSE.
    IF p_destination = space.
      CALL FUNCTION 'GET_PRINT_PARAMETERS'
        EXPORTING
          no_dialog      = 'X'
        IMPORTING
          out_parameters = pri_params
          valid          = valid.
      p_destination = pri_params-pdest.
    ENDIF.
    NEW-PAGE
      PRINT ON
      LINE-SIZE                132
      LIST NAME                par_vari
      LIST AUTHORITY           hlp_auth
      DESTINATION              p_destination
      COVER TEXT               p_cover_text
      LIST DATASET             p_list_dataset
      IMMEDIATELY              p_immediately
      NEW LIST IDENTIFICATION  up_param-nllid
      KEEP IN SPOOL            up_param-keeps
      LAYOUT                   up_param-layot
      SAP COVER PAGE           up_param-cpage
      NO DIALOG.
  ENDIF.


ENDFORM.                               "PRINT_ON



*----------------------------------------------------------------------*
* FORM PRINT_OFF                                                       *
*----------------------------------------------------------------------*
* new-page print off mit passender Parametrisierung aufrufen           *
* call new-page print off with appropiate parametrization              *
*----------------------------------------------------------------------*
* P_DATASET     - Name des Spool-Datasets      name of spool dataset   *
* P_NAME        - Name der Ausgabeliste        name of output list     *
*----------------------------------------------------------------------*
FORM print_off USING p_dataset  LIKE tab_ausgabe-dataset
                     p_name     TYPE any.


  NEW-PAGE PRINT OFF.
  CHECK sy-spono NE 0.
  CLEAR tab_ausgabe.
  tab_ausgabe-name    = p_name.
  tab_ausgabe-dataset = p_dataset.
  tab_ausgabe-spoolnr = sy-spono.
  COLLECT tab_ausgabe.


ENDFORM.                               "PRINT_OFF



*----------------------------------------------------------------------*
* FORM EMPFBANK_DATEN_LESEN                                            *
*----------------------------------------------------------------------*
* Empfängerbank-Anschriftsdaten lesen                                  *
* read address of payee                                                *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM empfbank_daten_lesen.


* Empfängerbank-Anschriftsdaten lesen ----------------------------------
* read address of payee ------------------------------------------------
  CLEAR bnka.
  SELECT SINGLE * FROM bnka
    WHERE banks EQ reguh-zbnks
    AND   bankl EQ reguh-zbnky.
  regud-zbnka    = bnka-banka.
  regud-zbstr    = bnka-stras.
  regud-zbort    = bnka-ort01.
  regud-zbank    = bnka-banka.
  regud-zbank+61 = bnka-ort01.
  CONDENSE regud-zbank.
  regud-zbrch    = bnka-brnch.

* Bankleitzahl ohne Aufbereitungszeichen für Begleitliste und DTA ------
* store numerical bank number ------------------------------------------
  hlp_zbnkl      = reguh-zbnkl.
  regud-ozbkl    = reguh-zbnkl.


ENDFORM.                               "EMPFBANK_DATEN_LESEN



*----------------------------------------------------------------------*
* FORM ZAHLUNGS_DATEN_LESEN                                            *
*----------------------------------------------------------------------*
* Zahlungsbelegnummer mit führenden Nullen für OCRA-Zeile speichern    *
* Textschlüssel bei HR-Beständen modifizieren                          *
* Ausgabefeld für die Belegwährung füllen                              *
* Ländername des Zahlungsempfängers lesen                              *
* Ausgabefelder für Postleitzahl und Ort füllen                        *
* Buchhaltungssachbearbeiter lesen                                     *
* Absendenden Buchungskreis lesen                                      *
* Datum in Worten                                                      *
*----------------------------------------------------------------------*
* store payment document number with leading zeros for code line       *
* modify key in code line for HR data                                  *
* fill print field for currency                                        *
* read country name of payee                                           *
* read vehicle country key                                             *
* read accounting clerk name                                           *
* read sending company code                                            *
* dates in words                                                       *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM zahlungs_daten_lesen.


* Leeren der Einzelposten-Tabelle - Verwendung: Avis, User-Exit --------
* refresh table with single items - use: payment advice, user-exit -----
  REFRESH tab_regup.
  CLEAR tab_regup.
  PERFORM zahlungs_daten_lesen_hlp.


ENDFORM.                               "ZAHLUNGS_DATEN_LESEN



*----------------------------------------------------------------------*
* FORM ZAHLUNGS_DATEN_LESEN_HLP                                        *
*----------------------------------------------------------------------*
* Hilfsprogamm für das Lesen der Zahlungsdaten                         *
* help program for read of payment data                                *
*----------------------------------------------------------------------*
FORM zahlungs_daten_lesen_hlp.


  STATICS up_sprache LIKE hlp_sprache.

  STATICS: BEGIN OF up_uiban OCCURS 0,
             zbukr LIKE reguh-zbukr,
             hbkid LIKE reguh-hbkid,
             hktid LIKE reguh-hktid,
             uiban LIKE regud-uiban,
           END OF up_uiban.
  DATA:    up_bkref LIKE reguh-bkref.

  DATA BEGIN OF up_t001.
  INCLUDE STRUCTURE t001.
  DATA END OF up_t001.

  DATA: up_plort LIKE szad_field-addr_dc,
        up_pfstr LIKE szad_field-addr_dc.

* Sprache bestimmen, in der das Formular gelesen werden soll
  IF par_espr EQ 'X'.
    hlp_sprache = reguh-zspra.         "Empfängersprache
  ELSE.
    hlp_sprache = t001-spras.          "Buchungskreissprache
  ENDIF.

* Ausgabeformat des Zahlungsempfängers einstellen (Datum, Betrag)
  SET COUNTRY reguh-zland.
  IF sy-subrc NE 0.
    SET COUNTRY space.
  ENDIF.

* Zahlweg für den Formularabschluß
* payment method for summary
  regud-zwels = reguh-rzawe.

* Nummern mit führenden Nullen für OCRA-Zeile speichern ----------------
* store numbers with leading zeros for code line -----------------------
  regud-ovbln = reguh-vblnr.
  regud-ozbkt = reguh-zbnkn.

* IBAN -----------------------------------------------------------------
  IF flg_iban EQ 1.

*   IBAN des Zahlungsempfängers ermitteln
*   determine IBAN of payee
    IF reguh-ziban IS INITIAL.
      CALL FUNCTION 'READ_IBAN_FROM_DB'
        EXPORTING
          i_banks           = reguh-zbnks
          i_bankl           = reguh-zbnky
          i_bankn           = reguh-zbnkn
          i_bkont           = reguh-zbkon
          i_bkref           = reguh-bkref
        IMPORTING
          e_iban            = regud-ziban
          e_iban_valid_from = hlp_date.
      IF hlp_date GT sy-datlo.
        CLEAR regud-ziban.
      ENDIF.
    ELSE.
      regud-ziban = reguh-ziban.  "from external via PAYRQ, e.g. IHC
    ENDIF.

*   IBAN für unserer Hausbankkonto
*   IBAN for our house bank account
    READ TABLE up_uiban WITH KEY zbukr = reguh-zbukr
                                 hbkid = reguh-hbkid
                                 hktid = reguh-hktid.
    IF sy-subrc NE 0.
      CLEAR up_uiban.
      up_uiban-zbukr = reguh-zbukr.
      up_uiban-hbkid = reguh-hbkid.
      up_uiban-hktid = reguh-hktid.
      SELECT SINGLE * FROM t012k WHERE bukrs EQ reguh-zbukr
                                 AND   hbkid EQ reguh-hbkid
                                 AND   hktid EQ reguh-hktid.
      IF sy-subrc EQ 0.
        up_bkref = t012k-refzl.
      ENDIF.
      CALL FUNCTION 'READ_IBAN_FROM_DB'
        EXPORTING
          i_banks           = reguh-ubnks
          i_bankl           = reguh-ubnky
          i_bankn           = reguh-ubknt
          i_bkont           = reguh-ubkon
          i_bkref           = up_bkref
        IMPORTING
          e_iban            = up_uiban-uiban
          e_iban_valid_from = hlp_date.
      IF hlp_date GT sy-datlo.
        CLEAR up_uiban-uiban.
      ENDIF.
      APPEND up_uiban.
    ENDIF.
    regud-uiban = up_uiban-uiban.

  ENDIF.

* Textschlüssel bei HR-Beständen modifizieren --------------------------
* modify key in code line for HR data ----------------------------------
  hrxblnr = regup-xblnr.
  IF hlp_laufk EQ 'P'                  "bei HR-Beständen ist spezieller
    AND t042z-xschk EQ space           "Textschlüssel zu verwenden
    AND hrxblnr-txtsl NE space.        "(falls gefüllt, leer aus PU11)
    t042z-txtsl = hrxblnr-txtsl.       "use special text key for HR data
  ENDIF.
  regud-otxsl = t042z-txtsl.

* Ausgabefeld für die Belegwährung füllen ------------------------------
* fill print field for currency ----------------------------------------
  IF par_isoc EQ 'X'.                  "ISO Code
    PERFORM isocode_umsetzen USING reguh-waers regud-waers.
  ELSE.
    regud-waers = reguh-waers.
  ENDIF.

* Ländername des Zahlungsempfängers lesen ------------------------------
* read country name of payee -------------------------------------------
  CLEAR t005t.
  SELECT SINGLE * FROM t005t
    WHERE spras EQ hlp_sprache
    AND   land1 EQ reguh-land1.
  regud-landx = t005t-landx.

  IF reguh-land1 NE reguh-zland.
    CLEAR t005t.
    SELECT SINGLE * FROM t005t
      WHERE spras EQ hlp_sprache
      AND   land1 EQ reguh-zland.
  ENDIF.
  regud-zlndx = t005t-landx.

* Bezeichnung der Region lesen -----------------------------------------
* read name of region --------------------------------------------------
  CLEAR t005u.
  SELECT SINGLE * FROM t005u
    WHERE spras EQ hlp_sprache
    AND   land1 EQ reguh-zland
    AND   bland EQ reguh-zregi.
  regud-zregx = t005u-bezei.

* Ausgabefelder für Postleitzahl und Ort füllen ------------------------
* fill print field for postal code and city ----------------------------
  IF reguh-name1 NE space.
    IF reguh-adrnr IS INITIAL.
      CLEAR adrs.
      adrs-name1 = reguh-name1.
      adrs-stras = reguh-stras.
      adrs-pfach = reguh-pfach.
      adrs-pstl2 = reguh-pstl2.
      adrs-land1 = reguh-land1.
      adrs-pstlz = reguh-pstlz.
      adrs-ort01 = reguh-ort01.
      adrs-regio = reguh-regio.
      adrs-inlnd = t001-land1.
      adrs-anzzl = '4'.
      CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
        EXPORTING
          adrswa_in  = adrs
        IMPORTING
          adrswa_out = adrs.
      regud-plort = adrs-lined.
      regud-pfstr = adrs-lined0.
    ELSE.
      CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
        EXPORTING
          address_type           = '1'
          sender_country         = t001-land1
          address_number         = reguh-adrnr
          number_of_lines        = '4'
        IMPORTING
          address_data_carrier   = up_plort
          address_data_carrier_0 = up_pfstr.
      regud-plort = up_plort.
      regud-pfstr = up_pfstr.
    ENDIF.
  ELSE.
    regud-plort = space.
    regud-pfstr = space.
  ENDIF.

  IF reguh-zadnr IS INITIAL.
    CLEAR adrs.
    adrs-name1 = reguh-znme1.
    adrs-stras = reguh-zstra.
    adrs-pfach = reguh-zpfac.
    adrs-pstl2 = reguh-zpst2.
    adrs-pfort = reguh-zpfor.
    adrs-land1 = reguh-zland.
    adrs-pstlz = reguh-zpstl.
    adrs-ort01 = reguh-zort1.
    adrs-ort02 = reguh-zort2.
    adrs-regio = reguh-zregi.
    adrs-inlnd = t001-land1.
    adrs-anzzl = '4'.
    CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
      EXPORTING
        adrswa_in  = adrs
      IMPORTING
        adrswa_out = adrs.
    regud-zplor = adrs-lined.
    regud-zpfst = adrs-lined0.
  ELSE.
    CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
      EXPORTING
        address_type           = '1'
        sender_country         = t001-land1
        address_number         = reguh-zadnr
        number_of_lines        = '4'
      IMPORTING
        address_data_carrier   = up_plort
        address_data_carrier_0 = up_pfstr.
    regud-zplor = up_plort.
    regud-zpfst = up_pfstr.
  ENDIF.

* Name mit Schutzsternen -----------------------------------------------
* name with protective asterisks ---------------------------------------
  CLEAR regud-znm1s.
  TRANSLATE regud-znm1s USING ' *'.
  sy-fdpos = strlen( reguh-znme1 ).
  IF sy-fdpos GT 0.
    regud-znm1s(sy-fdpos) = reguh-znme1.
  ENDIF.
  CLEAR regud-znm2s.
  TRANSLATE regud-znm2s USING ' *'.
  sy-fdpos = strlen( reguh-znme2 ).
  IF sy-fdpos GT 0.
    regud-znm2s(sy-fdpos) = reguh-znme2.
  ENDIF.

* Buchhaltungssachbearbeiter lesen -------------------------------------
* read accounting clerk data -------------------------------------------
  IF t001s-bukrs NE reguh-absbu OR t001s-busab NE reguh-busab
                                OR hlp_sprache NE up_sprache.
    up_sprache = hlp_sprache.
    CLEAR: fsabe, t001s, regud-ubusa.
    CALL FUNCTION 'CORRESPONDENCE_DATA_BUSAB'
      EXPORTING
        i_bukrs = reguh-absbu
        i_busab = reguh-busab
        i_langu = hlp_sprache
      IMPORTING
        e_t001s = t001s
        e_fsabe = fsabe
      EXCEPTIONS
        OTHERS  = 01.
    IF sy-subrc EQ 0.
      PERFORM sachbearbeiter_kurzinfo USING fsabe regud-ubusa.
    ENDIF.
    IF regud-ubusa EQ space.
      regud-ubusa = t001s-sname.
    ENDIF.
  ENDIF.

* Absendenden Buchungskreis lesen --------------------------------------
* read sending company code --------------------------------------------
  regud-abstx = space.
  regud-absor = space.
  IF reguh-absbu NE reguh-zbukr.
    SELECT SINGLE * FROM t001 INTO up_t001
      WHERE bukrs EQ reguh-absbu.
    regud-abstx = up_t001-butxt.
    regud-absor = up_t001-ort01.
  ENDIF.

* Buchungsdatum des Zahlungsbelegs (in Worten) -------------------------
* posting date of payment document (in words) --------------------------
  SELECT SINGLE * FROM t015m
    WHERE spras EQ hlp_sprache
    AND   monum EQ reguh-zaldt+4(2).
  IF sy-subrc EQ 0.
    regud-zaliw(2)   = reguh-zaldt+6(2).
    regud-zaliw+2(2) = '. '.
    regud-zaliw+4    = t015m-monam.
  ELSE.
    CLEAR regud-zaliw.
  ENDIF.

* Wechselausstellungsdatum (in Worten) ---------------------------------
* bill of exchange issue date (in words) -------------------------------
  SELECT SINGLE * FROM t015m
    WHERE spras EQ hlp_sprache
    AND   monum EQ reguh-wdate+4(2).
  IF sy-subrc EQ 0.
    regud-wdaiw(2)   = reguh-wdate+6(2).
    regud-wdaiw+2(2) = '. '.
    regud-wdaiw+4    = t015m-monam.
  ELSE.
    CLEAR regud-wdaiw.
  ENDIF.

* Wechselfälligkeitsdatum (in Worten) ----------------------------------
* due date of the bill of exchange (in words) --------------------------
  SELECT SINGLE * FROM t015m
    WHERE spras EQ hlp_sprache
    AND   monum EQ reguh-wefae+4(2).
  IF sy-subrc EQ 0.
    regud-wefiw(2)   = reguh-wefae+6(2).
    regud-wefiw+2(2) = '. '.
    regud-wefiw+4    = t015m-monam.
  ELSE.
    CLEAR regud-wefiw.
  ENDIF.


ENDFORM.                               "ZAHLUNGS_DATEN_LESEN_HLP



*----------------------------------------------------------------------*
* FORM SACHBEARBEITER_KURZINFO                                         *
*----------------------------------------------------------------------*
* Aus den Daten des Sachbearbeiters (Struktur FSABE, gelesen mit       *
* Baustein CORRESPONDENCE_DATA_BUSAB) wird ein Textfeld mit einer      *
* Kurzinfo gefüllt (Anrede, Name, Telefonnummer). Sollte das Feld      *
* zu kurz sein, wird zuerst die Anrede, dann der hintere Teil des      *
* Namens unterdrückt.                                                  *
*----------------------------------------------------------------------*
* XFSABE   - Sachbearbeiterdaten                                       *
* TEXTFELD - Kurzinfo                                                  *
*----------------------------------------------------------------------*
FORM sachbearbeiter_kurzinfo USING xfsabe STRUCTURE fsabe textfeld.


  DATA:
    up_actln     LIKE sy-fdpos,           "actually calculated string length
    up_maxln     LIKE sy-fdpos,            "maximal string length (textfeld)
    up_lname     LIKE fsabe-lname,
    up_salut     LIKE fsabe-salut,
    up_telf1(40) TYPE c.

  DESCRIBE FIELD textfeld LENGTH up_maxln IN CHARACTER MODE.
  up_lname     = xfsabe-lname.
  up_salut     = xfsabe-salut.
  CONCATENATE xfsabe-telf1 xfsabe-tel_exten1 INTO up_telf1.

  CONDENSE up_telf1 NO-GAPS.
  up_actln     = strlen( up_salut ) +
                 strlen( up_lname ) +
                 strlen( up_telf1 ) + 2.
  IF up_actln GT up_maxln.
    up_actln   = up_actln - strlen( up_salut ) - 1.
    CLEAR up_salut.
    IF up_actln GT up_maxln.
      up_actln = strlen( up_lname ) - up_actln + up_maxln.
      IF up_actln GT 0.
        up_lname+up_actln = space.
      ELSE.
        CLEAR up_lname.
      ENDIF.
    ENDIF.
  ENDIF.
  txt_zeile    = up_salut.
  txt_zeile+16 = up_lname.
  txt_zeile+52 = up_telf1.
  CONDENSE txt_zeile.
  textfeld     = txt_zeile.


ENDFORM.                               "SACHBEARBEITER_KURZINFO



*----------------------------------------------------------------------*
* FORM HR_REMITTANCE_ACKNOWLEDGEMENT                                   *
*----------------------------------------------------------------------*
* Zahlungen an Dritte (Third Party Remittance) an das HR zurückmelden  *
* report payments concerning 3rd parties back to HR                    *
*----------------------------------------------------------------------*
* No USING - parameters                                                *
*----------------------------------------------------------------------*
FORM hr_remittance_acknowledgement.
  DATA: l_remsn_exist TYPE xfeld.   "Indicator REMSN exists
  DATA: l_iv_duedt_exist TYPE xfeld. "Indicator IV_DUEDT exists
  DATA: l_remsn TYPE remsn.

* Prüfen ob Rückmeldung notwendig
* check that payment is 3rd party remittance
  hrxblnr = regup-xblnr.
  CHECK:
    zw_xvorl      EQ space,
    zw_laufi+5(1) NE 'P',
    hrxblnr-txtsl EQ 'HR',
    hrxblnr-txerg NE space AND hrxblnr-xhrfo EQ 'X' OR
    hrxblnr-txerg EQ space AND hrxblnr-xhrfo EQ space.
*    hrxblnr-remsn NE 0.
*    with extension of remsn from 5 to 10,
*    hrxblnr-remsn will always be zeros for the new posting numbers.
*    The data prior to the extension will not have zeros in the field.
*    So, removed the check.

  SELECT SINGLE * FROM bkpf WHERE bukrs EQ regup-bukrs
                            AND   belnr EQ regup-belnr
                            AND   gjahr EQ regup-gjahr.
  IF bkpf-awsys EQ t000-logsys OR bkpf-awsys IS INITIAL.

    DATA: l_import_tab     TYPE STANDARD TABLE OF rsimp WITH HEADER LINE,
          l_export_tab     TYPE STANDARD TABLE OF rsexp,
          l_tables_tab     TYPE STANDARD TABLE OF rstbl,
          l_exceptions_tab TYPE STANDARD TABLE OF rsexc,
          l_function(30).

    l_function = 'RP_REMITTANCE_ACKNOWLEDGEMENT'.

    CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
      EXPORTING
        funcname           = 'RP_REMITTANCE_ACKNOWLEDGEMENT'
        inactive_version   = ' '
      TABLES
        exception_list     = l_exceptions_tab
        export_parameter   = l_export_tab
        import_parameter   = l_import_tab
        tables_parameter   = l_tables_tab
      EXCEPTIONS
        error_message      = 1
        function_not_found = 2
        invalid_name       = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      READ TABLE l_import_tab WITH KEY parameter = 'IV_DUEDT'.
      IF sy-subrc = 0.
        l_iv_duedt_exist = 'X'.
      ENDIF.

      READ TABLE l_import_tab WITH KEY parameter = 'REMSN'.
      IF sy-subrc = 0.
        l_remsn_exist = 'X'.
      ENDIF.

      IF l_iv_duedt_exist = 'X' AND l_remsn_exist = 'X'.
        IF regup-sgtxt+1(5) EQ '00000'. "means new posting run number
          l_remsn = regup-sgtxt+6(10).
        ELSE.
          l_remsn = regup-sgtxt+1(5).
        ENDIF.

        CALL FUNCTION l_function
          EXPORTING
            laufd    = regup-laufd
            laufi    = regup-laufi
            bukrs    = regup-bukrs
            lifnr    = regup-lifnr
            xblnr    = regup-xblnr
            iv_duedt = regup-zfbdt
            remsn    = l_remsn
          EXCEPTIONS
            OTHERS   = 4.
        CLEAR tab_rfc-dest.
      ENDIF. "if both fields exist.

      IF l_iv_duedt_exist IS INITIAL AND l_remsn_exist IS INITIAL.
        CALL FUNCTION l_function
          EXPORTING
            laufd  = regup-laufd
            laufi  = regup-laufi
            bukrs  = regup-bukrs
            lifnr  = regup-lifnr
            xblnr  = regup-xblnr
          EXCEPTIONS
            OTHERS = 0.
        CLEAR tab_rfc-dest.
      ENDIF.  "neither fields exist.

      IF l_iv_duedt_exist = 'X' AND l_remsn_exist IS INITIAL.
        CALL FUNCTION l_function
          EXPORTING
            laufd    = regup-laufd
            laufi    = regup-laufi
            bukrs    = regup-bukrs
            lifnr    = regup-lifnr
            xblnr    = regup-xblnr
            iv_duedt = regup-zfbdt
          EXCEPTIONS
            OTHERS   = 4.
        CLEAR tab_rfc-dest.
      ENDIF. "if iv_duedt exists but remsn does not

      IF l_remsn_exist = 'X' AND l_iv_duedt_exist IS INITIAL.
        IF regup-sgtxt+1(5) EQ '00000'. "means new posting run number
          l_remsn = regup-sgtxt+6(10).
        ELSE.
          l_remsn = regup-sgtxt+1(5).
        ENDIF.

        CALL FUNCTION l_function
          EXPORTING
            laufd  = regup-laufd
            laufi  = regup-laufi
            bukrs  = regup-bukrs
            lifnr  = regup-lifnr
            xblnr  = regup-xblnr
            remsn  = l_remsn
          EXCEPTIONS
            OTHERS = 4.
        CLEAR tab_rfc-dest.
      ENDIF. "if remsn exists and iv_duedt does not

*  store error message from HR
      IF sy-subrc NE 0.
        fimsg-msgv1   = sy-msgv1.
        fimsg-msgv2   = sy-msgv2.
        fimsg-msgv3   = sy-msgv3.
        fimsg-msgv4   = sy-msgv4.
        fimsg-msgid   = sy-msgid.
        PERFORM message USING sy-msgno.
      ENDIF.
      CLEAR tab_rfc-dest.
    ENDIF.
  ELSE.
    CALL FUNCTION 'LOG_SYSTEM_GET_RFC_DESTINATION'
      EXPORTING
        logical_system  = bkpf-awsys
      IMPORTING
        rfc_destination = tab_rfc-dest
      EXCEPTIONS
        OTHERS          = 4.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'A' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      tab_rfc-bukrs = regup-bukrs.
      tab_rfc-belnr = regup-belnr.
      tab_rfc-gjahr = regup-gjahr.
      tab_rfc-lifnr = regup-lifnr.
      tab_rfc-xblnr = regup-xblnr.
      tab_rfc-zbukr = reguh-zbukr.
      tab_rfc-vblnr = reguh-vblnr.
      tab_rfc-zfbdt = regup-zfbdt.
      tab_rfc-sgtxt = regup-sgtxt.
      tab_rfc-awsys = bkpf-awsys.
      APPEND tab_rfc.
    ENDIF.
  ENDIF.

* store text for later determination of remittance from PCL4
* via RP_IMPORT_GARNISHMENT_LIST (see below in HR_FORMULAR_LESEN)
  tab_grn-zbukr = reguh-zbukr.
  tab_grn-vblnr = reguh-vblnr.
  tab_grn-dest  = tab_rfc-dest.
  tab_grn-sgtxt = regup-sgtxt.
  COLLECT tab_grn.


ENDFORM.                               "HR_REMITTANCE_ACKNOWLEDGEMENT



*----------------------------------------------------------------------*
* FORM HR_REMITTANCE_ACKNOWLEDGE_RFC                                   *
*----------------------------------------------------------------------*
* Zahlungen an Dritte (Third Party Remittance) via RFC zurückmelden    *
* report payments concerning 3rd parties back to HR (RFC)              *
*----------------------------------------------------------------------*
* No USING - parameters                                                *
*----------------------------------------------------------------------*
FORM hr_remittance_acknowledge_rfc.

  LOOP AT tab_rfc.

    DATA: l_import_tab     TYPE STANDARD TABLE OF rsimp WITH HEADER LINE,
          l_export_tab     TYPE STANDARD TABLE OF rsexp,
          l_tables_tab     TYPE STANDARD TABLE OF rstbl,
          l_exceptions_tab TYPE STANDARD TABLE OF rsexc,
          l_function(30).
    DATA: l_remsn_exist TYPE xfeld.   "Indicator REMSN exists
    DATA: l_iv_duedt_exist TYPE xfeld. "Indicator IV_DUEDT exists
    DATA: l_remsn TYPE remsn.
    l_function = 'RP_REMITTANCE_ACKNOWLEDGEMENT'.

    CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
      DESTINATION
      tab_rfc-dest
      EXPORTING
        funcname           = 'RP_REMITTANCE_ACKNOWLEDGEMENT'
        inactive_version   = ' '
      TABLES
        exception_list     = l_exceptions_tab
        export_parameter   = l_export_tab
        import_parameter   = l_import_tab
        tables_parameter   = l_tables_tab
      EXCEPTIONS
        error_message      = 1
        function_not_found = 2
        invalid_name       = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
*** Func found so check its import parameters.
      READ TABLE l_import_tab WITH KEY parameter = 'IV_DUEDT'.
      IF sy-subrc = 0.
        l_iv_duedt_exist = 'X'.
      ENDIF.

      READ TABLE l_import_tab WITH KEY parameter = 'REMSN'.
      IF sy-subrc = 0.
        l_remsn_exist = 'X'.
      ENDIF.

      IF l_iv_duedt_exist = 'X' AND l_remsn_exist = 'X'.
        IF tab_rfc-sgtxt+1(5) EQ '00000'. "means new posting run number
          l_remsn = tab_rfc-sgtxt+6(10).
        ELSE.
          l_remsn = tab_rfc-sgtxt+1(5).
        ENDIF.

        CALL FUNCTION l_function
          DESTINATION
          tab_rfc-dest
          EXPORTING
            laufd                 = zw_laufd
            laufi                 = zw_laufi
            bukrs                 = tab_rfc-bukrs
            lifnr                 = tab_rfc-lifnr
            xblnr                 = tab_rfc-xblnr
            iv_duedt              = tab_rfc-zfbdt
            remsn                 = l_remsn
          EXCEPTIONS
            communication_failure = 4 MESSAGE txt_zeile
            system_failure        = 4 MESSAGE txt_zeile
            OTHERS                = 8.
      ENDIF. "if remsn and iv_duedt both exist

      IF l_iv_duedt_exist IS INITIAL AND l_remsn_exist IS INITIAL.
        CALL FUNCTION l_function
          DESTINATION
          tab_rfc-dest
          EXPORTING
            laufd                 = zw_laufd
            laufi                 = zw_laufi
            bukrs                 = tab_rfc-bukrs
            lifnr                 = tab_rfc-lifnr
            xblnr                 = tab_rfc-xblnr
          EXCEPTIONS
            communication_failure = 4 MESSAGE txt_zeile
            system_failure        = 4 MESSAGE txt_zeile
            OTHERS                = 8.
      ENDIF. "neither exists

      IF l_iv_duedt_exist = 'X' AND l_remsn_exist IS INITIAL.
        CALL FUNCTION l_function
          DESTINATION
          tab_rfc-dest
          EXPORTING
            laufd                 = zw_laufd
            laufi                 = zw_laufi
            bukrs                 = tab_rfc-bukrs
            lifnr                 = tab_rfc-lifnr
            xblnr                 = tab_rfc-xblnr
            iv_duedt              = tab_rfc-zfbdt
          EXCEPTIONS
            communication_failure = 4 MESSAGE txt_zeile
            system_failure        = 4 MESSAGE txt_zeile
            OTHERS                = 8.
      ENDIF. "if iv_duedt exists but remsn does not

      IF l_remsn_exist = 'X' AND l_iv_duedt_exist IS INITIAL.
        IF tab_rfc-sgtxt+1(5) EQ '00000'. "means new posting run number
          l_remsn = tab_rfc-sgtxt+6(10).
        ELSE.
          l_remsn = tab_rfc-sgtxt+1(5).
        ENDIF.
        CALL FUNCTION l_function
          DESTINATION
          tab_rfc-dest
          EXPORTING
            laufd                 = zw_laufd
            laufi                 = zw_laufi
            bukrs                 = tab_rfc-bukrs
            lifnr                 = tab_rfc-lifnr
            xblnr                 = tab_rfc-xblnr
            remsn                 = l_remsn
          EXCEPTIONS
            communication_failure = 4 MESSAGE txt_zeile
            system_failure        = 4 MESSAGE txt_zeile
            OTHERS                = 8.
      ENDIF. "if remsn exists but iv_duedt does not

      IF sy-subrc NE 0.
*     RFC error header
        fimsg-msgv1   = tab_rfc-awsys.
        fimsg-msgv2   = tab_rfc-dest.
        fimsg-msgv3   = tab_rfc-bukrs.
        fimsg-msgv4   = tab_rfc-belnr.
        PERFORM message USING 378.
      ENDIF.
      IF sy-subrc EQ 4.
*     communication or system error
        fimsg-msgv1   = txt_zeile.
        fimsg-msgv2   = txt_zeile+50.
        fimsg-msgv3   = txt_zeile+100.
        PERFORM message USING 379.
        fimsg-msgv1   = tab_rfc-zbukr.
        fimsg-msgv2   = tab_rfc-vblnr.
        PERFORM message USING 380.
        REJECT.
      ENDIF.
      IF sy-subrc EQ 8.
*     store error message from HR
        fimsg-msgv1   = sy-msgv1.
        fimsg-msgv2   = sy-msgv2.
        fimsg-msgv3   = sy-msgv3.
        fimsg-msgv4   = sy-msgv4.
        fimsg-msgid   = sy-msgid.
        PERFORM message USING sy-msgno.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                               "HR_REMITTANCE_ACKNOWLEDGE_RFC



*----------------------------------------------------------------------*
* FORM HR_FORMULAR_LESEN                                               *
*----------------------------------------------------------------------*
* HR-Formular besorgen und Steuerungszeilen entfernen                  *
* Bei Pfändungen (HR GRN) zusätzlich die Textfelder in REGUD füllen    *
* read HR form and delete command lines                                *
* in addition fill text fields in REGUD when payment is garnishment    *
*----------------------------------------------------------------------*
* No USING - parameters                                                *
*----------------------------------------------------------------------*
FORM hr_formular_lesen.


  FIELD-SYMBOLS:
    <feld>.
  DATA:
    up_nr(1)    TYPE n,
    up_feld(11) TYPE c,
    up_pform1   LIKE pc408 OCCURS 9 WITH HEADER LINE,
    up_xpform   LIKE pc408 OCCURS 9 WITH HEADER LINE,
    up_xpform1  LIKE pc408 OCCURS 9 WITH HEADER LINE.

  REFRESH: pform, up_pform1.
  CLEAR sy-msgno.

  IF hrxblnr-txtsl EQ 'HR' AND hrxblnr-txerg EQ 'GRN'.
    LOOP AT tab_grn WHERE zbukr EQ reguh-zbukr
                      AND vblnr EQ reguh-vblnr.

      "fetch remittance information for each item (SGTXT)
      "in tables UP_XPFORM and combine the result in PFORM
      "fetch remittance field information of first item in
      "UP_XPFORM1, store it in UP_PFORM1 and fill REGUD-TEXT1..9
      "(i.e. only single payments support the field information!)
      REFRESH: up_xpform, up_xpform1.
      IF tab_grn-sgtxt CN '* '.
        WHILE tab_grn-sgtxt(1) CA '* '.
          SHIFT tab_grn-sgtxt.
        ENDWHILE.
      ENDIF.
      IF tab_grn-dest IS INITIAL.
        CALL FUNCTION 'RP_IMPORT_GARNISHMENT_LIST'
          EXPORTING
            sgtxt  = tab_grn-sgtxt
          TABLES
            pform  = up_xpform
            pform1 = up_xpform1
          EXCEPTIONS
            OTHERS = 8.
      ELSE.
        CALL FUNCTION 'RP_IMPORT_GARNISHMENT_LIST'
          DESTINATION
          tab_grn-dest
          EXPORTING
            sgtxt                 = tab_grn-sgtxt
          TABLES
            pform                 = up_xpform
            pform1                = up_xpform1
          EXCEPTIONS
            communication_failure = 4 MESSAGE txt_zeile
            system_failure        = 4 MESSAGE txt_zeile
            OTHERS                = 8.
        IF sy-subrc EQ 4.
          sy-msgid = 'F0'.
          sy-msgno = 379.
          sy-msgv1 = txt_zeile.
          sy-msgv2 = txt_zeile+50.
          sy-msgv3 = txt_zeile+100.
          sy-msgv4 = space.
        ENDIF.
      ENDIF.
      APPEND LINES OF up_xpform TO pform.       "garnishment form (text)
      IF up_pform1[] IS INITIAL.
        APPEND LINES OF up_xpform1 TO up_pform1."field values of first
      ENDIF.                                    "garnishment only
    ENDLOOP.
  ELSE.
    CALL FUNCTION 'RP_IMPORT_PAY_STATEMENT'
      EXPORTING
        laufd  = reguh-laufd
        laufi  = reguh-laufi
        pernr  = reguh-pernr
        seqnr  = reguh-seqnr
      TABLES
        pform  = pform
      EXCEPTIONS
        OTHERS = 8.
  ENDIF.
  IF sy-subrc NE 0.
    IF NOT sy-msgno IS INITIAL.
      fimsg-msgid = sy-msgid.
      fimsg-msgv1 = sy-msgv1.
      fimsg-msgv2 = sy-msgv2.
      fimsg-msgv3 = sy-msgv3.
      fimsg-msgv4 = sy-msgv4.
      PERFORM message USING sy-msgno.
    ENDIF.
    fimsg-msgv1 = reguh-zbukr.
    fimsg-msgv2 = reguh-hbkid.
    fimsg-msgv3 = reguh-hktid.
    IF NOT regud-chect IS INITIAL.
      fimsg-msgv4 = regud-chect.
    ELSE.
      fimsg-msgv4 = regud-chect.
    ENDIF.
    PERFORM message USING '286'.
  ELSE.
    LOOP AT pform WHERE ltype NE f__ltype-txt.
      DELETE pform.
    ENDLOOP.
    IF hrxblnr-txtsl EQ 'HR' AND hrxblnr-txerg EQ 'GRN'.
      LOOP AT up_pform1 WHERE ltype NE f__ltype-txt.
        DELETE up_pform1.
      ENDLOOP.
      DO 9 TIMES.
        up_nr         = sy-index.
        up_feld       = 'REGUD-TEXT '.
        up_feld+10(1) = up_nr.
        ASSIGN (up_feld) TO <feld>.
        CLEAR <feld>.
        READ TABLE up_pform1 INDEX sy-index.
        IF sy-subrc EQ 0.
          <feld> = up_pform1-linda.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDIF.


ENDFORM.                               "HR_FORMULAR_LESEN


*----------------------------------------------------------------------*
* FORM WEISUNGSSCHLUESSEL_LESEN                                        *
*----------------------------------------------------------------------*
* Lesen des aktuellen Weisungsschlüssels zu den REGUH-Daten            *
* Read instruction key for current REGUH-contents                      *
* Release 3.0: key fields are country of bank and payment method       *
*----------------------------------------------------------------------*
* No USING - parameters                                                *
*----------------------------------------------------------------------*
FORM weisungsschluessel_lesen.

  DATA: up_dtaws LIKE reguh-dtaws.
  STATICS: up_t015w LIKE t015w OCCURS 0 WITH HEADER LINE.

  CLEAR t015w.                         "Clear old values

  IF NOT reguh-dtaws IS INITIAL.
    up_dtaws = reguh-dtaws.
  ELSE.
    IF reguh-zbukr NE t012d-bukrs OR reguh-hbkid NE t012d-hbkid.
      SELECT SINGLE * FROM t012d WHERE bukrs EQ reguh-zbukr
                                 AND   hbkid EQ reguh-hbkid.
      IF sy-subrc NE 0.
        CLEAR t012d.
      ENDIF.
    ENDIF.
    IF NOT t012d-dtaws IS INITIAL.
      up_dtaws = t012d-dtaws.
    ELSE.
      MOVE-CORRESPONDING reguh TO err_t012d.
      COLLECT err_t012d.
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE up_t015w WITH KEY banks = reguh-ubnks
                               zlsch = reguh-rzawe
                               dtaws = up_dtaws
                          INTO t015w.
  CHECK sy-subrc NE 0.
  SELECT SINGLE * FROM t015w
          WHERE banks EQ reguh-ubnks
            AND zlsch EQ reguh-rzawe
            AND dtaws EQ up_dtaws.
  IF sy-subrc NE 0.                    "specified entry not found
    SELECT SINGLE * FROM t015w
            WHERE banks EQ space
              AND zlsch EQ space
              AND dtaws EQ up_dtaws.
    IF sy-subrc NE 0.                  "general (=old) entry not found
      err_kein_dtaws-banks = reguh-ubnks.
      err_kein_dtaws-zlsch = reguh-rzawe.
      err_kein_dtaws-dtaws = up_dtaws.
      COLLECT err_kein_dtaws.          "Store error
    ELSE.
      t015w-banks = reguh-ubnks.
      t015w-zlsch = reguh-rzawe.
      APPEND t015w TO up_t015w.
    ENDIF.
  ELSE.
    APPEND t015w TO up_t015w.
  ENDIF.

ENDFORM.                               "WEISUNGSSCHLUESSEL_LESEN


*----------------------------------------------------------------------*
* Form  WEISUNGSSCHLUESSEL_UMSETZEN                                    *
*----------------------------------------------------------------------*
* Weisungsschlüssel in Schlüsselwort u. Zusatzinfo umsetzen            *
* transpose instruction key into keyword and additional information    *
*----------------------------------------------------------------------*
*  P_DTWS. - Verwender, Feld, Weisung (wird evtl. überschrieben)       *
*            country, field, instruction (may be overwritten)          *
*  P_TEXT1 - Schlüsselwort                                             *
*            code word                                                 *
*  P_TEXT2 - Zusatzinformation                                         *
*          - additional information                                    *
*----------------------------------------------------------------------*
FORM weisungsschluessel_umsetzen USING p_dtwsc LIKE t015w1-dtwsc
                                       p_dtwsf LIKE t015w1-dtwsf
                                       p_dtwsx LIKE t015w1-dtwsx
                                       p_text1 TYPE any
                                       p_text2 TYPE any.
  DATA up_i015w1_par LIKE i015w1_par.

  MOVE-CORRESPONDING reguh TO up_i015w1_par.

  CALL FUNCTION 'FI_PAYMENT_INSTRUCTION_CONVERT'
    EXPORTING
      i_dtwsc      = p_dtwsc
      i_dtwsf      = p_dtwsf
      i_i015w1_par = up_i015w1_par
      i_bnka       = bnka
      i_dtzus      = t015w-dtzus
    IMPORTING
      e_code       = p_text1
      e_addinfo    = p_text2
    CHANGING
      c_dtwsx      = p_dtwsx.

ENDFORM.                               " WEISUNGSSCHLUESSEL_UMSETZEN


*----------------------------------------------------------------------*
* Form  GET_CLEARING_CODE
*----------------------------------------------------------------------*
* Clearing Code ermitteln                                              *
* determine clearing code                                              *
*----------------------------------------------------------------------*
*  P_LAND  - Länderschlüssel der Bank des Zahlungsempfängers           *
*            country key of the bank of payee
*  P_SOCO  - Bankleitzahl
*            sort code
*  P_CLCO  - clearing code
*----------------------------------------------------------------------*
FORM get_clearing_code USING p_land   LIKE reguh-zbnks
                             p_soco   LIKE reguh-zbnkl
                             p_clco   TYPE c.

  PERFORM laender_lesen USING p_land.

  CALL FUNCTION 'GET_BANKCODE'
    EXPORTING
      i_banks  = p_land
      i_bankl  = p_soco
    IMPORTING
      e_clcode = p_clco
    EXCEPTIONS
      OTHERS   = 4.

ENDFORM.                               " GET_CLEARING_CODE


*----------------------------------------------------------------------*
* FORM ADRESSE_LESEN                                                   *
*----------------------------------------------------------------------*
* Lesen einer Customizingadresse (z.B. Buchungskreisadresse)           *
* Read customizing address (e.g. address of company code)              *
*----------------------------------------------------------------------*
* ADRNR - address number                                               *
*----------------------------------------------------------------------*
FORM adresse_lesen USING VALUE(adrnr).

  PERFORM addr_get USING 'CA01' adrnr.

ENDFORM.                               " ADRESSE_LESEN


*----------------------------------------------------------------------*
* FORM BANKADRESSE_LESEN                                               *
*----------------------------------------------------------------------*
* Lesen einer Bankadresse                                              *
* Read bank address                                                    *
*----------------------------------------------------------------------*
* ADRNR - address number                                               *
*----------------------------------------------------------------------*
FORM bankadresse_lesen USING VALUE(adrnr).

  PERFORM addr_get USING 'CA02' adrnr.

ENDFORM.                               " BANKADRESSE_LESEN


*----------------------------------------------------------------------*
* FORM GET_ADDR                                                        *
*----------------------------------------------------------------------*
* Lesen einer Adresse                                                  *
* Read address                                                         *
*----------------------------------------------------------------------*
* ADRGR - address group                                                *
* ADRNR - address number                                               *
*----------------------------------------------------------------------*
FORM addr_get USING adrgr adrnr.

  CHECK adrnr NE sadr-adrnr.
  CLEAR addr1_sel.
  addr1_sel-addrnumber = adrnr.
  CALL FUNCTION 'ADDR_GET'
    EXPORTING
      address_selection = addr1_sel
      address_group     = adrgr
    IMPORTING
      address_value     = addr1_val
      sadr              = sadr
    EXCEPTIONS
      OTHERS            = 4.                                "SADR40A
  IF sy-subrc NE 0.
    CLEAR sadr.
  ENDIF.

ENDFORM.                               " GET_ADDR


*----------------------------------------------------------------------*
* FORM LAENDER_LESEN                                                   *
*----------------------------------------------------------------------*
* Lesen der Länderdaten zum Land LAND1                                 *
* Read country data for LAND1                                          *
*----------------------------------------------------------------------*
* LAND1 - countrycode                                                  *
*----------------------------------------------------------------------*
FORM laender_lesen USING VALUE(land1).

  CLEAR sy-subrc.

  IF tab_t005-land1 = land1.           "check last value
    t005 = tab_t005.
  ELSE.
    READ TABLE tab_t005 WITH KEY land1 = land1."check internal table
    IF sy-subrc EQ 0.                  "entry was found
      t005 = tab_t005.
    ELSE.
      SELECT SINGLE * FROM t005
             WHERE land1 = land1.
      IF sy-subrc NE 0.                "no entry found in T005
        CLEAR t005.
      ELSE.                            "entry found-> store temporarily
        tab_t005 = t005.               "fill header, too!
        APPEND tab_t005.
      ENDIF.
    ENDIF.
  ENDIF.

  IF t005-intca IS INITIAL.
    err_t005-land1 = land1.
    COLLECT err_t005.
    sy-subrc = 4.
  ENDIF.

ENDFORM.                               "LAENDER_LESEN


*----------------------------------------------------------------------*
* FORM ZAHLWEG_EINFUEGEN                                               *
*----------------------------------------------------------------------*
* Übergebenen Zahlweg in die übergebene Leiste übernehmen              *
* insert payment method into array of methods                          *
*----------------------------------------------------------------------*
* RZAWE  - payment method to insert                                    *
* LIST   - current list of payment methods                             *
*----------------------------------------------------------------------*
FORM zahlweg_einfuegen USING VALUE(rzawe) list.

  DATA up_list LIKE regud-zwels.

  CHECK list NA rzawe.
  up_list = list.
  SHIFT up_list.
  up_list+9(1) = rzawe.
  list = up_list.

ENDFORM.                               "ZAHLWEG_EINFUEGEN


*----------------------------------------------------------------------*
* FORM SUMMENFELDER_INITIALISIEREN                                     *
*----------------------------------------------------------------------*
* Summenfelder initialisieren                                          *
* initialize total amount fields                                       *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM summenfelder_initialisieren.


* Summenfelder initialisieren ------------------------------------------
* initialize total amount fields ---------------------------------------
  regud-sdmbt = 0.
  regud-swrbt = 0.
  regud-ssknt = 0.
  regud-swskt = 0.
  regud-sqste = 0.
  regud-swqst = 0.
  regud-sskfb = 0.
  regud-sqssh = 0.

* Nettosummenfelder mit Schutzstern für das Anschreiben vorab belegen --
* fill net total fields with protective asterisks ----------------------
  WRITE:
    reguh-rbetr TO regud-snets CURRENCY t001-waers,  "#EC CI_FLDEXT_OK[2610650]
    reguh-rwbtr TO regud-swnes CURRENCY reguh-waers. "#EC CI_FLDEXT_OK[2610650]
  TRANSLATE:
    regud-snets USING ' *',
    regud-swnes USING ' *'.
  PERFORM ziffern_in_worten.


ENDFORM.                               "SUMMENFELDER_INITIALISIEREN



*----------------------------------------------------------------------*
* FORM EINZELPOSTENFELDER_FUELLEN                                      *
*----------------------------------------------------------------------*
* Ausgabefelder füllen                                                 *
* fill single item fields                                              *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM einzelpostenfelder_fuellen.

* Segmenttext ohne * aufbereiten ---------------------------------------
* segment text without leading * ---------------------------------------
  IF regup-sgtxt CN '* '.
    WHILE regup-sgtxt(1) CA '* '.
      SHIFT regup-sgtxt.
    ENDWHILE.
  ENDIF.

* Einzelposteninfo merken - Verwendung: Avis, User-Exit
* store item-information  - use: payment advice, user-exit
  IF tab_regup NE regup.               "nicht im Loop über TAB_REGUP
    tab_regup = regup.                 "not in LOOP AT TAB_REGUP
    APPEND tab_regup.
  ENDIF.

* Text zum Buchungsschlüssel lesen
* read text of posting key
  SELECT SINGLE * FROM tbslt
    WHERE spras EQ hlp_sprache
      AND bschl EQ regup-bschl
      AND umskz EQ regup-umskz.
  regud-bschx = tbslt-ltext.

* Betragsfelder (Abzüge und Netto) füllen ------------------------------
* fill single item amount fields (deductions and net) ------------------
  PERFORM vorzeichen_setzen USING 'P'.
  regud-abzug = regud-sknto + regud-qsteu.
  regud-wabzg = regud-wskto + regud-wqste.
  regud-netto = regud-dmbtr - regud-abzug.
  regud-wnett = regud-wrbtr - regud-wabzg.
  WRITE:
    regud-netto TO regud-netts CURRENCY t001-waers,  "#EC CI_FLDEXT_OK[2610650]
    regud-wnett TO regud-wnets CURRENCY reguh-waers. "#EC CI_FLDEXT_OK[2610650]
  TRANSLATE:
    regud-netts USING ' *',
    regud-wnets USING ' *'.


ENDFORM.                               "EINZELPOSTENFELDER_FUELLEN



*----------------------------------------------------------------------*
* FORM SUMMENFELDER_FUELLEN                                            *
*----------------------------------------------------------------------*
* Summenfelder hochzählen                                              *
* Ausgabefelder füllen                                                 *
*----------------------------------------------------------------------*
* add up total amount fields                                           *
* fill print fields                                                    *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM summenfelder_fuellen.


  ADD regud-dmbtr TO regud-sdmbt.
  ADD regud-wrbtr TO regud-swrbt.
  ADD regud-sknto TO regud-ssknt.
  ADD regup-skfbt TO regud-sskfb.
  ADD regud-wskto TO regud-swskt.
  ADD regud-qsteu TO regud-sqste.
  ADD regud-wqste TO regud-swqst.
  ADD regup-qsshb TO regud-sqssh.
  regud-sabzg = regud-ssknt + regud-sqste.
  regud-swabz = regud-swskt + regud-swqst.
  regud-snett = regud-sdmbt - regud-sabzg.
  regud-swnet = regud-swrbt - regud-swabz.
  WRITE:
    regud-snett TO regud-snets CURRENCY t001-waers,  "#EC CI_FLDEXT_OK[2610650]
    regud-swnet TO regud-swnes CURRENCY reguh-waers.  "#EC CI_FLDEXT_OK[2610650]
  TRANSLATE:
    regud-snets USING ' *',
    regud-swnes USING ' *'.


ENDFORM.                               "SUMMENFELDER_FUELLEN



*----------------------------------------------------------------------*
* FORM DTA_GLOBALS_ERSETZEN                                            *
*----------------------------------------------------------------------*
* Ersetzt die Globals in Verwendungszweckzeilen im DTA                 *
* replace globals in the file extension fields (DME)                   *
*----------------------------------------------------------------------*
* TEXTFELD - Feld, in dem Globals ersetzt werden sollen                *
*            Field with globals to be replaced                         *
*----------------------------------------------------------------------*
FORM dta_globals_ersetzen USING textfeld.

  DATA: up_textfeld(512) TYPE c.
  up_textfeld = textfeld.

  WRITE regup-bldat TO hlp_datum DD/MM/YY.
  IF up_textfeld CS '&XBLNR'.
    sy-fdpos = strlen( regup-xblnr ).
    IF sy-fdpos GE 14.
      WRITE regup-bldat TO hlp_datum DDMMYY.
    ENDIF.
  ENDIF.
  REPLACE   '&BLDAT' WITH hlp_datum   INTO up_textfeld.
  REPLACE   '&EIKTO' WITH reguh-eikto INTO up_textfeld.
  REPLACE   '&GJAHR' WITH regud-gjahr INTO up_textfeld.
  IF reguh-lifnr NE space.
    REPLACE '&KTNRA' WITH reguh-lifnr INTO up_textfeld.
  ELSE.
    REPLACE '&KTNRA' WITH reguh-kunnr INTO up_textfeld.
  ENDIF.
  WRITE regud-netto TO hlp_betrag CURRENCY regud-hwaer.  "#EC CI_FLDEXT_OK[2610650]
  REPLACE   '&NETTO' WITH hlp_betrag  INTO up_textfeld.
  REPLACE   '&PERNR' WITH reguh-pernr INTO up_textfeld.
  WRITE regup-zbdxp TO hlp_betrag CURRENCY '3'.
  WRITE '%' TO hlp_betrag+15.
  REPLACE   '&PSATZ' WITH hlp_betrag  INTO up_textfeld.
  REPLACE   '&SEQNR' WITH reguh-seqnr INTO up_textfeld.
  REPLACE   '&SGTXT' WITH regup-sgtxt INTO up_textfeld.
  IF hlp_laufk EQ 'M'.
    DATA up_opbel(12) TYPE c.
    up_opbel(2) = reguh-seqnr(2).
    up_opbel+2  = reguh-vblnr.
    REPLACE '&VBLNR' WITH up_opbel    INTO up_textfeld.
  ELSE.
    REPLACE '&VBLNR' WITH reguh-vblnr INTO up_textfeld.
  ENDIF.
  REPLACE   '&VERTN' WITH regup-vertn INTO up_textfeld.
  WRITE regud-wrbtr  TO hlp_betrag CURRENCY regud-waers. "#EC CI_FLDEXT_OK[2610650]
  REPLACE   '&WBRUT' WITH hlp_betrag  INTO up_textfeld.
  WRITE regud-wnett TO hlp_betrag CURRENCY regud-waers.  "#EC CI_FLDEXT_OK[2610650]
  REPLACE   '&WNETT' WITH hlp_betrag  INTO up_textfeld.
  REPLACE   '&WAERS' WITH reguh-waers INTO up_textfeld.
  REPLACE   '&BELNR' WITH regup-belnr INTO up_textfeld.
  REPLACE   '&XBLNR' WITH regup-xblnr INTO up_textfeld.
  WRITE reguh-zaldt TO hlp_datum DD/MM/YY.
  REPLACE   '&ZALDT' WITH hlp_datum   INTO up_textfeld.
  REPLACE   '&ZBUKR' WITH reguh-zbukr INTO up_textfeld.
  CONDENSE up_textfeld.
  PERFORM dta_text_aufbereiten USING up_textfeld.
  textfeld = up_textfeld.


ENDFORM.                               "DTA_GLOBALS_ERSETZEN



*----------------------------------------------------------------------*
* FORM DTA_TEXT_AUFBEREITEN                                            *
*----------------------------------------------------------------------*
* Textfelder im DTA müssen Upper Case und ohne Umlaute sein            *
* text fields in DME have to be upper case and without 'äöü' etc.      *
*----------------------------------------------------------------------*
* TEXTFELD - enthält den zu bearbeitenden Text                         *
*            text that is to be checked                                *
*----------------------------------------------------------------------*
FORM dta_text_aufbereiten USING textfeld.

  IF flg_no_replace EQ space.
    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext  = textfeld
      IMPORTING
        outtext = textfeld
      EXCEPTIONS
        OTHERS  = 01.
  ENDIF.
  TRANSLATE textfeld TO UPPER CASE.

ENDFORM.                               "DTA_TEXT_AUFBEREITEN


*----------------------------------------------------------------------*
* FORM DATEN_SICHERN                                                   *
*----------------------------------------------------------------------*
* Sichern der REGUD-,REGUH-, REGUP-Informationen                       *
* Datenbankfelder für Probedruck belegen                               *
*----------------------------------------------------------------------*
* save REGUD-,REGUH-, REGUP-information during test print              *
* fill all fields with XXXXX                                           *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM daten_sichern.


  sic_fsabe     = fsabe.
  sic_itcpo     = itcpo.
  sic_regud     = regud.
  sic_reguh     = reguh.
  sic_regup     = regup.
  fsabe         = xxx_fsabe.
  regud         = xxx_regud.
  reguh         = xxx_reguh.
  regup         = xxx_regup.
  spell         = xxx_spell.
  itcpo-tdarmod = 1.
  reguh-rzawe   = sic_reguh-rzawe.     "Zahlweg erhalten
  regud-txtko   = sic_regud-txtko.     "Textbausteine sollen auch beim
  regud-txtfu   = sic_regud-txtfu.     "Probedruck erscheinen
  regud-txtun   = sic_regud-txtun.     "payment method and text includes
  regud-txtab   = sic_regud-txtab.     "are valid during test print
  regud-chect   = sic_regud-chect.     "Scheckinformation
  regud-stapt   = sic_regud-stapt.     "check information


ENDFORM.                               "DATEN_SICHERN



*----------------------------------------------------------------------*
* FORM DATEN_ZURUECK                                                   *
*----------------------------------------------------------------------*
* Zurückladen der REGUD-,REGUH-, REGUP-Informationen                   *
* data back after test print                                           *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM daten_zurueck.


  itcpo = sic_itcpo.
  MOVE-CORRESPONDING:
    sic_fsabe TO fsabe,
    sic_regud TO regud,
    sic_reguh TO reguh,
    sic_regup TO regup.
  CLEAR spell.


ENDFORM.                               "DATEN_ZURUECK



*----------------------------------------------------------------------*
* FORM ZIFFERN_IN_WORTEN                                               *
*----------------------------------------------------------------------*
* Umsetzten des Betrages und der Ziffern in Worte                      *
* transform numbers and digits in words                                *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM ziffern_in_worten.


  CLEAR spell.
  CALL FUNCTION 'SPELL_AMOUNT'
    EXPORTING
      language  = hlp_sprache
      currency  = reguh-waers
      amount    = regud-swnes
      filler    = hlp_filler
    IMPORTING
      in_words  = spell
    EXCEPTIONS
      not_found = 1
      too_large = 2.

  IF sy-subrc EQ 1.
*   in Tabelle T015Z fehlt ein Eintrag
*   entry in table T015Z not found
    CLEAR err_t015z.
    err_t015z-spras = sy-msgv1.
    err_t015z-einh  = sy-msgv2.
    err_t015z-ziff  = sy-msgv3.
    COLLECT err_t015z.

    IF hlp_sprache NE 'E'.
*     Letzter Versuch mit Sprache 'E' (besser als nichts)
*     Last trial with language 'E' (better than nothing)
      CALL FUNCTION 'SPELL_AMOUNT'
        EXPORTING
          language = 'E'
          currency = reguh-waers
          amount   = regud-swnes
          filler   = hlp_filler
        IMPORTING
          in_words = spell
        EXCEPTIONS
          OTHERS   = 1.
    ENDIF.
  ENDIF.

  IF sy-subrc EQ 2 OR spell-number GE hlp_maxbetrag.
*   Betrag ist zum Umsetzen zu groß
*   amount too large for transformation
    MOVE-CORRESPONDING reguh TO err_in_worten.
    COLLECT err_in_worten.
    CLEAR:
      spell-dig01, spell-dig02, spell-dig03, spell-dig04, spell-dig05,
      spell-dig06, spell-dig07, spell-dig08, spell-dig09, spell-dig10,
      spell-dig11, spell-dig12, spell-dig13, spell-dig14, spell-dig15.
  ENDIF.


ENDFORM.                               "ZIFFERN_IN_WORTEN



*----------------------------------------------------------------------*
* FORM DATUM_IN_DDMMYY                                                 *
*----------------------------------------------------------------------*
* Konvertierung des Datumfelds DATUM in das Format DDMMYY              *
* Diese Konvertierung geschieht unabhängig von den Benutzerfestwerten. *
*----------------------------------------------------------------------*
* Convert date-field (ccyymmdd) to the format ddmmyy.                  *
* This conversion does not take the user defaults into consideration.  *
*----------------------------------------------------------------------*
FORM datum_in_ddmmyy USING datum    TYPE d
                           ddmmyy TYPE any.


  DATA: up_str(6).

  up_str   = datum+6.                                       "Day
  up_str+2 = datum+4.                  "Month
  up_str+4 = datum+2.                  "Year
  ddmmyy = up_str.


ENDFORM.                               "DATUM_IN_DDMMYY



*----------------------------------------------------------------------*
* FORM ABBRUCH_DURCH_UEBERLAUF                                         *
*----------------------------------------------------------------------*
* Abbruch der Verarbeitung, da es zu unerlaubten Überlauf des Main-    *
* fensters kam. Grund: Die Positionen pro Formular sind in T042E       *
* zu groß definiert.                                                   *
*----------------------------------------------------------------------*
* abend of program because of an overflow of the main window,          *
* reason: number of line items per form are too large in T042E         *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM abbruch_durch_ueberlauf.


  IF sy-batch EQ space.
    MESSAGE a090 WITH reguh-rzawe reguh-zbukr.
  ELSE.
    MESSAGE s090 WITH reguh-rzawe reguh-zbukr.
    MESSAGE s091 WITH reguh-rzawe reguh-zbukr.
    MESSAGE s092 WITH reguh-rzawe reguh-zbukr.
    MESSAGE s093 WITH reguh-rzawe reguh-zbukr.
    PERFORM information.
    MESSAGE s094.
    STOP.
  ENDIF.


ENDFORM.                               "ABBRUCH_DURCH_UEBERLAUF



*----------------------------------------------------------------------*
* FORM FEHLERMELDUNGEN                                                 *
*----------------------------------------------------------------------*
* Ausgabe von Fehlermeldungen                                          *
* error messages                                                       *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM fehlermeldungen.


* Notlösung: Form Fehlermeldung ist die einzige gemeinsame Unterroutine
* aller RFFO-Programme zum Zeitpunkt End-Of-Selection. Daher wird hier
* der Baustein RP_REMITTANCE_ACKNOWLEDGEMENT gerufen, da zum Get-Zeit-
* punkt ein Laufzeitfehler wegen des RFC auftritt.
  PERFORM hr_remittance_acknowledge_rfc.

  SET LANGUAGE sy-langu.
  CLEAR fimsg.

* Fehlende Berechtigungen ----------------------------------------------
* Authority check errors -----------------------------------------------

  PERFORM berechtigung.

*  PERFORM GET_ERROR_LIST(SAPDBPYF) TABLES ERR_AUTH.
*  SORT ERR_AUTH.
*  LOOP AT ERR_AUTH.
*
*    AT FIRST.
*      ADD 1 TO CNT_ERROR.
*    ENDAT.
*
*    CASE ERR_AUTH-FIELD.
*      WHEN 'BRGRU'.
*        FIMSG-MSGNO   = '224'.
*        IF ERR_AUTH-AUTOB EQ 'F_KNA1_BED'.
*          FIMSG-MSGV1 = 'D'.
*        ELSE.
*          FIMSG-MSGV1 = 'K'.
*        ENDIF.
*      WHEN 'BUKRS'.
*        FIMSG-MSGNO   = '153'.
*        FIMSG-MSGV1   = ERR_AUTH-ACTVT.
*      WHEN 'KOART'.
*        FIMSG-MSGNO   = '154'.
*        FIMSG-MSGV1   = ERR_AUTH-ACTVT.
*    ENDCASE.
*    FIMSG-MSGV2       = ERR_AUTH-VALUE.
*    FIMSG-MSGV3       = ERR_AUTH-ACTVT.
*    FIMSG-MSGV4       = ERR_AUTH-AUTOB.
*    IF SY-BATCH EQ SPACE.
*      MESSAGE ID 'F0' TYPE 'I' NUMBER FIMSG-MSGNO
*        WITH FIMSG-MSGV1 FIMSG-MSGV2 FIMSG-MSGV3 FIMSG-MSGV4.
*    ENDIF.
*    PERFORM MESSAGE USING FIMSG-MSGNO.
*
*  ENDLOOP.


* nicht zulässige Zahlwege aus T042Z -----------------------------------
* not valid payment methods from T042Z ---------------------------------
  SORT err_t042z.
  LOOP AT err_t042z.

    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    AT NEW land1.
      CLEAR hlp_zwels.
    ENDAT.

    WRITE err_t042z-zlsch TO hlp_zwels+sy-tabix.

    AT END OF land1.
      CONDENSE hlp_zwels NO-GAPS.
      IF sy-batch EQ space.
        MESSAGE i282 WITH hlp_zwels err_t042z-land1 sy-repid.
      ENDIF.
      fimsg-msgv1 = hlp_zwels.
      fimsg-msgv2 = err_t042z-land1.
      fimsg-msgv3 = sy-repid.
      PERFORM message USING '282'.
    ENDAT.

  ENDLOOP.


* nichts selektiert ----------------------------------------------------
* no data selected -----------------------------------------------------
  IF flg_selektiert EQ 0.
    MESSAGE s073 WITH syst-repid.
  ENDIF.


* nicht gefundene Elemente und Fenster ---------------------------------
* elements and windows not found ---------------------------------------
  SORT err_element.
  LOOP AT err_element.

    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    IF err_element-elemt NE space.
      fimsg-msgv1 = err_element-fname.
      fimsg-msgv2 = err_element-fenst.
      fimsg-msgv3 = err_element-elemt.
      fimsg-msgv4 = err_element-text.
      PERFORM message USING '251'.
    ELSE.
      fimsg-msgv1 = err_element-fname.
      fimsg-msgv2 = err_element-fenst.
      PERFORM message USING '252'.
    ENDIF.

    AT LAST.
      PERFORM message USING '253'.
    ENDAT.

  ENDLOOP.


* nicht gedruckte Fremdwährungsschecks ---------------------------------
* checks in foreign currencies not printed -----------------------------
  SORT err_fw_scheck.
  LOOP AT err_fw_scheck.

    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    AT NEW fname.
      fimsg-msgv1 = err_fw_scheck-fname.
      PERFORM message USING '254'.
      fimsg-msgv1 = err_fw_scheck-fname.
      PERFORM message USING '255'.
      PERFORM message USING '256'.
    ENDAT.

    fimsg-msgv1 = err_fw_scheck-zbukr.
    fimsg-msgv2 = err_fw_scheck-vblnr.
    PERFORM message USING '257'.

  ENDLOOP.


* nicht in Worte umgesetzter Betrag ------------------------------------
* amount in words was not possible -------------------------------------
  SORT err_in_worten.
  LOOP AT err_in_worten.

    AT FIRST.
      ADD 1 TO cnt_error.
      PERFORM message USING '258'.
      PERFORM message USING '259'.
      PERFORM message USING '256'.
    ENDAT.

    fimsg-msgv1 = err_in_worten-zbukr.
    fimsg-msgv2 = err_in_worten-vblnr.
    PERFORM message USING '257'.

    AT LAST.
      PERFORM message USING '260'.
    ENDAT.

  ENDLOOP.


* nicht auf DTA ausgegebene Auslandsüberweisungen ----------------------
* foreign payment via DME not possible ---------------------------------
  SORT err_kein_dta.
  LOOP AT err_kein_dta.

    AT NEW error.
      ADD 1 TO cnt_error.
      fimsg-msgv1 = err_kein_dta-error.
      CASE err_kein_dta-error.
        WHEN 1.
          PERFORM message USING '261'.
        WHEN 2.
          PERFORM message USING '262'.
        WHEN 3.
          PERFORM message USING '263'.
        WHEN 4.
          PERFORM message USING '264'.
        WHEN 5.
          PERFORM message USING '265'.
        WHEN 6.
          PERFORM message USING '266'.
        WHEN 7.
          PERFORM message USING '267'.
        WHEN 8.
          PERFORM message USING '268'.
        WHEN 9.
          PERFORM message USING '269'.
        WHEN 10.
          PERFORM message USING '270'.
      ENDCASE.
      fimsg-msgv1 = err_kein_dta-error.
      PERFORM message USING '271'.
      PERFORM message USING '256'.
    ENDAT.

    fimsg-msgv1 = err_kein_dta-zbukr.
    fimsg-msgv2 = err_kein_dta-vblnr.
    PERFORM message USING '257'.

  ENDLOOP.


* nicht gefundene Weisungsschlüssel ------------------------------------
* missing instruction key ----------------------------------------------
  SORT err_kein_dtaws.
  LOOP AT err_kein_dtaws.

    AT FIRST.
      ADD 1 TO cnt_error.
      fimsg-msgv1 = err_kein_dtaws-banks.
      fimsg-msgv2 = err_kein_dtaws-zlsch.
      fimsg-msgv3 = err_kein_dtaws-dtaws.
      PERFORM message USING '291'.
    ENDAT.

    fimsg-msgv1 = err_kein_dtaws-banks.
    fimsg-msgv2 = err_kein_dtaws-zlsch.
    fimsg-msgv3 = err_kein_dtaws-dtaws.
    PERFORM message USING '292'.

    AT LAST.
      PERFORM message USING '293'.
    ENDAT.

  ENDLOOP.


* nicht verbuchte Belege (und daher nicht gedruckte Formulare) ---------
* payment documents not updated (therefore no form printed) ------------
  SORT err_nicht_verbucht.
  LOOP AT err_nicht_verbucht.

    AT FIRST.
      ADD 1 TO cnt_error.
      PERFORM message USING '272'.
      PERFORM message USING '273'.
      PERFORM message USING '256'.
    ENDAT.

    fimsg-msgv1   = err_nicht_verbucht-zbukr.
    IF err_nicht_verbucht-pyord EQ space.
      fimsg-msgv2 = err_nicht_verbucht-vblnr.
    ELSE.
      fimsg-msgv2 = err_nicht_verbucht-pyord.
    ENDIF.
    PERFORM message USING '257'.

    AT LAST.
      PERFORM message USING '274'.
    ENDAT.

  ENDLOOP.


* EDI Versendefehler ---------------------------------------------------
* error in EDI ---------------------------------------------------------
  SORT err_edi.
  READ TABLE err_edi WITH KEY edibn = 'E'.
  IF sy-subrc EQ 0.

    ADD 1 TO cnt_error.
    PERFORM message USING '357'.
    PERFORM message USING '358'.

    LOOP AT err_edi WHERE edibn EQ 'E'.
      fimsg-msgv1 = err_edi-zbukr.
      fimsg-msgv2 = err_edi-vblnr.
      fimsg-msgv3 = err_edi-rzawe.
      PERFORM message USING '257'.
    ENDLOOP.

    PERFORM message USING '359'.

  ENDIF.


* zuerst mit EDI versuchen ---------------------------------------------
* try EDI first --------------------------------------------------------
  READ TABLE err_edi WITH KEY edibn = 'X'.
  IF sy-subrc EQ 0.

    ADD 1 TO cnt_error.
    PERFORM message USING '360'.
    PERFORM message USING '256'.

    LOOP AT err_edi WHERE edibn EQ 'X'.
      fimsg-msgv1 = err_edi-zbukr.
      fimsg-msgv2 = err_edi-vblnr.
      PERFORM message USING '257'.
    ENDLOOP.

  ENDIF.


* Einträge in T005 falsch oder unvollständig ---------------------------
* T005-entries missing or not correct ----------------------------------
  SORT err_t005.
  LOOP AT err_t005.

    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    fimsg-msgv1 = err_t005-land1.
    PERFORM message USING '241'.

    AT LAST.
      PERFORM message USING '276'.
    ENDAT.

  ENDLOOP.


* nicht gefundene Einträge in T012D ------------------------------------
* entries not found in T012D -------------------------------------------
  SORT err_t012d.
  LOOP AT err_t012d.

    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    fimsg-msgv1 = err_t012d-zbukr.
    fimsg-msgv2 = err_t012d-hbkid.
    PERFORM message USING '275'.

    AT LAST.
      PERFORM message USING '276'.
    ENDAT.

  ENDLOOP.


* nicht gefundene Einträge in T015Z ------------------------------------
* entries not found in T015Z -------------------------------------------
  SORT err_t015z.
  LOOP AT err_t015z.

    AT FIRST.
      ADD 1 TO cnt_error.
      PERFORM message USING '277'.
    ENDAT.

    fimsg-msgv1 = err_t015z-spras.
    fimsg-msgv2 = err_t015z-einh.
    fimsg-msgv3 = err_t015z-ziff.
    PERFORM message USING '257'.

    AT LAST.
      PERFORM message USING '278'.
    ENDAT.

  ENDLOOP.


* fehlerhafte Einträge in T042E ----------------------------------------
* wrong entries in T042E -----------------------------------------------
  SORT err_t042e.
  LOOP AT err_t042e.

    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    fimsg-msgv1 = err_t042e-rzawe.
    fimsg-msgv2 = err_t042e-zbukr.
    PERFORM message USING '283'.

  ENDLOOP.


* nicht gefundene Einträge in T042T ------------------------------------
* entries not found in T042T -------------------------------------------
  SORT err_t042t.
  LOOP AT err_t042t.

    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    fimsg-msgv1 = err_t042t-zbukr.
    PERFORM message USING '279'.

    AT LAST.
      PERFORM message USING '280'.
    ENDAT.

  ENDLOOP.


* nicht in ISO-Code umgesetzte Währungsschlüssel -----------------------
* ISO code for currency not found --------------------------------------
  SORT err_tcurc.
  LOOP AT err_tcurc.

    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    fimsg-msgv1 = err_tcurc-waers.
    PERFORM message USING '281'.

  ENDLOOP.


* Zahlungen mit zu großen Beträgen--------------------------------------
* payments with too high amounts----------------------------------------
  SORT err_betrag.
  LOOP AT err_betrag.
    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    fimsg-msgv1 = err_betrag-waers.
    fimsg-msgv2 = err_betrag-rwbtr.
    fimsg-msgv3 = err_betrag-zbukr.
    fimsg-msgv4 = err_betrag-vblnr.
    PERFORM message USING '382'.
  ENDLOOP.


* Ausgabe des Fehlerprotokolls -----------------------------------------
* Output of error log --------------------------------------------------
  CALL FUNCTION 'FI_MESSAGE_CHECK'
    EXCEPTIONS
      no_message = 4.
  CHECK sy-subrc EQ 0.
  IF sy-batch NE space.
    CALL FUNCTION 'FI_MESSAGE_GET'
      TABLES
        t_fimsg = tab_fimsg.
    LOOP AT tab_fimsg.
      AT NEW msort.
        MESSAGE s257 WITH space space space space.
      ENDAT.
      fimsg = tab_fimsg.
      MESSAGE ID fimsg-msgid  TYPE 'S'     NUMBER fimsg-msgno
         WITH fimsg-msgv1     fimsg-msgv2  fimsg-msgv3  fimsg-msgv4.
    ENDLOOP.
  ELSEIF flg_selektiert EQ 0.
    ADD 1 TO cnt_error.
    fimsg-msgv1 = syst-repid.
    PERFORM message USING '073'.
  ENDIF.

  CLEAR hlp_auth.      " fehlermeldungen ohne Berechtigungsschutz
  PERFORM print_on USING ' ' text_003 par_prib par_sofb 'LISTFS'.

  FORMAT COLOR 6 INTENSIFIED.
  WRITE text_003.
  FORMAT RESET.
  SKIP 2.
  CALL FUNCTION 'FI_MESSAGE_PRINT'
    EXPORTING
      i_xskip = 'X'.

  PERFORM print_off USING 'LISTFS' text_003.
  IF sy-spono NE 0.
    tab_ausgabe-error = 'X'.
    MODIFY tab_ausgabe INDEX sy-tabix.
  ENDIF.

ENDFORM.                               "FEHLERMELDUNGEN



*----------------------------------------------------------------------*
* FORM MESSAGE                                                         *
*----------------------------------------------------------------------*
* Sammeln der Messages, die im Protokoll ausgegeben werden sollen.     *
* Vor Aufruf sind die FIMSG-MSGVn zu füllen, falls sie benutzt werden. *
* Es muß die ID übergeben werden, wenn sie von F0 abweicht.            *
* Wird kein Sortierkriterium übergeben, so wird CNT_ERROR benutzt.     *
*----------------------------------------------------------------------*
* MSGNO - Messagenummer                                                *
*----------------------------------------------------------------------*
FORM message USING msgno.


  IF fimsg-msort EQ space.
    fimsg-msort = cnt_error.
  ENDIF.
  IF fimsg-msgid EQ space.
    fimsg-msgid = 'F0'.
  ENDIF.
  fimsg-msgno   = msgno.
  fimsg-msgty   = 'S'.
  CALL FUNCTION 'FI_MESSAGE_COLLECT'
    EXPORTING
      i_fimsg = fimsg.
  CLEAR fimsg.


ENDFORM.                               "MESSAGE



*----------------------------------------------------------------------*
* FORM INFORMATION                                                     *
*----------------------------------------------------------------------*
* Ausgabe von Informationen über die erzeugten Spoolnummern            *
* information about generated spool datasets                           *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM information.

  "Bei bedingtem Loop über interne
  DATA:                                "Tabellen wird der Zeitpunkt AT
    up_titel(1) TYPE n VALUE 0,        "FIRST nicht durchlaufen!
    up_uline(1) TYPE n VALUE 0,        "LOOP durchlaufen ?
    up_rqident  LIKE tsp01-rqident.

  SORT tab_ausgabe.

* Erster Loop für Sofortdruck
  LOOP AT tab_ausgabe WHERE immed NE space.
    CHECK tab_ausgabe-spoolnr NE 0.
    up_rqident = tab_ausgabe-spoolnr.
    CALL FUNCTION 'RSPO_OUTPUT_SPOOL_REQUEST'
      EXPORTING
        spool_request_id = up_rqident
      EXCEPTIONS
        OTHERS           = 0.
  ENDLOOP.

  IMPORT flg_local FROM MEMORY ID 'MFCHKFN0'.
  CHECK:                               "keine Info bei Sofortdruck
    par_sofo EQ space,                 "und bei Neudruck eines Schecks
    sy-subrc NE 0.                     "no info when print immediately
  "and when re-print of a check
  IF sy-batch EQ space.
    NEW-PAGE NO-TITLE LINE-SIZE 81.
  ENDIF.

* Zweiter Loop zur Ausgabe der Files
  CLEAR tab_ausgabe.
  LOOP AT tab_ausgabe WHERE renum NE space.
    up_uline = 1.
    IF up_titel EQ 0.
      IF sy-batch EQ space.
        FORMAT COLOR 1 INTENSIFIED.
        flg_local = space.
        WRITE:
            / sy-uline.
        HIDE flg_local.
        WRITE:
            / sy-vline NO-GAP,
         (79) text_092,
           81 sy-vline.
        HIDE flg_local.
        WRITE:
            / sy-uline.
        HIDE flg_local.
        FORMAT COLOR 1 INTENSIFIED OFF.
        IF sy-langu <> 'J'
         AND sy-langu <> '1' AND sy-langu <> '2' AND sy-langu <> 'M'
         AND sy-langu <> '3' AND sy-langu <> '7'.
*  1 = ZH Simplified Chinese, 2 = TH Thai, ZF = Traditional Chinese
*  3 = KO korean, 7 = MS Malayian
          WRITE:
            / sy-vline NO-GAP,
         (79) text_093,
        40(1) sy-vline,
           81 sy-vline.
        ELSE.
          WRITE:
             / sy-vline NO-GAP,
          (15) text_093,
         40(1) sy-vline,
         41(20) text_093+35,
         81 sy-vline.
        ENDIF.
        HIDE flg_local.
        WRITE:
            / sy-uline.
        HIDE flg_local.
      ELSE.
        MESSAGE s065.
        WRITE:
          text_092 TO txt_zeile.
        MESSAGE s065 WITH txt_zeile.
        IF sy-langu <> 'J'
         AND sy-langu <> '1' AND sy-langu <> '2' AND sy-langu <> 'M'
         AND sy-langu <> '3' AND sy-langu <> '7'.
          WRITE:
          text_093 TO txt_zeile,
          '/'      TO txt_zeile+37(1).
          CONDENSE txt_zeile.
        ELSE.
          WRITE text_093 TO txt_zeile.
        ENDIF.
        MESSAGE s065 WITH txt_zeile.
        MESSAGE s064.
      ENDIF.
      up_titel = 1.
    ENDIF.
    IF sy-batch EQ space.
      FORMAT COLOR 2 INTENSIFIED OFF.
      flg_local = 'F'.
      FORMAT HOTSPOT ON.
      WRITE:
          / sy-vline NO-GAP,
            tab_ausgabe-name,
         40 sy-vline NO-GAP,
            tab_ausgabe-filename(40),
         81 sy-vline.
      FORMAT HOTSPOT OFF.
      HIDE: flg_local, tab_ausgabe.
    ELSE.
      IF sy-langu <> 'J'
       AND sy-langu <> '1' AND sy-langu <> '2' AND sy-langu <> 'M'
       AND sy-langu <> '3' AND sy-langu <> '7'.
        WRITE:
        tab_ausgabe-name TO txt_zeile,
        '/'              TO txt_zeile+37(1).
        CONDENSE txt_zeile.
      ELSE.
        WRITE tab_ausgabe-name TO txt_zeile.
      ENDIF.
      MESSAGE s065 WITH txt_zeile tab_ausgabe-filename.
    ENDIF.
    DELETE tab_ausgabe.
  ENDLOOP.

* Dritter Loop zur Ausgabe der Spool-Dateien
  CLEAR tab_ausgabe.
  LOOP AT tab_ausgabe.
    up_uline = 1.
    AT FIRST.
      IF sy-batch EQ space.
        FORMAT COLOR 1 INTENSIFIED.
        flg_local = space.
        WRITE:
            / sy-uline.
        HIDE flg_local.
        WRITE:
            / sy-vline NO-GAP,
         (79) text_090,
           81 sy-vline,
              sy-uline.
        HIDE flg_local.
        FORMAT COLOR 1 INTENSIFIED OFF.
        IF sy-langu <> 'J'
         AND sy-langu <> '1' AND sy-langu <> '2' AND sy-langu <> 'M'
         AND sy-langu <> '3' AND sy-langu <> '7'.
          WRITE:
            / sy-vline NO-GAP,
         (79) text_091,
        40(1) sy-vline NO-GAP,
        55(1) sy-vline NO-GAP,
           81 sy-vline.
        ELSE.
          WRITE:
            / sy-vline NO-GAP,
         (30) text_091,
        40(1) sy-vline NO-GAP,
         (35) text_091+35,
           81 sy-vline.
        ENDIF.
        HIDE flg_local.
        WRITE:
            / sy-uline.
        HIDE flg_local.
      ELSE.
        MESSAGE s065.
        WRITE:
          text_090 TO txt_zeile.
        MESSAGE s065 WITH txt_zeile.
        IF sy-langu <> 'J'
         AND sy-langu <> '1' AND sy-langu <> '2' AND sy-langu <> 'M'
         AND sy-langu <> '3' AND sy-langu <> '7'.
          WRITE:
          text_091 TO txt_zeile,
          '/'      TO txt_zeile+37(1),
          '/'      TO txt_zeile+52(1).
          CONDENSE txt_zeile.
        ELSE.
          WRITE text_091 TO txt_zeile.
        ENDIF.
        MESSAGE s065 WITH txt_zeile.
        MESSAGE s064.
      ENDIF.
    ENDAT.
    IF sy-batch EQ space.
      IF tab_ausgabe-error EQ 'X'.
        FORMAT COLOR 6 INTENSIFIED.
      ELSE.
        FORMAT COLOR 2 INTENSIFIED OFF.
      ENDIF.
      IF tab_ausgabe-error EQ 'X'.
        flg_local = 'E'.
      ELSE.
        flg_local = 'S'.
      ENDIF.
      FORMAT HOTSPOT ON.

      WRITE:
          / sy-vline NO-GAP,
            tab_ausgabe-name,
         40 sy-vline NO-GAP,
            tab_ausgabe-dataset,
         55 sy-vline NO-GAP.
      IF NOT tab_ausgabe-spoolnr IS INITIAL.
        WRITE tab_ausgabe-spoolnr.
      ENDIF.
      WRITE 81 sy-vline.
      FORMAT HOTSPOT OFF.
      HIDE: flg_local, tab_ausgabe.
    ELSE.
      IF sy-langu <> 'J'
       AND sy-langu <> '1' AND sy-langu <> '2' AND sy-langu <> 'M'
       AND sy-langu <> '3' AND sy-langu <> '7'.
        WRITE:
          tab_ausgabe-name     TO txt_zeile,
          '/'                  TO txt_zeile+37(1),
          tab_ausgabe-dataset  TO txt_zeile+40,
          '/'                  TO txt_zeile+52(1).
        IF NOT tab_ausgabe-spoolnr IS INITIAL.
          WRITE tab_ausgabe-spoolnr TO txt_zeile+55.
        ENDIF.
        CONDENSE txt_zeile.
        MESSAGE s065 WITH txt_zeile(50) txt_zeile+50.
      ELSE.
        CONCATENATE tab_ausgabe-name  '/' tab_ausgabe-dataset '/'
           INTO txt_zeile.
        CONDENSE txt_zeile.
        MESSAGE s065 WITH txt_zeile tab_ausgabe-spoolnr.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF sy-batch EQ space AND up_uline NE 0.
    flg_local = space.
    WRITE:
      / sy-uline.
    HIDE flg_local.
  ENDIF.


ENDFORM.                               "INFORMATION

*----------------------------------------------------------------------*
* FORM INFORMATION_2                                                   *
*----------------------------------------------------------------------*
* Output an accessible list with information about generated IDocs,
* payment advices (spool, E-mail, fax), accompanying sheet and spool
* error list when in dialog mode.
* Output the same information as F110-log compatible messages when in
* batch mode.
* Trigger immediate printing when required.
*----------------------------------------------------------------------*
*     -->TAB_AUSGABE (global):  file, idoc, advice and list output
*----------------------------------------------------------------------*
FORM information_2.

  DATA:
    up_spool                LIKE tsp01-rqident,
    up_list_heading_done(1) TYPE c VALUE space.

*--- trigger immediate printing
  LOOP AT tab_ausgabe WHERE immed NE space.
    CHECK tab_ausgabe-spoolnr NE 0.
    up_spool = tab_ausgabe-spoolnr.
    CALL FUNCTION 'RSPO_OUTPUT_SPOOL_REQUEST'
      EXPORTING
        spool_request_id = up_spool
      EXCEPTIONS
        OTHERS           = 0.
  ENDLOOP.
*--- no info when print immediately and when re-print of a check
  IMPORT flg_local FROM MEMORY ID 'MFCHKFN0'.
  CHECK:
    par_sofo EQ space,
    sy-subrc NE 0.
  IF sy-batch EQ space.
    NEW-PAGE NO-TITLE LINE-SIZE 81.
  ENDIF.

*--- info on files and idocs the program has created
  LOOP AT tab_ausgabe WHERE renum NE space.
    IF up_list_heading_done NE 'X'.
      PERFORM information_list_heading USING 'DME'.
      up_list_heading_done = 'X'.
    ENDIF.
    PERFORM information_on_dme USING tab_ausgabe.
    DELETE tab_ausgabe.
  ENDLOOP.
  IF  sy-subrc EQ 0
  AND sy-batch EQ space.
    WRITE / sy-uline.
  ENDIF.

  CLEAR up_list_heading_done.
*--- info on lists and advices the program has created
  LOOP AT tab_ausgabe.
    IF up_list_heading_done NE 'X'.
      PERFORM information_list_heading USING 'LIST'.
      up_list_heading_done = 'X'.
    ENDIF.
    PERFORM information_on_lists USING tab_ausgabe.
  ENDLOOP.
  IF  sy-subrc EQ 0
  AND sy-batch EQ space.
    WRITE / sy-uline.
  ENDIF.

ENDFORM.                               "INFORMATION_2

*----------------------------------------------------------------------*
* AT LINE-SELECTION                                                    *
*----------------------------------------------------------------------*
* Auswahl einer Zeile der Information                                  *
* Verzweigen in die DTA-Verwaltung oder Druckverwaltung                *
*----------------------------------------------------------------------*
AT LINE-SELECTION.

  TYPE-POOLS sp01r.
  DATA up_list TYPE sp01r_id_list WITH HEADER LINE.
  DATA BEGIN OF up_bdc OCCURS 9.
  INCLUDE STRUCTURE bdcdata.
  DATA END OF up_bdc.
  DATA up_fdta      LIKE tstc-tcode VALUE 'FDTA'.
  DATA up_laufd(10) TYPE c.
  DATA up_meldung   LIKE shkontext-meldung.
  DATA up_titel     LIKE shkontext-titel.

  IF sy-lsind EQ 1.

    CASE flg_local.
      WHEN space.                      "keine gültige Auswahl

      WHEN 'E'.                        "Fehlerliste
        FORMAT COLOR 6 INTENSIFIED.
        ULINE AT (102).
        WRITE:
          /     sy-vline NO-GAP,
          (100) text_003 NO-GAP,
                sy-vline.
        FORMAT COLOR 2.
        ULINE AT (102).
        REFRESH tab_fimsg.
        CALL FUNCTION 'FI_MESSAGE_GET'
          TABLES
            t_fimsg = tab_fimsg.
        LOOP AT tab_fimsg.
          fimsg = tab_fimsg.
          CALL FUNCTION 'K_MESSAGE_TRANSFORM'
            EXPORTING
              par_langu = sy-langu
              par_msgid = fimsg-msgid
              par_msgno = fimsg-msgno
              par_msgty = fimsg-msgty
              par_msgv1 = fimsg-msgv1
              par_msgv2 = fimsg-msgv2
              par_msgv3 = fimsg-msgv3
              par_msgv4 = fimsg-msgv4
            IMPORTING
              par_msgtx = txt_zeile
            EXCEPTIONS
              OTHERS    = 8.
          WRITE:
            /    sy-vline    NO-GAP,
                 fimsg-msgid(2),
                 fimsg-msgno NO-GAP,
                 sy-vline    NO-GAP,
            (93) txt_zeile   NO-GAP,
                 sy-vline.
          HIDE: flg_local, fimsg, txt_zeile.
          AT END OF msort.
            ULINE AT (102).
          ENDAT.
        ENDLOOP.
        CLEAR: fimsg, txt_zeile.

      WHEN 'F'.                        "Sprung in die DTA-Verwaltung
        CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
          EXPORTING
            tcode  = up_fdta
          EXCEPTIONS
            ok     = 0
            OTHERS = 4.
        IF sy-subrc NE 0.
          MESSAGE s172(00) WITH up_fdta.
        ELSE.
          REFRESH up_bdc.
          CLEAR up_bdc.
          up_bdc-program  = 'SAPMFDTA'.
          up_bdc-dynpro   = '100'.
          up_bdc-dynbegin = 'X'.
          APPEND up_bdc.
          CLEAR up_bdc.
          up_bdc-fnam     = 'REGUT-RENUM'.
          up_bdc-fval     = tab_ausgabe-renum.
          APPEND up_bdc.
          CLEAR up_bdc.
          up_bdc-fval     = '/8'.
          up_bdc-fnam     = 'BDC_OKCODE'.
          APPEND up_bdc.
          CLEAR up_bdc.
          up_bdc-program  = 'SAPMFDTA'.
          up_bdc-dynpro   = '200'.
          up_bdc-dynbegin = 'X'.
          APPEND up_bdc.
          CLEAR up_bdc.
          up_bdc-fval     = '/BDA'.
          up_bdc-fnam     = 'BDC_OKCODE'.
          APPEND up_bdc.
          CALL TRANSACTION up_fdta USING up_bdc MODE 'E'.
        ENDIF.

      WHEN 'S'.                        "Sprung in die Druckverwaltung
        CHECK tab_ausgabe-spoolnr NE space.
        REFRESH up_list.
        up_list-id = tab_ausgabe-spoolnr.
        APPEND up_list.
        CALL FUNCTION 'RSPO_RID_SPOOLREQ_LIST'
          EXPORTING
            id_list = up_list[]
          EXCEPTIONS
            OTHERS  = 0.

    ENDCASE.

  ELSE.

    CHECK:
      flg_local EQ 'E',
      NOT fimsg-msgid IS INITIAL,
      NOT fimsg-msgno IS INITIAL.
    up_titel   = text_003.
    up_meldung = txt_zeile.
    CALL FUNCTION 'HELPSCREEN_NA_CREATE'
      EXPORTING
        langu   = sy-langu
        meldung = up_meldung
        meld_id = fimsg-msgid
        meld_nr = fimsg-msgno
        msgv1   = fimsg-msgv1
        msgv2   = fimsg-msgv2
        msgv3   = fimsg-msgv3
        msgv4   = fimsg-msgv4
        titel   = up_titel.
    CLEAR fimsg.

  ENDIF.


*----------------------------------------------------------------------*
* FORM BELEGDATEN_SCHREIBEN                                            *
*----------------------------------------------------------------------*
* Beleginformation zum Speichern in interne Tabelle schreiben          *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM belegdaten_schreiben.

  CLEAR tab_belege30a.
  tab_belege30a-mandt   = sy-mandt.
  IF reguh-pyord IS INITIAL.
    tab_belege30a-bukrs = reguh-zbukr.
    tab_belege30a-belnr = reguh-vblnr.
    tab_belege30a-gjahr = regud-gjahr.
    tab_belege30a-ubhkt = reguh-ubhkt.
  ELSE.
    tab_belege30a-pyord = reguh-pyord.
  ENDIF.
  APPEND tab_belege30a.


ENDFORM.                               "BELEGDATEN_SCHREIBEN



*----------------------------------------------------------------------*
* FORM TAB_BELEGE_SCHREIBEN                                            *
*----------------------------------------------------------------------*
* Alle Beleginformationen in die Datenbank sichern                     *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM tab_belege_schreiben.

  DATA: relid LIKE sy-saprl.           "Current Release
  DATA: tab_belege40a LIKE dta_belege OCCURS 0 WITH HEADER LINE.

  rfdt-aedat = sy-datlo.
  rfdt-usera = sy-uname.
  rfdt-pgmid = sy-repid.

  relid = sy-saprl.
  EXPORT tab_belege40a FROM tab_belege30a
         relid                         "Tabelle und Release sichern
         TO DATABASE rfdt(fb)
         ID hlp_dta_id.
  IF sy-subrc NE 0.
    IF sy-batch EQ space.
      MESSAGE a226 WITH 'RFDT'.
    ELSE.
      MESSAGE s226 WITH 'RFDT'.
      STOP.
    ENDIF.
  ENDIF.

ENDFORM.                               "TAB_BELEGE_SCHREIBEN



*----------------------------------------------------------------------*
* FORM ZUSATZFELD_FUELLEN                                              *
*----------------------------------------------------------------------*
* Füllt die landes- oder formatspezifischen Zusatzdaten in REGUT-DTKEY *
* Fills the additional country or format specific information          *
*----------------------------------------------------------------------*
* EXPORT:                                                              *
* ZUSATZ  - Feld, in das die Zusatzdaten einzutragen sind              *
* KFZLAND - KFZ-Code des Landes des aufrufenden Druckprogramms         *
*----------------------------------------------------------------------*
FORM zusatzfeld_fuellen USING zusatz kfzland.

  DATA up_zusatz LIKE regut-dtkey.

* Formatspezifisches Zusatzfeld
  IF hlp_dtfor EQ 'MT100'.             "Swift international
    up_zusatz      = reguh-hbkid.

  ELSEIF hlp_dtfor EQ 'SAP IDOC'.      "IDoc für EDI
    up_zusatz      = reguh-hbkid.
    up_zusatz+5(1) = regud-xeinz.

  ELSEIF hlp_dtfor(5) EQ 'DTAUS'       "Deutschland Inland
      OR hlp_dtfor    EQ 'MTS'         "Neuseeland Inland
      OR hlp_dtfor    EQ 'BECS'.       "Australien Inland
    up_zusatz      = reguh-hbkid.
    up_zusatz+5(1) = regud-xeinz.
    up_zusatz+6    = reguh-hktid.

* Länderspezifisches Zusatzfeld
  ELSEIF kfzland EQ 'CH'.              "Schweiz
    up_zusatz      = reguh-hbkid.
    up_zusatz+5(1) = regud-xeinz.
    up_zusatz+6    = reguh-hktid.
  ELSEIF kfzland EQ 'DK'               "Dänemark
      OR kfzland EQ 'F'.               "Frankreich
    up_zusatz      = reguh-hbkid.
    up_zusatz+5(1) = regud-xeinz.

  ELSEIF kfzland EQ 'ZA'.              "Südafrika
    SELECT * FROM regut
        WHERE banks EQ 'ZA'
          AND dtfor EQ hlp_dtfor       "Entweder nur ACB oder nur EFT
          AND laufd EQ reguh-laufd
          AND laufi EQ reguh-laufi.
      EXIT.
    ENDSELECT.
    IF sy-subrc EQ 0.                  "Eintrag gefunden
      up_zusatz = regut-dtkey.         "Da schon vorhanden -> kopieren
    ELSE.
      SELECT * FROM regut
        WHERE banks EQ 'ZA '
          AND dtfor EQ hlp_dtfor
        ORDER BY dtkey.
      ENDSELECT.
      IF sy-subrc NE 0.
        up_zusatz+0(4) = '0001'.
      ELSE.
        ADD 1 TO regut-dtkey+0(4).
        WRITE regut-dtkey TO up_zusatz+0(4) RIGHT-JUSTIFIED.
        WHILE up_zusatz+0(4) CA space.
          REPLACE space WITH '0' INTO up_zusatz+0(4).
        ENDWHILE.
      ENDIF.
    ENDIF.

  ELSE.                                "default
    up_zusatz = reguh-hbkid.
  ENDIF.

  zusatz = up_zusatz.

ENDFORM.                               "ZUSATZFELD_FUELLEN



*----------------------------------------------------------------------*
* FORM TEMSE_OEFFNEN                                                   *
*----------------------------------------------------------------------*
* Öffnen einer TemSe-Datei für schreibenden Zugriff. Ein Zusatzfeld    *
* im Schlüssel und eine 8-stellige Referenz-Nummer werden übergeben.   *
* Es wird ein Satz in die Datei geschrieben.                           *
* Open a temporary sequential file for write access. The file-name     *
* is generated in the form 'TEMSE_NAME'. One record is stored.         *
* 'DTKEY' contains key-field-information for this record. An 8-digit   *
* random number is returned.                                           *
*----------------------------------------------------------------------*
* IMPORT:                                                              *
* *REGUT-DTKEY  - ein Zusatzkey zu den TemSe-Daten                     *
*               - an additional key                                    *
* EXPORT:                                                              *
* HLP_RENUM     - 8-stellige Nummer, noch nicht vergeben               *
*               - an 8-digit numeric to generate a reference-string    *
*----------------------------------------------------------------------*
FORM temse_oeffnen.
  DATA: _rc(5),
        _errmsg(100).

  PERFORM naechster_index USING hlp_renum.

  PERFORM fuellen_regut USING *regut-dtkey.

  PERFORM temse_name USING hlp_renum   "Dateinamen generieren lassen
                           hlp_temsename.

  *regut-tsnam = hlp_temsename.       "Name der TemSe-Datei

  CALL 'C_RSTS_OPEN_WRITE'
       ID 'HANDLE'  FIELD hlp_handle   "file-handle
       ID 'NAME'    FIELD hlp_temsename"gewünschter Dateiname
       ID 'BINARY'  FIELD 'X'          "binär öffnen !
       ID 'TYPE'    FIELD 'DATA'
       ID 'RECTYP'  FIELD 'U------'    "Zusatz zum binären Öffnen
       ID 'RC'      FIELD _rc
       ID 'ERRMSG'  FIELD _errmsg.                        "#EC CI_CCALL

  IF sy-subrc NE 0.                    "Fehler beim Öffnen
    IF sy-batch EQ space.
      MESSAGE a182(fr) WITH hlp_temsename.
    ELSE.
      MESSAGE s182(fr) WITH hlp_temsename.
      STOP.
    ENDIF.
  ENDIF.


ENDFORM.                               "TEMSE_OEFFNEN



*----------------------------------------------------------------------*
* FORM TEMSE_SCHREIBEN                                                 *
*----------------------------------------------------------------------*
* Schreiben in die geöffnete TemSe-Datei.                              *
* writing data to the already opened file                              *
*----------------------------------------------------------------------*
* IMPORT:                                                              *
* BUFFER - der zu schreibende Text                                     *
*        - data to be stored                                           *
*----------------------------------------------------------------------*
FORM temse_schreiben USING VALUE(buffer).
  DATA: _rc(5),
        _errmsg(100).

  CALL 'C_RSTS_WRITE'
       ID 'HANDLE'  FIELD hlp_handle
       ID 'BUFF'    FIELD buffer
       ID 'RC'      FIELD _rc
       ID 'ERRMSG'  FIELD _errmsg.                        "#EC CI_CCALL

  IF sy-subrc NE 0.                    "Fehler beim Schreiben
    IF sy-batch EQ space.
      MESSAGE a229.
    ELSE.
      MESSAGE s229.
      STOP.
    ENDIF.
  ENDIF.

ENDFORM.                               "TEMSE_SCHREIBEN



*----------------------------------------------------------------------*
* FORM TEMSE_SCHLIESSEN                                                *
*----------------------------------------------------------------------*
* Schließen der geöffneten TemSe-Datei.                                *
* close file                                                           *
*----------------------------------------------------------------------*
* IMPORT:                                                              *
*----------------------------------------------------------------------*
FORM temse_schliessen.
  DATA: _rc(5),
        _errmsg(100).

  CALL 'C_RSTS_CLOSE'
       ID 'HANDLE'  FIELD hlp_handle
       ID 'RC'      FIELD _rc
       ID 'ERRMSG'  FIELD _errmsg.                        "#EC CI_CCALL

  IF sy-subrc NE 0.                    " Fehler beim Schließen
    IF sy-batch EQ space.
      MESSAGE w230.
    ELSE.
      MESSAGE s230.
      STOP.
    ENDIF.
  ELSE.
    CLEAR hlp_handle.
  ENDIF.

ENDFORM.                               "TEMSE_SCHLIESSEN



*----------------------------------------------------------------------*
* FORM NAECHSTER_INDEX                                                 *
*----------------------------------------------------------------------*
* nächsten freien Index suchen                                         *
* get next free number                                                 *
*----------------------------------------------------------------------*
* NUMBER enthält diesen Index / contains a new number                  *
*----------------------------------------------------------------------*
FORM naechster_index USING number LIKE febkey-kukey.

  CALL FUNCTION 'GET_SHORTKEY_FOR_FEBKO'  "Nächsten freien Index holen
    EXPORTING
      i_tname             = 'TEMSE'
    IMPORTING
      e_kukey             = number
    EXCEPTIONS
      febkey_update_error = 1.

  IF sy-subrc = 1.
    IF sy-batch EQ space.
      MESSAGE a228 WITH 'FEBKEY'.
    ELSE.
      MESSAGE s228 WITH 'FEBKEY'.
      STOP.
    ENDIF.
  ELSEIF number EQ '00000000'.         "Field: FEBKEY-KUKEY: Numeric(8)
    PERFORM naechster_index USING number. "nächste Nummer holen
  ENDIF.

ENDFORM.                               "NAECHSTER_INDEX



*----------------------------------------------------------------------*
* FORM TEMSE_NAME                                                      *
*----------------------------------------------------------------------*
* TemSe-Namen generieren                                               *
* Generate a new TemSe-name                                            *
*----------------------------------------------------------------------*
* IMPORT:                                                              *
*   NUMBER: eine 8-stellige Zufallszahl (noch unbenutzt)               *
*   NUMBER: is a 8-digit random number (currently unused)              *
* EXPORT:                                                              *
*   FILE enthält Namen der TemSe-Datei                                 *
*   FILE contains the new file-name                                    *
* BEISPIEL / EXAMPLE:                                                  *
*   NUMBER = 00193711, DATE = MAR 2, 1994, TIME = 08:12:37             *
*   --> FILE = DTA940302081237_3711                                    *
*----------------------------------------------------------------------*
FORM temse_name USING VALUE(number)
                      file.

  DATA: up_name(64),
        up_num      LIKE febko-kukey,
        up_len      TYPE p.

  up_num = number.
  DESCRIBE FIELD file LENGTH up_len IN CHARACTER MODE.

  IF up_len LT 20.                     "Mindestlänge des Namens
    IF sy-batch EQ space.
      MESSAGE a183.
    ELSE.
      MESSAGE s183.
      STOP.
    ENDIF.
  ENDIF.

  CLEAR up_name.
  up_name    = 'DTA'.
  up_name+3  = sy-datlo+2(6).
  up_name+9  = sy-timlo(6).
  up_name+15 = '_'.
  up_name+16 = up_num+4(4).

  file = up_name.

ENDFORM.                               "TEMSE_NAME



*----------------------------------------------------------------------*
* FORM DATEI_OEFFNEN                                                   *
*----------------------------------------------------------------------*
* die jeweilige Datei (TemSe/File) öffnen                              *
* open current file (either in TemSe or file-system)                   *
*----------------------------------------------------------------------*
FORM datei_oeffnen.

  IF hlp_temse CA par_dtyp.            "TemSe-Format
    PERFORM temse_oeffnen.
  ELSE.                                "disk-/tape-fmt on file-system
    PERFORM naechster_index USING hlp_renum.
    PERFORM fuellen_regut USING *regut-dtkey.
    ADD 1 TO cnt_filenr.
    hlp_filename    = par_unix.
    hlp_filename+45 = cnt_filenr.
    CONDENSE hlp_filename NO-GAPS.
    DATA i TYPE i.
    i = cl_abap_char_utilities=>charsize.
    IF i = 1.
      OPEN DATASET hlp_filename IN BINARY MODE FOR OUTPUT.
    ELSE.  " unicode system
      IF p_unico IS NOT INITIAL               " note 970892
       OR p_codep = '4102' OR p_codep = '4103'.  "write file in unicode
        OPEN DATASET hlp_filename IN BINARY MODE FOR OUTPUT.
      ELSEIF p_codep = '4110'.   " UTF-8
        OPEN DATASET hlp_filename IN TEXT MODE FOR OUTPUT
                                          ENCODING DEFAULT.
      ELSEIF p_codep IS NOT INITIAL.   "write file in codepage
        OPEN DATASET hlp_filename FOR OUTPUT
                               IN LEGACY BINARY MODE
                               CODE PAGE p_codep.
      ELSE.
        OPEN DATASET hlp_filename FOR OUTPUT
                               IN LEGACY BINARY MODE.
      ENDIF.
    ENDIF.
    IF sy-subrc NE 0.
      IF sy-batch EQ space.
        MESSAGE a182(fr) WITH hlp_filename.
      ELSE.
        MESSAGE s182(fr) WITH hlp_filename.
        STOP.
      ENDIF.
    ENDIF.
  ENDIF.

* Referenznr für RFDT sichern, Tabelle für Zahlungsbelege löschen
* store reference-number, refresh table for document-numbers
  CALL FUNCTION 'COMPUTE_CONTROL_NUMBER'
    EXPORTING
      i_refno  = hlp_renum
    IMPORTING
      e_result = hlp_resultat.
  regud-label = hlp_dta_id-refnr = hlp_resultat.
  CLEAR   tab_belege30a.
  REFRESH tab_belege30a.

ENDFORM.                               "DATEI_OEFFNEN



*----------------------------------------------------------------------*
* FORM DATEI_SCHLIESSEN                                                *
*----------------------------------------------------------------------*
* die jeweilige Datei (TemSe/File) schliessen                          *
* close current file (either in TemSe or file-system)                  *
*----------------------------------------------------------------------*
* Benutzt wird HLP_TEMSENAME und TEXT_006 bei Schreiben in TemSe bzw.  *
*              HLP_FILENAME  und TEXT_005 bei Schreiben ins Filesystem *
* Routine uses HLP_TEMSENAME and TEXT_006 when writing to TemSe resp.  *
*              HLP_FILENAME  and TEXT_005 when writing to file system  *
*----------------------------------------------------------------------*
FORM datei_schliessen.

  DATA:
    up_fname LIKE hlp_filename,
    up_text  LIKE rfpdo2-fordtext.

  PERFORM abschluss_regut.
  IF hlp_temse CA par_dtyp.            "TemSe
    PERFORM temse_schliessen.
    up_fname = hlp_temsename.
    up_text  = text_006.
  ELSE.                                "Filesystem
    CLOSE DATASET hlp_filename.
    up_fname = hlp_filename.
    up_text  = text_005.
  ENDIF.

  CLEAR tab_ausgabe.
  tab_ausgabe-name     = up_text.
  tab_ausgabe-filename = up_fname.
  tab_ausgabe-renum    = *regut-renum.
  REPLACE '&' WITH reguh-hbkid INTO tab_ausgabe-name.
  COLLECT tab_ausgabe.

* Gesammelte Zahlungsbelegdaten in Datenbank RFDT sichern
* Store payment documents (not for proposal run or HR !)
  IF reguh-xvorl IS INITIAL AND        "Kein Vorschlagslauf und
     hlp_laufk CA ' *R'.               "nur FI oder Payment Request
    PERFORM tab_belege_schreiben.
  ENDIF.

ENDFORM.                               "DATEI_SCHLIESSEN



*----------------------------------------------------------------------*
* FORM FUELLEN_REGUT                                                   *
*----------------------------------------------------------------------*
* REGUT-Felder mit den bereits bekannten Werten füllen                 *
* Fill REGUT-fields with current values                                *
*----------------------------------------------------------------------*
* IMPORT:                                                              *
*----------------------------------------------------------------------*
FORM fuellen_regut USING VALUE(dtkey).

  CLEAR *regut.
  MOVE-CORRESPONDING reguh TO *regut.  "ZBUKR,LAUFD,LAUFI,XVORL füllen
  *regut-banks = reguh-ubnks.

* Die bereits bekannten Funktionsfelder füllen
  *regut-dtkey  = dtkey.              "Zusatzkey
  *regut-tsusr  = sy-uname.
  *regut-report = sy-repid.

  CLEAR regut-lfdnr.                   "Wichtig bei Bankwechsel !
  SELECT * FROM regut                  "Bestimme die tatsächliche LFDNR,
         WHERE zbukr = reguh-zbukr     "  wenn nur ein zahlender
           AND banks = reguh-ubnks     "  Buchungskreis vorliegt
           AND laufd = reguh-laufd
           AND laufi = reguh-laufi
           AND xvorl = reguh-xvorl
           AND dtkey = dtkey
         ORDER BY lfdnr DESCENDING.    "Größte Nummer
    EXIT.
  ENDSELECT.
  IF NOT sy-subrc IS INITIAL.
    SELECT * FROM regut                "Bestimme die tatsächliche LFDNR,
           WHERE zbukr = space         "  wenn mehrere zahlende
             AND banks = reguh-ubnks   "  Buchungskreise im File vor-
             AND laufd = reguh-laufd   "  kommen (ZBUKR = SPACE)
             AND laufi = reguh-laufi
             AND xvorl = reguh-xvorl
             AND dtkey = dtkey
           ORDER BY lfdnr DESCENDING.  "Größte Nummer
      EXIT.
    ENDSELECT.
  ENDIF.

  *regut-lfdnr = regut-lfdnr + 1.     "nächstgrößeren Wert nehmen

ENDFORM.                               "FUELLEN_REGUT



*----------------------------------------------------------------------*
* FORM ABSCHLUSS_REGUT                                                 *
*----------------------------------------------------------------------*
* Noch unbesetzte REGUT-Felder füllen, Datensatz aktualisieren         *
* Fill missing REGUT-fields and update record on database              *
*----------------------------------------------------------------------*
* IMPORT: FILENAME - Nur bei TemSe: Vorschlag für Download-Dateinamen  *
*         FILENAME - only for TemSe: Proposal filename for download    *
*----------------------------------------------------------------------*
FORM abschluss_regut.

  DATA:   up_lines LIKE sy-linno.
  RANGES: up_zbukr FOR  reguta-zbukr.

  IF hlp_tsdat IS INITIAL.             "Noch kein Datum erfasst
    hlp_tsdat = sy-datlo.
  ENDIF.

  IF hlp_tstim IS INITIAL.             "Noch keine Zeit erfasst
    hlp_tstim = sy-timlo.
  ENDIF.

* Noch unbelegte Funktionsfelder der Leiste für REGUT füllen
  *regut-waers = t001-waers.
  *regut-rbetr = sum_regut.
  *regut-renum = hlp_resultat.
  *regut-dtfor = hlp_dtfor.
  *regut-tsdat = hlp_tsdat.           "Werte der Zeiterfassung kopieren
  *regut-tstim = hlp_tstim.
  *regut-saprl = sy-saprl.

* Fill field regut-codepage:
*   get system-codepage:
  DATA ld_system_codepage LIKE tcp00-cpcodepage.
  CALL FUNCTION 'SCP_GET_CODEPAGE_NUMBER'
    IMPORTING
      appl_codepage = ld_system_codepage.

  IF cl_abap_char_utilities=>charsize > 1
   AND NOT ( hlp_temse CA par_dtyp ).     "Unicode and Filesystem
    IF p_unico IS NOT INITIAL.    "file is in unicode-cp
      *regut-codepage = ld_system_codepage.
    ELSEIF p_codep IS NOT INITIAL.
      *regut-codepage = p_codep.
    ELSE.
      DATA ld_cur_mb_codepage(4).
      CALL 'CUR_LCL' ID 'UUSEMBCP' FIELD ld_cur_mb_codepage.
      IF ld_cur_mb_codepage <> space.
        *regut-codepage = ld_cur_mb_codepage.
      ELSE.
        *regut-codepage = '0000'.
      ENDIF.
    ENDIF.
  ELSE.  " Non-Unicode, File-system or TEMSE
    *regut-codepage = ld_system_codepage.
  ENDIF.


  IF hlp_temse CA par_dtyp.            "TemSe
    IF NOT par_unix IS INITIAL.        "Wert wurde bereits angegeben
      *regut-dwnam = par_unix.        "als Vorschlagswert übernehmen
    ENDIF.
  ELSE.                                "keine Temse -> Download fertig !
    *regut-fsnam = hlp_filename.      "Dateinamen übernehmen,
    *regut-dwnam = hlp_filename.      "als Downloadnamen setzen und
    *regut-dwdat = sy-datlo.          "aktuelle Daten festhalten
    *regut-dwtim = sy-timlo.
    *regut-dwusr = sy-uname.
  ENDIF.

* Update DB table of paying company codes and origins of the file
* clear paying company code if it is not unique
  CLEAR: up_lines, up_zbukr, up_zbukr[].
  up_zbukr-sign   = 'I'.
  up_zbukr-option = 'EQ'.
  LOOP AT tab_reguta.
    up_zbukr-low = tab_reguta-zbukr.
    COLLECT up_zbukr.
  ENDLOOP.
  DESCRIBE TABLE up_zbukr LINES up_lines.
  IF up_lines > 1.
    CLEAR *regut-zbukr.
    DO.    "check REGUTA to avoid duprecs (see note 623539)
      SELECT COUNT(*) FROM reguta
                     WHERE banks EQ *regut-banks
                       AND laufd EQ *regut-laufd
                       AND laufi EQ *regut-laufi
                       AND xvorl EQ *regut-xvorl
                       AND dtkey EQ *regut-dtkey
                       AND lfdnr EQ *regut-lfdnr
                       AND zbukr IN up_zbukr.
      IF sy-dbcnt NE 0.
        ADD 1 TO *regut-lfdnr.
      ELSE.
        EXIT.
      ENDIF.
      IF *regut-lfdnr EQ 999.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  INSERT regut FROM *regut.
  IF sy-subrc NE 0.                    "Insert fehlerhaft
    DO 10 TIMES.                       "10 Versuche maximal !
      *regut-lfdnr = *regut-lfdnr + sy-index.
      INSERT regut FROM *regut.        "Suche springend nach freiem Wert
      IF sy-subrc EQ 0.
        EXIT.
      ENDIF.
    ENDDO.

    IF sy-subrc NE 0.                  "Inserts fehlerhaft --> Abbruch
      IF sy-batch EQ space.
        MESSAGE a228 WITH 'REGUT'.
      ELSE.
        MESSAGE s228 WITH 'REGUT'.
        STOP.
      ENDIF.
    ENDIF.
  ENDIF.                               "IF SY-SUBRC...

  READ TABLE tab_reguta INDEX 1.
  IF tab_reguta-lfdnr <> *regut-lfdnr.
    LOOP AT tab_reguta.
      tab_reguta-lfdnr = *regut-lfdnr.
      MODIFY tab_reguta.
    ENDLOOP.
  ENDIF.
  INSERT reguta FROM TABLE tab_reguta.
  CLEAR: tab_reguta, tab_reguta[].

  CALL FUNCTION 'DB_COMMIT'.

  SELECT * FROM regut UP TO 1 ROWS     "Test, ob gerade ein Duplikat
         WHERE zbukr = *regut-zbukr    "  erstellt worden ist
           AND banks = *regut-banks    "  (gleiche Attribute, aber
           AND laufd = *regut-laufd    "  andere Referenznummer)
           AND laufi = *regut-laufi
           AND xvorl = *regut-xvorl
           AND dtkey = *regut-dtkey
           AND waers = *regut-waers
           AND rbetr = *regut-rbetr
           AND dtfor = *regut-dtfor
           AND renum NE *regut-renum.
  ENDSELECT.
  IF sy-subrc EQ 0.
    fimsg-msgv1 = regut-renum.
    fimsg-msgv2 = *regut-renum.
    PERFORM message USING 417.
  ENDIF.

ENDFORM.                               "ABSCHLUSS_REGUT



*----------------------------------------------------------------------*
* FORM STORE_ON_FILE                                                   *
*----------------------------------------------------------------------*
* Ausgabe in die TemSe oder in das File-System                         *
* Output into TemSe or into file-system                                *
*----------------------------------------------------------------------*
* IMPORT: DATEN : zu schreibende Daten                                 *
*                 data to be stored                                    *
*----------------------------------------------------------------------*
FORM store_on_file USING daten.

  IF hlp_temse CA par_dtyp.            "Temse
    PERFORM temse_schreiben USING daten.
  ELSE.
    TRANSFER daten TO hlp_filename.
  ENDIF.

* Fill internal table of paying company codes of actual DME file
* for FDTA authority check
  MOVE-CORRESPONDING
       *regut TO tab_reguta.
  tab_reguta-zbukr = reguh-zbukr.
  tab_reguta-dorigin = reguh-dorigin.
  COLLECT tab_reguta.

ENDFORM.                               "STORE_ON_FILE


*&---------------------------------------------------------------------*
*&      Form  PRUEFUNG_BETRAG
*&---------------------------------------------------------------------*
*       prüft, ob der maximal zulässige Betrag gemäß                   *
*       Formatbeschreibung nicht überschritten wird                    *
*       checks if maximum amount is not exceeded
*----------------------------------------------------------------------*
*       -> p_length     Länge des Betragsfeldes auf dem Datenträger    *
*                       length of dme field for the amount             *
*       -> p_amount     zu prüfender Betrag                            *
*                       amount to be checked                           *
*----------------------------------------------------------------------*
FORM pruefung_betrag USING p_length p_amount.

  DATA: _max_amount(18) TYPE n,
        _length         TYPE i,
        _rwbtr          LIKE reguh-rwbtr,
        _amount         LIKE reguh-rwbtr.

  _amount              = abs( p_amount ).
  _length              = p_length.
  _max_amount(_length) = _amount.
  _rwbtr               = _max_amount(_length).

  IF _rwbtr NE _amount.
    err_betrag-waers = reguh-waers.
    err_betrag-rwbtr = reguh-rwbtr.  "#EC CI_FLDEXT_OK[2610650]
    err_betrag-zbukr = reguh-zbukr.
    err_betrag-vblnr = reguh-vblnr.
    COLLECT err_betrag.
    REJECT.
  ENDIF.

ENDFORM.                               " PRUEFUNG_BETRAG

*&---------------------------------------------------------------------*
* FORM READ_SCB_INDICATOR                                              *
*&---------------------------------------------------------------------*
*   reads table T015L. If no entry was found, sy-subrc is NOT 0 and    *
*   the workarea T015L is cleared. In this case no error message is    *
*   send, the calling program has to react by itself.                  *
*----------------------------------------------------------------------*

FORM read_scb_indicator USING p_lzbkz LIKE t015l-lzbkz.

  IF p_lzbkz IS INITIAL.
    sy-subrc = 4.
    CLEAR t015l.
    EXIT.
  ENDIF.

  sy-subrc = 0.
  CHECK p_lzbkz NE t015l-lzbkz.

  READ TABLE tab_t015l INTO t015l WITH TABLE KEY lzbkz = p_lzbkz.

  IF sy-subrc NE 0.
    SELECT SINGLE * FROM t015l WHERE lzbkz = p_lzbkz.
    IF sy-subrc EQ 0.
      INSERT t015l INTO TABLE tab_t015l.
    ELSE.
      CLEAR t015l.
    ENDIF.
  ENDIF.

ENDFORM.                               "READ_LZBKZ


*&---------------------------------------------------------------------*
*&      Form  GET_VALUE_DATE
*&---------------------------------------------------------------------*
*       compute the value date if it is initial
*----------------------------------------------------------------------*
FORM get_value_date.

  CHECK reguh-valut IS INITIAL.
  reguh-valut = reguh-zaldt.
  CALL FUNCTION 'DET_VALUE_DATE_FOR_PAYMENT'
    EXPORTING
      i_bldat            = reguh-zaldt
      i_budat            = reguh-zaldt
      i_bukrs            = reguh-zbukr
      i_faedt            = reguh-zaldt
      i_hbkid            = reguh-hbkid
      i_hktid            = reguh-hktid
      i_vorgn            = ' '
*     I_WBGRU            = ' '
      i_zlsch            = reguh-rzawe
    IMPORTING
      valuta             = reguh-valut
    EXCEPTIONS
      cal_id_error       = 1
      not_found_in_t012a = 2
      not_found_in_t012c = 3
      error_in_t012c     = 4
      OTHERS             = 5.

  IF sy-subrc NE 0.
    SELECT * FROM t042v WHERE bukrs EQ reguh-zbukr
                          AND zlsch EQ reguh-rzawe
                          AND hbkid EQ reguh-hbkid
                          AND hktid EQ reguh-hktid
                          AND betrg GE reguh-rbetr.
      EXIT.
    ENDSELECT.
    IF sy-subrc EQ 0.
      reguh-valut = reguh-valut + t042v-anztg.
    ENDIF.
  ENDIF.

ENDFORM.                               " GET_VALUE_DATE
*&---------------------------------------------------------------------*
*&      Form  BERECHTIGUNG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM berechtigung.

  DATA lt_fimsg LIKE TABLE OF fimsg WITH HEADER LINE.

* Lese Fehlertabelle aus LD und konvertiere in FIMSG Format
  IF sy-tcode <> 'ZTR004'. " Tr antigua 'ZFI172'.
    CALL FUNCTION 'FI_PYF_AUTHORITY_OUTPUT'
      TABLES
        t_fimsg    = lt_fimsg
        t_err_auth = err_auth.
  ENDIF.
  LOOP AT lt_fimsg INTO fimsg.
    AT FIRST.         " to emphasize the Type (authority) of error
      ADD 1 TO cnt_error.
    ENDAT.
    IF sy-batch EQ space.
      MESSAGE ID lt_fimsg-msgid TYPE lt_fimsg-msgty
                                NUMBER lt_fimsg-msgno
                                WITH lt_fimsg-msgv1 lt_fimsg-msgv2
                                     lt_fimsg-msgv3 lt_fimsg-msgv4.
    ENDIF.
    PERFORM message USING lt_fimsg-msgno.
  ENDLOOP.


ENDFORM.                               " BERECHTIGUNG

*&---------------------------------------------------------------------*
*&      Form  information_list_heading
*&---------------------------------------------------------------------*
*       Print headings of information list
*----------------------------------------------------------------------*
*      -->I_HEADING_TYPE   output type: DME, LIST
*----------------------------------------------------------------------*
FORM information_list_heading  USING   VALUE(i_heading_type) TYPE c.

  DATA:
    l_dme_number_text(35)  TYPE c.

  CASE i_heading_type.
    WHEN 'DME'..
      IF zw_edisl EQ 'X'.
        l_dme_number_text = text_085.  " idoc
      ELSE.
        l_dme_number_text = text_084.   "file
      ENDIF.
      IF sy-batch EQ space.
        flg_local = space.
        WRITE / sy-uline.
        HIDE flg_local.
*--- table title
        WRITE /              sy-vline NO-GAP.
        WRITE AT (len_uline) text_092 COLOR COL_NORMAL INTENSIFIED ON.
        WRITE AT tab_right   sy-vline.
        HIDE flg_local.
        WRITE /              sy-uline.
        HIDE flg_local.
*--- column "text identifion"
        WRITE /              sy-vline NO-GAP.
        WRITE AT (len_col1)  text_080 COLOR COL_HEADING INTENSIFIED ON.
        WRITE AT tab1(1)     sy-vline.
*--- column "idoc number" or "file number"
        WRITE AT tab11(len_col_dme) l_dme_number_text
              COLOR COL_HEADING INTENSIFIED ON.
        WRITE AT tab_right   sy-vline.
        HIDE flg_local.
        WRITE /              sy-uline.
        HIDE flg_local.
      ELSE.
*--- in batch: message like in payment program log
        MESSAGE s065.
        WRITE text_092 TO txt_zeile.
        MESSAGE s065 WITH txt_zeile.
        CONCATENATE text_080 l_dme_number_text INTO txt_zeile
          SEPARATED BY ' / '.
        CONDENSE txt_zeile.
        MESSAGE s065 WITH txt_zeile.
        MESSAGE s064.
      ENDIF.

    WHEN 'LIST'.
      IF sy-batch EQ space.
        flg_local = space.
        WRITE / sy-uline.
        HIDE flg_local.
*--- table title
        WRITE /              sy-vline NO-GAP.
        WRITE AT (len_uline) text_090 COLOR COL_NORMAL INTENSIFIED ON.
        WRITE AT tab_right   sy-vline.
        HIDE flg_local.
        WRITE /              sy-uline.
        HIDE flg_local.
*--- column "text identifion"
        WRITE /              sy-vline NO-GAP.
        WRITE AT (len_col1)  text_080 COLOR COL_HEADING INTENSIFIED ON.
        WRITE AT tab1(1)     sy-vline.
*--- column "dataset"
        WRITE AT tab11(len_col2) text_081
              COLOR COL_HEADING INTENSIFIED ON.
        WRITE AT tab2(1)     sy-vline NO-GAP.
*--- column "spool number"
        WRITE AT tab21(len_col3) text_082
              COLOR COL_HEADING INTENSIFIED ON.
        WRITE AT tab3(1)     sy-vline.
*--- column "count"
        WRITE AT tab31(len_col4) text_083
              COLOR COL_HEADING INTENSIFIED ON.
        WRITE AT tab_right(1) sy-vline.
        HIDE flg_local.
        WRITE /               sy-uline.
        HIDE flg_local.
      ELSE.
*--- in batch: message like in payment program log
        MESSAGE s065.
        WRITE text_090 TO txt_zeile.
        MESSAGE s065 WITH txt_zeile.
        CONCATENATE text_080
                    text_081
                    text_082
                    text_083 INTO txt_zeile SEPARATED BY ' / '.
        CONDENSE txt_zeile.
        MESSAGE s065 WITH txt_zeile.
        MESSAGE s064.
      ENDIF.

  ENDCASE.

ENDFORM.                    " information_list_heading

*&---------------------------------------------------------------------*
*&      Form  information_on_dme
*&---------------------------------------------------------------------*
*       Print information about a file or idoc
*----------------------------------------------------------------------*
*      --> LS_TAB_AUSGABE  file number etc.
*----------------------------------------------------------------------*
FORM information_on_dme  USING    ls_tab_ausgabe LIKE tab_ausgabe.

  IF sy-batch EQ space.
    flg_local = 'F'.
    WRITE /             sy-vline NO-GAP.
    WRITE AT (len_col1) ls_tab_ausgabe-name
                        COLOR COL_NORMAL INTENSIFIED OFF HOTSPOT ON.
    WRITE AT tab1(1)    sy-vline NO-GAP.
    WRITE AT tab11      ls_tab_ausgabe-filename(len_col_dme)
                        RIGHT-JUSTIFIED NO-ZERO
                        COLOR COL_NORMAL INTENSIFIED OFF HOTSPOT ON.
    WRITE AT tab_right  sy-vline.
    HIDE: flg_local,
          ls_tab_ausgabe.
  ELSE.
    WRITE ls_tab_ausgabe-name TO txt_zeile.
    CONDENSE txt_zeile.
    MESSAGE s065 WITH txt_zeile ls_tab_ausgabe-filename.
  ENDIF.


ENDFORM.                    " information_on_dme

*&---------------------------------------------------------------------*
*&      Form  information_on_lists
*&---------------------------------------------------------------------*
*       Print information about payment advices and lists
*----------------------------------------------------------------------*
*      --> LS_TAB_AUSGABE  dataset, spool number, count etc.
*----------------------------------------------------------------------*
FORM information_on_lists  USING    ls_tab_ausgabe LIKE tab_ausgabe.

  DATA:
    l_count_txt(12)  TYPE c.

  IF sy-batch EQ space.
*--- online: list with links to spool
    IF ls_tab_ausgabe-error EQ 'X'.
      flg_local = 'E'.
    ELSE.
      flg_local = 'S'.
    ENDIF.

    WRITE /             sy-vline NO-GAP.
*--- column for list name
    IF tab_ausgabe-error EQ 'X'.
      WRITE AT (len_col1) ls_tab_ausgabe-name
            COLOR COL_NEGATIVE INTENSIFIED OFF HOTSPOT ON.
    ELSE.
      IF NOT ls_tab_ausgabe-spoolnr IS INITIAL.
        WRITE AT (len_col1) ls_tab_ausgabe-name
              COLOR COL_NORMAL INTENSIFIED OFF HOTSPOT ON.
      ELSE.
        WRITE AT (len_col1) ls_tab_ausgabe-name
              COLOR COL_NORMAL INTENSIFIED OFF HOTSPOT OFF.
      ENDIF.
    ENDIF.
    WRITE AT tab1 sy-vline NO-GAP.
*--- column for dataset
    WRITE AT tab11(len_col2) ls_tab_ausgabe-dataset
          COLOR COL_NORMAL INTENSIFIED OFF.
    WRITE AT tab2 sy-vline NO-GAP.
*--- column for spool number
    IF NOT ls_tab_ausgabe-spoolnr IS INITIAL.
      WRITE AT tab21(len_col3) ls_tab_ausgabe-spoolnr
            RIGHT-JUSTIFIED NO-ZERO
            COLOR COL_NORMAL INTENSIFIED OFF HOTSPOT ON.
    ELSE.
      WRITE AT tab21(len_col3) space
            COLOR COL_NORMAL INTENSIFIED OFF.
    ENDIF.
    WRITE AT tab3 sy-vline.
*--- column for advice counter
    IF NOT ls_tab_ausgabe-spoolnr IS INITIAL.
      WRITE AT tab31(len_col4) ls_tab_ausgabe-count NO-ZERO
            COLOR COL_NORMAL INTENSIFIED OFF HOTSPOT ON.
    ELSE.
      WRITE AT tab31(len_col4) ls_tab_ausgabe-count NO-ZERO
            COLOR COL_NORMAL INTENSIFIED OFF HOTSPOT OFF.
    ENDIF.
    WRITE AT tab_right sy-vline.
    HIDE: flg_local, ls_tab_ausgabe.
  ELSE.
*--- background: message for payment program log
    CONCATENATE ls_tab_ausgabe-name
                ls_tab_ausgabe-dataset
                INTO txt_zeile SEPARATED BY ' / '.
    IF NOT ls_tab_ausgabe-spoolnr IS INITIAL.
      CONCATENATE txt_zeile tab_ausgabe-spoolnr INTO txt_zeile
      SEPARATED BY ' / '.
    ENDIF.
    WRITE ls_tab_ausgabe-count TO l_count_txt NO-ZERO.
    CONCATENATE txt_zeile l_count_txt INTO txt_zeile SEPARATED BY ' / '.
    CONDENSE txt_zeile.
    IF txt_zeile+30 CA ' '.
      DATA ld_offset TYPE i.
      ld_offset = 30 + sy-fdpos.
      MESSAGE s065 WITH txt_zeile(ld_offset) txt_zeile+ld_offset.
    ELSE.
      MESSAGE s065 WITH txt_zeile(50) txt_zeile+50.
    ENDIF.
  ENDIF.


ENDFORM.                    " information_on_lists
