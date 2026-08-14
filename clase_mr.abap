METHOD bapi.
    DATA: ls_alv       TYPE ty_alv.
    DATA: lt_data      TYPE cvis_ei_extern_t,
          lt_return    TYPE bapiretm,
          ls_return    TYPE bapireti,
          ls_retmsg    TYPE LINE OF bapiretct,
          lv_text(500) TYPE c,
          ls_data      TYPE cvis_ei_extern.
    DATA: gv_insert TYPE c VALUE 'I'.
    DATA: gv_update TYPE c VALUE 'U'.
    DATA: gv_task   TYPE c .
    DATA: lv_guid        TYPE guid_32,
          lv_pguid       TYPE but000-partner_guid,
          lv_dguid       TYPE adrc-adrc_uuid,
          lv_kunnr_nif   TYPE kna1-kunnr,
          lv_kunnr_franq TYPE kna1-kunnr,
          lv_first       TYPE flag VALUE abap_true,
          lv_cuenta      TYPE akont,
          ls_soc         TYPE cmds_ei_company,
          ls_vsoc        TYPE vmds_ei_company,
          ls_paadr       TYPE bus_ei_bupa_address.
    DATA: ls_role      TYPE bus_ei_bupa_roles,
          ls_relation  TYPE burs_ei_extern,

          ls_telefono1 TYPE bus_ei_bupa_telephone,
          ls_fax1      TYPE bus_ei_bupa_fax,
          ls_taxid     TYPE bus_ei_bupa_taxnumber,
          ls_mail      TYPE bus_ei_bupa_smtp,
          ls_sales     TYPE cmds_ei_sales,
          ls_funtions  TYPE cmds_ei_functions,
          ls_vfuntions TYPE vmds_ei_functions,
          lv_parvw     TYPE parvw,
          ls_tax       TYPE cmds_ei_tax_ind.
    DATA: ls_card TYPE bus_ei_bupa_creditcard.
    DATA: ls_uri TYPE bus_ei_bupa_uri.
    DATA: ls_bank TYPE bus_ei_bupa_bankdetail.
    DATA: lv_flag_paadr TYPE flag.
    DATA: ls_wtax_type TYPE vmds_ei_wtax_type.
    DATA: lv_lines TYPE i.
    DATA: ls_vsales TYPE vmds_ei_purchasing.
    DATA: ls_industries TYPE bus_ei_bupa_industrysector.

    DATA: ls_lfbw TYPE lfbw .
    DESCRIBE TABLE it_bps LINES lv_lines.
    rv_error = abap_true.

    IF gt_sales_data IS NOT INITIAL.
      LOOP AT gt_sales_data ASSIGNING FIELD-SYMBOL(<fs_sales_data>).
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_sales_data>-partner
          IMPORTING
            output = <fs_sales_data>-partner.
      ENDLOOP.
      SELECT * INTO TABLE @DATA(lt_sales_customer) FROM knvv FOR ALL ENTRIES IN @gt_sales_data WHERE kunnr = @gt_sales_data-partner.
    ENDIF.
    IF gt_vsales IS NOT INITIAL.
      LOOP AT gt_vsales ASSIGNING FIELD-SYMBOL(<fs_vsales_data>).
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_vsales_data>-lifnr
          IMPORTING
            output = <fs_vsales_data>-lifnr.
      ENDLOOP.
      SELECT * INTO TABLE @DATA(lt_sales_vendor)   FROM lfm1 FOR ALL ENTRIES IN @gt_vsales WHERE lifnr = @gt_vsales-lifnr.
    ENDIF.
    IF it_bps IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(lt_grupo)
        FROM tb001 JOIN nriv ON nriv~nrrangenr = tb001~nrrng
        FOR ALL ENTRIES IN @it_bps
        WHERE bu_group = @it_bps-bp_group AND nriv~object = 'BU_PARTNER'.
    ENDIF.

    LOOP AT it_bps INTO DATA(ls_general_data).
      DATA(tabix) = sy-tabix.

*    PERFORM reloj USING lv_lines tabix TEXT-r01.
      CLEAR:
      lt_return[], lt_data[], ls_role,  ls_relation, ls_paadr, ls_telefono1,
      ls_fax1, ls_taxid, ls_mail, ls_sales,  ls_funtions,  lv_parvw,
      ls_tax, ls_data, ls_card, ls_uri, ls_vsoc, ls_vsales, ls_industries,
      lv_pguid, lv_dguid, lv_flag_paadr,ls_wtax_type, ls_data.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_general_data-partner
        IMPORTING
          output = ls_general_data-partner.

      SELECT SINGLE partner_guid INTO lv_pguid  FROM but000
          WHERE partner = ls_general_data-partner.
      SELECT partner_guid, address_guid INTO TABLE @DATA(lt_direccion) FROM but000
        JOIN but020 ON but000~partner = but020~partner
       WHERE but000~partner = @ls_general_data-partner ORDER BY addr_valid_to DESCENDING .
      IF sy-subrc <> 0.
        SELECT SINGLE partner_guid INTO lv_pguid  FROM but000
           WHERE partner = ls_general_data-partner.
      ELSE.
        TRY.
            DATA(ls_direccion) = lt_direccion[ 1 ].
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_general_data-partner
        IMPORTING
          output = ls_general_data-partner.

      IF lv_pguid IS NOT INITIAL .
        gv_task =  gv_update.
      ELSE.
        gv_task = gv_insert.
        CALL METHOD cl_system_uuid=>if_system_uuid_static~create_uuid_c32
          RECEIVING
            uuid = lv_pguid.
        CALL METHOD cl_system_uuid=>if_system_uuid_static~create_uuid_c32
          RECEIVING
            uuid = lv_dguid.
      ENDIF.

      ls_data-partner-header-object_task = gv_task.
      TRY.
          DATA(ls_grupo) = lt_grupo[ tb001-bu_group = ls_general_data-bp_group ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
      IF ls_grupo-nriv-externind = abap_true.
        ls_data-partner-header-object_instance-bpartner = ls_general_data-partner.
      ENDIF.
      ls_data-partner-header-object_instance-bpartnerguid = lv_pguid.
      CLEAR: ls_data-partner-central_data-common-data-bp_control.
      ls_data-partner-central_data-common-data-bp_control-grouping = ls_general_data-bp_group.
      ls_data-partner-central_data-common-data-bp_control-category = '2'. "1 2 o 3 obligatorio.
      CLEAR: ls_data-partner-central_data-common-data-bp_centraldata.
      ls_data-partner-central_data-common-data-bp_centraldata-partnertype = ls_general_data-bpkind.
      CLEAR: ls_data-partner-central_data-common-data-bp_organization.
      ls_data-partner-central_data-common-data-bp_organization-name1 = ls_general_data-name_first.
      ls_data-partner-central_data-common-datax-bp_organization-name1 = abap_true. "obligatorio
      ls_data-partner-central_data-common-data-bp_organization-name2 = ls_general_data-name_last.
      ls_data-partner-central_data-common-data-bp_organization-name3 = ls_general_data-name3.
      ls_data-partner-central_data-common-data-bp_organization-name4 = ls_general_data-name4.
      ls_data-partner-central_data-common-data-bp_organization-liquidationdate = ls_general_data-liquid_dat.
      ls_data-partner-central_data-common-data-bp_organization-foundationdate = ls_general_data-found_dat.
      ls_data-partner-central_data-common-data-bp_centraldata-searchterm1 = ls_general_data-sortl.
      ls_data-partner-central_data-common-data-bp_centraldata-searchterm2 = ls_general_data-mcod2.
      ls_data-partner-central_data-common-data-bp_centraldata-authorizationgroup = '0001'. "ls_general_data-augrp. " ANN
      CLEAR: ls_data-partner-central_data-common-data-bp_person.
      ls_data-partner-central_data-common-data-bp_person-lastname = ls_general_data-name_last.
      ls_data-partner-central_data-common-data-bp_person-firstname = ls_general_data-name_first.

      ls_data-partner-central_data-common-datax-bp_centraldata-partnertype = abap_true.
      ls_data-partner-central_data-common-datax-bp_organization-name2 = abap_true.
      ls_data-partner-central_data-common-datax-bp_organization-name3 = abap_true.
      ls_data-partner-central_data-common-datax-bp_organization-name4 = abap_true.
      ls_data-partner-central_data-common-datax-bp_organization-liquidationdate = abap_true.
      ls_data-partner-central_data-common-datax-bp_organization-foundationdate = abap_true.
      ls_data-partner-central_data-common-datax-bp_centraldata-searchterm1 = abap_true.
      ls_data-partner-central_data-common-datax-bp_centraldata-searchterm2 = abap_true.
      ls_data-partner-central_data-common-datax-bp_person-lastname = abap_true.
      ls_data-partner-central_data-common-datax-bp_person-firstname = abap_true.
      ls_data-partner-central_data-common-datax-bp_centraldata-authorizationgroup = abap_true.
      LOOP AT gt_bank_data INTO DATA(ls_bank_data) WHERE partner = ls_general_data-partner.
        CLEAR: ls_bank.
        IF ls_bank_data-iban IS INITIAL.
          CALL FUNCTION 'CONVERT_BANK_ACCOUNT_2_IBAN'
            EXPORTING
              i_bank_account     = ls_bank_data-bank_acct
              i_bank_control_key = ls_bank_data-ctrl_key
              i_bank_country     = ls_bank_data-bank_ctry
              i_bank_number      = ls_bank_data-bank_key
              i_bank_key         = ls_bank_data-bank_key
            IMPORTING
              e_iban             = ls_bank_data-iban
            EXCEPTIONS
              no_conversion      = 1
              OTHERS             = 2.
        ENDIF.
        ls_data-partner-central_data-bankdetail-current_state = abap_true.
        ls_bank-task = gv_task.
        ls_bank-data_key = ls_bank_data-bu_move_bkvid.
        ls_bank-data-bank_ctry = ls_bank_data-bank_ctry.
        ls_bank-data-bank_key = ls_bank_data-bank_key.
        ls_bank-data-iban = ls_bank_data-iban.
        ls_bank-data-ctrl_key = ls_bank_data-ctrl_key.
        ls_bank-data-coll_auth = ls_bank_data-coll_auth.
        ls_bank-data-bank_acct = ls_bank_data-bank_acct.
        ls_bank-data-accountholder = ls_bank_data-accountholder.
        ls_bank-datax-bank_ctry =  abap_true.
        ls_bank-datax-bank_key = abap_true.
        ls_bank-datax-iban = abap_true.
        ls_bank-datax-ctrl_key = abap_true.
        ls_bank-datax-coll_auth = abap_true.
        ls_bank-datax-bank_acct = abap_true.
        ls_bank-datax-accountholder = abap_true.
        ls_bank-data-bank_ref =  ls_bank_data-bkref.
        ls_bank-datax-bank_ref =  abap_true.
        ls_bank-datax-bankdetailmoveid = abap_true.
        APPEND ls_bank TO ls_data-partner-central_data-bankdetail-bankdetails.
      ENDLOOP.



      IF ls_general_data-country IS NOT INITIAL.
        ls_paadr-data-postal-data-country = ls_general_data-country.
        ls_paadr-data-postal-datax-country = abap_true.
        lv_flag_paadr = abap_true.
      ENDIF.
      IF ls_general_data-city1 IS NOT INITIAL.
        ls_paadr-data-postal-data-city = ls_general_data-city1.
        ls_paadr-data-postal-datax-city  = abap_true.
        lv_flag_paadr = abap_true.
      ENDIF.
      IF ls_general_data-post_code1 IS NOT INITIAL.
        ls_paadr-data-postal-data-postl_cod1 = ls_general_data-post_code1.
        ls_paadr-data-postal-datax-postl_cod1  = abap_true.
        lv_flag_paadr = abap_true.
      ENDIF.
      IF ls_general_data-street IS NOT INITIAL.
        ls_paadr-data-postal-data-street = ls_general_data-street.
        ls_paadr-data-postal-datax-street = abap_true.
        lv_flag_paadr = abap_true.
      ENDIF.
      IF ls_general_data-str_suppl1 IS NOT INITIAL.
        ls_paadr-data-postal-data-str_suppl1 = ls_general_data-str_suppl1.
        ls_paadr-data-postal-datax-str_suppl1 = abap_true.
        lv_flag_paadr = abap_true.
      ENDIF.
      IF ls_general_data-str_suppl2 IS NOT INITIAL.
        ls_paadr-data-postal-data-str_suppl2 = ls_general_data-str_suppl2.
        ls_paadr-data-postal-datax-str_suppl2 = abap_true.
        lv_flag_paadr = abap_true.
      ENDIF.
      IF ls_general_data-str_suppl3 IS NOT INITIAL.
        ls_paadr-data-postal-data-str_suppl3 = ls_general_data-str_suppl3.
        ls_paadr-data-postal-datax-str_suppl3 = abap_true.
        lv_flag_paadr = abap_true.
      ENDIF.
      IF ls_general_data-location IS NOT INITIAL.
        ls_paadr-data-postal-data-location = ls_general_data-location.
        ls_paadr-data-postal-datax-location = abap_true.
        lv_flag_paadr = abap_true.
      ENDIF.
      IF ls_general_data-langu_corr IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
          EXPORTING
            input            = ls_general_data-langu_corr
          IMPORTING
            output           = ls_paadr-data-postal-data-langu
          EXCEPTIONS
            unknown_language = 1
            OTHERS           = 2.
        ls_paadr-data-postal-datax-langu = abap_true.
        lv_flag_paadr = abap_true.
        ls_paadr-data-postal-data-languiso = ls_general_data-langu_corr.
      ENDIF.
      IF ls_general_data-region IS NOT INITIAL.
        ls_paadr-data-postal-data-po_box_reg = ls_general_data-region.
        ls_paadr-data-postal-datax-po_box_reg = abap_true.
        lv_flag_paadr = abap_true.
      ENDIF.

      "email
      IF ls_general_data-smtp_addr IS NOT INITIAL.
        CLEAR: ls_mail.
        ls_mail-contact-task  = gv_task.
        ls_mail-contact-data-e_mail = ls_general_data-smtp_addr.
*        ls_mail-contact-datax-e_mail = abap_true." ADD JGE
        ls_paadr-data-communication-smtp-current_state =  abap_true.
        APPEND ls_mail TO ls_paadr-data-communication-smtp-smtp.
        lv_flag_paadr = abap_true.
      ENDIF.

      "telefono
      IF ls_general_data-telnr_long IS NOT INITIAL.
        ls_paadr-data-communication-phone-current_state = abap_true.
        ls_telefono1-contact-task = gv_task.
*    ls_telefono1-contact-data-countryiso = im_data-countryiso.
        ls_telefono1-contact-data-telephone = ls_general_data-telnr_long.
        ls_telefono1-contact-datax-telephone = abap_true.
        ls_telefono1-contact-datax-countryiso = abap_true.
        APPEND ls_telefono1 TO ls_paadr-data-communication-phone-phone.
        lv_flag_paadr = abap_true.
      ENDIF.

      "INI JGE - DTT 26.06.2026
      "perosna contacto ????????
*      IF ls_general_data-name3 IS NOT INITIAL. "?????
*        ls_paadr-data-postal-data-c_o_name  = ls_general_data-name3.
*        ls_paadr-data-postal-datax-c_o_name = abap_true.
*        lv_flag_paadr = abap_true.
*      ENDIF.
      " Móvil
*      IF ls_general_data-mobile_long IS NOT INITIAL.
*        CLEAR ls_telefono1.
*
*        ls_paadr-data-communication-phone-current_state = abap_true.
*
*        ls_telefono1-contact-task = COND #( WHEN gv_task = gv_insert THEN gv_insert ELSE gv_insert ).
*        ls_telefono1-contact-data-telephone = ls_general_data-mobile_long.
*        ls_telefono1-contact-data-r_3_user  = '3'. "móvil
*
*        ls_telefono1-contact-datax-telephone = abap_true.
*        ls_telefono1-contact-datax-r_3_user  = abap_true.
*
*        APPEND ls_telefono1 TO ls_paadr-data-communication-phone-phone.
*
*        lv_flag_paadr = abap_true.
*      ENDIF.
      "FIN JGE - DTT 26.06.2026

      "fax
      IF ls_general_data-faxnr_long IS NOT INITIAL.
        CLEAR: ls_fax1.
        ls_paadr-data-communication-fax-current_state = abap_true.
        ls_fax1-contact-task = gv_task.
*    ls_fax1-contact-data-countryiso = im_data-countryiso.
        ls_fax1-contact-data-fax = ls_general_data-faxnr_long.
        ls_fax1-contact-datax-fax = abap_true.
        ls_fax1-contact-datax-countryiso = abap_true.
        APPEND ls_fax1 TO ls_paadr-data-communication-fax-fax.
        ls_paadr-currently_valid = abap_true.
        lv_flag_paadr = abap_true.
      ENDIF.

      "url
      IF ls_general_data-uri_addr IS NOT INITIAL.
        CLEAR: ls_uri.
        ls_paadr-data-communication-uri-current_state = abap_true.
        ls_uri-contact-task = gv_task.
        ls_uri-contact-data-uri = ls_general_data-uri_addr.
        ls_uri-contact-data-uri_type = ls_general_data-uri_typ.
        ls_uri-contact-datax-uri = abap_true.
        ls_uri-contact-datax-uri_type = abap_true.
        APPEND ls_uri TO ls_paadr-data-communication-uri-uri.
        lv_flag_paadr = abap_true.
      ENDIF.

      IF  lv_flag_paadr = abap_true.
        IF ls_direccion-address_guid IS INITIAL.
          ls_paadr-task = gv_insert.
          CALL METHOD cl_system_uuid=>if_system_uuid_static~create_uuid_c32
            RECEIVING
              uuid = lv_dguid.
        ELSE.
          lv_dguid = ls_direccion-address_guid.
          ls_paadr-task = gv_task.
        ENDIF.

        ls_data-partner-central_data-address-current_state = abap_true.
        ls_paadr-data_key-guid = lv_dguid.
        ls_paadr-data_key-operation = 'XXDFLT'.
        APPEND ls_paadr TO ls_data-partner-central_data-address-addresses.
      ENDIF.

      CLEAR: ls_data-partner-central_data-paycard-paycards[].
      LOOP AT gt_tarjetas INTO DATA(ls_tarjetas) WHERE partner = ls_general_data-partner.

        IF gv_task = gv_update.
          DATA: lt_return1 TYPE TABLE OF bapiret2 INITIAL SIZE 0.

*        DATA: lt_return1 LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
          DATA: lv_partner  TYPE  bapibus1006_head-bpartner.
          DATA: lv_idcar  TYPE bapibus1006_pcard_details-card_id.
          DATA: ls_carddetail  TYPE  bapibus1006_pcard_data.
          CLEAR: ls_carddetail.
          IF ls_grupo-nriv-externind = abap_true.
            lv_partner = ls_general_data-partner.
          ENDIF.

          lv_idcar = ls_tarjetas-run_id.
          ls_carddetail-card_number = ls_tarjetas-ccnum.
          ls_carddetail-card_type = ls_tarjetas-ccin.
          CALL FUNCTION 'BAPI_BUPA_PCARD_CHANGE'
            EXPORTING
              businesspartner = lv_partner
              card_id         = lv_idcar
              carddetaildata  = ls_carddetail
*             CARDDETAILDATA_X       =
            TABLES
              return          = lt_return1.

          CALL FUNCTION 'BAPI_BUPA_PCARD_ADD'
            EXPORTING
              businesspartner = lv_partner
              card_id         = lv_idcar
              data            = ls_carddetail
*          IMPORTING
*             card_id_out     =
            TABLES
              return          = lt_return1.

          ls_data-partner-central_data-paycard-current_state = abap_true.
          ls_card-task = 'I'.
          ls_card-data_key = ls_tarjetas-run_id.
          ls_card-data-card_type = ls_tarjetas-ccin.
          ls_card-data-card_number = ls_tarjetas-ccnum.
          APPEND ls_card TO ls_data-partner-central_data-paycard-paycards.

        ELSE.



          IF ls_tarjetas-ccin IS NOT INITIAL AND ls_tarjetas-ccnum IS NOT INITIAL.
            ls_data-partner-central_data-paycard-current_state = abap_true.
            ls_card-task = gv_task.
            ls_card-data_key = ls_tarjetas-run_id.
            ls_card-data-card_type = ls_tarjetas-ccin.
            ls_card-data-card_number = ls_tarjetas-ccnum.
            APPEND ls_card TO ls_data-partner-central_data-paycard-paycards.
          ENDIF.

        ENDIF.
      ENDLOOP.

      CLEAR: ls_data-partner-central_data-taxnumber-taxnumbers[].
      LOOP AT it_taxnum_data INTO DATA(ls_taxnum) WHERE partner = ls_general_data-partner.
        CLEAR: ls_taxid.
        ls_data-partner-central_data-taxnumber-current_state = abap_true.
        ls_taxid-task  = gv_task.
        ls_taxid-data_key-taxnumber = ls_taxnum-taxnum.
        ls_taxid-data_key-taxtype =  ls_taxnum-taxtype.
        APPEND ls_taxid TO ls_data-partner-central_data-taxnumber-taxnumbers.
      ENDLOOP.

      CLEAR: ls_data-partner-central_data-role-roles[].
*    IF ch_kunnr = abap_true.
      "roles
      ls_data-partner-central_data-role-current_state = abap_true.
      CLEAR: ls_role.
      ls_role-task = gv_task.
      ls_role-data_key = 'FLCU01'.
      ls_role-data-rolecategory = 'FLCU01'.
      ls_role-data-valid_from = sy-datum.
      ls_role-data-valid_to = '99991231'.
      ls_role-currently_valid = abap_true.
      ls_role-datax-valid_from = abap_true.
      ls_role-datax-valid_to = abap_true.
      APPEND ls_role TO ls_data-partner-central_data-role-roles.


      ls_data-partner-central_data-role-current_state = abap_true.
      CLEAR: ls_role.
      ls_role-task = gv_task.
      ls_role-data_key = 'FLCU00'.
      ls_role-data-rolecategory = 'FLCU00'.
      ls_role-data-valid_from = sy-datum.
      ls_role-data-valid_to = '99991231'.
      ls_role-currently_valid = abap_true.
      ls_role-datax-valid_from = abap_true.
      ls_role-datax-valid_to = abap_true.
      APPEND ls_role TO ls_data-partner-central_data-role-roles.

      "customer
      IF gv_task = gv_update.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_general_data-partner
          IMPORTING
            output = ls_general_data-partner.
        SELECT SINGLE kunnr INTO @DATA(ls_kunnr) FROM kna1 WHERE kunnr = @ls_general_data-partner.

        IF sy-subrc <> 0.
          ls_data-customer-header-object_task = gv_insert.
        ELSE.
          ls_data-customer-header-object_task = gv_task.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = ls_general_data-partner
          IMPORTING
            output = ls_general_data-partner.
      ELSE.
        ls_data-customer-header-object_task = gv_task.
      ENDIF.
      ls_data-customer-sales_data-current_state = abap_true.
      IF ls_grupo-nriv-externind = abap_true.
        ls_data-customer-header-object_instance-kunnr = ls_general_data-partner.
      ENDIF.

      ls_data-customer-central_data-central-data-knrza = ls_general_data-knrza.
      ls_data-customer-central_data-central-data-lifnr = ls_general_data-third.
      ls_data-customer-central_data-central-data-vbund = ls_general_data-vbund.
      ls_data-customer-central_data-central-data-ktokd = ls_general_data-ktokd.
      ls_data-customer-central_data-central-datax-knrza = abap_true.
      ls_data-customer-central_data-central-datax-lifnr = abap_true.
      ls_data-customer-central_data-central-datax-vbund = abap_true.
      ls_data-customer-central_data-central-datax-ktokd = abap_true.

      ls_data-customer-central_data-central-data-niels = ls_general_data-niels.
      ls_data-customer-central_data-central-datax-niels = abap_true.

      CLEAR: ls_data-customer-sales_data-sales[],
      ls_sales-functions-functions[].
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_general_data-partner
        IMPORTING
          output = ls_general_data-partner.
      LOOP AT gt_sales_data INTO DATA(ls_sales_data) WHERE partner = ls_general_data-partner.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_general_data-partner
          IMPORTING
            output = ls_general_data-partner.

        TRY.
            DATA(ls_sales_customer) = lt_sales_customer[ kunnr = ls_general_data-partner
                                                         vkorg = ls_sales_data-vkorg
                                                         vtweg = ls_sales_data-vtweg
                                                         spart = ls_sales_data-spart ].
          CATCH cx_sy_itab_line_not_found.
            CLEAR: ls_sales_customer.
        ENDTRY.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = ls_general_data-partner
          IMPORTING
            output = ls_general_data-partner.


        CLEAR: ls_sales.
        IF ls_sales_customer IS INITIAL.
          ls_sales-task =  gv_insert.
        ELSE.
          ls_sales-task =  gv_update.
        ENDIF.
        ls_sales-data-kalks = '1'. "ls_sales_data-kalks. " ANN
        ls_sales-data-waers = ls_sales_data-waers.
        ls_sales-data-kurst = 'EURX'.
        ls_sales-data-ktgrd = ls_sales_data-ktgrd.
        ls_sales-data-kdgrp = ls_sales_data-kdgrp.
        ls_sales-data-vkbur = ls_sales_data-vkbur.
        ls_sales-data-vkgrp = ls_sales_data-vkgrp.
        ls_sales-data-pltyp = ls_sales_data-pltyp.
        ls_sales-data-vwerk = ls_sales_data-vwerk.
        ls_sales-data-perfk = ls_sales_data-perfk.
        ls_sales-data-inco1 = ls_sales_data-inco1.
        ls_sales-data-inco2_l = ls_sales_data-inco2_l.
        ls_sales-data-aufsd = ls_sales_data-aufsd.
        ls_sales-data-lifsd = ls_sales_data-lifsd.
        ls_sales-data-faksd = ls_sales_data-faksd.
        ls_sales-data-zterm = ls_sales_data-zterm.
        ls_sales-data_key-spart = ls_sales_data-spart.
        ls_sales-data_key-vkorg = ls_sales_data-vkorg.
        ls_sales-data_key-vtweg = ls_sales_data-vtweg.

        ls_sales-datax-kalks = abap_true.
        ls_sales-datax-waers = abap_true.
        ls_sales-datax-ktgrd = abap_true.
        ls_sales-datax-kurst = abap_true.
        ls_sales-datax-zterm = abap_true.
        ls_sales-datax-kdgrp = abap_true.
        ls_sales-datax-vkbur = abap_true.
        ls_sales-datax-vkgrp = abap_true.
        ls_sales-datax-pltyp = abap_true.
        ls_sales-datax-vwerk = abap_true.
        ls_sales-datax-perfk = abap_true.
        ls_sales-datax-inco1 = abap_true.
        ls_sales-datax-inco2_l = abap_true.
        ls_sales-datax-aufsd = abap_true.
        ls_sales-datax-lifsd = abap_true.
        ls_sales-datax-faksd = abap_true.

        ls_sales-functions-current_state = abap_true.
        LOOP AT gt_partner_data INTO DATA(ls_partner_data) WHERE partner = ls_general_data-partner AND vkorg = ls_sales_data-vkorg AND vtweg = ls_sales_data-vtweg
            AND spart = ls_sales_data-spart.
          CLEAR: ls_funtions.
          ls_funtions-task = gv_task.
          lv_parvw = ls_partner_data-parvw.
          CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
            EXPORTING
              input  = lv_parvw
            IMPORTING
              output = lv_parvw.
          ls_funtions-data_key-parvw = lv_parvw.
          ls_funtions-data_key-parza = '001'.

          IF ls_grupo-nriv-externind = abap_true.
            ls_funtions-data-partner = ls_partner_data-partner1.
          ENDIF.
          ls_funtions-datax-partner = abap_true.
          APPEND ls_funtions TO ls_sales-functions-functions.
        ENDLOOP.
        APPEND ls_sales TO ls_data-customer-sales_data-sales.
      ENDLOOP.
      CLEAR: ls_data-customer-central_data-tax_ind-tax_ind[].
      LOOP AT gt_taxclass_data INTO DATA(ls_taxclas) WHERE  partner = ls_general_data-partner.
        CLEAR: ls_tax.
        ls_data-customer-central_data-tax_ind-current_state = abap_true.
        ls_tax-task  = gv_task.
        ls_tax-data-taxkd = ls_taxclas-taxkd.
        ls_tax-data_key-aland = ls_taxclas-aland.
        ls_tax-data_key-tatyp = ls_taxclas-tatyp.
        ls_tax-datax-taxkd = abap_true.
        APPEND ls_tax TO  ls_data-customer-central_data-tax_ind-tax_ind.
      ENDLOOP.

      CLEAR: ls_data-customer-company_data-company[].
      LOOP AT gt_sociedad_data INTO DATA(ls_soc_data) WHERE partner = ls_general_data-partner.
        CLEAR: ls_soc.
        lv_cuenta = ls_soc_data-akont.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_cuenta
          IMPORTING
            output = lv_cuenta.

        ls_data-customer-company_data-current_state = abap_true.
        ls_soc-task = gv_task.
        ls_soc-data-sperr = ls_soc_data-sperr.
        ls_soc-data-zahls = ls_soc_data-zahls.
        ls_soc-data-xverr = ls_soc_data-xverr.
        ls_soc-data-knrzb = ls_soc_data-knrzb.
        ls_soc-data-altkn = ls_soc_data-altkn.
        ls_soc-data-fdgrv = ls_soc_data-fdgrv.
        ls_soc-data-zsabe = ls_soc_data-zsabe.
        ls_soc-data-intad = ls_soc_data-intad.
        ls_soc-data-akont = lv_cuenta.
        ls_soc-data-zwels = ls_soc_data-zwels.
        ls_soc-data-zterm = ls_soc_data-zterm.
        ls_soc-datax-akont = abap_true.
        ls_soc-datax-zwels = abap_true.
        ls_soc-datax-zterm = abap_true.
        ls_soc-datax-sperr = abap_true.
        ls_soc-datax-zahls = abap_true.
        ls_soc-datax-xverr = abap_true.
        ls_soc-datax-knrzb = abap_true.
        ls_soc-datax-altkn = abap_true.
        ls_soc-datax-fdgrv = abap_true.
        ls_soc-datax-zsabe = abap_true.
        ls_soc-datax-intad = abap_true.
        ls_soc-data_key-bukrs = ls_soc_data-bukrs.
        APPEND ls_soc TO ls_data-customer-company_data-company.
      ENDLOOP.


      IF gv_task = gv_insert .
        ls_data-ensure_create-create_customer = abap_true.
      ELSEIF  ls_data-customer-header-object_task = gv_insert AND  ls_data-vendor-header-object_task = gv_insert.
        ls_data-ensure_create-create_customer = abap_true.
      ELSEIF ls_data-customer-header-object_task = gv_insert.
        ls_data-ensure_create-create_customer = abap_true.
      ELSEIF ls_data-vendor-header-object_task = gv_insert.
      ENDIF.

      APPEND ls_data TO lt_data.


      cl_md_bp_maintain=>maintain(
        EXPORTING i_data = lt_data
        IMPORTING e_return = lt_return ).

      IF lt_return IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
        rv_error = abap_false.

      ELSE.
        rv_error = abap_false.
        CLEAR: ev_message.
        LOOP AT lt_return INTO ls_return.
          LOOP AT ls_return-object_msg INTO ls_retmsg WHERE type = 'E' OR type = 'A'.
            rv_error = abap_true.
            MESSAGE ID ls_retmsg-id TYPE 'S' NUMBER ls_retmsg-number INTO lv_text
                        WITH ls_retmsg-message_v1 ls_retmsg-message_v2 ls_retmsg-message_v3 ls_retmsg-message_v4.

            CONCATENATE ev_message lv_text INTO ev_message SEPARATED BY space.
          ENDLOOP.
        ENDLOOP.
        IF rv_error = abap_true.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ELSE. "son warnigs
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ENDIF.
      ENDIF.

      REFRESH: lt_return.
    ENDLOOP.

  ENDMETHOD. 
