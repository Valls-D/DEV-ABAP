  METHOD map_bp_data.

    CONSTANTS: gc_group TYPE c LENGTH 4 VALUE 'ZPCT'.

    FIELD-SYMBOLS: <fs_role>        TYPE bus_ei_bupa_roles,
                   <fs_taxnumber>   TYPE bus_ei_bupa_taxnumber,
                   <fs_taxnumber_2> TYPE bus_ei_bupa_taxnumber,
                   <fs_dir>         TYPE bus_ei_bupa_address,
                   <fs_phone>       TYPE bus_ei_bupa_telephone,
                   <fs_smtp>        TYPE bus_ei_bupa_smtp,
                   <fs_addr_usage>  TYPE bus_ei_bupa_addressusage,
                   <fs_company>     TYPE vmds_ei_company,
                   <fs_purchasing>  TYPE vmds_ei_purchasing,
                   <fs_sales>       TYPE cmds_ei_sales,
                   <fs_function>    TYPE cmds_ei_functions,
                   <fs_relation>    TYPE burs_ei_extern,
                   <fs_bankdetail>  TYPE bus_ei_bupa_bankdetail,
                   <fs_ct_address>  TYPE burs_ei_rel_address,
                   <fs_text>        TYPE cvis_ei_text,
                   <fs_line>        TYPE tline.

    DATA: ls_intervalo         TYPE zca_intervalos,
          ls_centraldata       TYPE bapibus1006_central,
          ls_centraldataperson TYPE bapibus1006_central_person,
          lt_ct_partner        TYPE zcacl_intervalos_bp=>tyt_partner,
          lv_it                TYPE i,
          lt_telf              TYPE TABLE OF bapiadtel,
          lt_email             TYPE TABLE OF bapiadsmtp,
          ls_addressdata       TYPE bapibus1006_address,
          lv_bankn             TYPE bankn35,
          ls_texts             TYPE zca_sol_textos,
          lv_text              TYPE string.

    cs_cvis_ei_extern-partner-header-object_task = action.
    cs_cvis_ei_extern-partner-header-object_instance-bpartner = is_sol-sol_head-partner.

    cs_cvis_ei_extern-partner-central_data-common-data-bp_control-category        = '2'.           "Tipo de socio comercial
    cs_cvis_ei_extern-partner-central_data-common-data-bp_control-grouping        = gc_zpgl.       "Clase de IC
    cs_cvis_ei_extern-partner-central_data-common-data-bp_centraldata-partnertype = gc_zpgl.

*    Rol Cliente (FLCU01)
    APPEND INITIAL LINE TO cs_cvis_ei_extern-partner-central_data-role-roles ASSIGNING <fs_role>.
    <fs_role>-task = action.
    <fs_role>-data_key = 'FLCU01'.
*    Rol Proveedor (FLVN00)
    APPEND INITIAL LINE TO cs_cvis_ei_extern-partner-central_data-role-roles ASSIGNING <fs_role>.
    <fs_role>-task = action.
    <fs_role>-data_key = 'FLVN00'.
*    Rol Proveedor (FLVN01)
    APPEND INITIAL LINE TO cs_cvis_ei_extern-partner-central_data-role-roles ASSIGNING <fs_role>.
    <fs_role>-task = action.
    <fs_role>-data_key = 'FLVN01'.

*------DATOS_GENERALES------*

    cs_cvis_ei_extern-partner-central_data-common-data-bp_organization-name1        = is_sol-sol_data-razon_social.     "Raz�n social
*    cs_cvis_ei_extern-partner-central_data-common-data-bp_centraldata-searchterm1   = is_sol-sol_data-nombre_comercial. "Nombre comercial
    cs_cvis_ei_extern-partner-central_data-common-datax-bp_organization-name1       = abap_true.
*    cs_cvis_ei_extern-partner-central_data-common-datax-bp_centraldata-searchterm1  = abap_true.

*    TIPO_NIF / NIF
    APPEND INITIAL LINE TO cs_cvis_ei_extern-partner-central_data-taxnumber-taxnumbers ASSIGNING <fs_taxnumber>.
    <fs_taxnumber>-task = action.
    <fs_taxnumber>-data_key-taxtype   = is_sol-sol_data-tipo_de_nif.
    <fs_taxnumber>-data_key-taxnumber = is_sol-sol_data-nif.

*    TIPO_NIF / NIF 2
    IF is_sol-sol_data-nif2 IS NOT INITIAL. "Si est� �nicamente informado
      APPEND INITIAL LINE TO cs_cvis_ei_extern-partner-central_data-taxnumber-taxnumbers ASSIGNING <fs_taxnumber_2>.
      <fs_taxnumber_2>-task = action.
      <fs_taxnumber_2>-data_key-taxtype   = is_sol-sol_data-tipo_de_nif2.
      <fs_taxnumber_2>-data_key-taxnumber = is_sol-sol_data-nif2.
    ENDIF.

*------DIRECCIONES------*

*--Direcci�n Est�ndar---*

    "Cambiar el Idioma a un D�gito.
    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        input  = is_sol-sol_data-idioma
      IMPORTING
        output = is_sol-sol_data-idioma.

    APPEND INITIAL LINE TO cs_cvis_ei_extern-partner-central_data-address-addresses ASSIGNING <fs_dir>.
    <fs_dir>-task = action.

    <fs_dir>-data-postal-data-street      = is_sol-sol_data-street.      "Calle
    <fs_dir>-data-postal-data-postl_cod1  = is_sol-sol_data-post_code1.  "C�digo postal
    <fs_dir>-data-postal-data-city        = is_sol-sol_data-city.        "Poblaci�n
    <fs_dir>-data-postal-data-country     = is_sol-sol_data-country.     "Pa�s
    <fs_dir>-data-postal-data-region      = is_sol-sol_data-region.      "Regi�n
    <fs_dir>-data-postal-data-langu       = is_sol-sol_data-idioma.      "Idioma
    <fs_dir>-data-postal-datax-street     = abap_true.
    <fs_dir>-data-postal-datax-postl_cod1 = abap_true.
    <fs_dir>-data-postal-datax-city       = abap_true.
    <fs_dir>-data-postal-datax-country    = abap_true.
    <fs_dir>-data-postal-datax-region     = abap_true.
    <fs_dir>-data-postal-datax-langu      = abap_true.

*    Tel�fono
    APPEND INITIAL LINE TO <fs_dir>-data-communication-phone-phone ASSIGNING <fs_phone>.
    <fs_phone>-contact-task = action.
    <fs_phone>-contact-data-telephone   = is_sol-sol_data-mob_number.
    <fs_phone>-contact-data-r_3_user    = '3'.
    <fs_phone>-contact-datax-telephone  = abap_true.
    <fs_phone>-contact-datax-r_3_user   = abap_true.

*    Correo
    APPEND INITIAL LINE TO <fs_dir>-data-communication-smtp-smtp ASSIGNING <fs_smtp>.
    <fs_smtp>-contact-task = action.
    <fs_smtp>-contact-data-e_mail   = is_sol-sol_data-smtp_address.
    <fs_smtp>-contact-datax-e_mail  = abap_true.

*    Utilizaci�n de direcci�n
    APPEND INITIAL LINE TO <fs_dir>-data-addr_usage-addr_usages ASSIGNING <fs_addr_usage>.
    <fs_addr_usage>-task = action.
    <fs_addr_usage>-data_key-addresstype  = 'XXDEFAULT'.
    <fs_addr_usage>-data_key-valid_to     = '99991231'.
    <fs_addr_usage>-data-standard         = abap_true.
    <fs_addr_usage>-datax-standard        = abap_true.
    <fs_addr_usage>-data-valid_from       = sy-datum.
    <fs_addr_usage>-datax-valid_from      = abap_true.

*--Direcci�n de factura-*
    APPEND INITIAL LINE TO cs_cvis_ei_extern-partner-central_data-address-addresses ASSIGNING <fs_dir>.
    <fs_dir>-task = action.

    <fs_dir>-data-postal-data-street      = is_sol-sol_data-street.      "Calle
    <fs_dir>-data-postal-data-postl_cod1  = is_sol-sol_data-post_code1.  "C�digo postal
    <fs_dir>-data-postal-data-city        = is_sol-sol_data-city.        "Poblaci�n
    <fs_dir>-data-postal-data-country     = is_sol-sol_data-country.     "Pa�s
    <fs_dir>-data-postal-data-region      = is_sol-sol_data-region.      "Regi�n
    <fs_dir>-data-postal-data-langu       = is_sol-sol_data-idioma.      "Idioma
    <fs_dir>-data-postal-datax-street     = abap_true.
    <fs_dir>-data-postal-datax-postl_cod1 = abap_true.
    <fs_dir>-data-postal-datax-city       = abap_true.
    <fs_dir>-data-postal-datax-country    = abap_true.
    <fs_dir>-data-postal-datax-region     = abap_true.
    <fs_dir>-data-postal-datax-langu      = abap_true.

*    Utilizaci�n de direcci�n
    APPEND INITIAL LINE TO <fs_dir>-data-addr_usage-addr_usages ASSIGNING <fs_addr_usage>.
    <fs_addr_usage>-task = action.
    <fs_addr_usage>-data_key-addresstype  = 'BILL_TO'.
    <fs_addr_usage>-data_key-valid_to     = '99991231'.
    <fs_addr_usage>-data-valid_from       = sy-datum.
    <fs_addr_usage>-datax-valid_from      = abap_true.

*------LFA1------*

    cs_cvis_ei_extern-vendor-header-object_task = action.
    cs_cvis_ei_extern-vendor-header-object_instance-lifnr = is_sol-sol_head-partner.

*    Bloqueo de Compras
    IF is_sol-sol_data-acreedor = abap_true.
      cs_cvis_ei_extern-vendor-central_data-central-data-sperm = abap_true.
    ELSE.
      cs_cvis_ei_extern-vendor-central_data-central-data-sperm = abap_false.
    ENDIF.

*    Bloqueo contado permitido
    IF is_sol-sol_data-condiciones_de_pago = 'F000'.
      cs_cvis_ei_extern-vendor-central_data-central-data-zzcontper = abap_true.
    ELSE.
      cs_cvis_ei_extern-vendor-central_data-central-data-zzcontper = abap_false.
    ENDIF.

    cs_cvis_ei_extern-vendor-central_data-central-data-kunnr      = is_sol-sol_head-partner.  "Cliente
    cs_cvis_ei_extern-vendor-central_data-central-data-ktokk      = gc_zpgl.                  "Grupo cuentas
    cs_cvis_ei_extern-vendor-central_data-central-data-zzsinrap   = is_sol-sol_data-zzsinrap. "Sin Rappel
    cs_cvis_ei_extern-vendor-central_data-central-data-zzajupre   = abap_false.               "Ajuste de precios
    cs_cvis_ei_extern-vendor-central_data-central-data-stkzu      = abap_true.                "Sujeto a retenci�n
    cs_cvis_ei_extern-vendor-central_data-central-data-sperr      = abap_false.               "Bloqueo contabilizaci�n todas sociedades
    cs_cvis_ei_extern-vendor-central_data-central-data-zzidelec   = is_sol-sol_data-zzidelec. "C�digo IDElectronet
    cs_cvis_ei_extern-vendor-central_data-central-datax-sperm     = abap_true.
    cs_cvis_ei_extern-vendor-central_data-central-datax-zzajupre  = abap_true.
    cs_cvis_ei_extern-vendor-central_data-central-datax-kunnr     = abap_true.
    cs_cvis_ei_extern-vendor-central_data-central-datax-ktokk     = abap_true.
    cs_cvis_ei_extern-vendor-central_data-central-datax-zzsinrap  = abap_true.
    cs_cvis_ei_extern-vendor-central_data-central-datax-zzcontper = abap_true.
    cs_cvis_ei_extern-vendor-central_data-central-datax-stkzu     = abap_true.
    cs_cvis_ei_extern-vendor-central_data-central-datax-sperr     = abap_true.
    cs_cvis_ei_extern-vendor-central_data-central-datax-zzidelec  = abap_true.

*------LFB1------*

    APPEND INITIAL LINE TO cs_cvis_ei_extern-vendor-company_data-company ASSIGNING <fs_company>.
    <fs_company>-task = action.

    <fs_company>-data_key-bukrs = is_sol-sol_data-sociedad.           "Sociedad

    <fs_company>-data-sperr = abap_false.                             "Bloqueo contabilizaci�n para sociedad
    <fs_company>-data-reprf = abap_true.                              "Verificaci�n factura doble
    <fs_company>-data-zwels = is_sol-sol_data-vias_de_pago.           "V�as de pago
    <fs_company>-data-zahls = abap_false.                             "Bloqueo de pago
    <fs_company>-data-reprf = is_sol-sol_data-verif_fact_doble.       "Verificaci�n factura doble

*    Cuenta Asociada
    CASE abap_true.
      WHEN is_sol-sol_data-acreedor.
        <fs_company>-data-akont = '0410000000'.
      WHEN is_sol-sol_data-proveedor.
        <fs_company>-data-akont = '0400000000'.
    ENDCASE.

    <fs_company>-datax-sperr = abap_true.
    <fs_company>-datax-akont = abap_true.
    <fs_company>-datax-reprf = abap_true.
    <fs_company>-datax-zwels = abap_true.
    <fs_company>-datax-zahls = abap_true.
    <fs_company>-datax-reprf = abap_true.

*------LFM1------*

    SELECT ekorg, bukrs
      FROM t024e
      INTO TABLE @DATA(lt_t024e)
      WHERE bukrs = @is_sol-sol_data-sociedad.

    LOOP AT lt_t024e INTO DATA(ls_t024e).
      APPEND INITIAL LINE TO cs_cvis_ei_extern-vendor-purchasing_data-purchasing ASSIGNING <fs_purchasing>.
      <fs_purchasing>-task = action.

      <fs_purchasing>-data_key-ekorg = ls_t024e-ekorg.                              "Org de compras

      <fs_purchasing>-data-transport_chain  = abap_false.                           "Cadena de transporte
      <fs_purchasing>-data-zterm            = is_sol-sol_data-condiciones_de_pago.  "Condiciones de pago
      <fs_purchasing>-data-zzpormin         = is_sol-sol_data-portes_minimo_pedido. "Portes m�nimo
      <fs_purchasing>-data-zzportes         = is_sol-sol_data-importes_portes.      "Importe portes
      <fs_purchasing>-data-waers            = is_sol-sol_data-moneda_de_pedido.     "Moneda de pedido
      <fs_purchasing>-data-kzret            = 'X'.                                  "Proveedor Devoluci�n
      <fs_purchasing>-data-zzkzret          = 'X'.                                  "Proveedor Devoluci�n
      <fs_purchasing>-data-sperm            = is_sol-sol_data-bloq_compras_pdv.     "Bloqueo de Compras por PDV
      <fs_purchasing>-data-minbw            = is_sol-sol_data-valor_min_ped.        "Valor m�nimo de Pedido
      <fs_purchasing>-data-kalsk            = 'Z1'.                                 "GRupo Esquema de Proveedor
      <fs_purchasing>-datax-transport_chain = abap_true.
      <fs_purchasing>-datax-zterm           = abap_true.
      <fs_purchasing>-datax-zzpormin        = abap_true.
      <fs_purchasing>-datax-zzportes        = abap_true.
      <fs_purchasing>-datax-waers           = abap_true.
      <fs_purchasing>-datax-kzret           = abap_true.
      <fs_purchasing>-datax-sperm           = abap_true.
      <fs_purchasing>-datax-zzkzret         = abap_true.
      <fs_purchasing>-datax-minbw           = abap_true.
      <fs_purchasing>-datax-kalsk           = abap_true.

* Textos Purchasing

      LOOP AT is_sol-sol_textos INTO ls_texts WHERE object = 'LFM1' AND ltext IS NOT INITIAL.
        APPEND INITIAL LINE TO <fs_purchasing>-texts-texts ASSIGNING <fs_text>.

        <fs_text>-task = mod_action.
        <fs_text>-data_key-text_id    = ls_texts-tdid.        "ID del Texto
        <fs_text>-data_key-langu      = ls_texts-spras.       "Idioma 1 D�gito
        <fs_text>-data_key-languiso   = ls_texts-spras.       "Idioma 2 D�gito

        "Contenido del Texto
        lv_text = ls_texts-ltext.

        WHILE strlen( lv_text ) > 132.

          APPEND INITIAL LINE TO <fs_text>-data ASSIGNING <fs_line>.
          <fs_line>-tdline = lv_text(132).

          lv_text = lv_text+132.

        ENDWHILE.

        IF lv_text IS NOT INITIAL.
          APPEND INITIAL LINE TO <fs_text>-data ASSIGNING <fs_line>.
          <fs_line>-tdformat = ' '.
          <fs_line>-tdline = lv_text.
        ENDIF.

        CLEAR: ls_texts, lv_text.
      ENDLOOP.

      "Texto Motivo de Bloqueo Z001
      IF is_sol-sol_data-bloq_compras_pdv IS NOT INITIAL.
        IF is_sol-sol_data-z001 IS NOT INITIAL.
          APPEND INITIAL LINE TO <fs_purchasing>-texts-texts ASSIGNING <fs_text>.

          <fs_text>-task = mod_action.
          <fs_text>-data_key-text_id    = 'Z001'.               "ID del Texto
          <fs_text>-data_key-langu      = 'S'.                  "Idioma 1 D�gito
          <fs_text>-data_key-languiso   = 'ES'.                 "Idioma 2 D�gito

          "Contenido del Texto
          lv_text = is_sol-sol_data-z001.

          WHILE strlen( lv_text ) > 132.
            APPEND INITIAL LINE TO <fs_text>-data ASSIGNING <fs_line>.
            <fs_line>-tdline = lv_text(132).

            lv_text = lv_text+132.
          ENDWHILE.

          IF lv_text IS NOT INITIAL.
            APPEND INITIAL LINE TO <fs_text>-data ASSIGNING <fs_line>.
            <fs_line>-tdformat = ' '.
            <fs_line>-tdline = lv_text.
          ENDIF.

          CLEAR: ls_texts, lv_text.
        ENDIF.
      ENDIF.
    ENDLOOP.

*------KNA1------*

    cs_cvis_ei_extern-customer-header-object_task = action.
    cs_cvis_ei_extern-customer-header-object_instance-kunnr = is_sol-sol_head-partner.      "Proveedor

    cs_cvis_ei_extern-customer-central_data-central-data-lifnr  = is_sol-sol_head-partner.  "Proveedor
    cs_cvis_ei_extern-customer-central_data-central-data-ktokd  = gc_zpgl.                  "Grupo de Cuentas Deudor
    cs_cvis_ei_extern-customer-central_data-central-datax-lifnr = abap_true.
    cs_cvis_ei_extern-customer-central_data-central-datax-ktokd = abap_true.

*------KNVV------*

    SELECT vkorg, bukrs
      FROM tvko
      INTO TABLE @DATA(lt_tvko)
      WHERE bukrs = @is_sol-sol_data-sociedad.

    SELECT land1, xegld
      FROM t005
      INTO TABLE @DATA(lt_t005).

    LOOP AT lt_tvko INTO DATA(ls_tvko).

      READ TABLE lt_t005 INTO DATA(ls_t005) WITH KEY land1 = is_sol-sol_data-country.

      APPEND INITIAL LINE TO cs_cvis_ei_extern-customer-sales_data-sales ASSIGNING <fs_sales>.
      <fs_sales>-task = action.
      <fs_sales>-data_key-vkorg = ls_tvko-vkorg.                        "Organizaci�n de Ventas
      <fs_sales>-data_key-vtweg = '01'.                                 "Canal de Distribuci�n
      <fs_sales>-data_key-spart = '01'.                                 "Sector

      IF ls_t005-land1 = 'ES'.                                          "Zona de Ventas y "Grupo Imputaci�n Cliente
        <fs_sales>-data-bzirk     = '01'. "Nacional
        <fs_sales>-data-ktgrd     = '01'. "Nacional
      ELSEIF ls_t005-land1 <> 'ES' AND ls_t005-xegld IS NOT INITIAL.
        <fs_sales>-data-bzirk     = '02'. "Europeo
        <fs_sales>-data-ktgrd     = '02'. "Europeo
      ELSEIF ls_t005-land1 <> 'ES' AND ls_t005-xegld IS INITIAL.
        <fs_sales>-data-bzirk     = '03'. "Extranjero
        <fs_sales>-data-ktgrd     = '03'. "Extranjero
      ENDIF.

      <fs_sales>-data-waers     = is_sol-sol_data-moneda_de_pedido.     "Moneda
      <fs_sales>-data-konda     = '01'.                                 "Grupo Precio Cliente
      <fs_sales>-data-kalks     = '1'.                                  "Esquema de Cliente
      <fs_sales>-data-lprio     = '50'.                                 "Prioridad de Entrega
      <fs_sales>-data-vwerk     = ls_tvko-vkorg.                        "Centro Suministrador
      <fs_sales>-data-vsbed     = '01'.                                 "Condici�n de Expedici�n

      <fs_sales>-datax-bzirk     = abap_true.
      <fs_sales>-datax-waers     = abap_true.
      <fs_sales>-datax-konda     = abap_true.
      <fs_sales>-datax-kalks     = abap_true.
      <fs_sales>-datax-lprio     = abap_true.
      <fs_sales>-datax-vwerk     = abap_true.
      <fs_sales>-datax-vsbed     = abap_true.
      <fs_sales>-datax-ktgrd     = abap_true.

*      Funciones de IC Cliente
*      SO
      APPEND INITIAL LINE TO <fs_sales>-functions-functions ASSIGNING <fs_function>.
      <fs_function>-task = action.

      CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
        EXPORTING
          input  = 'SO'
        IMPORTING
          output = <fs_function>-data_key-parvw.

      <fs_function>-data-partner  = is_sol-sol_head-partner. "Proveedor
      <fs_function>-datax-partner = abap_true.

*      DF
      APPEND INITIAL LINE TO <fs_sales>-functions-functions ASSIGNING <fs_function>.
      <fs_function>-task = action.

      CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
        EXPORTING
          input  = 'DF'
        IMPORTING
          output = <fs_function>-data_key-parvw.

      <fs_function>-data-partner  = is_sol-sol_head-partner. "Proveedor
      <fs_function>-datax-partner = abap_true.

*      RP
      APPEND INITIAL LINE TO <fs_sales>-functions-functions ASSIGNING <fs_function>.
      <fs_function>-task = action.

      CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
        EXPORTING
          input  = 'RP'
        IMPORTING
          output = <fs_function>-data_key-parvw.

      <fs_function>-data-partner  = is_sol-sol_head-partner. "Proveedor
      <fs_function>-datax-partner = abap_true.

*      DM
      APPEND INITIAL LINE TO <fs_sales>-functions-functions ASSIGNING <fs_function>.
      <fs_function>-task = action.

      CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
        EXPORTING
          input  = 'DM'
        IMPORTING
          output = <fs_function>-data_key-parvw.

      <fs_function>-data-partner  = is_sol-sol_head-partner. "Proveedor
      <fs_function>-datax-partner = abap_true.
    ENDLOOP.

*----BANCO---*

    APPEND INITIAL LINE TO cs_cvis_ei_extern-partner-central_data-bankdetail-bankdetails ASSIGNING <fs_bankdetail>.
    <fs_bankdetail>-task = action.

    <fs_bankdetail>-data-iban = is_sol-sol_data-iban.

    SELECT SINGLE tiban~iban, tiban~banks, tiban~bankl ,tiban~bankn, tiban~bkont
      FROM tiban
      INTO @DATA(ls_tiban)
      WHERE iban = @is_sol-sol_data-iban.
    IF sy-subrc = 0.

      <fs_bankdetail>-data-bank_ctry  = ls_tiban-banks.  "Pa�s/regi�n de banco
      <fs_bankdetail>-data-bank_key   = ls_tiban-bankl.  "Clave de banco
      <fs_bankdetail>-data-bank_acct  = ls_tiban-bankn.  "Cuenta bancaria
      <fs_bankdetail>-data-ctrl_key   = ls_tiban-bkont.  "Clave control bancos

    ELSE. "IBAN no existe

      CALL FUNCTION 'CONVERT_IBAN_2_BANK_ACCOUNT'
        EXPORTING
          i_iban             = is_sol-sol_data-iban
        IMPORTING
          e_bank_account     = <fs_bankdetail>-data-bank_acct
          e_bank_control_key = <fs_bankdetail>-data-ctrl_key
          e_bank_country     = <fs_bankdetail>-data-bank_ctry
          e_bank_number      = <fs_bankdetail>-data-bank_key
        EXCEPTIONS
          no_conversion      = 1
          OTHERS             = 2.
      IF sy-subrc <> 0.
      ENDIF.

      CLEAR: lv_bankn.
      lv_bankn = |{ <fs_bankdetail>-data-bank_acct ALPHA = IN }|.

      CALL FUNCTION 'CREATE_IBAN'
        EXPORTING
          i_banks          = <fs_bankdetail>-data-bank_ctry
          i_bankl          = <fs_bankdetail>-data-bank_key
          i_bankn          = lv_bankn
          i_bkont          = <fs_bankdetail>-data-ctrl_key
        EXCEPTIONS
          action_cancelled = 1
          bank_not_found   = 2
          OTHERS           = 3.
      IF sy-subrc = 2.
        add_return(
          EXPORTING
            iv_type   = 'E'
            iv_id     = 'ZCA_SOL'
            iv_number = '045'
          CHANGING
            ct_return = ct_return ).
        add_return(
          EXPORTING
            iv_type       = 'E'
            iv_id         = 'BF00'
            iv_number     = '211'
            iv_message_v1 = |{ <fs_bankdetail>-data-bank_ctry ALPHA = OUT }|
            iv_message_v2 = |{ <fs_bankdetail>-data-bank_key ALPHA = OUT }|
          CHANGING
            ct_return     = ct_return ).
      ELSE.
        add_return(
          EXPORTING
            iv_type   = 'E'
            iv_id     = 'ZCA_SOL'
            iv_number = '045'
          CHANGING
            ct_return = ct_return ).
      ENDIF.
    ENDIF.

    <fs_bankdetail>-datax-iban      = abap_true.
    <fs_bankdetail>-datax-bank_acct = abap_true.
    <fs_bankdetail>-datax-ctrl_key  = abap_true.
    <fs_bankdetail>-datax-bank_ctry = abap_true.
    <fs_bankdetail>-datax-bank_key  = abap_true.

*----CONTACTOS---*

    REFRESH: lt_ct_partner.
    DESCRIBE TABLE is_sol-sol_contacts LINES DATA(lv_count).
    lv_it = 1.

    LOOP AT is_sol-sol_contacts INTO DATA(ls_contact).

      CHECK ct_return[] IS INITIAL.

      CLEAR: ls_centraldata, ls_centraldataperson.

      IF ls_contact-smtp[] IS NOT INITIAL. "Solo se crean si est� informado

        APPEND INITIAL LINE TO cs_cvis_ei_extern-partner_relation ASSIGNING <fs_relation>.
        <fs_relation>-header-object_task = action.
        <fs_relation>-header-object_instance-partner1-bpartner  = is_sol-sol_head-partner.
        <fs_relation>-header-object_instance-relat_category     = 'BUR001'.                   "Tipo de Relaci�n
        <fs_relation>-header-object_instance-date_to            = '99991231'.                 "Fin de Validez

        IF lv_it = 1.
          zcacl_intervalos_bp=>get_new_value_interval(
            EXPORTING
              iv_group         = gc_group
              iv_range         = lv_count
            IMPORTING
              ev_partner_range = lt_ct_partner ).
        ENDIF.

        TRY.
            <fs_relation>-header-object_instance-partner2-bpartner = lt_ct_partner[ lv_it ]-partner.
          CATCH cx_root.
        ENDTRY.

        ls_centraldata-partnertype      = gc_group.

        ls_centraldataperson-firstname  = ls_contact-contact-name_first.

        IF ls_contact-contact-name_last IS NOT INITIAL.
          ls_centraldataperson-lastname   = ls_contact-contact-name_last.
        ELSE.
          ls_centraldataperson-lastname   = '-'. "Campo Obligatorio para la Relaci�n
        ENDIF.

        CLEAR: ls_addressdata.
        REFRESH: lt_telf, lt_email.

        LOOP AT ls_contact-tel INTO DATA(ls_tel).
          APPEND INITIAL LINE TO lt_telf ASSIGNING FIELD-SYMBOL(<fs_ct_tel>).

          <fs_ct_tel>-telephone = ls_tel-tel_number.
          <fs_ct_tel>-r_3_user  = ls_tel-r3_user.
        ENDLOOP.
        LOOP AT ls_contact-smtp INTO DATA(ls_smpt).
          APPEND INITIAL LINE TO lt_email ASSIGNING FIELD-SYMBOL(<fs_ct_email>).

          <fs_ct_email>-e_mail = ls_smpt-smtp_address.
        ENDLOOP.
        ls_addressdata-country = ls_contact-contact-country.
        IF ls_contact-contact-com_type = 'CON_DEL'.
          TRY.
              ls_addressdata-street     = ls_contact-dir[ 1 ]-street.
              ls_addressdata-postl_cod1 = ls_contact-dir[ 1 ]-post_code1.
              ls_addressdata-city       = ls_contact-dir[ 1 ]-city.
            CATCH cx_root.
          ENDTRY.
        ENDIF.
        CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
          EXPORTING
            businesspartnerextern = <fs_relation>-header-object_instance-partner2-bpartner
            partnercategory       = '1'             "Tipo de socio comercial
            partnergroup          = gc_group        "Agrupaci�n socios
            centraldata           = ls_centraldata
            centraldataperson     = ls_centraldataperson
            addressdata           = ls_addressdata
          TABLES
            telefondata           = lt_telf
            e_maildata            = lt_email
            return                = ct_return.
        IF NOT ( line_exists( ct_return[ type = 'A' ] ) OR line_exists( ct_return[ type = 'E' ] ) ).
          <fs_relation>-central_data-main-task = action.

          <fs_relation>-central_data-main-data-date_from    = sy-datum.
          <fs_relation>-central_data-main-data-date_to_new  = '99991231'.
          <fs_relation>-central_data-main-datax-date_from   = abap_true.
          <fs_relation>-central_data-main-datax-date_to_new = abap_true.

          <fs_relation>-central_data-contact-central_data-data-department = is_sol-sol_head-pdv.      "Departamento
          <fs_relation>-central_data-contact-central_data-data-comments   = ls_contact-contact-cargo. "Cargo

          <fs_relation>-central_data-contact-central_data-datax-department = abap_true.
          <fs_relation>-central_data-contact-central_data-datax-comments   = abap_true.

          CASE ls_contact-contact-com_type.
            WHEN 'CON_PED'.
              <fs_relation>-central_data-contact-central_data-data-function = '0017'.
            WHEN 'CON_INC'.
              <fs_relation>-central_data-contact-central_data-data-function = '0014'.
            WHEN 'CON_ADM'.
              <fs_relation>-central_data-contact-central_data-data-function = '0022'.
            WHEN 'CON_DEL'.
              <fs_relation>-central_data-contact-central_data-data-function = '0015'.
            WHEN 'CON_MAT'.
              <fs_relation>-central_data-contact-central_data-data-function = '0013'.
            WHEN 'CON_DEM'.
              <fs_relation>-central_data-contact-central_data-data-function = '0018'.
            WHEN 'CON_COM'.
              <fs_relation>-central_data-contact-central_data-data-function = '0016'.
            WHEN 'CON_NCF'.
              <fs_relation>-central_data-contact-central_data-data-function = '0020'.
          ENDCASE.
          <fs_relation>-central_data-contact-central_data-datax-function = abap_true.
        ENDIF.

        lv_it = lv_it + 1.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.
