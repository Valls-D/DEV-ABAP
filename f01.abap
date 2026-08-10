*&---------------------------------------------------------------------*
*&  Include           ZPRS4P0007_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  RB_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rb_check .
IF rb_clt = 'X'.
    gv_rb_press =  'C'.
ELSE.
    gv_rb_press = 'P'.
ENDIF.
ENDFORM.                    " RB_CHECK
*&---------------------------------------------------------------------*
*&      Form  PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process .

PERFORM read_file.
PERFORM get_tables.
PERFORM execute_bapi.

ENDFORM.                    " PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_file .

DATA: lo_gui TYPE REF TO gui_read,
        lv_filename TYPE string,
        lv_filetelf TYPE string,
        lv_filesmtp TYPE string.
CREATE OBJECT: lo_gui.

lv_filename = pa_file.
lv_filetelf = pa_ft.
lv_filesmtp = pa_fc.

lo_gui->upload_excel_file(
    EXPORTING
        iv_filename = lv_filename
        iv_filetelf = lv_filetelf
        iv_filesmtp = lv_filesmtp
    IMPORTING
        et_content  = gt_file
        et_telf     = gt_f_telf
        et_smtp     = gt_f_smtp
        et_targets  = gt_targets ).

ENDFORM.                    " READ_FILE
*&---------------------------------------------------------------------*
*&      Form  GET_TABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_tables .

CASE 'X'.

    WHEN rb_clt.  "Clientes
    SELECT kunnr
        FROM kna1
        INTO TABLE gt_kna1
        FOR ALL ENTRIES IN gt_targets
        WHERE kunnr = gt_targets-target
        AND kunnr <> ''.
    IF sy-subrc = 0.
        SORT gt_kna1 BY kunnr.
    ENDIF.

    WHEN rb_prov. "Proveedores
    SELECT lifnr adrnr
        FROM lfa1
        INTO TABLE gt_lfa1
        FOR ALL ENTRIES IN gt_targets
        WHERE lifnr = gt_targets-target
        AND lifnr <> ''.
    IF sy-subrc = 0.
        SORT gt_lfa1 BY lifnr.
    ENDIF.

**      INI ZPRY_PRS4 30.04.2024 54217049T
    IF gt_lfa1[] IS NOT INITIAL.
        SELECT *
        FROM adr2
        INTO TABLE gt_adr2
        FOR ALL ENTRIES IN gt_lfa1
        WHERE addrnumber = gt_lfa1-adrnr
            AND persnumber = ''.
        IF sy-subrc = 0.
        ENDIF.

        SELECT *
        FROM adr3
        INTO TABLE gt_adr3
        FOR ALL ENTRIES IN gt_lfa1
        WHERE addrnumber = gt_lfa1-adrnr
            AND persnumber = ''.
        IF sy-subrc = 0.
        ENDIF.

        SELECT *
        FROM adr6
        INTO TABLE gt_adr6
        FOR ALL ENTRIES IN gt_lfa1
        WHERE addrnumber = gt_lfa1-adrnr
            AND persnumber = ''.
        IF sy-subrc = 0.
        ENDIF.
    ENDIF.
**      FIN ZPRY_PRS4

ENDCASE.

SELECT id name
    FROM icon
    INTO TABLE gt_icons
    WHERE name = 'ICON_CHECKED'
        OR name = 'ICON_INCOMPLETE'
        OR name = 'ICON_MESSAGE_ERROR'
        OR name = 'ICON_MESSAGE_WARNING'
        OR name = 'ICON_MESSAGE_CRITICAL'.
IF sy-subrc = 0.
    SORT gt_icons BY icon.
ENDIF.

**  INI ZPRY_PRS4 30.04.2024 54217049T
PERFORM recuperar_hardcodes.
**  FIN ZPRY_PRS4

ENDFORM.                    " GET_TABLES
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM execute_bapi .

DATA: ls_target TYPE ty_trgt,
        lv_exists TYPE c.

FIELD-SYMBOLS <fs_kna1>   TYPE ty_kna1.
FIELD-SYMBOLS <fs_lfa1>   TYPE ty_lfa1.

DATA: lv_lines    TYPE i,
        lv_porc     TYPE i,
        lv_contador TYPE i.

DESCRIBE TABLE gt_targets LINES lv_lines.

LOOP AT gt_targets INTO ls_target.
    lv_porc = ( lv_contador * 100 ) / lv_lines  .

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
        percentage = lv_porc
        text       = text-003.

    "Comprobación existencia de deudor/acreedor
    CASE 'X'.
    WHEN rb_clt.
        READ TABLE gt_kna1 ASSIGNING <fs_kna1> WITH KEY kunnr = ls_target BINARY SEARCH.
        IF sy-subrc = 0.
        lv_exists = 'X'.
        ENDIF.
        PERFORM client_logic USING ls_target lv_exists.

    WHEN rb_prov.
        READ TABLE gt_lfa1 ASSIGNING <fs_lfa1> WITH KEY lifnr = ls_target BINARY SEARCH.
        IF sy-subrc = 0.
        lv_exists = 'X'.
        ENDIF.
        PERFORM vendor_logic USING ls_target lv_exists.
    ENDCASE.

    ADD 1 TO lv_contador.
ENDLOOP.

ENDFORM.                    " EXECUTE_BAPI
*&---------------------------------------------------------------------*
*&      Form  ADD_ALV_ADDR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*      -->P_ICON  text
*      -->P_LINE  text
*----------------------------------------------------------------------*
FORM add_alv_addr  USING    p_target
                            p_icon
                            p_exists.

DATA: ls_alv  TYPE ty_alv,
        ls_file TYPE ty_file.

ls_alv-icon = p_icon.

READ TABLE gt_file INTO ls_file WITH KEY target = p_target.
IF sy-subrc = 0.
    ls_alv-index = ls_file-index.
ENDIF.

ls_alv-file = 'Direcciones'.

CASE 'X'.
    WHEN rb_clt.
    ls_alv-kunnr = p_target.
    WHEN rb_prov.
    ls_alv-lifnr = p_target.
ENDCASE.

IF p_exists IS INITIAL.
    READ TABLE gt_icons INTO ls_alv-exists WITH KEY iconname = 'ICON_INCOMPLETE'.
ELSE.
    READ TABLE gt_icons INTO ls_alv-exists WITH KEY iconname = 'ICON_CHECKED'.
ENDIF.

CLEAR: gs_return.
READ TABLE gt_return INTO gs_return WITH KEY target = p_target.
IF sy-subrc = 0 AND gs_return-return_table[] IS NOT INITIAL.
    CASE p_icon.
    WHEN '1'.
        READ TABLE gt_icons INTO ls_alv-e_icon WITH KEY iconname = 'ICON_MESSAGE_ERROR'.
    WHEN '3'.
        READ TABLE gt_icons INTO ls_alv-e_icon WITH KEY iconname = 'ICON_MESSAGE_WARNING'.
    ENDCASE.
ENDIF.

IF p_icon = '2'.
    READ TABLE gt_icons INTO ls_alv-e_icon WITH KEY iconname = 'ICON_MESSAGE_CRITICAL'.
ENDIF.

APPEND ls_alv TO gt_alv.

ENDFORM.                    " ADD_ALV_ADDR
*&---------------------------------------------------------------------*
*&      Form  ADD_ALV_TELF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*      -->P_ICON  text
*      -->P_LINE  text
*----------------------------------------------------------------------*
FORM add_alv_telf  USING    p_target
                            p_icon
                            p_exists.

DATA: ls_alv  TYPE ty_alv,
        ls_telf TYPE ty_f_telf.

ls_alv-icon = p_icon.

LOOP AT gt_f_telf INTO ls_telf WHERE target = p_target.
    ls_alv-index = ls_telf-index.

    ls_alv-file = 'Teléfonos'.

    CASE 'X'.
    WHEN rb_clt.
        ls_alv-kunnr = p_target.
    WHEN rb_prov.
        ls_alv-lifnr = p_target.
    ENDCASE.

    IF p_exists IS INITIAL.
    READ TABLE gt_icons INTO ls_alv-exists WITH KEY iconname = 'ICON_INCOMPLETE'.
    ELSE.
    READ TABLE gt_icons INTO ls_alv-exists WITH KEY iconname = 'ICON_CHECKED'.
    ENDIF.

    CLEAR: gs_return.
    READ TABLE gt_return INTO gs_return WITH KEY target = p_target.
    IF sy-subrc = 0 AND gs_return-return_table[] IS NOT INITIAL.
    CASE p_icon.
        WHEN '1'.
        READ TABLE gt_icons INTO ls_alv-e_icon WITH KEY iconname = 'ICON_MESSAGE_ERROR'.
        WHEN '3'.
        READ TABLE gt_icons INTO ls_alv-e_icon WITH KEY iconname = 'ICON_MESSAGE_WARNING'.
    ENDCASE.
    ENDIF.

    IF p_icon = '2'.
    READ TABLE gt_icons INTO ls_alv-e_icon WITH KEY iconname = 'ICON_MESSAGE_CRITICAL'.
    ENDIF.

    APPEND ls_alv TO gt_alv.
ENDLOOP.

ENDFORM.                    " ADD_ALV_TELF
*&---------------------------------------------------------------------*
*&      Form  ADD_ALV_SMTP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*      -->P_ICON  text
*      -->P_LINE  text
*----------------------------------------------------------------------*
FORM add_alv_smtp  USING    p_target
                            p_icon
                            p_exists.

DATA: ls_alv  TYPE ty_alv,
        ls_smtp TYPE ty_f_smtp.

ls_alv-icon = p_icon.

LOOP AT gt_f_smtp INTO ls_smtp WHERE target = p_target.
    ls_alv-index = ls_smtp-index.

    ls_alv-file = 'Correos'.

    CASE 'X'.
    WHEN rb_clt.
        ls_alv-kunnr = p_target.
    WHEN rb_prov.
        ls_alv-lifnr = p_target.
    ENDCASE.

    IF p_exists IS INITIAL.
    READ TABLE gt_icons INTO ls_alv-exists WITH KEY iconname = 'ICON_INCOMPLETE'.
    ELSE.
    READ TABLE gt_icons INTO ls_alv-exists WITH KEY iconname = 'ICON_CHECKED'.
    ENDIF.

    CLEAR: gs_return.
    READ TABLE gt_return INTO gs_return WITH KEY target = p_target.
    IF sy-subrc = 0 AND gs_return-return_table[] IS NOT INITIAL.
    CASE p_icon.
        WHEN '1'.
        READ TABLE gt_icons INTO ls_alv-e_icon WITH KEY iconname = 'ICON_MESSAGE_ERROR'.
        WHEN '3'.
        READ TABLE gt_icons INTO ls_alv-e_icon WITH KEY iconname = 'ICON_MESSAGE_WARNING'.
    ENDCASE.
    ENDIF.

    IF p_icon = '2'.
    READ TABLE gt_icons INTO ls_alv-e_icon WITH KEY iconname = 'ICON_MESSAGE_CRITICAL'.
    ENDIF.

    APPEND ls_alv TO gt_alv.
ENDLOOP.

ENDFORM.                    " ADD_ALV_SMTP
*&---------------------------------------------------------------------*
*&      Form  SAVE_RETURN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_RETURN  text
*----------------------------------------------------------------------*
FORM save_return  TABLES   p_return
                USING    p_target.

CLEAR: gs_return.
gs_return-target = p_target.
APPEND LINES OF p_return TO gs_return-return_table.
APPEND gs_return TO gt_return.

ENDFORM.                    " SAVE_RETURN
*&---------------------------------------------------------------------*
*&      Form  FILL_CUSTOMER_ADDR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*----------------------------------------------------------------------*
FORM fill_customer_addr  USING    p_file TYPE ty_file
                        CHANGING p_master_data TYPE cmds_ei_main.

DATA: ls_data         TYPE cmds_ei_extern,
        ls_master_data  TYPE cmds_ei_main.

ls_data-header-object_task = 'M'.
ls_data-header-object_instance-kunnr = p_file-target.
APPEND ls_data TO ls_master_data-customers.

CALL METHOD cmd_ei_api_extract=>get_data
    EXPORTING
    is_master_data = ls_master_data
    IMPORTING
    es_master_data = p_master_data.

FIELD-SYMBOLS <fs_data> TYPE cmds_ei_extern.
READ TABLE p_master_data-customers ASSIGNING <fs_data> INDEX 1.
IF sy-subrc = 0.
    CLEAR: <fs_data>-central_data-text,
            <fs_data>-central_data-vat_number,
            <fs_data>-central_data-tax_grouping,
            <fs_data>-central_data-tax_ind,
            <fs_data>-central_data-export,
            <fs_data>-central_data-loading,
            <fs_data>-central_data-contact,
            <fs_data>-central_data-creditcard,
            <fs_data>-central_data-bankdetail.
ENDIF.

ENDFORM.                    " FILL_CUSTOMER_ADDR
*&---------------------------------------------------------------------*
*&      Form  FILL_VENDOR_ADDR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILE  text
*      <--P_VENDOR_MASTER_DATA  text
*----------------------------------------------------------------------*
FORM fill_vendor_addr   USING    p_file  TYPE ty_file
                        CHANGING p_master_data TYPE vmds_ei_main.

DATA: ls_data         TYPE vmds_ei_extern,
        ls_master_data  TYPE vmds_ei_main.

ls_data-header-object_task = 'M'.
ls_data-header-object_instance-lifnr = p_file-target.
APPEND ls_data TO ls_master_data-vendors.

CALL METHOD vmd_ei_api_extract=>get_data
    EXPORTING
    is_master_data = ls_master_data
    IMPORTING
    es_master_data = p_master_data.

FIELD-SYMBOLS <fs_data> TYPE vmds_ei_extern.
READ TABLE p_master_data-vendors ASSIGNING <fs_data> INDEX 1.
IF sy-subrc = 0.
    CLEAR: <fs_data>-central_data-text,
            <fs_data>-central_data-vat_number,
            <fs_data>-central_data-tax_grouping,
            <fs_data>-central_data-contact,
            <fs_data>-central_data-bankdetail.
ENDIF.

ENDFORM.                    " FILL_VENDOR_ADDR
*&---------------------------------------------------------------------*
*&      Form  FILL_CUSTOMER_TELF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*----------------------------------------------------------------------*
FORM fill_customer_telf  USING    p_telf TYPE ty_f_telf
                        CHANGING p_master_data TYPE cmds_ei_main.

DATA: ls_data         TYPE cmds_ei_extern,
        ls_master_data  TYPE cmds_ei_main.

ls_data-header-object_task = 'M'.
ls_data-header-object_instance-kunnr = p_telf-target.
APPEND ls_data TO ls_master_data-customers.

CALL METHOD cmd_ei_api_extract=>get_data
    EXPORTING
    is_master_data = ls_master_data
    IMPORTING
    es_master_data = p_master_data.

FIELD-SYMBOLS <fs_data> TYPE cmds_ei_extern.
READ TABLE p_master_data-customers ASSIGNING <fs_data> INDEX 1.
IF sy-subrc = 0.
    CLEAR: <fs_data>-central_data-text,
            <fs_data>-central_data-vat_number,
            <fs_data>-central_data-tax_grouping,
            <fs_data>-central_data-tax_ind,
            <fs_data>-central_data-export,
            <fs_data>-central_data-loading,
            <fs_data>-central_data-contact,
            <fs_data>-central_data-creditcard,
            <fs_data>-central_data-bankdetail.
ENDIF.

ENDFORM.                    " FILL_CUSTOMER_TELF
*&---------------------------------------------------------------------*
*&      Form  FILL_VENDOR_TELF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*----------------------------------------------------------------------*
FORM fill_vendor_telf  USING    p_telf TYPE ty_f_telf
                        CHANGING p_master_data TYPE vmds_ei_main.

DATA: ls_data         TYPE vmds_ei_extern,
        ls_master_data  TYPE vmds_ei_main.

ls_data-header-object_task = 'M'.
ls_data-header-object_instance-lifnr = p_telf-target.
APPEND ls_data TO ls_master_data-vendors.

CALL METHOD vmd_ei_api_extract=>get_data
    EXPORTING
    is_master_data = ls_master_data
    IMPORTING
    es_master_data = p_master_data.

FIELD-SYMBOLS <fs_data> TYPE vmds_ei_extern.
READ TABLE p_master_data-vendors ASSIGNING <fs_data> INDEX 1.
IF sy-subrc = 0.
    CLEAR: <fs_data>-central_data-text,
            <fs_data>-central_data-vat_number,
            <fs_data>-central_data-tax_grouping,
            <fs_data>-central_data-contact,
            <fs_data>-central_data-bankdetail.
ENDIF.

ENDFORM.                    " FILL_VENDOR_TELF
*&---------------------------------------------------------------------*
*&      Form  FILL_CUSTOMER_SMTP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*----------------------------------------------------------------------*
FORM fill_customer_smtp  USING    p_smtp TYPE ty_f_smtp
                        CHANGING p_master_data.

DATA: ls_data             TYPE cmds_ei_extern,
        ls_customer         TYPE cmds_ei_extern,
        ls_master_data      TYPE cmds_ei_main,
        ls_master_data_aux  TYPE cmds_ei_main.

ls_data-header-object_task = 'M'.
ls_data-header-object_instance-kunnr = p_smtp-target.
APPEND ls_data TO ls_master_data-customers.

CALL METHOD cmd_ei_api_extract=>get_data
    EXPORTING
    is_master_data = ls_master_data
    IMPORTING
    es_master_data = p_master_data.

ENDFORM.                    " FILL_CUSTOMER_SMTP
*&---------------------------------------------------------------------*
*&      Form  FILL_VENDOR_SMTP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*----------------------------------------------------------------------*
FORM fill_vendor_smtp  USING    p_smtp        TYPE ty_f_smtp
                        CHANGING p_master_data TYPE vmds_ei_main.

DATA: ls_data             TYPE vmds_ei_extern,
        ls_master_data      TYPE vmds_ei_main.

ls_data-header-object_task = 'M'.
ls_data-header-object_instance-lifnr = p_smtp-target.
APPEND ls_data TO ls_master_data-vendors.

CALL METHOD vmd_ei_api_extract=>get_data
    EXPORTING
    is_master_data = ls_master_data
    IMPORTING
    es_master_data = p_master_data.

ENDFORM.                    " FILL_VENDOR_SMTP
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DATA_CUSTOMER_ADDR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILE  text
*      <--P_MASTER_DATA  text
*----------------------------------------------------------------------*
FORM change_data_customer_addr  USING     p_file         TYPE ty_file
                                        p_exists       TYPE c
                                CHANGING  p_master_data  TYPE cmds_ei_main.

FIELD-SYMBOLS: <fs_data>        TYPE cmds_ei_extern,
                <fs_data_aux>    TYPE cmds_ei_extern.
DATA: ls_master_data_correct    TYPE cmds_ei_main,
        ls_master_data_aux        TYPE cmds_ei_main,
        ls_message_correct        TYPE cvis_message,
        ls_master_data_defective  TYPE cmds_ei_main,
        ls_message_defective      TYPE cvis_message,
        lv_fax_deletion           TYPE abap_bool.

READ TABLE p_master_data-customers ASSIGNING <fs_data> INDEX 1.
IF sy-subrc = 0.
    <fs_data>-header-object_task = 'M'.

    ls_master_data_aux = p_master_data.

    READ TABLE ls_master_data_aux-customers ASSIGNING <fs_data_aux> INDEX 1.
    IF sy-subrc = 0.
    DELETE <fs_data>-central_data-address-communication-fax-fax WHERE contact-task = 'I'.
    DELETE <fs_data_aux>-central_data-address-communication-fax-fax WHERE contact-task = 'D'.

    READ TABLE <fs_data>-central_data-address-communication-fax-fax WITH KEY contact-task = 'D' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
        lv_fax_deletion = abap_true.
    ENDIF.
    ENDIF.

    IF lv_fax_deletion = abap_true.
    CALL METHOD cmd_ei_api=>maintain_bapi
        EXPORTING
        iv_test_run              = cb_test
        is_master_data           = p_master_data
        IMPORTING
        es_master_data_correct   = ls_master_data_correct
        es_message_correct       = ls_message_correct
        es_master_data_defective = ls_master_data_defective
        es_message_defective     = ls_message_defective.
    IF sy-subrc = 0 AND ls_message_defective-is_error IS INITIAL.
        DATA: ls_adrcomc TYPE adrcomc.
        SELECT SINGLE adrcomc~client adrcomc~addrnumber adrcomc~persnumber adrcomc~comm_type adrcomc~date_from adrcomc~high_value
        FROM adrcomc INNER JOIN kna1 ON ( kna1~adrnr = adrcomc~addrnumber )
        INTO ls_adrcomc
        WHERE kna1~kunnr = <fs_data_aux>-header-object_instance-kunnr
            AND adrcomc~comm_type = 'FAX'.
        IF sy-subrc = 0.
        ls_adrcomc-high_value = '000'.
        MODIFY adrcomc FROM ls_adrcomc.
        ENDIF.
    ELSE.
        PERFORM save_return TABLES ls_message_defective-messages USING p_file-target.
        PERFORM add_alv_addr USING p_file-target 1 p_exists.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        EXIT.
    ENDIF.
    ENDIF.

    CALL METHOD cmd_ei_api=>maintain_bapi
    EXPORTING
        iv_test_run              = cb_test
        is_master_data           = ls_master_data_aux
    IMPORTING
        es_master_data_correct   = ls_master_data_correct
        es_message_correct       = ls_message_correct
        es_master_data_defective = ls_master_data_defective
        es_message_defective     = ls_message_defective.
    IF sy-subrc = 0 AND ls_message_defective-is_error IS INITIAL.
**      INI ZPRY_PRS4 07.05.2024 54217049T
    PERFORM export_smtp_cl USING ls_master_data_aux.
**      FIN ZPRY_PRS4
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
        wait = 'X'.
    PERFORM save_return TABLES ls_message_correct-messages USING p_file-target.
    PERFORM add_alv_addr USING p_file-target 3 p_exists.
    ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    PERFORM save_return TABLES ls_message_defective-messages USING p_file-target.
    PERFORM add_alv_addr USING p_file-target 1 p_exists.
    ENDIF.
ENDIF.

ENDFORM.                    " CHANGE_DATA_CUSTOMER_ADDR
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DATA_VENDOR_ADDR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILE  text
*      -->P_EXISTS  text
*      <--P_MASTER_DATA  text
*----------------------------------------------------------------------*
FORM change_data_vendor_addr  USING    p_file         TYPE ty_file
                                        p_exists       TYPE c
                            CHANGING p_master_data  TYPE vmds_ei_main.

FIELD-SYMBOLS: <fs_data>        TYPE vmds_ei_extern,
                <fs_data_aux>    TYPE vmds_ei_extern.
DATA: ls_master_data_correct    TYPE vmds_ei_main,
        ls_master_data_aux        TYPE vmds_ei_main,
        ls_message_correct        TYPE cvis_message,
        ls_master_data_defective  TYPE vmds_ei_main,
        ls_message_defective      TYPE cvis_message,
        lv_fax_deletion           TYPE abap_bool.

READ TABLE p_master_data-vendors ASSIGNING <fs_data> INDEX 1.
IF sy-subrc = 0.
    <fs_data>-header-object_task = 'M'.

    ls_master_data_aux = p_master_data.

    READ TABLE ls_master_data_aux-vendors ASSIGNING <fs_data_aux> INDEX 1.
    IF sy-subrc = 0.
    DELETE <fs_data>-central_data-address-communication-fax-fax WHERE contact-task = 'I'.
    DELETE <fs_data_aux>-central_data-address-communication-fax-fax WHERE contact-task = 'D'.

    READ TABLE <fs_data>-central_data-address-communication-fax-fax WITH KEY contact-task = 'D' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
        lv_fax_deletion = abap_true.
    ENDIF.
    ENDIF.

    IF lv_fax_deletion = abap_true.
    CALL METHOD vmd_ei_api=>maintain_bapi
        EXPORTING
        iv_test_run              = cb_test
        is_master_data           = p_master_data
        IMPORTING
        es_master_data_correct   = ls_master_data_correct
        es_message_correct       = ls_message_correct
        es_master_data_defective = ls_master_data_defective
        es_message_defective     = ls_message_defective.
    IF sy-subrc = 0 AND ls_message_defective-is_error IS INITIAL.
        DATA: ls_adrcomc TYPE adrcomc.
        SELECT SINGLE *
        FROM adrcomc
        INTO ls_adrcomc
        WHERE addrnumber = <fs_data_aux>-central_data-central-data-adrnr
            AND comm_type = 'FAX'.
        IF sy-subrc = 0.
        ls_adrcomc-high_value = '000'.
        MODIFY adrcomc FROM ls_adrcomc.
        ENDIF.
    ELSE.
        PERFORM save_return TABLES ls_message_defective-messages USING p_file-target.
        PERFORM add_alv_addr USING p_file-target 1 p_exists.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        EXIT.
    ENDIF.
    ENDIF.

    CALL METHOD vmd_ei_api=>maintain_bapi
    EXPORTING
        iv_test_run              = cb_test
        is_master_data           = ls_master_data_aux
    IMPORTING
        es_master_data_correct   = ls_master_data_correct
        es_message_correct       = ls_message_correct
        es_master_data_defective = ls_master_data_defective
        es_message_defective     = ls_message_defective.
    IF sy-subrc = 0 AND ls_message_defective-is_error IS INITIAL.
**      INI ZPRY_PRS4 30.04.2024 54217049T
    PERFORM save_srm_from_addr USING ls_master_data_aux.
    PERFORM export_smtp USING ls_master_data_aux.
**      FIN ZPRY_PRS4
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
        wait = 'X'.
    PERFORM save_return TABLES ls_message_correct-messages USING p_file-target.
    PERFORM add_alv_addr USING p_file-target 3 p_exists.
    ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    PERFORM save_return TABLES ls_message_defective-messages USING p_file-target.
    PERFORM add_alv_addr USING p_file-target 1 p_exists.
    ENDIF.
ENDIF.

ENDFORM.                    " CHANGE_DATA_VENDOR_ADDR
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DATA_CUSTOMER_TELF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILE  text
*      <--P_MASTER_DATA  text
*----------------------------------------------------------------------*
FORM change_data_customer_telf  USING     p_telf         TYPE ty_f_telf
                                        p_exists       TYPE c
                                CHANGING  p_master_data  TYPE cmds_ei_main.

FIELD-SYMBOLS: <fs_data>        TYPE cmds_ei_extern,
                <fs_data_aux>    TYPE cmds_ei_extern.
DATA: ls_master_data_correct    TYPE cmds_ei_main,
        ls_master_data_aux        TYPE cmds_ei_main,
        ls_message_correct        TYPE cvis_message,
        ls_master_data_defective  TYPE cmds_ei_main,
        ls_message_defective      TYPE cvis_message,
        lv_telf_deletion          TYPE abap_bool.

READ TABLE p_master_data-customers ASSIGNING <fs_data> INDEX 1.
IF sy-subrc = 0.
    <fs_data>-header-object_task = 'M'.

    ls_master_data_aux = p_master_data.

    READ TABLE ls_master_data_aux-customers ASSIGNING <fs_data_aux> INDEX 1.
    IF sy-subrc = 0.
    DELETE <fs_data>-central_data-address-communication-phone-phone WHERE contact-task = 'I'.
    DELETE <fs_data_aux>-central_data-address-communication-phone-phone WHERE contact-task = 'D'.

    READ TABLE <fs_data>-central_data-address-communication-phone-phone WITH KEY contact-task = 'D' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
        lv_telf_deletion = abap_true.
    ENDIF.
    ENDIF.

    IF lv_telf_deletion = abap_true.
    CALL METHOD cmd_ei_api=>maintain_bapi
        EXPORTING
        iv_test_run              = cb_test
        is_master_data           = p_master_data
        IMPORTING
        es_master_data_correct   = ls_master_data_correct
        es_message_correct       = ls_message_correct
        es_master_data_defective = ls_master_data_defective
        es_message_defective     = ls_message_defective.
    IF sy-subrc = 0 AND ls_message_defective-is_error IS INITIAL.
        DATA: ls_adrcomc TYPE adrcomc.
        SELECT SINGLE adrcomc~client adrcomc~addrnumber adrcomc~persnumber adrcomc~comm_type adrcomc~date_from adrcomc~high_value
        FROM adrcomc INNER JOIN kna1 ON ( kna1~adrnr = adrcomc~addrnumber )
        INTO ls_adrcomc
        WHERE kna1~kunnr = <fs_data_aux>-header-object_instance-kunnr
            AND adrcomc~comm_type = 'TEL'.
        IF sy-subrc = 0.
        ls_adrcomc-high_value = '000'.
        MODIFY adrcomc FROM ls_adrcomc.
        ENDIF.
    ELSE.
        PERFORM save_return TABLES ls_message_defective-messages USING p_telf-target.
        PERFORM add_alv_telf USING p_telf-target 1 p_exists.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        EXIT.
    ENDIF.
    ENDIF.

    CALL METHOD cmd_ei_api=>maintain_bapi
    EXPORTING
        iv_test_run              = cb_test
        is_master_data           = ls_master_data_aux
    IMPORTING
        es_master_data_correct   = ls_master_data_correct
        es_message_correct       = ls_message_correct
        es_master_data_defective = ls_master_data_defective
        es_message_defective     = ls_message_defective.
    IF sy-subrc = 0 AND ls_message_defective-is_error IS INITIAL.
**      INI ZPRY_PRS4 07.05.2024 54217049T
    PERFORM export_smtp_cl USING ls_master_data_aux.
**      FIN ZPRY_PRS4
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
        wait = 'X'.
    PERFORM save_return TABLES ls_message_correct-messages USING p_telf-target.
    PERFORM add_alv_telf USING p_telf-target 3 p_exists.
    ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    PERFORM save_return TABLES ls_message_defective-messages USING p_telf-target.
    PERFORM add_alv_telf USING p_telf-target 1 p_exists.
    ENDIF.
ENDIF.

ENDFORM.                    " CHANGE_DATA_CUSTOMER_TELF
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DATA_VENDOR_TELF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TELF text
*      -->P_EXISTS  text
*      <--P_MASTER_DATA  text
*----------------------------------------------------------------------*
FORM change_data_vendor_telf  USING    p_telf         TYPE ty_f_telf
                                        p_exists       TYPE c
                            CHANGING p_master_data  TYPE vmds_ei_main.

FIELD-SYMBOLS: <fs_data>        TYPE vmds_ei_extern,
                <fs_data_aux>    TYPE vmds_ei_extern.
DATA: ls_master_data_correct    TYPE vmds_ei_main,
        ls_master_data_aux        TYPE vmds_ei_main,
        ls_message_correct        TYPE cvis_message,
        ls_master_data_defective  TYPE vmds_ei_main,
        ls_message_defective      TYPE cvis_message,
        lv_telf_deletion          TYPE abap_bool.

READ TABLE p_master_data-vendors ASSIGNING <fs_data> INDEX 1.
IF sy-subrc = 0.
    <fs_data>-header-object_task = 'M'.

    ls_master_data_aux = p_master_data.

    READ TABLE ls_master_data_aux-vendors ASSIGNING <fs_data_aux> INDEX 1.
    IF sy-subrc = 0.
    DELETE <fs_data>-central_data-address-communication-phone-phone WHERE contact-task = 'I'.
    DELETE <fs_data_aux>-central_data-address-communication-phone-phone WHERE contact-task = 'D'.

    READ TABLE <fs_data>-central_data-address-communication-phone-phone WITH KEY contact-task = 'D' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
        lv_telf_deletion = abap_true.
    ENDIF.
    ENDIF.

    IF lv_telf_deletion = abap_true.
    CALL METHOD vmd_ei_api=>maintain_bapi
        EXPORTING
        iv_test_run              = cb_test
        is_master_data           = p_master_data
        IMPORTING
        es_master_data_correct   = ls_master_data_correct
        es_message_correct       = ls_message_correct
        es_master_data_defective = ls_master_data_defective
        es_message_defective     = ls_message_defective.
    IF sy-subrc = 0 AND ls_message_defective-is_error IS INITIAL.
        DATA: ls_adrcomc TYPE adrcomc.
        SELECT SINGLE *
        FROM adrcomc
        INTO ls_adrcomc
        WHERE addrnumber = <fs_data_aux>-central_data-central-data-adrnr
            AND comm_type = 'TEL'.
        IF sy-subrc = 0.
        ls_adrcomc-high_value = '000'.
        MODIFY adrcomc FROM ls_adrcomc.
        ENDIF.
    ELSE.
        PERFORM save_return TABLES ls_message_defective-messages USING p_telf-target.
        PERFORM add_alv_telf USING p_telf-target 1 p_exists.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        EXIT.
    ENDIF.
    ENDIF.

    CALL METHOD vmd_ei_api=>maintain_bapi
    EXPORTING
        iv_test_run              = cb_test
        is_master_data           = ls_master_data_aux
    IMPORTING
        es_master_data_correct   = ls_master_data_correct
        es_message_correct       = ls_message_correct
        es_master_data_defective = ls_master_data_defective
        es_message_defective     = ls_message_defective.
    IF sy-subrc = 0 AND ls_message_defective-is_error IS INITIAL.
**      INI ZPRY_PRS4 30.04.2024 54217049T
    PERFORM save_srm_from_telf USING ls_master_data_aux.
    PERFORM export_smtp USING ls_master_data_aux.
**      FIN ZPRY_PRS4
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
        wait = 'X'.
    PERFORM save_return TABLES ls_message_correct-messages USING p_telf-target.
    PERFORM add_alv_telf USING p_telf-target 3 p_exists.
    ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    PERFORM save_return TABLES ls_message_defective-messages USING p_telf-target.
    PERFORM add_alv_telf USING p_telf-target 1 p_exists.
    ENDIF.
ENDIF.

ENDFORM.                    " CHANGE_DATA_VENDOR_TELF
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DATA_CUSTOMER_SMTP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILE  text
*      <--P_MASTER_DATA  text
*----------------------------------------------------------------------*
FORM change_data_customer_smtp  USING     p_smtp         TYPE ty_f_smtp
                                        p_exists       TYPE c
                                CHANGING  p_master_data  TYPE cmds_ei_main.

FIELD-SYMBOLS: <fs_data>          TYPE cmds_ei_extern,
                <fs_data_aux>      TYPE cmds_ei_extern.
DATA: ls_master_data_correct      TYPE cmds_ei_main,
        ls_master_data_aux          TYPE cmds_ei_main,
        ls_message_correct          TYPE cvis_message,
        ls_master_data_defective    TYPE cmds_ei_main,
        ls_message_defective        TYPE cvis_message,
        lv_smtp_deletion            TYPE abap_bool.

READ TABLE p_master_data-customers ASSIGNING <fs_data> INDEX 1.
IF sy-subrc = 0.
    <fs_data>-header-object_task = 'M'.

    ls_master_data_aux = p_master_data.

    READ TABLE ls_master_data_aux-customers ASSIGNING <fs_data_aux> INDEX 1.
    IF sy-subrc = 0.
    DELETE <fs_data>-central_data-address-communication-smtp-smtp WHERE contact-task = 'I'.
    DELETE <fs_data_aux>-central_data-address-communication-smtp-smtp WHERE contact-task = 'D'.

    READ TABLE <fs_data>-central_data-address-communication-smtp-smtp WITH KEY contact-task = 'D' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
        lv_smtp_deletion = abap_true.
    ENDIF.
    ENDIF.

    IF lv_smtp_deletion = abap_true.
    CALL METHOD cmd_ei_api=>maintain_bapi
        EXPORTING
        iv_test_run              = cb_test
        is_master_data           = p_master_data
        IMPORTING
        es_master_data_correct   = ls_master_data_correct
        es_message_correct       = ls_message_correct
        es_master_data_defective = ls_master_data_defective
        es_message_defective     = ls_message_defective.
    IF sy-subrc = 0 AND ls_message_defective-is_error IS INITIAL.
        DATA: ls_adrcomc TYPE adrcomc.
        SELECT SINGLE adrcomc~client adrcomc~addrnumber adrcomc~persnumber adrcomc~comm_type adrcomc~date_from adrcomc~high_value
        FROM adrcomc INNER JOIN kna1 ON ( kna1~adrnr = adrcomc~addrnumber )
        INTO ls_adrcomc
        WHERE kna1~kunnr = <fs_data_aux>-header-object_instance-kunnr
            AND adrcomc~comm_type = 'INT'.
        IF sy-subrc = 0.
        ls_adrcomc-high_value = '000'.
        MODIFY adrcomc FROM ls_adrcomc.
        ENDIF.
    ELSE.
        PERFORM save_return TABLES ls_message_defective-messages USING p_smtp-target.
        PERFORM add_alv_smtp USING p_smtp-target 1 p_exists.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        EXIT.
    ENDIF.
    ENDIF.

    CALL METHOD cmd_ei_api=>maintain_bapi
    EXPORTING
        iv_test_run              = cb_test
        is_master_data           = ls_master_data_aux
    IMPORTING
        es_master_data_correct   = ls_master_data_correct
        es_message_correct       = ls_message_correct
        es_master_data_defective = ls_master_data_defective
        es_message_defective     = ls_message_defective.
    IF sy-subrc = 0 AND ls_message_defective-is_error IS INITIAL.
    PERFORM save_return TABLES ls_message_correct-messages USING p_smtp-target.
    PERFORM add_alv_smtp USING p_smtp-target 3 p_exists.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
        wait = 'X'.
    ELSE.
    PERFORM save_return TABLES ls_message_defective-messages USING p_smtp-target.
    PERFORM add_alv_smtp USING p_smtp-target 1 p_exists.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.
ENDIF.

ENDFORM.                    " CHANGE_DATA_CUSTOMER_SMTP
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DATA_VENDOR_SMTP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SMTP text
*      -->P_EXISTS  text
*      <--P_MASTER_DATA  text
*----------------------------------------------------------------------*
FORM change_data_vendor_smtp  USING    p_smtp         TYPE ty_f_smtp
                                        p_exists       TYPE c
                            CHANGING p_master_data  TYPE vmds_ei_main.

FIELD-SYMBOLS: <fs_data>          TYPE vmds_ei_extern,
                <fs_data_aux>      TYPE vmds_ei_extern.
DATA: ls_master_data_correct      TYPE vmds_ei_main,
        ls_master_data_aux          TYPE vmds_ei_main,
        ls_message_correct          TYPE cvis_message,
        ls_master_data_defective    TYPE vmds_ei_main,
        ls_message_defective        TYPE cvis_message,
        lv_smtp_deletion            TYPE abap_bool.

READ TABLE p_master_data-vendors ASSIGNING <fs_data> INDEX 1.
IF sy-subrc = 0.
    <fs_data>-header-object_task = 'M'.

    ls_master_data_aux = p_master_data.

    READ TABLE ls_master_data_aux-vendors ASSIGNING <fs_data_aux> INDEX 1.
    IF sy-subrc = 0.
    DELETE <fs_data>-central_data-address-communication-smtp-smtp WHERE contact-task = 'I'.
    DELETE <fs_data_aux>-central_data-address-communication-smtp-smtp WHERE contact-task = 'D'.

    READ TABLE <fs_data>-central_data-address-communication-smtp-smtp WITH KEY contact-task = 'D' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
        lv_smtp_deletion = abap_true.
    ENDIF.
    ENDIF.

    IF lv_smtp_deletion = abap_true.
    CALL METHOD vmd_ei_api=>maintain_bapi
        EXPORTING
        iv_test_run              = cb_test
        is_master_data           = p_master_data
        IMPORTING
        es_master_data_correct   = ls_master_data_correct
        es_message_correct       = ls_message_correct
        es_master_data_defective = ls_master_data_defective
        es_message_defective     = ls_message_defective.
    IF sy-subrc = 0 AND ls_message_defective-is_error IS INITIAL.
        DATA: ls_adrcomc TYPE adrcomc.
        SELECT SINGLE *
        FROM adrcomc
        INTO ls_adrcomc
        WHERE addrnumber = <fs_data_aux>-central_data-central-data-adrnr
            AND comm_type = 'INT'.
        IF sy-subrc = 0.
        ls_adrcomc-high_value = '000'.
        MODIFY adrcomc FROM ls_adrcomc.
        ENDIF.
    ELSE.
        PERFORM save_return TABLES ls_message_defective-messages USING p_smtp-target.
        PERFORM add_alv_smtp USING p_smtp-target 1 p_exists.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        EXIT.
    ENDIF.
    ENDIF.

    CALL METHOD vmd_ei_api=>maintain_bapi
    EXPORTING
        iv_test_run              = cb_test
        is_master_data           = ls_master_data_aux
    IMPORTING
        es_master_data_correct   = ls_master_data_correct
        es_message_correct       = ls_message_correct
        es_master_data_defective = ls_master_data_defective
        es_message_defective     = ls_message_defective.
    IF sy-subrc = 0 AND ls_message_defective-is_error IS INITIAL.
    PERFORM save_return TABLES ls_message_correct-messages USING p_smtp-target.
    PERFORM add_alv_smtp USING p_smtp-target 3 p_exists.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
        wait = 'X'.
    ELSE.
    PERFORM save_return TABLES ls_message_defective-messages USING p_smtp-target.
    PERFORM add_alv_smtp USING p_smtp-target 1 p_exists.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.
ENDIF.

ENDFORM.                    " CHANGE_DATA_VENDOR_SMTP
*&---------------------------------------------------------------------*
*&      Form  FILL_ADRESS_DATA_CUSTOMER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILE  text
*      <--P_DATA  text
*----------------------------------------------------------------------*
FORM fill_adress_data_customer  USING    p_file TYPE ty_file
                                CHANGING p_data TYPE cmds_ei_extern.

p_data-central_data-address-postal-data-title = p_file-title.
p_data-central_data-address-postal-datax-title = 'X'.

p_data-central_data-address-postal-data-name = p_file-name.
p_data-central_data-address-postal-datax-name = 'X'.

p_data-central_data-address-postal-data-name_2 = p_file-name_2.
p_data-central_data-address-postal-datax-name_2 = 'X'.

p_data-central_data-address-postal-data-city = p_file-city.
p_data-central_data-address-postal-datax-city = 'X'.

p_data-central_data-address-postal-data-postl_cod1 = p_file-postl_cod1.
p_data-central_data-address-postal-datax-postl_cod1 = 'X'.

p_data-central_data-address-postal-data-street = p_file-street.
p_data-central_data-address-postal-datax-street = 'X'.

p_data-central_data-address-postal-data-house_no = p_file-house_no.
p_data-central_data-address-postal-datax-house_no = 'X'.

p_data-central_data-address-postal-data-country = p_file-country.
p_data-central_data-address-postal-datax-country = 'X'.

p_data-central_data-address-postal-data-langu_iso = p_file-langu_iso.
p_data-central_data-address-postal-datax-langu_iso = 'X'.

p_data-central_data-address-postal-data-region = p_file-region.
p_data-central_data-address-postal-datax-region = 'X'.

p_data-central_data-address-postal-data-sort1 = p_file-sort1.
p_data-central_data-address-postal-datax-sort1 = 'X'.

p_data-central_data-address-postal-data-sort2 = p_file-sort2.
p_data-central_data-address-postal-datax-sort2 = 'X'.

p_data-central_data-address-postal-data-time_zone = p_file-time_zone.
p_data-central_data-address-postal-datax-time_zone = 'X'.

p_data-central_data-address-postal-data-house_no2 = p_file-house_no2.
p_data-central_data-address-postal-datax-house_no2 = 'X'.

p_data-central_data-address-postal-data-name_3 = p_file-name_3.
p_data-central_data-address-postal-datax-name_3 = 'X'.

p_data-central_data-address-postal-data-name_4 = p_file-name_4.
p_data-central_data-address-postal-datax-name_4 = 'X'.

p_data-central_data-address-postal-data-str_suppl1 = p_file-str_suppl1.
p_data-central_data-address-postal-datax-str_suppl1 = 'X'.

p_data-central_data-address-postal-data-str_suppl2 = p_file-str_suppl2.
p_data-central_data-address-postal-datax-str_suppl2 = 'X'.

p_data-central_data-address-postal-data-location = p_file-location.
p_data-central_data-address-postal-datax-location = 'X'.

DATA: ls_rem TYPE cvis_ei_rem.
ls_rem-task = 'I'.
ls_rem-data-adr_notes = p_file-remark.
ls_rem-datax-adr_notes = 'X'.
ls_rem-data-langu_iso = 'ES'.
ls_rem-datax-langu_iso = 'X'.
APPEND ls_rem TO p_data-central_data-address-remark-remarks.

DATA: ls_fax        TYPE cvis_ei_fax_str,
        ls_fax_remark TYPE cvis_ei_comrem.
ls_fax-contact-task = 'I'.
ls_fax-contact-data-fax = p_file-fax_number.
ls_fax-contact-data-extension = p_file-fax_extens.
ls_fax-contact-datax-fax = 'X'.
ls_fax-contact-datax-extension = 'X'.

ls_fax_remark-task = 'I'.
ls_fax_remark-data-comm_type = 'FAX'.
ls_fax_remark-data-langu_iso = 'ES'.
ls_fax_remark-data-comm_notes = p_file-fax_comment.
ls_fax_remark-datax-comm_type = 'X'.
ls_fax_remark-datax-langu_iso = 'X'.
ls_fax_remark-datax-comm_notes = 'X'.
APPEND ls_fax_remark TO ls_fax-remark-remarks.

APPEND ls_fax TO p_data-central_data-address-communication-fax-fax.

ENDFORM.                    " FILL_ADRESS_DATA_CUSTOMER
*&---------------------------------------------------------------------*
*&      Form  FILL_ADRESS_DATA_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILE  text
*      <--P_DATA>  text
*----------------------------------------------------------------------*
FORM fill_adress_data_vendor  USING    p_file TYPE ty_file
                            CHANGING p_data TYPE vmds_ei_extern.

p_data-central_data-address-postal-data-title = p_file-title.
p_data-central_data-address-postal-datax-title = 'X'.

p_data-central_data-address-postal-data-name = p_file-name.
p_data-central_data-address-postal-datax-name = 'X'.

p_data-central_data-address-postal-data-name_2 = p_file-name_2.
p_data-central_data-address-postal-datax-name_2 = 'X'.

p_data-central_data-address-postal-data-city = p_file-city.
p_data-central_data-address-postal-datax-city = 'X'.

p_data-central_data-address-postal-data-postl_cod1 = p_file-postl_cod1.
p_data-central_data-address-postal-datax-postl_cod1 = 'X'.

p_data-central_data-address-postal-data-street = p_file-street.
p_data-central_data-address-postal-datax-street = 'X'.

p_data-central_data-address-postal-data-house_no = p_file-house_no.
p_data-central_data-address-postal-datax-house_no = 'X'.

p_data-central_data-address-postal-data-country = p_file-country.
p_data-central_data-address-postal-datax-country = 'X'.

p_data-central_data-address-postal-data-langu_iso = p_file-langu_iso.
p_data-central_data-address-postal-datax-langu_iso = 'X'.

p_data-central_data-address-postal-data-region = p_file-region.
p_data-central_data-address-postal-datax-region = 'X'.

p_data-central_data-address-postal-data-sort1 = p_file-sort1.
p_data-central_data-address-postal-datax-sort1 = 'X'.

p_data-central_data-address-postal-data-sort2 = p_file-sort2.
p_data-central_data-address-postal-datax-sort2 = 'X'.

p_data-central_data-address-postal-data-time_zone = p_file-time_zone.
p_data-central_data-address-postal-datax-time_zone = 'X'.

p_data-central_data-address-postal-data-house_no2 = p_file-house_no2.
p_data-central_data-address-postal-datax-house_no2 = 'X'.

p_data-central_data-address-postal-data-name_3 = p_file-name_3.
p_data-central_data-address-postal-datax-name_3 = 'X'.

p_data-central_data-address-postal-data-name_4 = p_file-name_4.
p_data-central_data-address-postal-datax-name_4 = 'X'.

p_data-central_data-address-postal-data-str_suppl1 = p_file-str_suppl1.
p_data-central_data-address-postal-datax-str_suppl1 = 'X'.

p_data-central_data-address-postal-data-str_suppl2 = p_file-str_suppl2.
p_data-central_data-address-postal-datax-str_suppl2 = 'X'.

p_data-central_data-address-postal-data-location = p_file-location.
p_data-central_data-address-postal-datax-location = 'X'.

DATA: ls_rem TYPE cvis_ei_rem.
ls_rem-task = 'I'.
ls_rem-data-adr_notes = p_file-remark.
ls_rem-datax-adr_notes = 'X'.
ls_rem-data-langu_iso = 'ES'.
ls_rem-datax-langu_iso = 'X'.
APPEND ls_rem TO p_data-central_data-address-remark-remarks.

DATA: ls_fax        TYPE cvis_ei_fax_str,
        ls_fax_remark TYPE cvis_ei_comrem.
ls_fax-contact-task = 'I'.
ls_fax-contact-data-fax = p_file-fax_number.
ls_fax-contact-data-extension = p_file-fax_extens.
ls_fax-contact-datax-fax = 'X'.
ls_fax-contact-datax-extension = 'X'.

ls_fax_remark-task = 'I'.
ls_fax_remark-data-comm_type = 'FAX'.
ls_fax_remark-data-langu_iso = 'ES'.
ls_fax_remark-data-comm_notes = p_file-fax_comment.
ls_fax_remark-datax-comm_type = 'X'.
ls_fax_remark-datax-langu_iso = 'X'.
ls_fax_remark-datax-comm_notes = 'X'.
APPEND ls_fax_remark TO ls_fax-remark-remarks.

APPEND ls_fax TO p_data-central_data-address-communication-fax-fax.

ENDFORM.                    " FILL_ADRESS_DATA_VENDOR
*&---------------------------------------------------------------------*
*&      Form  FILL_TELF_DATA_CUSTOMER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*      <--P_DATA  text
*----------------------------------------------------------------------*
FORM fill_telf_data_customer  USING    p_target
                            CHANGING p_data   TYPE cmds_ei_extern.

DATA: ls_telf         TYPE ty_f_telf,
        ls_phone_remark TYPE cvis_ei_comrem,
        ls_phone        TYPE cvis_ei_phone_str.

LOOP AT gt_f_telf INTO ls_telf WHERE target = p_target.
    ls_phone-contact-task = 'I'.
    ls_phone-contact-data-telephone = ls_telf-number.
    ls_phone-contact-data-extension = ls_telf-extens.
    ls_phone-contact-datax-telephone = 'X'.
    ls_phone-contact-datax-extension = 'X'.

    ls_phone_remark-task = 'I'.
    ls_phone_remark-data-comm_type = 'TEL'.
    ls_phone_remark-data-langu_iso = 'ES'.
    ls_phone_remark-data-comm_notes = ls_telf-comment.
    ls_phone_remark-datax-comm_type = 'X'.
    ls_phone_remark-datax-langu_iso = 'X'.
    ls_phone_remark-datax-comm_notes = 'X'.
    APPEND ls_phone_remark TO ls_phone-remark-remarks.
    CLEAR: ls_phone_remark.

    APPEND ls_phone TO p_data-central_data-address-communication-phone-phone.
    CLEAR: ls_phone-remark-remarks.
ENDLOOP.

ENDFORM.                    " FILL_TELF_DATA_CUSTOMER
*&---------------------------------------------------------------------*
*&      Form  FILL_TELF_DATA_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*      <--P_DATA  text
*----------------------------------------------------------------------*
FORM fill_telf_data_vendor    USING    p_target
                            CHANGING p_data   TYPE vmds_ei_extern.

DATA: ls_telf         TYPE ty_f_telf,
        ls_phone_remark TYPE cvis_ei_comrem,
        ls_phone        TYPE cvis_ei_phone_str.

LOOP AT gt_f_telf INTO ls_telf WHERE target = p_target.
    ls_phone-contact-task = 'I'.
    ls_phone-contact-data-telephone = ls_telf-number.
    ls_phone-contact-data-extension = ls_telf-extens.
    ls_phone-contact-datax-telephone = 'X'.
    ls_phone-contact-datax-extension = 'X'.

    ls_phone_remark-task = 'I'.
    ls_phone_remark-data-comm_type = 'TEL'.
    ls_phone_remark-data-langu_iso = 'ES'.
    ls_phone_remark-data-comm_notes = ls_telf-comment.
    ls_phone_remark-datax-comm_type = 'X'.
    ls_phone_remark-datax-langu_iso = 'X'.
    ls_phone_remark-datax-comm_notes = 'X'.
    APPEND ls_phone_remark TO ls_phone-remark-remarks.
    CLEAR: ls_phone_remark.

    APPEND ls_phone TO p_data-central_data-address-communication-phone-phone.
    CLEAR: ls_phone-remark-remarks.
ENDLOOP.

ENDFORM.                    " FILL_TELF_DATA_VENDOR
*&---------------------------------------------------------------------*
*&      Form  FILL_SMTP_DATA_CUSTOMER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*      <--P_DATA  text
*----------------------------------------------------------------------*
FORM fill_smtp_data_customer  USING    p_target
                            CHANGING p_data   TYPE cmds_ei_extern.

DATA: ls_smtp_addr  TYPE ty_f_smtp,
        ls_smtp       TYPE cvis_ei_smtp_str,
        ls_remark     TYPE cvis_ei_comrem.

LOOP AT gt_f_smtp INTO ls_smtp_addr WHERE target = p_target.
    ls_smtp-contact-task = 'I'.
    ls_smtp-contact-data-e_mail = ls_smtp_addr-smtp_addr.
    ls_smtp-contact-datax-e_mail = 'X'.

    ls_remark-task = 'I'.
    ls_remark-data-comm_notes = ls_smtp_addr-comment.
    ls_remark-data-comm_type = 'INT'.
    ls_remark-data-langu = 'S'.
    ls_remark-data-langu_iso = 'ES'.
    ls_remark-datax-comm_notes = 'X'.
    ls_remark-datax-comm_type = 'X'.
    ls_remark-datax-langu = 'X'.
    ls_remark-datax-langu_iso = 'X'.

    APPEND ls_remark TO ls_smtp-remark-remarks.
    APPEND ls_smtp TO p_data-central_data-address-communication-smtp-smtp.
    CLEAR: ls_smtp.
ENDLOOP.

ENDFORM.                    " FILL_TELF_DATA_CUSTOMER
*&---------------------------------------------------------------------*
*&      Form  FILL_SMTP_DATA_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*      <--P_DATA  text
*----------------------------------------------------------------------*
FORM fill_smtp_data_vendor    USING    p_target
                            CHANGING p_data   TYPE vmds_ei_extern.

DATA: ls_smtp_addr  TYPE ty_f_smtp,
        ls_smtp       TYPE cvis_ei_smtp_str,
        ls_remark     TYPE cvis_ei_comrem.
DATA lv_cons TYPE i.
LOOP AT gt_f_smtp INTO ls_smtp_addr WHERE target = p_target.
    ls_smtp-contact-task = 'I'.
    ls_smtp-contact-data-e_mail = ls_smtp_addr-smtp_addr.
    ls_smtp-contact-datax-e_mail = 'X'.

    ls_remark-task = 'I'.
    ls_remark-data-comm_notes = ls_smtp_addr-comment.
    ls_remark-data-comm_type = 'INT'.
    ls_remark-data-langu = 'S'.
    ls_remark-data-langu_iso = 'ES'.
    ls_remark-datax-comm_notes = 'X'.
    ls_remark-datax-comm_type = 'X'.
    ls_remark-datax-langu = 'X'.
    ls_remark-datax-langu_iso = 'X'.

    APPEND ls_remark TO ls_smtp-remark-remarks.
    APPEND ls_smtp TO p_data-central_data-address-communication-smtp-smtp.
    CLEAR: ls_smtp.
ENDLOOP.

ENDFORM.                    " FILL_TELF_DATA_VENDOR
*&---------------------------------------------------------------------*
*&      Form  DELETE_CUSTOMER_OLD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TAB  text
*      <--P_MASTER_DATA  text
*----------------------------------------------------------------------*
FORM delete_customer_old   USING    p_tab         TYPE c
                            CHANGING p_master_data TYPE cmds_ei_main.

FIELD-SYMBOLS: <fs_customers> TYPE cmds_ei_extern,
                <fs_tel>       TYPE cvis_ei_phone_str,
                <fs_fax>       TYPE cvis_ei_fax_str,
                <fs_smtp>      TYPE cvis_ei_smtp_str,
                <fs_rem>       TYPE cvis_ei_rem,
                <fs_sales>     TYPE cmds_ei_sales.

READ TABLE p_master_data-customers ASSIGNING <fs_customers> INDEX 1.
IF sy-subrc = 0.
    CASE p_tab.
    WHEN lc_tab_dirc.
        LOOP AT <fs_customers>-central_data-address-communication-fax-fax ASSIGNING <fs_fax>.
        <fs_fax>-contact-task = 'D'.
        ENDLOOP.
        LOOP AT <fs_customers>-central_data-address-remark-remarks ASSIGNING <fs_rem>.
        <fs_rem>-task = 'D'.
        ENDLOOP.

    WHEN lc_tab_telf.
        LOOP AT <fs_customers>-central_data-address-communication-phone-phone ASSIGNING <fs_tel>.
        <fs_tel>-contact-task = 'D'.
        ENDLOOP.

    WHEN lc_tab_smtp.
        LOOP AT <fs_customers>-central_data-address-communication-smtp-smtp ASSIGNING <fs_smtp>.
        <fs_smtp>-contact-task = 'D'.
        ENDLOOP.

    ENDCASE.
    CLEAR: <fs_customers>-company_data-current_state.
    DELETE <fs_customers>-company_data-company WHERE task IS INITIAL.
    CLEAR: <fs_customers>-sales_data-current_state.
    DELETE <fs_customers>-sales_data-sales WHERE task IS INITIAL.
ENDIF.

ENDFORM.                    " DELETE_CUSTOMER_OLD
*&---------------------------------------------------------------------*
*&      Form  DELETE_VENDOR_OLD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILE  text
*      <--P_MASTER_DATA  text
*----------------------------------------------------------------------*
FORM delete_vendor_old  USING    p_tab          TYPE c
                        CHANGING p_master_data  TYPE vmds_ei_main.

FIELD-SYMBOLS: <fs_vendors>   TYPE vmds_ei_extern,
                <fs_tel>       TYPE cvis_ei_phone_str,
                <fs_fax>       TYPE cvis_ei_fax_str,
                <fs_smtp>      TYPE cvis_ei_smtp_str,
                <fs_rem>       TYPE cvis_ei_rem.

READ TABLE p_master_data-vendors ASSIGNING <fs_vendors> INDEX 1.
IF sy-subrc = 0.
    CASE p_tab.
    WHEN lc_tab_dirc.
        LOOP AT <fs_vendors>-central_data-address-communication-fax-fax ASSIGNING <fs_fax>.
        <fs_fax>-contact-task = 'D'.
        ENDLOOP.
        LOOP AT <fs_vendors>-central_data-address-remark-remarks ASSIGNING <fs_rem>.
        <fs_rem>-task = 'D'.
        ENDLOOP.

    WHEN lc_tab_telf.
        LOOP AT <fs_vendors>-central_data-address-communication-phone-phone ASSIGNING <fs_tel>.
        <fs_tel>-contact-task = 'D'.
        ENDLOOP.

    WHEN lc_tab_smtp.
        LOOP AT <fs_vendors>-central_data-address-communication-smtp-smtp ASSIGNING <fs_smtp>.
        <fs_smtp>-contact-task = 'D'.
        ENDLOOP.

    ENDCASE.
    CLEAR: <fs_vendors>-purchasing_data-current_state.
    DELETE <fs_vendors>-purchasing_data-purchasing WHERE task IS INITIAL.
    CLEAR: <fs_vendors>-company_data-current_state.
    DELETE <fs_vendors>-company_data-company WHERE task IS INITIAL.
ENDIF.

ENDFORM.                    " DELETE_VENDORS_OLD
*&---------------------------------------------------------------------*
*&      Form  CLIENT_LOGIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*      -->P_EXISTS  text
*----------------------------------------------------------------------*
FORM client_logic  USING    p_target
                            p_exists.

DATA: ls_file                   TYPE ty_file,
        ls_telf                   TYPE ty_f_telf,
        ls_smtp                   TYPE ty_f_smtp,
        lv_customer_master_data   TYPE cmds_ei_main.

FIELD-SYMBOLS <fs_data>         TYPE cmds_ei_extern.

"Comprobación tabla direcciónes
READ TABLE gt_file INTO ls_file WITH KEY target = p_target.
IF sy-subrc = 0.
    IF p_exists IS INITIAL.
    PERFORM add_alv_addr USING p_target 1 p_exists.
    ELSE.
    PERFORM fill_customer_addr USING ls_file CHANGING lv_customer_master_data.
    PERFORM delete_customer_old USING lc_tab_dirc CHANGING lv_customer_master_data.
    READ TABLE lv_customer_master_data-customers ASSIGNING <fs_data> INDEX 1.
    IF sy-subrc = 0.
        <fs_data>-central_data-address-task = 'U'.
        PERFORM fill_adress_data_customer USING ls_file CHANGING <fs_data>.
        IF lv_customer_master_data IS NOT INITIAL.
        PERFORM change_data_customer_addr USING ls_file p_exists CHANGING lv_customer_master_data.
        ENDIF.
    ENDIF.
    ENDIF.
ENDIF.

"Comprobación tabla teléfonos
READ TABLE gt_f_telf INTO ls_telf WITH KEY target = p_target.
IF sy-subrc = 0.
    IF p_exists IS INITIAL.
    PERFORM add_alv_telf USING p_target 1 p_exists.
    ELSE.
    PERFORM fill_customer_telf USING ls_telf CHANGING lv_customer_master_data.
    PERFORM delete_customer_old USING lc_tab_telf CHANGING lv_customer_master_data.
    READ TABLE lv_customer_master_data-customers ASSIGNING <fs_data> INDEX 1.
    IF sy-subrc = 0.
        <fs_data>-central_data-address-task = 'U'.
        PERFORM fill_telf_data_customer USING p_target CHANGING <fs_data>.
        IF lv_customer_master_data IS NOT INITIAL.
        PERFORM change_data_customer_telf USING ls_telf p_exists CHANGING lv_customer_master_data.
        ENDIF.
    ENDIF.
    ENDIF.
ENDIF.

"Comprobación tabla correos
READ TABLE gt_f_smtp INTO ls_smtp WITH KEY target = p_target.
IF sy-subrc = 0.
    IF p_exists IS INITIAL.
    PERFORM add_alv_smtp USING p_target 1 p_exists.
    ELSE.
**      INI ZPRY_PRS4 12.04.2024 54217049T
*      PERFORM fill_customer_smtp USING ls_smtp CHANGING lv_customer_master_data.
*      PERFORM delete_customer_old USING lc_tab_smtp CHANGING lv_customer_master_data.
*      READ TABLE lv_customer_master_data-customers ASSIGNING <fs_data> INDEX 1.
*      IF sy-subrc = 0.
*        <fs_data>-central_data-address-task = 'U'.
*        PERFORM fill_smtp_data_customer USING p_target CHANGING <fs_data>.
*        IF lv_customer_master_data IS NOT INITIAL.
*          PERFORM change_data_customer_smtp USING ls_smtp p_exists CHANGING lv_customer_master_data.
*        ENDIF.
*      ENDIF.

    PERFORM modify_data_customer_smtp USING ls_smtp-target p_exists.
**      FIN ZPRY_PRS4
    ENDIF.
ENDIF.

ENDFORM.                    " CLIENT_LOGIC
*&---------------------------------------------------------------------*
*&      Form  VENDOR_LOGIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*      -->P_EXISTS  text
*----------------------------------------------------------------------*
FORM vendor_logic  USING    p_target
                            p_exists.

DATA: ls_file               TYPE ty_file,
        ls_telf               TYPE ty_f_telf,
        ls_smtp               TYPE ty_f_smtp,
        lv_vendor_master_data TYPE vmds_ei_main.

FIELD-SYMBOLS <fs_data>         TYPE vmds_ei_extern.

"Comprobación tabla direcciónes
READ TABLE gt_file INTO ls_file WITH KEY target = p_target.
IF sy-subrc = 0.
    IF p_exists IS INITIAL.
    PERFORM add_alv_addr USING p_target 1 p_exists.
    ELSE.
    PERFORM fill_vendor_addr USING ls_file CHANGING lv_vendor_master_data.
    PERFORM delete_vendor_old USING lc_tab_dirc CHANGING lv_vendor_master_data.
    READ TABLE lv_vendor_master_data-vendors ASSIGNING <fs_data> INDEX 1.
    IF sy-subrc = 0.
        <fs_data>-central_data-address-task = 'U'.
        PERFORM fill_adress_data_vendor USING ls_file CHANGING <fs_data>.
        IF lv_vendor_master_data IS NOT INITIAL.
        PERFORM change_data_vendor_addr USING ls_file p_exists CHANGING lv_vendor_master_data.
        ENDIF.
    ENDIF.
    ENDIF.
ENDIF.

"Comprobación tabla teléfonos
READ TABLE gt_f_telf INTO ls_telf WITH KEY target = p_target.
IF sy-subrc = 0.
    IF p_exists IS INITIAL.
    PERFORM add_alv_telf USING p_target 1 p_exists.
    ELSE.
    PERFORM fill_vendor_telf USING ls_telf CHANGING lv_vendor_master_data.
    PERFORM delete_vendor_old USING lc_tab_telf CHANGING lv_vendor_master_data.
    READ TABLE lv_vendor_master_data-vendors ASSIGNING <fs_data> INDEX 1.
    IF sy-subrc = 0.
        <fs_data>-central_data-address-task = 'U'.
        PERFORM fill_telf_data_vendor USING p_target CHANGING <fs_data>.
        IF lv_vendor_master_data IS NOT INITIAL.
        PERFORM change_data_vendor_telf USING ls_telf p_exists CHANGING lv_vendor_master_data.
        ENDIF.
    ENDIF.
    ENDIF.
ENDIF.

"Comprobación tabla correos
READ TABLE gt_f_smtp INTO ls_smtp WITH KEY target = p_target.
IF sy-subrc = 0.
    IF p_exists IS INITIAL.
    PERFORM add_alv_smtp USING p_target 1 p_exists.
    ELSE.
**      INI ZPRY_PRS4 12.04.2024 54217049T
*      PERFORM fill_vendor_smtp USING ls_smtp CHANGING lv_vendor_master_data.
*      PERFORM delete_vendor_old USING lc_tab_smtp CHANGING lv_vendor_master_data.
*      READ TABLE lv_vendor_master_data-vendors ASSIGNING <fs_data> INDEX 1.
*      IF sy-subrc = 0.
*        <fs_data>-central_data-address-task = 'M'.
*        PERFORM fill_smtp_data_vendor USING p_target CHANGING <fs_data>.
*        IF lv_vendor_master_data IS NOT INITIAL.
*          PERFORM change_data_vendor_smtp USING ls_smtp p_exists CHANGING lv_vendor_master_data.
*        ENDIF.
*      ENDIF.

    PERFORM modify_data_vendor_smtp USING ls_smtp-target p_exists.
**      FIN ZPRY_PRS4
    ENDIF.
ENDIF.

ENDFORM.                    " VENDOR_LOGIC
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA_CUSTOMER_SMTP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*      -->P_EXISTS  text
*----------------------------------------------------------------------*
FORM modify_data_customer_smtp  USING    p_target
                                        p_exists.

DATA: ls_zprs4t0001 TYPE zprs4t0001,
        lv_adrnr      TYPE adrnr,
        ls_f_smtp     TYPE ty_f_smtp.

SELECT SINGLE adrnr
    FROM kna1
    INTO lv_adrnr
    WHERE kunnr = p_target.
IF lv_adrnr IS NOT INITIAL.
**    INI ZPRY_PRS4 10.05.2024 54217049T
    DATA: lv_datum TYPE datum,
        lv_uzeit TYPE uzeit.
    lv_datum = sy-datum.
    lv_uzeit = sy-uzeit.
**    FIN ZPRY_PRS4
    "Eliminar correos actuales
    IF cb_test IS INITIAL.
    SELECT SINGLE *
        FROM zprs4t0001
        INTO ls_zprs4t0001
        WHERE addrnumber = lv_adrnr.
    IF sy-subrc = 0.
        DELETE FROM zprs4t0001 WHERE addrnumber = lv_adrnr.
        IF sy-subrc <> 0.
**          INI ZPRY_PRS4 10.05.2024 54217049T
        PERFORM add_log_del USING p_target 1 lv_datum lv_uzeit.
**          FIN ZPRY_PRS4
        PERFORM add_alv_smtp USING p_target 1 p_exists.
        ROLLBACK WORK.
        EXIT.
**        INI ZPRY_PRS4 10.05.2024 54217049T
        ELSE.
        PERFORM add_log_del USING p_target 2 lv_datum lv_uzeit.
**        FIN ZPRY_PRS4
        ENDIF.
    ENDIF.
    ENDIF.

    LOOP AT gt_f_smtp INTO ls_f_smtp WHERE target = p_target.
    CLEAR: ls_zprs4t0001.
    ls_zprs4t0001-addrnumber = lv_adrnr.
    ls_zprs4t0001-high_value = ls_f_smtp-id.
    ls_zprs4t0001-smtp_addr = ls_f_smtp-smtp_addr.
    ls_zprs4t0001-principal = ls_f_smtp-principal.
    ls_zprs4t0001-comentario = ls_f_smtp-comment.
    "Insertar nuevos correos
    IF cb_test IS INITIAL.
        MODIFY zprs4t0001 FROM ls_zprs4t0001.
        IF sy-subrc <> 0.
**          INI ZPRY_PRS4 10.05.2024 54217049T
        PERFORM add_log USING p_target 1 ls_f_smtp-smtp_addr 'Error al insertar correo' lv_datum lv_uzeit.
**          FIN ZPRY_PRS4
        PERFORM add_alv_smtp USING p_target 1 p_exists.
        ROLLBACK WORK.
        EXIT.
**        INI ZPRY_PRS4 10.05.2024 54217049T
        ELSE.
        PERFORM add_log USING p_target 2 ls_f_smtp-smtp_addr 'Éxito al insertar correo' lv_datum lv_uzeit.
**        FIN ZPRY_PRS4
        ENDIF.
    ENDIF.
    ENDLOOP.
    PERFORM add_alv_smtp USING p_target 3 p_exists.
    COMMIT WORK AND WAIT.
ENDIF.

ENDFORM.                    " MODIFY_DATA_CUSTOMER_SMTP
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA_VENDOR_SMTP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*      -->P_EXISTS  text
*----------------------------------------------------------------------*
FORM modify_data_vendor_smtp    USING    p_target
                                        p_exists.

DATA: ls_zprs4t0001 TYPE zprs4t0001,
        lv_adrnr      TYPE adrnr,
        ls_f_smtp     TYPE ty_f_smtp.

SELECT SINGLE adrnr
    FROM lfa1
    INTO lv_adrnr
    WHERE lifnr = p_target.
IF lv_adrnr IS NOT INITIAL.
**    INI ZPRY_PRS4 10.05.2024 54217049T
    DATA: lv_datum TYPE datum,
        lv_uzeit TYPE uzeit.
    lv_datum = sy-datum.
    lv_uzeit = sy-uzeit.
**    FIN ZPRY_PRS4
    "Eliminar correos actuales
    IF cb_test IS INITIAL.
    SELECT SINGLE *
        FROM zprs4t0001
        INTO ls_zprs4t0001
        WHERE addrnumber = lv_adrnr.
    IF sy-subrc = 0.
        DELETE FROM zprs4t0001 WHERE addrnumber = lv_adrnr.
        IF sy-subrc <> 0.
**          INI ZPRY_PRS4 10.05.2024 54217049T
        PERFORM add_log_del USING p_target 1 lv_datum lv_uzeit.
**          FIN ZPRY_PRS4
        PERFORM add_alv_smtp USING p_target 1 p_exists.
        ROLLBACK WORK.
        EXIT.
**        INI ZPRY_PRS4 10.05.2024 54217049T
        ELSE.
        PERFORM add_log_del USING p_target 2 lv_datum lv_uzeit.
**        FIN ZPRY_PRS4
        ENDIF.
    ENDIF.
    ENDIF.

    LOOP AT gt_f_smtp INTO ls_f_smtp WHERE target = p_target.
    CLEAR: ls_zprs4t0001.
    ls_zprs4t0001-addrnumber = lv_adrnr.
    ls_zprs4t0001-high_value = ls_f_smtp-id.
    ls_zprs4t0001-smtp_addr = ls_f_smtp-smtp_addr.
    ls_zprs4t0001-principal = ls_f_smtp-principal.
    ls_zprs4t0001-comentario = ls_f_smtp-comment.
    "Insertar nuevos correos
    IF cb_test IS INITIAL.
        MODIFY zprs4t0001 FROM ls_zprs4t0001.
        IF sy-subrc <> 0.
**          INI ZPRY_PRS4 10.05.2024 54217049T
        PERFORM add_log USING p_target 1 ls_f_smtp-smtp_addr 'Error al insertar correo' lv_datum lv_uzeit.
**          FIN ZPRY_PRS4
        PERFORM add_alv_smtp USING p_target 1 p_exists.
        ROLLBACK WORK.
        EXIT.
**        INI ZPRY_PRS4 10.05.2024 54217049T
        ELSE.
        PERFORM add_log USING p_target 2 ls_f_smtp-smtp_addr 'Éxito al insertar correo' lv_datum lv_uzeit.
**        FIN ZPRY_PRS4
        ENDIF.
    ENDIF.
    ENDLOOP.
    PERFORM add_alv_smtp USING p_target 3 p_exists.

**    INI ZPRY_PRS4 30.04.2024 54217049T
    PERFORM save_srm_from_smtp USING p_target lv_adrnr.
**    FIN ZPRY_PRS4

    COMMIT WORK AND WAIT.
ENDIF.

ENDFORM.                    " MODIFY_DATA_VENDOR_SMTP
*&---------------------------------------------------------------------*
*&      Form  SAVE_SRM_FROM_ADDR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MASTER_DATA  text
*----------------------------------------------------------------------*
FORM save_srm_from_addr  USING    p_master_data TYPE vmds_ei_main.

DATA: lv_valid    TYPE c,
        lv_partner  TYPE bu_partner,
        ls_vendor   TYPE vmds_ei_extern.

READ TABLE p_master_data-vendors INTO ls_vendor INDEX 1.
IF sy-subrc = 0.
    PERFORM validar_prov USING    ls_vendor-header-object_instance-lifnr
                        CHANGING lv_valid
                                lv_partner.
ENDIF.

IF lv_valid = 'X'.
    DATA: ls_adr2   TYPE adr2,
        ls_adr3   TYPE adr3,
        ls_adr6   TYPE adr6,
        lt_xadr2  TYPE adr2_tab,
        lt_xadr3  TYPE adr3_tab,
        lt_xadr6  TYPE adr6_tab,
        ls_fax    TYPE cvis_ei_fax_str,
        ls_xadr2  LIKE LINE OF lt_xadr2,
        ls_xadr3  LIKE LINE OF lt_xadr3,
        ls_xadr6  LIKE LINE OF lt_xadr6.

    LOOP AT gt_adr2 INTO ls_adr2 WHERE addrnumber = ls_vendor-central_data-central-data-adrnr.
    MOVE-CORRESPONDING ls_adr2 TO ls_xadr2.
    APPEND ls_xadr2 TO lt_xadr2.
    CLEAR ls_xadr2.
    ENDLOOP.

    READ TABLE ls_vendor-central_data-address-communication-fax-fax INTO ls_fax INDEX 1.
    IF sy-subrc = 0.
    "FAX
    MOVE-CORRESPONDING ls_fax-contact-data TO ls_xadr3.
    ls_xadr3-country = ls_fax-contact-data-country.
    ls_xadr3-fax_number = ls_fax-contact-data-fax.
    ls_xadr3-fax_extens = ls_fax-contact-data-extension.
    APPEND ls_xadr3 TO lt_xadr3.

    "ADRC
    PERFORM fill_adrc_data USING ls_vendor-central_data-address-postal-data.
    ENDIF.

    LOOP AT gt_adr6 INTO ls_adr6 WHERE addrnumber = ls_vendor-central_data-central-data-adrnr.
    MOVE-CORRESPONDING ls_adr6 TO ls_xadr6.
    APPEND ls_xadr6 TO lt_xadr6.
    CLEAR ls_xadr6.
    ENDLOOP.

    PERFORM send_mod_srm TABLES lt_xadr2 lt_xadr3 lt_xadr6 USING ls_vendor-header-object_instance-lifnr
                                                                ls_vendor-central_data-central-data-adrnr lv_partner.

    PERFORM clear_globals.
ENDIF.

ENDFORM.                    " SAVE_SRM_FROM_ADDR
*&---------------------------------------------------------------------*
*&      Form  SAVE_SRM_FROM_TELF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MASTER_DATA  text
*----------------------------------------------------------------------*
FORM save_srm_from_telf  USING    p_master_data TYPE vmds_ei_main.

DATA: lv_valid    TYPE c,
        ls_f_telf   TYPE ty_f_telf,
        lv_partner  TYPE bu_partner,
        ls_vendor   TYPE vmds_ei_extern.

READ TABLE p_master_data-vendors INTO ls_vendor INDEX 1.
IF sy-subrc = 0.
    PERFORM validar_prov USING    ls_vendor-header-object_instance-lifnr
                        CHANGING lv_valid
                                lv_partner.
ENDIF.

IF lv_valid = 'X'.
    DATA: ls_adr3   TYPE adr3,
        ls_adr6   TYPE adr6,
        lt_xadr2  TYPE adr2_tab,
        lt_xadr3  TYPE adr3_tab,
        lt_xadr6  TYPE adr6_tab,
        ls_xadr2  LIKE LINE OF lt_xadr2,
        ls_xadr3  LIKE LINE OF lt_xadr3,
        ls_xadr6  LIKE LINE OF lt_xadr6.

    LOOP AT gt_f_telf INTO ls_f_telf WHERE target = ls_vendor-header-object_instance-lifnr.
    ls_xadr2-tel_number = ls_f_telf-number.
    ls_xadr2-tel_extens = ls_f_telf-extens.
    ls_xadr2-country = 'ES'.
    APPEND ls_xadr2 TO lt_xadr2.
    CLEAR ls_xadr2.
    ENDLOOP.

    LOOP AT gt_adr3 INTO ls_adr3 WHERE addrnumber = ls_vendor-central_data-central-data-adrnr.
    MOVE-CORRESPONDING ls_adr3 TO ls_xadr3.
    APPEND ls_xadr3 TO lt_xadr3.
    CLEAR ls_xadr3.
    ENDLOOP.

    LOOP AT gt_adr6 INTO ls_adr6 WHERE addrnumber = ls_vendor-central_data-central-data-adrnr.
    MOVE-CORRESPONDING ls_adr6 TO ls_xadr6.
    APPEND ls_xadr6 TO lt_xadr6.
    CLEAR ls_xadr6.
    ENDLOOP.

    PERFORM send_mod_srm TABLES lt_xadr2 lt_xadr3 lt_xadr6 USING ls_vendor-header-object_instance-lifnr
                                                                ls_vendor-central_data-central-data-adrnr lv_partner.

    PERFORM clear_globals.
ENDIF.

ENDFORM.                    " SAVE_SRM_FROM_TELF
*&---------------------------------------------------------------------*
*&      Form  SAVE_SRM_FROM_SMTP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*----------------------------------------------------------------------*
FORM save_srm_from_smtp  USING    p_target
                                p_adrnr.

DATA: lv_valid    TYPE c,
        lv_partner  TYPE bu_partner,
        ls_f_smtp   TYPE ty_f_smtp.

PERFORM validar_prov USING    p_target
                        CHANGING lv_valid
                                lv_partner.

IF lv_valid = 'X'.
    DATA: ls_adr2   TYPE adr2,
        ls_adr3   TYPE adr3,
        lt_xadr2  TYPE adr2_tab,
        lt_xadr3  TYPE adr3_tab,
        lt_xadr6  TYPE adr6_tab,
        ls_xadr2  LIKE LINE OF lt_xadr2,
        ls_xadr3  LIKE LINE OF lt_xadr3.

    LOOP AT gt_adr2 INTO ls_adr2 WHERE addrnumber = p_adrnr.
    MOVE-CORRESPONDING ls_adr2 TO ls_xadr2.
    APPEND ls_xadr2 TO lt_xadr2.
    CLEAR ls_xadr2.
    ENDLOOP.

    LOOP AT gt_adr3 INTO ls_adr3 WHERE addrnumber = p_adrnr.
    MOVE-CORRESPONDING ls_adr3 TO ls_xadr3.
    APPEND ls_xadr3 TO lt_xadr3.
    CLEAR ls_xadr3.
    ENDLOOP.

    DATA: ls_email TYPE zprs4t0001.
    REFRESH: gt_email.

    LOOP AT gt_f_smtp INTO ls_f_smtp WHERE target = p_target.
    ls_email-smtp_addr = ls_f_smtp-smtp_addr.
    APPEND ls_email TO gt_email.
    ENDLOOP.

    PERFORM send_mod_srm TABLES lt_xadr2 lt_xadr3 lt_xadr6 USING p_target
                                                                p_adrnr lv_partner.

    PERFORM clear_globals.
ENDIF.

ENDFORM.                    " SAVE_SRM_FROM_SMTP
*&---------------------------------------------------------------------*
*&      Form  RECUPERAR_HARDCODES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM recuperar_hardcodes .

DATA: lv_campo  TYPE string.
CONCATENATE sy-sysid sy-mandt INTO lv_campo.

DATA: ls_hardcodes TYPE zgrtutil0001,
        lt_hardcodes TYPE STANDARD TABLE OF zgrtutil0001.

* Recuperamos las costantes que se utilizarán a lo largo del programa
CALL FUNCTION 'ZGRUTIL001_RECUPERAR_HARDCODES'
    EXPORTING
    repid        = 'RFCORIGEN'
    TABLES
    it_hardcodes = lt_hardcodes
    EXCEPTIONS
    no_tiene     = 1
    OTHERS       = 2.

IF sy-subrc <> 0.
*   No están declaradas las constantes en la tabla ZGRTUTIL0001
    MESSAGE e005(zz).
ELSE.
    CONCATENATE sy-sysid sy-mandt INTO lv_campo.

    LOOP AT lt_hardcodes INTO ls_hardcodes.
    CASE ls_hardcodes-campo.
        WHEN lv_campo.
        IF ls_hardcodes-repid = 'RFCDESTINATION'.
            MOVE ls_hardcodes-low TO gv_destination.
        ENDIF.
    ENDCASE.
    ENDLOOP.
ENDIF.

ENDFORM.                    " RECUPERAR_HARDCODES
*&---------------------------------------------------------------------*
*&      Form  VALIDAR_PROV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIFNR  text
*      <--P_VALID  text
*----------------------------------------------------------------------*
FORM validar_prov  USING    p_lifnr
                    CHANGING p_valid
                            p_partner.

DATA: ls_proveedores TYPE ty_proveedores,
        lt_proveedores TYPE STANDARD TABLE OF ty_proveedores.

CALL FUNCTION 'ZSRM06_CONSULTA_PROVEEDORES'
    DESTINATION gv_destination
    EXPORTING
    lifnr           = p_lifnr
    TABLES
    lt_proveedores  = lt_proveedores
    EXCEPTIONS
    lifnr_no_existe = 1
    no_hay_datos    = 2
    type_error      = 3.
IF sy-subrc = 0.
    READ TABLE lt_proveedores INTO ls_proveedores INDEX 1.
    IF sy-subrc = 0.
    p_partner = ls_proveedores-vendor_no.
*      EXPORT partner = lv_partner TO MEMORY ID 'ZFIAP03ACRE01'.
    p_valid = 'X'.
    ENDIF.
ENDIF.

ENDFORM.                    " VALIDAR_PROV
*&---------------------------------------------------------------------*
*&      Form  SEND_MOD_SRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_XADR2  text
*      -->P_XADR3  text
*      -->P_XADR6  text
*      -->P_ADRNR  text
*      -->P_PARTNER  text
*----------------------------------------------------------------------*
FORM send_mod_srm  TABLES   p_xadr2 TYPE adr2_tab
                            p_xadr3 TYPE adr3_tab
                            p_xadr6 TYPE adr6_tab
                    USING    p_lifnr
                            p_adrnr
                            p_partner.
*  Telefonos
PERFORM busca_datos_adr2 TABLES p_xadr2.
*  Fax
PERFORM busca_datos_adr3 TABLES p_xadr3.
*  E-mail
PERFORM busca_datos_adr6 TABLES p_xadr6 USING p_lifnr p_adrnr.
*  SRM
PERFORM obtener_datos_acre_srm USING p_partner.

PERFORM mover_datos_modificados.

PERFORM zsrm08_modificacion_proveedor TABLES  gt_bapiadtel
                                                gt_bapiadtel_mod
                                                gt_bapiadfax
                                                gt_bapiadfax_mod
                                                gt_bapiadsmtp
                                                gt_bapiadsmtp_mod
                                                gt_taxnum_del
                                                gt_taxnum_ins
                                                gt_but_frg0061
                                                gt_errores
                                        USING p_partner
                                                gs_zmmpue0023
                                                gs_zmmpue0023_x
                                                gs_nombre
                                                gs_nombre_x
                                                gs_address
                                                gs_address_x
                                                gs_eeww_but000
                                                gs_eeww_but000_x
                                                cb_test.

ENDFORM.                    " SEND_MOD_SRM
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DATOS_ADR2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_XADR2  text
*----------------------------------------------------------------------*
FORM busca_datos_adr2  TABLES   p_xadr2 TYPE adr2_tab.

DATA ls_xadr2 TYPE LINE OF adr2_tab.

LOOP AT p_xadr2 INTO ls_xadr2.
    gs_telefono-country    = ls_xadr2-country .
    gs_telefono-tel_number = ls_xadr2-tel_number.
    gs_telefono-tel_extens = ls_xadr2-tel_extens.
    gs_telefono-r3_user    = ls_xadr2-r3_user.
    APPEND gs_telefono TO gt_telefono.
    CLEAR gs_telefono.
ENDLOOP.

ENDFORM.                    " BUSCA_DATOS_ADR2
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DATOS_ADR3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_datos_adr3 TABLES p_xadr3 TYPE  adr3_tab.

DATA ls_xadr3 TYPE LINE OF adr3_tab.

LOOP AT p_xadr3 INTO ls_xadr3.
    gs_fax-country    = ls_xadr3-country .
    gs_fax-fax_number = ls_xadr3-fax_number.
    gs_fax-fax_extens = ls_xadr3-fax_extens.
    APPEND gs_fax TO gt_fax.
    CLEAR gs_fax.
ENDLOOP.

ENDFORM.                    " BUSCA_DATOS_ADR3
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DATOS_ADR6
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_datos_adr6 TABLES p_xadr6 TYPE  adr6_tab
                    USING  p_lifnr
                            p_adrnr.

DATA: ls_xadr6 TYPE LINE OF adr6_tab.
DATA: lv_ktokk        TYPE ktokk,
        lt_ktokk        TYPE STANDARD TABLE OF zgreutil0001,
        lt_zgrtutil0001 TYPE STANDARD TABLE OF zgrtutil0001.

CALL FUNCTION 'ZGRUTIL001_RECUPERAR_HARDCODES'
    EXPORTING
    repid        = 'ZGPOCUENTAS'
    TABLES
    it_hardcodes = lt_zgrtutil0001
    EXCEPTIONS
    no_tiene     = 1
    OTHERS       = 2.
IF sy-subrc <> 0.
ENDIF.

CALL FUNCTION 'ZGRUTIL001_OBTENER_RANGO'
    EXPORTING
    campo        = 'KTOKK'
    TABLES
    it_hardcodes = lt_zgrtutil0001
    r_rango      = lt_ktokk
    EXCEPTIONS
    no_existe    = 1
    OTHERS       = 2.
IF sy-subrc <> 0.
ENDIF.

SELECT SINGLE ktokk
    FROM lfa1
    INTO lv_ktokk
    WHERE lifnr = p_lifnr.
IF sy-subrc = 0 AND lv_ktokk NOT IN lt_ktokk.
    DATA: ls_email TYPE zprs4t0001.

    IF gt_email[] IS NOT INITIAL.
    LOOP AT gt_email INTO ls_email.
        gs_mail-smtp_addr = ls_email-smtp_addr.
        APPEND gs_mail TO gt_mail.
        CLEAR gs_mail.
    ENDLOOP.
    ELSE.
    SELECT smtp_addr
        FROM zprs4t0001
        INTO TABLE gt_mail
        WHERE addrnumber = p_adrnr.
    ENDIF.
ELSE.
    LOOP AT p_xadr6 INTO ls_xadr6.
    gs_mail-smtp_addr = ls_xadr6-smtp_addr.
    APPEND gs_mail TO gt_mail.
    CLEAR gs_mail.
    ENDLOOP.
ENDIF.

ENDFORM.                    " BUSCA_DATOS_ADR6
*&---------------------------------------------------------------------*
*&      Form  OBTENER_DATOS_ACRE_SRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BUSINESSPARTNER  text
*----------------------------------------------------------------------*
FORM obtener_datos_acre_srm  USING p_businesspartner.

REFRESH: gt_bapiadtel_aux, gt_bapiadtel, gt_bapiadtel_mod.
REFRESH: gt_bapiadfax_aux, gt_bapiadfax, gt_bapiadfax_mod.
REFRESH: gt_bapiadsmtp_aux, gt_bapiadsmtp, gt_bapiadsmtp_mod.

TRY.
* Función que recupera los datos del domicilio del proveedor.
    CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
        DESTINATION gv_destination
        EXPORTING
        businesspartner = p_businesspartner
        IMPORTING
        addressdata     = gv_address
        TABLES
        bapiadtel       = gt_bapiadtel_aux
        bapiadfax       = gt_bapiadfax_aux
        bapiadsmtp      = gt_bapiadsmtp_aux.
    CATCH cx_root.
ENDTRY.

ENDFORM.                    " OBTENER_DATOS_ACRE_SRM
*&---------------------------------------------------------------------*
*&      Form  MOVER_DATOS_MODIFICADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mover_datos_modificados .

DATA: ls_mail           TYPE ty_mail,
        ls_telefono       TYPE ty_telefono,
        ls_fax            TYPE ty_fax,
        ls_adtel          TYPE bapiadtel,
        ls_adfax          TYPE bapiadfax,
        ls_adsmtp         TYPE bapiadsmtp,
        ls_bapiadtel      TYPE bapiadtel,
        ls_bapiadtel_mod  TYPE bapiadtelx,
        ls_bapiadfax      TYPE bapiadfax,
        ls_bapiadfax_mod  TYPE bapiadfaxx,
        ls_bapiadsmtp     TYPE bapiadsmtp,
        ls_bapiadsmtp_mod TYPE bapiadsmtx.

* Telefono ******************************************************
* Borrar
LOOP AT gt_bapiadtel_aux INTO ls_bapiadtel.
    APPEND ls_bapiadtel TO gt_bapiadtel.

*   MARCAMOS QUE SON TODOS PARA BORRAR
    MOVE 'DDDDDDDDDDDDDDD'  TO ls_bapiadtel_mod.
    APPEND ls_bapiadtel_mod TO gt_bapiadtel_mod.

    CLEAR: ls_bapiadtel_mod, ls_bapiadtel.
ENDLOOP.

* Incluir
LOOP AT gt_telefono INTO ls_telefono.
    MOVE: ls_telefono-country TO ls_bapiadtel-country,
    ls_telefono-tel_number    TO ls_bapiadtel-telephone,
    ls_telefono-tel_extens    TO ls_bapiadtel-extension,
    ls_telefono-r3_user       TO ls_bapiadtel-r_3_user.  " indrabr ppm42811
    MOVE 'IIIIIIIIIIIIIII'    TO ls_bapiadtel_mod.
    APPEND: ls_bapiadtel      TO gt_bapiadtel,
    ls_bapiadtel_mod          TO gt_bapiadtel_mod.

    CLEAR: ls_bapiadtel, ls_bapiadtel_mod.
ENDLOOP.

**** Fax ******************************************************
**** Borrar
LOOP AT gt_bapiadfax_aux INTO ls_bapiadfax.
    APPEND ls_bapiadfax TO gt_bapiadfax.

*   MARCAMOS QUE SON TODOS PARA BORRAR
    MOVE 'DDDDDDDDDDDDDDDD' TO ls_bapiadfax_mod.
    APPEND ls_bapiadfax_mod TO gt_bapiadfax_mod.

    CLEAR: ls_bapiadfax_mod, ls_bapiadfax.
ENDLOOP.


* Incluir
LOOP AT gt_fax INTO ls_fax.
    MOVE: ls_fax-country    TO ls_bapiadfax-country,
        ls_fax-fax_number TO ls_bapiadfax-fax,
        ls_fax-fax_extens TO ls_bapiadfax-extension.
    MOVE 'IIIIIIIIIIIIIIII' TO ls_bapiadfax_mod.
    APPEND: ls_bapiadfax    TO gt_bapiadfax,
    ls_bapiadfax_mod        TO gt_bapiadfax_mod.

    CLEAR: ls_bapiadfax, ls_bapiadfax_mod.
ENDLOOP.

* E-mail******************************************************
* Borrar
LOOP AT gt_bapiadsmtp_aux INTO ls_bapiadsmtp.
    APPEND ls_bapiadsmtp TO gt_bapiadsmtp.

*   MARCAMOS QUE SON TODOS PARA BORRAR
    MOVE 'DDDDDDDDDDDDD'     TO ls_bapiadsmtp_mod.
    APPEND ls_bapiadsmtp_mod TO gt_bapiadsmtp_mod.

    CLEAR: ls_bapiadsmtp_mod, ls_bapiadsmtp.
ENDLOOP.

* Incluir
LOOP AT gt_mail INTO ls_mail.
    MOVE ls_mail-smtp_addr TO ls_bapiadsmtp-e_mail.
    MOVE 'IIIIIIIIIIIII'   TO ls_bapiadsmtp_mod.
    APPEND: ls_bapiadsmtp  TO gt_bapiadsmtp,
    ls_bapiadsmtp_mod      TO gt_bapiadsmtp_mod.

    CLEAR: ls_bapiadsmtp, ls_bapiadsmtp_mod.
ENDLOOP.

ENDFORM.                    " MOVER_DATOS_MODIFICADOS
*&---------------------------------------------------------------------*
*&      Form  ZSRM08_MODIFICACION_PROVEEDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zsrm08_modificacion_proveedor  TABLES  lt_adtel_in
                                            lt_adtel_x_in
                                            lt_adfax_in
                                            lt_adfax_x_in
                                            lt_adsmtp_in
                                            lt_adsmtp_x_in
                                            gt_taxnum_del
                                            gt_taxnum_ins
                                            lt_frg0061_chg
                                            lt_return
                                    USING   p_partner
                                            is_data
                                            is_data_x
                                            is_data_organ_in
                                            is_data_organ_x_in
                                            is_address_in
                                            is_address_x_in
                                            is_eew_but000
                                            is_eew_but000_x
                                            p_test.

CALL FUNCTION 'ZSRM08_MODIFICACION_PROVEEDOR'
    DESTINATION gv_destination
    EXPORTING
    iv_partner_in                = p_partner
    is_data                      = is_data
    is_data_x                    = is_data_x
    is_data_organ_in             = is_data_organ_in
    is_data_organ_x_in           = is_data_organ_x_in
    is_address_in                = is_address_in
    is_address_x_in              = is_address_x_in
    iv_xxs_check                 = p_test
    iv_no_commit                 = p_test
    is_eew_but000_in             = is_eew_but000
    is_eew_but000_x_in           = is_eew_but000_x
    iv_save                      = 'X'
    TABLES
    it_adtel_in                  = lt_adtel_in
    it_adtel_x_in                = lt_adtel_x_in
    it_adfax_in                  = lt_adfax_in
    it_adfax_x_in                = lt_adfax_x_in
    it_adsmtp_in                 = lt_adsmtp_in
    it_adsmtp_x_in               = lt_adsmtp_x_in
    gt_taxnum_del                = gt_taxnum_del
    gt_taxnum_ins                = gt_taxnum_ins
    it_frg0061_chg               = lt_frg0061_chg
    et_return                    = lt_return
    EXCEPTIONS
    company_not_valid            = 1
    partner_type_not_valid       = 2
    error_message_passed         = 3
    error_changing_org_names     = 4
    error_reading_address        = 5
    error_changing_org_address   = 6
    error_changing_taxinfo       = 7
    error_changing_ven_mapping   = 8
    inconsistent_bank_changeinfo = 9
    OTHERS                       = 10.

CALL FUNCTION 'RFC_CONNECTION_CLOSE'
    EXPORTING
    destination          = gv_destination
    taskname             = ' '
    EXCEPTIONS
    destination_not_open = 1
    OTHERS               = 2.

ENDFORM.                    " ZSRM08_MODIFICACION_PROVEEDOR
*&---------------------------------------------------------------------*
*&      Form  FILL_ADRC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_VENDOR_CENTRAL_DATA_ADDRESS  text
*----------------------------------------------------------------------*
FORM fill_adrc_data  USING    p_vendor_data_address TYPE bapiad1vl.

gs_nombre-name1 = p_vendor_data_address-name.
gs_nombre_x-name1 = 'X'.
gs_nombre-name2 = p_vendor_data_address-name_2.
gs_nombre_x-name2 = 'X'.
gs_nombre-name3 = p_vendor_data_address-name_3.
gs_nombre_x-name3 = 'X'.
gs_nombre-name4 = p_vendor_data_address-name_4.
gs_nombre_x-name4 = 'X'.

gs_address-street = p_vendor_data_address-street.
gs_address_x-street = 'X'.
gs_address-house_no = p_vendor_data_address-house_no.
gs_address_x-house_no = 'X'.
gs_address-postl_cod1 = p_vendor_data_address-postl_cod1.
gs_address_x-postl_cod1 = 'X'.
gs_address-city = p_vendor_data_address-city.
gs_address_x-city = 'X'.
gs_address-country = p_vendor_data_address-country.
gs_address_x-country = 'X'.
gs_address-region = p_vendor_data_address-region.
gs_address_x-region = 'X'.

ENDFORM.                    " FILL_ADRC_DATA
*&---------------------------------------------------------------------*
*&      Form  EXPORT_SMTP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MASTER_DATA  text
*----------------------------------------------------------------------*
FORM export_smtp  USING    p_master_data TYPE vmds_ei_main.

DATA: ls_vendor   TYPE vmds_ei_extern.

READ TABLE p_master_data-vendors INTO ls_vendor INDEX 1.
IF sy-subrc = 0.
    SELECT *
    FROM zprs4t0001
    INTO TABLE gt_email
    WHERE addrnumber = ls_vendor-central_data-central-data-adrnr.
    IF sy-subrc = 0.
    EXPORT gt_email TO MEMORY ID 'GT_EMAIL'.
    CASE 'X'.
        WHEN rb_clt.
        EXPORT zz_via = 'C' TO MEMORY ID 'ZZ_VIA'.
        WHEN rb_prov.
        EXPORT zz_via = 'P' TO MEMORY ID 'ZZ_VIA'.
    ENDCASE.
    ENDIF.
ENDIF.

REFRESH: gt_email.

ENDFORM.                    " EXPORT_SMTP
*&---------------------------------------------------------------------*
*&      Form  EXPORT_SMTP_CL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MASTER_DATA  text
*----------------------------------------------------------------------*
FORM export_smtp_cl  USING    p_master_data TYPE cmds_ei_main.

DATA: ls_customer   TYPE cmds_ei_extern.

READ TABLE p_master_data-customers INTO ls_customer INDEX 1.
IF sy-subrc = 0.
    SELECT zprs4t0001~addrnumber zprs4t0001~high_value zprs4t0001~smtp_addr zprs4t0001~principal zprs4t0001~comentario zprs4t0001~comentario2
    FROM zprs4t0001 INNER JOIN kna1 ON ( kna1~adrnr = zprs4t0001~addrnumber )
    INTO CORRESPONDING FIELDS OF TABLE gt_email
    WHERE kna1~kunnr = ls_customer-header-object_instance-kunnr.
    IF sy-subrc = 0.
    EXPORT gt_email TO MEMORY ID 'GT_EMAIL'.
    CASE 'X'.
        WHEN rb_clt.
        EXPORT zz_via = 'C' TO MEMORY ID 'ZZ_VIA'.
        WHEN rb_prov.
        EXPORT zz_via = 'P' TO MEMORY ID 'ZZ_VIA'.
    ENDCASE.
    ENDIF.
ENDIF.

REFRESH: gt_email.

ENDFORM.                    " EXPORT_SMTP_CL
*&---------------------------------------------------------------------*
*&      Form  CLEAR_GLOBALS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_globals .

REFRESH: gt_email,
            gt_mail,
            gt_telefono,
            gt_fax,
            gt_bapiadtel_aux,
            gt_bapiadfax_aux,
            gt_bapiadsmtp_aux,
            gt_bapiadtel,
            gt_bapiadtel_mod,
            gt_bapiadfax,
            gt_bapiadfax_mod,
            gt_bapiadsmtp,
            gt_bapiadsmtp_mod,
            gt_adtel,
            gt_adfax,
            gt_adsmtp,
            gt_errores,
            gt_taxnum_del,
            gt_taxnum_ins,
            gt_but_frg0061.

CLEAR:  gs_mail,
        gs_telefono,
        gs_fax,
        gv_address,
        gs_zmmpue0023,
        gs_zmmpue0023_x,
        gs_nombre,
        gs_nombre_x,
        gs_address,
        gs_address_x,
        gs_eeww_but000,
        gs_eeww_but000_x.

ENDFORM.                    " CLEAR_GLOBALS
*&---------------------------------------------------------------------*
*&      Form  ADD_LOG_DEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*----------------------------------------------------------------------*
FORM add_log_del  USING    p_target
                            p_icon
                            p_datum
                            p_uzeit.

DATA: ls_zprs4t0002 TYPE zprs4t0002.

CASE p_icon.
    WHEN '1'.
    READ TABLE gt_icons INTO ls_zprs4t0002-e_icon WITH KEY iconname = 'ICON_MESSAGE_ERROR'.
    ls_zprs4t0002-msg = 'Error al borrar los correos actuales en BD'.
    WHEN '2'.
    READ TABLE gt_icons INTO ls_zprs4t0002-e_icon WITH KEY iconname = 'ICON_CHECKED'.
    ls_zprs4t0002-msg = 'Éxito al borrar los correos actuales en BD'.
ENDCASE.

CASE 'X'.
    WHEN rb_clt.
    ls_zprs4t0002-kunnr = p_target.
    WHEN rb_prov.
    ls_zprs4t0002-lifnr = p_target.
ENDCASE.

ls_zprs4t0002-datum = p_datum.
ls_zprs4t0002-uzeit = p_uzeit.
ls_zprs4t0002-smtp_addr = 'DELETE'.

IF ls_zprs4t0002-kunnr IS NOT INITIAL OR ls_zprs4t0002-lifnr IS NOT INITIAL.
    MODIFY zprs4t0002 FROM ls_zprs4t0002.
ENDIF.

ENDFORM.                    " ADD_LOG_DEL
*&---------------------------------------------------------------------*
*&      Form  ADD_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TARGET  text
*      -->P_ICON      text
*      -->P_SMTP_ADDR  text
*      -->P_MSG   text
*----------------------------------------------------------------------*
FORM add_log  USING    p_target
                        p_icon
                        p_smtp_addr
                        p_msg
                        p_datum
                        p_uzeit.

DATA: ls_zprs4t0002 TYPE zprs4t0002.

CASE p_icon.
    WHEN '1'.
    READ TABLE gt_icons INTO ls_zprs4t0002-e_icon WITH KEY iconname = 'ICON_MESSAGE_ERROR'.
    WHEN '2'.
    READ TABLE gt_icons INTO ls_zprs4t0002-e_icon WITH KEY iconname = 'ICON_CHECKED'.
ENDCASE.

CASE 'X'.
    WHEN rb_clt.
    ls_zprs4t0002-kunnr = p_target.
    WHEN rb_prov.
    ls_zprs4t0002-lifnr = p_target.
ENDCASE.

ls_zprs4t0002-datum = p_datum.
ls_zprs4t0002-uzeit = p_uzeit.
ls_zprs4t0002-smtp_addr = p_smtp_addr.
ls_zprs4t0002-msg = p_msg.

IF ls_zprs4t0002-kunnr IS NOT INITIAL OR ls_zprs4t0002-lifnr IS NOT INITIAL.
    MODIFY zprs4t0002 FROM ls_zprs4t0002.
ENDIF.

ENDFORM.                    " ADD_LOG