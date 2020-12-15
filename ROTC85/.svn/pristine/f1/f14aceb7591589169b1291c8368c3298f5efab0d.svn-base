CREATE OR REPLACE PACKAGE BODY APPS.xxgil_email_comm_invoice_pkg
IS
/*===========================================================================+
| FILENAME : xxgil_email_comm_invoice_pkg.pkb                                |
|                                                                            |
| PACKAGE NAME : xxgil_email_comm_invoice_pkg                                |
|                                                                            |
| DESCRIPTION : PACKAGE BODY.This package is used for emailing commercial invoices   |
|                                                                            |
| Usage/ Purpose : This package is used for emailing commercial invoices     |
|                                                                            |
| Ver       Date           Author           Modification/History             |
| 1.0       06/26/2017     Sankalpa Saha    CR1688 Initial Creation.     
| 1.1       07/23/2018     Deekshit Yerra   Retrofit changes for 12.2.6 upgrade |
| 1.2	    07/23/2020	   Kalyani M	    INC1094857			     |
+===========================================================================*/

   /**************************************************************************
    ||
    ||   Filename      : xxgil_email_comm_invoice_pkg.pkb
    ||   Procedure Name: attachment_title
    ||   Description   : fetches title of the invoice attachment file
    ||   Usage/Purpose :
    ||
    ||   Ver  Date          Author                      Modification/History
    ||   1.0  26-JUN-17     Sankalpa Saha               Created
    ||   1.1  07/23/2018    Deekshit Yerra              Retrofit changes for 12.2.6 upgrade 
    ***************************************************************************/

   FUNCTION attachment_title (p_cust_trx_id IN NUMBER)
      RETURN VARCHAR2
   IS
      lv_po_number              ra_customer_trx_all.purchase_order%TYPE;
      lv_trx_number             ra_customer_trx_all.trx_number%TYPE;
      lv_attach_title           VARCHAR2 (2000)     := NULL;
      lv_trx_type               ra_cust_trx_types_all.TYPE%TYPE;
      lv_error                  VARCHAR2(2000) := NULL;
      lv_conc_prog_start_date   VARCHAR2(2000) := NULL;
   BEGIN
      BEGIN
         SELECT rca.trx_number,
                RTRIM(TRANSLATE(rca.purchase_order,'\/:*?"<>|,',' '))  purchase_order,
                rcta.TYPE
           INTO lv_trx_number,
                lv_po_number,
                lv_trx_type
           FROM ra_customer_trx rca,
                ra_cust_trx_types rcta
          WHERE rca.cust_trx_type_id = rcta.cust_trx_type_id
            AND customer_trx_id = p_cust_trx_id;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            lv_po_number := NULL;
            lv_trx_number := NULL;
            lv_trx_type := NULL;
            RAISE;
         WHEN OTHERS THEN
            lv_po_number := NULL;
            lv_trx_number := NULL;
            lv_trx_type := NULL;
            RAISE;

      END;

      IF (lv_po_number IS NULL)
      THEN
         lv_attach_title := lv_trx_type||'#' || lv_trx_number;
      ELSE
         lv_attach_title :=
                      'PO#' || lv_po_number || '-' || lv_trx_type||'#' || lv_trx_number;
      END IF;

      BEGIN
        SELECT TO_CHAR(actual_start_date,'DDMMYYYY')
        INTO lv_conc_prog_start_date
        FROM fnd_concurrent_requests
        WHERE request_id = fnd_global.conc_request_id ;

      EXCEPTION
        WHEN OTHERS THEN
          lv_conc_prog_start_date := NULL;
      END;

      RETURN fnd_global.conc_request_id || '_' || lv_conc_prog_start_date || '-' || lv_attach_title;
   EXCEPTION
      WHEN OTHERS  THEN
         lv_attach_title := NULL;
         lv_error := SUBSTR(SQLERRM,1,1000);
         fnd_file.put_line (fnd_file.LOG, 'DEBUG:  Error in WHEN OTHERS of attachment_title.' || lv_error);
         RAISE;

   END attachment_title;

   /**************************************************************************
    ||
    ||   Filename      : xxgil_email_comm_invoice_pkg.pkb
    ||   Procedure Name: get_transaction_name
    ||   Description   : Determines the name of transaction
    ||   Usage/Purpose :
    ||
    ||   Ver  Date          Author                      Modification/History
    ||   1.0  26-JUN-17     Sankalpa Saha                Created
    ||
    ***************************************************************************/

   FUNCTION get_transaction_name (p_cust_trx_id IN NUMBER)
      RETURN VARCHAR2
   IS
      lv_trx_type        VARCHAR2(2000) := NULL;
      lv_error           VARCHAR2(2000) := NULL;
   BEGIN
      BEGIN
         SELECT arl.meaning
           INTO lv_trx_type
           FROM ra_customer_trx rca,
                ra_cust_trx_types rcta,
                ar_lookups arl
          WHERE rca.cust_trx_type_id = rcta.cust_trx_type_id
            AND rcta.type = arl.lookup_code
            AND arl.lookup_type = 'INV/CM'
            AND arl.enabled_flag = 'Y'
            AND TRUNC (SYSDATE) BETWEEN TRUNC (NVL(arl.start_date_active,SYSDATE)) AND TRUNC (NVL (arl.end_date_active, SYSDATE))
            AND customer_trx_id = p_cust_trx_id;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            lv_trx_type := NULL;
            RAISE;
         WHEN OTHERS THEN
            lv_trx_type := NULL;
            RAISE;
      END;

       RETURN lv_trx_type;
   EXCEPTION
      WHEN OTHERS THEN
         lv_trx_type := NULL;

         lv_error := SUBSTR(SQLERRM,1,1000);
         fnd_file.put_line (fnd_file.LOG, 'DEBUG:  Error in WHEN OTHERS of get_transaction_name.' || lv_error);
         RETURN NULL;

   END get_transaction_name;

   /**************************************************************************
    ||
    ||   Filename      : xxgil_email_comm_invoice_pkg.pkb
    ||   Procedure Name: get_transaction_type
    ||   Description   : determines the type of transaction
    ||   Usage/Purpose :
    ||
    ||   Ver  Date          Author                      Modification/History
    ||   1.0  26-JUN-17     Sankalpa Saha                Created
    ||
    ***************************************************************************/

   FUNCTION get_transaction_type (p_cust_trx_id IN NUMBER)
      RETURN VARCHAR2
   IS
      lv_trx_type   ra_cust_trx_types_all.TYPE%TYPE;
      lv_error      VARCHAR2(2000)  := NULL;
   BEGIN
      BEGIN
         SELECT rcta.TYPE
           INTO lv_trx_type
           FROM ra_customer_trx rca,
                ra_cust_trx_types rcta
          WHERE rca.cust_trx_type_id = rcta.cust_trx_type_id
            AND rca.customer_trx_id = p_cust_trx_id;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            lv_trx_type := NULL;
            RAISE;
         WHEN OTHERS THEN
            lv_trx_type := NULL;
            RAISE;
      END;

      RETURN lv_trx_type;
   EXCEPTION
      WHEN OTHERS THEN
         lv_trx_type := NULL;
         lv_error := SUBSTR(SQLERRM,1,1000);
         fnd_file.put_line (fnd_file.LOG, 'DEBUG:  Error in WHEN OTHERS of get_transaction_type.' || lv_error);
         RETURN NULL;

   END get_transaction_type;

   /**************************************************************************
    ||
    ||   Filename      : xxgil_email_comm_invoice_pkg.pkb
    ||   Procedure Name: get_customer_profile_dff_value
    ||   Description   : determines the transaction type which the customer is eligible to receive emails
    ||   Usage/Purpose :
    ||
    ||   Ver  Date          Author                      Modification/History
    ||   1.0  26-JUN-17     Sankalpa Saha                Created
    ||
    ***************************************************************************/

   FUNCTION get_customer_profile_dff_value (p_cust_trx_id IN NUMBER)
      RETURN VARCHAR2
   IS
      lv_cust_profile_value   hz_customer_profiles.attribute8%TYPE   := NULL;
      lv_email_profile        VARCHAR2 (2000)                        := NULL;
      lv_transaction_type     VARCHAR2 (2000)                        := NULL;
      lv_return               VARCHAR2 (2000)                        := NULL;
      lv_error                VARCHAR2 (2000)                        := NULL;
   BEGIN
      BEGIN
         SELECT hcp.attribute8
           INTO lv_cust_profile_value
           FROM ra_customer_trx rca,
                hz_cust_accounts hca,
                hz_customer_profiles hcp
          WHERE rca.bill_to_customer_id = hca.cust_account_id
            AND hca.cust_account_id = hcp.cust_account_id
            AND rca.customer_trx_id = p_cust_trx_id
            AND hcp.site_use_id IS NULL;

         IF (lv_cust_profile_value = 'Email All AR Transactions')
         THEN
            lv_email_profile := 'ALL';
         ELSIF (lv_cust_profile_value = 'Email Credit Memo')
         THEN
            lv_email_profile := 'CM';
         ELSIF (lv_cust_profile_value = 'Email Invoice')
         THEN
            lv_email_profile := 'INV';
         END IF;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            lv_email_profile := NULL;
         WHEN OTHERS THEN
            lv_email_profile := NULL;
      END;

      lv_transaction_type :=
                         get_transaction_type (p_cust_trx_id      => p_cust_trx_id);

      IF (lv_email_profile = 'ALL')
      THEN
         lv_return := 'YES';
      ELSIF (lv_transaction_type = lv_email_profile)
      THEN
         lv_return := 'YES';
      ELSE
         lv_return := 'NO';
      END IF;

      RETURN lv_return;
   EXCEPTION
      WHEN OTHERS THEN
         lv_return := 'NO';

         lv_error := SUBSTR(SQLERRM,1,1000);
         fnd_file.put_line (fnd_file.LOG, 'DEBUG:  Error in WHEN OTHERS of get_customer_profile_dff_value.' || lv_error);
         --RAISE;
         RETURN lv_return;

   END get_customer_profile_dff_value;

  /**************************************************************************
    ||
    ||   Filename      : xxgil_email_comm_invoice_pkg.pkb
    ||   Procedure Name: get_template_loc
    ||   Description   : fetches the template location of an operating unit
    ||   Usage/Purpose :
    ||
    ||   Ver  Date          Author                      Modification/History
    ||   1.0  26-JUN-17     Sankalpa Saha                Created
    ||
    ***************************************************************************/

  FUNCTION get_template_loc (p_request_id       IN NUMBER,
                             p_cust_trx_id IN NUMBER)
      RETURN VARCHAR2
   IS
      lv_org_name           hr_operating_units.name%TYPE := NULL;
      lv_template_code      VARCHAR2(2000) := NULL;
      lv_template_loc       VARCHAR2(2000) := NULL;
      lv_error              VARCHAR2(2000) := NULL;
      lv_conc_prog_name     fnd_concurrent_programs.concurrent_program_name%TYPE := NULL;
   BEGIN
      BEGIN
         SELECT hou.name
           INTO lv_org_name
           FROM ra_customer_trx rca,
                hr_operating_units hou
          WHERE rca.org_id = hou.organization_id
            AND rca.customer_trx_id = p_cust_trx_id;

      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            lv_org_name := NULL;
            RAISE;
         WHEN OTHERS  THEN
            lv_org_name := NULL;
            RAISE;
      END;

      fnd_file.put_line (fnd_file.LOG,'DEBUG:  Operating Unit.' || lv_org_name );

      BEGIN
         SELECT fcp.concurrent_program_name
           INTO lv_conc_prog_name
           FROM fnd_concurrent_requests fcr,
                fnd_concurrent_programs fcp
          WHERE fcr.concurrent_program_id = fcp.concurrent_program_id
            AND fcr.request_id = p_request_id ;

      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            lv_conc_prog_name := NULL;
            RAISE;
         WHEN OTHERS  THEN
            lv_conc_prog_name := NULL;
            RAISE;
      END;

      fnd_file.put_line (fnd_file.LOG,'DEBUG:  Concurrent program short name.' || lv_conc_prog_name );

      BEGIN
        SELECT fpa.argument2
          INTO lv_template_code
          FROM apps.fnd_concurrent_requests fcr
              ,fnd_conc_pp_actions fpa
              ,xdo_templates_tl xt
        WHERE fcr.request_id = fpa.concurrent_request_id
          AND fpa.argument2 = xt.template_code
          AND xt.language = 'US'
          AND fcr.request_id = p_request_id;

          fnd_file.put_line (fnd_file.LOG, 'DEBUG:  Template code.' || lv_template_code );

      EXCEPTION
        WHEN OTHERS THEN
          lv_template_code := NULL;
          fnd_file.put_line (fnd_file.LOG,'DEBUG:  No template found for concurrent request: ' || p_request_id );
          RAISE;
      END;


      BEGIN
        SELECT 'xdo://'||   xtb.application_short_name||'.'||
                 xtb.template_code ||'.'||
                 xtb.default_language ||'.'||
                 xtb.default_territory|| '/?getSource=true'
          INTO   lv_template_loc
          FROM   apps.xdo_templates_b xtb
         WHERE   xtb.template_code = lv_template_code ;

      EXCEPTION
        WHEN OTHERS THEN
          lv_template_loc := NULL;
          RAISE;
      END;

      fnd_file.put_line (fnd_file.LOG, 'DEBUG:  Template location.' || lv_template_loc );

      RETURN lv_template_loc ;
   EXCEPTION
      WHEN OTHERS THEN
         lv_template_loc := NULL;

         lv_error := SUBSTR(SQLERRM,1,1000);
         fnd_file.put_line (fnd_file.LOG, 'DEBUG:  Error in WHEN OTHERS of get_template_loc.' || lv_error);
         RAISE;
   END get_template_loc;


  /**************************************************************************
    ||
    ||   Filename      : xxgil_email_comm_invoice_pkg.pkb
    ||   Procedure Name: send_mail_with_invoice
    ||   Description   : sends email with invoices/CM/DM as attachments along with Terms and Conditions file, if any
    ||   Usage/Purpose :
    ||
    ||   Ver  Date          Author                      Modification/History
    ||   1.0  26-JUN-17     Sankalpa Saha                Created
    ||
    ***************************************************************************/

  PROCEDURE send_mail_with_invoice (p_request_id       IN NUMBER,
                                    p_operating_unit   IN VARCHAR2,
                                    p_file_path        IN VARCHAR2)
  AS
   lv_from              VARCHAR2 (100) := NULL;
   lv_to                VARCHAR2 (100) := NULL;
   lv_subject           VARCHAR2 (100) := NULL;
   lv_subject_copy      VARCHAR2 (100) := NULL;
   lv_blob              BLOB;
   lv_conn              UTL_SMTP.connection;
   lv_raw               RAW (57);
   lv_len               INTEGER := 0;
   lv_idx               INTEGER := 1;
   lv_buff_size         INTEGER := 57;
   lv_boundary          VARCHAR2 (32) := SYS_GUID ();
   lv_attachment_name   LONG;
   lv_mail_host         VARCHAR2 (240):= fnd_profile.value('FND_SMTP_HOST');
   lv_attachment_limit  NUMBER := TO_NUMBER(NVL(fnd_profile.VALUE ('XXGIL_AR_EMAIL_ATTACHMENT_LIMIT'),0));
   lv_attachment_count  NUMBER := 0;
   lv_ntile             NUMBER := 0;
   lv_loop_count        NUMBER := 0;
   lv_body_code         VARCHAR2 (4000):= NULL;
   lv_message_subject   VARCHAR2 (4000):= NULL;
   lv_message_body      VARCHAR2 (4000):= NULL;
   lv_message           VARCHAR2 (4000):= NULL;
   lv_err_flag          VARCHAR2(20) := NULL;
   lv_to_emails         VARCHAR2(2000) := NULL;
   lv_to_email          VARCHAR2(100):= NULL;
   lv_email_count       NUMBER  := 0;
   lv_trx_type_count    NUMBER := 0;
   lv_trx_meaning       VARCHAR2(2000) := NULL;
   lv_subject_code      VARCHAR2(2000) := NULL;
   lv_prog_start_date   VARCHAR2(2000) := NULL;
   lv_trx_type          VARCHAR2(2000) := NULL;
   lv_term_count        NUMBER  := 0;
   lv_term_sequence_number  NUMBER  := 0;
   lv_printing_count    NUMBER :=0;
   lv_printing_original_date    DATE    := NULL;
   lv_org_id            NUMBER := 0;
   lv_email_error       VARCHAR2(2000) := NULL;

   -- Cursor to fetch the customer details based on the current request id
   CURSOR cur_get_customer
   IS
   SELECT bill_to_site
         ,customer_number
         ,email_address
         ,cc_email_address
         ,count(*) inv_count
   FROM xxgil_ar_email_details
   WHERE request_id = p_request_id
     AND file_data IS NOT NULL
   GROUP BY bill_to_site
           ,customer_number
           ,email_address
           ,cc_email_address;

   -- Cursor to determine the number of batches of transactions to be sent in separate email
   CURSOR cur_get_invoice(p_bill_to_site NUMBER,p_inv_count NUMBER, p_email_address VARCHAR2)
   IS
   SELECT transaction_number
          ,file_name
          ,file_data
          ,CEIL(rank() over (ORDER BY ROWNUM DESC)/lv_attachment_limit) AS NTILE
   FROM xxgil_ar_email_details
   WHERE request_id = p_request_id
   AND bill_to_site = p_bill_to_site
   AND email_address = p_email_address
   AND file_data IS NOT NULL;


   -- Cursor to get the Terms and Conditions attachments created against any operating unit
   CURSOR cur_get_attachments
   IS
   SELECT flb.file_name
         ,flb.file_data
    FROM fnd_flex_value_sets ffvs,
         fnd_flex_values_vl ffvv,
         fnd_attached_documents fad,
         fnd_documents fd,
         fnd_lobs flb
  WHERE ffvs.flex_value_set_name = 'XXGIL_AR_EMAIL_TRANSACTIONS'
    AND ffvs.flex_value_set_id = ffvv.flex_value_set_id
    AND ffvv.enabled_flag = 'Y'
    AND TRUNC (SYSDATE) BETWEEN TRUNC (NVL(ffvv.start_date_active,SYSDATE)) AND TRUNC (NVL (ffvv.end_date_active, SYSDATE))
    AND TO_CHAR(ffvv.flex_value_id) = fad.pk1_value
    AND fad.entity_name = 'FND_FLEX_VALUES'
    AND ffvv.flex_value = p_operating_unit
    AND fad.document_id = fd.document_id
    AND flb.file_id = fd.media_id;

   -- Cursor for Print flag updates
   CURSOR cur_get_transactions
   IS
   SELECT transaction_number,
          trx_type,
          term_count,
          term_sequence_number,
          printing_count,
          printing_original_date,
          org_id
     FROM xxgil_ar_email_details
    WHERE request_id = p_request_id
      AND NVL(email_sent,'No') = 'Yes';

  BEGIN

    DBMS_OUTPUT.ENABLE(buffer_size => NULL);

    UPDATE xxgil_ar_email_details
    SET file_name = RTRIM(REPLACE(file_name,p_file_path||'/',''))
    WHERE request_id = p_request_id;

    BEGIN
      SELECT ffvv.attribute1,
             ffvv.attribute2,
             ffvv.attribute3,
             fnm_title.message_text,
             fnm_body.message_text
        INTO lv_from
            ,lv_subject_code
            ,lv_body_code
            ,lv_message_subject
            ,lv_message_body
        FROM apps.fnd_new_messages  fnm_title,
             apps.fnd_new_messages  fnm_body,
             apps.fnd_flex_value_sets ffvs,
             apps.fnd_flex_values_vl  ffvv
      WHERE ffvs.flex_value_set_name = 'XXGIL_AR_EMAIL_TRANSACTIONS'
        AND ffvs.flex_value_set_id = ffvv.flex_value_set_id
        AND ffvv.enabled_flag = 'Y'
        AND TRUNC (SYSDATE) BETWEEN TRUNC (NVL(ffvv.start_date_active,SYSDATE)) AND TRUNC (NVL (ffvv.end_date_active, SYSDATE))
        AND ffvv.attribute2 = fnm_title.message_name
        AND ffvv.attribute3 = fnm_body.message_name
        AND ffvv.flex_value = p_operating_unit
		and fnm_title.language_code = userenv('LANG')  -- 12.2.6 upgrade - dyerra Retrofit1 for V1.1
        and fnm_title.application_id = 20003
        and fnm_body.language_code = userenv('LANG')
        and fnm_body.application_id = 20003;

    EXCEPTION
      WHEN OTHERS THEN
        lv_from := NULL;
        lv_message_body := NULL;
        lv_subject_code := NULL;
        lv_body_code := NULL;
        lv_message_subject := NULL;
    END;

    FOR get_customer_rec IN cur_get_customer LOOP

        lv_trx_meaning := NULL;

        lv_to_emails := get_customer_rec.email_address;
        lv_email_count := LENGTH(lv_to_emails) - LENGTH(REPLACE(lv_to_emails,';',''));

        lv_NTILE := 0;
        lv_loop_count := 0;
        lv_attachment_count := 0;

        FOR get_invoice_rec IN cur_get_invoice(get_customer_rec.bill_to_site,get_customer_rec.inv_count,get_customer_rec.email_address ) LOOP

            BEGIN

              lv_loop_count := lv_loop_count + 1;
              lv_attachment_count := lv_attachment_count + 1;

              IF lv_NTILE = 0 THEN

               lv_NTILE := get_invoice_rec.ntile;
               lv_trx_meaning := NULL;

                BEGIN
                 SELECT LTRIM(MAX(DECODE(arl.lookup_code,'INV', arl.meaning || '(s)'))
                        || MAX(DECODE(arl.lookup_code,'CM', ', ' || arl.meaning || '(s)'))
                        || MAX(DECODE(arl.lookup_code,'DM', ', ' || arl.meaning || '(s)')),', ')
                    INTO lv_trx_meaning
                    FROM ra_customer_trx_all rca,
                         ra_cust_trx_types_all rcta,
                         ar_lookups arl
                  WHERE rca.cust_trx_type_id = rcta.cust_trx_type_id
                    AND rcta.type = arl.lookup_code
                    AND arl.lookup_type = 'INV/CM'
                    AND arl.enabled_flag = 'Y'
                    AND TRUNC (SYSDATE) BETWEEN TRUNC (NVL(arl.start_date_active,SYSDATE)) AND TRUNC (NVL (arl.end_date_active, SYSDATE))
                    AND (rca.trx_number,rca.org_id,lv_NTILE)
                       IN
                       (
                       SELECT transaction_number
                              ,org_id
                              ,CEIL(rank() over (ORDER BY ROWNUM DESC)/lv_attachment_limit) AS NTILE
                         FROM xxgil_ar_email_details
                        WHERE request_id = p_request_id
                          AND bill_to_site = get_customer_rec.bill_to_site
                          AND email_address = get_customer_rec.email_address
                          AND file_data IS NOT NULL
                       );

                EXCEPTION
                  WHEN OTHERS THEN
                    lv_trx_meaning := NULL;
                    RAISE;
                END;

                fnd_message.set_name ('XXGIL', lv_subject_code);

                IF INSTR(lv_message_subject,'TRX_TYPE') > 0 THEN
                  fnd_message.set_token ('TRX_TYPE', lv_trx_meaning);
                END IF;

                IF INSTR(lv_message_subject,'CUST_NUM') > 0 THEN
                  fnd_message.set_token ('CUST_NUM', get_customer_rec.customer_number);
                END IF;

                lv_subject := fnd_message.get;

                fnd_message.set_name ('XXGIL', lv_body_code);

                IF INSTR(lv_message_body,'TRX_TYPE') > 0 THEN
                  fnd_message.set_token ('TRX_TYPE', LOWER(lv_trx_meaning));
                END IF;

                lv_message := fnd_message.get;

                lv_conn := UTL_SMTP.open_connection (lv_mail_host, 25);
                UTL_SMTP.helo (lv_conn, lv_mail_host);
                UTL_SMTP.mail (lv_conn, lv_from);

                IF lv_email_count > 0 THEN
                   FOR i IN 0..lv_email_count LOOP
                    IF i = lv_email_count THEN
                        lv_to_email := lv_to_emails;
                    ELSE
                        lv_to_email := SUBSTR(lv_to_emails,1,INSTR(lv_to_emails,';')-1);
                        lv_to_emails := SUBSTR(lv_to_emails,INSTR(lv_to_emails,';')+1);
                    END IF;
                    UTL_SMTP.rcpt( lv_conn, lv_to_email );
                  END LOOP;
                ELSE
                  UTL_SMTP.rcpt( lv_conn, get_customer_rec.email_address );
                END IF;

                IF get_customer_rec.cc_email_address IS NOT NULL THEN
                  UTL_SMTP.rcpt( lv_conn, get_customer_rec.cc_email_address );
                END IF;

                UTL_SMTP.open_data (lv_conn);

                UTL_SMTP.write_data (lv_conn, 'From: ' || lv_from || UTL_TCP.crlf);
                UTL_SMTP.write_data (lv_conn, 'To: ' || get_customer_rec.email_address || UTL_TCP.crlf);

                IF get_customer_rec.cc_email_address IS NOT NULL THEN
                   UTL_SMTP.write_data(lv_conn, 'CC: ' || get_customer_rec.cc_email_address || UTL_TCP.crlf);
                END IF;

                UTL_SMTP.write_data (lv_conn, 'Subject: ' || lv_subject || UTL_TCP.crlf);
                UTL_SMTP.write_data (lv_conn, 'MIME-Version: 1.0' || UTL_TCP.crlf);
                UTL_SMTP.write_data (lv_conn,'Content-Type: multipart/mixed; ' || UTL_TCP.crlf);
                UTL_SMTP.write_data (lv_conn,' boundary= "' || lv_boundary || '"' || UTL_TCP.crlf);
                UTL_SMTP.write_data (lv_conn, UTL_TCP.crlf);

                UTL_SMTP.write_data (lv_conn, '--' || lv_boundary || UTL_TCP.crlf);
                UTL_SMTP.write_data (lv_conn, 'Content-Type: text/plain; UTF8'|| UTL_TCP.crlf);
                UTL_SMTP.write_data (lv_conn, UTL_TCP.crlf);
                UTL_SMTP.write_raw_data(lv_conn, utl_raw.cast_to_raw(lv_message));

                UTL_SMTP.write_data (lv_conn, UTL_TCP.crlf);
              END IF;

              lv_blob := get_invoice_rec.file_data;

               BEGIN
                 SELECT TO_CHAR(actual_start_date,'DDMMYYYY')
                 INTO lv_prog_start_date
                 FROM fnd_concurrent_requests
                 WHERE request_id = p_request_id ;

               EXCEPTION
                WHEN OTHERS THEN
                  lv_prog_start_date := NULL;
                  RAISE;
               END;

              lv_attachment_name := LTRIM(get_invoice_rec.file_name,p_request_id || '_' || lv_prog_start_date ||'-');

              -- Attachment
              UTL_SMTP.write_data (lv_conn, '--' || lv_boundary || UTL_TCP.crlf);
              UTL_SMTP.write_data (lv_conn,'Content-Type: application/octet-stream' || UTL_TCP.crlf);
              UTL_SMTP.write_data (lv_conn,'Content-Disposition: attachment; ' || UTL_TCP.crlf);
              UTL_SMTP.write_data (lv_conn,' filename="' || lv_attachment_name || '"' || UTL_TCP.crlf);
              UTL_SMTP.write_data (lv_conn,'Content-Transfer-Encoding: base64' || UTL_TCP.crlf);
              UTL_SMTP.write_data (lv_conn, UTL_TCP.crlf);
              -- Loop through the blob
              -- chuck it up into 57-byte pieces
              -- and base64 encode it and write it into the mail buffer

              lv_len := DBMS_LOB.getlength (lv_blob);
              lv_idx                := 1;
              lv_buff_size          := 57;

              WHILE lv_idx < lv_len
              LOOP
                 DBMS_LOB.read (lv_blob,
                                  lv_buff_size,
                                  lv_idx,
                                  lv_raw);
                 UTL_SMTP.write_raw_data (lv_conn, UTL_ENCODE.base64_encode (lv_raw));
                 UTL_SMTP.write_data (lv_conn, UTL_TCP.crlf);
                 lv_idx := lv_idx + lv_buff_size;
              END LOOP;

              UTL_SMTP.write_data (lv_conn, UTL_TCP.crlf);

              /* end of batch if loop*/
              IF lv_attachment_count = lv_attachment_limit OR lv_loop_count = get_customer_rec.inv_count THEN
                 FOR get_attachments_rec IN cur_get_attachments LOOP
                    lv_blob := get_attachments_rec.file_data;
                    lv_attachment_name := get_attachments_rec.file_name;

                    -- Attachment
                    UTL_SMTP.write_data (lv_conn, '--' || lv_boundary || UTL_TCP.crlf);
                    UTL_SMTP.write_data (lv_conn,'Content-Type: application/octet-stream' || UTL_TCP.crlf);
                    UTL_SMTP.write_data (lv_conn,'Content-Disposition: attachment; ' || UTL_TCP.crlf);
                    UTL_SMTP.write_data (lv_conn,' filename="' || lv_attachment_name || '"' || UTL_TCP.crlf);
                    UTL_SMTP.write_data (lv_conn,'Content-Transfer-Encoding: base64' || UTL_TCP.crlf);
                    UTL_SMTP.write_data (lv_conn, UTL_TCP.crlf);

                    -- Loop through the blob
                    -- chuck it up into 57-byte pieces
                    -- and base64 encode it and write it into the mail buffer

                    lv_len := DBMS_LOB.getlength (lv_blob);
                    lv_idx                := 1;
                    lv_buff_size          := 57;

                    WHILE lv_idx < lv_len
                    LOOP
                       DBMS_LOB.read (lv_blob,
                                        lv_buff_size,
                                        lv_idx,
                                        lv_raw);
                       UTL_SMTP.write_raw_data (lv_conn, UTL_ENCODE.base64_encode (lv_raw));
                       UTL_SMTP.write_data (lv_conn, UTL_TCP.crlf);
                       lv_idx := lv_idx + lv_buff_size;
                    END LOOP;

                    UTL_SMTP.write_data (lv_conn, UTL_TCP.crlf);

                 END LOOP;
                    -- Close Email
                UTL_SMTP.write_data (lv_conn, '--' || lv_boundary || '--' || UTL_TCP.crlf);
                UTL_SMTP.write_data (lv_conn, UTL_TCP.crlf || '.' || UTL_TCP.crlf);
                UTL_SMTP.close_data (lv_conn);
                UTL_SMTP.quit (lv_conn);

                BEGIN
                  UPDATE xxgil_ar_email_details xaed
                     SET xaed.email_sent = 'Yes',
                         xaed.email_error = NULL
                   WHERE xaed.request_id = p_request_id
                     AND xaed.file_data IS NOT NULL
                     AND (xaed.transaction_number, xaed.org_id, get_invoice_rec.ntile)
                         IN
                         (
                       SELECT  xaed_1.transaction_number
                              ,xaed_1.org_id
                              ,CEIL(RANK() OVER (ORDER BY ROWNUM DESC)/lv_attachment_limit) AS NTILE
                         FROM xxgil_ar_email_details XAED_1
                        WHERE xaed_1.request_id = p_request_id
                          AND xaed_1.bill_to_site = get_customer_rec.bill_to_site
                          AND xaed_1.email_address = get_customer_rec.email_address
                          AND xaed_1.file_data IS NOT NULL
                          );

                EXCEPTION
                  WHEN OTHERS THEN
                    dbms_output.put_line('Error updating email flag in xxgil_ar_email_details due to - SQLCODE: '||SQLCODE );
                    RAISE;
                END;

               lv_NTILE := 0;
               lv_attachment_count := 0;

              END IF; /* end of batch if loop*/

            EXCEPTION
                WHEN UTL_SMTP.transient_error OR UTL_SMTP.permanent_error
                THEN
                  dbms_output.put_line('DEBUG:  Error sending invoice mail.SQLCODE: ' || SQLCODE);
                  lv_email_error := SUBSTR(SQLERRM,1,1000);
                  BEGIN
                      UPDATE xxgil_ar_email_details xaed
                         SET xaed.email_sent = 'No',
                             xaed.email_error = lv_email_error
                       WHERE xaed.request_id = p_request_id
                         AND xaed.file_data IS NOT NULL
                         AND (xaed.transaction_number, xaed.org_id, get_invoice_rec.ntile)
                             IN
                             (
                           SELECT xaed_1.transaction_number
                                  ,xaed_1.org_id
                                  ,CEIL(RANK() OVER (ORDER BY ROWNUM DESC)/lv_attachment_limit) AS NTILE
                             FROM xxgil_ar_email_details XAED_1
                            WHERE xaed_1.request_id = p_request_id
                              AND xaed_1.bill_to_site = get_customer_rec.bill_to_site
                              AND xaed_1.email_address = get_customer_rec.email_address
                              AND xaed_1.file_data IS NOT NULL
                              );
                       lv_NTILE := 0;
                       lv_attachment_count := 0;
                  EXCEPTION
                    WHEN OTHERS THEN
                      dbms_output.put_line('DEBUG:  Error updating email error in xxgil_ar_email_details.SQLCODE: ' || SQLCODE);
                  END;

                WHEN OTHERS
                THEN
                  dbms_output.put_line('DEBUG:  Oracle Internal Error in sending mail.SQLCODE: ' || SQLCODE);
                  lv_email_error := SUBSTR(SQLERRM,1,1000);

                  BEGIN
                      UPDATE xxgil_ar_email_details xaed
                         SET xaed.email_sent = 'No',
                             xaed.email_error = lv_email_error
                       WHERE xaed.request_id = p_request_id
                         AND xaed.file_data IS NOT NULL
                         AND (xaed.transaction_number, xaed.org_id, get_invoice_rec.ntile)
                             in
                             (
                           SELECT xaed_1.transaction_number
                                  ,xaed_1.org_id
                                  ,CEIL(RANK() over (ORDER BY ROWNUM DESC)/lv_attachment_limit) AS NTILE
                             FROM xxgil_ar_email_details xaed_1
                            WHERE xaed_1.request_id = p_request_id
                              AND xaed_1.bill_to_site = get_customer_rec.bill_to_site
                              AND xaed_1.email_address = get_customer_rec.email_address
                              AND xaed_1.file_data IS NOT NULL
                              );

                       lv_NTILE := 0;
                       lv_attachment_count := 0;
                  EXCEPTION
                    WHEN OTHERS THEN
                      dbms_output.put_line('DEBUG:  Error updating email error in xxgil_ar_email_details.SQLCODE: ' || SQLCODE);
                  END;
              END;
        END LOOP; /* end of get_invoice_rec*/
       lv_subject := NULL;
    END LOOP; /* end of get_customer_rec*/

    /* Update the print flags */
    FOR get_transactions_rec IN cur_get_transactions LOOP
      BEGIN

        UPDATE ra_customer_trx_all rct
        SET rct.attribute10 = 'Yes',
          printing_pending = DECODE (get_transactions_rec.trx_type, 'CM', 'N',
                                    DECODE(get_transactions_rec.term_count,
                                           GREATEST(NVL(last_printed_sequence_num,0),
                                                        get_transactions_rec.term_sequence_number), 'N',
                                                        NULL, 'N',
                                                         1, 'N',
                                                        0, 'N',
                                                            'Y')),
           printing_count = DECODE(get_transactions_rec.printing_count, null, 0,
                                                                        get_transactions_rec.printing_count) + 1,
           printing_last_printed = SYSDATE,
           printing_original_date = DECODE(get_transactions_rec.printing_count, 0, SYSDATE,
                                                                                    get_transactions_rec.printing_original_date),
           last_printed_sequence_num = DECODE(get_transactions_rec.term_count,NULL,NULL,
                                                                              GREATEST(NVL(last_printed_sequence_num,0),
                                                                              get_transactions_rec.term_sequence_number))
        WHERE rct.trx_number = get_transactions_rec.transaction_number
          AND rct.org_id = get_transactions_rec.org_id;

      EXCEPTION
        WHEN OTHERS THEN
        dbms_output.put_line('Error updating print related details due to - SQLCODE: '||SQLCODE );
        RAISE;
      END;
    END LOOP;

    lv_err_flag := xxgil_email_comm_invoice_pkg.send_summary_email (p_request_id);

    IF (lv_err_flag = 'S') THEN
      dbms_output.put_line('Summary email submitted successfully' );
    ELSE
      dbms_output.put_line('Error while submitting summary email' );
    END IF;

  EXCEPTION
    WHEN OTHERS
    THEN
      dbms_output.put_line('send_mail_with_invoice package Error' );
      RAISE;
  END send_mail_with_invoice;

   /**************************************************************************
    ||
    ||   Filename      : xxgil_email_comm_invoice_pkg.pkb
    ||   Procedure Name: send_summary_email
    ||   Description   : sends summary email to the common email address set at Operating Unit Level
    ||   Usage/Purpose :
    ||
    ||   Ver  Date          Author                      Modification/History
    ||   1.0  26-JUN-17     Sankalpa Saha                Created
    ||
    ***************************************************************************/

   FUNCTION send_summary_email (p_request_id IN NUMBER)
      RETURN VARCHAR2
   IS
      lv_concurrent_program   VARCHAR2 (2000)     := NULL;
      lv_request_id           NUMBER              := 0;
      lv_actual_start_date    DATE                := NULL;
      lv_total_email          NUMBER              := 0;
      lv_total_failed         NUMBER              := 0;
      lv_from                 VARCHAR2 (240);
      lv_recipient            VARCHAR2 (240)      := NULL;
      lv_subject              VARCHAR2 (240);
      lv_mail_host            VARCHAR2 (240)      :=  fnd_profile.value('FND_SMTP_HOST'); --'mailrelay.gilead.com';
      lv_mail_conn            UTL_SMTP.connection;
      lv_message_body         VARCHAR2 (3000);
      lv_err_flag             VARCHAR2 (3)        := 'S';
      lv_email_sent           VARCHAR2 (3)        := NULL;
      lv_org_name             VARCHAR2 (2000)     := NULL;
      lv_error                VARCHAR2 (2000)     := NULL;
      lv_submitted_user       fnd_user.user_name%type := NULL;
      lv_cc_email_address     VARCHAR2 (240)      := NULL;

      CURSOR cur_invoice_details (p_request_id IN NUMBER, p_email_sent IN VARCHAR2)
      IS
         SELECT xaed.customer_number,
                hp.party_name customer_name,
                xaed.bill_to_site,
                xaed.transaction_number,
                arl.meaning    transaction_class,
                xaed.email_address email_address,
                xaed.email_error
           FROM ra_customer_trx_all rca,
                ra_cust_trx_types_all rcta,
                hz_parties hp,
                hz_cust_accounts hca,
                xxgil_ar_email_details xaed,
                ar_lookups arl
          WHERE 1 = 1
            AND rca.cust_trx_type_id = rcta.cust_trx_type_id
            AND rcta.type = arl.lookup_code
            AND arl.lookup_type = 'INV/CM'
            AND arl.enabled_flag = 'Y'
            AND TRUNC (SYSDATE) BETWEEN TRUNC (NVL(arl.start_date_active,SYSDATE)) AND TRUNC (NVL (arl.end_date_active, SYSDATE))
            AND rca.trx_number = xaed.transaction_number
            AND rca.org_id = xaed.org_id
            AND hca.cust_account_id = rca.bill_to_customer_id
            AND hp.party_id = hca.party_id
            AND xaed.request_id = p_request_id
            AND NVl(xaed.email_sent,'No') = p_email_sent
            ORDER BY xaed.bill_to_site,
                     xaed.transaction_number;
   BEGIN
      DBMS_OUTPUT.ENABLE(buffer_size => NULL);

      BEGIN
         SELECT  hou.NAME,
                 xaed.cc_email_address
            INTO lv_org_name,
                 lv_cc_email_address
            FROM hr_operating_units hou,
                 xxgil_ar_email_details xaed
           WHERE xaed.org_id = hou.organization_id
             AND xaed.request_id = p_request_id
             AND xaed.file_data IS NOT NULL
         GROUP BY hou.NAME,
                  xaed.cc_email_address;

         dbms_output.put_line(
                            'DEBUG:  Org Name.' || lv_org_name || CHR (10)
                           );
      EXCEPTION
         WHEN OTHERS THEN
            lv_org_name := NULL;
            dbms_output.put_line(
                               'DEBUG:  Operating unit is NULL.'
                              );
            RAISE;
      END;

      BEGIN
         SELECT ffv.attribute1
           INTO lv_from
           FROM fnd_flex_value_sets ffvs,
                fnd_flex_values ffv
          WHERE ffvs.flex_value_set_id = ffv.flex_value_set_id
            AND ffvs.flex_value_set_name = 'XXGIL_AR_EMAIL_TRANSACTIONS'
            AND ffv.enabled_flag = 'Y'
            AND TRUNC (SYSDATE) BETWEEN TRUNC (NVL(ffv.start_date_active,SYSDATE)) AND TRUNC (NVL (ffv.end_date_active, SYSDATE))
            AND flex_value = lv_org_name;
      EXCEPTION
         WHEN OTHERS THEN
            lv_from := NULL;
      END;

      lv_recipient := lv_from ;

      BEGIN
         SELECT   fcpt.user_concurrent_program_name concurrent_program,
                  fcr.request_id,
                  fcr.actual_start_date,
                  FU.user_name,
                  SUM(DECODE(NVL(email_sent,'No'),'Yes',1,0)) total_email,
                  SUM(DECODE(NVL(email_sent,'No'),'No',1,0)) total_failed
             INTO lv_concurrent_program,
                  lv_request_id,
                  lv_actual_start_date,
                  lv_submitted_user,
                  lv_total_email,
                  lv_total_failed
             FROM fnd_concurrent_requests fcr,
                  fnd_concurrent_programs_tl fcpt,
                  fnd_concurrent_programs fcp,
                  xxgil_ar_email_details xaed,
                  fnd_user fu
            WHERE 1 = 1
              AND xaed.request_id = fcr.request_id
              AND fcr.concurrent_program_id = fcpt.concurrent_program_id
              AND fcpt.concurrent_program_id = fcp.concurrent_program_id
              AND fcp.enabled_flag = 'Y'
              AND fcpt.LANGUAGE = 'US'
              AND fcr.request_id = p_request_id
              AND xaed.file_data IS NOT NULL
              AND fu.user_id = fcr.requested_by
         GROUP BY fcr.request_id,
                  fu.user_name,
                  fcpt.user_concurrent_program_name,
                  fcr.actual_start_date;

      EXCEPTION
         WHEN OTHERS THEN
            lv_concurrent_program := NULL;
            lv_request_id := 0;
            lv_actual_start_date := NULL;
            lv_total_email := 0;
            lv_from := NULL;
            lv_submitted_user := NULL;
            lv_total_failed := 0;
      END;

      IF lv_total_failed = 0 then
        lv_subject :=
           'Email AR Transactions for ' || lv_org_name || ' - '
           || TRUNC (SYSDATE);
      ELSE
        lv_subject :=
           'Email AR Transactions for ' || lv_org_name || ' - '
           || TRUNC (SYSDATE) || ' (Check failed records)';      
      END IF;

      lv_message_body :=
            'Concurrent Program Name : '
         || lv_concurrent_program
         || '<br>'
         || 'Concurrent Request id :'
         || lv_request_id
         || '<br>'
         || 'Date : '
         || TO_CHAR(lv_actual_start_date,'DD-MON-YYYY HH24:MI:SS')
         || '<br>'
         || 'Submitted by : '
         || lv_submitted_user
         || '<br>'
         || 'Operating Unit :'
         || lv_org_name
         || '<br>'
         || 'Total Transactions Emailed : '
         || lv_total_email
         || '<br><br><br>';

      lv_mail_conn := UTL_SMTP.open_connection (lv_mail_host, 25);

      UTL_SMTP.helo (lv_mail_conn, lv_mail_host);
      UTL_SMTP.mail (lv_mail_conn, lv_from);
      UTL_SMTP.rcpt (lv_mail_conn, lv_recipient);

      IF lv_cc_email_address IS NOT NULL THEN
        UTL_SMTP.rcpt( lv_mail_conn, lv_cc_email_address );
      END IF;
      UTL_SMTP.open_data (lv_mail_conn);
      UTL_SMTP.write_data (lv_mail_conn,
                              'Date: '
                           || TO_CHAR (SYSDATE, 'DD-MON-YYYY HH24:MI:SS')
                           || UTL_TCP.crlf
                          );
      UTL_SMTP.write_data (lv_mail_conn,
                           'To: ' || lv_recipient || UTL_TCP.crlf
                          );
      UTL_SMTP.write_data (lv_mail_conn, 'From: ' || lv_from || UTL_TCP.crlf);

      IF lv_cc_email_address IS NOT NULL THEN
         UTL_SMTP.write_data(lv_mail_conn, 'CC: ' || lv_cc_email_address || UTL_TCP.crlf);
      END IF;

      UTL_SMTP.write_data (lv_mail_conn,
                           'Subject: ' || lv_subject || UTL_TCP.crlf
                          );
      UTL_SMTP.write_data (lv_mail_conn,
                           'Reply-To: ' || lv_recipient || UTL_TCP.crlf
                          );

      UTL_SMTP.write_data (lv_mail_conn, 'MIME-Version: 1.0' || UTL_TCP.crlf);
      UTL_SMTP.write_data (lv_mail_conn,
                              '--' || '----=*#abc1234321cba#*=' || UTL_TCP.crlf
                             );
      UTL_SMTP.write_data
                           (lv_mail_conn,
                               'Content-Type: text/html; charset="iso-8859-1"'
                            || UTL_TCP.crlf
                            || UTL_TCP.crlf
                           );

      UTL_SMTP.write_data (lv_mail_conn, lv_message_body);

      lv_message_body :=
         '<table style="border:0px solid #000000;" cellpadding="0" cellspacing="0">
                        <tr>
                        <th style="border:1px solid #000000"  bgcolor="#C0C0C0"><FONT FACE="Arial" SIZE="2"><B>Customer Name</B></FONT></th>
                        <th style="border:1px solid #000000"  bgcolor="#C0C0C0"><FONT FACE="Arial" SIZE="2"><B>Customer Number</B></FONT></th>
                        <th style="border:1px solid #000000"  bgcolor="#C0C0C0"><FONT FACE="Arial" SIZE="2"><B>Bill to site Number</B></FONT></th>
                        <th style="border:1px solid #000000"  bgcolor="#C0C0C0"><FONT FACE="Arial" SIZE="2"><B>Transaction Number</B></FONT></th>
                        <th style="border:1px solid #000000"  bgcolor="#C0C0C0"><FONT FACE="Arial" SIZE="2"><B>Transaction Class </B></FONT></th>
                        <th style="border:1px solid #000000"  bgcolor="#C0C0C0"><FONT FACE="Arial" SIZE="2"><B>Emailed To</B></FONT></th>
                        </tr>';

      UTL_SMTP.write_data (lv_mail_conn, lv_message_body);

      lv_email_sent := 'Yes';
      FOR rec_invoice_details IN cur_invoice_details (p_request_id,lv_email_sent)
      LOOP

        lv_message_body := ' <tr>'
           || '<TD style="border:1px solid #000000" cellspacing="10"><FONT FACE="Arial" SIZE="2">'
           || rec_invoice_details.customer_name
           || '</FONT></TD>'
           || '<TD style="border:1px solid #000000" cellspacing="50"><FONT FACE="Arial" SIZE="2">'
           || rec_invoice_details.customer_number
           || '</FONT></TD>'
           || '<TD style="border:1px solid #000000" cellspacing="50"><FONT FACE="Arial" SIZE="2">'
           || rec_invoice_details.bill_to_site
           || '</FONT></TD>'
           || '<TD style="border:1px solid #000000" cellspacing="50"><FONT FACE="Arial" SIZE="2">'
           || rec_invoice_details.transaction_number
           || '</FONT></TD>'
           || '<TD style="border:1px solid #000000" cellspacing="10"><FONT FACE="Arial" SIZE="2">'
           || rec_invoice_details.transaction_class
           || '</FONT></TD>'
           || '<TD style="border:1px solid #000000" cellspacing="10"><FONT FACE="Arial" SIZE="2">'
           || rec_invoice_details.email_address
           || '</FONT></TD>'
           || '</tr>';

        dbms_output.put_line( rec_invoice_details.customer_name
                  ||'  '||rec_invoice_details.customer_number
                  ||'  '||rec_invoice_details.bill_to_site
                  ||'  '||rec_invoice_details.transaction_number
                  ||'  '||rec_invoice_details.transaction_class
                  ||'  '||rec_invoice_details.email_address);

        UTL_SMTP.write_data (lv_mail_conn, lv_message_body);
      END LOOP;

      IF lv_total_failed > 0 THEN
          /* send the failure details*/
          lv_message_body := '</table><br><br>'
             || 'Total Transactions Failed : '
             || lv_total_failed
             || '<br><br><br>';

          UTL_SMTP.write_data (lv_mail_conn, lv_message_body);

          lv_message_body :=
             '<table style="border:0px solid #000000;" cellpadding="0" cellspacing="0">
                            <tr>
                            <th style="border:1px solid #000000"  bgcolor="#C0C0C0"><FONT FACE="Arial" SIZE="2"><B>Customer Name</B></FONT></th>
                            <th style="border:1px solid #000000"  bgcolor="#C0C0C0"><FONT FACE="Arial" SIZE="2"><B>Customer Number</B></FONT></th>
                            <th style="border:1px solid #000000"  bgcolor="#C0C0C0"><FONT FACE="Arial" SIZE="2"><B>Bill to site Number</B></FONT></th>
                            <th style="border:1px solid #000000"  bgcolor="#C0C0C0"><FONT FACE="Arial" SIZE="2"><B>Transaction Number</B></FONT></th>
                            <th style="border:1px solid #000000"  bgcolor="#C0C0C0"><FONT FACE="Arial" SIZE="2"><B>Transaction Class </B></FONT></th>
                            <th style="border:1px solid #000000"  bgcolor="#C0C0C0"><FONT FACE="Arial" SIZE="2"><B>Error Details</B></FONT></th>
                            </tr>';

          UTL_SMTP.write_data (lv_mail_conn, lv_message_body);

          lv_email_sent := 'No';
          FOR rec_invoice_details IN cur_invoice_details (p_request_id,lv_email_sent)
          LOOP

           lv_message_body := ' <tr>'
           || '<TD style="border:1px solid #000000" cellspacing="10"><FONT FACE="Arial" SIZE="2">'
           || rec_invoice_details.customer_name
           || '</FONT></TD>'
           || '<TD style="border:1px solid #000000" cellspacing="50"><FONT FACE="Arial" SIZE="2">'
           || rec_invoice_details.customer_number
           || '</FONT></TD>'
           || '<TD style="border:1px solid #000000" cellspacing="50"><FONT FACE="Arial" SIZE="2">'
           || rec_invoice_details.bill_to_site
           || '</FONT></TD>'
           || '<TD style="border:1px solid #000000" cellspacing="50"><FONT FACE="Arial" SIZE="2">'
           || rec_invoice_details.transaction_number
           || '</FONT></TD>'
           || '<TD style="border:1px solid #000000" cellspacing="10"><FONT FACE="Arial" SIZE="2">'
           || rec_invoice_details.transaction_class
           || '</FONT></TD>'
           || '<TD style="border:1px solid #000000" cellspacing="10"><FONT FACE="Arial" SIZE="2">'
           || rec_invoice_details.email_error
           || '</FONT></TD>'
           || '</tr>';

           dbms_output.put_line( rec_invoice_details.customer_name
                  ||'  '||rec_invoice_details.customer_number
                  ||'  '||rec_invoice_details.bill_to_site
                  ||'  '||rec_invoice_details.transaction_number
                  ||'  '||rec_invoice_details.transaction_class
                  ||'  '||rec_invoice_details.email_address);

           UTL_SMTP.write_data (lv_mail_conn, lv_message_body);
          END LOOP;
     END IF;

    UTL_SMTP.write_data(lv_mail_conn,utl_tcp.crlf || utl_tcp.crlf);

    UTL_SMTP.close_data (lv_mail_conn);
    UTL_SMTP.quit (lv_mail_conn);
    RETURN lv_err_flag;
   EXCEPTION
      WHEN OTHERS THEN
         lv_err_flag := 'E';

         lv_error := SUBSTR(SQLERRM,1,1000);
         dbms_output.put_line('DEBUG:  Error in WHEN OTHERS of send_summary_email.' || lv_error);
         RAISE;

   END send_summary_email;

  /**************************************************************************
    ||
    ||   Filename      : xxgil_email_comm_invoice_pkg.pkb
    ||   Procedure Name: customer_email_address
    ||   Description   : fetches email address of customer from bill-to site/ customer profile/ OU level
    ||   Usage/Purpose :
    ||
    ||   Ver  Date          Author                      Modification/History
    ||   1.0  26-JUN-17     Sankalpa Saha                Created
    ||   1.1  23-JUL-2020   Kalyani M			 Changed for INC1094857
    ||
    ***************************************************************************/

   FUNCTION customer_email_address (p_cust_trx_id IN NUMBER)
      RETURN VARCHAR2
   IS
      lv_org_name                VARCHAR2 (2000) := NULL;

      -- Cursor fetch email address of customer set at bill-to site level
      CURSOR cur_bill_to_email_address (p_cust_trx_id IN VARCHAR2)
      IS
         SELECT hcp.email_address
           FROM hz_cust_site_uses hcsu,
                hz_cust_acct_sites hcasa,
                ra_customer_trx rca,
                hz_cust_accounts hca,
                hz_cust_account_roles hcar,
                hz_relationships hr,
                hz_contact_points hcp,
                hz_role_responsibility hrr,
                ra_cust_trx_types rcta
          WHERE rca.bill_to_site_use_id = hcsu.site_use_id
            AND rca.bill_to_customer_id = hca.cust_account_id
            AND hca.cust_account_id = hcasa.cust_account_id
            AND hcsu.cust_acct_site_id = hcasa.cust_acct_site_id
            AND hcsu.cust_acct_site_id = hcar.cust_acct_site_id
            AND hcar.cust_account_role_id = hrr.cust_account_role_id
            AND rca.cust_trx_type_id = rcta.cust_trx_type_id
            AND UPPER (hrr.responsibility_type) = DECODE(UPPER (rcta.TYPE),'DM','INV',UPPER (rcta.TYPE))
            AND hcsu.site_use_code = 'BILL_TO'
            AND hcar.role_type = 'CONTACT'
            AND hcar.status = 'A'
            AND hcar.party_id = hr.party_id
            AND hr.party_id = hcp.owner_table_id
            AND hr.directional_flag = 'F'
            AND hr.status = 'A'
            AND hcp.status = 'A'
            AND hcp.contact_point_type = 'EMAIL'
	    AND hcp.application_id = 222 --Added for 1.1
            AND rca.customer_trx_id = p_cust_trx_id;

        -- Cursor fetch email address of customer set at account level
      CURSOR cur_customer_email_address (p_cust_trx_id IN VARCHAR2)
      IS
         SELECT hp.email_address
           FROM hz_contact_points hp,
                hz_cust_accounts hca,
                hz_cust_account_roles hcar,
                hz_role_responsibility hrr,
                hz_relationships hr,
                hz_parties org,
                hz_parties cont,
                ra_customer_trx rca
          WHERE hp.status = 'A'
            AND rca.bill_to_customer_id = hca.cust_account_id
            AND hp.owner_table_name = 'HZ_PARTIES'
            AND hcar.cust_account_role_id = hrr.cust_account_role_id
            AND hp.owner_table_id = hcar.party_id
            AND hcar.party_id = hr.party_id
            AND hcar.current_role_state = 'A'
            AND hca.cust_account_id = hcar.cust_account_id
            AND org.party_id = hca.party_id
            AND org.party_id = hr.subject_id
            AND hr.subject_type = 'ORGANIZATION'
            AND cont.party_id = hr.object_id
            AND hr.object_type = 'PERSON'
            AND hrr.responsibility_type = 'STMTS'
            AND rca.customer_trx_id = p_cust_trx_id;

      lv_email_address           VARCHAR2 (2000) := NULL;
      lv_trx_type                VARCHAR2 (2000) := NULL;
      lv_error                   VARCHAR2 (2000) := NULL;
   BEGIN
      lv_trx_type :=
         xxgil_email_comm_invoice_pkg.get_transaction_type
                                               (p_cust_trx_id      => p_cust_trx_id);

       FOR bill_to_email_address_rec IN cur_bill_to_email_address (p_cust_trx_id)
       LOOP
          lv_email_address :=
                lv_email_address
             || ';'
             || bill_to_email_address_rec.email_address;
       END LOOP;

       IF (lv_email_address IS NOT NULL) THEN
         lv_email_address := SUBSTR (lv_email_address, 2);
         RETURN lv_email_address;
       ELSE

           FOR customer_email_address_rec IN cur_customer_email_address (p_cust_trx_id)
           LOOP
              lv_email_address :=
                    lv_email_address
                 || ';'
                 || customer_email_address_rec.email_address;
           END LOOP;

           IF (lv_email_address IS NOT NULL) THEN

             lv_email_address := SUBSTR (lv_email_address, 2);
             RETURN lv_email_address;

           ELSE

              BEGIN
                SELECT hou.NAME
                  INTO lv_org_name
                  FROM ra_customer_trx rcta,
                       hr_operating_units hou
                 WHERE rcta.org_id = hou.organization_id
                   AND customer_trx_id = p_cust_trx_id;

              EXCEPTION
                WHEN OTHERS THEN
                   lv_org_name := NULL;
                   fnd_file.put_line (fnd_file.LOG,
                                      'DEBUG:  Operating unit is NULL.'
                                     );
                   RAISE;
              END;

              BEGIN
                SELECT ffv.attribute1
                  INTO lv_email_address
                  FROM fnd_flex_value_sets ffvs,
                       fnd_flex_values ffv
                 WHERE ffvs.flex_value_set_id = ffv.flex_value_set_id
                   AND ffvs.flex_value_set_name = 'XXGIL_AR_EMAIL_TRANSACTIONS'
                   AND ffv.enabled_flag = 'Y'
                   AND TRUNC (SYSDATE) BETWEEN TRUNC (NVL(ffv.start_date_active,SYSDATE)) AND TRUNC (NVL (ffv.end_date_active, SYSDATE))
                   AND flex_value = lv_org_name;
              EXCEPTION
                WHEN OTHERS THEN
                   lv_email_address := NULL;
              END;

              RETURN lv_email_address;
           END IF;
       END IF;
   EXCEPTION
      WHEN OTHERS
      THEN
         lv_email_address := NULL;

         lv_error := SUBSTR(SQLERRM,1,1000);
         fnd_file.put_line (fnd_file.LOG, 'DEBUG:  Error in WHEN OTHERS of customer_email_address.' || lv_error);
         RETURN NULL;
   END customer_email_address;

   /**************************************************************************
    ||
    ||   Filename      : xxgil_email_comm_invoice_pkg.pkb
    ||   Procedure Name: update_table
    ||   Description   : Insert records in custom table required for mailing purpose
    ||   Usage/Purpose :
    ||
    ||   Ver  Date          Author                      Modification/History
    ||   1.0  26-JUN-17     Sankalpa Saha                Created
    ||
    ***************************************************************************/

   FUNCTION update_table (
      p_conc_request_id    IN   NUMBER,
      p_customer_trx_id    IN   NUMBER,
      p_bill_to_site       IN   NUMBER,
      p_cc_email_address   IN   VARCHAR2,
      p_customer_number    IN   VARCHAR2,
      p_trx_type           IN   VARCHAR2,
      p_term_count         IN   NUMBER,      
      p_term_sequence_number    IN  NUMBER,
      p_printing_count  IN  NUMBER,
      p_printing_original_date  IN  DATE
   )
      RETURN VARCHAR2
   IS
      lv_email_address   VARCHAR2 (2000) := NULL;
      lv_trx_number      NUMBER          := 0;
      lv_org_id          NUMBER          := 0;
      lv_file_name       VARCHAR2 (2000) := NULL;
      lv_bill_to_site    NUMBER          := 0;
      lv_trans_class     VARCHAR2 (2000) := NULL;
      lv_org_name        VARCHAR2 (2000) := NULL;
      lv_error           VARCHAR2 (2000) := NULL;
   BEGIN
      lv_email_address := customer_email_address (p_cust_trx_id      => p_customer_trx_id);
      lv_file_name := attachment_title (p_cust_trx_id => p_customer_trx_id);
      lv_file_name := lv_file_name || '.pdf';

      IF (lv_email_address IS NULL)
      THEN
         RETURN 'N';
      END IF;

      IF (p_conc_request_id IS NULL)
      THEN
         RETURN 'N';
      END IF;

      IF (p_customer_trx_id IS NULL)
      THEN
         RETURN 'N';
      END IF;

      BEGIN
         SELECT hps.party_site_number bill_to_site
           INTO lv_bill_to_site
           FROM hz_party_sites hps,
                hz_cust_accounts hca,
                hz_cust_acct_sites hcas,
                hz_cust_site_uses hcsu,
                ra_customer_trx rca
          WHERE 1 = 1
            AND rca.bill_to_site_use_id = hcsu.site_use_id
            AND rca.bill_to_customer_id = hca.cust_account_id
            AND hca.cust_account_id = hcas.cust_account_id
            AND hcas.cust_acct_site_id = hcsu.cust_acct_site_id
            AND hps.party_site_id = hcas.party_site_id
            AND hcsu.site_use_code = 'BILL_TO'
            AND rca.customer_trx_id = p_customer_trx_id;

      EXCEPTION
         WHEN OTHERS THEN
            lv_bill_to_site := p_bill_to_site;
            fnd_file.put_line (fnd_file.LOG,
                                  'DEBUG:  Bill-To Site/ Bill-To-Site ID.'
                               || lv_bill_to_site
                              );
      END;


      BEGIN
         SELECT rcta.trx_number,
                rcta.org_id,
                hou.NAME
           INTO lv_trx_number,
                lv_org_id,
                lv_org_name
           FROM ra_customer_trx rcta,
                hr_operating_units hou
          WHERE rcta.org_id = hou.organization_id
            AND customer_trx_id = p_customer_trx_id;

      EXCEPTION
         WHEN OTHERS THEN
            fnd_file.put_line
                     (fnd_file.LOG,
                      'DEBUG:  transaction number or operating unit is NULL.'
                     );
            RETURN 'N';
      END;

      BEGIN
         SELECT arl.lookup_code  transaction_class
           INTO lv_trans_class
           FROM ra_customer_trx rca,
                ra_cust_trx_types rcta,
                ar_lookups arl
          WHERE 1 = 1
            AND rca.cust_trx_type_id = rcta.cust_trx_type_id
            AND rcta.type = arl.lookup_code
            AND arl.lookup_type = 'INV/CM'
            AND arl.enabled_flag = 'Y'
            AND TRUNC (SYSDATE) BETWEEN TRUNC (NVL(arl.start_date_active,SYSDATE)) AND TRUNC (NVL (arl.end_date_active, SYSDATE))
            AND rca.customer_trx_id = p_customer_trx_id;

      EXCEPTION
         WHEN OTHERS THEN
            fnd_file.put_line (fnd_file.LOG,
                               'DEBUG:  Transaction Class is NULL.'
                              );
            RETURN 'N';
      END;

      BEGIN
         INSERT INTO xxgil_ar_email_details
                     (request_id,
                      transaction_number,
                      bill_to_site,
                      email_address,
                      cc_email_address,
                      org_id,
                      file_name,
                      customer_number,
                      transaction_class,
                      org_name,                     
                      term_sequence_number,
                      trx_type,
                      term_count,
                      printing_count,
                      printing_original_date,
                      last_updated_date,
                      last_updated_by,
                      last_update_login,
                      creation_date,
                      created_by
                     )
              VALUES (p_conc_request_id,
                      lv_trx_number,
                      lv_bill_to_site,
                      lv_email_address,
                      p_cc_email_address,
                      lv_org_id,
                      lv_file_name,
                      p_customer_number,
                      lv_trans_class,
                      lv_org_name,                     
                      p_term_sequence_number,
                      p_trx_type,
                      p_term_count,
                      p_printing_count,
                      p_printing_original_date,
                      SYSDATE,                          --last_updated_date
                      fnd_profile.VALUE ('USER_ID'),    --last_updated_by
                      USERENV ('SESSIONID'),            --last_updated_login
                      SYSDATE,                          --creation_date
                      fnd_profile.VALUE ('USER_ID')
                     );

         RETURN 'Y';
      EXCEPTION
         WHEN OTHERS THEN
            fnd_file.put_line
               (fnd_file.LOG,
                   'DEBUG:  Error while inserting data in custom table xxgil_email_test. '
                || SQLCODE||'-'||SUBSTR(SQLERRM,1,1000)
               );
            RETURN 'N';

      END;
   EXCEPTION
      WHEN OTHERS THEN

       lv_error := SUBSTR(SQLERRM,1,1000);
       fnd_file.put_line (fnd_file.LOG, 'DEBUG:  Error in WHEN OTHERS of update_table.' || lv_error);
       RETURN 'N';
   END update_table;

/**************************************************************************
    ||
    ||   Filename      : xxgil_email_comm_invoice_pkg.pkb
    ||   Procedure Name: wait_for_request
    ||   Description   : wait for bursting request to complete
    ||   Usage/Purpose :
    ||
    ||   Ver  Date          Author                      Modification/History
    ||   1.0  26-JUN-17     Sankalpa Saha                Created
    ||
    ***************************************************************************/

  PROCEDURE wait_for_request (p_request_id       IN NUMBER)
  AS
    lv_request_id       NUMBER;
    lv_phase            VARCHAR2(50);
    lv_status           VARCHAR2(50);
    lv_dev_phase        VARCHAR2(50);
    lv_dev_status       VARCHAR2(50);
    lv_message          VARCHAR2(50);
    lv_phase_code       CHAR(1);
    lv_status_code      CHAR(1);
    lv_resp_appl_id     fnd_concurrent_requests.responsibility_application_id%type;
    lv_resp_id          fnd_concurrent_requests.responsibility_id%type;
    lv_user_id          fnd_concurrent_requests.requested_by%type;
    lv_req_return_status BOOLEAN;
    bursting_excep      EXCEPTION;
    email_excep         EXCEPTION;
    lv_error_msg        VARCHAR2(2000) := NULL;

  BEGIN

     BEGIN

      dbms_output.put_line('Get the details for the Email Program');

      SELECT fcr.request_id,
             fcr.phase_code,
             fcr.status_code,
             fcr.responsibility_application_id,
             fcr.responsibility_id,
             fcr.requested_by
        INTO lv_request_id,
             lv_phase_code,
             lv_status_code,
             lv_resp_appl_id,
             lv_resp_id,
             lv_user_id
        FROM fnd_concurrent_requests fcr
      WHERE fcr.request_id = p_request_id;

      IF lv_phase_code = 'C' THEN
        IF lv_status_code = 'E' THEN
          dbms_output.put_line('The Email Program completed in error. Oracle request id: '||lv_request_id );
          RAISE email_excep;
        ELSIF lv_status_code IN ('X','D') THEN
          dbms_output.put_line('The Email Program Terminated / Cancelled. Oracle request id: '||lv_request_id);
          RAISE email_excep;
        ELSE
          dbms_output.put_line( 'The Email Program request successful for request id: ' || lv_request_id);
          lv_request_id := NULL;
        END IF;
      END IF;

    EXCEPTION
    WHEN email_excep THEN
        RAISE;
    WHEN OTHERS THEN
        dbms_output.put_line('The Email Program not found for request id: '||lv_request_id ||' '||SQLERRM);
        RAISE;
    END;

    --
    --Setting Context
    --

     fnd_global.apps_initialize (
        user_id             => lv_user_id,
        resp_id             => lv_resp_id,
        resp_appl_id        => lv_resp_appl_id
        );

    --
    -- Wait for Email Program for complete;
    --

    --dbms_output.put_line('id ' || lv_user_id || ' id2 '|| lv_resp_id  || ' id3 '|| lv_resp_appl_id );

    IF lv_request_id IS NOT NULL THEN
      LOOP

        --
        --To make process execution to wait for 1st program to complete
        --

        lv_req_return_status := fnd_concurrent.wait_for_request (request_id      => lv_request_id
                                                                  ,interval        => 5 --interval Number of seconds to wait between checks
                                                                  ,max_wait        => 60 --Maximum number of seconds to wait for the request completion
                                                                  ,phase           => lv_phase
                                                                  ,status          => lv_status
                                                                  ,dev_phase       => lv_dev_phase
                                                                  ,dev_status      => lv_dev_status
                                                                  ,message         => lv_message );

        EXIT WHEN UPPER (lv_phase) = 'COMPLETED' OR UPPER (lv_status) IN ('CANCELLED', 'ERROR', 'TERMINATED','WARNING');
      END LOOP;

      IF UPPER (lv_phase) = 'COMPLETED' THEN
        IF UPPER (lv_status) = 'ERROR' THEN
          dbms_output.put_line('The Email Program completed in error. Oracle request id: '||lv_request_id );
          RAISE email_excep;
        ELSIF UPPER (lv_status) IN ('TERMINATED','CANCELLED')  THEN
           dbms_output.put_line('The Email Program Terminated / Cancelled. Oracle request id: '||lv_request_id );
           RAISE email_excep;
        ELSIF UPPER (lv_status) IN ('NORMAL','WARNING') THEN
          dbms_output.put_line( 'The Email Program request successful for request id: ' || lv_request_id);
        END IF;
      END IF;
    END IF;

    --
    -- Submit and wait for Bursting program request id;
    --

    lv_request_id := NULL;
    BEGIN

      lv_request_id := fnd_request.submit_request (application => 'XDO',
                                                   program =>'XDOBURSTREP',
                                                    description =>NULL,
                                                    start_time => NULL,
                                                    sub_request => FALSE,
                                                    argument1 => 'N',
                                                    argument2 => p_request_id,
                                                    argument3 => 'Y',
                                                    argument4 => NULL,
                                                    argument5 => NULL,
                                                    argument6 => NULL  ) ;

      COMMIT;
    EXCEPTION
      WHEN OTHERS THEN
        lv_error_msg := SQLCODE||'-'||SUBSTR(1,1000,SQLERRM);
        dbms_output.put_line(' Exception while submitting bursting program :' || lv_error_msg);
        RAISE bursting_excep;
    END ;

    IF lv_request_id = 0 THEN
       dbms_output.put_line('Failed to submit bursting program ' || lv_request_id );
       RAISE bursting_excep;
    ELSE
      LOOP

        --
        --To make process execution to wait for 1st program to complete
        --

        lv_req_return_status :=  fnd_concurrent.wait_for_request (request_id      => lv_request_id
                                                                  ,interval        => 5 --interval Number of seconds to wait between checks
                                                                  ,max_wait        => 60 --Maximum number of seconds to wait for the request completion
                                                                  ,phase           => lv_phase
                                                                  ,status          => lv_status
                                                                  ,dev_phase       => lv_dev_phase
                                                                  ,dev_status      => lv_dev_status
                                                                  ,message         => lv_message );

        EXIT WHEN UPPER (lv_phase) = 'COMPLETED' OR UPPER (lv_status) IN ('CANCELLED', 'ERROR', 'TERMINATED','WARNING');
      END LOOP;

      IF UPPER (lv_phase) = 'COMPLETED' THEN
        IF UPPER (lv_status) = 'ERROR' THEN
          dbms_output.put_line('The Bursting Program completed in error. Oracle request id: '||lv_request_id );
          RAISE bursting_excep;
        ELSIF UPPER (lv_status) IN ('TERMINATED','CANCELLED')  THEN
           dbms_output.put_line('The Bursting Program Terminated / Cancelled. Oracle request id: '||lv_request_id);
           RAISE bursting_excep;
        ELSIF UPPER (lv_status) IN ('NORMAL','WARNING') THEN
          dbms_output.put_line( 'The Bursting Program request successful for request id: ' || lv_request_id);
        END IF;
      END IF;
    END IF;

  EXCEPTION
    WHEN email_excep THEN
        RAISE;
    WHEN bursting_excep THEN
        RAISE;
    WHEN OTHERS THEN
        RAISE;
  END wait_for_request;

END xxgil_email_comm_invoice_pkg;
/