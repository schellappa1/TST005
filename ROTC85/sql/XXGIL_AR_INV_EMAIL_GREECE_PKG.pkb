CREATE OR REPLACE PACKAGE BODY xxgil_ar_inv_email_greece_pkg AS

/*****************************************************************************************
*   Filename  :        xxgil_ar_inv_email_greece_pkg.pkb
*   Package Name:      xxgil_ar_inv_email_greece_pkg
*   Description :      Package Body is Created for Emailing Functionality of
*                           Greece Receivable Invoices (CR#2032)
*   Usage/Purpose :    Used in Concurrent Program - GIL Email Commercial Invoice - Greece
*
****************************Modification History******************************************
*   Ver     Date         Author               Modification/History
*   1.0     Nov-2019     pkutty               Intital Version for CR#2032
*   2.0     Jul-2019     stylam2              Increasing program wait time   #CHG0061534
*
**************************************************************************************** */

--===================================================================================================================================
/* Function to return full path of custom top with required path to store files based on request id*/

       FUNCTION get_custom_path (
        p_request_id IN NUMBER
    ) RETURN VARCHAR2 IS
l_full_path    VARCHAR2(240);
        l_top_name     VARCHAR2(50);
        l_stage_path   VARCHAR2(240);
    BEGIN
        NULL;
        BEGIN
            SELECT
                attribute2
            INTO l_stage_path
            FROM
                fnd_lookup_values
            WHERE
                lookup_type = 'XXGIL_ROTC85_GR_INV_SOURCE'
                AND lookup_code = 'ORDER MANAGEMENT'
                AND language = 'US'
                AND SYSDATE BETWEEN nvl(start_date_active, SYSDATE - 1) AND nvl(end_date_active, SYSDATE + 1)
                AND enabled_flag = 'Y';

        EXCEPTION
            WHEN OTHERS THEN
                l_stage_path := NULL;
       --RAISE;
        END;

        l_top_name := substr(l_stage_path, 2, instr(l_stage_path, '/', 1) - 2);

        BEGIN
            SELECT DISTINCT
    --variable_name   "TOP_NAME",
                value
            INTO l_full_path
            FROM
                fnd_env_context            fec,
                fnd_concurrent_processes   fcp,
                fnd_concurrent_requests    fcr
            WHERE
                1 = 1
                AND fec.concurrent_process_id = fcp.concurrent_process_id
                AND fec.concurrent_process_id = fcr.controlling_manager
                AND fec.variable_name = l_top_name
                AND fcr.request_id = p_request_id;

        EXCEPTION
            WHEN OTHERS THEN
                l_full_path := NULL;
        END;

        l_stage_path := l_full_path
                        || substr(l_stage_path, instr(l_stage_path, '/', 1));

        RETURN l_stage_path;
    EXCEPTION
        WHEN OTHERS THEN
            fnd_file.put_line(fnd_file.log, 'Error In  get_custom_path FUNCTION- ' || sqlerrm);
            RETURN NULL;
    END get_custom_path;

--====================================================================================================================================
  PROCEDURE get_email_details(p_request_id IN NUMBER,
                            p_stage_path OUT VARCHAR2,
                            p_smtp_host OUT VARCHAR2,
                            p_smtp_port OUT VARCHAR2,
                            p_from_email OUT VARCHAR2,
                            p_to_email OUT VARCHAR2,
                            p_msg_body OUT VARCHAR2,
                            p_templ_loc OUT VARCHAR2) IS

    l_full_path VARCHAR2(240);
    l_top_name VARCHAR2(50);
    BEGIN
    NULL;

    BEGIN
            SELECT
            attribute1,
            attribute2,
            attribute3
        INTO
            p_to_email,
            p_stage_path,
            p_from_email
        FROM
            fnd_lookup_values
        WHERE
            lookup_type = 'XXGIL_ROTC85_GR_INV_SOURCE'
            AND lookup_code = 'ORDER MANAGEMENT'
            AND language = 'US'
            AND SYSDATE BETWEEN nvl(start_date_active, SYSDATE - 1) AND nvl(end_date_active, SYSDATE + 1)
            AND enabled_flag = 'Y';
       EXCEPTION
       WHEN OTHERS THEN
       p_to_email := null;
       --RAISE;
       END;

     l_top_name := substr(p_stage_path, 2, INSTR(p_stage_path, '/', 1)-2);
     
     IF l_top_name LIKE '%TOP' THEN

     BEGIN
     SELECT DISTINCT
    --variable_name   "TOP_NAME",
    value  INTO l_full_path
FROM
    fnd_env_context            fec,
    fnd_concurrent_processes   fcp,
    fnd_concurrent_requests    fcr
WHERE
    1 = 1
    AND fec.concurrent_process_id = fcp.concurrent_process_id
    AND fec.concurrent_process_id = fcr.controlling_manager
    AND fec.variable_name = l_top_name
    AND fcr.request_id = p_request_id;

     EXCEPTION
     WHEN OTHERS THEN
     --l_full_path := '/appltop/ERPPRD/apps/apps_st/appl/xxgil/12.0.0';
     NULL;
     END;

     p_stage_path := l_full_path||substr(p_stage_path, INSTR(p_stage_path, '/', 1));
END IF;
     

     p_smtp_host := fnd_profile.value('FND_SMTP_HOST');
     p_smtp_port := fnd_profile.value('FND_SMTP_PORT');

    EXCEPTION
    WHEN OTHERS THEN
    fnd_file.put_line(fnd_file.log, 'Error In  get_email_details procedure- '||SQLERRM);

    END get_email_details;

--====================================================================================================================================
    PROCEDURE submit_email_prgm (p_cc_email IN VARCHAR2, p_request_id IN NUMBER
                    , po_email_req_id OUT NOCOPY NUMBER, po_err_msg OUT NOCOPY VARCHAR2)
    IS
    l_email_req_id NUMBER DEFAULT 0;
    l_from_email VARCHAR2(150) DEFAULT 'noreply@gilead.com';
    l_to_email VARCHAR2(150);
    l_file_path VARCHAR2(250) DEFAULT '/usr/tmp';
    l_file_name VARCHAR2(150);
    l_message1 VARCHAR2(240);
    l_message2  VARCHAR2(240);
    l_subject VARCHAR2(240);
    l_rm_files_days VARCHAR2(10);

    l_full_path VARCHAR2(240);
    l_top_name VARCHAR2(50);

    cursor c_trx_details is
    select transaction_number from xxgil_ar_email_details where request_id = p_request_id;

    type l_trx_tab is table of c_trx_details%rowtype index by binary_integer;
    l_trx_tbl l_trx_tab;

    BEGIN
    NULL;
       BEGIN
            SELECT
            attribute1,
            attribute2,
            attribute3,
            attribute4
        INTO
            l_to_email,
            l_file_path,
            l_from_email,
            l_rm_files_days
        FROM
            fnd_lookup_values
        WHERE
            lookup_type = 'XXGIL_ROTC85_GR_INV_SOURCE'
            AND lookup_code = 'ORDER MANAGEMENT'
            AND language = 'US'
            AND SYSDATE BETWEEN nvl(start_date_active, SYSDATE - 1) AND nvl(end_date_active, SYSDATE + 1)
            AND enabled_flag = 'Y';
       EXCEPTION
       WHEN OTHERS THEN
       po_err_msg := 'Unable to get Receipent Email Address';
       l_to_email := null;
       RAISE;
       END;

       l_top_name := substr(l_file_path, 2, INSTR(l_file_path, '/', 1)-2);

IF l_top_name LIKE '%TOP' THEN

     BEGIN
     SELECT DISTINCT
    --variable_name   "TOP_NAME",
    value  INTO l_full_path
FROM
    fnd_env_context            fec,
    fnd_concurrent_processes   fcp,
    fnd_concurrent_requests    fcr
WHERE
    1 = 1
    AND fec.concurrent_process_id = fcp.concurrent_process_id
    AND fec.concurrent_process_id = fcr.controlling_manager
    AND fec.variable_name = l_top_name
    AND fcr.request_id = p_request_id;

     EXCEPTION
     WHEN OTHERS THEN
     --l_full_path := '/appltop/ERPPRD/apps/apps_st/appl/xxgil/12.0.0';
     po_err_msg := 'Error - Unable to derive Stage path';
     fnd_file.put_line(fnd_file.log, 'Error In Getting Full Path of Custom Top- '||SQLERRM);
     RAISE;
     NULL;
     END;

     l_file_path := l_full_path||substr(l_file_path, INSTR(l_file_path, '/', 1));
END IF;

    OPEN c_trx_details;
    FETCH c_trx_details BULK COLLECT INTO l_trx_tbl;
    CLOSE c_trx_details;

    FOR i IN 1..l_trx_tbl.COUNT LOOP

    l_subject := l_trx_tbl(i).transaction_number;
    l_file_name := l_trx_tbl(i).transaction_number||'.pdf';

    l_email_req_id :=   FND_REQUEST.SUBMIT_REQUEST( application => 'XXGIL',
			  program     => 'XXGIL_SENDMAIL_ATTACH_GR',
			  description => 'GIL Email Commercial Invoices -Greece To RetailLink',
			  start_time => NULL,
			  sub_request => FALSE,
			  argument1  => l_from_email,
			  argument2  => l_to_email,
              argument3  => l_file_path,
              argument4  => l_file_name,
              argument5  => l_message1,
              argument6  => l_message2,
              argument7  => l_subject,
              argument8  => p_cc_email,
              argument9  => l_rm_files_days
              );


      END LOOP;
   COMMIT;
   po_email_req_id := l_email_req_id;
   po_err_msg := NULL;
   EXCEPTION
   WHEN OTHERS THEN
   po_err_msg := 'Error In  submit_email_prgm procedure- '||SQLERRM;
   fnd_file.put_line(fnd_file.log, 'Error In  submit_email_prgm procedure- '||SQLERRM);

    END submit_email_prgm;

--====================================================================================================================================
        PROCEDURE populate_trx_details (
        p_request_id   IN   NUMBER,
        p_trx_number   IN   VARCHAR2,
        p_org_id       IN   NUMBER,
        p_org_name     IN   VARCHAR2
    ) IS
    BEGIN
        NULL;
        INSERT INTO xxgil_ar_email_details (
            request_id,
            transaction_number,
            org_id,
            org_name,
            last_updated_date,
            last_updated_by,
            last_update_login,
            creation_date,
            created_by
        ) VALUES (
            p_request_id,
            p_trx_number,
            p_org_id,
            mo_global.get_ou_name(p_org_id),
            SYSDATE,                          --last_updated_date
            g_user_id,    --last_updated_by
            userenv('SESSIONID'),            --last_updated_login
            SYSDATE,                          --creation_date
            g_user_id
        );

        COMMIT;
    EXCEPTION
        WHEN OTHERS THEN
            fnd_file.put_line(fnd_file.log, 'Error In  populate_trx_details procedure- ' || sqlerrm);
       END populate_trx_details;

--====================================================================================================================================
    PROCEDURE update_trans_attrib (
        p_trx_number IN VARCHAR2,
        p_request_id IN NUMBER,
        po_err_msg OUT NOCOPY VARCHAR2
    ) IS

    cursor c_trx_details is
    select transaction_number, org_id from xxgil_ar_email_details where request_id = p_request_id;

    type l_upd_tab is table of c_trx_details%rowtype index by binary_integer;
    l_upd_tbl l_upd_tab;

    BEGIN
        NULL;

        OPEN c_trx_details;
        FETCH c_trx_details BULK COLLECT INTO l_upd_tbl;
        CLOSE c_trx_details;

        FORALL k IN l_upd_tbl.FIRST..l_upd_tbL.LAST
        UPDATE ra_customer_trx_all
        SET
            attribute10 = 'Yes'
        WHERE
            trx_number = l_upd_tbl(k).transaction_number
            AND org_id = l_upd_tbl(k).org_id
            AND (attribute10 IS NULL OR attribute10 <> 'Yes');
            
    fnd_file.put_line(fnd_file.log, 'Number of Invoice dff updated- ' || SQL%ROWCOUNT);        

        COMMIT;
        po_err_msg := null;
    EXCEPTION
        WHEN OTHERS THEN
            po_err_msg := 'Error In  update_trans_attrib procedure- ' || sqlerrm;
            fnd_file.put_line(fnd_file.log, 'Error In  update_trans_attrib procedure- ' || sqlerrm);
    END update_trans_attrib;

--==================================================================================================================================
    FUNCTION get_trx_count (
        p_request_id IN NUMBER
    ) RETURN NUMBER IS
l_trx_ctn NUMBER DEFAULT 0;
    BEGIN
        SELECT
            COUNT(1)
        INTO l_trx_ctn
        FROM
            xxgil_ar_email_details
        WHERE
            request_id = p_request_id;

        RETURN l_trx_ctn;
    EXCEPTION
        WHEN OTHERS THEN
            RETURN 0;
    END get_trx_count;

--====================================================================================================================================
    PROCEDURE main (
        p_err_buff                  OUT   VARCHAR2,
        p_ret_code                  OUT   NUMBER,
        p_order_by                  IN    VARCHAR2,
        p_choice                    IN    VARCHAR2,
        p_print_email               IN    VARCHAR2 DEFAULT 'Email',
        p_cust_trx_class            IN    VARCHAR2,
        p_cust_trx_type_id          IN    NUMBER,
        p_trx_number_low            IN    VARCHAR2,
        p_trx_number_high           IN    VARCHAR2,
        p_dates_low                 IN    VARCHAR2,--DATE,
        p_dates_high                IN    VARCHAR2,--DATE,
        p_sales_order_low           IN    NUMBER,
        p_sales_order_high          IN    NUMBER,
        p_delivery_num_low          IN    VARCHAR2,
        p_delivery_num_high         IN    VARCHAR2,
        p_cust_po_num_low           IN    VARCHAR2,
        p_cust_po_num_high          IN    VARCHAR2,
        p_customer_class_code       IN    VARCHAR2,
        p_customer_id               IN    NUMBER,
        p_customer_num_id           IN    NUMBER,
        p_open_invoice              IN    VARCHAR2,
        p_check_for_taxyn           IN    VARCHAR2,
        p_tax_registration_number   IN    VARCHAR2,
        p_header_pages              IN    NUMBER,
        p_debug_flag                IN    VARCHAR2,
        p_message_level             IN    NUMBER,
        p_print_einvoices           IN    VARCHAR2,
        p_cc_email                  IN    VARCHAR2
    ) IS

    l_org_id NUMBER;
    l_print_req_id NUMBER DEFAULT 0;
    l_burst_req_id NUMBER DEFAULT 0;
    l_phase            VARCHAR2(50);
  l_status           VARCHAR2(50);
  l_dev_phase        VARCHAR2(50);
  l_dev_status       VARCHAR2(50);
  l_message          VARCHAR2(50);
  l_req_return_status BOOLEAN;
  l_email_request_id NUMBER DEFAULT 0;

l_user_excep EXCEPTION;
l_err_message VARCHAR2(2000) DEFAULT NULL;

    BEGIN
       -- g_org_id := mo_global.get_current_org_id;


     IF NVL(p_debug_flag, 'N') = 'Y' THEN
       fnd_file.put_line(fnd_file.log, '*********************Start Debug Log**********************');
       fnd_file.put_line(fnd_file.log, 'Operating Unit Id Value - '||g_org_id);
       fnd_file.put_line(fnd_file.log, 'Trx Date From: '||p_dates_low);
       fnd_file.put_line(fnd_file.log, 'Trx Date To: '||p_dates_high);
       fnd_file.put_line(fnd_file.log, 'Email Choice: '||p_choice);
       fnd_file.put_line(fnd_file.log, 'Print E-Invoice: '||p_print_einvoices);
     END IF;

     IF p_choice = 'SEL' THEN

      IF NVL(p_debug_flag, 'N') = 'Y' THEN
      fnd_file.put_line(fnd_file.log, 'Validating Input parameters for Choice: '||p_choice);
      END IF;

     l_err_message := CASE WHEN (p_trx_number_low IS NULL AND p_trx_number_high IS NULL) AND (p_sales_order_low IS NULL AND p_sales_order_high IS NULL)
                        THEN 'Please Enter any one combination of following Parameters (Transaction Number Low and Transaction Number High) OR
                                                                (Sales Order Low and Sales Order High) when Email Choise is SELECT'
                        ELSE null END;
     END IF;

     IF l_err_message IS NOT NULL THEN
     RAISE l_user_excep;
     END IF;

    --Initialize Apps to Submit CP's
        fnd_global.APPS_INITIALIZE(user_id => g_user_id,
                                    resp_id => g_resp_id,
                                    resp_appl_id => g_resp_appl_id);

    IF NVL(p_debug_flag, 'N') = 'Y' THEN
      fnd_file.put_line(fnd_file.log, 'Set Operating Unit for the Print Program: '||g_org_id);
    END IF;

      --Set Operating Unit for CP submission
        FND_REQUEST.SET_ORG_ID(g_org_id);

--Submit Greece Invoice Print Program
l_print_req_id :=   FND_REQUEST.SUBMIT_REQUEST( application => 'XXGIL',
			  program     => 'XXGILARCOMINVGR',
			  description => 'GIL Print Commercial Invoices -Greece For Email',
			  start_time => NULL,
			  sub_request => FALSE,
			  argument1  => p_order_by,
			  argument2  => p_choice,
  			  argument3  => p_print_email,
			  argument4  => p_cust_trx_class,
			  argument5  => p_cust_trx_type_id,
			  argument6  => p_trx_number_low,
			  argument7  => p_trx_number_high,
			  argument8  => p_dates_low,
			  argument9  => p_dates_high,
			  argument10 => p_sales_order_low,
			  argument11 => p_sales_order_high,
			  argument12 => p_delivery_num_low,
  			  argument13 => p_delivery_num_high,
			  argument14 => p_cust_po_num_low,
			  argument15 => p_cust_po_num_high,
			  argument16 => p_customer_class_code,
			  argument17 => p_customer_id,
			  argument18 => p_customer_num_id,
			  argument19 => p_open_invoice,
			  argument20 => p_check_for_taxyn,
              argument21 => p_tax_registration_number,
              argument22 => p_header_pages,
              argument23 => p_debug_flag,
              argument24 => p_message_level,
              argument25 => p_print_einvoices);

COMMIT;

    IF NVL(p_debug_flag, 'N') = 'Y' THEN
      fnd_file.put_line(fnd_file.log, 'After Submssion of Print Program: '||l_print_req_id);
    END IF;

    IF l_print_req_id = 0 THEN
    p_ret_code :=  1;
    fnd_file.put_line(fnd_file.log, 'Error: Submission of Print Program Un-Successful');
    ELSE
        IF NVL(p_debug_flag, 'N') = 'Y' THEN
        fnd_file.put_line(fnd_file.log, 'Starting Wait for request for Print Program: '||l_print_req_id);
        END IF;

    ---Wait for Print Request to Get Completed
    l_req_return_status :=
            fnd_concurrent.wait_for_request (request_id      => l_print_req_id
                                            ,INTERVAL        => 60 --interval Number of seconds to wait between checks
                                            ,max_wait        => 1800 --Maximum number of seconds to wait for the request completion
                                             -- out arguments
                                            ,phase           => l_phase
                                            ,STATUS          => l_status
                                            ,dev_phase       => l_dev_phase
                                            ,dev_status      => l_dev_status
                                            ,message         => l_message
                                            );

    END IF;

        IF NVL(p_debug_flag, 'N') = 'Y' THEN
        fnd_file.put_line(fnd_file.log, 'End Wait for request for Print Program: '||l_print_req_id);
        END IF;

    IF l_req_return_status AND l_status = 'Normal' THEN

        IF NVL(p_debug_flag, 'N') = 'Y' THEN
        fnd_file.put_line(fnd_file.log, 'Begin Submission of Burst Program');
        END IF;

        IF get_trx_count (p_request_id => l_print_req_id) > 0 THEN
        --Submit Bursting Program to Generate and Place Invoice PDFs in custom location
            l_burst_req_id := FND_REQUEST.SUBMIT_REQUEST( application => 'XDO',
                                                        program     => 'XDOBURSTREP',
                                                          description => 'GIL Burst Commercial Invoices -Greece For Email',
                                                          start_time => NULL,
                                                          sub_request => FALSE,
                                                          argument1  => 'Y',
                                                          argument2  => l_print_req_id,
                                                          argument3  => 'Y');
        ELSE
        fnd_file.put_line(fnd_file.log, 'No Eligible Transactions To Burst for Emailing');
        END IF;
           COMMIT;

            IF NVL(p_debug_flag, 'N') = 'Y' THEN
            fnd_file.put_line(fnd_file.log, 'End Submission of Burst Program: '||l_burst_req_id);
            END IF;

    END IF;

         IF l_burst_req_id = 0 THEN
            p_ret_code :=  1;
            fnd_file.put_line(fnd_file.log, 'Error: Submission of Bursting Program Un-Successful');
         ELSE

        IF NVL(p_debug_flag, 'N') = 'Y' THEN
        fnd_file.put_line(fnd_file.log, 'Starting Wait for request for Burst Program: '||l_burst_req_id);
        END IF;

         ---Wait for Burst Request to Get Completed
    l_req_return_status :=
            fnd_concurrent.wait_for_request (request_id      => l_burst_req_id
                                            ,INTERVAL        => 60 --interval Number of seconds to wait between checks
                                            ,max_wait        => 1800 --Maximum number of seconds to wait for the request completion
                                             -- out arguments
                                            ,phase           => l_phase
                                            ,STATUS          => l_status
                                            ,dev_phase       => l_dev_phase
                                            ,dev_status      => l_dev_status
                                            ,message         => l_message
                                            );
        END IF;

        IF NVL(p_debug_flag, 'N') = 'Y' THEN
        fnd_file.put_line(fnd_file.log, 'End Wait for request for Burst Program: '||l_burst_req_id);
        END IF;

        IF l_req_return_status AND l_status = 'Normal' THEN

        IF NVL(p_debug_flag, 'N') = 'Y' THEN
        fnd_file.put_line(fnd_file.log, 'Start Execution of Submit email Procedure');
        END IF;

        --Submit Email Program to Email the Generated Invoice Files
        submit_email_prgm (p_cc_email => p_cc_email, p_request_id => l_print_req_id,
                            po_email_req_id => l_email_request_id, po_err_msg => l_err_message);
        fnd_file.put_line(fnd_file.log, 'Successful Submission');
        END IF;

         IF l_err_message IS NOT NULL THEN
         RAISE l_user_excep;
         END IF;

        IF l_email_request_id = 0 THEN
        p_ret_code :=  1;
            fnd_file.put_line(fnd_file.log, 'Error: Submission of Email Program Un-Successful');
        ELSE
            IF NVL(p_debug_flag, 'N') = 'Y' THEN
            fnd_file.put_line(fnd_file.log, 'Starting Wait for request for Last Email Program: '||l_email_request_id);
            END IF;
        ---Wait for Email Requests to Get Completed
        l_req_return_status :=
            fnd_concurrent.wait_for_request (request_id      => l_email_request_id
                                            ,INTERVAL        => 30 --interval Number of seconds to wait between checks
                                            --,max_wait        => 1800 --Maximum number of seconds to wait for the request completion
											,max_wait        => 3600 -- For 2.0
                                             -- out arguments
                                            ,phase           => l_phase
                                            ,STATUS          => l_status
                                            ,dev_phase       => l_dev_phase
                                            ,dev_status      => l_dev_status
                                            ,message         => l_message
                                            );


            IF l_req_return_status AND l_status = 'Normal' THEN

                --IF p_choice = 'NEW' THEN
                IF NVL(p_debug_flag, 'N') = 'Y' THEN
                fnd_file.put_line(fnd_file.log, 'Start Execution of Update Trans Attrib Procedure');
                END IF;
            --Update the Transaction Attribute (Transaction Sent via Email) to  Yes
                update_trans_attrib (
                                    p_trx_number => NULL,
                                    p_request_id => l_print_req_id,
                                    po_err_msg => l_err_message
                                );
              --END IF;
            END IF;
        END IF;

        IF l_err_message IS NOT NULL THEN
         RAISE l_user_excep;
         END IF;


                IF NVL(p_debug_flag, 'N') = 'Y' THEN
                fnd_file.put_line(fnd_file.log, '*********************End Debug Log**********************');
                END IF;

       -- NULL;
    EXCEPTION
    WHEN l_user_excep THEN
    p_ret_code :=  2;
    fnd_file.put_line(fnd_file.log, 'User Defined Exception - '||l_err_message);
    WHEN OTHERS THEN
    p_ret_code :=  2;
    fnd_file.put_line(fnd_file.log, 'Error: Main Exception - '||SQLERRM);
    END main;

END xxgil_ar_inv_email_greece_pkg;
/
