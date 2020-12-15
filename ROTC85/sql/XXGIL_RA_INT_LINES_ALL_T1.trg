create or replace TRIGGER APPS.XXGIL_RA_INT_LINES_ALL_T1
  /********************************************************************************
  * (c) Copyright Gilead Sciences.                                                *
  *  All Rights Reserved                                                          *
  *                                                                               *
  * $ Header: xxgil_ra_int_lines_all_t1.sql         Author : Rishabh Naidu  $     *
  *                                                                               *
  *                     M O D I F I C A T I O N   L O G                           *
  *                                                                               *
  * Date        Name                Issue/CR  Version    Description              *
  * ----------- ------------------  --------  --------   ------------------------ *
  * 15-Apr-10   Rishabh Naidu       EOTC134      1.0     Initial Creation         *
  * 15-Feb-17   Ssaha01             CR1832       1.1     Added for CR1832         *
  * 11-Oct-18   Deekshit Yerra      NA           1.2     Retrofit changes for     *
  *                                                      12.2.6 upgrade			  *
  * 19-Jul-19	PPENUMARTI			NA			 1.3	 Retrofit changes for	  *
  *														 Gilead CT Integration	  *
  ********************************************************************************/
BEFORE INSERT OR UPDATE
--12.2.6 upgrade dyerra retrofit1 for v1.2
--ON AR.RA_INTERFACE_LINES_ALL
--12.2.6 upgrade dyerra retrofit1 for v1.2
ON RA_INTERFACE_LINES_ALL
FOR EACH ROW
DECLARE
   lv_type_txt   		VARCHAR2 (240);
   -- Addition start for CR1832
   lv_ind_cust          VARCHAR2 (240);
   lv_ind_cust_dea      VARCHAR2 (240);
   lv_ind_cust_add      VARCHAR2 (240);
   lv_transfer_flag     VARCHAR2 (10);
   -- Addition end for CR1832
BEGIN

   BEGIN
      SELECT type
        INTO lv_type_txt
        --12.2.6 upgrade dyerra retrofit2 for v1.2
        --FROM ar.ra_cust_trx_types_all
		--12.2.6 upgrade dyerra retrofit2 for v1.2
		FROM ra_cust_trx_types_all
       WHERE cust_trx_type_id = :NEW.cust_trx_type_id;
   EXCEPTION
      WHEN OTHERS
      THEN
         lv_type_txt := NULL;
   END;
   --Started modification for 1832
   BEGIN
     SELECT 'YES'
      INTO lv_transfer_flag
      FROM fnd_lookup_values
     WHERE lookup_type = 'XXGIL_EDI_180_TRANS_MAP'
       AND LANGUAGE = USERENV ('LANG')
       AND attribute5 = 'ORDER_TYPE'
       AND enabled_flag = 'Y'
       AND attribute6 = :NEW.org_id
       AND TRUNC (SYSDATE) BETWEEN TRUNC (NVL (start_date_active, SYSDATE))
                               AND TRUNC (NVL (end_date_active, SYSDATE));
   EXCEPTION
     WHEN OTHERS THEN
        lv_transfer_flag := 'No';
   END;
   --Ended modification for 1832
   IF lv_type_txt = 'CM' --AND :NEW.reason_code IS NOT NULL  -- added and commented for CR1832
   THEN

     IF :NEW.reason_code IS NOT NULL
     THEN
        :NEW.reason_code := 'N/A - NOT APPLICABLE';
     END IF;
      -- Addition start for CR1832
     IF UPPER(lv_transfer_flag) = 'YES'
        THEN
           BEGIN
              SELECT
              SUBSTR(oeh.attribute8, 1,  150),
              SUBSTR(oeh.attribute17, 1, 150),
              SUBSTR(oeh.attribute18, 1, 150)
              INTO
              lv_ind_cust,
              lv_ind_cust_dea,
              lv_ind_cust_add
              FROM oe_order_headers_all oeh
              WHERE 1=1
              AND oeh.order_number =:NEW.interface_line_attribute1;

           EXCEPTION
             WHEN OTHERS THEN
              lv_ind_cust    := NULL;
              lv_ind_cust_dea := NULL;
              lv_ind_cust_add := NULL;
           END;

          :NEW.HEADER_attribute_category := :NEW.org_id;
          :NEW.HEADER_ATTRIBUTE2 := lv_ind_cust_dea;
          :NEW.HEADER_ATTRIBUTE3 := lv_ind_cust_add;

     END IF;
     -- Addition end for CR1832
   END IF;
   
    /* Retrofit Changes for v1.3 Start */
   IF :NEW.interface_line_context ='ORDER ENTRY' and :NEW.header_attribute_category like 'CT%'
   THEN
     
	 :NEW.HEADER_attribute_category := NULL;
	 :NEW.HEADER_ATTRIBUTE1 := NULL;
     :NEW.HEADER_ATTRIBUTE2 := NULL;
	 :NEW.HEADER_ATTRIBUTE3 := NULL;
	 :NEW.HEADER_ATTRIBUTE4 := NULL;
	 :NEW.HEADER_ATTRIBUTE5 := NULL;
	 :NEW.HEADER_ATTRIBUTE6 := NULL;
	 :NEW.HEADER_ATTRIBUTE7 := NULL;
	 :NEW.HEADER_ATTRIBUTE8 := NULL;
	 :NEW.HEADER_ATTRIBUTE9 := NULL;
	 :NEW.HEADER_ATTRIBUTE10 := NULL;
	 :NEW.HEADER_ATTRIBUTE11:= NULL;
	 :NEW.HEADER_ATTRIBUTE12 := NULL;
	 :NEW.HEADER_ATTRIBUTE13 := NULL;
	 :NEW.HEADER_ATTRIBUTE14 := NULL;
	 :NEW.HEADER_ATTRIBUTE15:= NULL;
	 
   
   END IF;
   /* Retrofit Changes for v1.3 End */
END XXGIL_RA_INT_LINES_ALL_T1;
/