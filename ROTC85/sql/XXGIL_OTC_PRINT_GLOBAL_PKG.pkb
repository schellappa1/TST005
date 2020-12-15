create or replace PACKAGE BODY      Xxgil_Otc_Print_Global_Pkg
AS
/******************************************************************************************************
||                                                                                                    ||
||   Filename     : xxgil_otc_print_global_pkg.pkb                                                    ||
||                                                                                                    ||
||   Package Name : xxgil_otc_print_global_pkg                                                        ||
||                                                                                                    ||
||   Description  : Legal Entity Global Address Format                                                ||
||                                                                                                    ||
||   Usage/Purpose: Purpose to use the format across all CFD                                          ||
||                  documents                                                                         ||
||                                                                                                    ||
||   Ver    Date          Author                    Modification/History                              ||
||   1.0    17-MAY-10   Pratap Yeragudipati       Created                                             ||
||   1.1    05-APR-18   Ranajit Sinha             Added procedure xxgil_inv_pack_addr for CR1934 and  ||
||                                                appended APPS as owner for the base tables/views for ebr ||
||   1.2    22-Jan-2020	Ajaya Kumar			      Retrofit changes to display country name in customer   || 
||									              address  (SR#REQ0618111)	                          ||
||                                                                                                    ||
||   Special Notes : None                                                                             ||
||                                                                                                    ||
********************************************************************************************************/
/*******************************************************************************************************
||                                                                                                    ||
||   Filename       : xxgil_otc_print_global_pkg.pks                                                  ||
||                                                                                                    ||
||   Function Name : xxgil_uom_conversion_qty                                                         ||
||                                                                                                    ||
||   Description    : Get UOM conversion quantity                                                     ||
||                                                                                                    ||
||   Usage/Purpose  : Purpose to use the uom conversion quantity in shipping line quantity to         ||
||                    show correct quantity on invoice line when product split by lot  (CR #OTC-103)  ||
||                                                                                                    ||
||                                                                                                    ||
||   Ver    Date          Author                    Modification/History                              ||
||   1.0    15-Feb-11   Suresh Kandru               Created                                           ||
||                                                                                                    ||
||   Special Notes : This function is used in invoice lines query                                     ||
||                                                                                                    ||
*********************************************************************************************************/

FUNCTION get_tax_prof_reference_num(   P_PARTY_TYPE      IN VARCHAR2,
                                                              P_PARTY_ID          IN NUMBER
 ) RETURN VARCHAR2 IS
l_tax_reference VARCHAR2(100);

BEGIN
           SELECT zPT.REP_registration_number
              INTO l_tax_reference
            FROM apps.zx_party_tax_profile zpt
         WHERE  zpt.party_id=   P_PARTY_ID
              AND zpt.party_type_code= P_PARTY_TYPE
              AND ROWNUM=1;
RETURN  l_tax_reference;
EXCEPTION
 WHEN NO_DATA_FOUND THEN
 RETURN NULL;
 WHEN OTHERS THEN
 RETURN NULL;
END;



FUNCTION get_tax_reference_num(   P_PARTY_TYPE      IN VARCHAR2,
                                                      P_PARTY_ID          IN NUMBER
 ) RETURN VARCHAR2 IS
l_tax_reference VARCHAR2(100);

BEGIN
           SELECT zr.registration_number
                    INTO l_tax_reference
  FROM apps.zx_party_tax_profile zpt,
           apps.zx_registrations zr
  --,hz_cust_acct_sites_all hca,hz_cust_site_uses_all hcu
  WHERE  zr.party_tax_profile_id=zpt.party_tax_profile_id
      AND zpt.party_type_code= P_PARTY_TYPE
      AND ZR.default_registration_flag ='Y'
      AND sysdate BETWEEN nvl(ZR.EFFECTIVE_FROM,sysdate-1) AND nvl(ZR.EFFECTIVE_TO,sysdate+1)
      AND zpt.party_id=   P_PARTY_ID
      AND ROWNUM=1;
RETURN  l_tax_reference;
EXCEPTION
 WHEN NO_DATA_FOUND THEN
 RETURN NULL;
 WHEN OTHERS THEN
 RETURN NULL;
END;

FUNCTION   xxgil_get_tax_reference_num(   P_CUST_TYPE      IN VARCHAR2,
                                                                p_party_id     IN    NUMBER,
                                                                p_party_site_id IN NUMBER,
                                                                P_PARTY_SITE_TAX_REFERENCE IN VARCHAR2,
                                                                P_PARTY_TAX_REFERENCE  IN VARCHAR2
                                                    ) RETURN VARCHAR2 IS
l_tax_reference VARCHAR2(100);
 l_party_type VARCHAR2(50);

BEGIN



IF P_PARTY_SITE_TAX_REFERENCE IS NOT NULL
      THEN
     l_tax_reference:=P_PARTY_SITE_TAX_REFERENCE;
 ELSE
 --Party Site level REGISTRATION - Tax Registration Number
       l_party_type:='THIRD_PARTY_SITE';
       l_tax_reference:=get_tax_reference_num(   P_PARTY_TYPE  =>  l_party_type,
                                                                       P_PARTY_ID    =>p_party_site_id);

       IF l_tax_reference IS NULL THEN
          --Get Party Site Tax Profile
           l_tax_reference:=get_tax_prof_reference_num(   P_PARTY_TYPE  =>  l_party_type,
                                                                                   P_PARTY_ID    =>p_party_site_id);


             IF  l_tax_reference is null
                THEN
                --Get party level  registration Tax registration tax Reference Number
                         l_party_type:='THIRD_PARTY';
                          l_tax_reference:=get_tax_reference_num(   P_PARTY_TYPE  =>  l_party_type,
                                                                       P_PARTY_ID    =>p_party_id);


                          IF l_tax_reference IS NULL THEN
                               --Get Party level  Tax Profile level Registration number
                                 l_tax_reference:=get_tax_prof_reference_num(   P_PARTY_TYPE  =>  l_party_type,
                                                                                   P_PARTY_ID    =>p_party_id);


                                    --if Party level Tax reference Number is null then get Party Tax Reference
                                          IF  l_tax_reference is null
                                                THEN
                                               l_tax_reference:=P_PARTY_TAX_REFERENCE;
                                           END IF;

                          END if;

             END IF;
     END IF;

END IF;

RETURN  l_tax_reference;
EXCEPTION
 WHEN NO_DATA_FOUND THEN
 RETURN NULL;
 WHEN OTHERS THEN
 RETURN NULL;
END;

FUNCTION xxgil_uom_conversion_qty( p_inventory_item_id     IN    NUMBER,
                          p_uom_code             IN    VARCHAR2) RETURN NUMBER IS
lv_uom_qty  NUMBER;
BEGIN
  SELECT conversion_rate
  INTO lv_uom_qty
  FROM apps.mtl_uom_conversions
  WHERE inventory_item_id=p_inventory_item_id
  AND uom_code=p_uom_code;

 RETURN lv_uom_qty;

EXCEPTION
 WHEN NO_DATA_FOUND THEN
 RETURN NULL;
 WHEN OTHERS THEN
 RETURN NULL;
END xxgil_uom_conversion_qty;

/*****************************************************************************
||                                                                                                     ||
||   Filename       : xxgil_otc_print_global_pkg.pkb                                       ||
||                                                                                                      ||
||   Procedure Name : xxgil_address_format                                                       ||
||                                                                                                      ||
||   Description    : Legal Entity Address Format                                      ||
||                                                                                                      ||
||   Usage/Purpose  : Purpose to use the format across all CFD                    ||
||                    documents                                                              ||
||                                                                                                      ||
||   Ver    Date          Author                    Modification/History           ||
||   1.0    17-MAY-10   Pratap Yeragudipati       Created                        ||
||   1.1    13-OCT-10   Pooja Nainwal             Updated    Australia address format              ||
||   1.2    17-NOV-10   Suresh Kandru             Updated    Added full country name to AU and NZ. ||
||   1.3    02-DEC-10    Gopi Vemula               Updated     Modified IE address formats.     ||
||   1.4    10-JAN-11   Suresh Kandru             Updated    Added full country name to all countries in address format ||
||   1.5    25-JAN-11   Gopi  Vemula              Updated    Modified Switzerland(CH) address format. Defect # 3674
||   Special Notes : None                                                                       ||
||                                                                                                      ||
******************************************************************************/
/* Formatted on 2010/10/14 11:47 (Formatter Plus v4.8.7) */
/* Formatted on 2010/10/14 11:50 (Formatter Plus v4.8.7) */
PROCEDURE xxgil_address_format (
   p_country       IN       VARCHAR2,
   p_city          IN       VARCHAR2,
   p_state         IN       VARCHAR2,
   p_province      IN       VARCHAR2,
   p_county        IN       VARCHAR2,
   p_postal_code   IN       VARCHAR2,
   x_address5      OUT      VARCHAR2,
   x_address6      OUT      VARCHAR2,
   x_address7      OUT      VARCHAR2
)
IS
   lv_address5   VARCHAR2 (240);
   lv_address6   VARCHAR2 (240);
   lv_address7   VARCHAR2 (240);
   lv_country    VARCHAR2 (100);
BEGIN
   lv_address5 := NULL;
   lv_address6 := NULL;
   lv_address7 := NULL;
   BEGIN
     SELECT territory_short_name
     INTO lv_country
     FROM apps.FND_TERRITORIES_TL
     WHERE territory_code=p_country
     AND LANGUAGE='US';
   EXCEPTION
     WHEN NO_DATA_FOUND THEN
     lv_country :=NULL;
     WHEN OTHERS THEN
     lv_country :=NULL;
   END;

   --For the Country Itlay
   IF p_country = 'IT'
   THEN
      IF p_province IS NULL
      THEN
         lv_address5 := TRIM (p_postal_code || ' ' || p_city);
      ELSIF p_province IS NOT NULL
      THEN
         lv_address5 := TRIM (p_postal_code || ' ' || p_city || ' - ' || p_province);
      --ELSIF p_postal_code IS NULL THEN
       --lv_address5 := TRIM(p_city||' - '||p_province);
      END IF;

      lv_address6 := lv_country;
      x_address5 := lv_address5;
      x_address6 := lv_address6;
      x_address7 := NULL;
   --END IF;
   --For the Country Turkey
   ELSIF p_country = 'TR'
      THEN
         IF p_postal_code IS NOT NULL AND p_city IS NOT NULL
         THEN
            lv_address5 := TRIM (p_postal_code || ',' || p_city);
         ELSIF p_postal_code IS NULL
         THEN
            lv_address5 := TRIM (p_city);
         END IF;

         lv_address6 := lv_country;
         x_address5 := lv_address5;
         x_address6 := lv_address6;
         x_address7 := NULL;
   --For the country France ( PF- French Polynesia)
   ELSIF p_country IN ('FR', 'PF')
      THEN
         IF p_postal_code IS NOT NULL AND p_city IS NOT NULL
         THEN
            lv_address5 := TRIM (p_postal_code || ' ' || p_city);
         ELSIF p_postal_code IS NULL
         THEN
            lv_address5 := TRIM (p_city);
         END IF;

            lv_address6 := lv_country;
            x_address5 := lv_address5;
            x_address6 := lv_address6;
            x_address7 := NULL;

   --For the country GERMANY
   ELSIF p_country = 'DE'
      THEN
        IF p_postal_code IS NOT NULL AND p_city IS NOT NULL
        THEN
           lv_address5 := TRIM (p_postal_code || ' ' || p_city);
        ELSIF p_postal_code IS NULL
           THEN
              lv_address5 := TRIM (p_city);
        END IF;
        -- lv_address6 := p_country;
           x_address5 := lv_address5;
           x_address6 := NULL;
           x_address7 := NULL;
   --For the Country Ireland
   ELSIF p_country = 'IE'
       THEN
           lv_address5 := TRIM (NVL (p_city, p_county));
           x_address5 := lv_address5;
           --x_address6 := NULL;
           --x_address7 := NULL;
          IF p_postal_code IS NOT NULL
             THEN
          x_address6 := p_postal_code;
          END IF;

           IF p_country IS NOT NULL
           THEN
           x_address7 :=lv_country;
           END IF;

      --    END IF;

    --For the Country Switzerland, Austria
   ELSIF p_country IN ('CH', 'AT')
     THEN

        IF UPPER(lv_country) = 'SWITZERLAND'
         THEN
             lv_country:='Schweiz';
        END IF;     --GV 01/25/10


          IF p_postal_code IS NULL AND p_city IS NULL
             THEN
                lv_address5 := TRIM (lv_country);
          ELSIF p_postal_code IS NOT NULL OR p_city IS NOT NULL
          THEN

          --IF P_COUNTRY IN ('AT') THEN
             --Defect 5358 Priska wants both Switzerland and Austria same format
           IF p_country IN ('CH', 'AT') THEN
          /*   lv_address5 :=
                           TRIM (   lv_country
                                 || ' - '
                                 || p_postal_code
                                 || ' '
                                 || p_city
                                );
           ELSE*/
           --GV Defect 3674 01/25/10
            lv_address5 :=
                           TRIM (
                                  p_postal_code
                                 || ' '
                                 || p_city
                                 ||CHR(13)
                                 ||lv_country
                                );                                --}

           END IF;
           END IF;
              x_address5 := lv_address5;
              x_address6 := NULL;
              x_address7 := NULL;
   --   For the Country Belgium and Netherlands
   ELSIF p_country IN ('NL', 'BE')
      THEN
         IF p_postal_code IS NOT NULL AND p_city IS NOT NULL
         THEN
            lv_address5 :=TRIM (p_postal_code || ' ' || p_city);
         ELSIF p_postal_code IS NULL
         THEN
            lv_address5 := TRIM (p_city);
         ELSIF p_city IS NULL
         THEN
            lv_address5 := TRIM (p_postal_code);
         END IF;
            lv_address6 := TRIM (lv_country);
            x_address5 := lv_address5;
            x_address6 := lv_address6;
            x_address7 := NULL;
    --    For the Country Spain
   ELSIF p_country = 'ES'
      THEN
         lv_address5 := TRIM (p_city);
      IF  p_postal_code IS NOT NULL AND p_province IS NOT NULL
      THEN
         lv_address6 := TRIM (p_postal_code || ',' || p_province);
      ELSIF p_postal_code IS NULL
      THEN
         lv_address6 := TRIM (p_province);
      ELSIF p_province IS NULL
      THEN
         lv_address6 := TRIM (p_postal_code);
      END IF;
      x_address5 := lv_address5;
      x_address6 := lv_address6;
      x_address7 := NULL;
    --    For the Country Portugal
   ELSIF p_country = 'PT'
      THEN
      IF p_postal_code IS NOT NULL AND p_city IS NOT NULL
      THEN
         lv_address5 := TRIM (p_postal_code || ',' || p_city);
      ELSIF p_postal_code IS NULL
      THEN
         lv_address5 := TRIM (p_city);
      ELSIF p_city IS NULL
      THEN
         lv_address5 := TRIM (p_postal_code);
      END IF;
      x_address5 := lv_address5;
      x_address6 := NULL;
      x_address7 := NULL;
--    For the Country UK
  ELSIF p_country = 'GB'
     THEN
         lv_address5 := p_city;
         lv_address6 := p_county;
         lv_address7 := p_postal_code;
         x_address5 := lv_address5;
         x_address6 := lv_address6;
         x_address7 := lv_address7;
  ELSIF  p_country = 'US'
      THEN
        IF p_city IS NOT NULL AND p_state IS NOT NULL
        THEN
           lv_address5 := p_city || ',' ||p_state;
        ELSE
           lv_address5 := p_city;
        END IF;
        lv_address6 := p_postal_code;
        lv_address7 := lv_country;
        x_address5 := lv_address5;
        x_address6 := lv_address6;
        x_address7 := lv_address7;

 --    For the Country US,Denmark,Norway,Sweden,Finland
  ELSIF p_country IN('US', 'DK', 'NO', 'FI', 'SE')
     THEN
        lv_address5 := p_city;
        lv_address6 := p_postal_code;
        lv_address7 := lv_country;
        x_address5 := lv_address5;
        x_address6 := lv_address6;
        x_address7 := lv_address7;
  --   For the Country Australia and New Zealand
  ELSIF p_country IN ('AU','NZ')
    THEN
       IF p_state IS NOT NULL
       THEN
          lv_address5 :=
             TRIM (   p_city
                   || ' '
                   || p_state
                   || ' '
                   || p_postal_code
                  );
       ELSE
          lv_address5 :=
             TRIM (   p_city
                   || ' '
                   || p_postal_code
                  );
       END IF;

     --  lv_address6 := TRIM (p_country);
       lv_address6 := lv_country;
       x_address5 := lv_address5;
       x_address6 := lv_address6;
       x_address7 := NULL;
  ELSE
       SELECT TRIM (   p_city
                    || ' '
                    || DECODE (p_county,
                               '', '',
                               p_county || ' '
                              )
                    || DECODE (p_state,
                               '', '',
                               p_state || ' '
                              )
                    || DECODE (p_province,
                               '', '',
                                  p_province
                               || ' '
                              )
                    || p_postal_code
                   )
       INTO lv_address5
       FROM DUAL;
      -- lv_address6 := TRIM (p_country);
       lv_address6 := lv_country;
       x_address5 := lv_address5;
       x_address6 := lv_address6;
       x_address7 := NULL;
    END IF;

EXCEPTION
   WHEN OTHERS
   THEN
      NULL;
END xxgil_address_format;


/*****************************************************************************
||                                                                                                     ||
||   Filename       : xxgil_otc_print_global_pkg.pkb                                       ||
||                                                                                                      ||
||   Procedure Name : xxgil_Pack_address_format                                                       ||
||                                                                                                      ||
||   Description    : Address Format  for  Custom package Slip Report                                  ||
||                                                                                                      ||
||   Usage/Purpose  : Purpose to use the format across Custom package Slip Report                    ||
||                                                                                                      ||
||   Ver    Date          Author                    Modification/History           ||
||   1.0    15-Dec-10   Pooja Nainwal       Created                        ||
||   1.1    22-Jan-2020	Ajaya Kumar 		Retrofit changes to display country in customer address for REQ0618111. ||
||   Special Notes : None                                                                       ||
||                                                                                                      ||
******************************************************************************/
PROCEDURE xxgil_Pack_address_format (
   p_country       IN       VARCHAR2,
   p_city          IN       VARCHAR2,
   p_state         IN       VARCHAR2,
   p_province      IN       VARCHAR2,
   p_county        IN       VARCHAR2,
   p_postal_code   IN       VARCHAR2,
   x_address5      OUT      VARCHAR2,
   x_address6      OUT      VARCHAR2,
   x_address7      OUT      VARCHAR2
)
IS
   lv_address5   VARCHAR2 (240);
   lv_address6   VARCHAR2 (240);
   lv_address7   VARCHAR2 (240);
   lv_country    VARCHAR2 (100);
BEGIN
   lv_address5 := NULL;
   lv_address6 := NULL;
   lv_address7 := NULL;
   BEGIN
     SELECT territory_short_name
     INTO lv_country
     FROM apps.FND_TERRITORIES_TL
     WHERE territory_code=p_country
     AND LANGUAGE='US';
   EXCEPTION
     WHEN NO_DATA_FOUND THEN
     lv_country :=NULL;
     WHEN OTHERS THEN
     lv_country :=NULL;
   END;

   --For the Country Itlay
   IF p_country = 'IT'
   THEN
      IF p_province IS NULL
      THEN
         lv_address5 := TRIM (p_postal_code || ' ' || p_city);
      ELSIF p_province IS NOT NULL
      THEN
         lv_address5 := TRIM (p_postal_code || ' ' || p_city || ' - ' || p_province);
      --ELSIF p_postal_code IS NULL THEN
       --lv_address5 := TRIM(p_city||' - '||p_province);
      END IF;

      lv_address6 := lv_country;
      x_address5 := lv_address5;
      x_address6 := lv_address6;
      x_address7 := NULL;
   --END IF;
   --For the Country Turkey
   ELSIF p_country = 'TR'
      THEN
         IF p_postal_code IS NOT NULL AND p_city IS NOT NULL
         THEN
            lv_address5 := TRIM (p_postal_code || ',' || p_city);
         ELSIF p_postal_code IS NULL
         THEN
            lv_address5 := TRIM (p_city);
         END IF;

         lv_address6 := lv_country;
         x_address5 := lv_address5;
         x_address6 := lv_address6;
         x_address7 := NULL;
   --For the country France ( PF- French Polynesia)
   ELSIF p_country IN ('FR', 'PF')
      THEN
         IF p_postal_code IS NOT NULL AND p_city IS NOT NULL
         THEN
            lv_address5 := TRIM (p_postal_code || ' ' || p_city);
         ELSIF p_postal_code IS NULL
         THEN
            lv_address5 := TRIM (p_city);
         END IF;

            lv_address6 := lv_country;
            x_address5 := lv_address5;
            x_address6 := lv_address6;
            x_address7 := NULL;

   --For the country GERMANY
   ELSIF p_country = 'DE'
      THEN
        IF p_postal_code IS NOT NULL AND p_city IS NOT NULL
        THEN
           lv_address5 := TRIM (p_postal_code || ' ' || p_city);
        ELSIF p_postal_code IS NULL
           THEN
              lv_address5 := TRIM (p_city);
        END IF;
		
		 lv_address6 := lv_country;--Added by Ajay for REQ0618111
         x_address5 := lv_address5;
		 --x_address6 := NULL;
         x_address6 := lv_address6;--Added by Ajay for REQ0618111
         x_address7 := NULL;
		
   --For the Country Ireland
   ELSIF p_country = 'IE'
       THEN
           lv_address5 := TRIM (NVL (p_city, p_county));
           x_address5 := lv_address5;
           --x_address6 := NULL;
           --x_address7 := NULL;
          IF p_postal_code IS NOT NULL
             THEN
          x_address6 := p_postal_code;
          END IF;

           IF p_country IS NOT NULL
           THEN
           x_address7 :=lv_country;
           END IF;

      --    END IF;

    --For the Country Switzerland, Austria
   ELSIF p_country IN ('CH', 'AT')
     THEN
          IF p_postal_code IS NULL AND p_city IS NULL
             THEN
                lv_address5 := TRIM (lv_country);
          ELSIF p_postal_code IS NOT NULL OR p_city IS NOT NULL
          THEN
             lv_address5 :=
                           TRIM (   lv_country
                                 || ' - '
                                 || p_postal_code
                                 || ' '
                                 || p_city
                                );
           END IF;
              x_address5 := lv_address5;
              x_address6 := NULL;
              x_address7 := NULL;
   --   For the Country Belgium and Netherlands
   ELSIF p_country IN ('NL', 'BE')
      THEN
         IF p_postal_code IS NOT NULL AND p_city IS NOT NULL
         THEN
            lv_address5 :=TRIM (p_postal_code || ' ' || p_city);
         ELSIF p_postal_code IS NULL
         THEN
            lv_address5 := TRIM (p_city);
         ELSIF p_city IS NULL
         THEN
            lv_address5 := TRIM (p_postal_code);
         END IF;
            lv_address6 := TRIM (lv_country);
            x_address5 := lv_address5;
            x_address6 := lv_address6;
            x_address7 := NULL;
    --    For the Country Spain
   ELSIF p_country = 'ES'
     THEN
         lv_address5 := TRIM (p_city);
      IF  p_postal_code IS NOT NULL AND p_province IS NOT NULL
      THEN
         lv_address6 := TRIM (p_postal_code || ',' || p_province || ',' || lv_country); --Modified by Ajay for REQ0618111
      ELSIF p_postal_code IS NULL
      THEN
         lv_address6 := TRIM (p_province || ',' || lv_country);--Modified by Ajay for REQ0618111
      ELSIF p_province IS NULL
      THEN
         lv_address6 := TRIM (p_postal_code || ',' || lv_country);--Modified by Ajay for REQ0618111
      ELSE 		 
		 lv_address6 := TRIM (lv_country);--Added by Ajay for REQ0618111
      END IF;
      x_address5 := lv_address5;
      x_address6 := lv_address6;
      --x_address7 := TRIM (lv_country);--Modified by Ajay for REQ0618111
	  x_address7 := NULL;
    --    For the Country Portugal
   ELSIF p_country = 'PT'
      THEN
      IF p_postal_code IS NOT NULL AND p_city IS NOT NULL
      THEN
         lv_address5 := TRIM (p_postal_code || ',' || p_city);
      ELSIF p_postal_code IS NULL
      THEN
         lv_address5 := TRIM (p_city);
      ELSIF p_city IS NULL
      THEN
         lv_address5 := TRIM (p_postal_code);
      END IF;
	  lv_address6 := lv_country;--Added by Ajay for REQ0618111
      x_address5 := lv_address5;
	  --x_address6 := NULL;
      x_address6 := lv_address6;--Added by Ajay for REQ0618111
      x_address7 := NULL;
--    For the Country UK
  ELSIF p_country = 'GB'
     THEN
         --Added by Ajay for REQ0618111
	    IF p_postal_code IS NOT NULL AND p_city IS NOT NULL
      THEN
         lv_address5 := TRIM (p_city || ',' || p_postal_code);
      ELSIF p_postal_code IS NULL
      THEN
         lv_address5 := TRIM (p_city);
      ELSIF p_city IS NULL
      THEN
         lv_address5 := TRIM (p_postal_code);
      END IF; 
	 
         --lv_address5 := p_city;
		 -- End of changes for REQ0618111 
		 
         lv_address6 := p_county;
         --lv_address7 := p_postal_code;
		 lv_address7 := lv_country;--Added by Ajay for REQ0618111
         x_address5 := lv_address5;
         x_address6 := lv_address6;
         x_address7 := lv_address7;
  ELSIF  p_country = 'US'
      THEN
        IF p_city IS NOT NULL AND p_state IS NOT NULL
        THEN
           lv_address5 := p_city || ',' ||p_state;
        ELSE
           lv_address5 := p_city;
        END IF;
        lv_address6 := p_postal_code;
        lv_address7 := lv_country;
        x_address5 := lv_address5;
        x_address6 := lv_address6;
        x_address7 := lv_address7;

 --    For the Country US,Denmark,Norway,Sweden,Finland
  ELSIF p_country IN('US', 'DK', 'NO', 'FI', 'SE')
     THEN
        lv_address5 := p_city;
        lv_address6 := p_postal_code;
        lv_address7 := lv_country;
        x_address5 := lv_address5;
        x_address6 := lv_address6;
        x_address7 := lv_address7;
  --   For the Country Australia and New Zealand
  ELSIF p_country IN ('AU','NZ')
    THEN
       IF p_state IS NOT NULL
       THEN
          lv_address5 :=
             TRIM (   p_city
                   || ' '
                   || p_state
                   || ' '
                   || p_postal_code
                  );
       ELSE
          lv_address5 :=
             TRIM (   p_city
                   || ' '
                   || p_postal_code
                  );
       END IF;

     --  lv_address6 := TRIM (lv_country);
       lv_address6 := lv_country;
       x_address5 := lv_address5;
       x_address6 := lv_address6;
       x_address7 := NULL;
  ELSE
       SELECT TRIM (   p_city
                    || ' '
                    || DECODE (p_county,
                               '', '',
                               p_county || ' '
                              )
                    || DECODE (p_state,
                               '', '',
                               p_state || ' '
                              )
                    || DECODE (p_province,
                               '', '',
                                  p_province
                               || ' '
                              )
                    || p_postal_code
                   )
       INTO lv_address5
       FROM DUAL;
      -- lv_address6 := TRIM (lv_country);
       lv_address6 := lv_country;
       x_address5 := lv_address5;
       x_address6 := lv_address6;
       x_address7 := NULL;
    END IF;

EXCEPTION
   WHEN OTHERS
   THEN
      NULL;
END xxgil_Pack_address_format;

/*****************************************************************************
||                                                                                                     ||
||   Filename       : xxgil_otc_print_global_pkg.pkb                                       ||
||                                                                                                      ||
||   Procedure Name : xxgil_le_address                                                       ||
||                                                                                                      ||
||   Description    : Legal Entity Address Format                                      ||
||                                                                                                      ||
||   Usage/Purpose  : Purpose to use the format across all CFD                    ||
||                    documents                                                              ||
||                                                                                                      ||
||   Ver    Date          Author                    Modification/History           ||
||   1.0    17-MAY-10   Pratap Yeragudipati       Created                          ||
||   1.1    31-AUG-10   Suresh Kandru            Added primary flag condition to get primary phone number
||   1.2    16-SEP-10   Suresh Kandru            Retrieve LE addresses from lookup XXGIL_OTC_LE_ADDRESS for pick and pack slip reports.
||   Special Notes : None                                                                     ||
||                                                                                                      ||
******************************************************************************/
PROCEDURE xxgil_le_address(p_org_id                    IN    NUMBER,
                                    x_party_name                 OUT     VARCHAR2,
                                    x_tax_registration_num     OUT     VARCHAR2,
                                    x_address1                    OUT      VARCHAR2,
                                    x_address2                    OUT      VARCHAR2,
                                    x_address3                    OUT      VARCHAR2,
                                    x_address4                    OUT      VARCHAR2,
                                    x_address5                    OUT      VARCHAR2,
                                    x_address6                    OUT      VARCHAR2,
                                    x_address7                    OUT      VARCHAR2,
                                    x_city                        OUT     VARCHAR2,
                                    x_county                    OUT      VARCHAR2,
                                    x_state                   OUT     VARCHAR2,
                                    x_province                   OUT    VARCHAR2,
                                    x_postal_code                 OUT      VARCHAR2,
                                    x_country                   OUT     VARCHAR2,
                                    x_phone                      OUT     VARCHAR2,
                                    x_fax                      OUT     VARCHAR2,
                                    x_return_msg                 OUT     VARCHAR2)
IS
CURSOR cur_le_address
    IS
SELECT glcd.object_name
      ,hp.party_name
      ,hp.party_id
      ,hp.address1
      ,hp.address2
      ,hp.address3
      ,hp.address4
      ,hp.city
      ,hp.postal_code
      ,hp.state
      ,hp.province
      ,hp.county
      ,hp.country
      ,hp.tax_reference
 FROM apps.gl_ledger_config_details glcd
     ,apps.xle_firstparty_information_v xfi
     ,apps.hz_parties hp
     ,apps.hr_operating_units hou
WHERE object_type_code                                     = 'LEGAL_ENTITY'
  AND xfi.legal_entity_id                                 = glcd.object_id
  AND hp.party_id                                         = xfi.party_id
  AND TO_NUMBER(hou.DEFAULT_LEGAL_CONTEXT_ID)     = GLCD.object_Id
  AND HP.STATUS                                             ='A'
  AND hou.organization_id                                 = p_org_id
ORDER BY HP.PARTY_NAME;
CURSOR cur_contact_pts (p_party_id IN NUMBER)
    IS
SELECT hcp.phone_area_code
      ,hcp.phone_country_code
      ,hcp.phone_number
      ,hcp.phone_extension
      ,hcp.raw_phone_number
      ,hcp.phone_line_type
  FROM apps.hz_contact_points hcp
 WHERE owner_table_name     = 'HZ_PARTIES'
   AND owner_table_id         = p_party_id
   AND primary_flag = 'Y'
   AND phone_line_type         IN ('FAX','GEN')
   AND contact_point_type     = 'PHONE';
lv_address5_txt  VARCHAR2(240);
lv_address6_txt  VARCHAR2(240);
lv_address7_txt  VARCHAR2(240);
BEGIN
  /*  FOR lcr in cur_le_address LOOP
        FOR lcr_contact IN cur_contact_pts(lcr.party_id) LOOP
        IF lcr_contact.phone_line_Type = 'GEN' THEN
            x_phone := lcr_contact.phone_country_code||' '||lcr_contact.phone_area_code||' '||lcr_contact.phone_number;
        END IF;
        IF lcr_contact.phone_line_Type = 'FAX' THEN
            x_fax := lcr_contact.phone_country_code||' '||lcr_contact.phone_area_code||' '||lcr_contact.phone_number;
        END IF;
        END LOOP;
        x_party_name                 := lcr.object_name;
        x_tax_registration_num     := lcr.tax_reference;
        x_address1                 := lcr.address1;
        x_address2                     := lcr.address2;
        x_address3                    := lcr.address3;
        x_address4                  := lcr.address4;
        --Call the Package to populate address5, address6, address7
        XXGIL_ADDRESS_FORMAT(lcr.country,lcr.city,lcr.state,lcr.province,lcr.county,lcr.postal_code,lv_address5_txt,lv_address6_txt,lv_address7_txt);
        x_address5                     := lv_address5_txt;
        x_address6                     := lv_address6_txt;
        x_address7                     := lv_address7_txt;
        x_city                       := lcr.city;
        x_county                   := lcr.county;
        x_state                   := lcr.state;
        x_province                 := lcr.province;
        x_postal_code                 := lcr.postal_code;
        x_country                   := lcr.country;
    END LOOP;*/


-- Logic added for all LE addresses retrieve from XXGIL_OTC_LE_ADDRESS
  IF p_org_id IS NOT NULL THEN
    SELECT NAME
       INTO x_country
      FROM apps.hr_operating_units
      WHERE organization_id = p_org_id;
  END IF;
BEGIN
  SELECT ATTRIBUTE1,
         ATTRIBUTE2,
     ATTRIBUTE3,
     ATTRIBUTE4,
     ATTRIBUTE5,
     ATTRIBUTE6,
     ATTRIBUTE7,
     ATTRIBUTE8,
     ATTRIBUTE9,
     ATTRIBUTE10,
     ATTRIBUTE11
   INTO  x_party_name,
     x_address1,
     x_address2,
     x_address3,
     x_address4,
     x_address5,
     x_address6,
     x_address7,
         x_phone,
         x_fax,
     x_tax_registration_num
    FROM apps.FND_LOOKUP_VALUES
    WHERE LOOKUP_TYPE ='XXGIL_OTC_LE_ADDRESS'
    AND ATTRIBUTE_CATEGORY ='XXGIL_OTC_LE_ADDRESS'
    AND LANGUAGE='US'
    AND LOOKUP_CODE=x_country;

 EXCEPTION
 WHEN NO_DATA_FOUND THEN
 x_address1 := NULL;
 WHEN OTHERS THEN
 x_address1 := NULL;
 END;

 IF  x_address1  IS NULL
 THEN

  FOR lcr IN cur_le_address LOOP
        FOR lcr_contact IN cur_contact_pts(lcr.party_id) LOOP
        IF lcr_contact.phone_line_Type = 'GEN' THEN
            x_phone := lcr_contact.phone_country_code||' '||lcr_contact.phone_area_code||' '||lcr_contact.phone_number;
        END IF;
        IF lcr_contact.phone_line_Type = 'FAX' THEN
            x_fax := lcr_contact.phone_country_code||' '||lcr_contact.phone_area_code||' '||lcr_contact.phone_number;
        END IF;
        END LOOP;
        x_party_name                 := lcr.object_name;
        x_tax_registration_num     := lcr.tax_reference;
        x_address1                 := lcr.address1;
        x_address2                     := lcr.address2;
        x_address3                    := lcr.address3;
        x_address4                  := lcr.address4;
        --Call the Package to populate address5, address6, address7
        XXGIL_ADDRESS_FORMAT(lcr.country,lcr.city,lcr.state,lcr.province,lcr.county,lcr.postal_code,lv_address5_txt,lv_address6_txt,lv_address7_txt);
        x_address5                     := lv_address5_txt;
        x_address6                     := lv_address6_txt;
        x_address7                     := lv_address7_txt;
        x_city                       := lcr.city;
        x_county                   := lcr.county;
        x_state                   := lcr.state;
        x_province                 := lcr.province;
        x_postal_code                 := lcr.postal_code;
        x_country                   := lcr.country;
    END LOOP;

 END IF;

EXCEPTION
   WHEN OTHERS THEN
        x_return_msg := 'Unexpected Error in Procedure xxgil_le_address :'||SQLERRM;
END xxgil_le_address;
/**************************************************************************************************|                                                                                                     ||
||   Filename       : xxgil_otc_print_global_pkg.pkb                                               ||
||                                                                                                 ||
||   Procedure Name : xxgil_le_pack_address                                                        ||
||                                                                                                 ||
||   Description    : Legal Entity Address Format  for package slip report                         ||
||                                                                                                 ||
||   Usage/Purpose  : Purpose to use the format across all CFD                                     ||
||                    documents                                                                    ||
||                                                                                                 ||
||   Ver    Date          Author               Modification/History                                ||
||   1.0    17-MAY-10   Pooja Nainwal          Created                                             ||
||   Special Notes : None                      Retrieve LE addresses from                          ||                      ||
||                                             lookup XXGIL_OTC_LE_ADDRESS for pack slip report    ||
****************************************************************************************************/
PROCEDURE xxgil_le_pack_address(p_org_id                    IN    NUMBER,
                                    x_party_name                 OUT     VARCHAR2,
                                    x_tax_registration_num     OUT     VARCHAR2,
                                    x_address1                    OUT      VARCHAR2,
                                    x_address2                    OUT      VARCHAR2,
                                    x_address3                    OUT      VARCHAR2,
                                    x_address4                    OUT      VARCHAR2,
                                    x_address5                    OUT      VARCHAR2,
                                    x_address6                    OUT      VARCHAR2,
                                    x_address7                    OUT      VARCHAR2,
                                    x_city                        OUT     VARCHAR2,
                                    x_county                    OUT      VARCHAR2,
                                    x_state                   OUT     VARCHAR2,
                                    x_province                   OUT    VARCHAR2,
                                    x_postal_code                 OUT      VARCHAR2,
                                    x_country                   OUT     VARCHAR2,
                                    x_phone                      OUT     VARCHAR2,
                                    x_fax                      OUT     VARCHAR2,
                                    x_return_msg                 OUT     VARCHAR2)
IS
CURSOR cur_le_pack_address
    IS
SELECT glcd.object_name
      ,hp.party_name
      ,hp.party_id
      ,hp.address1
      ,hp.address2
      ,hp.address3
      ,hp.address4
      ,hp.city
      ,hp.postal_code
      ,hp.state
      ,hp.province
      ,hp.county
      ,hp.country
      ,hp.tax_reference
 FROM apps.gl_ledger_config_details glcd
     ,apps.xle_firstparty_information_v xfi
     ,apps.hz_parties hp
     ,apps.hr_operating_units hou
WHERE object_type_code                                     = 'LEGAL_ENTITY'
  AND xfi.legal_entity_id                                 = glcd.object_id
  AND hp.party_id                                         = xfi.party_id
  AND TO_NUMBER(hou.DEFAULT_LEGAL_CONTEXT_ID)     = GLCD.object_Id
  AND HP.STATUS                                             ='A'
  AND hou.organization_id                                 = p_org_id
ORDER BY HP.PARTY_NAME;
CURSOR cur_contact_pts (p_party_id IN NUMBER)
    IS
SELECT hcp.phone_area_code
      ,hcp.phone_country_code
      ,hcp.phone_number
      ,hcp.phone_extension
      ,hcp.raw_phone_number
      ,hcp.phone_line_type
  FROM apps.hz_contact_points hcp
 WHERE owner_table_name     = 'HZ_PARTIES'
   AND owner_table_id         = p_party_id
   AND primary_flag = 'Y'
   AND phone_line_type         IN ('FAX','GEN')
   AND contact_point_type     = 'PHONE';
lv_address5_txt  VARCHAR2(240);
lv_address6_txt  VARCHAR2(240);
lv_address7_txt  VARCHAR2(240);
BEGIN
-- Logic added for all LE addresses retrieve from XXGIL_OTC_LE_PACK_ADDRESS
  IF p_org_id IS NOT NULL THEN
    SELECT NAME
       INTO x_country
      FROM hr_operating_units
      WHERE organization_id = p_org_id;
  END IF;
BEGIN
  SELECT ATTRIBUTE1,
         ATTRIBUTE2,
     ATTRIBUTE3,
     ATTRIBUTE4,
     ATTRIBUTE5,
     ATTRIBUTE6,
     ATTRIBUTE7,
     ATTRIBUTE8,
     ATTRIBUTE9,
     ATTRIBUTE10,
     ATTRIBUTE11
   INTO  x_party_name,
     x_address1,
     x_address2,
     x_address3,
     x_address4,
     x_address5,
     x_address6,
     x_address7,
         x_phone,
         x_fax,
     x_tax_registration_num
    FROM apps.FND_LOOKUP_VALUES
    WHERE LOOKUP_TYPE ='XXGIL_OTC_LE_PACK_ADDRESS'
    AND ATTRIBUTE_CATEGORY ='XXGIL_OTC_LE_PACK_ADDRESS'
    AND LANGUAGE='US'
    AND LOOKUP_CODE=x_country;

 EXCEPTION
 WHEN NO_DATA_FOUND THEN
 x_address1 := NULL;
 WHEN OTHERS THEN
 x_address1 := NULL;
 END;

 IF  x_address1  IS NULL
 THEN

  FOR lcr IN cur_le_pack_address LOOP
        FOR lcr_contact IN cur_contact_pts(lcr.party_id) LOOP
        IF lcr_contact.phone_line_Type = 'GEN' THEN
            x_phone := lcr_contact.phone_country_code||' '||lcr_contact.phone_area_code||' '||lcr_contact.phone_number;
        END IF;
        IF lcr_contact.phone_line_Type = 'FAX' THEN
            x_fax := lcr_contact.phone_country_code||' '||lcr_contact.phone_area_code||' '||lcr_contact.phone_number;
        END IF;
        END LOOP;
        x_party_name                 := lcr.object_name;
        x_tax_registration_num     := lcr.tax_reference;
        x_address1                 := lcr.address1;
        x_address2                     := lcr.address2;
        x_address3                    := lcr.address3;
        x_address4                  := lcr.address4;
        --Call the Package to populate address5, address6, address7
        XXGIL_PACK_ADDRESS_FORMAT(lcr.country,lcr.city,lcr.state,lcr.province,lcr.county,lcr.postal_code,lv_address5_txt,lv_address6_txt,lv_address7_txt);
        x_address5                     := lv_address5_txt;
        x_address6                     := lv_address6_txt;
        x_address7                     := lv_address7_txt;
        x_city                       := lcr.city;
        x_county                   := lcr.county;
        x_state                   := lcr.state;
        x_province                 := lcr.province;
        x_postal_code                 := lcr.postal_code;
        x_country                   := lcr.country;
    END LOOP;

 END IF;
EXCEPTION
   WHEN OTHERS THEN
        x_return_msg := 'Unexpected Error in Procedure xxgil_le_pack_address :'||SQLERRM;
END xxgil_le_pack_address;
/*****************************************************************************
||                                                                                                     ||
||   Filename       : xxgil_otc_print_global_pkg.pkb                                       ||
||                                                                                                      ||
||   Procedure Name : xxgil_get_line_description                                           ||
||                                                                                                      ||
||   Description    : Get MLS line description                                                          ||
||                                                                                                      ||
||   Usage/Purpose  : Purpose to use the MLS product description across all CFD                    ||
||                    documents                                                                         ||
||                                                                                                      ||
||   Ver    Date          Author                    Modification/History           ||
||   1.0    30-AUG-10   Suresh Kandru                     Created                          ||
||   1.1    10-SEP-10   gv                            Modified reference from IT AIC to IT MOH       ||
||  1.2     16-FEB-11  gv                            Added reference for NORDIC countries(Denmark, Finland, Iceland, Norway and Sweden ) CR # 0TC-89
||  1.3     25-MAY-2011 Pooja Nainwal                Added C.N for Spain (ES) description for Defect#3948 ||
||  1.4     26-MAY-11  gv                               Modified reference from IT MOH to IT AIC       ||
||  1.5 	22-FEB-2016 Bharath Thampi				 Added reference for Japan						 ||
||                                                                                                   ||
||   Special Notes : None                                                                            ||
||                                                                                                   ||
******************************************************************************/
PROCEDURE xxgil_get_line_description(p_description               IN    VARCHAR2,
                                     p_country                   IN    VARCHAR2,
                                     p_product                   IN    VARCHAR2,
                                    x_description                OUT     VARCHAR2,
                                    x_return_msg                 OUT     VARCHAR2)
IS
l_description   VARCHAR2(240);
l_aic_code      VARCHAR2(240);
l_cn_code       VARCHAR2(240);
BEGIN
  BEGIN
   SELECT ATTRIBUTE1
   INTO l_description
   FROM apps.FND_LOOKUP_VALUES
   WHERE LOOKUP_TYPE = 'XXGIL_MLS_PRODUCT_DESCRIPTIONS'
   AND LANGUAGE = 'US'
   AND TAG = p_country
   AND DESCRIPTION = p_product;
   x_description :=  l_description;
  EXCEPTION
   WHEN NO_DATA_FOUND THEN
     x_description :=  p_description;
   WHEN OTHERS THEN
     x_return_msg := 'Unexpected Error in Procedure xxgil_le_address :'||SQLERRM;
  END;
IF p_country ='IT' THEN
BEGIN
  SELECT cross_reference
  INTO l_aic_code
  FROM apps.MTL_CROSS_REFERENCES_V
  WHERE cross_reference_type= 'IT AIC'--'IT MOH' --Fix defect # 6442
  AND INV_ITEM_CONCAT_SEGS=p_product;
  IF l_aic_code IS NOT NULL THEN
  x_description := x_description||'  AIC:'||l_aic_code;
 ELSE
   x_description := x_description;
 END IF;

EXCEPTION
  WHEN NO_DATA_FOUND THEN
   l_aic_code := NULL;
  WHEN OTHERS THEN
   l_aic_code := NULL;
END;
END IF;
IF p_country ='ES' THEN
BEGIN
  SELECT cross_reference
  INTO l_cn_code
  FROM apps.MTL_CROSS_REFERENCES_V
  WHERE cross_reference_type='ES CN'
  AND INV_ITEM_CONCAT_SEGS=p_product;
  x_description := x_description||' C.N.: '||l_cn_code;
EXCEPTION
  WHEN NO_DATA_FOUND THEN
   l_cn_code := NULL;
  WHEN OTHERS THEN
    l_cn_code := NULL;
END;
END IF;

/**gv 02/16 Added below code for CR OTC-82 **/
IF p_country  IN ('DK','FI','IS','NO','SE' ) THEN
BEGIN
  SELECT cross_reference
  INTO l_aic_code
  FROM apps.MTL_CROSS_REFERENCES_V
  WHERE cross_reference_type='DK_NATIONAL_ITEM'
  AND INV_ITEM_CONCAT_SEGS=p_product;
  IF l_aic_code IS NOT NULL THEN
  x_description := x_description||'  NIC:'||l_aic_code;
 ELSE
   x_description := x_description;
 END IF;

EXCEPTION
  WHEN NO_DATA_FOUND THEN
   l_aic_code := NULL;
  WHEN OTHERS THEN
   l_aic_code := NULL;
END;
END IF; --}

/** Begin of added by Bharath Thampi on 22-Feb-2016 for Japanese reference **/
IF p_country = 'JP' THEN
BEGIN
  SELECT cross_reference
  INTO l_aic_code
  FROM apps.MTL_CROSS_REFERENCES_V
  WHERE cross_reference_type='JP JD-NET'
  AND INV_ITEM_CONCAT_SEGS = p_product;
  IF l_aic_code IS NOT NULL THEN
  x_description := x_description||'  JD-NET:'||l_cn_code;
 ELSE
   x_description := x_description;
  END IF;
EXCEPTION
  WHEN NO_DATA_FOUND THEN
   l_aic_code := NULL;
  WHEN OTHERS THEN
   l_aic_code := NULL;
END;
 END IF;
/** End of added by Bharath Thampi on 22-Feb-2016 for Japanese reference **/

END xxgil_get_line_description;
/*****************************************************************************
||                                                                                                     ||
||   Filename       : xxgil_otc_print_global_pkg.pkb                                       ||
||                                                                                                      ||
||   Procedure Name : xxgil_get_mls_uom_code                                                       ||
||                                                                                                      ||
||   Description    : Get MLS UOM code                                      ||
||                                                                                                      ||
||   Usage/Purpose  : Purpose to use the MLS UOM code across all CFD                    ||
||                    documents                                                              ||
||                                                                                                      ||
||   Ver    Date          Author                    Modification/History           ||
||   1.0    31-AUG-10    Suresh Kandru              Created                        ||
||                                                                                                      ||
||   Special Notes : None                                                                       ||
||                                                                                                      ||
******************************************************************************/
PROCEDURE xxgil_get_mls_uom_code(p_uom_code              IN    VARCHAR2,
                                 p_country                   IN    VARCHAR2,
                                 x_description                OUT     VARCHAR2,
                                 x_return_msg                 OUT     VARCHAR2)
IS
l_uom_code  VARCHAR2(240);
BEGIN
  SELECT ATTRIBUTE1
  INTO l_uom_code
  FROM apps.FND_LOOKUP_VALUES
  WHERE LOOKUP_TYPE='XXGIL_MLS_UOM_CODE'
  AND UPPER(DESCRIPTION) =UPPER(p_uom_code)
  AND UPPER(TAG) = UPPER(p_country)
  AND LANGUAGE = 'US';
  x_description  := l_uom_code;
EXCEPTION
  WHEN NO_DATA_FOUND THEN
    x_description  := p_uom_code;
  WHEN OTHERS THEN
    x_description  := p_uom_code;
  END xxgil_get_mls_uom_code;

/*****************************************************************************
||                                                                                                     ||
||   Filename       : xxgil_otc_print_global_pkg.pkb                                        ||
||                                                                                          ||
||   Function Name : xxgil_delsum_qty                                                       ||
||                                                                                          ||
||   Description    : Returns delivery details received qty sum                             ||
||                                                                                          ||
||   Usage/Purpose  : Purpose to return delivery details received qty sum                   ||
||                                                                                          ||
||                                                                                          ||
||   Ver    Date          Author                    Modification/History                    ||
||   1.0    05-MAY-17    Vikas Sathidev             Created                                 ||
||                                                                                          ||
||   Special Notes : None                                                                   ||
||                                                                                                      ||
******************************************************************************/
FUNCTION xxgil_delsum_qty(p_line_id IN NUMBER, p_item_id IN NUMBER)
   RETURN NUMBER
AS
   lv_qty                             NUMBER := 0;
BEGIN
   SELECT SUM(wdd.requested_quantity)
     INTO lv_qty
     FROM apps.wsh_delivery_details wdd, apps.mtl_lot_numbers mln
    WHERE 1 = 1
      AND wdd.lot_number = mln.lot_number
      AND wdd.organization_id = mln.organization_id
      AND wdd.source_line_id = p_line_id
      AND mln.inventory_item_id = p_item_id;

   RETURN lv_qty;
EXCEPTION
   WHEN OTHERS
   THEN
      RETURN 0;
END xxgil_delsum_qty;
/********************************************************************************************************
||                                                                                                      ||
||   Filename       : xxgil_otc_print_global_pkg.pks                                                    ||
||                                                                                                      ||
||   Procedure Name : xxgil_inv_pack_addr                                                               ||
||                                                                                                      ||
||   Description    : Shipping warehouse address for Pack Slip Report                                   ||
||                                                                                                      ||
||   Usage/Purpose  : Purpose to use the warehouse address in Pack Slip Report                          ||
||                                                                                                      ||
||   Ver    Date          Author                    Modification/History                                ||
||   1.0    05-APR-18    Ranajit Sinha              Created - for CR1934                                ||
||                                                                                                      ||
||   Special Notes : None                                                                               ||
||                                                                                                      ||
**********************************************************************************************************/
PROCEDURE xxgil_inv_pack_addr (p_inv_org_id                           IN     NUMBER,
                               p_org_id                           IN      NUMBER,
                                    p_party_name                  OUT     VARCHAR2,
                                    p_tax_registration_num        OUT     VARCHAR2,
                                    p_address1                    OUT     VARCHAR2,
                                    p_address2                    OUT     VARCHAR2,
                                    p_address3                    OUT     VARCHAR2,
                                    p_address4                    OUT     VARCHAR2,
                                    p_address5                    OUT     VARCHAR2,
                                    p_address6                    OUT     VARCHAR2,
                                    p_address7                    OUT     VARCHAR2,
                                    p_city                        OUT     VARCHAR2,
                                    p_county                      OUT     VARCHAR2,
                                    p_state                       OUT     VARCHAR2,
                                    p_province                    OUT     VARCHAR2,
                                    p_postal_code                 OUT     VARCHAR2,
                                    p_country                     OUT     VARCHAR2,
                                    p_phone                       OUT     VARCHAR2,
                                    p_fax                         OUT     VARCHAR2,
                                    p_return_msg                  OUT     VARCHAR2)
IS
BEGIN
  BEGIN
    SELECT flv.attribute1,
     flv.attribute2,
     flv.attribute3,
     flv.attribute4,
     flv.attribute5,
     flv.attribute6,
     flv.attribute7,
     flv.attribute8,
     flv.attribute9,
     flv.attribute10,
     flv.attribute11
   INTO  p_party_name,
     p_address1,
     p_address2,
     p_address3,
     p_address4,
     p_address5,
     p_address6,
     p_address7,
         p_phone,
         p_fax,
     p_tax_registration_num
    FROM apps.fnd_lookup_values flv
    WHERE flv.lookup_type ='XXGIL_OTC_ORG_PACK_ADDRESS'
    AND flv.attribute_category ='XXGIL_OTC_ORG_PACK_ADDRESS'
    AND flv.language='US'
    AND flv.enabled_flag = 'Y'
    AND NVL(flv.start_date_active,TRUNC(SYSDATE-1)) < TRUNC(SYSDATE)
    AND NVL(flv.end_date_active,TRUNC(SYSDATE)) >= TRUNC(SYSDATE)
    AND flv.attribute13=p_inv_org_id
    AND flv.attribute12= p_org_id;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      p_address1 := NULL;
    WHEN OTHERS THEN
      p_address1 := NULL;
  END;
  IF p_address1 IS NULL THEN
    BEGIN
      SELECT xep.name
            ,hl.address_line_1
            ,hl.address_line_2
            ,hl.address_line_3
            ,hl.town_or_city||DECODE(hl.town_or_city,NULL,hl.region_1,DECODE(hl.region_1,NULL,NULL,', '||hl.region_1))
            ,hl.region_2||DECODE(hl.region_2,NULL,hl.postal_code,DECODE(hl.postal_code,NULL,NULL,', '||hl.postal_code))
            ,hl.country
            ,NULL
            ,hl.telephone_number_1||
              DECODE(hl.telephone_number_1,NULL,hl.telephone_number_2,DECODE(hl.telephone_number_2,NULL,NULL,', '||hl.telephone_number_2))||
              DECODE(hl.telephone_number_1,NULL,DECODE(hl.telephone_number_2,NULL,hl.telephone_number_3,DECODE(hl.telephone_number_3,NULL,NULL,', '||                                 hl.telephone_number_3)),DECODE(hl.telephone_number_3,NULL,NULL,', '||hl.telephone_number_3))
            ,hp.tax_reference
      INTO  p_party_name
           ,p_address1
           ,p_address2
           ,p_address3
           ,p_address4
           ,p_address5
           ,p_address6
           ,p_address7
           ,p_phone
           ,p_tax_registration_num
      FROM apps.hr_all_organization_units haou
          ,apps.hr_locations hl
          ,apps.org_organization_definitions ood
          ,apps.xle_entity_profiles xep
          ,apps.xle_firstparty_information_v xfi
          ,apps.hz_parties hp
      WHERE 1=1
      AND haou.location_id = hl.location_id
      AND haou.organization_id = ood.organization_id
      AND ood.legal_entity = xep.legal_entity_id
      AND xep.legal_entity_id = xfi.legal_entity_id
      AND xfi.party_id = hp.party_id
      AND haou.organization_id = p_inv_org_id;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        p_return_msg := 'No data found while deriving warehouse address from hr_locations for : '||p_inv_org_id;
        p_party_name := NULL;
        p_address1 := NULL;
        p_address2:= NULL;
        p_address3:= NULL;
        p_address4:= NULL;
        p_address5:= NULL;
        p_address6:= NULL;
        p_address7:= NULL;
        p_phone:= NULL;
        p_fax:= NULL;
        p_tax_registration_num:= NULL;
      WHEN OTHERS THEN
        p_return_msg := 'Error in Procedure xxgil_inv_pack_addr while deriving warehouse address from hr_locations :'||SQLERRM;
        p_party_name := NULL;
        p_address1 := NULL;
        p_address2:= NULL;
        p_address3:= NULL;
        p_address4:= NULL;
        p_address5:= NULL;
        p_address6:= NULL;
        p_address7:= NULL;
        p_phone:= NULL;
        p_fax:= NULL;
        p_tax_registration_num:= NULL;
    END;
  END IF;
EXCEPTION
  WHEN OTHERS THEN
    p_return_msg := 'Unexpected Error in Procedure xxgil_inv_pack_addr :'||SQLERRM;
END xxgil_inv_pack_addr;
END Xxgil_Otc_Print_Global_Pkg;
/