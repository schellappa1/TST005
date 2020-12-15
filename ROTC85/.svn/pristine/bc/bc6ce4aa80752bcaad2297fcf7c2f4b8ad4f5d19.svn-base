create or replace PACKAGE xxgil_qrcode_generation_pkg 
AS
-- +====================================================================================+
-- | 
-- |Object Name: xxgil_qrcode_generation_pkg
-- |
-- |Description: The package body used to generate the QR Code in CLOB , BMP Format
-- |				  GIT Directory:
-- |				  https://github.com/zorantica/plsql-qr-code/tree/master/package
-- |				  https://github.com/zorantica/plsql-qr-code/tree/master/BMP2JPG	 
-- |
-- |Modification History:
-- |====================
-- |Version       Date          Author                      			Remarks
-- |=========   =============  =========                   				=======
-- |1.0         23-Nov-2020    Sridhar Tylam, Saravanan Vijayasundaram 	Initial Version
-- +=====================================================================================+

FUNCTION get_qrcode_bmp(
    p_data varchar2,  --data going to be encoded into QR code
    p_error_correction varchar2, --L, M, Q or H
    p_margines varchar2 default 'N' --margines around QR code (4 modules) - values Y or N
    ) RETURN blob;

FUNCTION get_qrcode_clob(p_text IN VARCHAR2)
   RETURN CLOB;

END xxgil_qrcode_generation_pkg;
