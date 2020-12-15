CREATE OR REPLACE AND COMPILE JAVA SOURCE NAMED apps.XXGIL_QRCODE_BMP2JPEG
AS
-- +===================================================================================+
-- +                       xxgil Technologies Inc.
-- +==============================================================================================================+
-- |
-- |Object Name: xxgil_QRCODE_BMP2JPEG
-- |
-- |Description: The package body used to convert the format from bmp to jpg for xmlp template
-- |CALLED BY    : xxgil_qrcode_generation_pkg.bmp2jpg function
-- |
-- |Modification History:
-- |===============
-- |Version       Date          Author                      Remarks
-- |=========   =============  =========                   =============================
-- |1.0         23-NOV-2020                                 Initial Version
-- +==============================================================================================================+

import oracle.sql.BLOB;
import oracle.sql.*;
import oracle.jdbc.driver.*;
import java.sql.*;
import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.*;
import java.util.*;
import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.sql.Blob;

public class Bmp2JPG {

 static OracleDriver ora = new OracleDriver();
 static Connection conn;
 static ByteArrayOutputStream out;
 static {
  try {
   conn = ora.defaultConnection();
  } catch (Exception ex) {}
 }
 public static ByteArrayOutputStream TO_JPG(java.sql.Blob blob) throws Exception {
  ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
  BufferedImage image = ImageIO.read(blob.getBinaryStream());
  ImageIO.write(image, "jpeg", outputStream);


  return outputStream;
 }

 public static oracle.sql.BLOB convert2JPG(oracle.sql.BLOB value) throws Exception {

  if (conn == null) conn = ora.defaultConnection();
  BLOB retBlob = BLOB.createTemporary(conn, true, oracle.sql.BLOB.DURATION_SESSION);

  ByteArrayOutputStream out = new ByteArrayOutputStream();
  out = Bmp2JPG.TO_JPG(value);

  try {
   java.io.OutputStream outStr = retBlob.setBinaryStream(0);
   outStr.write(out.toByteArray());
   outStr.flush();
  } finally {
   out.close();
  }
  return retBlob;
 }
}
/