@echo off

rem
rem Building certificates for ICS-SSL sample programs
rem By François PIETTE    http://www.overbyte.be
rem
rem This batch file is derived from chapter 5 of the excellent
rem book "Netwrok Security with OpenSSL" ISBN 0-596-00270-X
rem See page 42, 63 and 124
rem
rem Next serial number is in rool.srl text file
rem You must change the settings in OpenSSL.conf and
rem also in the environment variables below.
rem

@echo.
@echo Building certificates

set BASE_DIR=D:\FPiette\Delphi\Internet\SslCerts
set OPENSSL_BIN=D:\FPiette\OpenSSL\openssl-0.9.6g\out32dll
set OPENSSL_CONF=%BASE_DIR%\openssl.conf
set PASS=password

@echo.
echo Delete existing files
if exist RootKey.pem     del RootKey.pem
if exist RootReq.pem     del RootReq.pem
if exist RootCert.pem    del RootCert.pem
if exist Root.pem        del Root.pem
if exist Root.p12        del Root.p12
if exist ServerCAkey.pem del ServerCAkey.pem
if exist ServerCAreq.pem del ServerCAreq.pem
if exist ServerCA.pem    del ServerCA.pem
if exist ServerCA.p12    del ServerCA.p12
if exist ServerKey.pem   del ServerKey.pem
if exist ServerReq.pem   del ServerReq.pem
if exist Server.pem      del Server.pem
if exist ClientKey.pem   del ClientKey.pem
if exist ClientReq.pem   del ClientReq.pem
if exist Client.pem      del Client.pem

@echo.
echo Create the root CA
%OPENSSL_BIN%\OpenSSL req -newkey rsa:1024 -sha1 -keyout RootKey.pem -out RootReq.pem -passin pass:%PASS% -passout pass:%PASS% -text
%OPENSSL_BIN%\OpenSSL x509 -req -in RootReq.pem -sha1 -extfile openssl.conf -extensions v3_ca -signkey RootKey.pem -out RootCert.pem -passin pass:%PASS% -days 730
type RootCert.pem >Root.pem
type RootKey.pem >>Root.pem
%OPENSSL_BIN%\OpenSSL x509 -subject -issuer -noout -in Root.pem

@echo.
echo Export the root CA in a format suitable for IE
%OPENSSL_BIN%\openssl pkcs12 -export -inkey RootKey.pem -in RootCert.pem -out Root.p12 -passin pass:%PASS% -passout pass:%PASS%

@echo.
echo Create the server CA and sign it with the root CA
%OPENSSL_BIN%\OpenSSL req -newkey rsa:1024 -sha1 -keyout ServerCAkey.pem -out ServerCAreq.pem -passin pass:%PASS% -passout pass:%PASS% -text
%OPENSSL_BIN%\OpenSSL x509 -req -in ServerCAreq.pem -sha1 -extfile openssl.conf -extensions v3_ca -CA root.pem -CAkey Root.pem -CAcreateserial -out ServerCAcert.pem -passin pass:%PASS% -days 730
type ServerCAcert.pem >ServerCA.pem
type ServerCAkey.pem >>ServerCA.pem
%OPENSSL_BIN%\OpenSSL x509 -subject -issuer -noout -in ServerCA.pem

@echo.
echo Export the server CA in a format suitable for IE
%OPENSSL_BIN%\openssl pkcs12 -export -inkey ServerCAKey.pem -in ServerCACert.pem -out ServerCA.p12 -passin pass:%PASS% -passout pass:%PASS%

@echo.
echo Create the server's certificate and sign it with the server CA
%OPENSSL_BIN%\OpenSSL req -newkey rsa:1024 -sha1 -keyout ServerKey.pem -out ServerReq.pem -passin pass:%PASS% -passout pass:%PASS% -text
%OPENSSL_BIN%\OpenSSL x509 -req -in ServerReq.pem -sha1 -extfile openssl.conf -extensions usr_cert -CA ServerCA.pem -CAkey ServerCA.pem -CAcreateserial -out ServerCert.pem -passin pass:%PASS% -days 730
type ServerCert.pem >Server.pem
type ServerKey.pem >>Server.pem
%OPENSSL_BIN%\OpenSSL x509 -subject -issuer -noout -in Server.pem

@echo.
echo Create the client certificate and sign it with the root CA
%OPENSSL_BIN%\OpenSSL req -newkey rsa:1024 -sha1 -keyout ClientKey.pem -out ClientReq.pem -passin pass:%PASS% -passout pass:%PASS% -text
%OPENSSL_BIN%\OpenSSL x509 -req -in ClientReq.pem -sha1 -extfile openssl.conf -extensions usr_cert -CA Root.pem -CAkey Root.pem -CAcreateserial -out ClientCert.pem -passin pass:%PASS% -days 730
type ClientCert.pem >Client.pem
type ClientKey.pem >>Client.pem
%OPENSSL_BIN%\OpenSSL x509 -subject -issuer -noout -in Client.pem

@echo.

@echo Hit RETURN to terminate
pause
