Magenta Systems Code Signing Trust and Certificate Check component v1.0
========================================================================

Updated by Angus Robertson, Magenta Systems Ltd, England, 26th November 2018
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

Compatible with Delphi 7, 2007, 2009, 2010, XE, XE2, XE3, XE4, XE5,
XE6, XE7, XE8, 10 Seattle, 10.1 Berlin, 10.2 Tokyo and 10.3 Rio.
Compatible with Windows Vista, 2008, 7, 8, 2012, 10, 2016 and 2019.

Supports both VCL Win32 and Win64, but there is no 64-bit version of
capicom.dll so some functionality is not available.


Introduction
------------

SignProg functions are designed for Code Signing, aka Microsoft
Authenticode.

Currently just two functions that check if an EXE, DLL or CAB file has
a valid code signing certificate, that the certificate is trusted and
that the program is not corrupted.  These functions were written to
support an remote program updater tool, to ensure the new program was
not corrupted, but may also be used to self test a Delphi application
for image corruption.

ProgVerifyTrust - simple function to check code signing certificate exists
and is valid and not expired, and the program image is not corrupted.
This should work with Windows 2000 and later (which have wintrust.dll).
This function works in Win32 and win64 applications.

ProgVerifyCert - similar to ProgVerifyTrust, but also extracts the
certificate information, names, dates, etc.  But this function needs
capicom.dll COM object to be installed (from which the CAPICOM_TLB type
library is created), which is a free redistributable file, included with
this code.  This function only works in Win32 applications, because
Microsoft has not released a 64-bit of capicom.dll, instead a class error
is returned.

Microsoft claims Capicom is deprecated with Windows 7, but the DLL still
works on Windows 10 32-bit and 64-bit editions, with Win32 applications.
You need to register the COM object by running 'regsvr32.exe capicom.dll'

Note the API used in CapiCom to sign a program is SignerSign but probably
does not support current signing algorithms since it's so old.  See bwlow
for hoe to actually sign code.


Demonstration Application
-------------------------

A Windows demonstration application TRUSTCHK.EXE is supplied, with source
and compiled program.  A directory signed-samples includes several
programs to test checking signatures, trustnone.exe is unsigned,
trustbad.exe is deliberately corrupted, trustexpired.cab has an expired
certificate, trustodd.exe has an untrusted self signed certificate,
trustok.exe is valid.


Function ProgVerifyTrust
------------------------

function ProgVerifyTrust (const Fname: string ; const HashOnly,
                        Expired: boolean; var Response: string): integer ;

Returns ERROR_SUCCESS (0) for success, -1 for error, or Trust_E_xx or
CERT_E_xx literals for other partial success or failures, with a literal
response in Response.


Function ProgVerifyCert
-----------------------

function ProgVerifyCert (const Fname: string ; var CertInfo: TCertInfo): integer ;

TCertInfo = record
    Res: integer ;
    Response: string ;
    SignerName: string ;
    SignerIssuer: string ;
    SignerExpireDT: TDateTime ;
    DescName: string ;
    DescURL: string ;
    StamperName: string ;
    SigningDT: TDateTime ;
end ;

Res and Response similar to ProgVerifyTrust, also certificate and signing details.
A formatted response is similar to the following:

Check Cert OK for \signed-samples\trustok.exe
Trusted Code
Signed by: Magenta Systems Ltd
Issued by: Thawte Code Signing CA
Cert Expires: 17/10/2010
UTC Timestamp: 10/07/2010 18:19:44
Timestamped by: VeriSign Time Stamping Services Signer - G2
Description: Signals
URL: http://www.magsys.co.uk/


Changes
-------

26 Nov 2018 - renamed signprog to MagSignProg for consistency


Code Signing Tools
------------------

The old distribution included a tools directory with signcode.exe and
other files, these are now all obsolete and should not be used for
signing programs.

The latest signing tools are described at:

https://docs.microsoft.com/en-gb/windows/desktop/SecCrypto/signtool

with the main command line program being signtool.exe shipped in the
Windows SDK and later versions of the .NET Framework.  This needs
PKCS12/PFX certificate files that are password protected, either as
physical files or on a hardware signing dongle (USB key).

Also, applications are now mostly signed twice, once with a SHA1 digest
for compatibility with older operating systems, and then again with a
SHA256 for current compatibility.

Typical commands to dual sign and verify a file are as follows:

cd C:\Program Files (x86)\Windows Kits\10\bin\x86
signtool sign /p "password" /f "c:\certs\magenta-certkey.pfx" /d "Title" /t http://timestamp.verisign.com/scripts/timstamp.dll "c:\release\codelook.exe"
signtool sign /p "password" /f "c:\certs\magenta-certkey.pfx" /d "Title" /as /fd sha256 /tr http://sha256timestamp.ws.symantec.com/sha256/timestamp "c:\release\codelook.exe"
signtool verify /all /pa c:\release\codelook.exe

The full list of arguments is detailed at the signtool URL above.


Copyright Information
---------------------

Magenta Systems Code Signing Trust and Certificate Check component is
freeware, but is still copyrighted by Magenta Systems Ltd who may change
the status or withdraw it at any time, without notice.

Magenta Systems Code Signing Trust and Certificate Check component may
be freely distributed via web pages, FTP sites, BBS and conferencing
systems or on CD-ROM in unaltered zip format, but no charge may be made
other than reasonable media or bandwidth cost.


Magenta Systems Ltd
9 Vincent Road
Croydon
CR0 6ED
United Kingdom

Phone 020 8656 3636, International Phone +44 20 8656 3636
Fax 020 8656 8127, International Fax +44 20 8656 8127

Email: delphi@magsys.co.uk
Web: http://www.magsys.co.uk/delphi/



