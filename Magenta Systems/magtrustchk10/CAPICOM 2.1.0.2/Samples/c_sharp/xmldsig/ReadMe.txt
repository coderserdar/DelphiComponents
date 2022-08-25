CAPICOM sample xmldsig
-----------------------
Date:  April 1st, 2002

This is a .NET/CAPICOM sample which demonstrates how to do XML Digital Signature using the .NET cryptography 
classes and CAPICOM 2.0 and PInvoke to access CAPI methods:


1) To use CAPICOM from .NET, you need first to generate a Runtime Callable Wrapper (RCW). This is generated
with tlbimp.exe which is a tool that is included in the .NET framework SDK. In this sample, we call our RCW 
Interop.CAPICOM.DLL, and to generate it we use the followingcommand:
	tlbimp capicom.dll /out:Interop.CAPICOM.DLL

2) You can then build the sample using the build.bat batch file. 

NOTES: You need to have at least 1 certificate in the "MY" store with a private key associated with it
to be able to sign/verify using this sample. This sample supports verification using both
RSAKeyValue and X509Data key info.

To so the verify you will also need to build
You can create a self-signed cert using the SDK tool, makecert.exe which can be retrieved from:
  http://msdn.microsoft.com/downloads/default.asp?URL=/code/sample.asp?url=/msdn-files/027/000/219/msdncompositedoc.xml

  example: makecert -sk MyContainer -ss MY -n "CN=Joe Smith"

