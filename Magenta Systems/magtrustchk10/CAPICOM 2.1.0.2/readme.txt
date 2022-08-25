CAPICOM 2.1.0.2
================

The CAPICOM 2.1.0.2 update addresses an argument validation issue with the 
Find method of the Certificates interface.  No new features have been added
and all existing features are backwards compatible with 2.1.0.1.


The CAPICOM Team is pleased to announce the general availability of CAPICOM 
2.1.0.1. This new version of CAPICOM contains the following new features:

    + CertificateStatus: A new Certificates collection property that allows 
      the caller to set or retrieve the set of additional certificates that 
      may be useful during certificate chain building.
    + CertificateStatus.CheckFlag now defaults to CAPICOM_CHECK_ONLINE_ALL
    + Store: 2 new methods and 2 new properties are available on the Store 
      object. 
        + Delete: Allows the caller to delete non-system certificate stores. 
        + Close: Allows the caller to close a certificate store. This method 
          was added to address limitations in the JScript engine and is not 
          needed in other environments.
        + Name: Allows the caller to retrieve the name of a certificate store
        + Location: Allows the caller to retrieve the location 
          (CAPICOM_STORE_LOCATION) of a certificate store	
    + HashedData: Additional hash algorithms have been defined for SHA256, 
      SHA384 and SHA512. Note that these algorithms are not available on 
      Windows XP or below.

    + CHashData.vbs has been updated to use the new hash algorithms
    + Storesh.cs has been updated to use the new Delete method.

CAPICOM 2.1.0.1 also addresses several bugs including:

    + Memory leak when calling SignedData.Signers.Items or 
      SignedData.Cetificates.Item in a loop
    + The Certficate status object does not return an error when the 
      Certificate Signing bit is not set in the Key Usage extension of a CA 
      certificate in the chain.
    + Changes made to a Certificate object retrieved from a collection 
      returned by a Find not persisted.	


Known Issues include:
    + Exporting keys to a PKCS12/PFX file fails on Windows 2000 and 
      Windows XP with keys that are marked non-exportable even if 
      CAPICOM_EXPORT_IGNORE_PRIVATE_KEY_NOT_EXPORTABLE_ERROR has been 
      specified.
    + It is not possible to view the contents of a SignedData object if it is 
      signature invalid. As a work around users can use the Certificates 
      object to open the Pkcs7 and find the signer certificate.
    + The CVersion.vbs incorectly identifies the 2.0.0.3 release as version 
      2.0.


CAPICOM 2.1.0.0 is the best release of CAPICOM yet. The CAPICOM team 
recommends all users of previous releases of CAPICOM upgrade to the latest 
release. The package "CC2RINST.EXE" consists of the following files:

CC2RINST.EXE
     |
     |--x86
     |  |
     |  |- capicom.pdb
     |	|--capicom.cab
     |     	|
     |     	|--capicom.dll
     |    	|--capicom.inf
     |
     |--license.txt
     |--readme.txt
     |--samples
           |
           |--asp
           |   |
           |   |-workflow
           |
           |--c_sharp
           |   |
           |   |--storesh
           |   |--chainsh
           |   |--xmldsig
           |
           |--cpp
           |   |
           |   |--isapiCertPolicy
           |   |--store
           |
           |--html
           |   |
           |   |--Auxiliary.htm
           |   |--Certificates.htm
           |   |--EncryptedData.htm
           |   |--HashedData.htm
           |   |--SignData.htm
           |   |--Store.htm
           |
           |--vb
           |   |
           |   |--bridgedisplay
           |   |--find
           |   |--privatekey
           |   |--signcode
           |   |--smime
           |
           |--vbs
               |
               |--CEncrypt.vbs
               |--CEnvelop.vbs
               |--CHashData.vbs
               |--CSignCode.vbs
               |--CSignData.vbs
               |--CStore.vbs
               |--CView.vbs
               |--CVersion.vbs
               |--CSetKeyPerm.vbs

To install CAPICOM extract "CAPICOM.DLL" from CAPICOM.CAB to your system32 
directory, then execute "regsvr32.exe CAPICOM.DLL". For more information on 
installing CAPICOM see the section "Getting Ready to Use CAPICOM" in the 
Platform SDK.

We believe one of our largest strengths is our user community. Microsoft 
technical communities provide opportunities to interact with Microsoft 
employees, experts, and your peers in order to share knowledge and news 
about CAPICOM and related technologies. Please post your questions or 
comments about CAPICOM on the microsoft.public.security.crypto newsgroup. 

The CAPICOM team
