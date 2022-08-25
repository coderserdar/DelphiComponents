'******************************************************************************
'
' THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND, 
' EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED 
' WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
'
' Copyright (C) 1999- 2002.  Microsoft Corporation.  All rights reserved.
'
'******************************************************************************
'
' CStore.vbs
'
' This is a sample script to illustrate how to use the CAPICOM's Store and 
' Certificate objects to manage certificate(s) in a specified store with
' filtering options.
'
' Note: For simplicity, this script does not handle exception.
'
'******************************************************************************

Option Explicit

' Command.
Const Unknown                                                  = 0
Const View		                                                = 1
Const Import                                                   = 2
Const Export                                                   = 3
Const Delete                                                   = 4
Const Archive                                                  = 5
Const Activate                                                 = 6
                                                               
' Verbose level.                                               
Const Normal                                                   = 0
Const Detail                                                   = 1
Const UI                                                       = 2
                                                               
' CAPICOM constants.                                           
Const CAPICOM_MEMORY_STORE                                     = 0
Const CAPICOM_LOCAL_MACHINE_STORE                              = 1
Const CAPICOM_CURRENT_USER_STORE                               = 2
Const CAPICOM_ACTIVE_DIRECTORY_USER_STORE                      = 3
Const CAPICOM_SMART_CARD_USER_STORE                            = 4
                                                               
Const CAPICOM_STORE_OPEN_READ_ONLY                             = 0
Const CAPICOM_STORE_OPEN_READ_WRITE                            = 1
Const CAPICOM_STORE_OPEN_MAXIMUM_ALLOWED                       = 2
Const CAPICOM_STORE_OPEN_EXISTING_ONLY                         = 128
Const CAPICOM_STORE_OPEN_INCLUDE_ARCHIVED                      = 256
                                                                 
Const CAPICOM_CERTIFICATE_FIND_SHA1_HASH                       = 0
Const CAPICOM_CERTIFICATE_FIND_SUBJECT_NAME                    = 1
Const CAPICOM_CERTIFICATE_FIND_ISSUER_NAME                     = 2
Const CAPICOM_CERTIFICATE_FIND_ROOT_NAME                       = 3
Const CAPICOM_CERTIFICATE_FIND_TEMPLATE_NAME                   = 4
Const CAPICOM_CERTIFICATE_FIND_EXTENSION                       = 5
Const CAPICOM_CERTIFICATE_FIND_EXTENDED_PROPERTY               = 6
Const CAPICOM_CERTIFICATE_FIND_APPLICATION_POLICY              = 7
Const CAPICOM_CERTIFICATE_FIND_CERTIFICATE_POLICY              = 8
Const CAPICOM_CERTIFICATE_FIND_TIME_VALID                      = 9
Const CAPICOM_CERTIFICATE_FIND_TIME_NOT_YET_VALID              = 10
Const CAPICOM_CERTIFICATE_FIND_TIME_EXPIRED                    = 11
Const CAPICOM_CERTIFICATE_FIND_KEY_USAGE                       = 12
                                                               
Const CAPICOM_KEY_STORAGE_DEFAULT                              = 0
Const CAPICOM_KEY_STORAGE_EXPORTABLE                           = 1
Const CAPICOM_KEY_STORAGE_USER_PROTECTED                       = 2
                                                               
Const CAPICOM_STORE_SAVE_AS_SERIALIZED                         = 0
Const CAPICOM_STORE_SAVE_AS_PKCS7                              = 1
Const CAPICOM_STORE_SAVE_AS_PFX                                = 2

Const CAPICOM_EXPORT_DEFAULT                                   = 0
Const CAPICOM_EXPORT_IGNORE_PRIVATE_KEY_NOT_EXPORTABLE_ERROR   = 1

' Globals.
Dim StoreLocationNames(4)
StoreLocationNames(0) = "Certificate File"
StoreLocationNames(1) = "Local Machine"
StoreLocationNames(2) = "Current User"
StoreLocationNames(3) = "Active Directory"
StoreLocationNames(4) = "Smart Card"

' Command line arguments.
Dim Command           : Command           = Unknown
Dim OpenMode          : OpenMode          = CAPICOM_STORE_OPEN_MAXIMUM_ALLOWED OR CAPICOM_STORE_OPEN_EXISTING_ONLY 
Dim StoreLocation     : StoreLocation     = NULL
Dim StoreName         : StoreName         = "MY"
Dim CertFile          : CertFile          = Null
Dim Password          : Password          = ""
Dim SaveAs            : SaveAs            = CAPICOM_STORE_SAVE_AS_SERIALIZED
Dim ExportFlag        : ExportFlag        = CAPICOM_EXPORT_DEFAULT
Dim ValidOnly         : ValidOnly         = False
Dim DelKey            : DelKey            = False
Dim KeyStorageFlag    : KeyStorageFlag    = CAPICOM_KEY_STORAGE_DEFAULT
Dim NoPrompt          : NoPrompt          = False
Dim VerboseLevel      : VerboseLevel      = Normal
Dim ExtendedHelp      : ExtendedHelp      = False
                                          
' Filters (some can be multiples).        
Dim SHA1              : SHA1              = Null
Dim Subjects()
Dim Issuers()
Dim Roots()
Dim Templates()
Dim Extensions()
Dim Properties()
Dim Usages()
Dim Policies()
Dim Times()
Dim KeyUsages()

' First make sure the script is executed by CScript.exe.
If InStr(1, UCase(Wscript.FullName), "CSCRIPT.EXE", vbTextCompare) = 0 Then
   Wscript.Echo "This script can only be executed by CScript.exe." & vbCRLF & vbCRLF &_
                "You can either:" & vbCRLF & vbCRLF & _
                "1. Set CScript.exe as the default (Run CScript //h:cscript), or" & vbCRLF & _
                "2. Run CScript.exe directly as in, CScript " & Wscript.ScriptName & "."
   Wscript.Quit(-1)
End If

' Parse the command line.
ParseCommandLine

' Open the store.
Dim Store
Set Store = CreateObject("CAPICOM.Store")
Store.Open StoreLocation, StoreName, OpenMode

' Carry out the requested command.
Select Case Command
Case View
   DoViewCommand
      
Case Import
   DoImportCommand

Case Export
   DoExportCommand

Case Delete
   DoDeleteCommand

Case Archive
   DoArchiveCommand

Case Activate
   DoActivateCommand

Case Else
   ' This shouldn't happen!
   Wscript.Stdout.Writeline "Internal script error: Unknown command state (Command = " & CStr(Command) & ")."
   Wscript.Quit(-2)

End Select

' Free resources.
Set Store = Nothing
          
' We are all done.
Wscript.Quit(0)

' End Main


'******************************************************************************
'
' Subroutine: DoViewCommand
'
' Synopsis  : View certificates of a store or certificate file.
'
' Parameter : None
'
'******************************************************************************

Sub DoViewCommand
   Dim Certificate
   Dim Certificates
   Dim cIndex : cIndex = 0
   
   ' Load the specified certificate file if specified.
   If Not IsNull(CertFile) Then
      Store.Load CertFile, Password, CAPICOM_KEY_STORAGE_DEFAULT
   End If
 
   ' Perform filter(s) requested.
   Set Certificates = FilterCertificates(Store.Certificates)
   
   ' Display main title.
   Dim cCerts : cCerts = Certificates.Count
   Wscript.Stdout.Writeline "Viewing " & StoreLocationNames(StoreLocation) & " " & StoreName & " store - " & CStr(cCerts) & " certificate(s)"
   Wscript.Stdout.Writeline
   
   ' Now display all the certificates.
   For Each Certificate In Certificates
      cIndex = cIndex + 1
      DisplayCertificate Certificate, "=== Certificate " & CStr(cIndex) & " of " & CStr(cCerts) & " ==="
   Next
   
   ' Free resources.
   Set Certificates = Nothing
   
End Sub ' End DoViewCommand


'******************************************************************************
'
' Subroutine: DoImportCommand
'
' Synopsis  : Import certificates from a certificate file to store.
'
' Parameter : None
'
'******************************************************************************

Sub DoImportCommand
   Dim Certificate
   Dim Certificates
      
   ' Display main title.  
   Wscript.Stdout.Writeline "Importing " & CertFile & " to " & StoreLocationNames(StoreLocation) & " " & StoreName & " store, please wait..."
   Wscript.Stdout.Writeline
   
   ' Take a snapshot of the current certificates colletion.
   Set Certificates = Store.Certificates
   
   ' Import the certificate(s) to the store.
   Store.Load CertFile, Password, KeyStorageFlag
   
   ' Now display all imported certificates.
   Dim cIndex : cIndex = 0
   For Each Certificate In Store.Certificates      
      ' Try to find this certificate in the snapshot we grabbed previously.   
      Dim FoundCerts
      Set FoundCerts = Certificates.Find(CAPICOM_CERTIFICATE_FIND_SHA1_HASH, Certificate.Thumbprint)
      
      ' Skip it if we were able to find it, which means not added by us.
      If FoundCerts.Count = 0 Then
         ' Display the imported certificate.
         cIndex = cIndex + 1      
         DisplayCertificate Certificate, "=== Certificate " & CStr(cIndex) & " ==="
      End If
      
      Set FoundCerts = Nothing
   Next
   
   ' Display total certificates(s) imported.
   Wscript.Stdout.Writeline CStr(cIndex) & " certificate(s) successfully imported."
   
   ' Free resources.
   Set Certificates = Nothing
   
End Sub ' End DoImportCommand


'******************************************************************************
'
' Subroutine: DoExportCommand
'
' Synopsis  : Export certificates from a store to certificate file.
'
' Parameter : None.
'
'******************************************************************************

Sub DoExportCommand   
   Dim Certificate
   Dim Certificates
   Dim cIndex : cIndex = 0
   
   ' Perform filter(s) requested.
   Set Certificates = FilterCertificates(Store.Certificates)
   
   ' Display main title.
   Dim cCerts : cCerts = Certificates.Count
   Wscript.Stdout.Writeline "Exporting from " & StoreLocationNames(StoreLocation) & " " & StoreName & " store to " & CertFile & ", please wait..."
   Wscript.Stdout.Writeline
   
   ' Save the certificate(s) to file.
   If Certificates.Count > 0 Then
      Certificates.Save CertFile, Password, SaveAs, ExportFlag 
   
      ' Now display all exported certificates.
      For Each Certificate In Certificates
         cIndex = cIndex + 1
         DisplayCertificate Certificate, "=== Certificate " & CStr(cIndex) & " of " & CStr(cCerts) & " ==="
      Next
   End If

   ' Display total certificates(s) exported.
   Wscript.Stdout.Writeline CStr(cCerts) & " certificate(s) successfully exported."
   
   ' Free resources.
   Set Certificates = Nothing
   
End Sub ' End DoExportCommand


'******************************************************************************
'
' Subroutine: DoDeleteCommand
'
' Synopsis  : Delete certificates from a store.
'
' Parameter : None
'
'******************************************************************************

Sub DoDeleteCommand
   Dim Certificate
   Dim Certificates
   Dim cIndex   : cIndex   = 0
   Dim cDeleted : cDeleted = 0
   
   ' Perform filter(s) requested.
   Set Certificates = FilterCertificates(Store.Certificates)
   
   ' Display main title.
   Dim cCerts : cCerts = Certificates.Count
   Wscript.Stdout.Writeline "Deleting " & StoreLocationNames(StoreLocation) & " " & StoreName & " store, please wait... "
   Wscript.Stdout.Writeline
   
   ' Now delete all certificates(s).
   For Each Certificate In Certificates
      cIndex = cIndex + 1
      
      ' Display certificate to be deleted.
      DisplayCertificate Certificate, "=== Certificate " & CStr(cIndex) & " of " & CStr(cCerts) & " ==="

      ' Prompt user for permission.
      If DelKey Then
         If UserAgreed("Delete this certificate and its key container (No/Yes/All)? ") Then
            Certificate.PrivateKey.Delete
            Store.Remove Certificate
            cDeleted = cDeleted + 1
         End If
      Else
         If UserAgreed("Delete this certificate (No/Yes/All)? ") Then
            Store.Remove Certificate
            cDeleted = cDeleted + 1
         End If
      End If
   Next

   ' Display total certificates(s) deleted.
   Wscript.Stdout.Writeline CStr(cDeleted) & " certificate(s) successfully deleted."
   
   ' Free resources.
   Set Certificates = Nothing
   
End Sub ' End DoDeleteCommand


'******************************************************************************
'
' Subroutine: DoArchiveCommand
'
' Synopsis  : Archive certificates.
'
' Parameter : None
'
'******************************************************************************

Sub DoArchiveCommand
   Dim Certificate
   Dim Certificates
   Dim cIndex    : cIndex    = 0
   Dim cArchived : cArchived = 0
   
   ' Perform filter(s) requested.
   Set Certificates = FilterCertificates(Store.Certificates)
   
   ' Display main title.
   Dim cCerts : cCerts = Certificates.Count
   Wscript.Stdout.Writeline "Archiving " & StoreLocationNames(StoreLocation) & " " & StoreName & " store, please wait... "
   Wscript.Stdout.Writeline
   
   ' Now archive all certificates(s).
   For Each Certificate In Certificates
      cIndex = cIndex + 1

      ' Skip it if already archived.      
      If Not Certificate.Archived Then
         ' Display certificate to be archived.
         DisplayCertificate Certificate, "=== Certificate " & CStr(cIndex) & " of " & CStr(cCerts) & " ==="
   
         ' Prompt user for permission.
         If UserAgreed("Archive this certificate (No/Yes/All)? ") Then
            Certificate.Archived = True            
            cArchived = cArchived + 1
         End If
      End If
   Next

   ' Display total certificates(s) archived.
   Wscript.Stdout.Writeline CStr(cArchived) & " certificate(s) successfully archived."
   
   ' Free resources.
   Set Certificates = Nothing
   
End Sub ' End DoArchiveCommand


'******************************************************************************
'
' Subroutine: DoActivateCommand
'
' Synopsis  : Activate archived certificates.
'
' Parameter : None
'
'******************************************************************************

Sub DoActivateCommand
   Dim Certificate
   Dim Certificates
   Dim cIndex     : cIndex     = 0
   Dim cActivated : cActivated = 0
   
   ' Perform filter(s) requested.
   Set Certificates = FilterCertificates(Store.Certificates)
   
   ' Display main title.
   Dim cCerts : cCerts = Certificates.Count
   Wscript.Stdout.Writeline "Activating " & StoreLocationNames(StoreLocation) & " " & StoreName & " store, please wait... "
   Wscript.Stdout.Writeline
   
   ' Now activate all certificates(s).
   For Each Certificate In Certificates
      cIndex = cIndex + 1
      
      ' Skip it if not archived.
      If Certificate.Archived Then
         ' Display certificate to be activated.
         DisplayCertificate Certificate, "=== Certificate " & CStr(cIndex) & " of " & CStr(cCerts) & " ==="
   
         ' Prompt user for permission.
         If UserAgreed("Activate this certificate (No/Yes/All)? ") Then
            Certificate.Archived = False
            cActivated = cActivated + 1
         End If
      End If
   Next

   ' Display total certificates(s) activated.
   Wscript.Stdout.Writeline CStr(cActivated) & " certificate(s) successfully activated."
   
   ' Free resources.
   Set Certificates = Nothing
   
End Sub ' End DoActivateCommand


'******************************************************************************
'
' Function  : UserAgreed
'
' Synopsis  : Prompt user for permission to proceed with an operation.
'
' Parameter : Title - The prompt string.
'
' Return    : True if OK, else False.
'
'******************************************************************************

Function UserAgreed (Title)
   dim Tries  : Tries  = 0
   Dim Result : Result = NoPrompt
   
   ' If NoPrompt is not turned off, then prompt user for permission.
   If Result = False Then
      Do While (True)
         Dim Answer : Answer = ""
         
         ' Quit if more than 3 tries.                      
         Tries = Tries + 1
         If Tries > 3 Then
            Wscript.Stdout.Writeline "Too many tries. Program is exiting..."
            Wscript.Quit(-1)
         End If
         
         Wscript.Stdout.Write Title
         Answer = Wscript.Stdin.Readline
         Wscript.Stdout.Writeline
         
         Select Case UCase(Answer)
         Case "N", "NO"
            Result = False
            Exit Do
            
         Case "Y", "YES"
            Result = True
            Exit Do
   
         Case "A", "ALL"
            NoPrompt = True
            Result = True
            Exit Do
         
         Case Else
            Wscript.Stdout.Writeline "Valid answers are No, Yes, or All. Please try again."
         
         End Select         
      Loop
   End If
   
   UserAgreed = Result      
   
End Function ' End UserAgreed


'******************************************************************************
'
' Function  : FilterCertificates
'
' Synopsis  : Filter the set of certificates based on the filtering options.
'
' Parameter : Certificates - The certificates collection to be filtered.
'
' Return    : The filtered certificates collection.
'
'******************************************************************************

Function FilterCertificates (Certificates)
   Dim iIndex
   Dim Filtered
   Set Filtered = Certificates

   If Filtered.Count > 0 AND Not IsNull(SHA1) Then
      Set Filtered = Filtered.Find(CAPICOM_CERTIFICATE_FIND_SHA1_HASH, SHA1)
   End If
   
   If Filtered.Count > 0 AND IsReDimed(Subjects) Then
      For iIndex = LBound(Subjects) To UBound(Subjects) 
         Set Filtered = Filtered.Find(CAPICOM_CERTIFICATE_FIND_SUBJECT_NAME, Subjects(iIndex))
      Next
   End If
   
   If Filtered.Count > 0 AND IsReDimed(Issuers) Then
      For iIndex = LBound(Issuers) To UBound(Issuers) 
         Set Filtered = Filtered.Find(CAPICOM_CERTIFICATE_FIND_ISSUER_NAME, Issuers(iIndex))
      Next
   End If
   
   If Filtered.Count > 0 AND IsReDimed(Roots) Then
      For iIndex = LBound(Roots) To UBound(Roots) 
         Set Filtered = Filtered.Find(CAPICOM_CERTIFICATE_FIND_ROOT_NAME, Roots(iIndex))
      Next
   End If
   
   If Filtered.Count > 0 AND IsReDimed(Templates) Then
      For iIndex = LBound(Templates) To UBound(Templates) 
         Set Filtered = Filtered.Find(CAPICOM_CERTIFICATE_FIND_TEMPLATE_NAME, Templates(iIndex))
      Next
   End If
   
   If Filtered.Count > 0 AND IsReDimed(Extensions) Then
      For iIndex = LBound(Extensions) To UBound(Extensions) 
         Set Filtered = Filtered.Find(CAPICOM_CERTIFICATE_FIND_EXTENSION, Extensions(iIndex))
      Next
   End If
   
   If Filtered.Count > 0 AND IsReDimed(Properties) Then
      For iIndex = LBound(Properties) To UBound(Properties)
         Set Filtered = Filtered.Find(CAPICOM_CERTIFICATE_FIND_EXTENDED_PROPERTY, Properties(iIndex))
      Next
   End If
   
   If Filtered.Count > 0 AND IsReDimed(Usages) Then
      For iIndex = LBound(Usages) To UBound(Usages) 
         Set Filtered = Filtered.Find(CAPICOM_CERTIFICATE_FIND_APPLICATION_POLICY, Usages(iIndex))
      Next
   End If
   
   If Filtered.Count > 0 AND IsReDimed(Policies) Then
      For iIndex = LBound(Policies) To UBound(Policies) 
         Set Filtered = Filtered.Find(CAPICOM_CERTIFICATE_FIND_CERTIFICATE_POLICY, Policies(iIndex))
      Next
   End If

   If Filtered.Count > 0 AND IsReDimed(Times) Then
      For iIndex = LBound(Times) To UBound(Times)
         Set Filtered = Filtered.Find(Times(iIndex)) ', Now)
      Next
   End If
   
   If Filtered.Count > 0 AND IsReDimed(KeyUsages) Then
      For iIndex = LBound(KeyUsages) To UBound(KeyUsages) 
         Set Filtered = Filtered.Find(CAPICOM_CERTIFICATE_FIND_KEY_USAGE, KeyUsages(iIndex))
      Next
   End If

   If Filtered.Count > 0 AND ValidOnly = True Then
      ' Use the time valid find with valid flag set.
      Set Filtered = Filtered.Find(CAPICOM_CERTIFICATE_FIND_TIME_VALID, Now, True)
   End If
   
   Set FilterCertificates = Filtered

End Function ' End FilterCertificates


'******************************************************************************
'
' Subroutine: DisplayCertificate
'
' Synopsis  : Display the certificate.
'
' Parameter : Certificate - The certificate object to be displayed.
'             Title       - Display title.
'
'******************************************************************************

Sub DisplayCertificate (Certificate, Title)

   ' Turn on exception handling, since some methods, such as 
   ' Certificate.Template and PrivateKey.IsExportable, are not available
   ' in some down level platforms.
   On Error Resume Next
   
   Const CAPICOM_CERT_INFO_SUBJECT_EMAIL_NAME    = 2
   Const CAPICOM_CERT_INFO_SUBJECT_UPN           = 4
   Const CAPICOM_CERT_INFO_SUBJECT_DNS_NAME      = 6
   Const CAPICOM_ENCODED_DATA_FORMAT_MULTI_LINES = 1
   Const CAPICOM_CERT_POLICIES_OID               = "2.5.29.32"
   
   Dim KeySpecStrings(2)
   KeySpecStrings(0) = "Unknown"
   KeySpecStrings(1) = "Exchange"
   KeySpecStrings(2) = "Signature"
   
   Dim ProviderTypes(24)
   ProviderTypes(0)  = "Unknown"
   ProviderTypes(1)  = "PROV_RSA_FULL"
   ProviderTypes(2)  = "PROV_RSA_SIG"
   ProviderTypes(3)  = "PROV_DSS"
   ProviderTypes(4)  = "PROV_FORTEZZA"
   ProviderTypes(5)  = "PROV_MS_EXCHANGE"
   ProviderTypes(6)  = "PROV_SSL"
   ProviderTypes(7)  = "PROV_STT_MER"
   ProviderTypes(8)  = "PROV_STT_ACQ"
   ProviderTypes(9)  = "PROV_STT_BRND"
   ProviderTypes(10) = "PROV_STT_ROOT"
   ProviderTypes(11) = "PROV_STT_ISS"
   ProviderTypes(12) = "PROV_RSA_SCHANNEL"
   ProviderTypes(13) = "PROV_DSS_DH"
   ProviderTypes(14) = "PROV_EC_ECDSA_SIG"
   ProviderTypes(15) = "PROV_EC_ECNRA_SIG"
   ProviderTypes(16) = "PROV_EC_ECDSA_FULL"
   ProviderTypes(17) = "PROV_EC_ECNRA_FULL"
   ProviderTypes(18) = "PROV_DH_SCHANNEL"
   ProviderTypes(20) = "PROV_SPYRUS_LYNKS"
   ProviderTypes(21) = "PROV_RNG"
   ProviderTypes(22) = "PROV_INTEL_SEC"
   ProviderTypes(23) = "PROV_REPLACE_OWF"
   ProviderTypes(24) = "PROV_RSA_AES"
   
   Dim iIndex : iIndex = 0
   
   Wscript.Stdout.Writeline Title
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "Subject Name:"
   Wscript.Stdout.Writeline "  Simple name = " & Certificate.SubjectName
   Wscript.Stdout.Writeline "  Email name  = " & Certificate.GetInfo(CAPICOM_CERT_INFO_SUBJECT_EMAIL_NAME)
   Wscript.Stdout.Writeline "  UPN name    = " & Certificate.GetInfo(CAPICOM_CERT_INFO_SUBJECT_UPN)
   Wscript.Stdout.Writeline "  DNS name    = " & Certificate.GetInfo(CAPICOM_CERT_INFO_SUBJECT_DNS_NAME)
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "Issuer Name: " & Certificate.IssuerName
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "Serial Number: " & Certificate.SerialNumber
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "Not Before: " & Certificate.ValidFromDate
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "Not After: " & Certificate.ValidToDate
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "SHA1 Hash: " & Certificate.Thumbprint
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "IsValid: " & Certificate.IsValid()
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "Archived: " & Certificate.Archived
   Wscript.Stdout.Writeline
       
   If Certificate.BasicConstraints.IsPresent Then
      Wscript.Stdout.Writeline "Basic Constraints:" 
      Wscript.Stdout.Writeline "  Critical          = " & Certificate.BasicConstraints.IsCritical
      Wscript.Stdout.Writeline "  CA                = " & Certificate.BasicConstraints.IsCertificateAuthority
      Wscript.stdout.Write     "  PathLenConstraint = "
      If Certificate.BasicConstraints.IsPathLenConstraintPresent Then
         Wscript.Stdout.Writeline CStr(Certificate.BasicConstraints.PathLenConstraint)
      Else
         Wscript.Stdout.Writeline "Not present."
      End If
   Else
      Wscript.Stdout.Writeline "Basic Constraints: Not present."
   End If          
   Wscript.Stdout.Writeline
   
   If Certificate.KeyUsage.IsPresent Then
      Wscript.Stdout.Writeline "Key Usage:"
      Wscript.Stdout.Writeline "  Critical                  = "& Certificate.KeyUsage.IsCritical
      Wscript.Stdout.Writeline "  IsDigitalSignatureEnabled = " & Certificate.KeyUsage.IsDigitalSignatureEnabled 
      Wscript.Stdout.Writeline "  IsNonRepudiationEnabled   = " & Certificate.KeyUsage.IsNonRepudiationEnabled
      Wscript.Stdout.Writeline "  IsKeyEnciphermentEnabled  = " & Certificate.KeyUsage.IsKeyEnciphermentEnabled
      Wscript.Stdout.Writeline "  IsDataEnciphermentEnabled = " & Certificate.KeyUsage.IsDataEnciphermentEnabled
      Wscript.Stdout.Writeline "  IsKeyAgreementEnabled     = " & Certificate.KeyUsage.IsKeyAgreementEnabled
      Wscript.Stdout.Writeline "  IsKeyCertSignEnabled      = " & Certificate.KeyUsage.IsKeyCertSignEnabled
      Wscript.Stdout.Writeline "  IsCRLSignEnabled          = " & Certificate.KeyUsage.IsCRLSignEnabled
      Wscript.Stdout.Writeline "  IsEncipherOnlyEnabled     = " & Certificate.KeyUsage.IsEncipherOnlyEnabled
      Wscript.Stdout.Writeline "  IsDecipherOnlyEnabled     = " & Certificate.KeyUsage.IsDecipherOnlyEnabled
   Else
      Wscript.Stdout.Writeline "Key Usage: Not present."
   End If
   Wscript.Stdout.Writeline
   
   If Certificate.ExtendedKeyUsage.IsPresent Then
      If Certificate.ExtendedKeyUsage.EKUs.Count > 0 Then
         Dim OID
         Set OID = CreateObject("CAPICOM.OID")
         Wscript.Stdout.Writeline "Extended Key Usage:"
         Wscript.Stdout.Writeline "  Critical = " & Certificate.ExtendedKeyUsage.IsCritical
         Dim EKU
         For Each EKU In Certificate.ExtendedKeyUsage.EKUs
            OID.Value = EKU.OID
            Wscript.Stdout.Writeline "  " & OID.FriendlyName & " (" & OID.Value & ")"
         Next
         Set OID = Nothing
      Else
         Wscript.Stdout.Writeline "Extended Key Usage: Not valid for any usage."
         Wscript.Stdout.Writeline "  Critical = " & Certificate.ExtendedKeyUsage.IsCritical
      End If
   Else
      Wscript.Stdout.Writeline "Extended Key Usage: Not present (valid for all usages)."
   End If
   Wscript.Stdout.Writeline
   
   If Certificate.Template.IsPresent Then
      Wscript.Stdout.Writeline "Template:"
      Wscript.Stdout.Writeline "  Critical = " & Certificate.Template.IsCritical
      Wscript.Stdout.Writeline "  Name     = " & Certificate.Template.Name
      Wscript.Stdout.Writeline "  OID      = " & Certificate.Template.OID.FriendlyName & "(" & Certificate.Template.OID.Value & ")"
      Wscript.Stdout.Writeline "  Major    = " & CStr(Certificate.Template.MajorVersion)
      Wscript.Stdout.Writeline "  Minor    = " & CStr(Certificate.Template.MinorVersion)
   Else
      Wscript.Stdout.Writeline "Template: Not present."
   End If          
   Wscript.Stdout.Writeline
   
   Wscript.Stdout.Writeline "Public Key:"
   Wscript.Stdout.Writeline "  Algorithm  = " & Certificate.PublicKey.Algorithm.FriendlyName & "(" & Certificate.PublicKey.Algorithm.Value & ")"
   Wscript.Stdout.Writeline "  Length     = " & CStr(Certificate.PublicKey.Length) & " bits"
   Wscript.Stdout.Writeline "  Key blob   = " & Certificate.PublicKey.EncodedKey.Value
   Wscript.Stdout.Writeline "  Parameters = " & Certificate.PublicKey.EncodedParameters.Value
   
   If Certificate.HasPrivateKey Then
      Wscript.Stdout.Writeline "Private Key:"
      Wscript.Stdout.Writeline "  Container name   = " & Certificate.PrivateKey.ContainerName
      ' Don't display unique container name for hardware token because it may cause UI to be displayed.
      If StoreLocation = CAPICOM_SMART_CARD_USER_STORE OR Not Certificate.PrivateKey.IsHardwareDevice Then
         Wscript.Stdout.Writeline "  Unique name      = " & Certificate.PrivateKey.UniqueContainerName
      End If
      Wscript.Stdout.Writeline "  Provider name    = " & Certificate.PrivateKey.ProviderName
      Wscript.StdOut.Write     "  Provider type    = " 
      If Certificate.PrivateKey.ProviderType > UBound(ProviderTypes) Then
         Wscript.Stdout.Writeline ProviderTypes(0) & " (" & CStr(Certificate.PrivateKey.ProviderType) & ")"
      Else
         Wscript.Stdout.Writeline ProviderTypes(Certificate.PrivateKey.ProviderType) & " (" & CStr(Certificate.PrivateKey.ProviderType) & ")"
      End If
      Wscript.StdOut.Write     "  Key spec         = " 
      If Certificate.PrivateKey.KeySpec > UBound(KeySpecStrings) Then
         Wscript.Stdout.Writeline KeySpecStrings(0) & " (" & CStr(Certificate.PrivateKey.KeySpec) & ")"
      Else
         Wscript.Stdout.Writeline KeySpecStrings(Certificate.PrivateKey.KeySpec) & " (" & CStr(Certificate.PrivateKey.KeySpec) & ")"
      End If
      Wscript.Stdout.Writeline "  Accessible       = " & Certificate.PrivateKey.IsAccessible
      Wscript.Stdout.Writeline "  Protected        = " & Certificate.PrivateKey.IsProtected
      Wscript.Stdout.Writeline "  Exportable       = " & Certificate.PrivateKey.IsExportable
      Wscript.Stdout.Writeline "  Removable        = " & Certificate.PrivateKey.IsRemovable
      Wscript.Stdout.Writeline "  Machine keyset   = " & Certificate.PrivateKey.IsMachineKeyset
      Wscript.Stdout.Writeline "  Hardware storage = " & Certificate.PrivateKey.IsHardwareDevice
   Else
      Wscript.Stdout.Writeline "Private Key: Not found."
   End If
   Wscript.Stdout.Writeline
   
   If VerboseLevel = Detail Then
      iIndex = 0
      Dim Extension
      For Each Extension In Certificate.Extensions
         iIndex = iIndex + 1
         Wscript.Stdout.Writeline "Extension #" & CStr(iIndex) & ": " & Extension.OID.FriendlyName & "(" & Extension.OID.Value & ")"
         Wscript.Stdout.Writeline "  " & Extension.EncodedData.Format(CAPICOM_ENCODED_DATA_FORMAT_MULTI_LINES)
         
         If Not Extension.EncodedData.Decoder Is Nothing Then
            Select Case Extension.OID.Value
            Case CAPICOM_CERT_POLICIES_OID
               Dim CertPolicies
               Set CertPolicies = Extension.EncodedData.Decoder
               Wscript.Stdout.Writeline "Decoded Certificate Policies: " & CStr(CertPolicies.Count) & " PolicyInformation(s)"
               
               Dim pIndex : pIndex = 0
               Dim PolicyInformation
               For Each PolicyInformation In CertPolicies
                  pIndex = pIndex + 1
                  Wscript.Stdout.Writeline "  PolicyInformation #" & CStr(pIndex) & ": " & CStr(PolicyInformation.Qualifiers.Count) & " Qualifier(s)"
                  Wscript.Stdout.Writeline "    OID = " & PolicyInformation.OID.FriendlyName & "(" & PolicyInformation.OID.Value & ")"
                  Dim qIndex : qIndex = 0
                  Dim Qualifier
                  For Each Qualifier In PolicyInformation.Qualifiers
                     qIndex = qIndex + 1
                     Wscript.Stdout.Writeline "    Qualifier #" & CStr(qIndex) & ":"
                     Wscript.Stdout.Writeline "      OID               = " & Qualifier.OID.FriendlyName & "(" & Qualifier.OID.Value & ")"
                     Wscript.Stdout.Writeline "      CPS URI           = " & Qualifier.CPSPointer
                     Wscript.Stdout.Writeline "      Organization name = " & Qualifier.OrganizationName
                     Wscript.Stdout.Write     "      Notice number(s)  = "
                     If Not Qualifier.NoticeNumbers Is Nothing Then
                        Dim nIndex
                        For nIndex = 1 to Qualifier.NoticeNumbers.Count
                           If nIndex > 1 Then
                              Wscript.Stdout.Write ", "
                           End If
                           Wscript.Stdout.Write CStr(Qualifier.NoticeNumbers.Item(nIndex))
                        Next
                     End If
                     Wscript.Stdout.Writeline
                     Wscript.Stdout.Writeline "      Explicit text     = " & Qualifier.ExplicitText
                  Next
                  Wscript.Stdout.Writeline
               Next
               
            Case Else
               ' We don't have the decoder, so can't do much.
            End Select
         End If
      Next
      If iIndex = 0 Then
         Wscript.Stdout.Writeline "Extension: None."
         Wscript.Stdout.Writeline
      End If
      
      iIndex = 0
      Dim ExtendedProperty
      For Each ExtendedProperty In Certificate.ExtendedProperties
         iIndex = iIndex + 1
         Wscript.Stdout.Writeline "Property #" & CStr(iIndex) & " (ID = " & ExtendedProperty.PropID & "):" 
         Wscript.Stdout.Writeline "  " & ExtendedProperty.Value
      Next
      If iIndex = 0 Then
         Wscript.Stdout.Writeline "Property: None."
         Wscript.Stdout.Writeline
      End If
   Elseif VerboseLevel = UI Then
      ' Display the certificate UI.
      Certificate.Display
   End If    
   
   On Error Goto 0
   
End Sub ' End DisplayCertificate


'******************************************************************************
'
' Function  : IsReDimed
'
' Synopsis  : Check to see if an array has any element.
'
' Parameter : Array - array to check.
'
' Return    : True if contains element, else False.
'
'******************************************************************************

Function IsReDimed (Array)

   On Error Resume Next
   
   Dim i : i = LBound(Array)
   If Err.Number = 0 Then
      IsReDimed = True
   Else
      IsReDimed = False
   End If
   
   On Error Goto 0
   
End Function ' End IsReDimed

       
'******************************************************************************
'
' Subroutine: ParseCommandLine
'
' Synopsis  : Parse the command line, and set the options accordingly. Quit
'             with an exit code of either -1 or -2 if error is encountered.
'
' Parameter : None
'
'******************************************************************************

Sub ParseCommandLine

   ' Constants for command line parsing states.
   Const ARG_STATE_COMMAND       = 0
   Const ARG_STATE_OPTIONS       = 1
   Const ARG_STATE_LOCATION      = 2 
   Const ARG_STATE_STORENAME     = 3 
   Const ARG_STATE_CERTFILE      = 4  
   Const ARG_STATE_PASSWORD      = 5  
   Const ARG_STATE_SHA1          = 6 
   Const ARG_STATE_SUBJECT       = 7 
   Const ARG_STATE_ISSUER        = 8 
   Const ARG_STATE_ROOT          = 9 
   Const ARG_STATE_TEMPLATE      = 10
   Const ARG_STATE_EXTENSION     = 11
   Const ARG_STATE_PROPERTY      = 12
   Const ARG_STATE_USAGE         = 13
   Const ARG_STATE_POLICY        = 14
   Const ARG_STATE_TIME          = 15
   Const ARG_STATE_KEYUSAGE      = 16
   Const ARG_STATE_SAVEAS        = 17
   Const ARG_STATE_IGNORE_ERROR  = 18
   Const ARG_STATE_VERBOSE       = 19
   Const ARG_STATE_END           = 20
   
   ' Parse command line.
   Dim Arg
   Dim ArgState : ArgState = ARG_STATE_COMMAND

   For Each Arg In Wscript.Arguments
      Select Case ArgState
      Case ARG_STATE_COMMAND
         Select Case UCase(Arg) 
         Case "VIEW"
            Command = View
   
         Case "IMPORT"
            Command = Import
   
         Case "EXPORT"
            Command = Export
   
         Case "DELETE"
            Command = Delete
   
         Case "ARCHIVE"
            Command = Archive
   
         Case "ACTIVATE"
            OpenMode = OpenMode OR CAPICOM_STORE_OPEN_INCLUDE_ARCHIVED
            Command = Activate
   
         Case Else
            DisplayUsage
            
         End Select
         
         ArgState = ARG_STATE_OPTIONS

      Case ARG_STATE_OPTIONS
         Select Case UCase(Arg)
         Case "-L", "/L"
            ArgState = ARG_STATE_LOCATION
            
         Case "-S", "/S"
            ArgState = ARG_STATE_STORENAME
            
         Case "-CERTFILE", "/CERTFILE"
            ArgState = ARG_STATE_CERTFILE
            
         Case "-PWD", "/PWD"
            ArgState = ARG_STATE_PASSWORD
            
         Case "-A", "/A"
            OpenMode = OpenMode OR CAPICOM_STORE_OPEN_INCLUDE_ARCHIVED
            
         Case "-SHA1", "/SHA1"
            ArgState = ARG_STATE_SHA1
            
         Case "-SUBJECT", "/SUBJECT"
            ArgState = ARG_STATE_SUBJECT
            
         Case "-ISSUER", "/ISSUER"
            ArgState = ARG_STATE_ISSUER
            
         Case "-ROOT", "/ROOT"
            ArgState = ARG_STATE_ROOT
            
         Case "-TEMPLATE", "/TEMPLATE"
            ArgState = ARG_STATE_TEMPLATE
            
         Case "-EXTENSION", "/EXTENSION"
            ArgState = ARG_STATE_EXTENSION
            
         Case "-PROPERTY", "/PROPERTY"
            ArgState = ARG_STATE_PROPERTY
            
         Case "-EKU", "/EKU"
            ArgState = ARG_STATE_USAGE
            
         Case "-POLICY", "/POLICY"
            ArgState = ARG_STATE_POLICY
            
         Case "-TIME", "/TIME"
            ArgState = ARG_STATE_TIME
            
         Case "-KEYUSAGE", "/KEYUSAGE"
            ArgState = ARG_STATE_KEYUSAGE
            
         Case "-VALIDONLY", "/VALIDONLY"
            ValidOnly = True
            
         Case "-SAVEAS", "/SAVEAS"
            ArgState = ARG_STATE_SAVEAS
            
         Case "-DELKEY", "/DELKEY"
            DelKey = True
            
         Case "-NOPROMPT", "/NOPROMPT"
            NoPrompt = True
            
         Case "-IGNOREERROR", "/IGNOREERROR"
            ExportFlag = CAPICOM_EXPORT_IGNORE_PRIVATE_KEY_NOT_EXPORTABLE_ERROR
            
         Case "-E", "/E"
            KeyStorageFlag = KeyStorageFlag OR CAPICOM_KEY_STORAGE_EXPORTABLE
            
         Case "-P", "/P"
            KeyStorageFlag = KeyStorageFlag OR CAPICOM_KEY_STORAGE_USER_PROTECTED
            
         Case "-V", "/V"
            ArgState = ARG_STATE_VERBOSE
            
         Case "-?", "/?"
            DisplayUsage
            
         Case "~?"
            ExtendedHelp = True
            DisplayUsage
            
         Case Else
            If Left(Arg, 1) = "-"  OR Left(Arg, 1) = "/" Then
               DisplayUsage
            Else
               Select Case Command
               Case View, Delete, Archive, Activate
                  StoreName = UCase(Arg)
                  ArgState = ARG_STATE_END

               Case Else ' Import, Export
                  CertFile = Arg
                  ArgState = ARG_STATE_PASSWORD
               End Select
            End If
         End Select
         
      Case ARG_STATE_LOCATION
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            Select Case UCase(Arg)
            Case "CU"
               StoreLocation = CAPICOM_CURRENT_USER_STORE
               
            Case "LM"
               StoreLocation = CAPICOM_LOCAL_MACHINE_STORE
            
            Case "AD"
               StoreLocation = CAPICOM_ACTIVE_DIRECTORY_USER_STORE
      
            Case "SC"
               StoreLocation = CAPICOM_SMART_CARD_USER_STORE
      
            Case Else
               DisplayUsage
               
            End Select
         End If
         ArgState = ARG_STATE_OPTIONS
      
      Case ARG_STATE_STORENAME
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            StoreName = UCase(Arg)
         End If
         ArgState = ARG_STATE_OPTIONS
            
      Case ARG_STATE_CERTFILE
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            CertFile = Arg
         End If  
         ArgState = ARG_STATE_OPTIONS
      
      Case ARG_STATE_PASSWORD
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            Password = Arg
         End If  
         If Command = Import OR Command = Export Then
            ArgState = ARG_STATE_END
         Else
            ArgState = ARG_STATE_OPTIONS
         End If
      
      Case ARG_STATE_SHA1
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            SHA1 = Arg
         End If
         ArgState = ARG_STATE_OPTIONS
         
      Case ARG_STATE_SUBJECT
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            If IsReDimed(Subjects) Then
               ReDim Preserve Subjects(UBound(Subjects) + 1)
            Else
               ReDim Subjects(0)
            End If
            Subjects(UBound(Subjects)) = Arg
         End If
         ArgState = ARG_STATE_OPTIONS
         
      Case ARG_STATE_ISSUER
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            If IsReDimed(Issuers) Then
               ReDim Preserve Issuers(UBound(Issuers) + 1)
            Else
               ReDim Issuers(0)
            End If
            Issuers(UBound(Issuers)) = Arg
         End If
         ArgState = ARG_STATE_OPTIONS
         
      Case ARG_STATE_ROOT
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            If IsReDimed(Roots) Then
               ReDim Preserve Roots(UBound(Roots) + 1)
            Else
               ReDim Roots(0)
            End If
            Roots(UBound(Roots)) = Arg
         End If
         ArgState = ARG_STATE_OPTIONS
         
      Case ARG_STATE_TEMPLATE
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            If IsReDimed(Templates) Then
               ReDim Preserve Templates(UBound(Templates) + 1)
            Else
               ReDim Templates(0)
            End If
            Templates(UBound(Templates)) = Arg
         End If
         ArgState = ARG_STATE_OPTIONS
         
      Case ARG_STATE_EXTENSION
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            If IsReDimed(Extensions) Then
               ReDim Preserve Extensions(UBound(Extensions) + 1)
            Else
               ReDim Extensions(0)
            End If
            Extensions(UBound(Extensions)) = Arg
         End If
         ArgState = ARG_STATE_OPTIONS
         
      Case ARG_STATE_PROPERTY
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            If IsReDimed(Properties) Then
               ReDim Preserve Properties(UBound(Properties) + 1)
            Else
               ReDim Properties(0)
            End If
            If IsNumeric(Arg) Then
               Properties(UBound(Properties)) = CLng(Arg)
            Else
               DisplayUsage
            End If
         End If
         ArgState = ARG_STATE_OPTIONS
         
      Case ARG_STATE_USAGE
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            If IsReDimed(Usages) Then
               ReDim Preserve Usages(UBound(Usages) + 1)
            Else
               ReDim Usages(0)
            End If
            Usages(UBound(Usages)) = Arg
         End If
         ArgState = ARG_STATE_OPTIONS
         
      Case ARG_STATE_POLICY
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            If IsReDimed(Policies) Then
               ReDim Preserve Policies(UBound(Policies) + 1)
            Else
               ReDim Policies(0)
            End If
            Policies(UBound(Policies)) = Arg
         End If
         ArgState = ARG_STATE_OPTIONS
      
      Case ARG_STATE_TIME
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            If IsReDimed(Times) Then
               ReDim Preserve Times(UBound(Times) + 1)
            Else
               ReDim Times(0)
            End If
            
            Select Case Arg
            Case "-1"
               Times(UBound(Times)) = CAPICOM_CERTIFICATE_FIND_TIME_NOT_YET_VALID

            Case "0"
               Times(UBound(Times)) = CAPICOM_CERTIFICATE_FIND_TIME_VALID

            Case "1"
               Times(UBound(Times)) = CAPICOM_CERTIFICATE_FIND_TIME_EXPIRED

            Case Else
               DisplayUsage

            End Select
         End If
         ArgState = ARG_STATE_OPTIONS
      
      Case ARG_STATE_KEYUSAGE
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            If IsReDimed(KeyUsages) Then
               ReDim Preserve KeyUsages(UBound(KeyUsages) + 1)
            Else
               ReDim KeyUsages(0)
            End If
            If IsNumeric(Arg) Then
               KeyUsages(UBound(KeyUsages)) = CLng(Arg)
            Else
               KeyUsages(UBound(KeyUsages)) = Arg
            End If
         End If
         ArgState = ARG_STATE_OPTIONS
      
      Case ARG_STATE_SAVEAS
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            Select Case UCase(Arg)
               Case "SST"
                  SaveAs = CAPICOM_STORE_SAVE_AS_SERIALIZED
                  
               Case "PKCS7"
                  SaveAs = CAPICOM_STORE_SAVE_AS_PKCS7
            
               Case "PFX"
                  SaveAs = CAPICOM_STORE_SAVE_AS_PFX
            
               Case Else
                  DisplayUsage
                  
            End Select
         End If
         ArgState = ARG_STATE_OPTIONS
   
      Case ARG_STATE_VERBOSE
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            VerboseLevel = CLng(Arg)
            If VerboseLevel > UI Then
               DisplayUsage
            End If
         End If
         ArgState = ARG_STATE_OPTIONS
      
      Case Else       
         DisplayUsage
         
      End Select
   Next
   
   ' Make sure we are in good state.
   If ArgState <> ARG_STATE_OPTIONS AND ArgState <> ARG_STATE_PASSWORD AND ArgState <> ARG_STATE_END Then
      DisplayUsage
   End If

   ' Make sure all required options are valid.
   ' Note: As stated in the help screen, non-fatal invalid options for
   '       the specific command is ignore. You can add the logic here
   '       to further handle these invalid options if desired.
   Select Case Command
   Case View
      ' -l and -certfile are exclusive. 
      If Not IsNull(CertFile) Then
        If Not IsNull(StoreLocation) Then
           DisplayUsage
        Else
           StoreName = CertFile
           StoreLocation = CAPICOM_MEMORY_STORE
        End If
      End If
      
   Case Import
      ' Make sure we do have a certificate file name. 
      If IsNull(CertFile) Then
         DisplayUsage
      End If
   
      ' -validonly option not allowed.
      If ValidOnly = True Then
         DisplayUsage
      End If

   Case Export
      ' Make sure we do have a certificate file name. 
      If IsNull(CertFile) Then
         DisplayUsage
      End If
   
   Case Delete, Archive
      ' -certfile option not allowed.
      If Not IsNull(CertFile) Then
         DisplayUsage
      End If
      
      ' -validonly option not allowed.
      If ValidOnly = True Then
         DisplayUsage
      End If

   Case Activate
      ' -certfile option not allowed.
      If Not IsNull(CertFile) Then
         DisplayUsage
      End If

   Case Else      
      Wscript.Stdout.Writeline "Internal script error: Unknown command state (Command = " & CStr(Command) & ")."
      Wscript.Quit(-2)
      
   End Select

   ' Set default store location if both -l and -certfile are not specified.
   If IsNull(StoreLocation) Then
      StoreLocation = CAPICOM_CURRENT_USER_STORE
   End If      
      
End Sub ' ParseCommandLine
       

'******************************************************************************
'
' Subroutine: DisplayUsage
'
' Synopsis  : Display the usage screen, and then exit with a negative error 
'             code.
'
' Parameter : None.
'
' Remark    : Display example usages if the global variable ExtendedHelp is set
'             to True.
'
'******************************************************************************

Sub DisplayUsage

   Select Case Command
      Case Unknown
         Wscript.Stdout.Writeline "Usage: CStore Command [Options] <[Store] | CertFile [Password]>"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Command:"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  View                      -- View certificate(s) of store or file"
         Wscript.Stdout.Writeline "  Import                    -- Import certificate(s) from file to store"
         Wscript.Stdout.Writeline "  Export                    -- Export certificate(s) from store to file"
         Wscript.Stdout.Writeline "  Delete                    -- Delete certificate(s) from store"
         Wscript.Stdout.Writeline "  Archive                   -- Archive certificate(s) in store"
         Wscript.Stdout.Writeline "  Activate                  -- Activate (de-archive) certificate(s) in store"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "For help on a specific command, enter ""CStore Command -?"""

      Case View
         Wscript.Stdout.Writeline "Usage: CStore View [Options] [Store]" 
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The View command is used to view certificate(s) of a certificate store or file."
         Wscript.Stdout.Writeline "You can use the filtering option(s) to narrow down the set of certificate(s) to"
         Wscript.Stdout.Writeline "be displayed."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Options:" 
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  -l         <location>      -- CU, LM, AD, or SC (default to CU)"
         Wscript.Stdout.Writeline "  -certfile  <certfile>      -- Certificate file, CER, SST, P7B, PFX,"
         Wscript.Stdout.Writeline "                                etc. (exclusive with -l)"
         Wscript.Stdout.Writeline "  -pwd       <password>      -- Password for the PFX file (requires -certfile)."
         Wscript.Stdout.Writeline "  -a                         -- Include archived certificates"
         Wscript.Stdout.Writeline "  -sha1      <hash>          -- SHA1 hash of the signing certificate"
         Wscript.Stdout.Writeline "  -subject   <name>          ** Subject Name of the signing certificate must"
         Wscript.Stdout.Writeline "                                contain this name"
         Wscript.Stdout.Writeline "  -issuer    <name>          ** Issuer Name of the signing certificate must"
         Wscript.Stdout.Writeline "                                contain this name"
         Wscript.Stdout.Writeline "  -root      <name>          ** Subject Name of the root certificate must"
         Wscript.Stdout.Writeline "                                contain this name"
         Wscript.Stdout.Writeline "  -template  <name | oid>    ** Template name or OID"
         Wscript.Stdout.Writeline "  -extension <name | oid>    ** Extension name or OID"
         Wscript.Stdout.Writeline "  -property  <id>            ** Property ID"
         Wscript.Stdout.Writeline "  -eku       <name | oid>    ** EKU name or OID"
         Wscript.Stdout.Writeline "  -policy    <name | oid>    ** Certificate policy name or OID"
         Wscript.Stdout.Writeline "  -time      <-1 | 0 | 1>    ** Time validity, -1 for not yet valid, 0 for"
         Wscript.Stdout.Writeline "                                valid, 1 for expired (default to all)"
         Wscript.Stdout.Writeline "  -keyusage  <key usage>     ** Key usage bit flag or name"
         Wscript.Stdout.Writeline "  -validonly                 -- Display valid certificates only."
         Wscript.Stdout.Writeline "  -v         <level>         -- Verbose level, 0 for normal, 1 for detail"
         Wscript.Stdout.Writeline "                                2 for UI mode (default to level 0)"
         Wscript.Stdout.Writeline "  -?                         -- This help screen"
         Wscript.Stdout.Writeline                              
         Wscript.Stdout.Writeline "  Store                      -- My, CA, AddressBook, Root, etc. (default to My)"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Note: All non-fatal invalid options for this specific command will be ignored,"
         Wscript.Stdout.Writeline "      and the ** symbol indicates option can be listed multiple times."
         Wscript.Stdout.Writeline
         If ExtendedHelp Then
            Wscript.Stdout.Writeline "Examples:"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -a -validonly ca" 
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -l lm root" 
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -time 1 -v 2 addressbook" 
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -l ad ""cn=john smith""" 
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -v 2 -l sc" 
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -certfile mystore.sst" 
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -certfile mypfx.pfx -pwd mypwd" 
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -sha1 277969B46F5603AD7719F63AC66EF0179CCD9E47"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -l lm -subject john -subject smith -root microsoft -root developer"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -template AutoEnrollSmartcardUser"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -template 1.3.6.1.4.1.311.21.8.3692315854.1256661383.1690418588.4201632533.2654958950.1091409178"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -extension ""Application Policies"""
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -extension 1.3.6.1.4.1.311.21.10"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -extension ""Application Policies"" -extension ""Certificate Policies"""
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -property 2"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -eku ""Code Signing"""
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -eku 1.3.6.1.5.5.7.3.3 -validonly"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -policy ""Medium Assurance"""
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -policy 1.3.6.1.4.1.311.21.8.3692315854.1256661383.1690418588.4201632533.1.401"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore view -keyusage ""&H00000080"" -keyusage DecipherOnly"
         End If
   
      Case Import
         Wscript.Stdout.Writeline "Usage: CStore Import [Options] CertFile [Password]" 
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The Import command is used to import certificate(s) from a certificate file"
         Wscript.Stdout.Writeline "(.CER, .SST, .P7B, .PFX, etc.) to a store. You can use the filtering option(s)"
         Wscript.Stdout.Writeline "to narrow down the set of certificate(s) to be imported."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Options:" 
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  -l <location>              -- CU or LM (default to CU)"
         Wscript.Stdout.Writeline "  -s <store>                 -- My, CA, AddressBook, Root, etc. (default to My)"
         Wscript.Stdout.Writeline "  -e                         -- Mark private key as exportable (PFX only)"
         Wscript.Stdout.Writeline "  -p                         -- Mark private key as user protected (PFX only)"
         Wscript.Stdout.Writeline "                                Note: The DPAPI dialog will be displayed"
         Wscript.Stdout.Writeline "  -v         <level>         -- Verbose level, 0 for normal, 1 for detail"
         Wscript.Stdout.Writeline "                                2 for UI mode (default to level 0)"
         Wscript.Stdout.Writeline "  -?                         -- This help screen"
         Wscript.Stdout.Writeline                               
         Wscript.Stdout.Writeline "  CertFile                   -- Certificate file to be imported"
         Wscript.Stdout.Writeline                               
         Wscript.Stdout.Writeline "  Password                   -- Password for PFX file"
         Wscript.Stdout.Writeline                               
         Wscript.Stdout.Writeline "Note: All non-fatal invalid options for this specific command will be ignored."
         Wscript.Stdout.Writeline
         If ExtendedHelp Then
            Wscript.Stdout.Writeline "Examples:"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore import mycer.cer"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore import mysst.sst"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore import -s root myroot.cer"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore import -l lm -s root -v myroot.cer"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore import -v 2 myp7b.p7b"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore import -e -p mypfx.pfx mypwd"
            Wscript.Stdout.Writeline
         End If

      Case Export
         Wscript.Stdout.Writeline "Usage: CStore Export [Options] CertFile [Password]>" 
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The Export command is used to export certificate(s) from a certificate store to"
         Wscript.Stdout.Writeline "file (.SST, .P7B, or .PFX). You can use the filtering option(s) to narrow down"
         Wscript.Stdout.Writeline "the set of certificate(s) to be exported."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Options:" 
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  -l         <location>      -- CU, LM, AD, or SC (default to CU)"
         Wscript.Stdout.Writeline "  -s         <store>         -- My, CA, AddressBook, Root, etc. (default to My)"
         Wscript.Stdout.Writeline "  -a                         -- Include archived certificates"
         Wscript.Stdout.Writeline "  -sha1      <hash>          -- SHA1 hash of the signing certificate"
         Wscript.Stdout.Writeline "  -subject   <name>          ** Subject Name of the signing certificate must"
         Wscript.Stdout.Writeline "                                contain this name"
         Wscript.Stdout.Writeline "  -issuer    <name>          ** Issuer Name of the signing certificate must"
         Wscript.Stdout.Writeline "                                contain this name"
         Wscript.Stdout.Writeline "  -root      <name>          ** Subject Name of the root certificate must"
         Wscript.Stdout.Writeline "                                contain this name"
         Wscript.Stdout.Writeline "  -template  <name | oid>    ** Template name or OID"
         Wscript.Stdout.Writeline "  -extension <name | oid>    ** Extension name or OID"
         Wscript.Stdout.Writeline "  -property  <id>            ** Property ID"
         Wscript.Stdout.Writeline "  -eku       <name | oid>    ** EKU name or OID"
         Wscript.Stdout.Writeline "  -policy    <name | oid>    ** Certificate policy name or OID"
         Wscript.Stdout.Writeline "  -time      <-1 | 0 | 1>    ** Time validity, -1 for not yet valid, 0 for"
         Wscript.Stdout.Writeline "                                valid, 1 for expired (default to all)"
         Wscript.Stdout.Writeline "  -keyusage  <key usage>     ** Key usage bit flag or name"
         Wscript.Stdout.Writeline "  -validonly                 -- Export valid certificates only."
         Wscript.Stdout.Writeline "  -saveas    <type>          -- SST, PKCS7, or PFX (default to SST)"
         Wscript.Stdout.Writeline "  -ignoreerror               -- Ignore private key not exportable error for PFX"
         Wscript.Stdout.Writeline "  -v         <level>         -- Verbose level, 0 for normal, 1 for detail"
         Wscript.Stdout.Writeline "                                2 for UI mode (default to level 0)"
         Wscript.Stdout.Writeline "  -?                         -- This help screen"
         Wscript.Stdout.Writeline                               
         Wscript.Stdout.Writeline "  CertFile                   -- Certificate file to be exported to"
         Wscript.Stdout.Writeline                               
         Wscript.Stdout.Writeline "  Password                   -- Password for PFX file"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Note: All non-fatal invalid options for this specific command will be ignored,"
         Wscript.Stdout.Writeline "      and the ** symbol indicates option can be listed multiple times."
         Wscript.Stdout.Writeline
         If ExtendedHelp Then
            Wscript.Stdout.Writeline "Examples:"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore export cumystore.sst"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore export -l lm -subject marketing lmmy.sst"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore export -saveas pfx -property 2 lmmy.pfx mypwd"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore export -s ca -root microsoft -v cuca.sst"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore export -l sc -validonly -v 2 scmy.sst"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore export -l lm -s root -saveas pkcs7 lmroot.p7b"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore export -l ad -s ""cn=john smith"" adjs.sst"
            Wscript.Stdout.Writeline
         End If
   
      Case Delete
         Wscript.Stdout.Writeline "Usage: CStore Delete [Options] [Store]" 
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The Delete command is used to delete certificate(s) from a certificate store."
         Wscript.Stdout.Writeline "You can use the filtering option(s) to narrow down the set of certificate(s) to"
         Wscript.Stdout.Writeline "be deleted."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Options:" 
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  -l         <location>      -- CU or LM (default to CU)"
         Wscript.Stdout.Writeline "  -a                         -- Include archived certificates"
         Wscript.Stdout.Writeline "  -sha1      <hash>          -- SHA1 hash of the signing certificate"
         Wscript.Stdout.Writeline "  -subject   <name>          ** Subject Name of the signing certificate must"
         Wscript.Stdout.Writeline "                                contain this name"
         Wscript.Stdout.Writeline "  -issuer    <name>          ** Issuer Name of the signing certificate must"
         Wscript.Stdout.Writeline "                                contain this name"
         Wscript.Stdout.Writeline "  -root      <name>          ** Subject Name of the root certificate must"
         Wscript.Stdout.Writeline "                                contain this name"
         Wscript.Stdout.Writeline "  -template  <name | oid>    ** Template name or OID"
         Wscript.Stdout.Writeline "  -extension <name | oid>    ** Extension name or OID"
         Wscript.Stdout.Writeline "  -property  <id>            ** Property ID"
         Wscript.Stdout.Writeline "  -eku       <name | oid>    ** EKU name or OID"
         Wscript.Stdout.Writeline "  -policy    <name | oid>    ** Certificate policy name or OID"
         Wscript.Stdout.Writeline "  -time      <-1 | 0 | 1>    ** Time validity, -1 for not yet valid, 0 for"
         Wscript.Stdout.Writeline "                                valid, 1 for expired (default to all)"
         Wscript.Stdout.Writeline "  -keyusage  <key usage>     ** Key usage bit flag or name"
         Wscript.Stdout.Writeline "  -delkey                    -- Delete key container if exists"
         Wscript.Stdout.Writeline "  -noprompt                  -- Do not prompt (always delete)"
         Wscript.Stdout.Writeline "  -v         <level>         -- Verbose level, 0 for normal, 1 for detail"
         Wscript.Stdout.Writeline "                                2 for UI mode (default to level 0)"
         Wscript.Stdout.Writeline "  -?                         -- This help screen"
         Wscript.Stdout.Writeline                              
         Wscript.Stdout.Writeline "  Store                      -- My, CA, AddressBook, Root, etc. (default to My)"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Note: All non-fatal invalid options for this specific command will be ignored,"
         Wscript.Stdout.Writeline "      and the ** symbol indicates option can be listed multiple times."
         Wscript.Stdout.Writeline
         If ExtendedHelp Then
            Wscript.Stdout.Writeline "Examples:"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore delete"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore delete -l lm -delkey"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore delete -noprompt addressbook"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore delete -v 2 -time 1 -delkey -noprompt"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore delete -eku ""secure email"" -eku ""client authentication"" ca"
            Wscript.Stdout.Writeline
         End If

      Case Archive
         Wscript.Stdout.Writeline "Usage: CStore Archive [Options] [Store]" 
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The Archive command is used to archive certificate(s) in a certificate store."
         Wscript.Stdout.Writeline "You can use the filtering option(s) to narrow down the set of certificate(s) to"
         Wscript.Stdout.Writeline "be archived."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Options:" 
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  -l         <location>      -- CU or LM (default to CU)"
         Wscript.Stdout.Writeline "  -sha1      <hash>          -- SHA1 hash of the signing certificate"
         Wscript.Stdout.Writeline "  -subject   <name>          ** Subject Name of the signing certificate must"
         Wscript.Stdout.Writeline "                                contain this name"
         Wscript.Stdout.Writeline "  -issuer    <name>          ** Issuer Name of the signing certificate must"
         Wscript.Stdout.Writeline "                                contain this name"
         Wscript.Stdout.Writeline "  -root      <name>          ** Subject Name of the root certificate must"
         Wscript.Stdout.Writeline "                                contain this name"
         Wscript.Stdout.Writeline "  -template  <name | oid>    ** Template name or OID"
         Wscript.Stdout.Writeline "  -extension <name | oid>    ** Extension name or OID"
         Wscript.Stdout.Writeline "  -property  <id>            ** Property ID"
         Wscript.Stdout.Writeline "  -eku       <name | oid>    ** EKU name or OID"
         Wscript.Stdout.Writeline "  -policy    <name | oid>    ** Certificate policy name or OID"
         Wscript.Stdout.Writeline "  -time      <-1 | 0 | 1>    ** Time validity, -1 for not yet valid, 0 for"
         Wscript.Stdout.Writeline "                                valid, 1 for expired (default to all)"
         Wscript.Stdout.Writeline "  -keyusage  <key usage>     ** Key usage bit flag or name"
         Wscript.Stdout.Writeline "  -noprompt                  -- Do not prompt (always archive)"
         Wscript.Stdout.Writeline "  -v         <level>         -- Verbose level, 0 for normal, 1 for detail"
         Wscript.Stdout.Writeline "                                2 for UI mode (default to level 0)"
         Wscript.Stdout.Writeline "  -?                         -- This help screen"
         Wscript.Stdout.Writeline                              
         Wscript.Stdout.Writeline "  Store                      -- My, CA, AddressBook, Root, etc. (default to My)"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Note: All non-fatal invalid options for this specific command will be ignored,"
         Wscript.Stdout.Writeline "      and the ** symbol indicates option can be listed multiple times."
         Wscript.Stdout.Writeline
         If ExtendedHelp Then
            Wscript.Stdout.Writeline "Examples:"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore archive"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore archive -time 1"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore archive -l lm addressbook"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore archive -noprompt -subject ""john smith"" addressbook"
            Wscript.Stdout.Writeline
         End If

      Case Activate
         Wscript.Stdout.Writeline "Usage: CStore Activate [Options] [Store]" 
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The Activate command is used to activate archived certificate(s) in a"
         Wscript.Stdout.Writeline "certificate store. You can use the filtering option(s) to narrow down the set"
         Wscript.Stdout.Writeline "of certificate(s) to be activated (de-archived)."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Options:" 
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  -l         <location>      -- CU or LM (default to CU)"
         Wscript.Stdout.Writeline "  -sha1      <hash>          -- SHA1 hash of the signing certificate"
         Wscript.Stdout.Writeline "  -subject   <name>          ** Subject Name of the signing certificate must"
         Wscript.Stdout.Writeline "                                contain this name"
         Wscript.Stdout.Writeline "  -issuer    <name>          ** Issuer Name of the signing certificate must"
         Wscript.Stdout.Writeline "                                contain this name"
         Wscript.Stdout.Writeline "  -root      <name>          ** Subject Name of the root certificate must"
         Wscript.Stdout.Writeline "                                contain this name"
         Wscript.Stdout.Writeline "  -template  <name | oid>    ** Template name or OID"
         Wscript.Stdout.Writeline "  -extension <name | oid>    ** Extension name or OID"
         Wscript.Stdout.Writeline "  -property  <id>            ** Property ID"
         Wscript.Stdout.Writeline "  -eku       <name | oid>    ** EKU name or OID"
         Wscript.Stdout.Writeline "  -policy    <name | oid>    ** Certificate policy name or OID"
         Wscript.Stdout.Writeline "  -time      <-1 | 0 | 1>    ** Time validity, -1 for not yet valid, 0 for"
         Wscript.Stdout.Writeline "                                valid, 1 for expired (default to all)"
         Wscript.Stdout.Writeline "  -keyusage  <key usage>     ** Key usage bit flag or name"
         Wscript.Stdout.Writeline "  -validonly                 -- Activate valid certificates only."
         Wscript.Stdout.Writeline "  -noprompt                  -- Do not prompt (always activate)"
         Wscript.Stdout.Writeline "  -v         <level>         -- Verbose level, 0 for normal, 1 for detail"
         Wscript.Stdout.Writeline "                                2 for UI mode (default to level 0)"
         Wscript.Stdout.Writeline "  -?                         -- This help screen"
         Wscript.Stdout.Writeline                              
         Wscript.Stdout.Writeline "  Store                      -- My, CA, AddressBook, Root, etc. (default to My)"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Note: All non-fatal invalid options for this specific command will be ignored,"
         Wscript.Stdout.Writeline "      and the ** symbol indicates option can be listed multiple times."
         Wscript.Stdout.Writeline
         If ExtendedHelp Then
            Wscript.Stdout.Writeline "Examples:"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore activate"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore activate -property 2 -time 0"
            Wscript.Stdout.Writeline
            Wscript.Stdout.Writeline "  cstore activate -l lm -validonly -noprompt addressbook"
            Wscript.Stdout.Writeline
         End If

      Case Else
         Wscript.Stdout.Writeline "Internal script error: Unknown help state (Command = " & CStr(Command) & ")."
   
   End Select
   
   Wscript.Quit(-1)
   
End Sub ' End DisplayUsage

