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
' CView.vbs
'
' This is a sample script to illustrate how to use features introduced in
' CAPICOM v2.0 to display certificate, optionally including the chain, from a 
' CER or PFX file.
'
' Note: For simplicity, this script does not handle exception.
'
'******************************************************************************

Option Explicit

' Chain check flag.                                
Const CAPICOM_CHECK_NONE                              = &H00000000
Const CAPICOM_CHECK_TRUSTED_ROOT                      = &H00000001
Const CAPICOM_CHECK_TIME_VALIDITY                     = &H00000002
Const CAPICOM_CHECK_SIGNATURE_VALIDITY                = &H00000004
Const CAPICOM_CHECK_ONLINE_REVOCATION_STATUS          = &H00000008
Const CAPICOM_CHECK_OFFLINE_REVOCATION_STATUS         = &H00000010
Const CAPICOM_CHECK_COMPLETE_CHAIN                    = &H00000020
Const CAPICOM_CHECK_NAME_CONSTRAINTS                  = &H00000040
Const CAPICOM_CHECK_BASIC_CONSTRAINTS                 = &H00000080
Const CAPICOM_CHECK_NESTED_VALIDITY_PERIOD            = &H00000100
Const CAPICOM_CHECK_ONLINE_ALL                        = &H000001EF
Const CAPICOM_CHECK_OFFLINE_ALL                       = &H000001F7

' Chain status codes.
Const CAPICOM_TRUST_IS_NOT_TIME_VALID                 = &H00000001
Const CAPICOM_TRUST_IS_NOT_TIME_NESTED                = &H00000002
Const CAPICOM_TRUST_IS_REVOKED                        = &H00000004
Const CAPICOM_TRUST_IS_NOT_SIGNATURE_VALID            = &H00000008
Const CAPICOM_TRUST_IS_NOT_VALID_FOR_USAGE            = &H00000010
Const CAPICOM_TRUST_IS_UNTRUSTED_ROOT                 = &H00000020
Const CAPICOM_TRUST_REVOCATION_STATUS_UNKNOWN         = &H00000040
Const CAPICOM_TRUST_IS_CYCLIC                         = &H00000080
Const CAPICOM_TRUST_INVALID_EXTENSION                 = &H00000100
Const CAPICOM_TRUST_INVALID_POLICY_CONSTRAINTS        = &H00000200
Const CAPICOM_TRUST_INVALID_BASIC_CONSTRAINTS         = &H00000400
Const CAPICOM_TRUST_INVALID_NAME_CONSTRAINTS          = &H00000800
Const CAPICOM_TRUST_HAS_NOT_SUPPORTED_NAME_CONSTRAINT = &H00001000
Const CAPICOM_TRUST_HAS_NOT_DEFINED_NAME_CONSTRAINT   = &H00002000
Const CAPICOM_TRUST_HAS_NOT_PERMITTED_NAME_CONSTRAINT = &H00004000
Const CAPICOM_TRUST_HAS_EXCLUDED_NAME_CONSTRAINT      = &H00008000
Const CAPICOM_TRUST_IS_OFFLINE_REVOCATION             = &H01000000
Const CAPICOM_TRUST_NO_ISSUANCE_CHAIN_POLICY          = &H02000000
Const CAPICOM_TRUST_IS_PARTIAL_CHAIN                  = &H00010000
Const CAPICOM_TRUST_CTL_IS_NOT_TIME_VALID             = &H00020000
Const CAPICOM_TRUST_CTL_IS_NOT_SIGNATURE_VALID        = &H00040000
Const CAPICOM_TRUST_CTL_IS_NOT_VALID_FOR_USAGE        = &H00080000
Const KNOWN_TRUST_STATUS_MASK                         = &H030FFFFF

' Command line arguments
Dim CertFile            : CertFile              = NULL
Dim VerificationTime    : VerificationTime      = NULL
Dim UrlRetrievalTimeout : UrlRetrievalTimeout   = NULL
Dim Password            : Password              = ""
Dim CheckFlag           : CheckFlag             = NULL
Dim CompleteChain       : CompleteChain         = FALSE
Dim Verbose             : Verbose               = FALSE
Dim ExtendedHelp        : ExtendedHelp          = False

' Chain policies (can be multiples).        
Dim Usages()
Dim Policies()

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

' Load the certificate.
Dim Certificate
Set Certificate = CreateObject("CAPICOM.Certificate")
Certificate.Load CertFile, Password

' Create the chain object.
Dim Chain
Set Chain = CreateObject("CAPICOM.Chain")

' Set check flag.
If Not IsNull(CheckFlag) Then
   Certificate.IsValid.CheckFlag = CheckFlag
End If

' Set chain building application usages as necesary.
Dim OID
Dim strOid
Dim Index
If IsReDimed(Usages) Then
   For Index = LBound(Usages) To UBound(Usages) 
      Set OID = CreateObject("CAPICOM.OID")
      ' See if this is a dotted OID.
      If InStr(Usages(Index), ".") Then
         ' It is a dotted OID.
         '
         ' Note: For known OIDs, this will also reset
         '       OID.Name and OID.FriendlyName.
         OID.Value = Usages(Index)
      Else
         ' Note: For known OIDs, this will also reset
         '       OID.Name and OID.Value.
         OID.FriendlyName = Usages(Index)
      End If
      
      ' Make sure the OID.Value is indeed available, otherwise ignore it.
      If Len(OID.Value) > 0 Then
         Certificate.IsValid.ApplicationPolicies.Add OID
      End If
      Set OID = Nothing
   Next
End If

' Set chain building certificate policies as necessary.
If IsReDimed(Policies) Then
   For Index = LBound(Policies) To UBound(Policies) 
      Set OID = CreateObject("CAPICOM.OID")
      ' See if this is a dotted OID.
      If InStr(Policies(Index), ".") Then
         ' It is a dotted OID.
         '
         ' Note: For known OIDs, this will also reset
         '       OID.Name and OID.FriendlyName.
         OID.Value = Policies(Index)
      Else
         ' Note: For known OIDs, this will also reset
         '       OID.Name and OID.Value.
         OID.FriendlyName = Policies(Index)
      End If
      
      ' Make sure the OID.Value is indeed available, otherwise ignore it.
      If Len(OID.Value) > 0 Then
         Certificate.IsValid.CertificatePolicies.Add OID
      End If
      Set OID = Nothing
   Next
End If

' Set verification time as necessary.
If Not IsNull(VerificationTime) Then
   Certificate.IsValid.VerificationTime = VerificationTime
End If

' Set CRL retrieval timeout value as necessary.
If Not IsNull(UrlRetrievalTimeout) Then
   Certificate.IsValid.UrlRetrievalTimeout = UrlRetrievalTimeout
End If

' Build the chain.
If Chain.Build(Certificate) Then
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "The overall chain status is valid."
Else
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "The overall chain status is not valid."
   Wscript.Stdout.Writeline "Overall chain status code  = 0x" & Hex(Chain.Status) & " (" & GetStatusString(Chain.Status) & ")"
   Wscript.Stdout.Writeline "Extended error information = " & Chain.ExtendedErrorInfo
End If
Wscript.Stdout.Writeline
   
' Display the entire chain, if requested.
If Not IsNull(CheckFlag) Then
   Dim cIndex
   For cIndex = 1 to Chain.Certificates.Count 
      ' Display the current certificate.
      DisplayCertificate Chain, cIndex
      
      ' Note: When we load a PFX file, a key container is always created for any 
      '       certificate with private key associated. So we must manually delete 
      '       the container to ensure the private key will not remain on the
      '       system and become "orphaned".
      '
      ' Delete the key if available.
      If cIndex = 1 AND Certificate.HasPrivateKey Then
         Certificate.PrivateKey.Delete
      End If
   Next
Else
   ' Display only the end certificate.
   DisplayCertificate Chain, 1

   ' Note: When we load a PFX file, a key container is always created for any 
   '       certificate with private key associated. So we must manually delete 
   '       the container to ensure the private key will not remain on the
   '       system and become "orphaned".
   '
   ' Delete the key if available.
   If Certificate.HasPrivateKey Then
      Certificate.PrivateKey.Delete
   End If
End If

' Release resources.
Set Chain = Nothing
Set Certificate = Nothing

' We are all done.
Wscript.Quit(0)

' End Main

'******************************************************************************
'
' Function: GetStatusString
'
' Synopsis  : Return status string(s) of the specified status code.
'
' Parameter : Status - Status code.
'
'******************************************************************************

Function GetStatusString (Status)

   Dim StatusCodes(21)
   Dim ErrorStrings(21)
   
   StatusCodes(0)  = CAPICOM_TRUST_IS_NOT_TIME_VALID
   StatusCodes(1)  = CAPICOM_TRUST_IS_NOT_TIME_NESTED
   StatusCodes(2)  = CAPICOM_TRUST_IS_REVOKED
   StatusCodes(3)  = CAPICOM_TRUST_IS_NOT_SIGNATURE_VALID            
   StatusCodes(4)  = CAPICOM_TRUST_IS_NOT_VALID_FOR_USAGE            
   StatusCodes(5)  = CAPICOM_TRUST_IS_UNTRUSTED_ROOT
   StatusCodes(6)  = CAPICOM_TRUST_REVOCATION_STATUS_UNKNOWN
   StatusCodes(7)  = CAPICOM_TRUST_IS_CYCLIC
   StatusCodes(8)  = CAPICOM_TRUST_INVALID_EXTENSION
   StatusCodes(9)  = CAPICOM_TRUST_INVALID_POLICY_CONSTRAINTS
   StatusCodes(10) = CAPICOM_TRUST_INVALID_BASIC_CONSTRAINTS
   StatusCodes(11) = CAPICOM_TRUST_INVALID_NAME_CONSTRAINTS
   StatusCodes(12) = CAPICOM_TRUST_HAS_NOT_SUPPORTED_NAME_CONSTRAINT
   StatusCodes(13) = CAPICOM_TRUST_HAS_NOT_DEFINED_NAME_CONSTRAINT
   StatusCodes(14) = CAPICOM_TRUST_HAS_NOT_PERMITTED_NAME_CONSTRAINT
   StatusCodes(15) = CAPICOM_TRUST_HAS_EXCLUDED_NAME_CONSTRAINT
   StatusCodes(16) = CAPICOM_TRUST_IS_OFFLINE_REVOCATION
   StatusCodes(17) = CAPICOM_TRUST_NO_ISSUANCE_CHAIN_POLICY
   StatusCodes(18) = CAPICOM_TRUST_IS_PARTIAL_CHAIN
   StatusCodes(19) = CAPICOM_TRUST_CTL_IS_NOT_TIME_VALID
   StatusCodes(20) = CAPICOM_TRUST_CTL_IS_NOT_SIGNATURE_VALID
   StatusCodes(21) = CAPICOM_TRUST_CTL_IS_NOT_VALID_FOR_USAGE
   
   ErrorStrings(0)  = "Invalid time"
   ErrorStrings(1)  = "Time not nested"
   ErrorStrings(2)  = "Revoked"
   ErrorStrings(3)  = "Invalid signature"
   ErrorStrings(4)  = "Invalid usage"
   ErrorStrings(5)  = "Untrusted root"
   ErrorStrings(6)  = "Unknown revocation"
   ErrorStrings(7)  = "Cyclic chain"
   ErrorStrings(8)  = "Invalid extension"
   ErrorStrings(9)  = "Invalid policy constraints"
   ErrorStrings(10) = "Invalid basic constraints"
   ErrorStrings(11) = "Invalid name constraints"
   ErrorStrings(12) = "Not supported name constraint"
   ErrorStrings(13) = "Not defined name constraint"
   ErrorStrings(14) = "Not permitted name constraint"
   ErrorStrings(15) = "Has excluded name constraint"
   ErrorStrings(16) = "Offline revocation"
   ErrorStrings(17) = "No issuance chain policy"
   ErrorStrings(18) = "Partial chain"
   ErrorStrings(19) = "CTL invalid time"
   ErrorStrings(20) = "CTL invalid signature"
   ErrorStrings(21) = "CTL invalid usage"
   
   ' Initialize.
   GetStatusString = ""

   Dim sIndex
   For sIndex = LBound(StatusCodes) To UBound(StatusCodes)
      ' Check status.
      If (Status And StatusCodes(sIndex)) = StatusCodes(sIndex) Then
         If Len(GetStatusString) > 0 Then
            GetStatusString = GetStatusString & ", "
         End If
         GetStatusString = GetStatusString & ErrorStrings(sIndex)
      End If
   Next
   
   ' See if we have any unknown trust status.
   If (Status And Not KNOWN_TRUST_STATUS_MASK) <> 0 Then
      If Len(GetStatusString) > 0 Then
         GetStatusString = GetStatusString & ", "
      End If
      GetStatusString = GetStatusString & "Unknown trust error"
   End If
   
End Function ' End GetStatusString


'******************************************************************************
'
' Subroutine: DisplayCertificate
'
' Synopsis  : Display the certificate.
'
' Parameter : Chain     - The chain object containing the certificate to be 
'                         displayed.
'             cIndex    - Index of the certificate to be displayed.
'
'******************************************************************************

Sub DisplayCertificate (Chain, cIndex)

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
   
   Dim Certificate
   Set Certificate = Chain.Certificates.Item(cIndex)
   
   Wscript.Stdout.Writeline "=== Certificate " & CStr(cIndex) & " of " & CStr(Chain.Certificates.Count) & " (status = 0x" & Hex(Chain.Status(cIndex)) & ") ==="
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
   If Chain.Status(cIndex) = 0 Then
      Wscript.Stdout.Writeline "IsValid: True"
   Else
      Wscript.Stdout.Writeline "IsValid: False"
   End If
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
      Wscript.Stdout.Writeline "  Unique name      = " & Certificate.PrivateKey.UniqueContainerName
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
   
   If Verbose Then
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
' Parameter : None
'
' Synopsis  : Parse the command line, and set the options accordingly.
'
'******************************************************************************

Sub ParseCommandLine

   ' Constants for command line parsing states.
   Const ARG_STATE_OPTIONS  = 0
   Const ARG_STATE_CHAIN    = 1
   Const ARG_STATE_USAGE    = 2
   Const ARG_STATE_POLICY   = 3
   Const ARG_STATE_VALIDITY = 4
   Const ARG_STATE_TIMEOUT  = 5
   Const ARG_STATE_PASSWORD = 6
   Const ARG_STATE_END      = 7
   
   ' Our command line parsing state's variable.
   Dim ArgState : ArgState = ARG_STATE_OPTIONS
   
   ' Parse comman line.
   Dim Arg
   For Each Arg In Wscript.Arguments
      Select Case ArgState
      Case ARG_STATE_OPTIONS
         Select Case UCase(Arg)
         Case "-CHAIN", "/CHAIN"
            ArgState = ARG_STATE_CHAIN
   
         Case "-EKU", "/EKU"
            ArgState = ARG_STATE_USAGE
            
         Case "-POLICY", "/POLICY"
            ArgState = ARG_STATE_POLICY
         
         Case "-VALIDITY", "/VALIDITY"
            ArgState = ARG_STATE_VALIDITY
         
         Case "-TIMEOUT", "/TIMEOUT"
            ArgState = ARG_STATE_TIMEOUT
         
         Case "-V", "/V"
            Verbose = TRUE
   
         Case "-?", "/?"
            DisplayUsage
   
         Case "~?"
            ExtendedHelp = True
            DisplayUsage
         
         Case Else
            If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
               DisplayUsage
            Else
               CertFile = Arg
            End If
            ArgState = ARG_STATE_PASSWORD
         
         End Select
   
      Case ARG_STATE_CHAIN
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            CheckFlag = CLng(Arg)
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
      
      Case ARG_STATE_VALIDITY
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            VerificationTime = CDate(Arg)
         End If
         ArgState = ARG_STATE_OPTIONS
            
      Case ARG_STATE_TIMEOUT
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            UrlRetrievalTimeout = CLng(Arg)
         End If
         ArgState = ARG_STATE_OPTIONS
   
      Case ARG_STATE_PASSWORD
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            Password = Arg
         End If
         ArgState = ARG_STATE_END
   
      Case Else
         DisplayUsage
         
      End Select
   Next
   
   ' Make sure filename is provided.
   If IsNull(CertFile) = True Then
      DisplayUsage
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

   Wscript.Stdout.Writeline "Usage: CView [Options] CertFile [Password]" 
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "Options:" 
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "  -chain    <value>         -- Check flag in hex value (Default is no check)"
   Wscript.Stdout.Writeline "  -eku      <name | oid>    ** EKU name or OID"
   Wscript.Stdout.Writeline "  -policy   <name | oid>    ** Certificate policy name or OID"
   Wscript.Stdout.Writeline "  -validity <date/time>     -- Validity verification date/time"
   Wscript.Stdout.Writeline "  -timeout  <seconds>       -- CRL retrieval timeout value"
   Wscript.Stdout.Writeline "  -v                        -- Verbose operation"
   Wscript.Stdout.Writeline "  -?                        -- This help screen"
   Wscript.Stdout.Writeline              
   Wscript.Stdout.Writeline "  CertFile                  -- CER or PFX file path"
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "  Password                  -- Password to decrypt the PFX file"
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "Note: All non-fatal invalid options will be ignored, and the ** symbol"
   Wscript.Stdout.Writeline "      indicates option can be listed multiple times."
   Wscript.Stdout.Writeline
   If ExtendedHelp Then
      Wscript.Stdout.Writeline "Examples:" 
      Wscript.Stdout.Writeline
      Wscript.Stdout.Writeline "  cview MyCert.cer" 
      Wscript.Stdout.Writeline
      Wscript.Stdout.Writeline "  cview -chain ""&H000001EF"" -validity 03/18/2001 MyCert.cer" 
      Wscript.Stdout.Writeline
      Wscript.Stdout.Writeline "  cview -chain ""&H000001F7"" -timeout 30 MyCert.cer" 
      Wscript.Stdout.Writeline
      Wscript.Stdout.Writeline "  cview -chain ""&H000001EF"" -policy ""medium assurance"" MyCert.cer" 
      Wscript.Stdout.Writeline
      Wscript.Stdout.Writeline "  cview -chain ""&H000001F7"" -eku ""code signing"" -eku 1.3.6.1.5.5.7.3.4 MyPfx.pfx MyPwd"
      Wscript.Stdout.Writeline
      Wscript.Stdout.Writeline "  cview -chain ""&H0000002F"" -eku ""secure email"" -policy ""high assurance"" MyCert.cer" 
    End If
   
   Wscript.Quit(1)
   
End Sub ' End DisplayUsage

