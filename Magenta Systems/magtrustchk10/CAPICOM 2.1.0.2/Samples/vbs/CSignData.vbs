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
' CSignData.vbs
'
' This is a sample script to illustrate how to use the CAPICOM's SignedData 
' to sign/verify text file.
'
' Note: For simplicity, this script does not handle exception.
'
'******************************************************************************

Option Explicit

Const ForReading = 1, ForWriting = 2


' Command.
Const Unknown                                               = 0
Const Sign		                                             = 1
Const CoSign                                                = 2
Const Verify                                                = 3
                                                            
' CAPICOM's constants.                                                            
Const CAPICOM_LOCAL_MACHINE_STORE                           = 1
Const CAPICOM_CURRENT_USER_STORE                            = 2
Const CAPICOM_ACTIVE_DIRECTORY_USER_STORE                   = 3
Const CAPICOM_SMART_CARD_USER_STORE                         = 4
                                                            
Const CAPICOM_CERTIFICATE_FIND_SHA1_HASH                    = 0
Const CAPICOM_CERTIFICATE_FIND_SUBJECT_NAME                 = 1
Const CAPICOM_CERTIFICATE_FIND_ISSUER_NAME                  = 2
Const CAPICOM_CERTIFICATE_FIND_ROOT_NAME                    = 3
Const CAPICOM_CERTIFICATE_FIND_TEMPLATE_NAME                = 4
Const CAPICOM_CERTIFICATE_FIND_EXTENSION                    = 5
Const CAPICOM_CERTIFICATE_FIND_EXTENDED_PROPERTY            = 6
Const CAPICOM_CERTIFICATE_FIND_APPLICATION_POLICY           = 7
Const CAPICOM_CERTIFICATE_FIND_CERTIFICATE_POLICY           = 8
Const CAPICOM_CERTIFICATE_FIND_TIME_VALID                   = 9
Const CAPICOM_CERTIFICATE_FIND_TIME_NOT_YET_VALID           = 10
Const CAPICOM_CERTIFICATE_FIND_TIME_EXPIRED                 = 11

Const CAPICOM_VERIFY_SIGNATURE_ONLY                         = 0
Const CAPICOM_VERIFY_SIGNATURE_AND_CERTIFICATE              = 1

Const CAPICOM_AUTHENTICATED_ATTRIBUTE_SIGNING_TIME          = 0
Const CAPICOM_AUTHENTICATED_ATTRIBUTE_DOCUMENT_NAME         = 1
Const CAPICOM_AUTHENTICATED_ATTRIBUTE_DOCUMENT_DESCRIPTION  = 2

Const CAPICOM_CERTIFICATE_INCLUDE_CHAIN_EXCEPT_ROOT         = 0
Const CAPICOM_CERTIFICATE_INCLUDE_WHOLE_CHAIN               = 1
Const CAPICOM_CERTIFICATE_INCLUDE_END_ENTITY_ONLY           = 2

Const CAPICOM_PROPID_KEY_PROV_INFO                          = 2

' Command line arguments.
Dim Command          : Command                              = Unknown
Dim StoreLocation    : StoreLocation                        = Null
Dim PFXFileName      : PFXFileName                          = Null
Dim PFXPassword      : PFXPassword                          = Null
Dim DocName          : DocName                              = Null
Dim DocDescription   : DocDescription                       = Null
Dim SigningTime      : SigningTime                          = Null
Dim VerifyFlag       : VerifyFlag                           = CAPICOM_VERIFY_SIGNATURE_ONLY
Dim IncludeOption    : IncludeOption                        = CAPICOM_CERTIFICATE_INCLUDE_CHAIN_EXCEPT_ROOT
Dim bDetached        : bDetached                            = False
Dim bVerbose         : bVerbose                             = False
                                                            
' Filters (some can be multiples).                          
Dim SHA1             : SHA1                                 = Null
Dim Subjects()
Dim Issuers()
Dim Roots()
Dim FileNames()

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

' Find the signer's certificate for sign operation.       
Dim Signer
Set Signer = CreateObject("CAPICOM.Signer")

' Setup signer's properties.
If Command = Sign OR Command = CoSign Then
   ' Are we using PFX file?
   If IsNull(PFXFileName) Then
      Dim iIndex
      Dim Store
      Dim Certificates
      Dim StoreName : StoreName = "MY"

      ' Open the appropriate store.
      Set Store = CreateObject("CAPICOM.Store")
      
      If IsNull(StoreLocation) Then
         StoreLocation = CAPICOM_CURRENT_USER_STORE
      ElseIf StoreLocation = CAPICOM_SMART_CARD_USER_STORE Then
         StoreName = ""
      End If
      
      Store.Open StoreLocation, StoreName
      
      ' Find certificate(s) matching the search criteria, if requested.
      Set Certificates = Store.Certificates
      
      If Certificates.Count > 0 AND Not IsNull(SHA1) Then
         Set Certificates = Certificates.Find(CAPICOM_CERTIFICATE_FIND_SHA1_HASH, SHA1)
      End If
      
      If Certificates.Count > 0 AND IsReDimed(Subjects) Then
         For iIndex = LBound(Subjects) To UBound(Subjects) 
            Set Certificates = Certificates.Find(CAPICOM_CERTIFICATE_FIND_SUBJECT_NAME, Subjects(iIndex))
         Next
      End If
      
      If Certificates.Count > 0 AND IsReDimed(Issuers) Then
         For iIndex = LBound(Issuers) To UBound(Issuers) 
            Set Certificates = Certificates.Find(CAPICOM_CERTIFICATE_FIND_ISSUER_NAME, Issuers(iIndex))
         Next
      End If
      
      If Certificates.Count > 0 AND IsReDimed(Roots) Then
         For iIndex = LBound(Roots) To UBound(Roots) 
            Set Certificates = Certificates.Find(CAPICOM_CERTIFICATE_FIND_ROOT_NAME, Roots(iIndex))
         Next
      End If
      
      ' Filter out all certificates without a private key.
      If Certificates.Count > 0 Then
         Set Certificates = Certificates.Find(CAPICOM_CERTIFICATE_FIND_EXTENDED_PROPERTY, CAPICOM_PROPID_KEY_PROV_INFO)
      End If
      
      ' Finally, only select time valid certificates.
      If Certificates.Count > 0 Then
         Set Certificates = Certificates.Find(CAPICOM_CERTIFICATE_FIND_TIME_VALID, Now)
      End If
      
      ' Ask user to select certificate if more than one is found.
      Select Case Certificates.Count
      Case 0
         Wscript.Stdout.Writeline "Error: No signing certificate can be found."
         Wscript.Quit(1)
   
      Case 1
         Signer.Certificate = Certificates(1)
   
      Case Else
         Set Certificates = Certificates.Select("CSignData.vbs", "Please select a certificate to sign " & FileNames(0) & ".")
         If Certificates.Count = 0 Then
            Wscript.Stdout.Writeline "Error: Certificate selection dialog was cancelled."
            Wscript.Quit(2)
         End If
         Signer.Certificate = Certificates(1)
       
      End Select
      
      Set Certificates = Nothing
      Set Store = Nothing
   Else
      ' Load the signing certificate from PFX file directly.
      ' Note: The key container is automatically deleted from the system after
      '       the certificate is used for signing.
      Signer.Load PFXFileName, PFXPassword
   End If

   ' Set signer's include option and attribute(s).
   Signer.Options = IncludeOption
   
   If Not IsNull(DocName) Then
      AddAttribute Signer, CAPICOM_AUTHENTICATED_ATTRIBUTE_DOCUMENT_NAME, DocName
   End If
   
   If Not IsNull(DocDescription) Then
      AddAttribute Signer, CAPICOM_AUTHENTICATED_ATTRIBUTE_DOCUMENT_DESCRIPTION, DocDescription
   End If
   
   If Not IsNull(SigningTime) Then
      AddAttribute Signer, CAPICOM_AUTHENTICATED_ATTRIBUTE_SIGNING_TIME, SigningTime
   End If
End If

' Now process the command.
Select Case Command
Case Sign   
   DoSignCommand FileNames, bDetached, Signer

Case CoSign
   DoCoSignCommand FileNames, bDetached, Signer
   
Case Verify
   DoVerifyCommand FileNames, bDetached, VerifyFlag
   
End Select

' Free resources.
Set Signer = Nothing
     
Wscript.Quit(0)

' End Main

 
'******************************************************************************
'
' Subroutine: DoSignCommand
'
' Synopsis  : Sign content of text file FileNames(0) and save signed content to 
'             FileNames(1).
'
' Parameter : FileNames - Array of filenames.
'
'             bDetached - True for detached signing.
'
'             Signer - Signer.
'
'******************************************************************************

Sub DoSignCommand (FileNames, bDetached, Signer)
   Dim Content
   Dim Message
   Dim SignedData

   ' Create the SignedData object.
   Set SignedData = CreateObject("CAPICOM.SignedData")
   
   ' Display main title.
   Wscript.Stdout.Writeline "Signing text file " & FileNames(0) & "."
   Wscript.Stdout.Writeline
   
   ' Display more detail for verbose operation.
   If bVerbose Then
      DisplayDetail "=== Signer ===", Signer, bDetached
   End If
   
   ' Load content of text file to be signed.
   LoadFile FileNames(0), Content
   
   ' Now sign it.
   SignedData.Content = Content
   Message = SignedData.Sign(Signer, bDetached)
   
   ' Finally, save signed message to out FileNames(1).
   SaveFile FileNames(1), Message
   Wscript.Stdout.Writeline "Successful - Signed message saved to " & FileNames(1) & "."

   ' Free resources.
   Set SignedData = Nothing

End Sub ' End DoSignCommand


'******************************************************************************
'
' Subroutine: DoCoSignCommand
'
' Synopsis  : CoSign a signed text file FileNames(0) and save cosigned content
'             to FileNames(1).
'
' Parameter : FileNames - Array of filenames.
'
'             bDetached - True for detached signing.
'
'             Signer - Signer.
'
'******************************************************************************

Sub DoCoSignCommand (FileNames, bDetached, Signer)
   Dim Content
   Dim Message
   Dim SignedData

   ' Create the SignedData object.
   Set SignedData = CreateObject("CAPICOM.SignedData")
   
   ' Display main title.
   Wscript.Stdout.Writeline "CoSigning text file " & FileNames(0) & "."
   Wscript.Stdout.Writeline
   
   ' Display more detail for verbose operation.
   If bVerbose Then
      DisplayDetail  "=== CoSigner ===", Signer, bDetached
   End If
   
   ' Load the signed message.
   LoadFile FileNames(0), Message
   
   ' Verify to open the signed message.
   SignedData.Verify Message, bDetached, VerifyFlag

   ' Now CoSign it.
   Message = SignedData.CoSign(Signer, bDetached)
   
   ' Finally, save cosigned message to FileNames(0) if non-detached.
   If bDetached Then
      Wscript.Stdout.Writeline "Successful."   
   Else
      SaveFile FileNames(0), Message
      Wscript.Stdout.Writeline "Successful - CoSigned message saved to " & FileNames(0) & "."
   End If

   ' Free resources.
   Set SignedData = Nothing

End Sub ' End DoCoSignCommand


'******************************************************************************
'
' Subroutine: DoVerifyCommand
'
' Synopsis  : Verify a signed text file.
'
' Parameter : FileNames - Array of filenames.
'
'             bDetached - True if detached signature.
'
'             VerifyFlag - Verify flag.
'
'******************************************************************************

Sub DoVerifyCommand (FileNames, bDetached, VerifyFlag)
   Dim Content
   Dim Message
   Dim SignedData

   ' Create the SignedData object.
   Set SignedData = CreateObject("CAPICOM.SignedData")
   
   ' Display main title.
   Wscript.Stdout.Writeline "Verifying signed text file " & FileNames(0) & ", please wait..."
   Wscript.Stdout.Writeline
   
   ' Load signed message to be verified.
   LoadFile FileNames(0), Message
   
   ' Load original text content if detached.
   If bDetached Then
      LoadFile FileNames(1), Content
      SignedData.Content = Content
   End If
   
   ' Now verify it.
   SignedData.Verify Message, bDetached, VerifyFlag

   ' Display more detail for verbose operation.
   If bVerbose Then
      Dim Signer
      Dim cSigner : cSigner = SignedData.Signers.Count
      Dim sIndex  : sIndex = 0
      For Each Signer In SignedData.Signers
         sIndex = sIndex + 1
         DisplayDetail "=== Signer " & CStr(sIndex) & " of " & CStr(cSigner) & " === ", Signer, bDetached
      Next
   End If
      
   ' Finally, save verified content to FileNames(1) if non-detached.
   If bDetached Then
      Wscript.Stdout.Writeline "Successful."   
   Else
      SaveFile FileNames(1), SignedData.Content
      Wscript.Stdout.Writeline "Successful - Verified content saved to " & FileNames(1) & "."
   End If
      
   ' Free resources.
   Set SignedData = Nothing

End Sub ' End DoVerifyCommand


'******************************************************************************
'
' Subroutine: AddAttribute
'
' Synopsis  : Add an attribute to the signer object.
'
' Parameter : Signer - Signer.
'
'             AttrName - Attribute name.
'
'             AttrVal - Attribute value.
'
'******************************************************************************

Sub AddAttribute (Signer, AttrName, AttrVal)
   Dim Attribute
   Set Attribute = CreateObject("CAPICOM.Attribute")
   
   Attribute.Name = AttrName
   Attribute.Value = AttrVal
   Signer.AuthenticatedAttributes.Add Attribute
   
   Set Attribute = Nothing

End Sub ' End AddAttribute


'******************************************************************************
'
' Subroutine: LoadFile
'
' Synopsis  : Read content of a text file.
'
' Parameter : FileName - Input text filename.
'
'             Buffer - String buffer to receive the text file content.
'
'******************************************************************************

Sub LoadFile (FileName, Buffer)   
    Dim fso
    Set fso = CreateObject("Scripting.FileSystemObject")

    If Not fso.FileExists(FileName) Then
        Wscript.Stdout.Writeline "Error: File " & FileName & " not found."
        Wscript.Quit(-5)
    End If
   
    Dim ts
    Set ts = fso.OpenTextFile(FileName, ForReading)  
    Buffer = ts.ReadAll
    
End Sub ' End LoadFile


'******************************************************************************
'
' Subroutine: SaveFile
'
' Synopsis  : Save string to file.
'
' Parameter : FileName - Output filename.
'
'             Buffer - String buffer to be saved.
'
'******************************************************************************

Sub SaveFile (FileName, Buffer)
    Dim fso
    Set fso = CreateObject("Scripting.FileSystemObject")
    
    Dim ts
    Set ts = fso.OpenTextFile(FileName, ForWriting, True)
    ts.Write Buffer
    
End Sub ' End SaveFile


'******************************************************************************
'
' Subroutine: DisplayDetail
'
' Synopsis  : Display detail information.
'
' Parameter : Title - Title line.
'
'             Signer - Signer object.
'
'             bDetached - True if detached else False.
'
'******************************************************************************

Sub DisplayDetail (Title, Signer, bDetached)
   Const CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME = 0
   
   Wscript.Stdout.Writeline Title
   Wscript.Stdout.Writeline
   
   Wscript.Stdout.Writeline "Signer    : " & Signer.Certificate.GetInfo(CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME)

   If Signer.AuthenticatedAttributes.Count = 0 Then
      Wscript.Stdout.Writeline "Attributes: None."
   Else
      Dim Attribute
      Dim aIndex : aIndex = 0
      Wscript.Stdout.Writeline "Attributes:"
      For Each Attribute In Signer.AuthenticatedAttributes
         aIndex = aIndex + 1
         
         Select Case Attribute
         Case CAPICOM_AUTHENTICATED_ATTRIBUTE_DOCUMENT_NAME
            Wscript.Stdout.Writeline "  [" & CStr(aIndex) & "] Document name = " & Attribute.Value
   
         Case CAPICOM_AUTHENTICATED_ATTRIBUTE_DOCUMENT_DESCRIPTION
            Wscript.Stdout.Writeline "  [" & CStr(aIndex) & "] Document description = " & Attribute.Value   
            
         Case CAPICOM_AUTHENTICATED_ATTRIBUTE_SIGNING_TIME
            Wscript.Stdout.Writeline "  [" & CStr(aIndex) & "] Signing time = " & Attribute.Value   
                     
         End Select
      Next
   End If
   
   Wscript.Stdout.Writeline "Detached  : " & bDetached   
   
   If Not Signer.Chain Is Nothing Then
      Dim cCerts : cCerts = Signer.Chain.Certificates.Count
      Wscript.Stdout.Writeline "Chain     : " & CStr(cCerts) & " certificate(s)"
      Wscript.Stdout.Writeline
   
      Dim Certificate
      Dim cIndex : cIndex = 0
      For Each Certificate In Signer.Chain.Certificates
         cIndex = cIndex + 1
         DisplayCertificate Certificate, "=== Certificate " & CStr(cIndex) & " of " & CStr(cCerts) & " ==="
      Next
   End If
   
   Wscript.Stdout.Writeline 
   
End Sub ' End DisplayDetail


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

   Wscript.Stdout.Writeline Title
   Wscript.Stdout.Writeline "Subject name : " & Certificate.SubjectName
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "Issuer name  : " & Certificate.IssuerName
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "Serial number: " & Certificate.SerialNumber
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "SHA1 hash    : " & Certificate.Thumbprint
   Wscript.Stdout.Writeline

End Sub ' End DisplayCertificate


'******************************************************************************
'
' Subroutine: IsReDimed
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
' Synopsis  : Parse the command line, and set the options accordingly.
'
' Parameter : None
'
'******************************************************************************

Sub ParseCommandLine

   ' Constants for command line parsing states.
   Const ARG_STATE_COMMAND                   = 0
   Const ARG_STATE_OPTIONS                   = 1    
   Const ARG_STATE_LOCATION                  = 2 
   Const ARG_STATE_SHA1                      = 3 
   Const ARG_STATE_SUBJECT                   = 4 
   Const ARG_STATE_ISSUER                    = 5 
   Const ARG_STATE_ROOT                      = 6 
   Const ARG_STATE_PFX                       = 7 
   Const ARG_STATE_PWD                       = 8 
   Const ARG_STATE_NAME                      = 9 
   Const ARG_STATE_INCLUDE                   = 10
   Const ARG_STATE_DESCRIPTION               = 11
   Const ARG_STATE_SIGNINGTIME               = 12
   Const ARG_STATE_FILENAME                  = 13
   
   ' Parse command line.
   Dim Arg
   Dim ArgState : ArgState = ARG_STATE_COMMAND
   
   For Each Arg In Wscript.Arguments
      Select Case ArgState
      Case ARG_STATE_COMMAND
         Select Case UCase(Arg) 
         Case "SIGN"
            Command = Sign

         Case "COSIGN"
            Command = CoSign

         Case "VERIFY"
            Command = Verify

         Case Else
            DisplayUsage
            
         End Select
         
         ArgState = ARG_STATE_OPTIONS
         
      Case ARG_STATE_OPTIONS
         Select Case UCase(Arg) 
         Case "-L", "/L"
            ArgState = ARG_STATE_LOCATION
            
         Case "-SHA1", "/SHA1"
            ArgState = ARG_STATE_SHA1
            
         Case "-SUBJECT", "/SUBJECT"
            ArgState = ARG_STATE_SUBJECT
            
         Case "-ISSUER", "/ISSUER"
            ArgState = ARG_STATE_ISSUER
            
         Case "-ROOT", "/ROOT"
            ArgState = ARG_STATE_ROOT
            
         Case "-PFX", "/PFX"
            ArgState = ARG_STATE_PFX
            
         Case "-PWD", "/PWD"
            ArgState = ARG_STATE_PWD
            
         Case "-INCLUDE", "/INCLUDE"
            ArgState = ARG_STATE_INCLUDE
         
         Case "-AN", "/AN"
            ArgState = ARG_STATE_NAME
            
         Case "-AD", "/AD"
            ArgState = ARG_STATE_DESCRIPTION
            
         Case "-AT", "/AT"
            ArgState = ARG_STATE_SIGNINGTIME
            
         Case "-BOTH", "/BOTH"
            VerifyFlag = CAPICOM_VERIFY_SIGNATURE_AND_CERTIFICATE
         
         Case "-DETACHED", "/DETACHED"
            bDetached = True
            
         Case "-V", "/V"
            bVerbose = True
            
         Case "-?", "/?"
            DisplayUsage
            
         Case Else
            If Left(Arg, 1) = "-"  OR Left(Arg, 1) = "/" Then
               DisplayUsage
            Else
               ReDim FileNames(0)              
               FileNames(0) = Arg
            End If
            ArgState = ARG_STATE_FILENAME
            
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
            
            Case "SC"
               StoreLocation = CAPICOM_SMART_CARD_USER_STORE
      
            Case Else
               DisplayUsage
               
            End Select
         End If
         ArgState = ARG_STATE_OPTIONS
      
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
      
      Case ARG_STATE_PFX
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            PFXFileName = Arg            
         End If
         ArgState = ARG_STATE_OPTIONS
      
      Case ARG_STATE_PWD
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            PFXPassword = Arg            
         End If
         ArgState = ARG_STATE_OPTIONS
      
      Case ARG_STATE_INCLUDE
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            IncludeOption = CLng(Arg)
         End If
         ArgState = ARG_STATE_OPTIONS

      Case ARG_STATE_NAME
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            DocName = Arg            
         End If
         ArgState = ARG_STATE_OPTIONS
      
      Case ARG_STATE_DESCRIPTION
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            DocDescription = Arg            
         End If
         ArgState = ARG_STATE_OPTIONS
      
      Case ARG_STATE_SIGNINGTIME
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            If UCase(Arg) = "NOW" Then
               SigningTime = Now
            Else
               SigningTime = CDate(Arg)
            End If
         End If
         ArgState = ARG_STATE_OPTIONS

      Case ARG_STATE_FILENAME
         If Left(Arg, 1) = "-"  OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            ReDim Preserve FileNames(UBound(FileNames) + 1)
         End If
         FileNames(UBound(FileNames)) = Arg
          
      Case Else
         Wscript.Stdout.Writeline "Internal script error: Unknown argument state (" & CStr(ArgState) & ") encountered."
         Wscript.Quit(-3)
         
      End Select
   Next
   
   ' Make sure we are in good state.
   If ArgState <> ARG_STATE_FILENAME Then
      DisplayUsage
   End If

   ' Check options.
   Select Case Command
   Case Sign
      ' If -l, then cannot have -pfx.
      If (Not IsNull(StoreLocation)) AND (Not IsNull(PFXFileName)) Then
         DisplayUsage
      End If
   
      ' If -pwd, then must have -pfx
      If (Not IsNull(PFXPassword)) AND (IsNull(PFXFileName)) Then
          DisplayUsage
      End If

      ' Must have both in and out file.
      If (UBound(FileNames) - LBound(FileNames) + 1) <> 2 Then
         DisplayUsage
      End If
   
   Case CoSign
      ' If -l, then cannot have -pfx.
      If (Not IsNull(StoreLocation)) AND (Not IsNull(PFXFileName)) Then
         DisplayUsage
      End If
   
      ' If -pwd, then must have -pfx
      If (Not IsNull(PFXPassword)) AND (IsNull(PFXFileName)) Then
          DisplayUsage
      End If

      ' Must have the content file if detached.
      If (bDetached) AND ((UBound(FileNames) - LBound(FileNames) + 1) <> 2) Then
         DisplayUsage
      End If
   
   Case Verify
      ' Must have both in and out file.
      If (UBound(FileNames) - LBound(FileNames) + 1) <> 2 Then
         DisplayUsage
      End If
      
   End Select
   
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
'******************************************************************************

Sub DisplayUsage

   Select Case Command
      Case Unknown
         Wscript.Stdout.Writeline "Usage: CSignData Command [Options] File1 [File2]"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Command:"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  Sign                   -- Sign a text file"
         Wscript.Stdout.Writeline "  CoSign                 -- CoSign a signed text file"
         Wscript.Stdout.Writeline "  Verify                 -- Verify a signed text file"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "For help on a specific command, enter ""CSignData Command -?"""
         
      Case Sign
         Wscript.Stdout.Writeline "Usage: CSignData Sign [Options] ContentFile SignedFile"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The Sign command is used to sign a text file. Signing protects a file from"
         Wscript.Stdout.Writeline "tampering, and allows user to verify the signer based on signing certificate."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "For non-detached signing, both the content and signature will be saved to"
         Wscript.Stdout.Writeline "SignedFile. For detached signing, only the signature is saved to SignedFile."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Options:"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  -l       <location>    -- CU, LM, or SC (default to CU)"
         Wscript.Stdout.Writeline "  -pfx     <filename>    -- Use certificate in the PFX (exclusive with -l)"
         Wscript.Stdout.Writeline "  -pwd     <password>    -- Password for the PFX file (require -pfx)."
         Wscript.Stdout.Writeline "  -sha1    <hash>        -- SHA1 hash of the signing certificate"
         Wscript.Stdout.Writeline "  -subject <name>        ** Subject Name of the signing certificate must contain"
         Wscript.Stdout.Writeline "                            this name"
         Wscript.Stdout.Writeline "  -issuer  <name>        ** Issuer Name of the signing certificate must contain"
         Wscript.Stdout.Writeline "                            this name"
         Wscript.Stdout.Writeline "  -root    <name>        ** Subject Name of the root certificate must contain"
         Wscript.Stdout.Writeline "                            this name"
         Wscript.Stdout.Writeline "  -include <0 | 1 | 2>   -- Include option (0 for chain minus root, 1 for whole"
         Wscript.Stdout.Writeline "                            chain, or 2 for end certificate only (default to 0)"
         Wscript.Stdout.Writeline "  -an      <name>        -- Document name attribute"
         Wscript.Stdout.Writeline "  -ad      <description> -- Document description attribute"
         Wscript.Stdout.Writeline "  -at      <time>        -- Signing time attribute (""now"" for current time)"
         Wscript.Stdout.Writeline "  -detached              -- Detached signing"
         Wscript.Stdout.Writeline "  -v                     -- Verbose operation"
         Wscript.Stdout.Writeline "  -?                     -- This help screen"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  ContentFile            -- Text file to be signed"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  SignedFile             -- Signed file (contains signature only if detached)"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Note: All non-fatal invalid options for this specific command will be ignored,"
         Wscript.Stdout.Writeline "      and the ** symbol indicates option can be listed multiple times."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "      If there is only one certificate found in the MY store or PFX that"
         Wscript.Stdout.Writeline "      matches the requirement, that particular certificate will be used."
	  	   Wscript.Stdout.Writeline "      However, if there is more than one certificate matching the requirement,"
         Wscript.Stdout.Writeline "      a dialog will be displayed to allow selection of the signing certificate."
         Wscript.Stdout.Writeline
         
      Case CoSign
         Wscript.Stdout.Writeline "Usage: CSignData CoSign [Options] SignedFile [ContentFile]"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The CoSign command is used to cosign a signed text file. CoSigning provides the"
         Wscript.Stdout.Writeline "same type of benefits as signing, with an additional signature."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "For non-detached cosigning, both the content and signatures will be saved to"
         Wscript.Stdout.Writeline "SignedFile. For detached cosigning, only the signatures are saved to"
         Wscript.Stdout.Writeline "SignedFile."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Options:"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  -l       <location>    -- CU, LM, or SC (default to CU)"
         Wscript.Stdout.Writeline "  -pfx     <filename>    -- Use certificate in the PFX (exclusive with -l)"
         Wscript.Stdout.Writeline "  -pwd     <password>    -- Password for the PFX file (require -pfx)."
         Wscript.Stdout.Writeline "  -sha1    <hash>        -- SHA1 hash of the signing certificate"
         Wscript.Stdout.Writeline "  -subject <name>        ** Subject Name of the signing certificate must contain"
         Wscript.Stdout.Writeline "                            this name"
         Wscript.Stdout.Writeline "  -issuer  <name>        ** Issuer Name of the signing certificate must contain"
         Wscript.Stdout.Writeline "                            this name"
         Wscript.Stdout.Writeline "  -root    <name>        ** Subject Name of the root certificate must contain"
         Wscript.Stdout.Writeline "                            this name"
         Wscript.Stdout.Writeline "  -include <0 | 1 | 2>   -- Include option (0 for chain minus root, 1 for whole"
         Wscript.Stdout.Writeline "                            chain, or 2 for end certificate only (default to 0)"
         Wscript.Stdout.Writeline "  -an      <name>        -- Document name attribute"
         Wscript.Stdout.Writeline "  -ad      <description> -- Document description attribute"
         Wscript.Stdout.Writeline "  -at      <time>        -- Signing time attribute (""now"" for current time)"
         Wscript.Stdout.Writeline "  -detached              -- Detached signing"
         Wscript.Stdout.Writeline "  -v                     -- Verbose operation"
         Wscript.Stdout.Writeline "  -?                     -- This help screen"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  SignedFile             -- Signed file (contains signature only if detached)"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  ContentFile            -- Text file (required if detached)"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Note: All non-fatal invalid options for this specific command will be ignored,"
         Wscript.Stdout.Writeline "      and the ** symbol indicates option can be listed multiple times."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "      If there is only one certificate found in the MY store or PFX that"
         Wscript.Stdout.Writeline "      matches the requirement, that particular certificate will be used."
	  	   Wscript.Stdout.Writeline "      However, if there is more than one certificate matching the requirement,"
         Wscript.Stdout.Writeline "      a dialog will be displayed to allow selection of the signing certificate."
         Wscript.Stdout.Writeline
         
       Case Verify
         Wscript.Stdout.Writeline "Usage: CSignData Verify [Options] SignedFile ContentFile"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The Verify command is used to verify signed text file. Verification checks"
         Wscript.Stdout.Writeline "integrity of the signed file and determines if the signing certificate is"
         Wscript.Stdout.Writeline "valid and issued by a trusted party."
         Wscript.Stdout.Writeline 
         Wscript.Stdout.Writeline "For non-detached signed file, the content will be extracted and saved to"
         Wscript.Stdout.Writeline "ContentFile. For detached signed file, the ContentFile is not modified."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Options:"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  -both                  -- Verify both signature and certificate"
         Wscript.Stdout.Writeline "  -detached              -- Detached signature"
         Wscript.Stdout.Writeline "  -v                     -- Verbose operation"
         Wscript.Stdout.Writeline "  -?                     -- This help screen"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  SignedFile             -- Signed file (contains signature only if detached)"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  ContentFile            -- Text file (will not be over written if detached)"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Note: All non-fatal invalid options for this specific command will be ignored."
         Wscript.Stdout.Writeline
         
      Case Else
         Wscript.Stdout.Writeline "Internal script error: Unknown help state (Command = " & CStr(Command) & ")."
         Wscript.Quit(-2)
         
   End Select

   Wscript.Quit(-1)
   
End Sub ' End DisplayUsage

