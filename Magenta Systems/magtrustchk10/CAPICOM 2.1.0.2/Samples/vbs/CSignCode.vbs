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
' CSignCode.vbs
'
' This is a sample script to illustrate how to use the CAPICOM's SignedCode 
' objects to sign/timestamp/verify executable file.
'
' Note: For simplicity, this script does not handle exception.
'
'******************************************************************************

Option Explicit

' Command.
Const Unknown                                               = 0
Const Sign		                                             = 1
Const Timestamp                                             = 2
Const Verify                                                = 3
                                                            
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
                                                            
Const CAPICOM_CERTIFICATE_INCLUDE_CHAIN_EXCEPT_ROOT         = 0
Const CAPICOM_CERTIFICATE_INCLUDE_WHOLE_CHAIN               = 1
Const CAPICOM_CERTIFICATE_INCLUDE_END_ENTITY_ONLY           = 2

Const CAPICOM_AUTHENTICATED_ATTRIBUTE_SIGNING_TIME          = 0
Const CAPICOM_AUTHENTICATED_ATTRIBUTE_DOCUMENT_NAME         = 1
Const CAPICOM_AUTHENTICATED_ATTRIBUTE_DOCUMENT_DESCRIPTION  = 2

Const CAPICOM_PROPID_KEY_PROV_INFO                          = 2

' Command line arguments.
Dim Command          : Command         = Unknown
Dim StoreLocation    : StoreLocation   = Null
Dim PFXFileName      : PFXFileName     = Null
Dim PFXPassword      : PFXPassword     = NULL
Dim Description      : Description     = Null
Dim DescriptionURL   : DescriptionURL  = Null
Dim TimestampURL     : TimestampURL    = Null
Dim IncludeOption    : IncludeOption   = CAPICOM_CERTIFICATE_INCLUDE_CHAIN_EXCEPT_ROOT
Dim AttrName         : AttrName        = NULL
Dim AttrDesc         : AttrDesc        = NULL
Dim AttrTime         : AttrTime        = NULL
Dim bDisplayDialog   : bDisplayDialog  = False
Dim bVerbose         : bVerbose        = False
                                       
' Filters (some can be multiples).     
Dim SHA1             : SHA1            = Null
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
If Command = Sign Then
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
      
      ' Filter out certificates without private key.
      If Certificates.Count > 0 Then
         Set Certificates = Certificates.Find(CAPICOM_CERTIFICATE_FIND_EXTENDED_PROPERTY, CAPICOM_PROPID_KEY_PROV_INFO)
      End If
           
      ' Filter out time invalid certificates.
      If Certificates.Count > 0 Then
         Set Certificates = Certificates.Find(CAPICOM_CERTIFICATE_FIND_TIME_VALID, Now)
      End If
      
      ' Finally, only select Code Signing certificates.
      If Certificates.Count > 0 Then
         Set Certificates = Certificates.Find(CAPICOM_CERTIFICATE_FIND_APPLICATION_POLICY, "Code Signing")
      End If
      
      ' Ask user to select certificate if more than one is found.
      Select Case Certificates.Count
      Case 0
         Wscript.Stdout.Writeline "Error: No signing certificate can be found."
         Wscript.Quit(1)
   
      Case 1
         Signer.Certificate = Certificates(1)
   
      Case Else
         Set Certificates = Certificates.Select("CSignCode.vbs", "Please select a certificate to sign " & FileNames(0) & ".")
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
      Signer.Load PFXFileName, PFXPassword
   End If
   
   ' Set attributes, if any.
   Dim AuthAttr
   If Not IsNull(AttrName) Then
      Set AuthAttr = CreateObject("CAPICOM.Attribute")
      AuthAttr.Name = CAPICOM_AUTHENTICATED_ATTRIBUTE_DOCUMENT_NAME
      AuthAttr.Value = AttrName
      Signer.AuthenticatedAttributes.Add AuthAttr
      Set AuthAttr = Nothing
   End If
   If Not IsNull(AttrDesc) Then
      Set AuthAttr = CreateObject("CAPICOM.Attribute")
      AuthAttr.Name = CAPICOM_AUTHENTICATED_ATTRIBUTE_DOCUMENT_DESCRIPTION
      AuthAttr.Value = AttrDesc
      Signer.AuthenticatedAttributes.Add AuthAttr
      Set AuthAttr = Nothing
   End If
   If Not IsNull(AttrTime) Then
      Set AuthAttr = CreateObject("CAPICOM.Attribute")
      AuthAttr.Name = CAPICOM_AUTHENTICATED_ATTRIBUTE_SIGNING_TIME
      AuthAttr.Value = AttrTime
      Signer.AuthenticatedAttributes.Add AuthAttr
      Set AuthAttr = Nothing
   End If
   
   ' Finally, set signer's include option.
   Signer.Options = IncludeOption
End If
 
' Now process all files for the requested command.
Dim fIndex
For fIndex = LBound(FileNames) to UBound(FileNames)
   Select Case Command
   Case Sign
       DoSignCommand FileNames(fIndex), Description, DescriptionURL, TimestampURL, Signer
      
   Case Timestamp
      DoTimestampCommand FileNames(fIndex), TimestampURL
   
   Case Verify
      DoVerifyCommand FileNames(fIndex), bDisplayDialog
   
   Case Else
     Wscript.Stdout.Writeline "Internal script error: Unknown command (" & CStr(Command) & ") encountered."
     Wscript.Quit(-4)
     
   End Select
Next

' Free resources.
Set Signer = Nothing
     
Wscript.Quit(0)

' End Main


'******************************************************************************
'
' Subroutine: DoSignCommand
'
' Synopsis  : Authenticode sign the specified executable file using options
'             selected by the command line options.
'
' Parameter : IN FileName        - Executable file to be signed.
'
'             IN Description     - Description text.
'
'             IN DescriptionURL  - URL of description.
'
'             IN TimestampURL    - URL of timestamp server.
'
'             IN Signer          - Signer object.
'
'******************************************************************************

Sub DoSignCommand (FileName, Description, DescriptionURL, TimestampURL, Signer)
   Dim SignedCode
   
   ' Create a SignedCode object.
   Set SignedCode = CreateObject("CAPICOM.SignedCode")
   
   ' Display main title.
   Wscript.Stdout.Writeline "=== Signing " & FileName & " ==="
   Wscript.Stdout.Writeline

   ' Set all properties.
   SignedCode.FileName = FileName
   If Not IsNull(Description) Then
      SignedCode.Description = Description
   End If
   If Not IsNull(DescriptionURL) Then
      SignedCode.DescriptionURL = DescriptionURL
   End If

   ' Now sign it.      
   SignedCode.Sign Signer
      
   ' Finally, timestamp the file if requested.
   If Not IsNull(TimestampURL) Then
      SignedCode.Timestamp TimestampURL
   End If
   
   ' Display more detail for verbose operation.
   If bVerbose Then
     DisplayDetail SignedCode
   End If
   
   Wscript.Stdout.Writeline "Successful."
   Wscript.Stdout.Writeline
   
   ' Free resources.
   Set SignedCode = Nothing

End Sub ' End DoSignCommand


'******************************************************************************
'
' Subroutine: DoTimestampCommand
'
' Synopsis  : Timestamp the specified Authenticode signed executable file 
'             using options selected by the command line options.
'
' Parameter : IN FileName - Executable file to be timestamped.
'
'             IN TimestampURL - URL of timestamp server.
'
'******************************************************************************

Sub DoTimestampCommand (FileName, TimestampURL)
   Dim SignedCode
   
   ' Create a SignedCode object.
   Set SignedCode = CreateObject("CAPICOM.SignedCode")

   ' Display main title.
   Wscript.Stdout.Writeline "=== Timestamping " & Filename & " ==="
   Wscript.Stdout.Writeline

   ' Now timestamp it.      
   SignedCode.FileName = FileName
   SignedCode.Timestamp TimestampURL
   
   ' Display more detail for verbose operation.
   If bVerbose Then
     DisplayDetail SignedCode
   End If

   Wscript.Stdout.Writeline "Successful."
   Wscript.Stdout.Writeline

   ' Free resources.
   Set SignedCode = Nothing

End Sub ' End DoTimestampCommand

  
'******************************************************************************
'
' Subroutine: DoVerifyCommand
'
' Synopsis  : Verify if the specified executable is Authenticode signed and
'             signed by a trusted publisher.
'
' Parameter : IN FileName       - File to be verified.
'
'             IN bDisplayDialog - True to also display the Authenticode dialog.
'
'******************************************************************************

Sub DoVerifyCommand (FileName, bDisplayDialog)
   Dim SignedCode
   
   ' Create a SignedCode object.
   Set SignedCode = CreateObject("CAPICOM.SignedCode")

   ' Display main title.
   Wscript.Stdout.Writeline "=== Verifying " & Filename & " ==="
   Wscript.Stdout.Writeline

   ' Now verify it.      
   SignedCode.FileName = FileName
   SignedCode.Verify bDisplayDialog
   
   ' Display more detail for verbose operation.
   If bVerbose Then
     DisplayDetail SignedCode
   End If

   Wscript.Stdout.Writeline "Successful."
   Wscript.Stdout.Writeline

   ' Free resources.
   Set SignedCode = Nothing

End Sub ' End DoVerifyCommand


'******************************************************************************
'
' Subroutine: DisplayDetail
'
' Synopsis  : Display detail information.
'
' Parameter : SignedCode - SignedCode object.
'
'******************************************************************************

Sub DisplayDetail (SignedCode)

   Const CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME = 0
   
   Wscript.Stdout.Writeline "  Description    : " & SignedCode.Description
   Wscript.Stdout.Writeline "  Description URL: " & SignedCode.DescriptionURL
   Wscript.Stdout.Writeline "  Signer         : " & SignedCode.Signer.Certificate.GetInfo(CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME)
   If Not SignedCode.TimeStamper Is Nothing Then
      Wscript.Stdout.Writeline "  Timestamper    : " & SignedCode.TimeStamper.Certificate.GetInfo(CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME)
   End If
   If Not IsNull(TimestampURL) Then
      Wscript.Stdout.Writeline "  Timestamp URL  : " & TimestampURL
   End If
   Dim AuthAttr
   Dim aIndex : aIndex = 0
   For Each AuthAttr In SignedCode.Signer.AuthenticatedAttributes
      Select Case AuthAttr.Name
      Case CAPICOM_AUTHENTICATED_ATTRIBUTE_SIGNING_TIME
         Wscript.Stdout.Writeline "  Signing time   : " & AuthAttr.Value
         
      Case CAPICOM_AUTHENTICATED_ATTRIBUTE_DOCUMENT_NAME
         Wscript.Stdout.Writeline "  Name attribute : " & AuthAttr.Value

      Case CAPICOM_AUTHENTICATED_ATTRIBUTE_DOCUMENT_DESCRIPTION
         Wscript.Stdout.Writeline "  Desc attribute : " & AuthAttr.Value

      End Select
   Next
   If Not SignedCode.Signer.Chain Is Nothing Then
      Dim cCerts : cCerts = SignedCode.Signer.Chain.Certificates.Count
      Wscript.Stdout.Writeline "  Signing chain  : " & CStr(cCerts) & " certificate(s)"
      Wscript.Stdout.Writeline
   
      Dim Certificate
      Dim cIndex : cIndex = 0
      For Each Certificate In SignedCode.Signer.Chain.Certificates
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
   Const ARG_STATE_OPTIONS                   = 0   
   Const ARG_STATE_LOCATION                  = 1
   Const ARG_STATE_SHA1                      = 2
   Const ARG_STATE_SUBJECT                   = 3
   Const ARG_STATE_ISSUER                    = 4
   Const ARG_STATE_ROOT                      = 5
   Const ARG_STATE_PFX                       = 6
   Const ARG_STATE_PWD                       = 7
   Const ARG_STATE_DESCRIPTION               = 8
   Const ARG_STATE_DESCRIPTION_URL           = 9
   Const ARG_STATE_TIMESTAMP_SERVER_URL      = 10
   Const ARG_STATE_INCLUDE                   = 11
   Const ARG_STATE_ATTRIBUTE_NAME            = 12
   Const ARG_STATE_ATTRIBUTE_DESC            = 13
   Const ARG_STATE_ATTRIBUTE_TIME            = 14
   Const ARG_STATE_FILENAME                  = 15
   
   ' Parse command line.
   Dim Arg
   Dim ArgState : ArgState = ARG_STATE_OPTIONS
   
   For Each Arg In Wscript.Arguments
      Select Case Command
      Case Unknown
         Select Case UCase(Arg) 
         Case "SIGN"
            Command = Sign

         Case "TIMESTAMP"
            Command = Timestamp

         Case "VERIFY"
            Command = Verify

         Case Else
            DisplayUsage
            
         End Select
         
      Case Sign
         Select Case ArgState
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
               
            Case "-D", "/D"
               ArgState = ARG_STATE_DESCRIPTION
               
            Case "-URL", "/URL"
               ArgState = ARG_STATE_DESCRIPTION_URL
               
            Case "-T", "/T"
               ArgState = ARG_STATE_TIMESTAMP_SERVER_URL
               
            Case "-INCLUDE", "/INCLUDE"
               ArgState = ARG_STATE_INCLUDE
               
            Case "-AN", "/AN"
               ArgState = ARG_STATE_ATTRIBUTE_NAME
               
            Case "-AD", "/AD"
               ArgState = ARG_STATE_ATTRIBUTE_DESC
               
            Case "-AT", "/AT"
               ArgState = ARG_STATE_ATTRIBUTE_TIME
               
            Case "-V", "/V"
               bVerbose = True
               
            Case "-?", "/?"
               DisplayUsage
               
            Case Else
               If Left(Arg, 1) = "-"  OR Left(Arg, 1) = "/" Then
                  DisplayUsage
               Else
                  If IsReDimed(FileNames) Then
                     ReDim Preserve FileNames(UBound(FileNames) + 1)
                  Else
                     ReDim FileNames(0)
                  End If
                  FileNames(UBound(FileNames)) = Arg
               End If
               ArgState = ARG_STATE_FILENAME
               
            End Select

         Case ARG_STATE_FILENAME
            If Left(Arg, 1) = "-"  OR Left(Arg, 1) = "/" Then
               DisplayUsage
            Else
               ReDim Preserve FileNames(UBound(FileNames) + 1)
            End If
            FileNames(UBound(FileNames)) = Arg
             
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
         
         Case ARG_STATE_DESCRIPTION
            If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
               DisplayUsage
            Else
               Description = Arg            
            End If
            ArgState = ARG_STATE_OPTIONS
         
         Case ARG_STATE_DESCRIPTION_URL
            If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
               DisplayUsage
            Else
               DescriptionURL = Arg            
            End If
            ArgState = ARG_STATE_OPTIONS
         
         Case ARG_STATE_TIMESTAMP_SERVER_URL
            If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
               DisplayUsage
            Else
               TimestampURL = Arg            
            End If
            ArgState = ARG_STATE_OPTIONS

         Case ARG_STATE_INCLUDE
            If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
               DisplayUsage
            Else
               IncludeOption = CLng(Arg)
            End If
            ArgState = ARG_STATE_OPTIONS

         Case ARG_STATE_ATTRIBUTE_NAME
            If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
               DisplayUsage
            Else
               AttrName = Arg
            End If
            ArgState = ARG_STATE_OPTIONS
                      
         Case ARG_STATE_ATTRIBUTE_DESC
            If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
               DisplayUsage
            Else
               AttrDesc = Arg
            End If
            ArgState = ARG_STATE_OPTIONS
                      
         Case ARG_STATE_ATTRIBUTE_TIME
            If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
               DisplayUsage
            Else
               If UCase(Arg) = "NOW" Then
                  AttrTime = Now
               Else
                  AttrTime = CDate(Arg)
               End If
            End If
            ArgState = ARG_STATE_OPTIONS
         
         Case Else
            Wscript.Stdout.Writeline "Internal script error: Unknown argument state (" & CStr(ArgState) & ") encountered."
            Wscript.Quit(-3)
            
         End Select

      Case Timestamp
         Select Case ArgState
         Case ARG_STATE_OPTIONS
            Select Case UCase(Arg) 
            Case "-T", "/T"
               ArgState = ARG_STATE_TIMESTAMP_SERVER_URL
               
            Case "-V", "/V"
               bVerbose = True
               
            Case "-?", "/?"
               DisplayUsage
               
            Case Else
               If Left(Arg, 1) = "-"  OR Left(Arg, 1) = "/" Then
                  DisplayUsage
               Else
                  If IsReDimed(FileNames) Then
                     ReDim Preserve FileNames(UBound(FileNames) + 1)
                  Else
                     ReDim FileNames(0)
                  End If
                  FileNames(UBound(FileNames)) = Arg
               End If
               ArgState = ARG_STATE_FILENAME
             
            End Select
            
         Case ARG_STATE_FILENAME
            If Left(Arg, 1) = "-"  OR Left(Arg, 1) = "/" Then
               DisplayUsage
            Else
               ReDim Preserve FileNames(UBound(FileNames) + 1)
            End If
            FileNames(UBound(FileNames)) = Arg
             
         Case ARG_STATE_TIMESTAMP_SERVER_URL
            If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
               DisplayUsage
            Else
               TimestampURL = Arg            
            End If
            ArgState = ARG_STATE_OPTIONS
            
         Case Else
            Wscript.Stdout.Writeline "Internal script error: Unknown argument state (" & CStr(ArgState) & ") encountered."
            Wscript.Quit(-3)
            
         End Select
      
      Case Verify
         Select Case ArgState
         Case ARG_STATE_OPTIONS
            Select Case UCase(Arg) 
            Case "-UI", "/UI"
               bDisplayDialog = True
               
            Case "-V", "/V"
               bVerbose = True
               
            Case "-?", "/?"
               DisplayUsage
               
            Case Else
               If Left(Arg, 1) = "-"  OR Left(Arg, 1) = "/" Then
                  DisplayUsage
               Else
                  If IsReDimed(FileNames) Then
                     ReDim Preserve FileNames(UBound(FileNames) + 1)
                  Else
                     ReDim FileNames(0)
                  End If
                  FileNames(UBound(FileNames)) = Arg
               End If
               ArgState = ARG_STATE_FILENAME
               
            End Select
            
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
         
      Case Else
         Wscript.Stdout.Writeline "Internal script error: Unknown command (" & CStr(Command) & ") encountered."
         Wscript.Quit(-4)
         
      End Select
   Next
   
   ' Check options.
   Select Case Command
   Case Sign
      ' If -l, then cannot have -pfx.
      If (Not IsNull(StoreLocation)) And (Not IsNull(PFXFileName)) Then
         DisplayUsage
      End If
   
      ' If -pwd, then must have -pfx
      If (Not IsNull(PFXPassword)) And (IsNull(PFXFileName)) Then
          DisplayUsage
      End If
   
   Case Timestamp
      ' Must have the timestamp server's URL.
      If IsNull(TimestampURL) Then
         DisplayUsage
      End If
   
   Case Verify
      ' All are optionals.
   
   Case Else      
      DisplayUsage
      
   End Select

   ' Finally, make sure we have at least one filename.            
   If Not IsRedimed(FileNames) Then
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
'******************************************************************************

Sub DisplayUsage

   Select Case Command
      Case Unknown
         Wscript.Stdout.Writeline "Usage: CSignCode Command [Options] Filename ..."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Command:"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  Sign                   -- Sign executable file using Authenticode (TM)"
         Wscript.Stdout.Writeline "  Timestamp              -- Timestamp Authenticode signed executable"
         Wscript.Stdout.Writeline "  Verify                 -- Verify Authenticode signed executable"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "For help on a specific command, enter ""CSignCode Command -?"""
         
      Case Sign
         Wscript.Stdout.Writeline "Usage: CSignCode Sign [Options] Filename ..."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The Sign command is used to sign executable(s) using Microsoft Authenticode"
         Wscript.Stdout.Writeline "technology. Signing protects a file from tampering, and allows user to verify"
         Wscript.Stdout.Writeline "the signer based on signing certificate."
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
         Wscript.Stdout.Writeline "  -d       <description> -- Description of the signed content"
         Wscript.Stdout.Writeline "  -url     <URL>         -- URL where more information about the signed content"
         Wscript.Stdout.Writeline "                            can be found"
         Wscript.Stdout.Writeline "  -t       <URL>         -- The timestamp server's URL"
         Wscript.Stdout.Writeline "  -include <0 | 1 | 2>   -- Include option (0 for chain minus root, 1 for whole"
         Wscript.Stdout.Writeline "                            chain, or 2 for end certificate only (default to 0)"
         Wscript.Stdout.Writeline "  -an      <name>        -- Document name attribute"
         Wscript.Stdout.Writeline "  -ad      <description> -- Document description attribute"
         Wscript.Stdout.Writeline "  -at      <time>        -- Signing time attribute (""now"" for current time)"
         Wscript.Stdout.Writeline "  -v                     -- Verbose operation"
         Wscript.Stdout.Writeline "  -?                     -- This help screen"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Note: All non-fatal invalid options for this specific command will be ignored,"
         Wscript.Stdout.Writeline "      and the ** symbol indicates option can be listed multiple times."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "      If there is only one certificate found in the MY store or PFX that"
         Wscript.Stdout.Writeline "      matches the requirement, that particular certificate will be used."
	  	   Wscript.Stdout.Writeline "      However, if there is more than one certificate matching the requirement,"
         Wscript.Stdout.Writeline "      a dialog will be displayed to allow selection of the signing certificate."
         Wscript.Stdout.Writeline
         
       Case Timestamp
         Wscript.Stdout.Writeline "Usage: CSignCode Timestamp [Options] Filename ..."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The Timestamp command is used to add a timestamp signature to Authenticode"
         Wscript.Stdout.Writeline "signed executable(s)."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Options:"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  -t       <URL>         -- Specify the timestamp server's URL (required)"
         Wscript.Stdout.Writeline "  -v                     -- Verbose operation"
         Wscript.Stdout.Writeline "  -?                     -- This help screen"
         Wscript.Stdout.Writeline
         
       Case Verify
         Wscript.Stdout.Writeline "Usage: CSignCode Verify [Options] Filename ..."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The Verify command is used to verify Authenticode signature(s). Verification"
         Wscript.Stdout.Writeline "checks integrity of the signed executable(s) and determines if the signing"
         Wscript.Stdout.Writeline "certificate is valid and issued by a trusted party."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Options:"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  -ui                    -- Display Authenticode dialog"
         Wscript.Stdout.Writeline "  -v                     -- Verbose operation"
         Wscript.Stdout.Writeline "  -?                     -- This help screen"
         Wscript.Stdout.Writeline
         
      Case Else
         Wscript.Stdout.Writeline "Internal script error: Unknown help state (Command = " & CStr(Command) & ")."
         Wscript.Quit(-2)
         
   End Select

   Wscript.Quit(-1)
   
End Sub ' End DisplayUsage

