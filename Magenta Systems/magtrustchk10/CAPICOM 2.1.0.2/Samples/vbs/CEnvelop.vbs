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
' CEnvelop.vbs
'
' This is a sample script to illustrate how to use the CAPICOM's EnvelopedData 
' to envelop/decrypt text file.
'
' Note: For simplicity, this script does not handle exception.
'
'******************************************************************************

Option Explicit

Const ForReading = 1, ForWriting = 2

' Command.
Const Unknown                                      = 0
Const Encrypt                                      = 1
Const Decrypt                                      = 2
                                                   
' CAPICOM's constants.                                                   
Const CAPICOM_MEMORY_STORE                         = 0
Const CAPICOM_LOCAL_MACHINE_STORE                  = 1
Const CAPICOM_CURRENT_USER_STORE                   = 2
                                                   
Const CAPICOM_CERTIFICATE_FIND_SHA1_HASH           = 0
Const CAPICOM_CERTIFICATE_FIND_SUBJECT_NAME        = 1
Const CAPICOM_CERTIFICATE_FIND_ISSUER_NAME         = 2
Const CAPICOM_CERTIFICATE_FIND_ROOT_NAME           = 3
Const CAPICOM_CERTIFICATE_FIND_TEMPLATE_NAME       = 4
Const CAPICOM_CERTIFICATE_FIND_EXTENSION           = 5
Const CAPICOM_CERTIFICATE_FIND_EXTENDED_PROPERTY   = 6
Const CAPICOM_CERTIFICATE_FIND_APPLICATION_POLICY  = 7
Const CAPICOM_CERTIFICATE_FIND_CERTIFICATE_POLICY  = 8
Const CAPICOM_CERTIFICATE_FIND_TIME_VALID          = 9
Const CAPICOM_CERTIFICATE_FIND_TIME_NOT_YET_VALID  = 10
Const CAPICOM_CERTIFICATE_FIND_TIME_EXPIRED        = 11

Const CAPICOM_ENCRYPTION_ALGORITHM_RC2             = 0
Const CAPICOM_ENCRYPTION_ALGORITHM_RC4             = 1
Const CAPICOM_ENCRYPTION_ALGORITHM_DES             = 2
Const CAPICOM_ENCRYPTION_ALGORITHM_3DES            = 3
        
Const CAPICOM_ENCRYPTION_KEY_LENGTH_MAXIMUM        = 0
Const CAPICOM_ENCRYPTION_KEY_LENGTH_40_BITS        = 1
Const CAPICOM_ENCRYPTION_KEY_LENGTH_56_BITS        = 2
Const CAPICOM_ENCRYPTION_KEY_LENGTH_128_BITS       = 3

' Command line arguments.
Dim Command       : Command        = Unknown
Dim StoreLocation : StoreLocation  = Null
Dim StoreName     : StoreName      = "AddressBook"
Dim CertFile      : CertFile       = Null
Dim Algorithm     : Algorithm      = CAPICOM_ENCRYPTION_ALGORITHM_RC2
Dim KeyLength     : KeyLength      = CAPICOM_ENCRYPTION_KEY_LENGTH_MAXIMUM
Dim Verbose       : Verbose       = False
Dim FileNames()
                                                   
' Filters (some can be multiples).                 
Dim SHA1s()
Dim Subjects()

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

' Setup to find recipient's certificate(s).
Dim Certificate
Dim Certificates
If Command = Encrypt Then
   ' Open the store.
   Dim Store
   Set Store = CreateObject("CAPICOM.Store")
   Store.Open StoreLocation, StoreName
   
   ' Load the specified certificate file if specified.
   If Not IsNull(CertFile) Then
      Store.Load CertFile
   End If
   
   ' Find the certificate matching the search criteria, if requested.
   Set Certificates = Store.Certificates
   
   ' Note unlike other script samples, this is not a filtering process for
   ' recipient's selection. Instead, it is aggregate of each separate selection.
   Dim iIndex
   Dim SHA1Certs
   Set SHA1Certs = CreateObject("CAPICOM.Certificates")
   If Certificates.Count > 0 AND IsReDimed(SHA1s) Then
      For iIndex = LBound(SHA1s) to UBound(SHA1s) 
         Dim FoundSHA1s
         Set FoundSHA1s = Certificates.Find(CAPICOM_CERTIFICATE_FIND_SHA1_HASH, SHA1s(iIndex))
         
         For Each Certificate In FoundSHA1s
            Sha1Certs.Add Certificate
         Next
         Set FoundSHA1s = Nothing
      Next
   End If
   
   Dim NameCerts
   Set NameCerts = CreateObject("CAPICOM.Certificates")
   If Certificates.Count > 0 AND IsReDimed(Subjects) Then
      For iIndex = LBound(Subjects) To UBound(Subjects) 
         Dim FoundSubjects
         Set FoundSubjects = Certificates.Find(CAPICOM_CERTIFICATE_FIND_SUBJECT_NAME, Subjects(iIndex))

         For Each Certificate In FoundSubjects
            NameCerts.Add Certificate
         Next
         Set FoundSubjects = Nothing
      Next
   End If
   
   ' Now aggregate all found certificates (we use a memory store to perform
   ' the aggregation instead certificates is because Store.Add will drop
   ' duplicate, whereas Certificates.Add will not).
   Dim Store2
   Set Store2 = CreateObject("CAPICOM.Store")
   Store2.Open CAPICOM_MEMORY_STORE
   
   For Each Certificate In SHA1Certs
      Store2.Add Certificate
   Next
   Set SHA1Certs = Nothing
   
   For Each Certificate In NameCerts
      Store2.Add Certificate
   Next
   Set NameCerts = Nothing
   
   Set Certificates = Store2.Certificates
   Set Store2 = Nothing
   
   ' Finally, only select time valid certificates.
   If Certificates.Count > 0 Then
      Set Certificates = Certificates.Find(CAPICOM_CERTIFICATE_FIND_TIME_VALID, Now)
   End If
   
   ' Ask user to select certificate(s) if more than one is found.
   Select Case Certificates.Count
   Case 0
      Wscript.Stdout.Writeline "Error: No recipient's certificate can be found."
      Wscript.Quit(1)
   
   Case 1
      ' Only one, so this will be used.
         
   Case Else
      ' Allow user to further filter out recipients.
      Set Certificates = Certificates.Select("CEnvelop.vbs", "Please select one or more recipient's certificate(s).", True)
      If Certificates.Count = 0 Then
         Wscript.Stdout.Writeline "Error: Certificate selection dialog was cancelled."
         Wscript.Quit(2)
      End If
    
   End Select

   ' Free resources.
   Set Store = Nothing   
End If

' Now process the command.
Select Case Command
Case Encrypt   
   DoEncryptCommand FileNames, Certificates, Algorithm, KeyLength

Case Decrypt
   DoDecryptCommand FileNames
      
End Select

' Free resources.
Set Certificates = Nothing
     
Wscript.Quit(0)

' End Main

 
'******************************************************************************
'
' Subroutine: DoEncryptCommand
'
' Synopsis  : Envelop content of text file FileNames(0).
'
' Parameter : FileNames - Array of filenames.
'
'             Certificates - Recipient's certificate(s).
'
'             Algorithm - Encryption algorithm
'
'             KeyLength - Key length.
'
'******************************************************************************

Sub DoEncryptCommand (FileNames, Certificates, Algorithm, KeyLength)
   Dim Content
   Dim Message
   Dim EnvelopedData
   
   ' Create the EnvelopedData object.
   Set EnvelopedData = CreateObject("CAPICOM.EnvelopedData")
   
   ' Set algorithm and key size.
   EnvelopedData.Algorithm.Name = Algorithm
   EnvelopedData.Algorithm.KeyLength = KeyLength
   
   ' Add recipients.
   Dim Recipient
   For Each Recipient In Certificates 
      EnvelopedData.Recipients.Add Recipient
   Next
   
   ' Display main title.
   Wscript.Stdout.Writeline "Enveloping text file " & FileNames(0) & "."
   Wscript.Stdout.Writeline
   
   ' Display more detail for verbose operation.
   If Verbose Then
      DisplayDetail EnvelopedData
   End If
   
   ' Load content of text file to be enveloped.
   LoadFile FileNames(0), Content
   
   ' Now envelop it.
   EnvelopedData.Content = Content
   Message = EnvelopedData.Encrypt
   
   ' Finally, save enveloped message to out FileNames(1).
   SaveFile FileNames(1), Message
   Wscript.Stdout.Writeline "Successful - Enveloped message saved to " & FileNames(1) & "."

   ' Free resources.
   Set EnvelopedData = Nothing
   
End Sub ' End DoEncryptCommand


'******************************************************************************
'
' Subroutine: DoDecryptCommand
'
' Synopsis  : Decrypt an enveloped file.
'
' Parameter : FileNames - Array of filenames.
'
' Remarks   : CAPICOM will automatically search for the decrypting certificate
'             in CurrentUser/My or LocalMachine/My store. If no certificate
'             in either of these stores matches any of the recipients, then
'             an exception is thrown.
'
'******************************************************************************

Sub DoDecryptCommand (FileNames)
   Dim Message
   Dim EnvelopedData
   
   ' Create the EnvelopedData object.
   Set EnvelopedData = CreateObject("CAPICOM.EnvelopedData")
   
   ' Display main title.
   Wscript.Stdout.Writeline "Decrypting enveloped text file " & FileNames(0) & "."
   Wscript.Stdout.Writeline
   
   ' Load the enveloped message.
   LoadFile FileNames(0), Message
   
   ' Now decrypt it.
   EnvelopedData.Decrypt(Message)
   
   ' Display more detail for verbose operation.
   If Verbose Then
      DisplayDetail EnvelopedData
   End If
      
   ' Finally, save decrypted content to out FileNames(1).
   SaveFile FileNames(1), EnvelopedData.Content
   Wscript.Stdout.Writeline "Successful - Decrypted content saved to " & FileNames(1) & "."

   ' Free resources.
   Set EnvelopedData = Nothing
   
End Sub ' End DoDecryptCommand


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
' Parameter : EnvelopedData  - EnvelopedData object.
'
'******************************************************************************

Sub DisplayDetail (EnvelopedData)
   Const CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME = 0
   
   Dim AlgoNames(3)
   AlgoNames(0) = "RC2"
   AlgoNames(1) = "RC4"
   AlgoNames(2) = "DES"
   AlgoNames(3) = "3DES"

   Wscript.Stdout.Writeline "Algorithm: " & AlgoNames(EnvelopedData.Algorithm.Name) & "."
   Wscript.Stdout.Write "Key length: "
   
   Select Case EnvelopedData.Algorithm.KeyLength
   Case CAPICOM_ENCRYPTION_KEY_LENGTH_40_BITS
      Wscript.Stdout.Writeline "40 bits"
      
   Case CAPICOM_ENCRYPTION_KEY_LENGTH_56_BITS
      Wscript.Stdout.Writeline "56 bits"

   Case CAPICOM_ENCRYPTION_KEY_LENGTH_128_BITS
      Wscript.Stdout.Writeline "128 bits"
      
   Case Else
      Wscript.Stdout.Writeline "maximum"
                                             
   End Select
   Wscript.Stdout.Writeline 
   
   Dim cIndex      : cIndex = 0
   Dim cRecipients : cRecipients = EnvelopedData.Recipients.Count  
   Dim Recipient
   For Each Recipient In EnvelopedData.Recipients
      cIndex = cIndex + 1
      Wscript.Stdout.Writeline "=== Recipient " & CStr(cIndex) & " of " & CStr(cRecipients) & " ==="
      Wscript.Stdout.Writeline Recipient.GetInfo(CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME)
      Wscript.Stdout.Writeline
   Next
   
End Sub ' End DisplayDetail


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
   Const ARG_STATE_COMMAND    = 0
   Const ARG_STATE_OPTIONS    = 1    
   Const ARG_STATE_LOCATION   = 2 
   Const ARG_STATE_CERTFILE   = 3 
   Const ARG_STATE_SHA1       = 4
   Const ARG_STATE_SUBJECT    = 5 
   Const ARG_STATE_ALGORITHM  = 6
   Const ARG_STATE_LENGTH     = 7
   Const ARG_STATE_FILENAME   = 8
   
   ' Parse command line.
   Dim Arg
   Dim ArgState : ArgState = ARG_STATE_COMMAND
   
   For Each Arg In Wscript.Arguments
      Select Case ArgState
      Case ARG_STATE_COMMAND
         Select Case UCase(Arg) 
         Case "ENCRYPT"
            Command = Encrypt

         Case "DECRYPT"
            Command = Decrypt

         Case Else
            DisplayUsage
            
         End Select
         
         ArgState = ARG_STATE_OPTIONS
         
      Case ARG_STATE_OPTIONS
         Select Case UCase(Arg) 
         Case "-L", "/L"
            ArgState = ARG_STATE_LOCATION
            
         Case "-CERTFILE", "/CERTFILE"
            ArgState = ARG_STATE_CERTFILE
            
         Case "-SHA1", "/SHA1"
            ArgState = ARG_STATE_SHA1
            
         Case "-SUBJECT", "/SUBJECT"
            ArgState = ARG_STATE_SUBJECT
                        
         Case "-ALG", "/ALG"
            ArgState = ARG_STATE_ALGORITHM
                        
         Case "-LENGTH", "/LENGTH"
            ArgState = ARG_STATE_LENGTH
                        
         Case "-V", "/V"
            Verbose = True
            
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
            
            Case Else
               DisplayUsage
               
            End Select
         End If
         ArgState = ARG_STATE_OPTIONS
      
      Case ARG_STATE_CERTFILE
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            CertFile = Arg
         End If
         ArgState = ARG_STATE_OPTIONS
         
      Case ARG_STATE_SHA1
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            If IsReDimed(SHA1s) Then
               ReDim Preserve SHA1s(UBound(SHA1s) + 1)
            Else
               ReDim SHA1s(0)
            End If
            SHA1s(UBound(SHA1s)) = Arg            
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
         
      Case ARG_STATE_ALGORITHM
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            Select Case UCase(Arg)
            Case "RC2"
               Algorithm = CAPICOM_ENCRYPTION_ALGORITHM_RC2
               
            Case "RC4"
               Algorithm = CAPICOM_ENCRYPTION_ALGORITHM_RC4
               
            Case "DES"
               Algorithm = CAPICOM_ENCRYPTION_ALGORITHM_DES
               
            Case "3DES"
               Algorithm = CAPICOM_ENCRYPTION_ALGORITHM_3DES
               
            Case Else
               DisplayUsage
               
            End Select
         End If
         ArgState = ARG_STATE_OPTIONS
         
      Case ARG_STATE_LENGTH
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            Select Case UCase(Arg)
            Case "40"
               KeyLength = CAPICOM_ENCRYPTION_KEY_LENGTH_40_BITS
               
            Case "56"
               KeyLength = CAPICOM_ENCRYPTION_KEY_LENGTH_56_BITS
               
            Case "128"
               KeyLength = CAPICOM_ENCRYPTION_KEY_LENGTH_128_BITS
               
            Case "MAX"
               KeyLength = CAPICOM_ENCRYPTION_KEY_LENGTH_MAXIMUM
               
            Case Else
               DisplayUsage
               
            End Select
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
   Case Encrypt
      ' -l and -certfile are exclusive. 
      If Not IsNull(CertFile) Then
        If Not IsNull(StoreLocation) Then
           DisplayUsage
        Else
           StoreName = CertFile
           StoreLocation = CAPICOM_MEMORY_STORE
        End If
      End If

      ' Must have both in and out file.
      If (UBound(FileNames) - LBound(FileNames) + 1) <> 2 Then
         DisplayUsage
      End If
   
   Case Decrypt
      ' Must have both in and out file.
      If (UBound(FileNames) - LBound(FileNames) + 1) <> 2 Then
         DisplayUsage
      End If
      
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
'******************************************************************************

Sub DisplayUsage

   Select Case Command
      Case Unknown
         Wscript.Stdout.Writeline "Usage: CEnvelop Command [Options] InFile OutFile"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Command:"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  Encrypt                    -- Envelop a text file"
         Wscript.Stdout.Writeline "  Decrypt                    -- Decrypt an enveloped text file"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "For help on a specific command, enter ""CEnvelop Command -?"""
         
      Case Encrypt
         Wscript.Stdout.Writeline "Usage: CEnvelop Encrypt [Options] ContentFile EnvelopedFile"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The Encrypt command is used to envelop a text file to multiple recipients, of"
         Wscript.Stdout.Writeline "which the certificates must either reside in the AddressBook store, or in the"
         Wscript.Stdout.Writeline "specified certificate file. Enveloping protects the data from being read by"
         Wscript.Stdout.Writeline "others except the intended recipients."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Options:"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  -l        <location>       -- CU or LM (default to CU)"
         Wscript.Stdout.Writeline "  -certfile <certfile>       -- Certificate file, CER, SST, P7B, etc,"
         Wscript.Stdout.Writeline "                                (exclusive with -l)"
         Wscript.Stdout.Writeline "  -sha1     <hash>           ** SHA1 hash of the recipient's certificate"
         Wscript.Stdout.Writeline "  -subject  <name>           ** Subject Name of the recipient's certificate must"
         Wscript.Stdout.Writeline "                                contain this name"
         Wscript.Stdout.Writeline "  -alg      <algorithm>      -- RC2, RC4, DES, or 3DES (default to RC2)"
         Wscript.Stdout.Writeline "  -length   <key length>     -- 40, 56, 128 or MAX (default to MAX, and ignored"
         Wscript.Stdout.Writeline "                                for DES or 3DES)"
         Wscript.Stdout.Writeline "  -v                         -- Verbose operation"
         Wscript.Stdout.Writeline "  -?                         -- This help screen"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  ContentFile                -- Text file to be enveloped"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  EnvelopedFile              -- Enveloped text file"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Note: All non-fatal invalid options for this specific command will be ignored,"
         Wscript.Stdout.Writeline "      and the ** symbol indicates option can be listed multiple times."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "      If there is only one certificate found matching the requirement, that"
         Wscript.Stdout.Writeline "      particular certificate will be used as the recipient's certificate."
	  	   Wscript.Stdout.Writeline "      However, if there is more than one certificate matching the requirement,"
         Wscript.Stdout.Writeline "      a dialog will be displayed to allow selection of multiple recipient's"
         Wscript.Stdout.Writeline "      certificates."
         Wscript.Stdout.Writeline
        
       Case Decrypt
         Wscript.Stdout.Writeline "Usage: CEnvelop Decrypt [Options] EnvelopedFile ContentFile"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The Decrypt command is used to decrypt an enveloped text file. The decryption"
         Wscript.Stdout.Writeline "process will automatically search for the recipient's certificate in MY store"
         Wscript.Stdout.Writeline "(Current User or Local Machine)."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Options:"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  -v                         -- Verbose operation"
         Wscript.Stdout.Writeline "  -?                         -- This help screen"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  EnvelopedFile              -- Enveloped text file"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  ContentFile                -- Decrypted text file"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Note: All non-fatal invalid options for this specific command will be ignored."
         Wscript.Stdout.Writeline
         
      Case Else
         Wscript.Stdout.Writeline "Internal script error: Unknown help state (Command = " & CStr(Command) & ")."
         Wscript.Quit(-2)
         
   End Select

   Wscript.Quit(-1)
   
End Sub ' End DisplayUsage

