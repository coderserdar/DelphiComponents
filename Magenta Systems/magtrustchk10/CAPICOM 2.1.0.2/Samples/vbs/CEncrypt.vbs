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
' CEncrypt.vbs
'
' This is a sample script to illustrate how to use the CAPICOM's EncryptedData 
' to encrypt/decrypt text file.
'
' Note: For simplicity, this script does not handle exception.
'
'******************************************************************************

Option Explicit

Const ForReading = 1, ForWriting = 2

' Command.
Const Unknown                                = 0
Const Encrypt                                = 1
Const Decrypt                                = 2
                                             
' CAPICOM's constants.                                             
Const CAPICOM_ENCRYPTION_ALGORITHM_RC2       = 0
Const CAPICOM_ENCRYPTION_ALGORITHM_RC4       = 1
Const CAPICOM_ENCRYPTION_ALGORITHM_DES       = 2
Const CAPICOM_ENCRYPTION_ALGORITHM_3DES      = 3
Const CAPICOM_ENCRYPTION_ALGORITHM_AES       = 4
        
Const CAPICOM_ENCRYPTION_KEY_LENGTH_MAXIMUM  = 0
Const CAPICOM_ENCRYPTION_KEY_LENGTH_40_BITS  = 1
Const CAPICOM_ENCRYPTION_KEY_LENGTH_56_BITS  = 2
Const CAPICOM_ENCRYPTION_KEY_LENGTH_128_BITS = 3
Const CAPICOM_ENCRYPTION_KEY_LENGTH_192_BITS = 4
Const CAPICOM_ENCRYPTION_KEY_LENGTH_256_BITS = 5

' Command line arguments.
Dim Command       : Command                  = Unknown
Dim Password      : Password                 = Null
Dim Algorithm     : Algorithm                = CAPICOM_ENCRYPTION_ALGORITHM_RC2
Dim KeyLength     : KeyLength                = CAPICOM_ENCRYPTION_KEY_LENGTH_MAXIMUM
Dim Verbose       : Verbose                  = False
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

' Now process the command.
Select Case Command
Case Encrypt   
   DoEncryptCommand FileNames, Algorithm, KeyLength, Password

Case Decrypt
   DoDecryptCommand FileNames, Password
      
End Select
    
Wscript.Quit(0)

' End Main

 
'******************************************************************************
'
' Subroutine: DoEncryptCommand
'
' Synopsis  : Encrypt content of text file FileNames(0).
'
' Parameter : FileNames - Array of filenames.
'
'             Algorithm - Encryption algorithm
'
'             KeyLength - Key size.
'
'             Password - Secret password.
'
'******************************************************************************

Sub DoEncryptCommand (FileNames, Algorithm, KeyLength, Password)
   Dim Content
   Dim Message
   Dim EncryptedData
   
   ' Create the EncryptedData object.
   Set EncryptedData = CreateObject("CAPICOM.EncryptedData")
   
   ' Set algorithm, key size, and encryption password.
   EncryptedData.Algorithm.Name = Algorithm
   EncryptedData.Algorithm.KeyLength = KeyLength
   EncryptedData.SetSecret Password
   
   ' Display main title.
   Wscript.Stdout.Writeline "Encrypting text file " & FileNames(0) & "."
   Wscript.Stdout.Writeline
   
   ' Display more detail for verbose operation.
   If Verbose Then
      DisplayDetail EncryptedData
   End If
   
   ' Load content of text file to be encrypted.
   LoadFile FileNames(0), Content
   
   ' Now encrypt it.
   EncryptedData.Content = Content
   Message = EncryptedData.Encrypt
   
   ' Finally, save encrypted message to FileNames(1).
   SaveFile FileNames(1), Message
   Wscript.Stdout.Writeline "Successful - Encrypted message saved to " & FileNames(1) & "."

   ' Free resources.
   Set EncryptedData = Nothing
   
End Sub ' End DoEncryptCommand


'******************************************************************************
'
' Subroutine: DoDecryptCommand
'
' Synopsis  : Decrypt an encrypted file.
'
' Parameter : FileNames - Array of filenames.
'
'             Password - Secret password.
'
'******************************************************************************

Sub DoDecryptCommand (FileNames, Password)
   Dim Message
   Dim EncryptedData
   
   ' Create the EncryptedData object.
   Set EncryptedData = CreateObject("CAPICOM.EncryptedData")
   
   ' Set decryption password.
   EncryptedData.SetSecret Password
   
   ' Display main title.
   Wscript.Stdout.Writeline "Decrypting encrypted text file " & FileNames(0) & "."
   Wscript.Stdout.Writeline
   
   ' Load the encrypted message.
   LoadFile FileNames(0), Message
   
   ' Now decrypt it.
   EncryptedData.Decrypt(Message)
   
   ' Display more detail for verbose operation.
   If Verbose Then
      DisplayDetail EncryptedData
   End If
      
   ' Finally, save decrypted content to FileNames(1).
   SaveFile FileNames(1), EncryptedData.Content
   Wscript.Stdout.Writeline "Successful - Decrypted content saved to " & FileNames(1) & "."
   
   ' Free resources.
   Set EncryptedData = Nothing

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
' Parameter : EncryptedData - EncryptedData object.
'
'******************************************************************************

Sub DisplayDetail (EncryptedData)
   Dim AlgoNames(4)
   AlgoNames(0) = "RC2"
   AlgoNames(1) = "RC4"
   AlgoNames(2) = "DES"
   AlgoNames(3) = "3DES"
   AlgoNames(4) = "AES"

   Wscript.Stdout.Writeline "Algorithm : " & AlgoNames(EncryptedData.Algorithm.Name)
   Wscript.Stdout.Write     "Key length: "
   
   Select Case EncryptedData.Algorithm.KeyLength
   Case CAPICOM_ENCRYPTION_KEY_LENGTH_40_BITS
      Wscript.Stdout.Writeline "40 bits"
      
   Case CAPICOM_ENCRYPTION_KEY_LENGTH_56_BITS
      Wscript.Stdout.Writeline "56 bits"

   Case CAPICOM_ENCRYPTION_KEY_LENGTH_128_BITS
      Wscript.Stdout.Writeline "128 bits"
      
   Case CAPICOM_ENCRYPTION_KEY_LENGTH_192_BITS
      Wscript.Stdout.Writeline "192 bits"
      
   Case CAPICOM_ENCRYPTION_KEY_LENGTH_256_BITS
      Wscript.Stdout.Writeline "256 bits"
      
   Case Else
      Wscript.Stdout.Writeline "Maximum"
                                             
   End Select
   Wscript.Stdout.Writeline 
   
End Sub ' End DisplayDetail


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
   Const ARG_STATE_ALGORITHM  = 2
   Const ARG_STATE_LENGTH     = 3
   Const ARG_STATE_FILENAME   = 4
   Const ARG_STATE_PASSWORD   = 5
   Const ARG_STATE_END        = 6
   
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
               
            Case "AES"
               Algorithm = CAPICOM_ENCRYPTION_ALGORITHM_AES
               
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
               
            Case "192"
               KeyLength = CAPICOM_ENCRYPTION_KEY_LENGTH_192_BITS
               
            Case "256"
               KeyLength = CAPICOM_ENCRYPTION_KEY_LENGTH_256_BITS
               
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
            FileNames(UBound(FileNames)) = Arg
         End If
         ArgState = ARG_STATE_PASSWORD
                   
      Case ARG_STATE_PASSWORD
         If Left(Arg, 1) = "-"  OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            Password = Arg
         End If
         ArgState = ARG_STATE_END
          
      Case Else
         Wscript.Stdout.Writeline "Internal script error: Unknown argument state (" & CStr(ArgState) & ") encountered."
         Wscript.Quit(-3)
         
      End Select
   Next
   
   ' Make sure we are in good state.
   If ArgState <> ARG_STATE_END Then
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
         Wscript.Stdout.Writeline "Usage: CEncrypt Command [Options] InFile OutFile Password"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Command:"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  Encrypt                    -- Encrypt a text file"
         Wscript.Stdout.Writeline "  Decrypt                    -- Decrypt an encrypted text file"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "For help on a specific command, enter ""CEncrypt Command -?"""
         
      Case Encrypt
         Wscript.Stdout.Writeline "Usage: CEncrypt Encrypt [Options] ContentFile EncryptedFile Password"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The Encrypt command is used to encrypt a text file based on a secret password."
         Wscript.Stdout.Writeline "Encrypting protects the data from being read by others except those who know"
         Wscript.Stdout.Writeline "the secret password."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Options:"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  -alg    <algorithm>        -- RC2, RC4, DES, 3DES, or AES (default to RC2)"
         Wscript.Stdout.Writeline "  -length <key length>       -- 40, 56, 128, 192, 256, or MAX (default to MAX,"
         Wscript.Stdout.Writeline "                                and ignored for DES or 3DES)"
         Wscript.Stdout.Writeline "  -v                         -- Verbose operation"
         Wscript.Stdout.Writeline "  -?                         -- This help screen"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  ContentFile                -- Text file to be encrypted"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  EncryptedFile              -- Encrypted text file"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Note: All non-fatal invalid options for this specific command will be ignored."
         Wscript.Stdout.Writeline
        
       Case Decrypt
         Wscript.Stdout.Writeline "Usage: CEncrypt Decrypt [Options] EncryptedFile ContentFile Password"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The Decrypt command is used to decrypt an encrypted text file."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Options:"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  -v                         -- Verbose operation"
         Wscript.Stdout.Writeline "  -?                         -- This help screen"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  EncryptedFile              -- Encrypted text file"
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

