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
' CHashData.vbs
'
' This is a sample script to illustrate how to use the CAPICOM's HashedData 
' to hash a text file, and to verify a hash against the original text file.
'
' Note: For simplicity, this script does not handle exception.
'
'******************************************************************************

Option Explicit

Const ForReading = 1, ForWriting       = 2

' Command.
Const Unknown                          = 0
Const Hash                             = 1
Const Verify                           = 2
                                      
' CAPICOM's constants.                  
Const CAPICOM_HASH_ALGORITHM_SHA1      = 0
Const CAPICOM_HASH_ALGORITHM_MD2       = 1
Const CAPICOM_HASH_ALGORITHM_MD4       = 2
Const CAPICOM_HASH_ALGORITHM_MD5       = 3
Const CAPICOM_HASH_ALGORITHM_SHA256    = 4
Const CAPICOM_HASH_ALGORITHM_SHA384    = 5
Const CAPICOM_HASH_ALGORITHM_SHA512    = 6

' Command line arguments.
Dim Command       : Command            = Unknown
Dim Algorithm     : Algorithm          = CAPICOM_HASH_ALGORITHM_SHA1
Dim bVerbose      : bVerbose           = False
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
Case Hash   
   DoHashCommand FileNames, Algorithm

Case Verify
   DoVerifyCommand FileNames, Algorithm
      
End Select
    
Wscript.Quit(0)

' End Main

 
'******************************************************************************
'
' Subroutine: DoHashCommand
'
' Synopsis  : Hash content of text file FileNames(0).
'
' Parameter : FileNames - Array of filenames.
'
'             Algorithm - Hashing algorithm
'
'******************************************************************************

Sub DoHashCommand (FileNames, Algorithm)
   Dim Content
   Dim HashValue
   Dim HashedData
   
   ' Create the HashedData object.
   Set HashedData = CreateObject("CAPICOM.HashedData")
   
   ' Set algorithm.
   HashedData.Algorithm = Algorithm
   
   ' Display main title.
   Wscript.Stdout.Writeline "Hashing text file " & FileNames(0) & "."
   Wscript.Stdout.Writeline

   ' Load content of text file to be hashed.
   LoadFile FileNames(0), Content
   
   ' Now hash it.
   HashedData.Hash Content
   HashValue = HashedData.Value
      
   ' Display more detail for verbose operation.
   If bVerbose Then
      DisplayDetail Algorithm, HashValue, Null
   End If
   
   ' Finally, save hashed value to FileNames(1).
   SaveFile FileNames(1), HashValue
   Wscript.Stdout.Writeline "Successful - Hash saved to " & FileNames(1) & "."
      
   ' Free resources.
   Set HashedData = Nothing
   
End Sub ' End DoHashCommand


'******************************************************************************
'
' Subroutine: DoVerifyCommand
'
' Synopsis  : Verify a hashed file.
'
' Parameter : FileNames - Array of filenames.
'
'             Algorithm - Hashing algorithm
'
'******************************************************************************

Sub DoVerifyCommand (FileNames, Algorithm)
   Dim Content
   Dim HashValue1
   Dim HashValue2
   Dim HashedData
   
   ' Create the HashedData object.
   Set HashedData = CreateObject("CAPICOM.HashedData")
   
   ' Set algorithm.
   HashedData.Algorithm = Algorithm
   
   ' Display main title.
   Wscript.Stdout.Writeline "Verifying hash for text file " & FileNames(0) & "."
   Wscript.Stdout.Writeline
   
   ' Load content of text file to be verified.
   LoadFile FileNames(0), Content
   
   ' Load the hashed value.
   LoadFile FileNames(1), HashValue1
   
   ' Now hash the original text file.
   HashedData.Hash Content
   HashValue2 = HashedData.Value

   ' Display more detail for verbose operation.
   If bVerbose Then
      DisplayDetail Algorithm, HashValue1, HashValue2
   End If
   
   ' Now compare the hashes.
   If HashValue1 = HashValue2 Then
      Wscript.Stdout.Writeline "Successful - The file " & FileNames(0) & " has not been tampered with."
   Else
      Wscript.Stdout.Writeline "Error - The file " & FileNames(0) & " does not produce the same hash."   
   End If
   
   ' Free resources.
   Set HashedData = Nothing

End Sub ' End DoVerifyCommand


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
' Parameter : Algorithm - Hash algorithm.
'
'             HashValue1 - First hash value.
'
'             HashValue2 - Second hash value.
'
'******************************************************************************

Sub DisplayDetail (Algorithm, HashValue1, HashValue2)
   Dim AlgoNames(3)
   AlgoNames(0) = "SHA1"
   AlgoNames(1) = "MD2"
   AlgoNames(2) = "MD4"
   AlgoNames(3) = "MD5"
   AlgoNames(4) = "SHA256"
   AlgoNames(5) = "SHA384"
   AlgoNames(6) = "SHA512"
   
   Wscript.Stdout.Writeline "Algorithm : " & AlgoNames(Algorithm)
   Wscript.Stdout.Writeline "Hash value: " & HashValue1
   If Not IsNull(HashValue2) Then
      Wscript.Stdout.Writeline "Hash value: " & HashValue2
   End If
   
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
   Const ARG_STATE_FILENAME   = 3
   Const ARG_STATE_END        = 4
   
   ' Parse command line.
   Dim Arg
   Dim ArgState : ArgState = ARG_STATE_COMMAND
   
   For Each Arg In Wscript.Arguments
      Select Case ArgState
      Case ARG_STATE_COMMAND
         Select Case UCase(Arg) 
         Case "HASH"
            Command = Hash

         Case "VERIFY"
            Command = Verify

         Case Else
            DisplayUsage
            
         End Select
         
         ArgState = ARG_STATE_OPTIONS
         
      Case ARG_STATE_OPTIONS
         Select Case UCase(Arg) 
         Case "-ALG", "/ALG"
            ArgState = ARG_STATE_ALGORITHM
                        
         Case "-E", "/E"
            ArgState = ARG_STATE_ENCODING
                        
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

      Case ARG_STATE_ALGORITHM
         If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
            DisplayUsage
         Else
            Select Case UCase(Arg)
            Case "SHA1"
               Algorithm = CAPICOM_HASH_ALGORITHM_SHA1
               
            Case "MD2"
               Algorithm = CAPICOM_HASH_ALGORITHM_MD2
               
            Case "MD4"
               Algorithm = CAPICOM_HASH_ALGORITHM_MD4
               
            Case "MD5"
               Algorithm = CAPICOM_HASH_ALGORITHM_MD5

            Case "SHA256"
               Algorithm = CAPICOM_HASH_ALGORITHM_SHA256
               
            Case "SHA384"
               Algorithm = CAPICOM_HASH_ALGORITHM_SHA384
               
            Case "SHA512"
               Algorithm = CAPICOM_HASH_ALGORITHM_SHA512
               
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
         Wscript.Stdout.Writeline "Usage: CHashData Command [Options] ContentFile HashedFile"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Command:"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  Hash                      -- Hash a text file"
         Wscript.Stdout.Writeline "  Verify                    -- Verify the hash"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "For help on a specific command, enter ""CHashData Command -?"""
         
      Case Hash
         Wscript.Stdout.Writeline "Usage: CHashData Hash [Options] ContentFile HashedFile"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The Hash command is used to hash a text file. Hashing prevents the data from"
         Wscript.Stdout.Writeline "tampering. The hash will be written to HashedFile."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Options:"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  -alg <algorithm>           -- SHA1, MD2, MD4, or MD5, (default to SHA1)"
         Wscript.Stdout.Writeline "  -v                         -- Verbose operation"
         Wscript.Stdout.Writeline "  -?                         -- This help screen"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  ContentFile                -- Text file to be hashed"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  HashedFile                 -- Hashed file"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Note: All non-fatal invalid options for this specific command will be ignored."
         Wscript.Stdout.Writeline
        
       Case Verify
         Wscript.Stdout.Writeline "Usage: CHashData Verify [Options] ContentFile HashedFile"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "The Verify command is used to verify the hash of a text file."
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Options:"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  -alg <algorithm>           -- SHA1, MD2, MD4, or MD5, (default to SHA1)"
         Wscript.Stdout.Writeline "  -v                         -- Verbose operation"
         Wscript.Stdout.Writeline "  -?                         -- This help screen"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  ContentFile                -- Text file to be hashed"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "  HashedFile                 -- Hashed file"
         Wscript.Stdout.Writeline
         Wscript.Stdout.Writeline "Note: All non-fatal invalid options for this specific command will be ignored."
         Wscript.Stdout.Writeline
         
      Case Else
         Wscript.Stdout.Writeline "Internal script error: Unknown help state (Command = " & CStr(Command) & ")."
         Wscript.Quit(-2)
         
   End Select

   Wscript.Quit(-1)
   
End Sub ' End DisplayUsage

