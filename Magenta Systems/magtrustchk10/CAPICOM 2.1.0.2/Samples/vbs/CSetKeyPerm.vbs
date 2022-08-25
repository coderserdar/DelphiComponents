'******************************************************************************
'
' THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND, 
' EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED 
' WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
'
' Copyright (C) 1999- 2003.  Microsoft Corporation.  All rights reserved.
'
'******************************************************************************
'
' CSetKeyPerm
'
' This is a sample script to illustrate how to use the CAPICOM to determin which
' file contains the private key associated with a specific certificate and how
' to Set permissions on this file.
'
' Note: For simplicity, this script does not handle exception.
'
'******************************************************************************

Option Explicit

' CAPICOM's constants.  
Const CAPICOM_LOCAL_MACHINE_STORE		= 1
Const CAPICOM_CURRENT_USER_STORE		= 2

Const CAPICOM_MY_STORE				= "My"

Const CAPICOM_STORE_OPEN_READ_ONLY		= 0
const CAPICOM_CERTIFICATE_FIND_SHA1_HASH = 0
const CAPICOM_CERTIFICATE_FIND_EXTENDED_PROPERTY = 6

const CAPICOM_PROPID_KEY_PROV_INFO = 2

Dim oStore
Dim oCertificate
Dim oCertificates
Dim Display: Display = False
'Dim Remove
Dim SerialNumbertofind
Dim UserName: UserName = "BUILTIN\Administrators"
Dim ACL: ACL = "R"
Dim FileName
Dim SHA1
Dim StoreToSearch: StoreToSearch = CAPICOM_LOCAL_MACHINE_STORE


'First make sure the script is executed by CScript.exe.
If InStr(1, UCase(Wscript.FullName), "CSCRIPT.EXE", vbTextCompare) = 0 Then
   Wscript.Echo "This script can only be executed by CScript.exe." & vbCRLF & vbCRLF &_
                "You can either:" & vbCRLF & vbCRLF & _
                "1. Set CScript.exe as the default (Run CScript //h:cscript), or" & vbCRLF & _
                "2. Run CScript.exe directly as in, CScript " &	Wscript.ScriptName & "."
   Wscript.Quit(-1)
End If

'This script uses file paths that are only valid on Windows 2000 or greater, so block
'running on any OS less than Windows 2000.
if IsWin2KOrGreater = False then
   Wscript.Echo "This script can only be executed on Windows 2000 or greater."
   Wscript.Quit(-1)
end if


' Parse the command line.
ParseCommandLine

if StoreToSearch = CAPICOM_LOCAL_MACHINE_STORE and not IsAdmin() then
	WScript.Echo "To change the the Permmissions of the keys in the local machine store"
	WScript.Echo "please login as an Administrator"
	WScript.Quit(-1)
end if

Set oStore = CreateObject("CAPICOM.Store")
Set oCertificates = CreateObject("CAPICOM.Certificates")

oStore.Open StoreToSearch, CAPICOM_MY_STORE, CAPICOM_STORE_OPEN_READ_ONLY

for each oCertificate in oStore.certificates.find(CAPICOM_CERTIFICATE_FIND_EXTENDED_PROPERTY, CAPICOM_PROPID_KEY_PROV_INFO)
	if IsManagedByMicrosoftSoftwareCSP(oCertificate) then oCertificates.Add(oCertificate)
next

if SHA1 <> "" then
	set oCertificates = oStore.Certificates.Find(CAPICOM_CERTIFICATE_FIND_SHA1_HASH, SHA1)
end if

if oCertificates.Count > 1 or SHA1 = "" then
	set oCertificates = oCertificates.Select("Please select certificate(s)", "Select the certificate(s) you want to operate against.", TRUE)
end if

if Display = TRUE then
	For each oCertificate in oCertificates
		FileName = FindKeyContainerFromCertificate(oCertificate)
		DisplayFileACL FileName
	next
	Wscript.Quit(0)
end if

For each oCertificate in oCertificates
	FileName = FindKeyContainerFromCertificate(oCertificate)
	SetFileACL UserName, FileName, ACL
	DisplayFileACL FileName
next

' We are all done.
Wscript.Quit(0)



'******************************************************************************
'
' Function	: IsManagedByMicrosoftSoftwareCSP
'
' Synopsis  : Check if the supplied cert uses one of the standard MS CSPs
'
' Parameter : Certificate: Certificate the is to be checked
'
' Returns	: The key container file name under the All Users store
'******************************************************************************
Function IsManagedByMicrosoftSoftwareCSP(oCertificate)
	Dim MSCSPs 
	Dim CSP
	MSCSPs = Array("Microsoft RSA SChannel Cryptographic Provider", _
					   "Microsoft RSA SChannel Cryptographic Provider", _
					   "Microsoft Enhanced RSA and AES Cryptographic Provider", _
					   "Microsoft Enhanced Cryptographic Provider v1.0", _
					   "Microsoft DH SChannel Cryptographic Provider", _
					   "Microsoft Base DSS Cryptographic Provider", _
					   "Microsoft Base Cryptographic Provider v1.0", _
					   "Microsoft Strong Cryptographic Provider", _
					   "Microsoft Enhanced DSS and Diffie-Hellman Cryptographic Provider", _
					   "Microsoft Base DSS and Diffie-Hellman Cryptographic Provider")

	IsManagedByMicrosoftSoftwareCSP = False
	for each CSP in MSCSPs
		if oCertificate.PrivateKey.ProviderName = CSP then IsManagedByMicrosoftSoftwareCSP = True
	next
End Function

'******************************************************************************
'
' Function	: FindKeyContainerFromCertificate
'
' Synopsis  : Find the file containing the private for the cert specified
'
' Parameter : Certficate: Certficate object that we want to get the key container
'						  filename from
'
' Returns	: The key container's file name with full path
'******************************************************************************
Function FindKeyContainerFromCertificate(oCertificate)
	Dim KeyType
	Dim KeyPath
	Dim KeyFolder
	Dim fso, f, f1, s, sf
	Dim WshShell
	
	Set WshShell = WScript.CreateObject("WScript.Shell")	
	
	if StoreToSearch = CAPICOM_CURRENT_USER_STORE then
		KeyPath = WshShell.ExpandEnvironmentStrings("%USERPROFILE%") & "\Application Data\Microsoft\Crypto\"
	else
		KeyPath = WshShell.ExpandEnvironmentStrings("%ALLUSERSPROFILE%") & "\Application Data\Microsoft\Crypto\"
	end if
	
	'This is a workaround to the determine the subfolder, the better soulution is to create
	'a multi-dimensional array of all the CSPs that you will encounter and use a reference
	'between the CSP name and the provider type.
	if instr(1,UCase(oCertificate.PrivateKey.ProviderName), "DSS") then
		KeyType = "DSS"
	else
		KeyType = "RSA"
	end if
	
	if StoreToSearch = CAPICOM_CURRENT_USER_STORE then
		Set fso = CreateObject("Scripting.FileSystemObject")
		Set f = fso.GetFolder(KeyPath & KeyType)
		Set sf = f.SubFolders
		For Each f1 in sf
			s = f1.name 
		Next
		KeyFolder = "\" & s & "\"
	else
		KeyFolder = "\MachineKeys\"
	end if
	
	FindKeyContainerFromCertificate=KeyPath & KeyType & KeyFolder & oCertificate.PrivateKey.UniqueContainerName

End Function

'******************************************************************************
'
' Function: SetFileACL
'
' Synopsis  : Sets the ACL on specified file
'
' Parameter : UserName: the name of the user to add/change the ACLs of
'			  FileName: the file to change the ACLs on
'			  ACL:		N = None
'						R = Read
'						W = Write (Implied read access also granted)
'						C = Change (Write)
'						F = Full
'						D = Delete user ACLs (uses CACLS /r to remove ACLs)
'******************************************************************************
Sub SetFileACL(UserName, FileName, ACL)
	Dim Command
	Dim Shell
	
	Set Shell = CreateObject("WScript.Shell")
	if UCase(ACL) <> "D" then
		Command = "cacls """ & FileName & """ /e /p """ & UserName & """:" & ACL
	else
		Command = "cacls """ & FileName & """ /e /r """ & UserName & """"
	end if
	Shell.Run Command, 7, true
end Sub

'******************************************************************************
'
' Function: DisplayFileACL
'
' Synopsis  : Sets the ACL on specified file
'
' Parameter : FileName: the file to display the ACLs of
'******************************************************************************
Sub DisplayFileACL(FileName)
	Dim Command
	Dim oShell
	Dim oExec
	dim input
	
	WScript.Echo "ACLs for file: " & FileName & vbCRLF
	Set oShell = CreateObject("WScript.Shell")
	Command = "cacls """ & FileName
	set oExec = oShell.Exec(Command)
	
	While Not oExec.StdOut.AtEndOfStream
		If Not oExec.StdOut.AtEndOfStream Then
			input = input & oExec.StdOut.Read(1)
		End If
		'WScript.Sleep 100
	wend
	input = trim(right(input, len(input) - len(FileName)))
    while InStr(input, vbCRLF) + 2 <= len (input)
		WScript.Echo trim(left(input, InStr(input, vbCRLF)))
		input = trim(right(input, len (input) - (InStr(input, vbCRLF) + 2) ))
	wend
end Sub

'******************************************************************************
'
' Subroutine: DisplayUsage
'
' Synopsis  : Display the usage screen, and then exit with a negative error 
'             code.
'
' Parameter : None.
'
'
'******************************************************************************

Sub DisplayUsage

   Wscript.Stdout.Writeline "Usage: CSetKeyPerm [Options] " 
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "Options:" 
   Wscript.Stdout.Writeline
   Wscript.Stdout.Writeline "   -user     <username>    -- Specifies the user whose ACLs are to change"
   Wscript.Stdout.Writeline "                              (Default: BUILTIN\Administrators)"
   Wscript.Stdout.Writeline "   -Store    <LM|CU>       -- Sets the store that the key is in"
   Wscript.Stdout.Writeline "                                LM = Local Machine (Default)"
   Wscript.Stdout.Writeline "                                CU = Current User"
   Wscript.Stdout.Writeline "   -SHA1     <SHA1HASH>    -- Search for a particular cert based on hash"
   Wscript.Stdout.Writeline "   -ACL      <R|W|F>       -- Spacify the ACL to apply to the key"
   Wscript.Stdout.Writeline "                                R = Read (Default)"
   Wscript.Stdout.Writeline "                                W = Read+Write"
   Wscript.Stdout.Writeline "                                F = Full"
   Wscript.Stdout.Writeline "   -Remove                 -- Removes the ACL for the user spacified"
   Wscript.Stdout.Writeline "   -Display                -- Displays the ACL for the user spacified"
   Wscript.Stdout.Writeline "   -?                      -- This help screen"
   Wscript.Stdout.Writeline              
   Wscript.Quit(1)
   
End Sub ' End DisplayUsage


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
   Const ARG_STATE_OPTIONS	= 0
   Const ARG_STATE_STORE	= 1
   Const ARG_STATE_REMOVE	= 2
   Const ARG_STATE_USERNAME	= 3
   Const ARG_STATE_ACL		= 4
   Const ARG_STATE_SHA1		= 5
   
   ' Our command line parsing state's variable.
   Dim ArgState : ArgState = ARG_STATE_OPTIONS
   
   ' Parse comman line.
   Dim Arg
   For Each Arg In Wscript.Arguments
      Select Case ArgState
		Case ARG_STATE_OPTIONS
			Select Case UCase(Arg)
				Case "-USER", "/USER"
					ArgState = ARG_STATE_USERNAME
		   
				Case "-SHA1", "/SHA!"
					ArgState = ARG_STATE_SHA1
		            
				Case "-STORE", "/STORE"
					ArgState = ARG_STATE_STORE
		         
				Case "-ACL", "/ACL"
					ArgState = ARG_STATE_ACL
		         
				Case "-DISPLAY", "/DISPLAY"
					Display = TRUE
							   
				Case "-REMOVE", "/REMOVE"
					ACL = "D"
		   
				Case "-?", "/?"
					DisplayUsage
		   
				Case "~?"
					DisplayUsage
		         
				Case Else
					If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
					DisplayUsage
					End If
			End Select
	   
		Case ARG_STATE_SHA1
			If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
				DisplayUsage
			Else
				SHA1 = CStr(Arg)
			End If
			ArgState = ARG_STATE_OPTIONS
	      
		Case ARG_STATE_USERNAME
			If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
				DisplayUsage
			Else
				UserName = CStr(Arg)
			End If
			ArgState = ARG_STATE_OPTIONS
		
		Case ARG_STATE_STORE
			If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
				DisplayUsage
			Else
				if UCase(Arg) = "CU" then
					StoreToSearch = CAPICOM_CURRENT_USER_STORE
				else
					StoreToSearch = CAPICOM_LOCAL_MACHINE_STORE
				end if
			End If
			ArgState = ARG_STATE_OPTIONS         
		
		Case ARG_STATE_ACL
			If Left(Arg, 1) = "-" OR Left(Arg, 1) = "/" Then
				DisplayUsage
			Else
				ACL = CStr(Arg)
			End If
			ArgState = ARG_STATE_OPTIONS

		Case Else
			DisplayUsage
	         
      End Select
   Next

End Sub ' ParseCommandLine

'******************************************************************************
'
' Function: IsAdmin
'
' Synopsis  : Check to see if the current user is an Administrator
'
' Parameter : None
'
'******************************************************************************
Function IsAdmin()
 Dim Temp, UserName, Line
 Dim FSO, FO, WshShell, WshNetwork
 Const ForReading = 1 
 
 on Error Resume Next
 
 Set WshShell = WScript.CreateObject("WScript.Shell")
 Temp = WshShell.ExpandEnvironmentStrings("%TEMP%")
 Set WshNetwork = WScript.CreateObject("WScript.Network")
 Username = LCase(WshNetwork.UserName)
 
 WshShell.Run "cmd.exe /c %windir%\system32\net localgroup administrators > %temp%\isadmin.tmp 2>&1", 0, True

 Set FSO = Wscript.CreateObject("Scripting.FileSystemObject")
 Set FO = FSO.OpenTextFile (Temp + "\isadmin.tmp", ForReading)

 Do While FO.AtEndOfStream <> True
   Line = LCase(Trim(FO.ReadLine))
  If InStr(Line, UserName) >= 1 then
   IsAdmin=True
   Exit Do
     End If
   Loop
 
 FO.Close
 
 If IsAdmin <> True Then IsAdmin=False
 
 'Clean Up
 FSO.DeleteFile(Temp+"\isadmin.tmp")
 
 ' Free resources.
 Set FSO = Nothing
 Set FO = Nothing
 Set WshShell = Nothing
 Set WshNetwork = Nothing
End Function

'******************************************************************************
'
' Function: IsWin2KOrGreater
'
' Synopsis  : Check to see if the version of Windows is Windows 2000 or higher
'
' Parameter : None
'
'******************************************************************************
Function IsWin2KOrGreater()
	Dim oShell
	dim input
		
	Set oShell = CreateObject("WScript.Shell")
	input = oShell.RegRead("HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\CurrentVersion")
	if input >= 5.0 then
		IsWin2KOrGreater = True
	else
		IsWin2KOrGreater = False
	end if
end function