'******************************************************************************
'
' THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND, 
' EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED 
' WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
'
' Copyright (C) 2002.  Microsoft Corporation.  All rights reserved.
'
'******************************************************************************
'
'
' CVersion.vbs
'
' This is a sample script that attempts to determine the capicom version installed
' on the local machine.  It does so by attempting to use various methods and
' properties to detect differences from release to release.
'
' This script currently miss-identifies CAPICOM 2.0.0.2 and 2.0.0.3 as Version 2.0
' 
' author: johnla

'**************************** CAPICOM V2.1.0.0 ***************************** 
'--a-- W32i   DLL ENU         2.1.0.0 shp    483,040 09-21-2004 capicom.dll
'        FileVersion     2, 1, 0, 0
'        SpecialBuild    Gold
'**************************** CAPICOM V2.0.0.3 ***************************** 
'--a-- W32i   DLL ENU         2.0.0.3 shp    466,944 04-22-2003 capicom.dll
'        FileVersion     2, 0, 0, 3
'        SpecialBuild    Gold
'
'**************************** CAPICOM V2.0.0.2 ***************************** 
'--a-- W32i   DLL ENU         2.0.0.2 shp    473,744 03-01-2003 capicom.dll
'        FileVersion     2, 0, 0, 2
'        SpecialBuild    Gold
'
'**************************** CAPICOM V2.0.0.1 ***************************** 
'--a-- W32i   DLL ENU         2.0.0.1 shp    484,496 10-01-2002 capicom.dll
'        FileVersion     2, 0, 0, 1
'        SpecialBuild    Gold
'
'**************************** CAPICOM V2 Gold **************************** 
'--a-- W32i   DLL ENU         2.0.0.0 shp    480,904 06-05-2002 capicom.dll
'        FileVersion     2, 0, 0, 0
'        SpecialBuild    Gold
'CAPICOM Version is 2.0
'
'
'**************************** CAPICOM V2 BETA **************************** 
'--a-- W32i   DLL ENU         2.0.0.0 shp    472,176 04-09-2002 capicom.dll
'        FileVersion     2, 0, 0, 0
'        SpecialBuild    Beta
'CAPICOM Version is 2.0b
'
'
'**************************** CAPICOM V1.0A  **************************** 
'--a-- W32i   DLL ENU         1.0.0.2 shp    252,768 07-23-2001 capicom.dll
'        FileVersion     1.0.0.2
'        SpecialBuild
'CAPICOM Version is 1.0a
'
'
'**************************** CAPICOM V1  **************************** 
'--a-- W32i   DLL ENU         1.0.0.1 shp    254,392 01-26-2001 capicom.dll
'        FileVersion     1, 0, 0, 1
'        SpecialBuild
'CAPICOM Version is 1.0
'
' 
'******************************************************************************

If InStr(1, UCase(Wscript.FullName), "CSCRIPT.EXE", vbTextCompare) = 0 Then
   MsgBox "Detecting capicom version: " & GetCAPICOMVersion
Else 
   WScript.Echo "Detecting capicom version: " & GetCAPICOMVersion
End If
WScript.Quit (0)

Function GetCAPICOMVersion()
    On Error Resume Next
    Dim oCS
    Dim oCert
    Dim oStore
    Dim lngTimeout
    Dom StoreName
    Const CAPICOM_21 = "V2.1.0.0"
    Const CAPICOM_203 = "V2.0.0.3"
    Const CAPICOM_202 = "V2.0.0.2"
    Const CAPICOM_201 = "V2.0.0.1"
    Const CAPICOM_20_GOLD = "V2.0"
    Const CAPICOM_20_BETA = "V2.0 beta"
    Const CAPICOM_10A = "V1.0a"
    Const CAPICOM_10 = "V1.0"
    Const CAPICOM_NOT_INSTALLED = "*not installed*"
    Const CAPICOM_LOCAL_MACHINE_STORE                              = 1
    Const CAPICOM_STORE_OPEN_READ_ONLY                             = 0
    Const CAPICOM_CHECK_COMPLETE_CHAIN                             = &H20 

    ' Open the machine Root store, since it is safe to assume it always has some certs in it.
    Set oStore = CreateObject("CAPICOM.Store")
    oStore.Open CAPICOM_LOCAL_MACHINE_STORE, "Root", CAPICOM_STORE_OPEN_READ_ONLY
    If Err.Number <> 0 Then
        GetCAPICOMVersion = CAPICOM_NOT_INSTALLED
        Exit Function
    End If

    StoreName = oStore.Name
    if Err.Number = 0 then
	GetCAPICOMVersion = CAPICOM_21
	exit function
    end if


    Set oCert = oStore.Certificates.Item(1)
    
    Set oCS = oCert.IsValid()

    If ((oCS.CheckFlag And CAPICOM_CHECK_COMPLETE_CHAIN) = CAPICOM_CHECK_COMPLETE_CHAIN) Then
        GetCAPICOMVersion = CAPICOM_201
        Exit Function
    End If

    ' Access CertificateStatus.UrlRetrievalTimeout property as it is new in v2.0.
    lngTimeout = oCert.IsValid.UrlRetrievalTimeout 
    If Err.Number = 0 Then
        If lngTimeout = 0 Then
            GetCAPICOMVersion = CAPICOM_20_GOLD
        Else
           'v2.0 beta initialized this property to 15 seconds.
            GetCAPICOMVersion = CAPICOM_20_BETA
        End If
        Exit Function
    End If
    
    ' Get the certificate SHA1 hash length. V1.0a correctly returns 40
    ' whereas v1.0 wrongly returns 41.
    If Len(oCert.Thumbprint) = 40 Then
        GetCAPICOMVersion = CAPICOM_10A
        Exit Function
    End If
    
    GetCAPICOMVersion = CAPICOM_10
End Function