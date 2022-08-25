VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "CAPI / CAPICOM Bridging Sample"
   ClientHeight    =   4530
   ClientLeft      =   60
   ClientTop       =   450
   ClientWidth     =   5895
   LinkTopic       =   "Form1"
   ScaleHeight     =   4530
   ScaleWidth      =   5895
   StartUpPosition =   3  'Windows Default
   Begin VB.ComboBox txtStoreName 
      Height          =   315
      ItemData        =   "bridge_display.frx":0000
      Left            =   1200
      List            =   "bridge_display.frx":0010
      TabIndex        =   15
      Text            =   "My"
      Top             =   240
      Width           =   1335
   End
   Begin VB.Frame Frame1 
      Caption         =   "CRL Information"
      Height          =   3255
      Left            =   120
      TabIndex        =   5
      Top             =   1200
      Width           =   5655
      Begin VB.ListBox lstCRL 
         Height          =   1230
         ItemData        =   "bridge_display.frx":0038
         Left            =   120
         List            =   "bridge_display.frx":003A
         TabIndex        =   13
         Top             =   1920
         Width           =   5415
      End
      Begin VB.OptionButton optCache 
         Caption         =   "Cache &Only"
         Height          =   375
         Left            =   2040
         TabIndex        =   11
         Top             =   1440
         Width           =   2055
      End
      Begin VB.OptionButton optWire 
         Caption         =   "&Wire Only"
         Height          =   375
         Left            =   2040
         TabIndex        =   10
         Top             =   1080
         Width           =   2055
      End
      Begin VB.OptionButton optDefault 
         Caption         =   "&Default"
         Height          =   375
         Left            =   2040
         TabIndex        =   9
         Top             =   720
         Value           =   -1  'True
         Width           =   2055
      End
      Begin VB.TextBox txtTimeout 
         Height          =   285
         Left            =   2040
         TabIndex        =   8
         Text            =   "10000"
         Top             =   360
         Width           =   855
      End
      Begin VB.CommandButton cmdViewCRL 
         Caption         =   "View &CRL"
         Enabled         =   0   'False
         Height          =   375
         Left            =   4080
         TabIndex        =   6
         Top             =   360
         Width           =   1335
      End
      Begin VB.Label Label5 
         Caption         =   "CDPs:"
         Height          =   255
         Left            =   120
         TabIndex        =   14
         Top             =   1680
         Width           =   1695
      End
      Begin VB.Label Label4 
         Caption         =   "Wire behavior:"
         Height          =   375
         Left            =   240
         TabIndex        =   12
         Top             =   840
         Width           =   1095
      End
      Begin VB.Label Label3 
         Caption         =   "&Timeout (in milliseconds):"
         Height          =   255
         Left            =   240
         TabIndex        =   7
         Top             =   360
         Width           =   1815
      End
   End
   Begin VB.CommandButton cmdView 
      Caption         =   "&View Certificate"
      Enabled         =   0   'False
      Height          =   375
      Left            =   4440
      TabIndex        =   4
      Top             =   240
      Width           =   1335
   End
   Begin VB.CommandButton cmdSelect 
      Caption         =   "&Select"
      Height          =   375
      Left            =   2640
      TabIndex        =   0
      Top             =   240
      Width           =   1215
   End
   Begin VB.Label lblSubjectName 
      Caption         =   "<no certificate selected>"
      Height          =   255
      Left            =   1320
      TabIndex        =   3
      Top             =   720
      Width           =   4455
   End
   Begin VB.Label Label2 
      Caption         =   "Subject Name:"
      Height          =   255
      Left            =   120
      TabIndex        =   2
      Top             =   720
      Width           =   1215
   End
   Begin VB.Label Label1 
      Caption         =   "Store Name:"
      Height          =   375
      Left            =   120
      TabIndex        =   1
      Top             =   240
      Width           =   975
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'******************************************************************************
'
' THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
' EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED
' WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
'
' Copyright (C) 1999 - 2002.  Microsoft Corporation.  All rights reserved.
'
'******************************************************************************
'
' bridge_display.frm
'
' This is a sample script to illustrate how to use CAPICOM's bridging support
' to interop between CAPI and CAPICOM.
'
' Note: For simplicity, this script does not handle exception.
'
'
'
'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
'
'  NOTE: This sample requires Windows XP to run
'  It calls the following APIs that were added in WinXP
'    CryptUIDlgViewContext
'    CryptUIDlgSelectCertificateFromStore
'
'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
'
'
'******************************************************************************


'This function only exists on XP and above
Private Declare Function CryptUIDlgViewContext Lib "cryptui" _
    (ByVal dwContextType As Long, _
     ByVal pvContext As Long, _
     ByVal hwnd As Long, _
     ByVal pwszTitle As String, _
     ByVal dwFlags As Long, _
     ByVal pvReserved As Long _
     ) As Long


Const CERT_STORE_CERTIFICATE_CONTEXT = 1
Const CERT_STORE_CRL_CONTEXT = 2


'This function only exists on XP and above
Private Declare Function CryptUIDlgSelectCertificateFromStore Lib "cryptui" ( _
    ByVal hCertStore As Long, _
    ByVal hwnd As Long, _
    ByVal pwszTitle As String, _
    ByVal pwszDisplayString As String, _
    ByVal dwDontUseColumn As Long, _
    ByVal dwFlags As Long, _
    ByVal pvReserved As Long _
    ) As Long

' flags for dwDontUseColumn
Const CRYPTUI_SELECT_ISSUEDTO_COLUMN = &H1
Const CRYPTUI_SELECT_ISSUEDBY_COLUMN = &H2
Const CRYPTUI_SELECT_INTENDEDUSE_COLUMN = &H4
Const CRYPTUI_SELECT_FRIENDLYNAME_COLUMN = &H8
Const CRYPTUI_SELECT_LOCATION_COLUMN = &H10
Const CRYPTUI_SELECT_EXPIRATION_COLUMN = &H20

Const MAX_URLSIZE = 10000

Private Type CRYPT_URL_ARRAY
    cUrl As Long
    rgwszUrl(MAX_URLSIZE) As Byte  'should be long enough for most URLs
End Type

Private Declare Function CryptGetObjectUrl Lib "cryptnet" ( _
    ByVal pszUrlOid As Long, _
    ByVal pvPara As Long, _
    ByVal dwFlags As Long, _
    CRYPT_URL_ARRAY As Any, _
    pcbUrlArray As Long, _
    ByVal pUrlInfo As Long, _
    ByRef pcbUrlInfo As Long, _
    ByVal pvReserved As Long _
    ) As Long

Const URL_OID_CERTIFICATE_CRL_DIST_POINT = 2
Const CRYPT_GET_URL_FROM_EXTENSION = 2

Private Declare Function CryptRetrieveObjectByUrlA Lib "cryptnet" ( _
  ByVal pszUrl As String, _
  ByVal pszObjectOid As Long, _
  ByVal dwRetrievalFlags As Long, _
  ByVal dwTimeout As Long, _
  ByRef ppvObject As Long, _
  ByVal hAsyncRetrieve As Long, _
  ByVal pCredentials As Long, _
  ByVal pvVerify As Long, _
  ByVal pAuxInfo As Long _
) As Long

Const CRYPT_CACHE_ONLY_RETRIEVAL = 2
Const CRYPT_WIRE_ONLY_RETRIEVAL = 4


Const CONTEXT_OID_CRL = 2


Private Declare Function CertFreeCRLContext Lib "Crypt32" ( _
    ByVal pCrlContext As Long _
) As Long

Dim oCert As Certificate

Private Sub cmdSelect_Click()
    Dim iCert As ICertContext
    Dim hStore As Long
    Dim iStore As ICertStore
    Dim oStore As New Store
    Dim lDontUse As Long
    Dim strTitle As String
    Dim strDisplayString As String
    Dim oExt As Extension
    
    lDontUse = CRYPTUI_SELECT_LOCATION_COLUMN
    
    'we must encode this string as unicode, even though it already is, because VB will
    'automatically convert it to ANSI on us when it calls CAPI
    strTitle = StrConv("Using CAPI to Select a certificate", vbUnicode)
    strDisplayString = StrConv("Select a certificate using CAPI", vbUnicode)
    
    cmdViewCRL.Enabled = False
    cmdView.Enabled = False
    lstCRL.Clear
    
    'Open the store, then get the CAPI store handle
    oStore.Open CAPICOM_CURRENT_USER_STORE, txtStoreName, CAPICOM_STORE_OPEN_EXISTING_ONLY Or CAPICOM_STORE_OPEN_READ_ONLY
    Set iStore = oStore
    hStore = iStore.StoreHandle
       
    'This function only exists on XP and above
    'Pass the store handle into this function
    pCertContext = CryptUIDlgSelectCertificateFromStore( _
                        hStore, _
                        Form1.hwnd, _
                        strTitle, _
                        strDisplayString, _
                        lDontUse, _
                        0, _
                        0)
    
    If pCertContext = 0 Then  'the user hit cancel
        Exit Sub
    End If
    Set oCert = New Certificate
    Set iCert = oCert
    iCert.CertContext = pCertContext
    
    lblSubjectName = oCert.GetInfo(CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME)
    cmdView.Enabled = True
    
    If (Not IsEmpty(oCert.Extensions.Item(CAPICOM_CRL_DIST_POINTS_OID))) Then
        Set oExt = oCert.Extensions.Item(CAPICOM_CRL_DIST_POINTS_OID)
    
        cmdViewCRL.Enabled = True
        GetCRLs
        If lstCRL.ListCount > 0 Then
            lstCRL.Selected(0) = True
        End If
    End If
    
    'Don't forget to free the context handles
    iCert.FreeContext pCertContext  'Free the one returned from CAPI
    iStore.CloseHandle hStore       'Free the store handle
    
End Sub

Private Sub cmdView_Click()
    Dim retval
    Dim iCert As ICertContext
    Dim pCertContext As Long
    Dim strTitleView As String
    
    Set iCert = oCert
    
    'now get the underlying CERT_CONTEXT pointer
    pCertContext = iCert.CertContext
    
    'we must encode this string as unicode, even though it already is, because VB will
    'automatically convert it to ANSI on us when it calls CAPI
    strTitleView = StrConv("Using CAPI to display a certificate", vbUnicode)
    
    
    'We could just use oCert.Display() to show the cert, but
    'this sample is showing off the bridging API support CAPICOM 2 has.
    'Get the CERT_CONTEXT pointer and pass it to CAPI.
    
    'This function only exists on XP and above
    retval = CryptUIDlgViewContext( _
                CERT_STORE_CERTIFICATE_CONTEXT, _
                pCertContext, _
                Form1.hwnd, _
                strTitleView, _
                0, _
                0)
    
    'Don't forget to free the context handles
    iCert.FreeContext pCertContext

End Sub


Private Sub GetCRLs()
    Dim retval
    Dim iCert As ICertContext
    Dim pCertContext As Long
    Dim cbUrlArray As Long
    Dim strUrlArray As String
    Dim idx As Integer
    Dim bUrlArray() As Byte
    Dim cUrl As Integer
    Dim i As Integer
    Dim line As String
    Dim start
    Dim done As Boolean
    Dim sURLCount As String
    Dim CryptURLs As CRYPT_URL_ARRAY
    
    cbUrlArray = -1
    
    Set iCert = oCert
    pCertContext = iCert.CertContext
    
    'we must encode this string as unicode, even though it already is, because VB will
    'automatically convert it to ANSI on us when it calls CAPI
    strTitleView = StrConv("Using CAPI to display a certificate", vbUnicode)
    
    'retrieve the CRL array
    retval = CryptGetObjectUrl( _
                URL_OID_CERTIFICATE_CRL_DIST_POINT, _
                pCertContext, _
                CRYPT_GET_URL_FROM_EXTENSION, _
                CryptURLs, _
                MAX_URLSIZE, _
                0, _
                0, _
                0)
    If (retval = False) Then
        MsgBox "Could not retrieve the URLs from the CDP extension: 0x" & Hex(Err.LastDllError)
        Exit Sub
    End If
    
    cUrl = CryptURLs.cUrl
    done = False
    idx = 0
    
    'skip the cURL * LPWSTRs in the structure
    start = cUrl * 4 + 4
    
    'Parse the PCRYPT_URL_ARRAY structure received.  Extract the URLs.
    For i = 0 To cUrl - 1
        While Not done
            If CryptURLs.rgwszUrl(start + idx) = 0 Then
                done = True
            Else
                strUrl = strUrl & Chr$(CryptURLs.rgwszUrl(start + idx))
            End If
            idx = idx + 2
        Wend
        lstCRL.AddItem (strUrl)
        strUrl = ""
        done = False
    Next i
    
    iCert.FreeContext pCertContext

End Sub

Private Sub cmdViewCRL_Click()
    Dim pCrlContext As Long
    Dim lngFlags As Long
    Dim strTitleView As String
    Form1.MousePointer = vbHourglass

    If lstCRL.ListIndex < 0 Then
        Form1.MousePointer = vbDefault
        Exit Sub
    End If
    If optDefault Then
        lngFlags = 0
    ElseIf optWire Then
        lngFlags = CRYPT_WIRE_ONLY_RETRIEVAL
    ElseIf optCache Then
        lngFlags = CRYPT_CACHE_ONLY_RETRIEVAL
    End If
    
    'Download the CRL from the URL
    retval = CryptRetrieveObjectByUrlA( _
                lstCRL.List(lstCRL.ListIndex), _
                CONTEXT_OID_CRL, _
                lngFlags, _
                CLng(txtTimeout), _
                pCrlContext, _
                0, _
                0, _
                0, _
                0)

    If Err.LastDllError <> 0 Then
        MsgBox "The CRL could not be retrieved. Error code 0x" & Hex(Err.LastDllError)
    Else
        'we must encode this string as unicode, even though it already is, because VB will
        'automatically convert it to ANSI on us when it calls CAPI
        strTitleView = StrConv("Using CAPI to display a CRL", vbUnicode)
        
        'Display the CRL
        'This function only exists on XP and above
        retval = CryptUIDlgViewContext( _
                    CERT_STORE_CRL_CONTEXT, _
                    pCrlContext, _
                    Form1.hwnd, _
                    strTitleView, _
                    0, _
                    0)
        
        'Don't forget to free the CRL context
        CertFreeCRLContext pCrlContext
    End If
    
    Form1.MousePointer = vbDefault
End Sub

Private Sub lstCRL_DblClick()
    Call cmdViewCRL_Click
End Sub
