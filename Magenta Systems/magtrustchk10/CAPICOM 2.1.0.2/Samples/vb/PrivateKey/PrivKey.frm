VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form frmPrivKey 
   AutoRedraw      =   -1  'True
   Caption         =   "Private Key Info"
   ClientHeight    =   6450
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   7545
   LinkTopic       =   "Form1"
   ScaleHeight     =   6450
   ScaleWidth      =   7545
   StartUpPosition =   3  'Windows Default
   Begin VB.TextBox txtUniqueContainer 
      Height          =   285
      Left            =   1680
      Locked          =   -1  'True
      ScrollBars      =   1  'Horizontal
      TabIndex        =   21
      Top             =   5520
      Width           =   5655
   End
   Begin VB.CommandButton cmdExit 
      Caption         =   "&Exit"
      Height          =   375
      Left            =   6120
      TabIndex        =   20
      Top             =   6000
      Width           =   1215
   End
   Begin MSComctlLib.ListView ListView1 
      Height          =   2175
      Left            =   120
      TabIndex        =   19
      Top             =   480
      Width           =   7215
      _ExtentX        =   12726
      _ExtentY        =   3836
      View            =   3
      LabelEdit       =   1
      LabelWrap       =   -1  'True
      HideSelection   =   -1  'True
      OLEDragMode     =   1
      FullRowSelect   =   -1  'True
      _Version        =   393217
      ForeColor       =   -2147483640
      BackColor       =   -2147483643
      BorderStyle     =   1
      Appearance      =   1
      OLEDragMode     =   1
      NumItems        =   0
   End
   Begin VB.TextBox txtProviderType 
      Height          =   285
      Left            =   5160
      TabIndex        =   17
      Top             =   4320
      Width           =   2175
   End
   Begin VB.TextBox txtProviderName 
      Height          =   285
      Left            =   5160
      TabIndex        =   16
      Top             =   3840
      Width           =   2175
   End
   Begin VB.TextBox txtIsRemovable 
      Height          =   285
      Left            =   5160
      TabIndex        =   15
      Top             =   3360
      Width           =   2175
   End
   Begin VB.TextBox txtIsProtected 
      Height          =   285
      Left            =   5160
      MultiLine       =   -1  'True
      TabIndex        =   14
      Top             =   2880
      Width           =   2175
   End
   Begin VB.TextBox txtContainerName 
      Height          =   285
      Left            =   1680
      Locked          =   -1  'True
      ScrollBars      =   1  'Horizontal
      TabIndex        =   13
      Top             =   5040
      Width           =   5655
   End
   Begin VB.TextBox txtIsMachineKeySet 
      Height          =   285
      Left            =   1680
      TabIndex        =   4
      Top             =   4320
      Width           =   1815
   End
   Begin VB.TextBox txtIsHW 
      Height          =   285
      Left            =   1680
      TabIndex        =   3
      Top             =   3840
      Width           =   1815
   End
   Begin VB.TextBox txtIsExportable 
      Height          =   285
      Left            =   1680
      TabIndex        =   2
      Top             =   3360
      Width           =   1815
   End
   Begin VB.TextBox txtIsAccessible 
      Height          =   285
      Left            =   1680
      TabIndex        =   1
      Top             =   2880
      Width           =   1815
   End
   Begin VB.Label Label11 
      Caption         =   "Unique Container:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   120
      TabIndex        =   22
      Top             =   5520
      Width           =   1575
   End
   Begin VB.Label Label5 
      Caption         =   "Machine Key Set:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   120
      TabIndex        =   18
      Top             =   4320
      Width           =   1575
   End
   Begin VB.Label Label10 
      Caption         =   "Container:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   120
      TabIndex        =   12
      Top             =   5040
      Width           =   1335
   End
   Begin VB.Label Label9 
      Caption         =   "Provider Type:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   3720
      TabIndex        =   11
      Top             =   4320
      Width           =   1335
   End
   Begin VB.Label Label8 
      Caption         =   "Provider Name:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   3720
      TabIndex        =   10
      Top             =   3840
      Width           =   1455
   End
   Begin VB.Label Label7 
      Caption         =   "Removable:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   3720
      TabIndex        =   9
      Top             =   3360
      Width           =   1335
   End
   Begin VB.Label Label6 
      Caption         =   "Protected:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   3720
      TabIndex        =   8
      Top             =   2880
      Width           =   1335
   End
   Begin VB.Label Label4 
      Caption         =   "Exportable:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   120
      TabIndex        =   7
      Top             =   3360
      Width           =   1335
   End
   Begin VB.Label Label3 
      Caption         =   "H/W Device:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   120
      TabIndex        =   6
      Top             =   3840
      Width           =   1335
   End
   Begin VB.Label Label2 
      Caption         =   "Accessible:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   120
      TabIndex        =   5
      Top             =   2880
      Width           =   1335
   End
   Begin VB.Label Label1 
      Caption         =   "Choose Certificate from Personal Store:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   5055
   End
End
Attribute VB_Name = "frmPrivKey"
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
' Copyright (C) 1999- 2002.  Microsoft Corporation.  All rights reserved.
'
'******************************************************************************
'
' PrivKey.vbp
'
' This is a VB sample that illustrates how to use features introduced in
' CAPICOM v2.0 to display properties of a private key tied to a particular certificate.
'
' Note: For simplicity, this sample does not handle exception.
'
'******************************************************************************
Option Explicit

Dim myStore As New Store
Dim cert As Certificate
Dim li As ListItem

Const NTE_NO_KEY = &H8009000D

Public Sub Form_Load()
    
    Dim colSubject, colIssuer, colValidFrom, colValidTo As ColumnHeader
    Dim i As Integer
    
     'open my certificate store
    myStore.Open CAPICOM_CURRENT_USER_STORE, CAPICOM_MY_STORE, CAPICOM_STORE_OPEN_READ_ONLY
   
    Set colIssuer = ListView1.ColumnHeaders.Add()
    Set colSubject = ListView1.ColumnHeaders.Add()
    Set colValidFrom = ListView1.ColumnHeaders.Add()
    Set colValidTo = ListView1.ColumnHeaders.Add()
    
    'set column names
    colIssuer.Text = "Issuer"
    colSubject.Text = "Subject"
    colValidFrom.Text = "Valid From"
    colValidTo.Text = "Valid To"
      
    'set column widths
    colIssuer.Width = ListView1.Width / 4
    colSubject.Width = ListView1.Width / 4
    colValidFrom.Width = ListView1.Width / 4
    colValidTo.Width = ListView1.Width / 4

    ListView1.ListItems.Clear
    
    'list all available certificates in the store
    For i = 1 To myStore.Certificates.Count
        
        Set cert = myStore.Certificates(i)
        Set li = ListView1.ListItems.Add(, , cert.GetInfo(CAPICOM_CERT_INFO_ISSUER_SIMPLE_NAME))
        Set li.Tag = cert
        
        ListView1.ListItems(i).ListSubItems.Add , , cert.GetInfo(CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME)
        ListView1.ListItems(i).ListSubItems.Add , , cert.ValidFromDate
        ListView1.ListItems(i).ListSubItems.Add , , cert.ValidToDate
      
    Next i
End Sub

Private Sub ListView1_Click()
On Error GoTo err_handler
    Dim privKey As PrivateKey
       
    Call Clear
    
    Set cert = myStore.Certificates(ListView1.SelectedItem.Index)

    If cert.HasPrivateKey = True Then
    
        Set privKey = cert.PrivateKey
        
        'display key properties
        txtContainerName = privKey.ContainerName
        txtUniqueContainer = privKey.UniqueContainerName
        txtIsAccessible = privKey.IsAccessible
        txtIsExportable = privKey.IsExportable
        txtIsHW = privKey.IsHardwareDevice
        txtIsMachineKeySet = privKey.IsMachineKeyset
        txtIsProtected = privKey.IsProtected
        txtIsRemovable = privKey.IsRemovable
        txtProviderName = privKey.ProviderName
        
        'provide a friendly provider name
        Select Case privKey.ProviderType
            Case CAPICOM_PROV_RSA_FULL
                txtProviderType = "RSA_FULL"
            Case CAPICOM_PROV_RSA_SIG
                txtProviderType = "RSA_SIG"
            Case CAPICOM_PROV_DSS
                txtProviderType = "DSS"
            Case CAPICOM_PROV_FORTEZZA
                txtProviderType = "FORTEZZA"
            Case Else
                txtProviderType = "UNKNOWN"
        End Select
    Else
        MsgBox "This certificate has no private key."
    End If
Exit Sub

err_handler:
           MsgBox "Error " & Hex(Err.Number) & ": " & Err.Description
           Err.Clear
           Exit Sub
End Sub
Private Sub Clear()
        
        'clear key properties
        txtContainerName = ""
        txtUniqueContainer = ""
        txtIsAccessible = ""
        txtIsExportable = ""
        txtIsHW = ""
        txtIsMachineKeySet = ""
        txtIsProtected = ""
        txtIsRemovable = ""
        txtProviderName = ""
        txtProviderType = ""
End Sub

Private Sub ListView1_DblClick()
 'display selected certificate
 If Not ListView1.SelectedItem Is Nothing Then
        ListView1.SelectedItem.Tag.Display
 End If
End Sub

Private Sub cmdExit_Click()
    Set myStore = Nothing
    Set cert = Nothing
    Set li = Nothing
    Unload Me
End Sub
