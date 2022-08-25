VERSION 5.00
Begin VB.Form FrmSignedCode 
   Caption         =   "SignedCode"
   ClientHeight    =   4905
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   7380
   LinkTopic       =   "Form1"
   ScaleHeight     =   4905
   ScaleWidth      =   7380
   StartUpPosition =   3  'Windows Default
   Begin VB.TextBox txtDescriptionURL 
      Height          =   285
      Left            =   2040
      TabIndex        =   13
      Top             =   3720
      Width           =   5055
   End
   Begin VB.TextBox txtDescription 
      Height          =   285
      Left            =   2040
      TabIndex        =   12
      Top             =   3240
      Width           =   5055
   End
   Begin VB.CommandButton cmdSignInfo 
      Caption         =   "View Signing &Info"
      Height          =   375
      Left            =   5520
      TabIndex        =   10
      Top             =   4320
      Width           =   1575
   End
   Begin VB.CommandButton cmdExit 
      Caption         =   "&Exit"
      Height          =   375
      Left            =   5520
      TabIndex        =   7
      Top             =   240
      Width           =   1575
   End
   Begin VB.DriveListBox drive 
      Height          =   315
      Left            =   1320
      TabIndex        =   6
      Top             =   600
      Width           =   2775
   End
   Begin VB.DirListBox dir 
      Height          =   1440
      Left            =   1320
      TabIndex        =   5
      Top             =   960
      Width           =   2775
   End
   Begin VB.FileListBox file 
      Height          =   1455
      Left            =   4080
      Pattern         =   "*.exe;*.dll;*.ocx;*.vbs;*.cab"
      TabIndex        =   4
      Top             =   960
      Width           =   3015
   End
   Begin VB.CommandButton cmdVerify 
      Caption         =   "&Verify"
      Height          =   375
      Left            =   3720
      TabIndex        =   3
      Top             =   4320
      Width           =   1575
   End
   Begin VB.CommandButton cmdTimeStamp 
      Caption         =   "&Timestamp"
      Height          =   375
      Left            =   1920
      TabIndex        =   2
      Top             =   4320
      Width           =   1575
   End
   Begin VB.CommandButton cmdSign 
      Caption         =   "&Sign"
      Height          =   375
      Left            =   120
      TabIndex        =   1
      Top             =   4320
      Width           =   1575
   End
   Begin VB.Label Label7 
      Caption         =   "Description URL:"
      Height          =   255
      Left            =   240
      TabIndex        =   15
      Top             =   3720
      Width           =   1695
   End
   Begin VB.Label Label6 
      Caption         =   "Description:"
      Height          =   255
      Left            =   240
      TabIndex        =   14
      Top             =   3240
      Width           =   1695
   End
   Begin VB.Label Label2 
      Caption         =   "Please enter description and/or desription URL for the file you'd like to sign:"
      Height          =   375
      Left            =   240
      TabIndex        =   11
      Top             =   2640
      Width           =   5895
   End
   Begin VB.Label Label4 
      Caption         =   "Folder:"
      Height          =   255
      Left            =   240
      TabIndex        =   9
      Top             =   1080
      Width           =   1095
   End
   Begin VB.Label Label3 
      Caption         =   "Local Drive:"
      Height          =   255
      Left            =   240
      TabIndex        =   8
      Top             =   600
      Width           =   975
   End
   Begin VB.Label Label1 
      Caption         =   "Please select drive/folder/file:"
      Height          =   255
      Left            =   240
      TabIndex        =   0
      Top             =   240
      Width           =   3855
   End
End
Attribute VB_Name = "FrmSignedCode"
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
' SignedCode.vbp
'
' This is a VB sample that illustrates how to use features introduced in
' CAPICOM v2.0 to sign, timestamp, and verify files using a certificate
' from Personal Store.
'
' Note: For simplicity, this sample does not handle exception.
'
'******************************************************************************

'timestamping URL
Const URL = "http://timestamp.verisign.com/scripts/timstamp.dll"

Dim st As New Store
Dim certs As Certificates
Dim selectedCerts As Certificates
Dim cert As Certificate
Dim Signer As New Signer
Public SignedCode As New SignedCode

Private Sub cmdSignInfo_Click()
    FrmSignInfo.Show
End Sub

Private Sub Form_Load()
    'open current user store and obtain certificates
    st.Open CAPICOM_CURRENT_USER_STORE, CAPICOM_MY_STORE, CAPICOM_STORE_OPEN_READ_ONLY
    Set certs = st.Certificates
End Sub

Private Sub cmdSign_Click()
On Error GoTo err_handler
    
    'sign
    SignedCode.Sign Signer
    MsgBox ("The fle has been signed succesfully")

Exit Sub
err_handler:
        'if the user cancels, don't display error msg
        If Err.Number <> CAPICOM_E_CANCELLED Then
            MsgBox "Error " & Hex(Err.Number) & ": " & Err.Description
        End If
            Err.Clear
            Exit Sub
End Sub
Private Sub cmdTimeStamp_Click()
On Error GoTo err_handler
    'timestamp
    SignedCode.TimeStamp URL
    MsgBox ("The fle has been timestamped")
Exit Sub

err_handler:
    MsgBox Err.Description
    Exit Sub
End Sub

Private Sub cmdVerify_Click()
On Error GoTo err_handler
    'verify
    SignedCode.Verify
    MsgBox ("The file has been verified")
Exit Sub
    
err_handler:
    MsgBox Hex(Err.Number) & ": " & Err.Description
    Exit Sub
End Sub

Private Sub dir_Change()
On Error GoTo err_handler

    file.Path = dir.Path

Exit Sub
err_handler:
    MsgBox Hex(Err.Number) & ": " & Err.Description
Exit Sub
End Sub

Private Sub drive_Change()
On Error GoTo err_handler
    dir.Path = drive.drive & "\"
Exit Sub
err_handler:
    MsgBox Hex(Err.Number) & ": " & Err.Description
Exit Sub
End Sub

Private Sub file_Click()
    'set filename to sign
    SignedCode.FileName = file.Path & "\" & file.FileName
End Sub
Private Sub txtDescription_Change()
    'set description fields
    SignedCode.Description = txtDescription.Text
End Sub

Private Sub txtDescriptionURL_Change()
    'set description URL fields
    SignedCode.DescriptionURL = txtDescriptionURL.Text
End Sub

Private Sub cmdExit_Click()
    'Clean up
    Set st = Nothing
    Set certs = Nothing
    Set selectedCerts = Nothing
    Set cert = Nothing
    Set Signer = Nothing
    Set SignedCode = Nothing
    End
End Sub

