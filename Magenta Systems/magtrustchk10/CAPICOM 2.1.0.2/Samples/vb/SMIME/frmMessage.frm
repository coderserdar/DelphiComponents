VERSION 5.00
Object = "{3B7C8863-D78F-101B-B9B5-04021C009402}#1.2#0"; "RICHTX32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "COMCTL32.OCX"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form frmMessage 
   Caption         =   "Message"
   ClientHeight    =   5655
   ClientLeft      =   165
   ClientTop       =   855
   ClientWidth     =   7560
   Icon            =   "frmMessage.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   5655
   ScaleWidth      =   7560
   StartUpPosition =   3  'Windows Default
   Begin ComctlLib.Toolbar tbarMessage 
      Align           =   1  'Align Top
      Height          =   840
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   7560
      _ExtentX        =   13335
      _ExtentY        =   1482
      ButtonWidth     =   1164
      ButtonHeight    =   1376
      Wrappable       =   0   'False
      ImageList       =   "ilstMessageMed"
      _Version        =   327682
      BeginProperty Buttons {0713E452-850A-101B-AFC0-4210102A8DA7} 
         NumButtons      =   3
         BeginProperty Button1 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Sign"
            Description     =   "Sign"
            Object.ToolTipText     =   "Sign"
            Object.Tag             =   ""
            ImageIndex      =   1
            Style           =   1
            Value           =   1
         EndProperty
         BeginProperty Button2 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Encrypt"
            Description     =   "Encrypt"
            Object.ToolTipText     =   "Encrypt"
            Object.Tag             =   ""
            ImageIndex      =   2
            Style           =   1
         EndProperty
         BeginProperty Button3 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Check"
            Object.ToolTipText     =   "Check Names"
            Object.Tag             =   ""
            ImageIndex      =   3
         EndProperty
      EndProperty
      Begin MSComDlg.CommonDialog cmnFileSelection 
         Left            =   5040
         Top             =   120
         _ExtentX        =   847
         _ExtentY        =   847
         _Version        =   393216
      End
   End
   Begin VB.Frame gboxViewMail 
      BorderStyle     =   0  'None
      Height          =   1935
      Left            =   120
      TabIndex        =   7
      Top             =   960
      Visible         =   0   'False
      Width           =   7335
      Begin VB.TextBox txtSubjectValue 
         Appearance      =   0  'Flat
         BackColor       =   &H8000000F&
         BorderStyle     =   0  'None
         Height          =   285
         Left            =   1200
         Locked          =   -1  'True
         TabIndex        =   25
         Top             =   1560
         Width           =   6015
      End
      Begin VB.TextBox txtCCValue 
         Appearance      =   0  'Flat
         BackColor       =   &H8000000F&
         BorderStyle     =   0  'None
         Height          =   285
         Left            =   1200
         Locked          =   -1  'True
         TabIndex        =   24
         Top             =   1200
         Width           =   6015
      End
      Begin VB.TextBox txtDateValue 
         Appearance      =   0  'Flat
         BackColor       =   &H8000000F&
         BorderStyle     =   0  'None
         Height          =   285
         Left            =   1200
         Locked          =   -1  'True
         TabIndex        =   23
         Top             =   840
         Width           =   6015
      End
      Begin VB.TextBox txtFromValue 
         Appearance      =   0  'Flat
         BackColor       =   &H8000000F&
         BorderStyle     =   0  'None
         Height          =   285
         Left            =   1200
         Locked          =   -1  'True
         TabIndex        =   22
         Top             =   120
         Width           =   4935
      End
      Begin VB.TextBox txtToValue 
         Appearance      =   0  'Flat
         BackColor       =   &H8000000F&
         BorderStyle     =   0  'None
         Height          =   285
         Left            =   1200
         Locked          =   -1  'True
         TabIndex        =   21
         Top             =   480
         Width           =   4935
      End
      Begin VB.CommandButton btnBadEnc 
         Appearance      =   0  'Flat
         Height          =   375
         Left            =   6480
         MaskColor       =   &H00FF00FF&
         Picture         =   "frmMessage.frx":030A
         Style           =   1  'Graphical
         TabIndex        =   19
         Top             =   0
         UseMaskColor    =   -1  'True
         Visible         =   0   'False
         Width           =   375
      End
      Begin VB.CommandButton btnGoodEnc 
         Appearance      =   0  'Flat
         Height          =   375
         Left            =   6480
         MaskColor       =   &H00FF00FF&
         Picture         =   "frmMessage.frx":064C
         Style           =   1  'Graphical
         TabIndex        =   18
         Top             =   0
         UseMaskColor    =   -1  'True
         Visible         =   0   'False
         Width           =   375
      End
      Begin VB.CommandButton btnBadSig 
         Appearance      =   0  'Flat
         Height          =   375
         Left            =   6960
         MaskColor       =   &H00FF00FF&
         Picture         =   "frmMessage.frx":098E
         Style           =   1  'Graphical
         TabIndex        =   17
         Top             =   0
         UseMaskColor    =   -1  'True
         Visible         =   0   'False
         Width           =   375
      End
      Begin VB.CommandButton btnGoodSig 
         Appearance      =   0  'Flat
         Height          =   375
         Left            =   6960
         MaskColor       =   &H00FF00FF&
         Picture         =   "frmMessage.frx":0CD0
         Style           =   1  'Graphical
         TabIndex        =   16
         Top             =   0
         UseMaskColor    =   -1  'True
         Visible         =   0   'False
         Width           =   375
      End
      Begin VB.Label lblFrom 
         BackStyle       =   0  'Transparent
         Caption         =   "From:"
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
         Index           =   2
         Left            =   120
         TabIndex        =   20
         Top             =   120
         Width           =   855
      End
      Begin VB.Label lblDate 
         BackStyle       =   0  'Transparent
         Caption         =   "Date:"
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
         TabIndex        =   15
         Top             =   840
         Width           =   855
      End
      Begin VB.Label lblSubject 
         BackStyle       =   0  'Transparent
         Caption         =   "Subject:"
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
         Index           =   1
         Left            =   120
         TabIndex        =   14
         Top             =   1560
         Width           =   855
      End
      Begin VB.Label lblCC 
         BackStyle       =   0  'Transparent
         Caption         =   "CC:"
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
         Index           =   1
         Left            =   120
         TabIndex        =   13
         Top             =   1200
         Width           =   855
      End
      Begin VB.Label lblTo 
         BackStyle       =   0  'Transparent
         Caption         =   "To:"
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
         Index           =   1
         Left            =   120
         TabIndex        =   12
         Top             =   480
         Width           =   855
      End
   End
   Begin VB.Frame gboxNewMail 
      BorderStyle     =   0  'None
      Height          =   1215
      Left            =   120
      TabIndex        =   1
      Top             =   960
      Width           =   7335
      Begin VB.TextBox txtTo 
         Height          =   285
         Left            =   1200
         TabIndex        =   2
         Top             =   120
         Width           =   6015
      End
      Begin VB.TextBox txtCC 
         Height          =   285
         Left            =   1200
         TabIndex        =   3
         Top             =   480
         Width           =   6015
      End
      Begin VB.TextBox txtSubject 
         Height          =   285
         Left            =   1200
         TabIndex        =   4
         Top             =   840
         Width           =   6015
      End
      Begin VB.Label lblTo 
         BackStyle       =   0  'Transparent
         Caption         =   "To:"
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
         Index           =   0
         Left            =   120
         TabIndex        =   11
         Top             =   120
         Width           =   855
      End
      Begin VB.Label lblCC 
         BackStyle       =   0  'Transparent
         Caption         =   "CC:"
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
         Index           =   0
         Left            =   120
         TabIndex        =   10
         Top             =   480
         Width           =   855
      End
      Begin VB.Label lblSubject 
         BackStyle       =   0  'Transparent
         Caption         =   "Subject:"
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
         Index           =   0
         Left            =   120
         TabIndex        =   9
         Top             =   840
         Width           =   855
      End
   End
   Begin VB.Frame gboxSeperatorTop 
      Height          =   135
      Left            =   0
      TabIndex        =   8
      Top             =   740
      Width           =   7575
   End
   Begin MSComctlLib.StatusBar sbarMessage 
      Align           =   2  'Align Bottom
      Height          =   240
      Left            =   0
      TabIndex        =   6
      Top             =   5415
      Width           =   7560
      _ExtentX        =   13335
      _ExtentY        =   423
      Style           =   1
      _Version        =   393216
      BeginProperty Panels {8E3867A5-8586-11D1-B16A-00C0F0283628} 
         NumPanels       =   1
         BeginProperty Panel1 {8E3867AB-8586-11D1-B16A-00C0F0283628} 
         EndProperty
      EndProperty
   End
   Begin RichTextLib.RichTextBox rtxtMessageBody 
      Height          =   3210
      Left            =   0
      TabIndex        =   5
      Top             =   2200
      Width           =   7575
      _ExtentX        =   13361
      _ExtentY        =   5662
      _Version        =   393217
      ScrollBars      =   2
      TextRTF         =   $"frmMessage.frx":1012
   End
   Begin ComctlLib.ImageList ilstMessageMed 
      Left            =   6120
      Top             =   120
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   32
      ImageHeight     =   32
      MaskColor       =   16711935
      _Version        =   327682
      BeginProperty Images {0713E8C2-850A-101B-AFC0-4210102A8DA7} 
         NumListImages   =   3
         BeginProperty ListImage1 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "frmMessage.frx":1094
            Key             =   ""
         EndProperty
         BeginProperty ListImage2 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "frmMessage.frx":17A6
            Key             =   ""
         EndProperty
         BeginProperty ListImage3 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "frmMessage.frx":1EB8
            Key             =   ""
         EndProperty
      EndProperty
   End
   Begin VB.Menu mnuFile 
      Caption         =   "&File"
      Begin VB.Menu mnuFileNewMessage 
         Caption         =   "&New Message"
      End
      Begin VB.Menu mnuFileOpenMessage 
         Caption         =   "&Open Message"
      End
      Begin VB.Menu mnuFileSaveMessage 
         Caption         =   "S&ave Message"
      End
      Begin VB.Menu mnuFileExit 
         Caption         =   "E&xit"
      End
   End
End
Attribute VB_Name = "frmMessage"
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
' smime.vbp
'
' This is a VB sample that illustrates how to use CAPICOM's SignedData object
' with the CDO object to produce and verify S/MIME messages.
'
' This sample is in part based the knowledge base article "HOWTO: Send Digitally
' Signed Messages Using CDOEX" at:
' http://support.microsoft.com/support/kb/articles/Q280/3/91.ASP
'
' It utilizes the following references:
'   * Microsoft CDO for Exchange 2000 (for accessing messages)
'   * CAPICOM 2.0 (for signing and verifying messages)
'   * Microsoft ActiveX Data Objects 2.5 or above
'   * Microsoft Scripting Runtime
'
' See the following RFC's for more information on S/MIME:
'   * http://www.ietf.org/rfc/rfc2311.txt?number=2311
'   * http://www.ietf.org/rfc/rfc1847.txt?number=1847
'
' Note: For simplicity, this sample does not handle exception.
'
'******************************************************************************
Option Explicit

' Constants
Const CERT_KEY_SPEC_PROP_ID = 6
Const CdoAddressListGAL = 0
Const CdoAddressListPAB = 1
Const cdlOFNFileMustExist = &H1000
Const cdlOFNHideReadOnly = &H4
Const cdlOFNPathMustExist = &H800
Const cdlOFNCreatePrompt = &H2000

' Globals
Dim oSigner As New CAPICOM.Signer
Dim oMessage As New CDO.Message

Private Declare Function GetUserName Lib "advapi32.dll" Alias "GetUserNameA" (ByVal lpBuffer As String, nSize As Long) As Long

'******************************************************************************
'
' Function:     GetLoggedInUser
'
' Purpose:      Return a string containing the name of the currently
'               logged in user.
'
' Copyright (C) 1999- 2002.  Microsoft Corporation.  All rights reserved.
'
'******************************************************************************

Private Function GetLoggedInUser(sUserName As String) As Boolean
Dim sBuff   As String * 25
Dim lRet    As Long

GetLoggedInUser = True

'Get the user name, remove NULLs, and trim trailing spaces.
lRet = GetUserName(sBuff, 25)
sUserName = Trim$(Left(sBuff, InStr(sBuff, Chr(0)) - 1))

'Return false if no name is returned.
If sUserName = vbNullString Then
   GetLoggedInUser = False
End If

End Function

Private Sub btnBadEnc_Click()
    Call DecryptMessage(oMessage)
End Sub

Private Sub btnGoodSig_Click()
    Call oSigner.Certificate.Display
End Sub

Private Sub Form_Resize()
    ' make sure that the fields re-size with the form
    If (Me.Width > 1650) Then
        Me.txtTo.Width = (Me.Width - 1650)
        Me.txtCC.Width = (Me.Width - 1650)
        Me.txtSubject.Width = (Me.Width - 1650)
        Me.txtSubjectValue.Width = (Me.Width - 1650)
        Me.txtToValue.Width = (Me.Width - 1650)
        Me.txtCCValue.Width = (Me.Width - 1650)
        Me.txtSubjectValue.Width = (Me.Width - 1650)
    End If
    If (Me.Width > 90) Then
        Me.gboxSeperatorTop.Width = (Me.Width - 90)
        Me.gboxNewMail.Width = (Me.Width - 90)
        Me.gboxViewMail.Width = (Me.Width - 90)
        Me.rtxtMessageBody.Width = (Me.Width - 90)
    End If
    If (Me.Height > 600) Then
        Me.btnBadEnc.Left = (Me.Width - 1200)
        Me.btnBadSig.Left = (Me.Width - 1200)
        Me.btnGoodEnc.Left = (Me.btnGoodEnc.Width + (Me.btnBadEnc.Left + 100))
        Me.btnGoodSig.Left = (Me.btnGoodSig.Width + (Me.btnBadSig.Left + 100))
    End If
    If (Me.Height > 3255) Then Me.rtxtMessageBody.Height = (Me.Height - 3255)
End Sub

'******************************************************************************
'
' Function:     SignMessage
'
' Parameters:   oMsg    -   A CDO object representing a properly formed MIME
'                           message. [in/out]
'
'               bClear  -   a boolean specifying if the message is to be signed
'                           using a detached PKCS7 or attached PKCS7. [in]
'
'
' Purpose:      Return a S/MIME message derived from the passed in message
'
' Copyright (C) 1999- 2002.  Microsoft Corporation.  All rights reserved.
'
'******************************************************************************

Private Function SignMessage(ByRef oMsg As CDO.Message, bClear As Boolean) As Boolean
    Dim oSignedMsg As New CDO.Message
    Dim oBodyPart As CDO.IBodyPart
    Dim cFields As ADODB.Fields
    Dim oStream As ADODB.Stream
    Dim oSignedData As New CAPICOM.SignedData
    Dim oUtilities As New CAPICOM.Utilities
    Dim oAttribute As New CAPICOM.Attribute
    Dim oSignerCertificate As CAPICOM.Certificate
    Dim cSignerCertificates As CAPICOM.Certificates
    Dim oStore As New CAPICOM.Store
    Dim szSignature, byteSignature() As Byte

    On Error GoTo ErrorHandler
    
    ' create the SignedData object we will use to create the PKCS7
    Set oSignedData = New CAPICOM.SignedData
    
    ' create the new message
    Set oSignedMsg = New CDO.Message
    
    ' select the signer certificate
    oStore.Open CAPICOM_CURRENT_USER_STORE, "My", CAPICOM_STORE_OPEN_READ_ONLY
    Set cSignerCertificates = oStore.Certificates.Find(CAPICOM_CERTIFICATE_FIND_EXTENDED_PROPERTY, CERT_KEY_SPEC_PROP_ID).Find(CAPICOM_CERTIFICATE_FIND_APPLICATION_POLICY, "Secure Email")

    Select Case cSignerCertificates.Count
        Case 0
            MsgBox ("Error: No signing certificate can be found.")
        Case 1
            oSigner.Certificate = cSignerCertificates(1)
        Case Else
            Set cSignerCertificates = cSignerCertificates.Select("S/MIME Certificates", "Please select a certificate to sign with.")
            If (cSignerCertificates.Count = 0) Then
                MsgBox ("Error: Certificate selection dialog was cancelled.")
                Exit Function
            End If
            oSigner.Certificate = cSignerCertificates(1)
    End Select
    
    ' set the from field based off of the selected certificate
    oSignedMsg.From = oSigner.Certificate.GetInfo(CAPICOM_CERT_INFO_SUBJECT_EMAIL_NAME)

        
    ' set the signing time in UTC time
    Set oAttribute = New CAPICOM.Attribute
    oAttribute.Name = CAPICOM_AUTHENTICATED_ATTRIBUTE_SIGNING_TIME
    oAttribute.Value = oUtilities.LocalTimeToUTCTime(Now)
    oSigner.AuthenticatedAttributes.Add oAttribute
    
    Select Case bClear
    Case True
        ' this is to be a clear text signed message so we need to copy the interesting
        ' parts (sender, recipient, and subject) into the new header
        oSignedMsg.To = oMsg.To
        oSignedMsg.CC = oMsg.CC
        oSignedMsg.Subject = oMsg.Subject

        Set oBodyPart = oSignedMsg.BodyPart.AddBodyPart
        Set cFields = oBodyPart.Fields
        cFields.Item(cdoContentType).Value = oMsg.BodyPart.BodyParts(1).Fields.Item(cdoContentType).Value
        cFields.Update
        
        Set oStream = oBodyPart.GetDecodedContentStream
        oStream.WriteText oMsg.BodyPart.BodyParts(1).GetDecodedContentStream.ReadText
        oStream.Flush
                        
        ' set the content to be signed
        oSignedData.Content = StrConv(oSignedMsg.BodyPart.BodyParts(1).GetStream.ReadText, vbFromUnicode)
                
        ' sign the content
        szSignature = oSignedData.Sign(oSigner, True, CAPICOM_ENCODE_BINARY)
        
        ' Get the string data as a byte array
        byteSignature = szSignature
        
        ' Attach the signature and let CDO base64 encode it
        Set oBodyPart = oSignedMsg.BodyPart.AddBodyPart
        Set cFields = oBodyPart.Fields
        oBodyPart.Fields.Item("urn:schemas:mailheader:content-type").Value = "application/x-pkcs7-signature" & vbCrLf & "Name = ""smime.p7s"""
        oBodyPart.Fields.Item("urn:schemas:mailheader:content-transfer-encoding").Value = "base64"
        oBodyPart.Fields.Item("urn:schemas:mailheader:content-disposition").Value = "attachment;" & vbCrLf & "FileName=""smime.p7s"""
        cFields.Update
        
        Set oStream = oBodyPart.GetDecodedContentStream
        oStream.Type = ADODB.StreamTypeEnum.adTypeBinary
        oStream.Write (byteSignature)
        oStream.Flush
        
        ' Set the messages content type, this needs to be done last to ensure it is not changed when we add the BodyParts
        oSignedMsg.Fields.Item("urn:schemas:mailheader:content-type").Value = "multipart/signed;" & vbCrLf & "protocol=""application/x-pkcs7-signature"";" & vbCrLf & "micalg=SHA1;"
        oSignedMsg.Fields.Update
        
    Case False
        ' this is to be a opaquely signed message so we need to copy the entire message into our
        ' new encrypted message
        oSignedMsg.DataSource.OpenObject oMsg, cdoIMessage
        
        ' Set up main bodypart
        Set oBodyPart = oSignedMsg.BodyPart
        oBodyPart.ContentMediaType = "application/pkcs7-mime;" & vbCrLf & "smime-type=signed-data;" & vbCrLf & "name=""smime.p7m"""
        oBodyPart.ContentTransferEncoding = "base64"
        oBodyPart.Fields("urn:schemas:mailheader:content-disposition") = "attachment;" & vbCrLf & "FileName=""smime.p7m"""
        oBodyPart.Fields.Update
          
        ' set the from field based off of the selected certificate
        oMsg.From = oSigner.Certificate.GetInfo(CAPICOM_CERT_INFO_SUBJECT_EMAIL_NAME)
        
        ' set the content to be signed
        oSignedData.Content = StrConv(oMsg.BodyPart.GetStream.ReadText, vbFromUnicode)
        
        ' Sign the content
        szSignature = oSignedData.Sign(oSigner, False, CAPICOM_ENCODE_BINARY)
        
        ' Get the string data as a byte array
        byteSignature = szSignature
        
        ' Attach the signature and let CDO base64 encode it
        Set oStream = oBodyPart.GetDecodedContentStream
        oStream.Type = ADODB.StreamTypeEnum.adTypeBinary
        oStream.Write (byteSignature)
        oStream.Flush
    End Select

 
    ' Signing Was sucessfull
    SignMessage = True
    Set oMsg = oSignedMsg
   
GoTo CleanUp

ErrorHandler:
    'If the user cancels, don't display error message
    If Err.Number <> CAPICOM_E_CANCELLED Then
        MsgBox "Error: " & Hex(Err.Number) & ": " & Err.Description
    End If
    Err.Clear
    
    ' An error occurred
    SignMessage = False
    Set oMsg = Nothing

CleanUp:
    Set oSignedMsg = Nothing
    Set oBodyPart = Nothing
    Set cFields = Nothing
    Set oStream = Nothing
    Set oSignedData = Nothing
    Set oUtilities = Nothing
    Set oAttribute = Nothing
    Set oSignerCertificate = Nothing
    Set cSignerCertificates = Nothing
    Set oStore = Nothing
End Function

'******************************************************************************
'
' Function:     EncryptMessage
'
' Parameters:   oMsg        -   A CDO object representing a properly formed MIME
'                               message. [in/out]
'
'               oRecipients  -  A collection of CAPICOM certificate objects in
'                               which should be capable of decrypting this
'                               message. [in]
'
'
' Purpose:      Return a S/MIME Encrypted message derived from the passed in message
'
' Copyright (C) 1999- 2002.  Microsoft Corporation.  All rights reserved.
'
'******************************************************************************

Private Function EncryptMessage(ByRef oMsg As CDO.Message, oRecipients As Certificates) As Boolean

    Dim oEncryptedMsg As New CDO.Message
    Dim oBodyPart As CDO.IBodyPart
    Dim cFields As ADODB.Fields
    Dim oStream As ADODB.Stream
    Dim oEnvelopedData As New CAPICOM.EnvelopedData
    Dim oRecipient As CAPICOM.Certificate
    Dim szEncMessage, byteEncMessage() As Byte
     
    ' Copy input into output message
    oEncryptedMsg.DataSource.OpenObject oMsg, cdoIMessage
    
    ' Set up main bodypart
    Set oBodyPart = oEncryptedMsg.BodyPart
    oBodyPart.ContentMediaType = "application/pkcs7-mime;" & vbCrLf & "smime-type=enveloped-data;" & vbCrLf & "name=smime.p7m;"
    oBodyPart.ContentTransferEncoding = "base64"
    oBodyPart.Fields("urn:schemas:mailheader:content-disposition") = "attachment;FileName=""smime.p7m"""
    oBodyPart.Fields("urn:schemas:mailheader:date").Value = oMsg.Fields("urn:schemas:mailheader:date").Value
    oBodyPart.Fields.Update
    
    ' Add each of the passed in recipients to the EnvelopedData recipient's collection
    For Each oRecipient In oRecipients
        oEnvelopedData.Recipients.Add oRecipient
    Next
    
    ' Encrypt content
    oEnvelopedData.Content = StrConv(oMsg.BodyPart.GetStream.ReadText, vbFromUnicode)
    szEncMessage = oEnvelopedData.Encrypt(CAPICOM_ENCODE_BINARY)
    
    ' Get the string data as a byte array
    byteEncMessage = szEncMessage
    
    ' Write the CMS blob into the main bodypart and let CDO do the base64 encoding
    Set oStream = oEncryptedMsg.BodyPart.GetDecodedContentStream
    oStream.Type = adTypeBinary
    oStream.Write byteEncMessage
    oStream.Flush
       
    ' Return out finished message
    EncryptMessage = True
    Set oMsg = oEncryptedMsg

GoTo CleanUp

ErrorHandler:
    MsgBox Err.Number & ": " & Err.Description, , "Error:"
    Err.Clear
    EncryptMessage = False
    Set oMsg = Nothing

CleanUp:
    ' clean up
    Set oBodyPart = Nothing
    Set oEnvelopedData = Nothing
    Set oStream = Nothing
    Set oRecipient = Nothing
    Set oEncryptedMsg = Nothing
    Set oBodyPart = Nothing
    Set cFields = Nothing
End Function
Private Function IsSigned(oInMsg As CDO.Message) As Boolean
    Dim szContentType As String
    
    szContentType = oInMsg.BodyPart.Fields.Item("urn:schemas:mailheader:content-type").Value
    
    If ((InStr(1, szContentType, "application/x-pkcs7-signature", vbTextCompare) <> 0) Or (InStr(1, szContentType, "signed-data", vbTextCompare) <> 0)) Then
        IsSigned = True
    Else
        IsSigned = False
    End If
    
End Function

Private Function IsEncrypted(oInMsg As CDO.Message) As Boolean
    Dim szContentType As String
    
    szContentType = oInMsg.BodyPart.Fields.Item("urn:schemas:mailheader:content-type").Value
    
    If (InStr(1, szContentType, "enveloped-data", vbTextCompare) <> 0) Then
        IsEncrypted = True
    Else
        IsEncrypted = False
    End If

End Function

'******************************************************************************
'
' Function:     GetContent
'
' Parameters:   oInMsg      -   A CDO object representing a properly formed MIME
'                               message. [in/out]
'
'
' Purpose:      Return the portion of a S/MIME message that a signature was
'               calculated over.
'
' Copyright (C) 1999- 2002.  Microsoft Corporation.  All rights reserved.
'
'******************************************************************************
Private Function GetContent(oInMsg As CDO.Message) As String
    Dim iStart As Integer, iLength As Integer
    Dim szMessage, szBodyPart

    szMessage = oInMsg.GetStream.ReadText

    szBodyPart = "--" + oInMsg.BodyPart.GetFieldParameter("urn:schemas:mailheader:content-type", "boundary") + vbCrLf
    
    iStart = InStr(1, szMessage, szBodyPart) + Len(szBodyPart)
    iLength = InStr((iStart + 1), szMessage, szBodyPart) - iStart - 2
    
    GetContent = Mid(szMessage, iStart, iLength)
End Function

'******************************************************************************
'
' Function:     GetSignature
'
' Parameters:   oMsg        -   A CDO object representing a properly formed MIME
'                               message. [in/out]
'
'
' Purpose:      Return the portion of a S/MIME message that contains the PKCS7
'               signature.
'
' Copyright (C) 1999- 2002.  Microsoft Corporation.  All rights reserved.
'
'******************************************************************************
Private Function GetSignature(oInMsg As CDO.Message) As String

    If InStr(1, oInMsg.Fields.Item("urn:schemas:mailheader:content-disposition").Value, "attachment", vbTextCompare) <> 0 Then
        GetSignature = oInMsg.BodyPart.GetEncodedContentStream.ReadText
    Else
        GetSignature = oInMsg.BodyPart.BodyParts(2).GetEncodedContentStream.ReadText
    End If
End Function
'******************************************************************************
'
' Function:     LoadMessage
'
' Parameters:   FileName        -   String representing the FQFN of the message
'                                   to be loaded.
'
'
' Purpose:      Load a CDO message into the global CDO Message object and
'               display its important elements in the form.
'
' Copyright (C) 1999- 2002.  Microsoft Corporation.  All rights reserved.
'
'******************************************************************************
Private Function LoadMessage(FileName As String) As Boolean
    Dim oStream As New ADODB.Stream
    Dim iDsrc As IDataSource
    Dim oAttribute As CAPICOM.Attribute
    Dim szSignature As String
    Dim oUtilities As New CAPICOM.Utilities
    
    On Error GoTo ErrorHandler
    
    ' Load the message from disk
    oStream.Open
    oStream.LoadFromFile FileName

    Set iDsrc = oMessage
    iDsrc.OpenObject oStream, "_Stream"
    oStream.Close
    
    ' Make room for the fields associated with viewing a message
    Me.gboxViewMail.Visible = True
    Me.gboxNewMail.Visible = False
    Me.rtxtMessageBody.Top = ((Me.gboxViewMail.Top + Me.gboxViewMail.Height) + 25)
        
    ' Hide the encryption status buttons if the message is not encrypted
    If (IsEncrypted(oMessage) = False) Then
        Me.btnBadEnc.Visible = False
        Me.btnGoodEnc.Visible = False
    Else
        ' If the message is encrypted then update the form accordingly
        If (DecryptMessage(oMessage)) Then
            Me.btnGoodEnc.Visible = True
        Else
            Me.btnBadEnc.Visible = True
        End If
    End If
    
    ' Load the message contents into the form
    Me.txtFromValue.Text = oMessage.From
    Me.txtToValue.Text = oMessage.To
    Me.txtCCValue.Text = oMessage.CC
    Me.txtSubjectValue.Text = oMessage.Subject
    Me.txtDateValue.Text = oUtilities.UTCTimeToLocalTime(oMessage.Fields("urn:schemas:mailheader:date").Value)

    ' Lock the fields that could potentialy be modified since we are looking at an existing mail
    Me.rtxtMessageBody.Locked = True
    Me.rtxtMessageBody.Text = oMessage.TextBody
    
    ' Hide the signature status buttons if the message is not signed
    If (IsSigned(oMessage) = False) Then
            Me.btnGoodSig.Visible = False
            Me.btnBadSig.Visible = False
    End If

    ' If the message is signed then update the form accordingly
    If (IsSigned(oMessage) = True And (IsEncrypted(oMessage) = False)) Then

        ' We must verify the message to get the signing certificate
        If (VerifyMessage(oMessage) = True) Then
            Me.txtFromValue.Text = oSigner.Certificate.GetInfo(CAPICOM_CERT_INFO_SUBJECT_EMAIL_NAME)
        
            For Each oAttribute In oSigner.AuthenticatedAttributes
                If (oAttribute.Name = CAPICOM_AUTHENTICATED_ATTRIBUTE_SIGNING_TIME) Then
                    Me.txtDateValue.Text = oUtilities.UTCTimeToLocalTime(oAttribute.Value)
                End If
            Next
            
            Me.btnGoodSig.Visible = True
        Else
            Me.btnBadSig.Visible = True
        End If
    
    End If

GoTo CleanUp

ErrorHandler:
    MsgBox Err.Number & ": " & Err.Description, , "Error:"

CleanUp:
    Set oStream = Nothing
    Set iDsrc = Nothing
    Set oAttribute = Nothing
    Set oUtilities = Nothing
End Function
'******************************************************************************
'
' Function:     VerifyMessage
'
' Parameters:   oMsg        -   A CDO object representing a properly formed S/MIME
'                               message. [in/out]
'
'
' Purpose:      Verify that a S/MIME message is signature valid
'
' Copyright (C) 1999- 2002.  Microsoft Corporation.  All rights reserved.
'
'******************************************************************************
Private Function VerifyMessage(ByRef oInMsg As CDO.Message) As Boolean
    Dim oSignedData As New CAPICOM.SignedData
    Dim szSignature As String
    Dim iStart As Integer, iEnd As Integer, szTemp As String
    On Error GoTo ErrorHandler
    
    ' get the pkcs7 signature
    szSignature = GetSignature(oInMsg)

    ' verify The message
    Set oSignedData = New CAPICOM.SignedData
    
    ' is this a detached or attached signature, deal with the differences
    oSignedData.Content = StrConv(GetContent(oInMsg), vbFromUnicode)
    Call oSignedData.Verify(szSignature, True, CAPICOM_VERIFY_SIGNATURE_ONLY)
    

    ' update the global signer for use later
    Set oSigner = oSignedData.Signers.Item(1)
        
    VerifyMessage = True

GoTo CleanUp
ErrorHandler:
    MsgBox "Error: " & Hex(Err.Number) & ": " & Err.Description
    Err.Clear
    
    VerifyMessage = False

CleanUp:
    ' clean up
    Set oSignedData = Nothing

End Function
'******************************************************************************
'
' Function:     DecryptMessage
'
' Parameters:   oMsg        -   A CDO object representing a properly formed S/MIME
'                               message. [in/out]
'
'
' Purpose:      To decrypt the supplied message and return a decrypted version in
'               the oMsg parameter.
'
' Copyright (C) 1999- 2002.  Microsoft Corporation.  All rights reserved.
'
'******************************************************************************

Private Function DecryptMessage(ByRef oMsg As CDO.Message) As Boolean
    Dim oDecryptedMsg As New CDO.Message
    Dim oStream As New ADODB.Stream
    Dim iDsrc As IDataSource
    Dim oEnvelopedData As New CAPICOM.EnvelopedData
    Dim byteDecryptedMessage() As Byte
         
    On Error GoTo ErrorHandler
    
    ' Decrypt content
    Call oEnvelopedData.Decrypt(oMsg.BodyPart.GetEncodedContentStream.ReadText)
    
    ' Convert the message to a byte array
    byteDecryptedMessage = oEnvelopedData.Content

    ' Load the decrypted message into a stream
    oStream.Open
    oStream.Type = adTypeBinary
    oStream.Write byteDecryptedMessage

    Set iDsrc = oDecryptedMsg
    iDsrc.OpenObject oStream, "_Stream"
    
    ' Return the status
    Set oMsg = oDecryptedMsg
    DecryptMessage = True

GoTo CleanUp

ErrorHandler:
    MsgBox Err.Number & ": " & Err.Description, , "Error:"
    
    ' Return the false values
    DecryptMessage = False
    Set oMsg = Nothing
    
CleanUp:
    ' clean up
    Set oEnvelopedData = Nothing
    Set oDecryptedMsg = Nothing
    Set oStream = Nothing
    Set iDsrc = Nothing
End Function
Private Sub mnuFileExit_Click()
    Unload Me
End Sub
Private Sub mnuFileNewMessage_Click()
    ' Show the relative fields
    Me.gboxNewMail.Visible = True
    Me.gboxViewMail.Visible = False
    Me.rtxtMessageBody.Top = ((Me.gboxNewMail.Top + Me.gboxNewMail.Height) + 25)
    Me.rtxtMessageBody.Locked = False
    
    ' Blank the fields to prepare for new message
    Me.txtCC.Text = ""
    Me.txtTo.Text = ""
    Me.txtSubject = ""
    Me.rtxtMessageBody = ""
    
    ' Make the To field the one with focus
    Me.txtTo.SetFocus
End Sub

Private Sub mnuFileOpenMessage_Click()
    ' Show the open dialog
    Me.cmnFileSelection.DefaultExt = ".eml"
    Me.cmnFileSelection.Filter = "Message (*.eml;*.msg)|*.eml;*.msg|All Files (*.*)|*.*"
    Me.cmnFileSelection.DialogTitle = "Select Message to Open"
    Me.cmnFileSelection.Flags = (cdlOFNFileMustExist Or cdlOFNHideReadOnly Or cdlOFNPathMustExist)
    Me.cmnFileSelection.ShowOpen
        
    If Me.cmnFileSelection.FileName <> "" Then LoadMessage (Me.cmnFileSelection.FileName)
End Sub

Function FindRecipientByEmail(ByVal szEmail, ByRef oRecipient As CAPICOM.Certificate) As Boolean
    Dim oStore As New CAPICOM.Store
    Dim oCertificates As New CAPICOM.Certificates
    Dim oCertificate As New CAPICOM.Certificate
    
    On Error GoTo ErrorHandler
    
    ' Open the AddressBook store to see if we can find their certificate in their
    oStore.Open CAPICOM_CURRENT_USER_STORE, "AddressBook", CAPICOM_STORE_OPEN_READ_ONLY
    
    ' We are only interested in those certificates that are explicitly good for secure email and key encipherment
    Set oCertificates = oStore.Certificates.Find(CAPICOM_CERTIFICATE_FIND_APPLICATION_POLICY, "Secure Email").Find(CAPICOM_CERTIFICATE_FIND_KEY_USAGE, "KeyEncipherment", True)
  
    ' This simply picks the first match on email address
    For Each oCertificate In oCertificates
        If InStr(1, szEmail, "@") Then
            ' looks like a complete email address
            If (oCertificate.GetInfo(CAPICOM_CERT_INFO_SUBJECT_EMAIL_NAME) = szEmail) Then
                FindRecipientByEmail = True
                Exit For
            End If
        Else
            ' looks like a partial email address or alias
            If (InStr(1, oCertificate.GetInfo(CAPICOM_CERT_INFO_SUBJECT_EMAIL_NAME), szEmail)) Then
                FindRecipientByEmail = True
                Exit For
            End If
        End If
    Next

If (FindRecipientByEmail = True) Then
    Set oRecipient = oCertificate
Else
    MsgBox "Unable to find encryption certificate for '" & szEmail & "', this recipient will not be included."
    Set oRecipient = Nothing
End If

GoTo CleanUp

ErrorHandler:
    MsgBox Err.Number & ": " & Err.Description, , "Error:"

CleanUp:
Set oStore = Nothing
Set oCertificates = Nothing
Set oCertificate = Nothing
End Function
Function ResolveNames(ByRef szNames As String) As CAPICOM.Certificates
    Dim oRecipients As New CAPICOM.Certificates
    Dim oRecipient As New CAPICOM.Certificate
    Dim aNames As Variant, vName As Variant
    Dim bFound As Boolean
    
    On Error GoTo ErrorHandler
    
    ' Normalize the delimiter to be ;
    szNames = Replace(szNames, ",", ";")

    ' Convert the ; delimited list to an array
    aNames = Split(szNames, ";")
    szNames = ""
    
    For Each vName In aNames
        If (FindRecipientByEmail(vName, oRecipient)) Then
            oRecipients.Add oRecipient
            szNames = szNames + oRecipient.GetInfo(CAPICOM_CERT_INFO_SUBJECT_EMAIL_NAME) + ";"
        End If
    Next
    
    ' Trim the trailing ; delimiter
    If Len(szNames) > 1 Then
        szNames = Mid(szNames, 1, Len(szNames) - 1)
    End If
    
GoTo CleanUp

ErrorHandler:
    MsgBox Err.Number & ": " & Err.Description, , "Error:"
    Set ResolveNames = Nothing
    
CleanUp:
    ' clean up
    Set oRecipient = Nothing
    
    ' Return Recipient collection
    Set ResolveNames = oRecipients
End Function
Private Sub mnuFileSaveMessage_Click()
    Dim oBodyPart As CDO.IBodyPart
    Dim cFields As ADODB.Fields
    Dim oStream As ADODB.Stream
    Dim oUtilities As New CAPICOM.Utilities
    Dim oRecipients As CAPICOM.Certificates
    Dim szNames As String
    
    On Error GoTo ErrorHandler
    ' Lets resolve the names to email addresses
    If Len(Me.txtTo) > 0 Then
        szNames = Me.txtTo
        Call ResolveNames(szNames)
        Me.txtTo = szNames
    End If
    If Len(Me.txtCC) > 0 Then
        szNames = Me.txtCC
        Call ResolveNames(szNames)
        Me.txtCC = szNames
    End If
    
    ' Gather the recipient certificates
    szNames = Me.txtTo & ";" & Me.txtCC
    Set oRecipients = ResolveNames(szNames)
    

    ' Make sure the minimum fields are populated
    If Me.txtTo.Text = "" Then
        MsgBox "You must specify at least one valid recipient in the 'To:' field"
        Exit Sub
    End If
    If Me.txtSubject.Text = "" Then
        MsgBox "You must specify a subject for the message in the 'Subject:' field"
        Exit Sub
    End If
    
    ' create the message itself, this essentialy consists of setting a few header values and adding
    ' a new plain/text bodypart
    
    ' set sender, recipient, and subject.
    Set oMessage = New CDO.Message
    oMessage.To = Me.txtTo.Text
    oMessage.CC = Me.txtCC.Text
    oMessage.Subject = Me.txtSubject.Text
    oMessage.Fields("urn:schemas:mailheader:date").Value = oUtilities.LocalTimeToUTCTime(Now)
    oMessage.Fields.Update
    
    
    ' Set the current users email address to our best guess, we will get an authenticated address
    ' when we do signed mail from the signers certificate
    Dim szUserName As String
    If GetLoggedInUser(szUserName) Then
        oMessage.From = LCase(szUserName) & "@" & "yourdomain.com"
    Else
        oMessage.From = "Anonymous"
    End If
    
    Set oBodyPart = oMessage.BodyPart.AddBodyPart
    Set cFields = oBodyPart.Fields
    cFields.Item(cdoContentType) = cdoTextPlain
    cFields.Update
    
    Set oStream = oBodyPart.GetDecodedContentStream
    oStream.WriteText Me.rtxtMessageBody.Text
    oStream.Flush

    ' sign, encrypt or sign/encrypt the message
    If ((Me.tbarMessage.Buttons.Item(1).Value = tbrPressed) And (Me.tbarMessage.Buttons.Item(2).Value = tbrUnpressed)) Then
        ' It is a signed message
        If SignMessage(oMessage, True) = False Then Exit Sub
    ElseIf ((Me.tbarMessage.Buttons.Item(1).Value = tbrUnpressed) And (Me.tbarMessage.Buttons.Item(2).Value = tbrPressed)) Then
        ' It is a encrypted message
        If EncryptMessage(oMessage, oRecipients) = False Then Exit Sub
    ElseIf ((Me.tbarMessage.Buttons.Item(1).Value = tbrPressed) And (Me.tbarMessage.Buttons.Item(1).Value = tbrPressed)) Then
        ' It is a signed and encrypted message
        If SignMessage(oMessage, True) = False Then Exit Sub
        If EncryptMessage(oMessage, oRecipients) = False Then Exit Sub
    End If
    
    ' the message should look okay now, where would they like to save it to?
    Me.cmnFileSelection.DefaultExt = ".eml"
    Me.cmnFileSelection.Filter = "Message (*.eml;*.msg)|*.eml;*.msg|All Files (*.*)|*.*"
    Me.cmnFileSelection.DialogTitle = "Select file to save message to"
    Me.cmnFileSelection.Flags = (cdlOFNCreatePrompt Or cdlOFNHideReadOnly Or cdlOFNPathMustExist)
    Me.cmnFileSelection.ShowOpen

    If (Me.cmnFileSelection.FileName <> "") Then
        oMessage.GetStream.SaveToFile Me.cmnFileSelection.FileName, adSaveCreateOverWrite
    End If

Exit Sub
ErrorHandler:
    MsgBox Err.Number & ": " & Err.Description, , "Error:"

CleanUp:
    ' clean up
    Set oBodyPart = Nothing
    Set cFields = Nothing
    Set oStream = Nothing
    Set oUtilities = Nothing
    Set oRecipients = Nothing
End Sub

Private Sub tbarMessage_ButtonClick(ByVal Button As ComctlLib.Button)
    If Button.Index = 3 Then
        ' we need to resolve names
        Dim szNames As String
        
        If Len(Me.txtTo) > 0 Then
             szNames = Me.txtTo
             Call ResolveNames(szNames)
             Me.txtTo = szNames
         End If
         
         If Len(Me.txtCC) > 0 Then
             szNames = Me.txtCC
             Call ResolveNames(szNames)
             Me.txtCC = szNames
         End If
    End If
End Sub


