VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form FrmFind 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Find"
   ClientHeight    =   7560
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   7050
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   7560
   ScaleWidth      =   7050
   StartUpPosition =   3  'Windows Default
   Begin VB.ComboBox cmbStoreName 
      Height          =   315
      Left            =   2400
      TabIndex        =   12
      Top             =   1080
      Width           =   4455
   End
   Begin VB.ComboBox cmbStoreLocation 
      Height          =   315
      Left            =   2400
      TabIndex        =   8
      Top             =   600
      Width           =   4455
   End
   Begin MSComctlLib.ListView lstFoundCerts 
      Height          =   2895
      Left            =   240
      TabIndex        =   7
      Top             =   4440
      Width           =   6495
      _ExtentX        =   11456
      _ExtentY        =   5106
      View            =   3
      LabelEdit       =   1
      LabelWrap       =   -1  'True
      HideSelection   =   -1  'True
      FullRowSelect   =   -1  'True
      _Version        =   393217
      ForeColor       =   -2147483640
      BackColor       =   -2147483643
      BorderStyle     =   1
      Appearance      =   1
      NumItems        =   0
   End
   Begin VB.CommandButton cmdExit 
      Caption         =   "&Exit"
      Height          =   375
      Left            =   5040
      TabIndex        =   5
      Top             =   3600
      Width           =   1535
   End
   Begin VB.TextBox txtCriteria 
      Height          =   315
      Left            =   240
      TabIndex        =   3
      Top             =   3120
      Width           =   6375
   End
   Begin VB.ComboBox cmbFindType 
      Height          =   315
      Left            =   240
      TabIndex        =   2
      Top             =   2160
      Width           =   6375
   End
   Begin VB.CommandButton cmdFind 
      Caption         =   "&Find"
      Height          =   375
      Left            =   3360
      TabIndex        =   1
      Top             =   3600
      Width           =   1535
   End
   Begin VB.Label Label4 
      Caption         =   "Store Name/Search Filter:"
      Height          =   255
      Left            =   120
      TabIndex        =   11
      Top             =   1080
      Width           =   2175
   End
   Begin VB.Label Label3 
      Caption         =   "Store Location:"
      Height          =   255
      Left            =   120
      TabIndex        =   10
      Top             =   600
      Width           =   2175
   End
   Begin VB.Label Label2 
      Caption         =   "Search in Store:"
      Height          =   255
      Left            =   120
      TabIndex        =   9
      Top             =   120
      Width           =   6615
   End
   Begin VB.Label Label1 
      Caption         =   "Certificates Found:"
      Height          =   255
      Left            =   240
      TabIndex        =   6
      Top             =   4080
      Width           =   3495
   End
   Begin VB.Label lblCriteria 
      Caption         =   "Please enter search criteria:"
      Height          =   255
      Left            =   240
      TabIndex        =   4
      Top             =   2760
      Width           =   3495
   End
   Begin VB.Label lblFindType 
      Caption         =   "Please select find type:"
      Height          =   255
      Left            =   240
      TabIndex        =   0
      Top             =   1920
      Width           =   3495
   End
End
Attribute VB_Name = "FrmFind"
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
' Find.vbp
'
' This is a VB sample that illustrates how to use features introduced in
' CAPICOM v2.0 to open stores and find filter out certificates based on a
' particular search criteria.  If no search criteria is entered all certificates
' in the store will be displayed.
'
' Note: For simplicity, this sample does not handle exception.
'
'******************************************************************************
Option Explicit

Dim st As New Store
Dim certs As Certificates
Dim cert As New Certificate
Dim FindType As CAPICOM_CERTIFICATE_FIND_TYPE
Dim StoreLocation As CAPICOM_STORE_LOCATION
Dim StoreName As String
Dim li As ListItem
Dim i As Integer

Private Sub form_Load()
    
    Dim colSubject, colIssuer, colValidFrom, colValidTo As ColumnHeader

     
    'list all possible store locations
    cmbStoreLocation.AddItem "CAPICOM_ACTIVE_DIRECTORY_USER_STORE"
    cmbStoreLocation.AddItem "CAPICOM_CURRENT_USER_STORE"
    cmbStoreLocation.AddItem "CAPICOM_LOCAL_MACHINE_STORE"
    cmbStoreLocation.AddItem "CAPICOM_SMART_CARD_USER_STORE"
    
    'select default store location
    cmbStoreLocation.ListIndex = 1
        
    'list possible store names
    cmbStoreName.AddItem "My"
    cmbStoreName.AddItem "Root"
    cmbStoreName.AddItem "AddressBook"
    cmbStoreName.AddItem "CA"
    
    'select default store name
    cmbStoreName.ListIndex = 0
    
     'list all available find types
    cmbFindType.AddItem "CAPICOM_CERTIFICATE_FIND_SHA1_HASH"
    cmbFindType.AddItem "CAPICOM_CERTIFICATE_FIND_SUBJECT_NAME"
    cmbFindType.AddItem "CAPICOM_CERTIFICATE_FIND_ISSUER_NAME"
    cmbFindType.AddItem "CAPICOM_CERTIFICATE_FIND_ROOT_NAME"
    cmbFindType.AddItem "CAPICOM_CERTIFICATE_FIND_TEMPLATE_NAME"
    cmbFindType.AddItem "CAPICOM_CERTIFICATE_FIND_EXTENSION"
    cmbFindType.AddItem "CAPICOM_CERTIFICATE_FIND_EXTENDED_PROPERTY"
    cmbFindType.AddItem "CAPICOM_CERTIFICATE_FIND_APPLICATION_POLICY"
    cmbFindType.AddItem "CAPICOM_CERTIFICATE_FIND_CERTIFICATE_POLICY"
    cmbFindType.AddItem "CAPICOM_CERTIFICATE_FIND_TIME_VALID"
    cmbFindType.AddItem "CAPICOM_CERTIFICATE_FIND_TIME_NOT_YET_VALID"
    cmbFindType.AddItem "CAPICOM_CERTIFICATE_FIND_TIME_EXPIRED"
    
    'select default find type
    cmbFindType.ListIndex = 1
    
    'Set up output listbox
    Set colIssuer = lstFoundCerts.ColumnHeaders.Add()
    Set colSubject = lstFoundCerts.ColumnHeaders.Add()
    Set colValidFrom = lstFoundCerts.ColumnHeaders.Add()
    Set colValidTo = lstFoundCerts.ColumnHeaders.Add()
    
    'set column names
    colIssuer.Text = "Issuer"
    colSubject.Text = "Subject"
    colValidFrom.Text = "Valid From"
    colValidTo.Text = "Valid To"
      
    'set column widths
    colIssuer.Width = lstFoundCerts.Width / 4
    colSubject.Width = lstFoundCerts.Width / 4
    colValidFrom.Width = lstFoundCerts.Width / 4
    colValidTo.Width = lstFoundCerts.Width / 4

    lstFoundCerts.ListItems.Clear
End Sub

Private Sub cmdFind_Click()
On Error GoTo Err_handler:
    Dim cert As Certificate
    
    'clear list box
    lstFoundCerts.ListItems.Clear
    
    'get store location
     Select Case cmbStoreLocation.List(cmbStoreLocation.ListIndex)
         Case "CAPICOM_ACTIVE_DIRECTORY_USER_STORE"
           StoreLocation = CAPICOM_ACTIVE_DIRECTORY_USER_STORE
         Case "CAPICOM_CURRENT_USER_STORE"
            StoreLocation = CAPICOM_CURRENT_USER_STORE
         Case "CAPICOM_LOCAL_MACHINE_STORE"
            StoreLocation = CAPICOM_LOCAL_MACHINE_STORE
         Case "CAPICOM_SMART_CARD_USER_STORE"
            StoreLocation = CAPICOM_SMART_CARD_USER_STORE
         Case Else
            MsgBox "Please enter valid store location!"
            Exit Sub
     End Select
       
    'get store name
    StoreName = cmbStoreName.Text
       
    'get Find Type
    FindType = cmbFindType.ListIndex
      
    'open store
    st.Open StoreLocation, StoreName
    
    Set certs = st.Certificates
     
    If FindType = CAPICOM_CERTIFICATE_FIND_TIME_EXPIRED Or _
       FindType = CAPICOM_CERTIFICATE_FIND_TIME_NOT_YET_VALID Or _
       FindType = CAPICOM_CERTIFICATE_FIND_TIME_VALID Then
        If txtCriteria.Text = "" Then
            txtCriteria.Text = Now
        Else
            txtCriteria.Text = CDate(txtCriteria.Text)
        End If
        Set certs = certs.Find(FindType, txtCriteria.Text)
    Else
        If txtCriteria.Text <> "" Then
            Set certs = certs.Find(FindType, txtCriteria.Text)
        End If
    End If
    
    If certs.Count = 0 Then
        MsgBox "No certs found"
    End If
    
    
    'list all found certificates
    For Each cert In certs
        
        Set li = lstFoundCerts.ListItems.Add(, , cert.GetInfo(CAPICOM_CERT_INFO_ISSUER_SIMPLE_NAME))
        Set li.Tag = cert
        
        i = lstFoundCerts.ListItems.Count
        lstFoundCerts.ListItems(i).ListSubItems.Add , , cert.GetInfo(CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME)
        lstFoundCerts.ListItems(i).ListSubItems.Add , , cert.ValidFromDate
        lstFoundCerts.ListItems(i).ListSubItems.Add , , cert.ValidToDate
        
    Next cert

Exit Sub

Err_handler:
     MsgBox "Error " & Hex(Err.Number) & ": " & Err.Description
     Exit Sub
End Sub

Private Sub lstFoundCerts_DblClick()
 'display selected certificate
 If Not lstFoundCerts.SelectedItem Is Nothing Then
        lstFoundCerts.SelectedItem.Tag.Display
 End If
End Sub


Private Sub cmdExit_Click()
    'Clean up
    Set st = Nothing
    Set certs = Nothing
    Set cert = Nothing
    Set li = Nothing
    Unload Me
End Sub

