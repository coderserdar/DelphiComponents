VERSION 5.00
Begin VB.Form FrmSignInfo 
   Caption         =   "SignInfo"
   ClientHeight    =   1845
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   6630
   LinkTopic       =   "Form1"
   ScaleHeight     =   1845
   ScaleWidth      =   6630
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton cmdViewTimestamper 
      Caption         =   "&Timestamper"
      Height          =   375
      Left            =   3120
      TabIndex        =   6
      Top             =   1320
      Width           =   1535
   End
   Begin VB.TextBox txtDescriptionURL 
      Height          =   285
      Left            =   1440
      TabIndex        =   5
      Top             =   720
      Width           =   4935
   End
   Begin VB.TextBox txtDescription 
      Height          =   285
      Left            =   1440
      TabIndex        =   4
      Top             =   240
      Width           =   4935
   End
   Begin VB.CommandButton cmdUnloadForm 
      Caption         =   "&Close"
      Height          =   375
      Left            =   4800
      TabIndex        =   3
      Top             =   1320
      Width           =   1535
   End
   Begin VB.CommandButton cmdViewSigner 
      Caption         =   "&Signer"
      Height          =   375
      Left            =   1440
      TabIndex        =   2
      Top             =   1320
      Width           =   1535
   End
   Begin VB.Label lblDescriptionURL 
      Caption         =   "DescriptionURL:"
      Height          =   255
      Left            =   120
      TabIndex        =   1
      Top             =   720
      Width           =   1215
   End
   Begin VB.Label lblDescription 
      Caption         =   "Description:"
      Height          =   255
      Left            =   120
      TabIndex        =   0
      Top             =   240
      Width           =   1095
   End
End
Attribute VB_Name = "FrmSignInfo"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub cmdUnloadForm_Click()
    Unload Me
End Sub

Private Sub cmdViewSigner_Click()
On Error Resume Next
    If Not FrmSignedCode.SignedCode Is Nothing Then
        If Not FrmSignedCode.SignedCode.Signer Is Nothing Then
            FrmSignedCode.SignedCode.Signer.Certificate.Display
        Else
            MsgBox "The file hasn't been signed."
        End If
    Else
        MsgBox "Please select a file to sign."
    End If
End Sub

Private Sub cmdViewTimestamper_Click()
On Error Resume Next
    If Not FrmSignedCode.SignedCode Is Nothing Then
        If Not FrmSignedCode.SignedCode.TimeStamper Is Nothing Then
            FrmSignedCode.SignedCode.TimeStamper.Certificate.Display
        Else
            MsgBox "The file hasn't been timestamped."
        End If
    Else
           MsgBox "Please select a file to sign."
    End If
End Sub

Private Sub Form_Load()
On Error Resume Next
     txtDescription.Text = FrmSignedCode.SignedCode.Description
     txtDescriptionURL.Text = FrmSignedCode.SignedCode.DescriptionURL
End Sub

