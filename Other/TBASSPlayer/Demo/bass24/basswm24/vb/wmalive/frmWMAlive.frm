VERSION 5.00
Begin VB.Form frmWMAlive 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "BASSWMA live broadcast"
   ClientHeight    =   975
   ClientLeft      =   1905
   ClientTop       =   2820
   ClientWidth     =   5760
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   65
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   384
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer tmrWMAlive 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   2400
      Top             =   240
   End
   Begin VB.TextBox txtCaption 
      Enabled         =   0   'False
      Height          =   285
      Left            =   840
      TabIndex        =   7
      Top             =   600
      Width           =   1575
   End
   Begin VB.PictureBox picLevelBar 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H00C0C0C0&
      ForeColor       =   &H80000008&
      Height          =   735
      Left            =   5520
      ScaleHeight     =   47
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   5
      TabIndex        =   6
      Top             =   120
      Width           =   105
   End
   Begin VB.ComboBox cmbBitrate 
      Height          =   315
      Left            =   2880
      Style           =   2  'Dropdown List
      TabIndex        =   4
      Top             =   120
      Width           =   1095
   End
   Begin VB.TextBox txtTitle 
      Height          =   285
      Left            =   600
      TabIndex        =   1
      Top             =   120
      Width           =   1575
   End
   Begin VB.CommandButton btnStart 
      Caption         =   "Start"
      Height          =   375
      Left            =   4200
      TabIndex        =   0
      Top             =   120
      Width           =   1215
   End
   Begin VB.Label lblCaption 
      AutoSize        =   -1  'True
      Caption         =   "Caption:"
      Height          =   195
      Left            =   120
      TabIndex        =   8
      Top             =   645
      Width           =   585
   End
   Begin VB.Label lblOffAir 
      Alignment       =   2  'Center
      BorderStyle     =   1  'Fixed Single
      Caption         =   "Off Air"
      Height          =   285
      Left            =   2640
      TabIndex        =   5
      Top             =   600
      Width           =   2805
   End
   Begin VB.Label lblEncBitrate 
      AutoSize        =   -1  'True
      Caption         =   "Bitrate:"
      Height          =   195
      Left            =   2280
      TabIndex        =   3
      Top             =   120
      Width           =   495
   End
   Begin VB.Label lblTitle 
      AutoSize        =   -1  'True
      Caption         =   "Title:"
      Height          =   195
      Left            =   120
      TabIndex        =   2
      Top             =   120
      Width           =   345
   End
End
Attribute VB_Name = "frmWMAlive"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'////////////////////////////////////////////////////////////////////////////////
' frmWMAlive.frm - Copyright (c) 2002-2005 (: JOBnik! :) [Arthur Aminov, ISRAEL]
'                                                        [http://www.jobnik.org]
'                                                        [  jobnik@jobnik.org  ]
' Other source: modWMAlive.bas
'
' BASSWMA live broadcast example
' Originally translated from - wmalive.c - Example of Ian Luck
'////////////////////////////////////////////////////////////////////////////////

Option Explicit

Private Sub Form_Load()
    'change and set the current path
    'so it won't ever tell you that bass.dll & basswma.dll are not found
    ChDrive App.Path
    ChDir App.Path

    'check the correct BASS was loaded
    If (HiWord(BASS_GetVersion) <> BASSVERSION) Then
        Call MsgBox("An incorrect version of BASS.DLL was loaded", vbCritical)
        End
    End If

    'setup recording (using default device)
    If (BASS_RecordInit(-1) = 0) Then
        Call Error_("Can't initialize device")
        Unload Me
    Else
        'get the available bitrates, from a memory location
        Dim ratesPTR As Long    'a pointer to a memory location where rates array is stored
        ratesPTR = BASS_WMA_EncodeGetRates(SAMPLERATE, CHANNELS, 0)
         
        If (ratesPTR = 0) Then
            Call Error_("Can't find a codec")
            Unload Me
            End
        Else
            Dim buf(1) As Long
            Do
                Call CopyMemory(buf(0), ByVal ratesPTR, LenB(ratesPTR))
                If (buf(0)) Then
                    cmbBitrate.AddItem buf(0)
                    ratesPTR = ratesPTR + LenB(ratesPTR)
                End If
            Loop While (buf(0))
            
            Call UpdateDisplay
        End If
    End If

    lastip = "none"
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Call BASS_RecordFree
    Call BASS_WMA_EncodeClose(handle)  ' incase it was encoding on exit
End Sub

Private Sub btnStart_Click()
    If (BASS_ChannelIsActive(rchan) = 0) Then
        Call Start
    Else
        Call Stop_
    End If
End Sub

Private Sub tmrWMAlive_Timer()
    If (BASS_ChannelIsActive(rchan) = 0) Then
        Call Stop_
    Else
        Call UpdateDisplay
    End If
End Sub

'update "caption" tag
Private Sub txtCaption_Change()
    Call BASS_WMA_EncodeSetTag(handle, "Caption", txtCaption.text, BASS_WMA_TAG_ANSI)
End Sub

'-------------------------
' some useful function :)
'-------------------------

'check if any file exists
Public Function FileExists(ByVal FileName As String) As Boolean
  FileExists = (Dir$(FileName) <> "")
End Function

' RPP = Return Proper Path
Function RPP(ByVal fp As String) As String
    RPP = IIf(Mid(fp, Len(fp), 1) <> "\", fp & "\", fp)
End Function
