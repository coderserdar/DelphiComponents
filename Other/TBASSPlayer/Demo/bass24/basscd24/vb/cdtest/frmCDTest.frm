VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "mscomctl.ocx"
Begin VB.Form frmCDTest 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "BASSCD test"
   ClientHeight    =   4260
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   4740
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4260
   ScaleWidth      =   4740
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer tmrCDTest 
      Interval        =   50
      Left            =   2040
      Top             =   1920
   End
   Begin VB.CheckBox chkAdvance 
      Caption         =   "Auto Advance"
      Height          =   255
      Left            =   2640
      TabIndex        =   12
      Top             =   3120
      Width           =   1455
   End
   Begin VB.Frame frameCDTest 
      Caption         =   " Vol "
      Height          =   1215
      Index           =   2
      Left            =   4200
      TabIndex        =   9
      Top             =   2280
      Width           =   495
      Begin MSComctlLib.Slider sldVol 
         Height          =   855
         Left            =   120
         TabIndex        =   11
         Top             =   240
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   1508
         _Version        =   393216
         Orientation     =   1
         Max             =   100
         TickStyle       =   3
         TickFrequency   =   0
      End
   End
   Begin VB.CommandButton btnPlayPause 
      Caption         =   "Play / Pause"
      Height          =   300
      Left            =   2520
      TabIndex        =   8
      Top             =   1940
      Width           =   2175
   End
   Begin VB.Frame frameCDTest 
      Caption         =   " Speed "
      Height          =   615
      Index           =   1
      Left            =   2520
      TabIndex        =   5
      Top             =   2280
      Width           =   1575
      Begin MSComctlLib.Slider sldSpeed 
         Height          =   255
         Left            =   120
         TabIndex        =   10
         Top             =   240
         Width           =   1335
         _ExtentX        =   2355
         _ExtentY        =   450
         _Version        =   393216
         Min             =   50
         Max             =   150
         SelStart        =   50
         TickStyle       =   3
         TickFrequency   =   0
         Value           =   50
      End
   End
   Begin MSComctlLib.ProgressBar pbLevel 
      Height          =   150
      Index           =   0
      Left            =   120
      TabIndex        =   2
      Top             =   555
      Width           =   4575
      _ExtentX        =   8070
      _ExtentY        =   265
      _Version        =   393216
      BorderStyle     =   1
      Appearance      =   0
      Max             =   30000
   End
   Begin VB.ListBox lstTracks 
      Height          =   3180
      Left            =   120
      TabIndex        =   1
      Top             =   960
      Width           =   2295
   End
   Begin VB.ComboBox cmbCDs 
      Height          =   315
      Left            =   120
      Style           =   2  'Dropdown List
      TabIndex        =   0
      Top             =   120
      Width           =   4575
   End
   Begin MSComctlLib.ProgressBar pbLevel 
      Height          =   150
      Index           =   1
      Left            =   120
      TabIndex        =   3
      Top             =   705
      Width           =   4575
      _ExtentX        =   8070
      _ExtentY        =   265
      _Version        =   393216
      BorderStyle     =   1
      Appearance      =   0
      Max             =   30000
   End
   Begin VB.Frame frameCDTest 
      Caption         =   " Position "
      Height          =   1005
      Index           =   0
      Left            =   2520
      TabIndex        =   4
      Top             =   870
      Width           =   2175
      Begin MSComctlLib.Slider sldPos 
         Height          =   255
         Left            =   120
         TabIndex        =   7
         Top             =   600
         Width           =   1935
         _ExtentX        =   3413
         _ExtentY        =   450
         _Version        =   393216
         Max             =   100
         TickFrequency   =   0
      End
      Begin VB.Label lblPosition 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         BackStyle       =   0  'Transparent
         BorderStyle     =   1  'Fixed Single
         Caption         =   "-"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   120
         TabIndex        =   6
         Top             =   240
         Width           =   1935
      End
   End
   Begin VB.Frame frameCDTest 
      Caption         =   " Door "
      Height          =   680
      Index           =   3
      Left            =   2520
      TabIndex        =   13
      Top             =   3480
      Width           =   2175
      Begin VB.CheckBox chkLock 
         Appearance      =   0  'Flat
         Caption         =   "Lock"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   1320
         TabIndex        =   15
         Top             =   280
         Width           =   735
      End
      Begin VB.CheckBox chkOpen 
         Appearance      =   0  'Flat
         Caption         =   "Open"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   120
         TabIndex        =   14
         Top             =   280
         Width           =   855
      End
   End
End
Attribute VB_Name = "frmCDTest"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'///////////////////////////////////////////////////////////////////////////////
' frmCDTest.frm - Copyright (c) 2003-2005 (: JOBnik! :) [Arthur Aminov, ISRAEL]
'                                                       [http://www.jobnik.org]
'                                                       [  jobnik@jobnik.org  ]
' Other source: modCDTest.bas
'
' BASSCD Test
' Originally Translated from - cdtest.c - Example of Ian Luck
'///////////////////////////////////////////////////////////////////////////////

Option Explicit

'display error messages
Sub Error_(ByVal es As String)
    Call MsgBox(es & vbCrLf & vbCrLf & "error code: " & BASS_ErrorGetCode, vbExclamation, "Error")
End Sub

Private Sub Form_Load()
    'change and set the current path
    'so VB won't ever tell you, that "bass.dll" hasn't been found
    ChDrive App.Path
    ChDir App.Path
    
    'check the correct BASS was loaded
    If (HiWord(BASS_GetVersion) <> BASSVERSION) Then
        Call MsgBox("An incorrect version of BASS.DLL was loaded", vbCritical)
        End
    End If
        
    'Get list of available drives
    Dim a As Long, n As Long
    Dim cdi As BASS_CD_INFO
    
    a = 0
    While (a < MAXDRIVES And BASS_CD_GetInfo(a, cdi) <> 0)
        cmbCDs.AddItem Chr$(65 + cdi.letter) & ": " & VBStrFromAnsiPtr(cdi.vendor) & " " & VBStrFromAnsiPtr(cdi.product) & " " & VBStrFromAnsiPtr(cdi.rev)    ' "letter: description"
        a = a + 1
    Wend
    
    If (a = 0) Then
        Call Error_("No CD drives found")
        End
    End If
    
    cmbCDs.ListIndex = 0 ' select 1st drive
    
    ' Setup output - default device
    If (BASS_Init(-1, 44100, 0, Me.hWnd, 0) = 0) Then
        Call Error_("Can't initialize device")
        End
    End If
        
    seeking = -1
End Sub

Private Sub Form_Unload(Cancel As Integer)
    tmrCDTest.Enabled = False
    Call BASS_Free
End Sub

' play/pause
Private Sub btnPlayPause_Click()
    If (BASS_ChannelIsActive(stream(curdrive)) <> BASS_ACTIVE_PAUSED) Then
        Call BASS_ChannelPause(stream(curdrive))
    Else
        Call BASS_ChannelPlay(stream(curdrive), 0)
    End If
End Sub

' lock/unlock door
Private Sub chkLock_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If (Button = vbLeftButton) Then
        Call BASS_CD_Door(curdrive, IIf(BASS_CD_DoorIsLocked(curdrive), BASS_CD_DOOR_UNLOCK, BASS_CD_DOOR_LOCK))
    End If
End Sub

Private Sub chkLock_Click()
    chkLock.value = BASS_CD_DoorIsLocked(curdrive)
End Sub

' open/close door
Private Sub chkOpen_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If (Button = vbLeftButton) Then
        Call BASS_CD_Door(curdrive, IIf(BASS_CD_DoorIsOpen(curdrive), BASS_CD_DOOR_CLOSE, BASS_CD_DOOR_OPEN))
    End If
End Sub

Private Sub chkOpen_Click()
    chkOpen.value = BASS_CD_DoorIsOpen(curdrive)
End Sub

' change current drive
Private Sub cmbCDs_Click()
    curdrive = cmbCDs.ListIndex
    Call UpdateTrackList
End Sub

' change playing track
Private Sub lstTracks_Click()
    Dim track As Long
    track = lstTracks.ListIndex
    Call PlayTrack(curdrive, track)
End Sub

Private Sub sldPos_Change()
    seeking = -1
End Sub

' position scroller
Private Sub sldPos_Click()
    ' seek to new pos
    seeking = sldPos.value
    Call BASS_ChannelSetPosition(stream(curdrive), seeking * 176400, BASS_POS_BYTE)
End Sub

Private Sub sldPos_Scroll()
    seeking = sldPos.value
End Sub

' speed scroller
Private Sub sldSpeed_Scroll()
    Dim spd As Single
    spd = sldSpeed.value
    Call BASS_ChannelSetAttribute(stream(curdrive), BASS_ATTRIB_FREQ, spd * 441)
    frameCDTest(1).Caption = "Speed - " & spd & "% "
End Sub

' adjust volume
Private Sub sldVol_Scroll()
    Dim vol As Single
    vol = sldVol.value
    sldVol.text = 100 - vol
    Call BASS_ChannelSetAttribute(stream(curdrive), BASS_ATTRIB_VOL, (100 - vol) / 100)
End Sub

Private Sub tmrCDTest_Timer()
    On Local Error Resume Next
    ' update levels
    Static updatecount As Long, levl As Long, levr As Long
    Dim level As Long
    
    level = BASS_ChannelGetLevel(stream(curdrive))
    levl = levl - 1500
    If (levl < 0) Then levl = 0
    levr = levr - 1500
    If (levr < 0) Then levr = 0
    If (level <> -1) Then
        If (levl < LoWord(level)) Then levl = LoWord(level)
        If (levr < HiWord(level)) Then levr = HiWord(level)
    End If
    
    pbLevel(0).value = levl ' left
    pbLevel(1).value = levr ' right
    
    updatecount = updatecount + 1
    
    If ((updatecount And 3) = 0) Then 'do other stuff (only every 4th visit)
        Dim time As String
        time = "-"
        Dim isopen As Long, islock As Long
        isopen = BASS_CD_DoorIsOpen(curdrive)
        islock = BASS_CD_DoorIsLocked(curdrive)
        chkOpen.value = isopen
        chkLock.value = islock
        If (BASS_ChannelIsActive(stream(curdrive))) Then ' playing - update info
            Dim p As Long
            p = seeking
            If (p = -1) Then ' not seeking - update pos scroller
                p = BASS_ChannelBytes2Seconds(stream(curdrive), BASS_ChannelGetPosition(stream(curdrive), BASS_POS_BYTE))
                sldPos.value = CInt(p)
            End If
            time = LoWord(BASS_CD_StreamGetTrack(stream(curdrive))) + 1 & " - " & CInt(p / 60) & ":" & Format(CInt(p Mod 60), "00")
        Else
            If (lstTracks.ListCount = 0) Then ' empty track list - refresh
                If (isopen = 0) Then Call UpdateTrackList
            ElseIf (isopen Or BASS_CD_IsReady(curdrive) = 0) Then  ' no CD - free stream & clear list
                Call BASS_StreamFree(stream(curdrive))
                stream(curdrive) = 0
                lstTracks.Clear
            End If
            Call BASS_CD_Release(curdrive) ' release the drive to allow others to access it
        End If
        lblPosition.Caption = time
    End If
End Sub

'--------------------------
' some useful functions :)
'--------------------------

'check if any file exists
Public Function FileExists(ByVal fp As String) As Boolean
    FileExists = (Dir(fp) <> "")
End Function

'RPP = Return Proper Path
Function RPP(ByVal fp As String) As String
    RPP = IIf(Mid(fp, Len(fp), 1) <> "\", fp & "\", fp)
End Function
