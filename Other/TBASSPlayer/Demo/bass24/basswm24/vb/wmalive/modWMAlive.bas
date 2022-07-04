Attribute VB_Name = "modWMAlive"
'////////////////////////////////////////////////////////////////////////////////
' modWMAlive.BAS - Copyright (c) 2002-2005 (: JOBnik! :) [Arthur Aminov, ISRAEL]
'                                                        [http://www.jobnik.org]
'                                                        [  jobnik@jobnik.org  ]
' Other source: modWMAlive.bas
'
' BASSWMA live broadcast example
' Originally translated from - wmalive.c - Example of Ian Luck
'////////////////////////////////////////////////////////////////////////////////

Option Explicit

Public Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (Destination As Any, Source As Any, ByVal length As Long)

Public Const SAMPLERATE = 44100
Public Const CHANNELS = 2

Public rchan As Long            ' recording channel
Public handle As Long           ' encoder handle
Public time As Single           ' elapsed time
Public level As Long            ' input level
Public lastip As String         ' last client to connect
Public displaycount As Long

' display error messages
Public Sub Error_(ByVal es As String)
    Call MsgBox(es & vbCrLf & "(error code: " & BASS_ErrorGetCode() & ")", vbExclamation, "Error")
End Sub

' update the status and level display
Public Sub UpdateDisplay()

    Dim text As String
    text = "Off Air"
    
    If (BASS_ChannelIsActive(rchan)) Then
        Dim l As Long
        l = BASS_ChannelGetLevel(rchan) ' get current level
        level = IIf(level > 1500, level - 1500, 0)
        If (LoWord(l) > level) Then level = LoWord(l)
        If (HiWord(l) > level) Then level = HiWord(l)
        If (displaycount And 128) Then
            If (displaycount And 64) Then   ' display last client
                text = "last: " & lastip
            Else    ' display client count
                text = "current clients: " & BASS_WMA_EncodeGetClients(handle)
            End If
        Else    ' display "on air"
            Dim t As Long
            t = CLng(time)
            text = "On Air - port: " & BASS_WMA_EncodeGetPort(handle) & " - " & Int(t / 60) & ":" & Int(t Mod 60)
        End If
        displaycount = displaycount + 1
    Else
        level = 0
    End If
    
    With frmWMAlive
        'draw the level bar
        .picLevelBar.Cls
        .picLevelBar.Line (.picLevelBar.Width, .picLevelBar.Height)-(0, .picLevelBar.Height * (32768 - level) / 32768), vbWhite, BF:

        'update status text
        .lblOffAir.Caption = text
    End With
End Sub

' recording callback
Public Function RecordingCallback(ByVal chan As Long, ByVal buffer As Long, ByVal length As Long, ByVal user As Long) As Long
    time = time + (length / CSng((SAMPLERATE * 4))) ' increase elapsed time counter
    'encode the sample data, and continue recording if successful
    RecordingCallback = BASS_WMA_EncodeWrite(handle, ByVal buffer, length)
End Function

' client connection notification callback
Public Sub ClientConnect(ByVal handle As Long, ByVal connect As Long, ByVal ip As Long, ByVal user As Long)
    If (connect) Then lastip = VBStrFromAnsiPtr(ip)     'keep the client's ip for display
End Sub

' start recording & encoding
Public Sub Start()
    With frmWMAlive
        'get bitrate
        Dim bitrate As Long
        bitrate = Val(.cmbBitrate.List(.cmbBitrate.ListIndex))
        'initialize encoder - let system choose port, max 5 clients
        handle = BASS_WMA_EncodeOpenNetwork(SAMPLERATE, CHANNELS, BASS_WMA_ENCODE_SCRIPT, bitrate, 0, 5)
        If (handle = 0) Then
            Call Error_("Can't initialize encoding")
            Exit Sub
        End If

        'get title
        Dim title As String
        title = Trim(.txtTitle.text)
        Call BASS_WMA_EncodeSetTag(handle, "Title", title, BASS_WMA_TAG_ANSI) ' set WMA title tag
        Call BASS_WMA_EncodeSetNotify(handle, AddressOf ClientConnect, 0)   ' setup client notification
        time = 0
        displaycount = 0
        'start recording
        rchan = BASS_RecordStart(SAMPLERATE, CHANNELS, 0, AddressOf RecordingCallback, 0)
        If (rchan = 0) Then
            Call Error_("Can't start recording")
            Call BASS_WMA_EncodeClose(handle)
            Exit Sub
        End If

        .btnStart.Caption = "Stop"
        .txtTitle.Enabled = False
        .cmbBitrate.Enabled = False
        .txtCaption.Enabled = True
        frmWMAlive.tmrWMAlive.Enabled = True    'timer to update the display
    End With
End Sub

'stop recording & encoding
Public Sub Stop_()
    With frmWMAlive
        .tmrWMAlive.Enabled = False
        Call BASS_ChannelStop(rchan)  ' stop recording
        Call BASS_WMA_EncodeClose(handle)  ' stop encoding
        .btnStart.Caption = "Start"
        .txtTitle.Enabled = True
        .cmbBitrate.Enabled = True
        .txtCaption.Enabled = False
        Call UpdateDisplay
    End With
End Sub
