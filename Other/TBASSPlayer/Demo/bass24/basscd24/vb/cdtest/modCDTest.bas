Attribute VB_Name = "modCDTest"
'///////////////////////////////////////////////////////////////////////////////
' modCDTest.bas - Copyright (c) 2003-2005 (: JOBnik! :) [Arthur Aminov, ISRAEL]
'                                                       [http://www.jobnik.org]
'                                                       [  jobnik@jobnik.org  ]
' Other source: frmCDTest.frm
'
' BASSCD Test
' Originally Translated from - cdtest.c - Example of Ian Luck
'///////////////////////////////////////////////////////////////////////////////

Option Explicit

Public Const MAXDRIVES = 10
Public curdrive As Long
Public stream(MAXDRIVES) As Long
Public seeking As Long

' End sync
Public Sub EndSync(ByVal handle As Long, ByVal channel As Long, ByVal data As Long, ByVal user As Long)
    If (frmCDTest.chkAdvance.value) Then  ' advance onto next track
        Dim track As Long, drive As Long, tracks As Long
        track = BASS_CD_StreamGetTrack(channel)
        drive = HiWord(track)
        tracks = BASS_CD_GetTracks(drive)
        If (tracks = -1) Then Exit Sub  'error, eg. CD removed?
        track = (LoWord(track) + 1) Mod tracks
        If (drive = curdrive) Then frmCDTest.lstTracks.ListIndex = track
        Call PlayTrack(drive, track)
    End If
End Sub

Public Sub PlayTrack(ByVal drive As Long, ByVal track As Long)
    If (stream(drive)) Then
        Call BASS_CD_StreamSetTrack(stream(drive), track) ' already have a stream, so just set the track
    Else
        stream(drive) = BASS_CD_StreamCreate(drive, track, 0)  ' create stream
        Call BASS_ChannelSetSync(stream(drive), BASS_SYNC_END, 0, AddressOf EndSync, 0) ' set end sync
    End If
    If (drive = curdrive And stream(drive)) Then
        frmCDTest.sldPos.max = (BASS_ChannelGetLength(stream(drive), BASS_POS_BYTE) / 176400) - 1 ' set pos scroller range
    End If
    Call BASS_ChannelPlay(stream(drive), BASSFALSE) ' start playing
End Sub

Public Sub UpdateTrackList()
    Dim a As Long, tc As Long, l As Long, cdtext As Long, text As String
    Dim vol As Single, spd As Single
    
    tc = BASS_CD_GetTracks(curdrive)
    frmCDTest.lstTracks.Clear
    
    If (tc = -1) Then Exit Sub  'no CD
    
    cdtext = BASS_CD_GetID(curdrive, BASS_CDID_TEXT) 'get CD-TEXT
    
    For a = 0 To tc - 1
        l = BASS_CD_GetTrackLength(curdrive, a)
        text = "Track " & Format(a + 1, "00")
        If (cdtext) Then
            Dim t As Long, tag As String
            t = cdtext
            tag = "TITLE" & a + 1 & "="  'the CD-TEXT tag to look for
            Do While (VBStrFromAnsiPtr(t) <> "")
                If (Mid(VBStrFromAnsiPtr(t), 1, Len(tag)) = tag) Then 'found the track title...
                    text = VBStrFromAnsiPtr(t + Len(tag)) 'replace "track x" with title
                    Exit Do
                End If
                t = t + Len(VBStrFromAnsiPtr(t)) + 1
            Loop
        End If
        If (l = -1) Then
            text = text & " (data)"
        Else
            l = l / 176400
            text = text & " (" & Int(l / 60) & ":" & Format(Int(l Mod 60), "00") & ")"
        End If
        
        frmCDTest.lstTracks.AddItem text
    Next a
    
    a = BASS_CD_StreamGetTrack(stream(curdrive))
    If (a <> -1) Then ' this drive has a stream
        With frmCDTest
            .lstTracks.ListIndex = LoWord(a) ' select current track
            .sldPos.max = (BASS_ChannelGetLength(stream(curdrive), BASS_POS_BYTE) / 176400) - 1 ' set pos scroller range
        End With
    End If

    vol = 1
    spd = 44100
    Call BASS_ChannelGetAttribute(stream(curdrive), BASS_ATTRIB_FREQ, spd)
    Call BASS_ChannelGetAttribute(stream(curdrive), BASS_ATTRIB_VOL, vol)
    With frmCDTest
        .sldVol.value = 100 - vol * 100 ' set volume slider pos
        .sldSpeed.value = spd / 441 ' set speed slider pos
        .frameCDTest(1).Caption = " Speed - " & spd / 441 & "% "
    End With
End Sub
