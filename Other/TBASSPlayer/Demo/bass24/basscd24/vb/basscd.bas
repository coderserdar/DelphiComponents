Attribute VB_Name = "BASSCD"
' BASSCD 2.4 Visual Basic module
' Copyright (c) 2003-2010 Un4seen Developments Ltd.
'
' See the BASSCD.CHM file for more detailed documentation

' additional error codes returned by BASS_ErrorGetCode
Global Const BASS_ERROR_NOCD = 12      ' no CD in drive
Global Const BASS_ERROR_CDTRACK = 13   ' invalid track number
Global Const BASS_ERROR_NOTAUDIO = 17  ' not an audio track

' additional BASS_SetConfig options
Global Const BASS_CONFIG_CD_FREEOLD = &H10200
Global Const BASS_CONFIG_CD_RETRY = &H10201
Global Const BASS_CONFIG_CD_AUTOSPEED = &H10202
Global Const BASS_CONFIG_CD_SKIPERROR = &H10203

' BASS_CD_SetInterface options
Global Const BASS_CD_IF_AUTO = 0
Global Const BASS_CD_IF_SPTI = 1
Global Const BASS_CD_IF_ASPI = 2
Global Const BASS_CD_IF_WIO = 3

Type BASS_CD_INFO
    vendor As Long        'manufacturer
    product As Long       'model
    rev As Long           'revision
    letter As Long        'drive letter
    rwflags As Long       'read/write capability flags
    canopen As Long       'BASS_CD_DOOR_OPEN/CLOSE is supported?
    canlock As Long       'BASS_CD_DOOR_LOCK/UNLOCK is supported?
    maxspeed As Long      'max read speed (KB/s)
    cache As Long         'cache size (KB)
    cdtext As Long        'can read CD-TEXT
End Type

' "rwflag" read capability flags
Global Const BASS_CD_RWFLAG_READCDR = 1
Global Const BASS_CD_RWFLAG_READCDRW = 2
Global Const BASS_CD_RWFLAG_READCDRW2 = 4
Global Const BASS_CD_RWFLAG_READDVD = 8
Global Const BASS_CD_RWFLAG_READDVDR = 16
Global Const BASS_CD_RWFLAG_READDVDRAM = 32
Global Const BASS_CD_RWFLAG_READANALOG = &H10000
Global Const BASS_CD_RWFLAG_READM2F1 = &H100000
Global Const BASS_CD_RWFLAG_READM2F2 = &H200000
Global Const BASS_CD_RWFLAG_READMULTI = &H400000
Global Const BASS_CD_RWFLAG_READCDDA = &H1000000
Global Const BASS_CD_RWFLAG_READCDDASIA = &H2000000
Global Const BASS_CD_RWFLAG_READSUBCHAN = &H4000000
Global Const BASS_CD_RWFLAG_READSUBCHANDI = &H8000000
Global Const BASS_CD_RWFLAG_READC2 = &H10000000
Global Const BASS_CD_RWFLAG_READISRC = &H20000000
Global Const BASS_CD_RWFLAG_READUPC = &H40000000

' additional BASS_CD_StreamCreate/File flags
Global Const BASS_CD_SUBCHANNEL = &H200
Global Const BASS_CD_SUBCHANNEL_NOHW = &H400
Global Const BASS_CD_C2ERRORS = &H800

' additional CD sync types
Global Const BASS_SYNC_CD_ERROR = 1000
Global Const BASS_SYNC_CD_SPEED = 1002

' BASS_CD_Door actions
Global Const BASS_CD_DOOR_CLOSE = 0
Global Const BASS_CD_DOOR_OPEN = 1
Global Const BASS_CD_DOOR_LOCK = 2
Global Const BASS_CD_DOOR_UNLOCK = 3

' BASS_CD_GetID flags
Global Const BASS_CDID_UPC = 1
Global Const BASS_CDID_CDDB = 2
Global Const BASS_CDID_CDDB2 = 3
Global Const BASS_CDID_TEXT = 4
Global Const BASS_CDID_CDPLAYER = 5
Global Const BASS_CDID_MUSICBRAINZ = 6
Global Const BASS_CDID_ISRC = &H100 ' + track #

' CDDATAPROC "type" values
Global Const BASS_CD_DATA_SUBCHANNEL = 0
Global Const BASS_CD_DATA_C2 = 1

' BASS_CHANNELINFO type
Global Const BASS_CHANNEL_STREAM_CD = &H10200

Declare Function BASS_CD_SetInterface Lib "basscd.dll" (ByVal iface As Long) As Long

Declare Function BASS_CD_GetInfo Lib "basscd.dll" (ByVal drive As Long, ByRef info As BASS_CD_INFO) As Long
Declare Function BASS_CD_Door Lib "basscd.dll" (ByVal drive As Long, ByVal action As Long) As Long
Declare Function BASS_CD_DoorIsLocked Lib "basscd.dll" (ByVal drive As Long) As Long
Declare Function BASS_CD_DoorIsOpen Lib "basscd.dll" (ByVal drive As Long) As Long
Declare Function BASS_CD_IsReady Lib "basscd.dll" (ByVal drive As Long) As Long
Declare Function BASS_CD_GetTracks Lib "basscd.dll" (ByVal drive As Long) As Long
Declare Function BASS_CD_GetTrackLength Lib "basscd.dll" (ByVal drive As Long, ByVal track As Long) As Long
Declare Function BASS_CD_GetTrackPregap Lib "basscd.dll" (ByVal drive As Long, ByVal track As Long) As Long
Declare Function BASS_CD_GetID Lib "basscd.dll" (ByVal drive As Long, ByVal id As Long) As Long
Declare Function BASS_CD_GetSpeed Lib "basscd.dll" (ByVal drive As Long) As Long
Declare Function BASS_CD_SetSpeed Lib "basscd.dll" (ByVal drive As Long, ByVal speed As Long) As Long
Declare Function BASS_CD_SetOffset Lib "basscd.dll" (ByVal drive As Long, ByVal offset As Long) As Long
Declare Function BASS_CD_Release Lib "basscd.dll" (ByVal drive As Long) As Long

Declare Function BASS_CD_StreamCreate Lib "basscd.dll" (ByVal drive As Long, ByVal track As Long, ByVal flags As Long) As Long
Declare Function BASS_CD_StreamCreateFile Lib "basscd.dll" (ByVal f As String, ByVal flags As Long) As Long
Declare Function BASS_CD_StreamCreateEx Lib "basscd.dll" (ByVal drive As Long, ByVal track As Long, ByVal flags As Long, ByVal proc As Long, ByVal user As Long) As Long
Declare Function BASS_CD_StreamCreateFileEx Lib "basscd.dll" (ByVal f As String, ByVal flags As Long, ByVal proc As Long, ByVal user As Long) As Long
Declare Function BASS_CD_StreamGetTrack Lib "basscd.dll" (ByVal handle As Long) As Long
Declare Function BASS_CD_StreamSetTrack Lib "basscd.dll" (ByVal handle As Long, ByVal track As Long) As Long

Declare Function BASS_CD_Analog_Play Lib "basscd.dll" (ByVal drive As Long, ByVal track As Long, ByVal pos As Long) As Long
Declare Function BASS_CD_Analog_PlayFile Lib "basscd.dll" (ByVal f As String, ByVal pos As Long) As Long
Declare Function BASS_CD_Analog_Stop Lib "basscd.dll" (ByVal drive As Long) As Long
Declare Function BASS_CD_Analog_IsActive Lib "basscd.dll" (ByVal drive As Long) As Long
Declare Function BASS_CD_Analog_GetPosition Lib "basscd.dll" (ByVal drive As Long) As Long

' callback functions
Sub CDDATAPROC(ByVal handle As Long, ByVal pos As Long, ByVal type As Long, ByVal buffer As Long, ByVal length As Long, ByVal user As Long)
    
    'CALLBACK FUNCTION !!!

    ' Sub-channel/C2 reading callback function.
    ' handle : The CD stream handle
    ' pos    : The position of the data
    ' type   : The type of data (BASS_CD_DATA_xxx)
    ' buffer : Buffer containing the data.
    ' length : Number of bytes in the buffer
    ' user   : The 'user' parameter value given when calling BASS_CD_StreamCreate/FileEx
    
End Sub
