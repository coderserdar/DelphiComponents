Attribute VB_Name = "BASSWMA"
' BASSWMA 2.4 Visual Basic module
' Copyright (c) 2002-2010 Un4seen Developments Ltd.
'
' See the BASSWMA.CHM file for more detailed documentation

' Additional error codes returned by BASS_ErrorGetCode
Global Const BASS_ERROR_WMA_LICENSE = 1000     ' the file is protected
Global Const BASS_ERROR_WMA = 1001             ' Windows Media (9 or above) is not installed
Global Const BASS_ERROR_WMA_WM9 = BASS_ERROR_WMA
Global Const BASS_ERROR_WMA_DENIED = 1002      ' access denied (user/pass is invalid)
Global Const BASS_ERROR_WMA_INDIVIDUAL = 1004  ' individualization is needed
Global Const BASS_ERROR_WMA_PUBINIT = 1005     ' publishing point initialization problem

' Additional BASS_SetConfig options
Global Const BASS_CONFIG_WMA_PRECHECK = &H10100
Global Const BASS_CONFIG_WMA_PREBUF = &H10101
Global Const BASS_CONFIG_WMA_BASSFILE = &H10103
Global Const BASS_CONFIG_WMA_VIDEO = &H10105
Global Const BASS_CONFIG_WMA_NETSEEK = &H10104

' additional WMA sync types
Global Const BASS_SYNC_WMA_CHANGE = &H10100
Global Const BASS_SYNC_WMA_META = &H10101

' additional BASS_StreamGetFilePosition WMA mode
Global Const BASS_FILEPOS_WMA_BUFFER = 1000 ' internet buffering progress (0-100%)

' Additional flags for use with BASS_WMA_EncodeOpenFile/Network/Publish
Global Const BASS_WMA_ENCODE_STANDARD = &H2000 ' standard WMA
Global Const BASS_WMA_ENCODE_PRO = &H4000      ' WMA Pro
Global Const BASS_WMA_ENCODE_24BIT = 32768     ' 24-bit
Global Const BASS_WMA_ENCODE_PCM = &H10000     ' uncompressed PCM
Global Const BASS_WMA_ENCODE_SCRIPT = &H20000  ' set script (mid-stream tags) in the WMA encoding

' Additional flag for use with BASS_WMA_EncodeGetRates
Global Const BASS_WMA_ENCODE_RATES_VBR = &H10000 ' get available VBR quality settings

' WMENCODEPROC "type" values
Global Const BASS_WMA_ENCODE_HEAD = 0
Global Const BASS_WMA_ENCODE_DATA = 1
Global Const BASS_WMA_ENCODE_DONE = 2

' BASS_WMA_EncodeSetTag "form" values
Global Const BASS_WMA_TAG_ANSI = 0
Global Const BASS_WMA_TAG_UNICODE = 1
Global Const BASS_WMA_TAG_UTF8 = 2

' BASS_CHANNELINFO type
Global Const BASS_CTYPE_STREAM_WMA = &H10300
Global Const BASS_CTYPE_STREAM_WMA_MP3 = &H10301

' Additional BASS_ChannelGetTags types
Global Const BASS_TAG_WMA = 8 ' WMA header tags : series of null-terminated UTF-8 strings
Global Const BASS_TAG_WMA_META = 11 ' WMA mid-stream tag : UTF-8 string
Global Const BASS_TAG_WMA_CODEC = 12 ' WMA codec

Declare Function BASS_WMA_StreamCreateFile64 Lib "basswma.dll" Alias "BASS_WMA_StreamCreateFile" (ByVal mem As Long, ByVal file As Any, ByVal offset As Long, ByVal offsethigh As Long, ByVal length As Long, ByVal lengthhigh As Long, ByVal flags As Long) As Long
Declare Function BASS_WMA_StreamCreateFileAuth64 Lib "basswma.dll" Alias "BASS_WMA_StreamCreateFileAuth" (ByVal mem As Long, ByVal file As Any, ByVal offset As Long, ByVal offsethigh As Long, ByVal length As Long, ByVal lengthhigh As Long, ByVal flags As Long, ByVal user As String, ByVal pass As String) As Long
Declare Function BASS_WMA_StreamCreateFileUser Lib "basswma.dll" (ByVal system As Long, ByVal flags As Long, ByVal procs As Long, ByVal user As Long) As Long

Declare Function BASS_WMA_GetTags Lib "basswma.dll" (ByVal file As String, ByVal flags As Long) As Long

Declare Function BASS_WMA_EncodeGetRates Lib "basswma.dll" (ByVal freq As Long, ByVal chans As Long, ByVal flags As Long) As Long
Declare Function BASS_WMA_EncodeOpen Lib "basswma.dll" (ByVal freq As Long, ByVal chans As Long, ByVal flags As Long, ByVal bitrate As Long, ByVal proc As Long, ByVal user As Long) As Long
Declare Function BASS_WMA_EncodeOpenFile Lib "basswma.dll" (ByVal freq As Long, ByVal chans As Long, ByVal flags As Long, ByVal bitrate As Long, ByVal file As String) As Long
Declare Function BASS_WMA_EncodeOpenNetwork Lib "basswma.dll" (ByVal freq As Long, ByVal chans As Long, ByVal flags As Long, ByVal bitrate As Long, ByVal port As Long, ByVal clients As Long) As Long
Declare Function BASS_WMA_EncodeOpenNetworkMulti Lib "basswma.dll" (ByVal freq As Long, ByVal chans As Long, ByVal flags As Long, ByRef bitrates As Long, ByVal port As Long, ByVal clients As Long) As Long
Declare Function BASS_WMA_EncodeOpenPublish Lib "basswma.dll" (ByVal freq As Long, ByVal chans As Long, ByVal flags As Long, ByVal bitrate As Long, ByVal url As String, ByVal user As String, ByVal pass As String) As Long
Declare Function BASS_WMA_EncodeOpenPublishMulti Lib "basswma.dll" (ByVal freq As Long, ByVal chans As Long, ByVal flags As Long, ByRef bitrates As Long, ByVal url As String, ByVal user As String, ByVal pass As String) As Long
Declare Function BASS_WMA_EncodeGetPort Lib "basswma.dll" (ByVal handle As Long) As Long
Declare Function BASS_WMA_EncodeSetNotify Lib "basswma.dll" (ByVal handle As Long, ByVal proc As Long, ByVal user As Long) As Long
Declare Function BASS_WMA_EncodeGetClients Lib "basswma.dll" (ByVal handle As Long) As Long
Declare Function BASS_WMA_EncodeSetTag Lib "basswma.dll" (ByVal handle As Long, ByVal tag As String, ByVal text As String, ByVal form As Long) As Long
Declare Function BASS_WMA_EncodeWrite Lib "basswma.dll" (ByVal handle As Long, ByVal buffer As Long, ByVal length As Long) As Long
Declare Function BASS_WMA_EncodeClose Lib "basswma.dll" (ByVal handle As Long) As Long

Declare Function BASS_WMA_GetWMObject Lib "basswma.dll" (ByVal handle As Long) As Long

' 32-bit wrappers for 64-bit BASS functions
Function BASS_WMA_StreamCreateFile(ByVal mem As Long, ByVal file As Long, ByVal offset As Long, ByVal length As Long, ByVal flags As Long) As Long
BASS_WMA_StreamCreateFile = BASS_WMA_StreamCreateFile64(mem, file, offset, 0, length, 0, flags Or BASS_UNICODE)
End Function

Function BASS_WMA_StreamCreateFileAuth(ByVal mem As Long, file As Long, ByVal offset As Long, ByVal length As Long, ByVal flags As Long, ByVal user As String, ByVal pass As String) As Long
BASS_WMA_StreamCreateFileAuth = BASS_WMA_StreamCreateFileAuth64(mem, file, offset, 0, length, 0, flags Or BASS_UNICODE, StrPtr(user), StrPtr(pass))
End Function

Sub CLIENTCONNECTPROC(ByVal handle As Long, ByVal connect As Long, ByVal ip As Long, ByVal user As Long)

    'CALLBACK FUNCTION !!!

    ' Client connection notification callback function.
    ' handle : The encoder
    ' connect: TRUE=client is connecting, FALSE=disconnecting
    ' ip     : The client's IP (xxx.xxx.xxx.xxx:port)
    ' user   : The 'user' parameter value given when calling BASS_WMA_EncodeSetNotify

End Sub

Sub WMENCODEPROC(ByVal handle As Long, ByVal dtype As Long, ByVal buffer As Long, ByVal length As Long, ByVal user As Long)

    'CALLBACK FUNCTION !!!

    ' Encoder callback function.
    ' handle : The encoder handle
    ' dtype  : The type of data, one of BASS_WMA_ENCODE_xxx values
    ' buffer : The encoded data
    ' length : Length of the data
    ' user   : The 'user' parameter value given when calling BASS_WMA_EncodeOpen

End Sub
