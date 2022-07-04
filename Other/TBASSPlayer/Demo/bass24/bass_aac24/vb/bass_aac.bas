Attribute VB_Name = "bass_aac"
Option Explicit

' Additional BASS_SetConfig options
Global Const BASS_CONFIG_MP4_VIDEO = &H10700 ' play the audio from MP4 videos

' Additional tags available from BASS_StreamGetTags
Global Const BASS_TAG_MP4 = 7       ' MP4/iTunes metadata

Global Const BASS_AAC_DOWNMATRIX = &H400000  ' downmatrix to stereo

' BASS_CHANNELINFO type
Global Const BASS_CTYPE_STREAM_AAC = &H10B00
Global Const BASS_CTYPE_STREAM_MP4 = &H10B01

Private Declare Function BASS_AAC_StreamCreateFile64 Lib "bass_aac.dll" Alias "BASS_AAC_StreamCreateFile" (ByVal mem As Long, ByVal file As Any, ByVal offset As Long, ByVal offsethi As Long, ByVal length As Long, ByVal lengthhi As Long, ByVal flags As Long) As Long
Declare Function BASS_AAC_StreamCreateURL Lib "bass_aac.dll" (ByVal url As String, ByVal offset As Long, ByVal flags As Long, ByVal proc As Long, ByVal user As Long) As Long
Declare Function BASS_AAC_StreamCreateFileUser Lib "bass_aac.dll" (ByVal system As Long, ByVal flags As Long, ByVal procs As Long, ByVal user As Long) As Long
Private Declare Function BASS_MP4_StreamCreateFile64 Lib "bass_aac.dll" Alias "BASS_MP4_StreamCreateFile" (ByVal mem As Long, ByVal file As Any, ByVal offset As Long, ByVal offsethi As Long, ByVal length As Long, ByVal lengthhi As Long, ByVal flags As Long) As Long
Declare Function BASS_MP4_StreamCreateFileUser Lib "bass_aac.dll" (ByVal system As Long, ByVal flags As Long, ByVal procs As Long, ByVal user As Long) As Long

' 32-bit wrappers for 64-bit BASS functions
Function BASS_AAC_StreamCreateFile(ByVal mem As Long, ByVal file As Long, ByVal offset As Long, ByVal length As Long, ByVal flags As Long) As Long
BASS_AAC_StreamCreateFile = BASS_AAC_StreamCreateFile64(mem, file, offset, 0, length, 0, flags Or BASS_UNICODE)
End Function

Function BASS_MP4_StreamCreateFile(ByVal mem As Long, ByVal file As Long, ByVal offset As Long, ByVal length As Long, ByVal flags As Long) As Long
BASS_MP4_StreamCreateFile = BASS_MP4_StreamCreateFile64(mem, file, offset, 0, length, 0, flags Or BASS_UNICODE)
End Function
