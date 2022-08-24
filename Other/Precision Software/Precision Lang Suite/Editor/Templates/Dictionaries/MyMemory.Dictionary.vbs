' MyMemory translation service (http://mymemory.translated.net/)

forReading = 1
PLSApp.Event.Fails = True

' Read settings from MyMemory.Settings.cfg file
dim objFSO, srcFile, sLine, sKey, sP, url
sLine = PLSApp.ScriptFileName
sP = InStrRev(sLine,".")
If sP>0 Then
  sP = InStrRev(sLine,".",sP-1)
End If
If sP>0 Then
  sLine = Left(sLine,sP) & "Settings.cfg"
End If

set objFSO = createobject("Scripting.FileSystemObject")
set srcFile = objFSO.openTextFile(sLine, forReading)
do until srcFile.atEndOfStream
  sLine = srcFile.readLine
  sP = InStr(sLine,"=")
  If (sP>0) Then
    sKey=Left(sLine,sP-1)
    If sKey="url" Then
      url=Mid(sLine,sP+1)
      Exit Do
    End If
  End If
loop
srcFile.close
set srcFile = nothing
set objFSO = nothing

' Setup query URL
url=Replace(url,"$SRCLNG$",PLSApp.Event.SrcLng)
url=Replace(url,"$TGTLNG$",PLSApp.Event.TgtLng)
url=Replace(url,"$TEXT$",PLSApp.UrlEncode(PLSApp.Event.Text))
PLSApp.Event.Returns = ""

' Get translation result
dim xmlhttp : set xmlhttp=createObject("WinHttp.WinHttpRequest.5.1")
xmlhttp.open "GET", url, False
xmlhttp.send()

' Parse translation result
dim xmldoc : set xmldoc=createObject("Microsoft.XMLDOM")
xmldoc.async = False
xmldoc.validateOnParse = False
xmldoc.resolveExternals = False
xmldoc.loadXML(xmlhttp.responseText)
xmldoc.setProperty "SelectionLanguage", "XPath"

dim s : set s = xmldoc.selectSingleNode("(//seg)[2]")
If (not (s is nothing)) and (not IsNull(s)) Then
  PLSApp.Event.Returns = s.text
End If

' Set error flag to False
PLSApp.Event.Fails = False

set xmldoc = nothing
set xmlhttp = nothing
