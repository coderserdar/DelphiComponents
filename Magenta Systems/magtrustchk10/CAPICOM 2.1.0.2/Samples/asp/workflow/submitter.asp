<% @LANGUAGE=VBScript %>
<%
Option Explicit
Response.Expires = 0
%>

<SCRIPT LANGUAGE=VBScript>
  Const CAPICOM_CURRENT_USER_STORE = 2
  Const CAPICOM_MY_STORE = "My"
  Const CAPICOM_STORE_OPEN_READ_ONLY = 0
  Const CAPICOM_STORE_OPEN_EXISTING_ONLY = 128
  Const CAPICOM_CERTIFICATE_FIND_KEY_USAGE = 12
  Const CAPICOM_DIGITAL_SIGNATURE_KEY_USAGE = 128
  Const CAPICOM_CERT_INFO_SUBJECT_EMAIL_NAME = 2
function CheckForNumerics()

  Dim strDataToSign
  strDataToSign = Empty

  CheckForNumerics = TRUE             'Must be true to sign and submit
  Employee.rawData.value = EMPTY      'Raw data to be signed
  Employee.signedData.value = Empty   'Data after signing
  if Len(Employee.OfficeXP.value) > 0 then
    if Not IsNumeric(Employee.OfficeXP.value) then
      CheckForNumerics = FALSE
      Employee.OfficeXP.value = Empty
    elseif CInt(Employee.OfficeXP.value) > 0 Then
      strDataToSign = strDataToSign & "OfficeXP," & Employee.OfficeXP.value & vbNewLine
    End If
  End If
  
  if Len(Employee.WindowsXP.value) > 0 then
    if Not IsNumeric(Employee.WindowsXP.value) then
      CheckForNumerics = FALSE
      Employee.WindowsXP.value = Empty
    elseif CInt(Employee.WindowsXP.value) > 0 Then
      strDataToSign = strDataToSign & "WindowsXP," & Employee.WindowsXP.value & vbNewLine
    End If
  End If
  
  if Len(Employee.VisualBasicNET.value) > 0 then
    if Not IsNumeric(Employee.VisualBasicNET.value) then
      CheckForNumerics = FALSE
      Employee.VisualBasicNET.value = Empty
    elseif CInt(Employee.VisualBasicNET.value) > 0 Then
      strDataToSign = strDataToSign & "VisualBasic.NET," & Employee.VisualBasicNET.value & vbNewLine
    End If
  End If
  
  if Len(Employee.WindowsNETServer.value) > 0 then
    if Not IsNumeric(Employee.WindowsNETServer.value) then
      CheckForNumerics = FALSE
      Employee.WindowsNETServer.value = Empty
    elseif CInt(Employee.WindowsNETServer.value) > 0 Then
      strDataToSign = strDataToSign & "WindowsNETServer," & Employee.WindowsNETServer.value & vbNewLine
    End If
  End If
  
  if Len(Employee.XBox.value) > 0 then
    if Not IsNumeric(Employee.XBox.value) then
      CheckForNumerics = FALSE
      Employee.XBox.value = Empty
    elseif CInt(Employee.XBox.value) > 0 Then
      strDataToSign = strDataToSign & "XBox," & Employee.XBox.value & vbNewLine
    End If
  End If
  
  if CheckForNumerics = TRUE Then
    Employee.rawData.value = strDataToSign
    Employee.signedData.value = SignTheString(strDataToSign)
    if Len (Employee.signedData.value) = 0 then
		CheckForNumerics = FALSE
	end if
  End if 
  window.event.returnValue = CheckForNumerics

End Function

Function SignTheString(strDataToSign)
on error resume next
  Dim oSignedData
  Dim oStore
  Dim oCerts
  Dim oCert
  Dim oSignerCert
  Dim oSelectedCerts

  Set oStore = CreateObject("CAPICOM.Store")
  Set oCerts = CreateObject("CAPICOM.Certificates")
  Set oStoreCerts = CreateObject("CAPICOM.Certificates")
  Set oSigner = CreateObject("CAPICOM.Signer")

  oStore.Open CAPICOM_CURRENT_USER_STORE, CAPICOM_MY_STORE, CAPICOM_STORE_OPEN_READ_ONLY Or CAPICOM_STORE_OPEN_EXISTING_ONLY

  'Filter out certificates that don't have a key usage of Digital Signature
  'Passing in True for the last param to Find also filters out time invalid and revoked certs
  Set oStoreCerts = oStore.Certificates.Find(CAPICOM_CERTIFICATE_FIND_KEY_USAGE, CAPICOM_DIGITAL_SIGNATURE_KEY_USAGE, True)
  
  'Filter out the certificates that don't have an e-mail address in them
  For each oCert in oStoreCerts
    if Len (oCert.GetInfo ( CAPICOM_CERT_INFO_SUBJECT_EMAIL_NAME )) > 0 Then
        oCerts.Add oCert
    End if
  Next
  
  if oCerts.Count = 0 Then
     msgbox "No Valid signing certificates could be found."
  Else
      if oCerts.Count = 1 Then 
        Set oSignerCert = oCerts(1)
      Else
        set oSelectedCerts = oCerts.Select()
        
        'make sure they didn't hit cancel
        if err.number = 0 then 
			Set oSignerCert = oSelectedCerts (1)
		else
			msgbox "You must select a certificate to continue."
			exit function
		end if
			
	  End if
      oSigner.Certificate = oSignerCert
      Set oSignedData = CreateObject("CAPICOM.SignedData")
      oSignedData.Content = strDataToSign
      SignTheString = oSignedData.Sign( oSigner )
      Set oSignedData = Nothing
      Set oCerts = Nothing
      Set oStore = Nothing
      Set oSigner = Nothing
  End if

End function

</SCRIPT>


<HTML>
<HEAD>
<TITLE>CAPICOM 2 Digital Signing Application - Enter Transaction</TITLE>
</HEAD>
<BODY>
<!--#include file="menu.htm" -->
<P><FONT size=5><STRONG>New Transaction</STRONG>   
</FONT></P>
<HR>
<P><STRONG>Enter a quantity for the items to order.</STRONG><BR>
<P><STRONG>   Click Submit   to digitally sign and forward your order to your manager for approval.</STRONG></P><STRONG>

<FORM NAME="Employee" ACTION="submitter_srv.asp" AUTOCOMPLETE = "off" METHOD="POST">

<P><INPUT id=OfficeXP 
name=OfficeXP style="WIDTH: 81px; HEIGHT: 22px" size=11> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Office XP</P>
<P><INPUT id=WindowsXP 
name=WindowsXP style="LEFT: 22px; WIDTH: 81px; TOP: 235px; HEIGHT: 22px" size=11>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Windows XP</P>
<P><INPUT id=VisualBasicNET 
name=VisualBasicNET style="WIDTH: 82px; HEIGHT: 22px" size=11>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; VisualBasic.NET</P>
<P><INPUT id=WindowsNETServer 
name=WindowsNETServer style="WIDTH: 81px; HEIGHT: 22px" size=11> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Windows.NET Server</P>
<P><INPUT id=XBox 
name=XBox style="WIDTH: 81px; HEIGHT: 22px" size=11> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;X-Box</P>
<HR></STRONG>
<P>
<INPUT TYPE="Hidden" NAME="rawData">
<INPUT TYPE="Hidden" NAME="signedData">
<INPUT id=submit1 style="LEFT:  150px; TOP: 290px" type=submit value=Submit name=submit onClick=CheckForNumerics>
<INPUT id=reset1  style="LEFT: 500px; TOP: 0px" type=reset value=Reset name=reset1></P></FORM>

</BODY>
</HTML>
