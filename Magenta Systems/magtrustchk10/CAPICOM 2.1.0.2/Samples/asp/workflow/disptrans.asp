<%@Language=VBScript %>
<!--#include file="common.asp" -->

<SCRIPT LANGUAGE="VBscript">

  Const CAPICOM_CURRENT_USER_STORE = 2
  Const CAPICOM_MY_STORE = "My"
  Const CAPICOM_STORE_OPEN_READ_ONLY = 0
  Const CAPICOM_STORE_OPEN_EXISTING_ONLY = 128
  Const CAPICOM_CERTIFICATE_FIND_KEY_USAGE = 12
  Const CAPICOM_DIGITAL_SIGNATURE_KEY_USAGE = 128
  Const CAPICOM_CERT_INFO_SUBJECT_EMAIL_NAME = 2

Function ApproveIt
on error resume next
  Dim oSignedData
  Dim oStore
  Dim oCerts
  Dim oCert
  Dim oStoreCerts
  Dim oSignerCert
  Dim strDataToSign
  dim oSelectedCerts

  Set oStore = CreateObject("CAPICOM.Store")
  Set oStoreCerts = CreateObject("CAPICOM.Certificates")
  Set oCerts = CreateObject("CAPICOM.Certificates")
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
			window.event.returnValue = False
			exit function
		end if
	  End if
      oSigner.Certificate = oSignerCert
  
      strDataToSign = Transaction.rawData.value 
    
      Set oSignedData = CreateObject("CAPICOM.SignedData")
      oSignedData.Verify strDataToSign
      if err.number <> 0 then
        set oSignedData=nothing
        ApproveIt = False
        exit function
      end if
      
      Transaction.signedData.value = oSignedData.CoSign( oSigner )
      if err.number <> 0 then
        set oSignedData=nothing
        submit1.event.returnValue=False 
        ApproveIt = False
        exit function
      end if
      ApproveIt = True
  End If
  Set oSignedData = Nothing
  Set oStore = Nothing
  Set oCerts = Nothing
  Set oSigner = Nothing

End function
</SCRIPT>

<%

Sub ParseOriginalContent (strOriginalContent )
  
  Dim arrLineArray
  Dim arrFieldArray
  Dim strDetail
  Dim i
  arrLineArray = Split(strOriginalContent, vbNewLine, -1, 1)
  for i = 0 to uBound(arrLineArray) -1
    arrFieldArray = Split(arrLineArray(i), ",", -1, 1)
    strDetail = "<tt>"
    if CInt(arrFieldArray(1)) < 10 then
      strDetail = strDetail & "&nbsp;&nbsp;"
    end if
    strDetail = strDetail & CInt(arrFieldArray(1)) & "&nbsp;&nbsp;&nbsp;&nbsp;" & arrFieldArray(0)
    strDetail = strDetail & "</tt><BR>"
    Response.Write strDetail
  next
end sub

Sub DisplayTrans
  on error resume next
  Dim intTransactionID
  Dim signedContent
  Dim strOriginalContent 
  Dim oSignedData
  Dim approvalTime

  intTransactionID = Request.QueryString("transactionId")
  
  signedContent = GetTransData( intTransactionID )
  
  'First verify the received signed data string
  Set oSignedData = CreateObject("CAPICOM.SignedData")
  oSignedData.Verify signedContent
  if Err.Number <> 0 then
    Response.Write err.number
    Response.Write err.Description
    exit sub
  end if

  Response.Write "<h3>Transaction " & intTransactionID  & "</h3>"
  Response.Write "<table border=1><tr><td><b> Submitter Info</b></td><td><b>Detail</b></td></tr>"
  Response.Write "<tr><td>Name</td><td>" & oSignedData.Signers(1).Certificate.GetInfo(CAPICOM_INFO_SUBJECT_SIMPLE_NAME ) & "</td></tr>"
  Response.Write "<tr><td>E-mail alias</td><td>" & oSignedData.Signers(1).Certificate.GetInfo(CAPICOM_INFO_SUBJECT_EMAIL_NAME ) & "</td></tr>"
  Response.Write "<tr><td>Expires</td><td>" & oSignedData.Signers(1).Certificate.ValidToDate & "</td></tr>"
  Response.Write "<tr><td>Transaction details</td><td>"
  ParseOriginalContent oSignedData.Content

  approvalTime = GetTransactionApprovalTime ( intTransactionID )
  if Len ( approvalTime ) > 0 Then
    Response.Write "<tr><td>" & FormatStrong ( "Approved" ) & "</td><td>" & FormatStrong ( approvalTime ) & "</td></tr>"
    Response.Write "</td></tr></table>"
  Else 
    Response.Write "</td></tr></table>"
    Response.Write "<h3>Click the button to approve the transaction</h3>"
    Response.Write "<FORM NAME=""Transaction"" ACTION=""approver_srv.asp"" METHOD=""post"" >"

    Response.Write "<INPUT TYPE=""Hidden"" NAME=""transactionID"" VALUE=""" & intTransactionID &""" >"
    Response.Write "<INPUT TYPE=""Hidden"" NAME=""signedData"" >"
    Response.Write "<INPUT TYPE=""Hidden"" NAME=""rawdata"" value=""" & signedContent & """ >"

    Response.Write "<P><INPUT id=submit1 style='LEFT: 10px; TOP: 213px' type=submit value=Approve name=submit1 onClick=""ApproveIt""></P>"
    Response.Write " </FORM>"
  End If
  Set oSignedData = Nothing

End Sub

%>
<HTML>
<HEAD>
<TITLE>CAPICOM Digital Signing Application - Display Transaction Details</TITLE>
</HEAD>
<BODY>
<!--#include file="menu.htm" -->
<P><STRONG><FONT size=5>Display Transaction Details</FONT></STRONG></P><STRONG><FONT size=5>
<HR>
</FONT></STRONG>

<%
  DisplayTrans
%>

</BODY>
</HTML>
