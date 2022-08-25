<%@Language=VBScript %>
<!--#include file="common.asp" -->
<%
Response.Expires = 0

Dim strFilename
Dim strSignedData
Dim strOriginalContent
Dim bFlag

Function Verify ( strSignedData )
'  On Error Resume Next
  Dim oSignedData
  Dim emailAddress
  Dim cosigneremailAddress
  Dim authorizedApprover
  Dim retval

  retval = False
  Err.Clear
  Set oSignedData = CreateObject( "CAPICOM.SignedData" )
  if VerifyTheSignedString( strSignedData, oSignedData ) Then 'And oSignedData.Signers.Count = 2 Then
    
    cosigneremailAddress = oSignedData.Signers(1).Certificate.GetInfo( CAPICOM_INFO_SUBJECT_EMAIL_NAME ) 
    Response.Write "<p><b>Signed By</B>: "
    Response.Write oSignedData.Signers(1).Certificate.GetInfo( CAPICOM_INFO_SUBJECT_SIMPLE_NAME )
    Response.Write " (" & cosigneremailAddress & ")"

    emailAddress = oSignedData.Signers(2).Certificate.GetInfo( CAPICOM_INFO_SUBJECT_EMAIL_NAME )
    Response.Write "<p><B>Signed By</b>: "
    Response.Write  oSignedData.Signers(2).Certificate.GetInfo( CAPICOM_INFO_SUBJECT_SIMPLE_NAME )
    Response.Write " (" & emailAddress & ")"
  
    authorizedApprover = LookupMgrinAD( emailAddress ) 
    if strcomp(cosigneremailAddress, authorizedApprover) =0 then
      Response.Write "<h2>" & FormatStrong ( "Transaction approved" ) & "</h2>"
      retval = True
    else
      Response.Write "<h2>" & FormatStrong ( "You are not a valid approver" ) & "</h2>"
      retval = False
    End if
  Else
    Response.Write "<h2>" & FormatStrong ( "There was a problem validating the request." ) & "</h2>"
    Response.Write "<tt>" & FormatStrong ( "Data Length = " & Len(strSignedData) & "<p>") & "</tt>"
    retval = False
  End If
  Set oSignedData = Nothing
  Verify = retval
End Function


%>

<HTML>
<HEAD>
<TITLE>CAPICOM Digital Signing Application - Transaction Approval Screen</TITLE>
</HEAD>
<BODY>
<!--#include file="menu.htm" -->
<P><STRONG><FONT size=5>Transaction Approval Screen</FONT></STRONG></P>
<HR>
<P><STRONG><FONT size=4>Transaction:&nbsp;&nbsp;&nbsp;
<%
dim intTransactionID
dim signedData

intTransactionID = Request.Form("transactionID")
Response.Write intTransactionID

response.write "</FONT></STRONG></P>"

signedData = Request.Form("signedData")
'Response.Write ">>" & signedData & "<<"

If IsTransactionApproved ( intTransactionID ) Then
  PrintErrorMessage "This transaction has already been approved."
Else  
  If Verify ( signedData ) Then
    ApproveTransaction intTransactionID 
  End If
End If
  
%>

</BODY>
</HTML>
