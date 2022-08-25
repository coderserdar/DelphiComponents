<%@Language=VBScript %>
<!--#include file="common.asp" -->
<%


Response.Expires = 0


Function ProcessSignedData ( strSignedData, cert )
  Dim oSignedData
  Set oSignedData = CreateObject("CAPICOM.SignedData")

  if VerifyTheSignedString (strSignedData, oSignedData ) = False Then
    ProcessSignedData = False
  Else
    Set cert = oSignedData.Signers(1).Certificate
    ProcessSignedData = True
  end if 
  Set oSignedData = Nothing

End Function
%>



<HTML>
<HEAD>
<TITLE>CAPICOM Digital Signing Application - Transaction Submitted</TITLE>
</HEAD>
<BODY>
<!--#include file="menu.htm" -->

<P><FONT size=5><STRONG>Transaction Submitted</STRONG>   
</FONT></P>
<HR>
<P><P><STRONG>

<FORM NAME="Employee">
<%
  Call Initialize
  Dim strSignedData
  Dim cert
  Dim strMgrEmail
  Dim intNextIdNumber
  strSignedData = Request.Form("signedData")

  if ProcessSignedData ( strSignedData, cert ) Then
    strMgrEmail = LookupMgrinAD( cert.GetInfo( CAPICOM_INFO_SUBJECT_EMAIL_NAME ) )  
    if Len( strMgrEmail ) = 0 then
      PrintErrorMessage "Ensure the account accessing this webserver has access rights to the directory.<p>"  
      if Len(cert.GetInfo( CAPICOM_INFO_SUBJECT_EMAIL_NAME ))=0 Then 
         PrintErrorMessage "The certificate being used does not contain an e-mail component in the distinguished name: <br><tt>" & cert.SubjectName & "</tt>"
      end if
    else  
      intNextIdNumber = AssignNextTransaction 
      SaveSignedDocument intNextIdNumber, strSignedData
      Response.Write "<P><FONT size=5><STRONG>" & cert.GetInfo(CAPICOM_INFO_SUBJECT_SIMPLE_NAME) & _
        ", your digitally signed purchase has been verified at the server and assigned number " & _
        intNextIdNumber & "."
      Response.Write "<p>The request has been routed to <b>" & strMgrEmail  & "</b> for <a href=""approver.asp"">approval</a>."
    end if
  else
    PrintErrorMessage "An error occurred while verifying the message.<br><tt>" & Err.Description & "</tt><p>"
  end if
  Set cert = nothing
%>
</P>
</p>
</p>
</p>
<HR></STRONG></FORM>
<P>
</BODY>
</HTML>
