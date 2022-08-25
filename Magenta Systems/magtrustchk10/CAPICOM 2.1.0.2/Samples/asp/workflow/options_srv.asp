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
<TITLE>CAPICOM Digital Signing Application - Change Options</TITLE>
</HEAD>
<BODY>
<!--#include file="menu.htm" -->

<P><FONT size=5><STRONG>Options Changed</STRONG>   
</FONT></P>
<HR>
<P><P><STRONG>
<%
  Dim optGroupRevocation
  Dim optGroupApproval
  Dim txtTransLimit
  txtTransLimit = Request.Form("txtTransLimit")
  optGroupRevocation = Request.Form("optGroupRevocation")
  optGroupApproval = Request.Form("optGroupApproval")
  SetMaxTransaction ( txtTransLimit )
  Response.Write "<p>Max transactions = " & GetMaxTransaction
  if optGroupRevocation =  "NoRevocation" Then
    SetRevocation( False )
  Else 
    SetRevocation( True )
  End if
  Response.Write "<p>Check Revocation = " & IsRevocationEnabled
  if optGroupApproval = "Standalone" Then
    SetApproval ( True )
  Else
    SetApproval ( False )
  End If
  
  Response.Write "<p>Employees can approve their own transactions = " & GetSelfApproval
%>
<P>
</BODY>
</HTML>
