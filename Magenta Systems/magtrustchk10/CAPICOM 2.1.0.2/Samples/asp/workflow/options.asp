<%@Language=VBScript %>
<!--#include file="common.asp" -->

<HTML>
<HEAD>
<TITLE>CAPICOM Digital Signing Application - Display Options</TITLE>
</HEAD>
<BODY>
<!--#include file="menu.htm" -->
<P><STRONG><FONT size=5>Display Options</FONT></STRONG></P><STRONG><FONT size=5>
<HR>
</FONT></STRONG>

<%
  Call Initialize
%>
<FORM ACTION="options_srv.asp" METHOD="POST">
Approval Mode:<br>
<INPUT Type=Radio VALUE="Standalone" Name=optGroupApproval ID=optStandalone  
<% 
    if GetSelfApproval( ) Then
        Response.Write "Checked"
    End if
%>
>
<Label For=optStandalone >Submitters can approve their own transactions</Label>
<br>
<INPUT Type=Radio VALUE="UseAD" Name=optGroupApproval ID=optUseAD 
<% 
    if Not GetSelfApproval( ) Then
        Response.Write "Checked"
    End if
%>
>
<Label For=optUseAD >A Manager must approve the transaction (uses Active Directory)</Label>
<P>
Revocation Checking:<br>
<INPUT Type=Radio VALUE="NoRevocation" Name=optGroupRevocation ID=optNoRevocation 
<% 
    if Not IsRevocationEnabled( ) Then
        Response.Write "Checked"
    End if
%>
>
<Label For=optNoRevocation>Do not perform revocation checking</Label>
<br>
<INPUT Type=Radio VALUE="CheckRevocation" Name=optGroupRevocation ID=optCheckRevocation 
<% 
    if  IsRevocationEnabled( ) Then
        Response.Write "Checked"
    End if
%>
>
<Label For=optCheckRevocation>Check revocation on each signer certificate</Label>
<P>
<Label For=txtTransLimit>Transaction Limit</Label>
<INPUT Type=Text Value=
<% 
    Response.Write GetMaxTransaction
%>

Name=txtTransLimit ID=txtTransLimit SIZE=4>
<p>
<INPUT TYPE=SUBMIT Value="Change options">
</FORM>
</BODY>
</HTML>
