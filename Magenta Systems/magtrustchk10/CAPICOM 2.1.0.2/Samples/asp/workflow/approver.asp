<%@Language=VBScript %>
<!--#include file="common.asp" -->

<%

Sub PrintPendingTransactions
  Dim numTrans 
  Dim content
  Dim i

  numTrans = GetNextTransaction()
  Response.Write "<p> This sample will reset itself after " & CInt( GetMaxTransaction( )  ) & " transactions."
  Response.Write "<p> There are " & CInt( numTrans ) & " pending transactions."
  if numTrans > 0 then
    Response.Write "<table border=1><tr><td><b>Select a transaction to view</b></td><td><b>Approved</b></td></tr>"
    for i = 1 to numTrans
      Response.Write "<tr><td><a href='disptrans.asp?transactionId=" & i & "'>Transaction "& i & "</td>"
      Response.Write "<td> " & GetTransactionApprovalTime ( i ) & "</td></tr>"
    next
    Response.Write "</table>"    
  end if
End Sub

%>
<HTML>
<HEAD>
<META HTTP-EQUIV="Pragma" CONTENT="no-cache">
<TITLE>CAPICOM Digital Signing Application - Transaction List</TITLE>
</HEAD>
<BODY>
<!--#include file="menu.htm" -->
<P><STRONG><FONT size=5>Transaction List</FONT></STRONG></P>

<%
  Call Initialize
  PrintPendingTransactions
%>


</BODY>
</HTML>
