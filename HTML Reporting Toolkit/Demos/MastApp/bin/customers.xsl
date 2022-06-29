<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/TR/WD-xsl">
<xsl:template match="/">
<html>
  <head>
    <title>Customer List</title>
    <style>
      <![CDATA[
        body      { font-family: "Verdana, Arial, Helvetica";
                    font-size: "10pt" }
        hr        { color: blue }
        table     { font-family: "Verdana, Tahoma"; font-size: "11px" }
        td        { background-color:"#EEEEEE"; cursor: default;
                    padding-left: 3pt; padding-right: 3pt; padding-top: 2pt; padding-bottom: 2pt; }
        th        { background-color:"#E3E3E3";
                    padding-left: 3pt; padding-right: 3pt; padding-top: 2pt; padding-bottom: 2pt; }

        A        {  text-decoration: none; color: black; }
        A:hover  { text-decoration: underline; }
        A:visited { text-decoration: none; color: black }
        A:visited:hover { text-decoration: underline; color: black }

        .printOnly { display: none }

        @media print  {
          .printOnly { display: block }
        }
      ]]>
    </style>
    <script>
      <![CDATA[
        function Init() {
           window.external.SelectedNode = 2;
        }
      ]]>
    </script>
  </head>
  <body onLoad="Init()">
    <SPAN CLASS="printOnly">
    <P ALIGN="CENTER"><FONT SIZE="+2"><B>Customer List</B></FONT>
    <BR/>By Last Invoice
    </P>
    <H1><I>M.A.S.T.</I></H1>
    </SPAN>
    <table CELLPADDING="0" BORDER="0" CELLSPACING="1" Width="600">
      <thead>
        <tr>
          <th rowspan="2" valign="bottom">Last Invoice</th>
          <th>Customer</th>
          <th>Phone</th>
          <th rowspan="2" valign="bottom">Customer No</th>
        </tr>
        <tr>
          <th>Address</th>
          <th>FAX</th>
        </tr>
      </thead>
      <tbody>
        <xsl:for-each select="body/row">
          <tr valign="bottom">
            <td align="center"><xsl:value-of select="LastInvoiceDate"/></td>
            <td><A><xsl:attribute name="href">abc://././customer_orders?CustNo=<xsl:value-of select="CustNo"/></xsl:attribute>
                <B><xsl:value-of select="Company"/></B>
                <BR/><xsl:value-of select="Address"/></A></td>
            <td align="center"><xsl:value-of select="Phone"/>
                <BR/><xsl:value-of select="Fax"/></td>
            <td align="right"><xsl:value-of select="CustNo"/></td>
          </tr>
        </xsl:for-each>
      </tbody>
    </table>
  </body>
</html>
</xsl:template>
</xsl:stylesheet>
