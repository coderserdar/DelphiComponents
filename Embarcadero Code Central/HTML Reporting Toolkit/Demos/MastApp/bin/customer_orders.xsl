<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/TR/WD-xsl">
<xsl:template match="/">
<html>
  <head>
    <title>Customer Orders</title>
    <style>
      <![CDATA[
        body         { font-family: "Verdana, Arial, Helvetica";
                       font-size: "10pt" }
        table        { font-family: "Verdana, Tahoma"; font-size: "11px" }
        td           { background-color:"#EEEEEE"; cursor: default;
                       padding-left: 3pt; padding-right: 3pt; padding-top: 2pt; padding-bottom: 2pt; }
        th           { background-color:"#E3E3E3";
                       padding-left: 3pt; padding-right: 3pt; padding-top: 2pt; padding-bottom: 2pt; }

        A        {  text-decoration: none; color: black; }
        A:hover  { text-decoration: underline; }
        A:visited { text-decoration: none; color: black }
        A:visited:hover { text-decoration: underline; color: black }

        table.t2 { background-color:"#ffffe7"; border-style: solid;
          font-family: "Verdana, Arial, Tahoma"; font-size: "8pt" }
        table.t2 td { background-color:"#ffffe7";
          padding-left: 1pt; padding-right: 1pt; padding-top: 0pt; padding-bottom: 0pt;}
      ]]>
    </style>
  </head>
  <body>
    <table>
    <tr><td style='background-color:white'><table align="center" class="t2">
      <col align="center"/>
      <tr><td><B><xsl:value-of select="body/Info/CustNo"/>
        <xsl:value-of select="body/Info/Company"/></B></td></tr>
      <tr align="right"><td><xsl:value-of select="body/Info/Addr"/></td></tr>
      <tr><td><I><xsl:value-of select="body/Info/Contact"/></I></td></tr>
      <tr><td>Phone: <xsl:value-of select="body/Info/Phone"/>
        Fax: <xsl:value-of select="body/Info/Fax"/></td></tr>
      <tr><td>Last Invoice <xsl:value-of select="body/Info/LastInvoiceDate"/></td></tr>
    </table></td></tr>
    <tr><td style='background-color:white'>
      <table VALIGN="BOTTOM" CELLPADDING="0" BORDER="0" CELLSPACING="1" width="350">
      <thead>
        <tr>
          <th>Order No</th>
          <th>Sale Date</th>
          <th>Amount Paid</th>
          <th>Amount Due</th>
        </tr>
      </thead>
      <tbody>
        <xsl:for-each select="body/row">
          <tr>
            <td align="right"><A>
              <xsl:attribute name="href">javascript:window.external.EditOrder(<xsl:value-of select="OrderNo"/>)</xsl:attribute>
              <xsl:value-of select="OrderNo"/></A></td>
            <td align="center"><xsl:value-of select="SaleDate"/></td>
            <td align="right">
              <xsl:eval>formatNumber(this.selectSingleNode('AmountPaid').nodeTypedValue,
                 "###,#0.00")</xsl:eval></td>
            <td align="right">
              <xsl:eval>formatNumber(this.selectSingleNode('AmountDue').nodeTypedValue,
                 "###,#0.00")</xsl:eval></td>
          </tr>
        </xsl:for-each>
      </tbody>
    </table></td></tr>
    </table>
  </body>
</html>
</xsl:template>
</xsl:stylesheet>