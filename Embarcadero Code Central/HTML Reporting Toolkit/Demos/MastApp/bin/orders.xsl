<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/TR/WD-xsl">
<xsl:template match="/">
<html>
  <head>
    <title>Order History</title>
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
       var
         FromDate = '<xsl:value-of select="body/@date1"/>';
         ToDate = '<xsl:value-of select="body/@date2"/>';
      <![CDATA[
        function Init() {
           window.external.DateTimeTickerEnabled = true;
           window.external.FromDate = FromDate;
           window.external.ToDate = ToDate;
           window.external.SelectedNode = 1;
        }
      ]]>
    </script>
  </head>
  <body onLoad="Init()">
    <SPAN CLASS="printOnly">
    <P ALIGN="CENTER"><FONT SIZE="+2"><B>Order History</B></FONT>
    <BR/>By Date
    <BR/>From <xsl:value-of select="body/@date1"/> To <xsl:value-of select="body/@date2"/>
    </P>
    <H1><I>M.A.S.T.</I></H1>
    </SPAN>
    <table CELLPADDING="0" BORDER="0" CELLSPACING="1" Width="650">
      <thead>
        <tr>
          <th>Order Date</th>
          <th rowspan="2">Customer</th>
          <th rowspan="2">Ship Date</th>
          <th rowspan="2">Ship VIA</th>
          <th rowspan="2">Terms</th>
          <th rowspan="2">Pay Method</th>
          <th rowspan="2">Items Total</th>
        </tr>
        <tr><th>Order No</th></tr>
      </thead>
      <tbody>
        <xsl:for-each select="body/row">
          <tr>
            <td align="right">
              <xsl:value-of select="SaleDate"/><BR/><xsl:value-of select="OrderNo"/>
            </td>
            <td><A><xsl:attribute name="href">abc://././customer_orders?CustNo=<xsl:value-of select="CustNo"/></xsl:attribute>
               <B><xsl:value-of select="Company"/></B></A></td>
            <td><xsl:value-of select="ShipDate"/></td>
            <td><xsl:value-of select="ShipVIA"/></td>
            <td><xsl:value-of select="Terms"/></td>
            <td><xsl:value-of select="PaymentMethod"/></td>
            <td align="right">
              <xsl:eval>formatNumber(this.selectSingleNode('ItemsTotal').nodeTypedValue,
                 "###,#0.00")</xsl:eval>
            </td>
          </tr>
        </xsl:for-each>
      </tbody>
    </table>
  </body>
</html>
</xsl:template>

</xsl:stylesheet>
