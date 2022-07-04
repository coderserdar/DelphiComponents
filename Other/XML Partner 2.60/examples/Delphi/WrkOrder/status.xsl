<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
  <html>
    <body>
      <b><font size="4" face="Verdana">Information Technology Work Order Status</font></b>
      <hr/>
      <P/>
      <table border="0" width="80%" cellspacing="0" cellpadding="3">
        <tr>
          <td width="19%" bgcolor="#008000">
            <p align="center"><font color="#FFFFFF"><b>Work Order</b></font>
            </p>
          </td>
          <td width="19%" bgcolor="#008000">
            <p align="center"><font color="#FFFFFF"><b>Category</b></font></p>
          </td>
          <td width="15%" bgcolor="#008000">
            <p align="center"><font color="#FFFFFF"><b>Received</b></font></p>
          </td>
          <td width="51%" bgcolor="#008000"><font color="#FFFFFF"><b>Assigned</b></font>
          </td>
        </tr>
        <xsl:apply-templates select="//WorkOrder">
          <xsl:sort select="@number" order="ascending" date-type="number"/>
        </xsl:apply-templates>
      </table>
    </body>
  </html>
</xsl:template>

<xsl:template match="WorkOrder">
  <tr>
    <td width="19%" bgcolor="#FFFFCC">
      <p align="center"><font size="3">
        <xsl:value-of select="@number"/>
      </font></p></td>
    <td width="19%" bgcolor="#FFFFCC">
      <p align="center"><font size="3">
        <xsl:value-of select="Category"/>
      </font></p></td>
    <td width="15%" bgcolor="#FFFFCC">
      <p align="center"><font size="3">
        <xsl:value-of select='RequestDate'/>
      </font></p></td>
    <td width="51%" bgcolor="#FFFFCC"><font size="3">
      <xsl:choose>
        <xsl:when test="AssignedTo=''">Unassigned</xsl:when>
        <xsl:otherwise>
          <xsl:value-of select='AssignedTo'/>
          <xsl:text>, </xsl:text>
          <xsl:value-of select='AssignDate'/>
        </xsl:otherwise>
      </xsl:choose>
    </font></td>
  </tr>
  <tr>
    <td width="19%"></td>
    <td width="81%" colspan="3"><font color="#0000FF" size="3">
      <xsl:value-of select="concat(substring(RequestDesc, 1, 75),'...')"/>
    </font></td>
  </tr>
</xsl:template>


</xsl:stylesheet>
