<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="/">
    <html>
      <body>
        <table width="561" height="86" cellspacing="0">
          <tr align="center">
            <td width="258" height="8" style="border-left-style: solid;                border-top-style: solid;                border-bottom-style: solid">Description</td>
            <td width="65" height="8" style="border-top-style: solid;                border-bottom-style: solid">Retail Price</td>
            <td width="71" height="8" style="border-top-style: solid;                border-bottom-style: solid">Our Price</td>
            <td width="139" height="8" style="border-right-style: solid;                border-top-style: solid;                border-bottom-style: solid">Department</td>
          </tr>
          <xsl:apply-templates/>
        </table>
      </body>
    </html>
  </xsl:template>
  <xsl:template name="FormatPrice">
    <xsl:param name="Price" select="0"/>
    <xsl:value-of select="format-number($Price,'###.00')"/>
  </xsl:template>
  <xsl:template match="item">
    <tr>
      <td width="258" height="18" style="border-bottom-style: solid">
        <xsl:value-of select="."/>
        <xsl:if test="@picture='y'">
          <img border="0" src="..\..\xmldocs\pic.bmp" width="16" height="15"/>
        </xsl:if>
      </td>
      <td width="65" height="18" align="right" style="border-bottom-style: solid">
        <xsl:call-template name="FormatPrice">
          <xsl:with-param name="Price" select="@retail"/>
        </xsl:call-template>
      </td>
      <td width="71" height="18" align="right" style="border-bottom-style: solid">
        <xsl:call-template name="FormatPrice">
          <xsl:with-param name="Price" select="@sale"/>
        </xsl:call-template>
      </td>
      <td width="139" height="18" align="center" style="border-bottom-style: solid">
        <xsl:choose>
          <xsl:when test="@category='AP'">Appliances</xsl:when>
          <xsl:when test="@category='AU'">Auto</xsl:when>
          <xsl:when test="@category='GR'">Gardening</xsl:when>
          <xsl:when test="@category='SH'">Shoes</xsl:when>
          <xsl:when test="@category='TY'">Toys</xsl:when>
          <xsl:otherwise>Miscellaneous</xsl:otherwise>
        </xsl:choose>
      </td>
    </tr>
  </xsl:template>
</xsl:stylesheet>