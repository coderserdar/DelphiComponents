<?xml version="1.0"?>                    <!--XML declaration-->
<xsl:stylesheet version="1.0">           <!--Start style sheet-->
  <xsl:template match="STATE">           <!--Get all STATE elements-->
    <xsl:for-each select="CITY">         <!--Iterate STATE's CITY elements-->
      <xsl:element name="city">          <!--Mark start of new record-->
        <xsl:value-of select="@NAME"/>   <!--Send CITY's NAME to result-->
        <xsl:value-of select="../@NAME"/><!--Send STATE's NAME to result-->
        <xsl:value-of select="HI/@C"/>   <!--Send CITY's HI temp to result-->
        <xsl:value-of select="LOW/@C"/>  <!--Send CITY's LO temp to result-->
        <xsl:value-of select="."/>       <!--Send forecast text to result-->
      </xsl:element>                     <!--Mark the end of the record-->
    </xsl:for-each>                      <!--End of the CITY elements-->
  </xsl:template>                        <!--Release STATE elements-->
</xsl:stylesheet>                        <!--End style sheet-->