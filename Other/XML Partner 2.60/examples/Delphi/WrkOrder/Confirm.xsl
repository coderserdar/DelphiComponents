<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="WorkOrder">

  <xsl:text>Dear </xsl:text>
  <xsl:value-of select="Name"/>
  <xsl:text>,&#13;&#10;&#13;&#10;</xsl:text>

  <xsl:text>The following request was assigned</xsl:text>
  <xsl:text>to</xsl:text>
  <xsl:value-of select="AssignedTo"/>
  <xsl:text>on </xsl:text>
  <xsl:value-of select="AssignDate"/>
  <xsl:text>: &#13;&#10;&#13;&#10;</xsl:text>
  <xsl:value-of select="RequestDesc"/>
  <xsl:text>&#13;&#10;&#13;&#10;Sincerely,&#13;&#10;Technical Support</xsl:text>

</xsl:template>

</xsl:stylesheet>

