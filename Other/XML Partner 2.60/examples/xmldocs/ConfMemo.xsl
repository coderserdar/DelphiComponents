<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="basememo.xsl"/>

  <xsl:template match="Memo">
    <fo:block font-size="18pt"
              text-align="centered">CONFIDENTIAL</fo:block>
    <xsl:apply-imports/>
  </xsl:template>
</xsl:stylesheet>
