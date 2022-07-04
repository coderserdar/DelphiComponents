<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="/">
    <fo:root>
      <fo:layout-master-set>
        <fo:simple-page-master master-name="content" page-height="11in" page-width="8.5in" margin-top="0.5in" margin-bottom="0.5in" margin-left="0.5in" margin-right="0.5in">
          <fo:region-body margin-top="0in" margin-bottom="0.5in" margin-left="0in" margin-right="0in"/>
        </fo:simple-page-master>
      </fo:layout-master-set>
      <fo:page-sequence master-name="content">
        <xsl:apply-templates/>
      </fo:page-sequence>
    </fo:root>
  </xsl:template>
  <xsl:template match="Memo">
    <fo:block font-size="16pt" space-after.optimum="10">MEMO</fo:block>
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="to">
    <fo:block font-size="10pt" space-after.optimum="2pt">
      <fo:inline font-weight="bold">
        <xsl:text>To:   </xsl:text>
      </fo:inline>
      <xsl:value-of select="."/>
    </fo:block>
  </xsl:template>
  <xsl:template match="from">
    <fo:block font-size="10pt" space-after.optimum="2pt">
      <fo:inline font-weight="bold">
        <xsl:text>From: </xsl:text>
      </fo:inline>
      <xsl:value-of select="."/>
    </fo:block>
  </xsl:template>
  <xsl:template match="date">
    <fo:block  space-after.optimum="10pt" font-size="10pt">
      <xsl:text> </xsl:text>
      <fo:inline font-weight="bold">
        <xsl:text>Date: </xsl:text>
      </fo:inline>
      <xsl:value-of select="."/>
    </fo:block>
  </xsl:template>
  <xsl:template match="content">
    <fo:block>
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  <xsl:template match="emph">
    <xsl:text> </xsl:text>
    <fo:inline font-weight="bold">
      <xsl:text> </xsl:text>
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  <xsl:template match="ital">
    <xsl:text> </xsl:text>
    <fo:inline font-style="italic">
      <xsl:apply-templates/>
      <xsl:text> </xsl:text>
    </fo:inline>
  </xsl:template>
</xsl:stylesheet>