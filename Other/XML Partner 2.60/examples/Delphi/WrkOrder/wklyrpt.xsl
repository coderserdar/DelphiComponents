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
        <fo:block space-before.optimum="10" space-after.optimum="10" text-align="center" font-weight="bold" color="green" font-size="16pt">
          IT Weekly Status Report
        </fo:block>
        <!-- Generate weekly statistics -->
        <fo:block font-weight="bold" text-align="left" color="black" font-size="12pt">
        Work order summary
        </fo:block>
        <fo:leader leader-length="5in" leader-pattern="rule" rule-thickness="1pt"/>
        <fo:block>
        <xsl:text>  Week start: </xsl:text>
        <xsl:value-of select="count(//WorkOrder[@old='yes'])"/>
        </fo:block>
        <fo:block>
        <xsl:text>  New: </xsl:text>
        <xsl:value-of select="count(//WorkOrder[@old=''])"/>
        </fo:block>
        <fo:block>
        <xsl:text>  Completed: </xsl:text>
        <xsl:value-of select="count(//WorkOrder[@status='closed'])"/>
        </fo:block>
        <fo:block>
        <xsl:text>  Week end: </xsl:text>
        <xsl:value-of select="count(//WorkOrder[@status='open' or @status='new'])"/>
        </fo:block>
        <fo:block font-weight="bold" font-size="12pt" space-before.optimum="20" space-after.optimum="5">
        Open work orders
        </fo:block>
        <fo:leader leader-length="5in" leader-pattern="rule" rule-thickness="1pt"/>
        <xsl:for-each select="//WorkOrder[@status='open' or @status='new']">
          <fo:block>
            <xsl:value-of select="@number"/>
            <xsl:text> - </xsl:text>
            <xsl:value-of select="DateNeeded"/>
            <xsl:text> - </xsl:text>
            <xsl:value-of select="concat(substring(RequestDesc,1,50), '...')"/>
          </fo:block>
        </xsl:for-each>
      </fo:page-sequence>
    </fo:root>
  </xsl:template>

</xsl:stylesheet>