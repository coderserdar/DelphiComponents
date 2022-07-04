<?xml version='1.0'?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  >

<xsl:template match="/">
<fo:root>
<fo:layout-master-set>

<fo:simple-page-master master-name="first"
  page-height="21cm"
  page-width="15cm"
  margin-top="1cm"
  margin-bottom="2cm"
  margin-left="2.5cm"
  margin-right="2.5cm">
  <fo:region-body margin-top="3cm"/>
  <fo:region-before extent="3cm"/>
  <fo:region-after extent="1.5cm"/>
</fo:simple-page-master>

<fo:simple-page-master master-name="rest"
  page-height="21cm"
  page-width="15cm"
  margin-top="1cm"
  margin-bottom="2cm"
  margin-left="2.5cm"
  margin-right="2.5cm">
  <fo:region-body margin-top="2.5cm"/>
  <fo:region-before extent="2.5cm"/>
  <fo:region-after extent="1.5cm"/>
</fo:simple-page-master>

<fo:page-sequence-master master-name="defaultPSM" >
  <fo:repeatable-page-master-alternatives>
    <fo:conditional-page-master-reference master-name="first"
      page-position="first" />
    <fo:conditional-page-master-reference master-name="rest"
      page-position="rest" />
    <!-- recommended fallback procedure -->
    <fo:conditional-page-master-reference master-name="rest" />
  </fo:repeatable-page-master-alternatives>
</fo:page-sequence-master>

</fo:layout-master-set>

<fo:page-sequence master-name="first">

<fo:flow flow-name="xsl-region-body">
  <fo:block font-size="18pt"
    font-family="sans-serif"
    line-height="24pt"
    space-after.optimum="3pt"
    background-color="blue"
    color="white"
    text-align="center"
    padding-top="3pt">
    <xsl:value-of select="'Monthly Sales Report (by Customer)'"/>

  </fo:block>

  <table width="561" height="86" cellspacing="0">
    <tr align="center">
      <td width="200" height="8" style="border-bottom-style: solid"><b>Customer</b></td>
      <td width="97" height="8" style="border-bottom-style: solid" align="right"><b>Last month</b></td>
      <td width="97" height="8" style="border-bottom-style: solid" align="right"><b>YTD</b></td>
    </tr>
    <xsl:apply-templates/>
  </table>
</fo:flow>
</fo:page-sequence>

</fo:root>
</xsl:template>

<xsl:template match="Customer">
  <xsl:variable name="statusFont">
    <xsl:choose>
      <xsl:when test="SalesInfo/@targetMet='yes'">#008000</xsl:when>
      <xsl:otherwise>#000000</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <tr>
    <td width="200" height="18" style="border-bottom-style: solid">
      <xsl:choose>
        <xsl:when test="$statusFont='#008000'">
          <b>
          <font color="{$statusFont}"><xsl:value-of select="@name"/></font>
          </b>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@name"/>
        </xsl:otherwise>
      </xsl:choose>
    </td>
    <td width="97" height="18" align="right" style="border-bottom-style: solid">
      <xsl:choose>
        <xsl:when test="$statusFont='#008000'">
          <b>
          <font color="{$statusFont}"><xsl:value-of select="SalesInfo/@lastweek"/></font>
          </b>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="SalesInfo/@lastweek"/>
        </xsl:otherwise>
      </xsl:choose>
    </td>
    <td width="97" height="18" align="right" style="border-bottom-style: solid">
      <xsl:choose>
        <xsl:when test="$statusFont='#008000'">
          <b>
          <font color="{$statusFont}"><xsl:value-of select="SalesInfo/@YTD"/></font>
          </b>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="SalesInfo/@YTD"/>
        </xsl:otherwise>
      </xsl:choose>
    </td>
  </tr>
</xsl:template>

</xsl:stylesheet>
