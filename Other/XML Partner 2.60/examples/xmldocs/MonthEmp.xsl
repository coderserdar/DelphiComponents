<?xml version="1.0"?>
<xsl:stylesheet version="1.0" 
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
  <table border="1" width="384">
    <tr bgcolor="#FFFF99">
      <th>Location</th>
      <th>Employee</th>
    </tr>
    <xsl:for-each select="/EmployeesOfTheMonth/Location">
      <tr valign="top">
        <td><xsl:value-of select="@name"/></td>
        <td>
          <xsl:value-of select="Employee/@name"/>
          <br/>
          <xsl:value-of select="Employee/@title"/>
      </td>
      </tr>
    </xsl:for-each>
  </table>
</xsl:template>

</xsl:stylesheet>
