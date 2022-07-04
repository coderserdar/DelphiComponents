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
        <fo:block font-size="24pt" space-after.optimum="10">Work Order</fo:block>
        <fo:block font-size="12pt" space-after.optimum="4pt">
          <fo:inline font-weight="bold">Number: </fo:inline> 
          <xsl:value-of select="WorkOrder/@number"/>
        </fo:block>
        
        <fo:block font-size="12pt" space-after.optimum="4pt">
          <fo:inline font-weight="bold">Status: </fo:inline>
          <xsl:value-of select="WorkOrder/Status"/>
        </fo:block>

        <fo:block font-size="12pt" space-after.optimum="4pt">
          <fo:inline font-weight="bold">Request date: </fo:inline>
          <xsl:value-of select="WorkOrder/RequestDate"/>
        </fo:block>

        <fo:block font-size="12pt" space-after.optimum="10pt">
          <fo:inline font-weight="bold">Date needed: </fo:inline>
          <xsl:value-of select="WorkOrder/DateNeeded"/>
        </fo:block>
 
        <fo:block font-size="12pt" space-after.optimum="4pt">
          <fo:inline font-weight="bold">Assigned to: </fo:inline> 
          <xsl:value-of select="WorkOrder/AssignedTo"/>
        </fo:block>

        <fo:block font-size="12pt" space-after.optimum="10pt">
          <fo:inline font-weight="bold">Assigned date: </fo:inline>
          <xsl:value-of select="WorkOrder/AssignDate"/>
        </fo:block>

        <fo:block font-size="12pt" space-after.optimum="15pt">
          <fo:block font-size="12pt" space-after.optimum="3pt"
                    font-weight="bold">   
            Request :
          </fo:block> 
          <xsl:value-of select="WorkOrder/RequestDesc"/>
        </fo:block>

        <fo:block font-size="12pt" space-after.optimum="5pt"
                  space-before.optimum="15pt" font-weight="bold">
          Point of Contact
        </fo:block>
        <fo:block font-size="12pt" space-after.optimum="4pt">
          <fo:inline font-weight="bold">Name: </fo:inline>
          <xsl:value-of select="WorkOrder/Name"/>
        </fo:block>

        <fo:block font-size="12pt" space-after.optimum="4pt">
          <fo:inline font-weight="bold">Number: </fo:inline>
          <xsl:value-of select="WorkOrder/Number"/>
        </fo:block>

        <fo:block font-size="12pt" space-after.optimum="4pt">
          <fo:inline font-weight="bold">Email: </fo:inline>
          <xsl:value-of select="WorkOrder/email"/>
        </fo:block>

        <fo:block font-size="12pt" space-after.optimum="4pt">
          <fo:inline font-weight="bold">Department: </fo:inline>
          <xsl:value-of select="WorkOrder/Dept"/>
        </fo:block>


      </fo:page-sequence>
    </fo:root>
  </xsl:template>






</xsl:stylesheet>