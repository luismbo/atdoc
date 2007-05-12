<!--
    This file is used to transform XSLT stylesheets using "macros" into
    real XSLT stylesheets.
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:macro="http://lichteblau.com/macro"
		xmlns:extra="http://lichteblau.com/extra"
		version="1.0">
  <xsl:output method="xml" indent="no"/>

  <xsl:namespace-alias stylesheet-prefix="extra" result-prefix="xsl"/>

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="macro:maybe-columns">
    <extra:choose>
      <extra:when test="{@test}">
	<columns width="100%">
	  <column width="60%">
	    <xsl:apply-templates/>
	  </column>
	  <column width="5%">
	    &#160;
	  </column>
	  <column width="35%">
	    <extra:call-template name="main-right"/>
	  </column>
	</columns>
      </extra:when>
      <extra:otherwise>
	<xsl:apply-templates/>
      </extra:otherwise>
    </extra:choose>
  </xsl:template>

  <xsl:template match="macro:copy-attribute">
    <extra:if test="{@path}/@{@name}">
      <extra:attribute name="{@name}">
	<extra:value-of select="{@path}/@{@name}"/>
      </extra:attribute>
    </extra:if>
  </xsl:template>    
</xsl:stylesheet>
