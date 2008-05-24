<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:atdoc="http://www.lichteblau.com/atdoc/"
		version="1.0">

  <xsl:template name="copy-base-uri">
    <xsl:attribute name="xml:base">
      <xsl:value-of select="atdoc:base-uri(/*)"/>
    </xsl:attribute>
  </xsl:template>
</xsl:stylesheet>
