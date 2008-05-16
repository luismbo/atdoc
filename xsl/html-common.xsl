<!-- Hey, emacs, please consider this to be -*- xml -*-
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:macro="http://lichteblau.com/macro"
		version="1.0">
  <!--
      Symbol index
    -->
  <xsl:template match="*" mode="symbol-index"/>

  <xsl:template match="external-symbols" mode="symbol-index">
    <xsl:param name="packagep"/>
    <simple-table>
      <xsl:apply-templates mode="symbol-index">
	<xsl:sort select="@name" data-type="text" order="ascending"/>
	<xsl:with-param name="packagep" select="$packagep"/>
      </xsl:apply-templates>
    </simple-table>
  </xsl:template>

  <xsl:template name="index-entry">
    <xsl:param name="packagep"/>
    <xsl:param name="kind"/>

    <row>
      <xsl:if test="$packagep">
	<cell align="right" nowrap="nowrap">
	  <span class="nonlink">
	    <tt>
	      <span style="color: #777777">
		<xsl:value-of select="../../@name"/>
		<xsl:text>:</xsl:text>
	    </span>
	    </tt>
	  </span>
	</cell>
      </xsl:if>
      <cell>
	<a href="{$packagep}{@id}.html">
	  <tt>
	    <xsl:value-of select="@name"/>
	  </tt>
	</a>
	<xsl:text>, </xsl:text>
	<xsl:value-of select="$kind"/>
	<xsl:call-template name="undocumented"/>
      </cell>
    </row>
  </xsl:template>

  <xsl:template match="class-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'class'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="function-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'function'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="macro-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'macro'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="type-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'type'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="variable-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'variable'"/>
    </xsl:call-template>
  </xsl:template>
</xsl:stylesheet>
