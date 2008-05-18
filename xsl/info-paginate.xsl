<!-- Hey, emacs, please consider this to be -*- xml -*-

    Second of two steps for .info file generation.  This stylesheet
    reads a simple XML syntax genareted by info.xsl and turns it into
    the actual .info file.
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:macro="http://lichteblau.com/macro"
		xmlns:atdoc="http://www.lichteblau.com/atdoc/"
		extension-element-prefixes="atdoc"
		version="1.0">
  <xsl:output method="text"/>

  <xsl:template match="p">
    <xsl:call-template name="div"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="div">
    <xsl:call-template name="div"/>
  </xsl:template>

  <xsl:template name="div">
    <xsl:variable name="indent" select="number(concat(0, @indent))"/>
    <xsl:variable name="indent2">
      <xsl:choose>
	<xsl:when test="ancestor::indent">
	  <xsl:value-of select="$indent + ancestor::indent/@indent"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$indent"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="indent3" select="number($indent2)"/>

    <xsl:variable name="initial">
      <xsl:choose>
	<xsl:when test="@initial-indent">
	  <xsl:value-of select="@initial-indent"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$indent3"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="initial2" select="number($initial)"/>

    <xsl:value-of select="atdoc:indent(normalize-space(), $indent3, $initial2)"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="pre">
    <xsl:value-of select="."/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="menu">
    <xsl:text>&#10;* Menu:&#10;&#10;</xsl:text>
    <xsl:for-each select="menu-item">
      <xsl:text>* </xsl:text>
      <xsl:value-of select="@node"/>

      <xsl:text>::&#10;</xsl:text>
<!--
      <xsl:text>::    </xsl:text>
      <xsl:variable name="label" select="normalize-space(@label)"/>
      <xsl:value-of select="substring($label, 1, 30)"/>
      <xsl:value-of select="substring-before(substring($label, 31), ' ')"/>
      <xsl:text>...&#10;</xsl:text>
-->
    </xsl:for-each>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="node">
    <xsl:text>&#10;</xsl:text>
    <atdoc:write-character code="31"/>

    <xsl:text>&#10;File: </xsl:text>
    <xsl:value-of select="/document/@filename"/>

    <xsl:text>,  Node: </xsl:text>
    <xsl:value-of select="@name"/>

    <xsl:if test="following::node">
      <xsl:text>,  Next: </xsl:text>
      <xsl:value-of select="(following::node|descendant::node)/@name"/>
    </xsl:if>

    <xsl:choose>
      <xsl:when test="preceding::node">
	<xsl:text>,  Prev: </xsl:text>
	<xsl:value-of select="preceding::node/@name"/>
      </xsl:when>
      <xsl:when test="ancestor::node">
	<xsl:text>,  Prev: </xsl:text>
	<xsl:value-of select="ancestor::node/@name"/>
      </xsl:when>
    </xsl:choose>

    <xsl:text>,  Up: </xsl:text>
    <xsl:choose>
      <xsl:when test="ancestor::node">
	<xsl:value-of select="ancestor::node/@name"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>(dir)</xsl:text>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:text>&#10;&#10;</xsl:text>

    <xsl:variable name="line">
      <xsl:number level="multiple"/>
      <xsl:text> </xsl:text>
      <xsl:value-of select="@title|@name"/>
    </xsl:variable>
    <xsl:value-of select="$line"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="atdoc:stars($line)"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>&#10;</xsl:text>

    <xsl:apply-templates/>
  </xsl:template>
</xsl:stylesheet>
