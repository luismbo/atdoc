<!--
    This file is used to transform XSLT stylesheets using "macros" into
    real XSLT stylesheets.
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:macro="http://lichteblau.com/macro"
		xmlns:extra="http://lichteblau.com/extra"
		xmlns:atdoc="http://www.lichteblau.com/atdoc/"
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

  <xsl:template match="macro:normalize">
    <xsl:apply-templates mode="normalize"/>
  </xsl:template>

  <xsl:template match="text()" mode="normalize" priority="1">
    <extra:text><xsl:value-of select="normalize-space()"/></extra:text>
  </xsl:template>

  <xsl:template match="node()" mode="normalize">
    <xsl:apply-templates select="."/>
  </xsl:template>

  <xsl:template match="macro:escaped">
    <xsl:if test="string(@select) = ''">
      <xsl:message terminate="yes">
	suspicious @select in macro:escaped
      </xsl:message>
    </xsl:if>

    <extra:value-of select="atdoc:escape-latex-string({@select})"/>
  </xsl:template>

  <xsl:template match="macro:glsdefun">
    <extra:variable name="{generate-id()}-label">
      <xsl:value-of select="@label"/>
    </extra:variable>

    \rule{\linewidth}{0.1mm}
    
    \label{<extra:value-of select="@id"/>}
    \begin{defun}[<extra:value-of
		     select="atdoc:escape-latex-string(${generate-id()}-label)"/>]
    <extra:value-of select="atdoc:escape-latex-string(@name)"/>
    <extra:for-each select="lambda-list/elt">
      <extra:text xml:space="preserve"> </extra:text>
      <xsl:choose>
	<xsl:when test="@label = 'Function'">
	  <extra:value-of select="."/>
	</xsl:when>
	<xsl:otherwise>
	  <extra:value-of select="atdoc:escape-latex-string(.)"/>
	</xsl:otherwise>
      </xsl:choose>
    </extra:for-each>

    <extra:text xml:space="preserve">&#10;&#10;</extra:text>

    <xsl:apply-templates/>
    \end{defun}
  </xsl:template>

  <xsl:template match="macro:sc">
    \bigskip
    <extra:text>\textsc{</extra:text>
    <extra:text>
      <xsl:value-of select="@label"/>
    </extra:text>
    <extra:text>}</extra:text>
    <extra:text xml:space="preserve">&#10;&#10;</extra:text>
    <xsl:apply-templates/>
    <extra:text xml:space="preserve">&#10;&#10;</extra:text>
  </xsl:template>    

  <xsl:template match="macro:itemize">
    \begin{itemize}
    <xsl:apply-templates/>
    \end{itemize}
  </xsl:template>

  <xsl:template match="macro:hyperref">
    <extra:if test="count(key('id', @id)) = 0">
      <extra:message>
	Unresolved reference to <extra:value-of select="@id"/>
      </extra:message>
    </extra:if>
    <xsl:text>\hyperref[</xsl:text>
    <extra:value-of select="@id"/>
    <xsl:text>]{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
<!--
      (page~\pageref{<extra:value-of select="@id"/>})
-->
  </xsl:template>
</xsl:stylesheet>
