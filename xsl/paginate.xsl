<!--
    The final stylesheet for HTML generation.

    This file splits up the XML document and its <page> elements into
    individual HTML files.

    A few formatting elements like <columns> are resolved here.

    This is an XSLT 1.0 stylesheet.
    (We set version to 42 so that spec-conforming implementations don't
    trip over xsltprocs bogus xsl:document element.)
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:xuriella="http://common-lisp.net/project/xuriella"
		version="42"
		extension-element-prefixes="xuriella">
  <xsl:output method="html"
	      indent="yes"
	      doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
	      doctype-system="http://www.w3.org/TR/html4/loose.dtd"/>

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="/">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="pages">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="main-page">
    <xsl:call-template name="page"/>
  </xsl:template>

  <xsl:template match="page">
    <xsl:choose>
      <!-- first, try the correct approach and use the extension -->
      <xsl:when test="element-available('xuriella:document')">
	<xuriella:document
	   href="{@pathname}"
	   method="html"
	   indent="yes"
	   doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
	   doctype-system="http://www.w3.org/TR/html4/loose.dtd">
	  <xsl:call-template name="page"/>
	</xuriella:document>
      </xsl:when>
      <!-- if not available, fall back to what xsltproc wants.
	   See above for the version hack to allow this.
	-->
      <xsl:otherwise>
	<xsl:document href="{@pathname}"
		      method="html"
		      indent="yes"
		      doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
		      doctype-system="http://www.w3.org/TR/html4/loose.dtd">
	  <xsl:call-template name="page"/>
	</xsl:document>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template name="page">
    <html>
      <head>
	<title>
	  <xsl:value-of select="@title"/>
	</title>
	<link rel="stylesheet" type="text/css" href="{@base}{/pages/@css}"/>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
      </head>
      <body>
	<xsl:call-template name="header">
	  <xsl:with-param name="base" select="@base"/>
	</xsl:call-template>
	<div class="main">
	  <xsl:apply-templates/>
	</div>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="columns">
    <table cellspacing="0" cellpadding="0">
      <xsl:apply-templates select="@*"/>
      <tr>
	<xsl:apply-templates/>
      </tr>
    </table>
  </xsl:template>

  <xsl:template match="column">
    <td valign="top">
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </td>
  </xsl:template>

  <xsl:template match="simple-table">
    <table cellspacing="0" cellpadding="0">
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </table>
  </xsl:template>

  <xsl:template match="row">
    <tr>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </tr>
  </xsl:template>

  <xsl:template match="cell">
    <td valign="top">
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </td>
  </xsl:template>

  <xsl:template match="padded">
    <div class="padded">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template name="header">
    <xsl:param name="base"/>
    <div id="header">
      <table cellspacing="0" cellpadding="0" width="100%">
	<tr>
	  <xsl:if test="/pages/@logo">
	    <td width="176">
	      <a id="headerlink" href="{$base}../index.html">
		<img src="{$base}{/pages/@logo}" border="0"/>
	      </a>
	    </td>
	  </xsl:if>
	  <td valign="center">
	    <xsl:choose>
	      <xsl:when test="/pages/@logo">
		&#x2014;
	      </xsl:when>
	      <xsl:otherwise>
		&#160;&#160;
	      </xsl:otherwise>
	    </xsl:choose>
	    <b>
	      <xsl:text> </xsl:text>
	      <xsl:value-of select="/pages/@heading"/>
	    </b>
	  </td>
	  <td valign="center" align="right">
            <b>API documentation</b>
	  </td>
	</tr>
      </table>
    </div>
  </xsl:template>
</xsl:stylesheet>
