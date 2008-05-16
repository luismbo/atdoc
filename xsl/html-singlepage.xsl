<!-- Hey, emacs, please consider this to be -*- xml -*-

    This is an alternative to html.xsl for single-page output.
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:macro="http://lichteblau.com/macro"
		version="1.0">
  <xsl:import href="html-common.xsl"/>

  <xsl:output method="xml" indent="yes"/>

  <xsl:template match="/">
    <pages>
      <macro:copy-attribute name="logo" path="documentation"/>
      <macro:copy-attribute name="css" path="documentation"/>
      <macro:copy-attribute name="heading" path="documentation"/>
      <xsl:apply-templates select="documentation"/>
    </pages>
  </xsl:template>

  <xsl:template match="documentation">
    <main-page title="{@index-title}">
      <div id="sp-about-packages">
	<xsl:for-each select="package">
	  <p>
	    <i>About <xsl:value-of select="@name"/>:</i>
	    <xsl:apply-templates select="documentation-string"/>
	  </p>
	</xsl:for-each>
      </div>

      <h3>Contents</h3>
      <div class="indent">
	<xsl:if test="package/sections">
	  <ul>
	    <xsl:for-each select="package/sections/section">
	      <li>
		<a href="#{generate-id()}">
		  <xsl:value-of select="@section"/>
		</a>
	      </li>
	    </xsl:for-each>
	  </ul>
	</xsl:if>
      </div>

      <xsl:apply-templates select="package/sections"/>

      <h3><a name="index"></a>Exported Symbol Index</h3>
      <simple-table>
	<xsl:apply-templates select="package/external-symbols/*"
			     mode="symbol-index">
	  <xsl:sort select="@name" data-type="text" order="ascending"/>
	  <xsl:with-param name="packagep" select="'pages/'"/>
	</xsl:apply-templates>
      </simple-table>
    </main-page>
  </xsl:template>

  <xsl:template match="class-definition">
    <page base="../"
	  pathname="pages/{@id}.html"
	  title="Class {@name}">
      <padded>
	<p class="noindent">
	  Package:
	  <a href="{../../@id}.html">
	    <xsl:value-of select="../../@name"/>
	  </a>
	</p>
	<h2 class="page-title">
	  Class <xsl:value-of select="@name"/>
	</h2>
      </padded>
      <macro:maybe-columns
	 test="see-also
	       or //class-definition[@id=current()//superclass/@id]
	       //see-also/slot">
	<padded>
	  <h3>Superclasses</h3>
	  <div class="indent">
	    <xsl:for-each select="cpl/superclass">
	      <xsl:call-template name="class-list"/>
	    </xsl:for-each>
	  </div>
	  <h3>Documented Subclasses</h3>
	  <div class="indent">
	    <xsl:choose>
	      <xsl:when test="subclasses/subclass">
		<xsl:for-each select="subclasses/subclass">
		  <xsl:sort select="@id" data-type="text" order="ascending"/>
		  <xsl:call-template name="class-list"/>
		</xsl:for-each>
	      </xsl:when>
	      <xsl:otherwise>
		None
	      </xsl:otherwise>
	    </xsl:choose>
	  </div>
	  <xsl:if test="direct-slots">
	    <h3>Direct Slots</h3>
	    <div class="indent">
	      <xsl:choose>
		<xsl:when test="direct-slots/slot">
		  <xsl:for-each select="direct-slots/slot">
		    <xsl:sort select="@id" data-type="text" order="ascending"/>
		    <xsl:call-template name="slot-list"/>
		  </xsl:for-each>
		</xsl:when>
		<xsl:otherwise>
		  None
		</xsl:otherwise>
	      </xsl:choose>
	    </div>
	  </xsl:if>
	  <xsl:call-template name="main-left"/>
	</padded>
      </macro:maybe-columns>
    </page>
  </xsl:template>

  <xsl:template match="function-definition">
    <page base="../"
	  pathname="pages/{@id}.html"
	  title="Function {@name}">
      <padded>
	<p class="noindent">
	  Package:
	  <a href="{../../@id}.html">
	    <xsl:value-of select="../../@name"/>
	  </a>
	</p>
	<h2 class="page-title">
	  Function
	  <xsl:value-of select="@name"/>
	</h2>
      </padded>
      <macro:maybe-columns test="see-also">
	<padded>
          <h3>Lambda List</h3>
	  <div class="indent">
	    <xsl:apply-templates select="lambda-list"/>
	  </div>
	  <xsl:apply-templates select="arguments"/>
	  <xsl:apply-templates select="return"/>
	  <xsl:call-template name="main-left"/>
	</padded>
      </macro:maybe-columns>
    </page>
  </xsl:template>

  <xsl:template match="macro-definition">
    <page base="../"
	  pathname="pages/{@id}.html"
	  title="Macro {@name}">
      <padded>
	<p class="noindent">
	  Package:
	  <a href="{../../@id}.html">
	    <xsl:value-of select="../../@name"/>
	  </a>
	</p>
	<h2 class="page-title">
	  Macro
	  <xsl:value-of select="@name"/>
	</h2>
      </padded>
      <macro:maybe-columns test="see-also">
	<padded>
          <h3>Lambda List</h3>
	  <div class="indent">
	    <xsl:apply-templates select="lambda-list"/>
	  </div>
	  <xsl:apply-templates select="arguments"/>
	  <xsl:apply-templates select="return"/>
	  <xsl:call-template name="main-left"/>
	</padded>
      </macro:maybe-columns>
    </page>
  </xsl:template>

  <xsl:template match="type-definition">
    <page base="../"
	  pathname="pages/{@id}.html"
	  title="Type {@name}">
      <padded>
	<p class="noindent">
	  Package:
	  <a href="{../../@id}.html">
	    <xsl:value-of select="../../@name"/>
	  </a>
	</p>
	<h2 class="page-title">
	  Type
	  <xsl:value-of select="@name"/>
	</h2>
	<xsl:call-template name="main"/>
      </padded>
    </page>
  </xsl:template>

  <xsl:template match="variable-definition">
    <page base="../"
	  pathname="pages/{@id}.html"
	  title="Variable {@name}">
      <padded>
	<p class="noindent">
	  Package:
	  <a href="{../../@id}.html">
	    <xsl:value-of select="../../@name"/>
	  </a>
	</p>
	<h2 class="page-title">
	  Variable
	  <xsl:value-of select="@name"/>
	</h2>
	<xsl:call-template name="main"/>
      </padded>
    </page>
  </xsl:template>


  <!--
      Other templates
    -->

  <xsl:template match="arguments">
    <div class="sph3">Arguments:</div>
    <ul>
      <xsl:for-each select="arg">
	<li>
	  <tt>
	    <xsl:value-of select="@arg"/>
	  </tt>
	  <xsl:text> -- </xsl:text>
	  <xsl:apply-templates/>
	</li>
      </xsl:for-each>
    </ul>
  </xsl:template>

  <xsl:template name="main-documentation-string">
    <xsl:choose>
      <xsl:when test="documentation-string">
	<div class="sph3">Details:</div>
	<xsl:apply-templates select="documentation-string"/>
      </xsl:when>
      <xsl:otherwise>
	<p style="color: red; font-weight: bold">
	  No documentation string.  Possibly unimplemented or incomplete.
	</p>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates select="implementation-note"/>
  </xsl:template>

  <xsl:template name="main-right">
    <xsl:if test="see-also/constructor">
      <h3>Returned by</h3>
      <div class="indent">
	<simple-table>
	  <xsl:apply-templates select="see-also/constructor/see"/>
	</simple-table>
      </div>
    </xsl:if>
    <xsl:if test="see-also/slot">
      <h3>Slot Access Functions</h3>
      <div class="indent">
	<simple-table>
	  <xsl:apply-templates select="see-also/slot/see"/>
	</simple-table>
      </div>
    </xsl:if>
    <xsl:if
       test="//class-definition[@id=current()//superclass/@id]
	     //see-also
	     /slot">
      <h3>Inherited Slot Access Functions</h3>
      <div class="indent">
	<simple-table>
	  <xsl:apply-templates
	     select="//class-definition[@id=current()//superclass/@id]
		     //see-also/slot/see"/>
	</simple-table>
      </div>
    </xsl:if>
    <xsl:if test="see-also/other|see-also/auto">
      <h3>See also</h3>
      <div class="indent">
	<simple-table>
	  <xsl:for-each select="see-also/other/see|see-also/auto/see">
	    <xsl:variable name="name" select="text()"/>
	    <xsl:if test="not(preceding-sibling::see[text() = $name])">
	      <xsl:apply-templates select="."/>
	    </xsl:if>
	  </xsl:for-each>
	</simple-table>
      </div>
    </xsl:if>
  </xsl:template>

  <xsl:template name="main">
    <macro:maybe-columns test="see-also">
      <xsl:call-template name="main-left"/>
    </macro:maybe-columns>
  </xsl:template>

  <xsl:template name="class-list">
    <xsl:if test="position() != 1">
      <xsl:text>, </xsl:text>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@id">
	<a href="{@id}.html">
	  <tt>
	    <xsl:if test="@status = 'INTERNAL'">
	      <xsl:value-of select="@package"/>
	      <xsl:text>::</xsl:text>
	    </xsl:if>
	    <xsl:value-of select="@name"/>
	  </tt>
	</a>
      </xsl:when>
      <xsl:when test="@status = 'INTERNAL'">
	<tt style="color: #777777">
	  <xsl:value-of select="@package"/>
	  <xsl:text>::</xsl:text>
	  <xsl:value-of select="@name"/>
	</tt>
      </xsl:when>	  
      <xsl:otherwise>
	<tt style="color: #777777">
	  <xsl:value-of select="@package"/>
	  <xsl:text>:</xsl:text>
	  <xsl:value-of select="@name"/>
	</tt>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="lambda-list">
    <tt><xsl:value-of select="../@name"/></tt>
    <xsl:text> (</xsl:text>
    <xsl:for-each select="elt">
      <xsl:if test="position() != 1">
	<xsl:text>&#160;</xsl:text>
      </xsl:if>
      <b><xsl:value-of select="text()"/></b>
    </xsl:for-each>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template name="slot-list">
    <div class="indent">
      <simple-table>
        <xsl:value-of select="@name"/>
	<xsl:text> -- </xsl:text>
        <xsl:choose>
          <xsl:when
            test="documentation-string/short">
            <xsl:apply-templates select="documentation-string/short"/>
	    <a href="{@id}.html#details">...</a>
	  </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates
              select="documentation-string"/>
          </xsl:otherwise>
        </xsl:choose>
      </simple-table>
    </div>
  </xsl:template>

  <xsl:template name="about-arguments">
    <xsl:param name="label"/>
    <div class="sp-lambda-list">
      <b>
	<xsl:value-of select="$label"/>
	<xsl:text> </xsl:text>
	<xsl:value-of select="../@name"/>
      </b>
      <xsl:text> (</xsl:text>
      <xsl:for-each select="elt">
	<xsl:if test="position() != 1">
	  <xsl:text>&#160;</xsl:text>
	</xsl:if>
	<xsl:value-of select="text()"/>
      </xsl:for-each>
      <xsl:text>)</xsl:text>
    </div>
  </xsl:template>

  <xsl:template match="documentation-string">
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="short">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="em">
    <i>
      <xsl:apply-templates/>
    </i>
  </xsl:template>

  <xsl:template match="b">
    <b>
      <xsl:apply-templates/>
    </b>
  </xsl:template>

  <xsl:template match="var">
    <tt>
      <xsl:apply-templates/>
    </tt>
  </xsl:template>

  <xsl:template match="code">
    <tt>
      <xsl:apply-templates/>
    </tt>
  </xsl:template>

  <xsl:template match="pre">
    <pre>
      <xsl:apply-templates/>
    </pre>
  </xsl:template>

  <xsl:template match="a">
    <a href="{@a}">
      <xsl:apply-templates/>
    </a>
  </xsl:template>

  <xsl:template match="fun">
    <a href="{@id}.html">
      <tt>
	<xsl:apply-templates/>
      </tt>
    </a>
  </xsl:template>

  <xsl:template match="class">
    <a href="{@id}.html">
      <tt>
	<xsl:apply-templates/>
      </tt>
    </a>
  </xsl:template>

  <xsl:template match="slot">
    <a href="{@id}.html">
      <tt>
	<xsl:apply-templates/>
      </tt>
    </a>
  </xsl:template>

  <xsl:template match="type">
    <a href="{@id}.html">
      <tt>
	<xsl:apply-templates/>
      </tt>
    </a>
  </xsl:template>

  <xsl:template match="variable">
    <a href="{@id}.html">
      <tt>
	<xsl:apply-templates/>
      </tt>
    </a>
  </xsl:template>

  <xsl:template match="itemize">
    <ul>
      <xsl:apply-templates/>
    </ul>
  </xsl:template>

  <xsl:template match="item">
    <li>
      <xsl:apply-templates/>
    </li>
  </xsl:template>

  <xsl:template match="see">
    <tr>
      <td>
	<a href="{@id}.html">
	  <tt>
	    <xsl:apply-templates/>
	  </tt>
	</a>
      </td>
      <xsl:if test="@see">
	<td>
	  &#160;&#160;&#160;&#160;
	  <i>
	    <xsl:value-of select="@see"/>
	  </i>
	</td>
      </xsl:if>
    </tr>
  </xsl:template>

  <xsl:template match="implementation-note">
    <h3>Implementation notes</h3>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="break">
    <br/><br/>
  </xsl:template>

  <xsl:template match="sections">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="section">
    <h3>
      <a name="{generate-id()}"/>
      <xsl:value-of select="@section"/>
    </h3>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="aboutfun">
    <xsl:variable name="fun" select="//function-definition[@name=current()]"/>
    <xsl:for-each select="$fun/lambda-list">
      <xsl:call-template name="about-arguments">
	<xsl:with-param name="label" select="'Function'"/>
      </xsl:call-template>
    </xsl:for-each>
    <xsl:for-each select="$fun">
      <div class="sp-definition">
	<div class="sp-definition-body">
	  <xsl:apply-templates select="arguments"/>
	  <div class="sph3">Returns:</div>
	  <div class="indent">
	    <xsl:apply-templates select="return/node()"/>
	  </div>
	  <xsl:call-template name="main-documentation-string"/>
	</div>
      </div>
    </xsl:for-each>
    <br/>
  </xsl:template>

  <xsl:template match="aboutmacro">
    <xsl:variable name="fun" select="text()"/>
    <xsl:for-each select="//macro-definition[@name=$fun]/lambda-list">
      <xsl:call-template name="about-arguments">
	<xsl:with-param name="label" select="'Macro'"/>
      </xsl:call-template>
    </xsl:for-each>
    <div style="margin-left: 3em">
      <xsl:choose>
	<xsl:when
	   test="//macro-definition[@name=$fun]/documentation-string//short">
	  <xsl:for-each select="//macro-definition[@name=$fun]">
	    <xsl:apply-templates select="documentation-string//short"/>
	    <xsl:text> </xsl:text>
	    <a href="{@id}.html#details">...</a>
	  </xsl:for-each>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates
	     select="//macro-definition[@name=$fun]/documentation-string"/>
	</xsl:otherwise>
      </xsl:choose>
    </div>
    <br/>
  </xsl:template>

  <xsl:template match="aboutclass">
    <xsl:variable name="name" select="text()"/>
    <xsl:for-each select="//class-definition[@name=$name]">
      <div class="def">
	<a href="{@id}.html">
	  Class
	  <xsl:value-of select="@name"/>
	</a>
      </div>
    </xsl:for-each>
    <div style="margin-left: 3em">
      <xsl:choose>
	<xsl:when
	   test="//class-definition[@name=$name]/documentation-string//short">
	  <xsl:for-each select="//class-definition[@name=$name]">
	    <xsl:apply-templates select="documentation-string//short"/>
	    <xsl:text> </xsl:text>
	    <a href="{@id}.html#details">...</a>
	  </xsl:for-each>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates
	     select="//class-definition[@name=$name]/documentation-string"/>
	</xsl:otherwise>
      </xsl:choose>
    </div>
    <br/>
  </xsl:template>

  <xsl:template name="undocumented">
    <xsl:if test="not(documentation-string)">
      <xsl:text>&#160;</xsl:text>
      <span style="color: red">
	(undocumented)
      </span>
    </xsl:if>
  </xsl:template>
</xsl:stylesheet>
