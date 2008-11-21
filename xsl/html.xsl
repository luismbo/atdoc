<!-- Hey, emacs, please consider this to be -*- xml -*-
    This is the main stylesheet.
    Input must have been cleaned up using cleanup.xsl already.

    This stylesheet does nearly all of the formatting work, but still keeps
    all data together in one big XML document.

    A <page> element is produced for each package and symbol.

    The contents of each <page> will be mostly HTML, with the exception
    of a few formatting elements like <columns> that are replaced later.

  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:macro="http://lichteblau.com/macro"
		version="1.0">
  <xsl:import href="html-common.tmp"/>

  <xsl:include href="base-uri.xsl"/>

  <xsl:output method="xml" indent="yes"/>

  <xsl:key name="id"
	   match="class-definition|function-definition|macro-definition|variable-definition"
	   use="@id"/>

  <xsl:key name="function-by-name"
	   match="function-definition|macro-definition"
	   use="@name"/>
  <xsl:key name="class-by-name"
	   match="class-definition|type-definition"
	   use="@name"/>
  <xsl:key name="variable-by-name" match="variable-definition" use="@name"/>

  <xsl:template match="/">
    <pages>
      <xsl:call-template name="copy-base-uri"/>
      <xsl:call-template name="configuration-attributes"/>
      <xsl:apply-templates select="documentation"/>
      <xsl:apply-templates select="documentation/package"/>
      <xsl:apply-templates select="documentation/package/external-symbols/*"/>
      <xsl:if test="documentation/@include-internal-symbols-p">
	<xsl:apply-templates select="documentation/package/internal-symbols/*"/>
      </xsl:if>
      <!-- <xsl:apply-templates select="documentation/package/*/class-definition/direct-slots/*"/> -->
    </pages>
  </xsl:template>


  <xsl:template name="configuration-attributes">
    <macro:copy-attribute name="logo" path="documentation"/>
    <macro:copy-attribute name="css" path="documentation"/>
    <macro:copy-attribute name="heading" path="documentation"/>
  </xsl:template>


  <!--
      page generation templates
    -->

  <xsl:template match="documentation">
    <main-page title="{@index-title}">
      <padded>
	Index of packages:
      </padded>
      
      <columns>
	<column width="60%">
	  <padded>
	    <xsl:for-each select="package">
	      <xsl:variable name="url"
			    select="concat('pages/', @id, '.html')"/>
	      <h2 class="page-title">
		<a href="{$url}">
		  Package
		  <xsl:value-of select="@name"/>
		</a>
	      </h2>
	      <div style="left: 100px">
		<xsl:apply-templates select="documentation-string"/>
		<div class="indent">
		  <xsl:if test="sections">
		    <p><i>About this package:</i></p>
		    <ul>
		      <xsl:for-each select="sections/section">
			<li>
			  <a href="{$url}#{generate-id()}">
			    <xsl:value-of select="@section"/>
			  </a>
			</li>
		      </xsl:for-each>
		    </ul>
		  </xsl:if>
		</div>
	      </div>
	    </xsl:for-each>
	  </padded>
	</column>
	<column>
	  <h3><a name="index"></a>Exported Symbol Index</h3>
	  <simple-table>
	    <xsl:apply-templates select="package/external-symbols/*"
				 mode="symbol-index">
	      <xsl:sort select="@name" data-type="text" order="ascending"/>
	      <xsl:with-param name="packagep" select="'pages/'"/>
	    </xsl:apply-templates>
	  </simple-table>
	</column>
      </columns>
    </main-page>
  </xsl:template>

  <xsl:template match="package">
    <page base="../"
	  pathname="pages/{@id}.html"
	  title="Package {@name}">
      <padded>
	<xsl:if test="count(../package) > 1">
	  <p class="noindent">
	    Up:
	    <a href="../index.html">
	      <xsl:value-of select="/documentation/@index-title"/>
	    </a>
	  </p>
	</xsl:if>
	<h1>
	  Package
	  <xsl:value-of select="@name"/>
	</h1>
	<xsl:apply-templates select="documentation-string"/>
      </padded>
      <columns>
	<column width="60%">
	  <padded>
	    <xsl:if test="sections">
	      <div style="margin-left: -30px">
		<h3>About This Package</h3>
	      </div>
	      <xsl:for-each select="sections/section">
		<a href="#{generate-id()}" style="font-weight: bold">
		  <xsl:value-of select="@section"/>
		</a>
		<br/>
	      </xsl:for-each>
	      <br/>
	      <xsl:apply-templates select="sections"/>
	    </xsl:if>
	  </padded>
	</column>
	<column>
	  <h3><a name="index"></a>Exported Symbol Index</h3>
	  <xsl:apply-templates select="external-symbols" mode="symbol-index"/>
	</column>
      </columns>
    </page>
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
	 test="see-also or key('id', .//superclass/@id)//see-also/slot">
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
    <h3>Arguments</h3>
    <div class="indent">
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
    </div>
  </xsl:template>

  <xsl:template name="main-left">
    <xsl:choose>
      <xsl:when test="documentation-string">
	<h3>Details<a name="details"/></h3>
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
    <xsl:if test="see-also/condition">
      <h3>Condition Types Signalled</h3>
      <div class="indent">
	<simple-table>
	  <xsl:apply-templates select="see-also/condition/see"/>
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
       test="key('id', .//superclass/@id)//see-also/slot">
      <h3>Inherited Slot Access Functions</h3>
      <div class="indent">
	<simple-table>
	  <xsl:apply-templates
	     select="key('id', .//superclass/@id)//see-also/slot/see"/>
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
    <div class="def">
      <a href="{../@id}.html">
	<xsl:value-of select="$label"/>
	<xsl:text> </xsl:text>
	<xsl:value-of select="../@name"/>
	<xsl:text> (</xsl:text>
	<xsl:for-each select="elt">
	  <xsl:if test="position() != 1">
	    <xsl:text>&#160;</xsl:text>
	  </xsl:if>
	  <xsl:value-of select="text()"/>
	</xsl:for-each>
	<xsl:text>)</xsl:text>
      </a>
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

  <xsl:template match="return">
    <h3>Return Value</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="implementation-note">
    <h3>Implementation notes</h3>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="break">
    <br/><br/>
  </xsl:template>

  <xsl:template match="sections">
    <xsl:for-each select="section">
      <h2>
	<a name="{generate-id()}"/>
	<xsl:value-of select="@section"/>
      </h2>
      <xsl:apply-templates/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="aboutfun">
    <xsl:for-each select="key('function-by-name', text())">
      <xsl:for-each select="lambda-list">
	<xsl:call-template name="about-arguments">
	  <xsl:with-param name="label" select="'Function'"/>
	</xsl:call-template>
      </xsl:for-each>
      <div style="margin-left: 3em">
	<xsl:choose>
	  <xsl:when test="documentation-string//short">
	    <xsl:apply-templates select="documentation-string//short"/>
	    <xsl:text> </xsl:text>
	    <a href="{@id}.html#details">...</a>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:apply-templates select="documentation-string"/>
	  </xsl:otherwise>
	</xsl:choose>
      </div>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="aboutmacro">
    <xsl:for-each select="key('function-by-name', text())">
      <xsl:for-each select="lambda-list">
	<xsl:call-template name="about-arguments">
	  <xsl:with-param name="label" select="'Macro'"/>
	</xsl:call-template>
      </xsl:for-each>
      <div style="margin-left: 3em">
	<xsl:choose>
	  <xsl:when test="documentation-string//short">
	    <xsl:apply-templates select="documentation-string//short"/>
	    <xsl:text> </xsl:text>
	    <a href="{@id}.html#details">...</a>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:apply-templates select="documentation-string"/>
	  </xsl:otherwise>
	</xsl:choose>
      </div>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="aboutclass">
    <xsl:for-each select="key('class-by-name', text())">
      <div class="def">
	<a href="{@id}.html">
	  Class
	  <xsl:value-of select="@name"/>
	</a>
      </div>
      <div style="margin-left: 3em">
	<xsl:choose>
	  <xsl:when test="documentation-string//short">
	    <xsl:apply-templates select="documentation-string//short"/>
	    <xsl:text> </xsl:text>
	    <a href="{@id}.html#details">...</a>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:apply-templates select="documentation-string"/>
	  </xsl:otherwise>
	</xsl:choose>
      </div>
      <br/>
    </xsl:for-each>
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
