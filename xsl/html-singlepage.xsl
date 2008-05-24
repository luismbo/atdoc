<!-- Hey, emacs, please consider this to be -*- xml -*-

    This is an alternative to html.xsl for single-page output.
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:macro="http://lichteblau.com/macro"
		version="1.0">
  <xsl:import href="html-common.tmp"/>

  <xsl:include href="base-uri.xsl"/>

  <xsl:output method="xml" indent="yes"/>

  <xsl:key name="aboutfun" match="aboutfun" use="string(.)"/>
  <xsl:key name="aboutclass" match="aboutclass" use="string(.)"/>
  <xsl:key name="aboutmacro" match="aboutmacro" use="string(.)"/>

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

      <xsl:if test="package/sections">
	<h3>Contents</h3>
	<div class="indent">
	  <ul>
	    <xsl:for-each select="package">
	      <li>
		Package <xsl:value-of select="@name"/>
		<ul>
		  <xsl:for-each select="sections/section">
		    <li>
		      <a href="#{generate-id()}">
			<xsl:value-of select="@section"/>
		      </a>
		    </li>
		  </xsl:for-each>
		</ul>
	      </li>
	    </xsl:for-each>
	  </ul>
	</div>
      </xsl:if>

      <xsl:apply-templates select="package"/>

      <h3><a name="index"></a>Index of exported symbols</h3>
      <simple-table>
	<xsl:apply-templates select="package/external-symbols/*"
			     mode="symbol-index">
	  <xsl:sort select="@name" data-type="text" order="ascending"/>
	  <xsl:with-param name="packagep" select="'pages/'"/>
	</xsl:apply-templates>
      </simple-table>
      <div style="height: 10em"/>
    </main-page>
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
    <xsl:if test="see-also/constructor">
      <div class="sph3">Returned by:</div>
      <div>
 	<ul>
	  <xsl:apply-templates select="see-also/constructor/see"/>
	</ul>
      </div>
    </xsl:if>
    <xsl:if test="see-also/slot">
      <div class="sph3">Slot Access Functions:</div>
      <div>
	<ul>
	  <xsl:apply-templates select="see-also/slot/see"/>
	</ul>
      </div>
    </xsl:if>
    <xsl:variable name="inherited-slots"
		  select="key('id', current()//superclass/@id)//see-also/slot"
		  />
    <xsl:if test="$inherited-slots">
      <div class="sph3">Inherited Slot Access Functions:</div>
      <div>
	<ul>
	  <xsl:apply-templates select="$inherited-slots/see"/>
	</ul>
      </div>
    </xsl:if>
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
    <xsl:if test="see-also/other|see-also/auto">
      <div class="sph3">See also:</div>
      <div>
	<ul>
	  <xsl:for-each select="see-also/other/see|see-also/auto/see">
	    <xsl:variable name="name" select="text()"/>
	    <xsl:if test="not(preceding-sibling::see[text() = $name])">
	      <xsl:apply-templates select="."/>
	    </xsl:if>
	  </xsl:for-each>
	</ul>
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
	<a href="#{@id}">
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
	    <a href="#{@id}">...</a>
	  </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates
              select="documentation-string"/>
          </xsl:otherwise>
        </xsl:choose>
      </simple-table>
    </div>
  </xsl:template>

  <xsl:template name="definition">
    <xsl:param name="label"/>
    <a name="{@id}"/>
    <div class="sp-lambda-list">
      <b>
	<xsl:value-of select="$label"/>
	<xsl:text> </xsl:text>
	<xsl:value-of select="@name"/>
      </b>
      <xsl:if test="lambda-list">
	<xsl:text> (</xsl:text>
	<xsl:for-each select="lambda-list/elt">
	  <xsl:if test="position() != 1">
	    <xsl:text>&#160;</xsl:text>
	  </xsl:if>
	  <xsl:value-of select="text()"/>
	</xsl:for-each>
	<xsl:text>)</xsl:text>
      </xsl:if>
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
    <a href="#{@id}">
      <tt>
	<xsl:apply-templates/>
      </tt>
    </a>
  </xsl:template>

  <xsl:template match="class">
    <a href="#{@id}">
      <tt>
	<xsl:apply-templates/>
      </tt>
    </a>
  </xsl:template>

  <xsl:template match="slot">
    <a href="#{@id}">
      <tt>
	<xsl:apply-templates/>
      </tt>
    </a>
  </xsl:template>

  <xsl:template match="type">
    <a href="#{@id}">
      <tt>
	<xsl:apply-templates/>
      </tt>
    </a>
  </xsl:template>

  <xsl:template match="variable">
    <a href="#{@id}">
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
    <li>
      <a href="#{@id}">
	<tt>
	  <xsl:apply-templates/>
	</tt>
      </a>
      <xsl:if test="@see">
	&#160;
	<i>
	  <xsl:value-of select="@see"/>
	</i>
      </xsl:if>
    </li>
  </xsl:template>

  <xsl:template match="implementation-note">
    <div class="sph3">Implementation notes:</div>
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
    <xsl:apply-templates select="key('function-by-name', current())"/>
  </xsl:template>

  <xsl:template match="aboutmacro">
    <xsl:apply-templates select="key('function-by-name', current())"/>
  </xsl:template>

  <xsl:template match="aboutclass">
    <xsl:apply-templates select="key('class-by-name', current())"/>
  </xsl:template>

  <xsl:template match="function-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Function'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
	<xsl:apply-templates select="arguments"/>
	<xsl:if test="return">
	  <div class="sph3">Returns:</div>
	  <div class="indent">
	    <xsl:apply-templates select="return/node()"/>
	  </div>
	</xsl:if>
	<xsl:call-template name="main-documentation-string"/>
      </div>
    </div>
    <br/>
  </xsl:template>

  <xsl:template match="macro-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Macro'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
	<xsl:apply-templates select="arguments"/>
	<xsl:if test="return">
	  <div class="sph3">Returns:</div>
	  <div class="indent">
	    <xsl:apply-templates select="return/node()"/>
	  </div>
	</xsl:if>
	<xsl:call-template name="main-documentation-string"/>
      </div>
    </div>
    <br/>
  </xsl:template>

  <xsl:template match="class-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Class'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
	<div class="sph3">Superclasses:</div>
	<div class="indent">
	  <xsl:for-each select="cpl/superclass">
	    <xsl:call-template name="class-list"/>
	  </xsl:for-each>
	</div>
	<div class="sph3">Documented Subclasses:</div>
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
	  <div class="sph3">Direct Slots:</div>
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
	<xsl:call-template name="main-documentation-string"/>
      </div>
    </div>
    <br/>
  </xsl:template>

  <xsl:template match="type-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Type'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
	<xsl:call-template name="main-documentation-string"/>
      </div>
    </div>
    <br/>
  </xsl:template>

  <xsl:template match="variable-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Variable'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
	<xsl:call-template name="main-documentation-string"/>
      </div>
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

  <xsl:template match="package">
    <xsl:apply-templates select="sections/section"/>
    <xsl:variable name="unreferenced"
		  select="external-symbols/function-definition[
			  count(key('aboutfun',@name))=0
			  ]"/>
    <xsl:if test="$unreferenced">
      <h3>Other functions in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced">
	<xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>

    <xsl:variable name="unreferenced2"
		  select="external-symbols/macro-definition[
			  count(key('aboutmacro',@name))=0
			  ]"/>
    <xsl:if test="$unreferenced2">
      <h3>Other macros in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced2">
	<xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>

    <xsl:variable name="unreferenced3"
		  select="external-symbols/class-definition[
			  count(key('aboutclass',@name))=0
			  ]"/>
    <xsl:if test="$unreferenced3">
      <h3>Other classes in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced3">
	<xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>

    <xsl:variable name="unreferenced4"
		  select="external-symbols/variable-definition"/>
    <xsl:if test="$unreferenced4">
      <h3>Other variables in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced4">
	<xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>
  </xsl:template>
</xsl:stylesheet>
