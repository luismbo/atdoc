<!-- Hey, emacs, please consider this to be -*- xml -*-

    First of two steps for .info file generation.  This stylesheet
    outputs a simple XML syntax, which is turned then into the actual .info
    file by info-paginate.xsl.
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:macro="http://lichteblau.com/macro"
		xmlns:atdoc="http://www.lichteblau.com/atdoc/"
		extension-element-prefixes="atdoc"
		version="1.0">
  <xsl:include href="base-uri.xsl"/>

  <xsl:key name="aboutfun" match="aboutfun" use="string(.)"/>
  <xsl:key name="aboutclass" match="aboutclass" use="string(.)"/>
  <xsl:key name="aboutmacro" match="aboutmacro" use="string(.)"/>

  <xsl:key name="id" 
	   match="function-definition|class-definition|macro-definition|variable-definition"
	   use="@id"/>

  <xsl:key name="function-by-name"
	   match="function-definition|macro-definition"
	   use="@name"/>
  <xsl:key name="class-by-name"
	   match="class-definition|type-definition"
	   use="@name"/>
  <xsl:key name="variable-by-name" match="variable-definition" use="@name"/>

  <xsl:template match="/">
    <document filename="{/documentation/@name}.info">
      <xsl:call-template name="copy-base-uri"/>
      <xsl:apply-templates select="documentation"/>
    </document>
  </xsl:template>    

  <xsl:template match="documentation">
    <p>
      <atdoc:avt>
	This is {@name}.info, generated by atdoc on {atdoc:date()} from
	the following Common Lisp packages:
      </atdoc:avt>
      <xsl:for-each select="package">
	<xsl:if test="position() != 1">, </xsl:if>
	<xsl:value-of select="@name"/>
      </xsl:for-each>
    </p>

    <node name="Top" title="{@title}">
      <xsl:choose>
	<xsl:when test="count(package) > 1">
	  <p>
	    This manual documents the following packages:
	  </p>
	  <xsl:for-each select="package">
	    <p>
	      <atdoc:avt>
		{@name}:
	      </atdoc:avt>
	      <xsl:apply-templates select="documentation-string"/>
	    </p>
	  </xsl:for-each>
	</xsl:when>
	<xsl:otherwise>
	  <p>
	    <xsl:apply-templates select="documentation-string"/>
	  </p>
	</xsl:otherwise>
      </xsl:choose>

      <menu>
	<xsl:for-each select="package">
	  <menu-item node="Package {@name}"
		     label="{documentation-string}"/>
	</xsl:for-each>
      </menu>

      <xsl:apply-templates select="package"/>
    </node>

    <node name="Symbol Index" Title="Index of exported symbols">
      <menu>
	<xsl:apply-templates select="package/external-symbols/*"
			     mode="symbol-index">
	  <xsl:sort select="@name" data-type="text" order="ascending"/>
	  <xsl:with-param name="packagep" select="'pages/'"/>
	</xsl:apply-templates>
      </menu>
    </node>
  </xsl:template>

  <xsl:template match="arguments">
    <div indent="5">
      Arguments:
    </div>
    <xsl:for-each select="arg">
      <div indent="7">
	<xsl:value-of select="@arg"/>
	--- <xsl:apply-templates/>
      </div>
    </xsl:for-each>
    <div/>
  </xsl:template>

  <xsl:template name="main-documentation-string">
    <xsl:if test="see-also/constructor">
      <p indent="5">
	Returned by:
	<xsl:apply-templates select="see-also/constructor/see"/>
      </p>
    </xsl:if>
    <xsl:if test="see-also/condition">
      <p indent="5">
	Condition Types Signalled:
	<xsl:apply-templates select="see-also/condition/see"/>
      </p>
    </xsl:if>
    <xsl:if test="see-also/slot">
      <p indent="5">
	Slot Access Functions
      </p>
      <xsl:for-each select="see-also/slot/see">
	<div indent="7">
	  <xsl:apply-templates select="."/>
	</div>
      </xsl:for-each>
      <div/>
    </xsl:if>
    <xsl:variable name="inherited-slots"
		  select="key('id', current()//superclass/@id)//see-also/slot"
		  />
    <xsl:if test="$inherited-slots">
      <p indent="5">
	Inherited Slot Access Functions
      </p>
      <xsl:for-each select="$inherited-slots">
	<div indent="7">
	  <xsl:apply-templates select="."/>
	</div>	
      </xsl:for-each>
      <div/>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="documentation-string">
	<p indent="5">
	  Details:
	</p>
	<indent indent="7">
	  <xsl:apply-templates select="documentation-string"/>
	</indent>
      </xsl:when>
      <xsl:otherwise>
	<p indent="5">
	  No documentation string.  Possibly unimplemented or incomplete.
	</p>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates select="implementation-note"/>
    <xsl:if test="see-also/other|see-also/auto">
      <p indent="5">
	See also:
      </p>
      <xsl:for-each select="see-also/other/see|see-also/auto/see">
	<xsl:variable name="name" select="text()"/>
	<xsl:if test="not(preceding-sibling::see[text() = $name])">
	  <div indent="7">
	    <xsl:apply-templates select="."/>
	  </div>
	</xsl:if>
      </xsl:for-each>
      <div/>
    </xsl:if>
  </xsl:template>

  <xsl:template name="class-list">
    <div indent="7">
      <xsl:choose>
	<xsl:when test="@status = 'INTERNAL'">
	  <atdoc:avt>{@package}::{@name}</atdoc:avt>
	</xsl:when>
	<xsl:otherwise>
	  <atdoc:avt>{@package}:{@name}</atdoc:avt>
	</xsl:otherwise>
      </xsl:choose>

      <xsl:if test="@id">
	<xsl:text> (</xsl:text>
	<xsl:call-template name="note"/>
	<xsl:text>)</xsl:text>
      </xsl:if>
    </div>
  </xsl:template>

  <xsl:template name="definition">
    <xsl:param name="label"/>
    <p indent="10" initial-indent="0">
      <atdoc:avt> -- {$label}: {@name}</atdoc:avt>
      <xsl:apply-templates select="lambda-list"/>
    </p>
  </xsl:template>

  <xsl:template match="lambda-list">
    <xsl:text> (</xsl:text>
    <xsl:for-each select="elt">
      <xsl:if test="position() != 1">
	<xsl:text> </xsl:text>
      </xsl:if>
      <b><xsl:value-of select="text()"/></b>
    </xsl:for-each>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template name="slot-list">
    <xsl:value-of select="@name"/>
    <xsl:text> --- </xsl:text>
    <xsl:apply-templates select="documentation-string"/>
    <xsl:text>&#10;&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="documentation-string">
    <xsl:call-template name="process-paragraphs"/>
  </xsl:template>

  <xsl:template match="short">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="em|b|var|code">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="pre">
    <pre>
      <xsl:apply-templates/>
    </pre>
  </xsl:template>

  <xsl:template match="a">
    <xsl:apply-templates/>
    <atdoc:avt> ({@a})</atdoc:avt>
  </xsl:template>

  <xsl:template match="fun|class|slot|type|variable">
    <xsl:apply-templates/>
    <xsl:text> (</xsl:text>
    <xsl:call-template name="note"/>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template name="note">
    <xsl:variable name="target" select="key('id', @id)"/>

    <xsl:variable name="parent">
      <xsl:choose>
	<xsl:when test="$target/ancestor::section">
	  <xsl:value-of select="$target/ancestor::section/@section"/>
	</xsl:when>

	<xsl:otherwise>
	  <xsl:text>Package </xsl:text>
	  <xsl:value-of select="$target/ancestor::package/@name"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    
    <atdoc:avt>*Note {$parent}::</atdoc:avt>
  </xsl:template>

  <xsl:template match="itemize">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="item">
    <xsl:text> * </xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="see">
    <xsl:text> * </xsl:text>
    <xsl:apply-templates/>
    
    <xsl:text> (</xsl:text>
    <xsl:call-template name="note"/>
    <xsl:text>)</xsl:text>

    <xsl:if test="@see">
      <atdoc:vat> -- {@see}</atdoc:vat>
    </xsl:if>
  </xsl:template>

  <xsl:template match="implementation-note">
    <xsl:text>Implementation notes:</xsl:text>
    <xsl:text>  </xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="break">
    <xsl:message>unexpected break</xsl:message>
  </xsl:template>

  <xsl:template match="package">
    <node name="Package {@name}">
      <xsl:apply-templates select="documentation-string"/>
      <xsl:if test="sections/section">
	<menu>
	  <xsl:for-each select="sections/section">
	    <menu-item node="{@section}" label="{documentation-string}"/>
	  </xsl:for-each>
	</menu>
      </xsl:if>
      <xsl:apply-templates select="sections/section"/>
    </node>
  </xsl:template>

  <xsl:template match="section">
    <node name="{@section}">
      <xsl:if test="aboutfun|aboutmacro|aboutclass">
	<p>
	  In this section:
	</p>
	<xsl:for-each select="aboutfun|aboutmacro|aboutclass">
	  <atdoc:avt>  * {string()}&#10;</atdoc:avt>
	</xsl:for-each>
	<xsl:text>&#10;&#10;</xsl:text>
      </xsl:if>
      <xsl:if test="section">
	<menu>
	  <xsl:for-each select="section">
	    <menu-item node="{@section}" label="{documentation-string}"/>
	  </xsl:for-each>
	</menu>
      </xsl:if>
      <xsl:call-template name="process-paragraphs"/>
    </node>
  </xsl:template>

  <xsl:template match="return">
    <p indent="5">
      <xsl:text>Return Values:</xsl:text>
      <xsl:text>  </xsl:text>
      <xsl:apply-templates/>
    </p>
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
      <xsl:with-param name="label">Function</xsl:with-param>
    </xsl:call-template>
    <xsl:apply-templates select="arguments"/>
    <xsl:apply-templates select="return"/>
    <xsl:call-template name="main-documentation-string"/>
  </xsl:template>

  <xsl:template match="macro-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label">Macro</xsl:with-param>
    </xsl:call-template>
    <xsl:apply-templates select="arguments"/>
    <xsl:apply-templates select="return"/>
    <xsl:call-template name="main-documentation-string"/>
  </xsl:template>

  <xsl:template match="class-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label">Class</xsl:with-param>
    </xsl:call-template>

    <p indent="5">
      Superclasses: 
    </p>
    <xsl:for-each select="cpl/superclass">
      <xsl:call-template name="class-list"/>
    </xsl:for-each>
    <div/>

    <p indent="5">
      Documented Subclasses:
    </p>
    <xsl:choose>
      <xsl:when test="subclasses/subclass">
	<xsl:for-each select="subclasses/subclass">
	  <xsl:sort select="@id" data-type="text" order="ascending"/>
	  <xsl:call-template name="class-list"/>
	</xsl:for-each>
	<div/>
      </xsl:when>
      <xsl:otherwise>
	<p indent="7">None</p>
      </xsl:otherwise>
    </xsl:choose>
    
    <xsl:if test="direct-slots">
      <p indent="5">
	Direct Slots:
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
      </p>
    </xsl:if>
    <xsl:call-template name="main-documentation-string"/>
  </xsl:template>

  <xsl:template match="type-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label">Type</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="main-documentation-string"/>
  </xsl:template>

  <xsl:template match="variable-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label">Variable</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="main-documentation-string"/>
  </xsl:template>

  <xsl:template name="undocumented">
    <xsl:if test="not(documentation-string)">
      <xsl:text>(undocumented)</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template name="process-paragraphs">
    <xsl:apply-templates select="(*|node()[1])" mode="process-paragraph-node"/>
  </xsl:template>

  <xsl:template match="break" mode="process-paragraph-node" priority="2">
    <xsl:for-each select="following-sibling::node()[1]">
      <xsl:apply-templates select="." mode="maybe-paragraph"/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="*" mode="process-paragraph-node" priority="1">
    <xsl:variable name="inlinep">
      <xsl:apply-templates select="." mode="inlinep"/>
    </xsl:variable>

    <xsl:choose>
      <xsl:when test="$inlinep">
	<xsl:if test="position() = 1">
	  <p>
	    <xsl:apply-templates select="." mode="walk-paragraph"/>
	  </p>
	</xsl:if>
      </xsl:when>

      <xsl:otherwise>
	<xsl:apply-templates select="."/>
	<xsl:for-each select="following-sibling::node()[1]">
	  <xsl:apply-templates select="." mode="maybe-paragraph"/>
	</xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="node()" mode="process-paragraph-node">
    <p>
      <xsl:apply-templates select="." mode="walk-paragraph"/>
    </p>
  </xsl:template>

  <xsl:template priority="1" match="node()" mode="inlinep">
    <xsl:text>true</xsl:text>
  </xsl:template>

  <!-- block-level elements that break paragraphs:
       pre
       itemize
       section
       aboutfun
       aboutmacro
       aboutclass
       -->
  <xsl:template priority="2" match="*" mode="inlinep"/>

  <xsl:template priority="3"
		match="short|em|b|var|code|a|fun|class|slot|type|variable"
		mode="inlinep">
    <xsl:text>true</xsl:text>
  </xsl:template>

  <!--
      text nodes and inline elements are part of the paragraph:
    -->
  <xsl:template match="node()" mode="walk-paragraph">
    <xsl:variable name="inlinep">
      <xsl:apply-templates select="." mode="inlinep"/>
    </xsl:variable>
    <xsl:if test="$inlinep">
      <xsl:apply-templates select="."/>
      <xsl:apply-templates select="following-sibling::node()[1]"
			   mode="walk-paragraph"/>
    </xsl:if>
  </xsl:template>

  <xsl:template match="node()" mode="maybe-paragraph">
    <xsl:variable name="inlinep">
      <xsl:apply-templates select="." mode="inlinep"/>
    </xsl:variable>
    <xsl:if test="$inlinep">
      <p>
	<xsl:apply-templates select="." mode="walk-paragraph"/>
      </p>
    </xsl:if>
  </xsl:template>
</xsl:stylesheet>
