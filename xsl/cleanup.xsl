<!--  hey, emacs, treat this file as -*- xml -*-, please
    The first stylesheet to be used, this file cleans up the structure
    of the Lisp-generated XML file by extracting elements from all
    docstrings into their parent elements, so that only the textual
    description remains in the <documentation-string>.

    Example (input):

    <class-definition name="foo">
      <cpl>...</cpl>

      <documentation-string>
	The foo class.
	<see-slot id="foo">See also the foo function.</see-slot>
	Beware bugs.
      </documentation-string>
    </class-definition>

    Output:

    <class-definition name="foo">
      <cpl>...</cpl>

      <see-also>
	<slot>
	  <see id="foo">See also the foo function.</see>
	</slot>
      </see-also>

      <documentation-string>
	The foo class.
	Beware bugs.
      </documentation-string>
    </class-definition>

  -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:include href="base-uri.xsl"/>

  <xsl:output method="xml" indent="yes"/>

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="/*">
    <xsl:copy>
      <xsl:call-template name="copy-base-uri"/>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="see"/>
  <xsl:template match="see-slot"/>
  <xsl:template match="see-constructor"/>
  <xsl:template match="see-condition"/>
  <xsl:template match="arg"/>
  <xsl:template match="return"/>
  <xsl:template match="implementation-note"/>
  <xsl:template match="section"/>

  <xsl:template mode="extract" match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template mode="extract" match="see-slot|slot">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template mode="extract" match="see-constructor">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template mode="extract" match="see-condition">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template mode="auto-see" match="class|fun|variable">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates>
	<xsl:sort select="@name" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </see>
  </xsl:template>

  <xsl:template match="class-definition
		       |function-definition
                       |type-definition
		       |variable-definition">
    <xsl:if test="not(.//unexport)">
      <xsl:copy>
	<xsl:apply-templates select="@*|node()"/>
      </xsl:copy>
    </xsl:if>
  </xsl:template>

  <xsl:template match="documentation-string">
    <xsl:if test=".//arg">
      <arguments>
	<xsl:apply-templates mode="extract" select=".//arg"/>
      </arguments>
    </xsl:if>

    <xsl:if test=".//section">
      <sections>
	<xsl:apply-templates mode="extract" select=".//section"/>
      </sections>
    </xsl:if>

    <xsl:if test=".//see or .//see-slot or .//see-constructor
		  or .//class or .//fun or .//variable or .//slot
		  or .//see-condition">
      <see-also>
	<xsl:if test=".//class or .//fun or .//variable">
	  <auto>
	    <xsl:apply-templates mode="auto-see"
				 select=".//class|.//fun|.//variable"/>
	  </auto>
	</xsl:if>

	<xsl:if test=".//see">
	  <other>
	    <xsl:apply-templates mode="extract" select=".//see"/>
	  </other>
	</xsl:if>

	<xsl:if test=".//see-slot or .//slot">
	  <slot>
	    <xsl:apply-templates mode="extract" select=".//see-slot|.//slot"/>
	  </slot>
	</xsl:if>

	<xsl:if test=".//see-constructor">
	  <constructor>
	    <xsl:apply-templates mode="extract" select=".//see-constructor"/>
	  </constructor>
	</xsl:if>
	<xsl:if test=".//see-condition">
	  <condition>
	    <xsl:apply-templates mode="extract" select=".//see-condition"/>
	  </condition>
	</xsl:if>
      </see-also>
    </xsl:if>

    <xsl:apply-templates mode="extract" select=".//implementation-note"/>
    <xsl:apply-templates mode="extract" select=".//return"/>

    <documentation-string>
      <xsl:apply-templates/>
    </documentation-string>
  </xsl:template>
</xsl:stylesheet>
