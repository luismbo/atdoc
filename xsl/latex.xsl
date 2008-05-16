<!-- Hey, emacs, please consider this to be -*- xml -*-

    Generates documentation as a .tex file for use with pdflatex.
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:macro="http://lichteblau.com/macro"
		xmlns:atdoc="http://www.lichteblau.com/atdoc/"
		version="1.0">
  <xsl:output method="text"/>

  <xsl:key name="aboutfun" match="aboutfun" use="string(.)"/>
  <xsl:key name="aboutclass" match="aboutclass" use="string(.)"/>
  <xsl:key name="aboutmacro" match="aboutmacro" use="string(.)"/>

  <xsl:key name="id" 
	   match="function-definition|class-definition|macro-definition|variable-definition"
	   use="@id"/>

  <xsl:template match="/">
    <xsl:apply-templates select="documentation"/>
  </xsl:template>    

  <xsl:template match="documentation">
    \documentclass[a4paper]{article}
    \pagestyle{plain}
    %\usepackage{amsmath}
    %\usepackage{amssymb}
    %\usepackage{amsthm}
    %\usepackage{fancyvrb}
    \usepackage[usenames]{color}
    \usepackage[linkcolor=black,citecolor=black]{hyperref}
    \usepackage{makeidx}
    \makeindex

    \setlength{\parindent}{0cm}
    \addtolength{\parskip}{0.5em}
    %\renewcommand{\thefootnote}{\fnsymbol{footnote}}

    \begin{document}
    \input{defun.tex}

    \title{<macro:escaped select="@title"/>}
    %\date{}
    \maketitle
    \tableofcontents
    \newpage

    <xsl:apply-templates select="package/sections"/>

    <xsl:variable name="unreferenced"
		  select="package/external-symbols/function-definition[
			  count(key('aboutfun',@name))=0
			  ]"/>
    <xsl:if test="$unreferenced">
      \section{Other functions}
      <xsl:apply-templates select="$unreferenced">
	<xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>

    <xsl:variable name="unreferenced2"
		  select="package/external-symbols/macro-definition[
			  count(key('aboutmacro',@name))=0
			  ]"/>
    <xsl:if test="$unreferenced2">
      \section{Other macros}
      <xsl:apply-templates select="$unreferenced2">
	<xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>

    <xsl:variable name="unreferenced3"
		  select="package/external-symbols/class-definition[
			  count(key('aboutclass',@name))=0
			  ]"/>
    <xsl:if test="$unreferenced3">
      \section{Other classes}
      <xsl:apply-templates select="$unreferenced3">
	<xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>

    <xsl:variable name="unreferenced4"
		  select="package/external-symbols/variable-definition"/>
    <xsl:if test="$unreferenced4">
      \section{Other variables}
      <xsl:apply-templates select="$unreferenced4">
	<xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>

    \printindex
    \end{document}
  </xsl:template>

  <xsl:template match="arguments">
    <macro:sc label="Arguments">
      <xsl:for-each select="arg">
	<macro:escaped select="@arg"/>
	--- <xsl:apply-templates/>
	<xsl:text>&#10;&#10;</xsl:text>
      </xsl:for-each>
    </macro:sc>
  </xsl:template>

  <xsl:template name="main-documentation-string">
    <xsl:choose>
      <xsl:when test="documentation-string">
	<macro:sc label="Details">
	  <xsl:apply-templates select="documentation-string"/>
	</macro:sc>
      </xsl:when>
      <xsl:otherwise>
	No documentation string.  Possibly unimplemented or incomplete.
	<xsl:text>&#10;&#10;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates select="implementation-note"/>
    <xsl:if test="see-also/constructor">
      <macro:sc label="Returned by">
 	<macro:itemize>
	  <xsl:apply-templates select="see-also/constructor/see"/>
	</macro:itemize>
      </macro:sc>
    </xsl:if>
    <xsl:if test="see-also/slot">
      <macro:sc label="Slot Access Functions">
	<macro:itemize>
	  <xsl:apply-templates select="see-also/slot/see"/>
	</macro:itemize>
      </macro:sc>
    </xsl:if>
    <xsl:if
       test="//class-definition[@id=current()//superclass/@id]
	     //see-also
	     /slot">
      <macro:sc label="Inherited Slot Access Functions">
	<macro:itemize>
	  <xsl:apply-templates
	     select="//class-definition[@id=current()//superclass/@id]
		     //see-also/slot/see"/>
	</macro:itemize>
      </macro:sc>
    </xsl:if>
    <xsl:if test="see-also/other|see-also/auto">
      <macro:sc label="See also">
	<macro:itemize>
	  <xsl:for-each select="see-also/other/see|see-also/auto/see">
	    <xsl:variable name="name" select="text()"/>
	    <xsl:if test="not(preceding-sibling::see[text() = $name])">
	      <xsl:apply-templates select="."/>
	    </xsl:if>
	  </xsl:for-each>
	</macro:itemize>
      </macro:sc>
    </xsl:if>
  </xsl:template>

  <xsl:template name="class-list">
    <xsl:if test="position() != 1">
      <xsl:text>, </xsl:text>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@id">
	<macro:hyperref>
	  <xsl:if test="@status = 'INTERNAL'">
	    <macro:escaped select="@package"/>
	    <xsl:text>::</xsl:text>
	  </xsl:if>
	  <macro:escaped select="@name"/>
	</macro:hyperref>
      </xsl:when>
      <xsl:when test="@status = 'INTERNAL'">
	<macro:normalize>
	  \color[rgb]{0.5,0.5,0.5}
	  <macro:escaped select="@package"/>
	  <xsl:text>::</xsl:text>
	  <macro:escaped select="@name"/>
	  \color[rgb]{0,0,0}
	</macro:normalize>
      </xsl:when>	  
      <xsl:otherwise>
	<macro:normalize>
	  \color[rgb]{0.5,0.5,0.5}
	  <macro:escaped select="@package"/>
	  <xsl:text>:</xsl:text>
	  <macro:escaped select="@name"/>
	  \color[rgb]{0,0,0}
	</macro:normalize>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="lambda-list">
    <tt><macro:escaped select="../@name"/></tt>
    <xsl:text> (</xsl:text>
    <xsl:for-each select="elt">
      <xsl:if test="position() != 1">
	<xsl:text>&#160;</xsl:text>
      </xsl:if>
      <b><macro:escaped select="text()"/></b>
    </xsl:for-each>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template name="slot-list">
    <macro:escaped select="@name"/>
    <xsl:text> --- </xsl:text>
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
    <xsl:text>&#10;&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="documentation-string">
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="short">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="em">\emph{<xsl:apply-templates/>}</xsl:template>

  <xsl:template match="b">\textbf{<xsl:apply-templates/>}</xsl:template>

  <xsl:template match="var">\texttt{<xsl:apply-templates/>}</xsl:template>

  <!-- fixme: use verbatim? -->
  <xsl:template match="code">\texttt{<xsl:apply-templates/>}</xsl:template>

  <xsl:template match="pre">
    <macro:normalize>
      \begin{verbatim}
      <xsl:apply-templates mode="verbatim"/>
      \end{verbatim}
    </macro:normalize>
  </xsl:template>

  <xsl:template match="a">
    <macro:normalize>
      \href{<macro:escaped select="@a"/>}{
      <xsl:apply-templates/>
      }
    </macro:normalize>
  </xsl:template>

  <xsl:template match="fun|class|slot|type|variable">
    <macro:hyperref>\texttt{<xsl:apply-templates/>}</macro:hyperref>
  </xsl:template>

  <xsl:template match="itemize">
    \begin{itemize}
      <xsl:apply-templates/>
    \end{itemize}
  </xsl:template>

  <xsl:template match="item">
    \item <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="see">
    \item
    <macro:hyperref><xsl:apply-templates/></macro:hyperref>
    <xsl:if test="@see">
      \emph{(<macro:escaped select="@see"/>)}
    </xsl:if>
  </xsl:template>

  <xsl:template match="implementation-note">
    <macro:normalize>
<!--
      \textbf{Implementation notes:}
-->
      Implementation notes:
      <xsl:apply-templates/>
    </macro:normalize>
    <xsl:text>&#10;&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="break">
    <xsl:text>&#10;&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="sections">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="section">
    <macro:normalize>
      \
      <xsl:for-each select="ancestor::section">sub</xsl:for-each>
      section{<macro:escaped select="@section"/>}
      \label{<macro:escaped select="generate-id()"/>}
    </macro:normalize>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="return">
    <macro:sc label="Returns">
      <xsl:apply-templates/>
    </macro:sc>
  </xsl:template>

  <xsl:template match="aboutfun">
    <xsl:apply-templates select="//function-definition[@name=current()]"/>
  </xsl:template>

  <xsl:template match="aboutmacro">
    <xsl:apply-templates select="//macro-definition[@name=current()]"/>
  </xsl:template>

  <xsl:template match="aboutclass">
    <xsl:apply-templates select="//class-definition[@name=current()]
				 | //type-definition[@name=current()]"/>
  </xsl:template>

  <xsl:template match="function-definition">
    <macro:glsdefun label="Function">
      <xsl:apply-templates select="arguments"/>
      <xsl:apply-templates select="return"/>
      <xsl:call-template name="main-documentation-string"/>
    </macro:glsdefun>
  </xsl:template>

  <xsl:template match="macro-definition">
    <macro:glsdefun label="Macro">
      <xsl:apply-templates select="arguments"/>
      <xsl:apply-templates select="return"/>
      <xsl:call-template name="main-documentation-string"/>
    </macro:glsdefun>
  </xsl:template>

  <xsl:template match="class-definition">
    <macro:glsdefun label="Class">
      <macro:sc label="Superclasses">
	<xsl:for-each select="cpl/superclass">
	  <xsl:call-template name="class-list"/>
	</xsl:for-each>
      </macro:sc>
      <macro:sc label="Documented Subclasses">
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
      </macro:sc>
      <xsl:if test="direct-slots">
	<macro:sc label="Direct Slots">
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
	</macro:sc>
      </xsl:if>
      <xsl:call-template name="main-documentation-string"/>
    </macro:glsdefun>
  </xsl:template>

  <xsl:template match="type-definition">
    <macro:glsdefun label="Type">
      <xsl:call-template name="main-documentation-string"/>
    </macro:glsdefun>
  </xsl:template>

  <xsl:template match="variable-definition">
    <macro:glsdefun label="Variable">
      <xsl:call-template name="main-documentation-string"/>
    </macro:glsdefun>
  </xsl:template>

  <xsl:template name="undocumented">
    <xsl:if test="not(documentation-string)">
      <xsl:text>\color[rgb]{1,0,0}~(undocumented)</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template match="text()">
    <macro:escaped select="."/>
  </xsl:template>

  <xsl:template match="text()" mode="verbatim">
    <!-- fixme: is there anything we need to escape here? -->
    <xsl:value-of select="string(.)"/>
  </xsl:template>
</xsl:stylesheet>
