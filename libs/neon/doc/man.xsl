<?xml version='1.0'?>

<!-- This file wraps around the DocBook XSL manpages stylesheet to customise
   - some parameters; add the CSS stylesheet, etc.
 -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'
                xmlns="http://www.w3.org/TR/xhtml1/transitional"
                exclude-result-prefixes="#default">

<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/manpages/docbook.xsl"/>

<xsl:param name="man.output.quietly" select="1"/>

<xsl:param name="man.endnotes.list.enabled">1</xsl:param>

<xsl:param name="man.endnotes.are.numbered">1</xsl:param>

<!-- by convention, only RFC references will be used. -->
<xsl:param name="man.endnotes.list.heading">References</xsl:param>

<xsl:param name="man.authors.section.enabled">0</xsl:param>

<xsl:template match="xref">
  <xsl:variable name="target" select="id(@linkend)"/>
  <xsl:choose>
    <!-- Target is a refname element inside a refentry -->
    <xsl:when test="$target/self::refname">
      <xsl:variable name="refentry" select="$target/ancestor::refentry"/>
      <xsl:variable name="manvolnum">
        <xsl:choose>
          <xsl:when test="$refentry/refmeta/manvolnum">
            <xsl:value-of select="$refentry/refmeta/manvolnum"/>
          </xsl:when>
          <xsl:otherwise>3</xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:text>\fB</xsl:text>
      <xsl:value-of select="$target"/>
      <xsl:text>\fR(</xsl:text>
      <xsl:value-of select="$manvolnum"/>
      <xsl:text>)</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-imports/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

</xsl:stylesheet>
