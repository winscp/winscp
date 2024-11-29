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

</xsl:stylesheet>
