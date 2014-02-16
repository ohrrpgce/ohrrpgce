<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" encoding="iso-8859-1" media-type="text/plain"/>

	<xsl:template match="/">
	   <xsl:apply-templates />
	</xsl:template>


<xsl:template match="plotscript">
This is a listing of all the plotscripting commands implemented as of <xsl:value-of select="@lastmodified" />.

In addition to reading this document, I also recommend you check out [[Plotscripting Tutorial]] and the [[HamsterSpeak Specification]]

<xsl:apply-templates select="section" mode="full" />

[[Category:Unfinished Articles]]
</xsl:template>

<xsl:template match="section" mode="full">
=<xsl:value-of select="@title" />=
<xsl:apply-templates select="command" mode="full" />
</xsl:template>

<xsl:template match="command" mode="full">
<xsl:if test='boolean(canon)'>
==<xsl:value-of select="shortname" />==
'''<xsl:value-of select="canon" />'''<br/>
<xsl:apply-templates select="description" />
<xsl:apply-templates select="example" />
</xsl:if>
<xsl:if test='boolean(alias)'>
==<xsl:value-of select="shortname" />==
See [[#<xsl:value-of select="id(alias)/shortname" />|<xsl:value-of select="id(alias)/shortname" />]]
</xsl:if>
</xsl:template>

<xsl:template match="decription"><xsl:apply-templates /></xsl:template>

<xsl:template match="example"><pre><xsl:value-of select="." /></pre></xsl:template>

<xsl:template match="ref"><xsl:if test='count(id(.))=0'>'''[[#<xsl:value-of select='.' />|<xsl:value-of select='.' />]]'''</xsl:if><xsl:if test='count(id(.))>0'>[[#<xsl:value-of select='id(.)/shortname' />|<xsl:value-of select='id(.)/shortname' />]]</xsl:if></xsl:template>
<xsl:template match="a">[[<xsl:value-of select="@href" /> <xsl:value-of select="." />]]</xsl:template>

<xsl:template match="lb"><br/></xsl:template>
<xsl:template match="ul"><xsl:apply-templates /></xsl:template>
<xsl:template match="li">
* <xsl:apply-templates /></xsl:template>
</xsl:stylesheet>
