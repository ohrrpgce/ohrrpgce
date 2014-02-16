<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xdt="http://www.w3.org/2005/xpath-datatypes" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:hr="http://hamsterrepublic.com">
<xsl:output method="xml" encoding="iso-8859-1" media-type="text/xml" indent="no" />

<!-- -->
<xsl:variable name="cda" select="current-dateTime() - timezone-from-dateTime(current-dateTime())" />
<xsl:param name="letter" />

<!--
  you do NOT want to know how long it took me to write the following expression.

  And, yes, it is as ugly as it looks. However, it should be cross-platform...
-->

<xsl:variable name="da" select="dateTime(xs:date(concat(string(year-from-dateTime($cda)),'-',hr:string-pad('0', 2 - string-length(string(month-from-dateTime($cda)))), string(month-from-dateTime($cda)),'-',hr:string-pad('0', 2 - string-length(string(day-from-dateTime($cda)))), string(day-from-dateTime($cda)))),xs:time(concat(hr:string-pad('0', 2 - string-length(string(hours-from-dateTime($cda)))),string(hours-from-dateTime($cda)),':',hr:string-pad('0', 2 - string-length(string(minutes-from-dateTime($cda)))),string(minutes-from-dateTime($cda)),':',hr:string-pad('0', 2 - string-length(string(floor(seconds-from-dateTime($cda))))),string(floor(seconds-from-dateTime($cda))))))" />

	<xsl:template match="/">
	  <mediawiki xmlns="http://www.mediawiki.org/xml/export-0.3/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.mediawiki.org/xml/export-0.3/ http://www.mediawiki.org/xml/export-0.3.xsd" version="0.3" xml:lang="en">
	    <xsl:apply-templates select="//section/command[starts-with(@id,$letter)]"/>
<xsl:if test="$letter='!'">
	    <page>
	      <title>Plot:Index</title>
	      <revision>
	        <timestamp><xsl:value-of select="$da" />Z</timestamp>
          <contributor><username>Plotscripting Dictionary</username></contributor>
          <text xml:space="preserve">This is a full listing of all plotscripting commands to date.
<xsl:apply-templates select="//section" mode="index" />
[[Category:Plotscripting]]</text>
	      </revision>
	    </page>
</xsl:if>
	  </mediawiki>
	</xsl:template>

<xsl:template match="section" mode="index">=<xsl:value-of select="@title" />=
<xsl:apply-templates select="command" mode="index" /></xsl:template>

<xsl:template match="command" mode="index">* [[Plot:<xsl:value-of select="hr:wiki-title(shortname)" />|<xsl:value-of select="shortname" />]]
</xsl:template>

<xsl:template match="command">
  <page>
    <title>Plot:<xsl:value-of select="hr:wiki-title(shortname)" /></title>
    <revision>
      <timestamp><xsl:value-of select="$da" />Z</timestamp>
      <contributor><username>Plotscripting Dictionary</username></contributor>
      <text xml:space="preserve"><xsl:if test='boolean(canon)'> <xsl:value-of select="canon" />

<xsl:apply-templates select="description" /><xsl:if test="count(example)>0">

==Examples==
<xsl:apply-templates select="example" /></xsl:if>
<xsl:apply-templates select="seealso" />
      </xsl:if><xsl:if test='boolean(alias)'>#REDIRECT [[Plot:<xsl:value-of select="id(alias)/shortname" />]]</xsl:if>
[[Category:Plotscripting]]</text>
    </revision>
  </page>
</xsl:template>

<xsl:template match="decription"><xsl:apply-templates /></xsl:template>

<xsl:template match="example">&lt;pre><xsl:value-of select="." />&lt;/pre><xsl:text>
</xsl:text></xsl:template>

<xsl:template match="seealso"><xsl:text>==See Also==
</xsl:text><xsl:apply-templates select="ref" mode="sa" /></xsl:template>

<xsl:template match="lb"><xsl:text>

</xsl:text></xsl:template>
<xsl:template match="ul"><xsl:apply-templates /></xsl:template>
<xsl:template match="li">
* <xsl:apply-templates /></xsl:template>

<xsl:template match="p">'''<xsl:apply-templates />'''</xsl:template>

<xsl:template match="ref">[[Plot:<xsl:value-of select='hr:wiki-title(id(.)/shortname)' />|<xsl:value-of select='id(.)/shortname' />]]</xsl:template>

<xsl:template match="ref" mode="sa">* [[Plot:<xsl:value-of select='hr:wiki-title(id(.)/shortname)' />|<xsl:value-of select='id(.)/shortname' />]]
</xsl:template>

<xsl:template match="a">[[<xsl:value-of select="@href" /> <xsl:value-of select="." />]]</xsl:template>

<xsl:template match="note" xml:space="preserve">
&lt;div style="background-color:#EFF;border:thin black solid;padding:5px;">[[Image:Plotnote.png|left]]<xsl:apply-templates />&lt;br clear="all" />&lt;/div></xsl:template>

<xsl:template match="warn" xml:space="preserve">
&lt;div style="background-color:#FFE;border:thin black solid;padding:5px;">[[Image:Plotwarn.png|left]]<xsl:apply-templates />&lt;br clear="all" />&lt;/div></xsl:template>

<xsl:template match="danger" xml:space="preserve">
&lt;div style="background-color:#FEE;border:thin black solid;padding:5px;">[[Image:Plotdanger.png|left]]<xsl:apply-templates />&lt;br clear="all" />&lt;/div></xsl:template>

<xsl:function name="hr:string-pad" as="xs:string">
  <xsl:param name="padString" as="xs:string?"/>
  <xsl:param name="padCount" as="xs:integer"/>
  <xsl:sequence select="string-join((for $i in 1 to $padCount
                                    return $padString), '')"/>
 </xsl:function>

<xsl:function name="hr:wiki-title" as="xs:string">
  <xsl:param name="title" as="xs:string?"/>
  <xsl:sequence select="replace(replace(replace(replace($title, '\|', '(BAR)'), '&amp;', '(AMP)'), '>', '(GT)'), '&lt;', '(LT)')" />
</xsl:function>

<!--
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


-->
</xsl:stylesheet>
