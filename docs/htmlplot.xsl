<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" encoding="utf-8" indent="yes"/>
<xsl:preserve-space elements="text"/>

<xsl:param name="show-alias" select="yes"/>
	<xsl:template match="/">
	   <xsl:apply-templates />
	</xsl:template>


	<xsl:template match="plotscript">
		<html>
			<head>
				<title>Dictionary of Plotscripting Commands</title>
				<style type="text/css">
<![CDATA[
					body,p,h1,h2,h3,h4,h5,h6 {
						color: white;
						background-color: black;
					}

					h2 {
						font-style: italic;
					}

					.section h3 {
						color: #f06060;
						font-size: 130%;
					}
					.subsection h3 {
						color: #e05050;
					}

					h4 {
						color: yellow;
					}

					.command {
						margin-top: 17px;
						margin-bottom: 17px;
						line-height: 18px;
						/* equivalent to tt */
						font-family: 'Lucida Console',monospace;
						font-size: 0.75em;
					}
					/* Remove gap between command name and definition */
					.command h4 {
						margin-bottom: 3px;
					}
					.command p {
						margin-top: 3px;
					}

					pre {
						background-color:#FFFFFF;
						color:#000000;
						border-width:3px;
						border-style:ridge;
						padding:10px;
						padding-left:20px;
						/*font-family:fixedsys,monospace;*/
						font-size: 12px;
					}

					a {
						color:#00ff00;
					}
					a:active {
						color: #d0d000;
					}
					a:visited {
						color: #009090;
					}

					a.undef {
						font-weight:bold;
						color:red;
					}

					a.ref:after {
						/*content: " ?";*/
					}

					.key, .param {
						font-weight: bold;
						color: yellow;
					}

					/*.param {
						color: yellow;
					}*/

					.seealso {
						margin-top: 10px;
					}

					.seealso ul {
						display: inline;
						margin:0;
						padding:0;
					}
					.seealso li {
						display: inline;
					}

					.section {
					}
					.subsection {
					}

					.backlink {
						color: #f09090;
					}

					div.note {
					  border:thin black solid;
					  padding:5px;
					  color:black;
					  margin:3px;
					  min-height: 48px;
					  height: 48px;
					}

					div.note a {
					  color: #008800;
					}

					div.note .param, div.note .key {
					  color: #888800;
					}
					
					.icon {
						float:left;
						padding-right: 5px;
					}

					div.indent {
						padding-left: 20px;
						/*display: inline-block;*/
					}
					.sectionlist ul {
						margin:0;
						/* Hide bullets for sectionlinks at top */
						list-style-type: none;
					}
					.sectionlist li {
						/* This is needed to hide bullets for old IE versions*/
						list-style-type: none;
					}

]]>
				</style>
			</head>
			<body>
				<h1>Plotscripting Dictionary</h1>
				<p>This is a listing of all the plotscripting commands implemented as of <xsl:value-of select="@lastmodified" />.</p>
				<p>In addition to reading this document, I also recommend you check out Plotscripting Tutorial and the HamsterSpeak Specification</p>
				<hr/>
				<h2>Commands by Category</h2>
				<p>
				<div class="sectionlist">
				<xsl:apply-templates select="section" mode="sections"/>
				</div>
				</p>
				<hr/>
				<h2>Alphabetical Index</h2>
				<p>
				<xsl:apply-templates select=".//command" mode="alphalist">
				<xsl:sort select="@id" data-type="text" />
				</xsl:apply-templates>
				</p>
				<hr/>
				<xsl:apply-templates select="section" mode="full">
					<xsl:with-param name="section-class">section</xsl:with-param>
				</xsl:apply-templates>

				<p>Stats: There are <xsl:value-of select='count(//command)'/> commands in this file, of which <xsl:value-of select='count(//alias)'/> are only references to other commands.</p>
				<p>This file was generated from an XML file. The contents were painstakingly transcribed by Mike Caron from the original Plotscripting Dictionary, which was created by James Paige.</p>
			</body>
		</html>
	</xsl:template>

	<xsl:template match="section" mode="sections"><xsl:text>
		</xsl:text><!--<xsl:if test='@subsection_of'><div class="subsection-header-spacer" /></xsl:if>-->
		<li><a href="#{@title}"><xsl:value-of select="@title" /></a>
		<br/><xsl:text>
		</xsl:text>
		<!-- Recurse on subsections -->
		<ul><xsl:apply-templates select="section" mode="sections" /></ul>
		</li>
	</xsl:template>

	<xsl:template match="command" mode="alphalist"><xsl:text>
		</xsl:text><xsl:if test='boolean(canon)'>
			<a href="#about-{@id}"><xsl:value-of select="canon" /></a><br/>
		</xsl:if><xsl:text>
		</xsl:text><xsl:if test='boolean(alias)'>
			<a href="#about-{alias}"><xsl:value-of select="shortname" /></a><br/>
		</xsl:if><xsl:text>
	</xsl:text></xsl:template>

	<xsl:template match="section" mode="full">
		<xsl:param name="backlinks" />
		<xsl:param name="section-class" />

		<xsl:text>
		</xsl:text><div class="{$section-class}"><a name="{@title}"></a><xsl:text>
		</xsl:text><h3><span class="backlink"><xsl:copy-of select="$backlinks" /></span><xsl:value-of select="@title" /></h3><xsl:text>
		</xsl:text>
		<!-- Alternative way to show backlink to parent-->
		<!-- <xsl:if test="parent::section"> -->
		<!-- 	<div class="backlink">(Back to <a href="#{../@title}"><xsl:value-of select="../@title" /></a>.)</div> -->
		<!-- </xsl:if> -->
		<!-- Show links to subsections-->
		<ul><xsl:apply-templates select="section" mode="sections" /></ul>
		<!-- Show section description-->
		<p><xsl:apply-templates select="description"/></p><xsl:text>
		<!-- Show commands-->
		</xsl:text><xsl:apply-templates select="command|reference" mode="full" /><xsl:text>
		</xsl:text><hr></hr><xsl:text>
		</xsl:text></div>
		<!-- Followed by subsections (outside section div) -->
		<xsl:apply-templates select="section" mode="full">
			<xsl:with-param name="backlinks">
				<xsl:copy-of select="$backlinks" /> <a href="#{@title}"><xsl:value-of select="@title" /></a> →
			</xsl:with-param>
			<xsl:with-param name="section-class">subsection</xsl:with-param>
		</xsl:apply-templates>
	</xsl:template>

	<xsl:template match="reference" mode="full"><xsl:text>
		</xsl:text><div class="command"><xsl:text>
		</xsl:text><h4>
		<xsl:value-of select="//command[@id=current()/@id]/canon" />
		</h4><xsl:text>
		</xsl:text><p><a href="#about-{@id}">Defined in the <xsl:value-of select="//command[@id=current()/@id]/../@title" /> section.</a></p><xsl:text>
		</xsl:text></div>
	</xsl:template>

	<xsl:template match="command" mode="full"><xsl:text>
		</xsl:text><div class="command"><a name="about-{@id}" ></a><xsl:text>
		</xsl:text><xsl:if test='boolean(canon)'>
			<h4><xsl:value-of select="canon" /></h4><p><xsl:text>
			</xsl:text><xsl:apply-templates select="description" /><xsl:text>
			<!-- <example> can also appear nested inside <description> rather than following it -->
			</xsl:text><xsl:apply-templates select="example" /><xsl:text>
			</xsl:text><xsl:apply-templates select="seealso" /><xsl:text>
		</xsl:text></p></xsl:if>
		<xsl:if test='boolean(alias)'><xsl:text>
				</xsl:text><h4><xsl:value-of select="shortname" /></h4><xsl:text>
				</xsl:text>See <a href="#about-{alias}"><xsl:value-of select="id(alias)/shortname" /></a><xsl:text>
			</xsl:text>
		</xsl:if>
		</div>
	</xsl:template>

	<xsl:template match="description"><xsl:apply-templates /><!-- <br/> --></xsl:template>
	<xsl:template match="ul"><ul><xsl:apply-templates /></ul></xsl:template>
	<xsl:template match="li"><li><xsl:apply-templates /></li></xsl:template>

	<xsl:template match="example">
		<xsl:if test='@c'>
			<pre><xsl:value-of select="id(@c)/example" /></pre>
		</xsl:if>
		<xsl:if test="not(@c)">
			<pre><xsl:value-of select="." /></pre>
		</xsl:if>
	</xsl:template>

	<xsl:template match="a"><a href="{@href}"><xsl:value-of select="." /></a></xsl:template>
	<xsl:template match="p"><span class="key"><xsl:value-of select="." /></span></xsl:template>
	<xsl:template match="ref">
		<xsl:if test='count(id(.))=0'>
			<a href="#{.}" class="undef"><xsl:value-of select='.' /></a>
		</xsl:if>
		<xsl:if test='count(id(.))>0'>
			<xsl:if test='not(id(.)/alias)'>
				<a href="#about-{.}" class="ref"><xsl:value-of select='id(.)/shortname' /></a>
			</xsl:if>
			<xsl:if test='id(.)/alias'>
				<a href="#about-{id(.)/alias}" class="ref"><xsl:value-of select='id(.)/shortname' /></a>
			</xsl:if>
		</xsl:if>
	</xsl:template>
	<xsl:template match="seealso">
		<div class="seealso">See also: <ul><xsl:apply-templates select="ref" mode="seealso"/></ul></div>
	</xsl:template>
	<xsl:template match="ref" mode="seealso">
		<li>
		<xsl:if test='count(id(.))=0'>
			<a href="#{.}" class="undef"><xsl:value-of select='.' /></a>
		</xsl:if>
		<xsl:if test='count(id(.))>0'>
			<xsl:if test='not(id(.)/alias)'>
				<a href="#about-{.}" class="ref"><xsl:value-of select='id(.)/shortname' /></a>
			</xsl:if>
			<xsl:if test='id(.)/alias'>
				<a href="#about-{id(.)/alias}" class="ref"><xsl:value-of select='id(.)/shortname' /></a>
			</xsl:if>
		</xsl:if><xsl:if test="not(position() = last())">, </xsl:if>
		</li>
	</xsl:template>

	<xsl:template match="lb"><br/></xsl:template>

	<xsl:template match="p"><span class="param"><xsl:apply-templates /></span></xsl:template>

	<xsl:template match="note">
		<div style="background-color:#EFF;" class="note">
			<img src="http://gilgamesh.hamsterrepublic.com/wiki/ohrrpgce-images/0/01/Plotnote.png" alt="[Note]" class="icon"/>
			<xsl:apply-templates /><br clear="all" />
		</div>
	</xsl:template>

	<xsl:template match="warn">
		<div style="background-color:#FFE;" class="note">
			<img src="http://gilgamesh.hamsterrepublic.com/wiki/ohrrpgce-images/d/dc/Plotwarn.png" alt="[Warning]" class="icon"/>
			<xsl:apply-templates /><br clear="all" />
		</div>
	</xsl:template>

	<xsl:template match="danger">
		<div style="background-color:#FEE;" class="note">
			<img src="http://gilgamesh.hamsterrepublic.com/wiki/ohrrpgce-images/a/ab/Plotdanger.png" alt="[Danger]" class="icon"/>
			<xsl:apply-templates /><br clear="all" />
		</div>
	</xsl:template>

</xsl:stylesheet>

