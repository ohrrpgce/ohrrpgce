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
						/* Make the "Back to top link" fit snuggly below */
						margin-bottom: 6px;
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
						background-color:#B0B8B8;
						color:#000000;
						border-width:3px;
						border-style:ridge;
						padding:7px;
						padding-left:20px;
						margin:3px;
						white-space: pre-wrap;
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
						color: #00bb11;
					}

					a.undef {
						font-weight:bold;
						color:red;
					}

					a.ref:after {
						/*content: " ?";*/
					}

					.param {
						font-weight: bold;
						color: yellow;
					}

					.code {
						font-style: italic;
						color: greenyellow;
					}

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

					/* Used both for "Back to top" and parent section links */
					.backlink {
						color: #f09090;
						font-size: 90%;
					}

					/* Used by note, warn, and danger */
					div.notice {
					  border:thin black solid;
					  padding:5px;
					  padding-left:42px;
					  color:black;
					  margin:3px;
					  min-height: 32px;
					}

					div.notice a {
					  color: #008800;
					}

					div.notice .param, div.notice .code {
					  color: #888800;
					}

					div.note {
						background-color: #BCC;
					}

					div.warn {
						background-color: #CCB;
					}

					div.danger {
						background-color: #DCC;
					}

					.icon {
						position: absolute;
						left: 16px;
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


					button {
						/* Remove/override text styles */
						font-family: inherit;
						font-size: 90%;
						line-height: 1.15;
						/* Remove the margin in Firefox and Safari */
						margin: 0;
						padding: 2px 10px;
						/* Remove the bezelling */
						border: 0;
						border-radius: 5px;
						background-color: #004c00;
						color: #ddd;  /* inherit; */
					}
					button:hover {
						background: #006609;
					}
					button:focus {
						outline: 1px solid #019b01;
						outline-offset: -2px;
					}

					/* Used for toggling command links in section headers, which are inside
					   an .content-shown element inside a .select-content-shown/hidden element */
					.select-content-shown .content-hidden {
						display: none;
					}
					.select-content-hidden .content-shown {
						display: none;
					}


]]>
				</style>
				<script>
function toggleContent(target) {
	//var target = event.target || event.srcElement;
	var x = target.parentNode;
	if (x.className === "select-content-hidden") {
		x.className = "select-content-shown";
	} else {
		x.className = "select-content-hidden";
	}
}
				</script>
			</head>
			<body>
				<h1>Plotscripting Dictionary</h1>
				<xsl:if test="@version = 'wip'">
					<h2>For the current OHRRPGCE work-in-progress version (<xsl:value-of select="@datecode" />)</h2>
				</xsl:if>
				<xsl:if test="not(@version = 'wip')">
					<h2>For OHRRPGCE version <xsl:value-of select="@version" /></h2>
					(Generated on <xsl:value-of select="@datecode" />)
				</xsl:if>

				<p>This is the documentation for all <a href="https://rpg.hamsterrepublic.com/ohrrpgce/Plotscripting">plotscripting</a> commands.
				In addition to reading this document, we also recommend you check out the
				<a href="https://rpg.hamsterrepublic.com/ohrrpgce/Plotscripting_Tutorial">Plotscripting Tutorial</a>
				and the <a href="https://rpg.hamsterrepublic.com/ohrrpgce/Plotscripting">Plotscripting</a>
				article on the wiki.</p>
				<p>
				If you're reading this on the <a href="https://rpg.hamsterrepublic.com/ohrrpgce/">OHRRPGCE website</a>
				then make sure you're looking at the page for the correct stable release or latest
				nightly version.
				This documentation is also included as an HTML file with downloads of the engine.
				</p>
				<hr/>
				<a name="Categories"></a>
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

				<xsl:variable name="numconstants" select='count(//section[@constants="yes"]/command)'/>
				<xsl:variable name="numtopics" select='count(//section[@topics="yes"]/command)'/>
				<p>Stats: There are <xsl:value-of select='count(//command) - $numconstants - $numtopics'/> commands (of which <xsl:value-of select='count(//command[not(@constants) and not(@topics)]/alias)'/> are aliases), <xsl:value-of select='$numconstants'/> constants and <xsl:value-of select='$numtopics'/> definitions of types and other terms in this file.</p>
				<p>This file was generated from an XML file. The contents were painstakingly transcribed by Mike Caron from the original Plotscripting Dictionary, which was created by James Paige.</p>
			</body>
		</html>
	</xsl:template>

	<!-- Nested list of subsection links -->
	<xsl:template match="section" mode="sections"><xsl:text>
		</xsl:text><!--<xsl:if test='@subsection_of'><div class="subsection-header-spacer" /></xsl:if>-->
		<li><a href="#{@title}"><xsl:value-of select="@title" /></a>
		<br/><xsl:text>
		</xsl:text>
		<!-- Recurse on subsections -->
		<ul><xsl:apply-templates select="section" mode="sections" /></ul>
		</li>
	</xsl:template>

	<!-- Nested list of subsection links and all commands there-in -->
	<xsl:template match="section" mode="sections-and-commands"><xsl:text>
		</xsl:text><!--<xsl:if test='@subsection_of'><div class="subsection-header-spacer" /></xsl:if>-->
		<li><a href="#{@title}"><xsl:value-of select="@title" /></a>
		<br/><xsl:text>
		</xsl:text>
		<ul class="content-shown">
			<xsl:apply-templates select="command" mode="alphalist" />
		</ul>
		<!-- Recurse on subsections -->
		<ul><xsl:apply-templates select="section" mode="sections-and-commands" /></ul>
		</li>
	</xsl:template>

	<!-- A link to a command -->
	<xsl:template match="command" mode="alphalist">
		<xsl:if test='boolean(canon)'>
			<a href="#about-{@id}"><xsl:value-of select="canon" /></a><br/>
		</xsl:if><xsl:if test='boolean(alias)'>
			<a href="#about-{alias}"><xsl:value-of select="shortname" /></a><br/>
		</xsl:if><xsl:text>
		</xsl:text>
	</xsl:template>

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
		<xsl:if test="not(parent::section)">
			<!-- Top-level section: special back-link -->
			<a href="#Categories" class="backlink">[Back to top-level index]</a>
		</xsl:if>

		<!-- Show section description-->
		<p><xsl:apply-templates select="description"/></p>
		<xsl:if test="not(parent::section)">
			<!-- Top-level section: show all subsections and commands -->
			<div class="select-content-shown" id="{@title}-index" >
				<button onclick="toggleContent(this);">
					<span class="content-shown">Hide command index</span>
					<span class="content-hidden">Show command index</span>
				</button>
				<ul>
					<div class="content-shown">
						<xsl:apply-templates select="command" mode="alphalist" />
					</div>
					<xsl:apply-templates select="section" mode="sections-and-commands" />
				</ul>
			</div>
		</xsl:if>
		<xsl:if test="parent::section">
			<ul>
				<!-- Subsection: only show links to nested subsections-->
				<xsl:apply-templates select="section" mode="sections" />
			</ul>
		</xsl:if>

		<!-- Show commands-->
		<xsl:apply-templates select="command|reference" mode="full" /><xsl:text>
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
		<xsl:value-of select="//command[@id=current()/@ref]/canon" />
		</h4><xsl:text>
		</xsl:text><p><a href="#about-{@ref}">Documented in the <xsl:value-of select="//command[@id=current()/@ref]/../@title" /> section.</a></p><xsl:text>
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
				</xsl:text>An alias for <a href="#about-{alias}"><xsl:value-of select="id(alias)/shortname" /></a>.<xsl:text>
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
	<xsl:template match="ref" name="ref">
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
			<xsl:call-template name="ref"/>
			<xsl:if test="not(position() = last())">, </xsl:if>
		</li>
	</xsl:template>

	<xsl:template match="lb"><br/></xsl:template>

	<xsl:template match="p"><span class="param"><xsl:apply-templates /></span></xsl:template>

	<xsl:template match="tt"><span class="code"><xsl:apply-templates /></span></xsl:template>

	<xsl:template match="b"><b><xsl:apply-templates /></b></xsl:template>

	<xsl:template match="note">
		<div class="notice note">
			<img src="Plotnote.png" alt="[Notice]" class="icon"/>
			<xsl:apply-templates />
		</div>
	</xsl:template>

	<xsl:template match="warn">
		<div class="notice warn">
			<img src="Plotwarn.png" alt="[Warning]" class="icon"/>
			<xsl:apply-templates />
		</div>
	</xsl:template>

	<xsl:template match="danger">
		<div class="notice danger" title="Danger">
			<img src="Plotdanger.png" alt="[Danger]" class="icon"/>
			<xsl:apply-templates />
		</div>
	</xsl:template>

</xsl:stylesheet>

