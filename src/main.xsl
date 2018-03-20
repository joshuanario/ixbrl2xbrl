<!-- 

Copyright 2009, XBRL International Inc.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

This stylesheet, Main_xslt20.xsl, is provided as a convenient controller
     script, allowing you to invoke the entire pipeline of XBRL extraction
     from a single XSLT 2.0 script. -->

<!-- 2009-06-11 UPDATE: This script now incorporates Schematron input validation,
     (with the assumption that XSD schema validation has already taken place)

     See ../validator/inlineXBRL.sch for the list of validation rules
     enforced here.
-->
<xsl:stylesheet version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:svrl="http://purl.oclc.org/dsdl/svrl"
  xmlns:x="http://www.w3.org/1999/xhtml"
  exclude-result-prefixes="svrl">

  <xsl:import href="../validator/validator.xsl"/>
  <xsl:import href="Main_exslt.xsl"/>

  <!-- To override declaration in validator.xsl -->
  <xsl:output standalone="omit"/>

  <!-- By default, we terminate if Schematron validation fails.
       To disable this behavior, set this parameter to the string "true". -->
  <xsl:param name="disable-termination-on-invalid-input"/>

  <xsl:variable name="invalid-input-message"
    >Input is invalid. Please look for "failed-assert" in validation-results.xml.</xsl:variable>

  <!-- While preparing the input, also add the HTML <base> element if not already present;
       this is to support relative URI resolution in HTML content -->
  <xsl:template mode="prepare-input" match="x:head[not(x:base)]">
    <xsl:copy>
      <xsl:apply-templates mode="#current" select="@* | node()"/>
      <x:base href="{base-uri(.)}"/>
    </xsl:copy>
  </xsl:template>

  <!-- STAGE 3 (FINAL): Split the resulting set of documents, each into its own file -->
  <xsl:template match="/" name="Main">
    <!-- Get validation results -->
    <xsl:variable name="validation-results">
      <xsl:apply-templates mode="ixbrl-schematron-main" select="$IXDS"/>
    </xsl:variable>
    <!-- Store validation results in a file -->
    <xsl:result-document href="validation-results.xml">
      <xsl:copy-of select="$validation-results"/>
    </xsl:result-document>
    <!-- Warn or Terminate if there are validation errors -->
    <xsl:if test="$validation-results/svrl:schematron-output/svrl:failed-assert">
      <xsl:choose>
        <xsl:when test="$disable-termination-on-invalid-input = 'true'">
          <xsl:message              >WARNING: <xsl:value-of select="$invalid-input-message"/></xsl:message>
        </xsl:when>
        <xsl:otherwise>
          <xsl:message terminate="yes">ERROR: <xsl:value-of select="$invalid-input-message"/></xsl:message>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:apply-templates mode="split-output" select="$extracted-xbrl"/>
  </xsl:template>

  <!-- Override the use of <exsl:document>, which Saxon doesn't support -->
  <xsl:template mode="split-output" match="TargetDocument[not(@default = 'yes')]">
    <!-- Use the target ID and ".xbrl" as the file name -->
    <xsl:variable name="file-name" select="concat(@target, '.xbrl')"/>
    <xsl:result-document href="{$file-name}">
      <xsl:apply-templates mode="split-output"/>
    </xsl:result-document>
  </xsl:template>

  <!-- In case the XSLT 2.0 processor doesn't know about exsl:node-set(),
       its (trivial) implementation is provided here. -->
  <xsl:function name="exsl:node-set" xmlns:exsl="http://exslt.org/common">
    <xsl:param name="rtf-thats-already-a-node-set"/>
    <xsl:sequence select="$rtf-thats-already-a-node-set"/>
  </xsl:function>

</xsl:stylesheet>
