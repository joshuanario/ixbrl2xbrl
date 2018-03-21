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

This stylesheet, Main_exslt.xsl, is provided as a convenient controller
     script, allowing you to invoke the entire pipeline of XBRL extraction
     from a single (E)XSLT 1.0 script. -->
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:exsl="http://exslt.org/common"
  exclude-result-prefixes="exsl"
  extension-element-prefixes="exsl">

  <!-- STAGE 1 -->
  <xsl:import href="lib_prepare-input.xsl"/>
  <!-- STAGE 2 -->
  <xsl:import href="lib_extractXBRL.xsl"/>
  <!-- STAGE 3 -->
  <xsl:import href="lib_split-output-documents.xsl"/>
    

  <!-- Provided to enable override by wrapper scripts (e.g., test frameworks) -->
  <xsl:variable name="input-doc" select="/"/>

  <!-- STAGE 1: Prepare input document(s), retrieving each doc
                in the collection, if it's a collection. -->
  <xsl:variable name="prepared-input">
    <xsl:apply-templates mode="prepare-input" select="$input-doc"/>
  </xsl:variable>

  <!-- STAGE 2: Extract the XBRL into one or more XBRL target documents -->
  <xsl:variable name="prepared-input-ns" select="exsl:node-set($prepared-input)"/>
  <xsl:variable name="IXDS" select="$prepared-input-ns"/>
  <xsl:variable name="extracted-xbrl">
    <xsl:apply-templates select="$prepared-input-ns/*"/>
  </xsl:variable>


  <!-- STAGE 3 (FINAL): Split the resulting set of documents, each into its own file -->
  <xsl:template match="/" name="Main">

    <xsl:apply-templates mode="split-output" select="exsl:node-set($extracted-xbrl)"/>
    <!-- for debugging only -->
    <!--
    <exsl:document href="DEBUG/prepared-input.xml">
      <xsl:copy-of select="$prepared-input-ns"/>
    </exsl:document>
    -->

    <!-- for debugging only -->
    <!--
    <exsl:document href="DEBUG/extracted-xbrl.xml">
      <xsl:copy-of select="$extracted-xbrl"/>
    </exsl:document>
    -->
  </xsl:template>

</xsl:stylesheet>
