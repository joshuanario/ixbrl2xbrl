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

Apply this stylesheet (split-output-documents.xsl) to the
     result of extractXBRL.xsl to separate out each target
     document into its own file. If the input (result of extractXBRL.xsl)
     already consists of just one <xbrl> document, then no changes will be made.
-->
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:exsl="http://exslt.org/common"
  extension-element-prefixes="exsl">

  <xsl:template match="/">
    <xsl:apply-templates mode="split-output"/>
  </xsl:template>

  <!-- By default, copy everything unchanged -->
  <xsl:template mode="split-output" match="@* | node()">
    <xsl:copy>
      <xsl:apply-templates mode="split-output" select="@* | node()"/>
    </xsl:copy>
  </xsl:template>

  <!-- But when the document contains multiple target documents,
       then process those. -->
  <xsl:template mode="split-output" match="/TargetDocuments">
    <xsl:apply-templates mode="split-output" select="TargetDocument"/>
  </xsl:template>

          <!-- If there's a default target document, then send it to
               the primary result tree (e.g., stdout) -->
          <xsl:template mode="split-output" match="TargetDocument[@default = 'yes']">
            <xsl:apply-templates mode="split-output"/>
          </xsl:template>

          <!-- Process each target document that has a target specified -->
          <xsl:template mode="split-output" match="TargetDocument">
            <!-- Use the target ID and ".xbrl" as the file name -->
            <xsl:variable name="file-name" select="concat(@target, '.xbrl')"/>
            <!-- libxslt, Xalan, et al. should be happy with this -->
            <exsl:document href="{$file-name}">
              <xsl:apply-templates mode="split-output"/>
              <!--
              <xsl:fallback>
                <!- - Saxon will be okay with this - ->
                <xsl:result-document href="{$file-name}">
                  <xsl:apply-templates mode="split-output"/>
                </xsl:result-document>
              </xsl:fallback>
              -->
            </exsl:document>
            <!-- (MSXML is out of luck) -->
          </xsl:template>

</xsl:stylesheet>
