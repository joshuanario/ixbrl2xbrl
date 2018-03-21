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

This stylesheet, prepare-input.xsl, must be applied before applying
     extractXBRL.xsl, whether the input is a single document or a collection
     of documents.

     You can apply this stylesheet to either:
     
       1) an individual Inline XBRL Document (e.g., an XHTML doc), or
       2) an XML doc that lists a collection of Inline XBRL Documents,
          using the following format:

            <collection>
              <doc href="dir/part1.xhtml"/>
              <doc href="dir/part2.xhtml"/>
              <doc href="dir/part3.xhtml"/>
              <doc href="dir/part4.xhtml"/>
              ...
            </collection>

          (where each href value is the path or URI of a file in the collection).

          NOTE: This informal format is also used by Saxon's collection() function
                (not used here).

     The result will contain all the contents of the referenced Inline XBRL
     document files, as children of a master <IXDS> element. In addition, the
     content of each document is manipulated to facilitate further processing
     (in extractXBRL.xsl).
-->
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:ix  ="http://www.xbrl.org/2008/inlineXBRL"
  xmlns:temp="http://www.xbrl.org/2008/inlineXBRL/processor-internal-use-only"
  exclude-result-prefixes="ix temp">

  <xsl:template match="/">
    <xsl:apply-templates mode="prepare-input" select="."/>
  </xsl:template>

  <xsl:template mode="prepare-input" match="/">
    <IXDS> <!-- short for "Inline XBRL Document Set" -->
      <xsl:apply-templates mode="ixds-content" select="*"/>
    </IXDS>
  </xsl:template>

          <!-- When the input is a collection of document references,
               grab and prepare the content for each referenced document. -->
          <xsl:template mode="ixds-content" match="/collection">
            <xsl:apply-templates mode="prepare-input" select="document(doc/@href)/node()"/>
          </xsl:template>

          <!-- When the input is anything else (e.g., a single <html> document),
               just prepare its content -->
          <xsl:template mode="ixds-content" match="*">
            <xsl:apply-templates mode="prepare-input" select="."/>
          </xsl:template>


  <!-- Identity transformation -->
  <xsl:template mode="prepare-input" match="@* | node()">
    <xsl:copy>
      <xsl:apply-templates mode="prepare-input" select="@*"/>
      <xsl:apply-templates mode="insert-footnoteRef-elements" select="."/>
      <xsl:apply-templates mode="prepare-input"/>
    </xsl:copy>
  </xsl:template>

          <!-- By default, don't insert any content -->
          <xsl:template mode="insert-footnoteRef-elements" match="*"/>

          <!-- But for facts that have footnoteRefs, convert to repeating elements to
               facilitate the next processing stage -->
          <xsl:template mode="insert-footnoteRef-elements" match="*[self::ix:fraction
                                                                 or self::ix:nonFraction
                                                                 or self::ix:nonNumeric
                                                                 or self::ix:tuple] [@footnoteRefs]">
            <xsl:param name="list" select="concat(normalize-space(@footnoteRefs),' ')"/>
            <xsl:variable name="footnote-id" select="substring-before($list,' ')"/>

            <temp:footnoteRef ref="{$footnote-id}"/>

            <!-- If there are more left in the list... -->
            <xsl:if test="substring-after($list,' ')">
              <!-- Then recursively process the rest -->
              <xsl:apply-templates mode="insert-footnoteRef-elements" select=".">
                <xsl:with-param name="list" select="substring-after($list,' ')"/>
              </xsl:apply-templates>
            </xsl:if>
          </xsl:template>

</xsl:stylesheet>
