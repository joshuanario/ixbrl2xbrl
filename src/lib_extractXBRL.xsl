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

This stylesheet, extractXBRL.xsl, converts an Inline XBRL Document to an XBRL 2.1 Target Document.

     NOTE: This stylesheet must be used with a wrapper script or on pre-processed input. For details
     on how to use this stylesheet, please see the README file.

     It will work with any XSLT 1.0 or 2.0 processor.

     Template rules are indented as a convenience for reading the code, showing the 
     implicit hierarchical relationships and groupings of the rules.

     ASSUMPTION: The input is valid Inline XBRL, as constrained by the Inline XBRL specification.

     2009-03-19 - Direct questions to evan@lenzconsulting.com

-->
<!DOCTYPE xsl:stylesheet [

<!-- The following entity declarations are re-used in multiple places by the stylesheet. -->

<!-- Facts are uniquely identified by a combination of target (not shown below) and the following
     values, corresponding respectively to the XPath expressions in the entity declaration below:

       1) the local part of the fact's QName, e.g., "bar" in name="foo:bar" or name="bar"
       2) the namespace URI referenced by the prefix (or default namespace) of the fact's QName
       3) the type of fact (either "fraction", "nonFraction", "nonNumeric", or "tuple")
       4) the contextRef value (always empty for tuples)
       5) the unitRef value (always empty for tuples and nonNumeric facts)
       6) the tuple it belongs to (empty if N/A)
       7) its order within the tuple (empty if N/A)
       8) its string-value
       9) its numerator value (empty if not fraction)
       10) its denominator value (empty if not fraction)
       11) the node ID, if this fact is a tuple; in other words, never de-duplicate tuples

     Any two facts which have the same target and values for the above are assumed to be duplicates
     of each other. Duplicates will be not appear in the result.
     
     Duplicate removal is optional for Inline XBRL procesors and can be done selectively, if at all.
     Here we are removing duplicates non-exhaustively; it's possible that some duplicates will not
     be removed. In particular, XBRL 2.1 says that two items can be duplicates and have different
     values. However, a consequence of the Inline XBRL mapping rules is that certain duplicate items
     in the result, however erroneous, can't be prevented (e.g., when two items have the same name,
     context, unit, and target, but different values).  Also, since reliable identification of tuple
     duplicates requires access to the DTS, we've opted to never de-duplicate tuples.
-->
<!ENTITY composite-key-for-facts "' ', concat(substring-after(@name[contains(.,':')], ':'),
                                              @name[not(contains(.,':'))]),
                                  ' ', namespace::*[name() = substring-before(../@name, ':')],
                                  ' ', local-name(.),
                                  ' ', @contextRef,
                                  ' ', @unitRef,
                                  ' ', @tupleRef,
                                  ' ', @order,
                                  ' ', .,
                                  ' ', ix:numerator,
                                  ' ', ix:denominator,
                                  ' ', generate-id(self::ix:tuple)">

<!-- References are uniquely identified by a combination of target and the following
     values:

       1) the type of reference (either "schemaRef" or "linkbaseRef")
       2) the value of @xlink:href

     Any two references which have the same target and values for the above are assumed
     to be duplicates.
-->
<!ENTITY composite-key-for-references "' ', local-name(.),
                                       ' ', @xlink:href"> 


<!-- These are the XHTML attributes that may contain relative URIs needing to be resolved,
     whether in escaped nonNumeric output or (non-escaped) footnote content. -->
<!ENTITY relative-uri-attributes " @href
                                 | @src
                                 | @background
                                 | @longdesc
                                 | @usemap
                                 | @cite
                                 | @action
                                 | @profile
                                 | @codebase
                                 ">

<!-- For repeated use when processing the ixt-rule names -->
<!-- ASSUMPTION: The supplied format QName values use the ixt namespace.
     The template rules will work regardless of what prefix or URI is used in the QName. -->
<!ENTITY rule-name "substring-after(@format,':')">

<!-- Namespaces which will be excluded from the result -->
<!ENTITY ix    "http://www.xbrl.org/2008/inlineXBRL">
<!ENTITY temp  "http://www.xbrl.org/2008/inlineXBRL/processor-internal-use-only">
<!ENTITY xhtml "http://www.w3.org/1999/xhtml">
]>
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:ix="&ix;"
  xmlns:xbrli="http://www.xbrl.org/2003/instance"
  xmlns:link="http://www.xbrl.org/2003/linkbase"
  xmlns:xlink="http://www.w3.org/1999/xlink"
  xmlns:temp="&temp;"
  xmlns:x="&xhtml;"
  exclude-result-prefixes="ix xbrli link xlink temp x">

  <!-- For escaping inline markup when @escape is present on ix:nonNumeric -->
  <xsl:import href="lib_xml-to-string.xsl"/>

  <!-- Setting this parameter to true will prevent namespace declarations from
       ever appearing in escaped output (see hard-coded modification to xml-to-string.xsl) -->
  <xsl:param name="force-exclude-namespaces-from-escaped-output" select="false()"/>

  <!-- Helpful for HTML compatibility (though not required by the spec):
       render escaped <br/> and <hr/> as "<br />" and "<hr />" -->
  <xsl:variable name="empty-tag-end" select="' />'"/>

  <!-- Configure xml-to-string.xsl to exclude these namespace declarations -->
  <xsl:variable name="namespaces-to-exclude" select="document('')/*/temp:namespaces-to-exclude/ns"/>
  <temp:namespaces-to-exclude>
    <ns>&ix;</ns>
    <ns>&temp;</ns>
  </temp:namespaces-to-exclude>

  <!-- To override xml-to-string.xsl -->
  <xsl:output omit-xml-declaration="no"/>


  <!-- Includes the "resolve-uri" library template for resolving relative URIs -->
  <xsl:include href="lib_uri.xsl"/>


  <!-- Inline XBRL Document Set root (overridden by optional controller stylesheet) -->
  <xsl:variable name="IXDS" select="/"/>

  <!-- All facts -->
  <xsl:variable name="all-facts" select="$IXDS//ix:fraction
                                       | $IXDS//ix:nonFraction
                                       | $IXDS//ix:nonNumeric
                                       | $IXDS//ix:tuple"/>
  <!-- All references -->
  <xsl:variable name="all-references" select="$IXDS//link:schemaRef
                                            | $IXDS//link:linkbaseRef"/>

  <!-- Index of facts -->
  <xsl:key name="facts" match="ix:fraction
                             | ix:nonFraction
                             | ix:nonNumeric
                             | ix:tuple" use="concat(@target,
                                                     &composite-key-for-facts;)"/>

  <!-- Index of references -->
  <xsl:key name="references" match="link:schemaRef
                                  | link:linkbaseRef" use="concat(../@target,
                                                                  &composite-key-for-references;)"/>

  <!-- Index of targets -->
  <xsl:key name="targets" match="ix:fraction   /@target
                               | ix:nonFraction/@target
                               | ix:nonNumeric /@target
                               | ix:references /@target
                               | ix:tuple      /@target" use="."/>

  <!-- Indices for roleRef, arcroleRef, context, unit, and footnote elements -->
  <xsl:key name="roleRefs"    match="link:roleRef"    use="@roleURI"/>
  <xsl:key name="arcroleRefs" match="link:arcroleRef" use="@arcroleURI"/>
  <xsl:key name="contexts"    match="xbrli:context"   use="@id"/>
  <xsl:key name="units"       match="xbrli:unit"      use="@id"/>
  <xsl:key name="footnotes"   match="ix:footnote"     use="@footnoteID"/>


  <!-- Overrides the root rule in xml-to-string.xsl when extractXBRL.xsl is invoked by itself;
       the Main* wrapper stylesheets override this rule. -->
  <xsl:template match="/">
    <xsl:apply-templates select="*"/>
  </xsl:template>

  <xsl:template match="/*">
    <xsl:variable name="all-target-atts" select="//*[ self::ix:fraction
                                                    | self::ix:nonFraction
                                                    | self::ix:nonNumeric
                                                    | self::ix:references
                                                    | self::ix:tuple
                                                    ]
                                                    /@target"/>
    <!-- Filter duplicate target values, using the Muenchian Method -->
    <xsl:variable name="unique-target-values"
                  select="$all-target-atts[generate-id(.) = generate-id(key('targets', .)[1])]"/>
    <!-- If there are any facts or references without a target,
         then that means they belong to the default target document.
    -->
    <xsl:variable name="first-fact-slated-for-default-target"
                  select="($all-facts | //ix:references)[not(@target)][1]"/>
    <TargetDocuments>
      <!-- Decide whether to include a default target document among them -->
      <xsl:if test="$first-fact-slated-for-default-target">
        <TargetDocument default="yes">
          <xsl:call-template name="xbrl-doc"/>
        </TargetDocument>
      </xsl:if>
      <!-- Create a <TargetDocument> for each unique target value -->
      <xsl:for-each select="$unique-target-values">
        <TargetDocument target="{.}">
          <xsl:call-template name="xbrl-doc">
            <xsl:with-param name="target" select="string(.)"/>
          </xsl:call-template>
        </TargetDocument>
      </xsl:for-each>
    </TargetDocuments>
  </xsl:template>


  <!-- ###################################### -->
  <!-- Construct the XBRL 2.1 target document -->
  <!-- ###################################### -->
  <xsl:template name="xbrl-doc">
    <xsl:param name="target" select="''"/> <!-- empty string indicates the default target -->

    <!-- All references for this target, excluding duplicates using the Muenchian Method -->
    <xsl:variable name="references"
                  select="$all-references[generate-id(.) =
                                          generate-id(key('references',
                                                          concat($target, &composite-key-for-references;)
                                                         )[1])]"/>

    <!-- All facts for this target, excluding duplicates -->
    <xsl:variable name="facts"
                  select="$all-facts[generate-id(.) =
                                     generate-id(key('facts',
                                                     concat($target, &composite-key-for-facts;)
                                                    )[1])]"/>

    <!-- All top-level facts for this target, i.e., that aren't tuple children
         (whether by reference or by being embedded) -->
    <xsl:variable name="top-level-facts" select="$facts[not(@tupleRef or ancestor::ix:*[1][self::ix:tuple])]"/>
                                                                      <!-- (if nearest ix ancestor is tuple) -->

    <!-- All footnotes referenced by facts for this target -->
    <xsl:variable name="footnotes" select="key('footnotes', $facts/temp:footnoteRef/@ref)"/>

    <!-- All roleRefs referenced by footnotes for this target, with duplicates removed -->
    <xsl:variable name="roleRefs"
                  select="key('roleRefs', ( $footnotes/@footnoteRole
                                          | $footnotes/@footnoteLinkRole))[generate-id(.) =
                                                                           generate-id(key('roleRefs',
                                                                                           @roleURI)[1])]"/>

    <!-- All arcroleRefs referenced by footnotes for this target, with duplicates removed -->
    <xsl:variable name="arcroleRefs"
                  select="key('arcroleRefs', $footnotes/@arcrole)[generate-id(.) =
                                                                  generate-id(key('arcroleRefs',
                                                                                  @arcroleURI)[1])]"/>

    <!-- ASSUMPTION: Valid input contains no duplicate contexts and no duplicate units -->
    <!-- All contexts referenced by facts for this target -->
    <xsl:variable name="contexts" select="key('contexts', $facts/@contextRef)"/>

    <!-- All units referenced by facts for this target -->
    <xsl:variable name="units" select="key('units', $facts/@unitRef)"/>

    <xbrli:xbrl>

      <!-- Map attributes (@id or namespace-qualified attributes) from <ix:references> -->
      <xsl:apply-templates mode="xbrl-atts" select="$IXDS//ix:references[string(@target) = $target]/@*"/>

      <!-- Copy all the namespaces except the InlineXBRL and XHTML namespaces -->
      <xsl:copy-of select="$IXDS//x:html/namespace::*[not(. = '&ix;' or
                                                          . = '&xhtml;')]"/>

      <!-- Copy references and resources -->
      <!-- Enforce the order mandated by the XBRL 2.1 schema -->
      <xsl:apply-templates mode="replicate" select="$references[self::link:schemaRef]"/>
      <xsl:apply-templates mode="replicate" select="$references[self::link:linkbaseRef]"/>
      <xsl:apply-templates mode="replicate" select="$roleRefs"/>
      <xsl:apply-templates mode="replicate" select="$arcroleRefs"/>

      <!-- Convert just the top-level facts -->
      <xsl:apply-templates mode="fact" select="$top-level-facts"/>

      <!-- Create an extended link for each footnote -->
      <xsl:apply-templates mode="footnote-link" select="$footnotes">
        <xsl:with-param name="facts-for-target" select="$facts"/>
      </xsl:apply-templates>

      <!-- Copy the contexts and units -->
      <xsl:apply-templates mode="replicate" select="$contexts | $units"/>

    </xbrli:xbrl>
  </xsl:template>

          <!-- Copy attributes, text, comments, & PIs, as is -->
          <xsl:template mode="replicate" match="@* | comment() | text() | processing-instruction()">
            <xsl:copy/>
          </xsl:template>

          <!-- But "replicate" elements rather than copying them; that way,
               we prevent unwanted namespaces from appearing in the result. -->
          <xsl:template mode="replicate" match="*">
            <xsl:element name="{name()}" namespace="{namespace-uri()}">
              <!-- Copy all the namespaces except the InlineXBRL and XHTML namespaces.
                   This is necessary to ensure QNames are correctly resolved. We can't
                   assume that all namespace declarations will appear at the outermost
                   element of the input documents. -->
              <xsl:copy-of select="namespace::*[not(. = '&ix;' or
                                                    . = '&xhtml;')]"/>
              <xsl:apply-templates mode="replicate" select="@* | node()"/>
            </xsl:element>
          </xsl:template>


  <!-- ##################################### -->
  <!-- Convert iXBRL facts to XBRL 2.1 facts -->
  <!-- ##################################### -->

  <!-- Convert the generically named iXBRL elements into actual XBRL fact elements -->
  <xsl:template mode="fact"
                match="ix:fraction
                     | ix:nonFraction
                     | ix:nonNumeric
                     | ix:tuple">
                                   <!-- Use the in-scope namespace URI that corresponds to the given QName's prefix -->
                                   <!-- This will also work for the default namespace (no prefix) -->
    <xsl:element name="{@name}" namespace="{namespace::*[name() = substring-before(../@name, ':')]}">
      <xsl:apply-templates mode="xbrl-atts" select="@*"/>
      <!-- Force a new ID if the fact will need to participate in a footnote link -->
      <xsl:if test="@footnoteRefs">
        <xsl:attribute name="id">
          <xsl:apply-templates mode="fact-id" select="."/>
        </xsl:attribute>
      </xsl:if>
      <!-- Generate the XBRL fact's content -->
      <xsl:apply-templates mode="content" select="."/>
    </xsl:element>
  </xsl:template>

          <!-- If the fact already has an ID, use it -->
          <xsl:template mode="fact-id" match="*[@id]">
            <xsl:value-of select="@id"/>
          </xsl:template>

          <!-- Otherwise, generate a new one -->
          <xsl:template mode="fact-id" match="*">
                                         <!-- Add prefix; some processors may
                                              not generate valid standalone IDs -->
            <xsl:value-of select="concat('FACT_',generate-id())"/>
          </xsl:template>


          <!-- By default, don't copy the iXBRL attributes (not in a namespace); most are for iXBRL use only -->
          <xsl:template mode="xbrl-atts" match="@*"/>

          <!-- But do copy these through -->
          <xsl:template mode="xbrl-atts" match="@id
                                              | @contextRef
                                              | @unitRef
                                              | @decimals
                                              | @precision">
            <xsl:copy/>
          </xsl:template>

          <!-- Also, copy all attributes that are in a namespace (e.g., user-defined, extension attributes, or @xml:*) -->
          <xsl:template mode="xbrl-atts" match="@*[string(namespace-uri(.))]">
            <xsl:copy/>
          </xsl:template>


  <!-- ########################################################################### -->
  <!-- The mode="content" rules create the content of each resulting XBRL 2.1 fact -->
  <!-- ########################################################################### -->

  <!-- For tuple content, find all the tuple's children and convert them to XBRL facts -->
  <xsl:template mode="content" match="ix:tuple">
    <!-- Get all descendant facts whose nearest ix ancestor is the current fact -->
    <xsl:variable name="embedded-children" select="( .//ix:fraction
                                                   | .//ix:nonFraction
                                                   | .//ix:nonNumeric
                                                   | .//ix:tuple
                                                   )
                                                   [generate-id(ancestor::ix:*[1]) =
                                                    generate-id(current())]"/>

    <!-- Get all facts that are included by reference -->
    <xsl:variable name="referenced-children" select="$all-facts[@tupleRef = current()/@tupleID]"/>

    <!-- Process the union of the embedded and referenced children... -->
    <xsl:apply-templates mode="fact" select="$embedded-children | $referenced-children">
      <!-- ...sorting them by the "order" attribute -->
      <xsl:sort select="@order" data-type="number"/>
    </xsl:apply-templates>
  </xsl:template>


  <!-- Render nonNumeric content -->
  <xsl:template mode="content" match="ix:nonNumeric">
    <!-- Get the effective value of the element (minus exclusions and optionally escaped) -->
    <xsl:variable name="effective-value">
      <xsl:apply-templates mode="nonNumeric-value" select="."/>
    </xsl:variable>
    <xsl:choose>
      <!-- Only normalize whitespace when we're running the value through an ixt transformation rule -->
      <xsl:when test="@format">
        <!-- Apply any applicable transformation rules to ix:nonNumeric's value -->
        <xsl:apply-templates mode="ixt-rule" select=".">
          <xsl:with-param name="value" select="normalize-space($effective-value)"/>
        </xsl:apply-templates>
      </xsl:when>
      <!-- Otherwise, don't normalize the whitespace -->
      <xsl:otherwise>
        <xsl:value-of select="$effective-value"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

          <!-- Normally, just concatenate text nodes... -->
          <xsl:template mode="nonNumeric-value" match="ix:nonNumeric">
            <xsl:apply-templates mode="nonNumeric-text"/>
          </xsl:template>

                  <!-- Copy all descendant text nodes except those beneath a descendant <ix:exclude> -->
                  <xsl:template mode="nonNumeric-text" match="ix:exclude"/>


          <!-- But when escape="true", then convert elements to escaped markup -->
          <xsl:template mode="nonNumeric-value" match="ix:nonNumeric[normalize-space(@escape) = '1'
                                                                  or normalize-space(@escape) = 'true']">
            <xsl:call-template name="xml-to-string">
              <xsl:with-param name="node-set" select="node()"/>
            </xsl:call-template>
          </xsl:template>

                  <!-- Strip out <ix:exclude> (and any temp:* elements) -->
                  <xsl:template mode="xml-to-string" match="ix:exclude | temp:*"/>

                  <!-- Strip out the start & end tags of any other ix:* elements -->
                  <xsl:template mode="xml-to-string" match="ix:*">
                    <xsl:param name="depth"/>
                    <xsl:apply-templates mode="xml-to-string">
                      <!-- Babysit the xml-to-string code's way of keeping track of namespace declarations;
                           don't increment the depth, since we're not outputting an element here -->
                      <xsl:with-param name="depth" select="$depth"/>
                    </xsl:apply-templates>
                  </xsl:template>

                  <!-- Attempt to resolve relative URIs -->
                  <xsl:template mode="xml-to-string" match="&relative-uri-attributes;">
                    <!-- In XSLT 1.0, for getting the base URI, we're limited to querying the XHTML <base> element -->
                    <xsl:variable name="base-uri" select="string(ancestor::x:html/x:head/x:base/@href)"/>
                    <xsl:choose>
                      <!-- If the base URI isn't present, then we can't improve on things -->
                      <xsl:when test="not($base-uri)">
                        <!-- Just use the normal rule for serializing attributes -->
                        <xsl:apply-imports/>
                      </xsl:when>
                      <!-- But if we *do* have the base URI, then let's resolve the current
                           URI reference using it -->
                      <xsl:otherwise>
                        <xsl:variable name="resolved-uri">
                          <xsl:call-template name="uri:resolve-uri" xmlns:uri="http://xsltsl.org/uri">
                            <xsl:with-param name="reference" select="string(.)"/>
                            <xsl:with-param name="base"      select="$base-uri"/>
                          </xsl:call-template>
                        </xsl:variable>
                        <!-- Serialize the attribute but override its value -->
                        <xsl:call-template name="serialize-attribute">
                          <xsl:with-param name="att-value" select="string($resolved-uri)"/>
                        </xsl:call-template>
                      </xsl:otherwise>
                    </xsl:choose>
                  </xsl:template>


  <!-- Convert <ix:numerator> and <ix:denominator> to their XBRL counterparts -->
  <xsl:template mode="content" match="ix:fraction">
    <xsl:apply-templates mode="fraction-child" select="ix:numerator | ix:denominator"/>
  </xsl:template>

          <xsl:template mode="fraction-child" match="ix:numerator | ix:denominator">
            <xsl:element name="xbrli:{local-name()}">
              <xsl:apply-templates mode="xbrl-atts" select="@*"/>
              <xsl:apply-templates mode="content" select="."/>
            </xsl:element>
          </xsl:template>


  <!-- Transform numeric values, according to the values of "sign", "format", and "scale" -->
  <xsl:template mode="content" match="ix:nonFraction
                                    | ix:numerator
                                    | ix:denominator">
    <!-- Add minus sign if @sign is present -->
    <!-- ASSUMPTION: Input is valid; an actual minus sign, if present, will appear outside the element content -->
    <xsl:if test="@sign and string(.)"> <!-- make sure the content isn't empty, as when xsi:nil="true" -->
      <xsl:text>-</xsl:text>
    </xsl:if>

    <!-- Get the result of applying the ixt transformation rule (if applicable) -->
    <xsl:variable name="normalized-value">
      <xsl:apply-templates mode="ixt-rule" select=".">
        <!-- In any case, normalize the whitespace -->
        <xsl:with-param name="value" select="normalize-space(.)"/>
      </xsl:apply-templates>
    </xsl:variable>

    <!-- Apply @scale, if present -->
    <!-- This approach uses lexical rather than numeric manipulation to apply the
         scaling factor. There are a couple of benefits to this approach:
         
           1. It avoids accuracy/precision problems with numeric computations
              (e.g., processors outputting scientific notation or garbage data).
           2. Less important but nice to have: it preserves lexical details of
              the source, specifically the number of trailing zeroes that are present.
    -->
    <xsl:choose>
      <!-- If @scale isn't present, then just output the normalized value as is -->
      <xsl:when test="not(@scale)">
        <xsl:value-of select="$normalized-value"/>
      </xsl:when>
      <!-- Otherwise, move the decimal point accordingly -->
      <xsl:otherwise>

        <!-- To get the mantissa... -->
        <xsl:variable name="mantissa">
          <!-- First, remove the decimal point -->
          <xsl:variable name="decimal-stripped" select="translate($normalized-value, '.', '')"/>
          <!-- Then, remove leading zeroes -->
          <xsl:call-template name="strip-leading-zeroes">
            <xsl:with-param name="string" select="$decimal-stripped"/>
          </xsl:call-template>
        </xsl:variable>
        <!-- The initial exponent is always 0 or below -->
        <xsl:variable name="initial-exponent">
          <xsl:choose>
            <!-- If there's no decimal point, then the initial exponent is 0 -->
            <xsl:when test="not(contains($normalized-value, '.'))">0</xsl:when>
            <!-- Otherwise, the initial exponent is (the complement of)
                 how many decimal places there are in the given value -->
            <xsl:otherwise>
              <xsl:value-of select="-(string-length(substring-after($normalized-value, '.')))"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <!-- Add scale to get the new exponent -->
        <xsl:variable name="new-exponent" select="$initial-exponent + @scale"/>

        <!-- Finally, output the result of applying the new exponent to our mantissa -->
        <xsl:choose>
          <!-- If we're scaling up (using a positive power of ten) -->
          <xsl:when test="$new-exponent >= 0">
            <!-- Output the mantissa... -->
            <xsl:value-of select="$mantissa"/>
            <!-- followed by 0 or more zeroes. -->
            <xsl:call-template name="zeroes">
              <xsl:with-param name="how-many" select="$new-exponent"/>
            </xsl:call-template>
          </xsl:when>
          <!-- Otherwise, we need to add a decimal point (because the exponent is negative) -->
          <xsl:otherwise>
            <xsl:variable name="decimal-places"  select="-($new-exponent)"/>
            <xsl:variable name="mantissa-length" select="string-length($mantissa)"/>
            <xsl:choose>
              <!-- If the decimal point appears inside the mantissa, then split it up -->
              <xsl:when test="$mantissa-length > $decimal-places">
                <xsl:variable name="left-side-digits" select="$mantissa-length - $decimal-places"/>
                <!-- Left-side digits -->
                <xsl:value-of select="substring($mantissa, 1, $left-side-digits)"/>
                <xsl:text>.</xsl:text>
                <!-- Right-side digits -->
                <xsl:value-of select="substring($mantissa, 1 + $left-side-digits)"/>
              </xsl:when>
              <!-- Otherwise, lead with the decimal point -->
              <xsl:otherwise>
                <xsl:text>0.</xsl:text>
                <!-- Pad with as many zeroes as necessary -->
                <xsl:call-template name="zeroes">
                  <xsl:with-param name="how-many" select="$decimal-places - $mantissa-length"/>
                </xsl:call-template>
                <!-- The mantissa itself -->
                <xsl:value-of select="$mantissa"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

          <!-- Recursively remove all leading zeroes from the given string -->
          <xsl:template name="strip-leading-zeroes">
            <xsl:param name="string"/>
            <xsl:choose>
              <xsl:when test="starts-with($string, '0')">
                <xsl:call-template name="strip-leading-zeroes">
                  <xsl:with-param name="string" select="substring($string,2)"/>
                </xsl:call-template>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="$string"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:template>

          <!-- Output the given number of zeroes -->
          <xsl:template name="zeroes">
            <xsl:param name="how-many"/>
            <xsl:if test="$how-many >= 1">
              <xsl:text>0</xsl:text>
              <xsl:call-template name="zeroes">
                <xsl:with-param name="how-many" select="$how-many - 1"/>
              </xsl:call-template>
            </xsl:if>
          </xsl:template>


  <!-- ####################################################################################### -->
  <!-- The mode="ixt-rule" rules convert numbers and dates according to the "format" property -->
  <!-- ####################################################################################### -->

          <!-- By default, don't apply any format conversions -->
          <xsl:template mode="ixt-rule" match="*">
            <xsl:param name="value"/>
            <xsl:value-of select="$value"/>
          </xsl:template>

          <xsl:template mode="ixt-rule" match="*[&rule-name; = 'numcomma']">
            <xsl:param name="value"/>
            <!-- Convert commas to periods -->
            <xsl:value-of select="translate($value,',','.')"/>
          </xsl:template>

          <xsl:template mode="ixt-rule" match="*[&rule-name; = 'numcommadot']">
            <xsl:param name="value"/>
            <!-- Remove commas -->
            <xsl:value-of select="translate($value,',','')"/>
          </xsl:template>

          <xsl:template mode="ixt-rule" match="*[&rule-name; = 'numdash']">
            <!-- Input: "-"; Output: "0" -->
            <xsl:text>0</xsl:text>
          </xsl:template>

          <xsl:template mode="ixt-rule" match="*[&rule-name; = 'numdotcomma']">
            <xsl:param name="value"/>
            <!-- Remove periods and convert commas to periods -->
            <xsl:value-of select="translate($value,',.','.')"/>
          </xsl:template>

          <xsl:template mode="ixt-rule" match="*[&rule-name; = 'numspacecomma']">
            <xsl:param name="value"/>
            <!-- Convert commas to periods and remove spaces -->
            <xsl:value-of select="translate($value,', ','.')"/>
          </xsl:template>

          <xsl:template mode="ixt-rule" match="*[&rule-name; = 'numspacedot']">
            <xsl:param name="value"/>
            <!-- Remove spaces -->
            <xsl:value-of select="translate($value,' ','')"/>
          </xsl:template>


          <!-- Convert to ISO month-day format -->
          <xsl:template mode="ixt-rule" match="*[contains(&rule-name;, 'monthday') or
                                                 contains(&rule-name;, 'daymonth')]" priority="1">
            <xsl:param name="value"/>
            <xsl:text>--</xsl:text>
            <!-- Month -->
            <xsl:apply-templates mode="month" select=".">
              <xsl:with-param name="date" select="$value"/>
            </xsl:apply-templates>
            <xsl:text>-</xsl:text>
            <!-- Day -->
            <xsl:apply-templates mode="day" select=".">
              <xsl:with-param name="date" select="$value"/>
            </xsl:apply-templates>
          </xsl:template>

          <!-- For all other dates, Convert to ISO date or year-month format -->
          <xsl:template mode="ixt-rule" match="*[starts-with(&rule-name;, 'date')]">
            <xsl:param name="value"/>
            <!-- Year -->
            <xsl:apply-templates mode="year" select=".">
              <xsl:with-param name="date" select="$value"/>
            </xsl:apply-templates>
            <xsl:text>-</xsl:text>
            <!-- Month -->
            <xsl:apply-templates mode="month" select=".">
              <xsl:with-param name="date" select="$value"/>
            </xsl:apply-templates>

            <xsl:variable name="is-year-month" select="contains(&rule-name;,'yearmonth')
                                                    or contains(&rule-name;,'monthyear')"/>
            <xsl:if test="not($is-year-month)">
              <xsl:text>-</xsl:text>
              <!-- Day -->
              <xsl:apply-templates mode="day" select=".">
                <xsl:with-param name="date" select="$value"/>
              </xsl:apply-templates>
            </xsl:if>
          </xsl:template>

                  <xsl:template mode="year" match="*" name="do-year">
                    <xsl:param name="date"/>
                    <xsl:variable name="third-to-last-character" select="substring($date, string-length($date) - 2, 1)"/>
                    <!-- If it's a number, then the full year is present -->
                    <xsl:variable name="contains-full-year" select="string(number($third-to-last-character)) != 'NaN'"/>
                    <xsl:choose>
                      <xsl:when test="$contains-full-year">
                        <!-- last four characters -->
                        <xsl:value-of select="substring($date, string-length($date) - 3)"/>
                      </xsl:when>
                      <xsl:otherwise>
                        <!-- Assume the 21st century -->
                        <xsl:text>20</xsl:text>
                        <!-- last two characters -->
                        <xsl:value-of select="substring($date, string-length($date) - 1)"/>
                      </xsl:otherwise>
                    </xsl:choose>
                  </xsl:template>

                  <xsl:template mode="year" match="*[&rule-name; = 'datelongyearmonth' or
                                                     &rule-name; = 'dateshortyearmonth']"> 
                    <xsl:param name="date"/>
                    <xsl:call-template name="do-year">
                      <xsl:with-param name="date" select="substring-before($date, ' ')"/>
                    </xsl:call-template>
                  </xsl:template>


                  <!-- When the day comes after the month number -->
                  <xsl:template mode="day" match="*[&rule-name; = 'dateslashus' or
                                                    &rule-name; = 'datedotus']">
                    <xsl:param name="date"/>
                    <xsl:variable name="sRest" select="concat(substring-after($date,'/'),substring-after($date,'.'))"/>
                    <xsl:variable name="sDay" select="concat(substring-before($sRest,'/'),substring-before($sRest,'.'))"/>
                    <xsl:value-of select="format-number(number($sDay),'00')"/>
                  </xsl:template>

                  <xsl:template mode="day" match="*[&rule-name; = 'dateslashmonthdayus']">
                    <xsl:param name="date"/>
                    <xsl:value-of select="format-number(substring-after($date, '/'), '00')"/>
                  </xsl:template>


                  <!-- When the day comes after the month name (or abbreviation) -->
                  <xsl:template mode="day" match="*[&rule-name; = 'datelongus' or
                                                    &rule-name; = 'dateshortus']">
                    <xsl:param name="date"/>
                    <xsl:variable name="sDate" select="normalize-space($date)"/>
                    <xsl:variable name="sRest" select="substring-after($sDate,' ')"/>
                    <xsl:variable name="sDay" select="substring-before($sRest,',')"/>
                    <xsl:value-of select="format-number(number($sDay),'00')"/>
                  </xsl:template>

                  <xsl:template mode="day" match="*[&rule-name; = 'datelongmonthdayus' or
                                                    &rule-name; = 'dateshortmonthdayus']">
                    <xsl:param name="date"/>
                    <xsl:value-of select="format-number(substring-after($date, ' '), '00')"/>
                  </xsl:template>


                  <!-- In every other case, the day is the first token -->
                  <xsl:template mode="day" match="*">
                    <xsl:param name="date"/>
                    <xsl:variable name="sDate" select="translate($date,'./,abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ','   ')"/>
                    <xsl:variable name="sDay" select="substring-before($sDate,' ')"/>
                    <xsl:value-of select="format-number(number($sDay),'00')"/>
                  </xsl:template>



                  <!-- When the month is a number and the first token -->
                  <xsl:template mode="month" match="*[&rule-name; = 'dateslashus' or
                                                      &rule-name; = 'datedotus' or
                                                      &rule-name; = 'dateslashmonthdayus']">
                    <xsl:param name="date"/>
                    <xsl:variable name="sMonth" select="concat(substring-before($date,'/'),substring-before($date,'.'))"/>
                    <xsl:value-of select="format-number(number($sMonth),'00')"/>
                  </xsl:template>

                  <!-- When the month is a number that comes after the day -->
                  <xsl:template mode="month" match="*[&rule-name; = 'dateslasheu' or
                                                      &rule-name; = 'datedoteu']">
                    <xsl:param name="date"/>
                    <xsl:variable name="sDate" select="normalize-space($date)"/>
                    <xsl:variable name="sRest" select="concat(substring-after($sDate,'/'),substring-after($sDate,'.'))"/>
                    <xsl:variable name="sMonth" select="concat(substring-before($sRest,'/'),substring-before($sRest,'.'))"/>
                    <xsl:value-of select="format-number(number($sMonth),'00')"/>
                  </xsl:template>

                  <xsl:template mode="month" match="*[&rule-name; = 'dateslashdaymontheu']">
                    <xsl:param name="date"/>
                    <xsl:value-of select="format-number(substring-after($date, '/'), '00')"/>
                  </xsl:template>


                  <!-- When the month name (or abbreviation) comes first -->
                  <xsl:template mode="month" match="*[&rule-name; = 'datelongus'
                                                   or &rule-name; = 'dateshortus'
                                                   or &rule-name; = 'datelongmonthdayus'
                                                   or &rule-name; = 'dateshortmonthdayus'
                                                   or &rule-name; = 'datelongmonthyear'
                                                   or &rule-name; = 'dateshortmonthyear']">
                    <xsl:param name="date"/>
                    <xsl:call-template name="month-number">
                      <xsl:with-param name="month-name" select="substring-before($date, ' ')"/>
                    </xsl:call-template>
                  </xsl:template>

                  <!-- When the month name (or abbreviation) comes after the day -->
                  <xsl:template mode="month" match="*[&rule-name; = 'datelonguk'
                                                   or &rule-name; = 'dateshortuk']">
                    <xsl:param name="date"/>
                    <xsl:variable name="sDate" select="normalize-space($date)"/>
                    <xsl:variable name="sRest" select="substring-after($sDate,' ')"/>
                    <xsl:variable name="sMonth" select="concat(substring-before($sRest,','),substring-before($sRest,' '))"/>
                    <xsl:call-template name="month-number">
                      <xsl:with-param name="month-name" select="$sMonth"/>
                    </xsl:call-template>
                  </xsl:template>

                  <!-- When the month name (or abbreviation) comes after the year or day *and* comes last -->
                  <xsl:template mode="month" match="*[&rule-name; = 'datelongyearmonth'
                                                   or &rule-name; = 'dateshortyearmonth'
                                                   or &rule-name; = 'datelongdaymonthuk'
                                                   or &rule-name; = 'dateshortdaymonthuk']">
                    <xsl:param name="date"/>
                    <xsl:call-template name="month-number">
                      <xsl:with-param name="month-name" select="substring-after($date, ' ')"/>
                    </xsl:call-template>
                  </xsl:template>


                          <xsl:template name="month-number">
                            <xsl:param name="month-name"/>
                            <xsl:choose>
                              <xsl:when test="starts-with($month-name,'Jan')">01</xsl:when>
                              <xsl:when test="starts-with($month-name,'Feb')">02</xsl:when>
                              <xsl:when test="starts-with($month-name,'Mar')">03</xsl:when>
                              <xsl:when test="starts-with($month-name,'Apr')">04</xsl:when>
                              <xsl:when test="starts-with($month-name,'May')">05</xsl:when>
                              <xsl:when test="starts-with($month-name,'Jun')">06</xsl:when>
                              <xsl:when test="starts-with($month-name,'Jul')">07</xsl:when>
                              <xsl:when test="starts-with($month-name,'Aug')">08</xsl:when>
                              <xsl:when test="starts-with($month-name,'Sep')">09</xsl:when>
                              <xsl:when test="starts-with($month-name,'Oct')">10</xsl:when>
                              <xsl:when test="starts-with($month-name,'Nov')">11</xsl:when>
                              <xsl:when test="starts-with($month-name,'Dec')">12</xsl:when>
                            </xsl:choose>
                          </xsl:template>


  <!-- ########################################## -->
  <!-- Create an extended link for each footnote. -->
  <!-- ########################################## -->

  <!-- Construct each <link:footnoteLink> element -->
  <xsl:template mode="footnote-link" match="ix:footnote">
    <xsl:param name="facts-for-target"/>
    <xsl:variable name="link-role">
      <xsl:apply-templates mode="link-role" select="."/>
    </xsl:variable>
    <xsl:variable name="arcrole">
      <xsl:apply-templates mode="arcrole" select="."/>
    </xsl:variable>
    <xsl:variable name="footnote-role">
      <xsl:apply-templates mode="footnote-role" select="."/>
    </xsl:variable>
    <!-- The value of @xml:lang that's in scope for this element;
         in valid input, this will always be present. -->
    <xsl:variable name="lang">
      <xsl:value-of select="(ancestor-or-self::*/@xml:lang)[last()]"/>
    </xsl:variable>
    <xsl:variable name="footnoteID" select="@footnoteID"/>
    <link:footnoteLink xlink:type="extended" xlink:role="{$link-role}">
      <!-- Copy the nearest xml:base value that's in effect, if applicable. -->
      <xsl:copy-of select="(ancestor-or-self::*/@xml:base)[last()]"/>
      <!-- Add a locator for each fact that this footnote pertains to -->
      <xsl:for-each select="$facts-for-target[temp:footnoteRef/@ref = $footnoteID]">
        <!-- Get the (possibly auto-generated) fact ID -->
        <xsl:variable name="fact-id">
          <xsl:apply-templates mode="fact-id" select="."/>
        </xsl:variable>
        <!-- For purposes of determining the "order" attribute on the link:footnoteArc element -->
        <!-- Note: The "order" attribute is not required by Inline XBRL and in fact is of
             limited use for rendering, since it doesn't resolve ordering ambiguities when 
             two facts reference the same footnote. More to the point, what's needed (and what
             neither Inline XBRL nor XBRL 2.1 provides) is a way to order footnotes, not just
             references to footnotes for a given fact.
        -->
        <xsl:variable name="footnote-refs-before-this-one"
                      select="count(temp:footnoteRef[@ref = $footnoteID]
                                    /preceding-sibling::temp:footnoteRef)"/>

        <link:loc xlink:type="locator" xlink:href="#{$fact-id}"
                                       xlink:label="fact{position()}"/>

        <link:footnoteArc xlink:type="arc" xlink:from="fact{position()}"
                                           xlink:to="footnote"
                                           xlink:arcrole="{$arcrole}"
                                           order="{format-number(1 + $footnote-refs-before-this-one,
                                                                 '#.0')}"/>
      </xsl:for-each>

      <link:footnote xlink:type="resource" xlink:label="footnote"
                                           xlink:role="{$footnote-role}"
                                           xml:lang="{$lang}">

        <xsl:apply-templates mode="footnote-title-att" select="."/>

        <xsl:apply-templates mode="xbrl-atts" select="@*"/>

        <xsl:apply-templates mode="footnote-content"/>
      </link:footnote>
    </link:footnoteLink>
  </xsl:template>
                                                   <!-- default value for <link:footnoteLink>'s xlink:role -->
          <xsl:template mode="link-role" match="*">http://www.xbrl.org/2003/role/link</xsl:template>
          <xsl:template mode="link-role" match="*[@footnoteLinkRole]">
            <xsl:value-of select="@footnoteLinkRole"/>
          </xsl:template>

                                                 <!-- default value for <link:footnoteArc>'s xlink:arcrole -->
          <xsl:template mode="arcrole" match="*">http://www.xbrl.org/2003/arcrole/fact-footnote</xsl:template>
          <xsl:template mode="arcrole" match="*[@arcrole]">
            <xsl:value-of select="@arcrole"/>
          </xsl:template>

                                                       <!-- default value for <link:footnote>'s xlink:role -->
          <xsl:template mode="footnote-role" match="*">http://www.xbrl.org/2003/role/footnote</xsl:template>
          <xsl:template mode="footnote-role" match="*[@footnoteRole]">
            <xsl:value-of select="@footnoteRole"/>
          </xsl:template>
          
          <!-- Only output @xlink:title if @title is present on <ix:footnote> -->
          <xsl:template mode="footnote-title-att" match="*"/>
          <xsl:template mode="footnote-title-att" match="ix:footnote[@title]">
            <xsl:attribute name="xlink:title">
              <xsl:value-of select="@title"/>
            </xsl:attribute>
          </xsl:template>

          
          <!-- For footnote content, copy attributes, text, comments, & PIs, as is -->
          <xsl:template mode="footnote-content" match="@* | comment() | text() | processing-instruction()">
            <xsl:copy/>
          </xsl:template>

          <!-- Attempt to resolve relative URIs -->
          <xsl:template mode="footnote-content" match="&relative-uri-attributes;">
            <xsl:attribute name="{name()}" namespace="{namespace-uri()}">
              <!-- In XSLT 1.0, for getting the base URI, we're limited to querying the XHTML <base> element -->
              <xsl:variable name="base-uri" select="string(ancestor::x:html/x:head/x:base/@href)"/>
              <xsl:choose>
                <!-- If the base URI isn't present, then we can't improve on things -->
                <xsl:when test="not($base-uri)">
                  <!-- Just use the given value -->
                  <xsl:value-of select="."/>
                </xsl:when>
                <!-- But if we *do* have the base URI, then let's resolve the current
                     URI reference using it -->
                <xsl:otherwise>
                  <xsl:call-template name="uri:resolve-uri" xmlns:uri="http://xsltsl.org/uri">
                    <xsl:with-param name="reference" select="string(.)"/>
                    <xsl:with-param name="base"      select="$base-uri"/>
                  </xsl:call-template>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:attribute>
          </xsl:template>

          <!-- But "replicate" elements rather than copying them; that way,
               we prevent unwanted namespaces from appearing in the result. -->
          <xsl:template mode="footnote-content" match="*">
            <xsl:element name="{name()}" namespace="{namespace-uri()}">
              <xsl:apply-templates mode="footnote-content" select="@* | node()"/>
            </xsl:element>
          </xsl:template>

          <!-- Don't process <ix:exclude> and its descendants -->
          <xsl:template mode="footnote-content" match="ix:exclude"/>

          <!-- For other ix:* elements, just process their content -->
          <xsl:template mode="footnote-content" match="ix:*">
            <xsl:apply-templates mode="footnote-content"/>
          </xsl:template>

</xsl:stylesheet>
