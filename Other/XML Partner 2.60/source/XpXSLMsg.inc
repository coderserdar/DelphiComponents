(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower XMLPartner
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
{$DEFINE LANG_EN}

resourcestring

{*** English ***}
{$IFDEF LANG_EN}
  sXSLNotTopLevel = '%s must be a top-level element.';
  sXSLMatchOrNameAttrib = 'The xsl:template element must have a name or match attribute.';
  sXSLInvalidXSLTName = '%s is not a supported XSLT element.';
  sXSLInvalidTopLvlElement = '%s may not be a top-level element because it does not have a prefix or does not have a non-null namespace URI.';
  sXSLNotInBody = 'Element %s must be in a template body.';
  sXSLInvalidAttrib = 'The value %s may not be specified for attribute %s of element %s.';
  sXSLCorrectValue = ' The correct value is %s.';
  sXSLTextOneChild = 'xsl:text elements may not have more than one child text node.';
  sXSLTextNotText = 'The child node of the xsl:text element must be a text node.';
  sXSLWhenRequired = 'An xsl:choose element must contain at least one xsl:when element.';
  sXSLInvalidChooseElement = 'An xsl:choose element may not contain %s elements.';
  sXSLNotInChoose = 'Element %s must be contained within an xsl:choose element.';
  sXSLMustBeEmpty = 'Element %s must be empty (i.e., it may not have any child nodes).';
  sXSLAttribReq = 'The mandatory %s attribute is missing for element %s.';
  sXSLNotStylesheet = 'The document element of %s is not an xsl:stylesheet element.';
  sXSLImportPosition = 'An xsl:import element must precede all other child elements of xsl:stylesheet.';
  sLocPathPatternInvalidFunc = 'Function %s not allowed in location path pattern %s. Only the id() and key() functions are allowed.';
  sPatternRequireLiteral = 'Expected literal number or string in pattern %s.';
  sPatternRequireSlash = '''/'' or ''//'' required in pattern %s.';
  sPatternInvalidAxis = 'Axis specifier %s is invalid for a pattern. Only attribute:: & child:: are allowed.';
  sPatternUnexpectedToken = 'Unexpected token %s found in pattern.';
  sPatternRequireLeftParen = 'Left parenthesis required after %s.';
  sPatternRequireRightParen = 'Right parenthesis required after %s.';
  sPatternRequirePILiteral = 'Only literal string values may be specified for processing-instruction().';
  sPatternNoMatchingRFRAME = 'Could not find matching right brace for predicate in pattern %s.';
  sXSLUnresolvedNSPrefix = 'Could not resolve namespace prefix %s for %s.';
  sPatternRequireUnion = 'Expected Union ("|") character but found %s.';
  sXSLTemplateMatch = 'Found more than one template matching node %s. First template: %s in %s. Second template: %s in %s';
  sXSLParamPosition = 'xsl:param %s must be positioned as either a top-level element or before all other sibling elements.';
  sXSLVarNotBoth = '%s %s must have a select attribute or template body but not both.';
  sXSLInvalidXSLStyleRef = 'Invalid values (%s) specified xml-stylesheet element.';
  sXSLVarNameRequired = 'An xsl:variable element must have a name attribute.';
  sXSLDOMRequired = 'No XML document was specified for processing.';
  sXSLCannotClearVars = 'Global parameters & variables may not be cleared while a stylesheet is being applied.';
  sXSLWithParamParent = 'An xsl:when element must have an xsl:apply-templates or xsl:call-template element as its parent.';
  sXSLSelectRequired = '%s does not have a mandatory select attribute.';
  sXSLSortParent = 'An xsl:sort element must have an xsl:apply-templates or xsl:for-each element as its parent.';
  sXSLSortPrecedingSibling = 'When used in an xsl:for-each element, the xsl:sort elements must occur before all other child elements.';
  sXSLSortAttribInvalid = 'The %s attribute of xsl:sort has the value %s. It must have the value %s or %s.';
  sXSLInvalidStylesheet = 'The root element of the stylesheet is %s but should be an xsl:stylesheet element.';
  sXSLCharAttrib = 'The value of attribute %s for element %s must be a single character. It''s current value is %s';
  sXSLDefaultDecFmtConflict = 'More than one default decimal format was encountered and they are not compatible (see Section 12.3 of XSLT specification).';
  sXSLNamedDecFmtConflict = 'Two instances of decimal format %s were encountered and they are not compatible (see Section 12.3 of XSLT specification).';
  sXSLNoDecimalFormat = 'Could not find a xsl:decimal-format element having name %s';
  sXSLMalformedPattern = 'Malformed format-number pattern (%s): %s.';
  sXSLInvalidSuffixChars = 'Invalid characters in positive or negative suffix';
  sXSLNoDigitsPosPatt = 'No digits found in positive pattern. Make sure pattern separator is correctly placed';
  sXSLDupMultipliers = 'More than one multiplier (i.e., percent, per mille) found in pattern';
  sXSLMultDecSep = 'Pattern contains more than one decimal separator';
  sXSLUnexpectedZeroDigit = 'Unexpected zero digit encountered';
  sXSLDecSepPos = 'Digit character (i.e., #) encountered after zero digits but before decimal separator';
  sXSLMsgTerminate = 'The terminate attribute of xsl:message was assigned the value %s. Instead, it must have value "yes" or "no".';
  sXSLTerminatedByMsg = 'XSL processor execution terminated by xsl:message element.';
  sXSLOnlyTextNodes = 'An %s template body generates non-text nodes. Only text nodes may be generated.';
  sXSLCommentChars = 'An xsl:comment element generates a comment that either contains the characters "--" or ends with "-".';
  sXSLPINameIsXML = 'An xsl:processing-instruction has a name attribute evaluating to the value "xml" or a variant thereof.';
  sXSLPINameColon = 'An xsl:processing-instruction has the value %s for its name attribute. Colons are not allowed in PI names.';
  sXSLPINameInvalid = 'An xsl:processing-instruction has the value %s for its name attribute. However, the character %s is not allowed in PI names.';
  sXSLAttribNoCurrentElement = 'No current element in result tree when executing xsl:attribute with name %s and namespace %s.';
  sXSLAttribElNoChildNodes = 'The current element in the result tree already has child nodes when trying to execute xsl:attribute with name %s and namespace %s.';
  sXSLAttribNoXMLNS = 'Invalid attribute name: The xsl:attribute element with attributes name %s and namespace %s generated the attribute name %s.';
  sXSLAttrSetCircRef = 'xsl:attribute-set %s contains a circular reference to itself.';
  sXSLAttrSetSamePrec = 'Two instances of xsl:attribute-set have the same name %s and the same import precedence.';
  sXSLAttrSetBadRef = 'xsl:attribute element with name %s references attribute set %s which does not exist in the stylesheet.';
  sXSLCopyNoCurrentElement = 'No current element in result tree when executing xsl:copy on attribute %s.';
  sXSLCopyElNoChildNodes = 'The current element in the result tree already has child nodes when trying to execute xsl:copy on attribute %s.';
  sXSLNoKeyDef = 'No key definitions for key name %s.';
  sXSLNSAliasSamePrecedence = 'Encountered two instances of xsl:namespace-alias for stylesheet prefix %s with the same import precedence.';
  sXSLStripMatch = 'Found more than one xsl:strip-space or xsl:preserve-space element matching element %s.';
  sXSLUnrecognizedOutput = 'Unrecognized output method %s specified for %s.';

  { Formatting object error messages }
  sXSLFOInvalidRegionType = '%d is invalid region type.';
  sXSLFOPropertyMismatch = 'Property %d does not apply to object %d.';
  sXSLFONoLayoutMaster = 'fo:root requires one fo:layout-master-set child node.';
  sXSLFONoPageSequence = 'fo:root requires one or more fo:page-sequence children.';
  sXSLFOMultipleRoots = 'Each stylesheet can only have one fo:root element.';
  sXSLFORootRequired = '%d object must the child of a fo:root element.';
  sXSLFOMultLayoutMasters = 'fo:root may only contain one fo:layout-master-set child.';
  sXSLFOInternalError = 'Internal error: %s.';
  sXSLFOBadRegionPlace = 'fo:region* objects must be children of fo:simple-page-master objects.';
  sXSLFOMissingProperty = 'Formatting object %s requires a %s formatting property.';
  sXSLFOMissingChild = 'Formatting object %s requires a %s formatting object child.';
  sXSLFORootMustBeRoot = 'Formatting objects must be children of fo:root.';
  sXSLFOInvalidChild = '%s invalid child of %s.';
{$ENDIF}

{*** French ***}
{$IFDEF LANG_FR}
{$ENDIF}

{*** GERMAN ***}
{$IFDEF LANG_GER}
{$ENDIF}

{*** Spanish ***}
{$IFDEF LANG_SP}
{$ENDIF}

{*** Swedish ***}
{$IFDEF LANG_SWED}
{$ENDIF}

{*** Norwegian ***}
{$IFDEF LANG_NOR}
{$ENDIF}

{*** Danish ***}
{$IFDEF LANG_DK}
{$ENDIF}