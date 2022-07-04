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
{*********************************************************}
{* XMLPartner: XpXSLCon.PAS                              *}
{*********************************************************}
{* XMLPartner: Constants specific to XSL                 *}
{*********************************************************}
{$I XpDefine.inc}

unit XpXSLCon;

interface

{$I XpXSLMsg.inc}

const
  UNKNOWN = -1;
  TAG_APPLY_IMPORTS = 1;
  TAG_APPLY_TEMPLATES = 2;
  TAG_ATTRIBUTE = 3;
  TAG_ATTRIBUTE_SET = 4;
  TAG_CALL_TEMPLATE = 5;
  TAG_CHOOSE = 6;
  TAG_COMMENT = 7;
  TAG_COPY = 8;
  TAG_COPY_OF = 9;
  TAG_DECIMAL_FORMAT = 10;
  TAG_ELEMENT = 11;
  TAG_FALLBACK = 12;
  TAG_FOR_EACH = 13;
  TAG_IF = 14;
  TAG_IMPORT = 15;
  TAG_INCLUDE = 16;
  TAG_KEY = 17;
  TAG_MESSAGE = 18;
  TAG_NAMESPACE_ALIAS = 19;
  TAG_NUMBER = 20;
  TAG_OTHERWISE = 21;
  TAG_OUTPUT = 22;
  TAG_PARAM = 23;
  TAG_PRESERVE_SPACE = 24;
  TAG_PROCESSING_INSTRUCTION = 25;
  TAG_SORT = 26;
  TAG_STRIP_SPACE = 27;
  TAG_STYLESHEET = 28;
  TAG_TEMPLATE = 29;
  TAG_TEXT = 30;
  TAG_TRANSFORM = 31;
  TAG_VALUE_OF = 32;
  TAG_VARIABLE = 33;
  TAG_WHEN = 34;
  TAG_WITH_PARAM = 35;

  FO_BASIC_LINK = 100;
  FO_BLOCK = 101;
  FO_CONDITIONAL_PAGE_MASTER_REFERENCE = 102;
  FO_EXTERNAL_GRAPHIC = 103;
  FO_FLOW = 104;
  FO_INLINE = 105;
  FO_LAYOUT_MASTER_SET = 106;
  FO_LEADER = 107;
  FO_LIST_BLOCK = 108;
  FO_LIST_ITEM = 109;
  FO_LIST_ITEM_BODY = 110;
  FO_LIST_ITEM_LABEL = 111;
  FO_PAGE_NUMBER = 112;
  FO_PAGE_SEQUENCE = 113;
  FO_PAGE_SEQUENCE_MASTER = 114;
  FO_REGION_AFTER = 115;
  FO_REGION_BEFORE = 116;
  FO_REGION_BODY = 117;
  FO_REGION_END = 118;
  FO_REGION_START = 119;
  FO_REPEATABLE_PAGE_MASTER_ALTERNATIVES = 120;
  FO_REPEATABLE_PAGE_MASTER_REFERENCE = 121;
  FO_ROOT = 122;
  FO_SIMPLE_PAGE_MASTER = 123;
  FO_SINGLE_PAGE_MASTER_REFERENCE = 124;
  FO_STATIC_CONTENT = 125;
  FO_TABLE = 126;
  FO_TABLE_BODY = 127;
  FO_TABLE_CELL = 128;
  FO_TABLE_COLUMN = 129;
  FO_TABLE_ROW = 130;
  FO_TITLE = 131;
  {!!! renumber and alphabetize}
  FO_RETRIEVE_MARKER = 132;
  FO_INLINE_CONTAINER = 133;
  FO_INSTREAM_FOREIGN_OBJECT = 134;
  FO_MARKER = 135;
  FO_MULTI_CASE = 136;
  FO_MULTI_PROPERTIES = 137;
  FO_MULTI_PROPERTY_SET = 138;
  FO_MULTI_SWITCH = 139;
  FO_MULTI_TOGGLE = 140;
  FO_BIDI_OVERRIDE = 141;
  FO_BLOCK_CONTAINER = 142;
  FO_CHARACTER = 143;
  FO_COLOR_PROFILE = 144;
  FO_DECLARATIONS = 145;
  FO_FLOAT = 146;
  FO_FOOTNOTE = 147;
  FO_FOOTNOTE_BODY = 148;
  FO_INITIAL_PROPERTY_SET = 149;
  FO_PAGE_NUMBER_CITATION = 150;
  FO_TABLE_AND_CAPTION = 151;
  FO_TABLE_CAPTION = 152;
  FO_FOOTER = 153;
  FO_HEADER = 154;
  FO_WRAPPER = 155;

  FOP_ALIGNMENT_ADJUST = 300;
  FOP_BACKGROUND_ATTACHMENT = 301;
  FOP_BACKGROUND_COLOR = 302;
  FOP_BACKGROUND_IMAGE = 303;
  FOP_BACKGROUND_POSITION_H = 304;
  FOP_BACKGROUND_POSITION_V = 305;
  FOP_BLANK_OR_NOT_BLANK = 306;
  FOP_BORDER_AFTER_COLOR = 307;
  FOP_BORDER_AFTER_WIDTH = 309;
  FOP_BORDER_BEFORE_COLOR = 310;
  FOP_BORDER_BEFORE_WIDTH = 311;
  FOP_BORDER_BOTTOM_COLOR = 312;
  FOP_BORDER_BOTTOM_WIDTH = 313;
  FOP_BORDER_END_COLOR = 314;
  FOP_BORDER_END_WIDTH = 315;
  FOP_BORDER_LEFT_COLOR = 316;
  FOP_BORDER_LEFT_WIDTH = 317;
  FOP_BORDER_RIGHT_COLOR = 318;
  FOP_BORDER_RIGHT_WIDTH = 319;
  FOP_BORDER_START_COLOR = 320;
  FOP_BORDER_START_WIDTH = 321;
  FOP_BORDER_TOP_COLOR = 322;
  FOP_BORDER_TOP_WIDTH = 323;
  FOP_BREAK_AFTER = 324;
  FOP_BREAK_BEFORE = 325;
  FOP_COLOR = 326;
  FOP_COLUMN_NUMBER = 327;
  FOP_COLUMN_WIDTH = 328;
  FOP_CONTENT_HEIGHT = 329;
  FOP_CONTENT_WIDTH = 330;
  FOP_DISPLAY_ALIGN = 331;
  FOP_EMPTY_CELLS = 332;
  FOP_END_INDENT = 333;
  FOP_ENDS_ROW = 334;
  FOP_EXTERNAL_DESTINATION = 335;
  FOP_FLOW_NAME = 336;
  FOP_FONT_FAMILY = 337;
  FOP_FONT_SIZE = 338;
  FOP_FONT_SIZE_ADJUST = 339;
  FOP_FONT_STRETCH = 340;
  FOP_FONT_STYLE = 341;
  FOP_FONT_VARIANT = 342;
  FOP_FONT_WEIGHT = 343;
  FOP_HEIGHT = 344;
  FOP_HYPHENATE = 345;
  FOP_HYPHENATE_CHAR = 346;
  FOP_HYPHENATE_KEEP = 347;
  FOP_HYPHENATE_PUSH_CHARACTER_COUNT = 348;
  FOP_HYPHENATE_REMAIN_CHARACTER_COUNT = 349;
  FOP_ID = 350;
  FOP_KEEP_TOGETHER = 351;
  FOP_KEEP_WITH_NEXT = 352;
  FOP_KEEP_WITH_PREVIOUS = 353;
  FOP_LANGUAGE = 354;
  FOP_LEADER_LENGTH = 355;
  FOP_LEADER_PATTERN = 356;
  FOP_LETTER_SPACING = 357;
  FOP_LINE_HEIGHT = 358;
  FOP_LINE_HEIGHT_SHIFT_ADJUSTMENT = 359;
  FOP_MARGIN_BOTTOM = 360;
  FOP_MARGIN_LEFT = 361;
  FOP_MARGIN_RIGHT = 362;
  FOP_MARGIN_TOP = 363;
  FOP_MASTER_NAME = 364;
  FOP_NUMBER_COLUMNS_REPEATED = 365;
  FOP_ODD_OR_EVEN = 366;
  FOP_ORPHANS = 367;
  FOP_OVERFLOW = 368;
  FOP_PADDING_AFTER = 369;
  FOP_PADDING_BEFORE = 370;
  FOP_PADDING_BOTTOM = 371;
  FOP_PADDING_END = 372;
  FOP_PADDING_LEFT = 373;
  FOP_PADDING_RIGHT = 374;
  FOP_PADDING_START = 375;
  FOP_PADDING_TOP = 376;
  FOP_PAGE_HEIGHT = 377;
  FOP_PAGE_POSITION = 378;
  FOP_PAGE_WIDTH = 379;
  FOP_PROVISIONAL_DISTANCE_BETWEEN_STARTS = 380;
  FOP_PROVISIONAL_LABEL_SEPARATION = 381;
  FOP_REGION_NAME = 382;
  FOP_RULE_THICKNESS = 383;
  FOP_SCALING = 384;
  FOP_SCALING_METHOD = 385;
  FOP_SHOW_DESTINATION = 386;
  FOP_SPACE_AFTER_MAX = 387;
  FOP_SPACE_BEFORE_MAX = 388;
  FOP_SPACE_END = 389;
  FOP_SPACE_START = 390;
  FOP_SRC = 391;
  FOP_START_INDENT = 392;
  FOP_STARTS_ROW = 393;
  FOP_TEXT_ALIGN = 394;
  FOP_TEXT_ALIGN_LAST = 395;
  FOP_TEXT_INDENT = 396;
  FOP_WIDOWS = 397;
  FOP_WIDTH = 398;
  FOP_WORD_SPACING = 399;
  FOP_WRAP_OPTION = 400;
  { TODO:: Alphabetize and renumber }
  FOP_EXTENT = 401;
  FOP_PRECEDENCE = 402;
  FOP_SPACE_AFTER_MIN = 403;
  FOP_SPACE_AFTER_OPT = 404;
  FOP_SPACE_BEFORE_MIN = 405;
  FOP_SPACE_BEFORE_OPT = 406;
  FOP_MAXIMUM_REPEATS = 407;

  { XSLT property names }
  XpsCaseOrder   = 'case-order';
  XpsCDataSectElements = 'cdata-section-elements';
  XpsCount       = 'count';
  XpsDataType    = 'data-type';
  XpsDecimalSep  = 'decimal-separator';
  XpsDigit       = 'digit';
  XpsDisOutEsc   = 'disable-output-escaping';
  XpsDocTypePub  = 'doctype-public';
  XpsDocTypeSys  = 'doctype-system';
  XpsElements    = 'elements';
  XpsEncoding    = 'encoding';
  XpsExclResultPrefixes = 'exclude-result-prefixes';
  XpsFormat      = 'format';
  XpsFrom        = 'from';
  XpsGroupSep    = 'grouping-separator';
  XpsGroupSize   = 'grouping-size';
  XpsHref        = 'href';
  XpsIndent      = 'indent';
  XpsInfinity    = 'infinity';
  XpsLang        = 'lang';
  XpsLevel       = 'level';
  XpsMatch       = 'match';
  XpsMediaType   = 'media-type';
  XpsMethod      = 'method';
  XpsMinusSign   = 'minus-sign';
  XpsMode        = 'mode';
  XpsName        = 'name';
  XpsNamespace   = 'namespace';
  XpsNaN         = 'NaN';
  XpsNumber      = 'number';
  XpsOmitXMLDecl = 'omit-xml-declaration';
  XpsOrder       = 'order';
  XpsPattSep     = 'pattern-separator';
  XpsPercent     = 'percent';
  XpsPerMille    = 'per-mille';
  XpsPriority    = 'priority';
  XpsResultPrefix= 'result-prefix';
  XpsSelect      = 'select';
  XpsStandalone  = 'standalone';
  XpsStylePrefix = 'stylesheet-prefix';
  XpsTerminate   = 'terminate';
  XpsTest        = 'test';
  XpsText        = 'text';
  XpsUse         = 'use';
  XpsUseAttrSets = 'use-attribute-sets';
  XpsValue       = 'value';
  XpsVersion     = 'version';
  XpsXSLColon    = 'xsl:';
  XpsXSL         = 'xsl';
  XpsZeroDigit   = 'zero-digit';

  { XSLT element names }
  XpsXSLApplyImports = 'xsl:apply-imports';
  XpsXSLApplyTemplates = 'xsl:apply-templates';
  XpsXSLAttribute = 'xsl:attribute';
  XpsXSLAttributeSet = 'xsl:attribute-set';
  XpsXSLCallTemplate = 'xsl:call-template';
  XpsXSLChoose = 'xsl:choose';
  XpsXSLComment = 'xsl:comment';
  XpsXSLCopy = 'xsl:copy';
  XpsXSLCopyOf = 'xsl:copy-of';
  XpsXSLDecimalFormat = 'xsl:decimal-format';
  XpsXSLElement = 'xsl:element';
  XpsXSLFallback = 'xsl:fallback';
  XpsXSLForEach = 'xsl:for-each';
  XpsXSLIf = 'xsl:if';
  XpsXSLImport = 'xsl:import';
  XpsXSLInclude = 'xsl:include';
  XpsXSLKey = 'xsl:key';
  XpsXSLMessage = 'xsl:message';
  XpsXSLNamespaceAlias = 'xsl:namespace-alias';
  XpsXSLNumber = 'xsl:number';
  XpsXSLOtherwise = 'xsl:otherwise';
  XpsXSLOutput = 'xsl:output';
  XpsXSLParam = 'xsl:param';
  XpsXSLPreserveSpace = 'xsl:preserve-space';
  XpsXSLProcessingInstruction = 'xsl:processing-instruction';
  XpsXSLSort = 'xsl:sort';
  XpsXSLStripSpace = 'xsl:strip-space';
  XpsXSLStylesheet = 'xsl:stylesheet';
  XpsXSLTemplate = 'xsl:template';
  { Note: XpsXSLText is declared in unit XPBASE. }
  XpsXSLTransform = 'xsl:transform';
  XpsXSLValueOf = 'xsl:value-of';
  XpsXSLVariable = 'xsl:variable';
  XpsXSLWhen = 'xsl:when';
  XpsXSLWithParam = 'xsl:with-param';

  { Formatting object names }
  XpsFOBasicLink = 'fo:basic-link';
  XpsFOBidiOverride = 'fo:bidi-override';
  XpsFOBlock = 'fo:block';
  XpsFOBlockContainer = 'fo:block-container';
  XpsFOCharacter = 'fo:character';
  XpsFOColorProfile = 'fo:color-profile';
  XpsFOConditionalPageMasterReference = 'fo:conditional-page-master-reference';
  XpsFODeclarations = 'fo:declarations';
  XpsFOExternalGraphic = 'fo:external-graphic';
  XpsFOFloat = 'fo:float';
  XpsFOFlow = 'fo:flow';
  XpsFOFootnote = 'fo:footnote';
  XpsFOFootnoteBody = 'fo:footnoteBody';
  XpsFOInitialPropertySet = 'fo:initial-property-set';
  XpsFOInline = 'fo:inline';
  XpsFOInlineContainer = 'fo:inline-container';
  XpsFOInstreamForeignObject = 'fo:instream-foreign-object';
  XpsFOLayoutMasterSet = 'fo:layout-master-set';
  XpsFOLeader = 'fo:leader';
  XpsFOListBlock = 'fo:list-block';
  XpsFOListItem = 'fo:list-item';
  XpsFOListItemBody = 'fo:list-item-body';
  XpsFOListItemLabel = 'fo:list-item-label';
  XpsFOMarker = 'fo:marker';
  XpsFOMultiCase = 'fo:multi-case';
  XpsFOMultiProperties = 'fo:multi-properties';
  XpsFOMultiPropertySet = 'fo:multi-property-set';
  XpsFOMultiSwitch = 'fo:multi-switch';
  XpsFOMultiToggle = 'fo:multi-toggle';
  XpsFOPageNumber = 'fo:page-number';
  XpsFOPageNumberCitation = 'fo:page-number-citation';
  XpsFOPageSequence = 'fo:page-sequence';
  XpsFOPageSequenceMaster = 'fo:page-sequence-master';
  XpsFORegionAfter = 'fo:region-after';
  XpsFORegionBefore = 'fo:region-before';
  XpsFORegionBody = 'fo:region-body';
  XpsFORegionEnd = 'fo:region-end';
  XpsFORegionStart = 'fo:region-start';
  XpsFORetrieveMarker = 'fo:retrieve-marker';
  XpsFORepeatablePageMasterAlternatives = 'fo:repeatable-page-master-alternatives';
  XpsFORepeatablePageMasterReference = 'fo:repeatable-page-master-reference';
  XpsFORoot = 'fo:root';
  XpsFOSimplePageMaster = 'fo:simple-page-master';
  XpsFOSinglePageMasterReference = 'fo:single-page-master-reference';
  XpsFOStaticContent = 'fo:static-content';
  XpsFOTable = 'fo:table';
  XpsFOTableAndCaption = 'fo:table-and-caption';
  XpsFOTableBody = 'fo:table-body';
  XpsFOTableCaption = 'fo:table-caption';
  XpsFOTableCell = 'fo:table-cell';
  XpsFOTableColumn = 'fo:table-column';
  XpsFOTableFooter = 'fo:table-footer';
  XpsFOTableHeader = 'fo:table-header';
  XpsFOTableRow = 'fo:table-row';
  XpsFOTitle = 'fo:title';
  XpsFOWrapper = 'fo:wrapper';

  { XSL-FO Region Names }
  XpsFORegAfter = 'xsl-region-after';
  XpsFORegBefore = 'xsl-region-before';
  XpsFORegBody = 'xsl-region-body';
  XpsFORegEnd = 'xsl-region-end';
  XpsFORegStart = 'xsl-region-start';
  XpsFORegTitle = 'TITLE';

  { Formatting object property names }
  XpsAlignmentAdjust = 'alignment-adjust';
  XpsBackgroundAttachment = 'background-attachment';
  XpsBackgroundColor = 'background-color';
  XpsBackgroundImage = 'background-image';
  XpsBackgroundPositionHorizontal = 'background-position-horizontal';
  XpsBackgroundPositionVertical = 'background-position-vertical';
  XpsBlankOrNotBlank = 'blank-or-not-blank';
  XpsBorderAfterColor = 'border-after-color';
  XpsBorderAfterWidth = 'border-after-width';
  XpsBorderBeforeColor = 'border-before-color';
  XpsBorderBeforeWidth = 'border-before-width';
  XpsBorderBottomColor = 'border-bottom-color';
  XpsBorderBottomWidth = 'border-bottom-width';
  XpsBorderEndColor = 'border-end-color';
  XpsBorderEndWidth = 'border-end-width';
  XpsBorderLeftColor = 'border-left-color';
  XpsBorderLeftWidth = 'border-left-width';
  XpsBorderRightColor = 'border-right-color';
  XpsBorderRightWidth = 'border-right-width';
  XpsBorderStartColor = 'border-start-color';
  XpsBorderStartWidth = 'border-start-width';
  XpsBorderTopColor = 'border-top-color';
  XpsBorderTopWidth = 'border-top-width';
  XpsBreakAfter = 'break-after';
  XpsBreakBefore = 'break-before';
  XpsColor = 'color';
  XpsColumnNumber = 'column-number';
  XpsColumnWidth = 'column-width';
  XpsContentHeight = 'content-height';
  XpsContentWidth = 'content-width';
  XpsDisplayAlign = 'display-align';
  XpsEmptyCells = 'empty-cells';
  XpsEndIndent = 'end-indent';
  XpsEndsRow = 'ends-row';
  XpsExternalDestination = 'external-destination';
  XpsExtent = 'extent';
  XpsFlowName = 'flow-name';
  XpsFontFamily = 'font-family';
  XpsFontSize = 'font-size';
  XpsFontSizeAdjust = 'font-size-adjust';
  XpsFontStretch = 'font-stretch';
  XpsFontStyle = 'font-style';
  XpsFontVariant = 'font-variant';
  XpsFontWeight = 'font-weight';
  XpsHeight = 'height';
  XpsHyphenate = 'hyphenate';
  XpsHyphenationCharacter = 'hyphenation-character';
  XpsHyphenationKeep = 'hyphenation-keep';
  XpsHyphenationPushCharacterCount = 'hyphenation-push-character-count';
  XpsHyphenationRemainCharacterCount = 'hyphenation-remain-character-count';
  XpsId = 'id';
  XpsInternalDestination = 'internal-destination';
  XpsKeepTogether = 'keep-together';
  XpsKeepWithNext = 'keep-with-next';
  XpsKeepWithPrevious = 'keep-with-previous';
  XpsLanguage = 'language';
  XpsLeaderLength = 'leader-length';
  XpsLeaderPattern = 'leader-pattern';
  XpsLetterSpacing = 'letter-spacing';
  XpsLineHeight = 'line-height';
  XpsLineHeightShiftAdjustment = 'line-height-shift-adjustment';
  XpsMarginBottom = 'margin-bottom';
  XpsMarginLeft = 'margin-left';
  XpsMarginRight = 'margin-right';
  XpsMarginTop = 'margin-top';
  XpsMasterName = 'master-name';
  XpsMasterReference = 'master-reference';                             {!!.57}
  XpsMaximumRepeats = 'maximum-repeats';
  XpsNumberColumnsRepeated = 'number-columns-repeated';
  XpsOddOrEven = 'odd-or-even';
  XpsOrphans = 'orphans';
  XpsOverflow = 'overflow';
  XpsPaddingAfter = 'padding-after';
  XpsPaddingBefore = 'padding-before';
  XpsPaddingBottom = 'padding-bottom';
  XpsPaddingEnd = 'padding-end';
  XpsPaddingLeft = 'padding-left';
  XpsPaddingRight = 'padding-right';
  XpsPaddingStart = 'padding-start';
  XpsPaddingTop = 'padding-top';
  XpsPageHeight = 'page-height';
  XpsPagePosition = 'page-position';
  XpsPageWidth = 'page-width';
  XpsPrecedence = 'precedence';
  XpsProvisionalDistanceBetweenStarts = 'provisional-distance-between-starts';
  XpsProvisionalLabelSeparation = 'provisional-label-separation';
  XpsRegionName = 'region-name';
  XpsRuleThickness = 'rule-thickness';
  XpsScaling = 'scaling';
  XpsScalingMethod = 'scaling-method';
  XpsShowDestination = 'show-destination';
  XpsSpaceAfter = 'space-after';
  XpsSpaceAfterMaximum = 'space-after.maximum';
  XpsSpaceAfterMinimum = 'space-after.mimimum';
  XpsSpaceAfterOptimum = 'space-after.optimum';
  XpsSpaceBefore = 'space-before';
  XpsSpaceBeforeMaximum = 'space-before.maximum';
  XpsSpaceBeforeMinimum = 'space-before.mimimum';
  XpsSpaceBeforeOptimum = 'space-before.optimum';
  XpsSpaceEnd = 'space-end';
  XpsSpaceStart = 'space-start';
  XpsSrc = 'src';
  XpsStartIndent = 'start-indent';
  XpsStartsRow = 'starts-row';
  XpsTextAlign = 'text-align';
  XpsTextAlignLast = 'text-align-last';
  XpsTextIndent = 'text-indent';
  XpsWhiteSpace = 'white-space';
  XpsWidows = 'widows';
  XpsWidth = 'width';
  XpsWordSpacing = 'word-spacing';
  XpsWrapOption = 'wrap-option';

  { Misc strings }
  XpsAny                 = 'any';
  XpsAscending           = 'ascending';
  XpsBaseXSLTSpec        = '1.0';
  XpsCR                  = #13#10;
  XpsDefault             = '#default';
  XpsDefaultNumberFormat = '1';
  XpsDescending          = 'descending';
  XpsFilter              = 'filter';
  XpsFO                  = 'fo:';
  XpsLowerFirst          = 'lower-first';
  XpsMultiple            = 'multiple';
  XpsNo                  = 'no';
  XpsPrint               = 'print';
  XpsRTF                 = 'rtf';
  XpsSingle              = 'single';
  XpsTurboPowerURI       = 'http://www.turbopower.com/xmlpartner';
  XpsUpperFirst          = 'upper-first';
  XpsXSLNS               = 'xmlns:xsl';
  XpsXSLTURI             = 'http://www.w3.org/1999/XSL/Transform';
  XpsYes                 = 'yes';

  { Misc integers }
  XpiDefaultImportPrecedence = 0;

  { Built-in templates }
  XpsBuiltInElement : string =
                               '<xsl:stylesheet version="1.0">' +
                               '<xsl:template match="*|/">' +
                               '  <xsl:apply-templates/>' +
                               '</xsl:template>' +
                               '</xsl:stylesheet>';

  XpsBuiltInTextAttrib : string =
                               '<xsl:stylesheet version="1.0">' +
                               '<xsl:template match="text()|@*">' +
                               '  <xsl:value-of select="."/>' +
                               '</xsl:template>' +
                               '</xsl:stylesheet>';

  XpsBuiltInPIComment : string =
                               '<xsl:stylesheet version="1.0">' +
                               '<xsl:template match=' +
                               '"processing-instruction()|comment()"/>' +
                               '</xsl:stylesheet>';

  XpsBuiltInMode : string =
                               '<xsl:stylesheet version="1.0">' +
                               '<xsl:template match="*|/" mode="%s">' +
                               '  <xsl:apply-templates mode = "%s"/>' +
                               '</xsl:template>' +
                               '</xsl:stylesheet>';

  { Element names. }
  XpsBODY = 'body';
  XpsBR = 'br';
  XpsDIV = 'div';
  XpsDOCFORMAT = 'DOCFORMAT';
  XpsFOOTER = 'FOOTER';
  XpsFOOTNOTE = 'FOOTNOTE';
  XpsHEADER = 'HEADER';
  XpsHR = 'hr';
  XpsHTML = 'html';
  XpsIMG = 'img';
  XpsINFO = 'INFO';
  XpsP = 'p';
  XpsPAGENUM = 'PAGENUM';
  XpsSPAN = 'span';
  XpsSTYLE = 'style';
  XpsTABLE = 'table';
  XpsTITLE = 'title';
  XpsTH = 'th';
  XpsTD = 'td';
  XpsTR = 'tr';
  XpsTBODY = 'tbody';                                                  {!!.57}

implementation

end.
