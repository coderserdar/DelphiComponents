{*******************************************************}
{                                                       }
{              CA Drawing Engine Library                }
{                                                       }
{  Copyright (c) 1996,2004 CodeAccelerate Corporation   }
{                                                       }
{*******************************************************}

resourcestring
  SCRes_ParseError = '%s Line %d, Position %d.';
  SCRes_Error = '%s%s';
  SCRes_EmptyDocumentName = 'File name can not be blank.';
  SCRes_DataHasCDataTerminationTag = 'CDATAs can not contain '']]>''.';
  SCRes_DTDSourceError = 'DTD source must be ''SYSTEM'' or ''PUBLIC''.';
  SCRes_ErrorInvalidNameChar = 'Character ''%s'' at position %d, is an invalid character for an element name.';
  SCRes_ErrorUndefined = 'Undefined error.';
  SCRes_ErrorDataHasCommentTermination = 'Comment blocks can not contain ''--''.';
  SCRes_ErrorEmptyDocument = 'Empty document.';
  SCRes_ErrorNonUnicodeDocument = 'Document is not in Unicode form.';
  SCRes_ErrorNoRootDefined = 'XML documents must have a top level root element.';
  SCRes_ErrorInternal = 'Internal error.';
  SCRes_ErrorSameTokens = 'Released and entered tokens are same.';
  SCRes_ErrorAttrValueNotFound = 'An attributed started but it does not have any value.';
  SCRes_ErrorTagOpenedInTag = 'Another tag opened while processing in a tag.';
  SCRes_ErrorMissingWhiteSpace = 'A space is required at the given position.';
  SCRes_ErrorBadTokenSequence = 'An unexpected token started.';
  SCRes_ErrorFilenotfound = 'Given file does not exists.';
  SCRes_ErrorElementMismatch = 'Closed element does not have an opened pair.';
  SCRes_ErrorTagCloseExpected = 'Entered tag must be closed at the given position.';
  SCRes_ErrorWhiteSpaceNotAllowed = 'Space is not required at the given position.';
  SCRes_ErrorWrongElementStart = 'A name started with an invalid character.';
  SCRes_ErrorOneRootAllowed = 'Document can not have more than one root.';
  SCRes_ErrorTagsNotClosed = 'Document ended without closing the given tags: %s.';
  SCRes_ErrorTagsNotClosed_2 = 'Document ended without closing some tags.';
  SCRes_ErrorInvalidDeclaration = 'Declaration is not valid.';
  SCRes_ErrorTextAtDocumentTop = 'Text element at document top is not allowed.';
  SCRes_ErrorElementNameHasSpace = 'Element names can not contain space characters.';
  SCRes_ErrorElementNameContains = 'Element names can not contain any of ''%s'' characters.';
  SCRes_ErrorElementNameStarts = 'Element names can not start with any of ''%s'' characters.';
  SCRes_ErrorBlankElementName = 'Blank element names can not be used.';
  SCRes_InvalidElementName = 'Element names can not start with any of ''%s'' characters' +
    ' ' + #13 + 'and can not contain any of ''%s''.';
  SCRes_InvalidAttributeValue = 'Attribute values can not contain any of ''%s''.';
  SCRes_InvalidCharForToken = 'Given character is not a valid for %s.';
  SCRes_ErrorBlankAttrName = 'Blank attribute name is not allowed.';
  SCRes_ErrorDuplicateElement = 'Duplicate element.';
  SCRes_ErrorDuplicateEntity = 'Duplicate entity.';
  SCRes_ErrorDuplicateAttr = 'Duplicate attribute.';
  SCRes_ErrorDuplicateNotation = 'Duplicate notation.';
  SCRes_NeedEscapeClose = 'A semi colon character was expected.';
  SCRes_UndefinedEscape = 'Reference to undefined entity.';
  SCRes_InvalidCharacter = 'The character ''%s'' cannot be used in ''%s''.';
  SCRes_InvalidUnicodeCharacter = 'Invalid unicode character.';
  SCRes_ReservedNameMustBeLowerCase = 'The name ''%s'' is reserved and must be lower case.';
  SCRes_CannotSwitchEncoding = 'Switch from current encoding to specified encoding not supported.';
  SCRes_CannotSwitchVersion = 'Switch from current version to specified version not supported.';
  SCRes_UnsupportedEncoding = 'System does not support the specified encoding.';
  SCRes_InvalidXmlDecSyntax = 'Invalid syntax for an xml declaration.';
  SCRes_UnexpectedAttribute = 'The specified attribute was not expected at this location.';
  SCRes_InvalidVersionNumber = 'Invalid version number.';
  SCRes_InvalidStandaloneValue = 'The standalone attribute must have the value ''yes'' or ''no''.';
  SCRes_InvalidXmlInstReplacement = 'Invalid replacement. Xml instructions must be in ''version, encoding, standalone'' order.';
  SCRes_InvalidExpectedCharacter = 'The character ''%s'' was expected.';
  SCRes_CDATANotClosed = 'A CDATA section was not closed.';
  SCRes_CommentNotClosed = 'A comment was not closed.';
  SCRes_MissingWhiteSpace = 'Required white space was missing.';
  SCRes_ExpectedCharacterNotFound = 'An expected character can not be found.';
  SCRes_InvalidDTDName = 'Invalid DTD name.';
  SCRes_InvalidDTDLocation = 'Invalid DTD location.';
  SCRes_InvalidDTDPublicValue = 'Invalid DTD Public value.';
  SCRes_InvalidDTDSystemValue = 'Invalid DTD System value.';
  SCRes_InvalidDTDParameter = 'Invalid DTD parameter.';
  SCRes_InvalidDTDElementName = 'Invalid element name in DTD.';
  SCRes_InvalidDTDAttributeValue = 'Invalid DTD attribute value.';
  SCRes_InvalidDTDAttributeDefault = 'Invalid DTD atrribute default.';
  SCRes_InvalidDTDEntityValue = 'Invalid DTD entity value.';
  SCRes_InvalidDTDCharacter = 'Invalid character found in DTD.';
  SCRes_InvalidDTDDefination = 'A string literal was expected, but no opening quote character was found.';
  SCRes_InvalidDTDXMLElement = 'An XML element is not allowed inside a DTD.';
  SCRes_InvalidDTDDeclarationName = 'Declaration has an invalid name.';
  SCRes_InvalidDTDContentModel = 'Content model is invalid.';
  SCRes_InvalidExternalID = 'External ID is invalid.';
  SCRes_InvalidAttributeType = 'Invalid type defined in ATTLIST.';
  SCRes_InvalidAttributeDefault = 'Invalid ATTDEF declaration.';
  SCRes_ElementNameExpected = 'A name was expected.';
  SCRes_MultipleDOCTYPEDeclaration = 'Cannot have multiple DOCTYPE declarations.';
  SCRes_OutSideDOCTYPEDeclaration = 'Cannot have a DOCTYPE declaration outside of a prolog.';
  SCRes_DuplicateDTDElement = 'Element ''%s'' already defined.';
  SCRes_DuplicateDTDElementWithoutName = 'Element already defined.';
  SCRes_UndefinedDTDElement = 'Element ''%s''is not defined in schema.';

  SCRes_Element = 'Element';
  SCRes_Attribute = 'Attribute';
  SCRes_AttributeValue = 'Attribute value';
  SCRes_Comment = 'Comment';
  SCRes_CDATA = 'CDATA';
  SCRes_CharacterData = 'Character data';
  SCRes_PI = 'Processing instruction';
  SCRes_PIData = 'Processing instruction data';
  SCRes_DTD = 'DTD';
  SCRes_Document = 'Document';
  SCRes_DeclarationLoop = 'Declaration loop';
  SCRes_Declaration = 'Declaration';

