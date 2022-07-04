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
  sIENotInstalled    = 'Cannot open WININET, Microsoft IE required';
  sOpenFileFailed    = 'Unable to open file ';
  sFileNotFound      = 'File %s could not be found';
  sAllocSrcMemFailed = 'Unable to allocate memory for XML source';
  sHttpReadReqFailed = 'Http read request failed';
  sHttpDataNotAvail  = 'Http data not available';
  sHttpReqSendFailed = 'Unable to send http request';
  sHttpReqOpenFailed = 'Unable to open http request';
  sInetConnectFailed = 'Unable to make Internet connection';
  sInetOpenFailed    = 'Unable to open Internet';
  sInvalidFtpLoc     = 'Invalid ftp location';
  sInvalidFtpDir     = 'Invalid ftp directory';
  sFtpReadReqFailed  = 'Ftp read request failed';
  sFtpDataNotAvail   = 'Ftp data not available';
  sFtpOpenFileFailed = 'Unable to open ftp file';
  sFtpPutFileFailed  = 'Could not save file via ftp to %s';            {!!.51}
  sSrcLoadFailed     = 'Unable to load source ';
  sInvalidMemPtr     = 'Invalid memory Pointer';
  sFmtErrorMsg       = 'File: %s. Line: %d Col: %d Error: %s';         {!!.53}
  sIndexOutOfBounds  = 'ERROR INDEX OUT OF BOUNDS';
  sExpMarkupDecl     = 'Expected markup declaration, but found: ';
  sIllAttrType       = 'Illegal attribute type: ';
  sIllAttrDefKeyw    = 'Illegal keyword for attribute default value: ';
  sSysIdMissing      = 'System identifier missing ';
  sExtModifMissing   = 'External modifier missing: ';
  sIllCondSectStart  = 'Conditional section must begin with INCLUDE or IGNORE';
  sBadSepInModel     = 'Bad separator in content model: ';
  sExpCommentOrCDATA = 'Expected comment or CDATA section ';
  sUnexpectedEof     = 'Unexpected end of file ';
  sMismatchEndTag    = 'Mismatched end tag: ';
  sIllCharInRef      = 'Illegal character in reference: %s (%d)';      {!!.53}
  sUndeclaredEntity  = 'Reference to undeclared entity: ';
  sExpectedString    = 'Expected String: ';
  sSpaceExpectedAt   = 'Whitespace expected at ';                      {!!.53}
  sUnexpEndOfInput   = 'End of input while looking for delimiter: ';
  sQuoteExpected     = 'Expected " or ' + Chr (39);
  sInvalidXMLVersion = 'XMLPartner does not support XML specification greater than %s'; {!!.51}
  sUnableCreateStr   = 'Unable to create stream for input.';
  sInvalidName       = 'Invalid XML name: ';
  sInvalidCommentText= 'Invalid comment text';
  sCommentBeforeXMLDecl = 'Document cannot start with a comment if it also contains an XML declaration';
  sInvalidCDataSection = 'Invalid characters in CDATA section';
  sRedefinedAttr     = 'Attributes cannot be redefined in a start tag';
  sCircularEntRef    = 'Circular reference to: ';
  sInvAttrChar       = 'Invalid character in attribute value: ';
  sInvPCData         = 'Invalid characters in element''s character data: ';
  sDataAfterValDoc   = 'There is invalid data after valid XML document';
  sNoIntConditional  = 'Conditional sections not allowed in internal subset of document type declaration';
  sNotationNotDeclared = 'Notation not declared: ';
  sInvPubIDChar      = 'Invalid PublicID character: ';
  sNoNDATAInPeDecl   = 'NDATA not allowed in parameter entity declaration';
  sInvStandAloneVal  = 'Standalone value must equal ''yes'' or ''no''';
  sInvEncName        = 'Invalid encoding declaration: ';
  sInvVerNum         = 'Invalid XML version number: ';
  sInvEntityValue    = 'Invalid character in entity value: ';
  sNoCommentInMarkup = 'Comments can not be placed within other markup';
  sNoPEInIntDTD      = 'Parameter entities not allowed in DTD internal subset';
  sXMLDecNotAtBeg   =  'The XML declaration must appear before the first element';
  sInvalidElementName = 'Invalid element name: ';
  sBadParamEntNesting = 'Parameter-entity text must be properly nested: '; {!!.51}
  sInvalidCharEncoding = 'Invalid character encoding specified.';
  sAttrNotNum        = 'Attribute %s of element %s does not have an integer value.'; {!!.52}
  sUnknownAxis       = 'Unknown axis specifier: %s';                   {!!.52}
  sMalformedIgnore   = 'Encountered malformed IGNORE section.';        {!!.52}
  sExpectedElement   = 'Expected start of new element.';               {!!.52}
  sAbsoluteInetPathRequired = 'Relative path %s encountered before absolute internet path established.'; {!!.52}
  sMixedRelPathInet  = 'Cannot mix relative internet path %s with local path %s'; {!!.52}
  sMixedInetLocalPath = 'Cannot mix absolute local path %s with internet path %s'; {!!.52}
  sStringNaN = 'String %s cannot be converted into a valid number.';   {!!.52}
  sXPathNoHandler    = 'No handler specified for %s() function.';      {!!.53}
  sXPathExpectedNodeSet = 'Node set expected %s';                      {!!.53}
  sXPathForDocumentParm2 = 'for 2nd parameter of document() function'; {!!.53}
  sXPathNotUnparsedEnt = 'Entity %s is not an unparsed entity.';       {!!.53}
  sXPathUnparsedEntCount = 'Multiple entity declarations found for name %s'; {!!.53}
  sXPathNoDTD = 'No DTD in document for unparsed-entity-uri(%s)';      {!!.53}

  {xpchrstm related errors}
  sInvalidXMLChar    = 'Invalid XML Character found';
  sInvalidBEChar     = 'Invalid (big-endian) UTF-16 character encoding';
  sInvalidLEChar     = 'Invalid (little-endian) UTF-16 character encoding';
  sBadUTF8Char       = 'Badly formed UTF-8 character in stream';
  sErrEndOfDocument  = 'Unexpected end of document stream';
  sUCS_ISOConvertErr = 'Cannot convert UCS-4 character to ISO-8859-1';
  sUCS_U16ConvertErr = 'Cannot convert UCS-4 character to UTF-16';
  sUCS_U8ConverErr   = 'Cannot convert UCS-4 character to UTF-8';

{$ENDIF}

{*** French ***}
{$IFDEF LANG_FR}
  sIENotInstalled    = 'Cannot open WININET, Microsoft IE required';
  sOpenFileFailed    = 'Impossible d''ouvrir le fichier ';
  sFileNotFound      = 'File %s could not be found';
  sAllocSrcMemFailed = 'Impossible d''allouer la mémoire pour la source XML';
  sHttpReadReqFailed = 'Impossible de lire la requête HTTP';
  sHttpDataNotAvail  = 'Données Http non disponibles';
  sHttpReqSendFailed = 'Impossible d''envoyer la requête HTTP';
  sHttpReqOpenFailed = 'Impossible d''ouvrir la requête HTTP';
  sInetConnectFailed = 'Impossible d''établir la connexion Internet';
  sInetOpenFailed    = 'Impossible d''accéder à Internet';
  sInvalidFtpLoc     = 'Localisation FTP incorrecte';
  sInvalidFtpDir     = 'Répertoire FTP incorrect';
  sFtpReadReqFailed  = 'Impossible de lire la requête FTP';
  sFtpDataNotAvail   = 'Données FTP non disponibles';
  sFtpOpenFileFailed = 'Impossible d''ouvrir le fichier FTP';
  sFtpPutFileFailed  = 'Could not save file via ftp to %s';            {!!.51}
  sSrcLoadFailed     = 'Impossible de charger la source ';
  sInvalidMemPtr     = 'Pointeur incorrect';
  sFmtErrorMsg       = 'Ligne: %d Colonne: %d Erreur: %s';
  sIndexOutOfBounds  = 'ERREUR - INDEX HORS BORNES';
  sExpMarkupDecl     = 'Markup de déclaration attendu';
  sIllAttrType       = 'Type d''attribut invalide';
  sIllAttrDefKeyw    = 'Valeur par défaut incorrecte';
  sSysIdMissing      = 'Identificateur système manquant ';
  sExtModifMissing   = 'Modificateur externe manquant ';
  sIllCondSectStart  =
    'Une section conditionnelle doit débuter par INCLUDE ou IGNORE';
  sBadSepInModel     = 'Mauvais séparateur dans un modèle de contenu';
  sExpCommentOrCDATA = 'Commentaire ou section CDATA attendu ';
  sUnexpectedEof     = 'Fin de fichier inattendue';
  sMismatchEndTag    = 'Tag de fin sans correspondance ';
  sIllCharInRef      = 'Caractère illegal dans la référence';
  sUndeclaredEntity  = 'Référence à une entité non déclarée ';
  sExpectedString    = 'Chaîne attendue ';
  sSpaceExpectedAt   = 'Espace attendu en position ';
  sUnexpEndOfInput   = 'Fin de données pendant la recherche du délimiteur ';
  sQuoteExpected     = '" ou '' attendu';
  sInvalidXMLVersion = 'XMLPartner does not support XML specification greater than %s'; {!!.51}
  sUnableCreateStr   = 'Unable to create stream for input.';
  sInvalidName       = 'Invalid XML name';
  sInvalidCommentText= 'Invalid comment text';
  sCommentBeforeXMLDecl = 'Document cannot start with a comment if it also contains an XML declaration';
  sInvalidCDataSection = 'Invalid characters in CDATA section';
  sRedefinedAttr     = 'Attributes cannot be redefined in a start tag';
  sCircularEntRef    = 'Circular reference to: ';
  sInvAttrChar       = 'Invalid character in attribute value: ';
  sInvPCData         = 'Invalid characters in element''s character data: ';
  sDataAfterValDoc   = 'There is invalid data after valid XML document';
  sNoIntConditional  = 'Conditional sections not allowed in internal subset of document type declaration';
  sNotationNotDeclared = 'Notation not declared: ';
  sInvPubIDChar      = 'Invalid PublicID character: ';
  sNoNDATAInPeDecl   = 'NDATA not allowed in parameter entity declaration.';
  sInvStandAloneVal  = 'Standalone value must equal ''yes'' or ''no''';
  sInvEncName        = 'Invalid encoding declaration: ';
  sInvVerNum         = 'Invalid XML version number: ';
  sInvEntityValue    = 'Invalid character in entity value: ';
  sNoCommentInMarkup = 'Comments can not be placed within other markup';
  sNoPEInIntDTD      = 'Parameter entities not allowed in DTD internal subset';
  sXMLDecNotAtBeg   =  'The XML declaration must appear before the first element';
  sInvalidElementName = 'Invalid element name: ';
  sBadParamEntNesting = 'Parameter-entity text must be properly nested: '; {!!.51}
  sInvalidCharEncoding = 'Invalid character encoding specified.';
  sAttrNotNum        = 'Attribute %s of element %s does not have an integer value.'; {!!.52}
  sUnknownAxis       = 'Unknown axis specifier: %s';                   {!!.52}
  sMalformedIgnore   = 'Encountered malformed IGNORE section.';        {!!.52}
  sExpectedElement   = 'Expected start of new element.';               {!!.52}
  sAbsoluteInetPathRequired = 'Relative path %s encountered before absolute internet path established.'; {!!.52}
  sMixedRelPathInet  = 'Cannot mix relative internet path %s with local path %s'; {!!.52}
  sMixedInetLocalPath = 'Cannot mix absolute local path %s with internet path %s'; {!!.52}
  sStringNaN = 'String %s cannot be converted into a valid number.';   {!!.52}
  sXPathNoHandler    = 'No handler specified for %s() function.';      {!!.53}
  sXPathExpectedNodeSet = 'Node set expected %s';                      {!!.53}
  sXPathForDocumentParm2 = 'for 2nd parameter of document() function'; {!!.53}
  sXPathNotUnparsedEnt = 'Entity %s is not an unparsed entity.';       {!!.53}
  sXPathUnparsedEntCount = 'Multiple entity declarations found for name %s'; {!!.53}
  sXPathNoDTD = 'No DTD in document for unparsed-entity-uri(%s)';      {!!.53}

  {xpchrstm related errors}
  sInvalidXMLChar    = 'Invalid XML Character found';
  sInvalidBEChar     = 'Invalid (big-endian) UTF-16 character encoding';
  sInvalidLEChar     = 'Invalid (little-endian) UTF-16 character encoding';
  sBadUTF8Char       = 'Badly formed UTF-8 character in stream';
  sErrEndOfDocument  = 'Unexpected end of document stream';
  sUCS_ISOConvertErr = 'Cannot convert UCS-4 character to ISO-8859-1';
  sUCS_U16ConvertErr = 'Cannot convert UCS-4 character to UTF-16';
  sUCS_U8ConverErr   = 'Cannot convert UCS-4 character to UTF-8';
{$ENDIF}

{*** GERMAN ***}
{$IFDEF LANG_GER}
  sIENotInstalled    = 'Cannot open WININET, Microsoft IE required';
  sOpenFileFailed    = 'Datei kann nicht geöffnet werden ';
  sFileNotFound      = 'File %s could not be found';
  sAllocSrcMemFailed = 'Speicher für XML Quelle kann nicht reserviert werden';
  sHttpReadReqFailed = 'Http Leseanforderung fehlgeschlagen';
  sHttpDataNotAvail  = 'Http Daten nicht verfügbar';
  sHttpReqSendFailed = 'Kann http Anforderung nicht senden';
  sHttpReqOpenFailed = 'Kann http Anforderung nicht öffnen';
  sInetConnectFailed = 'Kann keine Internet-Verbindung herstellen';
  sInetOpenFailed    = 'Kann Internet nicht öffnen';
  sInvalidFtpLoc     = 'Ungültiger FTP-Servername';
  sInvalidFtpDir     = 'Ungültiges FTP-Verzeichnis';
  sFtpReadReqFailed  = 'FTP Leseanforderung fehlgeschlagen';
  sFtpDataNotAvail   = 'FTP Daten nicht verfügbar';
  sFtpOpenFileFailed = 'Kann FTP Datei nicht öffnen';
  sFtpPutFileFailed  = 'Could not save file via ftp to %s';            {!!.51}
  sSrcLoadFailed     = 'Kann Quelle nicht laden ';
  sInvalidMemPtr     = 'Ungültiger Speicherzeiger';
  sFmtErrorMsg       = 'Zeile: %d Spalte: %d Fehler: %s';
  sIndexOutOfBounds  = 'FEHLER INDEX AUSSERHALB DER LISTENGRENZE';
  sExpMarkupDecl     = 'Markup erwartet';
  sIllAttrType       = 'Ungültiger Attributtyp';
  sIllAttrDefKeyw    = 'Ungültiges Schlüsselwort für Attribut-Standardwert';
  sSysIdMissing      = 'System-Id fehlt ';
  sExtModifMissing   = 'Externer Modifier fehlt ';
  sIllCondSectStart  = 'Wahlfreier Abschnitt muß mit INCLUDE oder IGNORE beginnen';
  sBadSepInModel     = 'Fehlerhaftes Trennzeichen in Modell ';
  sExpCommentOrCDATA = 'Kommentar oder CDATA Abschnitt erwartet ';
  sUnexpectedEof     = 'Unerwartetes Dateiende';
  sMismatchEndTag    = 'Anzahl Ende-Tags stimmt nicht ';
  sIllCharInRef      = 'Ungültiges Zeichen in Referenz';
  sUndeclaredEntity  = 'Referenz zu nicht deklarierter Entität ';
  sExpectedString    = 'Erwarte Zeichenkette ';
  sSpaceExpectedAt   = 'Trennzeichen erwartet bei ';
  sUnexpEndOfInput   = 'Ende der Eingabe beim Suchen nach Begrenzungszeichen';
  sQuoteExpected     = 'Erwartet " oder ' + Chr (39);
  sInvalidXMLVersion = 'XMLPartner does not support XML specification greater than %s'; {!!.51}
  sUnableCreateStr   = 'Unable to create stream for input.';
  sInvalidName       = 'Invalid XML name';
  sInvalidCommentText= 'Invalid comment text';
  sCommentBeforeXMLDecl = 'Document cannot start with a comment if it also contains an XML declaration';
  sInvalidCDataSection = 'Invalid characters in CDATA section';
  sRedefinedAttr     = 'Attributes cannot be redefined in a start tag';
  sCircularEntRef    = 'Circular reference to: ';
  sInvAttrChar       = 'Invalid character in attribute value: ';
  sInvPCData         = 'Invalid characters in element''s character data: ';
  sDataAfterValDoc   = 'There is invalid data after valid XML document';
  sNoIntConditional  = 'Conditional sections not allowed in internal subset of document type declaration';
  sNotationNotDeclared = 'Notation not declared: ';
  sInvPubIDChar      = 'Invalid PublicID character: ';
  sNoNDATAInPeDecl   = 'NDATA not allowed in parameter entity declaration.';
  sInvStandAloneVal  = 'Standalone value must equal ''yes'' or ''no''';
  sInvEncName        = 'Invalid encoding declaration: ';
  sInvVerNum         = 'Invalid XML version number: ';
  sInvEntityValue    = 'Invalid character in entity value: ';
  sNoCommentInMarkup = 'Comments can not be placed within other markup';
  sNoPEInIntDTD      = 'Parameter entities not allowed in DTD internal subset';
  sXMLDecNotAtBeg   =  'The XML declaration must appear before the first element';
  sInvalidElementName = 'Invalid element name: ';
  sBadParamEntNesting = 'Parameter-entity text must be properly nested: '; {!!.51}
  sInvalidCharEncoding = 'Invalid character encoding specified.';
  sAttrNotNum        = 'Attribute %s of element %s does not have an integer value.'; {!!.52}
  sUnknownAxis       = 'Unknown axis specifier: %s';                   {!!.52}
  sMalformedIgnore   = 'Encountered malformed IGNORE section.';        {!!.52}
  sExpectedElement   = 'Expected start of new element.';               {!!.52}
  sAbsoluteInetPathRequired = 'Relative path %s encountered before absolute internet path established.'; {!!.52}
  sMixedRelPathInet  = 'Cannot mix relative internet path %s with local path %s'; {!!.52}
  sMixedInetLocalPath = 'Cannot mix absolute local path %s with internet path %s'; {!!.52}
  sStringNaN = 'String %s cannot be converted into a valid number.';   {!!.52}
  sXPathNoHandler    = 'No handler specified for %s() function.';      {!!.53}
  sXPathExpectedNodeSet = 'Node set expected %s';                      {!!.53}
  sXPathForDocumentParm2 = 'for 2nd parameter of document() function'; {!!.53}
  sXPathNotUnparsedEnt = 'Entity %s is not an unparsed entity.';       {!!.53}
  sXPathUnparsedEntCount = 'Multiple entity declarations found for name %s'; {!!.53}
  sXPathNoDTD = 'No DTD in document for unparsed-entity-uri(%s)';      {!!.53}

  {xpchrstm related errors}
  sInvalidXMLChar    = 'Invalid XML Character found';
  sInvalidBEChar     = 'Invalid (big-endian) UTF-16 character encoding';
  sInvalidLEChar     = 'Invalid (little-endian) UTF-16 character encoding';
  sBadUTF8Char       = 'Badly formed UTF-8 character in stream';
  sErrEndOfDocument  = 'Unexpected end of document stream';
  sUCS_ISOConvertErr = 'Cannot convert UCS-4 character to ISO-8859-1';
  sUCS_U16ConvertErr = 'Cannot convert UCS-4 character to UTF-16';
  sUCS_U8ConverErr   = 'Cannot convert UCS-4 character to UTF-8';
{$ENDIF}

{*** Spanish ***}
{$IFDEF LANG_SP}
  sIENotInstalled    = 'Cannot open WININET, Microsoft IE required';
  sOpenFileFailed    = 'No se puede abrir el fichero ';
  sFileNotFound      = 'File %s could not be found';
  sAllocSrcMemFailed = 'No se puede reservar memoria para el fuente XML';
  sHttpReadReqFailed = 'Ha fallado la petición de lectura Http';
  sHttpDataNotAvail  = 'Los datos Http no están disponibles';
  sHttpReqSendFailed = 'No se puede enviar la petición Http';
  sHttpReqOpenFailed = 'No se puede abrir la petición Http';
  sInetConnectFailed = 'No se puede establecer la conexión Internet';
  sInetOpenFailed    = 'No se puede abrir Internet';
  sInvalidFtpLoc     = 'Localización ftp no válida';
  sInvalidFtpDir     = 'Directorio ftp no válido';
  sFtpReadReqFailed  = 'Ha fallado la petición de lectura ftp';
  sFtpDataNotAvail   = 'Los datos ftp no están disponibles';
  sFtpOpenFileFailed = 'No se puede abrir el fichero ftp';
  sFtpPutFileFailed  = 'Could not save file via ftp to %s';            {!!.51}
  sSrcLoadFailed     = 'No se puede cargar el fuente ';
  sInvalidMemPtr     = 'Puntero de memoria no válido';
  sFmtErrorMsg       = 'Línea: %d Columna: %d Error: %s';
  sIndexOutOfBounds  = 'ERROR: ÍNDICE FUERA DE RANGO';
  sExpMarkupDecl     = 'Se esperaba una declaración de etiquetado';
  sIllAttrType       = 'Tipo de atributo incorrecto';
  sIllAttrDefKeyw    = 'Valor por defecto del atributo incorrecto';
  sSysIdMissing      = 'Falta identificador de sistema ';
  sExtModifMissing   = 'Falta modificador externo ';
  sIllCondSectStart  = 'La sección condicional debe comenzar con INCLUDE o con IGNORE';
  sBadSepInModel     = 'Separador incorrecto en el modelo de contenido ';
  sExpCommentOrCDATA = 'Se esperaba un comentario o una sección CDATA ';
  sUnexpectedEof     = 'Fin de fichero inesperado';
  sMismatchEndTag    = 'Etiqueta de fin sin correspondencia ';
  sIllCharInRef      = 'Carácter ilegal en la referencia';
  sUndeclaredEntity  = 'Referencia a una entidad no declarada ';
  sExpectedString    = 'Se esperaba la cadena ';
  sSpaceExpectedAt   = 'Se esperaba espacio en blanco en la posición ';
  sUnexpEndOfInput   = 'Fin de los datos mientras se buscaba un delimitador ';
  sQuoteExpected     = 'Se esperaba " o ' + Chr (39);
  sInvalidXMLVersion = 'XMLPartner does not support XML specification greater than %s'; {!!.51}
  sUnableCreateStr   = 'Unable to create stream for input.';
  sInvalidName       = 'Invalid XML name';
  sInvalidCommentText= 'Invalid comment text';
  sCommentBeforeXMLDecl = 'Document cannot start with a comment if it also contains an XML declaration';
  sInvalidCDataSection = 'Invalid characters in CDATA section';
  sRedefinedAttr     = 'Attributes cannot be redefined in a start tag';
  sCircularEntRef    = 'Circular reference to: ';
  sInvAttrChar       = 'Invalid character in attribute value: ';
  sInvPCData         = 'Invalid characters in element''s character data: ';
  sDataAfterValDoc   = 'There is invalid data after valid XML document';
  sNoIntConditional  = 'Conditional sections not allowed in internal subset of document type declaration';
  sNotationNotDeclared = 'Notation not declared: ';
  sInvPubIDChar      = 'Invalid PublicID character: ';
  sNoNDATAInPeDecl   = 'NDATA not allowed in parameter entity declaration.';
  sInvStandAloneVal  = 'Standalone value must equal ''yes'' or ''no''';
  sInvEncName        = 'Invalid encoding declaration: ';
  sInvVerNum         = 'Invalid XML version number: ';
  sInvEntityValue    = 'Invalid character in entity value: ';
  sNoCommentInMarkup = 'Comments can not be placed within other markup';
  sNoPEInIntDTD      = 'Parameter entities not allowed in DTD internal subset';
  sXMLDecNotAtBeg   =  'The XML declaration must appear before the first element';
  sInvalidElementName = 'Invalid element name: ';
  sBadParamEntNesting = 'Parameter-entity text must be properly nested: '; {!!.51}
  sInvalidCharEncoding = 'Invalid character encoding specified.';
  sAttrNotNum        = 'Attribute %s of element %s does not have an integer value.'; {!!.52}
  sUnknownAxis       = 'Unknown axis specifier: %s';                   {!!.52}
  sMalformedIgnore   = 'Encountered malformed IGNORE section.';        {!!.52}
  sExpectedElement   = 'Expected start of new element.';               {!!.52}
  sAbsoluteInetPathRequired = 'Relative path %s encountered before absolute internet path established.'; {!!.52}
  sMixedRelPathInet  = 'Cannot mix relative internet path %s with local path %s'; {!!.52}
  sMixedInetLocalPath = 'Cannot mix absolute local path %s with internet path %s'; {!!.52}
  sStringNaN = 'String %s cannot be converted into a valid number.';   {!!.52}
  sXPathNoHandler    = 'No handler specified for %s() function.';      {!!.53}
  sXPathExpectedNodeSet = 'Node set expected %s';                      {!!.53}
  sXPathForDocumentParm2 = 'for 2nd parameter of document() function'; {!!.53}
  sXPathNotUnparsedEnt = 'Entity %s is not an unparsed entity.';       {!!.53}
  sXPathUnparsedEntCount = 'Multiple entity declarations found for name %s'; {!!.53}
  sXPathNoDTD = 'No DTD in document for unparsed-entity-uri(%s)';      {!!.53}

  {xpchrstm related errors}
  sInvalidXMLChar    = 'Invalid XML Character found';
  sInvalidBEChar     = 'Invalid (big-endian) UTF-16 character encoding';
  sInvalidLEChar     = 'Invalid (little-endian) UTF-16 character encoding';
  sBadUTF8Char       = 'Badly formed UTF-8 character in stream';
  sErrEndOfDocument  = 'Unexpected end of document stream';
  sUCS_ISOConvertErr = 'Cannot convert UCS-4 character to ISO-8859-1';
  sUCS_U16ConvertErr = 'Cannot convert UCS-4 character to UTF-16';
  sUCS_U8ConverErr   = 'Cannot convert UCS-4 character to UTF-8';
{$ENDIF}

{*** Swedish ***}
{$IFDEF LANG_SWED}
  sIENotInstalled    = 'Cannot open WININET, Microsoft IE required';
  sOpenFileFailed    = 'Kunde inte öppna filen ';
  sFileNotFound      = 'File %s could not be found';
  sAllocSrcMemFailed = 'Kunde inte allokera minne för XML-källa';
  sHttpReadReqFailed = 'HTTP-läsning misslyckades';
  sHttpDataNotAvail  = 'HTTP-data ej tillgängligt';
  sHttpReqSendFailed = 'Kunde inte skicka HTTP-begäran';
  sHttpReqOpenFailed = 'Kunde inte öppna HTTP-begäran';
  sInetConnectFailed = 'Kunde inte skapa internet-uppkoppling';
  sInetOpenFailed    = 'Kunde inte öppna Internet';
  sInvalidFtpLoc     = 'Felaktig FTP-adress';
  sInvalidFtpDir     = 'Felaktig FTP-katalog';
  sFtpReadReqFailed  = 'FTP-läsning misslyckades';
  sFtpDataNotAvail   = 'FTP-data ej tillgängligt';
  sFtpOpenFileFailed = 'Kunde inte öppna FTP-fil';
  sFtpPutFileFailed  = 'Could not save file via ftp to %s';            {!!.51}
  sSrcLoadFailed     = 'Kunde inte ladda in ';
  sInvalidMemPtr     = 'Felaktig minnespekare';
  sFmtErrorMsg       = 'Rad: %d Kol: %d Fel: %s';
  sIndexOutOfBounds  = 'FEL: FÖR STORT INDEX';
  sExpMarkupDecl     = 'Förväntade sig markup declaration';
  sIllAttrType       = 'Felaktigt attributtyp';
  sIllAttrDefKeyw    = 'Felaktigt keyword för attributets default-värde';
  sSysIdMissing      = 'System-identiferare saknas ';
  sExtModifMissing   = 'Extern identifierare saknas ';
  sIllCondSectStart  = 'Villkorligt avsnitt måste börja med INCLUDE eller IGNORE';
  sBadSepInModel     = 'Felaktigt skiljetecken ';
  sExpCommentOrCDATA = 'Borde vara kommentar eller CDATA section ';
  sUnexpectedEof     = 'Oväntat slut på filen';
  sMismatchEndTag    = 'Felaktig placerad slut-tag ';
  sIllCharInRef      = 'Ogiltigt tecken i referens';
  sUndeclaredEntity  = 'Referens till odeklarerad entity ';
  sExpectedString    = 'Förväntad sträng ';
  sSpaceExpectedAt   = 'Whitespace förväntat vid ';
  sUnexpEndOfInput   = 'Input tog slut innan det kom en delimiter ';
  sQuoteExpected     = 'Förväntade sig " or ' + Chr (39);
  sInvalidXMLVersion = 'XMLPartner does not support XML specification greater than %s'; {!!.51}
  sUnableCreateStr   = 'Unable to create stream for input.';
  sInvalidName       = 'Invalid XML name';
  sInvalidCommentText= 'Invalid comment text';
  sCommentBeforeXMLDecl = 'Document cannot start with a comment if it also contains an XML declaration';
  sInvalidCDataSection = 'Invalid characters in CDATA section';
  sRedefinedAttr     = 'Attributes cannot be redefined in a start tag';
  sCircularEntRef    = 'Circular reference to: ';
  sInvAttrChar       = 'Invalid character in attribute value: ';
  sInvPCData         = 'Invalid characters in element''s character data: ';
  sDataAfterValDoc   = 'There is invalid data after valid XML document';
  sNoIntConditional  = 'Conditional sections not allowed in internal subset of document type declaration';
  sNotationNotDeclared = 'Notation not declared: ';
  sInvPubIDChar      = 'Invalid PublicID character: ';
  sNoNDATAInPeDecl   = 'NDATA not allowed in parameter entity declaration.';
  sInvStandAloneVal  = 'Standalone value must equal ''yes'' or ''no''';
  sInvEncName        = 'Invalid encoding declaration: ';
  sInvVerNum         = 'Invalid XML version number: ';
  sInvEntityValue    = 'Invalid character in entity value: ';
  sNoCommentInMarkup = 'Comments can not be placed within other markup';
  sNoPEInIntDTD      = 'Parameter entities not allowed in DTD internal subset';
  sXMLDecNotAtBeg   =  'The XML declaration must appear before the first element';
  sInvalidElementName = 'Invalid element name: ';
  sBadParamEntNesting = 'Parameter-entity text must be properly nested: '; {!!.51}
  sInvalidCharEncoding = 'Invalid character encoding specified.';
  sAttrNotNum        = 'Attribute %s of element %s does not have an integer value.'; {!!.52}
  sUnknownAxis       = 'Unknown axis specifier: %s';                   {!!.52}
  sMalformedIgnore   = 'Encountered malformed IGNORE section.';        {!!.52}
  sExpectedElement   = 'Expected start of new element.';               {!!.52}
  sAbsoluteInetPathRequired = 'Relative path %s encountered before absolute internet path established.'; {!!.52}
  sMixedRelPathInet  = 'Cannot mix relative internet path %s with local path %s'; {!!.52}
  sMixedInetLocalPath = 'Cannot mix absolute local path %s with internet path %s'; {!!.52}
  sStringNaN = 'String %s cannot be converted into a valid number.';   {!!.52}
  sXPathNoHandler    = 'No handler specified for %s() function.';      {!!.53}
  sXPathExpectedNodeSet = 'Node set expected %s';                      {!!.53}
  sXPathForDocumentParm2 = 'for 2nd parameter of document() function'; {!!.53}
  sXPathNotUnparsedEnt = 'Entity %s is not an unparsed entity.';       {!!.53}
  sXPathUnparsedEntCount = 'Multiple entity declarations found for name %s'; {!!.53}
  sXPathNoDTD = 'No DTD in document for unparsed-entity-uri(%s)';      {!!.53}

  {xpchrstm related errors}
  sInvalidXMLChar    = 'Invalid XML Character found';
  sInvalidBEChar     = 'Invalid (big-endian) UTF-16 character encoding';
  sInvalidLEChar     = 'Invalid (little-endian) UTF-16 character encoding';
  sBadUTF8Char       = 'Badly formed UTF-8 character in stream';
  sErrEndOfDocument  = 'Unexpected end of document stream';
  sUCS_ISOConvertErr = 'Cannot convert UCS-4 character to ISO-8859-1';
  sUCS_U16ConvertErr = 'Cannot convert UCS-4 character to UTF-16';
  sUCS_U8ConverErr   = 'Cannot convert UCS-4 character to UTF-8';
{$ENDIF}

{*** Norwegian ***}
{$IFDEF LANG_NOR}
  sIENotInstalled    = 'Cannot open WININET, Microsoft IE required';
  sOpenFileFailed    = 'Klarte ikke å åpne fil ';
  sFileNotFound      = 'File %s could not be found';
  sAllocSrcMemFailed = 'Klarte ikke å allokere minne for XML input';
  sHttpReadReqFailed = 'Http lese-forespørsel mislyktes';
  sHttpDataNotAvail  = 'Http data ikke tilgjengelig';
  sHttpReqSendFailed = 'Klarte ikke sende http-forespørsel';
  sHttpReqOpenFailed = 'Klarte ikke åpne http-forespørsel';
  sInetConnectFailed = 'Klarte ikke å opprette internett-forbindelse';
  sInetOpenFailed    = 'Klarte ikke å åpne internett';
  sInvalidFtpLoc     = 'Ugyldig ftp-adresse';
  sInvalidFtpDir     = 'Ugyldig ftp-mappe (directory)';
  sFtpReadReqFailed  = 'Ftp lese-forespørsel mislyktes';
  sFtpDataNotAvail   = 'Ftp data ikke tilgjengelig';
  sFtpOpenFileFailed = 'Klarte ikke å åpne fil via ftp';
  sFtpPutFileFailed  = 'Could not save file via ftp to %s';            {!!.51}
  sSrcLoadFailed     = 'Klarte ikke å laste input ';
  sInvalidMemPtr     = 'Ugyldig minnepeker';
  sFmtErrorMsg       = 'Linje: %d Kolonne: %d Feil: %s';
  sIndexOutOfBounds  = 'FEILINDEKS UTENFOR GYLDIG OMRÅDE (out of bounds)';
  sExpMarkupDecl     = 'Forventet markup-deklarasjon';
  sIllAttrType       = 'Ulovlig attributt-type';
  sIllAttrDefKeyw    = 'Ulovlig nøkkelord for attributt-initialverdi';
  sSysIdMissing      = 'Manglende systemidentifikator ';
  sExtModifMissing   = 'Manglende ekstern modifikator ';
  sIllCondSectStart  = 'Betingelsesseksjon (conditional section) må begynne med enten INCLUDE eller IGNORE';
  sBadSepInModel     = 'Ulovlig separator i innholdsmodell (content model) ';
  sExpCommentOrCDATA = 'Forventet kommentar eller CDATA-seksjon ';
  sUnexpectedEof     = 'Uventet slutt på fil';
  sMismatchEndTag    = 'Uoverenstemmelse mellom start og slutt-tag ';
  sIllCharInRef      = 'Ulolig karakter i referanse';
  sUndeclaredEntity  = 'Referanse til ikke-deklarert entitet ';
  sExpectedString    = 'Forventet streng ';
  sSpaceExpectedAt   = 'Blank (whitespace) forventet ved byte ';
  sUnexpEndOfInput   = 'Uventet slutt på input mens systemet ventet på skilletegn ';
  sQuoteExpected     = 'Forventet " eller ' + Chr (39);
  sInvalidXMLVersion = 'XMLPartner does not support XML specification greater than %s'; {!!.51}
  sUnableCreateStr   = 'Unable to create stream for input.';
  sInvalidName       = 'Invalid XML name';
  sInvalidCommentText= 'Invalid comment text';
  sCommentBeforeXMLDecl = 'Document cannot start with a comment if it also contains an XML declaration';
  sInvalidCDataSection = 'Invalid characters in CDATA section';
  sRedefinedAttr     = 'Attributes cannot be redefined in a start tag';
  sCircularEntRef    = 'Circular reference to: ';
  sInvAttrChar       = 'Invalid character in attribute value: ';
  sInvPCData         = 'Invalid characters in element''s character data: ';
  sDataAfterValDoc   = 'There is invalid data after valid XML document';
  sNoIntConditional  = 'Conditional sections not allowed in internal subset of document type declaration';
  sNotationNotDeclared = 'Notation not declared: ';
  sInvPubIDChar      = 'Invalid PublicID character: ';
  sNoNDATAInPeDecl   = 'NDATA not allowed in parameter entity declaration.';
  sInvStandAloneVal  = 'Standalone value must equal ''yes'' or ''no''';
  sInvEncName        = 'Invalid encoding declaration: ';
  sInvVerNum         = 'Invalid XML version number: ';
  sInvEntityValue    = 'Invalid character in entity value: ';
  sNoCommentInMarkup = 'Comments can not be placed within other markup';
  sNoPEInIntDTD      = 'Parameter entities not allowed in DTD internal subset';
  sXMLDecNotAtBeg   =  'The XML declaration must appear before the first element';
  sInvalidElementName = 'Invalid element name: ';
  sBadParamEntNesting = 'Parameter-entity text must be properly nested: '; {!!.51}
  sInvalidCharEncoding = 'Invalid character encoding specified.';
  sAttrNotNum        = 'Attribute %s of element %s does not have an integer value.'; {!!.52}
  sUnknownAxis       = 'Unknown axis specifier: %s';                   {!!.52}
  sMalformedIgnore   = 'Encountered malformed IGNORE section.';        {!!.52}
  sExpectedElement   = 'Expected start of new element.';               {!!.52}
  sAbsoluteInetPathRequired = 'Relative path %s encountered before absolute internet path established.'; {!!.52}
  sMixedRelPathInet  = 'Cannot mix relative internet path %s with local path %s'; {!!.52}
  sMixedInetLocalPath = 'Cannot mix absolute local path %s with internet path %s'; {!!.52}
  sStringNaN = 'String %s cannot be converted into a valid number.';   {!!.52}
  sXPathNoHandler    = 'No handler specified for %s() function.';      {!!.53}
  sXPathExpectedNodeSet = 'Node set expected %s';                      {!!.53}
  sXPathForDocumentParm2 = 'for 2nd parameter of document() function'; {!!.53}
  sXPathNotUnparsedEnt = 'Entity %s is not an unparsed entity.';       {!!.53}
  sXPathUnparsedEntCount = 'Multiple entity declarations found for name %s'; {!!.53}
  sXPathNoDTD = 'No DTD in document for unparsed-entity-uri(%s)';      {!!.53}

  {xpchrstm related errors}
  sInvalidXMLChar    = 'Invalid XML Character found';
  sInvalidBEChar     = 'Invalid (big-endian) UTF-16 character encoding';
  sInvalidLEChar     = 'Invalid (little-endian) UTF-16 character encoding';
  sBadUTF8Char       = 'Badly formed UTF-8 character in stream';
  sErrEndOfDocument  = 'Unexpected end of document stream';
  sUCS_ISOConvertErr = 'Cannot convert UCS-4 character to ISO-8859-1';
  sUCS_U16ConvertErr = 'Cannot convert UCS-4 character to UTF-16';
  sUCS_U8ConverErr   = 'Cannot convert UCS-4 character to UTF-8';
{$ENDIF}

{*** Danish ***}
{$IFDEF LANG_DK}
  sIENotInstalled    = 'Cannot open WININET, Microsoft IE required';
  sOpenFileFailed    = 'Kan ikke åbne filen ';
  sFileNotFound      = 'File %s could not be found';
  sAllocSrcMemFailed = 'Kan ikke allokere hukommelse til XML source';
  sHttpReadReqFailed = 'Http læse forespørgsel fejlede';
  sHttpDataNotAvail  = 'Http data findes ikke';
  sHttpReqSendFailed = 'Kan ikke udføre http request';
  sHttpReqOpenFailed = 'Kan ikke åbne http request';
  sInetConnectFailed = 'Kan ikke oprette internet forbindelse';
  sInetOpenFailed    = 'Fejl ved kald af open internet';
  sInvalidFtpLoc     = 'Ugyldig ftp lokation';
  sInvalidFtpDir     = 'Ugyldig ftp mappe';
  sFtpReadReqFailed  = 'Ftp read request fejlede';
  sFtpDataNotAvail   = 'Ftp data er ikke tilgængelig';
  sFtpOpenFileFailed = 'Kan ikke åbne ftp filen';
  sFtpPutFileFailed  = 'Could not save file via ftp to %s';            {!!.51}
  sSrcLoadFailed     = 'Kan ikke indlæse source';
  sInvalidMemPtr     = 'Ugyldig memory peger';
  sFmtErrorMsg       = 'Linie: %d Kolonne: %d Fejl: %s';
  sIndexOutOfBounds  = 'FEJL INDEKS ER UDEN FOR GRÆNSEVÆRDIER';
  sExpMarkupDecl     = 'Forventet markup declaration';
  sIllAttrType       = 'Ugyldig attribut type';
  sIllAttrDefKeyw    = 'Ugyldig keyword for attribute default value';
  sSysIdMissing      = 'System identifier missing ';
  sExtModifMissing   = 'External modifier missing ';
  sIllCondSectStart  = 'Betinget sektion skal starte med INCLUDE eller IGNORE';
  sBadSepInModel     = 'Bad separator in content model ';
  sExpCommentOrCDATA = 'Forventede kommentar eller CDATA sektion';
  sUnexpectedEof     = 'Uventet end of file';
  sMismatchEndTag    = 'Manglende afslutnings tag';
  sIllCharInRef      = 'Ugyldige tegn i reference';
  sUndeclaredEntity  = 'Der refereres til en ikke deklareret entitet';
  sExpectedString    = 'Forventede tekst streng ';
  sSpaceExpectedAt   = 'Forventede whitespace ved byte ';
  sUnexpEndOfInput   = 'End of input fundet mens der ledes efter delimiter';
  sQuoteExpected     = 'Forventet " or '+Chr(39);
  sInvalidXMLVersion = 'XMLPartner does not support XML specification greater than %s'; {!!.51}
  sUnableCreateStr   = 'Unable to create stream for input.';
  sInvalidName       = 'Invalid XML name';
  sInvalidCommentText= 'Invalid comment text';
  sCommentBeforeXMLDecl = 'Document cannot start with a comment if it also contains an XML declaration';
  sInvalidCDataSection = 'Invalid characters in CDATA section';
  sRedefinedAttr     = 'Attributes cannot be redefined in a start tag';
  sCircularEntRef    = 'Circular reference to: ';
  sInvAttrChar       = 'Invalid character in attribute value: ';
  sInvPCData         = 'Invalid characters in element''s character data: ';
  sDataAfterValDoc   = 'There is invalid data after valid XML document';
  sNoIntConditional  = 'Conditional sections not allowed in internal subset of document type declaration';
  sNotationNotDeclared = 'Notation not declared: ';
  sInvPubIDChar      = 'Invalid PublicID character: ';
  sNoNDATAInPeDecl   = 'NDATA not allowed in parameter entity declaration.';
  sInvStandAloneVal  = 'Standalone value must equal ''yes'' or ''no''';
  sInvEncName        = 'Invalid encoding declaration: ';
  sInvVerNum         = 'Invalid XML version number: ';
  sInvEntityValue    = 'Invalid character in entity value: ';
  sNoCommentInMarkup = 'Comments can not be placed within other markup';
  sNoPEInIntDTD      = 'Parameter entities not allowed in DTD internal subset';
  sXMLDecNotAtBeg   =  'The XML declaration must appear before the first element';
  sInvalidElementName = 'Invalid element name: ';
  sBadParamEntNesting = 'Parameter-entity text must be properly nested: '; {!!.51}
  sInvalidCharEncoding = 'Invalid character encoding specified.';
  sAttrNotNum        = 'Attribute %s of element %s does not have an integer value.'; {!!.52}
  sUnknownAxis       = 'Unknown axis specifier: %s';                   {!!.52}
  sMalformedIgnore   = 'Encountered malformed IGNORE section.';        {!!.52}
  sExpectedElement   = 'Expected start of new element.';               {!!.52}
  sAbsoluteInetPathRequired = 'Relative path %s encountered before absolute internet path established.'; {!!.52}
  sMixedRelPathInet  = 'Cannot mix relative internet path %s with local path %s'; {!!.52}
  sMixedInetLocalPath = 'Cannot mix absolute local path %s with internet path %s'; {!!.52}
  sStringNaN = 'String %s cannot be converted into a valid number.';   {!!.52}
  sXPathNoHandler    = 'No handler specified for %s() function.';      {!!.53}
  sXPathExpectedNodeSet = 'Node set expected %s';                      {!!.53}
  sXPathForDocumentParm2 = 'for 2nd parameter of document() function'; {!!.53}
  sXPathNotUnparsedEnt = 'Entity %s is not an unparsed entity.';       {!!.53}
  sXPathUnparsedEntCount = 'Multiple entity declarations found for name %s'; {!!.53}
  sXPathNoDTD = 'No DTD in document for unparsed-entity-uri(%s)';      {!!.53}

  {xpchrstm related errors}
  sInvalidXMLChar    = 'Invalid XML Character found';
  sInvalidBEChar     = 'Invalid (big-endian) UTF-16 character encoding';
  sInvalidLEChar     = 'Invalid (little-endian) UTF-16 character encoding';
  sBadUTF8Char       = 'Badly formed UTF-8 character in stream';
  sErrEndOfDocument  = 'Unexpected end of document stream';
  sUCS_ISOConvertErr = 'Cannot convert UCS-4 character to ISO-8859-1';
  sUCS_U16ConvertErr = 'Cannot convert UCS-4 character to UTF-16';
  sUCS_U8ConverErr   = 'Cannot convert UCS-4 character to UTF-8';
{$ENDIF}
