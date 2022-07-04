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
{* XMLPartner Pro: XpFOHash.INC                          *}
{*********************************************************}
{* XMLPartner Pro: Hash table of formatting object       *}
{*                 classes                               *}
{*********************************************************}
{$I XpDefine.inc}

interface

uses
  XpHash;

var
  xpoFOHash : TXpStrHash;
      { Used to map an FO name to the appropriate tag code that is to be
        passed to a filter. }

implementation

uses
  XpXSLCon,
{$IFDEF UsingCLX}
  XpQXSLFO;
{$ELSE}
  XpvXSLFO;
{$ENDIF}

{====================================================================}
procedure InitializeUnit;
begin
  xpoFOHash := TXpStrHash.Create(xpc_Size59);

  { Add the formatting objects to their hash table. }
  with xpoFOHash do begin
    Add(XpsFOBasicLink, Pointer(TXpFOBasicLink));
    Add(XpsFOBidiOverride, Pointer(TXpFOBidiOverride));
    Add(XpsFOBlock, Pointer(TXpFOBlock));
    Add(XpsFOBlockContainer, Pointer(TXpFOBlockContainer));
    Add(XpsFOCharacter, Pointer(TXpFOCharacter));
    Add(XpsFOColorProfile, Pointer(TXpFOColorProfile));
    Add(XpsFOConditionalPageMasterReference, Pointer(TXpFOPageRefCondition));
    Add(XpsFODeclarations, Pointer(TXpFODeclarations));
    Add(XpsFOExternalGraphic, Pointer(TXpFOExternalGraphic));
    Add(XpsFOFloat, Pointer(TXpFOFloat));
    Add(XpsFOFlow, Pointer(TXpFOFlow));
    Add(XpsFOFootnote, Pointer(TXpFOFootnote));
    Add(XpsFOFootnoteBody, Pointer(TXpFOFootnoteBody));
    Add(XpsFOInitialPropertySet, Pointer(TXpFOInitialPropertySet));
    Add(XpsFOInline, Pointer(TXpFOInline));
    Add(XpsFOInlineContainer, Pointer(TXpFOInlineContainer));
    Add(XpsFOInstreamForeignObject, Pointer(TXpFOInstreamForeignObject));
    Add(XpsFOLayoutMasterSet, Pointer(TXpFOLayoutMaster));
    Add(XpsFOLeader, Pointer(TXpFOLeader));
    Add(XpsFOListBlock, Pointer(TXpFOListBlock));
    Add(XpsFOListItem, Pointer(TXpFOListItem));
    Add(XpsFOListItemBody, Pointer(TXpFOListItemBody));
    Add(XpsFOListItemLabel, Pointer(TXpFOListItemLabel));
    Add(XpsFOMarker, Pointer(TXpFOMarker));
    Add(XpsFOMultiCase, Pointer(TXpFOMultiCase));
    Add(XpsFOMultiProperties, Pointer(TXpFOMultiProperties));
    Add(XpsFOMultiPropertySet, Pointer(TXpFOMultiPropertySet));
    Add(XpsFOMultiSwitch, Pointer(TXpFOMultiSwitch));
    Add(XpsFOMultiToggle, Pointer(TXpFOMultiToggle));
    Add(XpsFOPageNumber, Pointer(TXpFOPageNumber));
    Add(XpsFOPageNumberCitation, Pointer(TXpFOPageNumberCitation));
    Add(XpsFOPageSequence, Pointer(TXpFOPageSequence));
    Add(XpsFOPageSequenceMaster, Pointer(TXpFOPageSeqMaster));
    Add(XpsFORegionAfter, Pointer(TXpFORegionAfter));
    Add(XpsFORegionBefore, Pointer(TXpFORegionBefore));
    Add(XpsFORegionBody, Pointer(TXpFORegionBody));
    Add(XpsFORegionEnd, Pointer(TXpFORegionEnd));
    Add(XpsFORegionStart, Pointer(TXpFORegionStart));
    Add(XpsFORetrieveMarker, Pointer(TXpFORetrieveMarker));
    Add(XpsFORoot, Pointer(TXpFORoot));
    Add(XpsFORepeatablePageMasterAlternatives, Pointer(TXpFOAlternatingPageRef));
    Add(XpsFORepeatablePageMasterReference, Pointer(TXpFORepeatingPageRef));
    Add(XpsFOSimplePageMaster, Pointer(TXpFOSimplePageMaster));
    Add(XpsFOSinglePageMasterReference, Pointer(TXpFOSinglePageRef));
    Add(XpsFOStaticContent, Pointer(TXpFOStaticContent));
    Add(XpsFOTable, Pointer(TXpFOTable));
    Add(XpsFOTableAndCaption, Pointer(TXpFOTableAndCaption));
    Add(XpsFOTableBody, Pointer(TXpFOTableBody));
    Add(XpsFOTableCaption, Pointer(TXpFOTableCaption));
    Add(XpsFOTableCell, Pointer(TXpFOTableCell));
    Add(XpsFOTableColumn, Pointer(TXpFOTableColumn));
    Add(XpsFOTableFooter, Pointer(TXpFOTableFooter));
    Add(XpsFOTableHeader, Pointer(TXpFOTableHeader));
    Add(XpsFOTableRow, Pointer(TXpFOTableRow));
    Add(XpsFOTitle, Pointer(TXpFOTitle));
    Add(XpsFOWrapper, Pointer(TXpFOWrapper));
  end;  { with  }
end;
{--------}
procedure FinalizeUnit;
begin
  xpoFOHash.Free;
end;
{====================================================================}
initialization
  InitializeUnit;

finalization
  FinalizeUnit;
{====================================================================}
end.