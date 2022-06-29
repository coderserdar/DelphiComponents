{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: DataEditorConstants.pas,v 1.11 2003/03/24 12:57:40 mvj Exp $}

unit DataEditorConstants;
{$i common.inc}

interface

const
(*  { Error messages }
  MSGE_IndexOutOfBounds = 'Index out of bounds';
  MSGE_InvalidParameter = 'Invalid parameter!';
  MSGE_InvalidValue = 'Invalid Value!';
  MSGE_ErrorOccuredWhileFreeing = 'An error occured while freeing objects!';
  MSGE_FieldReadOnly = 'Field is ReadOnly!';
  MSGE_NegativeIndex = 'Negative index';
  MSGE_NoSuchType = 'No such type!';
  MSGE_OnlyTDataRowCanHaveCombo = 'Only rows of type TDataRow can have combos!';
  MSGE_OnlyTKeyFieldCanHaveCombo = 'Only fields of type TKeyField can have combos!';
  MSGE_ThisIsA = 'This is a';
  MSGE_TooLargeIndex = 'Too large index';

  { Information messages }
  MSGI_CanNotDeleteRowsContainingLocked = 'You can not delete rows containing locked months';
  MSGI_CanNotDeleteSubtotal = 'You can not delete a subtotal row!';
  MSGI_StillHaveInvalidKeys = 'You still have rows with invalid key values!';
  MSGI_YouCanNotLockMoreColsThanSpace = 'You can not lock more columns than there is space on the screen!';
  MSGI_KeyCombinationExists = 'This key combination already exists for a row.';
  MSGI_OldRowCanNotBeOverwritten = 'The old row can not be overwritten!';

  { Question messages }
  MSGQ_DeleteSelectedRow = 'Do you want to delete the selected row?';
  MSGQ_DeleteSelectedRows = 'Do you want to delete the selected rows?';
  MSGQ_OverwriteOld = 'Do you want to owerwrite the old row?';
  MSGQ_SaveChanges = 'Save changes to';
*)
  { Component default sizes and texts }
  PanBorWidth = 5;
  PanTotBorWidth = 6;
  PanTotDefHeight = 31;
  PanTotDescTop = 6;
  PanTotDescLeft = 6;
  PanTotDescHeight = 18;
  LabTotDescLeft = 0;
  LabTotDescTop = 0;
  LabTotDescWidth = 27;
  LabTotDescHeight = 13;
  LabTotDescCaption = 'Total:';

  { Value formatting }
  CurrencyFormatString = '.00';

  { Clipbrd }
  KEY_NULL : char = #0;
  KEY_TABCHAR : char = #9;
  NEW_LINE : string[2] = #13#10;

implementation

end.

