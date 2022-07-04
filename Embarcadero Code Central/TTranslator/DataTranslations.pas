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

unit DataTranslations;

interface

uses
  Dialogs, TranslatorInterfaces;

type
  TMessageDlgProc = function(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
  TShowMessageProc = procedure(const Msg: string);

function TranslateMessage(const AProperty : String; Variables : array of String) : String; overload;
function TranslateMessage(const AProperty : String) : String; overload;

function TranslateMessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
procedure TranslateShowMessage(const Msg: string);

function MsgTranslator : IStringTranslator;
procedure SetMsgTranslator(ATrans : IStringTranslator);

procedure SetShowMessage(AProc : TShowMessageProc);
procedure SetMessageDlg(AProc : TMessageDlgProc);

const
{1}   E_ValueInvalidType                  = 'E_ValueInvalidType';
{2}   E_ValueInvalidDate                  = 'E_ValueInvalidDate';
{3}   E_ValueInvalidTime                  = 'E_ValueInvalidTime';
{4}   E_ValueInvalidInteger               = 'E_ValueInvalidInteger';
{5}   E_ValueInvalidNumber                = 'E_ValueInvalidNumber';
{6}   E_ValueInvalidPercent               = 'E_ValueInvalidPercent';
{7}   E_ValueInvalidCurrency              = 'E_ValueInvalidCurrency';

{8}   E_CouldntSetValue 	                 = 'E_CouldntSetValue';
{9}   E_RestoringOldValue                 = 'E_RestoringOldValue';
{10}  E_RestoreOldValue                   = 'E_RestoreOldValue';

{11}  E_IndexOutOfBounds		               = 'E_IndexOutOfBounds';
{12}  E_InvalidParameter                  = 'E_InvalidParameter';
{13}  E_InvalidValue                      = 'E_InvalidValue';
{14}  E_ErrorOccuredWhileFreeing          = 'E_ErrorOccuredWhileFreeing';
{15}  E_FieldReadOnly                     = 'E_FieldReadOnly';
{16}  E_NegativeIndex                     = 'E_NegativeIndex';
{17}  E_NoSuchType                        = 'E_NoSuchType';
{18}  E_OnlyTDataRowCanHaveCombo          = 'E_OnlyTDataRowCanHaveCombo';
{19}  E_OnlyTKeyFieldCanHaveCombo         = 'E_OnlyTKeyFieldCanHaveCombo';
{20}  E_ThisIsA                           = 'E_ThisIsA';
{21}  E_TooLargeIndex                     = 'E_TooLargeIndex';

{22}  I_CanNotDeleteRowsContainingLocked  = 'I_CanNotDeleteRowsContainingLocked';
{23}  I_CanNotDeleteSubtotal              = 'I_CanNotDeleteSubtotal';
{24}  I_StillHaveInvalidKeys              = 'I_StillHaveInvalidKeys';
{25}  I_YouCanNotLockMoreColsThanSpace    = 'I_YouCanNotLockMoreColsThanSpace';
{26}  I_KeyCombinationExists              = 'I_KeyCombinationExists';
{27}  I_OldRowCanNotBeOverwritten         = 'I_OldRowCanNotBeOverwritten';

{28}  Q_DeleteSelectedRow                 = 'Q_DeleteSelectedRow';
{29}  Q_DeleteSelectedRows                = 'Q_DeleteSelectedRows';
{30}  Q_OverwriteOld                      = 'Q_OverwriteOld';
{31}  Q_SaveChanges                       = 'Q_SaveChanges';

implementation

uses
  DataEditorConstants;

var
  FDefaultTranslator : TDefaultTranslator = nil;
  FMsgTranslator : IStringTranslator = nil;
  FShowMessage : TShowMessageProc = nil;
  FMessageDlg : TMessageDlgProc = nil;

const
  DefaultTranslations : array [1..31] of TDoubleStringArray = (
{1}    (E_ValueInvalidType                    , 'The given value ''<$>'' is not valid!'),
{2}    (E_ValueInvalidDate                    , 'The given value ''<$>'' is not a valid date!'),
{3}    (E_ValueInvalidTime                    , 'The given value ''<$>'' is not a valid time!'),
{4}    (E_ValueInvalidInteger                 , 'The given value ''<$>'' is not a valid integer!'),
{5}    (E_ValueInvalidNumber                  , 'The given value ''<$>'' is not a valid number!'),
{6}    (E_ValueInvalidPercent                 , 'The given value ''<$>'' is not a valid percent value!'),
{7}    (E_ValueInvalidCurrency                , 'The given value ''<$>'' is not a valid currency value!'),
{8}    (E_CouldntSetValue                     , 'Could not set the value to ''<$>''!'),
{9}    (E_RestoringOldValue                   , 'Restoring old value!'),
{10}   (E_RestoreOldValue                     , 'Restore old value?'),
{11}   (E_IndexOutOfBounds                    , 'Index out of bounds'),
{12}   (E_InvalidParameter                    , 'Invalid parameter!'),
{13}   (E_InvalidValue                        , 'Invalid Value!'),
{14}   (E_ErrorOccuredWhileFreeing            , 'An error occured while freeing objects!'),
{15}   (E_FieldReadOnly                       , 'Field is ReadOnly!'),
{16}   (E_NegativeIndex                       , 'Negative index'),
{17}   (E_NoSuchType                          , 'No such type!'),
{18}   (E_OnlyTDataRowCanHaveCombo            , 'Only rows of type TDataRow can have combos!'),
{19}   (E_OnlyTKeyFieldCanHaveCombo           , 'Only fields of type TKeyField can have combos!'),
{20}   (E_ThisIsA                             , 'This is a'),
{21}   (E_TooLargeIndex                       , 'Too large index'),
{22}   (I_CanNotDeleteRowsContainingLocked    , 'You can not delete rows containing locked months'),
{23}   (I_CanNotDeleteSubtotal                , 'You can not delete a subtotal row!'),
{24}   (I_StillHaveInvalidKeys                , 'You still have rows with invalid key values!'),
{25}   (I_YouCanNotLockMoreColsThanSpace      , 'You can not lock more columns than there is space on the screen!'),
{26}   (I_KeyCombinationExists                , 'This key combination already exists for a row.'),
{27}   (I_OldRowCanNotBeOverwritten           , 'The old row can not be overwritten!'),
{28}   (Q_DeleteSelectedRow                   , 'Do you want to delete the selected row?'),
{29}   (Q_DeleteSelectedRows                  , 'Do you want to delete the selected rows?'),
{30}   (Q_OverwriteOld                        , 'Do you want to owerwrite the old row?'),
{31}   (Q_SaveChanges                         , 'Save changes to')
  );

function DefaultTranslator : IStringTranslator;
begin
  if not Assigned(FDefaultTranslator) then
    FDefaultTranslator := TDefaultTranslator.Create(DefaultTranslations);

  Result := FDefaultTranslator;
end;

function TranslateMessage(const AProperty : String) : String;
begin
  if not MsgTranslator.HasTranslation(AProperty, Result) and
     not DefaultTranslator.HasTranslation(AProperty, Result) then
    Result := DefaultTranslator.GetString(E_ValueInvalidType);
end;

function TranslateMessage(const AProperty : String; Variables : array of String) : String;
begin
  if not MsgTranslator.HasTranslation(AProperty, Variables, Result) and
     not DefaultTranslator.HasTranslation(AProperty, Variables, Result) then
    Result := DefaultTranslator.GetString(E_ValueInvalidType, Variables);
end;

procedure SetMsgTranslator(ATrans : IStringTranslator);
begin
  FMsgTranslator := ATrans;
end;

function MsgTranslator : IStringTranslator;
begin
  if Assigned(FMsgTranslator) then
    Result := FMsgTranslator
  else
    Result := DefaultTranslator;
end;

procedure SetShowMessage(AProc : TShowMessageProc);
begin
  FShowMessage := AProc;
end;

procedure SetMessageDlg(AProc : TMessageDlgProc);
begin
  FMessageDlg := AProc;
end;

function TranslateMessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
begin
  if Assigned(FMessageDlg) then
    Result := FMessageDlg(Msg, DlgType, Buttons, HelpCtx)
  else
    Result := MessageDlg(Msg, DlgType, Buttons, HelpCtx);
end;

procedure TranslateShowMessage(const Msg: string);
begin
  if Assigned(FShowMessage) then
    FShowMessage(Msg)
  else
    ShowMessage(Msg);
end;

initialization
finalization
  FDefaultTranslator.Free;

end.


















