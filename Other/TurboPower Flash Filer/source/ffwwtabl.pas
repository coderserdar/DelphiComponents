{*********************************************************}
{* FlashFiler: InfoPower-compatible FlashFiler table     *}
{*********************************************************}

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
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

{Notes: Based on information provided by Woll2Woll Software.
        Requires InfoPower 3.01 or later
        InfoPower is Copyright (c) 1995-1999 by Woll2Woll Software}

unit ffwwtabl;

interface

uses
  SysUtils,
  Windows,
  Classes,
  DB,
  wwFilter,
  wwStr,
  wwSystem,
  wwTable,
  wwTypes,
  ffdb;

type
  TffwwTable = class(TffTable)
    protected {private}
      FControlType    : TStrings;
      FPictureMasks   : TStrings;
      FUsePictureMask : boolean;
      FOnInvalidValue : TwwInvalidValueEvent;

    protected
      function GetControlType : TStrings;
      procedure SetControlType(CT : TStrings);
      function GetPictureMasks : TStrings;
      procedure SetPictureMasks(PM : TStrings);

      procedure DoBeforePost; override; { For picture support }

    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;

    published
      property IndexDefs;

      property ControlType : TStrings read GetControlType write SetControltype;
      property PictureMasks: TStrings read GetPictureMasks write SetPictureMasks;
      property ValidateWithMask : boolean read FUsePictureMask write FUsePictureMask;
      property OnInvalidValue: TwwInvalidValueEvent read FOnInvalidValue write FOnInvalidValue;
  end;

  TffwwQuery = class(TffQuery)
    protected {private}
      FControlType    : TStrings;
      FPictureMasks   : TStrings;
      FUsePictureMask : boolean;
      FOnInvalidValue : TwwInvalidValueEvent;

    protected
      function GetControlType : TStrings;
      procedure SetControlType(CT : TStrings);
      function GetPictureMasks : TStrings;
      procedure SetPictureMasks(PM : TStrings);

      procedure DoBeforePost; override; { For picture support }

    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;

    published
      property ControlType : TStrings read GetControlType write SetControltype;
      property PictureMasks: TStrings read GetPictureMasks write SetPictureMasks;
      property ValidateWithMask : boolean read FUsePictureMask write FUsePictureMask;
      property OnInvalidValue: TwwInvalidValueEvent read FOnInvalidValue write FOnInvalidValue;
  end;

implementation

uses
  wwCommon,
  DBConsts;

{===TffwwTable=======================================================}
constructor TffwwTable.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FControlType := TStringList.Create;
  FPictureMasks := TStringList.Create;
  FUsePictureMask := true;
end;
{--------}
destructor TffwwTable.Destroy;
begin
  FControlType.Free;
  FPictureMasks.Free;
  inherited Destroy;
end;
{--------}
Procedure TffwwTable.DoBeforePost;
begin
  inherited DoBeforePost;
  if FUsePictureMask then
    wwValidatePictureFields(Self, FOnInvalidValue);
end;
{--------}
function TffwwTable.GetControlType : TStrings;
begin
  Result := FControlType;
end;
{--------}
function TffwwTable.GetPictureMasks : TStrings;
begin
  Result:= FPictureMasks;
end;
{--------}
procedure TffwwTable.SetControlType(CT : TStrings);
begin
  FControlType.Assign(CT);
end;
{--------}
procedure TffwwTable.SetPictureMasks(PM : TStrings);
begin
  FPictureMasks.Assign(PM);
end;
{====================================================================}


{===TffwwQuery=======================================================}
constructor TffwwQuery.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FControlType := TStringList.Create;
  FPictureMasks := TStringList.Create;
  FUsePictureMask := true;
end;
{--------}
destructor TffwwQuery.Destroy;
begin
  FControlType.Free;
  FPictureMasks.Free;
  inherited Destroy;
end;
{--------}
Procedure TffwwQuery.DoBeforePost;
begin
  inherited DoBeforePost;
  if FUsePictureMask then
    wwValidatePictureFields(Self, FOnInvalidValue);
end;
{--------}
function TffwwQuery.GetControlType : TStrings;
begin
  Result := FControlType;
end;
{--------}
function TffwwQuery.GetPictureMasks : TStrings;
begin
  Result:= FPictureMasks;
end;
{--------}
procedure TffwwQuery.SetControlType(CT : TStrings);
begin
  FControlType.Assign(CT);
end;
{--------}
procedure TffwwQuery.SetPictureMasks(PM : TStrings);
begin
  FPictureMasks.Assign(PM);
end;
{====================================================================}

end.
