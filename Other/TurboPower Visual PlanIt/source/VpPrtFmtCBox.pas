{*********************************************************}
{*               VPPRTFMTCBOX.PAS 1.03                   *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I Vp.INC}

unit VpPrtFmtCBox;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  VpBase,
  VpBaseDS,
  VpConst;

type
  TVpPrintFormatComboBox = class (TCustomComboBox)
    private
      FControlLink : TVpControlLink;

    protected
      procedure Change; override;
      function GetAbout : string;
      procedure Loaded; override;
      procedure Notification (AComponent : TComponent;
                              Operation  : TOperation); override;
      procedure SetAbout (const Value : string);
      procedure SetControlLink (const v : TVpControlLink);
      procedure VpPrintFormatChanged (var Msg : TMessage); message Vp_PrintFormatChanged;

    public
      constructor Create (AOwner : TComponent); override;
      destructor Destroy; override;

      procedure UpdateItems;

      property Style;

    published
      property Version : string read GetAbout write SetAbout stored False;
      property ControlLink : TVpControlLink
               read FControlLink write SetControlLink;

      property Cursor;
      property DragCursor;
      property DragMode;
      property DropDownCount;
      property Enabled;
      property Font;
      property HelpContext;
      property Hint;
      property ImeMode;
      property ImeName;
      property ItemHeight;
      property Items;
      property MaxLength;
      property ParentColor;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property ShowHint;
      property Sorted;
      property TabStop;
      property TabOrder;
      property Text;
      property Visible;

      property OnChange;
      property OnClick;
      property OnDblClick;
      property OnDragDrop;
      property OnDragOver;
      property OnEndDrag;
      property OnEnter;
      property OnExit;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
      property OnMeasureItem;
      property OnMouseDown;
      property OnMouseMove;
      property OnMouseUp;
      property OnStartDrag;
  end;

function SearchControlLink (const C : TComponent) : TVpControlLink;

implementation

function SearchControlLink (const C : TComponent) : TVpControlLink;

  function FindControlLink (const C : TComponent) : TVpControlLink;
  var
    I  : Integer;
  begin
    Result := nil;
    if not Assigned (C) then
      Exit;

    {Look through all of the owned components}
    for I := 0 to C.ComponentCount - 1 do begin
      if C.Components[I] is TVpControlLink then begin
        Result := TVpControlLink (C.Components[I]);
        Exit;
      end;

      {If this isn't one, see if it owns other components}
      Result := FindControlLink (C.Components[I]);
    end;
  end;

begin
  {Search the entire form}
  Result := FindControlLink (C);
end;

constructor TVpPrintFormatComboBox.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);

  Style  := csDropDownList;
  Sorted := True;

  FControlLink := SearchControlLink (Owner); 
  UpdateItems;
end;

destructor TVpPrintFormatComboBox.Destroy;
begin
  if (HandleAllocated) and (Assigned (FControlLink)) then
    FControlLink.Printer.DeregisterWatcher (Handle);
    
  inherited Destroy;
end;

procedure TVpPrintFormatComboBox.Change;
var
  NewFormat : Integer;

begin
  if not Assigned (FControlLink) then
    Exit;

  NewFormat := FControlLink.Printer.Find (Text);
  if NewFormat >= 0 then
    FControlLink.Printer.CurFormat := NewFormat;
end;

function TVpPrintFormatComboBox.GetAbout: string;
begin
  Result := VpVersionStr;
end;

procedure TVpPrintFormatComboBox.Loaded;
begin
  inherited Loaded;

  UpdateItems;
end;

procedure TVpPrintFormatComboBox.Notification (AComponent : TComponent;
                                                Operation  : TOperation);
{ Handle new/deleted components }
begin
  inherited Notification (AComponent, Operation);

  if Operation = opRemove then begin
    { Owned components going away }
    if AComponent = FControlLink then begin
      FControlLink := nil;
      UpdateItems;
    end;
  end else if Operation = opInsert then begin
    { Check for new TVpControlLink }
    if AComponent is TVpControlLink then begin
      if not Assigned (FControlLink) then begin
        FControlLink := TVpControlLink (AComponent);
        UpdateItems;
      end;
    end;
  end;
end;

procedure TVpPrintFormatComboBox.SetAbout(const Value: string);
begin
  //Empty on purpose
end;

procedure TVpPrintFormatComboBox.SetControlLink (const v : TVpControlLink);
begin
  if v <> FControlLink then begin
    if Assigned (FControlLink) then
      FControlLink.Printer.DeregisterWatcher (Handle);
    FControlLink := v;
    if Assigned (FControlLink) then
      FControlLink.Printer.RegisterWatcher (Handle);
    UpdateItems;
  end;
end;

procedure TVpPrintFormatComboBox.VpPrintFormatChanged (var Msg : TMessage);
begin
  UpdateItems;
end;

procedure TVpPrintFormatComboBox.UpdateItems;
var
  i   : Integer;
  Ctr : Integer;

begin
  if not Assigned (FControlLink) then
    Exit;
  if not Assigned (FControlLink.Printer) then
    Exit;
  if csDesigning in ComponentState then
    Exit;

  Items.Clear;
    
  Ctr := 0;

  for i := 0 to FControlLink.Printer.PrintFormats.Count - 1 do begin
    if (FControlLink.Printer.PrintFormats.Items[i].FormatName <> '') and
       (FControlLink.Printer.PrintFormats.Items[i].Visible) then
      Items.Add (FControlLink.Printer.PrintFormats.Items[i].FormatName)
    else if FControlLink.Printer.PrintFormats.Items[i].Visible then begin
      Items.Add ('Unknown ' + IntToStr (Ctr));
      Inc (Ctr);
    end;
  end;
  i := 0;
  while i < Items.Count do
    if FControlLink.Printer.PrintFormats.Items[FControlLink.Printer.CurFormat].FormatName =
       Items[i] then begin
      ItemIndex := i;
      Break;
    end else
      Inc (i);
  if i >= Items.Count then
    ItemIndex := -1;
end;

initialization

  RegisterClass (TVpPrintFormatComboBox);

end.
