
{************************************************************}
{                                                            }
{      TDBComboBoxPlus & TComboBoxPlus Component             }
{                                                            }
{  Copyright (c) 1995,1996,1997,1998 Out & About Production  }
{Portions Copyright (c) 1995,96,97,98 Inprise International  }
{                                                            }
{                     Version 4.00                           }
{                                                            }
{************************************************************}

{$IFDEF VER100}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER110}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER120}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER130}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER150}
  {$DEFINE D3OR4OR5}
{$ENDIF}

unit dblup1b;

interface

uses
  SysUtils, Messages,
{$IFDEF Win32}
  Windows,
{$ELSE}
  WinTypes,
{$ENDIF}

{$IFDEF D3OR4OR5}
  DBCtrls,
{$ENDIF}

  DBTables,
  Classes, Graphics, Menus, Controls, Forms,
  StdCtrls, Grids, Buttons,
  DB,
  Dialogs, DBLUP1A;

Type
 TDBComboBoxPlus = class(TComboBoxPlus)
 private
    FFieldLink: TFieldDataLink;
    fPaintCopyCanvas : TControlCanvas;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure FieldLinkActive(Sender: TObject);
    function GetTextMargins: TPoint;
{    procedure WMPaint(var Message: TWMPaint); message WM_PAINT; }
{$IFDEF Win32}
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
{$ENDIF}
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CreateWnd; override;
    procedure Change; override;
    procedure Loaded; override;
    function Editable: Boolean; override;
    function CanEdit: Boolean; override;
    procedure edit; override;
    function editing : boolean; override;
    function CanModify : Boolean; override;
    procedure Reset; override;
    procedure DataModified; override;
    procedure UpdateRecord; override;
    Procedure DrawNonEditFocusRect(var Message: TWMPaint); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

implementation

uses
{$IFNDEF Win32}
  WinProcs,
{$ENDIF}
 Consts;

{------------------------------------------------------------}
                        { TDBComboBoxPlus }
{------------------------------------------------------------}
constructor TDBComboBoxPlus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFieldLink := TFieldDataLink.Create;
{$IFDEF WIN32}
  ControlStyle := ControlStyle + [csReplicatable];
{$ENDIF}
  FFieldLink.Control := Self;
  FFieldLink.OnDataChange := DataChange;
  FFieldLink.OnEditingChange := EditingChange;
  FFieldLink.OnUpdateData := UpdateData;
  FFieldLink.OnActiveChange := FieldLinkActive;
  FPaintCopyCanvas := nil;
end;

destructor TDBComboBoxPlus.Destroy;
begin
  FFieldLink.OnDataChange := nil;
  FFieldLink.Free;
  FFieldLink := nil;
  FPaintCopyCanvas.Free;
  inherited Destroy;
end;

function TDBComboBoxPlus.GetDataField: string;
begin
  Result := FFieldLink.FieldName;
end;

procedure TDBComboBoxPlus.SetDataField(const Value: string);
begin
  FFieldLink.FieldName := Value;
end;

function TDBComboBoxPlus.GetDataSource: TDataSource;
begin
  Result := FFieldLink.DataSource;
end;

procedure TDBComboBoxPlus.SetDataSource(Value: TDataSource);
begin
{$IFDEF Win32}
  if not (FFieldLink.DataSourceFixed and (csLoading in ComponentState)) then
{$ENDIF}
  FFieldLink.DataSource := Value;
{$IFDEF Win32}
  if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
end;

procedure TDBComboBoxPlus.DataChange(Sender: TObject);
begin
  if (FFieldLink.Field <> nil) and
      not (csLoading in ComponentState) then
  begin
    Value := FFieldLink.Field.AsString;
    if value = '' then
      Text := FFieldLink.Field.AsString;
  end
  else
    Text := EmptyStr;
end;

procedure TDBComboBoxPlus.CreateWnd;
begin
  inherited CreateWnd;
  DataChange (Self);  {update to current value}
end;

procedure TDBComboBoxPlus.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FFieldLink <> nil) then
    if (AComponent = DataSource) then
      DataSource := nil;
end;

procedure TDBComboBoxPlus.Loaded;
begin
  inherited Loaded;
  DataChange(Self);
end;

procedure TDBComboBoxPlus.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not CanEdit;
end;

procedure TDBComboBoxPlus.UpdateData(Sender: TObject);
var
  str : String;
begin
  if FFieldLink.Field <> nil then
  begin
    if Editable then
    begin
      if Style in [csIncSrchEdit] then
      begin  {bypass the new record if it's really OK data. See also the vk_enter in KeyPress}
        Str := Text;
        If AnsiCompareText(Str,
           Copy(TDDLinkPlus(DDLink).Cols[1].Strings[Row],
           1, Length(TDDLinkPlus(DDLink).Cols[1].Strings[Row]))) = 0 then
          Text := TDDLinkPlus(DDLink).Cols[1].Strings[Row];
      end;
      if not assigned(fOnNewLookupRec) or (DisplayValue = '') then
        FFieldLink.Field.AsString := Text
      else
      begin
        if (TDDLinkPlus(DDLink).Cols[1].Strings[Row] <> DisplayValue) then
          EnterLookupRec;
        if ((not TDDLinkPlus(DDLink).LookupActive) and (Style = csIncSrchEdit)) then
          FFieldLink.Field.AsString := Text
        else
          FFieldLink.Field.AsString := TDDLinkPlus(DDLink).Cols[0].Strings[row];
      end
    end
    else
      FFieldLink.Field.AsString := TDDLinkPlus(DDLink).Value;
  end;
end;

procedure TDBComboBoxPlus.FieldLinkActive(Sender: TObject);
begin
  if FFieldLink.Active then
  begin
    TDDLinkPlus(DDLink).Value := '';   {force a data update}
    DataChange (Self)
  end;
end;

procedure TDBComboBoxPlus.Change;
begin
  if FFieldLink.Editing then
    FFieldLink.Modified;
  inherited Change;
end;

function TDBComboBoxPlus.GetReadOnly: Boolean;
begin
  Result := FFieldLink.ReadOnly;
end;

procedure TDBComboBoxPlus.SetReadOnly(Value: Boolean);
begin
  FFieldLink.ReadOnly := Value;
  inherited ReadOnly := not CanEdit;
end;

function TDBComboBoxPlus.Editable: Boolean;
begin

  Result := (FFieldLink.DataSource = nil) or {editable if there is no connection a DB}
    (( Not(TDDLinkPlus(DDLink).LookupActive) or Assigned(fOnNewLookupRec)) and
      (Style = csIncSrchEdit));
  If (FFieldLink.DataSource = nil) and
    (Style in [csIncSearch]) then
    result := False;
end;

function TDBComboBoxPlus.CanEdit: Boolean;
begin
  Result := (FFieldLink.DataSource = nil) or
    (FFieldLink.Editing and Editable);
  If (FFieldLink.DataSource = nil) then
    result := inherited CanEdit;
end;

procedure TDBComboBoxPlus.edit;
begin
  FFieldLink.Edit;
end;

function TDBComboBoxPlus.editing : boolean;
begin
  result := (FFieldLink.DataSource = nil) or FFieldLink.Editing;
end;

function TDBComboBoxPlus.CanModify : Boolean;
begin
  if (FFieldLink.DataSource <> nil) then
    result := (FFieldLink.DataSource.DataSet.CanModify)
  else
    result := (FFieldLink.DataSource.DataSet.CanModify);
end;

procedure TDBComboBoxPlus.Reset;
begin
  if (FFieldLink.DataSource <> nil) then
    FFieldLink.Reset;
end;

 procedure TDBComboBoxPlus.DataModified;
 begin
    FFieldLink.Modified;
 end;

procedure TDBComboBoxPlus.UpdateRecord;
begin
  FFieldLink.UpdateRecord;
end;


function TDBComboBoxPlus.GetTextMargins: TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  if NewStyleControls then
  begin
    if BorderStyle = bsNone then I := 0 else
      if Ctl3D then I := 1 else I := 2;
{$IFDEF WIN32}
    Result.X := SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF + I;
{$ELSE}
    Result.X := I;
{$ENDIF}
    Result.Y := I;
  end else
  begin
    if BorderStyle = bsNone then I := 0 else
    begin
      DC := GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      I := SysMetrics.tmHeight;
      if I > Metrics.tmHeight then I := Metrics.tmHeight;
      I := I div 4;
    end;
    Result.X := I;
    Result.Y := I;
  end;
end;

Procedure TDBComboBoxPlus.DrawNonEditFocusRect(var Message: TWMPaint);
var
  ARect: TRect;
  DC: HDC;
  Margins: TPoint;
  PS: TPaintStruct;
{$IFNDEF Win32}
  S: array[0..255] of Char;
{$ELSE}
  S : String;
{$ENDIF}
begin
{$IFNDEF Win32}
  inherited DrawNonEditFocusRect(Message);
{$ELSE}
  if not (csPaintCopy in ControlState) then
    inherited
  else
  if (csPaintCopy in ControlState) then
  begin
    if FPaintCopyCanvas = nil then
    begin
      FPaintCopyCanvas := TControlCanvas.Create;
      FPaintCopyCanvas.Control := Self;
    end;
    DC := Message.DC;
    if DC = 0 then DC := BeginPaint(Handle, PS);
    FPaintCopyCanvas.Handle := DC;
    try
      FPaintCopyCanvas.Font := Font;
      with FPaintCopyCanvas do
      begin
        ARect := ClientRect;
        if not (NewStyleControls and Ctl3D) and (BorderStyle = bsSingle) then
        begin
          Brush.Color := clWindowFrame;
          FrameRect(ARect);
          InflateRect(ARect, -1, -1);
        end;
        Brush.Color := Color;
        if not Enabled then
          Font.Color := clGrayText;
        dec(ARect.Right, Button.WIdth);
        if (csPaintCopy in ControlState) and (FFieldLink.Field <> nil) then
        begin
          if LookupActive then
            S := ''
          else
            S := FFieldLink.Field.DisplayText;
          Margins := GetTextMargins;
          TextRect(ARect, Margins.X, Margins.Y, S);
        end;
      end;
    finally
      FPaintCopyCanvas.Handle := 0;
      if Message.DC = 0 then EndPaint(Handle, PS);
    end;
  end;
{$ENDIF}
end;

{$IFDEF Win32}
procedure TDBComboBoxPlus.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FFieldLink);
end;
{$ENDIF}


end.

