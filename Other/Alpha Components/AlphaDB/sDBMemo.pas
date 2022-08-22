unit sDBMemo;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, DBCtrls,
  sConst, sCommonData, sDefaults, acSBUtils;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBMemo = class(TDBMemo)
  private
    FOnVScroll: TNotifyEvent;
    FOnScrollCaret: TNotifyEvent;
    FBoundLabel: TsBoundLabel;
    FEmptyIsNull: boolean;
    FCommonData: TsScrollWndData;
    FDisabledKind: TsDisabledKind;
    FOldDataChange: TNotifyEvent;
    procedure SetDisabledKind(const Value: TsDisabledKind);
    procedure DataChange(Sender: TObject);
    function FontStored: boolean;
  protected
    procedure WndProc (var Message: TMessage); override;
    procedure Change; override;
  public
    ListSW: TacScrollWnd;
    procedure AfterConstruction; override;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    function IsEmpty: boolean;
  published
    property Font stored FontStored;
    property ParentFont stored FontStored;
    property BoundLabel: TsBoundLabel read FBoundLabel write FBoundLabel;
    property CharCase;
    property DisabledKind: TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property EmptyIsNull: boolean read FEmptyIsNull write FEmptyIsNull default False;
    property SkinData: TsScrollWndData read FCommonData write FCommonData;
  end;


implementation

uses
  DB,
  sStyleSimply, sMessages, sVCLUtils, sGraphUtils;


type
  _TDBMemo = class(TCustomMemo)
  private
    FDataLink: TFieldDataLink;
  end;


procedure TsDBMemo.AfterConstruction;
begin
  inherited AfterConstruction;
  FCommonData.Loaded;
end;

constructor TsDBMemo.Create(AOwner: TComponent);
begin
  FCommonData := TsScrollWndData.Create(Self, True);
  FCommonData.COC := COC_TsMemo;
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque];
  FDisabledKind := DefDisabledKind;
  FEmptyIsNull := False;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
  ParentColor := False;                                 
  FOldDataChange := _TDBMemo(Self).FDataLink.OnDataChange;
  _TDBMemo(Self).FDataLink.OnDataChange := DataChange;
end;


procedure TsDBMemo.DataChange(Sender: TObject);
begin
  FOldDataChange(Sender);
end;


destructor TsDBMemo.Destroy;
begin
  if ListSW <> nil then
    FreeAndNil(ListSW);

  FreeAndNil(FBoundLabel);
  FreeAndNil(FCommonData);
  inherited Destroy;
end;


function TsDBMemo.FontStored: boolean;
begin
  Result := IsCustomFont(Self, Font, not SkinData.Skinned or SkinData.CustomFont);
end;


procedure TsDBMemo.Loaded;
begin
  inherited Loaded;
  FCommonData.Loaded;
  RefreshEditScrolls(SkinData, ListSW);
end;


procedure TsDBMemo.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsDBMemo.WndProc(var Message: TMessage);
begin
  if Message.Msg = SM_ALPHACMD then
    case Message.WParamHi of
      AC_CTRLHANDLED: begin
        Message.Result := 1;
        Exit;
      end; // AlphaSkins supported

      AC_SETNEWSKIN: begin
        CommonWndProc(Message, FCommonData);
        Exit;
      end;

      AC_REMOVESKIN: begin
        if ListSW <> nil then
          FreeAndNil(ListSW);

        CommonWndProc(Message, FCommonData);
        RecreateWnd;
        Exit;
      end;

      AC_REFRESH: begin
        CommonWndProc(Message, FCommonData);
        Repaint;
        RefreshEditScrolls(SkinData, ListSW);
        Exit;
      end;

      AC_ENDPARENTUPDATE:
        if FCommonData.Updating then begin
          FCommonData.Updating := False;
          Perform(WM_NCPAINT, 0, 0);
          Exit;
        end;

      AC_GETDEFINDEX: begin
        if FCommonData.SkinManager <> nil then
          Message.Result := FCommonData.SkinManager.SkinCommonInfo.Sections[ssEdit] + 1;

        Exit;
      end;
    end;

  if not ControlIsReady(Self) or not FCommonData.Skinned then
    inherited
  else begin
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      CM_SHOWINGCHANGED:
        RefreshEditScrolls(SkinData, ListSW);

      EM_SETSEL:
        if Assigned(FOnScrollCaret) then
          FOnScrollCaret(Self);

      WM_HSCROLL, WM_VSCROLL:
        if (Message.Msg = WM_VSCROLL) and Assigned(FOnVScroll) then
          FOnVScroll(Self);
    end;
  end;
  if Assigned(BoundLabel) then
    BoundLabel.HandleOwnerMsg(Message, Self);
end;


function TsDBMemo.IsEmpty: boolean;
begin
  Result := Text = '';
end;


procedure TsDBMemo.Change; 
begin
  inherited;
  if EmptyIsNull and (Text = '') and (Field <> nil) then begin
    if not (Field.DataSet.State in dsEditModes{[dsEdit, dsInsert]}) then
      Field.DataSet.Edit;

    Field.Clear;
  end;
end;

end.
