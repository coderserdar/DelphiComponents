unit sDBLookupListBox;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, DBCtrls,
  sConst, sCommonData, sDefaults, acSBUtils;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBLookupListBox = class(TDBLookupListBox)
  private
    FBoundLabel: TsBoundLabel;
    FCommonData: TsScrollWndData;
    FDisabledKind: TsDisabledKind;
    procedure SetDisabledKind(const Value: TsDisabledKind);
    function FontStored: boolean;
  protected
    procedure WndProc (var Message: TMessage); override;
  public
    ListSW: TacScrollWnd;
    procedure AfterConstruction; override;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property Font stored FontStored;
    property ParentFont stored FontStored;
    property BoundLabel: TsBoundLabel read FBoundLabel write FBoundLabel;
    property DisabledKind: TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property SkinData: TsScrollWndData read FCommonData write FCommonData;
  end;


implementation

uses sVCLUtils, sMessages, sGraphUtils, sMaskData, sStyleSimply;


procedure TsDBLookupListBox.AfterConstruction;
begin
  inherited AfterConstruction;
  FCommonData.Loaded;
end;


constructor TsDBLookupListBox.Create(AOwner: TComponent);
begin
  FCommonData := TsScrollWndData.Create(Self, True);
  FCommonData.COC := COC_TsEdit;
  inherited Create(AOwner);
  FDisabledKind := DefDisabledKind;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
end;


destructor TsDBLookupListBox.Destroy;
begin
  if ListSW <> nil then
    FreeAndNil(ListSW);

  FreeAndNil(FBoundLabel);
  FreeAndNil(FCommonData);
  inherited Destroy;
end;


function TsDBLookupListBox.FontStored: boolean;
begin
  Result := IsCustomFont(Self, Font, not SkinData.Skinned or SkinData.CustomFont);
end;


procedure TsDBLookupListBox.Loaded;
begin
  inherited Loaded;
  FCommonData.Loaded;
  if FCommonData.Skinned then begin
    if not FCommonData.CustomColor then
      Color := FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].Props[0].Color;

    if not FCommonData.CustomFont then
      Font.Color := FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].Props[0].FontColor.Color;
  end;
end;


procedure TsDBLookupListBox.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsDBLookupListBox.WndProc(var Message: TMessage);
begin
  if Message.Msg = SM_ALPHACMD then
    case Message.WParamHi of
      AC_CTRLHANDLED: begin
        Message.Result := 1;
        Exit;
      end; // AlphaSkins supported

      AC_REMOVESKIN: begin
        if ListSW <> nil then
          FreeAndNil(ListSW);

        CommonWndProc(Message, FCommonData);
        Color := clWindow;
        RecreateWnd;
        Exit;
      end;

      AC_REFRESH: begin
        CommonWndProc(Message, FCommonData);
        if FCommonData.Skinned then begin
          if not FCommonData.CustomColor then
            Color := FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].Props[0].Color;

          if not FCommonData.CustomFont then
            Font.Color := FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].Props[0].FontColor.Color;
        end;
        SendMessage(Handle, WM_NCPAINT, 0, 0);
        RefreshEditScrolls(SkinData, ListSW);
        Exit;
      end;

      AC_ENDPARENTUPDATE:
        if FCommonData.Updating then begin
          FCommonData.Updating := False;
          Repaint;
          Exit;
        end;

      AC_SETNEWSKIN: begin
        CommonWndProc(Message, FCommonData);
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
    case Message.Msg of
      WM_ERASEBKGND, CN_DRAWITEM:
        Exit;

      WM_SETFOCUS, CM_ENTER:
        if CanFocus then begin
          inherited;
          if Focused then begin
            FCommonData.FFocused := True;
            FCommonData.FMouseAbove := False;
            FCommonData.BGChanged := True;
          end;
        end;

      WM_KILLFOCUS, CM_EXIT: begin
        FCommonData.FFocused := False;
        FCommonData.FMouseAbove := False;
        FCommonData.BGChanged := True;
      end;

      CM_MOUSEENTER, CM_MOUSELEAVE:
        if FCommonData.CommonSkinData.gd[FCommonData.Skinindex].ReservedBoolean then begin
          FCommonData.FMouseAbove := Message.Msg = CM_MOUSEENTER;
          FCommonData.BGChanged := True;
        end;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      CM_SHOWINGCHANGED:
        RefreshEditScrolls(SkinData, ListSW);

      CM_VISIBLECHANGED, CM_ENABLEDCHANGED, WM_SETFONT: begin
        FCommonData.Invalidate;
      end;
    end;
  end;
  if Assigned(BoundLabel) then
    BoundLabel.HandleOwnerMsg(Message, Self);
end;

end.
