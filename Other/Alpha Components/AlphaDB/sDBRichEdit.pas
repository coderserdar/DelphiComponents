unit sDBRichEdit;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, db, StdCtrls, ComCtrls, dbctrls,
  sConst, sCommonData, sDefaults, acSBUtils;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBRichEdit = class(TDBRichEdit)
{$IFNDEF NOTFORHELP}
  private
    FCommonData: TsScrollWndData;
    FDisabledKind: TsDisabledKind;
    FBoundLabel: TsBoundLabel;
    procedure SetDisabledKind(const Value: TsDisabledKind);
    function FontStored: boolean;
  protected
    procedure Change; override;
  public
    ListSW: TacScrollWnd;
    Refreshing: boolean;
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure WndProc (var Message: TMessage); override;
  published
    property Font stored FontStored;
    property ParentFont stored FontStored;
    property CharCase;
    property Text;
{$ENDIF} // NOTFORHELP
    property BoundLabel: TsBoundLabel read FBoundLabel write FBoundLabel;
    property SkinData: TsScrollWndData read FCommonData write FCommonData;
    property DisabledKind: TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
  end;


implementation

uses
  RichEdit,
  {$IFDEF LOGGED}sDebugMsgs, {$ENDIF}
  sStyleSimply, sVCLUtils, sMessages, sMaskData, sGraphUtils;


procedure TsDBRichEdit.AfterConstruction;
begin
  inherited AfterConstruction;
  UpdateData(FCommonData);
end;


procedure TsDBRichEdit.Change;
begin
  if not Refreshing then
    inherited;
end;


constructor TsDBRichEdit.Create(AOwner: TComponent);
begin
  FCommonData := TsScrollWndData.Create(Self, True);
  FCommonData.COC := COC_TsMemo;
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDisabledKind := DefDisabledKind;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
  Perform(EM_EXLIMITTEXT, 0, $7FFFFFF0);
  Refreshing := False;
end;


destructor TsDBRichEdit.Destroy;
begin
  FreeAndNil(ListSW);
  FreeAndNil(FBoundLabel);
  FreeAndNil(FCommonData);
  inherited Destroy;
end;


function TsDBRichEdit.FontStored: boolean;
begin
  Result := IsCustomFont(Self, Font, not SkinData.Skinned or SkinData.CustomFont);
end;


procedure TsDBRichEdit.Loaded;
begin
  inherited Loaded;
  FCommonData.Loaded;
  RefreshEditScrolls(SkinData, ListSW);
end;


procedure TsDBRichEdit.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsDBRichEdit.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then
    case Message.WParamHi of
      AC_CTRLHANDLED: begin
        Message.Result := 1;
        Exit;
      end; // AlphaSkins supported

      AC_REMOVESKIN:
        if not (csDestroying in ComponentState) then begin
          FreeAndNil(ListSW);
          CommonWndProc(Message, FCommonData);
          if not FCommonData.CustomFont then
            DefAttributes.Color := Font.Color;

          RecreateWnd;
          Exit;
        end;

      AC_REFRESH:
        if Visible then begin
          Refreshing := True;
          CommonWndProc(Message, FCommonData);
          if FCommonData.Skinned then
            if not FCommonData.CustomFont then
              DefAttributes.Color := FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].Props[0].FontColor.Color;

          SendMessage(Handle, WM_NCPaint, 0, 0);
          RefreshEditScrolls(SkinData, ListSW);
          Refreshing := False;
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

  if not ControlIsReady(Self) or not Assigned(FCommonData) or not FCommonData.Skinned then
    inherited
  else begin
    case Message.Msg of
      SM_ALPHACMD:
        case Message.WParamHi of
          AC_ENDPARENTUPDATE:
            if FCommonData.Updating then begin
              FCommonData.Updating := False;
              Perform(WM_NCPAINT, 0, 0); Exit
            end;
        end;

      WM_PRINT: begin
        Perform(WM_PAINT, Message.WParam, Message.LParam);
        Perform(WM_NCPAINT, Message.WParam, Message.LParam);
        Exit;
      end;

      WM_ENABLE:
        Exit;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      CM_SHOWINGCHANGED:
        RefreshEditScrolls(SkinData, ListSW);

      CM_ENABLEDCHANGED: begin
        if not FCommonData.CustomColor then
          Color := FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].Props[0].Color;

        if not FCommonData.CustomFont then
          with FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].Props[0] do
            if not Enabled then begin
              Font.Color := AverageColor(FontColor.Color, Color);
              DefAttributes.Color := Font.Color;
            end
            else begin
              Font.Color := FontColor.Color;
              DefAttributes.Color := FontColor.Color;
            end;
      end;
    end;
  end;
  if Assigned(BoundLabel) then
    BoundLabel.HandleOwnerMsg(Message, Self);
end;

end.
