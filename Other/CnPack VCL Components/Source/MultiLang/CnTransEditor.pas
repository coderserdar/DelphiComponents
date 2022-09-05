{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnTransEditor;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �����
* ��Ԫ���ƣ������ IDE ����༭��Ԫ
* ��Ԫ���ߣ�CnPack������ ��Х (liuxiao@cnpack.org)
* ��    ע���õ�Ԫʵ���˶������ IDE ����༭����
* ����ƽ̨��PWin2000 + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2012.11.06 V1.5
*               ����δ�����������������·���г���$(Platform)������
*           2008.10.13 V1.4
*               ������ Filter ����ĸ���ʱ�����������Ե�����
*           2008.05.30 V1.3
*               ������ֻ���� string ����
*           2004.03.05 V1.2
*               ������ TFrame ������������
*           2003.12.10 V1.1
*               ���Ӷ�����Ķ��⴦��
*           2003.08.20 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, Variants,
  {$ELSE}
  Dsgnintf,
  {$ENDIF}
  SysUtils, Classes, Forms, Windows, Messages, Dialogs, Graphics, Menus, Grids,
  ComCtrls, Controls, ExtCtrls, ToolWin, ActnList, ImgList, TypInfo, StdCtrls,
  FileCtrl,
  CnLangConsts, CnLangTranslator, CnLangMgr, CnLangCollection, CnLangStorage,
  CnIniLangFileStorage, CnIniStrUtils, CnCommon, ToolsApi, ComObj, CnOTAUtils,
  CnClasses, CnTransFilter, CnLangUtils;

type

{$IFNDEF COMPILER6_UP}
  IDesigner = IFormDesigner;
{$ENDIF}

  TCnTranslatorEditor = class(TComponentEditor)
  private
    FExtractor: TCnLangStringExtractor;
  protected
    procedure ExecFormTranslationManager;
    {* ��ʾ��������� }
  public
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    destructor Destroy; override;
    procedure Edit; override;
    {* ˫���Ĺ��� }
    procedure ExecuteVerb(Index: Integer); override;
    {* ִ���Ҽ��˵��Ĺ��� }
    function GetVerb(Index: Integer): string; override;
    {* �����Ҽ��˵���Ŀ }
    function GetVerbCount: Integer; override;
    {* �����Ҽ��˵���Ŀ�� }
    property Extractor: TCnLangStringExtractor read FExtractor;
  end;

type
  TFrmTransEditor = class(TForm)
    statTrans: TStatusBar;
    tvStorages: TTreeView;
    tlbMain: TToolBar;
    splTrans: TSplitter;
    pnlTrans: TPanel;
    clbrTrans: TCoolBar;
    pnlTitle: TPanel;
    StringGrid: TStringGrid;
    lblLangName: TLabel;
    lblLangID: TLabel;
    lblIndex: TLabel;
    lblLangNameValue: TLabel;
    lblLangIDValue: TLabel;
    lblLangIndexValue: TLabel;
    actlstTrans: TActionList;
    actGenStrs: TAction;
    ToolButton1: TToolButton;
    actCopyStrs: TAction;
    ToolButton2: TToolButton;
    actUpdateStrs: TAction;
    actSaveCurItem: TAction;
    actAddLine: TAction;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    actDelLine: TAction;
    ToolButton6: TToolButton;
    ilStorage: TImageList;
    ilImages: TImageList;
    lbl1: TLabel;
    lblFileName: TLabel;
    actClear: TAction;
    btn1: TToolButton;
    btn2: TToolButton;
    actCollectForm: TAction;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    actClose: TAction;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    actDelBlank: TAction;
    ToolButton11: TToolButton;
    btnFilter: TToolButton;
    actFilter: TAction;
    procedure tvStoragesChange(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject);
    procedure StringGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure actGenStrsExecute(Sender: TObject);
    procedure StringGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure actCopyStrsExecute(Sender: TObject);
    procedure actUpdateStrsExecute(Sender: TObject);
    procedure actAddLineExecute(Sender: TObject);
    procedure actSaveCurItemExecute(Sender: TObject);
    procedure tvStoragesCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure actDelLineExecute(Sender: TObject);
    procedure pnlTransResize(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actCollectFormExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actDelBlankExecute(Sender: TObject);
    procedure actFilterExecute(Sender: TObject);
  private
    FFormFilterOptions: TLangTransFilterSet;
    FTransEditor: TCnTranslatorEditor;
    FContainer: TWinControl;
    { Private declarations }
  public
    { Public declarations }
    procedure WriteNameValueStringsToGrid(List: TStrings; Item: TCnLanguageItem);
    procedure LoadStorageFromList(List: TList);
    procedure SetBasicInfo(List: TStrings; Item: TCnLanguageItem);
    procedure UpdateLangDisplay(Storage: TCnCustomLangStorage; Item: TCnLanguageItem);
    property TransEditor: TCnTranslatorEditor read FTransEditor write FTransEditor;
    property Container: TWinControl read FContainer write FContainer;
  end;

implementation

{$R *.dfm}

procedure SetDesignPathFile(Storage: TCnCustomLangFileStorage);
var
  aPath, aFile: string;
begin
  if Storage = nil then Exit;

  aPath := CnOtaReplaceToActualPath(CnOtaGetOutputDir);
  if aPath = '' then
    aPath := _CnExtractFilePath(CnOtaGetFileNameOfCurrentModule);
  (Storage as TCnCustomLangFileStorage).SetDesignLangPath(aPath);

  aFile := _CnChangeFileExt(CnOtaGetCurrentProjectFileName, '');
  (Storage as TCnCustomLangFileStorage).SetDesignLangFile(_CnExtractFileName(aFile));
end;

constructor TCnTranslatorEditor.Create(AComponent: TComponent;
  ADesigner: IDesigner);
begin
  inherited;
  FExtractor := TCnLangStringExtractor.Create;
end;

destructor TCnTranslatorEditor.Destroy;
begin
  FExtractor.Free;
  inherited;
end;

procedure TCnTranslatorEditor.Edit;
begin
  ExecFormTranslationManager;
end;

procedure TCnTranslatorEditor.ExecFormTranslationManager;
var
  ATranslator: TCnLangTranslator;
  Container: TComponent;
  i: Integer;
  List: TList;
begin
  if CnLanguageManager = nil then
  begin
    MessageBox(0, PChar(SCnErrorNoLangManager), PChar(SCnErrorCaption),
      MB_OK or MB_ICONWARNING);
    Exit;
  end;
  ATranslator := TCnLangTranslator(Self.Component);
  Container := ATranslator.Owner;

  List := TList.Create;
  for i := 0 to Container.ComponentCount - 1 do
    if Container.Components[i] is TCnCustomLangStorage then
    begin
      List.Add(Container.Components[i]);
      if Container.Components[i] is TCnCustomLangFileStorage then
        SetDesignPathFile(Container.Components[i] as TCnCustomLangFileStorage);
    end;

  if CnLanguageManager.LanguageStorage <> nil then
    if List.IndexOf(CnLanguageManager.LanguageStorage) < 0 then
    begin
      List.Add(CnLanguageManager.LanguageStorage);
      if CnLanguageManager.LanguageStorage is TCnCustomLangFileStorage then
        SetDesignPathFile(CnLanguageManager.LanguageStorage as TCnCustomLangFileStorage);
    end;

  if List.Count = 0 then
  begin
    MessageBox(0, PChar(SCnErrorNoStorage), PChar(SCnErrorCaption),
      MB_OK or MB_ICONWARNING);
    Exit;
  end;

  with TFrmTransEditor.Create(nil) do
  begin
    TransEditor := Self;
    LoadStorageFromList(List);
    ShowModal;
    Free;
  end;
  List.Free;
end;

procedure TCnTranslatorEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ExecFormTranslationManager;
  end;
end;

function TCnTranslatorEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SCnFormTranslationManager;
  end;
end;

function TCnTranslatorEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TFrmTransEditor }

procedure TFrmTransEditor.LoadStorageFromList(List: TList);
var
  i, j: Integer;
  ANode: TTreeNode;
begin
  Self.tvStorages.Items.Clear;
  for i := 0 to List.Count - 1 do
  begin
    ANode := Self.tvStorages.Items.AddObject(nil,
      TCnCustomLangStorage(List[i]).Name, List[i]);
    for j := 0 to TCnCustomLangStorage(List[i]).LanguageCount - 1 do
      Self.tvStorages.Items.AddChildObject(ANode,
        InttoStr(TCnCustomLangStorage(List[i]).Languages[j].LanguageID) + ' - ' +
        TCnCustomLangStorage(List[i]).Languages[j].LanguageName,
        TCnCustomLangStorage(List[i]).Languages[j]);
  end;

  for i := 0 to Self.tvStorages.Items.Count - 1 do
  begin
    Self.tvStorages.Items[i].ImageIndex := Self.tvStorages.Items[i].Level;
    Self.tvStorages.Items[i].SelectedIndex := Self.tvStorages.Items[i].Level;
    if Self.tvStorages.Items[i].Level = 0 then
      Self.tvStorages.Items[i].Expand(True);
  end;
end;

procedure TFrmTransEditor.tvStoragesChange(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.Level = 1 then
  begin
    Self.UpdateLangDisplay(TCnCustomLangStorage(Node.Parent.Data),
      TCnLanguageItem(Node.Data));
    Self.pnlTrans.Enabled := True;
  end
  else
    Self.pnlTrans.Enabled := False;
end;

procedure TFrmTransEditor.UpdateLangDisplay(Storage: TCnCustomLangStorage;
  Item: TCnLanguageItem);

  function IsLangComment(const CommentString: string): Boolean;
  begin
    Result := (CommentString[1] = SCnCommentChar1) or
              (CommentString[1] = SCnCommentChar2) or
              ((CommentString[1] = SCnCommentChar3) and
              (CommentString[2] = SCnCommentChar3));
  end;

var
  List: TStringList;
  i: Integer;
  S: WideString;
  bPrompted: Boolean;
begin
  Self.lblLangNameValue.Caption := Item.LanguageName;
  Self.lblLangIDValue.Caption := InttoStr(Item.LanguageID);
  Self.lblLangIndexValue.Caption := InttoStr(Item.Index);

  if Storage is TCnCustomLangFileStorage then
  begin
    if DirectoryExists((Storage as TCnCustomLangFileStorage).LanguagePath) then
      ForceDirectories((Storage as TCnCustomLangFileStorage).LanguagePath);

    if (Storage as TCnCustomLangFileStorage).StorageMode = smByFile then
      Self.lblFileName.Caption := Item.LanguageFileName +
        (Storage as TCnCustomLangFileStorage).GetLanguageFileExt
    else
      Self.lblFileName.Caption := '.\' + Item.LanguageDirName + '\'
        + (Storage as TCnCustomLangFileStorage).DesignLangFile
        + (Storage as TCnCustomLangFileStorage).GetLanguageFileExt;
  end;

  if Self.Container = nil then
    if (Self.TransEditor = nil) then
      Exit
    else
      Self.Container := TWinControl(TCnLangTranslator(Self.TransEditor.Component).Owner);

  Storage.CurrentLanguageIndex := Item.Index;

  List := TStringList.Create;
  Storage.GetNamesList(List);

  Self.StringGrid.RowCount := 2;
  Self.StringGrid.FixedRows := 1;

  if List.Count > 0 then
  begin
    //������к�ע��
    bPrompted := False;
    for i := List.Count - 1 downto 0 do
    begin
      if (List[i] = '') or IsLangComment(List[i]) then
      begin
        if not bPrompted then
        begin
          WarningDlg(SCnLangInvalidLine);
          bPrompted := True;
        end;
        List.Delete(i);
      end;
    end;

    Self.StringGrid.RowCount := List.Count + 1;
    for i := 1 to Self.StringGrid.RowCount - 1 do
      Self.StringGrid.Cells[0, i] := InttoStr(i);
      
    for i := 0 to List.Count - 1 do
    begin
      Self.StringGrid.Cells[1, i + 1] := List[i];
      S := List[i];
      if S[1] <> SystemNamePrefix then // �����ַ���ȡ��ԭ��ֵ
      begin
        S := GetValueByTransName(Self.Container, S);
        Self.StringGrid.Cells[2, i + 1] := S
      end;
      // ��ʾ�䷭����ֵ
      if Storage.GetString(List[i], S) then
        Self.StringGrid.Cells[3, i + 1] := S;
    end;
  end
  else
  begin
    Self.StringGrid.Rows[1].Text := '';
    Self.StringGrid.Cells[0, 1] := '1';
  end;
end;

procedure TFrmTransEditor.FormCreate(Sender: TObject);
begin
  Self.StringGrid.ColWidths[0] := 30;
  Self.StringGrid.ColWidths[1] := 141;
  Self.StringGrid.ColWidths[2] := 194;
  Self.StringGrid.ColWidths[3] := 194;

  // Localize UI
  actAddLine.Caption := SCnactAddLineCaption;
  actAddLine.Hint := SCnactAddLineHint;
  actClear.Caption := SCnactClearCaption;
  actClear.Hint := SCnactClearHint;
  actClose.Caption := SCnactCloseCaption;
  actCollectForm.Caption := SCnactCollectFormCaption;
  actCollectForm.Hint := SCnactCollectFormHint;
  actCopyStrs.Caption := SCnactCopyStrsCaption;
  actCopyStrs.Hint := SCnactCopyStrsHint;
  actDelBlank.Caption := SCnactDelBlankCaption;
  actDelBlank.Hint := SCnactDelBlankHint;
  actDelLine.Caption := SCnactDelLineCaption;
  actDelLine.Hint := SCnactDelLineHint;
  actFilter.Caption := SCnactFilterCaption;
  actFilter.Hint := SCnactFilterHint;
  actGenStrs.Caption := SCnactGenStrsCaption;
  actGenStrs.Hint := SCnactGenStrsHint;
  actSaveCurItem.Caption := SCnactSaveCurItemCaption;
  actSaveCurItem.Hint := SCnactSaveCurItemHint;
  actUpdateStrs.Caption := SCnactUpdateStrsCaption;
  actUpdateStrs.Hint := SCnactUpdateStrsHint;
  Caption := SCnCaption;
  lbl1.Caption := SCnlbl1Caption;
  lblIndex.Caption := SCnlblIndexCaption;
  lblLangID.Caption := SCnlblLangIDCaption;
  lblLangName.Caption := SCnlblLangNameCaption;
  StringGrid.Hint := SCnStringGridHint;
  tvStorages.Hint := SCntvStoragesHint;

  Self.StringGrid.Cells[1, 0] := SCnStringGridCells10;
  Self.StringGrid.Cells[2, 0] := SCnStringGridCells20;
  Self.StringGrid.Cells[3, 0] := SCnStringGridCells30;

  FFormFilterOptions := [];
end;

procedure TFrmTransEditor.StringGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  OutStr: string;
begin
  if (gdFixed in State)then
    with Sender as TStringGrid do
    begin
      OutStr := Cells[ACol, ARow];
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(Rect);
      Canvas.TextOut(Rect.Left + ((Rect.Right - Rect.Left -
        Canvas.TextWidth(OutStr)) shr 1), Rect.Top + ((Rect.Bottom - Rect.top
        - Canvas.TextHeight(OutStr)) shr 1), OutStr);
    end;

  if ACol = 0 then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Rect);
    Canvas.TextOut(Rect.Left + 3, Rect.Top + 2, InttoStr(ARow));
  end;
end;

procedure TFrmTransEditor.actGenStrsExecute(Sender: TObject);
var
  List: TStringList;
  Item: TCnLanguageItem;
begin
  if (tvStorages.Selected = nil) or (tvStorages.Selected.Level <> 1) then
    Exit;

  if Self.Container = nil then
    if (Self.TransEditor = nil) then
      Exit
    else
      Self.Container := TWinControl(TCnLangTranslator(Self.TransEditor.Component).Owner);

  List := TStringList.Create;
  Item := TCnLanguageItem(tvStorages.Selected.Data);

  TransEditor.Extractor.SetFilterOptions(FFormFilterOptions);
  Self.TransEditor.Extractor.GetFormStrings(Self.Container, List);

  WriteNameValueStringsToGrid(List, Item);
end;

procedure TFrmTransEditor.StringGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if ACol in [1, 3] then
    Self.StringGrid.Options := Self.StringGrid.Options + [goEditing]
  else
    Self.StringGrid.Options := Self.StringGrid.Options - [goEditing];
end;

procedure TFrmTransEditor.actCopyStrsExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 1 to Self.StringGrid.RowCount do
    if Self.StringGrid.Cells[3, i] = '' then
      Self.StringGrid.Cells[3, i] := Self.StringGrid.Cells[2, i];
end;

procedure TFrmTransEditor.actUpdateStrsExecute(Sender: TObject);
var
  List: TStringList;
  DestList: array[1..3] of TStringList;
  i, Index: Integer;
  Item: TCnLanguageItem;
begin
  { DONE : ��������ִ��б������Ѿ��з���ģ���Ӧ�ƶ������ִ���������ӡ� }
  if (tvStorages.Selected = nil) or (tvStorages.Selected.Level <> 1) then
    Exit;

  Item := TCnLanguageItem(Self.tvStorages.Selected.Data);

  if Self.Container = nil then
    if (Self.TransEditor = nil) then
      Exit
    else
      Self.Container := TWinControl(TCnLangTranslator(Self.TransEditor.Component).Owner);

  List := nil;
  for i := Low(DestList) to High(DestList) do
    DestList[i] := nil;
  TransEditor.Extractor.SetFilterOptions(FFormFilterOptions);

  try
    List := TStringList.Create;
    Self.TransEditor.Extractor.GetFormStrings(Self.Container, List);

    for i := Low(DestList) to High(DestList) do
      DestList[i] := TStringList.Create;

    for i := 1 to Self.StringGrid.RowCount - 1 do
    begin
      if Self.StringGrid.Cells[1, i] <> '' then
      begin
        DestList[1].Add(Self.StringGrid.Cells[1, i]);
        DestList[2].Add(Self.StringGrid.Cells[2, i]);
        DestList[3].Add(Self.StringGrid.Cells[3, i]);
      end;
    end;

    Index := DestList[1].IndexOf(SystemNamePrefix + SCnLanguageID);
    if Index > 0 then DestList[3][Index] := InttoStr(Item.LanguageID);
    Index := DestList[1].IndexOf(SystemNamePrefix + SCnLanguageName);
    if Index > 0 then DestList[3][Index] := Item.LanguageName;
    Index := DestList[1].IndexOf(SystemNamePrefix + SCnAuthor);
    if Index > 0 then DestList[3][Index] := Item.Author;
    Index := DestList[1].IndexOf(SystemNamePrefix + SCnAuthorEmail);
    if Index > 0 then DestList[3][Index] := Item.AuthorEmail;
    Index := DestList[1].IndexOf(SystemNamePrefix + SCnDefaultFont);
    if Index > 0 then DestList[3][Index] := Item.DefaultFontStr;

    for i := 0 to List.Count - 1 do
    begin
      Index := DestList[1].IndexOf(List.Names[i]);
      if Index > 0 then
      begin
        DestList[2][Index] := List.Values[List.Names[i]];
      end
      else
      begin
        DestList[1].Add(List.Names[i]);
        DestList[2].Add(List.Values[List.Names[i]]);
        DestList[3].Add('');
      end;
    end;

    Self.StringGrid.RowCount := DestList[1].Count + 1;
    for i := 0 to DestList[1].Count - 1 do
    begin
      Self.StringGrid.Cells[0, i + 1] := InttoStr(i + 1);
      Self.StringGrid.Cells[1, i + 1] := DestList[1][i];
      Self.StringGrid.Cells[2, i + 1] := DestList[2][i];
      Self.StringGrid.Cells[3, i + 1] := DestList[3][i];
    end;
  finally
    for i := Low(DestList) to High(DestList) do
      DestList[i].Free;
    List.Free;
  end;
end;

procedure TFrmTransEditor.actAddLineExecute(Sender: TObject);
var
  R: TGridRect;
begin
  if (tvStorages.Selected = nil) or (tvStorages.Selected.Level <> 1) then
    Exit;
    
  with Self.StringGrid do
  begin
    RowCount := RowCount + 1;
    Rows[RowCount - 1].Text := '';
    Cells[0, RowCount - 1] := InttoStr(RowCount - 1);
    R.Left := 1; R.Top := RowCount - 1;
    R.Right := 1; R.Bottom := RowCount - 1;
    Selection := R;
    SetFocus;
  end;
end;

procedure TFrmTransEditor.actSaveCurItemExecute(Sender: TObject);
var
  Storage: TCnCustomLangStorage;
  i: Integer;
begin
  if (tvStorages.Selected = nil) or (tvStorages.Selected.Level <> 1) then
    Exit;

  Storage := TCnCustomLangStorage(tvStorages.Selected.Parent.Data);
  if Storage is TCnCustomLangFileStorage then
    SetDesignPathFile(Storage as TCnCustomLangFileStorage);

  Storage.ClearCurrentLanguage;
  for i := 1 to Self.StringGrid.RowCount - 1 do
    if StringGrid.Cells[1, i] <> '' then
      Storage.SetString(StringGrid.Cells[1, i], StringGrid.Cells[3, i]);
  Storage.SaveCurrentLanguage;
end;

procedure TFrmTransEditor.SetBasicInfo(List: TStrings; Item: TCnLanguageItem);
begin
  if (List <> nil) and (Item <> nil) then
  begin
    List.Values[SystemNamePrefix + SCnLanguageID] := InttoStr(Item.LanguageID);
    List.Values[SystemNamePrefix + SCnLanguageName] := Item.LanguageName;
    List.Values[SystemNamePrefix + SCnAuthor] := Item.Author;
    List.Values[SystemNamePrefix + SCnAuthorEmail] := Item.AuthorEmail;
  end;
end;

procedure TFrmTransEditor.tvStoragesCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Node.Level = 0 then  // ����������Ĵ洢����ô�����ʾ
  begin
    if CnLanguageManager <> nil then
      if CnLanguageManager.LanguageStorage = TCnCustomLangStorage(Node.Data) then
        Sender.Canvas.Font.Style := [fsBold];
  end
  else if Node.Level = 1 then // ����������ĵ�ǰ������ĿҲ�ô�����ʾ
  begin
    if CnLanguageManager <> nil then
      if CnLanguageManager.LanguageStorage = TCnCustomLangStorage(Node.Parent.Data) then
        if CnLanguageManager.CurrentLanguageIndex = Node.Parent.IndexOf(Node) then
          Sender.Canvas.Font.Style := [fsBold];
  end;

  DefaultDraw := True;
end;

procedure TFrmTransEditor.actDelLineExecute(Sender: TObject);
var
  i: Integer;
begin
  { DONE : ɾ��һ�С� }
  with Self.StringGrid do
    if RowCount > 2 then
    begin
      for i := Row to RowCount - 2 do
      begin
        Rows[i].Text := Rows[i + 1].Text;
        Cells[0, i] := InttoStr(i);
      end;
      RowCount := RowCount - 1;
    end
    else if RowCount = 2 then
      Rows[1].Text := '1';
end;

procedure TFrmTransEditor.pnlTransResize(Sender: TObject);
begin
  { DONE : ����Resize�¼����ı�Grid�п� }
  Self.StringGrid.ColWidths[1] := (Self.StringGrid.Width - 30 - 24) * 7 div 27 + 1;
  Self.StringGrid.ColWidths[2] := (Self.StringGrid.Width - 30 - 24) * 10 div 27;
  Self.StringGrid.ColWidths[3] := (Self.StringGrid.Width - 30 - 24) * 10 div 27;
end;

procedure TFrmTransEditor.actClearExecute(Sender: TObject);
begin
  if (tvStorages.Selected = nil) or (tvStorages.Selected.Level <> 1) then
    Exit;

  Self.StringGrid.RowCount := 2;
  Self.StringGrid.Rows[1].Text := '';
  Self.StringGrid.Cells[0, 1] := '1';
end;

procedure TFrmTransEditor.actCollectFormExecute(Sender: TObject);
var
  List: TStringList;
  Item: TCnLanguageItem;
  i: Integer;
  Project: IOTAProject;
  ModuleInfo: IOTAModuleInfo;
  FormEditor: IOTAFormEditor;
  AModule: IOTAModule;
  Opened: Boolean;

  function CnOtaGetProjectGroup: IOTAProjectGroup;
  var
    IModuleServices: IOTAModuleServices;
    IModule: IOTAModule;
    i: Integer;
  begin
    Result := nil;
    Supports(BorlandIDEServices, IOTAModuleServices, IModuleServices);
    if IModuleServices <> nil then
      for i := 0 to IModuleServices.ModuleCount - 1 do
      begin
        IModule := IModuleServices.Modules[i];
        if Supports(IModule, IOTAProjectGroup, Result) then
          Break;
      end;
  end;

  function CnOtaGetCurrentProject: IOTAProject;
  var
    IProjectGroup: IOTAProjectGroup;
  begin
    Result := nil;

    IProjectGroup := CnOtaGetProjectGroup;
    if not Assigned(IProjectGroup) then
      Exit;

    try
      // This raises exceptions in D5 with .bat projects active
      Result := IProjectGroup.ActiveProject;
    except
      Result := nil;
    end;
  end;

  function ExtractUpperFileExt(const FileName: string): string;
  begin
    Result := UpperCase(_CnExtractFileExt(FileName));
  end;

  function IsCpp(const FileName: string): Boolean;
  var
    FileExt: string;
  begin
    FileExt := ExtractUpperFileExt(FileName);
    Result := (FileExt = '.CPP');
  end;

  function CnOtaGetFileEditorForModule(Module: IOTAModule; Index: Integer): IOTAEditor;
  begin
    Result := nil;
    if not Assigned(Module) then Exit;
    try
      // BCB 5 ��Ϊһ���򵥵ĵ�Ԫ���� GetModuleFileEditor(1) �����
      {$IFDEF BCB5}
      if IsCpp(Module.FileName) and (Module.GetModuleFileCount = 2) and (Index = 1) then
        Index := 2;
      {$ENDIF}
      Result := Module.GetModuleFileEditor(Index);
    except
      Result := nil; // �� IDE �ͷ�ʱ�����ܻ����쳣����
    end;
  end;

  function CnOtaGetFormEditorFromModule(const Module: IOTAModule): IOTAFormEditor;
  var
    i: Integer;
    Editor: IOTAEditor;
    FormEditor: IOTAFormEditor;
  begin
    Result := nil;
    if not Assigned(Module) then
      Exit;
    for i := 0 to Module.GetModuleFileCount - 1 do
    begin
      Editor := CnOtaGetFileEditorForModule(Module, i);
      if Supports(Editor, IOTAFormEditor, FormEditor) then
      begin
        Result := FormEditor;
        Exit;
      end;
    end;
  end;

  function CnOtaGetRootComponentFromEditor(Editor: IOTAFormEditor): TComponent;
  var
    Component: IOTAComponent;
    NTAComponent: INTAComponent;
  begin
    Result := nil;
    if not Assigned(Editor) then Exit;

    try
      Component := Editor.GetRootComponent;
    except
      // ��ĳЩ�ļ�ʱ�����Delphi5��
      Exit;
    end;
  
    if Assigned(Component) and Supports(Component, INTAComponent,
      NTAComponent) then
    begin
      Result := NTAComponent.GetComponent;
      Exit;
    end;
  end;

begin
  if (tvStorages.Selected = nil) or (tvStorages.Selected.Level <> 1) then
    Exit;

  Item := TCnLanguageItem(tvStorages.Selected.Data);
  List := TStringList.Create;

  Project := CnOtaGetCurrentProject;
  if Project = nil then
    Exit;

  Screen.Cursor := crHourGlass;
  try
    for i := 0 to Project.GetModuleCount - 1 do
    begin
      ModuleInfo := Project.GetModule(i);
      Opened := CnOtaIsFileOpen(ModuleInfo.FileName);
      if ModuleInfo.FormName = '' then Continue;
      try
        AModule := ModuleInfo.OpenModule;
        FormEditor := CnOtaGetFormEditorFromModule(AModule);
      except
        FormEditor := nil;
        AModule := nil;
      end;

      if FormEditor <> nil then
      begin
        TransEditor.Extractor.SetFilterOptions(FFormFilterOptions);
        Self.TransEditor.Extractor.GetFormStrings(CnOtaGetRootComponentFromEditor(FormEditor), List);

        if not Opened and (AModule <> nil) then
        begin
          try
            AModule.CloseModule(True);
          except
            ;
          end;
          AModule := nil;
        end;
      end;
    end;

    Application.ProcessMessages;
    WriteNameValueStringsToGrid(List, Item);
  finally
    Screen.Cursor := crDefault;
  end;
end;


procedure TFrmTransEditor.WriteNameValueStringsToGrid(List: TStrings;
  Item: TCnLanguageItem);
var
  i, EquPos: Integer;
  S: string;
begin
  if (List = nil) or (Item = nil) then
    Exit;

  Self.StringGrid.RowCount := List.Count + 6;
  Self.StringGrid.Cells[1, 1] := SystemNamePrefix + SCnLanguageID;
  Self.StringGrid.Cells[3, 1] := InttoStr(Item.LanguageID);
  Self.StringGrid.Cells[1, 2] := SystemNamePrefix + SCnLanguageName;
  Self.StringGrid.Cells[3, 2] := Item.LanguageName;
  Self.StringGrid.Cells[1, 3] := SystemNamePrefix + SCnAuthor;
  Self.StringGrid.Cells[3, 3] := Item.Author;
  Self.StringGrid.Cells[1, 4] := SystemNamePrefix + SCnAuthorEmail;
  Self.StringGrid.Cells[3, 4] := Item.AuthorEmail;
  Self.StringGrid.Cells[1, 5] := SystemNamePrefix + SCnDefaultFont;
  Self.StringGrid.Cells[3, 5] := Item.DefaultFontStr;

  for i := 1 to 5 do
  begin
    Self.StringGrid.Cells[0, i] := InttoStr(i);
    Self.StringGrid.Cells[2, i] := '';
  end;

  for i := 0 to List.Count - 1 do
  begin
    Self.StringGrid.Cells[0, i + 6] := InttoStr(i + 6);
    S := List[I];
    EquPos := Pos('=', S);
    if EquPos > 0 then
    begin
      Self.StringGrid.Cells[1, i + 6] := Copy(S, 1, EquPos - 1);
      Self.StringGrid.Cells[2, i + 6] := Copy(S, EquPos + 1, MaxInt);
    end
    else // �޵Ⱥ�
    begin
      Self.StringGrid.Cells[1, i + 6] := S;
      Self.StringGrid.Cells[2, i + 6] := '';
    end;
    Self.StringGrid.Cells[3, i + 6] := '';
  end;
  Self.StringGrid.Cells[0, 0] := '';
end;

procedure TFrmTransEditor.actCloseExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TFrmTransEditor.actDelBlankExecute(Sender: TObject);
var
  I, BlankRow: Integer;

  function IsBlankText(AText: string): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 1 to Length(AText) do
      if not CharInSet(AText[I], [' '..'@', '['..'_', '{'..'~']) then
      begin
        Result := False;
        Exit;
      end;   
  end;  
begin
  Screen.Cursor := crHourGlass;
  BlankRow := -1; // BlankRow ʼ�ռ�¼��һ�����л���Ч��
  try
    for I := 5 to Self.StringGrid.RowCount - 1 do
    begin
      if (StringGrid.Cells[1, I][1] <> '!') and IsBlankText(StringGrid.Cells[2, I])
        and IsBlankText(StringGrid.Cells[3, I]) then // �����������
      begin
        if BlankRow = -1 then // ����ǰ�޿��У�
          BlankRow := I;      // ���¼�˿���λ��
      end
      else // �����ǰ�в���
      begin
        if BlankRow >= 0 then // ��ǰ�п���
        begin
          // ��ǰ�������ƶ�������ȥ
          StringGrid.Cells[0, BlankRow] := IntToStr(BlankRow);
          StringGrid.Cells[1, BlankRow] := StringGrid.Cells[1, I];
          StringGrid.Cells[2, BlankRow] := StringGrid.Cells[2, I];
          StringGrid.Cells[3, BlankRow] := StringGrid.Cells[3, I];
          // BlankRow ָ����һ���У����ܷǿյ���������
          Inc(BlankRow);
        end;
      end;
    end;
    if BlankRow >= 0 then
      Self.StringGrid.RowCount := BlankRow;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFrmTransEditor.actFilterExecute(Sender: TObject);
begin
  with TFrmTransFilter.Create(Self) do
  begin
    try
      Open(FFormFilterOptions);
    finally
      Free;
    end;
  end;
end;

end.

