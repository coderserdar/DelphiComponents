unit PlusKeys;
{ TPlusMemo.Keywords and StartStopKeys property editor
  This file must be accompanied by PlusKeys.dfm, which is the form
  resource file for these property editors (TDlgExtKeysEditor)

  For PlusMemoClx v6.3 }

{ © Electro-Concept Mauricie, 1997-2005 }

{$DEFINE PlusKeys}

{UCONVERT}
  {$IFDEF PlusKeysClx}
    {$DEFINE pmClx}
  {$ENDIF}
{/UCONVERT}

interface

{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D7New} {$ENDIF}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

uses
  {$IFDEF pmClx}
  QGraphics, QForms, QControls, QComCtrls, QTypes, QButtons, QStdCtrls, QGrids, QExtCtrls, QDialogs,
  QMenus, PlusMemoClx, PMSupportClx,
  
  {$ELSE}
  Windows, Graphics, Forms, Controls, Buttons,
  StdCtrls, Grids, ExtCtrls, ComCtrls, Dialogs, Menus, Messages,
  PlusMemo, PMSupport,
  {$ENDIF}
  Classes;
  
type
  TKeywordListClass = class of TKeywordList;
  TStartStopListClass = class of TStartStopKeyList;

  TDlgExtKeysEditor = class(TForm)
{UCONVERT}
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    gbTitle: TGroupBox;
    gbOptions: TGroupBox;
    ChkBMatchCase: TCheckBox;
    ChkBWholeWords: TCheckBox;
    ChkBHighlight: TCheckBox;
    ChkBAltFont: TCheckBox;
    ChkBItalic: TCheckBox;
    ChkBBold: TCheckBox;
    ChkBUnderline: TCheckBox;
    ChkBStrikeOut: TCheckBox;
    LblHint: TLabel;
    ColorDialog1: TColorDialog;
    ShpForegnd: TShape;
    BtnForegnd: TButton;
    BtnBackgnd: TButton;
    ShpBackgnd: TShape;
    Label1: TLabel;
    EdContext: TEdit;
    BtnDelete: TButton;
    LblMouse: TLabel;
    cbCursors: TComboBox;
    ChkBCrossPar: TCheckBox;
    lvKeys: TListView;
    btnClearForeground: TButton;
    btnClearBackground: TButton;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuLoadList: TMenuItem;
    mnuLoadListDefText: TMenuItem;
    mnuLoadListDefBin: TMenuItem;
    N1: TMenuItem;
    mnuSaveList: TMenuItem;
    mnuSaveListDefText: TMenuItem;
    mnuSaveListDefBin: TMenuItem;
    OpenDialog1: TOpenDialog;
    btnAdd: TButton;
    btnInsert: TButton;
    SaveDialog1: TSaveDialog;
    lblSelCount: TLabel;
    panExtended: TPanel;
    Label2: TLabel;
    EdScope: TEdit;
    EdPriority: TEdit;
    Label3: TLabel;
    btnHelp: TBitBtn;
    rgPosInfo: TRadioGroup;
    cbEndAtDel: TCheckBox;
    cbCollapsible: TCheckBox;
    btnMoveUp: TSpeedButton;
    btnMoveDown: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure ChkBxClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnForegndClick(Sender: TObject);
    procedure BtnResetColorsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbCursorsChange(Sender: TObject);
    procedure EdContextExit(Sender: TObject);
    procedure lvKeysEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure lvKeysEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure lvKeysKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvKeysEnter(Sender: TObject);
    procedure lvKeysExit(Sender: TObject);
    procedure mnuLoadListClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure mnuLoadListDefClick(Sender: TObject);
    procedure mnuSaveListClick(Sender: TObject);
    procedure mnuSaveListDefClick(Sender: TObject);
    procedure lvKeysChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvKeysEdited2(Sender: TObject; Item: TListItem;
      var S: WideString);
    procedure lvKeysSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnHelpClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure rgPosInfoClick(Sender: TObject);
    procedure btnMoveClick(Sender: TObject);
  private
{/UCONVERT}
    { Private declarations }
    fSavedKeyword: string;
    fSelectionProcessed: Boolean;
    inSelect: Boolean;
    Modified: Boolean;
    woList: TList;
    fKeywordClass: TKeywordListClass;      // used to create the correct class (normal or extended) for ini file handling
    fStartStopClass: TStartStopListClass;  // used to create the correct class (normal or extended) for ini file handling
    procedure AddCursor(const s: AnsiString);
    procedure ProcessSelection;
    function GetNextSelItem(Item: TListItem): TListItem;
    procedure ExchangeItems(Index1, Index2: Integer);
  public
    {$IFNDEF pmClx} flvWindowProc: TWndMethod; {$ENDIF}
    procedure lvWindowProc(var Msg: TMessage);  // process selection only after all activity is finished
  end;

var
  DlgExtKeysEditor: TDlgExtKeysEditor;

function EditKeywordList(KeywordList: TKeywordList; Extended, AllowKeywordChanges: Boolean): Boolean;
function EditStartStopList(StartStopList: TStartStopKeyList; Extended: Boolean): Boolean;

implementation
uses SysUtils, IniFiles {$IFNDEF pmClx}, CommCtrl {$ENDIF};
{$R *.DFM}

const TextFilter =  'Ini files (*.ini)|*.ini|Text files (*.txt)|*.txt|All files|*.*';
      BinFilter  =  'Binary files (*.kbn)|*.kbn|All files|*.*';
      BinKeywordSig = 'Extended keywords';
      BinSSSig = 'Extended start-stop keys';

type
  TEditKeyInfo = record
    Attributes: TKeywordInfo;
    ssOpt: TssOptions;
    Scope, Priority: Integer
    end;
  pEditKeyInfo = ^TEditKeyInfo;


function EditKeywordList(KeywordList: TKeywordList; Extended, AllowKeywordChanges: Boolean): Boolean;
var i, j: Integer;
    pek: pEditKeyInfo;
    pkl: pKeyInfoLen;
    kw: string;
    li: TListItem;
    sdlg: TDlgExtKeysEditor;
begin
  sdlg:= TDlgExtKeysEditor.Create(Application);
  sdlg.woList:= TList.Create;
  sdlg.panExtended.Visible:= Extended;
  sdlg.mnuLoadList.Visible:= AllowKeywordChanges;
  sdlg.mnuLoadListDefText.Visible:= AllowKeywordChanges;
  sdlg.mnuLoadListDefBin.Visible:= AllowKeywordChanges;
  if Extended then sdlg.Caption:= 'Extended ' + sdlg.Caption
  else
    begin
      sdlg.gbOptions.Height:= 153;
      sdlg.ClientHeight:= sdlg.gbOptions.Top + sdlg.gbOptions.Height + 4
    end;
  sdlg.fKeywordClass:= TKeywordListClass(KeywordList.ClassType);

  sdlg.lvKeys.Items.BeginUpdate;
  for i:= 0 to KeywordList.Count-1 do
    begin
      New(pek);
      pkl:= KeywordList.KeyList[i];
      pek^.Attributes:= pkl.BasicPart;
      pek^.ssOpt:= [];
      pek.Scope:= pkl.Scope;
      pek.Priority:= pkl.Priority;
      sdlg.woList.Add(pek);
      li:= sdlg.lvKeys.Items.Add;
      li.Caption:= KeywordList[i]
    end;
  sdlg.lvKeys.Items.EndUpdate;

  if not AllowKeywordChanges then
    begin
      sdlg.btnInsert.Enabled:= False;
      sdlg.btnAdd.Enabled:= False;
      sdlg.BtnDelete.Enabled:= False;
      sdlg.LblHint.Visible:= False;
      sdlg.ChkBMatchCase.Enabled:= False;
      sdlg.ChkBWholeWords.Enabled:= False;
      sdlg.EdContext.Enabled:= False;
      sdlg.EdScope.Enabled:= False;
      sdlg.EdPriority.Enabled:= False;
      sdlg.rgPosInfo.Enabled:= False;
      sdlg.lvKeys.ReadOnly:= True
    end;

  if (sdlg.ShowModal=mrOk) and sdlg.Modified then
    begin
      KeywordList.Clear;
      KeywordList.BeginUpdate;
      for i:= 0 to sdlg.lvKeys.Items.Count-1 do
        begin
          kw:= sdlg.lvKeys.Items[i].Caption;
          pek:= sdlg.woList[i];
          j:= KeywordList.AddKeyWord(kw, pek.Attributes.Options, pek.Attributes.Style, pek.Attributes.ContextNumber,
                                         pek.Attributes.Cursor, pek.Attributes.Backgnd, pek.Attributes.Foregnd);
          pkl:= KeywordList.KeyList[j];
          pkl.Scope:= pek.Scope;
          pkl.Priority:= pek.Priority;
          Dispose(pek)
        end;
      KeywordList.EndUpdate;
      Result:= True
    end
  else
    begin
      for i:= 0 to sdlg.woList.Count-1 do Dispose(pEditKeyInfo(sdlg.woList[i]));
      Result:= False
    end;
  sdlg.woList.Free;
  sdlg.Free
end;

function EditStartStopList(StartStopList: TStartStopKeyList; Extended: Boolean): Boolean;
var i: Integer;
    pek: pEditKeyInfo;
    ssinfo: pStartStopInfo;
    startkey: string;
    li: TListItem;
    sdlg: TDlgExtKeysEditor;
begin
  sdlg:= TDlgExtKeysEditor.Create(Application);
  sdlg.panExtended.Visible:= Extended;
  sdlg.mnuFile.Visible:= Extended;
  sdlg.fStartStopClass:= TStartStopListClass(StartStopList.ClassType);

  with sdlg do
    begin
      Caption:= 'Start-Stop keys Editor';
      if Extended then Caption:= 'Extended ' + Caption
      else
        begin
          gbOptions.Height:= 153;
          ClientHeight:= gbOptions.Top + gbOptions.Height + 4
        end;
      gbTitle.Caption:= '';
      lvKeys.ViewStyle:= vsReport;
      gbOptions.Caption:= 'Options for selected start-stop keys';
      ChkBCrossPar.Visible:= True;
      cbCollapsible.Visible:= True;
      cbEndAtDel.Visible:= True;
      woList:= TList.Create;
    end;

  for i:= 0 to StartStopList.Count-1 do
    begin
      New(pek);
      ssinfo:= StartStopList.Pointers[i];
      pek^.Attributes:= ssinfo^.Attributes;
      pek^.Scope:= ssinfo^.Scope;
      pek^.ssOpt:= ssinfo^.ssOptions;
      pek^.Priority:= ssinfo^.Priority;
      sdlg.woList.Add(pek);
      li:= sdlg.lvKeys.Items.Add;
      li.SubItems.Add(ssinfo^.StartKeyStr);
      li.SubItems.Add(ssinfo^.StopKeyStr);
    end;

  if (sdlg.ShowModal=mrOk) and sdlg.Modified then
    begin
      StartStopList.Clear;
      for i:= 0 to sdlg.lvKeys.Items.Count-1 do
        begin
          pek:= sdlg.woList[i];
          startkey:= sdlg.lvKeys.Items[i].SubItems[0];
          if startkey<>'' then
            begin
              StartStopList.AddStartStopKey(startkey, sdlg.lvKeys.Items[i].SubItems[1],
                              pek^.Attributes.Options, pek^.Attributes.Style, pek^.Attributes.ContextNumber,
                              pek^.Attributes.Cursor, pek^.Attributes.Backgnd, pek^.Attributes.Foregnd,
                              ssoParStop in pek^.ssOpt);
              ssinfo:= StartStopList.Pointers[StartStopList.Count-1];
              ssinfo.Priority:= pek^.Priority;
              ssinfo.ssOptions:= pek^.ssOpt;
              ssinfo.Scope:= pek^.Scope;
            end;
          Dispose(pek)
        end;
      Result:= True
    end
  else
    begin
      for i:= 0 to sdlg.woList.Count-1 do Dispose(pEditKeyInfo(sdlg.woList[i]));
      Result:= False
    end;

  sdlg.woList.Free;
  sdlg.Free
end;


procedure TDlgExtKeysEditor.FormShow(Sender: TObject);
begin
  if lvKeys.Items.Count>0 then lvKeys.Items[0].Selected:= True
  else
    begin
      gbOptions.Enabled:= False;
      gbOptions.Font.Color:= clGray
    end
end;

procedure TDlgExtKeysEditor.ChkBxClick(Sender: TObject);
  procedure setWOptions(cb: TCheckBox; var Opt: TWordOptions; Item: TWordOption);
    begin
      if cb.State<>cbGrayed then
        if cb.Checked then Include(Opt, Item)
                      else Exclude(Opt, Item)
    end;
  procedure setSSOptions(cb: TCheckBox; var Opt: TssOptions; Item: TssOption; Invert: Boolean);
    var sr: Boolean;
    begin
      if cb.State<>cbGrayed then
        begin
          sr:= cb.Checked;
          if Invert then sr:= not sr;
          if sr then Include(Opt, Item)
             else Exclude(Opt, Item)
        end
    end;
  procedure setStyle(cb: TCheckBox; var Style: TFontStyles; Item: TFontStyle);
    begin
      if cb.State<>cbGrayed then
        if cb.Checked then Include(Style, Item)
                      else Exclude(Style, Item)
    end;
var sitem: TListItem; skey: pEditKeyInfo;
begin
  if inSelect then Exit;
  Modified:= True;
  sitem:= lvKeys.Items[0];
  {$IFDEF pmClx}
    inSelect:= True;
    if TCheckBox(Sender).State=cbGrayed then
      if TCheckBox(Sender).Checked then TCheckBox(Sender).State:= cbChecked
                                   else TCheckBox(Sender).State:= cbUnChecked;
    inSelect:= False;
  {$ENDIF}
  if not sitem.Selected then sitem:= GetNextSelItem(sitem);
  while sitem<>nil do
    begin
      skey:= woList[sitem.Index];
      setWOptions(ChkBMatchCase, skey.Attributes.Options, woMatchCase);
      setWOptions(ChkBWholeWords, skey.Attributes.Options, woWholeWordsOnly);
      setSSOptions(ChkBCrossPar, skey.ssOpt, ssoParStop, True);
      setSSOptions(cbCollapsible, skey.ssOpt, ssoCollapsible, False);
      setSSOptions(cbEndAtDel, skey.ssOpt, ssoDelStop, False);
      setStyle(ChkBBold, skey.Attributes.Style, fsBold);
      setStyle(ChkBItalic, skey.Attributes.Style, fsItalic);
      setStyle(ChkBUnderline, skey.Attributes.Style, fsUnderline);
      setStyle(ChkBStrikeOut, skey.Attributes.Style, fsStrikeOut);
      setStyle(ChkBHighlight, skey.Attributes.Style, TFontStyle(fsHighlight));
      setStyle(ChkBAltFont, skey.Attributes.Style, TFontStyle(fsAltFont));
      sitem:= GetNextSelItem(sitem)
    end;
end;

procedure TDlgExtKeysEditor.BtnDeleteClick(Sender: TObject);
var sitem: TListItem;
var pk: pEditKeyInfo;
begin
  lvKeys.Items.BeginUpdate;
  sitem:= lvKeys.Selected;
  while sitem<>nil do
    begin
      pk:= woList[sitem.Index];
      Dispose(pk);
      woList.Delete(sitem.Index);
      lvKeys.Items.Delete(sitem.Index);
      sitem:= lvKeys.Selected
    end;
  Modified:= True;
  btnDelete.Enabled:= False;
  lvKeys.Items.EndUpdate
end;

procedure TDlgExtKeysEditor.BtnForegndClick(Sender: TObject);
var sitem: TListItem; skey: pEditKeyInfo; sshape: TShape;
begin
  if Sender=btnForegnd then sshape:= ShpForegnd
                       else sshape:= ShpBackgnd;
  if sshape.Visible then ColorDialog1.Color:= sshape.Brush.Color
                    else ColorDialog1.Color:= clBlack;
  if ColorDialog1.Execute then
    begin
      Modified:= True;
      if Sender=btnForegnd then btnClearForeground.Show
                           else btnClearBackground.Show;
      sshape.Brush.Color:= ColorDialog1.Color;
      sshape.Brush.Style:= bsSolid;
      sshape.Show;
      sitem:= lvKeys.Items[0];
      if not sitem.Selected then sitem:= GetNextSelItem(sitem);
      while sitem<>nil do
        begin
          skey:= woList[sitem.Index];
          if Sender=btnForegnd then skey.Attributes.Foregnd:= ColorDialog1.Color
                               else skey.Attributes.Backgnd:= ColorDialog1.Color;
          sitem:= GetNextSelItem(sitem)
        end;
    end
end;

procedure TDlgExtKeysEditor.BtnResetColorsClick(Sender: TObject);
var sitem: TListItem; skey: pEditKeyInfo;
begin
  Modified:= True;
  if Sender=btnClearBackground then
    begin
      ShpBackgnd.Hide;
      btnClearBackground.Hide
    end
  else
    begin
      ShpForegnd.Hide;
      btnClearForeground.Hide
    end;

  sitem:= lvKeys.Items[0];
  if not sitem.Selected then sitem:= GetNextSelItem(sitem);
  while sitem<>nil do
    begin
      skey:= woList[sitem.Index];
      if Sender=btnClearForeground then skey.Attributes.Foregnd:= -1
                                   else skey.Attributes.Backgnd:= -1;
      sitem:= GetNextSelItem(sitem)
    end
end;

procedure TDlgExtKeysEditor.FormCreate(Sender: TObject);
begin
  GetCursorValues(AddCursor);
  {$IFNDEF pmClx}
  flvWindowProc:= lvKeys.WindowProc;
  lvKeys.WindowProc:= lvWindowProc;
  {$ENDIF}
end;

procedure TDlgExtKeysEditor.AddCursor(const s: AnsiString);
begin
  cbCursors.Items.Add(s)
end;

procedure TDlgExtKeysEditor.cbCursorsChange(Sender: TObject);
var scursor: TCursor; sitem: TListItem; skey: pEditKeyInfo;
begin
  if inSelect then Exit;
  Modified:= True;
  cbCursors.Font.Color:= clBlack;
  sitem:= lvKeys.Items[0];
  if not sitem.Selected then sitem:= GetNextSelItem(sitem);
  scursor:= StringToCursor(cbCursors.Items[cbCursors.ItemIndex]);
  LblMouse.Cursor:= scursor;
  while sitem<>nil do
    begin
      skey:= woList[sitem.Index];
      skey.Attributes.Cursor:= scursor;
      sitem:= GetNextSelItem(sitem)
    end;
end;

procedure TDlgExtKeysEditor.rgPosInfoClick(Sender: TObject);
  procedure SetWPOpt(var WOpt: TWordOptions; Value: Integer);
    begin
      WOpt:= WOpt * [woMatchCase, woWholeWordsOnly];
      case Value of
        1: Include(WOpt, woFirstParWord);
        2: Include(WOpt, woFirstNonBlank);
        3: Include(WOpt, woStartPar)
        end
    end;
var sitem: TListItem; skey: pEditKeyInfo;
begin
  if inSelect then Exit;
  Modified:= True;
  rgPosInfo.Font.Color:= clBlack;
  sitem:= lvKeys.Items[0];
  if not sitem.Selected then sitem:= GetNextSelItem(sitem);
  while sitem<>nil do
    begin
      skey:= woList[sitem.Index];
      SetWPOpt(skey.Attributes.Options, rgPosInfo.ItemIndex);
      sitem:= GetNextSelItem(sitem)
    end;
end;


procedure TDlgExtKeysEditor.EdContextExit(Sender: TObject);
var sedit: TEdit; newvalue: SmallInt; sitem: TListItem; skey: pEditKeyInfo;
begin
  sedit:= Sender as TEdit;
  if sedit.Modified then
    begin
      sedit.Font.Color:= clBlack;
      sitem:= lvKeys.Items[0];
      if not sitem.Selected then sitem:= GetNextSelItem(sitem);
      sedit.Modified:= False;
      Modified:= True;
      try
        while sitem<>nil do
          begin
            skey:= woList[sitem.Index];
            newvalue:= StrToInt(sedit.Text);
            if Sender= EdContext then skey.Attributes.ContextNumber:= newvalue
            else
              if Sender=EdScope then skey.Scope:= newvalue
                                else skey.Priority:= newvalue;
            sitem:= GetNextSelItem(sitem)
          end
      except
        on E: Exception do
          begin
            ShowMessage(E.Message);
            sedit.SetFocus
          end
        end
    end
end;

procedure TDlgExtKeysEditor.lvKeysEdited(Sender: TObject; Item: TListItem; var S: {UCONVERT} String {/UCONVERT});
begin
  if S<>fSavedKeyword then Modified:= True;
  lblHint.Show
end;

procedure TDlgExtKeysEditor.lvKeysEditing(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
var s: AnsiString;
begin
  fSavedKeyword:= Item.Caption;
  if lvKeys.ViewStyle=vsReport then
    begin
      s:= Item.SubItems[0];
      InputQuery('Start-Stop key Editor', 'Enter the new start key', s);
      if s<>Item.SubItems[0] then
        begin
          Modified:= True;
          Item.SubItems[0]:= s
        end;
      s:= Item.SubItems[1];
      InputQuery('Start-Stop key Editor', 'Enter the new stop key', s);
      if s<>Item.SubItems[1] then
        begin
          Modified:= True;
          Item.SubItems[1]:= s
        end;
      AllowEdit:= False
    end
end;

procedure TDlgExtKeysEditor.lvKeysKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  {$IFNDEF pmClx}
  if (Key=VK_F2) and (lvKeys.ItemFocused<>nil) then PostMessage(lvKeys.Handle, LVM_EDITLABEL, lvKeys.ItemFocused.Index, 0)
  {$ENDIF}
end;

procedure TDlgExtKeysEditor.lvKeysChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  fSelectionProcessed:= False
end;


procedure TDlgExtKeysEditor.ProcessSelection;
  function WOptToPosSet(WOpt: TWordOptions): Integer;
    begin
      if woStartPar in WOpt then Result:= 3
      else
        if woFirstNonBlank in WOpt then Result:= 2
        else
          if woFirstParWord in WOpt then Result:= 1
          else Result:= 0
    end;

var skey, srunkey: pEditKeyInfo; Item: TListItem;
begin
  fSelectionProcessed:= True;
  if lvKeys.Items.Count=0 then lblSelCount.Caption:= ''
                          else lblSelCount.Caption:= IntToStr(lvKeys.SelCount) + ' of ' + IntToStr(lvKeys.Items.Count) + ' selected';
  if lvKeys.SelCount=0 then
    begin
      gbOptions.Enabled:= False;
      gbOptions.Font.Color:= clGray;
      btnDelete.Enabled:= False;
      lblHint.Visible:= False;
      Exit
    end;

  if not lvKeys.ReadOnly then lblHint.Visible:= lvKeys.Focused;
  gbOptions.Enabled:= True;
  if not lvKeys.ReadOnly then btnDelete.Enabled:= True;
  gbOptions.Font.Color:= clBlack;

  inSelect:= True;   // avoid OnClick updating info

  { Set UI elements according to first selected item }
  //Item:= lvKeys.Selected;   // does not work in Clx
  Item:= lvKeys.ItemFocused;

  if (Item=nil) or (not Item.Selected) then
    begin
      Item:= lvKeys.Items[0];
      if not Item.Selected then Item:= GetNextSelItem(Item)
    end;
  skey:= woList[Item.Index];
  ChkBMatchCase.Checked:= woMatchCase in skey.Attributes.Options;
  ChkBWholeWords.Checked:= woWholeWordsOnly in skey.Attributes.Options;
  ChkBBold.Checked:= fsBold in skey.Attributes.Style;
  ChkBItalic.Checked:= fsItalic in skey.Attributes.Style;
  ChkBUnderline.Checked:= fsUnderline in skey.Attributes.Style;
  ChkBStrikeOut.Checked:= fsStrikeOut in skey.Attributes.Style;
  ChkBHighlight.Checked:=  Byte(skey.Attributes.Style) and (1 shl Ord(fsHighlight)) <> 0;
  ChkBAltFont.Checked:= Byte(skey.Attributes.Style) and (1 shl Ord(fsAltFont)) <> 0;
  ChkBCrossPar.Checked:= not (ssoParStop in skey.ssOpt);
  cbCollapsible.Checked:= ssoCollapsible in skey.ssOpt;
  cbEndAtDel.Checked:= ssoDelStop in skey.ssOpt;
  rgPosInfo.ItemIndex:= WOptToPosSet(skey.Attributes.Options);
  rgPosInfo.Font.Color:= clBlack;
  ShpForegnd.Brush.Color:= skey.Attributes.Foregnd;
  ShpForegnd.Brush.Style:= bsSolid;
  ShpBackgnd.Brush.Color:= skey.Attributes.Backgnd;
  ShpBackgnd.Brush.Style:= bsSolid;
  if (skey.Attributes.Backgnd=0) and (skey.Attributes.Foregnd=0) then
    begin
      ShpForegnd.Hide;
      ShpBackgnd.Hide;
    end
  else
    begin
      ShpForegnd.Visible:= skey.Attributes.Foregnd<>-1;
      ShpBackgnd.Visible:= skey.Attributes.Backgnd<>-1;
    end;
  EdContext.Text:= IntToStr(skey.Attributes.ContextNumber);
  EdContext.Font.Color:= clBlack;
  EdScope.Text  := IntToStr(skey.Scope);
  EdScope.Font.Color:= clBlack;
  EdPriority.Text:= IntToStr(skey.Priority);
  EdPriority.Font.Color:= clBlack;
  EdContext.Modified:= False;
  EdScope.Modified:= False;
  EdPriority.Modified:= False;
  LblMouse.Cursor:= skey.Attributes.Cursor;
  cbCursors.ItemIndex:= cbCursors.Items.IndexOf(CursorToString(skey.Attributes.Cursor));
  cbCursors.Font.Color:= clBlack;

  { run through selected items and gray UI elements for non consistent properties }
  if lvKeys.SelCount>1 then
    begin
      Item:= lvKeys.Items[0];
      if not Item.Selected then Item:= GetNextSelItem(Item);
      while Item<>nil do
        begin
          srunkey:= woList[Item.Index];
          if (woMatchCase in srunkey.Attributes.Options) <> ChkBMatchCase.Checked then ChkBMatchCase.State:= cbGrayed;
          if (woWholeWordsOnly in srunkey.Attributes.Options) <> ChkBWholeWords.Checked then ChkBWholeWords.State:= cbGrayed;
          if (ssoParStop in srunkey.ssOpt) <> ChkBCrossPar.Checked then ChkBCrossPar.State:= cbGrayed;
          if (fsBold in srunkey.Attributes.Style) <> ChkBBold.Checked then ChkBBold.State:= cbGrayed;
          if (fsItalic in srunkey.Attributes.Style) <> ChkBItalic.Checked then ChkBItalic.State:= cbGrayed;
          if (fsUnderline in srunkey.Attributes.Style) <> ChkBUnderline.Checked then ChkBUnderline.State:= cbGrayed;
          if (fsStrikeOut in srunkey.Attributes.Style) <> ChkBStrikeOut.Checked then ChkBStrikeOut.State:= cbGrayed;
          if (Byte(srunkey.Attributes.Style) and (1 shl Ord(fsAltFont)) <> 0) <> ChkBAltFont.Checked then
              ChkBAltFont.State:= cbGrayed;
          if (Byte(srunkey.Attributes.Style) and (1 shl Ord(fsHighlight)) <> 0) <> ChkBHighlight.Checked then
              ChkBHighlight.State:= cbGrayed;
          if srunkey.Attributes.Foregnd<>ShpForegnd.Brush.Color then
            begin
              ShpForegnd.Show;
              ShpForegnd.Brush.Style:= bsDiagCross
            end;
          if srunkey.Attributes.Backgnd<>ShpBackgnd.Brush.Color then
            begin
              ShpBackgnd.Show;
              ShpBackgnd.Brush.Style:= bsDiagCross
            end;
          if srunkey.Attributes.ContextNumber<>skey.Attributes.ContextNumber then EdContext.Font.Color:= clGray;
          if srunkey.Scope<>skey.Scope then EdScope.Font.Color:= clGray;
          if srunkey.Priority<>skey.Priority then EdPriority.Font.Color:= clGray;
          if srunkey.Attributes.Cursor<>skey.Attributes.Cursor then cbCursors.Font.Color:= clGray;
          if WOptToPosSet(srunkey.Attributes.Options)<>rgPosInfo.ItemIndex then rgPosInfo.Font.Color:= clGray;
          Item:= GetNextSelItem(Item)
        end
    end;

  btnClearForeground.Visible:= ShpForegnd.Visible;
  btnClearBackground.Visible:= ShpBackgnd.Visible;
  inSelect:= False
end;

procedure TDlgExtKeysEditor.lvKeysEnter(Sender: TObject);
begin
  if not lvKeys.ReadOnly then lblHint.Visible:= lvKeys.Items.Count>0
end;

procedure TDlgExtKeysEditor.lvKeysExit(Sender: TObject);
begin
  lblHint.Hide
end;

procedure TDlgExtKeysEditor.mnuLoadListClick(Sender: TObject);
var skeys: TStringList; pk: pEditKeyInfo; li: TListItem; i, seppos: Integer; s: string;
begin
  OpenDialog1.Filter:= TextFilter;

  if OpenDialog1.Execute then
    begin
      skeys:= TStringList.Create;
      lvKeys.Items.BeginUpdate;
      try
        skeys.LoadFromFile(OpenDialog1.FileName);
        if skeys.Count>0 then Modified:= True;
        for i:= 0 to skeys.Count-1 do
          begin
            New(pk);
            FillChar(pk^, SizeOf(pk^), 0);
            pk^.Attributes.ContextNumber:= 1;
            pk^.Attributes.Foregnd:= -1;
            pk^.Attributes.Backgnd:= -1;
            woList.Add(pk);
            li:= lvKeys.Items.Add;
            if lvKeys.ViewStyle<>vsReport then li.Caption:= skeys[i]
            else
              begin
                s:= skeys[i];
                seppos:= Pos('|', s);
                if seppos<=0 then seppos:= Length(s);
                li.SubItems.Add(Copy(s, 1, seppos-1));
                li.SubItems.Add(Copy(s, seppos+1, Length(s)-seppos))
              end
          end
      finally
        lvKeys.Items.EndUpdate;
        skeys.Free
        end
    end
end;

procedure TDlgExtKeysEditor.lvWindowProc(var Msg: TMessage);
begin
  {$IFNDEF pmClx}
  if (Msg.Msg=WM_Paint) and not fSelectionProcessed then ProcessSelection;
  flvWindowProc(Msg)
  {$ENDIF}
end;

procedure TDlgExtKeysEditor.btnInsertClick(Sender: TObject);
var sindex: Integer; sitem: TListItem; spk: pEditKeyInfo; dummy: Boolean;
begin
  if Sender=btnInsert then
    begin
      sitem:= lvKeys.ItemFocused;
      if sitem<>nil then sindex:= sitem.Index
                    else sindex:= 0;
      if sindex<lvKeys.Items.Count then sitem:= lvKeys.Items.Insert(sindex)
                                   else sitem:= lvKeys.Items.Add
    end
  else
    begin
      sitem:= lvKeys.Items.Add;
      sindex:= sitem.Index
    end;
  New(spk);
  FillChar(spk^, SizeOf(spk^), 0);
  spk^.Attributes.ContextNumber:= 1;
  spk^.Attributes.Foregnd:= -1;
  spk^.Attributes.Backgnd:= -1;
  woList.Insert(sindex, spk);
  if lvKeys.ViewStyle=vsReport then
    begin
      sitem.SubItems.Add('');
      sitem.SubItems.Add('');
      lvKeysEditing(btnInsert, sitem, dummy)
    end;
  lvKeys.SetFocus;
  if lvKeys.ViewStyle<>vsReport then
    {$IFDEF pmClx}
      {$IFDEF D7New} sitem.EditText
      {$ELSE}
        begin
          for sindex:= 0 to lvKeys.Items.Count-1 do lvKeys.Items[sindex].Selected:= False;
          lvKeys.Selected:= sitem;
          lvKeys.Items[lvKeys.Selected.Index].Focused:= True
        end
      {$ENDIF}
    {$ELSE}
      sitem.EditCaption
    {$ENDIF}
end;

procedure TDlgExtKeysEditor.mnuLoadListDefClick(Sender: TObject);
var
  sklist: TKeywordList;
  ssslist: TStartStopKeyList;
  sini: TCustomIniFile;
  pek: pEditKeyInfo;
  pkl: pKeyInfoLen;
  ssk: pStartStopInfo;
  li: TListItem;
  i: Integer;
  sstream: TStream;
  sreader: TReader;
  ssig: string;
  sstrings: TStrings;
begin
  if Sender=mnuLoadListDefText then OpenDialog1.Filter:= TextFilter
                               else OpenDialog1.Filter:= BinFilter;

  if OpenDialog1.Execute then
    begin
      sklist:= nil;
      ssslist:= nil;
      sstream:= nil;
      sreader:= nil;
      sini:= nil;
      sstrings:= nil;
      lvKeys.Items.BeginUpdate;
      if lvKeys.ViewStyle<>vsReport then sklist:= fKeywordClass.Create
                                    else ssslist:= fStartStopClass.Create;
      try
        if Sender=mnuLoadListDefText then
          begin
            sini:= TMemIniFile.Create(OpenDialog1.FileName);
            sstrings:= TStringList.Create;
            if sklist<>nil then
              begin
                sini.ReadSectionValues('Keywords', sstrings);
                sklist.LoadFromIniStrings(sstrings)
              end
            else
              begin
                sini.ReadSectionValues('Sections', sstrings);
                ssslist.LoadFromIniStrings(sstrings)
              end;
          end
        else
          begin
            sstream:= TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
            sreader:= TReader.Create(sstream, 1024);
            ssig:= sreader.ReadString;
            if sklist<>nil then
              begin
                if ssig<>BinKeywordSig then raise Exception.Create('File does not contain valid keyword list');
                sklist.ReadData(sreader)
              end
            else
              begin
                if ssig<>BinSSSig then raise Exception.Create('File does not contain valid start-stop list');
                ssslist.ReadData(sreader)
              end
          end;

        if sklist<>nil then
          for i:= 0 to sklist.Count-1 do
            begin
              Modified:= True;
              New(pek);
              pkl:= sklist.KeyList[i];
              pek.Attributes:= pkl.BasicPart;
              pek.ssOpt:= [];
              pek.Scope:= pkl.Scope;
              pek.Priority:= pkl.Priority;
              woList.Add(pek);
              li:= lvKeys.Items.Add;
              li.Caption:= sklist[i]
            end
        else
          for i:= 0 to ssslist.Count-1 do
            begin
              Modified:= True;
              New(pek);
              ssk:= ssslist.Pointers[i];
              pek^.Attributes:= ssk^.Attributes;
              pek^.Scope:= ssk^.Scope;
              pek^.Priority:= ssk^.Priority;
              pek^.ssOpt:= ssk^.ssOptions;
              woList.Add(pek);
              li:= lvKeys.Items.Add;
              li.SubItems.Add(ssk^.StartKeyStr);
              li.SubItems.Add(ssk^.StopKeyStr)
            end

      finally
        sreader.Free;
        sstream.Free;
        sklist.Free;
        ssslist.Free;
        sstrings.Free;
        sini.Free;
        lvKeys.Items.EndUpdate
        end
    end
end;

procedure TDlgExtKeysEditor.mnuSaveListClick(Sender: TObject);
var slist: TStringList; i: Integer; sstartstop: Boolean; sitem: TListItem;
begin
  SaveDialog1.Filter:= TextFilter;

  if SaveDialog1.Execute then
    begin
      sstartstop:= lvKeys.ViewStyle=vsReport;
      slist:= TStringList.Create;
      slist.BeginUpdate;
      for i:= 0 to lvKeys.Items.Count-1 do
        begin
          sitem:= lvKeys.Items[i];
          if sstartstop then slist.Add(sitem.SubItems[0] + '|' + sitem.SubItems[1])
                        else slist.Add(sitem.Caption)
        end;
      slist.EndUpdate;
      try
        slist.SaveToFile(SaveDialog1.FileName)
      finally
        slist.Free
        end
    end
end;

procedure TDlgExtKeysEditor.mnuSaveListDefClick(Sender: TObject);
var
  sklist: TKeywordList;
  ssslist: TStartStopKeyList;
  sini: TCustomIniFile;
  sstrings: TStrings;
  startkey: string;
  pek: pEditKeyInfo;
  spk: pStartStopInfo;
  i, j: Integer;
  sstream: TStream;
  swriter: TWriter;
  ssection: string;
begin
  if Sender=mnuSaveListDefText then SaveDialog1.Filter:= TextFilter
                               else SaveDialog1.Filter:= BinFilter;

  if SaveDialog1.Execute then
    begin
      sklist:= nil;
      ssslist:= nil;
      sini:= nil;
      sstream:= nil;
      swriter:= nil;
      
      if lvKeys.ViewStyle<>vsReport then
        begin
          sklist:= fKeywordClass.Create;
          sklist.BeginUpdate;
          for i:= 0 to lvKeys.Items.Count-1 do
            begin
              pek:= woList[i];
              j:= sklist.AddKeyWord(lvKeys.Items[i].Caption, pek.Attributes.Options, pek.Attributes.Style,
                                                             pek.Attributes.ContextNumber,
                                                             pek.Attributes.Cursor, pek.Attributes.Backgnd,
                                                             pek.Attributes.Foregnd);
              with pKeyInfoLen(sklist.KeyList[j])^ do
                begin
                  Scope:= pek.Scope;
                  Priority:= pek.Priority;
                  //Extra:= pk.Extra
                end
            end;
          sklist.EndUpdate
        end
      else
        begin
          ssslist:= fStartStopClass.Create;
          for i:= 0 to lvKeys.Items.Count-1 do
            begin
              pek:= woList[i];
              startkey:= lvKeys.Items[i].SubItems[0];
              if startkey<>'' then
                begin
                  j:= ssslist.AddStartStopKey(startkey, lvKeys.Items[i].SubItems[1],
                                  pek^.Attributes.Options, pek^.Attributes.Style, pek^.Attributes.ContextNumber,
                                  pek^.Attributes.Cursor, pek^.Attributes.Backgnd, pek^.Attributes.Foregnd,
                                  ssoParStop in pek^.ssOpt);
                  spk:= ssslist.Pointers[j];
                  spk.Scope:= pek.Scope;
                  spk.Priority:= pek.Priority;
                  spk.ssOptions:= pek^.ssOpt
                  //spk.Extra:= pk.Extra
                end;
            end
        end;

      try
        if Sender=mnuSaveListDefText then
          begin
            sstrings:= TStringList.Create;
            sini:= TMemIniFile.Create(SaveDialog1.FileName);
            if sklist<>nil then ssection:= 'Keywords'
                           else ssection:= 'Sections';
            try
              sini.EraseSection(ssection)
            except   // ignore failed attempts at erasing section
              end;

            if sklist<>nil then sklist.SaveToIniStrings(sstrings)
                           else ssslist.SaveToIniStrings(sstrings);
            for i:= 0 to sstrings.Count-1 do sini.WriteString(ssection, sstrings.Names[i], sstrings.Values[sstrings.Names[i]])
          end

        else
          begin     // binary save
            sstream:= TFileStream.Create(SaveDialog1.FileName, fmCreate);
            swriter:= TWriter.Create(sstream, 1024);
            if sklist<>nil then
              begin
                swriter.WriteString(BinKeywordSig);
                sklist.WriteData(swriter)
              end
            else
              begin
                swriter.WriteString(BinSSSig);
                ssslist.WriteData(swriter)
              end
          end
      finally
        sini.Free;
        sklist.Free;
        ssslist.Free;
        swriter.Free;
        sstream.Free
        end
    end
end;

procedure TDlgExtKeysEditor.lvKeysEdited2(Sender: TObject; Item: TListItem; var S: WideString);
begin
  if S<>fSavedKeyword then Modified:= True;
  lblHint.Show
end;

procedure TDlgExtKeysEditor.lvKeysSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  ProcessSelection
end;

procedure TDlgExtKeysEditor.btnHelpClick(Sender: TObject);
const HlpKey = 'Keywords editor'#0;
begin
  {$IFDEF pmClx}
  Application.KeywordHelp(HlpKey)
  {$ELSE}
  Application.HelpCommand(HELP_KEY, Integer(@HlpKey[1]))
  {$ENDIF}
end;

procedure TDlgExtKeysEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  lvKeys.OnSelectItem:= nil  // avoid doing ProcessSelection
end;

function TDlgExtKeysEditor.GetNextSelItem(Item: TListItem): TListItem;
var AIndex: Integer;
begin
  Result:= nil;
  AIndex:= Item.Index;
  while True do
    begin
      Inc(AIndex);
      if AIndex >= lvKeys.Items.Count then Exit;
      Item := lvKeys.Items[AIndex];
      if Item.Selected then
        begin
          Result:= Item;
          Exit;
        end;
    end
end;

procedure TDlgExtKeysEditor.ExchangeItems(Index1, Index2: Integer);
var sli: TListItem; i: Integer;
begin
  for i:= 0 to lvKeys.Items.Count-1 do lvKeys.Items[i].Selected:= False;  // otherwise index out of range error in Clx
  lvKeys.Items.BeginUpdate;
  try
    sli:= TListItem.Create(lvKeys.Items);
    sli.Assign(lvKeys.Items[Index1]);
    lvKeys.Items[Index1].Assign(lvKeys.Items[Index2]);
    lvKeys.Items[Index2].Assign(sli);
    woList.Exchange(Index1, Index2);
    sli.Free;
  finally
      lvKeys.Items.EndUpdate
    end;
end;

procedure TDlgExtKeysEditor.btnMoveClick(Sender: TObject);
var sitem: TListItem; i, scur, snew: Integer;
begin
  sitem:= lvKeys.ItemFocused;
  if sitem=nil then sitem:= lvKeys.Selected;
  if sitem<>nil then
    begin
      scur:= sitem.Index;
      if Sender=btnMoveUp then snew:= scur-1
                          else snew:= scur+1;
      if (snew>=0) and (snew<lvKeys.Items.Count) then
        begin
          ExchangeItems(scur, snew);
          Modified:= True;
          fSelectionProcessed:= False;
          for i:= 0 to lvKeys.Items.Count-1 do lvKeys.Items[i].Selected:= False;
          lvKeys.ItemFocused:= lvKeys.Items[snew];
          lvKeys.Items[snew].Selected:= True
        end
    end
end;

end.
