{$I ViewerOptions.inc}

unit UFormPluginsOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TFormPluginsOptions = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    boxPlugins: TGroupBox;
    List: TListView;
    btnAdd: TButton;
    btnRemove: TButton;
    btnConfig: TButton;
    btnUp: TButton;
    btnDown: TButton;
    boxOptions: TGroupBox;
    chkPriority: TCheckBox;
    chkTCVar: TCheckBox;
    chkHideKeys: TCheckBox;
    btnHelp: TButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnConfigClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnHelpClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    function PluginPresent(const fn: string): boolean;
    procedure AddPluginsFromTCIni(const fExeFilename, fIniFilename: string);
    procedure AddPluginsFromFolder(const fn: string);
    procedure AddPluginFile(const fn: string);
    procedure AddPluginZip(Files: TStrings);
  public
    { Public declarations }
    FConfigFolder: string;
  end;


function FArchiveProp(
  const fn: string;
  var SDesc, SType, SDefDir: string): boolean;
function FAddPluginZip(
  const fn, sFolderPlugins: string;
  const Handle: THandle;
  var sPlugin: string): boolean;


implementation

uses
  ATxSProc, ATxFProc, WLXProc,
  ATxMsg, ATxMsgProc, ATViewerMsg,
  ATxUtils, ATxTotalCmd, ATxVersionInfo, 
  ATxUnpack_Dll,
  UFormPluginsAdd, UFormPluginsEdit,
  IniFiles;

{$R *.DFM}

//-------------------------------------
function FArchiveProp(
  const fn: string;
  var SDesc, SType, SDefDir: string): boolean;
const
  cInf = 'pluginst.inf';
  cSec = 'PluginInstall';
var
  Ini: TIniFile;
  SInf: string;
begin
  SDesc:= '';
  SType:= '';
  SDefDir:= '';

  SInf:= FTempPath + '\' + cInf;
  FDelete(SInf);
  FUnpackSingle(fn, FTempPath, cInf);
  Result:= IsFileExist(SInf);
  if not Result then Exit;

  Ini:= TIniFile.Create(SInf);
  try
    SDesc:= Ini.ReadString(cSec, 'description', '');
    SType:= Ini.ReadString(cSec, 'type', '');
    SDefDir:= Ini.ReadString(cSec, 'defaultdir', '');
    SDefDir:= ExtractFileName(SDefDir);
  finally
    Ini.Free;
    FDelete(SInf);
  end;
end;


procedure ListSwapItems(List: TListView; n1, n2: integer);
var
  s: string;
  en: boolean;
begin
  with List do
    begin
    Items.BeginUpdate;

    s:= Items[n1].Caption;
    Items[n1].Caption:= Items[n2].Caption;
    Items[n2].Caption:= s;

    s:= Items[n1].SubItems[0];
    Items[n1].SubItems[0]:= Items[n2].SubItems[0];
    Items[n2].SubItems[0]:= s;

    s:= Items[n1].SubItems[1];
    Items[n1].SubItems[1]:= Items[n2].SubItems[1];
    Items[n2].SubItems[1]:= s;

    en:= Items[n1].Checked;
    Items[n1].Checked:= Items[n2].Checked;
    Items[n2].Checked:= en;

    Items.EndUpdate;
    end;
end;

procedure ListSelect(List: TListView; n: integer);
begin
  with List do
    begin
    if n>Items.Count-1 then Dec(n);
    if n>=0 then
      begin
      ItemFocused:= Items[n];
      Selected:= ItemFocused;
      Selected.MakeVisible(false);
      end;
    end;
end;

procedure TFormPluginsOptions.btnAddClick(Sender: TObject);
var
  fn1, fn2: AnsiString;
begin
  with TFormPluginsAdd.Create(Self) do
    try
      if ShowModal=mrOk then
        begin
        fn1:= edPath1.Text;
        fn2:= edPath2.Text;

        if chkSrcFolder.Checked then AddPluginsFromFolder(fn1) else
         if chkSrcTC.Checked then AddPluginsFromTCIni(fn2, fn1) else
          if chkSrcFile.Checked then AddPluginFile(fn1) else
           if chkSrcZip.Checked then AddPluginZip(OpenDialog1.Files);
        end;
    finally
      Release;
    end;
end;

procedure TFormPluginsOptions.btnRemoveClick(Sender: TObject);
var
  n: integer;
begin
  with List do
    if Assigned(Selected) then
      begin
      n:= Selected.Index;
      List.Items.Delete(n);
      ListSelect(List, n);
      end;
end;

procedure TFormPluginsOptions.btnConfigClick(Sender: TObject);
begin
  with List do
    if Assigned(Selected) then
      with Items[Selected.Index] do
        with TFormPluginsEdit.Create(Self) do
          try
            edFilename.Text:= SubItems[1];
            edDetect.Text:= SubItems[0];
            if ShowModal=mrOk then
              SubItems[0]:= edDetect.Text;
          finally
            Release;
          end;
end;

procedure TFormPluginsOptions.btnUpClick(Sender: TObject);
var
  n: integer;
begin
  with List do
    if Assigned(Selected) then
      begin
      n:= Selected.Index;
      if n>0 then
        begin
        ListSwapItems(List, n, n-1);
        ListSelect(List, n-1);
        end;
      end;
end;

procedure TFormPluginsOptions.btnDownClick(Sender: TObject);
var
  n: integer;
begin
  with List do
    if Assigned(Selected) then
      begin
      n:= Selected.Index;
      if n<Items.Count-1 then
        begin
        ListSwapItems(List, n, n+1);
        ListSelect(List, n+1);
        end;
      end;
end;

procedure TFormPluginsOptions.FormShow(Sender: TObject);
begin
  {$I Lang.FormPluginsOptions.inc}

  ListSelect(List, 0);
  ListSelectItem(Self, nil, false);
end;

function TFormPluginsOptions.PluginPresent(const fn: TWlxFilename): boolean;
var
  i: integer;
begin
  Result:= false;
  with List do
    for i:= 0 to Items.Count-1 do
      if UpperCase(fn) = UpperCase(Items[i].SubItems[1]) then
        begin Result:= true; Break end;
end;

//-----------------------------------------------
procedure TFormPluginsOptions.AddPluginsFromTCIni(const fExeFilename, fIniFilename: string);
const
  Section = 'ListerPlugins';
var
  i: integer;
  fn: TWlxFilename;
  detect, desc: string;
  sAdded, sDups: string;
begin
  if (fIniFilename='') or (not IsFileExist(fIniFilename)) then
    begin MsgError(MsgViewerPluginsInvalidFile, Handle); Exit end;

  if (fExeFilename='') or (Pos('Total Commander', FGetVersionInfo(fExeFilename, vsFileDescription))=0) then
    begin MsgError(MsgViewerPluginsInvalidTCExe, Handle); Exit end;

  SetEnvironmentVariable('COMMANDER_PATH', PChar(ExtractFileDir(fExeFilename)));
  sAdded:= '';
  sDups:= '';

  with List do
    begin
    //Items.BeginUpdate;

    for i:= 0 to WlxPluginsMaxCount-1 do
      begin
      SetTcIniFilename(fIniFilename);
      fn:= GetTcIniKey(Section, IntToStr(i));
      fn:= SExpandVars(fn);
      if fn='' then Break;
      detect:= GetTcIniKey(Section, IntToStr(i)+'_detect');
      if detect='' then
        detect:= WlxGetDetectString(fn);
      desc:= SPluginName(fn);

      if PluginPresent(fn) then
        begin
        sDups:= sDups + desc + #13;
        end
      else
        begin
        sAdded:= sAdded + desc + #13;
        with Items.Add do
          begin
          Checked:= true;
          Caption:= desc;
          SubItems.Add(detect);
          SubItems.Add(fn);
          end;
        end;
        end;

    //Items.EndUpdate;
    MsgInstall(sAdded, sDups, False, Handle);
    end;
end;

//-----------------------------------------------
procedure TFormPluginsOptions.AddPluginFile(const fn: TWlxFilename);
var
  detect, desc: string;
  sAdded, sDups: string;
begin
  if (fn='') or (not IsFileExist(fn)) then
    begin MsgError(MsgViewerPluginsInvalidFile, Handle); Exit end;

  sAdded:= '';
  sDups:= '';

  detect:= WlxGetDetectString(fn);
  desc:= SPluginName(fn);

  if PluginPresent(fn) then
    begin
    sDups:= sDups+desc+#13;
    end
  else
    begin
    sAdded:= sAdded+desc+#13;
    with List.Items.Add do
      begin
      Checked:= true;
      Caption:= desc;
      SubItems.Add(detect);
      SubItems.Add(fn);
      end;
    end;

  MsgInstall(sAdded, sDups, False, Handle);
end;


//-----------------------------------------------
function SFolderNameFromZipFile(const fn: string): string;
begin
  //Strip extension
  Result:= ChangeFileExt(ExtractFileName(fn), '');
  //Strip trailing numbers
  while (Result <> '') and
    (Result[Length(Result)] in ['0'..'9', '.', '_', '-']) do
    SetLength(Result, Length(Result) - 1);
end;


//-----------------------------------------------
function FAddPluginZip(
  const fn, sFolderPlugins: string;
  const Handle: THandle;
  var sPlugin: string): boolean;
const
  cInf = 'pluginst.inf';
var
  SDesc, SType, SDefDir,
  sFolder: string;
begin
  Result:= false;
  sPlugin:= '';

  FArchiveProp(fn, SDesc, SType, SDefDir);
  if sDefDir <> '' then
    sFolder:= sFolderPlugins + '\' + sDefDir
  else
    sFolder:= sFolderPlugins + '\' + SFolderNameFromZipFile(fn);

  if not IsFileExist(fn) then
    begin MsgError(MsgViewerPluginsInvalidFile, Handle); Exit end;

  if not FUnpackAll(fn, sFolder) then
    begin MsgError(Format(MsgString(215), [ExtractFileName(fn), sFolder]), Handle); Exit end;

  if not FSearchDir('*.wlx', sFolder, sPlugin) then
    begin MsgError(Format(MsgString(216), [ExtractFileName(fn)]), Handle); Exit end;

  DeleteFile(PChar(sFolder + '\' + cInf));
  Result:= true;
end;


//-----------------------------------------------
procedure TFormPluginsOptions.AddPluginZip(Files: TStrings);
var
  detect, desc: string;
  sAdded, sDups: string;
  sFolderPlugins, sPlugin: string;
  i: integer;
begin
  sAdded:= '';
  sDups:= '';

  sFolderPlugins:= FConfigFolder + '\Plugins';
  FCreateDir(sFolderPlugins);

  for i:= 0 to Files.Count - 1 do
    begin
    if not FAddPluginZip(Files[i],
      sFolderPlugins, Handle, sPlugin) then Continue;

    detect:= WlxGetDetectString(sPlugin);
    desc:= SPluginName(sPlugin);

    if PluginPresent(sPlugin) then
      begin
      sDups:= sDups + desc + #13;
      end
    else
      begin
      sAdded:= sAdded + desc + #13;
      with List.Items.Add do
        begin
        Checked:= true;
        Caption:= desc;
        SubItems.Add(detect);
        SubItems.Add(sPlugin);
        end;
      end;

    Application.ProcessMessages;
    end;

  MsgInstall(sAdded, sDups, True, Handle);
end;


//-----------------------------------------------
procedure TFormPluginsOptions.AddPluginsFromFolder(const fn: TWlxFilename);
begin
  //
end;

//-----------------------------------------------
procedure TFormPluginsOptions.ListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  Sel: boolean;
begin
  Sel:= Assigned(List.Selected);
  btnRemove.Enabled:= Sel;
  btnConfig.Enabled:= Sel;
  btnUp.Enabled:= Sel and (List.Selected.Index > 0);
  btnDown.Enabled:= Sel and (List.Selected.Index < List.Items.Count - 1);
end;

procedure TFormPluginsOptions.btnHelpClick(Sender: TObject);
begin
  ShowHelp(Handle, 'Plugins.html');
end;

procedure TFormPluginsOptions.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_INSERT then
    btnAddClick(Self)
  else
  if Key = VK_DELETE then
    btnRemoveClick(Self);  
end;

end.
