unit main;

interface

uses
  Windows,Messages,SysUtils,Variants,Classes,Graphics,Controls,
  Forms,Dialogs,StdCtrls,ExtCtrls,ComCtrls,ShlObj,
  Misc,StrUtils,nuServer;

type
  TXPHintWindow = class(THintWindow)
  private
    FActivating: Boolean;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNCPaint(var msg: TMessage); message WM_NCPAINT;
    procedure Paint; override;
  public
    procedure ActivateHint(Rect: TRect; const AHint: string);override;  
  end;

  TfmSFiles = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnBack: TButton;
    btnCancel: TButton;
    btnNext: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    PageView: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    RadioButton2: TRadioButton;
    RadioButton1: TRadioButton;
    Panel3: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label6: TLabel;
    Label18: TLabel;
    cbLocation1: TComboBox;
    cbLogin: TCheckBox;
    cbUpdateMethod: TComboBox;
    tEdit1: TEdit;
    tEdit3: TEdit;
    nEdit1: TEdit;
    tEdit4: TEdit;
    tEdit5: TEdit;
    SpeedButton2: TButton;
    Panel2: TPanel;
    Label3: TLabel;
    lblFName2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label9: TLabel;
    Label17: TLabel;
    lFileSize: TLabel;
    Label20: TLabel;
    Label19: TLabel;
    btnFilesAdd: TButton;
    cbFiles1: TComboBox;
    btnFilesDelete: TButton;
    eFiles1: TEdit;
    eFileSize: TEdit;
    eFiles2: TEdit;
    Gridview1: TListView;
    Button1: TButton;
    Panel4: TPanel;
    Label13: TLabel;
    lblFName3: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label22: TLabel;
    Label21: TLabel;
    eVersion3: TMemo;
    eVersion1: TEdit;
    eVersion2: TEdit;
    Panel5: TPanel;
    Label23: TLabel;
    lblFName4: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label14: TLabel;
    eOptions2: TComboBox;
    eOptions4: TComboBox;
    eOptions3: TEdit;
    eOptions5: TEdit;
    procedure btnBackClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnFilesAddClick(Sender: TObject);
    procedure btnFilesDeleteClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure cbLocation1Change(Sender: TObject);
    procedure cbLoginClick(Sender: TObject);
    procedure cbUpdateMethodChange(Sender: TObject);
    procedure eFiles1Change(Sender: TObject);
    procedure eOptions5KeyPress(Sender: TObject; var Key: Char);
    procedure eVersionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Gridview1Change(Sender: TObject; Item: TListItem;Change: TItemChange);
    procedure HotLabelMouseEnter(Sender: TObject);
    procedure HotLabelMouseLeave(Sender: TObject);
    procedure lvFileSizeClick(Sender: TObject);
    procedure lvGenerateClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure tEdit3Change(Sender: TObject);
  private
    { Private declarations }
    ActivePage: integer;
    DirPath,FileName: string;
    IsEdit: boolean;
    nuServer1 : TnuServer;
    function GetFileSize(FileName: String): LongInt;
    procedure Check4InfFile();
    procedure Initialize();
    procedure SaveFileInfo();
    procedure SetHints();
    procedure UpdateHeader(hIndex:integer);
  public
    { Public declarations }
  end;

var
  fmSFiles: TfmSFiles;

implementation

{$R *.dfm}
{$R winxp.res}

{TXPHintWindow}
procedure TXPHintWindow.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  Inc(R.Left,4); { indent just a little more }
  Inc(R.Top,2);
  Canvas.Font.Color := Screen.HintFont.Color;
  DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_LEFT or DT_NOPREFIX or
    DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
end; { Paint }

{ ----------------------------------------------------------------- }

procedure TXPHintWindow.WMNCPaint(var msg: TMessage);
var 
  DC: HDC; 
  R: TRect;
begin
  DC := GetWindowDC(Handle);
  try
    R := Rect(0, 0, Width, Height);
    Windows.DrawEdge(DC, R, EDGE_ETCHED, BF_RECT or BF_MONO);
  finally
    ReleaseDC(Handle, DC); 
  end; 
end; { WMNCPaint }

{ ----------------------------------------------------------------- }

procedure TXPHintWindow.ActivateHint(Rect: TRect; const AHint: string);
begin
  FActivating := True;
  try
    Caption := AHint;
    Inc(Rect.Right,4); { make a little wider }
    Inc(Rect.Bottom,4);
    UpdateBoundsRect(Rect);
    if Rect.Top + Height > Screen.DesktopHeight then
      Rect.Top := Screen.DesktopHeight - Height;
    if Rect.Left + Width > Screen.DesktopWidth then
      Rect.Left := Screen.DesktopWidth - Width;
    if Rect.Left < Screen.DesktopLeft then Rect.Left := Screen.DesktopLeft;
    if Rect.Bottom < Screen.DesktopTop then Rect.Bottom := Screen.DesktopTop;
    SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, Width, Height,
      SWP_SHOWWINDOW or SWP_NOACTIVATE);
    Invalidate;
  finally
    FActivating := False;
  end;
end; { ActivateHint }

{ ----------------------------------------------------------------- }

procedure TXPHintWindow.CreateParams(var Params: TCreateParams);
{ Enable drop shadow effect on Windows XP and later }
const
  CS_DROPSHADOW = $00020000;
begin
  inherited;
  //Params.Style := Params.Style and not ws_border;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and
     ((Win32MajorVersion > 5) or
      ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end; { CreateParams }

{ ----------------------------------------------------------------- }
{ ----------------------------------------------------------------- }

{TfmSFiles}
procedure TfmSFiles.FormCreate(Sender: TObject);
begin
  nuServer1 := TnuServer.Create(self);
  SetHints();
end; { FormCreate }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.FormDestroy(Sender: TObject);
begin
  nuServer1.Free;
end; { FormDestroy }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.FormShow(Sender: TObject);
begin
  ActivePage := 0;
  TabSheet1.TabVisible := False;
  TabSheet2.TabVisible := false;
  TabSheet3.TabVisible := false;
  TabSheet4.TabVisible := false;
  TabSheet5.TabVisible := false;
  PageView.Height := 252;
  PageView.ActivePageIndex := ActivePage;
  UpdateHeader(0);
  btnBack.Enabled := false;
  btnNext.Enabled := true;
  Initialize();
  Check4InfFile();
end; { FormShow }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.Initialize();
begin
  DirPath := ExtractFilePath(Application.ExeName);
  FileName := '';
  IsEdit := false;
  cbUpdateMethodChange(self);
  tEdit1.Text := 'update_01.inf';
  tEdit3.Text := 'www.mywebsite.com/updates/files/';
  tEdit4.Enabled := false;
  tEdit5.Enabled := false;
  nEdit1.Text := '80';
  cbLogin.Checked := false;
  tEdit4.Text := '';
  tEdit5.Text := '';
  cbFiles1.ItemIndex := 0;
  eFiles1.Text := '';
  eFiles2.Text := '';
  eFileSize.Text := '0';
  Gridview1.Clear;
  eVersion1.Text := '';
  eVersion2.Text := '';
  eVersion3.Lines.Clear;
  Label28.Enabled := false;
  Label29.Enabled := false;
  Label31.Enabled := false;
  eOptions2.Enabled := false;
  eOptions3.Enabled := false;
  eOptions5.Enabled := false;
  eOptions2.Clear;
  eOptions2.Items.Add(' None');
  eOptions2.ItemIndex := 0;
  eOptions3.Text := '';
  eOptions4.ItemIndex := 0;
  eOptions5.Text := '0';
  SpeedButton2.Visible := false;
  btnNext.Caption := 'Next >';
end; { Initialize }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.cbUpdateMethodChange(Sender: TObject);
begin
  cbLocation1.Clear;
  if cbUpdateMethod.ItemIndex < 2 then begin
    cbLocation1.Items.Add(' http://');
    cbLocation1.Items.Add(' https://');
    cbLocation1.Items.Add(' ftp://');
    cbLocation1.Items.Add(' file://');
    Label5.Enabled := true;
    Label9.Enabled := true;
    cbFiles1.Enabled := true;
    eFiles2.Enabled := true;
    end
  else begin
    cbLocation1.Items.Add(' http://');
    cbLocation1.Items.Add(' https://');
    Label5.Enabled := false;
    Label9.Enabled := false;
    cbFiles1.Enabled := false;
    eFiles2.Enabled := false;
    end;
  cbLocation1.ItemIndex := 0;
  cbLocation1Change(self);
  if cbUpdateMethod.ItemIndex = 1 then begin
    Label28.Enabled := true;
    Label29.Enabled := true;
    Label31.Enabled := true;
    eOptions2.Enabled := true;
    eOptions3.Enabled := true;
    eOptions5.Enabled := true;
    end
  else begin
    Label28.Enabled := false;
    Label29.Enabled := false;
    Label31.Enabled := false;
    eOptions2.Enabled := false;
    eOptions3.Enabled := false;
    eOptions5.Enabled := false;
    end;
end; { cbUpdateMethodChange }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.Check4InfFile();
var
  i: integer;
  InfFile: string;
  aListItem: TListItem;
begin
  InfFile := DirPath + tEdit1.Text;
  if FileExists(InfFile) then
    nuServer1.Load(InfFile)
  else exit;

  IsEdit := true;
  cbUpdateMethod.ItemIndex := Integer(nuServer1.UpgradeMethod);
  cbUpdateMethodChange(self);
  for i := 0 to 3 do
    begin
    if trim(cbLocation1.Items.Strings[i]) = nuServer1.Protocol then
      cbLocation1.ItemIndex := i;
      cbLocation1Change(self);
    end;

  tEdit3.Text := nuServer1.FilePath;
  nEdit1.Text := IntToStr(nuServer1.Port);
  cbLogin.Checked := nuServer1.Login;
  tEdit4.Text := nuServer1.UserID;
  tEdit5.Text := EncryptDecrypt(nuServer1.Password);
  eVersion1.Text := nuServer1.VersionDate;
  eVersion2.Text := nuServer1.VersionNumber;
  eVersion3.Lines.Text := nuServer1.UpgradeMsg;

  GridView1.Clear;
  for i := 0 to nuServer1.ServerFiles.Count - 1 do begin
    aListItem := Gridview1.Items.Add;
    aListItem.Caption := nuServer1.ServerFiles[i].FileName;
    if nuServer1.ServerFiles[i].SubDir = '' then
      aListItem.SubItems.Add(cbFiles1.Items.Strings[Integer(nuServer1.ServerFiles[i].TargetDir)])
    else
      aListItem.SubItems.Add(cbFiles1.Items.Strings[Integer(nuServer1.ServerFiles[i].TargetDir)] + ' \' + nuServer1.ServerFiles[i].SubDir);
    aListItem.SubItems.Add(IntToStr(Integer(nuServer1.ServerFiles[i].TargetDir)));
    aListItem.SubItems.Add(nuServer1.ServerFiles[i].SubDir);
    aListItem.SubItems.Add(IntToStr(nuServer1.ServerFiles[i].FileSize));
    end;

  { eOptions2.ItemIndex is set later when page is selected }
  eOptions3.Text := nuServer1.ExtractParams;
  eOptions4.ItemIndex := Integer(nuServer1.MiscFilesUpgrade);
  eOptions5.Text := IntToStr(nuServer1.ExitCode);
end; { Check4InfFile }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.UpdateHeader(hIndex:integer);
begin
  case hIndex of
    0: begin
       Label1.Caption := 'Update Information Files';
       Label2.Caption := 'Select whether you want to create a new file or open an existing file.';
       end;
    1: begin
       Label1.Caption := 'Update File Location';
       Label2.Caption := 'Provide the file path and login information required for Net Update to locate the update files required for the update methed specified.';
       end;
    2: begin
       Label1.Caption := 'Update Files';
       Label2.Caption := 'List the update files required and their destination (target) directory. If using the External Setup method, then only list the setup file to launch.';
       end;
    3: begin
       Label1.Caption := 'Update Version';
       Label2.Caption := 'Provide version information for this update. Text message, if provided, will appear when Net Updater advises user there is a new update available.';
       end;
    4: begin
       Label1.Caption := 'Update Options';
       Label2.Caption := 'If using the External Setup method then select the file to execute and provide any required parameters. Indicate how you want miscellaneous files handled.';
       end;
  end;
end; { UpdateHeader }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.btnCancelClick(Sender: TObject);
begin
  Close;
end; { btnCancelClick }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.btnNextClick(Sender: TObject);
var
  i:integer;
begin
  inc(ActivePage);
  btnBack.Enabled := true;
  case ActivePage of
    1: begin
       Initialize();
       if RadioButton2.Checked then begin
         OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);
         OpenDialog1.Filter := 'InfoFiles (*.inf)|*.inf';
         OpenDialog1.FileName := '';
         if OpenDialog1.Execute() then begin
           FileName := OpenDialog1.FileName;
           tEdit1.Text := ExtractFileName(OpenDialog1.FileName);
           DirPath := ExtractFilePath(OpenDialog1.FileName);
           Check4InfFile();
           end;
         end;
       PageView.ActivePageIndex := ActivePage;
       tEdit1.SetFocus;
       UpdateHeader(1);
       end;

    2: begin
       PageView.ActivePageIndex := ActivePage;
       lblFName2.Caption := tEdit1.Text;
       if cbLocation1.ItemIndex < 3 then
         if not AnsiEndsStr('/', tEdit3.Text) then
         tEdit3.Text := tEdit3.Text + '/';

       if cbLocation1.ItemIndex = 3 then
         if not AnsiEndsStr('\', tEdit3.Text) then
           tEdit3.Text := tEdit3.Text + '\';

       if Gridview1.Items.Count = 0 then
         btnNext.Enabled := false else
         btnNext.Enabled := true;
       eFiles1.SetFocus;
       eFiles1.SelectAll;
       UpdateHeader(2);
       end;

    3: begin
       PageView.ActivePageIndex := ActivePage;
       lblFName3.Caption := tEdit1.Text;
       if (trim(eVersion1.Text) = '') or (trim(eVersion2.Text) = '') then
         btnNext.Enabled := false else
         btnNext.Enabled := true;
       eVersion1.SetFocus;
       eVersion1.SelectAll;
       UpdateHeader(3);
       end;

    4: begin
       PageView.ActivePageIndex := ActivePage;
       lblFName4.Caption := tEdit1.Text;
       Panel5.SetFocus;
       eOptions2.Clear;
       eOptions2.Items.Add(' None');
       eOptions2.ItemIndex := 0;
       for i := 0 to Gridview1.Items.Count - 1 do begin
         if ExtractFileExt(Gridview1.Items.Item[i].Caption) = '.exe' then begin
           eOptions2.Items.Add(' '+ExtractFileName(Gridview1.Items.Item[i].Caption));
           if IsEdit then begin
             if Gridview1.Items.Item[i].Caption = nuServer1.ExeFile then
               eOptions2.ItemIndex := eOptions2.Items.Count - 1;
             end;
           end;
         end;
       UpdateHeader(4);
       btnNext.Caption := 'Finish';
       end;

    5: SaveFileInfo();
  end;
end; { btnNextClick }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.SaveFileInfo();
var
  i: integer;
  ServerFile: TServerFile;
begin
  nuServer1.Protocol := trim(cbLocation1.Text);
  nuServer1.FilePath := trim(tEdit3.Text);
  nuServer1.Port := StrToInt(trim(nEdit1.Text));
  nuServer1.Login := cbLogin.Checked;
  nuServer1.UserID := trim(tEdit4.Text);
  nuServer1.Password := EncryptDecrypt(trim(tEdit5.Text));
  nuServer1.VersionDate := trim(eVersion1.Text);
  nuServer1.VersionNumber := trim(eVersion2.Text);
  nuServer1.UpgradeMsg := trim(eVersion3.Lines.Text);
  nuServer1.UpgradeMethod := TnuUpgradeMethod(cbUpdateMethod.ItemIndex);
  if eOptions2.ItemIndex = 0 then
    nuServer1.ExeFile := '' else
    nuServer1.ExeFile := trim(eOptions2.Items.Strings[eOptions2.ItemIndex]);
  nuServer1.ExtractParams := trim(eOptions3.Text);
  nuServer1.MiscFilesUpgrade := TnuMiscFilesUpgradeBehavior(eOptions4.ItemIndex);
  nuServer1.Exitcode := StrToInt(trim(eOptions5.Text));
  //nuServer1.CloseNetUpdate := cbCloseNU.Checked;
  //nuServer1.SecurityCode := ssLabel.Caption;

  nuServer1.ServerFiles.Clear;
  for i := 0 to Gridview1.Items.Count - 1 do begin
    ServerFile := nuServer1.ServerFiles.Add();
    ServerFile.FileName := Gridview1.Items.Item[i].Caption;
    ServerFile.TargetDir := TnuTargetDir(StrToInt(GridView1.Items.Item[i].SubItems[1]));
    ServerFile.SubDir := GridView1.Items.Item[i].SubItems[2];
    ServerFile.FileSize := StrToInt(GridView1.Items.Item[i].SubItems[3]);
    end;

  if FileName = '' then begin
    SaveDialog1.InitialDir := ExtractFilePath(Application.ExeName);
    SaveDialog1.Filter := 'InfoFiles (*.inf)|*.inf';
    SaveDialog1.FileName := tEdit1.Text;
    if SaveDialog1.Execute() then begin
      FileName := SaveDialog1.FileName;
      end;
    end;
  if FileName <> '' then begin
    nuServer1.Store(FileName);
    MessageBeep(MB_ICONASTERISK);
    MessageBox(Handle,PChar('File saved successfully.'),PChar('Net Updater'),MB_OK);
    ActivePage := 0;
    PageView.ActivePageIndex := ActivePage;
    btnBack.Enabled := false;
    btnNext.Enabled := true;
    btnNext.Caption := 'Next >';
    end
  else MessageBox(Handle,PChar('Error saving file.'),PChar('Net Updater'),MB_OK);
end; { SaveFileInfo }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.btnBackClick(Sender: TObject);
begin
  btnNext.Caption := 'Next >';
  if ActivePage > 0 then
    dec(ActivePage);
  PageView.ActivePageIndex := ActivePage;
  if ActivePage = 0 then begin
    btnBack.Enabled := false;
    end;
  if ActivePage = 1 then begin
    cbLocation1.SetFocus;
    end;
  if ActivePage = 2 then begin
    eFiles1.SetFocus;
    eFiles1.SelectAll;
    end;
  if ActivePage = 3 then begin
    eVersion1.SetFocus;
    eVersion1.SelectAll;
    end;
  UpdateHeader(ActivePage);
  btnNext.Enabled := true;
end; { btnBackClick }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.cbLocation1Change(Sender: TObject);
begin
  if not fmSFiles.Visible then exit;
  if cbLocation1.ItemIndex = 0 then nEdit1.Text := '80';
  if cbLocation1.ItemIndex = 1 then nEdit1.Text := '443';
  if cbLocation1.ItemIndex = 2 then nEdit1.Text := '21';
  if cbLocation1.ItemIndex = 3 then begin
    SpeedButton2.Visible := true;
    nEdit1.Text := '0';
    nEdit1.Visible := false;
    Label10.Visible := false;
    tEdit3.Text := 'x:\mydirectory\updates\files\';
    end
  else begin
    SpeedButton2.Visible := false;
    nEdit1.Visible := true;
    Label10.Visible := true;
    tEdit3.Text := 'www.mywebsite.com/updates/files/';
    tEdit3Change(Sender);
    end;
end; { cbLocation1Change }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.SpeedButton2Click(Sender: TObject);
var
  sFolder : string;
begin
  sFolder := BrowseDialog('Select the server directory where update files are stored',BIF_RETURNONLYFSDIRS);
  if sFolder <> '' then begin
    if not AnsiEndsStr('\',sFolder) then sFolder := sFolder + '\';
    tEdit3.Text := sFolder;
    end;
end; { SpeedButton2Click }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.cbLoginClick(Sender: TObject);
begin
  Label11.Enabled := cbLogin.Checked;
  Label12.Enabled := cbLogin.Checked;
  tEdit4.Enabled := cbLogin.Checked;
  tEdit5.Enabled := cbLogin.Checked;
end; { cbLoginClick }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.SpeedButton1Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Executable Files (*.exe)|*.exe|All Files (*.*)|*.*';
  if not DirectoryExists(DirPath) then begin
    MessageBeep(MB_ICONHAND);
    MessageDlg(Format('Directory "%s" does not exist.'+#13#10+'Return to previous page and correct directory path.',[tEdit3.Text]),mtERROR,[mbOK],0);
    exit;
    end;
  OpenDialog1.InitialDir := tEdit3.Text;
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute() then begin
    eFiles1.Text := ExtractFileName(OpenDialog1.FileName);
    eFileSize.Text := IntToStr(GetFileSize(OpenDialog1.FileName));
    end;
end; { SpeedButton1Click }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.eFiles1Change(Sender: TObject);
begin
  if trim(eFiles1.Text) <> '' then
    btnFilesAdd.Enabled := true else
    btnFilesAdd.Enabled := false;
end; { eFiles1Change }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.btnFilesAddClick(Sender: TObject);
var
  s:string;
  aListItem: TListItem;
begin
  s := eFiles1.Text;
  if AnsiStartsStr('\',s) then Delete(s,1,1);
  if AnsiStartsStr('/',s) then Delete(s,1,1);
  if AnsiStartsStr(tEdit3.Text,s) then
    Delete(s,1,Length(tEdit3.Text));

  aListItem := Gridview1.Items.Add;
  aListItem.Caption := s;
  if cbUpdateMethod.ItemIndex < 2 then begin
    if trim(eFiles2.Text) = '' then
      aListItem.SubItems.Add(trim(cbFiles1.Text)) else
      aListItem.SubItems.Add(trim(cbFiles1.Text) + ' \' + trim(eFiles2.Text));

    aListItem.SubItems.Add(IntToStr(cbFiles1.ItemIndex));
    aListItem.SubItems.Add(eFiles2.Text);
    aListItem.SubItems.Add(eFileSize.Text);
    end;
  btnNext.Enabled := true;
  Gridview1.Selected := Gridview1.Selected;  // force selection
  eFiles1.Text := '';
  eFileSize.Text := '0';
  cbFiles1.ItemIndex := 0;
  eFiles2.Text := '';
  eFiles1.SetFocus;
end; { btnFilesAddClick }

{ ----------------------------------------------------------------- }


procedure TfmSFiles.Gridview1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  if Gridview1.Items.Count > 0 then
    btnFilesDelete.Enabled := true else
    btnFilesDelete.Enabled := False;
end; { Gridview1Change }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.btnFilesDeleteClick(Sender: TObject);
var
  r:integer;
begin
  r := Gridview1.Selected.Index;
  Gridview1.Items.Delete(r);
  if Gridview1.Items.Count = 0 then begin
    btnFilesDelete.Enabled := false;
    btnNext.Enabled := false;
    end
  else begin
    if r > 0 then dec(r);
    Gridview1.ItemIndex := r;
    GridView1.SetFocus;
    end;
end; { btnFilesDeleteClick }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.tEdit3Change(Sender: TObject);
begin
  if trim(tEdit3.Text) = '' then
    btnNext.Enabled := false else
    btnNext.Enabled := true;
end; { tEdit3Change }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.eVersionChange(Sender: TObject);
begin
  if (trim(eVersion1.Text) = '') or (trim(eVersion2.Text) = '')  then
    btnNext.Enabled := false else
    btnNext.Enabled := true;
end; { eVersionChange }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.lvGenerateClick(Sender: TObject);
begin
  eVersion1.Text := FormatDateTime('yyyy-mm-dd hh:nn:ss',now());
  eVersion1.SetFocus;
end; { lvGenerateClick }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.SetHints();
begin
  Label7.Hint :=
  'Server file name where list of update files'+#13+
  'and installation parameters are stored.';

  Label8.Hint :=
  'Select the server protocol and the path to the directory '+#13+
  'where the updates files are stored.';

  Label10.Hint :=
  'Enter the port to be used.  '+#13+
  'Typically this would be 80 for HTTP, '+#13+
  '443 for HTTPS, and 21 for FTP.';

  Label6.Hint :=
  'Provide a Username and Password ONLY if your server'+#13+
  'requires a login.'+#13#13+

  'You can also specify login information dynamically,'+#13+
  'when neccessary, in the built-in password-request'+#13+
  'dialog, if the "Password Reqested" flag under Messages'+#13+
  'is selected.';

  Label12.Hint :=
  'The Username property specifies the username needed to access the data'+#13+
  'in password protected Web directories. You don''t need to specify the'+#13+
  'username/password if you reading non-protected data.';

  Label11.Hint :=
  'The Password property specifies the password needed to access the data'+#13+
  'in password protected Web directories. You don''t need to specify the'+#13+
  'username/password if you reading non-protected data.';

  Label3.Hint :=
  'Server file name where list of update files '+#13+
  'and installation parameters are stored.';

  Label4.Hint :=
  'This is the name of the actual file to be downloaded during'+#13+
  'the update. Ideally, if a file is to be executed after download,'+#13+
  'it should be the first file selected.'+#13#13+

  'Do not enter the path... it is listed on the previous '+#13+
  'page. However, if the file is not in the directory specified'+#13+
  'in the path previously listed, include the subdirectory along'+#13+
  'with the file name, ie: subdir\filename.exe.';

  lFileSize.Hint :=
  'This is the file size, in bytes, of the specified file '+#13+
  'to be downloaded.  This value is not required and can be '+#13+
  'left at zero if not known. However, providing the file size '+#13+
  'here will speed up internet performance as the file size '+#13+
  'will not have to be retrieved from the remote file before '+#13+
  'downloading begins. '+#13#13+
  'You can press the <Get size from file...> link to retrieve '+#13+
  'the size directly from a local copy of the file.'+#13#13+
  'File size is used to accurately display download progress'+#13+
  'and to determine if the file should indeed be downloaded '+#13+
  'if the <Only if size different> option is selected '+#13+
  'for micellaneous files. ';

  Label5.Hint :=
  'Select the target directory where the update files '+#13+
  'are to be placed. Generally, this would be the main '+#13+
  'application directory { ApplicationDir }. You can also specify '+#13+
  'a subdirectory if necessay.'+#13#13+
  'If you are using an installer to update the application, you '+#13+
  'may want to select the temp directory { TempDir } as the '+#13+
  'target directory.';

  Label17.Hint :=
  'Files are a the list of URLs (path plus file name) which points to'+#13+
  'files which should be downloaded on application upgrade. Instead of'+#13+
  'a URL, this could also be a list of files on a network directory if'+#13+
  'the server protocol is set to "FILE://".'+#13#13+

  'All these files will be downloaded to replace previous files on'+#13+
  'application update (if Upgrade Method is Auto Upgrade). In case'+#13+
  'if you want to redirect the user to some URL without upgrading'+#13+
  '(Upgrade Method is Redirect To URL), then Net Updater will redirect'+#13+
  'the user to first listed URL.'+#13#13+

  'Input the desired file above, select the target directory and '+#13+
  'press the <Add> button.'+#13#13+

  'To remove files, simply press <Delete>.';

  Label13.Hint :=
  'Server file name where list of update files '+#13+
  'and installation parameters are stored.';

  Label15.Hint :=
  'Updates are controlled by file date or version number as'+#13+
  'selected when configuring the client agent. If using the date'+#13+
  'to control updates, enter today''s date and time.'+#13+
  'You can press <Generate> to automatically input the current'+#13+
  'date and time.';

  Label16.Hint :=
  'Updates are controlled by file date or version number as'+#13+
  'selected when configuring the client agent. If using version'+#13+
  'number to control updates, enter the version number of this'+#13+
  'update.';

  Label22.Hint :=
  'The Version Message specifies the text string which will appears in'+#13+
  'the standard dialog which asks user about the upgrade. This can be'+#13+
  'release notes, list of new features or anything else.'+#13#13+
  'Notes'+#13+
  '  1. The standard text which appears in the dialog box which asks about'+#13+
  '     update will be displayed in native user''s language. However this'+#13+
  '     custom text can be displayed only in the language in which it is'+#13+
  '     written.'+#13+
  '  2. The upgrade-request dialog box will displayed only if the "Prompt to'+#13+
  '     update" Message flag was selected.';

  Label23.Hint :=
  'Server file name where list of update files '+#13+
  'and installation parameters are stored.';

  Label18.Hint :=
  'The Update Method controls how you would like to upgrade your'+#13+
  'application: automatically downloading and updating all newer'+#13+
  'files (Self Upgrade), using an external setup program'+#13+
  '(External Setup), or just redirecting users to the first URL'+#13+
  'listed in the Files property (Redirect To URL).'+#13#13+

  'There is three possible values:'+#13#13+

  'Self Upgrade'+#13+
  '  - application should download and replace all newer files itself.'+#13+
  'External Setup'+#13+
  '  - application should download just setup-file which will locally'+#13+
  '    extract all required. The application will be restarted upon'+#13+
  '    completion of external application.'+#13+
  'Redirect To URL'+#13+
  '  - application does not download anything. It just opens new '+#13+
  '    browser window and redirect users to first URL specified in '+#13+
  '    Files property on the next page.';

  Label28.Hint :=
  'Select the file that the application will launch when the dowload'+#13+
  'is complete. You can specify command-line options (Params) to use'+#13+
  'which will secretly extract your files locally on the user''s PC. This'+#13+
  'can be the setup program or self-extracting archive, which may require'+#13+
  'some command line options to extract files in "silent mode".'+#13#13+

  'For example, if you think that local update is better done by a setup'+#13+
  'program, you just need to download the setup file and execute it with'+#13+
  '"silent installation" command line option. The setup will locally'+#13+
  'extract all required files, then run the main program upon completion.'+#13#13+

  'Let''s suppose you are using Inno Setup available from this website:'+#13+
  '(http://www.jrsoftware.org/isinfo.htm).'+#13+
  'Then just set the Restart Params property to "/silent" or "/verysilent"'+#13+
  'and select the setup file to execute. The update will complete'+#13+
  'automatically.';

  Label29.Hint :=
  'List the command-line options (Params) to use when launching the'+#13+
  'selected executable file. This could be a ommand-line option which'+#13+
  'will secretly extract your files locally on the user''s PC. This'+#13+
  'can be the setup program or self-extracting archive, which may require'+#13+
  'some command line options to extract files in "silent mode".'+#13#13+

  'For example, if you think that local update is better done by a setup'+#13+
  'program, you just need to download the setup file and execute it with'+#13+
  '"silent installation" command line option. The setup will locally'+#13+
  'extract all required files, then run the main program upon completion.'+#13#13+

  'Let''s suppose you are using Inno Setup available from this website:'+#13+
  '(http://www.jrsoftware.org/isinfo.htm).'+#13+
  'Then just set the Restart Params property to "/silent" or "/verysilent"'+#13+
  'and select the setup file to execute. The update will complete'+#13+
  'automatically.';

  Label30.Hint :=
  'The Misc Files property determines how the component should'+#13+
  'behave when it is about to download the updated files.'+#13#13+

  'There are two possible values for the Misc Files property:'+#13#13+

  'Always Download'+#13+
  '  - this is the default value. If it is set, Net Updater will always'+#13+
  '    download all the files in queue, as described in the Info-file,'+#13+
  '    disregarding their sizes, even if the remote file has the'+#13+
  '    same size as its local version.'+#13+
  'Only If Size Different'+#13+
  '  - specifies that Net Updater should automatically compare the'+#13+
  '    size of file which is about to be downloaded with the size of'+#13+
  '    the file which already exists on the local hard disk. If the'+#13+
  '    sizes are equal, then Net Updater will not download it and pass'+#13+
  '    to the next file in the download queue.';

  Label31.Hint :=
  'When using an external setup program to install the updates,'+#13+
  'Net Update checks the exit code returned by the external program'+#13+
  'to determine if it completed successfully or not. Typically, a'+#13+
  'value of 0 indicates success. Both Inno Setup and Macrovision'+#13+
  'Installshield use 0 to indicate success.'+#13#13+
  'If you are not sure which value to put here, then leave it at 0.'
end; { SetHints }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.eOptions5KeyPress(Sender: TObject; var Key: Char);
begin
  if not (StrToInt(Key) in [0..9]) then Key := #0;
end; { eOptions5KeyPress }

{ ----------------------------------------------------------------- }

function TfmSFiles.GetFileSize(FileName: String): Integer;
var
  FS: TFileStream;
begin
  //Result := 0;
  FS := nil;
  try
    FS := TFileStream.Create(Filename,fmOpenRead OR fmShareDenyNone);
    Result := FS.Size;
  except
    result := -1;
  end;
  FS.Free;
end; { GetFileSize }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.lvFileSizeClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  OpenDialog1.Filter := 'Executable Files (*.exe)|*.exe|All Files (*.*)|*.*';
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute() then
    eFileSize.Text := FloatToStr(GetFileSize(OpenDialog1.FileName))
  else eFileSize.Text := '0';
end; { lvFileSizeClick }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.HotLabelMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := Label19.Font.Style + [fsUnderline];
end; { HotLabelMouseEnter }

{ ----------------------------------------------------------------- }

procedure TfmSFiles.HotLabelMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := Label19.Font.Style - [fsUnderline];
end; { HotLabelMouseLeave }

{ ----------------------------------------------------------------- }

{ set new hint class here}

var
  OriginalHintClass: THintWindowClass;

function SetHintClass(AClass:THintWindowClass):THintWindowClass;
var
  IsChanged: boolean;
begin
  Result := HintWindowClass;
  IsChanged := False;
  if Application.ShowHint then begin
    Application.ShowHint := False;
    IsChanged := True;
    end;
  HintWindowClass := AClass;
  if IsChanged then
    Application.ShowHint := True;
end;

initialization
  OriginalHintClass := SetHintClass(TXPHintWindow);

finalization
  SetHintClass(OriginalHintClass);

end.


