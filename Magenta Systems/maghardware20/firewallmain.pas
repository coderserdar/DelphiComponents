unit firewallmain;

{
Updated by Angus Robertson, Magenta Systems Ltd, England, 1st February 2022 
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

This unit test the Magenta Firewall Component with functions to search and
list selected Windows Defender Firewall rules and settings, and to add and
remove such rules.

Currently, only rules enabling inbound access for an application with all
protocols, addresses and ports are added, adding more precise rules needs
more parameters to be passed and handled.

WARNING - this unit used COM Automation, so the application should call
CoInitialize(nil); before calling these functions, and CoUninitialize; after,
however Windows 10 seems to work without these...
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, ActiveX, Buttons, IniFiles,
  MagFirewall, magsubs1;

type
  TFormMain = class(TForm)
    RuleName: TEdit;
    RuleGroup: TEdit;
    RuleDir: TRadioGroup;
    RuleFileName: TEdit;
// not saved
    Log: TMemo;
    Panel1: TPanel;
    ListSearch: TEdit;
    doListRules: TButton;
    doClear: TButton;
    doClose: TButton;
    Label1: TLabel;
    doListApps: TButton;
    doListServ: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    doAddRule: TButton;
    doRemRule: TButton;
    LabelAdmin: TLabel;
    OpenDialog: TOpenDialog;
    doOpenFile: TBitBtn;
    procedure doClearClick(Sender: TObject);
    procedure doCloseClick(Sender: TObject);
    procedure doListRulesClick(Sender: TObject);
    procedure doListAppsClick(Sender: TObject);
    procedure doListServClick(Sender: TObject);
    procedure doAddRuleClick(Sender: TObject);
    procedure doRemRuleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure doOpenFileClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;
  FIniFileName: string;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
var
    IniFile: TMemIniFile;
    SectionData: string ;
begin
    CoInitialize(nil);
// get old settings
    FIniFileName := ChangeFileExt (ParamStr (0), '.ini') ;
    IniFile := TMemIniFile.Create(FIniFileName);
    with IniFile do
    begin
        SectionData := 'Main' ;
        RuleName.Text := ReadString (SectionData, 'RuleName_Text', RuleName.Text) ;
        RuleGroup.Text := ReadString (SectionData, 'RuleGroup_Text', RuleGroup.Text) ;
        RuleDir.ItemIndex := ReadInteger (SectionData, 'RuleDir_ItemIndex', RuleDir.ItemIndex) ;
        RuleFileName.Text := ReadString (SectionData, 'RuleFileName_Text', RuleFileName.Text) ;
        Top := ReadInteger ('Window', 'Top', (Screen.Height - Height) div 2);
        Left := ReadInteger ('Window', 'Left', (Screen.Width - Width) div 2);
        Width := ReadInteger ('Window', 'Width', Width);
        Height := ReadInteger ('Window', 'Height', Height);
    end;
    IniFile.Free;
    if IsProgAdmin then
        LabelAdmin.Caption := 'Program has Admin Rights'
    else
        LabelAdmin.Caption := 'Program needs Admin Rights to add rules';    
end;

procedure TFormMain.FormDestroy(Sender: TObject);
var
    IniFile : TMemIniFile;
    SectionData: string ;
begin
    IniFile := TMemIniFile.Create(FIniFileName);
    with IniFile do
    begin
        SectionData := 'Main' ;
        WriteString (SectionData, 'RuleName_Text', RuleName.Text) ;
        WriteString (SectionData, 'RuleGroup_Text', RuleGroup.Text) ;
        WriteInteger (SectionData, 'RuleDir_ItemIndex', RuleDir.ItemIndex) ;
        WriteString (SectionData, 'RuleFileName_Text', RuleFileName.Text) ;
        WriteInteger ('Window', 'Top', Top);
        WriteInteger ('Window', 'Left', Left);
        WriteInteger ('Window', 'Width', Width);
        WriteInteger ('Window', 'Height', Height);
    end ;
    IniFile.UpdateFile;
    IniFile.Free;
    CoUninitialize;
end;

procedure TFormMain.doClearClick(Sender: TObject);
begin
    Log.Lines.Clear;
end;

procedure TFormMain.doCloseClick(Sender: TObject);
begin
    Close;
end;

procedure TFormMain.doListAppsClick(Sender: TObject);
begin
    Log.Lines.Add(MagFireWallAppsEnum(ListSearch.Text, NET_FW_PROFILE_STANDARD));
end;

procedure TFormMain.doListRulesClick(Sender: TObject);
begin
    Log.Lines.Add(MagFireWallRulesEnum(ListSearch.Text));
end;

procedure TFormMain.doListServClick(Sender: TObject);
begin
    Log.Lines.Add(MagFireWallServEnum(ListSearch.Text, NET_FW_PROFILE_STANDARD));

end;

procedure TFormMain.doOpenFileClick(Sender: TObject);
begin
    OpenDialog.FileName := RuleFileName.Text;
    if OpenDialog.Execute(Handle) then
        RuleFileName.Text := OpenDialog.FileName;
end;

procedure TFormMain.doAddRuleClick(Sender: TObject);
begin
    Log.Lines.Add('Adding New Rule');
    Log.Lines.Add(MagFireWallRulesAdd(RuleName.Text, RuleGroup.Text,
             RuleName.Text, RuleFileName.Text, TFirewallDir(RuleDir.ItemIndex)));
end;

procedure TFormMain.doRemRuleClick(Sender: TObject);
begin
    Log.Lines.Add('Removing Rule');
    Log.Lines.Add(MagFireWallRulesAdd(RuleName.Text, RuleGroup.Text,
                            RuleName.Text, '', TFirewallDir(RuleDir.ItemIndex)));
end;


end.
