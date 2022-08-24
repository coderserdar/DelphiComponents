unit Main;

interface

uses
  SysUtils, LResources, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TTfrmPerf = class(TForm)
    lbCount: TLabel;
    edCount: TEdit;
    lbInfo: TLabel;
    sbBtns: TScrollBox;
    sbCreate: TButton;
    sbLoad: TButton;
    sbApply: TButton;
    lbCreateCap: TLabel;
    lbCreate: TLabel;
    lbLoadCap: TLabel;
    lbLoad: TLabel;
    lbLangVCLCap: TLabel;
    lbLangVCL: TLabel;
    procedure sbCreateClick(Sender: TObject);
    procedure sbLoadClick(Sender: TObject);
    procedure sbApplyClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TfrmPerf: TTfrmPerf;

implementation

uses
  plsLangMan, IniFiles;

procedure TTfrmPerf.sbCreateClick(Sender: TObject);
var
  i,n:Integer;
  B:TButton;
  Ini:TMemIniFile;
  dt:TDateTime;
begin
  dt:=now;
  try
    sbBtns.Visible:=False;
    ForceDirectories(LanguageManager.Folder);
    Ini:=TMemIniFile.Create(IncludeTrailingPathDelimiter(LanguageManager.Folder)+'Temp.en.lng');
    try
      Ini.Clear;
      Ini.writestring('precision language file','languagename','English');
      for i:=sbBtns.ControlCount-1 downto 0 do
        sbBtns.Controls[i].Free;
      n:=strtointdef(edCount.Text,1);
      for i:=1 To n do
      begin
        B:=TButton.Create(self);
        B.Parent:=sbBtns;
        B.Name:='B_'+IntToStr(i);
        B.Caption:=B.Name;
        B.Left:=8;
        B.Top:=8+(i-1)*29;
        Ini.writestring(Self.ClassName,B.Name+'.Caption','Button '+IntToStr(i));
      end;
      Ini.UpdateFile;
    finally
      Ini.Free;
      sbBtns.Visible:=True;
    end;
  except
    on E:Exception do
      ShowMessage(e.message);
  end;
  lbCreate.Caption:=FormatDateTime('nn:ss,zz',now-dt);
end;

procedure TTfrmPerf.sbLoadClick(Sender: TObject);
var
  dt:TDateTime;
begin
  dt:=now;
  LanguageManager.LanguageCode:='en';
  lbLoad.Caption:=FormatDateTime('nn:ss,zz',now-dt);
end;

procedure TTfrmPerf.sbApplyClick(Sender: TObject);
var
  dt:TDateTime;
begin
  dt:=now;
  LanguageManager.LangVCL(Self);
  lbLangVCL.Caption:=FormatDateTime('nn:ss,zz',now-dt);
end;

initialization
  {$I Main.lrs}

end.
