{-----------------------------------------------------------------------------
  Precision Language Suite for VCL

  written by Precision software & consulting
            copyright ©  2008 - 2010
            Email : info@be-precision.com
            Web : http://www.be-precision.com

  Purpose: Main unit for Localize from scratch demo

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  This unit can be freely used in any application. The complete
  source code remains property of the author and may not be distributed,
  published, given or sold in any form as such. No parts of the source
  code can be included in any other component or application without
  written authorization of the author.
------------------------------------------------------------------------------}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, plsLangMan, StdCtrls, ExtCtrls, Menus, ComCtrls;

type
  TForm1 = class(TForm)
    lbField1: TLabel;
    pumLanguages: TPopupMenu;
    lbLabelName: TLabel;
    lbName: TLabel;
    sbMsgWarn: TButton;
    sbMsgInfo: TButton;
    sbProperty: TButton;
    mmMain: TMainMenu;
    miLanguage: TMenuItem;
    miFile: TMenuItem;
    N1: TMenuItem;
    miExit: TMenuItem;
    mTest: TMemo;
    lvTest: TListView;
    sbChildWindow: TButton;
    edField2: TEdit;
    lbField2: TLabel;
    sbMessage1: TButton;
    sbMessage2: TButton;
    lbInfo: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure sbChildWindowClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Child, plsEmbedded;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  { pls_Editor_IncludeConstants:=True; // uncomment this line, if you want to list the constants in embedded editor
                                       // see also LanguageChanged method - LanguageManager.Clear(...) }
  LE_InstallCaptureControlShortcut(LanguageManager,TextToShortcut('Alt+F2'));
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  LE_RemoveCaptureControlShortcut;
end;

procedure TForm1.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.sbChildWindowClick(Sender: TObject);
begin
  TfrmChild.Create(nil).Show;
end;

end.
