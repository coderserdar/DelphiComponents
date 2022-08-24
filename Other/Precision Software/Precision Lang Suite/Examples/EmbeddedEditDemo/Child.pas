{-----------------------------------------------------------------------------
  Precision Language Suite for VCL

  written by Precision software & consulting
            copyright ©  2008 - 2010
            Email : info@be-precision.com
            Web : http://www.be-precision.com

  Purpose: Child form unit, see WMLanguageChanged handler

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  This unit can be freely used in any application. The complete
  source code remains property of the author and may not be distributed,
  published, given or sold in any form as such. No parts of the source
  code can be included in any other component or application without
  written authorization of the author.
------------------------------------------------------------------------------}

unit Child;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  plsLangMan;

type
  TfrmChild = class(TForm)
    lbTest: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure WMLanguageChanged(var Message: TMessage); message WM_LANGUAGECHANGED;
    procedure LanguageChanged(Sender:TObject);
  public
    { Public declarations }
  end;

var
  frmChild: TfrmChild;

implementation

uses
  plsDialogs;

{$R *.dfm}

procedure TfrmChild.WMLanguageChanged(var Message: TMessage);
begin
  LanguageChanged(Self);  // perform language changed event when language is changed by the user
  Message.Result := 0;
end;

procedure TfrmChild.FormCreate(Sender: TObject);
begin
  LanguageChanged(Self);  // perform language changed event for newly created form
end;

procedure TfrmChild.LanguageChanged(Sender:TObject);
begin
  if not LanguageManager.LangVCL(Self) then
    MessageDlgLM(Caption,LanguageManager.LanguageName+' ('+LanguageManager.LanguageCode+'): ['+Self.Name+' ('+Self.ClassName+')] '+LanguageManager.LastError,
      mtWarning,[mbOK]);
end;

procedure TfrmChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
end;

end.
