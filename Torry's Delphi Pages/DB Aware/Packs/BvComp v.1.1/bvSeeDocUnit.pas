unit bvSeeDocUnit;

interface

{$ifdef LINUX}
 ERROR: not compatible with LINUX
{$endif}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls, Mask, bvFormSaver,
  bvLocalization;

type
  TSeeDocForm = class(TForm)
    Panel1: TPanel;
    SpButtonOpen: TSpeedButton;
    SpButtonPrint: TSpeedButton;
    EditName: TEdit;
    SpeedButtonSave: TSpeedButton;
    LabelFile: TLabel;
    PrintDialog: TPrintDialog;
    Editor: TRichEdit;
    Panel4: TPanel;
    SpeedButtonClose: TSpeedButton;
    StatusBar1: TStatusBar;
    SpeedButtonClear: TSpeedButton;
    SpeedButton3: TSpeedButton;
    OpenDialog1: TOpenDialog;
    Bevel1: TBevel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpButtonOpenClick(Sender: TObject);
    procedure SpButtonPrintClick(Sender: TObject);
    procedure SpeedButtonSaveClick(Sender: TObject);
    procedure SpeedButtonCloseClick(Sender: TObject);
    procedure SpeedButtonClearClick(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    function GetFileName:string;
    procedure SetFileName(Value:string);
  public
    { Public declarations }
    property FileName:string read GetFileName write SetFileName;

  end;

var
  SeeDocForm: TSeeDocForm;

implementation

uses bvStringUtils,Printers, bvMessageUnit, bvConfirmUnit;

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}

procedure TSeeDocForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action:=caFree;
end;

function TSeeDocForm.GetFileName:string;
begin
  Result:=Deletequoted(EditName.Text);
end;

procedure TSeeDocForm.SetFileName(Value:string);
begin
  EditName.Text:=Value;
//  SpButtonopen.Click;
end;

procedure TSeeDocForm.SpButtonOpenClick(Sender: TObject);
begin
  if not FileExists(FileName) then begin
    bvMessageError(StrErrorNotFoundFile+' '+FileName+#13+
               StrCheckPath);
  end
  else  begin
     Editor.Lines.Clear;
     Editor.Lines.LoadFromFile(FileName);
     Editor.SelStart:=length(Editor.Lines.Text);
     if Self.Visible then Self.Refresh;
  end;
end;

procedure TSeeDocForm.SpButtonPrintClick(Sender: TObject);
var i:integer;
    y:integer;
    ThText:string;
begin
   y:=10;
   if PrintDialog.Execute then with Printer do begin
     BeginDoc;
     ThText:=StrPrintFile+': '+FileName;
     Canvas.TextOut(10,y,ThText);
     y:=y+canvas.TextHeight(ThText)+3;
     for i:=0 to Editor.Lines.Count-1 do begin
       ThText:=Editor.Lines.Strings[i];
       Canvas.TextOut(10,y,ThText);
       y:=y+canvas.TextHeight(ThText)+3;
//       Editor. Print('Печать файла: '+FileName);
     end;
     enddoc;
   end;
end;


procedure TSeeDocForm.SpeedButtonSaveClick(Sender: TObject);
begin
  if not FileExists(FileName)
     or (GetConfirmSmall(StrConfirmReplaceFile)=mrOk)
  then begin
    if Editor.Lines.Count=0 then  DeleteFile(FileName)
//      Editor.Lines.Clear;
    else Editor.Lines.SaveToFile(FileName);
  end;
end;


procedure TSeeDocForm.SpeedButtonCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TSeeDocForm.SpeedButtonClearClick(Sender: TObject);
begin
  if (GetConfirmSmall(StrConfirmClearFile)=mrOk)
  then begin
    Editor.Clear;
    if Fileexists(FileName) then  DeleteFile(FileName);
  end;
end;

procedure TSeeDocForm.SpeedButton3Click(Sender: TObject);
begin
  OpenDialog1.Filename:=EditName.text;
  if OpenDialog1.execute then EditName.Text:=OpenDialog1.filename;
end;

procedure TSeeDocForm.FormCreate(Sender: TObject);
begin
  self.caption:=strSeeDoc;
  LabelFile.caption:=StrFile;
  SpeedButtonClose.hint:=StrClose;
  SpButtonOpen.hint:=StrOpen;
  SpeedButtonclear.hint:=StrClear;
  SpeedButtonSave.hint:=StrSave;
  SpButtonPrint.hint:=strPrint;
  restoreform(self);
end;

procedure TSeeDocForm.FormDestroy(Sender: TObject);
begin
  saveform(self);
end;

end.
