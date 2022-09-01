unit bvConfirmUnit;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  Buttons,ExtCtrls,StdCtrls,
{$else}
  QGraphics,
  QControls,
  QForms,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QDBGrids,
  QMenus,
  Qt,
  QDialogs,
  QComCtrls,
{$endif}

  SysUtils, Classes, bvLocalization ;


type
  TConfirmForm1 = class(TForm)
    BitBtnOk: tBitBtn;
    BitBtnNo: TBitBtn;
    BitBtnIgnore: tBitBtn;
    Bevel1: TBevel;
    Bevel2: TBevel;
    WarningImage: TImage;
    ScrollBox1: TScrollBox;
    LabelText: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


function GetConfirm(Text:String):TModalResult;
function GetConfirmSmall(Text:string):TModalResult;
//function GetRepoConfirm(Text:String;var ThCount:integer):TModalResult;
function GetConfirmAll(Text:String):TModalResult;

var
  ConfirmForm1: TConfirmForm1;

implementation

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}


function GetConfirm(Text:String):TModalResult;
var ConfForm:TConfirmForm1;
begin
  ConfForm:=TConfirmForm1.Create(Application);
  try
    if trim(Text)<>'' then ConfForm.LabelText.Caption:=Text;
    result:=ConfForm.ShowModal;
  finally
     ConfForm.free;
  end;
  Application.ProcessMessages;
end;

function GetConfirmAll(Text:String):TModalResult;
var ConfForm:TConfirmForm1;
begin
  ConfForm:=TConfirmForm1.Create(Application);
  try
    if trim(Text)<>'' then ConfForm.LabelText.Caption:=Text;
    ConfForm.BitBtnIgnore.Caption:=StrYesForAll;
    result:=ConfForm.ShowModal;
  finally
    ConfForm.free;
  end;
  Application.ProcessMessages;
end;

function GetConfirmSmall(Text:String):TModalResult;
var ConfForm:TConfirmForm1;
begin
  ConfForm:=TConfirmForm1.Create(Application);
  try
    if trim(Text)<>'' then ConfForm.LabelText.Caption:=Text;
    ConfForm.clientHeight:=ConfForm.BitBtnIgnore.Top ; //+ConfForm.BitBtnIgnore.Height div 2+ConfForm.Height-ConfForm.ClientHeight;
    Confform.BitBtnIgnore.Visible:=false;
    result:=ConfForm.ShowModal;
  finally
     ConfForm.free;
  end;
  Application.ProcessMessages;
end;

procedure TConfirmForm1.FormCreate(Sender: TObject);
begin
  self.caption:=StrConfirm;
  LabelText.caption:=StrConfirmOperation;
  BitBtnOk.caption:=StrYes;
  BitBtnNo.caption:=StrNo;
  BitbtnIgnore.caption:=StrCancel;
end;

end.
