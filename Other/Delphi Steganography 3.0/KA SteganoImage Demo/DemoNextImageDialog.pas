unit DemoNextImageDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtDlgs, StdCtrls;

type
  TNextImageForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    function GetFileName: String;
    { Private declarations }
  public
    { Public declarations }
    Dialog   : TOpenPictureDialog;
    Function Execute:Boolean;
    Property FileName : String read GetFileName;
  end;

var
  NextImageForm: TNextImageForm;

implementation

{$R *.dfm}

{ TNextImageForm }

function TNextImageForm.Execute: Boolean;
begin
  ShowModal;
  Result := ModalResult=mrOK;
end;

procedure TNextImageForm.Button2Click(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TNextImageForm.Button3Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TNextImageForm.Button1Click(Sender: TObject);
begin
  if Dialog.Execute Then
     Begin
       Edit1.Text := Dialog.FileName;
     End;
end;


function TNextImageForm.GetFileName: String;
begin
  Result := Edit1.Text;
end;


end.
