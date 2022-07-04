unit ItemPropsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmItemProps = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
  private
    function GetLines: TStrings;
    { Private declarations }
  public
    { Public declarations }
    function Execute: Boolean;
    property Lines: TStrings read GetLines;
  end;

implementation

{$R *.dfm}

{ TdlgItemProps }

function TfrmItemProps.Execute: Boolean;
begin
  Result := (ShowModal = mrOk);
end;

function TfrmItemProps.GetLines: TStrings;
begin
  Result := Memo1.Lines;
end;

end.
