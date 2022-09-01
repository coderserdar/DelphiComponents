unit ColorEditUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, bvColorEdit, bvFormSaver;

type
  TColorEditForm = class(TForm)
    Editor1: TbvColorEdit;
    Label1: TLabel;
    Bevel1: TBevel;
    Editor2: TbvColorEdit;
    Shape: TShape;
    bvFormSaver1: TbvFormSaver;
    procedure Editor1ChangeColor(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ColorEditForm: TColorEditForm;

implementation

{$R *.DFM}

procedure TColorEditForm.Editor1ChangeColor(Sender: TObject);
begin
   Shape.Brush.color:=(sender as tbvColorEdit).colorValue;

   if Editor1.colorValue<>Shape.Brush.color
   then Editor1.colorValue:=Shape.brush.color;

   if Editor2.colorValue<>Shape.Brush.color
   then Editor2.colorValue:=Shape.brush.color;
end;

procedure TColorEditForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=caFree
end;

end.
