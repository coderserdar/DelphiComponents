unit DlgSelFld;

interface                           

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, Buttons, ExtCtrls;
  
type
  TSelFldDialog = class(TForm)
    Bevel1: TBevel;
    btnMarkON: TSpeedButton;
    btnMarkOFF: TSpeedButton;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    clbSelField: TCheckListBox;
    procedure MarkONClick(Sender: TObject);
    procedure MarkOFFClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FFields : TStrings;

    procedure SetFields(Value: TStrings);

  public
    { Public declarations }
    property Fields : TStrings read FFields write SetFields;

  end;

var
  SelFldDialog: TSelFldDialog;

implementation

{$R *.DFM}


procedure TSelFldDialog.SetFields(Value: TStrings);
begin
     FFields.Assign(Value);
end;


procedure TSelFldDialog.MarkONClick(Sender: TObject);
var
   i : Integer;
begin
     for i := 0 to clbSelField.Items.Count-1 do
     begin
          clbSelField.Checked[i] := True;
     end;
end;


procedure TSelFldDialog.MarkOFFClick(Sender: TObject);
var
   i : Integer;
begin
     for i := 0 to clbSelField.Items.Count-1 do
     begin
          clbSelField.Checked[i] := False;
     end;
end;


procedure TSelFldDialog.FormCreate(Sender: TObject);
begin
     FFields := TStringList.Create;
end;


procedure TSelFldDialog.FormDestroy(Sender: TObject);
begin
     FFields.Free;
     FFields := nil;
end;


end.
  