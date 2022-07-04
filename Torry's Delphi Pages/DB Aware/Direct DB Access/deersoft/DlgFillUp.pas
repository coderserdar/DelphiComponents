unit DlgFillUp;

interface
              
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, Db, Mask, DUtils;

const
  cniToUpper       = 0;
  cniToLower       = 1;
  cniFillConst     = 2;
  cniSQL           = 3;
  cnsFillUpCaption = 'Conversion or fill up the "%s" field';

type
  TFillUpDialog = class(TForm)
    rgpKonvert: TRadioGroup;
    edTolt: TEdit;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    mSQL: TMemo;
    lbUpdate: TLabel;
    lbWhere: TLabel;
    procedure rgpKonvertClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);

  private
    { Private declarations }

    // Set all invisible field
    procedure SetInVisible;

  public
    { Public declarations }
    Field : TField;
  end;

var
  FillUpDialog: TFillUpDialog;

implementation


{$R *.DFM}


// Activation
procedure TFillUpDialog.FormActivate(Sender: TObject);
begin
     Caption := Format('Conversion or fill up the "%s" field', [AnsiUpperCase(Field.DisplayLabel)]);
     lbUpdate.Caption := 'UPDATE ... SET ' + Field.FieldName + ' = ';
     rgpKonvert.ItemIndex := -1;
     SetInVisible;
end;


// Set all invisible field
procedure TFillUpDialog.SetInVisible;
begin
     edTolt.Visible   := False;
     lbUpdate.Visible := False;
     lbWhere.Visible  := False;
     mSQL.Visible     := False;
end;


// Select what want to do
procedure TFillUpDialog.rgpKonvertClick(Sender: TObject);
begin
     SetInVisible;
     case rgpKonvert.ItemIndex of
          cniFillConst : // fill up with constant value
          begin
               edTolt.Visible := True;
               edTolt.SetFocus;
          end;
          cniSQL : // SQL command
          begin
               mSQL.Visible     := True;
               lbUpdate.Visible := True;
               lbWhere.Visible  := True;
               mSQL.Lines.Clear;
               mSQL.SetFocus;
          end;
     end;
end;


end.
  