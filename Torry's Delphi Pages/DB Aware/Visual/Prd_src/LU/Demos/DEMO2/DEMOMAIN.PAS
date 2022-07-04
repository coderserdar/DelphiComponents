unit Demomain;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Mask, DB, DBTables, TabNotBk, Dblup2,
  StdCtrls, DBCtrls, SubCat, DBLookup, Grids, DBGrids, ComCtrls;

type
  TForm1 = class(TForm)
    TabbedNotebook1: TTabbedNotebook;
    TableRest: TTable;
    DataSourceRest: TDataSource;
    TableRestName: TStringField;
    TableRestSubCat: TSmallintField;
    TableRestLocation: TStringField;
    TableRestPhone: TStringField;

    TableLocation: TTable;
    DataSourceLocation: TDataSource;
    TableLocationLocation: TStringField;

    TableSubCat: TTable;
    DataSourceSubCat: TDataSource;

    DBEditName: TDBEdit;
    ComboPlusSubCat: TDBLookupComboPlus;
    DBEditPhone: TDBEdit;
    ComboPlusLocation: TDBLookupComboPlus;
    Navigator1: TDBNavigator;
    Name: TLabel;
    Style: TLabel;
    Phone: TLabel;
    Location: TLabel;

    DBEditName2: TDBEdit;
    ComboPlusSubCat2: TDBLookupComboPlus;
    DBEditPhone2: TDBEdit;
    ComboPlusLocation2: TDBLookupComboPlus;
    DBNavigator1: TDBNavigator;
    Name2: TLabel;
    Style2: TLabel;
    Phone2: TLabel;
    Location2: TLabel;

    DBEditName3: TDBEdit;
    ComboPlusSubCat3: TDBLookupComboPlus;
    DBEditPhone3: TDBEdit;
    ComboPlusLocation3: TDBLookupComboPlus;
    DBNavigator2: TDBNavigator;
    Name3: TLabel;
    Style3: TLabel;
    Phone3: TLabel;
    Location3: TLabel;

    DBEditName4: TDBEdit;
    ComboPlusSubCat4: TDBLookupComboPlus;
    DBEditPhone4: TDBEdit;
    ComboPlusLocation4: TDBLookupComboPlus;
    DBNavigator3: TDBNavigator;
    Name4: TLabel;
    Style4: TLabel;
    Phone4: TLabel;
    Location4: TLabel;
    Panel1: TPanel;
    RadioGroupAutoDropDown: TRadioGroup;
    RadioGroupSpeedButton: TRadioGroup;
    RadioGroupBorderStyle: TRadioGroup;
    RadioGroupDropDownAlign: TRadioGroup;
    RadioGroupDropDownTop: TRadioGroup;
    RadioGroupCtrl3D: TRadioGroup;
    TableSubCatSubCatName: TStringField;
    TableSubCatSubCatNum: TSmallintField;

    procedure FormCreate(Sender: TObject);
    procedure ComboPlusSubCat4NewLookupRec(Sender: TObject;
      var Cancelled: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure RadioGroupAutoDropDownClick(Sender: TObject);
    procedure RadioGroupBorderStyleClick(Sender: TObject);
    procedure RadioGroupCtrl3DClick(Sender: TObject);
    procedure RadioGroupDropDownAlignClick(Sender: TObject);
    procedure RadioGroupDropDownTopClick(Sender: TObject);
    procedure RadioGroupSpeedButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormActivate(Sender: TObject);
begin
  TabbedNotebook1.PageIndex := 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ComboPlusSubCat.IgnorLUIndexErr := True;
  ComboPlusSubCat2.IgnorLUIndexErr := True;
  ComboPlusSubCat3.IgnorLUIndexErr := True;
  ComboPlusSubCat4.IgnorLUIndexErr := True;
end;

procedure TForm1.ComboPlusSubCat4NewLookupRec(Sender: TObject;
  var Cancelled: Boolean);
begin
  Cancelled := False;                 {Set cancelled to false initially}
  TableSubCat.Insert;                 {Start a new SubCat record}
  TableSubCat.FieldByName('SubCatName').AsString := {and set the new name value}
    ComboPlusSubCat4.DisplayValue;
  SubCatDlg.ShowModal;                { display the dialog box }
  if SubCatDlg.ModalResult=mrOK then  {if user pressed OK then save the new SubCat}
  begin
    TableSubCat.Post;                 {if user said oK then post}
    {*** VERY IMPORTANT*** Now Update the Compbo's value property.
     The Combo component doesn't know anything about the table that
     the SubCatDlg box uses so you must tell the combo what the
     SubCat number is. This will need to be done in any case where
     the lookup field is different than the display field.}
    ComboPlusSubCat4.Value := TableSubCat.FieldByName('SubCatNum').AsString;
  end
  else
  begin
    TableSubCat.Cancel;
    Cancelled := True;
  end;
  ComboPlusSubCat4.SetFocus;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  If ComboPlusSubCat4.ShowSpeedButton then
    ComboPlusSubCat4.ShowSpeedButton := False
  else
    ComboPlusSubCat4.ShowSpeedButton := True;
end;

procedure TForm1.RadioGroupAutoDropDownClick(Sender: TObject);
begin
   case  RadioGroupAutoDropDown.ItemIndex of
    0 : begin
          ComboPlusSubCat.AutoDropDown := True;
          ComboPlusLocation.AutoDropDown := True;
          ComboPlusSubCat2.AutoDropDown := True;
          ComboPlusLocation2.AutoDropDown := True;
          ComboPlusSubCat3.AutoDropDown := True;
          ComboPlusLocation3.AutoDropDown := True;
          ComboPlusSubCat4.AutoDropDown := True;
          ComboPlusLocation4.AutoDropDown := True;
        end;
    1 : begin
          ComboPlusSubCat.AutoDropDown := False;
          ComboPlusLocation.AutoDropDown := False;
          ComboPlusSubCat2.AutoDropDown := False;
          ComboPlusLocation2.AutoDropDown := False;
          ComboPlusSubCat3.AutoDropDown := False;
          ComboPlusLocation3.AutoDropDown := False;
          ComboPlusSubCat4.AutoDropDown := False;
          ComboPlusLocation4.AutoDropDown := False;
        end;
  end; {Case}

end;

procedure TForm1.RadioGroupBorderStyleClick(Sender: TObject);
begin
  case  RadioGroupBorderStyle.ItemIndex of
    0 : begin
          ComboPlusSubCat.BorderStyle := bsNone;
          ComboPlusLocation.BorderStyle := bsNone;
          ComboPlusSubCat2.BorderStyle := bsNone;
          ComboPlusLocation2.BorderStyle := bsNone;
          ComboPlusSubCat3.BorderStyle := bsNone;
          ComboPlusLocation3.BorderStyle := bsNone;
          ComboPlusSubCat4.BorderStyle := bsNone;
          ComboPlusLocation4.BorderStyle := bsNone;
        end;
    1 : Begin
          ComboPlusSubCat.BorderStyle := bsSingle;
          ComboPlusLocation.BorderStyle := bsSingle;
          ComboPlusSubCat2.BorderStyle := bsSingle;
          ComboPlusLocation2.BorderStyle := bsSingle;
          ComboPlusSubCat3.BorderStyle := bsSingle;
          ComboPlusLocation3.BorderStyle := bsSingle;
          ComboPlusSubCat4.BorderStyle := bsSingle;
          ComboPlusLocation4.BorderStyle := bsSingle;
        end;
  end; {Case}

end;

procedure TForm1.RadioGroupCtrl3DClick(Sender: TObject);
begin
  case  RadioGroupCtrl3D.ItemIndex of
    0 : begin
          ComboPlusSubCat.Ctl3D := True;
          ComboPlusLocation.Ctl3D := True;
          ComboPlusSubCat2.Ctl3D := True;
          ComboPlusLocation2.Ctl3D := True;
          ComboPlusSubCat3.Ctl3D := True;
          ComboPlusLocation3.Ctl3D := True;
          ComboPlusSubCat4.Ctl3D := True;
          ComboPlusLocation4.Ctl3D := True;
        end;
    1 : Begin
          ComboPlusSubCat.Ctl3D := False;
          ComboPlusLocation.Ctl3D := False;
          ComboPlusSubCat2.Ctl3D := False;
          ComboPlusLocation2.Ctl3D := False;
          ComboPlusSubCat3.Ctl3D := False;
          ComboPlusLocation3.Ctl3D := False;
          ComboPlusSubCat4.Ctl3D := False;
          ComboPlusLocation4.Ctl3D := False;
        end;
   end;
end;

procedure TForm1.RadioGroupDropDownAlignClick(Sender: TObject);
begin
  case  RadioGroupDropDownAlign.ItemIndex of
    0 : begin
          ComboPlusSubCat.DropDownAlign := Dblup2.Left;
          ComboPlusLocation.DropDownAlign := Dblup2.Left;
          ComboPlusSubCat2.DropDownAlign := Dblup2.Left;
          ComboPlusLocation2.DropDownAlign := Dblup2.Left;
          ComboPlusSubCat3.DropDownAlign := Dblup2.Left;
          ComboPlusLocation3.DropDownAlign := Dblup2.Left;
          ComboPlusSubCat4.DropDownAlign := Dblup2.Left;
          ComboPlusLocation4.DropDownAlign := Dblup2.Left;
        end;
    1 : begin
          ComboPlusSubCat.DropDownAlign := Dblup2.Right;
          ComboPlusLocation.DropDownAlign := Dblup2.Right;
          ComboPlusSubCat2.DropDownAlign := Dblup2.Right;
          ComboPlusLocation2.DropDownAlign := Dblup2.Right;
          ComboPlusSubCat3.DropDownAlign := Dblup2.Right;
          ComboPlusLocation3.DropDownAlign := Dblup2.Right;
          ComboPlusSubCat4.DropDownAlign := Dblup2.Right;
          ComboPlusLocation4.DropDownAlign := Dblup2.Right;
        end;
  end; {Case}

end;

procedure TForm1.RadioGroupDropDownTopClick(Sender: TObject);
begin
  case  RadioGroupDropDownTop.ItemIndex of
    0 : begin
          ComboPlusSubCat.DropDownTop := Dblup2.Above;
          ComboPlusLocation.DropDownTop := Dblup2.Above;
          ComboPlusSubCat2.DropDownTop := Dblup2.Above;
          ComboPlusLocation2.DropDownTop := Dblup2.Above;
          ComboPlusSubCat3.DropDownTop := Dblup2.Above;
          ComboPlusLocation3.DropDownTop := Dblup2.Above;
          ComboPlusSubCat4.DropDownTop := Dblup2.Above;
          ComboPlusLocation4.DropDownTop := Dblup2.Above;
        end;
    1 : begin
          ComboPlusSubCat.DropDownTop := Dblup2.Below;
          ComboPlusLocation.DropDownTop := Dblup2.Below;
          ComboPlusSubCat2.DropDownTop := Dblup2.Below;
          ComboPlusLocation2.DropDownTop := Dblup2.Below;
          ComboPlusSubCat3.DropDownTop := Dblup2.Below;
          ComboPlusLocation3.DropDownTop := Dblup2.Below;
          ComboPlusSubCat4.DropDownTop := Dblup2.Below;
          ComboPlusLocation4.DropDownTop := Dblup2.Below;
        end;
  end; {Case}

end;

procedure TForm1.RadioGroupSpeedButtonClick(Sender: TObject);
begin
  case  RadioGroupSpeedButton.ItemIndex of
    0 : begin
          ComboPlusSubCat.ShowSpeedButton := True;
          ComboPlusLocation.ShowSpeedButton := True;
          ComboPlusSubCat2.ShowSpeedButton := True;
          ComboPlusLocation2.ShowSpeedButton := True;
          ComboPlusSubCat3.ShowSpeedButton := True;
          ComboPlusLocation3.ShowSpeedButton := True;
          ComboPlusSubCat4.ShowSpeedButton := True;
          ComboPlusLocation4.ShowSpeedButton := True;
        end;
    1 : begin
          ComboPlusSubCat.ShowSpeedButton := False;
          ComboPlusLocation.ShowSpeedButton := False;
          ComboPlusSubCat2.ShowSpeedButton := False;
          ComboPlusLocation2.ShowSpeedButton := False;
          ComboPlusSubCat3.ShowSpeedButton := False;
          ComboPlusLocation3.ShowSpeedButton := False;
          ComboPlusSubCat4.ShowSpeedButton := False;
          ComboPlusLocation4.ShowSpeedButton := False;
        end;
  end; {Case}

end;

end.

