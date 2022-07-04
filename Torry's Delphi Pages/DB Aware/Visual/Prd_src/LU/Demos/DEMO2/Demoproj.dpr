program Demoproj;

uses
  Forms,
  Demomain in 'DEMOMAIN.PAS' {Form1},
  Subcat in 'SUBCAT.PAS' {SubCatDlg};

{$R *.RES}

begin
  Application.Title := 'TDBLookUpComboPlus Demo ';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TSubCatDlg, SubCatDlg);
  Application.Run;
end.
