{*********************************************************************}
{ Dynamic load library for Loader (http://visualdesigner.fatal.ru/)   }
{            (C) Copyright 2006 SVD, Pfaffenrot S.                    }
{*********************************************************************}

unit UfrmCountry;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, Forms, StdCtrls,
  ExtCtrls, Grids, DBGrids, DB;

type
  TfrmCountry = class(TForm)
    pnlBottom: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
  published
    {Don't remove this declaration}
    class function GetMenuPath: string;
    class function GetCaption: string;
    class function CreateNecessary: boolean;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TForm2 }

{Don't remove this declaration}
class function TfrmCountry.CreateNecessary: boolean;
begin
  {If you need do automatical creation for this form  (for examle if this
  form contain some very impotent dataset or database), then you need
  return "True"}
  Result := False;
end;

class function TfrmCountry.GetCaption: string;
begin
  {hier need return one caption for MainMenu}
  Result := 'Country';
end;

class function TfrmCountry.GetMenuPath: string;
begin
  {hier need return one path in the MainMenu to this Form.
    example: Result := mmJurForms"Jornals Forms".mmSales }
  Result := 'mmAux"Auxiliary forms".mmCountry';
end;
{-----------------------------------------------------}

{Don't remove this declaration}

initialization

  RegisterClass(TfrmCountry);


end.
