{*********************************************************************}
{ Dynamic load library for Loader (http://visualdesigner.fatal.ru/)   }
{            (C) Copyright 2006 SVD, Pfaffenrot S.                    }
{*********************************************************************}

unit UfrmVendors;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, Forms, StdCtrls,
  ExtCtrls, Grids, DBGrids, DB, ActnList, DBFrmActn;

type
  TfrmVendors = class(TForm)
    pnlBottom: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    ActionList1: TActionList;
    DBFormAction1: TDBFormAction;
    Button1: TButton;
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

{ TForm3 }

{Don't remove this declaration}
class function TfrmVendors.CreateNecessary: boolean;
begin
  {If you need do automatical creation for this form  (for examle if this
  form contain some very impotent dataset or database), then you need
  return "True"}
  Result := False;
end;

class function TfrmVendors.GetCaption: string;
begin
  {hier need return one caption for MainMenu}
  Result := 'List of Vendors';
end;

class function TfrmVendors.GetMenuPath: string;
begin
  {hier need return one path in the MainMenu to this Form.
    example: Result := mmJurForms"Jornals Forms".mmSales }
  Result := 'mmJournals"Journal forms".mmListOfVendors';
end;
{-----------------------------------------------------}

{Don't remove this declaration}

initialization

  RegisterClass(TfrmVendors);


end.
