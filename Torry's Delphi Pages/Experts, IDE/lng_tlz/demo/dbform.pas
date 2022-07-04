unit dbform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, StdCtrls, DBCtrls, smLngCmp, ExtCtrls, ADODB, ActnList,
  Menus;

type
  Tadbform = class(TsmForm)
    DataSource: TDataSource;
    smDBText: TsmDBText;
    DBNavigator: TDBNavigator;
    ADOTable: TADOTable;
    ActionList1: TActionList;
    smLowercase: TsmAction;
    smUppercase: TsmAction;
    PopupMenu: TPopupMenu;
    LOWERCASE1: TMenuItem;
    UPPERCASE1: TMenuItem;
    procedure smLowercaseExecute(Sender: TObject);
    procedure smUppercaseExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AfterConstruction; override;
  end;

var
  adbform: Tadbform;

implementation

{$R *.dfm}

{ Tadbform }

procedure Tadbform.AfterConstruction;
begin
  inherited;
  case Language of
    0: smLowercase.Checked:=true;
    1: smUppercase.Checked:=true;
  end;
end;

procedure Tadbform.smLowercaseExecute(Sender: TObject);
begin
  Language := 0;
  smLowercase.Checked := true;
end;

procedure Tadbform.smUppercaseExecute(Sender: TObject);
begin
  Language := 1;
  smUppercase.Checked := true;
end;

procedure Tadbform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
