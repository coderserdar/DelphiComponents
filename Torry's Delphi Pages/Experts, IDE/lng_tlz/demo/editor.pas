unit editor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, smLngCmp, ActnList, Menus;

type
  TeditorForm = class(TsmForm)
    smRichEdit: TsmRichEdit;
    ActionList: TActionList;
    PopupMenu: TPopupMenu;
    smLowercase: TsmAction;
    smUppercase: TsmAction;
    LOWERCASE1: TMenuItem;
    UPPERCASE1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure smLowercaseExecute(Sender: TObject);
    procedure smUppercaseExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AfterConstruction; override;
  end;

var
  editorForm: TeditorForm;

implementation
uses smLanguage;

{$R *.dfm}

procedure TeditorForm.AfterConstruction;
begin
  inherited;
  case Language of
    0: smLowercase.Checked:=true;
    1: smUppercase.Checked:=true;
  end;
end;

procedure TeditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TeditorForm.smLowercaseExecute(Sender: TObject);
begin
  Language := 0;
  smLowercase.Checked := true;
end;

procedure TeditorForm.smUppercaseExecute(Sender: TObject);
begin
  Language := 1;
  smUppercase.Checked := true;
end;

end.
