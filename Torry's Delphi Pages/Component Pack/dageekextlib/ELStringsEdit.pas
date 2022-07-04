{*******************************************************}
{                                                       }
{       Extension Library                               }
{       Strin List Editor dialog Unit                   }
{                                                       }
{       (c) 2002, Balabuyev Yevgeny                     }
{       E-mail: stalcer@rambler.ru                      }
{                                                       }
{*******************************************************}

unit ELStringsEdit;

interface

uses
  Windows, Messages, SysUtils {$IFDEF VER140} , Variants {$ENDIF} , Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls;

var
  SELStringsEditorDlgCaption: string              = 'String List Editor';
  SELStringsEditorDlgOkBtnCaption: string         = '&Ok';
  SELStringsEditorDlgCancelBtnCaption: string     = 'Cancel';
  SELStringsEditorDlgLinesCountTemplate: string   = '%d lines';

type
  TELStringsEditorDlg = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    lbLineCount: TLabel;
    bvlMain: TBevel;
    memMain: TRichEdit;
    procedure memMainChange(Sender: TObject);
  private
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
  public
    function Execute: Boolean;
    property Lines: TStrings read GetLines write SetLines;
  end;

implementation

{$R *.dfm}

{ TELStringsEditorDlg }

function TELStringsEditorDlg.Execute: Boolean;
begin
  Caption := SELStringsEditorDlgCaption;
  btnOk.Caption := SELStringsEditorDlgOkBtnCaption;
  btnCancel.Caption := SELStringsEditorDlgCancelBtnCaption;
  lbLineCount.Caption := Format(SELStringsEditorDlgLinesCountTemplate,
    [memMain.Lines.Count]);

  Result := (ShowModal = mrOk);
end;

function TELStringsEditorDlg.GetLines: TStrings;
begin
  Result := memMain.Lines;
end;

procedure TELStringsEditorDlg.SetLines(const Value: TStrings);
begin
  memMain.Lines := Value;
end;

procedure TELStringsEditorDlg.memMainChange(Sender: TObject);
begin
  lbLineCount.Caption := Format(SELStringsEditorDlgLinesCountTemplate,
    [memMain.Lines.Count]);
end;

end.
