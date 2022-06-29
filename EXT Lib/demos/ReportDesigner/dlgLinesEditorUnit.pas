{**********************************************************}
{                                                          }
{  Report Designer                                         }
{  Devrace Extension Library example of                    }
{  TELDesigner, TELDesignPanel                             }
{                                                          }
{  Copyright (c) 2001 - 2002, Balabuyev Yevgeny            }
{  Contact: yebalabuyev@devrace.com                        }
{                                                          }
{ -------------------------------------------------------- }
{  ExtLib home page      : http://www.devrace.com/extlib   }
{  ExtLib support e-mail : extlib@devrace.com              }
{ -------------------------------------------------------- }
{                                                          }
{  Please see the file License.txt for full license        }
{  information                                             }
{                                                          }
{**********************************************************}

unit dlgLinesEditorUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DB;

type
  TdlgLinesEditor = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Button3: TButton;
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FDataSet: TDataSet;
  public
    { Public declarations }
    function Execute(ALines: TStrings; ADataSet: TDataSet): Boolean; overload;
    function Execute(var AStr: string; ADataSet: TDataSet): Boolean; overload;
  end;

var
  dlgLinesEditor: TdlgLinesEditor;

implementation

uses dlgFieldsUnit;

{$R *.dfm}

{ TdlgLinesEditor }

function TdlgLinesEditor.Execute(ALines: TStrings; ADataSet: TDataSet): Boolean;
begin
  FDataSet := ADataSet;
  Button3.Enabled := ADataSet <> nil;
  Memo1.Lines.Assign(ALines);
  Result := ShowModal = mrOk;
  if Result then ALines.Assign(Memo1.Lines);
end;

function TdlgLinesEditor.Execute(var AStr: string; ADataSet: TDataSet): Boolean;
begin
  FDataSet := ADataSet;
  Button3.Enabled := ADataSet <> nil;
  Memo1.Lines.Text := AStr;
  Result := ShowModal = mrOk;
  if Result then AStr := Memo1.Lines.Text;
end;

procedure TdlgLinesEditor.Button3Click(Sender: TObject);
var
  LS: string;
  LSelStart, LSelLength: Integer;
begin
  if dlgFields.Execute(FDataSet, LS) then
  begin
    LSelStart := Memo1.SelStart;
    LSelLength := Memo1.SelLength;
    Memo1.Lines.Text := Copy(Memo1.Lines.Text, 1, LSelStart) + LS +
      Copy(Memo1.Lines.Text, LSelStart + LSelLength + 1, MaxInt);

    Memo1.SelLength := Length(LS);
  end;
end;

end.
