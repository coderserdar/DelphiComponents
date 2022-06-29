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

unit dlgFieldsUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB;

type
  TdlgFields = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    Label1: TLabel;
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function Execute(ADataSet: TDataSet; var AField: string): Boolean;
  end;

var
  dlgFields: TdlgFields;

implementation

{$R *.dfm}

{ TdlgFields }

function TdlgFields.Execute(ADataSet: TDataSet;
  var AField: string): Boolean;
var
  LI: Integer;
begin
  ListBox1.Items.Clear;
  for LI := 0 to ADataSet.FieldCount - 1 do
    ListBox1.Items.Add(ADataSet.Fields[LI].FieldName);
  Button1.Enabled := ListBox1.ItemIndex <> -1;
  Result := ShowModal = mrOk;
  if Result then AField := ListBox1.Items[ListBox1.ItemIndex];
end;

procedure TdlgFields.ListBox1Click(Sender: TObject);
begin
  Button1.Enabled := ListBox1.ItemIndex <> -1;
end;

procedure TdlgFields.ListBox1DblClick(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    ModalResult := mrOk;
end;

end.
