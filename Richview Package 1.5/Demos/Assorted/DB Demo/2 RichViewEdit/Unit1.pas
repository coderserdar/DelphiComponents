{=============================} unit Unit1; {==================================}
{ Main properties were set at design time:                                     }
{ Table1.TableName           := 'SampleTable.db'; // Paradox table             }
{ Table1.ReadOnly            := False;                                         }
{ DataSource1.Dataset        := Table1;                                        }
{ DBNavigator1.DataSource    := DataSource1;                                   }
{ DBEdit1.DataSource         := DataSource1;                                   }
{ DBRichView1.DataSource     := DataSource1;                                   }
{ DBRichView1.Style          := RVStyle1;                                      }
{ DBEdit1.DataField          := 'Caption';  // Alphanumeric field              }
{ DBRichViewEdit1.DataField  := 'RVFField'; // Binary field                    }
{ nbEdit was removed from DBNavigator1.VisibleButtons (because of autoedit)    }
{------------------------------------------------------------------------------}
{ Note: changes after last posting are not saved when exiting application.     }
{==============================================================================}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DBCtrls, RVScroll, RichView, RVEdit, DBRV, RVStyle, DB,
  DBTables, StdCtrls, Mask, Buttons;

type
  TForm1 = class(TForm)
    Table1: TTable;
    DataSource1: TDataSource;
    RVStyle1: TRVStyle;
    DBNavigator1: TDBNavigator;
    Label1: TLabel;
    Label2: TLabel;
    DBEdit1: TDBEdit;
    Label3: TLabel;
    DBRichView1: TDBRichView;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateStatusLabel;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses Unit2;

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Opening table...
  Table1.DatabaseName := ExtractFilePath(Application.ExeName);
  Table1.Open;

  RVStyle1.TextStyles[1] := RVStyle1.TextStyles[0];
  RVStyle1.TextStyles[1].Style := RVStyle1.TextStyles[1].Style+[fsBold];
end;

procedure TForm1.UpdateStatusLabel;
begin
  // where we are?
  if Table1.RecordCount=0 then
    Label3.Caption := '(empty)'
  else if Table1.RecNo<1 then
    Label3.Caption := '(new)'
  else
    Label3.Caption := Format('Record %d of %d', [Table1.RecNo, Table1.RecordCount]);
end;

procedure TForm1.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  UpdateStatusLabel;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // See Unit2.pas
  Form2.SetField('RVFField',Table1);
  Form2.ShowModal;
end;

end.
