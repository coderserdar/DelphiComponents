{=============================} unit Unit1; {==================================}
{ Main properties were set at design time:                                     }
{ Table1.TableName           := 'SampleTable.db'; // Paradox table             }
{ Table1.ReadOnly            := False;                                         }
{ DataSource1.Dataset        := Table1;                                        }
{ DBNavigator1.DataSource    := DataSource1;                                   }
{ DBEdit1.DataSource         := DataSource1;                                   }
{ DBRichViewEdit1.DataSource := DataSource1;                                   }
{ DBRichViewEdit1.Style      := RVStyle1;                                      }
{ DBEdit1.DataField          := 'Caption';  // Alphanumeric field              }
{ DBRichViewEdit1.DataField  := 'RVFField'; // Binary field                    }
{ nbEdit was removed from DBNavigator1.VisibleButtons (because of autoedit)    }
{------------------------------------------------------------------------------}
{ DBRichViewEdit has data-aware features that require no code to use           }
{ (just like most of other Delphi db controls).                                }
{ Some more event handlers are needed for advanced RVF features, such as saving}
{ "bullets" and "hotspots". There is no difference here with saving/loading    }
{ RVF files.                                                                   }
{ The code below opens table, updates label displaying record number,          }
{ provides "bold" button functionality.                                        }
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
    DBRichViewEdit1: TDBRichViewEdit;
    DBNavigator1: TDBNavigator;
    Label1: TLabel;
    Label2: TLabel;
    DBEdit1: TDBEdit;
    Label3: TLabel;
    SpeedButton1: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure DBRichViewEdit1CurTextStyleChanged(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateStatusLabel;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Opening table...
  Table1.DatabaseName := ExtractFilePath(Application.ExeName);
  Table1.Open;
  // For demonstrating rich text ability of TDBRichViewEdit,
  // we've added "Bold" button. It will switch the 0-th and the 1-st styles.
  // Making 1-st text style a bold copy of 0-th style...
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

procedure TForm1.DBRichViewEdit1CurTextStyleChanged(Sender: TObject);
begin
  SpeedButton1.Down := DBRichViewEdit1.CurTextStyleNo<>0;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  // switching 1-st and 0-th styles
  if SpeedButton1.Down then
    DBRichViewEdit1.ApplyTextStyle(1)
  else
    DBRichViewEdit1.ApplyTextStyle(0);
end;

end.
