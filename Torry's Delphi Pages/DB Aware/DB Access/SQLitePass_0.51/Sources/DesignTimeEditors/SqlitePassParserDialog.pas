unit SqlitePassParserDialog;
{$i ..\..\Sources\SqlitePassDbo.inc}

interface

uses
{$IFDEF FPC}
LResources,
{$ELSE}
Windows,
Messages,
Grids,
{$ENDIF}
SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
Buttons, ExtCtrls, ComCtrls, SqlitePassDbo, StdCtrls;

type

  { TSqlitePassParserDlg }

  TSqlitePassParserDlg = class(TForm)
    PanelIndexApplyChanges: TPanel;
    SbOk: TSpeedButton;
    CheckBoxShowWhiteSpaces: TCheckBox;
    Tv: TTreeView;
    procedure SbOkClick(Sender: TObject);
    procedure CheckBoxShowWhiteSpacesClick(Sender: TObject);
  private
    MyDataset: TSqlitePassDataset;
  public
    constructor Create(AOwner: TComponent; Dataset: TSqlitePassDataset); reintroduce;
    procedure ShowParserInfos;
  end;

var
  SqlitePassParserDlg: TSqlitePassParserDlg;

implementation


{$IFNDEF FPC}
 {$R *.DFM}
{$ENDIF}

{ TForm1 }

constructor TSqlitePassParserDlg.Create(AOwner: TComponent; Dataset: TSqlitePassDataset);
begin
Inherited Create(aOwner);
MyDataset := Dataset;
ShowParserInfos;
end;


procedure TSqlitePassParserDlg.ShowParserInfos;
var
Level: Integer;
TvItem: TTreeNode;
Text: String;

begin
 Try
 Tv.Items.BeginUpdate;
 Tv.Items.Clear;
 TvItem := nil;
 Level := -1;
 MyDataset.SQLSelectStmt.Tokenizer.Text :=  MyDataset.SQLSelectStmt.SQL;
 MyDataset.SQLSelectStmt.Tokenizer.First;
 While Not MyDataset.SQLSelectStmt.Tokenizer.EOF do
     begin
     if ((Not CheckBoxShowWhiteSpaces.Checked) and (Not (MyDataset.SQLSelectStmt.Tokenizer.Token.TokenType = ttWhiteSpace)))
        or CheckBoxShowWhiteSpaces.Checked then
        begin
        Text := MyDataset.SQLSelectStmt.Tokenizer.Token.Text + '  (' +
             MyDataset.SQLSelectStmt.Tokenizer.Token.TokenTypeAsText + ')';

        if MyDataset.SQLSelectStmt.Tokenizer.Token.NestingLevel > Level
           then begin
                TvItem := Tv.Items.AddChild(TvItem, Text);
                Level := MyDataset.SQLSelectStmt.Tokenizer.Token.NestingLevel;
                end
           else if MyDataset.SQLSelectStmt.Tokenizer.Token.NestingLevel < Level
                then begin
                     TvItem := Tv.Items.AddChild(TvItem.Parent, Text);
                     Level := MyDataset.SQLSelectStmt.Tokenizer.Token.NestingLevel;
                     end
                     else TvItem := Tv.Items.Add(TvItem, Text);
        end;
        MyDataset.SQLSelectStmt.Tokenizer.Next;
        end;
 finally
 Tv.FullExpand;
 Tv.Items.EndUpdate;
 end;
 end;

procedure TSqlitePassParserDlg.SbOkClick(Sender: TObject);
begin
 ModalResult := mrOk;
end;

procedure TSqlitePassParserDlg.CheckBoxShowWhiteSpacesClick(
  Sender: TObject);
begin
  ShowParserInfos;
end;

initialization
{$IFDEF FPC}
  {$I SqlitePassParserDialog.lrs}
{$ENDIF}

end.
