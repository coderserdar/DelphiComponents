{*******************************************************}
{File:      NCMemo.PAS                                  }
{Revision:  2.05 / 06.02.2000                           }
{Comment:   Simple MemoEdit                             }
{Copyright: (c) 1997-2000, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, dmitrya@inthink.com         }
{*******************************************************}
{$I NCOciDef.inc}

unit NCMemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, DB;

type
  TNCMemoEditDlg = class(TForm)
    Memo: TMemo;
    Panel2: TPanel;
    OpenBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    CutBtn: TSpeedButton;
    CopyBtn: TSpeedButton;
    PasteBtn: TSpeedButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    OKBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    UndoBtn: TSpeedButton;
    PrintBtn: TSpeedButton;
    PrintDlg: TPrintDialog;
    procedure FormCreate(Sender: TObject);
    procedure CancelBtn2Click(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure CutBtnClick(Sender: TObject);
    procedure CopyBtnClick(Sender: TObject);
    procedure PasteBtnClick(Sender: TObject);
    procedure MemoKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OKBtnClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure UndoBtnClick(Sender: TObject);
    procedure PrintBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    procedure ShowCrsPos;
  public
    { Public declarations }
  end;

  TNCMemoDlgOption = (moReadOnly, moWordWrap);
  TNCMemoDlgOptions = set of TNCMemoDlgOption;

  TNCMemoDialog = class(TComponent)
  private
    FCaption: String;
    FLines: TStrings;
    FDataSource: TDataSource;
    FDataField: String;
    FFont: TFont;
    FOptions: TNCMemoDlgOptions;
    FColor, FROColor: TColor;
    procedure SetLines(AValue: TStrings);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetFont(Avalue: TFont);
    function MemoEdit: Boolean;
    function DBMemoEdit: Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    property Caption: String read FCaption write FCaption;
    property Lines: TStrings read FLines write SetLines;
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property DataField: String read FDataField write FDataField;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write FColor default clWindow;
    property ROColor: TColor read FROColor write FROColor default clBtnFace;
    property Options: TNCMemoDlgOptions read Foptions write FOptions default [];
  end;

  function MemoEdit(ALines: TStrings; ACaption: String; ARO: Boolean): Boolean;
  function DBMemoEdit(ADataSource: TDataSource; ADataField: String): Boolean;

var
  NCMemoEditDlg: TNCMemoEditDlg;

{ ---------------------------------------------------------------------------------- }
{ ---------------------------------------------------------------------------------- }

implementation

{$R *.DFM}

Uses Printers, NCUIUtil, NCStrs;

{ ---------------------------------------------------------------------------------- }
{ ---------------------------------------------------------------------------------- }

function MemoEdit(ALines: TStrings; ACaption: String; ARO: Boolean): Boolean;
begin
    with TNCMemoDialog.Create(nil) do
    try
        Lines := ALines;
        Caption := ACaption;
        if ARO then
            Options := [moReadOnly];
        Result := Execute;
    finally
        Free;
    end;
end;

function DBMemoEdit(ADataSource: TDataSource; ADataField: String): Boolean;
begin
    with TNCMemoDialog.Create(nil) do
    try
        DataSource := ADataSource;
        DataField := ADataField; 
        Result := Execute;
    finally
        Free;
    end;
end;

{ ---------------------------------------------------------------------------------- }
{ ---------------------------------------------------------------------------------- }

procedure TNCMemoEditDlg.FormCreate(Sender: TObject);
begin
    IUStr2Font(SDefaultFixedFont, Font);
end;

procedure TNCMemoEditDlg.CancelBtn2Click(Sender: TObject);
begin
    ModalResult := mrCancel;
    if Memo.Modified and not YesNoDlg([SMemoChnged], -1) then
        ModalResult := mrNone;
end;

procedure TNCMemoEditDlg.OKBtnClick(Sender: TObject);
begin
    ModalResult := mrOK;
end;

procedure TNCMemoEditDlg.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = #27 then // Esc
        CancelBtn2Click(Self)
    else if Key = #23 then // Ctrl-W
        OKBtnClick(Self);
end;

procedure TNCMemoEditDlg.OpenBtnClick(Sender: TObject);
begin
    if OpenDialog1.Execute then
        Memo.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TNCMemoEditDlg.SaveBtnClick(Sender: TObject);
begin
    if SaveDialog1.Execute then
        Memo.Lines.SaveToFile(SaveDialog1.FileName);
end;

procedure TNCMemoEditDlg.PrintBtnClick(Sender: TObject);
var
  i: Integer;
  prn: TextFile;
begin
    if PrintDlg.Execute then begin
        AssignPrn(prn);
        Rewrite(prn);
        try
            IUStr2Font(SDefaultFont, Printer.Canvas.Font);
            Printer.Canvas.Font.Style := Printer.Canvas.Font.Style + [fsBold];
            WriteLn(prn);
            WriteLn(prn);
            WriteLn(prn, '           ' + SMemoContentOf + '''' + Caption + '''');
            WriteLn(prn);
            WriteLn(prn, '           ' + SMemoDateTime + ' ' + DateTimeToStr(now));
            IUStr2Font(SDefaultFont, Printer.Canvas.Font);
            for i := 0 to Memo.Lines.Count - 1 do
                WriteLn(prn, '     ', Memo.Lines[i]);
        finally
            CloseFile(prn);
        end;
    end;
end;

procedure TNCMemoEditDlg.CutBtnClick(Sender: TObject);
begin
    Memo.CutToClipboard;
end;

procedure TNCMemoEditDlg.CopyBtnClick(Sender: TObject);
begin
    Memo.CopyToClipboard;
end;

procedure TNCMemoEditDlg.PasteBtnClick(Sender: TObject);
begin
    Memo.PasteFromClipboard;
end;

procedure TNCMemoEditDlg.UndoBtnClick(Sender: TObject);
begin
    SendMessage(Memo.Handle, EM_UNDO, 0, 0)
end;

procedure TNCMemoEditDlg.MemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    ShowCrsPos;
end;

procedure TNCMemoEditDlg.ShowCrsPos;
var
    L, C: LongInt;
    P: TPoint;
begin
    with Memo do begin
        GetCaretPos(P);
        L := SendMessage(Handle, EM_CHARFROMPOS, 0, MakeLong(P.X, P.Y));
        C := LoWord(L) - SendMessage(Handle, EM_LINEINDEX, -1, 0);
        L := HiWord(L);
    end;
    with StatusBar1 do begin
        Panels[0].Text := IntToStr(L + 1);
        Panels[1].Text := IntToStr(C + 1);
        if Memo.ReadOnly then
            Panels[2].Text := 'RO'
        else
            Panels[2].Text := '';
    end;
end;

procedure TNCMemoEditDlg.FormActivate(Sender: TObject);
begin
    ShowCrsPos;
end;

{ ---------------------------------------------------------------------------------- }
{ ---------------------------------------------------------------------------------- }

constructor TNCMemoDialog.Create(AOwner: Tcomponent);
begin
    inherited Create(AOwner);
    FLines := TStringList.Create;
    FFont := TFont.Create;
    IUStr2Font(SDefaultFixedFont, FFont);
    FColor := clWindow;
    FROColor := clBtnFace;
end;

destructor TNCMemoDialog.Destroy;
begin
    FLines.Free;
    FFont.Free;
    inherited Destroy;
end;

procedure TNCMemoDialog.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then
        if AComponent = FDataSource then
            FDataSource := nil;
end;

procedure TNCMemoDialog.SetLines(AValue: TStrings);
begin
    FLines.Assign(AValue);
end;

procedure TNCMemoDialog.SetDataSource(AValue: TDataSource);
begin
    if FDataSource <> AValue then begin
        FDataSource := AValue;
        if AValue <> nil then
            AValue.FreeNotification(Self);
    end;
end;

procedure TNCMemoDialog.SetFont(Avalue: TFont);
begin
    FFont.Assign(AValue);
end;

function TNCMemoDialog.MemoEdit: Boolean;
begin
    if NCMemoEditDlg = nil then
        NCMemoEditDlg := TNCMemoEditDlg.Create(Application);
    with NCMemoEditDlg do begin
        Memo.Lines.Assign(Lines);
        Memo.ReadOnly := moReadOnly in Options;
        Memo.WordWrap := moWordWrap in Options;
        Memo.Modified := False;
        Memo.Font.Assign(Self.Font);
        if moReadOnly in Options then
            Memo.Color := ROColor
        else
            Memo.Color := Self.Color;
        if Self.Caption = '' then
            Caption := SMemoCapt
        else
            Caption := Self.Caption;
        OpenBtn.Enabled := not (moReadOnly in Options);
        CutBtn.Enabled := not (moReadOnly in Options);
        PasteBtn.Enabled := not (moReadOnly in Options);
        Result := (ShowModal = mrOK);
        if Result then
            Lines.Assign(Memo.Lines);
    end;
end;

function TNCMemoDialog.DBMemoEdit: Boolean;
var
    AField: TField;
    prevOptions: TNCMemoDlgOptions;
begin
    Result := False;
    if (DataSource = nil) or (DataSource.DataSet = nil) or (DataField = '') then
        Exit;
    AField := DataSource.DataSet.FieldByName(DataField);
    prevOptions := Options;
    try
        if not (moReadOnly in Options) and not AField.CanModify then
            Options := Options + [moReadOnly];
        if AField is TBlobField then
            Lines.Assign(AField)
        else
            Lines.Text := AField.Text;
        Caption := AField.DisplayLabel;
        if MemoEdit then begin
            DataSource.Edit;
            if AField is TBlobField then
                AField.Assign(Lines)
            else
                AField.Text := Lines.Text;
        end;
    finally
        Options := prevOptions;
    end;
end;

function TNCMemoDialog.Execute: Boolean;
begin
    if (DataSource <> nil) or (DataField <> '') then
        Result := DBMemoEdit
    else
        Result := MemoEdit;
end;

end.
