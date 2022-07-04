unit uTestObjectParser;
{$B-,O-,D+,L+}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  RegExpr, DbxObjectParser;

type
  TfrmMain = class(TForm)
    btnDecode: TButton;
    edCatalog: TEdit;
    edSchema: TEdit;
    edObject: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btnClearDecore: TButton;
    cmFullName: TComboBox;
    rgParser: TRadioGroup;
    edQuote: TEdit;
    Label4: TLabel;
    btnNext: TButton;
    mDefaultTestedNames: TMemo;
    btnPrevious: TButton;
    btnEncode: TButton;
    edEncode: TEdit;
    btnClearEncode: TButton;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    edRegExprNew: TMemo;
    Shape1: TShape;
    procedure FormCreate(Sender: TObject);
    procedure btnDecodeClick(Sender: TObject);
    procedure btnClearDecoreClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure btnEncodeClick(Sender: TObject);
    procedure Button6_Click(Sender: TObject);
  private
    { Private declarations }
    function CreateObjectNameParser: TObjectNameParser;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
 var F:String;
begin
  F := ExtractFilePath(ParamStr(0))+'TestedObjectNames.txt';
  if FileExists(F) then
  try
    cmFullName.Items.LoadFromFile(F);
    if cmFullName.Items.Count=0 then
      cmFullName.Items.Assign(mDefaultTestedNames.Lines);
    //if cmFullName.Items.Count>mDefaultTestedNames.Lines.Count then
    //  cmFullName.ItemIndex := cmFullName.Items.Count-1;
  except
    cmFullName.Items.Assign(mDefaultTestedNames.Lines);
  end
  else
    cmFullName.Items.Assign(mDefaultTestedNames.Lines);
  if cmFullName.ItemIndex<0 then
  begin
    if mDefaultTestedNames.Lines.Count > 1 then
      cmFullName.ItemIndex := 1
    else
      cmFullName.ItemIndex := 0;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  try
    cmFullName.Items.SaveToFile(ExtractFilePath(ParamStr(0))+'TestedObjectNames.txt');
  except
  end;
end;

function TfrmMain.CreateObjectNameParser: TObjectNameParser;
var
  sQuote, sRegExprNew: AnsiString;
begin
  sQuote := AnsiString(edQuote.Text);
  sRegExprNew := AnsiString(Trim(edRegExprNew.Text));
  case rgParser.ItemIndex of
    0: Result := TObjectNameParser.Create(@DefaultObjectNameTemplateInfo, sQuote, sRegExprNew);
    1: Result := TObjectNameParser.Create(@SQLServerObjectNameTemplateInfo, sQuote, sRegExprNew);
    2: Result := TObjectNameParser.Create(@TextObjectNameTemplateInfo, sQuote, sRegExprNew);
    3: Result := TObjectNameParser.Create(@InformixObjectNameTemplateInfo, sQuote, sRegExprNew);
    4: Result := TObjectNameParser.Create(@OracleObjectNameTemplateInfo, sQuote, sRegExprNew);
    else
       Result := TObjectNameParser.Create(@DefaultObjectNameTemplateInfo, sQuote, sRegExprNew);
  end;
end;

// DecodeObjectFullName to patrs
procedure TfrmMain.btnDecodeClick(Sender: TObject);
 var P:TObjectNameParser;
     CatalogName,SchemaName,ObjectName: AnsiString;
begin
  if cmFullName.Items.IndexOf(cmFullName.Text)<0 then
    cmFullName.Items.Add(cmFullName.Text);

  P := CreateObjectNameParser;
  try
    P.DecodeObjectFullName(
      AnsiString(cmFullName.Text),
        CatalogName,
        SchemaName,
        ObjectName
    );
    edCatalog.Text := string(CatalogName);
    edSchema.Text  := string(SchemaName);
    edObject.Text  := string(ObjectName);
  finally
    P.Free;
  end;
end;

// EncodeObjectFullName from parts
procedure TfrmMain.btnEncodeClick(Sender: TObject);
 var P:TObjectNameParser;
begin
  P := CreateObjectNameParser;
  try
  edEncode.Text :=  string(P.EncodeObjectFullName(
    AnsiString(edCatalog.Text),
    AnsiString(edSchema.Text),
    AnsiString(edObject.Text)
  ));
  finally
    P.Free;
  end;
end;

// Clear
procedure TfrmMain.btnClearDecoreClick(Sender: TObject);
begin
  edCatalog.Text := '';
  edSchema.Text  := '';
  edObject.Text  := '';
  edEncode.Text  := '';
end;

procedure TfrmMain.Button6_Click(Sender: TObject);
begin
  edEncode.Text  := '';
end;

// Next
procedure TfrmMain.btnNextClick(Sender: TObject);
begin
   if cmFullName.ItemIndex<cmFullName.Items.Count-1 then
     cmFullName.ItemIndex := cmFullName.ItemIndex + 1
   else
     cmFullName.ItemIndex := 0;
end;

// Previous
procedure TfrmMain.btnPreviousClick(Sender: TObject);
begin
   if cmFullName.ItemIndex>0 then
     cmFullName.ItemIndex := cmFullName.ItemIndex - 1
   else
     cmFullName.ItemIndex := cmFullName.Items.Count-1;
end;

end.
