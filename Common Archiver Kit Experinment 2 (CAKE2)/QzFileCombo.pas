unit QzFileCombo;


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ShellApi, CakComboBoxEx, ExtCtrls, ImgList,
  QzSimpleFileList, QzFileImgList;

type
  TQzFileCombo = class(TCakComboBoxEx)
  private
    { Private declarations }
    ListView1: TQzSimpleFileList;
    FNames : TStrings;
    fn : string;

    aPanel : TPanel;
    procedure ListView1Click(Sender: TObject);
    procedure SetfNames(Value : TStrings);
    procedure Setfn(Value : string);
  protected
    { Protected declarations }
  public
    { Public declarations }
    TreeViewHeight : integer;
    findex : integer;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure FormDropDown(Sender : TObject);
    procedure Paint; override;
  published
    { Published declarations }
    property FileNames : TStrings read fNames write SetfNames;
    property FileName : string read Fn write SetFn;
  end;

  procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('QZIP', [TQzFileCombo]);
end;

constructor TQzFileCombo.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  OnDroppedDown := FormDropDown;

  ListView1 := TQzSimpleFileList.Create(DropDownForm);
  ListView1.Parent := DropDownForm;
  ListView1.Align := alClient;
  ListView1.OnClick := ListView1Click;
  Listview1.ViewStyle := vsReport;
  Listview1.ShowColumnHeaders := False;

  FNames := TStringList.Create;
  FileName := '';
  findex := -1;
end;

destructor TQzFileCombo.Destroy;
begin
  ListView1.Free;
  FNames.Free;
  inherited Destroy;
end;

procedure TQzFileCombo.FormDropDown(Sender : TObject);
var i : integer;
var ListItem : TListItem;
begin
  DropDownForm.Width := 500;
  ListView1.FList.Clear;
  ListView1.FList.Assign(FileNames);
  ListView1.Load;
end;

procedure TQzFileCombo.paint;
var t : integer;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0,0,Width,Height));
  t := (Height - Font.Size) div 2 - 4;
  if Assigned(DropDownForm) then
  if findex >= 0 then
    ListView1.ImageList.SmallImages.Draw(Canvas,2,t,findex);
  Canvas.TextOut(20,t,Format('%s',[ExtractFilename(FileName)]))
 end;

procedure TQzFileCombo.ListView1Click(Sender: TObject);
begin
  if Listview1.Selected <> nil then
  if Listview1.Selected.Index >= 0 then
  begin
  filename := Filenames.strings[Listview1.Selected.index];
  findex := Listview1.Selected.ImageIndex;
  DropDownForm.Close;
  if Assigned(OnChanged) then
    OnChanged(Sender);
  end;
  Invalidate;
end;

procedure TQzFileCombo.SetfNames(Value : TStrings);
begin
  fNames.Assign(Value);
end;

procedure TQzFileCombo.Setfn(Value : string);
begin
  fn := Value;
  findex := ListView1.ImageList.ReturnIconType(fn);
  Invalidate;
end;

end.
