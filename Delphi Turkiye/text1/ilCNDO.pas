unit ilCNDO;

interface

uses
  Windows, Messages, SysUtils, Classes, Dialogs, Forms, ComCtrls, ShellCtrls,
  Controls, StdCtrls;

type
  TilCNDO = class(TComponent)

  private
  DestFolder,ETMCFile,DistanceFile,BondFile:String;

  protected
  FileForm,DestForm,RunForm:TForm;
  ShellTreeView1: TShellTreeView;  //File Selection
  ShellTreeView2: TShellTreeView;  //DestFolder Selection
  CNDOFiles1,CNDOFiles2,lbx1:TListBox;
  BtnRun,BtnClear,BtnCancel,BtnOK,BtnExit:TButton;

  public
    constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Function Execute : Boolean;
    procedure ShellTreeView1DblClick(Sender: TObject);
    procedure BtnRunClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);

    procedure BtnOKClick(Sender: TObject);
    procedure BtnExitClick(Sender: TObject);
//    procedure TForm1.ShellTreeView1Click(Sender: TObject);

  published

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ilker', [TilCNDO]);
end;

{ TilCNDO }

constructor TilCNDO.Create(AOwner: TComponent);
begin
  inherited Create ( AOwner );
//CNDO Files Selection Form
  FileForm:=TForm.Create(Application.Owner);
  ShellTreeView1:=TShellTreeView.Create(AOwner);
  CNDOFiles1:=TListBox.Create(AOwner);
  BtnRun:=TButton.Create(AOwner);
  BtnClear:=TButton.Create(AOwner);
  BtnCancel:=TButton.Create(AOwner);

//Output Folder selection Form
  DestForm:=TForm.Create(Application.Owner);
  ShellTreeView2:=TShellTreeView.Create(AOwner);
  BtnOK:=TButton.Create(AOwner);
  BtnExit:=TButton.Create(AOwner);

//CNDO Files Processing Form
  RunForm:=TForm.Create(Application.Owner);
  CNDOFiles2:=TListBox.Create(AOwner);
  lbx1:=TListBox.Create(AOwner);

//FileForm Formatting***************
  With FileForm do
  begin
        Height:=600;
        Width:=800;
        Caption:='Select CNDO Files From The Browser';
  end;

  With ShellTreeView1 do
  begin
        Parent:=FileForm;
        Height:=300;
        Width:=800;
        Hint:='Duble click to select input CNDO files';
        ShowHint:=True;
        Align:=alTop;
        ObjectTypes:=[otFolders,otNonFolders];
        OnDblClick:=ShellTreeView1DblClick;
        Show;
        AutoRefresh:=True;
  end;

  With BtnClear do
  begin
        Parent:=FileForm;
        Caption:='Clear';
        Height:=25;
        Width:=75;
        Top:=315;
        Left:=50;
        OnClick:=BtnClearClick;
        Show;
  end;

  With BtnRun do
  begin
        Parent:=FileForm;
        Caption:='Run';
        Height:=25;
        Width:=75;
        Top:=315;
        Left:=250;
        OnClick:=BtnRunClick;
        Show;
  end;

  With BtnCancel do
  begin
        Parent:=FileForm;
        Caption:='Cancel';
        Height:=25;
        Width:=75;
        Top:=315;
        Left:=450;
        OnClick:=BtnCancelClick;
        Show;
  end;

  With CNDOFiles1 do
  begin
        Parent:=FileForm;
        Height:=220;
        Width:=800;
        Align:=alBottom;
        Show;
  end;
//DestForm Formatting***************

  With DestForm do
  begin
        Height:=600;
        Width:=400;
        Caption:='Destination Folder Selection';
  end;

  With ShellTreeView2 do
  begin
        Parent:=DestForm;
        AutoRefresh:=True;
        Hint:='Select destination folder and click "OK" button';
        ShowHint:=True;
        Height:=500;
        Align:=alTop;
        ObjectTypes:=[otFolders];
        Show;
  end;

  With BtnOK do
  begin
        Parent:=DestForm;
        Caption:='OK';
        Height:=25;
        Width:=75;
        Top:=530;
        Left:=50;
        OnClick:=BtnOKClick;
        Show;
  end;

  With BtnExit do
  begin
        Parent:=DestForm;
        Caption:='Exit';
        Height:=25;
        Width:=75;
        Top:=530;
        Left:=250;
        OnClick:=BtnExitClick;
        Show;
  end;

//RunForm Formatting***************

  With RunForm do
  begin
        Height:=800;
        Width:=900;
  end;

  With CNDOFiles2 do
  begin
        Parent:=RunForm;
        Height:=200;
        Align:=alTop;
        Show;
  end;

  With lbx1 do
  begin
        Parent:=RunForm;
        Height:=450;
        Align:=alBottom;
        Show;
  end;


end;

destructor TilCNDO.Destroy;
begin

  inherited;
end;

function TilCNDO.Execute: Boolean;
begin
{if OpenDialog1.Execute then
InputFiles.Add(OpenDialog1.FileName);
ShowMessage(OpenDialog1.FileName);}

DestForm.Show;

end;

procedure TilCNDO.ShellTreeView1DblClick(Sender: TObject);
begin
if (StrPos(PChar(ShellTreeView1.Path), PChar('.OUT'))<>nil) or (StrPos(PChar(ShellTreeView1.Path), PChar('.out'))<>nil) then
CNDOFiles1.Items.Add(ShellTreeView1.Path)

end;

procedure TilCNDO.BtnCancelClick(Sender: TObject);
begin
ShowMessage('Application will shut down!');
DestFolder:='';
FileForm.Close;
end;

procedure TilCNDO.BtnClearClick(Sender: TObject);
begin
CNDOFiles1.Clear;
end;

procedure TilCNDO.BtnExitClick(Sender: TObject);
begin
DestForm.Close;
end;

procedure TilCNDO.BtnOKClick(Sender: TObject);
begin
if (ShellTreeView2.Path<>'') then
        begin
        DestFolder:=ShellTreeView2.Path+'\';
        ShowMessage('You selected this folder: '+ShellTreeView2.Path+'\');
        FileForm.Show;
        DestForm.Close;
        end  else
        begin
        ShowMessage('You didn''t select any folder, Application will shut down');
        DestForm.Close;
        end;
end;


procedure TilCNDO.BtnRunClick(Sender: TObject);
var
i,j,k,l,m,line,linecount, viebergline:integer;   //for loop, variable must be a local one
s,v,b:string;
F:TextFile;
d,e:double;

  a_no:integer;
  index:integer;  //order of target word in the line
  SepWord:tstrings; //Separated words of each line
  a_counter, i_counter:integer; //atom number and indice counter variable
  row,column:integer;

  ANO: array of integer;
  X: array of string;
  Y: array of string;
  Z: array of string;
  Diagonal: array of string;
  Charge: array of string;
  VAL: array of string;
  HOMC: array of string;
  LUMC: array of string;
  SORT: array of string;
  VIEBERG:array of string;
  ETMC:array of String;
  DISTANCE:array of String;
  MolName:String;
  HeavyANumber:integer;
  AtomNumber:integer;
  DipMom: string;
  EHomo,ELumo:string;


begin
a_no:=0;

RunForm.Show;
CNDOFiles2.Items:=CNDOFiles1.Items;
FileForm.Close;

for k:=0 to CNDOFiles2.Items.Count-1 do

begin
lbx1.Clear;
lbx1.Items.LoadFromFile(CNDOFiles2.Items.Strings[k]);
CNDOFiles2.Selected[k]:=True;

if FileCreate(DestFolder+DateTimeToStr(Date)+'_ETMC.txt')<0 then
begin
ShowMessage('Cannot create the file: '+DestFolder+DateToStr(Now)+'.txt');
end
else ETMCFile:=DestFolder+DateTimeToStr(Date)+'_ETMC.txt';


AssignFile(F,ETMCFile);
FileSetReadOnly(ETMCFile,False);
Append(F);
Writeln(F,'Tamam!...');

CloseFile(F);

SepWord := TStringList.Create;

end;



end;


end.
