{*********************************************************}
{File:      NCOciGenSetupDlg.PAS                          }
{Revision:  0.01.00 / 30.01.2002                          }
{Comment:   NC OCI8 VCL: PL/SQL package wrapper generator }
{Copyright: (c) 1999-2002, Dmitry Arefiev                 }
{Author:    Dmitry Arefiev, darefiev@da-soft.com          }
{*********************************************************}
{$I NCOciDef.inc}

unit NCOciPLSQLGenSetupDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, NCOci, NCOciWrapper, NCOciDB, ExtCtrls, Grids, DBGrids,
  ComCtrls, StdCtrls, DBCtrls, Buttons, NCOciPLSQLGen;

type
  TOciPLSQLGenSetupFrm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    dsPackProcs: TDataSource;
    dsPackProcParams: TDataSource;
    dsPackages: TDataSource;
    dblckPackage: TDBLookupComboBox;
    Label1: TLabel;
    TabSheet3: TTabSheet;
    Bevel1: TBevel;
    dbgProcs: TDBGrid;
    Splitter1: TSplitter;
    DBGrid2: TDBGrid;
    Bevel2: TBevel;
    Label2: TLabel;
    edtParPackClass: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    edtParRecClass: TEdit;
    edtParTabClass: TEdit;
    cbSkipUnsupProcs: TCheckBox;
    Bevel3: TBevel;
    pnlProc: TPanel;
    Label5: TLabel;
    Edit1: TEdit;
    Label6: TLabel;
    ComboBox1: TComboBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label7: TLabel;
    pnlPar: TPanel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Edit2: TEdit;
    ComboBox2: TComboBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    pnlPack: TPanel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Edit3: TEdit;
    ComboBox3: TComboBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    pnlTab: TPanel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Edit4: TEdit;
    ComboBox4: TComboBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    pnlRec: TPanel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Edit5: TEdit;
    ComboBox5: TComboBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    pnlField: TPanel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Edit6: TEdit;
    ComboBox6: TComboBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    TabSheet2: TTabSheet;
    edtUnitFileName: TEdit;
    Label23: TLabel;
    SpeedButton1: TSpeedButton;
    SaveDialog1: TSaveDialog;
    btnSelectAll: TButton;
    BitBtn1: TBitBtn;
    btnGenerate: TBitBtn;
    Label24: TLabel;
    edtUnitName: TEdit;
    dbGen: TOCIDatabase;
    qryPackages: TOCIQuery;
    qryPackProcs: TOCIQuery;
    qryPackProcParams: TOCIQuery;
    procedure dblckPackageExit(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure edtUnitFileNameExit(Sender: TObject);
  private
    { Private declarations }
    FRootGen: TOCIPLSQLGenerator;
    FLastPackage: Variant;
    procedure LoadGenName(ANameKind: TOCIPLSQLNameKind; APanel: TPanel);
    procedure SaveGenName(ANameKind: TOCIPLSQLNameKind; APanel: TPanel);
    procedure SaveProcsList;
  public
    { Public declarations }
    procedure Execute;
  end;

var
  OciPLSQLGenSetupFrm: TOciPLSQLGenSetupFrm;

implementation

{$R *.dfm}

{$IFDEF OCI_D6}
uses
  Variants, RTLConsts;
{$ENDIF}

procedure TOciPLSQLGenSetupFrm.FormCreate(Sender: TObject);
begin
    FRootGen := TOCIPLSQLGenerator.Create;
    FLastPackage := null;
    FRootGen.Database := dbGen;
    dblckPackage.KeyValue := null;
    edtParPackClass.Text := FRootGen.ParentPackageClass;
    edtParRecClass.Text := FRootGen.ParentRecClass;
    edtParTabClass.Text := FRootGen.ParentTabClass;
    cbSkipUnsupProcs.Checked := FRootGen.SkipUnsupportedProcs;
    LoadGenName(pnRecType, pnlRec);
    LoadGenName(pnTabType, pnlTab);
    LoadGenName(pnPackage, pnlPack);
    LoadGenName(pnField, pnlField);
    LoadGenName(pnProc, pnlProc);
    LoadGenName(pnParam, pnlPar);
end;

procedure TOciPLSQLGenSetupFrm.FormDestroy(Sender: TObject);
begin
    FRootGen.Free;
    FRootGen := nil;
end;

procedure TOciPLSQLGenSetupFrm.dblckPackageExit(Sender: TObject);
var
    V: Variant;
    S: String;
    i: Integer;
begin
    if (VarIsNull(FLastPackage) and not VarIsNull(dblckPackage.KeyValue)) or
       (not VarIsNull(FLastPackage) and not VarIsNull(dblckPackage.KeyValue) and
        (FLastPackage <> dblckPackage.KeyValue)) then begin
        V := dblckPackage.KeyValue;
        qryPackProcs.Close;
        if VarIsNull(V) then begin
            qryPackProcs.ParamByName('OWN').Clear;
            qryPackProcs.ParamByName('PACK').Clear;
            edtUnitFileName.Text := '';
        end
        else begin
            S := V;
            i := Pos('.', S);
            qryPackProcs.ParamByName('OWN').AsString := Copy(S, 1, i - 1);
            qryPackProcs.ParamByName('PACK').AsString := Copy(S, i + 1, Length(S));
            edtUnitFileName.Text := qryPackProcs.ParamByName('PACK').AsString + '.pas';
        end;
        qryPackProcParams.ParamByName('OWN').Assign(qryPackProcs.ParamByName('OWN'));
        qryPackProcParams.ParamByName('PACK').Assign(qryPackProcs.ParamByName('PACK'));
        qryPackProcs.Open;
        qryPackProcParams.Open;
        btnSelectAllClick(nil);
        FLastPackage := dblckPackage.KeyValue;
    end;
end;

procedure TOciPLSQLGenSetupFrm.SpeedButton1Click(Sender: TObject);
begin
    SaveDialog1.FileName := edtUnitFileName.Text;
    if SaveDialog1.Execute then
        edtUnitFileName.Text := SaveDialog1.FileName;
end;

procedure TOciPLSQLGenSetupFrm.edtUnitFileNameExit(Sender: TObject);
var
    s: String;
    i: Integer;
begin
    s := ExtractFileName(edtUnitFileName.Text);
    for i := Length(s) downto 1 do
        if s[i] = '.' then begin
            s := Copy(s, 1, i - 1);
            Break;
        end;
    edtUnitName.Text := s;
end;

procedure TOciPLSQLGenSetupFrm.LoadGenName(ANameKind: TOCIPLSQLNameKind; APanel: TPanel);
begin
    with APanel, FRootGen.NamingRules[ANameKind] do begin
        (Controls[3] as TEdit).Text := Fmt;
        with (Controls[4] as TComboBox) do begin
            if noUpperCase in Options then
                ItemIndex := 0
            else if noLowerCase in Options then
                ItemIndex := 1
            else if noPretty in Options then
                ItemIndex := 2;
        end;
        (Controls[5] as TCheckBox).Checked := noTrimUnderscore in Options;
        (Controls[6] as TCheckBox).Checked := noTrimOthers in Options;
    end;
end;

procedure TOciPLSQLGenSetupFrm.SaveGenName(ANameKind: TOCIPLSQLNameKind; APanel: TPanel);
begin
    with APanel, FRootGen.NamingRules[ANameKind] do begin
        Fmt := (Controls[3] as TEdit).Text;
        with (Controls[4] as TComboBox) do begin
            if ItemIndex = 0 then
                Options := Options + [noUpperCase]
            else if ItemIndex = 1 then
                Options := Options + [noLowerCase]
            else if ItemIndex = 2 then
                Options := Options + [noPretty];
        end;
        if (Controls[5] as TCheckBox).Checked then
            Options := Options + [noTrimUnderscore];
        if (Controls[6] as TCheckBox).Checked then
            Options := Options + [noTrimOthers];
    end;
end;

procedure TOciPLSQLGenSetupFrm.btnSelectAllClick(Sender: TObject);
begin
    dbgProcs.SelectedRows.Clear;
    qryPackProcs.DisableControls;
    try
        qryPackProcs.First;
        while not qryPackProcs.Eof do begin
            dbgProcs.SelectedRows.CurrentRowSelected := True;
            qryPackProcs.Next;
        end;
    finally
        qryPackProcs.First;
        qryPackProcs.EnableControls;
    end;
end;

procedure TOciPLSQLGenSetupFrm.SaveProcsList;
var
    i: Integer;
    s: String;
begin
    FRootGen.ProcsToGenerate.Clear;
    qryPackProcs.DisableControls;
    try
        for i := 0 to dbgProcs.SelectedRows.Count - 1 do begin
            qryPackProcs.Bookmark := dbgProcs.SelectedRows.Items[i];
            s := qryPackProcs.FieldByName('OVERLOAD').AsString;
            if s = '' then
                s := '0';
            FRootGen.ProcsToGenerate.Add(qryPackProcs.FieldByName('OBJECT_NAME').AsString +
                '[' + s + ']');
        end;
    finally
        qryPackProcs.First;
        qryPackProcs.EnableControls;
    end;
end;

procedure TOciPLSQLGenSetupFrm.btnGenerateClick(Sender: TObject);
var
    flStr: TFileStream;
begin
    if VarIsNull(dblckPackage.KeyValue) then begin
        PageControl1.ActivePage := TabSheet1;
        MessageDlg('You must select a package', mtError, [mbOk], -1);
        dblckPackage.SetFocus;
    end;
    if dbgProcs.SelectedRows.Count = 0 then
        btnSelectAllClick(nil);
    if Trim(edtParPackClass.Text) = '' then
        edtParPackClass.Text := FRootGen.ParentPackageClass;
    if Trim(edtParRecClass.Text) = '' then
        edtParRecClass.Text := FRootGen.ParentRecClass;
    if Trim(edtUnitFileName.Text) = '' then
        edtUnitFileName.Text := qryPackProcs.ParamByName('PACK').AsString + '.pas';
    if Trim(edtUnitName.Text) = '' then
        edtUnitFileNameExit(nil);
    FRootGen.PackName := dblckPackage.KeyValue;
    FRootGen.ParentPackageClass := edtParPackClass.Text;
    FRootGen.ParentRecClass := edtParRecClass.Text;
    FRootGen.ParentTabClass := edtParTabClass.Text;
    FRootGen.SkipUnsupportedProcs := cbSkipUnsupProcs.Checked;
    FRootGen.UnitName := edtUnitName.Text;
    SaveGenName(pnRecType, pnlRec);
    SaveGenName(pnTabType, pnlTab);
    SaveGenName(pnPackage, pnlPack);
    SaveGenName(pnField, pnlField);
    SaveGenName(pnProc, pnlProc);
    SaveGenName(pnParam, pnlPar);
    SaveProcsList;
    FRootGen.Gen;
    flStr := TFileStream.Create(edtUnitFileName.Text, fmCreate);
    try
        FRootGen.SaveToStream(flStr);
    finally
        flStr.Free;
    end;
    MessageDlg('Unit was succesfully generated', mtInformation, [mbOk], -1);
end;

procedure TOciPLSQLGenSetupFrm.Execute;
begin
    dbGen.Open;
    qryPackages.Open;
    qryPackProcs.Open;
    qryPackProcParams.Open;
    ShowModal;
end;

end.
