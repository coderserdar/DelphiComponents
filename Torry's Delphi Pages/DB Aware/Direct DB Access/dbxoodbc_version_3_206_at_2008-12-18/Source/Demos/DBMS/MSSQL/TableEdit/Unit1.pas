unit Unit1;

  { Delphi 6/7/2005/2006/2007 }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SqlConst, DBConsts,
  {$IF CompilerVersion > 17.00}
  WideStrings,
  {$IFEND}
  {$IF CompilerVersion < 18.50}
  DBXpress,
  {$ELSE}
  DBXCommon,
  {$IFEND}
  DbxOpenOdbcInterface,
  DB, SqlExpr, FMTBcd, StdCtrls, ExtCtrls, ComCtrls, DBCtrls, Grids, DBGrids, DBClient, Provider;

type
  TForm1 = class(TForm)
    SQLConnection: TSQLConnection;
    SQLDataSet: TSQLDataSet;
    p1: TPanel;
    statBar1: TStatusBar;
    p2: TPanel;
    p3: TPanel;
    split1: TSplitter;
    mem_sql: TMemo;
    p4: TPanel;
    btn_connect: TButton;
    btn_open: TButton;
    btn_apply: TButton;
    btn_close: TButton;
    btn_disconnect: TButton;
    cbx_connection_string: TComboBox;
    sh_connection_status: TShape;
    txt1: TStaticText;
    SUID: TStaticText;
    EUID: TEdit;
    SPWD: TStaticText;
    EPWD: TEdit;
    DSP: TDataSetProvider;
    CDS: TClientDataSet;
    DataSource: TDataSource;
    p5: TPanel;
    DBNavigator: TDBNavigator;
    PC: TPageControl;
    ts_grid: TTabSheet;
    DBGrid: TDBGrid;
    ts_blob: TTabSheet;
    p6: TPanel;
    db_memo: TDBMemo;
    cbx_fields: TComboBox;
    StaticText1: TStaticText;
    p7: TPanel;
    cbx_query: TComboBox;
    procedure SQLConnectionAfterConnect(Sender: TObject);
    procedure SQLConnectionAfterDisconnect(Sender: TObject);
    procedure btn_connectClick(Sender: TObject);
    procedure btn_disconnectClick(Sender: TObject);
    procedure btn_openClick(Sender: TObject);
    procedure btn_closeClick(Sender: TObject);
    procedure btn_applyClick(Sender: TObject);
    procedure CDSBeforeApplyUpdates(Sender: TObject; var OwnerData: OleVariant);
    procedure DSPUpdateError(Sender: TObject; DataSet: TCustomClientDataSet; E: EUpdateError; UpdateKind: TUpdateKind;
      var Response: TResolverResponse);
    procedure cbx_fieldsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CDSBeforeClose(DataSet: TDataSet);
    procedure cbx_fieldsChange(Sender: TObject);
    procedure CDSAfterOpen(DataSet: TDataSet);
    procedure cbx_queryChange(Sender: TObject);
  private
    { Private declarations }
    FErrorCount: Integer;
    //TD: TTransactionDesc;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Registry;

procedure TForm1.btn_applyClick(Sender: TObject);
begin
  if CDS.Active and (CDS.ChangeCount > 0) then
  begin
    ///SQLConnection.StartTransaction(TD);

    CDS.ApplyUpdates(0);
    if FErrorCount = 0 then
    begin
      //SQLConnection.Commit(TD);
      MessageDlg('Applying updates is successfully !', mtInformation, [mbOK], 0)
    end
    else begin
      //SQLConnection.Rollback(TD);
      if FErrorCount > 1 then
        MessageDlg('Error Count = ' + IntToStr(FErrorCount), mtError, [mbOK], 0);
    end;
  end
  else
    MessageDlg('Changes not found !', mtWarning, [mbOK], 0);
end;

procedure TForm1.btn_closeClick(Sender: TObject);
begin
  CDS.Close;
  SQLDataSet.Close;
end;

procedure TForm1.btn_connectClick(Sender: TObject);
var
  sConnectionString, sVendorLib, S: string;
  iPos1, iPos2: Integer;
  bIsDirectMode: Boolean;
  vOptions, vKeyNames: TStringList;
  R: TRegistry;
begin
  SQLConnection.Close;

  sConnectionString := cbx_connection_string.Text;

  S := AnsiLowerCase(sConnectionString);
  if (S = '') or (S = '?') or (S = 'dsn=?') then
    sConnectionString := '';

  if sConnectionString <> '' then
  begin
    iPos1 := Pos('#2', sConnectionString);
    if iPos1 <= 0 then
      raise Exception.Create('uncorrect connection string (not defined delimiter #2)');
    iPos2 := Pos('#3', sConnectionString);
    if iPos2 <= 0 then
      raise Exception.Create('uncorrect connection string (not defined delimiter #3)');
    if iPos2 <= iPos1 then
      raise Exception.Create('uncorrect connection string (delimiter #2 must be before delimiter #3)');

    sVendorLib := LowerCase(Trim(Copy(sConnectionString, iPos1 + 2, iPos2 - iPos1 - 2)));
    sConnectionString := Trim(Copy(sConnectionString, iPos2 + 2, Length(sConnectionString)));

    if sVendorLib <> '' then
    begin
      iPos1 := Pos('=', sVendorLib);
      if iPos1 <= 0 then
        raise Exception.Create('uncorrect connection string (uncorected section #2. Example: VendorLib=sqlsrv32.dll)');
      S := Trim(Copy(sVendorLib, 1, iPos1-1));
      if S <> 'vendorlib' then
        raise Exception.Create('uncorrect connection string (uncorected section #2. Example: VendorLib=sqlsrv32.dll)');
      sVendorLib := Trim(Copy(sVendorLib, iPos1+1, Length(sVendorLib)));
    end;

    bIsDirectMode := (sVendorLib <> '') and (sVendorLib <> 'odbc32.dll');

    S := AnsiLowerCase(sConnectionString);

    if (s = '') or (s='?') or (s='dsn=?') then
      sConnectionString := '';

  end
  else
    bIsDirectMode := False;

  if not bIsDirectMode then
    sVendorLib := 'odbc32.dll';

  if sConnectionString <> '' then
  begin
    vOptions := TStringList.Create;
    try

      S := StringReplace(S, ';', #13#10, [rfReplaceAll]);
      vOptions.Text := S;
      for iPos1 := 0 to vOptions.Count - 1 do
      begin
        S := Trim(vOptions[iPos1]);
        vOptions[iPos1] := S;
      end;

      S := Trim(vOptions.Values['trusted_connection']);
      if S = 'yes' then
      else if (S = '') then
      begin
        sConnectionString := sConnectionString + ';Trusted_Connection=Yes';
      end
      else
      begin
        S := Trim(EUID.Text);
        if S <> '' then
          sConnectionString := sConnectionString + ';Trusted_Connection=No;UID='
            + Trim(EUID.Text) + ';PWD=' + EPWD.Text
        else
          sConnectionString := sConnectionString + ';Trusted_Connection=Yes';
      end;

      S := vOptions.Values['dsn'];
      if S = '' then
      begin
        S := vOptions.Values['server'];
        if (S = '') or (S = '?') then
        begin
          //
          // autosearch first local instance 2005:
          //

          // [HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Microsoft SQL Server\Instance Names\SQL]
          // "SQLEXPRESS2005"="MSSQL.1"
          // [HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Microsoft SQL Server\MSSQL.1]
          // @="SQLEXPRESS2005"

          S := '';
          vKeyNames := nil;
          R := TRegistry.Create(KEY_READ);
          try
            R.RootKey := HKEY_LOCAL_MACHINE;
            if R.OpenKeyReadOnly('SOFTWARE\Microsoft\Microsoft SQL Server\Instance Names\SQL') then
            begin
              vKeyNames := TStringList.Create;
              R.GetValueNames(vKeyNames);
              if vKeyNames.Count > 0 then
                S := vKeyNames[0];
            end;
          except

          end;
          R.Free;
          vKeyNames.Free;
          if S = '' then
            raise Exception.Create('uncorrect connection string (not defined server name. example: localhost\SQLEXPRESS)');

          S := 'localhost\' + S;
          sConnectionString := 'SERVER=' + S + ';' + sConnectionString;

        end;
      end;

      if not bIsDirectMode then
      begin
        S := vOptions.Values['driver'];
        if S = '' then
        begin
          S := vOptions.Values['dsn'];
          if S = '' then
          begin
            // SQL Server 2005 Native
            if sVendorLib = 'sqlncli.dll' then
              sConnectionString := 'DRIVER=SQL Native Client;' + sConnectionString
            // SQL Server 2000/2005
            else { if sVendorLib = 'sqlsrv32.dll' then }
              sConnectionString := 'DRIVER=SQL Server;' + sConnectionString;
          end;
        end;
      end;

      S := Trim(vOptions.Values['cocatprefix']);
      if S = '' then
        sConnectionString := sConnectionString + ';coCatPrefix=DATABASE';

    finally
      vOptions.Free;
    end;
  end
  else
    sConnectionString := 'DSN=?';

  SQLConnection.LibraryName  := 'dbxoodbc.dll';

  //
  // DBX3
  //
  //{
  SQLConnection.GetDriverFunc := 'getSQLDriverODBCW';
  //DbxOpenOdbcInterface.RegisterDbXpressLibW(); // for static linking
  {}

  // DBX 2
  {$IF CompilerVersion < 18.00}
  SQLConnection.GetDriverFunc := 'getSQLDriverODBC';
  //DbxOpenOdbcInterface.RegisterDbXpressLibA(); // for static linking
  {$IFEND}
  {}

  SQLConnection.Params.Clear;
  //SQLConnection.Params.Values['Trim Char'] := 'True'; { optional }
  SQLConnection.VendorLib := sVendorLib;

  {$IF CompilerVersion > 14.01}
     // Delphi 7 Up
     {$IF CompilerVersion = 18.50}
       // Delphi 2007 bug: not transfer connection options CUSTOM_INFO ("Custom String")
       if Length(sConnectionString) > 255 then
         SetLength(sConnectionString, 255); // Buffer overflow AV protect :(
       SQLConnection.Params.Values[DATABASENAME_KEY]  := sConnectionString;
     {$ELSE}
       SQLConnection.Params.Values[DATABASENAME_KEY]  := '?';
       SQLConnection.Params.Values[CUSTOM_INFO] := cConnectionOptionsNames[coConnectionString] + '=' + sConnectionString;
     {$IFEND}
  {$ELSE}
     // Delphi 6
     if Length(sConnectionString) > 255 then
       SetLength(sConnectionString, 255); // Buffer overflow AV protect :(
     SQLConnection.Params.Values[DATABASENAME_KEY]  := sConnectionString;
  {$IFEND}

  SQLConnection.Open;

  // set option: "Prepare SQL".  { optional }
  {}
{$IF CompilerVersion < 18.50}
  {$IF CompilerVersion >= 18.00} { Delphi 2005 UP }
  SQLConnection.SQLConnection.SetOption(eConnPrepareSQL, LongInt(False));
  {$ELSE} { Delphi 6, 7 }
  SQLConnection.SQLConnection.SetOption(TSQLConnectionOption(xeConnPrepareSQL), LongInt(False));
  {$IFEND}
{$IFEND}
  {}
end;

procedure TForm1.btn_disconnectClick(Sender: TObject);
begin
  SQLConnection.Close;
end;

procedure TForm1.btn_openClick(Sender: TObject);
begin
  if not SQLConnection.Connected then
    Exit;
  CDS.Close;
  SQLDataSet.Close;
  SQLDataSet.CommandText := mem_sql.Lines.Text;
  CDS.Open;
end;

procedure TForm1.cbx_fieldsChange(Sender: TObject);
begin
  db_memo.DataField := '';
  if cbx_fields.ItemIndex >= 0 then
    db_memo.DataField := cbx_fields.Text;
end;

procedure TForm1.cbx_fieldsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    cbx_fieldsChange(cbx_fields);
end;

procedure TForm1.cbx_queryChange(Sender: TObject);
begin
  mem_sql.Lines.Text := cbx_query.Text;
end;

procedure TForm1.CDSAfterOpen(DataSet: TDataSet);
var
  i: Integer;
begin
  cbx_fields.Items.Clear;
  for i := 0 to CDS.FieldCount - 1 do
    cbx_fields.Items.Add(CDS.Fields[i].FieldName);
end;

procedure TForm1.CDSBeforeApplyUpdates(Sender: TObject; var OwnerData: OleVariant);
begin
  FErrorCount := 0;
end;

procedure TForm1.CDSBeforeClose(DataSet: TDataSet);
begin
  db_memo.DataField := '';
  cbx_fields.ItemIndex := -1;
  cbx_fields.Items.Clear;
  cbx_fields.Text := '';
end;

procedure TForm1.DSPUpdateError(Sender: TObject; DataSet: TCustomClientDataSet; E: EUpdateError;
  UpdateKind: TUpdateKind; var Response: TResolverResponse);
begin
//  if AnsiSameText(E.Message, DBConsts.SRecordChanged) then
//  begin
//    Response := rrIgnore;
//    Exit;
//  end;

  Inc(FErrorCount);

  if (FErrorCount = 1) and Assigned(E) then
    MessageDlg(E.Message, mtError, [mbOK], 0);
end;

procedure TForm1.SQLConnectionAfterConnect(Sender: TObject);
begin
  sh_connection_status.Brush.Color := clRed;
end;

procedure TForm1.SQLConnectionAfterDisconnect(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
    sh_connection_status.Brush.Color := clSilver;
end;

end.
