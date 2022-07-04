unit AccessHelperForAdo;
interface
{$I 'AHADOCD.PAS'}
uses
  SysUtils, Classes, DB, Dialogs;

type
  TAdoHelper = class(TComponent)
  private
    FDatabaseName: String;
    FDatabaseType: String;
    FAdoConnection: TComponent;
    FConnectionString: String;
    FAutoUpdate: Boolean;
    FUseIndexes: Boolean;
    FUserName: String;
    FPassword: String;
    FSystemDatabase: String;
    FAutoOpen: Boolean;
    FActivate: Boolean;
    FDatabasesList: TStringList;
    FDatabasePassword: String;
    procedure SetDatabaseName(const Value: String);
    procedure SetDatabaseType(const Value: String);
    procedure SetAdoConnection(const Value: TComponent);
    procedure GetDBTypesList(List: TStrings;ExtList:TStrings);
    procedure SetConnectionString(const Value: String);
    procedure SetAutoUpdate(const Value: Boolean);
    function GetConnectionString: String;
    procedure SetUseIndexes(const Value: Boolean);
    procedure SetPassword(const Value: String);
    procedure SetUserName(const Value: String);
    procedure SetSystemDatabase(const Value: String);
    procedure SetAutoOpen(const Value: Boolean);
    procedure SetActivate(const Value: Boolean);
    procedure SetDatabasesList(const Value: TStringList);
    procedure SetDatabasePassword(const Value: String);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    DBTypesList : TStringList;
    DBExtList   : TStringList;
    Constructor Create(AOwner:TComponent);Override;
    Destructor  Destroy;Override;
  published
    { Published declarations }
    Property AdoConnection      : TComponent      read FAdoConnection      write SetAdoConnection;
    Property Database           : String          read FDatabaseName       write SetDatabaseName;
    Property SystemDatabase     : String          read FSystemDatabase     write SetSystemDatabase;
    Property DatabaseType       : String          read FDatabaseType       write SetDatabaseType;
    Property ConnectionString   : String          read GetConnectionString write SetConnectionString;
    Property UseIndexes         : Boolean         read FUseIndexes         write SetUseIndexes;
    Property UserName           : String          read FUserName           write SetUserName;
    Property Password           : String          read FPassword           write SetPassword;
    Property DatabasePassword   : String          read FDatabasePassword   write SetDatabasePassword;
    Property AutoUpdate         : Boolean         read FAutoUpdate         write SetAutoUpdate;
    Property AutoOpen           : Boolean         read FAutoOpen           write SetAutoOpen;
    Property DatabasesList      : TStringList     read FDatabasesList      write SetDatabasesList;
    Property Activate           : Boolean         read FActivate           write SetActivate;
  end;

procedure Register;

implementation
Uses
  Windows, TypInfo, Registry;

Const
  cl_adUseServer = 1;
  clUseServer    = 0;
  adUseServer    = 1;

procedure Register;
begin
  RegisterComponents('KA Ado', [TAdoHelper]);
end;

{ TAdoHelper }

constructor TAdoHelper.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAdoConnection     := Nil;
  FDatabaseName      := '';
  FSystemDatabase    := '';
  FDatabaseType      := 'Microsoft Access';
  FConnectionString  := '';
  FUseIndexes        := True;
  FAutoUpdate        := False;
  FAutoOpen          := False;
  FUserName          := 'Admin';
  FPassword          := '';
  FDatabasePassword  := '';
  DBTypesList        := TStringlist.Create;
  DBExtList          := TStringlist.Create;
  FDatabasesList     := DBTypesList;
  GetDBTypesList(DBTypesList,DBExtList);
end;

destructor TAdoHelper.Destroy;
begin
  DBTypesList.Free;
  inherited Destroy;
end;

function TAdoHelper.GetConnectionString: String;
begin
 if (Assigned(FAdoConnection)) And (FDatabaseName<>'') And (FDatabaseType <> '') Then
    Begin
      if FDatabaseType= 'Microsoft Access' Then
         Begin
           Result := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source='+FDatabaseName+';Persist Security Info=False';
           if FUserName         <> '' Then Result := Result + ';User ID='+FUserName;
           if FPassword         <> '' Then Result := Result + ';Password='+FPassword;
           if FSystemDatabase   <> '' Then Result := Result + ';Jet OLEDB:System database='+FSystemDatabase;
           if FDatabasePassword <> '' Then Result := Result + ';Jet OLEDB:Database Password='+FDatabasePassword;
         End
      Else
        Begin
          Result := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source='+FDatabaseName+';Persist Security Info=False;Extended Properties='+FDatabaseType;
          if FUserName <> '' Then Result := Result + ';User ID='+FUserName;
          if FPassword <> '' Then Result := Result + ';Password='+FPassword;
        End;
    End;
end;

Procedure TAdoHelper.GetDBTypesList(List: TStrings;ExtList:TStrings);
var
   Reg  : TRegistry;
   X    : Integer;
   OTPF : Boolean;
   BUF  : Array [1..100] of Byte;
Begin
  List.Clear;
  ExtList.Clear;
  Reg := TRegistry.Create;
  {$IFDEF D5UP} Reg.Access:=KEY_READ; {$ENDIF}
  Try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    {$IFNDEF D4UP}
    if Reg.OpenKey('SOFTWARE\Microsoft\JET\4.0\ISAM Formats',False) then
    {$ELSE}
    if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\JET\4.0\ISAM Formats') then
    {$ENDIF}
       Begin
         Reg.GetKeyNames(List);
         Reg.CloseKey;
       End;
    For X := List.Count-1 Downto 0  do
        Begin
           OTPF := False;
           {$IFNDEF D4UP}
           if Reg.OpenKey('SOFTWARE\Microsoft\JET\4.0\ISAM Formats\'+List.Strings[X],False) then
           {$ELSE}
           if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\JET\4.0\ISAM Formats\'+List.Strings[X]) then
           {$ENDIF}
              Begin
                BUF[1] := 0;
                if Reg.GetDataType('OneTablePerFile') = rdBinary Then
                   Begin
                     Reg.ReadBinaryData('OneTablePerFile',BUF,100)
                   End
                else
                if Reg.GetDataType('OneTablePerFile') = rdInteger Then
                   Begin
                     BUF[1] := Byte(Reg.ReadInteger('OneTablePerFile'))
                   End
                Else
                   BUF[1] := 0;
                OTPF := BUF[1]=1;
                Reg.CloseKey;
              End;
           ExtList.AddObject('All Files (*.*)|*.*',TObject(OTPF));
        End;
    For X := List.Count-1 Downto 0  do
        Begin
          if AnsiCompareText(List.Strings[X],'dBase 5.0')=0   Then ExtList.Strings[X] := ('dBASE 5 (*.dbf)|*.dbf'+'|All Files (*.*)|*.*');
          if AnsiCompareText(List.Strings[X],'dBase III')=0   Then ExtList.Strings[X] := ('dBASE III (*.dbf)|*.dbf'+'|All Files (*.*)|*.*');
          if AnsiCompareText(List.Strings[X],'dBase IV')=0    Then ExtList.Strings[X] := ('dBASE IV (*.dbf)|*.dbf'+'|All Files (*.*)|*.*');
          if AnsiCompareText(List.Strings[X],'Excel 3.0')=0   Then ExtList.Strings[X] := ('Microsoft Excel 3 (*.xls)|*.xls'+'|All Files (*.*)|*.*');
          if AnsiCompareText(List.Strings[X],'Excel 4.0')=0   Then ExtList.Strings[X] := ('Microsoft Excel 4 (*.xls)|*.xls'+'|All Files (*.*)|*.*');
          if AnsiCompareText(List.Strings[X],'Excel 5.0')=0   Then ExtList.Strings[X] := ('Microsoft Excel 5-7 (*.xls)|*.xls'+'|All Files (*.*)|*.*');
          if AnsiCompareText(List.Strings[X],'Excel 8.0')=0   Then ExtList.Strings[X] := ('Microsoft Excel 97-2000 (*.xls)|*.xls'+'|All Files (*.*)|*.*');
          if AnsiCompareText(List.Strings[X],'HTML Export')=0 Then ExtList.Strings[X] := ('HTML Documents (*.html;*.htm)|*.html;*.htm'+'|All Files (*.*)|*.*');
          if AnsiCompareText(List.Strings[X],'HTML Import')=0 Then ExtList.Strings[X] := ('HTML Documents (*.html;*.htm)|*.html;*.htm'+'|All Files (*.*)|*.*');

          if AnsiCompareText(List.Strings[X],'Lotus WJ2')=0   Then ExtList.Strings[X] := ('Lotus 1-2-3/DOS (*.wj*)|*.wj*'+'|All Files (*.*)|*.*');
          if AnsiCompareText(List.Strings[X],'Lotus WJ3')=0   Then ExtList.Strings[X] := ('Lotus 1-2-3/DOS (*.wj*)|*.wj*'+'|All Files (*.*)|*.*');
          if AnsiCompareText(List.Strings[X],'Lotus WK1')=0   Then ExtList.Strings[X] := ('Lotus 1-2-3 (*.wk*)|*.wk*'+'|All Files (*.*)|*.*');
          if AnsiCompareText(List.Strings[X],'Lotus WK3')=0   Then ExtList.Strings[X] := ('Lotus 1-2-3 WK3 (*.wk3)|*.wk3'+'|All Files (*.*)|*.*');

          if AnsiCompareText(List.Strings[X],'Lotus WK4')=0   Then ExtList.Strings[X] := ('Lotus 1-2-3 WK4 (*.wk4)|*.wk4'+'|All Files (*.*)|*.*');
          if AnsiCompareText(List.Strings[X],'Paradox 3.X')=0 Then ExtList.Strings[X] := ('Paradox (*.db)|*.db'+'|All Files (*.*)|*.*');
          if AnsiCompareText(List.Strings[X],'Paradox 4.X')=0 Then ExtList.Strings[X] := ('Paradox 4 (*.db)|*.db'+'|All Files (*.*)|*.*');
          if AnsiCompareText(List.Strings[X],'Paradox 5.X')=0 Then ExtList.Strings[X] := ('Paradox 5 (*.db)|*.db'+'|All Files (*.*)|*.*');
          if AnsiCompareText(List.Strings[X],'Paradox 7.X')=0 Then ExtList.Strings[X] := ('Paradox 7-8 (*.db)|*.db'+'|All Files (*.*)|*.*');
          if AnsiCompareText(List.Strings[X],'Text')=0        Then ExtList.Strings[X] := ('Text Files (*.txt;*.csv;*.tab;*.asc)|*.txt;*.csv;*.tab;*.asc'+'|All Files (*.*)|*.*');
          
          if (AnsiCompareText(List.Strings[X],'Jet 2.x')=0)
          OR (AnsiCompareText(List.Strings[X],'Jet 3.x')=0)
          OR (AnsiCompareText(List.Strings[X],'Outlook 9.0')=0)
          OR (AnsiCompareText(List.Strings[X],'WSS')=0)
          OR (AnsiCompareText(List.Strings[X],'Exchange 4.0')=0) Then
             Begin
               List.Delete(X);
               ExtList.Delete(X);
             End;
        End;
    List.Insert(0,'Microsoft Access');
    ExtList.Insert(0,'Microsoft Access Database (*.mdb)|*.mdb|All Files (*.*)|*.*');
  Finally
     Reg.Free;
  End;
End;

procedure TAdoHelper.SetActivate(const Value: Boolean);
begin
  FActivate := Value;
  if csLoading in ComponentState then Exit;
  if Value Then
     Begin
       if NOT Assigned(FAdoConnection) Then
          Begin
            FActivate := False;
            Raise Exception.Create('AdoConnection property not set!');
          End;
       if FDatabaseName='' Then
          Begin
            FActivate := False;
            Raise Exception.Create('Database property not set!');
          End;
     End;
  if FActivate Then
     Begin
       If FAutoOpen Then SetPropValue(FAdoConnection,'Connected',False);
       if FAutoUpdate Then
          Begin
            if FUseIndexes Then
               Begin
                 if (AnsiCompareText(FAdoConnection.ClassName,'TKAAdoDatabase')=0)   Then SetPropValue(FAdoConnection,'CursorLocation',adUseServer);
                 if (AnsiCompareText(FAdoConnection.ClassName,'TAdoConnection')=0)   Then SetPropValue(FAdoConnection,'CursorLocation',clUseServer);
                 if (AnsiCompareText(FAdoConnection.ClassName,'TAoAdoConnection')=0) Then SetPropValue(FAdoConnection,'CursorLocation',cl_adUseServer);
               End;
            SetStrProp(FAdoConnection,'ConnectionString',ConnectionString);
          End;
       If FAutoOpen Then SetPropValue(FAdoConnection,'Connected',True);
     End;
end;

procedure TAdoHelper.SetAdoConnection(const Value: TComponent);
begin
  FAdoConnection := Value;
end;

procedure TAdoHelper.SetAutoOpen(const Value: Boolean);
begin
  FAutoOpen := Value;
end;

procedure TAdoHelper.SetAutoUpdate(const Value: Boolean);
begin
  FAutoUpdate := Value;
end;

procedure TAdoHelper.SetConnectionString(const Value: String);
begin
  FConnectionString := Value;
end;

procedure TAdoHelper.SetDatabaseName(const Value: String);
begin
  FDatabaseName := Value;
end;

procedure TAdoHelper.SetDatabasePassword(const Value: String);
begin
  FDatabasePassword := Value;
end;

procedure TAdoHelper.SetDatabasesList(const Value: TStringList);
begin
  //************************************
end;

procedure TAdoHelper.SetDatabaseType(const Value: String);
begin
  FDatabaseType := Value;
  if csLoading in ComponentState Then Exit;
  FDatabaseName := '';
end;

procedure TAdoHelper.SetPassword(const Value: String);
begin
  FPassword := Value;
end;

procedure TAdoHelper.SetSystemDatabase(const Value: String);
begin
  FSystemDatabase := Value;
end;

procedure TAdoHelper.SetUseIndexes(const Value: Boolean);
begin
  FUseIndexes := Value;
end;

procedure TAdoHelper.SetUserName(const Value: String);
begin
  FUserName := Value;
end;

end.
