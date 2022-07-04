unit DaoAddOns;

{$I KADaoCommonDirectives.pas}
interface
 Uses Windows, Classes, SysUtils, ComObj, ActiveX{$IFDEF D6UP}, Variants {$ENDIF};

Function CopyTableStructure(Database : OleVariant; OriginalTable, NewTable : String):Boolean;
Function CopyTable(Database : OleVariant; OriginalTable, NewTable : String):Boolean;
Function GetAllDaoEngines(const LicenseString: String):TStringList;


implementation
Uses DaoApi;

Function CopyTableStructure(Database : OleVariant; OriginalTable, NewTable : String):Boolean;
Var
 SQL : String;
Begin
  Result := False;
  SQL := Format('SELECT * INTO [%s] FROM [%s] WHERE 1=2;',[NewTable,OriginalTable]);
  Try
   Database.Execute(SQL,DaoApi.dbFailOnError);
  Except
   Exit;
  End;
  Result := True;
End;

Function CopyTable(Database : OleVariant; OriginalTable, NewTable : String):Boolean;
Var
 SQL : String;
Begin
  Result := False;
  SQL := Format('INSERT INTO [%s] SELECT * FROM [%s];',[NewTable,OriginalTable]);
  Try
   Database.Execute(SQL,DaoApi.dbFailOnError);
  Except
   Exit;                                                                
  End;
  Result := True;
End;                                                              

Function CreateOleDBEngineObject(const ClassName, RuntimeLicense: string): IDispatch;
Const
  DBEngine_TGUID: TGUID = '{00000021-0000-0010-8000-00AA006D2EA4}';
Var
  LicenseClass       : IClassFactory2;
  DWReserved         : DWORD;
  LicenseString      : Widestring;
  ClassID : TGUID;                                  
Begin                                                       
  ClassID := ProgIDToClassID(ClassName);
  //****************************************************************************
  LicenseClass := Nil;
  OleCheck(CoGetClassObject(ClassID,CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, nil, IClassFactory2, LicenseClass));
  if Assigned(LicenseClass) Then
     Begin                                                                                         
       SetLength(LicenseString,2000);
       DWReserved:=0;
       if RuntimeLicense <> '' Then
          LicenseString := RuntimeLicense
       Else
          LicenseClass.RequestLicKey(DWReserved,LicenseString);
       OleCheck(LicenseClass.CreateInstanceLic (nil, nil, DBEngine_TGUID, LicenseString, Result));
     End;
  //****************************************************************************
End;

Function  GetAllDaoEngines(const LicenseString: String):TStringList;
Var
 TempDBEngine      : OleVariant;
Begin
  Result := TStringList.Create;
  Result.Sorted:=True;
  Result.Duplicates:=dupIgnore;
  Try
    TempDBEngine               := CreateOleDBEngineObject('DAO.PrivateDBEngine.35',LicenseString);
    Result.Add('3.5');
    VarClear(TempDBEngine);
  Except
  End;

  Try
    TempDBEngine               := CreateOleDBEngineObject('DAO.PrivateDBEngine.36',LicenseString);
    Result.Add('3.6');
    VarClear(TempDBEngine);
  Except
  End;

  Try
    TempDBEngine               := CreateOleDBEngineObject('DAO.PrivateDBEngine.120',LicenseString);
    Result.Add('12.0');
    VarClear(TempDBEngine);
  Except
  End;

  Try
   TempDBEngine               := CreateOleDBEngineObject('DAO.PrivateDBEngine',LicenseString);
   Result.Add(TempDBEngine.Version);
   VarClear(TempDBEngine);
  Except
  End;
End;

end.
