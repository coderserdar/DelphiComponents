{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

program TVISDemo;

{$I BrDefOpt.Inc}
{$R-,S-,F+,V-,P-}

uses
  HeapChk,
  Dos,
  Objects,
  Drivers,
  Memory,
  Views,
  Editors,
  Menus,
  Dialogs,
  Validate,
  MsgBox,
  App,
  CCSKeys,                                                        {!!.05}
{$IFDEF BRUseShell}
  OPSBase,
  OOPShell,
  OPSCodes,
  TVStatus,
  TVRebld,
{$ENDIF}
{$IFDEF BRUseIsam}
  BTBase,
  BTIsBase,
  Filer,     {!!.TP}
  Rebuild,
{$ENDIF}
  LowBrows,
  TVBrows;

const
  WinCount: Integer =   0;
  cmNewWin          = 100;
  cmTile            = 101;
  cmCascade         = 102;
  cmAbout           = 103;

  cmAdd             = 120;
  cmEdit            = 121;
  cmKey             = 122;
  cmDelete          = 123;
  cmSearch          = 124;
  cmStatus          = 125;
  cmDisplay         = 126;

  cmUpdate          = 130;

  MaxEMSPages = 250;
  HandlesreservedForShell = 7;
  TheFileBlockname = 'Adressen';
  Key1Len = 30;
  Key2Len = 8;

  EDLoc : TRect = ( A : ( X : 10; Y : 8 );
                    B : ( X : 60; Y : 20 ));

  SearchExpr : String = '';

  LowIntChar = #128;                                                  {!!.03a}
  HiIntChar = #165;                                                   {!!.03a}

(*  ToKeyTable : Array [LowIntChar..HiIntChar] Of Char = (            {!!.05rem}
      'C', 'U', 'E', 'A', 'A', 'A', 'A', 'C', 'E', 'E', 'E', 'I', 'I',
      'I', 'A', 'A', 'E', 'A', 'A', 'O', 'O', 'O', 'U', 'U', 'Y', 'O',
      'U', '›', 'œ', '', 'ž', 'Ÿ', 'A', 'I', 'O', 'U', 'N', 'N' ); *)


Var
  TheHeader : BRLRowEltString;
{$IFDEF BrUseShell}
  SupNet : IFDNetSupportType;
{$ENDIF}
{$IFDEF BrUseIsam}
  SupNet : NetSupportType;
{$ENDIF}

Type
  PTVISDemoApp = ^TTVISDemoApp;                                   {!!.05}
  TTVISDemoApp = object( TApplication )
    SourceDestCP  : Word;                                         {!!.05}
    OtherCP       : LongInt;                                      {!!.05}
    Constructor Init;
    Procedure HandleEvent ( Var Event: TEvent ); Virtual;
    Procedure InitMenuBar; Virtual;
    Procedure InitStatusLine; Virtual;
    Procedure NewWindow;
    Procedure Idle; Virtual;
    Function InitDataBase : Boolean;

  End;

  PCodePageValidator = ^TCodePageValidator;                       {!!.05}
  TCodePageValidator = Object ( TValidator )                      {!!.05}
    Procedure Error; Virtual;                                     {!!.05}
    Function IsValid ( Const S: string ): Boolean; Virtual;       {!!.05}
    Function IsValidInput ( Var S: string;                        {!!.05}
        SuppressFill: Boolean ): Boolean; Virtual;                {!!.05}
  end;                                                            {!!.05}

  PPersBrowser = ^TPersBrowser;
  TPersBrowser = Object ( TBrowserWindow )
  {$IFDEF BRUseIsam}
    Constructor Init ( var Bounds               : TRect;
                           ATitle               : TTitleStr;
                           ANumber              : Integer;
                           ADrvOrFileBlockPtr   : Pointer;
                           ANumberOfEltsPerRow  : Word;
                           ANumberOfRows        : Word;
                           AKeyNr               : Word;
                           ALKey,
                           AHKey                : GenKeyStr;
                           AHeader,
                           AFooter              : BRLRowEltString;
                       Var ADatS;
                           AIsVarRec            : Boolean );

{$ENDIF}

{$IFDEF BRUseShell}
    Constructor Init ( Var Bounds               : TRect;
                           ATitle               : TTitleStr;
                           ANumber              : Integer;
                           ADrvOrFileBlockPtr   : Pointer;
                           ANumberOfEltsPerRow : Word;
                           ANumberOfRows        : Word;
                           AKeyNr               : Word;
                           ALKey,
                           AHKey                : GenKeyStr;
                           AHeader,
                           AFooter              : BRLRowEltString );
{$ENDIF}

    Function BuildRow ( Var RR         : RowRec ) : Integer; Virtual;
    Procedure HandleEvent ( Var Event: TEvent ); Virtual;

    Procedure EditRec;
    Procedure NewRec;
    Procedure DelRec;
    Procedure Search;
    Procedure Status;
    Procedure ToggleKey;
    Procedure SearchByKey ( C : Char );

    Function EditDataRec ( ATitle : String;
                           DisplayOnly : Boolean ) : Boolean;
  End;


  PPersondef = ^Persondef;
  Persondef = Record
    Dele : LongInt;
    Name : String [25];
    Vorname : String [20];
    Land : String[3];
    Plz : String [5];
    Wohnort : String [30];
    Strasse : String [30];
    Telefon : String [15];
  End;

  Var
    Person  : PersonDef;
{$IFDEF BRUseShell}
    PersD   : IFDriverPtr;
{$ENDIF}
{$IFDEF BRUseIsam}
    PersFB  : IsamFileBlockPtr;
{$ENDIF}


  Function GetTimeMs : LongInt;
  Var
    Regs : Registers;
  Begin
    With Regs Do Begin
      AH := $2C;
      MsDos(Regs);
      GetTimeMs := 1000 * ( LongInt ( DH ) +
                   60 * ( LongInt ( CL ) +
                   60 * LongInt ( CH ) ) ) +
                   10 * LongInt ( DL );
    End;
  End;


  Function RemoveBlanks (S : String) : String;

  Var
    I : Word;
    SLen : Byte absolute S;

  Begin
    While ( SLen > 0 ) And ( S [ SLen ] <= ' ' ) Do
      Dec ( SLen );
    I := 1;
    While ( I <= SLen ) And ( S [ I ] <= ' ' ) Do
      Inc ( I );
    Dec ( I );
    If I > 0 Then
      Delete ( S, 1, I );
    RemoveBlanks := S;
  End;


(*  Function IntStrToKey ( S : String ) : String;                 {!!.05rem}

  Var
    I : Integer;

  Begin
    For I := 1 To Length (S) Do Begin
      Case S [I] Of
        LowIntChar..HiIntChar : Begin
          S [I] := ToKeyTable [S [I]];
        End;
        Else Begin
          S [I] := UpCase (S [I]);
          If S [I] = 'á' Then S [I] := 'S';
        End;
      End; {Case}
    End;
    IntStrToKey := S;
  End; *)


  Function YesNo ( S : String; MsgType : Byte ) : Boolean;

  Begin
    YesNo := MessageBox ( S, Nil , MsgType and $3 Or mfYesButton Or mfNoButton ) = cmYes;
  End;


  Procedure OkBox ( S : String; MsgType : Byte  );

  Begin
    If MessageBox ( S, Nil , MsgType And $3 Or mfOKButton ) = cmOk Then;
  End;


  Procedure OkBoxRect ( R : TRect; S : String; MsgType : Byte  );

  Begin
    If MessageBoxRect ( R, S, Nil , MsgType And $3 Or mfOKButton ) = cmOk Then;
  End;


  Function Extend ( S : String; Anz : Byte ) : String;

  Var
    S1 : String [255];

  Begin
    S1 := S;
    While Length (S1) < Anz Do S1 := S1 + ' ';
      S1 := Copy ( S1, 1, Anz );
    Extend := S1;
  End;

  Function LeftPadCH ( S : String; Ch : Char; Len : Byte ) : String;

  Var
    TempS : String;

  Begin
    If Len > Length (S) Then Begin
      FillChar ( TempS [1], Len - Length (S), Ch );
      TempS [0] := Chr (Len - Length (S));
      LeftPadCH := TempS + S;
    End Else Begin
      LeftPadCH := S;
    End;
  End;


  Function PadCH ( S : String; Ch : Char; Len : Byte) : String;

  Var
    TempS : String;

  Begin
    If Len > Length (S) Then Begin
      FillChar ( S [Succ (Length (S))], Len - Length (S), Ch );
      S [0] := Chr (Len);
    End;
    PadCH := S;
  End;


  Function Trim ( S : String ) : String;

  Var
    I    : Word;
    SLen : Byte Absolute S;

  Begin
    While (SLen > 0) And (S [SLen] <= ' ') Do Begin
      Dec (SLen);
    End;

    I := 1;
    While (I <= SLen) And (S[I] <= ' ') Do Begin
      Inc (I);
    End;
    If I > 1 Then Delete (S, 1, Pred (I));

    Trim := S;
  End;


{$IFDEF BRUseShell}
  Function BuildPersKey ( Var P; KeyNr : Word ) : IFDKeyStr;
{$ENDIF}
{$IFDEF BRUseIsam}
  Function BuildPersKey ( Var P; KeyNr : Word ) : IsamKeyStr;
{$ENDIF}

  Var
    TempS : String;                                               {!!.05}

  Begin
    TempS := '';                                                  {!!.05}
    WITH PersonDef (P) Do
      CASE KeyNr OF
        1: TempS := Extend ( Name, 20 ) +                         {!!.05mod}
            Extend ( Vorname, 10 );
        2: TempS :=                                               {!!.05mod}
            PadCh ( Trim ( Land ), ' ', 3 ) +
            LeftPadCh ( Trim ( Plz ) ,' ', 5 );
      End;
      BuildPersKey := NLStrToKey ( TempS,
         PTVISDemoApp (Application)^.SourceDestCP,
         PTVISDemoApp (Application)^.OtherCP );                   {!!.05}
  End;

  Procedure DemoCharConvert ( DataPtr  : Pointer;                 {!!.05}
                              DataLen  : LongInt;
                              PostRead : Boolean;
                              HookPtr  : Pointer ); Far;

  Var
    SDCP : Word;
    OCP  : LongInt;

  Begin
    If PostRead Then Begin
      SDCP := Swap ( PTVISDemoApp (Application)^.SourceDestCP );
    End Else Begin
      SDCP := PTVISDemoApp (Application)^.SourceDestCP;
    End;
    OCP := PTVISDemoApp (Application)^.OtherCP;
    With PPersonDef (DataPtr)^ Do Begin
      ConvertChars ( @Name [1], Length ( Name ), SDCP, OCP );
      ConvertChars ( @Vorname [1], Length ( Vorname ), SDCP, OCP );
      ConvertChars ( @Land [1], Length ( Land ), SDCP, OCP );
      ConvertChars ( @Plz [1], Length ( Plz ), SDCP, OCP );
      ConvertChars ( @Wohnort [1], Length ( Wohnort ), SDCP, OCP );
      ConvertChars ( @Strasse [1], Length ( Strasse ), SDCP, OCP );
      ConvertChars ( @Telefon [1], Length ( Telefon ), SDCP, OCP );
    End;
  End;

  Procedure TCodePageValidator.Error;                             {!!.05}
  Begin
    OKBox ( 'You have entered an unsupported character.', mfError );
  End;

  Function TCodePageValidator.IsValid ( Const S: string ):
                                                         Boolean; {!!.05}

  Begin
    IsValid := CheckChars ( @S [1], Length ( S ),
        PTVISDemoApp (Application)^.SourceDestCP,
        PTVISDemoApp (Application)^.OtherCP ) = 0;
  End;

  Function TCodePageValidator.IsValidInput ( Var S: string;
        SuppressFill: Boolean ): Boolean;                         {!!.05}
  Begin
    IsValidInput := CheckChars ( @S [1], Length ( S ),
        PTVISDemoApp (Application)^.SourceDestCP,
        PTVISDemoApp (Application)^.OtherCP ) = 0;
  End;

{$IFDEF BRUseIsam}
  {$I TVIsStuf.Inc}
  Procedure TPersBrowser.Status;

  Begin
    OKBox ( 'Status not available.', mfError) {!!.TP}
  End;
{$ENDIF}


{$IFDEF BRUseShell}
  Function UserAnswer ( DrName : IFDriverName; Msg : IFDUserMsgType )
                                                                   : Boolean;

  Begin
    UserAnswer := False;
    Case Msg Of
      IULockViolation :
          UserAnswer := YesNo ( DrName +
          ' record or file is locked. Try again ', {!!.TP}
          mfConfirmation );
      IULockRec :
          UserAnswer := YesNo ( DrName +
          ' <LockRec> failed. Try again ', {!!.TP}
          mfConfirmation );
      IULockFile :
          UserAnswer := YesNo ( DrName +
          ' <LockFileBlock> failed. Try again ', {!!.TP}
          mfConfirmation );
      IUIndexDefect :
          UserAnswer := YesNo ( DrName +
          ' Index is defective or invalid. Rebuild ', {!!.TP}
          mfConfirmation );
      IUFileNotExist :
          UserAnswer := YesNo ( DrName + ' does not exist. Create new ', {!!.TP}
          mfConfirmation );
      IURecDeleted :
          UserAnswer := YesNo ( DrName +
          ' record was deleted. Create new ', {!!.TP}
          mfConfirmation );
      IUDriveNotReady :
          UserAnswer:= YesNo ( DrName +
          ': drive not ready. Try again ', {!!.TP}
          mfConfirmation );
      IURebuildOnlyIndex :
          UserAnswer := YesNo ( DrName +                          {!!.05}
          ': Rebuild only the index ', {!!.TP}
          mfConfirmation );
      Else
        UserAnswer:= YesNo ( DrName +
            ': unknown error. Try again ', {!!.TP}
            mfConfirmation );
    End;
  End;
{$ENDIF}


  Function TPersBrowser.EditDataRec ( ATitle: String;
                                      DisplayOnly : Boolean ) : Boolean;
  Var
    Dialog  : PDialog;
    Control : PView;
    R       : TRect;
    C       : Word;

  Procedure CreateField ( ALine : Integer; ASize : Integer; ALabel : String );

  Var
    Control : PInputLine;                                         {!!.05mod}

  Begin
    With Dialog^ Do begin
      R.Assign( 14, ALine, ASize + 16, ALine +1 );
      Control := New(PInputLine, Init(R, ASize));
      If DisplayOnly Then Begin                                   {!!.05mod}
        Control^.Options := Control^.Options And ( ofSelectable Xor $FFFF);
      End Else Begin                                              {!!.05}
        Control^.SetValidator ( New (PCodePageValidator, Init )); {!!.05}
      End;                                                        {!!.05}
      Insert(Control);
      R.Assign( 1, ALine, 10, ALine +1 );
      Insert ( New(PLabel, Init(R, ALabel, Control )));
    End;
  End;

  Begin
    EditDataRec := False;
    R.Copy ( EDLoc );
    Dialog := New ( PDialog, Init ( R, ATitle ));
    With Dialog^ Do Begin
      CreateField ( 1, Sizeof ( Person.Name ) -1, 'LastName' );       {!!.TP}
      CreateField ( 2, Sizeof ( Person.Vorname ) -1, 'FirstName' );   {!!.TP}
      CreateField ( 3, Sizeof ( Person.Land ) -1, 'Country' );        {!!.TP}
      CreateField ( 4, Sizeof ( Person.PLZ ) -1, 'Zip' );             {!!.TP}
      CreateField ( 5, Sizeof ( Person.Wohnort ) -1, 'City' );        {!!.TP}
      CreateField ( 6, Sizeof ( Person.Strasse ) -1, 'Street' );      {!!.TP}
      CreateField ( 7, Sizeof ( Person.Telefon ) -1, 'Telephone' );   {!!.TP}

      R.Assign ( 10, Size.Y - 3,  20, Size.Y - 1 );
      Insert ( New ( PButton, Init(R, 'O~K~', cmOk, bfDefault )));
      R.A.X := Size.X - 20; R.B.X := Size.X - 10;
      Insert ( New ( PButton, Init ( R, 'Cancel', cmCancel, bfNormal ))); {!!.TP}
      Inc ( R.A.X, 12 ); Inc ( R.B.X, 12 );
      SelectNext ( False );
    End;
    Dialog^.SetData ( Person.Name );
    C := DeskTop^.ExecView ( Dialog );
    If C <> cmCancel Then
        Dialog^.GetData ( Person.Name );
    Dialog^.GetBounds ( Edloc );
    Dispose ( Dialog, Done );
    EditDataRec := C <> cmCancel;
  End;


  Function TPersBrowser.BuildRow ( Var RR    : RowRec ) : Integer;

  Begin
    BuildRow := NoError;
    With RR, Person Do Begin
      Row := ' ';                                                 {!!.03amod}
      If Status = BRCurrentlyLocked Then Begin                    {!!.03amod}
        While Length ( Row ) < MaxCols Do Begin                   {!!.03amod}
          Row := Row + '**   ';                                   {!!.03amod}
        End;                                                      {!!.03amod}
      End Else Begin                                              {!!.03amod}
        Row := Row + Extend ( RemoveBlanks ( Name ) + ', ' +
                              RemoveBlanks ( Vorname ) , 25 ) + ' ' +
                     Extend ( Telefon, 15) + ' ' +
                     Extend ( RemoveBlanks (Land)                 {!!.03amod}
                         + '-' + RemoveBlanks (Plz), 9 ) + ' ' +  {!!.03amod}
                     Extend ( Wohnort, 20 ) + ' ' +
                     Extend ( Strasse, 20 );
      End;
    End;
  End;

{$IFDEF BrUseIsam}
  Constructor TPersbrowser.Init ( Var Bounds               : TRect;
                                      ATitle               : TTitleStr;
                                      ANumber              : Integer;
                                      ADrvOrFileBlockPtr   : Pointer;
                                      ANumberOfEltsPerRow  : Word;
                                      ANumberOfRows        : Word;
                                      AKeyNr               : Word;
                                      ALKey,
                                      AHKey                : GenKeyStr;
                                      AHeader,
                                      AFooter              : BRLRowEltString;
                                  Var ADatS;
                                      AIsVarRec            : Boolean );

{$ENDIF}
{$IFDEF BrUseShell}
  Constructor TPersbrowser.Init ( Var Bounds               : TRect;
                                      ATitle               : TTitleStr;
                                      ANumber              : Integer;
                                      ADrvOrFileBlockPtr   : Pointer;
                                      ANumberOfEltsPerRow  : Word;
                                      ANumberOfRows        : Word;
                                      AKeyNr               : Word;
                                      ALKey,
                                      AHKey                : GenKeyStr;
                                      AHeader,
                                      AFooter              : BRLRowEltString );
{$ENDIF}
  var
    R: TRect;
    MenuBar : PMenuBar;

  Begin
{$IFDEF BrUseShell}
    If TBrowserWindow.Init ( Bounds,
                             ATitle,
                             ANumber,
                             ADrvOrFileBlockPtr,
                             ANumberOfEltsPerRow,
                             ANumberOfRows,
                             AKeyNr,
                             ALKey,
                             AHKey,
                             AHeader,
                             AFooter ) Then Begin
{$ENDIF}

{$IFDEF BRUseIsam}
    If TBrowserWindow.Init ( Bounds,
                             ATitle,
                             ANumber,
                             ADrvOrFileBlockPtr,
                             ANumberOfEltsPerRow,
                             ANumberOfRows,
                             AKeyNr,
                             ALKey,
                             AHKey,
                             AHeader,
                             AFooter,
                             ADatS,
                             AIsVarRec ) Then Begin
{$ENDIF}
      PInterior^.GetBounds ( R );
      Dec ( R.B.Y );
      PInterior^.ChangeBounds ( R );
      GetExtent ( R );
      R.A.Y := R.B.Y - 2;
      Dec ( R.B.Y );
      Inc ( R.A.X );
      Dec ( R.B.X );
      MenuBar := New ( PMenuBar, Init( R , NewMenu (
        NewItem ( '~N~ew', '', kbAltN, cmAdd, hcNoContext,       {!!.TP}
        NewItem ( '~E~dit', '', kbAltE, cmEdit, hcNoContext,
        NewItem ( 'S~o~rt', '', kbAltO, cmKey, hcNoContext,
        NewItem ( '~D~elete', '', kbAltL, cmDelete, hcNoContext, {!!.TP}
        NewItem ( '~S~earch', '', kbAltS, cmSearch, hcNoContext, {!!.TP}
        NewItem ( 'S~t~atus', '', kbAltT, cmStatus, hcNoContext,
        Nil )))))))));
      MenuBar^.DragMode := dmLimitAll;
      MenuBar^.GrowMode := gfGrowAll xor gfGrowLoX;
      Insert ( Menubar );
    End Else
      Fail;
  End;

{$IFDEF BrUseShell}
  Procedure TPersBrowser.NewRec;
  Begin
    PersD^.IFDClearWorkBuffer;
    If EditDataRec ( ' New ', False ) Then Begin {!!.TP}
      PersD^.IFDAddRec;
      If MaxErrorClass > 0 Then Begin
        OKBox ( 'Database Error', mfError );    {!!.TP}
        PersD^.IFDRestoreWorkBuffer;
      End Else
      SetAndUpdateBrowserScreen (
        PersD^.IFDBuildKeyWithNumber ( GetCurrentKeyNr ),
        GetCurrentDatRef );
    End;
  End;


  Procedure TPersBrowser.EditRec;

  Var                                                             {!!.03}
    Match : Boolean;                                              {!!.03}

  Begin
    If  GetCurrentRec ( Match ) > 0 Then Begin                    {!!.03mod}
      OkBox ( 'Read failed.', mferror);                           {!!.TP}
      Exit;
    End;
    If Not Match Then Begin                                       {!!.03}
      OKBox ( 'The record has been changed in the meantime.',  {!!.03}{!!.TP}
               mfInformation);                                    {!!.03}
      Exit;                                                       {!!.03}
    End;                                                          {!!.03}
    If EditDataRec ( ' Edit ', False ) Then Begin
      PersD^.IFDChangeRec;
      If MaxErrorClass > 0 Then Begin
        OKBox ( 'Database error', mfError); {!!.TP}
        PersD^.IFDRestoreWorkBuffer;
      End Else
        SetAndUpdateBrowserScreen (
            PersD^.IFDBuildKeyWithNumber ( GetCurrentKeyNr ),
            GetCurrentDatRef );
    End;
  End;


  Procedure TPersBrowser.DelRec;

  Var                                                             {!!.03}
    Match : Boolean;                                              {!!.03}

  Begin
    If  GetCurrentRec ( Match ) > 0 Then Begin                    {!!.03mod}
      OkBox ( 'Read Failed.', mferror);                           {!!.TP}
      Exit;
    End;
    If Not Match Then Begin                                       {!!.03}
      OKBox ( 'The record has been changed in the meantime.',  {!!.03}{!!.TP}
               mfInformation);                                    {!!.03}
      Exit;                                                       {!!.03}
    End;                                                          {!!.03}
    If EditDataRec ( ' Delete ', True ) Then Begin {!!.TP}
      PersD^.IFDDeleterec ( IpNone );
      If MaxErrorClass > 0 Then Begin
        OKBox ( 'Database error', mfError);       {!!.TP}
      End Else Begin
        UpDateBrowserScreen;
      End;
    End;
  End;

  Procedure TPersBrowser.Search;

  Var
    UK : IFDKeyStr;
    NewKeyNr : Integer;

  Begin
    PersD^.IFDClearWorkBuffer;
    NewKeyNr := 0;
    If EditDataRec ( ' Search', False ) Then Begin {!!.TP}
      UK := PersD^.IFDBuildKeyWithNumber ( 1 );
      If Length ( UK ) > 0 Then If UK [1] > ' ' Then Begin
        NewKeyNr := 1;
      End Else Begin
        UK := PersD^.IFDBuildKeyWithNumber ( 2 );
        If Length ( UK ) > 0 Then If UK [1] > ' ' Then Begin
          NewKeynr := 2;
        End Else begin
          OKBox ( 'No records matched search criteria.', mfError ); {!!.TP}
          Exit;
        End;
      End;
      PersD^.IFDSearchRec ( NewKeyNr, UK );
      If MaxErrorClass > 0 Then Begin
        OKBox ( 'Search failed', mfError) {!!.TP}
      End Else Begin
        With PersD^ Do Begin
          SetKeyNr ( NewKeyNr );
          SetAndUpdateBrowserScreen (
              IFDBuildKeyWithNumber ( IFDGetActiveKeyNr ), IFDGetWorkRefNr );
        End;
      End;
    End;
  End;

  Procedure TPersBrowser.Status;

  Begin
    DisplayStatusInfo ( PersD );
  End;

  Procedure TPersbrowser.ToggleKey;

  Var                                                             {!!.03}
    Match : Boolean;                                              {!!.03}

  Begin
    If GetCurrentRec ( Match ) = 0 Then Begin                    {!!.03mod}
      SetKeyNr ( (GetCurrentKeyNr + 1) Mod 3 );                  {!!.04amod}
      SetAndUpdateBrowserScreen (
          PersD^.IFDBuildKeyWithNumber ( GetCurrentKeyNr ),
          GetCurrentDatRef );
    End;
  End;

  Procedure TPersBrowser.SearchByKey ( C : Char );

  Var                                                             {!!.05}
    S : String;                                                   {!!.05}

  Begin
    S := C;                                                       {!!.05mod}
    S := NLStrToKey ( S, PTVISDemoApp (Application)^.SourceDestCP,
        PTVISDemoApp (Application)^.OtherCP );                    {!!.05mod}
    PersD^.IFDSearchRec ( GetCurrentKeyNr, S );                   {!!.05mod}
    If MaxErrorClass = 0 Then Begin
      SetAndUpdateBrowserScreen (
          PersD^.IFDBuildKeyWithNumber ( GetCurrentKeyNr ),
          0 );
    End;
  End;
{$ENDIF}


  Procedure TPersBrowser.HandleEvent ( Var Event : TEvent );

  Begin
    TBrowserWindow.HandleEvent ( Event );
    If Event.What = evNothing Then Exit;
    If ( Event.What = evBroadcast ) Then Begin
      Case Event.Command Of
        cmUpdate : Begin
          UpdateBrowserScreen;
          If Not GetBrowseStatus Then Begin
            ClearEvent ( Event );
          End;
          Exit;
        End;
        cmListItemSelected : Begin
          If ( Event.InfoPtr = PInterior ) Then
            EditRec
          Else
            Exit;
        End;
      End;
    End;

    If Event.What = evCommand Then Begin
      Case Event.Command Of
        cmAdd : NewRec;
        cmEdit : EditRec;
        cmKey : ToggleKey;
        cmDelete : DelRec;
        cmSearch : Search;
        cmStatus : Status;
      Else
        Exit;
      End;
    End;

    If Event.What = evKeydown Then Begin
      Case Event.CharCode Of
        ' ',
        '0'..'9',
        'a'..'z',
        'A'..'Z',
        LowIntChar..HiIntChar,                                    {!!.03amod}
        'á' : Begin                                               {!!.03amod}
           SearchByKey ( Event.CharCode );                        {!!.03amod}
        End;
        #13 : EditRec;
      Else
        Exit;
      End;
    End;

    ClearEvent ( Event );
  End;


  { TTVISDemoApp }
  Procedure TTVISDemoApp.HandleEvent(var Event: TEvent);

{$IFNDEF Ver70}
  Procedure Tile;

  Var
    R : TRect;

  Begin
    With DeskTop^ Do Begin
      GetExtent ( R );
      Tile ( R );
    End;
  End;

  Procedure Cascade;

  Var
    R : TRect;

  Begin
    With DeskTop^ Do Begin
      GetExtent ( R );
      Cascade ( R );
    End;
  End;

{$ENDIF}

  Procedure About;
  Var
    R : TRect;
  Begin
    R.Assign ( 15, 3, 66, 13 );
    OkBoxRect ( R, '  B-Tree Filer - Turbo Vision Demo'+     {!!.04amod}{!!.TP}
        #13 + #13 +
        'Copyright (C) 1991-94 ENZ EDV-Beratung GmbH', mfInformation );
  End;

  Begin
    TApplication.HandleEvent(Event);
    If Event.What = evCommand Then Begin
      Case Event.Command Of
        cmNewWin  : NewWindow;
        cmTile    : Tile;
        cmCascade : Cascade;
        cmAbout   : About;
        cmDisplay : If HiResScreen Then
                      SetScreenMode ( ScreenMode Xor smFont8x8 );
      Else
        Exit;
      End;
      ClearEvent(Event);
    End;
  End;

  Function TTVISDemoApp.InitDataBase : Boolean;

    {$IFDEF BRUseShell}
      Function InitPF : Boolean;

      Var
        NetFileBlock : Boolean;

      Begin
        InitPF := False;
        NetFileBlock := IFDNetSupported <> NoNet;
        New ( PersD, Init ( 'Address', TheFileBlockName,
            YesNo ( 'Open the Fileblock in Save mode?', mfConfirmation), NetFileBlock,{!!.TP}
            False, True, SizeOf (Persondef), 2, @Person, SizeOf ( Persondef ),
            BuildPerskey, DefaultNrRetries ));
        If ( PersD = Nil ) Then Begin
          Exit;
        End;

        PersD^.IFDDefineKey ( 1, Key1Len, False );
        If MaxErrorClass <> 0 Then Begin
          Exit;
        End;
        PersD^.IFDDefineKey ( 2, Key2Len, True );
        If MaxErrorClass <> 0 Then Begin
          Exit;
        End;
        PersD^.IFDSetActBuffersOnDataChange ( True );
        PersD^.IFDSetRexUserProc ( DisplayRebuildInfo );
        PersD^.IFDSetCharConvert ( DemoCharConvert, Nil );        {!!.05}
        InitPF := True;
      End;

      Function InitForShell : Boolean;

      Begin
       InitForShell := False;
{$IFNDEF DPMI}
       IFDInitEMS ( SupNet,
                    IFDMinimizeUseOfNormalHeap,
                    MaxEmsPages,
                    HandlesReservedForShell );
       If MaxErrorClass <> 0 Then Begin
         Exit;
       End;
       If (IFDPageBufferSize And $FFFF0000 ) Shr 16 = 0 Then
         IFDAdjustPageBuffer ( 50000, 0 );

       If MaxErrorClass <> 0 Then Begin
         Halt;
       End;
{$ELSE}
       IFDInitEMS ( SupNet,
                    MemAvail - 50000,
                    0,
                    HandlesReservedForShell );
       If MaxErrorClass <> 0 Then Begin
         Exit;
       End;
{$ENDIF}
       IFDSetDefaultYesNoRequest ( UserAnswer );
       AskRebuildIndexOnly := True;                               {!!.05}
       If Not InitPf Then Exit;
       PersD^.IFDOpenFile;
       InitForShell := True;
    End;
{$ENDIF}

{$IFDEF BrUseIsam}
    Function InitForIsam : Boolean;

    Var
      NetType : NetSupportType;
      PS  : Word;
      Save,
        NetFileBlock : Boolean;

    Begin
      InitForIsam := False;
{$IFNDEF DPMI}
      PS :=BTInitIsam ( SupNet, MinimizeUseOfNormalHeap, MaxEmsPages );
{$ELSE}
      PS :=BTInitIsam ( SupNet, MemAvail - 200000, 0);
{$ENDIF}
      If Not IsamOK Then Exit;
      Save := YesNo ( 'Open the FileBlock in Save mode?', {!!.TP}
              mfConfirmation);
      NetFileBlock := BTNetSupported <> NoNet;
      If Not OpenTheFile ( Save, NetFileBlock ) Then Exit;
      BTSetCharConvert ( PersFB, DemoCharConvert, Nil, False );   {!!.05}
      InitForIsam := True;
    End;
    {$ENDIF}

  Begin
    InitDataBase := False;
    {$IFDEF BrUseShell}
      InitDataBase := InitForShell;
    {$ENDIF}
    {$IFDEF BrUseIsam}
      InitDataBase := InitForIsam;
    {$ENDIF}
  End;

  Constructor TTVISDemoApp.Init;
  Begin
    TApplication.Init;
    SourceDestCP := CPDos437;                                     {!!.05}
    SourceDestCP := SourceDestCP Shl 8 Or GetCodePageConst;       {!!.05}
    OtherCP := BuildCodePageMask ( CPWin1252 )                    {!!.05}
        Or BuildCodePageMask ( CPDos850 );                        {!!.05}

(*    InstallHeapErrorFunc;*)
    If Not InitDataBase Then Fail;
  End;

  Procedure TTVISDemoApp.InitMenuBar;

  Var
    R: TRect;

  Begin
    GetExtent ( R );
    R.B.Y := R.A.Y + 1;
    MenuBar := New ( PMenuBar, Init ( R, NewMenu (
      NewSubMenu ( '~ð~', hcNoContext, NewMenu (
        NewItem ( '~I~nfo', '', kbNoKey, cmAbout, hcNoContext,
        NewItem ( '~D~isplay', '', kbNoKey, cmDisplay, hcNoContext,     {!!.TP}
        Nil ))),
      NewSubMenu ( '~F~ile', hcNoContext, NewMenu (                     {!!.TP}
        NewItem ( '~B~rowser', '', kbNoKey, cmNewWin, hcNoContext,
        NewLine (
        NewItem ( 'E~x~it', 'Alt-X', kbNoKey, cmQuit, hcNoContext,
        Nil)))),
      NewSubMenu ( '~W~indow', hcNoContext, NewMenu (                   {!!.TP}
        NewItem ( '~R~esize','F4', kbF4, cmResize, hcNoContext,         {!!.TP}
        NewItem ( '~Z~oom', 'F5', kbF5, cmZoom, hcNoContext,
        NewItem ( '~N~ext', 'Tab', kbTab, cmNext, hcNoContext,          {!!.TP}
        NewItem ( '~C~lose', 'Alt-F3', kbAltF3, cmClose, hcNoContext,   {!!.TP}
        NewItem ( 'C~a~scade', 'Ctrl-F6', kbCtrlF6, cmCascade, hcNoContext, {!!.TP}
        NewItem ( '~T~ile', 'Ctrl-F7', kbCtrlF7, cmTile, hcNoContext,   {!!.TP}
        Nil))))))),
      Nil))
    ))));
  End;

  Procedure TTVISDemoApp.InitStatusLine;

  Var
    R: TRect;

  Begin
    GetExtent ( R );
    R.A.Y := R.B.Y - 1;
    StatusLine := New(PStatusLine, Init(R,
      NewStatusDef ( 0, $FFFF,
        NewStatusKey ( '', kbF10, cmMenu,
        NewStatusKey ( '~Alt-X~ Exit', kbAltX, cmQuit,
        NewStatusKey ( '~Alt-B~ Browser', kbAltB, cmNewWin,
        NewStatusKey ( '~Alt-F3~ Close', kbAltF3, cmClose, {!!.TP}
        Nil)))),
      Nil)
    ));
  End;


  Procedure TTVISDemoApp.NewWindow;

  Var
    Window: PPersBrowser;
    R: TRect;
    PS : PScrollBar;

  Begin
    R.Assign ( 0, 0, 62, 10 );
    R.Move ( Wincount mod 8, Wincount mod 8 );
    Inc ( WinCount );
    Window := Nil;
{$IFDEF BrUseShell}
     New ( Window, Init ( R, PersD^.IFDGetDriverName, WinCount,
                          PersD, 1, 50, 1, '', '',
                          TheHeader,
                          '' ));

{$ENDIF}
{$IFDEF BrUseIsam}
    New ( Window, Init ( R, 'Address', WinCount,
                         PersFB, 1, 50, 1, '', '',
                         TheHeader,
                         '', Person, False));
{$ENDIF}
    If Window <> Nil then Begin
      Window := PPersBrowser ( ValidView ( Window ));
    End;
    If Window <> Nil Then Begin
      Window^.Options := Window^.Options Or ofTileable;
      DeskTop^.Insert ( Window )
    End Else Begin
      OKBox ( 'Insufficient memory or file error.', mfError ) {!!.TP}
    End;
  End;


  Procedure TTVISDemoApp.Idle;

  Const
    L : LongInt = -1;
    ReadIntervall = 5000; { 5 secs }

  Begin
    If L = -1 Then L:= GetTimeMS;
    TApplication.Idle;
{$IFDEF BrUseShell}
    If IFDGetShellIsActive Then Exit;
{$ENDIF}
    If GetTimeMS > L + ReadIntervall Then Begin
      If DeskTop^.Current <> Nil  Then
        If DeskTop^.Current^.State And sfModal= 0  Then
          If Message ( @Self, evBroadcast, cmUpdate, @Self ) = Nil Then;
      L := GetTimeMs;
    End;
  End;


  Procedure ShowParams;

  Begin {!!.TP}
    Writeln ( 'Usage: TVISDEMO Net');
    Writeln;
    Writeln ( 'For the Net parameter specify exactly one of the following' );
    Writeln;
    Writeln ( '/O        No network' );
    Writeln ( '/N        Novell NetWare (not NetWare Lite!)' );
    Writeln ( '/M        MS-Net or compatible' );
    Writeln;
  End;

  Procedure GetParams;

  Var
    Err  : Integer;
    Para : String;
    WSNr : Integer;

  Begin
    If ParamCount < 1 Then Begin
      ShowParams;
      Halt;
    End;
    Para := ParamStr (1);
    If Para [1] <> '/' Then Begin
      ShowParams;
      Halt;
    End Else Begin
      Delete ( Para, 1, 1 );
    End;
    Case UpCase (Para [1]) Of
      'O' : SupNet := NoNet;
      'N' : SupNet := Novell;
      'M' : SupNet := MsNet;
      Else Begin
        ShowParams;
        Halt;
      End;
    End; {Case}
  End;

Var
  TVISDemoApp: TTVISDemoApp;

Begin
  GetParams;
  TheHeader := ' ' +
               Extend ( 'LastName', 25 ) + ' ' +  {!!.TP}
               Extend ( 'Telephone', 15) + ' ' +  {!!.TP}
               Extend ( 'Zip', 9) + ' '+          {!!.TP}
               Extend ( 'City', 20 ) + ' ' +      {!!.TP}
               Extend ( 'Street', 20 );           {!!.TP}
  If TVISDemoApp.Init Then Begin
    TVISDemoApp.Run;
    TVISDemoApp.Done;
  End;
End.
