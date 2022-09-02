{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

Unit OPISMain;

{$I OPDefine.INC}
{$I BRDefOpt.Inc}

{$I-,V-,F+,O+}  {<- F+ required }


{$IFNDEF UseDrag}
  *****Error : This unit requires OPDrag
{$ENDIF }

{$IFNDEF BRUseIsam}
  *****Error : This unit compiles with B-Tree Isam only
{$ENDIF }

Interface

Uses
  OPDos,
{$IFDEF OPRO12}       {!! 1.20}
  OPConst,
{$ENDIF}
{ OPSCodes,}   {removed !!.TP}
  OpInline,
  OpString,
  OpRoot,
  OpCrt,
  OpColor,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpAbsFld,
  OpCmd,
  OpDate,
  OpPrnLow,
  OpPrint,
  OpField,
  OpFrame,
  OpWindow,
  OPDrag,
  FuncKeys,
  MkDrag,
  OpPick,
  OpSelect,
  OpEntry,
  OpCtrl,
  OpDialog,
  BTBase,
  BTIsBase,
  Filer,     {!!.TP}
  ReBuild,
  IsamTool,
  LowBrows,
  CCSKeys,                                                        {!!.05}
  OPBrow;

Procedure Main;

Implementation

Const
  FName = 'Adressen';
  Key1Len = 30;
  Key2Len = 8;

  Header : String[80] = 'ÄÄ Address Demo ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ'; {!!.TP}

  HandlesReservedForShell = 13;

  ScrollBarChar  = '²';
  SliderChar     = #254 {'°'};

  BackdropChar = {'°'} '±';
  BackdropAttr = $07;

{$IFDEF UseMouse}
Const
  MouseChar  : Char = #04;
{$ENDIF}

  LowIntChar = #128;                                                  {!!.03a}
  HiIntChar = #165;                                                   {!!.03a}

  ToKeyTable : Array [LowIntChar..HiIntChar] Of Char = (              {!!.03a}
      'C', 'U', 'E', 'A', 'A', 'A', 'A', 'C', 'E', 'E', 'E', 'I', 'I',
      'I', 'A', 'A', 'E', 'A', 'A', 'O', 'O', 'O', 'U', 'U', 'Y', 'O',
      'U', '›', 'œ', '', 'ž', 'Ÿ', 'A', 'I', 'O', 'U', 'N', 'N' );


Type
  DialogHeaderString = String[18];
  CharSet = Set Of Char;
  PPersonDef = ^Persondef;
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

  Adressbrowser = Object ( ISBrowser )
    Function BuildRow ( Var RR : RowRec ) : Integer; Virtual;
    Procedure CharHandler; Virtual;
    Procedure NewRec;
    Procedure EditRec;
    Procedure DelRec;
    Procedure SearchRec;
    Procedure ToggleKey;
    Procedure SearchByKey ( C : Char );
    Procedure Rebuild;
    Procedure Status;
    Procedure DisplayInfo;
  End;

Var
  Person   : Persondef;
  SupNet   : NetSupportType;
  Pf       : IsamFileBlockPtr;
  AdrBrow  : AdressBrowser;
  SaveWin  : RawWindow;          {used to save underlying screen}
  BrFkLine,
  ESFkLine,
  DefFKLine: FuncKeyLine;
  UseEms   : Boolean;
  SourceDestCP  : Word;                                           {!!.05}
  OtherCP       : LongInt;                                        {!!.05}

Const
  NDColors : ColorSet = (
    TextColor       : $70; TextMono        : $70;
    CtrlColor       : $3A; CtrlMono        : $08;
    FrameColor      : $7F; FrameMono       : $70;
    HeaderColor     : $7F; HeaderMono      : $70;
    ShadowColor     : $08; ShadowMono      : $00;
    HighlightColor  : $4F; HighlightMono   : $7F;
    PromptColor     : $70; PromptMono      : $70;
    SelPromptColor  : $7F; SelPromptMono   : $70;
    ProPromptColor  : $70; ProPromptMono   : $70;
    FieldColor      : $1E; FieldMono       : $07;
    SelFieldColor   : $1F; SelFieldMono    : $0F;
    ProFieldColor   : $70; ProFieldMono    : $70;
    ScrollBarColor  : $31; ScrollBarMono   : $07;
    SliderColor     : $31; SliderMono      : $0F;
    HotSpotColor    : $7F; HotSpotMono     : $0F;
    BlockColor      : $1E; BlockMono       : $0F;
    MarkerColor     : $1F; MarkerMono      : $70;
    DelimColor      : $7E; DelimMono       : $0F;
    SelDelimColor   : $11; SelDelimMono    : $0F;
    ProDelimColor   : $7E; ProDelimMono    : $0F;
    SelItemColor    : $2F; SelItemMono     : $70;
    ProItemColor    : $77; ProItemMono     : $07;
    HighItemColor   : $7F; HighItemMono    : $0F;
    AltItemColor    : $3E; AltItemMono     : $0F;
    AltSelItemColor : $2F; AltSelItemMono  : $70;
    FlexAHelpColor  : $7F; FlexAHelpMono   : $0F;
    FlexBHelpColor  : $7A; FlexBHelpMono   : $0F;
    FlexCHelpColor  : $7B; FlexCHelpMono   : $70;
    UnselXrefColor  : $7E; UnselXrefMono   : $09;
    SelXrefColor    : $9F; SelXrefMono     : $70;
    MouseColor      : $70; MouseMono       : $70);
  dColors : DialogColorSet = (
    HiPromptColor   : $7E; HiPromptMono    : $0F;
    ButtonColor     : $20; ButtonMono      : $07;
    DefButtonColor  : $2B; DefButtonMono   : $07;
    HiButtonColor   : $2E; HiButtonMono    : $0F;
    SelButtonColor  : $2F; SelButtonMono   : $0F;
    ProButtonColor  : $70; ProButtonMono   : $70;
    BtnShadowColor  : $70; BtnShadowMono   : $70;
    ClusterColor    : $30; ClusterMono     : $07;
    ProClusterColor : $70; ProClusterMono  : $07;
    HiClusterColor  : $3E; HiClusterMono   : $0F;
    SelClusterColor : $3F; SelClusterMono  : $0F);
Const
  NDFuncKeyColorSet : FuncKeyColorSet = (
    TxtColor      : $70; TxtMono      : $70;
    HiColor       : $74; HiMono       : $7F;
    SelColor      : $20; SelMono      : $7F;
    SelHiColor    : $24; SelHiMono    : $7F;
    ProColor      : $78; ProMono      : $70;
    ProHiColor    : $78; ProHiMono    : $70;
    SelProColor   : $28; SelProMono   : $70;
    SelProHiColor : $28; SelProHiMono : $70 );

Const
  OPSHWindowOptions  : LongInt =
    wClear+wSaveContents+wStoreContents+wAllMouseEvents+wBordered+wAltFrame;

Var
  StatusColor   : Byte;
  StatusMono    : Byte;
  NDemoSaveExitProc : Pointer;

Var
  DateMask    : string[10];  {picture mask for date strings}
  TimeMask    : string[11];  {picture mask for time strings}

Const
  {status line}
  StatusRow   : Byte = 25;
  MaxEmsPages : Word = 500;


Var
  BRLineDescript : String;


Const
  icAddARec = ccUser0;
  icDelARec = ccUser1;
  icSearchARec = ccUser2;
  icEditARec = ccUser3;
  icChangeKey = ccUser5;
  icStatus = ccUser6;
  icReind  = ccUser7;
  icInfo = ccUser8;

{Entry field constants}
Const
  idName     = 0;
  idVorname  = idName + 1;
  idLand      = idVorname + 1;
  idPLZ      = idLand + 1;
  idWohnort  = idPLZ + 1;
  idStrasse  = idWohnort + 1;
  idTelefon  = idStrasse + 1;



  Procedure ShowDateTime;

  Var
    A : Byte;
    B,C, D : LongInt;

  Begin
    With NDColors Do
      A := ColorMono(HighlightColor, HighlightMono);
    FastWrite ({' ' +}TodayString ( DateMask ){+ ' '}, 1 , 60, A );       {!!.tp}
    FastWrite ({' ' +}CurrentTimeString ( TimeMask ){ + ' '}, 1, 70, A ); {!!.tp}
  End;

  Function OKeyPressed : Boolean;

  Begin
    ShowDateTime;
    OKeyPressed := OPCrt.KeyPressed;
  End;


  Function IntStrToKey ( S : String ) : String;                   {!!.03a}

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
  End;


  Function Extend ( S : String; Anz : Byte ) : String;

  Begin
    Extend := Copy ( Pad ( S, Anz ), 1, Anz );
  End;

  Procedure DrawStatusLine ( Status : String; Centered : Boolean );
  Var
    MS : Boolean;

  Begin
   {$IFDEF UseMouse}
   HideMousePrim ( MS );
   {$ENDIF}
   If Centered Then
      Status := Center ( Status, ScreenWidth );
    With NDColors Do
      FastWrite ( Pad ( Status, Screenwidth ), StatusRow, 1,
          ColorMono ( StatusColor, StatusMono ) );
   {$IFDEF UseMouse}
   ShowMousePrim ( MS );
   {$ENDIF}
  End;

  Procedure WriteHeader;

  Begin
    FastWrite ( Header, 1, 1,
        ColorMono ( NDColors.HighlightColor,
                    NDColors.HighlightMono ) );
  End;



  Procedure ClearWholeScreen;

  Begin
    {$IFDEF UseMouse}
    HideMouse;
    {$ENDIF}
    Savewin.draw;
    {clear the screen}
    TextChar := BackdropChar;
    TextAttr := BackdropAttr;
    ClrScr;

    {$IFDEF UseMouse}
    ShowMouse;
    {$ENDIF}
  End;


  Procedure OPSHErrorProc (     UnitCode  : Byte;
                            Var ErrorCode : Word;
                                ErrorMsg  : String );

  Var
    S : String;
    P : Pointer;
    SSaved : Boolean;

  Begin
    RingBell;
    SSaved := SaveWindow ( 1, StatusRow, ScreenWidth, StatusRow, True, P );

    If Trim ( ErrorMsg ) > '' Then
      S := ErrorMsg
    Else
      S := 'Error ' + Form ( '#####', ErrorCode ); {!!.TP}
    S := S + '; Press a key.'; {!!.TP}
    DrawStatusLine ( S, True ) ;

    Repeat Until OKeyPressed;

    If SSaved Then
      RestoreWindow ( 1, StatusRow, ScreenWidth, StatusRow, True, P );
  End;

  Procedure CustomizeWindow ( Var CW : CommandWindow;
                                  Title : string;
                                  TWidth : Byte );
    {-Customize a window (add title, scroll bars, etc.)}
  begin
    With CW Do Begin
      {set limits for window coordinates}
      SetPosLimits(1, 2, ScreenWidth, ScreenHeight - 1);

      if wOptionsAreOn(wResizeable) then
        {set resizing limits, accounting for hot spots}
        SetSizeLimits(TWidth+8, 4, ScreenWidth, ScreenHeight - 1);
    End;
    with CW, wFrame, NDColors do begin
      {specify shadow type}
      SetFrameType ( DblWindowFrame );
      AddShadow(shBR, shSeeThru);

      if wOptionsAreOn ( wResizeable ) then begin
        {add scroll bars}
        AddCustomScrollBar (
          frBB, 0, MaxLongInt, 1, 1, SliderChar, ScrollBarChar, NDColors );
        AddCustomScrollBar (
          frRR, 0, MaxLongInt, 1, 1, SliderChar, ScrollBarChar, NDColors );
      end;

      {add title header}
      if Title <> '' then Begin
        Title := ' '+Title+' ';
        AddHeader ( Title, heTC );
      end;

      If wOptionsAreOn ( wResizeable) Then Begin
        {add zoom button}
        AddCustomHeader ( #24, frtr, -1, 0,
                          NDColors.HeaderColor,
                          NDColors.HeaderMono);
        AddHotRegion ( frtr, ZoomHotCode, -1, 0, 1, 1 );
        {add hot spot for resizing}
        AddCustomHeader ( #$D9, frBR, 0, 0,
                          NDColors.PromptColor,
                          NDColors.PromptMono );
        AddHotRegion ( frBr, ResizeHotCode, 0, 0, 1, 1);
      end;

      {add hot bar for moving}
      wFrame.AddHotBar ( frTT, MoveHotCode );

      If LongFlagIsSet ( wFlags, wAltFrame) Then
      With NDColors do Begin
        With aFrame do begin
          SetFrameType ( SglWindowFrame );
          SetFrameAttr ( FrameColor, FrameMono );
          SetHeaderAttr ( FrameColor, FrameMono, False );
          AddShadow ( shBR, shSeeThru );
          if Title <> '' then begin
            Title := ' '+Title+' ';
            AddHeader(Title, heTC);
          End;
          if wOptionsAreOn ( wResizeable ) then begin
            {add zoom button}
            AddCustomHeader ( #24, frtr, -1, 0,
                            NDColors.Headercolor,
                            NDColors.HeaderMono);
            AddHotRegion ( frTR, ZoomHotcode, -1, 0, 1, 1 );
            AddCustomHeader (  #$D9, frBR, 0, 0,
                               NDColors.PromptColor,
                               NDColors.PromptMono );
            AddHotRegion ( frBr, ResizeHotCode, 0, 0, 1, 1 );
          end;
          {add hot bar for moving}
          AddHotBar ( frTT, MoveHotCode );
        end;
      end;
    end;
  end;

  Function YesNo ( Prompt : String;
                   DefaultYes : Boolean ) : Boolean;

  Var
    DB        : DialogBox;
    Finished  : Boolean;
    X, Width : Byte;
    HotCode   : Byte;
    CMD       : Word;

  Begin
    YesNo := DefaultYes;
    Prompt := Copy ( Prompt, 1, 75 );
    Width :=  Length ( Prompt ) + 2;
    If Width < 26 Then Width := 26;
    X :=  ScreenWidth - Width - 1;
    If Not DB.InitCustom(X, ScreenHeight - 4,
                         X + Width, ScreenHeight - 2,    {window coordinates}
                         NDColors,                       {ColorSet}
                         OPShWindowOptions,              {window options}
                         dColors) Then Begin             {DialogColorSet}
      WriteLn('Init error: ', InitStatus);
      Halt(1);
    End;

    With DB Do Begin

      CustomizeWindow ( DB, ' Dialog Box ', Length ( ' Dialog Box ' ));

      DB.AddCenteredTextField ( Prompt, 1 );

      {add pushbuttons}
      DB.AddPushButton('&Yes',  2, Width Div 2 -10, 8, 0, ccUser0, DefaultYes); {!!.TP}
      DB.AddPushButton('&No',   2, Width Div 2 + 2, 8, 0, ccUser1, Not DefaultYes); {!!.TP}
      Finished := False;
      Repeat
        DefFKLine.DisplayFKLine;
        DB.Process;
        HotCode := HandleMouseOrKBDPress ( DB, @DefFKLine );
        DefFKLine.EraseFKLine;
        Case DB.GetLastCommand of
          ccUser0  : Begin
                       Finished := True;
                       YesNo := True;
                     End;
          ccUser1  : Begin
                       Finished := True;
                       YesNo := False;
                     End;
          ccQuit,
          ccSelect,
          ccError  : Finished := True;
          ccHelp   : DB.RequestHelp ( 0 );
        End;
      Until Finished;
      DB.Erase;
      DB.Done;
    End;
  End;

  Procedure OKBox ( DialogHeader : DialogHeaderString;
                    Prompt1, Prompt2 : String );

  Var
    DB       : DialogBox;
    X, Width,
    Height   : Byte;
    HotCode  : Byte;
    CMD      : Word;

  Begin
    DialogHeader := ' ' + DialogHeader + ' ';
    Prompt1 := Copy ( Prompt1, 1, 75 );
    Prompt2 := Copy ( Prompt2, 1, 75 );
    Width :=  Length ( Prompt1 );
    If Length ( Prompt2 ) > Width Then
      Width :=  Length ( Prompt2 );
    Inc ( Width , 2 );
    If Width < 26 Then Width := 26;
    Height := 3;
    X :=  ScreenWidth - Width - 1;
    If Not DB.InitCustom ( X, ScreenHeight - 2 - Height,
                           X + Width, ScreenHeight - 2,    {window coordinates}
                           NDColors,                       {ColorSet}
                           OPSHWindowOptions,              {window options}
                           dColors ) Then Begin            {DialogColorSet}
      WriteLn ( 'Init error: ', InitStatus );
      Halt(1);
    End;

    With DB Do Begin
      CustomizeWindow ( DB, Dialogheader, Length ( DialogHeader ));
      DB.AddCenteredTextField ( Prompt1, 1 );
      DB.AddCenteredTextField ( Prompt2, 2 );

      DB.AddPushButton('&Ok',   3, Width Div 2 - 4, 8, 0, ccSelect, True);
      Repeat
        DefFKLine.DisplayFKLine;
        DB.Process;
        HotCode := HandleMouseOrKBDPress ( DB, @DefFKLine );
        DefFKLine.EraseFKLine;
        Case DB.GetLastCommand Of
          ccHelp : DB.RequestHelp ( 0 );
        End;
      Until DB.GetLastCommand in [ ccError, ccSelect, ccQuit];
      DB.Erase;
      DB.Done;
    End;
  End;


  Procedure ErrorHalt;

  Var
    S           : String;

  Begin
    If IsamError = 0 Then
      S := 'No specific error.' {!!.TP}
    Else Begin
      Str ( IsamError: 5, S );
      S := 'Error code : ' + S; {!!.TP}
    End;
    OKBox ( 'Error', S, '' );   {!!.TP}
    Halt;
  End;

  Function BuildKey ( Var P; KeyNr : Word ) : IsamKeyStr;

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
      BuildKey := NLStrToKey ( TempS, SourceDestCP, OtherCP );    {!!.05}
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
      SDCP := Swap ( SourceDestCP );
    End Else Begin
      SDCP := SourceDestCP;
    End;
    OCP := OtherCP;
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

  Function InitForIsam : Boolean;

  Var
    NetType : NetSupportType;
    PS  : Word;

  Begin
{$IFNDEF DPMI}
    If UseEms Then
      PS :=BTInitIsam ( SupNet, MinimizeUseOfNormalHeap, MaxEmsPages )
    Else
      PS :=BTInitIsam ( SupNet, 30000, 0 );
{$ELSE}
    PS :=BTInitIsam ( SupNet, MemAvail - 200000, 0 );
{$ENDIF}
    InitForIsam := IsamOK;
  End;


  Procedure InitBrowser;

  Const
    WinOptions = wBordered+wClear+wUserContents+wResizeable+wAllMouseEvents;
    BRHeader : String = ' ADDRESS ';

  Begin
    BRLineDescript  :=
         Extend ( 'LastName', 25 ) + ' ' +   {!!.TP}
         Extend ( 'FirstName', 21 ) + ' ' +  {!!.TP}
         Extend ( 'Telephone', 16 ) + ' ' +  {!!.TP}
         Extend ( 'Country/Zip/City', 40 );  {!!.TP}

    AdrBrow.InitCustom ( 2,4,79,20,
                         NDColors,
                         Pf,
                         1,
                         50,
                         1,
                         '', '',
                         BRLineDescript,
                         '',
                         Person,
                         False,
                         OPShWindowOptions or wResizeable );
    If InitStatus = 0 Then Begin
      With AdrBrow Do Begin
        CustomizeWindow ( AdrBrow, BrHeader, Length ( BrHeader ));
      End;
    End Else Begin
      OKBox ( 'Error', 'Initializing Browser window.' , '' ); {!!.TP}
      Halt;
    End;
    BrFkLine.InitCustom ( 12, NDFuncKeyColorSet );
    If InitStatus > 0 Then Halt;
    With AdrBrow Do Begin
      cwCMDPtr^.AddCommand ( icAddARec, 1, $3C00, $0 );
      cwCMDPtr^.AddCommand ( icDelARec, 1, $3D00, $0 );
      cwCMDPtr^.AddCommand ( icSearchARec, 1, $3E00, $0 );
      cwCMDPtr^.AddCommand ( icEditARec, 1, $3F00, $0 );
      cwCMDPtr^.AddCommand ( icChangeKey, 1, $4100, $0 );
      cwCMDPtr^.AddCommand ( icStatus, 1, $4200, $0 );
      cwCMDPtr^.AddCommand ( icReind, 1, $4300, $0 );
      cwCMDPtr^.AddCommand ( icInfo, 1, $7100, $0 );
      cwCMDPtr^.AddCommand ( icMoreFK, 1, FKMoreKey, $0 );
      SetUpdateInterval ( 2000 );
    End;
    With BrFKLine Do Begin
      AddCommand ( icAddARec, ^A'F2'^A' New' );           {!!.TP}
      AddCommand ( icDelARec, ^A'F3'^A' Delete' );        {!!.TP}
      AddCommand ( icSearchARec, ^A'F4'^A' Search' );     {!!.TP}
      AddCommand ( icEditARec, ^A'F5'^A' Edit' );
      AddCommand ( icChangeKey, ^A'F7'^A' Key' );
      AddCommand ( icStatus, ^A'F8'^A' Status' );
      AddCommand ( ccQuit, ^A'ESC'^A' Quit' );            {!!.TP}
      AddCommand ( icReind, ^A'F9'^A' Reindex' );
      AddCommand ( icInfo, ^A'Alt-F10'^A' Info' );
    End;
  End;

  Procedure DisplayHelpPrompt ( ESP : EntryScreenPtr );

  Var
    S : String[80];

  Begin
    S := '';
    With ESP^ Do
      Case GetCurrentID Of
        idName    : S := 'Enter last name (key field).';    {!!.TP}
        idVorname : S := 'Enter first name (key field).';   {!!.TP}
        idLand    : S := 'Enter country code (key field).'; {!!.TP}
        idPLZ     : S := 'Enter zip code (key field).';     {!!.TP}
        idWohnort : S := 'Enter city.';                     {!!.TP}
        idStrasse : S := 'Enter address.';                  {!!.TP}
        idTelefon : S := 'Enter telephone number.';         {!!.TP}
      End; {Case}
      EsFKLine.SetHelpLine ( S );
      EsFKLine.DisplayFKLine;
  End;


  Function GetPerson ( DisplayOnly : Boolean;
                       Header      : String;
                       Search      : Boolean ) : Boolean;

  Const
    X1 : Byte = 12;
    Y1 : Byte = 8;
    X2 : Byte = 60;
    Y2 : Byte = 18;

  Var
    ES         : EntryScreen;
    Status     : Word;
    I          : Integer;
    HotCode    : Byte;
    CMD        : Word;
    EditBuffer : PersonDef;
    Stop       : Boolean;                                         {!!.05}
    FaultID    : Integer;                                         {!!.05}
    FaultPos   : Word;                                            {!!.05}

  Function InitEntryScreen : Word;

  Var
    Status : Word;

    Begin
      With ES Do Begin
        If Not InitCustom ( X1, Y1, X2, Y2,
            NDColors, OPShWindowOptions ) Then Begin
          InitEntryScreen := InitStatus;
          Exit;
        End;
        CustomizeWindow ( ES, Header, Length ( Header ));
        SetWrapMode ( WrapAtEdges );
        SetPreEditProc ( DisplayHelpPrompt );
        With EditBuffer Do Begin

        {idName:}
          AddStringField (
            'LastName', 1, 2,                             {!!.TP}
            CharStr ( 'X', 25 ), 1, 12, 25,
            0, Name );

        {idVorname:}
          AddStringField (
            'FirstName', 3, 2,                            {!!.TP}
            CharStr ( 'X', 20 ), 3, 12, 20,
            0, Vorname );

        {idLand:}
          AddStringField (
            'Ctry/Zip', 5, 2,                             {!!.TP}
            '!!!', 5, 12, 3,
            0, Land );

        {idPLZ:}
{          esFieldOptionsOn ( efRightJustify );}                  {!!.03a}
          AddStringField (
            '', 5, 2,
            '#####', 5, 16, 5,
            0, PLZ );
{          esFieldOptionsOff ( efRightJustify );}                 {!!.03a}

        {idWohnort:}
          AddStringField (
            'City', 7, 2,                                 {!!.TP}
            CharStr ( 'X', 30 ), 7, 12, 30,
            0, Wohnort );

        {idStrasse:}
          AddStringField (
            'Street', 9, 2,                               {!!.TP}
            CharStr ( 'X', 30 ), 9, 12, 30,
            0, Strasse );

        {idTelefon:}
          AddStringField (
            'Telephone', 11, 2,                           {!!.TP}
            CharStr ( '#', 15 ), 11, 12, 15,
            0, Telefon );
        End;
        Status := RawError;
      End;
      If Status = 0 Then Begin
        ESFkLine.InitCustom ( 3, NDFuncKeyColorSet );
        Status := InitStatus;
        If Status = 0 Then With ESFKLine Do Begin
          AddCommand ( ccHelp, ^A'F1'^A + ' Help ' );     {!!.TP}
          AddCommand ( ccDone, ^A'^Enter'^A + Header ) ;
          AddCommand ( ccQuit, ^A'ESC'^A' Cancel' );      {!!.TP}
        End;
      End;
      InitEntryScreen := Status;
    End;

    Procedure ShowAndPosOnError ( FaultID : Integer;
        FaultPos : Word );                                        {.!!.05}

    Var
      EFP   : EntryFieldPtr;
      Found : Boolean;

    Begin
      ES.SetNextField ( FaultID );
      OKBox ( 'Error', 'You have entered an invalid value.',  {!!.TP}
              'Please correct your input for this field.');   {!!.TP}
    End;

    Function CheckErrorChars ( Var FaultID : Integer; FaultPos : Word ) :
        Boolean;                                                  {!!.05}

    Var
      Res     : LongInt;

    Begin
      With Person Do Begin
        FaultID := 0;
        Res := CheckChars ( @Name [1], Length ( Name ),
            SourceDestCP, OtherCP );
        If Res = 0 Then Begin
          FaultID := 1;
          Res := CheckChars ( @Vorname [1], Length ( Vorname ),
              SourceDestCP, OtherCP );
        End;
        If Res = 0 Then Begin
          FaultID := 2;
          Res := CheckChars ( @Land [1], Length ( Land ),
              SourceDestCP, OtherCP );
        End;
        If Res = 0 Then Begin
          FaultID := 3;
          Res := CheckChars ( @Plz [1], Length ( Plz ),
              SourceDestCP, OtherCP );
        End;
        If Res = 0 Then Begin
          FaultID := 4;
          Res := CheckChars ( @Wohnort [1], Length ( Wohnort ),
              SourceDestCP, OtherCP );
        End;
        If Res = 0 Then Begin
          FaultID := 5;
          Res := CheckChars ( @Strasse [1], Length ( Strasse ),
              SourceDestCP, OtherCP );
        End;
        If Res = 0 Then Begin
          FaultID := 6;
          Res := CheckChars ( @Telefon [1], Length ( Telefon ),
             SourceDestCP, OtherCP );
        End;
      End;
      FaultPos := Res And $FFFF;
      CheckErrorChars := Res <> 0;
    End;

  Begin
    GetPerson := False;

    {initialize entry screen}
    Status := InitEntryScreen;
    If Status <> 0 Then Begin
      OKBox ( 'Error',                                            {!!.TP}
              'Error initializing entry screen. Aborting!', '' ); {!!.TP}
      Halt;
    End;

    If DisplayOnly Then Begin
      ES.esOptionsOn ( esReadOnly )
    End Else Begin
      If Search Then Begin
        ES.ChangeProtection ( idWohnort, True );
        ES.ChangeProtection ( idTelefon, True );
        ES.ChangeProtection ( idStrasse, True );
      End;
    End;

    EditBuffer := Person;
    Repeat
      ESFKLine.DisplayFKLine;
      ES.Process;
      HotCode := HandleMouseOrKBDPress ( ES, @ESFKLine );
      ESFKLine.EraseFkLine;
      Stop := ES.GetLastCommand in                                {!!.05}
          [ccError, ccQuit, ccClickExit, ccSelect, ccDone];
      Case ES.GetLastCommand Of
        ccHelp : ES.RequestHelp ( ES.GetHelpIndex ( ES.GetCurrentID ));
        ccSelect, ccDone : Begin                                  {!!.05}
          Person := EditBuffer;                                   {!!.05}
          If CheckErrorChars ( FaultID, FaultPos ) Then Begin     {!!.05}
            ShowAndPosOnError ( FaultID, FaultPos );              {!!.05}
            Stop := False;                                        {!!.05}
          End;                                                    {!!.05}
        End;                                                      {!!.05}
      End;
    Until Stop;                                                   {!!.05mod}
    Person := EditBuffer;

    GetPerson := ES.GetLastCommand in [ ccDone, ccClickExit ];
    ES.Coordinates ( X1, Y1, X2, Y2 );
    ES.Erase;
    ES.Done;
    ESFKLine.Done;
  End;

{$I OpIsStuf.inc}




  Procedure AdressBrowser.Rebuild;

  Begin
    If YesNo ( 'Rebuild fileblock', True )  Then Begin  {!!.TP}
      If BTFileBlockIsOpen ( PF ) Then BTCloseFileBlock ( PF );
      If Not RebuildTheFile Then
        OKBox ( 'Error',
            'Fileblock could not be rebuilt.', '' ); {!!.TP}
      If Not OpenTheFile Then Begin
        OKBox ( 'Error',
            'Fileblock could not be reopened.', '' );  {!!.TP}
      End;
    End;
  End;

  Procedure AdressBrowser.DisplayInfo;

  Begin
   OKBox ( ' Info ',
           'OPISDEMO',
           'Copyright (C) 1991-94 ENZ EDV-Beratung GmbH' );    {mod !!.04a}
  End;


  Function AdressBrowser.BuildRow (  Var RR         : RowRec ) : Integer;

  Begin
    BuildRow := NoError;
    With Person, RR Do Begin
      If Status = 0 Then Begin
        Row := Extend ( Name, 25 ) + ' ' +
               Extend ( Vorname, 21 ) + ' ' +
               Extend ( Telefon, 16 ) + ' ' +
               Extend ( Trim (Land) + '-' + Trim (PLZ), 9) +   {mod !!.03a}
               Extend ( Wohnort, 30 );
      End Else Begin
        Row := '';
        While Length ( Row ) < MaxCols Do Begin
          Row := Row + '**   ';
        End;
      End;
    End;
  End;


  Procedure AdressBrowser.CharHandler;

  Var                                                             {!!.05}
    S : String;                                                   {!!.05}

  Begin
    If cwCmd = ccChar Then Begin                                  {!!.05mod}
      S := Chr ( cwKey );                                         {!!.05mod}
      S := NLStrToKey ( S, SourceDestCP, OtherCP );               {!!.05mod}
      Case S [1] Of                                               {!!.05mod}
       '0'..'9', 'A'..'Z' :                                       {!!.05mod}
            SetAndUpdateBrowserScreen ( S, 0 );                   {!!.05mod}
      End;
    End;
  End;

  Procedure ShowParams;

  Begin {!!.TP}
    Writeln ( 'Usage: OPISDEMO Net [/NOEMS]');
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
    UseEms := True;
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
    If ParamCount > 1 Then Begin
      Para := ParamStr ( 2 );
      If StUpcase ( Para ) <> '/NOEMS' Then Begin
        ShowParams;
        Halt;
      End Else Begin
        UseEms := False;
      End;
    End;
  End;


  Procedure NDemoExitProc;

  Var
    S, S1       : String;

  Begin
    ExitProc := NDemoSaveExitProc;
    If BTIsamErrorClass > 2 Then Begin
      S := 'Severe Error : '; {!!.TP}
      S1 := IsamErrorMessage ( IsamError );
      OKBox ( '*** Error ***', S, S1 );  {!!.TP}
      SaveWin.Erase;
      SaveWin.Done;
      NormalCursor;
    End;
  End;

  Procedure Main;

  Var
    HotCode : Byte;
    Cmd     : Word;

  Begin
    DateMask := InternationalDate ( False, False );
    TimeMask := InternationalTime ( True, False, True, True );

    AutoDelayTicks := 10;

    DefaultColorSet := NDColors;
    StatusColor := NDColors.TextColor;
    StatusMono := NDColors.TextMono;
    DefArrows := TriangleArrows;

    GetParams;

    wStack.Init ( 100 );
    wStack.OptimizeTiled ( False );

  {$IFDEF UseMouse}
    If MouseInstalled Then
      With NDColors Do Begin
        {activate mouse cursor}
        SoftMouseCursor ( $0000, ( ColorMono ( MouseColor, MouseMono ) shl 8 )+
                          Byte(MouseChar));
        ShowMouse;

        EntryCommands.cpOptionsOn ( cpEnableMouse );
        OPBrCommands.cpOptionsOn ( cpEnableMouse );
        MouseWindow ( 1, 2, ScreenWidth, ScreenHeight );
      End;
 {$ENDIF}

    If Not SaveWin.InitCustom (
        1, 1, ScreenWidth, ScreenHeight, NDColors,
        DefWindowOptions and not ( wBordered or wClear )) Then Halt;

    SaveWin.Draw;

    ClearWholeScreen;
    WriteHeader;

    EntryCommands.SetKeyPressedProc ( OKeyPressed );
    OPBRCommands.SetKeyPressedProc ( OKeyPressed );
    DialogCommands.SetKeyPressedProc ( OKeyPressed );

  {$IFDEF UseDrag}
    If AddMoveZoomCommands ( OPBRCommands ) = 0 Then;
    If AddMoveZoomCommands ( DialogCommands ) = 0 Then;
    If AddMoveZoomCommands ( EntryCommands ) = 0 Then;
  {$ENDIF}

    DefFKLine.InitCustom ( 1, NDFuncKeyColorSet );
    If InitStatus > 0 Then Halt;
    With DefFKLine Do Begin
      SetHelpLine ( 'Choose from these.' ); {!!.TP}
    End;

    If Not InitForIsam Then ErrorHalt;

    If Not OpenTheFile Then ErrorHalt;

    BTSetCharConvert ( PF, DemoCharConvert, Nil, False );         {!!.05}

    InitBrowser;

    AdrBrow.SetKeyNr ( 1 );
    AdrBrow.SetAndUpdateBrowserScreen ( '', 0 );
    Repeat
      Case AdrBrow.GetCurrentKeyNr Of
        0: AdrBrow.SetHeaderFooter ( BRLineDescript, 'Unsorted' );    {!!.04} {!!.TP}
        1: AdrBrow.SetHeaderFooter ( BRLineDescript, 'Sorted by Name' );      {!!.TP}
        2: AdrBrow.SetHeaderFooter ( BRLineDescript, 'Sorted by Country+Zip' ); {!!.TP}
       Else
         AdrBrow.SetHeaderFooter ( BRLineDescript, 'Sorted by Name' ); {!!.TP}
      End;

      BrFkLine.DisplayFkLine;
      AdrBrow.Process;

      Cmd := AdrBrow.GetLastCommand;

      HotCode := HandleMouseOrKBDPress ( AdrBrow, @BrFkLine);
      BrFkLine.EraseFkLine;

      Case AdrBrow.GetLastCommand of
        ccSelect,
        icEditARec   : AdrBrow.EditRec;
        icAddARec    : AdrBrow.NewRec;
        icDelARec    : AdrBrow.DelRec;
        icSearchARec : AdrBrow.SearchRec;
        icChangeKey  : AdrBrow.ToggleKey;
        icStatus     : AdrBrow.Status;
        icReind      : AdrBrow.ReBuild;
        icInfo       : AdrBrow.DisplayInfo;
        ccQuit,
        ccError      : If Not YesNo ( 'Quit Program', True ) Then {!!.TP}
                         AdrBrow.SetLastCommand ( ccNone )
                       Else
                         AdrBrow.SetLastCommand ( ccQuit );
      End; {Case}
    Until AdrBrow.GetLastCommand In [ccQuit, ccError];

    BrFKLine.Done;
    DefFKLine.Done;
    AdrBrow.Done;

    BTExitIsam;
    {$IFDEF UseMouse}
    HideMouse;
    {$ENDIF}

    SaveWin.Erase;
    SaveWin.Done;
    wStack.Done;
  End;

Begin
  SourceDestCP := CPDos437;                                       {!!.05}
  SourceDestCP := SourceDestCP Shl 8 Or GetCodePageConst;         {!!.05}
  OtherCP := BuildCodePageMask ( CPWin1252 )                      {!!.05}
      Or BuildCodePageMask ( CPDos850 );                          {!!.05}
  NDemoSaveExitProc := ExitProc;                                  {mod !!.03a}
  ExitProc := @NDemoExitProc;
End.
