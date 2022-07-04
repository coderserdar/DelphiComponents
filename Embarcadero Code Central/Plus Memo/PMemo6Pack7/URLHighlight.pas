unit UrlHighlight;
{ Copyright (c) Electro-Concept Mauricie, 1998-2004

  A component to dynamically highlight Internet urls of all kind, including short forms of
  web and email urls (ex: www.ecmqc.com)
}


{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D7New} {$ENDIF}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

{$DEFINE UrlHighlight}
{UCONVERT}
  {$IFDEF UrlHighlightClx}
    {$DEFINE pmClx}
  {$ENDIF}
{/UCONVERT}


interface

{$IFDEF pmClx}
uses Classes, PlusMemoClx, PMSupportClx, ExtHilitClx;
{$ELSE}
uses Classes, PlusMemo, PMSupport, ExtHilit;
{$ENDIF}

const
  URL_HTTP     = 0;    { values to add to ContextNumber to know which particular kind of url }
  URL_WWW      = 1;
  URL_MAILTO   = 2;
  URL_MAILshort= 3;
  URL_NEWS     = 4;
  URL_NNTP     = 5;
  URL_FTP      = 6;
  URL_FTPshort = 7;
  URL_GOPHER   = 8;
  URL_GOPHERshort = 9;
  URL_TELNET   = 10;
  URL_TN3270   = 11;
  URL_WAIS     = 12;
  URL_PROSPERO = 13;
  URL_HTTPS    = 14;

type
  TURLType = (urlWeb, urlEmail, urlNNTP, urlFile, urlOther);
  TURLEvent = function (Sender: TObject; CurrentContext: SmallInt; URLType: TURLType): Boolean of object;
  TURLLaunchEvent = function (Sender: TObject; var URL: string): Boolean of object;

  TURLHighlighter = class(TPlusHighlighter)
    private
      fWebURL,
      fEmailURL,
      fFileURL,
      fNNTP_URL,
      fOtherURL     : THighlightInfo;
      fBaseContext  : Integer;
      fLaunchURLS   : Boolean;
      fDblClickLaunch: Boolean;
      fScope        : Integer;
      fPriority     : Boolean;
      fOnUrl        : TURLEvent;
      fOnUrlLaunch  : TURLLaunchEvent;

      fMemo         : TPlusMemo;    // remember last memo we attached to notify list, to avoid doing it repetitively
      fContextString: string;
      fDblClick     : Boolean;

      fNav1, fNav2: TPlusNavigator;

      function DeNotify(Memo: TPlusMemo): Boolean;
      procedure HighlightChange(Sender: TObject);
      procedure setWebURL(h: THighlightInfo);
      procedure setEmailURL(h: THighlightInfo);
      procedure setFileURL(h: THighlightInfo);
      procedure setNNTP_URL(h: THighlightInfo);
      procedure setOtherURL(h: THighlightInfo);
      procedure setScope(sc: Integer);
      procedure setPriority(const Value: Boolean);

    protected
      procedure Loaded; override;
      procedure ApplyKeywordsList(Start, Stop: TPlusNavigator; BaseIndex: Integer);  override;

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Notify(Sender: TComponent; Events: TpmEvents); override;

    published
      property BaseContext: Integer      read fBaseContext write fBaseContext;
      property WebURL   : THighlightInfo read fWebURL   write setWebURL;
      property EmailURL : THighlightInfo read fEmailURL write setEmailURL;
      property NNTP_URL : THighlightInfo read fNNTP_URL write setNNTP_URL;
      property FileURL  : THighlightInfo read fFileURL  write setFileURL;
      property OtherURL : THighlightInfo read fOtherURL write setOtherURL;
      property LaunchURLs: Boolean       read fLaunchURLs write fLaunchURLs;
      property DblClickLaunch: Boolean   read fDblClickLaunch write fDblClickLaunch;
      property Scope    : Integer        read fScope    write setScope;
      property Priority : Boolean        read fPriority write setPriority;
      property OnURL    : TURLEvent      read fOnUrl    write fOnUrl;
      property OnURLLaunch: TURLLaunchEvent read fOnUrlLaunch write fOnUrlLaunch;
    end;


procedure Register;

implementation

{$IFDEF pmClx}
uses Qt, QGraphics, QControls, QForms {$IFDEF LINUX}, LibC {$ENDIF};
{$ELSE}
uses Graphics, Controls, Forms, ShellAPI, Windows, Messages;
{$ENDIF}

const
  WebURLColor    = clBlue;    { default creation colors for TURLHighlighter }
  EmailURLColor  = clGreen;
  NNTP_URLColor  = clRed;
  FileURLColor   = clFuchsia;
  OtherURLColor  = clSilver;

  NbUrls = 15;

type
  URLRecord = record
      Key      : string;
      URLSpecie: TURLType;
      StartURL : Boolean
    end;

  URLArray = array[0..NbURLs-1] of URLRecord;

const
  URLs: URLArray =
      ((Key: 'HTTP://';   URLSpecie: urlWeb; StartURL: True),
       (Key: 'WWW.';      URLSpecie: urlWeb; StartURL: True),
       (Key: 'MAILTO:';   URLSpecie: urlEmail; StartURL: True),
       (Key: '@';         URLSpecie: urlEmail; StartURL: False),
       (Key: 'NEWS:';     URLSpecie: urlNNTP;  StartURL: True),
       (Key: 'NNTP://';   URLSpecie: urlNNTP;  StartURL: True),
       (Key: 'FTP://';    URLSpecie: urlFile;  StartURL: True),
       (Key: 'FTP.';      URLSpecie: urlFile;  StartURL: True),
       (Key: 'GOPHER://'; URLSpecie: urlOther; StartURL: True),
       (Key: 'GOPHER.';   URLSpecie: urlOther; StartURL: True),
       (Key: 'TELNET://'; URLSpecie: urlOther; StartURL: True),
       (Key: 'TN3270://'; URLSpecie: urlOther; StartURL: True),
       (Key: 'WAIS://';   URLSpecie: urlOther; StartURL: True),
       (Key: 'PROSPERO://'; URLSpecie: urlOther; StartURL: True),
       (Key: 'HTTPS://';  URLSpecie: urlWeb;   StartURL: True));

  URLDelimiters = ['<', '>', ' ', #9, '"', '''', '(', ')'];

var LongestKey: Integer;  { initialized at startup }

{$IFDEF MSWindows}
  {$IFDEF pmClx}
  function ShellExecute(hWnd: THandle; Operation, FileName, Parameters,
                        Directory: PChar; ShowCmd: Integer): HINST; stdcall; external 'shell32.dll' name 'ShellExecuteA';
  const SW_SHOWNORMAL = 1;
  {$ENDIF}
{$ENDIF}

constructor TURLHighlighter.Create(AOwner: TComponent);
  procedure CreateHighlight(var h: THighlightInfo; style: TFontStyles; bk, fr: TColor);
    begin
      h:= THighlightInfo.Create;
      h.AltFont:=  fsAltFont in TPlusFontStyles(style);
      h.Style:= TFontStyles(TPlusFontStyles(style) - [fsAltFont]);
      h.Background:= bk;
      h.Foreground:= fr;
      h.Cursor:= crHandPoint;
      h.OnChange:= HighlightChange
    end;
begin
  inherited Create(AOwner);
  fBaseContext:= 1024;
  CreateHighlight(fWebURL, [fsUnderline], -1, WebURLColor);
  CreateHighlight(fEmailURL, [fsUnderline], -1, EmailURLColor);
  CreateHighlight(fNNTP_URL, [fsUnderline], -1, NNTP_URLColor);
  CreateHighlight(fFileURL, [fsUnderline], -1, FileURLColor);
  CreateHighlight(fOtherURL, [fsItalic], clInfoBk, OtherURLColor);
  fLaunchURLs:= True;
  fNav1:= TPlusNavigator.Create(nil);
  fNav2:= TPlusNavigator.Create(nil);
end;


destructor TURLHighlighter.Destroy;
var i: Integer;
begin
  for i:= 0 to MemoList.Count-1 do DeNotify(MemoList[i]);
  fWebURL.Destroy;
  fEmailURL.Destroy;
  fNNTP_URL.Destroy;
  fFileURL.Destroy;
  fOtherURL.Destroy;
  fNav1.Destroy;
  fNav2.Destroy;
  inherited Destroy
end;

function TURLHighlighter.DeNotify(Memo: TPlusMemo): Boolean;
var i: Integer; snotify: IpmsNotify;
begin
  snotify:= Self;
  i:= Memo.MsgList.IndexOf(Pointer(snotify));
  Result:= i>=0;
  if Result then
    begin
      Memo.MsgList[i]:= nil;
      fContextString:= '';
    end
end;

procedure TURLHighlighter.HighlightChange(Sender: TObject);
  { event handler for internal THighlightInfo modifications }
var i: Integer;
begin
  if ComponentState*[csDesigning, csLoading]=[] then
    for i:= 0 to MemoList.Count-1 do
        TPlusMemo(MemoList[i]).ReApplyKeywords
end;

procedure TURLHighlighter.Loaded;
begin
  inherited Loaded;
  HighlightChange(nil)
end;

{$IFDEF pmClx}
const
  WM_LBUTTONDBLCLK = QEventType_MouseButtonDblClick;
  WM_LBUTTONUP = QEventType_MouseButtonRelease;
  WM_MOUSEMOVE = QEventType_MouseMove;
  WM_MOUSEFIRST = QEventType_MouseButtonPress;
  WM_MOUSELAST = QEventType_MouseMove;
  WM_KEYDOWN = QEventType_KeyPress;
{$ENDIF}

procedure TURLHighlighter.Notify(Sender: TComponent; Events: TpmEvents);
  procedure GoUrl;
    {$IFDEF Linux} var scstring: AnsiString; {$ENDIF}
    begin
      if Assigned(OnURLLaunch) and not fOnURLLaunch(Self, fContextString) then Exit;
      {$IFDEF Linux}
        scstring:= fContextString;
        scstring:= 'kfmclient openURL ''' + scstring + '''';
        Libc.system(PAnsiChar(scstring))
      {$ELSE}
        ShellExecute({$IFDEF pmClx} 0 {$ELSE} Application.Handle {$ENDIF}, nil,
                     PChar(fContextString), nil, nil, SW_SHOWNORMAL)
      {$ENDIF}
    end;

var
  smemo: TPlusMemo;
  spos: Integer;
  scontext: Integer;
  snav: TPlusNavigator;
  snotify: IpmsNotify;
  sMsg: {$IFDEF pmClx} QEventType {$ELSE} Cardinal {$ENDIF};
begin
  smemo:= Sender as TPlusMemo;

  { Note: with DblClickLaunch, here are the order of events:
    - pm_Context: received directly by TPlusMemo.MouseUp: urlhighlighter registers in MsgList
    - WM_MOUSEMOVE
    - WM_LBUTTONDBLCLK => flags a double click (fDblClick=True)
    - pm_SelMove
    - WM_MOUSEUP  => launch url, remove registration
    - pm_Context: received directly by TPlusMemo.MouseUp => we remove the double click flag
  }

  if LaunchUrls and (Events*[pmeContext, pmeRightContext]<>[]) then
    begin
      if fDblClick then fDblClick:= False
      else
        begin
          scontext:= smemo.MouseNav.Context;
          if (scontext>=BaseContext) and (scontext<BaseContext+NbUrls) then
            begin
              snav:= TPlusNavigator.Create(nil);
              snav.fPMemo:= smemo;
              snav.Assign(smemo.MouseNav);
              if not snav.ForwardToDyn(smemo.CharCount) then snav.Pos:= smemo.CharCount;
              spos:= snav.Pos;
              snav.BackToDyn(0);
              SetLength(fContextString, spos-snav.Pos);
              snav.GetTextBuf(PChar(fContextString), Length(fContextString));
              snav.fPMemo:= nil;
              snav.Free;

              { the following is necessary to have Win95 properly launch some short form urls
                Win98 and WinNT do not require this }
              case scontext - BaseContext of
                  URL_MAILshort  : fContextString:= 'mailto:' + fContextString;
                  URL_FTPshort   : fContextString:= 'ftp://' + fContextString;
                  URL_GOPHERshort: fContextString:= 'gopher://' + fContextString
                end;

              if not fDblClickLaunch then GoUrl
              else
                begin   // add to message notify list
                  snotify:= Self;
                  if smemo.MsgList.IndexOf(Pointer(snotify))<0 then smemo.MsgList.Add(Pointer(snotify))
                end
            end  // context in range
        end  // not fDblClick
    end

  else

    if pmeChange in Events then DeNotify(smemo)
    else
        if pmeMessage in Events then
          begin
            sMsg:= {$IFDEF pmClx} QEvent_Type(QEventH(smemo.WinMsg.Msg)) {$ELSE} smemo.WinMsg.Msg {$ENDIF};
            case sMsg of
              WM_LBUTTONDBLCLK: fDblClick:= True;
              WM_LBUTTONUP:   // do the launch, remove notification
                begin
                  if fDblClick and (fContextString<>'') then 
                    begin
                      GoUrl;
                      smemo.SelStart:= smemo.MouseNav.Pos
                    end;
                  DeNotify(smemo)
                end;
              WM_Mousemove: begin end  // nothing to do, just prevent removing notifications
              else
                case sMsg of
                  WM_MOUSEFIRST..WM_MOUSELAST, WM_KEYDOWN: DeNotify(smemo)
                end
            end
        end
end;     // procedure Notify

procedure TURLHighlighter.setWebURL(h: THighlightInfo);
begin
  fWebURL.Assign(h)
end;

procedure TURLHighlighter.setEmailURL(h: THighlightInfo);
begin
  fEmailURL.Assign(h)
end;

procedure TURLHighlighter.setNNTP_URL(h: THighlightInfo);
begin
  fNNTP_URL.Assign(h)
end;

procedure TURLHighlighter.setFileURL(h: THighlightInfo);
begin
  fFileURL.Assign(h)
end;

procedure TURLHighlighter.setOtherURL(h: THighlightInfo);
begin
  fOtherURL.Assign(h)
end;

procedure TURLHighlighter.setScope(sc: Integer);
begin
  if sc<>fScope then
    begin
      fScope:= sc;
      HighlightChange(nil)
    end
end;

procedure TURLHighlighter.setPriority(const Value: Boolean);
begin
  if fPriority<>Value then
    begin
      fPriority:= Value;
      HighlightChange(nil)
    end;
end;

procedure TURLHighlighter.ApplyKeywordsList(Start, Stop: TPlusNavigator; BaseIndex: Integer); { they must be in the same par. }
  function isurl(const d: DynInfoRec): Boolean;
    begin
      Result:= (d.DynStyle and $c0 = $c0) and (d.Level>=0) and
               (d.KeyIndex[d.Level]>=BaseIndex) and (d.KeyIndex[d.Level]<BaseIndex+NbUrls);
    end;

  function urlok(anav: TPlusNavigator; utype: TUrlType): Boolean;
    var curscope: Integer;
    begin
      Result:= False;
      if DynToLevel(anav.pDynAttr^)>=15 then Exit;
      curscope:= DynToContext(anav.pDynAttr^);
      if Assigned(OnUrl) then Result:= OnUrl(Self, curscope, utype)
                         else Result:= Priority or (curscope=fScope)
    end;


var
  slen            : Byte;             { length of key currently under test }
  t, tsearch      : PChar;
  tlen            : Integer;
  startparoffset  : Integer;     { real offset value of Start }

  backstart, backend, eoff: Integer;  { how much text we must examine to the left and right of Start-Stop }

  i             : Integer;  { index in t }
  skfound       : SmallInt;      { index in URLs array when a url is found }
  j             : SmallInt;      { index for looping in URLs array }
  tcomp1, tcomp2: PChar;         { used when comparing retrieved text against the keys }
  delfound      : Boolean;       { used in checking/finding the boundaries of a url just found }
  dinfo         : DynInfoRec;    { filled and passed to SetDynStyleP when a url is found }
  h: THighlightInfo;             { instance used to highlight the url just found }
  smemo  : TPlusMemo;
  doneurl, someurl: Boolean;
  urllen : Integer;
  snotify: IpmsNotify;

begin
  smemo:= TPlusMemo(Start.fPMemo);
  if GetParLength(Start.Par^)=0 then Exit;
  fNav1.fPMemo:= smemo;
  fNav2.fPMemo:= smemo;
  fNav1.Assign(Start);
  someurl:= False;

  // check for being at the right of a previously trimmed off url
  if fNav1.BackToDyn(fNav1.fPar.StartOffset) and isurl(fNav1.pDynAttr^) then
    begin
      someurl:= True;
      for i:= fNav1.fOffset to Start.fOffset-1 do
        if not (pmChar(fNav1.fPar.ParText[i]) in ['.', ',', ';']) then
          begin
            someurl:= False;
            Break
          end;
      if someurl then
        begin
          fNav1.RemoveDyn;
          if fNav1.BackToDyn(fNav1.fPar.StartOffset) then fNav1.RemoveDyn;
          Start.Assign(fNav1);
          Stop.fDynNb:= -1
        end
    end;

  fNav1.RightOfDyn;

  if Start.fOffset>=LongestKey+1 then backstart:= LongestKey+1
                                 else backstart:= Start.fOffset;
  fNav1.Pos:= Start.Pos-backstart;

  eoff:= Stop.ParOffset;
  if eoff+LongestKey+1>GetParLength(Stop.fPar^) then backend:= GetParLength(Stop.fPar^)-eoff
                                                else backend:= LongestKey+1;

  tlen:= eoff-Start.ParOffset+backstart+backend;
  t:= smemo.GetUpText(Start.fPar, Start.fParNb, fNav1.ParOffset, tlen);

  startparoffset:= fNav1.ParOffset;

  i:= 0;
  while i<=tlen-backend do
    begin
    skfound:= -1;

    for j:= 0 to NbURLs-1 do
      with URLs[j] do
        begin
          slen:= Length(Key);
          if (i+slen+1>backstart) and (i+slen<=tlen) then
            begin
              tcomp1:= PChar(Key);
              tsearch:=t;

              {if CompareMem(@key[1], @tsearch[i], slen) then} {//lets provide a faster way!}
              tcomp2:= tsearch+i;
              while (slen>0) and (tcomp1^=tcomp2^) do
                begin
                  Dec(slen);
                  Inc(tcomp1);
                  Inc(tcomp2)
                end;
              if slen=0 then
                begin
                  slen:= Length(Key);
                  if ((StartURL and ((i=0) or (pmChar(tsearch[i-1]) in URLDelimiters)) or
                      (not StartURL and (i>0) and (not (pmChar(tsearch[i-1]) in URLDelimiters)))) and
                     ((i+slen<tlen) and (not (pmChar(tsearch[i+slen]) in URLDelimiters)))) then
                    begin
                      skfound:= j;
                      Break
                    end
                end  { slen=0, i.e. found URL key }
            end;  { i is in range for this key }
          end;   { j loop in URLs }

    if {not found} skfound=-1 then Inc(i)
    else
      begin  { found url key }
        doneurl:= False;
        fNav1.Pos:= fNav1.fPar^.StartOffset+i+startparoffset;

        if not URLs[skfound].StartURL then
          begin     { reach the start of the URL with skey}
            delfound:= False;
            while (fNav1.ParOffset>0) and (not delfound) do
              begin
                fNav1.Pos:= fNav1.Pos-1;
                delfound:= fNav1.AnsiText in URLDelimiters
              end;
            if delfound then fNav1.Pos:= fNav1.Pos + 1
          end;
        fNav1.RightOfDyn;
        { reach the end of the url with skey1 }
        fNav2.Assign(fNav1);
        fNav2.Pos:= fNav2.Pos + slen;
        dinfo.Level:= DynToLevel(fNav2.DynAttr)+1;
        while (fNav2.fOffset<GetParLength(fNav2.fPar^)) and (not (fNav2.AnsiText in URLDelimiters)) do
             fNav2.Pos:= fNav2.Pos +1;
        { go back to trim off ending period or comma }
        while (fNav2.Pos>fNav1.Pos+1) and (pmChar(fNav1.fPar.ParText[fNav2.ParOffset-1]) in ['.', ',', ';']) do
            fNav2.Pos:= fNav2.Pos-1;

        if (Priority or (fNav2.DynNb=fNav1.DynNb)) and urlok(fNav1, URLs[skfound].URLSpecie) then
          begin
            { everything is in place: fill the dinfo fields }
            case URLs[skfound].URLSpecie of
                urlWeb: h:= fWebURL;
                urlEmail: h:= fEmailURL;
                urlNNTP:  h:= fNNTP_URL;
                urlFile:  h:= fFileURL;
                urlOther: h:= fOtherURL
                else h:= fOtherURL
              end;

            with dinfo do
              begin
                KeyIndex:= fNav2.DynAttr.KeyIndex;
                DynStyle:= Byte(h.Style) or $c0;
                KeyIndex[Level]:= skfound+BaseIndex;
                CollpsState:= [];
                Cursor:= h.Cursor;
                Backgnd:= h.Background;
                Foregnd:= h.Foreground;
                Context:= BaseContext + skfound;
                urllen:= fNav2.Pos-fNav1.Pos;
                if urllen>65535 then
                  begin
                    urllen:= 65535;
                    fNav2.Pos:= fNav1.Pos+65535
                  end;
                StartKlen:= urllen;
                StopKLen:= StartKLen
              end;

            { that's it }
            SetDynStyleP(smemo.IParList, fNav1, fNav2, dinfo, True, False);

            { prepare to continue the loop }
            i:= fNav2.ParOffset + startparoffset;
            doneurl:= True
          end; { urlok }

        someurl:= someurl or doneurl;
        if not doneurl then Inc(i)
      end { found url }

    end; { iloop in text buffer }

  if someurl then
    begin
      if LaunchUrls and (smemo<>fMemo) then // add ourself to notification list if not already done, to receive mouse context events
        begin
          fMemo:= smemo;
          snotify:= Self;
          if smemo.NotifyList.IndexOf(Pointer(snotify))<0 then smemo.NotifyList.Add(Pointer(snotify))
        end;
      InvalidateNavs(smemo.INavigators, Start.Pos, Start.fParNb)
    end;

  fNav1.fPMemo:= nil;
  fNav2.fPMemo:= nil
end;  { ApplyKeywordsList }


procedure Register;
begin
  RegisterComponents({UCONVERT}'PlusMemo'{/UCONVERT}, [TURLHighlighter])
end;

var i, klen: Integer;

initialization
  for i:= 0 to NbURLs-1 do
    begin
      klen:= Length(URLs[i].Key);
      if klen>LongestKey then LongestKey:= klen
    end;

end.

