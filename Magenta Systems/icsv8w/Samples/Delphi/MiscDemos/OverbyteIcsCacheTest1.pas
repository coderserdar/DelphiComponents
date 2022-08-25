{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Creation:     March 2009
Description:  Test of TCacheTree class used in TSslAvlSessionCache.
              Delphi 7 and better.
Version:      1.00
EMail:        francois.piette@overbyte.be    http://www.overbyte.be
Support:      Unsupported code.
Legal issues: Copyright (C) 2009 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to François PIETTE. Use a nice stamp and mention your name,
                 street address, EMail address and any comment you like to say.

History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsCacheTest1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ExtCtrls, ComCtrls, DateUtils,
  OverbyteIcsAvlTrees;

const
  WM_APPINIT = WM_USER + 1;

type
  TCacheTestForm = class(TForm)
    btnInsert: TButton;
    btnListTree: TButton;
    btnSearch: TButton;
    lbResult: TLabel;
    SpinEdit1: TSpinEdit;
    Edit1: TEdit;
    btnOldest: TButton;
    btnFill: TButton;
    btnRemove: TButton;
    btnClear: TButton;
    btnDeleteRoot: TButton;
    lbRoot: TLabel;
    btnClearMemo: TButton;
    lbCount: TLabel;
    lbCaption: TLabel;
    btnPerform: TButton;
    Memo1: TMemo;
    btnFlush: TButton;
    DateTimePicker1: TDateTimePicker;
    procedure FormCreate(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnListTreeClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnOldestClick(Sender: TObject);
    procedure btnFillClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnDeleteRootClick(Sender: TObject);
    procedure btnClearMemoClick(Sender: TObject);
    procedure btnPerformClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnFlushClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private
    TestCache : TCacheTree;
    FIsAppInit: Boolean;
    procedure TestCacheFreeData(Sender: TObject; Data: Pointer; Len: Integer);
    procedure TestCacheList(Sender: TObject; const Key: String;
         TimeStamp: TDateTime; Data: Pointer; Len: Integer;
         Expires: TDateTime; var Cancel: Boolean);
    procedure RefreshLabels;
  protected
    procedure WmAppInit(var Msg: TMessage); message WM_APPINIT;
  end;

  TCounter = record
    A : Int64;
    B : Int64;
    C : Int64;
  end;

var
    CacheTestForm: TCacheTestForm;

implementation

{$R *.DFM}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure StartCount(var ACounter : TCounter);
begin
    ACounter.C := 0;
    QueryPerformanceFrequency(ACounter.A);
    QueryPerformanceCounter(ACounter.B);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure StopCount(var ACounter : TCounter);
begin
    QueryPerformanceCounter(ACounter.C);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetCountMsec(ACounter : TCounter): Int64;
begin
    if (ACounter.C > ACounter.B) and (ACounter.A > 0) then
        Result := (ACounter.C - ACounter.B) * 1000 div ACounter.A
    else
        Result := 0;    
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.FormCreate(Sender: TObject);
begin
    TestCache                := TCacheTree.Create;
    TestCache.OnList         := TestCacheList;
    TestCache.OnFreeData     := TestCacheFreeData;
    DateTimePicker1.DateTime := Now;
    Memo1.Clear;
    Constraints.MinWidth     := Width;
    Randomize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.FormDestroy(Sender: TObject);
begin
    TestCache.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.FormShow(Sender: TObject);
begin
    if not FIsAppInit then begin
        FIsAppInit := TRUE;
        PostMessage(Handle, WM_APPINIT, 0, 0);
    end;
end;


procedure TCacheTestForm.Memo1Change(Sender: TObject);
begin

end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.WmAppInit(var Msg: TMessage);
begin
    ShowMessage(
    'Note that TCacheTree will leak memory if FastMM is not used!'#13#10 +
    'Since Delphi 2006 FastMM is the default memory manager shipped'#13#10 +
    'with Delphi.'#13#10 +
    'The FastMM is open source, supports older compiler as well'#13#10 +
    'and is downloadable at http://fastmm.sourceforge.net.'#13#10 +
    'In order to get best performance define RELEASE in the project'#13#10 +
    'options. If your compiler is Delphi 2009 turn off "String format'#13#10 +
    'checking" in the project options as well');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.TestCacheFreeData(Sender: TObject; Data: Pointer;
  Len: Integer);
begin
    FreeMem(Data);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.TestCacheList(Sender: TObject; const Key: String;
    TimeStamp: TDateTime; Data: Pointer; Len: Integer; Expires: TDateTime;
    var Cancel: Boolean);
begin
    if Data <> nil then
        Memo1.Lines.Add(Key + #09 + FormatDateTime('hh:nn:ss:zzz', TimeStamp) +
                        #09 + String(PAnsiChar(Data)))
    else
        Memo1.Lines.Add(Key + #09 + FormatDateTime('hh:nn:ss:zzz', TimeStamp));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.RefreshLabels;
begin
    lbCount.Caption := 'Count=' + IntToStr(TestCache.Count);
    if Assigned(TestCache.Root) then
        lbRoot.Caption := 'Root=' + TCacheNode(TestCache.Root).Key
    else
        lbRoot.Caption := 'Root= nil'; 
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.btnInsertClick(Sender: TObject);
begin
    TestCache.Insert(Edit1.Text, nil, 0);
    RefreshLabels;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.btnListTreeClick(Sender: TObject);
begin
    lbCaption.Caption := 'Key' + #09 + 'TimeStamp' + #09#09 + 'Data';
    lbCaption.Update;
    Memo1.Clear;
    TestCache.ListTree;
    RefreshLabels;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.btnSearchClick(Sender: TObject);
var
    Node : TCacheNode;
begin
    Node := TestCache.FindKey(Edit1.Text);
    if Node <> nil then
        lbResult.Caption := Node.Key
    else
        lbResult.Caption := 'Not Found';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.btnOldestClick(Sender: TObject);
var
    Node: TCacheNode;
begin
    Node := TestCache.Oldest;
    if Assigned(Node) then
        lbResult.Caption := '"' + Node.Key + '" ' +
                      FormatDateTime('hh:nn:ss:zzz', Node.IdxRef.TimeStamp)
    else
        lbResult.Caption := 'Not found';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.btnFillClick(Sender: TObject);
var
    I : Integer;
    C : TCounter;
    DT : TDateTime;
    Data : Pointer;
    DataLen : Integer;
begin
    //TestCache.Clear;
    DT := Now;
    StartCount(C);
    for I := 1 to SpinEdit1.Value do begin
        // Random data length
        DataLen := Random(32);
        if DataLen > 0 then begin
            GetMem(Data, DataLen);
            FillChar(Data^, DataLen, #124);
            PAnsiChar(Data)[DataLen - 1] := #0;
        end
        else
            Data := nil;

        TestCache.Insert(IntToStr(I), Data, DataLen, DT);
        if I mod 2 = 0 then  // Comment to avoid secondary index duplicates
            DT := IncMilliSecond(DT, 1);
    end;
    StopCount(C);
    lbResult.Caption := 'Duration = ' + IntToStr(GetCountMsec(C));
    RefreshLabels;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.btnRemoveClick(Sender: TObject);
begin
    if not TestCache.RemoveKey(Edit1.Text) then
        Beep;
    RefreshLabels;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.btnClearClick(Sender: TObject);
var
    C : TCounter;
begin
    StartCount(C);
    TestCache.Clear;
    StopCount(C);
    lbResult.Caption := 'Duration = ' + IntToStr(GetCountMsec(C));
    RefreshLabels
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.btnDeleteRootClick(Sender: TObject);
var
    C : TCounter;
begin
    StartCount(C);
    while Assigned(TestCache.Root) do
        TestCache.Remove(TestCache.Root);
    StopCount(C);
    lbResult.Caption := 'Duration = ' + IntToStr(GetCountMsec(C));
    RefreshLabels;    
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.btnClearMemoClick(Sender: TObject);
begin
    lbCaption.Caption := '';
    Memo1.Clear
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.btnPerformClick(Sender: TObject);
var
    SL  : TStringList;
    SL2 : TStringList;
    DT  : TDateTime;
    I   : Integer;
    C   : TCounter;
begin
    lbCaption.Caption := '';
    lbCaption.Update;
    TestCache.Clear;
    RefreshLabels;
    SL := TStringList.Create;
    SL2 := TStringList.Create;
    try
        Memo1.Lines.Add('Loading and randomizing word list..');

        SL.LoadFromFile('Dictionary.txt');
        while SL.Count > 0 do begin
            I := Random(SL.Count - 1);
            SL2.Add(SL[I]);
            SL.Delete(I);
        end;
        Memo1.Lines.Add('Word list contains ' + IntToStr(SL2.Count) + ' entries');
        Memo1.Lines.Add('Inserting ' + IntToStr(SL2.Count) + ' entries..');

        DT := Now;

        StartCount(C);
        for I := 0 to SL2.Count -1 do begin
            TestCache.Insert(SL2[I], nil, 0, DT);
            if I mod 2 = 0 then  // Comment to avoid secondary index duplicates
                DT := IncMilliSecond(DT, 1);
        end;
        StopCount(C);
        Memo1.Lines.Add('Duration = ' + IntToStr(GetCountMsec(C)) + ' ms');

        Memo1.Lines.Add('Now starting 10.000 random searches on the string keys..');

        StartCount(C);
        for I := 1 to 10000 do begin
            if TestCache.FindKey(SL2[Random(SL2.Count - 1)]) = nil then
                raise Exception.Create('NOT FOUND WHY?');
        end;
        StopCount(C);
        Memo1.Lines.Add('Duration = ' + IntToStr(GetCountMsec(C)) + ' ms');
        Memo1.Lines.Add('finished');
        RefreshLabels;

    finally
        FreeAndNil(SL);
        FreeAndNil(SL2)
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCacheTestForm.btnFlushClick(Sender: TObject);
var
    C : TCounter;
begin
    StartCount(C);
    TestCache.Flush(DateTimePicker1.DateTime);
    StopCount(C);
    lbResult.Caption := 'Duration = ' + IntToStr(GetCountMsec(C));
    RefreshLabels;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
