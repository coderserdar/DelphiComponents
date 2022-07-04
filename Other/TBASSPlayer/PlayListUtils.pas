// unit PlayListUtils

//    written by Silhwan Hyun  (hyunsh@hanafos.com)
//
// Ver 1.1                        20 Apr 2009
//   - Modified for Delphi 2009
//
// Ver 1.00.1                      3 Oct 2008
//   - Added a function GetAPlayListArtist.
//   - Modified procedure _AddToPlayList to include a parameter "Artist".
//
// Ver 1.0                         2 Aug 2008
//   - Initial release
//
//  note) The functions of this unit can be called by BASSPlayer.pas(= main unit of TBASSPlayer)
//        or by the sub units of TBASSPlayer.
//       So, the call of the functions which causes altering of Play List is only executed by
//        BASSPlayer.pas to keep consistency.


unit PlayListUtils;

interface

uses classes;

  procedure _ClearPlayList;
  procedure _AddToPlayList(FilePath, Title, Artist : string); // * Changed at Ver 1.00.1
  procedure _DeleteFromPlayList(ListIndex : integer);
  procedure _SetPlayListIndex(ListIndex : integer);
  function  GetNumOfPlayListItem : integer;
  function  GetAPlayListTitle(ListIndex : integer) : string;
  function  GetAPlayListArtist(ListIndex : integer) : string; // * New at Ver 1.00.1
  function  GetAPlayListFile(ListIndex : integer) : string;
  function  GetIndexOfPlayList(FilePath : string) : integer;
  procedure ChangeTitle(FilePath : string; Title : string);


implementation

var
   PlayList_Path : TStringList;
   PlayList_Title : TStringList;
   PlayList_Artist : TStringList;
   PlayList_Index : integer;

procedure _ClearPlayList;
begin
   PlayList_Path.Clear;
   PlayList_Title.Clear;
   PlayList_Artist.Clear;
   PlayList_Index := -1;
end;

procedure _AddToPlayList(FilePath, Title, Artist : string);
begin
   PlayList_Path.Add(FilePath);
   PlayList_Title.Add(Title);
   PlayList_Artist.Add(Artist);
end;

procedure _DeleteFromPlayList(ListIndex : integer);
begin
   PlayList_Path.Delete(ListIndex);
   PlayList_Title.Delete(ListIndex);
   PlayList_Artist.Delete(ListIndex);
end;

procedure _SetPlayListIndex(ListIndex : integer);
begin
   if (ListIndex < -1) or (ListIndex > (PlayList_Title.Count - 1)) then
      exit;

   PlayList_Index := ListIndex;
end;

function GetNumOfPlayListItem : integer;
begin
   result := PlayList_Title.Count;
end;

function GetAPlayListTitle(ListIndex : integer) : string;
begin
   result := '';

   if PlayList_Path.Count = 0 then
      exit;

   if (ListIndex < 0) or (ListIndex >= PlayList_Path.Count) then
      exit;

   result := PlayList_Title[ListIndex];
end;

function GetAPlayListArtist(ListIndex : integer) : string;
begin
   result := '';

   if PlayList_Path.Count = 0 then
      exit;

   if (ListIndex < 0) or (ListIndex >= PlayList_Path.Count) then
      exit;

   result := PlayList_Artist[ListIndex];
end;

function GetAPlayListFile(ListIndex : integer) : string;
begin
   result := '';

   if PlayList_Path.Count = 0 then
      exit;

   if (ListIndex < 0) or (ListIndex >= PlayList_Path.Count) then
      exit;

   result := PlayList_Path[ListIndex];
end;

function GetIndexOfPlayList(FilePath : string) : integer;
var
   i : integer;
begin
   result := -1;

   if PlayList_Path.Count = 0 then
      exit;

 // If (FilePath = '') then the current index number of Play List is returned,
 //  else the the index number of the item which holds matched FilePath is returned.
   if FilePath = '' then
   begin
      result := PlayList_Index;
      exit;
   end;

   if PlayList_Path[PlayList_Index] = FilePath then
   begin
      result := PlayList_Index;
      exit;
   end;

   for i := 0 to (PlayList_Path.Count - 1) do
      if FilePath = PlayList_Path[i] then
      begin
         result := i;
         break;
      end;

end;

procedure ChangeTitle(FilePath : string; Title : string);
var
   RegisteredNo : integer;
begin
   RegisteredNo := GetIndexOfPlayList(FilePath);
   if RegisteredNo <> -1 then
      PlayList_Title[RegisteredNo] := Title;
end;

initialization
   PlayList_Path := TStringList.Create;
   PlayList_Title := TStringList.Create;
   PlayList_Artist := TStringList.Create;
   PlayList_Index := -1;

finalization
   PlayList_Path.Free;
   PlayList_Title.Free;
   PlayList_Artist.Free;

end.
