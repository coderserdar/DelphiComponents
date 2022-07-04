(*

eICQ: the free ICQ for Microsoft(tm) Windows(tm)

Copyright 2003-2004 eICQ ICQ project,
all portions of this codebase are copyrighted to the people
listed in contributors.txt.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*)

unit PktDump;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  StdCtrls, ComCtrls, ICQWorks;

type
  TPktDumpForm = class(TForm)
    ListView1: TListView;
    Memo1: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
  public
    FPktList: TList;
  end;

var
  PktDumpForm: TPktDumpForm;

implementation

{$R *.dfm}

procedure TPktDumpForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TPktDumpForm.FormCreate(Sender: TObject);
begin
  FPktList := TList.Create;
end;

procedure TPktDumpForm.FormDestroy(Sender: TObject);
var
  i: Word;
begin
  //Let's free all packets stored in memory
  if FPktList.Count > 0 then
    for i := 0 to FPktList.Count - 1 do
      FreeMem(FPktList.Items[i], SizeOf(TRawPkt));
  FPktList.Free;
end;

procedure TPktDumpForm.ListView1Click(Sender: TObject);
var
  p: PRawPkt;
begin
  if ListView1.Selected = nil then
    Exit;
  p := FPktList.Items[ListView1.Selected.Index];
  if p^.Len = 0 then
    Memo1.Text := DumpPacket(p, Swap16(PFlapHdr(p)^.DataLen) + TFLAPSZ)
  else
    Memo1.Text := DumpPacket(p, p^.Len)  
end;

procedure TPktDumpForm.ListView1SelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  ListView1Click(Sender);
end;

end.
