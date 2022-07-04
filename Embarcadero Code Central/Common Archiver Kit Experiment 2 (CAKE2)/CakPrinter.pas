unit CakPrinter;

// Common Archiver Kit (CAK) File List Printer
// Common Interface for Compression/Decompression components.

//Copyright (C) Joseph Leung 2003 (lycj@yahoo.com)
//
//This library is free software; you can redistribute it and/or
//modify it under the terms of the GNU Lesser General Public
//License as published by the Free Software Foundation; either
//version 2.1 of the License, or (at your option) any later version.
//
//This library is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//Lesser General Public License for more details.
//
//You should have received a copy of the GNU Lesser General Public
//License along with this library; if not, write to the Free Software
//Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

interface

uses
  Windows, Messages, SysUtils, Classes, Cakdir2, CakDefs2;

const
  TotalColumns = 8;
  Columns: array[1..TotalColumns] of string =
    ('Filename', 'Type', 'Size', 'Date', 'Pack',
    '%', 'Crc', 'Path');
  StartAt: array[1..TotalColumns] of Integer =
    (70, 140, 240, 270, 360, 390, 410, 460);

type
  TCakPrinter = class(TComponent)
  private
    { Private declarations }
    Fltype : FileListType;
    Flname : string;
    Sepr : char;
    Floption: FileListOptionsType;
    CakDir: TCakDir2;
    function Maxlength(coltocheck: FileListItemType): Integer;
    procedure SetFltype(ftype : FileListType);
    procedure WriteCsv;
    procedure WriteTxt;
    procedure WriteHtm;

  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Print;
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property OutputType : FileListType read Fltype write SetFltype;
    property OutputName : String read Flname write Flname;
    property ItemShow : FileListItemTypes read Floption.Item2Show  write Floption.Item2Show ;
    property Options : FileListOptionsType read Floption write Floption;
    property CakDir2 : TCakDir2 read CakDir write CakDir;
    property Seperator : char read Sepr write Sepr;
  end;

procedure Register;

implementation
uses CakUtils, Graphics;

procedure TCakPrinter.Print;
begin
        If Assigned(Cakdir2) then
        if Cakdir2.ArchiveName <> '' then
        begin
        Case OutputType of
        _Txt : WriteTxt;
        _Csv : begin Seperator := ','; WriteCsv; end;
        _RawTxt : begin Seperator := ' '; WriteCsv; end;
        _Htm : WriteHtm;
        end;
        Cakdir2.FOnMsg(nil,0,0,msg_ok,'File list printed - ' + OutputName);
        end;
end;

procedure TCakPrinter.WriteHtm;
var tf : textfile;
    i,y,l : integer;
begin
      AssignFile(tf, OutputName);
        
      Rewrite(tf);
      Writeln(tf, '<html>' + #10 + '<head> ');
      Writeln(tf, '<meta name=GENERATOR content=Common Archiver Kit ' + CAKVER + '>');
      Writeln(tf, '<title> Archive Contents </title>');
      Writeln(tf, Format('<body bgcolor="#%s" text="#%s">',
        [TColor2WebColor(Floption.BackgroundColor),
        TColor2WebColor(Floption.NormalFontColor)]));
      Write(tf, '<H5>Content of archive: <a href="');
      Write(tf, Cakdir2.ArchiveName + '">');
      Write(tf, Cakdir2.ArchiveName + '</a> ');
      Writeln(tf, 'total ' + IntToStr(Cakdir2.Total_Contents) + ' files.');
      Writeln(tf, '<HR SIZE=3>');

        if Floption.UseBorder then
          Writeln(tf, '<TABLE BORDER=1 cellpadding=1 cellspacing=1>')
        else
          Writeln(tf, '<TABLE BORDER=0 cellpadding=1 cellspacing=1>');

        y := 0;
        for l := 1 to TotalColumns do
         begin
          //write(tf,'<TD>');
          Writeln(tf, Format('<TD bgcolor="#%s" align = left>',
            [TColor2WebColor(Floption.HeaderBackground)]));

          Write(tf, Format('<B><font color="#%s">',
            [TColor2WebColor(Floption.HeaderFontColor)]));
          case l of
            1: if fl_fname in Floption.Item2Show then
               begin 
                Write(tf, Columns[l]); 
                Inc(y); 
               end;
            2: if fl_ftype in Floption.Item2Show then
               begin 
                Write(tf, Columns[l]); 
                Inc(y); 
               end;
            3: if fl_fsize in Floption.Item2Show then
               begin 
                Write(tf, Columns[l]); 
                Inc(y); 
               end;
            4: if fl_fdate in Floption.Item2Show then
               begin 
                Write(tf, Columns[l]); 
                Inc(y); 
               end;
            5: if fl_fpsize in Floption.Item2Show then
               begin 
                Write(tf, Columns[l]); 
                Inc(y); 
               end;
            6: if fl_fratio in Floption.Item2Show then
               begin 
                Write(tf, Columns[l]); 
                Inc(y); 
               end;
            7: if fl_fcrc in Floption.Item2Show then
               begin 
                Write(tf, Columns[l]); 
                Inc(y); 
               end;
            8: if fl_fdefpath in Floption.Item2Show then
               begin 
                Write(tf, Columns[l]); 
                Inc(y); 
               end;
           end;
          if y = 1 then
          Writeln(tf, '</TD></B></font>') else
          writeln(tf,'<TD></B></font>');
         end;

        with Cakdir2 do
        for i := 0 to Total_Contents - 1 do
         begin
          y := 0;
          with Archive_Contents[i] do
           begin
            Write(tf, '<TR>');
                        
            for l := 1 to TotalColumns do
             begin
              //write(tf,'<TD>');
              Writeln(tf, Format('<TD bgcolor="#%s" align = left>',
                [TColor2WebColor(Floption.FListBackground)]));

              if l = 1 then
                if (FlType = _Htm2) then
                  Write(tf, Format('<img src="icons/I%s.jpg" border="0">',
                    [ExtractFileExt(_FileName)]));

              case l of
                1: if fl_fname in Floption.Item2Show then
                   begin 
                    Write(tf, ' ' + _FileName); 
                    Inc(y); 
                   end;
                2: if fl_ftype in Floption.Item2Show then
                   begin 
                    Write(tf, _Filetype); 
                    Inc(y); 
                   end;
                3: if fl_fsize in Floption.Item2Show then
                   begin
                    if Floption.SizeInK then
                      Write(tf, SizeInK(_FileSize)) 
                    else
                      Write(tf, IntToStr(_FileSize));
                    Inc(y); 
                   end;
                4: if fl_fdate in Floption.Item2Show then
                   begin 
                    Write(tf, DateTimeToStr(_FileTime)); 
                    Inc(y); 
                   end;
                5: if fl_fpsize in Floption.Item2Show then
                   begin 
                    Write(tf, SizeInK(_FilePackedSize)); 
                    Inc(y); 
                   end;
                6: if fl_fratio in Floption.Item2Show then
                   begin 
                    Write(tf, IntToStr(_FileRatio)); 
                    Inc(y); 
                   end;
                7: if fl_fcrc in Floption.Item2Show then
                   begin 
                    Write(tf, _FileCRC); 
                    Inc(y); 
                   end;
                8: if fl_fdefpath in Floption.Item2Show then
                   begin 
                    Write(tf, _FileDefPath); 
                    Inc(y); 
                   end;
               end;
              if y = 1 then
              Writeln(tf, '</TD>') else
              writeln(tf,'<TD>');
             end;
            Writeln(tf);
           end;
         end;
        Writeln(tf, '<TD>');
        Writeln(tf, '<TD>');
        Writeln(tf, '</TABLE>');
        Writeln(tf, '<HR SIZE=3>');
      Writeln(tf, '</HTML>');
      CloseFile(tf);
end;

procedure TCakPrinter.WriteTxt;
var tf : textfile;
    Counter, i, j, l : integer;
    ColWidth:    array[1..TotalColumns] of Integer;
    ColType:     array[1..TotalColumns] of FileListItemType;

procedure writesp(Text: string; total: Integer);
  var
    text2write: String;
    i:          Integer;
begin
    text2write := Text;
    for i := Length(Text) to total do
      text2write := text2write + ' ';
    Write(tf, text2write);
end;

begin
        Counter := 0;
        for l := 1 to TotalColumns do
        case l of
          1: if fl_fname in Floption.Item2Show then
             begin
              Inc(Counter);
              ColWidth[Counter] := maxlength(fl_fname);
              ColType[Counter]  := fl_fname;
             end;
          2: if fl_ftype in Floption.Item2Show then
             begin
              Inc(Counter);
              ColWidth[Counter] := maxlength(fl_ftype);
              ColType[Counter]  := fl_ftype;
             end;
          3: if fl_fsize in Floption.Item2Show then
             begin 
              Inc(Counter);
              ColWidth[Counter] := maxlength(fl_fsize);
              ColType[Counter]  := fl_fsize;
             end;
          4: if fl_fdate in Floption.Item2Show then
             begin 
              Inc(Counter);
              ColWidth[Counter] := maxlength(fl_fdate);
              ColType[Counter]  := fl_fdate;
             end;
          5: if fl_fpsize in Floption.Item2Show then
             begin 
              Inc(Counter);
              ColWidth[Counter] := maxlength(fl_fpsize);
              ColType[Counter]  := fl_fpsize;
             end;
          6: if fl_fratio in Floption.Item2Show then
             begin 
              Inc(Counter);
              ColWidth[Counter] := maxlength(fl_fratio);
              ColType[Counter]  := fl_fratio;
             end;
          7: if fl_fcrc in Floption.Item2Show then
             begin 
              Inc(Counter);
              ColWidth[Counter] := maxlength(fl_fcrc);
              ColType[Counter]  := fl_fcrc;
             end;
          8: if fl_fdefpath in Floption.Item2Show then
             begin 
              Inc(Counter);
              ColWidth[Counter] := maxlength(fl_fdefpath);
              ColType[Counter]  := fl_fdefpath;
             end;
         end;
      AssignFile(tf, Flname);
      Rewrite(tf);
      Writeln(tf, '-------------------------------------------');
      Writeln(tf, 'Archive List - ' + ExtractFileName(Cakdir2.ArchiveName));
      Writeln(tf, 'Compile Time - ' + DateTimeToStr(Now));   
      Writeln(tf, 'Created by ' + PRODUCT);
      Writeln(tf, '-------------------------------------------');
      Writeln(tf);
      Writeln(tf);
      for i := 1 to Counter do
       begin
        case ColType[i] of
          fl_fname: Write(tf, Columns[1]: ColWidth[i]);
          fl_ftype: Write(tf, Columns[2]: ColWidth[i]);
          fl_fsize: Write(tf, Columns[3]: ColWidth[i]);
          fl_fdate: Write(tf, Columns[4]: ColWidth[i]);
          fl_fpsize: Write(tf, Columns[5]: ColWidth[i]);
          fl_fratio: Write(tf, Columns[6]: ColWidth[i]);
          fl_fcrc: Write(tf, Columns[7]: ColWidth[i]);
          fl_fdefpath: Write(tf, Columns[8]: ColWidth[i]);
         end;
        Write(tf, ' ');
       end;
      Writeln(tf);
      for i := 1 to Counter do
       begin
        for j := 1 to ColWidth[i] do
          Write(tf, '-');
        Write(tf, ' ');
       end;
      Writeln(tf);

      with Cakdir2 do
      for i := 0 to Total_Contents - 1 do
       begin
        for j := 1 to Counter do
         begin
          with Archive_Contents[i] do
            case ColType[j] of
              fl_fname: writesp(_FileName, ColWidth[j]);
              fl_ftype: writesp(_Filetype, ColWidth[j]);
              fl_fsize: if Floption.SizeInK then
                  writesp(SizeInK(_FileSize), ColWidth[j]) 
                else
                  writesp(IntToStr(_FileSize), ColWidth[j]);
              fl_fdate: writesp(DateTimeToStr(_FileTime), ColWidth[j]);
              fl_fpsize: if Floption.SizeInK then
                  writesp(SizeInK(_FilePackedSize), ColWidth[j]) 
                else
                  writesp(IntToStr(_FilePackedSize), ColWidth[j]);
              fl_fratio: writesp(IntToStr(_FileRatio) + '%', ColWidth[j]);
              fl_fcrc: writesp(_FileCRC, ColWidth[j]);
              fl_fdefpath: writesp(_FileDefPath, ColWidth[j]);
             end;
          Write(tf, ' ');
         end;
        Writeln(tf);
       end;

      CloseFile(tf);
end;

function TCakPrinter.Maxlength(coltocheck: FileListItemType): Integer;
  var
    i, Size: Integer;
   begin
    Size := 0;
    with Cakdir2 do
    case coltocheck of
      fl_fname: for i := 0 to Total_Contents - 1 do
          if Size < Length(Archive_Contents[i]._FileName) then
            Size := Length(Archive_Contents[i]._FileName);
      fl_fdefpath: for i := 0 to Total_Contents - 1 do
          if Size < Length(Archive_Contents[i]._FileDefPath) then
            Size := Length(Archive_Contents[i]._FileDefPath);
      fl_ftype: for i := 0 to Total_Contents - 1 do
          if Size < Length(Archive_Contents[i]._filetype) then
            Size := Length(Archive_Contents[i]._filetype);
      fl_fsize: for i := 0 to Total_Contents - 1 do
          if Floption.SizeInK then
           begin
            if Size < Length(SizeInK(Archive_Contents[i]._FileSize)) then
              Size := Length(SizeInK(Archive_Contents[i]._FileSize));
           end
        else
         begin
          if Size < Length(IntToStr(Archive_Contents[i]._FileSize)) then
            Size := Length(IntToStr(Archive_Contents[i]._FileSize));
         end;
      fl_fpsize: for i := 0 to Total_Contents - 1 do
          if Floption.SizeInK then
           begin
            if Size < Length(SizeInK(Archive_Contents[i]._FilePackedSize)) then
              Size := Length(SizeInK(Archive_Contents[i]._FilePackedSize));
           end
        else
         begin
          if Size < Length(IntToStr(Archive_Contents[i]._FileSize)) then
            Size := Length(IntToStr(Archive_Contents[i]._FileSize));
         end;

      fl_fratio: Size := 4;
      fl_fdate: Size  := Length(DateTimeToStr(Now));
      fl_fcrc: Size   := 8;
     end;
    Result := Size + 2;
   end;

procedure TCakPrinter.WriteCsv;
var tf : textfile;
    i : integer;
    k : string;
begin
      AssignFile(tf, Flname);
      Rewrite(tf);
      With CakDir2 do
      for i := 0 to Total_Contents - 1 do
          with Archive_Contents[i] do
           begin
            k := '"' + _FileName + '"' + Sepr;
            k := k + _Filetype + Sepr;
            k := k + IntToStr(_FileSize) + Sepr;
            k := k + DateTimeToStr(_FileTime) + Sepr;
            k := k + IntToStr(_FilePackedSize) + Sepr;
            k := k + IntToStr(_FileRatio) + '%'+ Sepr;
            k := k + _FileCRC + Sepr;
            k := k + '"' + _FileDefPath + '"';
            Writeln(tf, k);
           end;
      CloseFile(tf);
end;

procedure TCakPrinter.SetFltype(ftype : FileListType);
begin
        Case ftype of
        _RawTxt, _Txt : Changefileext(Outputname,'.Txt');
        _Csv : Changefileext(Outputname,'.Txt');
        _Htm : Changefileext(Outputname,'.Htm');
        end;
        fltype := ftype;
end;


constructor TCakPrinter.Create(AOwner: TComponent);
begin
        inherited Create(AOwner);
        floption.BackgroundColor := clWhite;
        floption.HeaderFontColor := clBlue;
        floption.HeaderBackground := clYellow;
        floption.FListBackground := clWhite;
        floption.NormalFontColor := clBlack;
        floption.SizeInK := true;
        floption.UseBorder := false;
end;

procedure Register;
begin
  RegisterComponents('CAKE', [TCakPrinter]);
end;

end.
