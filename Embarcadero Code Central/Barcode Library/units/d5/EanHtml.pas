unit EanHtml;

interface

procedure CreateHtml_AllCodes(Root:Boolean; Dir:String);

implementation
uses Classes, SysUtils, EanSpecs, EanKod;

const Html_Template='<HTML><HEAD>%s<STYLE TYPE="text/css">@import url(../styles.css);</STYLE></HEAD><BODY>%s</BODY></HTML>';

function YesNo(B:Boolean):String;
begin
     if B then Result :='Yes'
     else      Result :='No';
end;

procedure CreateHtml_AllCodes(Root:Boolean; Dir:String);
const anchor='<A HREF="%s" alt="%s">%s</A><BR>';
      table_row='<TR><TD WIDTH="30%%">%s</TD><TD WIDTH="70%%">%s</TD></TR>';
      img_tag  ='<img src="%s" width=%d height=%d border=0 vspace=20>';
var i:Integer;
    LI:TStringList;
    BI:TBarCodeInfo;
    T :TTypBarcode;
    s,header,pom :String;
    ext,fn      :String;
    E        :TEan;
begin
   LI:=TStringList.Create;
   try
     E:=TEan.Create(nil);
     try
       s   := ExtractFileDir(dir);
       ext := GrExtensions[gtJPeg];

       if Root then begin
          for T:=Low(TTypBarcode) to High(TTypBarcode) do begin
              BI:=BarcodeInfo(T);
              LI.Add(format(anchor,['barcode_'+IntToStr(Integer(T))+'.html',BI.LongName,BI.LongName]));
          end;
          LI.Text := Format(html_template,['',LI.Text]);
          LI.saveToFile(Dir);
       end;
       for T:=Low(TTypBarcode) to High(TTypBarcode) do begin
          E.TypBarCode := T;
          E.Height     := 100;
          E.Width      := 2*E.MinWidth;
          E.SaveToFile(Lowercase(s+'\'+E.BarcodeTypeName+ext));

          LI.Clear;
          BI:=BarcodeInfo(T);
          LI.Add('<TABLE WIDTH="100%">');
          LI.Add('<TH COLSPAN=2>'+BI.LongName+'</TH>');
          LI.Add(Format(table_row,['Barcode type',             BI.Name                         ]));
          LI.Add(Format(table_row,['Barcode family',           BI.LongName                     ]));
          LI.Add(Format(table_row,['Number of available char', IntToStr(Length(BI.Chars))      ]));
          LI.Add(Format(table_row,['Set of chars',             BI.Chars                        ]));
          LI.Add(Format(table_row,['Year',                     IntToStr(BI.Year)               ]));
          LI.Add(Format(table_row,['Country',                  BI.Country                      ]));
          LI.Add(Format(table_row,['Supplement digits',        YesNo(BI.EnabledAdd)            ]));
          LI.Add(Format(table_row,['Optimal char width in mm', FloatToStr(BI.OptCharWidthMM)+' mm']));
          LI.Add(Format(table_row,[
                   Format(img_tag,[lowercase(E.BarcodeTypeName+ext), E.Width, E.Height]), '']));

          if BI.EnabledAdd then begin
                pom:=E.barcode;
                fn :=lowercase(E.BarcodeTypeName+'_2'+ext);
                E.Barcode := pom+' 12';
                E.Width   := 2*E.MinWidth;
                E.SaveToFile(Lowercase(s+'\'+fn));
                LI.Add(Format(table_row,[
                   Format(img_tag,[lowercase(fn), E.Width, E.Height]), '']));

                fn:=lowercase(E.BarcodeTypeName+'_5'+ext);
                E.Barcode := pom+' 12345';
                E.Width   := 2*E.MinWidth;
                E.SaveToFile(Lowercase(s+'\'+fn));
                LI.Add(Format(table_row,[
                   Format(img_tag,[lowercase(fn), E.Width, E.Height]), '']));
            end;

          LI.Add('</TABLE>');

          Header := '<META NAME=DESCRIPTION CONTENT="'+BI.LongName+'">'
                  + '<META NAME=KEYWORDS CONTENT="barcode,delphi,component,print,vcl,'+BI.LongName+'">'
                  + '<TITLE>PSOFT Barcode library : '+BI.LongName+'</TITLE>';

          LI.Text := Format(html_template,[header,LI.Text]);
          LI.saveToFile(s+'\barcode_'+IntToStr(Integer(T))+'.html');
       end;
     finally
       E.Free;
     end;
   finally
       LI.Free;
   end;
end;

end.
