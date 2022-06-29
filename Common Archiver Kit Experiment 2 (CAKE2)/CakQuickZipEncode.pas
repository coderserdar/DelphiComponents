unit CakQuickZipEncode;

interface

uses
  Windows, Messages, SysUtils, Classes, CakDefs2, CakArchiver,
  LbCipher, LbClass,LbUtils,LbProc;

const CanExtract = TRUE;
      CanAdd = TRUE;
      CanList = TRUE;
      CanSFX = TRUE;
      Dllname = 'dummydll.dll';
      SfxStub = 'Decrypt.bin';

type
  encrypttype = (eUnknown,eBlowfish,eDes,e3Des,eRijndael);
  encryptheaderinfo = record
                    fileencrypttype : encrypttype;
                    filename : string;
                    sha1 : string;
                    size : integer;
                  end;
  TCakQuickZipEncode = class(TCakArchiver)
  private
    { Private declarations }
    Stopping : boolean;

  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure SetCakdir(pCakdir : TComponent); override;
    procedure Process(dowhat : WorkType); override;
    function Cando(aWork: WorkType): Boolean; override;
    function Def_TreatAs : string; override;
    procedure Load_DLL; override;
    procedure UnLoad_DLL; override;
    function DllExists : Boolean; override;
    procedure DoStop(Stopp: Boolean); override;
  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
  end;

procedure Register;
{function readheaderfilename(filename : string) : string;
 function decryptfile(workfile, outputfile,password : string) : boolean;
 procedure encryptfile(workfile, outputfile,password : string; enctype : encrypttype);}

implementation
uses CakUtils2, Cakdir2, Dialogs;
var Cakdir : TCakdir2;

function writeencryptheader(info : encryptheaderinfo) : string;
var k : string;
begin
    k := '';
    Case info.fileencrypttype of
    eBlowfish : k := k + 'Blowfish Encrypted File' + #13#10;
    eDes : k := k + 'DES Encrypted File' + #13#10;
    e3Des : k := k + '3DES Encrypted File' + #13#10;
    eRijndael : k := k + 'Rijndael Encrypted File' + #13#10;
    end;
    k := k + 'File: ' + info.filename + #13#10;
    k := k + 'SHA-1: ' + info.sha1 + #13#10;
    result := k+#13#10;
end;


function readencryptheader(filename : string) : encryptheaderinfo;
var info : encryptheaderinfo;
var  i : integer;
     k,k1,k2,k3 : string;
     tf : textfile;

begin
  assignfile(tf,filename);
  Reset(tf);
  Readln(tf,k1);
  Readln(tf,k2);
  Readln(tf,k3);

  Closefile(tf);
  i := pos(' ',k1);
  k := copy(k1,0,i-1);
  info.fileencrypttype := eUnknown;
  
  if lowercase(k) = 'blowfish' then
        info.fileencrypttype := eBlowfish else
  if lowercase(k) = 'des' then
        info.fileencrypttype := eDes;
  if lowercase(k) = '3des' then
        info.fileencrypttype := e3Des;
  if lowercase(k) = 'rijndael' then
        info.fileencrypttype := eRijndael;
  
  i := pos(' ',k2);
  info.filename := copy(k2,i+1,length(k2)-i);
  i := pos(' ',k3);
  info.sha1 := copy(k3,i+1,length(k3)-i);
  info.size := length(k1+k2+k3)+8;
  result := info;
end;

procedure encryptfile(workfile, outputfile,password : string; enctype : encrypttype);
var
  InputStream,OutputStream : TFileStream;
  Key128           : TKey128;
  Key64           : TKey64;
  info : encryptheaderinfo;
  SHA1Digest : TSHA1Digest;
  k : string;
  LbSHA11 : TLbSHA1;
  LbBlowfish1 : TLbBlowfish;
  LbDes1 : TLbDes;
  Lb3Des1 : TLb3Des;
  LbRijndael1 : TLbRijndael;

begin
  lbsha11 := Tlbsha1.Create(nil);

  lbBlowfish1 := tlbblowfish.Create(nil);
  lbDes1 := Tlbdes.Create(nil);
  lb3des1 := Tlb3des.Create(nil);
  LbRijndael1 := TLbRijndael.Create(nil);

  lbBlowfish1.CipherMode := cmCBC;
  lbDes1.CipherMode := cmCBC;
  lb3Des1.CipherMode := cmCBC;
  LbRijndael1.CipherMode := cmCBC;

  lbSha11.HashFile(workfile);
  lbSha11.GetDigest(SHA1Digest);

  InputStream := TFileStream.Create(workfile,fmOpenRead);
  Outputstream := TFileStream.Create(outputfile,fmCreate);
  info.sha1 := BufferToHex(SHA1Digest, SizeOf(SHA1Digest));
  info.fileencrypttype := enctype;
  info.filename := Extractfilename(workfile);
  k := writeencryptheader(info);

  Outputstream.Write(k[1],length(k));
  GenerateLMDKey(Key128, SizeOf(Key128), password);
  GenerateLMDKey(Key64, SizeOf(Key64), password);
  try
  Case info.fileencrypttype of
  eBlowfish : begin
                 LbBlowfish1.SetKey(Key128);
                 LbBlowfish1.EncryptStream(InputStream,Outputstream);
              end;
  eDES      : begin
                 LbDes1.SetKey(Key64);
                 LbDes1.EncryptStream(InputStream,Outputstream);
              end;
  e3DES      : begin
                 Lb3Des1.SetKey(Key128);
                 Lb3Des1.EncryptStream(InputStream,Outputstream);
              end;
  eRijndael: begin
                 LbRijndael1.SetKey(Key128);
                 LbRijndael1.EncryptStream(InputStream,Outputstream);
              end;
  end;
  finally
  InputStream.Free;
  OutputStream.Free;
  //showmessage('OK');
  end;

  LbSHA11.free;
  LbBlowfish1.free;
  LbDES1.free;
  Lb3Des1.free;
  LbRijndael1.free;
end;



function decryptfile(workfile, outputfile,password : string) : boolean;
var
  InputStream,OutputStream : TFileStream;
  ProcessStream : TMemoryStream;
  Key128  : TKey128;
  Key64 : TKey64;
  SHA1Digest : TSHA1Digest;
  info : encryptheaderinfo;
  LbSHA11 : TLbSHA1;
  LbBlowfish1 : TLbBlowfish;
  LbDes1 : TLbDes;
  Lb3Des1 : TLb3Des;
  LbRijndael1 : TLbRijndael;

begin
  lbsha11 := Tlbsha1.Create(nil);

  lbBlowfish1 := tlbblowfish.Create(nil);
  lbDes1 := Tlbdes.Create(nil);
  lb3des1 := Tlb3des.Create(nil);
  LbRijndael1 := TLbRijndael.Create(nil);

  lbBlowfish1.CipherMode := cmCBC;
  lbDes1.CipherMode := cmCBC;
  lb3Des1.CipherMode := cmCBC;
  LbRijndael1.CipherMode := cmCBC;
  
  info := readencryptheader(workfile);

  InputStream := TFileStream.Create(workfile,fmOpenRead or fmShareDenyWrite);
  Outputstream := TFileStream.Create(outputfile,fmCreate);
  ProcessStream := TMemoryStream.Create;
  try
  InputStream.Seek(info.size,soFromBeginning );
  ProcessStream.CopyFrom(InputStream, InputStream.Size - info.size);
  ProcessStream.Position := 0;
  GenerateLMDKey(Key128, SizeOf(Key128), password);
  GenerateLMDKey(Key64, SizeOf(Key64), password);
  Case info.fileencrypttype of
  eBlowfish : begin
                LbBlowfish1.SetKey(Key128);
                LbBlowfish1.DecryptStream(ProcessStream,Outputstream);
              end;
  eDes      : begin
                LbDes1.SetKey(Key64);
                LbDes1.DecryptStream(ProcessStream,Outputstream);
              end;
  e3Des     : begin
                Lb3Des1.SetKey(Key128);
                Lb3Des1.DecryptStream(ProcessStream,Outputstream);
              end;

  eRijndael: begin
                LbRijndael1.SetKey(Key128);
                LbRijndael1.DecryptStream(ProcessStream,Outputstream);
              end;
  end;
  finally
  InputStream.Free;
  OutputStream.Free;
  ProcessStream.Free;
  end;

  lbSha11.HashFile(outputfile);
  lbSha11.GetDigest(SHA1Digest);
  Result := (info.sha1 = BufferToHex(SHA1Digest, SizeOf(SHA1Digest)));
  LbSHA11.free;
  LbBlowfish1.free;
  LbDes1.free;
  Lb3Des1.free;
  LbRijndael1.free;
end;

function readheaderfilename(filename : string) : string;
var info : encryptheaderinfo;
begin
   info := readencryptheader(filename);
   result := info.filename;
end;

procedure TCakQuickZipEncode.SetCakdir(pCakdir : TComponent);
begin
    Cakdir := TCakdir2(pCakdir);
end;

procedure TCakQuickZipEncode.DoStop(Stopp: Boolean);
begin
end;

procedure TCakQuickZipEncode.Process(dowhat : WorkType);
var k : string;
    etype : encrypttype;
begin
    Load_DLL;
    with (Cakdir as TCakdir2) do
    begin
    case doWhat of
    wtExtract, wtAdd:
       if Password = '' then
        if Assigned(FOnPwd) then
          FOnPwd(Self,ArchiveName,'',Password);
    end;

    case doWhat of
    wtLoadContents: if fileexists(ArchiveName) then
     begin
      k := readHeaderfilename(archivename);
      AddContents(Archivename,k,'',k,getfilesize(archivename),getfilesize(archivename),true,now,'000000');
     end;
    wtExtract: if fileexists(ArchiveName) then
     begin
      k := readHeaderfilename(archivename);
      if not decryptfile(archivename,Appendslash(Extractoptions.Extr_to)+k,Password) then
        if Assigned(FOnMsg) then
          FOnMsg(Self,CODE_PASSWORD,CODE_PASSWORD,Msg_Error,ERR_PASSWORD);
     end;
    wtAdd:
     begin
      //if addoptions.Add_Files.Count > 0 then
      //encryptfile(addoptions.Add_Files.Strings[0],archivename,password,eBlowFish);
     end;
    wtSFX:
     begin
      if fileexists(GrabProgramPath + sfxstub) then
          Combine(GrabProgramPath + sfxstub,Archivename,'',Changefileext(Archivename,'.exe'));
     end;
    wtEncrypt:
     begin
      k := Changefileext(Cakdir.EncryptOptions.File2Encrypt,'.qze');
      Case EncryptOptions.EncryptMethod of
        qeBlowfish : etype := eBlowfish;
        qeDes      : etype := eDes;
        qe3Des     : etype := e3Des;
        qeRijndael : etype := eRijndael;
      end;

      encryptfile(Cakdir.EncryptOptions.File2Encrypt,k,Cakdir.Password,etype);
        if Assigned(FOnMsg) then
          FOnMsg(Self,CODE_NOERROR,CODE_NOERROR,Msg_Error,ERR_NOERROR);
     end; 
    end;
    Password := '';
    end;
end;

function TCakQuickZipEncode.DllExists : boolean;
begin
        Result := Fileexists(GrabProgrampath+Dllname);
end;

function TCakQuickZipEncode.Cando(aWork: WorkType): Boolean;
begin
        Case aWork of
        wtNone : Result := true;
        wtTest, wtExtract : Result := CanExtract;
        wtAdd, wtDelete : Result := CanAdd;
        wtSFX :  Result := CanSFX;
        wtLoadContents: Result := CanList;
        wtEncrypt : Result := true;
        else Result := false;
        end;
end;

function TCakQuickZipEncode.Def_TreatAs : string;
begin
end;

procedure TCakQuickZipEncode.Load_DLL;
begin
end;

procedure TCakQuickZipEncode.UnLoad_DLL;
begin
end;

procedure Register;
begin
  //RegisterComponents('QZip', [TCakQuickZipEncode]);
end;

end.
 