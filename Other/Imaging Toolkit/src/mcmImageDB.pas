// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  22347: mcmImageDB.pas 
//
//    Rev 1.7    2014-02-02 21:09:58  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.6    01-04-2006 14:21:42  mcm
// Added validation of FDataLink.DataSet.
//
//   Rev 1.5    20-03-2005 18:24:08  mcm    Version: IMG 2.9
// Corrected an faulty loop where a record contained an empty image field.

//
//   Rev 1.4    14-01-2005 19:50:44  mcm    Version: IMG 2.7
// Correction to Paint method and replicatable mode.

//
//   Rev 1.3    07-01-2005 15:28:04  mcm
// Added override Paint method, and modified control to be replicatable, that is
// require for use with TDBCtrlGrid.

//
//   Rev 1.2    05-01-2005 12:14:28  mcm
// Changed BlobStream from TBlobStream to a TStream. This solution should fix
// "Invalid Typecat" errors in methods UpdateData and LoadPicture.

//
//   Rev 1.1    30-01-2004 20:14:40  mcm    Version: IMG 2.3
// Modified to support images stored by Borland's TDBImage and auto-detection of
// file formats.

//
//   Rev 1.0    24-11-2003 20:29:56  mcm
// Initial edition.

unit mcmImageDB;

{$Include 'mcmDefines.pas'}

{$IFOPT R+}{$DEFINE RANGE_OFF}{$R-}{$ENDIF}

interface

uses {$IFNDEF GE_DXE2}
      Classes, Controls, SysUtils, Forms, Graphics, Messages, Windows,
      dbctrls, Db, DbTables,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms,
      Vcl.DBCtrls, Data.Db, Bde.DbTables,
     {$ENDIF}
     mcmImageTypeDef,
     mcmImage;

type
  TmcmImageDB = class(TmcmImageCtrl)
  private
    // Private declarations
    FError            : TmcmErrorCode;
    FDataLink         : TFieldDataLink;
    FAutoDisplay      : boolean;
    FPictureLoaded    : Boolean;
    FAutoEdit         : boolean;
    FAutoFormat       : boolean;
    FAddGraphicHeader : boolean;
    FFileFormat       : TmcmFileFormat;
  protected
    // Protected declarations
    procedure   Changed(Sender : TObject); override;
    procedure   DataChange(Sender : TObject);
    procedure   EditingChange(Sender : TObject);
    function    GetCompression : TmcmCompress;
    function    GetDataField : string;
    function    GetDataSource : TDataSource;
    function    GetField : TField;
    function    GetQuality : word;
    function    GetReadOnly : boolean;
    procedure   PictureChanged(Sender : TObject);
    procedure   Notification(AComponent : TComponent; Operation : TOperation); override;
    procedure   SetAddGraphicHeader(Value : boolean);
    procedure   SetAutoDisplay(Value : boolean);
    procedure   SetAutoEdit(Value : boolean);
    procedure   SetAutoFormat(Value : boolean);
    procedure   SetCompression(Value : TmcmCompress);
    procedure   SetDataField(const Value : string);
    procedure   SetDataSource(Value : TDataSource);
    procedure   SetFileFormat(Value : TmcmFileFormat);
    procedure   SetQuality(Value : word);
    procedure   SetReadOnly(Value : boolean);
    procedure   UpdateData(Sender : TObject);
    procedure   Paint; override;
  public
    // Public declarations
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   LoadPicture;

    property    Field : TField
      read      GetField;
  published
    // Published declarations
    property    AddGraphicHeader : boolean
      read      FAddGraphicHeader
      write     SetAddGraphicHeader default False;
    property    Align;
    property    AutoDisplay : boolean
      read      FAutoDisplay
      write     SetAutoDisplay default True;
    property    AutoEdit : boolean
      read      FAutoEdit
      write     SetAutoEdit default False;
    property    AutoFormat : boolean
      read      FAutoFormat
      write     SetAutoFormat default True;
    property    Color;
    property    Compression  : TmcmCompress
      read      GetCompression
      write     SetCompression;
    property    DataField : string
      read      GetDataField
      write     SetDataField;
    property    DataSource : TDataSource
      read      GetDataSource
      write     SetDataSource;
    property    DragCursor;
    property    DragMode;
    property    Enabled;
    property    FileFormat : TmcmFileFormat
      read      FFileFormat
      write     SetFileFormat default FF_BMP;
    property    Font;
    property    ParentColor default False;
    property    ParentFont;
    property    ParentShowHint;
    property    PopupMenu;
    property    Quality : word
      read      GetQuality
      write     SetQuality;
    property    ReadOnly : boolean
      read      GetReadOnly
      write     SetReadOnly default False;
    property    ShowHint;

//    property    TabOrder;
//    property    TabStop default True;
    property    Visible;
    property    OnClick;
    property    OnDblClick;
    property    OnDragDrop;
    property    OnDragOver;
    property    OnEndDrag;
//    property    OnEnter;
//    property    OnExit;
//    property    OnKeyDown;
//    property    OnKeyPress;
//    property    OnKeyUp;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnStartDrag;
  end;

implementation

// Paradox graphic BLOB header.
type
  TGraphicHeader = record
  Count : Word;    // Fixed at 1.
  HType : Word;    // Fixed at $0100.
  Size  : Longint; // Size not including header.
  end;

constructor TmcmImageDB.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FPictureLoaded := False;
  FAutoDisplay := True;
  FAutoEdit := False;
  FAutoFormat := True; 
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;

  FAddGraphicHeader := False;
  FFileFormat := FF_BMP;
end; // TmcmImageDB.Create.


destructor TmcmImageDB.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  Inherited Destroy;
end; // TmcmImageDB.Destroy.


procedure TmcmImageDB.Changed(Sender : TObject);
begin
  FFileFormat := Image.ImageInfo.FileFormat;
  if Not(csReadingState in ControlState)
  then begin
       FFileFormat := Image.ImageInfo.FileFormat;
       if FAutoEdit
       then FDataLink.Edit;
       PictureChanged(Sender);
  end;
  Inherited Changed(Sender);
end; // TmcmImageDB.Changed.


procedure TmcmImageDB.DataChange(Sender : TObject);
begin
  if Not(Assigned(FDataLink.DataSet))
  then Exit;
  case FDataLink.DataSet.State of
  dsBrowse : begin
               Image.Clear;
               FPictureLoaded := False;
               if FAutoDisplay
               then LoadPicture;
             end;
  dsInsert : begin
               Image.Clear;
               DrawImage;
             end;
  end;
end; // TmcmImageDB.DataChange.


procedure TmcmImageDB.EditingChange(Sender : TObject);
begin
 // ReadOnly := not FDataLink.Editing;
end; // TmcmImageDB.EditingChange.


function TmcmImageDB.GetDataSource : TDataSource;
begin
  Result := FDataLink.DataSource;
end; // TmcmImageDB.GetDataSource.


procedure TmcmImageDB.SetDataSource(Value : TDataSource);
begin
  FDataLink.DataSource := Value;
  if (Value <> Nil)
  then Value.FreeNotification(Self);
end; // TmcmImageDB.SetDataSource.


function TmcmImageDB.GetField : TField;
begin
  Result := FDataLink.Field;
end; // TmcmImageDB.GetField.


function TmcmImageDB.GetDataField : string;
begin
  Result := FDataLink.FieldName;
end; // TmcmImageDB.GetDataField.


procedure TmcmImageDB.SetDataField(const Value : string);
begin
  FDataLink.FieldName := Value;
end; // TmcmImageDB.SetDataField.


function TmcmImageDB.GetReadOnly : boolean;
begin
  Result := FDataLink.ReadOnly
end; // TmcmImageDB.GetReadOnly.


procedure TmcmImageDB.SetReadOnly(Value : boolean);
begin
  FDataLink.ReadOnly := Value;
end; // TmcmImageDB.SetReadOnly.


procedure TmcmImageDB.PictureChanged(Sender : TObject);
begin
  FPictureLoaded := True;
  FDataLink.Modified;
end; // TmcmImageDB.PictureChanged.


procedure TmcmImageDB.Notification(AComponent : TComponent; Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and
     (FDataLink <> Nil) and
     (AComponent = DataSource)
  then DataSource := Nil;
end; // TmcmImageDB.Notification.


procedure TmcmImageDB.SetAutoDisplay(Value : boolean);
begin
  if (FAutoDisplay <> Value)
  then begin
       FAutoDisplay := Value;
       if Value
       then LoadPicture;
  end;
end; // TmcmImageDB.SetAutoDisplay.


procedure TmcmImageDB.SetAutoEdit(Value : boolean);
begin
  if (FAutoEdit <> Value)
  then begin
       FAutoEdit := Value;
  end;
end; // TmcmImageDB.SetAutoEdit.


procedure TmcmImageDB.SetAddGraphicHeader(Value : boolean);
begin
  if (FAddGraphicHeader <> Value)
  then begin
       FAddGraphicHeader := Value;
       if FAddGraphicHeader
       then FAutoFormat := True;
  end;
end; // TmcmImageDB.SetAddGraphicHeader.


procedure TmcmImageDB.SetAutoFormat(Value : boolean);
begin
  if (FAutoFormat <> Value)
  then begin
       FAutoFormat := Value;
       if Not(FAutoFormat)
       then FAddGraphicHeader := False;
  end;
end; // TmcmImageDB.SetAutoFormat.


procedure TmcmImageDB.UpdateData(Sender : TObject);
var BlobStream    : TStream; // TBlobStream; Should solve "Invalid Typecast"'s when updating image fields.
    MemStream     : TMemoryStream;
    i             : integer;
    GraphicHeader : TGraphicHeader;
begin
  if Not(Image.Empty) and Assigned(FDataLink.Field) and Assigned(FDataLink.DataSet)
  then begin
       MemStream  := TMemoryStream.Create;
       // BlobStream := TBlobStream.Create(TBlobField(FDataLink.Field), bmWrite); // See above
       BlobStream := FDataLink.DataSet.CreateBlobStream(TBlobField(FDataLink.Field), bmWrite);
       try
         case FDataLink.Field.DataType of
         ftBlob,
         ftTypedBinary : begin
                           Image.SaveToStreamEx(MemStream, FFileFormat);
                           MemStream.Position := 0;
                           if Not(FAutoFormat)
                           then begin
                                i := integer(FFileFormat);
                                BlobStream.Write(i, SizeOf(integer));
                           end
                           else begin
                                if (FAddGraphicHeader)
                                then begin
                                     GraphicHeader.Count := 1;
                                     GraphicHeader.HType := $0100;
                                     GraphicHeader.Size  := MemStream.Size;
                                     BlobStream.Write(GraphicHeader, SizeOf(GraphicHeader));
                                end;
                           end;
                           if (BlobStream.CopyFrom(MemStream, MemStream.Size) <> MemStream.Size)
                           then FError := EC_WRITETOFILE;
                         end;
         ftGraphic     : begin
                           FFileFormat := FF_BMP;
                           Image.SaveToStream(MemStream);
                           if (FAddGraphicHeader)
                           then begin
                                GraphicHeader.Count := 1;
                                GraphicHeader.HType := $0100;
                                GraphicHeader.Size  := MemStream.Size;
                                BlobStream.Write(GraphicHeader, SizeOf(GraphicHeader));
                           end;

                           MemStream.Position := 0;
                           if (BlobStream.CopyFrom(MemStream, MemStream.Size) <> MemStream.Size)
                           then FError := EC_WRITETOFILE;
                         end;
         end;
       finally
         if Assigned(MemStream)
         then MemStream.Free;
         if Assigned(BlobStream)
         then BlobStream.Free;
       end;
  end;
end; // TmcmImageDB.UpdateData.


procedure TmcmImageDB.LoadPicture;
var BlobStream    : TStream; // TBlobStream; Should solve "Invalid Typecast"'s when updating image fields.
    MemStream     : TMemoryStream;
    i             : integer;
    Size          : longint;
    Offset        : integer;
    GraphicHeader : TGraphicHeader;
begin
  if Assigned(FDataLink.Field) and Assigned(FDataLink.DataSet)
  then begin
       if Not(FPictureLoaded) and (not Assigned(FDataLink.Field) or FDataLink.Field.IsBlob)
       then begin
            ControlState := ControlState + [csReadingState];
            MemStream  := TMemoryStream.Create;
            // BlobStream := TBlobStream.Create(TBlobField(FDataLink.Field), bmRead); // See above
            BlobStream := FDataLink.DataSet.CreateBlobStream(TBlobField(FDataLink.Field), bmRead);
            try
              Size := BlobStream.Size;
              if (Size > 0)
              then begin
                   Offset := 0;
                   case FDataLink.Field.DataType of
                   ftBlob,
                   ftTypedBinary : begin
                                     if FAutoFormat
                                     then begin
                                          FFileFormat := FF_DETECT;
                                          // Check if field starts with a TGraphicHeader.
                                          if (Size > SizeOf(TGraphicHeader))
                                          then begin
                                               BlobStream.Read(GraphicHeader, SizeOf(GraphicHeader));
                                               if (GraphicHeader.Count = 1) or
                                                  (GraphicHeader.HType = $0100) or
                                                  (GraphicHeader.Size = Size - SizeOf(GraphicHeader))
                                               then Offset := SizeOf(TGraphicHeader)
                                               else BlobStream.Position := 0;
                                          end;
                                     end
                                     else begin
                                          BlobStream.Read(i, SizeOf(integer));
                                          FFileFormat := TmcmFileFormat(i);
                                          Offset := SizeOf(integer);
                                     end;

                                     MemStream.CopyFrom(BlobStream, Size - Offset);
                                     MemStream.Position := 0;
                                     Image.LoadFromStreamEx(MemStream, FFileFormat);
                                   end;
                   ftGraphic     : begin
                                     FFileFormat := FF_BMP;
                                     // Check if field starts with a TGraphicHeader.
                                     if (Size > SizeOf(TGraphicHeader))
                                     then begin
                                          BlobStream.Read(GraphicHeader, SizeOf(GraphicHeader));
                                          if (GraphicHeader.Count = 1) or
                                             (GraphicHeader.HType = $0100) or
                                             (GraphicHeader.Size = Size - SizeOf(GraphicHeader))
                                          then Offset := SizeOf(TGraphicHeader)
                                          else BlobStream.Position := 0;
                                     end;
                                     MemStream.CopyFrom(BlobStream, Size - Offset);
                                     MemStream.Position := 0;
                                     Image.LoadFromStreamEx(MemStream, FFileFormat);
                                   end;
                   end;
                   FPictureLoaded := True;
              end
              else begin
                   FPictureLoaded := True;
                   Image.Clear;
                   DrawImage;
              end;
            finally
              if Assigned(MemStream)
              then MemStream.Free;
              if Assigned(BlobStream)
              then BlobStream.Free;
              ControlState := ControlState - [csReadingState];
            end;
       end;
  end
  else begin
       FPictureLoaded := True;
       Image.Clear;
       DrawImage;
  end;
end; // TmcmImageDB.LoadPicture.


procedure TmcmImageDB.SetFileFormat(Value : TmcmFileFormat);
begin
  if (FFileFormat <> Value)
  then begin
       FFileFormat := Value;
       if FAutoEdit
       then FDataLink.Edit;
       FDataLink.Modified;
  end;
end; // TmcmImageDB.SetFileFormat.


function TmcmImageDB.GetCompression : TmcmCompress;
begin
  Result := Image.Compression;
end; // TmcmImageDB.GetCompression.


procedure TmcmImageDB.SetCompression(Value : TmcmCompress);
begin
  if (Image.Compression <> Value)
  then begin
       Image.Compression := Value;
       if FAutoEdit
       then FDataLink.Edit;
       FDataLink.Modified;
  end;
end; // TmcmImageDB.SetCompression.


function TmcmImageDB.GetQuality : word;
begin
  Result := Image.Quality;
end; // TmcmImageDB.GetQuality.


procedure TmcmImageDB.SetQuality(Value : word);
begin
  if (0 < Value) and (Value <= 100) and (Image.Quality <> Value)
  then begin
       Image.Quality := Value;
       if FAutoEdit
       then FDataLink.Edit;
       FDataLink.Modified;
  end;
end; // TmcmImageDB.SetQuality.


procedure TmcmImageDB.Paint;
begin
  if Assigned(FDataLink.Field)
  then begin
       if (csPaintCopy in ControlState)
       then begin
            FPictureLoaded := False;
            if FAutoDisplay
            then LoadPicture;
            FPictureLoaded := False;
       end
       else if Not(FPictureLoaded) and FAutoDisplay
            then LoadPicture;
  end;

  Inherited Paint;
end; // TmcmImageDB.Paint.

{$UNDEF DCB3_4}

Initialization

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

Finalization
end.
