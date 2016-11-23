unit acSkinPack;
{$I sDefs.inc}
{$WARNINGS OFF}
// ZLib version 1.2.3 used (http://www.zlib.net)

interface

uses
  Windows, Messages, SysUtils, Classes;

type
  TacImageItem = record
    FileName : string;
    IsBitmap : boolean;
    FileStream : TMemoryStream;
  end;

  TacImageItems = array of TacImageItem;

  TacSkinConvertor = class(TPersistent)
  public
    // Unpacked data
    ImageCount : integer;
    Files : TacImageItems;
    Options : TMemoryStream;
    // Packed data
    PackedData : TMemoryStream;
    procedure Clear;
    destructor Destroy; override;
  end;

const
  acAbbr = 'ASzf';

procedure PackDir(const SrcPath, FileName : string);
procedure UnpackSkinFile(const Filename, DestDirectory : String);
procedure LoadSkinFromFile(const FileName : string; var Convertor : TacSkinConvertor; FreePackedData : boolean = True);
procedure ExtractPackedData(Convertor : TacSkinConvertor);

implementation

uses acntUtils, sConst, ZLibEx, Dialogs;

procedure GetFiles(const DirPath, FileExt: acString; FileList: TStringList);
var
  Status: THandle;
  FindData: TacWin32FindData;
begin
  Status := acFindFirstFile(PacChar(DirPath + FileExt), FindData);
  if Status <> INVALID_HANDLE_VALUE then repeat
    if (FindData.cFileName[0] <> '.') and (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0) then FileList.Add(FindData.cFileName);
  until not acFindNextFile(Status, FindData);
  Windows.FindClose(Status);
end;

procedure CompressFiles(FilesDir : string; Files : TStrings; const Filename : String);
var
  InFile, OutFile : TFileStream;
  TmpFile : TMemoryStream;
  Compr : TZCompressionStream;
  i, l : Integer;
  s : String;
begin
  if Files.Count > 0 then begin
    OutFile := TFileStream.Create(Filename, fmCreate);
    FilesDir := NormalDir(FilesDir);
    try
      OutFile.Write(acAbbr, SizeOf(acAbbr));
      l := Files.Count;
      OutFile.Write(l, SizeOf(l));
      for i := 0 to Files.Count - 1 do begin
        InFile := TFileStream.Create(FilesDir + Files[i], fmOpenRead);
        try
          s := Files[i];
          l := Length(s);
          OutFile.Write(l, SizeOf(l));
          OutFile.Write(s[1], l);

          l := InFile.Size;
          OutFile.Write(l, SizeOf(l));

          TmpFile := TMemoryStream.Create;
          Compr := TZCompressionStream.Create(TmpFile);
          Compr.CopyFrom(InFile, l);
          FreeAndNil(Compr);

          OutFile.CopyFrom(TmpFile, 0);
          FreeAndNil(TmpFile);
        finally
          FreeAndNil(InFile);
        end;
      end;
    finally
      FreeAndNil(OutFile);
    end;
  end;
end;

procedure PackDir(const SrcPath, FileName : string);
var
  FilesList : TStringList;
begin
  FilesList := TStringList.Create;
  GetFiles(SrcPath, '*.dat', FilesList);
  GetFiles(SrcPath, '*.bmp', FilesList);
  GetFiles(SrcPath, '*.png', FilesList);
  GetFiles(SrcPath, '*.jpg', FilesList);
  CompressFiles(SrcPath, FilesList, FileName);
  FreeAndNil(FilesList);
end;

procedure UnpackSkinFile(const Filename, DestDirectory : String);
var
  dest, s  : String;
  decompr : TZDecompressionStream;
  infile, outfile : TFilestream;
  i,l,c : Integer;
begin
  Dest := NormalDir(DestDirectory);
  if not acDirExists(Dest) then acCreateDir(Dest);
  InFile := TFileStream.Create(Filename, fmOpenRead);
  try
    SetLength(s, 4);
    InFile.Read(s[1], 4);
    if s <> acAbbr then begin
      MessageDlg(FileName + ' is not packed AlphaSkin file', mtWarning, [mbOk], 0);
      FreeAndnil(InFile);
      Exit;
    end;
    InFile.Read(c, SizeOf(c));
    for i := 1 to c do begin
      Dest := NormalDir(Dest);
      InFile.Read(l, SizeOf(l));
      SetLength(s, l);
      InFile.Read(s[1], l);
      InFile.Read(l, SizeOf(l));
      s := Dest + s;

      if FileExists(s) then DeleteFile(s);
      OutFile := TFileStream.Create(s, fmCreate);
      Decompr := TZDecompressionStream.Create(InFile);
      OutFile.CopyFrom(decompr, l);
      OutFile.Free;
      Decompr.Free;
    end;
  finally
    infile.Free;
  end;
end;

procedure LoadSkinFromFile(const FileName : string; var Convertor : TacSkinConvertor; FreePackedData : boolean = True);
begin
  if FileExists(FileName) then begin
    if Convertor = nil then Convertor := TacSkinConvertor.Create else Convertor.Clear;
    Convertor.PackedData := TMemoryStream.Create;
    Convertor.PackedData.LoadFromFile(FileName);
    try
      ExtractPackedData(Convertor);
    finally
      if FreePackedData then FreeAndnil(Convertor.PackedData);
    end;
  end;
end;

procedure ExtractPackedData(Convertor : TacSkinConvertor);
var
  s  : OldString;
  decompr : TZDecompressionStream;
  i, l, c : Integer;
begin
  SetLength(s, 4);
  Convertor.PackedData.Seek(0, 0);
  Convertor.PackedData.Read(s[1], 4);
  if s <> acAbbr then begin
    MessageDlg('Loaded data is not packed AlphaSkin file', mtWarning, [mbOk], 0);
    Convertor.Clear;
    FreeAndnil(Convertor);
    Exit;
  end;
  Convertor.PackedData.Read(c, SizeOf(c));
  Convertor.ImageCount := c - 1;
  for i := 1 to c do begin
    Convertor.PackedData.Read(l, SizeOf(l));
    SetLength(s, l);
    Convertor.PackedData.Read(s[1], l);
    Convertor.PackedData.Read(l, SizeOf(l));

    Decompr := TZDecompressionStream.Create(Convertor.PackedData);
    if UpperCase(s) = UpperCase(OptionsDatName) then begin
      Convertor.Options := TMemoryStream.Create;
      Convertor.Options.CopyFrom(Decompr, l);
    end
    else begin
      SetLength(Convertor.Files, Length(Convertor.Files) + 1);
      Convertor.Files[Length(Convertor.Files) - 1].FileName := s;
      Convertor.Files[Length(Convertor.Files) - 1].IsBitmap := UpperCase(ExtractFileExt(s)) = '.BMP';
      Convertor.Files[Length(Convertor.Files) - 1].FileStream := TMemoryStream.Create;
      Convertor.Files[Length(Convertor.Files) - 1].FileStream.CopyFrom(Decompr, l);
    end;
    FreeAndNil(Decompr);
  end;
end;

{ TacSkinConvertor }

procedure TacSkinConvertor.Clear;
begin
  while Length(Files) > 0 do begin
    Files[Length(Files) - 1].FileStream.Free;
    SetLength(Files, Length(Files) - 1);
  end;
  if Options <> nil then FreeAndNil(Options);
  if PackedData <> nil then FreeAndNil(PackedData);
end;

destructor TacSkinConvertor.Destroy;
begin
  Clear;
  inherited;
end;

end.
