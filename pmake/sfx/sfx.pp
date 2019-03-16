program sfx;

{$mode objfpc}{$H+}

{$R sfx.rc}

uses
  Classes,
  SysUtils,
  Zipper;

const
  RT_RCDATA = MAKEINTRESOURCE(10);
  {$I settings.inc}

type

  { TSFX }

  TSFX = object
    zip: TUnZipper;
    procedure OnStartFile(Sender: TObject; const AFileName: string);
    procedure ExtractFileFromZip(fname, destdir: string);
  end;

  { TSFX }

  procedure TSFX.OnStartFile(Sender: TObject; const AFileName: string);
  begin
    writeln('extracting - ', AFileName);
  end;

  procedure TSFX.ExtractFileFromZip(fname, destdir: string);
  begin
    zip := TUnzipper.Create;

    zip.FileName := fname;
    zip.OutputPath := destdir;
    zip.OnStartFile := @OnStartFile;
    zip.Examine;
    zip.UnZipAllFiles;

    zip.Free;
  end;

var
  S: TResourceStream;
  F: TFileStream;
  tmp_fname: string;
  sfx_: TSFX;

begin
  writeln('SFX installer by pmake');
  writeln('Copyright (c) 2019 by Darius Blaszyk');
  writeln;

  tmp_fname := GetTempFileName;

  // create a resource stream which points to our resource
  S := TResourceStream.Create(HInstance, 'ZIPDATA', RT_RCDATA);

  try
    // create a temp file
    F := TFileStream.Create(tmp_fname, fmCreate);
    try
      // copy data from the resource stream to file stream
      F.CopyFrom(S, S.Size);
    finally
      F.Free;
    end;
  finally
    S.Free;
  end;

  sfx_.ExtractFileFromZip(tmp_fname, DEST_FOLDER);

  writeln;
  writeln('done.');
  DeleteFile(tmp_fname);
end.
