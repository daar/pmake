unit make2_package;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function package_zip(const package_folder: string): boolean;

implementation

uses
  Zipper, pmake_variables, pmake_utilities;

procedure search_files(var file_list: TStringList; const path: string);
var
  info: TSearchRec;
begin
  if FindFirst(path + '*', faAnyFile, info) = 0 then
  begin
    try
      repeat
        if (info.Attr and faDirectory) = 0 then
        begin
          //add to the file list
          file_list.Add(path + info.Name);
        end
        else
        //start the recursive search
        if (info.Name <> '.') and (info.Name <> '..') then
          search_files(file_list, IncludeTrailingBackSlash(path + info.Name));

      until FindNext(info) <> 0
    finally
      FindClose(info);
    end;
  end;
end;

function package_zip(const package_folder: string): boolean;
var
  AZipper: TZipper;
  i: integer;
  ZEntries : TZipFileEntries;
  FileList: TStringList;
  fname: string;
begin
  fname := val_('PMAKE_PACKAGE_FILE') + '.zip';
  StdOutLn(format('(5025) Creating package zip file - %s', [fname]));

  AZipper := TZipper.Create;
  try
    try
      AZipper.Filename := fname;
      AZipper.Clear;
      ZEntries := TZipFileEntries.Create(TZipFileEntry);

      If DirectoryExists(package_folder) then
      begin
        FileList := TStringList.Create;
        try
          search_files(FileList, package_folder);

          for i:=0 to FileList.Count -1 do
            ZEntries.AddFileEntry(FileList[i], ExtractRelativepath(package_folder, FileList[i]));

        finally
          FileList.Free;
        end;
      end;
      if ZEntries.Count > 0 then
        AZipper.ZipFiles(ZEntries);
      except
        On E: EZipError do
          E.CreateFmt('Zipfile could not be created%sReason: %s', [LineEnding, E.Message])
      end;
    result := True;
  finally
    FreeAndNil(ZEntries);
    AZipper.Free;
  end;
end;

end.

