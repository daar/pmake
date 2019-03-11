unit make2_package;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function package_zip(const package_folder: string): boolean;
function package_deb(const package_folder: string): boolean;

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
  StdOutLn(format('(5025) Creating zip package file - %s', [fname]));

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

var
  str: TStringList;

//parsing dpkg-deb output
procedure command_callback(line: string; active: boolean);
begin
  str.Text := str.Text + line;

  while str.Count > 0 do
  begin
    OutputLn(str[0]);
    str.Delete(0);
  end;
end;

function package_deb(const package_folder: string): boolean;
var
  exit_code: integer;
  control, param: TStringList;
  fname, dir: string;
begin
{$IFDEF LINUX}
  //http://www.king-foo.com/2011/11/creating-debianubuntu-deb-packages/

  StdOutLn(format('(5025) Creating deb package file - %s', [fname]));

  //Step 1: Create the directories
  //  already done in execute_make2

  //Step 2: Copy files into your package
  //  already done in execute_make2

  //Step 3: Create the control file
  //
  control := TStringList.Create;
  control.Add('Package: ' + LowerCase(macros_expand('$(PMAKE_PROJECT_NAME)')));
  control.Add(macros_expand('Version: $(PROJECT_VERSION)'));
  control.Add(macros_expand('Maintainer: $(PMAKE_DEBIAN_PACKAGE_MAINTAINER)'));

  //replace x86_64 by amd64
  if val_('$(PMAKE_HOST_SYSTEM_PROCESSOR)') = 'x86_64' then
    control.Add('Architecture: amd64')
  else
    control.Add(macros_expand('Architecture: $(PMAKE_HOST_SYSTEM_PROCESSOR)'));

  control.Add(macros_expand('Description: $(PMAKE_DEBIAN_PACKAGE_DESCRIPTION)'));
  control.Add(macros_expand('Depends: $(PMAKE_DEBIAN_PACKAGE_DEPENDS)'));

  dir := macros_expand('$(PMAKE_PACKAGE_DIR)DEBIAN');

  if not ForceDirectories(dir) then
    writeln(Format('(1009) fatal error: failed to create directory "%s"', [dir]));

  control.SaveToFile(dir + '/control');

  //Step 4: Add a post-installation script
  //  not implemented yet

  //Step 5: Create the package
  param := TStringList.Create;
  param.Add('--build');
  param.Add(macros_expand('$(PMAKE_PACKAGE_DIR)'));
  param.Add(LowerCase(macros_expand('$(PMAKE_PACKAGE_FILE).deb')));

  str := TStringList.Create;
  exit_code := command_execute('dpkg-deb', param, @command_callback);
  str.Free;

  if exit_code <> 0 then
    StdOutLn('(1009) fatal error: cannot execute dpkg-deb');

  param.Free;

{$ELSE}
  StdOutLn(format('(5025) Cannot create deb pacakge file - %s on non Linux systems', [fname]));
{$ENDIF}
end;

end.

