unit make2_package;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function package_zip(const package_folder: string): boolean;
function package_deb(const package_folder: string): boolean;
function package_sfx(const package_folder: string): boolean;

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
  fname := vals('PMAKE_PACKAGE_FILE') + '.zip';
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

//parsing command_execute output
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
{$IFDEF LINUX}
  exit_code: integer;
  control, param: TStringList;
  dir: string;
{$ENDIF}
  fname: string;
begin
  fname := LowerCase(macros_expand('$(PMAKE_PACKAGE_FILE).deb'));
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
  if vals('$(PMAKE_HOST_SYSTEM_PROCESSOR)') = 'x86_64' then
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
  param.Add(fname);

  str := TStringList.Create;
  exit_code := command_execute('dpkg-deb', param, @command_callback);
  str.Free;

  if exit_code <> 0 then
    StdOutLn('(1009) fatal error: cannot execute dpkg-deb');

  param.Free;

  Result := True;
{$ELSE}
  StdOutLn(format('(5025) Cannot create deb pacakge file - %s on non Linux systems', [fname]));
  Result := False;
{$ENDIF}
end;

function package_sfx(const package_folder: string): boolean;
var
  s, param: TStringList;
  fname: string;
  exit_code: Integer;
begin
  //1. create the zip file
  package_zip(package_folder);

  StdOutLn('(5025) Creating SFX...');

  //2. create the sfx.rc file
  fname := ExtractFileName(macros_expand('$(PMAKE_PACKAGE_FILE)'));
  s := TStringList.Create;
  s.Add('ZIPDATA         RCDATA "' + fname + '.zip"');
  s.SaveToFile(macros_expand('$(PMAKE_BINARY_DIR)sfx.rc'));
  s.Free;

  //3. create settings.inc
  s := TStringList.Create;
  s.Add('//settings for SFX');
  {$IFDEF WINDOWS}
  s.Add(macros_expand('DEST_FOLDER = ''C:\Program Files\$(PMAKE_PROJECT_NAME)'';'));
  {$ENDIF}
  {$IFDEF LINUX}
  s.Add('DEST_FOLDER = ''/usr/bin'';');
  {$ENDIF}
  {$IFDEF DARWIN}
  s.Add('DEST_FOLDER = ''/usr/local'';');
  {$ENDIF}
  s.SaveToFile(macros_expand('$(PMAKE_BINARY_DIR)settings.inc'));
  s.Free;

  //4. copy sfx source file
  copyfile(macros_expand('$(PMAKE_TOOL_DIR)sfx') + DirectorySeparator + 'sfx.pp', macros_expand('$(PMAKE_BINARY_DIR)sfx.pp'));

  //5. compile sfx
  param := TStringList.Create;
  param.Add('sfx.pp');
  param.Add(macros_expand('-o$(PMAKE_PACKAGE_FILE)$(EXE)'));

  str := TStringList.Create;
  exit_code := command_execute(vals('PMAKE_PAS_COMPILER'), param, @command_callback);
  str.Free;

  if exit_code <> 0 then
    StdOutLn('(1009) fatal error: cannot execute sfx.pp');

  param.Free;
end;

end.
