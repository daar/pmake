unit pmake_quickstart;

{$mode objfpc}{$H+}

interface

procedure pmake_run_quickstart(srcdir: string);

implementation

uses
  Classes,
  SysUtils,
  make_main,
  pmake_utilities,
  pmake_variables;

type
  ITM_type = (itUnknown, itLibrary, itExecutable, itInclude, itSubDirectory, itInstall, itTest);

  ppmkItem = ^pmkItem;
  pmkItem = record
    next, prev: pointer;

    depends    : string;
    destination: string;
    directory  : string;
    executable : string;
    pattern    : string;
    srcfile    : string;
  end;

  ppmkFile = ^pmkFile;
  pmkFile = record
    next, prev: pointer;
    directory : shortstring;
    pkgname   : shortstring;
    executable: PMK_ListBase;
    include   : boolean;
    install   : PMK_ListBase;
    library_  : PMK_ListBase;
    subdir    : PMK_ListBase;
    test      : boolean;
  end;

  SettingsDef = record
    ignore_fpmake: boolean;
    install: string;
    prefix: string;
    projname: string;
    projversion: string;
    recursive: boolean;
    test: string;
    version: string;
  end;

  opYesNo = (opYes, opNo);

var
  pmkList: PMK_ListBase;
  settings: SettingsDef;

  function prompt(description, question, default: string): string;
  var
    answer: string;
  begin
    writeln(description);

    if default <> '' then
      write('> ', question, ' [', default, ']: ')
    else
      write('> ', question, ': ');

    readln(answer);
    if answer = '' then
      answer := default;

    writeln;

    Result := answer;
  end;

function prompt_yn(description, question: string; default: opYesNo): boolean;
var
  answer: string;
begin
  writeln(description);

  if default = opYes then
    write('> ', question, ' (y/n) [y]: ')
  else
    write('> ', question, ' (y/n) [n]: ');

  readln(answer);
  writeln;

  if answer = '' then
    if default = opYes then
      answer := 'y'
    else
      answer := 'n';

  if LowerCase(answer[1]) = 'y' then
    exit(True)
  else
    exit(False);
end;

procedure ParseSourceFile(fname: string; out pmtype: ITM_type; out name: string);
var
  f: TStrings;
  i: integer;
begin
  f := TStringList.Create;
  f.LoadFromFile(fname);

  pmtype := itUnknown;

  //find pmtype: very naive, but it's a start!
  if ExtractFileExt(LowerCase(fname)) = '.inc' then
    pmtype := itInclude
  else
  begin
    for i := 0 to f.Count - 1 do
    begin
      if pos('unit', Trim(LowerCase(f[i]))) = 1 then
      begin
        pmtype := itLibrary;
        break;
      end;
      if pos('program', Trim(LowerCase(f[i]))) = 1 then
      begin
        //name is here based on the source file
        name := LowerCase(ExtractFileName(ChangeFileExt(fname, '')));
        pmtype := itExecutable;
        break;
      end;
      if pos('library', Trim(LowerCase(f[i]))) = 1 then
      begin
        //name is here based on the source file
        name := LowerCase(ExtractFileName(ChangeFileExt(fname, '')));
        pmtype := itExecutable;
        break;
      end;
    end;
  end;
  f.Free;
end;

function find_or_create_itm(const path: string): ppmkFile;
var
  p: ppmkFile;
  tmp: string;
begin
  p := pmkList.first;

  while p <> nil do
  begin
    if p^.directory = path then
      exit(p);

    p := p^.next;
  end;

  //create a new pmkFile
  p := callocN(SizeOf(pmkFile));
  addtail(@pmkList, p);

  p^.directory := path;

  //pkgname is here based on the directory the file is located in
  tmp := LowerCase(ExtractFileDir(path) + '.tmp');
  p^.pkgname := settings.prefix + '_' + LowerCase(ExtractFileName(ChangeFileExt(tmp, '')));;

  exit(p);
end;

procedure parse_source_tree(const basepath, path: string; recursive: boolean);
var
  info: TSearchRec;
  p: ppmkFile;
  pt: ITM_type;
  name: string;
  itm: ppmkItem;
  relpath: RawByteString;
begin
  //search pascal files in base folder
  if FindFirst(path + '*', faAnyFile, info) = 0 then
  begin
    try
      repeat
        if (info.Attr and faDirectory) = 0 then
        begin
          //add PMake.txt to the file list
          case ExtractFileExt(info.Name) of
            '.p', '.pp', '.pas', '.inc':
              begin
                p := find_or_create_itm(path);

                ParseSourceFile(path + info.Name, pt, name);

                if not p^.test then
                begin
                  //check if folder is a test folder
                  relpath := DirectorySeparator + ExtractRelativePath(basepath, path);
                  if pos(DirectorySeparator + settings.test + DirectorySeparator, relpath) <> 0 then
                    p^.test := True;
                end;

                case pt of
                  itLibrary:
                          begin
                            itm := callocN(SizeOf(pmkItem));
                            addtail(@p^.library_, itm);

                            itm^.srcfile := info.Name;
                          end;
                  itExecutable:
                          begin
                            if not (settings.ignore_fpmake and (ChangeFileExt(info.Name, '') = 'fpmake')) then
                            begin
                              itm := callocN(SizeOf(pmkItem));
                              addtail(@p^.executable, itm);

                              itm^.executable := name;
                              itm^.srcfile := info.Name;

                              if settings.install <> '' then
                              begin
                                itm := callocN(SizeOf(pmkItem));
                                addtail(@p^.install, itm);

                                itm^.directory := '$(BINOUTPUTDIR)';
                                itm^.destination := '$(PMAKE_PACKAGE_DIR)';
                                itm^.pattern := name + '$(EXE)';
                              end;
                            end;
                          end;
                  itInclude:
                          begin
                            if not (settings.ignore_fpmake and (ChangeFileExt(info.Name, '') = 'fpmake')) then
                              p^.include := True;
                          end;
                end;
             end;
          end;
        end
        else
          //start the recursive search
          if (info.Name <> '.') and (info.Name <> '..') and (recursive = True) then
            parse_source_tree(basepath, IncludeTrailingBackSlash(path + info.Name), recursive);

      until FindNext(info) <> 0
    finally
      FindClose(info);
    end;
  end;
end;

procedure write_pmake_files(const path: string);
var
  p: ppmkFile;
  f: TStrings;
  uname: string = '';
  it: ppmkItem;
begin
  f := TStringList.Create;

  p := pmkList.first;

  while p <> nil do
  begin
    writeln('  > ', ExtractRelativepath(path, p^.directory) + 'PMake.txt');

    f.Clear;

    f.Add('(* This file was auto-generated');
    f.Add(' *');
    f.Add(' * PMake version: ' + PMAKE_VERSION);
    uname := GetEnvironmentVariable('USER');
    if uname = '' then
      uname := GetEnvironmentVariable('USERNAME');
    f.Add(' * Generated by : ' + uname);
    f.Add(' * Date         : ' + FormatDateTime('mmmm dd, yyyy', Now));
    f.Add(' *)');
    f.Add('');

    //root folder items to add
    if p^.directory = path then
    begin
      f.Add('compiler_minimum_required(' + StringReplace(settings.version, '.', ',', [rfReplaceAll]) + ');');
      f.Add(Format('project(''%s'', ''%s'');', [settings.projname, settings.projversion]));
      f.Add('');
    end;

    //subdirectories
    if p^.subdir.first <> nil then
    begin
      f.Add('// subdirectories');

      it := p^.subdir.first;
      while it <> nil do
      begin
        f.Add(Format('add_subdirectory(''%s'');', [it^.directory]));
        it := it^.next;
      end;
      f.Add('');
    end;

    //include files
    if p^.include then
    begin
      f.Add('// include files');
      f.Add(Format('include_directories(''%s'', [''$(PMAKE_CURRENT_SOURCE_DIR)'']);', [p^.pkgname]));
      f.Add('');
    end;

    //libraries
    if p^.library_.first <> nil then
    begin
      f.Add('// libraries');
      f.Add(Format('add_library(''%s'',', [p^.pkgname]));
      f.Add('[');

      it := p^.library_.first;
      while it <> nil do
      begin
        if it^.next <> nil then
          f.Add(Format('  ''%s'',', [it^.srcfile]))
        else
          f.Add(Format('  ''%s''', [it^.srcfile]));

        it := it^.next;
      end;
      f.Add('], []);');
      f.Add('');
    end;

    //executables
    if p^.executable.first <> nil then
    begin
      if not p^.test then
      begin
        f.Add('// executables');

        it := p^.executable.first;
        while it <> nil do
        begin
          f.Add(Format('add_executable(''%s'', ''%s$(EXE)'', ''%s'', []);', [p^.pkgname, it^.executable, it^.srcfile]));
          it := it^.next;
        end;
      end
      else
      begin
        f.Add('// tests');

        it := p^.executable.first;
        while it <> nil do
        begin
          f.Add(Format('add_test(''%s$(EXE)'', ''%s'', [], ''%s test file'');', [it^.executable, it^.srcfile, it^.srcfile]));
          it := it^.next;
        end;
      end;

      f.Add('');
    end;

    //install
    if not p^.test and (p^.install.first <> nil) then
    begin
      f.Add('// install');

      it := p^.install.first;
      while it <> nil do
      begin
        f.Add(Format('install(''$(BINOUTPUTDIR)'', ''$(PMAKE_PACKAGE_DIR)'', ''%s'', ''%s'');', [it^.pattern, p^.pkgname]));
        it := it^.next;
      end;
      f.Add('');
    end;

    //root folder items to add
    if p^.directory = path then
    begin
      f.Add('// create the package');
      f.Add('create_package(''$(PMAKE_PROJECT_NAME)-$(PROJECT_VERSION)-$(PMAKE_HOST_SYSTEM_PROCESSOR)-$(PMAKE_HOST_SYSTEM_NAME)'', ''package'');');
    end;

    f.SaveToFile(p^.directory + 'PMake.txt');

    p := p^.next;
  end;
end;

function find_subdirectory(list: PMK_ListBase; subdir: string): boolean;
var
  p: ppmkItem;
begin
  p := list.first;

  while p <> nil do
  begin
    if p^.directory = subdir then
      exit(True);

    p := p^.next;
  end;

  exit(false);
end;

//this procedure does a second pass on the pmkList to check which subdirectory calls should be added
procedure parse_subdirectories(const path: string);
var
  p, p2: ppmkFile;
  d: TStrings;
  folder: string;
  itm: ppmkItem;
begin
  d := TStringList.Create;
  d.Delimiter := DirectorySeparator;

  p := pmkList.first;

  while p <> nil do
  begin
    d.DelimitedText := ExcludeTrailingPathDelimiter(p^.directory);

    //traverse down one directory
    folder := d[d.Count - 1];
    d.Delete(d.Count - 1);

    //traverse the path until the base path is reached or even below
    //need to stop in that case!
    while Length(d.DelimitedText) + 1 >= Length(path) do
    begin
      p2 := find_or_create_itm(d.DelimitedText + DirectorySeparator);

      if not find_subdirectory(p2^.subdir, folder) then
      begin
        //add subdirectory
        itm := callocN(SizeOf(pmkItem));
        addtail(@p2^.subdir, itm);

        itm^.directory := folder;
      end;

      //traverse down one directory
      folder := d[d.Count - 1];
      d.Delete(d.Count - 1);
    end;

    p := p^.next;
  end;

  d.Free;
end;

procedure pmake_run_quickstart(srcdir: string);
begin
  writeln('Welcome to the PMake ', PMAKE_VERSION, ' quickstart utility.');
  writeln;
  writeln('Please enter values for the following settings (just press ENTER to');
  writeln('accept a default value, if one is given in brackets).');
  writeln;

  //user preferences
  with settings do
  begin
    recursive := prompt_yn('PMake can recursively search the source directory provided and add PMake.txt files as required', 'Would you like PMake to search recursively?', opYes);
    version := prompt('Some projects require a specific minimum compiler version, you can specify this now', 'Please enter the minimum compiler version', val_('PMAKE_PAS_COMPILER_VERSION'));
    projname := prompt('PMake requires a project name to define the project', 'Please enter the project name', '');
    projversion := prompt('A project version helps to distinguish between releases (use dot `.` or dash `-` or a combination in the version string)', 'Please enter the current project version', 'none');
    prefix := prompt('Please specify which library prefix to use', 'Enter the desired prefix', 'lib');
    install := prompt('Please specify if executables should be installed', 'Enter the install directory (press enter to ignore)', '');
    test := prompt('PMake comes out of the box with test running (run tests by doing a `make test`)', 'Please enter the test folder name (press enter to ignore)', '');
    ignore_fpmake := prompt_yn('If your project is already using fpmake, the you have tha chance here to ignore them for this build', 'Ignore fpmake files', opYes);
  end;

  //do the magic!
  writeln('... generating build information');
  parse_source_tree(srcdir, srcdir, settings.recursive);

  //parse subdirectories
  writeln('... parse subdirectory calls');
  parse_subdirectories(srcdir);

  //write every PMake.txt to file
  writeln('... writing all PMake.txt files');
  write_pmake_files(srcdir);

  writeln('done.');
  halt(0);
end;

end.

