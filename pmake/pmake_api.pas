unit pmake_api;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  msgMode = (NONE, STATUS, WARNING, AUTHOR_WARNING, SEND_ERROR,
    FATAL_ERROR, DEPRECATION);
  fileAction = (WRITEFILE, READFILE);

procedure add_executable(pkgname, executable, srcfile: string; depends: array of const);

procedure add_library(pkgname: string; srcfiles: array of const);
procedure add_library(pkgname: string; srcfiles, depends: array of const);

procedure add_test(executable, srcfile: string; depends: array of const; description: string);

procedure include_directories(pkgname: string; directories: array of const);

procedure create_package(const file_name, base_directory: string);
procedure install(directory, destination, pattern, depends: string);
procedure add_custom_command(pkgname, executable, parameters: string; depends: array of const);

procedure compiler_minimum_required(major, minor, revision: integer);
procedure project(name: string; version: string = '');

//todo: add as command to package
procedure message(mode: msgMode; msg: string);
//todo: add as command to package
procedure message(msg: string);
//todo: add as command to package
procedure messagefmt(mode: msgMode; msg: string; const args: array of const);
//todo: add as command to package
procedure messagefmt(msg: string; const args: array of const);

procedure add_subdirectory(path: string);
procedure define_pmake(path: string);

//todo: add as command to package
function execute_process(const curdir, exename: string; const commands: array of string; name: string): boolean;
//todo: add as command to package
procedure execute_file(const filename, name: string; action: fileAction);

implementation

uses
  Process,
  depsolver,
  pmake_variables,
  pmake_utilities;

procedure add_dependecies_to_cache(pkgname: string; depends: array of const);
var
  i: integer;
begin
  for i := Low(depends) to High(depends) do
  begin
    //ignore if dependency is empty string 
    if AnsiString(depends[i].VAnsiString) <> '' then
      add_dependency_to_cache(depcache, pkgname, AnsiString(depends[i].VAnsiString));
  end;
end;

procedure add_executable(pkgname, executable, srcfile: string; depends: array of const);
var
  pkg: pPackage = nil;
  cmd: pExecutableCommand;
begin
  pkg := find_or_create_package(pkglist, pkgname, val_('PMAKE_CURRENT_SOURCE_DIR'), val_('PMAKE_CURRENT_BINARY_DIR'));

  cmd := allocmem(sizeof(ExecutableCommand));

  cmd^.command := ctExecutable;
  cmd^.filename := srcfile;
  cmd^.executable := executable;

  //add the command to the package
  pkg^.commands.Add(cmd);

  Inc(cmd_count);

  //dependencies will be processed once all packages are processed
  add_dependecies_to_cache(pkgname, depends);
end;

procedure add_library(pkgname: string; srcfiles: array of const);
begin
  add_library(pkgname, srcfiles, []);
end;

procedure add_library(pkgname: string; srcfiles, depends: array of const);
var
  i: integer;
  pkg: pPackage = nil;
  cmd: pExecutableCommand;
begin
  pkg := find_or_create_package(pkglist, pkgname, val_('PMAKE_CURRENT_SOURCE_DIR'), val_('PMAKE_CURRENT_BINARY_DIR'));

  //for each source file add a command to the package
  for i := 0 to High(srcfiles) do
  begin
    cmd := allocmem(sizeof(ExecutableCommand));

    cmd^.command := ctUnit;
    cmd^.filename := AnsiString(srcfiles[I].VAnsiString);

    //add the command to the package
    pkg^.commands.Add(cmd);

    Inc(cmd_count);
  end;

  //dependencies will be processed once all packages are processed
  add_dependecies_to_cache(pkgname, depends);
end;

procedure add_test(executable, srcfile: string; depends: array of const; description: string);
var
  pkg: pPackage = nil;
  cmd: pTestCommand;
begin
  pkg := find_or_create_package(pkglist, _TEST_PGK_NAME_, val_('PMAKE_CURRENT_SOURCE_DIR'), val_('PMAKE_CURRENT_BINARY_DIR'));

  cmd := allocmem(sizeof(TestCommand));

  cmd^.command := ctTest;
  cmd^.filename := srcfile;
  cmd^.executable := executable;
  cmd^.description := description;

  //add the command to the package
  pkg^.commands.Add(cmd);

  Inc(cmd_count);

  //dependencies will be processed once all packages are processed
  add_dependecies_to_cache(_TEST_PGK_NAME_, depends);
end;

procedure include_directories(pkgname: string; directories: array of const);
var
  i: integer;
  pkg: pPackage;
  dir: string;
  curdir: string;
begin
  curdir := val_('PMAKE_CURRENT_SOURCE_DIR');
  pkg := find_or_create_package(pkglist, pkgname, curdir, val_('PMAKE_CURRENT_BINARY_DIR'));

  for i := Low(directories) to High(directories) do
  begin
    dir := IncludeTrailingPathDelimiter(ExpandFileName(macros_expand(AnsiString(directories[i].VAnsiString))));
    pkg^.includes.Add(dir);
  end;
end;

procedure create_package(const file_name, base_directory: string);
var
  dir: string;
begin
  dir := IncludeTrailingPathDelimiter(ExpandFileName(macros_expand(base_directory)));

  set_('PMAKE_PACKAGE_FILE', ExpandFileName(macros_expand(file_name)));
  set_('PMAKE_PACKAGE_DIR', dir);

  if not ForceDirectories(dir) then
    messagefmt(FATAL_ERROR, '(1009) fatal error: failed to create directory "%s"', [dir]);
end;

procedure install(directory, destination, pattern, depends: string);
var
  cmd: pInstallCommand;
  pkg: pPackage;
begin
  pkg := find_pkg_by_name(pkglist, depends);

  if pkg = nil then
    messagefmt(FATAL_ERROR, '(1009) fatal error: cannot find dependency "%s" for install command', [depends]);

  cmd := AllocMem(sizeof(InstallCommand));
  cmd^.directory := ExpandFileName(IncludeTrailingPathDelimiter(macros_expand(directory, pkg)));
  cmd^.destination := ExpandFileName(IncludeTrailingPathDelimiter(macros_expand(destination, pkg)));
  cmd^.pattern := macros_expand(pattern, pkg);
  cmd^.depends := pkg;

  instlist.Add(cmd);
end;

procedure add_custom_command(pkgname, executable, parameters: string; depends: array of const);
var
  cmd: pCustomCommand;
  pkg: pPackage;
begin
  pkg := find_or_create_package(pkglist, pkgname, val_('PMAKE_CURRENT_SOURCE_DIR'), val_('PMAKE_CURRENT_BINARY_DIR'));

  cmd := AllocMem(sizeof(CustomCommand));

  cmd^.executable := macros_expand(executable, pkg);
  cmd^.parameters := macros_expand(parameters, pkg);
  cmd^.command := ctCustom;

  pkg^.commands.Add(cmd);

  Inc(cmd_count);

  //dependencies will be processed once all packages are processed
  add_dependecies_to_cache(pkgname, depends);
end;

procedure message(mode: msgMode; msg: string);
begin
  case mode of
    NONE: StdErrLn(msg);
    STATUS: StdOutLn(msg);
    WARNING: StdErrLn( msg);
    AUTHOR_WARNING: StdErrLn(msg);
    SEND_ERROR: StdErrLn(msg);
    FATAL_ERROR:
    begin
      StdErrLn(msg);
      halt(1);
    end;
    DEPRECATION: StdErrLn(msg);
  end;
end;

procedure message(msg: string);
begin
  message(NONE, msg);
end;

procedure messagefmt(mode: msgMode; msg: string; const args: array of const);
begin
  message(mode, Format(msg, args));
end;

procedure messagefmt(msg: string; const args: array of const);
begin
  messagefmt(NONE, msg, args);
end;

procedure add_subdirectory(path: string);
var
  srcdir: string;
  pmkdir: string;
  locdir: string;
  dir: string;
  bindir: string;
begin
  srcdir := val_('PMAKE_SOURCE_DIR');
  pmkdir := val_('PMAKE_CURRENT_DEFINE_DIR');

  locdir := macros_expand(path);

  //check if path is relative or not
  if ExpandFileName(locdir) = locdir then
    dir := locdir
  else
    dir := IncludeTrailingPathDelimiter(pmkdir + locdir);

  if srcdir = '' then
    set_('PMAKE_SOURCE_DIR', dir);

  //update the source directory
  set_('PMAKE_CURRENT_SOURCE_DIR', dir);

  //update the binary directory
  bindir := val_('PMAKE_BINARY_DIR');
  set_('PMAKE_CURRENT_BINARY_DIR', ExpandFileName(bindir + ExtractRelativepath(srcdir, dir)));
end;

procedure define_pmake(path: string);
begin
  set_('PMAKE_CURRENT_DEFINE_DIR', path);
  add_subdirectory(path);
end;

function execute_process(const curdir, exename: string; const commands: array of string; name: string): boolean;
var
  outputstring: string;
begin
{$IF FPC_FULLVERSION >= 30002}
  Result := RunCommandIndir(macros_expand(curdir), exename, commands, outputstring, [poWaitOnExit]);
{$ELSE}
  Result := RunCommandIndir(macros_expand(curdir), exename, commands, outputstring);
{$ENDIF}
  
  if name <> '' then
    set_(name, outputstring);
end;

procedure execute_file(const filename, name: string; action: fileAction);
var
  f: TStrings;
  v: pPMK_variant;
  s: string;
begin
  f := TStringList.Create;

  case action of
    WRITEFILE:
      begin
        v := find_variable(name);
        //v = nil
        case v^.vtype of
          ptBoolean: s := BoolToStr(v^.PM_Boolean);
          ptInteger: s := IntToStr(v^.PM_Integer);
          ptFloat: s := FloatToStr(v^.PM_Float);
          ptString: s := v^.PM_String;
        end;

        f.Add(s);
        f.SaveToFile(macros_expand(filename));
      end;
    READFILE:
      begin
        f.LoadFromFile(macros_expand(filename));
        set_(name, f.Text);
      end;
  end;

  f.Free;
end;

procedure compiler_minimum_required(major, minor, revision: integer);
var
  ver: TStrings;
  isOK: boolean = False;
begin
  ver := TStringList.Create;
  ver.Delimiter := '.';
  ver.DelimitedText := val_('PMAKE_PAS_COMPILER_VERSION');

  //check version numbers
  if StrToInt(ver[0]) > major then
    isOK := True
  else
  if (StrToInt(ver[0]) = major) and (StrToInt(ver[1]) > minor) then
    isOK := True
  else
  if (StrToInt(ver[0]) = major) and (StrToInt(ver[1]) = minor) and
    (StrToInt(ver[2]) >= revision) then
    isOK := True;

  ver.Free;

  if not isOK then
    messagefmt(FATAL_ERROR, '(1009) fatal error: minimum compiler version required is %d.%d.%d, got %s', [major, minor, revision, val_('PMAKE_PAS_COMPILER_VERSION')]);
end;

procedure project(name: string; version: string = '');
var
  str: string;
  s: TStrings;
begin
  set_('PMAKE_PROJECT_NAME', name);

  if version <> '' then
  begin
    set_('PROJECT_VERSION', version);

    str := version;

    //replace separators in version string e.g. 2.5.0, 2.05-rc1
    str := StringReplace(str, '.', ' ', []);
    str := StringReplace(str, '-', ' ', []);

    s := TStringList.Create;
    s.Delimiter := ' ';
    s.DelimitedText := str;

    if s.Count > 0 then
      set_('PROJECT_VERSION_MAJOR', s[0]);
    if s.Count > 1 then
      set_('PROJECT_VERSION_MINOR', s[1]);
    if s.Count > 2 then
      set_('PROJECT_VERSION_PATCH', s[2]);
    if s.Count > 3 then
      set_('PROJECT_VERSION_TWEAK', s[3]);
  end;
end;

end.
