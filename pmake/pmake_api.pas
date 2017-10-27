unit pmake_api;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  msgMode = (none, STATUS, WARNING, AUTHOR_WARNING, SEND_ERROR,
    FATAL_ERROR, DEPRECATION);

procedure add_executable(pkgname, executable, srcfile: string; depends: array of const);

procedure add_library(pkgname: string; srcfiles: array of const);
procedure add_library(pkgname: string; srcfiles, depends: array of const);

procedure install(directory, destination, pattern, depends: string);
procedure add_custom_command(pkgname, executable, parameters: string; depends: array of const);

procedure compiler_minimum_required(major, minor, revision: integer);
procedure project(Name: string; major: integer = 0; minor: integer = 0; patch: integer = 0; tweak: integer = 0);

procedure message(mode: msgMode; msg: string);
procedure message(msg: string);
procedure messagefmt(mode: msgMode; msg: string; const args: array of const);
procedure messagefmt(msg: string; const args: array of const);

procedure add_subdirectory(path: string);

implementation

uses
  depsolver,
  pmake_variables,
  pmake_utilities;

procedure add_dependecies_to_cache(pkgname: string; depends: array of const);
var
  i: integer;
begin
  for i := Low(depends) to High(depends) do
    add_dependency_to_cache(depcache, pkgname, string(depends[i].VAnsiString));
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
  for i := Low(srcfiles) to High(srcfiles) do
  begin
    cmd := allocmem(sizeof(ExecutableCommand));

    cmd^.command := ctUnit;
    cmd^.filename := string(srcfiles[i].VAnsiString);

    //add the command to the package
    pkg^.commands.Add(cmd);

    Inc(cmd_count);
  end;

  //dependencies will be processed once all packages are processed
  add_dependecies_to_cache(pkgname, depends);
end;

procedure install(directory, destination, pattern, depends: string);
var
  cmd: pInstallCommand;
  pkg: pPackage;
begin
  pkg := find_pkg_by_name(pkglist, depends);

  if pkg = nil then
    message(FATAL_ERROR, 'fatal error: cannot find dependency "' + depends + '" for install command');

  cmd := AllocMem(sizeof(InstallCommand));
  cmd^.directory := IncludeTrailingPathDelimiter(macros_expand(directory, pkg));
  cmd^.destination := IncludeTrailingPathDelimiter(macros_expand(destination, pkg));
  cmd^.pattern := macros_expand(pattern, pkg);
  cmd^.depends := pkg;

  instlist.Add(cmd);
end;

procedure add_custom_command(pkgname, executable, parameters: string;
  depends: array of const);
var
  cmd: pCustomCommand;
  pkg: pPackage;
begin
  pkg := find_or_create_package(pkglist, pkgname, val_('PMAKE_CURRENT_SOURCE_DIR'), val_('PMAKE_CURRENT_BINARY_DIR'));

  cmd := AllocMem(sizeof(CustomCommand));
  cmd^.executable := macros_expand(executable, pkg);
  cmd^.parameters := macros_expand(parameters, pkg);

  pkg^.commands.Add(cmd);

  Inc(cmd_count);

  //dependencies will be processed once all packages are processed
  add_dependecies_to_cache(pkgname, depends);
end;

procedure message(mode: msgMode; msg: string);
begin
  case mode of
    none: writeln(stderr, msg);
    STATUS: writeln(stdout, msg);
    WARNING: writeln(stderr, msg);
    AUTHOR_WARNING: writeln(stderr, msg);
    SEND_ERROR: writeln(stderr, msg);
    FATAL_ERROR:
    begin
      writeln(stderr, msg);
      halt(1);
    end;
    DEPRECATION: writeln(stderr, msg);
  end;
end;

procedure message(msg: string);
begin
  message(none, msg);
end;

procedure messagefmt(mode: msgMode; msg: string; const args: array of const);
begin
  message(mode, Format(msg, args));
end;

procedure messagefmt(msg: string; const args: array of const);
begin
  messagefmt(none, msg, args);
end;

procedure add_subdirectory(path: string);
begin
  if val_('PMAKE_SOURCE_DIR') = '' then
    set_('PMAKE_SOURCE_DIR', path);

  set_('PMAKE_CURRENT_SOURCE_DIR', path);
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
    messagefmt(FATAL_ERROR, 'fatal error: minimum compiler version required is %d.%d.%d, got %s', [major, minor, revision, val_('PMAKE_PAS_COMPILER_VERSION')]);
end;

procedure project(Name: string; major: integer = 0; minor: integer = 0;
  patch: integer = 0; tweak: integer = 0);
begin
  set_('PMAKE_PROJECT_NAME', Name);

  set_('PROJECT_VERSION_MAJOR', major);
  set_('PROJECT_VERSION_MINOR', minor);
  set_('PROJECT_VERSION_PATCH', patch);
  set_('PROJECT_VERSION_TWEAK', tweak);

  if tweak <> 0 then
    set_('PROJECT_VERSION', Format('%d.%d.%d.%d', [major, minor, patch, tweak]))
  else
    set_('PROJECT_VERSION', Format('%d.%d.%d', [major, minor, patch]));
end;

end.
