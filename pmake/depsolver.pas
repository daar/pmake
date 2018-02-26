unit depsolver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCommandType = (ctUnit, ctExecutable, ctCustom, ctTest);
  TCommandTypes = set of TCommandType;

  pPackage = ^Package;
  Package = record
    name: string;
    resolved: boolean;
    dependency: TFPList;
    unresolved: TFPList;
    commands: TFPList;
    includes: TStrings;
    activepath: string;
    binoutput: string;
    unitsoutput: string;
  end;

  pUnitCommand = ^UnitCommand;
  UnitCommand = record
    command: TCommandType;
    filename: string;
  end;

  pExecutableCommand = ^ExecutableCommand;
  ExecutableCommand = record
    command: TCommandType;
    filename: string;
    executable: string;
  end;

  pTestCommand = ^TestCommand;
  TestCommand = record
    command: TCommandType;
    filename: string;
    executable: string;
    description: string;
  end;

  pInstallCommand = ^InstallCommand;
  InstallCommand = record
    command: TCommandType;
    directory: string;
    destination: string;
    pattern: string;
    depends: pPackage;
  end;

  pCustomCommand = ^CustomCommand;
  CustomCommand = record
    command: TCommandType;
    executable: string;
    parameters: string;
  end;

  pDependency = ^Dependency;
  Dependency = record
    source: string;
    target: string;
  end;

function find_pkg_by_name(pkglist: TFPList; name: string): pPackage;
function find_or_create_package(pkglist: TFPList; name, srcpath, binpath: string): pPackage;
procedure add_dependency_to_cache(depcache: TFPList; source, target: string);
procedure add_dependency(pkglist: TFPList; source, target: string);
procedure remove_resolved_package(var pkglist: TFPList; sourcepkg: pPackage);
function dep_resolve(pkglist: TFPList): TFPList;

implementation

uses
  pmake_utilities, pmake_api;

function find_pkg_by_name(pkglist: TFPList; name: string): pPackage;
var
  pkg: pPackage = nil;
  i: integer;
begin
  for i := 0 to pkglist.Count - 1 do
  begin
    pkg := pkglist[i];

    if pkg^.name = name then
      exit(pkg);
  end;

  exit(nil);
end;

function find_or_create_package(pkglist: TFPList; name, srcpath, binpath: string): pPackage;
var
  pkg: pPackage;
begin
  pkg := find_pkg_by_name(pkglist, name);

  //if package does not yet exist then create a new one
  if pkg = nil then
  begin
    pkg := allocmem(sizeof(Package));
    pkg^.name := name;
    pkg^.resolved := False;

    pkg^.dependency := TFPList.Create;
    pkg^.unresolved := TFPList.Create;
    pkg^.commands := TFPList.Create;

    pkg^.includes := TStringList.Create;

    pkg^.resolved := false;
    pkg^.activepath := srcpath;
    pkg^.unitsoutput := UnitsOutputDir(binpath);;
    pkg^.binoutput := BinOutputDir(binpath);

    pkglist.Add(pkg);
  end;
  result := pkg;
end;

procedure add_dependency_to_cache(depcache: TFPList; source, target: string);
var
  dep: pDependency = nil;
  i: integer;
begin
  //check if dependency already exists in the cache
  for i := 0 to depcache.Count - 1 do
  begin
    dep := depcache[i];

    if (dep^.source = source) and (dep^.target = target) then
      exit;
  end;

  dep := allocmem(sizeof(Dependency));

  dep^.source := source;
  dep^.target := target;

  depcache.Add(dep);
end;

procedure add_dependency(pkglist: TFPList; source, target: string);
var
  sourcepkg: pPackage = nil;
  targetpkg: pPackage = nil;
begin
  sourcepkg := find_pkg_by_name(pkglist, source);
  targetpkg := find_pkg_by_name(pkglist, target);

  if sourcepkg = nil then
    messagefmt(FATAL_ERROR, 'fatal error: cannot find package %s', [source]);

  if targetpkg = nil then
    messagefmt(FATAL_ERROR, 'fatal error: cannot find package %s', [target]);

  //put the dependecies in a separate list
  sourcepkg^.dependency.Add(targetpkg);

  //also add these packages to unresolved for the dependency algorithm
  sourcepkg^.unresolved.Add(targetpkg);
end;

procedure remove_resolved_package(var pkglist: TFPList; sourcepkg: pPackage);
var
  i: integer;
  pkg: pPackage = nil;
  index: integer;
begin
  for i := 0 to pkglist.Count - 1 do
  begin
    pkg := pkglist[i];
    if not pkg^.resolved then
      if pkg^.unresolved.Count > 0 then
      begin
        index := pkg^.unresolved.IndexOf(sourcepkg);
        if index <> -1 then
          pkg^.unresolved.Delete(index);
      end;
  end;
end;

function dep_resolve(pkglist: TFPList): TFPList;
var
  depcount: integer = 0;
  i, j, k: integer;
  pkg: pPackage;
begin
  Result := TFPList.Create;

  while depcount < pkglist.Count do
  begin

    //determine the order to build, based on dependency count
    i := 0;
    pkg := pkglist[i];
    while (pkg^.unresolved.Count > 0) or (pkg^.resolved = True) do
    begin
      inc(i);

      //if no dependency found then raise error
      if i >= pkglist.Count then
      begin
        writeln('fatal error: cannot resolve remaining dependencies');

        //make a dump here for all unresolved packages
        for j := 0 to pkglist.Count - 1 do
        begin
          pkg := pkglist[j];
          if pkg^.unresolved.Count > 0 then
          begin
            write(pkg^.name, ' -> ');
            for k := 0 to pkg^.unresolved.Count - 1 do
              if k <> pkg^.unresolved.Count - 1 then
                write(pPackage(pkg^.unresolved[k])^.name, ', ')
              else
                writeln(pPackage(pkg^.unresolved[k])^.name);
          end;
        end;
        halt(1);
      end;
      pkg := pkglist[i];
    end;

    //save the package to a new list
    Result.Add(pkg);

    //mark package as resolved
    pkg^.resolved := True;

    //keep track of the amount of targets processed
    inc(depcount);

    //delete resolved package for all other packages
    remove_resolved_package(pkglist, pkg);
  end;
end;

end.

