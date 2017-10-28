unit make2_main;

{$mode objfpc}{$H-}

interface

uses
  Classes, SysUtils,
  pmake_utilities,
  pmake_variables;

procedure init_make2;
procedure execute_make2;

implementation

uses
  depsolver, compiler, pmake_api;

type
  TRunMode = (rmBuild, rmInstall, rmClean);

var
  verbose: Boolean;
  RunMode: TRunMode;

procedure command_callback(line: string);
begin
  write(line);
end;

procedure parse_commandline;
begin
  //need to implement a propoper command line parser here

  verbose := True;
end;

procedure init_make2;
begin
  pmake_initialize;
  pmakecache_read;

  parse_commandline;

  pkglist := TFPList.Create;
  instlist := TFPList.Create;
  depcache := TFPList.Create;

  cmd_count := 0;
end;

procedure free_make;
var
  i, j: integer;
  pkg: pPackage = nil;
begin
  //free all commands from all pacakges
  for i := 0 to pkglist.Count - 1 do
  begin
    pkg := pkglist[i];

    for j := 0 to pkg^.commands.Count - 1 do
      freemem(pkg^.commands[j]);

    pkg^.commands.Free;
    freemem(pkg);
  end;

  pkglist.Free;

  //free all install commands
  for i := 0 to instlist.Count - 1 do
    freemem(instlist[j]);
  instlist.Free;

  //free the dependecy cache
  for i := 0 to depcache.Count - 1 do
    freemem(depcache[i]);

  depcache.Free;
end;

procedure ExecutePackages(pkglist: TFPList; mode: TCommandTypes);
var
  i, j, k, exit_code: integer;
  param: TStringList;
  cmd_out: TStrings;
  progress: double = 0;
  pkg: pPackage = nil;
  cmdtype: TCommandType;
  cmd: pointer;
begin
  //execute commands
  for i := 0 to pkglist.Count - 1 do
  begin
    pkg := pkglist[i];

    for j := 0 to pkg^.commands.Count - 1 do
    begin
      progress += 100 / cmd_count;

      cmd := pkg^.commands[j];
      cmdtype := TCommandType(cmd^);

      if cmdtype in mode then
        case cmdtype of
          ctExecutable, ctUnit:
          begin
            param := CompilerCommandLine(pkg, cmd);
            exit_code := command_execute(val_('PMAKE_PAS_COMPILER'), param, @command_callback);
            param.Free;

            if exit_code <> 0 then
              message(FATAL_ERROR, 'fatal error: cannot compile ' + pUnitCommand(cmd)^.filename);
          end;
          ctCustom:
          begin
            writeln('Executing ', pCustomCommand(cmd)^.executable);

            param := TStringList.Create;
            param.Add(pCustomCommand(cmd)^.parameters);
            exit_code := command_execute(pCustomCommand(cmd)^.executable, param, @command_callback);
            param.Free;

            if verbose then
              for k := 0 to cmd_out.Count - 1 do
                writeln(cmd_out[k]);

            cmd_out.Free;
          end;
        end;
    end;
    writeln(format('[%3.0f%%] Built package %s', [progress, pkg^.name]));
  end;
end;

procedure InstallPackages;
var
  i: integer;
  progress: double = 0;
  cmd: pInstallCommand;
  info: TSearchRec;
  First: boolean = True;
begin
  //execute commands
  for i := 0 to instlist.Count - 1 do
  begin
    cmd := instlist[i];

    progress += 100 / instlist.Count;
    write(format('[%3.0f%%] ', [progress]));

    First := True;

    if FindFirst(cmd^.directory + cmd^.pattern, faAnyFile, info) = 0 then
    begin
      try
        repeat
          if (info.Attr and faDirectory) = 0 then
          begin
            if not ForceDirectories(cmd^.destination) then
            begin
              writeln;
              message(FATAL_ERROR, 'fatal error: failed to create directory "' + cmd^.directory + '"');
            end;

            //give proper offset for consequtive copies
            if not First then
              write('       ');

            writeln('Installing - ', cmd^.destination + info.name);
            copyfile(cmd^.directory + info.name, cmd^.destination + info.name);
            First := False;
          end;
        until FindNext(info) <> 0
      finally
        FindClose(info);
      end;
    end;
  end;

  writeln('Installed files');
end;

procedure CleanMode(pkglist: TFPList);
var
  i, j: integer;
  pkg: pPackage = nil;
  cmdtype: TCommandType;
  progress: double = 0;
begin
  for i := 0 to pkglist.Count - 1 do
  begin
    pkg := pkglist[i];

    progress += 100 / pkglist.Count;
    write(format('[%3.0f%%] ', [progress]));

    writeln('package ', pkg^.name);

    for j := 0 to pkg^.commands.Count - 1 do
    begin
      cmdtype := TCommandType(pkg^.commands[j]^);

      if cmdtype in [ctExecutable, ctUnit] then
      begin
        if DirectoryExists(pkg^.unitsoutput) then
        begin
          if not DeleteDirectory(pkg^.unitsoutput, False) then
          begin
            writeln;
            message(FATAL_ERROR, 'fatal error: cannot remove directory ' + pkg^.unitsoutput);
          end
          else
          if verbose then
            writeln('       deleting ', pkg^.unitsoutput);
        end;

        if DirectoryExists(pkg^.binoutput) then
        begin
          if not DeleteDirectory(pkg^.binoutput, False) then
          begin
            writeln;
            message(FATAL_ERROR, 'fatal error: cannot remove directory ' + pkg^.binoutput);
          end
          else
          if verbose then
            writeln('       deleting ', pkg^.binoutput);
        end;
      end;
    end;
  end;
  writeln('Cleaned all packages');
end;

procedure execute_make2;
var
  i: integer;
  dep: pDependency;
  deplist: TFPList;
begin
  //test to make sure the project is well defined
  if val_('PMAKE_PROJECT_NAME') = '' then
    message(FATAL_ERROR, 'fatal error: no project defined');

  //add all dependencies for all packages. we do this only here to make sure all
  //packages are created first. if a package is not found then something must
  //have gone wrong in the build script.
  for i := 0 to depcache.Count - 1 do
  begin
    dep := depcache[i];
    add_dependency(pkglist, dep^.source, dep^.target);
  end;

  deplist := dep_resolve(pkglist);

  case RunMode of
    rmBuild:
      ExecutePackages(deplist, [ctUnit, ctExecutable, ctCustom]);
    rmClean:
      CleanMode(deplist);
    rmInstall:
      InstallPackages;
  end;

  deplist.Free;
end;


end.

