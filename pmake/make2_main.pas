unit make2_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pmake_utilities,
  pmake_variables;

procedure init_make2;
procedure execute_make2;

implementation

uses
  depsolver, compiler, pmake_api, make2_package;

type
  TRunMode = (rmBuild, rmInstall, rmClean, rmPackage, rmTest);
  TPackage = (pkZip);

var
  RunMode: TRunMode;
  Package: TPackage;
  sline: TStringList;
  progress: double = 0;

procedure output_line(var sline: TStringList);
var
  line: string;
begin
  line := StringReplace(sline[0], val_('PMAKE_SOURCE_DIR'), '.' + DirectorySeparator, [rfReplaceAll]);
  line := StringReplace(line, '(3104)', format('(3104) [%3.0f%%]', [progress]), [rfReplaceAll]);

  StdOutLn(line);

  sline.Delete(0);
end;

procedure command_callback(line: string; active: boolean);
begin
  //parse the output
  sline.Text := sline.Text + line;

  while sline.Count > 1 do
    output_line(sline);

  if not active then
  begin
    while sline.Count > 0 do
      output_line(sline);
  end;
end;

procedure execute_callback(line: string; active: boolean);
begin
  if verbose then
    write(StdOut, line);
end;

procedure parse_commandline;
var
  i: integer;
begin
  RunMode := rmBuild;

  i := 1;

  while i <= ParamCount do
  begin
    case ParamStr(i) of
      'build': RunMode := rmBuild;
      'clean': RunMode := rmClean;
      'install': RunMode := rmInstall;
      'package':
      begin
        RunMode := rmPackage;

        Inc(i);

        case ParamStr(i) of
          'zip': package := pkZip;
          else
            package := pkZip;
        end;
      end;
      'test': RunMode := rmTest;
      '--verbose': verbose := True;
    end;
    Inc(i);
  end;
end;

procedure init_make2;
begin
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
  i, j, exit_code: integer;
  param: TStringList;
  pkg: pPackage = nil;
  cmdtype: TCommandType;
  cmd: pointer;
  count: longint = 0;
begin
  progress := 0;

  //determine number of packages
  for i := 0 to pkglist.Count - 1 do
  begin
    pkg := pkglist[i];

    for j := 0 to pkg^.commands.Count - 1 do
    begin
      cmd := pPackage(pkglist[i])^.commands[j];
      cmdtype := TCommandType(cmd^);

      if cmdtype in mode then
        inc(count);
    end;
  end;

  //execute commands
  for i := 0 to pkglist.Count - 1 do
  begin
    pkg := pkglist[i];

    for j := 0 to pkg^.commands.Count - 1 do
    begin
      progress += 100 / count;

      cmd := pkg^.commands[j];
      cmdtype := TCommandType(cmd^);

      if cmdtype in mode then
        case cmdtype of
          ctExecutable, ctUnit:
          begin
            param := CompilerCommandLine(pkg, cmd);
            sline := TStringList.Create;
            exit_code := command_execute(val_('PMAKE_PAS_COMPILER'), param, @command_callback);
            sline.Free;
            param.Free;

            if exit_code <> 0 then
              messagefmt(FATAL_ERROR, '(1009) fatal error: cannot compile %s', [pUnitCommand(cmd)^.filename]);
          end;
          ctCustom:
          begin
            StdOutLn('(5025) Executing ' + pCustomCommand(cmd)^.executable);

            param := TStringList.Create;
            param.Add(pCustomCommand(cmd)^.parameters);
            exit_code := command_execute(pCustomCommand(cmd)^.executable, param, @execute_callback);
            param.Free;

            if exit_code <> 0 then
              messagefmt(FATAL_ERROR, '(1009) fatal error: executing %s', [pCustomCommand(cmd)^.executable]);
          end;
        end;
    end;

    if cmdtype in mode then
      if cmdtype in [ctCustom] then
        StdOutLn(format('(5025) [%3.0f%%] Executed package %s', [progress, pkg^.name]))
      else
        StdOutLn(format('(5025) [%3.0f%%] Built package %s', [progress, pkg^.name]));
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
    write(StdOut, format('(-100) [%3.0f%%] ', [progress]));

    First := True;

    if FindFirst(cmd^.directory + cmd^.pattern, faAnyFile, info) = 0 then
    begin
      try
        repeat
          if (info.Attr and faDirectory) = 0 then
          begin
            if not ForceDirectories(cmd^.destination) then
            begin
              StdOutLn('');
              messagefmt(FATAL_ERROR, '(1009) fatal error: failed to create directory "%s"', [cmd^.destination]);
            end;

            //give proper offset for consecutive copies
            if not First then
              write(StdOut, '(-100)        ');

            StdOutLn('Installing - ' + cmd^.destination + info.name);
            copyfile(cmd^.directory + info.name, cmd^.destination + info.name);
            First := False;
          end;
        until FindNext(info) <> 0
      finally
        FindClose(info);
      end;
    end;
  end;

  StdOutLn('(5025) Installed files');
end;

procedure ExecuteTests(pkglist: TFPList);
var
  j, exit_code: integer;
  param: TStringList;
  pkg: pPackage = nil;
  cmd: pTestCommand;
  res: String;
  stime: TDateTime;
begin
  StdOutLn('(5025) ');
  StdOutLn('(5025) running tests');

  progress := 0;

  pkg := find_pkg_by_name(pkglist, _TEST_PGK_NAME_);

  stime := Now;

  //execute commands
  for j := 0 to pkg^.commands.Count - 1 do
  begin
    progress += 100 / pkg^.commands.Count;

    cmd := pkg^.commands[j];

    //compile test
    param := CompilerCommandLine(pkg, cmd);
    sline := TStringList.Create;
    exit_code := command_execute(val_('PMAKE_PAS_COMPILER'), param, @execute_callback);
    sline.Free;
    param.Free;

    if exit_code <> 0 then
      messagefmt(FATAL_ERROR, '(1009) fatal error: cannot compile %s', [cmd^.filename]);

    //execute test
    exit_code := command_execute(pkg^.binoutput + cmd^.executable, nil, @execute_callback);

    if exit_code <> 0 then
      res := 'fail'
    else
      res := 'ok';

    StdOutLn(format('(5025)   %s (%s) ... %s', [cmd^.executable, cmd^.description, res]))
  end;

  StdOutLn(format('(5025) Ran %d tests in %0.3fs', [pkg^.commands.Count, (Now - stime) * 24 * 3600]));
end;

procedure PackageAll(package: TPackage);
begin
  case package of
    pkZip: package_zip(val_('PMAKE_PACKAGE_DIR'));
  end;
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
    StdOutLn(format('(-90) [%3.0f%%] package $s', [progress, pkg^.name]));

    for j := 0 to pkg^.commands.Count - 1 do
    begin
      cmdtype := TCommandType(pkg^.commands[j]^);

      if cmdtype in [ctExecutable, ctUnit] then
      begin
        if DirectoryExists(pkg^.unitsoutput) then
        begin
          if not DeleteDirectory(pkg^.unitsoutput, False) then
          begin
            StdOutLn('');
            messagefmt(FATAL_ERROR, '(1009) fatal error: cannot remove directory %s', [pkg^.unitsoutput]);
          end
          else
          if verbose then
            StdOutLn('       deleting ' + pkg^.unitsoutput);
        end;

        if DirectoryExists(pkg^.binoutput) then
        begin
          if not DeleteDirectory(pkg^.binoutput, False) then
          begin
            StdOutLn('');
            messagefmt(FATAL_ERROR, '(1009) fatal error: cannot remove directory $s', [pkg^.binoutput]);
          end
          else
          if verbose then
            StdOutLn('       deleting ' + pkg^.binoutput);
        end;
      end;
    end;
  end;
  StdOutLn('(5025) Cleaned all packages');
end;

procedure execute_make2;
var
  i: integer;
  dep: pDependency;
  deplist: TFPList;
begin
  //test to make sure the project is well defined
  if val_('PMAKE_PROJECT_NAME') = '' then
    message(FATAL_ERROR, '(1009) fatal error: no project defined');

  (*
   * add all dependencies for all packages. we do this only here to make sure all
   * packages are created first. if a package is not found then something must
   * have gone wrong in the build script.
   *)
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
    begin
      ExecutePackages(deplist, [ctUnit, ctExecutable, ctCustom]);
      InstallPackages;
    end;
    rmPackage:
    begin
      ExecutePackages(deplist, [ctUnit, ctExecutable, ctCustom]);
      InstallPackages;
      PackageAll(package);
    end;
    rmTest:
    begin
      ExecutePackages(deplist, [ctUnit, ctExecutable, ctCustom]);
      ExecuteTests(deplist);
    end;
  end;

  deplist.Free;
end;

end.

