unit ufmake;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, fpmkunit;

type
  TCmdTool = (ctFMake, ctMake);
  TCmdTools = set of TCmdTool;

  TCmdOption = record
    Name: string;
    descr: string;
    tools: TCmdTools;
  end;

const
  FMakeVersion = '0.01';

procedure add_executable(pkgname, executable, srcfile: string; depends: array of const);
procedure add_library(pkgname: string; srcfiles, depends: array of const);
procedure install(directory, destination, pattern: string);
procedure add_custom_command(pkgname, executable, parameters: string; depends: array of const);
procedure project(name: string);

procedure add_subdirectory(path: string);

procedure init_make;
procedure run_make;
procedure free_make;

function RunCommand(Executable: string; Parameters: TStrings): TStrings;
procedure check_options(tool: TCmdTool);
procedure usage(tool: TCmdTool);

function BuildCPU: TCpu;
function BuildOS: TOS;
function UnitsOutputDir(BasePath: string; ACPU: TCPU; AOS: TOS): string;
function BinOutputDir(BasePath: string; ACPU: TCPU; AOS: TOS): string;

var
  fpc: string;
  verbose: boolean = False;

implementation

uses
  Crt, SysUtils, Process, depsolver, compiler;

type
  TRunMode = (rmBuild, rmInstall, rmClean);

const
  CmdOptions: array[1..6] of TCmdOption = (
    (Name: 'build'; descr: 'Build all targets in the project.'; tools: [ctMake]),
    (Name: 'clean'; descr: 'Clean all units and folders in the project'; tools: [ctMake]),
    (Name: 'install'; descr: 'Install all targets in the project.'; tools: [ctMake]),
    (Name: '--compiler'; descr: 'Use indicated binary as compiler'; tools: [ctMake, ctFMake]),
    (Name: '--help'; descr: 'This message.'; tools: [ctMake, ctFMake]),
    (Name: '--verbose'; descr: 'Be more verbose.'; tools: [ctMake, ctFMake])
    );

var
  //active_package: pPackage;
  ActivePath: string;
  BasePath: string = '';
  RunMode: TRunMode;
  pkglist: TFPList;
  instlist: TFPList;
  depcache: TFPList;
  projname: string;
  cmd_count: integer = 0;

function BuildCPU: TCpu;
begin
  Result := StringToCPU({$I %FPCTARGETCPU%});
end;

function BuildOS: TOS;
begin
  Result := StringToOS({$I %FPCTARGETOS%});
end;

function UnitsOutputDir(BasePath: string; ACPU: TCPU; AOS: TOS): string;
begin
  Result := BasePath + 'units' + DirectorySeparator + MakeTargetString(ACPU, AOS) + DirectorySeparator;
  if not ForceDirectories(Result) then
  begin
    writeln('Failed to create directory "' + Result + '"');
    halt(1);
  end;
end;

function BinOutputDir(BasePath: string; ACPU: TCPU; AOS: TOS): string;
begin
  Result := BasePath + 'bin' + DirectorySeparator + MakeTargetString(ACPU, AOS) + DirectorySeparator;
  if not ForceDirectories(Result) then
  begin
    writeln('Failed to create directory "' + Result + '"');
    halt(1);
  end;
end;

function RunCommand(Executable: string; Parameters: TStrings): TStrings;
const
  BUF_SIZE = 2048; // Buffer size for reading the output in chunks
var
  AProcess: TProcess;
  OutputStream: TStream;
  BytesRead: longint;
  Buffer: array[1..BUF_SIZE] of byte;
  i: integer;
begin
  for i := 1 to BUF_SIZE do
    Buffer[i] := 0;

  AProcess := TProcess.Create(nil);
  AProcess.Executable := Executable;
  AProcess.Parameters.AddStrings(Parameters);
  AProcess.Options := [poUsePipes];
  AProcess.Execute;

  OutputStream := TMemoryStream.Create;

  repeat
    BytesRead := AProcess.Output.Read(Buffer, BUF_SIZE);
    OutputStream.Write(Buffer, BytesRead)
  until BytesRead = 0;

  AProcess.Free;

  Result := TStringList.Create;

  OutputStream.Position := 0;
  Result.LoadFromStream(OutputStream);

  // Clean up
  OutputStream.Free;
end;

procedure add_dependecies_to_cache(pkgname: string; depends: array of const);
var
  i: Integer;
begin
  for i := Low(depends) to High(depends) do
    add_dependency_to_cache(depcache, pkgname, string(depends[i].VAnsiString));
end;

procedure add_executable(pkgname, executable, srcfile: string; depends: array of const);
var
  pkg: pPackage = nil;
  cmd: pExecutableCommand;
begin
  pkg := find_or_create_package(pkglist, pkgname, activepath);

  cmd := allocmem(sizeof(ExecutableCommand));

  cmd^.command := ctExecutable;
  cmd^.filename := srcfile;
  cmd^.executable := executable;

  //add the command to the package
  pkg^.commands.Add(cmd);

  inc(cmd_count);

  //dependencies will be processed once all packages are processed
  add_dependecies_to_cache(pkgname, depends);
end;

procedure add_library(pkgname: string; srcfiles, depends: array of const);
var
  i: integer;
  pkg: pPackage = nil;
  cmd: pExecutableCommand;
begin
  pkg := find_or_create_package(pkglist, pkgname, activepath);

  //for each source file add a command to the package
  for i := Low(srcfiles) to High(srcfiles) do
  begin
    cmd := allocmem(sizeof(ExecutableCommand));

    cmd^.command := ctUnit;
    cmd^.filename := string(srcfiles[i].VAnsiString);

    //add the command to the package
    pkg^.commands.Add(cmd);

    inc(cmd_count);
  end;

  //dependencies will be processed once all packages are processed
  add_dependecies_to_cache(pkgname, depends);
end;

procedure add_subdirectory(path: string);
begin
  if BasePath = '' then
    BasePath := path;
  ActivePath := path;
end;

procedure project(Name: string);
begin
  projname := Name;
end;

procedure copyfile(old, new: string);
const
  BUF_SIZE = 2048; // Buffer size for reading the output in chunks
var
  infile, outfile: file;
  buf: array[1..BUF_SIZE] of char;
  numread: longint = 0;
  numwritten: longint = 0;
  i: integer;
begin
  for i := 1 to BUF_SIZE do
    buf[i] := #0;

  // open files - no error checking this should be added
  Assign(infile, old);
  reset(infile, 1);
  Assign(outfile, new);
  rewrite(outfile, 1);

  // copy file
  repeat
    blockread(infile, buf, sizeof(buf), numread);
    blockwrite(outfile, buf, numread, numwritten);
  until (numread = 0) or (numwritten <> numread);

  Close(infile);
  Close(outfile);
end;

procedure ExecutePackages(pkglist: TFPList; mode: TCommandTypes);
var
  i, j, k: integer;
  param: TStringList;
  fpc_out: TStrings;
  cmd_out: TStrings;
  fpc_msg: TFPList;
  progress: double = 0;
  pkg: pPackage = nil;
  cmdtype: TCommandType;
  cmd: pointer;
  info: TSearchRec;
begin
  //execute commands
  for i := 0 to pkglist.Count - 1 do
  begin
    progress += 100 / cmd_count;
    write(format('[%3.0f%%] ', [progress]));

    pkg := pkglist[i];

    for j := 0 to pkg^.commands.Count - 1 do
    begin
      cmd := pkg^.commands[j];
      cmdtype := TCommandType(cmd^);

      if cmdtype in mode then
      case cmdtype of
        ctExecutable, ctUnit:
          begin
    param := CompilerCommandLine(pkg, cmd);

    fpc_out := RunCompilerCommand(param);
    param.Free;

    fpc_msg := ParseFPCCommand(fpc_out, BasePath);
    fpc_out.Free;

    WriteFPCCommand(fpc_msg, [mCompiling, mLinking, mFail], progress);
    fpc_msg.Free;
  end;
        ctInstall: begin
          TextColor(blue);

          if FindFirst(pInstallCommand(cmd)^.directory + pInstallCommand(cmd)^.pattern, faAnyFile, info) = 0 then
          begin
            try
              repeat
                if (info.Attr and faDirectory) = 0 then
                begin
                  if not ForceDirectories(pInstallCommand(cmd)^.destination) then
                  begin
                    writeln('Failed to create directory "' + pInstallCommand(cmd)^.directory + '"');
                    halt(1);
                  end;
                  copyfile(pInstallCommand(cmd)^.directory + info.Name, pInstallCommand(cmd)^.destination + info.Name);
                end;
              until FindNext(info) <> 0
            finally
              FindClose(info);
            end;
          end;
          NormVideo;
        end;
        ctCustom:
        begin
          TextColor(blue);
          writeln('Executing ', pCustomCommand(cmd)^.executable);
          NormVideo;

          param := TStringList.Create;
          param.Add(pCustomCommand(cmd)^.parameters);

          cmd_out := RunCommand(pCustomCommand(cmd)^.executable, param);
          param.Free;

          if verbose then
            for k := 0 to cmd_out.Count - 1 do
              writeln(cmd_out[k]);

          cmd_out.Free;
        end;
      end;
     end;
  end;

  writeln('Built package ', pkg^.name);
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

function DeleteDirectory(const DirectoryName: string): boolean;
var
  info: TSearchRec;
begin
  if FindFirst(DirectoryName + '*', faAnyFile, info) = 0 then
  begin
    try
      repeat
        if (info.Attr and faDirectory) = 0 then
        begin
          if not DeleteFile(DirectoryName + info.Name) then
          begin
            Result := False;
            exit;
          end;
        end
        else
        //start the recursive search
        if (info.Name <> '.') and (info.Name <> '..') then
        begin
          if not DeleteDirectory(IncludeTrailingBackSlash(DirectoryName + info.Name)) then
          begin
            Result := False;
            exit;
          end;
          if not RemoveDir(IncludeTrailingBackSlash(DirectoryName + info.Name)) then
          begin
            Result := False;
            exit;
          end;
        end;
      until FindNext(info) <> 0
    finally
      FindClose(info);
    end;
  end;
end;

procedure CleanMode(pkglist: TFPList);
var
  i, j: integer;
  pkg: pPackage = nil;
  cmdtype: TCommandType;
begin
  for i := 0 to pkglist.Count - 1 do
  begin

    pkg := pkglist[i];

    for j := 0 to pkg^.commands.Count - 1 do
    begin
      cmdtype := TCommandType(pkg^.commands[j]^);

      if cmdtype in [ctExecutable, ctUnit] then
      begin
        if DirectoryExists(pkg^.unitsoutput) then
          if not DeleteDirectory(pkg^.unitsoutput) then
          begin
            writeln('error: cannot remove directory ', pkg^.unitsoutput);
            halt(1);
          end;

        if DirectoryExists(pkg^.binoutput) then
          if not DeleteDirectory(pkg^.binoutput) then
          begin
            writeln('error: cannot remove directory ', pkg^.binoutput);
            halt(1);
          end;
      end;
    end;
  end;
end;

//expand some simple macro's
function ExpandMacros(str: string; pkg: pPackage): string;
var
  tmp: string = '';
begin
  tmp := StringReplace(str, '$(TargetOS)', OSToString(BuildOS), [rfReplaceAll]);
  tmp := StringReplace(tmp, '$(TargetCPU)', CPUToString(BuildCPU), [rfReplaceAll]);

  if pkg <> nil then
  begin
    tmp := StringReplace(tmp, '$(UNITSOUTPUTDIR)', pkg^.unitsoutput, [rfReplaceAll]);
    tmp := StringReplace(tmp, '$(BINOUTPUTDIR)', pkg^.binoutput, [rfReplaceAll]);
  end;

  {$ifdef unix}
  tmp := StringReplace(tmp, '$(EXE)', '', [rfReplaceAll]);
  {$else}
  tmp := StringReplace(tmp, '$(EXE)', '.exe', [rfReplaceAll]);
  {$endif}

  {$ifdef windows}
  tmp := StringReplace(tmp, '$(DLL)', '.dll', [rfReplaceAll]);
  {$else}
  tmp := StringReplace(tmp, '$(DLL)', '.so', [rfReplaceAll]);
  {$endif}

  Result := tmp;
end;

procedure install(directory, destination, pattern: string);
var
  cmd: pInstallCommand;
begin
  cmd := AllocMem(sizeof(InstallCommand));
  cmd^.directory := IncludeTrailingPathDelimiter(ExpandMacros(directory, nil));
  cmd^.destination := IncludeTrailingPathDelimiter(ExpandMacros(destination, nil));
  cmd^.pattern := ExpandMacros(pattern, nil);

  instlist.Add(cmd);
end;

procedure add_custom_command(pkgname, executable, parameters: string; depends: array of const);
var
  cmd : pCustomCommand;
  pkg: pPackage;
begin
  pkg := find_or_create_package(pkglist, pkgname, activepath);

  cmd := AllocMem(sizeof(CustomCommand));
  cmd^.executable := ExpandMacros(executable, pkg);
  cmd^.parameters := ExpandMacros(parameters, pkg);

  pkg^.commands.Add(cmd);

  Inc(cmd_count);

  //dependencies will be processed once all packages are processed
  add_dependecies_to_cache(pkgname, depends);
end;

procedure usage(tool: TCmdTool);
var
  i: integer;
  First: boolean;
begin
  writeln('FMake the freepascal build tool. Version ', FMakeVersion, ' [', {$I %DATE%}, '] for ', {$I %FPCTARGETCPU%});
  writeln('Copyright (c) 2016 by Darius Blaszyk');
  writeln;
  writeln('usage: ', ParamStr(0), ' <subcommand> [options] [args]');
  writeln;

  First := True;
  for i := low(CmdOptions) to high(CmdOptions) do
    if tool in CmdOptions[i].tools then
      if pos('--', CmdOptions[i].Name) = 0 then
      begin
        if First then
          writeln('Subcommands');
        First := False;
        writeln(Format(' %-16s %s', [CmdOptions[i].Name, CmdOptions[i].descr]));
      end;

  if First = False then
    writeln;

  First := True;
  for i := low(CmdOptions) to high(CmdOptions) do
    if tool in CmdOptions[i].tools then
      if pos('--', CmdOptions[i].Name) <> 0 then
      begin
        if First then
          writeln('Options');
        First := False;
        writeln(Format(' %-16s %s', [CmdOptions[i].Name, CmdOptions[i].descr]));
      end;

  halt(1);
end;

procedure check_options(tool: TCmdTool);
var
  i, j: integer;
  found: boolean;
begin
  i := 1;

  while i <= ParamCount do
  begin
    found := False;
    for j := low(CmdOptions) to high(CmdOptions) do
    begin
      if ParamStr(i) = CmdOptions[j].Name then
      begin
        if tool in CmdOptions[j].tools then
        begin

          found := True;

          case CmdOptions[j].Name of
            'build': RunMode := rmBuild;
            'clean': RunMode := rmClean;
            'install': RunMode := rmInstall;
            '--compiler':
            begin
              if i < ParamCount then
              begin
                Inc(i);
                fpc := ParamStr(i);
                if not FileExists(fpc) then
                begin
                  writeln('error: cannot find the supplied compiler');
                  halt(1);
                end;
              end
              else
              begin
                writeln('error: please supply a valid path for the compiler');
                usage(tool);
              end;
            end;
            '--help': usage(tool);
            '--verbose': verbose := True;
          end;
        end;
        if found then
          break;
      end;
    end;

    if not found then
    begin
      writeln('error: invalid commandline parameter ', ParamStr(i));
      usage(tool);
    end;

    Inc(i);
  end;
end;

procedure init_make;
begin
  if RunMode = rmBuild then
  begin
    if fpc = '' then
      fpc := ExeSearch('fpc', SysUtils.GetEnvironmentVariable('PATH'));

    if fpc = '' then
    begin
      writeln('error: cannot find the FPC compiler');
      usage(ctMake);
    end;
  end;

  pkglist := TFPList.Create;
  instlist := TFPList.Create;
  depcache := TFPList.Create;

  cmd_count := 0;
end;

procedure run_make;
var
  i: Integer;
  dep: pDependency;
  deplist: TFPList;
begin
  //test to make sure the project is well defined
  if projname = '' then
  begin
    writeln('error: no project defined');
    halt(1);
  end;

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
    rmBuild: ExecutePackages(deplist, [ctUnit, ctExecutable, ctCustom]);
    rmClean: CleanMode(deplist);
    rmInstall: ExecutePackages(deplist, [ctInstall]);
  end;

  deplist.free;
end;

end.
