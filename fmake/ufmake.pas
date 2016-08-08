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

  msgMode = (none, STATUS, WARNING, AUTHOR_WARNING, SEND_ERROR,
    FATAL_ERROR, DEPRECATION);

const
  FMakeVersion = '0.01';

procedure add_executable(pkgname, executable, srcfile: string; depends: array of const);

procedure add_library(pkgname: string; srcfiles: array of const);
procedure add_library(pkgname: string; srcfiles, depends: array of const);

procedure install(directory, destination, pattern, depends: string);
procedure add_custom_command(pkgname, executable, parameters: string; depends: array of const);
procedure project(name: string);

procedure message(mode: msgMode; msg: string);
procedure message(msg: string);

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
    (name: 'build'; descr: 'Build all targets in the project.'; tools: [ctMake]),
    (name: 'clean'; descr: 'Clean all units and folders in the project'; tools: [ctMake]),
    (name: 'install'; descr: 'Install all targets in the project.'; tools: [ctMake]),
    (name: '--compiler'; descr: 'Use indicated binary as compiler'; tools: [ctMake, ctFMake]),
    (name: '--help'; descr: 'This message.'; tools: [ctMake, ctFMake]),
    (name: '--verbose'; descr: 'Be more verbose.'; tools: [ctMake, ctFMake])
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

//expand some simple macro's
function ExpandMacros(str: string; pkg: pPackage = nil): string;
var
  tmp: string = '';
begin
  tmp := StringReplace(str, '$(TargetOS)', OSToString(BuildOS), [rfReplaceAll]);
  tmp := StringReplace(tmp, '$(TargetCPU)', CPUToString(BuildCPU), [rfReplaceAll]);

  if pkg <> nil then
  begin
    tmp := StringReplace(tmp, '$(UNITSOUTPUTDIR)', pkg^.unitsoutput, [rfReplaceAll]);
    tmp := StringReplace(tmp, '$(BINOUTPUTDIR)', pkg^.binoutput, [rfReplaceAll]);
  end
  else
  begin
    if pos('$(UNITSOUTPUTDIR)', tmp) <> 0 then
    begin
      writeln('invalid use of macro $(UNITSOUTPUTDIR) in "' + str + '"');
      halt(1);
    end;
    if pos('$(BINOUTPUTDIR)', tmp) <> 0 then
    begin
      writeln('invalid use of macro $(BINOUTPUTDIR) in "' + str + '"');
      halt(1);
    end;
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

  Parameters.Delimiter := ' ';

  if verbose then
    writeln(Executable, ' ', Parameters.DelimitedText);

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

procedure message(mode: msgMode; msg: string);
begin
  case mode of
    none: ;
    STATUS: ;
    WARNING: ;
    AUTHOR_WARNING: ;
    SEND_ERROR: ;
    FATAL_ERROR: ;
    DEPRECATION: ;
  end;
  writeln(msg);
end;

procedure message(msg: string);
begin
  message(none, msg);
end;

procedure add_subdirectory(path: string);
begin
  if BasePath = '' then
    BasePath := path;
  ActivePath := path;
end;

procedure project(name: string);
begin
  projname := name;
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

            fpc_out := RunCompilerCommand(param);
            param.Free;

            fpc_msg := ParseFPCCommand(fpc_out, BasePath);
            fpc_out.Free;

            writeFPCCommand(fpc_msg, [mCompiling, mLinking, mFail], progress);
            fpc_msg.Free;
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

    TextColor(blue);

    if FindFirst(cmd^.directory + cmd^.pattern, faAnyFile, info) = 0 then
    begin
      try
        repeat
          if (info.Attr and faDirectory) = 0 then
          begin
            if not ForceDirectories(cmd^.destination) then
            begin
              NormVideo;
              writeln;
              writeln('Failed to create directory "' + cmd^.directory + '"');
              halt(1);
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
    NormVideo;
  end;

  writeln('Installed files');
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

function DeleteDirectory(const Directoryname: string; OnlyChildren: boolean): boolean;
const
  //Don't follow symlinks on *nix, just delete them
  DeleteMask = faAnyFile
{$ifdef unix}
    or faSymLink
{$endif unix}
  ;
  {$IFDEF WINDOWS}
  GetAllFilesMask = '*.*';
  {$ELSE}
  GetAllFilesMask = '*';
  {$ENDIF}
var
  FileInfo: TSearchRec;
  CurSrcDir: String;
  CurFilename: String;
begin
  Result := False;
  CurSrcDir := Directoryname;
  if FindFirst(CurSrcDir + GetAllFilesMask, DeleteMask, FileInfo) = 0 then
  begin
    repeat
      // check if special file
      if (FileInfo.Name = '.') or (FileInfo.Name = '..') or (FileInfo.Name = '') then
        continue;

      CurFilename := CurSrcDir + FileInfo.Name;

      if ((FileInfo.Attr and faDirectory) > 0)
      {$ifdef unix}
        and ((FileInfo.Attr and faSymLink) = 0)
      {$endif unix} then
      begin
        if not DeleteDirectory(CurFilename, False) then
          exit;
      end
      else
      begin
        if not DeleteFile(CurFilename) then
          exit;
      end;
    until FindNext(FileInfo) <> 0;
  end;
  FindClose(FileInfo);

  if (not OnlyChildren) and (not RemoveDir(CurSrcDir)) then
    exit;

  Result := True;
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

    NormVideo;

    progress += 100 / pkglist.Count;
    write(format('[%3.0f%%] ', [progress]));

    TextColor(red);
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
            NormVideo;
            writeln;
            writeln('error: cannot remove directory ', pkg^.unitsoutput);
            halt(1);
          end
          else
          if verbose then
            writeln('       deleting ', pkg^.unitsoutput);
        end;

        if DirectoryExists(pkg^.binoutput) then
        begin
          if not DeleteDirectory(pkg^.binoutput, False) then
          begin
            NormVideo;
            writeln;
            writeln('error: cannot remove directory ', pkg^.binoutput);
            halt(1);
          end
          else
          if verbose then
            writeln('       deleting ', pkg^.binoutput);
        end;
      end;
    end;
  end;
  NormVideo;
  writeln('Cleaned all packages');
end;

procedure install(directory, destination, pattern, depends: string);
var
  cmd: pInstallCommand;
  pkg: pPackage;
begin
  pkg := find_pkg_by_name(pkglist, depends);

  if pkg = nil then
  begin
    writeln('error: cannot find dependency "' + depends + '" for install command');
    halt(1);
  end;

  cmd := AllocMem(sizeof(InstallCommand));
  cmd^.directory := IncludeTrailingPathDelimiter(ExpandMacros(directory, pkg));
  cmd^.destination := IncludeTrailingPathDelimiter(ExpandMacros(destination, pkg));
  cmd^.pattern := ExpandMacros(pattern, pkg);
  cmd^.depends := pkg;

  instlist.Add(cmd);
end;

procedure add_custom_command(pkgname, executable, parameters: string; depends: array of const);
var
  cmd: pCustomCommand;
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
      if pos('--', CmdOptions[i].name) = 0 then
      begin
        if First then
          writeln('Subcommands');
        First := False;
        writeln(Format(' %-16s %s', [CmdOptions[i].name, CmdOptions[i].descr]));
      end;

  if First = False then
    writeln;

  First := True;
  for i := low(CmdOptions) to high(CmdOptions) do
    if tool in CmdOptions[i].tools then
      if pos('--', CmdOptions[i].name) <> 0 then
      begin
        if First then
          writeln('Options');
        First := False;
        writeln(Format(' %-16s %s', [CmdOptions[i].name, CmdOptions[i].descr]));
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
      if ParamStr(i) = CmdOptions[j].name then
      begin
        if tool in CmdOptions[j].tools then
        begin

          found := True;

          case CmdOptions[j].name of
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
  i: integer;
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
    rmInstall: InstallPackages;
  end;

  deplist.Free;
end;

initialization
  fpc := ExeSearch(ExpandMacros('fpc$(EXE)'), SysUtils.GetEnvironmentVariable('PATH'));

end.
