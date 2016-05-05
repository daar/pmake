unit ufmake;

{$mode objfpc}{$H+}
{ $define debug}

interface

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, Crt;

type

  TMessage = (mCompiling, mDebug, mError, mFail, mHint, mInformation, mLinking,
    mNote, mOption, mUnitInfo, mUnknown, mWarning);
  TMessages = set of TMessage;

  TFPCOutput = record
    msgno: integer;
    msgtype: TMessage;
  end;

  TFPCColor = record
    msgtype: TMessage;
    msgcol: integer;
  end;

  TFPCMessage = record
    msgidx: integer;
    Text: string;
  end;
  PFPCMessage = ^TFPCMessage;

  TCmdTool = (ctFMake, ctMake);
  TCmdTools = set of TCmdTool;

  TCmdOption = record
    Name: string;
    descr: string;
    tools: TCmdTools;
  end;

const
  FMakeVersion = '0.01';

  mAll = [mCompiling, mDebug, mError, mFail, mHint, mInformation,
    mLinking, mNote, mOption, mUnitInfo, mUnknown, mWarning];

procedure add_executable(Name: string; files: array of const);
procedure add_library(Name: string; files: array of const);
procedure add_subdirectory(Name: string);
procedure project(Name: string);
procedure target_link_libraries(Name: string; files: array of const);
procedure install(directory, destination, pattern: string);

procedure init_make;
procedure run_make;
procedure free_make;

function RunFPCCommand(Parameters: TStrings): TStrings;
function ParseFPCCommand(FPCOutput: TStrings): TFPList;
procedure WriteFPCCommand(FPCMsgs: TFPList; ShowMsg: TMessages; progress: double = -1);
function GetFPCMsgType(msgidx: integer): TMessage;
procedure check_options(tool: TCmdTool);
procedure usage(tool: TCmdTool);

var
  fpc: string;
  verbose: boolean = False;

implementation

uses
  SysUtils, fpmkunit, Process;

type
  TTargetType = (ttExe, ttUnit);

  TInstallCommand = record
    pattern: string;
    directory: string;
    destination: string;
  end;
  PInstallCommand = ^TInstallCommand;

  TTarget = class
    Dep: TStrings;
    Todo_Dep: TStrings;
    Units: TStrings;
    Name: string;
    Executable: string;
    TargetType: TTargetType;
    Done: boolean;
    ActivePath: string;
    BinOutput: string;
    UnitsOutput: string;
    Install: TFPList;
  end;

  TBuild = class
    Targets: TFPList;
    projname: string;
    sfilecount: integer;
    progress: double;
  end;

  TRunMode = (rmBuild, rmInstall, rmClean);

const
  CmdOptions: array[1..6] of TCmdOption = (
    (Name: 'build'; descr: 'Build all targets in the project.'; tools: [ctMake]),
    (Name: 'clean'; descr: 'Clean all units and folders in the project'; tools: [ctMake]),
    (Name: 'install'; descr: 'Install all targets in the project.'; tools: [ctMake]),
    (Name: '--fpc-compiler'; descr: 'Use indicated binary as compiler'; tools: [ctMake, ctFMake]),
    (Name: '--help'; descr: 'This message.'; tools: [ctMake, ctFMake]),
    (Name: '--verbose'; descr: 'Be verbose.'; tools: [ctMake, ctFMake])
    );

  MsgCol: array [TMessage] of TFPCColor = (
    (msgtype: mCompiling; msgcol: Green),
    (msgtype: mDebug; msgcol: LightGray),
    (msgtype: mError; msgcol: White),
    (msgtype: mFail; msgcol: Yellow),
    (msgtype: mHint; msgcol: LightGray),
    (msgtype: mInformation; msgcol: LightGray),
    (msgtype: mLinking; msgcol: Red),
    (msgtype: mNote; msgcol: LightGray),
    (msgtype: mOption; msgcol: LightGray),
    (msgtype: mUnitInfo; msgcol: LightGray),
    (msgtype: mUnknown; msgcol: LightGray),
    (msgtype: mWarning; msgcol: LightGray));

//to add more FPC versions, ifdef this include file
{$i fpc300.inc}

var
  fbuild: TBuild;
  active_target: TTarget;
  ActivePath: string;
  BasePath: string = '';
  RunMode: TRunMode;

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

function GetFPCMsgType(msgidx: integer): TMessage;
begin
  if msgidx = -1 then
    Result := mUnknown
  else
    Result := Msg[msgidx].msgtype;
end;

procedure WriteFPCCommand(FPCMsgs: TFPList; ShowMsg: TMessages; progress: double = -1);

  function GetFPCMsgCol(msgidx: integer): TMessage;
  begin
    if msgidx = -1 then
      Result := mUnknown
    else
      Result := Msg[msgidx].msgtype;
  end;

var
  i: integer;
  fpc_msg: TFPCMessage;
  fpc_msgtype: TMessage;
begin
  for i := 0 to FPCMsgs.Count - 1 do
  begin
    fpc_msg := TFPCMessage(FPCMsgs[i]^);
    fpc_msgtype := GetFPCMsgType(fpc_msg.msgidx);
    if fpc_msgtype in ShowMsg then
    begin
      if (fpc_msgtype = mCompiling) and (progress >= 0) and (progress <= 100) then
        Write(format('[%3.0f%%]', [progress]));

      if fpc_msg.msgidx <> -1 then
        TextColor(MsgCol[fpc_msgtype].msgcol);
      writeln(fpc_msg.Text);
      NormVideo;
    end;
  end;
end;

function ParseFPCCommand(FPCOutput: TStrings): TFPList;
var
  sLine, snum: string;
  found: boolean;
  i: integer;
  FPCmsg: PFPCMessage;
  msgidx: integer;
  ipos: SizeInt;
begin
  Result := TFPList.Create;

  for i := 0 to FPCOutput.Count - 1 do
  begin
    sLine := StringReplace(FPCOutput[i], BasePath, '.' + DirectorySeparator, [rfReplaceAll]);

    found := False;
    for msgidx := Low(Msg) to High(Msg) do
    begin
      snum := Format('(%d)', [Msg[msgidx].msgno]);
      ipos := Pos(snum, sLine);
      if ipos <> 0 then
      begin
        sLine := StringReplace(sLine, sNum + ' ', '', [rfReplaceAll]);

        found := True;
        break;
      end;
    end;

    FPCmsg := GetMem(sizeof(TFPCMessage));

    if found then
      FPCmsg^.msgidx := msgidx
    else
      FPCmsg^.msgidx := -1;

    FPCmsg^.Text := sLine;
    Result.Add(FPCmsg);
  end;
end;

function RunFPCCommand(Parameters: TStrings): TStrings;
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
  AProcess.Executable := fpc;
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

function FindDepTarget(Dependency: string): TTarget;
var
  i: integer;
  target: TTarget;
begin
  for i := 0 to fbuild.Targets.Count - 1 do
  begin
    target := TTarget(fbuild.Targets[i]);
    if target.Name = dependency then
      exit(target);
  end;

  //dependency not found!
  exit(nil);
end;

function CompilerCommand(ATarget: TTarget; Source: string; TType: TTargetType): TStringList;
var
  i: integer;
  targ: TTarget;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;

  // Output file paths
  if TType = ttExe then
    Result.Add('-FE' + ATarget.BinOutput);

  Result.Add('-FU' + ATarget.UnitsOutput);

  //search other unit files for the dependencies
  for i := 0 to ATarget.Dep.Count - 1 do
  begin
    targ := FindDepTarget(ATarget.Dep[i]);

    if targ = nil then
    begin
      writeln('fail: cannot find dependency ', ATarget.Dep[i]);
      halt(1);
    end;

    Result.Add('-Fu' + targ.UnitsOutput);
  end;

  // Output executable name
  if TType = ttExe then
  begin
    Result.Add(Source);
    Result.Add('-o' + ATarget.Name + ExtractFileExt(ParamStr(0)));
  end;

  // compile unit name
  if TType = ttUnit then
    Result.Add(Source);

  // Force the compiler-output to be easy parseable
  //if not Verbose then
  Result.Add('-viq');
end;

procedure add_executable(Name: string; files: array of const);
var
  i: integer;
begin
  //add a new target with files associated to it
  active_target := TTarget.Create;
  active_target.units := TStringList.Create;
  active_target.dep := TStringList.Create;
  active_target.todo_dep := TStringList.Create;
  active_target.Install := TFPList.Create;

  active_target.Name := Name;
  active_target.Executable := ActivePath + string(files[Low(files)].VAnsiString);
  Inc(fbuild.sfilecount);

  for i := Low(files) + 1 to High(files) do
  begin
    active_target.units.Add(string(files[i].VAnsiString));
    Inc(fbuild.sfilecount);
  end;

  active_target.TargetType := ttExe;
  active_target.done := False;
  active_target.ActivePath := ActivePath;

  active_target.UnitsOutput := UnitsOutputDir(ActivePath, BuildCPU, BuildOS);
  active_target.BinOutput := BinOutputDir(ActivePath, BuildCPU, BuildOS);

  fbuild.Targets.Add(active_target);
end;

procedure add_library(Name: string; files: array of const);
var
  i: integer;
begin
  //add a new target with files associated to it
  active_target := TTarget.Create;
  active_target.units := TStringList.Create;
  active_target.dep := TStringList.Create;
  active_target.todo_dep := TStringList.Create;
  active_target.Install := TFPList.Create;

  active_target.Name := Name;

  for i := Low(files) to High(files) do
  begin
    active_target.units.Add(string(files[i].VAnsiString));
    Inc(fbuild.sfilecount);
  end;
  active_target.TargetType := ttUnit;
  active_target.done := False;
  active_target.ActivePath := ActivePath;

  active_target.UnitsOutput := UnitsOutputDir(ActivePath, BuildCPU, BuildOS);

  fbuild.Targets.Add(active_target);
end;

procedure add_subdirectory(Name: string);
begin
  if BasePath = '' then
    BasePath := Name;
  ActivePath := Name;
end;

procedure project(Name: string);
begin
  fbuild.projname := Name;
end;

procedure ExecuteTarget(target: TTarget);
var
  i: integer;
  param: TStringList;
  fpc_out: TStrings;
  fpc_msg: TFPList;
begin
  //build units of target
  for i := 0 to target.units.Count - 1 do
  begin
    fbuild.progress += 100 / fbuild.sfilecount;
    param := CompilerCommand(target, target.ActivePath + target.units[i], ttUnit);

    fpc_out := RunFPCCommand(param);
    param.Free;

    fpc_msg := ParseFPCCommand(fpc_out);
    fpc_out.Free;

    WriteFPCCommand(fpc_msg, [mCompiling, mLinking, mFail], fbuild.progress);
    fpc_msg.Free;
  end;

  //build exe
  if target.TargetType = ttExe then
  begin
    fbuild.progress += 100 / fbuild.sfilecount;
    param := CompilerCommand(target, target.Executable, ttExe);

    fpc_out := RunFPCCommand(param);
    param.Free;

    fpc_msg := ParseFPCCommand(fpc_out);
    fpc_out.Free;

    WriteFPCCommand(fpc_msg, [mCompiling, mLinking, mFail], fbuild.progress);
    fpc_msg.Free;
  end;

  writeln('Built target ', target.Name);
end;

procedure free_make;
var
  i, j: integer;
  target: TTarget;
  ic: PInstallCommand;
begin
  //free all objects
  for i := 0 to fbuild.Targets.Count - 1 do
  begin
    target := TTarget(fbuild.Targets[i]);
    target.todo_dep.Free;
    target.dep.Free;
    target.units.Free;

    for j := 0 to target.Install.Count - 1 do
    begin
      ic := PInstallCommand(target.Install[j]);
      FreeMem(ic);
    end;
    target.Install.Free;

    target.Free;
  end;
  fbuild.Targets.Free;
  fbuild.Free;
end;

procedure BuildMode;
var
  done: integer = 0;
  depname: string;
  i, j, k: integer;
  target: TTarget;
begin
  while done < fbuild.Targets.Count do
  begin

    //determine the order to build, based on dependency count
    i := 0;
    target := TTarget(fbuild.Targets[i]);
    while (target.todo_dep.Count > 0) or (target.done = True) do
    begin
      Inc(i);
      if i > fbuild.Targets.Count - 1 then
      begin
        writeln('error: dependencies between remaining packages cannot be resolved!');

        //make a dump here for all unresolved packages
        for j := 0 to fbuild.Targets.Count - 1 do
        begin
          target := TTarget(fbuild.Targets[j]);
          if target.Todo_Dep.Count > 0 then
          begin
            writeln('Target:       ', target.Name);
            Write('Dependencies: ');
            for k := 0 to target.Todo_Dep.Count - 1 do
              if k <> target.Todo_Dep.Count - 1 then
                Write(target.Todo_Dep[k], ', ')
              else
                writeln(target.Todo_Dep[k]);
          end;
        end;

        halt(1);
      end;
      target := TTarget(fbuild.Targets[i]);
    end;

    ExecuteTarget(target);

    target.Done := True;

    depname := '';
    if target.TargetType = ttUnit then
      depname := target.Name;

    //keep track of the amount of targets processed
    Inc(done);

    //remove depencency as it is resolved
    if depname <> '' then
      for i := 0 to fbuild.Targets.Count - 1 do
      begin
        target := TTarget(fbuild.Targets[i]);
        j := target.todo_dep.IndexOf(depname);
        if j <> -1 then
          target.todo_dep.Delete(j);
      end;
  end;
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

procedure CleanMode;
var
  i: integer;
  target: TTarget;
begin
  for i := 0 to fbuild.Targets.Count - 1 do
  begin
    target := TTarget(fbuild.Targets[i]);

    if DirectoryExists(target.UnitsOutput) then
      if not DeleteDirectory(target.UnitsOutput) then
      begin
        writeln('error: cannot remove directory ', target.UnitsOutput);
        halt(1);
      end;

    if DirectoryExists(target.BinOutput) then
      if not DeleteDirectory(target.BinOutput) then
      begin
        writeln('error: cannot remove directory ', target.BinOutput);
        halt(1);
      end;
  end;
end;

procedure copyfile(old, new: string);
var
  infile, outfile: file;
  buf: array[1..2048] of char;
  numread: longint = 0;
  numwritten: longint = 0;
begin
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

procedure InstallMode;
var
  info: TSearchRec;
  i, j: integer;
  target: TTarget;
  ic: PInstallCommand;
begin
  for i := 0 to fbuild.Targets.Count - 1 do
  begin
    target := TTarget(fbuild.Targets[i]);

    for j := 0 to target.Install.Count - 1 do
    begin
      ic := PInstallCommand(target.Install[j]);

      if FindFirst(ic^.directory + ic^.pattern, faAnyFile, info) = 0 then
      begin
        try
          repeat
            if (info.Attr and faDirectory) = 0 then
            begin
              if not ForceDirectories(ic^.destination) then
              begin
                writeln('Failed to create directory "' + ic^.directory + '"');
                halt(1);
              end;
              copyfile(ic^.directory + info.Name, ic^.destination + info.Name);
            end;
          until FindNext(info) <> 0
        finally
          FindClose(info);
        end;
      end;
    end;
  end;
end;

//expand some simple macro's
function ExpandMacros(str: string; Target: TTarget): string;
var
  tmp: string = '';
begin
  tmp := StringReplace(str, '$(TargetOS)', OSToString(BuildOS), [rfReplaceAll]);
  tmp := StringReplace(tmp, '$(TargetCPU)', CPUToString(BuildCPU), [rfReplaceAll]);
  tmp := StringReplace(tmp, '$(UNITSOUTPUTDIR)', Target.UnitsOutput, [rfReplaceAll]);
  tmp := StringReplace(tmp, '$(BINOUTPUTDIR)', Target.BinOutput, [rfReplaceAll]);

  {$ifdef unix}
  tmp := StringReplace(tmp, '$(EXE)', '', [rfReplaceAll]);
  {$else}
  tmp := StringReplace(tmp, '$(EXE)', '.exe', [rfReplaceAll]);
  {$endif}

  Result := tmp;
end;

procedure install(directory, destination, pattern: string);
var
  ic: ^TInstallCommand;
begin
  ic := AllocMem(sizeof(TInstallCommand));
  ic^.directory := IncludeTrailingPathDelimiter(ExpandMacros(directory, active_target));
  ic^.destination := IncludeTrailingPathDelimiter(ExpandMacros(destination,
    active_target));
  ic^.pattern := ExpandMacros(pattern, active_target);

  active_target.Install.Add(ic);
end;

procedure target_link_libraries(Name: string; files: array of const);
var
  i: integer;
begin
  if active_target <> nil then
  begin
    for i := Low(files) to High(files) do
      active_target.dep.Add(string(files[i].VAnsiString));
    active_target.todo_dep.Text := active_target.dep.Text;
  end;
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
            '--fpc-compiler':
            begin
              if i < ParamCount then
              begin
                Inc(i);
                fpc := ParamStr(i);
                if not FileExists(fpc) then
                begin
                  writeln('error: cannot find the supplied FPC-compiler');
                  halt(1);
                end;
              end
              else
              begin
                writeln('error: please supply a valid path for the FPC-compiler');
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

  fbuild := TBuild.Create;
  fbuild.Targets := TFPList.Create;
  fbuild.sfilecount := 0;
  fbuild.progress := 0;
end;

procedure run_make;
begin
  if fbuild.projname = '' then
  begin
    writeln('error: no project defined');
    halt(1);
  end;

  case RunMode of
    rmBuild: BuildMode;
    rmClean: CleanMode;
    rmInstall: InstallMode;
  end;
end;

end.
