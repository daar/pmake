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

const
  mAll = [mCompiling, mDebug, mError, mFail, mHint, mInformation, mLinking,
    mNote, mOption, mUnitInfo, mUnknown, mWarning];

procedure add_executable(Name: string; files: array of const);
procedure add_library(Name: string; files: array of const);
procedure add_subdirectory(Name: string);
procedure project(Name: string);
procedure run_make;
procedure target_link_libraries(Name: string; files: array of const);

function RunFPCCommand(Parameters: TStrings): TStrings;
function ParseFPCCommand(FPCOutput: TStrings): TFPList;
procedure WriteFPCCommand(FPCMsgs: TFPList; ShowMsg: TMessages; progress: double = -1);

var
  fpc: string;

implementation

uses
  SysUtils, fpmkunit, Process;

type
  TTargetType = (ttExe, ttUnit);

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
  end;

  TBuild = class
    Targets: TFPList;
    projname: string;
    sfilecount: integer;
    progress: double;
  end;

const
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

function BuildCPU: TCpu;
begin
  Result := StringToCPU(
{$I %FPCTARGETCPU%}
    );
end;

function BuildOS: TOS;
begin
  Result := StringToOS(
{$I %FPCTARGETOS%}
    );
end;

function UnitsOutputDir(BasePath: string; ACPU: TCPU; AOS: TOS): string;
begin
  Result := IncludeTrailingBackSlash(BasePath + 'units') + MakeTargetString(ACPU, AOS);
  if not ForceDirectories(Result) then
  begin
    writeln('Failed to create directory "' + Result + '"');
    halt(1);
  end;
end;

function BinOutputDir(BasePath: string; ACPU: TCPU; AOS: TOS): string;
begin
  Result := IncludeTrailingBackSlash(BasePath + 'bin') + MakeTargetString(ACPU, AOS);
  if not ForceDirectories(Result) then
  begin
    writeln('Failed to create directory "' + Result + '"');
    halt(1);
  end;
end;

procedure WriteFPCCommand(FPCMsgs: TFPList; ShowMsg: TMessages; progress: double = -1);

  function GetFPCMsgType(msgidx: integer): TMessage;
  begin
    if msgidx = -1 then
      Result := mUnknown
    else
      Result := Msg[msgidx].msgtype;
  end;

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
  fpc_msgtype : TMessage;
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
    sLine := FPCOutput[i];

    found := False;
    for msgidx := Low(Msg) to High(Msg) do
    begin
      snum := Format('(%d)', [Msg[msgidx].msgno]);
      ipos := Pos(snum, sLine);
      if ipos <> 0 then
      begin
        sLine := Copy(sLine, ipos + Length(snum), Length(sLine) - ipos - Length(snum) + 1);
        sLine := StringReplace(sLine, BasePath, '.' + DirectorySeparator, [rfReplaceAll]);
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
    Result.Add('-FE' + BinOutputDir(ATarget.ActivePath, BuildCPU, BuildOS));

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
    Result.Add('-o' + ATarget.Name);
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

  active_target.Name := Name;

  for i := Low(files) to High(files) do
  begin
    active_target.units.Add(string(files[i].VAnsiString));
    Inc(fbuild.sfilecount);
  end;
  active_target.TargetType := ttUnit;
  active_target.done := False;
  active_target.ActivePath := ActivePath;

  fbuild.Targets.Add(active_target);
end;

procedure add_subdirectory(Name: string);
begin
  if BasePath = '' then
    BasePath := Name;
  ActivePath := Name;
end;

procedure init_make;
begin
  //{$note fix this for windows, allow to add commandline parameter}
  //fpc := 'C:\Work\develop\fpc264\bin\i386-win32\fpc.exe';

  if fpc = '' then
    fpc := ExeSearch('fpc', SysUtils.GetEnvironmentVariable('PATH'));

  if fpc = '' then
  begin
    writeln('error: cannot find the fpc compiler on searchpath');
    writeln('       either set the PATH variable or use the commandline parameter -fpcbin');
    halt(1);
  end;
  fbuild := TBuild.Create;
  fbuild.Targets := TFPList.Create;
  fbuild.sfilecount := 0;
  fbuild.progress := 0;
end;

procedure project(Name: string);
begin
  fbuild.projname := Name;
end;

procedure ExecuteTarget(target: TTarget);
var
  i: integer;
  cmd: TMemoryStream;
  param: TStringList;
  fpc_out: TStrings;
  fpc_msg: TFPList;
begin
  cmd := TMemoryStream.Create;

  //build units of target
  target.UnitsOutput := UnitsOutputDir(target.ActivePath, BuildCPU, BuildOS);
  for i := 0 to target.units.Count - 1 do
  begin
    fbuild.progress += 100 / fbuild.sfilecount;
    param := CompilerCommand(target, target.ActivePath + target.units[i], ttUnit);
    //writeln(s);
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

  cmd.Free;
end;

procedure free_make;
var
  i: integer;
  target: TTarget;
begin
  //free all objects
  for i := 0 to fbuild.Targets.Count - 1 do
  begin
    target := TTarget(fbuild.Targets[i]);
    target.todo_dep.Free;
    target.dep.Free;
    target.units.Free;
    target.Free;
  end;
  fbuild.Targets.Free;
  fbuild.Free;
end;

procedure run_make;
var
  target: TTarget;
  i, j: integer;
  depname: string;
  done: integer = 0;
begin
  if fbuild.projname = '' then
  begin
    writeln('error: no project defined');
    halt(1);
  end;

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

initialization
  init_make;

finalization
  free_make;
end.
