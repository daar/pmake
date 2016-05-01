unit ufmake;

{$mode objfpc}{$H+}
{ $define debug}

interface

uses
  {$IFDEF UNIX}
  cthreads;

  {$ENDIF}

procedure add_executable(Name: string; files: array of const);
procedure add_library(Name: string; files: array of const);
procedure add_subdirectory(Name: string);
procedure init_make;
procedure project(Name: string);
procedure run_make;
procedure target_link_libraries(Name: string; files: array of const);

implementation

uses
  Crt, SysUtils, Classes, fpmkunit, Process;

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

var
  fbuild: TBuild;
  active_target: TTarget;
  ActivePath: string;
  BasePath: string = '';
  fpc: string;

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

Function UnitsOutputDir(BasePath: string; ACPU: TCPU; AOS: TOS): String;
begin
  Result := IncludeTrailingBackSlash(BasePath + 'units') + MakeTargetString(ACPU, AOS);
  if not ForceDirectories(Result) then
  begin
    writeln('Failed to create directory "' + Result + '"');
    halt(1);
  end;
end;

Function BinOutputDir(BasePath: string; ACPU: TCPU; AOS: TOS): String;
begin
  Result := IncludeTrailingBackSlash(BasePath + 'bin') + MakeTargetString(ACPU, AOS);
  if not ForceDirectories(Result) then
  begin
    writeln('Failed to create directory "' + Result + '"');
    halt(1);
  end;
end;

function FindDepTarget(Dependency: string): TTarget;
var
  i: Integer;
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

Function CompilerCommand(ATarget: TTarget; Source: string; TType: TTargetType): String;
Var
  Args: TStringList;
  i: Integer;
  targ: TTarget;
begin
  Args := TStringList.Create;
  Args.Duplicates := dupIgnore;

  Result := '';

  // Output file paths
  If TType = ttExe then
    Args.Add('-FE' + BinOutputDir(ATarget.ActivePath, BuildCPU, BuildOS));

  Args.Add('-FU' + ATarget.UnitsOutput);

  //search other unit files for the dependencies
  for i := 0 to ATarget.Dep.Count - 1 do
  begin
    targ := FindDepTarget(ATarget.Dep[i]);

    if targ = nil then
    begin
      writeln('fail: cannot find dependency ', ATarget.Dep[i]);
      halt(1);
    end;

    Args.Add('-Fu' + targ.UnitsOutput);
  end;

  // Output executable name
  If TType = ttExe then
  begin
    Args.Add(Source);
    Args.Add('-o' + ATarget.Name);
  end;

  // compile unit name
  If TType = ttUnit then
    Args.Add(Source);

  // Force the compiler-output to be easy parseable
  //if not Verbose then
  args.Add('-viq');

  // Convert to string
  Result := '';
  for i := 0 to Args.Count - 1 do
    Result := Result + ' ' + Args[i];

  Args.Free;
end;

procedure add_executable(Name: string; files: array of const);
var
  i: Integer;
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
  i: Integer;
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
  fpc := ExeSearch('fpc', SysUtils.GetEnvironmentVariable('PATH'));

  fbuild := TBuild.Create;
  fbuild.Targets := TFPList.Create;
  fbuild.sfilecount := 0;
  fbuild.progress := 0;

  //create the unit output path if needed
end;

procedure project(Name: string);
begin
  fbuild.projname := Name;
end;

function ExecFPC(Verbose: boolean; const Path: string; const ComLine: string;
  const Env: TStrings; ConsoleOutput: TMemoryStream): integer;
var
  P: TProcess;
  BytesRead: longint;

  function ReadFromStream(const ReadFromStdErr: boolean): longint;

  const
    READ_BYTES = 2048;

  type
    TMessages = (mCompiling, mLinking, mFatal);

  var
    //ifdef the MsgNum so it contains the correct message numbers for each compiler version.
    MsgNum: array [TMessages] of integer = (3104, 9015, 10022);

    n, available: longint;
    BuffPos: longint;
    sLine: string;
    ch: char;
    msg: TMessages;
    ipos: integer;
    snum: string;
  begin

    // try reading it
    if ReadFromStdErr then
    begin
      available := P.Stderr.NumBytesAvailable;
      // make sure we have room
      if (bytesRead + Available > ConsoleOutput.Size) then
        ConsoleOutput.SetSize(BytesRead + Available);
      n := P.Stderr.Read((ConsoleOutput.Memory + BytesRead)^, available);
    end
    else
    begin
      available := P.Output.NumBytesAvailable;
      // make sure we have room
      if (bytesRead + Available > ConsoleOutput.Size) then
        ConsoleOutput.SetSize(BytesRead + Available);
      n := P.Output.Read((ConsoleOutput.Memory + BytesRead)^, available);
    end;
    if n > 0 then
    begin
      Inc(BytesRead, n);

      sLine := '';
      BuffPos := ConsoleOutput.Position;

      //read lines from the stream
      repeat
        ConsoleOutput.Read(ch, 1);

        if ch in [#10, #13] then
        begin
          if Verbose then
            writeln(sLine)
          else
          begin
            for msg := Low(TMessages) to High(TMessages) do
            begin
              snum := Format('(%d)', [MsgNum[msg]]);
              ipos := Pos(snum, sLine);
              if ipos <> 0 then
              begin
                sLine := StringReplace(sLine, BasePath,
                  IncludeTrailingBackSlash('.'), [rfReplaceAll]);

                case msg of
                  mCompiling:
                  begin
                    Write(format('[%3.0f%%]', [fbuild.progress]));
                    TextColor(Green);
                  end;
                  mLinking: TextColor(Red);
                  mFatal:
                  begin
                    TextColor(White);
                    writeln(sLine);
                    halt(1);
                  end;
                end;
                writeln(Copy(sLine, ipos + Length(snum), Length(sLine) -
                  ipos - Length(snum) + 1));

                NormVideo;
              end;
            end;
          end;
          if (LineEnding = #13#10) and (ch = #13) and
            (ConsoleOutput.Position < BytesRead) then
          begin
            ConsoleOutput.Read(ch, 1);
            if ch = #10 then
              sLine := ''
            else
              sLine := ch;
          end
          else
            sLine := '';
          BuffPos := ConsoleOutput.Position;
        end
        else
          sLine := sLine + ch;

      until ConsoleOutput.Position >= BytesRead;

      ConsoleOutput.Position := BuffPos;
    end;

    Result := n;
  end;

begin
  fbuild.progress += 100 / fbuild.sfilecount;

  Result := -1;
  BytesRead := 0;
  P := TProcess.Create(nil);
  try
    P.CommandLine := Path + ' ' + ComLine;
    if assigned(Env) then
      P.Environment.Assign(Env);

    P.Options := [poUsePipes];

    P.Execute;
    while P.Running do
    begin
      // Only call ReadFromStream if Data from corresponding stream
      // is already available, otherwise, on  linux, the read call
      // is blocking, and thus it is not possible to be sure to handle
      // big data amounts bboth on output and stderr pipes. PM.
      if P.Output.NumBytesAvailable > 0 then
        ReadFromStream(False)
      else if P.StdErr.NumBytesAvailable > 0 then
        ReadFromStream(True)
      else
        // no data, wait 100 ms
        Sleep(100);
    end;

    // read last part
    repeat
    until ReadFromStream(False) = 0;

    // read stderr
    // JvdS: Note that this way stderr is added to the end of the stream. But I
    // see no way showing the stderr output at the place it was actually written
    repeat
    until ReadFromStream(True) = 0;
    ConsoleOutput.SetSize(BytesRead);

    Result := P.ExitStatus;
  finally
    P.Free;
  end;
end;

procedure ExecuteFPC(target: TTarget);
var
  i: Integer;
  s: String;
  cmd: TMemoryStream;
begin
  cmd := TMemoryStream.Create;

  //build units of target
  target.UnitsOutput := UnitsOutputDir(target.ActivePath, BuildCPU, BuildOS);
  for i := 0 to target.units.Count - 1 do
  begin
    s := CompilerCommand(target, target.ActivePath + target.units[i], ttUnit);
    writeln(s);
    ExecFPC(False, fpc, s, nil, cmd);
  end;

  //build exe
  if target.TargetType = ttExe then
  begin
    s := CompilerCommand(target, target.Executable, ttExe);
    ExecFPC(False, fpc, s, nil, cmd);
  end;

  writeln('Built target ', target.Name);

  cmd.Free;
end;

procedure run_make;
var
  target: TTarget;
  i, j: Integer;
  depname: String;
  done: integer = 0;
begin
  if fbuild.projname = '' then
  begin
    writeln('error: no project defined');
    halt(1);
  end;

  try
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

      ExecuteFPC(target);

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
  finally
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

end.
