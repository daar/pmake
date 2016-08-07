unit compiler;

{$mode objfpc}{$H+}

interface

uses
  Crt, Classes, depsolver;

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

function GetFPCMsgType(msgidx: integer): TMessage;
procedure WriteFPCCommand(FPCMsgs: TFPList; ShowMsg: TMessages; progress: double = -1);
function ParseFPCCommand(FPCOutput: TStrings; BasePath: string): TFPList;
function RunCompilerCommand(Parameters: TStrings): TStrings;

function CompilerCommandLine(pkg: pPackage; cmd: pointer): TStringList;

implementation

uses
  SysUtils, ufmake;

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
        Write(format('[%3.0f%%] ', [progress]));

      if fpc_msg.msgidx <> -1 then
        TextColor(MsgCol[fpc_msgtype].msgcol);
      writeln(fpc_msg.Text);
      NormVideo;
    end;
  end;
end;

function ParseFPCCommand(FPCOutput: TStrings; BasePath: string): TFPList;
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

function RunCompilerCommand(Parameters: TStrings): TStrings;
begin
  Result := RunCommand(fpc, Parameters);
end;

function CompilerCommandLine(pkg: pPackage; cmd: pointer): TStringList;
var
  i: integer;
  cmdtype: TCommandType;
  dep: pPackage;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;

  cmdtype := TCommandType(cmd^);

  // Output file paths
  if cmdtype = ctExecutable then
    Result.Add('-FE' + pkg^.binoutput);

  Result.Add('-FU' + pkg^.unitsoutput);

  //search other unit files for the dependencies
  for i := 0 to pkg^.dependency.Count - 1 do
  begin
    dep := pkg^.dependency[i];

    if dep = nil then
    begin
      writeln('fail: cannot find dependency ', pPackage(pkg^.dependency[i])^.name);
      halt(1);
    end;

    Result.Add('-Fu' + dep^.unitsoutput);
  end;

  // Output executable name
  if cmdtype = ctExecutable then
  begin
    Result.Add(pExecutableCommand(cmd)^.filename);
    Result.Add('-o' + pkg^.name + ExtractFileExt(ParamStr(0)));
  end;

  // compile unit name
  if cmdtype = ctUnit then
    Result.Add(pUnitCommand(cmd)^.filename);

  // Force the compiler-output to be easy parseable
  //if not Verbose then
  Result.Add('-viq');
end;

end.

