unit compiler;

{$mode objfpc}{$H+}

interface

uses
  Crt, Classes, depsolver;

type
  TMessage = (mCompiling, mDebug, mError, mFail, mHint, mInformation, mLinking,
    mNote, mOption, mUnitInfo, mUnknown, mWarning);
  TMessages = set of TMessage;

  TFMakeItem = record
    fname: string;
    startpos: integer;
    endpos: integer;
  end;
  PFMakeItem = ^TFMakeItem;

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
  AllMessages = [mCompiling, mDebug, mError, mFail, mHint, mInformation, mLinking,
    mNote, mOption, mUnitInfo, mUnknown, mWarning];

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

procedure UpdateFMakePostions(var FPCMsgs: TFPList; fName: string);
function GetFPCMsgType(msgidx: integer): TMessage;
procedure WriteFPCCommand(FPCMsgs: TFPList; ShowMsg: TMessages; progress: double = -1);
function ParseFPCCommand(FPCOutput: TStrings; BasePath: string): TFPList;
function RunCompilerCommand(ExeName, SrcName: string): TStrings;

function CompilerCommandLine(pkg: pPackage; cmd: pointer): TStringList;

implementation

uses
  SysUtils, ufmake;

procedure UpdateFMakePostions(var FPCMsgs: TFPList; fName: string);
var
  i: integer;
  fpc_msg: PFPCMessage;
  fpc_msgtype: TMessage;
  from_file: string;
  sep: integer;
  lineno, j: integer;
  fitem: PFMakeItem;
  found: boolean;
  tmp: string;
begin
  from_file := ExtractFileName(fName);

  for i := 0 to FPCMsgs.Count - 1 do
  begin
    fpc_msg := PFPCMessage(FPCMsgs[i]);
    fpc_msgtype := GetFPCMsgType(fpc_msg^.msgidx);
    if fpc_msgtype in [mError, mFail] then
    begin
      sep := pos(',', fpc_msg^.Text);
      if sep > 0 then
      begin
        sep := sep - length(from_file) - 2;
        lineno := StrToInt(copy(fpc_msg^.Text, length(from_file) + 2, sep));

        //find the line no
        for j := 0 to fmakelist.Count - 1 do
        begin
          fitem := PFMakeItem(fmakelist[j]);
          found := False;
          if (fitem^.startpos <= lineno) and (fitem^.endpos >= lineno) then
          begin
            found := True;
            break;
          end;
        end;

        if found then
        begin
          sep := pos(',', fpc_msg^.Text);
          tmp := copy(fpc_msg^.Text, sep, length(fpc_msg^.Text) - sep + 1);
          fpc_msg^.Text :=
            format('%s(%d%s', [fitem^.fname, lineno - fitem^.startpos, tmp]);
        end;
      end;
    end;
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

function RunCompilerCommand(ExeName, SrcName: string): TStrings;
var
  param: TStrings;
begin
  param := TStringList.Create;

  param.Add('-viq');
{$ifdef debug}
  param.Add('-gh');
{$endif}
  //add the unit search path where the fmake executable is locate
  param.Add('-FU' + ExtractFilePath(ParamStr(0)));
  param.Add(SrcName);
  //based on the app extension of fmake, add the same extension to make
  param.Add(ExpandMacros('-o' + ExeName));

  Result := RunCommand(fpc, param);

  param.Free;
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

  Result.Add('-Fu' + pkg^.unitsoutput);
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
    Result.Add(pkg^.activepath + pExecutableCommand(cmd)^.filename);
    Result.Add('-o' + pExecutableCommand(cmd)^.executable + ExtractFileExt(ParamStr(0)));
  end;

  // compile unit name
  if cmdtype = ctUnit then
    Result.Add(pkg^.activepath + pUnitCommand(cmd)^.filename);

  // Force the compiler-output to be easy parseable
  //if not Verbose then
  Result.Add('-viq');
end;

end.

