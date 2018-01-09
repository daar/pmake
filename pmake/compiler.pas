unit compiler;

{$mode objfpc}{$H+}

interface

uses
  Crt, Classes, depsolver;

type
  TMessage = (mCompiling, mDebug, mError, mFail, mHint, mInformation, mLinking,
    mNote, mOption, mUnitInfo, mUnknown, mWarning);
  TMessages = set of TMessage;

  TPMakeItem = record
    fname: string;
    startpos: integer;
    endpos: integer;
  end;
  PPMakeItem = ^TPMakeItem;

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
    text: string;
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

//add here more FPC versions
{$i fpc300.inc}

procedure UpdatePMakePostions(var FPCMsgs: TFPList; fName: string);
procedure WriteFPCCommand(fpc_msg: TFPCMessage; ShowMsg: TMessages);
function ParseFPCCommand(FPCOutput: string): TFPCMessage;

function CompilerCommandLine(pkg: pPackage; cmd: pointer): TStringList;

implementation

uses
  SysUtils, pmake_api;

procedure UpdatePMakePostions(var FPCMsgs: TFPList; fName: string);
//var
//  i: integer;
//  fpc_msg: PFPCMessage;
//  fpc_msgtype: TMessage;
//  from_file: string;
//  sep: integer;
//  lineno, j: integer;
//  fitem: PPMakeItem;
//  found: boolean;
//  tmp: string;
begin
  //from_file := ExtractFileName(fName);
  //
  //for i := 0 to FPCMsgs.Count - 1 do
  //begin
  //  fpc_msg := PFPCMessage(FPCMsgs[i]);
  //  fpc_msgtype := GetFPCMsgType(fpc_msg^.msgidx);
  //  if fpc_msgtype in [mError, mFail] then
  //  begin
  //    sep := pos(',', fpc_msg^.text);
  //    if sep > 0 then
  //    begin
  //      sep := sep - length(from_file) - 2;
  //      lineno := StrToInt(copy(fpc_msg^.text, length(from_file) + 2, sep));
  //
  //      //find the line no
  //      for j := 0 to pmakelist.Count - 1 do
  //      begin
  //        fitem := PPMakeItem(pmakelist[j]);
  //        found := False;
  //        if (fitem^.startpos <= lineno) and (fitem^.endpos >= lineno) then
  //        begin
  //          found := True;
  //          break;
  //        end;
  //      end;
  //
  //      if found then
  //      begin
  //        sep := pos(',', fpc_msg^.text);
  //        tmp := copy(fpc_msg^.text, sep, length(fpc_msg^.text) - sep + 1);
  //        fpc_msg^.text := format('%s(%d%s', [fitem^.fname, lineno - fitem^.startpos, tmp]);
  //      end;
  //    end;
  //  end;
  //end;
end;

procedure WriteFPCCommand(fpc_msg: TFPCMessage; ShowMsg: TMessages);

  function GetFPCMsgType(msgidx: integer): TMessage;
  begin
    if msgidx = -1 then
      Result := mUnknown
    else
      Result := Msg[msgidx].msgtype;
  end;

var
  fpc_msgtype: TMessage;
  sline: string;
begin
  fpc_msgtype := GetFPCMsgType(fpc_msg.msgidx);

  if fpc_msgtype in ShowMsg then
  begin
    sline := fpc_msg.text;

    if  Msg[fpc_msg.msgidx].msgtype = mCompiling then
    begin
      write(copy(sline, 1, 6));
      Delete(sline, 1, 6);
    end;
    if fpc_msg.msgidx <> -1 then
      TextColor(MsgCol[fpc_msgtype].msgcol);

    writeln(sline);

    NormVideo;
  end;
end;

function ParseFPCCommand(FPCOutput: string): TFPCMessage;
var
  snum: string;
  found: boolean;
  msgidx: integer;
  ipos: SizeInt;
begin
  found := False;
  for msgidx := Low(Msg) to High(Msg) do
  begin
    snum := Format('(%d)', [Msg[msgidx].msgno]);
    ipos := Pos(snum, FPCOutput);
    if ipos <> 0 then
    begin
      FPCOutput := StringReplace(FPCOutput, sNum + ' ', '', [rfReplaceAll]);

      found := True;
      break;
    end;
  end;

  if found then
    Result.msgidx := msgidx
  else
    Result.msgidx := -1;

  Result.text := FPCOutput;
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
      messagefmt(FATAL_ERROR, 'fatal error: cannot find dependency %s', [pPackage(pkg^.dependency[i])^.name]);

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

