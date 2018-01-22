unit compiler;

{$mode objfpc}{$H+}

interface

uses
  Crt, Classes, depsolver;

type
  TMessage = (mCompiling, mDebug, mError, mFail, mHint, mInformation, mLinking,
    mNote, mOption, mUnitInfo, mUnknown, mWarning);
  TMessages = set of TMessage;

  pPMakeItem = ^TPMakeItem;
  TPMakeItem = record
    fname: string;
    startpos: integer;
    endpos: integer;
  end;

  TFPCOutput = record
    msgno: integer;
    msgtype: TMessage;
  end;

  TFPCColor = record
    msgtype: TMessage;
    msgcol: integer;
  end;

  pFPCMessage = ^TFPCMessage;
  TFPCMessage = record
    msgidx: integer;
    text: string;
  end;

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

procedure UpdatePMakePostions(fpc_msg: TFPCMessage; ShowMsg: TMessages; pmakefiles: TFPList);
procedure WriteFPCCommand(fpc_msg: TFPCMessage; ShowMsg: TMessages);
function ParseFPCCommand(FPCOutput: string): TFPCMessage;

function CompilerCommandLine(pkg: pPackage; cmd: pointer): TStringList;

implementation

uses
  SysUtils, pmake_api, pmake_utilities, pmake_variables;

function GetFPCMsgType(msgidx: integer): TMessage;
begin
  if msgidx = -1 then
    Result := mUnknown
  else
    Result := Msg[msgidx].msgtype;
end;

procedure UpdatePMakePostions(fpc_msg: TFPCMessage; ShowMsg: TMessages; pmakefiles: TFPList);
var
  lineno, rowno: integer;
  errmsg: string;
  fpc_msgtype: TMessage;
  j: integer;
  fitem: pPMakeItem;
  found: boolean;
  tmp, fname: string;
  p1, p2: cardinal;
  f: TStrings;
begin
  fpc_msgtype := GetFPCMsgType(fpc_msg.msgidx);
  if fpc_msgtype in [mError, mFail] then
  begin
    p1 := pos('(', fpc_msg.text);
    if p1 > 0 then
    begin
      //line number
      p2 := pos(',', fpc_msg.text);
      lineno := StrToInt(copy(fpc_msg.text, p1 + 1, p2 - p1 - 1));

      //row number
      p1 := p2;
      p2 := pos(')', fpc_msg.text);
      rowno := StrToInt(copy(fpc_msg.text, p1 + 1, p2 - p1 - 1));

      //error message
      errmsg := copy(fpc_msg.text, p2 + 2, length(fpc_msg.text) - p2);

      //find the line no
      found := False;
      for j := 0 to pmakefiles.Count - 1 do
      begin
        fitem := pPMakeItem(pmakefiles[j]);
        if (fitem^.startpos <= lineno) and (fitem^.endpos >= lineno) then
        begin
          found := True;
          break;
        end;
      end;

      if found then
      begin
        f := TStringList.Create;
        f.LoadFromFile(fitem^.fname);

        fname := '.' + DirectorySeparator + ExtractRelativepath(val_('PMAKE_SOURCE_DIR'), fitem^.fname);
        lineno := lineno - fitem^.startpos;

        tmp := StringOfChar(' ', rowno - 1);
        OutputLn(f[lineno - 1]);
        OutputLn(tmp + '^');
        OutputLn(format(' %s%s(%d,%d) %s', [tmp, fname, lineno, rowno, errmsg]));

        if fpc_msgtype  = mFail then
          halt(1);
      end;
    end;
  end;
end;

procedure WriteFPCCommand(fpc_msg: TFPCMessage; ShowMsg: TMessages);
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

    OutputLn(sline);

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

  //output file paths
  if cmdtype = ctExecutable then
    Result.Add('-FE' + pkg^.binoutput);

  for i := 0 to pkg^.includes.Count - 1 do
    Result.Add('-Fi' + pkg^.includes[i]);

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

  //output executable name
  if cmdtype = ctExecutable then
  begin
    Result.Add(pkg^.activepath + pExecutableCommand(cmd)^.filename);
    Result.Add('-o' + pExecutableCommand(cmd)^.executable + ExtractFileExt(ParamStr(0)));
  end;

  //compile unit name
  if cmdtype = ctUnit then
    Result.Add(pkg^.activepath + pUnitCommand(cmd)^.filename);

  //force the compiler-output to be easy parseable
  Result.Add('-viq');
end;

end.

