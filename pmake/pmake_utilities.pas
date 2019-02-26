unit pmake_utilities;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  depsolver;

const
  _TEST_PGK_NAME_ = '@@PMAKE_TEST_PKG';

type
  TCmdOption = record
    name: string;
    descr: string;
  end;

  //callback function for the command_execute function
  PMAKECommandFun = procedure(line: string; active: boolean);

  pPMK_Link = ^PMK_Link;
  PMK_Link = record
    next, prev: pointer;
  end;

  pPMK_ListBase = ^PMK_ListBase;
  PMK_ListBase = record
    first, last: pointer;
  end;

function UnitsOutputDir(BasePath: string): string;
function BinOutputDir(BasePath: string): string;

function command_execute(executable: string; parameters: TStrings; callback: PMAKECommandFun; HasStdOut: boolean = true): integer;
function macros_expand(str: string; pkg: pPackage = nil): string;

procedure copyfile(old, new: string);
function DeleteDirectory(const Directoryname: string; OnlyChildren: boolean): boolean;

procedure CompilerDefaults;

function countlist(listbase: pPMK_ListBase): integer;
function callocN(Size: PtrUInt): pointer;
procedure remlink(vlink: pointer);
procedure addtail(listbase: pPMK_ListBase; vlink: pointer);

var
  pmakefiles: TFPList;
  pkglist: TFPList;
  depcache: TFPList;
  instlist: TFPList;
  cmd_count: longint = 0;
  verbose: boolean = False;
  OutputLn: procedure(msg: string);
  StdOutLn: procedure(msg: string);
  StdErrLn: procedure(msg: string);

implementation

uses
  Process, pmake_variables, pmake_api;

procedure CmdOutputLn(msg: string);
begin
  writeln(msg);
end;

procedure CmdStdOutLn(msg: string);
begin
  writeln(StdOut, msg);
end;

procedure CmdStdErrLn(msg: string);
begin
  writeln(StdErr, msg);
end;

function UnitsOutputDir(BasePath: string): string;
begin
  Result := macros_expand(IncludeTrailingPathDelimiter(BasePath) + 'units' + DirectorySeparator +
    '$(PMAKE_HOST_SYSTEM_PROCESSOR)-$(PMAKE_HOST_SYSTEM_NAME)' + DirectorySeparator);
  if not ForceDirectories(Result) then
    messagefmt(FATAL_ERROR, '(1009) fatal error: failed to create directory "%s"', [Result]);
end;

function BinOutputDir(BasePath: string): string;
begin
  Result := macros_expand(IncludeTrailingPathDelimiter(BasePath) + 'bin' + DirectorySeparator +
    '$(PMAKE_HOST_SYSTEM_PROCESSOR)-$(PMAKE_HOST_SYSTEM_NAME)' + DirectorySeparator);
  if not ForceDirectories(Result) then
    messagefmt(FATAL_ERROR, '(1009) fatal error: failed to create directory "%s"', [Result]);
end;

function GetCompilerInfo(const ACompiler, AOptions: string): string;
const
  BUF_SIZE = 1024;
var
  AProcess: TProcess;
  buf: array [0..BUF_SIZE - 1] of char;
  count: longint;
begin
  AProcess := TProcess.Create(nil);
  AProcess.Executable := ACompiler;

  if AOptions <> '' then
    AProcess.Parameters.DelimitedText := AOptions;

  AProcess.Options := [poUsePipes];
  AProcess.Execute;

  count := AProcess.Output.Read(buf, BUF_SIZE);

  AProcess.Free;

  SetLength(Result, count);
  Move(Buf, Result[1], count);
end;

procedure CompilerDefaults;
var
  infoSL: TStringList;
  compiler: string;
begin
  compiler := val_('PMAKE_PAS_COMPILER');

  //for now we initialize the FPC compiler, but we will add other compilers here later!!
  if compiler = '' then
    compiler := ExeSearch(macros_expand('fpc$(EXE)'), SysUtils.GetEnvironmentVariable('PATH'));

  if not FileExists(compiler) then
    message(FATAL_ERROR, '(1009) fatal error: cannot find the pascal compiler');

  set_('PMAKE_PAS_COMPILER', compiler);

  //detect compiler version/target from -i option
  infosl := TStringList.Create;
  infosl.Delimiter := ' ';

  infosl.DelimitedText := GetCompilerInfo(compiler, '-iVTPTO');

  if infosl.Count <> 3 then
    message(FATAL_ERROR, '(1009) fatal error: compiler returns invalid information, check if fpc -iV works');

  set_('PMAKE_PAS_COMPILER_VERSION', infosl[0]);
  set_('PMAKE_HOST_SYSTEM_PROCESSOR', infosl[1]);
  set_('PMAKE_HOST_SYSTEM_NAME', infosl[2]);

  infosl.Free;
end;

function command_execute(executable: string; parameters: TStrings; callback: PMAKECommandFun; HasStdOut: boolean = true): integer;
const
  BUF_SIZE = 2048;
var
  AProcess: TProcess;
  BytesRead: longint;
  buf: array[1..BUF_SIZE] of byte;
  sStream: TStringStream;
begin
  if verbose then
  begin
    if HasStdOut then
    begin
      if parameters = nil then
        StdOutLn('(5025) -- Executing ' + executable)
      else
        StdOutLn('(5025) -- Executing ' + executable + ' ' + parameters.Text);
    end
    else
    begin
      if parameters = nil then
        OutputLn('(5025) -- Executing ' + executable)
      else
        OutputLn('(5025) -- Executing ' + executable + ' ' + parameters.Text);
    end;
  end;

  if callback = nil then
    message(FATAL_ERROR, '(5025) fatal error: command_execute, no callback assigned!');

  AProcess := TProcess.Create(nil);
  AProcess.Executable := executable;

  if parameters <> nil then
    AProcess.Parameters.AddStrings(parameters);

  AProcess.Options := [poUsePipes];
  AProcess.Execute;

  sStream := TStringStream.Create('');

  repeat
    BytesRead := AProcess.Output.Read(buf, BUF_SIZE);
    sStream.Write(buf, BytesRead);
    sStream.Position := 0;

    if callback <> nil then
      callback(sStream.DataString, AProcess.Running);

    sStream.Size := 0;
  until BytesRead = 0;

  Result := AProcess.ExitStatus;

  //clean up
  sStream.Free;
  AProcess.Free;
end;

//expand some simple macro's
function macros_expand(str: string; pkg: pPackage = nil): string;
var
  tmp: string = '';
  spos, epos: integer;
begin
  tmp := str;
  replace_variable_macros(tmp);

  if pkg <> nil then
  begin
    tmp := StringReplace(tmp, '$(UNITSOUTPUTDIR)', pkg^.unitsoutput, [rfReplaceAll]);
    tmp := StringReplace(tmp, '$(BINOUTPUTDIR)', pkg^.binoutput, [rfReplaceAll]);
  end
  else
  begin
    if pos('$(UNITSOUTPUTDIR)', tmp) <> 0 then
      messagefmt(FATAL_ERROR, '(1009) fatal error: invalid use of macro $(UNITSOUTPUTDIR) in "%s"', [str]);
    if pos('$(BINOUTPUTDIR)', tmp) <> 0 then
      messagefmt(FATAL_ERROR, '(1009) fatal error: invalid use of macro $(BINOUTPUTDIR) in "%s"', [str]);
  end;

{$ifdef windows}
  tmp := StringReplace(tmp, '$(EXE)', '.exe', [rfReplaceAll]);
{$else}
  tmp := StringReplace(tmp, '$(EXE)', '', [rfReplaceAll]);
{$endif}

{$ifdef windows}
  tmp := StringReplace(tmp, '$(DLL)', '.dll', [rfReplaceAll]);
{$else}
  tmp := StringReplace(tmp, '$(DLL)', '.so', [rfReplaceAll]);
{$endif}

  //check for unknown macro's
  spos := pos('$(', tmp);
  epos := pos(')', tmp);
  if (spos > 0) and (epos > spos) then
    messagefmt(WARNING, '(2009) warning: unknown macro %s found.', [copy(tmp, spos, epos)]);

  Result := tmp;
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

function countlist(listbase: pPMK_ListBase): integer;
var
  link: pPMK_Link;
  count: integer = 0;
begin
  if listbase <> nil then
  begin
    link := listbase^.first;
    while link <> nil do
    begin
      inc(count);
      link := link^.next;
    end;
  end;

  exit(count);
end;

function callocN(Size: PtrUInt): pointer;
var
  p: pointer;
begin
  p := GetMem(Size);
  FillByte(p^, Size, 0);
  exit(p);
end;

procedure remlink(vlink: pointer);
var
  link: pPMK_Link;
begin
  link := pPMK_Link(vlink);

  if link = nil then
    exit;

  if link^.next <> nil then
    pPMK_Link(link^.next)^.prev := link^.prev;

  if link^.prev <> nil then
    pPMK_Link(link^.prev)^.next := link^.next;

  if pointer(varlist.last) = pointer(link) then
    varlist.last := link^.prev;

  if pointer(varlist.first) = pointer(link) then
    varlist.first := link^.next;
end;

procedure addtail(listbase: pPMK_ListBase; vlink: pointer);
var
  link: pPMK_Link;
begin
  link := pPMK_Link(vlink);

  if link = nil then
    exit;

  link^.next := nil;
  link^.prev := listbase^.last;

  if listbase^.last <> nil then
    pPMK_Link(listbase^.last)^.next := link;

  if listbase^.first = nil then
    listbase^.first := link;

  listbase^.last := link;
end;

initialization
  OutputLn := @CmdOutputLn;
  StdOutLn := @CmdStdOutLn;
  StdErrLn := @CmdStdErrLn;

end.
