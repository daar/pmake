unit pmake_utilities;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  depsolver;

type
  TCmdOption = record
    name: string;
    descr: string;
  end;

  //callback function for the command_execute function
  PMAKECommandFun = procedure(line: string; active: boolean);

function UnitsOutputDir(BasePath: string): string;
function BinOutputDir(BasePath: string): string;

function command_execute(executable: string; parameters: TStrings; callback: PMAKECommandFun; HasStdOut: boolean = true): integer;
function macros_expand(str: string; pkg: pPackage = nil): string;

procedure copyfile(old, new: string);
function DeleteDirectory(const Directoryname: string; OnlyChildren: boolean): boolean;

procedure CompilerDefaults;

var
  pmakefiles: TStringList;
  pkglist: TFPList;
  depcache: TFPList;
  instlist: TFPList;
  cmd_count: longint = 0;
  verbose: boolean = False;

implementation

uses
  Process, pmake_variables, pmake_api;

function UnitsOutputDir(BasePath: string): string;
begin
  Result := macros_expand(BasePath + 'units' + DirectorySeparator +
    '$(PMAKE_HOST_SYSTEM_PROCESSOR)-$(PMAKE_HOST_SYSTEM_NAME)' + DirectorySeparator);
  if not ForceDirectories(Result) then
    messagefmt(FATAL_ERROR, 'fatal error: failed to create directory "%s"', [Result]);
end;

function BinOutputDir(BasePath: string): string;
begin
  Result := macros_expand(BasePath + 'bin' + DirectorySeparator +
    '$(PMAKE_HOST_SYSTEM_PROCESSOR)-$(PMAKE_HOST_SYSTEM_NAME)' + DirectorySeparator);
  if not ForceDirectories(Result) then
    messagefmt(FATAL_ERROR, 'fatal error: failed to create directory "%s"', [Result]);
end;

function GetCompilerInfo(const ACompiler, AOptions: string): string;
const
  BufSize = 1024;
var
  AProcess: TProcess;
  Buf: array [0..BufSize - 1] of char;
  Count: longint;
begin
  AProcess := TProcess.Create(nil);
  AProcess.Executable := ACompiler;

  if AOptions <> '' then
    AProcess.Parameters.DelimitedText := AOptions;

  AProcess.Options := [poUsePipes];
  AProcess.Execute;

  Count := AProcess.Output.Read(buf, BufSize);

  AProcess.Free;

  SetLength(Result, Count);
  Move(Buf, Result[1], Count);
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
    message(FATAL_ERROR, 'fatal error: cannot find the pascal compiler');

  set_('PMAKE_PAS_COMPILER', compiler);

  //detect compiler version/target from -i option
  infosl := TStringList.Create;
  infosl.Delimiter := ' ';

  infosl.DelimitedText := GetCompilerInfo(compiler, '-iVTPTO');

  if infosl.Count <> 3 then
    message(FATAL_ERROR, 'fatal error: compiler returns invalid information, check if fpc -iV works');

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
  Buffer: array[1..BUF_SIZE] of byte;
  sStream: TStringStream;
begin
  if verbose then
  begin
    if HasStdOut then
    begin
      if parameters = nil then
        writeln(StdOut, '-- Executing ', executable)
      else
        writeln(StdOut, '-- Executing ', executable, ' ', parameters.Text);
    end
    else
    begin
      if parameters = nil then
        writeln('-- Executing ', executable)
      else
        writeln('-- Executing ', executable, ' ', parameters.Text);
    end;
  end;

  if callback = nil then
    message(FATAL_ERROR, 'fatal error: command_execute, no callback assigned!');

  AProcess := TProcess.Create(nil);
  AProcess.Executable := executable;

  if parameters <> nil then
    AProcess.Parameters.AddStrings(parameters);

  AProcess.Options := [poUsePipes];
  AProcess.Execute;

  sStream := TStringStream.Create('');

  repeat
    BytesRead := AProcess.Output.Read(Buffer, BUF_SIZE);
    sStream.Write(Buffer, BytesRead);
    sStream.Position := 0;

    if callback <> nil then
      callback(sStream.DataString, AProcess.Running);

    sStream.Size := 0;
  until BytesRead = 0;

  Result := AProcess.ExitCode;

  //clean up
  sStream.Free;
  AProcess.Free;
end;

//expand some simple macro's
function macros_expand(str: string; pkg: pPackage = nil): string;
var
  tmp: string = '';
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
      messagefmt(FATAL_ERROR, 'fatal error: invalid use of macro $(UNITSOUTPUTDIR) in "%s"', [str]);
    if pos('$(BINOUTPUTDIR)', tmp) <> 0 then
      messagefmt(FATAL_ERROR, 'fatal error: invalid use of macro $(BINOUTPUTDIR) in "%s"', [str]);
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

end.
