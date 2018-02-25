unit make_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure make_execute;
procedure search_pmake(const path: string; callee: string = '');

implementation

uses
  crc16,
  pmake_utilities, pmake_variables, pmake_api, compiler;

type
  TRunMode = (rmBuild, rmInstall, rmClean);

const
  CmdOptions: array[1..9] of TCmdOption = (
    (name: 'build'; descr: 'Build all targets in the project.'),
    (name: 'clean'; descr: 'Clean all units and folders in the project'),
    (name: 'install'; descr: 'Install all targets in the project.'),
    (name: 'package'; descr: 'Create a package (zip)'),
    (name: '--compiler'; descr: 'Use indicated binary as compiler'),
    (name: '--debug'; descr: 'Do not delete the make2 source file.'),
    (name: '--force'; descr: 'Force building make2.exe'),
    (name: '--help'; descr: 'This message.'),
    (name: '--verbose'; descr: 'Be more verbose.')
    );

var
  sline: TStringList;
  force_build: boolean = False;
  debug: boolean = False;
  make2_params: TStrings;

procedure output_line(var sline: TStringList);
var
  fpc_msg: TFPCMessage;
begin
  fpc_msg := ParseFPCCommand(sline[0]);

  WriteFPCCommand(fpc_msg, [mCompiling, mDebug, mError, mFail, mHint, mLinking, mNote, mOption, mUnitInfo, mWarning]);

  sline.Delete(0);
end;

//make2 execution
procedure command_callback(line: string; active: boolean);
begin
  //parse the output
  sline.Text := sline.Text + line;

  while sline.Count > 1 do
    output_line(sline);

  if not active then
  begin
    while sline.Count > 0 do
      output_line(sline);
  end;
end;

//make2 compilation
procedure make2_callback(line: string; active: boolean);
var
  fpc_msg: TFPCMessage;
begin
  if not verbose then
    exit;

  //parse the output
  sline.Text := sline.Text + line;

  while sline.Count > 1 do
  begin
    fpc_msg := ParseFPCCommand(sline[0]);
    UpdatePMakePostions(fpc_msg, [mCompiling, mDebug, mError, mFail, mHint, mLinking, mNote, mOption, mUnitInfo, mWarning], pmakefiles);
    sline.Delete(0);
  end;

  if not active then
  begin
    while sline.Count > 0 do
    begin
      fpc_msg := ParseFPCCommand(sline[0]);
      UpdatePMakePostions(fpc_msg, [mCompiling, mDebug, mError, mFail, mHint, mLinking, mNote, mOption, mUnitInfo, mWarning], pmakefiles);
      sline.Delete(0);
    end;
  end;
end;

function check_rebuild_make2: boolean;
var
  j: integer;
  f: TStrings;
  pmakecrc: word;
  found: boolean;
  fc: pPMK_ListBase;
  pm: pPMK_FileCache;
begin
  if not FileExists('PMakeCache.txt') then
    exit(True);

  if not FileExists(macros_expand('make2$(EXE)')) then
    exit(True);

  fc := VALfc('PMAKE_TXT');

  //return true if the PMake.txt is count is different
  if countlist(fc) <> pmakefiles.Count then
    exit(True);

  //return true if a crc / PMake.txt combination is not found
  f := TStringList.Create;

  pm := fc^.first;
  while pm <> nil do
  begin
    f.LoadFromFile(pm^.fname);
    pmakecrc := crc_16(@f.Text[1], length(f.Text));

    found := False;
    for j := 0 to pmakefiles.Count - 1 do
      if pPMakeItem(pmakefiles[j])^.fname = pm^.fname then
      begin
        found := true;
        break;
      end;

    //either the PMake.txt file does not exist, or the crc changed
    if (found = False) or (pm^.crc <> pmakecrc) then
    begin
      f.Free;
      exit(True);
    end;

    pm := pm^.next;
  end;
  f.Free;

  exit(False);
end;

procedure search_pmake(const path: string; callee: string = '');
var
  p: pPMakeItem;
  fn: string;
  pm: TStrings;
  line: string;
  i: integer;
  spos, epos: integer;
begin
  fn := IncludeTrailingPathDelimiter(path) + 'PMake.txt';

  if not FileExists(fn) then
  begin
    messagefmt(WARNING, 'warning: error in %sPMake.txt', ['.' + DirectorySeparator + ExtractRelativepath(val_('PMAKE_SOURCE_DIR'), callee) + DirectorySeparator]);
    messagefmt(FATAL_ERROR, 'fatal error: could not find %s', ['.' + DirectorySeparator + ExtractRelativepath(val_('PMAKE_SOURCE_DIR'), fn)]);
  end
  else
  begin
    p := GetMem(SizeOf(TPMakeItem));
    p^.fname := fn;
    pmakefiles.Add(p);

    //parse the PMake.txt file
    pm := TStringList.Create;
    pm.LoadFromFile(fn);

    //search for 'add_subdirectory'
    for i := 0 to pm.Count - 1 do
    begin
      line := LowerCase(Trim(pm[i]));
      if pos('add_subdirectory', line) = 1 then
      begin
        spos := pos('''', line) + 1;
        line := copy(Trim(pm[i]), spos, length(line));
        epos := pos('''', line) - 1;
        line := macros_expand(copy(line, 1, epos));

        //make sure the directory becomes absolute
        line := ExtractRelativepath(path, line);
        line := IncludeTrailingPathDelimiter(path) + line;

        search_pmake(line, path);
      end;
    end;

    pm.Free;
  end;
end;

procedure make2_build;
var
  make2, f: TStrings;
  src_name: string;
  i, exit_code: integer;
  param: TStrings;
  p: pPMakeItem;
begin
  OutputLn('-- Processing PMake.txt files');

  make2 := TStringList.Create;

  make2.Add('program make2;');
  make2.Add('uses {$IFDEF UNIX} cthreads, {$ENDIF} make2_main, pmake_api, pmake_variables;');
  make2.Add('begin');
  make2.Add('  init_make2;');

  //insert code from PMake.txt files
  f := TStringList.Create;
  for i := 0 to pmakefiles.Count - 1 do
  begin
    p := pPMakeItem(pmakefiles[i]);

    f.LoadFromFile(p^.fname);

    make2.Add('  add_subdirectory(''%s'');', [ExtractFilePath(p^.fname)]);
    make2.Add('//--------');

    //determine the start and end position of the PMake.txt in the source file
    p^.startpos := make2.Count + 1;

    make2.AddStrings(f);
    make2.Add('//--------');

    p^.endpos := make2.Count + 1;
end;
  f.Free;

  make2.Add('  execute_make2;');
  make2.Add('end.');

  src_name := GetTempFileName('.', 'pmake');
  make2.SaveToFile(src_name);

  param := TStringList.Create;
  param.Add('-viq');

  //add the unit search path where the pmake executable is locate
  param.Add('-FU' + UnitsOutputDir(val_('PMAKE_BINARY_DIR')));
  param.Add('-Fu' + val_('PMAKE_TOOL_DIR'));
  param.Add(src_name);
  param.Add(macros_expand('-omake2$(EXE)'));

  sline := TStringList.Create;
  exit_code := command_execute(val_('PMAKE_PAS_COMPILER'), param, @make2_callback, false);
  sline.Free;

  param.Free;

  //remove the object and source files
  if verbose then
    OutputLn('-- Deleting temporary files');

  if not debug then
    DeleteFile(src_name);

  DeleteFile(ChangeFileExt(src_name, '.o'));

  if exit_code <> 0 then
    messagefmt(FATAL_ERROR, 'fatal error: cannot compile %s', [macros_expand('make2$(EXE)')]);
end;

procedure usage;
var
  i: integer;
begin
  OutputLn('PMake the pascal build tool. Version ' + PMAKE_VERSION + ' [' +
{$I %DATE%}
    + '] for ' +
{$I %FPCTARGETCPU%}
    );
  OutputLn('Copyright (c) 2016-2017 by Darius Blaszyk');
  OutputLn('');
  OutputLn('Usage ');
  OutputLn('  make [options] <path-to-source>');
  OutputLn('');
  OutputLn('Options');

  for i := low(CmdOptions) to high(CmdOptions) do
    OutputLn(Format('  %-16s %s', [CmdOptions[i].name, CmdOptions[i].descr]));

  halt(1);
end;

procedure parse_commandline;
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
          found := True;

          case CmdOptions[j].name of
            'build': make2_params.Add('build');
            'clean': make2_params.Add('clean');
            'install': make2_params.Add('install');
            'package':
            begin
              make2_params.Add('package');
              inc(i);
              make2_params.Add(ParamStr(i));
            end;
            '--compiler':
            begin
              if i < ParamCount then
              begin
                Inc(i);
                set_('PMAKE_PAS_COMPILER', ParamStr(i));
                if not FileExists(ParamStr(i)) then
                  message(FATAL_ERROR, 'fatal error: cannot find the supplied compiler');
              end
              else
              begin
                StdErrLn('error: please supply a valid path for the compiler');
                usage;
              end;
            end;
            '--force': force_build := True;
            '--debug': debug := True;
            '--help': usage;
            '--verbose': verbose := True;
          end;
        if found then
          break;
      end;
    end;

    if not found then
    begin
      StdErrLn('error: invalid commandline parameter ' + ParamStr(i));
      usage;
    end;

    Inc(i);
  end;
end;

//a custom sort for the pmake file list
function ComparePath(Item1: Pointer; Item2: Pointer): Integer;
var
  path1, path2: string;
begin
  path1 := ExtractFilePath(pPMakeItem(Item1)^.fname);
  path2 := ExtractFilePath(pPMakeItem(Item2)^.fname);

  if path1 < path2 then
    Result := -1
  else
    if path1 = path2 then
      Result := 0
    else
      Result := 1;
end;

procedure make_execute;
var
  exit_code: integer;
begin
  pmakecache_read;

  make2_params := TStringList.Create;
  parse_commandline;

  pmakefiles := TFPList.Create;
  search_pmake(val_('PMAKE_SOURCE_DIR'));
  pmakefiles.Sort(@ComparePath);

  if check_rebuild_make2 or force_build then
    make2_build;

  //save PMakeCache for make2 to use it
  pmakecache_write;

  sline := TStringList.Create;
  exit_code := command_execute(macros_expand('$(PMAKE_BINARY_DIR)make2$(EXE)', nil), make2_params, @command_callback, false);

  make2_params.Free;
  sline.Free;

  pmakefiles.Free;

  if exit_code <> 0 then
    messagefmt(FATAL_ERROR, 'fatal error: cannot execute %s', [macros_expand('make2$(EXE)')]);
end;

end.

