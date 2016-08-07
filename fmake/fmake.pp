program fmake;

{$mode objfpc}{$H+}
{ $define debug}

uses 
{$IFDEF UNIX}
  cthreads, 
{$ENDIF} 
{$ifdef debug}
  HeapTrc, 
{$endif}
  Classes, SysUtils,
  ufmake, compiler;

type
  TFMakeItem = record
    fname: string;
    startpos: integer;
    endpos: integer;
  end;
  PFMakeItem = ^TFMakeItem;

var
  fname: string = '';
  make: TStrings;
  BasePath: string = '';
  fmakelist: TFPList;

  procedure FileSearch(const path: string);
  var
    info: TSearchRec;
    infile: TStrings;
    fitem: PFMakeItem;
  begin
    if FindFirst(path + '*', faAnyFile, info) = 0 then
    begin
      try
        repeat
          if (info.Attr and faDirectory) = 0 then
          begin
            //add FMake.txt to the compile 'script'
            if info.Name = 'FMake.txt' then
            begin
              fitem := getmem(sizeof(TFMakeItem));
              fmakelist.Add(fitem);

              fitem^.fname := StringReplace(path + info.Name, BasePath,
                '.' + DirectorySeparator, [rfReplaceAll]);
              writeln('-- Found ', fitem^.fname);

              make.Add('  //' + path + info.Name);
              make.Add('  add_subdirectory(''' + ExtractFilePath(path + info.Name) + ''');');

              fitem^.startpos := make.Count;

              infile := TStringList.Create;
              infile.LoadFromFile(path + info.Name);

              make.AddStrings(infile);

              fitem^.endpos := make.Count;

              infile.Free;
            end;
          end
          else
          //start the recursive search
          if (info.Name <> '.') and (info.Name <> '..') then
            FileSearch(IncludeTrailingBackSlash(path + info.Name));

        until FindNext(info) <> 0
      finally
        FindClose(info);
      end;
    end;
  end;

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

var
  fpc_out: TStrings;
  param: TStrings;
  fpc_msg: TFPList;
  fitem: PFMakeItem;
  ShowMsg: TMessages;
  i: integer;

begin
  check_options(ctFMake);

  if verbose then
    writeln('-- FPC compiler ', fpc);

  if (fpc = '') or (not FileExists(fpc)) then
  begin
    writeln('error: cannot find the FPC compiler');
    usage(ctFMake);
  end;

  fname := GetTempFileName('.', 'fmake');

  make := TStringList.Create;

  make.Add('program make;');
  make.Add('uses ufmake;');
  make.Add('begin');
  make.Add('  check_options(ctMake);');
  make.Add('  init_make;');

  BasePath := IncludeTrailingBackSlash(GetCurrentDir);
  make.Add('  add_subdirectory(''' + BasePath + ''');');

  fmakelist := TFPList.Create;
  FileSearch(BasePath);

  make.Add('  run_make;');
  make.Add('  free_make;');
  make.Add('end.');
  make.SaveToFile(fname);

  param := TStringList.Create;
  param.Add('-viq');

  //build the makefile

  {$ifdef debug}
  param.Add('-gh');
  {$endif}

  param.Add(fname);

  //based on the app extension of fmake, add the same extension to make
  param.Add('-omake' + ExtractFileExt(ParamStr(0)));

  fpc_out := RunCompilerCommand(param);
  param.Free;

  fpc_msg := ParseFPCCommand(fpc_out, BasePath);
  fpc_out.Free;

  UpdateFMakePostions(fpc_msg, fname);

  for i := 0 to fmakelist.Count - 1 do
  begin
    fitem := PFMakeItem(fmakelist[i]);
    freemem(fitem);
  end;
  fmakelist.Free;

  if verbose then
    ShowMsg := [mCompiling, mDebug, mError, mFail, mHint, mInformation,
    mLinking, mNote, mOption, mUnitInfo, mUnknown, mWarning]
  else
    ShowMsg := [mFail, mError];

  WriteFPCCommand(fpc_msg, ShowMsg);
  fpc_msg.Free;

  //remove the object and source files
  if verbose then
    writeln('-- Deleting temporary files');

  {$ifndef debug}
  DeleteFile(fname);
  {$endif}
  DeleteFile(ChangeFileExt(fname, '.o'));

  writeln('-- Generating done');
  writeln('-- Build file has been written to: ', ExpandFileName('make' + ExtractFileExt(ParamStr(0))));
end.
