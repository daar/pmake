unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, EditBtn, ButtonPanel, ExtCtrls, Menus, LCLTranslator,
  DefaultTranslator, ValEdit;

type

  { TPMakeGUIForm }

  TPMakeGUIForm = class(TForm)
    BrowseSourceButton: TButton;
    BrowseBuildButton: TButton;
    ConfigureButton: TButton;
    GenerateButton: TButton;
    ButtonPanel: TButtonPanel;
    GroupedCheckBox: TCheckBox;
    BrowseSourceEdit: TEdit;
    BrowseBuildEdit: TEdit;
    LanguagesMenuItem: TMenuItem;
    EnglishMenuItem: TMenuItem;
    DutchMenuItem: TMenuItem;
    MenuItem8: TMenuItem;
    SourceTreeLocationLabel: TLabel;
    BinaryLocationLabel: TLabel;
    MainMenu: TMainMenu;
    MessagesMemo: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    ConfigureMenuItem: TMenuItem;
    MenuItem9: TMenuItem;
    MessagePanel: TPanel;
    ConfigurePanel: TPanel;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    Splitter: TSplitter;
    OptionsValueListEditor: TValueListEditor;
    procedure BrowseSourceButtonClick(Sender: TObject);
    procedure BrowseBuildButtonClick(Sender: TObject);
    procedure ConfigureButtonClick(Sender: TObject);
    procedure LanguagesMenuItemClick(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
    procedure ButtonPanelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
  private
    { private declarations }

    function CheckAndCreateDirectory(ADirectory: string): boolean;
    procedure UpdateOptionsValueListEditor;
    procedure UpdatePMakeCache;
    procedure ParsePMakeFiles;
  public
    { public declarations }
  end;

var
  PMakeGUIForm: TPMakeGUIForm;

implementation

{$R *.lfm}

uses
  About, pmake_variables, make_main, pmake_utilities, pmake_main, compiler;

procedure GUIOutputLn(msg: string);
begin
  PMakeGUIForm.MessagesMemo.Lines.Add(msg);
end;

{ TPMakeGUIForm }

procedure TPMakeGUIForm.ButtonPanelClick(Sender: TObject);
begin
  Close;
end;

procedure TPMakeGUIForm.BrowseSourceButtonClick(Sender: TObject);
var
  BrowseSource: string;
begin
  if SelectDirectoryDialog.Execute then
  begin
    ConfigureButton.Enabled := True;
    BrowseSource := IncludeTrailingPathDelimiter(SelectDirectoryDialog.FileName);

    BrowseSourceEdit.Text := BrowseSource;

    //initialize default build folder
    BrowseBuildEdit.Text := IncludeTrailingPathDelimiter(ExpandFileName(BrowseSource + '..' + PathDelim + 'build'));
  end;
end;

procedure TPMakeGUIForm.BrowseBuildButtonClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
    BrowseBuildEdit.Text := IncludeTrailingPathDelimiter(SelectDirectoryDialog.FileName);
end;

procedure TPMakeGUIForm.ConfigureButtonClick(Sender: TObject);
begin
  if not DirectoryExists(BrowseSourceEdit.Text) then
  begin
    ShowMessage('Cannot find source directory, please specify.');
    exit;
  end;
  if not CheckAndCreateDirectory(BrowseBuildEdit.Text) then
     //folder does not exists
    exit;

  GenerateButton.Enabled := True;

  pmake_initialize(BrowseBuildEdit.Text);

  set_('PMAKE_SOURCE_DIR', BrowseSourceEdit.Text);

  ParsePMakeFiles;

  pmakecache_write;

  UpdateOptionsValueListEditor;
end;

procedure TPMakeGUIForm.GenerateButtonClick(Sender: TObject);
begin
  UpdatePMakeCache;
  pmakecache_write;

  create_and_build_make(False);
end;

procedure TPMakeGUIForm.LanguagesMenuItemClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: SetDefaultLang('en');
    1: SetDefaultLang('nl');
  end;
end;

procedure TPMakeGUIForm.FormCreate(Sender: TObject);
//var
//  menuItem: TMenuItem;
begin
  OutputLn := @GUIOutputLn;
  StdOutLn := @GUIOutputLn;
  StdErrLn := @GUIOutputLn;

  Caption := Format('PMake %s', [PMAKE_VERSION]);

  //menuItem := TMenuItem.Create(LanguagesMenuItem) ;
  //menuItem.Caption := 'Dutch';
  //menuItem.OnClick := @LanguagesMenuItemClick;
  //menuItem.Tag := 1;
  //LanguagesMenuItem.Add(menuItem) ;

  ButtonPanel.CloseButton.Cancel := True;

  //clear the GUI
  BrowseSourceEdit.Text := '';
  BrowseBuildEdit.Text := '';
  OptionsValueListEditor.Clear;
  MessagesMemo.Lines.Clear;
end;

procedure TPMakeGUIForm.MenuItem18Click(Sender: TObject);
begin
  if AboutForm = nil then
    AboutForm := TAboutForm.Create(nil);

  AboutForm.ShowModal;

  FreeAndNil(AboutForm);
end;

procedure TPMakeGUIForm.MenuItem1Click(Sender: TObject);
begin

end;

procedure TPMakeGUIForm.MenuItem5Click(Sender: TObject);
begin
  Close;
end;

function TPMakeGUIForm.CheckAndCreateDirectory(ADirectory: string): boolean;
begin
  Result := True;

  if not DirectoryExists(ADirectory) then
  begin
    if MessageDlg('Create directory',
        Format(
          'The directory %s does not exist.%sWould you like to create it?', [
          BrowseBuildEdit.Text, LineEnding]),
        mtInformation, [mbYes, mbNo], 0) = mrYes
    then
    begin
      if not CreateDir(BrowseBuildEdit.Text) then
      begin
        OutputLn(Format('Failed to create directory %s?', [BrowseBuildEdit.Text]));
        Result := False;
      end
      else
        Result := True
    end
    else
      Result := False;
  end
  else
    Result := True;
end;

procedure TPMakeGUIForm.UpdateOptionsValueListEditor;
var
  v: pPMK_variant;
begin
  OptionsValueListEditor.Clear;
  OptionsValueListEditor.FixedCols := 1;

  v := varlist.first;

  while v <> nil do
  begin
    case v^.vtype of
      ptBoolean:
        begin
          if v^.PM_Boolean then
            OptionsValueListEditor.Strings.Add(v^.name + '=True')
          else
            OptionsValueListEditor.Strings.Add(v^.name + '=False')
        end;
      ptInteger:
        OptionsValueListEditor.Strings.Add(v^.name + '=' + IntToStr(v^.PM_Integer));
      ptFloat:
        OptionsValueListEditor.Strings.Add(v^.name + '=' + FloatToStr(v^.PM_Float));
      ptString:
        OptionsValueListEditor.Strings.Add(v^.name + '=' + v^.PM_String);
    end;

    v := v^.next;
  end;

  OptionsValueListEditor.AutoSizeColumn(0);
end;

procedure TPMakeGUIForm.UpdatePMakeCache;
var
  i: integer;
  key, value: string;
begin
  for i := 1 to OptionsValueListEditor.Strings.Count - 1 do
  begin
    key := OptionsValueListEditor.Keys[i];
    value := OptionsValueListEditor.Values[key];

    case LowerCase(value) of
      'true' : set_(key, True);
      'false' : set_(key, False);
    else
      set_(key, value);
    end;
  end;
end;

procedure TPMakeGUIForm.ParsePMakeFiles;
var
  i, j: integer;
  f: TStringList;
  sline, sline2, key: string;
  p: integer;
begin
  //search all PMake.txt files in the source directory
  pmakefiles := TFPList.Create;
  search_pmake(val_('PMAKE_SOURCE_DIR'));
  pmakefiles.Sort(@ComparePath);

  (*
   * parse the PMake.txt file for all options
   * this search is just a simple, naive search
   * for the occurences of "option("
   *)
  for i := 0 to pmakefiles.Count - 1 do
  begin
    f := TStringList.Create;
    f.LoadFromFile(pPMakeItem(pmakefiles[i])^.fname);

    for j := 0 to f.Count - 1 do
    begin
      sline := f[j];

      //remove all spaced between option and (
      while sline <> sline2 do
      begin
        sline2 := sline;
        sline := StringReplace(sline, ' (', '(', []);
      end;

      //check if option function is found
      p := pos('option(', lowercase(sline));
      if p <> 0 then
      begin
        delete(sline, p, length('option('));

        //extract name
        while sline[1] <> '''' do
          delete(sline, 1, 1);
        delete(sline, 1, 1);

        p := pos('''', sline);

        key := copy(sline, 1, p - 1);

        //extract value
        if pos('_ON_', UpperCase(sline)) <> 0 then
          set_(key, True)
        else
          set_(key, False);
      end;
    end;

    f.Free;
  end;
end;

end.

