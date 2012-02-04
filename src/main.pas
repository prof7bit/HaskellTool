{ HaskellTool - A simple Haskell code editor and shell

  Copyright (C) 2012 Bernd Kreuss <prof7bit@googlemail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynEditKeyCmds,
  SynHighlighterAny, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ExtCtrls, Console, highlighthaskell;

const
  C_ACTIVE = TColor($ffeedd);
  C_INACTIVE = clBtnFace;
  FONT_NAME = 'Envy Code R';
  FONT_SIZE = 10;

type

  { TTabSheetEx }

  TTabSheetEx = class(TTabSheet)
    constructor Create(AOwner: TComponent); override;
    procedure OpenFile(AName: String);
    procedure SaveFile(AName: String);
    procedure SaveFile;
  private
    Ed: TSynEdit;
    FileName: String;
  end;


  { TForm1 }

  TForm1 = class(TForm)
    ConsoleEdit: TSynEdit;
    DOpen: TOpenDialog;
    DSave: TSaveDialog;
    MReload: TMenuItem;
    PageControl: TPageControl;
    MRunMain: TMenuItem;
    MGhc: TMenuItem;
    MLoad: TMenuItem;
    MNew: TMenuItem;
    MSaveAs: TMenuItem;
    MFile: TMenuItem;
    MOpen: TMenuItem;
    MSave: TMenuItem;
    MExit: TMenuItem;
    MM: TMainMenu;
    Sb: TStatusBar;
    Splitter1: TSplitter;
    Highlighter: TSynHaskell;
    procedure ConsoleEditMouseEnter(Sender: TObject);
    procedure ConsoleEditMouseLeave(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GHCReload(Sender: TObject);
    procedure GHCRunMain(Sender: TObject);
    procedure GHCLoadCurrentFile(Sender: TObject);
    procedure MOpenClick(Sender: TObject);
    procedure MSaveAsClick(Sender: TObject);
    procedure MSaveClick(Sender: TObject);
    procedure MExitClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PageControlCloseTabClicked(Sender: TObject);
    procedure AddTab(AFileName: String);
    procedure CloseTab(ATab: TTabSheet);
    procedure OpenFile(AFileName: String);
    procedure SetEditorColor(AColor: TColor);
  private
    ConsoleWrapper: TConsole;
  public
    { public declarations }
  end; 

var
  Form1: TForm1;

implementation

{ TTabSheetEx }

constructor TTabSheetEx.Create(AOwner: TComponent);
var
  I : Integer;
begin
  inherited Create(AOwner);
  Caption := 'Unnamed';
  FileName := '';
  Ed := TSynEdit.Create(self);
  Ed.Parent := Self;
  Ed.Align := alClient;

  Ed.Color := C_ACTIVE;
  Ed.Font.Name := FONT_NAME;
  Ed.Font.Size := FONT_SIZE;

  // Synedit Ctrl+M (Line Break) would collide with
  // our menu shortcut for :main so we simply remove it
  I := Ed.Keystrokes.FindKeycode(Ord('M'), [ssCtrl]);
  if I <> -1 then
      Ed.Keystrokes.Items[I].ShortCut := 0;

  Ed.Options := Ed.Options - [eoScrollPastEol]
                           + [eoTabsToSpaces];

  Ed.Gutter.MarksPart.Visible := False;

  Ed.Highlighter := Form1.Highlighter;
end;

procedure TTabSheetEx.OpenFile(AName: String);
begin
  if AName = '' then exit;
  FileName := AName;
  Caption := ExtractFileName(AName);
  Ed.Lines.LoadFromFile(AName);
  PageControl.ActivePageIndex := PageIndex;
end;

procedure TTabSheetEx.SaveFile(AName: String);
begin
  FileName := AName;
  Caption := ExtractFileName(FileName);
  SaveFile;
end;

procedure TTabSheetEx.SaveFile;
begin
  Ed.Lines.SaveToFile(FileName);
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Highlighter := TSynHaskell.Create(self);

  ConsoleWrapper := TConsole.Create(ConsoleEdit);
  ConsoleEdit.Color := C_INACTIVE;
  ConsoleEdit.Font.Name := FONT_NAME;
  ConsoleEdit.Font.Size := FONT_SIZE;

  ConsoleEdit.Gutter.MarksPart.Visible := False;

  AddTab('');
end;

procedure TForm1.ConsoleEditMouseEnter(Sender: TObject);
begin
  ConsoleEdit.SetFocus;
  ConsoleEdit.Color := C_ACTIVE;
  SetEditorColor(C_INACTIVE);
end;

procedure TForm1.ConsoleEditMouseLeave(Sender: TObject);
begin
  TTabSheetEx(PageControl.ActivePage).Ed.SetFocus;
  ConsoleEdit.Color := C_INACTIVE;
  SetEditorColor(C_ACTIVE);
end;

procedure TForm1.GHCReload(Sender: TObject);
begin
  MSaveClick(Sender);
  ConsoleWrapper.SendCommand(':r');
end;

procedure TForm1.GHCLoadCurrentFile(Sender: TObject);
var
  FN : String;
begin
  MSaveClick(Sender);
  FN := TTabSheetEx(PageControl.ActivePage).FileName;
  if FN <> '' then begin
    ConsoleWrapper.SendCommand(':load ' + FN);
  end;
end;

procedure TForm1.GHCRunMain(Sender: TObject);
begin
  ConsoleWrapper.SendCommand(':main');
end;

procedure TForm1.MOpenClick(Sender: TObject);
begin
  if DOpen.Execute then begin
    OpenFile(DOpen.FileName);
    DSave.InitialDir := DOpen.InitialDir;
  end;
end;

procedure TForm1.MSaveAsClick(Sender: TObject);
begin
  DSave.FileName := TTabSheetEx(PageControl.ActivePage).FileName;
  if DSave.Execute then begin
    TTabSheetEx(PageControl.ActivePage).SaveFile(DSave.FileName);
    DOpen.InitialDir := DSave.InitialDir;
  end;
end;

procedure TForm1.MSaveClick(Sender: TObject);
begin
  if TTabSheetEx(PageControl.ActivePage).FileName = '' then begin
    MSaveAsClick(Sender);
  end else begin
    TTabSheetEx(PageControl.ActivePage).SaveFile;
  end;
end;

procedure TForm1.MExitClick(Sender: TObject);
begin
  Close
end;

procedure TForm1.PageControlChange(Sender: TObject);
begin
  Caption := TTabSheetEx(PageControl.ActivePage).Caption + ' - HaskellTool';
end;

procedure TForm1.PageControlCloseTabClicked(Sender: TObject);
begin
  if PageControl.PageCount > 1 then
    CloseTab(TTabSheet(Sender))
  else begin
    with TTabSheetEx(Sender) do begin
      FileName := '';
      Caption := 'Unnamed';
      Ed.ClearAll;
    end;
    PageControlChange(nil);
  end;
end;

procedure TForm1.AddTab(AFileName: String);
var
  Sheet: TTabSheetEx;
begin
  Sheet := TTabSheetEx.Create(PageControl);
  Sheet.PageControl := PageControl;
  Sheet.OpenFile(AFileName);
end;

procedure TForm1.CloseTab(ATab: TTabSheet);
begin
  ATab.Free;
end;

procedure TForm1.OpenFile(AFileName: String);
var
  I : Integer;
begin
  // only one tab? check if it is empty
  if (PageControl.PageCount = 1)
  and (TTabSheetEx(PageControl.Pages[0]).FileName = '')
  then begin
    TTabSheetEx(PageControl.Pages[0]).OpenFile(AFileName);
    PageControlChange(nil);
  end

  // otherwise search for a tab with the same filename
  else begin
    for I := 0 to PageControl.PageCount-1 do begin
      if TTabSheetEx(PageControl.Pages[i]).FileName = AFileName then
      begin
        PageControl.ActivePageIndex := I;
        exit;
      end;
    end;

    // nothing found, create a new tab
    AddTab(AFileName);
  end;
end;

procedure TForm1.SetEditorColor(AColor: TColor);
var
  I : Integer;
begin
  for I := 0 to Pred(PageControl.PageCount) do begin
    TTabSheetEx(PageControl.Pages[I]).Ed.Color := AColor;
  end;
end;

end.

