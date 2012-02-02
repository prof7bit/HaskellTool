unit console;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, SynEdit, Forms, LCLType;

type

  { TConsole }

  TConsole = class(TThread)
    constructor Create(ASynEdit: TSynEdit);
    procedure SendCommand(ACommand: String);
  strict protected
    Proc : TProcess;
    Ed: TSynEdit;
    EndOfPrompt: Integer;
    Buf : Array[0..1024] of Char;
    procedure OnEnter;
    function GetUserInput: String;
    procedure HandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure StartProcess;
    procedure MoveToEnd;
    procedure SendLineToProc(ALine: String);
    procedure Execute; override;
    procedure BufferIntoEdit;
  end;


implementation

{ TConsole }

constructor TConsole.Create(ASynEdit: TSynEdit);
begin
  Ed := ASynEdit;
  Ed.Options := Ed.Options - [eoScrollPastEol, eoTrimTrailingSpaces]
                           + [eoTabsToSpaces];
  Ed.OnKeyDown := @HandleKeyDown;
  Inherited Create(False);
end;

procedure TConsole.SendCommand(ACommand: String);
begin
  Ed.SelStart := EndOfPrompt;
  Ed.SelEnd := Length(Ed.Text);
  Ed.SelText := ACommand + LineEnding;
  OnEnter;
end;

procedure TConsole.OnEnter;
begin
  SendLineToProc(GetUserInput);
end;

procedure TConsole.StartProcess;
begin
  Proc := TProcess.Create(nil);
  Proc.Options := [poUsePipes, poNoConsole, poStderrToOutPut];
  Proc.CommandLine := 'ghci';
  Proc.Execute;
end;

procedure TConsole.MoveToEnd;
begin
  Ed.SelStart := Length(Ed.Text);
end;

function TConsole.GetUserInput: String;
begin
  Ed.SelStart := EndOfPrompt;
  Ed.SelEnd := Length(Ed.Text);
  Result :=  Trim(Ed.SelText);
  MoveToEnd;
end;

procedure TConsole.SendLineToProc(ALine: String);
begin
  ALine := ALine + LineEnding;
  Proc.Input.Write(ALine[1], Length(ALine));
end;

procedure TConsole.BufferIntoEdit;
begin
  Ed.BeginUpdate(False);
  MoveToEnd;
  Ed.InsertTextAtCaret(String(PChar(@Buf)));
  EndOfPrompt := Ed.SelEnd;
  Ed.CaretX := 0; // force x-scrollbar to the left end
  Ed.EndUpdate;
  MoveToEnd;
end;

procedure TConsole.HandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    OnEnter;
  end;

  if Key = VK_UP then begin
    Key := VK_NONAME;
    exit;
  end;

  if Key = VK_DOWN then begin
    Key := VK_NONAME;
    exit;
  end;

  // somewhere inside the text, before the edit line
  if Ed.SelStart < EndOfPrompt then begin
    MoveToEnd;
    exit;
  end;

  // exactly at beginning of edit line (right after the prompt)
  if Ed.SelStart = EndOfPrompt then begin
    case Key of
      VK_LEFT, VK_BACK: Key := VK_NONAME;
    end;
  end;

  // exactly at the end of line
  if Ed.SelStart = Length(Ed.Text) then begin
    case Key of
      VK_RIGHT:  Key := VK_NONAME;
      VK_LEFT: exit;
    end;
  end;
end;

procedure TConsole.Execute;
var
  NumBytes: Integer;
begin
  StartProcess;
  repeat
    NumBytes:= Proc.Output.Read(Buf, 1024);
    Buf[NumBytes] :=Char(0);
    Synchronize(@BufferIntoEdit);
  until False
end;

end.
