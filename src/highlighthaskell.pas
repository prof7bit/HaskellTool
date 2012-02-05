{ Simple (probably incomplete) Syntax
  highlighter for Haskell source code

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

unit highlighthaskell;
{$mode objfpc}{$H+}

interface
uses
  Graphics, Classes, sysutils, SynEditHighlighter, contnrs;

type

  { TSynHaskell }

  TSynHaskell = class(TSynCustomHighlighter)
    constructor Create(AOwner: TComponent); override;

  private
    FKeywordList: TFPHashList;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FOperAttri: TSynHighlighterAttributes;
    FBracAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FKeywordAttri: TSynHighlighterAttributes;

    FLineText: String;
    FTokenAttr: TSynHighlighterAttributes;
    FTokenPos, FTokenEnd: Integer;
    FCurRange: PtrInt;

    procedure SetupAttributes;
    procedure SetupKeywordList;
    function AddAttr(AName: String; AColor: TColor; AStyle: TFontStyles = []): TSynHighlighterAttributes;
    function IsKeyword(const AKeyword: string): boolean; override;
    procedure FindTokenEnd;

  public
    destructor Destroy; override;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    procedure Next; override;
    function GetEol: Boolean; override;
    function GetToken: String; override;
    function GetTokenPos: Integer; override;
    function GetTokenKind: integer; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function GetRange: Pointer; override;

  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property OperAttri: TSynHighlighterAttributes read FOperAttri write FOperAttri;
    property BracAttri: TSynHighlighterAttributes read FBracAttri write FBracAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property KeywordAttri: TSynHighlighterAttributes read FKeywordAttri write FKeywordAttri;

  end;

implementation
type
  TCharSet = set of Char;

const
  C_SPACE = [#0..#32];
  C_BRAC = ['(','{','[',']','}',')'];
  C_LETTER = ['a'..'z','A'..'Z','_'];
  C_DIGIT = ['0'..'9'];
  C_OPER = [#0..#255] - C_SPACE - C_BRAC - C_LETTER - C_DIGIT;
  N_KEYWORDS = 29;
  S_KEYWORDS : array [1..N_KEYWORDS] of String =
    ( 'as'
    , 'case','of'
    , 'class'
    , 'data'
    , 'default'
    , 'deriving'
    , 'do'
    , 'forall'
    , 'foreign'
    , 'hiding'
    , 'if', 'then', 'else'
    , 'import'
    , 'infix', 'infixl', 'infixr'
    , 'instance'
    , 'let', 'in'
    , 'mdo'
    , 'module'
    , 'newtype'
    , 'proc'
    , 'qualified'
    , 'rec'
    , 'type'
    , 'where'
    );

constructor TSynHaskell.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetupKeywordList;
  SetupAttributes;
end;

destructor TSynHaskell.Destroy;
begin
  FKeywordList.Free;
  inherited Destroy;
end;

procedure TSynHaskell.SetupAttributes;
begin
  FCommentAttri := AddAttr('comment', TColor($ff0000));
  FNumberAttri := AddAttr('number', TColor($009900));
  FOperAttri := AddAttr('operator', TColor($000099));
  FBracAttri := AddAttr('bracket', TColor($000099));
  FStringAttri := AddAttr('string', TColor($999900));
  FKeywordAttri := AddAttr('keyword', TColor($000000), [fsBold]);
  FIdentifierAttri := AddAttr('ident', TColor($000000));
  FSpaceAttri := AddAttr('space', TColor($000000));
end;

procedure TSynHaskell.SetupKeywordList;
var
  I: Integer;
begin
  FKeywordList := TFPHashList.Create;
  for I := 1 to N_KEYWORDS do begin
    FKeywordList.Add(S_KEYWORDS[i], PChar(S_KEYWORDS[i]));
  end;
end;

function TSynHaskell.AddAttr(AName: String; AColor: TColor; AStyle: TFontStyles = []): TSynHighlighterAttributes;
begin
  Result := TSynHighlighterAttributes.Create(AName, AName);
  Result.Foreground := AColor;
  Result.Style := AStyle;
  AddAttribute(Result);
end;

function TSynHaskell.IsKeyword(const AKeyword: string): boolean;
begin
  Result := (FKeywordList.FindIndexOf(AKeyword) > -1);
end;

procedure TSynHaskell.FindTokenEnd;
var
  l: Integer;

  procedure ScanUntil(Chars: TCharSet);
  begin
    repeat
      inc(FTokenEnd);
    until (FTokenEnd > l) or (FLineText[FTokenEnd] in Chars) ;
  end;

  procedure ScanUntilNot(Chars: TCharSet);
  begin
    repeat
      inc(FTokenEnd);
    until (FTokenEnd > l) or not (FLineText[FTokenEnd] in Chars) ;
  end;

  // scan until '{-' or '-}'
  // this is only used when already insinde such
  // a comment to determine whether anoter nested
  // comment starts or the comment is ending
  function ScanUntilComment: Boolean;
  begin
    Result := False;
    repeat
      inc(FTokenEnd);
      if (FLineText[FTokenEnd-1] = '-')
      and (FLineText[FTokenEnd] = '}')
      then exit(True);
      if (FLineText[FTokenEnd-1] = '{')
      and (FLineText[FTokenEnd] = '-')
      then exit(True);
    until (FTokenEnd > l);
  end;

begin
  l := length(FLineText);
  FTokenEnd := FTokenPos;
  If FTokenPos > l then exit;

  if FLineText[FTokenPos] in C_SPACE then begin
    // at beginning of whitespace token, looking for end of whitespace
    // (whitespace is regarded as a token too)
    FTokenAttr := FSpaceAttri;
    ScanUntilNot(C_SPACE);

  end else begin
    // at beginning of any other token. We determine what it is
    // by the first one or two characters and then scan until
    // the end of that token (FTokenEnd must be the beginning
    // of the next token or until we reach end of line)

    // range>0 means we are still inside a multi-line comment,
    // scan until -} or {- or line end, nothing else matters
    if (FCurRange > 0) then begin
      FTokenAttr := FCommentAttri;
      if ScanUntilComment then begin
        Inc(FTokenEnd);
        if FLineText[FTokenEnd-2] = '{' then
          Inc(FCurRange); // found the end
        if FLineText[FTokenEnd-2] = '-' then
          Dec(FCurRange); // found the end
      end;
    end

    else if (FLineText[FTokenPos] = '{')
    and (FTokenPos < l)
    and (FLineText[FTokenPos+1] = '-')
    then begin
      FTokenAttr := FCommentAttri;
      Inc(FCurRange);
      Inc(FTokenEnd, 2);
    end

    else if (FLineText[FTokenPos] = '''')
    and (FTokenPos < l-1)
    and (FLineText[FTokenPos+2] = '''')
    then begin
      FTokenAttr := FStringAttri;
      inc(FTokenEnd, 3);
    end

    else if (FLineText[FTokenPos] = '"') then begin
      FTokenAttr := FStringAttri;
      ScanUntil(['"']);
      Inc(FTokenEnd); // closing " is part of the token
    end

    else if (FLineText[FTokenPos] = '-')
    and (FTokenPos < l)
    and (FLineText[FTokenPos+1] = '-')
    then begin
      FTokenAttr := FCommentAttri;
      FTokenEnd := l+1;
    end

    else if FLineText[FTokenPos] in C_DIGIT then begin
      FTokenAttr := FNumberAttri;
      ScanUntilNot(C_DIGIT);
    end

    else if FLineText[FTokenPos] in C_LETTER then begin
      FTokenAttr := FIdentifierAttri;
      ScanUntil(C_SPACE + C_OPER + C_BRAC);
    end

    else if FLineText[FTokenPos] in C_BRAC then begin
      FTokenAttr := FBracAttri;
      Inc(FTokenEnd);
    end

    else if FLineText[FTokenPos] in C_OPER then begin
      FTokenAttr := FOperAttri;
      ScanUntilNot(C_OPER);
    end;

    if FTokenAttr = FIdentifierAttri then
      if IsKeyword(GetToken) then
        FTokenAttr := FKeywordAttri;
  end;
end;

procedure TSynHaskell.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  FTokenAttr := nil;
  FTokenPos := 1;
  FLineText := NewValue;
  FindTokenEnd;
end;

procedure TSynHaskell.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenStart := @FLineText[FTokenPos];
  TokenLength := FTokenEnd - FTokenPos;
end;

function TSynHaskell.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := FTokenAttr;
end;

procedure TSynHaskell.Next;
begin
  FTokenPos := FTokenEnd;
  FindTokenEnd;
end;

function TSynHaskell.GetEol: Boolean;
begin
  Result := FTokenPos > length(FLineText);
end;

function TSynHaskell.GetToken: String;
begin
  Result := copy(FLineText, FTokenPos, FTokenEnd - FTokenPos);
end;

function TSynHaskell.GetTokenPos: Integer;
begin
  Result := FTokenPos - 1;
end;

{ This is called by synedit when it tries to find matching
  brackets. They will only match if both brackets are inside
  the same "Kind" of token. }
function TSynHaskell.GetTokenKind: integer;
var
  a: TSynHighlighterAttributes;
begin
  a := GetTokenAttribute;
  if a = FBracAttri then Result := 1
  else if a = FCommentAttri then Result := 2
  else if a = FStringAttri then Result := 3
  else Result := 0;
end;

function TSynHaskell.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    else Result := nil;
  end;
end;

procedure TSynHaskell.SetRange(Value: Pointer);
begin
  FCurRange := PtrInt(Value);
end;

procedure TSynHaskell.ResetRange;
begin
  FCurRange := 0;
end;

function TSynHaskell.GetRange: Pointer;
begin
  Result := Pointer(FCurRange);
end;


end.
