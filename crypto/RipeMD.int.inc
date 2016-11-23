{******************************************************************************}
{*  RipeMD.int.inc                                                            *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
    PRMDCtx = ^TRMDCtx;
    TRMDCtx = record
        MDBuffer      : array [0..4] of LongWord;
        Length        : LongWord;
        CurrentLength : LongWord;
        Buffer        : array [0..63] of Byte;
    end;

procedure RMD128Init (var ctx: TRMDCtx);
procedure RMD160Init (var ctx: TRMDCtx);
procedure RMDUpdate (var ctx: TRMDCtx; aBuffer: Pointer; aLength: LongWord; aSize: Word);
function RMDFinal (var ctx: TRMDCtx; aSize: LongWord) : String;

function RipeMD128 (const aValue: String) : String; overload;
function RipeMD128 (const aValue: PChar) : String; overload;
function RipeMD128 (const aStream: TStream;
                    const aBlockSize: LongWord = 1024) : String; overload;
function RipeMD128File (const aFileName: String;
                        const aBlockSize: LongWord = 1024) : String;

function RipeMD160 (const aValue: String) : String; overload;
function RipeMD160 (const aValue: PChar) : String; overload;
function RipeMD160 (const aStream: TStream;
                    const aBlockSize: LongWord = 1024) : String; overload;
function RipeMD160File (const aFileName: String;
                        const aBlockSize: LongWord = 1024) : String;