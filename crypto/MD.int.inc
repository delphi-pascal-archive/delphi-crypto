{******************************************************************************}
{*  MD.int.inc                                                                *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
    PMD4Ctx = ^TMD4Ctx;
    TMD4Ctx = record
        State  : array [0..3] of LongWord;
        Count  : array [0..1] of LongWord;
        Buffer : array [0..63] of Byte;
    end;

    TMDTransform = procedure (var State: array of LongWord; Block: Pointer);

procedure MDInit (ctx: PMD4Ctx);
procedure MDUpdate (ctx: PMD4Ctx; Input: Pointer; InputLength: LongWord; Transform: TMDTransform);
procedure MD4Transform (var State: array of LongWord; Block: Pointer);
procedure MD5Transform (var State: array of LongWord; Block: Pointer);
function MDFinal (ctx: PMD4Ctx; Transform: TMDTransform) : String;

function MD4 (const aValue: String) : String; overload;
function MD4 (const aValue: PChar) : String; overload;
function MD4 (const aStream: TStream;
              const aBlockSize: LongWord = 1024) : String; overload;
function MD4File (const aFileName: String;
                  const aBlockSize: LongWord = 1024) : String;

function MD5 (const aValue: String) : String; overload;
function MD5 (const aValue: PChar) : String; overload;
function MD5 (const aStream: TStream;
              const aBlockSize: LongWord = 1024) : String; overload;
function MD5File (const aFileName: String;
                  const aBlockSize: LongWord = 1024) : String;
