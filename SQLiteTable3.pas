unit SQLiteTable3;

{
  Simple classes for using SQLite's exec and get_table.

  TSQLiteDatabase wraps the calls to open and close an SQLite database.
  It also wraps SQLite_exec for queries that do not return a result set

  TSQLiteTable wraps sqlite_get_table.
  It allows accessing fields by name as well as index and can step through a
  result set with the Next procedure.

  Adapted by Tim Anderson (tim@itwriting.com)
  Originally created by Pablo Pissanetzky (pablo@myhtpc.net)
  Modified and enhanced by Lukas Gebauer
  Modified by black.rabbit
      07.10.2011 - added method IndexExists to class TSQLiteDatabase
      02.11.2011 - added method TriggerExists to class TSQLiteDatabase
      03.11.2011 - added method Compress to class TSQLiteDatabase
      14.11.2011 - use PUTF8String file name in function SQLite3_Open
      12.03.2012 - use const SQLite_CacheSize
                   added method GetCacheSize
                   added method SetCacheSize
                   use const SQLite_Synchronus
                   use const SQLite_AutoVacuum
}

interface

{$I 'std.inc'}

uses
  Windows, SQLite3, Classes, SysUtils;

const

  dtInt = 1;
  dtNumeric = 2;
  dtStr = 3;
  dtBlob = 4;
  dtNull = 5;

type

  ESQLiteException = class(Exception)
  end;

  TSQLiteTable = class;

  TSQLiteDatabase = class
  private
    fDB: TSQLiteDB;
    fInTrans: boolean;
    procedure RaiseError(s: string; SQL: string);
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    function GetTable(const SQL: string): TSQLiteTable;
    procedure ExecSQL(const SQL: string);
    function GetTableValue(const SQL: string): int64;
    function GetTableString(const SQL: string): string;
    procedure UpdateBlob(const SQL: string; BlobData: TStream);
    procedure BeginTransaction;
    procedure Commit;
    procedure Rollback;
    function TableExists(TableName: string): boolean;
    function IndexExists(IndexName: string): boolean;
    function TriggerExists(TriggerName: string): boolean;
    procedure Compress;
    function GetCacheSize: longint;
    procedure SetCacheSize(Value: longint);
    function GetLastInsertRowID: int64;
    procedure SetTimeout(Value: integer);
    function version: string;
  {published}
    property isTransactionOpen: boolean read fInTrans;
  end;

  TSQLiteTable = class
  private
    fResults: TList;
    fRowCount: cardinal;
    fColCount: cardinal;
    fCols: TStringList;
    fColTypes: TList;
    fRow: cardinal;
    function GetFields(I: cardinal): string;
    function GetEOF: boolean;
    function GetBOF: boolean;
    function GetColumns(I: integer): string;
    function GetFieldByName(FieldName: string): string;
    function GetFieldIndex(FieldName: string): integer;
    function GetCount: integer;
    function GetCountResult: integer;
  public
    constructor Create(DB: TSQLiteDatabase; const SQL: string);
    destructor Destroy; override;
    function FieldAsInteger(I: cardinal): int64;
    function FieldAsBlob(I: cardinal): TMemoryStream;
    function FieldAsBlobText(I: cardinal): string;
    function FieldIsNull(I: cardinal): boolean;
    function FieldAsString(I: cardinal): string;
    function FieldAsDouble(I: cardinal): double;
    function Next: boolean;
    function Previous: boolean;
    property EOF: boolean read GetEOF;
    property BOF: boolean read GetBOF;
    property Fields[I: cardinal]: string read GetFields;
    property FieldByName[FieldName: string]: string read GetFieldByName;
    property FieldIndex[FieldName: string]: integer read GetFieldIndex;
    property Columns[I: integer]: string read GetColumns;
    property ColCount: cardinal read fColCount;
    property RowCount: cardinal read fRowCount;
    property Row: cardinal read fRow;
    function MoveFirst: boolean;
    function MoveLast: boolean;
    property Count: integer read GetCount;
    // The property CountResult is used when you execute count(*) queries.
    // It returns 0 if the result set is empty or the value of the
    // first field as an integer.
    property CountResult: integer read GetCountResult;
  end;

procedure DisposePointer(ptr: pointer); cdecl;


implementation

procedure DisposePointer(ptr: pointer); cdecl;
begin
  if assigned(ptr) then
    freemem(ptr);
end;

//------------------------------------------------------------------------------
// TSQLiteDatabase
//------------------------------------------------------------------------------

constructor TSQLiteDatabase.Create(const FileName: string);
const
  SQLite_CacheSize = 4096;
const
{$IF     Defined(SQLITE_SYNCHRONOUS_FULL) and
     not Defined(SQLITE_SYNCHRONOUS_NORMAL) and
     not Defined(SQLITE_SYNCHRONOUS_OFF) }
  SQLite_Synchronus = 'FULL';
{$ELSEIF not Defined(SQLITE_SYNCHRONOUS_FULL) and
             Defined(SQLITE_SYNCHRONOUS_NORMAL) and
         not Defined(SQLITE_SYNCHRONOUS_OFF) }
  SQLite_Synchronus = 'NORMAL';
{$ELSEIF not Defined(SQLITE_SYNCHRONOUS_FULL) and
         not Defined(SQLITE_SYNCHRONOUS_NORMAL) and
             Defined(SQLITE_SYNCHRONOUS_OFF) }
  SQLite_Synchronus = 'OFF';
{$IFEND}
const
{$IF     Defined(SQLITE_AUTO_VACUUM_NONE) and
     not Defined(SQLITE_AUTO_VACUUM_FULL) and
     not Defined(SQLITE_AUTO_VACUUM_INCREMENTAL) }
  SQLite_AutoVacuum = 0;
{$ELSEIF not Defined(SQLITE_AUTO_VACUUM_NONE) and
             Defined(SQLITE_AUTO_VACUUM_FULL) and
         not Defined(SQLITE_AUTO_VACUUM_INCREMENTAL) }
  SQLite_AutoVacuum = 1;
{$ELSEIF not Defined(SQLITE_AUTO_VACUUM_NONE) and
         not Defined(SQLITE_AUTO_VACUUM_FULL) and
             Defined(SQLITE_AUTO_VACUUM_INCREMENTAL) }
  SQLite_AutoVacuum = 2;
{$IFEND}
const
  SQLite_TimeOut = 4096; { ms }
const
  MaxTryCount = 3;
var
  Msg: pchar;
  iResult: integer;
  TryCount: integer;
begin
  inherited Create;

  self.fInTrans := False;

  Msg := nil;
  try

    TryCount := 0;
    repeat
        Inc (TryCount);

        iResult := SQLite3_Open( PUTF8String ( AnsiToUtf8 (FileName) ), Fdb );

    until ( iResult = SQLITE_OK ) or
          ( TryCount >= MaxTryCount );

    if iResult <> SQLITE_OK then
      if Assigned(Fdb) then
      begin
        Msg := Sqlite3_ErrMsg(Fdb);
        raise ESqliteException.CreateFmt('Failed to open database "%s" : %s',
          [FileName, Msg]);
      end
      else
        raise ESqliteException.CreateFmt('Failed to open database "%s" : unknown error',
          [FileName]);

    //set a few configs
    self.ExecSQL( Format ('PRAGMA SYNCHRONOUS=%s;',[SQLite_Synchronus]) );
//    self.ExecSQL('PRAGMA full_column_names = 1;');
    self.ExecSQL('PRAGMA temp_store = MEMORY;');

    // set cache size
    if ( SQLite_CacheSize > 0 ) then
        self.ExecSQL( Format ('PRAGMA cache_size=%d;',[SQLite_CacheSize]) );

    // auto-compress mode
    self.ExecSQL( Format ('PRAGMA auto_vacuum=%d;',[SQLite_AutoVacuum]) );

    // set timeout
    SetTimeout (SQLite_TimeOut);

  finally
    if Assigned(Msg) then
      SQLite3_Free(Msg);
  end;

end;


//..............................................................................

destructor TSQLiteDatabase.Destroy;
begin

  if self.fInTrans then
    self.ExecSQL('ROLLBACK;'); //assume rollback

  if Assigned(fDB) then
    SQLite3_Close(fDB);

  inherited;
end;

function TSQLiteDatabase.GetLastInsertRowID: int64;
begin
  Result := Sqlite3_LastInsertRowID(self.fDB);
end;

//..............................................................................

procedure TSQLiteDatabase.RaiseError(s: string; SQL: string);
//look up last error and raise an exception with an appropriate message
var
  Msg: PChar;
begin

  Msg := nil;

  if sqlite3_errcode(self.fDB) <> SQLITE_OK then
    Msg := sqlite3_errmsg(self.fDB);

  if Msg <> nil then
    raise ESqliteException.CreateFmt(s + ' "%s" : %s', [SQL, Msg])
  else
    raise ESqliteException.CreateFmt(s, [SQL, 'No message']);

end;

procedure TSQLiteDatabase.ExecSQL(const SQL: string);
const
  MaxTryCount = 3;
var
  Stmt: TSQLiteStmt;
  NextSQLStatement: Pchar;
  iStepResult: integer;
  Success: boolean;
  TryCount: integer;
begin
  TryCount := 0;
  repeat try
    Success := TRUE;
    Inc (TryCount);

    try

      if Sqlite3_Prepare(self.fDB, PChar(SQL), -1, Stmt, NextSQLStatement) <>
        SQLITE_OK then
        RaiseError('Error executing SQL', SQL);

      if (Stmt = nil) then
        RaiseError('Could not prepare SQL statement', SQL);

      iStepResult := Sqlite3_step(Stmt);

      if (iStepResult <> SQLITE_DONE) then
        RaiseError('Error executing SQL statement', SQL);

    finally

      if Assigned(Stmt) then
        Sqlite3_Finalize(stmt);

    end;

  except
    Sleep (100);
    Success := FALSE;
    if ( TryCount >= MaxTryCount) then raise;
  end until Success;
end;

procedure TSQLiteDatabase.UpdateBlob(const SQL: string; BlobData: TStream);
const
  MaxTryCount = 3;
var
  iSize: integer;
  ptr: pointer;
  Stmt: TSQLiteStmt;
  Msg: Pchar;
  NextSQLStatement: Pchar;
  iStepResult: integer;
  iBindResult: integer;
  Success: boolean;
  TryCount: integer;
begin
  TryCount := 0;
  repeat try
    Success := TRUE;
    Inc (TryCount);

    //expects SQL of the form 'UPDATE MYTABLE SET MYFIELD = ? WHERE MYKEY = 1'
    if pos('?', SQL) = 0 then
      RaiseError('SQL must include a ? parameter', SQL);

    Msg := nil;
    try

      if Sqlite3_Prepare(self.fDB, PChar(SQL), -1, Stmt, NextSQLStatement) <>
        SQLITE_OK then
        RaiseError('Could not prepare SQL statement', SQL);

      if (Stmt = nil) then
        RaiseError('Could not prepare SQL statement', SQL);

      //now bind the blob data
      iSize := BlobData.size;

      GetMem(ptr, iSize);

      if (ptr = nil) then
        raise ESqliteException.CreateFmt('Error getting memory to save blob',
          [SQL, 'Error']);

      BlobData.position := 0;
      BlobData.Read(ptr^, iSize);

      iBindResult := SQLite3_BindBlob(stmt, 1, ptr, iSize, @DisposePointer);

      if iBindResult <> SQLITE_OK then
        RaiseError('Error binding blob to database', SQL);

      iStepResult := Sqlite3_step(Stmt);

      if (iStepResult <> SQLITE_DONE) then
        RaiseError('Error executing SQL statement', SQL);

    finally

      if Assigned(Stmt) then
        Sqlite3_Finalize(stmt);

      if Assigned(Msg) then
        SQLite3_Free(Msg);
    end;

  except
    Sleep (100);
    Success := FALSE;
    if ( TryCount >= MaxTryCount) then raise;
  end until Success;
end;

//..............................................................................

function TSQLiteDatabase.GetTable(const SQL: string): TSQLiteTable;
const
  MaxTryCount = 3;
var
  TryCount: integer;
begin
  Result := NIL;
  TryCount := 0;
  repeat try
    Inc (TryCount);
    Result := TSQLiteTable.Create(Self, SQL);
  except
    FreeAndNil (Result);
    Sleep (100); 
    if ( TryCount >= MaxTryCount ) then raise;
  end until Assigned (Result);
end;

function TSQLiteDatabase.GetTableValue(const SQL: string): int64;
var
  Table: TSQLiteTable;
begin
  Table := self.GetTable(SQL);
  try
    Result := Table.FieldAsInteger(0);
  finally
    Table.Free;
  end;
end;

function TSQLiteDatabase.GetTableString(const SQL: string): string;
var
  Table: TSQLiteTable;
begin
  Table := self.GetTable(SQL);
  try
    Result := Table.FieldAsString(0);
  finally
    Table.Free;
  end;
end;


procedure TSQLiteDatabase.BeginTransaction;
begin
  if not self.fInTrans then
  begin
    self.ExecSQL('BEGIN TRANSACTION;');
    self.fInTrans := True;
  end
  else
    raise ESqliteException.Create('Transaction already open');
end;

procedure TSQLiteDatabase.Commit;
begin
  self.ExecSQL('COMMIT;');
  self.fInTrans := False;
end;

procedure TSQLiteDatabase.Rollback;
begin
  self.ExecSQL('ROLLBACK;');
  self.fInTrans := False;
end;

function TSQLiteDatabase.TableExists(TableName: string): boolean;
var
  sql: string;
  ds: TSqliteTable;
begin
  Result := false;
  //returns true if table exists in the database
  sql := 'select [sql] from sqlite_master where [type] = ''table'' and lower(name) = ''' +
    lowercase(TableName) + ''' ';
  ds := self.GetTable(sql);
  try
    Result := (ds.Count > 0);
  finally
    ds.Free;
  end;
end;

function TSQLiteDatabase.IndexExists(IndexName: string): boolean;
var
  sql: string;
  ds: TSqliteTable;
begin
  Result := false;
  //returns true if index exists in the database
  sql := 'select [sql] from sqlite_master where [type] = ''index'' and lower(name) = ''' +
    lowercase(IndexName) + ''' ';
  ds := self.GetTable(sql);
  try
    Result := (ds.Count > 0);
  finally
    ds.Free;
  end;
end;

function TSQLiteDatabase.TriggerExists(TriggerName: string): boolean;
var
  sql: string;
  ds: TSqliteTable;
begin
  Result := false;
  //returns true if trigger exists in the database
  sql := 'select [sql] from sqlite_master where [type] = ''trigger'' and lower(name) = ''' +
    lowercase(TriggerName) + ''' ';
  ds := self.GetTable(sql);
  try
    Result := (ds.Count > 0);
  finally
    ds.Free;
  end;
end;

procedure TSQLiteDatabase.Compress;
var
  sql: string;
begin
  //compress database
  sql := 'vacuum ';
  self.ExecSQL(sql);
end;

function TSQLiteDatabase.GetCacheSize: longint;
var
  sql: string;
  ds: TSqliteTable;
begin
  Result := -1;
  //returns cache size
  sql := 'PRAGMA cache_size;';
  ds := self.GetTable(sql);
  try
    if ( ds.Count > 0 ) then
        TryStrToInt ( ds.Fields [0], Result );
  finally
    ds.Free;
  end;
end;

procedure TSQLiteDatabase.SetCacheSize(Value: longint);
var
  sql: string;
begin
  // set cache size
  if (Value > 0) then
  begin
      sql := Format ('PRAGMA cache_size=%d;',[Value]);
      self.ExecSQL(sql);
  end;
end;

procedure TSQLiteDatabase.SetTimeout(Value: integer);
begin
  SQLite3_BusyTimeout(self.fDB, Value);
end;

function TSQLiteDatabase.version: string;
begin
  Result := SQLite3_Version;
end;


//------------------------------------------------------------------------------
// TSQLiteTable
//------------------------------------------------------------------------------

constructor TSQLiteTable.Create(DB: TSQLiteDatabase; const SQL: string);
var
  Stmt: TSQLiteStmt;
  NextSQLStatement: Pchar;
  iStepResult: integer;
  ptr: pointer;
  iNumBytes: integer;
  thisBlobValue: TMemoryStream;
  thisStringValue: pstring;
  thisDoubleValue: pDouble;
  thisIntValue: pInt64;
  thisColType: pInteger;
  i: integer;
  DeclaredColType: Pchar;
  ActualColType: integer;
  ptrValue: Pchar;
begin
  try
    self.fRowCount := 0;
    self.fColCount := 0;
    //if there are several SQL statements in SQL, NextSQLStatment points to the
    //beginning of the next one. Prepare only prepares the first SQL statement.
    if Sqlite3_Prepare(DB.fDB, PChar(SQL), -1, Stmt, NextSQLStatement) <> SQLITE_OK then
      DB.RaiseError('Error executing SQL', SQL);
    if (Stmt = nil) then
      DB.RaiseError('Could not prepare SQL statement', SQL);
    iStepResult := Sqlite3_step(Stmt);
    while (iStepResult <> SQLITE_DONE) do
    begin
      case iStepResult of
        SQLITE_ROW:
          begin
            Inc(fRowCount);
            if (fRowCount = 1) then
            begin
            //get data types
              fCols := TStringList.Create;
              fColTypes := TList.Create;
              fColCount := SQLite3_ColumnCount(stmt);
              for i := 0 to Pred(fColCount) do
                fCols.Add(AnsiUpperCase(Sqlite3_ColumnName(stmt, i)));
              for i := 0 to Pred(fColCount) do
              begin
                new(thisColType);
                DeclaredColType := Sqlite3_ColumnDeclType(stmt, i);
                if DeclaredColType = nil then
                  thisColType^ := Sqlite3_ColumnType(stmt, i) //use the actual column type instead
                //seems to be needed for last_insert_rowid
                else
                  if (DeclaredColType = 'INTEGER') or (DeclaredColType = 'BOOLEAN') then
                    thisColType^ := dtInt
                  else
                    if (DeclaredColType = 'NUMERIC') or
                      (DeclaredColType = 'FLOAT') or
                      (DeclaredColType = 'DOUBLE') or
                      (DeclaredColType = 'REAL') then
                      thisColType^ := dtNumeric
                    else
                      if DeclaredColType = 'BLOB' then
                        thisColType^ := dtBlob
                      else
                        thisColType^ := dtStr;
                fColTypes.Add(thiscoltype);
              end;
              fResults := TList.Create;
            end;

          //get column values
            for i := 0 to Pred(ColCount) do
            begin
              ActualColType := Sqlite3_ColumnType(stmt, i);
              if (ActualColType = SQLITE_NULL) then
                fResults.Add(nil)
              else
                if pInteger(fColTypes[i])^ = dtInt then
                begin
                  new(thisintvalue);
                  thisintvalue^ := Sqlite3_ColumnInt64(stmt, i);
                  fResults.Add(thisintvalue);
                end
                else
                  if pInteger(fColTypes[i])^ = dtNumeric then
                  begin
                    new(thisdoublevalue);
                    thisdoublevalue^ := Sqlite3_ColumnDouble(stmt, i);
                    fResults.Add(thisdoublevalue);
                  end
                  else
                    if pInteger(fColTypes[i])^ = dtBlob then
                    begin
                      iNumBytes := Sqlite3_ColumnBytes(stmt, i);
                      if iNumBytes = 0 then
                        thisblobvalue := nil
                      else
                      begin
                        thisblobvalue := TMemoryStream.Create;
                        thisblobvalue.position := 0;
                        ptr := Sqlite3_ColumnBlob(stmt, i);
                        thisblobvalue.writebuffer(ptr^, iNumBytes);
                      end;
                      fResults.Add(thisblobvalue);
                    end
                    else
                    begin
                      new(thisstringvalue);
                      ptrValue := Sqlite3_ColumnText(stmt, i);
                      setstring(thisstringvalue^, ptrvalue, strlen(ptrvalue));
                      fResults.Add(thisstringvalue);
                    end;
            end;
          end;
        SQLITE_BUSY:
          raise ESqliteException.CreateFmt('Could not prepare SQL statement',
            [SQL, 'SQLite is Busy']);
      else
        DB.RaiseError('Could not retrieve data', SQL);
      end;
      iStepResult := Sqlite3_step(Stmt);
    end;
    fRow := 0;
  finally
    if Assigned(Stmt) then
      Sqlite3_Finalize(stmt);
  end;
end;

//..............................................................................

destructor TSQLiteTable.Destroy;
var
  i: cardinal;
  iColNo: integer;
begin
  if Assigned(fResults) then
  begin
    for i := 0 to fResults.Count - 1 do
    begin
      //check for blob type
      iColNo := (i mod fColCount);
      case pInteger(self.fColTypes[iColNo])^ of
        dtBlob:
          TMemoryStream(fResults[i]).Free;
        dtStr:
          if fResults[i] <> nil then
          begin
            setstring(string(fResults[i]^), nil, 0);
            dispose(fResults[i]);
          end;
      else
        dispose(fResults[i]);
      end;
    end;
    fResults.Free;
  end;
  if Assigned(fCols) then
    fCols.Free;
  if Assigned(fColTypes) then
    for i := 0 to fColTypes.Count - 1 do
      dispose(fColTypes[i]);
  fColTypes.Free;
  inherited;
end;

//..............................................................................

function TSQLiteTable.GetColumns(I: integer): string;
begin
  Result := fCols[I];
end;

//..............................................................................

function TSQLiteTable.GetCountResult: integer;
begin
  if not EOF then
    Result := StrToInt(Fields[0])
  else
    Result := 0;
end;

function TSQLiteTable.GetCount: integer;
begin
  Result := FRowCount;
end;

//..............................................................................

function TSQLiteTable.GetEOF: boolean;
begin
  Result := fRow >= fRowCount;
end;

function TSQLiteTable.GetBOF: boolean;
begin
  Result := fRow <= 0;
end;

//..............................................................................

function TSQLiteTable.GetFieldByName(FieldName: string): string;
begin
  Result := GetFields(self.GetFieldIndex(FieldName));
end;

function TSQLiteTable.GetFieldIndex(FieldName: string): integer;
begin

  if (fCols = nil) then
  begin
    raise ESqliteException.Create('Field ' + fieldname + ' Not found. Empty dataset');
    exit;
  end;

  if (fCols.count = 0) then
  begin
    raise ESqliteException.Create('Field ' + fieldname + ' Not found. Empty dataset');
    exit;
  end;

  Result := fCols.IndexOf(AnsiUpperCase(FieldName));

  if (result < 0) then
  begin raise ESqliteException.Create('Field not found in dataset: ' + fieldname) end;

end;

//..............................................................................

function TSQLiteTable.GetFields(I: cardinal): string;
var
  thisvalue: pstring;
  thistype: integer;
begin
  Result := '';
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  //integer types are not stored in the resultset
  //as strings, so they should be retrieved using the type-specific
  //methods
  thistype := pInteger(self.fColTypes[I])^;

  case thistype of
    dtStr:
      begin
        thisvalue := self.fResults[(self.frow * self.fColCount) + I];
        if (thisvalue <> nil) then
          Result := thisvalue^
        else
          Result := '';
      end;
    dtInt:
      Result := IntToStr(self.FieldAsInteger(I));
    dtNumeric:
      Result := FloatToStr(self.FieldAsDouble(I));
    dtBlob:
      Result := self.FieldAsBlobText(I);
  else
    Result := '';
  end;
end;

function TSqliteTable.FieldAsBlob(I: cardinal): TMemoryStream;
begin
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  if (self.fResults[(self.frow * self.fColCount) + I] = nil) then
    Result := nil
  else
    if pInteger(self.fColTypes[I])^ = dtBlob then
      Result := TMemoryStream(self.fResults[(self.frow * self.fColCount) + I])
    else
      raise ESqliteException.Create('Not a Blob field');
end;

function TSqliteTable.FieldAsBlobText(I: cardinal): string;
var
  MemStream: TMemoryStream;
  Buffer: PChar;
begin
  Result := '';
  MemStream := self.FieldAsBlob(I);
  if MemStream <> nil then
    if MemStream.Size > 0 then
    begin
      MemStream.position := 0;
      Buffer := stralloc(MemStream.Size + 1);
      MemStream.readbuffer(Buffer[0], MemStream.Size);
      (Buffer + MemStream.Size)^ := chr(0);
      SetString(Result, Buffer, MemStream.size);
      strdispose(Buffer);
    end;
end;


function TSqliteTable.FieldAsInteger(I: cardinal): int64;
begin
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  if (self.fResults[(self.frow * self.fColCount) + I] = nil) then
    Result := 0
  else
    if pInteger(self.fColTypes[I])^ = dtInt then
      Result := pInt64(self.fResults[(self.frow * self.fColCount) + I])^
    else
      if pInteger(self.fColTypes[I])^ = dtNumeric then
        Result := trunc(strtofloat(pString(self.fResults[(self.frow * self.fColCount) + I])^))
      else
        raise ESqliteException.Create('Not an integer or numeric field');
end;

function TSqliteTable.FieldAsDouble(I: cardinal): double;
begin
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  if (self.fResults[(self.frow * self.fColCount) + I] = nil) then
    Result := 0
  else
    if pInteger(self.fColTypes[I])^ = dtInt then
      Result := pInt64(self.fResults[(self.frow * self.fColCount) + I])^
    else
      if pInteger(self.fColTypes[I])^ = dtNumeric then
        Result := pDouble(self.fResults[(self.frow * self.fColCount) + I])^
      else
        raise ESqliteException.Create('Not an integer or numeric field');
end;

function TSqliteTable.FieldAsString(I: cardinal): string;
begin
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  if (self.fResults[(self.frow * self.fColCount) + I] = nil) then
    Result := ''
  else
    Result := self.GetFields(I);
end;

function TSqliteTable.FieldIsNull(I: cardinal): boolean;
var
  thisvalue: pointer;
begin
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  thisvalue := self.fResults[(self.frow * self.fColCount) + I];
  Result := (thisvalue = nil);
end;

//..............................................................................

function TSQLiteTable.Next: boolean;
begin
  Result := False;
  if not EOF then
  begin
    Inc(fRow);
    Result := True;
  end;
end;

function TSQLiteTable.Previous: boolean;
begin
  Result := False;
  if not BOF then
  begin
    Dec(fRow);
    Result := True;
  end;
end;

function TSQLiteTable.MoveFirst: boolean;
begin
  Result := False;
  if self.fRowCount > 0 then
  begin
    fRow := 0;
    Result := True;
  end;
end;

function TSQLiteTable.MoveLast: boolean;
begin
  Result := False;
  if self.fRowCount > 0 then
  begin
    fRow := fRowCount - 1;
    Result := True;
  end;
end;


end.

