unit uRedisSimpleClient;

interface

uses
  {$IF Defined(MSWINDOWS)} Winapi.Windows, {$ENDIF}

  IdGlobal, System.Classes, system.SyncObjs,
  System.Generics.Collections, System.SysUtils, IdTCPClient;

type
  TRedisValueType = (rvtNone, rvtErr, rvtNullArray, rvtNullBulk, rvtOK, rvtQueued, rvtInt, rvtBulk, rvtArray);

  ERedisValueTypeError = class(Exception);


  TRedisValue = class
  private type
    TListRedisValue = TList<TRedisValue>;
  private const
    S_ERR_TYPE_GET_VALUE = 'Get value type err';
  public const
    _Auth = TRedisValueType.rvtOK;
    _Select = TRedisValueType.rvtOK;
    //==============
    _Watch = TRedisValueType.rvtOK;
    _Unwatch = TRedisValueType.rvtOK;
    _Multi = TRedisValueType.rvtOK;
    _Discard = TRedisValueType.rvtOK;
    _Exec = TRedisValueType.rvtArray;
    _Exec_faild = TRedisValueType.rvtNullArray;
    //==============
    _Exists = TRedisValueType.rvtInt;  //exists = 1;
        _Sub_Exists = 1;
    _Ttl = TRedisValueType.rvtInt;    //-1 = never timeout;
        _Sub_Ttl_Naver_Timeout = -1;
        _Sub_Ttl_Timeout_Or_Not_Exists = -2;
    _Del = TRedisValueType.rvtInt;    //result int for del count
    _Keys = TRedisValueType.rvtArray; //result a list of any key
    //==============
    _StrLen = TRedisValueType.rvtInt; //>= 0, no exists = 0  result ansi len 中国 = 4 length
    _SETRANGE = TRedisValueType.rvtInt;
    _GETRANGE = TRedisValueType.rvtBulk;
    //==============
    _Incr = TRedisValueType.rvtInt;   //result int for new value, if key exists and not int , then error
    _Decr = TRedisValueType.rvtInt;
    //==============
    _Append = TRedisValueType.rvtInt;  //result int for new length
    //==============
    _Set_ = TRedisValueType.rvtOK;
    _Get = TRedisValueType.rvtBulk;
    //==============
    _MSet = TRedisValueType.rvtOK;
    _MGet = TRedisValueType.rvtArray; //array of result value list
    //==============
    _HSet = TRedisValueType.rvtInt;    //1 = new value , 0 = replace value
        _Sub_HSet_NewValue = 1;
        _Sub_HSet_NewReplace = 0;
    _HGet = TRedisValueType.rvtBulk;
    //==============
    _HMSet = TRedisValueType.rvtOK;
    _HMGet = TRedisValueType.rvtArray; //array of result value list
    //==============

    _RPUSH = TRedisValueType.rvtInt;   //current count
    _LPUSH = TRedisValueType.rvtInt;   //current count
    _RPOP = TRedisValueType.rvtBulk;   //data
    _LPOP = TRedisValueType.rvtBulk;   //data

    _REM = TRedisValueType.rvtInt;      //success delete count
    _LRANGE = TRedisValueType.rvtArray; //array of bulk
    _LLEN = TRedisValueType.rvtInt;     //current count
    _LINDEX = TRedisValueType.rvtBulk;  //read data
    _LSET = TRedisValueType.rvtOK;
    //==============
    _SADD = TRedisValueType.rvtInt;       //success count
    _SREM = TRedisValueType.rvtInt;       //delete count
    _SCARD = TRedisValueType.rvtInt;      //exists count
    _SISMEMBER = TRedisValueType.rvtInt;  //value is exists
    _SMEMBERS = TRedisValueType.rvtArray; //get all value

  private
    FValueType: TRedisValueType;
    FValueInt: Integer;
    FBulk: TBytes;
    FList: TListRedisValue;
    FErrStr: string;
    FEncoding: TEncoding;
    function GetItems(const Index: integer): TRedisValue;
  protected
    property List: TListRedisValue read FList;
    property Bulk: TBytes read FBulk;
    property ErrStr: string read FErrStr;

    function AddItem: TRedisValue;

  public
    property Encoding: TEncoding read FEncoding;
    property Items[const Index: integer]: TRedisValue read GetItems; default;
    property ValueType: TRedisValueType read FValueType;

    constructor Create(aEncoding: TEncoding);
    destructor Destroy; override;

    function GetBulkAsString: string;
    function GetBulkAsBytes: TBytes;
    function GetInteger: Integer;
    function GetErrString: string;

    function TryGetBulk(var b: TBytes): boolean; overload;
    function TryGetBulk(var s: string): boolean; overload;
    function TryGetInt(var i: integer): boolean;
    function TryGetErr(var s: string): boolean;


    function ValueTypeIsErr: boolean;
    function ValueTypeIsOK: Boolean;
    function ValueTypeIsQueued: boolean;
    function ValueTypeIsOKQueued: boolean;
    function ValueTypeIsInt: boolean;
    function ValueTypeIsBulk: boolean;
    function ValueTypeIsArray: boolean;
    function ValueTypeIsNullArray: boolean;
    function ValueTypeIsNullBulk: boolean;


    function ArrCount: integer;

    procedure Clear;

  end;



type      //RedisCommand
  TRedisCommand = class
  private
    FCommandIsSet: Boolean;
    FEncoding: TEncoding;
  protected
    FParts: TList<TBytes>;

  const
    ASTERISK_BYTE: Byte = Byte('*');
    DOLLAR_BYTE: Byte = Byte('$');

  public
    property Encoding: TEncoding read FEncoding;
    constructor Create( aEncoding: TEncoding);
    destructor Destroy; override;
    function GetToken(const Index: Integer): TBytes;
    procedure Clear;
    function Count: Integer;
    function Add(ABytes: TBytes): TRedisCommand; overload;
    function Add(AString: string): TRedisCommand; overload;
    function Add(AInteger: NativeInt): TRedisCommand; overload;
    function SetCommand(AString: string): TRedisCommand; overload;
    function AddRange(AStrings: array of string): TRedisCommand; overload;
    function AddRange(ArrBytes: array of TBytes): TRedisCommand; overload;
    function ToRedisCommand: TBytes;

  end;

  refTRedisCommand = TRedisCommand;

type

  TSimpleIdTcpClient = class
  private
    FTcpClient: TIdTcpClient;
    FConnected: boolean;
    FAutoUpdateConnectState: boolean;
    function GetHost: string;
    function GetPort: Word;
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: Word);
    function GetConnectTimeout: integer;
    procedure SetConnectTimeout(const Value: integer);
    function GetOnConnected: TNotifyEvent;
    procedure SetOnConnected(const Value: TNotifyEvent);

  /////////
  protected
    property TcpClient: TIdTcpClient read FTcpClient;

    procedure CheckUpdateConnectState;
  public
    property OnConnected: TNotifyEvent read GetOnConnected write SetOnConnected;
    property ConnectTimeout: integer read GetConnectTimeout write SetConnectTimeout;
    property Host: string read GetHost write SetHost;
    property Port: Word read GetPort write SetPort;
    property AutoUpdateConnectState: boolean read FAutoUpdateConnectState write FAutoUpdateConnectState;
    property Connected: boolean read FConnected;

    constructor Create;
    destructor Destroy; override;

    function Connect: boolean;
    function Disconnect: boolean;

    procedure UpdateConnectState; overload;
    procedure UpdateConnectState(LastReceiveData: TBytes); overload;

    function ReadLine(var s: string; timeout: Cardinal = 50): boolean;
    function ReadBytes(var b: TBytes; Len: Integer; Append: Boolean = false; timeout: Cardinal = 50): boolean;
    function WriteBytes(b: TBytes; len: integer = -1; offset: Integer = 0): boolean;

    function ClearReadBuffer: boolean;

  end;

  TRedisSimpleTcpClient = class( TSimpleIdTcpClient);

type

    // 单行回复，第一个字节将是“+”
    // 错误消息，第一个字节将是“-”
    // 整型数字，第一个字节将是“:”
    // 批量回复，第一个字节将是“$”
    // 多个批量回复，第一个字节将是“*”
  TBaseRedisSimpleClient = class
  private const
    S_OK = 'OK';
    S_QUEUED = 'QUEUED';
    S_NULL_ARRAY = '*-1';
    S_NULL_BULK_STRING = '$-1';
    S_NET_READ_ERROR = 'Net read error';
    S_NET_CONNECT_FAILD = 'Net connect faild';
    S_NET_SEND_ERROR = 'Net send error';
    S_EXPECTED_INTEGER_FAILD = 'Expected integer faild';
    S_COMMAND_EMPTY = 'Command is empty';
    S_HANDLE_LINE_FAILD = 'Handle line faild';
    S_PARAM_ERROR = 'Param has error';
  public const
    ERR_NET_CONNECT_FAILD = 1;                           //连接失败
    ERR_NET_READ_ERROR = 2;                      //读取失败或者超时
    ERR_NET_SEND_ERROR = 3;
    ERR_EXPECTED_INTEGER_FAILD = 4;
    ERR_COMMAND_EMPTY = 5;
    ERR_HANDLE_LINE_FAILD = 6;
    ERR_PARAM_ERROR = 7;

  private
    FCmd: TRedisCommand;
    FSocket: TRedisSimpleTcpClient;
    FErrorCount: Integer;
    FLastErrStr: string;
    FLastErrCode: Integer;
    FTick: Cardinal;
    FForceDbIndex: Integer;
    FAuthUserName: string;
    FReceiveTimeout: Cardinal;
    FEncoding: TEncoding;
    function GetHost: string;
    function GetPort: Word;
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: Word);
    function GetConnectTimeout: integer;
    procedure SetConnectTimeout(const Value: integer);
  protected
    property Tick: Cardinal read FTick;
    property Socket: TRedisSimpleTcpClient read FSocket;
    property Cmd: TRedisCommand read FCmd;

    function GetCmdList(const Cmd: string): refTRedisCommand;

    function ReadOneRecive( Value: TRedisValue): boolean;

    function ArrStrToArrBytes(const Arr: TArray<String>): TArray<TBytes>;

    procedure NewError(code: Integer; s: string);

    function ReadLine(var Line: string): boolean;
    function ReadBytes(var b: TBytes; Len: integer; Append: boolean = false): boolean;

    function SendCmd(aCmd: TRedisCommand): Boolean; virtual;

    function DbIndexNeed(Value: TRedisValue): boolean;
    function AuthNeed(Value: TRedisValue): boolean;

    procedure OnConnected(sender: TObject); virtual;
  public
    property Encoding: TEncoding read FEncoding;
                         //发出指令后，就会接收数据，ReceiveTimeout 指最长等待多久
                         //实际上，数据交互时， 总是两个以上的 read 来完成一次指令的读取，
                         //所以 ReceiveTimeout 是指每次 read 的 timeout
    property ReceiveTimeout: Cardinal read FReceiveTimeout write FReceiveTimeout;
    property ErrorCount: Integer read FErrorCount;
    property LastErrCode: Integer read FLastErrCode;
    property LastErrStr: string read FLastErrStr;
    //=================
                      // < 0 is off. > -1 then force all cmd to DB index, auto use select to setting after net connect
    property ForceDbIndex: Integer read FForceDbIndex write FForceDbIndex;
    property ConnectTimeout: integer read GetConnectTimeout write SetConnectTimeout;
    property Host: string read GetHost write SetHost;
    property Port: Word read GetPort write SetPort;
    property AuthUserName: string read FAuthUserName write FAuthUserName;
    //=================
    constructor Create(aEncoding: TEncoding);
    destructor Destroy; override;
    //=================
    function Connect: boolean;
    function ConnectNeed: boolean;

    function Disconnect: boolean;
    function GetConnected: boolean;
    //=================
    procedure BeginTick;
    function EndTick: Cardinal;


  end;

  TRedisSimpleClient = class( TBaseRedisSimpleClient)
  private
    FResValue: TRedisValue;
    FAutoConnect: boolean;
  protected

    function ReciveToResValue: boolean;

    function SendCmdAndReciveToResValue: boolean;
    function CheckArrayParam(Arr: TArray<TBytes>; CheckDoubleArray: boolean = false): boolean;
    function SendCmd(aCmd: TRedisCommand): Boolean; override;


  public    
    property AutoConnect: boolean read FAutoConnect write FAutoConnect;
    property ResValue: TRedisValue read FResValue;
    //=================
    constructor Create(aEncoding: TEncoding);
    destructor Destroy; override;

    procedure ChangeResValue( out CurResValue: TRedisValue);  //把当前的 ResValue 输出， 然后控件内部自己重建一个，外部必须负责释放 out value: TRedisValue
    //==============
    function Auth(const UseName: string): Boolean;
    function Select(const DBIndex: Integer): boolean;
    //==============
    function Watch(const Keys: TArray<String>): boolean;  overload;
    function Watch(const Keys: TArray<TBytes>): boolean; overload;
    function Unwatch: boolean;
    function Multi: boolean;
    function Discard: boolean;
    function Exec: boolean;
    //==============
    function Expire(const Key: string; AExpireInSecond: UInt32): boolean; overload;
    function Expire(const Key: TBytes; AExpireInSecond: UInt32): boolean; overload;
    function Ttl(const Key: string): Boolean; overload; //result time remaining,  sec unit
    function Ttl(const Key: TBytes): Boolean; overload;
    function Del(const Keys: TArray<String>): boolean; overload;
    function Del(const Keys: TArray<TBytes>): boolean; overload;
    function Exists(const Key: string): boolean; overload;  //result 0 = no exists, 1 = exists
    function Exists(const Key: TBytes): boolean; overload;
    function Keys(const KeyPattern: string): boolean;
    //===============
    function StrLen(const Key: string): boolean; overload;
    function StrLen(const Key: TBytes): boolean; overload;
    function GetRange(const Key: string; StartPos, EndPos: integer): boolean;
    function SetRange(const Key: string; Pos: integer; value: string): boolean; overload;
    function SetRange(const Key: string; Pos: integer; value: TBytes): boolean; overload;
    //==============
    function Incr(const Key: string): boolean; overload;
    function Incr(const Key: TBytes): boolean; overload;
    function Decr(const Key: string): boolean; overload;
    function Decr(const Key: TBytes): boolean; overload;
    //==============
    function Append(const Key, Value: TBytes): boolean; overload;
    function Append(const Key, Value: string): boolean; overload;
    //==============
    function Set_(const key: string; const value: string; ASecsExpire: NativeInt = -10): boolean; overload;
    function Set_(const key: string; const value: TBytes; ASecsExpire: NativeInt = -10): boolean; overload;
    function Set_(const key: TBytes; const value: TBytes; ASecsExpire: NativeInt = -10): boolean; overload;
    function Get(const Key: TBytes): boolean; overload;
    function Get(const Key: string): boolean; overload;
    //==============
    function MSet(const KeyValues: TArray<String>): boolean; overload;
    function MSet(const KeyValues: TArray<TBytes>): boolean; overload;
    function MGet(const Keys: TArray<String>): boolean; overload;
    function MGet(const Keys: TArray<TBytes>): boolean; overload;
    //==============
    function HSet(const Key: string; const Field: string; const Value: string): boolean; overload;
    function HSet(const Key: TBytes; const Field: TBytes; const Value: TBytes): boolean; overload;
    function HGet(const Key: string; const Field: string): boolean; overload;
    function HGet(const Key: TBytes; const Field: TBytes): boolean; overload;
    //==============
    function HMSet(const Key: string; FieldValues: TArray<String>): boolean; overload;
    function HMSet(const Key: TBytes; FieldValues: TArray<TBytes>): boolean; overload;
    function HMGet(const Key: string; Fields: TArray<String>): boolean; overload;
    function HMGet(const Key: TBytes; Fields: TArray<TBytes>): boolean; overload;
    //==============
    function RPUSH(const ListKey: string; Values: TArray<String>): boolean; overload;
    function RPUSH(const ListKey: string; Values: TArray<TBytes>): boolean; overload;
    function RPOP(const ListKey: string): boolean; overload;
    function RPOP(const ListKey: TBytes): boolean; overload;

    function LPUSH(const ListKey: string; Values: TArray<String>): boolean; overload;
    function LPUSH(const ListKey: string; Values: TArray<TBytes>): boolean; overload;
    function LPOP(const ListKey: string): boolean; overload;
    function LPOP(const ListKey: TBytes): boolean; overload;
    //=============  list
    function LINDEX(const ListKey: string; Index: integer): boolean;
    function LSET(const ListKey: string; Index: integer; value: TBytes): boolean; overload;
    function LSET(const ListKey: string; Index: integer; value: string): boolean; overload;
    function LLEN(const ListKey: string): boolean;  //count
    function LRANGE(const ListKey: string; IndexStart, IndexStop: Integer): boolean;
    function LREM(const ListKey: string; const Count: Integer; const Value: string): boolean;
    //=============  sets
    function SADD(const Key: string; Values: TArray<String>): boolean;  //set key v1, v2 >> result count
    function SREM(const Key: string; Values: TArray<String>): boolean;  //delete key v1, v2 >> result count
    function SCARD(const key: string): boolean;    //return item count
    function SISMEMBER(const Key: string; const value: string): boolean; //exist >> result 0 or 1
    function SMEMBERS(const Key: string): boolean; //get all >> result array

  end;

  TSafeRedisSimpleClient = class(TRedisSimpleClient)
  private
    FCS: TCriticalSection;
  protected
    property CS: TCriticalSection read FCS;
  public

    constructor Create(aEncoding: TEncoding);
    destructor Destroy; override;

    procedure Lock;
    procedure UnLock;
  end;


implementation

function TickDiff(StartTick, EndTick: Cardinal): Cardinal; overload;
begin
  if EndTick >= StartTick then
    result := EndTick - StartTick
  else
    result := High(Cardinal) - StartTick + EndTick;
end;

function TickDiff(TickStart: Cardinal): Cardinal; overload;
begin
  result := TickDiff(TickStart, GetTickCount);
end;

{ TRedisValue }

function TRedisValue.AddItem: TRedisValue;
begin
  result := TRedisValue.create(FEncoding);
  Flist.Add( Result);
end;

function TRedisValue.ArrCount: integer;
begin
  result := FList.count;
end;

function TRedisValue.GetBulkAsBytes: TBytes;
begin
  if not TryGetBulk( result) then
    raise ERedisValueTypeError.Create( S_ERR_TYPE_GET_VALUE);
end;

function TRedisValue.GetInteger: Integer;
begin
  if not TryGetInt( result) then
    raise ERedisValueTypeError.Create( S_ERR_TYPE_GET_VALUE);
end;

function TRedisValue.GetBulkAsString: string;
begin
  if not TryGetBulk( result) then
    raise ERedisValueTypeError.Create( S_ERR_TYPE_GET_VALUE);
end;

function TRedisValue.GetErrString: string;
begin
  if not tryGetErr( result) then
    raise ERedisValueTypeError.Create( S_ERR_TYPE_GET_VALUE);
end;

procedure TRedisValue.Clear;
var
  i: Integer;
begin
  if ArrCount > 0 then
  begin
    for i := 0 to ArrCount - 1 do
      Items[ i].DisposeOf;
    Flist.Clear;
  end;

  FErrStr := '';
  setlength( FBulk, 0);
  FValueInt := -1;
  FValueType := TRedisValueType.rvtNone;
end;

constructor TRedisValue.Create(aEncoding: TEncoding);
begin
  FEncoding := aEncoding;
  FList := TListRedisValue.Create;
  FValueInt := -1;
end;

destructor TRedisValue.Destroy;
begin
  Clear;
  FList.DisposeOf;
  inherited Destroy;
end;

function TRedisValue.GetItems(const Index: integer): TRedisValue;
begin
  result := FList[index];
end;

function TRedisValue.TryGetBulk(var b: TBytes): boolean;
begin
  result := FValueType = TRedisValueType.rvtBulk;
  if result then
    b := FBulk;
end;

function TRedisValue.TryGetBulk(var s: string): boolean;
begin
  result := FValueType = TRedisValueType.rvtBulk;
  if result then
  try
    s := FEncoding.GetString(FBulk);
  except
    result := false;
  end;
end;

function TRedisValue.TryGetErr(var s: string): boolean;
begin
  result := fvalueType = TRedisValueType.rvtErr;
  if result then
    s := FErrStr;
end;

function TRedisValue.TryGetInt(var i: integer): boolean;
begin
  result := FValueType = TRedisValueType.rvtInt;
  if result then
    i := FValueInt;
end;

function TRedisValue.ValueTypeIsArray: boolean;
begin
  result := FValueType = TRedisValueType.rvtArray;
end;

function TRedisValue.ValueTypeIsBulk: boolean;
begin
  result := FValueType = TRedisValueType.rvtBulk;
end;

function TRedisValue.ValueTypeIsErr: boolean;
begin
  result := FvalueType = TRedisValueType.rvtErr;
end;

function TRedisValue.ValueTypeIsInt: boolean;
begin
  result := FValueType = TRedisValueType.rvtInt;
end;

function TRedisValue.ValueTypeIsNullArray: boolean;
begin
  result := FValueType = TRedisValueType.rvtNullArray;
end;

function TRedisValue.ValueTypeIsNullBulk: boolean;
begin
  result := FValueType = TRedisValueType.rvtNullBulk;
end;

function TRedisValue.ValueTypeIsOK: Boolean;
begin
  result := FValueType = TRedisValueType.rvtOK;
end;

function TRedisValue.ValueTypeIsOKQueued: boolean;
begin
  result := FValueType in [TRedisValueType.rvtOK, TRedisValueType.rvtQueued];
end;

function TRedisValue.ValueTypeIsQueued: boolean;
begin
  result := FValueType = TRedisValueType.rvtQueued;
end;

{ TRedisCommand }

function TRedisCommand.Add(ABytes: TBytes): TRedisCommand;
begin
  FParts.Add(ABytes);
  Result := Self;
end;

function TRedisCommand.Add(AString: string): TRedisCommand;
begin
  FParts.Add(FEncoding.GetBytes(AString));
  Result := Self;
end;

function TRedisCommand.Add(AInteger: NativeInt): TRedisCommand;
begin
  Result := Add(IntToStr(AInteger));
end;

function TRedisCommand.AddRange(ArrBytes: array of TBytes): TRedisCommand;
var
  b: TBytes;
begin
  for b in ArrBytes do
    Add(b);
  Result := Self;
end;

function TRedisCommand.AddRange(AStrings: array of string): TRedisCommand;
var
  s: string;
begin
  for s in AStrings do
    Add(s);
  Result := Self;
end;

procedure TRedisCommand.Clear;
begin
  FParts.Clear;
  FCommandIsSet := False;
end;

function TRedisCommand.Count: Integer;
begin
  Result := FParts.Count;
end;

constructor TRedisCommand.Create(aEncoding: TEncoding);
begin
  inherited Create;
  FEncoding := aEncoding;
  FParts := TList<TBytes>.Create;
end;

destructor TRedisCommand.Destroy;
begin
  FParts.DisposeOf;
  inherited;
end;

function TRedisCommand.GetToken(
  const
  Index:
  Integer): TBytes;
begin
  Result := FParts[index];
end;

function TRedisCommand.SetCommand(AString: string): TRedisCommand;
begin
  FParts.Clear;
  FParts.Add(TEncoding.ASCII.GetBytes(AString));
  FCommandIsSet := True;
  Result := Self;
end;

function TRedisCommand.ToRedisCommand: TBytes;
var
  L: TList<Byte>;
  I: Integer;
begin
  if not FCommandIsSet then
    setlength( result, 0)
  else
  begin
    L := TList<Byte>.Create;
    try
      L.Add(ASTERISK_BYTE); // FEncoding.GetBytes('*')[0]);
      L.AddRange(FEncoding.GetBytes(IntToStr(Count)));
      L.Add(Byte(#13));
      L.Add(Byte(#10));

      for I := 0 to Count - 1 do
      begin
        L.Add(DOLLAR_BYTE); // FEncoding.GetBytes('$')[0]);
        L.AddRange(FEncoding.GetBytes(IntToStr(Length(FParts[I]))));
        L.Add(Byte(#13));
        L.Add(Byte(#10));
        L.AddRange(FParts[I]);
        L.Add(Byte(#13));
        L.Add(Byte(#10));
      end;
      Result := L.ToArray;
    finally
      L.DisposeOf;
    end;
  end;
end;

{ TSimpleIdTcpClient }

procedure TSimpleIdTcpClient.CheckUpdateConnectState;
begin
  if FAutoUpdateConnectState then
    UpdateConnectState;
end;

function TSimpleIdTcpClient.ClearReadBuffer: boolean;
begin
  result := false;
  if TcpClient.IOHandler <> nil then
  begin
    TcpClient.IOHandler.InputBuffer.clear;
    result := true;
  end;
end;

function TSimpleIdTcpClient.Connect: boolean;
begin
  UpdateConnectState;

  if not FConnected then
  begin

    try
      FTcpClient.Connect;
      UpdateConnectState;

      {$IF Defined(POSIX)}
        if FConnected then   //android 连接成功后，好像要等待 0.5 秒才能正常发送消息。
          Sleep( 250);
      {$ENDIF}
    except
    end;

  end;

  result := FConnected;
end;

constructor TSimpleIdTcpClient.Create;
begin
  FTcpClient := TIdTCPClient.Create( nil);
  FTcpClient.ReadTimeout := 5; //这个参数在这个类里面应该没能发挥作用。
  FAutoUpdateConnectState := true;
end;

destructor TSimpleIdTcpClient.Destroy;
begin
  FTcpClient.DisposeOf;
  inherited destroy;
end;

function TSimpleIdTcpClient.Disconnect: boolean;
begin
  result := false;
  try
    TcpClient.Disconnect;
    result := true;
  except
    CheckUpdateConnectState;
  end;
end;

function TSimpleIdTcpClient.GetConnectTimeout: integer;
begin
  result := TcpClient.ConnectTimeout;
end;

function TSimpleIdTcpClient.GetHost: string;
begin
  result := TcpClient.Host;
end;

function TSimpleIdTcpClient.GetOnConnected: TNotifyEvent;
begin
  result := TcpClient.onConnected;
end;

function TSimpleIdTcpClient.GetPort: word;
begin
  result := TcpClient.Port;
end;

function TSimpleIdTcpClient.ReadBytes(var b: TBytes;
  Len: Integer; Append: Boolean; timeout: Cardinal): boolean;
var
  i: Integer;
  t: Cardinal;
begin
  result := false;

  if len > 0 then
  try

    if TcpClient.IOHandler.InputBuffer.Size >= len then
    begin
      TcpClient.IOHandler.InputBuffer.ExtractToBytes( TidBytes(b), len, Append);
      result := true;
    end

    else
    if timeout <= 0 then
    begin
      TcpClient.IOHandler.CheckForDataOnSource( 0);
      if TcpClient.IOHandler.InputBuffer.Size >= len then
      begin
        TcpClient.IOHandler.InputBuffer.ExtractToBytes( TidBytes(b), len, Append);
        result := true;
      end;
    end

    else
    begin

      i := 0;
      t := GetTickCount;
      while TickDiff( t) < timeout do
      begin
        TcpClient.IOHandler.CheckForDataOnSource( 0);
        if TcpClient.IOHandler.InputBuffer.Size >= len then
        begin
          TcpClient.IOHandler.InputBuffer.ExtractToBytes( TidBytes(b), len, Append);
          result := true;
          break;
        end

        else
        begin
          Inc( i);
          if ( i = 10) then   //10个循环 sleep 一次， 这是为了 sleep 精度不准的原因.一般在 15-20ms
          begin
            i := 0;
            sleep( 1);        //然而为了 cpu 使用率， 不得不 sleep
          end;
        end;
      end;

    end;

  except
    CheckUpdateConnectState;
  end;
end;

function TSimpleIdTcpClient.ReadLine( var s: string; timeout: Cardinal): boolean;
begin
  result := false;
  try
    s := TcpClient.IOHandler.ReadLn(LF, integer( timeout), -1,
      IndyTextEncoding_ASCII {IndyTextEncoding_Default}); //ansi 模式先试试看， default 本来也是 ansi， 然而 utf8 好像也得考虑

    result := not TcpClient.IOHandler.ReadLnTimedout;
  except
    CheckUpdateConnectState;
  end;
end;

procedure TSimpleIdTcpClient.SetConnectTimeout(const Value: integer);
begin
  TcpClient.ConnectTimeout := value;
end;

procedure TSimpleIdTcpClient.SetHost(const Value: string);
begin
  TcpClient.Host := value;
end;

procedure TSimpleIdTcpClient.SetOnConnected(const Value: TNotifyEvent);
begin
  TcpClient.onConnected := value;
end;

procedure TSimpleIdTcpClient.SetPort(const Value: word);
begin
  TcpClient.Port := value;
end;

procedure TSimpleIdTcpClient.UpdateConnectState(LastReceiveData: TBytes);
begin
  try

    if (TcpClient.IOHandler = nil) or (TcpClient.IOHandler.InputBuffer = nil) then
      FConnected := false
    else
    begin

      if TcpClient.IOHandler.InputBuffer.Size = 0 then
        FConnected := TcpClient.Connected
      else
      begin
        TcpClient.IOHandler.InputBuffer.ExtractToBytes(TIdBytes( LastReceiveData), -1, false);
        FConnected := TcpClient.Connected;
      end;

    end;

  except
    FConnected := false;
  end;
end;

procedure TSimpleIdTcpClient.UpdateConnectState;
var
  b: TBytes;
begin
  try

    if (TcpClient.IOHandler = nil) or (TcpClient.IOHandler.InputBuffer = nil) then
      FConnected := false
    else
    begin

      if TcpClient.IOHandler.InputBuffer.Size = 0 then
        FConnected := TcpClient.Connected
      else
      begin
        TcpClient.IOHandler.InputBuffer.ExtractToBytes(TIdBytes( b), -1, false);
        try
          FConnected := TcpClient.Connected;
        finally
          TcpClient.IOHandler.InputBuffer.Write(TIdBytes( b), -1);
        end;
      end;

    end;

  except
    FConnected := false;
  end;
end;

function TSimpleIdTcpClient.WriteBytes(b: TBytes; len: integer; offset: Integer): boolean;
begin
  result := false;
  try
    TcpClient.IOHandler.WriteDirect(  TIdBytes( b), len, offset);
    result := true;
  except
    CheckUpdateConnectState;
  end;
end;

{ TBaseRedisSimpleClient }

function TBaseRedisSimpleClient.ArrStrToArrBytes(
  const Arr: TArray<String>): TArray<TBytes>;
var
  i: Integer;
begin
  i := length( arr);
  setlength( result, i);
  if i > 0 then
    for i := 0 to i - 1 do
      result[i] := FEncoding.GetBytes( arr[i]);
end;

procedure TBaseRedisSimpleClient.BeginTick;
begin
  FTick := GetTickCount;
end;

function TBaseRedisSimpleClient.Connect: boolean;
begin
  result := socket.Connect;
  if not result then
    NewError(ERR_NET_CONNECT_FAILD, S_NET_CONNECT_FAILD)
  else
  begin

  end;
end;

function TBaseRedisSimpleClient.ConnectNeed: boolean;
var
  b: TBytes;
begin
  BeginTick;

  socket.UpdateConnectState( b); //eat
  Result := socket.Connected;
  if not result then
  begin
    if socket.TcpClient.IOHandler <> nil then
      Disconnect;

    result := Connect;
  end;

  EndTick;
end;

constructor TBaseRedisSimpleClient.Create(aEncoding: TEncoding);
begin
  FEncoding := aEncoding;
  FReceiveTimeout := 500;
  FCmd := TRedisCommand.Create(FEncoding);
  FSocket := TRedisSimpleTcpClient.Create;
  FSocket.OnConnected := self.OnConnected;
  FForceDbIndex := -1;
end;

destructor TBaseRedisSimpleClient.Destroy;
begin
  FCmd.DisposeOf;
  FSocket.DisposeOf;
  inherited Destroy;
end;

function TBaseRedisSimpleClient.Disconnect: boolean;
begin
  result := socket.Disconnect;
end;

function TBaseRedisSimpleClient.EndTick: cardinal;
begin
  result := TickDiff( FTick);
end;

function TBaseRedisSimpleClient.GetCmdList(const Cmd: string): refTRedisCommand;
begin
  FCmd.Clear;
  Result := FCmd;
  Result.SetCommand(Cmd);
end;

function TBaseRedisSimpleClient.GetConnected: boolean;
begin
  result := socket.Connected;
end;

function TBaseRedisSimpleClient.GetConnectTimeout: integer;
begin
  result := socket.ConnectTimeout;
end;

function TBaseRedisSimpleClient.GetHost: string;
begin
  result := socket.Host;
end;

function TBaseRedisSimpleClient.GetPort: Word;
begin
  result := socket.Port;
end;

procedure TBaseRedisSimpleClient.NewError(code: Integer; s: string);
begin
  inc( FErrorCount);
  FLastErrCode := code;
  FLastErrStr := s;
end;

procedure TBaseRedisSimpleClient.OnConnected(sender: TObject);
var
  b: boolean;
  Value: TRedisValue;
begin
  b := true;

  Value := TRedisValue.create(FEncoding);
  try

    if AuthUserName <> emptyStr then
      b := AuthNeed( value) and value.ValueTypeIsOK;

    if b and (ForceDbIndex > -1) then
      b := DbIndexNeed( value) and Value.ValueTypeIsOK;

  finally
    Value.DisposeOf;
  end;

  if not b then
    socket.Disconnect;
end;

function TBaseRedisSimpleClient.ReadBytes(var b: TBytes; Len: integer;
  Append: boolean): boolean;
begin
  result := Socket.ReadBytes(b, Len, append, FReceiveTimeout);
  if not Result then
    NewError( ERR_NET_READ_ERROR, S_NET_READ_ERROR);
end;

function TBaseRedisSimpleClient.ReadLine(var Line: string): boolean;
begin
  result := socket.ReadLine(Line, FReceiveTimeout);
  if not result then
    NewError(ERR_NET_READ_ERROR, S_NET_READ_ERROR);
end;

function TBaseRedisSimpleClient.ReadOneRecive(Value: TRedisValue): boolean;
var
  S: string;
  Cnt: Integer;
  b: TBytes;
begin
  Value.clear;
  Result := ReadLine( s);
  if result then
  begin
    if s = S_NULL_ARRAY then
      value.FValueType := TRedisValueType.rvtNullArray
    else
    if s = S_NULL_BULK_STRING then
      Value.FValueType := TRedisValueType.rvtNullBulk
    else
    begin

      case s.Chars[0] of
        '-':
          begin
            value.FValueType := TRedisValueType.rvtErr;
            Value.FErrStr :=  s.subString(1);
          end;
        '+':
          begin
            if s.Substring(1) = S_OK then
              Value.FValueType := TRedisValueType.rvtOK
            else
              Value.FValueType := TRedisValueType.rvtQueued;
          end;
        ':':
          begin
            if Not TryStrToInt( s.Substring( 1), Cnt) then
            begin
              NewError(ERR_EXPECTED_INTEGER_FAILD, S_EXPECTED_INTEGER_FAILD);
              result := False;
            end
            else
            begin
              value.FValueInt := Cnt;
              Value.FValueType := TRedisValueType.rvtInt;
            end;
          end;

        '$':
          begin
            if not TryStrToInt( s.Substring(1), cnt) then
            begin
              NewError(ERR_EXPECTED_INTEGER_FAILD, S_EXPECTED_INTEGER_FAILD);
              result := false;
            end
            else
            begin
              if Cnt < 0 then    //null bulk
                value.FValueType := TRedisValueType.rvtNullBulk
              else
              if cnt = 0 then    //empty string
                Value.FValueType := TRedisValueType.rvtBulk
              else
              begin
                result := ReadBytes( Value.FBulk, cnt);

                if result then
                begin
                  Value.FValueType := TRedisValueType.rvtBulk;
                  setlength(b, 2);  //must setlength, to ReAllocMem
                  ReadBytes(b, 2);  // eat crlf
                end;

              end;
            end;
          end;

        '*':
          begin
            if not TryStrToInt( s.Substring(1), cnt) then
            begin
              NewError(ERR_EXPECTED_INTEGER_FAILD, S_EXPECTED_INTEGER_FAILD);
              result := false;
            end
            else
            if cnt < 0 then
              Value.FValueType := TRedisValueType.rvtNullArray
            else
            begin
              Value.FValueType := TRedisValueType.rvtArray;
              if cnt > 0 then
                for Cnt := 0 to Cnt - 1 do
                  ReadOneRecive( value.AddItem);
            end;
          end;

        else
          NewError( ERR_HANDLE_LINE_FAILD, S_HANDLE_LINE_FAILD);
          result := false;
      end;

    end;
  end;
end;

function TBaseRedisSimpleClient.SendCmd(aCmd: TRedisCommand): Boolean;
var
  b: TBytes;
begin
  result := false;

  b := aCmd.ToRedisCommand;
  if length( b) = 0 then
    NewError(ERR_COMMAND_EMPTY, S_COMMAND_EMPTY)

  else
  begin
    result := socket.WriteBytes( b);
    if not result then
      NewError(ERR_NET_SEND_ERROR, S_NET_SEND_ERROR);
  end;
end;

procedure TBaseRedisSimpleClient.SetConnectTimeout(const Value: integer);
begin
  socket.ConnectTimeout := value;
end;

procedure TBaseRedisSimpleClient.SetHost(const Value: string);
begin
  socket.Host := value;
end;

procedure TBaseRedisSimpleClient.SetPort(const Value: Word);
begin
  socket.Port := value;
end;

function TBaseRedisSimpleClient.AuthNeed(Value: TRedisValue): boolean;
var
  command: TRedisCommand;
begin
  command := TRedisCommand.create(FEncoding);
  try
    command.SetCommand('AUTH').add(FAuthUserName);
    result := SendCmd(command) and readOneRecive( value);
  finally
    command.DisposeOf;
  end;
end;

function TBaseRedisSimpleClient.DbIndexNeed(Value: TRedisValue): boolean;
var
  command: TRedisCommand;
begin
  command := TRedisCommand.create(FEncoding);
  try
    command.SetCommand('SELECT').add(FForceDbIndex);
    result := SendCmd(command) and readOneRecive( Value);
  finally
    command.DisposeOf;
  end;
end;

{ TRedisSimpleClient }

function TRedisSimpleClient.Append(const Key, Value: TBytes): boolean;
begin
  GetCmdList('APPEND').Add(Key).add( value);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.Append(const Key, Value: string): boolean;
begin
  result := Append( FEncoding.GetBytes( Key), FEncoding.GetBytes( value));
end;

function TRedisSimpleClient.Auth(const UseName: string): Boolean;
begin
  GetCmdList('AUTH').Add(UseName);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.CheckArrayParam(Arr: TArray<TBytes>; CheckDoubleArray: boolean): boolean;
begin
  result := (length( arr) > 0) and (not CheckDoubleArray or (length( arr)  mod 2 = 0));
  if not result then
    NewError(ERR_PARAM_ERROR, S_PARAM_ERROR);
end;

constructor TRedisSimpleClient.Create(aEncoding: TEncoding);
begin
  inherited Create(aEncoding);
  FAutoConnect := true;
  FResValue :=  TRedisValue.create(Encoding);
end;

function TRedisSimpleClient.Decr(const Key: TBytes): boolean;
begin
  GetCmdList('DECR').add( Key);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.Decr(const Key: string): boolean;
begin
  result := decr( FEncoding.GetBytes( key));
end;

function TRedisSimpleClient.Del(const Keys: TArray<TBytes>): boolean;
begin
  result := CheckArrayParam( keys);
  if result then
  begin
    GetCmdList('DEL').addRange( Keys);
    result := SendCmdAndReciveToResValue;
  end;
end;

destructor TRedisSimpleClient.Destroy;
begin
  FResValue.DisposeOf;
  inherited destroy;
end;

function TRedisSimpleClient.Del(const Keys: TArray<String>): boolean;
begin
  result := Del( ArrStrToArrBytes( Keys));
end;


function TRedisSimpleClient.Discard: boolean;
begin
  GetCmdList('DISCARD');
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.Exec: boolean;
begin
  GetCmdList('EXEC');
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.Exists(const Key: TBytes): boolean;
begin
  GetCmdList('EXISTS').add( key);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.Exists(const Key: string): boolean;
begin
  result := Exists( FEncoding.GetBytes( key));
end;

function TRedisSimpleClient.Expire(const Key: string;
  AExpireInSecond: UInt32): boolean;
begin
  result := expire( FEncoding.GetBytes( key), AExpireInSecond);
end;

function TRedisSimpleClient.Expire(const Key: TBytes;
  AExpireInSecond: UInt32): boolean;
begin
  GetCmdList('EXPIRE').add(key).add( inttostr( AExpireInSecond));
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.Get(const Key: string): boolean;
begin
  result := Get( FEncoding.GetBytes( key));
end;

function TRedisSimpleClient.GetRange(const Key: string; StartPos,
  EndPos: integer): boolean;
begin
  GetCmdList('GETRANGE').add( Key).add(startPos).add( endPos);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.HGet(const Key, Field: TBytes): boolean;
begin
  GetCmdList('HGET').add( key).add( field);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.HMGet(const Key: string;
  Fields: TArray<String>): boolean;
begin
  result := HMGet( FEncoding.GetBytes( key), ArrStrToArrBytes( Fields));
end;

function TRedisSimpleClient.HMGet(const Key: TBytes;
  Fields: TArray<TBytes>): boolean;
begin
  result := CheckArrayParam(Fields);
  if result then
  begin
    GetCmdList('HMGET').add( key).addRange( Fields);
    result := SendCmdAndReciveToResValue;
  end;
end;

function TRedisSimpleClient.HMSet(const Key: string;
  FieldValues: TArray<String>): boolean;
begin
  result := HMSet(FEncoding.GetBytes( key), ArrStrToArrBytes( FieldValues));
end;

function TRedisSimpleClient.HMSet(const Key: TBytes;
  FieldValues: TArray<TBytes>): boolean;
begin
  result := CheckArrayParam(FieldValues, true);
  if result then
  begin
    GetCmdList('HMSET').add( key).addRange( FieldValues);
    result := SendCmdAndReciveToResValue;
  end;
end;

function TRedisSimpleClient.HSet(const Key, Field, Value: TBytes): boolean;
begin
  GetCmdList('HSET').add( key).add( field).add( value);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.HSet(const Key, Field, Value: string): boolean;
begin
  result := HSET( FEncoding.GetBytes( key), FEncoding.GetBytes( Field), FEncoding.GetBytes( value));
end;

function TRedisSimpleClient.HGet(const Key, Field: string): boolean;
begin
  result := hGet( FEncoding.GetBytes( key), FEncoding.GetBytes( field));
end;

function TRedisSimpleClient.Get(const Key: TBytes): boolean;
begin
  GetCmdList('GET').Add(Key);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.Incr(const Key: string): boolean;
begin
  result := Incr( FEncoding.GetBytes( key));
end;

function TRedisSimpleClient.Incr(const Key: TBytes): boolean;
begin
  GetCmdList('INCR').add( Key);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.Keys(const KeyPattern: string): boolean;
begin
  GetCmdList('KEYS').add( KeyPattern);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.LPOP(const ListKey: string): boolean;
begin
  result := LPOP(FEncoding.GetBytes( ListKey));
end;

function TRedisSimpleClient.LINDEX(const ListKey: string;
  Index: integer): boolean;
begin
  GetCmdList('LINDEX').add( ListKey).add( index);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.LLEN(const ListKey: string): boolean;
begin
  GetCmdList('LLEN').add( ListKey);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.LPOP(const ListKey: TBytes): boolean;
begin
  GetCmdList('LPOP').add( ListKey);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.LPUSH(const ListKey: string;
  Values: TArray<String>): boolean;
begin
  result := LPUSH(listKey, ArrStrToArrBytes( Values));
end;

function TRedisSimpleClient.LPUSH(const ListKey: string;
  Values: TArray<TBytes>): boolean;
begin
  result := false;
  if length( values) > 0 then
  begin
    GetCmdList('LPUSH').add( ListKey).addRange( Values);
    result := SendCmdAndReciveToResValue;
  end;

end;

function TRedisSimpleClient.LRANGE(const ListKey: string; IndexStart,
  IndexStop: Integer): boolean;
begin
  GetCmdList('LRANGE').add(ListKey).add( IndexStart.ToString).Add( IndexStop.ToString);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.LREM(const ListKey: string; const Count: Integer;
  const Value: string): boolean;
begin
  GetCmdList('LREM').addRange( ListKey).add( Count.ToString).add( value);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.LSET(const ListKey: string; Index: integer;
  value: string): boolean;
begin
  result := LSET(ListKey, Index, FEncoding.GetBytes( Value));
end;

function TRedisSimpleClient.LSET(const ListKey: string; Index: integer;
  value: TBytes): boolean;
begin
  GetCmdList('LSET').add( ListKey).add( index).add( value);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.MGet(const Keys: TArray<TBytes>): boolean;
begin
  GetCmdList('MGET').addRange( Keys);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.MGet(const Keys: TArray<String>): boolean;
begin
  result := MGet( ArrStrToArrBytes( Keys));
end;

function TRedisSimpleClient.MSet(const KeyValues: TArray<TBytes>): boolean;
begin
  result := CheckArrayParam( KeyValues, true);
  if result then
  begin
    GetCmdList('MSET').addRange( KeyValues);
    result := SendCmdAndReciveToResValue;
  end;
end;

function TRedisSimpleClient.MSet(const KeyValues: TArray<String>): boolean;
begin
  result := MSet( ArrStrToArrBytes( KeyValues));
end;

function TRedisSimpleClient.Multi: boolean;
begin
  GetCmdList('MULTI');
  result := SendCmdAndReciveToResValue;
end;

procedure TRedisSimpleClient.ChangeResValue(out CurResValue: TRedisValue);
begin
  CurResValue := self.FResValue;
  self.FResValue := TRedisValue.create(  self.Encoding);
end;

function TRedisSimpleClient.ReciveToResValue: boolean;
begin
  result := ReadOneRecive( FResValue);
end;

function TRedisSimpleClient.RPOP(const ListKey: TBytes): boolean;
begin
  GetCmdList('RPOP').add( ListKey);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.RPOP(const ListKey: string): boolean;
begin
  result := RPop(FEncoding.GetBytes( ListKey));
end;

function TRedisSimpleClient.RPUSH(const ListKey: string;
  Values: TArray<TBytes>): boolean;
begin
  result := false;
  if length( values) > 0 then
  begin
    GetCmdList('RPUSH').add( ListKey).addRange( Values);
    result := SendCmdAndReciveToResValue;
  end;
end;

function TRedisSimpleClient.RPUSH(const ListKey: string;
  Values: TArray<String>): boolean;
begin
  result := RPUSH(listKey, ArrStrToArrBytes( Values));
end;

function TRedisSimpleClient.SADD(const Key: string;
  Values: TArray<String>): boolean;
begin
  result := false;
  if length( values) > 0 then
  begin
    GetCmdList('SADD').add( Key).addRange( Values);
    result := SendCmdAndReciveToResValue;
  end;
end;

function TRedisSimpleClient.SCARD(const key: string): boolean;
begin
  GetCmdList('SCARD').add( Key);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.Select(const DBIndex: Integer): boolean;
begin
  GetCmdList('SELECT').Add(DBIndex.ToString);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.SendCmd(aCmd: TRedisCommand): Boolean;
begin
  fsocket.ClearReadBuffer;  //既然要发送数据了， 那么过往的数据已经失效，
                            //如果还存在于 buffer 之中， 会导致识别错误
                            //进而导致接下来的所有指令都不正常

  if FAutoConnect and not fsocket.Connected then
    self.ConnectNeed;
  result := inherited SendCmd( aCmd);
end;

function TRedisSimpleClient.SendCmdAndReciveToResValue: boolean;
begin
  result := SendCmd( FCmd) and ReciveToResValue;
end;

function TRedisSimpleClient.SetRange(const Key: string; Pos: integer;
  value: TBytes): boolean;
begin
  GetCmdList('SETRANGE').add( Key).add(Pos).add( value);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.Set_(const key: string; const value: TBytes;
  ASecsExpire: NativeInt): boolean;
begin
  result := set_( FEncoding.GetBytes( key), value, ASecsExpire);
end;

function TRedisSimpleClient.SetRange(const Key: string; Pos: integer;
  value: string): boolean;
begin
  result := SETRANGE( key, pos, FEncoding.GetBytes( value));
end;

function TRedisSimpleClient.Set_(const key, value: TBytes;
  ASecsExpire: NativeInt): boolean;
var
  ref: refTRedisCommand;
begin
  ref := GetCmdList('SET').Add( key).add( value);
  if ASecsExpire <> -10 then
  begin
    ref.Add('EX');
    ref.Add( inttostr( ASecsExpire));
  end;
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.SISMEMBER(const Key, value: string): boolean;
begin
  GetCmdList('SISMEMBER').add( Key).add( value);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.SMEMBERS(const Key: string): boolean;
begin
  GetCmdList('SMEMBERS').add( Key);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.SREM(const Key: string;
  Values: TArray<String>): boolean;
begin
  result := false;
  if length( values) > 0 then
  begin
    GetCmdList('SREM').add( Key).addRange( Values);
    result := SendCmdAndReciveToResValue;
  end;

end;

function TRedisSimpleClient.Set_(const key, value: string;
  ASecsExpire: NativeInt): boolean;
begin
  result := set_( FEncoding.GetBytes( key), FEncoding.GetBytes( value), ASecsExpire);
end;

function TRedisSimpleClient.StrLen(const Key: string): boolean;
begin
  result := StrLen(FEncoding.GetBytes( Key));
end;

function TRedisSimpleClient.StrLen(const Key: TBytes): boolean;
begin
  GetCmdList('STRLEN').add( Key);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.Ttl(const Key: string): Boolean;
begin
  result := ttl( FEncoding.GetBytes( key));
end;

function TRedisSimpleClient.Ttl(const Key: TBytes): Boolean;
begin
  GetCmdList('TTL').add( Key);
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.Unwatch: boolean;
begin
  GetCmdList('UNWATCH');
  result := SendCmdAndReciveToResValue;
end;

function TRedisSimpleClient.Watch(const Keys: TArray<String>): boolean;
begin
  result := Watch( ArrStrToArrBytes( keys));
end;

function TRedisSimpleClient.Watch(const Keys: TArray<TBytes>): boolean;
begin
  result := CheckArrayParam( Keys);
  if result then
  begin
    GetCmdList('WATCH').AddRange( Keys);
    result := SendCmdAndReciveToResValue;
  end;

end;

{ TSafeRedisSimpleClient }

constructor TSafeRedisSimpleClient.Create(aEncoding: TEncoding);
begin
  inherited Create(aEncoding);
  FCS := TCriticalSection.create;
end;

destructor TSafeRedisSimpleClient.Destroy;
begin
  inherited destroy;
  FCS.DisposeOf;
end;

procedure TSafeRedisSimpleClient.Lock;
begin
  fcs.enter;
end;

procedure TSafeRedisSimpleClient.UnLock;
begin
  fcs.leave;
end;

end.
