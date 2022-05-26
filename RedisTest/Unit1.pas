unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uRedisSimpleClient, Vcl.StdCtrls,
  Vcl.Buttons, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    BitBtn1: TBitBtn;
    IdTCPClient1: TIdTCPClient;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    BitBtn15: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure test1;
    procedure test2;
    procedure test3;
    procedure test4;
    procedure Add(s: string);
  end;

var
  Form1: TForm1;
  cli: TRedisSimpleClient;

implementation

{$R *.dfm}

procedure TForm1.Add(s: string);
begin
  memo1.Lines.Add(s)
end;

procedure TForm1.BitBtn10Click(Sender: TObject);
begin
  cli.SISMEMBER('sets', 'aa');
  if cli.ResValue.ValueTypeIsInt then
    caption := cli.ResValue.GetInteger.ToString

end;

procedure TForm1.BitBtn11Click(Sender: TObject);
begin
  cli.SMEMBERS('sets');
  if cli.ResValue.ValueTypeIsArray then
  begin
    if cli.ResValue.ArrCount > 0 then
      caption := cli.ResValue.ArrCount.ToString
  end;
end;

procedure TForm1.BitBtn12Click(Sender: TObject);
begin
  cli.SREM('sets', ['aa','cc']);
  if cli.ResValue.ValueTypeIsInt then
    caption := cli.ResValue.GetInteger.ToString

end;

procedure TForm1.BitBtn13Click(Sender: TObject);
var
  b: TBytes;
begin
  setlength( b, 3);
  b[0] := 50;
  b[1] := 0;
  b[2] := 0;
  cli.SETRANGE('c', 2, b);
  if cli.ResValue.ValueTypeIsInt then
    caption := cli.ResValue.GetInteger.ToString
  else
  if cli.ResValue.ValueTypeIsBulk then
    caption := cli.ResValue.GetBulkAsString;
end;

procedure TForm1.BitBtn15Click(Sender: TObject);
var
  b: TBytes;
begin
  cli.getRANGE('c', 2, 5);
  if cli.ResValue.ValueTypeIsInt then
    caption := cli.ResValue.GetInteger.ToString
  else
  if cli.ResValue.ValueTypeIsBulk then
  begin
    b := cli.ResValue.GetBulkAsBytes;
    //caption := cli.ResValue.GetBulkAsString;
  end;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  if cli.Watch(['a']) then
    if cli.ResValue.ValueType = cli.ResValue._Watch then
    begin
      memo1.Lines.Add('ok');
      cli.Multi
    end;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  cli.Set_('b', '4');
  cli.Exec;
  if cli.ResValue.ValueType = cli.ResValue._Exec then
    memo1.Lines.Add('ok');
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
var
  s: string;
begin
  s := memo1.Lines.Text;
  cli.RPUSH('pp', ['yes', 'no']);
  Memo1.Lines.Add( cli.ResValue.GetInteger.ToString);
//  cli.Set_('ok', s);
//  cli.Get('ok');
//  memo1.Lines.Add( cli.ResValue.GetBulkAsString);

end;

procedure TForm1.BitBtn4Click(Sender: TObject);
var
  i: Integer;
begin
  cli.LRANGE('pp', 0, 10);
  if cli.ResValue.ValueTypeIsBulk then
    memo1.Lines.Add( cli.ResValue.GetBulkAsString)
  else
  if cli.ResValue.ValueTypeIsArray then
    for i := 0 to cli.ResValue.ArrCount - 1 do
      memo1.Lines.Add( cli.ResValue.Items[i].GetBulkAsString)
end;

procedure TForm1.BitBtn5Click(Sender: TObject);
begin
  cli.LReM('pp', 2, 'no');
//  cli.LLEN('pp');
  Memo1.Lines.Add(cli.ResValue.GetInteger.ToString)
end;

procedure TForm1.BitBtn6Click(Sender: TObject);
var
  b: TBytes;
begin
  b := TEncoding.Unicode.GetBytes( 'okok中国');

  cli.SET_(BytesOf('ok'), b);
  cli.Get('ok');

  memo1.Lines.Add( TEncoding.UTF8.GetString(cli.ResValue.GetBulkAsBytes))


end;

procedure TForm1.BitBtn7Click(Sender: TObject);
var
  e: TEncoding;
begin
  e := TEncoding.UTF8;
  caption := e.ClassName;
exit;
 if cli.LINDEX('pp', 2) then
  memo1.Lines.Add( cli.ResValue.GetBulkAsString);

end;

procedure TForm1.BitBtn8Click(Sender: TObject);
begin
  cli.SADD('sets', ['aa', 'bb', 'cc']);
  if cli.ResValue.ValueTypeIsInt then
    caption := cli.ResValue.GetInteger.ToString
  else
  if cli.ResValue.ValueTypeIsBulk then
    caption := cli.ResValue.GetBulkAsString;

end;

procedure TForm1.BitBtn9Click(Sender: TObject);
begin
  cli.SCARD('sets');
  if cli.ResValue.ValueTypeIsInt then
    caption := cli.ResValue.GetInteger.ToString
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if cli.GetConnected then
    cli.Disconnect;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  b: TBytes;
  i: integer;
begin
  cli := TRedisSimpleClient.Create(TEncoding.UTF8);
  cli.Host := '127.0.0.1';
  cli.Port := 6379;
  cli.ConnectTimeout := 1000;
  cli.AuthUserName := '000000';
  cli.ForceDbIndex := -1;
  //cli.ConnectNeed;


    //test1;
    //test2;
   // test3;
  //  test4




end;

procedure TForm1.test1;
begin


    if cli.Select( 0) and cli.ResValue.ValueTypeIsOK then
      add('select');
    if cli.Set_('ok', 'yes') and cli.ResValue.ValueTypeIsOK then
      add( 'set(ok)');
    if cli.Get('ok') and cli.ResValue.ValueTypeIsBulk then
      add('get(ok)= ' + cli.ResValue.GetBulkAsString);
    if cli.Del(['ok']) and cli.ResValue.ValueTypeIsInt then
      add('del ok=' + inttostr( cli.ResValue.getInteger));

    if cli.Get('ok') then
    begin
      if cli.ResValue.ValueTypeIsBulk then
        add('get(ok)= ' + cli.ResValue.GetBulkAsString)
      else
      if cli.ResValue.ValueTypeIsNullBulk then
        add('get(ok)=  null');
    end;

    if cli.Set_('ok', 'yes', 1) and cli.ResValue.ValueTypeIsOK then
      Sleep( 1500);

    if cli.Get('ok') then
    begin
      if cli.ResValue.ValueTypeIsBulk then
        add('get(ok)= ' + cli.ResValue.GetBulkAsString)
      else
      if cli.ResValue.ValueTypeIsNullBulk then
        add('get(ok)=  null');
    end;

    cli.Set_('ok', 'yes');
    if cli.Expire( 'ok', 1) then
    begin
      Add('expire');
      sleep(500);
      if cli.Get('ok') then
      begin
        if cli.ResValue.ValueTypeIsBulk then
          add('tick 500 = get(ok)= ' + cli.ResValue.GetBulkAsString)
        else
        if cli.ResValue.ValueTypeIsNullBulk then
          add('get(ok)=  null');
      end;

      if cli.Ttl('ok') then
      begin
        if (cli.ResValue.ValueType = cli.ResValue._Ttl) then
          if cli.ResValue.getInteger >= -1 then
            add('ok timeout at ='+ inttostr( cli.ResValue.getInteger));
      end;

      Sleep( 1000);
      if cli.Get('ok') then
      begin
        if cli.ResValue.ValueTypeIsBulk then
          add('get(ok)= ' + cli.ResValue.GetBulkAsString)
        else
        if cli.ResValue.ValueTypeIsNullBulk then
          add('tick 1500 =  get(ok)=  null');
      end;

      cli.Set_( 'ok', '4');
      if cli.Get('ok') then
      begin
        if cli.ResValue.ValueTypeIsBulk then
          add('get(ok)= ' + cli.ResValue.GetBulkAsString)
        else
        if cli.ResValue.ValueTypeIsNullBulk then
          add('tick 1500 =  get(ok)=  null');
      end;

      if cli.Ttl('ok') then
      begin
        if (cli.ResValue.ValueType = cli.ResValue._Ttl) then
          if cli.ResValue.getInteger >= cli.ResValue._Sub_Ttl_Naver_Timeout then
            add('ok never timeout');
      end;

    end;

end;

procedure TForm1.test2;
var
  i: Integer;
begin
  if cli.Exists('ok') and (cli.ResValue.ValueType = cli.ResValue._Exists) then
  begin
    if (cli.ResValue.GetInteger = cli.ResValue._Sub_Exists) then
      add( 'ok exists')
    else
      Add( 'ok not exists');
  end;

  cli.Set_('ok', '中国AB');
  cli.Set_('okk', 'noyes');
  if cli.Keys('o*') and cli.ResValue.ValueTypeIsArray then
  begin
    for i := 0 to cli.ResValue.ArrCount - 1 do
      add('keys u* = ' + cli.ResValue.Items[i].GetBulkAsString);

  end;

  if cli.StrLen('ok') and cli.ResValue.ValueTypeIsInt then
    add('ok strlen = ' + inttostr( cli.ResValue.GetInteger));

  cli.Set_('num', '1');
  cli.Incr('num');
  cli.Incr('num');
  cli.Decr('num');
  cli.Get('num');
  if cli.ResValue.ValueTypeIsBulk then
    add('num inc dec = ' + cli.ResValue.GetBulkAsString);

  cli.Append('num', 'ssss');
  cli.Get('num');
  if cli.ResValue.ValueTypeIsBulk then
    add('num append = ' + cli.ResValue.GetBulkAsString);



end;

procedure TForm1.test3;
var
  i: integer;
begin
  if cli.MSet(['ok','yes', 'right', 'no']) and cli.ResValue.ValueTypeIsOK then
  begin
    Add('MSet ok');
    cli.MGet(['ok', 'right']);
    Add('begin mget ok right');
    for i := 0 to cli.ResValue.ArrCount - 1 do
      add('res' + IntToStr( i) + '=' + cli.ResValue.Items[i].GetBulkAsString);


    add('begin hash test');

    cli.HSet('h', 'f1', 'v11');
    cli.HSet('h', 'f2', 'v22');
    cli.HGet('h', 'f1');
    add('h f1 =' + cli.ResValue.GetBulkAsString);
    cli.HGet('h', 'f2');
    add('h f2 =' + cli.ResValue.GetBulkAsString);

    add('begin multi hash ');

    cli.Get('ok');
      if cli.ResValue.ValueType = TRedisValueType.rvtBulk then
        add( cli.ResValue.GetBulkAsString) ;

    if cli.HMSet('h', ['f1', 'mv1', 'f2', 'mv2']) then
      Add('hmset ok');

    if cli.ResValue.ValueTypeIsOK then
    begin
      cli.HMGet('h', ['f1', 'f2']);
      if cli.ResValue.ValueType = TRedisValueType.rvtArray then
        Add( 'HMGet res:');

      if cli.ResValue.ValueType = TRedisValueType.rvtBulk then
        add( cli.ResValue.GetBulkAsString)
      else
      for i := 0 to cli.ResValue.ArrCount - 1 do
        Add( cli.ResValue.Items[i].GetBulkAsString);
    end;
  end;
end;

procedure TForm1.test4;
var
  ok: string;
  i: integer;
begin
  add(' begin tick');
  cli.BeginTick;
 // cli.Watch(['ok']);
 // cli.Get('ok');
  cli.Multi;
  cli.Set_('no', '2');
  cli.HSet('h', 'f1', 'multiset v');
  cli.HSet('h', 'f2', 'multiset 2');
  cli.Exec;
  if cli.ResValue.ArrCount > 0 then
  for i := 0 to cli.ResValue.ArrCount - 1 do
  begin
    if cli.ResValue.Items[i].ValueTypeIsOK then
      add( 'result ok')
    else
    if cli.ResValue.Items[i].ValueTypeIsBulk then
      add('result bulk:' + cli.ResValue.Items[i].GetBulkAsString)
    else
    if cli.ResValue.Items[i].ValueTypeIsInt then
      add('result bulk:' + inttostr(  cli.ResValue.Items[i].getinteger))
  end;

  add( 'end tick = ' + inttostr( cli.EndTick));


  cli.mSet(['name', '/sdf', 'right', 'nono']);
  if cli.ResValue.ValueTypeIsOK then
  if  cli.mGet(['name', 'right']) then
    for i := 0 to cli.ResValue.ArrCount - 1 do
      Add( cli.ResValue.Items[i].GetBulkAsString);


end;

end.
