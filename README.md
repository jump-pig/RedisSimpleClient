# Redis simple client

一个简单小巧的 redis 客户端， 封装了常用的功能，包括 hash set 和 get， queue 之类的命令，也包括事务 Watch unWatch。

核心单元： uRedisSimpleClient.pas

这个客户端只在我自己的一个项目上运行。至今还没发觉有 BUG。

代码借鉴了 dephiredisclient：

https://github.com/danieleteti/delphiredisclient

delphiredisclient 用上了工厂模式等 oop 技术， 但我觉得它对 redis 的通讯理解（包括事务）有点不对。所以重新封装了这一个客户端。

两相比较， RedisSimpleClient 提供了更可控的 API 交互模式。

server 开发的要求是严谨的，这个客户端被设计成自动连接的工作模式，它会自己判断并一直维系连接的可靠性。

对于 redis db index （数据库索引）的维持，也是自动化的，指定 forceDbIndex = y 即可， 当TCP重连的时候，它会保证你依然连接到指定的 db index。

初始化的代码如下：

    cli := TRedisSimpleClient.Create( TEncoding.UTF8);
    cli.Host := '127.0.0.1';
    cli.Port := 6379;
    cli.ConnectTimeout := 1000;
    cli.AuthUserName := '000000';
    cli.ForceDbIndex := -1;
  
操作代码如下：

    if cli.Get('ok') then  //ok exist
    begin
      if cli.ResValue.ValueTypeIsBulk then  //ok have data
        add('get(ok)= ' + cli.ResValue.GetBulkAsString)  
      else
      if cli.ResValue.ValueTypeIsNullBulk then  //value of ok is null
        add('get(ok)=  null');
    end;
    
你会注意到： redis 在执行一个命令后， 就会返回一个固定的数据类型，我把它封装在了 cli.ResValue, cli.resValue.ValueType:

TRedisValueType = (rvtNone, rvtErr, rvtNullArray, rvtNullBulk, rvtOK, rvtQueued, rvtInt, rvtBulk, rvtArray);
    
事务的执行是这样的：

    cli.Watch(['ok']);
    cli.Get('ok');
    cli.Multi;
    cli.Set_('no', '2');
    cli.HSet('h', 'f1', 'multiset v');
    cli.HSet('h', 'f2', 'multiset 2');
    cli.Exec;
  
目前只是封装了常用的命令，如果你需要的不在列，请参考 delphiredisclient，把命令追加上去（很容易做到）。  
  
介绍完毕
