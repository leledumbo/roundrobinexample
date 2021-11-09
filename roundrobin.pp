unit RoundRobin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, fpTimer, fgl;

type

  { TJob }

  TJob = class(TThread)
  private
    FWorking: Boolean;
    FEventObj: TEventObject;
    procedure SetWorking(AValue: Boolean);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoWork; virtual; abstract;
    procedure Terminate;
    property Working: Boolean read FWorking write SetWorking;
  end;

  { TJobList }

  TJobList = class(specialize TFPGObjectList<TJob>)
  private
    FCurrentIndex: Integer;
    function GetCurrentJob: TJob;
  public
    constructor Create;
    destructor Destroy; override;
    procedure NextJob;
    procedure StartAll;
    procedure StopAll;
    property CurrentJob: TJob read GetCurrentJob;
  end;

  { TRoundRobinTimer }

  TRoundRobinTimer = class(TFPTimer)
  private
    FJobList: TJobList;
    FOnTimerCalled: TNotifyEvent;
    procedure ContextSwitch(Sender: TObject);
    procedure SetOnTimerCalled(AValue: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartTimer; override;
    procedure StopTimer; override;
    procedure AddJob(AJob: TJob);
    property OnTimerCalled: TNotifyEvent read FOnTimerCalled write SetOnTimerCalled;
  end;

implementation

{ TJob }

constructor TJob.Create;
begin
  // always create in suspended mode
  inherited Create(true);
  FEventObj := TEventObject.Create(nil,true,false,'');
  FWorking := false;
end;

destructor TJob.Destroy;
begin
  FEventObj.Free;
  inherited Destroy;
end;

procedure TJob.SetWorking(AValue: Boolean);
begin
  if FWorking = AValue then Exit;
  FWorking := AValue;
  FEventObj.SetEvent;
end;

procedure TJob.Execute;
begin
  repeat
    FEventObj.WaitFor(INFINITE);
    FEventObj.ResetEvent;
    DoWork;
  until Terminated;
end;

procedure TJob.Terminate;
begin
  (Self as TThread).Terminate;
  FEventObj.SetEvent;
end;

{ TJobList }

constructor TJobList.Create;
begin
  inherited Create(true);
  FCurrentIndex := 0;
end;

destructor TJobList.Destroy;
begin
  StopAll;
  inherited Destroy;
end;

function TJobList.GetCurrentJob: TJob;
begin
  Result := Items[FCurrentIndex];
end;

procedure TJobList.NextJob;
begin
  Inc(FCurrentIndex);
  if FCurrentIndex >= Count then FCurrentIndex := 0;
end;

procedure TJobList.StartAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Start;
end;

procedure TJobList.StopAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Terminate;
end;

{ TRoundRobinTimer }

procedure TRoundRobinTimer.ContextSwitch(Sender: TObject);
begin
  if Assigned(FOnTimerCalled) then FOnTimerCalled(Sender);
  FJobList.CurrentJob.Working := false;
  FJobList.NextJob;
  FJobList.CurrentJob.Working := true;
end;

constructor TRoundRobinTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FJobList := TJobList.Create;
  FOnTimerCalled := nil;
  OnTimer := @ContextSwitch;
end;

destructor TRoundRobinTimer.Destroy;
begin
  Enabled := false;
  FJobList.Free;
  inherited Destroy;
end;

procedure TRoundRobinTimer.StartTimer;
begin
  FJobList.StartAll;
  FJobList.CurrentJob.Working := true;
  inherited StartTimer;
end;

procedure TRoundRobinTimer.StopTimer;
begin
  inherited StopTimer;
  FJobList.StopAll;
end;

procedure TRoundRobinTimer.AddJob(AJob: TJob);
begin
  if Enabled then
    raise Exception.Create('Please stop the timer before adding new job(s)');
  FJobList.Add(AJob);
end;

procedure TRoundRobinTimer.SetOnTimerCalled(AValue: TNotifyEvent);
begin
  if FOnTimerCalled = AValue then Exit;
  FOnTimerCalled := AValue;
end;

end.

