program RoundRobinExample;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}
  cthreads,
  {$endif}
  keyboard,
  Classes,SysUtils,CustApp,RoundRobin;

type

  { TWriteJob }

  TWriteJob = class(TJob)
  private
    FID: String;
  public
    constructor Create(const AID: String);
    procedure DoWork; override;
  end;

constructor TWriteJob.Create(const AID: String);
begin
  inherited Create;
  Randomize;
  FID := AID;
end;

procedure TWriteJob.DoWork;
begin
  repeat
    WriteLn(FID + ': Working..');
    Sleep(Random(1000));
  until not Working or Terminated;
end;

type

  { TRoundRobinApp }

  TRoundRobinApp = class(TCustomApplication)
  private
    RRT: TRoundRobinTimer;
  public
    procedure ShowSwitchingNote(Sender: TObject);
    procedure DoRun; override;
  end;

procedure TRoundRobinApp.DoRun;
var
  K: TKeyEvent;
begin
  InitKeyboard;
  RRT := TRoundRobinTimer.Create(Self);
  RRT.Interval := 1500;
  RRT.OnTimerCalled := @ShowSwitchingNote;

  RRT.AddJob(TWriteJob.Create('Write Job 1'));
  RRT.AddJob(TWriteJob.Create('Write Job 2'));
  RRT.AddJob(TWriteJob.Create('Write Job 3'));

  RRT.Enabled := true;
  repeat
    K := PollKeyEvent;
    if K <> 0 then begin
      K := GetKeyEvent;
      K := TranslateKeyEvent(K);
    end else begin
      CheckSynchronize;
      Sleep(1); // avoid near 100% CPU usage
    end;
  until (UpCase(GetKeyEventChar(K)) = 'Q');
  RRT.Enabled := false;

  RRT.Free;
  DoneKeyboard;
  Terminate;
end;

procedure TRoundRobinApp.ShowSwitchingNote(Sender: TObject);
begin
  WriteLn('Switching to next job after ',RRT.Interval / 1000:1:3,' seconds');
end;

{ Main }

begin
  DefaultTextLineBreakStyle := tlbsCRLF;
  with TRoundRobinApp.Create(nil) do
    try
      Run;
    finally
      Free;
    end;
end.
