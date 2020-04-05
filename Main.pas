unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Gestures, FMX.Controls.Presentation, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView, FMX.Edit,
  FMX.Layouts, FMX.ListBox, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, FMX.ScrollBox, FMX.Memo;

type
  TTabForm_Main = class(TForm)
    GestureManager_Main: TGestureManager;
    StyleBook_Main: TStyleBook;
    TCPClient_Main: TIdTCPClient;

    ToolBar_Main: TToolBar;
    Label_Main: TLabel;
    TabControl_Main: TTabControl;

    TabItem_Client: TTabItem;
    Edit_Client_Remote_IP: TEdit;
    Edit_Client_Remote_Port: TEdit;
    Button_Client_Connect: TButton;
    GridPanelLayout_Client: TGridPanelLayout;
    Memo_Client_Console: TMemo;
    Memo_Client_Message: TMemo;
    Button_Client_Send: TButton;

    TabItem_Server: TTabItem;
    TabItem_ANIALI: TTabItem;
    TabItem_ProQA: TTabItem;

    function GetNow():String;

    procedure FormCreate(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);

    procedure Button_Client_Connect_OnClick(Sender: TObject);

    Procedure Client_Log(Message_Type: String; Message :String);
    procedure Button_Client_Send_OnClick(Sender: TObject);
    procedure TCPClient_Main_OnConnected(Sender: TObject);
    procedure TCPClient_Main_OnDisconnected(Sender: TObject);

  private
  public
end;

var TabForm_Main: TTabForm_Main;
var Client_Connected: Boolean;

implementation

{$R *.fmx}

function TTabForm_Main.GetNow() : String;
begin
    result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
end;

procedure TTabForm_Main.FormCreate(Sender: TObject);
begin
  Application.Title := 'Technician Diagnostics';
  TabControl_Main.ActiveTab := TabItem_Client;
  Client_Connected := False;
end;

procedure TTabForm_Main.FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
{$IFDEF ANDROID}
  case EventInfo.GestureID of
    sgiLeft:
    begin
      if TabControl_Main.ActiveTab <> TabControl_Main.Tabs[TabControl_Main.TabCount-1] then
        TabControl_Main.ActiveTab := TabControl_Main.Tabs[TabControl_Main.TabIndex+1];
      Handled := True;
    end;

    sgiRight:
    begin
      if TabControl_Main.ActiveTab <> TabControl_Main.Tabs[0] then
        TabControl_Main.ActiveTab := TabControl_Main.Tabs[TabControl_Main.TabIndex-1];
      Handled := True;
    end;
  end;
{$ENDIF}
end;

procedure TTabForm_Main.TCPClient_Main_OnConnected(Sender: TObject);
begin
  Client_Log('ST', 'Connected to server.');
end;

procedure TTabForm_Main.TCPClient_Main_OnDisconnected(Sender: TObject);
begin
  Client_Log('ST', 'Disconnected from server.');
end;

procedure TTabForm_Main.Button_Client_Connect_OnClick(Sender: TObject);
begin
  Client_Connected := not Client_Connected;
  if (Client_Connected)
  then
    begin
      try
        TCPClient_Main.Host := Edit_Client_Remote_IP.Text;
        TCPClient_Main.Port := Edit_Client_Remote_Port.Text.ToInteger;
        TCPClient_Main.Connect();
        Memo_Client_Message.Enabled := True;
        Button_Client_Send.Enabled := True;
        Button_Client_Connect.Text := 'Disconnect';
      except
        Client_Connected := False;
        Client_Log('ST', 'Connect failed.');
      end;
    end
  else
    begin
      try
        TCPClient_Main.Disconnect();
      finally
        Memo_Client_Message.Enabled := False;
        Button_Client_Send.Enabled := False;
        Button_Client_Connect.Text := 'Connect';
      end
    end
end;

procedure TTabForm_Main.Client_Log(Message_Type: String; Message: String);
begin
  TThread.Queue(nil, procedure
    begin
      Memo_Client_Console.Lines.Add(GetNow() + ' ' + Message_Type);
      Memo_Client_Console.Lines.Add(Message + chr(13) + chr(10));
      Memo_Client_Console.SelStart:=Memo_Client_Console.Lines.Text.Length-1;
    end);
end;

procedure TTabForm_Main.Button_Client_Send_OnClick(Sender: TObject);
begin
  try
    TCPClient_Main.Socket.WriteLn(Memo_Client_Message.Text);
    Client_Log('TX', Memo_Client_Message.Text);
    Memo_Client_Message.Lines.Clear();
  except
    Button_Client_Connect_OnClick(Sender);
    Client_Log('ST', 'Send failed.');
  end;
  Memo_Client_Message.SetFocus();
end;

end.
