unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Gestures, FMX.Controls.Presentation, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView, FMX.Edit,
  FMX.Layouts, FMX.ListBox, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, FMX.ScrollBox, FMX.Memo, IDThreadComponent, IDGlobal, System.StrUtils;

type
  TTabForm_Main = class(TForm)
    GestureManager_Main: TGestureManager;
    StyleBook_Main: TStyleBook;
    TCPClient_Main: TIDTCPClient;

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
    ThreadComponent_Main: TIDThreadComponent;

    function GetNow():String;

    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);

    procedure FormCreate(Sender: TObject);

    procedure TCPClient_Main_OnConnected(Sender: TObject);
    procedure TCPClient_Main_OnDisconnected(Sender: TObject);
    procedure TCPClient_Main_OnStatus(ASender: TObject; const AStatus: TIDStatus; const AStatusText: String);
    procedure ThreadComponent_Main_OnRun(Sender: TIDThreadComponent);

    procedure Button_Client_Connect_OnClick(Sender: TObject);
    Procedure Client_Log(Message_Type: String; Message :String);
    procedure Button_Client_Send_OnClick(Sender: TObject);

  private
  public
end;

var TabForm_Main: TTabForm_Main;
var Client_Connected: Boolean;
var CRLF: String;

implementation

{$R *.fmx}

function TTabForm_Main.GetNow() : String;
begin
    result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
end;

procedure TTabForm_Main.FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
{$IFDEF ANDROID}
  case EventInfo.GestureID of
    SGILeft:
    begin
      if TabControl_Main.ActiveTab <> TabControl_Main.Tabs[TabControl_Main.TabCount - 1] then
        TabControl_Main.ActiveTab := TabControl_Main.Tabs[TabControl_Main.TabIndex + 1];
      Handled := True;
    end

    SGIRight:
    begin
      if TabControl_Main.ActiveTab <> TabControl_Main.Tabs[0] then
        TabControl_Main.ActiveTab := TabControl_Main.Tabs[TabControl_Main.TabIndex - 1];
      Handled := True;
    end
  end
{$ENDIF}
end;

procedure TTabForm_Main.FormCreate(Sender: TObject);
begin
  Application.Title := 'Technician Diagnostics';
  TabControl_Main.ActiveTab := TabItem_Client;
  Client_Connected := False;
  CRLF := Chr(13) + Chr(10);
end;

procedure TTabForm_Main.TCPClient_Main_OnConnected(Sender: TObject);
begin
  ThreadComponent_Main.Active := True;
  Memo_Client_Message.Enabled := True;
  Button_Client_Send.Enabled := True;
  Button_Client_Connect.Text := 'Disconnect';
  Memo_Client_Message.SetFocus();
  Client_Log('ST', 'Connected to server.');
end;

procedure TTabForm_Main.TCPClient_Main_OnDisconnected(Sender: TObject);
begin
  ThreadComponent_Main.Active := False;
  Memo_Client_Message.Enabled := False;
  Button_Client_Send.Enabled := False;
  Button_Client_Connect.Text := 'Connect';
  Button_Client_Connect.SetFocus();
  Client_Log('ST', 'Disconnected from server.');
end;

procedure TTabForm_Main.TCPClient_Main_OnStatus(ASender: TObject; const AStatus: TIDStatus; const AStatusText: String);
begin
  if (not TCPClient_Main.Connected) and (Client_Connected)
  then
    begin
      Client_Connected := False;
      Client_Log('ST', 'Server terminated connection.');
      TCPClient_Main_OnDisconnected(ASender);
    end
end;

procedure TTabForm_Main.ThreadComponent_Main_OnRun(Sender: TIDThreadComponent);
var Bytes: TIDBytes;
var Message: String;

begin
  if TCPClient_Main.IOHandler.InputBufferIsEmpty
  then
    begin
      TCPClient_Main.IOHandler.CheckForDataOnSource(250);
      TCPClient_Main.IOHandler.CheckForDisconnect;
      if TCPClient_Main.IOHandler.InputBufferIsEmpty
        then
          begin
            Exit;
          end
    end;

  TCPClient_Main.IOHandler.ReadBytes(Bytes, TCPClient_Main.IOHandler.InputBuffer.Size);
  Message := BytesToString(Bytes,IndyTextEncoding_UTF8);
  Client_Log('RX', Message);
end;

procedure TTabForm_Main.Button_Client_Connect_OnClick(Sender: TObject);
begin
  Client_Connected := not Client_Connected;
  if (Client_Connected)
  then
    begin
      try
        TCPClient_Main.ConnectTimeout := 2500;
        TCPClient_Main.Host := Edit_Client_Remote_IP.Text;
        TCPClient_Main.Port := Edit_Client_Remote_Port.Text.ToInteger;
        TCPClient_Main.Connect();
       except
        on E: Exception do
          begin
            Client_Connected := False;
            Client_Log('ST', '** Connect_Exception **' + CRLF + E.Message);
          end
      end
    end
  else
    begin
      try
        TCPClient_Main.Disconnect;
      except
        on E: Exception do
          begin
            Client_Connected := False;
            Client_Log('ST', '** Disconnect_Exception **' + CRLF + E.Message);
            TCPClient_Main_OnDisconnected(Sender);
            TCPClient_Main.IOHandler.InputBuffer.Clear;
            TCPClient_Main.IOHandler.CloseGracefully;
            TCPClient_Main.Disconnect;
          end
      end
    end
end;

procedure TTabForm_Main.Client_Log(Message_Type: String; Message: String);
begin
  TThread.Queue(nil, procedure
    begin
      while (RightStr(Message,1) = Chr(13)) or (RightStr(Message,1) = Chr(10)) do
      begin
        Message := LeftStr(Message, Message.Length - 1);
      end;

      Memo_Client_Console.Lines.Add(GetNow() + ' ' + Message_Type);
      Memo_Client_Console.Lines.Add(Message + CRLF);
      Memo_Client_Console.SelStart := Memo_Client_Console.Lines.Text.Length - 1;
    end
  );
end;

procedure TTabForm_Main.Button_Client_Send_OnClick(Sender: TObject);
begin
  try
    TCPClient_Main.Socket.WriteLn(Memo_Client_Message.Text);
    Client_Log('TX', Memo_Client_Message.Text);
    Memo_Client_Message.Lines.Clear();
  except
    on E: Exception do
      begin
        Client_Connected := False;
        Client_Log('ST', '** Send_Exception **' + CRLF + E.Message);
        TCPClient_Main_OnDisconnected(Sender);
        TCPClient_Main.IOHandler.InputBuffer.Clear;
        TCPClient_Main.IOHandler.CloseGracefully;
        TCPClient_Main.Disconnect;
      end
  end;
  Memo_Client_Message.SetFocus();
end;

end.
