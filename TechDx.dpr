program TechDx;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {TabForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTabForm, TabForm);
  Application.Run;
end.
