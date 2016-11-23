program t_ddemo;

uses
  Forms,
  t_ddemou in 't_ddemou.pas' {Form1},
  mp_types in 'mp_types.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
