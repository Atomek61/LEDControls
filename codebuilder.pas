unit codebuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TCodeBuilder }

  TCodeBuilder = class
  private
    FTemplate: string;
  public

    property Template :string read FTemplate write FTemplate;
    property DefaultFloatFormat :string read FDefaultFloatFormat write FDefaultFloatFormat;
  end;

implementation

end.

