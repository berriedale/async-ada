private with Ada.Text_IO;
private with GNAT.Sockets;
private with Async.Epoll;

use Ada.Text_IO;
use GNAT.Sockets;

procedure Call_Me_Back (Sock : Socket_Type;  Ctx : Async.Epoll.Context_Type) is
begin
    Put_Line (">>> Starting Call_Me_Back");
end Call_Me_Back;

