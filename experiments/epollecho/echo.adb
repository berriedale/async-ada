with Ada.Text_IO,
        GNAT.Sockets,
        Async.Epoll;

use Ada.Text_IO,
    GNAT.Sockets;

package body Echo is
    procedure Call_Me_Back (Sock : Socket_Type;
                            Ctx : Echo_State) is
        New_Sock : Socket_Type;
        Addr : Sock_Addr_Type := Ctx.Server_Addr;
    begin
        Put_Line (">>> Starting Call_Me_Back");

        Accept_Socket(Sock, New_Sock, Addr);
    end Call_Me_Back;
end Echo;

