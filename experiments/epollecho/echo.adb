with Ada.Text_IO,
        GNAT.Sockets,
        Async.Epoll;

use Ada.Text_IO,
    GNAT.Sockets;

package body Echo is
    procedure Call_Me_Back (Sock : Socket_Type;
                            Ctx : Echo_State) is
        Addr : Sock_Addr_Type := Ctx.Server_Addr;
    begin
        Put_Line (">>> Starting Call_Me_Back");

        if Sock = Ctx.Listening_Socket then
            Put_Line (">>> Accept_Socket");
            declare
                New_Sock : Socket_Type;
            begin
                Accept_Socket(Sock, New_Sock, Addr);
            end;
            return;
        end if;

        Put_Line (">>> Ready to read data");
    end Call_Me_Back;
end Echo;

