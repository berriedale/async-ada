with Ada.Text_IO,
        GNAT.Sockets,
        Async.Epoll;

use Ada.Text_IO,
    GNAT.Sockets;

package body Echo is
    procedure Call_Me_Back (Sock : Socket_Type;
                            Ctx : Async.Epoll.Context_Type) is
        New_Sock : Socket_Type;
    begin
        Put_Line (">>> Starting Call_Me_Back");

        Accept_Socket(Sock, New_Sock);
    end Call_Me_Back;
end Echo;

