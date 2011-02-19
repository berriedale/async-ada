with GNAT.Sockets,
            Async.Epoll;

use GNAT.Sockets;

package Echo is
    type Echo_State is record
        Server_Addr : Sock_Addr_Type;
        Listening_Socket : Socket_Type;
    end record;

    procedure Call_Me_Back (Sock : GNAT.Sockets.Socket_Type;
        Ctx : Echo_State);
end Echo;
