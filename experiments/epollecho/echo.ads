with GNAT.Sockets;
with Async.Epoll;

package Echo is
    type Echo_State is record
        Server_Addr : GNAT.Sockets.Sock_Addr_Type;
    end record;

    procedure Call_Me_Back (Sock : GNAT.Sockets.Socket_Type;
        Ctx : Echo_State);
end Echo;
