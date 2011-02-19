with GNAT.Sockets;
with Async.Epoll;

package Echo is
    type Echo_State is new Async.Epoll.Context_Type with record
        Server_Addr : GNAT.Sockets.Sock_Addr_Type;
    end record;

    procedure Call_Me_Back (Sock : GNAT.Sockets.Socket_Type;
        Ctx : Async.Epoll.Context_Type);
end Echo;
