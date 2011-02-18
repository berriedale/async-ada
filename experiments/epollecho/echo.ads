with GNAT.Sockets;
with Async.Epoll;

package Echo is
    procedure Call_Me_Back (Sock : GNAT.Sockets.Socket_Type;
                        Ctx : Async.Epoll.Context_Type);
end Echo;
