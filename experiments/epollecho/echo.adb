with Ada.Text_IO,
        GNAT.Sockets,
        Async.Epoll;

use Ada.Text_IO;

package body Echo is
    procedure Call_Me_Back (Sock : GNAT.Sockets.Socket_Type;
                            Ctx : Async.Epoll.Context_Type) is
    begin
        Put_Line (">>> Starting Call_Me_Back");
    end Call_Me_Back;
end Echo;

