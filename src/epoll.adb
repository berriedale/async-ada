

package body Epoll is

    procedure Register (This : in Hub; Descriptor : in C.int) is
    begin
        null;
    end Register;

    procedure Run (This : in Hub) is
    begin
        null;
    end Run;

    function Create return Hub is
        Created_Hub : Hub;
        Epoll_Fd : C.int;
    begin
        -- The "size" argument for Epoll_Create is unused, so we'll just
        -- default it to 10, because hey why not
        Epoll_Fd := Epoll_Create (10);

        if Epoll_Fd = -1 then
            raise Hub_Create_Failed;
        end if;

        Created_Hub.Epoll_Fd := Epoll_Fd;

        return Created_Hub;
    end Create;

end Epoll;
