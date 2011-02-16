--
--  epoll.adb - Loose binding on top of the epoll(7) interface in the Linux
--  kernel
--


package body Async.Epoll is

    procedure Register (This : in out Hub;
                        Descriptor : in C.int;
                        Cb : in Callback_Tuple) is
        Event : aliased Epoll_Event;
    begin
        Validate_Hub (This);

        if Descriptor <= 0 then
            raise Invalid_Descriptor;
        end if;

        Event.Events := EPOLLIN;
        Event.Data.Fd := Descriptor;

        declare
            Status : C.int;
        begin
            Status := Epoll_Ctl (This.Epoll_fd, EPOLL_CTL_ADD, Descriptor, Event'Access);

            if Status = -1 then
                raise Descriptor_Registration_Falied;
            end if;
        end;

        Callback_Registry.Insert (This.Callbacks, Natural(Descriptor), Cb);
    end Register;


    procedure Run (This : in Hub) is
    begin
        Validate_Hub (This);

        while This.Should_Continue loop
            Wait_Loop :
                declare
                    Max_Events : constant C.int := 10;
                    Events : Epoll_Event_Array(0 .. C.size_t (Max_Events));
                    Num_Descriptors : C.int := Epoll_Wait (This.Epoll_Fd,
                                                            Events,
                                                            Max_Events,
                                                            This.Timeout);
                begin
                    if Num_Descriptors < 0 then
                        raise Epoll_Wait_Failure;
                    end if;

                    for Index in 0 .. Num_Descriptors loop
                        declare
                            Event : constant Epoll_Event :=
                                                    Events (C.size_t (Index));
                            Descriptor : constant C.int :=
                                                    Event.Data.Fd;
                            Cb : constant Callback_Tuple :=
                                                    Callback_Registry.Element (This.Callbacks,
                                                                                Natural (Descriptor));
                        begin
                            Cb.Callback.all (Descriptor, Cb.Context);
                        end;
                    end loop;
                end Wait_Loop;
        end loop;

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


    procedure Validate_Hub (H : in Hub) is
    begin
        Debug_Trace (H, "Entering Validate_Hub");
        if H.Epoll_Fd < 0 then
            raise Hub_Invalid;
        end if;
    end Validate_Hub;


    procedure Debug_Trace (H : in Hub; Line : in String) is
    begin
        if not H.Debug then
            return;
        end if;
    end Debug_Trace;

end Async.Epoll;
