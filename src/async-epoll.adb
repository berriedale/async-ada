--
--  epoll.adb - Loose binding on top of the epoll(7) interface in the Linux
--  kernel
--


private with Ada.Strings.Hash,
            Ada.Text_IO;

use Ada.Text_IO;

package body Async.Epoll is

    procedure Register (This : in out Hub;
                        Cb : in Callback_Tuple) is
        Event : aliased Epoll_Event;
        Descriptor : constant C.int := C.int (GNAT.Sockets.To_C (Cb.Socket));
        Descriptor_Str : constant String := Natural'Image (Natural(Descriptor));
    begin
        Validate_Hub (This);

        This.Debug_Trace ("<Register>> The Hub is valid");

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

            This.Debug_Trace ("<Register>> Successfully added descriptor:" &
                                    Descriptor_Str & " to the Epoll_Fd");
        end;

        This.Debug_Trace ("<Register>> Inserting descriptor:" & Descriptor_Str);

        Callback_Registry.Insert (This.Callbacks, Descriptor, Cb);
    end Register;


    procedure Run (This : in Hub) is
    begin
        Validate_Hub (This);

        This.Debug_Trace ("<Run>> The Hub is valid");

        while This.Should_Continue loop
            This.Debug_Trace ("<Run>> Continuing run loop");

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

                    This.Debug_Trace ("<Run>> Epoll_Wait returned with changes on" &
                                            C.int'Image (Num_Descriptors) &
                                            " descriptors");

                    if Num_Descriptors > 0 then
                        for Index in 0 .. (Num_Descriptors - 1) loop
                            This.Debug_Trace ("<Run>> Descriptor loop, index:" & C.int'Image (Index));

                            declare
                                Event : constant Epoll_Event :=
                                                        Events (C.size_t (Index));
                                Descriptor : constant C.int :=
                                                        Event.Data.Fd;
                                Cb : constant Callback_Tuple :=
                                                        Callback_Registry.Element (This.Callbacks,
                                                                                    Descriptor);
                            begin
                                Cb.Callback.all (Cb.Socket, Cb.Context);
                            end;
                        end loop;
                    end if;
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
        if H.Epoll_Fd < 0 then
            raise Hub_Invalid;
        end if;
    end Validate_Hub;

    procedure Enable_Tracing (H : in out Hub) is
    begin
        H.Debug := True;
    end Enable_Tracing;

    procedure Debug_Trace (H : in Hub; Line : in String) is
    begin
        if not H.Debug then
            return;
        end if;
        Put_Line (Line);
    end Debug_Trace;

    function Descriptor_Hash (Id : in C.int) return Hash_Type is
    begin
        return Ada.Strings.Hash (C.int'Image (Id));
    end Descriptor_Hash;

end Async.Epoll;
