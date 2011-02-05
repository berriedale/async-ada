--
--  Main interface for dealing with epoll(7) from Ada
--

with Interfaces.C;
with System;

use Interfaces;

package Epoll is
    type Callback is access procedure (Descriptor : C.int);
    -- My_Callback.all (Fd);

    type Hub is tagged private;

    procedure Register (This : in Hub; Descriptor : C.int);
    procedure Run (This : in Hub);

    function Create return Hub;

    Hub_Create_Failed : exception;

    private
        use Interfaces.C;

        type Hub is tagged record
            Epoll_Fd : C.int := -1;
        end record;

        type Epoll_Data_Type is (Pointer,
                                File_Descriptor,
                                Int_32,
                                Int_64);

        type Ctl_Operations is (
                    Ctl_Add,
                    Ctl_Mod,
                    Ctl_Del);
        for Ctl_Operations use (
                    Ctl_Add => 1,
                    Ctl_Mod => 2,
                    Ctl_Del => 3);

        type Epoll_Data (D : Epoll_Data_Type := File_Descriptor) is record
            case D is
                when Pointer =>
                    Ptr : System.Address;
                when File_Descriptor =>
                    Fd : C.int;
                when Int_32 =>
                    u32 : C.int;
                when Int_64 =>
                    u64 : C.int;
            end case;
        end record;

        pragma Unchecked_Union (Epoll_Data);

        type Epoll_Event is record
            Events : Integer;
            Data : Epoll_Data;
        end record;
        type Epoll_Event_Ptr is access all Epoll_Event;


        function Epoll_Create (Size : C.int) return C.int;

        function Epoll_Ctl (Epoll_Descriptor : C.int;
                                Op : C.int;
                                Descriptor : C.int;
                                Event : Epoll_Event_Ptr) return C.int;

        function Epoll_Wait (Epoll_Descriptor : C.int;
                                Events : Epoll_Event_Ptr;
                                Max_Events : C.int;
                                Timeout : C.int) return C.int;



        pragma Import (C, Epoll_Create);
        pragma Import (C, Epoll_Ctl);
        pragma Import (C, Epoll_Wait);
end Epoll;
