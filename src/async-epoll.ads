--
--  Main interface for dealing with epoll(7) from Ada
--

with Ada.Containers.Hashed_Maps,
        Interfaces.C,
        GNAT.Sockets;

use Interfaces,
    Ada.Containers;

private with System;

generic
    type Context_Type is private;

package Async.Epoll is
    type Callback_Type is access procedure (Sock : GNAT.Sockets.Socket_Type; Context : Context_Type);

    -- Callback_Tuple is just an arbitrary record to carry the Callback_Type
    -- and the Context_Type to be passed into that Callback_Type along through
    -- the Callback_Registry.
    type Callback_Tuple is record
        Socket : GNAT.Sockets.Socket_Type;
        Callback : Callback_Type;
        Context : Context_Type;
    end record;

    function Descriptor_Hash (Id : in C.int) return Hash_Type;

    -- My_Callback.all (Fd);
    use Interfaces.C;
    package Callback_Registry is new Hashed_Maps (Key_Type => C.int,
                                            Element_Type => Callback_Tuple,
                                            Hash => Descriptor_Hash,
                                            Equivalent_Keys => "="
                                            );

    type Hub is tagged private;

    procedure Register (This : in out Hub;
                        Cb : in Callback_Tuple);
    procedure Run (This : in Hub);

    function Create return Hub;
    procedure Enable_Tracing (H : in out Hub);

    Hub_Create_Failed : exception;
    Hub_Invalid : exception;
    Descriptor_Registration_Falied : exception;
    Invalid_Descriptor : exception;
    Epoll_Wait_Failure : exception;

    private
        use Interfaces.C;

        --  Callback_Registry will be used to determine "who" to invoke when a
        --  specific descriptor receives data in the `Run` loop

        type Hub is tagged record
            Epoll_Fd : C.int := -1;
            Timeout : C.int := -1;
            Should_Continue : Boolean := True;
            Debug : Boolean := False;
            Callbacks : Callback_Registry.Map;
        end record;

        procedure Validate_Hub (H: in Hub);
        procedure Debug_Trace (H: in Hub; Line : in String);

        subtype EPOLL_CTL_OPS is C.unsigned;
        EPOLL_CTL_ADD : constant EPOLL_CTL_OPS := 1;
        EPOLL_CTL_DEL : constant EPOLL_CTL_OPS := 2;
        EPOLL_CTL_MOD : constant EPOLL_CTL_OPS := 3;

        subtype EPOLL_EVENTS is C.unsigned;
        EPOLLIN : constant EPOLL_EVENTS := 1;
        EPOLLPRI : constant EPOLL_EVENTS := 2;
        EPOLLOUT : constant EPOLL_EVENTS := 4;
        EPOLLRDNORM : constant EPOLL_EVENTS := 64;
        EPOLLRDBAND : constant EPOLL_EVENTS := 128;
        EPOLLWRNORM : constant EPOLL_EVENTS := 256;
        EPOLLWRBAND : constant EPOLL_EVENTS := 512;
        EPOLLMSG : constant EPOLL_EVENTS := 1024;
        EPOLLERR : constant EPOLL_EVENTS := 8;
        EPOLLHUP : constant EPOLL_EVENTS := 16;
        EPOLLRDHUP : constant EPOLL_EVENTS := 8192;
        EPOLLONESHOT : constant EPOLL_EVENTS := 1073741824;
        EPOLLET : constant EPOLL_EVENTS := -2147483648;


        type Epoll_Data_Type is (Pointer,
                                File_Descriptor,
                                Int_32,
                                Int_64);

        type Epoll_Data (D : Epoll_Data_Type := File_Descriptor) is record
            case D is
                when Pointer =>
                    Ptr : System.Address;
                when File_Descriptor =>
                    Fd : aliased C.int;
                when Int_32 =>
                    u32 : aliased C.int;
                when Int_64 =>
                    u64 : aliased C.int;
            end case;
        end record;
        pragma Convention (C_Pass_By_Copy, Epoll_Data);
        pragma Unchecked_Union (Epoll_Data);

        type Epoll_Event is record
            Events : aliased C.unsigned;
            Data : Epoll_Data;
        end record;
        pragma Convention (C_Pass_By_Copy, Epoll_Event);
        type Epoll_Event_Array is array (C.size_t range <>) of aliased Epoll_Event;


        function Epoll_Create (Size : C.int) return C.int;

        function Epoll_Ctl (Epoll_Descriptor : C.int;
                                Op : EPOLL_CTL_OPS;
                                Descriptor : C.int;
                                Event : access Epoll_Event) return C.int;

        function Epoll_Wait (Epoll_Descriptor : C.int;
                                Events : Epoll_Event_Array;
                                Max_Events : C.int;
                                Timeout : C.int) return C.int;


        pragma Import (C, Epoll_Create, "epoll_create");
        pragma Import (C, Epoll_Ctl, "epoll_ctl");
        pragma Import (C, Epoll_Wait, "epoll_wait");
end Async.Epoll;
