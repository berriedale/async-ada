

#include <sys/epoll.h>

epoll_data_t fd_of_one() {
    epoll_data_t rc;
    rc.fd = 1;
    return rc;
}

