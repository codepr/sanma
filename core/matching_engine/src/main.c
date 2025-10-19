#include "logger.h"
#include "server.h"
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

static Engine_Server server = {0};

static void sig_handler(int signum)
{
    log_info(">>>>: received signal %d, gracefully shutting down...", signum);

    // Set the flag to terminate the main loop
    engine_server_stop(&server);
}

int main(void)
{
    if (signal(SIGINT, sig_handler) == SIG_ERR)
        log_warning(">>>>: error registering signal handler.");

    int err = engine_server_start(&server);
    if (err < 0)
        log_critical(">>>>: error initing matching engine");

    return 0;
}
