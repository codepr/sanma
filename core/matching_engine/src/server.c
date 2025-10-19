#include "server.h"
#include "iomux.h"
#include "logger.h"
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#define BUFFER_SIZE 256

// NETWORKING PRIMITIVES
static int set_nonblocking(int fd)
{
    int flags, result;
    flags = fcntl(fd, F_GETFL, 0);
    if (flags == -1)
        return -1;

    result = fcntl(fd, F_SETFL, flags | O_NONBLOCK);
    if (result == -1)
        return -1;

    return 0;
}

static int unix_socket_listen(const char *sockpath)
{
    // Create Unix socket
    int listen_fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (listen_fd < 0)
        return -1;

    struct sockaddr_un addr;
    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, sockpath, sizeof(addr.sun_path) - 1);

    unlink(sockpath);

    if (bind(listen_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0)
        goto error;

    if (listen(listen_fd, 32) < 0)
        goto error;

    set_nonblocking(listen_fd);

    return listen_fd;

error:
    close(listen_fd);
    return -1;
}

static int init_engine(Engine_Server *server)
{
    orderbook_init(&server->book);

    memset(server->connections, 0, sizeof(server->connections));
    server->connection_count   = 0;
    server->next_connection_id = 1;

    for (int i = 0; i < MAX_CONNECTIONS; ++i)
        server->connections[i].fd = -1;

    server->listen_fd = unix_socket_listen(SOCKET_PATH);
    if (server->listen_fd < 0)
        return -1;

    return 0;
}

static void process_message(Engine_Server *engine, uint32_t conn_idx, void *buffer)
{
    uint8_t *packet               = buffer;
    struct message_header *header = (struct message_header *)packet;

    switch (header->type) {
    case MSG_NEW_ORDER:
        if (header->length != sizeof(struct order_request_message))
            return;

        uint8_t *body                               = packet + sizeof(struct message_header);
        struct order_request_message *order_request = (struct order_request_message *)body;

        Order order                                 = {.order_id  = engine->book.next_order_id++,
                                                       .client_id = engine->connections[conn_idx].client_id,
                                                       .price     = order_request->price,
                                                       .quantity  = order_request->quantity,
                                                       .filled    = 0,
                                                       .type      = ORDER_LIMIT,
                                                       .side      = order_request->side};

        log_debug("recv: order request from %u - \"%s\" \"%s\" %llu %u %u", order.client_id,
                  order.type == ORDER_LIMIT ? "limit" : "market",
                  order.side == ORDER_BUY ? "buy" : "sell", order.order_id, order.price,
                  order.quantity);

        uint32_t result                    = orderbook_add(&engine->book, &order);

        struct message_header ack_header   = {.type   = (result == UINT32_MAX) ? MSG_ORDER_REJECT
                                                                               : MSG_ORDER_ACK,
                                              .length = sizeof(struct order_ack_message)};
        struct order_ack_message order_ack = {.order_id = order.order_id,
                                              .status   = (result == UINT32_MAX) ? 1 : 0};

        send(engine->connections[conn_idx].fd, &ack_header, sizeof(ack_header), 0);
        send(engine->connections[conn_idx].fd, &order_ack, sizeof(order_ack), 0);

        log_debug("sent: order ack to %u - status %u", order.client_id, ack_header.type);

        if (result == UINT32_MAX)
            return;

        // Try to match
        Trade trades[100];
        uint32_t trade_count;
        orderbook_match(&engine->book, trades, &trade_count, 100);

        if (trade_count > 0) {
            struct message_header trade_header = {.type   = MSG_TRADE,
                                                  .length = sizeof(struct trade_message)};

            for (uint32_t i = 0; i < trade_count; ++i) {
                for (uint32_t c = 0; c < MAX_CONNECTIONS; ++c) {
                    if (engine->connections[c].active) {
                        send(engine->connections[c].fd, &trade_header, sizeof(trade_header), 0);
                        send(engine->connections[c].fd, &trades[i], sizeof(Trade), 0);
                    }
                }
            }
        }
        break;

    default:
        break;
    }
}

int engine_server_start(Engine_Server *server)
{
    if (init_engine(server) < 0)
        return -1;

    log_info(">>>>: matching engine listening on %s", SOCKET_PATH);
    log_info(">>>>: price range: $%.2f - $%.2f (1 cent precision)", MIN_PRICE / 100.0,
             MAX_PRICE / 100.0);

    server->is_running = 1;

    uint8_t buffer[BUFFER_SIZE];
    IO_Mux *iomux = iomux_create();
    if (!iomux)
        return -1;

    iomux_add(iomux, server->listen_fd, IOMUX_READ);

    while (server->is_running) {
        int numevents = iomux_wait(iomux, -1);
        if (numevents < 0)
            log_error(">>>>: iomux error: %s", strerror(errno));

        for (int i = 0; i < numevents; ++i) {
            int fd = iomux_get_event_fd(iomux, i);

            if (fd == server->listen_fd) {
                int conn_fd = accept(server->listen_fd, NULL, NULL);
                if (conn_fd >= 0 && server->connection_count < MAX_CONNECTIONS) {
                    set_nonblocking(conn_fd);
                    // uint32_t idx                           = engine->connection_count++;
                    server->connections[conn_fd].fd        = conn_fd;
                    server->connections[conn_fd].client_id = server->next_connection_id++;
                    server->connections[conn_fd].active    = 1;

                    iomux_add(iomux, conn_fd, IOMUX_READ);

                    log_info(">>>>: client %u connected", server->connections[conn_fd].client_id);
                }
            } else if (server->connections[fd].fd == fd) {
                // Check client messages
                ssize_t n = recv(fd, buffer, BUFFER_SIZE - 1, 0);
                if (n > 0) {
                    buffer[n] = '\0';
                    process_message(server, fd, buffer);
                } else {
                    // Client disconnected
                    iomux_del(iomux, server->connections[fd].fd);
                    close(server->connections[fd].fd);
                    server->connections[fd].fd     = -1;
                    server->connections[fd].active = false;
                    log_info(">>>>: client %u disconnected", server->connections[fd].client_id);
                }
            }
        }
    }

    iomux_free(iomux);
    unlink(SOCKET_PATH);

    return 0;
}

void engine_server_stop(Engine_Server *server) { server->is_running = 0; }
